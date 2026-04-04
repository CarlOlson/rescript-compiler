// Port from rewatch/src/build/compiler_info.rs
// Compiler version tracking and build artifact validation

import * as fs from "node:fs";
import * as path from "node:path";
import type { CompilerInfo, Package, BuildState } from "../types/build.ts";
import {
  getPackageBuildPath,
  getPackageCompilerInfoPath,
} from "../types/build.ts";
import { computeFileHashSync } from "../utils/hash.ts";
import { getBsc, getRuntimePath, getSystemTime } from "../utils/helpers.ts";

// Package version (would come from package.json in real implementation)
const VERSION = "0.1.0";

interface CompilerInfoFile {
  version: string;
  bsc_path: string;
  bsc_hash: string;
  rescript_config_hash: string;
  runtime_path?: string;
  generated_at: string;
}

export type CompilerCheckResult =
  | { type: "sameCompilerAsLastRun" }
  | { type: "cleanedPackagesDueToCompiler"; packageCount: number };

/**
 * Get the rescript.json config hash for a package
 */
function getRescriptConfigHash(pkg: Package): string | undefined {
  const configPath = pkg.config.path;
  if (configPath === undefined) {
    return undefined;
  }
  return computeFileHashSync(configPath);
}

/**
 * Check if the ocaml build compiler log exists for a package
 */
function doesOcamlBuildCompilerLogExist(pkg: Package): boolean {
  const buildPath = getPackageBuildPath(pkg);
  const compilerLogPath = path.join(buildPath, ".compiler.log");
  return fs.existsSync(compilerLogPath);
}

/**
 * Get compiler info (bsc path, hash, runtime path)
 */
export function getCompilerInfo(): CompilerInfo {
  const bscPath = getBsc();
  const bscHash = computeFileHashSync(bscPath) ?? "";
  const runtimePath = getRuntimePath();

  return {
    bscPath,
    bscHash,
    runtimePath,
  };
}

/**
 * Verify compiler info matches last build, clean if not
 */
export function verifyCompilerInfo(
  packages: Map<string, Package>,
  compiler: CompilerInfo,
): CompilerCheckResult {
  const mismatchedPackages: Package[] = [];

  for (const pkg of packages.values()) {
    const infoPath = getPackageCompilerInfoPath(pkg);

    let contents: string;
    try {
      contents = fs.readFileSync(infoPath, "utf-8");
    } catch {
      // Can't read compiler-info.json, check if ocaml build exists
      if (doesOcamlBuildCompilerLogExist(pkg)) {
        mismatchedPackages.push(pkg);
      }
      continue;
    }

    let parsed: CompilerInfoFile;
    try {
      parsed = JSON.parse(contents);
    } catch {
      // Invalid format, treat as mismatch
      mismatchedPackages.push(pkg);
      continue;
    }

    const currentRescriptConfigHash = getRescriptConfigHash(pkg);
    if (currentRescriptConfigHash === undefined) {
      // Can't compute hash, treat as mismatch
      mismatchedPackages.push(pkg);
      continue;
    }

    let mismatch = false;

    if (parsed.bsc_path !== compiler.bscPath) {
      mismatch = true;
    }
    if (parsed.bsc_hash !== compiler.bscHash) {
      mismatch = true;
    }
    if (parsed.runtime_path !== compiler.runtimePath) {
      mismatch = true;
    }
    if (parsed.rescript_config_hash !== currentRescriptConfigHash) {
      mismatch = true;
    }

    if (mismatch) {
      mismatchedPackages.push(pkg);
    }
  }

  // Clean mismatched packages (parallel in Rust, sequential here for simplicity)
  for (const pkg of mismatchedPackages) {
    cleanPackageQuiet(pkg);
  }

  if (mismatchedPackages.length === 0) {
    return { type: "sameCompilerAsLastRun" };
  }

  return {
    type: "cleanedPackagesDueToCompiler",
    packageCount: mismatchedPackages.length,
  };
}

/**
 * Clean a package's build artifacts quietly
 */
function cleanPackageQuiet(pkg: Package): void {
  const buildPath = getPackageBuildPath(pkg);
  try {
    fs.rmSync(buildPath, { recursive: true, force: true });
  } catch {
    // Ignore errors
  }
}

/**
 * Write compiler info synchronously for all packages
 */
export function writeCompilerInfoSync(buildState: BuildState): void {
  const { bscPath, bscHash, runtimePath } = buildState.compilerInfo;
  const generatedAt = getSystemTime().toString();

  for (const pkg of buildState.packages.values()) {
    const rescriptConfigHash = getRescriptConfigHash(pkg);
    if (rescriptConfigHash === undefined) {
      continue;
    }

    const info: CompilerInfoFile = {
      version: VERSION,
      bsc_path: bscPath,
      bsc_hash: bscHash,
      rescript_config_hash: rescriptConfigHash,
      runtime_path: runtimePath,
      generated_at: generatedAt,
    };

    const contents = JSON.stringify(info, null, 2);
    const infoPath = getPackageCompilerInfoPath(pkg);

    // Check if we need to write
    try {
      const existing = fs.readFileSync(infoPath, "utf-8");
      if (existing === contents) {
        continue; // No change needed
      }
    } catch {
      // File doesn't exist or can't be read, proceed with write
    }

    // Ensure directory exists
    const dir = path.dirname(infoPath);
    fs.mkdirSync(dir, { recursive: true });

    // Write atomically using temp file + rename
    const tmpPath = `${infoPath}.tmp`;
    try {
      fs.writeFileSync(tmpPath, contents);
      fs.renameSync(tmpPath, infoPath);
    } catch {
      // Clean up temp file on error
      try {
        fs.unlinkSync(tmpPath);
      } catch {
        // Ignore
      }
    }
  }
}
