// Port from rewatch/src/build/logs.rs
// Compiler log file management

import * as fs from "node:fs";
import * as path from "node:path";
import type { Package } from "../types/build.ts";
import {
  getPackageBuildPath,
  getPackageOcamlBuildPath,
} from "../types/build.ts";
import { getSystemTime, createPathSync } from "../utils/helpers.ts";

type Location = "bs" | "ocaml";

/**
 * Get the compiler log file path for a package
 */
function getLogFilePath(pkg: Package, location: Location): string {
  const buildFolder =
    location === "bs"
      ? getPackageBuildPath(pkg)
      : getPackageOcamlBuildPath(pkg);
  return path.join(buildFolder, ".compiler.log");
}

/**
 * Escape ANSI color codes from a string
 */
function escapeColours(str: string): string {
  // Match ANSI escape sequences
  return str.replace(
    // biome-ignore lint: n/a
    /[\u001b\u009b]\[[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]/g,
    "",
  );
}

/**
 * Write content to a log file
 */
function writeToLogFile(filePath: string, content: string): void {
  try {
    fs.appendFileSync(filePath, escapeColours(content));
  } catch (e) {
    console.error(`Could not write to compiler log file: ${filePath}`, e);
  }
}

/**
 * Initialize compiler logs for all packages
 */
export function initialize(packages: Map<string, Package>): void {
  for (const pkg of packages.values()) {
    const logPath = getLogFilePath(pkg, "bs");
    const dir = path.dirname(logPath);

    try {
      createPathSync(dir);
      fs.writeFileSync(logPath, `#Start(${getSystemTime()})\n`);
    } catch (e) {
      throw new Error(
        `Cannot create compiler log for package ${pkg.name}: ${e}`,
      );
    }
  }
}

/**
 * Append content to a package's compiler log
 */
export function append(pkg: Package, content: string): void {
  const logPath = getLogFilePath(pkg, "bs");

  try {
    writeToLogFile(logPath, content);
  } catch (e) {
    throw new Error(`Cannot write compiler log: ${logPath} (${e})`);
  }
}

/**
 * Finalize compiler logs for all packages
 */
export function finalize(packages: Map<string, Package>): void {
  for (const pkg of packages.values()) {
    finalizePackage(pkg);
  }
}

/**
 * Finalize compiler log for a single package
 */
function finalizePackage(pkg: Package): void {
  const bsLogPath = getLogFilePath(pkg, "bs");
  const ocamlLogPath = getLogFilePath(pkg, "ocaml");

  try {
    // Append done marker
    fs.appendFileSync(bsLogPath, `#Done(${getSystemTime()})\n`);

    // Copy to ocaml location
    const ocamlDir = path.dirname(ocamlLogPath);
    createPathSync(ocamlDir);
    fs.copyFileSync(bsLogPath, ocamlLogPath);
  } catch {
    // Ignore errors in finalization
  }
}
