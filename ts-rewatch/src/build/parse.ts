// Port from rewatch/src/build/parse.rs
// AST generation via bsc

import * as path from "node:path";
import { spawnSync } from "node:child_process";
import type { BuildState, Package } from "../types/build.ts";
import {
  getPackageBuildPath,
  getPackageOcamlBuildPath,
  namespaceToSuffix,
} from "../types/build.ts";
import { getAstPath, containsAsciiCharacters } from "../utils/paths.ts";
import {
  flattenFlags,
  getJsxArgs,
  getJsxModeArgs,
  getJsxModuleArgs,
  getJsxPreserveArgs,
  getExperimentalFeaturesArgs,
  getWarningArgs,
} from "../types/config.ts";
import { readFileSync, createPathSync } from "../utils/helpers.ts";
import { append as logAppend } from "./logs.ts";
import { compileMlmapSync, generateMlmap } from "./namespaces.ts";
import { computeFileHashSync } from "../utils/hash.ts";
import * as fs from "node:fs";

export interface ParseResult {
  stderr: string;
  hasErrors: boolean;
}

/**
 * Get parser arguments for bsc -bs-ast
 */
function getParserArgs(
  buildState: BuildState,
  pkg: Package,
  filename: string,
  contents: string,
  warnErrorOverride?: string,
): { astPath: string; args: string[] } {
  const rootConfig =
    buildState.projectContext.monorepoContext?.type === "package"
      ? buildState.projectContext.monorepoContext.parentConfig
      : buildState.projectContext.currentConfig;

  const astPath = getAstPath(filename);

  // Get various flags
  const jsxArgs = getJsxArgs(rootConfig);
  const jsxModeArgs = getJsxModeArgs(rootConfig);
  const jsxModuleArgs = getJsxModuleArgs(rootConfig);
  const jsxPreserveArgs = getJsxPreserveArgs(rootConfig);
  const experimentalFeaturesArgs = getExperimentalFeaturesArgs(rootConfig);
  const bscFlags = flattenFlags(pkg.config["compiler-flags"]);
  const warningArgs = getWarningArgs(
    pkg.config,
    pkg.isLocalDep,
    warnErrorOverride,
  );

  // PPX flags would need more complex handling - simplified for now
  const ppxFlags = filterPpxFlags(pkg.config["ppx-flags"], contents);

  // The file path is relative from lib/bs
  const fileRelative = path.join("..", "..", filename);

  const args = [
    ...ppxFlags,
    ...jsxArgs,
    ...jsxModuleArgs,
    ...jsxModeArgs,
    ...jsxPreserveArgs,
    ...experimentalFeaturesArgs,
    ...warningArgs,
    ...bscFlags,
    "-absname",
    "-bs-ast",
    "-o",
    astPath,
    fileRelative,
  ];

  return { astPath, args };
}

/**
 * Filter PPX flags based on file contents
 */
function filterPpxFlags(
  ppxFlags: (string | string[])[] | undefined,
  contents: string,
): string[] {
  if (ppxFlags === undefined) {
    return [];
  }

  const result: string[] = [];

  for (const flag of ppxFlags) {
    const flagStr = Array.isArray(flag) ? flag[0] : flag;

    if (!includePpx(flagStr, contents)) {
      continue;
    }

    if (Array.isArray(flag)) {
      result.push("-ppx", flag.join(" "));
    } else {
      result.push("-ppx", flag);
    }
  }

  return result;
}

/**
 * Check if a PPX should be included based on file contents
 */
function includePpx(flag: string, contents: string): boolean {
  // Bisect requires environment variable
  if (flag.includes("bisect")) {
    return process.env.BISECT_ENABLE !== undefined;
  }

  // Skip certain PPXs if their markers aren't present
  if (
    (flag.includes("graphql-ppx") || flag.includes("graphql_ppx")) &&
    !contents.includes("%graphql")
  ) {
    return false;
  }
  if (flag.includes("spice") && !contents.includes("@spice")) {
    return false;
  }
  if (flag.includes("rescript-relay") && !contents.includes("%relay")) {
    return false;
  }
  if (flag.includes("re-formality") && !contents.includes("%form")) {
    return false;
  }

  return true;
}

/**
 * Generate AST for a single file
 */
function generateAst(
  pkg: Package,
  filename: string,
  buildState: BuildState,
  warnErrorOverride?: string,
): { astPath: string; stderr: string | undefined; hasError: boolean } {
  const filePath = path.join(pkg.path, filename);
  const contents = readFileSync(filePath);

  const buildPath = getPackageBuildPath(pkg);
  const { astPath, args } = getParserArgs(
    buildState,
    pkg,
    filename,
    contents,
    warnErrorOverride,
  );

  // Create directory for AST file
  const astDir = path.join(buildPath, path.dirname(astPath));
  createPathSync(astDir);

  // Run bsc
  const result = spawnSync(buildState.compilerInfo.bscPath, args, {
    cwd: buildPath,
    encoding: "utf-8",
  });

  const stderr = result.stderr?.trim();
  const hasStderr = stderr !== undefined && containsAsciiCharacters(stderr);

  if (result.status !== 0) {
    return {
      astPath,
      stderr: `Error in ${pkg.name}:\n${stderr}`,
      hasError: true,
    };
  }

  // Copy AST to ocaml build path
  const ocamlBuildPath = getPackageOcamlBuildPath(pkg);
  createPathSync(ocamlBuildPath);
  try {
    fs.copyFileSync(
      path.join(buildPath, astPath),
      path.join(ocamlBuildPath, path.basename(astPath)),
    );
  } catch {
    // Ignore copy errors
  }

  return {
    astPath,
    stderr: hasStderr ? stderr : undefined,
    hasError: false,
  };
}

/**
 * Generate ASTs for all dirty modules
 */
export function generateAsts(
  buildState: BuildState,
  warnErrorOverride?: string,
  onProgress?: () => void,
): ParseResult {
  let hasErrors = false;
  let stderr = "";
  const dirtyPackages = new Set<string>();

  // Process all modules
  for (const [_moduleName, module] of buildState.modules) {
    const pkg = buildState.packages.get(module.packageName);
    if (pkg === undefined) {
      continue;
    }

    if (module.sourceType.type === "mlMap") {
      // MlMap modules are handled separately
      continue;
    }

    const sourceFile = module.sourceType.sourceFile;

    // Check if implementation needs parsing
    if (sourceFile.implementation.parseDirty) {
      onProgress?.();

      const result = generateAst(
        pkg,
        sourceFile.implementation.path,
        buildState,
        warnErrorOverride,
      );

      if (result.hasError) {
        hasErrors = true;
        sourceFile.implementation.parseState = "parseError";
        sourceFile.implementation.parseDirty = true;
        if (result.stderr) {
          logAppend(pkg, result.stderr);
          stderr += `${result.stderr}\n`;
        }
      } else if (result.stderr && pkg.isLocalDep) {
        sourceFile.implementation.parseState = "warning";
        sourceFile.implementation.parseDirty = true;
        logAppend(pkg, result.stderr);
        stderr += `${result.stderr}\n`;
      } else {
        sourceFile.implementation.parseState = "success";
        sourceFile.implementation.parseDirty = false;
      }

      module.compileDirty = true;
      module.depsDirty = true;
      dirtyPackages.add(module.packageName);
    }

    // Check if interface needs parsing
    if (sourceFile.interface?.parseDirty) {
      const result = generateAst(
        pkg,
        sourceFile.interface.path,
        buildState,
        warnErrorOverride,
      );

      if (result.hasError) {
        hasErrors = true;
        sourceFile.interface.parseState = "parseError";
        sourceFile.interface.parseDirty = true;
        if (result.stderr) {
          logAppend(pkg, result.stderr);
          stderr += `${result.stderr}\n`;
        }
      } else if (result.stderr && pkg.isLocalDep) {
        sourceFile.interface.parseState = "warning";
        sourceFile.interface.parseDirty = true;
        logAppend(pkg, result.stderr);
        stderr += `${result.stderr}\n`;
      } else {
        sourceFile.interface.parseState = "success";
        sourceFile.interface.parseDirty = false;
      }

      module.compileDirty = true;
      module.depsDirty = true;
      dirtyPackages.add(module.packageName);
    }
  }

  // Compile mlmaps for dirty packages
  for (const [_moduleName, module] of buildState.modules) {
    if (module.sourceType.type !== "mlMap") {
      continue;
    }

    const pkg = buildState.packages.get(module.packageName);
    if (pkg === undefined || !dirtyPackages.has(module.packageName)) {
      continue;
    }

    const suffix = namespaceToSuffix(pkg.namespace);
    if (suffix === undefined) {
      continue;
    }

    // Generate mlmap
    const modules = pkg.modules ?? new Set();
    generateMlmap(pkg, suffix, modules);

    // Compile mlmap
    const compilePath = path.join(getPackageBuildPath(pkg), `${suffix}.cmi`);
    const hashBefore = computeFileHashSync(compilePath);

    try {
      compileMlmapSync(
        pkg,
        suffix,
        buildState.compilerInfo.bscPath,
        buildState.compilerInfo.runtimePath,
      );
    } catch (e) {
      hasErrors = true;
      stderr += `${e}\n`;
    }

    const hashAfter = computeFileHashSync(compilePath);

    // Copy mlmap artifacts to ocaml build path
    const buildPath = getPackageBuildPath(pkg);
    const ocamlBuildPath = getPackageOcamlBuildPath(pkg);
    const baseName = suffix;

    for (const ext of ["cmi", "cmt", "cmj", "mlmap"]) {
      try {
        fs.copyFileSync(
          path.join(buildPath, `${baseName}.${ext}`),
          path.join(ocamlBuildPath, `${baseName}.${ext}`),
        );
      } catch {
        // Ignore copy errors
      }
    }

    // Mark as compile dirty if mlmap changed
    if (hashBefore !== hashAfter) {
      module.compileDirty = true;
    }
  }

  return { stderr, hasErrors };
}
