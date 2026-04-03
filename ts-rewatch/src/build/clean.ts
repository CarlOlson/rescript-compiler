// Port from rewatch/src/build/clean.rs
// Build artifact cleanup

import * as fs from "node:fs";
import * as path from "node:path";
import type { Package, BuildState, Module, Namespace } from "../types/build.ts";
import {
  getPackageBuildPath,
  getPackageOcamlBuildPath,
  getPackageCompilerInfoPath,
} from "../types/build.ts";
import { getBasename, getSourceFileFromRescriptFile } from "../utils/paths.ts";
import { getSuffix, getPackageSpecs } from "../types/config.ts";
import { emojis } from "../utils/helpers.ts";

/**
 * Get the compiler asset path for a source file
 */
function getCompilerAsset(
  pkg: Package,
  namespace: Namespace,
  sourceFile: string,
  extension: string,
): string {
  const useNamespace = extension !== "ast" && extension !== "iast";
  const ns = useNamespace ? namespace : { type: "noNamespace" as const };

  const basename = getBasename(sourceFile);
  let assetName = basename;

  if (ns.type === "namespace") {
    assetName = `${basename}-${ns.name}`;
  } else if (ns.type === "namespaceWithEntry") {
    assetName = `${basename}-@${ns.name}`;
  }

  return path.join(getPackageOcamlBuildPath(pkg), `${assetName}.${extension}`);
}

/**
 * Remove AST file for a source file
 */
function removeAst(pkg: Package, sourceFile: string): void {
  const astPath = getCompilerAsset(
    pkg,
    { type: "noNamespace" },
    sourceFile,
    "ast",
  );
  try {
    fs.unlinkSync(astPath);
  } catch {
    // Ignore errors
  }
}

/**
 * Remove interface AST file for a source file
 */
function removeIast(pkg: Package, sourceFile: string): void {
  const iastPath = getCompilerAsset(
    pkg,
    { type: "noNamespace" },
    sourceFile,
    "iast",
  );
  try {
    fs.unlinkSync(iastPath);
  } catch {
    // Ignore errors
  }
}

/**
 * Remove compiled JS file for a source file
 */
function removeMjsFile(sourceFile: string, suffix: string): void {
  const jsPath = getSourceFileFromRescriptFile(sourceFile, suffix);
  try {
    fs.unlinkSync(jsPath);
  } catch {
    // Ignore errors
  }
}

/**
 * Remove a single compile asset for a source file
 */
function removeCompileAsset(
  pkg: Package,
  sourceFile: string,
  extension: string,
): void {
  const assetPath = getCompilerAsset(pkg, pkg.namespace, sourceFile, extension);
  try {
    fs.unlinkSync(assetPath);
  } catch {
    // Ignore errors
  }
}

/**
 * Remove all compile assets for a source file
 */
export function removeCompileAssets(pkg: Package, sourceFile: string): void {
  for (const extension of ["cmj", "cmi", "cmt", "cmti"]) {
    removeCompileAsset(pkg, sourceFile, extension);
  }
}

/**
 * Clean source files (remove generated JS files)
 */
function cleanSourceFiles(buildState: BuildState): void {
  const rootConfig =
    buildState.projectContext.monorepoContext?.type === "package"
      ? buildState.projectContext.monorepoContext.parentConfig
      : buildState.projectContext.currentConfig;

  const specs = getPackageSpecs(rootConfig);

  for (const module of buildState.modules.values()) {
    if (module.sourceType.type !== "sourceFile") {
      continue;
    }

    const pkg = buildState.packages.get(module.packageName);
    if (pkg === undefined) {
      continue;
    }

    const sourceFile = module.sourceType.sourceFile;
    const sourcePath = path.join(pkg.path, sourceFile.implementation.path);

    for (const spec of specs) {
      if (spec["in-source"] !== false) {
        const suffix = getSuffix(rootConfig, spec);
        removeMjsFile(sourcePath, suffix);
      }
    }
  }
}

/**
 * Clean a single package's build artifacts
 */
export function cleanPackage(
  pkg: Package,
  showProgress: boolean = false,
): void {
  if (showProgress) {
    process.stdout.write(
      `${emojis.LINE_CLEAR}${emojis.SWEEP}Cleaning ${pkg.name}...`,
    );
  }

  // Remove lib/bs directory
  const bsPath = getPackageBuildPath(pkg);
  try {
    fs.rmSync(bsPath, { recursive: true, force: true });
  } catch {
    // Ignore errors
  }

  // Remove lib/ocaml directory
  const ocamlPath = getPackageOcamlBuildPath(pkg);
  try {
    fs.rmSync(ocamlPath, { recursive: true, force: true });
  } catch {
    // Ignore errors
  }

  // Remove compiler info file
  const compilerInfoPath = getPackageCompilerInfoPath(pkg);
  try {
    fs.unlinkSync(compilerInfoPath);
  } catch {
    // Ignore errors
  }
}

/**
 * Clean all packages
 */
export function cleanPackages(
  packages: Map<string, Package>,
  showProgress: boolean = false,
): void {
  for (const pkg of packages.values()) {
    cleanPackage(pkg, showProgress);
  }
}

/**
 * Check if a module has parse warnings
 */
function hasParseWarnings(module: Module): boolean {
  if (module.sourceType.type !== "sourceFile") {
    return false;
  }
  const sf = module.sourceType.sourceFile;
  return (
    sf.implementation.parseState === "warning" ||
    sf.interface?.parseState === "warning"
  );
}

/**
 * Check if a module has compile warnings
 */
function hasCompileWarnings(module: Module): boolean {
  if (module.sourceType.type !== "sourceFile") {
    return false;
  }
  const sf = module.sourceType.sourceFile;
  return (
    sf.implementation.compileState === "warning" ||
    sf.interface?.compileState === "warning"
  );
}

/**
 * Cleanup after build - remove AST files for modules with warnings
 */
export function cleanupAfterBuild(buildState: BuildState): void {
  for (const module of buildState.modules.values()) {
    const pkg = buildState.packages.get(module.packageName);
    if (pkg === undefined) {
      continue;
    }

    if (module.sourceType.type !== "sourceFile") {
      continue;
    }

    const sourceFile = module.sourceType.sourceFile;

    if (hasParseWarnings(module)) {
      removeIast(pkg, sourceFile.implementation.path);
      removeAst(pkg, sourceFile.implementation.path);
    }

    if (hasCompileWarnings(module)) {
      removeAst(pkg, sourceFile.implementation.path);
      removeIast(pkg, sourceFile.implementation.path);
    }
  }
}

/**
 * Full clean command
 */
export function clean(
  buildState: BuildState,
  showProgress: boolean = false,
): void {
  // Step 1: Clean compiler assets
  if (showProgress) {
    process.stdout.write(`${emojis.SWEEP}Cleaning compiler assets...\n`);
  }

  cleanPackages(buildState.packages, showProgress);

  // Step 2: Clean source files
  if (showProgress) {
    process.stdout.write(`${emojis.SWEEP}Cleaning generated files...\n`);
  }

  cleanSourceFiles(buildState);
}
