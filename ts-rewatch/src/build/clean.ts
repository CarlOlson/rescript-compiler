// Port from rewatch/src/build/clean.rs
// Build artifact cleanup

import * as fs from "node:fs";
import * as path from "node:path";
import type { Package, BuildState, Module, Namespace } from "../types/build.ts";
import { getPackageOcamlBuildPath } from "../types/build.ts";
import { getBasename } from "../utils/paths.ts";

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
