// Port from rewatch/src/build/compile.rs
// Module compilation

import * as path from "node:path";
import * as fs from "node:fs";
import { spawnSync } from "node:child_process";
import type { BuildState, Module, Package, Namespace } from "../types/build.ts";
import {
  getPackageBuildPath,
  getPackageOcamlBuildPath,
  namespaceToSuffix,
} from "../types/build.ts";
import { getBasename, containsAsciiCharacters, getSourceFileFromRescriptFile } from "../utils/paths.ts";
import {
  flattenFlags,
  getPackageSpecs,
  getSuffix,
  getWarningArgs,
} from "../types/config.ts";
import { createPathSync, getLastModifiedSync } from "../utils/helpers.ts";
import { append as logAppend } from "./logs.ts";
import { findCycle, formatCycle } from "./cycle.ts";

export interface CompileResult {
  errors: string;
  warnings: string;
  numCompiled: number;
}

/**
 * Get compiler arguments for a module
 */
function getCompilerArgs(
  buildState: BuildState,
  pkg: Package,
  module: Module,
  warnErrorOverride?: string,
): string[] {
  if (module.sourceType.type !== "sourceFile") {
    return [];
  }

  const rootConfig =
    buildState.projectContext.monorepoContext?.type === "package"
      ? buildState.projectContext.monorepoContext.parentConfig
      : buildState.projectContext.currentConfig;

  const sourceFile = module.sourceType.sourceFile;
  const specs = getPackageSpecs(rootConfig);
  const bscFlags = flattenFlags(pkg.config["compiler-flags"]);
  const warningArgs = getWarningArgs(pkg.config, pkg.isLocalDep, warnErrorOverride);

  // Include paths for dependencies
  const includePaths: string[] = [];
  for (const depName of module.deps) {
    const depModule = buildState.modules.get(depName);
    if (depModule === undefined) continue;

    const depPkg = buildState.packages.get(depModule.packageName);
    if (depPkg === undefined) continue;

    const depBuildPath = getPackageOcamlBuildPath(depPkg);
    if (!includePaths.includes(depBuildPath)) {
      includePaths.push(depBuildPath);
    }
  }

  // Add own package build path
  const ownBuildPath = getPackageOcamlBuildPath(pkg);
  if (!includePaths.includes(ownBuildPath)) {
    includePaths.push(ownBuildPath);
  }

  // Build include args
  const includeArgs = includePaths.flatMap((p) => ["-I", p]);

  // Get suffix for output
  const suffix = getSuffix(rootConfig, specs[0] || { module: "esmodule" });
  const implBasename = getBasename(sourceFile.implementation.path);
  const namespace = pkg.namespace;
  const assetName = getAssetName(implBasename, namespace);

  // Build args
  const args: string[] = [
    "-runtime",
    buildState.compilerInfo.runtimePath,
    ...includeArgs,
    ...warningArgs,
    ...bscFlags,
    "-color",
    "always",
  ];

  // Add interface file if present
  if (sourceFile.interface !== undefined) {
    const ifaceBasename = getBasename(sourceFile.interface.path);
    args.push(`${ifaceBasename}.iast`);
  }

  // Add implementation file
  args.push(`${implBasename}.ast`);

  return args;
}

/**
 * Get asset name with namespace suffix
 */
function getAssetName(basename: string, namespace: Namespace): string {
  const suffix = namespaceToSuffix(namespace);
  if (suffix === undefined) {
    return basename;
  }
  return `${basename}-${suffix}`;
}

/**
 * Compile a single module
 */
function compileModule(
  buildState: BuildState,
  moduleName: string,
  warnErrorOverride?: string,
): { success: boolean; stderr: string } {
  const module = buildState.modules.get(moduleName);
  if (module === undefined) {
    return { success: false, stderr: `Module not found: ${moduleName}` };
  }

  const pkg = buildState.packages.get(module.packageName);
  if (pkg === undefined) {
    return { success: false, stderr: `Package not found: ${module.packageName}` };
  }

  if (module.sourceType.type === "mlMap") {
    // MlMap modules are already compiled during parsing
    return { success: true, stderr: "" };
  }

  const buildPath = getPackageBuildPath(pkg);
  const args = getCompilerArgs(buildState, pkg, module, warnErrorOverride);

  // Run bsc
  const result = spawnSync(buildState.compilerInfo.bscPath, args, {
    cwd: buildPath,
    encoding: "utf-8",
  });

  const stderr = result.stderr?.trim() ?? "";

  if (result.status !== 0) {
    return { success: false, stderr };
  }

  // Copy artifacts to ocaml build path
  const sourceFile = module.sourceType.sourceFile;
  const basename = getBasename(sourceFile.implementation.path);
  const assetName = getAssetName(basename, pkg.namespace);
  const ocamlBuildPath = getPackageOcamlBuildPath(pkg);

  createPathSync(ocamlBuildPath);

  for (const ext of ["cmi", "cmt", "cmj", "cmti"]) {
    try {
      fs.copyFileSync(
        path.join(buildPath, `${assetName}.${ext}`),
        path.join(ocamlBuildPath, `${assetName}.${ext}`),
      );
    } catch {
      // Ignore copy errors (some files may not exist)
    }
  }

  // Copy generated JS file
  const rootConfig =
    buildState.projectContext.monorepoContext?.type === "package"
      ? buildState.projectContext.monorepoContext.parentConfig
      : buildState.projectContext.currentConfig;

  const specs = getPackageSpecs(rootConfig);
  for (const spec of specs) {
    if (spec["in-source"] !== false) {
      const suffix = getSuffix(rootConfig, spec);
      const implPath = sourceFile.implementation.path;
      const jsPath = getSourceFileFromRescriptFile(implPath, suffix);
      const srcJsPath = path.join(pkg.path, jsPath);

      try {
        fs.copyFileSync(
          path.join(buildPath, `${assetName}${suffix}`),
          srcJsPath,
        );
      } catch {
        // JS file may not have been generated
      }
    }
  }

  return {
    success: true,
    stderr: containsAsciiCharacters(stderr) ? stderr : "",
  };
}

/**
 * Get the compile universe - all modules that might need compiling
 */
function getCompileUniverse(
  buildState: BuildState,
  dirtyModules: Set<string>,
): Set<string> {
  const universe = new Set(dirtyModules);
  let currentStep = new Set(dirtyModules);

  while (currentStep.size > 0) {
    const nextStep = new Set<string>();

    for (const moduleName of currentStep) {
      const module = buildState.modules.get(moduleName);
      if (module === undefined) continue;

      for (const dependent of module.dependents) {
        if (!universe.has(dependent)) {
          nextStep.add(dependent);
          universe.add(dependent);
        }
      }
    }

    currentStep = nextStep;
  }

  return universe;
}

/**
 * Get modules that are ready to compile (all deps are compiled)
 */
function getReadyModules(
  buildState: BuildState,
  toCompile: Set<string>,
  compiled: Set<string>,
): string[] {
  const ready: string[] = [];

  for (const moduleName of toCompile) {
    const module = buildState.modules.get(moduleName);
    if (module === undefined) continue;

    // Check if all deps in the compile universe are compiled
    let allDepsReady = true;
    for (const dep of module.deps) {
      if (toCompile.has(dep) && !compiled.has(dep)) {
        allDepsReady = false;
        break;
      }
    }

    if (allDepsReady) {
      ready.push(moduleName);
    }
  }

  return ready;
}

/**
 * Compile all dirty modules in dependency order
 */
export function compile(
  buildState: BuildState,
  warnErrorOverride?: string,
  showProgress: boolean = false,
  onProgress?: () => void,
): CompileResult {
  // Get dirty modules
  const dirtyModules = new Set<string>();
  for (const [name, module] of buildState.modules) {
    if (module.compileDirty) {
      dirtyModules.add(name);
    }
  }

  if (dirtyModules.size === 0) {
    return { errors: "", warnings: "", numCompiled: 0 };
  }

  // Check for cycles
  const cycle = findCycle(buildState.modules);
  if (cycle.length > 0) {
    const cycleStr = formatCycle(cycle, buildState);
    return {
      errors: `Circular dependency detected:\n${cycleStr}`,
      warnings: "",
      numCompiled: 0,
    };
  }

  // Get all modules that might need compiling
  const compileUniverse = getCompileUniverse(buildState, dirtyModules);
  const toCompile = new Set(compileUniverse);
  const compiled = new Set<string>();
  let errors = "";
  let warnings = "";
  let numCompiled = 0;

  // Compile in waves
  while (toCompile.size > 0) {
    const ready = getReadyModules(buildState, toCompile, compiled);

    if (ready.length === 0 && toCompile.size > 0) {
      // No progress possible - likely a cycle not caught earlier
      errors += `Unable to compile: possible circular dependency\n`;
      break;
    }

    for (const moduleName of ready) {
      onProgress?.();

      const module = buildState.modules.get(moduleName);
      if (module === undefined) continue;

      // Only compile if dirty
      if (!module.compileDirty) {
        toCompile.delete(moduleName);
        compiled.add(moduleName);
        continue;
      }

      const pkg = buildState.packages.get(module.packageName);
      if (pkg === undefined) continue;

      const result = compileModule(buildState, moduleName, warnErrorOverride);

      if (!result.success) {
        errors += result.stderr + "\n";
        logAppend(pkg, result.stderr);

        // Update module state
        if (module.sourceType.type === "sourceFile") {
          module.sourceType.sourceFile.implementation.compileState = "error";
        }
      } else if (result.stderr) {
        warnings += result.stderr + "\n";
        logAppend(pkg, result.stderr);

        // Update module state
        if (module.sourceType.type === "sourceFile") {
          module.sourceType.sourceFile.implementation.compileState = "warning";
        }
        numCompiled++;
      } else {
        // Update module state
        if (module.sourceType.type === "sourceFile") {
          module.sourceType.sourceFile.implementation.compileState = "success";
        }

        // Update last compiled times
        const buildPath = getPackageBuildPath(pkg);
        if (module.sourceType.type === "sourceFile") {
          const basename = getBasename(
            module.sourceType.sourceFile.implementation.path,
          );
          const assetName = getAssetName(basename, pkg.namespace);

          try {
            module.lastCompiledCmi = getLastModifiedSync(
              path.join(buildPath, `${assetName}.cmi`),
            );
          } catch {
            // Ignore
          }
          try {
            module.lastCompiledCmt = getLastModifiedSync(
              path.join(buildPath, `${assetName}.cmt`),
            );
          } catch {
            // Ignore
          }
        }

        numCompiled++;
      }

      module.compileDirty = false;
      toCompile.delete(moduleName);
      compiled.add(moduleName);
    }
  }

  return { errors, warnings, numCompiled };
}
