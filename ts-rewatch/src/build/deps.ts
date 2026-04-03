// Port from rewatch/src/build/deps.rs
// Module dependency analysis

import * as path from "node:path";
import type { BuildState, Module, Package, Namespace } from "../types/build.ts";
import { getPackageBuildPath, namespaceToSuffix } from "../types/build.ts";
import { getAstPath } from "../utils/paths.ts";
import { readLinesSync } from "../utils/helpers.ts";

/**
 * Get dependency modules from an AST file
 */
function getDepModules(
  astFile: string,
  namespace: string | undefined,
  packageModules: Set<string>,
  validModules: Set<string>,
  pkg: Package,
  buildState: BuildState,
): Set<string> {
  const deps = new Set<string>();
  const astFilePath = path.join(getPackageBuildPath(pkg), astFile);

  try {
    const lines = readLinesSync(astFilePath);

    // Skip the first line which has null characters
    // Following lines are dependency modules
    // Stop when we hit a line that is an absolute path
    for (let i = 1; i < lines.length; i++) {
      const line = lines[i].trim();
      if (path.isAbsolute(line)) {
        break;
      }
      if (line.length > 0) {
        deps.add(line);
      }
    }
  } catch {
    throw new Error(`Could not read file ${astFilePath}`);
  }

  // Get allowed dependency packages
  const allowedDependencies = new Set<string>([
    ...(pkg.config.dependencies ?? []),
    ...(pkg.config["dev-dependencies"] ?? []),
  ]);

  const result = new Set<string>();

  for (const dep of deps) {
    const parts = dep.split(".");
    const depFirst = parts[0];
    const depSecond = parts[1];

    let resolvedDep: string;

    if (namespace !== undefined) {
      // If the module is in the own namespace, take the submodule
      // e.g., TeamwalnutApp.MyModule inside namespace TeamwalnutApp -> MyModule
      const moduleName =
        depSecond !== undefined && depFirst === namespace ? depSecond : depFirst;

      const namespacedName = `${moduleName}-${namespace}`;

      if (packageModules.has(namespacedName) || validModules.has(namespacedName)) {
        resolvedDep = namespacedName;
      } else {
        resolvedDep = moduleName;
      }
    } else {
      resolvedDep = depFirst;
    }

    // Check if module exists
    const moduleExists =
      validModules.has(resolvedDep) &&
      (namespace === undefined || resolvedDep !== namespace);

    if (!moduleExists) {
      continue;
    }

    const depModule = buildState.modules.get(resolvedDep);
    if (depModule !== undefined) {
      // Same package is always allowed
      if (depModule.packageName === pkg.name) {
        result.add(resolvedDep);
        continue;
      }

      // Different package must be a declared dependency
      if (allowedDependencies.has(depModule.packageName)) {
        result.add(resolvedDep);
      }
    } else {
      result.add(resolvedDep);
    }
  }

  return result;
}

/**
 * Analyze dependencies for all modules in the build state
 */
export function getDeps(
  buildState: BuildState,
  deletedModules: Set<string>,
): void {
  // Union of all module names including deleted ones
  const allModules = new Set([...buildState.moduleNames, ...deletedModules]);

  // Collect deps for each module
  const moduleDepsList: [string, Set<string>][] = [];

  for (const [moduleName, module] of buildState.modules) {
    if (module.sourceType.type === "mlMap") {
      moduleDepsList.push([moduleName, new Set(module.deps)]);
      continue;
    }

    const pkg = buildState.packages.get(module.packageName);
    if (pkg === undefined) {
      throw new Error(`Package not found: ${module.packageName}`);
    }

    const sourceFile = module.sourceType.sourceFile;
    const astPath = getAstPath(sourceFile.implementation.path);

    if (module.depsDirty || !buildState.depsInitialized) {
      const namespace = namespaceToSuffix(pkg.namespace);

      let deps = getDepModules(
        astPath,
        namespace,
        pkg.modules ?? new Set(),
        allModules,
        pkg,
        buildState,
      );

      // If there's an interface, add its dependencies too
      if (sourceFile.interface !== undefined) {
        const iastPath = getAstPath(sourceFile.interface.path);
        const interfaceDeps = getDepModules(
          iastPath,
          namespace,
          pkg.modules ?? new Set(),
          allModules,
          pkg,
          buildState,
        );
        for (const dep of interfaceDeps) {
          deps.add(dep);
        }
      }

      // If this is a namespace entry module, add the namespace as dependency
      if (
        pkg.namespace.type === "namespaceWithEntry" &&
        pkg.namespace.entry === moduleName
      ) {
        const suffix = namespaceToSuffix(pkg.namespace);
        if (suffix !== undefined) {
          deps.add(suffix);
        }
      }

      // Remove self-reference
      deps.delete(moduleName);

      moduleDepsList.push([moduleName, deps]);
    } else {
      moduleDepsList.push([moduleName, new Set(module.deps)]);
    }
  }

  // Update modules with computed deps and populate dependents
  for (const [moduleName, deps] of moduleDepsList) {
    const module = buildState.modules.get(moduleName);
    if (module !== undefined) {
      module.deps = deps;
      module.depsDirty = false;
    }

    // Add reverse dependencies (dependents)
    for (const depName of deps) {
      const depModule = buildState.modules.get(depName);
      if (depModule !== undefined) {
        depModule.dependents.add(moduleName);
      }
    }
  }

  buildState.depsInitialized = true;
}

/**
 * Mark a module and all its dependents as compile dirty
 */
export function markDirtyDependents(
  moduleName: string,
  modules: Map<string, Module>,
  visited: Set<string> = new Set(),
): void {
  if (visited.has(moduleName)) {
    return;
  }
  visited.add(moduleName);

  const module = modules.get(moduleName);
  if (module === undefined) {
    return;
  }

  for (const dependent of module.dependents) {
    const depModule = modules.get(dependent);
    if (depModule !== undefined && !depModule.compileDirty) {
      depModule.compileDirty = true;
      markDirtyDependents(dependent, modules, visited);
    }
  }
}

/**
 * Get all transitive dependencies of a module
 */
export function getTransitiveDeps(
  moduleName: string,
  modules: Map<string, Module>,
  visited: Set<string> = new Set(),
): Set<string> {
  if (visited.has(moduleName)) {
    return new Set();
  }
  visited.add(moduleName);

  const module = modules.get(moduleName);
  if (module === undefined) {
    return new Set();
  }

  const result = new Set<string>();
  for (const dep of module.deps) {
    result.add(dep);
    const transitive = getTransitiveDeps(dep, modules, visited);
    for (const t of transitive) {
      result.add(t);
    }
  }

  return result;
}
