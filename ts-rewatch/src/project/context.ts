// Port from rewatch/src/project_context.rs
// Project context for monorepo detection and management

import * as fs from "node:fs";
import * as path from "node:path";
import type { Config } from "../types/config.ts";
import type { ProjectContext } from "../types/build.ts";
import { parseConfigSync, getConfigPath } from "../config/parser.ts";
import { isLocalPackage } from "../utils/paths.ts";
import { getNearestConfig } from "../utils/helpers.ts";

/**
 * Read local packages from node_modules that are symlinked (monorepo deps)
 */
function readLocalPackages(
  folderPath: string,
  dependencies: string[],
): Set<string> {
  const localDependencies = new Set<string>();

  for (const dep of dependencies) {
    const depPath = path.join(folderPath, "node_modules", dep);
    try {
      const realPath = fs.realpathSync(depPath);
      if (isLocalPackage(folderPath, realPath)) {
        localDependencies.add(dep);
      }
    } catch {
      // Dependency not found in node_modules
    }
  }

  return localDependencies;
}

/**
 * Check if the current config is listed in the workspace config
 */
function isConfigListedInWorkspace(
  currentConfig: Config,
  workspaceConfig: Config,
): boolean {
  const deps = workspaceConfig.dependencies ?? [];
  const devDeps = workspaceConfig["dev-dependencies"] ?? [];
  return (
    deps.includes(currentConfig.name) || devDeps.includes(currentConfig.name)
  );
}

/**
 * Determine if project is a monorepo or single project
 */
function monorepoOrSingleProject(
  projectPath: string,
  currentConfig: Config,
): ProjectContext {
  const localDeps = readLocalPackages(
    projectPath,
    currentConfig.dependencies ?? [],
  );
  const localDevDeps = readLocalPackages(
    projectPath,
    currentConfig["dev-dependencies"] ?? [],
  );

  if (localDeps.size === 0 && localDevDeps.size === 0) {
    return {
      currentConfig,
      rootPath: projectPath,
      monorepoContext: undefined,
    };
  }

  return {
    currentConfig,
    rootPath: projectPath,
    monorepoContext: {
      type: "root",
      localDeps,
      localDevDeps,
    },
  };
}

/**
 * Create a project context from a folder path
 */
export function createProjectContext(folder: string): ProjectContext {
  const projectPath = path.resolve(folder);
  const configPath = getConfigPath(projectPath);

  // Read the current config
  const currentConfig = parseConfigSync(configPath);
  currentConfig.path = configPath;

  // Look for a parent config
  const parentDir = path.dirname(projectPath);
  const nearestParentConfigDir = getNearestConfig(parentDir);

  if (nearestParentConfigDir === undefined) {
    // No parent config, check if this is a monorepo root or single project
    return monorepoOrSingleProject(projectPath, currentConfig);
  }

  // There's a parent config - check if it references this package
  try {
    const parentConfigPath = getConfigPath(nearestParentConfigDir);
    const workspaceConfig = parseConfigSync(parentConfigPath);
    workspaceConfig.path = parentConfigPath;

    if (isConfigListedInWorkspace(currentConfig, workspaceConfig)) {
      // This is a package within a monorepo
      return {
        currentConfig,
        rootPath: nearestParentConfigDir,
        monorepoContext: {
          type: "package",
          parentConfig: workspaceConfig,
        },
      };
    }

    // Parent doesn't reference this package, but this could still be a monorepo root
    return monorepoOrSingleProject(projectPath, currentConfig);
  } catch {
    // Couldn't read parent config, treat as single project or monorepo root
    return monorepoOrSingleProject(projectPath, currentConfig);
  }
}
