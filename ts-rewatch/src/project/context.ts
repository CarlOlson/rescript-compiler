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

/**
 * Get the root config from project context
 */
export function getRootConfig(context: ProjectContext): Config {
  if (context.monorepoContext?.type === "package") {
    return context.monorepoContext.parentConfig;
  }
  return context.currentConfig;
}

/**
 * Get the root path from project context
 */
export function getRootPath(context: ProjectContext): string {
  return context.rootPath;
}

/**
 * Get the scoped local packages for the current context
 * Returns the set of package names that are local to this build context
 */
export function getScopedLocalPackages(context: ProjectContext): Set<string> {
  const localPackages = new Set<string>();
  localPackages.add(context.currentConfig.name);

  if (context.monorepoContext?.type === "root") {
    for (const dep of context.monorepoContext.localDeps) {
      localPackages.add(dep);
    }
    for (const dep of context.monorepoContext.localDevDeps) {
      localPackages.add(dep);
    }
  }

  return localPackages;
}

/**
 * Check if the project context is for a monorepo
 */
export function isMonorepo(context: ProjectContext): boolean {
  return context.monorepoContext !== undefined;
}

/**
 * Check if the project context is for a monorepo root
 */
export function isMonorepoRoot(context: ProjectContext): boolean {
  return context.monorepoContext?.type === "root";
}

/**
 * Check if the project context is for a package within a monorepo
 */
export function isMonorepoPackage(context: ProjectContext): boolean {
  return context.monorepoContext?.type === "package";
}

/**
 * Format project context for debugging
 */
export function formatProjectContext(context: ProjectContext): string {
  if (context.monorepoContext === undefined) {
    return `Single project: "${context.currentConfig.name}" at "${context.currentConfig.path}"`;
  }

  if (context.monorepoContext.type === "root") {
    const deps = Array.from(context.monorepoContext.localDeps);
    const devDeps = Array.from(context.monorepoContext.localDevDeps);
    return `Monorepo root: "${context.currentConfig.name}" at "${context.currentConfig.path}" with dependencies: [${deps.join(", ")}] and devDependencies: [${devDeps.join(", ")}]`;
  }

  return `MonorepoPackage: "${context.currentConfig.name}" at "${context.currentConfig.path}" with parent "${context.monorepoContext.parentConfig.name}" at "${context.monorepoContext.parentConfig.path}"`;
}
