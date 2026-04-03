// Port from rewatch/src/build/packages.rs
// Package discovery and source file scanning

import * as fs from "node:fs";
import * as path from "node:path";
import type {
  Package,
  SourceFileMeta,
  ProjectContext,
  Module,
  Namespace,
  BuildState,
} from "../types/build.ts";
import type { Config, Source, PackageSource } from "../types/config.ts";
import { oneOrMoreToArray, toQualifiedWithoutChildren } from "../types/config.ts";
import { parseConfigSync, getNamespace, getConfigPath } from "../config/parser.ts";
import { isSourceFile, getExtension, packagePath, isLocalPackage } from "../utils/paths.ts";
import { filePathToModuleName, namespaceToSuffix } from "../utils/helpers.ts";

/**
 * Read a rescript.json config from a directory
 */
export function readConfig(packageDir: string): Config {
  const configPath = getConfigPath(packageDir);
  const config = parseConfigSync(configPath);
  config.path = configPath;
  return config;
}

/**
 * Get source directories from config sources
 */
function getSourceDirs(
  source: Source,
  subPath?: string,
): Set<PackageSource> {
  const result = new Set<PackageSource>();

  const sourceFolder = toQualifiedWithoutChildren(source, subPath);
  result.add(sourceFolder);

  // Handle subdirs
  if (typeof source !== "string" && Array.isArray(source.subdirs)) {
    const parentType = typeof source === "string" ? undefined : source.type;
    for (const subSource of source.subdirs) {
      const subSourceWithType =
        typeof subSource === "string"
          ? { dir: subSource, type: parentType }
          : { ...subSource, type: subSource.type ?? parentType };
      const nestedDirs = getSourceDirs(subSourceWithType, sourceFolder.dir);
      for (const dir of nestedDirs) {
        result.add(dir);
      }
    }
  }

  return result;
}

/**
 * Read source files from a directory
 */
function readSourceFiles(
  packageDir: string,
  sourceDir: string,
  recurse: boolean,
  isTypeDev: boolean,
): Map<string, SourceFileMeta> {
  const result = new Map<string, SourceFileMeta>();
  const fullPath = path.join(packageDir, sourceDir);

  try {
    const entries = fs.readdirSync(fullPath, { withFileTypes: true });

    for (const entry of entries) {
      if (entry.isDirectory() && recurse) {
        const subDir = path.join(sourceDir, entry.name);
        const subFiles = readSourceFiles(packageDir, subDir, recurse, isTypeDev);
        for (const [key, value] of subFiles) {
          result.set(key, value);
        }
      } else if (entry.isFile()) {
        const ext = getExtension(entry.name);
        if (isSourceFile(ext)) {
          const filePath = path.join(sourceDir, entry.name);
          const fullFilePath = path.join(packageDir, filePath);
          const stats = fs.statSync(fullFilePath);
          result.set(filePath, {
            modified: stats.mtimeMs,
            isTypeDev,
          });
        }
      }
    }
  } catch {
    // Directory doesn't exist or can't be read
  }

  return result;
}

/**
 * Read package name from package.json or rescript.json
 */
function readPackageName(packageDir: string): string {
  // Try package.json first
  try {
    const pkgJsonPath = path.join(packageDir, "package.json");
    const content = fs.readFileSync(pkgJsonPath, "utf-8");
    const json = JSON.parse(content);
    if (json.name) {
      return json.name;
    }
  } catch {
    // Ignore
  }

  // Fall back to rescript.json
  const configPath = getConfigPath(packageDir);
  const content = fs.readFileSync(configPath, "utf-8");
  const json = JSON.parse(content);
  return json.name;
}

/**
 * Make a package from a config
 */
function makePackage(
  config: Config,
  packagePath: string,
  isRoot: boolean,
  isLocalDep: boolean,
): Package {
  const sources = oneOrMoreToArray(config.sources);

  // Get all source folders
  const sourceFolders = new Set<PackageSource>();
  for (const source of sources) {
    const dirs = getSourceDirs(source);
    for (const dir of dirs) {
      sourceFolders.add(dir);
    }
  }

  // Read source files
  const sourceFiles = new Map<string, SourceFileMeta>();
  for (const sourceFolder of sourceFolders) {
    const isTypeDev = sourceFolder.type === "dev";
    const shouldRecurse = sourceFolder.subdirs === true;
    const files = readSourceFiles(
      packagePath,
      sourceFolder.dir,
      shouldRecurse,
      isTypeDev,
    );
    for (const [key, value] of files) {
      sourceFiles.set(key, value);
    }
  }

  const namespace = getNamespace(config);

  return {
    name: config.name,
    config,
    sourceFolders,
    sourceFiles,
    namespace,
    modules: undefined,
    path: packagePath,
    dirs: undefined,
    isLocalDep,
    isRoot,
  };
}

/**
 * Find a dependency package path
 */
function findDependencyPath(
  packageDir: string,
  depName: string,
  projectContext: ProjectContext,
): string | undefined {
  // Try package's node_modules first
  const localPath = packagePath(packageDir, depName);
  if (fs.existsSync(localPath)) {
    return fs.realpathSync(localPath);
  }

  // Try project root's node_modules
  const rootPath = packagePath(projectContext.rootPath, depName);
  if (fs.existsSync(rootPath)) {
    return fs.realpathSync(rootPath);
  }

  // Try current config's node_modules
  const configDir = path.dirname(projectContext.currentConfig.path ?? "");
  const currentPath = packagePath(configDir, depName);
  if (fs.existsSync(currentPath)) {
    return fs.realpathSync(currentPath);
  }

  return undefined;
}

/**
 * Discover all packages starting from the root
 */
export function discoverPackages(
  projectContext: ProjectContext,
): Map<string, Package> {
  const packages = new Map<string, Package>();
  const registeredDeps = new Set<string>();

  // Add root package
  const rootConfig = projectContext.currentConfig;
  const rootPath = path.dirname(rootConfig.path ?? projectContext.rootPath);
  const rootPackage = makePackage(rootConfig, rootPath, true, true);
  packages.set(rootPackage.name, rootPackage);
  registeredDeps.add(rootPackage.name);

  // Process dependencies recursively
  const processPackage = (pkgConfig: Config, pkgPath: string, isLocalDep: boolean) => {
    const deps = [
      ...(pkgConfig.dependencies ?? []),
      ...(isLocalDep ? pkgConfig["dev-dependencies"] ?? [] : []),
    ];

    for (const depName of deps) {
      if (registeredDeps.has(depName)) {
        continue;
      }
      registeredDeps.add(depName);

      const depPath = findDependencyPath(pkgPath, depName, projectContext);
      if (depPath === undefined) {
        console.error(`Could not find dependency: ${depName}`);
        continue;
      }

      try {
        const depConfig = readConfig(depPath);
        const depIsLocal = isLocalPackage(projectContext.rootPath, depPath);
        const depPackage = makePackage(depConfig, depPath, false, depIsLocal);
        packages.set(depPackage.name, depPackage);

        // Process nested dependencies
        processPackage(depConfig, depPath, depIsLocal);
      } catch (e) {
        console.error(`Could not read config for ${depName}: ${e}`);
      }
    }
  };

  processPackage(rootConfig, rootPath, true);

  return packages;
}

/**
 * Parse packages into modules
 */
export function parsePackages(
  packages: Map<string, Package>,
  buildState: BuildState,
): void {
  for (const pkg of packages.values()) {
    // Create module set for package
    const modules = new Set<string>();

    // Add namespace mlmap module if namespaced
    if (pkg.namespace.type !== "noNamespace") {
      const suffix = namespaceToSuffix(pkg.namespace);
      if (suffix) {
        modules.add(suffix);

        // Add mlmap module to build state
        const mlmapModule: Module = {
          sourceType: { type: "mlMap", mlMap: { parseDirty: true } },
          deps: new Set(),
          dependents: new Set(),
          packageName: pkg.name,
          compileDirty: true,
          parseDirty: true,
          depsDirty: true,
          lastCompiledCmi: undefined,
          lastCompiledCmt: undefined,
          isTypeDev: false,
        };
        buildState.modules.set(suffix, mlmapModule);
        buildState.moduleNames.add(suffix);
      }
    }

    // Process source files
    const sourceFiles = pkg.sourceFiles;
    if (sourceFiles === undefined) {
      pkg.modules = modules;
      continue;
    }

    // Group files by module
    const moduleFiles = new Map<string, { impl?: SourceFileMeta & { path: string }, iface?: SourceFileMeta & { path: string } }>();

    for (const [filePath, meta] of sourceFiles) {
      const moduleName = filePathToModuleName(filePath, pkg.namespace);
      const ext = getExtension(filePath);

      if (!moduleFiles.has(moduleName)) {
        moduleFiles.set(moduleName, {});
      }
      const entry = moduleFiles.get(moduleName)!;

      if (ext === "res") {
        entry.impl = { ...meta, path: filePath };
      } else if (ext === "resi") {
        entry.iface = { ...meta, path: filePath };
      }
    }

    // Create modules
    for (const [moduleName, files] of moduleFiles) {
      if (files.impl === undefined) {
        continue;
      }

      modules.add(moduleName);

      const module: Module = {
        sourceType: {
          type: "sourceFile",
          sourceFile: {
            implementation: {
              path: files.impl.path,
              parseState: "pending",
              compileState: "pending",
              lastModified: files.impl.modified,
              parseDirty: true,
              compileWarnings: undefined,
            },
            interface: files.iface
              ? {
                  path: files.iface.path,
                  parseState: "pending",
                  compileState: "pending",
                  lastModified: files.iface.modified,
                  parseDirty: true,
                  compileWarnings: undefined,
                }
              : undefined,
          },
        },
        deps: new Set(),
        dependents: new Set(),
        packageName: pkg.name,
        compileDirty: true,
        parseDirty: true,
        depsDirty: true,
        lastCompiledCmi: undefined,
        lastCompiledCmt: undefined,
        isTypeDev: files.impl.isTypeDev,
      };

      buildState.modules.set(moduleName, module);
      buildState.moduleNames.add(moduleName);
    }

    pkg.modules = modules;
  }
}
