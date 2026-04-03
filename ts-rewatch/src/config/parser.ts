// Port from rewatch/src/config.rs
// Configuration parsing and validation

import * as fs from "node:fs/promises";
import * as fsSync from "node:fs";
import * as path from "node:path";
import type {
  Config,
  Source,
  PackageSource,
  PackageSpec,
} from "../types/config.ts";
import { oneOrMoreToArray, namespaceFromPackageName } from "../types/config.ts";
import type { Namespace } from "../types/build.ts";

/**
 * Parse a rescript.json file
 */
export async function parseConfig(filePath: string): Promise<Config> {
  const content = await fs.readFile(filePath, "utf-8");
  const config = parseConfigFromString(content);
  config.path = filePath;
  return config;
}

/**
 * Parse a rescript.json file synchronously
 */
export function parseConfigSync(filePath: string): Config {
  const content = fsSync.readFileSync(filePath, "utf-8");
  const config = parseConfigFromString(content);
  config.path = filePath;
  return config;
}

/**
 * Parse rescript.json from a string
 */
export function parseConfigFromString(configStr: string): Config {
  const config = JSON.parse(configStr) as Config;
  validateConfig(config);
  return config;
}

/**
 * Validate a parsed config
 */
function validateConfig(config: Config): void {
  // Validate package-specs
  validatePackageSpecs(config);
}

/**
 * Validate package-specs for duplicate suffixes
 */
function validatePackageSpecs(config: Config): void {
  const specs = config["package-specs"];
  if (specs === undefined) {
    return;
  }

  const topLevelSuffix = config.suffix ?? ".js";
  const seen = new Set<string>();

  const specList = oneOrMoreToArray(specs);
  for (const spec of specList) {
    validatePackageSpecModule(spec);
    const suffix = spec.suffix ?? topLevelSuffix;
    const inSource = spec["in-source"] ?? true;
    const key = `${suffix}:${inSource}`;

    if (seen.has(key)) {
      throw new Error(
        `Duplicate package-spec suffix "${suffix}" is not allowed.`,
      );
    }
    seen.add(key);
  }
}

/**
 * Validate package spec module
 */
function validatePackageSpecModule(spec: PackageSpec): void {
  if (spec.module !== "commonjs" && spec.module !== "esmodule") {
    throw new Error(
      `Module system "${spec.module}" is unsupported. Expected "commonjs" or "esmodule".`,
    );
  }
}

/**
 * Get the namespace for a config
 */
export function getNamespace(config: Config): Namespace {
  const namespaceFromPackage = namespaceFromPackageName(config.name);
  const namespaceConfig = config.namespace;
  const namespaceEntry = config["namespace-entry"];

  // No namespace config or false
  if (namespaceConfig === undefined || namespaceConfig === false) {
    return { type: "noNamespace" };
  }

  // Boolean true
  if (namespaceConfig === true) {
    if (namespaceEntry !== undefined) {
      return {
        type: "namespaceWithEntry",
        name: namespaceFromPackage,
        entry: namespaceEntry,
      };
    }
    return { type: "namespace", name: namespaceFromPackage };
  }

  // String namespace
  if (namespaceConfig === "true") {
    if (namespaceEntry !== undefined) {
      return {
        type: "namespaceWithEntry",
        name: namespaceFromPackage,
        entry: namespaceEntry,
      };
    }
    return { type: "namespace", name: namespaceFromPackage };
  }

  // Check if it's UpperFlat case (all uppercase)
  const isUpperFlat = /^[A-Z][A-Z0-9]*$/.test(namespaceConfig);
  const namespaceName = isUpperFlat
    ? namespaceConfig
    : namespaceFromPackageName(namespaceConfig);

  if (namespaceEntry !== undefined) {
    return {
      type: "namespaceWithEntry",
      name: namespaceName,
      entry: namespaceEntry,
    };
  }
  return { type: "namespace", name: namespaceName };
}

/**
 * Get all source folders from a config, flattened
 */
export function getSourceFolders(config: Config): PackageSource[] {
  const sources = config.sources;
  if (sources === undefined) {
    return [];
  }

  const sourceList = oneOrMoreToArray(sources);
  return flattenSources(sourceList, undefined);
}

/**
 * Flatten sources recursively
 */
function flattenSources(
  sources: Source[],
  parentPath: string | undefined,
): PackageSource[] {
  const result: PackageSource[] = [];

  for (const source of sources) {
    const flattened = flattenSource(source, parentPath);
    result.push(...flattened);
  }

  return result;
}

/**
 * Flatten a single source
 */
function flattenSource(
  source: Source,
  parentPath: string | undefined,
): PackageSource[] {
  if (typeof source === "string") {
    const dir = parentPath ? path.join(parentPath, source) : source;
    return [{ dir }];
  }

  const dir = parentPath ? path.join(parentPath, source.dir) : source.dir;
  const base: PackageSource = {
    dir,
    subdirs: source.subdirs,
    type: source.type,
  };

  const result: PackageSource[] = [base];

  // If subdirs is an array, flatten recursively
  if (Array.isArray(source.subdirs)) {
    const nested = flattenSources(source.subdirs, dir);
    result.push(...nested);
  }

  return result;
}

/**
 * Get the directories to watch/scan for a package source
 */
export function getDirectoriesForSource(
  packageDir: string,
  source: PackageSource,
): string[] {
  const baseDir = path.join(packageDir, source.dir);
  const result: string[] = [baseDir];

  // If subdirs is true (recurse), we need to scan recursively
  // This is handled during scanning, not here
  // If subdirs is an array, add each subdir
  if (Array.isArray(source.subdirs)) {
    for (const subdir of source.subdirs) {
      const subdirPath = typeof subdir === "string" ? subdir : subdir.dir;
      result.push(path.join(baseDir, subdirPath));
    }
  }

  return result;
}

/**
 * Check if a source folder should recurse into subdirectories
 */
export function shouldRecurse(source: PackageSource): boolean {
  return source.subdirs === true;
}

/**
 * Get all subdirs as Source array
 */
export function getSubdirs(source: PackageSource): Source[] {
  if (Array.isArray(source.subdirs)) {
    return source.subdirs;
  }
  return [];
}

/**
 * Check if a path is within a dev source
 */
export function isPathInDevSource(
  config: Config,
  relativePath: string,
): boolean {
  const sources = config.sources;
  if (sources === undefined) {
    return false;
  }

  const parent = path.dirname(relativePath);
  const sourceList = oneOrMoreToArray(sources);

  return sourceList.some((source) => {
    if (typeof source === "string") {
      return false;
    }
    if (source.type !== "dev") {
      return false;
    }

    return isPathInSource(source, parent);
  });
}

/**
 * Check if a path is within a source
 */
function isPathInSource(source: PackageSource, targetPath: string): boolean {
  const normalizedTarget = path.normalize(targetPath);
  const normalizedDir = path.normalize(source.dir);

  // Exact match
  if (normalizedTarget === normalizedDir) {
    return true;
  }

  // Check if target is under dir
  if (!normalizedTarget.startsWith(normalizedDir + path.sep)) {
    return false;
  }

  // If subdirs is true (recurse), any path under dir matches
  if (source.subdirs === true) {
    return true;
  }

  // If subdirs is false or undefined, only exact match counts
  if (source.subdirs === false || source.subdirs === undefined) {
    return false;
  }

  // subdirs is an array - check each
  const relativePath = normalizedTarget.slice(normalizedDir.length + 1);
  return source.subdirs.some((subdir) => {
    if (typeof subdir === "string") {
      return (
        relativePath === subdir || relativePath.startsWith(subdir + path.sep)
      );
    }
    return isPathInSource(
      { ...subdir, dir: path.join(source.dir, subdir.dir) },
      targetPath,
    );
  });
}

/**
 * Get the config file path for a directory
 */
export function getConfigPath(dir: string): string {
  return path.join(dir, "rescript.json");
}

/**
 * Check if a directory has a rescript.json file
 */
export function hasConfig(dir: string): boolean {
  return fsSync.existsSync(getConfigPath(dir));
}

/**
 * Get unknown fields in the config (fields not in the schema)
 */
export function getUnknownFields(config: Config): string[] {
  const knownFields = new Set([
    "name",
    "sources",
    "package-specs",
    "warnings",
    "suffix",
    "dependencies",
    "dev-dependencies",
    "ppx-flags",
    "compiler-flags",
    "namespace",
    "jsx",
    "experimental-features",
    "gentypeconfig",
    "js-post-build",
    "editor",
    "reanalyze",
    "namespace-entry",
    "allowed-dependents",
    "path",
  ]);

  const unsupportedFields = new Set([
    "ignored-dirs",
    "generators",
    "cut-generators",
    "pp-flags",
    "entries",
    "bs-external-includes",
  ]);

  const unknown: string[] = [];
  for (const key of Object.keys(config)) {
    if (!knownFields.has(key) && !unsupportedFields.has(key)) {
      unknown.push(key);
    }
  }
  return unknown;
}

/**
 * Get unsupported fields in the config
 */
export function getUnsupportedFields(config: Config): string[] {
  const unsupportedFields = new Set([
    "ignored-dirs",
    "generators",
    "cut-generators",
    "pp-flags",
    "entries",
    "bs-external-includes",
  ]);

  const unsupported: string[] = [];
  for (const key of Object.keys(config)) {
    if (unsupportedFields.has(key)) {
      unsupported.push(key);
    }
  }
  return unsupported;
}
