// Port from rewatch/src/build/build_types.rs and rewatch/src/build/packages.rs
// Core build state types

import type { CompilerInfo, SourceType } from "./compiler.ts";
import type { Config, PackageSource } from "./config.ts";

// Re-export for convenience
export type { SourceType, CompilerInfo };

export type Namespace =
  | { type: "namespace"; name: string }
  | { type: "namespaceWithEntry"; name: string; entry: string }
  | { type: "noNamespace" };

/**
 * Get the namespace suffix string
 */
export function namespaceToSuffix(namespace: Namespace): string | undefined {
  switch (namespace.type) {
    case "namespace":
      return namespace.name;
    case "namespaceWithEntry":
      return `@${namespace.name}`;
    case "noNamespace":
      return undefined;
  }
}

/**
 * Metadata for a source file
 */
export interface SourceFileMeta {
  modified: number;
  isTypeDev: boolean;
}

/**
 * A module in the build system
 */
export interface Module {
  sourceType: SourceType;
  deps: Set<string>;
  dependents: Set<string>;
  packageName: string;
  compileDirty: boolean;
  parseDirty: boolean;
  depsDirty: boolean;
  lastCompiledCmi: number | undefined;
  lastCompiledCmt: number | undefined;
  isTypeDev: boolean;
}

/**
 * A package in the build system
 */
export interface Package {
  name: string;
  config: Config;
  sourceFolders: Set<PackageSource>;
  // These are the relative file paths (relative to the package root)
  sourceFiles: Map<string, SourceFileMeta> | undefined;
  namespace: Namespace;
  modules: Set<string> | undefined;
  // Canonicalized dir of the package
  path: string;
  dirs: Set<string> | undefined;
  isLocalDep: boolean;
  isRoot: boolean;
}

/**
 * Get the lib/bs build path for a package
 */
function getBuildPath(canonicalPath: string): string {
  return `${canonicalPath}/lib/bs`;
}

/**
 * Get the lib/ocaml path for a package
 */
function getOcamlBuildPath(canonicalPath: string): string {
  return `${canonicalPath}/lib/ocaml`;
}

/**
 * Get the build path for a package
 */
export function getPackageBuildPath(pkg: Package): string {
  return getBuildPath(pkg.path);
}

/**
 * Get the ocaml build path for a package
 */
export function getPackageOcamlBuildPath(pkg: Package): string {
  return getOcamlBuildPath(pkg.path);
}

/**
 * Get the compiler-info.json path for a package
 */
export function getPackageCompilerInfoPath(pkg: Package): string {
  return `${getPackageBuildPath(pkg)}/compiler-info.json`;
}

/**
 * Project context for monorepo detection
 */
export type MonoRepoContext =
  | { type: "root"; localDeps: Set<string>; localDevDeps: Set<string> }
  | { type: "package"; parentConfig: Config };

/**
 * Project context containing the current config and root path
 */
export interface ProjectContext {
  currentConfig: Config;
  rootPath: string;
  monorepoContext: MonoRepoContext | undefined;
}

/**
 * Core build state containing all the essential data needed for compilation.
 */
export interface BuildState {
  projectContext: ProjectContext;
  modules: Map<string, Module>;
  packages: Map<string, Package>;
  moduleNames: Set<string>;
  deletedModules: Set<string>;
  compilerInfo: CompilerInfo;
  depsInitialized: boolean;
}

/**
 * Create a new BuildState
 */
export function createBuildState(
  projectContext: ProjectContext,
  packages: Map<string, Package>,
  compilerInfo: CompilerInfo,
): BuildState {
  return {
    projectContext,
    modules: new Map(),
    packages,
    moduleNames: new Set(),
    deletedModules: new Set(),
    compilerInfo,
    depsInitialized: false,
  };
}
