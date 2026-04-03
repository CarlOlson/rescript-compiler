// Port from rewatch/src/build/build_types.rs and rewatch/src/build/packages.rs
// Core build state types

import type {
  CompileState,
  CompilerInfo,
  Implementation,
  Interface,
  MlMap,
  ParseState,
  SourceFile,
  SourceType,
} from "./compiler.ts";
import type { Config, PackageSource } from "./config.ts";

// Re-export for convenience
export type {
  ParseState,
  CompileState,
  Implementation,
  Interface,
  SourceFile,
  MlMap,
  SourceType,
  CompilerInfo,
};

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
 * Check if a module is an mlmap module
 */
export function isModuleMlmap(module: Module): boolean {
  return module.sourceType.type === "mlMap";
}

/**
 * Get the interface from a module if it's a source file
 */
export function getModuleInterface(module: Module): Interface | undefined {
  if (module.sourceType.type === "sourceFile") {
    return module.sourceType.sourceFile.interface;
  }
  return undefined;
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
export function getBuildPath(canonicalPath: string): string {
  return `${canonicalPath}/lib/bs`;
}

/**
 * Get the lib/js path for a package
 */
export function getJsPath(canonicalPath: string): string {
  return `${canonicalPath}/lib/js`;
}

/**
 * Get the lib/es6 path for a package
 */
export function getEsmodulePath(canonicalPath: string): string {
  return `${canonicalPath}/lib/es6`;
}

/**
 * Get the lib/ocaml path for a package
 */
export function getOcamlBuildPath(canonicalPath: string): string {
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
 * Get the js path for a package
 */
export function getPackageJsPath(pkg: Package): string {
  return getJsPath(pkg.path);
}

/**
 * Get the esmodule path for a package
 */
export function getPackageEsmodulePath(pkg: Package): string {
  return getEsmodulePath(pkg.path);
}

/**
 * Get the compiler-info.json path for a package
 */
export function getPackageCompilerInfoPath(pkg: Package): string {
  return `${getPackageBuildPath(pkg)}/compiler-info.json`;
}

/**
 * Get the mlmap path for a package
 */
export function getPackageMlmapPath(pkg: Package): string {
  const suffix = namespaceToSuffix(pkg.namespace);
  if (suffix === undefined) {
    throw new Error("namespace should be set for mlmap module");
  }
  return `${getPackageBuildPath(pkg)}/${suffix}.mlmap`;
}

/**
 * Get the mlmap compile path for a package
 */
export function getPackageMlmapCompilePath(pkg: Package): string {
  const suffix = namespaceToSuffix(pkg.namespace);
  if (suffix === undefined) {
    throw new Error("namespace should be set for mlmap module");
  }
  return `${getPackageBuildPath(pkg)}/${suffix}.cmi`;
}

/**
 * Check if a source file is type dev in a package
 */
export function isPackageSourceFileTypeDev(
  pkg: Package,
  path: string,
): boolean {
  return pkg.sourceFiles?.get(path)?.isTypeDev ?? false;
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
 * Get the root config from project context
 */
export function getProjectRootConfig(context: ProjectContext): Config {
  if (context.monorepoContext?.type === "package") {
    return context.monorepoContext.parentConfig;
  }
  return context.currentConfig;
}

/**
 * Get the root path from project context
 */
export function getProjectRootPath(context: ProjectContext): string {
  return context.rootPath;
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
 * Extended build state that includes command-line specific overrides.
 */
export interface BuildCommandState {
  buildState: BuildState;
  // Command-line --warn-error flag override (takes precedence over rescript.json config)
  warnErrorOverride: string | undefined;
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

/**
 * Create a new BuildCommandState
 */
export function createBuildCommandState(
  projectContext: ProjectContext,
  packages: Map<string, Package>,
  compilerInfo: CompilerInfo,
  warnErrorOverride: string | undefined,
): BuildCommandState {
  return {
    buildState: createBuildState(projectContext, packages, compilerInfo),
    warnErrorOverride,
  };
}

/**
 * Get a package from the build state
 */
export function getPackage(
  state: BuildState,
  packageName: string,
): Package | undefined {
  return state.packages.get(packageName);
}

/**
 * Get a module from the build state
 */
export function getModule(
  state: BuildState,
  moduleName: string,
): Module | undefined {
  return state.modules.get(moduleName);
}

/**
 * Insert a module into the build state
 */
export function insertModule(
  state: BuildState,
  moduleName: string,
  module: Module,
): void {
  state.modules.set(moduleName, module);
  state.moduleNames.add(moduleName);
}

/**
 * AST module information
 */
export interface AstModule {
  moduleName: string;
  packageName: string;
  namespace: Namespace;
  lastModified: number;
  astFilePath: string;
  isRoot: boolean;
  suffix: string;
}

/**
 * Compile assets state for tracking build artifacts
 */
export interface CompileAssetsState {
  astModules: Map<string, AstModule>;
  cmiModules: Map<string, number>;
  cmtModules: Map<string, number>;
  astRescriptFileLocations: Set<string>;
  rescriptFileLocations: Set<string>;
}
