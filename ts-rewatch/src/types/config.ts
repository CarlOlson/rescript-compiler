// Port from rewatch/src/config.rs
// Configuration types for rescript.json

export type OneOrMore<T> = T | T[];

export type Subdirs = Source[] | boolean;

export interface PackageSource {
  dir: string;
  subdirs?: Subdirs;
  type?: string;
}

export type Source = string | PackageSource;

export type PackageModule = "commonjs" | "esmodule";

export interface PackageSpec {
  module: PackageModule;
  "in-source"?: boolean;
  suffix?: string;
}

export type WarningError = boolean | string;

export interface Warnings {
  number?: string;
  error?: WarningError;
}

export type NamespaceConfig = boolean | string;

export type JsxMode = "classic" | "automatic";

export type JsxModule = "react" | string;

export interface JsxSpecs {
  version?: number;
  module?: JsxModule;
  mode?: JsxMode;
  "v3-dependencies"?: string[];
  preserve?: boolean;
}

// GenType config is loaded by bsc, we don't need the internal structure
export type GenTypeConfig = Record<string, unknown>;

export interface JsPostBuild {
  cmd: string;
}

export type ExperimentalFeature = "LetUnwrap";

export interface Config {
  name: string;
  sources?: OneOrMore<Source>;
  "package-specs"?: OneOrMore<PackageSpec>;
  warnings?: Warnings;
  suffix?: string;
  dependencies?: string[];
  "dev-dependencies"?: string[];
  "ppx-flags"?: OneOrMore<string>[];
  "compiler-flags"?: OneOrMore<string>[];
  namespace?: NamespaceConfig;
  jsx?: JsxSpecs;
  "experimental-features"?: Record<ExperimentalFeature, boolean>;
  gentypeconfig?: GenTypeConfig;
  "js-post-build"?: JsPostBuild;
  editor?: Record<string, unknown>;
  reanalyze?: Record<string, unknown>;
  "namespace-entry"?: string;
  "allowed-dependents"?: string[];
  // Internal: path to the config file (not in JSON)
  path?: string;
}

/**
 * Get the type from a source
 */
function getSourceType(source: Source): string | undefined {
  if (typeof source === "string") {
    return undefined;
  }
  return source.type;
}

/**
 * Convert a Source to a PackageSource without children (flattened)
 */
export function toQualifiedWithoutChildren(
  source: Source,
  subPath?: string,
): PackageSource {
  if (typeof source === "string") {
    const dir = subPath ? `${subPath}/${source}` : source;
    return { dir, type: getSourceType(source) };
  }

  const dir = subPath ? `${subPath}/${source.dir}` : source.dir;

  if (source.subdirs === true || source.subdirs === false) {
    return {
      dir,
      subdirs: source.subdirs,
      type: source.type,
    };
  }

  // For qualified subdirs, we only keep the recurse info
  return {
    dir,
    type: source.type,
  };
}

/**
 * Flatten OneOrMore to array
 */
export function oneOrMoreToArray<T>(value: OneOrMore<T> | undefined): T[] {
  if (value === undefined) {
    return [];
  }
  if (Array.isArray(value)) {
    return value;
  }
  return [value];
}

/**
 * Flatten flags from config (OneOrMore<string>[])
 */
export function flattenFlags(flags: OneOrMore<string>[] | undefined): string[] {
  if (!flags) {
    return [];
  }
  return flags
    .flatMap((flag) => (Array.isArray(flag) ? flag : [flag]))
    .flatMap((flag) => flag.split(" "))
    .filter((flag) => flag.length > 0);
}

/**
 * Default suffix is .js
 */
const DEFAULT_SUFFIX = ".js";

/**
 * Get package specs from config with defaults
 */
export function getPackageSpecs(config: Config): PackageSpec[] {
  const specs = config["package-specs"];
  if (specs === undefined) {
    return [
      {
        module: "esmodule",
        "in-source": true,
        suffix: ".js",
      },
    ];
  }
  return oneOrMoreToArray(specs);
}

/**
 * Get the suffix for a config and spec
 */
export function getSuffix(config: Config, spec: PackageSpec): string {
  return spec.suffix ?? config.suffix ?? DEFAULT_SUFFIX;
}

/**
 * Get warning args for bsc
 */
export function getWarningArgs(
  config: Config,
  isLocalDep: boolean,
  warnErrorOverride?: string,
): string[] {
  // Ignore warning config for non-local dependencies (node_module dependencies)
  if (!isLocalDep) {
    return [];
  }

  // Command-line --warn-error flag takes precedence over rescript.json configuration
  if (warnErrorOverride !== undefined) {
    return ["-warn-error", warnErrorOverride];
  }

  const warnings = config.warnings;
  if (!warnings) {
    return [];
  }

  const result: string[] = [];

  if (warnings.number !== undefined) {
    result.push("-w", warnings.number);
  }

  if (warnings.error !== undefined) {
    if (warnings.error === true) {
      result.push("-warn-error", "A");
    } else if (typeof warnings.error === "string") {
      result.push("-warn-error", warnings.error);
    }
  }

  return result;
}

/**
 * Get JSX args for bsc
 */
export function getJsxArgs(config: Config): string[] {
  const jsx = config.jsx;
  if (!jsx || jsx.version === undefined) {
    return [];
  }
  if (jsx.version === 4) {
    return ["-bs-jsx", jsx.version.toString()];
  }
  throw new Error(`JSX version ${jsx.version} is unsupported`);
}

/**
 * Get JSX mode args for bsc
 */
export function getJsxModeArgs(config: Config): string[] {
  const jsx = config.jsx;
  if (!jsx || jsx.mode === undefined) {
    return [];
  }
  return ["-bs-jsx-mode", jsx.mode];
}

/**
 * Get JSX module args for bsc
 */
export function getJsxModuleArgs(config: Config): string[] {
  const jsx = config.jsx;
  if (!jsx || jsx.module === undefined) {
    return [];
  }
  return ["-bs-jsx-module", jsx.module];
}

/**
 * Get JSX preserve args for bsc
 */
export function getJsxPreserveArgs(config: Config): string[] {
  const jsx = config.jsx;
  if (!jsx || !jsx.preserve) {
    return [];
  }
  return ["-bs-jsx-preserve"];
}

/**
 * Get experimental features args for bsc
 */
export function getExperimentalFeaturesArgs(config: Config): string[] {
  const features = config["experimental-features"];
  if (!features) {
    return [];
  }
  const result: string[] = [];
  for (const [feature, enabled] of Object.entries(features)) {
    if (enabled) {
      result.push("-enable-experimental", feature);
    }
  }
  return result;
}

/**
 * Get gentype arg for bsc
 */
export function getGentypeArg(config: Config): string[] {
  if (config.gentypeconfig !== undefined) {
    return ["-bs-gentype"];
  }
  return [];
}

/**
 * Convert package name to namespace name
 */
export function namespaceFromPackageName(packageName: string): string {
  let result = "";
  let capital = true;

  for (let i = 0; i < packageName.length; i++) {
    const ch = packageName.charAt(i);
    if (
      (ch >= "a" && ch <= "z") ||
      (ch >= "A" && ch <= "Z") ||
      (ch >= "0" && ch <= "9") ||
      ch === "_"
    ) {
      result += capital ? ch.toUpperCase() : ch;
      capital = false;
    } else if (ch === "/" || ch === "-") {
      capital = true;
    }
    // Other characters are skipped
  }

  return result;
}
