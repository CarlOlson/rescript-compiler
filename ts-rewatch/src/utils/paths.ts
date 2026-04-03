// Port from rewatch/src/helpers.rs
// Path manipulation utilities

import * as path from "node:path";

/**
 * Convert to a lexical absolute path (resolves . and .. without filesystem access)
 */
export function toLexicalAbsolute(p: string): string {
  if (path.isAbsolute(p)) {
    return path.normalize(p);
  }
  return path.resolve(p);
}

/**
 * Capitalizes the first character of a string.
 */
export function capitalize(s: string): string {
  if (s.length === 0) return "";
  return s.charAt(0).toUpperCase() + s.slice(1);
}

/**
 * Get the basename of a path (file stem without extension)
 */
export function getBasename(p: string): string {
  const ext = path.extname(p);
  return path.basename(p, ext);
}

/**
 * Get the extension of a path without the leading dot
 */
export function getExtension(p: string): string {
  const ext = path.extname(p);
  return ext.startsWith(".") ? ext.slice(1) : ext;
}

/**
 * Check if a path has one of the given extensions
 */
export function stringEndsWithAny(p: string, suffixes: string[]): boolean {
  const ext = getExtension(p);
  return suffixes.includes(ext);
}

/**
 * Check if a file is an interface file (.resi)
 */
export function isInterfaceFile(extension: string): boolean {
  return extension === "resi";
}

/**
 * Check if a file is an implementation file (.res)
 */
export function isImplementationFile(extension: string): boolean {
  return extension === "res";
}

/**
 * Check if extension indicates a ReScript source file (.res or .resi)
 */
export function isSourceFile(extension: string): boolean {
  return isInterfaceFile(extension) || isImplementationFile(extension);
}

/**
 * Check if a path represents a ReScript source file
 */
export function isSourceFilePath(filePath: string): boolean {
  const ext = getExtension(filePath);
  return isSourceFile(ext);
}

/**
 * Check if a file is an interface AST file (.iast)
 */
export function isInterfaceAstFile(filePath: string): boolean {
  const ext = getExtension(filePath).toLowerCase();
  return ext === "iast";
}

/**
 * Get the path to the AST file for a source file
 */
export function getAstPath(sourceFile: string): string {
  const dir = path.dirname(sourceFile);
  const basename = getBasename(sourceFile);
  const ext = getExtension(sourceFile);
  const astExt = ext.endsWith("i") ? ".iast" : ".ast";
  return path.join(dir, basename + astExt);
}

/**
 * Check if the module name is a valid non-exotic module name.
 * A non-exotic module name starts with an uppercase letter
 * and contains only alphanumeric characters and underscores.
 */
export function isNonExoticModuleName(moduleName: string): boolean {
  if (moduleName.length === 0) return false;
  const firstChar = moduleName.charAt(0);
  if (firstChar < "A" || firstChar > "Z") return false;
  for (let i = 1; i < moduleName.length; i++) {
    const c = moduleName.charAt(i);
    const isAlphanumeric =
      (c >= "a" && c <= "z") ||
      (c >= "A" && c <= "Z") ||
      (c >= "0" && c <= "9") ||
      c === "_";
    if (!isAlphanumeric) return false;
  }
  return true;
}

/**
 * Check if a string contains ASCII alphanumeric characters
 */
export function containsAsciiCharacters(str: string): boolean {
  for (const char of str) {
    if (
      (char >= "a" && char <= "z") ||
      (char >= "A" && char <= "Z") ||
      (char >= "0" && char <= "9")
    ) {
      return true;
    }
  }
  return false;
}

/**
 * Get the namespace from a module name (e.g., "Foo-MyNamespace" -> "MyNamespace")
 */
export function getNamespaceFromModuleName(
  moduleName: string,
): string | undefined {
  const parts = moduleName.split("-");
  if (parts.length < 2) return undefined;
  return parts[1];
}

/**
 * Format a namespaced module name from "ModuleName-Namespace" to "Namespace.ModuleName"
 * Also handles "@Namespace" by removing the "@" prefix.
 */
export function formatNamespacedModuleName(moduleName: string): string {
  const parts = moduleName.split("-");
  const name = parts[0];
  const namespace = parts[1];
  if (namespace === undefined) {
    return name;
  }
  const cleanNamespace = namespace.startsWith("@")
    ? namespace.slice(1)
    : namespace;
  return `${cleanNamespace}.${name}`;
}

/**
 * Get the package path within node_modules
 */
export function packagePath(root: string, packageName: string): string {
  return path.join(root, "node_modules", packageName);
}

/**
 * Get source file path from ReScript file by changing extension
 */
export function getSourceFileFromRescriptFile(
  filePath: string,
  suffix: string,
): string {
  // suffix includes the dot, so we strip it
  const ext = suffix.startsWith(".") ? suffix.slice(1) : suffix;
  const dir = path.dirname(filePath);
  const basename = getBasename(filePath);
  return path.join(dir, `${basename}.${ext}`);
}

/**
 * Check if a package path is a local package (within workspace, not in node_modules)
 */
export function isLocalPackage(
  workspacePath: string,
  canonicalPackagePath: string,
): boolean {
  if (!canonicalPackagePath.startsWith(workspacePath)) {
    return false;
  }
  // Check if path contains "node_modules"
  const parts = canonicalPackagePath.split(path.sep);
  return !parts.includes("node_modules");
}

/**
 * Get relative path from one path to another
 */
export function getRelativePath(from: string, to: string): string {
  return path.relative(from, to);
}
