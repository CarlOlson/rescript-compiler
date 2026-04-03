// Port from rewatch/src/helpers.rs
// General utility functions

import * as fs from "node:fs/promises";
import * as fsSync from "node:fs";
import * as path from "node:path";
import { stripVerbatimPath, getBasename, capitalize } from "./paths.ts";
import type { Namespace } from "../types/build.ts";

// Emoji constants for console output
export const emojis = {
  COMMAND: "\u{1F3C3} ", // Running person
  SWEEP: "\u{1F9F9} ", // Broom
  CODE: "\u{1F9F1} ", // Brick
  SWORDS: "\u{1F93A} ", // Fencing
  CHECKMARK: "\u2705 ", // Check mark
  CROSS: "\u274C ", // Cross mark
  SPARKLES: "\u2728 ", // Sparkles
  LINE_CLEAR: "\x1b[2K\r",
};

/**
 * Get the directory containing the current executable (or module)
 */
export function getBinDir(): string {
  // In Node.js, we use the directory of the current module
  return path.dirname(process.argv[1] || __dirname);
}

/**
 * Find the bsc executable.
 * First checks RESCRIPT_BSC_EXE environment variable,
 * then falls back to bin/bsc.exe relative to the bin dir.
 */
export function getBsc(): string {
  const envBsc = process.env.RESCRIPT_BSC_EXE;
  if (envBsc) {
    const resolved = fsSync.realpathSync(envBsc);
    return stripVerbatimPath(resolved);
  }
  const bscPath = path.join(getBinDir(), "bsc.exe");
  const resolved = fsSync.realpathSync(bscPath);
  return stripVerbatimPath(resolved);
}

/**
 * Get the runtime path from environment variable
 */
export function getRuntimePath(): string {
  const envRuntime = process.env.RESCRIPT_RUNTIME;
  if (envRuntime) {
    return fsSync.realpathSync(envRuntime);
  }
  // Default to @rescript/runtime relative to bin dir
  return path.join(getBinDir(), "..", "@rescript", "runtime");
}

/**
 * Add namespace suffix to a base name
 */
export function addSuffix(base: string, namespace: Namespace): string {
  if (namespace.type === "noNamespace") {
    return base;
  }
  if (
    namespace.type === "namespaceWithEntry" &&
    namespace.entry === capitalize(base)
  ) {
    return base;
  }
  const suffix = namespaceToSuffix(namespace);
  return suffix ? `${base}-${suffix}` : base;
}

/**
 * Get the namespace suffix string
 */
export function namespaceToSuffix(namespace: Namespace): string | undefined {
  switch (namespace.type) {
    case "namespace":
      return namespace.name;
    case "namespaceWithEntry":
      return namespace.name;
    case "noNamespace":
      return undefined;
  }
}

/**
 * Get the module name with namespace suffix
 */
export function moduleNameWithNamespace(
  moduleName: string,
  namespace: Namespace,
): string {
  return capitalize(addSuffix(moduleName, namespace));
}

/**
 * Get the compiler asset basename from a file path
 * This doesn't capitalize the module name! If the rescript name of the file is "foo.res",
 * the compiler assets are foo-Namespace.cmt and foo-Namespace.cmj, but the module name is Foo
 */
export function filePathToCompilerAssetBasename(
  filePath: string,
  namespace: Namespace,
): string {
  const base = getBasename(filePath);
  return addSuffix(base, namespace);
}

/**
 * Convert a file path to a module name with namespace
 */
export function filePathToModuleName(
  filePath: string,
  namespace: Namespace,
): string {
  return capitalize(filePathToCompilerAssetBasename(filePath, namespace));
}

/**
 * Read lines from a file
 */
export async function readLines(filePath: string): Promise<string[]> {
  const content = await fs.readFile(filePath, "utf-8");
  return content.split("\n");
}

/**
 * Read lines from a file synchronously
 */
export function readLinesSync(filePath: string): string[] {
  const content = fsSync.readFileSync(filePath, "utf-8");
  return content.split("\n");
}

/**
 * Read file contents as string
 */
export async function readFile(filePath: string): Promise<string> {
  return await fs.readFile(filePath, "utf-8");
}

/**
 * Read file contents as string synchronously
 */
export function readFileSync(filePath: string): string {
  return fsSync.readFileSync(filePath, "utf-8");
}

/**
 * Create a directory path recursively
 */
export async function createPath(dirPath: string): Promise<void> {
  await fs.mkdir(dirPath, { recursive: true });
}

/**
 * Create a directory path recursively (synchronous)
 */
export function createPathSync(dirPath: string): void {
  fsSync.mkdirSync(dirPath, { recursive: true });
}

/**
 * Get the current system time in milliseconds since Unix epoch
 */
export function getSystemTime(): number {
  return Date.now();
}

/**
 * Check if a rescript.json config exists in a directory
 */
export function hasRescriptConfig(dirPath: string): boolean {
  return fsSync.existsSync(path.join(dirPath, "rescript.json"));
}

/**
 * Traverse up the directory tree until we find a rescript.json
 * Returns the directory containing the config, or undefined if not found.
 */
export function getNearestConfig(startPath: string): string | undefined {
  let currentDir = startPath;
  while (true) {
    if (hasRescriptConfig(currentDir)) {
      return currentDir;
    }
    const parent = path.dirname(currentDir);
    if (parent === currentDir) {
      // Reached root
      return undefined;
    }
    currentDir = parent;
  }
}

/**
 * Get the absolute path, handling relative paths
 */
export function getAbsPath(p: string): string {
  const absPath = path.resolve(p);
  return absPath;
}

/**
 * Check if a file or directory exists
 */
export function exists(p: string): boolean {
  return fsSync.existsSync(p);
}

/**
 * Check if a file or directory exists (async)
 */
export async function existsAsync(p: string): Promise<boolean> {
  try {
    await fs.access(p);
    return true;
  } catch {
    return false;
  }
}

/**
 * Get the modification time of a file
 */
export async function getLastModified(filePath: string): Promise<number> {
  const stats = await fs.stat(filePath);
  return stats.mtimeMs;
}

/**
 * Get the modification time of a file (synchronous)
 */
export function getLastModifiedSync(filePath: string): number {
  const stats = fsSync.statSync(filePath);
  return stats.mtimeMs;
}
