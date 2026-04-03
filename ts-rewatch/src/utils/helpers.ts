// Port from rewatch/src/helpers.rs
// General utility functions

import * as fs from "node:fs/promises";
import * as fsSync from "node:fs";
import * as path from "node:path";
import { getBasename, capitalize } from "./paths.ts";
import { createRequire } from 'node:module';
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
 * Find the bsc executable.
 * First checks RESCRIPT_BSC_EXE environment variable,
 * then falls back to bin/bsc.exe relative to the bin dir.
 */
export function getBsc(): string {
  const require = createRequire(path.join(process.cwd(), "./package.json"));
  const envBsc =
    process.env.RESCRIPT_BSC_EXE ?? require.resolve(".bin/bsc");
    // require.resolve("rescript/bsc"); // 11.x
  if (envBsc) {
    const resolved = fsSync.realpathSync(envBsc);
    return resolved;
  } else if (process.env.RESCRIPT_BSC_EXE) {
    throw new Error(
      `Could not find bsc, RESCRIPT_BSC_EXE=${process.env.RESCRIPT_BSC_EXE}`,
    );
  } else {
    throw new Error("Could not find rescript/bsc");
  }
}

/**
 * Get the runtime path from environment variable
 */
export function getRuntimePath(): string | undefined {
  const envRuntime = process.env.RESCRIPT_RUNTIME;
  if (envRuntime?.trim() === '') {
    return undefined;
  } else if (envRuntime) {
    return fsSync.realpathSync(envRuntime);
  } else if (process.env.RESCRIPT_RUNTIME) {
    throw new Error(
      `Could not find runtime, RESCRIPT_RUNTIME=${process.env.RESCRIPT_RUNTIME}`,
    );
  } else {
    try {
      const require = createRequire(path.join(process.cwd(), "./package.json"));
      const runtime = require.resolve("@rescript/runtime/package.json");
      return fsSync.realpathSync(runtime);
    } catch (cause) {
      throw new Error("Could not find @rescript/runtime, try setting RESCRIPT_RUNTIME", { cause });
    }
  }
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
 * Get the modification time of a file (synchronous)
 */
export function getLastModifiedSync(filePath: string): number {
  const stats = fsSync.statSync(filePath);
  return stats.mtimeMs;
}
