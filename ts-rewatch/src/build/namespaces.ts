// Port from rewatch/src/build/namespaces.rs
// Namespace handling and mlmap generation

import * as fs from "node:fs";
import * as path from "node:path";
import { spawn } from "node:child_process";
import type { Package } from "../types/build.ts";
import { getPackageBuildPath } from "../types/build.ts";
import { createPathSync } from "../utils/helpers.ts";

/**
 * Generate mlmap file content for a namespace.
 *
 * Namespaces work like the following: The build system will generate a file
 * called `MyModule.mlmap` which contains all modules that are in the namespace.
 *
 * Internal modules are not accessible with the following trick: they are
 * compiled to a module name such as `MyModule-MyNameSpace`. A dash in a module
 * name is not possible to make in a source file, but it's possible when
 * constructing the AST, so these modules are hidden from compilation.
 */
export function generateMlmap(
  pkg: Package,
  namespace: string,
  dependingModules: Set<string>,
): string {
  const buildPath = getPackageBuildPath(pkg);
  const mlmapPath = path.join(buildPath, `${namespace}.mlmap`);

  // We don't really need to create a digest, because we track if we need to
  // recompile in a different way but we need to put it in the file for it to
  // be readable.
  let content = "randjbuildsystem\n";

  // Sort modules for deterministic output
  const sortedModules = Array.from(dependingModules).sort();
  for (const module of sortedModules) {
    content += `${module}\n`;
  }

  // Ensure directory exists
  createPathSync(buildPath);

  // Write the file
  fs.writeFileSync(mlmapPath, content);

  return mlmapPath;
}

/**
 * Generate mlmap file and return the path
 */
export function generateMlmapPath(
  pkg: Package,
  namespace: string,
  dependingModules: Set<string>,
): string {
  return generateMlmap(pkg, namespace, dependingModules);
}

/**
 * Get mlmap compiler arguments
 */
export function getMlmapCompilerArgs(
  runtimePath: string | undefined,
  namespace: string,
): string[] {
  return [
    ...(runtimePath ? ["-runtime", runtimePath] : []),
    "-w",
    "-49",
    "-color",
    "always",
    "-no-alias-deps",
    `${namespace}.mlmap`,
  ];
}

/**
 * Compile the mlmap file via bsc
 */
export async function compileMlmap(
  pkg: Package,
  namespace: string,
  bscPath: string,
  runtimePath?: string,
): Promise<void> {
  const buildPath = getPackageBuildPath(pkg);
  const mlmapName = `${namespace}.mlmap`;

  const args = [
    ...(runtimePath ? ["-runtime", runtimePath] : []),
    "-w",
    "-49",
    "-color",
    "always",
    "-no-alias-deps",
    mlmapName,
  ];

  return new Promise((resolve, reject) => {
    const proc = spawn(bscPath, args, {
      cwd: buildPath,
      stdio: ["ignore", "pipe", "pipe"],
    });

    let stderr = "";
    proc.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    proc.on("close", (code) => {
      if (code !== 0) {
        reject(
          new Error(
            `Failed to compile namespace mlmap ${namespace} in ${buildPath}: ${stderr}`,
          ),
        );
      } else {
        resolve();
      }
    });

    proc.on("error", (err) => {
      reject(
        new Error(
          `Failed to spawn bsc for namespace mlmap ${namespace}: ${err.message}`,
        ),
      );
    });
  });
}

/**
 * Compile the mlmap file synchronously
 */
export function compileMlmapSync(
  pkg: Package,
  namespace: string,
  bscPath: string,
  runtimePath?: string,
): void {
  const { spawnSync } = require("node:child_process");
  const buildPath = getPackageBuildPath(pkg);
  const mlmapName = `${namespace}.mlmap`;

  const args = [
    ...(runtimePath ? ["-runtime", runtimePath] : []),
    "-w",
    "-49",
    "-color",
    "always",
    "-no-alias-deps",
    mlmapName,
  ];

  const result = spawnSync(bscPath, args, {
    cwd: buildPath,
    encoding: "utf-8",
  });

  if (result.status !== 0) {
    throw new Error(
      `Failed to compile namespace mlmap ${namespace} in ${buildPath}: ${result.stderr}`,
    );
  }
}

/**
 * Get the mlmap path for a package
 */
export function getMlmapPath(pkg: Package, namespace: string): string {
  return path.join(getPackageBuildPath(pkg), `${namespace}.mlmap`);
}

/**
 * Check if mlmap needs to be regenerated
 */
export function needsMlmapRegeneration(
  pkg: Package,
  namespace: string,
  modules: Set<string>,
): boolean {
  const mlmapPath = getMlmapPath(pkg, namespace);

  try {
    const content = fs.readFileSync(mlmapPath, "utf-8");
    const lines = content.split("\n").filter((line) => line.length > 0);

    // First line is the header
    if (lines[0] !== "randjbuildsystem") {
      return true;
    }

    const existingModules = new Set(lines.slice(1));

    // Check if modules match
    if (existingModules.size !== modules.size) {
      return true;
    }

    for (const module of modules) {
      if (!existingModules.has(module)) {
        return true;
      }
    }

    return false;
  } catch {
    // File doesn't exist or can't be read
    return true;
  }
}
