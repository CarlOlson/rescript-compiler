// Port from rewatch/src/build/logs.rs
// Compiler log file management

import * as fs from "node:fs";
import * as path from "node:path";
import type { Package } from "../types/build.ts";
import { getPackageBuildPath, getPackageOcamlBuildPath } from "../types/build.ts";
import { getSystemTime, createPathSync } from "../utils/helpers.ts";

type Location = "bs" | "ocaml";

/**
 * Get the compiler log file path for a package
 */
function getLogFilePath(pkg: Package, location: Location): string {
  const buildFolder =
    location === "bs" ? getPackageBuildPath(pkg) : getPackageOcamlBuildPath(pkg);
  return path.join(buildFolder, ".compiler.log");
}

/**
 * Check if the ocaml build compiler log exists
 */
export function doesOcamlBuildCompilerLogExist(pkg: Package): boolean {
  return fs.existsSync(getLogFilePath(pkg, "ocaml"));
}

/**
 * Escape ANSI color codes from a string
 */
function escapeColours(str: string): string {
  // Match ANSI escape sequences
  return str.replace(
    /[\u001b\u009b]\[[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]/g,
    "",
  );
}

/**
 * Write content to a log file
 */
function writeToLogFile(filePath: string, content: string): void {
  try {
    fs.appendFileSync(filePath, escapeColours(content));
  } catch (e) {
    console.error(`Could not write to compiler log file: ${filePath}`, e);
  }
}

/**
 * Initialize compiler logs for all packages
 */
export function initialize(packages: Map<string, Package>): void {
  for (const pkg of packages.values()) {
    const logPath = getLogFilePath(pkg, "bs");
    const dir = path.dirname(logPath);

    try {
      createPathSync(dir);
      fs.writeFileSync(logPath, `#Start(${getSystemTime()})\n`);
    } catch (e) {
      throw new Error(`Cannot create compiler log for package ${pkg.name}: ${e}`);
    }
  }
}

/**
 * Initialize compiler log for a single package
 */
export function initializePackage(pkg: Package): void {
  const logPath = getLogFilePath(pkg, "bs");
  const dir = path.dirname(logPath);

  try {
    createPathSync(dir);
    fs.writeFileSync(logPath, `#Start(${getSystemTime()})\n`);
  } catch (e) {
    throw new Error(`Cannot create compiler log for package ${pkg.name}: ${e}`);
  }
}

/**
 * Append content to a package's compiler log
 */
export function append(pkg: Package, content: string): void {
  const logPath = getLogFilePath(pkg, "bs");

  try {
    writeToLogFile(logPath, content);
  } catch (e) {
    throw new Error(`Cannot write compiler log: ${logPath} (${e})`);
  }
}

/**
 * Finalize compiler logs for all packages
 */
export function finalize(packages: Map<string, Package>): void {
  for (const pkg of packages.values()) {
    finalizePackage(pkg);
  }
}

/**
 * Finalize compiler log for a single package
 */
export function finalizePackage(pkg: Package): void {
  const bsLogPath = getLogFilePath(pkg, "bs");
  const ocamlLogPath = getLogFilePath(pkg, "ocaml");

  try {
    // Append done marker
    fs.appendFileSync(bsLogPath, `#Done(${getSystemTime()})\n`);

    // Copy to ocaml location
    const ocamlDir = path.dirname(ocamlLogPath);
    createPathSync(ocamlDir);
    fs.copyFileSync(bsLogPath, ocamlLogPath);
  } catch {
    // Ignore errors in finalization
  }
}

/**
 * Get the content of a package's compiler log
 */
export function getLogContent(pkg: Package): string | undefined {
  const logPath = getLogFilePath(pkg, "bs");

  try {
    return fs.readFileSync(logPath, "utf-8");
  } catch {
    return undefined;
  }
}

/**
 * Clear the compiler log for a package
 */
export function clearLog(pkg: Package): void {
  const logPath = getLogFilePath(pkg, "bs");

  try {
    fs.writeFileSync(logPath, "");
  } catch {
    // Ignore errors
  }
}
