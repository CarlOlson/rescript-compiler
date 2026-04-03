// Port from rewatch/src/build.rs
// Build orchestration

import type { BuildState } from "../types/build.ts";
import { createBuildState } from "../types/build.ts";
import { createProjectContext } from "../project/context.ts";
import { getLock, formatLockError } from "../project/lock.ts";
import { discoverPackages, parsePackages } from "./packages.ts";
import {
  getCompilerInfo,
  verifyCompilerInfo,
  writeCompilerInfoSync,
} from "./compilerInfo.ts";
import { initialize as initLogs, finalize as finalizeLogs } from "./logs.ts";
import { generateAsts, type ParseResult } from "./parse.ts";
import { getDeps } from "./deps.ts";
import { compile, type CompileResult } from "./compile.ts";
import { cleanupAfterBuild } from "./clean.ts";
import { emojis } from "../utils/helpers.ts";

export interface BuildResult {
  success: boolean;
  errors: string;
  warnings: string;
  numCompiled: number;
}

export interface BuildOptions {
  warnErrorOverride?: string;
  showProgress?: boolean;
  afterBuild?: string;
}

/**
 * Initialize a build state
 */
export function initializeBuild(
  folder: string,
  _warnErrorOverride?: string,
): { buildState: BuildState; release: () => void } | { error: string } {
  // Acquire build lock
  const lockResult = getLock(folder);
  if (!lockResult.acquired) {
    return { error: formatLockError(lockResult.error) };
  }

  try {
    // Create project context
    const projectContext = createProjectContext(folder);

    // Get compiler info
    const compilerInfo = getCompilerInfo();

    // Discover packages
    const packages = discoverPackages(projectContext);

    // Verify compiler info (cleans packages if compiler changed)
    verifyCompilerInfo(packages, compilerInfo);

    // Create build state
    const buildState = createBuildState(projectContext, packages, compilerInfo);

    // Parse packages into modules
    parsePackages(packages, buildState);

    // Initialize compiler logs
    initLogs(packages);

    return {
      buildState,
      release: lockResult.release,
    };
  } catch (e) {
    lockResult.release();
    throw e;
  }
}

/**
 * Run an incremental build
 */
export function incrementalBuild(
  buildState: BuildState,
  options: BuildOptions = {},
): BuildResult {
  const { warnErrorOverride, showProgress = false, afterBuild } = options;

  let errors = "";
  let warnings = "";
  let numCompiled = 0;

  // Step 1: Generate ASTs
  if (showProgress) {
    process.stdout.write(`${emojis.CODE}Parsing...\n`);
  }

  const parseResult = generateAsts(
    buildState,
    warnErrorOverride,
    showProgress ? () => process.stdout.write(".") : undefined,
  );

  if (parseResult.hasErrors) {
    errors += parseResult.stderr;
    finalizeLogs(buildState.packages);
    writeCompilerInfoSync(buildState);
    return { success: false, errors, warnings, numCompiled };
  }

  warnings += parseResult.stderr;

  // Step 2: Analyze dependencies
  if (showProgress) {
    process.stdout.write(`\n${emojis.SWORDS}Analyzing dependencies...\n`);
  }

  getDeps(buildState, buildState.deletedModules);

  // Step 3: Compile
  if (showProgress) {
    process.stdout.write(`${emojis.CODE}Compiling...\n`);
  }

  const compileResult = compile(
    buildState,
    warnErrorOverride,
    showProgress,
    showProgress ? () => process.stdout.write(".") : undefined,
  );

  errors += compileResult.errors;
  warnings += compileResult.warnings;
  numCompiled = compileResult.numCompiled;

  // Step 4: Cleanup
  cleanupAfterBuild(buildState);

  // Step 5: Finalize logs
  finalizeLogs(buildState.packages);

  // Step 6: Write compiler info
  writeCompilerInfoSync(buildState);

  // Step 7: Run after-build command
  if (afterBuild && errors.length === 0) {
    if (showProgress) {
      process.stdout.write(`\n${emojis.COMMAND}Running after-build...\n`);
    }
    // Would run afterBuild command here
  }

  const success = errors.length === 0;

  if (showProgress) {
    if (success) {
      process.stdout.write(
        `\n${emojis.CHECKMARK}Build successful! Compiled ${numCompiled} modules.\n`,
      );
    } else {
      process.stdout.write(`\n${emojis.CROSS}Build failed.\n`);
    }
  }

  return { success, errors, warnings, numCompiled };
}

/**
 * Full build pipeline
 */
export function build(folder: string, options: BuildOptions = {}): BuildResult {
  const initResult = initializeBuild(folder, options.warnErrorOverride);

  if ("error" in initResult) {
    return {
      success: false,
      errors: initResult.error,
      warnings: "",
      numCompiled: 0,
    };
  }

  try {
    return incrementalBuild(initResult.buildState, options);
  } finally {
    initResult.release();
  }
}

// Re-export types and functions
export type { ParseResult, CompileResult };
export { compile } from "./compile.ts";
export { generateAsts } from "./parse.ts";
export { getDeps } from "./deps.ts";
export { discoverPackages, parsePackages } from "./packages.ts";
export {
  getCompilerInfo,
  verifyCompilerInfo,
  writeCompilerInfo,
} from "./compilerInfo.ts";
