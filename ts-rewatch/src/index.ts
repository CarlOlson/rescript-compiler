#!/usr/bin/env node
// TypeScript ReScript Build System CLI
// Port from rewatch/src/main.rs and rewatch/src/cli.rs

import { Command } from "commander";
import { build, type BuildOptions } from "./build/index.ts";
import { emojis } from "./utils/helpers.ts";

const VERSION = "0.1.0";

/**
 * Handle the build command
 */
function handleBuild(
  folder: string,
  options: {
    warnError?: string;
    afterBuild?: string;
    noTiming?: boolean;
    verbose?: boolean;
  },
): void {
  const startTime = Date.now();

  const buildOptions: BuildOptions = {
    warnErrorOverride: options.warnError,
    showProgress: options.verbose ?? false,
    afterBuild: options.afterBuild,
  };

  const result = build(folder, buildOptions);

  // Print errors
  if (result.errors.length > 0) {
    process.stderr.write(result.errors);
  }

  // Print warnings
  if (result.warnings.length > 0) {
    process.stderr.write(result.warnings);
  }

  // Print timing
  if (!options.noTiming) {
    const elapsed = Date.now() - startTime;
    if (result.success) {
      console.log(
        `${emojis.CHECKMARK}Finished in ${(elapsed / 1000).toFixed(2)}s`,
      );
    } else {
      console.log(
        `${emojis.CROSS}Build failed after ${(elapsed / 1000).toFixed(2)}s`,
      );
    }
  }

  // Exit with appropriate code
  process.exit(result.success ? 0 : 1);
}

/**
 * Main CLI entry point
 */
function main(): void {
  const program = new Command()
    .name("rescript")
    .description("ReScript build system (TypeScript port)")
    .version(VERSION)
    .option("-v, --verbose", "Verbose output")
    .option("-q, --quiet", "Quiet output");

  program
    .command("build", { isDefault: true })
    .description("Build the project")
    .option("--after-build <cmd>", "Command to run after successful build")
    .option("--warn-error <flags>", "Warning/error flags override")
    .option("--no-timing", "Disable timing output")
    .argument("[folder]", "Project folder", ".")
    .action((folder: string, cmdOptions: Record<string, unknown>) => {
      process.chdir(folder);
      // Merge parent options with command options
      const parentOptions = program.opts();
      handleBuild(folder, {
        ...cmdOptions,
        verbose: parentOptions.verbose,
      } as Parameters<typeof handleBuild>[1]);
    });

  program
    .command("clean")
    .description("Clean build artifacts")
    .argument("[folder]", "Project folder", ".")
    .action((folder: string) => {
      console.log(`Clean command not yet implemented for folder: ${folder}`);
      process.exit(1);
    });

  program.parse();
}

// Run main
main();
