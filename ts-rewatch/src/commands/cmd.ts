// Port from rewatch/src/cmd.rs
// External command execution with streaming output

import { spawn } from "node:child_process";
import { emojis } from "../utils/helpers.ts";

export interface CommandResult {
  exitCode: number;
  stdout: string;
  stderr: string;
}

/**
 * Run an external command with streaming output
 */
export async function runCommand(
  commandString: string,
  cwd?: string,
): Promise<CommandResult> {
  const startTime = Date.now();

  console.log(`${emojis.COMMAND}Running subcommand...`);
  console.log("────────");

  const parts = commandString.trim().split(/\s+/);
  const command = parts[0];
  const args = parts.slice(1);

  return new Promise((resolve, reject) => {
    const proc = spawn(command, args, {
      cwd,
      stdio: ["ignore", "pipe", "pipe"],
    });

    let stdout = "";
    let stderr = "";

    proc.stdout.on("data", (data) => {
      const text = data.toString();
      stdout += text;
      process.stdout.write(text);
    });

    proc.stderr.on("data", (data) => {
      const text = data.toString();
      stderr += text;
      process.stderr.write(text);
    });

    proc.on("close", (code) => {
      const duration = (Date.now() - startTime) / 1000;
      console.log(`${emojis.COMMAND}Ran subcommand in ${duration.toFixed(2)}s`);

      resolve({
        exitCode: code ?? 0,
        stdout,
        stderr,
      });
    });

    proc.on("error", (err) => {
      reject(new Error(`Failed to execute command: ${err.message}`));
    });
  });
}

/**
 * Run an external command synchronously
 */
export function runCommandSync(commandString: string, cwd?: string): CommandResult {
  const { spawnSync } = require("node:child_process");

  const parts = commandString.trim().split(/\s+/);
  const command = parts[0];
  const args = parts.slice(1);

  const result = spawnSync(command, args, {
    cwd,
    encoding: "utf-8",
  });

  return {
    exitCode: result.status ?? 0,
    stdout: result.stdout ?? "",
    stderr: result.stderr ?? "",
  };
}

/**
 * Execute bsc with given arguments
 */
export async function runBsc(
  bscPath: string,
  args: string[],
  cwd?: string,
): Promise<CommandResult> {
  return new Promise((resolve, reject) => {
    const proc = spawn(bscPath, args, {
      cwd,
      stdio: ["ignore", "pipe", "pipe"],
    });

    let stdout = "";
    let stderr = "";

    proc.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    proc.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    proc.on("close", (code) => {
      resolve({
        exitCode: code ?? 0,
        stdout,
        stderr,
      });
    });

    proc.on("error", (err) => {
      reject(new Error(`Failed to execute bsc: ${err.message}`));
    });
  });
}

/**
 * Execute bsc synchronously
 */
export function runBscSync(
  bscPath: string,
  args: string[],
  cwd?: string,
): CommandResult {
  const { spawnSync } = require("node:child_process");

  const result = spawnSync(bscPath, args, {
    cwd,
    encoding: "utf-8",
  });

  return {
    exitCode: result.status ?? 0,
    stdout: result.stdout ?? "",
    stderr: result.stderr ?? "",
  };
}
