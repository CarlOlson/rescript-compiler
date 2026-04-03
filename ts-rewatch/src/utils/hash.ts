// Port from rewatch/src/helpers.rs:508-513
// File hashing using blake3

import { blake3 } from "@napi-rs/blake-hash";
import * as fs from "node:fs/promises";

/**
 * Compute blake3 hash of file contents.
 * Returns undefined if the file cannot be read.
 */
export async function computeFileHash(
  filePath: string,
): Promise<string | undefined> {
  try {
    const contents = await fs.readFile(filePath);
    return blake3(contents).toString("hex");
  } catch {
    return undefined;
  }
}

/**
 * Compute blake3 hash of file contents synchronously.
 * Returns undefined if the file cannot be read.
 */
export function computeFileHashSync(filePath: string): string | undefined {
  try {
    const contents = require("node:fs").readFileSync(filePath);
    return blake3(contents).toString("hex");
  } catch {
    return undefined;
  }
}

/**
 * Compute blake3 hash of a string/buffer
 */
export function computeHash(data: Buffer | string): string {
  return blake3(data).toString("hex");
}
