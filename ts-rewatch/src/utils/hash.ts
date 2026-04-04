// Port from rewatch/src/helpers.rs:508-513
// File hashing using blake3

import { blake3 } from "@napi-rs/blake-hash";
import * as fs from "node:fs";

/**
 * Compute blake3 hash of file contents synchronously.
 * Returns undefined if the file cannot be read.
 */
export function computeFileHashSync(filePath: string): string | undefined {
  try {
    const contents = fs.readFileSync(filePath);
    return blake3(contents).toString("hex");
  } catch {
    return undefined;
  }
}
