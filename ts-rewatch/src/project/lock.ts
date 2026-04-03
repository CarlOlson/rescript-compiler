// Port from rewatch/src/lock.rs
// Build locking mechanism using PID-based lockfiles

import * as fs from "node:fs";
import * as path from "node:path";

export const LOCKFILE = "rescript.lock";

export type LockError =
  | { type: "locked"; pid: number }
  | { type: "parsingLockfile"; error: Error }
  | { type: "readingLockfile"; error: Error }
  | { type: "writingLockfile"; error: Error }
  | { type: "projectFolderMissing"; path: string };

export type LockResult =
  | { acquired: true; pid: number; release: () => void }
  | { acquired: false; error: LockError };

/**
 * Format lock error message
 */
export function formatLockError(error: LockError): string {
  switch (error.type) {
    case "locked":
      return `A ReScript build is already running. The process ID (PID) is ${error.pid}`;
    case "parsingLockfile":
      return `Could not parse lockfile: ${error.error.message} (try removing it and running the command again)`;
    case "readingLockfile":
      return `Could not read lockfile: ${error.error.message} (try removing it and running the command again)`;
    case "writingLockfile":
      return `Could not write lockfile: ${error.error.message}`;
    case "projectFolderMissing":
      return `Could not write lockfile because the specified project folder does not exist: ${error.path}`;
  }
}

/**
 * Check if a process with the given PID is running
 */
function isProcessRunning(pid: number): boolean {
  try {
    // On Unix, sending signal 0 checks if process exists without killing it
    // This works on Windows too with Node.js
    process.kill(pid, 0);
    return true;
  } catch {
    // ESRCH means no such process
    // EPERM means process exists but we don't have permission (still running)
    return false;
  }
}

/**
 * Check if the process matches our executable name
 * This is a simplified version - in Node.js we can't easily get another process's name
 * So we check if the process is running and assume it's a rewatch process
 */
function pidMatchesCurrentProcess(pid: number): boolean {
  // In Node.js, we can't easily check the process name
  // We just check if the process is still running
  return isProcessRunning(pid);
}

/**
 * Get the lock file path for a project folder
 */
export function getLockPath(folder: string): string {
  return path.join(folder, "lib", LOCKFILE);
}

/**
 * Acquire a build lock for the given folder
 */
export function getLock(folder: string): LockResult {
  // Check if project folder exists
  if (!fs.existsSync(folder)) {
    return {
      acquired: false,
      error: { type: "projectFolderMissing", path: folder },
    };
  }

  const libDir = path.join(folder, "lib");
  const lockPath = path.join(libDir, LOCKFILE);
  const pid = process.pid;

  // Check if lockfile already exists
  try {
    const contents = fs.readFileSync(lockPath, "utf-8");
    const parsedPid = parseInt(contents.trim(), 10);

    if (Number.isNaN(parsedPid)) {
      return {
        acquired: false,
        error: {
          type: "parsingLockfile",
          error: new Error(`Invalid PID in lockfile: ${contents}`),
        },
      };
    }

    // Check if the process is still running
    if (pidMatchesCurrentProcess(parsedPid)) {
      return {
        acquired: false,
        error: { type: "locked", pid: parsedPid },
      };
    }
    // Process is not running, we can take over the lock
  } catch (e) {
    const error = e as NodeJS.ErrnoException;
    if (error.code !== "ENOENT") {
      return {
        acquired: false,
        error: { type: "readingLockfile", error: error },
      };
    }
    // File doesn't exist, which is fine
  }

  // Create lib directory if needed
  try {
    fs.mkdirSync(libDir, { recursive: true });
  } catch (e) {
    return {
      acquired: false,
      error: { type: "writingLockfile", error: e as Error },
    };
  }

  // Write our PID to the lockfile
  try {
    fs.writeFileSync(lockPath, pid.toString());
  } catch (e) {
    return {
      acquired: false,
      error: { type: "writingLockfile", error: e as Error },
    };
  }

  return {
    acquired: true,
    pid,
    release: () => {
      // Remove the lock file when releasing
      try {
        fs.unlinkSync(lockPath);
      } catch {
        // Ignore errors when removing lock file
      }
    },
  };
}

/**
 * Check if a lock is held for the given folder
 */
export function isLocked(folder: string): boolean {
  const lockPath = getLockPath(folder);

  try {
    const contents = fs.readFileSync(lockPath, "utf-8");
    const parsedPid = parseInt(contents.trim(), 10);

    if (Number.isNaN(parsedPid)) {
      return false;
    }

    return pidMatchesCurrentProcess(parsedPid);
  } catch {
    return false;
  }
}

/**
 * Force remove a stale lock file
 */
export function removeLock(folder: string): void {
  const lockPath = getLockPath(folder);
  try {
    fs.unlinkSync(lockPath);
  } catch {
    // Ignore errors
  }
}
