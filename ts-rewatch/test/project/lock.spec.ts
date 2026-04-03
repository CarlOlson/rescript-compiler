import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";
import {
  getLock,
  getLockPath,
  isLocked,
  removeLock,
  formatLockError,
  LOCKFILE,
  type LockError,
} from "../../src/project/lock.ts";

describe("getLockPath", () => {
  test("returns correct lock path", () => {
    expect(getLockPath("/project")).toBe("/project/lib/rescript.lock");
  });
});

describe("formatLockError", () => {
  test("formats locked error", () => {
    const error: LockError = { type: "locked", pid: 1234 };
    expect(formatLockError(error)).toContain("1234");
    expect(formatLockError(error)).toContain("already running");
  });

  test("formats projectFolderMissing error", () => {
    const error: LockError = { type: "projectFolderMissing", path: "/missing" };
    expect(formatLockError(error)).toContain("/missing");
    expect(formatLockError(error)).toContain("does not exist");
  });
});

describe("getLock", () => {
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "lock-test-"));
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  test("returns error when project folder missing", () => {
    const missing = path.join(tempDir, "missing");
    const result = getLock(missing);

    expect(result.acquired).toBe(false);
    if (!result.acquired) {
      expect(result.error.type).toBe("projectFolderMissing");
    }
  });

  test("acquires lock in empty project folder", () => {
    const projectFolder = path.join(tempDir, "project");
    fs.mkdirSync(projectFolder);

    const result = getLock(projectFolder);

    expect(result.acquired).toBe(true);
    if (result.acquired) {
      expect(result.pid).toBe(process.pid);

      // Verify lib directory was created
      expect(fs.existsSync(path.join(projectFolder, "lib"))).toBe(true);

      // Verify lockfile was created
      const lockPath = path.join(projectFolder, "lib", LOCKFILE);
      expect(fs.existsSync(lockPath)).toBe(true);
      expect(fs.readFileSync(lockPath, "utf-8")).toBe(process.pid.toString());

      result.release();
    }
  });

  test("takes over stale lock", () => {
    const projectFolder = path.join(tempDir, "project");
    const libDir = path.join(projectFolder, "lib");
    fs.mkdirSync(libDir, { recursive: true });

    // Write a stale lock with non-existent PID
    const stalePid = 999999;
    fs.writeFileSync(path.join(libDir, LOCKFILE), stalePid.toString());

    const result = getLock(projectFolder);

    expect(result.acquired).toBe(true);
    if (result.acquired) {
      expect(result.pid).toBe(process.pid);
      result.release();
    }
  });

  test("release removes lock file", () => {
    const projectFolder = path.join(tempDir, "project");
    fs.mkdirSync(projectFolder);

    const result = getLock(projectFolder);
    expect(result.acquired).toBe(true);

    if (result.acquired) {
      const lockPath = path.join(projectFolder, "lib", LOCKFILE);
      expect(fs.existsSync(lockPath)).toBe(true);

      result.release();
      expect(fs.existsSync(lockPath)).toBe(false);
    }
  });
});

describe("isLocked", () => {
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "lock-test-"));
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  test("returns false when no lock file", () => {
    expect(isLocked(tempDir)).toBe(false);
  });

  test("returns false for stale lock", () => {
    const libDir = path.join(tempDir, "lib");
    fs.mkdirSync(libDir, { recursive: true });
    fs.writeFileSync(path.join(libDir, LOCKFILE), "999999");

    expect(isLocked(tempDir)).toBe(false);
  });
});

describe("removeLock", () => {
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "lock-test-"));
  });

  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  test("removes existing lock file", () => {
    const libDir = path.join(tempDir, "lib");
    fs.mkdirSync(libDir, { recursive: true });
    const lockPath = path.join(libDir, LOCKFILE);
    fs.writeFileSync(lockPath, "1234");

    removeLock(tempDir);

    expect(fs.existsSync(lockPath)).toBe(false);
  });

  test("does not throw when lock file missing", () => {
    expect(() => removeLock(tempDir)).not.toThrow();
  });
});
