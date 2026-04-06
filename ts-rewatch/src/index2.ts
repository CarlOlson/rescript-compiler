import { Command } from "commander";
import * as z from "zod";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import * as assert from "node:assert";

// FORCE_COLOR

// https://rescript-lang.org/docs/manual/build-configuration-schema
const RescriptJson = z.object({
  name: z.string(),
  namespace: z.union([z.boolean(), z.string()]).optional(),
  sources: z
    .array(
      z.union([
        z.string(),
        z.object({
          dir: z.string(),
          subdirs: z.union([z.boolean(), z.array(z.string())]),
          type: z.string().optional(),
          public: z.array(z.string()),
        }),
      ]),
    )
    .optional(),
  suffix: z.literal([".js", ".mjs", ".cjs", ".res.js", ".res.mjs", ".res.cjs"]),
  dependencies: z.array(z.string()).default([]),
  "dev-dependencies": z.array(z.string()).default([]),
  jsx: z
    .object({
      version: z.literal([3, 4]),
      module: z.string().optional(),
      preserve: z.boolean().optional(),
    })
    .optional(),
  "package-specs": z.array(
    z.object({
      module: z.literal(["commonjs", "esmodule"]),
      "in-source": z.boolean(),
    }),
  ),
  // TODO warn if used in rescript 12+
  uncurried: z.boolean().optional(),
  // TODO warn on unrecognized compiler flags
  "compiler-flags": z.array(z.string()).default([]),
  // TODO warn on unsupported error codes
  warnings: z
    .object({
      number: z.string(),
      error: z.union([z.boolean(), z.string()]),
    })
    .optional(),
});

type RescriptJson = z.infer<typeof RescriptJson>;

const PackageJson = z.object({
  name: z.string(),
  // TODO warn on unknown package manager
  packageManager: z.string().optional(),
  workspaces: z.array(z.string()).optional(),
  type: z.literal(["commonjs", "module"]).default("commonjs"),
  dependencies: z.record(z.string(), z.string()).optional(),
  devDependencies: z.record(z.string(), z.string()).optional(),
  peerDependencies: z.record(z.string(), z.string()).optional(),
});

type PackageJson = z.infer<typeof PackageJson>;

if (import.meta.main) {
  const program = new Command()
    .name("rescript")
    .description("ReScript build system (TypeScript port)")
    .version("0.1.0")
    .option("-v, --verbose", "Verbose output")
    .option("-q, --quiet", "Quiet output");

  program
    .command("build", { isDefault: true })
    .description("Build the project")
    .option("--project <dir>", "Project directory")
    .action(async function () {
      const options = this.opts();
      if (options?.project) process.chdir(options.project);

      const packageJsonPath = await findConfigFile("package.json");
      if (!packageJsonPath) return error("package.json not found");

      const rescriptJsonPath = await findConfigFile("rescript.json");
      if (!rescriptJsonPath) return error("rescript.json not found");

      assert.equal(packageJsonPath?.dir, rescriptJsonPath?.dir);

      // TODO catch readFile error
      // TODO catch JSON.parse error
      const packageJson = PackageJson.safeParse(
        JSON.parse(
          await fs.readFile(path.format(packageJsonPath), { encoding: "utf8" }),
        ),
      );
      if (!packageJson.success) {
        // TODO customize error output
        console.log(`Error parsing ${path.format(packageJsonPath)}`);
        return error(z.prettifyError(packageJson.error));
      }

      const rescriptJson = RescriptJson.safeParse(
        JSON.parse(
          await fs.readFile(path.format(rescriptJsonPath), {
            encoding: "utf8",
          }),
        ),
      );
      if (!rescriptJson.success) {
        console.log(`Error parsing ${path.format(rescriptJsonPath)}`);
        return error(z.prettifyError(rescriptJson.error));
      }
    });

  program.parse();
}

/** Finds file in current or closest parent directory. */
async function findConfigFile(
  filename: string,
  options: { cwd?: string } = {},
): Promise<path.ParsedPath | undefined> {
  const cwd = options?.cwd ?? process.cwd();
  const filePath = path.join(cwd, filename);

  try {
    await fs.access(filePath);
    return path.parse(filePath);
  } catch (_error) {
    const parent = path.dirname(cwd);
    return cwd === parent
      ? undefined
      : findConfigFile(filename, { cwd: parent });
  }
}

function error(message: string) {
  console.log(message);
  process.exit(1);
}
