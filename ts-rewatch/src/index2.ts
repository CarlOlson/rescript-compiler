import { Command } from "commander";
import * as z from "zod";
import * as fs from "node:fs";
import * as path from "node:path";

// FORCE_COLOR

// https://rescript-lang.org/docs/manual/build-configuration-schema
const RescriptJson = z.object({
  name: z.string(),
  namespace: z.union([z.boolean(), z.string()]).optional(),
  sources: z.array(
    z.union([
      z.string(),
      z.object({
        dir: z.string(),
        subdirs: z.union([z.boolean(), z.array(z.string())]),
        type: z.string().optional(),
        public: z.array(z.string()),
      }),
    ]),
  ),
  suffix: z.literal([".js", ".mjs", ".cjs", ".res.js", ".res.mjs", ".res.cjs"]),
  dependencies: z.array(z.string()).default([]),
  "dev-dependencies": z.array(z.string()).default([]),
  jsx: z.object({
    version: z.literal([3, 4]),
    module: z.string().optional(),
    preserve: z.boolean().optional(),
  }).optional(),
  "package-specs": z.object({
    module: z.literal(["commonjs", "esmodule"]),
    "in-source": z.boolean(),
  }),
  // TODO warn if used in rescript 12+
  uncurried: z.boolean().optional(),
  // TODO warn on unrecognized compiler flags
  "compiler-flags": z.array(z.string()).default([]),
  // TODO warn on unsupported error codes
  warnings: z.object({
    number: z.string(),
    error: z.union([z.boolean(), z.string()]),
  })
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
})

type PackageJson = z.infer<typeof PackageJson>;

if (import.meta.main) {
  main();
}

function main(): void {

}
