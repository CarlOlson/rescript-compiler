# TypeScript Port of Rewatch Build System

## Overview

Port the Rust `rewatch` build system to TypeScript. Focus on the `build` command first, with watch/format/clean commands to follow later.

**Key Decisions:**
- Use `Promise.all` for parallelism (no worker_threads)
- Use `commander` for CLI parsing
- Maintain interface/output parity with Rust (console output may differ)
- Prioritize `build` command; other commands come later

## Directory Structure

```
ts-rewatch/src/
├── index.ts                 # Entry point + CLI
├── types/
│   ├── build.ts             # Module, BuildState, Package, etc.
│   ├── config.ts            # Config types from rescript.json
│   └── compiler.ts          # CompilerInfo, parse/compile states
├── config/
│   └── parser.ts            # rescript.json parsing + validation
├── project/
│   ├── context.ts           # ProjectContext, MonoRepoContext
│   └── lock.ts              # Build locking (PID-based)
├── build/
│   ├── index.ts             # Build orchestration (initialize, incremental)
│   ├── packages.ts          # Package discovery
│   ├── parse.ts             # AST generation (bsc -bs-ast)
│   ├── deps.ts              # Module dependency analysis
│   ├── compile.ts           # Module compilation loop
│   ├── cycle.ts             # Circular dependency detection
│   ├── namespaces.ts        # Namespace/mlmap handling
│   ├── clean.ts             # Build artifact cleanup
│   ├── compilerInfo.ts      # Compiler version tracking
│   ├── readState.ts         # Previous build state loading
│   └── logs.ts              # Compiler log aggregation
├── commands/
│   └── cmd.ts               # External command execution
└── utils/
    ├── helpers.ts           # General utilities
    ├── paths.ts             # Path manipulation
    └── hash.ts              # File hashing (blake3)
```

## Implementation Phases

### Phase 1: Types & Utilities
Small, independently testable modules with no external dependencies.

#### 1.1 Common Types (`types/compiler.ts`)
```typescript
// Port from build_types.rs lines 1-23
export type ParseState = 'pending' | 'parseError' | 'warning' | 'success';
export type CompileState = 'pending' | 'error' | 'warning' | 'success';

export interface Implementation {
  path: string;
  parseState: ParseState;
  compileState: CompileState;
  lastModified: number;
  parseDirty: boolean;
  compileWarnings: string | undefined;
}

export interface Interface {
  path: string;
  parseState: ParseState;
  compileState: CompileState;
  lastModified: number;
  parseDirty: boolean;
  compileWarnings: string | undefined;
}
```

**Test:** Type compilation only - no runtime tests needed.

**Rust source:** `rewatch/src/build/build_types.rs:1-86`

---

#### 1.2 Path Utilities (`utils/paths.ts`)
```typescript
// Strip Windows verbatim path prefix (\\?\)
export function stripVerbatimPath(p: string): string;

// Convert file path to module name (capitalize, strip extension)
export function filePathToModuleName(filePath: string): string;

// Check if path is a ReScript source file
export function isSourceFile(filePath: string): boolean;

// Get relative path from project root
export function getRelativePath(from: string, to: string): string;
```

**Test:** Unit tests for each function with edge cases (Windows paths, various extensions).

**Rust source:** `rewatch/src/helpers.rs:94-154`

---

#### 1.3 File Hashing (`utils/hash.ts`)
```typescript
import { blake3 } from '@napi-rs/blake-hash';

// Compute blake3 hash of file contents
export async function computeFileHash(filePath: string): Promise<string>;
```

**Test:** Hash known files, verify consistency.

**Rust source:** `rewatch/src/helpers.rs:76-92`

**Dependency:** `@napi-rs/blake-hash` npm package (same algorithm as Rust)

---

#### 1.4 General Helpers (`utils/helpers.ts`)
```typescript
// Find bsc executable in node_modules or PATH
export function getBsc(projectRoot: string): string;

// Get compiler asset path (lib/bs/...)
export function getCompilerAsset(sourceFile: string, namespace: string | undefined, ext: string): string;

// Read lines from file (for AST parsing)
export async function readLines(filePath: string): Promise<string[]>;
```

**Test:** Unit tests, mock filesystem for getBsc.

**Rust source:** `rewatch/src/helpers.rs:1-75`, `rewatch/src/helpers.rs:155-230`

---

### Phase 2: Configuration
Parse and validate rescript.json files.

#### 2.1 Config Types (`types/config.ts`)
```typescript
// Port the ~40 fields from config.rs
export interface Config {
  name: string;
  sources: Source[];
  bsDependencies?: string[];
  bsDevDependencies?: string[];
  pinned_dependencies?: string[];
  ppxFlags?: string[];
  bscFlags?: string[];
  suffix?: '.js' | '.mjs' | '.cjs' | '.bs.js' | '.bs.mjs';
  packageSpecs?: PackageSpec | PackageSpec[];
  namespace?: boolean | string;
  warnings?: WarningConfig;
  jsx?: JsxConfig;
  // ... remaining fields
}

export type Source = string | PackageSource;

export interface PackageSource {
  dir: string;
  subdirs?: boolean | Source[];
  type?: 'dev';
}
```

**Test:** Type compilation.

**Rust source:** `rewatch/src/config.rs:1-200`

---

#### 2.2 Config Parser (`config/parser.ts`)
```typescript
// Parse rescript.json with validation
export function parseConfig(filePath: string): Config;

// Flatten sources to list of directories
export function getSourceFolders(config: Config): PackageSource[];

// Get warning arguments for bsc
export function getWarningArgs(config: Config): string[];
```

**Test:** Parse sample rescript.json files from testrepo/, validate against expected output.

**Rust source:** `rewatch/src/config.rs:200-800`

---

### Phase 3: Project Infrastructure
Lock mechanism and project context.

#### 3.1 Build Lock (`project/lock.ts`)
```typescript
export type LockResult =
  | { acquired: true; release: () => void }
  | { acquired: false; error: 'alreadyLocked' | 'otherError'; pid?: number };

// Acquire build lock for folder
export function getLock(folder: string): LockResult;
```

**Test:** Integration test with temp directories, concurrent access.

**Rust source:** `rewatch/src/lock.rs`

---

#### 3.2 Project Context (`project/context.ts`)
```typescript
export type MonoRepoContext =
  | { type: 'root'; localDeps: Set<string>; localDevDeps: Set<string> }
  | { type: 'package'; parentConfig: Config };

export interface ProjectContext {
  currentConfig: Config;
  rootPath: string;
  monorepoContext: MonoRepoContext | undefined;
}

// Create project context from folder path
export function createProjectContext(folder: string): ProjectContext;

// Check if folder is monorepo root
export function detectMonorepo(folder: string): MonoRepoContext | undefined;
```

**Test:** Test with single project, monorepo root, package within monorepo.

**Rust source:** `rewatch/src/project_context.rs`

---

### Phase 4: Build Types
Core data structures for the build system.

#### 4.1 Build Types (`types/build.ts`)
```typescript
export interface Module {
  sourceType: SourceType;
  deps: Set<string>;
  dependents: Set<string>;
  packageName: string;
  compileDirty: boolean;
  parseDirty: boolean;
  depsDirty: boolean;
  lastCompiledCmi: number | undefined;
  lastCompiledCmt: number | undefined;
  isTypeDev: boolean;
}

export type SourceType =
  | { type: 'sourceFile'; sourceFile: SourceFile }
  | { type: 'mlMap'; mlMap: MlMap };

export interface SourceFile {
  implementation: Implementation;
  interface: Interface | undefined;
}

export interface Package {
  name: string;
  config: Config;
  sourceFolders: Set<PackageSource>;
  sourceFiles: Map<string, SourceFileMeta> | undefined;
  modules: Set<string> | undefined;
  namespace: Namespace;
  path: string;
  isLocalDep: boolean;
  isRoot: boolean;
}

export type Namespace =
  | { type: 'namespace'; name: string }
  | { type: 'namespaceWithEntry'; name: string; entry: string }
  | { type: 'noNamespace' };

export interface BuildState {
  projectContext: ProjectContext;
  modules: Map<string, Module>;
  packages: Map<string, Package>;
  moduleNames: Set<string>;
  deletedModules: Set<string>;
  compilerInfo: CompilerInfo;
  depsInitialized: boolean;
}

export interface CompilerInfo {
  bscPath: string;
  bscHash: string;
  runtimePath: string;
}
```

**Test:** Type compilation.

**Rust source:** `rewatch/src/build/build_types.rs`

---

### Phase 5: Package Discovery
Find and parse all packages in the project.

#### 5.1 Package Discovery (`build/packages.ts`)
```typescript
// Discover all packages starting from root
export async function discoverPackages(
  projectContext: ProjectContext
): Promise<Map<string, Package>>;

// Read source files from package directories
export async function readSourceFiles(
  pkg: Package
): Promise<Map<string, SourceFileMeta>>;

// Validate cross-package dependencies
export function validatePackageDependencies(
  packages: Map<string, Package>
): void;

// Parse packages into modules
export async function parsePackages(
  packages: Map<string, Package>,
  modules: Map<string, Module>
): Promise<void>;
```

**Test:** Use testrepo/ fixtures, verify package graph construction.

**Rust source:** `rewatch/src/build/packages.rs` (largest file ~1200 lines)

---

### Phase 6: Build Pipeline
Core build operations.

#### 6.1 Compiler Info (`build/compilerInfo.ts`)
```typescript
export interface CompilerCheckResult {
  needsClean: boolean;
  compilerInfo: CompilerInfo;
}

// Get compiler info (path, hash)
export async function getCompilerInfo(projectRoot: string): Promise<CompilerInfo>;

// Verify compiler hasn't changed since last build
export async function verifyCompilerInfo(
  folder: string,
  compilerInfo: CompilerInfo
): Promise<CompilerCheckResult>;

// Write compiler info to lib/bs/compiler-info.json
export async function writeCompilerInfo(
  folder: string,
  compilerInfo: CompilerInfo
): Promise<void>;
```

**Test:** Verify hash computation, file writing.

**Rust source:** `rewatch/src/build/compiler_info.rs`

---

#### 6.2 AST Generation (`build/parse.ts`)
```typescript
// Generate parser arguments for bsc -bs-ast
export function parserArgs(
  config: Config,
  sourceFile: string,
  isInterface: boolean,
  warnErrorOverride: string | undefined
): string[];

// Generate ASTs for all dirty modules
export async function generateAsts(
  buildState: BuildState,
  warnErrorOverride: string | undefined,
  onProgress?: () => void
): Promise<{ stderr: string; parserArgs: Map<string, string[]> }>;

// Generate AST for single module
async function generateAst(
  module: Module,
  config: Config,
  warnErrorOverride: string | undefined
): Promise<{ stderr: string; parserArgs: string[] }>;
```

**Test:** Mock bsc, verify argument generation.

**Rust source:** `rewatch/src/build/parse.rs`

---

#### 6.3 Dependency Analysis (`build/deps.ts`)
```typescript
// Parse AST file to extract module dependencies
export function getDependencyModules(
  astPath: string,
  namespace: Namespace,
  allModuleNames: Set<string>
): Set<string>;

// Analyze all modules and build dependency graph
export function analyzeDependencies(
  buildState: BuildState
): void;
```

**Test:** Parse sample AST files, verify dependency extraction.

**Rust source:** `rewatch/src/build/deps.ts` (~160 lines)

---

#### 6.4 Circular Dependency Detection (`build/cycle.ts`)
```typescript
// Find shortest circular dependency path
export function findCycle(
  modules: Map<string, Module>
): string[] | undefined;
```

**Test:** Create graphs with known cycles, verify detection.

**Rust source:** `rewatch/src/build/compile/dependency_cycle.rs`

---

#### 6.5 Namespace Handling (`build/namespaces.ts`)
```typescript
// Generate .mlmap file content for namespace
export function generateMlmap(
  namespace: string,
  modules: string[]
): string;

// Compile namespace mlmap via bsc
export async function compileMlmap(
  pkg: Package,
  bscPath: string
): Promise<void>;
```

**Test:** Verify mlmap content generation.

**Rust source:** `rewatch/src/build/namespaces.rs`

---

#### 6.6 Read Previous State (`build/readState.ts`)
```typescript
// Read compile state from previous build artifacts
export async function readCompileState(
  buildState: BuildState
): Promise<void>;
```

**Test:** Create mock artifacts, verify state loading.

**Rust source:** `rewatch/src/build/read_compile_state.rs`

---

#### 6.7 Compiler Logs (`build/logs.ts`)
```typescript
// Initialize compiler log file
export function initLog(folder: string): void;

// Append to compiler log
export function appendLog(folder: string, content: string): void;

// Finalize and copy log
export function finalizeLog(folder: string): void;
```

**Test:** Verify log file creation and content.

**Rust source:** `rewatch/src/build/logs.rs`

---

#### 6.8 Build Cleanup (`build/clean.ts`)
```typescript
// Clean artifacts from previous build
export async function cleanupPreviousBuild(
  buildState: BuildState
): Promise<void>;

// Clean artifacts after successful build
export async function cleanupAfterBuild(
  buildState: BuildState
): Promise<void>;

// Remove compile assets for a module
export async function removeCompileAssets(
  module: Module,
  folder: string
): Promise<void>;
```

**Test:** Create mock artifacts, verify cleanup.

**Rust source:** `rewatch/src/build/clean.rs`

---

#### 6.9 Module Compilation (`build/compile.ts`)
```typescript
// Generate compiler arguments for bsc
export function compilerArgs(
  module: Module,
  pkg: Package,
  buildState: BuildState,
  warnErrorOverride: string | undefined
): string[];

// Compile all dirty modules in dependency order
export async function compile(
  buildState: BuildState,
  warnErrorOverride: string | undefined,
  showProgress: boolean,
  onProgress?: () => void
): Promise<{ errors: string; warnings: string; numCompiled: number }>;

// Mark modules with changed dependencies as dirty
export function markDirtyDependents(
  module: Module,
  modules: Map<string, Module>
): void;
```

**Test:** Mock bsc, verify argument generation and compilation order.

**Rust source:** `rewatch/src/build/compile.rs` (~1050 lines)

---

#### 6.10 Build Orchestration (`build/index.ts`)
```typescript
export interface BuildResult {
  success: boolean;
  errors: string;
  warnings: string;
  numCompiled: number;
}

// Initialize build state
export async function initializeBuild(
  folder: string,
  warnErrorOverride: string | undefined
): Promise<BuildState>;

// Run incremental build
export async function incrementalBuild(
  buildState: BuildState,
  warnErrorOverride: string | undefined,
  showProgress: boolean
): Promise<BuildResult>;

// Full build pipeline
export async function build(
  folder: string,
  warnErrorOverride: string | undefined,
  showProgress: boolean
): Promise<BuildResult>;
```

**Test:** Integration test with testrepo/.

**Rust source:** `rewatch/src/build.rs`

---

### Phase 7: CLI & Entry Point
Wire everything together.

#### 7.1 Command Execution (`commands/cmd.ts`)
```typescript
// Execute external command with streaming output
export async function runCommand(
  command: string,
  cwd: string
): Promise<{ exitCode: number; stdout: string; stderr: string }>;
```

**Test:** Run simple commands, verify output capture.

**Rust source:** `rewatch/src/cmd.rs`

---

#### 7.2 CLI Entry Point (`index.ts`)
```typescript
import { Command } from 'commander';

// Parse CLI arguments and execute command
async function main(): Promise<void> {
  const program = new Command()
    .name('rescript')
    .description('ReScript build system')
    .option('-v, --verbose', 'Verbose output')
    .option('-q, --quiet', 'Quiet output');

  program
    .command('build', { isDefault: true })
    .description('Build the project')
    .option('--filter <regex>', 'Filter files by regex')
    .option('--after-build <cmd>', 'Command to run after build')
    .option('--warn-error <flags>', 'Warning/error flags')
    .option('--no-timing', 'Disable timing output')
    .argument('[folder]', 'Project folder', '.')
    .action(handleBuild);

  await program.parseAsync();
}
```

**Test:** CLI argument parsing, end-to-end build.

**Rust source:** `rewatch/src/cli.rs`, `rewatch/src/main.rs`

---

## Testing Strategy

### Unit Tests
- Each module has corresponding `*.spec.ts` file
- Mock filesystem operations using `memfs` or manual mocks
- Mock child_process.spawn for bsc calls

### Integration Tests
- Use `rewatch/testrepo/` as test fixture
- Copy to temp directory for each test
- Run full build, verify output

### Validation
- Compare TypeScript output with Rust output
- Run both on same project, diff results

---

## Dependencies to Add

None, dependencies are already installed.

---

## Implementation Order (28 units)

| # | Module | Est. Lines | Dependencies | Priority |
|---|--------|------------|--------------|----------|
| 1 | `types/compiler.ts` | 50 | none | P1 |
| 2 | `utils/paths.ts` | 80 | none | P1 |
| 3 | `utils/hash.ts` | 30 | @napi-rs/blake-hash | P1 |
| 4 | `utils/helpers.ts` | 120 | utils/paths | P1 |
| 5 | `types/config.ts` | 150 | none | P1 |
| 6 | `config/parser.ts` | 300 | types/config | P1 |
| 7 | `project/lock.ts` | 80 | none | P1 |
| 8 | `types/build.ts` | 100 | types/config, types/compiler | P1 |
| 9 | `project/context.ts` | 150 | types/build, config/parser | P1 |
| 10 | `build/packages.ts` | 400 | types/build, project/context | P2 |
| 11 | `build/compilerInfo.ts` | 100 | utils/hash | P2 |
| 12 | `build/namespaces.ts` | 80 | types/build | P2 |
| 13 | `build/parse.ts` | 200 | types/build, config/parser | P2 |
| 14 | `build/deps.ts` | 120 | types/build | P2 |
| 15 | `build/cycle.ts` | 60 | types/build | P2 |
| 16 | `build/readState.ts` | 80 | types/build | P2 |
| 17 | `build/logs.ts` | 60 | none | P2 |
| 18 | `build/clean.ts` | 120 | types/build | P2 |
| 19 | `build/compile.ts` | 350 | all build/* | P3 |
| 20 | `build/index.ts` | 200 | all build/* | P3 |
| 21 | `commands/cmd.ts` | 50 | none | P3 |
| 22 | `index.ts` | 100 | commander, build/index | P3 |

**Total estimated: ~2,580 lines** (vs ~7,000 Rust lines - TypeScript is more concise)

---

## Verification Plan

1. **Unit Tests:** Run `yarn test` after each module
2. **Type Check:** Run `yarn typecheck` continuously
3. **Lint:** Run `yarn lint` before commits
4. **Integration:** Test against `rewatch/testrepo/`
5. **Comparison:** Run both Rust and TS builds, compare output

---

## Future Phases (Not in Initial Scope)

- **Watch command:** File watching with chokidar
- **Format command:** Code formatting via bsc -format
- **Clean command:** Full artifact cleanup
- **Compiler-args command:** IDE integration support
