// Port from rewatch/src/build/build_types.rs lines 1-86
// Core compiler state types for the build system

export type ParseState = "pending" | "parseError" | "warning" | "success";

export type CompileState = "pending" | "error" | "warning" | "success";

export interface Interface {
  path: string;
  parseState: ParseState;
  compileState: CompileState;
  lastModified: number;
  parseDirty: boolean;
  // Compiler warning output (from bsc stderr) stored for re-emission
  // during incremental builds when this module is not recompiled.
  // Written to `.compiler.log` on each build cycle.
  compileWarnings: string | undefined;
}

export interface Implementation {
  path: string;
  parseState: ParseState;
  compileState: CompileState;
  lastModified: number;
  parseDirty: boolean;
  // Compiler warning output (from bsc stderr) stored for re-emission
  // during incremental builds when this module is not recompiled.
  // Written to `.compiler.log` on each build cycle.
  compileWarnings: string | undefined;
}

export interface SourceFile {
  implementation: Implementation;
  interface: Interface | undefined;
}

export interface MlMap {
  parseDirty: boolean;
}

export type SourceType =
  | { type: "sourceFile"; sourceFile: SourceFile }
  | { type: "mlMap"; mlMap: MlMap };

export function sourceTypeToString(sourceType: SourceType): string {
  return sourceType.type === "sourceFile" ? "SourceFile" : "MlMap";
}

export interface CompilerInfo {
  bscPath: string;
  bscHash: string;
  runtimePath?: string;
}
