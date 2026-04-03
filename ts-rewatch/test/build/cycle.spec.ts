import { findCycle, hasCycle } from "../../src/build/cycle.ts";
import type { Module } from "../../src/types/build.ts";

function createModule(deps: string[]): Module {
  return {
    sourceType: { type: "mlMap", mlMap: { parseDirty: false } },
    deps: new Set(deps),
    dependents: new Set(),
    packageName: "test",
    compileDirty: false,
    parseDirty: false,
    depsDirty: false,
    lastCompiledCmi: undefined,
    lastCompiledCmt: undefined,
    isTypeDev: false,
  };
}

describe("findCycle", () => {
  test("returns empty array for no modules", () => {
    const modules = new Map<string, Module>();
    expect(findCycle(modules)).toEqual([]);
  });

  test("returns empty array for no cycles", () => {
    const modules = new Map<string, Module>([
      ["A", createModule(["B"])],
      ["B", createModule(["C"])],
      ["C", createModule([])],
    ]);
    expect(findCycle(modules)).toEqual([]);
  });

  test("finds simple two-node cycle", () => {
    const modules = new Map<string, Module>([
      ["A", createModule(["B"])],
      ["B", createModule(["A"])],
    ]);
    const cycle = findCycle(modules);
    expect(cycle.length).toBeGreaterThan(0);
    expect(cycle).toContain("A");
    expect(cycle).toContain("B");
  });

  test("finds three-node cycle", () => {
    const modules = new Map<string, Module>([
      ["A", createModule(["B"])],
      ["B", createModule(["C"])],
      ["C", createModule(["A"])],
    ]);
    const cycle = findCycle(modules);
    expect(cycle.length).toBeGreaterThan(0);
    expect(cycle).toContain("A");
    expect(cycle).toContain("B");
    expect(cycle).toContain("C");
  });

  test("finds self-referencing cycle", () => {
    const modules = new Map<string, Module>([["A", createModule(["A"])]]);
    const cycle = findCycle(modules);
    expect(cycle.length).toBeGreaterThan(0);
    expect(cycle).toContain("A");
  });

  test("finds shortest cycle when multiple exist", () => {
    const modules = new Map<string, Module>([
      ["A", createModule(["B", "D"])],
      ["B", createModule(["C"])],
      ["C", createModule(["A"])],
      ["D", createModule(["A"])], // Shorter cycle: A -> D -> A
    ]);
    const cycle = findCycle(modules);
    expect(cycle.length).toBeLessThanOrEqual(3);
  });
});

describe("hasCycle", () => {
  test("returns false for no cycles", () => {
    const modules = new Map<string, Module>([
      ["A", createModule(["B"])],
      ["B", createModule([])],
    ]);
    expect(hasCycle(modules)).toBe(false);
  });

  test("returns true for cycle", () => {
    const modules = new Map<string, Module>([
      ["A", createModule(["B"])],
      ["B", createModule(["A"])],
    ]);
    expect(hasCycle(modules)).toBe(true);
  });
});
