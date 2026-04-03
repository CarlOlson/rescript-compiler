// Port from rewatch/src/build/compile/dependency_cycle.rs
// Circular dependency detection using BFS

import type { Module, BuildState } from "../types/build.ts";
import { formatNamespacedModuleName } from "../utils/paths.ts";
import * as path from "node:path";

/**
 * Find the shortest circular dependency path in the module graph
 */
export function findCycle(modules: Map<string, Module>): string[] {
  return findShortestCycle(modules);
}

/**
 * Find the shortest cycle using BFS from each node
 */
function findShortestCycle(modules: Map<string, Module>): string[] {
  let shortestCycle: string[] = [];

  // Build graph and compute in-degrees
  const graph = new Map<string, Set<string>>();
  const inDegrees = new Map<string, number>();

  // Initialize all nodes
  for (const [name] of modules) {
    graph.set(name, new Set());
    inDegrees.set(name, 0);
  }

  // Build the graph and count in-degrees
  for (const [name, module] of modules) {
    for (const dep of module.deps) {
      const count = inDegrees.get(dep);
      if (count !== undefined) {
        inDegrees.set(dep, count + 1);
      }
    }
    graph.set(name, module.deps);
  }

  // Remove nodes with no outgoing edges (can't be in a cycle)
  for (const [name, deps] of graph) {
    if (deps.size === 0) {
      graph.delete(name);
    }
  }

  // Sort nodes by connectivity (in-degree + out-degree) descending
  const startNodes = Array.from(graph.keys()).sort((a, b) => {
    const aConn = (inDegrees.get(a) ?? 0) + (graph.get(a)?.size ?? 0);
    const bConn = (inDegrees.get(b) ?? 0) + (graph.get(b)?.size ?? 0);
    return bConn - aConn;
  });

  let currentShortestLength = Infinity;
  const noCycleCache = new Set<string>();

  for (const startNode of startNodes) {
    // Skip nodes we know don't have cycles
    if (noCycleCache.has(startNode)) {
      continue;
    }

    // Skip nodes with no incoming edges
    const inDegree = inDegrees.get(startNode);
    if (inDegree === undefined || inDegree === 0) {
      noCycleCache.add(startNode);
      continue;
    }

    const cycle = findCycleBfs(startNode, graph, currentShortestLength);
    if (cycle !== undefined) {
      if (shortestCycle.length === 0 || cycle.length < shortestCycle.length) {
        shortestCycle = cycle;
        currentShortestLength = cycle.length;

        // Early termination for very short cycles
        if (cycle.length <= 3) {
          break;
        }
      }
    } else {
      noCycleCache.add(startNode);
    }
  }

  return shortestCycle;
}

/**
 * BFS to find a cycle starting from a given node
 */
function findCycleBfs(
  start: string,
  graph: Map<string, Set<string>>,
  maxLength: number,
): string[] | undefined {
  const queue: string[] = [];
  // Map: node -> [distance, parent]
  const visited = new Map<string, [number, string | undefined]>();

  visited.set(start, [0, undefined]);
  queue.push(start);

  while (queue.length > 0) {
    const current = queue.shift()!;
    const [dist] = visited.get(current)!;

    // Early termination if we've gone too far
    if (dist >= maxLength - 1) {
      continue;
    }

    const neighbors = graph.get(current);
    if (neighbors === undefined) {
      continue;
    }

    for (const neighbor of neighbors) {
      // Found a cycle back to start
      if (neighbor === start) {
        // Reconstruct the cycle
        const cyclePath: string[] = [start];
        let curr = current;
        while (curr !== start) {
          cyclePath.push(curr);
          const parent = visited.get(curr)?.[1];
          if (parent === undefined) break;
          curr = parent;
        }
        return cyclePath;
      }

      // If not visited, add to queue
      if (!visited.has(neighbor)) {
        visited.set(neighbor, [dist + 1, current]);
        queue.push(neighbor);
      }
    }
  }

  return undefined;
}

/**
 * Format a cycle for display
 */
export function formatCycle(cycle: string[], buildState: BuildState): string {
  if (cycle.length === 0) {
    return "";
  }

  const nodes = [...cycle].reverse();
  nodes.push(nodes[0]); // Close the cycle

  const rootConfig =
    buildState.projectContext.monorepoContext?.type === "package"
      ? buildState.projectContext.monorepoContext.parentConfig
      : buildState.projectContext.currentConfig;

  const rootPath = rootConfig.path
    ? path.dirname(rootConfig.path)
    : buildState.projectContext.rootPath;

  return nodes
    .map((name) => {
      const displayName = formatNamespacedModuleName(name);
      const module = buildState.modules.get(name);

      if (module === undefined || module.sourceType.type !== "sourceFile") {
        return displayName;
      }

      const pkg = buildState.packages.get(module.packageName);
      if (pkg === undefined) {
        return displayName;
      }

      const absPath = path.join(
        pkg.path,
        module.sourceType.sourceFile.implementation.path,
      );
      const relPath = path.relative(rootPath, absPath);
      return `${displayName} (${relPath})`;
    })
    .join("\n → ");
}

/**
 * Check if the module graph has any cycles
 */
export function hasCycle(modules: Map<string, Module>): boolean {
  return findCycle(modules).length > 0;
}
