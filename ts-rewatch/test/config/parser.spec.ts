import {
  parseConfigFromString,
  getNamespace,
  getSourceFolders,
  isPathInDevSource,
  shouldRecurse,
  getUnknownFields,
  getUnsupportedFields,
} from "../../src/config/parser.ts";
import type { Config } from "../../src/types/config.ts";

describe("parseConfigFromString", () => {
  test("parses minimal config", () => {
    const json = `{"name": "test"}`;
    const config = parseConfigFromString(json);
    expect(config.name).toBe("test");
  });

  test("parses full config", () => {
    const json = `{
      "name": "my-project",
      "sources": [{"dir": "src", "subdirs": true}],
      "package-specs": [{"module": "esmodule", "in-source": true}],
      "suffix": ".mjs",
      "dependencies": ["@rescript/core"]
    }`;
    const config = parseConfigFromString(json);
    expect(config.name).toBe("my-project");
    expect(config.suffix).toBe(".mjs");
    expect(config.dependencies).toEqual(["@rescript/core"]);
  });

  test("throws on duplicate suffix", () => {
    const json = `{
      "name": "test",
      "sources": ".",
      "package-specs": [
        {"module": "commonjs", "in-source": true},
        {"module": "esmodule", "in-source": true}
      ]
    }`;
    expect(() => parseConfigFromString(json)).toThrow(
      'Duplicate package-spec suffix ".js" is not allowed.',
    );
  });

  test("allows duplicate suffix with different in-source", () => {
    const json = `{
      "name": "test",
      "sources": ".",
      "package-specs": [
        {"module": "esmodule", "in-source": true, "suffix": ".res.js"},
        {"module": "esmodule", "in-source": false, "suffix": ".res.js"}
      ]
    }`;
    const config = parseConfigFromString(json);
    expect(config["package-specs"]).toHaveLength(2);
  });

  test("throws on unsupported module system", () => {
    const json = `{
      "name": "test",
      "package-specs": [{"module": "es6"}]
    }`;
    expect(() => parseConfigFromString(json)).toThrow(
      'Module system "es6" is unsupported',
    );
  });
});

describe("getNamespace", () => {
  test("returns noNamespace when not set", () => {
    const config: Config = { name: "test" };
    expect(getNamespace(config)).toEqual({ type: "noNamespace" });
  });

  test("returns noNamespace when false", () => {
    const config: Config = { name: "test", namespace: false };
    expect(getNamespace(config)).toEqual({ type: "noNamespace" });
  });

  test("returns namespace from package name when true", () => {
    const config: Config = { name: "my-package", namespace: true };
    expect(getNamespace(config)).toEqual({
      type: "namespace",
      name: "MyPackage",
    });
  });

  test("returns namespace from string", () => {
    const config: Config = { name: "test", namespace: "MyNamespace" };
    expect(getNamespace(config)).toEqual({
      type: "namespace",
      name: "MyNamespace",
    });
  });

  test("returns namespaceWithEntry when entry provided", () => {
    const config: Config = {
      name: "test",
      namespace: true,
      "namespace-entry": "Main",
    };
    expect(getNamespace(config)).toEqual({
      type: "namespaceWithEntry",
      name: "Test",
      entry: "Main",
    });
  });

  test("handles scoped package names", () => {
    const config: Config = { name: "@rescript/core", namespace: true };
    expect(getNamespace(config)).toEqual({
      type: "namespace",
      name: "RescriptCore",
    });
  });
});

describe("getSourceFolders", () => {
  test("returns empty for no sources", () => {
    const config: Config = { name: "test" };
    expect(getSourceFolders(config)).toEqual([]);
  });

  test("returns single source as string", () => {
    const config: Config = { name: "test", sources: "src" };
    const folders = getSourceFolders(config);
    expect(folders).toHaveLength(1);
    expect(folders[0].dir).toBe("src");
  });

  test("returns multiple sources", () => {
    const config: Config = { name: "test", sources: ["src", "lib"] };
    const folders = getSourceFolders(config);
    expect(folders).toHaveLength(2);
  });

  test("flattens nested sources", () => {
    const config: Config = {
      name: "test",
      sources: [{ dir: "src", subdirs: ["sub1", "sub2"] }],
    };
    const folders = getSourceFolders(config);
    expect(folders).toHaveLength(3); // src, src/sub1, src/sub2
  });
});

describe("shouldRecurse", () => {
  test("returns true when subdirs is true", () => {
    expect(shouldRecurse({ dir: "src", subdirs: true })).toBe(true);
  });

  test("returns false when subdirs is false", () => {
    expect(shouldRecurse({ dir: "src", subdirs: false })).toBe(false);
  });

  test("returns false when subdirs is undefined", () => {
    expect(shouldRecurse({ dir: "src" })).toBe(false);
  });

  test("returns false when subdirs is array", () => {
    expect(shouldRecurse({ dir: "src", subdirs: ["sub1"] })).toBe(false);
  });
});

describe("isPathInDevSource", () => {
  test("returns false for no sources", () => {
    const config: Config = { name: "test" };
    expect(isPathInDevSource(config, "src/file.res")).toBe(false);
  });

  test("returns false for non-dev source", () => {
    const config: Config = {
      name: "test",
      sources: [{ dir: "src" }],
    };
    expect(isPathInDevSource(config, "src/file.res")).toBe(false);
  });

  test("returns true for dev source", () => {
    const config: Config = {
      name: "test",
      sources: [{ dir: "test", type: "dev" }],
    };
    expect(isPathInDevSource(config, "test/file.res")).toBe(true);
  });

  test("returns true for recursive dev source", () => {
    const config: Config = {
      name: "test",
      sources: [{ dir: "test", type: "dev", subdirs: true }],
    };
    expect(isPathInDevSource(config, "test/sub/file.res")).toBe(true);
  });
});

describe("getUnknownFields", () => {
  test("returns empty for valid config", () => {
    const config: Config = { name: "test", sources: "src" };
    expect(getUnknownFields(config)).toEqual([]);
  });

  test("returns unknown fields", () => {
    const config = { name: "test", "unknown-field": true } as unknown as Config;
    expect(getUnknownFields(config)).toEqual(["unknown-field"]);
  });

  test("does not include unsupported fields", () => {
    const config = {
      name: "test",
      "ignored-dirs": ["scripts"],
    } as unknown as Config;
    expect(getUnknownFields(config)).toEqual([]);
  });
});

describe("getUnsupportedFields", () => {
  test("returns empty for valid config", () => {
    const config: Config = { name: "test" };
    expect(getUnsupportedFields(config)).toEqual([]);
  });

  test("returns unsupported fields", () => {
    const config = {
      name: "test",
      "ignored-dirs": ["scripts"],
    } as unknown as Config;
    expect(getUnsupportedFields(config)).toEqual(["ignored-dirs"]);
  });
});
