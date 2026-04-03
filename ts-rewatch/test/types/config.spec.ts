import {
  flattenFlags,
  getPackageSpecs,
  getSuffix,
  getWarningArgs,
  getJsxArgs,
  getJsxModeArgs,
  getJsxModuleArgs,
  getJsxPreserveArgs,
  getExperimentalFeaturesArgs,
  getGentypeArg,
  namespaceFromPackageName,
  oneOrMoreToArray,
  type Config,
} from "../../src/types/config.ts";

describe("oneOrMoreToArray", () => {
  test("returns empty array for undefined", () => {
    expect(oneOrMoreToArray(undefined)).toEqual([]);
  });

  test("wraps single value in array", () => {
    expect(oneOrMoreToArray("hello")).toEqual(["hello"]);
  });

  test("returns array unchanged", () => {
    expect(oneOrMoreToArray(["a", "b"])).toEqual(["a", "b"]);
  });
});

describe("flattenFlags", () => {
  test("returns empty array for undefined", () => {
    expect(flattenFlags(undefined)).toEqual([]);
  });

  test("flattens single flags", () => {
    expect(flattenFlags(["-w", "+8"])).toEqual(["-w", "+8"]);
  });

  test("flattens space-separated flags", () => {
    expect(flattenFlags(["-open ABC"])).toEqual(["-open", "ABC"]);
  });

  test("flattens nested arrays", () => {
    expect(flattenFlags([["-w", "+8"], "-open ABC"])).toEqual([
      "-w",
      "+8",
      "-open",
      "ABC",
    ]);
  });
});

describe("getPackageSpecs", () => {
  test("returns default specs when not specified", () => {
    const config: Config = { name: "test" };
    const specs = getPackageSpecs(config);
    expect(specs).toEqual([
      { module: "esmodule", "in-source": true, suffix: ".js" },
    ]);
  });

  test("returns single spec wrapped in array", () => {
    const config: Config = {
      name: "test",
      "package-specs": { module: "commonjs" },
    };
    const specs = getPackageSpecs(config);
    expect(specs).toEqual([{ module: "commonjs" }]);
  });

  test("returns array specs as-is", () => {
    const config: Config = {
      name: "test",
      "package-specs": [{ module: "esmodule" }, { module: "commonjs" }],
    };
    const specs = getPackageSpecs(config);
    expect(specs).toHaveLength(2);
  });
});

describe("getSuffix", () => {
  test("uses spec suffix first", () => {
    const config: Config = { name: "test", suffix: ".mjs" };
    const spec = { module: "esmodule" as const, suffix: ".js" };
    expect(getSuffix(config, spec)).toBe(".js");
  });

  test("uses config suffix as fallback", () => {
    const config: Config = { name: "test", suffix: ".mjs" };
    const spec = { module: "esmodule" as const };
    expect(getSuffix(config, spec)).toBe(".mjs");
  });

  test("uses default .js suffix", () => {
    const config: Config = { name: "test" };
    const spec = { module: "esmodule" as const };
    expect(getSuffix(config, spec)).toBe(".js");
  });
});

describe("getWarningArgs", () => {
  test("returns empty for non-local deps", () => {
    const config: Config = {
      name: "test",
      warnings: { number: "+8+32", error: true },
    };
    expect(getWarningArgs(config, false)).toEqual([]);
  });

  test("uses override when provided", () => {
    const config: Config = {
      name: "test",
      warnings: { number: "+8+32", error: true },
    };
    expect(getWarningArgs(config, true, "+3+8+11")).toEqual([
      "-warn-error",
      "+3+8+11",
    ]);
  });

  test("uses config warnings when no override", () => {
    const config: Config = {
      name: "test",
      warnings: { number: "+8+32", error: true },
    };
    expect(getWarningArgs(config, true)).toEqual([
      "-w",
      "+8+32",
      "-warn-error",
      "A",
    ]);
  });

  test("handles string error", () => {
    const config: Config = {
      name: "test",
      warnings: { error: "+101" },
    };
    expect(getWarningArgs(config, true)).toEqual(["-warn-error", "+101"]);
  });

  test("returns empty when no warnings", () => {
    const config: Config = { name: "test" };
    expect(getWarningArgs(config, true)).toEqual([]);
  });
});

describe("getJsxArgs", () => {
  test("returns empty when no jsx", () => {
    const config: Config = { name: "test" };
    expect(getJsxArgs(config)).toEqual([]);
  });

  test("returns jsx args for version 4", () => {
    const config: Config = { name: "test", jsx: { version: 4 } };
    expect(getJsxArgs(config)).toEqual(["-bs-jsx", "4"]);
  });

  test("throws for unsupported version", () => {
    const config: Config = { name: "test", jsx: { version: 3 } };
    expect(() => getJsxArgs(config)).toThrow("JSX version 3 is unsupported");
  });
});

describe("getJsxModeArgs", () => {
  test("returns empty when no mode", () => {
    const config: Config = { name: "test", jsx: { version: 4 } };
    expect(getJsxModeArgs(config)).toEqual([]);
  });

  test("returns mode args for classic", () => {
    const config: Config = { name: "test", jsx: { version: 4, mode: "classic" } };
    expect(getJsxModeArgs(config)).toEqual(["-bs-jsx-mode", "classic"]);
  });

  test("returns mode args for automatic", () => {
    const config: Config = {
      name: "test",
      jsx: { version: 4, mode: "automatic" },
    };
    expect(getJsxModeArgs(config)).toEqual(["-bs-jsx-mode", "automatic"]);
  });
});

describe("getJsxModuleArgs", () => {
  test("returns empty when no module", () => {
    const config: Config = { name: "test", jsx: { version: 4 } };
    expect(getJsxModuleArgs(config)).toEqual([]);
  });

  test("returns module args", () => {
    const config: Config = {
      name: "test",
      jsx: { version: 4, module: "react" },
    };
    expect(getJsxModuleArgs(config)).toEqual(["-bs-jsx-module", "react"]);
  });
});

describe("getJsxPreserveArgs", () => {
  test("returns empty when not preserve", () => {
    const config: Config = { name: "test", jsx: { version: 4 } };
    expect(getJsxPreserveArgs(config)).toEqual([]);
  });

  test("returns preserve args", () => {
    const config: Config = { name: "test", jsx: { version: 4, preserve: true } };
    expect(getJsxPreserveArgs(config)).toEqual(["-bs-jsx-preserve"]);
  });
});

describe("getExperimentalFeaturesArgs", () => {
  test("returns empty when no features", () => {
    const config: Config = { name: "test" };
    expect(getExperimentalFeaturesArgs(config)).toEqual([]);
  });

  test("returns enabled features", () => {
    const config: Config = {
      name: "test",
      "experimental-features": { LetUnwrap: true },
    };
    expect(getExperimentalFeaturesArgs(config)).toEqual([
      "-enable-experimental",
      "LetUnwrap",
    ]);
  });

  test("skips disabled features", () => {
    const config: Config = {
      name: "test",
      "experimental-features": { LetUnwrap: false },
    };
    expect(getExperimentalFeaturesArgs(config)).toEqual([]);
  });
});

describe("getGentypeArg", () => {
  test("returns empty when no gentypeconfig", () => {
    const config: Config = { name: "test" };
    expect(getGentypeArg(config)).toEqual([]);
  });

  test("returns gentype arg when config exists", () => {
    const config: Config = { name: "test", gentypeconfig: {} };
    expect(getGentypeArg(config)).toEqual(["-bs-gentype"]);
  });
});

describe("namespaceFromPackageName", () => {
  test("converts simple name", () => {
    expect(namespaceFromPackageName("my-package")).toBe("MyPackage");
  });

  test("handles scoped package", () => {
    expect(namespaceFromPackageName("@rescript/core")).toBe("RescriptCore");
  });

  test("handles underscores", () => {
    expect(namespaceFromPackageName("my_package")).toBe("My_package");
  });

  test("handles numbers", () => {
    expect(namespaceFromPackageName("package123")).toBe("Package123");
  });

  test("handles multiple separators", () => {
    expect(namespaceFromPackageName("my-cool-package")).toBe("MyCoolPackage");
  });
});
