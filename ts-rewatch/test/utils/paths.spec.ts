import {
  stripVerbatimPath,
  capitalize,
  getBasename,
  getExtension,
  stringEndsWithAny,
  isInterfaceFile,
  isImplementationFile,
  isSourceFile,
  isSourceFilePath,
  isInterfaceAstFile,
  getAstPath,
  isNonExoticModuleName,
  containsAsciiCharacters,
  getNamespaceFromModuleName,
  formatNamespacedModuleName,
  packagePath,
  getSourceFileFromRescriptFile,
  isLocalPackage,
} from "../../src/utils/paths.ts";

describe("stripVerbatimPath", () => {
  test("returns unchanged on non-Windows", () => {
    const path = "/home/user/project";
    expect(stripVerbatimPath(path)).toBe(path);
  });

  test("strips \\\\?\\ prefix on Windows paths", () => {
    // On non-Windows, this just returns unchanged
    const path = "\\\\?\\C:\\Users\\test";
    if (process.platform === "win32") {
      expect(stripVerbatimPath(path)).toBe("C:\\Users\\test");
    } else {
      expect(stripVerbatimPath(path)).toBe(path);
    }
  });
});

describe("capitalize", () => {
  test("capitalizes first letter", () => {
    expect(capitalize("hello")).toBe("Hello");
  });

  test("handles empty string", () => {
    expect(capitalize("")).toBe("");
  });

  test("handles single character", () => {
    expect(capitalize("a")).toBe("A");
  });

  test("handles already capitalized", () => {
    expect(capitalize("Hello")).toBe("Hello");
  });
});

describe("getBasename", () => {
  test("gets basename without extension", () => {
    expect(getBasename("/path/to/file.res")).toBe("file");
  });

  test("handles multiple extensions", () => {
    expect(getBasename("/path/to/file.spec.ts")).toBe("file.spec");
  });

  test("handles no extension", () => {
    expect(getBasename("/path/to/file")).toBe("file");
  });
});

describe("getExtension", () => {
  test("gets extension without dot", () => {
    expect(getExtension("/path/to/file.res")).toBe("res");
  });

  test("handles no extension", () => {
    expect(getExtension("/path/to/file")).toBe("");
  });
});

describe("stringEndsWithAny", () => {
  test("returns true for matching extension", () => {
    expect(stringEndsWithAny("/path/to/file.res", ["res", "resi"])).toBe(true);
  });

  test("returns false for non-matching extension", () => {
    expect(stringEndsWithAny("/path/to/file.js", ["res", "resi"])).toBe(false);
  });
});

describe("isInterfaceFile", () => {
  test("returns true for resi", () => {
    expect(isInterfaceFile("resi")).toBe(true);
  });

  test("returns false for res", () => {
    expect(isInterfaceFile("res")).toBe(false);
  });
});

describe("isImplementationFile", () => {
  test("returns true for res", () => {
    expect(isImplementationFile("res")).toBe(true);
  });

  test("returns false for resi", () => {
    expect(isImplementationFile("resi")).toBe(false);
  });
});

describe("isSourceFile", () => {
  test("returns true for res", () => {
    expect(isSourceFile("res")).toBe(true);
  });

  test("returns true for resi", () => {
    expect(isSourceFile("resi")).toBe(true);
  });

  test("returns false for js", () => {
    expect(isSourceFile("js")).toBe(false);
  });
});

describe("isSourceFilePath", () => {
  test("returns true for .res files", () => {
    expect(isSourceFilePath("/path/to/file.res")).toBe(true);
  });

  test("returns true for .resi files", () => {
    expect(isSourceFilePath("/path/to/file.resi")).toBe(true);
  });

  test("returns false for .js files", () => {
    expect(isSourceFilePath("/path/to/file.js")).toBe(false);
  });
});

describe("isInterfaceAstFile", () => {
  test("returns true for .iast files", () => {
    expect(isInterfaceAstFile("/path/to/file.iast")).toBe(true);
  });

  test("returns false for .ast files", () => {
    expect(isInterfaceAstFile("/path/to/file.ast")).toBe(false);
  });
});

describe("getAstPath", () => {
  test("converts .res to .ast", () => {
    expect(getAstPath("/path/to/file.res")).toBe("/path/to/file.ast");
  });

  test("converts .resi to .iast", () => {
    expect(getAstPath("/path/to/file.resi")).toBe("/path/to/file.iast");
  });
});

describe("isNonExoticModuleName", () => {
  test("returns true for valid module name", () => {
    expect(isNonExoticModuleName("MyModule")).toBe(true);
  });

  test("returns true for name with underscore", () => {
    expect(isNonExoticModuleName("My_Module")).toBe(true);
  });

  test("returns true for name with numbers", () => {
    expect(isNonExoticModuleName("Module123")).toBe(true);
  });

  test("returns false for lowercase start", () => {
    expect(isNonExoticModuleName("myModule")).toBe(false);
  });

  test("returns false for empty string", () => {
    expect(isNonExoticModuleName("")).toBe(false);
  });

  test("returns false for special characters", () => {
    expect(isNonExoticModuleName("My-Module")).toBe(false);
  });
});

describe("containsAsciiCharacters", () => {
  test("returns true for alphanumeric", () => {
    expect(containsAsciiCharacters("hello123")).toBe(true);
  });

  test("returns false for empty string", () => {
    expect(containsAsciiCharacters("")).toBe(false);
  });

  test("returns false for special characters only", () => {
    expect(containsAsciiCharacters("---")).toBe(false);
  });
});

describe("getNamespaceFromModuleName", () => {
  test("extracts namespace from module name", () => {
    expect(getNamespaceFromModuleName("Foo-MyNamespace")).toBe("MyNamespace");
  });

  test("returns undefined for no namespace", () => {
    expect(getNamespaceFromModuleName("Foo")).toBeUndefined();
  });
});

describe("formatNamespacedModuleName", () => {
  test("formats namespaced module name", () => {
    expect(formatNamespacedModuleName("Foo-MyNamespace")).toBe(
      "MyNamespace.Foo",
    );
  });

  test("handles @ prefix", () => {
    expect(formatNamespacedModuleName("Foo-@MyNamespace")).toBe(
      "MyNamespace.Foo",
    );
  });

  test("returns unchanged for no namespace", () => {
    expect(formatNamespacedModuleName("Foo")).toBe("Foo");
  });
});

describe("packagePath", () => {
  test("creates node_modules path", () => {
    expect(packagePath("/project", "@rescript/core")).toBe(
      "/project/node_modules/@rescript/core",
    );
  });
});

describe("getSourceFileFromRescriptFile", () => {
  test("changes extension with dot prefix", () => {
    expect(getSourceFileFromRescriptFile("/path/to/file.res", ".mjs")).toBe(
      "/path/to/file.mjs",
    );
  });

  test("changes extension without dot prefix", () => {
    expect(getSourceFileFromRescriptFile("/path/to/file.res", "mjs")).toBe(
      "/path/to/file.mjs",
    );
  });
});

describe("isLocalPackage", () => {
  test("returns true for package within workspace", () => {
    expect(isLocalPackage("/workspace", "/workspace/packages/foo")).toBe(true);
  });

  test("returns false for package outside workspace", () => {
    expect(isLocalPackage("/workspace", "/other/packages/foo")).toBe(false);
  });

  test("returns false for package in node_modules", () => {
    expect(
      isLocalPackage("/workspace", "/workspace/node_modules/@rescript/core"),
    ).toBe(false);
  });
});
