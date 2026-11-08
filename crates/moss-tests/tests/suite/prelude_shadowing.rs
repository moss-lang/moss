//! The implicit prelude supplies defaults; ordinary imports stay strict.
use moss_tests::{compile, oracle, run_module, sources, ORACLE_REJECTED};

fn accepts(files: &[(&str, &str)], expected: &str) {
    let dir = sources(files);
    let entry = format!("{dir}/main.moss");
    let compiled = compile(&entry, "lib/prelude.moss");
    assert!(compiled.is_module(), "{entry}: {}", compiled.text());
    assert_eq!(run_module(&compiled.stdout, &[]), expected);
    let checked = oracle(&entry, &[]);
    assert_eq!(checked.code, 0, "{entry}: {}", checked.stderr);
    assert_eq!(checked.text(), expected);
}

fn rejects(files: &[(&str, &str)], code: &str) {
    let dir = sources(files);
    let entry = format!("{dir}/main.moss");
    let compiled = compile(&entry, "lib/prelude.moss");
    assert!(!compiled.is_module(), "{entry} unexpectedly compiled");
    assert!(
        compiled.text().contains(code),
        "{entry}: {}",
        compiled.text()
    );
    let checked = oracle(&entry, &[]);
    assert_eq!(checked.code, ORACLE_REJECTED, "{entry}: {}", checked.stderr);
}

const SCALAR: &str = "type Base;\nassume Base { fn identity(x: Base): Base { x } }\n";

#[test]
fn local_declarations_shadow_prelude_names() {
    accepts(
        &[(
            "main.moss",
            "type Base;\nassume Base { fn identity(x: Base): Base { x } }\nfn main() {}\n",
        )],
        "",
    );
}

#[test]
fn explicit_names_shadow_the_prelude_and_can_be_reimported() {
    accepts(&[
        ("scalar.moss", SCALAR),
        ("main.moss", "import \"./scalar.moss\" use Base;\nimport \"./scalar.moss\" use Base;\nassume Base { fn identity(x: Base): Base { x } }\nfn main() {}\n"),
    ], "");
}

#[test]
fn explicit_globs_shadow_prelude_defaults() {
    accepts(&[
        ("scalar.moss", SCALAR),
        ("main.moss", "import \"./scalar.moss\" use *;\nassume Base { fn local(x: Base): Base { identity(x) } }\nfn main() {}\n"),
    ], "");
}

#[test]
fn local_detached_methods_shadow_prelude_methods() {
    accepts(&[("main.moss", "type Item;\nassume Item {\n  fn .read(): Item;\n  assume Item.read { fn read_it(x: Item): Item { x.read() } }\n}\nfn main() {}\n")], "");
}

#[test]
fn explicit_module_aliases_shadow_prelude_aliases() {
    accepts(
        &[
            ("ops.moss", "fn go() {}\n"),
            (
                "main.moss",
                "import \"./ops.moss\" as char;\nfn main() { char::go(); }\n",
            ),
        ],
        "",
    );
}

#[test]
fn module_aliases_from_explicit_globs_shadow_the_prelude() {
    accepts(
        &[
            ("ops.moss", "fn go() {}\n"),
            ("aliases.moss", "import \"./ops.moss\" as char;\n"),
            (
                "main.moss",
                "import \"./aliases.moss\" use *;\nfn main() { char::go(); }\n",
            ),
        ],
        "",
    );
}

#[test]
fn unshadowed_prelude_defaults_remain_available() {
    accepts(&[("main.moss", "type Local;\nassume Base { fn main() { bind StdBase; print(\"defaults\"); putchar(char::newline); } }\n")], "defaults\n");
}

#[test]
fn conflicting_explicit_imports_still_fail_in_either_order() {
    for (first, second) in [("a", "b"), ("b", "a")] {
        for names in ["Base", "*"] {
            let main = format!("import \"./{first}.moss\" use {names};\nimport \"./{second}.moss\" use {names};\nfn main() {{}}\n");
            rejects(
                &[
                    ("a.moss", "type Base;\n"),
                    ("b.moss", "type Base;\n"),
                    ("main.moss", &main),
                ],
                "!E ",
            );
        }
    }
}

#[test]
fn explicit_imports_conflict_with_local_declarations() {
    rejects(
        &[
            ("scalar.moss", SCALAR),
            (
                "main.moss",
                "import \"./scalar.moss\" use Base;\ntype Base;\nfn main() {}\n",
            ),
        ],
        "!E ",
    );
}

#[test]
fn an_explicit_prelude_glob_is_an_ordinary_import() {
    rejects(
        &[(
            "main.moss",
            "import \"prelude.moss\" use *;\ntype Base;\nfn main() {}\n",
        )],
        "!E ",
    );
}

#[test]
fn implicit_prelude_names_are_not_reexported() {
    rejects(
        &[
            ("helper.moss", "fn exported() {}\n"),
            (
                "main.moss",
                "import \"./helper.moss\" use Base;\nfn main() {}\n",
            ),
        ],
        "!C ",
    );
}

#[test]
fn explicit_named_imports_can_reexport_prelude_symbols() {
    accepts(
        &[
            ("helper.moss", "import \"base.moss\" use Base;\n"),
            (
                "main.moss",
                "import \"./helper.moss\" use Base;\nassume Base { fn main() {} }\n",
            ),
        ],
        "",
    );
}

#[test]
fn a_named_import_promotes_a_glob_binding_to_an_export() {
    accepts(
        &[
            (
                "helper.moss",
                "import \"base.moss\" use *;\nimport \"base.moss\" use Base;\n",
            ),
            (
                "main.moss",
                "import \"./helper.moss\" use Base;\nassume Base { fn main() {} }\n",
            ),
        ],
        "",
    );
}
