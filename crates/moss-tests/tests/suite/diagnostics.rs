//! CLI diagnostics retain source ranges independently of their rendering.
use moss_tests::{compiler, sources, std_main, wasmtime};

fn report(entry: &str) -> String {
    let out = wasmtime(
        compiler(),
        Some("lib/prelude.moss"),
        &[entry, "--diagnostics=human"],
    );
    assert_eq!(out.code, 0, "{}", out.stderr);
    assert!(!out.is_module(), "{entry} unexpectedly compiled");
    out.text()
}

#[test]
fn missing_names_are_underlined_at_each_use() {
    let text = std_main("assume Std {\n  fn main() {\n    missing();\n    missing();\n  }\n}");
    let dir = sources(&[("main.moss", &text)]);
    let entry = format!("{dir}/main.moss");
    let got = report(&entry);
    assert_eq!(
        got.matches("this name is not in scope: `missing`").count(),
        2,
        "{got}"
    );
    assert!(got.contains(&format!("{entry}:5:5")), "{got}");
    assert!(got.contains(&format!("{entry}:6:5")), "{got}");
    assert!(
        got.contains("5 |     missing();\n  |     ^^^^^^^^^"),
        "{got}"
    );
}

#[test]
fn columns_handle_utf8_tabs_and_crlf() {
    let text = std_main("assume Std {\n  fn main() {\n\tlet text = \"λ\"; missing();\n  }\n}")
        .replace('\n', "\r\n");
    let dir = sources(&[("main.moss", &text)]);
    let entry = format!("{dir}/main.moss");
    let got = report(&entry);
    assert!(got.contains(&format!("{entry}:5:21")), "{got}");
    assert!(
        got.contains("5 |     let text = \"λ\"; missing();\n  |                     ^^^^^^^^^"),
        "{got}"
    );
    assert!(!got.contains('\r'), "{got:?}");
}

#[test]
fn duplicate_declarations_point_to_the_second_definition() {
    let dir = sources(&[("main.moss", "type Twice;\ntype Twice;\n")]);
    let entry = format!("{dir}/main.moss");
    let got = report(&entry);
    assert!(
        got.starts_with("error: duplicate definition: `Twice`\n"),
        "{got}"
    );
    assert!(got.contains(&format!("{entry}:2:1")), "{got}");
    assert!(got.contains("2 | type Twice;\n  | ^^^^^^^^^^^"), "{got}");
    assert!(
        got.contains(&format!(
            "note: previous definition is here\n  --> {entry}:1:1"
        )),
        "{got}"
    );
}

#[test]
fn syntax_errors_in_multiple_modules_keep_their_own_locations() {
    let dir = sources(&[
        (
            "main.moss",
            "import \"./broken.moss\" as broken;\ntype Missing =\n",
        ),
        ("broken.moss", "type Broken =\n"),
    ]);
    let got = report(&format!("{dir}/main.moss"));
    assert_eq!(got.matches("invalid syntax").count(), 2, "{got}");
    assert!(got.contains(&format!("{dir}/main.moss:3:1")), "{got}");
    assert!(got.contains(&format!("{dir}/broken.moss:2:1")), "{got}");
    assert!(got.contains("3 | \n  | ^\n"), "{got}");
}

#[test]
fn unreadable_imports_report_the_missing_path_without_a_fake_excerpt() {
    let dir = sources(&[("main.moss", "import \"./absent.moss\" as absent;\n")]);
    let got = report(&format!("{dir}/main.moss"));
    assert!(got.contains("cannot read this source file"), "{got}");
    assert!(got.contains(&format!("--> {dir}/absent.moss\n")), "{got}");
    assert!(!got.contains(" | "), "{got}");
}

#[test]
fn backend_errors_have_messages_and_expression_locations() {
    let text = std_main("assume Std {\n  fn main() {\n    break;\n  }\n}");
    let dir = sources(&[("main.moss", &text)]);
    let entry = format!("{dir}/main.moss");
    let got = report(&entry);
    assert!(
        got.starts_with("error: break is only allowed inside a loop"),
        "{got}"
    );
    assert!(got.contains(&format!("{entry}:5:5")), "{got}");
    assert!(got.contains("5 |     break;\n  |     ^^^^^"), "{got}");
}

#[test]
fn missing_semicolons_name_the_expected_token() {
    let text =
        std_main("assume Std {\n  fn main() {\n    let n = 1\n    print(\"hello\");\n  }\n}");
    let dir = sources(&[("main.moss", &text)]);
    let entry = format!("{dir}/main.moss");
    let got = report(&entry);
    assert!(
        got.starts_with("error: invalid syntax: expected `;`"),
        "{got}"
    );
    assert!(got.contains(&format!("{entry}:6:5")), "{got}");
}

#[test]
fn missing_main_has_a_path_without_an_invented_source_range() {
    let dir = sources(&[("main.moss", "type Thing;\n")]);
    let entry = format!("{dir}/main.moss");
    let got = report(&entry);
    assert!(
        got.starts_with("error: no valid main function was found"),
        "{got}"
    );
    assert!(got.contains(&format!("--> {entry}\n")), "{got}");
    assert!(!got.contains(" | "), "{got}");
}

#[test]
fn invalid_main_signatures_report_the_declaration() {
    let dir = sources(&[("main.moss", "fn main(value: ()) {}\n")]);
    let entry = format!("{dir}/main.moss");
    let got = report(&entry);
    assert!(
        got.starts_with("error: no valid main function was found"),
        "{got}"
    );
    assert!(got.contains(&format!("{entry}:1:1")), "{got}");
    assert!(got.contains("1 | fn main(value: ()) {}\n  | ^"), "{got}");
}

#[test]
fn missing_functor_arrows_name_the_expected_token() {
    let dir = sources(&[("main.moss", "functor F: Int Int;\n")]);
    let got = report(&format!("{dir}/main.moss"));
    assert!(
        got.starts_with("error: invalid syntax: expected `->`"),
        "{got}"
    );
}

#[test]
fn semantic_errors_found_in_codegen_keep_their_code_domain() {
    // Semantic e_incoherent and back-end c_unknown_op both have code 17.
    // A stale functor binding must never be described as an unknown Wasm op.
    let got = report("tests/wasi/functor_fixed_stale.moss");
    assert!(
        got.contains(
            "error: the available binding was provided at a different type instantiation: `Heap`"
        ),
        "{got}"
    );
    assert!(
        got.contains("tests/wasi/functor_fixed_stale.moss:15:5"),
        "{got}"
    );
    assert!(!got.contains("unknown WebAssembly instruction"), "{got}");
}
