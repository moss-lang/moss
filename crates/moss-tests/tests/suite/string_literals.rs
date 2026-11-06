//! Quoted syntax stays an ordinary, context-rebindable byte construction protocol.
use moss_tests::{build, oracle_run, run_module};

#[test]
fn quoted_strings_preserve_utf8_escapes_and_immutable_contents() {
    let entry = "tests/fixtures/string_literals/quoted.moss";
    let expected = "hello\né\n\"\\\t\r\ndone\nAcé\n";
    let raw = build(entry);
    assert_eq!(run_module(&raw, &[]), expected);
    assert_eq!(oracle_run(entry, &[]), expected);
}

#[test]
fn quoted_strings_resolve_ordinary_names_and_only_run_when_reached() {
    let entry = "tests/fixtures/string_literals/rebound.moss";
    let expected = "BFBWWWFO\n";
    let raw = build(entry);
    assert_eq!(run_module(&raw, &[]), expected);
    assert_eq!(oracle_run(entry, &[]), expected);
}

#[test]
fn quoted_strings_reject_invalid_escapes_and_missing_statement_separators() {
    for body in [r#"let text = "bad\q";"#, r#""a" "b""#] {
        let entry = moss_tests::source(&moss_tests::std_program("", body));
        assert!(!moss_tests::compile(&entry, "lib/prelude.moss").is_module());
        assert_ne!(moss_tests::oracle_command(&["--elab-only", &entry]).code, 0);
    }
}

#[test]
fn standard_gc_and_raw_string_providers_preserve_builder_aliases() {
    for (name, expected) in [
        ("library_smoke", "hi\nih\nhi\nhihi\nA\nB\none\n"),
        ("library_raw_smoke", "hi\nih\nhi\nhihi\nA\nB\none\n"),
        ("library_array", "hi\nhihi\n"),
    ] {
        let entry = format!("tests/fixtures/string_literals/{name}.moss");
        let args = ["one"];
        assert_eq!(run_module(&build(&entry), &args), expected);
        assert_eq!(oracle_run(&entry, &args), expected);
    }
}

#[test]
fn gc_strings_cross_wasi_file_and_argument_boundaries() {
    let entry = "tests/fixtures/string_literals/library_files.moss";
    let path = "tests/fixtures/string_literals/quoted.moss";
    let expected = format!("i\n{path}\n");
    assert_eq!(run_module(&build(entry), &[path]), expected);
    assert_eq!(oracle_run(entry, &[path]), expected);
}

#[test]
fn quoted_literals_compile_without_one_builder_local_per_byte() {
    let payload = "abcd".repeat(1024);
    let body = format!("print(\"{payload}\");");
    let entry = moss_tests::source(&moss_tests::std_program("", &body));
    assert_eq!(run_module(&build(&entry), &[]), payload);
    assert_eq!(oracle_run(&entry, &[]), payload);
}

#[test]
fn runtime_length_builders_use_the_standard_protocol() {
    let entry = "tests/fixtures/string_literals/runtime_builder.moss";
    for (args, expected) in [(vec![], "literalA\n"), (vec!["one", "two"], "literalABC\n")] {
        assert_eq!(run_module(&build(entry), &args), expected);
        assert_eq!(oracle_run(entry, &args), expected);
    }
}
