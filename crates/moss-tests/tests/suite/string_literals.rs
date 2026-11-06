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

#[test]
fn all_byte_constants_round_trip_through_the_nominal_union() {
    let entry = "tests/fixtures/string_literals/all_bytes.moss";
    assert_eq!(run_module(&build(entry), &[]), "OK\n");
    assert_eq!(oracle_run(entry, &[]), "OK\n");
}

#[test]
fn quoted_payloads_are_independent_of_integer_zero_and_one() {
    let entry = "tests/fixtures/string_literals/byte_independent.moss";
    assert_eq!(run_module(&build(entry), &[]), "OK\n");
    assert_eq!(oracle_run(entry, &[]), "OK\n");
}

#[test]
fn quoted_scalars_dispatch_to_their_utf8_width() {
    let entry = "tests/fixtures/string_literals/grouped_protocol.moss";
    assert_eq!(run_module(&build(entry), &[]), "1234\n");
    assert_eq!(oracle_run(entry, &[]), "1234\n");
}

#[test]
fn quoted_utf8_scalar_boundaries_round_trip_including_nul() {
    let payload = "\0\u{7f}\u{80}\u{7ff}\u{800}\u{d7ff}\u{e000}\u{ffff}\u{10000}\u{10ffff}";
    let entry = moss_tests::source(&moss_tests::std_program("", &format!("print(\"{payload}\");")));
    assert_eq!(run_module(&build(&entry), &[]), payload);
    assert_eq!(oracle_run(&entry, &[]), payload);
}

#[test]
fn quoted_strings_reject_non_scalar_and_malformed_utf8() {
    let malformed: &[&[u8]] = &[
        &[0x80], &[0xbf], &[0xc0, 0x80], &[0xc1, 0xbf], &[0xc2],
        &[0xc2, 0x20], &[0xe0, 0x80, 0x80], &[0xe0, 0x9f, 0xbf],
        &[0xed, 0xa0, 0x80], &[0xed, 0xbf, 0xbf], &[0xe1, 0x80],
        &[0xe2, 0x28, 0xa1], &[0xf0, 0x80, 0x80, 0x80],
        &[0xf0, 0x8f, 0xbf, 0xbf], &[0xf0, 0x90, 0x80],
        &[0xf0, 0x90, 0x28, 0x80], &[0xf4, 0x90, 0x80, 0x80],
        &[0xf5, 0x80, 0x80, 0x80], &[0xff],
    ];
    let source = moss_tests::std_program("", "print(\"PAYLOAD\");");
    let (prefix, suffix) = source.split_once("PAYLOAD").unwrap();
    for bytes in malformed {
        let entry = moss_tests::source("");
        let raw = [prefix.as_bytes(), bytes, suffix.as_bytes()].concat();
        std::fs::write(moss_tests::repo().join(&entry), raw).unwrap();
        assert!(!moss_tests::compile(&entry, "lib/prelude.moss").is_module(), "accepted {bytes:x?}");
        let oracle = moss_tests::oracle_command(&["--elab-only", &entry]);
        assert_eq!(oracle.code, 63, "oracle accepted or did not parse-reject {bytes:x?}: {}", oracle.stderr);
    }
}

#[test]
fn integer_values_cannot_supply_standard_byte_payloads() {
    let entry = moss_tests::source(&moss_tests::std_program("", "let b = string_write1(string_begin(1), 0, 65);"));
    assert!(!moss_tests::compile(&entry, "lib/prelude.moss").is_module());
    assert_ne!(moss_tests::oracle_command(&["--elab-only", &entry]).code, 0);
}

#[test]
fn typed_manual_utf8_chunks_cover_every_prefix_variant() {
    let expected = "A¢\u{800}\u{d7ff}\u{ffff}\u{10000}\u{50000}\u{10ffff}";
    for name in ["typed_chunks", "typed_chunks_raw"] {
        let entry = format!("tests/fixtures/string_literals/{name}.moss");
        assert_eq!(run_module(&build(&entry), &[]), expected);
        assert_eq!(oracle_run(&entry, &[]), expected);
    }
}

#[test]
fn typed_manual_utf8_chunks_reject_invalid_ranges_and_rep_repacking() {
    for call in [
        "string_write1(b, 0, byte::x80)",
        "string_write2(b, 0, byte::xc0, byte::x80)",
        "string_write3(b, 0, byte::ThreeE0 { lead = byte::xe0, second = byte::x80 }, byte::x80)",
        "string_write3(b, 0, byte::ThreeED { lead = byte::xed, second = byte::xa0 }, byte::x80)",
        "string_write4(b, 0, byte::FourF0 { lead = byte::xf0, second = byte::x80 }, byte::x80, byte::x80)",
        "string_write4(b, 0, byte::FourF4 { lead = byte::xf4, second = byte::x90 }, byte::x80, byte::x80)",
        "string_write2(b, 0, byte::xc2, byte::x41)",
        "match byte::x80 { byte::Cont808F(rep) => string_write1(b, 0, byte::Ascii(rep)) }",
    ] {
        let body = format!("let b = string_begin(4); let invalid = {call};");
        let entry = moss_tests::source(&moss_tests::std_program("", &body));
        assert!(!moss_tests::compile(&entry, "lib/prelude.moss").is_module(), "accepted {call}");
        let oracle = moss_tests::oracle_command(&["--elab-only", &entry]);
        assert_eq!(oracle.code, 64, "oracle did not type-reject {call}: {}", oracle.stderr);
    }
}
