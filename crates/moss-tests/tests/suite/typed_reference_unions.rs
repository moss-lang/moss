//! Precise reference lanes remain correct through unions and heap containers.
use moss_tests::{build, oracle_run, run_module};

#[test]
fn reordered_and_widened_unions_preserve_reference_payloads() {
    let entry = "tests/fixtures/array_literals/main.moss";
    let raw = build(entry);
    for (args, expected) in [
        (vec![], "hello\nhello\nB!\nM\n"),
        (vec!["one"], "C!\nhello\nC!\nM\n"),
        (vec!["one", "two", "three"], "E!\nhello\nE!\nM\n"),
    ] {
        assert_eq!(run_module(&raw, &args), expected);
        assert_eq!(oracle_run(entry, &args), expected);
    }
}

#[test]
fn recursive_boxes_preserve_reference_payloads() {
    let entry = "tests/fixtures/array_literals/recursive.moss";
    assert_eq!(run_module(&build(entry), &[]), "A\n");
    assert_eq!(oracle_run(entry, &[]), "A\n");
}

#[test]
fn boxed_records_and_gc_arrays_preserve_reference_payloads() {
    let entry = "tests/fixtures/array_literals/stored.moss";
    let raw = build(entry);
    for (args, expected) in [
        (vec![], "hello\nhello\nB!\n"),
        (vec!["one"], "hello\nhello\nC!\n"),
    ] {
        assert_eq!(run_module(&raw, &args), expected);
        assert_eq!(oracle_run(entry, &args), expected);
    }
}
