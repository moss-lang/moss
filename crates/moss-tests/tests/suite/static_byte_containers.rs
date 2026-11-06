//! Literal backing remains poolable through private, typed GC containers.
use moss_cli::binaryen::{optimize_wasm, parse_opt, StaticBytes};
use moss_tests::{build, oracle_run, run_module, wasm_text};

#[test]
fn globals_pool_strings_stored_in_boxed_records_and_gc_arrays() {
    let entry = "tests/fixtures/array_literals/stored.moss";
    let raw = build(entry);
    let globals = optimize_wasm(
        raw.clone(),
        parse_opt("3")
            .unwrap()
            .with_static_bytes(StaticBytes::Globals),
    )
    .unwrap();
    let text = wasm_text(&globals);
    assert!(
        text.contains("array.new_data"),
        "private container storage blocked pooling"
    );
    assert_eq!(
        text.lines()
            .filter(|line| line.trim_start().starts_with("(memory "))
            .count(),
        1,
        "the globals target must not add a linear memory"
    );
    for (args, expected) in [
        (vec![], "hello\nhello\nB!\n"),
        (vec!["one"], "hello\nhello\nC!\n"),
    ] {
        assert_eq!(run_module(&raw, &args), expected);
        assert_eq!(oracle_run(entry, &args), expected);
        assert_eq!(run_module(&globals, &args), expected);
    }
}
