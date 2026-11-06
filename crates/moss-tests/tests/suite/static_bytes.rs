//! The actual generic Moss construction protocol through the production backend.
use moss_tests::{build, run_module, wasm_text};

#[test]
fn production_optimizer_preserves_the_compiler_fixpoint() {
    for storage in [
        moss_cli::binaryen::StaticBytes::Memory,
        moss_cli::binaryen::StaticBytes::Globals,
    ] {
        let optimized = moss_cli::binaryen::optimize_wasm(
            moss_tests::compiler_raw().to_vec(),
            moss_cli::binaryen::parse_opt("3")
                .unwrap()
                .with_static_bytes(storage),
        )
        .unwrap();
        let next = moss_tests::compile_self(&moss_tests::wasm_file(&optimized));
        assert!(
            next == moss_tests::compiler_raw(),
            "the {storage:?} optimizer changed the compiler"
        );
    }
}

#[test]
fn array_literals_pool_through_a_union_without_affecting_dynamic_arrays() {
    let entry = "tests/fixtures/array_literals/main.moss";
    let raw = build(entry);
    let cases = [
        (vec![], "hello\nhello\nB!\nM\n"),
        (vec!["one"], "C!\nhello\nC!\nM\n"),
        (vec!["one", "two", "three"], "E!\nhello\nE!\nM\n"),
    ];
    for (args, expected) in &cases {
        assert_eq!(run_module(&raw, args), *expected);
        assert_eq!(moss_tests::oracle_run(entry, args), *expected);
    }
    for storage in [
        moss_cli::binaryen::StaticBytes::Memory,
        moss_cli::binaryen::StaticBytes::Globals,
    ] {
        for level in ["2", "3", "4", "s", "z"] {
            let optimized = moss_cli::binaryen::optimize_wasm(
                raw.clone(),
                moss_cli::binaryen::parse_opt(level)
                    .unwrap()
                    .with_static_bytes(storage),
            )
            .unwrap_or_else(|error| panic!("-O{level} {storage:?}: {error}"));
            let text = wasm_text(&optimized);
            assert!(text.contains("(data "), "-O{level}: missing pooled data");
            assert!(
                text.contains("array.set"),
                "-O{level}: dynamic array writes disappeared"
            );
            match storage {
                moss_cli::binaryen::StaticBytes::Memory => {
                    assert!(text.contains("hello"), "-O{level}: missing byte payload");
                    assert!(
                        text.contains("ref.i31"),
                        "-O{level}: missing static handles"
                    );
                    assert_eq!(
                        text.lines()
                            .filter(|line| line.trim_start().starts_with("(memory "))
                            .count(),
                        2
                    );
                }
                moss_cli::binaryen::StaticBytes::Globals => {
                    assert!(
                        text.contains("array.new_data"),
                        "-O{level}: missing startup initializer"
                    );
                    assert_eq!(
                        text.lines()
                            .filter(|line| line.trim_start().starts_with("(memory "))
                            .count(),
                        1,
                        "globals must add no memory"
                    );
                    let arrays: std::collections::BTreeSet<_> = text
                        .lines()
                        .filter_map(|line| {
                            line.trim()
                                .strip_prefix("(array.get ")
                                .and_then(|rest| rest.split_whitespace().next())
                        })
                        .collect();
                    assert_eq!(arrays.len(), 1, "-O{level}: backing types did not merge");
                }
            }
            for (args, expected) in &cases {
                assert_eq!(
                    run_module(&optimized, args),
                    *expected,
                    "-O{level} {storage:?}"
                );
            }
        }
    }
}
