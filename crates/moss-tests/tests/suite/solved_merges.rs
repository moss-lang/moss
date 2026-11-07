//! A missing intermediate type cannot hide a solved merge constraint.
use moss_tests::{compile, oracle_rejects, oracle_run, run};

#[test]
fn solved_merges_check_only_their_actual_free_types() {
    for name in ["merge_intermediate_absent", "merge_intermediate_present"] {
        let entry = format!("tests/errors/{name}.moss");
        let native = compile(&entry, "lib/prelude.moss");
        assert!(!native.is_module(), "{entry} must be refused before emission");
        assert!(native.text().starts_with("!Y "), "{}", native.text());
        let diagnostic = oracle_rejects(&entry);
        assert!(diagnostic.contains("to be one type, which its context merged (D43)"), "{diagnostic}");
    }
    let valid = "tests/wasi/merge_intermediate_valid.moss";
    assert_eq!(run(valid, &[]), "");
    assert_eq!(oracle_run(valid, &[]), "");
}
