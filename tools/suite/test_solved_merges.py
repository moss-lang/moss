from suite.support import (
    compile_source,
    oracle_rejects,
    oracle_run,
    run,
)

# A missing intermediate type cannot hide a solved merge constraint.


def test_solved_merges_check_only_their_actual_free_types():
    for name in ["merge_intermediate_absent", "merge_intermediate_present"]:
        entry = f"tests/errors/{name}.moss"
        native = compile_source(entry, "lib/prelude.moss")
        assert not (native.is_module()), f"{entry} must be refused before emission"
        assert native.text().startswith("!Y "), f"{native.text()}"
        diagnostic = oracle_rejects(entry)
        assert "to be one type, which its context merged (D43)" in diagnostic, f"{diagnostic}"
    valid = "tests/wasi/merge_intermediate_valid.moss"
    assert run(valid, []) == ""
    assert oracle_run(valid, []) == ""
