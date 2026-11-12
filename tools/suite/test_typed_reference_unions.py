from suite.support import (
    build,
    oracle_run,
    run_module,
)

# Precise reference lanes remain correct through unions and heap containers.


def test_reordered_and_widened_unions_preserve_reference_payloads():
    entry = "tests/fixtures/array_literals/main.moss"
    raw = build(entry)
    for args, expected in [
        ([], "hello\nhello\nB!\nM\n"),
        (["one"], "C!\nhello\nC!\nM\n"),
        (["one", "two", "three"], "E!\nhello\nE!\nM\n"),
    ]:
        assert run_module(raw, args) == expected
        assert oracle_run(entry, args) == expected


def test_recursive_boxes_preserve_reference_payloads():
    entry = "tests/fixtures/array_literals/recursive.moss"
    assert run_module(build(entry), []) == "A\n"
    assert oracle_run(entry, []) == "A\n"


def test_boxed_records_and_gc_arrays_preserve_reference_payloads():
    entry = "tests/fixtures/array_literals/stored.moss"
    raw = build(entry)
    for args, expected in [([], "hello\nhello\nB!\n"), (["one"], "hello\nhello\nC!\n")]:
        assert run_module(raw, args) == expected
        assert oracle_run(entry, args) == expected
