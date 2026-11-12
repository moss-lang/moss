from suite import support as moss
from suite.support import (
    build,
    oracle_run,
    read,
    run_module,
)

# Quoted syntax stays an ordinary, context-rebindable byte construction protocol.


def test_quoted_strings_preserve_utf8_escapes_and_immutable_contents():
    entry = "tests/fixtures/string_literals/quoted.moss"
    expected = 'hello\né\n"\\\t\r\ndone\nAcé\n'
    raw = build(entry)
    assert run_module(raw, []) == expected
    assert oracle_run(entry, []) == expected


def test_quoted_strings_resolve_ordinary_names_and_only_run_when_reached():
    entry = "tests/fixtures/string_literals/rebound.moss"
    expected = "BFBWWWFO\n"
    raw = build(entry)
    assert run_module(raw, []) == expected
    assert oracle_run(entry, []) == expected


def test_quoted_strings_reject_invalid_escapes_and_missing_statement_separators():
    for body in ['let text = "bad\\q";', '"a" "b"']:
        entry = moss.source(moss.std_program("", body))
        assert not (moss.compile_source(entry, "lib/prelude.moss").is_module())
        assert moss.oracle_command(["--elab-only", entry]).code != 0


def test_standard_gc_and_raw_string_providers_preserve_builder_aliases():
    for name, expected in [
        ("library_smoke", "hi\nih\nhi\nhihi\nA\nB\none\n"),
        ("library_raw_smoke", "hi\nih\nhi\nhihi\nA\nB\none\n"),
        ("library_array", "hi\nhihi\n"),
    ]:
        entry = f"tests/fixtures/string_literals/{name}.moss"
        args = ["one"]
        assert run_module(build(entry), args) == expected
        assert oracle_run(entry, args) == expected


def test_gc_strings_cross_wasi_file_and_argument_boundaries():
    entry = "tests/fixtures/string_literals/library_files.moss"
    path = "tests/fixtures/string_literals/quoted.moss"
    expected = f"i\n{path}\n"
    assert run_module(build(entry), [path]) == expected
    assert oracle_run(entry, [path]) == expected


def test_quoted_literals_compile_without_one_builder_local_per_byte():
    payload = "abcd" * 1024
    body = f'print("{payload}");'
    entry = moss.source(moss.std_program("", body))
    assert run_module(build(entry), []) == payload
    assert oracle_run(entry, []) == payload


def test_runtime_length_builders_use_the_standard_protocol():
    entry = "tests/fixtures/string_literals/runtime_builder.moss"
    for args, expected in [([], "literalA\n"), (["one", "two"], "literalABC\n")]:
        assert run_module(build(entry), args) == expected
        assert oracle_run(entry, args) == expected


def test_all_byte_constants_round_trip_through_the_nominal_union():
    entry = "tests/fixtures/string_literals/all_bytes.moss"
    assert run_module(build(entry), []) == "OK\n"
    assert oracle_run(entry, []) == "OK\n"


def test_quoted_payloads_are_independent_of_integer_zero_and_one():
    entry = "tests/fixtures/string_literals/byte_independent.moss"
    assert run_module(build(entry), []) == "OK\n"
    assert oracle_run(entry, []) == "OK\n"


def test_quoted_scalars_dispatch_to_their_utf8_width():
    entry = "tests/fixtures/string_literals/grouped_protocol.moss"
    assert run_module(build(entry), []) == "1234\n"
    assert oracle_run(entry, []) == "1234\n"


def test_quoted_utf8_scalar_boundaries_round_trip_including_nul():
    payload = "\x00\x7f\x80߿ࠀ\ud7ff\ue000\uffff𐀀\U0010ffff"
    entry = moss.source(moss.std_program("", f'print("{payload}");'))
    assert run_module(build(entry), []) == payload
    assert oracle_run(entry, []) == payload


def test_quoted_strings_reject_non_scalar_and_malformed_utf8():
    malformed = [
        [0x80],
        [0xBF],
        [0xC0, 0x80],
        [0xC1, 0xBF],
        [0xC2],
        [0xC2, 0x20],
        [0xE0, 0x80, 0x80],
        [0xE0, 0x9F, 0xBF],
        [0xED, 0xA0, 0x80],
        [0xED, 0xBF, 0xBF],
        [0xE1, 0x80],
        [0xE2, 0x28, 0xA1],
        [0xF0, 0x80, 0x80, 0x80],
        [0xF0, 0x8F, 0xBF, 0xBF],
        [0xF0, 0x90, 0x80],
        [0xF0, 0x90, 0x28, 0x80],
        [0xF4, 0x90, 0x80, 0x80],
        [0xF5, 0x80, 0x80, 0x80],
        [0xFF],
    ]
    source = moss.std_program("", 'print("PAYLOAD");')
    prefix, suffix = source.split("PAYLOAD", 1)
    for octets in malformed:
        entry = moss.source("")
        raw = b"".join([prefix.encode(), bytes(octets), suffix.encode()])
        (moss.repo() / entry).write_bytes(raw)
        assert not (moss.compile_source(entry, "lib/prelude.moss").is_module()), (
            f"accepted {octets!r}"
        )
        oracle = moss.oracle_command(["--elab-only", entry])
        assert oracle.code == 63, (
            f"oracle accepted or did not parse-reject {octets!r}: {oracle.stderr}"
        )


def test_integer_values_cannot_supply_standard_byte_payloads():
    entry = moss.source(moss.std_program("", "let b = string_write1(string_begin(1), 0, 65);"))
    assert not (moss.compile_source(entry, "lib/prelude.moss").is_module())
    assert moss.oracle_command(["--elab-only", entry]).code != 0


def test_typed_manual_utf8_chunks_cover_every_prefix_variant():
    expected = "A¢ࠀ\ud7ff\uffff𐀀\U00050000\U0010ffff"
    for name in ["typed_chunks", "typed_chunks_raw"]:
        entry = f"tests/fixtures/string_literals/{name}.moss"
        assert run_module(build(entry), []) == expected
        assert oracle_run(entry, []) == expected


def test_typed_manual_utf8_chunks_reject_invalid_ranges_and_rep_repacking():
    for call in [
        "string_write1(b, 0, byte::x80)",
        "string_write2(b, 0, byte::xc0, byte::x80)",
        "string_write3(b, 0, byte::ThreeE0 { lead = byte::xe0, second = byte::x80 }, byte::x80)",
        "string_write3(b, 0, byte::ThreeED { lead = byte::xed, second = byte::xa0 }, byte::x80)",
        "string_write4(b, 0, byte::FourF0 { lead = byte::xf0, second = byte::x80 }, byte::x80, byte::x80)",
        "string_write4(b, 0, byte::FourF4 { lead = byte::xf4, second = byte::x90 }, byte::x80, byte::x80)",
        "string_write2(b, 0, byte::xc2, byte::x41)",
        "match byte::x80 { byte::Cont808F(rep) => string_write1(b, 0, byte::Ascii(rep)) }",
    ]:
        body = f"let b = string_begin(4); let invalid = {call};"
        entry = moss.source(moss.std_program("", body))
        assert not (moss.compile_source(entry, "lib/prelude.moss").is_module()), f"accepted {call}"
        oracle = moss.oracle_command(["--elab-only", entry])
        assert oracle.code == 64, f"oracle did not type-reject {call}: {oracle.stderr}"


def test_raw_multiline_strings_preserve_text_and_control_newlines():
    for body, expected in [
        ("print(\n  > Hello,\n  > world!\n);", "Hello,\nworld!\n"),
        ("print(\n  >\n);", "\n"),
        ("print(\n > A¢ࠀ𐀀\n);", "A¢ࠀ𐀀\n"),
        ("print(\n  > first\n  >\n  > third\n  >\n);", "first\n\nthird\n\n"),
        ('print(\n\t>  é \\" # >\t \n    > second\n);', ' é \\" # >\t \nsecond\n'),
        ("print(\r\n  > first\r\n  > second\r\n);", "first\nsecond\n"),
        ("print(\r  > first\r  > second\r);", "first\nsecond\n"),
        ("let s =\n > text\n; print(s);", "text\n"),
        ("print(\n > a\n);\n\nprint(\n > b\n);", "a\nb\n"),
        ('print(" > quoted"); # > comment\nprint(\n > raw\n);', " > quotedraw\n"),
        ("if 2 > 1 { print(\n > yes\n); }", "yes\n"),
        ("if 2\n >= 1 { if (8\n >> 1) == 4 { print(\n > yes\n); } }", "yes\n"),
        ("if 2\n >1 { print(\n > yes\n); }", "yes\n"),
    ]:
        entry = moss.source(moss.std_program("", body))
        assert run_module(build(entry), []) == expected, f"{body}"
        assert oracle_run(entry, []) == expected, f"{body}"


def test_raw_multiline_strings_require_line_leading_markers_and_adjacent_lines():
    for body in [
        "print(> text\n);",
        "print(\n >\ttext\n);",
        "print(\n > first\n\n > second\n);",
        "print(\n > first\n # comment\n > second\n);",
        "if 2\n > 1 {}",
        "print(\n > first\n >second\n);",
    ]:
        entry = moss.source(moss.std_program("", body))
        assert not (moss.compile_source(entry, "lib/prelude.moss").is_module()), f"{body}"
        assert moss.oracle_command(["--elab-only", entry]).code != 0, f"{body}"


def test_raw_multiline_expansion_preserves_diagnostic_source_positions():
    entry = moss.source('fn main() {\n let text =\n > a "quote" \\\n > b\n ; @\n}')
    out = moss.oracle_command(["--elab-only", entry])
    assert out.code == 63
    assert "5:4:" in out.stderr, f"{out.stderr}"


def test_raw_multiline_strings_use_the_contextual_string_protocol():
    quoted = read("tests/fixtures/string_literals/rebound.moss")
    raw = (
        quoted.replace("../../../lib/", "{root}/lib/")
        .replace('"abc"', "\n        > abc\n")
        .replace('""', "\n        >\n")
    )
    raw = raw.replace("bind byte::x61", "bind byte::x0a = byte::Ascii(10);\n      bind byte::x61")
    entry = moss.source(raw)
    assert run_module(build(entry), []) == "BWFBWWWWFC\n"
    assert oracle_run(entry, []) == "BWFBWWWWFC\n"


def test_raw_multiline_strings_reject_malformed_utf8():
    program = moss.std_program("", "print(\n > PAYLOAD\n);")
    prefix, suffix = program.split("PAYLOAD", 1)
    for octets in [[0xFF], [0xED, 0xA0, 0x80], [0xF4, 0x90, 0x80, 0x80]]:
        entry = moss.source("")
        (moss.repo() / entry).write_bytes(prefix.encode() + bytes(octets) + suffix.encode())
        assert not (moss.compile_source(entry, "lib/prelude.moss").is_module())
        assert moss.oracle_command(["--elab-only", entry]).code == 63
