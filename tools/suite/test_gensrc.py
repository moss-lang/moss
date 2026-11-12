"""The checked-in compiler and library tables match their generators."""

import re

from gensrc import byte_class, byte_values, chars, generated, wasm_ops
from suite.support import read


def test_files_match_their_generators():
    for path, text in generated().items():
        assert read(path) == text, f"regenerate {path} with python3 tools/gensrc.py"


def test_chars_values_are_single_chars():
    names, values = zip(*chars())
    assert len(set(names)) == len(names), "a char constant is named twice"
    assert len(set(values)) == len(values), "two char constants are one character"
    assert all(len(value) == 1 for value in values)


def test_byte_constants_cover_every_value_once():
    values = byte_values()
    assert len(values) == 256
    signature = read("lib/byte.moss")
    provider = read("lib/wasibyte.moss")
    for index, (name, value) in enumerate(values):
        assert value == index
        assert name == f"x{index:02x}"
        assert f"  val {name}: {byte_class(value)};" in signature
        assert f"  bind byte::{name} = byte_{name}();" in provider
    assert sum(line.lstrip().startswith("val x") for line in signature.splitlines()) == 256
    assert "wasistd.moss" not in provider
    assert "int.moss" not in provider


def test_byte_ranges_match_utf8_encoding_rules():
    # Check the partition against an independent UTF-8 decoder.
    def valid(*octets):
        try:
            bytes(octets).decode()
        except UnicodeDecodeError:
            return False
        return True

    for value in range(256):
        classes = [
            name
            for name, accepts in [
                ("Ascii", valid(value)),
                ("Cont808F", valid(0xF4, value, 0x80, 0x80)),
                ("Cont909F", valid(0xF0, value, 0x80, 0x80) and valid(0xED, value, 0x80)),
                ("ContA0BF", valid(0xE0, value, 0x80)),
                ("Lead2", valid(value, 0x80)),
            ]
            if accepts
        ]
        three = {
            (False, True): "LeadE0",
            (True, True): "LeadE1EC_EEEF",
            (True, False): "LeadED",
        }
        four = {
            (False, True): "LeadF0",
            (True, True): "LeadF1F3",
            (True, False): "LeadF4",
        }
        if name := three.get((valid(value, 0x80, 0x80), valid(value, 0xA0, 0x80))):
            classes.append(name)
        if name := four.get((valid(value, 0x80, 0x80, 0x80), valid(value, 0xBF, 0x80, 0x80))):
            classes.append(name)
        assert len(classes) <= 1, f"overlapping ranges for {value:02x}: {classes}"
        assert byte_class(value) == (classes[0] if classes else "Invalid")


def test_every_wasm_item_has_an_instruction():
    declared = set(re.findall(r"^\s*(?:fn|val)\s+([^\s(:]+)\s*[( :]", read("lib/wasm.moss"), re.M))
    assert declared
    instructions = {op.name for op in wasm_ops()}
    # These instructions depend on the nominal layout and are selected by the backend.
    missing = {
        name
        for name in declared
        if name not in instructions and name.removeprefix("Addr.") not in instructions
    }
    assert missing == {
        ".boxed",
        "Ref.deref",
        "Ref.set",
        ".arrayed",
        "Array.length",
        "Array.get",
        "Array.set",
        "Array.fill",
        "Array.copy",
        "StructCast.cast",
        "ArrayCast.cast",
    }
