"""Modules are validated before use, with the project's Wasm features."""

from suite.support import wasm_file


def module(params):
    def leb(value):
        return bytes([0x80 | (value & 0x7F), value >> 7])

    types = b"\x01\x60" + leb(params) + b"\x7f" * params + b"\0"
    return (
        b"\0asm\x01\0\0\0\x01"
        + leb(len(types))
        + types
        + b"\x03\x02\x01\x00\x0a\x04\x01\x02\x00\x0b"
    )


def test_a_valid_module_is_written():
    assert wasm_file(module(1000)).is_file()


def invalid_module(wasm, message):
    try:
        wasm_file(wasm)
    except AssertionError as error:
        assert message in str(error), error
    else:
        raise AssertionError("the validator accepted an invalid module")


def test_too_many_parameters_are_refused():
    invalid_module(module(4477), "function params size is out of bounds")


def test_only_the_listed_features():
    invalid_module(b"\0asm\x01\0\0\0\x05\x04\x01\x03\x01\x01", "threads")
