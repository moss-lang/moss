"""Wasm backend, first slice: compile the scalar subset to a WASI module.

This is the stage-5 swap-out the pipeline was designed for
(docs/design/semantics.md §11): it consumes the same core IR as the
interpreter, and because every contextual dependency is already an explicit
keyed need, "compilation" of context is just parameter passing — a
function's val needs become extra i32 parameters, and each call site's
needs_map says which caller values to pass (D2: vals are the only context
alive at runtime).

Scalar subset: every value is an i32. Chars are codepoints, Ints are i32,
`()` is 0, Bool is 0/1 (the nominal tag is erased), other unit values get
small codes. Std's methods on Int/Char compile to Wasm instructions;
`putchar` and `print` are fd_write shims. Tags and records are boxed in
linear memory behind a bump allocator (units stay scalar, so their codes
must stay below the heap base); match compiles to code/pointer tests;
strings are [len|bytes] entering via a WASI args shim; CellInt is a boxed
word; IntList is a handle to a [len, cap, elems] block that grows by
copying. Path, fn binds, and closures are not yet in the slice and report
themselves as such.

Alongside that, and under it, the primitive context of D52: a program may
assume `Wasm` and `Wasi` (lib/wasm.moss, lib/wasip1.moss) instead of
`Std`. There the compilation is direct — a `Wasm` intrinsic is the
instruction of the same name, a `Wasi` function is an import of the same
name — so such a program needs no shims at all. The i64 half of `Wasm` is
declared but not covered here, since every value in this slice is an i32.
"""

import sys

from . import ir
from .collect import Program, SymKind, Symbol

# Wasm opcodes
I32_CONST = b"\x41"
LOCAL_GET = b"\x20"
LOCAL_SET = b"\x21"
CALL = b"\x10"
DROP = b"\x1a"
IF = b"\x04"
ELSE = b"\x05"
END = b"\x0b"
BLOCK = b"\x02"
LOOP = b"\x03"
BR = b"\x0c"
BR_IF = b"\x0d"
RETURN = b"\x0f"
I32_EQZ = b"\x45"
EMPTY = b"\x40"
I32 = b"\x7f"
UNREACHABLE = b"\x00"
I32_LOAD8 = b"\x2d"
I32_STORE8 = b"\x3a"
HEAP_BASE = 1024
I32_LOAD = b"\x28"
I32_STORE = b"\x36"

# Function index space: the WASI imports the module actually uses, then a
# fixed run of shims, then the compiled Moss functions, then `_start`.
# Only the shims' *offsets* are constant — how many imports come before
# them depends on what the program calls (D52: `Wasi` is reachable from
# Moss), so indices are computed from the backend's import table.
WASI = "wasi_snapshot_preview1"

# The shims call these, so they are always imported, always first.
BASE_IMPORTS = (("fd_write", 4, 1), ("args_sizes_get", 2, 1), ("args_get", 2, 1))
FD_WRITE, ARGS_SIZES_GET, ARGS_GET = 0, 1, 2

SHIMS = ("putchar", "alloc", "first_arg", "list_push", "print", "slice", "concat")
S_PUTCHAR, S_ALLOC, S_FIRST_ARG, S_LIST_PUSH, S_PRINT, S_SLICE, S_CONCAT = range(
    len(SHIMS)
)

BINOPS = {
    "add": b"\x6a",
    "sub": b"\x6b",
    "mul": b"\x6c",
    "div": b"\x6d",
    "rem": b"\x6f",
    "and": b"\x71",
    "or": b"\x72",
    "xor": b"\x73",
    "shl": b"\x74",
    "shr": b"\x75",
    "eq": b"\x46",
    "ne": b"\x47",
    "lt": b"\x48",
    "gt": b"\x4a",
    "le": b"\x4c",
    "ge": b"\x4e",
}


class NotCompilable(Exception):
    """This slice of the backend doesn't cover the construct."""


def uleb(n: int) -> bytes:
    out = bytearray()
    while True:
        byte = n & 0x7F
        n >>= 7
        if n:
            out.append(byte | 0x80)
        else:
            out.append(byte)
            return bytes(out)


def sleb(n: int) -> bytes:
    out = bytearray()
    while True:
        byte = n & 0x7F
        n >>= 7
        if (n == 0 and not byte & 0x40) or (n == -1 and byte & 0x40):
            out.append(byte)
            return bytes(out)
        out.append(byte | 0x80)


def vec(items: list[bytes]) -> bytes:
    return uleb(len(items)) + b"".join(items)


def section(sid: int, body: bytes) -> bytes:
    return bytes([sid]) + uleb(len(body)) + body


def name(text: str) -> bytes:
    raw = text.encode()
    return uleb(len(raw)) + raw


# `Wasm` intrinsics (lib/wasm.moss), each the instruction of the same name.
# Loads and stores carry their natural alignment and a zero offset; the i64
# half of the module is declared but not in this slice.
def _memarg(align: int) -> bytes:
    return uleb(align) + uleb(0)


WASM_OPS = {
    "unreachable": UNREACHABLE,
    "i32_load": b"\x28" + _memarg(2),
    "i32_load8_s": b"\x2c" + _memarg(0),
    "i32_load8_u": b"\x2d" + _memarg(0),
    "i32_load16_s": b"\x2e" + _memarg(1),
    "i32_load16_u": b"\x2f" + _memarg(1),
    "i32_store": b"\x36" + _memarg(2),
    "i32_store8": b"\x3a" + _memarg(0),
    "i32_store16": b"\x3b" + _memarg(1),
    "memory_size": b"\x3f\x00",
    "memory_grow": b"\x40\x00",
    "memory_copy": b"\xfc\x0a\x00\x00",
    "memory_fill": b"\xfc\x0b\x00",
}
for _i, _name in enumerate(
    "eqz eq ne lt_s lt_u gt_s gt_u le_s le_u ge_s ge_u".split()
):
    WASM_OPS[f"i32_{_name}"] = bytes([0x45 + _i])
for _i, _name in enumerate(
    "clz ctz popcnt add sub mul div_s div_u rem_s rem_u and or xor shl"
    " shr_s shr_u rotl rotr".split()
):
    WASM_OPS[f"i32_{_name}"] = bytes([0x67 + _i])
WASM_OPS["i32_extend8_s"] = b"\xc0"
WASM_OPS["i32_extend16_s"] = b"\xc1"


class Backend:
    def __init__(self, program: Program, lower, natives: dict):
        self.program = program
        self.lower = lower
        self.natives = natives  # key -> interp provision (for classification)
        self.fn_index: dict[int, int] = {}  # id(Symbol) -> func index
        self.compiled: dict[int, tuple] = {}  # func index -> (param count, body)
        self.unit_codes: dict[int, int] = {}
        self.lib = {}
        for module in program.modules.values():
            for short in ("bool", "num", "char", "int", "std", "string",
                          "strlist", "cell", "list", "wasm", "wasip1"):
                if module.path.endswith(f"lib/{short}.moss"):
                    self.lib[short] = module
        self.imports: list[tuple[str, int, int]] = []  # (field, params, results)
        self.import_index: dict[str, int] = {}
        for field, nparams, nresults in BASE_IMPORTS:
            self.wasi_import(field, nparams, nresults)

    def wasi_import(self, field: str, nparams: int, nresults: int) -> int:
        """The index of a WASI import, adding it to the module if new."""
        if field not in self.import_index:
            self.import_index[field] = len(self.imports)
            self.imports.append((field, nparams, nresults))
        return self.import_index[field]

    def shim(self, which: int) -> int:
        return len(self.imports) + which

    def first_fn(self) -> int:
        return len(self.imports) + len(SHIMS)

    def unit_code(self, symbol: Symbol) -> int:
        if "bool" in self.lib:
            if symbol is self.lib["bool"].names.get("True"):
                return 1
            if symbol is self.lib["bool"].names.get("False"):
                return 0
        code = self.unit_codes.setdefault(id(symbol), len(self.unit_codes) + 2)
        if code >= HEAP_BASE:
            raise NotCompilable("too many unit kinds for this slice")
        return code

    def native_const(self, provision) -> int | None:
        from .interp import CharVal, IntVal

        if isinstance(provision, CharVal):
            return ord(provision.value)
        if isinstance(provision, IntVal):
            return provision.value
        return None

    def method_op(self, key) -> bytes | None:
        """A native method key that compiles to a plain instruction."""
        if not isinstance(key, tuple):
            return None
        receiver, method = key
        if "num" in self.lib and method.module is self.lib["num"]:
            short = method.name.lstrip(".")
            if short == "neg":
                return None  # handled specially (0 - x)
            return BINOPS.get(short)
        return None

    def is_putchar(self, key) -> bool:
        return "std" in self.lib and key is self.lib["std"].names.get("putchar")

    def passed_need(self, key) -> bool:
        """Whether a need key is passed as a parameter: vals without a
        native constant provision. Everything else compiles at its use site
        (constants inline, native methods as instructions, putchar as the
        shim) — or fails there if this slice doesn't cover it. Unused needs
        cost nothing, which matters because `main` assumes all of Std."""
        return (
            not isinstance(key, tuple)
            and key.kind == SymKind.VAL
            and key not in self.natives
        )

    def val_needs(self, symbol: Symbol) -> list:
        return [k for k in self.lower.needs_of[id(symbol)] if self.passed_need(k)]

    def compile_fn(self, symbol: Symbol) -> int:
        if id(symbol) in self.fn_index:
            return self.fn_index[id(symbol)]
        index = self.first_fn() + len(self.fn_index)
        self.fn_index[id(symbol)] = index
        fn = self.lower.fns[id(symbol)]
        needs = self.val_needs(symbol)
        compiler = FnCompiler(self, fn, needs)
        body = compiler.run()  # may recursively compile callees
        self.compiled[index] = (compiler.param_count, body)
        return index

    def build(self, main: Symbol) -> bytes:
        # Two passes. The first discovers which WASI functions the program
        # calls, and that count is what fixes every function index; the
        # second compiles against the final numbering. Compilation is
        # deterministic, so the second pass discovers nothing new — which
        # the assertion below states rather than assumes.
        self.compile_fn(main)
        discovered = len(self.imports)
        self.fn_index.clear()
        self.compiled.clear()
        self.unit_codes.clear()
        main_index = self.compile_fn(main)
        assert len(self.imports) == discovered, "the import set must be stable"
        ordered = [self.compiled[i] for i in sorted(self.compiled)]

        types: list[tuple[int, int]] = []
        type_of: dict[tuple[int, int], int] = {}

        def ty(nparams: int, nresults: int) -> int:
            key = (nparams, nresults)
            if key not in type_of:
                type_of[key] = len(types)
                types.append(key)
            return type_of[key]

        import_types = [ty(nparams, nresults) for _, nparams, nresults in self.imports]
        shim_types = [
            ty(1, 0),  # putchar: (char) -> ()
            ty(1, 1),  # alloc: (nbytes) -> addr
            ty(0, 1),  # first_arg: () -> string
            ty(2, 1),  # list_push: (handle, value) -> dummy
            ty(1, 0),  # print: (string) -> ()
            ty(3, 1),  # slice: (string, start, len) -> string
            ty(2, 1),  # concat: (string, string) -> string
        ]
        func_types = [ty(count, 1) for count, _ in ordered]
        start_type = ty(0, 0)

        type_section = section(
            1, vec([b"\x60" + vec([I32] * p) + vec([I32] * r) for p, r in types])
        )
        import_section = section(
            2,
            vec(
                [
                    name(WASI) + name(field) + b"\x00" + uleb(t)
                    for (field, _, _), t in zip(self.imports, import_types)
                ]
            ),
        )
        function_section = section(
            3,
            vec(
                [uleb(t) for t in shim_types]
                + [uleb(t) for t in func_types]
                + [uleb(start_type)]
            ),
        )
        memory_section = section(5, vec([b"\x00" + uleb(2)]))
        start_index = self.first_fn() + len(ordered)
        export_section = section(
            7,
            vec(
                [
                    name("memory") + b"\x02" + uleb(0),
                    name("_start") + b"\x00" + uleb(start_index),
                ]
            ),
        )
        putchar_body = self.putchar_shim()
        alloc_body = self.alloc_fn()
        # _start initializes the heap pointer, then runs main.
        start_body = (
            vec([])
            + I32_CONST
            + sleb(16)
            + I32_CONST
            + sleb(HEAP_BASE)
            + I32_STORE
            + uleb(2)
            + uleb(0)
            + CALL
            + uleb(main_index)
            + DROP
            + END
        )
        bodies = (
            [
                putchar_body,
                alloc_body,
                self.first_arg_shim(),
                self.list_push_shim(),
                self.print_shim(),
                self.slice_shim(),
                self.concat_shim(),
            ]
            + [b for _, b in ordered]
            + [start_body]
        )
        code_section = section(10, vec([uleb(len(b)) + b for b in bodies]))
        return (
            b"\x00asm\x01\x00\x00\x00"
            + type_section
            + import_section
            + function_section
            + memory_section
            + export_section
            + code_section
        )

    def alloc_fn(self) -> bytes:
        # (nbytes) -> addr: bump the heap pointer stored at address 16.
        body = bytearray()
        body += vec([uleb(1) + I32])  # one scratch local
        body += I32_CONST + sleb(16) + I32_LOAD + uleb(2) + uleb(0)
        body += LOCAL_SET + uleb(1)
        body += I32_CONST + sleb(16)
        body += LOCAL_GET + uleb(1) + LOCAL_GET + uleb(0) + BINOPS["add"]
        body += I32_STORE + uleb(2) + uleb(0)
        body += LOCAL_GET + uleb(1)
        body += END
        return bytes(body)

    def first_arg_shim(self) -> bytes:
        # () -> string ptr: read argv[1] via WASI and box it as [len|bytes].
        # Locals: 0 argv, 1 buf, 2 p, 3 n, 4 s, 5 i.
        b = bytearray()
        b += vec([uleb(6) + I32])
        # args_sizes_get(24, 28)
        b += I32_CONST + sleb(24) + I32_CONST + sleb(28) + CALL + uleb(ARGS_SIZES_GET) + DROP
        # argv = alloc(argc * 4); buf = alloc(bufsize)
        b += I32_CONST + sleb(24) + I32_LOAD + uleb(2) + uleb(0)
        b += I32_CONST + sleb(4) + BINOPS["mul"] + CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(0)
        b += I32_CONST + sleb(28) + I32_LOAD + uleb(2) + uleb(0)
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(1)
        b += LOCAL_GET + uleb(0) + LOCAL_GET + uleb(1) + CALL + uleb(ARGS_GET) + DROP
        # if argc < 2: return an empty string
        b += I32_CONST + sleb(24) + I32_LOAD + uleb(2) + uleb(0)
        b += I32_CONST + sleb(2) + BINOPS["lt"]
        b += IF + EMPTY
        b += I32_CONST + sleb(4) + CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(4)
        b += LOCAL_GET + uleb(4) + I32_CONST + sleb(0) + I32_STORE + uleb(2) + uleb(0)
        b += LOCAL_GET + uleb(4) + RETURN
        b += END
        # p = argv[1] (NUL-terminated); n = strlen(p)
        b += LOCAL_GET + uleb(0) + I32_LOAD + uleb(2) + uleb(4) + LOCAL_SET + uleb(2)
        b += I32_CONST + sleb(0) + LOCAL_SET + uleb(3)
        b += BLOCK + EMPTY + LOOP + EMPTY
        b += LOCAL_GET + uleb(2) + LOCAL_GET + uleb(3) + BINOPS["add"]
        b += I32_LOAD8 + uleb(0) + uleb(0) + I32_EQZ + BR_IF + uleb(1)
        b += LOCAL_GET + uleb(3) + I32_CONST + sleb(1) + BINOPS["add"] + LOCAL_SET + uleb(3)
        b += BR + uleb(0) + END + END
        # s = alloc(4 + n); *s = n; copy bytes
        b += I32_CONST + sleb(4) + LOCAL_GET + uleb(3) + BINOPS["add"]
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(4)
        b += LOCAL_GET + uleb(4) + LOCAL_GET + uleb(3) + I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(0) + LOCAL_SET + uleb(5)
        b += BLOCK + EMPTY + LOOP + EMPTY
        b += LOCAL_GET + uleb(5) + LOCAL_GET + uleb(3) + BINOPS["ge"] + BR_IF + uleb(1)
        b += LOCAL_GET + uleb(4) + LOCAL_GET + uleb(5) + BINOPS["add"]
        b += LOCAL_GET + uleb(2) + LOCAL_GET + uleb(5) + BINOPS["add"]
        b += I32_LOAD8 + uleb(0) + uleb(0)
        b += I32_STORE8 + uleb(0) + uleb(4)
        b += LOCAL_GET + uleb(5) + I32_CONST + sleb(1) + BINOPS["add"] + LOCAL_SET + uleb(5)
        b += BR + uleb(0) + END + END
        b += LOCAL_GET + uleb(4)
        b += END
        return bytes(b)

    def list_push_shim(self) -> bytes:
        # (handle, value) -> 0. A list is a one-word handle pointing at
        # [len, cap, elems...]; growing copies to a fresh block (the bump
        # allocator never frees). Locals: 2 data, 3 len, 4 cap, 5 nd, 6 i.
        b = bytearray()
        b += vec([uleb(5) + I32])
        b += LOCAL_GET + uleb(0) + I32_LOAD + uleb(2) + uleb(0) + LOCAL_SET + uleb(2)
        b += LOCAL_GET + uleb(2) + I32_LOAD + uleb(2) + uleb(0) + LOCAL_SET + uleb(3)
        b += LOCAL_GET + uleb(2) + I32_LOAD + uleb(2) + uleb(4) + LOCAL_SET + uleb(4)
        # if len == cap: grow
        b += LOCAL_GET + uleb(3) + LOCAL_GET + uleb(4) + BINOPS["eq"]
        b += IF + EMPTY
        # nd = alloc(8 + 8*cap); nd.len = len; nd.cap = cap*2
        b += I32_CONST + sleb(8)
        b += LOCAL_GET + uleb(4) + I32_CONST + sleb(8) + BINOPS["mul"]
        b += BINOPS["add"] + CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(5)
        b += LOCAL_GET + uleb(5) + LOCAL_GET + uleb(3) + I32_STORE + uleb(2) + uleb(0)
        b += LOCAL_GET + uleb(5)
        b += LOCAL_GET + uleb(4) + I32_CONST + sleb(2) + BINOPS["mul"]
        b += I32_STORE + uleb(2) + uleb(4)
        # copy elements
        b += I32_CONST + sleb(0) + LOCAL_SET + uleb(6)
        b += BLOCK + EMPTY + LOOP + EMPTY
        b += LOCAL_GET + uleb(6) + LOCAL_GET + uleb(3) + BINOPS["ge"] + BR_IF + uleb(1)
        b += LOCAL_GET + uleb(5)
        b += LOCAL_GET + uleb(6) + I32_CONST + sleb(4) + BINOPS["mul"] + BINOPS["add"]
        b += LOCAL_GET + uleb(2)
        b += LOCAL_GET + uleb(6) + I32_CONST + sleb(4) + BINOPS["mul"] + BINOPS["add"]
        b += I32_LOAD + uleb(2) + uleb(8)
        b += I32_STORE + uleb(2) + uleb(8)
        b += LOCAL_GET + uleb(6) + I32_CONST + sleb(1) + BINOPS["add"] + LOCAL_SET + uleb(6)
        b += BR + uleb(0) + END + END
        # handle -> nd; data = nd
        b += LOCAL_GET + uleb(0) + LOCAL_GET + uleb(5) + I32_STORE + uleb(2) + uleb(0)
        b += LOCAL_GET + uleb(5) + LOCAL_SET + uleb(2)
        b += END
        # data[8 + 4*len] = value; data.len = len + 1
        b += LOCAL_GET + uleb(2)
        b += LOCAL_GET + uleb(3) + I32_CONST + sleb(4) + BINOPS["mul"] + BINOPS["add"]
        b += LOCAL_GET + uleb(1) + I32_STORE + uleb(2) + uleb(8)
        b += LOCAL_GET + uleb(2)
        b += LOCAL_GET + uleb(3) + I32_CONST + sleb(1) + BINOPS["add"]
        b += I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(0)
        b += END
        return bytes(b)

    def slice_shim(self) -> bytes:
        # (string, start, len) -> string: a fresh [len|bytes] block holding
        # the requested run of bytes. Locals: 3 s, 4 i.
        b = bytearray()
        b += vec([uleb(2) + I32])
        # s = alloc(4 + len); *s = len
        b += I32_CONST + sleb(4) + LOCAL_GET + uleb(2) + BINOPS["add"]
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(3)
        b += LOCAL_GET + uleb(3) + LOCAL_GET + uleb(2) + I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(0) + LOCAL_SET + uleb(4)
        b += BLOCK + EMPTY + LOOP + EMPTY
        b += LOCAL_GET + uleb(4) + LOCAL_GET + uleb(2) + BINOPS["ge"] + BR_IF + uleb(1)
        b += LOCAL_GET + uleb(3) + LOCAL_GET + uleb(4) + BINOPS["add"]
        b += LOCAL_GET + uleb(0) + LOCAL_GET + uleb(1) + BINOPS["add"]
        b += LOCAL_GET + uleb(4) + BINOPS["add"]
        b += I32_LOAD8 + uleb(0) + uleb(4)
        b += I32_STORE8 + uleb(0) + uleb(4)
        b += LOCAL_GET + uleb(4) + I32_CONST + sleb(1) + BINOPS["add"] + LOCAL_SET + uleb(4)
        b += BR + uleb(0) + END + END
        b += LOCAL_GET + uleb(3)
        b += END
        return bytes(b)

    def concat_shim(self) -> bytes:
        # (a, b) -> string: one fresh block holding a's bytes then b's.
        # Locals: 2 s, 3 i, 4 la.
        b = bytearray()
        b += vec([uleb(3) + I32])
        b += LOCAL_GET + uleb(0) + I32_LOAD + uleb(2) + uleb(0) + LOCAL_SET + uleb(4)
        # s = alloc(4 + la + lb); *s = la + lb
        b += I32_CONST + sleb(4) + LOCAL_GET + uleb(4) + BINOPS["add"]
        b += LOCAL_GET + uleb(1) + I32_LOAD + uleb(2) + uleb(0) + BINOPS["add"]
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(2)
        b += LOCAL_GET + uleb(2)
        b += LOCAL_GET + uleb(4)
        b += LOCAL_GET + uleb(1) + I32_LOAD + uleb(2) + uleb(0) + BINOPS["add"]
        b += I32_STORE + uleb(2) + uleb(0)
        for source, offset in ((0, None), (1, 4)):
            # Copy this operand's bytes; the second lands after the first.
            b += I32_CONST + sleb(0) + LOCAL_SET + uleb(3)
            b += BLOCK + EMPTY + LOOP + EMPTY
            b += LOCAL_GET + uleb(3)
            b += LOCAL_GET + uleb(source) + I32_LOAD + uleb(2) + uleb(0)
            b += BINOPS["ge"] + BR_IF + uleb(1)
            b += LOCAL_GET + uleb(2) + LOCAL_GET + uleb(3) + BINOPS["add"]
            if offset is not None:
                b += LOCAL_GET + uleb(4) + BINOPS["add"]
            b += LOCAL_GET + uleb(source) + LOCAL_GET + uleb(3) + BINOPS["add"]
            b += I32_LOAD8 + uleb(0) + uleb(4)
            b += I32_STORE8 + uleb(0) + uleb(4)
            b += LOCAL_GET + uleb(3) + I32_CONST + sleb(1) + BINOPS["add"]
            b += LOCAL_SET + uleb(3)
            b += BR + uleb(0) + END + END
        b += LOCAL_GET + uleb(2)
        b += END
        return bytes(b)

    def print_shim(self) -> bytes:
        # (string) -> (): fd_write the whole [len|bytes] buffer.
        b = bytearray()
        b += vec([])
        b += I32_CONST + sleb(0)
        b += LOCAL_GET + uleb(0) + I32_CONST + sleb(4) + BINOPS["add"]
        b += I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(4)
        b += LOCAL_GET + uleb(0) + I32_LOAD + uleb(2) + uleb(0)
        b += I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(1)
        b += I32_CONST + sleb(0)
        b += I32_CONST + sleb(1)
        b += I32_CONST + sleb(12)
        b += CALL + uleb(FD_WRITE) + DROP
        b += END
        return bytes(b)

    def putchar_shim(self) -> bytes:
        # iovec at 0: ptr=8, len=1; byte at 8; retptr at 12.
        body = bytearray()
        body += vec([])  # no extra locals
        body += I32_CONST + sleb(0) + I32_CONST + sleb(8) + b"\x36" + uleb(2) + uleb(0)
        body += I32_CONST + sleb(4) + I32_CONST + sleb(1) + b"\x36" + uleb(2) + uleb(0)
        body += I32_CONST + sleb(8) + LOCAL_GET + uleb(0) + b"\x3a" + uleb(0) + uleb(0)
        body += I32_CONST + sleb(1)
        body += I32_CONST + sleb(0)
        body += I32_CONST + sleb(1)
        body += I32_CONST + sleb(12)
        body += CALL + uleb(FD_WRITE) + DROP
        body += END
        return bytes(body)


class FnCompiler:
    def __init__(self, backend: Backend, fn: ir.FnIR, needs: list):
        self.b = backend
        self.fn = fn
        self.code = bytearray()
        self.slots: dict = {}  # name or ('need', key) -> local index
        self.provisions: dict = {}  # key -> ('local', idx) | ('const', n)
        index = 0
        if fn.has_this:
            self.slots["this"] = index
            index += 1
        for p in fn.params:
            self.slots[p] = index
            index += 1
        for key in needs:
            self.provisions[key] = ("local", index)
            index += 1
        self.param_count = index
        self.extra_locals = 0
        self.break_depths: list[int] = []
        self.depth = 0

    def local(self) -> int:
        idx = self.param_count + self.extra_locals
        self.extra_locals += 1
        return idx

    def run(self) -> bytes:
        self.block(self.fn.body)
        self.code += END
        decls = vec([uleb(self.extra_locals) + I32] if self.extra_locals else [])
        return bytes(decls) + bytes(self.code)

    # Provisions: how a need key is satisfied *here*.

    def provide(self, key):
        if key in self.provisions:
            return self.provisions[key]
        provision = self.b.natives.get(key)
        const = self.b.native_const(provision)
        if const is not None:
            return ("const", const)
        raise NotCompilable(
            f"need `{getattr(key, 'name', key)}` has no compilable provision"
        )

    def push_provision(self, key):
        kind, value = self.provide(key)
        if kind == "const":
            self.code += I32_CONST + sleb(value)
        else:
            self.code += LOCAL_GET + uleb(value)

    # Statements

    def block(self, node: ir.Block):
        saved = dict(self.provisions)
        for stmt in node.stmts:
            self.stmt(stmt)
        if node.tail is None:
            self.code += I32_CONST + sleb(0)
        else:
            self.expr(node.tail)
        self.provisions = saved

    def stmt(self, node):
        if isinstance(node, (ir.Let, ir.Assign)):
            self.expr(node.expr)
            if node.name not in self.slots:
                self.slots[node.name] = self.local()
            self.code += LOCAL_SET + uleb(self.slots[node.name])
        elif isinstance(node, ir.BindVal):
            self.expr(node.expr)
            idx = self.local()
            self.code += LOCAL_SET + uleb(idx)
            self.provisions[node.key] = ("local", idx)
        elif isinstance(node, ir.BindFn):
            raise NotCompilable("fn binds")
        elif isinstance(node, ir.ExprStmt):
            self.expr(node.expr)
            self.code += DROP
        elif isinstance(node, ir.While):
            self.code += BLOCK + EMPTY
            self.depth += 1
            self.break_depths.append(self.depth)
            self.code += LOOP + EMPTY
            self.depth += 1
            self.expr(node.cond)
            self.code += I32_EQZ + BR_IF + uleb(1)
            self.block(node.body)
            self.code += DROP + BR + uleb(0) + END + END
            self.depth -= 2
            self.break_depths.pop()
        elif isinstance(node, ir.Loop):
            self.code += BLOCK + EMPTY
            self.depth += 1
            self.break_depths.append(self.depth)
            self.code += LOOP + EMPTY
            self.depth += 1
            self.block(node.body)
            self.code += DROP + BR + uleb(0) + END + END
            self.depth -= 2
            self.break_depths.pop()
        elif isinstance(node, ir.Block):
            self.block(node)
            self.code += DROP
        else:
            raise NotCompilable(type(node).__name__)

    # Expressions: every expression leaves exactly one i32.

    def expr(self, node):
        if isinstance(node, ir.Unit):
            self.code += I32_CONST + sleb(0)
        elif isinstance(node, ir.Local):
            self.code += LOCAL_GET + uleb(self.slots[node.name])
        elif isinstance(node, ir.This):
            self.code += LOCAL_GET + uleb(self.slots["this"])
        elif isinstance(node, ir.NeedVal):
            self.push_provision(node.key)
        elif isinstance(node, ir.MakeUnit):
            self.code += I32_CONST + sleb(self.b.unit_code(node.symbol))
        elif isinstance(node, ir.MakeTag):
            symbol = node.symbol
            if "bool" in self.b.lib and symbol is self.b.lib["bool"].names.get("Bool"):
                self.expr(node.payload)  # the Bool tag is erased
            else:
                tmp = self.local()
                self.code += I32_CONST + sleb(8) + CALL + uleb(self.b.shim(S_ALLOC))
                self.code += LOCAL_SET + uleb(tmp)
                self.code += LOCAL_GET + uleb(tmp)
                self.code += I32_CONST + sleb(self.b.unit_code(symbol))
                self.code += I32_STORE + uleb(2) + uleb(0)
                self.code += LOCAL_GET + uleb(tmp)
                self.expr(node.payload)
                self.code += I32_STORE + uleb(2) + uleb(4)
                self.code += LOCAL_GET + uleb(tmp)
        elif isinstance(node, ir.MakeRecord):
            count = len(node.fields)
            tmp = self.local()
            self.code += I32_CONST + sleb(4 + 4 * count) + CALL + uleb(self.b.shim(S_ALLOC))
            self.code += LOCAL_SET + uleb(tmp)
            self.code += LOCAL_GET + uleb(tmp)
            self.code += I32_CONST + sleb(self.b.unit_code(node.symbol))
            self.code += I32_STORE + uleb(2) + uleb(0)
            for i, (_, expr) in enumerate(node.fields):
                self.code += LOCAL_GET + uleb(tmp)
                self.expr(expr)
                self.code += I32_STORE + uleb(2) + uleb(4 + 4 * i)
            self.code += LOCAL_GET + uleb(tmp)
        elif isinstance(node, ir.Field):
            if node.index < 0:
                raise NotCompilable("field access without layout")
            self.expr(node.obj)
            self.code += I32_LOAD + uleb(2) + uleb(4 + 4 * node.index)
        elif isinstance(node, ir.Call):
            self.call(node)
        elif isinstance(node, ir.If):
            self.expr(node.cond)
            self.code += IF + I32
            self.depth += 1
            self.block(node.then)
            self.code += ELSE
            if node.els is None:
                self.code += I32_CONST + sleb(0)
            elif isinstance(node.els, ir.Block):
                self.block(node.els)
            else:
                self.expr(node.els)
            self.code += END
            self.depth -= 1
        elif isinstance(node, ir.Return):
            if node.expr is None:
                self.code += I32_CONST + sleb(0)
            else:
                self.expr(node.expr)
            self.code += RETURN
        elif isinstance(node, ir.Break):
            target = self.break_depths[-1]
            self.code += BR + uleb(self.depth - target) + I32_CONST + sleb(0)
        elif isinstance(node, ir.Block):
            self.block(node)
        elif isinstance(node, ir.Match):
            self.match(node)
        else:
            raise NotCompilable(type(node).__name__)

    def match(self, node: ir.Match):
        scrut = self.local()
        self.expr(node.scrutinee)
        self.code += LOCAL_SET + uleb(scrut)
        bool_sym = self.b.lib["bool"].names.get("Bool") if "bool" in self.b.lib else None
        arms = list(node.arms)

        def bind_and_body(arm):
            pat = arm.pat
            if pat.head is None:
                if pat.binder is not None:
                    slot = self.slots.setdefault(pat.binder, self.local())
                    self.code += LOCAL_GET + uleb(scrut) + LOCAL_SET + uleb(slot)
            else:
                if pat.binder is not None:
                    slot = self.slots.setdefault(pat.binder, self.local())
                    self.code += LOCAL_GET + uleb(scrut)
                    self.code += I32_LOAD + uleb(2) + uleb(4)
                    self.code += LOCAL_SET + uleb(slot)
                if pat.fields is not None:
                    for _, binder, index in pat.fields:
                        slot = self.slots.setdefault(binder, self.local())
                        self.code += LOCAL_GET + uleb(scrut)
                        self.code += I32_LOAD + uleb(2) + uleb(4 + 4 * index)
                        self.code += LOCAL_SET + uleb(slot)
            if isinstance(arm.body, ir.Block):
                self.block(arm.body)
            else:
                self.expr(arm.body)

        def chain(i: int):
            if i >= len(arms):
                self.code += UNREACHABLE
                return
            arm = arms[i]
            pat = arm.pat
            if pat.head is None:
                bind_and_body(arm)
                return
            if pat.head is bool_sym:
                raise NotCompilable("matching through Bool (use if)")
            code = self.b.unit_code(pat.head)
            boxed = pat.binder is not None or pat.fields is not None
            if boxed or pat.head.kind == SymKind.TAG:
                # (s >= heap) & (mem[s] == code); the load is safe either way.
                self.code += LOCAL_GET + uleb(scrut) + I32_CONST + sleb(HEAP_BASE)
                self.code += BINOPS["ge"]
                self.code += LOCAL_GET + uleb(scrut) + I32_LOAD + uleb(2) + uleb(0)
                self.code += I32_CONST + sleb(code) + BINOPS["eq"]
                self.code += BINOPS["and"]
            else:
                self.code += LOCAL_GET + uleb(scrut)
                self.code += I32_CONST + sleb(code) + BINOPS["eq"]
            self.code += IF + I32
            self.depth += 1
            bind_and_body(arm)
            self.code += ELSE
            chain(i + 1)
            self.code += END
            self.depth -= 1

        chain(0)

    def call(self, node: ir.Call):
        kind, target = node.callee
        if kind == "direct":
            if node.this is not None:
                self.expr(node.this)
            for arg in node.args:
                self.expr(arg)
            for callee_key, caller_key in node.needs_map:
                if self.b.passed_need(callee_key):
                    self.push_provision(caller_key)
            index = self.b.compile_fn(target)
            self.code += CALL + uleb(index)
            return
        # Contextual call: a native method, putchar, or out of slice.
        key = target
        op = self.b.method_op(key)
        if op is not None:
            self.expr(node.this)
            for arg in node.args:
                self.expr(arg)
            self.code += op
            return
        if isinstance(key, tuple):
            receiver, method = key
            short = method.name.lstrip(".")
            if "num" in self.b.lib and method.module is self.b.lib["num"]:
                if short == "neg":
                    self.code += I32_CONST + sleb(0)
                    self.expr(node.this)
                    self.code += BINOPS["sub"]
                    return
                if short == "not":
                    self.expr(node.this)
                    self.code += I32_CONST + sleb(1) + BINOPS["xor"]
                    return
            if "char" in self.b.lib and method.module is self.b.lib["char"]:
                if short in ("code", "char"):
                    self.expr(node.this)  # chars are already their codepoints
                    return
            if "string" in self.b.lib and method.module is self.b.lib["string"]:
                if short == "length":
                    self.expr(node.this)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    return
                if short == "get":
                    self.expr(node.this)
                    self.expr(node.args[0])
                    self.code += BINOPS["add"] + I32_LOAD8 + uleb(0) + uleb(4)
                    return
                if short == "slice":
                    self.expr(node.this)
                    self.expr(node.args[0])
                    self.expr(node.args[1])
                    self.code += CALL + uleb(self.b.shim(S_SLICE))
                    return
                if short == "concat":
                    self.expr(node.this)
                    self.expr(node.args[0])
                    self.code += CALL + uleb(self.b.shim(S_CONCAT))
                    return
            if "list" in self.b.lib and method.module is self.b.lib["list"]:
                if short == "push":
                    self.expr(node.this)
                    self.expr(node.args[0])
                    self.code += CALL + uleb(self.b.shim(S_LIST_PUSH))
                    return
                if short == "length":
                    self.expr(node.this)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    return
                if short == "get":
                    self.expr(node.this)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    self.expr(node.args[0])
                    self.code += I32_CONST + sleb(4) + BINOPS["mul"] + BINOPS["add"]
                    self.code += I32_LOAD + uleb(2) + uleb(8)
                    return
                if short == "set":
                    self.expr(node.this)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    self.expr(node.args[0])
                    self.code += I32_CONST + sleb(4) + BINOPS["mul"] + BINOPS["add"]
                    self.expr(node.args[1])
                    self.code += I32_STORE + uleb(2) + uleb(8)
                    self.code += I32_CONST + sleb(0)
                    return
            if "strlist" in self.b.lib and method.module is self.b.lib["strlist"]:
                # A String is a pointer, so a StrList is an IntList.
                if short == "push":
                    self.expr(node.this)
                    self.expr(node.args[0])
                    self.code += CALL + uleb(self.b.shim(S_LIST_PUSH))
                    return
                if short == "length":
                    self.expr(node.this)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    return
                if short == "get":
                    self.expr(node.this)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    self.expr(node.args[0])
                    self.code += I32_CONST + sleb(4) + BINOPS["mul"] + BINOPS["add"]
                    self.code += I32_LOAD + uleb(2) + uleb(8)
                    return
            if "cell" in self.b.lib and method.module is self.b.lib["cell"]:
                if short == "read":
                    self.expr(node.this)
                    self.code += I32_LOAD + uleb(2) + uleb(0)
                    return
                if short == "write":
                    self.expr(node.this)
                    self.expr(node.args[0])
                    self.code += I32_STORE + uleb(2) + uleb(0) + I32_CONST + sleb(0)
                    return
            raise NotCompilable(f"method `{method.name}`")
        if self.b.is_putchar(key):
            for arg in node.args:
                self.expr(arg)
            self.code += CALL + uleb(self.b.shim(S_PUTCHAR)) + I32_CONST + sleb(0)
            return
        if "string" in self.b.lib and key is self.b.lib["string"].names.get("first_arg"):
            self.code += CALL + uleb(self.b.shim(S_FIRST_ARG))
            return
        if "string" in self.b.lib and key is self.b.lib["string"].names.get("print"):
            for arg in node.args:
                self.expr(arg)
            self.code += CALL + uleb(self.b.shim(S_PRINT)) + I32_CONST + sleb(0)
            return
        empty_list = "list" in self.b.lib and key is self.b.lib["list"].names.get(
            "int_list"
        )
        empty_list = empty_list or (
            "strlist" in self.b.lib
            and key is self.b.lib["strlist"].names.get("str_list")
        )
        if empty_list:
            tmp = self.local()
            self.code += I32_CONST + sleb(4) + CALL + uleb(self.b.shim(S_ALLOC)) + LOCAL_SET + uleb(tmp)
            data = self.local()
            self.code += I32_CONST + sleb(40) + CALL + uleb(self.b.shim(S_ALLOC)) + LOCAL_SET + uleb(data)
            self.code += LOCAL_GET + uleb(data) + I32_CONST + sleb(0)
            self.code += I32_STORE + uleb(2) + uleb(0)
            self.code += LOCAL_GET + uleb(data) + I32_CONST + sleb(8)
            self.code += I32_STORE + uleb(2) + uleb(4)
            self.code += LOCAL_GET + uleb(tmp) + LOCAL_GET + uleb(data)
            self.code += I32_STORE + uleb(2) + uleb(0)
            self.code += LOCAL_GET + uleb(tmp)
            return
        if "cell" in self.b.lib and key is self.b.lib["cell"].names.get("cell_int"):
            tmp = self.local()
            self.code += I32_CONST + sleb(4) + CALL + uleb(self.b.shim(S_ALLOC)) + LOCAL_SET + uleb(tmp)
            self.code += LOCAL_GET + uleb(tmp) + I32_CONST + sleb(0)
            self.code += I32_STORE + uleb(2) + uleb(0)
            self.code += LOCAL_GET + uleb(tmp)
            return
        module = getattr(key, "module", None)
        if "wasm" in self.b.lib and module is self.b.lib["wasm"]:
            self.wasm_instruction(key, node)
            return
        if "wasip1" in self.b.lib and module is self.b.lib["wasip1"]:
            # D52: a WASI function is exactly an import of this module.
            for arg in node.args:
                self.expr(arg)
            results = 0 if key.decl.ret is None else 1
            index = self.b.wasi_import(key.name, len(node.args), results)
            self.code += CALL + uleb(index)
            if results == 0:
                self.code += I32_CONST + sleb(0)  # the Moss call yields ()
            return
        raise NotCompilable(f"contextual fn `{getattr(key, 'name', key)}`")

    def wasm_instruction(self, key: Symbol, node: ir.Call):
        """A `Wasm` intrinsic is the instruction of the same name."""
        op = WASM_OPS.get(key.name)
        if op is None:
            raise NotCompilable(f"`{key.name}` (i64 is not in this slice)")
        for arg in node.args:
            self.expr(arg)
        self.code += op
        if key.decl.ret is None:
            self.code += I32_CONST + sleb(0)  # stores and fills yield ()


def build(program: Program, lower, main: Symbol) -> bytes:
    from .interp import native_env

    natives = native_env(program, [])
    backend = Backend(program, lower, natives)
    return backend.build(main)
