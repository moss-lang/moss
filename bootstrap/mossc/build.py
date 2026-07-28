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
copying. A fn bind compiles by specialising the callee per binding, so a
contextual call is a direct call and no function table is needed. `Path`
is a string relative to the WASI preopen, so `pwd` is empty and `read` is
path_open plus fd_read — which means the self-hosted compiler, which
reads its inputs off disk, compiles. What is left outside the slice is
the i64 half of `Wasm` and matching through `Bool`, both of which report
themselves.

Alongside that, and under it, the primitive context of D52: a program may
assume `Wasm` and `Wasi` (lib/wasm.moss, lib/wasip1.moss) instead of
`Std`. There the compilation is direct — a `Wasm` intrinsic is the
instruction of the same name, a `Wasi` function is an import of the same
name — so such a program needs no shims at all. The i64 half of `Wasm` is
declared but not covered here, since every value in this slice is an i32.
"""

import sys

from . import ir
from .ast import TyRecord as ast_TyRecord
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
I64 = b"\x7e"
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
# Most WASI parameters are i32; the rights masks of path_open are not, so
# an import's type comes from its declaration rather than from its arity.
# These are the ones the shims call, so they are always present and always
# first — and registered up front, since the import section is built
# before the shim bodies that use them.
BASE_IMPORTS = (
    ("fd_write", 4, 1),
    ("args_sizes_get", 2, 1),
    ("args_get", 2, 1),
    ("path_open", (I32, I32, I32, I32, I32, I64, I64, I32, I32), 1),
    ("fd_read", 4, 1),
    ("fd_close", 1, 1),
)
FD_WRITE, ARGS_SIZES_GET, ARGS_GET, PATH_OPEN, FD_READ, FD_CLOSE = range(6)

SHIMS = (
    "putchar", "alloc", "arg_at", "arg_count", "list_push", "print",
    "slice", "concat", "join", "read",
)
(
    S_PUTCHAR, S_ALLOC, S_ARG_AT, S_ARG_COUNT, S_LIST_PUSH, S_PRINT,
    S_SLICE, S_CONCAT, S_JOIN, S_READ,
) = range(len(SHIMS))

# WASI: the first preopened directory. Paths are resolved against it, so
# `pwd` is the empty path and everything else is relative to wherever the
# host opened — which is what `wasmtime --dir` decides.
PREOPEN_FD = 3
RIGHT_FD_READ = 2
RIGHT_FD_SEEK = 1

BINOPS_NE = b"\x47"

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
    # Bool is 0/1 with the tag erased, so this is `n != 0`.
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

# The i64 half. Comparisons yield an i32; the rest yield an i64.
for _i, _name in enumerate(
    "eqz eq ne lt_s lt_u gt_s gt_u le_s le_u ge_s ge_u".split()
):
    WASM_OPS[f"i64_{_name}"] = bytes([0x50 + _i])
for _i, _name in enumerate(
    "clz ctz popcnt add sub mul div_s div_u rem_s rem_u and or xor shl"
    " shr_s shr_u rotl rotr".split()
):
    WASM_OPS[f"i64_{_name}"] = bytes([0x79 + _i])
WASM_OPS["i32_wrap_i64"] = b"\xa7"
WASM_OPS["i64_extend_i32_s"] = b"\xac"
WASM_OPS["i64_extend_i32_u"] = b"\xad"
WASM_OPS["i64_extend8_s"] = b"\xc2"
WASM_OPS["i64_extend16_s"] = b"\xc3"
WASM_OPS["i64_extend32_s"] = b"\xc4"
WASM_OPS["i64_load"] = b"\x29" + _memarg(3)
WASM_OPS["i64_store"] = b"\x37" + _memarg(3)
# The narrow i64 accesses. lib/wasm.moss declares them, so leaving them
# out here made a declared `Wasm` item uncompilable; the self-hosted
# table in mossc/gensrc.py has them and the two are checked against
# each other.
WASM_OPS["i64_load8_s"] = b"\x30" + _memarg(0)
WASM_OPS["i64_load8_u"] = b"\x31" + _memarg(0)
WASM_OPS["i64_load16_s"] = b"\x32" + _memarg(1)
WASM_OPS["i64_load16_u"] = b"\x33" + _memarg(1)
WASM_OPS["i64_load32_s"] = b"\x34" + _memarg(2)
WASM_OPS["i64_load32_u"] = b"\x35" + _memarg(2)
WASM_OPS["i64_store8"] = b"\x3c" + _memarg(0)
WASM_OPS["i64_store16"] = b"\x3d" + _memarg(1)
WASM_OPS["i64_store32"] = b"\x3e" + _memarg(2)

# Which of them leave an i64 on the stack.
I64_RESULTS = {
    name
    for name in WASM_OPS
    if name.startswith("i64_")
    and not name.startswith(("i64_eq", "i64_ne", "i64_lt", "i64_gt", "i64_le", "i64_ge"))
    and not name.startswith("i64_store")
}


def freeze_env(env: dict) -> tuple:
    """A hashable form of a binding environment, for keying specializations."""
    return tuple(
        sorted((id(k), id(g), freeze_env(sub)) for k, (g, sub) in env.items())
    )


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
            for short in ("access", "bool", "num", "char", "int", "std", "string",
                          "strlist", "cell", "list", "wasm", "wasip1", "path",
                          "wasistd"):
                if module.path.endswith(f"lib/{short}.moss"):
                    self.lib[short] = module
        self.imports: list[tuple[str, int, int]] = []  # (field, params, results)
        self.import_index: dict[str, int] = {}
        for field, nparams, nresults in BASE_IMPORTS:
            self.wasi_import(field, nparams, nresults)
        self.rebound = self.rebound_keys()

    def wasi_import(self, field: str, params, results) -> int:
        """The index of a WASI import, adding it to the module if new.
        `params` and `results` are counts of i32s, or explicit type lists."""
        if isinstance(params, int):
            params = (I32,) * params
        if isinstance(results, int):
            results = (I32,) * results
        if field not in self.import_index:
            self.import_index[field] = len(self.imports)
            self.imports.append((field, tuple(params), tuple(results)))
        return self.import_index[field]

    def valtype(self, ty_ast) -> bytes:
        """The Wasm value type a `Wasm` type annotation names."""
        if "wasm" in self.lib and getattr(ty_ast, "path", None):
            target = self.lib["wasm"].names.get(ty_ast.path[-1])
            if target is not None and target is self.lib["wasm"].names.get("I64"):
                return I64
        return I32

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

    def rebound_keys(self) -> set:
        """Val keys the program binds somewhere. A val with a native
        provision is normally inlined as its constant, but a `bind` may
        replace it — a functor providing `Std` over `Wasi` does exactly
        that to `zero` and `one` — and then it has to travel as data like
        any other val."""
        found: set = set()

        def walk(node):
            if isinstance(node, ir.BindVal):
                found.add(id(node.key))
            if isinstance(node, (list, tuple)):
                for item in node:
                    walk(item)
            elif hasattr(node, "__dataclass_fields__") and type(node).__module__ == ir.__name__:
                # IR nodes only: a need key is a Symbol, and following one
                # of those leads into the whole module graph.
                for name in node.__dataclass_fields__:
                    walk(getattr(node, name))

        for fn in self.lower.fns.values():
            walk(fn.body)
        return found

    def passed_need(self, key) -> bool:
        """Whether a need key is passed as a parameter: vals, unless they
        have a native provision that nothing rebinds. Everything else
        compiles at its use site (constants inline, native methods as
        instructions, putchar as the shim) — or fails there if this slice
        doesn't cover it. Leaving unrebound natives out matters because
        `main` assumes all of Std, and most of Std is char constants."""
        if isinstance(key, tuple) or key.kind != SymKind.VAL:
            return False
        return key not in self.natives or id(key) in self.rebound

    def val_slots(self, symbol: Symbol, env: dict) -> list:
        """The val keys this specialization takes as parameters: its own
        (D2 — vals are the only runtime context), plus, for every fn need
        bound to a provider, whatever that provider needs in turn. A bound
        provider is called from wherever the need is used, which may be
        several frames below the bind, so its values are threaded down."""
        out: list = []
        seen: set = set()

        def visit(sym: Symbol, e: dict, depth: int):
            if depth > 32:
                raise NotCompilable("fn binds nested too deeply")
            for key in self.lower.needs_of[id(sym)]:
                if self.passed_need(key):
                    if id(key) not in seen:
                        seen.add(id(key))
                        out.append(key)
                elif key in e:
                    provider, sub = e[key]
                    visit(provider, sub, depth + 1)

        visit(symbol, env, 0)
        return out

    def compile_fn(self, symbol: Symbol, env: dict | None = None) -> int:
        """Compile a *specialization*: the function under a specific set of
        fn bindings. Two call sites that bind different providers get two
        function bodies, each with its calls resolved to direct ones — D2's
        "statically a direct call, with the captured context passed as
        data", which is what makes fn binds compilable without a table."""
        env = env or {}
        cache = (id(symbol), freeze_env(env))
        if cache in self.fn_index:
            return self.fn_index[cache]
        index = self.first_fn() + len(self.fn_index)
        self.fn_index[cache] = index
        fn = self.lower.fns[id(symbol)]
        compiler = FnCompiler(self, fn, self.val_slots(symbol, env), env)
        body = compiler.run()  # may recursively compile callees
        self.compiled[index] = (compiler.param_count, fn.ret_slots, body)
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

        def ty(params, results) -> int:
            if isinstance(params, int):
                params = (I32,) * params
            if isinstance(results, int):
                results = (I32,) * results
            key = (tuple(params), tuple(results))
            if key not in type_of:
                type_of[key] = len(types)
                types.append(key)
            return type_of[key]

        import_types = [ty(params, results) for _, params, results in self.imports]
        shim_types = [
            ty(1, 0),  # putchar: (char) -> ()
            ty(1, 1),  # alloc: (nbytes) -> addr
            ty(1, 1),  # arg_at: (index) -> string
            ty(0, 1),  # arg_count: () -> count
            ty(2, 1),  # list_push: (handle, value) -> dummy
            ty(1, 0),  # print: (string) -> ()
            ty(3, 1),  # slice: (string, start, len) -> string
            ty(2, 1),  # concat: (string, string) -> string
            ty(2, 1),  # join: (path, name) -> path
            ty(1, 1),  # read: (path) -> string
        ]
        func_types = [ty(count, results) for count, results, _ in ordered]
        start_type = ty(0, 0)

        type_section = section(
            1, vec([b"\x60" + vec(list(p)) + vec(list(r)) for p, r in types])
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
                self.arg_at_shim(),
                self.arg_count_shim(),
                self.list_push_shim(),
                self.print_shim(),
                self.slice_shim(),
                self.concat_shim(),
                self.join_shim(),
                self.read_shim(),
            ]
            + [b for _, _, b in ordered]
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
        # (nbytes) -> addr: bump the heap pointer stored at address 16. The
        # bump is rounded up to a multiple of 4, so that every block starts
        # word-aligned however many odd-sized strings precede it — i32
        # accesses declare alignment 2 and trap otherwise.
        body = bytearray()
        body += vec([uleb(1) + I32])  # one scratch local
        body += I32_CONST + sleb(16) + I32_LOAD + uleb(2) + uleb(0)
        body += LOCAL_SET + uleb(1)
        body += I32_CONST + sleb(16)
        body += LOCAL_GET + uleb(1) + LOCAL_GET + uleb(0) + BINOPS["add"]
        body += I32_CONST + sleb(3) + BINOPS["add"]
        body += I32_CONST + sleb(-4) + BINOPS["and"]
        body += I32_STORE + uleb(2) + uleb(0)
        # Grow if the new top is past the end of memory. Two pages is the
        # starting size, and nothing else grows it, so without this a
        # program that allocates enough simply writes off the end.
        body += BLOCK + EMPTY
        body += I32_CONST + sleb(16) + I32_LOAD + uleb(2) + uleb(0)
        body += b"\x3f\x00"  # memory.size, in pages
        body += I32_CONST + sleb(16) + BINOPS["shl"]
        body += BINOPS["lt"] + BR_IF + uleb(0)
        # grow by (needed - size) pages, rounded up, plus one for slack
        body += I32_CONST + sleb(16) + I32_LOAD + uleb(2) + uleb(0)
        body += I32_CONST + sleb(16) + BINOPS["shr"]
        body += b"\x3f\x00"
        body += BINOPS["sub"] + I32_CONST + sleb(1) + BINOPS["add"]
        body += b"\x40\x00" + DROP  # memory.grow
        body += END
        body += LOCAL_GET + uleb(1)
        body += END
        return bytes(body)

    def arg_count_shim(self) -> bytes:
        # () -> argc.
        b = bytearray()
        b += vec([])
        b += I32_CONST + sleb(24) + I32_CONST + sleb(28)
        b += CALL + uleb(ARGS_SIZES_GET) + DROP
        b += I32_CONST + sleb(24) + I32_LOAD + uleb(2) + uleb(0)
        b += END
        return bytes(b)

    def arg_at_shim(self) -> bytes:
        # (i) -> string ptr: argv[i] via WASI, boxed as [len|bytes]. Out of
        # range gives the empty string, matching the interpreter.
        # Param 0 i; locals: 1 argv, 2 buf, 3 p, 4 n, 5 s, 6 j.
        b = bytearray()
        b += vec([uleb(6) + I32])
        b += I32_CONST + sleb(24) + I32_CONST + sleb(28)
        b += CALL + uleb(ARGS_SIZES_GET) + DROP
        # argv = alloc(argc * 4); buf = alloc(bufsize); args_get(argv, buf)
        b += I32_CONST + sleb(24) + I32_LOAD + uleb(2) + uleb(0)
        b += I32_CONST + sleb(4) + BINOPS["mul"]
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(1)
        b += I32_CONST + sleb(28) + I32_LOAD + uleb(2) + uleb(0)
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(2)
        b += LOCAL_GET + uleb(1) + LOCAL_GET + uleb(2) + CALL + uleb(ARGS_GET) + DROP
        # if i >= argc (unsigned, so a negative i is out of range too):
        # return the empty string
        b += LOCAL_GET + uleb(0)
        b += I32_CONST + sleb(24) + I32_LOAD + uleb(2) + uleb(0)
        b += b"\x4f"  # i32.ge_u
        b += IF + EMPTY
        b += I32_CONST + sleb(4) + CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(5)
        b += LOCAL_GET + uleb(5) + I32_CONST + sleb(0) + I32_STORE + uleb(2) + uleb(0)
        b += LOCAL_GET + uleb(5) + RETURN
        b += END
        # p = argv[i] (NUL-terminated); n = strlen(p)
        b += LOCAL_GET + uleb(1)
        b += LOCAL_GET + uleb(0) + I32_CONST + sleb(4) + BINOPS["mul"] + BINOPS["add"]
        b += I32_LOAD + uleb(2) + uleb(0) + LOCAL_SET + uleb(3)
        b += I32_CONST + sleb(0) + LOCAL_SET + uleb(4)
        b += BLOCK + EMPTY + LOOP + EMPTY
        b += LOCAL_GET + uleb(3) + LOCAL_GET + uleb(4) + BINOPS["add"]
        b += I32_LOAD8 + uleb(0) + uleb(0) + I32_EQZ + BR_IF + uleb(1)
        b += LOCAL_GET + uleb(4) + I32_CONST + sleb(1) + BINOPS["add"] + LOCAL_SET + uleb(4)
        b += BR + uleb(0) + END + END
        # s = alloc(4 + n); *s = n; copy bytes
        b += I32_CONST + sleb(4) + LOCAL_GET + uleb(4) + BINOPS["add"]
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(5)
        b += LOCAL_GET + uleb(5) + LOCAL_GET + uleb(4) + I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(0) + LOCAL_SET + uleb(6)
        b += BLOCK + EMPTY + LOOP + EMPTY
        b += LOCAL_GET + uleb(6) + LOCAL_GET + uleb(4) + BINOPS["ge"] + BR_IF + uleb(1)
        b += LOCAL_GET + uleb(5) + LOCAL_GET + uleb(6) + BINOPS["add"]
        b += LOCAL_GET + uleb(3) + LOCAL_GET + uleb(6) + BINOPS["add"]
        b += I32_LOAD8 + uleb(0) + uleb(0)
        b += I32_STORE8 + uleb(0) + uleb(4)
        b += LOCAL_GET + uleb(6) + I32_CONST + sleb(1) + BINOPS["add"] + LOCAL_SET + uleb(6)
        b += BR + uleb(0) + END + END
        b += LOCAL_GET + uleb(5)
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

    def join_shim(self) -> bytes:
        # (path, name) -> path. A path is a [len|bytes] block like a String,
        # relative to the preopen, so `pwd` is empty and joining onto it is
        # just the name. Locals: 2 s, 3 i, 4 la.
        b = bytearray()
        b += vec([uleb(3) + I32])
        b += LOCAL_GET + uleb(0) + I32_LOAD + uleb(2) + uleb(0) + LOCAL_SET + uleb(4)
        b += LOCAL_GET + uleb(4) + I32_EQZ
        b += IF + EMPTY
        b += LOCAL_GET + uleb(1) + RETURN
        b += END
        # s = alloc(4 + la + 1 + lb); *s = la + 1 + lb
        b += I32_CONST + sleb(5) + LOCAL_GET + uleb(4) + BINOPS["add"]
        b += LOCAL_GET + uleb(1) + I32_LOAD + uleb(2) + uleb(0) + BINOPS["add"]
        b += CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(2)
        b += LOCAL_GET + uleb(2)
        b += LOCAL_GET + uleb(4) + I32_CONST + sleb(1) + BINOPS["add"]
        b += LOCAL_GET + uleb(1) + I32_LOAD + uleb(2) + uleb(0) + BINOPS["add"]
        b += I32_STORE + uleb(2) + uleb(0)
        # copy the directory, then '/', then the name
        for source, offset in ((0, 0), (1, 1)):
            b += I32_CONST + sleb(0) + LOCAL_SET + uleb(3)
            b += BLOCK + EMPTY + LOOP + EMPTY
            b += LOCAL_GET + uleb(3)
            b += LOCAL_GET + uleb(source) + I32_LOAD + uleb(2) + uleb(0)
            b += BINOPS["ge"] + BR_IF + uleb(1)
            b += LOCAL_GET + uleb(2) + LOCAL_GET + uleb(3) + BINOPS["add"]
            if offset:
                b += LOCAL_GET + uleb(4) + I32_CONST + sleb(1) + BINOPS["add"]
                b += BINOPS["add"]
            b += LOCAL_GET + uleb(source) + LOCAL_GET + uleb(3) + BINOPS["add"]
            b += I32_LOAD8 + uleb(0) + uleb(4)
            b += I32_STORE8 + uleb(0) + uleb(4)
            b += LOCAL_GET + uleb(3) + I32_CONST + sleb(1) + BINOPS["add"]
            b += LOCAL_SET + uleb(3)
            b += BR + uleb(0) + END + END
        b += LOCAL_GET + uleb(2) + LOCAL_GET + uleb(4) + BINOPS["add"]
        b += I32_CONST + sleb(0x2F) + I32_STORE8 + uleb(0) + uleb(4)  # '/'
        b += LOCAL_GET + uleb(2)
        b += END
        return bytes(b)

    def read_shim(self) -> bytes:
        # (path) -> string: path_open against the preopen, then fd_read in
        # chunks straight onto the top of the heap, which the bump
        # allocator leaves contiguous — so the string is built in place
        # with no copying. Traps on a path that will not open.
        # Locals: 1 fd, 2 s, 3 total, 4 n.
        opened, read, close = PATH_OPEN, FD_READ, FD_CLOSE
        b = bytearray()
        b += vec([uleb(4) + I32])
        # path_open(preopen, 0, path+4, len, 0, RIGHT_FD_READ|SEEK, 0, 0, 32)
        b += I32_CONST + sleb(PREOPEN_FD) + I32_CONST + sleb(0)
        b += LOCAL_GET + uleb(0) + I32_CONST + sleb(4) + BINOPS["add"]
        b += LOCAL_GET + uleb(0) + I32_LOAD + uleb(2) + uleb(0)
        b += I32_CONST + sleb(0)
        b += b"\x42" + sleb(RIGHT_FD_READ | RIGHT_FD_SEEK)  # i64.const rights
        b += b"\x42" + sleb(0)  # i64.const inheriting
        b += I32_CONST + sleb(0) + I32_CONST + sleb(32)
        b += CALL + uleb(opened)
        b += IF + EMPTY + UNREACHABLE + END  # a path that will not open traps
        b += I32_CONST + sleb(32) + I32_LOAD + uleb(2) + uleb(0) + LOCAL_SET + uleb(1)
        # s = heap top; the bytes go straight after its length word
        b += I32_CONST + sleb(4) + CALL + uleb(self.shim(S_ALLOC)) + LOCAL_SET + uleb(2)
        b += I32_CONST + sleb(0) + LOCAL_SET + uleb(3)
        b += BLOCK + EMPTY + LOOP + EMPTY
        # reserve a chunk, then read into it
        b += I32_CONST + sleb(4096) + CALL + uleb(self.shim(S_ALLOC))
        b += I32_CONST + sleb(0) + I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(0)
        b += LOCAL_GET + uleb(2) + I32_CONST + sleb(4) + BINOPS["add"]
        b += LOCAL_GET + uleb(3) + BINOPS["add"]
        b += I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(4) + I32_CONST + sleb(4096) + I32_STORE + uleb(2) + uleb(0)
        b += LOCAL_GET + uleb(1) + I32_CONST + sleb(0) + I32_CONST + sleb(1)
        b += I32_CONST + sleb(12) + CALL + uleb(read) + DROP
        b += I32_CONST + sleb(12) + I32_LOAD + uleb(2) + uleb(0) + LOCAL_SET + uleb(4)
        b += LOCAL_GET + uleb(4) + I32_EQZ + BR_IF + uleb(1)
        b += LOCAL_GET + uleb(3) + LOCAL_GET + uleb(4) + BINOPS["add"]
        b += LOCAL_SET + uleb(3)
        b += BR + uleb(0) + END + END
        # length word, and hand back the bytes actually read
        b += LOCAL_GET + uleb(2) + LOCAL_GET + uleb(3) + I32_STORE + uleb(2) + uleb(0)
        b += I32_CONST + sleb(16)
        b += LOCAL_GET + uleb(2) + I32_CONST + sleb(4) + BINOPS["add"]
        b += LOCAL_GET + uleb(3) + BINOPS["add"]
        b += I32_CONST + sleb(3) + BINOPS["add"] + I32_CONST + sleb(-4) + BINOPS["and"]
        b += I32_STORE + uleb(2) + uleb(0)
        b += LOCAL_GET + uleb(1) + CALL + uleb(close) + DROP
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
    def __init__(self, backend: Backend, fn: ir.FnIR, needs: list, env: dict):
        self.b = backend
        self.fn = fn
        self.env = dict(env)  # fn need key -> (provider symbol, its own env)
        self.code = bytearray()
        # D59: a value is a run of scalars, so a name owns a *list* of
        # local indices rather than one.
        self.slots: dict = {}  # name -> [local index, ...]
        self.provisions: dict = {}  # key -> ('local', idx) | ('const', n)
        index = 0
        widths = list(fn.param_slots) or [1] * (len(fn.params) + (1 if fn.has_this else 0))
        names = (["this"] if fn.has_this else []) + list(fn.params)
        for name, width in zip(names, widths):
            self.slots[name] = list(range(index, index + width))
            index += width
        self.param_types = [I32] * index
        for key in needs:
            self.provisions[key] = ("local", index)
            self.param_types.append(I32)
            index += 1
        self.param_count = index
        self.local_types: list = []
        self.break_depths: list[int] = []
        self.depth = 0

    def local(self, valtype: bytes = I32) -> int:
        idx = self.param_count + len(self.local_types)
        self.local_types.append(valtype)
        return idx

    def local_type_of(self, index: int) -> bytes:
        if index < self.param_count:
            return self.param_types[index]
        return self.local_types[index - self.param_count]

    def group(self, layout) -> list:
        return [self.local(valtype) for valtype in layout]

    def store(self, indices: list):
        """Pop a value off the stack into its slots, last slot first."""
        for idx in reversed(indices):
            self.code += LOCAL_SET + uleb(idx)

    def load(self, indices: list):
        for idx in indices:
            self.code += LOCAL_GET + uleb(idx)

    def run(self) -> bytes:
        self.block(self.fn.body)
        self.code += END
        # Locals are declared in runs of one type (D59: a value is a
        # sequence of scalars, and not every scalar is an i32).
        runs = []
        for valtype in self.local_types:
            if runs and runs[-1][1] == valtype:
                runs[-1][0] += 1
            else:
                runs.append([1, valtype])
        decls = vec([uleb(n) + valtype for n, valtype in runs])
        return bytes(decls) + bytes(self.code)

    # Provisions: how a need key is satisfied *here*.

    def provide(self, key):
        if key in self.provisions:
            return self.provisions[key]
        provision = self.b.natives.get(key)
        const = self.b.native_const(provision)
        if const is not None:
            return ("const", const)
        if "path" in self.b.lib and key is self.b.lib["path"].names.get("pwd"):
            return ("empty_string", 0)
        raise NotCompilable(
            f"need `{getattr(key, 'name', key)}` has no compilable provision"
        )

    def push_provision(self, key):
        kind, value = self.provide(key)
        if kind == "const":
            self.code += I32_CONST + sleb(value)
        elif kind == "empty_string":
            # `pwd`: paths are relative to the preopen, so it is empty.
            tmp = self.local()
            self.code += I32_CONST + sleb(4)
            self.code += CALL + uleb(self.b.shim(S_ALLOC)) + LOCAL_SET + uleb(tmp)
            self.code += LOCAL_GET + uleb(tmp) + I32_CONST + sleb(0)
            self.code += I32_STORE + uleb(2) + uleb(0)
            self.code += LOCAL_GET + uleb(tmp)
        else:
            self.code += LOCAL_GET + uleb(value)

    # Statements

    def block(self, node: ir.Block):
        saved = dict(self.provisions)
        saved_env = dict(self.env)
        for stmt in node.stmts:
            self.stmt(stmt)
        if node.tail is None:
            self.code += I32_CONST + sleb(0)
            layout = (I32,)
        else:
            layout = self.expr(node.tail)
        self.provisions = saved
        self.env = saved_env
        return layout

    def stmt(self, node):
        if isinstance(node, (ir.Let, ir.Assign)):
            layout = self.expr(node.expr)
            if node.name not in self.slots:
                self.slots[node.name] = self.group(layout)
            self.store(self.slots[node.name])
        elif isinstance(node, ir.BindVal):
            layout = self.expr(node.expr)
            if len(layout) != 1:
                raise NotCompilable("binding a val of more than one scalar")
            idx = self.local(layout[0])
            self.code += LOCAL_SET + uleb(idx)
            self.provisions[node.key] = ("local", idx)
        elif isinstance(node, ir.BindFn):
            # Record which function this key now names, and alias the
            # provider's own needs to what they were bound to *here* — the
            # capture happens at the bind site, so the values are the ones
            # in scope now, whatever frame ends up making the call.
            sub: dict = {}
            for callee_key, caller_key in node.needs_map:
                if caller_key in self.env:
                    sub[callee_key] = self.env[caller_key]
                elif self.b.passed_need(callee_key):
                    self.provisions[callee_key] = self.provide(caller_key)
            self.env[node.key] = (node.fn, sub)
        elif isinstance(node, ir.ExprStmt):
            layout = self.expr(node.expr)
            self.code += DROP * len(layout)
        elif isinstance(node, ir.While):
            self.code += BLOCK + EMPTY
            self.depth += 1
            self.break_depths.append(self.depth)
            self.code += LOOP + EMPTY
            self.depth += 1
            self.expr(node.cond)
            self.code += I32_EQZ + BR_IF + uleb(1)
            self.code += DROP * len(self.block(node.body))
            self.code += BR + uleb(0) + END + END
            self.depth -= 2
            self.break_depths.pop()
        elif isinstance(node, ir.Loop):
            self.code += BLOCK + EMPTY
            self.depth += 1
            self.break_depths.append(self.depth)
            self.code += LOOP + EMPTY
            self.depth += 1
            self.code += DROP * len(self.block(node.body))
            self.code += BR + uleb(0) + END + END
            self.depth -= 2
            self.break_depths.pop()
        elif isinstance(node, ir.Block):
            self.block(node)
            self.code += DROP
        else:
            raise NotCompilable(type(node).__name__)

    # Expressions: every expression leaves exactly one i32.

    def expr(self, node) -> tuple:
        """Compiles `node` and reports its *layout*: the valtypes it left
        on the stack, one per scalar the value occupies (D59)."""
        got = self.expr_inner(node)
        if got is None:
            return (I32,)
        return got if isinstance(got, tuple) else (got,)

    def expr_inner(self, node):
        if isinstance(node, ir.Unit):
            self.code += I32_CONST + sleb(0)
        elif isinstance(node, ir.Local):
            indices = self.slots[node.name]
            self.load(indices)
            return tuple(self.local_type_of(i) for i in indices)
        elif isinstance(node, ir.This):
            indices = self.slots["this"]
            self.load(indices)
            return tuple(self.local_type_of(i) for i in indices)
        elif isinstance(node, ir.NeedVal):
            self.push_provision(node.key)
        elif isinstance(node, ir.MakeUnit):
            self.code += I32_CONST + sleb(self.b.unit_code(node.symbol))
        elif isinstance(node, ir.MakeTag):
            # D58: a nominal value *is* its payload. Nothing to attach.
            self.expr(node.payload)
        elif isinstance(node, ir.Inject):
            return self.inject(node)
        elif isinstance(node, ir.MakeRecord):
            # D59: the fields *are* the value, one scalar each.
            layout = []
            for _, expr in node.fields:
                field = self.expr(expr)
                if len(field) != 1:
                    raise NotCompilable("a record field of more than one scalar")
                layout.extend(field)
            return tuple(layout)
        elif isinstance(node, ir.Field):
            if node.index < 0:
                raise NotCompilable("field access without layout")
            layout = self.expr(node.obj)
            staged = self.group(layout)
            self.store(staged)
            self.code += LOCAL_GET + uleb(staged[node.index])
            return (layout[node.index],)
        elif isinstance(node, ir.Call):
            return self.call(node)
        elif isinstance(node, ir.If):
            return self.if_expr(node)
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
            return self.match(node)
        else:
            raise NotCompilable(type(node).__name__)

    def match(self, node: ir.Match):
        layout = self.probe_expr(node.scrutinee)
        scrut = self.group(layout)
        self.expr(node.scrutinee)
        self.store(scrut)
        bool_sym = self.b.lib["bool"].names.get("Bool") if "bool" in self.b.lib else None
        arms = list(node.arms)
        result = [None]

        def bind_and_body(arm, bare=False):
            pat = arm.pat
            if pat.head is None:
                if pat.binder is not None:
                    self.slots.setdefault(pat.binder, list(scrut))
            elif bare and pat.fields is None:
                # Untagged: the scrutinee is the payload.
                if pat.binder is not None:
                    self.slots.setdefault(pat.binder, list(scrut))
            else:
                if pat.binder is not None:
                    # Injected: the payload sits beside the discriminant.
                    self.slots.setdefault(pat.binder, list(scrut[1:]))
                if pat.fields is not None:
                    for _, binder, index in pat.fields:
                        self.slots.setdefault(binder, [scrut[index]])
            if isinstance(arm.body, ir.Block):
                got = self.block(arm.body)
            else:
                got = self.expr(arm.body)
            result[0] = got
            return got

        def chain(i: int):
            if i >= len(arms):
                self.code += UNREACHABLE
                return
            arm = arms[i]
            pat = arm.pat
            if not node.tagged:
                # One possible head, so the first arm always matches.
                bind_and_body(arm, bare=True)
                if staged:
                    self.store(staged)
                return
            if pat.head is None:
                bind_and_body(arm)
                if staged:
                    self.store(staged)
                return
            if pat.head is bool_sym:
                raise NotCompilable("matching through Bool (use if)")
            code = self.b.unit_code(pat.head)
            # Slot zero is the discriminant, whatever the member is.
            self.code += LOCAL_GET + uleb(scrut[0])
            self.code += I32_CONST + sleb(code) + BINOPS["eq"]
            self.code += IF + arm_type
            self.depth += 1
            bind_and_body(arm)
            if staged:
                self.store(staged)
            self.code += ELSE
            chain(i + 1)
            self.code += END
            self.depth -= 1

        arm_layout = (
            self.probe_arm(bind_and_body, arms[0], not node.tagged) if arms else (I32,)
        )
        staged = self.group(arm_layout) if len(arm_layout) != 1 else None
        arm_type = EMPTY if staged else arm_layout[0]
        chain(0)
        if staged:
            self.load(staged)
        return arm_layout

    def inject(self, node: ir.Inject):
        """A value entering a union: a discriminant scalar beside the
        payload, with no allocation at all (D59). A union whose members are
        all units needs no discriminant — the code is the value."""
        if node.width == 1:
            return self.expr(node.value)
        self.code += I32_CONST + sleb(self.b.unit_code(node.symbol))
        payload = self.expr(node.value)
        if len(payload) + 1 > node.width:
            raise NotCompilable("a union member wider than its union")
        for _ in range(node.width - 1 - len(payload)):
            self.code += I32_CONST + sleb(0)  # pad to the union's width
        return (I32,) + payload + (I32,) * (node.width - 1 - len(payload))

    def if_expr(self, node: ir.If):
        """A block type names at most one result, so a branch yielding
        several scalars is staged through locals instead (D59)."""
        def els():
            if node.els is None:
                self.code += I32_CONST + sleb(0)
                return (I32,)
            if isinstance(node.els, ir.Block):
                return self.block(node.els)
            return self.expr(node.els)

        probe = self.probe(node.then)
        if len(probe) == 1:
            self.expr(node.cond)
            self.code += IF + probe[0]
            self.depth += 1
            self.block(node.then)
            self.code += ELSE
            els()
            self.code += END
            self.depth -= 1
            return probe
        staged = self.group(probe)
        self.expr(node.cond)
        self.code += IF + EMPTY
        self.depth += 1
        self.block(node.then)
        self.store(staged)
        self.code += ELSE
        els()
        self.store(staged)
        self.code += END
        self.depth -= 1
        self.load(staged)
        return probe

    def probe_expr(self, node):
        return self.probing(lambda: self.expr(node))

    def probe_arm(self, bind_and_body, arm, bare):
        return self.probing(lambda: bind_and_body(arm, bare))

    def probe(self, block: ir.Block):
        return self.probing(lambda: self.block(block))

    def probing(self, emit):
        """The layout a block yields, found by compiling it to a scratch
        buffer and throwing the code away. Cheap, and it keeps layout out
        of the IR for constructs whose type the lowering does not record."""
        saved_code, saved_locals = self.code, list(self.local_types)
        saved_slots, saved_prov = dict(self.slots), dict(self.provisions)
        saved_env, saved_depth = dict(self.env), self.depth
        self.code = bytearray()
        try:
            return emit()
        finally:
            self.code = saved_code
            self.local_types = saved_locals
            self.slots, self.provisions = saved_slots, saved_prov
            self.env, self.depth = saved_env, saved_depth

    def call(self, node: ir.Call):
        kind, target = node.callee
        if kind == "direct":
            if node.this is not None:
                self.expr(node.this)
            for arg in node.args:
                self.expr(arg)
            callee_env = {}
            translate = {}
            for callee_key, caller_key in node.needs_map:
                translate[callee_key] = caller_key
                if caller_key in self.env:
                    callee_env[callee_key] = self.env[caller_key]
            # The callee's parameters are its own val needs (named on its
            # side, so translated) followed by whatever its bound providers
            # capture (already named on ours).
            for slot in self.b.val_slots(target, callee_env):
                self.push_provision(translate.get(slot, slot))
            self.code += CALL + uleb(self.b.compile_fn(target, callee_env))
            return (I32,) * self.b.lower.fns[id(target)].ret_slots
        if target in self.env:
            provider, sub = self.env[target]
            if node.this is not None:
                self.expr(node.this)
            for arg in node.args:
                self.expr(arg)
            for slot in self.b.val_slots(provider, sub):
                self.push_provision(slot)
            self.code += CALL + uleb(self.b.compile_fn(provider, sub))
            return (I32,) * self.b.lower.fns[id(provider)].ret_slots
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
            # Which native container a shim is for: normally the module
            # that declares the method, but `.get`/`.length`/`.push`/`.read`
            # are declared once in access.moss and provided at several
            # receivers (D61), so there the receiver's home module says.
            home = method.module
            if "access" in self.b.lib and home is self.b.lib["access"]:
                home = receiver.module
            if "num" in self.b.lib and home is self.b.lib["num"]:
                if short == "neg":
                    self.code += I32_CONST + sleb(0)
                    self.expr(node.this)
                    self.code += BINOPS["sub"]
                    return
                if short == "not":
                    self.expr(node.this)
                    self.code += I32_CONST + sleb(1) + BINOPS["xor"]
                    return
            if "char" in self.b.lib and home is self.b.lib["char"]:
                if short in ("code", "char"):
                    self.expr(node.this)  # chars are already their codepoints
                    return
            if "string" in self.b.lib and home is self.b.lib["string"]:
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
            if "list" in self.b.lib and home is self.b.lib["list"]:
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
            if "strlist" in self.b.lib and home is self.b.lib["strlist"]:
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
            if "path" in self.b.lib and home is self.b.lib["path"]:
                if short == "join":
                    self.expr(node.this)
                    self.expr(node.args[0])
                    self.code += CALL + uleb(self.b.shim(S_JOIN))
                    return
                if short == "read":
                    self.expr(node.this)
                    self.code += CALL + uleb(self.b.shim(S_READ))
                    return
            if "cell" in self.b.lib and home is self.b.lib["cell"]:
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
        if "string" in self.b.lib:
            string = self.b.lib["string"]
            if key is string.names.get("first_arg"):
                self.code += I32_CONST + sleb(1)  # first_arg is arg_at(1)
                self.code += CALL + uleb(self.b.shim(S_ARG_AT))
                return
            if key is string.names.get("arg_at"):
                self.expr(node.args[0])
                self.code += CALL + uleb(self.b.shim(S_ARG_AT))
                return
            if key is string.names.get("arg_count"):
                self.code += CALL + uleb(self.b.shim(S_ARG_COUNT))
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
        if "wasistd" in self.b.lib and module is self.b.lib["wasistd"]:
            if key.name == "i32_bool":
                # Bool is 0/1 with its tag erased, so this is `n != 0`.
                self.expr(node.args[0])
                self.code += I32_CONST + sleb(0) + BINOPS_NE
                return
        if "wasm" in self.b.lib and module is self.b.lib["wasm"]:
            return self.wasm_instruction(key, node)
        if "wasip1" in self.b.lib and module is self.b.lib["wasip1"]:
            # D52: a WASI function is exactly an import of this module.
            for arg in node.args:
                self.expr(arg)
            results = () if key.decl.ret is None else (self.b.valtype(key.decl.ret),)
            params = tuple(self.b.valtype(p.ty) for p in key.decl.params)
            index = self.b.wasi_import(key.name, params, results)
            self.code += CALL + uleb(index)
            if not results:
                self.code += I32_CONST + sleb(0)  # the Moss call yields ()
            return
        raise NotCompilable(f"contextual fn `{getattr(key, 'name', key)}`")

    def wasm_instruction(self, key: Symbol, node: ir.Call):
        """A `Wasm` intrinsic is the instruction of the same name."""
        op = WASM_OPS.get(key.name)
        if op is None:
            raise NotCompilable(f"`{key.name}` is not an instruction")
        for arg in node.args:
            self.expr(arg)
        self.code += op
        if key.decl.ret is None:
            self.code += I32_CONST + sleb(0)  # stores and fills yield ()
            return I32
        return I64 if key.name in I64_RESULTS else I32


def build(program: Program, lower, main: Symbol) -> bytes:
    from .interp import native_env

    natives = native_env(program, [])
    backend = Backend(program, lower, natives)
    return backend.build(main)
