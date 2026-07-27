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
`putchar` is an fd_write shim. Records, non-Bool tags, match, strings,
cells, lists, and fn binds are not in this slice and report themselves as
such.
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
            for short in ("bool", "num", "char", "int", "std"):
                if module.path.endswith(f"lib/{short}.moss"):
                    self.lib[short] = module

    def unit_code(self, symbol: Symbol) -> int:
        if "bool" in self.lib:
            if symbol is self.lib["bool"].names.get("True"):
                return 1
            if symbol is self.lib["bool"].names.get("False"):
                return 0
        return self.unit_codes.setdefault(id(symbol), len(self.unit_codes) + 2)

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
        index = 2 + len(self.fn_index)  # after fd_write import and putchar shim
        self.fn_index[id(symbol)] = index
        fn = self.lower.fns[id(symbol)]
        needs = self.val_needs(symbol)
        compiler = FnCompiler(self, fn, needs)
        body = compiler.run()  # may recursively compile callees
        self.compiled[index] = (compiler.param_count, body)
        return index

    def build(self, main: Symbol) -> bytes:
        main_index = self.compile_fn(main)
        ordered = [self.compiled[i] for i in sorted(self.compiled)]
        # Types: 0 = fd_write, then one per distinct param count, then _start.
        types = [(4, 1)]  # fd_write: 4 params, 1 result
        type_of = {(4, 1): 0}
        func_types = []
        for count, _ in ordered:
            key = (count, 1)
            if key not in type_of:
                type_of[key] = len(types)
                types.append(key)
            func_types.append(type_of[key])
        if (1, 0) not in type_of:
            type_of[(1, 0)] = len(types)
            types.append((1, 0))
        putchar_type = type_of[(1, 0)]
        if (0, 0) not in type_of:
            type_of[(0, 0)] = len(types)
            types.append((0, 0))
        start_type = type_of[(0, 0)]

        type_section = section(
            1,
            vec(
                [
                    b"\x60" + vec([I32] * p) + vec([I32] * r)
                    for p, r in types
                ]
            ),
        )
        import_section = section(
            2,
            vec(
                [
                    name("wasi_snapshot_preview1") + name("fd_write") + b"\x00" + uleb(0)
                ]
            ),
        )
        # Functions: putchar shim, compiled fns, _start.
        function_section = section(
            3, vec([uleb(putchar_type)] + [uleb(t) for t in func_types] + [uleb(start_type)])
        )
        memory_section = section(5, vec([b"\x00" + uleb(1)]))
        start_index = 2 + len(ordered)
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
        start_body = vec([]) + CALL + uleb(main_index) + DROP + END
        bodies = [putchar_body] + [b for _, b in ordered] + [start_body]
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
        body += CALL + uleb(0) + DROP
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
                raise NotCompilable(f"tag `{symbol.name}`")
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
            raise NotCompilable("match")
        else:
            raise NotCompilable(type(node).__name__)

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
            raise NotCompilable(f"method `{method.name}`")
        if self.b.is_putchar(key):
            for arg in node.args:
                self.expr(arg)
            self.code += CALL + uleb(1) + I32_CONST + sleb(0)
            return
        raise NotCompilable(f"contextual fn `{getattr(key, 'name', key)}`")


def build(program: Program, lower, main: Symbol) -> bytes:
    from .interp import native_env

    natives = native_env(program, [])
    backend = Backend(program, lower, natives)
    return backend.build(main)
