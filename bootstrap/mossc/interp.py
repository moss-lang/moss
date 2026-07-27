"""Interpreter over the core IR (stage 5, docs/design/semantics.md §11).

The runtime environment is exactly the explicit context structure lowering
produced: a dict from need keys (Symbol, or (receiver, method) pairs) to
values and callables. `bind` writes entries; calls read the callee's needs
out of the current environment; fn binds capture the environment pieces
their provider needs (closures). Types are gone except as the nominal tags
values carry for `match`.
"""

import sys
from dataclasses import dataclass

from . import ir
from .collect import Program, SymKind, Symbol
from .native import CHARS


@dataclass(frozen=True)
class UnitVal:
    symbol: Symbol | None  # None is ()


@dataclass(frozen=True)
class TagVal:
    symbol: Symbol
    payload: object


@dataclass(frozen=True)
class RecordVal:
    symbol: Symbol
    fields: dict


@dataclass(frozen=True)
class CharVal:
    value: str


@dataclass(frozen=True)
class IntVal:
    value: int


@dataclass(frozen=True)
class StrVal:
    value: str


@dataclass(frozen=True)
class PathVal:
    value: str


@dataclass(eq=False)
class CellVal:
    value: object


@dataclass(eq=False)
class ListVal:
    items: list


@dataclass(frozen=True)
class Closure:
    fn: ir.FnIR
    ctx: dict


@dataclass(frozen=True)
class NativeFn:
    name: str
    call: object  # (args: list, this) -> value


UNIT = UnitVal(None)


class MossPanic(Exception):
    pass


class _Return(Exception):
    def __init__(self, value):
        self.value = value


class _Break(Exception):
    pass


@dataclass(frozen=True)
class _Tail:
    """A tail call, unwound by the trampoline in call_fn. Per D49 this is a
    non-semantic implementation detail: the language does not guarantee
    tail-call elimination, and Moss code must iterate with loops."""

    fn: ir.FnIR
    args: list
    ctx: dict
    this: object


class Frame:
    def __init__(self, locals_: dict, ctx: dict, this):
        self.locals = locals_
        self.ctx = ctx
        self.this = this


class Interp:
    def __init__(self, fns: dict):
        self.fns = fns  # id(Symbol) -> FnIR

    def call_fn(self, fn: ir.FnIR, args: list, ctx: dict, this=None):
        while True:
            frame = Frame(dict(zip(fn.params, args)), ctx, this)
            try:
                result = self.eval_block(fn.body, frame, tail=True)
            except _Return as r:
                result = r.value
            if isinstance(result, _Tail):
                fn, args, ctx, this = result.fn, result.args, result.ctx, result.this
                continue
            return result

    def invoke(self, impl, args: list, this=None):
        if isinstance(impl, NativeFn):
            return impl.call(args, this)
        if isinstance(impl, Closure):
            return self.call_fn(impl.fn, args, impl.ctx, this)
        raise MossPanic(f"not callable: {impl!r}")

    def eval_block(self, block: ir.Block, frame: Frame, tail=False):
        # Binds are lexically scoped to the block: restore ctx on exit. A
        # _Tail escaping the block is safe: it carries its own ctx snapshot.
        saved = frame.ctx
        frame.ctx = dict(saved)
        try:
            for stmt in block.stmts:
                self.exec_stmt(stmt, frame)
            if block.tail is None:
                return UNIT
            return self.eval(block.tail, frame, tail=tail)
        finally:
            frame.ctx = saved

    def exec_stmt(self, stmt, frame: Frame):
        if isinstance(stmt, (ir.Let, ir.Assign)):
            frame.locals[stmt.name] = self.eval(stmt.expr, frame)
        elif isinstance(stmt, ir.BindVal):
            frame.ctx[stmt.key] = self.eval(stmt.expr, frame)
        elif isinstance(stmt, ir.BindFn):
            fn = self.fns[id(stmt.fn)]
            captured = {ck: frame.ctx[sk] for ck, sk in stmt.needs_map}
            frame.ctx[stmt.key] = Closure(fn, captured)
        elif isinstance(stmt, ir.While):
            while self.truthy(self.eval(stmt.cond, frame)):
                try:
                    self.eval_block(stmt.body, frame)
                except _Break:
                    break
        elif isinstance(stmt, ir.Loop):
            while True:
                try:
                    self.eval_block(stmt.body, frame)
                except _Break:
                    break
        elif isinstance(stmt, ir.ExprStmt):
            self.eval(stmt.expr, frame)
        elif isinstance(stmt, ir.Block):
            self.eval_block(stmt, frame)
        else:
            raise MossPanic(f"unsupported statement {stmt!r}")

    def truthy(self, value) -> bool:
        if isinstance(value, TagVal):
            value = value.payload  # Bool is nominal over the units (D45)
        if isinstance(value, UnitVal) and value.symbol is not None:
            return value.symbol.name == "True"
        raise MossPanic(f"not a Bool: {value!r}")

    def eval(self, expr, frame: Frame, tail=False):
        if isinstance(expr, ir.Unit):
            return UNIT
        if isinstance(expr, ir.Local):
            return frame.locals[expr.name]
        if isinstance(expr, ir.This):
            return frame.this
        if isinstance(expr, ir.NeedVal):
            return frame.ctx[expr.key]
        if isinstance(expr, ir.MakeUnit):
            return UnitVal(expr.symbol)
        if isinstance(expr, ir.Inject):
            # The interpreter tags nominal values all the time (TagVal), so
            # entering a union changes nothing here.
            return self.eval(expr.value, frame)
        if isinstance(expr, ir.MakeTag):
            return TagVal(expr.symbol, self.eval(expr.payload, frame))
        if isinstance(expr, ir.MakeRecord):
            return RecordVal(
                expr.symbol, {n: self.eval(e, frame) for n, e in expr.fields}
            )
        if isinstance(expr, ir.MakeTuple):
            return tuple(self.eval(e, frame) for e in expr.items)
        if isinstance(expr, ir.Field):
            obj = self.eval(expr.obj, frame)
            return obj.fields[expr.name]
        if isinstance(expr, ir.Call):
            args = [self.eval(a, frame) for a in expr.args]
            this = self.eval(expr.this, frame) if expr.this is not None else None
            kind, target = expr.callee
            if kind == "direct":
                fn = self.fns[id(target)]
                ctx = {ck: frame.ctx[sk] for ck, sk in expr.needs_map}
                if tail:
                    return _Tail(fn, args, ctx, this)
                return self.call_fn(fn, args, ctx, this)
            impl = frame.ctx[target]
            if tail and isinstance(impl, Closure):
                return _Tail(impl.fn, args, impl.ctx, this)
            return self.invoke(impl, args, this)
        if isinstance(expr, ir.If):
            if self.truthy(self.eval(expr.cond, frame)):
                return self.eval_block(expr.then, frame, tail=tail)
            if expr.els is None:
                return UNIT
            if isinstance(expr.els, ir.Block):
                return self.eval_block(expr.els, frame, tail=tail)
            return self.eval(expr.els, frame, tail=tail)
        if isinstance(expr, ir.Match):
            value = self.eval(expr.scrutinee, frame)
            for arm in expr.arms:
                if self.match_pat(arm.pat, value, frame):
                    if isinstance(arm.body, ir.Block):
                        return self.eval_block(arm.body, frame, tail=tail)
                    return self.eval(arm.body, frame, tail=tail)
            raise MossPanic(f"no match arm for {value!r}")
        if isinstance(expr, ir.Return):
            raise _Return(
                UNIT if expr.expr is None else self.eval(expr.expr, frame, tail=True)
            )
        if isinstance(expr, ir.Break):
            raise _Break()
        if isinstance(expr, ir.Block):
            return self.eval_block(expr, frame, tail=tail)
        raise MossPanic(f"unsupported expression {expr!r}")

    def match_pat(self, pat: ir.Pat, value, frame: Frame) -> bool:
        if pat.head is None:
            if pat.binder is not None:
                frame.locals[pat.binder] = value
            return True
        head = getattr(value, "symbol", None)
        if head is not pat.head:
            return False
        if pat.binder is not None:
            frame.locals[pat.binder] = value.payload
        if pat.fields is not None:
            for field_name, binder, _ in pat.fields:
                frame.locals[binder] = value.fields[field_name]
        return True


def native_env(program: Program, args: list | None = None) -> dict:
    """Runtime provisions for the native Std (D38): keys are the abstract
    symbols (or (receiver, method) pairs) declared in lib/, values their
    Python implementations."""
    env = {}
    args = args or []
    lib = {}
    for module in program.modules.values():
        for name in ("std", "char", "bool", "num", "int", "string", "strlist",
                     "cell", "path", "list", "wasm", "wasip1", "wasistd"):
            if module.path.endswith(f"lib/{name}.moss"):
                lib[name] = module
    if "bool" in lib:
        bool_ty = lib["bool"].names["Bool"]
        true = TagVal(bool_ty, UnitVal(lib["bool"].names["True"]))
        false = TagVal(bool_ty, UnitVal(lib["bool"].names["False"]))

        def boolean(b):
            return true if b else false

    def native(fn):
        return NativeFn(fn.__name__, fn)

    if "bool" in lib and "num" in lib:
        env[(bool_ty, lib["num"].detached["not"])] = native(
            lambda a, this: boolean(this.payload.symbol.name != "True")
        )

    if "std" in lib:

        def putchar(a, this):
            sys.stdout.write(a[0].value)
            return UNIT

        env[lib["std"].names["putchar"]] = native(putchar)
    if "char" in lib:
        char_ty = lib["char"].names["Char"]
        for name, char in CHARS.items():
            env[lib["char"].names[name]] = CharVal(char)
        env[(char_ty, lib["char"].detached["code"])] = native(
            lambda a, this: IntVal(ord(this.value))
        )
        if "int" in lib:
            env[(lib["int"].names["Int"], lib["char"].detached["char"])] = native(
                lambda a, this: CharVal(chr(this.value))
            )
        if "num" in lib:
            det = lib["num"].detached
            char_compare = {
                "eq": lambda x, y: x == y,
                "ne": lambda x, y: x != y,
                "lt": lambda x, y: x < y,
                "gt": lambda x, y: x > y,
                "le": lambda x, y: x <= y,
                "ge": lambda x, y: x >= y,
            }
            for name, op in char_compare.items():
                env[(char_ty, det[name])] = NativeFn(
                    name, lambda a, this, op=op: boolean(op(this.value, a[0].value))
                )
    if "int" in lib:
        int_ty = lib["int"].names["Int"]
        env[lib["int"].names["zero"]] = IntVal(0)
        env[lib["int"].names["one"]] = IntVal(1)
        if "num" in lib:
            det = lib["num"].detached
            env[(int_ty, det["neg"])] = native(lambda a, this: IntVal(-this.value))
            arith = {
                "add": lambda x, y: x + y,
                "sub": lambda x, y: x - y,
                "mul": lambda x, y: x * y,
                "and": lambda x, y: x & y,
                "or": lambda x, y: x | y,
                "xor": lambda x, y: x ^ y,
                "shl": lambda x, y: x << y,
                "shr": lambda x, y: x >> y,
            }
            for name, op in arith.items():
                env[(int_ty, det[name])] = NativeFn(
                    name, lambda a, this, op=op: IntVal(op(this.value, a[0].value))
                )

            def div(a, this):
                if a[0].value == 0:
                    raise MossPanic("division by zero")
                return IntVal(this.value // a[0].value)

            def rem(a, this):
                if a[0].value == 0:
                    raise MossPanic("remainder by zero")
                return IntVal(this.value % a[0].value)

            env[(int_ty, det["div"])] = native(div)
            env[(int_ty, det["rem"])] = native(rem)
            compare = {
                "eq": lambda x, y: x == y,
                "ne": lambda x, y: x != y,
                "lt": lambda x, y: x < y,
                "gt": lambda x, y: x > y,
                "le": lambda x, y: x <= y,
                "ge": lambda x, y: x >= y,
            }
            for name, op in compare.items():
                env[(int_ty, det[name])] = NativeFn(
                    name, lambda a, this, op=op: boolean(op(this.value, a[0].value))
                )
    if "wasm" in lib:
        # D52's primitive layer. Only the two I32 constants are values, and
        # only they are meaningful here: the instructions themselves need a
        # linear memory the interpreter does not have yet, so a program
        # assuming `Wasm` compiles but does not interpret.
        env[lib["wasm"].names["i32_zero"]] = IntVal(0)
        env[lib["wasm"].names["i32_one"]] = IntVal(1)
    if "string" in lib:
        string = lib["string"]
        string_ty = string.names["String"]

        def first_arg(a, this):
            if not args:
                raise MossPanic("first_arg: the program was given no arguments")
            return StrVal(args[0])

        def print_(a, this):
            sys.stdout.write(a[0].value)
            return UNIT

        def length(a, this):
            return IntVal(len(this.value))

        def get(a, this):
            index = a[0].value
            if not 0 <= index < len(this.value):
                raise MossPanic(f"String.get: index {index} out of range")
            return CharVal(this.value[index])

        def arg_at(a, this):
            index = a[0].value
            argv = [sys.argv[0], *args]
            return StrVal(argv[index] if 0 <= index < len(argv) else "")

        env[string.names["first_arg"]] = native(first_arg)
        env[string.names["arg_count"]] = native(
            lambda a, this: IntVal(len(args) + 1)
        )
        env[string.names["arg_at"]] = native(arg_at)
        env[string.names["print"]] = NativeFn("print", print_)
        def slice_(a, this):
            start, count = a[0].value, a[1].value
            if start < 0 or count < 0 or start + count > len(this.value):
                raise MossPanic(
                    f"String.slice: {start}+{count} out of range for a string "
                    f"of length {len(this.value)}"
                )
            return StrVal(this.value[start : start + count])

        env[(string_ty, string.detached["length"])] = native(length)
        env[(string_ty, string.detached["slice"])] = native(slice_)
        env[(string_ty, string.detached["concat"])] = native(
            lambda a, this: StrVal(this.value + a[0].value)
        )
        env[(string_ty, string.detached["get"])] = native(get)
    if "cell" in lib:
        cell = lib["cell"]
        cell_ty = cell.names["CellInt"]
        env[cell.names["cell_int"]] = native(lambda a, this: CellVal(IntVal(0)))

        def read(a, this):
            return this.value

        def write(a, this):
            this.value = a[0]
            return UNIT

        env[(cell_ty, cell.detached["read"])] = native(read)
        env[(cell_ty, cell.detached["write"])] = native(write)
    if "list" in lib:
        lst = lib["list"]
        list_ty = lst.names["IntList"]
        env[lst.names["int_list"]] = native(lambda a, this: ListVal([]))

        def list_push(a, this):
            this.items.append(a[0])
            return UNIT

        def list_index(a, this):
            index = a[0].value
            if not 0 <= index < len(this.items):
                raise MossPanic(f"IntList index {index} out of range")
            return index

        env[(list_ty, lst.detached["push"])] = native(list_push)
        env[(list_ty, lst.detached["get"])] = native(
            lambda a, this: this.items[list_index(a, this)]
        )

        def list_set(a, this):
            this.items[list_index(a, this)] = a[1]
            return UNIT

        env[(list_ty, lst.detached["set"])] = native(list_set)
        env[(list_ty, lst.detached["length"])] = native(
            lambda a, this: IntVal(len(this.items))
        )
    if "strlist" in lib:
        sl = lib["strlist"]
        strlist_ty = sl.names["StrList"]
        env[sl.names["str_list"]] = native(lambda a, this: ListVal([]))
        env[(strlist_ty, sl.detached["push"])] = native(list_push)

        def strlist_get(a, this):
            index = a[0].value
            if not 0 <= index < len(this.items):
                raise MossPanic(f"StrList index {index} out of range")
            return this.items[index]

        env[(strlist_ty, sl.detached["get"])] = native(strlist_get)
        env[(strlist_ty, sl.detached["length"])] = native(
            lambda a, this: IntVal(len(this.items))
        )
    if "path" in lib:
        import os

        path = lib["path"]
        path_ty = path.names["Path"]
        env[path.names["pwd"]] = PathVal(os.getcwd())

        def join(a, this):
            return PathVal(os.path.normpath(os.path.join(this.value, a[0].value)))

        def read_file(a, this):
            try:
                with open(this.value, encoding="utf-8") as f:
                    return StrVal(f.read())
            except OSError as e:
                raise MossPanic(f"Path.read: {e}")

        env[(path_ty, path.detached["join"])] = native(join)
        env[(path_ty, path.detached["read"])] = native(read_file)
    wasm_env(program, lib, env, args or [])
    return env


PAGE = 65536
HEAP_BASE = 1024
PREOPEN_FD = 3


def wasm_env(program: Program, lib: dict, env: dict, args: list) -> None:
    """The primitive context (D52), emulated: a linear memory, the i32/i64
    instructions over it, and the WASI calls the standard library uses.

    This is what lets the interpreter run a program whose `Std` is written
    in Moss — the same program the backend compiles. It is deliberately the
    *only* thing the interpreter provides natively, everything above it
    being ordinary Moss."""
    if "wasm" not in lib:
        return
    memory = bytearray(2 * PAGE)
    memory[16:20] = HEAP_BASE.to_bytes(4, "little")  # what `_start` does
    files: dict = {}
    argv = [sys.argv[0], *args]

    def wrap(n: int, bits: int = 32) -> int:
        n &= (1 << bits) - 1
        return n - (1 << bits) if n >> (bits - 1) else n

    def grow(end: int) -> None:
        if end > len(memory):
            memory.extend(bytes(-(-(end - len(memory)) // PAGE) * PAGE))

    def load(addr: int, width: int, signed: bool) -> int:
        grow(addr + width)
        return int.from_bytes(memory[addr : addr + width], "little", signed=signed)

    def store(addr: int, width: int, value: int) -> None:
        grow(addr + width)
        memory[addr : addr + width] = (value & ((1 << (width * 8)) - 1)).to_bytes(
            width, "little"
        )

    def text(addr: int, length: int) -> str:
        grow(addr + length)
        return memory[addr : addr + length].decode("utf-8", "replace")

    def fn(name: str, impl):
        symbol = lib["wasm"].names.get(name) or lib["wasip1"].names.get(name)
        if symbol is not None:
            env[symbol] = NativeFn(name, lambda a, this, impl=impl: impl(a))

    # Memory instructions.
    loads = {"i32_load": (4, True), "i32_load8_s": (1, True), "i32_load8_u": (1, False),
             "i32_load16_s": (2, True), "i32_load16_u": (2, False),
             "i64_load": (8, True)}
    for name, (width, signed) in loads.items():
        fn(name, lambda a, w=width, sg=signed: IntVal(load(a[0].value, w, sg)))
    for name, width in {"i32_store": 4, "i32_store8": 1, "i32_store16": 2,
                        "i64_store": 8}.items():
        fn(name, lambda a, w=width: (store(a[0].value, w, a[1].value), UNIT)[1])
    fn("memory_size", lambda a: IntVal(len(memory) // PAGE))

    def memory_grow(a):
        before = len(memory) // PAGE
        memory.extend(bytes(a[0].value * PAGE))
        return IntVal(before)

    fn("memory_grow", memory_grow)
    fn("memory_copy", lambda a: (memory.__setitem__(
        slice(a[0].value, a[0].value + a[2].value),
        memory[a[1].value : a[1].value + a[2].value]), UNIT)[1])
    fn("memory_fill", lambda a: (memory.__setitem__(
        slice(a[0].value, a[0].value + a[2].value),
        bytes([a[1].value & 0xFF]) * a[2].value), UNIT)[1])

    def unreachable(a):
        raise MossPanic("unreachable")

    fn("unreachable", unreachable)

    # Numeric instructions. Comparisons yield an i32, as in Wasm.
    def unsigned(n, bits=32):
        return n & ((1 << bits) - 1)

    for bits, prefix in ((32, "i32"), (64, "i64")):
        ops = {
            "add": lambda x, y: x + y, "sub": lambda x, y: x - y,
            "mul": lambda x, y: x * y,
            "and": lambda x, y: x & y, "or": lambda x, y: x | y,
            "xor": lambda x, y: x ^ y,
        }
        for op, f in ops.items():
            fn(f"{prefix}_{op}", lambda a, f=f, b=bits: IntVal(wrap(f(a[0].value, a[1].value), b)))
        fn(f"{prefix}_div_s", lambda a, b=bits: IntVal(wrap(int(a[0].value / a[1].value), b)))
        fn(f"{prefix}_div_u", lambda a, b=bits: IntVal(wrap(unsigned(a[0].value, b) // unsigned(a[1].value, b), b)))
        fn(f"{prefix}_rem_s", lambda a, b=bits: IntVal(wrap(abs(a[0].value) % abs(a[1].value) * (1 if a[0].value >= 0 else -1), b)))
        fn(f"{prefix}_rem_u", lambda a, b=bits: IntVal(wrap(unsigned(a[0].value, b) % unsigned(a[1].value, b), b)))
        fn(f"{prefix}_shl", lambda a, b=bits: IntVal(wrap(a[0].value << (a[1].value % b), b)))
        fn(f"{prefix}_shr_s", lambda a, b=bits: IntVal(wrap(a[0].value >> (a[1].value % b), b)))
        fn(f"{prefix}_shr_u", lambda a, b=bits: IntVal(wrap(unsigned(a[0].value, b) >> (a[1].value % b), b)))
        fn(f"{prefix}_eqz", lambda a: IntVal(1 if a[0].value == 0 else 0))
        cmps = {"eq": lambda x, y: x == y, "ne": lambda x, y: x != y,
                "lt_s": lambda x, y: x < y, "gt_s": lambda x, y: x > y,
                "le_s": lambda x, y: x <= y, "ge_s": lambda x, y: x >= y}
        for op, f in cmps.items():
            fn(f"{prefix}_{op}", lambda a, f=f: IntVal(1 if f(a[0].value, a[1].value) else 0))
        ucmps = {"lt_u": lambda x, y: x < y, "gt_u": lambda x, y: x > y,
                 "le_u": lambda x, y: x <= y, "ge_u": lambda x, y: x >= y}
        for op, f in ucmps.items():
            fn(f"{prefix}_{op}", lambda a, f=f, b=bits: IntVal(
                1 if f(unsigned(a[0].value, b), unsigned(a[1].value, b)) else 0))
        for op, bit in (("clz", None), ("ctz", None), ("popcnt", None)):
            fn(f"{prefix}_{op}", lambda a, o=op, b=bits: IntVal(
                (unsigned(a[0].value, b).bit_length() and b - unsigned(a[0].value, b).bit_length()) or (b if a[0].value == 0 else 0)
                if o == "clz"
                else (b if a[0].value == 0 else (unsigned(a[0].value, b) & -unsigned(a[0].value, b)).bit_length() - 1)
                if o == "ctz"
                else bin(unsigned(a[0].value, b)).count("1")))
        for op, left in (("rotl", True), ("rotr", False)):
            fn(f"{prefix}_{op}", lambda a, lf=left, b=bits: IntVal(wrap(
                (unsigned(a[0].value, b) << (a[1].value % b) | unsigned(a[0].value, b) >> (b - a[1].value % b))
                if lf else
                (unsigned(a[0].value, b) >> (a[1].value % b) | unsigned(a[0].value, b) << (b - a[1].value % b)),
                b)))
    for name, width in {"i64_load8_s": 1, "i64_load8_u": 1, "i64_load16_s": 2,
                        "i64_load16_u": 2, "i64_load32_s": 4, "i64_load32_u": 4}.items():
        fn(name, lambda a, w=width, sg=name.endswith("_s"): IntVal(load(a[0].value, w, sg)))
    for name, width in {"i64_store8": 1, "i64_store16": 2, "i64_store32": 4}.items():
        fn(name, lambda a, w=width: (store(a[0].value, w, a[1].value), UNIT)[1])
    for name, bits in {"i32_extend8_s": 8, "i32_extend16_s": 16, "i64_extend8_s": 8,
                       "i64_extend16_s": 16, "i64_extend32_s": 32}.items():
        fn(name, lambda a, b=bits: IntVal(wrap(a[0].value, b)))
    fn("i32_wrap_i64", lambda a: IntVal(wrap(a[0].value, 32)))
    fn("i64_extend_i32_s", lambda a: IntVal(a[0].value))
    fn("i64_extend_i32_u", lambda a: IntVal(unsigned(a[0].value, 32)))

    # WASI.
    def iovecs(ptr: int, count: int):
        out = []
        for i in range(count):
            base = load(ptr + i * 8, 4, False)
            length = load(ptr + i * 8 + 4, 4, False)
            out.append((base, length))
        return out

    def fd_write(a):
        total = 0
        for base, length in iovecs(a[1].value, a[2].value):
            sys.stdout.write(text(base, length))
            total += length
        store(a[3].value, 4, total)
        return IntVal(0)

    def args_sizes_get(a):
        store(a[0].value, 4, len(argv))
        store(a[1].value, 4, sum(len(x) + 1 for x in argv))
        return IntVal(0)

    def args_get(a):
        cursor = a[1].value
        for i, item in enumerate(argv):
            store(a[0].value + i * 4, 4, cursor)
            raw = item.encode() + b"\0"
            grow(cursor + len(raw))
            memory[cursor : cursor + len(raw)] = raw
            cursor += len(raw)
        return IntVal(0)

    def path_open(a):
        name = text(a[2].value, a[3].value)
        try:
            handle = open(name, "rb")
        except OSError:
            return IntVal(44)  # ENOENT
        descriptor = PREOPEN_FD + 1 + len(files)
        files[descriptor] = handle
        store(a[8].value, 4, descriptor)
        return IntVal(0)

    def fd_read(a):
        handle = files.get(a[0].value)
        total = 0
        for base, length in iovecs(a[1].value, a[2].value):
            chunk = handle.read(length) if handle else b""
            grow(base + len(chunk))
            memory[base : base + len(chunk)] = chunk
            total += len(chunk)
        store(a[3].value, 4, total)
        return IntVal(0)

    def fd_close(a):
        handle = files.pop(a[0].value, None)
        if handle:
            handle.close()
        return IntVal(0)

    def proc_exit(a):
        raise SystemExit(a[0].value)

    for name, impl in (
        ("fd_write", fd_write), ("args_sizes_get", args_sizes_get),
        ("args_get", args_get), ("path_open", path_open), ("fd_read", fd_read),
        ("fd_close", fd_close), ("proc_exit", proc_exit),
    ):
        fn(name, impl)

    # The rest of preview 1 is declared, so `Wasi` can be assumed whole;
    # anything not implemented here reports ENOSYS rather than pretending.
    def unsupported(name):
        def call(a, this, name=name):
            raise MossPanic(f"{name} is not implemented by the interpreter")

        return NativeFn(name, call)

    for name, symbol in lib["wasip1"].names.items():
        if symbol.kind == SymKind.FN and symbol not in env:
            env[symbol] = unsupported(name)

    # The one coercion that is neither instruction nor syscall.
    if "wasistd" in lib and "bool" in lib:
        bool_ty = lib["bool"].names["Bool"]
        true = TagVal(bool_ty, UnitVal(lib["bool"].names["True"]))
        false = TagVal(bool_ty, UnitVal(lib["bool"].names["False"]))
        env[lib["wasistd"].names["i32_bool"]] = NativeFn(
            "i32_bool", lambda a, this: true if a[0].value != 0 else false
        )


class LinkError(Exception):
    pass


def run_main(program: Program, lower, args: list | None = None) -> None:
    """D39: find main, check its needs against the native Std, run it."""
    entry = program.entry
    main = entry.names.get("main")
    if main is None or main.kind != SymKind.FN or main.decl.body is None:
        raise LinkError(f"{entry.path}: no `main` function")
    if main.decl.params or main.decl.ret is not None:
        raise LinkError("`main` must take no parameters and return the unit type")
    fn = lower.fns[id(main)]
    natives = native_env(program, args)
    ctx = {}
    for need in fn.needs:
        provision = natives.get(need)
        if provision is None:
            name = need.name if isinstance(need, Symbol) else (
                f"{need[0].name}{need[1].name}"
            )
            raise LinkError(
                f"`main` assumes `{name}`, which is not part of the `Std` context"
            )
        ctx[need] = provision
    Interp(lower.fns).call_fn(fn, [], ctx)
