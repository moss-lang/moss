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


class Frame:
    def __init__(self, locals_: dict, ctx: dict, this):
        self.locals = locals_
        self.ctx = ctx
        self.this = this


class Interp:
    def __init__(self, fns: dict):
        self.fns = fns  # id(Symbol) -> FnIR

    def call_fn(self, fn: ir.FnIR, args: list, ctx: dict, this=None):
        frame = Frame(dict(zip(fn.params, args)), ctx, this)
        try:
            return self.eval_block(fn.body, frame)
        except _Return as r:
            return r.value

    def invoke(self, impl, args: list, this=None):
        if isinstance(impl, NativeFn):
            return impl.call(args, this)
        if isinstance(impl, Closure):
            return self.call_fn(impl.fn, args, impl.ctx, this)
        raise MossPanic(f"not callable: {impl!r}")

    def eval_block(self, block: ir.Block, frame: Frame):
        # Binds are lexically scoped to the block: restore ctx on exit.
        saved = frame.ctx
        frame.ctx = dict(saved)
        try:
            for stmt in block.stmts:
                self.exec_stmt(stmt, frame)
            if block.tail is None:
                return UNIT
            return self.eval(block.tail, frame)
        finally:
            frame.ctx = saved

    def exec_stmt(self, stmt, frame: Frame):
        if isinstance(stmt, (ir.Let, ir.Assign)):
            frame.locals[stmt.name] = self.eval(stmt.expr, frame)
        elif isinstance(stmt, ir.BindVal):
            frame.ctx[stmt.key] = self.eval(stmt.expr, frame)
        elif isinstance(stmt, ir.BindFn):
            fn = self.fns[id(stmt.fn)]
            captured = {k: frame.ctx[k] for k in fn.needs}
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
        if isinstance(value, UnitVal) and value.symbol is not None:
            return value.symbol.name == "True"
        raise MossPanic(f"not a Bool: {value!r}")

    def eval(self, expr, frame: Frame):
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
                ctx = {k: frame.ctx[k] for k in fn.needs}
                return self.call_fn(fn, args, ctx, this)
            return self.invoke(frame.ctx[target], args, this)
        if isinstance(expr, ir.If):
            if self.truthy(self.eval(expr.cond, frame)):
                return self.eval_block(expr.then, frame)
            if expr.els is None:
                return UNIT
            if isinstance(expr.els, ir.Block):
                return self.eval_block(expr.els, frame)
            return self.eval(expr.els, frame)
        if isinstance(expr, ir.Match):
            value = self.eval(expr.scrutinee, frame)
            for arm in expr.arms:
                if self.match_pat(arm.pat, value, frame):
                    if isinstance(arm.body, ir.Block):
                        return self.eval_block(arm.body, frame)
                    return self.eval(arm.body, frame)
            raise MossPanic(f"no match arm for {value!r}")
        if isinstance(expr, ir.Return):
            raise _Return(UNIT if expr.expr is None else self.eval(expr.expr, frame))
        if isinstance(expr, ir.Break):
            raise _Break()
        if isinstance(expr, ir.Block):
            return self.eval_block(expr, frame)
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
            for field_name, binder in pat.fields:
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
        for name in ("std", "char", "bool", "num", "int", "string", "cell", "path"):
            if module.path.endswith(f"lib/{name}.moss"):
                lib[name] = module
    if "bool" in lib:
        true = UnitVal(lib["bool"].names["True"])
        false = UnitVal(lib["bool"].names["False"])

        def boolean(b):
            return true if b else false

    def native(fn):
        return NativeFn(fn.__name__, fn)

    if "std" in lib:

        def putchar(a, this):
            sys.stdout.write(a[0].value)
            return UNIT

        env[lib["std"].names["putchar"]] = native(putchar)
    if "char" in lib:
        char_ty = lib["char"].names["Char"]
        for name, char in CHARS.items():
            env[lib["char"].names[name]] = CharVal(char)
        if "num" in lib:
            det = lib["num"].detached
            env[(char_ty, det["eq"])] = native(
                lambda a, this: boolean(this.value == a[0].value)
            )
            env[(char_ty, det["ne"])] = native(
                lambda a, this: boolean(this.value != a[0].value)
            )
    if "int" in lib:
        int_ty = lib["int"].names["Int"]
        env[lib["int"].names["zero"]] = IntVal(0)
        env[lib["int"].names["one"]] = IntVal(1)
        if "num" in lib:
            det = lib["num"].detached
            arith = {
                "add": lambda x, y: x + y,
                "sub": lambda x, y: x - y,
                "mul": lambda x, y: x * y,
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

        env[string.names["first_arg"]] = native(first_arg)
        env[string.names["print"]] = NativeFn("print", print_)
        env[(string_ty, string.detached["length"])] = native(length)
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
    return env


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
