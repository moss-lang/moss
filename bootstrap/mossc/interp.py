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


def native_env(program: Program) -> dict:
    """Runtime provisions for the native Std (D38): keys are the abstract
    symbols declared in lib/, values their Python implementations."""
    env = {}

    def putchar(args, this):
        sys.stdout.write(args[0].value)
        return UNIT

    for module in program.modules.values():
        if module.path.endswith("lib/std.moss"):
            env[module.names["putchar"]] = NativeFn("putchar", putchar)
        elif module.path.endswith("lib/char.moss"):
            for name, char in CHARS.items():
                env[module.names[name]] = CharVal(char)
    return env


class LinkError(Exception):
    pass


def run_main(program: Program, lower) -> None:
    """D39: find main, check its needs against the native Std, run it."""
    entry = program.entry
    main = entry.names.get("main")
    if main is None or main.kind != SymKind.FN or main.decl.body is None:
        raise LinkError(f"{entry.path}: no `main` function")
    if main.decl.params or main.decl.ret is not None:
        raise LinkError("`main` must take no parameters and return the unit type")
    fn = lower.fns[id(main)]
    natives = native_env(program)
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
