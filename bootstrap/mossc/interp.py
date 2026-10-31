"""Interpreter over the core IR (stage 5, docs/design/semantics.md §11).

The runtime environment is exactly the explicit context structure lowering
produced: a dict from need keys (Symbol, or (receiver, method) pairs) to
values and callables. `bind` writes entries; calls read the callee's needs
out of the current environment; fn binds capture the environment pieces
their provider needs (closures). Types are gone except as the nominal tags
values carry for `match`.
"""

import math
import struct
import sys
from dataclasses import dataclass

from . import ir
from .collect import Program, SymKind, Symbol
from .native import CHARS


def emit(data: bytes) -> None:
    """Write program output as *bytes*.

    The Wasm backend's `putchar` is an `i32.store8` and its `fd_write`
    hands WASI the raw memory, so a Moss program that writes byte 0x80
    writes one byte there. Going through `sys.stdout` would have written
    two (UTF-8), which is a difference the goldens cannot see until a
    program emits binary — and the self-hosted emitter does. Tests may
    replace this sink; `redirect_stdout` alone cannot, since a StringIO
    has no `.buffer`.
    """
    buffer = getattr(sys.stdout, "buffer", None)
    if buffer is None:  # a text-only sink (io.StringIO under a test)
        sys.stdout.write(data.decode("utf-8", "surrogateescape"))
    else:
        buffer.write(data)


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
class FloatVal:
    """An f32 or an f64 (D67). Either is a Python float; an f32 is one
    that `f32` has already rounded, so it is exactly representable."""

    value: float


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


@dataclass(eq=False)
class GcRef:
    """A garbage-collected struct (D75): Python's heap is the GC heap. It
    remembers the type it was made at, which is what a Wasm struct's type
    is, so a reference forged at another type fails as `ref.cast` does."""

    ty: object
    value: object


@dataclass(frozen=True)
class Closure:
    fn: ir.FnIR
    ctx: dict
    # The provider's type arguments, kept when the program binds `Memory`
    # (D79).
    theta: object = None


@dataclass(frozen=True)
class NativeFn:
    name: str
    call: object  # (args: list, this) -> value


@dataclass(frozen=True)
class MemoryFn(NativeFn):
    """A memory instruction (D79): `select` points the emulated
    instructions at the memory a `Memory` type is, for one call."""

    select: object = None  # (Memory type | None) -> None


UNIT = UnitVal(None)
_ABSENT = object()  # a local a block's `let` did not shadow


class MossPanic(Exception):
    pass


# Float semantics, as Wasm defines them (D67). Python's floats are IEEE
# doubles and its arithmetic rounds to nearest, so f64 is Python's own,
# except where Python raises rather than producing an infinity or a NaN.
# An f32 operation is computed in double and rounded once to single,
# which for + - * / and sqrt is exactly the single-precision result: a
# double has more than twice a single's precision, so the second rounding
# never sees a tie the first one made.


def f32(x: float) -> float:
    """The nearest single to `x`, ties to even, as a Python float."""
    try:
        return struct.unpack("<f", struct.pack("<f", x))[0]
    except OverflowError:
        # Past the largest single by at least half an ulp: an infinity.
        return math.copysign(math.inf, x)


def int_to_float(n: int, precision: int) -> float:
    """`n` rounded to `precision` significant bits (24 for f32, 53 for
    f64), ties to even — rounded once, from the integer itself, since
    going through a double first would round an i64 twice on its way to
    an f32."""
    a = abs(n)
    excess = a.bit_length() - precision
    if excess > 0:
        q, r = divmod(a, 1 << excess)
        half = 1 << (excess - 1)
        if r > half or (r == half and q & 1):
            q += 1
        a = q << excess
    return math.copysign(float(a), -1.0 if n < 0 else 1.0)


def fdiv(x: float, y: float) -> float:
    if y == 0.0:
        if x == 0.0 or math.isnan(x):
            return math.nan
        return math.copysign(math.inf, x) * math.copysign(1.0, y)
    return x / y


def fsqrt(x: float) -> float:
    if math.isnan(x) or x < 0.0:
        return math.nan
    return math.sqrt(x)


def fmin(x: float, y: float) -> float:
    """Wasm's `min`: NaN if either is, and -0 below +0."""
    if math.isnan(x) or math.isnan(y):
        return math.nan
    if x == y:
        return x if math.copysign(1.0, x) < 0 else y
    return min(x, y)


def fmax(x: float, y: float) -> float:
    if math.isnan(x) or math.isnan(y):
        return math.nan
    if x == y:
        return x if math.copysign(1.0, x) > 0 else y
    return max(x, y)


def fround(x: float, how) -> float:
    """`ceil`, `floor`, `trunc` or `nearest` (`round`, which in Python
    rounds a float's ties to even): integral, keeping the sign of a zero,
    with NaN and the infinities left alone."""
    if not math.isfinite(x) or x == 0.0:
        return x
    return math.copysign(float(how(x)), x)


def trunc_to_int(x: float, bits: int, signed: bool, saturate: bool) -> int:
    """Float to integer, toward zero. The trapping form traps on NaN and
    on anything outside the range; the saturating one clamps, with NaN
    going to 0. The result is in the interpreter's representation of an
    integer of that width: signed, whatever the instruction's signedness."""
    lo, hi = (-(1 << (bits - 1)), (1 << (bits - 1)) - 1) if signed else (0, (1 << bits) - 1)
    if math.isnan(x):
        if saturate:
            return 0
        raise MossPanic("invalid conversion to integer")
    if math.isinf(x):
        t = hi if x > 0 else lo
        if not saturate:
            raise MossPanic("integer overflow")
    else:
        t = math.trunc(x)
        if t < lo or t > hi:
            if not saturate:
                raise MossPanic("integer overflow")
            t = max(lo, min(hi, t))
    t &= (1 << bits) - 1
    return t - (1 << bits) if t >> (bits - 1) else t


def reinterpret(value, src: str, dst: str):
    """The same bytes under another struct code (`i`/`f`, `q`/`d`)."""
    if src in "iq":
        value &= (1 << (32 if src == "i" else 64)) - 1
        src = src.upper()
    return struct.unpack("<" + dst, struct.pack("<" + src, value))[0]


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
    theta: object = None


class Frame:
    def __init__(self, locals_: dict, ctx: dict, this, theta=None):
        self.locals = locals_
        self.ctx = ctx
        self.this = this
        # The frame's type arguments, as the back end's `theta`, kept only
        # when the program binds `Memory` (D79), so that a generic
        # function's memory instructions know which memory.
        self.theta = theta or {}


class Interp:
    def __init__(self, fns: dict, memory_sym=None):
        self.fns = fns  # id(Symbol) -> FnIR
        # lib/wasm.moss's `Memory`, when the program binds it (D79).
        self.memory_sym = memory_sym

    def theta_for(self, tyargs, theta: dict) -> dict:
        """A callee's type arguments, at the caller's (D79): all of them,
        since `Memory` may be bound to another abstract type a caller
        supplies."""
        if self.memory_sym is None or not tyargs:
            return {}
        from .build import subst

        return {p: subst(theta, t) for p, t in tyargs}

    def call_fn(self, fn: ir.FnIR, args: list, ctx: dict, this=None, theta=None):
        while True:
            frame = Frame(dict(zip(fn.params, args)), ctx, this, theta)
            try:
                result = self.eval_block(fn.body, frame, tail=True)
            except _Return as r:
                result = r.value
            if isinstance(result, _Tail):
                fn, args, ctx, this = result.fn, result.args, result.ctx, result.this
                theta = result.theta
                continue
            return result

    def invoke(self, impl, args: list, this=None):
        if isinstance(impl, NativeFn):
            return impl.call(args, this)
        if isinstance(impl, Closure):
            return self.call_fn(impl.fn, args, impl.ctx, this, impl.theta)
        raise MossPanic(f"not callable: {impl!r}")

    def eval_block(self, block: ir.Block, frame: Frame, tail=False):
        # Binds are lexically scoped to the block: restore ctx on exit. A
        # _Tail escaping the block is safe: it carries its own ctx snapshot.
        # Its `let`s are scoped the same way: a name it shadows is back
        # afterwards, with the value it had when it was shadowed.
        saved = frame.ctx
        frame.ctx = dict(saved)
        shadowed = {}
        try:
            for stmt in block.stmts:
                if isinstance(stmt, ir.Let) and stmt.name not in shadowed:
                    shadowed[stmt.name] = frame.locals.get(stmt.name, _ABSENT)
                self.exec_stmt(stmt, frame)
            if block.tail is None:
                return UNIT
            return self.eval(block.tail, frame, tail=tail)
        finally:
            frame.ctx = saved
            for name, value in shadowed.items():
                if value is _ABSENT:
                    frame.locals.pop(name, None)
                else:
                    frame.locals[name] = value

    def exec_stmt(self, stmt, frame: Frame):
        if isinstance(stmt, (ir.Let, ir.Assign)):
            frame.locals[stmt.name] = self.eval(stmt.expr, frame)
        elif isinstance(stmt, ir.BindVal):
            frame.ctx[stmt.key] = self.eval(stmt.expr, frame)
        elif isinstance(stmt, ir.BindFn):
            fn = self.fns[id(stmt.fn)]
            captured = {ck: frame.ctx[sk] for ck, sk in stmt.needs_map}
            # An anonymous function is in this frame's terms (D70).
            theta = (
                frame.theta if stmt.tyargs is None
                else self.theta_for(stmt.tyargs, frame.theta)
            )
            frame.ctx[stmt.key] = Closure(fn, captured, theta)
        elif isinstance(stmt, ir.BindPrim):
            frame.ctx[stmt.key] = gc_prim(stmt)
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
            payload = self.eval(expr.payload, frame)
            if expr.ty is not None:
                forged_check(expr.ty, payload)
            return TagVal(expr.symbol, payload)
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
                theta = self.theta_for(expr.tyargs, frame.theta)
                if tail:
                    return _Tail(fn, args, ctx, this, theta)
                return self.call_fn(fn, args, ctx, this, theta)
            impl = frame.ctx[target]
            if expr.memory is not None and isinstance(impl, MemoryFn):
                from .build import subst

                impl.select(subst(frame.theta, expr.memory))
                try:
                    return impl.call(args, this)
                finally:
                    impl.select(None)
            if tail and isinstance(impl, Closure):
                return _Tail(impl.fn, args, impl.ctx, this, impl.theta)
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
                # A pattern's binders are scoped to its arm: whatever they
                # shadowed is back afterwards.
                names = ir.pat_binders(arm.pat)
                saved = {n: frame.locals[n] for n in names if n in frame.locals}
                if self.match_pat(arm.pat, value, frame):
                    try:
                        if isinstance(arm.body, ir.Block):
                            return self.eval_block(arm.body, frame, tail=tail)
                        return self.eval(arm.body, frame, tail=tail)
                    finally:
                        for n in names:
                            frame.locals.pop(n, None)
                        frame.locals.update(saved)
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
            for field_name, binder, *_ in pat.fields:
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
        for name in ("std", "char", "bool", "num", "int", "float", "string",
                     "strlist", "cell", "path", "list", "wasm", "wasip1", "wasistd"):
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
        det = lib["num"].detached

        def truth(v) -> bool:
            return v.payload.symbol.name == "True"

        env[(bool_ty, det["not"])] = native(lambda a, this: boolean(not truth(this)))
        # D68: `&`, `|` and `^` on booleans. Both operands have already
        # been evaluated, as for any method.
        env[(bool_ty, det["and"])] = native(lambda a, this: boolean(truth(this) and truth(a[0])))
        env[(bool_ty, det["or"])] = native(lambda a, this: boolean(truth(this) or truth(a[0])))
        env[(bool_ty, det["xor"])] = native(lambda a, this: boolean(truth(this) != truth(a[0])))

    if "std" in lib:

        def putchar(a, this):
            # One byte, as `i32.store8` gives in the Wasm backend.
            emit(bytes([ord(a[0].value) & 0xFF]))
            return UNIT

        env[lib["std"].names["putchar"]] = native(putchar)

        def put_bytes(a, this):
            # A whole buffer, each element's low byte — the same bytes the
            # loop over `putchar` wrote, in one call rather than one each.
            emit(bytes(v.value & 0xFF for v in a[0].items))
            return UNIT

        env[lib["std"].names["put_bytes"]] = native(put_bytes)
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
            # The compiled native `Std` lowers `Int` to i32 (`i32.add`,
            # `i32.div_s`, ...), so the interpreter computes exactly what those
            # instructions do: wrapping, truncating division, shift counts
            # taken mod 32, and the same traps.
            def i32(n: int) -> int:
                return (n + 2**31) % 2**32 - 2**31

            env[(int_ty, det["neg"])] = native(lambda a, this: IntVal(i32(-this.value)))
            arith = {
                "add": lambda x, y: i32(x + y),
                "sub": lambda x, y: i32(x - y),
                "mul": lambda x, y: i32(x * y),
                "and": lambda x, y: i32(x & y),
                "or": lambda x, y: i32(x | y),
                "xor": lambda x, y: i32(x ^ y),
                "shl": lambda x, y: i32(x << (y % 32)),
                "shr": lambda x, y: x >> (y % 32),
            }
            for name, op in arith.items():
                env[(int_ty, det[name])] = NativeFn(
                    name, lambda a, this, op=op: IntVal(op(this.value, a[0].value))
                )

            def div(a, this):
                x, y = this.value, a[0].value
                if y == 0:
                    raise MossPanic("division by zero")
                if x == -(2**31) and y == -1:
                    raise MossPanic("integer overflow")
                q = abs(x) // abs(y)
                return IntVal(q if (x < 0) == (y < 0) else -q)

            def rem(a, this):
                x, y = this.value, a[0].value
                if y == 0:
                    raise MossPanic("remainder by zero")
                r = abs(x) % abs(y)
                return IntVal(r if x >= 0 else -r)

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
    if "float" in lib and "num" in lib:
        # D67: the native `Float` is an f64, which is a Python float, and
        # its conversions to `Int` saturate at i32, as `WasiStd`'s do.
        float_ty = lib["float"].names["Float"]
        det = lib["num"].detached
        float_arith = {
            "add": lambda x, y: x + y, "sub": lambda x, y: x - y,
            "mul": lambda x, y: x * y, "div": fdiv,
        }
        for name, op in float_arith.items():
            env[(float_ty, det[name])] = NativeFn(
                name, lambda a, this, op=op: FloatVal(op(this.value, a[0].value))
            )
        float_unary = {"neg": lambda x: -x, "sqrt": fsqrt, "abs": math.fabs}
        for name, op in float_unary.items():
            env[(float_ty, det[name])] = NativeFn(
                name, lambda a, this, op=op: FloatVal(op(this.value))
            )
        float_compare = {
            "eq": lambda x, y: x == y, "ne": lambda x, y: x != y,
            "lt": lambda x, y: x < y, "gt": lambda x, y: x > y,
            "le": lambda x, y: x <= y, "ge": lambda x, y: x >= y,
        }
        for name, op in float_compare.items():
            env[(float_ty, det[name])] = NativeFn(
                name, lambda a, this, op=op: boolean(op(this.value, a[0].value))
            )
        fdet = lib["float"].detached
        env[lib["float"].names["float"]] = native(
            lambda a, this: FloatVal(int_to_float(a[0].value, 53))
        )
        env[(float_ty, fdet["trunc"])] = native(
            lambda a, this: IntVal(trunc_to_int(this.value, 32, True, True))
        )
        env[(float_ty, fdet["round"])] = native(
            lambda a, this: IntVal(trunc_to_int(fround(this.value, round), 32, True, True))
        )
        # D69: `FloatList` is a Python list of them, as `IntList` is of Ints.
        flist_ty = lib["float"].names["FloatList"]
        env[lib["float"].names["float_list"]] = native(lambda a, this: ListVal([]))

        def flist_index(a, this):
            index = a[0].value
            if not 0 <= index < len(this.items):
                raise MossPanic(f"FloatList index {index} out of range")
            return index

        def flist_push(a, this):
            this.items.append(a[0])
            return UNIT

        def flist_set(a, this):
            this.items[flist_index(a, this)] = a[1]
            return UNIT

        env[(flist_ty, fdet["push"])] = native(flist_push)
        env[(flist_ty, fdet["get"])] = native(
            lambda a, this: this.items[flist_index(a, this)]
        )
        env[(flist_ty, fdet["set"])] = native(flist_set)
        env[(flist_ty, fdet["length"])] = native(
            lambda a, this: IntVal(len(this.items))
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
            emit(a[0].value.encode("utf-8", "surrogateescape"))
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

        def readable(a, this):
            # What `Path.read` would do, without the bytes: the WASI
            # implementation opens the file, since a path outside the
            # preopen is refused rather than absent.
            try:
                with open(this.value, encoding="utf-8"):
                    return boolean(True)
            except OSError:
                return boolean(False)

        env[(path_ty, path.detached["join"])] = native(join)
        env[(path_ty, path.detached["readable"])] = native(readable)
        env[(path_ty, path.detached["read"])] = native(read_file)
    wasm_env(program, lib, env, args or [])
    # Method keys carry the receiver's whole type (D62); the natives above
    # are written against bare receivers, which have no bindings.
    return {
        (receiver_type(k[0]), k[1]) if isinstance(k, tuple) else k: v
        for k, v in env.items()
    }


def receiver_type(sym: Symbol):
    """A bare receiver symbol as the type a method key holds."""
    from .types import TAbstract, TNominal, param

    if sym.kind == SymKind.TYPE:
        return TAbstract(param(sym), ())
    return TNominal(sym, ())


PAGE = 65536
# The instructions lib/wasm.moss declares under `assume Memory` (D79).
MEMORY_OPS = frozenset(
    [f"i32_load{s}" for s in ("", "8_s", "8_u", "16_s", "16_u")]
    + [f"i64_load{s}" for s in ("", "8_s", "8_u", "16_s", "16_u", "32_s", "32_u")]
    + [f"i32_store{s}" for s in ("", "8", "16")]
    + [f"i64_store{s}" for s in ("", "8", "16", "32")]
    + ["f32_load", "f64_load", "f32_store", "f64_store"]
    + ["memory_size", "memory_grow", "memory_copy", "memory_fill"]
)
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
    # D79: the memory the instructions below act on, which is the main
    # one except during a memory instruction specialized at another — a
    # unit `Memory` is bound to, each its own, made the first time it is
    # used with its heap pointer set as `_start` sets every memory's.
    memory_sym = lib["wasm"].names.get("Memory")
    others: dict = {}
    current = [memory]

    def select(t) -> None:
        from .types import TAbstract, TNominal

        if t is None or (
            isinstance(t, TAbstract) and getattr(t.symbol, "origin", t.symbol) is memory_sym
        ):
            current[0] = memory
            return
        if isinstance(t, TNominal) and t.symbol.kind == SymKind.UNIT:
            got = others.get(id(t.symbol))
            if got is None:
                got = bytearray(2 * PAGE)
                got[16:20] = HEAP_BASE.to_bytes(4, "little")
                others[id(t.symbol)] = got
            current[0] = got
            return
        raise MossPanic("a memory instruction where `Memory` is not a memory (D79)")

    def wrap(n: int, bits: int = 32) -> int:
        n &= (1 << bits) - 1
        return n - (1 << bits) if n >> (bits - 1) else n

    def grow(end: int) -> None:
        mem = current[0]
        if end > len(mem):
            mem.extend(bytes(-(-(end - len(mem)) // PAGE) * PAGE))

    def load(addr: int, width: int, signed: bool) -> int:
        grow(addr + width)
        return int.from_bytes(current[0][addr : addr + width], "little", signed=signed)

    def store(addr: int, width: int, value: int) -> None:
        grow(addr + width)
        current[0][addr : addr + width] = (value & ((1 << (width * 8)) - 1)).to_bytes(
            width, "little"
        )

    def text(addr: int, length: int) -> str:
        grow(addr + length)
        return memory[addr : addr + length].decode("utf-8", "replace")

    def fn(name: str, impl):
        symbol = lib["wasm"].names.get(name) or lib["wasip1"].names.get(name)
        if symbol is not None:
            if name in MEMORY_OPS:
                env[symbol] = MemoryFn(name, lambda a, this, impl=impl: impl(a), select)
            else:
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
    fn("memory_size", lambda a: IntVal(len(current[0]) // PAGE))

    def memory_grow(a):
        before = len(current[0]) // PAGE
        current[0].extend(bytes(a[0].value * PAGE))
        return IntVal(before)

    fn("memory_grow", memory_grow)
    fn("memory_copy", lambda a: (current[0].__setitem__(
        slice(a[0].value, a[0].value + a[2].value),
        current[0][a[1].value : a[1].value + a[2].value]), UNIT)[1])
    fn("memory_fill", lambda a: (current[0].__setitem__(
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

    # The float half (D67). An f32 is rounded after every operation that
    # could leave it with more than single precision.
    for prefix, rnd in (("f32", f32), ("f64", lambda x: x)):
        width, code = (4, "<f") if prefix == "f32" else (8, "<d")
        fn(f"{prefix}_load", lambda a, w=width, c=code: FloatVal(
            struct.unpack(c, (grow(a[0].value + w), current[0][a[0].value : a[0].value + w])[1])[0]))
        fn(f"{prefix}_store", lambda a, w=width, c=code: (
            grow(a[0].value + w),
            current[0].__setitem__(slice(a[0].value, a[0].value + w), struct.pack(c, a[1].value)),
            UNIT)[2])
        binary = {"add": lambda x, y: x + y, "sub": lambda x, y: x - y,
                  "mul": lambda x, y: x * y, "div": fdiv, "min": fmin, "max": fmax,
                  "copysign": math.copysign}
        for op, f in binary.items():
            fn(f"{prefix}_{op}", lambda a, f=f, r=rnd: FloatVal(r(f(a[0].value, a[1].value))))
        unary = {"abs": math.fabs, "neg": lambda x: -x, "sqrt": fsqrt,
                 "ceil": lambda x: fround(x, math.ceil),
                 "floor": lambda x: fround(x, math.floor),
                 "trunc": lambda x: fround(x, math.trunc),
                 "nearest": lambda x: fround(x, round)}
        for op, f in unary.items():
            fn(f"{prefix}_{op}", lambda a, f=f, r=rnd: FloatVal(r(f(a[0].value))))
        # Python's comparisons are IEEE's: a NaN is unordered, so it is
        # unequal to everything, itself included, and less than nothing.
        fcmps = {"eq": lambda x, y: x == y, "ne": lambda x, y: x != y,
                 "lt": lambda x, y: x < y, "gt": lambda x, y: x > y,
                 "le": lambda x, y: x <= y, "ge": lambda x, y: x >= y}
        for op, f in fcmps.items():
            fn(f"{prefix}_{op}", lambda a, f=f: IntVal(1 if f(a[0].value, a[1].value) else 0))
    for ibits in (32, 64):
        for fprefix in ("f32", "f64"):
            for sign in "su":
                for sat in ("", "sat_"):
                    fn(f"i{ibits}_trunc_{sat}{fprefix}_{sign}",
                       lambda a, b=ibits, sg=sign == "s", st=bool(sat): IntVal(
                           trunc_to_int(a[0].value, b, sg, st)))
                precision = 24 if fprefix == "f32" else 53
                fn(f"{fprefix}_convert_i{ibits}_{sign}",
                   lambda a, b=ibits, sg=sign == "s", p=precision: FloatVal(int_to_float(
                       a[0].value if sg else unsigned(a[0].value, b), p)))
    fn("f32_demote_f64", lambda a: FloatVal(f32(a[0].value)))
    fn("f64_promote_f32", lambda a: FloatVal(a[0].value))
    fn("i32_reinterpret_f32", lambda a: IntVal(reinterpret(a[0].value, "f", "i")))
    fn("i64_reinterpret_f64", lambda a: IntVal(reinterpret(a[0].value, "d", "q")))
    fn("f32_reinterpret_i32", lambda a: FloatVal(reinterpret(a[0].value, "i", "f")))
    fn("f64_reinterpret_i64", lambda a: FloatVal(reinterpret(a[0].value, "q", "d")))

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
            grow(base + length)
            emit(bytes(memory[base : base + length]))
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


def _differ(a, b) -> bool:
    """Whether two types are certainly different. An abstract type may be
    anything here — the interpreter knows a function's types only as it
    was lowered, generically — so it differs from nothing."""
    from .types import TAbstract, TNominal, TRecord, TUnion

    if isinstance(a, TAbstract) or isinstance(b, TAbstract):
        return False
    if type(a) is not type(b):
        return True
    if isinstance(a, TNominal):
        if a.symbol is not b.symbol or len(a.args) != len(b.args):
            return True
        return any(_differ(x, y) for (_, x), (_, y) in zip(a.args, b.args))
    if isinstance(a, TUnion):
        if len(a.members) != len(b.members):
            return True
        return any(_differ(x, y) for x, y in zip(a.members, b.members))
    if isinstance(a, TRecord):
        if [n for n, _ in a.fields] != [n for n, _ in b.fields]:
            return True
        return any(_differ(x, y) for (_, x), (_, y) in zip(a.fields, b.fields))
    return a != b


def forged_check(tag_ty, payload) -> None:
    """D75: building a `Ref[Boxed=X]` from a bare `StructRef` is a cast,
    and traps unless the struct is an `X`, as the back ends' `ref.cast`
    does."""
    held = next((a for s, a in tag_ty.args if s.name == "Boxed"), None)
    if not isinstance(payload, GcRef) or held is None:
        return
    if _differ(held, payload.ty):
        raise MossPanic("cast failure: a reference forged at another type (D75)")


def gc_prim(bind: "ir.BindPrim") -> NativeFn:
    """D75: what `bind Heap;` provides — boxing a value in a struct that
    remembers the type it was made at, and reading one back."""
    if bind.prim == "box":
        ref = bind.key[1].module.names["Ref"]

        def box(a, this, held=bind.held):
            return TagVal(ref, GcRef(held, this))

        return NativeFn("boxed", box)

    def deref(a, this):
        if this.payload is None:
            raise MossPanic("dereferencing a null reference")
        return this.payload.value

    return NativeFn("deref", deref)


class LinkError(Exception):
    pass


def run_main(program: Program, lower, args: list | None = None) -> None:
    """D39: find main, check its needs against the native Std, run it."""
    # A Moss frame costs several Python ones, and the self-hosted compiler
    # dispatches on a node kind with a chain of forty `else if`s — which
    # nests forty deep in the AST and so in any recursive walk of it.
    # Nothing here is meant to be a limit on Moss programs; raise it.
    sys.setrecursionlimit(max(sys.getrecursionlimit(), 100000))
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
    memory_sym = lower.memory_sym if lower.uses_memories else None
    Interp(lower.fns, memory_sym).call_fn(fn, [], ctx)
