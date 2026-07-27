"""Core IR: the output of lowering, the input of the interpreter.

Context is explicit here (docs/design/semantics.md section 11): every
defined function carries the ordered list of its runtime-relevant needs,
and every use of a contextual val/fn/method references it by *key*. A key
is a Symbol for plain vals and fns, or a (receiver head, method Symbol)
pair for methods; the receiver head is the type-identity head of the
receiver (an abstract symbol, or a nominal symbol). At runtime the
environment is literally a mapping from these keys to values/callables —
`bind` executes by writing an entry, calls execute by reading the callee's
needs out of the caller's environment. Nothing downstream of lowering ever
sees an `assume` or a name again.

Types exist at runtime only as the nominal tags carried by values (needed
for `match`); all other typing was discharged statically.
"""

from dataclasses import dataclass

from .collect import Symbol

# Need keys: Symbol | tuple[Symbol receiver_head, Symbol method]
Key = object


@dataclass(frozen=True)
class FnIR:
    symbol: Symbol
    params: tuple  # names
    needs: tuple  # runtime-relevant Keys, in stable order
    body: "Block"
    has_this: bool = False


# Expressions


@dataclass(frozen=True)
class Unit:
    pass


@dataclass(frozen=True)
class Local:
    name: str


@dataclass(frozen=True)
class NeedVal:
    key: Key


@dataclass(frozen=True)
class Call:
    """callee is ('direct', FnIR-symbol) for defined functions or
    ('env', Key) for contextual functions and methods."""

    callee: tuple
    args: tuple
    this: "object | None" = None  # receiver expression for method calls


@dataclass(frozen=True)
class MakeUnit:
    symbol: Symbol


@dataclass(frozen=True)
class MakeTag:
    symbol: Symbol
    payload: object


@dataclass(frozen=True)
class MakeRecord:
    symbol: Symbol | None  # None for structural records
    fields: tuple  # (name, expr)


@dataclass(frozen=True)
class MakeTuple:
    items: tuple


@dataclass(frozen=True)
class Field:
    obj: object
    name: str


@dataclass(frozen=True)
class This:
    pass


# Patterns, compiled: test a nominal head, maybe destructure.


@dataclass(frozen=True)
class Pat:
    head: Symbol | None  # None matches anything (binder/wildcard)
    binder: str | None  # bind whole value (or payload for tag heads)
    fields: tuple | None  # (name, Pat|None-binder-name) for record payloads


@dataclass(frozen=True)
class MatchArm:
    pat: Pat
    body: object


@dataclass(frozen=True)
class Match:
    scrutinee: object
    arms: tuple


@dataclass(frozen=True)
class If:
    cond: object
    then: "Block"
    els: "Block | If | None"


@dataclass(frozen=True)
class Return:
    expr: object | None


@dataclass(frozen=True)
class Break:
    pass


# Statements


@dataclass(frozen=True)
class Let:
    name: str
    expr: object


@dataclass(frozen=True)
class Assign:
    name: str
    expr: object


@dataclass(frozen=True)
class BindVal:
    key: Key
    expr: object


@dataclass(frozen=True)
class BindFn:
    """Provide a contextual fn/method: a closure over the current env.

    A method bind may write several keys: binding `A.gimme` where `A` is an
    abstract symbol currently bound to `Char` provides the same atom under
    both `(A, gimme)` (how consumers elaborated under abstract `A` read it)
    and `(Char, gimme)` (how concrete-receiver consumers read it)."""

    keys: tuple  # one or more Keys
    fn: Symbol  # a defined FnIR symbol
    # Needs of `fn` are captured from the env at bind time.


@dataclass(frozen=True)
class While:
    cond: object
    body: "Block"


@dataclass(frozen=True)
class Loop:
    body: "Block"


@dataclass(frozen=True)
class ExprStmt:
    expr: object


@dataclass(frozen=True)
class Block:
    stmts: tuple
    tail: object | None
