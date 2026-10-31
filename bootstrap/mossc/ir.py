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

# Need keys: Symbol | tuple[receiver type, Symbol method]. The receiver is
# the canonical type the provision was made at — declaration *and* static
# bindings (D18, D62) — so `Box[Elem=Char].peek` and `Box[Elem=Point].peek`
# are two keys.
Key = object


@dataclass(frozen=True)
class FnIR:
    symbol: Symbol
    params: tuple  # names
    needs: tuple  # runtime-relevant Keys, in stable order
    body: "Block"
    has_this: bool = False
    # D59: how many scalars each value occupies. `param_slots` covers
    # `this` first when there is one, then the parameters in order.
    param_slots: tuple = ()
    ret_slots: int = 1


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
    ('env', Key) for contextual functions and methods. For direct calls,
    needs_map is the (callee key, caller key) translation computed at the
    call site — the callee may know a receiver by an abstract symbol the
    caller has since bound or merged (D43)."""

    callee: tuple
    args: tuple
    this: "object | None" = None  # receiver expression for method calls
    needs_map: tuple | None = None
    # How many scalars the caller expects back, from the callee's
    # signature instantiated where it is called. The callee was lowered
    # once, at its declaration, so the back end checks the two agree.
    ret_slots: int | None = None
    # A direct call's type arguments: for each type symbol the callee
    # assumes, its parameter and the type the caller supplies for it, in
    # the caller's terms. The back end composes them with its own, so a
    # specialization knows what its abstract types are where that decides
    # a valtype (D75: a reference's type is the struct of what it holds).
    tyargs: tuple | None = None
    # D79: for a memory instruction, the `Memory` in force at the call, in
    # the caller's terms; which memory it is, is that type at the
    # specialization's (or the interpreter frame's) type arguments.
    memory: object = None


@dataclass(frozen=True)
class MakeUnit:
    symbol: Symbol


@dataclass(frozen=True)
class MakeTag:
    symbol: Symbol
    payload: object
    # The tag's type, recorded only for D75's `Ref`, whose construction
    # from a bare `StructRef` is a cast to the struct of its `Boxed`.
    ty: object = None


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
    # Which scalars of the record the field occupies (D59), set by
    # lowering: `width` of them, starting at `offset`.
    offset: int = -1
    width: int = 1


@dataclass(frozen=True)
class This:
    pass


# Patterns, compiled: test a nominal head, maybe destructure.


@dataclass(frozen=True)
class Pat:
    head: Symbol | None  # None matches anything (binder/wildcard)
    binder: str | None  # bind whole value (or payload for tag heads)
    # (name, binder, offset, width) for record payloads: which scalars of
    # the payload each binder takes.
    fields: tuple | None
    # How many scalars a tag pattern's payload occupies, for its binder.
    width: int = 1


def pat_binders(pat: Pat) -> list:
    """The names a pattern binds."""
    names = [pat.binder] if pat.binder is not None else []
    if pat.fields is not None:
        names.extend(binder for _, binder, *_ in pat.fields)
    return names


@dataclass(frozen=True)
class MatchArm:
    pat: Pat
    body: object


@dataclass(frozen=True)
class Match:
    scrutinee: object
    arms: tuple
    # Whether the scrutinee's static type has more than one possible head.
    # If not, nothing needs discriminating and the first arm always wins.
    tagged: bool = True


@dataclass(frozen=True)
class Inject:
    """D58: a value entering a union. Representation follows the static
    type — a nominal value is just its payload — so this is the one place
    a tag is attached, at the injection D17 allows."""

    symbol: object
    value: object
    # How wide the union is: one scalar when every member is a unit (the
    # code discriminates by itself), otherwise a discriminant plus payload.
    width: int = 2
    # The union itself, so the back end can pad a narrower member with
    # zeros of the union's own valtypes (a reference's is `ref.null`).
    union: object = None


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
    """Provide a contextual fn/method under its canonical key (D43): a
    closure over the pieces of the current env the provider needs, gathered
    through needs_map exactly like a direct call."""

    key: Key
    fn: Symbol  # a defined FnIR symbol
    needs_map: tuple
    # The provider's type arguments, as `Call.tyargs`; None for an
    # anonymous function (D70), which is lowered in the bind site's own
    # terms and so shares its type arguments.
    tyargs: tuple | None = None


@dataclass(frozen=True)
class BindPrim:
    """Provide a method under its key with a primitive the platform
    supplies (D75's `bind Heap;`): `"box"` is `struct.new` and `"deref"`
    a `struct.get` per field, of the struct for `held`, the type `Boxed`
    was at the bind, in the binding function's terms. Nothing is
    captured, so a call through the key compiles to the instructions in
    place."""

    key: Key
    prim: str
    held: object


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
