"""Elaborated types (static only; values carry just nominal tags at runtime).

Type identity is structural over (declaration symbol, static type bindings)
per D18: `TNominal(Import, (TokenId, TInt))` from two different bind sites is
the same type. Symbols hash by identity (collect.Symbol has eq=False), which
is exactly nominal identity.
"""

from dataclasses import dataclass

from .collect import Symbol


@dataclass(frozen=True)
class TUnit:
    pass


@dataclass(frozen=True)
class TNever:
    pass


@dataclass(frozen=True)
class TTuple:
    items: tuple


@dataclass(frozen=True)
class TRecord:
    fields: tuple  # ordered (name, Type)


@dataclass(frozen=True)
class TUnion:
    members: tuple  # nominal/abstract types, declaration order, deduped


@dataclass(frozen=True)
class TNominal:
    symbol: Symbol  # UNIT or TAG
    args: tuple  # sorted ((Symbol, Type), ...) — static bindings, D18


@dataclass(frozen=True)
class TAbstract:
    symbol: Symbol  # TYPE symbol, still abstract in this region
    args: tuple


Type = object


def head(t) -> Symbol | None:
    if isinstance(t, (TNominal, TAbstract)):
        return t.symbol
    return None


def members(t) -> tuple:
    if isinstance(t, TUnion):
        return t.members
    if isinstance(t, TNever):
        return ()
    return (t,)


def is_member(t, union) -> bool:
    return any(t == m for m in members(union))


def fits(t, expected) -> bool:
    """t is acceptable where expected is required: equality, divergence, or
    injection into a union (D17)."""
    if t == expected or isinstance(t, TNever):
        return True
    if isinstance(expected, TUnion):
        return all(is_member(m, expected) for m in members(t))
    return False


def show(t) -> str:
    if isinstance(t, TUnit):
        return "()"
    if isinstance(t, TNever):
        return "|"
    if isinstance(t, TTuple):
        return "(" + ", ".join(show(i) for i in t.items) + ")"
    if isinstance(t, TRecord):
        inner = ", ".join(f"{n}: {show(ty)}" for n, ty in t.fields)
        return "{" + inner + "}"
    if isinstance(t, TUnion):
        return " | ".join(show(m) for m in t.members)
    if isinstance(t, (TNominal, TAbstract)):
        if t.args:
            inner = ", ".join(f"{s.name}={show(ty)}" for s, ty in t.args)
            return f"{t.symbol.name}[{inner}]"
        return t.symbol.name
    return repr(t)
