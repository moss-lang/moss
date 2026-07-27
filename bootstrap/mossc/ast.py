"""AST for the grammar in docs/reference/syntax.md.

Nodes are plain dataclasses. `Path` is a list of names; a detached method
name is represented by `dot` fields carrying the name after the `.`.
"""

from dataclasses import dataclass, field, fields, is_dataclass


Path = list[str]


# Names and applications


@dataclass(frozen=True)
class Spec:
    path: Path | None  # None for a bare detached method like `.m`
    dot: str | None  # method name after `.`, if any
    app: "list[Binding] | None"  # None = unapplied; [] = empty brackets
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Binding:
    path: Path
    spec: Spec


@dataclass(frozen=True)
class AssumeItem:
    path: Path
    dot: str | None
    offset: int = field(default=-1, compare=False)


# Types


@dataclass(frozen=True)
class TyNever:
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class TyRef:
    path: Path
    app: list[Binding] | None
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class TyThis:
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class TyTuple:
    items: "list[Type]"  # [] is the unit type
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class TyRecord:
    fields: "list[tuple[str, Type]]"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class TyUnion:
    members: "list[Type]"
    offset: int = field(default=-1, compare=False)


Type = TyNever | TyRef | TyThis | TyTuple | TyRecord | TyUnion


# Files and declarations


@dataclass(frozen=True)
class UseName:
    dotted: bool
    name: str


@dataclass(frozen=True)
class UseItem:
    name: UseName
    alias: UseName | None
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Import:
    path: str
    alias: str | None
    glob: bool
    uses: list[UseItem]
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Assume:
    items: list[AssumeItem]
    decls: "list[Decl]"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Tydef:
    name: str
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Aliasdef:
    name: str
    ty: Type
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Tagdef:
    name: str
    ty: Type
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Unitdef:
    name: str
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Valdef:
    name: str
    ty: Type
    init: "Expr | None" = None  # a defined val (D50) when present
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class FnName:
    receiver: str | None  # attached method receiver
    dotted: bool  # detached method
    name: str


@dataclass(frozen=True)
class Param:
    name: str
    ty: Type


@dataclass(frozen=True)
class Fndef:
    name: FnName
    params: list[Param]
    ret: Type | None
    body: "Block | None"  # None = abstract (signature only)
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Ctxdef:
    name: str
    items: list[Spec]
    offset: int = field(default=-1, compare=False)


Decl = Assume | Tydef | Aliasdef | Tagdef | Unitdef | Valdef | Fndef | Ctxdef


@dataclass(frozen=True)
class File:
    imports: list[Import]
    decls: list[Decl]


# Statements and blocks


@dataclass(frozen=True)
class Let:
    name: str
    ty: Type | None
    expr: "Expr"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Var:
    name: str
    ty: Type | None
    expr: "Expr"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Assign:
    name: str
    expr: "Expr"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Bind:
    items: "list[tuple[Spec, Expr]]"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class While:
    cond: "Expr"
    body: "Block"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Loop:
    body: "Block"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class ExprStmt:
    expr: "Expr"
    offset: int = field(default=-1, compare=False)


Stmt = Let | Var | Assign | Bind | While | Loop | ExprStmt


@dataclass(frozen=True)
class Block:
    stmts: list[Stmt]
    tail: "Expr | None"


# Expressions


@dataclass(frozen=True)
class UnitExpr:
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class ThisExpr:
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class PathExpr:
    path: Path
    app: list[Binding] | None
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Call:
    callee: PathExpr
    args: "list[Expr]"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class RecordExpr:
    callee: PathExpr
    fields: "list[tuple[str, Expr | None]]"  # None value = shorthand
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Field:
    obj: "Expr"
    name: str
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class MethodCall:
    obj: "Expr"
    path: Path  # usually one name; qualified via :: per D44
    args: "list[Expr]"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class If:
    cond: "Expr"
    then: Block
    els: "Block | If | None"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Arm:
    pattern: "Pattern"
    body: "Expr | Block"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Match:
    scrutinee: "Expr"
    arms: list[Arm]
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Return:
    expr: "Expr | None"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class Break:
    offset: int = field(default=-1, compare=False)


Expr = (
    UnitExpr
    | ThisExpr
    | PathExpr
    | Call
    | RecordExpr
    | Field
    | MethodCall
    | If
    | Match
    | Return
    | Break
)


# Patterns


@dataclass(frozen=True)
class PatWild:
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class PatPath:
    path: Path  # binder or unit; resolution decides
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class PatTag:
    path: Path
    payload: "Pattern"
    offset: int = field(default=-1, compare=False)


@dataclass(frozen=True)
class PatRecord:
    path: Path | None
    fields: "list[tuple[str, Pattern | None]]"  # None value = shorthand
    offset: int = field(default=-1, compare=False)


Pattern = PatWild | PatPath | PatTag | PatRecord


def dump(node, indent: int = 0) -> str:
    """Stable, diffable S-expression-ish rendering of any AST node."""
    pad = "  " * indent
    if is_dataclass(node):
        parts = [type(node).__name__]
        for f in fields(node):
            if f.name == "offset":
                continue
            value = getattr(node, f.name)
            rendered = dump(value, indent + 1)
            parts.append(f"\n{pad}  {f.name}={rendered.lstrip()}")
        return f"{pad}({''.join(parts)})"
    if isinstance(node, list):
        if not node:
            return f"{pad}[]"
        inner = "\n".join(dump(item, indent + 1) for item in node)
        return f"{pad}[\n{inner}\n{pad}]"
    if isinstance(node, tuple):
        inner = " ".join(dump(item, 0) for item in node)
        return f"{pad}({inner})"
    return f"{pad}{node!r}"
