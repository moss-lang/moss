"""AST for the grammar in docs/reference/syntax.md.

Nodes are plain dataclasses. `Path` is a list of names; a detached method
name is represented by `dot` fields carrying the name after the `.`.
"""

from dataclasses import dataclass, fields, is_dataclass


Path = list[str]


# Names and applications


@dataclass(frozen=True)
class Spec:
    path: Path | None  # None for a bare detached method like `.m`
    dot: str | None  # method name after `.`, if any
    app: "list[Binding] | None"  # None = unapplied; [] = empty brackets


@dataclass(frozen=True)
class Binding:
    path: Path
    spec: Spec


@dataclass(frozen=True)
class AssumeItem:
    path: Path
    dot: str | None


# Types


@dataclass(frozen=True)
class TyNever:
    pass


@dataclass(frozen=True)
class TyRef:
    path: Path
    app: list[Binding] | None


@dataclass(frozen=True)
class TyThis:
    pass


@dataclass(frozen=True)
class TyTuple:
    items: "list[Type]"  # [] is the unit type


@dataclass(frozen=True)
class TyRecord:
    fields: "list[tuple[str, Type]]"


@dataclass(frozen=True)
class TyUnion:
    members: "list[Type]"


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


@dataclass(frozen=True)
class Import:
    path: str
    alias: str | None
    glob: bool
    uses: list[UseItem]


@dataclass(frozen=True)
class Assume:
    items: list[AssumeItem]
    decls: "list[Decl]"


@dataclass(frozen=True)
class Tydef:
    name: str


@dataclass(frozen=True)
class Aliasdef:
    name: str
    ty: Type


@dataclass(frozen=True)
class Tagdef:
    name: str
    ty: Type


@dataclass(frozen=True)
class Unitdef:
    name: str


@dataclass(frozen=True)
class Valdef:
    name: str
    ty: Type


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


@dataclass(frozen=True)
class Ctxdef:
    name: str
    items: list[Spec]


Decl = Assume | Tydef | Aliasdef | Tagdef | Unitdef | Valdef | Fndef | Ctxdef


@dataclass(frozen=True)
class File:
    imports: list[Import]
    decls: list[Decl]


# Statements and blocks


@dataclass(frozen=True)
class Let:
    name: str
    expr: "Expr"


@dataclass(frozen=True)
class Var:
    name: str
    expr: "Expr"


@dataclass(frozen=True)
class Assign:
    name: str
    expr: "Expr"


@dataclass(frozen=True)
class Bind:
    items: "list[tuple[Spec, Expr]]"


@dataclass(frozen=True)
class While:
    cond: "Expr"
    body: "Block"


@dataclass(frozen=True)
class Loop:
    body: "Block"


@dataclass(frozen=True)
class ExprStmt:
    expr: "Expr"


Stmt = Let | Var | Assign | Bind | While | Loop | ExprStmt


@dataclass(frozen=True)
class Block:
    stmts: list[Stmt]
    tail: "Expr | None"


# Expressions


@dataclass(frozen=True)
class UnitExpr:
    pass


@dataclass(frozen=True)
class ThisExpr:
    pass


@dataclass(frozen=True)
class PathExpr:
    path: Path
    app: list[Binding] | None


@dataclass(frozen=True)
class Call:
    callee: PathExpr
    args: "list[Expr]"


@dataclass(frozen=True)
class RecordExpr:
    callee: PathExpr
    fields: "list[tuple[str, Expr | None]]"  # None value = shorthand


@dataclass(frozen=True)
class Field:
    obj: "Expr"
    name: str


@dataclass(frozen=True)
class MethodCall:
    obj: "Expr"
    path: Path  # usually one name; qualified via :: per D44
    args: "list[Expr]"


@dataclass(frozen=True)
class If:
    cond: "Expr"
    then: Block
    els: "Block | If | None"


@dataclass(frozen=True)
class Arm:
    pattern: "Pattern"
    body: "Expr | Block"


@dataclass(frozen=True)
class Match:
    scrutinee: "Expr"
    arms: list[Arm]


@dataclass(frozen=True)
class Return:
    expr: "Expr | None"


@dataclass(frozen=True)
class Break:
    pass


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
    pass


@dataclass(frozen=True)
class PatPath:
    path: Path  # binder or unit; resolution decides


@dataclass(frozen=True)
class PatTag:
    path: Path
    payload: "Pattern"


@dataclass(frozen=True)
class PatRecord:
    path: Path | None
    fields: "list[tuple[str, Pattern | None]]"  # None value = shorthand


Pattern = PatWild | PatPath | PatTag | PatRecord


def dump(node, indent: int = 0) -> str:
    """Stable, diffable S-expression-ish rendering of any AST node."""
    pad = "  " * indent
    if is_dataclass(node):
        parts = [type(node).__name__]
        for f in fields(node):
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
