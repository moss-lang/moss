"""A compact rendering of the AST, shared with the self-hosted parser.

`ast.dump` is for humans. This is for comparison: `src/syntax.moss` builds
its own arenas and `src/dump.moss` writes them out in exactly this format,
so the two parsers can be checked against each other over the whole
corpus. The tags are single characters because the self-hosted side has
no string literals (D48) — a letter costs one `putchar`, a word costs a
function.

The letters are the node kinds of src/ast.moss in order: index 0-25 are
'a'-'z' and 26-47 are 'A'-'V'.
"""

from . import ast

KINDS = [
    "ty_unit", "ty_never", "ty_this", "ty_ref", "ty_tuple", "ty_record",
    "ty_union", "ty_field", "ex_unit", "ex_this", "ex_path", "ex_call",
    "ex_field", "ex_method", "ex_record", "ex_rfield", "ex_if", "ex_match",
    "ex_return", "ex_break", "ex_block", "st_let", "st_var", "st_assign",
    "st_bind", "st_item", "st_while", "st_loop", "st_expr", "pt_wild",
    "pt_path", "pt_record", "pt_field", "arm", "spec", "binding", "param",
    "d_import", "d_assume", "d_type", "d_alias", "d_tag", "d_unit", "d_val",
    "d_fn", "d_context", "d_functor", "d_bad",
]
TAG = {
    name: (chr(ord("a") + i) if i < 26 else chr(ord("A") + i - 26))
    for i, name in enumerate(KINDS)
}


def _node(kind, *children):
    return "(" + " ".join([TAG[kind], *children]) + ")"


def _list(items):
    return "[" + " ".join(items) + "]"


def _opt(value, render):
    return "_" if value is None else render(value)


def _path(path):
    return "::".join(path)


def _app(app):
    if app is None:
        return "_"
    return _list([_node("binding", _path(b.path), _spec(b.spec)) for b in app])


def _spec(s):
    return _node(
        "spec", _opt(s.path, _path), _opt(s.dot, str), _app(s.app)
    )


def _type(t):
    if isinstance(t, ast.TyNever):
        return _node("ty_never")
    if isinstance(t, ast.TyThis):
        return _node("ty_this")
    if isinstance(t, ast.TyTuple):
        if not t.items:
            return _node("ty_unit")
        return _node("ty_tuple", _list([_type(i) for i in t.items]))
    if isinstance(t, ast.TyRecord):
        return _node(
            "ty_record",
            _list([_node("ty_field", n, _type(ty)) for n, ty in t.fields]),
        )
    if isinstance(t, ast.TyUnion):
        return _node("ty_union", _list([_type(m) for m in t.members]))
    if isinstance(t, ast.TyRef):
        return _node("ty_ref", _path(t.path), _app(t.app))
    raise AssertionError(f"unknown type node {t!r}")


def _expr(e):
    if isinstance(e, ast.UnitExpr):
        return _node("ex_unit")
    if isinstance(e, ast.ThisExpr):
        return _node("ex_this")
    if isinstance(e, ast.PathExpr):
        return _node("ex_path", _path(e.path), _app(e.app))
    if isinstance(e, ast.Call):
        return _node("ex_call", _expr(e.callee), _list([_expr(a) for a in e.args]))
    if isinstance(e, ast.Field):
        return _node("ex_field", _expr(e.obj), e.name)
    if isinstance(e, ast.MethodCall):
        return _node(
            "ex_method",
            _expr(e.obj),
            _path(e.path),
            _list([_expr(a) for a in e.args]),
        )
    if isinstance(e, ast.RecordExpr):
        return _node(
            "ex_record",
            _expr(e.callee),
            _list(
                [
                    _node("ex_rfield", n, _opt(v, _expr))
                    for n, v in e.fields
                ]
            ),
        )
    if isinstance(e, ast.If):
        return _node("ex_if", _expr(e.cond), _block(e.then), _opt(e.els, _else))
    if isinstance(e, ast.Match):
        return _node(
            "ex_match",
            _expr(e.scrutinee),
            _list(
                [_node("arm", _pattern(a.pattern), _arm_body(a.body)) for a in e.arms]
            ),
        )
    if isinstance(e, ast.Return):
        return _node("ex_return", _opt(e.expr, _expr))
    if isinstance(e, ast.Break):
        return _node("ex_break")
    if isinstance(e, ast.Block):
        return _block(e)
    raise AssertionError(f"unknown expression node {e!r}")


def _else(e):
    return _block(e) if isinstance(e, ast.Block) else _expr(e)


def _arm_body(b):
    return _block(b) if isinstance(b, ast.Block) else _expr(b)


def _pattern(p):
    if isinstance(p, ast.PatWild):
        return _node("pt_wild")
    if isinstance(p, ast.PatPath):
        return _node("pt_path", _path(p.path), "_")
    if isinstance(p, ast.PatTag):
        return _node("pt_path", _path(p.path), _pattern(p.payload))
    if isinstance(p, ast.PatRecord):
        return _node(
            "pt_record",
            _opt(p.path, _path),
            _list(
                [_node("pt_field", n, _opt(v, _pattern)) for n, v in p.fields]
            ),
        )
    raise AssertionError(f"unknown pattern node {p!r}")


def _block(b):
    return _node(
        "ex_block",
        _list([_stmt(s) for s in b.stmts]),
        _opt(b.tail, _expr),
    )


def _bind(b):
    return _node(
        "st_bind",
        _list([_node("st_item", _spec(s), _opt(v, _expr)) for s, v in b.items]),
    )


def _stmt(s):
    if isinstance(s, ast.Let):
        return _node("st_let", s.name, _opt(s.ty, _type), _expr(s.expr))
    if isinstance(s, ast.Var):
        return _node("st_var", s.name, _opt(s.ty, _type), _expr(s.expr))
    if isinstance(s, ast.Assign):
        return _node("st_assign", s.name, _expr(s.expr))
    if isinstance(s, ast.Bind):
        return _bind(s)
    if isinstance(s, ast.While):
        return _node("st_while", _expr(s.cond), _block(s.body))
    if isinstance(s, ast.Loop):
        return _node("st_loop", _block(s.body))
    if isinstance(s, ast.ExprStmt):
        return _node("st_expr", _expr(s.expr))
    raise AssertionError(f"unknown statement node {s!r}")


def _use(item):
    def side(u):
        return ("." if u.dotted else "") + u.name

    text = side(item.name)
    if item.alias is not None:
        text += ">" + side(item.alias)
    return text


def _import(i):
    return _node(
        "d_import",
        i.path,
        _opt(i.alias, str),
        "*" if i.glob else "_",
        _list([_use(u) for u in i.uses]),
    )


def _decl(d):
    if isinstance(d, ast.Assume):
        return _node(
            "d_assume",
            _list(
                [
                    _node("spec", _path(i.path), _opt(i.dot, str), "_")
                    for i in d.items
                ]
            ),
            _list([_decl(x) for x in d.decls]),
        )
    if isinstance(d, ast.Tydef):
        return _node("d_type", d.name)
    if isinstance(d, ast.Aliasdef):
        return _node("d_alias", d.name, _type(d.ty))
    if isinstance(d, ast.Tagdef):
        return _node("d_tag", d.name, _type(d.ty))
    if isinstance(d, ast.Unitdef):
        return _node("d_unit", d.name)
    if isinstance(d, ast.Valdef):
        return _node("d_val", d.name, _type(d.ty), _opt(d.init, _expr))
    if isinstance(d, ast.Fndef):
        receiver = "." if d.name.dotted else _opt(d.name.receiver, str)
        return _node(
            "d_fn",
            receiver,
            d.name.name,
            _list([_node("param", p.name, _type(p.ty)) for p in d.params]),
            _opt(d.ret, _type),
            _opt(d.body, _block),
        )
    if isinstance(d, ast.Ctxdef):
        return _node("d_context", d.name, _list([_spec(s) for s in d.items]))
    if isinstance(d, ast.Functordef):
        return _node(
            "d_functor",
            d.name,
            _list([_spec(s) for s in d.args]),
            _list([_spec(s) for s in d.result]),
            _list([_bind(b) for b in d.binds]),
        )
    raise AssertionError(f"unknown declaration node {d!r}")


def dump(file: ast.File) -> str:
    """The whole file: its imports, then its other declarations."""
    return (
        "($ "
        + _list([_import(i) for i in file.imports])
        + " "
        + _list([_decl(d) for d in file.decls])
        + ")"
    )
