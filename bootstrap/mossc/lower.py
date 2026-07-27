"""Lower: elaborate a collected Program into the core IR.

This implements the static semantics (docs/design/semantics.md section 11,
stage 4): requirement environments from `assume` nesting, context
flattening, the D22 totality rule, the D26-D28 bind rules, forward type
inference, method resolution (D36), match exhaustiveness (D34), and the
D39 scope-versus-context error distinction. Every defined function is
elaborated once, under its own assumptions (the interpreter carries context
at runtime; monomorphization is a later backend's job).

Deliberate v0 simplifications, each marked TODO in place:
- D43 consistent merging is equality-only (conflicting duplicate keys are
  errors instead of unifying atoms).
- Square-bracket applications bind type symbols only.
- Attached-method lookup searches the receiver's home module and the
  calling module (no orphan-style global search).
"""

from dataclasses import dataclass

from . import ast, ir
from .collect import CollectError, Module, Program, SymKind, Symbol, resolve_detached, resolve_path
from .types import (
    TAbstract,
    TNever,
    TNominal,
    TRecord,
    TTuple,
    TUnion,
    TUnit,
    fits,
    head,
    members,
    show,
)


class LowerError(Exception):
    def __init__(self, module: str, message: str):
        super().__init__(f"{module}: {message}")
        self.module = module
        self.message = message


@dataclass(frozen=True)
class FnSig:
    params: tuple  # (name, Type)
    ret: object
    this: object | None = None


class Env:
    """What a region has: type bindings plus val/fn/method provisions."""

    def __init__(self, module: Module, parent: "Env | None" = None):
        self.module = module
        if parent is None:
            self.tymap = {}
            self.vals = {}
            self.fns = {}
            self.methods = {}
            self.locals = {}
        else:
            self.tymap = dict(parent.tymap)
            self.vals = dict(parent.vals)
            self.fns = dict(parent.fns)
            self.methods = dict(parent.methods)
            self.locals = dict(parent.locals)


class Lower:
    def __init__(self, program: Program):
        self.program = program
        self.fns: dict[int, ir.FnIR] = {}  # id(Symbol) -> FnIR
        self.in_progress: set[int] = set()
        self.fn_symbols: dict[int, Symbol] = {}  # id(decl) -> Symbol
        self.needs_of: dict[int, tuple] = {}  # id(Symbol) -> runtime need keys
        self._reqs_cache: dict[int, list] = {}
        for module in program.order:
            for table in (module.names, module.detached, module.attached):
                for symbol in table.values():
                    self.fn_symbols[id(symbol.decl)] = symbol
        self.bool_true, self.bool_false = self.find_bool()

    def error(self, module: Module, message: str):
        raise LowerError(module.path, message)

    def find_bool(self):
        for module in self.program.order:
            if module.path.endswith("lib/bool.moss"):
                return module.names.get("True"), module.names.get("False")
        return None, None

    def run(self) -> None:
        for module in self.program.order:
            for decl, symbol in self.defined_fns(module):
                self.lower_fn(module, symbol)

    def defined_fns(self, module: Module):
        for table in (module.names, module.attached):
            for symbol in table.values():
                if (
                    symbol.kind in (SymKind.FN, SymKind.METHOD)
                    and isinstance(symbol.decl, ast.Fndef)
                    and symbol.decl.body is not None
                ):
                    yield symbol.decl, symbol

    # Requirements and environments

    def base_env(self, symbol: Symbol) -> tuple[Env, list]:
        """The ambient environment of a declaration: everything its enclosing
        assume blocks provide, in order. Returns (env, runtime need keys)."""
        module = symbol.module
        env = Env(module)
        needs: list = []
        for item in symbol.assumes:
            self.add_assume_item(env, module, item, needs)
        return env, needs

    def add_assume_item(self, env: Env, module: Module, item: ast.AssumeItem, needs: list):
        if item.dot is not None:
            receiver = self.resolve_type_symbol(env, module, item.path)
            self.add_method_item(env, module, receiver, item.dot, {}, needs)
            return
        target = resolve_path(module, item.path)
        if target is None:
            self.error(module, f"`{'::'.join(item.path)}` is not in scope")
        if isinstance(target, Module):
            self.error(module, f"cannot assume a module (`{'::'.join(item.path)}`)")
        self.add_symbol_item(env, module, target, {}, needs)

    def add_symbol_item(self, env: Env, module: Module, sym: Symbol, subst: dict, needs: list):
        self.check_totality(module, sym, subst, env)
        if sym.kind == SymKind.TYPE:
            if sym not in env.tymap:
                env.tymap[sym] = TAbstract(sym, self.nominal_args(module, sym, env.tymap | subst))
        elif sym.kind == SymKind.VAL:
            ty = self.elab_type(sym.decl.ty, sym.module, env.tymap | subst, env)
            self.merge(env.vals, module, sym, ty)
            if sym not in needs:
                needs.append(sym)
        elif sym.kind == SymKind.FN:
            sig = self.fn_sig(sym, env.tymap | subst, env)
            self.merge(env.fns, module, sym, sig)
            if sym not in needs:
                needs.append(sym)
        elif sym.kind == SymKind.CONTEXT:
            for spec in sym.decl.items:
                self.add_spec(env, sym.module, spec, outer_subst=subst, needs=needs)
        elif sym.kind in (SymKind.UNIT, SymKind.TAG, SymKind.ALIAS):
            pass  # concrete; always available
        else:
            self.error(module, f"cannot assume `{sym.name}` (a {sym.kind.name.lower()})")

    def add_spec(self, env: Env, module: Module, spec: ast.Spec, outer_subst: dict, needs: list):
        subst = dict(outer_subst)
        if spec.app is not None:
            subst.update(self.app_subst(env, module, spec.app, outer_subst))
        if spec.dot is not None:
            if spec.path is None:
                self.error(module, "a context item needs a receiver for a detached method")
            receiver = self.resolve_type_symbol(env, module, spec.path)
            self.add_method_item(env, module, receiver, spec.dot, subst, needs)
            return
        target = resolve_path(module, spec.path)
        if target is None:
            self.error(module, f"`{'::'.join(spec.path)}` is not in scope")
        if isinstance(target, Module):
            self.error(module, f"cannot put a module in a context")
        self.add_symbol_item(env, module, target, subst, needs)

    def add_method_item(
        self, env: Env, module: Module, receiver: Symbol, name: str, subst: dict, needs: list
    ):
        method = self.find_method_decl(module, receiver, name)
        if method is None:
            self.error(
                module, f"no method `.{name}` for `{receiver.name}` is in scope"
            )
        tymap = env.tymap | subst
        this_ty = tymap.get(receiver)
        if this_ty is None:
            this_ty = self.symbol_type(module, receiver, tymap, env)
        sig = self.fn_sig(method, tymap, env, this=this_ty)
        key = (receiver, method)
        self.merge(env.methods, module, key, sig)
        if key not in needs:
            needs.append(key)

    def find_method_decl(self, module: Module, receiver: Symbol, name: str) -> Symbol | None:
        """An abstract attached method on the receiver, or a detached method
        in scope — falling back to the receiver's home module, so that
        `CellInt.read` in a context resolves to the `.read` declared next to
        `CellInt` without the context's module importing it. TODO: global
        attached search is deliberately not done."""
        for home in (receiver.module, module):
            attached = home.attached.get((id(receiver), name))
            if attached is not None:
                return attached
        found = module.detached.get(name)
        if found is None:
            found = receiver.module.detached.get(name)
        return found

    def merge(self, table: dict, module: Module, key, value):
        # TODO(D43): unify atoms instead of requiring equality.
        existing = table.get(key)
        if existing is not None and existing != value:
            self.error(module, f"conflicting provisions for `{key}` (D43 merge unimplemented)")
        table[key] = value

    def check_totality(self, module: Module, sym: Symbol, subst: dict, env: Env):
        reqs = self.type_requirements(sym)
        if subst:
            missing = [r.name for r in reqs if r not in subst and r not in env.tymap]
            if missing:
                self.error(
                    module,
                    f"application of `{sym.name}` must bind all requirements (D22); "
                    f"missing {missing}",
                )

    def type_requirements(self, sym: Symbol) -> list:
        """The type symbols that parameterize a declaration (D18/D21).

        For nominal types and aliases these are the abstract type symbols
        their payload actually *mentions* — not everything the enclosing
        assumes happen to provide, which would make `unit Stop;` under
        `assume Std` spuriously distinct per ambient binding. For abstract
        types, functions, vals, and contexts (which have no payload to
        scan), the assume nesting is the answer.
        """
        cached = self._reqs_cache.get(id(sym))
        if cached is not None:
            return cached
        out: list = []
        # Register before scanning so recursive types terminate.
        self._reqs_cache[id(sym)] = out
        if sym.kind == SymKind.UNIT:
            pass
        elif sym.kind in (SymKind.TAG, SymKind.ALIAS):
            self.free_type_syms(sym.decl.ty, sym.module, out, {id(sym)})
        else:
            self.assume_type_syms(sym, out)
        return out

    def assume_type_syms(self, sym: Symbol, out: list):
        def walk_items(module, items):
            for item in items:
                if item.dot is not None:
                    continue
                target = resolve_path(module, item.path)
                if isinstance(target, Symbol):
                    if target.kind == SymKind.TYPE and target not in out:
                        out.append(target)
                    elif target.kind == SymKind.CONTEXT:
                        walk_specs(target.module, target.decl.items)

        def walk_specs(module, specs):
            for spec in specs:
                if spec.dot is not None or spec.path is None:
                    continue
                target = resolve_path(module, spec.path)
                if isinstance(target, Symbol):
                    if target.kind == SymKind.TYPE and target not in out:
                        out.append(target)
                    elif target.kind == SymKind.CONTEXT:
                        walk_specs(target.module, target.decl.items)

        walk_items(sym.module, sym.assumes)

    def free_type_syms(self, ty, module: Module, out: list, visiting: set):
        if isinstance(ty, (ast.TyNever, ast.TyThis)):
            return
        if isinstance(ty, ast.TyTuple):
            for item in ty.items:
                self.free_type_syms(item, module, out, visiting)
            return
        if isinstance(ty, ast.TyRecord):
            for _, field_ty in ty.fields:
                self.free_type_syms(field_ty, module, out, visiting)
            return
        if isinstance(ty, ast.TyUnion):
            for member in ty.members:
                self.free_type_syms(member, module, out, visiting)
            return
        if isinstance(ty, ast.TyRef):
            target = resolve_path(module, ty.path)
            if not isinstance(target, Symbol):
                return  # scope errors surface during elaboration
            bound = set()
            if ty.app is not None:
                for binding in ty.app:
                    lhs = resolve_path(module, binding.path)
                    if isinstance(lhs, Symbol):
                        bound.add(lhs)
                    if binding.spec.path is not None:
                        self.free_type_syms(
                            ast.TyRef(binding.spec.path, binding.spec.app),
                            module,
                            out,
                            visiting,
                        )
            if target.kind == SymKind.TYPE:
                if target not in bound and target not in out:
                    out.append(target)
                return
            if id(target) in visiting:
                return
            for req in self.type_requirements(target):
                if req not in bound and req not in out:
                    out.append(req)

    def app_subst(self, env: Env, module: Module, app: list, outer: dict) -> dict:
        subst = {}
        for binding in app:
            target = resolve_path(env.module, binding.path)
            if not isinstance(target, Symbol) or target.kind != SymKind.TYPE:
                self.error(
                    env.module,
                    f"`{'::'.join(binding.path)}` in a bracket binding must be an "
                    "abstract type symbol",
                )
            if binding.spec.dot is not None or binding.spec.path is None:
                self.error(env.module, "bracket bindings bind types in v0")
            ty = self.elab_type(
                ast.TyRef(binding.spec.path, binding.spec.app),
                env.module,
                env.tymap | outer,
                env,
            )
            subst[target] = ty
        return subst

    # Types

    def resolve_type_symbol(self, env: Env, module: Module, path: ast.Path) -> Symbol:
        target = resolve_path(module, path)
        if target is None:
            self.error(module, f"`{'::'.join(path)}` is not in scope")
        if not isinstance(target, Symbol) or target.kind not in (
            SymKind.TYPE,
            SymKind.UNIT,
            SymKind.TAG,
            SymKind.ALIAS,
        ):
            self.error(module, f"`{'::'.join(path)}` is not a type")
        return target

    def nominal_args(self, module: Module, sym: Symbol, tymap: dict) -> tuple:
        args = []
        for req in self.type_requirements(sym):
            ty = tymap.get(req)
            if ty is None:
                self.error(
                    module,
                    f"type `{req.name}` (required by `{sym.name}`) is not available "
                    "in the context here",
                )
            args.append((req, ty))
        return tuple(args)

    def symbol_type(self, module: Module, sym: Symbol, tymap: dict, env: Env):
        if sym.kind == SymKind.TYPE:
            ty = tymap.get(sym)
            if ty is None:
                self.error(
                    module, f"type `{sym.name}` is not available in the context here"
                )
            return ty
        if sym.kind in (SymKind.UNIT, SymKind.TAG):
            return TNominal(sym, self.nominal_args(module, sym, tymap))
        if sym.kind == SymKind.ALIAS:
            return self.elab_type(sym.decl.ty, sym.module, tymap, env)
        self.error(module, f"`{sym.name}` is not a type")

    def elab_type(self, ty, module: Module, tymap: dict, env: Env, this=None):
        if isinstance(ty, ast.TyNever):
            return TNever()
        if isinstance(ty, ast.TyThis):
            if this is None:
                self.error(module, "`This` is only legal inside method declarations")
            return this
        if isinstance(ty, ast.TyTuple):
            if not ty.items:
                return TUnit()
            return TTuple(tuple(self.elab_type(i, module, tymap, env, this) for i in ty.items))
        if isinstance(ty, ast.TyRecord):
            return TRecord(
                tuple((n, self.elab_type(t, module, tymap, env, this)) for n, t in ty.fields)
            )
        if isinstance(ty, ast.TyUnion):
            out = []
            for member in ty.members:
                elaborated = self.elab_type(member, module, tymap, env, this)
                for m in members(elaborated):
                    if isinstance(m, TAbstract):
                        self.error(
                            module,
                            f"union member `{show(m)}` is abstract and not constrained "
                            "to a nominal type (D15)",
                        )
                    if not isinstance(m, TNominal):
                        self.error(module, f"union member `{show(m)}` is not nominal (D15)")
                    if m in out:
                        self.error(module, f"duplicate union member `{show(m)}`")
                    out.append(m)
            return TUnion(tuple(out))
        if isinstance(ty, ast.TyRef):
            target = self.resolve_type_symbol(env, module, ty.path)
            tymap = tymap if ty.app is None else tymap | self.app_subst(env, module, ty.app, tymap)
            if ty.app is not None:
                self.check_totality(module, target, dict(tymap), env)
            return self.symbol_type(module, target, tymap, env)
        self.error(module, f"unsupported type {ty!r}")

    def payload_type(self, tag: TNominal, env: Env):
        tymap = dict(tag.args)
        return self.elab_type(tag.symbol.decl.ty, tag.symbol.module, tymap, env)

    def fn_sig(self, sym: Symbol, tymap: dict, env: Env, this=None) -> FnSig:
        decl = sym.decl
        params = tuple(
            (p.name, self.elab_type(p.ty, sym.module, tymap, env, this)) for p in decl.params
        )
        ret = (
            TUnit()
            if decl.ret is None
            else self.elab_type(decl.ret, sym.module, tymap, env, this)
        )
        return FnSig(params, ret, this)

    # Functions

    def lower_fn(self, module: Module, symbol: Symbol) -> None:
        """Elaborate one defined function. Its needs are derivable from the
        assume nesting alone and get registered *before* the body is checked,
        so recursive and mutually recursive calls terminate."""
        if id(symbol) in self.fns or id(symbol) in self.in_progress:
            return
        self.in_progress.add(id(symbol))
        env, needs = self.base_env(symbol)
        self.needs_of[id(symbol)] = tuple(needs)
        decl = symbol.decl
        has_this = symbol.receiver is not None
        this_ty = None
        if has_this:
            if symbol.receiver.kind not in (SymKind.UNIT, SymKind.TAG):
                self.error(
                    module,
                    f"attached method `{symbol.name}` requires a nominal receiver (Q5); "
                    f"`{symbol.receiver.name}` is abstract",
                )
            this_ty = TNominal(
                symbol.receiver, self.nominal_args(module, symbol.receiver, env.tymap)
            )
        sig = self.fn_sig(symbol, env.tymap, env, this=this_ty)
        checker = FnChecker(self, module, env, sig, needs, this_ty)
        body = checker.check_block(decl.body, expected=sig.ret)
        fn = ir.FnIR(symbol, tuple(n for n, _ in sig.params), tuple(needs), body, has_this)
        self.fns[id(symbol)] = fn
        self.in_progress.discard(id(symbol))

    def flatten_context(self, symbol: Symbol) -> tuple[Env, list]:
        """Public: the environment/needs a context provides (for linking)."""
        env = Env(symbol.module)
        needs: list = []
        self.add_symbol_item(env, symbol.module, symbol, {}, needs)
        return env, needs


class FnChecker:
    """Type-checks one function body and produces its IR."""

    def __init__(self, lower: Lower, module: Module, env: Env, sig: FnSig, needs, this_ty):
        self.lower = lower
        self.module = module
        self.env = env
        self.sig = sig
        self.needs = needs
        self.this_ty = this_ty
        self.loop_depth = 0
        for name, ty in sig.params:
            env.locals[name] = (ty, False)

    def error(self, message: str):
        raise LowerError(self.module.path, message)

    def push(self) -> Env:
        outer = self.env
        self.env = Env(self.module, parent=outer)
        return outer

    # Blocks and statements

    def check_block(self, block: ast.Block, expected=None) -> ir.Block:
        outer = self.push()
        try:
            stmts = []
            for stmt in block.stmts:
                checked = self.check_stmt(stmt)
                # Binds flatten into the enclosing block: wrapping them in a
                # nested ir.Block would scope them to themselves.
                if isinstance(checked, list):
                    stmts.extend(checked)
                else:
                    stmts.append(checked)
            if block.tail is None:
                tail = None
                tail_ty = TUnit()
            else:
                tail, tail_ty = self.synth(block.tail, expected=expected)
            if expected is not None and not fits(tail_ty, expected):
                self.error(f"block yields `{show(tail_ty)}`, expected `{show(expected)}`")
            return ir.Block(tuple(stmts), tail)
        finally:
            self.env = outer

    def check_stmt(self, stmt: ast.Stmt):
        if isinstance(stmt, (ast.Let, ast.Var)):
            declared = None
            if stmt.ty is not None:
                declared = self.lower.elab_type(
                    stmt.ty, self.module, self.env.tymap, self.env, this=self.this_ty
                )
            expr, ty = self.synth(stmt.expr, expected=declared)
            if declared is not None:
                if not fits(ty, declared):
                    self.error(
                        f"`{stmt.name}` is declared `{show(declared)}` but initialized "
                        f"with `{show(ty)}`"
                    )
                ty = declared
            self.env.locals[stmt.name] = (ty, isinstance(stmt, ast.Var))
            return ir.Let(stmt.name, expr)
        if isinstance(stmt, ast.Assign):
            local = self.env.locals.get(stmt.name)
            if local is None:
                self.error(f"`{stmt.name}` is not in scope")
            ty, mutable = local
            if not mutable:
                self.error(f"`{stmt.name}` is not a `var`")
            expr, ety = self.synth(stmt.expr, expected=ty)
            if not fits(ety, ty):
                self.error(f"cannot assign `{show(ety)}` to `{stmt.name}: {show(ty)}`")
            return ir.Assign(stmt.name, expr)
        if isinstance(stmt, ast.Bind):
            binds = [self.check_bind(spec, expr) for spec, expr in stmt.items]
            return [b for b in binds if b is not None]
        if isinstance(stmt, ast.While):
            cond = self.check_bool(stmt.cond)
            self.loop_depth += 1
            body = self.check_block(stmt.body)
            self.loop_depth -= 1
            return ir.While(cond, body)
        if isinstance(stmt, ast.Loop):
            self.loop_depth += 1
            body = self.check_block(stmt.body)
            self.loop_depth -= 1
            return ir.Loop(body)
        if isinstance(stmt, ast.ExprStmt):
            expr, _ = self.synth(stmt.expr)
            return ir.ExprStmt(expr)
        self.error(f"unsupported statement {stmt!r}")

    def check_bool(self, expr):
        true, false = self.lower.bool_true, self.lower.bool_false
        if true is None:
            self.error("`if`/`while` need lib/bool.moss to be loaded (D45)")
        expected = TUnion((TNominal(false, ()), TNominal(true, ())))
        cond, ty = self.synth(expr, expected=expected)
        if not fits(ty, expected):
            self.error(f"condition is `{show(ty)}`, expected `Bool`")
        return cond

    def check_bind(self, spec: ast.Spec, expr: ast.Expr):
        module = self.module
        if spec.app is not None:
            self.error("a bind left-hand side takes no bracket application")
        if spec.dot is not None:
            receiver = self.lower.resolve_type_symbol(self.env, module, spec.path)
            method = self.lower.find_method_decl(module, receiver, spec.dot)
            if method is None:
                self.error(f"no method `.{spec.dot}` for `{receiver.name}` is in scope")
            this_ty = self.lower.symbol_type(module, receiver, self.env.tymap, self.env)
            sig = self.lower.fn_sig(method, self.env.tymap, self.env, this=this_ty)
            fn_symbol = self.expect_defined_fn(expr)
            self.match_fn_sig(fn_symbol, sig, kind="method")
            self.lower.merge(self.env.methods, module, (receiver, method), sig)
            return ir.BindFn((receiver, method), fn_symbol)
        target = resolve_path(module, spec.path)
        if target is None:
            self.error(f"`{'::'.join(spec.path)}` is not in scope")
        if isinstance(target, Module):
            self.error("cannot bind a module")
        if target.kind == SymKind.TYPE:
            if not isinstance(expr, ast.PathExpr):
                self.error(f"binding type `{target.name}` needs a type on the right")
            ty = self.lower.elab_type(
                ast.TyRef(expr.path, expr.app), module, self.env.tymap, self.env
            )
            self.env.tymap[target] = ty
            return None  # static only
        if target.kind == SymKind.VAL:
            declared = self.lower.elab_type(
                target.decl.ty, target.module, self.env.tymap, self.env
            )
            value, ty = self.synth(expr, expected=declared)
            if not fits(ty, declared):
                self.error(
                    f"bind of `{target.name}`: got `{show(ty)}`, its declared type is "
                    f"`{show(declared)}`"
                )
            self.lower.merge(self.env.vals, module, target, declared)
            return ir.BindVal(target, value)
        if target.kind == SymKind.FN:
            sig = self.lower.fn_sig(target, self.env.tymap, self.env)
            fn_symbol = self.expect_defined_fn(expr)
            self.match_fn_sig(fn_symbol, sig, kind="fn")
            self.lower.merge(self.env.fns, module, target, sig)
            return ir.BindFn(target, fn_symbol)
        self.error(f"cannot bind `{target.name}` (a {target.kind.name.lower()})")

    def expect_defined_fn(self, expr) -> Symbol:
        if not isinstance(expr, ast.PathExpr) or expr.app is not None:
            self.error("the right side of a fn bind must name a defined function")
        target = resolve_path(self.module, expr.path)
        if (
            not isinstance(target, Symbol)
            or target.kind != SymKind.FN
            or not isinstance(target.decl, ast.Fndef)
            or target.decl.body is None
        ):
            self.error(
                f"`{'::'.join(expr.path)}` must be a defined function to provide a bind"
            )
        return target

    def match_fn_sig(self, provider: Symbol, wanted: FnSig, kind: str):
        # D27: the provider's signature must match after current substitutions,
        # with no inference; and its own needs must be satisfiable here.
        self.lower.lower_fn(provider.module, provider)
        self.require_needs(provider, self.lower.needs_of[id(provider)])
        got = self.lower.fn_sig(provider, self.env.tymap, self.env)
        if tuple(t for _, t in got.params) != tuple(t for _, t in wanted.params) or (
            got.ret != wanted.ret
        ):
            self.error(
                f"{kind} bind signature mismatch: provider `{provider.name}` has "
                f"({', '.join(show(t) for _, t in got.params)}) -> {show(got.ret)}, "
                f"need ({', '.join(show(t) for _, t in wanted.params)}) -> {show(wanted.ret)}"
            )

    def require_needs(self, callee: Symbol, needs):
        for need in needs:
            if isinstance(need, tuple):
                if need not in self.env.methods:
                    receiver, method = need
                    self.error(
                        f"calling `{callee.name}` needs `{receiver.name}{method.name}`, "
                        "which is not available in the context here"
                    )
            elif need.kind == SymKind.VAL:
                if need not in self.env.vals:
                    self.error(
                        f"calling `{callee.name}` needs `{need.name}`, which is not "
                        "available in the context here"
                    )
            elif need.kind == SymKind.FN:
                if need not in self.env.fns:
                    self.error(
                        f"calling `{callee.name}` needs `{need.name}`, which is not "
                        "available in the context here"
                    )

    # Expressions

    def synth(self, expr, expected=None):
        if isinstance(expr, ast.UnitExpr):
            return ir.Unit(), TUnit()
        if isinstance(expr, ast.ThisExpr):
            if self.this_ty is None:
                self.error("`this` is only legal inside method bodies")
            return ir.This(), self.this_ty
        if isinstance(expr, ast.Return):
            if expr.expr is None:
                if not isinstance(self.sig.ret, (TUnit, TNever)):
                    self.error(f"`return` with no value, but the function returns `{show(self.sig.ret)}`")
                return ir.Return(None), TNever()
            value, ty = self.synth(expr.expr, expected=self.sig.ret)
            if not fits(ty, self.sig.ret):
                self.error(f"return of `{show(ty)}`, expected `{show(self.sig.ret)}`")
            return ir.Return(value), TNever()
        if isinstance(expr, ast.Break):
            if self.loop_depth == 0:
                self.error("`break` outside of a loop")
            return ir.Break(), TNever()
        if isinstance(expr, ast.If):
            return self.synth_if(expr, expected)
        if isinstance(expr, ast.Match):
            return self.synth_match(expr, expected)
        if isinstance(expr, ast.PathExpr):
            return self.synth_path(expr)
        if isinstance(expr, ast.Call):
            return self.synth_call(expr, expected)
        if isinstance(expr, ast.RecordExpr):
            return self.synth_record(expr)
        if isinstance(expr, ast.Field):
            return self.synth_field(expr)
        if isinstance(expr, ast.MethodCall):
            return self.synth_method(expr)
        self.error(f"unsupported expression {expr!r}")

    def synth_if(self, expr: ast.If, expected):
        cond = self.check_bool(expr.cond)
        then = self.check_block(expr.then, expected=expected)
        then_ty = self.block_type(expr.then, expected)
        if expr.els is None:
            if expected is not None and not fits(TUnit(), expected):
                self.error("an `if` without `else` yields `()`")
            return ir.If(cond, then, None), TUnit()
        if isinstance(expr.els, ast.If):
            els, els_ty = self.synth_if(expr.els, expected)
        else:
            els = self.check_block(expr.els, expected=expected)
            els_ty = self.block_type(expr.els, expected)
        ty = self.join(then_ty, els_ty, expected)
        return ir.If(cond, then, els), ty

    def block_type(self, block: ast.Block, expected):
        # check_block already validated; recompute the tail type cheaply.
        if block.tail is None:
            return TUnit()
        outer = self.push()
        try:
            for stmt in block.stmts:
                self.check_stmt(stmt)
            _, ty = self.synth(block.tail, expected=expected)
            return ty
        finally:
            self.env = outer

    def join(self, a, b, expected):
        if expected is not None:
            return expected
        if isinstance(a, TNever):
            return b
        if isinstance(b, TNever) or a == b:
            return a
        self.error(
            f"branches disagree: `{show(a)}` vs `{show(b)}` (annotate an expected type)"
        )

    def synth_match(self, expr: ast.Match, expected):
        scrutinee, sty = self.synth(expr.scrutinee)
        heads = {}
        for m in members(sty):
            h = head(m)
            if h is not None:
                heads[h] = m
        covered = set()
        catch_all = False
        arms = []
        result_ty = None
        for arm in expr.arms:
            pat, binders, arm_heads, is_catch_all = self.compile_pattern(arm.pattern, sty, heads)
            covered.update(arm_heads)
            catch_all = catch_all or is_catch_all
            outer = self.push()
            try:
                for name, ty in binders:
                    self.env.locals[name] = (ty, False)
                if isinstance(arm.body, ast.Block):
                    body = self.check_block(arm.body, expected=expected)
                    body_ty = expected if expected is not None else self.block_type(arm.body, expected)
                else:
                    body, body_ty = self.synth(arm.body, expected=expected)
                    if expected is not None and not fits(body_ty, expected):
                        self.error(
                            f"match arm yields `{show(body_ty)}`, expected `{show(expected)}`"
                        )
            finally:
                self.env = outer
            result_ty = body_ty if result_ty is None else self.join(result_ty, body_ty, expected)
            arms.append(ir.MatchArm(pat, body))
        if isinstance(sty, TNever):
            if expr.arms:
                self.error("a match on `|` takes no arms")
            return ir.Match(scrutinee, ()), expected if expected is not None else TNever()
        if not catch_all:
            missing = [h.name for h in heads if h not in covered]
            if missing or not heads:
                self.error(f"match is not exhaustive; missing {missing or show(sty)}")
        if result_ty is None:
            result_ty = TNever()
        return ir.Match(scrutinee, tuple(arms)), result_ty

    def compile_pattern(self, pattern, sty, heads):
        """Returns (ir.Pat, binders, covered heads, is_catch_all)."""
        if isinstance(pattern, ast.PatWild):
            return ir.Pat(None, None, None), [], set(), True
        if isinstance(pattern, ast.PatPath):
            target = resolve_path(self.module, pattern.path)
            if target is None and len(pattern.path) == 1:
                # A binder.
                return (
                    ir.Pat(None, pattern.path[0], None),
                    [(pattern.path[0], sty)],
                    set(),
                    True,
                )
            if isinstance(target, Symbol) and target.kind in (SymKind.UNIT, SymKind.TAG):
                return ir.Pat(target, None, None), [], {target}, False
            self.error(f"`{'::'.join(pattern.path)}` is not a matchable pattern")
        if isinstance(pattern, ast.PatTag):
            target = resolve_path(self.module, pattern.path)
            if not isinstance(target, Symbol) or target.kind != SymKind.TAG:
                self.error(f"`{'::'.join(pattern.path)}` is not a tag")
            member = heads.get(target)
            if member is None:
                self.error(f"`{target.name}` is not a member of `{show(sty)}`")
            payload_ty = self.lower.payload_type(member, self.env)
            sub = pattern.payload
            if isinstance(sub, ast.PatPath) and len(sub.path) == 1 and resolve_path(
                self.module, sub.path
            ) is None:
                name = sub.path[0]
                return ir.Pat(target, name, None), [(name, payload_ty)], {target}, False
            self.error("v0 supports only a binder as a tag pattern payload")
        if isinstance(pattern, ast.PatRecord):
            if pattern.path is None:
                self.error("v0 record patterns need a tag head")
            target = resolve_path(self.module, pattern.path)
            if not isinstance(target, Symbol) or target.kind != SymKind.TAG:
                self.error(f"`{'::'.join(pattern.path)}` is not a tag")
            member = heads.get(target)
            if member is None:
                self.error(f"`{target.name}` is not a member of `{show(sty)}`")
            payload_ty = self.lower.payload_type(member, self.env)
            if not isinstance(payload_ty, TRecord):
                self.error(f"`{target.name}` does not have a record payload")
            field_types = dict(payload_ty.fields)
            binders = []
            fields = []
            for name, sub in pattern.fields:
                if name not in field_types:
                    self.error(f"`{target.name}` has no field `{name}`")
                if sub is None:
                    binders.append((name, field_types[name]))
                    fields.append((name, name))
                elif (
                    isinstance(sub, ast.PatPath)
                    and len(sub.path) == 1
                    and resolve_path(self.module, sub.path) is None
                ):
                    binders.append((sub.path[0], field_types[name]))
                    fields.append((name, sub.path[0]))
                else:
                    self.error("v0 record patterns support binders only")
            return ir.Pat(target, None, tuple(fields)), binders, {target}, False
        self.error(f"unsupported pattern {pattern!r}")

    def synth_path(self, expr: ast.PathExpr):
        if len(expr.path) == 1 and expr.app is None:
            local = self.env.locals.get(expr.path[0])
            if local is not None:
                return ir.Local(expr.path[0]), local[0]
        target = resolve_path(self.module, expr.path)
        if target is None:
            self.error(f"`{'::'.join(expr.path)}` is not in scope")
        if isinstance(target, Module):
            self.error(f"`{'::'.join(expr.path)}` is a module, not a value")
        if target.kind == SymKind.UNIT:
            return ir.MakeUnit(target), TNominal(target, ())
        if target.kind == SymKind.VAL:
            if target not in self.env.vals:
                self.error(
                    f"`{target.name}` is not available in the context here "
                    "(assume or bind it)"
                )
            return ir.NeedVal(target), self.env.vals[target]
        if target.kind == SymKind.FN:
            self.error(f"`{target.name}` is a function; call it (no first-class fns yet)")
        if target.kind == SymKind.TAG:
            self.error(f"`{target.name}` is a tag; construct it with a payload")
        self.error(f"`{target.name}` is not a value")

    def synth_call(self, expr: ast.Call, expected):
        path = expr.callee.path
        app = expr.callee.app
        target = resolve_path(self.module, path)
        if target is None:
            self.error(f"`{'::'.join(path)}` is not in scope")
        if isinstance(target, Module):
            self.error(f"`{'::'.join(path)}` is a module")
        if target.kind == SymKind.TAG:
            tymap = self.env.tymap
            if app is not None:
                tymap = tymap | self.lower.app_subst(self.env, self.module, app, self.env.tymap)
            args = self.lower.nominal_args(self.module, target, tymap)
            tag_ty = TNominal(target, args)
            payload_ty = self.lower.payload_type(tag_ty, self.env)
            if len(expr.args) != 1:
                self.error(f"tag `{target.name}` takes exactly one payload")
            value, vty = self.synth(expr.args[0], expected=payload_ty)
            if not fits(vty, payload_ty):
                self.error(
                    f"payload of `{target.name}` is `{show(vty)}`, expected "
                    f"`{show(payload_ty)}`"
                )
            return ir.MakeTag(target, value), tag_ty
        if target.kind != SymKind.FN:
            self.error(f"`{target.name}` is not callable")
        if app is not None:
            self.error("v0 does not support bracket applications at call sites")
        defined = isinstance(target.decl, ast.Fndef) and target.decl.body is not None
        if defined:
            self.lower.lower_fn(target.module, target)
            self.require_needs(target, self.lower.needs_of[id(target)])
            sig = self.lower.fn_sig(target, self.env.tymap, self.env)
            callee = ("direct", target)
        else:
            sig = self.env.fns.get(target)
            if sig is None:
                self.error(
                    f"`{target.name}` is not available in the context here "
                    "(assume or bind it)"
                )
            callee = ("env", target)
        args = self.check_args(target.name, sig, expr.args)
        return ir.Call(callee, tuple(args)), sig.ret

    def check_args(self, name, sig: FnSig, arg_exprs):
        if len(arg_exprs) != len(sig.params):
            self.error(
                f"`{name}` takes {len(sig.params)} argument(s), got {len(arg_exprs)}"
            )
        out = []
        for (pname, pty), arg in zip(sig.params, arg_exprs):
            value, ty = self.synth(arg, expected=pty)
            if not fits(ty, pty):
                self.error(
                    f"argument `{pname}` of `{name}` is `{show(ty)}`, expected `{show(pty)}`"
                )
            out.append(value)
        return out

    def synth_record(self, expr: ast.RecordExpr):
        path = expr.callee.path
        target = resolve_path(self.module, path)
        if not isinstance(target, Symbol) or target.kind != SymKind.TAG:
            self.error(f"`{'::'.join(path)}` is not a tag with a record payload")
        tymap = self.env.tymap
        if expr.callee.app is not None:
            tymap = tymap | self.lower.app_subst(
                self.env, self.module, expr.callee.app, self.env.tymap
            )
        args = self.lower.nominal_args(self.module, target, tymap)
        tag_ty = TNominal(target, args)
        payload_ty = self.lower.payload_type(tag_ty, self.env)
        if not isinstance(payload_ty, TRecord):
            self.error(f"`{target.name}` does not have a record payload")
        field_types = dict(payload_ty.fields)
        given = dict()
        fields = []
        for name, value in expr.fields:
            if name not in field_types:
                self.error(f"`{target.name}` has no field `{name}`")
            if name in given:
                self.error(f"duplicate field `{name}`")
            if value is None:
                value_ir, vty = self.synth(ast.PathExpr([name], None), expected=field_types[name])
            else:
                value_ir, vty = self.synth(value, expected=field_types[name])
            if not fits(vty, field_types[name]):
                self.error(
                    f"field `{name}` is `{show(vty)}`, expected `{show(field_types[name])}`"
                )
            given[name] = True
            fields.append((name, value_ir))
        missing = [n for n, _ in payload_ty.fields if n not in given]
        if missing:
            self.error(f"missing field(s) {missing} of `{target.name}`")
        return ir.MakeRecord(target, tuple(fields)), tag_ty

    def synth_field(self, expr: ast.Field):
        obj, oty = self.synth(expr.obj)
        record = oty
        if isinstance(oty, TNominal) and oty.symbol.kind == SymKind.TAG:
            record = self.lower.payload_type(oty, self.env)
        if not isinstance(record, TRecord):
            self.error(f"`{show(oty)}` has no fields")
        for name, ty in record.fields:
            if name == expr.name:
                return ir.Field(obj, expr.name), ty
        self.error(f"`{show(oty)}` has no field `{expr.name}`")

    def synth_method(self, expr: ast.MethodCall):
        obj, oty = self.synth(expr.obj)
        h = head(oty)
        if h is None:
            self.error(f"`{show(oty)}` has no methods (it has no nominal head)")
        name = expr.path[-1]
        if len(expr.path) > 1:
            method = resolve_detached(self.module, expr.path)
            if method is None:
                self.error(f"`{'::'.join(expr.path)}` is not a detached method in scope")
        else:
            method = None
        # Attached, defined: an ordinary function with a receiver.
        if method is None:
            for home in (h.module, self.module):
                attached = home.attached.get((id(h), name))
                if attached is not None and attached.decl.body is not None:
                    self.lower.lower_fn(attached.module, attached)
                    self.require_needs(attached, self.lower.needs_of[id(attached)])
                    sig = self.lower.fn_sig(attached, self.env.tymap, self.env, this=oty)
                    args = self.check_args(attached.name, sig, expr.args)
                    return ir.Call(("direct", attached), tuple(args), this=obj), sig.ret
        # Provided: detached or abstract-attached, from the context.
        for key, sig in self.env.methods.items():
            receiver, msym = key
            if receiver is h and msym.name.lstrip(".") == name:
                if method is not None and msym is not method:
                    continue
                args = self.check_args(msym.name, sig, expr.args)
                return ir.Call(("env", key), tuple(args), this=obj), sig.ret
        self.error(
            f"no method `.{name}` is available for `{show(oty)}` in the context here"
        )
