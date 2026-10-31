"""Lower: elaborate a collected Program into the core IR.

This implements the static semantics (docs/design/semantics.md section 11,
stage 4): requirement environments from `assume` nesting, context
flattening, the D22 totality rule, the D26-D28 bind rules, forward type
inference, method resolution (D36), match exhaustiveness (D34), and the
D39 scope-versus-context error distinction. Every defined function is
elaborated once, under its own assumptions (the interpreter carries context
at runtime; monomorphization is a later backend's job).

Deliberate v0 simplifications, each marked TODO in place:
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
    TUnion,
    TUnit,
    fits,
    head,
    members,
    show,
)


# D68: the method of lib/num.moss each operator means, by its spelling
# and how many operands besides the receiver it has.
OPERATOR_METHODS = {
    ("-", 0): "neg",
    ("!", 0): "not",
    ("*", 1): "mul",
    ("/", 1): "div",
    ("%", 1): "rem",
    ("+", 1): "add",
    ("-", 1): "sub",
    ("<<", 1): "shl",
    (">>", 1): "shr",
    ("==", 1): "eq",
    ("!=", 1): "ne",
    ("<", 1): "lt",
    (">", 1): "gt",
    ("<=", 1): "le",
    (">=", 1): "ge",
    ("&", 1): "and",
    ("^", 1): "xor",
    ("|", 1): "or",
}


@dataclass(frozen=True)
class Built:
    """An expression already lowered, standing where `synth` wants syntax:
    what a literal's desugaring (D68) is assembled from."""

    node: object
    ty: object
    offset: int = -1

    def __iter__(self):
        return iter((self.node, self.ty))


class LowerError(Exception):
    def __init__(self, module: str, message: str, where: str = ""):
        super().__init__(f"{module}{where}: {message}")
        self.module = module
        self.message = message


def has_break(node) -> bool:
    """Whether a loop body can break out of *this* loop (breaks belonging to
    nested loops don't count)."""
    from dataclasses import fields, is_dataclass

    if isinstance(node, ast.Break):
        return True
    if isinstance(node, (ast.Loop, ast.While, ast.Fndef)):
        return False
    if is_dataclass(node):
        return any(has_break(getattr(node, f.name)) for f in fields(node))
    if isinstance(node, (list, tuple)):
        return any(has_break(item) for item in node)
    return False


def locate(module: Module, node) -> str:
    """`:line:col` for a node with a source offset, if we can compute it."""
    offset = getattr(node, "offset", -1)
    if node is None or offset < 0 or module.source is None:
        return ""
    from .lex import position

    line, col = position(module.source, offset)
    return f":{line}:{col}"


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
            # Provision key -> the instantiation it was made at: for each
            # type symbol the item's own telescope mentions, the type that
            # symbol denoted (see `Lower.provision_app`).
            self.apps = {}
        else:
            self.tymap = dict(parent.tymap)
            self.vals = dict(parent.vals)
            self.fns = dict(parent.fns)
            self.methods = dict(parent.methods)
            self.locals = dict(parent.locals)
            self.apps = dict(parent.apps)


class Lower:
    def __init__(self, program: Program):
        self.program = program
        self.fns: dict[int, ir.FnIR] = {}  # id(Symbol) -> FnIR
        self.checked_functors: set = set()
        self.in_progress: set[int] = set()
        self.fn_symbols: dict[int, Symbol] = {}  # id(decl) -> Symbol
        self.needs_of: dict[int, tuple] = {}  # id(Symbol) -> runtime need keys
        # id(Symbol) -> {need key: the instantiation the function requires
        # it at, in the function's own terms} (coherence; see require_needs).
        self.need_apps: dict[int, dict] = {}
        # id(val Symbol) -> how many scalars its value occupies (D59), for
        # every val some defined function needs.
        self.val_widths: dict[int, int] = {}
        self._reqs_cache: dict[int, list] = {}
        self._sig_syms_cache: dict[int, list] = {}
        self._const_cache: dict[int, tuple] = {}
        # D66: the forwarder synthesized for each abstract fn bound as a
        # provider, and the other way round.
        self.forwarders: dict[int, Symbol] = {}  # id(abstract fn) -> forwarder
        self.forwarded: dict[int, Symbol] = {}  # id(forwarder) -> abstract fn
        for module in program.order:
            for table in (module.names, module.detached, module.attached):
                for symbol in table.values():
                    self.fn_symbols[id(symbol.decl)] = symbol
        self.bool_true, self.bool_false = self.find_bool()
        # D68: operators and literals mean fixed symbols of these library
        # modules, found by the file that declares them as `Bool` is.
        self.lang: dict[str, Module] = {}
        for module in program.order:
            for short in ("num", "int", "float"):
                if module.path.endswith(f"lib/{short}.moss"):
                    self.lang[short] = module

    def error(self, module: Module, message: str, node=None):
        raise LowerError(module.path, message, locate(module, node))

    def find_bool(self):
        for module in self.program.order:
            if module.path.endswith("lib/bool.moss"):
                return module.names.get("Bool"), None
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
        target = self.forwarded.get(id(symbol))
        if target is not None:
            # A forwarder assumes what its target's signature needs — the
            # type symbols of the target's assumes — and the target.
            for ty in self.type_requirements(target):
                self.add_symbol_item(env, module, ty, {}, needs)
            self.add_symbol_item(env, module, target, {}, needs)
            return env, needs
        for item in symbol.assumes:
            self.add_assume_item(env, module, item, needs)
        return env, needs

    def forwarder(self, target: Symbol) -> Symbol:
        """D66: binding an abstract fn the context provides,
        `bind f = g;`, is sugar for binding a defined function that
        forwards to it,

            assume <the types g's signature needs>, g {
              fn g'(x0: T0, ...): R { g(x0, ...) }
            }

        so it captures `g` as provided at the bind site exactly as any
        provider's needs are captured (D26). One per target: it is the
        same function whichever bind asks for it."""
        cached = self.forwarders.get(id(target))
        if cached is not None:
            return cached
        decl = target.decl
        # Parameter names no source program can spell, so none can
        # capture or shadow anything.
        params = [ast.Param(f"%{i}", p.ty) for i, p in enumerate(decl.params)]
        call = ast.Call(
            ast.PathExpr([target.name], None, offset=decl.offset),
            [ast.PathExpr([p.name], None) for p in params],
            offset=decl.offset,
        )
        body = ast.Block([], call, offset=decl.offset)
        fwd_decl = ast.Fndef(decl.name, params, decl.ret, body, offset=decl.offset)
        fwd = Symbol(target.module, target.name, SymKind.FN, fwd_decl)
        self.forwarders[id(target)] = fwd
        self.forwarded[id(fwd)] = target
        self.fn_symbols[id(fwd_decl)] = fwd
        return fwd

    def add_assume_item(self, env: Env, module: Module, item: ast.AssumeItem, needs: list):
        if item.dot is not None:
            receiver = self.resolve_type_symbol(env, module, item.path)
            self.add_method_item(env, module, receiver, item.dot, {}, needs)
            return
        target = resolve_path(module, item.path)
        if target is None:
            self.error(module, f"`{'::'.join(item.path)}` is not in scope", item)
        if isinstance(target, Module):
            self.error(module, f"cannot assume a module (`{'::'.join(item.path)}`)", item)
        self.add_symbol_item(env, module, target, {}, needs)

    def add_symbol_item(self, env: Env, module: Module, sym: Symbol, subst: dict, needs: list):
        self.check_totality(module, sym, subst, env)
        if sym.kind == SymKind.TYPE:
            if sym not in env.tymap:
                env.tymap[sym] = TAbstract(sym, self.nominal_args(module, sym, env.tymap | subst))
        elif sym.kind == SymKind.VAL:
            if sym.decl.init is not None:
                return  # a defined val is concrete; nothing to provide (D50)
            ty = self.elab_type(sym.decl.ty, sym.module, env.tymap | subst, env)
            self.merge(env, env.vals, module, sym, ty)
            env.apps.setdefault(sym, self.provision_app(sym, env.tymap | subst))
            if sym not in needs:
                needs.append(sym)
        elif sym.kind == SymKind.FN:
            sig = self.fn_sig(sym, env.tymap | subst, env)
            self.merge(env, env.fns, module, sym, sig)
            env.apps.setdefault(sym, self.provision_app(sym, env.tymap | subst))
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
            self.error(module, f"`{'::'.join(spec.path)}` is not in scope", spec)
        if isinstance(target, Module):
            self.error(module, "cannot put a module in a context", spec)
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
        key = self.method_key(env, module, this_ty, method)
        self.merge(env, env.methods, module, key, sig)
        env.apps.setdefault(key, self.provision_app(method, tymap, method=True))
        if key not in needs:
            needs.append(key)

    def check_functor_total(self, functor: Symbol):
        """D55/D22: a functor must bind every item of its result signature.
        Checked once, where the functor is written, so a half-built bridge
        reports at the bridge rather than at each application."""
        if id(functor) in self.checked_functors:
            return
        self.checked_functors.add(id(functor))
        module = functor.module
        wanted = {}
        for spec in functor.decl.result:
            self.expand_spec(module, spec, wanted, 0)
        for bind in functor.decl.binds:
            for spec, expr in bind.items:
                if expr is None:
                    # An application: it provides its own result signature.
                    inner = resolve_path(module, spec.path)
                    if isinstance(inner, Symbol) and inner.kind == SymKind.FUNCTOR:
                        self.check_functor_total(inner)
                        for item in inner.decl.result:
                            got = {}
                            self.expand_spec(inner.module, item, got, 0)
                            for key in got:
                                wanted.pop(key, None)
                    continue
                key = self.spec_key(module, spec)
                wanted.pop(key, None)
        if wanted:
            missing = ", ".join(sorted(wanted.values()))
            self.error(
                module,
                f"functor `{functor.name}` does not bind {missing}",
                functor.decl,
            )

    def expand_spec(self, module: Module, spec: ast.Spec, out: dict, depth: int):
        """The leaf items a result-signature entry stands for, keyed the
        same way `spec_key` keys a bind's left-hand side."""
        if depth > 16:
            self.error(module, "context nests too deeply", spec)
        target = resolve_path(module, spec.path)
        if target is None or isinstance(target, Module):
            return  # a real error, reported where the signature is used
        if spec.dot is None and target.kind == SymKind.CONTEXT:
            for item in target.decl.items:
                self.expand_spec(target.module, item, out, depth + 1)
            return
        if spec.dot is None and self.already_provided(target):
            return  # a definition, not something anyone can bind
        key = self.spec_key(module, spec)
        if key is not None:
            name = f"`{target.name}.{spec.dot}`" if spec.dot else f"`{target.name}`"
            out[key] = name

    def already_provided(self, symbol: Symbol) -> bool:
        """Whether the symbol carries its own definition, so a functor has
        nothing to bind: units, tags, aliases, defined fns and defined
        vals. `Bool` and `true` are in `Std` but nobody provides them."""
        if symbol.kind in (SymKind.UNIT, SymKind.TAG, SymKind.ALIAS):
            return True
        if symbol.kind == SymKind.FN:
            return getattr(symbol.decl, "body", None) is not None
        if symbol.kind == SymKind.VAL:
            return getattr(symbol.decl, "init", None) is not None
        return False

    def spec_key(self, module: Module, spec: ast.Spec):
        target = resolve_path(module, spec.path)
        if target is None or isinstance(target, Module):
            return None
        return (id(target), spec.dot)

    def slots_of(self, env, ty, visiting: frozenset = frozenset()) -> int:
        """D59: how many Wasm scalars a value of this type occupies. A
        record is its fields' runs, one after another; a tag is its payload
        (D58); a union of units is its code, and any other union is a
        discriminant followed by room for its widest member. Everything
        else is one.

        A tag that contains itself would need an endless run of scalars —
        there is no heap to break the cycle — so it is an error. Every
        such cycle passes through a tag, since only a nominal type refers
        to a type by name, so `visiting` only needs to hold tags."""
        ty = self.canon(env, ty)
        if isinstance(ty, TRecord):
            return sum(self.slots_of(env, t, visiting) for _, t in ty.fields)
        if isinstance(ty, TUnion):
            units = all(
                head(m) is not None and head(m).kind == SymKind.UNIT
                for m in members(ty)
            )
            if units:
                return 1
            widest = 0
            for m in members(ty):
                if isinstance(m, TNominal) and m.symbol.kind == SymKind.TAG:
                    widest = max(widest, self.slots_of(env, m, visiting))
            return 1 + widest
        if isinstance(ty, TNominal) and ty.symbol.kind == SymKind.TAG:
            if id(ty.symbol) in visiting:
                self.error(
                    ty.symbol.module,
                    f"`{ty.symbol.name}` contains itself, so it has no finite layout (D58)",
                )
            return self.slots_of(
                env, self.payload_type(ty, env), visiting | {id(ty.symbol)}
            )
        return 1

    def field_layout(self, env, record: TRecord) -> list:
        """Each field's (offset, width) within the record's run."""
        out = []
        offset = 0
        for _, t in record.fields:
            width = self.slots_of(env, t)
            out.append((offset, width))
            offset += width
        return out

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

    # Consistent merging (D43). The union-find lives in env.tymap: an entry
    # whose value is not the symbol's own TAbstract is a substitution edge,
    # written either by an explicit type bind or by unification during a
    # merge. `canon` chases those edges; `unify` adds them, erroring only
    # when two genuinely distinct concrete types would have to be equal.

    def canon(self, env: Env, t):
        if isinstance(t, TAbstract):
            target = env.tymap.get(t.symbol)
            if target is not None and not (
                isinstance(target, TAbstract) and target.symbol is t.symbol
            ):
                return self.canon(env, target)
            if t.args:
                return TAbstract(
                    t.symbol, tuple((s, self.canon(env, a)) for s, a in t.args)
                )
            return t
        if isinstance(t, TNominal):
            if t.args:
                return TNominal(
                    t.symbol, tuple((s, self.canon(env, a)) for s, a in t.args)
                )
            return t
        if isinstance(t, TUnion):
            return TUnion(tuple(self.canon(env, m) for m in t.members))
        if isinstance(t, TRecord):
            return TRecord(tuple((n, self.canon(env, ty)) for n, ty in t.fields))
        return t

    def method_key(self, env: Env, module: Module, receiver_ty, method: Symbol) -> tuple:
        """D36/D62: a method provision is keyed by the receiver's *type* —
        so `IsCell[Cell=MyCell]` provides its methods at MyCell, where calls
        will look them up — and by its whole [D18] identity, declaration and
        static bindings, not just its head. A provision made at
        `Box[Elem=Char]` then serves no `Box[Elem=Point]`. Canonicalized
        through the union-find, so the spellings of one merged atom are one
        key (D43)."""
        receiver = self.canon(env, receiver_ty)
        if head(receiver) is None:
            self.error(
                module, f"`{show(receiver_ty)}` cannot receive methods (no nominal head)"
            )
        return (receiver, method)

    def canon_head(self, env: Env, module: Module, sym: Symbol) -> Symbol:
        """The representative receiver symbol for method keys."""
        if sym.kind == SymKind.TYPE:
            t = self.canon(env, TAbstract(sym, ()))
            h = head(t)
            if h is None:
                self.error(
                    module,
                    f"`{sym.name}` resolves to `{show(t)}`, which cannot receive methods",
                )
            return h
        return sym

    def unify(self, env: Env, module: Module, a, b, why: str = ""):
        a = self.canon(env, a)
        b = self.canon(env, b)
        if a == b:
            return
        if isinstance(a, TAbstract) and not a.args:
            env.tymap[a.symbol] = b
            return
        if isinstance(b, TAbstract) and not b.args:
            env.tymap[b.symbol] = a
            return
        pairs = None
        if isinstance(a, (TNominal, TAbstract)) and type(a) is type(b) and a.symbol is b.symbol:
            pairs = zip((x for _, x in a.args), (y for _, y in b.args))
        elif isinstance(a, TUnion) and isinstance(b, TUnion) and len(a.members) == len(b.members):
            pairs = zip(a.members, b.members)
        elif (
            isinstance(a, TRecord)
            and isinstance(b, TRecord)
            and [n for n, _ in a.fields] == [n for n, _ in b.fields]
        ):
            pairs = zip((x for _, x in a.fields), (y for _, y in b.fields))
        if pairs is not None:
            for x, y in pairs:
                self.unify(env, module, x, y, why)
            return
        self.error(module, f"cannot merge `{show(a)}` with `{show(b)}`{why} (D43)")

    def unify_sig(self, env: Env, module: Module, a: FnSig, b: FnSig, key):
        name = key[1].name if isinstance(key, tuple) else getattr(key, "name", key)
        why = f" for `{name}`"
        if len(a.params) != len(b.params):
            self.error(module, f"conflicting provisions{why} (arity)")
        for (_, x), (_, y) in zip(a.params, b.params):
            self.unify(env, module, x, y, why)
        self.unify(env, module, a.ret, b.ret, why)
        if a.this is not None and b.this is not None:
            self.unify(env, module, a.this, b.this, why)

    def shadow(self, table: dict, key, value):
        """A `bind`'s entry: it replaces whatever the key had, to the end of
        the block (D25). The bind was already checked against the
        signature as it reads *here*; the entry it replaces may have been
        elaborated before a type it mentions was rebound — inside a scope
        with an f64 `Float`, `bind Float = Single;` and then its ops — so
        it is not merged with (D43 merges the items of contexts being
        assumed, where two mentions of one key are the same requirement;
        a bind is a new provision, not another mention)."""
        table[key] = value

    def merge(self, env: Env, table: dict, module: Module, key, value):
        existing = table.get(key)
        if existing is None:
            table[key] = value
            return
        if isinstance(value, FnSig):
            self.unify_sig(env, module, existing, value, key)
        else:
            name = getattr(key, "name", key)
            self.unify(env, module, existing, value, f" for `{name}`")

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

    def provision_app(self, sym: Symbol, tymap: dict, method: bool = False) -> dict:
        """The instantiation a val, fn, or method item is in force at: for
        each type symbol it depends on, what that symbol denoted where the
        item was assumed or bound — absent (`None`) where it was not in
        force at all.

        For a val or fn the symbols are its whole telescope
        (`type_requirements`): the ambient instantiation its coherence
        premise compares against (CoreMoss.v deviation 8, `dep_tys`). For a
        method item they are the telescope's symbols its *signature*
        mentions — the application `θ` that a method item carries in the
        calculus (`IMth τ m θ`) beside its receiver, which is the key's
        other half. A symbol the signature does not mention cannot change
        what the provision is, and the receiver is `This`, not a
        parameter.

        Assumed items are frozen once their function's environment is
        complete (`freeze_app`), since a later D43 merge may still identify
        their symbols; bound items are frozen at the bind."""
        reqs = self.type_requirements(sym)
        if method:
            reqs = self.signature_type_syms(sym, reqs)
        return {t: tymap.get(t) for t in reqs}

    def signature_type_syms(self, sym: Symbol, reqs: list) -> list:
        cached = self._sig_syms_cache.get(id(sym))
        if cached is None:
            found: list = []
            decl = sym.decl
            for p in decl.params:
                self.free_type_syms(p.ty, sym.module, found, set())
            if decl.ret is not None:
                self.free_type_syms(decl.ret, sym.module, found, set())
            cached = [t for t in reqs if t in found]
            self._sig_syms_cache[id(sym)] = cached
        return cached

    def freeze_app(self, env: Env, app: dict) -> dict:
        """An instantiation read through the union-find as it stands now,
        so a later `bind` of one of its symbols — a new generation of that
        symbol [D25] — no longer reads into it."""
        return {t: None if ty is None else self.canon(env, ty) for t, ty in app.items()}

    def show_app(self, app: dict) -> str:
        def one(t, ty):
            if ty is None:
                return f"{t.name} not in force"
            if isinstance(ty, TAbstract) and ty.symbol is t and not ty.args:
                return f"{t.name} as assumed, before a later `bind {t.name}`"
            return f"{t.name}={show(ty)}"

        return ", ".join(one(t, ty) for t, ty in app.items())

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
        if sym.kind in (SymKind.UNIT, SymKind.TYPE):
            # An abstract type symbol is a bare unification variable (D43);
            # giving it identity args for everything its assumes provide
            # would block merging (and made `unit Stop;` under Std spuriously
            # distinct per ambient binding).
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
        if isinstance(ty, ast.TyUnit):
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
        """Both sides of `[Elem=Char]` are read in `module` — the module the
        application is *written* in, which for a context's own item list is
        the module declaring the context, not the one assuming it. The
        result is a substitution the caller applies locally: it is
        deliberately not written into `env.tymap`, so one `Elem` can be
        `Char` at one receiver and `Int` at another (D51/D61)."""
        subst = {}
        for binding in app:
            target = resolve_path(module, binding.path)
            if not isinstance(target, Symbol) or target.kind != SymKind.TYPE:
                self.error(
                    module,
                    f"`{'::'.join(binding.path)}` in a bracket binding must be an "
                    "abstract type symbol",
                )
            if binding.spec.dot is not None or binding.spec.path is None:
                self.error(module, "bracket bindings bind types in v0")
            ty = self.elab_type(
                ast.TyRef(binding.spec.path, binding.spec.app),
                module,
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
        if isinstance(ty, ast.TyUnit):
            return TUnit()
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
        env.apps = {k: self.freeze_app(env, app) for k, app in env.apps.items()}
        self.needs_of[id(symbol)] = tuple(needs)
        self.need_apps[id(symbol)] = {k: env.apps.get(k, {}) for k in needs}
        decl = symbol.decl
        has_this = symbol.receiver is not None
        this_ty = None
        if has_this:
            if symbol.receiver.kind not in (SymKind.UNIT, SymKind.TAG):
                self.error(
                    module,
                    f"attached method `{symbol.name}` requires a nominal receiver (Q5); "
                    f"`{symbol.receiver.name}` is abstract",
                    symbol.decl,
                )
            this_ty = TNominal(
                symbol.receiver, self.nominal_args(module, symbol.receiver, env.tymap)
            )
        sig = self.fn_sig(symbol, env.tymap, env, this=this_ty)
        checker = FnChecker(self, module, env, sig, needs, this_ty)
        body = checker.check_block(decl.body, expected=sig.ret)
        param_slots = tuple(
            [self.slots_of(env, this_ty)] if has_this else []
        ) + tuple(self.slots_of(env, t) for _, t in sig.params)
        for key in needs:
            if isinstance(key, Symbol) and key.kind == SymKind.VAL and key in env.vals:
                self.val_widths[id(key)] = self.slots_of(env, env.vals[key])
        fn = ir.FnIR(
            symbol,
            tuple(n for n, _ in sig.params),
            tuple(needs),
            body,
            has_this,
            param_slots,
            self.slots_of(env, sig.ret),
        )
        self.fns[id(symbol)] = fn
        self.in_progress.discard(id(symbol))

    def const_val(self, symbol: Symbol):
        """Elaborate a defined val (D50). v0 restrictions: no assumptions,
        and the initializer is inlined at each use (it must therefore need
        nothing from any context — a fresh empty environment enforces that)."""
        cached = self._const_cache.get(id(symbol))
        if cached is not None:
            return cached
        if symbol.assumes:
            self.error(
                symbol.module,
                f"defined val `{symbol.name}` cannot take assumptions yet (D50)",
                symbol.decl,
            )
        env = Env(symbol.module)
        declared = self.elab_type(symbol.decl.ty, symbol.module, {}, env)
        checker = FnChecker(self, symbol.module, env, FnSig((), declared), [], None)
        value, ty = checker.synth(symbol.decl.init, expected=declared)
        if not fits(ty, declared):
            self.error(
                symbol.module,
                f"`{symbol.name}` is declared `{show(declared)}` but its "
                f"initializer is `{show(ty)}`",
                symbol.decl,
            )
        self._const_cache[id(symbol)] = (value, declared)
        return value, declared


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
        self.applying: set = set()  # functors being applied, to catch cycles
        self.at_node = None  # the statement/expression currently checked
        for name, ty in sig.params:
            env.locals[name] = (ty, False)

    def error(self, message: str):
        raise LowerError(self.module.path, message, locate(self.module, self.at_node))

    def fits(self, t, expected) -> bool:
        """D17 fitting, through the union-find (D43): merged symbols are
        interchangeable."""
        return fits(
            self.lower.canon(self.env, t), self.lower.canon(self.env, expected)
        )

    def push(self) -> Env:
        outer = self.env
        self.env = Env(self.module, parent=outer)
        return outer

    # Blocks and statements

    def check_block(self, block: ast.Block, expected=None) -> ir.Block:
        return self.synth_block(block, expected)[0]

    def synth_block(self, block: ast.Block, expected=None):
        """A block, scoping its `let`s and `bind`s, and the type it
        yields: its tail's, `()` without one, or `|` when its last
        statement cannot fall through."""
        outer = self.push()
        try:
            stmts = []
            self.diverged = False
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
                # A block whose last statement cannot fall through (a loop
                # with no break, or an expression of type `|`) diverges.
                tail_ty = TNever() if self.diverged else TUnit()
            else:
                tail, tail_ty = self.synth(block.tail, expected=expected)
            if expected is not None and not self.fits(tail_ty, expected):
                self.error(f"block yields `{show(tail_ty)}`, expected `{show(expected)}`")
            return ir.Block(tuple(stmts), tail), tail_ty
        finally:
            self.env = outer

    def check_stmt(self, stmt: ast.Stmt):
        result = self._check_stmt(stmt)
        # Decide divergence only now: nested block checking (inside loops,
        # ifs, matches) clobbers the flag along the way.
        if isinstance(stmt, ast.Loop):
            self.diverged = not has_break(stmt.body)
        elif isinstance(stmt, ast.ExprStmt):
            self.diverged = self._expr_diverged
        else:
            self.diverged = False
        return result

    def _check_stmt(self, stmt: ast.Stmt):
        if getattr(stmt, "offset", -1) >= 0:
            self.at_node = stmt
        if isinstance(stmt, (ast.Let, ast.Var)):
            declared = None
            if stmt.ty is not None:
                declared = self.lower.elab_type(
                    stmt.ty, self.module, self.env.tymap, self.env, this=self.this_ty
                )
            expr, ty = self.synth(stmt.expr, expected=declared)
            if declared is not None:
                if not self.fits(ty, declared):
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
            if not self.fits(ety, ty):
                self.error(f"cannot assign `{show(ety)}` to `{stmt.name}: {show(ty)}`")
            return ir.Assign(stmt.name, expr)
        if isinstance(stmt, ast.Bind):
            binds = []
            for spec, expr in stmt.items:
                if expr is None:
                    binds.extend(self.apply_functor(spec))
                else:
                    binds.append(self.check_bind(spec, expr))
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
            expr, ty = self.synth(stmt.expr)
            self._expr_diverged = isinstance(ty, TNever)
            return ir.ExprStmt(expr)
        self.error(f"unsupported statement {stmt!r}")

    def check_bool(self, expr):
        bool_sym = self.lower.bool_true
        if bool_sym is None:
            self.error("`if`/`while` need lib/bool.moss to be loaded (D45)")
        expected = TNominal(bool_sym, ())
        cond, ty = self.synth(expr, expected=expected)
        if not self.fits(ty, expected):
            self.error(f"condition is `{show(ty)}`, expected `Bool`")
        return cond

    def apply_functor(self, spec: ast.Spec) -> list:
        """D55: `bind F;` installs F's binds here. They are *written* in F's
        module, so names resolve there, but they take effect in this
        function's environment and their needs become this function's —
        which is what makes the argument signature a requirement of the
        application site rather than of the functor."""
        if spec.dot is not None or spec.app is not None:
            self.error("a functor application takes no method or bracket part")
        target = resolve_path(self.module, spec.path)
        if target is None:
            self.error(f"`{'::'.join(spec.path)}` is not in scope")
        if isinstance(target, Module) or target.kind != SymKind.FUNCTOR:
            name = getattr(target, "name", spec.path[-1])
            self.error(f"`{name}` is not a functor; a bind needs `= value`")
        if id(target) in self.applying:
            self.error(f"functor `{target.name}` applies itself")
        self.lower.check_functor_total(target)
        self.applying.add(id(target))
        outer = self.module
        self.module = target.module
        try:
            out = []
            for bind in target.decl.binds:
                for item_spec, item_expr in bind.items:
                    if item_expr is None:
                        out.extend(self.apply_functor(item_spec))
                    else:
                        out.append(self.check_bind(item_spec, item_expr))
        finally:
            self.module = outer
            self.applying.discard(id(target))
        return [b for b in out if b is not None]

    def check_bind(self, spec: ast.Spec, expr: ast.Expr):
        module = self.module
        if spec.app is not None and spec.dot is None:
            self.error("a bind left-hand side takes no bracket application")
        if spec.dot is not None:
            # `bind IntList.get[Elem=Int] = Ints.get;` — a shared accessor
            # is provided at one receiver at a time, and the element type
            # comes with the provision (D61). The substitution is local to
            # this bind, so the same symbol can be something else at the
            # next receiver.
            tymap = self.env.tymap
            if spec.app is not None:
                tymap = tymap | self.lower.app_subst(self.env, module, spec.app, {})
            receiver = self.lower.resolve_type_symbol(self.env, module, spec.path)
            method = self.lower.find_method_decl(module, receiver, spec.dot)
            if method is None:
                self.error(f"no method `.{spec.dot}` for `{receiver.name}` is in scope")
            this_ty = self.lower.symbol_type(module, receiver, tymap, self.env)
            sig = self.lower.fn_sig(method, tymap, self.env, this=this_ty)
            fn_symbol = self.expect_provider(expr, this_ty)
            needs_map = self.match_fn_sig(fn_symbol, sig, kind="method")
            key = self.lower.method_key(self.env, module, this_ty, method)
            self.lower.shadow(self.env.methods, key, sig)
            self.provided_at(key, method, tymap, method=True)
            return ir.BindFn(key, fn_symbol, needs_map)
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
            if not self.fits(ty, declared):
                self.error(
                    f"bind of `{target.name}`: got `{show(ty)}`, its declared type is "
                    f"`{show(declared)}`"
                )
            self.lower.shadow(self.env.vals, target, declared)
            self.provided_at(target, target, self.env.tymap)
            return ir.BindVal(target, value)
        if target.kind == SymKind.FN:
            sig = self.lower.fn_sig(target, self.env.tymap, self.env)
            fn_symbol = self.expect_fn_provider(expr)
            needs_map = self.match_fn_sig(fn_symbol, sig, kind="fn")
            self.lower.shadow(self.env.fns, target, sig)
            self.provided_at(target, target, self.env.tymap)
            return ir.BindFn(target, fn_symbol, needs_map)
        self.error(f"cannot bind `{target.name}` (a {target.kind.name.lower()})")

    def provided_at(self, key, sym: Symbol, tymap: dict, method: bool = False):
        """Record the instantiation a bind's provision is made at."""
        self.env.apps[key] = self.lower.freeze_app(
            self.env, self.lower.provision_app(sym, tymap, method)
        )

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

    def expect_fn_provider(self, expr) -> Symbol:
        """The right side of a *fn* bind: a defined function, or (D66) an
        abstract one this context provides, through its forwarder."""
        if isinstance(expr, ast.PathExpr) and expr.app is None:
            target = resolve_path(self.module, expr.path)
            if (
                isinstance(target, Symbol)
                and target.kind == SymKind.FN
                and isinstance(target.decl, ast.Fndef)
                and target.decl.body is None
            ):
                if target not in self.env.fns:
                    self.error(
                        f"`{target.name}` is not available in the context here, "
                        "so it cannot provide a bind (assume or bind it)"
                    )
                return self.lower.forwarder(target)
        return self.expect_defined_fn(expr)

    def expect_provider(self, expr, this_ty) -> Symbol:
        """The right side of a *method* bind: a defined function, or an
        attached method (`bind MyCell.read=MyCell.get;`) — the latter is the
        only provider that can see the receiver as `this` (Q4)."""
        if isinstance(expr, ast.Field) and isinstance(expr.obj, ast.PathExpr):
            receiver = resolve_path(self.module, expr.obj.path)
            if isinstance(receiver, Symbol) and receiver.kind in (
                SymKind.UNIT,
                SymKind.TAG,
            ):
                for home in (receiver.module, self.module):
                    attached = home.attached.get((id(receiver), expr.name))
                    if attached is not None and attached.decl.body is not None:
                        if head(this_ty) is not receiver:
                            self.error(
                                f"provider `{attached.name}` is attached to "
                                f"`{receiver.name}`, not `{show(this_ty)}`"
                            )
                        return attached
                self.error(
                    f"`{receiver.name}` has no defined attached method `.{expr.name}`"
                )
        return self.expect_defined_fn(expr)

    def match_fn_sig(self, provider: Symbol, wanted: FnSig, kind: str):
        # D27: the provider's signature must match after current substitutions,
        # with no inference; and its own needs must be satisfiable here.
        self.lower.lower_fn(provider.module, provider)
        needs_map = self.require_needs(provider, self.lower.needs_of[id(provider)])
        got = self.lower.fn_sig(provider, self.env.tymap, self.env)
        canon = lambda t: self.lower.canon(self.env, t)
        if tuple(canon(t) for _, t in got.params) != tuple(
            canon(t) for _, t in wanted.params
        ) or canon(got.ret) != canon(wanted.ret):
            self.error(
                f"{kind} bind signature mismatch: provider `{provider.name}` has "
                f"({', '.join(show(t) for _, t in got.params)}) -> {show(got.ret)}, "
                f"need ({', '.join(show(t) for _, t in wanted.params)}) -> {show(wanted.ret)}"
            )
        return needs_map

    def require_needs(self, callee: Symbol, needs) -> tuple:
        """Check each of the callee's needs against this environment and
        return the (callee key -> caller key) translation: the callee may
        know a receiver by an abstract symbol this environment has since
        bound or merged (D43).

        A need is met only by a provision *at the instantiation the callee
        means*. Finding something under the key is not enough: a method
        provision carries its own application (`bind X.m[T=Char] = ...`,
        or whatever `T` was when it was bound), and a val or fn provision
        was made while its telescope's type symbols meant something that a
        later `bind` may have changed. So the instantiation the callee
        requires — its own, read through this environment, which is the
        whole of a call's application while calls take no brackets — must
        equal the one the provision was made at, symbol for symbol: the
        calculus's `avail` on `IMth τ m θ`, application included, and its
        coherence premise on val/fn requirements (CoreMoss.v deviation 8,
        `sat_at`)."""
        mapping = []
        for need in needs:
            if isinstance(need, tuple):
                receiver, method = need
                key = self.lower.method_key(self.env, self.module, receiver, method)
                if key not in self.env.methods:
                    self.error(
                        f"calling `{callee.name}` needs `{show(key[0])}{method.name}`, "
                        "which is not available in the context here"
                    )
                what = f"`{show(key[0])}{method.name}`"
                mapping.append((need, key))
            elif need.kind == SymKind.VAL:
                if need not in self.env.vals:
                    self.error(
                        f"calling `{callee.name}` needs `{need.name}`, which is not "
                        "available in the context here"
                    )
                key = need
                what = f"`{need.name}`"
                mapping.append((need, need))
            elif need.kind == SymKind.FN:
                if need not in self.env.fns:
                    self.error(
                        f"calling `{callee.name}` needs `{need.name}`, which is not "
                        "available in the context here"
                    )
                key = need
                what = f"`{need.name}`"
                mapping.append((need, need))
            else:
                continue
            self.check_coherent(callee, need, key, what)
        return tuple(mapping)

    def check_coherent(self, callee: Symbol, need, key, what: str):
        wanted = self.lower.need_apps.get(id(callee), {}).get(need, {})
        have = self.env.apps.get(key, {})
        required = {}
        for t, ty in wanted.items():
            if ty is None:
                # Not in force where the callee was declared: it means
                # whatever this context means by it.
                ty = self.env.tymap.get(t)
            required[t] = None if ty is None else self.lower.canon(self.env, ty)
        differ = [t for t in required if required[t] != have.get(t)]
        if differ:
            self.error(
                f"calling `{callee.name}` needs {what} at "
                f"[{self.lower.show_app({t: required[t] for t in differ})}], "
                f"but the one in the context here was provided at "
                f"[{self.lower.show_app({t: have.get(t) for t in differ})}] "
                "(a provision serves only the instantiation it was made at; D62)"
            )

    # Expressions

    def synth(self, expr, expected=None):
        node, ty = self.synth_inner(expr, expected)
        return self.inject(node, ty, expected), ty

    def inject(self, node, t, expected):
        """D58: attach a tag exactly where a value enters a union. Every
        path that widens a type runs through `synth` with an `expected`, so
        this is the only place representation changes."""
        if expected is None:
            return node
        t = self.lower.canon(self.env, t)
        want = self.lower.canon(self.env, expected)
        if not isinstance(want, TUnion):
            return node
        if isinstance(t, (TUnion, TNever)) or t == want:
            return node  # already discriminated, or not a value at all
        symbol = head(t)
        if symbol is None:
            return node
        return ir.Inject(symbol, node, self.lower.slots_of(self.env, want))

    def synth_inner(self, expr, expected=None):
        if getattr(expr, "offset", -1) >= 0:
            self.at_node = expr
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
            if not self.fits(ty, self.sig.ret):
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
        if isinstance(expr, ast.Block):
            # A bare block (D65). Its tail was checked against `expected`
            # and injected there, so — as for `if` — the block as a whole
            # already has the expected type and is not injected again.
            block, ty = self.synth_block(expr, expected)
            return block, (expected if expected is not None else ty)
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
        if isinstance(expr, ast.Unary):
            return self.synth_operator(expr.op, expr.operand, [])
        if isinstance(expr, ast.Binary):
            return self.synth_operator(expr.op, expr.lhs, [expr.rhs])
        if isinstance(expr, ast.Literal):
            return self.synth_literal(expr)
        if isinstance(expr, Built):
            return expr.node, expr.ty
        self.error(f"unsupported expression {expr!r}")

    def synth_if(self, expr: ast.If, expected):
        cond = self.check_bool(expr.cond)
        then = self.check_block(expr.then, expected=expected)
        then_ty = self.block_type(expr.then, expected)
        if expr.els is None:
            if expected is not None and not self.fits(TUnit(), expected):
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
                    if expected is not None and not self.fits(body_ty, expected):
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
            return ir.Match(scrutinee, (), False), expected if expected is not None else TNever()
        if not catch_all:
            missing = [h.name for h in heads if h not in covered]
            if missing or not heads:
                self.at_node = expr
                self.error(f"match is not exhaustive; missing {missing or show(sty)}")
        if result_ty is None:
            result_ty = TNever()
        return ir.Match(scrutinee, tuple(arms), len(members(sty)) > 1), result_ty

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
                width = self.lower.slots_of(self.env, payload_ty)
                return (
                    ir.Pat(target, name, None, width),
                    [(name, payload_ty)],
                    {target},
                    False,
                )
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
            layout = dict(
                zip((n for n, _ in payload_ty.fields), self.lower.field_layout(self.env, payload_ty))
            )
            binders = []
            fields = []
            for name, sub in pattern.fields:
                if name not in field_types:
                    self.error(f"`{target.name}` has no field `{name}`")
                if sub is None:
                    binders.append((name, field_types[name]))
                    fields.append((name, name, *layout[name]))
                elif (
                    isinstance(sub, ast.PatPath)
                    and len(sub.path) == 1
                    and resolve_path(self.module, sub.path) is None
                ):
                    binders.append((sub.path[0], field_types[name]))
                    fields.append((name, sub.path[0], *layout[name]))
                elif isinstance(sub, ast.PatWild):
                    pass
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
            if target.decl.init is not None:
                return self.lower.const_val(target)
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
            if not self.fits(vty, payload_ty):
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
        needs_map = None
        if defined:
            self.lower.lower_fn(target.module, target)
            needs_map = self.require_needs(target, self.lower.needs_of[id(target)])
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
        ret = self.lower.slots_of(self.env, sig.ret)
        return ir.Call(callee, tuple(args), needs_map=needs_map, ret_slots=ret), sig.ret

    def check_args(self, name, sig: FnSig, arg_exprs):
        if len(arg_exprs) != len(sig.params):
            self.error(
                f"`{name}` takes {len(sig.params)} argument(s), got {len(arg_exprs)}"
            )
        out = []
        for (pname, pty), arg in zip(sig.params, arg_exprs):
            value, ty = self.synth(arg, expected=pty)
            if not self.fits(ty, pty):
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
            if not self.fits(vty, field_types[name]):
                self.error(
                    f"field `{name}` is `{show(vty)}`, expected `{show(field_types[name])}`"
                )
            given[name] = True
            fields.append((name, value_ir))
        missing = [n for n, _ in payload_ty.fields if n not in given]
        if missing:
            self.error(f"missing field(s) {missing} of `{target.name}`")
        # Emit fields in declaration order so the backend can lay them out.
        by_name = dict(fields)
        ordered = tuple((n, by_name[n]) for n, _ in payload_ty.fields)
        return ir.MakeRecord(target, ordered), tag_ty

    def synth_field(self, expr: ast.Field):
        obj, oty = self.synth(expr.obj)
        record = oty
        if isinstance(oty, TNominal) and oty.symbol.kind == SymKind.TAG:
            record = self.lower.payload_type(oty, self.env)
        if not isinstance(record, TRecord):
            self.error(f"`{show(oty)}` has no fields")
        layout = self.lower.field_layout(self.env, record)
        for (name, ty), (offset, width) in zip(record.fields, layout):
            if name == expr.name:
                return ir.Field(obj, expr.name, offset, width), ty
        self.error(f"`{show(oty)}` has no field `{expr.name}`")

    def synth_method(self, expr: ast.MethodCall):
        obj, oty = self.synth(expr.obj)
        h = head(self.lower.canon(self.env, oty))
        if h is None:
            self.error(f"`{show(oty)}` has no methods (it has no nominal head)")
        name = expr.path[-1]
        if len(expr.path) > 1:
            method = resolve_detached(self.module, expr.path)
            if method is None:
                self.error(f"`{'::'.join(expr.path)}` is not a detached method in scope")
        else:
            method = None
        # Attached: a call matches it *by name* in the receiver's own module,
        # with no import (D61). A defined one is an ordinary function with a
        # receiver; an abstract one is a key the context provides, so it
        # goes on to the provision lookup below carrying its symbol.
        if method is None:
            for home in (h.module, self.module):
                attached = home.attached.get((id(h), name))
                if attached is None:
                    continue
                if attached.decl.body is not None:
                    self.lower.lower_fn(attached.module, attached)
                    needs_map = self.require_needs(
                        attached, self.lower.needs_of[id(attached)]
                    )
                    sig = self.lower.fn_sig(attached, self.env.tymap, self.env, this=oty)
                    args = self.check_args(attached.name, sig, expr.args)
                    ret = self.lower.slots_of(self.env, sig.ret)
                    return (
                        ir.Call(
                            ("direct", attached), tuple(args), this=obj,
                            needs_map=needs_map, ret_slots=ret,
                        ),
                        sig.ret,
                    )
                if method is None:
                    method = attached
        # Provided: detached or abstract-attached, from the context. The
        # symbol half of the key is an ordinary lexical lookup — a detached
        # method must be in scope, and a renamed import is called by its new
        # name; it does *not* match on the name it was declared under (D61).
        # Matching a provision by spelling instead is the structural
        # matching D1 exists to rule out.
        if method is None and len(expr.path) == 1:
            method = self.module.detached.get(name)
            if method is None:
                self.error(
                    f"`.{name}` is not a detached method in scope; import it, or "
                    "call it as `mod::m` (D61)"
                )
        return self.provided_call(obj, oty, method, expr.args)

    def lang_symbol(self, short: str, name: str, table: str, why: str) -> Symbol:
        """One of the fixed library symbols an operator or a literal
        means (D68). It is found by the module that declares it, never by
        a lookup of its name where it is used — and then provided through
        the context like any other."""
        module = self.lower.lang.get(short)
        symbol = None if module is None else getattr(module, table).get(name)
        if symbol is None:
            self.error(f"{why} means `{name}` of lib/{short}.moss, which is not loaded here")
        return symbol

    def synth_operator(self, op: str, operand, args):
        """D68: `-a` is `a.neg()`, `a + b` is `a.add(b)`, and so on — calls
        of `num.moss`'s detached methods, keyed by the operand's type and
        provided by the context exactly as a method call's are."""
        name = OPERATOR_METHODS[(op, len(args))]
        method = self.lang_symbol("num", name, "detached", f"`{op}`")
        obj, oty = self.synth(operand)
        if head(self.lower.canon(self.env, oty)) is None:
            self.error(f"`{show(oty)}` has no methods (it has no nominal head)")
        return self.provided_call(obj, oty, method, args)

    # Literals (D68). A literal is nothing but arithmetic over fixed
    # library symbols, provided through the context like anything else,
    # so it means whatever the arithmetic bound here means — it wraps
    # where `Int`'s `.add` wraps and traps where that traps, and nothing
    # about it is checked at compile time:
    #
    #   integer `123`  ->  int.moss's `zero` and `one`, num.moss's `.add`
    #                      and `.mul` at `Int`: ((1 * ten) + 2) * ten + 3,
    #                      each digit and `ten` a sum of `one`s.
    #   decimal `d.dd` ->  float(m).div(float(10^k)) for its digits `m`
    #                      and `k` fractional digits, with float.moss's
    #                      `float` and num.moss's `.mul`, `.add`, `.div` at
    #                      `Float`. `m` and `10^k` are built in `Float` —
    #                      a Horner sum of `float(digit)`s, a product of
    #                      `float(ten)`s — since `Int` may be too narrow to
    #                      hold them. While both are exact the division is
    #                      the only rounding; past that, whatever `Float`'s
    #                      arithmetic computes.

    def synth_literal(self, expr: ast.Literal):
        why = f"the literal `{expr.text}`"
        if "." not in expr.text:
            return self.int_literal(expr.text, why)
        whole, fraction = expr.text.split(".")
        digits = (whole + fraction).lstrip("0")
        if not digits:
            mantissa = self.float_of(self.int_literal("0", why), why)
        else:
            ten = self.float_of(self.int_literal("10", why), why)
            mantissa = self.float_of(self.int_literal(digits[0], why), why)
            for digit in digits[1:]:
                mantissa = self.lang_method_call(mantissa, "mul", [ten], why)
                if digit != "0":
                    d = self.float_of(self.int_literal(digit, why), why)
                    mantissa = self.lang_method_call(mantissa, "add", [d], why)
        scale = self.float_of(self.int_literal("10", why), why)
        ten = scale
        for _ in range(len(fraction) - 1):
            scale = self.lang_method_call(scale, "mul", [ten], why)
        return self.lang_method_call(mantissa, "div", [scale], why)

    def int_literal(self, text: str, why: str):
        digits = text.lstrip("0")
        if not digits:
            return self.lang_val("zero", why)
        one = Built(*self.lang_val("one", why))

        def small(n: int) -> Built:
            acc = one
            for _ in range(n - 1):
                acc = self.lang_method_call(acc, "add", [one], why)
            return acc

        acc = small(int(digits[0]))
        ten = small(10) if len(digits) > 1 else None
        for digit in digits[1:]:
            acc = self.lang_method_call(acc, "mul", [ten], why)
            if digit != "0":
                acc = self.lang_method_call(acc, "add", [small(int(digit))], why)
        return acc.node, acc.ty

    def lang_val(self, name: str, why: str):
        symbol = self.lang_symbol("int", name, "names", why)
        if symbol not in self.env.vals:
            self.error(
                f"{why} means `{name}`, which is not available in the context "
                "here (assume or bind it)"
            )
        return ir.NeedVal(symbol), self.env.vals[symbol]

    def lang_method_call(self, obj, name: str, args, why: str):
        obj = obj if isinstance(obj, Built) else Built(*obj)
        args = [a if isinstance(a, Built) else Built(*a) for a in args]
        method = self.lang_symbol("num", name, "detached", why)
        return Built(*self.provided_call(obj.node, obj.ty, method, args))

    def float_of(self, value, why: str) -> Built:
        """float.moss's `float`, applied: an abstract fn the context
        provides, as any call of it would be."""
        value = value if isinstance(value, Built) else Built(*value)
        symbol = self.lang_symbol("float", "float", "names", why)
        sig = self.env.fns.get(symbol)
        if sig is None:
            self.error(
                f"{why} means `float`, which is not available in the context "
                "here (assume or bind it)"
            )
        args = self.check_args(symbol.name, sig, [value])
        ret = self.lower.slots_of(self.env, sig.ret)
        return Built(ir.Call(("env", symbol), tuple(args), ret_slots=ret), sig.ret)

    def provided_call(self, obj, oty, method: Symbol, arg_exprs):
        """A call of a method the context provides at the receiver's
        type: the lookup a method call ends in, and all an operator is."""
        name = method.name.lstrip(".")
        key = self.lower.method_key(self.env, self.module, oty, method)
        sig = self.env.methods.get(key)
        if sig is not None:
            args = self.check_args(method.name, sig, arg_exprs)
            ret = self.lower.slots_of(self.env, sig.ret)
            return ir.Call(("env", key), tuple(args), this=obj, ret_slots=ret), sig.ret
        # D62: the same method at another instantiation of the receiver is
        # the likely mistake, so say where it *is* provided.
        elsewhere = [
            show(r) for r, m in self.env.methods
            if m is method and head(r) is key[0].symbol and r != key[0]
        ]
        if elsewhere:
            self.error(
                f"`.{name}` is provided at `{'`, `'.join(elsewhere)}`, not at "
                f"`{show(oty)}`: a method provision serves only the instantiation "
                "it was made at (D62)"
            )
        self.error(
            f"no method `.{name}` is available for `{show(oty)}` in the context here"
        )
