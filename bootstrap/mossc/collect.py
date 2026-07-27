"""Collect: module loading, module-level scopes, and export tables.

This is stage 3 of the pipeline (docs/design/semantics.md section 11), minus
the parts that inherently need types: expression-level resolution happens in
lower, because method calls resolve through the receiver's type (D36).

Per module this stage produces:

- `names`: every plain name in scope (own declarations plus imports),
- `detached`: detached method names in scope (the `.m` namespace),
- `attached`: attached methods keyed by (receiver symbol id, method name),
- `aliases`: module aliases in scope,
- export tables for each of the above except `attached` (attached methods
  travel with their receiver symbol and need no separate namespace).

Import collisions are errors per D44: one local name may not refer to two
different symbols. Explicit `use` names and `as` aliases re-export; glob
imports do not (D9). Import cycles are errors (D10).
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import PurePosixPath

from . import ast
from .parse import parse


class CollectError(Exception):
    def __init__(self, module: str, message: str):
        super().__init__(f"{module}: {message}")
        self.module = module
        self.message = message


class SymKind(Enum):
    TYPE = auto()  # abstract type symbol
    UNIT = auto()
    ALIAS = auto()
    TAG = auto()
    VAL = auto()
    FN = auto()  # abstract or defined; see Symbol.decl.body
    CONTEXT = auto()
    METHOD = auto()  # attached (receiver is not None) or detached
    FUNCTOR = auto()  # D55: a map from one structure to another


DECL_KINDS = {
    ast.Tydef: SymKind.TYPE,
    ast.Unitdef: SymKind.UNIT,
    ast.Aliasdef: SymKind.ALIAS,
    ast.Tagdef: SymKind.TAG,
    ast.Valdef: SymKind.VAL,
    ast.Ctxdef: SymKind.CONTEXT,
    ast.Functordef: SymKind.FUNCTOR,
}


@dataclass(eq=False)
class Symbol:
    module: "Module"
    name: str  # display name; detached methods include the leading dot
    kind: SymKind
    decl: object  # the AST declaration node
    receiver: "Symbol | None" = None  # attached methods only
    # Assume items lexically enclosing the declaration, outermost first.
    assumes: tuple[ast.AssumeItem, ...] = ()

    def __repr__(self):
        return f"<{self.kind.name} {self.module.path}:{self.name}>"


@dataclass(eq=False)
class Module:
    path: str  # normalized, repo-relative or absolute; unique key
    tree: ast.File = None
    source: str = None
    names: dict = field(default_factory=dict)  # str -> Symbol
    detached: dict = field(default_factory=dict)  # str -> Symbol (no dot in key)
    attached: dict = field(default_factory=dict)  # (id(Symbol), str) -> Symbol
    aliases: dict = field(default_factory=dict)  # str -> Module
    export_names: dict = field(default_factory=dict)
    export_detached: dict = field(default_factory=dict)
    export_aliases: dict = field(default_factory=dict)

    def __repr__(self):
        return f"<Module {self.path}>"


@dataclass
class Program:
    modules: dict  # path -> Module
    order: list  # Modules in dependency order (imports before importers)
    entry: Module


def default_read(path: str) -> str:
    with open(path, encoding="utf-8") as f:
        return f.read()


class Loader:
    def __init__(self, read=default_read, prelude: str | None = None):
        self.read = read
        self.prelude = prelude
        self.modules: dict[str, Module] = {}
        self.order: list[Module] = []
        self.loading: list[str] = []

    def normalize(self, base: str | None, path: str) -> str:
        if base is None:
            joined = PurePosixPath(path)
        else:
            joined = PurePosixPath(base).parent / path
        parts = []
        for part in joined.parts:
            if part == ".":
                continue
            if part == ".." and parts and parts[-1] != "..":
                parts.pop()
            else:
                parts.append(part)
        return str(PurePosixPath(*parts)) if parts else "."

    def wants_prelude(self, path: str) -> bool:
        if self.prelude is None or path == self.prelude:
            return False
        # Files that make up the standard library don't auto-import it.
        prelude_dir = str(PurePosixPath(self.prelude).parent)
        return not path.startswith(prelude_dir + "/")

    def load(self, path: str, base: str | None = None) -> Module:
        path = self.normalize(base, path)
        if path in self.modules:
            return self.modules[path]
        if path in self.loading:
            cycle = " -> ".join(self.loading[self.loading.index(path) :] + [path])
            raise CollectError(path, f"import cycle: {cycle}")
        self.loading.append(path)
        module = Module(path)
        module.source = self.read(path)
        module.tree = parse(module.source)
        deps = []
        if self.wants_prelude(path):
            deps.append((self.load(self.prelude), ast.Import(self.prelude, None, True, [])))
        for imp in module.tree.imports:
            deps.append((self.load(imp.path, base=path), imp))
        self.loading.pop()
        self.modules[path] = module
        declare(module)
        for target, imp in deps:
            link(module, target, imp)
        self.order.append(module)
        return module


def load(path: str, read=default_read, prelude: str | None = None) -> Program:
    loader = Loader(read=read, prelude=prelude)
    entry = loader.load(path)
    return Program(loader.modules, loader.order, entry)


def declare(module: Module) -> None:
    """Two passes over the declarations: plain names, then attached methods
    (whose receiver names must already be defined)."""
    pending_attached = []

    def define_name(name: str, symbol: Symbol) -> None:
        if name in module.names:
            raise CollectError(module.path, f"duplicate definition of `{name}`")
        module.names[name] = symbol

    def walk(decl, assumes: tuple) -> None:
        if isinstance(decl, ast.Assume):
            inner = assumes + tuple(decl.items)
            for d in decl.decls:
                walk(d, inner)
        elif isinstance(decl, ast.Fndef):
            fn_name = decl.name
            if fn_name.dotted:
                if decl.body is not None:
                    raise CollectError(
                        module.path, f"detached method `.{fn_name.name}` cannot have a body"
                    )
                if fn_name.name in module.detached:
                    raise CollectError(
                        module.path, f"duplicate definition of `.{fn_name.name}`"
                    )
                module.detached[fn_name.name] = Symbol(
                    module, f".{fn_name.name}", SymKind.METHOD, decl, assumes=assumes
                )
            elif fn_name.receiver is not None:
                pending_attached.append((decl, assumes))
            else:
                define_name(
                    fn_name.name, Symbol(module, fn_name.name, SymKind.FN, decl, assumes=assumes)
                )
        else:
            kind = DECL_KINDS[type(decl)]
            define_name(decl.name, Symbol(module, decl.name, kind, decl, assumes=assumes))

    for decl in module.tree.decls:
        walk(decl, ())

    for decl, assumes in pending_attached:
        receiver_name = decl.name.receiver
        receiver = module.names.get(receiver_name)
        if receiver is None:
            raise CollectError(
                module.path,
                f"attached method receiver `{receiver_name}` is not in scope",
            )
        key = (id(receiver), decl.name.name)
        if key in module.attached:
            raise CollectError(
                module.path,
                f"duplicate definition of `{receiver_name}.{decl.name.name}`",
            )
        module.attached[key] = Symbol(
            module,
            f"{receiver_name}.{decl.name.name}",
            SymKind.METHOD,
            decl,
            receiver=receiver,
            assumes=assumes,
        )

    module.export_names.update(module.names)
    module.export_detached.update(module.detached)


def bind_name(module: Module, table: dict, name: str, symbol) -> None:
    existing = table.get(name)
    if existing is not None and existing is not symbol:
        raise CollectError(
            module.path, f"`{name}` would refer to two different symbols (D44)"
        )
    table[name] = symbol


def link(module: Module, target: Module, imp: ast.Import) -> None:
    """Wire one import (possibly the synthetic prelude glob) into scope."""
    if imp.alias is not None:
        bind_name(module, module.aliases, imp.alias, target)
        module.export_aliases[imp.alias] = target
    if imp.glob:
        for name, symbol in target.export_names.items():
            bind_name(module, module.names, name, symbol)
        for name, symbol in target.export_detached.items():
            bind_name(module, module.detached, name, symbol)
        for name, sub in target.export_aliases.items():
            bind_name(module, module.aliases, name, sub)
        return
    for item in imp.uses:
        local = (item.alias or item.name).name
        if item.name.dotted:
            symbol = target.export_detached.get(item.name.name)
            if symbol is None:
                raise CollectError(
                    module.path,
                    f"`{target.path}` does not export `.{item.name.name}`",
                )
            bind_name(module, module.detached, local, symbol)
            module.export_detached[local] = symbol
        else:
            symbol = target.export_names.get(item.name.name)
            if symbol is None:
                sub = target.export_aliases.get(item.name.name)
                if sub is not None:
                    bind_name(module, module.aliases, local, sub)
                    module.export_aliases[local] = sub
                    continue
                raise CollectError(
                    module.path, f"`{target.path}` does not export `{item.name.name}`"
                )
            bind_name(module, module.names, local, symbol)
            module.export_names[local] = symbol


def resolve_path(module: Module, path: ast.Path):
    """Resolve a possibly module-qualified path to a Symbol or Module.

    Returns None if the first segment is not in scope; raises CollectError
    when a qualified segment is missing from the named module's exports (the
    name is then provably wrong, not merely local).
    """
    head, *rest = path
    current = module.names.get(head)
    if current is None:
        current = module.aliases.get(head)
    if current is None:
        return None
    for segment in rest:
        if not isinstance(current, Module):
            raise CollectError(module.path, f"`{head}` is not a module")
        symbol = current.export_names.get(segment)
        if symbol is None:
            symbol = current.export_aliases.get(segment)
        if symbol is None:
            raise CollectError(
                module.path, f"`{current.path}` does not export `{segment}`"
            )
        head = segment
        current = symbol
    return current


def resolve_detached(module: Module, path: ast.Path) -> Symbol | None:
    """Resolve a detached-method path: `m` in scope or `mod::m` via alias."""
    if len(path) == 1:
        return module.detached.get(path[0])
    target = resolve_path(module, path[:-1])
    if not isinstance(target, Module):
        return None
    return target.export_detached.get(path[-1])
