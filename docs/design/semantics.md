# Moss semantics: working decision log

This is a working document for the `ml-modules` iteration, not user-facing
documentation. Its job is to pin down the language precisely enough to write a
bootstrap compiler. It was produced by inventorying every construct actually
used in `src/` and `docs/learn/hello.md` (treated as the most current
expressions of the design), reconciling them against `docs/reference/syntax.md`
and `lib/` (both stale), and proposing answers to the questions none of those
artifacts settle.

Every decision point is tagged:

- **DECIDED** — already evident from the current artifacts or from explicit
  statements by the language designer; recorded here so it's written down.
- **PROPOSED** — a concrete answer is proposed; the compiler will assume it
  unless overridden. These need sign-off.
- **OPEN** — genuinely undecided; a default is suggested but the question
  deserves real thought before code depends on it.

Section 13 indexes all PROPOSED/OPEN points for easy discussion.

## 1. Design thesis

Moss separates **scope** from **context**:

- *Scope* is knowing what a name refers to. It is resolved purely lexically,
  from imports and enclosing declarations. Scope resolution never depends on
  types or on what has been bound.
- *Context* is having the thing a name refers to. Top-level declarations
  without a definition (`type T;`, `val v: T;`, `fn f(): T;`) introduce
  *abstract symbols*: names that are in scope but that nobody has yet provided.
  `assume` declares that a region of code requires certain symbols to be
  provided; `bind` provides them.

**[D1] DECIDED (thesis).** Context requirements are sets of *symbols*, and a
requirement is satisfied only by naming that exact symbol — either by an
enclosing `assume` of the same symbol, or by an explicit `bind` of the same
symbol. There is no structural matching, no search by type shape, and no
specificity ranking. This is the deliberate retreat from the second iteration
(PR #14), whose lowering phase (`Lower::resolve_need` / `synthesize` /
`unique_option`) had to *search* in-scope providers by shape and rank them.
Everything else in this document is downstream of this rule; any future
feature that would reintroduce provider search should be treated as suspect.

**[D2] DECIDED (static/dynamic split).** What it means to "provide" a symbol
depends on its kind, and this determines the compilation model:

- Providing a **type** symbol means choosing a concrete representation for it.
  Type bindings are static: they must be resolvable at compile time, and they
  drive monomorphization.
- Providing a **fn** symbol means supplying code for it. Fn bindings are
  static as code, but the supplied function may itself capture context, so a
  fn binding is a (code, captured-context) pair — statically a direct call,
  with the captured context passed as data.
- Providing a **val** symbol means supplying a runtime value. Val bindings
  are dynamic data, passed along context edges at runtime (e.g. as hidden
  parameters after lowering).

So after lowering, types and fns disappear into specialization, and vals are
the only context that exists at runtime.

## 2. Lexical structure

**[D3] DECIDED (keywords).** The keyword set is:

```
as assume bind break context else fn for if import let loop match return
type unit use val var while
```

Changes from `docs/reference/syntax.md`: `static` is dropped; `break`, `for`,
`loop`, `match`, `return`, `unit` are added (all six are used in `src/`).
`src/token.moss` and `src/lex.moss` must gain tokens for the six added
keywords.

**[D4] DECIDED (no literals).** There are no literal expressions of any kind:
no integer, char, or string literals. hello.md's `putchar(char::H)` style is
the current reality. Consequences:

- The token classes _uint32_/_int32_/_uint64_/_int64_/_uint_/_int_/_char_ are
  removed from the lexer; `src/token.moss`'s `Uint32`..`Char` units go away.
- `src/lex.moss` currently compares against char literals (`c == '!'`) and
  `src/cli.moss` computes `pos + 1`; both are stale and need rewriting against
  named constants (see [D5] and the errata in §12).
- hello.md's `fn main(): () { println("Hello, world!"); }` example
  (hello.md:73) is stale within its own document and needs a literal-free
  replacement.

**[D5] PROPOSED (strings only in import position).** The _string_ token
survives in the lexer solely because `import` paths need it
(`import "./lex.moss" as lexer;`). Strings are not expressions; an import path
is the only place the parser accepts the token. Runtime `String` values enter
a program exclusively through the environment: `first_arg()`, `Path.read`,
etc. (This is actually sufficient for a compiler, which reads files and
writes bytes.) Char values come from named vals like the `char` module in
hello.md; small integers from named vals (`zero`, plus whatever Std grows,
e.g. `one` or an `Int.succ`-style function).

**[D6] DECIDED (comments).** `#` to end of line; comments and whitespace are
not tokens. `#!` shebang lines fall out for free.

**[D7] PROPOSED (symbol tokens).** Exactly the one- and two-character symbol
tokens currently listed in syntax.md and `src/token.moss`:
`! % & ( ) * + , - . / : ; < = > [ ] ^ { | }` and
`!= :: << <= == >= >>`. (Several currently have no grammar production that
uses them — e.g. `&`, `^`, `<<` — which is fine; they're reserved.)

## 3. Modules, imports, and scope

**[D8] DECIDED.** A module is a source file. `import` is the only inter-file
mechanism:

```moss
import "./lex.moss" as lexer;          # qualified access: lexer::lex
import "./token.moss" use Eof, Name;   # unqualified names into file scope
import "./prelude.moss" use *;         # glob (used by src/main.moss)
import "./wasip1.moss" as wasi use Wasi;  # both at once
```

`as` binds a module alias usable with `::`; `use` copies specific names (or
all exported names, with `*`) into the importing file's scope. Importing is a
pure scoping construct with no context effect: importing an abstract symbol
does not provide it. The glob form must be added to the grammar (syntax.md's
**Import** lacks `*`).

**[D9] PROPOSED (exports).** Every top-level declaration of a module is
exported; there is no visibility control yet. `use *` imports all of them.
Names a module itself imported are *not* re-exported by `use *` (no transitive
glob), but can be re-exported deliberately the way `src/prelude.moss` does —
a file consisting only of imports, whose own importers then `use *` it.
(That pattern only works if plain `use` — as opposed to `use *` — *does*
re-export; so the rule is: explicit `use` names become part of the module's
exports, glob imports do not.)

**[D10] PROPOSED (module identity & instantiation).** A module is not a unit
of instantiation and has no state; it is a bag of declarations, elaborated
once. All parameterization happens per-symbol via assume/bind, not per-module.
Two importers of `cell.moss` see the same symbols `Cell`, `Cell.read`, etc.;
the shared abstract `T` in `inner.moss` is what lets `IsCell[T=Int, ...]` from
one file and `bind cell=...` from another agree. Import cycles: **OPEN**, but
propose forbidding them for the bootstrap (the compiler pipeline in
`src/cli.moss` already assumes a topological order of some graph).

## 4. Declarations

The declaration forms, as actually used in `src/`:

| form | meaning |
|---|---|
| `type T;` | abstract type symbol (a "need") |
| `unit X;` | concrete nominal type with exactly one value, also named `X` |
| `type X = Ty;` | transparent alias |
| `type X Ty;` | nominal (tag) type wrapping payload `Ty` |
| `type X { f: Ty, ... };` | nominal type with record payload (tag form where the payload is a record) |
| `type X = \| A \| B;` | alias for a union (see §5) |
| `val v: Ty;` | abstract value symbol |
| `fn f(x: Ty): Ty;` | abstract function symbol (signature only) |
| `fn f(x: Ty): Ty { ... }` | defined function |
| `fn T.m(x: Ty): Ty;` / `{...}` | method on receiver type `T` (§9) |
| `context C = item, ...;` | named context (§6) |
| `assume items { decls }` | requirement block wrapping declarations |

**[D11] DECIDED.** Omitting a return type means the unit type `()`
(hello.md). A `fn` with a body whose requirements aren't satisfied *defines*
a function that *requires* them; there is nothing wrong with a defined
function deep inside `assume` blocks.

**[D12] PROPOSED (which declarations may appear where).** All of the above
are declarations and may appear at top level or inside `assume` blocks,
arbitrarily nested. Function *bodies* contain only statements/expressions —
no nested declarations except through `bind` (no local `fn`/`type`). This
matches all of `src/`.

**[D13] OPEN (nominal-union shorthand).** `src/option.moss` declares
`type Option | None | Some;` — no `=`, so by the table above this is a
*nominal* type whose payload is the union `None | Some`, whereas
`src/lex.moss`'s `type Token = | Eof | ...;` is a transparent alias. But
`src/parse.moss` then uses `Some (...)` / `None` directly where an
`Option[T=TokenId]` is expected, which only typechecks if `Option` is
transparent (or if nominal-over-union types auto-inject, which smells like
search). Proposal: this is a typo in `option.moss`; it should be
`type Option = | None | Some;`. If nominal unions are instead intentional,
their injection/projection rules need defining.

**[D14] PROPOSED (drop declaration-site `Needs`).** syntax.md attaches an
optional `[Need, ...]` clause to every declaration form (**Needs**) and a
`static` marker on needs. Nothing in `src/` uses either; requirements are
expressed exclusively by `assume` blocks, and staticness is determined by
symbol kind ([D2]). Drop both from the grammar. (Square-bracket *application*
at use sites, like `Range[T=NameId]`, absolutely stays — see §6.)

## 5. Types

**[D15] PROPOSED.** Type expressions:

- A path to a type symbol, alias, or nominal type: `Token`, `parse::TypeId`,
  possibly applied: `Option[T=TokenId]`, `Range[T=NameId]`.
- Unit `()` and tuples `(A, B)` (grammar only; unused in `src/` so far —
  keep tuples in the grammar, defer implementing them).
- Structural records `{ from: TokenId, name: Option[T=TokenId] }`. Records
  are structural; nominality comes only from tag declarations wrapping them.
- Unions `A | B | C`, including the leading-pipe multiline form. Members
  must be *nominal* types (units, tags) with distinct heads — the union is
  discriminated by nominal identity, and matching tests that identity.
  `Char | Eof` (with `Char` an abstract type symbol!) is allowed and means
  the union is discriminable only once `Char`'s representation is known;
  see [D16].
- The empty union `|`: the uninhabited/divergence type. `fn err(...): |;`
  declares a function that cannot return; `match e {}` on an expression of
  type `|` is the eliminator and has any type. `type Type = |;` in
  `src/parse.moss` is a placeholder alias.

**[D16] OPEN (unions containing abstract types).** `Char | Eof` requires
that, after all type binds are resolved, union members remain disjoint (a
binding `Char=Eof` would make `Char | Eof` ambiguous). Proposal: a
post-monomorphization check that every union's members have pairwise distinct
heads, with an error naming the offending bind. Also **OPEN**: whether a
non-nominal type (e.g. bare `Int`, a record) may be a union member when it's
behind an abstract symbol like `Char`. Proposal: yes, any type may be a
member as long as heads stay distinct — units and tags are cheap enough to
wrap things in when they don't.

**[D17] PROPOSED (subtyping is injection-only).** A value of a union member
type implicitly injects into any union containing that member (this is how
`lex()` returns `Eof` where `Token` is expected). There is no other implicit
conversion, no width/depth record subtyping, and no union-to-union coercion
beyond re-injection of each member (defer even that). Nominal tags do not
inject into anything implicitly; `Some x` constructs a `Some`, which then
injects into `Option`'s union because it is a member.

**[D18] PROPOSED (type identity / applicativity).** Type identity is
structural over (declaration, static bindings): `Import` elaborated with
`TokenId=Int` is the *same* type everywhere it arises with those same
bindings, and different from `Import` with `TokenId=Int32`. Runtime val
bindings and fn bindings never participate in type identity. (This is the
applicative-functor choice; the generative alternative would make separately
written `bind parser::TokenId=Int;` statements produce incompatible parser
ASTs, which would make `src/cli.moss`-style wiring unusable.)

## 6. Contexts, `assume`, and implicit parameterization

**[D19] DECIDED (what a context is).** A `context` declaration names a finite
set of context items:

```moss
context Parsing =
  TokenId, next, now, peek,
  AstIds, AstLists,
  name_list, IsList[T=NameId, List=NameList],
  TokenSet, expected, TokenSet.add, err,
;
```

An item is a reference to a symbol of any kind — type, val, fn, method, or
another context — optionally with square-bracket bindings applied. Context
references flatten: assuming `Parsing` is exactly assuming its members,
recursively. A context is a compile-time artifact only; there are no
first-class context values at runtime (what exists at runtime is the val data
of [D2]).

**[D20] DECIDED (assume).** `assume xs { decls }` adds the items `xs` to the
requirement set of every declaration inside. Requirement sets nest by union:
`src/lex.moss`'s `lex` sits inside `assume Char { ... assume next_byte { }}`
and so requires `{Char, next_byte}`. Assuming a symbol also makes everything
it *mentions* usable: assuming `next_byte` (whose signature returns
`Char | Eof`) is only well-formed in a region that also assumes `Char`, which
is why the blocks nest in that order. **PROPOSED:** that well-formedness rule
stated precisely — an `assume x` is legal only where every abstract symbol
appearing in `x`'s declared signature/definition is already assumed or bound.

**[D21] DECIDED (implicit parameterization = generics).** A declaration's
requirement set is its parameter list. `Range` is declared inside `assume T`,
so `Range` is implicitly parameterized by `T`; `Range[T=NameId]` applies it.
This is the entire generics mechanism — there are no separate type
parameters. Square-bracket application may bind any subset of the
requirements ([D22]); whatever is left unbound flows into the requirement set
of the referencing declaration. An *unapplied* reference to `Range` inside
another `assume T` region refers to the same `T` symbol and thus stays
coherent (the `inner.moss` shared-`T` idiom).

**[D22] PROPOSED (partial application).** `C[X=A]` with the remaining
requirements unbound is legal both in context items and in type expressions;
elaboration just records the partial substitution. (Used implicitly all over
`src/`; stating it explicitly.)

**[D23] PROPOSED (subsumption).** A function may be called wherever its
requirement set is a subset of what the caller has (assumed or bound) — the
"main may assume any subset of Std" rule from hello.md, generalized: extra
available context is simply dropped. Two contexts are compatible by flattened
set inclusion; there is no nominal identity to contexts themselves.

**[D24] PROPOSED (assume-with-binding).** The grammar allows `assume` items
to be **Binding**s, i.e. `assume Foo[T=Int] { ... }`. Meaning: the region
requires `Foo` with `T` already fixed to `Int` — the requirement propagated
outward is the applied one. Unused in `src/` so far; keep in grammar, low
implementation priority.

## 7. `bind`

The novel construct; every rule here should be treated as needing sign-off.

**[D25] PROPOSED (form and scope).** `bind x=e;` is a *statement* (drop
**Bind** from the **Expr** production in syntax.md). Its effect is lexical
and extends from the statement to the end of the enclosing block. A later
`bind` of the same symbol in the same or an inner block shadows. Binds do not
escape the block upward or survive into the next iteration of a loop.

**[D26] PROPOSED (what may be bound, and to what).**

- `bind v=e;` where `v` is an abstract val: `e` is evaluated (once, at the
  bind) and its value provides `v`. Requires `e`'s type to equal `v`'s
  declared type after current substitutions.
- `bind T=U;` where `T` is an abstract type and `U` a type expression:
  static substitution from here down.
- `bind f=g;` where `f` is an abstract fn and `g` a defined (or currently
  provided) fn whose signature matches `f`'s after current substitutions.
  `g`'s own requirements must be satisfied *at the bind site*, and are
  captured: the binding carries the code of `g` plus the context data it
  needs (this is how `cli::next_byte`, which requires `cell` and `string`,
  gets handed to the lexer as `lexer::next_byte`).
- The bound symbol may be module-qualified (`bind lexer::next_byte=...;`).
  Binding another module's symbol is the normal way to instantiate its
  machinery; there is nothing special about it ([D10]).

**[D27] PROPOSED (no inference at binds).** A bind never infers other binds.
Signature matching in [D26] is checked *after* substitutions already in
force, and it is an error — not a unification opportunity — if abstract
symbols remain unmatched. Concretely: `src/cli.moss`'s
`bind lexer::next_byte=next_byte;` is currently *incomplete*, because
`lexer::next_byte`'s type mentions `lexer::Char` while the local `next_byte`
returns Std's `Char`; the code must first say `bind lexer::Char=Char;`.
Allowing that bind to be inferred from the function's type would be
[D1]-style search through the back door. (The same applies to
`bind parser::next=next;` needing `parser::TokenId` bound first — which
cli.moss in fact does, in the right order.)

**[D28] PROPOSED (satisfaction rule, restated).** An expression may use an
abstract symbol `s` (call it, read it, mention the type) iff `s` is in the
enclosing declaration's requirement set or `s` is bound in an enclosing
block. A call to a defined function `g` requires each element of `g`'s
requirement set to be available the same way — matched by symbol identity,
with square-bracket applications composed. This check is the core of the
lowering phase, and per [D1] it is a set-membership test, not a search.

**[D29] OPEN (bind-returning functions).** syntax.md contains two unused
related productions: a **Fndef** return position of `bind Needs`
(`fn f(): bind cell, string`) and a **Call** alternative inside **Bind**
(`bind f();`). The natural reading: `f` returns a bundle of bindings, and
`bind f();` applies them to the caller's remaining block — a way to package
setup like `src/cli.moss`'s `parse` prologue (`let pos = zero(); bind
cell=pos; bind string=text;`) behind one call. Nothing in `src/` uses it.
Proposal: keep the idea on the shelf, cut it from the grammar and from the
bootstrap until the plain form is proven; it's the first feature whose
absence hurts when writing `src/` for real, so it will earn its way in
quickly if needed.

**[D30] OPEN (recursion × static binds).** Because type/fn binds drive
specialization, a recursive function that re-binds a *type* on the recursive
path could demand infinitely many specializations. `src/lower.moss`'s
`scope_items` recursion is fine (it rebinds only the val `ctx`). Proposal:
the bootstrap interpreter doesn't care (it just carries an environment);
detection of unbounded static specialization is deferred to the
monomorphizing backend, which can put a depth limit on the (declaration,
static-bindings) instantiation graph and report the cycle.

## 8. Expressions, statements, and patterns

**[D31] PROPOSED (statements).**

```
let x = e;      # immutable local
var x = e;      # reassignable local
x = e;          # reassignment of a var only
e;              # expression statement
bind ...;       # see §7
while e { ... }
loop { ... }    # with break
for p in e { ... }
return e?;      # early return; `return` alone returns ()
break;          # loops only; carries no value
```

`let`/`var` bind names, not patterns, for now. `var` permits reassignment of
the local slot only; it creates no aliasable storage — shared or captured
mutable state goes through `Cell` (which is why `src/cli.moss` threads a
`CellInt` for the lexer position instead of a captured `var`). Blocks are
expressions; the final expression without `;` is the block's value, `()`
otherwise.

**[D32] PROPOSED (expressions).** Parenthesization, `()` unit, paths
(`lexer::lex`, `char::H`), calls `f(a, b)`, method calls `x.m(a)`, field
access `x.f` (which projects through a nominal-record tag: `imp.name` where
`imp: Import`), record construction `Import { from, name, names = ns }`
(shorthand when the local variable name equals the field name), tag
construction by juxtaposition `Some (expect(Name))`, unit values by name
(`Eof`), unary `!`, the binary operators of syntax.md, `if`/`else if`/`else`
as an expression, and `match`.

**[D33] OPEN (operators without literals or traits).** `src/cli.moss` uses
`pos >= string.length()` and `pos + 1`; the old `lib/ops.moss`
Lhs/Rhs/AddOut machinery is dead. What do operators mean now? Proposal:
binary operators are *syntax* for method calls on the left operand
(`a + b` ≡ `a.add(b)`, `a >= b` ≡ `a.ge(b)`, `a == b` ≡ `a.eq(b)`), so
operator availability rides the same rails as any method (§9): usable when a
matching method for the receiver's type is in context. Std provides them for
`Int` etc. No user-facing precedence surprises: precedence per syntax.md's
eventual table (to be written; currently the grammar note admits ambiguity).

**[D34] PROPOSED (match).**

```moss
match scrutinee {
  Eof => expr,                    # unit pattern
  Some token => expr,             # tag pattern binding payload
  parse::Assume assump => expr,   # qualified tag pattern
  parse::Symbol { name = token } => expr,   # tag + record destructure
  parse::Fn { sig } => expr,      # field shorthand binds `sig`
  c => expr,                      # binder (irrefutable)
  _ => expr,                      # wildcard
}
```

Arms are `pattern => expr,` with the comma optional after a block arm.
Matching is by nominal head; exhaustiveness over the scrutinee's union is
required (an irrefutable binder or `_` arm satisfies it). `match e {}`
requires `e : |`. Note `src/parse.moss`'s `tree()` matches a `Token` against
only three heads with no wildcard — stale code under this rule (§12), and
`src/lower.moss:103,106` omit `=>` — recorded as typos.

**[D35] OPEN (iteration protocol).** `for` is used three ways in `src/`:
`for using in imp.names` (a `Range[T=NameId]`), `for node in
graph.topological_sort()`, and `for Import imp in imports` — the last with a
*type-ascribed binder* and iterating something (`imports`) that is never
declared. A protocol is needed: what must `e` provide for `for p in e`?
Options: (a) desugar to a context-supplied pair `e.iter()`/`Cell`-driven
`next(): T | Done`, riding §6 machinery; (b) compiler-magic iteration over a
few blessed types (`Range`, lists) for the bootstrap. Proposal: (b) now, (a)
when the method story (§9) settles. The typed-binder form
(`for Import imp in ...`) suggests heterogeneous iteration with a filter or
refinement — needs a real design; suggest cutting it and using `match` in the
loop body instead.

## 9. Methods — OPEN

Attached declarations `fn Cell.read(): T;`, `fn TokenId.name(): StrId;`,
`fn String.length(): Int;` exist throughout `src/`, are referenced in
contexts by their full names (`Path.join`, `TokenSet.add`, `IsCell =
Cell.read, Cell.write`), and are called as `x.m(...)`. The old detached
`.name`/`This` design from `lib/` is dead, and the designer has said the new
story "still needs a bit of fleshing out." Until then, the bootstrap assumes
the minimal reading:

**[D36] PROPOSED (minimal method semantics).** `fn T.m(args): R` declares an
item whose name is the pair `T.m`; `T` must be a type symbol in scope, and
the declaration implicitly takes a receiver parameter of type `T`. A call
`x.m(a)` resolves as follows: determine the (already elaborated) type of `x`;
among the requirement set / binds in force, there must be exactly *one*
available method named `m` whose receiver type is that type — ambiguity or
absence is an error at the call site. This is name-plus-receiver-identity
lookup, not search: nothing is inferred, coerced, or ranked. Field access and
method call are syntactically distinguishable only by the argument list
(`x.f` vs `x.f()`); record fields and method names on the same type must not
collide.

Known open sub-questions for the real design: can methods attach to aliases,
unions, or only to type symbols and tags? Are `T.m` items assumable
independently of `T` (currently yes — `IsCell` includes `Cell.read` while
`T`/`Cell` are assumed separately)? Is there any receiver-position
auto-injection into unions ([D17] says no)?

## 10. Execution model and entry point

**[D37] DECIDED.** `main` takes no parameters, returns `()`, and may assume
any subset of `Std` (hello.md). The only primitively provided contexts are
`Wasm` (instruction intrinsics, `src/wasm.moss`) and `Wasi` (host imports,
`src/wasip1.moss`); `Std` is meant to be *implemented in Moss* on top of them
and bound by a driver, which is the `# TODO: Bind Std` in `src/main.moss`.

**[D38] PROPOSED (bootstrap shortcut).** The bootstrap compiler provides
`Std` natively at first (Python implementations of `String`, `Path`, `Cell`,
`print`, chars, ints), *and* provides `Wasm`/`Wasi` natively (Python ints and
a bytearray memory), so that `moss run` works before the in-Moss `Std`
implementation exists. Switching `Std` from native to in-language is then a
milestone that exercises §7 hard, on purpose. The set of native symbols is
kept in one Python table so the two layers can't drift apart silently.

**[D39] PROPOSED (linking model for `moss run file.moss`).** The CLI
elaborates the file, checks `main`'s requirement set ⊆ `Std`'s flattened
items, then runs `main` with the native (later: in-language) `Std` bindings
in force. Extra assumed-but-unprovided symbols produce hello.md's two
distinct errors: out-of-scope (name resolution) vs not-in-context
(requirement not satisfiable) — worth implementing as literally two different
diagnostics from day one, since the distinction is the language's pedagogical
core.

## 11. Bootstrap compiler pipeline (Python)

The agreed shape, answering "checker + tree-walking interpreter vs
monomorphizing abstract interpreter":

1. **Lex** and **parse** to a plain AST.
2. **Collect**: per-module symbol tables, import graph, scope resolution.
   Every name in the AST resolves to a symbol id or errors here. (Scope,
   fully discharged.)
3. **Lower**: the static-semantics phase and the point of the exercise.
   Computes requirement sets, checks every [D20]/[D26]/[D28] rule, and
   elaborates to a core IR in which context is *explicit*: every function
   carries its ordered requirement list; every call site records, per
   requirement, exactly which enclosing assumption or bind satisfies it;
   every type is resolved to (declaration, static-bindings) form ([D18]).
   Nothing downstream ever looks at an `assume` or a `bind` again.
4. **Interpret** the core IR. Not the AST: the interpreter's environment is
   exactly the explicit context structure lowering produced, so the lowering
   phase is load-bearing (and therefore debugged) from the first program run.
   Type and fn bindings are entries in the interpreter's static environment;
   val bindings are runtime data — the same [D2] split the real backend
   needs.
5. **Later — monomorphizing Wasm backend**: partial evaluation of the same
   core IR with respect to its static parts, i.e. the abstract-interpreter
   approach of the existing Rust `wasm.rs`, but over an IR where every
   contextual dependency is already explicit and nominal. The hypothesis to
   validate: with [D1], stage 5 contains no resolution logic at all, only
   specialization and instruction selection.

So: yes to "a checking phase similar to the eventual lowering," but the
interpreter consumes lowering's *output* rather than combining side-table
data with the AST — that keeps stage 3 honest and makes stage 5 a swap-out of
stage 4 rather than a second reckoning with the static semantics.

**[D40] PROPOSED (test strategy).** Corpus-driven: every file in `src/` and
every doc example must lex and parse from day one (golden AST dumps);
elaboration and execution tests grow file-by-file starting from a rewritten
literal-free hello. `tests/errors/` continues as golden-diagnostics tests.
The old `examples/` and `lib/` are excluded until rewritten.

## 12. Errata in existing artifacts

Stale things this document supersedes; each needs a mechanical fix once the
decisions above are confirmed.

- `docs/reference/syntax.md`: keyword list ([D3]); no literals ([D4]); add
  `match`/`loop`/`break`/`return`/`for`/`unit` productions and union/never
  types ([D15], [D34]); add `use *` ([D8]); drop declaration-site Needs and
  `static` ([D14]); drop **Bind** from **Expr** ([D25]) and the
  bind-returning forms pending [D29]; grammar's `assume List[Binding];`
  statement form is unused — **OPEN** whether to keep a braceless
  rest-of-file `assume Std;` form as sugar (hello.md's "typical pattern"
  would benefit).
- `docs/learn/hello.md`: the `println("Hello, world!")` example at line 73
  contradicts [D4]; `putchar`/`char::*` need to actually exist in the new
  `lib/`.
- `src/token.moss`, `src/lex.moss`: token set per [D3]/[D4]; keyword and
  name lexing missing entirely; char-literal comparisons (`c == '!'`) need
  named char constants; `lex()` must gain the two-character-symbol,
  whitespace/comment, keyword, and name paths.
- `src/cli.moss`: `pos + 1` needs a literal-free spelling ([D4]);
  missing `bind lexer::Char=Char;` ([D27]); `Graph`, `print_bytes`,
  `node.lower()`, `graph.codegen()` are undeclared sketch holes; `Std`'s
  member list in `src/std.moss` names undeclared `File` and `println`.
- `src/parse.moss`: `ScopeId` undeclared; `tree()`'s match is
  non-exhaustive ([D34]); `names.push(next())` pushes a `TokenId` where
  `T=NameId` (needs a conversion or a rethink of `NameId`).
- `src/option.moss`: `type Option | None | Some;` — [D13].
- `src/lower.moss`: missing `=>` on the `Sig`/`Fn` arms (lines 103, 106);
  `ImportId`, `toplevel`, the `imports` iterable, and `ScopeId` undeclared;
  `for Import imp in imports` uses the typed-binder form ([D35]).
- `lib/` (all of it): previous-iteration design (`for X=Y {}` grouping,
  `This`/`this`, detached `.to_string`, `Numerals`/`Arithmetic` operator
  machinery) — to be rewritten from scratch against §§4–9 once decisions
  land; nothing in it should be consulted as precedent.

## 13. Decision index

Needs sign-off (**PROPOSED**): D5 strings-only-for-imports · D7 symbol
tokens · D9 export rules · D10 module identity · D12 declaration placement ·
D14 drop declaration-site Needs · D15 type forms · D17 injection-only
subtyping · D18 applicative type identity · D20(second half)
assume well-formedness · D22 partial application · D23 subsumption ·
D24 assume-with-binding · D25 bind scope · D26 bind forms · D27 no inference
at binds · D28 satisfaction rule · D31 statements · D32 expressions ·
D34 match · D36 minimal methods · D38 native-Std bootstrap · D39 linking ·
D40 test strategy

Genuinely undecided (**OPEN**): D13 `type Option | ...` nominal-union
shorthand · D16 unions over abstract types · D29 bind-returning functions ·
D30 recursion × static binds · D33 operator semantics · D35 iteration
protocol · D36/§9 the real method story · import cycles (D10) · braceless
`assume` statement form (§12)

The highest-leverage discussions, in order: §7 as a whole (D25–D28 decide
whether lowering is trivial or not), §9 methods, D33 operators, D35
iteration — the last three because `src/` cannot be rewritten honestly until
they're settled.
