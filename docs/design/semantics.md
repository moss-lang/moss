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

**Revision 2** incorporated the designer's feedback in [`notes.md`](notes.md):
points confirmed there were re-tagged DECIDED; D22, D24, D29, D31, D33, and
D35 were revised or replaced; §9 was rewritten around the attached/detached
method model with questions **Q1**–**Q7**.

**Revision 3** incorporates the answers to Q1–Q7 (notes.md §9): methods are
now settled ([D36]), `This`/`this` are keywords, attached methods are
restricted to nominal receivers, context formation gains *consistent
merging* (new [D43]), `unit` stays ([D41] closed), and [D29] records that
functors will start static-only with the `val` question still live.

**Revision 4** closes the remaining sign-offs (D15, D38, D42, and D36's
leftovers) and adds [D44]: import collisions are errors, `use ... as`
renames (detached names included), and `x.b::m()` calls a module-qualified
detached method because `::` binds more tightly than `.`. The log is now
settled enough to build against; subsequent work happens in
`docs/reference/` and the bootstrap compiler.

**Revision 7** adds [D52], the designer's ruling that `Wasi` — not `Std` —
is the primitive context, [D53], which separates the two things "context"
had been naming, and corrects [D48]: the self-hosted compiler
does not need a string literal to find its prelude, it needs a way to
reach `argv`.

**Revision 5 onward** is incremental: decision points appended as
implementation forces them, PROPOSED until the designer reacts. So far:
[D45] (concrete `Bool`), [D46] (module aliases export), [D47] (a finding:
one-binding-per-key means a multi-type `Std` must use receiver-keyed
methods, with a consequence for how [D33] operators should desugar), the
[D31] amendment (optional `let`/`var` type annotations), [D48] and [D49]
(both since revised in place after designer review: keywords need no
string literals — a character trie suffices — and there is no automatic
tail-call elimination; loops are the idiom).

## 1. Design thesis

Moss separates **scope** from **context**:

- *Scope* is knowing what a name refers to. It is resolved lexically, from
  imports and enclosing declarations — with one deliberate exception: at a
  method call `x.m(...)`, the *type* of `x` is part of the lookup key, so
  symbol resolution interleaves with type inference (§9). Inference is
  strictly forward — receiver types are always already known when a method
  call is resolved; there is no global constraint solving.
- *Context* is having the thing a name refers to. Top-level declarations
  without a definition (`type T;`, `val v: T;`, `fn f(): T;`) introduce
  *abstract symbols*: names that are in scope but that nobody has yet provided.
  `assume` declares that a region of code requires certain symbols to be
  provided; `bind` provides them.

**[D1] DECIDED (thesis).** Context requirements are sets of *symbols*, and a
requirement is satisfied only by naming that exact symbol — either by an
enclosing `assume` of the same symbol, or by an explicit `bind` of the same
symbol. There is no structural matching, no search by type shape, and no
specificity ranking. Method dispatch obeys the same discipline: its lookup
key is the pair (receiver type, method name), and it demands exactly one hit
— type-*directed*, never type-*searched* (§9). This is the deliberate
retreat from the second iteration (PR #14), whose lowering phase
(`Lower::resolve_need` / `synthesize` / `unique_option`) had to *search*
in-scope providers by shape and rank them.
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
This this type unit use val var while
```

Changes from `docs/reference/syntax.md`: `static` is dropped; `break`, `for`,
`loop`, `match`, `return`, `unit` are added (all six are used in `src/`), as
are `this` and `This` (§9 — the receiver value in method bodies and the
receiver *type* in method signatures are keywords, replacing the old
`lib/this.moss` symbols). `for` is reserved but has no MVP grammar
production ([D31]). `src/token.moss` and `src/lex.moss` must gain tokens for
the added keywords.

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

**[D5] DECIDED (strings only in import position).** The _string_ token
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

**[D7] DECIDED (symbol tokens).** The one- and two-character symbol tokens
listed in syntax.md: `! % & ( ) * + , - . / : ; < = > [ ] ^ { | }` and
`!= :: => << <= == >= >>` (`=>` is new relative to `src/token.moss` — the
old grammar had no `match`, so arms never needed an arrow). With operators
out of the MVP ([D33]), the operator tokens are lexed but reserved — no
grammar production uses them yet.

## 3. Modules, imports, and scope

**[D8] DECIDED.** A module is a source file. `import` is the only inter-file
mechanism:

```moss
import "./lex.moss" as lexer;          # qualified access: lexer::lex
import "./token.moss" use Eof, Name;   # unqualified names into file scope
import "./prelude.moss" use *;         # glob (used by src/main.moss)
import "./wasip1.moss" as wasi use Wasi;  # both at once
import "./ops.moss" use .m as .m1;     # rename, detached names included ([D44])
```

`as` binds a module alias usable with `::`; `use` copies specific names (or
all exported names, with `*`) into the importing file's scope. Importing is a
pure scoping construct with no context effect: importing an abstract symbol
does not provide it. The glob form must be added to the grammar (syntax.md's
**Import** lacks `*`).

**[D9] DECIDED (exports).** Every top-level declaration of a module is
exported; there is no visibility control yet. `use *` imports all of them.
Names a module itself imported are *not* re-exported by `use *` (no transitive
glob), but can be re-exported deliberately the way `src/prelude.moss` does —
a file consisting only of imports, whose own importers then `use *` it.
(That pattern only works if plain `use` — as opposed to `use *` — *does*
re-export; so the rule is: explicit `use` names become part of the module's
exports, glob imports do not.)

**[D46] DECIDED (module aliases are exports).** hello.md's story requires
the prelude to provide `char` as a *module* (`char::H`); the prelude spells
that `import "./char.moss" as char;`. So an `as` alias is an export of the
declaring module, like an explicit `use` name and unlike a glob ([D9]) —
aliases are deliberate, named acts. Consequence: `use x` where the exporter's
`x` is an alias imports the alias.

**[D44] DECIDED (import collisions and qualified method calls).** From the
designer, closing [D36]'s leftover (b). Imports may not place two
*different* symbols under one local name:

```moss
import "./a.moss" use .m;
import "./b.moss" use .m;   # error: two symbols, one local name
```

This applies to detached method names and ordinary names alike. The escape
hatches are module aliases (`import "./b.moss" as b;`) and renaming in the
use list (`use .m as .m1` — the dot is part of a detached name's spelling
and the rename keeps it). A detached method reached through a module alias
is called with a qualified name: `x.b::m()`, which parses as
`x . (b::m) ()` because `::` binds more tightly than `.`. So the shadowing
case [D36] worried about cannot arise, and no disambiguation rule is
needed.

**[D10] DECIDED (module identity & instantiation).** A module is not a unit
of instantiation and has no state; it is a bag of declarations, elaborated
once. All parameterization happens per-symbol via assume/bind, not per-module.
Two importers of `cell.moss` see the same symbols `Cell`, `Cell.read`, etc.;
the shared abstract `T` in `inner.moss` is what lets `IsCell[T=Int, ...]` from
one file and `bind cell=...` from another agree. Import cycles are forbidden,
at least for now.

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
| `fn T.m(x: Ty): Ty;` / `{...}` | attached method on receiver type `T` (§9) |
| `fn .m(x: Ty): Ty;` / `{...}` | detached method, receiver type supplied at use (§9) |
| `context C = item, ...;` | named context (§6) |
| `assume items { decls }` | requirement block wrapping declarations |

**[D11] DECIDED.** Omitting a return type means the unit type `()`
(hello.md). A `fn` with a body whose requirements aren't satisfied *defines*
a function that *requires* them; there is nothing wrong with a defined
function deep inside `assume` blocks.

**[D12] DECIDED (which declarations may appear where).** All of the above
are declarations and may appear at top level or inside `assume` blocks,
arbitrarily nested. Function *bodies* contain only statements/expressions —
no nested declarations except through `bind` (no local `fn`/`type`). This
matches all of `src/`.

**[D13] DECIDED (`option.moss` becomes an alias).** `src/option.moss`
declares `type Option | None | Some;` — no `=`, so by the table above this is
a *nominal* type whose payload is the union `None | Some`, whereas
`src/parse.moss` uses `Some (...)` / `None` directly where an
`Option[T=TokenId]` is expected. The designer confirmed the inconsistency;
the fix is the alias form, `type Option = | None | Some;`. Nominal-over-union
(the tag form with a union payload) remains expressible, but values must be
explicitly wrapped and unwrapped through the nominal head — there is no
auto-injection ([D17]).

**[D41] DECIDED (keep `unit`).** The designer dislikes that `unit X;` breaks
the pattern `type` and `fn` otherwise follow, but the alternative floated in
rev 2 — `type X ();` plus a bare-name construction/matching rule — was
considered and not loved. `unit` stays.

**[D14] DECIDED (drop declaration-site `Needs`).** syntax.md attaches an
optional `[Need, ...]` clause to every declaration form (**Needs**) and a
`static` marker on needs. Nothing in `src/` uses either; requirements are
expressed exclusively by `assume` blocks, and staticness is determined by
symbol kind ([D2]). Drop both from the grammar. (Square-bracket *application*
at use sites, like `Range[T=NameId]`, absolutely stays — see §6.)

## 5. Types

**[D15] DECIDED.** Type expressions:

- A path to a type symbol, alias, or nominal type: `Token`, `parse::TypeId`,
  possibly applied: `Option[T=TokenId]`, `Range[T=NameId]`.
- Unit `()` and tuples `(A, B)` (grammar only; unused in `src/` so far —
  keep tuples in the grammar, defer implementing them).
- Structural records `{ from: TokenId, name: Option[T=TokenId] }`. Records
  are structural; nominality comes only from tag declarations wrapping them.
- Unions `A | B | C`, including the leading-pipe multiline form. Members
  must be *nominal* types (units, tags) with distinct heads — the union is
  discriminated by nominal identity, and matching tests that identity. An
  abstract type symbol may appear as a member *only if* the context the
  union is written in already constrains that symbol to a specific nominal
  type; a bare abstract member like `src/lex.moss`'s `Char | Eof` is illegal
  and gets a nominal wrapper instead (§12). This keeps head-distinctness
  checkable where the union is written, per [D16].
- The empty union `|`: the uninhabited/divergence type. `fn err(...): |;`
  declares a function that cannot return; `match e {}` on an expression of
  type `|` is the eliminator and has any type. `type Type = |;` in
  `src/parse.moss` is a placeholder alias.

**[D16] DECIDED (no post-monomorphization checks).** All semantic checking
happens before monomorphization; a program that elaborates successfully must
never fail later during specialization. This is a global principle, not just
a union rule — rev 1's proposal of a post-monomorphization distinctness
check for unions is off the table, which is exactly why union members must
already be nominal where the union is written ([D15]). The sole sanctioned
exception is the instantiation-depth backstop of [D30].

**[D17] DECIDED (subtyping is injection-only).** A value of a union member
type implicitly injects into any union containing that member (this is how
`lex()` returns `Eof` where `Token` is expected). There is no other implicit
conversion, no width/depth record subtyping, and no union-to-union coercion
beyond re-injection of each member (defer even that). Nominal tags do not
inject into anything implicitly; `Some x` constructs a `Some`, which then
injects into `Option`'s union because it is a member.

**[D18] DECIDED (type identity / applicativity).** Type identity is
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
recursively, with duplicate keys merged consistently ([D43]). A context is a
compile-time artifact only; there are no first-class context values at
runtime (what exists at runtime is the val data of [D2]).

**[D53] DECIDED by the designer (module vocabulary).** This branch took its
framing from ML modules, and "context" has been doing two jobs since. They
are different things and conflating them causes real errors, so:

- A `context` **declaration is a signature** — a module type, the finite
  list of items some code requires. `assume Ctx` demands a structure of
  that signature. [D19]'s "compile-time artifact only" is this sense.
- **The context at a program point is a structure** — the provisions
  actually in force there, from enclosing `assume`s and `bind`s. Its only
  runtime residue is [D2]'s val data.
- `IsCell[T=Int, Cell=CellInt]` **refines a signature** by fixing its type
  components, as ML's `where type` does. It yields another signature; it
  instantiates nothing and produces no provisions, so it is *not* functor
  application.
- A **functor** maps a structure to a structure. In Moss it is a module
  with abstract symbols, and `bind` applies it. `src/cli.moss` is a functor
  application written out one component at a time: it supplies `lexer::src`,
  the tree and interner arenas, and gets back a structure it can call
  `parser::run()` in.

The consequence for [D29]: the `Wasi`-to-`Std` bridge is a functor in the
strict sense, not a new kind of construct, and what it wants is the ability
to apply a whole signature at once rather than one component per line. It
also settles [D29]'s open half against the static-only reframing — `Std` is
mostly vals (`zero`, `one`, `pwd`, ~90 char constants), so a functor that
carried only types and fns would leave most of the signature to a
hand-written prologue regardless.

**[D43] DECIDED (consistent merging).** From notes.md's answer to Q6. A
context carries at most one binding per key (one `A.gimme`, one `T`, ...),
but forming a context that mentions the same key with two different bindings
is not automatically an error — the bindings *merge*:

```moss
context Ctx1 = A, B, A.gimme[Foo=B];
context Ctx2 = A, C, A.gimme[Foo=C];
context Ctx3 = Ctx1, Ctx2;
```

`Ctx3` is satisfiable; it just additionally requires `B` and `C` to be the
same type, because merging the two `A.gimme` bindings unifies their `Foo`
targets. The representation is symmetric — neither `B` nor `C` becomes
"primary". A context is (1) a set of *atoms* and (2) a binding structure (a
DAG) over those atoms, with symbols mapping to atoms, possibly many-to-one:
`Ctx1`, `Ctx2`, and `Ctx3` each have three atoms and differ only in which
symbols name which atoms. Merging is deterministic congruence — union the
atom graphs, unify per key — not search, so it stays inside [D1]. A merge
that would identify two *distinct concrete* nominal types (say
`A.gimme[Foo=Eof]` with `A.gimme[Foo=Comma]`) is an error at context
formation, which also answers Q6's "where do collisions error" — at
formation, and only when genuinely unsatisfiable. Assuming a merged context
makes the merged symbols interchangeable in that region: binding one binds
them all. Related literature on the merging idea: ["Making a Type
Difference: Subtraction on Intersection Types as Generalized Record
Operations"](https://doi.org/10.1145/3571224) and the work it cites.

*Implementation note (rev 6):* the bootstrap now implements this. The
union-find lives in the environment's type map (an entry pointing anywhere
but the symbol itself is a substitution edge); `merge` unifies duplicate
keys instead of requiring equality; abstract type symbols are bare
unification variables; method keys canonicalize through the union-find;
and every call site records a (callee key → caller key) translation, which
subsumed the earlier "provide under both keys" workaround — the two
"same atom" findings that motivated all this are now just what
canonicalization does. Not yet the full symmetric atoms/DAG
representation: symbols still chase pointers to a representative rather
than mapping into an atom set, which is invisible semantically but worth
revisiting when contexts get reified for tooling.

**[D20] DECIDED (assume).** `assume xs { decls }` adds the items `xs` to the
requirement set of every declaration inside. Requirement sets nest by union:
`src/lex.moss`'s `lex` sits inside `assume Char { ... assume next_byte { }}`
and so requires `{Char, next_byte}`. Assuming a symbol also makes everything
it *mentions* usable: assuming `next_byte` (whose signature returns
`Char | Eof`) is only well-formed in a region that also assumes `Char`, which
is why the blocks nest in that order. **DECIDED** (confirmed): an `assume x`
is legal only where every abstract symbol appearing in `x`'s declared
signature/definition is already assumed or bound.

**[D21] DECIDED (implicit parameterization = generics).** A declaration's
requirement set is its parameter list. `Range` is declared inside `assume T`,
so `Range` is implicitly parameterized by `T`; `Range[T=NameId]` applies it.
This is the entire generics mechanism — there are no separate type
parameters. A reference is either fully applied or not applied at all
([D22]); an *unapplied* reference to `Range` inside another `assume T`
region refers to the same `T` symbol and thus stays coherent (the
`inner.moss` shared-`T` idiom).

**[D22] DECIDED (no partial application: total or absent).** Rev 1 proposed
partial application and claimed `src/` used it "all over"; the designer
challenged that claim, and re-inventory proves the challenge right. There
are exactly four bracket applications in all of `src/` —
`Option[T=TokenId]` (parse.moss:28), `Range[T=NameId]` (parse.moss:29),
`IsList[T=NameId, List=NameList]` (parse.moss:126), and
`IsCell[T=Int, Cell=CellInt]` (std.moss:47) — and every one binds *all* of
its target's requirements. What rev 1 mislabeled as partial application was
*unapplied* references (`next`, `now`, `peek` listed in `context Parsing`;
`Range` in `fn List.done(): Range;`), which perform no substitution at all:
their requirements flow outward as the *same symbols*, already assumed in
the surrounding region. So the rule is: a bracket application must be
total, and a bare reference substitutes nothing. There is nothing in
between, and elaboration never has to adapt a partially-instantiated
provider to a differently-shaped need — the spring-2026 tarpit.

**[D42] DECIDED (tag construction sites must apply explicitly).** The one
borderline case in the corpus: `Some (expect(Name))` (parse.moss:147)
constructs `Some` with no brackets in a region where `Some`'s requirement
`T` is neither assumed nor bound — it could only come from *inferring*
`T=TokenId` from the argument's type. That is deriving a binding rather
than using a known one, so for the MVP it is illegal: write
`Some[T=TokenId] (expect(Name))`. Forward construction-site inference can
be revisited if the boilerplate hurts; if it ever lands it stays confined
to tag construction, where the argument's type is already known ([D1]).

**[D23] DECIDED (subsumption).** A function may be called wherever its
requirement set is a subset of what the caller has (assumed or bound) — the
"main may assume any subset of Std" rule from hello.md, generalized: extra
available context is simply dropped. Two contexts are compatible by flattened
set inclusion; there is no nominal identity to contexts themselves.

**[D24] DECIDED (assume-with-binding dropped).** The grammar allowed
`assume` items to be **Binding**s, i.e. `assume Foo[T=Int] { ... }`. Ruled
out by the designer: it demands too much cleverness in inference. `assume`
items are bare symbol references only. Applied items live in `context`
declarations ([D19]), where applications are total ([D22]); a region that
wants an applied requirement names it in a context and assumes that.

## 7. `bind`

The novel construct. D25–D28 were confirmed in notes.md.

**[D25] DECIDED (form and scope).** `bind x=e;` is a *statement* (drop
**Bind** from the **Expr** production in syntax.md). Its effect is lexical
and extends from the statement to the end of the enclosing block. A later
`bind` of the same symbol in the same or an inner block shadows. Binds do not
escape the block upward or survive into the next iteration of a loop.

**[D26] DECIDED (what may be bound, and to what).**

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

**[D27] DECIDED (no inference at binds).** A bind never infers other binds.
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

**[D28] DECIDED (satisfaction rule, restated).** An expression may use an
abstract symbol `s` (call it, read it, mention the type) iff `s` is in the
enclosing declaration's requirement set or `s` is bound in an enclosing
block. A call to a defined function `g` requires each element of `g`'s
requirement set to be available the same way — matched by symbol identity,
with square-bracket applications composed. This check is the core of the
lowering phase, and per [D1] it is a set-membership test, not a search.

**[D29] OPEN (functors).** syntax.md's unused `fn f(): bind Needs` return
form and `bind f();` call form were the spring design's answer to a real
need: mapping an instance of one context shape to an instance of another.
The designer's new framing: under the ML-modules reading that operation is a
*functor*, and functors deserve to be their own construct, distinct from
functions even syntactically — the fn-returning-`bind` conflation was one of
the least elegant parts of the spring design. The unresolved wrinkle is
vals: "take some stuff in my current context plus a couple of runtime
values, and give back a context that uses them" is genuinely useful, and a
val-taking functor is what made fn-returning-`bind` attractive in the first
place.

One reframing worth discussing, riding the [D2] static/dynamic split:
restrict functors to *static* items — they may consume and produce only type
and fn bindings, applied entirely at elaboration time. Since the only
runtime content of a context is val data, the val-threading half of the use
case is then served by ordinary code: a defined function that evaluates its
arguments and `bind`s them before calling onward, i.e. exactly the manual
prologue `src/cli.moss`'s `parse` already writes
(`let pos = zero(); bind cell=pos; bind string=text; ...`). If that split
holds, functors never touch runtime data, and "functor over vals" stops
being a construct and becomes a code pattern. Whether that pattern is
ergonomic enough without sugar is the open half of the question.

For the MVP: no functors, no fn-returning-`bind`; both grammar forms are
cut, and the manual-prologue pattern covers the current corpus. The designer
has accepted the static-only split as a starting point but remains concerned
that `val` support will be needed eventually — so the val half of the
question stays open rather than dissolved, to be revisited with evidence
from rewriting `src/`.

**[D30] DECIDED (monomorphization depth backstop).** Because type/fn binds
drive specialization, a recursive function that re-binds a *type* on the
recursive path could demand infinitely many specializations.
(`src/lower.moss`'s `scope_items` recursion is fine — it rebinds only the
val `ctx`.) The bootstrap interpreter doesn't care (it just carries an
environment); the monomorphizing backend puts a depth limit on the
(declaration, static-bindings) instantiation graph and reports the cycle.
This is the sole exception to [D16]'s no-post-monomorphization-checks
principle, with precedent: Rust likewise accepts polymorphic-recursion-
shaped programs at check time and only fails with a recursion-limit error
when monomorphization actually runs.

## 8. Expressions, statements, and patterns

**[D31] DECIDED (statements).**

```
let x = e;      # immutable local
var x = e;      # reassignable local
x = e;          # reassignment of a var only
e;              # expression statement
bind ...;       # see §7
while e { ... }
loop { ... }    # with break
return e?;      # early return; `return` alone returns ()
break;          # loops only; carries no value
```

`for` is out of the MVP entirely — designing an iteration protocol is
deferred with it ([D35]). Both `while` and `loop` stay; each is trivial once
the other exists.

*Amendment (rev 5, DECIDED):* `let`/`var` take an optional type annotation
(`var x: Foo = ...;`). Forced by forward inference: without it, initializing
from a union member infers the narrow member type. (For booleans
specifically, [D45]'s prelude `true`/`false` already have type `Bool`, so
`var b = true;` needs no annotation.)

**[D50] DECIDED (defined vals), v0 restrictions PROPOSED.** From the
designer's [D45] refinement: `val name: Type = expr;` declares a *defined*
val — concrete like a defined function, e.g. the prelude's
`val true: Bool = Bool (True);`. A defined val is not a context item:
nothing provides it, listing it in a context is a no-op, and reading it
needs only scope. Bootstrap restrictions pending a real
global-initialization story: a defined val may not sit inside `assume`
blocks, its initializer elaborates in an empty environment (so it can need
nothing from any context), and it is inlined at each use — fine while
initializers are pure constructions, revisit before anybody writes an
effectful one.

`let`/`var` bind names, not patterns, for now. `var` permits reassignment of
the local slot only; it creates no aliasable storage — shared or captured
mutable state goes through `Cell` (which is why `src/cli.moss` threads a
`CellInt` for the lexer position instead of a captured `var`). Blocks are
expressions; the final expression without `;` is the block's value, `()`
otherwise.

**[D32] DECIDED (expressions).** Parenthesization, `()` unit, paths
(`lexer::lex`, `char::H`), calls `f(a, b)`, method calls `x.m(a)`, field
access `x.f` (which projects through a nominal-record tag: `imp.name` where
`imp: Import`), record construction `Import { from, name, names = ns }`
(shorthand when the local variable name equals the field name), tag
construction by juxtaposition `Some[T=TokenId] (expect(Name))` ([D42]), unit
values by name (`Eof`), `if`/`else if`/`else` as an expression, and `match`.
Unary and binary operator expressions are out of the MVP ([D33]).

**[D33] DECIDED (no operators in the MVP).** No unary or binary operator
expressions at all for now; the operator tokens stay lexed but reserved
([D7]). `src/` uses operators freely (`pos + 1`, `pos >= string.length()`,
`peek() == kind`, the whole `c == '!'` ladder), so those sites get rewritten
as ordinary calls (§12). The post-MVP direction is settled: operators
desugar to the *specific symbols of `lib/ops.moss`* — which, unlike the rest
of `lib/`, is already current with the intended semantics and survives —
with `Lhs`/`Rhs`/output symbols applied at the operand types. That is
Rust-lang-item style, not name-driven lookup of any method that happens to
be called `add`; availability rides the ordinary context rails for exactly
those `ops` symbols, and the desugaring is therefore not strictly syntactic
sugar. It may prove cheap enough to pull into the MVP once §9 settles.

**[D47] DECIDED (operators are methods; `lib/ops.moss` is superseded).**
The designer confirmed the finding below and withdrew the earlier
assertion that `lib/ops.moss` matched the intended semantics: arithmetic
and comparison are *detached methods* (`lib/num.moss`), provided per
receiver type, and [D33]'s operators will desugar to those method symbols
(`a + b` ≡ `a.add(b)` against `num.moss`'s `.add`, not a free function).
`lib/ops.moss` is deleted; its remaining operations (negation, bitwise,
shifts) moved into `lib/num.moss`. One wrinkle discovered while folding
them in: a method cannot attach to `Bool` because `Bool` is a transparent
alias of a union, and unions have no single nominal head to key on — so
there is no `.not` method; write the `if` out (or match). Methods-on-union
types are a real design question if that ever hurts. Original finding for
the record: Implementation hit a real tension between [D33] and [D43]: a
single context cannot provide `ops::eq` at both `Int` and `Char`, because
the two applications share the key `eq` and consistent merging would unify
`Int = Char` — exactly the collapse D43 is *supposed* to perform. The old
design escaped this with `for Number=Uint32 { ... }` grouping, which is
dead. The new design's own answer is already in §9: *receiver-keyed
methods*. `Int.eq` and `Char.eq` are distinct keys, so the bootstrap `Std`
provides arithmetic and comparison as detached methods (`lib/num.moss`:
`fn .add(rhs: This): This;`, `fn .eq(rhs: This): Bool;`, ...) referenced as
`Int.add`, `Char.eq`, etc. Consequence to weigh for [D33]: if operators
desugar to the *free functions* of `lib/ops.moss` (`eq(x, y)` with
`Lhs`/`Rhs`), a context can hold at most one operand-type instance at a
time, which makes `a == b` next to `c == d` at different types impossible
in one scope. Desugaring to the *methods* (`a.eq(b)` → key
`(type of a).eq`) has no such limit. `lib/ops.moss` is untouched pending
that call.

**[D48] FINDING, revised (what "no literals" actually costs).** Rev 5
claimed keyword recognition in the self-hosted lexer was "unwritable"
without string literals; the designer questioned that, and the claim was
wrong. Keywords need no strings at all: a hand-written character trie
(`take(char::a)` then `take(char::s)` then boundary-check → `As`, ...)
recognizes them, exactly like old-school lexers — `src/lex.moss` now does
this, at a cost of roughly two hundred lines for the 22 keywords. The real
finding is about *scale*, not possibility: every string the compiler must
know at compile time — error message text, Wasm section names, the
spelling of anything — must be encoded as character-by-character code.
Identifier handling doesn't need literals either (slices plus a native
`String.eq` suffice for interning and lookup). So [D4] can stand for a
while yet; the pressure to revisit it will come from diagnostics and
codegen, not from the lexer or parser. Decision deferred until it bites.

Rev 7 first claimed a concrete bite here and was wrong about it. The
self-hosted collect (`src/collect.moss`) loads a module graph from the
paths written in `import` declarations, which it slices out of the
importing file's source — no literal needed. It cannot yet see the
*prelude*, which nobody imports; rev 7 concluded that naming the prelude
needs a string the compiler holds itself. It does not. A path is data the
environment hands the program: `argv[0]` (the old Rust compiler's
`get_lib_dir` derived `../lib` from it) or an explicit path argument both
arrive as runtime Strings. The real obstacle is that Moss has no way to
*reach* argv beyond `first_arg`, which is [D52], not this. What still
argues for literals is codegen — a Wasm emitter must write
`"wasi_snapshot_preview1"`, `"fd_write"` and `"_start"` into its output —
and diagnostics. Both are further off. Still deferred.

**[D52] DECIDED by the designer (`Wasi` is the primitive context; `Std` is
a library over it).** [D38] made `Std` primitive: the bootstrap provides it
natively, and [D39] says `main` may assume a subset of it. The designer's
ruling generalizes that. What `main` actually receives is the whole `Wasi`
context; `Std` is an ordinary Moss library built on top of it, reached
through a language-provided convenience bridge. Typical programs go
through the bridge. **The compiler does not**: it starts from raw `Wasi`
and defines its own abstractions, so it is free to do whatever it needs —
read `argv[0]`, open files, manage its own memory — without asking `Std`
to grow a feature per need.

The declarations already exist and predate this branch: `lib/wasip1.moss`
covers the preview-1 surface and bundles it as `context Wasi`,
`lib/wasm.moss` declares `I32`/`I64` with the memory instructions. Nothing
*provides* either, which is the whole of the current gap: the interpreter's
native table answers only `Std` keys and `run_main` rejects anything else,
so `assume Wasi { fn main() ... }` fails at link.

Consequences to work through when it lands. In the Wasm backend providing
`Wasi` is nearly free — these *are* the imports and instructions, reached
by the same use-site machinery that already turns `Int.add` into `i32.add`
— but the import section has to stop being three hardcoded entries. In the
interpreter it is a real lift: emulated linear memory and a descriptor
table, which is the abstract-interpreter direction anyway. The backend's
existing shims *are* the bridge already, written in Python — `first_arg` is
`args_sizes_get`+`args_get`, `print` is `fd_write`, `String` is
`[len|bytes]` over a bump allocator — so writing them in Moss is what
retires them, and with them the `Std`-side natives rev 7 added
(`StrList`, `String.concat`, `String.slice`). [D39] restates as: `main`
may assume a subset of the *primitive* context.

**[D49] DECIDED (no automatic tail-call elimination).** Rev 5 proposed
guaranteeing proper tail calls after deep tail recursion in `src/lex.moss`
overflowed the interpreter's stack. The designer rejected it: `loop` and
`while` exist precisely so tail recursion need not be load-bearing, and
languages that require TCE for basic iteration are disliked on purpose.
Ruling: iteration is written with loops (`src/lex.moss` rewritten
accordingly); unbounded tail recursion is not a supported pattern; a
future *explicit* tail-call construct that maps to Wasm `return_call` is
welcome, but elimination is never implicit semantics. The bootstrap
interpreter happens to contain a tail-call trampoline — that is a
non-semantic implementation detail that code must not rely on.

**[D51] FINDING, corrected by the designer (generic containers work via
unique receivers).** Rev 6 claimed generic containers hit the [D47] wall —
that `IsList[T=Int]` and `IsList[T=Char]` in one scope would merge their
`List.push` keys. The designer pointed out the intended idiom already
avoids this: with *detached* methods and a **unique receiver type per
instantiation** (`src/parse.moss` always meant `NameList` to be its own
type), the keys are `(NameList, push)` vs `(TokenList, push)` — distinct
atoms, no merge. Verified by test: one scope holds
`IsCell[T=Int, Cell=CA]` and `IsCell[T=Char, Cell=CB]` simultaneously,
with the shared `T` rebound between the two groups of provision binds
(binds are lexical, and each provision's signature is captured at its
bind statement). What remains true from the original finding: an
instantiation costs one named type declaration (no anonymous
instantiations — arguably a feature); a context that lists a shared type
*symbol* as an item still single-instantiates per scope; and `Std` ships
concrete `IntList` as the native backing store that per-receiver list
wrappers delegate to.

**[D45] DECIDED (nominal `Bool` as a lang item; revised).** Confirmed by
the designer as the easiest way for `if` to work, then refined by them to
kill the [D47] wrinkle: `Bool` is *nominal over* the sum
(`type Bool | False | True;` — the tag-with-union-payload form), so methods
can attach to it (`Std` provides `Bool.not`). Construction is
`Bool (True)`, but nobody writes that: the prelude defines
`val true: Bool = Bool (True);` and `false` likewise ([D50]), which also
makes `var b = true;` infer `Bool` with no annotation. `if c { a } else
{ b }` requires `c : Bool` and eliminates it; matching *through* `Bool` to
`True`/`False` needs nested patterns, which the bootstrap doesn't support
yet — use `if`. The old
`lib/bool.moss` made even booleans contextual (`type Bool;` with abstract
`val true`/`val false`). The bootstrap diverges: `lib/bool.moss` now
declares concrete units and a transparent alias —

```moss
unit False;
unit True;

type Bool = | False | True;
```

— because the elaborator must *eliminate* a boolean at every `if`, and with
an abstract `Bool` the meaning of `if` itself would depend on context. With
units, `if c { a } else { b }` is exactly
`match c { True => { a } False => { b } }` and needs no new machinery.
`False`/`True` are lang items the compiler knows by (module, name), like
the future `ops` symbols ([D33]). Interaction to resolve when operators
land: `lib/ops.moss` says `assume Bool { ... }`, which is vacuous once
`Bool` is a concrete alias — those assumes would just drop. If the designer
wants contextual booleans back, `if` needs a story first.

**[D34] DECIDED (match).**

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

**[D35] DECIDED (iteration deferred with `for`).** No `for` and no
iteration protocol in the MVP ([D31]); designing iterator semantics is
punted deliberately. The four `for` loops in `src/` (§12) get rewritten with
`loop`/`while` over explicit state, and the typed-binder form
(`for Import imp in imports`, `src/lower.moss:45`) is cut along with the
rest.

## 9. Methods: attached and detached

Settled across notes.md's two rounds. There are two kinds of methods, and
their coexistence is the reason symbol resolution needs types (§1). The
designer's example, reproduced because nothing in the codebase demonstrates
the new model yet:

```moss
type Foo;

assume Foo {
  fn .gimme(): Foo;
}

type A;
type B;
type C;

assume C {
  fn C.gimme(): C {
    this
  }
}

context Ctx =
  A,
  B,
  C,

  A.gimme[Foo=B],
  B.gimme[Foo=C],
;

assume Ctx {
  fn example(a: A): C {
    let b = a.gimme();
    let c = b.gimme();
    c.gimme()
  }
}
```

- **Attached** methods, `fn T.m(args): R;` / `{...}`, name their receiver
  type at the declaration. A *defined* attached method is an ordinary
  defined function: `c.gimme()` above calls `fn C.gimme(): C { this }`
  directly, needing only that the method is in scope and its requirement
  set (`{C}`) is satisfied — `C.gimme` is not (and need not be) listed in
  `Ctx`.
- **Detached** methods, `fn .m(args): R;` / `{...}`, name no receiver type.
  The receiver is supplied where the method is *referenced*: the context
  item `A.gimme[Foo=B]` reads "the detached `.gimme`, at receiver `A`, with
  its requirement `Foo` bound to `B`". The same detached symbol may be
  provided at many receivers in one context (`A.gimme[Foo=B]` and
  `B.gimme[Foo=C]` coexist), so the receiver type is part of the item's
  identity — the [D1] "symbol" for a detached method is really the pair
  (receiver type, method symbol). Note the reference is total in the [D22]
  sense: the `A.` prefix plus the brackets bind everything.

Resolution of `x.m(a)`: forward-infer the type `X` of `x` (always already
known, §1), then look up the key (`X`, `m`) — a scope-visible attached
method `X.m`, or a detached `.m` provided at `X` by the requirement
set/binds in force. Exactly one must be available; absence and ambiguity are
both errors at the call site. Nothing is ranked or adapted.

The questions from revision 2, answered in notes.md:

- **Q1 — answered.** A detached method's signature refers to the receiver's
  type as `This`, now a keyword in type position ([D3]) rather than the old
  `lib/this.moss` symbol. So `fn .clone(): This;` is expressible.
- **Q2 — answered.** Likewise `this` is a keyword: the receiver value,
  legal in method bodies.
- **Q3 — answered (defined detached methods disallowed).** Detached methods
  are declaration-only; there is no evident point to defining one. The
  designer asked whether a definition would ever be useful — one real
  candidate: *default implementations*, e.g. a defined `.ne` in terms of an
  assumed `.eq`, or `.le`/`.gt`/`.ge` derived from `.lt`, which is exactly
  the shape `lib/ops.moss` will want post-MVP so that providing `lt` yields
  the rest for free. But the same thing is expressible today as a free
  defined function that a `bind X.ne=that_fn;` points at, so nothing is
  lost by disallowing it; revisit alongside operators ([D33]).
- **Q4 — answered (call-site signature interpretation).** Once the
  receiver's type and the method are resolved, the bracket bindings of the
  providing context item are used to interpret the method's *signature* at
  that call site. In the example, `Foo` is not in the ambient context at
  all; the item `A.gimme[Foo=B]` supplies `Foo=B` for reading `.gimme`'s
  signature once `a.gimme()` resolves to it. This is how the design threads
  ergonomics without search-y synthesis from the context: the bindings ride
  along with the provision instead of being discovered.
- **Q5 — answered (attached methods on nominal receivers only).** The
  attached-on-abstract declarations throughout `src/` are simply errors:
  `fn T.m(...)` requires `T` nominal. They all become *detached*
  declarations (`fn String.length(): Int;` → `fn .length(): Int;`), and the
  *context items* keep their current `String.length` spellings, since a
  provision may key a detached method on any type symbol, abstract included
  — the designer's own example keys `.gimme` on the abstract `A` and `B`.
  See §12 for the full sweep.
- **Q6 — answered (consistent merging).** See [D43]: a context has at most
  one binding per key; forming a context that would bind the same key twice
  merges the bindings' targets, and only a genuinely unsatisfiable merge
  (two distinct concrete nominal types) errors, at formation time.
- **Q7 — answered (collisions allowed).** Record fields and method names
  may collide; disambiguation is Rust-style — `x.f` is the field, `x.f()`
  is the method.

**[D36] DECIDED (method semantics).** The resolution rule above plus the
Q1–Q7 answers. The two former leftovers are closed: (a) `This`/`this` are
also legal in *attached* declarations, where `This` simply equals the named
receiver type; (b) two detached methods with the same local name cannot
coexist in scope at all — the colliding import is an error, and module
aliases or `use`-renames plus the qualified call form `x.b::m()` cover
every case ([D44]).

## 10. Execution model and entry point

**[D37] DECIDED.** `main` takes no parameters, returns `()`, and may assume
any subset of `Std` (hello.md). The only primitively provided contexts are
`Wasm` (instruction intrinsics, `lib/wasm.moss`) and `Wasi` (host imports,
`lib/wasip1.moss`); `Std` is meant to be *implemented in Moss* on top of them
and bound by a driver, which is the `# TODO: Bind Std` in `src/main.moss`.

**[D38] DECIDED (bootstrap shortcut).** The bootstrap compiler provides
`Std` natively at first (Python implementations of `String`, `Path`, `Cell`,
`print`, chars, ints), *and* provides `Wasm`/`Wasi` natively (Python ints and
a bytearray memory), so that `moss run` works before the in-Moss `Std`
implementation exists. Switching `Std` from native to in-language is then a
milestone that exercises §7 hard, on purpose. The set of native symbols is
kept in one Python table so the two layers can't drift apart silently.
(notes.md: the designer would sequence this differently but has OK'd it;
the in-language `Std` remains the bar for the MVP proper, so the native
table is scaffolding with a planned demolition date.)

**[D39] DECIDED (linking model for `moss run file.moss`).** The CLI
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

**[D40] DECIDED (test strategy).** Corpus-driven: every file in `src/` and
every doc example must lex and parse from day one (golden AST dumps);
elaboration and execution tests grow file-by-file starting from a rewritten
literal-free hello. `tests/errors/` continues as golden-diagnostics tests.
The old `examples/` and `lib/` are excluded until rewritten.

## 12. Errata in existing artifacts

Stale things this document supersedes; each needs a mechanical fix once the
decisions above are confirmed.

- `docs/reference/syntax.md`: keyword list ([D3]); no literals ([D4]); add
  `match`/`loop`/`break`/`return`/`unit` productions and union/never types
  ([D15], [D34]); add `use *` ([D8]); drop declaration-site Needs and
  `static` ([D14]); drop **Bind** from **Expr** ([D25]); cut the
  bind-returning forms ([D29]), all operator expression productions ([D33]),
  `for` ([D31]), and bracket bindings in `assume` items ([D24] — assume
  takes bare paths); grammar's `assume List[Binding];` statement form is
  unused — **DECIDED** (delegated to the implementer): no braceless form
  in the MVP; there is exactly one way to write requirements, and the
  question can be reopened when enough real code exists to feel the
  rightward drift.
- `docs/learn/hello.md`: the `println("Hello, world!")` example at line 73
  contradicts [D4]; `putchar`/`char::*` need to actually exist in the new
  `lib/`.
- `src/token.moss`, `src/lex.moss`: token set per [D3]/[D4]; keyword and
  name lexing missing entirely; the `c == '!'` ladder is doubly stale —
  char literals ([D4]) *and* the `==` operator ([D33]) — and becomes calls
  against named char constants; `next_byte(): Char | Eof` has an abstract
  union member ([D15]) and needs a nominal wrapper, e.g. `type Read Char;`
  and `Read | Eof`; `lex()` must gain the two-character-symbol,
  whitespace/comment, keyword, and name paths.
- `src/cli.moss`: `pos + 1` and `pos >= string.length()` use literals and
  operators ([D4], [D33]); missing `bind lexer::Char=Char;` ([D27]); the
  `for` loop at line 49 ([D35]); `Graph`, `print_bytes`, `node.lower()`,
  `graph.codegen()` are undeclared sketch holes; `Std`'s member list in
  `src/std.moss` names undeclared `File` and `println`.
- `src/parse.moss`: `ScopeId` undeclared; `tree()`'s match is
  non-exhaustive ([D34]); the `peek() == ...` comparisons ([D33]);
  `Some (expect(Name))` needs explicit application ([D42]);
  `names.push(next())` pushes a `TokenId` where `T=NameId` (needs a
  conversion or a rethink of `NameId`).
- `src/option.moss`: `type Option | None | Some;` becomes
  `type Option = | None | Some;` ([D13]).
- `src/lower.moss`: missing `=>` on the `Sig`/`Fn` arms (lines 103, 106);
  `ImportId`, `toplevel`, the `imports` iterable, and `ScopeId` undeclared;
  three `for` loops (lines 45, 50, 81) to rewrite ([D35]).
- Attached-on-abstract methods everywhere (Q5/[D36]): `src/std.moss`
  (`String.length`, `String.get`, `Path.join`, `Path.read`),
  `src/cell.moss` (`Cell.read`, `Cell.write`), `src/parse.moss`
  (`List.push`, `List.done`, `TokenSet.add`), `src/lower.moss`
  (`TokenId.name`, `ImportId.module`, `ModuleId.insert`, `ModuleId.get`,
  `Context.extend`, `Context.depend`) — every one becomes a detached
  `fn .m(...)` declaration; the context items and call sites keep their
  current spellings.
- `lib/` (almost all of it): previous-iteration design (`for X=Y {}`
  grouping, `This`/`this`, old-style detached `.to_string`,
  `Numerals`/`Arithmetic` context machinery) — to be rewritten from scratch
  against §§4–9 once decisions land. The one exception, per notes.md, is
  `lib/ops.moss`: it already matches the intended operator semantics and is
  the designated desugaring target for post-MVP operators ([D33]).

## 13. Decision index

Still awaiting the designer, all deferred rather than blocking: [D48]
string literals (when diagnostics/codegen make embedded strings
unavoidable — *not* the prelude path, see [D52]) · methods on union-headed *aliases* in general ([D47] — solved
for `Bool` by [D45]'s nominal wrapper) · D29 the `val` half of functors ·
[D50]'s v0 restrictions on defined vals

Everything else is **DECIDED**: D1–D28 · D30–D49 (D3/D4 by designer fiat,
D13 option.moss → alias, D16 no post-monomorphization checks, D22 total or
absent, D24 dropped, D30 depth backstop as sole D16 exception, D31/D35 no
`for`, D33 no operators with ops.moss as the future desugaring target, D36
methods via Q1–Q7, D41 keep `unit`, D43 consistent merging, D44 import
collisions + `::` tighter than `.`, D45 concrete `Bool`, D46 aliases
export, D47 operators are methods, D49 no automatic TCE — loops are the
idiom, D52 `Wasi` primitive with `Std` a library over it, D53 signature
vs structure)

The MVP language is fully pinned down. Next: rewrite
`docs/reference/syntax.md` against this log, then build the bootstrap
pipeline of §11 with the [D40] corpus tests, applying the §12 errata to
`src/` as the corpus comes online.
