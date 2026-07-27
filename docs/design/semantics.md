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

**Revision 2** incorporates the designer's feedback in [`notes.md`](notes.md):
points confirmed there are re-tagged DECIDED; D22, D24, D29, D31, D33, and
D35 are revised or replaced; §9 is rewritten around the attached/detached
method model; and the things revision 2 most needs answered are the numbered
questions **Q1**–**Q7** in §9 plus [D41]/[D42].

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
this type unit use val var while
```

Changes from `docs/reference/syntax.md`: `static` is dropped; `break`, `for`,
`loop`, `match`, `return`, `unit` are added (all six are used in `src/`), as
is `this` (§9 — the receiver in method bodies is now a keyword, replacing the
old `lib/this.moss` symbol). `for` is reserved but has no MVP grammar
production ([D31]), and `unit` may yet be dropped in favor of a `type` form
([D41]). `src/token.moss` and `src/lex.moss` must gain tokens for the added
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

**[D7] DECIDED (symbol tokens).** Exactly the one- and two-character symbol
tokens currently listed in syntax.md and `src/token.moss`:
`! % & ( ) * + , - . / : ; < = > [ ] ^ { | }` and
`!= :: << <= == >= >>`. With operators out of the MVP ([D33]), all the
operator tokens are lexed but reserved — no grammar production uses them
yet.

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

**[D9] DECIDED (exports).** Every top-level declaration of a module is
exported; there is no visibility control yet. `use *` imports all of them.
Names a module itself imported are *not* re-exported by `use *` (no transitive
glob), but can be re-exported deliberately the way `src/prelude.moss` does —
a file consisting only of imports, whose own importers then `use *` it.
(That pattern only works if plain `use` — as opposed to `use *` — *does*
re-export; so the rule is: explicit `use` names become part of the module's
exports, glob imports do not.)

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

**[D41] OPEN (the `unit` keyword).** The designer dislikes that `unit X;`
breaks the pattern `type` and `fn` otherwise follow, and asked for
alternatives. The least invasive one: the tag form with a unit payload,
`type X ();`, plus one special rule — a tag whose payload is `()` is
constructed and matched by its bare name (`X`, not `X ()`), which is exactly
the behavior `unit` provides. That trades the extra keyword for a special
rule; the grammar shrinks and `unit` stops being reserved, at the cost of
`type X ();` reading a little oddly. (`type X {};` — empty record payload —
is worse: `{}` already means the empty record type, and the bare-name rule
would be a lie for it.) No urgency; decide before the grammar freezes.

**[D14] DECIDED (drop declaration-site `Needs`).** syntax.md attaches an
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
recursively. A context is a compile-time artifact only; there are no
first-class context values at runtime (what exists at runtime is the val data
of [D2]).

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

**[D42] PROPOSED (tag construction sites must apply explicitly).** The one
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
cut, and the manual-prologue pattern covers the current corpus.

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

## 9. Methods: attached and detached — OPEN

Rewritten per notes.md. There are two kinds of methods, and their coexistence
is the reason symbol resolution needs types (§1). The designer's example,
reproduced because nothing in the codebase demonstrates the new model yet:

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

Questions for the designer — the bootstrap needs these before implementing
§9, and [D36] below records the working assumptions it will use in the
meantime:

- **Q1**: Can a detached method's signature refer to its receiver's type?
  The example's `.gimme` doesn't need to (its return is the separately
  assumed `Foo`), but something like `.clone` can't be expressed without a
  name for "the receiver's type". The old design had the `This` symbol
  (`for This=Uint32 { .to_string }`); notes.md says the new story is
  "significantly different". Is there an implicit receiver-type symbol per
  detached method, or is the answer simply "no — use an assumed symbol like
  `Foo` when you need to talk about related types"?
- **Q2**: Is `this` now a keyword, legal exactly in method bodies (with the
  receiver's declared type in an attached body)? The example suggests yes;
  `lib/this.moss` dies. Confirm.
- **Q3**: Can detached methods be *defined* (`fn .m() { ... }`), or only
  declared abstract? If defined, what is `this`'s type in the body, and what
  may the body do with it (presumably nothing beyond passing it around,
  absent Q1)?
- **Q4**: What provides a detached method at bind time? `Ctx` *asserts*
  `A.gimme[Foo=B]` as a requirement; eventually someone must satisfy it.
  Guess: `bind A.gimme=f;` where `f` is any in-scope function of matching
  signature (an attached `fn A.gimme` included) after substitutions, per
  the ordinary [D26]/[D27] rules. Confirm the syntax and whether an attached
  method can serve as the provider.
- **Q5**: Attached-on-abstract vs detached. `src/lower.moss` declares
  `fn TokenId.name(): StrId;` — attached, but to an *abstract* type. When
  `TokenId` is applied or bound (`bind parser::TokenId=Int;`), does
  `TokenId.name` become a method available at receiver `Int`, i.e. does the
  (receiver, name) key get rewritten by substitution? The same question
  makes `src/parse.moss` work: `names.push(...)` with `names: NameList`
  resolves because `IsList[T=NameId, List=NameList]` re-keys `List.push` to
  receiver `NameList`. If yes, is an attached-on-abstract method
  semantically just a detached method that happens to be declared at one
  symbol, or is there a real difference (e.g. in what may collide)?
- **Q6**: Where are collisions rejected? Two provisions of the same
  (receiver, name) key — say `A.gimme[Foo=B]` and a second `A.gimme[Foo=C]`
  — could be an error when the context is formed, when it is assumed, or
  only at a call site that actually looks up the key. Call-site-only is most
  permissive and cheapest; context-formation is the earliest diagnostic.
- **Q7**: Fields vs methods: rev 1 proposed that record fields and method
  names at the same receiver type must not collide (`x.f` vs `x.f()` being
  the only distinguisher otherwise). Confirm.

**[D36] PROPOSED (interim method semantics for the bootstrap).** Until the
Q's are answered, the bootstrap implements the resolution rule above with
these working assumptions: detached signatures cannot name the receiver's
type (Q1: no); `this` is a keyword valid only in method bodies (Q2: yes);
detached methods are abstract-only (Q3: sig-only); provision is
`bind X.m=f;` with ordinary signature matching (Q4); attached-on-abstract
methods re-key under substitution exactly like detached provisions (Q5:
yes, no semantic difference observable to callers); collisions error at the
call site only (Q6); fields and methods at the same receiver must not
collide (Q7).

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
  unused — still **OPEN** (unaddressed in notes.md) whether to keep a
  braceless rest-of-file `assume Std;` form as sugar (hello.md's "typical
  pattern" would benefit).
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
- `lib/` (almost all of it): previous-iteration design (`for X=Y {}`
  grouping, `This`/`this`, old-style detached `.to_string`,
  `Numerals`/`Arithmetic` context machinery) — to be rewritten from scratch
  against §§4–9 once decisions land. The one exception, per notes.md, is
  `lib/ops.moss`: it already matches the intended operator semantics and is
  the designated desugaring target for post-MVP operators ([D33]).

## 13. Decision index

Needs sign-off (**PROPOSED**): D15 type forms (union rule now decided via
[D16]; tuples/records still unconfirmed) · D36 interim method semantics
(the Q1–Q7 working assumptions) · D38 native-Std bootstrap sequencing ·
D42 explicit application at tag construction

Genuinely undecided (**OPEN**): §9 Q1–Q7 the real method story ·
D29 functors (does the static-only split dissolve the val wrinkle?) ·
D41 `unit` keyword vs `type X ();` · braceless `assume` statement form
(§12, unaddressed in notes.md)

Resolved by notes.md (now **DECIDED**): D5 · D7 · D9 · D10 (+ import cycles
forbidden) · D12 · D13 (option.moss → alias) · D14 · D16 (no
post-monomorphization checks — global principle) · D17 · D18 · D20 · D22
(no partial application: total or absent) · D23 · D24 (dropped) · D25 · D26
· D27 · D28 · D30 (depth backstop = sole D16 exception) · D31 (no `for`) ·
D32 · D33 (no operators in MVP; ops.moss is the future desugaring target) ·
D34 · D35 (iteration deferred) · D39 · D40

The highest-leverage remaining discussions, in order: §9 Q1–Q7 (methods
block any honest rewrite of `src/` and the [D33] operator follow-up), D29
functors, then the small syntax calls D41/D42. Everything else is settled
enough to start building against.
