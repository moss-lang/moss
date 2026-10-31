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

**Revision 8** rewrites the self-hosted compiler against this log rather
than patching it, which discharges the `src/` half of §12's errata and
prices the codegen half of [D48] exactly: a Wasm emitter needs no string
literals, only a generated function per name. What remains of [D48] is
diagnostics.

**Revision 9** implements [D61] in both compilers — one declaration per
detached accessor with the element type bracket-applied per provision —
and the compiler under `src/` compiles itself to a fixpoint. That closes
the last item this log had marked as outstanding work.

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
as assume bind break context else fn for functor if import let loop match
return This this type unit use val var while
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

Numeric literals have since arrived, as arithmetic over library symbols
rather than constants ([D68]); character and string literals have not.

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
grammar production uses them yet. ([D68] gave them their productions.)

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
- Unit `()`. There are no tuples: `(T)` is grouping, and records are the
  product type ([D63], which retired the deferred `(A, B)`).
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
- A **module** is neither. It is only a unit of code organization and
  symbol scoping — a namespace. It is not a structure, does not carry
  provisions, and is not itself a functor. (Rev 7 first claimed otherwise
  and the designer corrected it.)
- A **functor** maps a structure to a structure. **Moss has no functor
  construct today.** What resembles one — `src/cli.moss` binding
  `lexer::src` and then calling `lexer::lex()` — is not functor
  application but ordinary context propagation: each symbol's needs are
  satisfied by whatever happens to be in scope where it is used, with no
  module-level parameterization anywhere. Adding the construct is [D55].

The consequence for [D29]: since a functor would be a genuinely new
construct, the designer's note there ("functors deserve to be their own
construct, distinct from functions even syntactically") is the live
reading, and the `Wasi`-to-`Std` bridge is the use case that forces it.
It also settles [D29]'s open half against the static-only reframing —
`Std` is mostly vals (`zero`, `one`, `pwd`, ~90 char constants), so a
functor carrying only types and fns would leave most of the signature to
a hand-written prologue regardless.

**[D58] DECIDED by the designer (tagging belongs to the sum, not to the
nominal type).** The backend boxed every nominal tag: `type A Foo;` became
a heap cell `[code | payload]` wherever it appeared, which is what made
[D54]'s wrappers expensive. That is wrong. Tagging exists to discriminate
a sum, so a value of type `A` is represented exactly as a `Foo` — no tag
word, no box — and the tag is attached at the *injection* [D17] allows,
where a value of `A` enters an `A | B`. Crucially this follows the static
type at each point, not a property of the declaration: `A` stays bare in
its own uses even when `A` is a member of a union somewhere else in the
program. (Rev 7 first proposed the weaker whole-program rule, "untagged
if the type appears in no union at all", and the designer rejected it.)

Implemented: lowering emits an explicit `ir.Inject` wherever a value
widens into a union — every such path runs through `synth` with an
expected type, so there is one place to do it — and `ir.Match` records
whether its scrutinee has more than one possible head, since a match that
cannot discriminate anything is irrefutable. Construction of a tag is now
the identity. A unit needs no injection (its code already discriminates)
and a record's box already carries one, so only a payload-carrying tag
grows a word, and only where it enters a union.

The consequence for [D54]: its wrappers are free. `Num`, and the `String`
and `Path` wrappers a bridge needs, are in no union, so they are bare
i32s — an in-language `Std` no longer boxes what it hands out. Not yet
done: a union of payload-carrying members is still a heap cell, where two
Wasm locals would do; the value model is one i32 per value throughout, so
that is a separate change.

**[D60] FINDING (an attached method needs its receiver declared beside
it).** Collect links a module's imports *after* declaring its own names,
so `fn Bool.flip()` in a module that merely imports `Bool` is rejected —
"attached method receiver `Bool` is not in scope". The `Wasi` bridge hit
this providing `Bool.not`, whose provider must be a method on `Bool`
([D54]); the fix was to define `Bool.flip` in bool.moss, which is
arguably where it belonged. Settled the other way by [D61]: an attached
method *must* be declared in the same file as the nominal type it
attaches to, which is what lets a call find it without importing
anything. So the ordering is a rule rather than an implementation
detail, and a bridge that wants to attach to a type it does not own
cannot — it provides a *detached* method at that receiver instead, which
is exactly what `Bool.not` does.

**[D59] DECIDED by the designer (the compiler has no heap).** Every value
of a Moss program compiles to a *finite sequence of Wasm scalars* — i32
and i64 now, f32/f64 if they ever arrive (they have: [D67]) — and the compiler allocates
nothing. A heap is a library's business: lib/wasistd.moss already has a
bump allocator written in Moss, and that is where dynamic memory belongs.

Where the backend stands against that rule: it allocates in exactly two
places of its own, `MakeRecord` and `Inject`. Everything else that calls
its allocator is the native `Std` — strings, lists, cells, `pwd` — which
is being retired into Moss anyway and takes its allocations with it. So
the rule costs two constructs, but it needs one new thing under them:

**Layouts.** Each Moss type maps to a tuple of Wasm valtypes: `()` to
none, `Int`/`Char` to one i32, a record to the concatenation of its
fields', a tag to its payload's ([D58]), a union to one i32 of
discriminant followed by enough slots for its widest member. The IR must
carry the layout of every expression, since the backend can no longer
assume "one i32" — and that is the same mechanism [D57] needs, because a
value being i64 is just a different layout. So i64 support and the
heapless value model are one change, not two — and its first half is
done: a slot carries a valtype, `Wasm`'s i64 instructions compile, and
`lib/wasistd.moss` implements `Path.read` with real u64 rights masks.
Both halves are done. A slot carries a valtype, so `Wasm`'s i64
instructions compile and `lib/wasistd.moss` implements `Path.read` with
real u64 rights; and a value is a *run* of slots, so a record is one
scalar per field and an injected union is a discriminant beside its
payload. Neither allocates. What is left of the backend's allocator
serves only the native `Std` — strings, lists, cells — and goes when
those do.

Two things fell out worth keeping. A block type names at most one
result, so a branch or match arm yielding several scalars stages
through locals rather than needing multi-value block types. And with a
discriminant in slot zero, the old `>= HEAP_BASE` pointer test
disappears: units and tags discriminate the same way, by comparing one
scalar.

Since covered, in both compilers: a record field or tag payload of more
than one scalar, so records nest and sit inside unions of any width, and
binding a val to a multi-slot value. A type that contains itself would be
an endless run of scalars, so it is an error rather than a layout; an
arena of indices is the idiom for recursive data ([D51]).

**[D57] RESOLVED by [D59] (an in-language `Path.read` needs i64).** `path_open`
takes its two rights masks as u64, as lib/wasip1.moss correctly declares.
The Wasm backend's value model is uniformly i32, so a Moss implementation
of `Path.read` cannot construct those arguments — it is the one part of
`Std` that cannot be written in Moss today, and the backend keeps
providing it as a shim. Supporting i64 means typed locals and parameters
rather than "everything is an i32", so it is a real slice of work; a
narrower option is to allow i64 expressions only in argument position,
where the Wasm validator can check them without the value model changing.

**[D56] DECIDED (module identity is a canonical path).** A module was keyed
by the *spelling* of the path that reached it, so the same file arrived at
two ways — an absolute prelude and a relative import — became two modules
whose every name then collided with itself ("`Bool` would refer to two
different symbols"). Nothing hit it until lib/wasm.moss gained an import.
The loader now canonicalizes: relative paths resolve against a root (the
working directory, or one the caller supplies), `.` and `..` collapse, and
the result is the module's identity. The prelude is canonicalized the same
way, or it fails to recognize itself and imports itself.

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

*Calculus note (July 2026):* merging is now part of the core calculus
([docs/design/core](core/)): telescope formation merges duplicate keys by
unifying the colliding bindings, failing exactly when two distinct
concrete heads would be identified — and, a finding of the mechanization,
when the equations are cyclic (D43 is silent on cyclic merges; the core
adds the occurs check, so `t ≡ Full[Elem=t]` is a formation error). The
mechanization orients the unifier (earlier occurrence is the
representative) and verifies its output at formation; satisfaction goes
per equivalence class, so "binding one binds them all" is a theorem-level
property at every use site. One clarification the mechanization forces: a
method requirement's key is the receiver's *type* — the [D18] identity
[D62] repairs dispatch to use — not its head. Keying on the head would
give `Slot[Elem=Int].get` and `Slot[Elem=Char].get` one key, so a
telescope needing the method at two instantiations would try to identify
`Int` with `Char`; D43's own example is unaffected, since there the
receiver is shared and only the method's application differs.

**[D20] DECIDED (assume).** `assume xs { decls }` adds the items `xs` to the
requirement set of every declaration inside. Requirement sets nest by union:
`src/lex.moss`'s `lex` sits inside `assume Char { ... assume next_byte { }}`
and so requires `{Char, next_byte}`. Assuming a symbol also makes everything
it *mentions* usable: assuming `next_byte` (whose signature returns
`Char | Eof`) is only well-formed in a region that also assumes `Char`, which
is why the blocks nest in that order. **DECIDED** (confirmed): an `assume x`
is legal only where every abstract symbol appearing in `x`'s declared
signature/definition is already assumed or bound.
*Note (October 2026):* the bootstrap enforced this from the start, as a
side effect of elaborating the item ("type `T` is not available in the
context here"). The self-hosted compiler did not: `assume W.m` with
`.m(): T`, or `assume seed` with `seed: T`, and no `T` in force, compiled.
It now reports `e_absent` (`!P`, naming the type) at the assume.
Standing tests: `tests/wasi/assume_without_type*.moss`.

**[D21] DECIDED (implicit parameterization = generics).** A declaration's
requirement set is its parameter list. `Range` is declared inside `assume T`,
so `Range` is implicitly parameterized by `T`; `Range[T=NameId]` applies it.
This is the entire generics mechanism — there are no separate type
parameters. A reference is either fully applied or not applied at all
([D22]); an *unapplied* reference to `Range` inside another `assume T`
region refers to the same `T` symbol and thus stays coherent (the
`inner.moss` shared-`T` idiom).

*Narrowed for nominals by [D71].* For a tag, alias or unit the parameter
list is its *relevant* parameters — the type symbols its payload
mentions, closed through the nominals it mentions — not the whole
nesting; binding any other symbol in brackets is an error. Functions,
vals and contexts keep the whole nesting.

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

*Clarification (July 2026, from mechanizing the core).* "Total or absent"
was too strong as written, and the calculus is the looser rule the
designer intended: a reference must leave nothing **dangling**, but it
need not answer every requirement in brackets — what the enclosing
`assume`s already supply, they supply. Inside `assume U`, a reference
`f[T=Char]` to an `f` requiring `{T, U}` is fine; `U` passes through to
the ambient symbol. The two extremes above are just the common cases, not
the only legal ones. What stays banned is a partially instantiated
reference *travelling*: after elaboration every requirement of every
reference is pinned, either by the application or to an in-force symbol,
which is what keeps the tarpit closed. The core enforces exactly this
(rules S-Inst/S-Pass), and always did — the paper's prose claimed the
stronger rule its own figures did not impose.

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
escape the block upward or survive into the next iteration of a loop. For
a type, shadowing starts a new generation, and what was typed before the
bind keeps the old one ([D72]).

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
(It was: [D68], against `num.moss`'s methods per [D47].)

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
types are a real design question if that ever hurts. (Settled for detached
methods by [D75]: they are keyed by the canonical type, a union's
included; attached methods still need a nominal head.) Original finding for
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

Rev 8 wrote the emitter and can price the codegen half exactly: it cost
one generated function per name. `src/spell.moss` holds twenty-one of
them — the WASI module and field names, `_start`, `memory`, and the
handful of names the compiler must *recognize* rather than write — each
pushing characters against the `char` constants, exactly as the keyword
trie does. `src/wasmops.moss` does the same in bulk for the hundred-odd
`Wasm` intrinsic names, interned once at startup so that recognizing
`i32_add` is an id comparison. Both are generated and neither is
pleasant, but nothing was blocked, and a name the compiler already read
out of a file needs no spelling at all: a WASI import's field name is
copied straight out of the interner. So the codegen argument for
literals is weaker than rev 7 expected. The diagnostics argument is
unchanged and is now the whole of it — the self-hosted compiler reports
an error as a code letter and a name, because a sentence is a string it
would have to hold. Still deferred.

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
- **All the `src/` entries below are discharged.** Rev 8 rewrote the
  self-hosted compiler against this log: `src/lex.moss` and
  `src/token.moss` carry the [D3]/[D4] token set with a keyword trie;
  `src/syntax.moss` parses the whole grammar; `src/prog.moss` loads the
  module graph; `src/codegen.moss` writes Wasm. The files the entries
  named — `src/parse.moss`, `src/tree.moss`, `src/mods.moss`,
  `src/collect.moss`, `src/lower.moss`, `src/std.moss` — are gone,
  replaced rather than patched, and `src/cell.moss`/`src/option.moss`
  moved to `tests/fixtures/` where the tests that use them live. The
  attached-on-abstract methods of Q5/[D36] went with them. Kept here for
  the record of what the rewrite had to fix:
  the `c == '!'` ladder (char literals [D4] *and* the `==` operator
  [D33]); `next_byte(): Char | Eof`, an abstract union member ([D15])
  that needed the nominal `type Read Char;` wrapper; `pos + 1` and
  `pos >= string.length()` in the old CLI; the `for` loops of [D35];
  `Some (expect(Name))` needing explicit application ([D42]); and
  `type Option | None | Some;` becoming an alias ([D13]).
- `lib/` (almost all of it): previous-iteration design (`for X=Y {}`
  grouping, `This`/`this`, old-style detached `.to_string`,
  `Numerals`/`Arithmetic` context machinery) — to be rewritten from scratch
  against §§4–9 once decisions land. The one exception, per notes.md, is
  `lib/ops.moss`: it already matches the intended operator semantics and is
  the designated desugaring target for post-MVP operators ([D33]).

**[D54] DECIDED by the designer (keep the wrapper).** Verified behaviour:
`bind Int.add = p` checks `p` against the method's *declared* signature,
which excludes the receiver. So a plain function may provide a method but
cannot see what it was called on — `this` in it is "only legal inside
method bodies" — and widening it to take the receiver is a signature
mismatch. The only provider that can use its receiver is an attached
method, which requires a nominal type. A `Wasi` bridge providing `Int.add`
therefore writes:

```moss
type Num I32;
assume Wasm {
  fn Num.add(rhs: Num): Num {
    match this { Num a => match rhs { Num b => Num (w::i32_add(a, b)) } }
  }
}
# bind int::Int = Num;  bind Int.add = Num.add;
```

Rev 7's first slice of `Std`-over-`Wasi` shows the cost is larger than
"a wrapper per provider". It is a wrapper per *type*: any Std type with
methods must bind to a nominal one, because the provider has to be an
attached method and an abstract type cannot carry one. So `Int` is boxed,
and `String` and `Path` will be too as soon as `.length` or `.join` are
provided — `String` is still bare `I32` in `Console` only because `print`
and `first_arg` are plain functions. An in-language `Std` therefore boxes
every value it hands out, and unboxing single-payload tags over scalars
stops being a nice-to-have.

Rev 7 proposed letting a plain-function provider take the receiver as its
first parameter instead, which needs no wrapper and matches the convention
both backends already use internally. The designer declined for now: it is
one more syntactic burden either way, and the whole pile of them is worth
revisiting deliberately after the MVP rather than one at a time. The cost
until then is a nominal type per provider, plus boxing until the backend
unboxes single-payload tags over scalars — an optimization, not on the
critical path. [D70] lifts this for anonymous providers, whose
receiver is `this`, and records that a plain function providing a method
is now implemented as this entry says in both compilers.

**[D55] DECIDED and implemented (a functor construct).** [D53] leaves Moss with no way to
map a structure to a structure. `bind` provides one component per line,
which suits `src/cli.moss`'s handful and does not suit `Std`'s ~40 items
(~95 counting chars). The `Wasi`-to-`Std` bridge needs the construct.

**B1 — bulk bind by name.** `bind Std = wasistd;` where the right side is
a module: for each item of the signature, bind it to the same-named
top-level name in that module.

```moss
# lib/wasistd.moss — names chosen to match Std's item names
assume Wasm, Wasi {
  type Int = I32;
  val zero: Int = w::i32_zero;
  fn putchar(c: Char) { ... }
}
# main: bind Std = wasistd;   ==>   bind Int = wasistd::Int, bind zero = ...
```

Cheap, but the correspondence is *spelling*, which is weaker than [D1]'s
exact-symbol rule, nothing states the intent, and renaming an item in `Std`
silently stops matching. It also has no clean answer for the items that are
not plain names: `Int.add` is a (receiver, method) pair, and there is no
obvious top-level name in a module for it to match against.

**B2 — module-level provisions.** Let `provide std::putchar = wasi_putchar;`
appear at a module's top level, and `apply wasistd;` install them.
**Rejected by the designer**: a Moss module is only code organization and
symbol scoping ([D53]), so letting one carry provisions would make it a
structure, which is exactly the conflation [D53] exists to prevent.

**B3 — a functor declaration (recommended).** Its own construct, as [D29]
already anticipated: a name, an argument signature, a result signature, and
a body of binds.

```moss
functor WasiStd: Wasm, Wasi -> Std {
  bind int::Int = Num;
  bind char::Char = Num;
  bind int::zero = Num (w::i32_zero);
  bind Int.add = Num.add;              # per [D54], through the wrapper
  bind string::print = wasi_print;
  ...
}

assume Wasm, Wasi {
  fn main() {
    apply WasiStd;
    my_program();                      # assumes Std
  }
}
```

Checked once, where it is written, against its declared result signature —
so a half-built bridge reports at the bridge rather than at every use — and
totality follows [D22]: every item of the result signature gets a bind or it
is an error. At an application site the argument signature must be in
context, and the binds land in the current block, lexically scoped like any
other bind. Nothing about modules changes: a functor is a declaration that
lives in one, like a `type` or a `context`.

Implementation is close to free given the backend as it stands: a functor
body is a list of binds, application inlines them at the site, and both
kinds of bind already compile (val binds as locals, fn binds by
specialization). It needs a grammar production, the totality check, and
inlining — no new backend machinery.

Sub-questions, as the designer settled them:

1. **Application is spelled `bind WasiStd;`** — the `bind` keyword rather
   than a new one, for consistency with the statement it abbreviates. A
   `bind` with no `=` applies a functor; the functor's declaration already
   names its result, so the application site need not restate it.
2. **No value parameters yet.** Nothing needs them: everything in `Std` is
   derivable from `Wasi` plus constants, and anything that *would* want one
   can put the val in the functor's argument signature and bind it before
   applying — which costs encapsulation (the caller must name the
   functor's own val) but no expressiveness. Supporting them later
   surfaces less than [D29] feared: [D18] already says val bindings never
   participate in type identity, so two applications with different values
   still yield the same types; and application already evaluates
   expressions, so a value argument adds no runtime notion. The one real
   cost is that `bind F(x);` reads like a call, blurring the
   function/functor line [D29] wants kept — which the `bind` keyword at the
   application site is enough to hold. Revisit when the argument-signature
   workaround actually bites. Note this answers only [D29]'s *taking* half;
   functors must **produce** val bindings, since `Std` is mostly vals.
3. **The result may be composite**, exactly as the argument is:
   `functor F: A, B -> C, D { ... }`. A declared `context` is the common
   case, not a requirement.
4. **Composition needs no rule.** An application is a run of binds, so
   `bind A; bind B;` is their concatenation; if both bind a key, the later
   shadows for the rest of the block, exactly as two hand-written binds do
   today. The only check worth adding is that a functor may not apply
   itself, which would not terminate at elaboration.

Two options from rev 7 are withdrawn. Extending bracket application to fns
and vals was a category error — brackets *refine a signature* ([D53]) and
application *produces provisions*. Dropping signatures for `Std` and
letting need propagation deliver ordinary definitions would remove the one
thing this design exists to test. (That `Std` stays a signature is
independent of [D52]'s point that the *compiler* skips it and assumes
`Wasi` directly; `Wasi` is a signature too.)

**[D61] DECIDED by the designer (how a method call finds its method).**
The two kinds of method reach the (receiver type, method symbol) key by
different routes, and the difference is not a detail:

- An **attached** method must attach to a nominal type and must be
  declared in the same file as that type. It never needs importing, and
  a call matches it **by name** in the receiver's own module.
- A **detached** method may be declared anywhere, must be **explicitly
  imported** (or reached as `mod::m`), and does **not** match on the name
  it was declared under. A renamed import is called by its new name.

Implementation found the bootstrap violating the second half. Its
`synth_method` fell back, when the name was in scope nowhere, to
searching the environment's provisions for one whose method's
*declaration* name matched the call — with an ambiguity error when two
did. That is matching by spelling, which is the weakness [D55] rejected
option B1 for and which [D1] exists to rule out. **Both compilers now
implement the rule as stated above**; the library change below is what
made the bootstrap able to follow.

**What the strict rule cost, and the fix.** Removing the fallback broke
103 of the bootstrap's tests, all for one reason. Four detached
accessors were declared *separately in each container module* —

| name | declared in |
|---|---|
| `.get` | list.moss, string.moss, strlist.moss |
| `.length` | list.moss, string.moss, strlist.moss |
| `.push` | list.moss, strlist.moss |
| `.read` | cell.moss, path.moss |

— so each spelling was several distinct symbols, and by [D44] one scope
cannot name two of them. Every file in `src/` calls `.length` on both a
`String` and an `IntList`, so no import list could rescue this: it was
not a prelude problem, it was a library one. Note the fallback is exactly
what had been hiding it.

The designed answer, now **implemented**, is to declare each accessor
**once**, with the element type as a requirement, and to supply that per
provision with a bracket application — which is what an application on a
context item is for ([D22], and [D43]'s own `A.gimme[Foo=B]`), and what
makes "the key is (receiver, method)" earn its keep.
[`lib/access.moss`](/lib/access.moss) is the whole of it:

```moss
type Elem;
assume Int {
  fn .length(): Int;
  assume Elem { fn .get(index: Int): Elem; }
}
assume Elem { fn .push(value: Elem); fn .read(): Elem; }
# and in Std:  String.get[Elem=Char], IntList.get[Elem=Int], StrList.get[Elem=String]
```

The question the sketch left open — whether `Elem` can be one shared
symbol across all the receivers or wants one per container — answers
itself as **one**, exactly [D51]'s corrected finding: a provision is
keyed by (receiver, method), so `String.get` and `IntList.get` are
distinct provisions of one symbol and their substitutions never meet. The
substitution is local to the item that writes it and is deliberately not
recorded in the environment's type map, which is what keeps `Elem = Char`
at one receiver from unifying with `Elem = Int` at the next.

It touched a new `lib/access.moss`, the five container modules, `Std`'s
item list, the containers' and files' provisions in `lib/wasistd.moss`,
and the prelude, which now imports all twenty-nine detached names once.
Two things fell out of implementing it:

- A **bind**'s left-hand side may carry a bracket application
  (`bind IntList.get[Elem=Int] = Ints.get;`). A shared accessor is
  provided at one receiver at a time, and the signature the provider is
  matched against has to be read under that substitution or the bind is
  a type error. Only a *method* bind takes one.
- Both sides of `[Elem=Char]` are read in the module the item is
  **written** in, not the one assuming it. That distinction never arose
  while every application sat in the same module as its `assume`; a
  context declared in `lib/std.moss` and assumed in `src/prog.moss` makes
  it load-bearing.

One consequence for user code, which is [D44] rather than new but is felt
for the first time now that the prelude names every accessor: a module
outside `lib/` may not *declare* a detached method the prelude already
provides, because the declaration and the glob import would be two
symbols under one name. `tests/fixtures/cell.moss`'s `IsCell` had to
rename its `.read`/`.write` to `.fetch`/`.stash`. Whether a local
declaration should instead shadow a glob import is a question this log has
not asked; nothing in `lib/` or `src/` needs it.

`lib/prelude.moss` had already been made to import `.not`, the one
detached name `src/` called with nothing at all in scope; and
`docs/reference/semantics.md` states the two rules, which it previously
blurred into one sentence that sanctioned the fallback.

**What it bought.** The compiler compiles itself, and `S1 == S2` byte for
byte — see [selfhosting.md](../implementation/selfhosting.md). Nothing
else stood in the way: what the compiler reported about its own source
once the library was right was two bugs in `codegen.moss`'s local
numbering, not a missing construct.

**[D62] DECIDED by the designer (provisions keyed by identity; first found by the mechanization).** The
core-calculus soundness proof
([docs/design/core](core/)) refuted two rules the bootstrap shares, and
probing the bootstrap resolves both:

- **Method provisions are head-keyed, and that is a live hole.** A
  provision is stored under (receiver *symbol*, method) —
  `lower.py`'s `env.methods[(head(canon(oty)), method)]`, with `head`
  discarding the bindings — so a provision made while `Elem=Int` was in
  force serves a receiver at a *different* instantiation of the same
  nominal. The bootstrap accepts and runs this, typing a `Char` payload
  as `Int`:

  ```moss
  type Elem;

  assume Elem {
    type Box Elem;
    fn .peek(): Elem;
    fn Box.open(): Elem { match this { Box e => e } }
  }

  # in main: bind Elem = Int; bind Box.peek = Box.open;
  let bc = Box[Elem=Char] (char::a);
  let n: Int = bc.peek();   # accepted; n is char::a
  ```

  Today every value is one scalar, so nothing crashes; under [D59]
  layouts this is a miscompile. The proof's repair is to key provisions
  by the receiver's [D18] identity — (declaration, static bindings) —
  not its head; [D51]'s unique-nominal idiom is why the corpus never
  trips it. *Calculus note (July 2026):* the core now carries this
  repair in full, together with [D43] merging, which is what makes
  identity keying practical: T-Meth demands a unique provision at the
  receiver's (declaration, static bindings) identity *compared modulo
  the merged σ*, so re-keying does not fracture provisions across the
  spellings of one merged atom. The bootstrap still needs the
  corresponding change when [D59] layouts land.

  *Decided (October 2026): the repair, in both compilers.* Once records
  made [D59] layouts real, the example above stopped being harmless: at
  `Box[Elem=Point]` the bootstrap compiled it to an invalid module. Of
  the options — key by identity; keep head keys but check the
  instantiation at each use, which forbids two instantiations' provisions
  in one scope; or one provision for every instantiation, re-specialized
  per receiver, which [D26]'s capture makes unsound — the designer chose
  the first, the calculus's own. A provision is keyed by the receiver's
  canonical type, so one made at `Box[Elem=Char]` serves no
  `Box[Elem=Point]`, and the bootstrap says where it *is* provided; to
  provide at two instantiations, bind twice — rebinding `Elem` between,
  or with the bind's own brackets, `bind Box.peek[Elem=Char] = ...`. The
  self-hosted compiler compiles each such provision for its own type; the
  bootstrap lowers a function once and refuses a wider instantiation
  rather than miscompile it. Standing tests: `tests/wasi/mismatch.moss`
  and `tests/wasi/provisions.moss`.

- **Cross-kind instantiation coherence was a live hole, and is now
  checked.** Nothing compared the `T` of an application against the `T`
  an ambient val/fn provision was made under (`f[T=Char]` satisfied by a
  `seed : T` bound under `T=Int`). This record once called it
  unreachable, because v0 rejects bracket applications at call sites
  altogether. It was not: rebinding a type [D25] does the same thing
  with no brackets at all, and a method bind's own brackets do it for
  methods. The bootstrap matched every requirement by its key alone —
  presence of a provision at `(receiver, method)`, or of the val/fn
  symbol — and so accepted, and ran, programs that read a `Char` as an
  `Int` or a `Point`. Its Wasm printed `97`, and its interpreter
  panicked. The shapes: `bind T = Char; bind seed = char::a; bind T =
  Int;` and then a generic `get_seed()` (the same for an abstract fn);
  `bind W.m = W.get;` at `T = Char`, then `bind T = Int`, then a generic
  consumer of `W.m`; and, with no type ever rebound, `bind
  X.gimme[Foo=Char] = X.get;` while `Foo = Int`, where `X` does not
  mention `Foo` so the key is the same at every `Foo`. The calculus
  rules each of these out: a method item carries its application
  (`IMth τ m θ`) and availability compares it, and val/fn satisfaction
  carries the coherence premise (S-Item; CoreMoss.v deviation 8). The
  mechanization also shows that matching method requirements by
  `(receiver, method)` alone — the implementations' rule — admits a
  well-formed program that goes wrong.

  *Repaired (October 2026), in the bootstrap's front end.* Every val,
  fn and method provision records the instantiation it was made at:
  for each type symbol it depends on, what that symbol denoted where it
  was assumed or bound, read through the union-find there, so a later
  rebinding does not read into it. For a val or fn the symbols are its
  whole telescope (the calculus's `dep_tys`). For a method they are the
  telescope's symbols its signature mentions — its application `θ`,
  beside the receiver that is the key's other half. A callee's
  requirement is met only when the instantiation the callee means,
  read through the caller's bindings, equals the provision's, symbol
  for symbol. Otherwise `lower.py` rejects the call ("calling `use_it`
  needs `X[...].gimme` at [Foo=Int], but the one in the context here
  was provided at [Foo=Char]"). Since calls take no brackets, a call's
  application is the caller's own bindings. A direct call of a provided
  method needs no check, since it is typed by the provision's own
  signature, as T-Meth types it by the provision's `θ`. Nothing in
  `src/` or `lib/` tripped the check. The self-hosted compiler already
  rejected these programs from its back end (`types::fits` in
  `emit_as`, `?S`); moving that check to its front end is deferred. One
  shape it still accepts: a function that assumes `T` and `seed`, then
  rebinds `T` and calls a generic consumer of `seed`
  (`tests/wasi/incoherent_shadowed.moss`), because the old and new `T`
  are one scalar wide. The bootstrap rejects it. (Both since done, by
  [D71]: the self-hosted compiler checks coherence on the program as
  written and rejects `incoherent_shadowed`.) Standing tests:
  `tests/wasi/incoherent_*.moss`, with `tests/wasi/coherent_rebound.moss`
  as the control.

**[D63] DECIDED by the designer (records are the only product type).**
[D15] kept tuples `(A, B)` in the type grammar and deferred implementing
them; they had no expressions or patterns, and nothing used them. The
options were full tuples, with `Tag (a, b)` constructing a tag over the
tuple `(a, b)`; tuples as sugar for records with numbered fields, which
buys nothing because there are no numeric tokens to write `t.0` with;
positional multi-field tags only, `type Add (Expr, Expr)`, with no
standalone tuple values; or none at all. Decided: none. Records already
serve every product — `type Add { l: Expr, r: Expr }`, matched as
`Add { l, r }`, which is as short as a positional pattern — and naming
fields rather than positions is the same preference Moss shows for
context. `(A, B)` is now a syntax error and `(T)` is grouping. Nothing
is ruled out: positional tags, and tuples after them, can still be added
later as pure sugar, provided `(a, b)` is not given another meaning
first. The same change finished records in both back ends, which [D59]
had laid out but neither compiled in full.

**[D64] DECIDED by the designer (bare import paths name library files).**
Every import path was relative to the importing file, so the only way to
reach the standard library was to walk up to it — `"../lib/wasi.moss"` —
which works only for programs that sit inside a copy of this repository.
A program elsewhere had to keep its own copy of `lib/` beside it, and a
symlink would not do, because the compiler sees one preopened directory
under WASI. The driver already knows where the library is: it finds the
prelude, beside the entry file or installed, to pass it as argument zero.
So an import path whose first segment is `.` or `..` keeps its meaning,
and any other path is relative to the prelude's directory: `import
"wasi.moss"` is the library's `wasi.moss` from any file. Absolute paths
are unchanged. Identity is still the canonical path ([D56]), so a library
file reached both ways is one module. Every existing import already began
with `./` or `../`, so nothing changed meaning. A run without a prelude
(only test harnesses do that) has no library, and reads bare paths the
old way.

**[D65] DECIDED by the designer (bare block expressions).** A block was
already an expression in everything but the grammar: the body of a
function, an `if` or a match arm yields its tail, and scopes its `let`s and
its binds ([D25]). But there was no way to write one on its own, so running
a call under a bind and no further — the functor application `bind
MaxLevelDebug;` around one call to `foo()` — took a helper function
holding just those two lines. Now **Block** is a **Primary**: `{ bind
MaxLevelDebug; foo(); }` is an expression statement, and like `if` and
`match` it may drop its `;`. It may also yield a value (`let v = { let t =
f(); g(t) };`). Nothing about blocks changes: the binds and `let`s inside
are gone at the closing brace, and the value is the tail, or `()`, or `|`.
Where a record expression is forbidden — the top of an `if` or `while`
condition or a `match` scrutinee — so is a bare block, since the brace
there opens the body; parenthesize it. Making it real exposed a bug of the
bootstrap's that `if` bodies had always had: its interpreter and Wasm back
end kept a `let` past the end of its block, so one that shadowed an outer
local overwrote it. Both now restore the outer local, as the self-hosted
compiler already did. Standing test: `tests/wasi/block.moss`.

**[D66] DECIDED by the designer (binding a fn to a contextual fn).**
[D26] let `bind f=g;` name "a defined (or currently provided) fn", but
only the first was implemented: with `g` abstract, provided by the
context, the bootstrap said `g` must be a defined function and the
self-hosted compiler said `c_unsupported`. That is exactly the case a
functor written against signatures meets — `functor MaxLevelInfo: Std,
ForceLog -> MaybeLog { bind error = force_error; ... }`, where nothing
about `force_error` is known but that it is provided. The options were
to capture `g` at the bind, or to resolve it late, at each call of `f`,
to whatever `g` is then. Decided: capture, as everything else in a bind
is captured. `bind f=g;` with `g` abstract means precisely binding a
defined forwarder

```moss
assume <the type symbols g's signature needs>, g {
  fn g'(x0: T0, ...): R { g(x0, ...) }
}
```

so the signature check of [D27] is `g`'s signature against `f`'s under
the substitutions in force, the requirement at the bind site is `g`
itself, and a later rebind of `g` — in the same block or anywhere deeper
in the call tree — does not reach `f`. This is pure sugar over the core
calculus (`docs/design/core/CoreMoss.v`): the forwarder is one more
defined declaration in the signature, `EBindFn f g' sm e` with it
satisfies `TBindFnR` as stated (its requirements, `g` and the types,
hold at the bind site), it evaluates by `EvBindFn` to a closure over the
bind-site environment (`DClo g' dc`, with `compose_smap d sm`), and the
forwarder's body is an abstract call, `ECallA g`. The elaborated program
is an ordinary core program, so the soundness proof covers it unchanged
and neither the mechanization nor `core-moss.tex` needed an edit. The bootstrap elaborates it that
way, synthesizing one forwarder per abstract `g`. The self-hosted
compiler gives `f` the provision `g` has at the bind — the same
provider, under the environment it was bound in, with the values it
captured — which is what the forwarder would compile to minus its own
frame; a primitive `g` (a `Wasm` instruction, a `Wasi` import) has no
provision, so there `f` is an alias a call resolves to the primitive.
Method binds are unchanged: the right side of `bind X.m = ...` must
still be a defined fn or an attached method. An abstract method there
would want the same treatment, but it falls out of neither
implementation and nothing needs it yet. Running the motivating program
also relied on the back-end change just before this one — a val need is
passed only if the function can reach it (selfhosting.md, "Keeping it out
of quadratic time") — which is not a language change. Standing test:
`tests/wasi/forward.moss`.

**[D67] DECIDED by the designer (floating point).** [D59] said every
value compiles to a finite sequence of Wasm scalars, "i32 and i64 now,
f32/f64 if they ever arrive". They have arrived.

*Scalars.* `lib/wasm.moss` declares `type F32; type F64;` beside `I32`
and `I64`, and one function per instruction of Wasm 2.0's float half,
named the way the integer ones are (`f64_add`, `f32_lt`,
`i32_trunc_sat_f64_s`): the loads and stores; the comparisons `eq ne lt
gt le ge`, which yield an `I32` exactly as the integer ones do; `abs neg
ceil floor trunc nearest sqrt add sub mul div min max copysign`; and
every conversion among the four types — `convert`, `demote`/`promote`,
`reinterpret`, and both the trapping `trunc` and the saturating
`trunc_sat`, which is the 0xFC-prefixed `nontrapping-float-to-int` that
`wasm-features.txt` already enables. Seventy-four items: the whole float
half but its two constants. There are no float literals, for the reason
there are no others ([D4]): a float starts life as a conversion of an
integer built from `i32_zero` and `i32_one`, so
`f64_convert_i32_s(i32_one)` is 1.0. `f32.const` and `f64.const` stay out with them —
the only constants a literal-free program could name are zero and one,
and the conversions already give those. A slot's valtype may now be any
of the four, and nothing else about [D59]'s layouts changed.

In the back ends, the self-hosted compiler is type-directed, so this was
its valtype table growing from two entries to four, and its locals from
two runs to one per valtype. The bootstrap lowers a function once, with
widths but no valtypes, which was harmless while every parameter was an
i32 (an i64 only ever lived in a local); it now takes a
specialization's parameter valtypes from the call site's arguments —
one specialization per fn binding *and* parameter valtypes — and its
result valtypes from its compiled body, a recursive call in the
meantime assuming i32s and the assumption checked once the body is
done. The interpreter's floats are Python floats, an f32 rounded to
single precision after every operation; NaN, the infinities, signed
zeros, `min`/`max`, ties-to-even `nearest`, trapping and saturating
truncation, and integer-to-float conversion rounded once are all as
Wasm specifies them. Not covered, in either back end, and already true
of i64: a union whose members put different valtypes in one slot, and
the padding of a member narrower than its union where the union's slot
is not an i32. Standing test: `tests/wasi/float.moss`, which the
interpreter and both compilers run to the same output.

*`Float`.* `lib/float.moss` declares an abstract `type Float;`, the
counterpart of `Int`, and `Std` gains it with its operations as the
context `Floats`. Arithmetic and comparison are `num.moss`'s detached
methods provided at `Float` ([D47]): `.add .sub .mul .div .neg .eq .ne
.lt .gt .le .ge`, plus two new to `num.moss` because they are numeric
rather than float-specific, `.sqrt` and `.abs`. Not `.rem`, nor the
bitwise ones. The conversions are `float(n: Int): Float` and, provided at
`Float`, `.trunc(): Int` (toward zero) and `.round(): Int` (to the
nearest, ties to even, which is Wasm's `nearest`); both saturate at
`Int`'s range, and NaN is 0. `.trunc` and `.round` are named for their
rounding, since which of the two integers around a float it becomes is a
choice. `float` is a plain
function where `.char` is a method at `Int`, and that is deliberate: a
method provided at `Int` is an entry in `Int`'s methods, so a scope that
binds `Float` differently would have to rebind it at another type. There
is no `Float` `zero` or `one`; `float(zero)` and `float(one)` are those.

`WasiStd` provides `Float` at f64, by binding a nominal wrapper over
`F64` ([D54], as `Int` binds one over `I32`) in the functor `WasiFloats`.
Nothing in `float.moss` or `Std` says f64, so another provision is
another functor: `lib/wasistd.moss` also has `WasiFloats32`, the same
over `F32`, and a `Std` assembled with it instead runs the same Moss at
single precision — one specialization of each function per `Std`.
`tests/wasi/newton.moss` does exactly that in two sibling blocks. The
shorter form, `bind WasiFloats32;` *inside* a scope that already has
`WasiStd`'s floats, shadows them to the end of that block as [D25] says
of any bind, and now works in all three implementations
(`tests/wasi/shadow.moss`): the bootstrap used to merge a bind's
signature with the provision already in force — elaborated under the
old `Float` — and report a [D43] conflict, where [D43]'s merging is for
the items of contexts being assumed, not for a bind. The bootstrap's
own `Std` provides `Float` natively as an f64, so a program over `Std`
that never applies `WasiStd` has floats too, interpreted and compiled.
No float literals and no float formatting: a program prints a float by
scaling and rounding it to an `Int`. Standing test: `tests/wasi/newton.moss`, run
by the interpreter and both compilers. (Literals arrived with [D68].)

**[D68] DECIDED by the designer (operators and numeric literals).** [D33] reserved the operator
tokens and settled their direction; [D47] fixed the target: an operator is a
call of one specific detached method of `lib/num.moss`, keyed by the receiver
type like any method, so `a + b` at `Int` and `c + d` at `Char` coexist in one
scope. This entry pins down the rest.

Operators and their symbols (all methods of `lib/num.moss` unless noted):

| Syntax   | Means        | | Syntax   | Means       |
| -------- | ------------ |-| -------- | ----------- |
| `-a`     | `a.neg()`    | | `a == b` | `a.eq(b)`   |
| `!a`     | `a.not()`    | | `a != b` | `a.ne(b)`   |
| `a * b`  | `a.mul(b)`   | | `a < b`  | `a.lt(b)`   |
| `a / b`  | `a.div(b)`   | | `a > b`  | `a.gt(b)`   |
| `a % b`  | `a.rem(b)`   | | `a <= b` | `a.le(b)`   |
| `a + b`  | `a.add(b)`   | | `a >= b` | `a.ge(b)`   |
| `a - b`  | `a.sub(b)`   | | `a & b`  | `a.and(b)`  |
| `a << b` | `a.shl(b)`   | | `a ^ b`  | `a.xor(b)`  |
| `a >> b` | `a.shr(b)`   | | `a \| b` | `a.or(b)`   |

`.not` joins `lib/num.moss`; `Bool` (nominal over its union, [D45]) is
provided `.not`, `.and`, `.or`, `.xor` by `Std`, as `Int` is provided
`.and` etc. There are no `&&`/`||`: `&` and `|` on `Bool` are ordinary
methods, so both operands are always evaluated. Short-circuiting is an `if`.

Precedence is a partial order, not a ladder: where people group operators
differently by habit, the operators are simply not ordered, and mixing them
without parentheses is a syntax error (as in Carbon). Precedence exists only
where everyone agrees:

- Unary `-` and `!` bind tightest (method calls and field access already
  bind tighter still). One exception: `-` directly before a numeric literal
  that has a method call is an error, since `-1.abs()` would mean
  `-(1.abs())`; write `(-1).abs()` or `-(1.abs())`.
- Multiplicative: `*` chains freely (`a * b * c`). `/` and `%` are like a
  fraction bar: the left operand may be a product, the right must be a
  single unary operand, and neither associates. So `a * b * c / d` and
  `a * b % c` are fine; `a * b / c * d`, `a / b * c`, `a / b / c`,
  `a / b % c` and `a % b * c` are errors.
- Additive: `+` and `-` are ordinary and left-associative (`a - b + c`,
  `a - b - c`), and bind looser than multiplicative.
- Comparison: `==` `!=` `<` `>` `<=` `>=` bind looser than additive and do
  not associate (`a < b < c` is an error).
- Shifts `<<` `>>` take unary operands only: they are unordered with the
  arithmetic operators (`1 << n + 1` is an error; C, Rust and Java all read
  it as `1 << (n + 1)`, which surprises people who read a shift as a
  multiplication) and do not associate (`a << b << c`, `a << b >> c` are
  errors). A shift may be an operand of a comparison (`x << 1 == y`), where
  every language agrees.
- Bitwise/logical `&`, `^`, `|`: each chains with itself (`a & b & c`), but
  they are unordered with one another (`a & b | c` is an error) and with
  everything except unary operators, comparison included: `a < b & c < d`
  and `x & mask == 0` are both errors; write `(a < b) & (c < d)` or
  `(x & mask) == 0`. C and Rust disagree on exactly this pair, and without
  `&&` the C reading would be the natural one for booleans and the Rust one
  for masks, so neither is given.

Numeric literals. A literal is not a primitive constant but nothing more
than arithmetic over fixed symbols, resolved through the context like
everything else, so it means whatever the arithmetic bound there means:

- An integer literal `123` is `Int`, and means the fold of its digits over
  `zero`, `one`, `.add` and `.mul` (`ten` built from `one`). Monomorphization
  then sees only `I32`/`I64` instructions on constants, which the optimizer
  folds. An oversized literal does what the bound arithmetic does with an
  oversized result: under a wrapping `Int` it wraps; under a checked one it
  traps at run time, when the literal is evaluated, exactly as `n * k`
  would. Overflow semantics is a context choice — `bind int32_unchecked;`
  or `bind int64_checked;` in the designer's notes — and literals are no
  exception; moving a run-time error to compile time for literals alone
  would be against the language's grain. `WasiStd`'s `Int` wraps: its
  `.add` and `.mul` are `i32.add` and `i32.mul`.
- A decimal literal `d.ddd` is `Float`, and means `float(m).div(float(10^k))`
  for its digits `m` and `k` fractional digits, with `m` and `10^k` formed
  in `Float` itself — `m` by Horner's rule over `float` of each digit with
  `.mul` and `.add`, `10^k` as a product of `float(ten)`s — rather than as
  `Int`s handed to `float`, since `Int` may be too narrow to hold them (an
  i32 holds neither `10^10` nor most 16-digit `m`). While `m` and `10^k`
  are exact in the bound format the division is the only rounding, and
  because IEEE division rounds correctly the literal is the nearest float:
  in practice up to about 15 significant digits with small exponents at
  f64, about 7 at f32 (exactly: `m < 2^53` and `k <= 22` at f64, `m < 2^24`
  and `k <= 10` at f32). A longer literal rounds on the way and may be an
  ulp off the nearest float — at f32, `0.333333333333333333333` is not
  `1.0 / 3.0` — and that is simply what the bound float arithmetic
  computes. No literal is checked at compile time. Literals guaranteed to
  be the nearest float could later be one more contextual symbol.
- No suffixes, exponents, hex, or underscores yet.
- Lexing: a digit run followed by `.` and a digit is a decimal literal; a
  digit run followed by `.` and anything else is an integer literal followed
  by `.` (so `1.add(2)` calls a method on `1`). A digit run running straight
  into a letter or `_` is a syntax error.

A literal is typed `Int` or `Float`, never inferred from the expected type:
`zero`/`one` are vals, which cannot be keyed by a receiver type the way
methods are. Other types convert explicitly.

The VS Code grammar gains a `constant.numeric` rule and relabels the
operator tokens from reserved to arithmetic/comparison/logical scopes.

*Implementation.* Both compilers and the interpreter, and the whole of
it: the lexers produce _integer_ and _decimal_ tokens; the parsers climb
the partial order with an error for each unordered pair (the bootstrap
names the two operators, `` `&` and `|` are not ordered; add parentheses
``; the self-hosted parser counts it as a syntax error, as it does every
other); and the desugaring is to the symbols themselves, never to a name.
Which symbols those are is settled once, by the file that declares them
— `lib/num.moss`'s detached `.add` … `.ge`, `.neg`, `.not`;
`lib/int.moss`'s `zero` and `one`; `lib/float.moss`'s `float` — exactly
as `Bool`'s units are found ([D45]; `Lower.lang` in the bootstrap,
`cg_lang` in `src/codegen.moss`). Each is then provided through the
context where the operator or literal is written, so a function that
uses a literal needs `zero`, `one`, `Int.add` and `Int.mul` in its
context (`assume Std` has them), a decimal one `float`, `Float.mul`,
`Float.add` and `Float.div` as well, and an operator at a type nobody
provides it at is the method call's error: `` no method `.add` is
available for `Char` in the context here ``. A literal's `zero` and `one`
are vals, so in a function over `Std` they arrive as parameters like any
other val ([D59]).

`Bool`'s four are attached methods of `lib/bool.moss` — `Bool.flip`,
`Bool.both`, `Bool.either`, `Bool.differs` — which `Std` lists as
`Bool.not`, `Bool.and`, `Bool.or`, `Bool.xor` and `WasiStd` binds; the
bootstrap's native `Std` has them natively, and its back end compiles
them to `i32.and`/`i32.or`/`i32.xor`, since a `Bool` is 0 or 1.

Folding, measured on small programs over `WasiStd` with `wasm-opt -O3`:
`n * 1000003` and `float(n) * 2.71828` compile, in both compilers, to
`i32.const 1000003`, `f64.const 2.71828` and — in the `WasiFloats32`
block — `f32.const 2.71828`; over the bootstrap's native `Std` too. A
literal alone in a function called from two specializations, `fn
answer(): Int { 1234567 }`, kept its `one` as a parameter and its
additions under one `-O3`, and folded to `i32.const 1234567` under
`-O3 --converge`: the constants are only constants once `wasi_one()` has
been inlined into the caller.

Standing tests: `tests/wasi/ops.moss` and `tests/wasi/literals.moss`
(the latter at f64 and, in a nested `{ bind WasiFloats32; … }`, at f32,
long literals included), run by the interpreter and both compilers to the
same output; `tests/wasi/overflow.moss`, where `4294967297` is 1 under
`WasiStd`'s wrapping `Int` and, in a bare block binding a test-local
`Int` whose `.add` and `.mul` trap on overflow, traps — all three
implementations; `tests/wasi/numbers.moss` and `tests/wasi/shadow.moss`
now use both; the precedence table and every refusal are parser tests,
held to the self-hosted parser snippet by snippet; and
`bootstrap/tests/test_grammar.py` keeps the VS Code grammar in step with
the lexer.

**[D69] DECIDED by the designer (direction) and implemented (a packed list
for any element type, generic over its layout).** [D59] made containers
the library's business, and the library had lists of `Int` and of
`String` only. A program that stored floats wrote its own format over
`IntList` — reinterpret an f32 to one i32 word, split an f64 into two —
which is twenty lines of storage code in the thesis's mixed-precision
example, all of it beside the point. The designer approved the direction:
an element type says how it is laid out in words, and *one* list
implementation serves every layout.

*`Std`.* `lib/float.moss` gains `type FloatList` and `fn float_list():
FloatList`, with access.moss's accessors provided at it with
`Elem=Float`, as the context `FloatLists` inside `Floats`. It goes with
`Float` and not in `list.moss` for the reason `float` is a plain function
([D67]): a scope that binds `Float` at another precision has to bind its
list with it, and `bind WasiFloats32;` now does — `float_list()` in that
block makes a list of f32s, and everything else in the program is
untouched. `.set` joins the shared accessors of `lib/access.moss` ([D61]),
since two containers now have one: `IntList.set` is
`IntList.set[Elem=Int]`. So, under any `Std`:

```moss
let xs = float_list();
xs.push(0.1);
xs.set(0, xs.get(0) * 2.0);
num(xs.length());
```

*One implementation, over a layout.* In `lib/wasistd.moss`:

```moss
type Item;
assume Wasm {
  val width: I32;                     # 4-byte words per element
  assume Item {
    fn put(at: I32, value: Item);
    fn take(at: I32): Item;
    context Layout = Item, width, put, take;
  }
}
# packed_list, packed_get, packed_set, packed_push, packed_length:
# assume Wasm, Branch, Layout, over [len | cap | elements...]
```

A layout is a functor binding those four — `NumLayout` (one word,
`i32.store`), `SingleLayout` (one word, `f32.store`), `DoubleLayout` (two
words, `f64.store`) — so storage is packed, four bytes to an f32 and
eight to an f64, and since each element is stored as its own scalar
nothing is reinterpreted on the way in or out. The list is the block
`Ints` always was, with `len` and `cap` counted in elements of `width`
words; growth copies words and does not know what they hold. `IntList`'s
`Ints` is now this code at `NumLayout` (and `StrList` already delegated
to `Ints`), so a list of `Int`, f32, f64 or any type that can say how it
sits in words is the same implementation.

*Receivers.* Each element type still has a nominal wrapper over the
handle — `Ints`, `Singles`, `Doubles` — with five one-line attached
methods, each of which applies its layout and calls the shared code:

```moss
fn Singles.get(index: Num): Single {
  bind SingleLayout;
  packed_get(singles_addr(this), index)
}
```

That is [D51]'s cost and [D54]'s together: a provision is keyed by its
receiver, so two lists of different element types in one scope need two
receiver types; and only an attached method can reach the handle inside
one, which needs a nominal type declared in the same module. Binding the
layout *inside* the method, rather than leaving it a need, keeps the
methods self-contained: a call on a `Singles` finds them by name with
nothing in scope ([D61]), and as providers they ask nothing of the
caller's context. `WasiFloats` binds `FloatList` to `Doubles`,
`WasiFloats32` to `Singles`.

Under the bootstrap's native `Std`, `FloatList` is native like `IntList`:
a Python list in the interpreter, and in its back end an `IntList` of
words, two to an element, read and written with one `f64.load` or
`f64.store`. The self-hosted compiler has no native `Std` and needs
nothing.

*A list of something other than `Float`.* `Singles`, `Doubles` and their
constructors `wasi_singles`, `wasi_doubles` are exported, so a program
that computes in f64 but stores in f32 names `Singles`; a function
written against an abstract list type takes it as any receiver is taken
([D51]):

```moss
bind Store = Singles;
bind store = wasi_singles;
bind Store.push[Elem=Stored] = Singles.push;
bind Store.get[Elem=Stored] = Singles.get;
```

With `Store = FloatList` the last two are unnecessary — the provisions
at its canonical receiver are already in force. A new element type costs
a layout (two one-line functions and a four-line functor) and a wrapper
with its five methods. Rewritten this way, the precision example loses
its whole storage format — `bits32`, `put_double`, `take_double`, the
shift by 32 and the `width` bookkeeping in `report` — and goes from 93
lines of code to 76, with the same output in both compilers.

*Alternatives not taken.*

- **A generic nominal, `type Packed I32;` inside `assume Item`.** Under
  [D62], `Packed[Item=Single]` and `Packed[Item=Double]` would be distinct
  receivers with one set of attached methods, and a new element type
  would cost only its layout. Both compilers parameterize a nominal only
  by the type symbols its payload *mentions*, and a handle mentions no
  `Item`. [D21] as written says a declaration's requirements are its
  assume nesting, which would allow it; the implementations narrowed it
  so that `unit Stop;` under `assume Std` is not a different type per
  binding of `Int`. A way to declare such a phantom parameter is the one
  language change that would shrink this design. Left to the designer.
- **The layout as a need of generic attached methods.** Needs are met
  from the caller's context, so a scope holding a list of `Single` and a
  list of `Num` would need two `put`s under one name at once — [D51]'s
  single instantiation per scope. Capturing the layout at a provision
  bind would work, but then a call by name would not.
- **Layouts over `IntList` in portable Moss**, words read and written
  with `.get` and `.set`. A program over `Std` alone could then define
  layouts, but an f64 would be two calls and an i64 assembly each way,
  and a `Wasm`-level wrapper calling it would have to bind all of `Int`
  and `IntList` first. Not needed yet.

Cost: the self-hosted compiler's generations run in the same time under
`wasm-opt -O3`, and `S1 == S2` still holds. Standing tests:
`tests/wasi/lists.moss` — an `IntList` beside a `FloatList`, pushed past
their first capacity, summed, `.set` and read back, at f64 and in a
`{ bind WasiFloats32; … }` block at f32 — run by the interpreter and both
compilers to the same output, which differs by precision (a thousand
0.1s sum to 100 and to 99.999048); and `test_native_float_list`, the
native `FloatList` interpreted and compiled.

**[D71] DECIDED by the designer and implemented (a nominal's identity is its relevant parameters).** [D21]
made a declaration's requirements its parameters, and the calculus took that
literally: `TNom n θ` is total over every type requirement of `n`'s
telescope. Both compilers had quietly narrowed it for nominals — a tag or
alias is parameterized only by the abstract type symbols its payload
mentions, a unit by none — so that `unit Stop;` under `assume Std` is one
type and not one per binding of `Int`. That narrowing was never recorded,
and the proof did not cover it. Two investigations settled what it costs
(scratch reproducers `narrow/`, `narrow-rocq/`, `coh/`).

*Decided.* A nominal's identity is its **relevant parameters**: the type
symbols its payload mentions, closed through the nominals it mentions — the
least fixpoint over mutually recursive declarations, not the first answer an
in-progress traversal happens to give. A unit has none; an abstract type
symbol has none (as in the calculus already, `TAbs t`). Functions, vals and
contexts keep their whole assume nesting ([D21] unchanged for them).
`Range[T=NameId]` is still how a relevant parameter is applied; applying an
irrelevant one is an error.

*Why it is sound.* The calculus is unchanged except that a tag's telescope
is its relevant parameters. Mechanized (`Narrowing.v`, no `Admitted`, no
axioms beyond the development's): value typing cannot tell apart two
instantiations that agree on the relevant parameters
(`vty_narrow_invariant`), every well-formed type at such a signature is
already narrowed (`wf_ty_narrowed`), and soundness holds for it
(`narrowed_soundness`, an instance of the existing theorem, which never
needed a tag's telescope to be its whole assume nesting). These are now
in the tracked development: `docs/design/core/Narrowing.v`, with the
counterexample below as `KeyOnly.v`, both built and audited by the
`core-calculus-rocq` flake check.

*What it depends on: coherence keyed by θ.* The calculus is protected because
a method item `IMth τ m θ` carries the method's own type arguments θ, and
availability, satisfaction and T-Meth compare them; a provision serves only
the instantiation it was made at ([D62]), and a val or fn requirement
answered from the context must agree with it on the symbols the requirement
mentions (`sat_at`). The implementations matched a method need by
`(receiver, method)` alone, which is unsound with or without narrowing — a
copy of the rules with that one change admits a program that goes wrong
(`KeyOnly.v`), and the bootstrap compiled one that read a `Char` as an `Int`.
Narrowing widens the hole, since it removes the receiver parameter that
used to pin θ. So both compilers check coherence **in the front end**, on
`(receiver, method, θ)` with θ restricted to the symbols the method's
signature mentions, and on `dep_tys` for vals and fns. The bootstrap does
since [a3f1797]; the self-hosted compiler no longer relies on its back
end's type check of a specialization (`types::fits` in `emit_as`, where
a same-width confusion — `Char` as `Int` — escaped it, and where [D16]
says there are no checks).

*What it changes, as specified behaviour.* Two provisions of a method whose
signature mentions none of a narrowed receiver's dropped symbols, made under
different bindings of those symbols, are one provision: the later bind
shadows the earlier ([D25]). Under [D21]'s full identity they were two.
(`narrow/p9b`, `p10`: every implementation prints `b`, which is now what
the language says.) Requirements that differ only at such a receiver
collide when a telescope is formed.

*Phantom parameters: not now* (since done, as [D73]). A handle whose payload mentions nothing but
whose meaning depends on a symbol — [D69]'s `type Packed I32;` under
`assume Elem` — has methods that do not all mention it (`.length(): Int`
depends on the element width but not, in its signature, on `Elem`), so θ
does not save it and it must keep a nominal wrapper per element type. An
explicit phantom list on the declaration (`type Packed[Elem] I32;`,
parameters = mentioned ∪ listed) would lift that; it stays an option for when
the wrappers' cost matters.

*Implementation.* Relevance by least fixpoint in both compilers; front-end
coherence in the self-hosted compiler (closing `incoherent_shadowed`); the
Rocq lemmas into `docs/design/core/`, with the paper's prose on nominal
identity amended; [D21]'s text gains a pointer here.

*Implemented (October 2026).* Where the implementation forced a detail
the text above did not fix, it is recorded here.

- *Relevance.* `type_requirements` (bootstrap) and `reqs_of`
  (`src/types.moss`) solve a declaration and every tag or alias it
  reaches as one least fixpoint, rescanning each payload against the
  current answers until none changes. Both used to read an in-progress
  entry as final, so `type A { b: B, t: T }; type B { a: A };` gave `B`
  no parameters whenever `A` was asked first. Standing tests:
  `tests/wasi/relevance_{a,b}_first.moss` (the same program in both
  orders; `get_b(): B` mentions `T`, so assuming it without `T` is
  [D20]'s error in both) and `relevance_applied.moss` (`B[T=Int]` is
  legal).
- *Irrelevant applications.* Brackets on a unit, an abstract type
  symbol, or a tag or alias that bind a symbol outside its relevant
  parameters are rejected: the bootstrap says "application of `W` binds
  `T`, which is not one of its parameters (the type symbols its payload
  mentions: `Char`; D71)", the self-hosted compiler reports
  `e_irrelevant` (`!Q`), named by the symbol. Both used to ignore such a
  binding (the self-hosted compiler rejected brackets on a unit as
  `e_apply`). Two standing tests wrote one and are corrected:
  `tests/wasi/applied.moss`, whose comment said such a binding "changes
  nothing" — `Box`'s payload now mentions `Elem`, so the application
  substitutes — and `incoherent_rebound.moss`, whose `W[T=Int]` bound
  nothing `W` depends on. Nothing in `src/`, `lib/` or `examples/` wrote
  one. The self-hosted compiler also read a type bind's right side
  without its brackets (`bind U = Box[Elem=Char]` bound `Box`); it now
  applies them. Standing tests: `tests/wasi/irrelevant_{unit,tag}.moss`.
- *Self-hosted coherence.* The self-hosted compiler has no typing pass
  separate from emission, so the check runs where the back end resolves
  a call, but on the program *as written*: each frame keeps a generic
  environment beside its specialized one — its declaration's own, where
  an assumed type is itself, extended by the body's binds — and every
  val, fn and method provision row records the app it was made at (an
  assumed item only what its brackets bind; a bound one in full). A call
  of a defined function or method, and a bind of a defined provider or
  (D66) of a contextual fn, compares the callee's app for each need,
  read through the generic environment, with the provision's, symbol for
  symbol, as `require_needs` does. A mismatch is `e_incoherent` (`!R`),
  named by the item (`!R tests/wasi/incoherent_shadowed.moss seed`). So
  the answer does not depend on what the frame is specialized to; but,
  as for every other check of this compiler, only functions the program
  reaches are checked. `types::fits` in `emit_as` stays: it is this
  compiler's whole check of expression types ([D17];
  `tests/wasi/misfit.moss` is an ordinary type error only it catches),
  and is no longer what catches an incoherent provision. A
  generation of the compiler compiling itself goes from 0.68 to 0.77
  seconds.
- *An assumed item is at the function's whole instantiation.* The
  bootstrap recorded an assumed item's instantiation when the item was
  added, so in `assume seed, T` it read `T` as not in force for `seed`,
  and refused a call between two functions of that block. Both compilers
  now read an assumed item's symbols against the finished environment
  (`tests/wasi/coherent_assume_order.moss`).

**[D70] DECIDED by the designer and implemented (anonymous functions at binds).** Moss has no function types:
by [D2], supplying behaviour means binding a `fn` symbol, which is
"statically a direct call, with the captured context passed as data". The
provider had to be a named, defined function — or, for a method that needs
its receiver, an attached method on a nominal type in its own module
([D54]), which is why the WASI library wraps every representation (`Num`,
`Double`, `Single`) only to have somewhere to attach. This entry lets the
right side of a fn or method bind be an anonymous function, written with a
fat arrow, and nothing else: it is still code plus captured context, never a
value.

Syntax (an alternative of a **Bind** item's right side only):

- **Lambda** = `(` **List**\[_name_ (`:` **Type**)?\] `)` (`:` **Type**)? `=>` **Expr**

```moss
bind narrow = (x: Double) => Single (w::f32_demote_f64(dbl(x)));
bind narrow = (x) => Single (w::f32_demote_f64(dbl(x)));        # types from the declaration
bind Stored.widen = () => Double (w::f64_promote_f32(sgl(this)));  # a method: `this` is the receiver
bind debug = (message) => {};                                      # instead of a named `noop`
bind scale = (x) => x * k;                                         # captures the local `k`
```

The body is an expression, so a block (`=> { ... }`, [D65]) for anything
longer. A lambda anywhere else — a `let` initializer, an argument, a return
value — is an error: there is still nothing to give it a type. The parser
tells a lambda from a parenthesized expression or `()` by what follows the
closing parenthesis (`=>`, or `:` then a type then `=>`).

Typing. The bound declaration fixes the signature, read under the
substitutions in force at the bind as for any provider ([D27]): parameters
and result may be left unannotated, and an annotation that is given must
match that signature exactly — no inference beyond reading it off. The
parameter count must match. For a **method** bind (`bind T.m = ...`), the
parameters are the method's own, excluding the receiver, and `this` in the
body is the receiver, at the bind's receiver type; `This` names that type.
For a plain fn bind there is no `this`.

Capture. Everything free in the body is captured at the bind, as everything
else in a bind is ([D26], [D66]):

- a context item (fn, val, type, method) means what it means at the bind
  site; a later rebind does not reach the lambda;
- a local of the enclosing function (`let`, `var`, parameter) is captured by
  value when the bind runs; a later assignment to a `var` is not seen, and
  the body may not assign to a captured local.

A lambda has no name, so it cannot call itself; recursion takes a named fn.

Elaboration — pure sugar over the core calculus, so the soundness proof of
`docs/design/core/CoreMoss.v` covers it unchanged (as for [D66]): each
captured local `k` becomes a fresh val symbol bound to `k`'s value
immediately before the bind (`EBindVal`), and the lambda becomes a fresh
defined function, assuming the context items and vals it uses, whose body
is the lambda's (with captured locals read through their vals), bound as an
ordinary provider (`EBindFn`; for a method, `EBindMth`, whose rule
`TBindMthR` already states that "the provider's first parameter is the
receiver", and `EvMeth` evaluates it so: the synthesized provider takes
`this` as that parameter). So the calculus never had [D54]'s restriction —
that only an attached method sees its receiver is a limit of the surface
language, which this entry lifts for anonymous providers. Nothing new
exists at run time: the provider is a direct call, its captures travel as
ordinary captured context.

Consequences for the library: a provider that needs its receiver no longer
needs a nominal wrapper to attach to, so the WASI bridge's wrappers stay only
where a nominal type is wanted for its own sake ([D18] identity, [D15] union
heads). Simplifying `lib/wasistd.moss` accordingly is a follow-up, not part
of this entry. The VS Code grammar already highlights `=>`.

*Implemented (October 2026).* Where the implementation forced a detail
the text above did not fix, it is recorded here.

- *Parsing.* A **Lambda** is parsed wherever a **Primary** may stand, so
  that the parser's decision is local (two tokens of lookahead in the
  self-hosted parser: `()` or `(x)` then `=>` or `:`, or `(x` then `:`
  or `,`). "Anywhere else is an error" is therefore reported by
  lowering, not by the parser: the bootstrap says "an anonymous function
  may only be the right side of a fn or method bind (D70)", the
  self-hosted compiler `e_lambda` (`!U`). A lambda as the right side of a
  val or type bind is the same error (resp. the type bind's own). The
  sexpr tag is `Z` (`ex_lambda`); a parameter's absent type prints `_`.
- *What is captured.* The captured locals are the enclosing function's
  locals in scope at the bind that the body names, found by a syntactic
  scan of the body (nested lambdas included) that over-approximates —
  a name the body binds again, or one naming a context item, may be
  captured needlessly, which costs a val and changes nothing, since the
  body's own binder shadows it. A lambda's own parameter names are never
  captured. A functor's binds belong to a declaration, which has no
  locals: a lambda there captures none. The elaborated provider assumes
  *everything* in context at the bind (the draft says "the context items
  and vals it uses"); both back ends already pass a function only the
  vals it can reach, so the difference is not observable.
- *`this`.* In a lambda bound to a fn, `this` is an error even inside a
  method: the lambda does not capture an enclosing receiver. The
  bootstrap's message gains that reason; the self-hosted compiler reports
  `e_this` (`!T`), which it now also reports for `this` in any plain
  function — it used to read the function's first parameter.
- *Errors.* A parameter count or annotation that is not the bound
  declaration's is reported against the lambda (bootstrap: "the
  anonymous function bound to `step` takes 2 parameter(s), but `step`
  takes 1 (D70)", "parameter `c` of the anonymous function bound to
  `step` is annotated `Int`, but `step` declares `Char` there (D70)"),
  and by the self-hosted compiler as `e_signature` (`!S`), named by the
  bound symbol. An assignment to a captured local is "`k` is a local
  captured by an anonymous function, which captures it by value and may
  not assign it (D70)", or `e_lambda` named by the local. A lambda that
  calls the fn it is bound to calls whatever that fn meant at the bind —
  nothing, if the bind is the first (`tests/wasi/lambda_recursive.moss`).
- *Bootstrap.* `check_bind` elaborates a lambda exactly as above: a
  `BindVal` of a fresh val per captured local, then a `BindFn` of a fresh
  `FnIR` whose body is `let k = <val k>; ...; body` and whose needs are
  every key of the bind site's environment. A method lambda is an
  `FnIR` with `has_this`, so the interpreter and the Wasm back end pass
  the receiver as `this` — the calculus's first parameter. Making it run
  in Wasm exposed a bug of [D65]'s: the back end typed every bare block
  used as a value as an `i32`, so `{ 2.5 + 1.0 }` was invalid Wasm; a
  block now yields its tail's layout.
- *Self-hosted compiler.* `bind_lambda` (`src/codegen.moss`) synthesizes
  one symbol per lambda node, generic environment and specialized
  environment of the frame (a captured local's type is the frame's,
  which is specialized): a declaration whose parameter and result types
  are already-elaborated `k_ty_known` nodes — the receiver first and
  unnamed for a method — a requirement list read off the generic
  environment at the bind plus one fresh val per captured local
  (`lower::set_needs`), and the uses `need_used` reads. At each bind the
  captured locals are copied into fresh locals provided as those vals,
  and the provider is bound as a defined one is, its captures taken by
  `capture`. In its frame, each captured val's parameter is named by the
  local it holds, which is how the body reads it and how an assignment
  to it is found.
- *[D54], as written, in both compilers.* A plain function may provide a
  method; its signature is the method's declared one, which excludes the
  receiver, and it cannot see the receiver. The bootstrap's front end and
  interpreter already did so, but its Wasm back end passed the receiver
  anyway and refused the call. The self-hosted compiler checked no
  method bind's signature and passed every provider the receiver, so a
  plain function written to take the receiver first was accepted (the
  `narrow/p9c` probe printed `a`) and one written as D54 says produced
  invalid Wasm. Both now evaluate the receiver and drop it for a plain
  provider, and the self-hosted compiler checks a method provider's
  signature against the method's, through the generic environment with
  the bind's brackets and `This` the receiver (`e_signature`). Nothing
  conflicts with the calculus: a plain provider is the lambda
  `(xs) => p(xs)`, which ignores the receiver its elaboration takes
  first. Standing tests: `tests/wasi/plain_provider{,_receiver,_this}.moss`.
- *Standing tests.* `tests/wasi/lambda.moss` (interpreter, bootstrap
  Wasm and self-hosted compiler: annotated and not, a `let` and a `var`
  captured by value, a rebound fn and val not seen, a functor body, a
  method seeing `this` through a wrapper and with none, a block body, one
  lambda inside another, a float result), the errors
  `tests/wasi/lambda_{arity,annotation,assign,outside,this,recursive}.moss`
  in both compilers, the bootstrap goldens `tests/errors/lambda_*.moss`,
  and the parser agreement snippets `LAMBDA_SNIPPETS`.
- *The gain.* In the thesis's examples (rewritten in a scratch copy),
  the logging example's `noop` becomes `(message) => {}` (91 to 89
  lines) and the precision example's six one-line providers become
  lambdas at their binds (115 to 104 lines); both print the same in
  both compilers.

**[D72] DECIDED by the designer and implemented (a type bind starts a new generation).** [D25] lets a bind
shadow, and for vals and fns that has always meant what it means for a
`let`: code after the bind sees the new one, and whatever was made before
it keeps the old. For types, nothing said so, and the implementations did
something else. A type bind was an edge in one substitution keyed by the
symbol, and an assumed `T` was the identity entry `T ↦ T`, so `bind T =
U;` changed what `T` meant everywhere, including in types already given
to locals and provisions. When `T` is concrete at the bind that is
invisible, because those types were already resolved. When it is
abstract — any `assume`d type, in any generic function — the old and new
`T` become one. The calculus never allowed this: `TBindTyR` requires that
the bound symbol not be in force (CoreMoss.v, deviation 7, which names
exactly this hazard: "shadowing an in-use abstract type symbol would
conflate its two generations").

Dual numbers are the program that needs it:

```moss
functor Dualize: Std -> Float, float, Float.add, Float.sub, Float.mul, Float.div {
  bind Base = Float;    # the duals' base: the Float in force here
  bind lift = float;
  bind Float = Dual;    # the new Float, over the old
  bind float = constant;
  ...
}
```

and then, under `assume Std`, `{ bind Dualize; f(variable(x)).d }` is
f′(x) for an `f` written against `Float`. The functor makes the new
`Float` a `Dual[Base=Float]` over the *old* `Float`. Keyed
by symbol, that is `Float = Dual[Base=Float]`, a cycle: the bootstrap
recursed forever and the self-hosted compiler reported a false
incoherence.

*Decided.* Every type bind starts a new generation of the symbol it binds.

- The right side means what it means at the bind ([D26]), resolved
  there: `bind Float = Dual;` after `bind Base = Float;` is
  `Dual[Base=Float₀]`, where `Float₀` is the generation in force before.
- After the bind, every reference to `T` — a type written in the source,
  a callee's requirement on `T`, a method key with `T` as receiver, a
  literal or operator that goes through `T` ([D68]) — is to the new
  generation.
- Everything that was given a type before the bind keeps it: locals,
  parameters, vals, and provisions made at `T₀` (a provision is keyed by
  its receiver's type, so `Float.add` made at `Float₀` is still there, at
  `Float₀`).
- A generation is the type it was bound to, not a new abstract type, so a
  block's value may leave the block whenever its type can be written
  outside. Since the right side is resolved at the bind, it can unless it
  mentions a type bound inside the block itself, which stays an error (as
  in `TBindTyR`, whose result type must be well-formed outside). It also
  means a diagnostic shows a generation as the type it is: the only
  abstract type printed by name is the one an `assume` supplied, so no
  error says "expected `Float`, got `Float`".

This applies to every type bind, not only to rebinding an assumed type:
when the old generation was concrete, no program can tell the difference,
and one rule is simpler than two.

*Why it is sound.* Elaboration gives each type bind a fresh core symbol,
bound to the resolved right side, and resolves the surface name to it for
the rest of the block. So `TBindTyR`'s freshness premise holds by
construction, and deviation 7 becomes a fact about elaboration rather
than a difference between the surface language and the calculus. The
calculus and its proofs are unchanged, as for [D66] and [D70].

*Consequences.*

- A value of the old generation does not mix with the new: inside the
  block above, `x + 1.0` is a type error, since `x : Float₀` and `1.0 :
  Float`. Moving a value into the new generation is explicit
  (`variable(x)`, `constant(n)`). For automatic differentiation this is
  what rules out *perturbation confusion* (Siskind and Pearlmutter): in
  nested derivatives, the outer variable cannot be mistaken for the inner
  one, because the two are different types and the program has to say
  how the outer is lifted (as a constant, with derivative zero). This
  is not peculiar to Moss: any statically typed language whose nested
  duals have distinct types (`Dual<Dual<f64>>` in Rust) rejects the
  confused program the same way, and Haskell's `ad`, which lifts
  implicitly, needs a rank-2 brand for it, while dynamically typed AD
  (ForwardDiff's promotion in Julia) can silently compute the wrong
  answer. Here it falls out of how binds shadow, with the differentiated
  code written against plain `Float` and no type parameter.
- A type bind makes every fn and val whose signature mentions the type a
  requirement at the new generation, so a provision made at the old one
  no longer satisfies it ([D62], [D71]'s coherence). Code that uses such
  an item after the bind must rebind it — `Dualize` binds `float` for
  this reason — or get an error at the use.
- A program that relied on the old rule changes meaning, and only one
  that was unsound: `let n: Int = ...; bind Int = Checked; n + n` adds
  with the `Int` that `n` has.

*Implementation.* What an assumed `T` is, is no longer `T`: it is `T`'s
*parameter*, a type symbol of its own (`param` in the bootstrap's
`types.py`, `prog::param_of` in the self-hosted compiler) that no bind
ever rebinds. A type stored anywhere mentions parameters, never the
declared symbol, so a later `bind T` cannot reach into it; the declared
symbol `T` is only ever read as a name, through the environment, which
gives the generation in force. A type bind resolves its right side at
the bind. One thing changes shape: a type as a *declaration* states it —
a callee's need app, a need's receiver — mentions the callee's
parameters, and reading it at a call is no longer `canon` but
`instantiate`, which maps each parameter to the generation of its symbol
in force at the call (one made in the calling frame, D66's forwarder's,
is in the frame's terms already). The bootstrap's layout check, which
called any tag containing itself a cycle, now keys by the whole
instantiation, since `Dual[Base=Dual[Base=Float]]` contains a different
`Dual` (with a depth bound for types that only grow). Standing tests:
`tests/wasi/dual.moss` (f, f′ and f″ by rebinding `Float` in functions
generic over it, with one functor binding both generations, Newton's method with the derivative from the same `f`,
and nested differentiation giving 1), `tests/wasi/perturbation.moss`
(the same nested program adding the outer variable as it is: a type
error in both compilers), and `tests/wasi/generations.moss` (`n + n`
above). The bootstrap's front end handles the dual programs; its back
end compiles each function once, at one scalar per abstract type, and so
cannot run a `Float` two wide.

**[D73] DECIDED by the designer and implemented (listed, or phantom, parameters).** [D71] made a nominal's identity its
relevant parameters: the type symbols its payload mentions. That is right
for almost every type — `unit Stop;` under `assume Std` should not be a
different type for every binding of `Int` — and wrong for one important
kind: a *handle*, whose value is an address but whose meaning depends on
a type it never stores. [D69]'s packed list is the case. One
implementation serves every element type, over a `Layout`; but its
handle, `I32`, mentions no `Item`, so `type Packed I32;` under
`assume Item` would be one type whatever `Item` is. Then a list of `Long`s
and a list of `Num`s would be the same type, and their methods would
collide: a provision is keyed by its receiver ([D62]), and [D71]'s
θ-aware coherence tells two provisions apart only by the symbols the
method's own signature mentions, which `.length(): Int` does not. So
every element type gets a wrapper of its own — `Ints`, `Singles`,
`Doubles` in `lib/wasistd.moss`, and in a program that wants a list of
anything else, the same again: in an Advent of Code solution (Day 5,
sorting 64-bit IDs), thirteen of the program's 122 lines are the wrapper
`Longs` and its forwarding methods.

*Decided.* A tag declaration may list, in brackets after its name,
parameters its payload does not mention:

```moss
assume Layout {
  type Packed[Item] I32;

  fn Packed.length(): Int { packed_length(addr(this)) }
  fn Packed.get(index: Int): Item { packed_get(addr(this), index) }
  ...
}
```

- **Tagdef** = `type` _name_ (`[` **List**[_name_] `]`)? **Type** `;`
- A tag's parameters are the type symbols its payload mentions, closed
  through the nominals it mentions as in [D71], **together with** those
  it lists. So `Packed[Item=Long]` and `Packed[Item=Num]` are different
  types, both an `I32` at run time.
- A listed name must be a type requirement of the declaration's assume
  nesting, as a bracket application's must ([D22]). Listing one the
  payload already mentions is allowed and changes nothing: a declaration
  may spell out its whole identity if its author likes, though nothing
  checks that such a list is complete (a lint could).
- Only tags. An alias is the type it stands for, so a parameter it does
  not mention cannot affect it; a unit has no payload to stand beside,
  and nothing yet wants a family of units.
- Nothing else is new, because a listed parameter is a parameter like any
  other:
  - *construction* follows [D71]'s rule for applying one: inside the
    declaration's assume nesting, `Packed (a)` is at the `Item` in force;
    elsewhere it is written `Packed[Item=Long] (a)`;
  - *relevance closes over it*: a tag whose payload mentions `Packed` is
    parameterized by `Item` too;
  - *an attached method sees the receiver's*: inside `Packed.get`, `Item`
    is the receiver's `Item`, as `T` is the receiver's inside a method of
    `Some` — which is the point, since a `Packed[Item=Long]` read at the
    `Item` in force somewhere else would be read at the wrong layout. The
    rest of the method's requirements (`width`, `put`, `take`) come from
    the context at the call or bind, and [D71]'s coherence already
    requires them to have been provided at that `Item`.

*Why it is sound.* The calculus's `TNom n θ` is total over a tag's
telescope; [D71]'s implementations narrow it to the relevant parameters,
and `Narrowing.v` shows that narrowing is safe. Its theorems are already
stated for any relevance map `R` that is *closed* — whatever a tag's
payload mentions, read with nested references narrowed, is in `R` of that
tag (`rel_closed`) — with each tag's telescope exactly its `R`, without
duplicates (`narrowed_sig`). Nothing there asks `R` to be the *least*
closed map; the file's comment calls the least one "the intended" choice,
not a hypothesis. Mentioned-plus-listed is a closed map (listing only
adds), so `vty_narrow_invariant`, `wf_ty_narrowed` and
`narrowed_soundness` cover it as they stand. The one obligation is the
closure itself, which is why relevance must seed the listed symbols
before taking its fixpoint: a tag whose payload mentions `Packed` must
get `Item` too.

*Consequences.*

- The library gains the one generic handle: `lib/wasistd.moss` declares
  `type Packed[Item] I32;` under `assume Wasm, Branch, Layout`, with
  `packed()` and `.length`, `.get`, `.set` and `.push`, so a program's list
  of anything is a `Layout` (how one is laid out in words) and nothing
  more: bind the layout, then `BigList = Packed`, `big_list = packed` and
  the accessors `= Packed.push` and so on, each capturing the layout where
  it is bound ([D26]). The library's own lists followed: `Ints`, `Singles`
  and `Doubles` are gone, and `Std`'s `IntList` is `Packed[Item=Num]` and
  its `FloatList` `Packed[Item=Double]` (`Packed[Item=Single]` under
  `WasiFloats32`), each accessor a lambda that binds the layout inside
  itself, so that the layout does not reach the program's scope. The
  compiler compiling itself, which lives on `IntList`, takes the same
  time either way.
- A phantom parameter keeps apart what a program keeps apart; it does not
  stop one that sets out to forge. Moss has no private constructors, so
  `match l { Packed a => Packed[Item=Num] (a) }` turns a list of `Long`s
  into a list of `Num`s, exactly as `Longs a` can be rewrapped as
  `Singles (a)`. (Rust's `PhantomData` relies on field privacy for that.)
  This entry claims distinct types, not unforgeable ones.
- More distinct instantiations means more specializations, but no more
  than the wrappers they replace.
- It needed [D74]: a phantom is only as good as the rule that a method of
  a `Packed[Item=Long]` is at `Item=Long`, which the compilers did not
  follow.

*Implementation.* Both front ends parse the list (`Tagdef` gains
`params`; the parser-agreement dump prints it as `(d_tag Name [Item]
type)`), seed relevance with the listed symbols before the least
fixpoint, and check each is a type requirement of the declaration's
assume nesting (the bootstrap's message; the self-hosted `e_listed`,
`!V`). The VS Code grammar needed nothing. One thing in the self-hosted
back end had to change: it resolves a method call at the receiver's
*specialized* type, where code written against an abstract receiver
(`xs.push` on a `BigList`) can find an attached method of the same name
(`Packed.push`) and call it directly, skipping the provision that
carries the captured layout. It now prefers a provision, at that
receiver, of a detached method of the name the call uses, when the scope
has one — found by name, since the generic code may have reached the
method through its receiver's home module ([D61]) — which is what the
code meant; where the provision is the attached method itself
(`bind IntList.push[Elem=Int] = Ints.push`), the two are one. Standing
tests: `tests/wasi/phantom.moss` (two element types in one function,
each through its own codec), `tests/wasi/packed.moss` (generic code over
a list of i64s that is `Packed[Item=Long]`), `tests/wasi/phantom_unlisted.moss`
(listing a type nothing requires) and `tests/wasi/phantom_misread.moss`
(with [D74]).

**[D74] DECIDED by the designer and implemented (an attached method is at its receiver's instantiation).**
A call of an attached method took the method's type parameters from the
context of the call, not from its receiver. So with

```moss
assume T { type Box T; fn Box.get(): T { match this { Box x => x } } }
let b = { bind T = Int; Box (42) };     # b : Box[T=Int]
{ bind T = Char; putchar(b.get()); }
```

`b.get()` was typed `Char` and printed the `Int` 42 as `*`, in both
compilers — an `Int` read as a `Char`, and at types of different widths a
miscompilation. The calculus rules it out: a method is either a provision,
which `TMethR` uses only at a receiver type equal to the receiver's, or a
direct call with an explicit θ (`ECallD`), whose first parameter would
then have to be a `Box[T=Char]`.

*Decided.* An attached method is at its receiver's instantiation: for
every type its receiver's declaration is parameterized by, the call reads
it as the receiver's type binds it, whatever the calling scope has bound
since. `b.get()` above is an `Int`, and passing it to `putchar` is a type
error. Everything else the method needs comes from the calling scope as
before, and [D71]'s coherence requires it to have been provided at that
instantiation, so a `Cell[Item=Int]` read where the `Char` codec is in
force ([D73]) needs its codec at `Item=Int` and is an error. This is the
calculus's `ECallD` with θ chosen by elaboration, and what generic types
mean elsewhere (`List<Int>.get` is an `Int` wherever it is called); it is
also [D72]'s rule seen from the value's side: what was typed before a
bind keeps its generation, and so do its methods. The alternative — the
method at the calling scope's θ, and a receiver at another one a type
error — would forbid calling `b.get()` at all once `T` is rebound.

*Implementation.* The bootstrap checks an attached call in a view of the
environment where the receiver's type arguments are bound; the
self-hosted compiler builds the callee's environment the same way
(`receiver_callee_env`). The bootstrap reports the misread codec as
incoherence; the self-hosted compiler, whose coherence check runs in the
generic environment without the receiver's view, rejects it as a type
error (`?S`) — still rejected, less precisely named. Standing tests:
`tests/wasi/receiver_rebound.moss` and `tests/wasi/phantom_misread.moss`.

**[D75] DECIDED by the designer and implemented (garbage-collected references, as one allocator among others).**
[D59] gave Moss no heap: a value is a run of scalars, and a type that
contains itself has no finite layout ([D58]). So Moss has no recursive
data. Its programs build trees the way the thesis's flattened-AST example
does: a node is an index into a pool, and the interpreter is written
against an interface —

```moss
type ExprRef;
assume Std, ExprRef {
  type Binary { op: BinOp, lhs: ExprRef, rhs: ExprRef };
  ...
  fn .data(): Expr;            # read a node through a reference
  fn add(expr: Expr): ExprRef; # make one
  context Ast = ExprRef, ExprRef.data, add;
}
```

— which is a generic allocation interface: what a reference is, how to
follow one, how to make one. A pool of indices is one way to provide it.
WebAssembly's garbage collection proposal (WasmGC,
<https://github.com/WebAssembly/gc>, which Wasmtime 50 —
the version we pin — runs by default) is another, and this entry adds it as
a provider and nothing more: no syntax, no new kind of type, and the code
written against `Ast` does not change.

*Decided.*

- `lib/wasm.moss` gains one abstract scalar type, a reference **typed by
  what it holds** (a listed parameter, [D73]), two methods on it as
  ordinary context items keyed by receiver ([D47], [D62]), and a
  functor with no body that the platform provides:

  ```moss
  type StructRef;              # any GC struct reference: (ref null struct)

  type Boxed;
  assume Boxed {
    type Ref[Boxed] StructRef;
    fn .boxed(): Ref;          # detached: provided at receiver `Boxed`
    fn Ref.deref(): Boxed;     # attached: provided at `Ref[Boxed=X]`
  }

  functor Heap: Wasm, Boxed -> Boxed.boxed, Ref.deref;
  ```

  The back end gives each type `Boxed` is instantiated at an immutable
  GC struct type whose fields are that type's run of scalars ([D59]),
  and a `Ref[Boxed=X]` the precise type `(ref null $X)`. `.boxed()` is
  `struct.new`; `.deref()` is a `struct.get` per field. They are
  compiled to instructions like any `Wasm` item ([D52]), except that
  which instructions depends on the layout, as a packed list's code
  already does ([D69]). By [D74], `.deref()` is at its
  receiver's `Boxed`, so a well-typed program cannot read a
  `Ref[Boxed=Expr]` as anything but an `Expr`.
- **Recursive types, as in Rust.** A type that names itself through a
  reference has a finite layout (a reference is one scalar) and a
  finite identity (it is named, not expanded):

  ```moss
  type Expr = Binary | Literal;
  type Binary { op: BinOp, lhs: Ref[Boxed=Expr], rhs: Ref[Boxed=Expr] };
  ```

  This is Rust's `Binary(Box<Expr>, Box<Expr>)`, and needs nothing new.
  Code written *generic* over its reference type, as the thesis's AST
  interpreter is (`Binary { lhs: ExprRef, ... }` under `assume
  ExprRef`), cannot bind `ExprRef = Ref[Boxed=Expr]` directly — that
  `Expr` is `Expr[ExprRef=Ref[Boxed=Expr[...]]]`, infinite, exactly as
  Rust's `Expr<R>` at `R = Box<Expr<R>>` is — and ties the knot with a
  nominal wrapper, as Rust's `struct Node(Box<Expr<Node>>)` does:

  ```moss
  type Node Ref[Boxed=Expr[ExprRef=Node]];
  {
    bind ExprRef = Node;
    bind Boxed = Expr[ExprRef=Node];
    bind Heap;
    bind add = (e) => Node (e.boxed());
    bind ExprRef.data = () => match this { Node r => r.deref() };
    report(bin(Div, lit(7), lit(2)));   # the same interp, now on the GC heap
  }
  ```

  (Both shapes are tests: `gc_tree` and `gc_ast`.)
- **The one cast.** Taking a `Ref` apart gives a bare `StructRef`, which
  forgets the precise type for free. Building a `Ref[Boxed=X]` *from* a
  bare `StructRef` is the one place the back end must cast, to
  `(ref null $X)`, and it is exactly forging ([D73]: Moss cannot forbid
  rebuilding a tag at another parameter). A program that never does it
  contains no cast; one that forges at the wrong layout traps at that
  construction, rather than reading the wrong data later. Struct types
  that refer to each other (`Expr`'s struct holds `(ref null $Expr)`s) are
  emitted together as a `rec` group.
- **Every reference is nullable**, `(ref null $X)`. A non-nullable one
  would need the emitter to prove each local assigned before it is read;
  nullable ones are always valid and cost no space. The possible cost is
  a null check on each field read, which Binaryen's optimizer removes
  where it can prove a local is never null.
- A reference is one scalar, so records, tags and unions holding one
  have finite layouts, and locals, parameters and results carry them as
  they carry anything. It cannot be stored in linear memory, so there is
  no `Layout` for it ([D69]) and no packed list of references; a list of
  them is WasmGC arrays, later.
- `wasm-features.txt` gains `gc` (it has `reference-types`).

*Implemented (October 2026), and what the implementation changed.*

- *One struct per type, not per layout.* The draft keyed a struct by
  the run of scalars `Boxed` is laid out as. A recursive type's run
  contains a reference to its own struct (`Expr`'s is `i32 i32 (ref
  null $Expr) (ref null $Expr)`), so a key made of valtypes is circular;
  making it finite would mean minimizing the whole graph of struct
  types, as Wasm's own iso-recursive canonicalization does. A Moss type
  is named, so keying by the (specialized) type `Boxed` is bound to is
  finite and trivial. Consequence: two types with the same layout get
  two struct types, so forging a `Ref[Boxed=Int]` into a
  `Ref[Boxed=Char]` traps too — the cast checks the type, not only the
  layout, which is stricter than the draft's "at the wrong layout".
  All struct types are one `rec` group at the low type indices; every
  function type's index counts past them.
- *Boxing is a context item, provided by the platform's `Heap`
  (designer's ruling).* The first implementation made `new_ref` and
  `Ref.deref` primitives at every instantiation, recognized by name — a
  capability outside the context system, which the designer rejected.
  Instead both are abstract context items, and the platform provides
  them through a functor with no body:

  - `.boxed` is a **detached** method declared under `assume Boxed`, so
    `This` is the receiver and its result `Ref` means `Ref[Boxed=...]`
    at whatever `Boxed` the provision is made at. It is the [D47]/[D61]
    shape: as `IntList.get[Elem=Int]` and `String.get[Elem=Char]` are
    two provisions of one `.get`, `Expr.boxed[Boxed=Expr]` and
    `Stmt.boxed[Boxed=Stmt]` are two of one `.boxed`, keyed by receiver
    ([D62]). A plain `fn box(value: Boxed): Ref` would be keyed by its
    symbol, so `bind Boxed = Stmt; bind Heap;` after the `Expr` one
    would shadow it. An *attached* `fn Boxed.boxed()` would not do: a
    call finds an attached method by name on the receiver's own
    declaration ([D61]), and `e: Expr` is not a `Boxed`; Q5 also wants
    an attached method's receiver nominal.
  - `Ref.deref` is attached to `Ref`, abstract, so it is provided at
    the receiver's whole type, `Ref[Boxed=X]` — keyed by its
    instantiation, and so also one per boxed type.
  - **`functor Heap: Wasm, Boxed -> Boxed.boxed, Ref.deref;`** is a
    functor with no body, which the platform provides. Grammar:
    **Functordef** = `functor` _name_ `:` **List**[**Spec**] `->`
    **List**[**Spec**] (`{` **Bind**\* `}` | `;`), in both parsers and
    both tree dumps (a bodiless functor's binds print `_`; an empty body
    still prints `[]`). [D78] makes this an ordinary abstract functor;
    until it is implemented, only `lib/wasm.moss` may declare one — the file
    by which both compilers already recognize `Wasm` ([D52]) — and
    anywhere else it is an error where it is written (bootstrap:
    "functor `F` has no body; only the platform provides a functor, and
    only lib/wasm.moss declares one"; self-hosted: `!W`, new code
    `e_platform`).
  - **`bind Heap;`** checks its argument signature is in context here —
    `Wasm` assumed, `Boxed` assumed or bound (bootstrap: "applying `Heap`
    needs `Boxed`, which is not available in the context here";
    self-hosted `?Q ... Boxed`) — and then, for each method of its
    result, makes the provision a hand-written method bind would: keyed
    at the receiver the item names read here (`Boxed` → its binding;
    `Ref` → `Ref[Boxed=` that `]`), shadowed into the environment, and
    its application recorded in both compilers' coherence environments,
    so [D71] coherence and [D74] receiver instantiation apply
    unchanged. Only the provider is special: it is the method itself
    (bodiless), and a call through it compiles in place — `struct.new`
    for `.boxed`, a `struct.get` per field for `.deref`, at the struct
    of the type `Boxed` was at the bind. The back ends' code generation
    is the first implementation's. The bootstrap lowers it to a new
    `ir.BindPrim(key, prim, held)`; the interpreter binds a native
    closure; the self-hosted compiler pushes an environment row whose
    provider is the bodiless method.
  - So **code that has not applied `Heap`** — directly, or by assuming
    the items from a caller that did — **cannot box**: `n.boxed()` there
    is "no method `.boxed` is available for `Int` in the context here"
    (self-hosted `?K ... boxed`). Code that is generic over its heap
    assumes the items as any others, e.g. `context ExprHeap =
    Expr.boxed[Boxed=Expr], Ref.deref[Boxed=Expr];` (the method-item
    bracket form of [D61]), and its caller applies `Heap`.
  - The name-based special-casing is gone from both compilers (the
    bootstrap's `gc_prim`/`new_ref` paths, the self-hosted `is_deref`/
    `emit_new_ref`); what is recognized is the symbol of each method
    in lib/wasm.moss, as the provider of a `Heap` provision only.
  - *What it cost.* **A detached method may now be provided and called
    at a receiver with no nominal head** — a union or a record — keyed
    by that type, in both compilers. `Boxed` is usually a union
    (`Expr = Binary | Literal`), and `.boxed` is keyed by receiver, so
    without this nothing but a nominal could be boxed. [D47] left
    "methods on union types" open; this answers it for detached methods
    only, which need no declaration to attach to and are keyed by the
    canonical type since [D62]. Attached methods, and operators on a
    union, still need a nominal head. The designer approved it.
  - *And the bootstrap's specialization key.* Because `main` now binds
    `Boxed = Expr` in its own terms (`Int` = `I32`), while `lit` boxes
    an `Expr` in the terms of an `assume Std` (`Int` abstract), the
    struct for "`Expr`" must be one struct across frames. The bootstrap
    now carries *all* of a callee's type arguments when a program uses
    references (before: only `Boxed` and types holding a reference), so
    `Int` reaches the callee as `I32` and the two are one type. A
    program without references specializes exactly as before.
- *Under [D77].* `Ref` has an attached abstract `.deref`; a detached
  `.deref` in scope and provided at a `Ref` wins over it (rule 4). A spec
  `Ref.deref` always names the attached one, so the detached one reaches
  a `Ref` only through an abstract receiver bound to it
  (`tests/wasi/gc_resolution.moss`). The self-hosted back end preferred
  an attached *abstract* method's provision there, so it now looks up
  the detached name's provision first, as the bootstrap does. `.boxed`
  is detached only and never in question.
- *A union's slot holds one valtype.* [D59] gives a union a
  discriminant and room for its widest member, each slot with the
  valtype of the first member that reaches it. A member that puts a
  reference where another puts a scalar — `type Tree = Leaf | Node;
  type Leaf Int; type Node { l: Ref[Boxed=Tree], r: Ref[Boxed=Tree] }`
  — would need a slot of each. Both compilers refuse it, naming the
  member (bootstrap: "`Node` puts a reference where an earlier member of
  its union puts a scalar, and a union has one valtype per slot";
  self-hosted: `c_unsupported`, `?P ... Node`), rather than emit a
  module that does not validate. (The same was already true, silently,
  of i64 and f64 slots, [D67]; only reference conflicts are reported,
  in both, so they agree.) `Expr = Binary | Literal` with `op` first is
  fine. Laying a union out in per-valtype lanes is the fix; the
  self-hosted compiler could, but the bootstrap lowers a function once
  with its field offsets fixed, so it is *later*.
- *Padding.* A union member narrower than its union is padded with
  zeros of the union's own valtypes — `ref.null $X` for a reference
  slot — where both compilers used to pad with `i32.const 0` (which
  also fixes [D67]'s note about non-i32 padding, wherever the type is
  known).
- *The bootstrap's back end tracks type arguments.* It compiles a
  function once per (fn bindings, parameter valtypes) and learned
  layouts from values; generic code (`lit(n)` under `assume ExprRef`)
  injects a `Literal` into an `Expr` whose padding is a reference only
  if `ExprRef` is `Node`. So a direct call and a named provider's bind
  now carry the callee's type arguments (`ir.Call.tyargs`,
  `ir.BindFn.tyargs`), and a specialization is also keyed by them
  (all of them, since the revision above). A program with no `Ref`
  specializes exactly as before.
- *The interpreter* holds a reference as a Python object that remembers
  the type it was made at, and traps on a forged one where it can tell
  the types apart: it lowers a generic function once, so where either
  type is still abstract it cannot.
- *No `Layout` for a reference.* Nothing can store one in linear memory:
  no `Wasm` primitive takes a `StructRef`, and the bootstrap's native
  containers hold `Int`s. So there is nothing to reject yet.
- *Wasmtime.* The dev shell's is 50.0.0 (after upstream's `flake.lock`
  bump), which runs GC modules by default; so does the flake's.

*Why it is sound.* The calculus barely changes: `StructRef` is a type of
the `Wasm` signature; `Ref[Boxed]` is an ordinary tag with a listed
parameter ([D73]); `.boxed` and `.deref` are ordinary method items, and
`.deref` is at its receiver's instantiation ([D74]). The one addition is
a provider the calculus does not write — the platform's `Heap`, whose
application is a run of method binds (EBindMth) to `struct_new[X]` and
`struct_get[X]`, a family of primitives indexed by the type — and a
detached method keyed at a structural receiver, which is [D62]'s
identity keying with no nominal head to require.

*What it buys.*

- **Allocation strategy as a context.** The same interpreter runs on a
  pool of indices in one block and on the GC heap in another, chosen by
  a bind, statically specialized each way. This is the thesis's central
  capability applied to memory management.
- **The tree examples become expressible.** Sampson's pointer-based AST
  (`Box<Expr>` in Rust), which every other language in the comparison
  writes first, has had no Moss version; with `Ref` it is written as in
  Rust.

*Later, not now* — all of it library primitives and back end, none of it
syntax or types:

- the **null-pointer optimization**: a union of one member whose payload
  is a single reference and otherwise units (`Some r | None`) laid out as
  one nullable reference, `None` as `ref.null` and matching as
  `ref.is_null` — Rust's guarantee for `Option<Box<T>>`;
- mutable fields (`Ref.set`, over `struct.set`);
- reference equality (`Ref.eq` over `ref.eq`, and so `==`, [D68]);
- arrays, for a list of references;
- how it meets multi-memory: probably one allocation interface with the
  GC heap and each linear memory as providers.

*Implementation.* Both back ends: a reference scalar kind (valtypes
`(ref null struct)` and `(ref null $X)`), a struct type per boxed type in
the type section (in `rec` groups where they refer to each other),
`struct.new`, `struct.get`, and `ref.cast` at a forging construction;
the bootstrap's interpreter holds a reference as a Python object. Tests: a
recursive tree built and folded through `Ref`; the AST example's
interpreter over `Ref` beside the pool, in one program; an `Expr` and a
`Stmt` boxed side by side in one scope (`gc_two`); reading a
`Ref[Boxed=X]` at another `Boxed` rejected as a type error; boxing
without `Heap` (`gc_unheaped`) and applying `Heap` without `Boxed`
(`gc_needs_boxed`) rejected as context errors; a bodiless functor
outside lib/wasm.moss rejected (`gc_bodiless`).

**[D77] DECIDED by the designer and implemented (method resolution, consolidated; the provision wins).**
How a method call finds its method is spread over [D36], [D61], [D62],
[D68] and [D74], and two things went wrong once [D73]'s `Packed` gave a
type attached methods with the same names as `lib/access.moss`'s
detached accessors (`IntList` is `Packed[Item=Num]`):

- **The two compilers resolve at different times.** The bootstrap
  resolves while lowering a function once, against its generic types.
  The self-hosted compiler resolves in codegen, against each
  specialization's types. A call written against an abstract `IntList`
  resolves to the context's `IntList.push` in the one and can find
  `Packed`'s attached `.push` in the other — a different function, which
  needs a layout the caller does not have. (The self-hosted back end
  preferred the provision; the bootstrap failed where the receiver is
  concrete, e.g. `xs.push(7)` in `main`.)
- **Nothing says which wins when both exist.** [D61] says an attached
  method matches by name and a detached one must be imported; it does not
  say what happens when a receiver has an attached method `.push` *and*
  the context provides the imported detached `.push` at that receiver.

*The rules, as one list (current behaviour, unchanged except where
marked).* For a call `e.m(args)` with `e : τ`:

1. **Attached.** If `τ`'s declaration (its nominal head) has an attached
   method named `m` — declared in the head's own file ([D61]) — that is a
   candidate. A defined one is called directly; an abstract one is a key
   the context provides ([D36]).
2. **Detached.** If the name `m` is a detached method in scope — imported,
   or reached as `mod::m` ([D61]) — the context's provision of it at
   `τ`, keyed by `τ`'s whole canonical type ([D62]; a structural `τ`
   included, [D75]), is a candidate. An operator is fixed to `num.moss`'s
   symbol whatever is in scope ([D68]).
3. **Instantiation.** An attached method is at its receiver's
   instantiation ([D74]); a provision, at the one it was made at, checked
   by coherence ([D71]).
4. **Which wins** (new): the provision — below.
5. **When** (new): resolution is a front-end decision, made against the
   types *as the code is written* — generic where the code is generic —
   and recorded; a back end never re-resolves against specialized types.
   So a call resolves the same way in every specialization, and the two
   compilers agree by construction.

*Decided with the designer: the provision wins (rule 4).* Where a
receiver has an attached method `m` and the context provides the
imported detached `m` at that receiver, the call is the provision.

- **It is what the code means.** The provider idiom of [D54] — `bind
  Int.add = Num.add;` — makes the detached `.add` the interface and the
  attached `Num.add` only where its implementation lives; `n.add(m)` on a
  `Num` means "the context's `.add` at `Num`", and if a scope rebinds it,
  the call follows. That a context decides behaviour is the language's
  premise.
- **It makes resolution stable under specialization, as a matter of
  meaning.** The other rule, the type's own method winning (Rust's: an
  inherent method beats a trait method), makes one spelling `x.push(v)`
  two functions — the context's where `x`'s type is abstract, the type's
  own where it is concrete — which is the bug that prompted this entry,
  made a rule. With the provision winning, a call means the context's
  method wherever the context provides one, so specializing a function
  cannot change what a call in it means; the self-hosted back end's
  preference for the provision was this rule, applied late.
- **The cost:** a type's own method can be overridden from outside, by a
  context that imports the same detached name and provides it at exactly
  that type — which Rust forbids, and which here is [D76]'s and the
  designer's polyfill story. A lint may flag an override by a different
  function where that is known statically.
- **Rejected: ambiguity as an error.** It rejects the [D54] idiom, whose
  two candidates are usually one function, and a "unless they are the
  same function" carve-out is not decidable where the provision comes
  from a caller's context.

*Why both kinds of method stay.* With [D70]'s lambdas the [D54] idiom is
no longer needed — a provider can be written at its bind — and
`lib/wasistd.moss` can drop attached methods that exist only to be bound.
But attached methods themselves remain, because they are a different
thing: a type's own operations, where detached methods are interfaces —
Rust's inherent and trait methods, under other names.

- An attached method with a body is a function with dot syntax: calling
  it needs nothing in context. Were every method detached, every type's
  ordinary API would be a context requirement, assumed by every function
  that uses it and by everything that calls those — the threading Moss
  exists to remove, moved into method calls.
- It lives in its receiver's namespace and needs no import, so two types'
  `.roll`s never clash; detached names are one symbol per spelling in a
  scope ([D44]), which is why [D61] had to declare the accessors once.
- It is declared in its type's own file ([D61]) — the natural boundary for
  the encapsulation [D73] and [D75] note Moss lacks: "only a type's own
  methods may take it apart" would make a forged `Ref` impossible, should
  that ever be wanted.
- An abstract attached method (`fn CellInt.read(): Int;`) is an interface
  namespaced by its type: a requirement that needs no import.

*Later, perhaps.* The property rule 5 is for — "resolution is stable under
specialization": resolving against generic types and against any
specialization gives the same target — is a candidate theorem about
elaboration, which the calculus (over elaborated terms, deviation 5) does
not cover. A differential test over both compilers, at generic and
concrete call sites, is the cheap first step.

*Implementation.* The bootstrap now prefers the provision of the detached
method the call's name resolves to (imported, [D61]) where an attached
method of that name also exists, and only then: elsewhere resolution is
unchanged, and the tests that pin a detached method's import and an
operator's fixed symbol stand. The self-hosted compiler already chose the
provision, in codegen (`method_of_ty`); under rule 4 resolving there gives
the same answer as resolving early, so rule 5 — moving that decision to its
front end — is a matter of the two compilers agreeing by construction, not
of correctness, and is still to do. `Packed` keeps its attached methods.
Providers in `lib/wasistd.moss` that exist only to be bound may become
lambdas, as a separate change. Standing tests:
`tests/wasi/concrete_list.moss` (an `IntList` method called in `main`) and
`tests/wasi/resolution.moss` (a type's own `.get` beside the context's, at
a concrete and a generic call site, with the provision rebound in a nested
scope: `91122` in both compilers).

**[D79] DECIDED by the designer and implemented (multiple memories).** WebAssembly modules may have more than
one linear memory (multi-memory, part of Wasm 3.0, shipped in Chrome,
Firefox, Node and Wasmtime; not yet Safari), but source languages barely
use it: LLVM's back end cannot put a memory index on a load or store, and
Rust, Zig, Go, AssemblyScript and Virgil have one memory; Dart's
experimental `Memory` API chooses a memory per access but, by its own
account, allows no polymorphism over memories, and its addresses are plain
`int`s. In 2023 Ben Titzer wrote that "no mainstream language has support
for multiple memories" (AssemblyScript #2716). Moss had multi-memory once,
as a `wasm::MemIdx` val passed in brackets to every instruction (2025),
which is the threading Moss exists to remove.

An instruction names its memory as a static immediate, so which memory is
a compile-time choice — in Moss, a contextual type, like `Float`.

*Decided.*

- `lib/wasm.moss` declares `type Memory;` and puts the memory instructions
  (loads, stores, `memory.size`/`grow`/`copy`/`fill`) under `assume Memory`.
  The platform provides `Memory` at the root as the module's first memory,
  index 0, exported as `memory` (which WASI preview 1 requires).
- A program names more memories as units (`unit Scratch;`) and binds
  `Memory` to one in a scope. The back end gives each distinct memory
  reached its own index, emits the memory section, and puts the index on
  every instruction specialized at it.
- Library code is written once against `Memory`: the bump allocator keeps
  its state in the memory it allocates from, and packed lists, strings and
  cells live in the memory in force where they are made —

  ```moss
  {
    bind Memory = Scratch;
    bind WasiContainers;     # lists and strings here live in Scratch
    ...
  }
  ```

- **Handles are typed by their memory.** A handle into a memory lists it as
  a parameter ([D73]): `type Packed[Item, Memory] I32;`, and so for strings
  and cells. A list in `Scratch` and one in the main memory are different
  types, both an address. Inside the block above, `Memory` is a new
  generation ([D72]), so an outer list cannot be passed where an inner one
  is expected; and an outer list's methods run in its own memory, wherever
  they are called ([D74]). Pointers into different memories are told apart
  statically, at no run-time cost — handles, that is: a bare `I32` address
  handed to an instruction carries no memory, and turning a handle into
  one and back at another memory is forging ([D73]).
- WASI preview 1 reads I/O buffers only from memory 0, so `print` takes a
  string in the main memory; text in another memory is copied there
  first — which the types now require rather than leave to be forgotten.
- **The WASI functions are memory-specific too** *(settled with the
  designer after implementation)*. `lib/wasip1.moss` declares them under
  `assume Memory`, and the platform provides them at the root's memory
  only; the memory instructions, under `assume Memory` as well, are
  provided at every memory. So code that reaches a WASI function where
  `Memory` is rebound — a WASI-calling functor applied in a `Scratch`
  block — is incoherent ([D71]), the error for any provision used at an
  instantiation it was not made at. Before this the functor wrote its
  buffer into `Scratch` and WASI read the same address in memory 0 (it
  printed `AA` for `AB`). That was not a soundness failure — the program
  did what its binds said — but an import typed too loosely: a bare `I32`
  where the contract was an address in memory 0. The fix is the same as
  for handles, putting the memory in the declaration, and needs no rule
  at the boundary between the library and the compiler, and no name for
  the main memory.
- `wasm-features.txt` gains `multimemory`.

*What it is for.* Arenas dropped all at once (reset one memory's allocator
— a compiler's per-function scratch data); keeping untrusted input apart
from everything else; and, with a later memory64 entry, a 64-bit memory for
large data beside a 32-bit main one. And for the thesis: the central
capability — a choice at a dynamic point in the call tree that statically
specializes everything beneath it — applied to *where data lives*.

*Prior work and claim.* To our knowledge, Moss is the first language where
reusable library code (allocators, collections, strings) is written once
and statically specialized to any linear memory chosen by an enclosing
scope, with pointers into different memories given distinct types. The
closest designs — Dart's experimental `Memory` API, and the 8f4e language's
memory regions (May–October 2026, since removed), which tracked a pointer's
memory — choose a memory per access or per declaration and have no
memory-polymorphic code. Tools that use multi-memory today do so after
compilation (Binaryen's `wasm-merge`, and binary rewriters such as Bento
and WBSan); RichWasm's types distinguish two kinds of memory but compile to
one. (Survey: the thesis repository's `scratch/mmres/`.)

*Implemented (October 2026), and what the implementation changed.*

- *Instructions are provided at every memory.* Under [D71] an assumed
  instruction would be tied to the `Memory` its provision was made at, so
  after `bind Memory = Scratch` every load and store would be
  unavailable. The platform provides each at every memory, so none is
  tied to one; which memory it uses is the `Memory` in force where it is
  specialized. Binding `Memory` to anything but a unit or the platform's
  own is refused where an instruction is compiled, not at the bind.
- *The library regrouped.* `bind Memory = Scratch; bind WasiContainers;`
  did not work as the library was grouped: the string list did not
  type-check, and `put_bytes`, a WASI call, would have run in `Scratch`.
  Strings are now a `Strings` context that `Containers` includes (and
  `WasiBasics` applies), and `put_bytes` moved to an `Output` context,
  applied by `WasiStd` through `WasiOutput`.
- *`print` is in the main memory because `WasiStd` binds it at the root*,
  there being no name for that memory; inside a block that rebinds
  strings it cannot be called at all, even on a main-memory string.
- *Code written against all of `Std` cannot run in a `Scratch` block*:
  `Std` also holds WASI-backed items that mention strings and lists
  (`print`, `first_arg`, `put_bytes`, paths), provided only at the main
  memory. Code to run there is written against the narrower `Char,
  Numerics, Containers`, as the tests do.
- *Small additions.* `heap_mark` and `heap_reset`, for arenas; `_start`
  sets the heap pointer at address 16 of every memory; memory 0 keeps the
  short encodings, so a one-memory module is as before; only memory 0 is
  exported.
- *The bootstrap* records the `Memory` in force on each memory
  instruction (`Call.memory`), and when a program binds `Memory` carries
  type arguments in its back end and its interpreter, which keeps one
  emulated memory per unit. *The self-hosted compiler* reads `Memory`
  through each specialization's environment and specializes `main` with
  `Memory` bound to a symbol of its own, the main memory, which no
  environment rebinds (without that, binding a name to the main memory
  and then rebinding `Memory` copied within the wrong memory or gave a
  false type error).

*Later.*

- Splitting `Std` into a memory part and an I/O part, so code over `Std`
  can run on another memory's containers.
- Specializing a function only at the context items it reaches: today
  every function under `assume Wasm` is compiled once per memory it is
  reached in, whether or not it touches memory.
- The incoherence error names the first item of `Wasi` (`args_get`), not
  the function actually reached (`fd_write`).
- A name for the main memory, should one be wanted for its own sake.
- Custom page sizes, a per-memory static attribute; memory64, an address
  type per memory; shared memories for threads.

*Implementation.* `lib/wasm.moss`, `lib/wasip1.moss` and
`lib/wasistd.moss` (memory instructions and WASI functions under `assume
Memory`, allocator state, handles' listed `Memory`, the regrouping); both
back ends: memory indices on instructions, the memory section, the main
memory's export; the feature flag. Tests, in `tests/wasi/`: two memories
with a list in each, summed (`memories`); resetting one memory's
allocator leaving the other's data intact (`memory_reset`); text made in
`Scratch` copied into the main memory to be printed, and back
(`memory_copy`); and rejected in both compilers, passing a `Scratch`
list where the main memory's is expected (`memory_mismatch`), printing a
`Scratch` string as it is (`memory_print`), and applying a WASI-calling
functor where `Memory` is rebound (`memory_wasi`).

## 13. Decision index

No outstanding implementation work is blocked on a decision. [D61]'s
library change has landed — one declaration per detached accessor,
provided with bracket applications — the bootstrap's name-matching
fallback is gone, and the compiler under `src/` compiles itself to a
fixpoint. What remains is engineering: the constant factor of `Std`
written in Moss. (Records are done, and there are no tuples [D63].)

Still awaiting the designer, all deferred rather than blocking: [D48]
string literals (when diagnostics/codegen make embedded strings
unavoidable — *not* the prelude path, see [D52]) · attached methods on union-headed *aliases* in general ([D47] — solved
for `Bool` by [D45]'s nominal wrapper, and for detached methods by [D75]) · D29 the `val` half of functors ·
[D50]'s v0 restrictions on defined vals

Everything else is **DECIDED**: D1–D28 · D30–D49 (D3/D4 by designer fiat,
D13 option.moss → alias, D16 no post-monomorphization checks, D22 total or
absent, D24 dropped, D30 depth backstop as sole D16 exception, D31/D35 no
`for`, D33 no operators in the MVP (since landed as D68), D36
methods via Q1–Q7, D41 keep `unit`, D43 consistent merging, D44 import
collisions + `::` tighter than `.`, D45 concrete `Bool`, D46 aliases
export, D47 operators are methods, D49 no automatic TCE — loops are the
idiom, D52 `Wasi` primitive with `Std` a library over it, D53 signature
vs structure, D54 method providers keep the nominal wrapper (lifted
for anonymous providers by D70), D55 functors
as their own declaration applied with `bind`, D61 how a method call
finds its method, D62 provisions keyed by the receiver's identity, D63
records are the only product type, D64 bare import paths name library
files, D65 bare block expressions, D66 binding a fn to a contextual fn
captures it at the bind, D67 f32/f64 are scalars, with the float half of
`Wasm`, and an abstract `Float` in `Std` provided at f64, with no
formatting, D68 operators as calls of `num.moss`'s methods under a
partial-order precedence, with no `&&`/`||`, and numeric literals as
nothing but arithmetic over `zero`, `one`, `.add`, `.mul`, `float` and
`.div`, meaning whatever that bound arithmetic computes, D69 a packed
list for any element type — one implementation over a layout, a nominal
receiver per element type, and `FloatList` in `Std` bound with `Float`,
D71 a nominal's identity is its relevant parameters — the type symbols
its payload mentions, by least fixpoint; applying any other is an error —
with coherence checked on (receiver, method, θ) and on `dep_tys` in both
compilers' front ends, D70 anonymous functions as a fn or method bind's
right side only — `(params) => body`, typed by the bound declaration,
capturing context at the bind and locals by value, `this` the receiver of
a method bind — sugar over the calculus, with D54's plain-function
method providers implemented as written in both compilers, D72 every
type bind starts a new generation of its symbol — what was typed before
it keeps the old one — which is what lets `Float` be rebound in terms of
itself and rules out perturbation confusion, D73 a tag may list
parameters its payload does not mention (`type Packed[Item] I32;`), so
one handle serves every element type, D74 an attached method is at its
receiver's instantiation, not the calling scope's, D77 where a
receiver has an attached method and the context provides the detached
method of that name at it, the provision wins — what the code means, and
the same in every specialization — and both kinds of method stay, as a
type's own operations and as interfaces, D75 garbage-collected references
as one allocator among others — `Ref[Boxed]` over the platform's
`StructRef`, boxing provided by the platform's `Heap` functor, recursive
types through a reference as in Rust, and detached methods at union and
record receivers — D79 multiple memories, `Memory` a contextual type that
memory instructions run in and handles list, with WASI's functions
provided at the main memory alone)

The MVP language is fully pinned down. Next: rewrite
`docs/reference/syntax.md` against this log, then build the bootstrap
pipeline of §11 with the [D40] corpus tests, applying the §12 errata to
`src/` as the corpus comes online.
