# Semantics

This page specifies what Moss programs *mean*, assuming the grammar of
[Syntax](syntax.md). The design history behind these rules, including open
questions, lives in the [decision log](../design/semantics.md); this page
states only the current language.

## Scope and context

Moss separates two questions that most languages merge:

- **Scope**: what does a name refer to? Resolved lexically from imports and
  enclosing declarations, with one exception: resolving a method call
  `x.m(...)` uses the type of `x` as part of the lookup key. Type inference
  is strictly forward, so the receiver's type is always known first.
- **Context**: is the thing a name refers to *available*? Declarations
  without definitions — `type T;`, `val v: T;`, `fn f(): T;`, detached
  methods — introduce *abstract symbols*, in scope but provided by nobody.
  `assume` makes a region require them; `bind` provides them.

A requirement is satisfied only by naming the exact symbol: an enclosing
`assume` of it, or a `bind` of it. There is no structural matching, no
search by type shape, and no specificity ranking. Method dispatch obeys the
same discipline with the pair (receiver type, method symbol) as its key,
demanding exactly one hit.

What "providing" means depends on the symbol's kind, and this fixes the
compilation model:

- A **type** binding chooses a representation. Static; drives
  monomorphization.
- A **fn** or **method** binding supplies code plus the context it captured
  at the bind site.
- A **val** binding supplies a runtime value. Vals are the only context
  that exists at runtime.

## Modules

A module is a source file. An `import` path whose first segment is `.`
or `..` is relative to the importing file; any other names a file in the
standard library, so `import "wasi.moss"` works from anywhere ([D64]).
Cycles are errors. `as` binds a module alias for `::`
access; `use` copies names into file scope (`*` copies all exports).
Exports are the module's own top-level declarations, its explicit `use`d
names, and its `as` aliases — glob imports do not re-export. One local name
may not refer to two different symbols; renames (`use .m as .m1`) resolve
collisions, and `::` binds more tightly than `.`, so `x.b::m()` calls a
module-qualified method. Every file implicitly glob-imports the prelude,
except the standard library itself.

## Declarations and types

| form | meaning |
|---|---|
| `type T;` | abstract type symbol |
| `unit X;` | nominal type with one value, both named `X` |
| `type X = Ty;` | transparent alias |
| `type X Ty;` | nominal tag wrapping a payload |
| `val v: Ty;` | abstract value |
| `val v: Ty = e;` | defined value (concrete; needs nothing from context) |
| `fn f(...): Ty;` / `{...}` | abstract / defined function |
| `fn T.m(...)` | attached method (receiver must be nominal) |
| `fn .m(...);` | detached method (receiver supplied per provision) |
| `context C = items;` | named requirement bundle |
| `assume items { decls }` | requirement block |

Types are unit `()`, structural records, unions, the uninhabited
`|`, and references to declared types. Union members must be nominal with
distinct heads; a member type injects implicitly into any union listing it,
and that is the only subtyping. Type identity is structural over
(declaration, static type bindings): the same declaration applied the same
way is the same type everywhere. A nominal type's identity arguments are
its *relevant parameters*: the abstract type symbols its payload mentions,
directly or through the nominal types it names (less what their brackets
bind), taken over mutually recursive declarations as the least set closed
under that rule. A `unit` has none, so `unit Stop;` is one type whatever
the enclosing `assume`s provide; an abstract `type T;` has none either.

A function, val or context nested in `assume` blocks is implicitly
parameterized by everything they name, and a nominal type by its relevant
parameters among them — this is the entire generics mechanism. A
square-bracket application like `Pair[T=Int]` must bind *all* of the
target's requirements, and on a type may bind only its relevant
parameters: `Stop[Int=Char]`, or `W[T=Char]` where `W`'s payload does not
mention `T`, is an error. An unapplied reference substitutes nothing and
passes requirements through as the same symbols. There is nothing in
between.

A val, fn or method provision serves only the instantiation it was made
at: for a val or fn, what each type symbol of its whole `assume` nesting
denoted where it was assumed or bound; for a method, what the symbols its
signature mentions denoted (its own application, beside the receiver).
A call whose callee needs the item at a different instantiation — after
`bind T = ...` rebinds a symbol, or through a method bind's own brackets —
is rejected.

## Contexts and merging

A `context` declaration names a finite list of items: symbols of any kind,
possibly with total applications, including receiver-keyed methods
(`Int.eq`, `A.gimme[Foo=B]`). Assuming a context is exactly assuming its
members, flattened recursively. Requirements are checked in order: an item
may be assumed only where the symbols its own declaration mentions are
already available.

A context holds at most one binding per key. Mentioning the same key twice
with different bindings *merges* them: the two targets are unified, so the
symbols involved become interchangeable in the assuming region, and binding
one binds them all. Only a merge that would identify two distinct concrete
types is an error, reported where the context is formed.

## `bind`

`bind x=e;` provides a symbol from the statement to the end of the
enclosing block; inner binds shadow outer ones. Type binds take a type and
are static. Val binds evaluate their expression once. Fn binds name a
defined function whose signature must match the abstract signature under
the substitutions in force — nothing is inferred from a bind, ever — and
whose own requirements are satisfied and captured at the bind site — or an
abstract function available here, which means binding a defined forwarder
to it, so it is captured as provided at the bind ([D66]): `bind
error=force_error;` and a later rebind of `force_error` leave `error` as it
was. Method
binds (`bind Horn.loud=Horn.blast;`) provide a detached method at a
receiver type. A plain function may provide one; its signature is the
method's declared one, which excludes the receiver, so it cannot see what
the method was called on ([D54]). An attached method is a provider that
sees the receiver as `this`.

The right side of a fn or method bind may instead be an anonymous function
([D70]), `(x, y: T): R => body`, and nowhere else may one appear. Its
signature is the bound declaration's under the substitutions in force:
the parameter count must match, and annotations, all optional, must match
exactly. In a method bind the parameters exclude the receiver, `this` is
the receiver at the bind's receiver type and `This` names that type; in a
fn bind there is no `this`. Everything free in the body is captured at the
bind: a context item means what it means there, and a later rebind does
not reach the lambda; a local of the enclosing function is captured by
value when the bind runs, so a later assignment to a `var` is not seen and
the body may not assign it. A lambda has no name and cannot call itself.
It is sugar over the calculus: each captured local becomes a fresh val
bound just before, and the lambda a fresh defined function assuming the
context of the bind, bound as any provider is — for a method, with the
receiver its first parameter. `bind debug = (message) => {};` and
`bind Stored.widen = () => Double (w::f64_promote_f32(sgl(this)));` need
no named `noop` and no wrapper to attach to. A method bind may carry a bracket application
(`bind IntList.get[Elem=Int]=Ints.get;`), which is how a method declared
once for several receivers is provided at one of them: the bindings
interpret the abstract signature the provider is matched against, and they
apply to that bind alone. No other kind of bind takes one.

Calling a defined function requires each of its needs to be available at
the call site by key. Since a bound symbol may be known to the callee under
an abstract name the caller has since bound or merged, keys are compared
after canonicalization through the merges in force.

## Methods

`fn T.m(...)` attaches to the nominal type `T`; a call on a `T` receiver
finds it by scope. `fn .m(...);` declares a detached method; a context item
or bind provides it at a receiver, and the bracket bindings attached to the
providing item are used to interpret the method's signature at the call
site. In method declarations, `This` is the receiver's type and `this` the
receiver value. At a call `x.m(a)` the
receiver's (forward-inferred) type is one half of the key and the method
symbol is the other, and the two kinds of method reach that key
differently. An **attached** method is declared in the same module as the
nominal type it attaches to and never needs importing, so the receiver's
own module is the only place it is looked for, and it matches by name. A
**detached** method must be imported, or reached as `mod::m`, which
resolves it to one exact symbol; the name it was *declared* under is not
what a call matches on, so a renamed import (`use .m as .m1`) is called
as `.m1`. Absence is an error.

## Expressions

Blocks are expressions valued by their trailing expression, or `()` — or
`|` when the last statement cannot fall through (a `loop` with no `break`,
or an expression of type `|`). `if`/`while` conditions have type `Bool`,
which is a standard-library tag over two units, eliminated only by `if` (or
by matching the tag). `match` is exhaustive over the scrutinee's union;
`match e {}` requires `e : |`. `return` and `break` are expressions of type
`|`. Iteration is `while` and `loop`: tail calls are *not* eliminated as a
matter of semantics, and recursion is not the loop idiom. There are no
character or string literals; strings appear only as import paths, and
other values enter programs through the environment (`char::a`,
`first_arg()`, `Path.read`).

## Operators and numeric literals

An operator is a call of one specific detached method of `lib/num.moss`
— `a + b` is `a.add(b)`, `-a` is `a.neg()`, `!a` is `a.not()` (the table
is in [Syntax](syntax.md#operators)). Those exact symbols are what it
means, not whatever `.add` happens to be in scope where it is written, and
they are keyed by the left operand's type and provided by the context like
any method: an operator on a type whose context provides no such method is
the same error as calling the method. `Std` provides them at `Int`, at
`Float` (arithmetic and comparison) and at `Char` (comparison), and
`.not`, `.and`, `.or` and `.xor` at `Bool`. There is no `&&` or `||`:
both operands of `&` and `|` are always evaluated, and short-circuiting is
an `if`.

A numeric literal is likewise not a constant but nothing more than
arithmetic over fixed library symbols, resolved through the context, so it
means whatever the arithmetic bound there means. Nothing about a literal
is checked at compile time.

- An integer literal is an `Int`: its digits folded over `lib/int.moss`'s
  `zero` and `one` and `num.moss`'s `.add` and `.mul` at `Int` (each digit,
  and ten, a sum of `one`s). An oversized one does what that arithmetic
  does: under a wrapping `Int` (the standard one: `.add` and `.mul` are
  `i32.add` and `i32.mul`) it wraps, and under a checked one it traps at
  run time, when it is evaluated, as `n * k` would.
- A decimal literal `d.ddd` is a `Float`: `float(m).div(float(10^k))` for
  its digits `m` and its `k` fractional digits, with `lib/float.moss`'s
  `float`, where `m` and `10^k` are built in `Float`'s own arithmetic.
  While both are exact in the format `Float` is bound to (`m < 2^53`,
  `k <= 22` at f64; `m < 2^24`, `k <= 10` at f32 — roughly 15 and 7
  significant digits) the result is the nearest float; a longer literal
  rounds on the way and may be an ulp off it.

A literal is typed `Int` or `Float`, never inferred from the expected
type; other types convert explicitly. With `zero` and `one` provided as
constants, the expansion is constant-foldable, and an optimizer folds it.

## Execution

`main` takes no parameters, returns `()`, and may assume any subset of
`Std`. Running a program elaborates every definition, checks `main`'s
requirements against `Std`, and runs `main` with the runtime's provisions
in force. Three distinct errors mark the boundaries: a name nobody declared
is *not in scope*; a declared but unprovided symbol is *not available in
the context*; a requirement beyond `Std` is *not part of the `Std`
context*.
