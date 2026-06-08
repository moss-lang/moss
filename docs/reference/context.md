# Context

Context is the central idea in Moss. Almost everything that other languages
treat as a builtin — primitive types, arithmetic, even integer and string
literals — is in Moss provided *contextually*. This page specifies what context
is, how a definition declares the context it needs, and how the compiler decides
what satisfies a need.

## Scope versus context

Two separate questions are easy to conflate:

- **Scope** is about knowing *what* a name refers to. Names come into scope
  lexically: from the [prelude](/lib/prelude.moss), from `import`s, and from
  declarations in the same file.
- **Context** is about *having* the thing a name refers to.

A name can be in scope without the thing it names being available in context. In
the "Hello, world!" program, `println` is brought into scope by the prelude, but
calling it still requires the `Std` context to be available — without it, you get
a *context* error, not a *scope* error. (See [Hello, world!](/docs/learn/hello.md)
for a gentle introduction.)

## Contextual definitions

Most top-level declarations introduce a name and describe a **contextual
definition** — a thing that may or may not be available in the context at a given
point in the program. There are several kinds:

| Declaration | Kind | Meaning |
| --- | --- | --- |
| `type T;` | abstract type | A type named `T` exists in context; nothing is said about its representation. |
| `type T U;` | nominal type | A distinct new type whose values wrap a value of type `U`. |
| `type T = U;` | type alias | A name for `U`. Transparently equal to `U` (see below). |
| `val v: T;` | value | A value named `v` of type `T`. |
| `fn f(p: P): R;` | function signature | A function whose *implementation* is supplied by context. |
| `fn f(p: P): R { … }` | defined function | A function with an implementation, which still declares the context it needs. |
| `context C = …;` | composite context | A bundle of needs grouped under one name. |

The key point: a `type T;` does not define a type, and a `fn f(): R;` without a
body does not define a function. They declare that *if* the context provides such
a thing, here is its shape. What the context actually provides is decided
elsewhere, by whoever supplies the context.

### Type aliases are transparent; nominal types are opaque

`type T = U;` (an *alias*) and `type T U;` (a *nominal* type) look similar but are
opposites:

- An alias `type MyString = String;` is **transparent**: `MyString` and `String`
  are the same type, interchangeable everywhere. Equality unfolds aliases before
  comparing.
- A nominal type `type Meters Int;` is **opaque**: `Meters` is a brand-new type,
  distinct from `Int` and from every other nominal type, even one declared as
  `type Feet Int;`. Its values are constructed and destructured explicitly.

## Needs

The square brackets after a name list its **needs**: the contextual definitions
that must be available wherever this definition is used.

```moss
fn print_it[string]() {
  println(string);
}
```

`print_it` needs a value `string` to be in context. A function with no extra
needs can omit the brackets, or write `[]`.

Needs may be **parameterized** and **bound**. In `lib/ops.moss`:

```moss
type AddOut[Lhs, Rhs];
fn add[Lhs, Rhs, AddOut[Lhs, Rhs]](x: Lhs, y: Rhs): AddOut[Lhs, Rhs];
```

`AddOut` is a type parameterized by two other types, and `add` needs `Lhs`, `Rhs`,
and the `AddOut[Lhs, Rhs]` type. At a use site, needs are satisfied by **bindings**
that may pin parameters, like `add[Lhs=Number, Rhs=Number]`.

### `assume`

An `assume` statement at the top of a file adds its bindings to the needs of
every declaration in that file. It is pure convenience — any program can be
rewritten without it by listing the context explicitly in each declaration's
brackets:

```moss
assume Std;
fn main() { … }
```

is exactly

```moss
fn main[Std]() { … }
```

### Composite contexts

A `context` declaration bundles a list of needs under one name, so they can be
required or provided together:

```moss
context Arithmetic[Number] =
  ops::AddOut[Lhs=Number, Rhs=Number]=Number,
  ops::Add[Lhs=Number, Rhs=Number],
  … ;
```

Needing `Arithmetic[Number=Uint32]` is the same as needing each of its members
with `Number` pinned to `Uint32`. `Std` itself is just a large composite context.

## The context stack

At every point in a program there is a **stack of context frames**. Frames are
established by, from the bottom up:

- a function's declared needs (its brackets, including anything folded in by a
  file-level `assume`), and
- each `bind` expression evaluated so far in the current body, which pushes one
  new frame on top.

```moss
fn main() {
  bind greeting = "first";     // pushes a frame
  {
    bind greeting = "second";  // pushes another frame on top
    show();                    // sees "second"
  }
  show();                      // back to "first"
}
```

A single `bind` adds a single frame, even if it binds several things at once:
`bind a = …, b = …;` is one frame containing both.

## Resolving a need

When the compiler needs to satisfy a need, it searches the stack **from the top
down**:

1. Look in the topmost frame for candidates that satisfy the need.
2. If exactly one candidate satisfies it, use it.
3. If two or more candidates in *that same frame* satisfy it, the most specific
   one wins — but only if there is a unique most specific one. If the matching
   candidates have no single most-specific member, that is an **ambiguity error**.
4. If the frame has no candidate, continue to the frame below.

So a binding in a higher frame **shadows** one in a lower frame without
ambiguity; ambiguity only ever arises *within* one frame. This is exactly what
`bind` is for: overriding context that is already present.

```moss
fn main() {
  // Ambiguity error: both candidates live in the same (single) frame, and
  // neither is more specific than the other.
  show[greeting = "first", greeting = "second"]();
}
```

### What "most specific" means

Specificity is not a new mechanism: it is the same adaptation check —
`synthesize` — that decides whether a candidate satisfies a need in the first
place. `synthesize(a, b)` succeeds exactly when `b` is general enough to be
adapted into `a`, i.e. when `a` is an *instance* of `b`. So:

- A candidate **satisfies** a need when the need is an instance of the candidate
  (the candidate is general enough to produce what is needed).
- Candidate `A` is **at least as specific as** candidate `B` when
  `synthesize(A, B)` succeeds — when `A` is an instance of `B`.

Within a frame, the winner is the matching candidate that is at least as specific
as every other matching candidate (the greatest element under this order). Two
mutually-adaptable candidates are equivalent and either may be chosen; if no
single candidate dominates all the others, resolution is ambiguous.

This is a preorder defined entirely by `synthesize`, so it costs nothing new to
implement: the resolver already calls `synthesize` to test satisfaction, and
reuses it pairwise to rank the survivors.

### Providing more than is needed

A definition may assume a **subset** of the context that is available to it; any
surplus is silently discarded. Assuming something that is *not* available is an
error. For example, the runtime provides `Std` to `main`; `main` may use all of
`Std`, some of it, or none, but it may not assume anything beyond `Std`.

## Resolution is static

Context is resolved at compile time: there is no runtime search for an
implementation. The backend monomorphizes types and functions against the
concrete context they are ultimately used in, so each use site is specialized to
exactly the definitions that satisfied its needs. (Contextual *values* are not
monomorphized away; see the implementation docs for how they are represented.)

A consequence worth internalizing: because resolution is contextual and static,
the *same* source — a literal, an arithmetic expression, a function — can compile
to different concrete code depending on the context it is used in. That is the
feature, not a bug. See [Literals](literals.md) for the most striking example.
