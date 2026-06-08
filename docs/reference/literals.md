# Literals

Moss has a strong ambition: *everything* that would normally be a builtin type or
function is instead contextual. Literals are where this ambition is most severely
tested, because the digits of `42` and the bytes of `"hello"` are concrete data
that originate in the compiler's lexer, yet the types they produce (`Int`,
`String`, …) are abstract and contextual. This page specifies how that tension is
resolved: a literal is **desugared into an ordinary expression built from
contextual primitives**, so it is typed and resolved like any other code.

## Why not just make a literal a primitive value?

The tempting move — tag the term `42` with the abstract type `Int` and hand it to
a contextual "realize" function — is unsound. `Int` could be bound to anything: a
record, an arbitrary-precision bignum, a user type `Foo`. The compiler has the
number `42` but no way to manufacture a value of an unknown type from it. So a
literal cannot be a primitive *value* of a contextual type.

The way out is that a literal need not be a primitive value at all. It can be a
*composition* of contextual operations whose result has whatever type the context
dictates. The number lives in the **shape** of that composition, not in any
opaque value.

## Numeric literals

### The type comes from the suffix

A numeric literal's concrete type is determined entirely by its suffix (or its
absence), per [`lib/int.moss`](/lib/int.moss) and the lexer:

| Suffix | Type |
| --- | --- |
| `u32` | `Uint32` |
| `i32` | `Int32` |
| `u64` | `Uint64` |
| `i64` | `Int64` |
| `u` | `Uint` |
| *(none)* | `Int` |

`Uint` and `Int` are *default* types for when you would rather not commit to a
specific width. `Std` guarantees only that `Int` is at least as wide as `i32` and
`Uint` at least as wide as `u32`. A context may make them wider (e.g. an
arbitrary-precision implementation), so a bare literal that does not fit in those
minimum widths requires more than `Std`.

### Desugaring

A literal is rewritten into positional notation over a small set of contextual
digit values (`0`–`10`) and the contextual arithmetic from
[`lib/ops.moss`](/lib/ops.moss), all at the suffix-selected number type. Using
Horner's method, `423` at type `Number` becomes:

```moss
add(mul(add(mul(d4, d10), d2), d10), d3)   // ((4·10 + 2)·10 + 3), all at Number
```

where `d4`, `d10`, etc. are the contextual digit values. Every node here is
typed and resolved by ordinary [context](context.md) resolution: `d4 : Number`,
`mul : (Number, Number) → Number`, and so on. There is no assertion that any
value inhabits an opaque type.

This also means a bare `42` only typechecks where the digit values and arithmetic
for `Int` are in context — which `Std` provides. To use a different number system
for literals, rebind `Int` (and provide that system's digits and arithmetic);
the same literal then builds a value of the new type.

### Where the regress bottoms out

The digit values are themselves values, so something must ground them. That
ground set is finite, small, and lives in the compiler's root context provider,
not in the language surface: it is enough to provide `0` and `1` for each concrete
number type (the additive and multiplicative identities) and derive the rest. The
only genuinely primitive thing the compiler owns is the *desugaring template*
(how a digit string becomes an arithmetic tree); every type and function involved
is contextual.

## String literals

A string literal desugars using a **string builder**, with three contextual
operations:

- a function to create a builder with a buffer of a given size,
- a function to set the character at an index of a builder, and
- a function to convert a finished builder into a `String`.

So `"hi"` becomes, schematically:

```moss
b = string_builder(2);
set_char(b, 0, char_from_codepoint(104));   // 'h'
set_char(b, 1, char_from_codepoint(105));    // 'i'
build(b)
```

Characters are produced from their Unicode code points via a contextual
`char_from_codepoint`, reusing the numeric machinery above. The sizes, indices,
and code points are themselves numeric literals; in the current prototype they
use a fixed internal integer type (`Uint32`). (This couples `Char`/`String` to
the integer context and leaves "which integer type" as a prototype choice; both
are easy to revisit.)

## Correctness, folding, and the cost question

Because a literal is now an ordinary expression, it is **sound and correct with no
compile-time evaluation at all** — `42` genuinely computes `4·10 + 2` and `"hi"`
genuinely fills a buffer. That would be absurdly slow if left as-is, so:

- For the concrete number and string types, the contextual operations are
  compiler intrinsics, and **constant folding** collapses the whole tree to a
  single constant (`i32.const 42`) or a static data segment for the string. This
  is an ordinary optimization, not a correctness requirement.
- For *user-defined* number types, folding their literals would mean evaluating
  user code at compile time — compile-time function evaluation (CTFE). That is
  deferred; until then, literals of user-defined types are correct but unfolded.

## Overflow

Because a literal is built by arithmetic, a literal that overflows its type
overflows *during construction*, with whatever behavior the context's arithmetic
has (checked or wrapping). This unifies literal overflow with arithmetic
overflow, but it pushes the diagnostic to a later phase than a dedicated literal
range check would. A possible future improvement is to give integers size-indexed
types and have the literal operations map each size to the next larger one, so
that an out-of-range literal becomes a *type* error; whether that is worth the
complexity is undecided.
