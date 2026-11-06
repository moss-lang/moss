# Syntax

A valid Moss source file can be parsed according to a grammar defined on an
alphabet of tokens. This grammar describes the MVP language pinned down in
the [semantics decision log](../design/semantics.md); constructs that were
cut from the MVP (character literal expressions, declaration-site
needs, bind-returning functions) are absent here even where earlier
revisions of this document had them. Operator expressions and numeric
literals came back with [D68], and `for` loops, over an iteration protocol,
with [D85]. References like [D68] are to entries of the decision log.

## Tokens

A Moss source file can be represented as a sequence of tokens ending with the
"end of file" token. Whitespace between tokens is ignored and does not form a
separate token. Comments are also not tokens. A comment is denoted by the
octothorpe character `#` followed by any number of characters until the end
of that line (which makes `#!` shebang lines comments too).

Here is a comprehensive list of all tokens. A token can be written as either
a literal code value or as an italicized lowercase word representing that
token or class of tokens.

- _end_
- one-character symbols
  - `!`
  - `%`
  - `&`
  - `(`
  - `)`
  - `*`
  - `+`
  - `,`
  - `-`
  - `.`
  - `/`
  - `:`
  - `;`
  - `<`
  - `=`
  - `>`
  - `[`
  - `]`
  - `^`
  - `{`
  - `|`
  - `}`
- two-character symbols
  - `!=`
  - `::`
  - `=>`
  - `<<`
  - `<=`
  - `==`
  - `>=`
  - `>>`
  - `->`
- keywords
  - `as`
  - `assume`
  - `bind`
  - `break`
  - `context`
  - `else`
  - `fn`
  - `for`
  - `functor`
  - `if`
  - `import`
  - `in`
  - `let`
  - `loop`
  - `match`
  - `return`
  - `tag`
  - `This`
  - `this`
  - `type`
  - `use`
  - `val`
  - `var`
  - `while`
- _name_: must start with a letter or underscore that can be followed by zero
  or more letters, underscores, or numbers
- _string_: delimited by double quotes, may include escape sequences `\"`,
  `\\`, `\n`, `\r`, `\t`
- _integer_: one or more decimal digits (`0`, `123`, `007`)
- _decimal_: one or more digits, optionally `.` and one or more digits,
  then an optional exponent — `e` or `E`, optionally `+` or `-`, one or
  more digits — where at least one of the point and the exponent must be
  present (`3.14`, `0.5`, `1e-9`, `2.5E10`, `1.5e+3`)

Notes:

- There are no literal tokens for characters. A _string_ is an import
  path or a quoted expression (see [Quoted strings](semantics.md#quoted-strings)).
  Longest-match applies to symbols
  (`==` is one token, never two `=`).
- A digit run followed by `.` and a digit is a _decimal_; followed by `.`
  and anything else it is an _integer_ and then `.`, so `1.add(2)` calls a
  method on `1` and `2.5.abs()` one on `2.5`. So too `1.e5` is `1`, `.`
  and the name `e5`, not an exponent. A `.` after an exponent is never
  part of the literal: `1e5.abs()` calls a method on `1e5`, and `1.5.e3`
  is `1.5`, `.`, `e3`.
- An exponent's value is at most 999 (`1e1000` and `1e-1000` are errors;
  leading zeros do not count). `e` or `E` not followed by a digit, or by a
  sign and a digit, is no exponent, and an `e` and sign with no digit
  after them is an error (`1e+`).
- A digit run may not run straight into a letter or `_` (no suffixes,
  hex or underscores yet: `2x`, `1e`, `1ex` and `1e5x` are errors), but a
  digit may continue a _name_ (`x2`).
- `in` is a keyword: it is the word of a **For**, and names nothing ([D85]).

## Grammar

A nonterminal is written as a bold capitalized word. The top-level node is a
**File**. A **List**\[**X**\] is a possibly-empty sequence of **X** separated
by the comma token `,` with an optional trailing comma.

### Names and applications

- **Path** = _name_ (`::` _name_)\*
- **DotName** = `.` _name_
- **Spec** = (**Path** **DotName**? | **DotName**) **App**?
- **App** = `[` **List**\[**Binding**\] `]`
- **Binding** = **Path** `=` **Spec**
- **AssumeItem** = **Path** **DotName**?

A **Spec** names a symbol: a plain path (`Token`, `parser::TokenId`), an
attached or receiver-keyed detached method (`Path.join`, `A.gimme[Foo=B]`),
or a bare detached method (`.m`). An **App** binds some of its target's
requirements; each one it does not bind must be in force where the
reference is written, and passes through as that same symbol, so nothing
is left dangling ([D22] and its clarification). **AssumeItem**
deliberately has no **App** ([D24]).

### Types

- **Type** = `|` | `|`? **TypeAtom** (`|` **TypeAtom**)\*
- **TypeAtom** = **TypeRef** | `This` | `(` `)` | `(` **Type** `)` | **RecordType**
- **TypeRef** = **Path** **App**?
- **RecordType** = `{` **List**\[_name_ `:` **Type**\] `}`

A bare `|` is the uninhabited (divergence) type. A union's members must
elaborate to nominal types with distinct heads ([D15]). `()` is the unit
type and `(T)` is just `T`: there are no tuple types, because records are
the product type ([D63]). `This` is legal only inside method declarations.

### Files and declarations

- **File** = (**Import** | **Decl**)\* _end_
- **Import** = `import` _string_ (`as` _name_)? (`use` (`*` | **List**\[**UseItem**\]))? `;`
- **UseItem** = **UseName** (`as` **UseName**)?
- **UseName** = _name_ | **DotName**
- **Decl** = **Assume** | **Tydef** | **Aliasdef** | **Tagdef** | **Unitdef** | **Valdef** | **Fndef** | **Ctxdef** | **Functordef**
- **Assume** = `assume` **List**\[**AssumeItem**\] `{` **Decl**\* `}`
- **Tydef** = `type` _name_ `;`
- **Aliasdef** = `type` _name_ `=` **Type** `;`
- **Tagdef** = `tag` _name_ (`[` **List**[_name_] `]`)? (`(` **Type** `)` | **RecordType**) `;` — the bracketed names are listed parameters ([D73])
- **Unitdef** = `tag` _name_ `;` — no brackets, not even empty ones: `tag X[];` is an error ([D73])
- **Valdef** = `val` _name_ `:` **Type** (`=` **Expr**)? `;`
- **Fndef** = `fn` **FnName** `(` **List**\[**Param**\] `)` (`:` **Type**)? (`;` | **Block**)
- **FnName** = _name_ | _name_ `.` _name_ | **DotName**
- **Param** = _name_ `:` **Type**
- **Ctxdef** = `context` _name_ `=` **List**\[**Spec**\] `;`
- **Functordef** = `functor` _name_ (`[` **List**\[_name_\] `]`)? `:` **List**\[**Spec**\] `->` **List**\[**Spec**\] (`{` **Bind**\* `}` | `;`)

In a **UseItem** rename, both sides must agree on dottedness (`use .m as
.m1`, never `use .m as m1`). The three **FnName** forms are a plain
function, an attached method (the receiver must elaborate to a nominal
type), and a detached method. A **Fndef** ending in `;` declares an abstract
function; detached methods admit only that form ([D36]). A **Valdef** with
an initializer is a *defined* val — concrete like a defined function, e.g.
the prelude's `val true: Bool = Bool(True);` ([D50]). A **Functordef**
ending in `;` declares an *abstract functor*, a requirement like an
abstract fn, provided by `bind F = G;` ([D78]); a module anywhere may
declare one, and the platform provides those of `lib/wasm.moss`. Only an
abstract functor may have brackets, which list the types it is
polymorphic in (`functor Heap[Boxed]: …;`, `F[]` included); a functor with
a body has none ([D84]). The binds of a functor with a body are its
whole body, and its result is what they provide ([D55]).

### Statements and blocks

- **Block** = `{` **Stmt**\* **Expr**? `}`
- **Stmt** = **Let** | **Var** | **Assign** | **Bind** | **While** | **Loop** | **For** | (**Expr** `;`?)
- **Let** = `let` _name_ (`:` **Type**)? `=` **Expr** `;`
- **Var** = `var` _name_ (`:` **Type**)? `=` **Expr** `;`
- **Assign** = _name_ `=` **Expr** `;`
- **Bind** = `bind` **List**\[**Spec** (`=` (**Lambda** | **Expr**))?\] `;`
  - an item with no `=` applies a functor of that name ([D55])
- **Lambda** = `(` **List**\[_name_ (`:` **Type**)?\] `)` (`:` **Type**)? `=>` **Expr**
  - an anonymous function ([D70]), legal only as the right side of a fn or
    method bind
- **While** = `while` **Expr** **Block**
- **Loop** = `loop` **Block**
- **For** = `for` _name_ `in` **Expr** **Block**
  - sugar for a `while` over the collection's `.begin`, `.more`, `.get`
    and `.after` ([D85])

The `;` after an **Expr** statement may be omitted only when the expression
is an **If**, a **Match** or a bare **Block** (which end in `}`). The
condition of a **While** and the collection of a **For** are parsed as the
condition of an **If** is (see the notes under Expressions).

A **Block** is an expression context: its value is the trailing **Expr**, or
`()` if there is none. It scopes the `let`s, `var`s and `bind`s inside it,
whether it is the body of a function, an `if` or an arm, or stands alone
as an expression ([D65]): `{ bind MaxLevelDebug; foo(); }` runs `foo` under
that bind and undoes it afterwards. In a **Bind**, a right-hand side that names a type is
written as an ordinary path; which kind of binding it is falls out of the
left-hand symbol's kind.

A **Lambda** is told from `()` and from a parenthesized expression by what
follows the opening parenthesis: `()` or `(x)` followed by `=>` or `:`, or
`(x` followed by `:` or `,` — which no expression has. Both parsers accept
one wherever a **Primary** may stand and leave it to lowering to reject it
anywhere but a bind's right side. Its body is a whole **Expr**, so
`(x) => x + 1` is one lambda, and `=> { ... }` takes a bare **Block**
([D65]) for anything longer.

### Expressions

- **Expr** = **If** | **Match** | **Jump** | **Operators**
- **If** = `if` **Expr** **Block** (`else` (**If** | **Block**))?
- **Jump** = `return` **Expr**? | `break`
- **Match** = `match` **Expr** `{` **Arm**\* `}`
- **Arm** = **Pattern** `=>` (**Expr** `,` | **Block** `,`?)
- **Postfix** = **Primary** **Suffix**\*
- **Suffix** = `.` **Path** (`(` **List**\[**Expr**\] `)`)?
- **Primary** = `(` **Expr** `)` | `(` `)` | `this` | _integer_ | _decimal_ | _string_ | **Block** | **Path** **App**? (**Args** | **RecordExpr**)?
- **Args** = `(` **List**\[**Expr**\] `)`
- **RecordExpr** = `{` **List**\[_name_ (`=` **Expr**)?\] `}`

Notes:

- A **Jump** is an expression of the uninhabited type, so `Eof => return,`
  and `Semi => break,` are valid match arms (as in `src/lex.moss`).
- A **Suffix** with parentheses is a method call; its **Path** is usually a
  single name (`x.m(a)`) but may be module-qualified (`x.b::m(a)`) because
  `::` binds more tightly than `.` ([D44]). A **Suffix** without parentheses
  is field access and its **Path** must be a single name.
- A **Primary** path followed by **Args** is a function call or a tag
  construction; which one is determined during name resolution, not by the
  grammar. A path followed by a **RecordExpr** constructs a record-payload
  tag; a bare path is a unit value or a variable/val reference.
- Like Rust, a **RecordExpr** may not attach to a path at the top level of
  the condition of an **If** or **While** or the scrutinee of a **Match**
  (otherwise `match x { ... }` would parse the braces as a record).
  Parenthesize to construct a record there. The same goes for a bare
  **Block**: there, `{` opens the body, so `if { c } { ... }` is a syntax
  error and `if ({ c }) { ... }` is not.
- A bare **Block** at the start of an **Arm** body is the arm's block
  form, so the comma after it stays optional.

### Operators

Precedence is a partial order rather than a ladder ([D68]): where people
group operators differently by habit, the operators are not ordered, and
writing them together without parentheses is a syntax error. The grammar
says which may meet.

- **Operators** = **Comparison** | **Bitwise**
- **Comparison** = **Operand** (**CmpOp** **Operand**)?
- **CmpOp** = `==` | `!=` | `<` | `>` | `<=` | `>=`
- **Operand** = **Unary** (`<<` | `>>`) **Unary** | **Additive**
- **Additive** = **Term** ((`+` | `-`) **Term**)\*
- **Term** = **Unary** (`*` **Unary**)\* ((`/` | `%`) **Unary**)?
- **Bitwise** = **Unary** (`&` **Unary**)+ | **Unary** (`^` **Unary**)+ | **Unary** (`|` **Unary**)+
- **Unary** = (`-` | `!`) **Unary** | **Postfix**

What that comes to:

| | groups | refused |
|---|---|---|
| unary | `-a * b` is `(-a) * b`; `-a.f()` is `-(a.f())` | `-1.abs()` (write `(-1).abs()` or `-(1.abs())`) |
| `*` `/` `%` | `a * b * c / d`, `a * b % c` | `a / b * c`, `a / b / c`, `a / b % c`, `a % b * c` |
| `+` `-` | `a - b + c` is `(a - b) + c`; `a + b * c` | |
| comparison | `a + b == c * d` | `a < b < c`, `a == b == c` |
| `<<` `>>` | `a << b`, `x << 1 == y` | `1 << n + 1`, `a * b >> c`, `a << b << c` |
| `&` `^` `\|` | `a & b & c`, `(a < b) & (c < d)` | `a & b \| c`, `a < b & c < d`, `x & mask == 0` |

Each operator is a call of one method of `lib/num.moss` on its left (or
only) operand, keyed by that operand's type and provided by the context
like any method ([D47], [D68]):

| Syntax | Means | | Syntax | Means |
|---|---|---|---|---|
| `-a` | `a.neg()` | | `a == b` | `a.eq(b)` |
| `!a` | `a.not()` | | `a != b` | `a.ne(b)` |
| `a * b` | `a.mul(b)` | | `a < b` | `a.lt(b)` |
| `a / b` | `a.div(b)` | | `a > b` | `a.gt(b)` |
| `a % b` | `a.rem(b)` | | `a <= b` | `a.le(b)` |
| `a + b` | `a.add(b)` | | `a >= b` | `a.ge(b)` |
| `a - b` | `a.sub(b)` | | `a & b` | `a.and(b)` |
| `a << b` | `a.shl(b)` | | `a ^ b` | `a.xor(b)` |
| `a >> b` | `a.shr(b)` | | `a \| b` | `a.or(b)` |

There is no `&&` or `||`: `&` and `|` on `Bool` are methods like any
other, so both operands are always evaluated.

### Patterns

- **Pattern** = `_` | **RecordPat** | **Path** (`(` **Pattern** `)` | **RecordPat**)?
- **RecordPat** = `{` **List**\[_name_ (`=` **Pattern**)?\] `}`

A bare single-name **Path** is a binder unless it resolves to a unit or tag;
a **Path** followed by a parenthesized **Pattern** matches a tag and
destructures its payload (`Some(token)`, `parse::Assume(assump)`), written
as the tag is constructed and declared ([D83]); a **RecordPat** after a path
destructures a record payload, with `{ sig }` shorthand for `{ sig = sig }`.
Match arms must be exhaustive over the scrutinee's union; `match e {}`
requires `e` to have the uninhabited type ([D34]).
