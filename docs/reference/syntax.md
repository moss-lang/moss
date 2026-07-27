# Syntax

A valid Moss source file can be parsed according to a grammar defined on an
alphabet of tokens. This grammar describes the MVP language pinned down in
the [semantics decision log](../design/semantics.md); constructs that were
cut from the MVP (operator expressions, `for` loops, literal expressions,
declaration-site needs, bind-returning functions) are absent here even where
earlier revisions of this document had them.

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
  - `<<`
  - `<=`
  - `==`
  - `>=`
  - `>>`
- keywords
  - `as`
  - `assume`
  - `bind`
  - `break`
  - `context`
  - `else`
  - `fn`
  - `for`
  - `if`
  - `import`
  - `let`
  - `loop`
  - `match`
  - `return`
  - `This`
  - `this`
  - `type`
  - `unit`
  - `use`
  - `val`
  - `var`
  - `while`
- _name_: must start with a letter or underscore that can be followed by zero
  or more letters, underscores, or numbers
- _string_: delimited by double quotes, may include escape sequences `\"`,
  `\\`, `\n`, `\r`, `\t`

Notes:

- There are no literal tokens for integers or characters, and _string_ is
  accepted by the grammar only as an `import` path. Longest-match applies to
  symbols (`==` is one token, never two `=`).
- Some tokens are lexed but reserved: the keyword `for` and the operator
  symbols `!` `%` `&` `*` `+` `-` `/` `<` `>` `^` `!=` `<<` `<=` `==` `>=`
  `>>` appear in no production below.

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
or a bare detached method (`.m`). An **App** must bind *all* of its target's
requirements (total application, [D22]); **AssumeItem** deliberately has no
**App** ([D24]).

### Types

- **Type** = `|` | `|`? **TypeAtom** (`|` **TypeAtom**)\*
- **TypeAtom** = **TypeRef** | `This` | `(` **List**\[**Type**\] `)` | **RecordType**
- **TypeRef** = **Path** **App**?
- **RecordType** = `{` **List**\[_name_ `:` **Type**\] `}`

A bare `|` is the uninhabited (divergence) type. A union's members must
elaborate to nominal types with distinct heads ([D15]). `()` is the unit
type; `(A, B)` is a tuple. `This` is legal only inside method declarations.

### Files and declarations

- **File** = (**Import** | **Decl**)\* _end_
- **Import** = `import` _string_ (`as` _name_)? (`use` (`*` | **List**\[**UseItem**\]))? `;`
- **UseItem** = **UseName** (`as` **UseName**)?
- **UseName** = _name_ | **DotName**
- **Decl** = **Assume** | **Tydef** | **Aliasdef** | **Tagdef** | **Unitdef** | **Valdef** | **Fndef** | **Ctxdef**
- **Assume** = `assume` **List**\[**AssumeItem**\] `{` **Decl**\* `}`
- **Tydef** = `type` _name_ `;`
- **Aliasdef** = `type` _name_ `=` **Type** `;`
- **Tagdef** = `type` _name_ **Type** `;`
- **Unitdef** = `unit` _name_ `;`
- **Valdef** = `val` _name_ `:` **Type** `;`
- **Fndef** = `fn` **FnName** `(` **List**\[**Param**\] `)` (`:` **Type**)? (`;` | **Block**)
- **FnName** = _name_ | _name_ `.` _name_ | **DotName**
- **Param** = _name_ `:` **Type**
- **Ctxdef** = `context` _name_ `=` **List**\[**Spec**\] `;`

In a **UseItem** rename, both sides must agree on dottedness (`use .m as
.m1`, never `use .m as m1`). The three **FnName** forms are a plain
function, an attached method (the receiver must elaborate to a nominal
type), and a detached method. A **Fndef** ending in `;` declares an abstract
function; detached methods admit only that form ([D36]).

### Statements and blocks

- **Block** = `{` **Stmt**\* **Expr**? `}`
- **Stmt** = **Let** | **Var** | **Assign** | **Bind** | **While** | **Loop** | **Return** | **Break** | (**Expr** `;`)
- **Let** = `let` _name_ `=` **Expr** `;`
- **Var** = `var` _name_ `=` **Expr** `;`
- **Assign** = _name_ `=` **Expr** `;`
- **Bind** = `bind` **List**\[**Spec** `=` **Expr**\] `;`
- **While** = `while` **Expr** **Block**
- **Loop** = `loop` **Block**
- **Return** = `return` **Expr**? `;`
- **Break** = `break` `;`

A **Block** is an expression context: its value is the trailing **Expr**, or
`()` if there is none. In a **Bind**, a right-hand side that names a type is
written as an ordinary path; which kind of binding it is falls out of the
left-hand symbol's kind.

### Expressions

- **Expr** = **If** | **Match** | **Postfix**
- **If** = `if` **Expr** **Block** (`else` (**If** | **Block**))?
- **Match** = `match` **Expr** `{` **Arm**\* `}`
- **Arm** = **Pattern** `=>` (**Expr** `,` | **Block** `,`?)
- **Postfix** = **Primary** **Suffix**\*
- **Suffix** = `.` **Path** (`(` **List**\[**Expr**\] `)`)?
- **Primary** = `(` **Expr** `)` | `(` `)` | `this` | **Path** **App**? (**Args** | **RecordExpr**)?
- **Args** = `(` **List**\[**Expr**\] `)`
- **RecordExpr** = `{` **List**\[_name_ (`=` **Expr**)?\] `}`

Notes:

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
  Parenthesize to construct a record there.

### Patterns

- **Pattern** = `_` | **RecordPat** | **Path** (**Pattern** | **RecordPat**)?
- **RecordPat** = `{` **List**\[_name_ (`=` **Pattern**)?\] `}`

A bare single-name **Path** is a binder unless it resolves to a unit or tag;
a **Path** followed by a **Pattern** matches a tag and destructures its
payload (`Some token`, `parse::Assume assump`); a **RecordPat** after a path
destructures a record payload, with `{ sig }` shorthand for `{ sig = sig }`.
Match arms must be exhaustive over the scrutinee's union; `match e {}`
requires `e` to have the uninhabited type ([D34]).
