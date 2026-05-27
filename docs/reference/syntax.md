# Syntax

A valid Moss source file can be parsed according to a grammar defined on an alphabet of tokens.

## Tokens

A Moss source file can be represented as a sequence of tokens ending with the "end of file" token. Whitespace between tokens is ignored and does not form a separate token. Comments are also not tokens. A comment is denoted by the octothorpe character `#` followed by any number of characters until the end of that line.

Here is a comprehensive list of all tokens. A token can be written as either a literal code value or as an italicized lowercase word representing that token or class of tokens.

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
  - `context`
  - `else`
  - `fn`
  - `if`
  - `import`
  - `let`
  - `static`
  - `type`
  - `use`
  - `val`
  - `var`
  - `while`
- _name_: must start with a letter or underscore that can be followed by zero or more letters, underscores, or numbers
- integer literals
  - _uint32_: must end with `u32`
  - _int32_: may begin with `-`, must end with `i32`
  - _uint64_: must end with `u64`
  - _int64_: may begin with `-`, must end with `i64`
  - _uint_: must end with `u`
  - _int_: may begin with `-`, no suffix
- _char_: delimited by single quotes
- _string_: delimited by double quotes, may include escape sequences `\\`, `\n`, `\r`, `\t`

## Grammar

A nonterminal in the grammar is written as a bold capitalized word. A local variable in a parametric declaration is written as a bold lowercase word. The top-level node is a **File**. A **List** is a possibly-empty sequence of items separated by the comma token `,` with an optional trailing comma.

- **Literal** = _uint32_ | _int32_ | _uint64_ | _int64_ | _uint_ | _int_ | _char_ | _string_
- **Path** = _name_ (`::` _name_)\*
- **Type** = TODO
- **Spec** = `.`? **Path** (`[` **List**\[**Bind**\] `]`)?
- **Entry** = **Spec** | **Literal**
- **Bind** = **Spec** (`=` **Entry**)?
- **Need** = `static`? **Bind**
- **Needs** = (`[` **List**\[**Need**\] `]`)?
- **Block** = TODO
- **Import** = `import` _string_ (`as` _name_)? (`use` **List**\[_name_ | `.` _name_\])? `;`
- **Assume** = `assume` **List**\[**Bind**\] `;`
- **Tydef** = `type` _name_ **Needs** `;`
- **Aliasdef** = `type` _name_ **Needs** `=` **Type** `;`
- **Tagdef** = `type` _name_ **Needs** **Type** `;`
- **Fndef** = **Needs** `(` **List**\[_name_ `:` **Type**\] `)` (`:` (**Type** | `bind` **List**\[**Need**\]))? (`;` | **Block**)
- **Funcdef** = `fn` _name_ **Fndef**
- **Attachdef** = `fn` `.` _name_ **Fndef**
- **Detachdef** = `fn` _name_ `.` _name_ **Fndef**
- **Valdef** = `val` _name_ **Needs** `:` **Type** `;`
- **Ctxdef** = `context` _name_ **Needs** `=` **List**\[**Need**\] `;`
- **Decl** = **Tydef** | **Aliasdef** | **Tagdef** | **Funcdef** | **Attachdef** | **Detachdef** | **Valdef** | **Ctxdef**
- **File** = (**Import** | **Assume** | **Decl**)\* _end_
