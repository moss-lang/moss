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
- _identifier_: must start with a letter or underscore that can be followed by zero or more letters, underscores, or numbers
- integer literals
  - _uint32_: must end with `u32`
  - _int32_: may begin with `-`, must end with `i32`
  - _uint64_: must end with `u64`
  - _int64_: may begin with `-`, must end with `i64`
  - _uint_: must end with `u`
  - _int_: may begin with `-`, no suffix
- _character_: delimited by single quotes
- _string_: delimited by double quotes, may include escape sequences `\\`, `\n`, `\r`, `\t`

## Grammar

A nonterminal in the grammar is written as a bold capitalized word. A local variable in a parametric declaration is written as a bold lowercase word. The top-level node is a **File**.

- **List**\[**element**\] = TODO
- **File** = (**Import** | **Assume** | **Declaration**)\* _end_
- **Import** = `import` _string_ (`as` _identifier_)? (`use` **List**\[_identifier_ | `.` _identifier_\])? `;`
- **Assume** = `assume` **List**\[**Bind**\] `;`
- **Bind** = TODO
