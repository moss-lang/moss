# Scoping

Moss distinguishes between _lexical_ scoping (determining which declaration a particular symbol refers to) and _semantic_ scoping (determining which definition to use for a particular usage of a declaration). This document describes the former, which can be mostly resolved using only syntactic information before typechecking, although there is one exception.

Like in most languages, at each point in the program, there is a stack of scopes. Two scopes are always present at the bottom of the stack:

- The _prelude_ scope, which has the lowest precedence.
- The _module_ scope, which has the second-lowest precedence.

Within a given scope in the stack, every symbol must refer to exactly one of the following:

- nothing
- a module
- a `type` declaration
- a `fn` declaration
- a `val` declaration
- a `context` declaration
- a local variable

There are two "namespaces" of symbols: "normal" symbols and "detached method" symbols. The latter are prefixed by a `.` token. Unless otherwise noted, we use the word "symbol" to refer to normal symbols.

If a **Path** resolves to a module, then appending "`::` _symbol_" to that **Path** refers to whatever is mapped from that symbol in the module scope of that module.

## Prelude scope

The prelude scope contains all symbols defined in [`prelude.moss`](/lib/prelude.moss) in the standard library.

## Module scope

The module scope includes all symbols from the following kinds of top-level items:

- `import`
- `type`
- `fn`
- `val`
- `context`

Other than `import`, each of these puts exactly one symbol into the module scope. In the case of `fn`, that may be a detached method symbol.

### `import`

If "`as` _symbol_" is present, that symbol maps to the module being imported.

If "`use` **List**\[`.`? _symbol_\]", each (possibly detached-method) symbol maps to whatever that symbol maps to in the module scope of that the module being imported.

## Deeper scopes

A `fn` declaration is the only kind of top-level item that can define additional scopes that take precedence over the prelude scope and module scope. To start, the function parameter list "`(` **List**\[_symbol_ `:` **Type**\] `)`" creates a scope in which each symbol maps to a local variable.

Within a **Block**, a `let` or `var` statement always define a new scope with exactly one symbol mapping to the newly declared local variable; that scope extends to the end of the **Block**.

## Record fields

In a record construction expression "`{` **List**\[_symbol_ (`=` **Expr**)?\] `}`", whenever the "`=` **Expr**" is omitted for a field, it is treated as if the `=` were present and the right-hand side of it were the same as the left-hand symbol.

Other than that special-case for field name punning, record field names are treated as raw symbols and are not subject to normal scoping rules.

## Attached methods

The one exception to the standard lexical scoping system is methods which are attached directly to a nominal type, sometimes referred to as a "tag". In an **Attachdef** like "`fn` _symbol_ `.` _symbol_ **Fndef**", the symbol to the left of the `.` must refer to a **Tagdef** in the same module, not coming from an `import`.

For a method call "**Expr** `.` _symbol_ `(` **List**\[**Expr**\] `)`", resolving the referent of "`.` symbol" depends on the result of typechecking the **Expr** on the left. If typechecking says that the type of the expression is a nominal type, and that nominal type has an attached method named by the given symbol, then this method call refers to that attached method. Otherwise, it refers to whatever detached method is mapped from "`.` _symbol_" according to normal scoping rules.
