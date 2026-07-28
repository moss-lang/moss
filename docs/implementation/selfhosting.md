# Self-hosting

A Moss compiler written in Moss lives under [`src`](/src). It reads a
source file, loads everything that file imports, resolves every name in
all of it, and writes a WebAssembly module.

```sh
moss run src/main.moss "" tests/wasi/prim.moss > prim.wasm && wasmtime prim.wasm
```

The two arguments are a prelude path and an entry path; an empty prelude
is what a program written against the primitive context wants, since
`Wasm` and `Wasi` are imported by name and there is no standard library
underneath ([D52](../design/semantics.md)).

It compiles under the bootstrap too, into a single WASI module — the
whole compiler, `Std` and all, as one 1.1MB `.wasm` — and that module
writes byte-for-byte the same output, about a thousand times faster:

```sh
moss build src/main.moss > mossc.wasm
wasmtime --dir . mossc.wasm lib/prelude.moss examples/hello.moss > hello.wasm
wasmtime hello.wasm            # Hello, world!
```

Every runnable example compiles that way and matches its golden output.
Those are ordinary Moss over `Std`, and `Std` is Moss too — provided
over the primitive context by one functor ([D52], [D55]) — so the whole
of the context machinery is exercised by getting them right.

## The stages

| file | what it is |
|---|---|
| [`lex.moss`](/src/lex.moss) | a loop-based lexer with a keyword trie |
| [`tcode.moss`](/src/tcode.moss) | token kinds as Ints, generated from the `Token` union |
| [`intern.moss`](/src/intern.moss) | names as codepoint runs, so an id outlives the file it came from |
| [`ast.moss`](/src/ast.moss) | the whole tree, as parallel arenas |
| [`syntax.moss`](/src/syntax.moss) | recursive descent over the whole grammar |
| [`prog.moss`](/src/prog.moss) | the module graph, symbols, and scopes |
| [`types.moss`](/src/types.moss) | elaborated types, hash-consed, and their layouts |
| [`lower.moss`](/src/lower.moss) | requirement environments and each function's needs |
| [`bytes.moss`](/src/bytes.moss) | byte buffers, LEB128, small integers |
| [`insn.moss`](/src/insn.moss) | instruction encoding |
| [`emit.moss`](/src/emit.moss) | the module builder and section layout |
| [`wasmops.moss`](/src/wasmops.moss) | the `Wasm` context as a lookup table |
| [`codegen.moss`](/src/codegen.moss) | instruction selection |
| [`spell.moss`](/src/spell.moss) | the strings the compiler must hold itself |
| [`boot.moss`](/src/boot.moss) | a functor from `Std` to the whole compiler context |
| [`dump.moss`](/src/dump.moss) | the tree, in the format the bootstrap also writes |
| [`cli.moss`](/src/cli.moss), [`main.moss`](/src/main.moss) | the command line, over `Wasi` |

## How it is checked

Not by looking at its output, but by holding it to the bootstrap's.

- **Parsing.** Both parsers write a tree in one compact S-expression
  format — [`bootstrap/mossc/sexpr.py`](/bootstrap/mossc/sexpr.py) and
  [`dump.moss`](/src/dump.moss) — and over the whole live corpus, every
  file in `src/`, `lib/`, `examples/` and `tests/wasi/`, the two agree
  character for character.
- **Collect.** The module graph, the symbol every declaration gets, and
  the scope each module ends up with are compared as a set of
  (module, namespace, name, target) rows. For the compiler's own
  sources — now 34 modules — the two agree on all 2425 rows.
- **Needs.** Every defined function's requirement list, in order — its
  calling convention. Checked on `tests/wasi/full.moss`, which reaches
  all of `Std` provided in Moss over the primitive context (184
  functions), and on the compiler's own 34 modules (721).
- **Codegen.** The `tests/wasi/` programs are compiled by both
  compilers and the modules behave identically; the self-hosted one is
  also run twice — once interpreted, once as Wasm — and the bytes match.
  And every runnable example, compiled by the self-hosted compiler,
  matches the golden output the bootstrap produces for it.

## What it does not do yet

The back end covers the *primitive* context and nothing above it:
`Wasm` instructions, `Wasi` imports, `Bool` for `if` to eliminate,
plain functions, `let`/`var`, assignment, `if`/`else`, `while`,
`loop`/`break`, `return`, `I32`/`I64`, the scalar half of the value
model — nominal tags, units, unions of units, and `match` over them —
contextual vals with `bind`, methods, and the static context — type
binds, fn binds, method binds and functor application, with one compiled
specialization per environment. That is the language
`tests/wasi/{raw,prim,across,tags,ctx,functor}.moss` are written in.

Not `Std` itself: `Std` is Moss already, and
[`lib/wasi.moss`](/lib/wasi.moss)'s `WasiStd` maps
`Wasm, Wasi, Branch -> Std` in one functor with nothing native
underneath. What is missing is compiler support for the constructs that
functor is *written in* — and the self-hosted compiler is in the odd
position of running on `WasiStd` (the bootstrap compiles `bind WasiStd;`
into `src/main.moss`'s module, so the Moss `Std` is what does its
allocation and its I/O) while being unable to compile it.

Four things, and `lib/wasistd.moss` uses all four in its first twenty
lines — `type Str I32;`, `match s { Str a => a }`, `fn Str.length()`,
`bind putchar = wasi_putchar;`. A program that needs any of them is
reported rather than mis-compiled:

- **Contexts at runtime.** The val half is done: a function's val needs
  travel as trailing parameters in the order `lower.moss` fixed, a
  `bind` puts a value in a local for the rest of its block, and a call
  site supplies the callee's needs from its own frame. What is left is
  the *static* half — a fn or type bind changes the code rather than the
  data, so the callee has to be compiled once per environment that
  reaches it, cached on the environment chain.
- **Functors** ([D55](../design/semantics.md)) — the construct, not the
  library: `bind WasiStd;` has to inline the functor's binds at the
  application site, which needs the above.
- **Methods** ([D36](../design/semantics.md),
  [D54](../design/semantics.md)). The attached half works: the
  receiver's type is the other half of the lookup key, forward
  inference means it is known first, and `this` is a parameter of the
  method's frame like any other. What is left is a method reached
  through the *context* rather than by scope — `bind Int.add =
  Num.add;` provides a detached symbol at a receiver, and finding it
  means asking the environment, which is the same specialization
  machinery a fn bind needs.
- **Records and tuples.** Nothing in `src/` or `lib/` uses either, so
  they are the only part of the value model still missing; everything
  else of [D58] and [D59] compiles, including a union wider than one
  scalar. `tests/wasi/noheap.moss` is the standing test for them.
- **Bracket application.** Nothing in `src/` or `lib/` applies a type
  (`Pair[T=Int]`) today, so the back end ignores one where it appears.
  [D61]'s library change will introduce them — an accessor declared once
  and provided as `String.get[Elem=Char]` — and the back end will have to
  interpret the bindings rather than skip them.
- **Diagnostics** are a code letter and a name, with no position. The
  machinery for a real message is a string the compiler holds, which is
  [D48](../design/semantics.md).

All of the bootstrap's own order is taken: requirement lists, real types
and layouts, the calling convention both halves, methods, functors, and
one specialization per environment. What stops the compiler compiling
itself is no longer a stage — it is [D61]'s library change. Its own
source calls `.length` on a `String` and on an `IntList`, and those are
two different symbols with one spelling today, which by [D44] one scope
cannot name. Declaring each accessor once and bracket-applying the
element type per provision fixes that, and then the fixpoint is the next
thing to try.

The Python originals are [`lower.py`](/bootstrap/mossc/lower.py) and
[`build.py`](/bootstrap/mossc/build.py) — but the self-hosted back end
is much less work than their line count suggests, because most of
`build.py` is shims implementing a native `Std`, and there is nothing
there to reimplement: the day the four constructs above compile,
`WasiStd` compiles, and `Std` comes for free.

The milestone that ends this list is the compiler compiling itself. It
is one milestone, not several: `src/main.moss` assumes
`Wasm, Wasi, Branch` and opens with `bind WasiStd;`, so the moment the
back end can compile that line and the library behind it, it can compile
every other file in `src/` too — they are ordinary Moss over `Std`.

## Keeping it out of quadratic time

Every table here is a parallel-array arena, which invites reading it
front to back, and reading it front to back is what the compiler spent
almost all of its time on. Two measurements, on the compiler's own 34
modules: resolving one name per context item against all 2,374 scope
entries came to roughly 250 million row comparisons, and working out
each function's environment 710 times when only 31 `assume` blocks
exist between them multiplied everything by 23.

Both fixes lean on the same fact. **A name is already a dense small
integer** by the time any table sees it — the interner hands out 0, 1,
2, … in first-seen order — so an index over anything keyed by a name,
a symbol or a module needs no hash at all: an array indexed by the id
holding the newest row, and a `link` array parallel to the rows
chaining the rest. `prog.moss`'s scope index measures a mean bucket of
two. `lower.moss` memoizes each `assume` run's environment, since
`declare_decl` hands every declaration in one block the same run and
identical items give an identical answer. `types.moss` remembers `()`
and `|`, indexes nominal types by their symbol, and memoizes
elaboration on the syntax node.

The interner is the one table whose own key is text rather than a
number, so it is the one place that hashes: a base-31 polynomial over
the codepoints into a power-of-two bucket table, doubling when it
passes half full.

What that came to, all under the interpreter, on `tests/wasi/full.moss`:

| phase | before | after |
|---|---|---|
| load and resolve the graph | 26.5s | 14.7s |
| requirement lists | 47.0s | 2.7s |

And the case that mattered: the compiler's own sources went from not
finishing inside ten minutes to 88 seconds — which is also what made it
checkable against the bootstrap at all, and the first thing that check
found was a bug in the comparison rather than in the compiler.

## The idiom

There are no generic containers ([D51](../design/semantics.md)), so
every data structure is a *typed arena*: parallel `IntList`s indexed by
an id, with a `context` bundling them and accessor functions keyed on
the id. [`ast.moss`](/src/ast.moss) is the worked example — copy its
shape rather than inventing another. A node has four operand slots and
spills to a run in `kids` when it needs more; the meaning of each slot
is recorded beside the node kind and nowhere else.

`StrList` exists for the few places a real String must be kept (module
paths). Everything else is an interned id or an arena index.

Because every arena is an abstract val, starting the compiler means
binding about sixty of them. That is one `bind` per line in
[`boot.moss`](/src/boot.moss) — a functor from `Std` to the whole
compiler context, which is exactly what [D55](../design/semantics.md)
exists for.

## What [D48] actually cost

The previous plan flagged string literals as the thing to settle before
writing an emitter: a Wasm module names `wasi_snapshot_preview1` and
`fd_write` in its own bytes, and the compiler has to hold those strings
somehow.

It cost one generated function per name.
[`spell.moss`](/src/spell.moss) has twenty-one of them, each pushing a
name character by character against the `char` constants, exactly as the
lexer's keyword trie recognizes keywords without literals.
[`wasmops.moss`](/src/wasmops.moss) does the same in bulk for the
hundred-odd `Wasm` intrinsic names, which the back end must recognize to
select an instruction — interned once at startup, after which a lookup
is a comparison of ids. Both files are
generated by [`bootstrap/mossc/gensrc.py`](/bootstrap/mossc/gensrc.py)
and checked against it, and neither is pleasant to read, but nothing
about the emitter was blocked. The
pressure that remains is diagnostics: a message with words in it is
still out of reach, which is why an error here is a letter and a name.

## Sharp edges

- Interpreting the compiler is orders of magnitude slower than running
  it as Wasm, because `src/main.moss` reaches `Std` through
  `lib/wasistd.moss` (D52) and the bootstrap interpreter builds a Python
  object per value it handles. Nothing is boxed in the compiled module —
  D58 made a nominal value its payload — but the interpreter does not
  know that. `moss build src/main.moss` is the way to actually run this
  compiler; a driver that assumes `Std` directly gets the bootstrap's
  native one and is what the tests use.
- `prog.moss` reports a syntax error as a per-module flag; the position
  the parser recorded is not surfaced.
- The back end recognizes `Wasm`, `Wasi`, `Bool` and `i32_bool` by the
  file that declares them. So does the bootstrap's back end, which is
  the precedent; it is still a spelling dependency.
