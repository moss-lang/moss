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
whole compiler, `Std` and all, as one 800KB `.wasm` — and that module
writes byte-for-byte the same output:

```sh
moss build src/main.moss > mossc.wasm
wasmtime --dir . mossc.wasm "" tests/wasi/prim.moss > prim.wasm
```

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
  sources — 23 modules — the two agree on all 1101 rows.
- **Needs.** Every defined function's requirement list, in order — its
  calling convention. For `tests/wasi/full.moss`, which reaches all of
  `Std` provided in Moss over the primitive context, the two agree on
  all 184 of them.
- **Codegen.** [`tests/wasi/prim.moss`](/tests/wasi/prim.moss) and
  [`tests/wasi/raw.moss`](/tests/wasi/raw.moss) are compiled by both
  compilers, and the modules behave identically. The self-hosted one is
  also run twice — once interpreted, once as Wasm — and the bytes match.

## What it does not do yet

The back end covers the *primitive* context and nothing above it:
`Wasm` instructions, `Wasi` imports, `Bool` for `if` to eliminate,
plain functions, `let`/`var`, assignment, `if`/`else`, `while`,
`loop`/`break`, `return`, and `I32`/`I64`. That is the language
`tests/wasi/raw.moss` and `tests/wasi/prim.moss` are written in.

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

- **Contexts at runtime.** Half done. `lower.moss` computes each
  function's requirement list and the environment it sits in, checked
  against the bootstrap; what consumes them does not exist yet — val
  needs as hidden parameters, the call-site map from callee key to
  caller key, and one compiled specialization per environment.
- **Functors** ([D55](../design/semantics.md)) — the construct, not the
  library: `bind WasiStd;` has to inline the functor's binds at the
  application site, which needs the above.
- **Methods** ([D36](../design/semantics.md),
  [D54](../design/semantics.md)): attached lookup by (receiver, name)
  and detached by name with the receiver's home module as fallback. The
  symbol tables in `prog.moss` already hold both keys; nothing consumes
  them.
- **Tags, unions and `match`.** `types.moss` knows the layouts —
  a tag is its payload ([D58]), a union of units is one scalar and any
  wider union is a discriminant beside it ([D59]) — but `codegen.moss`
  still has a three-valued notion of type (nothing, i32, i64) where
  those belong, and emits neither injection nor discrimination. No file
  in `src/` or `lib/` uses a record or a tuple, so those can wait.
- **Monomorphization.** Type binds are static and drive specialization
  ([D2](../design/semantics.md)); `lower.moss` records them and
  `canon` chases them, but the back end never asks.
- **Diagnostics** are a code letter and a name, with no position. The
  machinery for a real message is a string the compiler holds, which is
  [D48](../design/semantics.md).

The order to take them in is the bootstrap's own, and the first step is
taken: contexts and needs, since methods, functors and binds all reduce
to them. What is left is one rewrite of `codegen.moss` — compile a
(function, environment) pair rather than a function, take the val needs
as trailing parameters, translate the caller's keys to the callee's at
each call, dispatch a method on its receiver's head, inline a functor's
binds where it is applied, and give a value more than one scalar.

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

- Interpreting the compiler is about three orders of magnitude slower
  than running it as Wasm — `tests/wasi/prim.moss` takes 2m24s one way
  and 180ms the other — because `src/main.moss` goes through the Moss
  `Std` of `lib/wasistd.moss` (D52), so every `Int` is a boxed tag over
  an interpreted instruction. A driver that assumes `Std` directly gets
  the bootstrap's native one and is far quicker; that is what the tests
  use, and it is also why `moss build src/main.moss` is the way to
  actually run this compiler.
- `prog.moss` reports a syntax error as a per-module flag; the position
  the parser recorded is not surfaced.
- The back end recognizes `Wasm`, `Wasi`, `Bool` and `i32_bool` by the
  file that declares them. So does the bootstrap's back end, which is
  the precedent; it is still a spelling dependency.
