# Self-hosting

A Moss compiler written in Moss lives under [`src`](/src). It reads a
source file, loads everything that file imports, resolves every name in
all of it, and writes a WebAssembly module — **including its own source**:

```sh
python3 -m mossc src/main.moss > S0.wasm                            # B(S)
wasmtime --argv0 lib/prelude.moss --dir . S0.wasm src/main.moss > S1.wasm
wasmtime --argv0 lib/prelude.moss --dir . S1.wasm src/main.moss > S2.wasm
cmp S1.wasm S2.wasm                                                 # equal
```

`S1 == S2` byte for byte is the fixpoint: whatever the bootstrap did
differently is gone by the second generation, and the compiler reproduces
its own input exactly. `S0 != S1` is expected — two different compilers
emit different code for one source — and is asserted too, so that the
comparison cannot degenerate into comparing something with itself.
`TestSelfHostedFixpoint` in [`tests/test_build.py`](/bootstrap/tests/test_build.py)
is the standing check.

It runs under the bootstrap's interpreter too:

```sh
wasmtime --argv0 "" --dir . S1.wasm tests/wasi/prim.moss > prim.wasm
wasmtime prim.wasm
```

The prelude path is `argv[0]` and the sole argument is the entry path; an empty
prelude is what a program written against the primitive context wants, since
`Wasm` and `Wasi` are imported by name and there is no standard library
underneath ([D52](../design/semantics.md)).

Compiled by the bootstrap it is a single WASI module — the whole
compiler, `Std` and all, as one 1.2MB `.wasm` — which writes
byte-for-byte the same output, about a thousand times faster:

```sh
python3 -m mossc src/main.moss > mossc.wasm
wasmtime --argv0 lib/prelude.moss --dir . mossc.wasm examples/hello.moss > hello.wasm
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
  sources — now 35 modules — the two agree on all 3061 rows.
- **Needs.** Every defined function's requirement list, in order — its
  calling convention. Checked on `tests/wasi/full.moss`, which reaches
  all of `Std` provided in Moss over the primitive context (185
  functions), and on the compiler's own 35 modules (780).
- **Codegen.** The `tests/wasi/` programs are compiled by both
  compilers and the modules behave identically; the self-hosted one is
  also run twice — once interpreted, once as Wasm — and the bytes match.
  And every runnable example, compiled by the self-hosted compiler,
  matches the golden output the bootstrap produces for it.
- **Itself.** Two generations of self-compilation agree byte for byte,
  which is the only check with nothing outside it to appeal to: the
  compiler is held to its own output on its own source.

## What it does not do yet

The back end covers everything the compiler and the library are written
in: `Wasm` instructions, `Wasi` imports, `Bool` for `if` to eliminate,
plain functions, `let`/`var`, assignment, `if`/`else`, `while`,
`loop`/`break`, `return`, `I32`/`I64`, the scalar half of the value
model — nominal tags, units, unions of units, and `match` over them —
contextual vals with `bind`, methods, and the static context: type binds,
fn binds, method binds and functor application, with one compiled
specialization per environment.

That includes `Std` itself. `Std` is Moss already, and
[`lib/wasi.moss`](/lib/wasi.moss)'s `WasiStd` maps
`Wasm, Wasi, Branch -> Std` in one functor with nothing native
underneath; the compiler both *runs on* it and *compiles* it, which is
what makes `src/main.moss` — `assume Wasm, Wasi, Branch` and one
`bind WasiStd;` — a program it can read.

What is left, none of it on the path to self-hosting:

- **Records and tuples.** Nothing in `src/` or `lib/` uses either, so
  they are the only part of the value model still missing; everything
  else of [D58] and [D59] compiles, including a union wider than one
  scalar. `tests/wasi/noheap.moss` is the standing test for them, and a
  program that needs them is reported — `e_shape`, raised where the type
  is elaborated — rather than mis-compiled.
- **Applying a type.** [D61] made the library apply one *per provision*
  (`String.get[Elem=Char]`), and for those the back end does nothing:
  the receiver is written concretely, a provision is keyed by (receiver,
  method), and every signature it needs comes from the *provider*, so
  the substitution changes no code here. The bootstrap needs it — it
  checks the provider's signature against the abstract one — and the
  environment chain does not. What is not done is an application on a
  **type** (`Pair[T=Int]`), which substitutes into a payload; that is
  reported as `e_apply` rather than elaborating `Pair` and dropping the
  bindings, which is what it used to do silently.
  `tests/wasi/applied.moss` is the standing test: the bootstrap compiles
  and runs it, this compiler says no.
  What *is* done, since it is the other half of the same mechanism, is a
  receiver an application binds: a context item may key a detached method
  on an **abstract** receiver (`context IsList = L.get[Elem=T];`) which
  an application then fixes (`IsList[T=Int, L=IntArr]`). That is the
  generic container of [D51], and unlike the concrete case it does need
  the brackets read — a provision keyed at `L` is one no call finds.
  `lower.moss`'s `app_view` extends the environment an item's types are
  read through with whatever its own brackets bind, for the length of
  that item and no further, which is what lets one scope hold the same
  interface at two element types. `tests/wasi/generic.moss` is the
  standing test, held to the same output under both compilers.
- **Diagnostics** are a code letter, the frame they came from, and a
  name — no line or column. The machinery for a real message is a string
  the compiler holds, which is [D48](../design/semantics.md).
- **Speed.** One generation is 0.9s optimized, 11.5s raw. Where it goes,
  measured by truncating the compiler after each phase and timing what is
  left:

  | phase | |
  |---|---|
  | lex, parse, declare, link, and every requirement list | 0.02s |
  | the scan pass | 0.41s |
  | `declare_all` and the emit pass | 0.46s |
  | writing 1.2MB to stdout | 0.00s |

  The front end is not the problem and never was. Writing the module used
  to be 0.46s of this — a third of a generation — because `putchar` was
  the only output `Std` had and it is one `fd_write` per byte, 1,216,466
  syscalls for one module. [`put_bytes`](/lib/std.moss) is the bulk
  primitive that replaced it: 593 `write`s for the same module, and those
  593 are the host's own 4096-byte stdout chunking rather than anything
  Moss asks for. Written where it was measured, the phase is now free.

  The two body passes are what is left, and they are ~50/50 by
  construction: a Wasm function index counts the imports first, so
  indices cannot be handed out until every import is known, and the scan
  pass exists to find that out. Emitting once into a buffer with
  placeholders for call indices and patching them afterwards would
  recover the 0.41s — at the cost of the invariant below, which has
  already gone wrong twice.

  What is left after that is the constant factor of `Std` written in
  Moss, and it is real: there are no literals ([D4]), so `four()` is a
  function call, `eight()` is three of them, and `Ints.get` reaches an
  element through `ints_addr`, `elem`, `raw` and two of those constants —
  half a dozen Wasm calls for one array read, on the hottest path a
  compiler of parallel arenas has. Inlining those by hand inside the
  accessors is what would close it.

  `wasm-opt -O3` does that mechanically in the meantime: 0.8s of
  optimizer takes a generation from 11.5s to 0.9s and shrinks the module
  from 1.2MB to 200KB. Nothing this back end emits is *wrong*, just
  unoptimized — it emits straight-line code and leaves every call a call.
  The tests run each generation through it, but they always compare the
  **raw** output of a generation: raw equality implies optimized equality
  and not the other way round, so comparing optimized modules could hide
  a difference the optimizer happens to erase. That the optimizer
  preserves what a compiler *does* is itself asserted, by compiling one
  example with both.

Two invariants worth not breaking, both of them things that have gone
wrong. The scan pass and the emit pass in `codegen.moss` must hand out
local *indices* in the same order, so the reveal cursor only ever moves
forward and leaving a scope retires the names it introduced rather than
rewinding the cursor — rewinding hands one index out twice in the pass
that rewinds and once in the pass that appends, and every local after the
first nested block belongs to the wrong name. Any place that scans a
binder must also mirror exactly what emitting binds: `scan_arm_binder`
and `bind_pattern` are one decision written twice. And a bind statement
is walked by all three of discovery, scanning and emission, which is why
there is one traversal with a mode rather than three to keep in step.

A third, cheaper to state: a provision's local carries **no** name. A
contextual val arrives as a trailing parameter and is found by its
symbol, so naming that parameter would let `val n: Char;` shadow a
parameter called `n` — which it did.

The Python originals are [`lower.py`](/bootstrap/mossc/lower.py) and
[`build.py`](/bootstrap/mossc/build.py) — and the self-hosted back end
turned out to be much less work than their line count suggests, because
most of `build.py` is shims implementing a native `Std` and there was
nothing there to reimplement: the day the constructs `WasiStd` is written
in compiled, `Std` came for free.

Self-hosting was one milestone, not several, for the same reason.
`src/main.moss` assumes `Wasm, Wasi, Branch` and opens with
`bind WasiStd;`, so the moment the back end could compile that line and
the library behind it, it could compile every other file in `src/` too —
they are ordinary Moss over `Std`.

## What the last mile actually was

Worth recording, because none of it was a missing stage.

[D61]'s library change came first: `.get`, `.length`, `.push` and `.read`
declared once in [`lib/access.moss`](/lib/access.moss) with the element
type as a requirement, supplied per provision with a bracket application
(`String.get[Elem=Char]`, `IntList.get[Elem=Int]`). Before that, each
spelling was several distinct symbols and [D44] would not let one scope
name two of them — and `src/` calls `.length` on a `String` and on an
`IntList` in the same file. With one symbol each, `lib/prelude.moss` can
import all twenty-nine detached names the library has, which is what a
call site needs under D61's strict rule. Two bootstrap changes fell out:
a bracket application on a *bind*'s left-hand side (a shared accessor is
provided at one receiver at a time, and the wanted signature has to be
read under that substitution), and reading both sides of `[Elem=Char]` in
the module the item is *written* in rather than the one assuming it.

Then removing the bootstrap's `synth_method` fallback, which had been
matching a provision by the spelling of its method's declaration name.
That is what had been hiding the library problem. It broke nothing.

What the compiler then reported about its own source was 243 diagnostics
with two causes between them, both in `codegen.moss` and neither about
[D61]:

- The reveal cursor rewound when a block ended, so `let sep` after a
  `while` that declared locals got an index the scan had given to
  something else. Every name after the first nested block in a function
  resolved to the wrong local or to nothing — which is most functions in
  this compiler.
- A contextual val's parameter carried the val's name, and `Chars`
  provides `val n: Char;`, so a parameter called `n` (or `p`, or `j`)
  was shadowed by a `Char` and every method call on it looked for
  `Char.add`.

The lesson is the one the diagnostics almost hid: a wrong *type* for a
receiver is reported as a missing *method*, several frames from the
mistake. Recording which function a diagnostic came from is what made
243 letters into two bugs; before that they were unreadable.

## Keeping it out of quadratic time

Every table here is a parallel-array arena, which invites reading it
front to back, and reading it front to back is what the compiler spent
almost all of its time on. Two measurements, on the compiler's own
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

That round fixed the front end and left the back end, whose cost was
then written off as the constant factor of `Std` in Moss. It was not: as
Wasm, three more tables had the same problem, and between them they were
79% of a generation. The profile said so plainly — `val_slots` 27%,
`env_find` 24%, `need_key_recv` 17%, `env_push` 7%.

- `codegen.moss` **memoizes `val_slots`** on (symbol, environment).
  Every call site asks what its callee takes as parameters, and the
  answer is a pure function of that key: 21,684 walks over 68 million
  requirements, for 755 distinct keys. It also drove almost every
  `env_find` and `need_key_recv` call in the compiler, which is why three
  profile entries fall to one fix.
- `lower.moss` **indexes `env_push`**, which was consing environment
  entries by scanning every row ever created — the one table the earlier
  round missed, three functions above the memo it added.
- `types.moss` **memoizes `width`**, asked of a local's type on every
  load and every store, recomputed from the members each time.

| | optimized | raw |
|---|---|---|
| before | 6.5s | 143s |
| after | 1.4s | 12s |

(0.9s optimized once `put_bytes` took the output phase out too — that one
is in **Speed**, since a syscall per byte is not a scan.)

None of it needed a generic container or a hash: every key is a dense
small integer, so all three are an array and a `link`. The output is
byte-identical — the same module for `src/`, and the same module for all
32 programs in `examples/` and `tests/wasi/` — which is the only check
worth having for a change that is supposed to compute the same answer
faster.

The lesson the first round half-learned and this one finished: **a
lookup is not a scan.** `asm_find` is still a linear scan over the
`assume` memo, harmless at 31 rows and the fourth copy of an index
nobody has factored out; if it ever holds a row per declaration it will
be the next entry in this section.

## The idiom

Every data structure here is a *typed arena*: parallel `IntList`s
indexed by an id, with a `context` bundling them and accessor functions
keyed on the id. [`ast.moss`](/src/ast.moss) is the worked example —
copy its shape rather than inventing another. A node has four operand
slots and spills to a run in `kids` when it needs more; the meaning of
each slot is recorded beside the node kind and nowhere else.

Not because the language cannot express a generic container:
[D51](../design/semantics.md) is a *corrected* finding, and generic
containers work — a detached accessor keyed on an abstract receiver,
instantiated at a unique receiver type per element type, which is what
[`access.moss`](/lib/access.moss) already is. What one costs is a
nominal receiver type and one attached method per operation, because
only an attached method can see its receiver ([D54]) and an attached
method needs a nominal one (Q5/[D36]) — so the *body* is per
instantiation even when the interface is not. An arena keyed by a dense
id needs neither, since everything it holds is an `Int` already.

The one thing an arena must not skip is its **index**. A lookup keyed by
a name, a symbol, a module, a type or an environment is keyed by a dense
small integer, so it costs an array indexed by that id and a `link`
array chaining collisions — and a table without one is a linear scan
that grows with the program. That is not a style preference: three of
them were the whole of this compiler's running time. See **Speed**.

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
still out of reach, which is why an error here is a letter, a frame and a
name — and why finding the two bugs that stood between it and its own
source meant reading 243 of those.

## Sharp edges

- Interpreting the compiler is about three orders of magnitude slower
  than running it as Wasm, because the bootstrap interpreter is a Python
  tree-walker over the core IR and builds an object per value it handles.
  Nothing is boxed in the compiled module — D58 made a nominal value its
  payload — but the interpreter does not know that. So the tests compile
  each driver to a WASI module with the bootstrap and run *that*: the
  four self-hosted stage tests took 534 seconds interpreted and take 8.5
  compiled. `moss build src/main.moss` is likewise the way to actually run
  this compiler rather than `moss run`.
- The suite is about forty-five seconds. The fixpoint test's two
  generations are under two seconds each — 0.9 of Wasm and 0.8 of
  `wasm-opt` — and no longer the bulk of it. Without the optimizer the
  same two generations are twenty-three seconds. See **Speed** above.
- `prog.moss` reports a syntax error as a per-module flag; the position
  the parser recorded is not surfaced.
- The two diagnostic code spaces are `prog.moss`'s `e_` constants (A–N)
  and `codegen.moss`'s `c_` constants (O–T), which continue where the
  first stop *because they share one list* — the back end reports both
  kinds, and two codes that print as the same letter cannot be told
  apart. They could once. Adding an `e_` means shifting every `c_`.
- A resolution error can be raised *during* codegen — elaborating a type
  and flattening a context are the back end's work — so `cli.moss` checks
  both tables after compiling and not just before. It once checked
  `prog`'s only before, and a module could be written while that table
  held errors nobody printed.
- The back end recognizes `Wasm`, `Wasi`, `Bool` and `i32_bool` by the
  file that declares them. So does the bootstrap's back end, which is
  the precedent; it is still a spelling dependency.
