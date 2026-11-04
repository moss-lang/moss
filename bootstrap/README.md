# The bootstrap: a verified front end and interpreter

Moss's compiler is written in Moss (`src/`). This directory is how it
gets built from nothing but Rocq and OCaml: a front end and interpreter
for Moss written in [Rocq](https://rocq-prover.org/), extracted to OCaml,
which runs `src/main.moss` compiling itself. What it writes is the
compiler as a WebAssembly module, byte for byte what the compiled compiler
writes of itself, and the flake checks exactly that
([`flake.nix`](../flake.nix), `packages.compiler`):

```
src/main.moss --(theories/Parse.v: lexer, parser, module loader)--> bundle
              --(theories/Elab.v: elaborator)--> elaborated program
              --(theories/Prune.v)--> MossX.run_main, on `src/main.moss`
              --> S1, the compiler; S1 compiling src/ must give S1 again
```

It takes about 35 minutes and 3 GB, so everyday work reuses a compiler
built before ([`CONTRIBUTING.md`](../CONTRIBUTING.md)). The same program,
`moss_m2`, is the *oracle* of the test suite
([`crates/moss-tests`](../crates/moss-tests)): it runs test programs as a
second implementation of the language, and its front end has to accept
and reject the same programs as the compiler's.

Until this replaced it, the bootstrap was a compiler written in Python
(`bootstrap/mossc/`, removed; see D86 in
[`docs/design/semantics.md`](../docs/design/semantics.md)). The Rocq
parser was checked against the Python one before the switch: identical
parse dumps for all 282 files of the corpus and for 30000 mutants of them,
errors and positions included.

## Layout

- [`theories/`](theories): the Rocq development, the dune theory `NM`.
  - [`Audit.v`](theories/Audit.v): what a reader has to trust, and nothing
    else (below). It imports only Rocq's standard library.
  - [`Parse.v`](theories/Parse.v): the lexer, the parser, the parse dump
    and the module loader, from a file's bytes to the bundle `Audit.v`'s
    Part I reads.
  - [`Elab.v`](theories/Elab.v): the elaborator, from a bundle to an
    elaborated program (Core Moss extended with records, loops, memory and
    primitives) or an error message. [`Sx.v`](theories/Sx.v) and
    [`NMap.v`](theories/NMap.v) support it.
  - [`MossX.v`](theories/MossX.v): the interpreter of elaborated programs
    and the concrete primitives (linear memory, IEEE floats, WASI, the GC
    heap); [`FastX.v`](theories/FastX.v) (map-keyed environments) and
    [`Prune.v`](theories/Prune.v) (read-set pruning) make it fast and are
    proved equal to it.
  - [`CoreMoss.v`](theories/CoreMoss.v): the core calculus, its typing
    and dynamics, and its metatheory (type soundness included);
    [`Narrowing.v`](theories/Narrowing.v) (D71),
    [`KeyOnly.v`](theories/KeyOnly.v) (why coherence is keyed by θ),
    [`Specialization.v`](theories/Specialization.v) (D16) and
    [`D84.v`](theories/D84.v) (D84's rules against the calculus) build on
    it. Their comments cite the sections of the Core Moss paper,
    `core-moss.tex`, which is no longer kept; it is in the history, at
    `docs/design/core/core-moss.tex` as of 5ea4e2e.
  - [`ElabSound.v`](theories/ElabSound.v), [`RelFacts.v`](theories/RelFacts.v),
    [`RelSupport.v`](theories/RelSupport.v),
    [`RelExamples.v`](theories/RelExamples.v),
    [`RelNegative.v`](theories/RelNegative.v),
    [`RelFamily.v`](theories/RelFamily.v): what is proved about the
    elaborator, and evidence that the draft elaboration relation says what
    is intended (the elaborator's output for a few test programs is
    related to their source). The `Bx*.v` files are those programs'
    bundles as Gallina terms, generated once from parse dumps of an
    earlier tree; they are inputs to those proofs, not to the oracle.
  - [`Certified.v`](theories/Certified.v): binds the implementation to the
    obligations `Audit.v` states, as far as they are proved.
- [`ocaml/`](ocaml): the oracle. [`Extract2.v`](ocaml/Extract2.v) is
  the extraction to OCaml, and `moss_m2.ml` the driver (it reads files and
  the command line, and prints). The extracted code links against Rocq's
  own implementations of its primitive integers, floats and arrays
  (`Uint63`, `Float64` and `Parray`, in the `rocq-runtime.kernel`
  library).
- [`dune-project`](dune-project): `dune build` here compiles the theories,
  extracts them and builds the oracle, `_build/default/ocaml/moss_m2.exe`.
- [`check.sh`](check.sh): compiles every file and audits every theorem.

## Building and checking

With Nix, everything is a flake output:

```sh
nix build .#oracle            # result/bin/moss_m2
nix build .#compiler          # the bootstrap: about 35 minutes
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).bootstrap-rocq
```

By hand, in `nix develop .#bootstrap` (OCaml 4.14 with flambda, and dune,
Rocq 9.1 and its standard library built with it):

```sh
(cd bootstrap && dune build ocaml/moss_m2.exe)   # under a minute
bootstrap/check.sh            # about 5 minutes, 6 GB
m2=bootstrap/_build/default/ocaml/moss_m2.exe
$m2 examples/hello.moss                       # run a program
$m2 --elab-only tests/wasi/tags.moss          # only check it
ulimit -s unlimited && $m2 --pages 2 \
  --argv0 lib/prelude.moss src/main.moss src/main.moss > s1.wasm   # the bootstrap
```

Run it from the repository root: it reads the prelude as
`lib/prelude.moss` (or `$MOSS_LIB/prelude.moss`, or `--prelude`). Its
exit status is 63 if the front end rejects the program (a lex, parse or
load error), 64 if the elaborator does, 65 for a construct it does not
model (below), 134 for a trap, and otherwise the program's own.

The OCaml must be flambda: only flambda honours the `[@inlined]` in
`Extract2.v`'s `nat_case`, and without it the interpreter keeps every
active frame's incoming machine state alive, so the whole version chain of
linear memory stays reachable (17 GB on the self-compile instead of 3).
The build fails on any other OCaml, and compiles the oracle with `-O3
-inline 200 -unsafe` ([`ocaml/dune`](ocaml/dune)).

`check.sh` is the audit, since `coqc` alone accepts `Admitted`: every
top-level `Theorem` and `Corollary` has a `Print Assumptions` line, and
the audited names are exactly those; the calculus files' theorems must be
closed under the global context; the others may assume only the kernel's
`Primitive` declarations (Rocq's primitive integers, floats and arrays);
nothing says `Admitted`; and `Audit.v` declares `Parameter`s and `Axiom`s
only inside module types.

## What is trusted

**`Audit.v`** is the contract, and is meant to be read in full. It states
the input (Part I: the bundle and the surface syntax tree), elaborated
programs and their meaning (Parts II–IV: Core Moss and MossX, with the
primitives abstract), the declarative elaboration relation (Part V, a
DRAFT for the designer's review, with numbered DRAFT NOTES on each
decision), and the obligations, as module types (Part VI):

- `PROVED_NOW`, which `Certified.v` binds: the elaborator is a total
  function; Core Moss's evaluation is deterministic and its interpreter
  adequate; specialization preserves evaluation; MossX is conservative
  over Core Moss; the primitives obey their law; the core type system is
  sound; and method resolution is search-free (D1).
- **Pending**, the rest of `COMPLETE`: the elaborator is sound and
  complete for the relation (`elaborate_sound`, `elaborate_complete`),
  accepted programs never get stuck (`accepted_never_err`), and the
  core-fragment output is typed (`elaborated_typed`); and `FRONT_END`,
  the parser's soundness and completeness for a grammar that is not yet
  written as a relation. Until these are proved, what the oracle
  establishes about the front end and the elaborator is that they are
  total and deterministic Gallina functions, plus the lemmas of
  `Parse.v` and `ElabSound.v`, plus the evidence of testing.
- **The `Memories` fast path** (`Audit.v` DRAFT NOTE 24). Applying the
  platform's memory family at every memory access would compose all 27 of
  its members each time, two thirds of the bootstrap's time; the
  elaborator instead gives each access only the members it reads
  (`Elab.memories_branch`), unless a program binds `Memories` itself, in
  which case it elaborates again without the fast path, so what is
  accepted does not change. That the two elaborations behave alike is a
  stated, **unproved** lemma (in note 24), needed for the elaborator's
  soundness against the relation.

Outside Rocq, what is trusted is the OCaml driver (reading files and
arguments, printing), Rocq's extraction, the OCaml compiler, and Rocq's
runtime implementations of the primitives. Whether MossX's primitives are
the right meaning of `i32.add` or `fd_write` matters to the
comparison with the compiler, not to any theorem: the test suite checks it.

## What it does not model

The oracle reports these as unsupported (exit 65) rather than judging
them; the agreement check of the test suite lists each program it affects
(`crates/moss-tests/tests/suite/agreement.rs`):

- more than one linear memory (D79): MossX has one, so a program that
  binds `Memory` and uses memory is unsupported;
- a `Ref` or `Array` rebuilt from a bare handle outside the library (D75,
  D81), which is a checked cast natively;
- a program with no `main`, or whose `main` assumes more than the
  platform's root context.
