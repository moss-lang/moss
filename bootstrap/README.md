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
              --(theories/Prune.v)--> pruned program
              --(SlotX.v, FoldX.v, CloX.v: the engine)--> MossX.run_main's
                result, on `src/main.moss`
              --> S1, the compiler; S1 compiling src/ must give S1 again
```

It takes about 20 minutes and 3 GB, so everyday work reuses a compiler
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
    proved equal to it; [`FastPath.v`](theories/FastPath.v) applies a
    platform family keeping only the members used, proved equal to
    applying it whole.
  - [`SlotX.v`](theories/SlotX.v), [`FoldX.v`](theories/FoldX.v) and
    [`CloX.v`](theories/CloX.v): the engine that runs the pruned program
    (below), each proved equal to the interpreter before it.
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
    related to their source). [`WorldSound.v`](theories/WorldSound.v)
    proves the first step of soundness: the loader's reading of a bundle
    is a reading (`WorldOf`) in all but its requirements and field
    numbering, and [`ElabFrame.v`](theories/ElabFrame.v) that every step
    of the elaborator keeps that reading. The `Bx*.v` files are those programs'
    bundles as Gallina terms, generated once from parse dumps of an
    earlier tree; they are inputs to those proofs, not to the oracle.
  - [`XTyping.v`](theories/XTyping.v), [`XEnv.v`](theories/XEnv.v) and
    [`XSafety.v`](theories/XSafety.v): a typing judgment for MossX's
    elaborated programs (the core's, extended with machine numbers,
    structs, `var`, loops, `return`, field patterns and primitives typed by
    their signatures, and abstract functors with Rule A's captures and
    pins) and its type safety for `xinterp`: a well-typed program never
    produces `XErr`, at any fuel, GC heap included.
    [`XHeap.v`](theories/XHeap.v) states the GC primitives' law and
    [`XHeapC.v`](theories/XHeapC.v) proves it
    of MossX's GC heap, which is a trie ([`HTrie.v`](theories/HTrie.v))
    rather than a primitive array, so that no array axiom is needed.
    [`XBridge.v`](theories/XBridge.v) begins the bridge from the
    elaboration relation to that typing: Σ, the representations and the
    functor-body table built from a reading of the bundle, under stated
    hypotheses on what the soundness proof is to supply;
    [`MergeWf.v`](theories/MergeWf.v) proves that a formed telescope's
    merge substitution binds only formed types.
  - [`FrontEnd.v`](theories/FrontEnd.v): the lexer and parser against
    `Audit.Syntax.Derives`, the front end's grammar (Audit.v Part I.2):
    the parser is sound for it and the lexer complete;
    [`FrontEndComplete.v`](theories/FrontEndComplete.v): the parser is
    complete for it; [`FrontEndLoader.v`](theories/FrontEndLoader.v): the
    loader against `Audit.Loader.Bundled`. [`FrontEndExamples.v`](theories/FrontEndExamples.v)
    checks the grammar on real files and on syntax errors of `tests/errors`.
  - [`Certified.v`](theories/Certified.v): binds the implementation to the
    obligations `Audit.v` states, as far as they are proved.
- [`ocaml/`](ocaml): the oracle. [`Extract2.v`](ocaml/Extract2.v) is
  the extraction to OCaml (`CloX.v` a second time on its own, with
  extraction's optimizations off; below), and `moss_m2.ml` the driver (it
  reads files and the command line, builds the engine's tables, and
  prints). The extracted code links against Rocq's own implementations of
  its primitive integers, floats and arrays (`Uint63`, `Float64` and
  `Parray`, in the `rocq-runtime.kernel` library).
- [`dune-project`](dune-project): `dune build` here compiles the theories,
  extracts them and builds the oracle, `_build/default/ocaml/moss_m2.exe`.
- [`check.sh`](check.sh): compiles every file and audits the certificate
  ([`assumptions/`](assumptions), below).

## Building and checking

With Nix, everything is a flake output:

```sh
nix build .#oracle            # result/bin/moss_m2
nix build .#compiler          # the bootstrap: about 20 minutes
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

`check.sh` is the audit, since Rocq alone accepts `Admitted`. It audits
the certificate, which transitively covers everything the guarantee
depends on: what `Certified.v` binds, the extracted roots (the default
engine's among them), and `CloX.engine_agrees`, may assume only the
kernel's `Primitive` declarations (Rocq's primitive integers, floats and
arrays; [`assumptions/Certificate.v`](assumptions/Certificate.v)).
The theorems the design notes cite as closed under the global context
must be ([`assumptions/Closed.v`](assumptions/Closed.v)). Nothing says
`Admitted`, and `Parameter`s and `Axiom`s are declared only inside module
types (`Audit.v`'s obligations).

## The engine

`MOSS_ENGINE` chooses what runs the elaborated program, which is first
given the family fast path (`FastPath.fast_program`; `MOSS_FASTPATH=0`
skips it), then pruned (`Prune.oracle_program`; `MOSS_PRUNE=0` skips it).
Every engine is proved to compute exactly what Audit's interpreter
(`XS.run_main`) computes, for every fuel, unless that run gets stuck
(pruning's caveat, `Prune.oracle_agrees`) or, after the fast path, runs
out of fuel (`FastPath.fast_run`):

- `fold`, the default: the program pre-resolved, its calls to small fns
  folded, and compiled to closures, run by `CloX.crun_main`
  (`CloX.engine_agrees`). It bootstraps the compiler about 1.7 times as
  fast as `ref`.
  - [`SlotX.v`](theories/SlotX.v) resolves each lookup in the
    environment to a position before the run, wherever the environment's
    keys are known, as they are at most program points (a fn body starts in
    the composition of its satisfaction map, and each bind conses a known
    key); `SlotX.rinterp` mirrors `xinterp` clause for clause
    (`rinterp_agrees`, `slot_run_main`).
  - [`FoldX.v`](theories/FoldX.v) replaces a direct call to a fn that
    only reads, tags, matches one pattern, applies primitives and calls
    such fns (to depth 6) by one expression over the caller's environment,
    which runs when the fuel suffices and every step succeeds, and
    otherwise falls back to the call (`fold_sound`, `fold_agrees`).
  - [`CloX.v`](theories/CloX.v) turns each expression into a Gallina
    function, an OCaml closure once extracted, so the dispatch on the term
    happens once (`cc_agrees`). Extract2.v extracts CloX a second time
    alone (`Extraction Library CloX`, last) with extraction's term
    optimizations off (`Set Extraction Flag 0`), so the code of a node's
    children and argument lists stays computed once, outside the closure
    that runs it, rather than being rebuilt at every evaluation.
- `clo`: the same without folding (`CloX.clo_oracle_agrees`);
- `slot`: `SlotX.rinterp` on the pre-resolved program
  (`SlotX.slot_oracle_agrees`);
- `ref`: Audit's interpreter itself;
- `fast`: [`FastX.v`](theories/FastX.v)'s, with environments keyed by a
  map (`FastX.frun_main_eq`).

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
  core-fragment output is typed (`elaborated_typed`). Until these are
  proved, what the oracle establishes about the elaborator is that it is
  a total and deterministic Gallina function, plus the lemmas of
  `ElabSound.v`, plus the evidence of testing.
- `FRONT_END`, which `Certified.v` binds: the lexer and parser
  (`Parse.parse_moss`) are sound and complete for `Audit.Syntax.Derives`,
  the grammar of Moss's surface syntax (`Audit.v` Part I.2), file by
  file; and `LOADER`: a bundle the loader loads is `Audit.Loader.Bundled`,
  every module a readable file deriving its AST, each once, closed under
  its imports and the prelude. That the dump reads back as the AST it
  was made from is not proved yet (`FrontEnd.v`, `bundle_todo`).
- **The family fast path** (`Audit.v` DRAFT NOTE 24) is proved, and is
  not the elaborator's: the elaborator applies a family exactly by rule 7
  as drafted. Applying the platform's memory family at every memory
  access would compose all 27 of its members each time, two thirds of the
  bootstrap's time; so the oracle runs `FastPath.fast_program` of the
  elaborated program (before pruning; `MOSS_FASTPATH=0` skips it), which
  gives each application of a platform family whose members are
  primitives (`Memories`) only the members the code after it can look
  up, when a syntactic check shows that nothing binds, copies or renames
  the family. `FastPath.fast_run`: the result is exactly the elaborated
  program's, at every fuel at which that run does not run out of fuel;
  `Certified.v` binds it (`fast_path_agrees`), and composed with
  `CloX.engine_agrees` (`fast_path_engine_agrees`) it covers the default
  pipeline.

Outside Rocq, what is trusted is the OCaml driver (reading files and
arguments, printing, and the engine's tables, below), Rocq's extraction,
the OCaml compiler, and Rocq's runtime implementations of the primitives.
Whether MossX's primitives are the right meaning of `i32.add` or
`fd_write` matters to the comparison with the compiler, not to any
theorem: the test suite checks it.

The driver also realizes some of the theorems' functions as tables, as
it has always realized `program_of`'s fn lookup (`xp_fn`) with a hash
table. For the `fold`, `clo` and `slot` engines, `SlotX.rprog_of`'s
lookup of a compiled body (`nlook` over `SlotX.compile_fns` and
`compile_funs`) is an array indexed by symbol, its first entry for a
symbol winning, as `nlook`'s does; for `fold`, the static bodies FoldX
may unfold (`tb`) and the folded program (`R'`) are arrays of the same;
and the closure tables `tbl` and `ftbl` are arrays of closures that read
the very arrays they are stored in (the knot `CloX.tbl_ok` describes).
These are the hypotheses of `CloX.engine_agrees`, and no theorem checks
that the arrays meet them: that rests on reading `moss_m2.ml`'s
`MOSS_ENGINE` block, and on testing against the `ref` engine.

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
