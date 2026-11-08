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
              --(SlotX.v, FoldX.v, CachedClo.v: the engine)--> MossX.run_main's
                result, on `src/main.moss`
              --> S1, the compiler; S1 compiling src/ must give S1 again
```

The [verification status](STATUS.md) describes the proved components and
remaining source soundness and runtime typing obligations for this milestone.

The interpreted self-compile is expensive, so everyday work reuses a compiler
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
  - [`ExecutionModel.v`](theories/ExecutionModel.v): the current bootstrap IR,
    its interpreter and the component elaboration relation. Existing component
    proofs and extraction use this internal representation. Source syntax is
    shared with Audit.v; semantic equivalence remains to be proved.
  - [`AuditRepresentation.v`](theories/AuditRepresentation.v): explicit type
    and value representation relations, and the pending elaboration-refinement
    property. It assumes no model equivalence.
  - [`AuditExamples.v`](theories/AuditExamples.v) and
    [`AuditBuildExample.v`](theories/AuditBuildExample.v): semantic regression
    checks and a complete public build/run derivation.
  - [`Parse.v`](theories/Parse.v): the lexer, the parser, the parse dump
    and the module loader, from a file's bytes to the bundle specified in
    `Audit.v` Part I.
  - [`Sx.v`](theories/Sx.v): the S-expression codec for parsed files and
    bundles.
  - [`CoreInterp.v`](theories/CoreInterp.v): the fueled implementation of
    Core Moss's `eval` in [`CoreFragment.v`](theories/CoreFragment.v), bound
    to [`ComponentContracts.v`](theories/ComponentContracts.v) by `Certified.v`.
  - [`Elab.v`](theories/Elab.v): the elaborator, from a bundle to an
    elaborated program (Core Moss extended with records, loops, memory and
    primitives) or an error message. [`NMap.v`](theories/NMap.v) supports it.
  - [`MossX.v`](theories/MossX.v): the interpreter of elaborated programs
    and the concrete primitives (linear memory, IEEE floats, WASI, the GC
    heap); [`FastX.v`](theories/FastX.v) (map-keyed environments) and
    [`Prune.v`](theories/Prune.v) (read-set pruning) make it fast and are
    proved equal to it; [`FastPath.v`](theories/FastPath.v) applies a
    platform family keeping only the members used, proved equal to
    applying it whole.
  - [`SlotX.v`](theories/SlotX.v), [`FoldX.v`](theories/FoldX.v) and
    [`CachedClo.v`](theories/CachedClo.v): the engine that runs the pruned program
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
    `ComponentContracts.HeapContracts.HEAP_LAWS` states the GC primitives' law,
    [`XHeap.v`](theories/XHeap.v) re-exports it, and
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
  - [`Certified.v`](theories/Certified.v): binds the checked component
    obligations for ExecutionModel. The public refinement remains pending.
- [`ocaml/`](ocaml): the oracle. [`Extract2.v`](ocaml/Extract2.v) is
  the extraction to OCaml (`CachedClo.v` and `LazyClo.v` again on their own, with
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
nix build .#oracle            # audited build; result/bin/moss_m2
nix build .#compiler          # the bootstrap (prints phase timings)
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).bootstrap-rocq
```

The Nix oracle build runs the full proof and assumption audit before
extracting the executable, reusing the same release-profile build tree.
The `bootstrap-rocq` check exposes its audit logs without rebuilding proofs.

By hand, in `nix develop .#bootstrap` (OCaml 4.14 with flambda, and dune,
Rocq 9.1 and its standard library built with it):

```sh
(cd bootstrap && dune build ocaml/moss_m2.exe)   # under a minute
bootstrap/check.sh            # all theories and assumption audits
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
engine's among them), and both pipelines' `fast_engine_agrees` theorems, may assume only the
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
Every engine is proved to compute exactly what ExecutionModel's interpreter
(`XS.run_main`) computes, for every fuel, unless that run gets stuck
(pruning's caveat, `Prune.oracle_agrees`) or, after the fast path, runs
out of fuel (`FastPath.fast_run`):

- `fold`: the program pre-resolved, its calls to small fns
  folded when eligible, and compiled to closures, run by `CachedClo.crun_main`
  (`CachedCloFacts.engine_agrees`). Programs with checked GC casts retain runtime
  type environments, so `FoldX.guard_fold_e` preserves their original
  compiled bodies. Closure compilation and read-set pruning still apply.
  [`CloScalar.v`](theories/CloScalar.v) prepares nonallocating scalar-family
  results once, preserving all provisions, type arguments and fuel checks
  while avoiding repeated interpretation of their primitive bindings.
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
  - [`CachedClo.v`](theories/CachedClo.v) turns each expression into a Gallina
    function, an OCaml closure once extracted, so the dispatch on the term
    happens once (`CachedCloFacts.cc_agrees`). Its environments cache the
    type substitution and type-only rows while preserving the raw environment
    and its lookup order. Extract2.v extracts CachedClo a second time
    alone (`Extraction Library CachedClo`) with extraction's term
    optimizations off (`Set Extraction Flag 0`), so the code of a node's
    children and argument lists stays computed once, outside the closure
    that runs it, rather than being rebuilt at every evaluation.
- `lazy`, the default: [`LazyClo.v`](theories/LazyClo.v) compiles expressions directly
  to closures over [`LazyX.v`](theories/LazyX.v)'s split environments.
  Ordinary bindings, type bindings and the proper-type projection have
  ordered streams. When every type row is proper, the type bindings and
  proper-type projection share one list; malformed rows retain a separate
  projection so ordinary lookup and type lookup keep their exact behavior.
  The type streams themselves are suspended: instantiation, append and
  filtering run only if a type lookup observes them. This avoids allocating
  unused type parameters when ordinary arithmetic helpers call one another.
  Each forced stream still has exactly the reference list and lookup order.
  Ordinary lookup selects the binding kind before scanning the value stream,
  preserving the same first match while avoiding repeated kind comparisons.
  Calls construct the final environment directly, with a single suspended
  concatenation for the common proper-type case.
  Captured environments retain those streams;
  entering a closure does not rebuild its type projection. Type substitution
  is deferred until a cast or allocating primitive observes it, and its
  result is memoized by Rocq's standard coinductive extraction to OCaml
  `Lazy.t`. A suspension captures only the type projection and the type,
  so it does not retain a machine state or ordinary value bindings.
  [`LazyEnvFacts.v`](theories/LazyEnvFacts.v) relates ordinary lookups and
  type observations to the reference environment, including duplicate keys
  and malformed type rows. [`LazyInterpFacts.v`](theories/LazyInterpFacts.v)
  proves exact interpreter agreement at every fuel, and
  [`LazyCloFacts.v`](theories/LazyCloFacts.v) proves closure compilation and
  composes that agreement with pruning and the family fast path.
  [`LazyFold.v`](theories/LazyFold.v) unfolds small direct calls whose
  environment sources are ordinary bindings. It remaps lookups into the
  caller's environment and gathers those entries once, including every
  composition source. Checked casts and type-dependent lookups retain the
  original code. Unsupported operations, failed lookups or evaluation, and
  insufficient fuel fall back with the original state.
  [`LazyFoldFacts.v`](theories/LazyFoldFacts.v) proves exact agreement,
  including runtime types, composition failures and fuel exhaustion.
- `lazy-ref`: the same environments with AST dispatch
  (`LazyInterpFacts.frun_main_eq`).
- `clo`: the same without folding (`CachedCloFacts.crun_main_eq` and
  `SlotX.slot_oracle_agrees`);
- `slot`: `SlotX.rinterp` on the pre-resolved program
  (`SlotX.slot_oracle_agrees`);
- `ref`: ExecutionModel's interpreter itself;
- `fast`: [`FastX.v`](theories/FastX.v)'s, with environments keyed by a
  map (`FastX.frun_main_eq`).

## What is trusted

**[Audit.v](theories/Audit.v)** is the self-contained language model and public build/run
contract. It defines source files, syntax, elaboration, reference execution,
the host interface laws, and both semantic predicates in the same file.
The reference language lives in `Audit.Semantics`; `Audit.Public` exposes
its build/run contract. Source syntax stays in `Input`, `Syntax`, and `Loader`,
shared with the current implementation. Public data types use lowercase names;
`file_path` distinguishes filesystem paths from syntax-level name paths.
Its final module type, **Public.CORRECT**, has exactly five obligations:

- **build_sound**: every successful build satisfies Builds and produces a
  program compatible with the fixed default interface.
- **build_complete**: every source admitted by Builds is accepted.
- **run_sound**: for an admissible program and compatible interface, every
  terminating implementation result satisfies Runs.
- **run_complete**: under those conditions, every terminating Runs
  execution is reproduced.
- **safety**: a built program supplied with a compatible interface excludes
  an internal Wrong result at every finite observation.

Builds includes platform linking: source declarations, primitive signatures,
GC and memory layouts, and generated platform-family bindings must agree
under the same static reading as elaboration. Compatible additionally
requires the interface to supply every installed primitive and satisfy the
primitive and heap laws. Arguments and the read-only file snapshot belong
to the world state, so the default interface is fixed across invocations.

[DefaultRuntime.v](theories/DefaultRuntime.v) proves the executable's
primitive and heap laws for **ExecutionModel**, exposed by Certified.v and
included in the assumption audit. These are component results. A concrete
host adapter and program/execution refinement are still needed for the revised
public model; no implementation of Audit.Public.CORRECT is supplied.
Compatibility already guarantees validity of the empty store typing, so the
public execution theorems need no separate initial-state premise.

These end-to-end obligations are pending. Fuel represents finite
observations of partial execution; fuel exhaustion is distinct from an
explicit Wrong outcome. Traps, exits and unsupported primitives have
separate outcomes.

**[ComponentContracts.v](theories/ComponentContracts.v)** retains the smaller contracts and pending proof
milestones bound by [Certified.v](theories/Certified.v). **[CoreFragment.v](theories/CoreFragment.v)** contains the supporting
core calculus over ExecutionModel. Their existing proofs and the executable extraction roots
remain covered by the assumption audits. These component results do not
constitute an implementation of Public.CORRECT.

Historical review notes and counterexamples are preserved in
[audit-notes.md](../docs/design/audit-notes.md).

- **The family fast path** ([design note 24](../docs/design/audit-notes.md)) is proved, and is
  not the elaborator's: the elaborator applies a family exactly by rule 7
  as drafted. Applying the platform's memory family at every memory
  access would compose all 27 of its members each time, two thirds of the
  bootstrap's time; so the oracle runs `FastPath.fast_program` of the
  elaborated program (before pruning; `MOSS_FASTPATH=0` skips it), which
  gives each application of a platform family whose members are
  nonallocating primitives (`Memories`) and whose results contain no types
  only the members the code after it can look up, when a syntactic check
  shows that nothing binds, copies or renames the family. This also applies
  to programs with runtime type tracking: the simulation preserves their
  complete type streams, including instantiation and type exports from
  other families, so checked GC casts keep the same behavior.
  `FastPath.fast_run`: the result is exactly the elaborated
  program's, at every fuel at which that run does not run out of fuel;
  `OracleCertified.interpreter_correct` binds the default pipeline through
  `LazyOracle.Certified`.

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
may unfold (`tb`) and the guarded folded program (`R'`) are arrays of the same;
and the closure tables `tbl` and `ftbl` are arrays of closures that read
the very arrays they are stored in (the knot `CachedCloFacts.tbl_ok` describes).
Functor entries also contain the scalar result templates computed by the
extracted `CachedClo.prepare_scalar_cache`; `tbl_ok` requires those exact templates.
These are the hypotheses of `CachedCloFacts.engine_agrees`, and no theorem checks
that the arrays meet them: that rests on reading `moss_m2.ml`'s
`MOSS_ENGINE` block, and on testing against the `ref` engine.

For `lazy`, the driver compiles the bodies in the elaborated program
straight into two arrays. `LazyCloFacts.tbl_ok` requires each function entry
and functor entry to be exactly the compiled first body for that symbol,
and compiled closures read the same arrays. The driver fills entries in
reverse order so the first definition wins, and finishes both arrays before
running the program. This table construction remains trusted driver code;
the exact engine theorem and the combined fast-path theorem are included
in the certificate assumption audit alongside the existing engine roots.

## What it does not model

The oracle reports these as unsupported (exit 65) rather than judging
them; the agreement check of the test suite lists each program it affects
(`crates/moss-tests/tests/suite/agreement.rs`):

- more than one linear memory (D79): MossX has one, so a program that
  binds `Memory` and uses memory is unsupported;
- a program with no `main`, or whose `main` assumes more than the
  platform's root context.
