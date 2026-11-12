# run: the reference implementation's executable

`moss-ref` (this directory's `driver.exe`, as the flake installs it) is
Moss's reference implementation. The compiler is bootstrapped with it, and
the test suite holds the compiler to it. It runs one pipeline:

1. **Front end**. `SnapshotLoader.load_from` loads the program and its
   imports through the driver's file reads and returns the parsed source bundle.
   `SnapshotLoader.load_from_sound` proves that a successful load satisfies
   `Audit.Loader.Bundled` for those reads.
   [MossGrammar.vy](../theories/MossGrammar.vy) defines the token grammar.
   Dune runs Menhir in Rocq mode with completeness checking enabled.
   `Audit.Syntax.File` uses the generated parse-tree relation, and the
   reference executable uses the generated parser. `FrontEnd.v` and
   `FrontEndComplete.v` connect the lexer and whole-file parser to that
   specification.
2. **Elaborator**: `EaMain.elaborate` (`theories/Ea*.v`) builds an
   `AuditBase.Semantics.program` from the bundle, or rejects it.
   `BootstrapVerified.elaborate_sound` proves that a successful build
   satisfies Audit’s typing and translation judgments and links against Base.
3. **Passes**: `ProgramFastPath.fastpath`, `ProgramPrune.prune` and
   `ProgramCapTrim.captrim`, in the order extracted from `BootstrapRun.passes`.
4. **Interpreter**: `InterpreterCompiled.crun_program`, the closure-compiled
   implementation of Audit.v's execution semantics. Its primitives are
   `MossX.run_prim_fast` through the Base host adapter.

`BootstrapRun.Sound.run_optimized_sound` composes the passes and interpreter
against public `Runs` for a safe input program, a host whose primitives
ignore program code, and compiled tables satisfying `tbl_ok`.
`BootstrapRun.Native.run_optimized_sound` gives this result for the
driver's state layout, where only primitive-managed state is threaded
through the interpreter. Its extracted program view uses indexed lookups
proved to return the same bodies as Audit.
`BootstrapRun.Native.native_tables_ok` verifies the compiled tables from
the extracted builders. The driver ties their lookup functions together
before the run and uses OCaml arrays for dense table storage.

The public build takes a finite snapshot of canonical paths and UTF-8 files.
A successful CLI load supplies that snapshot when its normalized paths are
canonical and its loaded bytes are UTF-8. The CLI parser can accept a
non-UTF-8 byte in a comment, so the theorem requires UTF-8 for the loaded
bytes. The combined theorem uses the driver's compiled table equations
and proves that a successful load, elaboration, and run satisfy the public
build and execution judgments, that Base is compatible, and that the
execution's outcome cannot be `Wrong`.
[Snapshot bridge](../theories/SnapshotLoader.v) · [Pipeline theorem](../assumptions/Certificate.v)

Source loading uses `LoadWorld.v`. The read-only tag parameter check uses
`LoadListed.v`; `LoadListedFacts.v` proves its complete result, including
diagnostics, equal to the legacy checker. The existing `WorldOf` proof
uses that equality. Legacy elaborator state remains in proof dependencies
and is absent from executable extraction.

## Trust

Audit.v defines the generic language semantics, and AuditBase.v supplies
the concrete Base platform. The token grammar, its semantic actions and
the definitions in Surface.v are part of the source specification.
Rocq checks Menhir's parser certificate during the bootstrap build.
Steps 3 and 4 use the interpreter and passes proved against these definitions.
`BaseHost.primitive_laws` verifies the extracted host's scalar, WASI and
heap operations against the Base contract, and `BaseHost.default_compatible`
supplies the interface laws once the program is ready to link.
`BaseHost.call_in_agrees` proves that the extracted call shares its decoded
operation with the Base adapter and that direct I32 steps implement the
same call as `BaseCall.call`.
The native runner delays the method receiver type until a box or array
allocation needs its header. The compiled primitive adapter agrees with
the ordinary call for every suspended type.
`EaBodyAssembly.elaborate_bodies_sound` verifies body elaboration, and
`BootstrapVerified.build_sound` combines it with the front end and the
world checks. The executable trusts OCaml's recursive table binding and
array representation, host context setup, I/O, and extraction directives.
The decoder memo in `primCache.ml` stores pure results alongside immutable
operation names and operands. `check.py` audits the extracted roots'
assumptions.

The compiler package validates S1 as WebAssembly, runs its optimized form
under Wasmtime to produce S2, and compares the raw S1 and S2 bytes.
The soundness certificate covers the reference pipeline that produces S1.
[Compiler build](../../flake.nix)

## Files

- `Extract.v`: the extraction. It covers the roots above, `Parse.parse_moss`,
  `Dump.dump_file` and `print_sx` for `--dump`, and `MossX.run_prim_fast` and
  `init_state`.
- `driver.ml`: the command line. Its header documents the options.
- `prims.ml`: setup and dispatch for the extracted Base host.
- `primCache.ml`: the native operation carrier and shared decoder memo.
- `dune`: the extraction, and the executable. The executable needs flambda:
  without it the interpreter keeps every version of linear memory alive.
- [`tools/profile_bootstrap.py`](../../tools/profile_bootstrap.py) runs the
  bootstrap step by hand and records wall time and peak RSS.

## Command line

```
moss-ref [--pages N] [--argv0 A] [--prelude P] [--elab-only] [--time]
         [--debug-no-passes] PROGRAM.moss [ARG...]
moss-ref --dump OUTDIR FILE.moss...
```

- The prelude is `P` (`--prelude ""` for none), else `$MOSS_LIB/prelude.moss`,
  else `lib/prelude.moss` in the working directory.
- `--time` writes `[time: ...]` lines to stderr.
- `--debug-no-passes` is a debugging aid. It skips step 3, to compare the
  passes with their absence.
- `--dump` writes each file's AST dump to `OUTDIR`. The self-hosted parser is
  compared with it (`selfhosted.rs`).

stdout is the program's output, and stderr its error output. An abnormal
outcome adds one more line: `[trap NAME]`, `[stuck N]`, `[unsupported
primitive NAME]`, `[out of fuel]`. Rejections are `[front end: MSG]` and
`[elab: MSG]`, with paths relative to the working directory.

| exit | meaning |
|---|---|
| 0, or proc_exit's code | normal |
| 63 | the front end rejected the program (lex, parse or load error) |
| 64 | the elaborator rejected it |
| 65 | a construct not modelled, which is an elaborator message starting `unsupported: `: no `main` (a library), a `main` whose assumptions the reference platform cannot supply |
| 134 / 3 / 4 / 5 | trap / stuck / unsupported primitive / out of fuel |
| 2 | usage |

**WASI contexts.** `WasiCore` declares the calls the reference host
implements. `ReferenceBase` combines it with Wasm, branching, and GC and
is available through the prelude. Use it with `StdBase` for programs
that run on both back ends. `Wasi` and `Base` offer the full Preview 1
declarations to compiled programs. The reference checks each assumption
of `main` during elaboration, including unused assumptions.

**Linear memories (D79).** Base selects the memory from the resolved
`Memory` argument of each `Addr` method. Each distinct type names its own
pages and allocator. Additional memories start at one page with the heap
pointer at address 16 set to 1024. The `--pages` option sets the main
memory's initial size. The WASI operations use main memory.

The interpreter recurses as deeply as the program it runs. Run it with
`ulimit -s unlimited`. The full x86 Linux self-compile at `9af8a75`
took 65 minutes 21 seconds and peaked at 2.7 GiB; its compiler fixpoint
was checked byte for byte.

## Building and running

From `bootstrap/`:

```sh
nix develop ..#bootstrap -c dune build --release run/driver.exe
```

With Nix alone, `nix build .#moss-ref`. That runs `check.py` too.
`nix build .#compiler` bootstraps the compiler with it.

From the repository root:

```sh
ulimit -s unlimited
bootstrap/_build/default/run/driver.exe examples/hello.moss
python3 tools/profile_bootstrap.py OUTDIR [REFERENCE-S1.wasm]
```

## Name

The name `moss-ref` is defined in two places: `referenceName` in
`flake.nix` (the package attribute and `bin/moss-ref`), and `REFERENCE` in
`tools/suite/support.py`, which builds `#moss-ref` and runs
`bin/moss-ref`. The driver's own messages do not name it. The docs that
mention it are this file and `CONTRIBUTING.md`.
