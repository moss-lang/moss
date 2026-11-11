# run: the reference implementation's executable

`moss-ref` (this directory's `driver.exe`, as the flake installs it) is
Moss's reference implementation. The compiler is bootstrapped with it, and
the test suite holds the compiler to it. It runs one pipeline:

1. **Front end**. `SnapshotLoader.load_from` loads the program and its
   imports through the driver's file reads and returns the typed AST bundle.
   `SnapshotLoader.load_from_sound` proves that a successful load satisfies
   `Audit.Loader.Bundled` for those reads.
2. **Elaborator**: `EaMain.elaborate` (`theories/Ea*.v`) builds an
   `Audit.Semantics.program` from the bundle, or rejects it.
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

Source loading uses `LoadWorld.v`. The read-only tag parameter check uses
`LoadListed.v`; `LoadListedFacts.v` proves its complete result, including
diagnostics, equal to the legacy checker. The existing `WorldOf` proof
uses that equality. Legacy elaborator state remains in proof dependencies
and is absent from executable extraction.

## Trust

Audit.v defines the generic language semantics, and AuditBase.v supplies
the concrete Base platform. Both files contain definitions without proofs.
Steps 3 and 4 use the interpreter and passes proved against these definitions.
`BaseHost.primitive_laws` verifies the extracted host's scalar, WASI and
heap operations against the Base contract, and `BaseHost.default_compatible`
supplies the interface laws once the program is ready to link.
**Whole elaborator soundness remains open.**
The `EaTypesSound`, `EaMentionsSound`, `EaDependencyFuel`, `EaContextFuel`
and `EaWorldSound` proofs cover readers, their fuel conditions and the
world builder. Ea's `SOUND:` comments state the intended per-function
contracts. The executable trusts the OCaml driver's table construction,
setup of the fixed host context, I/O and extraction directives. The
decoder memo in `primCache.ml` stores pure results alongside immutable
operation names and operands. `check.sh` audits the extracted roots'
assumptions.

## Files

- `Extract.v`: the extraction. It covers the roots above, `Parse.parse_moss`,
  `Dump.dump_file` and `print_sx` for `--dump`, and `MossX.run_prim_fast` and
  `init_state`.
- `driver.ml`: the command line. Its header documents the options.
- `prims.ml`: setup and dispatch for the extracted Base host.
- `primCache.ml`: the native operation carrier and shared decoder memo.
- `dune`: the extraction, and the executable. The executable needs flambda:
  without it the interpreter keeps every version of linear memory alive.
- `self-compile.sh`: the bootstrap step run by hand, timed with GNU time.

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
| 65 | a construct not modelled, which is an elaborator message starting `unsupported: `: no `main` (a library), a `main` that assumes an item no platform library provides, or more than one memory (D79, below) |
| 134 / 3 / 4 / 5 | trap / stuck / unsupported primitive / out of fuel |
| 2 | usage |

**One memory (D79).** MossX's machine has one linear memory, and a memory
primitive is not told which memory it is a method of. An `Addr` is
unboxed, so a load or store receives only an I32, and `run_prim` does not
receive the method key's site type. The driver therefore refuses a program
that rebinds `Memory`, by a bind or as a functor application's output, with
`unsupported: D79` (exit 65), as the old oracle did. Otherwise its memories
would share one address space, and it could print a wrong answer.

The interpreter recurses as deeply as the program it runs. Run it with
`ulimit -s unlimited`. The full x86 Linux self-compile at `9af8a75`
took 65 minutes 21 seconds and peaked at 2.7 GiB; its compiler fixpoint
was checked byte for byte.

## Building and running

From `bootstrap/`:

```sh
nix develop ..#bootstrap -c dune build --release run/driver.exe
```

With Nix alone, `nix build .#moss-ref`. That runs `check.sh` too.
`nix build .#compiler` bootstraps the compiler with it.

From the repository root:

```sh
ulimit -s unlimited
bootstrap/_build/default/run/driver.exe examples/hello.moss
bootstrap/run/self-compile.sh OUTDIR [REFERENCE-S1.wasm]
```

## Name

The name `moss-ref` is defined in two places: `referenceName` in
`flake.nix` (the package attribute and `bin/moss-ref`), and `REFERENCE` in
`crates/moss-tests/src/lib.rs`, which builds `#moss-ref` and runs
`bin/moss-ref`. The driver's own messages do not name it. The docs that
mention it are this file and `CONTRIBUTING.md`.
