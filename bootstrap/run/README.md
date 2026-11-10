# run: the reference implementation's executable

`moss-ref` (this directory's `driver.exe`, as the flake installs it) is
Moss's reference implementation. The compiler is bootstrapped with it, and
the test suite holds the compiler to it. It runs one pipeline:

1. **Front end**: `Parse.load` (`theories/Parse.v`) reads, lexes, parses and
   bundles the program and its imports. The driver only reads files.
2. **Elaborator**: `EaMain.elaborate` (`theories/Ea*.v`) builds an
   `Audit.Semantics.program` from the bundle, or rejects it.
3. **Passes**: `AuditFastPath.fastpath`, `AuditPrune.prune` and
   `AuditCapTrim.captrim`, in that order. Each is proved to keep `Runs`
   (`theories/AuditFastPath.v`, `AuditPrune.v`, `AuditCapTrim.v`).
4. **Interpreter**: `Audit.Semantics.Interpreter.run_main`, the reference
   interpreter of Audit.v's run-time semantics. Its primitives are
   `MossX.run_prim`.

## Trust

Audit.v defines the semantics, and steps 3 and 4 are Audit's own interpreter
and passes proved against it. **The elaborator is not proved.** Ea's
`SOUND:` comments state what each part should establish, and nothing proves
it. Neither is the glue: `driver.ml`, and the value and primitive conversions
in `prims.ml`. The `prim` types are shared through `Obj.magic`, because
ExecutionModel.v and Audit.v declare them alike. `check.sh` audits the
extracted roots' assumptions (`assumptions/Certificate.v`).

## Files

- `Extract.v`: the extraction. It covers the roots above, `Parse.parse_moss`,
  `Dump.dump_file` and `print_sx` for `--dump`, and `MossX.run_prim` and
  `init_state`.
- `driver.ml`: the command line. Its header documents the options.
- `prims.ml`: MossX's primitives at Audit's value type.
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
`ulimit -s unlimited`. The self-compile needs about 5 GB.

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
bootstrap/run/self-compile.sh OUTDIR [REFERENCE-S1.wasm]   # hours
```

## Name

The name `moss-ref` is defined in two places: `referenceName` in
`flake.nix` (the package attribute and `bin/moss-ref`), and `REFERENCE` in
`crates/moss-tests/src/lib.rs`, which builds `#moss-ref` and runs
`bin/moss-ref`. The driver's own messages do not name it. The docs that
mention it are this file and `CONTRIBUTING.md`.
