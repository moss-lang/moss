# Contributing to Moss

Run compiler and test commands in the default Nix dev shell from the root of
the checkout. Use `nix develop .#bootstrap` for Rocq, Dune, and VsRocq.

```sh
nix develop
```

## How the compiler is built

The compiler is written in Moss, under [`src`](src), and compiles itself.
Building it from nothing is the job of [`bootstrap`](bootstrap). There,
`moss-ref`, the *reference implementation* (the suite calls it the
*oracle*), runs `src/main.moss` compiling itself. `moss-ref` is written in
Rocq and extracted to OCaml
([`bootstrap/run`](bootstrap/run/README.md)), and it has four parts: the
front end, the executable elaborator (`Ea*.v`), the passes proved to keep
Audit.v's `Runs`, and Audit.v's reference interpreter. That self-compile
is the flake's `compiler` package. It is an expensive interpreted run, and
the build prints phase timings to stderr:

```sh
nix build .#compiler -o result-compiler   # the bootstrap
nix build .#moss-ref -o result-moss-ref   # audited moss-ref without self-compilation
```

The interpreter and the passes are Audit's own, proved against it. The
elaborator and the driver's glue are not proved
([`bootstrap/run/README.md`](bootstrap/run/README.md)).

You do not do that for everyday work. Any compiler built before, from an
older commit too, can build the current one, as long as it compiles the
current source correctly: what it writes is then compiled once more by
itself. So:

- **Everyday work reuses a previously built compiler**, the *seed*. The
  The development command and the test suite look for one in `MOSS_SEED`
  (a `mossc.wasm`) or `result-compiler`. The development command can also
  use the compiler it last built in `target/`. Keep a seed from the
  bootstrap above or copy one from a machine that has built it.
- **Rebuild through the bootstrap only when needed**: when no compiler you
  have can compile the current source (a change the old compiler does not
  understand, such as new syntax the compiler's own source uses), or when
  you change `bootstrap/` itself. When the source uses a feature its own
  previous version lacks, land the feature first and its use after.
- **CI runs the full `nix flake check`** on every platform, bootstrap
  included, so every merged commit is known to bootstrap from Rocq alone.

Keep the bootstrap's dependency graph limited to the compiler and the
libraries it needs. Future tools written in Moss, such as an LSP server,
should reuse shared compiler services through separate entry points; the
compiler's bootstrap entry point must not import tool-specific code.
User-facing integration must preserve this boundary. Check the actual
module/import closure: removing unused code during WebAssembly generation
does not spare the Rocq front end from loading and elaborating it first.
Keep compiler build-input filesets equally narrow, so edits to tool-only
files do not invalidate an otherwise unchanged compiler bootstrap.

Run a program with the current checkout from the default dev shell:

```sh
python3 tools/build.py dev examples/hello.moss
```

The development command caches the C++ launcher and the Moss compiler
separately. It rebuilds the compiler from a seed when the source changes.
Executable examples use the installed `moss` command on `PATH`.

Release bundles contain `bin/moss` (or `bin/moss.exe`),
`libexec/moss/compiler.cwasm`, and `share/moss/lib`. Keep these together
when moving a bundle.

## Tests

Run the Python suite with a seed compiler:

```sh
python3 tools/test.py
python3 tools/test.py language.context
python3 tools/test.py --list
```

The suite checks the compiler fixpoint, the programs in `tests/` and
`examples/`, and the native launcher. It builds the current checkout's
`moss-ref` once if needed. Set `MOSS_ORACLE` to use a specific executable,
such as `bootstrap/_build/default/run/driver.exe`. CI supplies the oracle and
both raw compiler generations from the bootstrap, so the suite can reuse
the verified compiler and compare the fixpoint without compiling it again.

The harness reports the source root, seed, and oracle paths as it selects
them. `MOSS_TEST_JOBS=1` runs the test cases and corpus checks serially.
Corpus checks use at most eight workers and report diagnostics in source
order. `MOSS_BLESS=1` rewrites the compiler and oracle diagnostic goldens and
the WASI output goldens.

The oracle and the compiler must accept and reject the same programs, up
to the documented [known gaps](tools/suite/test_agreement.py). A new
program that one accepts and the other rejects fails the suite until one
of them is fixed or the gap is recorded with its reason.

For syntax changes, edit the reference grammar in
[`MossGrammar.vy`](bootstrap/theories/MossGrammar.vy) and the self-hosted
parser in [`syntax.moss`](src/syntax.moss). The bootstrap build regenerates
the Rocq parser and checks the Menhir certificate. The
[`selfhosted.syntax` tests](tools/suite/test_selfhosted.py)
compare AST dumps from both parsers.

The native CLI requests readable diagnostics from the self-hosted compiler.
Direct users of `mossc.wasm ENTRY` retain the compact diagnostic protocol;
`mossc.wasm ENTRY --diagnostics=human` selects messages and source excerpts.
The `diagnostics` test module exercises that presentation, including UTF-8,
tabs, CRLF, imported files, and repeated errors at different locations.

Source ranges are half-open UTF-8 byte offsets into module snapshots. The AST
retains them separately from its operands, and the diagnostic tables retain
codes, code domains, names, and ranges separately from the renderer in
[`src/diagnostic.moss`](src/diagnostic.moss). Preserve these distinctions when
adding diagnostics so future tools can consume the data without parsing CLI
output. Display columns count Unicode scalars, with tabs at four-column stops.

Everything CI enforces is a flake check:

```sh
nix flake check
```

That includes `bootstrap-rocq`, which depends on the audited `moss-ref`
build. That build compiles every Rocq theory, audits the assumptions of
what it certifies, and extracts the executable in the same release build
tree. `python3 bootstrap/check.py` runs the same checks in `nix develop .#bootstrap`,
and `dune build --release run/driver.exe` in `bootstrap` builds the
executable.

For a proof edit, build the individual module so Dune reuses its unchanged
dependencies (choose a job count that fits available memory):

```sh
nix develop .#bootstrap
cd bootstrap
dune build -j 2 theories/PublicEntry.vo
```

Keep Rocq checking within two minutes, including the full audit at integration
checkpoints. Measure slow checks and investigate overruns rather than letting
proof checking become an obstacle to iteration.

A full reference-interpreter self-compile at `9af8a75` took 65 minutes
21 seconds and peaked at 2.7 GiB on the local x86 Linux VM. Its output
reached the compiler fixpoint.
Start it in the background and continue other work while it runs
(`python3 tools/profile_bootstrap.py`). Record timings and investigate
regressions. Bootstrap completion should not block ongoing development.

Use the full audit at integration checkpoints. Keep expensive stable
computations and reusable witnesses separate from frequently edited proofs.

When a proof is slow, measure both tactic construction and kernel checking;
replace repeated search with named lemmas where it helps. Generic tactics
should inspect the relevant hypothesis, such as `discriminate H`, rather
than search an entire context containing large induction hypotheses.
For speculative preservation lemmas, prove cheap `keeps` premises before
constructing `ok` or other invariants. Otherwise a failed preservation check
can cause tactics to construct and discard large invariant proofs repeatedly.

For a reproducible full-build measurement, use an empty, separate build
directory. `check.py` uses it for compilation and both assumption audits:

```sh
DUNE_BUILD_DIR="$PWD/scratch/rocq-profile" nix develop .#bootstrap -c python3 bootstrap/check.py
```

For editing proofs with
[VsRocq](https://marketplace.visualstudio.com/items?itemName=rocq-prover.vsrocq),
build the dependencies and launch VS Code from the default dev shell:

```sh
nix develop .#bootstrap
(cd bootstrap && dune build)
code .
```

The shell supplies `vsrocqtop` built with the same Rocq and OCaml as the
proofs. [`_RocqProject`](_RocqProject) maps `NM` to Dune's compiled theories;
rerun `dune build` in `bootstrap/` after changing dependencies.

## Where things are

The working design record lives in
[`docs/design/semantics.md`](docs/design/semantics.md); language changes
should land there first. The self-hosted compiler sources are under
[`src`](src), and the standard library under [`lib`](lib). Seven files are
generated, and a test enforces that they are up to date: `src/spell.moss`,
`src/tcode.moss`, `src/wasmops.moss`, `lib/char.moss`, `lib/wasichar.moss`,
`lib/byte.moss` and `lib/wasibyte.moss`. Regenerate them with

```sh
python3 tools/gensrc.py
```
