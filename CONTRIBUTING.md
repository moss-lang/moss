# Contributing to Moss

All commands in this file are assumed to be run within the Nix dev shell from
the root of a clone of this repository:

```sh
nix develop
```

## How the compiler is built

The compiler is written in Moss, under [`src`](src), and compiles itself.
Building it from nothing is the job of [`bootstrap`](bootstrap): a front
end and interpreter for Moss written in Rocq and extracted to OCaml (the
*oracle*), which runs `src/main.moss` compiling itself. That is the
flake's `compiler` package. This is an expensive interpreted self-compile;
the build prints phase timings to stderr:

```sh
nix build .#compiler -o result-compiler   # the bootstrap
nix build .#oracle -o result-oracle       # audited oracle without self-compilation
```

You do not do that for everyday work. Any compiler built before, from an
older commit too, can build the current one, as long as it compiles the
current source correctly: what it writes is then compiled once more by
itself. So:

- **Everyday work reuses a previously built compiler**, the *seed*. The
  [`moss`](bin/moss) launcher and the test suite both look for one in
  `MOSS_SEED` (a `mossc.wasm`), and otherwise in `result-compiler`; the
  launcher also reuses the compiler it built last in `target/`. Get one
  once, with the bootstrap above or copied from a machine that has built
  one, and keep it.
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

## Tests

The test suite is the Rust crate [`crates/moss-tests`](crates/moss-tests).
It needs a seed (above) and the oracle (`MOSS_ORACLE`, else
`result-oracle/bin/moss_m2`, else
`bootstrap/_build/default/ocaml/moss_m2.exe`):

```sh
cargo test
```

That runs the whole workspace: the driver's unit tests and the suite,
which compiles `src/` with the seed, checks that the result is a fixpoint
of itself, and then holds it to every program of `tests/` and `examples/`
(goldens, expected errors, and what the oracle makes of the same
programs). Name a module to iterate on it, as in `cargo test -p
moss-tests language::`, and run the whole suite before you commit. Its
modules say what they check; `MOSS_BLESS=1` rewrites the goldens of
[`tests/errors`](tests/errors).

The oracle and the compiler must accept and reject the same programs, up
to an explicit, commented list of known gaps
([`agreement.rs`](crates/moss-tests/tests/suite/agreement.rs)). A program
added to `tests/` that one accepts and the other rejects fails the suite
until one of them is fixed or the gap is listed with its reason.

Everything CI enforces is a flake check:

```sh
nix flake check
```

That includes `bootstrap-rocq`, which depends on the audited oracle build.
The oracle compiles every Rocq theory, audits the assumptions of what it
certifies, and extracts the executable in the same release build tree;
`bootstrap/check.sh` runs the same in `nix develop .#bootstrap`, where
`dune build` in `bootstrap` builds the oracle
([`bootstrap/README.md`](bootstrap/README.md)).

For editing proofs with
[VsRocq](https://marketplace.visualstudio.com/items?itemName=rocq-prover.vsrocq),
build the dependencies and launch VS Code from the default dev shell:

```sh
nix develop
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
cargo run -p moss-tests --bin gensrc
```
