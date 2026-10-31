# Contributing to Moss

All commands in this file are assumed to be run within the Nix dev shell from
the root of a clone of this repository:

```sh
nix develop
```

The compiler is a dependency-free Python program under
[`bootstrap`](bootstrap). To run its test suite (the dev shell provides
Wasmtime, which the Wasm backend tests require):

```sh
cd bootstrap && python3 -m tests.parallel
```

That spreads the tests over a worker process per core, and is
what the flake's `bootstrap` check runs; `python3 -m unittest` runs the
same tests one after another, some seven times slower. The floor is the
slowest single test, which the runner lists at the end. To iterate on
anything else, name what you are working on —
`python3 -m unittest tests.test_run` — and run the whole suite before you
commit. What a generation costs and where it goes is in
[`docs/implementation/selfhosting.md`](docs/implementation/selfhosting.md).

Everything CI enforces is a flake check:

```sh
nix flake check
```

The working design record lives in
[`docs/design/semantics.md`](docs/design/semantics.md); language changes
should land there first. The self-hosted compiler sources are under
[`src`](src), and the standard library under [`lib`](lib) —
[`lib/char.moss`](lib/char.moss) is generated from
`bootstrap/mossc/native.py` (a test enforces agreement).
