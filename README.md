# Moss

An experimental programming language, exploring new ways to manage context.

> [!CAUTION]
> This project is still in its very early stages, and is _extremely_ unstable.

## Setup

First you must [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) this Git repository; any commands listed below should be run in that clone.

The compiler is a dependency-free Python program under [`bootstrap`](bootstrap); see [`bootstrap/README.md`](bootstrap/README.md) for its pipeline and [`docs/design/semantics.md`](docs/design/semantics.md) for the working design record. A second compiler, written in Moss, lives under [`src`](src) and now compiles itself: see [`docs/implementation/selfhosting.md`](docs/implementation/selfhosting.md).

If you use [direnv](https://direnv.net/) and have [Nix](https://nixos.org/) with [flakes](https://wiki.nixos.org/wiki/Flakes) enabled, the easiest way to get all necessary dependencies is via the dev shell provided in this repo:

```sh
echo use flake > .envrc && direnv allow
```

The dev shell puts the [`moss`](bin/moss) development launcher on your `PATH`.
It builds a native driver that embeds
[Wasmtime](https://wasmtime.dev/) and links
[Binaryen](https://github.com/WebAssembly/binaryen). Python 3.12+ is needed
only to bootstrap a changed self-hosted compiler in a source checkout; the
packaged compiler already contains that result.

`nix build` produces the normal Nix package. On Linux,
`nix build .#standalone` produces a statically linked driver in `result/bin`,
with the optimized self-hosted compiler and standard library under
`result/share/moss`. Those three pieces can be copied together without Nix.

## Usage

Moss source files use the `.moss` file extension, and can be made into executable scripts on Unix OSes via the `#!/usr/bin/env moss` shebang. For instance:

```sh
examples/hello.moss
```

Or, equivalently:

```sh
moss examples/hello.moss
```

Specifically, this implicitly invokes `run`, which compiles the program and
executes the resulting Wasm:

```sh
moss run examples/hello.moss
```

Pass an explicit Binaryen optimization level with `-O0` through `-O4`, `-Os`,
or `-Oz`.

You can alternatively use the `build` command to output [WebAssembly](https://webassembly.org/) code for [WASI P1](https://wasi.dev/interfaces#wasi-01), supported by many WebAssembly engines such as [Wasmtime](https://wasmtime.dev/):

```sh
moss build examples/hello.moss > hello.wasm && wasmtime hello.wasm
```

## Documentation

See the [`docs`](docs) folder.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md).

## License

This project is licensed under the [MIT License](LICENSE).
