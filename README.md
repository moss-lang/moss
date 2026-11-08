# Moss

An experimental programming language, exploring new ways to manage context.

> [!CAUTION]
> This project is still in its very early stages, and is _extremely_ unstable.

## Setup

First you must [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) this Git repository; any commands listed below should be run in that clone.

The compiler is written in Moss, under [`src`](src), and compiles itself; see [`docs/implementation/selfhosting.md`](docs/implementation/selfhosting.md). It is bootstrapped by a front end and interpreter for Moss written in [Rocq](https://rocq-prover.org/) and extracted to OCaml, under [`bootstrap`](bootstrap), which runs the compiler compiling itself; see [`bootstrap/README.md`](bootstrap/README.md) for what it is and what of it is proved. The working design record is [`docs/design/semantics.md`](docs/design/semantics.md).

If you use [direnv](https://direnv.net/) and have [Nix](https://nixos.org/) with [flakes](https://wiki.nixos.org/wiki/Flakes) enabled, the easiest way to get all necessary dependencies is via the dev shell provided in this repo:

```sh
echo use flake > .envrc && direnv allow
```

The dev shell puts the [`moss`](bin/moss) development launcher on your `PATH`.
It builds a native driver that links
[Wasmtime](https://wasmtime.dev/) and
[Binaryen](https://github.com/WebAssembly/binaryen), both from Nix. In a
source checkout, the first run after `src` or `lib` changes rebuilds the
self-hosted compiler with one built before (see
[`CONTRIBUTING.md`](CONTRIBUTING.md)); the packaged compiler already
contains that result.

To try a compiler revision without a checkout, set `MOSS_SEED` to a previously
built `mossc.wasm` and run `nix run github:moss-lang/moss#dev --refresh -- PROGRAM`;
see the [seed workflow](CONTRIBUTING.md#trying-a-revision-without-a-checkout).

`nix build` produces the normal Nix package. `nix build .#standalone` produces
a bundle for Linux or macOS that can be copied and used without Nix; an
x86-64 Windows bundle can be cross-compiled from x86-64 Linux with
`nix build .#windows`. Each bundle contains a driver with the optimized
self-hosted compiler embedded in it, plus the standard library under
`share/moss/lib`.

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
