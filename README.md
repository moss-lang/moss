# Moss

An experimental programming language, exploring new ways to manage context.

> [!CAUTION]
> This project is still in its very early stages, and is _extremely_ unstable.

## Setup

First you must [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) this Git repository; any commands listed below should be run in that clone.

If you use [direnv](https://direnv.net/) and have [Nix](https://nixos.org/) with [flakes](https://wiki.nixos.org/wiki/Flakes) enabled, the easiest way to get all necessary dependencies is via the dev shell provided in this repo:

```sh
echo use flake > .envrc && direnv allow
```

The dev shell also puts a [`moss`](bin/moss) script on your `PATH` which wraps the compiler, rebuilding it if it is ever out of date.

If you don't use Nix, the only thing you need for the compiler itself is [Rust](https://rust-lang.org/tools/install/). You won't be able to use shebangs unless you also have [Python](https://www.python.org/) installed and manually put the [`bin`](bin) directory of this repo on your `PATH`. Instead, in any example below you can just replace the `moss` command with any of these three:

- `cargo run` for a debug build
- `cargo run --release` for a release build
- `cargo run --profile=release-with-debug` for a release build with debug info

## Usage

Moss source files use the `.moss` file extension, and can be made into executable scripts on Unix OSes via the `#!/usr/bin/env moss` shebang. For instance:

```sh
examples/hello.moss
```

Or, equivalently:

```sh
moss examples/hello.moss
```

Specifically, this implicitly invokes the `run` subcommand of the compiler:

```sh
moss run examples/hello.moss
```

You can alternatively use the `build` command to output [WebAssembly](https://webassembly.org/) code for [WASI P1](https://wasi.dev/interfaces#wasi-01):

```sh
moss build examples/hello.moss | wasm-tools print
```

This is supported by many WebAssembly engines, such as [Wasmtime](https://wasmtime.dev/) which is what `moss run` uses internally:

```sh
moss build examples/hello.moss | wasmtime -
```

An experimental native backend is available for proof-of-concept testing. It ignores the Moss source and always emits a small native "Hello, world!" object file via LLVM:

```sh
moss run --backend native examples/hello.moss
```
This backend links against LLVM directly and does not need a `clang` binary at runtime. The Nix dev shell sets the necessary `llvm-sys` environment so this works out of the box.

## Language

Documentation pending.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md).

## License

This project is licensed under the [MIT License](LICENSE).
