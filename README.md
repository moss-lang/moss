# Connie

An experimental programming language, exploring new ways to manage context.

## Setup

First you must [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) this Git repository; any commands listed below should be run in that clone.

If you use [direnv](https://direnv.net/) and have [Nix](https://nixos.org/) with [flakes](https://wiki.nixos.org/wiki/Flakes) enabled, the easiest way to get all necessary dependencies is via the dev shell provided in this repo:

```sh
echo use flake > .envrc && direnv allow
```

The dev shell also puts a [`connie`](bin/connie) script on your `PATH` which wraps the compiler, rebuilding it if it is ever out of date.

If you don't use Nix, the only thing you need for the compiler itself is [Rust](https://rust-lang.org/tools/install/). You won't be able to use shebangs unless you also have [Python](https://www.python.org/) installed and manually put the [`bin`](bin) directory of this repo on your `PATH`. Instead, in any example below you can just replace the `connie` command with any of these three:

- `cargo run` for a debug build
- `cargo run --release` for a release build
- `cargo run --profile=release-with-debug` for a release build with debug info

## Usage

Connie source files use the `.con` file extension, and can be made into executable scripts on Unix OSes via the `#!/usr/bin/env connie` shebang. For instance:

```sh
examples/hello.con
```

Or, equivalently:

```sh
connie examples/hello.con
```

Specifically, this implicitly invokes the `run` subcommand of the compiler:

```sh
connie run examples/hello.con
```

You can alternatively use the `build` command to output [WebAssembly](https://webassembly.org/) code for [WASI P1](https://wasi.dev/interfaces#wasi-01):

```sh
connie build examples/hello.con | wasm-tools print
```

This is supported by many WebAssembly engines, such as [Wasmtime](https://wasmtime.dev/) which is what `connie run` uses internally:

```sh
connie build examples/hello.con | wasmtime -
```

## Language

Documentation pending.

## License

This project is licensed under the [MIT License](LICENSE).
