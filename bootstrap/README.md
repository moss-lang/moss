# Bootstrap compiler

A Moss compiler written in Python (stdlib only, no dependencies), following
the pipeline in [`docs/design/semantics.md`](../docs/design/semantics.md)
§11:

1. **lex** ([`mossc/lex.py`](mossc/lex.py)) — done
2. **parse** ([`mossc/parse.py`](mossc/parse.py), AST in
   [`mossc/ast.py`](mossc/ast.py)) — done
3. **collect** ([`mossc/collect.py`](mossc/collect.py)): module graph,
   scopes, exports, D44 collisions — done
4. **lower** ([`mossc/lower.py`](mossc/lower.py)): requirement
   environments, context flattening, bind checking, forward type
   inference, method resolution, elaboration to the explicit-context IR
   ([`mossc/ir.py`](mossc/ir.py)) — working; v0 simplifications are marked
   TODO in place
5. **interpret** ([`mossc/interp.py`](mossc/interp.py)): runs the core IR —
   the runtime environment is literally the explicit context structure
   lowering produced — with the native Std of
   [`mossc/native.py`](mossc/native.py) (D38) and `path:line:col`
   diagnostics that keep hello.md's scope/context error distinction —
   working

Stage 5's swap-out is done: [`mossc/build.py`](mossc/build.py) compiles
the same IR — records, tags, match, strings, fn binds and all — to a
WASI module, and `python3 -m mossc FILE | wasmtime -` runs every
example with output identical to the interpreter's.

Status highlights: all seven runnable `examples/` match their goldens
under both the interpreter and the Wasm backend, and `tests/errors/` are
golden-checked diagnostics.

The self-hosted compiler under `src/` is now a compiler rather than a
front end: it lexes, parses the whole grammar into arenas, loads the
module graph, resolves every name in it, and writes a WASI module. See
[`docs/implementation/selfhosting.md`](../docs/implementation/selfhosting.md)
for what it covers — the primitive context of D52 — and what it does
not. It is held to this compiler at each stage rather than to goldens of
its own: both parsers write the tree in the one compact format of
[`mossc/sexpr.py`](mossc/sexpr.py) and agree character for character
over the whole corpus; both collects agree on all 1101 scope rows of the
compiler's own 23 modules; and both back ends produce modules that
behave the same on `tests/wasi/prim.moss`. It compiles too — the whole
thing, `Std` and all, as one WASI module that does the same job in a
fraction of the time and writes the same bytes.

Under all of that sits the primitive context of D52. A program may
assume `Wasm` and `Wasi` ([`lib/wasm.moss`](/lib/wasm.moss),
[`lib/wasip1.moss`](/lib/wasip1.moss)) instead of `Std`, in which case
intrinsics compile to instructions and WASI functions to imports, with
no shims involved at all — see `tests/wasi/raw.moss`. A signature can be
implemented over that primitive context and installed with a functor
(D55), which is how `Std` is being moved into Moss:
[`lib/wasistd.moss`](/lib/wasistd.moss) provides `putchar`, `print`,
`first_arg`, a bump allocator, all of `Int`'s arithmetic, `String` with
its methods, `Path` including a `read` built from path_open and fd_read,
and `CellInt`/`IntList`; [`lib/wasichar.moss`](/lib/wasichar.moss)
provides the char constants. All of it is there now, and
[`lib/wasi.moss`](/lib/wasi.moss) composes the pieces into a single
functor providing the whole `Std` signature: `tests/wasi/full.moss` is
ordinary Moss written against `Std`, whose `main` assumes only
`Wasm, Wasi, Branch` and opens with `bind WasiStd;`.

## Usage

```sh
python3 -m mossc ../examples/hello.moss > hello.wasm  # then: wasmtime hello.wasm
```

(Run from this directory, or set `PYTHONPATH` to it.)

This is deliberately the bootstrap compiler's entire command-line interface:
it loads the module graph rooted at `FILE` and writes Wasm to stdout. The
Python interpreter remains a test oracle for the compiler pipeline; the native
`moss` driver does not expose it as an execution mode.

## Tests

```sh
cd bootstrap && python3 -m unittest
```

The parser tests include a corpus check over every live `.moss` file in
the repository (`corpus()` in `tests/test_run.py`); the one exclusion is
`examples/escape.moss`, which needs string literals (D48).
