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

Stage 5's swap-out has begun: [`mossc/build.py`](mossc/build.py)
compiles the scalar subset (chars/ints/bools as i32, vals as hidden
parameters, `putchar` as an fd_write shim) of the same IR to a WASI
module — `python3 -m mossc build FILE | wasmtime -` runs every example
with output identical to the interpreter's. Records, tags, match,
strings, and fn binds are the next slices.

Status highlights: all seven runnable `examples/` match their goldens
under both the interpreter and the Wasm backend; `tests/errors/` are
golden-checked diagnostics; and the self-hosted front end under `src/` —
lexer with a keyword trie, arena parser, codepoint-arena interner,
duplicate-declaration and unresolved-reference checks — runs on the
interpreter *and* compiles to a single WASI module with byte-identical
output (`moss run src/main.moss FILE`, or the drivers in the Wasm backend
tests).

On top of that, `src/collect.moss` loads a whole module graph: it reads
the entry file and everything it imports, transitively, and reports
duplicates and out-of-scope references per module. Imports contribute
the names they bring in, and a prelude — passed as an argument, since
`arg_at` lets a program find paths without holding any — puts its scope
under every module below it. Pointed at `lib/prelude.moss` and
`src/main.moss` it reaches all eighteen modules of the compiler's own
sources and explains every name in them. It runs on the interpreter
only: `Path` is not in the Wasm slice (nor are fn binds).

Under all of that sits the primitive context of D52. A program may
assume `Wasm` and `Wasi` ([`lib/wasm.moss`](/lib/wasm.moss),
[`lib/wasip1.moss`](/lib/wasip1.moss)) instead of `Std`, in which case
intrinsics compile to instructions and WASI functions to imports, with
no shims involved at all — see `tests/wasi/raw.moss`. Implementing `Std`
itself in Moss on top of that is what retires the native table.

## Usage

```sh
python3 -m mossc lex ../src/token.moss     # token dump
python3 -m mossc parse ../lib/wasm.moss    # AST dump
python3 -m mossc run ../examples/hello.moss
python3 -m mossc build ../examples/hello.moss > hello.wasm  # then: wasmtime hello.wasm
```

(Run from this directory, or set `PYTHONPATH` to it.)

## Tests

```sh
cd bootstrap && python3 -m unittest
```

The parser tests include a corpus check over the `src/` files that are
already written in the MVP language; the remaining `src/` files join the
corpus as the rewrites listed in the decision log's §12 errata land.
