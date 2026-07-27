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

Status highlights: all seven runnable `examples/` match their goldens;
`tests/errors/` are golden-checked diagnostics; and the self-hosted lexer
in `src/lex.moss` runs on this interpreter and tokenizes real Moss files,
including its own source (`python3 -m mossc run ../src/main.moss FILE`, or
`bin/moss-boot run src/main.moss FILE` from the repo root), with keywords
recognized by a character trie (no string literals needed — D48).

## Usage

```sh
python3 -m mossc lex ../src/token.moss     # token dump
python3 -m mossc parse ../src/wasm.moss    # AST dump
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
