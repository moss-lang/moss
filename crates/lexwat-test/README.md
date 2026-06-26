# lexwat-test — a WebAssembly port of the moss lexer, and a performance autopsy

> **Status: throwaway experiment.** This crate is not meant to be merged. It exists
> as a record of an exploration: hand-writing the moss lexer in WebAssembly text
> format, verifying it against the real lexer, and then chasing down *why* it runs
> slower than native and how much of that gap is recoverable.

## What's here

The starting point was a single question: write `lex.wat` — a `.wat` module that
imports a `next_byte` function and exports a `next_token` function emulating
[`crates/moss-core/src/lex.rs`](../moss-core/src/lex.rs) (a [logos](https://docs.rs/logos)
lexer) — and test it. It then grew several variants to isolate performance factors.

### The lexer variants (`.wat`)

All produce the **identical token stream** as `moss-core`'s logos lexer (token ids =
`Token` enum discriminants; `0` = Eof, `-1` = invalid token). Each is verified
token-for-token against logos in `tests/equivalence.rs`.

| file | dispatch | source bytes | notes |
|---|---|---|---|
| `lex.wat` | `if`-chain | `next_byte` import, buffered in memory | the original deliverable |
| `lex_brtable.wat` | class table + `br_table` | same | isolates dispatch strategy |
| `lex_nomem.wat` | `if`-chain | `next_byte` import, **no memory** | pushback in globals + i64 keyword match |
| `lex_nomem_brtable.wat` | `br_table` on byte | **no memory** | both optimizations combined |
| `lex_selfsource.wat` | `if`-chain | reads directly from its own memory | no per-byte boundary crossing |

Because logos does maximal-munch with backtracking (`123u3` → `123u` then `3`), the
streaming variants either buffer all bytes (memory) or keep a small bounded pushback
(no-memory) — the lookahead is never more than 3 bytes. Keyword-vs-identifier is
done by scanning `[A-Z_a-z]\w*` then matching the lexeme; UTF-8 char literals (`'é'`)
consume a whole scalar like logos's `.`. `\w`/`\s` use their ASCII definitions
(moss source is ASCII).

### The "driver" modules and merge artifacts

To remove the host⇄wasm boundary crossings, a driver module owns the source bytes
and the merge tool wires it to the lexer (no lexer changes):

| file | role |
|---|---|
| `driver.wat` | owns source, **exports** `next_byte` |
| `driver2.wat` | also **imports** `next_token` and loops it internally; two memories (input bytes / output token ids), exports `run()` |
| `merged.wasm` | `wasm-merge lex_nomem.wat lexer driver.wat env` — `next_byte` internalized |
| `merged2.wasm` | `wasm-merge lex_nomem.wat lexer driver2.wat env --enable-multimemory` — both `next_byte` *and* the `next_token` loop internalized; one host call per document |
| `self_opt.wasm` | `wasm-opt -O3 lex_selfsource.wat` |
| `merged2_opt.wasm` | `wasm-opt -O3 --enable-multimemory merged2.wasm` |

These `.wasm` are **not** committed. [`build.rs`](build.rs) regenerates them into
`OUT_DIR` from the `.wat` sources using [binaryen](https://github.com/WebAssembly/binaryen)'s
`wasm-merge`/`wasm-opt`, which the Nix dev shell provides. Outside the dev shell
(tools absent), `build.rs` skips generation and the few tests that use these
artifacts report as `ignored`; everything else still runs.

### `wasm2c_bench.c`

The "compile wasm to native via LLVM" data point: translate `lex_selfsource.wat` to C
with [wabt](https://github.com/WebAssembly/wabt)'s `wasm2c`, compile with `clang -O3`,
benchmark the same corpus. See the header comment for the exact commands.

### Rust

- `src/lib.rs` — `hand_count` / `hand_tokens`: a **native** Rust port of the exact
  `lex_selfsource` algorithm, used to separate "algorithm cost" from "wasm cost".
- `tests/equivalence.rs` — differential tests: every variant vs logos over curated
  cases, **all repo `.moss` files**, and ~19k fuzzed inputs.
- `tests/bench.rs` — the throughput comparison (`#[ignore]`d).

## Running it

Run inside the Nix dev shell so binaryen is on `PATH` (otherwise `build.rs` can't
generate the merged/optimized modules and those tests are ignored):

```sh
nix develop
cargo test -p lexwat-test                                                  # correctness
cargo test -p lexwat-test --release --test bench -- --ignored --nocapture  # benchmark
```

## Findings

Throughput on a ~549 KB corpus (all clean `.moss` files, repeated), 100 passes.
Numbers are representative; they jitter a few percent.

| | MB/s | ns/token |
|---|--:|--:|
| **rust logos** (the reference) | ~765 | 4.7 |
| rust hand-rolled, native (our algorithm) | ~1010 | 3.5 |
| rust byte-sum, native (vectorized ceiling) | ~10900 | — |
| wasm `lex.wat` (if-chain, import) | ~76 | 47 |
| wasm `lex_brtable.wat` | ~81 | 44 |
| wasm `lex_nomem.wat` | ~99 | 36 |
| wasm `lex_nomem_brtable.wat` | ~100 | 36 |
| wasm `lex_selfsource.wat` | ~112 | 32 |
| &nbsp;&nbsp;+ `wasm-opt -O3` | ~164 | — |
| wasm `merged` (internalize `next_byte`) | ~122 | 29 |
| wasm `merged2` (also internalize the `next_token` loop) | ~158 | 23 |
| &nbsp;&nbsp;+ `wasm-opt -O3` | ~395 | 9 |
| wasm2c → `clang -O3` (LLVM, AOT native) | ~440 | 8 |
| wasm2c → `gcc -O3` (AOT native) | ~510 | 7 |
| wasm `next_byte` drain only (import overhead floor) | ~378 | — |
| wasm scalar byte-scan ceiling (in-wasm, no calls) | ~2730 | — |

What the numbers say:

1. **`br_table` vs `if`-chain**: ~6% with memory (77→81), negligible without (99→100).
   Dispatch was never the bottleneck — the `if`-chain short-circuits on the common
   bytes.

2. **Dropping `memory`**: ~28% (the if-chain version 77 → no-memory 99), mostly from
   avoiding a per-byte `i32.store8` into the buffer.

3. **Boundary crossings matter, and the per-*token* one most**: internalizing
   `next_byte` (`merged`, +22%) and then the `next_token` loop (`merged2`, +28%) nearly
   doubles throughput vs the original. A host⇄wasm typed call per token is pricier than
   the per-byte fetch even though there are far fewer tokens.

4. **The algorithm is not the problem.** Our hand-rolled scanner compiled *natively*
   (1010 MB/s) is **faster than logos** (765). logos's elegant table-driven DFA (256-entry
   `[u8;256]` bitmask transition tables + an 8-byte-unrolled identifier scan) is nice, but
   it is not what makes logos ~5× faster than our wasm.

5. **The ~5× gap is the execution substrate**, and it decomposes cleanly:
   - **~2.5× was un-optimized codegen** — the `wat` assembler emits naive, non-inlined
     functions and wasmtime's Cranelift (a fast-compile JIT) doesn't recover them.
     `wasm-opt -O3` inlines them and gets `merged2` from 158 → 395 MB/s (~52% of logos).
   - **~2× is the genuine native-vs-wasm tax** — Cranelift < LLVM `-O3`, wasm's
     linear-memory bounds model, and no autovectorization (scalar wasm scan tops out at
     ~2730 MB/s vs ~10900 for LLVM's vectorized native scan).

6. **Compiling wasm to native via LLVM** (`wasm2c` → `clang -O3`) reaches ~440 MB/s
   (gcc ~510) — better than the JIT, but still short of native Rust, because lowering to
   wasm already discarded the high-level structure (aliasing, loop shapes, types) the C
   compiler would need to fully optimize/vectorize.

**Bottom line:** wasm costs roughly 2× even with the best AOT LLVM compilation; about
half of the gap we were originally measuring was simply that the module was never
optimized. If raw speed is the goal, compile the lexer straight from a high-level
language; wasm is a portability/sandboxing substrate and you pay for it.

## External tools (provided by the Nix dev shell)

`flake.nix`'s dev shell adds:

- [binaryen](https://github.com/WebAssembly/binaryen) — `wasm-merge`, `wasm-opt`
  (used by `build.rs` to generate the merged/optimized `.wasm`)
- [wabt](https://github.com/WebAssembly/wabt) — `wasm2c` + its `wasm-rt` runtime
- `clang` — AOT compilation of the `wasm2c` output for the native bench

Nothing is vendored or committed as a binary; the artifacts are regenerated.
