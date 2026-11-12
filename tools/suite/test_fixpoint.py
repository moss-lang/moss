from suite.support import (
    bootstrap_details,
    compile_self,
    compiler,
    compiler_raw,
    wasm_file,
    wasmtime,
)

# The compiler compiles itself, and the result is a fixpoint.
#
# The compiler under test is the seed's compile of `src/`
# (`moss_tests::compiler_raw`). Compiling `src/` once more with it must
# reproduce it byte for byte: whatever the seed did differently is gone.
# Nothing outside the compiler holds it to that: it is held to its own
# output on its own input, which is the one golden it cannot get wrong by
# agreeing with itself, because a compiler that mis-compiles anything it
# *uses* stops reproducing. (The flake's `compiler` package checks the
# same of the oracle's output, which is the first generation there.)
#
# Each generation *runs* through `wasm-opt -O3`, but what is compared is
# the raw output of each, never the optimized one: raw equality implies
# optimized equality and not the other way round.


def test_the_compiler_is_a_fixpoint_of_itself():
    details = bootstrap_details()
    actual = (details / "s2.wasm").read_bytes() if details else compile_self(compiler())
    assert actual == compiler_raw(), "the compiler is not a fixpoint of itself"


# What running the optimized compiler rests on: the optimized compiler
# and the compiler as emitted write the same module for the same input.
# Run on an example rather than on `src/`, so it costs one slow
# generation instead of two.


def test_optimizing_the_compiler_does_not_change_it():
    args = ["examples/context.moss"]
    raw = wasm_file(compiler_raw())
    emitted = wasmtime(raw, "lib/prelude.moss", args)
    assert emitted.code == 0, f"{emitted.stderr}"
    assert emitted.is_module()
    optimized = compiler()
    again = wasmtime(optimized, "lib/prelude.moss", args)
    assert again.stdout == emitted.stdout, "the optimizer changed what the compiler writes"
