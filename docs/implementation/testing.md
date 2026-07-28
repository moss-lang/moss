# Testing

We believe that golden file testing provides an exceptional balance of
flexibility, clarity, and convenience. Especially for a compiler, it
should be first testing technique you reach for.

Three practices this repository has settled into on top of that.

## For a second implementation, the golden is the first one

The compiler under [`src`](/src) is not checked against goldens of its
own. Every stage is held to the bootstrap's output for the same input,
exactly: parse trees compared character for character in one shared
format ([`mossc/sexpr.py`](/bootstrap/mossc/sexpr.py) and
[`src/dump.moss`](/src/dump.moss)), scope tables compared as a set of
rows, requirement lists compared in order, and compiled modules compared
by what they print.

This is worth the trouble because a plausible answer is the failure mode.
An approximate front end produces a believable letter-per-declaration
dump; it does not produce the bootstrap's tree. Both compilers agreeing
on all 810 requirement lists of the compiler's own source is a claim
neither could fake alone.

There is one check with no first implementation to appeal to, and it is
the strongest: the compiler compiles its own source twice and the two
generations agree byte for byte. Nothing outside the compiler holds it to
that — it is held to its own output on its own input, which is the one
golden it cannot get wrong by agreeing with itself, because a compiler
that mis-compiles anything it *uses* stops reproducing. That test also
asserts the first generation differs from the bootstrap's, so it cannot
quietly become a comparison of something with itself.

It has a second-order benefit worth knowing about: twice, the first thing
an equivalence check found was a bug in the *comparison* rather than in
either compiler — a symbol counted once per module that imported it, a
test whose premise had quietly become false. A golden of its own would
have agreed with itself and said nothing.

## Run the compiler, do not interpret it

`mossc/interp.py` is a tree-walking interpreter over the core IR, so it
runs a Moss program some three orders of magnitude slower than the same
program compiled. That is fine for the language's own tests and wrong for
anything at compiler scale.

A driver written in Moss is ordinary Moss over `Std`, so the bootstrap can
compile it to a WASI module and the test can run *that*. Doing so took the
four self-hosted stage tests from 534 seconds to 8.5, and the whole suite
from nine minutes to under thirty seconds — the program under test is
identical either way. `run_driver` in
[`tests/test_run.py`](/bootstrap/tests/test_run.py) is the helper. Use
`moss build` rather than `moss run` for the same reason.

The same trick has a second stage, for the one program big enough to need
it: run it through `wasm-opt -O3` first. The self-hosted back end emits
straight-line code and leaves every call a call, so the *compiler* as a
module is about fifteen times slower than it has to be; one second of
optimizer takes a generation of self-compilation from two and a half
minutes to seven seconds. `wasm_opt` in
[`tests/test_build.py`](/bootstrap/tests/test_build.py) is the helper.

Two rules come with it. Optimize the thing that *runs*, never the thing
under test: what the fixpoint compares is the raw output of each
generation, because raw equality implies optimized equality and not the
other way round — comparing optimized modules could hide a difference the
optimizer happens to erase. And assert the premise: one test compiles the
same input with the optimized and the unoptimized compiler and requires
the same bytes, so "the optimizer preserves behaviour" is checked here
rather than assumed.

The suite is now about five and a half minutes again, and five of those
are one test: the fixpoint, which is two generations of the compiler
compiling its own 35 modules. That is not the interpreter — it runs as
Wasm — it is the constant factor of `Std` written in Moss, which is
fifteen times the bootstrap's native shims for the same work. The cost is
worth stating rather than hiding, because it is a standing invitation to
fix `lib/wasistd.moss`: doing so gives the whole suite back.

## A suite that speeds up is a claim to check

A sixty-fold speedup and a dropped failure count look the same as a test
that has stopped asserting. Both have happened here. When either
appears, prove the test still does its work — count the inputs it
covers, check that two different inputs still disagree — before believing
it.

The same goes for counting tests instead of naming them: a test class was
once deleted by a careless edit while the total stayed flat, because
others were added in the same commit.
