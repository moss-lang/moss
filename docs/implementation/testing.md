# Testing

We believe that golden file testing provides an exceptional balance of
flexibility, clarity, and convenience. Especially for a compiler, it
should be first testing technique you reach for.

Three practices this repository has settled into on top of that.

## For a second implementation, the golden is the first one

The compiler under [`src`](/src) was not checked against goldens of its
own while it was being written. Every stage was held to the Python
bootstrap's output for the same input, exactly: parse trees compared
character for character in one shared format, scope tables compared as a
set of rows, requirement lists compared in order, and compiled modules
compared by what they print. The Python bootstrap has since been replaced
by a front end and interpreter written in Rocq ([`bootstrap`](/bootstrap),
D86), and the same discipline holds the self-hosted compiler to it:

- the Rocq parser and the self-hosted one (`src/dump.moss`) write the
  same tree for every file of the corpus, and the same refusals for the
  operator and literal snippets that pin D68's grammar down;
- the oracle and the self-hosted compiler accept and reject the same
  programs, over every program of `tests/` and `examples/`, up to a
  commented list of known gaps (`tests/suite/agreement.rs`);
- programs that both accept print the same thing, run by the oracle and
  compiled by the compiler, against goldens where there are any.

This is worth the trouble because a plausible answer is the failure mode.
An approximate front end produces a believable letter-per-declaration
dump; it does not produce another implementation's tree. The Rocq
parser was itself held to the Python one before the switch: identical
dumps for all 282 files of the corpus and for 30000 mutants of them.

There is one check with no first implementation to appeal to, and it is
the strongest: the compiler compiles its own source and gets itself back,
byte for byte. Nothing outside the compiler holds it to that — it is held
to its own output on its own input, which is the one golden it cannot get
wrong by agreeing with itself, because a compiler that mis-compiles
anything it *uses* stops reproducing. The bootstrap makes it a comparison
between two implementations as well: the oracle *interprets* the compiler
compiling itself, and the compiled compiler has to write the same bytes.

It has a second-order benefit worth knowing about: twice, the first thing
an equivalence check found was a bug in the *comparison* rather than in
either compiler — a symbol counted once per module that imported it, a
test whose premise had quietly become false. A golden of its own would
have agreed with itself and said nothing. Moving the tests to the oracle
found more, all in the self-hosted compiler, each now a test that asserts
the current behaviour with a comment saying so: it accepts
an attached method on an abstract type (Q5), a call to `putchar` with no
argument, and a fn bind whose provider's signature does not match; it
refuses the `IsCell` idiom when the detached methods are imported under
other names (D44); and a program importing `src/dump.moss` exhausts
Wasmtime's default stack while compiling. Most of these the old suite
never reached: its programs' `main` assumed the Python bootstrap's native
`Std`, which only Python ran, and the self-hosted compiler refused them
for that first.

## Run the compiler, do not interpret it

The oracle is a tree-walking interpreter, proved correct rather than
built for speed: it runs the compiler compiling itself about 1600 times
slower than the compiled compiler does. That is fine for the language's
own tests and for a bootstrap that runs once per commit in CI, and wrong
for anything at compiler scale that runs often. So the test suite runs
every program, the compiler included, as Wasm, and asks the oracle only
for what it is for: a second opinion on programs of test size, and the
accept/reject check, which only elaborates.

The compiler as a module runs through `wasm-opt -O3` first. The
self-hosted back end emits straight-line code and leaves every call a
call, so the *compiler* as emitted is many times slower than it has to
be. Two rules come with that. Optimize the thing that *runs*, never the
thing under test: what the fixpoint compares is the raw output of each
generation, because raw equality implies optimized equality and not the
other way round — comparing optimized modules could hide a difference
the optimizer happens to erase. And assert the premise: one test compiles
the same input with the optimized and the unoptimized compiler and
requires the same bytes, so "the optimizer preserves behaviour" is
checked here rather than assumed.

Every module the suite writes is validated with `wasm-tools validate`
before anything else reads it (`wasm_file` in
[`crates/moss-tests/src/lib.rs`](/crates/moss-tests/src/lib.rs)): on the
compiler's raw output, before Binaryen, which reads an invalid module
without complaint and may quietly repair it, and before Wasmtime, whose
refusal reads like a runtime failure.

## A suite that speeds up is a claim to check

A sixty-fold speedup and a dropped failure count look the same as a test
that has stopped asserting. Both have happened here. When either
appears, prove the test still does its work — count the inputs it
covers, check that two different inputs still disagree — before believing
it.

The same goes for counting tests instead of naming them: a test class was
once deleted by a careless edit while the total stayed flat, because
others were added in the same commit.
