# Verification milestone: partial source and runtime proofs

This checkpoint contains a substantial proof development, not an end-to-end
verified compiler. In particular, **source elaboration soundness and the
universal runtime typing certificate remain open**. The public contract is
[`Audit.Public.CORRECT`](theories/Audit.v): build soundness and completeness,
run soundness and completeness, and safety. These five end-to-end
obligations remain pending. Audit.v defines Builds, Runs and their semantic
dependencies directly. Building also promises compatibility with the fixed
default interface; execution and safety quantify over any compatible interface.
[DefaultRuntime.v](theories/DefaultRuntime.v) proves the concrete default
interface's primitive and heap laws. The source-to-platform link guarantee
remains part of the pending build soundness proof.

[Certified.v](theories/Certified.v) binds the existing component results in
[ComponentContracts.v](theories/ComponentContracts.v).
[CoreFragment.v](theories/CoreFragment.v) contains the smaller calculus used
by those proofs.

## Checked components

The development includes proofs of:

- Lexer/parser soundness and completeness, loader correctness and exact
  parse-dump round trips ([`FrontEnd.v`](theories/FrontEnd.v),
  [`FrontEndComplete.v`](theories/FrontEndComplete.v),
  [`FrontEndLoader.v`](theories/FrontEndLoader.v),
  [`DumpRoundTrip.v`](theories/DumpRoundTrip.v)).
- The loader's world, final requirement reading, and preservation through
  elaboration ([`WorldSound.v`](theories/WorldSound.v),
  [`ReqsTotal.v`](theories/ReqsTotal.v), [`ElabFrame.v`](theories/ElabFrame.v)).
- Actual context formation, source type reading, many expression and binding
  cases, ordinary and demanded calls, family selection/application, and
  generated lambda/provider cases. These feed the remaining body-proof
  dispatcher in [`BodyPending.v`](theories/BodyPending.v); they do not yet
  establish soundness for every accepted bundle.
- All method bindings follow from actual elaborator execution, including
  anonymous providers, attached named methods, plain-function receiver
  wrappers, and explicit left-hand type brackets
  ([`BindMethodExec.v`](theories/BindMethodExec.v)).
  [`BodyPending.v`](theories/BodyPending.v) excludes method bindings from
  its remaining callback; non-method provider cases remain open.
- Fresh linked-functor execution yields its input contexts, capture checks,
  typed body and actual emission receipts. The assembler still requires four
  explicit result-stage coherence conditions
  ([`LinkBodySound.v`](theories/LinkBodySound.v)). The target result context
  and output-key validity are proved from execution
  ([`FunctorAbstractBodyContext.v`](theories/FunctorAbstractBodyContext.v),
  [`LinkBodyResultView.v`](theories/LinkBodyResultView.v),
  [`LinkOutputKeys.v`](theories/LinkOutputKeys.v)); these component proofs do
  not discharge all four conditions.
- Actual source Bool selection for every successful elaboration, including
  the absence case, with no pending body-soundness premises
  ([`MainProgramBool.v`](theories/MainProgramBool.v)).
- Completed source-function body proofs under explicit body-soundness and
  final-state premises ([`FnElabExec.v`](theories/FnElabExec.v)), whole-run
  generated-function frame receipts
  ([`FunctionFrameProgram.v`](theories/FunctionFrameProgram.v)), and the
  actual main-program phase trace
  ([`MainProgramTrace.v`](theories/MainProgramTrace.v)).
- Core type safety, MossX type safety including the concrete heap, and
  correctness of the optimized interpreter engines, subject to each theorem's
  stated premises ([`CoreMoss.v`](theories/CoreMoss.v),
  [`XSafety.v`](theories/XSafety.v), [`XHeapC.v`](theories/XHeapC.v),
  [`CloX.v`](theories/CloX.v)).
- Exact preparation and reuse of nonallocating scalar-family results in the
  closure engine, retaining type arguments, result order and fuel behavior
  ([`CloScalar.v`](theories/CloScalar.v)).
- Deferred runtime type substitutions and split captured environments,
  with exact interpreter and closure-compiler agreement at every fuel
  ([`LazyEnvFacts.v`](theories/LazyEnvFacts.v),
  [`LazyInterpFacts.v`](theories/LazyInterpFacts.v),
  [`LazyCloFacts.v`](theories/LazyCloFacts.v)), bound to the direct pipeline
  contract in [`LazyOracle.v`](theories/LazyOracle.v); available as
  `MOSS_ENGINE=lazy`.
- Narrowing nonallocating primitive families with no type results while
  preserving runtime type meanings and checked GC casts
  ([`FastPath.v`](theories/FastPath.v),
  [`FastPathExamples.v`](theories/FastPathExamples.v)).

“Checked” here describes proved component theorems, including their explicit
premises. It does not assert that every premise has been discharged for every
successful elaboration, or record a fresh full-build result for this checkout.

## Open end-to-end obligations

1. Close the remaining provider/binding cases in `BodyPending`, then assemble
   the actual common world, final-state facts, all function bodies and main
   environment into `elaborate_sound` for the raw source elaboration relation.
   The existing function theorem still takes `Pending` and `Later` as inputs.
2. Prove that every successful `elaborate_with_metadata` run produces a
   successful immutable lowering with a well-typed runtime program. This
   includes the actual context/profile construction, callable interfaces,
   traversal typing and initial environment; executable candidate tests do
   not supply these universal proofs.
3. Bind the completed source and runtime theorems to the final public
   contract and executable path, and rerun the coherent proof/assumption and
   regression checks.

[`ReifyProgram.v`](theories/ReifyProgram.v) defines the staged runtime
candidate. It is deliberately separate from the production driver.
[`XElabTyped.v`](theories/XElabTyped.v) proves that a program *with* a runtime
typing certificate never produces `XErr`.
`ComponentContracts.SOURCE_RUNTIME_COMPLETE.source_runtime_certificate` states the pending
universal certificate-construction obligation. Consequently, this
checkpoint does **not** prove that every accepted source program avoids
`XErr`. Type safety also permits modeled traps and fuel exhaustion; it is
not a termination or trap-freedom guarantee.

`ComponentContracts.COMPLETE` additionally names elaboration completeness. No completeness
claim is made here; completeness is outside the current soundness milestone.

Attached method-provider selection now uses the canonical receiver in both
native and oracle execution. Named providers reject right-hand applications
rather than silently discarding them. Regression tests cover merged receiver
contexts, ignored application names, and empty applications.

Linked outputs still have an alpha-capture bug: an old merged input
representative can have the same identifier as a newly rebound output name.
In the verified runtime reproducer, native execution prints `yn`, while the
raw, reference and lazy oracle paths get stuck with `XErr 6`. The checked
counterexample in
[`LinkOutputCaptureBoundary.v`](theories/LinkOutputCaptureBoundary.v) records
the conflicting output/body key readings. Fixing theta priority covers one
ordering of the merged inputs; it does not remove this identity collision.

A repair must separate rigid input generations from mutable source names
through body bindings, result headers/refiling, linked/captured satisfaction
maps and formal-versus-actual type arguments. [`Reify.v`](theories/Reify.v)
already distinguishes name and generation maps, but its staged integration
and typing proofs remain open. The four conditions in the fresh-link
assembler remain explicit; the output regression is not treated as proved
safe or rejected as invalid source.

## Trust and validation boundaries

[`Audit.v`](theories/Audit.v) states the syntax, semantics and proof
obligations; the declarative elaboration relation remains a reviewed draft.
The [design decision log](../docs/design/semantics.md) records language
history, and a decision marked “DECIDED” is not a proof-completion marker.

[`check.sh`](check.sh) builds the theories, rejects admitted proofs and audits
the selected exported roots through [`assumptions/`](assumptions). Passing it
checks the implemented contract, not the unimplemented `COMPLETE` obligations.
It allows the stated Rocq kernel primitives; it does not verify Rocq's kernel,
extraction, the OCaml compiler/runtime, or the OCaml driver's file I/O and
lookup-table construction. See [the README's trust discussion](README.md#what-is-trusted).

Native/oracle agreement, self-compilation and candidate-runtime tests provide
useful regression evidence. They do not prove universal source soundness,
compiler equivalence, or correctness of generated WebAssembly. See
[the testing guide](../docs/implementation/testing.md) for the test workflow.
