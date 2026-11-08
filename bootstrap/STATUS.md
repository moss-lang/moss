# Verification milestone: partial source and runtime proofs

The [proof roadmap](PROOFS.md) shows the five public targets as terminal
nodes, distinguishes checked components from partial and open tasks, and
links each task to its evidence or finish condition.

The [first public foundation checkpoint](PUBLIC-FOUNDATION.md) implements
finite public source snapshots, proves direct type-emission/capture facts,
and checks a partial public-syntax closure compiler with a concrete table.
The production interpreter is unchanged. Its default configuration passed
bootstrap in 38m08s; removing either optional transformation exceeded the
one-hour cap. The public elaborator/interpreter port and all five public
obligations remain open.

This checkpoint contains a substantial proof development, not an end-to-end
verified compiler. In particular, **source elaboration soundness and the
universal runtime typing certificate remain open**. The public contract is
[`Audit.Public.CORRECT`](theories/Audit.v): build soundness and completeness,
run soundness and completeness, and safety. These five end-to-end
obligations remain pending. Audit.v defines Builds, Runs and their semantic
dependencies directly. Building also promises compatibility with the fixed
default interface; execution and safety quantify over any compatible interface.
[DefaultRuntime.v](theories/DefaultRuntime.v) proves the concrete runtime's
primitive and heap laws for the internal `ExecutionModel`. The revised public
semantics has explicit structural records and machine types, separate runtime
type/provision environments, one binding grammar, exact literal expansions,
checked representation metadata, lexical capture, and stronger freshness/result
contracts. Existing component proofs are preserved against ExecutionModel;
they are not automatically proofs of the revised Audit model.

[AuditRepresentation.v](theories/AuditRepresentation.v) defines type/value
representation relations and names the pending elaboration-refinement property.
The source grammar and module-loading relations are shared definitionally, so
the existing front-end proofs still apply. Program, primitive-host and execution
refinement remain open. No additional axiom asserts those refinements.
[AuditBuildExample.v](theories/AuditBuildExample.v) proves one complete public
build/run example; [AuditExamples.v](theories/AuditExamples.v) checks the repaired
semantic boundaries. Neither constitutes universal source soundness.

Bare `return;` now requires a unit return type in the model, elaborator and
self-hosted compiler. A never-returning function cannot return unit and then
supply it to an empty match; `tests/errors/return_never.moss` covers this bug.

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
  its remaining callback. Successful remaining non-method bindings are
  characterized as abstract-to-defined functor closures
  ([`FunctorRemainingDispatch.v`](theories/FunctorRemainingDispatch.v));
  their complete soundness proof remains open.
- Fresh linked-functor execution yields its input contexts, capture checks,
  typed body and actual emission receipts. The assembler still requires four
  explicit result-stage coherence conditions
  ([`LinkBodySound.v`](theories/LinkBodySound.v)). The target result context
  and output-key validity are proved from execution
  ([`FunctorAbstractBodyContext.v`](theories/FunctorAbstractBodyContext.v),
  [`LinkBodyResultView.v`](theories/LinkBodyResultView.v),
  [`LinkOutputKeys.v`](theories/LinkOutputKeys.v)); these component proofs do
  not discharge all four conditions.
- Actual whole-build execution preserves authenticated linked-code cache
  creators and final functor rows
  ([`LinkProgramPreserve.v`](theories/LinkProgramPreserve.v),
  [`FunctorRowsProgram.v`](theories/FunctorRowsProgram.v)). These are physical
  provenance facts; semantic agreement for cached linked bodies still needs
  to be assembled with the fresh-body proof.
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
   environment into `elaborate_sound` for ExecutionModel’s raw source elaboration relation.
   The existing function theorem still takes `Pending` and `Later` as inputs.
2. Prove that every successful `elaborate_with_metadata` run produces a
   successful immutable lowering with a well-typed runtime program. This
   includes the actual context/profile construction, callable interfaces,
   traversal typing and initial environment; executable candidate tests do
   not supply these universal proofs.
3. Prove program and execution refinement from ExecutionModel to Audit, bind
   the completed results to the five public obligations, and rerun the coherent proof/assumption and
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
In the [runtime reproducer](../tests/regressions/functor_input_output_generations.moss),
native execution prints `yn`. After the theta-priority correction, the oracle
prints `n` for the first call and then gets stuck with `XErr 6`. The expected
oracle behavior is recorded in a newly ignored regression test; the native
control remains enabled. The checked
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

Private input preparation now has checked components:
[`InputGenerationScope.v`](theories/InputGenerationScope.v) snapshots the
input tables while retaining source names and argument labels;
[`InputGenerationMerge.v`](theories/InputGenerationMerge.v) preserves the
source merge action in private generation space, including after a source
name is rebound. [`InputGenerationFresh.v`](theories/InputGenerationFresh.v),
[`InputGenerationPlan.v`](theories/InputGenerationPlan.v), and
[`InputGenerationAllocate.v`](theories/InputGenerationAllocate.v) connect a
finite, injective scope to actual symbol allocations and an inverse reading.
The support includes method receivers appearing only in application records.
[`InputGenerationRuntime.v`](theories/InputGenerationRuntime.v) specifies
input, capture and source-name transfers and proves that old input arguments
and captured meanings survive shadowing. These preparation steps are not yet
called by `link_body`; the alpha-capture regression above remains open.

A separate runtime output transfer is implemented: `xresult_env` now exports
each result type's actual meaning under the caller's fresh generation as well
as its source name. Previously, a functor taking and replacing `Fresh` could
produce an I32, but boxing it after application used the old or unresolved
type and a valid dereference trapped. The native compiler and all oracle
engines now pass the
[GC regression](../tests/regressions/functor_output_generation_cast.moss).
[`InputGenerationExports.v`](theories/InputGenerationExports.v) proves the
runtime generation reading. The optimized engines retain their agreement
proofs; core safety derives the empty-transfer case from its existing fresh
input/output separation and has no additional assumption.

## Trust and validation boundaries

[`Audit.v`](theories/Audit.v) states the syntax, semantics and proof
obligations; the declarative elaboration relation remains a draft;
existing component results concern ExecutionModel unless stated otherwise.
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
