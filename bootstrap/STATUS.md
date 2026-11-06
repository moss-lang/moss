# Verification milestone: partial source and runtime proofs

This checkpoint contains a substantial proof development, not an end-to-end
verified compiler. In particular, **source elaboration soundness and the
universal runtime typing certificate remain open**. The authoritative bound
contract is [`Certified.v`](theories/Certified.v), which implements
`Audit.PROVED_NOW`, `FRONT_END` and `LOADER`; it does not implement
`Audit.COMPLETE`.

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
typing certificate never produces `XErr`; its final comment identifies the
missing universal certificate-construction theorem. Consequently, this
checkpoint does **not** prove that every accepted source program avoids
`XErr`. Type safety also permits modeled traps and fuel exhaustion; it is
not a termination or trap-freedom guarantee.

`Audit.COMPLETE` additionally names elaboration completeness. No completeness
claim is made here; completeness is outside the current soundness milestone.

A known remaining native/oracle discrepancy is attached method-provider
selection for a merged receiver: `expect_provider` still reads its head
before canonicalization, so the oracle can reject a native-accepted program.
The unfinished correction is outside this checkpoint.

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
