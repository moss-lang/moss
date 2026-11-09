# roles: Audit change — separate output roles in a linked body's keys

## What
- `functor_code` gains a last field `fc_roles : list (tsym * tsym)`
  (output symbol t, its role gen t). `Platform.Family`'s code: `[]`.
- `LinkBody`: the code's result keys (fc_outputs' items and fc_sources'
  first components) are `rkey_of Cr k` with the roles kept (no
  `output_renaming`); the body side of fc_sources is unchanged;
  `fc_roles = map (fun t => (t, gen t)) outs`.
- `apply_functor` selects
  `result_smap (result_substitution th (role_outputs (fc_roles code) outputs)) Dr (fc_sources code)`
  with `role_outputs roles outputs := map (fun q => (snd q, output_destination outputs (fst q))) roles`
  (`output_destination`: the first destination of t in outputs, t if none).
  Each role gen t reads as t's destination; every raw symbol (an input,
  even one spelled as an output) by θ. `read_result_types` is unchanged.

## Why
results4-AUDIT.md change 2/3: with output_renaming, a key `X.m` that F's
merges spell `T.m` (input T, T also an output) was filed as the output
T's key and installed under the new T, so the caller's `x.m()` missed it.
Same bug as tests/regressions/functor_input_output_generations.moss
(E06), which the project expects to print `yn`.

## Tests / evidence
- `tests/regressions/functor_input_output_generations.moss`: native
  (ob-family build, seed 1h1710x0...mossc.wasm) prints `yn`.
- `bootstrap/examples/output-merge-key.moss`: native prints 7.
- Oracle: unchanged; stuck (XErr 6) on both (results4's runs; E06/E33).
- Audit, at the key level: PublicSavedTypeExample
  `output_role_key_goes_to_destination` (role 9 of output 4 goes to
  destination 6) and `merged_input_key_stays_input` (raw 4 goes to θ(4) = 5).
- Rejects nothing: no new premise, only the application's semantics
  changes (for codes with no roles it is the old selection with no output
  entries, i.e. th; the only codes with roles are linked bodies).

## Proof repair
Mechanical: the restated terms in LinkData, link_sources,
link_source_of, apply_functor_eq (PublicBodySlice), LinkResultsOk,
ChainResultsOk, ChainCoreOk, ChainNewOk, ChainKeysOk, ChainRealizeOk,
FamilyLinkResultsOk, FamilyLinkNewOk; `fc_roles` in the cbn lists and
FunctorCode patterns (PublicCopyApply app_env_thof, PublicPlatformApply,
PublicEntry) and the 8 example codes; PublicClosurePrototype's
compiled_apply mirrors the new selection. All proofs went through
without new reasoning.

## Oracle-relating proofs
None broke. The oracle's runtime is ExecutionModel's (XBApply,
`xinterp_apply`, its own `Elaboration.LinkBody` with its own
`output_renaming`); Audit's `binding`, `functor_code` and interpreter are
not shared with it. LinkBodySound, LinkOutputFresh
(`link_output_renaming_actual`) and ElabSound relate Elab.v's link_body
to `ExecutionModel.Elaboration.LinkBody`, not to Audit's LinkBody, and
all compile unchanged (check.sh green). So decision (a)/(b) did not
arise: no oracle theorem ties the oracle's keys to Audit's. The oracle
remains wrong on this shape: an oracle-conformance gap (E06/E33), as
before. Fixing it means ExecutionModel's functor code and every fast
interpreter, or an oracle-only change in Elab.v link_body plus
xinterp_apply; not attempted.

## Alternative weighed (coordinator, mid-task): install under the caller's keys
Proposal: the application installs each result under the caller's key
for F's declared result (`rkey_of C' k`, k in F's ks), taking the value
from the root body's source for the corresponding declared item. It
would also fix E32 (a copy's root installs extra keys; the strict
two-way CopyOutputs could be reverted) and chainfwd's
linked-copy-bracket.moss (caller key `I32 = Z`, root installs at I32).

Assessment (why roles was done first, not instead):
- Identity of a declared item. The source must be found from F's
  declared item, across copies. Keys at the root's own context conflate
  exactly in E33's shape (input T vs output T) unless roles are kept; a
  context-independent identity would be positional (the i-th item of
  F's ResultKeys), with nth/combine reasoning in every chain proof.
- Copies. F's declared items must be mapped to H's (one per copy step,
  chosen statically from CopyOutputs, like `sms` for needs), so
  `BCopyFunctor` and the `FunctorClosure` value gain a field: ~70
  FunctorClosure sites in the Public proofs and ClosureOk/CopyChain
  invariants, plus BFunCopy's rule.
- BApply gains the caller's key list in every rule that emits it:
  BApplyAbstract, and the family applications in LitOp, Demand and
  Provided (apply_expression) — PublicFamilyStep's proofs with them.
- Platform families: compatible. Their code's source for item k is
  `rkey_of outputs k` itself (sources = identity on Dr), so they need
  only the new identity indexing.
- Oracle proofs: unaffected either way (as above); the oracle would stay
  wrong on all three shapes.
- Size: roughly 2-3x the roles change (which needed no new reasoning).
  Gain: ChainKeysOk / ChainForwardKeys become true by construction (the
  installed keys are the caller's), and E32's converse can be reverted.
Conclusion: the caller-keyed install is the better end state and
subsumes roles (fc_roles would go again), and chainfwd's counterexample
needs a semantic change of that kind anyway (its options b/c). It is not
"no bigger", and it touches closure invariants other agents (chainfwd,
family4) are editing now, so it should be a planned change of its own.
Roles is small, done, and makes the length-zero ChainKeysOk true for
E33's shape meanwhile.
