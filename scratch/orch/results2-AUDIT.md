# Audit changes by the "results2" agent

## 1. result_environment shadows an occupied destination (removes b4f8279's check)

**Prominent note.** This removes a runtime check that the user's commit
b4f8279 ("Prove public loader/world boundaries and preserve type
generations") added: `fresh_result_types` in `result_environment`.
The coordinator approved it as option (A).

**What.** `result_environment caller outputs d3 Dr dr` no longer returns
None when an output's destination already has a meaning in
`type_meanings caller`. When the outputs can be read, it returns
`Some (EnvironmentWithAliases writes aliases (provision_values dr))`, and
the caller appends it in front (`append_environment result d`), so the
writes shadow. `fresh_result_types` stays defined, as the statement of
the freshness the static rules give.

**Why (counterexamples: Audit accepts them, and its semantics was Stuck).**
- bootstrap/examples/const-kept-output.moss. `ExConst` elaborates a
  defined val's initializer in `Cx [] [] [] []`, so every output of a
  `bind F;` in it is installed as itself (destination T). The initializer
  is inlined and evaluated in the caller's environment, where demo's own
  `bind F;` has already given T a meaning, so the result was Stuck.
  - Oracle (`moss_m2.exe`): exit 0. ExecutionModel prepends results
    (`xinterp ... (dr ++ d)`); it has no freshness check.
  - Native (`MOSS_SEED=... moss run`): rejects, "no binding provides this
    item in the current context: `K`". That is unrelated: native does not
    treat defined vals this way.
- bootstrap/examples/lambda-declared-output.moss.
  - The lambda is an argument of a call whose family demand installs
    `T.foo`, with T declared but not in main's table
    (family-unbound-need.moss), so `context_gens` of the lambda's context
    contains T.
  - `lexical_capture` gives the closure a meaning for T (`T |-> TAbs T`).
  - The lambda body's `bind H;` keeps output T, whose destination is T,
    so the result was Stuck.
  - Oracle: exit 0. Native: exit 0.
- No invariant on `Pre` can rule these out, since the runs really were
  Stuck. The slice's const case also needs `Pre` at `Cx []` with the
  caller's environment, so a `MeaningsScoped` bound in `Pre` fails there.

**Safety argument (now in the proof, not at run time).**
- Shadowing a destination g changes `ground d t` only for types t that
  mention g.
- `PublicLinkResults.dest_scope` shows that every destination is out of:
  - the caller's table keys;
  - its `context_gens` (locals, provisions, merges, table values);
  - its frame's return and receiver types.
- `pre_after_result` uses `ground_grow`/`absorbs_grow` on these facts, so
  no typed local, provision, merge or return value is re-grounded, and
  `MeaningsGrow (map gensof outs)` still holds (writes ++ meanings).
- For fresh destinations, these facts come from `FreshFor`.
- For an output installed as itself, `~ In t (context_gens C)` comes from
  the named premise `DeclaredScoped` (declared and mentioned implies a
  table key). family-unbound-need.moss makes that premise false, and
  lambda-declared-output.moss is exactly such a case: T is mentioned
  (`T.foo`) and kept. So this case still needs a decision. Either:
  - add an Audit kept-output clause `~ In t (context_gens C)` (the old
    MadeFor clause, which matched the oracle on the 356-program corpus,
    results-AUDIT.md section 1), which would reject
    lambda-declared-output.moss although both implementations accept it;
    or
  - prove that re-grounding an unbound declared symbol is harmless.

**Dependencies on fresh_result_types (all checked; check.sh passes).**
- PublicGenerationFacts:
  - `result_environment_snapshot` loses its freshness conjunct.
  - `result_environment_preserves_generation` and
    `_preserves_supported_ground` take the freshness as a premise. It is
    the guarantee the static side now gives.
  - `result_environment_rejects_unfresh_snapshot` becomes
    `result_environment_shadows_occupied`.
  - Examples `occupied_destination_is_rejected` and
    `duplicate_destination_is_rejected` become `..._is_shadowed`.
    Duplicates are excluded statically: gensof is injective on outputs
    (`app_outputs_nodup`).
- PublicGenerationCallExample: `output_cannot_overwrite_callers_generation`
  becomes `output_shadows_callers_generation`. The
  "ambient generation does not block" examples (b4f8279's intent:
  activations do not see ambient caller generations) still hold and are
  still checked.
- PublicSavedTypeExample, PublicClosurePrototype and PublicPlatformApply
  (whose `cbn` mentions it) build unchanged.
- PublicLinkResults: `fresh_result_types_ok/_occupied`,
  `result_environment_occupied` and `MeaningsScoped` are removed.
  `result_environment_ok` needs no scope premise.
- AuditExamples and the b4f8279 .moss examples (saved-type-rebinding,
  saved-generic-rebinding, captured-output-scope, captured-type-input)
  do not mention the check. Their intended behavior is about grounding
  and capture boundaries, which are unchanged.

**Alternatives considered.**
- (B) Keep the check. This needs a semantic change for consts (evaluate
  the initializer without type meanings), a frame-dependent
  MeaningsScoped in Pre (the FrameSame problem in results-STATUS), a
  kept-output clause excluding `fr_fresh`, and a formation hypothesis.
  It is much larger, and lambda-declared-output.moss would still need
  rejecting.
