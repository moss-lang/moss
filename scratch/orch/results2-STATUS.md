# results2: MeaningsScoped / ChainCoreOk / FamilyLinkResultsOk — STATUS

Branch ob-results2. safety is merged. check.sh passed at 712aecd. Time:
about 3 hours.

## For family2: no formation hypothesis from results2

`DeclaredScopedOk` (and `MeaningsScoped` in `Pre`) are **no longer
needed**. The coordinator chose option (A): Audit's `result_environment`
now shadows an occupied destination instead of being Stuck (commit
712aecd, scratch/orch/results2-AUDIT.md). Nothing in `Pre` or `FormedCx`
needs to bound the environment's meanings any more.

## Done

- **Audit change (712aecd):** dropped b4f8279's `fresh_result_types`
  check in `result_environment`.
  - Counterexamples: bootstrap/examples/const-kept-output.moss and
    lambda-declared-output.moss. Audit accepted both and its semantics was
    Stuck. The oracle runs both, and native runs the lambda one.
  - PublicGenerationFacts and PublicGenerationCallExample are restated to
    check shadowing, with generation preservation kept under the
    statically given freshness.
  - Recorded in SAFETY-SITES (Observations, first entry; stuck-site table)
    and results2-AUDIT.md.
- **PublicLinkResults:**
  - `MeaningsScoped` is removed.
  - `dest_scope`: destinations are out of the caller's table keys,
    `context_gens` and frame types.
  - `result_environment_ok` needs no scope premise.
  - `pre_after_result` builds Pre after the result from the caller's Pre.
  - `chain_results_scoped : ChainCoreOk -> (forall C, FormedCx W C ->
    DeclaredScoped W C) -> CA.ChainResultsOk ...`
  - `DeclaredScoped C := forall x, In x (context_gens C) -> Fresh W x \/
    In x (map fst (c_ty C))`. It is used only by `dest_scope`, for an
    output installed as itself.

## Open issue (needs a decision): kept outputs mentioned by the context

`dest_scope` needs a kept output T (installed as itself) to be out of
`context_gens C`. Otherwise the result's write `T |-> m` re-grounds types
the context mentions, such as a provision key or signature over T. Its
premise `DeclaredScoped` is false after family-unbound-need.moss, and
lambda-declared-output.moss is a concrete accepted program where T is
mentioned (via `T.foo`) and kept. Options:
1. An Audit kept-output clause `~ In t (A.context_gens C)` in
   `BApplyAbstract` (the old MadeFor clause, which matched the oracle on
   the corpus). It would reject lambda-declared-output.moss, which both
   implementations accept.
2. Prove that re-grounding an unbound declared symbol is harmless.
   Before the write, `ground d (TAbs T) = TAbs T`. A value can have a type
   mentioning an abstract T only through provisions whose `CallableOk`
   is stated at grounded types, so this is not obvious.
Until then, `ChainResultsOk` from `ChainCoreOk` carries this premise.

Variants, run on the oracle and native (results2-AUDIT.md section 2):
- (i)+(ii) (kept-output-family-method.moss): both accept and run it
  correctly.
- (iii) (kept-output-captured-fn.moss): both reject it statically.

There is no implementation bug. Re-grounding is harmless because an
unbound declared symbol is mentioned only by parametric family method
provisions. Proving that is large (a parametricity argument for
CallableOk). The cheap alternative is the Audit clause
`~ In t (context_gens C)`, which rejects programs that both
implementations accept and run correctly.

## Not done

- **ChainCoreOk** (PublicLinkResults, Section Assemble) is not started.
  The predecessor's plan (results-STATUS.md, "Step 2") still applies
  unchanged, minus anything about MeaningsScoped. Pieces:
  - selection from body keys: `link_source_of`, `select_ok`,
    `realized_key` at Cb;
  - outputs readable: Pre3's AliasOk plus LinkData's OutM;
  - KeysMatch/AliasOk/EnvOk of C' at `append (result_of d3 outs gensof dr) d`;
  - for EnvOk, `PublicOutputSubst.provide_result_rel` with
    s1 = body meanings after `read_rename` and s2 = caller meanings;
  - the copy chain's CopyOutputs.

  Expect several hundred lines.
- **FamilyLinkResultsOk**: not attempted. It is the outs = [] instance of
  the same correspondence.
- safety_obligations is unchanged: ChainResultsOk and FamilyLinkResultsOk
  are still obligations.

## Files

- scratch/orch/results2/ms-frame-free-wip.diff: the abandoned frame-free
  MeaningsScoped edit of PublicBodySlice. Obsolete under (A), and kept for
  reference only.
