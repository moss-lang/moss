# results agent: LinkResultsOk / ChainResultsOk (branch ob-results) — STATUS

Branch is merged with `safety` 41baf76; check.sh green.

## Done (committed)
- Audit `ResultRealizes` compares at the body's output meanings
  (`read_rename`, as the oracle's promised_result_type). Before, a body that
  binds `T = Bool` and provides `fn mk(): T` could never satisfy LinkBody.
  Test bootstrap/examples/result-output-meaning.moss (oracle and native
  print 1). Notes: scratch/orch/results-AUDIT.md §2, SAFETY-SITES entry.
- My earlier MadeFor fix is superseded by ob-family's D78 fix and was
  dropped at the merge (AUDIT §1 marked superseded).
- AliasOk strengthened (PublicBodySafety): `nlook x (c_ty C) = Some tx ->
  output_meaning d x = Some (ground d tx)` (lead decision 2). Established at
  every entry via `KeysMeant` (callee envs: θ keys = callee table; application
  envs: fc_inputs = body table; main: InitialTypes; lambdas: aliases of the
  whole table); body_call/body_call_method take `KeysMeant`. Fixed in
  PublicBodySlice, PublicCopyApply, PublicFamilyStep, PublicPlatformApply,
  PublicEntry (entry_pre takes InitialTypes).
- Copy chains keep type outputs: `ClosureOk.CCopy` and `CopyChain.CCStep`
  carry `FunOutputs u <-> FunOutputs h` (from BFunCopy's SameResult,
  `same_result_outputs`). Without it ChainResultsOk was unprovable: the
  root's result telescope must list every output the caller reads.
- PublicLinkResults.v, aimed at ChainResultsOk:
  - `dest_scope`/`result_environment_ok`: destinations are out of scope and
    free in the caller; result_environment = Some (result_of ...).
  - `result_grows_by_destinations`: MeaningsGrow (map gensof outs).
  - `pre_after_result`: Pre after the application from the caller's Pre,
    given AliasOk/EnvOk/KeysMatch of C' at the new environment.
  - `chain_results_scoped : ChainCoreOk -> ChainResultsScopedOk`
    (ChainResultsOk + premise `MeaningsScoped W F C d`).
  - `pre_junk_meaning`, `result_environment_occupied`: Pre alone cannot
    bound the caller's meanings, so some MeaningsScoped-like invariant is
    necessary.
- PublicOutputSubst.v: `output_entries_rel`, `provide_result_rel` (two
  readings of a functor result at Agree-related tables correspond one to
  one; DomGap for receivers present in one base table only). Usable for
  ChainResultAgrees / PlatformResultAgrees.

## Not done
1. MeaningsScoped in Pre (lead decision 1). Attempted (diff saved in
   scratch/orch/results/ms-in-pre-attempt.diff, reverted). Findings:
   - With `MeaningsScoped F C d := meaning x -> key of C \/ (Fresh x /\ In x
     (fr_fresh F))`, entries are easy (callees: `keyed_callee`; application
     env: body table; lambdas: captured gens are fresh (in fr_fresh Fl via
     LambdaProvider) or, being declared, keys — this needs a formation fact
     `DeclaredScoped C` (declared symbols C mentions are keys), which Formed
     does not carry; I'd add it as a *_scoped-style hypothesis like
     ex_scoped, to be discharged by DeclaredInForce).
   - Blocker: the slice reframes code between frames with different
     fr_fresh (codesafe_frame / FrameSame / pre_frame compare only
     ret/this/loop/var). A dependence of Pre on fr_fresh breaks those
     lemmas unless FrameSame also orders fr_fresh, which must be checked at
     every reframing (BindStep's F -> with_fresh F newgens grows; reframes
     back would shrink).
   - An F-independent alternative: `meaning x -> key of C \/ (Fresh x /\ x
     occurs in c_ty C's values)`. Fresh generations enter only as output
     destinations (t ↦ TAbs g in c_ty), c_ty is append-only within a body
     and lambdas keep it, so this should be preserved by every step; the
     kept-output and fresh-destination freshness then follow (kept t: not a
     key, not Fresh; fresh g: FreshFor puts it outside context_gens).
     Needs DeclaredScoped at lambda capture as above.
2. ChainCoreOk (the correspondence): select from body keys (result_smap /
   link_sources), AliasOk/EnvOk/KeysMatch for C'. Tools ready
   (provide_result_rel with s1 = body meanings after read_rename, s2 =
   caller meanings after result_of; AliasOk at Cb now gives outputs
   readable), not assembled.
3. FamilyLinkResultsOk not attempted.

Time: about 7 hours in total.
