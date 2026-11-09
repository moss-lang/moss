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


## HANDOFF BRIEF (for a fresh agent; branch ob-results = safety 5522b69)

The previous agent's context was too full to continue well. The lead decided:
(1) put the frame-free MeaningsScoped into the slice's Pre, with
DeclaredScopedOk as a named slice hypothesis (a plain fact about C);
(2) assemble ChainCoreOk so ChainResultsOk (PublicCopyApply.v) is proved;
(3) then FamilyLinkResultsOk (PublicFamilyStep.v) if possible.
Rules: ORCH-RULES.md (name "results"); check.sh green before each commit;
merge `safety` before reporting.

### Step 1: MeaningsScoped in Pre (PublicBodySlice.v)
- Definition (no frame argument, so FrameSame/codesafe_frame/pre_frame stay
  untouched):
    MeaningsScoped C d := forall x, lookup_subst x (type_meanings d) <> None ->
      In x (map fst (A.c_ty C)) \/ (A.Fresh W x /\ In x (tm_gens (A.c_ty C)))
  (tm_gens = flat_map (gens o snd); maybe use the c_ty part of context_gens,
  i.e. gens v ++ gens (canon C v), if canon is a problem at output_cx).
- Hypothesis (approved): DeclaredScopedOk := forall C, FormedCx W C ->
  forall x, In x (A.context_gens C) -> A.Fresh W x \/ In x (map fst (A.c_ty C)).
- Append `/\ MeaningsScoped C d` to Pre (after Formed F C). The reverted
  attempt (scratch/orch/results/ms-in-pre-attempt.diff) used an F-dependent
  version but shows the mechanical edits: destruct patterns
  `[Real Fmd]]]]]` -> `[Real [Fmd Ms]]]]]]`, the regex
  `\[(\w+) (Fm\w*)\]\]\]\]\]` -> `[\1 [\2 _]]]]]]`, the `conj ... Fmd`
  constructions in pre_lower/pre_le/pre_after/realizes_pre_lower, pre_keep.
  Helper lemmas worth reusing from it: MeaningsKeyed C d (meanings only on
  keys), ms_keyed, keyed_callee (callee_env over θ with θ keys = table keys),
  ms_same/ms_keep (same meanings, table keys grow).
- Entries: body_call/body_call_method get `MeaningsKeyed Cf de` (callers pass
  keyed_callee ... (proj1 CT), or theta_ctx keys at the wrapper, line with
  `RealN Sok eq_refl`); app_step PreB and PublicCopyApply/PublicFamilyStep
  PreB: application_environment meanings are over fc_inputs = keys of Cg
  (same proof as their KeysMeant subgoal); main: InitialTypes (keys);
  lambda_callable: captured meanings are over context_gens C, so each is a
  key of Cl (= keys of C) or Fresh (DeclaredScopedOk C) and then it must be
  in Cl's table values: needs "a Fresh x in context_gens C is in tm_gens
  (c_ty C)" — either strengthen MeaningsScoped's second disjunct to
  context_gens of the *table part* and prove fresh gens only enter through
  output destinations (t |-> TAbs g), or add that to DeclaredScopedOk
  (it is also a formation fact: fresh generations a context mentions occur
  in its table). Decide and state it as a plain fact about C.
- Steps: binds keep d's meanings (install/bind_type: aliases only) and the
  table keys only grow (set_ty conses; output_cx keys = outs ++ keys C;
  provide/set_* keep c_ty); demand_case uses with_ty C0 C (C0's table);
  applications: the result writes gensof t: kept t is a key of C'
  (provide_result_shape), fresh g occurs as TAbs g in C''s table.
- PublicLinkResults.v currently takes MeaningsScoped as an extra premise
  (with an fr_fresh-based definition and DeclaredScoped inside); rewrite it
  to the Pre version: dest_scope's freshness then uses "kept t: not a key,
  not Fresh (funoutputs_declared); fresh g: Fresh but FreshFor puts it
  outside context_gens C ⊇ table values". Remove pre_after_result's
  MeaningsScoped premise; it must now prove MeaningsScoped C' d' (writes as
  above).

### Step 2: ChainCoreOk (PublicLinkResults.v, Section Assemble)
Goal: prove ChainCoreOk, then `CA.ChainResultsOk W Prog argv read state_ok`
from chain_results_scoped (MeaningsScoped then comes from Pre).
Needed pieces, all under ChainResultsOk's premises (LD = S.LinkData of the
root h0; Pre3 = Pre at Cb, d3; RR = ResultRealizes Cb Cr (gen t ↦ TAbs t) ks'
with Audit's new read_rename comparison):
- Selection: `select_provisions d3 (result_smap θ' Dr (S.link_sources Cb outs'
  gen Cr ks')) = Some dr` — each source is the body key
  rkey_of Cb (at_site Cb (output_renaming outs' gen k)); Pre3's EnvOk gives a
  provision there (RR gives the Cb provision; realized_key-style lemma).
  Use PublicCallRealize.select_ok; read result_smap/link_sources defs in
  Audit.v/PublicBodySlice.v.
- Outputs readable: `output_meaning d3 t <> None` from Pre3's (strengthened)
  AliasOk and LD's OutM (outputs are keys of c_ty Cb).
- C' facts at d' = append (result_of d3 outs gensof dr) d:
  * KeysMatch C': new provisions come from entry_provision (kinds match).
  * AliasOk C': outputs t: aliases of result give read_meaning d3 t =
    ground d' (TAbs (gensof t)) (writes first); other keys: as in d
    (aliases/meanings unchanged for them; they avoid the destinations by
    dest_scope).
  * EnvOk C' (the core): for a new provision k_c of C' (caller
    ProvideResult at output_type_map C outs (TAbs∘gensof)), relate to the
    formal entry of Cr (ProvideResult at output_type_map Cu outs (TAbs∘gen),
    and through the copy chain: CopyChain's CCStep carries CopyOutputs
    (h's result read at u's table gives each of u's keys the same
    provision) and FunOutputs equality) with
    PublicOutputSubst.provide_result_rel; s1/s2 chosen so that Agree holds:
    s2 = type_meanings d' (caller), s1 = (gen t ↦ read_meaning d3 t) then
    θ grounded in d (inputs). The runtime value at dr's key is d3's value at
    the body key, typed by Pre3 at ground d3 (Cb type); RR says the Cb type
    equals canon Cb of the promised type under read_rename; ground d3 of
    that equals ground d' of the caller's type (inputs: d3 reads linked
    inputs as ground d (θ x) via application_environment; outputs: alias
    read_meaning). Old provisions of C: lookups in d' fall through dr unless
    a dr key collides; collisions only at keys C' shadows statically
    (key correspondence via provide_result_rel's EntryRel).
  This is the largest piece; expect several hundred lines.

### Step 3: FamilyLinkResultsOk (PublicFamilyStep.v, line ~276)
Same as LinkResultsOk with a FamilyApplication instead of AppPremises, no
type outputs (`result_substitution th []`), conclusion FamilyNewOk.
Reuse the EnvOk correspondence of step 2 with outs = [].
