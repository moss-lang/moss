# results2: MeaningsScoped / ChainCoreOk / FamilyLinkResultsOk — STATUS (in progress)

Branch ob-results2 (safety 68900a4 merged).

## Formation hypothesis (candidate statement, for family2)

Only needed if Audit keeps `fresh_result_types` (option B below):

    Definition table_gens (C : A.cx) : list nat :=
      flat_map (fun q => A.gens (snd q) ++ A.gens (A.canon C (snd q))) (A.c_ty C).
    Definition DeclaredScoped (C : A.cx) : Prop :=
      forall x, In x (A.context_gens C) ->
        In x (map fst (A.c_ty C)) \/ (A.Fresh W x /\ In x (table_gens C)) \/
        A.kind_sym W x = Some A.DKType.
    Definition DeclaredScopedOk : Prop :=
      forall C, PublicFormation.FormedCx W C -> DeclaredScoped C.

That is, a generation a formed context mentions is a table key, a fresh
generation that the table mentions, or a declared type symbol (option (b)).
The middle disjunct is what a lambda needs to capture fresh generations
without a frame.

## Blocker found: Audit's result freshness check (needs a lead decision)

`result_environment` is Stuck when an output destination already has a
meaning in the caller's environment (`fresh_result_types`, b4f8279).
ExecutionModel/the oracle shadows instead (`dr ++ d`). Two kinds of
accepted programs reach the check with an occupied destination, so no
invariant in `Pre` can help:

1. Constants. `ExConst` elaborates the initializer in `Cx [] [] [] []` with
   `fr_fresh F0 = []`, so every output of a `bind F;` in it is installed as
   itself (destination T). The initializer is inlined and evaluated in the
   caller's environment. If the caller has made T, the run is Stuck.
   Example: scratch/orch/results2/const-bind.moss (oracle: accepted, runs,
   exit 0). The slice's const case also needs Pre at `Cx []` with the
   caller's d, so any frame-free MeaningsScoped fails there.
2. Lambdas, under option (b). `capture_meanings` captures every x in
   `context_gens C`, including a declared non-key T (`T |-> TAbs T`). A
   kept output T in the lambda body is then Stuck, unless the kept-output
   clause also excludes `fr_fresh F`. The lambda rule puts
   `context_gens C`, declared symbols included, into `fr_fresh Fl`, so
   `fr_fresh` does not hold only fresh symbols. That makes
   MeaningsScoped frame-dependent.

Options sent to the coordinator:
- (A) Drop the freshness check (mirrors the oracle). MeaningsScoped and
  DeclaredScopedOk become unnecessary.
- (B) Keep it. This needs a const semantics change, a frame-dependent
  MeaningsScoped, and the hypothesis above.

## Work in progress

- scratch/orch/results2/ms-frame-free-wip.diff: frame-free MeaningsScoped
  in Pre, about 90% through PublicBodySlice. It stops at the const case
  (blocker 1).
