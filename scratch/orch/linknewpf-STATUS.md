# linknewpf: status

Branch ob-linknewpf, from safety e69a7cd, fast-forwarded to safety 460a720
(copyfresh). No Rocq file changed; check.sh
not rerun (only a .moss example and scratch files are added).

## Stopped: CC.FamilyLinkNewOk is false for an Audit-accepted program

Counterexample: bootstrap/examples/link-merge-rep-rebind.moss (header has
the full walk). Scratch variants: scratch/linknewpf/m2-*.moss, m5-*.moss,
m3-rebind-unmentioned-input.moss (control), run.sh.

- F[X]: Merge -> X.m, where Merge merges X with T (A.gimme[Foo=X] and
  A.gimme[Foo=T]). G: Merge -> X.m { bind T = Bool; bind T.m = ... }.
- Audit's world is a witness, and w_context is unconstrained by WorldOf,
  so the merge representative is free. Take T for both F and G.
- F's key X.m reads at Cu as T.m. M(F) = {X}, so G's pins (3d) are {X},
  and `bind T = Bool` is allowed.
- LinkBody's ResultRealizes (and the code's sources, link_sources) look
  the key up at `at_site Cb (T.m)`, which re-reads T through G's final
  table: Bool.m. G's Bool.m (`if this {}`) is the result source.
- main demands Concrete.m (X := Concrete; T = Concrete there, so the
  merge holds). The install puts the Bool.m closure under the caller's
  Concrete.m key, and `if this` runs on a Concrete: stuck.
- Both implementations reject it. The oracle says "functor `G` does not
  bind `X.m`"; native says "the functor does not bind every item its
  result promises: `m`".
- Why: a linked input's run-time meaning is the caller's (the
  application environment), but `bind T = ...` writes only an alias, and
  at_site reads the receiver through the table (aliases). Signatures are
  compared under c_sub only, so they are not affected. Only receivers are.

The same hole should affect CN.ChainRealizeOk for a non-family linked
root, since it uses the same ResultRealizes and link_sources. I did not
build a test for that case.

### Fix options (for the coordinator / user)

(a) Pins include the merge representatives of M(F): x is pinned iff
    Mentions W u x, or x is in gens (canon Cu (TAbs y)) for some
    mentioned y.
    - This mirrors the oracle. On m5-XT (G also binds X.m), the oracle
      says "`bind T` ... changes a type its result mentions (D84 3d)"
      when its representative is T.
    - Native accepts m5-XT and m5-TX and runs them correctly (exit 0).
      So (a) rejects a program native accepts. The oracle accepts m5-TX,
      whose representative is X, and Audit with representative X still
      accepts it.
    - It is a minimal one-conjunct change to LinkBody's pins clause.
(b) Faithful: read the result key's receiver with the inputs at G's own
    readings (c_ty Cg) and the outputs at the body's (Cb), in both
    LinkBody's ResultRealizes lookup and link_sources.
    - This matches native on m5.
    - It changes the code's sources and LinkData, with proof repair in
      PublicBodySlice, PublicChainCore and PublicLinkResults.
(c) Require that the body does not rebind an input that a result key's
    receiver mentions at Cu. This is close to (a), with the same native
    disagreement on m5-XT.

## Plan for FamilyLinkNewOk once fixed (findings, nothing committed)

Proof outline:
- Positions. FamilyApplication's NoDup means the caller key mkey r m has
  exactly one position i. Use family_items_pos with
  `outputs := Cr'`, `keys := ks'` (LinkData's ProvideResult; F is
  abstract, so `b = None` holds; outs = [] by family_no_outputs, so the
  out map is irrelevant). It gives θ(rkey_of Cr' key_i) = rkey Cr r m.
  PublicCallerKeys.selection_* gives dr's lookup at rkey Cr r m as the
  body's source at position i.
- Body side. body_provides with ren = [] (read_rename Cb [] = [])
  gives pv callable at d3 at (rb :: params gf) -> ret gf, where
  rb = canon Cb (c_ty Cb rf).
- Signature side. family_items_agree gives SigAgree, which is
  canon Cfam tc = canon Cfam (θ tf) with gens tf ⊆ dom Cu. Absorbs at the
  caller then gives ground d tc = ground d (θ tf).
- Run fact. The body's dests are empty: FamilyBindings means every
  BApply is of a family, and a family has no outputs. A static walk over
  BindItems, like PublicPlatformApply's items_np, is needed to see that
  each BApply's outs are its functor's FunOutputs. Then bindings_writes
  gives type_meanings d3 = the application environment's meanings.
  - For x ∈ dom Cu ∩ dom Cg, cap3b's LinkBody condition and θ's merge
    clause (th_sigma) give ground d3 (TAbs x) = ground d (θ x). The
    condition has to be recovered from Ok0's CLink LinkBody, since
    LinkData carries the weaker form.
  - Still needed: every gen of gf's types is in dom Cg. ResultRealizes
    compares under c_sub Cb = c_sub Cg, which is scoped in dom Cg. The
    body's new provisions have gens in scope_gens, never the
    declared-symbol escape that PublicScoping.binditems_scoped allows.
    That needs a tighter BindItems walk.
- Receivers. They also need c_ty Cb y = c_ty Cg y for y ∈ gens rf, so
  that the alias equals the meaning. That is exactly what the
  counterexample breaks, and what the fix must deliver.

## Lemmas for realizepf

No new lemma was proved. BodyReal and body_provides already exist in
PublicChainCore. realizepf should note that the same receiver hole
(above) likely makes ChainRealizeOk false for a linked root with merges
whose body rebinds an unpinned representative.

## CF.ChainForwardCopySigs

Not started. It waits for copyfresh's CopyOutputs change, and I stopped
at the FamilyLinkNewOk finding as instructed.

## Time

About 1.5 hours.
