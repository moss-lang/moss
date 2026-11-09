# linknewpf: status

Branch ob-linknewpf.

## Done

- **Audit change, 94d89ce (merged into safety):** `A.link_site`. LinkBody
  reads result keys with the inputs at G's own table. Details:
  scratch/orch/linknewpf-AUDIT.md and a SAFETY-SITES Observation.
  - Counterexample: bootstrap/examples/link-merge-rep-rebind.moss.
  - Companion: link-merge-rep-rebind-both.moss.
- **Corpus comparison, 17c5108:** scratch/linknewpf/cmp.sh and cmp.out;
  the analysis is in the AUDIT file.
- **Lemmas (PublicBodySlice):** `realizes_link_site`,
  `rkey_of_link_site` and `canon_link_site`.
- `BodyReal` and `body_provides` in PublicChainCore are unchanged. They
  apply at `A.link_site Cg Cb outs'`, since Realizes at Cb converts to
  Realizes at the site.

## Suspected hole for realizepf (ChainRealizeOk, non-family linked root)

scratch/linknewpf/m7-kept-output-receiver.moss. This is not checked
against Audit in full; I read the rules by hand.
- F: T -> T.m, done. G does not take T. G's body applies Make, whose
  output T is not in force in G, so the application installs T as
  itself. Make provides T.flipit = Bool.flip.
- G then binds T.m = () => { this.flipit(); }. F's key T.m is read at
  G's site as T.m (T is not in G's table), so the body's T.m is found.
- The caller files that closure under its own T.m (Concrete). So
  Concrete.m() would run Bool.flip on a Concrete.
- The pins only stop `bind T`; they do not stop an application's output.
- Oracle: accepts it and runs to exit 0. Native: rejects ("compilation
  failed: `T`").
- The hole predates link_site: at_site Cb read it the same way.
- It cannot occur for a family body, since FamilyBindings allows only
  family applications, and families have no outputs.

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
