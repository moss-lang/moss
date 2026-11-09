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

## FamilyLinkNewOk: proved

`CC.FamilyLinkNewOk` is no longer in `safety_obligations`.

- **theories/PublicFamilyLink.v**, module `FamilyLink`. It is now the
  root instance: PublicEntry takes `CF := FL.CF`.
  - `family_link_new_ok : CC.FamilyLinkNewOk`, under the premises
    WorldOf, MergeFormed and MentionsTyped. PublicEntry discharges all
    three.
  - Helpers: `link_body_parts` (a LinkBody's FunCtx, cap3b's input
    condition, FamilyBindings and funbody_of), `functx_table_in` (a
    FunCtx table entry is `σ (TAbs x)`), `sig_subst_gens`,
    `forall2_cons_inv`, `subst_nil_item`.
- **theories/PublicLinkWalk.v.** `family_binds_no_dests`, with
  `items_applies` and `bindsof_applies`: for a family body,
  `made = []`, `binding_dests binds = []`, and every BApply has
  `outs = []`.
- **theories/PublicLinkScope.v.** `items_scope`, with `step_provide`,
  `step_set_fun`, `step_trans` and `provide_set_in`:
  - every provision a run of bind items adds has its gens and key gens
    in `scope_gens` of the result;
  - scope grows only by `made`;
  - c_sub is unchanged;
  - unlike PublicScoping's GenScoped, there is no declared-symbol
    escape;
  - premises: UnitsBare and FamilySigReadOffOk.
- **Statement change (the top-level assumptions only shrink).**
  `FS.FamilyLinkResultsOk` and `CC.FamilyLinkNewOk` gained
  `PublicFormation.TysScoped W C`, as `FamilyPlatformApplyOk` already
  has. `family_run` supplies it. It is needed for KeysT in
  family_items_pos/agree.
- **Proof shape** (realize2 may reuse it):
  1. The run fact `RunFact`: ground d3 t = ground d (θ t) for t over
     dom Cg ∩ dom Cu, from the application environment's meanings
     (Mde), cap3b's condition and PublicFamilyAgree.th_sigma.
  2. Positions: family_items_pos and family_items_agree at
     outputs := Cr', keys := ks'.
  3. body_provides at `A.link_site Cg Cb []`.
  4. R2, F's signature and receiver gens ⊆ dom Cg: ResultRealizes
     with items_scope and functx_scoped.
  5. callable_transfer.

## CF.ChainForwardCopySigs: not started (handoff)

I stopped here to keep a clean, compiling handoff; my context is large.

Pointers for whoever continues:
- **Statement.** PublicChainFwd.v, `ChainForwardCopySigs` (sms <> []).
  The empty chain is `chain_forward_sigs`, from
  PublicFamilyAgree.items_agree_gen and items_agree_pos.
- **Argument.** moss-safety scratch/hunt/hunt2-chain/REPORT.md:
  - a platform root's polymorphic inputs all sit in receivers (keys);
  - everything else H mentions is fixed in H, hence in F;
  - it is checked by FixedAgreement at the copy site and pinned at the
    application.
- **Copy-site facts.** These are in `CCStep`/`CopySite` (PublicCopyApply,
  PublicBodySlice). CopyOutputs now compares after
  `sigma ++ [y := cg y]` (fresh cg avoiding sigma's image).
  PublicCopyFresh.v's `copy_outputs_at` / `copy_provision_read` read that
  agreement back at a substitution. Use them to step SigAgreeG down the
  chain.
- **Shape.** Mirror PublicPlatformApply.family_platform_ok's
  selection/position argument, at each CCStep. The root is a platform
  family (FamilyData), where SigAgreeG is items_agree_gen at the root's
  context.

## Time

About 5.5 hours in total.
