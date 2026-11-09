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

## FamilyLinkNewOk: in progress

### Proved so far

- **theories/PublicLinkWalk.v.** `family_binds_no_dests` (with
  `items_applies`, `bindsof_applies`): for a family body,
  `made = []`, `binding_dests binds = []`, and every BApply has
  `outs = []`.
- **theories/PublicLinkScope.v.** `items_scope` (with `step_provide`,
  `step_set_fun`, `step_trans`, `provide_set_in`): every provision a run
  of bind items adds has its gens and key gens in `scope_gens` of the
  result; scope grows only by `made`; c_sub is unchanged.
  - Unlike PublicScoping's GenScoped, there is no declared-symbol
    escape.
  - Premises: UnitsBare and FamilySigReadOffOk (for binditem_ok).
- **Statement change (no weakening of the top-level theorem).**
  `FS.FamilyLinkResultsOk` and `CC.FamilyLinkNewOk` gained
  `PublicFormation.TysScoped W C`, as `FamilyPlatformApplyOk` already
  has. `family_run` supplies it. It is needed for
  family_items_pos/agree's KeysT (PublicFormation.family_keys_typed).

### Remaining: the main proof

Planned file theories/PublicFamilyLink.v. Premises: WorldOf, MergeFormed
and MentionsTyped, all available in PublicEntry. Steps:

1. **outs' = [].** From LD's OutsR and family_no_outputs.
2. **Recover the full LinkBody.** Use closure_cases on Ok: the copy and
   platform branches are impossible by the closure's shape. Match its
   funbody_of with LD's FB to get c_ty Cg and binds0. This gives
   FamilyBindings binds0 and cap3b's strong input condition.
3. **The run fact.** `family_binds_no_dests` on LD's Items/BO and
   `bindings_writes` give `type_meanings d3 = type_meanings de`. Compute
   de's meanings as link_entry does (Mde/MeanL). Then, for x in dom Cg
   and dom Cu, x's meaning is ground d (θ (c_ty Cg x)) = ground d (θ x):
   cap3b's condition, plus PublicFamilyAgree.th_sigma with the
   FamilyApplication parts.
4. **Positions.**
   - PublicFamilyAgree.family_items_pos and family_items_agree, with
     outputs := Cr' and keys := ks'. PRf is LD's PR' with outs = [].
   - KeysM: ks' are methods, from family_news's NewM via
     provide_result_kinds/same_kind_keys. DKMeth comes from
     PublicFamilyCoherent.family_member (WorldOf).
   - KeysT: family_keys_typed with Ts.
   - Selection: PublicCallerKeys.selection_at and selection_last, as in
     PublicPlatformApply.family_platform_ok.
5. **The body.** body_provides at `A.link_site Cg Cb []` (Real3 by
   conversion; RR) gives pv at the source with BodyReal.
6. **R2: gens ⊆ dom Cg.** The gens of gf's types and of rf lie in dom Cg.
   - From RR at key1: the signatures are equal under c_sub Cb = c_sub
     Cg, which is scoped in dom Cg.
   - The actual provision is Cg's (functx_scoped:
     provision_sig_gens/key_gens ⊆ dom Cg), or new, with gens in
     scope_gens Cb ⊆ scope_gens Cg (items_scope, made = []) ⊆ dom Cg.
   - For the receiver, use gens_keep: a y outside dom Cg survives
     c_ty Cg and canon Cb.
7. **Transfer.** callable_transfer from d3 to d:
   - sig types: ground d3 tf = ground d (θ tf) = ground d tc (SigAgree,
     Absorbs at C0);
   - receiver: Absorbs Cb, then step 3 on app_subst (c_ty Cg) rf.
8. **PublicEntry.** Instantiate the new theorem, drop CC.FamilyLinkNewOk
   from safety_obligations, and add the file to Certificate roots.

## CF.ChainForwardCopySigs

Not started; after FamilyLinkNewOk. copyfresh (CopyOutputs) is merged.

## Time

About 1.5 hours.
