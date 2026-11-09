# fwdsigs: status

Branch ob-fwdsigs, created from safety 01e0647 and merged with safety 6dcd915
(realize2's two Audit changes).

## Done

`CF.ChainForwardCopySigs` is proved and no longer in `safety_obligations`.
What `safety_obligations` still assumes: `CN.ChainRealizeOk` and
`FS.FamilyCopyRealizeOk`.

### Audit change (own commit cd1108d; scratch/orch/fwdsigs-AUDIT.md)

`BFunCopy` now requires the copy's target u to be abstract:
`exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)`.
- BFunClosure already has this premise, and the oracle's `bind_functor`
  rejects any target with a body.
- Test: bootstrap/examples/copy-into-body.moss. The oracle rejects it
  ("`F` has a body ... only an abstract functor is bound (D78)"), and so
  does native. The control, scratch/fwdsigs/copy-into-body-ok.moss, prints 7
  on both.
- `S.CopySite` carries the premise as its last conjunct. `FunctorCopyExec`
  discharges it from `gs_decl`.
- Why it is needed: the proof relates each copy's result read in two
  contexts (`entries_rel`), and that asks the result's mentions to be inputs
  (`mentions_dom`). Only an abstract functor guarantees this.

### theories/PublicChainSigs.v (new; a Certificate root)

**Static lemmas:**
- `pos_last`: on two related entry lists (keyed alike, with a transfer
  condition), the last second-side position for a method key is also the
  last first-side position for its key, and `ilook` on each side finds that
  entry's provision.
- `entries_rel_kinds`: `PublicFamilyAgree.entries_rel` (the same hypotheses)
  that also gives `SameKind` for each pair.
- `result_gens_vals` and `result_key_gens_vals`: a result with no outputs,
  read in a FunCtx, mentions only generations of that FunCtx's table
  readings (in its provisions and in its key receivers).
- Helpers: `table_gens_dom`, `pr_noout`, `pr_keys_match`, `functx_mgm`,
  `functx_ty_sub`, `TableOk`/`sub_inv`/`th_inv` (T-invariance), `sig_inv`,
  `sigrel_eq`.

**Module `ChainSigs`.** It is now PublicEntry's root instance: `FL := CSg.FL`
and `CF := FL.CF`. This is needed because the inductives of separate functor
instances do not unify.
- `chain_sigs`: induction on CopyChain with a fixed valuation T. The
  premises are that T satisfies the level's table (`TableOk`) and that the
  fixed inputs mean T in gfv. The conclusion is `SigInv`: the level's own
  reading exists, and at every position its method agrees at T (receiver and
  signature) with the root item that the chain's sources give that position.
  - The base case is the root family.
  - Each step is the copy step at T (`PublicCopyFresh.copy_outputs_at`,
    with rho0 the copy site's meanings, and CopyGen from `CopySite` and
    `result_gens_vals`/`ys`), then h@Cu against h@Ch (`entries_rel_kinds`
    + `pos_last`, at θ_h = Cu's readings), then the IH at the copy's
    source position.
- `chain_forward_copy_sigs : CF.ChainForwardCopySigs W Prog argv read so`.
  It takes two premises, `MergeFormed W` and `MentionsTyped W`, and
  PublicEntry discharges both (`PublicWorldFormation`).
  - T is `ground d ∘ θ ++ type_meanings d`.
  - The top step relates the caller's reading at C to u's own reading at
    θ (`th_sigma`, `pos_last`).
  - Lastness is used only at the top. The `incl` part of SigAgreeG comes
    from the root reading (`result_gens_vals`, `result_key_gens_vals`).

check.sh passes (see the final commit).

## Remaining

- `CN.ChainRealizeOk` (realize2 is working on it) and
  `FS.FamilyCopyRealizeOk`. The latter's copy steps could reuse
  `chain_sigs`' shape: the same T, `copy_outputs_at` with `result_gens_vals`
  for CopyGen's second clause, and `entries_rel_kinds`/`pos_last` across
  contexts.
- **Merge note:** PublicEntry's root moved from `PublicFamilyLink.FamilyLink`
  to `PublicChainSigs.ChainSigs` (`FL := CSg.FL`). If another branch also
  re-roots, chain the instances: make the new root's module contain the
  other's.

## Time

About 3 hours.
