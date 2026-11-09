# callerkeys2: status

Task: finish the caller-keyed result install (Round 2 of
scratch/orch/roles-STATUS.md), delete or prove the key obligations it
makes true by construction. Done; `check.sh` passes.

## Audit change (own commit, scratch/orch/callerkeys2-AUDIT.md)

`BApply` carries the caller's key for each position of F's declared
result; `fc_sources` the root body's key for each position; a copy
(`BCopyFunctor`, `FunctorClosure`'s `rms`) its position map;
`apply_functor` selects `result_selection rs rms (fc_sources code)`
(last position wins, matching the static last setter). `CopyOutputs`
records positions (same provision id, last setter, the kept agreement
after sigma) and results4's converse is reverted. `fc_roles`,
`result_smap`, `inst_smap`, `role_outputs` and friends are gone. Corpus:
an oracle variant with the matching rule agrees with the oracle on 398/398
files (scratch/orch/callerkeys2-cmp.out). Tests: copy-extra-key.moss
(accepted again, 31 as native), linked-copy-bracket.moss,
family-copy-linked-bracket.moss (hunt t11).

## Deleted or proved (no longer in `safety_obligations`)

- `CN.ChainKeysOk`: deleted (PublicChainNew). ChainNewOk's former clause
  (a) is proved by construction (`PublicChainCore.selection_news`, used
  in `chain_core_ok`); `ChainNewOk` is now clause (b) only, and
  `chain_new_ok : ChainRealizeOk -> ChainNewOk`.
- `CF.ChainForwardCopyKeys` and `PA.ChainForwardKeys`: deleted, with
  `ChainResultAgrees`, `ChainForwardAgrees`, `ResultAgree`, `ItemsAgreeG`,
  `chain_result_agrees`, `chain_forward_agrees`. The key half is by
  construction in the new `family_core`; what remains is
  `PA.ChainForwardSigs` (signatures at the caller's last position for a
  key), proved for the empty chain (`CF.chain_forward_sigs`, from the new
  `PublicFamilyAgree.items_agree_pos` and `items_agree_gen`) with the copy
  case `CF.ChainForwardCopySigs` as the obligation.
- `FS.FamilyCopyApplyOk`: now proved (`family_copy_apply_of_realize`)
  from `FS.FamilyCopyRealizeOk`, its realization half; the key half from
  `apply_functor_keys`.
- `CC.FamilyLinkNewOk`: its key clause dropped (proved in
  `family_link_results_ok`).
- `family_platform_ok` (direct family application) re-proved for the
  positional selection (`family_items_pos`).

`safety_obligations` now assumes: `CN.ChainRealizeOk`,
`CF.ChainForwardCopySigs`, `CC.FamilyLinkNewOk` (realization only),
`FS.FamilyCopyRealizeOk`, `S.FamilyCoherentOk`.

## New files and lemmas

- theories/PublicCallerKeys.v (in Certificate roots): `same_kind_entries`
  (two readings of one result have entries of the same kinds),
  `provide_result_length`, `result_key_id`, `rkey_of_inj`, selection
  lemmas (`selection_in`, `selection_at`, `selection_last`,
  `sm_find_rev_combine`), `provide_result_key_news`/`news_key`.
- PublicCopyApply.chain_sources_range (positions in range along a chain).
- PublicFamilyAgree: `keys_of_pairs`, `items_agree_pos`,
  `family_items_pos` (added, no statement changed).
- PublicPlatformApply: `chain_core_premises`, new `family_core`,
  `chain_platform_ok`, `family_platform_ok`.

## What remains

- `ChainForwardCopySigs`, `ChainRealizeOk`, `FamilyLinkNewOk`,
  `FamilyCopyRealizeOk`: realization (value/signature) halves through
  copies; they need the readings composed along the chain (CopyOutputs'
  provision agreement under sigma at each step, CapturedFuns/FixedMeans).
  Not attempted.
- Oracle conformance: E32/E33 (proof-plan.json updated): the oracle still
  installs the root's keys read at θ.
- `PublicChainAgree.chain_cover` is no longer used by the entry (kept,
  still proved).

Time: about 3.5 hours.
