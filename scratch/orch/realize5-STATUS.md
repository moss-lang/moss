# realize5: status

Branch ob-realize5, from safety f18e42d. About 1.5 hours. No Audit change.

## Done: the last obligation is proved; `public_safety` is closed

`FS.FamilyCopyRealizeOk` is proved (`PublicFamilyCopy.family_copy_realize_ok`)
and removed from `PublicEntry.safety_obligations`, whose premise is now
empty. `PublicEntry.public_safety` states `Audit.Public`'s `CORRECT.safety`
for every prelude, with no hypotheses:

    Theorem public_safety : forall prelude program interface initial,
      M.Admissible prelude program ->
      M.Compatible program interface ->
      forall code final,
        ~ M.Runs program interface initial (M.Result (M.Wrong code) final).

It is in the `assumptions/Certificate.v` roots (`PublicEntryTop.public_safety`),
and check.sh passes, so it assumes only the kernel's primitives.

PublicEntry's root instance is now `PublicFamilyCopy.FamilyCopy`
(`Module FCp := ...; Module CRz := FCp.CRz`), which contains
`PublicChainRealize.ChainRealize`.

## The approved statement change

`PublicFamilyStep.FamilyCopyRealizeOk` and `FamilyCopyApplyOk` gain
`PublicFormation.TysScoped W C ->` after the `A.c_locals` equation, before
`A.FamilyApplication`, as `FamilyLinkResultsOk` and `FamilyPlatformApplyOk`
have it. `family_run` passes its own `Ts` at the call, so `family_step_ok`'s
assumptions do not grow; `family_copy_apply_of_realize` passes it through.
Recorded in SAFETY-SITES.md (Observations, the hypotheses table, the entry
section).

## theories/PublicFamilyCopy.v (new; Certificate root)

Module `FamilyCopy (Prims) (I0)`, with `CRz := PublicChainRealize.ChainRealize`.

- `family_top` (Section Top): from u's `CRz.LevelReal` at its own context
  Cu, at the input valuation `Tin = ground d ∘ θ ++ type_meanings d`, and
  the selection `dr`, each new method of Cr (keyed in ks) is callable at
  its signature from the state d. `level_rel` with C = the family context,
  g2 = id and no outputs (KeysT from `family_keys_typed`, which is the use
  of TysScoped), `selection_at`/`selection_last`, `pos_last_all`,
  `entry_peq` to the valuation `type_meanings d`, `real_transfer`,
  `callable_transfer`.
- `platform_level` (Section PlatformLevel): a platform root's LevelReal:
  position i's source `rkey_of outputs key_i` holds the primitive the code
  installs last for it (`installs_lookup`, `last_prim`), callable at the
  item's signature (`PrimsGroundOk`); the code reads the root's inputs as θ
  (`app_env_ground`), and the item's signature and receiver mention only
  the root's inputs (`result_gens`, `result_key_gens_out`).
- `family_copy_realize_ok` (Section Main): the obligation, under World,
  MergeFormed, MentionsTyped, FamilySigReadOffOk, all_fn_elab,
  CapturedInForceOk, LinkResultsOk, CopyApplyOk, PlatformApplyOk and
  PrimsGroundOk (PublicEntry has all of them). The run follows
  `family_run`'s set-up at the family context and `copy_apply_ok`'s
  `chain_entry`:
  - linked root: `items_chain` (with `case_BApplyAbstract`), `chain_run`,
    `chain_selection` for dr, then `root_level` + `chain_real` +
    `family_top`;
  - platform root: `chain_adapt`, `installs_run`, `select_ok` for dr, the
    machine inputs grounded through `chain_fixed` and the root's
    `PlatformFixed`, then `platform_level` + `chain_real` + `family_top`.
  No separate family version of `PA.family_core` was needed: the platform
  root goes through the same chain induction as the linked one.

## Remaining

Nothing for G05's statement: `public_safety` is closed. (SAFETY-SITES.md
still lists `KeptOutputsUnmentioned` as a user decision in its prose; in the
code PublicEntry uses `LRes.kept_outputs_unmentioned`, a proved lemma.)

## Checks

check.sh is green at the final commit (about 670 s for a full build).

The comment above `FamilyCopyRealizeOk` in PublicFamilyStep.v still says
"HYPOTHESIS (T15)"; it is now proved in PublicFamilyCopy.v (left unedited to
avoid another full rebuild).
