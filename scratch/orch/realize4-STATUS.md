# realize4: status

Branch ob-realize4, from safety b4ebcc9. About 4 hours. No Audit change.

## Done: `CN.ChainRealizeOk` is proved and removed from `safety_obligations`

`PublicEntry.safety_obligations` now assumes only `FS.FamilyCopyRealizeOk`.
PublicEntry's root instance is `PublicChainRealize.ChainRealize` (`CSg := CRz.CSg`),
which contains `PublicChainSigs.ChainSigs`.

### theories/PublicChainRealize.v (new; Certificate root)

The proof does the whole chain at once (the CCRefl case is the chain of
length zero), so there is no separate `ChainRealizeCopyOk`.

**Static lemmas (top level):**
- `pos_last_all`: `PublicChainSigs.pos_last` for any key, not only methods
  (its transfer hypothesis is for any key: `tr_all` discharges it).
- `level_rel` (Section LevelRel): `entries_rel_kinds` with outputs. v's result
  read in its own context Cv with roles g1, and in an application context C
  (Parts) with roles g2, entry by entry at s1 = th ++ [g1 t := g2 t] and
  `c_sub C`: `EntryRel` (all kinds), `EntryRelM` (methods exact), `SameKind`,
  the first side's gens in `dom Cv ++ map g1 outs`, and functor entries'
  fixed inputs covered (`fun_entries_fixed`).
- `entry_peq`: a related pair of entries gives provisions that agree at two
  valuations (`PEq`) under keys that agree (`KeyRel`).
- `result_gens_out`, `result_key_gens_out`: `result_gens_vals` /
  `result_key_gens_vals` with output roles.
- `result_fun_cover`, `rel_gens`, `ren_id`/`item_ren_id`, `lookup_site_role`/
  `lookup_site_in`, `tv_off`/`tv_role`, `tableok_roles`.

**Module `ChainRealize` (Section Real, at fuel n and heap H3):**
- `RealAt T k p pv`: pv realizes p (filed under k) with its types read at
  the valuation T. `real_transfer` moves it along `PEq`/`KeyRel`.
- `LevelReal srcs d3 M Tin v Cv rms`: for some fresh roles g, every position
  of v's own result (at Cv) has a source (through rms) in d3 realizing the
  position's provision at `Tv g outs = [g t := M t] ++ Tin`.
- `r2` (Section R2): every input of h0 that a formal provision (signature,
  functor readings, method receiver) mentions is one of G's
  (`items_scope`, LinkData's made clause): realize3's R2, generalized from
  family_link_new_ok step 6 to all kinds and outputs.
- `root_level`: the root's LevelReal from the body (`CC.body_provides`): a
  role reads as `read_meaning d3 t` (AliasOk of the body's context), an input
  as `ground d (θ x)` (`LRn.link_run_fact`, R2). Takes `TableOk Tin Ch0` and
  `TableOk th Ch0`.
- `chain_real`: induction on CopyChain. The copy step is
  `PublicCopyFresh.copy_outputs_at` at Tu (rho0 = roles ++ the copy site's
  meanings), then `level_rel` for h at Cu against h at Ch, `pos_last_all`
  for the position, the IH, `entry_peq` and `real_transfer`. TableOk of Tin
  and of θ go down the chain (`tableok_down`).
- `top_key`, `top_envok` (Section Top): from u's LevelReal, the caller's new
  provisions are realized after the application (`CC.EnvOkOn`): selection_at
  / selection_last for the position, `level_rel` for u at C against u at Cu,
  `pos_last_all`, `entry_peq` with the state after the application
  (ground d' of θ x is ground d's, of gensof t is read_meaning d3 t).
- `chain_realize_ok : CN.ChainRealizeOk W Prog argv read state_ok`, under
  `exists b, WorldOf b W`, `MergeFormed W`, `MentionsTyped W` (PublicEntry
  discharges all three).

### One change to an existing definition (for coordinator review)

`PublicBodySlice.ClosureOk`'s `CLink` now records that the linked functor
is abstract: `(exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None))`.
- Why: `level_rel` (and `entries_rel_kinds` before it) needs the functor
  abstract, so that every symbol its result mentions is one of its inputs
  (`mentions_dom`). The root of a copy chain may be a linked closure, and
  nothing in `LinkBody`, `LinkData` or `CLink` said that F is abstract.
  (fwdsigs needed the same for copy targets and added it to `CopySite`.)
- It is not an Audit change: `BFunClosure` (the only rule that makes a
  `LinkBody`) already asks `ast_of W u = Some (ADFunctor _ _ _ _ None)`, and
  the slice's BFunClosure case now passes that premise on.
- Touched: the CLink constructor, `closure_lower`, the BFunClosure case,
  `closure_cases` (statement unchanged) and `PublicCopyApply.chain_entry`
  (patterns only); new lemma `S.closure_link_abstract`.
- Effect on obligations: ChainRealizeOk (and FamilyCopyRealizeOk) mention
  ClosureOk in their premises, so their premises are a little stronger.
  This is the same kind of change fwdsigs made to CopySite. No statement
  text of any `...Ok` changed.

## Remaining: `FS.FamilyCopyRealizeOk`

Not started. Plan, reusing this file:
- The realization part is `chain_real` with outs = []: LevelReal for the
  family at its own context, then a family version of `top_key`/`top_envok`
  at the family context Cf (Parts from FamilyApplication, as in
  `family_link_new_ok`; KeysT from `PublicFormation.family_keys_typed`).
  `level_rel` already takes any C with Parts, so only the selection and the
  caller's lookup (`A.rkey Cr r m`, methods only: `family_news`) change.
- The run part is `PublicCopyApply.copy_apply_ok`'s, at a family
  application: `chain_entry` down to the root, then
  - a linked root: `S.items_chain` / `S.chain_run` for the body
    (Pre at Cg from RealG), `CC.chain_selection` for dr, then
    `root_level` + `chain_real` + the family top;
  - a platform root reached through copies: nothing proves this for a
    family application yet (`PA.family_platform_ok` is the length-zero
    case; `PA.family_core`/`chain_platform_ok` need AppPremises). It needs
    a family version of `family_core` with the chain's sources
    (`chain_sigs` from PublicChainSigs gives the signature agreement).
- FamilyCopyRealizeOk's conclusion also states
  `I.apply_functor j P d u th [] sm (map (rkey_of Cr) ks) st = inl (dr, st')`,
  so the run must be unfolded as in copy_apply_ok (bindings_apply_eq,
  apply_functor_eq).

## Checks

check.sh is green at the final commit (see the commit message; about
200 s for a full build).
