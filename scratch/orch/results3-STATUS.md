# results3: ChainCoreOk / FamilyLinkResultsOk — STATUS

Branch ob-results3, safety (e776440) merged. check.sh passes.

## Proved (bootstrap/theories/PublicChainCore.v, module `ChainCore`)

`ChainCore` instantiates `LR := PublicLinkResults.LinkResults`; PublicEntry
now takes `CC := PublicChainCore.ChainCore ...` and `LRes := CC.LR`, so
there is one slice instance.

- `chain_core_ok : ChainNewOk -> LR.KeptOutputsUnmentioned W -> LR.ChainCoreOk ...`.
  Its parts, each a lemma:
  - `chain_selection`: the result selection from the body's keys succeeds
    (`ResultRealizes` gives a Cb provision at each source key; Pre3's
    Realizes gives a run-time provision there, `realized_key`).
  - Outputs readable: Pre3's `AliasOk` at `OutM`'s keys.
  - `alias_after`: `AliasOk C'` after the application (outputs: the
    aliases `read_meaning`, the destination's write; other keys: as in d,
    the destinations being out of C's keys and `context_gens`).
  - KeysMatch C' (`provide_result_news`: C' = news ++ C, news of their
    own kinds).
  - `envok_kept`: every provision C' keeps from C (no new provision has
    its key) is realized as before, given that every key dr fills is the
    run-time key of a new provision (`rkey_kept`, `ground_after`).
- `chain_results_of_new : ChainNewOk -> KeptOutputsUnmentioned -> CA.ChainResultsOk`.
- `body_provides`: the body side of the correspondence. For each formal
  result key k of Cr, the body's source key holds a value realizing Cr's
  provision of k read at the body's meanings (`BodyReal`: types under
  `read_rename Cb ren`, a method's receiver as `at_site Cb` reads it).
  From `ResultRealizes` plus Pre3's Absorbs/EnvOk.
- `family_link_results_ok : FamilyLinkNewOk -> FS.FamilyLinkResultsOk`
  (the selection is `chain_selection`).

`safety_obligations` lists `CC.ChainNewOk` in place of `LRes.ChainCoreOk`
and `CC.FamilyLinkNewOk` in place of `FS.FamilyLinkResultsOk`.
SAFETY-SITES updated (entry paragraph and hypotheses table).

## Remaining premises

- `ChainNewOk`: with dr selected and `c_provisions C' = news ++ c_provisions C`:
  (a) every key dr fills is `rkey_of C' k1` for a new key k1;
  (b) `EnvOkOn (In _ (map fst news))`: C''s new provisions realized at d'.
- `FamilyLinkNewOk`: the selection's dr gives `FamilyNewOk`.

Both are the caller/formal correspondence. See "Next" for the plan.

## Next (plan and findings)

Write the rest in terms of `body_provides`. For a caller key kc in news,
dr's value at `rkey_of C' kc` comes, via `select_lookup` / `sm_find`, from
the first formal key k0 whose run-time key
`app_subst_item Θ (output_renaming outs' gen (rkey_of Cr k0))` equals it,
where `Θ = result_substitution th (t ↦ gensof t)`. Its value is the body
source of the first k' with the same renamed key (`link_source_of`).
What is missing splits into two parts.

1. **Static (an analogue of ChainResultAgrees, but with outputs).**
   - Key correspondence, both ways:
     - every k0 ∈ ks' has a caller key with that run-time key (clause (a));
     - every new caller key has such a k0.
   - The provisions correspond: caller type `tc` against formal `tr` with
     `canon C tc = canon C (app_subst Ψ tr)`, where
     `Ψ = (gen t ↦ TAbs (gensof t)) ++ th`.
   - For the empty chain (h0 = u, Ch0 = Cu) this is
     `PublicOutputSubst.provide_result_rel` with `s1 = Ψ`-like and
     `s2 = c_sub C`. Better, use famplat's `PublicFamilyAgree.family_entries_rel`
     (it has no DomGap and instantiations agree exactly), generalised to
     outputs.
   - Keys need exact syntactic equality. `θ` values are C-canonical, and
     θ absorbs Cu's merge (Satisfies' merge clause; famplat's `th_sigma`).
   - Copy chains: famplat found that `CopyOutputs` is one-directional
     (scratch/orch/famplat-STATUS.md, "Finding"). Clause (a) needs the
     converse (every root key is a caller key). Otherwise dr installs an
     extra key, which may shadow an old provision of C. The same options
     apply as for ChainResultAgrees.
   - Edge case to check: a raw output symbol t that is also an input and
     is mentioned through a merge in Ch0's table. `output_renaming` and Θ
     send raw t to `gensof t`, while the caller reads it as `th t`.
2. **Run fact (body inputs).**
   - `ground d3 (rr tr) = ground d' (Ψ tr)` needs, for each input x of
     the root mentioned by the formal types, that d3's meaning of x is
     `ground d (th x)`.
   - For a receiver (read through `at_site Cb`, i.e. Cb's table and
     aliases), it needs `output_meaning d3 x` to equal that too.
   - Neither ChainCoreOk nor LinkResultsOk carries the body run's
     `MeaningsGrow made de d3`, although `app_step` has it from
     `chain_run` (it discards it). Proving it inside ChainNewOk means
     either:
     - a standalone lemma on `eval_bindings` (only BApply writes
       meanings, at its destinations) plus a static walk over `BindItems`
       (destinations are never keys of the body's table); or
     - a statement change, which the rules forbid for the `...Ok`
       obligations.
   - Then `application_environment`'s reading of a linked input
     (through Cg's table and merges, LinkData's 4th conjunct) gives
     `ground d (th x)`.

## Time

About 2.5 hours.
