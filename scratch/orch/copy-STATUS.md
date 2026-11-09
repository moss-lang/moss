# copy: status (CopyApplyOk)

Time: about 05:05 to 05:50 (UTC), one session.

## Proved

`bootstrap/theories/PublicCopyApply.v` (module functor `CopyApply`, added
to `assumptions/Certificate.v`):

- `copy_apply_ok : S.CopyApplyOk` from `RefileRunOk`, `LinkMergesOk`,
  the items-chain hypotheses (formation premises, `all_fn_elab`, `World`,
  `BindMergesOk`, `CapturedInForceOk`) and two new obligations,
  `ChainResultsOk` and `ChainPlatformOk` (below).
- `CopyChain` (the copies from u down to the root of u's closure),
  `chain_entry` (induction over the `ClosureOk` derivation: the root is a
  linked body, whose context the application environment realizes, or a
  platform family), `copy_step_realizes` (one copy: H's context realized
  at the selected `select e1 sm'`, its inputs read at F's θ),
  `adapt_snoc`, `app_env_thof`.
- `link_results_of_chain : ChainResultsOk -> LinkResultsOk` and
  `platform_apply_of_chain : ChainPlatformOk -> PlatformApplyOk` (the chain
  obligations are generalizations: the empty chain).

`PublicEntry.safety_obligations` now takes `RefileRunOk`,
`ChainResultsOk`, `ChainPlatformOk` instead of `RefileRunOk`,
`LinkResultsOk`, `CopyApplyOk`, `PlatformApplyOk` (other premises
unchanged). `PublicEntry`'s `S` is now `CopyA.S` (same functor, same
arguments). No obligation Definition in PublicBodySlice changed.

## Why new obligations instead of LinkResultsOk / PlatformApplyOk

`LinkResultsOk` cannot be instantiated for a copy: it fixes the caller's
applied functor u to the linked body's own functor (`LinkData u gg h`,
closure `FunctorClosure h [linked_needs ns nsg ...]`, `AppPremises ... u`
with `ProvideResult W u C ...`). A copy's caller applies F, whose closure
is `code (sms ++ [sm'])`, and has no `AppPremises` for the root H0 (H0 is
not in its context, may not even resolve from its module). Faking a
caller would change `d`, `C` and `C'`. So the results part must be stated
for the whole chain: `ChainResultsOk` is `LinkResultsOk` with the closure
being any `CopyChain` above a linked root (`adapt_provisions d1 sms`
instead of one select). Likewise `PlatformApplyOk` requires the closure
`code [] empty`, but a copy of a platform family has `code [sm'] empty`:
`ChainPlatformOk`. Whoever proves `LinkResultsOk` / `PlatformApplyOk`
should prove the chain versions instead (the extra content: F's result,
read at the caller, is H0's, by `CopyOutputs` along the chain, and F's
inputs read at θ are H0's, which `copy_step_realizes` already shows for
the needs).

## Audit changes (two commits; scratch/orch/copy-AUDIT.md, SAFETY-SITES Observations)

1. Counterexample: `bootstrap/examples/copy-output-order.moss`. F and H
   list `out[P=U], out[P=V]` vs `out[P=V], out[P=U]` (same leaves); the
   copy then typed the caller's `out` as V while G's body provides U:
   stuck match. Oracle (exit 64, "effective output interpretation
   differs (D78)") and native reject it (native test
   `copied_functors_compare_final_output_interpretations`). `BFunCopy`
   now requires `CopyOutputs` (mirrors `copy_outputs_match`); `CCopy`
   records it.
2. `BFunCopy` requires H's type table (implicit inputs too) to be among
   F's (mirrors the oracle's input-interpretation check failing on an
   input F lacks); needed so the root body's linked inputs read at θ.

Known possible over-strictness of change 1 vs the oracle (documented in
copy-AUDIT.md): keys compared by spelling in F's context, fixed inputs
substituted by their site readings rather than the oracle's solved
`generic`. No corpus program is affected; no mechanized comparison.

## Remains

`ChainResultsOk`, `ChainPlatformOk` (replacing `LinkResultsOk`,
`PlatformApplyOk`), plus the other obligations as before.
`ChainResultsOk` for a nonempty chain will need `CopyOutputs` (recorded in
`CCopy`/`CCStep`) and an instantiation lemma for `ProvideResult`.
