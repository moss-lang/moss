# Status: PlatformApplyOk ("platform")

## Proved

- `bootstrap/theories/PublicPlatformApply.v`, module functor
  `PlatformApply (Prims) (I0)` (its `S` is `BodySlice Prims I0`):
  `platform_apply_ok : PlatformPrimsOk W Prog argv read so ->
   PlatformResultAgrees W Prog -> S.PlatformApplyOk W Prog argv read so`.
  The run is computed: inputs selected (`static_realizes_le`), no copy
  chain, the family's code only installs primitive closures
  (`installs_run`, `installs_lookup`), results selected under the caller's
  run-time keys (`inst_smap` at θ), `result_environment` with no outputs.
  Then `Pre` at C': frame, locals and aliases (C' only re-canonicalizes
  C's table), formation (`apply_abstract_formed`), and `Realizes`: values,
  fns and functors untouched (the result holds method keys only); old
  methods not shadowed; new methods are the family's primitives, callable
  at the caller's signature (`callable_transfer`, `app_env_ground`: the
  family's code reads its inputs' types as the caller's θ does).
- `bootstrap/theories/PublicEntry.v`: `platform_prims_public` discharges
  `PlatformPrimsOk` for the reference host (from `primitive_safe`, as
  `entry_primitive` does for main's primitives; `Compatible` supplies
  support of every operation in a functor body). `EntrySafety`'s slice is
  now `PA.S` (`Module PA := PlatformApply M.ReferenceHost M.Reference`) so
  the instances agree. `safety_obligations` now lists, in place of
  `S.PlatformApplyOk`, `PA.PlatformResultAgrees W Prog` and
  `PlatformSigsOk W Prog argv read so`.
- Roots: `PublicPlatform.platform_apply_ok`,
  `PublicEntryTop.platform_prims_public` in assumptions/Certificate.v.

## Audit changes (scratch/orch/platform-AUDIT.md)

Two, each its own commit, both in `Platform.Family`: the family's items are
filed under their run-time keys (`rkey_of outputs`; example
`bootstrap/examples/platform-family-keys.moss`, oracle and native run it),
and a platform family has no type output. Without them the obligation is
false.

## Remaining premises

- `PlatformResultAgrees` (static, T15/T16): the caller's ProvideResult
  (at θ, C') and the family's (at its inputs) list the same items: C'
  adds method provisions only, each under the run-time key the family's
  item gets at θ and with that item's signature read at θ (canonically in
  the caller, its types scoped in the family's inputs), and every family
  item is among them. Needs a substitution lemma for Audit's result
  readings (OutputItemsEntries / RecvTy / SigOf / AppOf / brackets)
  between the family's type table and the caller's.
- `PlatformSigsOk` (T11/T29, Public only): the AtKey signatures of a
  family's primitives have handle tags formed (`HandleFormed`, as already
  assumed for main's platform signatures), and their machine type symbols
  ground to the machines in the environment the family's code runs in.
  The latter is a runtime invariant the slice does not record: `CPlatform`
  closures carry an arbitrary `gfx`; recording that a platform closure's
  fixed machine inputs mean the machine types would let it be proved from
  the application's fixed-input agreement.

## Time

About 2 hours (reading ~45 min, Audit changes and tests ~20 min, proofs ~55 min).
