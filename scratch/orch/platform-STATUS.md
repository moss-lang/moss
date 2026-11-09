# Status: PlatformApplyOk / ChainPlatformOk ("platform")

Branch ob-platform, merged with safety at 3a869cf (worldform, copy,
refile, family). The platform step's slice is FS.CopyA.S (PublicFamilyStep);
platform codes have fc_sources = []; ChainPlatformOk's made list is
map gensof outs (D78). Main's HandleFormed is now proved (handles_formed),
so PlatformHandlesFormed covers the families only. FamilyPlatformApplyOk
(PublicFamilyStep) is not discharged: its static setting (family_context,
FamilyStepOut) differs enough that family_core does not apply directly.
Earlier: ob-platform also contains ob-copy (merged at 9abedeb,
since ChainPlatformOk and the copy chain live there; conflicts resolved:
PublicBodySlice's ClosureOk constructors, PublicEntry's obligation list,
Certificate roots, SAFETY-SITES; `chain_entry` in PublicCopyApply.v passes
CPlatform's new field).

## Proved

- `bootstrap/theories/PublicPlatformApply.v`, `PlatformApply (Prims) (I0)`,
  whose slice is `CopyA.S` (`CopyA := PublicCopyApply.CopyApply Prims I0`):
  - `chain_platform_ok : LinkMergesOk -> ChainResultAgrees -> ChainPrimsOk ->
     PlatformNotLinked -> CopyA.ChainPlatformOk`. With
     `CopyA.platform_apply_of_chain` it gives `S.PlatformApplyOk`.
  - `family_core`: one application of a closure whose code is a platform
    family's (reached through the copies' smaps): the run is computed
    (inputs selected, needs re-selected through the chain, the code only
    installs primitive closures, results filed under the caller's run-time
    keys) and the caller's result context is realized (`Pre`).
  - `chain_adapt`, `chain_keys` (the chain re-selects the needs; the root's
    inputs are among F's), `chain_fixed_ground` (the code reads a root's
    fixed machine input as the machine type), `ctx_fun_self` (a function
    context's functor provision reads fixed inputs as the symbols).
- `PublicBodySlice.v`: `CPlatform` records `PlatformFixed u gfx` (fixed
  machine-type inputs mean the machine types).
- `PublicEntry.v`: `entry_realizes` establishes `PlatformFixed` for main's
  platform closures (initial types read machine symbols as machines);
  `platform_prims_public` discharges `ChainPrimsOk` for the reference host
  from `primitive_safe`. `safety_obligations` now lists (besides the
  others' obligations) `MainMachinesUnmerged`, `PlatformHandlesFormed`
  (replacing main's HandleFormed premise: main's and the families'),
  `CopyA.ChainResultsOk`, `PA.ChainResultAgrees`, `PA.PlatformNotLinked`,
  `PlatformSigsFixed`.

## Audit changes (scratch/orch/platform-AUDIT.md)

`Platform.Family`: items filed under run-time keys (`rkey_of outputs`;
example `bootstrap/examples/platform-family-keys.moss`, runs on oracle and
native), and no type outputs. Both were needed (the obligation was false).

## Remaining premises (mine)

- `ChainResultAgrees` (T15/T16, static): for F applied at θ with its
  closure a copy chain down to platform family h0, F has no type output,
  and C' adds exactly the family's method items, each under the run-time
  key h0's item gets at θ, with that item's signature read at θ. Needs the
  OutputItemsEntries substitution lemma (results agent) plus CopyOutputs
  along the chain. Its empty-chain instance is the old PlatformResultAgrees.
- `PlatformNotLinked` (static): a platform family's code that has
  bindings is not a linked body's code (platform codes are minted by
  platform_fun; linked binds come from source items, none a primitive).
  Only used to identify a closure's root when the code is shared.
- `PlatformHandlesFormed` (T29): main's and the families' platform
  signatures have handle tags formed (one premise; same gap as main's).
- `PlatformSigsFixed` (D84, static): a machine symbol a family's AtKey
  signature mentions is a fixed input of the family's functor, in its
  context's table (true of lib/wasm.moss's Heap, Mutable, Arrays,
  Memories; Platform.Family does not require it).
- `MainMachinesUnmerged` (T29, static): main's context merges no machine
  type symbol away. (MostGeneralMerge picks representatives freely.)

## Time

About 4 hours in total.
