# Status: FamilyPlatformApplyOk ("famplat")

Branch ob-famplat, merged with safety at 5522b69 (ob-results).

## Proved

- `bootstrap/theories/PublicPlatformApply.v`, new section `FamilyPlatform`
  inside `PlatformApply`:
  - `family_platform_ok : PrimsGroundOk -> FamilyResultAgrees ->
     FS.FamilyPlatformApplyOk`. The run follows `family_core`'s prefix
     (select at the family context, `sms = []`, `installs_run`,
     `result_smap` with no outputs) and concludes `FamilyResultOk` directly
     (no `Pre`). The closure's `PlatformFixed` comes from `Pre`'s `Funs`
     (closure_cases); the fixed-reading agreement from FamilyApplication's
     `FixedAgreement` at the family context, as in `family_run`.
  - `PrimsGroundOk`: the primitive half of `ChainPrimsOk`, for any
    environment that reads the family's fixed machine-type inputs as the
    machine types. `chain_prims_of_ground : PrimsGroundOk -> ChainPrimsOk`
    (via `chain_fixed_ground`).
  - `ItemsAgree`: the shared agreement between a result's new methods
    (a predicate `New r m g`) and a family's own items at θ. `ResultAgree`
    (so `ChainResultAgrees`) is now restated through it (same content);
    `chain_platform_ok` unchanged.
- `PublicEntry.v`: `platform_prims_public` now proves `PA.PrimsGroundOk`
  (same proof, the machine fact taken as the premise).
  `safety_obligations` no longer lists `FS.FamilyPlatformApplyOk`; it lists
  `PA.FamilyResultAgrees W Prog` instead.
- Certificate roots: `family_platform_ok`, `chain_prims_of_ground`.
  `check.sh` passes.

## New premise

- `FamilyResultAgrees` (static, T15/T16): for `FamilyApplication W C u
  matching th sm Cr ks` and u's `FamilyData` (outputs, keys), `ItemsAgree
  (family_context C matching) Cr Cu outputs th (fun r m g => In (mkey r m)
  ks /\ mlook r m (c_mths Cr) = Some g) keys`. ResultAgree minus the
  provisions' shape. Not proved: it is the same substitution argument as
  `ChainResultAgrees`. `PublicOutputSubst.provide_result_rel` relates the
  two readings, but the family context's table (matching ++ caller's
  table) and u's inputs have different domains (so `DomGap` is not ruled
  out by `DomSame`), and the run-time keys' recorded instantiations
  (`theta_of`, canonical in each context) need their own argument relating
  them through θ. A results agent should discharge both premises together
  through `ItemsAgree`.

## Time

About 1 hour.
