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

## Follow-up: discharging the result agreements

### FamilyResultAgrees: proved, from a narrower formation premise

- `bootstrap/theories/PublicFamilyAgree.v` (new, a Certificate root):
  - `family_entries_rel`: PublicOutputSubst's `output_entries_rel` without
    the DomGap escape. For a family u, if every symbol u's result mentions
    (ResultMentions) is a key of the first table, the first table's keys
    are keys of the second (`DomT`), and an extra key of the second that a
    path names is a type symbol, then method entries correspond exactly:
    receivers and signatures related by (s1, s2), every instantiation
    requirement in force on both sides (`InstSome`), and the first side's
    entries scoped in a given list.
  - `family_items_agree`: for a FamilyApplication and u's own result
    (outputs/keys at its FunCtx Cu), `ItemsAgree (family_context C
    matching) Cr Cu outputs th New keys`. It uses s1 = θ, s2 = C's merge.
    θ absorbs u's merge σ (Satisfies' merge clause, `th_sigma`). Keys
    correspond through NoDup of ks (FamilyApplication), and the run-time
    keys' instantiations match through `InstSome`.
  - `SigAgree`/`ItemsAgree` now live here; PublicPlatformApply uses them
    through notations.
- `PublicPlatformApply.family_result_agrees`: FamilyResultAgrees's content
  from `FamilyKeysTyped` plus the family context's idempotent merge (from
  Pre). `family_platform_ok` now takes `FamilyKeysTyped`, not
  `FamilyResultAgrees`.
- **Remaining premise: `FamilyKeysTyped`** (formation, T29). An extra key
  of `family_context C matching`'s table (not one of u's inputs) that some
  path names is a type symbol.
  - It is needed. `FamilyPlatformApplyOk` puts no constraint on C's table
    (only sub, provisions and locals equal C0's). A C whose table has a tag
    key such as `Ref` makes Cr's receiver for `Ref.deref` that key's
    meaning, while the code installs at `TNom Ref ...`. As stated,
    `FamilyPlatformApplyOk` is then false for such C.
  - Elaboration never builds such a C: table keys are type symbols or
    fresh generations, and no path names a fresh generation. So it can be
    discharged once the family step's C carries a key-formation fact
    (e.g. `TysScoped`, with world formation for the generations). That
    belongs with the family agent's scoping invariants on `FamilyStepOk`'s
    C (`FamilyTableIn` covers values only).
- The family agent's finding (a callee need read as a declared symbol, a
  family applied at `A := T` with T not a key) does not affect this proof.
  Nothing assumes that receivers are table keys. Only the extra keys that
  are present get the type-kind requirement.

### ChainResultAgrees: not discharged

The empty chain would follow the same proof with C in place of the family
context. A nonempty copy chain needs a correspondence through each copy,
and `CopyChain` lacks the facts for it:
- `CCStep`'s `CopyOutputs W sg u h Cu` relates u's and h's results read at
  u's context in one direction only. Every key of u's result has a
  provision in h's, equal after `sg`.
- `ItemsAgree`'s clause B needs the converse: every item of the root
  family's result is one of the caller's. Otherwise the code installs a
  primitive at a run-time key the caller's context does not list, which
  may shadow an old method; `family_core`'s old-method case uses clause B.
- Audit's `BFunCopy` has `SameResult W u h` (the same result leaves), but
  `ClosureOk.CCopy`/`CCStep` do not carry it. Carrying it, plus a lemma
  turning `SameResult` into key-set equality of the two readings at one
  table, plus composing the readings along the chain with `sg`, is a
  separate multi-hour job.

Time for this follow-up: about 3 hours.
