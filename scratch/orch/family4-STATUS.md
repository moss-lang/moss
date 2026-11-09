# family4 — STATUS

Branch `ob-family4`. `safety` 8a7dbbe is merged (86969ec). `check.sh` passes.

## Done

### Goal 3: `CapturedInForceOk` proved (Audit change 1)

- Test `bootstrap/examples/functor-captured-unbound.moss`: `bind H = P;` in an
  argument of a call that demands family F at `A := T`, with T unbound.
  P captures `T.twice`, keyed at T, which is not in force.
  Oracle exit 0, native exit 0. So option (i), rejecting it, was out.
- The oracle's functor closures use `xclose d dc` (the whole type env), as
  its lambdas do. So I chose option (ii):
  - `BFunctor` carries a `type_capture`.
  - `BFunClosure` gives it `lexical_capture C`, which is what lambdas use.
  - The closure is `typed_capture d types dc`.
  - This change is run-time only. See family4-AUDIT.md §1 and the
    SAFETY-SITES Observations.
- Slice: `cmeaning_capture` shows a closure over `lexical_capture C` reads
  x as `ground d (canon C (θ x))` when x is in force or in `capture_gens C`.
  `case_BFunClosure`'s GroundGm now uses it, so no run-time alias fact is
  needed.
- `CapturedInForceOk W` is restated. It gains a `FunCtx W gg Cg nsg`
  premise and concludes `nlook x (c_ty C) <> None \/ In x (capture_gens C)`.
  - It is proved statically by `PublicLinkMerges.captured_mentioned`, with
    instances `PublicEntry.captured_in_force_ok` and
    `LinkMerges.captured_in_force_ok`.
  - Removed: `captured_in_force_unprovable`, `captured_in_force_of` and
    `DeclaredInForce`.
- Fixtures updated: PublicClosurePrototype, PublicClosureExample and
  PublicFunctorScopeExample.

### Goal 5: `FamilyKeysTyped` proved, and chainfwd's `AppKeysTyped` (Audit change 2)

- `PublicFormation.TysScoped` is now strict: a formed context's type keys
  are declared type symbols (`entries_keys_decl`, `root_keys_decl`).
  Nothing needed the old "or in scope" disjunct.
- Both premises are restated with `TysScoped W C`:
  - `AppKeysTyped`: then immediate (`PublicChainFwd.app_keys_typed`).
  - `FamilyKeysTyped`: `PublicFormation.family_keys_typed`. A matching key
    is a P(F) symbol. Unlisted, it is M(F), so an input of F. Listed, it is
    a type by the new FamilyShape clause.
- **Audit change 2** (family4-AUDIT.md §2): `FamilyShape` requires F's
  listed polymorphic inputs to be type symbols, mirroring the oracle's
  `functor_params` check.
  - Test `examples/family-poly-nontype.moss`: the oracle rejects it.
  - Native accepts it (exit 0), which is a native bug against D84 rule 1.
    Filed as **proof-plan E34** (PROOFS views regenerated).
- Threading:
  - `TysScoped W C` is a new premise of the slice's `FamilyStepOk`, of
    `FamilyPlatformApplyOk` and `family_run`, and of `demand_gen`.
  - `demand_case`'s scoping hypothesis also returns it.
  - Its sources:
    - from `Formed` (`family_then`, call demands);
    - an attached call's table adds `w_reqs h` (`reqs_typed`);
    - the demand chain keeps C's table (`family_restore`).
- The w_reqs source is covered (`PublicWorldFormation.reqs_typed`). The
  Poly source is covered by the Audit clause.

`safety_obligations` no longer lists `CapturedInForceOk`, `AppKeysTyped`
or `FamilyKeysTyped`.

## Not done: Goal 4 (`SigCoherent` in `Formed`, `FamilyCoherentOk`)

Not started, because of the context budget. Plan, unchanged from
family-STATUS "Suggested design" 3:
- `SigCoherent C`: each `(mkey r m, ProvidedMethod g a)` among C's
  provisions has `g` equal to the declared `SigOf m` read at `theta_of C a`
  with receiver r (canonically).
- Put it in the slice's `Formed` as a 4th conjunct, which changes every
  `[Fc [Ff Cs]]` pattern; grep `PublicScoping.` for the `Pre`-concluding
  proofs. Alternatively put it inside `PublicScoping.CxScoped`, which is
  less churn: only CxScoped's own lemmas change.
- Sources:
  - entries: `IEMth`/`OIEMeth` readings;
  - `BMethBind`'s `SigOf`/`AppOf`;
  - family/functor results through `output_cx`;
  - lambda/narrow, which keep provisions.
- Then `FamilyCoherentOk` follows from `sig_rel` (PublicTypeRead) at equal
  θ and receiver.

## Remaining obligations in `safety_obligations` (that I know of)

`ChainKeysOk`, `ChainRealizeOk`, `ChainForwardCopyKeys`, `FamilyLinkNewOk`,
`FamilyCopyApplyOk` and `FamilyCoherentOk`.

## Time

About 3h.
