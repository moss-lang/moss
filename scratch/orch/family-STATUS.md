# family: FamilyStepOk

## Proved

`bootstrap/theories/PublicFamilyStep.v`, functor `FamilyStep Prims I0`
(`Module BS := PublicBodySlice.BodySlice Prims I0`):

- `family_entries_methods`, `family_news` (outside the functor): a family's
  result entries are method entries only. So `Cr`'s provisions are some new
  method provisions, keyed exactly by `ks`, followed by C's provisions, with
  C's merge and locals.
- `family_run`: one family application `BApply u θ [] sm`, from `Pre` at
  C0, is the abstract application at `family_context C matching`. The state
  realizes that context, which differs from C0 only in its table. The
  closure for u is
  - linked: `static_realizes_le`/`apply_theta` at the family context, F's
    fixed inputs read through `FixedAgreement`, `link_entry`, the body's
    chain via `case_BApplyAbstract`/`items_chain`, `refile_run`, then
    `FamilyLinkResultsOk`;
  - a copy: `FamilyCopyApplyOk`;
  - a platform family: `FamilyPlatformApplyOk`.
  It completes, installing only provisions (no meanings or aliases) under
  the run-time keys of the new methods, each callable at its signature
  (`FamilyResultOk`), or it stops.
- `realizes_family`: `with_ty C0 Cr` is realized after it. This needs no
  coherence: a new key never collides with one of C0's methods that the
  family does not provide.
- `keep_family`: any Cx among C0's provisions (same merge) stays realized,
  given `FamilyShadowOk`.
- `family_step_ok : FamilyFormedOk -> FamilyShadowOk -> BS.FamilyStepOk ...`.

`PublicEntry.safety_obligations` no longer lists `FamilyStepOk`. It lists
the five premises below instead, and `S` is now `FS.BS`. `Certificate.v`
has `PublicFamilyStep` as a root (`@PublicFamily.family_step_ok`).
`check.sh` passes.

## New premises (in safety_obligations)

- `FamilyLinkResultsOk`, `FamilyCopyApplyOk`, `FamilyPlatformApplyOk`: the
  application obligations restated for a family application. `AppPremises`
  does not fit: there is no source path (`resolve_sym ... = Some u`), and
  the state realizes C0, not the family context, whose table differs (so
  `Pre` at it fails: `AliasOk` of the matching's entries). Their
  conclusions say only what the family installs (`FamilyNewOk`), not a
  `Pre` at the result. They are the same claims as the abstract ones,
  instantiated at outs = [], newgens = [] and θ = arguments. A
  generalization of `AppPremises` to "read at a context Ca whose merge and
  provisions are the state's" would serve both. That changes existing
  obligation statements, so I did not do it.
- `FamilyFormedOk`, `FamilyShadowOk`: **false at FamilyStepOk's
  generality**, so `FamilyStepOk` as stated is false (for programs with
  families), and so was the obligation list before this change. This is
  not an Audit gap: every rule that applies a family has the missing facts
  or could have them. Details are in SAFETY-SITES.md, Observations,
  "`FamilyStepOk` is too general":
  1. Formation. `FamilyStepOk` has no `FamilyReadOff`, so the matching is
     arbitrary on P(F) symbols that the needs do not mention. Example:
     `T = TAbs g` for g outside C0's scope makes the family's key
     non-`KeysScoped`, while the application still completes. Even with
     `FamilyReadOff`, the receiver must also be in C0's scope. That needs
     an "Ex types are scoped" lemma, which PublicFormation does not prove
     (locals are not scoped by `FormedCx`).
  2. Signature coherence. `Pre` does not say that C0's method provisions
     are `SigOf` readings at their θ, so C0 may have a provision with the
     same receiver, method and θ as a family member but another signature.
     The family's closure shadows it at the same run-time key. For
     elaborated contexts this should hold (same m, receiver and θ over the
     signature's mentioned requirements; `sig_rel`), but only as an
     invariant carried in `Pre`/`StaticOk`.
  No `.moss` counterexample exists for either: both need states that
  elaboration does not produce.

## Remaining / suggested

Restate `FamilyStepOk` (and `family_then`/`demand_gen`'s callers) with
`FamilyReadOff` for a demanded key whose receiver is ok and in C0's scope,
and add a signature-coherence invariant to `Pre`. Then prove
`FamilyFormedOk`/`FamilyShadowOk` in that restricted form. The first
follows `PublicFormation.family_application_ok`; the second is a `sig_rel`
argument plus preserving the invariant. `PublicFamilyStep.v`'s run part is
unaffected.

Time: about 1h40m.
