# family: FamilyStepOk

## Changes to other obligations (read first)

- **Audit D78** (commit 914cd3c; scratch/orch/family-AUDIT.md):
  `BApplyAbstract`'s made list is now `map gensof outs`. The
  not-in-force output clause also requires that the frame's ret/this types
  not mention the output. In the slice:
  - `LinkResultsOk` concludes `MeaningsGrow (map gensof outs) d (append_environment result d)`
    (was `MeaningsGrow newgens`);
  - `CopyApplyOk`/`PlatformApplyOk` conclude
    `StepOut ... (map gensof outs) ...` (was `newgens`);
  - `AppPremises`/`AppAt` gain the frame clause in the outputs premise.
  The frame (`with_fresh F newgens`) and `FreshFor` are unchanged. The old
  `MeaningsGrow newgens` was false for outputs not in force, whose meaning
  the result writes.
- **`FamilyStepOk` restated** (now proved, `PublicFamilyStep.family_step_ok`):
  - new premises `FamilyReadOff W C u ot m matching`, `TyOk W ot`,
    `incl (gens ot) (scope_gens C0)`, `FamilyTableIn C C0`;
  - the keep clause requires `FamilyCoherent Cx Cr ks`.
  `family_then`, `demand_gen`/`demand_case`, `family_method_case` and
  `literal_family_case` take the matching premises. Their callers supply
  them from four new slice hypotheses: `family_coherent`, `ex_scoped`,
  `lit_scoped`, `demand_scoped`.

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

## Follow-up (coordinator: restate FamilyStepOk, SigCoherent in Formed): blocked

Before restating, I checked whether "elaborated types are scoped"
(`incl (gens t) (scope_gens C)` for `Ex`) is true. Under Audit as it
stands, it is not, and the reason is an Audit gap of its own.

- `BApplyAbstract`'s `made` is `newgens`, the fresh generations for
  outputs already in force. An output not in force is installed as itself
  (`gensof t = t`, table entry `(t, TAbs t)`) and is not in `made`. So
  `BlkE`'s escape check (`made` not in the block's type) lets a value of
  that output type leave the block. Outside, `t` is not in the table, so
  the block's type is not in scope.
- The oracle rejects this: `scratch/t/escape.moss` gives "this block
  yields `T`, which mentions `T`, a type `bind F;` made in it: a value of
  that type cannot leave the block (D78)". I could not run native: the
  moss-safety checkout has no built compiler (`MOSS_SEED` unset).
- Runtime consequence: `result_environment` writes the meaning of `T`
  (the destination `gensof T = T`). So `LinkResultsOk`'s
  `MeaningsGrow newgens d d'` is false for such applications: another
  obligation that is false for programs Audit accepts and the oracle
  rejects.
- Likely fix (Audit): `BApplyAbstract`'s `made` should be every output
  destination, `map gensof outs` (or `newgens` plus the not-in-force
  outputs), and `with_fresh` should be adjusted to match. This touches
  `LinkResultsOk`/`AppAt`/`BindStep` (`MeaningsGrow made`), so it belongs
  with the link-results owner. I have not made the change.

Even with that fix, restating `FamilyStepOk` with scoped receivers needs a
new mutual "Ex types are scoped" fixpoint in PublicFormation. That
fixpoint needs scoping invariants that `FormedCx` lacks today (locals'
types and val/fn provision types are only `TyOk`, not scoped). Call
demands also need their wanted keys' receivers in scope. `at_site` re-reads
a callee need's receiver at the caller's table and leaves symbols the
caller has not bound as `TAbs x`, which need not be in scope. Whether
CallTheta/Satisfies rule that out needs its own check.

`SigCoherent` in `Formed` would change what `RefileRunOk`/`LinkResultsOk`/
`CopyApplyOk` must deliver (they conclude `Pre`), and must be preserved at
every step of the slice that produces `Pre`.

I stopped here to report, rather than start these changes.


## Follow-up 2 (D78 fixed, FamilyStepOk restated)

- Proved: `FamilyStepOk` itself, with no premises beyond the application
  obligations (`family_step_ok`).
  - `family_formed` (outside the functor): the formation half, following
    `family_application_ok` at C0's scope with the matching read off the
    receiver.
  - `keep_family`: the coherence half, now conditional on
    `FamilyCoherent`.
  - `pre_same_table`: assembles `Pre` at `with_ty C0 Cr`.
- `safety_obligations` now lists:
  - the family application restatements;
  - the four slice hypotheses (`FamilyCoherentOk`, `ExScopedOk`,
    `LitScopedOk`, `DemandScopedOk`) in place of
    `FamilyFormedOk`/`FamilyShadowOk`.
- Still open, with reasons:
  - `ExScopedOk`/`LitScopedOk`: a scoping fixpoint over `Ex`. It needs
    `Formed` to scope local and provision types, an invariant to carry
    through every bind, let, arm and lambda. Multi-hour.
  - `DemandScopedOk`: as well as that, a callee need's symbol the caller
    leaves unbound reads as itself (`TAbs x`, a declared type symbol),
    which `scope_gens` excludes. Either such calls cannot demand a family
    (to check), or `KeysScoped` should admit declared type symbols, as
    `TysScoped` does. The lambda capture use of `keys_scoped` then needs
    checking.
  - `FamilyCoherentOk`: needs a `SigCoherent` invariant in `Formed`,
    established at body entries and preserved by every step, including
    `RefileRunOk`/`LinkResultsOk`/`CopyApplyOk`, which conclude `Pre`.
    Multi-hour.
