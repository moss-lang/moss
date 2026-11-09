# repair: merges in the semantics, CapturedInForceOk's invariant

Branch `ob-repair`. `safety` (77d6fb0, with the platform merge) is merged in
(377f481), and `check.sh` passes (2m35s). Platform families record `[]` as
fc_table; PublicPlatformApply drops `link_merges`.

## Done

**Task A, the Audit change** (commit 206d739; details and alternatives are
in scratch/orch/repair-AUDIT.md §1; there is a SAFETY-SITES Observations
entry):
- `functor_code` has a new field `fc_table : list (tsym * ty)`, the third
  one, between `fc_linked` and `fc_outputs`.
- `application_environment` keeps the old per-input meaning `base x`: θ
  for a linked input, the capture otherwise. When the table maps x to t,
  input x now reads as `app_subst bases t`. When the table has no entry
  for x, it reads as `base x`, as before.
- `LinkBody` records `c_ty Cg`. `Platform.Family` and every example code
  record `[]`, so their behaviour is unchanged. With `[]`, `cbn` reduces
  the environment to the old term.
- Elaborated code cannot observe the change: it reads only canonical
  types, which mention only representatives, and their meanings are
  unchanged.

**Task B, the proof** (`PublicBodySlice.link_entry`):
- New lemmas `GD`, `functx_canon_prov` and `envok_transfer` prove G's
  merges from the new semantics.
- Deleted: `LinkMergesOk`, `BindMergesOk`, the `link_merges`/`bind_merges`
  hypotheses, `ClosureOk`'s merge clause (`CLink`, `closure_lower` and
  `closure_cases` follow), and the merges bullet of `case_BFunClosure`.
- `PublicEntry.safety_obligations` no longer lists them.
- In the merged files, `PublicCopyApply` and `PublicFamilyStep` drop
  those hypotheses and their arguments. `app_env_thof` now accepts any
  table, using the new `app_meanings_ext`.

**Task C: `DeclaredInForce` is false in Audit as it stood (counterexample).**
- The program is `let y = { bind F; make() };`, where F's output T is not
  in force. It leaves `y : T` in a context where T is not in force.
- Both implementations reject it: the oracle with D78's escape message,
  and native with "a type created by a binding cannot escape its block".
- I fixed this in Audit (b2c4a90). `safety` had meanwhile made the same
  fix (examples/output-escape.moss, `made := map gensof outs`), so the
  merge keeps `safety`'s version. My extra evidence is in repair-AUDIT.md
  §2: native also rejects the program and its statement variant.

## Not done

Task C's proof is not done: `CapturedInForceOk` is still an obligation.
The lead has handed the scoping invariant in `Formed` to the family agent;
the plan below is for them.

The plan, checked against the definitions:
- **Ex-level lemma** (new file, mirroring PublicFormation's `ex_ok`
  fixpoint):
  - With `CxIn K C` (every declared symbol in `context_gens C` is in K)
    and `FrameIn K F` (the same for `fr_ret` and `fr_this`), an
    elaborated type's declared symbols are in K.
  - A `BindItem` turns `CxIn K C` into `CxIn (K ++ made) C'`, using
    `tyof_gens`, `sigof_scoped`, `provide_result_ok`-style reasoning for
    results and families, and `FamilyReadOff` for matchings.
  - `BlkE`'s escape check (now over all outputs) removes `made` again, so
    the K-generalization closes.
- **Entries:** `entries_cx_formed`'s premises already give
  `DeclaredInForce` for `CtxOf`/`FunCtx` contexts. Lambda contexts follow
  from C.
- **Slice:** add `CxIn (keys C) C /\ FrameIn (keys C) F` to `Formed`, so
  that `case_BFunClosure` can use `PublicLinkMerges.captured_in_force_of`.
- **Interface cost:** this changes `Pre`, which the remaining obligations
  (`ChainResultsOk`, `ChainPlatformOk`, the Family* ones) mention in
  their premises and conclusions.
- **Overlap:** the family branch's new slice hypotheses (`ExScopedOk`,
  `LitScopedOk`, `DemandScopedOk`, see SAFETY-SITES) need the same kind
  of "scoped types in `Formed`" invariant. Both should be done together,
  in one change to `Formed`.

## Notes for the lead

- `FunctorCode` now has 6 fields: inputs, linked, table, outputs,
  bindings, sources. Literal constructions need `[]` for the table.
- `link_entry` no longer takes a merge clause in its `Caps` premise.

Time: about 4.5 h.
