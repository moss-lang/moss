# worldform: status

`safety_obligations` (bootstrap/theories/PublicEntry.v) no longer takes
MergeFormed, MentionsTyped, FamilySigOk or the HandleFormed premise. Its
remaining premises are FamilyStepOk, LinkMergesOk, BindMergesOk,
CapturedInForceOk, RefileRunOk, LinkResultsOk, CopyApplyOk and
PlatformApplyOk, all unchanged. check.sh is green.

## Proved

- `PublicWorldFormation.mentions_typed_of : forall b W, WorldOf b W -> MentionsTyped W`.
  - `reqs_typed`: every requirement of a declared symbol is a type symbol.
    Tags and aliases go through `wo_reqs_least` with the filtered relevance
    reading `typed_reqs`. The other kinds go through
    `wo_reqs_plain`/`wo_reqs_none`.
  - `resolve_sym_declared`: a loaded module's resolutions are declared
    symbols.
- `PublicWorldFormation.merge_formed_of : forall W, MergeFormed W`. It uses
  most generality alone: `mark` keeps formed nodes and wraps unformed ones
  injectively (`unmark_mark`). Applied after σ, it is again a unifier of
  formed entries (`unifies_mark`), and it fixes exactly the formed types
  (`mark_fixed_formed`).
- `PublicFamilySig.family_sig_ok_readoff : forall W, FamilySigReadOffOk W`.
  This was done by a forked helper agent.
- `PublicEntry.EntrySafety.handles_formed`: proved from ElaboratesIn and
  Linked. A fn of main's context is a platform entry (so HandlesRead
  applies), and its signature is formed (ctxof_cx_formed).

All four are in the Certificate.v roots.

## New premises / changes (details in worldform-AUDIT.md)

1. Audit `MostGeneralMerge`: σ is a function (a fifth conjunct). Without it,
   MergeFormed was false by definition: a shadowed junk pair sits in c_sub.
2. Audit `Platform.Entry` (IFn): `HandlesRead`. HandleFormed was false for
   examples/handle-rebind.moss. The oracle rejects that program; native
   accepts it, since main never calls the fn.
3. `FamilySigOk` is false as stated, with a counterexample both
   implementations accept: examples/family-bracket-leaf.moss. The
   PublicFormation and PublicBodySlice hypothesis `family_sig_ok` now has the
   type `FamilySigReadOffOk` (FamilySigOk plus a FamilyReadOff premise),
   which the only use site (ex_ok's family Provided case) has in hand. This
   weakens a hypothesis rather than changing an obligation the final theorem
   keeps. FamilySigOk is left defined but unused. This departs from ORCH rule
   3 (do not change a `...Ok` statement), so the orchestrator should
   confirm it.

## Remaining

Nothing from this task. The other obligations are untouched.

Time: about 1h45m.
