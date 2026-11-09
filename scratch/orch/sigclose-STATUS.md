# sigclose: status

Branch ob-sigclose, on safety d2b1a68 (callerkeys2 is included). check.sh
is green.

## Commits

- 61040f2 "Record every signature type symbol in a method's θ". This is
  the Audit change, with its oracle mirror (Elab.v), the proof repairs and
  the test bootstrap/examples/sig-nonreq-shadow.moss. Details are in
  sigclose-AUDIT.md.
- 5b98fd9: E35 in proof-plan.json (status done, since the oracle is
  fixed), the SAFETY-SITES observation and sigclose-AUDIT.md.

## The change

A method's `AppOf` is the requirements its signature mentions, in fixed
order, followed by `SigOutside`: every other type symbol its signature
mentions, ascending. `RMMeth` counts every symbol a method's signature
mentions. `w_reqs` is unchanged.

Fns are not widened. Their keys carry no θ, so `FamilyCoherent` (which is
about methods only) does not need it, and widening fn requirements would
change functors' fixed inputs and the requirement-cache proofs. This is a
deliberate narrowing of "a fn's or method's requirements"; flag it if fns
must be widened too.

## Corpus

Full runs over tests, examples and bootstrap/examples
(scratch/orch/sigclose-cmp.out): 381 same, 3 different.
- sig-nonreq-shadow: stuck (XErr 14), now 42. This is safe with distinct
  keys, not rejected.
- unbound-merge-representative: stuck (XErr 10), now rejected (D62).
- partially-bound-merge: printed k, now rejected (D62), because the caller
  leaves Y unbound. It is accepted again with `bind Y = Char`.

merge.moss, bridged.moss and signature.moss are unchanged.

## Proof repairs

Every file that destructured or built `AppOf`, `RMMeth` or
`signature_type_syms` was repaired:
- `AppOf` lost its `incl rs (w_reqs W s)` conjunct in ExecutionModel. It
  was the M side's first conjunct, and these files used it: CtxWalk,
  RelSimpleItems, ProvideResultSubst, ProvideResultReading, BindValue,
  ContextFnSnapshots, FnSnapshotAlign, LinkCaptureRegression and
  LinkOutputBoundary.
- Determinism (`appof_det`, `appof_rel`, PublicFamilyAgree) now uses the
  new `sig_outside_det` / `ascending_det`, proved in Audit.v and
  ExecutionModel.v.
- FreeVisitorCore: `signature_type_syms_filter` now also gives the sorted
  remainder (`nsort_in`, `nsort_ascending`). Its users were updated:
  FreeTypesExec, ResultMentionsCover and ResultMentionsExact.
- `RMMeth` users: PublicWorldFormation (`sig_mentions_typed`, new),
  XBridge and RelFamily (`get_sig_in`, new). RelSupport's `appof` tactic
  now asks the example for a `sig_in_reqs` proof.

No obligation statement changed. No new premises.

## FamilyCoherentOk: not restated; why it should now hold

The statement is unchanged; I believe it is plausibly true as it stands.

`FamilyCoherent C Cr ks` compares a method provision of C with one of Cr
at the same mkey (receiver r, method m) and equal canonical θ, and asks
that the signatures be equal.

1. Both provisions are made by IEMeth, OIEMeth or a bind of m, so their
   signatures are `SigOf` readings of m's one declaration. Each is read
   at the same type map as its `AppOf` (bs ++ ob ++ tm), with `this` = the
   receiver r.
2. A `TyOf` reading of a type expression depends on the type map only at
   the symbols `FreeIn` reports:
   - `FType` heads;
   - an unbracketed nominal's requirements (`FNom`);
   - bracket right sides (`FBracket`).

   ATThis is the receiver, which the key fixes. Now θ has an entry for
   every such symbol of m's signature, not only those in m's assume
   nesting. That was exactly the gap in sig-nonreq-shadow, where U was
   free but unrecorded.
3. The θ list's order is a function of m alone: `Select` over `w_reqs`,
   then ascending. So equal θ means equal canonical meanings at every
   symbol the signature reads. Since provisions are stored canonical, the
   two readings are equal.
4. `known` drops None entries, but every symbol `FreeIn` reports must be
   in force for `SigOf` to hold. Neither side has a None where the other
   has Some, so equal θ lists cover the same symbols.
5. Rebinding a symbol between the two provisions (the BTypeBind route
   that broke the planned SigCoherent argument) now changes θ, so the
   keys differ and FamilyCoherent holds vacuously.

Residual risks for the next hunt:
- whether the canonicalization (`canon C` and `canon Cr`, under different
  `c_sub`) commutes with the readings as step 3 assumes;
- family results that reach a method provision by a route other than a
  `SigOf`/`AppOf` pair at one type map, for example copies under
  `provision_subst`.

The SAFETY-SITES hypotheses-table text for `family_coherent` already
describes this intent ("both are SigOf readings at θ").

## Remaining

- `FamilyCoherentOk` is unproved (by instruction); hunt it again first.
- Native already prints 42 on the counterexample. Native's analogue of the
  D62 rejection of partially-bound-merge was not checked: native still
  accepts that file.

## Time

About 3 hours, including the first audit round (stopped and reported).
