# results agent: LinkResultsOk (branch ob-results) — STATUS

LinkResultsOk is NOT proved. It is false as stated, for two independent
reasons; one is fixed in Audit, the other needs Pre strengthened (not done:
Pre is shared by every case). What is proved isolates the remaining work.

## 1. False: kept outputs are written but were not "made" (Audit fixed)
`result_environment` writes a meaning for every output destination, so
`MeaningsGrow newgens` requires every `gensof t` in `newgens`. For an output
not in force (`gensof t = t`), `t` is a declared symbol and `FreshFor` made
all of `newgens` `Fresh W`, so this failed for every such application
(e.g. examples/captured-output-scope.moss). It also hid a real Audit hole:
examples/kept-output-escape.moss (accepted by Audit's D78 check, rejected by
the oracle and native; runs `x + 1` on `true`).
Fix (commit a79663e): `BApplyAbstract` requires `MadeFor W F C outs newgens`
(each member fresh or a not-in-force output, none in fr_fresh/context_gens/
frame types) and `In t newgens` for kept outputs. Obligation texts unchanged.
Details and oracle-variant evidence (356/356 same verdict):
scratch/orch/results-AUDIT.md. NOTE: the lead says ob-family makes an
equivalent change (made = map gensof outs); reconcile with one of the two.
`result_grows_by_destinations` proves MeaningsGrow over `map gensof outs`
directly for that form.

## 2. False: Pre does not bound the caller's meanings (NOT fixed)
`fresh_result_types` needs every destination absent from the caller's
`type_meanings d`. Pre says nothing about d's meaning domain: adding a
meaning for a generation outside C's keys, types and receiver keeps Pre
(checked: `PublicLinkResults.pre_junk_meaning`, via `pre_meanings_free`),
and the run before the result step is unchanged, but then the application
is Stuck (`result_environment_occupied`). So any reachable completed
application with an in-force output gives premises of LinkResultsOk with a
false conclusion. Needed invariant (proposed, to add to Pre):
  MeaningsScoped F C d := every x with a meaning in d is a key of c_ty C or
  in fr_fresh F ++ context_gens C.
It is plausible at every step (calls: θ keys + captures of context_gens,
which LambdaProvider puts in the body frame's fr_fresh; applications: keys
and fresh gens now in context_gens/fr_fresh; type binds add aliases only)
but threading it through Pre is a slice-wide change for the lead.
With it, destinations are free: `made_free` (uses FormedCx's TysScoped).

## Proved (bootstrap/theories/PublicLinkResults.v, PublicOutputSubst.v)
- `result_environment_ok`: with MeaningsScoped and the outputs readable in
  d3, result_environment = Some (result_of d3 outs gensof dr) and
  MeaningsGrow newgens d (append result d).
- `pre_after_result`: Pre at (with_fresh F newgens, C', append result d,
  H3, g, st3) from the caller's Pre, MeaningsGrow, heap extension and
  state_ok, given only AliasOk C' d', EnvOk rr C' d' H3 and KeysMatch C'
  (frame, locals, receiver, Absorbs, idempotence, formation all proved).
- `link_results_scoped : ResultCoreOk -> LinkResultsScopedOk`, where
  LinkResultsScopedOk = LinkResultsOk + premise `MeaningsScoped F C d`, and
  ResultCoreOk (same premises as LinkResultsOk) asks exactly:
  result selection succeeds (dr), outputs have meanings in d3, and
  AliasOk/EnvOk/KeysMatch for C' at append (result_of d3 outs gensof dr) d.
  ChainResultsOk (ob-copy) has the same conclusion; the same glue applies
  unchanged (it only uses AppPremises, LinkData's outs', d3 and Pre).
- `PublicOutputSubst.output_entries_rel` / `provide_result_rel` (requested
  by the lead for PlatformResultAgrees too): two OutputItemsEntries
  derivations of one spec list at tables that Agree after s1/s2 give
  one-to-one corresponding entries (EntryRel: same symbols; types,
  signatures, instantiations (where both Some), fixed readings equal after
  s1/s2). A method receiver symbol present in only one base table is
  reported as DomGap (impossible under DomSame).
check.sh green; all in assumptions/Certificate.v.

## Remaining (ResultCoreOk), with what I found
- Result keys: per the lead, ob-refile drops refile and selects each
  F-key (θ'-instantiated) from the body key at_site Cb (output_renaming k);
  so lookups come from Pre at Cb + ResultRealizes.
- Outputs readable in d3 (`output_meaning d3 t <> None`): not implied by
  Pre at Cb (AliasOk only fixes cmeaning). Needs a run invariant "every
  key of c_ty has an alias or meaning" (true: application env aliases all
  fc_inputs, BType and applications add aliases), or a premise on runs.
- Types: value at Cb typed at ground d3 tb; ResultRealizes gives
  canon Cb tb = canon Cb (rename tr) with rename = gen t -> TAbs t; caller
  wants ground d' tc. Use provide_result_rel with s1 = (rename then d3's
  meanings), s2 = d' meanings. CONCERN: ground uses meanings only, while a
  body's `bind T = τ` sets an alias; and ResultRealizes compares the
  promised signature (output as TAbs t) to the body's (read through c_ty
  Cb, so τ) only modulo c_sub Cb. For a body `bind T = Bool; bind mk =
  () => true;` with result `fn mk(): T`, TAbs T vs Bool looks unequal, i.e.
  Audit's LinkBody may reject the main functor pattern (the oracle instead
  substitutes the body's output meanings, Elab.v promised_result_type).
  Worth checking before proving the correspondence: if so, ResultRealizes
  should read outputs at their body meanings, which also changes the
  correspondence to prove.
- Old provisions of C at d': need dr's keys not to shadow them except
  where C' shadows statically (key correspondence through θ').

Time: about 3 hours.
