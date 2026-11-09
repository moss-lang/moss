# refile: Audit.v changes

## 1. No refile in a linked body; results read from their source keys

**What.**
- `functor_code` has a new last field `fc_sources : list (item * item)`.
  Each entry pairs a result key, as the functor declares it, with the key
  the body left that item under. An item that is not listed is read under
  its own key.
- New definitions: `source_key sources i` (the first matching entry, else
  `i`) and `result_smap th D sources := map (fun q => (fst q, source_key
  sources (snd q))) (inst_smap th D)`.
- `apply_functor` selects
  `select_provisions d3 (result_smap (result_substitution th outputs) Dr (fc_sources code))`.
  It used `inst_smap ... Dr` before.
- `LinkBody` (`LB`):
  - The `binds` variable and its equation `binds = flat_map (refile pairs) binds0` are gone.
  - `FamilyBindings` is now about `binds0`.
  - The body's code is
    `FunctorCode inputs linked (map ITy outs ++ map (fun k => output_renaming outs gen (rkey_of Cr k)) ks) binds0 (map (fun k => (output_renaming outs gen (rkey_of Cr k), rkey_of Cb (at_site Cb (output_renaming outs gen k)))) ks)`.
- The `refile` definition is removed. It was unused elsewhere: LinkOutputRefile and LinkBodySound use ExecutionModel's own `refile`.
- `Family` (platform functors) puts `[]` as `fc_sources`, so every output
  is read under its own (runtime) key. The examples (AuditExamples,
  PublicClosureExample, PublicGenerationCallExample,
  PublicFunctorScopeExample, PublicOutputTransferExample) also use `[]`.

**Why.** `RefileRunOk` was false. bootstrap/examples/refile-stale-shadow.moss shows this:
- refile's extra install, made right after the method bind, overwrote a stale provision (`T.out` bound while T was still G's input). The body's context still listed that provision under the same runtime key.
- A later call through it ran `Int.out` on a Bool. Evidence: the oracle plus the reference interpreter print `[moss_ref: stuck (XErr 14)]`, or 8 with `() => 8`. Native prints 7.
- Native does not expose F's spelling inside the body. The caller still gets the body's `Int.out` as `T.out`: scratch/refile/v4.moss prints 6 on both implementations.

The coordinator chose native's behavior.

**Acceptance.** The change rejects nothing. LinkBody lost a defining
equation and gained no premise, so the accepted programs are unchanged;
only the runtime meaning of linked bodies changes, and it now matches native.

**Alternatives considered.**
- A LinkBody premise forbidding the shadow. It rejects a program native accepts.
- Refile only under a key that is new. This is unsound: the caller would
  get the stale `T.out`, which is typed at G's input.
- Keeping refile and adding a `RefileShadowOk` obligation. That obligation
  is false today and unprovable for abstract chains.

**Proof impact.**
- PublicBodySlice:
  - `LinkData`'s code conjunct uses `binds0` and the new `link_sources`.
  - `apply_functor_eq` and the closure clause read `result_smap`.
  - `RefileRunOk` and `refile_run` are deleted. `app_step` runs the body with `chain_run`, and a run that ends out of fuel is an OutOfFuel outcome.
  - `LinkResultsOk` now takes the run of `binds0` and the selection by
    `result_smap ... (link_sources Cb outs' gen Cr ks')`.
  - New lemmas: `result_smap_in` and `link_source_of`.
- PublicEntry: `safety_obligations` no longer has `RefileRunOk`.
- PublicClosurePrototype's `compiled_apply` reads sources from `view_functor`.
- The earlier PublicRefile.v is deleted, since it is now moot.

**Oracle.** The oracle's `link_trace_rebinds` still refiles at bind time and
gets stuck on the example. That is roadmap task E31.
