# refile: status

## Outcome

`RefileRunOk` was false, and it is now removed rather than proved. Audit
behaves like native: a linked body runs only its own binds, and the
result is selected from the body's own keys (scratch/orch/refile-AUDIT.md).
`app_step` runs the body directly with `chain_run`. `safety_obligations`
has one fewer obligation, and check.sh passes.

## How result selection works now (for LinkResultsOk / ob-results and ob-platform)

- Functor code has `fc_sources : list (item * item)`.
- `apply_functor`, after running `fc_bindings code` to d3, selects
  `select_provisions d3 (result_smap θ' (fc_outputs code) (fc_sources code))`
  where `θ' = result_substitution th outputs` and
  `result_smap θ' D src = map (fun q => (fst q, source_key src (snd q))) (inst_smap θ' D)`.
  Each non-type item i of D gives the entry `(app_subst_item θ' i, source_key src i)`.
  The caller's key is i read at θ', and it is looked up in d3 under `source_key src i`.
  `source_key` returns the first pair whose left side is `item_eqb`-equal to i, else i itself.
- `result_environment d outputs d3 Dr dr` is unchanged.
- Linked body (LinkBody / `S.LinkData`): `fc_outputs = map ITy outs ++ map (fun k => output_renaming outs gen (rkey_of Cr k)) ks`,
  `fc_bindings = binds0`, and `fc_sources = S.link_sources Cb outs gen Cr ks`, which is
  `map (fun k => (output_renaming outs gen (rkey_of Cr k), rkey_of Cb (at_site Cb (output_renaming outs gen k)))) ks`.
  - For a method k, the source is the body's runtime key `rkey Cb r m` of its receiver read in the body. EnvOk at Cb finds that key in d3, since ResultRealizes gives the method in `c_provisions Cb`.
  - For vals, fns and functors, `at_site`, `output_renaming` and `rkey_of` leave the key unchanged (up to the renaming of output types, which these keys do not mention).
- Lemmas in PublicBodySlice:
  - `result_smap_in`: an entry (a, b) comes from some i ∈ D with `is_ty_item i = false`, `a = app_subst_item θ' i` and `b = source_key src i`.
  - `link_source_of`: for k ∈ ks, there is a k' ∈ ks with the same F-key such that
    `source_key (link_sources ...) (F-key k) = rkey_of Cb (at_site Cb (output_renaming outs gen k'))`.
    This is the first such k'. Distinct ks with the same F-key are possible only if the keys coincide.
- `LinkResultsOk` now assumes `I.eval_bindings j' P de [] st binds0 = inl (d3, g3, st3)`
  and concludes with `select_provisions d3 (result_smap ... (link_sources Cb outs' gen Cr ks')) = Some dr`.
- Platform families (`Family`): `fc_sources = []`, so every output is read under its own key.

## Commits on ob-refile

- 2a9a80f: the example.
- 0afa8e7: the first approach, PublicRefile.v with `RefileShadowOk`, now superseded.
- The final commit: the Audit change, the slice and entry update, the
  deletion of PublicRefile.v, E31 in proof-plan.json, and the docs.

## Not done

- The oracle fix (E31). The oracle's `link_trace_rebinds` still refiles at bind time.
- `LinkResultsOk` itself. That is the results agent's task.

## Time

About 3h in total.
