# roles: separate output roles (E33/E06) — STATUS

Branch ob-roles, from safety 67343d6 (safety had nothing newer at the
last merge). check.sh passes.

## Phase A (done): the Audit change, commit cff9949

Record: scratch/orch/roles-AUDIT.md; SAFETY-SITES Observations (the E33
entry, now "fixed in Audit").
- `functor_code.fc_roles` (output symbol t, role gen t); Platform.Family `[]`.
- `LinkBody` files keys `rkey_of Cr k` (roles kept, no output_renaming);
  fc_sources' first components likewise; fc_roles = `map (t, gen t) outs`.
- `apply_functor` selects with
  `result_substitution th (role_outputs (fc_roles code) outputs)`
  (`role_outputs`, `output_destination` new in Audit.v next to
  result_substitution).
- Restated (terms only): LinkData/link_sources/link_source_of/
  apply_functor_eq (PublicBodySlice), LinkResultsOk, ChainResultsOk,
  ChainCoreOk, ChainNewOk, ChainKeysOk, ChainRealizeOk,
  FamilyLinkResultsOk, FamilyLinkNewOk. All existing proofs went through
  with only the new terms and the extra FunctorCode field; the 8 example
  codes take `[]`; PublicClosurePrototype's compiled_apply mirrors the
  new selection. New examples: PublicSavedTypeExample
  `output_role_key_goes_to_destination`, `merged_input_key_stays_input`.
- Native: functor_input_output_generations.moss prints `yn`,
  output-merge-key.moss prints 7. Audit's application now installs as
  native does for this shape.

### Oracle side: nothing broke; no oracle change
The oracle runs ExecutionModel's interpreter (XBApply/xinterp_apply)
with its own `Elaboration.LinkBody`/`output_renaming`. Audit's binding
syntax, functor_code and interpreter are not shared with it, and
LinkBodySound / LinkOutputFresh (`link_output_renaming_actual`) /
ElabSound relate Elab.v's link_body to ExecutionModel's LinkBody only.
All compile unchanged. So the (a)/(b) decision did not arise. The oracle
still gets this shape stuck: an oracle-conformance gap (E06/E33),
unchanged.

### Coordinator's alternative (install under the caller's keys)
Assessed in roles-AUDIT.md, "Alternative weighed". Summary: the better
end state (fixes E33, E32 and chainfwd's linked-copy-bracket at once,
makes ChainKeysOk/ChainForwardKeys true by construction, would let E32's
two-way CopyOutputs be reverted; platform families fit, with sources =
identity on `rkey_of outputs`; oracle proofs unaffected) but 2-3x the
roles change: it needs a context-independent identity for F's declared
items (keys at the root's context conflate in E33's shape unless roles
are kept, so positional), a per-copy result map in BCopyFunctor and the
FunctorClosure value (~70 proof sites, ClosureOk/CopyChain), and the
caller's keys in every BApply emitter (BApplyAbstract, LitOp, Demand,
Provided). It touches invariants chainfwd and family4 are editing now.
Roles was small and is done. I recommend the caller-keyed install as the
next planned semantic change (it subsumes fc_roles).

## Phase B: started only (helpers)

`PublicChainNew.v` (Static section), proved:
- `output_destination_map`: `output_destination (map (t, f t) outs) t = f t` for t in outs.
- `role_subst_role_at` / `role_subst_role`: Θ(gen t) = gensof t
  (Θ = result_substitution th (role_outputs (map (t, gen t) outs) (map (t, gensof t) outs)), NoDup (map gen outs)).
- `role_subst_input`: a symbol that is no role reads by θ.

Not proved: length-zero `ChainKeysOk` and `ChainRealizeOk`. Plan
(results4-STATUS, updated for roles): generalise PublicFamilyAgree's
`FamilyAgree` section (stated for FamilyApplication, no outputs) to
AppPremises with Cf = C and outputs. Use `family_entries_rel` with
s1 = Θ, s2 = c_sub C, T1 = output_type_map Cu outs (gen),
T2 = output_type_map C outs gensof. Agreement at an output t:
Θ(canon Cu (TAbs (gen t))) = TAbs (gensof t) (gen t fresh, so not in
Cu's sub; then role_subst_role) vs canon C (TAbs (gensof t))
(canon_abs_free; need gensof t outside context_gens C: fresh when in
force; when gensof t = t, t is not in C's table — check its sub). At an
input x: Θ = θ there (role_subst_input, gen x fresh so x is no role),
and θ absorbs Cu's σ (th_sigma). With roles there is no longer a raw
output symbol read as the new output, so the input entries agree
without results4's reverted premise. Then keys via InstSome as in
`family_items_agree`; mind duplicate root items (choose the caller key
that rkey_of C' reads, or use the last-pair lookup chainfwd describes).
Do this only if the caller-keyed alternative is not adopted (it makes
ChainKeysOk unnecessary).

## Time
About 1.5 hours.
