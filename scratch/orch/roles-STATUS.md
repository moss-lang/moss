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

## Round 2: caller-keyed install (decided) — NOT started; design handoff

`safety` c60e2b7 merged (fast-forward; it already holds cff9949 and
48e5106). I stopped here because my context is too tight for a refactor
of this size. Nothing was changed for round 2; the tree is clean at the
merge. Below is the design I worked out, for the next agent.

### Audit.v
1. `functor_code`: drop `fc_roles`. `fc_sources : list item` holds the
   body's key for each position of the root's declared result (the i-th
   key of its `ResultKeys`). `fc_outputs` is only read by
   `read_result_types` (the `ITy` membership) and `result_environment`,
   so linked code gets `map ITy outs` and a family's code gets `[]`.
   - LinkBody: `fc_sources = map (fun k => rkey_of Cb (at_site Cb (output_renaming outs gen k))) ks`
     (body side as now). The caller-side keys no longer appear in the
     code, so the roles and output_renaming problem (E33) disappears.
   - Platform.Family: `fc_sources = map (rkey_of outputs) keys`, which
     is what its BPrimitive bindings install.
2. Positions are context-independent: `ResultKeys s es ks` is a
   flat_map of `entry_key s e`, and `entry_key` is None exactly for
   `NTy` (Audit.v ~3046), independent of s. So F's ks at the caller (C)
   and at F's own context (Cu) align when the entries come from the
   same declaration. Lemma needed: two OutputItemsEntries of one result
   have Forall2 entries of the same kind (PublicFamilyAgree's
   `family_entries_rel` already shows Forall2 of EntryRelM, which pins
   kinds; a kind-only version is easier).
3. `BApply u th outputs sm rs`, where `rs : list item` holds the caller's
   key for each position of F's declared result. Emitters:
   - `BApplyAbstract`: `rs = map (rkey_of C') ks`. C' and ks come from
     the rule's ProvideResult at C, so these are the keys the caller's
     static context records.
   - `LitOpFamily`, the Demand family rule (~3945) and the Provided
     family rule (~4186) use `apply_expression u th sm` and need
     `map (rkey_of C') ks` from their FamilyApplication. Change the
     `apply_expression`/`apply_output_expression` signatures.
4. Copies: `BCopyFunctor u u' sm rm` with `rm : list nat`. For each
   position i of F's result, rm gives the position j in H's result.
   `FunctorClosure h sms dc rms`, and a copy appends: `rms ++ [rm]`.
   Translate the root's sources through the chain:
   `chain_sources rms srcs := fold_left (fun s rm => map (fun j => nth j s (ITy 0)) rm) rms srcs`.
   The default is unreachable statically. At run time it gives Stuck
   e_compose, because no provision has key `ITy 0`.
5. `apply_functor`:
   `select_provisions d3 (combine rs (chain_sources rms (fc_sources code)))`
   replaces the result_smap selection. Delete result_smap/inst_smap/
   source_key/role_outputs/output_destination if unused.
   `read_result_types` and `result_environment` stay the same.
6. `BFunCopy`/`CopyOutputs`: CopyOutputs gains `rm`. Revert results4's
   converse conjunct (E32), since extra H keys are simply never
   installed. Then require `length rm = length ks` and, for each i with
   `nth_error ks i = Some k`, some j with `nth_error rm i = Some j`,
   `nth_error ks' j = Some k'`, and `provision_id k' = provision_id k`.
   Keep the existing provision equality under sigma.
   - Open point: is provision_id enough? E32's H has `Box.m[T=Bool]` and
     `Box.m[T=Char]`, which share a provision_id. A witness that maps
     F's `[T=Bool]` to H's `[T=Char]` would install the wrong closure
     under the caller's key, and ChainRealizeOk would be false for that
     witness, because Audit is relational and accepts any witness.
   - Suggested extra condition: `app_subst_item sigma k' = app_subst_item sigma k`,
     with keys as read at Cu with roles gen. Check it against
     linked-copy-bracket.moss: F's `Array.length[I32 = Z]` vs Arrays'
     `Array.length`. Z is fixed, and sigma reads it as I32 at the copy
     site, so they should agree. Run the corpus comparison
     (results4-cmp.sh style) before relying on it.
   - The BFunCopy rule picks rm existentially and emits it.

### Proofs (expected work)
- Mechanical: every `FunctorClosure h sms dc` pattern or term gains
  rms (~70 sites: PublicBodySlice ~22, PublicCopyApply, PublicChainCore,
  PublicPlatformApply, PublicFamilyStep, PublicLinkResults,
  PublicChainNew, PublicEntry, examples, PublicClosurePrototype).
  Every BApply/BCopyFunctor term gains its field. ClosureOk's CCopy and
  CopyChain's CCStep carry rm and the new CopyOutputs.
- Becomes true by construction: the keys dr fills are exactly
  `map (rkey_of C') ks`, by select_provisions/combine. This covers
  ChainNewOk clause (a), so ChainKeysOk (PublicChainNew) goes, and
  PublicPlatformApply's ChainForwardKeys plus PublicChainFwd's
  ChainForwardCopyKeys go. What remains is a value/type premise: the
  provision at `chain_sources ... [i]` in d3 realizes the caller's
  provision for `ks[i]`. That is the ChainRealizeOk/ChainForwardAgrees
  realization half. It now needs only ground-meaning agreement, which
  chainfwd says holds (CapturedFuns, FixedMeans).
- PublicPlatformApply's family selection proofs (`sm'`, `Filt`, `Inst`
  around lines 540-575 and 960-990) are rebuilt for
  `combine (map (rkey_of C') ks) (map (rkey_of outputs) keys)`.
  `last_prim`/`installs_lookup` still give the existence of each source.
- Remove `fc_roles` uses from the cff9949 restatements. Also remove
  `role_subst_*` and `output_destination_map` (PublicChainNew) and the
  two PublicSavedTypeExample role examples, or restate them.

### Tests to add (bootstrap/examples, header comments)
- `copy-extra-key.moss` (E32) must be accepted again and print 31 as
  native does. The oracle prints 21 (E32 oracle bug).
- `linked-copy-bracket.moss` (from ob-chainfwd; native 42, oracle stuck).
- `output-merge-key.moss` and
  tests/regressions/functor_input_output_generations.moss (native 7 and
  `yn`; oracle stuck, E33/E06).
- Oracle: no change. The oracle-relating proofs are untouched for the
  same reason as in round 1.
