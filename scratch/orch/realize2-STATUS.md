# realize2: status (handoff)

Branch ob-realize2, merged up to safety 01e0647 (linknewpf). About 3 hours.
check.sh passes at every commit listed below.

## Audit changes (two commits; scratch/orch/realize2-AUDIT.md, SAFETY-SITES Observation)

1. **268bf3a** (approved). BApplyAbstract's kept-output case also asks
   `~ In t (fr_pins F)`.
   - Test: bootstrap/examples/link-kept-output-pin.moss. It is linknewpf's
     m7 made observable. The oracle elaborates it and is stuck at run
     time (XErr 14, proof-plan **E37**); native rejects it.
   - Control: link-kept-output-pin-control.moss.
2. **4d55710** (needs coordinator review: made after the approval,
   because (1) was not enough). LinkBody asks
   `forall t, In t made -> nlook t (c_ty Cu) = None`, and LinkData has
   it as its last conjunct.
   - Test: bootstrap/examples/link-kept-output-rep.moss. It keeps an
     output at T, an unmentioned merge representative of X, which an
     Audit world may pick. The oracle and native reject it.
   - (1) is now implied for linked bodies and could be dropped.
- Corpus (refined oracle variant, scratch/realize2/variant-Elab.patch):
  **401 same, 5 different** (the new programs only).

## Proved (step 1: the run fact)

- bootstrap/theories/PublicLinkPins.v:
  - `items_dests_pins`: every destination of the binds a run of items
    elaborates to is (Fresh or outside the frame's pins) and in `made`.
  - `link_dests_avoid`: no destination is a declared symbol of M(F).
  - `link_dests_out`: with LinkData's made clause, no destination is a
    key of c_ty Cu.
- bootstrap/theories/PublicLinkRun.v:
  - `app_env_meaning` (top level): the body's environment gives input x
    the meaning `ground d (app_subst th (table_reading code x))` when
    the table reading's symbols are linked inputs that th covers.
  - `LinkRun.body_meaning_kept`, `body_meaning_mentioned` and
    `body_meaning_input`: d3's meaning of every input of F equals de's.
- All of these are Certificate roots.

## Remaining

### Step 2: the length-zero chain, ChainRealizeOk with CCRefl

CCRefl gives u = h0, sms = [lnk], rms = [], Cu = Ch0 and ns = nsh0
(functx_unique). The goal is `CC.EnvOkOn (In . news) rr C'
(append (result_of d3 outs gensof dr) d) H3`. Plan:

1. **Positions.** C''s news come from `ProvideResult W u C (TAbs .
   gensof) C' ks`. Cr comes from `ProvideResult W u Cu (TAbs . gen) Cr
   ks'`. Both are the same OutputItemsEntries walk at different tables.
   - Model: PublicFamilyLink.family_link_new_ok uses
     `PublicFamilyAgree.family_items_pos` / `family_items_agree` (with
     outs = []).
   - Candidate: `PublicOutputSubst.provide_result_rel`.
   - The new part here is the output map: gensof at C against gen at Cu.
2. **Selection.** `PublicCallerKeys.selection_at` / `selection_last`
   (as in PublicPlatformApply.family_platform_ok). dr at `rkey_of C' k_i`
   is d3 at link source i.
3. **Body.** `body_provides` at `A.link_site Cg Cb outs'`, with Realizes
   from `realizes_link_site` and `ren = map (fun t => (gen t, TAbs t))
   outs'`. This gives BodyReal at `read_rename (link_site ..) ren`.
4. **Type transfer.** Show ground d3 (app_subst rr t_formal) = ground
   env' t_caller.
   - Input x in dom Cg and dom Cu: use `body_meaning_input` +
     `app_env_meaning`. Then cap3b's LinkBody condition (only the weak
     form is in LinkData; PublicFamilyLink recovers the strong form via
     link_body_parts / closure_cases) and `PublicFamilyAgree.th_sigma`.
     linknewpf's inline "run fact" in family_link_new_ok is this
     computation for outs = []; extract it as a lemma.
   - Gens outside dom Cg: linknewpf's R2 (items_scope,
     functx_scoped/gens_keep), inline in family_link_new_ok.
   - Output role gen t: read_rename gives canon Cb (c_ty Cb t). Its
     ground at d3 must be `read_meaning d3 t`, which result_of installs
     at gensof t. Use Absorbs Cb d3 and Pre3's alias facts, and
     LR.result_environment_ok / dest_scope for env' against d.
   - Receivers: as in linknewpf step 7.
5. `S.callable_transfer` from d3 to env', and value_typed transfer for
   vals. Functors (ProvidedFunctor) through BodyReal's gfx clause.

### Step 3: induction along CopyChain

Use `PublicCopyFresh.copy_outputs_read` per CCStep (realizepf-STATUS
"Remaining" 4):
- tau = the application's meanings of F's symbols;
- rho0 = type_meanings d at the copy site;
- discharge its three premises from FixedMeans and CopySite.

### Step 4: FS.FamilyCopyRealizeOk

The same with outs = []. The root is either FamilyLinkNewOk's argument
(now proved: PublicFamilyLink.family_link_new_ok) or platform
(family_platform_ok). Then remove CN.ChainRealizeOk and
FS.FamilyCopyRealizeOk from PublicEntry.safety_obligations.

## Notes

- Neither obligation is proved yet; safety_obligations is unchanged.
- The oracle variant tree is scratch/realize2/varsrc (ignored). Rebuild
  it with `nix develop <worktree>#bootstrap -c dune build --root .
  ocaml/moss_m2.exe` from inside scratch/realize2/varsrc/bootstrap.
