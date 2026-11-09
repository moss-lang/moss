# results2: MeaningsScoped / ChainCoreOk / FamilyLinkResultsOk — STATUS

Branch ob-results2. safety is merged. check.sh passed at 712aecd. Time:
about 3 hours.

## For family2: no formation hypothesis from results2

`DeclaredScopedOk` (and `MeaningsScoped` in `Pre`) are **no longer
needed**. The coordinator chose option (A): Audit's `result_environment`
now shadows an occupied destination instead of being Stuck (commit
712aecd, scratch/orch/results2-AUDIT.md). Nothing in `Pre` or `FormedCx`
needs to bound the environment's meanings any more.

## Done

- **Audit change (712aecd):** dropped b4f8279's `fresh_result_types`
  check in `result_environment`.
  - Counterexamples: bootstrap/examples/const-kept-output.moss and
    lambda-declared-output.moss. Audit accepted both and its semantics was
    Stuck. The oracle runs both, and native runs the lambda one.
  - PublicGenerationFacts and PublicGenerationCallExample are restated to
    check shadowing, with generation preservation kept under the
    statically given freshness.
  - Recorded in SAFETY-SITES (Observations, first entry; stuck-site table)
    and results2-AUDIT.md.
- **PublicLinkResults:**
  - `MeaningsScoped` is removed.
  - `dest_scope`: destinations are out of the caller's table keys,
    `context_gens` and frame types.
  - `result_environment_ok` needs no scope premise.
  - `pre_after_result` builds Pre after the result from the caller's Pre.
  - `chain_results_scoped : ChainCoreOk -> (forall C, FormedCx W C ->
    DeclaredScoped W C) -> CA.ChainResultsOk ...`
  - `DeclaredScoped C := forall x, In x (context_gens C) -> Fresh W x \/
    In x (map fst (c_ty C))`. It is used only by `dest_scope`, for an
    output installed as itself.

## Open issue (needs a decision): kept outputs mentioned by the context

`dest_scope` needs a kept output T (installed as itself) to be out of
`context_gens C`. Otherwise the result's write `T |-> m` re-grounds types
the context mentions, such as a provision key or signature over T. Its
premise `DeclaredScoped` is false after family-unbound-need.moss, and
lambda-declared-output.moss is a concrete accepted program where T is
mentioned (via `T.foo`) and kept. Options:
1. An Audit kept-output clause `~ In t (A.context_gens C)` in
   `BApplyAbstract` (the old MadeFor clause, which matched the oracle on
   the corpus). It would reject lambda-declared-output.moss, which both
   implementations accept.
2. Prove that re-grounding an unbound declared symbol is harmless.
   Before the write, `ground d (TAbs T) = TAbs T`. A value can have a type
   mentioning an abstract T only through provisions whose `CallableOk`
   is stated at grounded types, so this is not obvious.
Until then, `ChainResultsOk` from `ChainCoreOk` carries this premise.

Variants, run on the oracle and native (results2-AUDIT.md section 2):
- (i)+(ii) (kept-output-family-method.moss): both accept and run it
  correctly.
- (iii) (kept-output-captured-fn.moss): both reject it statically.

There is no implementation bug. Re-grounding is harmless because an
unbound declared symbol is mentioned only by parametric family method
provisions. Proving that is large (a parametricity argument for
CallableOk). The cheap alternative is the Audit clause
`~ In t (context_gens C)`, which rejects programs that both
implementations accept and run correctly.

## Not done

- **ChainCoreOk** (PublicLinkResults, Section Assemble) is not started.
  The predecessor's plan (results-STATUS.md, "Step 2") still applies
  unchanged, minus anything about MeaningsScoped. Pieces:
  - selection from body keys: `link_source_of`, `select_ok`,
    `realized_key` at Cb;
  - outputs readable: Pre3's AliasOk plus LinkData's OutM;
  - KeysMatch/AliasOk/EnvOk of C' at `append (result_of d3 outs gensof dr) d`;
  - for EnvOk, `PublicOutputSubst.provide_result_rel` with
    s1 = body meanings after `read_rename` and s2 = caller meanings;
  - the copy chain's CopyOutputs.

  Expect several hundred lines.
- **FamilyLinkResultsOk**: not attempted. It is the outs = [] instance of
  the same correspondence.
- safety_obligations is unchanged: ChainResultsOk and FamilyLinkResultsOk
  are still obligations.

## Files

- scratch/orch/results2/ms-frame-free-wip.diff: the abandoned frame-free
  MeaningsScoped edit of PublicBodySlice. Obsolete under (A), and kept for
  reference only.


## Update (280e5bc): premise in place; HANDOFF BRIEF for ChainCoreOk

The coordinator decided: no Audit clause. The kept-output gap is now the
single named premise `PublicLinkResults.KeptOutputsUnmentioned`:

    forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
      S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
      PublicFormation.FormedCx W C ->
      forall t, A.FunOutputs W u t -> A.nlook t (A.c_ty C) = None ->
        ~ In t (A.context_gens C)

It is for the user to decide later: an Audit clause or the parametricity
proof (results2-AUDIT.md section 2). `DeclaredScoped` is gone.

`safety_obligations` (PublicEntry) now lists `LRes.ChainCoreOk` and
`LRes.KeptOutputsUnmentioned` in place of `CopyA.ChainResultsOk`. The
proof derives it with `LRes.chain_results_scoped`. Module plumbing:
- `PublicLinkResults.LinkResults` instantiates `PA := PlatformApply`,
  with `CA := PA.CopyA` and `S := CA.S`.
- PublicEntry takes `PA := LRes.PA`, so both use one slice instance.
  Separate functor applications would give distinct inductives.
- SAFETY-SITES's entry paragraph and hypotheses table name the premise.

check.sh passes.

### Next: prove ChainCoreOk (PublicLinkResults, Section Assemble)

Once proved, drop it from `safety_obligations`: `chain_results_scoped`
then needs only `KeptOutputsUnmentioned`. The statement is in the file.
Under ChainCoreOk's premises:
- `LD := S.LinkData ... h0 ...`, the root's link facts, including:
  - `OutM`: outputs are keys of `c_ty Cb`;
  - `PR'`: `ProvideResult h0 Ch0 (TAbs o gen) Cr ks'`;
  - `RR`: `ResultRealizes Cb Cr ...`, comparing at `read_rename`;
  - `FB`: the code's sources are `link_sources Cb outs' gen Cr ks'`.
- `Pre3`: the body's final state.
- `AP`: the caller's `AppPremises`, with
  `PR : ProvideResult u C (TAbs o gensof) C' ks`.

Pieces to prove, in order of difficulty:
1. **Outputs readable.** `forall t, In t outs -> output_meaning d3 t <> None`:
   - Pre3's `AliasOk` (CtxLocals' third part) at a key of `c_ty Cb`
     gives `Some`.
   - `outs` = `outs'` as sets (`chain_outputs` with LD's OutsI;
     AP's OutsI).
2. **KeysMatch C'.** `C'` is `output_cx C tm' es`
   (`PublicFormation.provide_result_shape`). New provisions come from
   `entry_provision`, which files each under its own kind.
   `PublicFormation.apply_abstract_formed` may already give it, or look
   for a KeysMatch lemma for output_cx in PublicCallRealize or
   PublicFormation.
3. **Selection.** `select_provisions d3 (result_smap θ' D (link_sources
   ...)) = Some dr`:
   - use `select_ok` (PublicCallRealize): each source key must have a
     provision in d3;
   - `result_smap_in` and `link_source_of` (PublicBodySlice) give the
     source `rkey_of Cb (at_site Cb (output_renaming outs' gen k'))`;
   - RR says Cb provides it at that key; Pre3's EnvOk then gives a
     run-time provision (`S.realized_key`).
4. **AliasOk C' at d'**, where
   `d' = append_environment (result_of d3 outs gensof dr) d`:
   - an output t: alias `read_meaning d3 t`, and the table entry
     `t |-> canon C (TAbs (gensof t))` grounds to the same value, since
     the write `gensof t |-> read_meaning d3 t` comes first in d';
   - other keys: as in d, because `dest_scope` keeps destinations out of
     their types, and its first conjunct (`~ In (gensof t) keys`)
     separates aliases.
5. **EnvOk C' at d' (the core, the largest piece).**
   - Old provisions of C: `envok_scoped`-style transfer. Meanings change
     only at destinations, which are out of `context_gens C` by
     `dest_scope`. Provisions: `dr`'s keys may shadow C's keys at the
     same run-time key. Those are exactly the keys that C' re-files, by
     `ResultKeys`/`output_cx`.
   - New provisions: relate the caller's entry (`ProvideResult` at
     `output_type_map C outs (TAbs o gensof)`) to the formal entry of
     Cr, read through the copy chain (`CopyChain`'s
     `CopyOutputs`/FunOutputs steps), with
     `PublicOutputSubst.provide_result_rel`:
     - `s1` = `(gen t |-> read_meaning d3 t)` over θ grounded in d;
     - `s2` = `type_meanings d'`.
   - The value at dr's key is d3's value at the body key, typed by
     Pre3's EnvOk at `ground d3` of Cb's type. RR equates that with
     `canon Cb` of the promised type under `read_rename`. `ground d3`
     of that equals `ground d'` of the caller's type: inputs through
     `application_environment` read θ in d, outputs through the
     aliases.
   - Expect several hundred lines. Start with the length-zero chain
     (`CCRefl`, h0 = u): `link_results_of_chain` is that case. Then
     generalize along `CCStep`.
6. **FamilyLinkResultsOk** (PublicFamilyStep.v): the same correspondence
   with outs = [] and `result_substitution th []`. Pieces 1 and 4 vanish.
