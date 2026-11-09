# realize3: status

## Round 2 (coordinator: take repair 1, then steps 2-4; assess rep choice)

- **Audit change fc75de7** (repair 1): link_site reads input symbols at G's
  table and output roles at the body's. See scratch/orch/realize3-AUDIT.md.
  Corpus: 414 same, 1 different (output-merge-key.moss, which the old rule
  rejected and the oracle accepts). check.sh is green (68s).
- **Assessment: matching the oracle's merge representative.** See below.
- Steps 2-4: see "Round 2 progress" at the end.

### Would constraining Audit's representative to the oracle's close this class?

**The oracle's rule** (Elab.v, `unify`, line ~1405; the val/fn re-declaration
site, line ~1845):
- `unify e old y` canonicalizes both sides. If the earlier reading `old` is
  a generation `YAbs g`, it records `g ↦ y`.
- So the earlier-read generation is merged into the later reading. The
  representative is whichever comes later in the walk's item order.
- `merged_walk` reruns the walk with the edges it found until they settle.

The choice depends on order. bootstrap/examples/output-merge-key.moss (context
`A, X, T, A.gimme[Foo=X], A.gimme[Foo=T]`) has the oracle's representative T.
The earlier claim that it is "X in either order" (realize2) cannot be read off
the error messages. In my five reorderings of link-output-input-rep.moss
(scratch/realize3/v1..v5) the oracle reports the same message, which is
consistent with it not depending on the representative at all.

**Why the oracle is safe anyway.** It keeps symbols (`en_tn`) apart from
generations (`en_uf`). A rebind such as `bind T = Char` writes a new `tn`
entry for the symbol T. It never changes what a merged generation means. A
key read at F's inputs refers to generations, and the link check maps only
the output roles to the body's meanings (`made_tys`). So whichever
representative the walk picks, nothing reads an input generation through a
table the body rebound.

**The three holes have one shape.** In each, Audit read an input generation
of F, spelled `TAbs x` as Cu's table and σ spell it, through a table the body
can write:
- link-merge-rep-rebind: the body's at_site Cb (fixed by linknewpf's
  link_site);
- link-kept-output-rep: a kept output writes the body's table at T (fixed
  by realize2's made clause);
- link-output-input-rep: link_site's entry for an output symbol (fixed
  now).

The representative choice only decides which symbol's spelling the key
takes. The defect is the re-reading.

**Verdict: no, it would not close the class.**
- Pinning Audit's representative to the oracle's would rule out only the
  orders in which the oracle picks the harmless symbol. Each of the three
  tests has a reordering in which the oracle's own representative is the
  dangerous one (output-merge-key.moss shows such orders exist). Audit
  would then still accept it, unless the readings were fixed anyway.
- The principled invariant is: an input generation of F is read only
  through θ (at an application) or G's own table (in a linked body), never
  through a table the body or caller can rebind. With the three repairs,
  link_site, read_rename (roles only) and the run fact
  (body_meaning_input) respect it.
- A worthwhile follow-up is an audit of other places that apply a final
  table to Cu-generations: CopyOutputs' sigma, copy sites, and the family
  restore.

**How much proof it would touch (if done anyway).**
- MostGeneralMerge sits inside FunCtx and w_context, so every FunCtx
  destruct pattern would change, mechanically: dozens of sites in
  PublicCallRealize, PublicFamilyAgree, PublicCopyFresh, PublicFamilyLink,
  PublicFormation and PublicScoping.
- Consumers use only MGM's algebraic properties (unifier, idempotent,
  most general, range in the telescope), so their proofs would survive.
- The real cost is on the construction side:
  - Audit would need a deterministic specification of the oracle's walk
    (item order, unify's direction, the merged_walk fixpoint rounds).
  - WorldOf and PublicWorldFormation would have to show that such a σ
    exists and is most general.
  - Oracle conformance (ElabSound and friends) would have to show the
    oracle computes it.
- That is a new formal development, on the order of the existing merge
  formation proofs, with no safety gain over the reading fixes.

## Round 1 (stopped and reported)

Branch ob-realize3, from safety 6dcd915. About 1.5 hours. No Audit change.
`safety_obligations` is unchanged: CN.ChainRealizeOk and FS.FamilyCopyRealizeOk
are still assumed.

## Stopped: step 2 (CCRefl case of CN.ChainRealizeOk) looks false

Test: bootstrap/examples/link-output-input-rep.moss. The full argument is in
its header; I read the rules by hand. Control:
bootstrap/examples/link-output-input-rebind-control.moss.

- It is realize2's link-kept-output-rep.moss with the write done by
  `bind T = Char` (BTypeBind) in G. BTypeBind has `made = []`, so LinkBody's
  made clause (4d55710) does not see it. T is an output of F, and
  M(F) excludes outputs, so T is not a pin either.
- F[X]: Merge, Std -> T, X.m. T is both an input (through Merge) and an
  output of F. Merge merges X with T. With the world's representative T
  (MostGeneralMerge allows it, as in realize2's test):
  - F's key X.m reads at F's inputs as T.m, with the *input* T, through
    Cu's table, not the output role gen T.
  - `A.link_site` reads every output at Cb, the body's final table.
    That includes an input occurrence of an output symbol, so the key
    reads as Char.m, which G binds.
  - The caller (X = T = Bool) reads X.m at its own table as Bool.m and
    files G's Char.m closure under it. `true.m()` would run `putchar` on
    a Bool.
- Oracle (ob-sigclose var moss_m2): rejects ("functor `G` does not bind
  `X.m`"), since its representative is X. Native (ob-family seed
  1h1710x0): rejects at `bind F = G`.
- Control: both accept an input of F that is also an output, with no
  merge, when G rebinds it. Both print 7. So forbidding that rebind
  outright would reject accepted programs.

Only method receivers are affected. Value, fn and functor types read an
input T through `read_rename` (it changes only roles), and d3's
*meaning* of T (not its alias) is the caller's θ reading
(body_meaning_input).

Candidate repairs (for the coordinator; neither is made):
1. Read an output role at Cb, and input symbols at G's table only. Make
   `ren` send `gen t` straight to Cb's (canonical) meaning of t, and make
   `link_site`'s table `c_ty Cg`. The control still passes, since its key's
   receiver is the role. The test is rejected, since the input T reads as T.
   This touches realizes_link_site / canon_link_site and PublicFamilyLink
   (outs = [] there, so little).
2. A LinkBody clause that an output of F which is also an input is no other
   input's merge representative in Cu:
   `forall t y ty, In t outs -> nlook y (c_ty Cu) = Some ty -> y <> t -> ~ In t (gens ty)`.
   This is smaller, but it needs a corpus comparison: whether the oracle
   ever accepts such a merge is untested.

## Proved (commit below; check.sh green)

- `PublicLinkRun.LinkRun.link_input_meaning` is the run fact, extracted
  from linknewpf's inline proof and generalized to any linked body (not
  only family bodies). For x an input of both G and F, d3's meaning is
  `ground d (θ x)`. Premises: FunCtx of G, cap3b's input condition,
  BindItems/BindsOf, LinkBody's made clause, funbody_of, θ defined on dom
  Cu, and `θ ∘ canon Cu = θ`.
- `LinkRun.link_run_fact`: ground d3 t = ground d (θ t) for t over
  dom Cg ∩ dom Cu.
- PublicFamilyLink.family_link_new_ok now uses link_run_fact in place of
  its inline MeanX/RunFact (the w = [] computation is gone). Statement
  unchanged.
- The R2 scope argument (items_scope) is still inline in
  family_link_new_ok. With outputs it would also need `made` avoiding
  dom Cu, which LinkData has.

## Plan notes for whoever continues step 2 (after a repair)

- Positions and relation: use PublicOutputSubst.output_entries_rel for
  vals, fns and functors, and PublicFamilyAgree.family_entries_rel for
  methods (its statement is generic in u, s1, s2 and K). Both apply at:
  - tm1 = output_type_map Cu outs (TAbs ∘ gen) and tm2 = output_type_map C
    outs (TAbs ∘ gensof);
  - s1 = θ ++ [gen t := TAbs (gensof t)] and s2 = c_sub C;
  - K = dom Cu ++ map gen outs.
  DomT's second half is CF.app_keys_typed. ResultMentions ⊆ dom Cu, as in
  mentions_dom (F is abstract, b = None).
- The key transfer is pointwise, with N x = ground d (θ x) for x in dom Cu
  and N (gen t) = read_meaning d3 t:
  - ground d3 (rr t) = N t, by link_run_fact and AliasOk/Absorbs of Cb at
    d3 for the roles;
  - ground env' (s1 t) = N t, by dest_scope and result_of;
  - N ∘ σ = N, by th_sigma.
  It needs R2: a formal type's dom-Cu gens lie in dom Cg.
- Step 3/4: not started. Families have no outputs, so step 4
  (FamilyCopyRealizeOk) is not affected by this hole. It still needs step 3's
  CopyChain induction with PublicCopyFresh.copy_outputs_read, and the run of
  the root.

## Round 2 progress and handoff (steps 2-4 not proved)

I stopped here to keep a clean, compiling handoff. My context is large after
the Audit change, the corpus run and the assessment. `safety_obligations`
still assumes CN.ChainRealizeOk and FS.FamilyCopyRealizeOk. Commits:
fc75de7 (Audit), 52af307 (status), and earlier 2d5567d (run fact).

### What the new link_site makes simpler for step 2

The site is `Ls := A.link_site Cg Cb outs' gen` and the renaming is
`ren := A.link_renaming outs' gen`, the identity on the roles. Then:
- **Promised types.** `read_rename Ls ren` sends gen t to `canon Cb m_t`,
  where m_t is Cb's meaning of t (OutM gives it). It leaves every other
  symbol alone.
- **Receivers.** at_site reads gen t at Cb (m_t) and every declared
  symbol y at `c_ty Cg`.
  - Roles are Fresh and Cg's keys are declared
    (PublicCopyFresh.functx_keys_declared). So for y ∈ dom Cu ∩ dom Cg the
    table entry is `Cg.ty y` even when y is also an output of F. The
    output-input case of round 1 is gone.
- **The pointwise target.** For a formal type t (gens in dom Cu ∪ roles,
  PublicCopyFresh.result_gens), define N by:
  - N y = ground d (θ y) for y ∈ dom Cu;
  - N (gen t) = read_meaning d3 t.

  Then:
  - ground d3 (rr t) = N t. Inputs: LRn.link_run_fact / link_input_meaning
    (needs the dom-Cu gens ⊆ dom Cg, "R2"). Roles: AliasOk Cb d3 (in Pre3's
    CtxLocals) and Absorbs Cb d3.
  - ground env' (s1 t) = N t, with s1 = θ ++ map (gen t ↦ TAbs (gensof t)).
    This uses LR.dest_scope (gensof t avoids C's scope), result_of (first in
    append_environment), and gensof injective on outs (AppPremises).
  - N ∘ σ_Cu = N, by th_sigma-style reasoning (Satisfies' merges, as
    PublicFamilyAgree.th_sigma).
- **Relating the readings.** Relate the caller's entries (tm2 =
  output_type_map C outs (TAbs ∘ gensof)) to the formal ones (tm1 =
  output_type_map Cu outs (TAbs ∘ gen)) at s1 / s2 = c_sub C:
  - vals, fns and functors: PublicOutputSubst.output_entries_rel;
  - methods: PublicFamilyAgree.family_entries_rel (generic in u, s1, s2,
    K). Its K = dom Cu ++ map gen outs. Its DomT second half is
    CF.app_keys_typed. Its ResultMentions ⊆ dom Cu, as
    FamilyAgree.mentions_dom, using b = None since F is abstract.
  - Positions: the last caller position for a key is also the last formal
    position for the related key (the caller key is a function of the
    formal key). Then use PublicCallerKeys.selection_at / selection_last,
    as in family_link_new_ok steps 4-5.
- **R2** (a formal type's dom-Cu gens lie in dom Cg). Generalize
  family_link_new_ok step 6, still inline, into a lemma over any kind of
  provision:
  - Use PublicLinkScope.items_scope (scope grows only by made) and
    LinkData's made clause (made avoids dom Cu).
  - Functor provisions are not covered by provision_refines (no signature),
    so their fixed readings need their own scope argument, through
    BodyReal's gfx clause and the body's CxScoped.
- **EnvOkOn's functor clause** also needs FixedMeans' first half for C''s
  functor entry (every Fixed input has a reading). Take it from
  FormedCx/CxScoped of C' (PublicFormation.apply_abstract_formed,
  PublicScoping.apply_abstract_cxscoped).

### Suggested structure

- New file PublicChainRealize.v, module `ChainRealize (Prims) (I0)` over
  PublicFamilyLink.FamilyLink's CF.CN, so the instances match PublicEntry.
- `chain_realize_refl`: the CCRefl case.
- `ChainRealizeCopyOk`: ChainRealizeOk with the chain built by CCStep. This
  narrows the obligation, as callerkeys2 did with ChainForwardCopySigs.
- `chain_realize_ok : ChainRealizeCopyOk -> CN.ChainRealizeOk`.
- PublicEntry then assumes ChainRealizeCopyOk in its place.

### Step 3 and step 4

Unchanged from realize2-STATUS.md:
- Step 3 is the CopyChain induction with PublicCopyFresh.copy_outputs_read
  (tau = the application's meanings, rho0 = d's meanings at the copy
  site).
- Step 4 is the family version (outs = []). Its root is a linked root
  (family_link_new_ok's argument) or a platform root (family_platform_ok),
  and it must also run the root's code (the apply_functor clause of
  FamilyCopyRealizeOk).
