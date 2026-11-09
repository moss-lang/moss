# realize3: status (stopped and reporting)

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
