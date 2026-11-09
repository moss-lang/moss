(* ChainNewOk (PublicChainCore), split. Its first clause (every key the
   result selection dr fills is the run-time key of one of C''s new
   provisions) follows from a static fact, ChainKeysOk: each key of the
   root's formal result, renamed and substituted as the application's
   selection does, is the caller's run-time key of one of F's result keys.
   Its second clause (the new provisions are realized after the
   application) is ChainRealizeOk. ChainKeysOk needs CopyOutputs in both
   directions (examples/copy-extra-key.moss). *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicTypeFacts PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply
  PublicLinkResults PublicFamilyAgree PublicChainCore.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Static.
Variable W : A.world.

(* The keys of a result context's new provisions are the result's keys. *)
Lemma provide_result_key_news : forall u C out C' ks k, A.ProvideResult W u C out C' ks -> In k ks ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list -> exists p, In (k, p) news.
Proof.
  intros u C out C' ks k (d & ps & a & r & b & outs & es & tm' & _ & _ & _ & _ & RK & ->) Ik news En.
  cbn [A.output_cx A.c_provisions] in En. apply app_inv_tail in En. subst news.
  unfold A.ResultKeys in RK. subst ks. apply in_flat_map in Ik as [e [Ie Ik]].
  destruct (A.entry_key (A.c_sub C) e) as [k'|] eqn:Ek; [|contradiction].
  destruct Ik as [->|[]]. destruct (entry_provision_of (A.c_sub C) (fun a => a) e k Ek) as [p Ep].
  exists p. apply in_flat_map. exists e. split; [apply in_rev in Ie; exact Ie|]. rewrite Ep. left. reflexivity.
Qed.

End Static.

Module ChainNew (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module CC := PublicChainCore.ChainCore Prims I0.
Module LR := CC.LR.
Module CA := CC.CA.
Module S := CC.S.

Section New.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Let P := program_of Prog argv read.

Notation ClosureOk := (S.ClosureOk W Prog argv read state_ok).

(* PREMISE (T15/T16, static). At an application of F (AppPremises) whose
   closure is a copy chain down to a root h0, each key k0 of h0's formal
   result (read at h0's context with fresh output roles gen), substituted
   as the application's result selection does (each role gen t to t's
   destination, every other symbol, an input, by θ), is the caller's
   run-time key of a key of F's result as the caller reads it (C'). *)
Definition ChainKeysOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  (forall t, A.canon C (A.canon C t) = A.canon C t) ->
  forall gfx sms h0 gf0 sms0 Ch0 nsh0 outs' gen Cr ks',
  CA.CopyChain W u gfx sms h0 gf0 sms0 ->
  A.FunCtx W h0 Ch0 nsh0 ->
  (forall t, In t outs' <-> A.FunOutputs W h0 t) ->
  NoDup (map gen outs') -> (forall t, In t outs' -> A.Fresh W (gen t)) ->
  A.ProvideResult W h0 Ch0 (fun t => TAbs (gen t)) Cr ks' ->
  forall k0, In k0 ks' -> exists k1, In k1 ks /\
    A.rkey_of C' k1 = app_subst_item (result_substitution th (role_outputs (map (fun t => (t, gen t)) outs') (map (fun t => (t, gensof t)) outs)))
      (A.rkey_of Cr k0).

(* PREMISE (T15/T16): ChainNewOk's second clause, the new provisions
   realized after the application. *)
Definition ChainRealizeOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st code sms dc gfx h0 gf0 gg dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0
    d1 d2 d3 g3 st3 H3 j',
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure code sms dc) ->
  ClosureOk rr H u gfx (FunctorClosure code sms dc) ->
  S.FixedMeans W u d fx gfx ->
  CA.CopyChain W u gfx sms h0 gf0 [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] ->
  ClosureOk rr H h0 gf0 (FunctorClosure code [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] dc) ->
  S.LinkData W Prog h0 gg code dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 ->
  select_provisions d sm = Some d1 ->
  adapt_provisions d1 sms = Some d2 ->
  S.I.eval_bindings j' P (application_environment P d code th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> S.Pre W Prog argv read state_ok rr Fb Cb d3 [] H3 g3 st3 ->
  forall dr, select_provisions d3 (result_smap (result_substitution th (role_outputs (map (fun t => (t, gen t)) outs') (map (fun t => (t, gensof t)) outs))) (map ITy outs' ++ map (A.rkey_of Cr) ks')%list (S.link_sources Cb outs' gen Cr ks')) = Some dr ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list ->
  CC.EnvOkOn W Prog argv read state_ok (fun k => In k (map fst news)) rr C' (append_environment (LR.result_of d3 outs gensof dr) d) H3.

(* Every key a selection fills is a first component of its map. *)
Lemma select_keys : forall d sm dc k pv, select_provisions d sm = Some dc -> lookup_provision k dc = Some pv ->
  exists s, In (k, s) sm.
Proof.
  intros d sm dc k pv Sel L. rewrite (select_lookup d sm dc k Sel) in L.
  destruct (sm_find k sm) as [s|] eqn:F; [|discriminate]. exists s. exact (CC.PA.sm_find_in k sm s F).
Qed.

Theorem chain_new_ok : ChainKeysOk -> ChainRealizeOk -> CC.ChainNewOk W Prog argv read state_ok.
Proof.
  intros Keys Real F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms dc gfx h0 gf0 gg dg n ps a r bodies
    Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm Chain Ok0 LD Sel Sel2 Run Ext Pre3
    dr Sdr news En.
  split; [|exact (Real F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms dc gfx h0 gf0 gg dg n ps a r bodies
    Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm Chain Ok0 LD Sel Sel2 Run Ext Pre3
    dr Sdr news En)].
  intros k pv L.
  pose proof Pre0 as [_ [_ [_ [_ [[_ [_ [Idem _]]] _]]]]].
  pose proof LD as (FC0 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OutsR & _ & NdG & FrG & PR' & _).
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & PR).
  destruct (select_keys _ _ _ _ _ Sdr L) as [s Iq].
  destruct (S.result_smap_in _ _ _ _ _ Iq) as [i [Ii [Nt [-> _]]]].
  apply in_app_or in Ii as [Ii|Ii].
  - apply in_map_iff in Ii as [t [<- _]]. discriminate.
  - apply in_map_iff in Ii as [k0 [<- Ik0]].
    destruct (Keys F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Idem gfx sms h0 gf0 _ Ch0 nsh0 outs' gen Cr ks'
      Chain FC0 OutsR NdG FrG PR' k0 Ik0) as [k1 [Ik1 E]].
    destruct (provide_result_key_news W _ _ _ _ _ k1 PR Ik1 news En) as [p1 Ip1].
    exists k1, p1. split; [exact Ip1|exact E].
Qed.

End New.
End ChainNew.
