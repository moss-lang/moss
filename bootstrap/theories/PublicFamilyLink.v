(* CC.FamilyLinkNewOk, proved (linknewpf): a family F linked to a body G,
   applied at a family application, installs under each caller key a
   closure callable at the caller's signature.
   - Positions: the caller's key at position i reads F's result at θ
     (PublicFamilyAgree.family_items_pos); the body's source at i is the
     body's provision at F's key, read at A.link_site (ResultRealizes).
   - The body's provision is callable at F's signature read at the body's
     final meanings (PublicChainCore.body_provides).
   - A family body writes no type meaning (PublicLinkWalk), so those
     meanings are the application environment's: a linked input means the
     caller's θ reading (cap3b's LinkBody input condition, θ's merges).
   - F's signature mentions only inputs G has (PublicLinkScope: what the
     body adds mentions only G's scope), so the two readings agree with
     the caller's signature (SigAgree). *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicTypeFacts PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply
  PublicLinkResults PublicChainCore PublicLinkWalk PublicLinkScope.
From NM Require PublicChainFwd PublicFormation PublicCallerKeys PublicFamilyAgree PublicFamilyStep PublicFamilyCoherent PublicFamilySig PublicScoping.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Module FamilyLink (Prims : PRIMITIVES) (I0 : INTERP Prims).
(* The instance PublicEntry uses (CF, and through it CN and CC), so the
   obligations match. *)
Module CF := PublicChainFwd.ChainFwd Prims I0.
Module CC := CF.CN.CC.
Module S := CC.S.

Section Link.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Let P := program_of Prog argv read.
Hypothesis World : exists b, A.WorldOf b W.
Hypothesis merge_formed : PublicFormation.MergeFormed W.
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.

Lemma subst_nil_item : forall k, app_subst_item [] k = k.
Proof.
  intros [t|v|f|r m a|u]; try reflexivity. cbn. rewrite PublicTypeRead.app_subst_nil. f_equal.
  unfold map_snd. induction a as [|[x t] a IH]; [reflexivity|]. cbn. rewrite PublicTypeRead.app_subst_nil, IH. reflexivity.
Qed.

Lemma link_body_parts : forall u gg h, A.LinkBody W Prog u gg h ->
  exists (Cu : A.cx) (nsu : list item) (Cg : A.cx) (nsg : list item) (binds0 : list binding) (Dr srcs : list item), A.FunCtx W u Cu nsu /\
    (forall x tx, A.nlook x (A.c_ty Cg) = Some tx -> A.nlook x (A.c_ty Cu) <> None ->
       (forall y, In y (A.gens tx) -> A.nlook y (A.c_ty Cu) <> None) /\ A.canon Cu (TAbs x) = A.canon Cu tx) /\
    (A.Family W u -> A.FamilyBindings W binds0) /\
    A.funbody_of Prog h = Some (FunctorCode (map fst (A.c_ty Cg))
      (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))) (A.c_ty Cg) Dr binds0 srcs).
Proof.
  intros u gg h LB. destruct LB as [u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0
    Dg Ast FCu FCg B3 LPeq LRd Mod Ret This Loop Num Pins Items MadeU Outs OutM NdG FrG PR RR BO Fam FrH FB].
  exists Cu, nsu, Cg, nsg, binds0, (map ITy outs ++ map (A.rkey_of Cr) ks)%list,
    (map (fun k => A.rkey_of (A.link_site Cg Cb outs) (A.at_site (A.link_site Cg Cb outs) (A.output_renaming outs gen k))) ks). split; [exact FCu|split; [exact LRd|split; [exact Fam|exact FB]]].
Qed.

Lemma functx_table_in : forall u C ns, A.FunCtx W u C ns -> forall x v, In (x, v) (A.c_ty C) ->
  v = app_subst (A.c_sub C) (TAbs x).
Proof.
  intros u C ns (d & ps & a & r & b & es & tm & tm' & σ & implicit & D & FP & IE & Imp & -> & MGM & -> & _) x v I.
  destruct (proj2 (PublicCallRealize.entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT _].
  cbn in I |- *. apply in_map_iff in I as [[y v0] [E I]]. cbn in E. injection E as -> <-.
  apply in_app_or in I as [I|I].
  - apply in_map_iff in I as [t [E _]]. injection E as -> <-. reflexivity.
  - unfold PublicCallRealize.IdForm in IdT. rewrite Forall_forall in IdT. pose proof (IdT _ I) as E. cbn in E. rewrite E. reflexivity.
Qed.

Lemma forall2_cons_inv : forall X Y (R : X -> Y -> Prop) x y l1 l2, Forall2 R (x :: l1) (y :: l2) -> R x y /\ Forall2 R l1 l2.
Proof. intros X Y R x y l1 l2 F. inversion F; subst. split; assumption. Qed.

Lemma sig_subst_gens : forall s g y, In y (A.sig_gens (A.sig_subst s g)) ->
  exists t, In t (A.sg_ret g :: A.sg_params g) /\ In y (A.gens (app_subst s t)).
Proof.
  intros s [ps rt] y I. unfold A.sig_gens in I. cbn in I. apply in_app_or in I as [I|I]; [exists rt; split; [left; reflexivity|exact I]|].
  apply in_flat_map in I as [t' [It I]]. apply in_map_iff in It as [t [<- It]]. exists t. split; [right; exact It|exact I].
Qed.

Theorem family_link_new_ok : CC.FamilyLinkNewOk W Prog argv read state_ok.
Proof.
  intros C0 C u matching th sm Cr ks Es Ep El Ts FA rr F d types H g st fx h dc gfx gg dg n ps a r bodies Cu ns Cg nsg Fg wrap
    Cb Fb made outs' gen Cr' ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Fx Look Ok Fm LD Sel Sel2 Run Ext Pre3 dr Sdr r0 m0 g0 Ik0 Mg0.
  pose proof FA as (fx' & Cu1 & ns1 & Fx' & Fam & Poly & FC & Th & InF & FAg & Sat & PR & Nd).
  pose proof LD as (FCu & FCg & LPeq & LRd & Dg & Ast & Mod & Ret & This & Loop & Num & Items & OutsR & OutM & NdG & FrG & PR' & RR & BO & FB & _).
  destruct (S.functx_unique W _ _ _ _ _ FC FCu) as [-> ->].
  destruct (PublicFormation.family_context_shape matching C) as [Ety [EsubF [EprF EloF]]].
  set (Cf := A.family_context C matching) in *.
  (* 1: a family has no outputs *)
  assert (NoOut : forall t, ~ A.FunOutputs W u t) by (intros t Ft; exact (PublicFormation.family_no_outputs W u t (proj1 Fam) Ft)).
  destruct outs' as [|t0 outs0]; [|exfalso; exact (NoOut t0 (proj1 (OutsR t0) (or_introl eq_refl)))].
  (* 2: the full LinkBody, from the closure *)
  destruct (S.closure_cases W Prog argv read state_ok _ _ _ _ _ Ok) as [CL|[CCp|CP]].
  2: { destruct CCp as (h' & Cu0 & nsu & Ch & nsh & code & sms & dc' & rms & rm & gfh & E & _).
       injection E as _ _ _ E. destruct rms; discriminate. }
  2: { destruct CP as (code & E & _). discriminate. }
  destruct CL as (gg2 & h2 & Cu2 & nsu2 & Cg2 & nsg2 & dc2 & E & LB & _).
  injection E as E1h _ E3h. subst h2 dc2.
  destruct (link_body_parts _ _ _ LB) as (Cu3 & nsu3 & Cg3 & nsg3 & binds3 & Dr3 & srcs3 & FCu3 & LRd3 & FamB & FB3).
  destruct (S.functx_unique W _ _ _ _ _ FCu FCu3) as [<- <-].
  rewrite FB in FB3. injection FB3 as E1 E2 ETy E4 EBinds E6.
  (* cap3b's input condition, at G's table *)
  assert (LRd' : forall x tx, A.nlook x (A.c_ty Cg) = Some tx -> A.nlook x (A.c_ty Cu) <> None ->
     (forall y, In y (A.gens tx) -> A.nlook y (A.c_ty Cu) <> None) /\ A.canon Cu (TAbs x) = A.canon Cu tx).
  { intros x tx L1 L2. rewrite ETy in L1. exact (LRd3 x tx L1 L2). }
  pose proof (FamB Fam) as FBs. rewrite <- EBinds in FBs.
  (* 3: the body writes no type meaning *)
  destruct (family_binds_no_dests W Prog _ _ _ _ _ _ _ binds0 Items BO FBs) as [Made [Dests _]].
  pose proof (CC.bindings_writes _ _ _ _ _ _ _ _ _ Run) as [[w [Ew Iw]] _].
  unfold CC.binding_dests in Iw. change (flat_map _ binds0) with (binding_dests binds0) in Iw. rewrite Dests in Iw.
  destruct w as [|q w]; [|exfalso; exact (Iw (fst q) (or_introl eq_refl))]. cbn [app] in Ew.
  assert (Code : view_functor (program_of Prog argv read) h = A.funbody_of Prog h) by reflexivity.
  set (lnk := filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))).
  set (base := fun source => match (if existsb (Nat.eqb source) lnk then lookup_subst source th else None) with
    | Some meaning => ground d meaning
    | None => match output_meaning (append_environment d2 dc) source with Some meaning => meaning | None => TAbs source end
    end).
  set (B := map (fun source => (source, base source)) (map fst (A.c_ty Cg))).
  assert (Mde : type_meanings d3 = map (fun source => (source, match lookup_subst source (A.c_ty Cg) with
    | Some t => app_subst B t | None => base source end)) (map fst (A.c_ty Cg))).
  { rewrite Ew. unfold application_environment. rewrite Code, FB. reflexivity. }
  assert (ThL : forall z, In z (map fst (A.c_ty Cu)) ->
    lookup_subst z th = Some (A.canon Cf (app_subst (A.c_ty Cf) (TAbs z)))).
  { intros z Iz. rewrite Th. exact (S.lookup_on_keys (fun y => A.canon Cf (app_subst (A.c_ty Cf) (TAbs y))) _ z Iz). }
  assert (BaseL : forall z, In z (map fst (A.c_ty Cg)) -> In z (map fst (A.c_ty Cu)) ->
    lookup_subst z B = Some (ground d (app_subst th (TAbs z)))).
  { intros z Ig Iu. unfold B. rewrite (S.lookup_map_nat base _ z Ig). unfold base.
    assert (Ex : existsb (Nat.eqb z) lnk = true).
    { unfold lnk. apply existsb_exists. exists z. split; [apply filter_In; split; [exact Ig|]|apply Nat.eqb_refl].
      apply existsb_exists. exists z. split; [exact Iu|apply Nat.eqb_refl]. }
    rewrite Ex, (ThL z Iu). cbn [app_subst]. rewrite (ThL z Iu). reflexivity. }
  destruct (PublicCallRealize.functx_scoped W gg Cg nsg FCg) as [TblG [ProvG [KMg [_ [_ [_ SubG]]]]]].
  destruct (PublicCallRealize.functx_scoped W u Cu ns FCu) as [TblU [ProvU [KMu _]]].
  assert (GgDom : forall x tx, A.nlook x (A.c_ty Cg) = Some tx -> incl (A.gens tx) (map fst (A.c_ty Cg))).
  { intros x tx L z Iz. rewrite (TblG x tx L) in Iz. cbn [app_subst] in Iz.
    destruct (lookup_subst x (A.c_sub Cg)) as [v|] eqn:Lx.
    - exact (proj2 (SubG x v (lookup_subst_in_pair _ _ _ Lx)) z Iz).
    - destruct Iz as [<-|[]]. apply PublicFamilyAgree.nlook_dom. rewrite L. discriminate. }
  assert (Parts : th = map (fun q => (fst q, A.canon Cf (app_subst (A.c_ty Cf) (TAbs (fst q))))) (A.c_ty Cu) /\
    (forall t, In t (map fst (A.c_ty Cu)) -> A.nlook t (A.c_ty Cf) <> None) /\
    (forall g t, In (g, t) (A.c_sub Cu) -> (forall x, In x (g :: A.gens t) -> A.nlook x (A.c_ty Cf) <> None) ->
       A.canon Cf (app_subst (A.c_ty Cf) (TAbs g)) = A.canon Cf (app_subst (A.c_ty Cf) t)) /\
    (forall d ps a r b, A.w_decl W u = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None) /\
    A.ProvideResult W u Cf (fun t => TAbs t) Cr ks).
  { destruct Sat as [_ [_ [_ [_ Merge]]]]. split; [exact Th|]. split; [exact InF|]. split; [exact Merge|]. split; [|exact PR].
    intros d' ps' a' r' b' D FP. destruct Fam as [Shape _]. destruct Shape as (d'' & n'' & ps'' & a'' & r'' & D' & Ast'' & _).
    rewrite D in D'. injection D' as <-. rewrite Ast'' in FP. cbn in FP. injection FP as _ _ _ <-. reflexivity. }
  pose proof (PublicFamilyAgree.th_sigma W C u th Cf (fun t => TAbs t) Cr ks Cu ns EsubF Parts FCu) as ThSig.
  assert (MeanX : forall x, In x (map fst (A.c_ty Cg)) -> In x (map fst (A.c_ty Cu)) ->
    lookup_subst x (type_meanings d3) = Some (ground d (app_subst th (TAbs x)))).
  { intros x Ig Iu. rewrite Mde, (S.lookup_map_nat _ _ x Ig).
    destruct (A.nlook x (A.c_ty Cg)) as [tx|] eqn:Lx; [|exfalso; exact (proj2 (PublicFamilyAgree.nlook_dom _ x _) Ig Lx)].
    rewrite lookup_subst_nlook', Lx. f_equal.
    destruct (LRd' x tx Lx (proj2 (PublicFamilyAgree.nlook_dom _ x _) Iu)) as [InU Can].
    assert (EB : app_subst B tx = ground d (app_subst th tx)).
    { unfold ground. rewrite <- (PublicTypeRead.app_subst_nil (app_subst B tx)). apply subst2_agree.
      intros z Iz. rewrite PublicTypeRead.app_subst_nil. cbn [app_subst].
      rewrite (BaseL z (GgDom x tx Lx z Iz) (proj1 (PublicFamilyAgree.nlook_dom _ z _) (InU z Iz))). reflexivity. }
    rewrite EB. f_equal. rewrite <- (ThSig (TAbs x)), <- (ThSig tx). unfold A.canon in Can. rewrite Can. reflexivity. }
  assert (RunFact : forall t, incl (A.gens t) (map fst (A.c_ty Cg)) -> incl (A.gens t) (map fst (A.c_ty Cu)) ->
    ground d3 t = ground d (app_subst th t)).
  { intros t Ig Iu. unfold ground. rewrite <- (PublicTypeRead.app_subst_nil (app_subst (type_meanings d3) t)).
    apply subst2_agree. intros z Iz. rewrite PublicTypeRead.app_subst_nil. change (app_subst (type_meanings d3) (TAbs z)) with (match lookup_subst z (type_meanings d3) with Some t => t | None => TAbs z end).
    rewrite (MeanX z (Ig z Iz) (Iu z Iz)). reflexivity. }
  (* 4: positions *)
  assert (PRf : A.ProvideResult W u Cu (fun t => TAbs t) Cr' ks').
  { destruct PR' as (dd & pss & aa & rr0 & bb & outsI & es & tm' & D & FP & OutsI & OIE & RK & ECr).
    destruct outsI as [|t1 o1]; [|exfalso; exact (NoOut t1 (proj1 (OutsI t1) (or_introl eq_refl)))].
    exists dd, pss, aa, rr0, bb, [], es, tm'. exact (conj D (conj FP (conj OutsI (conj OIE (conj RK ECr))))). }
  destruct (PublicChainCore.provide_result_news W _ _ _ _ _ PRf) as [newsr [Enr KMnr]].
  assert (KMr : PublicCallRealize.KeysMatch (A.c_provisions Cr')) by (rewrite Enr; exact (PublicChainCore.keys_match_app _ _ KMnr KMu)).
  destruct World as [bw Wo].
  assert (KeysM : forall key, In key ks' -> exists r m a, key = IMth r m a /\ A.kind_sym W m = Some A.DKMeth).
  { destruct (PublicFamilyStep.family_news W C u matching th sm Cr ks FA) as (news & En & _ & _ & NewM & KeysN).
    destruct (PublicCallerKeys.provide_result_kinds W u Cf _ Cr ks Cu _ Cr' ks' PR PRf) as (es1 & es2 & E1' & E2' & Fk).
    pose proof (PublicCallerKeys.same_kind_keys (A.c_sub Cf) (A.c_sub Cu) _ _ Fk) as FK. rewrite <- E1', <- E2' in FK.
    intros key Ik.
    destruct (S.Forall2_In_r _ _ _ _ _ _ FK Ik) as [k1 [Ik1 Rk]].
    destruct (proj1 (KeysN k1) Ik1) as [p1 Ip1]. destruct (NewM k1 p1 Ip1) as (r1 & m1 & g1 & a1 & -> & _).
    destruct key as [?|?|?|rk mk ak|?]; cbn in Rk; try contradiction.
    pose proof (PublicCallerKeys.result_key_id W u Cu _ Cr' ks' _ PRf Ik) as Pid. cbn in Pid. injection Pid as Ea. subst ak.
    destruct (RR (IMth rk mk []) Ik) as [req [act [Lr _]]]. cbn [A.provision_id] in Lr.
    pose proof (KMr _ _ (S.ilook_in _ _ _ Lr)) as Kd. destruct req as [| |gq aq|]; cbn in Kd; try contradiction.
    destruct (PublicFamilyCoherent.family_member bw W Wo u Cu _ Cr' ks' rk mk gq aq PRf Ik Lr) as (_ & _ & _ & _ & _ & _ & _ & Km).
    exists rk, mk, []. split; [reflexivity|exact Km]. }
  assert (Idem : forall t, A.canon C (A.canon C t) = A.canon C t).
  { pose proof Pre0 as [_ [_ [_ [_ [[_ [_ [Id0 _]]] _]]]]]. intros t. unfold A.canon. rewrite Es. exact (Id0 t). }
  assert (KeysT : forall q pa x, A.resolve_sym W q pa = Some x -> A.nlook x (A.c_ty Cf) <> None -> A.nlook x (A.c_ty Cu) = None ->
    A.kind_sym W x = Some A.DKType) by (intros q pa x _ Lx Nu; exact (PublicFormation.family_keys_typed W C u matching th sm Cr ks Ts FA Cu ns FC x Lx Nu)).
  pose proof (PublicFamilyAgree.family_items_pos W C u matching th sm Cr ks Cu ns Cr' ks' FA FCu NoOut PRf KeysM Idem KeysT) as Pos.
  pose proof (PublicFamilyAgree.family_items_agree W C u matching th sm Cr ks Cu ns Cr' ks' FA FCu NoOut PRf KeysM Idem KeysT) as [NewSig _].
  set (Ls := A.link_site Cg Cb []) in *.
  set (srcs := S.link_sources Ls [] gen ks') in *.
  set (rs := map (A.rkey_of Cr) ks) in *.
  assert (LenRs : List.length (chain_sources [] srcs) = List.length rs)
    by (unfold rs, srcs, S.link_sources; cbn [chain_sources fold_left]; rewrite !length_map; exact (eq_sym (Forall2_length Pos))).
  destruct (In_nth_error _ _ Ik0) as [i0 E0].
  assert (R0 : nth_error rs i0 = Some (A.rkey Cr r0 m0)) by (unfold rs; rewrite nth_error_map, E0; reflexivity).
  destruct (PublicCallerKeys.selection_at rs [] srcs i0 _ R0 LenRs) as [s Fs].
  destruct (PublicCallerKeys.selection_last rs [] srcs _ s Fs LenRs) as [i [Ri [Si _]]].
  cbn [chain_sources fold_left] in Si. unfold srcs, S.link_sources in Si. rewrite nth_error_map in Si.
  destruct (nth_error ks' i) as [key1|] eqn:Ek1; [|discriminate]. injection Si as Es'.
  unfold rs in Ri. rewrite nth_error_map in Ri.
  destruct (nth_error ks i) as [ki|] eqn:Ki; [|discriminate]. injection Ri as Eki.
  pose proof (PublicCallerKeys.forall2_nth _ _ _ _ _ _ _ _ Pos Ki Ek1) as Kq. cbv beta in Kq. rewrite Eki in Kq.
  pose proof (nth_error_In _ _ Ek1) as Ik1.
  destruct (KeysM key1 Ik1) as (rf & m1 & a1 & -> & _).
  assert (Em : m1 = m0).
  { cbn [A.rkey_of A.rkey app_subst_item] in Kq. unfold A.rkey in Kq. injection Kq as _ Em _. exact Em. }
  subst m1.
  pose proof (PublicCallerKeys.result_key_id W u Cu _ Cr' ks' _ PRf Ik1) as Pid. cbn in Pid. injection Pid as Ea. subst a1.
  (* 5: the body's provision at key1 *)
  pose proof Pre3 as [_ [_ [_ [_ [Real3 [Fmd3 _]]]]]].
  destruct (CC.body_provides W Prog argv read state_ok rr Ls d3 H3 Cr' ks' [] Real3 RR _ Ik1) as [p [pv [Lp [Lpv BR]]]].
  cbn [A.provision_id] in Lp.
  pose proof (KMr _ _ (S.ilook_in _ _ _ Lp)) as Kd. destruct p as [| |gf af|]; cbn in Kd; try contradiction.
  assert (Mgf : A.mlook rf m0 (A.c_mths Cr') = Some gf) by exact (PublicCallRealize.ilook_mths _ rf m0 gf af KMr Lp).
  pose proof (proj2 (NewSig r0 m0 g0 (conj Ik0 Mg0)) (IMth rf m0 []) rf [] gf Ik1 eq_refl Mgf Kq) as Agr.
  destruct BR as (rb & mb & ib & Eat & Call).
  exists pv. split.
  { rewrite (select_lookup _ _ _ _ Sdr), Fs, <- Es'. exact Lpv. }
  (* 6: F's signature and receiver mention only G's inputs *)
  pose proof (PublicFormation.units_bare_of W bw Wo) as UB.
  pose proof (PublicFormation.functx_cx_formed W merge_formed mentions_typed gg Cg nsg FCg) as FormG.
  assert (FrG' : PublicFormation.FrameFormed W Fg).
  { split; [rewrite Ret|rewrite This]; exact (PublicFormation.optok_none W). }
  destruct (items_scope W Prog UB (PublicFamilySig.family_sig_ok_readoff W) _ _ _ _ _ _ _ Items FormG FrG') as [_ [BackB [PvB SubB]]].
  rewrite Made in BackB.
  set (DG := map fst (A.c_ty Cg)) in *.
  assert (SubDom : forall y v, lookup_subst y (A.c_sub Cg) = Some v -> In y DG /\ incl (A.gens v) DG)
    by (intros y v L; exact (SubG y v (lookup_subst_in_pair _ _ _ L))).
  assert (SubNone : forall y, ~ In y DG -> lookup_subst y (A.c_sub Cb) = None).
  { intros y Ny. rewrite SubB. destruct (lookup_subst y (A.c_sub Cg)) as [v|] eqn:L; [exfalso; exact (Ny (proj1 (SubDom y v L)))|reflexivity]. }
  assert (SigmaGens : forall t y, In y (A.gens (app_subst (A.c_sub Cg) t)) -> In y (A.gens t) \/ In y DG).
  { intros t y Iy. destruct (gens_subst t _ y Iy) as [z [Iz [[_ ->]|[v [L Iv]]]]]; [left; exact Iz|right; exact (proj2 (SubDom z v L) y Iv)]. }
  assert (ScopeG : forall y, In y (PublicFormation.scope_gens Cg) -> In y DG).
  { intros y Iy. unfold PublicFormation.scope_gens in Iy. apply in_app_or in Iy as [Iy|Iy].
    - apply in_flat_map in Iy as [[g' v] [Iq Iy]]. cbn [fst snd] in Iy. destruct Iy as [E|Iy].
      + subst y. exact (proj1 (SubG g' v Iq)).
      + exact (proj2 (SubG g' v Iq) y Iy).
    - apply in_flat_map in Iy as [[x v] [Iq Iy]]. cbn [fst snd] in Iy. rewrite (functx_table_in _ _ _ FCg x v Iq) in Iy.
      assert (Ix : In x DG) by (change x with (fst (x, v)); apply in_map; exact Iq).
      assert (B1 : forall y, In y (A.gens (app_subst (A.c_sub Cg) (TAbs x))) -> In y DG).
      { intros y' Iy'. destruct (SigmaGens _ _ Iy') as [[<-|[]]|I']; [exact Ix|exact I']. }
      apply in_app_or in Iy as [Iy|Iy]; [exact (B1 y Iy)|].
      unfold A.canon in Iy. destruct (SigmaGens _ _ Iy) as [I'|I']; [exact (B1 y I')|exact I']. }
  assert (InCb : forall k p, In (k, p) (A.c_provisions Cb) -> forall y, ~ In y DG ->
    ~ In y (PublicCallRealize.provision_sig_gens p) /\ ~ In y (PublicCallRealize.key_gens k)).
  { intros k p I y Ny. destruct (PvB k p I) as [Ig|[Sp Sk]].
    - destruct (ProvG k p Ig) as [S1 [S2 _]]. split; intros Iy; [exact (Ny (S1 y Iy))|exact (Ny (S2 y Iy))].
    - split; intros Iy.
      + apply Ny, ScopeG.
        assert (Ip : In y (A.provision_gens p)).
        { unfold PublicCallRealize.provision_sig_gens in Iy. unfold A.provision_gens.
          destruct (A.provision_signature p); [apply in_or_app; left; exact Iy|contradiction]. }
        destruct (BackB y (Sp y Ip)) as [I'|[]]. exact I'.
      + apply Ny, ScopeG. destruct (BackB y (Sk y Iy)) as [I'|[]]. exact I'. }
  destruct (RR (IMth rf m0 []) Ik1) as [req [act [Lr [La Ref]]]].
  cbn [A.provision_id] in Lr. rewrite Lp in Lr. injection Lr as <-. cbn [map] in La, Ref.
  pose proof (S.ilook_in _ _ _ La) as Iact.
  change (In (A.provision_id (A.at_site Ls (app_subst_item [] (IMth rf m0 []))), act) (A.c_provisions Cb)) in Iact.
  rewrite Eat in Iact. cbn [A.provision_id] in Iact.
  cbn [app_subst_item A.at_site] in Eat. injection Eat as Erb Emb _. subst mb.
  rewrite PublicTypeRead.app_subst_nil, PublicScoping.map_pair_id in Erb.
  change (A.canon Cb (app_subst (A.c_ty Cg) rf) = rb) in Erb.
  assert (SigNil : forall g, A.sig_subst [] g = g).
  { intros [ps0 rt0]. unfold A.sig_subst. cbn. rewrite PublicTypeRead.app_subst_nil, (map_ext _ _ PublicTypeRead.app_subst_nil), map_id. reflexivity. }
  destruct act as [| |ga aa|]; cbn in Ref; destruct Ref as [Kind [SigE _]]; try contradiction.
  injection SigE as SigP SigR. rewrite (map_ext _ _ PublicTypeRead.app_subst_nil), map_id in SigP.
  rewrite PublicTypeRead.app_subst_nil in SigR.
  assert (SigE : A.sig_subst (A.c_sub Cb) gf = A.sig_subst (A.c_sub Cb) ga).
  { destruct gf as [psf rtf], ga as [psa rta]. unfold A.sig_subst. cbn in SigP, SigR |- *. rewrite SigP, SigR. reflexivity. }
  assert (R2sig : forall t y, In t (A.sg_ret gf :: A.sg_params gf) -> In y (A.gens t) -> In y DG).
  { intros t y It Iy. destruct (in_dec Nat.eq_dec y DG) as [I'|Ny]; [exact I'|exfalso].
    assert (I1 : In y (A.sig_gens (A.sig_subst (A.c_sub Cb) gf))).
    { pose proof (PublicFormation.gens_keep t (A.c_sub Cb) y Iy (SubNone y Ny)) as Ik. unfold A.sig_gens, A.sig_subst. cbn.
      destruct It as [<-|It]; [apply in_or_app; left; exact Ik|apply in_or_app; right].
      apply in_flat_map. exists (app_subst (A.c_sub Cb) t). split; [apply in_map; exact It|exact Ik]. }
    rewrite SigE in I1. destruct (sig_subst_gens _ _ _ I1) as [t' [It' Iy']]. rewrite SubB in Iy'.
    destruct (SigmaGens _ _ Iy') as [Iy''|D']; [|exact (Ny D')].
    apply (proj1 (InCb _ _ Iact y Ny)). unfold PublicCallRealize.provision_sig_gens. cbn [A.provision_signature].
    unfold A.sig_gens. apply in_flat_map. exists t'. split; [exact It'|exact Iy'']. }
  assert (R2rec : forall y, In y (A.gens rf) -> In y DG).
  { intros y Iy. destruct (in_dec Nat.eq_dec y DG) as [I'|Ny]; [exact I'|exfalso].
    assert (Lc : lookup_subst y (A.c_ty Cg) = None).
    { rewrite lookup_subst_nlook'. destruct (A.nlook y (A.c_ty Cg)) eqn:L; [|reflexivity].
      exfalso. apply Ny. apply PublicFamilyAgree.nlook_dom. rewrite L. discriminate. }
    pose proof (PublicFormation.gens_keep _ _ y (PublicFormation.gens_keep _ _ y Iy Lc) (SubNone y Ny)) as Irb.
    change (In y (A.gens (A.canon Cb (app_subst (A.c_ty Cg) rf)))) in Irb. rewrite Erb in Irb.
    exact (proj2 (InCb _ _ Iact y Ny) Irb). }
  (* 7: the transfer *)
  pose proof Pre0 as [_ [_ [_ [_ [[Abs0 _] _]]]]].
  pose proof Real3 as [Abs3 _].
  assert (CanF : forall t, A.canon Cf t = A.canon C0 t) by (intros t; unfold A.canon; rewrite EsubF, Es; reflexivity).
  assert (GE : forall tc tf, PublicFamilyAgree.SigAgree Cf Cu th tc tf -> incl (A.gens tf) DG -> ground d3 tf = ground d tc).
  { intros tc tf [Sc Ec] Ig. rewrite (RunFact tf Ig Sc), <- (Abs0 tc), <- CanF, Ec, CanF, (Abs0 (app_subst th tf)). reflexivity. }
  destruct (forall2_cons_inv _ _ _ _ _ _ _ Agr) as [Aret Rest]. destruct (forall2_cons_inv _ _ _ _ _ _ _ Rest) as [Arec Rps].
  assert (Recv : ground d3 rb = ground d r0).
  { destruct Arec as [ScR EcR].
    assert (ThX : app_subst th (app_subst (A.c_ty Cg) rf) = app_subst th rf).
    { rewrite <- (PublicTypeRead.app_subst_nil rf) at 2. apply subst2_agree. intros y Iy. cbn [app_subst].
      pose proof (R2rec y Iy) as Iyg. destruct (A.nlook y (A.c_ty Cg)) as [ty|] eqn:Ly;
        [|exfalso; exact (proj2 (PublicFamilyAgree.nlook_dom _ y _) Iyg Ly)].
      rewrite lookup_subst_nlook', Ly. change (app_subst th ty = app_subst th (TAbs y)).
      destruct (LRd' y ty Ly (proj2 (PublicFamilyAgree.nlook_dom _ y _) (ScR y Iy))) as [_ Can].
      rewrite <- (ThSig (TAbs y)), <- (ThSig ty). unfold A.canon in Can. rewrite Can. reflexivity. }
    assert (XG : forall z, In z (A.gens (app_subst (A.c_ty Cg) rf)) -> In z DG /\ In z (map fst (A.c_ty Cu))).
    { intros z Iz. destruct (gens_subst rf _ z Iz) as [y [Iy [[L _]|[v [L Iv]]]]].
      - exfalso. pose proof (R2rec y Iy) as Iyg. rewrite lookup_subst_nlook' in L. exact (proj2 (PublicFamilyAgree.nlook_dom _ y _) Iyg L).
      - rewrite lookup_subst_nlook' in L. split; [exact (GgDom y v L z Iv)|].
        destruct (LRd' y v L (proj2 (PublicFamilyAgree.nlook_dom _ y _) (ScR y Iy))) as [InU _].
        exact (proj1 (PublicFamilyAgree.nlook_dom _ z _) (InU z Iv)). }
    rewrite <- Erb, Abs3, (RunFact _ (fun z I => proj1 (XG z I)) (fun z I => proj2 (XG z I))), ThX.
    assert (CanF2 : forall t, A.canon (A.family_context C matching) t = A.canon C0 t) by exact CanF.
    rewrite <- (Abs0 r0), <- CanF2, EcR, CanF2, (Abs0 (app_subst th rf)). reflexivity. }
  assert (GEp : forall l1 l2, Forall2 (PublicFamilyAgree.SigAgree Cf Cu th) l1 l2 -> incl l2 (A.sg_params gf) ->
    map (ground d3) l2 = map (ground d) l1).
  { intros l1' l2' F2. induction F2 as [|tc tf l1' l2' A1 _ IH]; intros Inc; [reflexivity|]. cbn [map].
    rewrite (GE tc tf A1 (fun y Iy => R2sig tf y (or_intror (Inc tf (or_introl eq_refl))) Iy)).
    rewrite IH; [reflexivity|]. intros t It. exact (Inc t (or_intror It)). }
  cbn [A.read_rename map] in Call. rewrite PublicTypeRead.app_subst_nil, (map_ext _ _ PublicTypeRead.app_subst_nil), map_id in Call.
  apply (S.callable_transfer W Prog argv read state_ok rr rr H3 d3 d (rb :: A.sg_params gf) (r0 :: A.sg_params g0)
    (A.sg_ret gf) (A.sg_ret g0) pv (le_n _)); [| |exact Call].
  - cbn [map]. rewrite Recv, (GEp _ _ Rps (fun t I => I)). reflexivity.
  - exact (GE _ _ Aret (fun y Iy => R2sig _ y (or_introl eq_refl) Iy)).
Qed.


End Link.
End FamilyLink.
