(* ChainCoreOk (PublicLinkResults), assembled. What a completed linked
   body leaves for the caller of `bind F;` splits into:
   - the result selection from the body's keys succeeds (ResultRealizes
     and the body's realized context);
   - the outputs have meanings in the body's final environment;
   - the caller's context after the application, C', has its provisions
     filed under keys of their own kind, its aliases right, and every
     provision C' keeps from C realized as before (the destinations are
     out of C's scope, and the result's keys are C''s new keys);
   - the new provisions are realized (ChainNewOk, the correspondence
     between the caller's reading of F's result and the body's).
   This file proves ChainCoreOk from ChainNewOk; ChainNewOk is the
   remaining premise. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require AuditFacts.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicTypeFacts PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply
  PublicLinkResults.
From NM Require PublicFormation PublicCallerKeys PublicContextFacts.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- static facts about a result context ---------------------------------- *)

Section Static.
Variable W : A.world.

Lemma entry_provision_kinds : forall s es, PublicCallRealize.KeysMatch (flat_map (A.entry_provision s (fun a => a)) es).
Proof.
  intros s es k p I. apply in_flat_map in I as [e [_ I]].
  destruct e as [t|v t a|f g a|r m g a|u fx|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; injection E as <- <-; exact Logic.I.
Qed.

Lemma keys_match_app : forall l1 l2, PublicCallRealize.KeysMatch l1 -> PublicCallRealize.KeysMatch l2 ->
  PublicCallRealize.KeysMatch (l1 ++ l2).
Proof. intros l1 l2 K1 K2 k p I. apply in_app_or in I as [I|I]; [exact (K1 k p I)|exact (K2 k p I)]. Qed.

(* A result context: C's provisions after new ones, each of its own kind. *)
Lemma provide_result_news : forall u C out C' ks, A.ProvideResult W u C out C' ks ->
  exists news, A.c_provisions C' = (news ++ A.c_provisions C)%list /\ PublicCallRealize.KeysMatch news.
Proof.
  intros u C out C' ks (d & ps & a & r & b & outs & es & tm' & _ & _ & _ & _ & _ & ->).
  exists (flat_map (A.entry_provision (A.c_sub C) (fun a => a)) (rev es)). split; [reflexivity|].
  apply entry_provision_kinds.
Qed.

End Static.

(* ---- lookups in a list with a prefix --------------------------------------- *)

Section Lookups.

Lemma ilook_in_gen : forall X k (l : list (item * X)) p, A.ilook k l = Some p -> In (k, p) l.
Proof.
  intros X k l p. induction l as [|[k' p'] r IH]; intros L; [discriminate|]. cbn in L.
  destruct (item_eqb k k') eqn:E; [apply item_eqb_spec in E; subst k'; injection L as ->; left; reflexivity|].
  right. exact (IH L).
Qed.

Lemma ilook_app_out : forall X k (l1 l2 : list (item * X)), ~ In k (map fst l1) -> A.ilook k (l1 ++ l2) = A.ilook k l2.
Proof.
  intros X k l1 l2 N. induction l1 as [|[k' p] r IH]; [reflexivity|]. cbn.
  destruct (item_eqb k k') eqn:E; [apply item_eqb_spec in E; subst k'; exfalso; apply N; left; reflexivity|].
  apply IH. intros I. apply N. right. exact I.
Qed.

Lemma lookup_entry_app : forall k (l1 l2 : list (item * provision_value)),
  lookup_entry k (l1 ++ l2) = match lookup_entry k l1 with Some v => Some v | None => lookup_entry k l2 end.
Proof.
  intros k l1 l2. induction l1 as [|[k' p] r IH]; [reflexivity|]. cbn. destruct (item_eqb k k'); [reflexivity|exact IH].
Qed.

Lemma vals_of_out : forall l v, PublicCallRealize.KeysMatch l -> ~ In (IVal v) (map fst l) -> A.nlook v (vals_of l) = None.
Proof. intros l v K N. rewrite (vals_by_ilook l v K). destruct (A.ilook (IVal v) l) as [p|] eqn:L; [|reflexivity].
  exfalso. apply N. change (IVal v) with (fst (IVal v, p)). apply in_map. exact (ilook_in_gen _ _ _ _ L). Qed.

End Lookups.

Section Keys.

Lemma rkey_of_kind : forall C k k', A.rkey_of C k = k' ->
  match k' with IMth r m _ => exists i, k = IMth r m i | _ => k = k' end.
Proof. intros C [t|v|f|r m i|u] k' <-; cbn; eauto. Qed.

Lemma rkey_of_provision_id : forall C k, A.rkey_of C (A.provision_id k) = A.rkey_of C k.
Proof. intros C [t|v|f|r m i|u]; reflexivity. Qed.

Lemma entry_key_not_ty : forall s e t, A.entry_key s e <> Some (ITy t).
Proof. intros s [x|v x a|f g a|r m g a|u fx|eql eqr] t; cbn; discriminate. Qed.

Lemma result_keys_not_ty : forall s es ks t, A.ResultKeys s es ks -> ~ In (ITy t) ks.
Proof.
  intros s es ks t -> I. apply in_flat_map in I as [e [_ I]].
  destruct (A.entry_key s e) eqn:E; [|contradiction]. destruct I as [Ei|[]]. subst i. exact (entry_key_not_ty s e t E).
Qed.

Lemma provide_result_keys_not_ty : forall W u C out C' ks t, A.ProvideResult W u C out C' ks -> ~ In (ITy t) ks.
Proof. intros W u C out C' ks t (d & ps & a & r & b & outs & es & tm' & _ & _ & _ & _ & RK & _). exact (result_keys_not_ty _ _ _ t RK). Qed.

Lemma need_shape_id : forall k, (forall t, k <> ITy t) -> NeedShape (A.provision_id k).
Proof. intros [t|v|f|r m i|u] N; cbn; try exact Logic.I; [exact (N t eq_refl)|reflexivity]. Qed.

Lemma at_site_not_ty : forall C k, (forall t, k <> ITy t) -> forall t, A.at_site C k <> ITy t.
Proof. intros C [x|v|f|r m i|u] N t; cbn; try discriminate. intros E. injection E as <-. exact (N x eq_refl). Qed.

Lemma app_subst_item_not_ty : forall s k, (forall t, k <> ITy t) -> forall t, app_subst_item s k <> ITy t.
Proof. intros s [x|v|f|r m i|u] N t; cbn; try discriminate. intros E. injection E as <-. exact (N x eq_refl). Qed.

Lemma item_in_dec : forall (k : item) l, In k l \/ ~ In k l.
Proof.
  intros k l. destruct (existsb (item_eqb k) l) eqn:E.
  - left. apply existsb_exists in E as [k' [I E]]. apply item_eqb_spec in E. subst k'. exact I.
  - right. intros I. assert (T : existsb (item_eqb k) l = true) by (apply existsb_exists; exists k; split; [exact I|apply item_eqb_spec; reflexivity]).
    congruence.
Qed.

Lemma mlook_app_l : forall r m (l1 l2 : list ((ty * nat) * A.fsig)),
  A.mlook r m (l1 ++ l2) = match A.mlook r m l1 with Some g => Some g | None => A.mlook r m l2 end.
Proof.
  intros r m l1 l2. induction l1 as [|[[r' m'] g'] rest IH]; [reflexivity|]. cbn.
  destruct (ty_eqb r r' && Nat.eqb m m'); [reflexivity|exact IH].
Qed.

Lemma nlook_map_snd : forall (f : ty -> ty) (l : list (nat * ty)) x,
  A.nlook x (map (fun q => (fst q, f (snd q))) l) = option_map f (A.nlook x l).
Proof. intros f l x. induction l as [|[y v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb x y); [reflexivity|exact IH]. Qed.

Lemma nlook_map_key : forall (f : nat -> ty) l x v, A.nlook x (map (fun t => (t, f t)) l) = Some v -> v = f x /\ In x l.
Proof.
  intros f l x v. induction l as [|y r IH]; intros L; [discriminate|]. cbn in L.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [injection L as <-; split; [reflexivity|left; reflexivity]|].
  destruct (IH L) as [E I]. split; [exact E|right; exact I].
Qed.

Lemma nlook_map_key_none : forall (f : nat -> ty) l x, A.nlook x (map (fun t => (t, f t)) l) = None -> ~ In x l.
Proof.
  intros f l x. induction l as [|y r IH]; intros L I; [exact I|]. cbn in L.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [discriminate|]. destruct I as [E|I]; [congruence|exact (IH L I)].
Qed.

Lemma lookup_dest : forall (dest : nat -> nat) (f : nat -> ty) outs x, In x outs ->
  (forall a b, In a outs -> In b outs -> dest a = dest b -> a = b) ->
  lookup_subst (dest x) (map (fun t => (dest t, f t)) outs) = Some (f x).
Proof.
  intros dest f outs x. induction outs as [|y r IH]; intros I Inj; [contradiction|]. cbn.
  destruct (Nat.eqb_spec (dest x) (dest y)) as [E|Ne].
  - assert (Ix : In x (y :: r)) by exact I. rewrite (Inj x y Ix (or_introl eq_refl) E). reflexivity.
  - destruct I as [<-|I]; [congruence|]. apply IH; [exact I|]. intros a b Ia Ib. exact (Inj a b (or_intror Ia) (or_intror Ib)).
Qed.

Lemma lookup_not_dest : forall (dest : nat -> nat) (f : nat -> ty) outs x, (forall t, In t outs -> dest t <> x) ->
  lookup_subst x (map (fun t => (dest t, f t)) outs) = None.
Proof.
  intros dest f outs x. induction outs as [|y r IH]; intros N; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec x (dest y)) as [E|Ne]; [exfalso; exact (N y (or_introl eq_refl) (eq_sym E))|].
  apply IH. intros t I. exact (N t (or_intror I)).
Qed.

End Keys.

Module ChainCore (Prims : PRIMITIVES) (I0 : INTERP Prims).
(* The instance PublicEntry uses, so the obligations match. *)
Module LR := PublicLinkResults.LinkResults Prims I0.

Section Core.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Let P := program_of Prog argv read.

Notation CallableOk := (LR.PA.FS.CopyA.S.CallableOk W Prog argv read state_ok).
Notation ClosureOk := (LR.PA.FS.CopyA.S.ClosureOk W Prog argv read state_ok).

(* EnvOk, for the provisions whose keys K holds. *)
Definition EnvOkOnWith (rk : item -> item) (K : item -> Prop) (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  (forall v t, K (IVal v) -> A.nlook v (A.c_vals C) = Some t -> exists w,
     lookup_provision (rk (IVal v)) d = Some (ValueProvision w) /\ value_typed W P H w (ground d t)) /\
  (forall f g, K (IFn f) -> A.nlook f (A.c_fns C) = Some g -> exists pv,
     lookup_provision (rk (IFn f)) d = Some pv /\ CallableOk n H d (A.sg_params g) (A.sg_ret g) pv) /\
  (forall r m g, K (A.mkey r m) -> A.mlook r m (A.c_mths C) = Some g -> exists pv,
     lookup_provision (rk (IMth r m [])) d = Some pv /\ CallableOk n H d (r :: A.sg_params g) (A.sg_ret g) pv) /\
  (forall u fx, K (IFun u) -> A.nlook u (A.c_funs C) = Some fx -> exists pv gfx,
     lookup_provision (rk (IFun u)) d = Some pv /\ ClosureOk n H u gfx pv /\ LR.PA.FS.CopyA.S.FixedMeans W u d fx gfx).

Definition EnvOkOn (K : item -> Prop) (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  EnvOkOnWith (fun k => match k with
    | IMth r m _ => A.rkey C r m
    | _ => k
    end) K n C d H.

Lemma envok_with_of_on : forall rk (K : item -> Prop) n C d H,
  (forall k, K k \/ ~ K k) ->
  EnvOkOnWith rk K n C d H -> EnvOkOnWith rk (fun k => ~ K k) n C d H ->
  LR.PA.FS.CopyA.S.EnvOkWith W Prog argv read state_ok rk n C d H.
Proof.
  intros rk K n C d H Dec [V1 [F1 [M1 U1]]] [V2 [F2 [M2 U2]]].
  split; [|split; [|split]].
  - intros v t L. destruct (Dec (IVal v)); [exact (V1 v t H0 L)|exact (V2 v t H0 L)].
  - intros f g L. destruct (Dec (IFn f)); [exact (F1 f g H0 L)|exact (F2 f g H0 L)].
  - intros r m g L. destruct (Dec (A.mkey r m)); [exact (M1 r m g H0 L)|exact (M2 r m g H0 L)].
  - intros u fx L. destruct (Dec (IFun u)); [exact (U1 u fx H0 L)|exact (U2 u fx H0 L)].
Qed.

Lemma envok_of_on : forall (K : item -> Prop) n C d H, (forall k, K k \/ ~ K k) ->
  EnvOkOn K n C d H -> EnvOkOn (fun k => ~ K k) n C d H -> LR.PA.FS.CopyA.S.EnvOk W Prog argv read state_ok n C d H.
Proof.
  intros K n C d H Dec Yes No.
  exact (envok_with_of_on (fun k => match k with
    | IMth r m _ => A.rkey C r m
    | _ => k
    end) K n C d H Dec Yes No).
Qed.

(* PREMISE (T15/T16; the correspondence). In ChainCoreOk's situation, with
   the result selected from the body (dr) and C''s new provisions (news):
   the new provisions are realized after the application. It is the
   caller's reading of F's result (ProvideResult at θ and the destinations)
   against the root's formal one (Cr, through the copy chain's
   CopyInterpretation and positions), with the body's provisions typed at the
   body's meanings (ResultRealizes, role_reads). That every key dr fills
   is the run-time key of a new provision of C' is true by construction
   (Audit.result_selection files position i under the caller's key for
   it), so it is no longer part of this premise (chain_core_ok). *)
Definition ChainNewOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  LR.PA.FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st code sms dc rms gfx h0 gf0 gg dg n ps a r bodies Ch0 nsh0 Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr ks' binds0
    pins d1 d2 d3 g3 st3 H3 j',
  LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure code sms dc rms) ->
  ClosureOk rr H u gfx (FunctorClosure code sms dc rms) ->
  LR.PA.FS.CopyA.S.FixedMeans W u d fx gfx ->
  LR.PA.FS.CopyA.CopyChain W u gfx sms rms h0 gf0 (Some (A.link_map Ch0 Cg linktarget nsg)) None ->
  ClosureOk rr H h0 gf0 (FunctorClosure code (Some (A.link_map Ch0 Cg linktarget nsg)) dc None) ->
  LR.PA.FS.CopyA.S.LinkData W Prog h0 gg code dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 pins ->
  PublicContextFacts.ClosureBinding W h0 Ch0 nsh0 Cg nsg linktarget dc gf0 ->
  LR.PA.FS.CopyA.S.PinsAgree W h0 Ch0 Cg dc gf0 pins ->
  select_provisions d sm = Some d1 ->
  adapt_provisions d1 sms = Some d2 ->
  LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j' P (application_environment P d code th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok rr Fb Cb d3 [] H3 g3 st3 ->
  forall dr, select_provisions d3 (result_selection (map (A.rkey_of C') ks) rms (LR.PA.FS.CopyA.S.link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks')) = Some dr ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list ->
  EnvOkOn (fun k => In k (map fst news)) rr C' (append_environment (LR.result_of d3 outs gensof dr) d) H3.

(* Every key the selection fills is the caller's run-time key of one of
   its new provisions. *)
Lemma selection_news : forall u C out C' ks rms srcs d3 dr, A.ProvideResult W u C out C' ks ->
  select_provisions d3 (result_selection (map (A.rkey_of C') ks) rms srcs) = Some dr ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list ->
  forall k pv, lookup_provision k dr = Some pv -> exists k1 p1, In (k1, p1) news /\ A.rkey_of C' k1 = k.
Proof.
  intros u C out C' ks rms srcs d3 dr PR Sdr news En k pv L.
  rewrite (select_lookup _ _ _ k Sdr) in L. destruct (sm_find k _) as [s|] eqn:F; [|discriminate].
  destruct (PublicCallerKeys.selection_in _ _ _ _ _ (PublicCallerKeys.sm_find_some_in _ _ _ F)) as [Ik _].
  apply in_map_iff in Ik as [k1 [<- Ik1]].
  destruct (PublicCallerKeys.provide_result_key_news W u C out C' ks k1 PR Ik1 news En) as [p1 Ip1].
  exists k1, p1. split; [exact Ip1|reflexivity].
Qed.

(* ---- the result selection ------------------------------------------------ *)

(* Every position's source is a body key the body's context provides
   (ResultRealizes), so the selection succeeds. *)
Lemma chain_selection : forall n Cb d3 H3 h0 Ch0 out outs' gen Cr ks' rs rms rds,
  LR.PA.FS.CopyA.S.Realizes W Prog argv read state_ok n Cb d3 H3 ->
  A.ProvideResult W h0 Ch0 out Cr ks' ->
  A.ResultRealizes Cb Cr rds ks' ->
  (forall s, In s (closure_sources rms (LR.PA.FS.CopyA.S.link_sources Cb outs' gen ks')) -> In s (LR.PA.FS.CopyA.S.link_sources Cb outs' gen ks')) ->
  exists dr, select_provisions d3 (result_selection rs rms (LR.PA.FS.CopyA.S.link_sources Cb outs' gen ks')) = Some dr.
Proof.
  intros n Cb d3 H3 h0 Ch0 out outs' gen Cr ks' rs rms rds Real PR RR Rg. apply select_ok. intros [a b] Iq.
  destruct (PublicCallerKeys.selection_in _ _ _ _ _ Iq) as [_ Ib]. cbn [snd].
  pose proof (Rg b Ib) as Ib'. unfold LR.PA.FS.CopyA.S.link_sources in Ib'. apply in_map_iff in Ib' as [k' [<- Ik']].
  destruct (RR k' Ik') as [req [act [_ [La _]]]].
  assert (NT : forall t, k' <> ITy t) by (intros t E; subst k'; exact (provide_result_keys_not_ty W _ _ _ _ _ t PR Ik')).
  assert (NT' : forall t, A.at_site Cb k' <> ITy t)
    by (apply at_site_not_ty, NT).
  destruct (LR.PA.FS.CopyA.S.realized_key W Prog argv read state_ok _ _ _ _ _ _ Real (ilook_in_gen _ _ _ _ La) (need_shape_id _ NT')) as [pv L].
  rewrite rkey_of_provision_id in L. intros E. change (lookup_provision (A.rkey_of Cb (A.at_site Cb k')) d3 = None) in E. congruence.
Qed.

(* ---- the caller's aliases after the application ---------------------------- *)

Lemma lookup_subst_app : forall x (l1 l2 : subst),
  lookup_subst x (l1 ++ l2) = match lookup_subst x l1 with Some v => Some v | None => lookup_subst x l2 end.
Proof. intros x l1 l2. rewrite !lookup_subst_nlook', nlook_app'. reflexivity. Qed.

Lemma canon_abs_free : forall C g, ~ In g (A.context_gens C) -> A.canon C (TAbs g) = TAbs g.
Proof.
  intros C g N. unfold A.canon. cbn [app_subst]. destruct (lookup_subst g (A.c_sub C)) as [t|] eqn:E; [|reflexivity].
  exfalso. apply N. exact (proj1 (LR.PA.FS.CopyA.S.sub_context_gens C g t (lookup_subst_in_pair _ _ _ E))).
Qed.

Lemma nlook_ty_canon_gens : forall C x tx, A.nlook x (A.c_ty C) = Some tx -> incl (A.gens (A.canon C tx)) (A.context_gens C).
Proof.
  intros C x tx L y Iy. unfold A.context_gens. apply in_or_app. right. apply in_or_app. left.
  apply in_flat_map. exists (x, tx). split; [exact (nlook_in _ _ _ _ L)|apply in_or_app; right; exact Iy].
Qed.

Lemma nlook_key : forall (l : list (nat * ty)) x v, A.nlook x l = Some v -> In x (map fst l).
Proof. intros l x v L. change x with (fst (x, v)). apply in_map. exact (nlook_in _ _ _ _ L). Qed.

(* After the application the caller's meanings grow by the destinations,
   which nothing C mentions: so its types ground as before. *)
Lemma ground_after : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  LR.PA.FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  PublicFormation.FormedCx W C -> LR.KeptOutputsUnmentioned W ->
  forall d d3 dr t, incl (A.gens t) (LR.PA.FS.CopyA.S.capture_gens C) ->
  ground (append_environment (LR.result_of d3 outs gensof dr) d) t = ground d t.
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku d d3 dr t Inc.
  apply (LR.PA.FS.CopyA.S.ground_grow (map gensof outs) d _ t (LR.result_grows_by_destinations d d3 dr outs gensof)).
  intros g Ig Ig'. apply in_map_iff in Ig as [t0 [<- It]].
  exact (proj1 (proj2 (LR.dest_scope W F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku t0 It)) (Inc _ Ig')).
Qed.

Lemma alias_after : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  LR.PA.FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  PublicFormation.FormedCx W C -> LR.KeptOutputsUnmentioned W ->
  forall d d3 dr, AliasOk C d -> LR.PA.FS.CopyA.S.AbsorbsCanon C d -> (forall t, A.canon C (A.canon C t) = A.canon C t) ->
  AliasOk C' (append_environment (LR.result_of d3 outs gensof dr) d).
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku d d3 dr Al0 Abs Idem.
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & _ & Uniq & _ & _ & OutsI & PR).
  destruct (PublicFormation.provide_result_shape W _ _ _ _ _ PR) as [_ [_ [outs0 [O0 Ety]]]].
  assert (Inj : forall a b, In a outs -> In b outs -> gensof a = gensof b -> a = b)
    by (intros a0 b0 Ia Ib; exact (Uniq a0 b0 (proj1 (OutsI a0) Ia) (proj1 (OutsI b0) Ib))).
  set (d' := append_environment (LR.result_of d3 outs gensof dr) d).
  intros x tx L. rewrite Ety in L. rewrite nlook_map_snd in L. unfold A.output_type_map in L.
  rewrite nlook_map_snd, nlook_app' in L.
  destruct (A.nlook x (map (fun t => (t, TAbs (gensof t))) outs0)) as [v|] eqn:Lo.
  - cbn in L. injection L as <-. destruct (nlook_map_key (fun t => TAbs (gensof t)) _ _ _ Lo) as [-> Ix0].
    assert (Ix : In x outs) by (apply OutsI, O0, Ix0).
    destruct (LR.dest_scope W F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku x Ix) as [_ [Nc _]].
    assert (Nc' : ~ In (gensof x) (A.context_gens C)) by (intros I; exact (Nc (LR.PA.FS.CopyA.S.cg_l C _ I))).
    rewrite (canon_abs_free C _ Nc'), (canon_abs_free C _ Nc').
    unfold output_meaning. unfold d'. cbn [type_aliases type_meanings append_environment LR.result_of].
    rewrite lookup_subst_app, (LR.PA.FS.CopyA.S.lookup_map_nat (LR.read_meaning d3) outs x Ix).
    unfold ground. cbn [type_meanings].
    change (app_subst ?s (TAbs ?g)) with (match lookup_subst g s with Some t => t | None => TAbs g end).
    cbn [type_meanings append_environment LR.result_of]. rewrite lookup_subst_app, (lookup_dest gensof (LR.read_meaning d3) outs x Ix Inj).
    reflexivity.
  - destruct (A.nlook x (A.c_ty C)) as [cx|] eqn:Lc; [|discriminate]. cbn in L. injection L as <-.
    assert (Nx : ~ In x outs) by (intros Ix; exact (nlook_map_key_none (fun t => TAbs (gensof t)) _ _ Lo (proj2 (O0 x) (proj1 (OutsI x) Ix)))).
    assert (Same : output_meaning d' x = output_meaning d x).
    { unfold output_meaning, d'. cbn [type_aliases type_meanings append_environment LR.result_of].
      rewrite lookup_subst_app, (lookup_not_dest (fun t => t) (LR.read_meaning d3) outs x (fun t It E => Nx ltac:(rewrite <- E; exact It))).
      destruct (lookup_subst x (type_aliases d)); [reflexivity|].
      rewrite lookup_subst_app, (lookup_not_dest gensof (LR.read_meaning d3) outs x).
      - reflexivity.
      - intros t It E. destruct (LR.dest_scope W F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku t It) as [Nk _].
        apply Nk. rewrite E. exact (nlook_key _ _ _ Lc). }
    rewrite Same, (Al0 x cx Lc). f_equal. rewrite Idem.
    unfold d'. rewrite (ground_after F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku d d3 dr _
      (fun y Iy => LR.PA.FS.CopyA.S.cg_l C y (nlook_ty_canon_gens C x cx Lc y Iy))).
    symmetry. exact (Abs cx).
Qed.

(* ---- the provisions C' keeps from C ---------------------------------------- *)

Lemma rkey_kept : forall C C' news r m, A.c_sub C' = A.c_sub C ->
  A.c_provisions C' = (news ++ A.c_provisions C)%list -> ~ In (A.mkey r m) (map fst news) ->
  A.rkey C' r m = A.rkey C r m.
Proof.
  intros C C' news r m Es En N. unfold A.rkey, A.theta_of, A.canon. rewrite Es. unfold A.c_apps. rewrite En, map_app.
  rewrite ilook_app_out; [reflexivity|]. rewrite map_map. exact N.
Qed.

(* A provision C' keeps from C (no new provision has its key) is realized
   as before: the result's keys are new keys, and the destinations are out
   of everything C mentions. *)
Lemma envok_kept : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  LR.PA.FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  PublicFormation.FormedCx W C -> LR.KeptOutputsUnmentioned W ->
  forall n d d3 dr H H3 news, LR.PA.FS.CopyA.S.EnvOk W Prog argv read state_ok n C d H -> LR.PA.FS.CopyA.S.KeysMatch C -> heap_extends H H3 ->
  A.c_provisions C' = (news ++ A.c_provisions C)%list -> PublicCallRealize.KeysMatch news ->
  (forall k pv, lookup_provision k dr = Some pv -> exists k1 p1, In (k1, p1) news /\ A.rkey_of C' k1 = k) ->
  EnvOkOn (fun k => ~ In k (map fst news)) n C' (append_environment (LR.result_of d3 outs gensof dr) d) H3.
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku n d d3 dr H H3 news [Vals [Fns [Mths Funs]]] KM Ext En KMn Cov.
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & PR).
  destruct (PublicFormation.provide_result_shape W _ _ _ _ _ PR) as [Esub _].
  set (d' := append_environment (LR.result_of d3 outs gensof dr) d).
  assert (Gk : forall t, incl (A.gens t) (LR.PA.FS.CopyA.S.capture_gens C) -> ground d' t = ground d t)
    by (intros t Inc; exact (ground_after F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku d d3 dr t Inc)).
  assert (Gc : forall t, incl (A.gens t) (A.context_gens C) -> ground d' t = ground d t)
    by (intros t Inc; apply Gk; intros y Iy; exact (LR.PA.FS.CopyA.S.cg_l C y (Inc y Iy))).
  assert (Gm : forall ps, (forall t, In t ps -> incl (A.gens t) (A.context_gens C)) -> map (ground d) ps = map (ground d') ps).
  { intros ps Inc. apply map_ext_in. intros t It. symmetry. exact (Gc t (Inc t It)). }
  assert (Lk : forall k, (forall k1 p1, In (k1, p1) news -> A.rkey_of C' k1 <> k) -> lookup_provision k d' = lookup_provision k d).
  { intros k N. unfold lookup_provision, d'. cbn [provision_values append_environment LR.result_of]. rewrite lookup_entry_app.
    destruct (lookup_entry k (provision_values dr)) as [pv|] eqn:E; [|reflexivity].
    exfalso. destruct (Cov k pv E) as [k1 [p1 [I R]]]. exact (N k1 p1 I R). }
  assert (KMC : PublicCallRealize.KeysMatch (A.c_provisions C')) by (rewrite En; exact (keys_match_app news _ KMn KM)).
  assert (Key : forall k1 p1, In (k1, p1) news -> In k1 (map fst news))
    by (intros k1 p1 I; change k1 with (fst (k1, p1)); apply in_map; exact I).
  split; [|split; [|split]].
  - intros v t Nk L. change (A.c_vals C') with (vals_of (A.c_provisions C')) in L.
    rewrite (vals_by_ilook _ v KMC), En, (ilook_app_out _ _ _ _ Nk), <- (vals_by_ilook _ v KM) in L.
    destruct (Vals v t L) as [w [Look T]]. exists w.
    rewrite Lk; [|intros k1 p1 I R; pose proof (rkey_of_kind C' k1 _ R) as K; cbn in K; subst k1; exact (Nk (Key _ _ I))].
    split; [exact Look|]. rewrite Gc; [eapply value_typed_extends; [exact Ext|exact T]|].
    destruct (vals_ilook _ v t KM L) as [a Il]. apply LR.PA.FS.CopyA.S.ilook_in in Il.
    intros x Ix. apply (LR.PA.FS.CopyA.S.prov_context_gens C _ _ Il). exact (proj1 (LR.PA.FS.CopyA.S.sig_provision_gens (A.ProvidedVal t a) (A.FSig [] t) eq_refl) x Ix).
  - intros f g0 Nk L. change (A.c_fns C') with (fns_of (A.c_provisions C')) in L.
    rewrite (fns_by_ilook _ f KMC), En, (ilook_app_out _ _ _ _ Nk), <- (fns_by_ilook _ f KM) in L.
    destruct (Fns f g0 L) as [pv [Look Ok]]. exists pv.
    rewrite Lk; [|intros k1 p1 I R; pose proof (rkey_of_kind C' k1 _ R) as K; cbn in K; subst k1; exact (Nk (Key _ _ I))].
    split; [exact Look|].
    destruct (fns_ilook _ f g0 KM L) as [a Il]. apply LR.PA.FS.CopyA.S.ilook_in in Il.
    destruct (LR.PA.FS.CopyA.S.sig_provision_gens (A.ProvidedFn g0 a) g0 eq_refl) as [Gr Gp].
    refine (LR.PA.FS.CopyA.S.callable_transfer W Prog argv read state_ok n n H3 d d' _ _ _ _ pv (le_n n) _ _
      (LR.PA.FS.CopyA.S.callable_extends W Prog argv read state_ok n H H3 d _ _ pv Ext Ok)).
    + apply Gm. intros t It x Ix. exact (LR.PA.FS.CopyA.S.prov_context_gens C _ _ Il x (Gp t It x Ix)).
    + symmetry. apply Gc. intros x Ix. exact (LR.PA.FS.CopyA.S.prov_context_gens C _ _ Il x (Gr x Ix)).
  - intros r m g0 Nk L. change (A.c_mths C') with (mths_of (A.c_provisions C')) in L.
    rewrite (mths_by_ilook _ r m KMC), En, (ilook_app_out _ _ _ _ Nk), <- (mths_by_ilook _ r m KM) in L.
    destruct (Mths r m g0 L) as [pv [Look Ok]]. exists pv.
    rewrite (rkey_kept C C' news r m Esub En Nk).
    rewrite Lk.
    2: { intros k1 p1 I R. pose proof (rkey_of_kind C' k1 _ R) as K. unfold A.rkey in K. cbn in K. destruct K as [i ->].
         pose proof (KMn _ _ I) as Kd. destruct i as [|q i]; [|destruct p1; cbn in Kd; contradiction].
         exact (Nk (Key _ _ I)). }
    split; [exact Look|].
    destruct (mths_ilook _ r m g0 KM L) as [a Il]. apply LR.PA.FS.CopyA.S.ilook_in in Il.
    destruct (LR.PA.FS.CopyA.S.sig_provision_gens (A.ProvidedMethod g0 a) g0 eq_refl) as [Gr Gp].
    refine (LR.PA.FS.CopyA.S.callable_transfer W Prog argv read state_ok n n H3 d d' _ _ _ _ pv (le_n n) _ _
      (LR.PA.FS.CopyA.S.callable_extends W Prog argv read state_ok n H H3 d _ _ pv Ext Ok)).
    + rewrite (map_ext_in (ground d) (ground d') (r :: A.sg_params g0)); [reflexivity|].
      intros t [<-|It]; symmetry; [apply Gk; exact (LR.PA.FS.CopyA.S.cg_key W C r m [] _ (proj1 (proj2 Fc)) Il)|].
      apply Gc. intros x Ix. exact (LR.PA.FS.CopyA.S.prov_context_gens C _ _ Il x (Gp t It x Ix)).
    + symmetry. apply Gc. intros x Ix. exact (LR.PA.FS.CopyA.S.prov_context_gens C _ _ Il x (Gr x Ix)).
  - intros u0 fx0 Nk L. change (A.c_funs C') with (funs_of (A.c_provisions C')) in L.
    rewrite (funs_by_ilook _ u0 KMC), En, (ilook_app_out _ _ _ _ Nk), <- (funs_by_ilook _ u0 KM) in L.
    destruct (Funs u0 fx0 L) as [pv [gfx [Look [Ok [Cv Vm]]]]]. exists pv, gfx.
    rewrite Lk; [|intros k1 p1 I R; pose proof (rkey_of_kind C' k1 _ R) as K; cbn in K; subst k1; exact (Nk (Key _ _ I))].
    split; [exact Look|]. split; [exact (proj1 (LR.PA.FS.CopyA.S.closure_lower W Prog argv read state_ok n n H H3 (le_n n) Ext) u0 gfx pv Ok)|].
    split; [exact Cv|].
    intros x tx Ix. rewrite Gc; [exact (Vm x tx Ix)|].
    pose proof (funs_ilook _ u0 fx0 KM L) as Il. apply LR.PA.FS.CopyA.S.ilook_in in Il.
    intros y Iy. apply (LR.PA.FS.CopyA.S.prov_context_gens C _ _ Il). unfold A.provision_gens. cbn [A.provision_signature].
    apply in_or_app. right. apply in_flat_map. exists (x, Some tx). split; [|exact Iy].
    cbn [A.provision_inst]. apply in_map_iff. exists (x, tx). split; [reflexivity|exact Ix].
Qed.

(* ---- ChainCoreOk ------------------------------------------------------------ *)

Theorem chain_core_ok : ChainNewOk -> LR.KeptOutputsUnmentioned W -> LR.ChainCoreOk W Prog argv read state_ok.
Proof.
  intros New Ku F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms dc rms gfx h0 gf0 gg dg n ps a r bodies
    Ch0 nsh0 Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr ks' binds0 pins d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm Chain Ok0 LD Binding PinsOk Sel Sel2 Run Ext Pre3.
  pose proof Pre0 as [_ [[_ [_ Al0]] [_ [_ [[_ [Env [Section KM]]] [Fc _]]]]]].
  pose proof (PublicContextFacts.section_canonical C Section) as Idem.
  pose proof (LR.PA.FS.CopyA.S.pre_absorbs_canon W Prog argv read state_ok _ _ _ _ _ _ _ _ Pre0) as Abs.
  pose proof Pre3 as [_ [[_ [_ Al3]] [_ [_ [Real3 _]]]]].
  pose proof LD as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OutsR & OutM & _ & _ & PR' & RR & _).
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OutsI & PR).
  destruct (LR.PA.FS.CopyA.chain_sources_range W _ _ _ _ _ _ _ _ Chain eq_refl (LR.PA.FS.CopyA.S.link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks')
    ltac:(intros C0 o C0' ks0 P0; unfold LR.PA.FS.CopyA.S.link_sources; rewrite length_map;
          exact (PublicCallerKeys.provide_result_length W h0 _ _ _ _ _ _ _ _ PR' P0))) as [_ Rg].
  destruct (chain_selection rr (A.link_site Cg Cb pins outs' gen) d3 H3 h0 Ch0 _ outs' gen Cr ks' (map (A.rkey_of C') ks) rms _
    Real3 PR' RR Rg) as [dr Sdr].
  destruct (provide_result_news W _ _ _ _ _ PR) as [news [En KMn]].
  pose proof (selection_news u C _ C' ks rms _ d3 dr PR Sdr news En) as Cov.
  pose proof (New F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms dc rms gfx h0 gf0 gg dg n ps a r bodies
    Ch0 nsh0 Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr ks' binds0 pins d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm Chain Ok0 LD Binding PinsOk Sel Sel2 Run Ext Pre3
    dr Sdr news En) as EnvN.
  exists dr. split; [exact Sdr|]. split.
  { intros t It. pose proof (proj1 (OutsI t) It) as Ft.
    pose proof (proj2 (OutsR t) (proj1 (LR.chain_outputs W _ _ _ _ _ _ _ _ Chain t) Ft)) as It'.
    destruct (OutM t It') as [m Lm]. rewrite (Al3 t m Lm). discriminate. }
  split; [exact (alias_after F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku d d3 dr Al0 Abs Idem)|].
  split.
  - apply (envok_of_on (fun k => In k (map fst news))); [intros k; apply item_in_dec|exact EnvN|].
    exact (envok_kept F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ku rr d d3 dr H H3 news Env KM Ext En KMn Cov).
  - unfold LR.PA.FS.CopyA.S.KeysMatch. rewrite En. exact (keys_match_app _ _ KMn KM).
Qed.

(* So ChainResultsOk, from the new provisions' correspondence and the
   kept-output premise. *)
Corollary chain_results_of_new : ChainNewOk -> LR.KeptOutputsUnmentioned W -> LR.PA.FS.CopyA.ChainResultsOk W Prog argv read state_ok.
Proof. intros New Ku. exact (LR.chain_results_scoped W Prog argv read state_ok (chain_core_ok New Ku) Ku). Qed.

(* ---- what the body provides at each result key ------------------------------- *)

(* A result key k (renamed into the body's terms), promised as p by the
   formal result, is realized in the body's final state by pv: p's types
   read at the body's meanings of the output roles (rr, role_reads),
   a method's receiver as the body's site reads it. *)
Definition BodyReal (n : nat) (H3 : heap_types) (d3 : environment) (Cb : A.cx) (rr : subst) (k : item)
    (p : A.provision) (pv : provision_value) : Prop :=
  match p with
  | A.ProvidedVal t _ => exists w, pv = ValueProvision w /\ value_typed W P H3 w (ground d3 (app_subst rr t))
  | A.ProvidedFn g _ => CallableOk n H3 d3 (map (app_subst rr) (A.sg_params g)) (app_subst rr (A.sg_ret g)) pv
  | A.ProvidedMethod g _ => exists rb m i, A.at_site Cb k = IMth rb m i /\
      CallableOk n H3 d3 (rb :: map (app_subst rr) (A.sg_params g)) (app_subst rr (A.sg_ret g)) pv
  | A.ProvidedFunctor fx => exists u gfx, k = IFun u /\ ClosureOk n H3 u gfx pv /\
      forall x tx, In (x, tx) fx -> lookup_subst x gfx = Some (ground d3 (app_subst rr tx))
  end.

Lemma absorbs_sub : forall Cb d3 t t', PublicContextFacts.CxSection Cb -> LR.PA.FS.CopyA.S.Absorbs Cb d3 -> app_subst (A.c_sub Cb) t = app_subst (A.c_sub Cb) t' ->
  ground d3 t = ground d3 t'.
Proof.
  intros Cb d3 t t' Section Abs E. apply Abs.
  apply (PublicContextFacts.context_canon_iff_mergeeq Cb Section). exact E.
Qed.

Lemma absorbs_sub_map : forall Cb d3 ts ts', PublicContextFacts.CxSection Cb -> LR.PA.FS.CopyA.S.Absorbs Cb d3 -> map (app_subst (A.c_sub Cb)) ts = map (app_subst (A.c_sub Cb)) ts' ->
  map (ground d3) ts = map (ground d3) ts'.
Proof.
  intros Cb d3 ts. induction ts as [|t r IH]; intros [|t' r'] Section Abs E; cbn in E; try discriminate; [reflexivity|].
  injection E as E1 E2. cbn. rewrite (absorbs_sub Cb d3 t t' Section Abs E1), (IH r' Section Abs E2). reflexivity.
Qed.

Lemma body_provides : forall n Cb d3 H3 Cr ks reads,
  LR.PA.FS.CopyA.S.Realizes W Prog argv read state_ok n Cb d3 H3 -> A.ResultRealizes Cb Cr reads ks ->
  forall k, In k ks -> exists p pv, A.ilook (A.provision_id k) (A.c_provisions Cr) = Some p /\
    lookup_provision (A.rkey_of Cb (A.at_site Cb k)) d3 = Some pv /\
    BodyReal n H3 d3 Cb reads k p pv.
Proof.
  intros n Cb d3 H3 Cr ks reads [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]] RR k Ik.
  destruct (RR k Ik) as [req [act [Lr [La [Kind [Sig Inst]]]]]].
  pose proof (KM _ _ (LR.PA.FS.CopyA.S.ilook_in _ _ _ La)) as Kd.
  exists req.
  destruct k as [x|v|f|rf m i|u0]; cbn [A.at_site A.provision_id] in La, Kd |- *;
    destruct act as [tb ab|gb ab|gb ab|fxb]; cbn in Kd; try contradiction;
    destruct req as [t a|g a|g a|fx]; cbn in Kind; try contradiction.
  - (* val *)
    pose proof (ilook_vals _ v tb ab KM La) as Lv. destruct (Vals v tb Lv) as [w [Look T]].
    exists (ValueProvision w). split; [exact Lr|]. split; [exact Look|]. exists w. split; [reflexivity|].
    cbn in Sig. injection Sig as Sig. rewrite (absorbs_sub Cb d3 _ _ Section Abs Sig). exact T.
  - (* fn *)
    pose proof (ilook_fns _ f gb ab KM La) as Lf. destruct (Fns f gb Lf) as [pv [Look Ok]].
    exists pv. split; [exact Lr|]. split; [exact Look|]. cbn in Sig. injection Sig as Sp Sr.
    refine (LR.PA.FS.CopyA.S.callable_transfer W Prog argv read state_ok n n H3 d3 d3 _ _ _ _ pv (le_n n) _ _ Ok).
    + symmetry. apply (absorbs_sub_map Cb d3 _ _ Section Abs). exact Sp.
    + symmetry. exact (absorbs_sub Cb d3 _ _ Section Abs Sr).
  - (* method *)
    clear Kd.
    set (rb := A.canon Cb (app_subst (map (fun q => (fst q, snd q)) (A.c_ty Cb)) rf)) in *.
    pose proof (ilook_mths _ rb m gb ab KM La) as Lm. destruct (Mths rb m gb Lm) as [pv [Look Ok]].
    exists pv. split; [exact Lr|]. split; [exact Look|]. exists rb, m, i. split; [reflexivity|].
    cbn in Sig. injection Sig as Sp Sr.
    refine (LR.PA.FS.CopyA.S.callable_transfer W Prog argv read state_ok n n H3 d3 d3 _ _ _ _ pv (le_n n) _ _ Ok).
    + cbn [map]. f_equal. symmetry. apply (absorbs_sub_map Cb d3 _ _ Section Abs). exact Sp.
    + symmetry. exact (absorbs_sub Cb d3 _ _ Section Abs Sr).
  - (* functor *)
    assert (Lu : A.nlook u0 (A.c_funs Cb) = Some fxb).
    { change (A.c_funs Cb) with (funs_of (A.c_provisions Cb)). rewrite (funs_by_ilook _ u0 KM), La. reflexivity. }
    destruct (Funs u0 fxb Lu) as [pv [gfx [Look [Ok [_ Vm]]]]].
    exists pv. split; [exact Lr|]. split; [exact Look|]. exists u0, gfx. split; [reflexivity|]. split; [exact Ok|].
    intros x tx Ix.
    assert (Ip : In (x, Some (app_subst (A.c_sub Cb) (app_subst reads tx)))
      (A.provision_inst (A.provision_subst (A.c_sub Cb) (A.provision_subst reads (A.ProvidedFunctor fx))))).
    { cbn. rewrite !map_map. apply in_map_iff. exists (x, tx). split; [reflexivity|exact Ix]. }
    pose proof (Inst _ _ Ip) as Ia. cbn in Ia. rewrite map_map in Ia. apply in_map_iff in Ia as [[x' tb] [E Ib]].
    injection E as -> E. rewrite (Vm x tb Ib). f_equal. symmetry. exact (absorbs_sub Cb d3 _ _ Section Abs (eq_sym E)).
Qed.

(* ---- FamilyLinkResultsOk: the selection ------------------------------------- *)


(* PREMISE (T15). FamilyLinkResultsOk, its selection discharged: the
   family's new methods are installed by what the body selection dr
   finds, callable at their signatures (the correspondence with no type
   outputs). That dr fills only the run-time keys of new methods is true
   by construction (Audit.result_selection), proved in
   family_link_results_ok. *)
Definition FamilyLinkNewOk : Prop := forall C0 C u matching th sm Cr ks,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 -> A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  PublicFormation.TysScoped W C ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  forall rr F d types H g st fx h dc gfx gg dg n ps a r bodies Cu ns Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr' ks' binds0
    pins d1 d2 d3 g3 st3 H3 j',
  LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok rr F C0 d types H g st -> A.nlook u (A.c_funs C0) = Some fx ->
  lookup_provision (IFun u) d = Some (FunctorClosure h (Some (A.link_map Cu Cg linktarget nsg)) dc None) ->
  ClosureOk rr H u gfx (FunctorClosure h (Some (A.link_map Cu Cg linktarget nsg)) dc None) ->
  LR.PA.FS.CopyA.S.FixedMeans W u d fx gfx ->
  LR.PA.FS.CopyA.S.LinkData W Prog u gg h dg n ps a r bodies Cu ns Cg nsg Fg wrap Cb Fb made outs' gen Cr' ks' binds0 pins ->
  PublicContextFacts.ClosureBinding W u Cu ns Cg nsg linktarget dc gfx ->
  LR.PA.FS.CopyA.S.PinsAgree W u Cu Cg dc gfx pins ->
  select_provisions d sm = Some d1 ->
  select_provisions d1 (A.link_map Cu Cg linktarget nsg) = Some d2 ->
  LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j' P (application_environment P d h th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok rr Fb Cb d3 [] H3 g3 st3 ->
  forall dr, select_provisions d3 (result_selection (map (A.rkey_of Cr) ks) None (LR.PA.FS.CopyA.S.link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks')) = Some dr ->
  forall r m g, In (A.mkey r m) ks -> A.mlook r m (A.c_mths Cr) = Some g -> exists pv,
     lookup_provision (A.rkey Cr r m) dr = Some pv /\ CallableOk rr H3 d (r :: A.sg_params g) (A.sg_ret g) pv.

Theorem family_link_results_ok : FamilyLinkNewOk -> LR.PA.FS.FamilyLinkResultsOk W Prog argv read state_ok.
Proof.
  intros New C0 C u matching th sm Cr ks Es Emg Ep El Ts FA rr F d types H g st fx h dc gfx gg dg n ps a r bodies Cu ns Cg nsg linktarget Fg wrap
    Cb Fb made outs' gen Cr' ks' binds0 pins d1 d2 d3 g3 st3 H3 j' Pre0 Fx Look Ok Fm LD Binding PinsOk Sel Sel2 Run Ext Pre3.
  pose proof Pre3 as [_ [_ [_ [_ [Real3 _]]]]].
  pose proof LD as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & PR' & RR & _).
  destruct (chain_selection rr (A.link_site Cg Cb pins outs' gen) d3 H3 u Cu _ outs' gen Cr' ks' (map (A.rkey_of Cr) ks) None _
    Real3 PR' RR
    ltac:(intros s0 I0; exact I0)) as [dr Sdr].
  exists dr. split; [exact Sdr|]. split.
  - intros K pv L. destruct (PublicFamilyStep.family_news W C u matching th sm Cr ks FA) as (news & En & _ & _ & NewM & Keys).
    rewrite (select_lookup _ _ _ K Sdr) in L. destruct (sm_find K _) as [s|] eqn:Fs; [|discriminate].
    destruct (PublicCallerKeys.selection_in _ _ _ _ _ (PublicCallerKeys.sm_find_some_in _ _ _ Fs)) as [Ik _].
    apply in_map_iff in Ik as [k1 [<- Ik1]]. destruct (proj1 (Keys k1) Ik1) as [p1 Ip1].
    destruct (NewM k1 p1 Ip1) as (r1 & m1 & _ & _ & -> & _). exists r1, m1. split; [exact Ik1|reflexivity].
  - exact (New C0 C u matching th sm Cr ks Es Emg Ep El Ts FA rr F d types H g st fx h dc gfx gg dg n ps a r bodies Cu ns Cg nsg linktarget Fg wrap
      Cb Fb made outs' gen Cr' ks' binds0 pins d1 d2 d3 g3 st3 H3 j' Pre0 Fx Look Ok Fm LD Binding PinsOk Sel Sel2 Run Ext Pre3 dr Sdr).
Qed.

(* ---- what a run of bindings writes ------------------------------------------- *)

(* Only an application writes meanings (at its destinations) and aliases
   (at its outputs); a type binding writes an alias. *)
Definition binding_dests (bs : list binding) : list nat :=
  flat_map (fun b => match b with BApply _ _ outs _ _ => map snd outs | _ => [] end) bs.
Definition binding_aliases (bs : list binding) : list nat :=
  flat_map (fun b => match b with BApply _ _ outs _ _ => map fst outs | BType t _ => [t] | _ => [] end) bs.

Definition Grows (Xm Xa : list nat) (d d' : environment) : Prop :=
  (exists w, type_meanings d' = (w ++ type_meanings d)%list /\ incl (map fst w) Xm) /\
  (exists a, type_aliases d' = (a ++ type_aliases d)%list /\ incl (map fst a) Xa).

Lemma grows_refl : forall Xm Xa d, Grows Xm Xa d d.
Proof. intros Xm Xa d. split; exists []; split; [reflexivity|intros x []|reflexivity|intros x []]. Qed.

Lemma grows_trans : forall X1 Y1 X2 Y2 d1 d2 d3, Grows X1 Y1 d1 d2 -> Grows X2 Y2 d2 d3 -> Grows (X1 ++ X2) (Y1 ++ Y2) d1 d3.
Proof.
  intros X1 Y1 X2 Y2 d1 d2 d3 [[w1 [E1 I1]] [a1 [F1 J1]]] [[w2 [E2 I2]] [a2 [F2 J2]]]. split.
  - exists (w2 ++ w1)%list. split; [rewrite E2, E1, app_assoc; reflexivity|].
    intros x Ix. rewrite map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; apply in_or_app; [right; exact (I2 x Ix)|left; exact (I1 x Ix)].
  - exists (a2 ++ a1)%list. split; [rewrite F2, F1, app_assoc; reflexivity|].
    intros x Ix. rewrite map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; apply in_or_app; [right; exact (J2 x Ix)|left; exact (J1 x Ix)].
Qed.

Lemma grows_install : forall Xm Xa d k v, Grows Xm Xa d (install_provision k v d).
Proof. intros Xm Xa d k v. split; exists []; split; [reflexivity|intros x []|reflexivity|intros x []]. Qed.

Lemma read_result_keys : forall d Dr outs w a, read_result_types d Dr outs = Some (w, a) ->
  map fst w = map snd outs /\ map fst a = map fst outs.
Proof.
  intros d Dr outs. induction outs as [|[src dst] rest IH]; intros w a E; cbn in E.
  - injection E as <- <-. split; reflexivity.
  - destruct (existsb (item_eqb (ITy src)) Dr); [|discriminate].
    destruct (output_meaning d src) as [m|]; [|discriminate].
    destruct (read_result_types d Dr rest) as [[w' a']|]; [|discriminate].
    injection E as <- <-. destruct (IH w' a' eq_refl) as [Ew Ea]. cbn. rewrite Ew, Ea. split; reflexivity.
Qed.

Lemma apply_functor_writes : forall j Pv d u th outs sm rs st dr st1,
  LR.PA.FS.CopyA.S.AR.O.I.apply_functor j Pv d u th outs sm rs st = inl (dr, st1) ->
  Grows (map snd outs) (map fst outs) d (append_environment dr d).
Proof.
  intros j Pv d u th outs sm rs st dr st1 E. destruct j as [|j]; [discriminate|]. cbn in E.
  unfold result_environment in E.
  repeat match type of E with context[match ?x with _ => _ end] =>
    match x with context[read_result_types] => fail 1 | _ => destruct x; try discriminate E end end.
  destruct (read_result_types _ _ outs) as [[w a]|] eqn:R; [|discriminate].
  injection E as <- _. destruct (read_result_keys _ _ _ _ _ R) as [Ew Ea].
  split; [exists w|exists a]; cbn; (split; [reflexivity|]); [rewrite Ew|rewrite Ea]; intros x I; exact I.
Qed.

Lemma grows_mono : forall X Y X' Y' d d', incl X X' -> incl Y Y' -> Grows X Y d d' -> Grows X' Y' d d'.
Proof.
  intros X Y X' Y' d d' IX IY [[w [E I]] [a [F J]]]. split; [exists w|exists a]; split; auto.
  - intros x Ix. exact (IX x (I x Ix)).
  - intros x Ix. exact (IY x (J x Ix)).
Qed.

Lemma bindings_writes : forall j Pv d g st bs d' g' st', LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j Pv d g st bs = inl (d', g', st') ->
  Grows (binding_dests bs) (binding_aliases bs) d d'.
Proof.
  induction j as [|j IH]; intros Pv d g st bs d' g' st' E; [discriminate|].
  destruct bs as [|b r]; cbn in E.
  - injection E as <- _ _. apply grows_refl.
  - assert (Step : forall Xm Xa d1, Grows Xm Xa d d1 -> forall g1 st1, LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j Pv d1 g1 st1 r = inl (d', g', st') ->
      incl (Xm ++ binding_dests r) (binding_dests (b :: r)) -> incl (Xa ++ binding_aliases r) (binding_aliases (b :: r)) ->
      Grows (binding_dests (b :: r)) (binding_aliases (b :: r)) d d').
    { intros Xm Xa d1 G1 g1 st1 E1 IX IY. exact (grows_mono _ _ _ _ _ _ IX IY (grows_trans _ _ _ _ _ _ _ G1 (IH _ _ _ _ _ _ _ _ E1))). }
    destruct b as [t tau|v e1|f f' types sm|tauk m th f' types sm|u h tys sm smc|u u' sm rm|u th outs sm rs|i p].
    + refine (Step [] [t] (bind_type d t tau) _ g st E _ _).
      * split; [exists []|exists [(t, ground d tau)]]; split; [reflexivity|intros x []|reflexivity|]. intros x [<-|[]]; left; reflexivity.
      * intros x Ix. exact Ix.
      * intros x Ix. exact Ix.
    + destruct (LR.PA.FS.CopyA.S.AR.O.I.eval j Pv d g st e1) as [sg g1 st1]; destruct sg; try discriminate.
      refine (Step [] [] _ (grows_install _ _ _ _ _) g1 st1 E _ _); intros x Ix; exact Ix.
    + destruct (select_provisions d sm); [|discriminate].
      refine (Step [] [] _ (grows_install _ _ _ _ _) g st E _ _); intros x Ix; exact Ix.
    + destruct (select_provisions d sm); [|discriminate].
      refine (Step [] [] _ (grows_install _ _ _ _ _) g st E _ _); intros x Ix; exact Ix.
    + destruct (select_provisions d smc); [|discriminate].
      refine (Step [] [] _ (grows_install _ _ _ _ _) g st E _ _); intros x Ix; exact Ix.
    + destruct (lookup_provision (IFun u') d) as [pv|]; [destruct pv; try discriminate|discriminate].
      refine (Step [] [] _ (grows_install _ _ _ _ _) g st E _ _); intros x Ix; exact Ix.
    + destruct (LR.PA.FS.CopyA.S.AR.O.I.apply_functor j Pv d u th outs sm rs st) as [[dr st1]|] eqn:A1; [|discriminate].
      refine (Step _ _ _ (apply_functor_writes _ _ _ _ _ _ _ _ _ _ _ A1) g st1 E _ _); intros x Ix; exact Ix.
    + refine (Step [] [] _ (grows_install _ _ _ _ _) g st E _ _); intros x Ix; exact Ix.
Qed.

End Core.
End ChainCore.
