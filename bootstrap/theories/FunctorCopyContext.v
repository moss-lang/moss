(* The actual copy guards and setter retain the fixed-input context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr CanonTypes FunctorDecl BindFunctorCtx ProvideResultFixed CopyFixedExec
 ResultMentionsExact ResultMentionsNoDup.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Section Copy.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).

Theorem actual_functor_snapshot : forall g n names args res mi e c F C s xs sf sg stamp sa,
 nget(w_syms w)(gs_id g)=Some g ->gs_decl g=ADFunctor n names args res None ->
 functor_fixed g s=Good(xs,sf) ->bind_fixed_present mi e g xs sf=Good(tt,sg) ->
 Inv s ->st_w s=w ->Later w P R Fl sg ->CxRel w R Fl c e F C ->
 let fx:=output_fixed_values(A.c_ty C)xs in
 A.FixedReadings WW(gs_id g)(A.c_ty C)fx /\
 CxRel w R Fl c
   (set_app(set_fun e(gs_id g)stamp)(KyFun(gs_id g))
      (map(fun x=>(x,option_map(canon e)(tn_get e x)))xs)sa)
   F(A.set_fun C(gs_id g)fx).
Proof.
 intros g n names args res mi e c F C s xs sf sg stamp sa Hg Decl Run Guard Hi Hw L Hc fx.
 destruct(bind_fixed_present_success mi e g xs sf sg Guard)as[-> Present].
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload g n names args res s xs sf Hg Decl Run Hi Hw L)as Dom.
 assert(Av:forall x,A.Fixed WW(gs_id g)x ->A.nlook x(A.c_ty C)<>None).
 {intros x Fix;rewrite(cr_ty _ _ _ _ _ _ _ Hc).
  destruct(tn_get e x)eqn:Here;[discriminate|].
  exfalso;exact(Present x (proj2(Dom x)Fix)Here). }
 assert(Exact:forall x tx,In(x,tx)fx<->A.Fixed WW(gs_id g)x/\A.nlook x(A.c_ty C)=Some tx).
 {apply output_fixed_values_exact;assumption. }
 split;[exact Exact|].
 apply(cxrel_functor w R Fl c e F C(gs_id g)stamp _ sa fx Hc).
 - unfold FunctorDecl;exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),n,names,args,res.
   split;[exact(w_decl_read w R Fl _ _ Hg)|cbn;exact Decl].
 - unfold fx;rewrite output_fixed_values_keys;exact(functor_fixed_nodup g s xs sf Run).
 - intros x tx Inx;exact(proj1(proj1(Exact x tx)Inx)).
 - unfold fun_tr_inst,A.fixed_app,fx;rewrite map_map.
   rewrite <-(output_fixed_values_apps(A.c_ty C)xs).
   + apply map_ext;intro x;cbn[fst snd].
     rewrite(cr_ty _ _ _ _ _ _ _ Hc).
     destruct(tn_get e x);reflexivity.
   + intros x Inx;apply Av;apply Dom;exact Inx.
Qed.

Theorem actual_copy_fixed_agreement : forall g n names args res mi e c F C s xs sf sg,
 nget(w_syms w)(gs_id g)=Some g ->gs_decl g=ADFunctor n names args res None ->
 functor_fixed g s=Good(xs,sf) ->copy_fixed_current mi e g xs sf=Good(tt,sg) ->
 Inv s ->st_w s=w ->Later w P R Fl sg ->CxRel w R Fl c e F C ->
 A.FixedAgreement WW C(gs_id g).
Proof.
 intros g n names args res mi e c F C s xs sf sg Hg Decl Run Guard Hi Hw L Hc.
 destruct(copy_fixed_current_success mi e g xs sf sg Guard)as[-> Agreement].
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload g n names args res s xs sf Hg Decl Run Hi Hw L)as Dom.
 unfold A.FixedAgreement;intros x Fix.
 pose proof(Agreement x(proj2(Dom x)Fix))as Same.
 rewrite(cr_funapps _ _ _ _ _ _ _ Hc),(cr_ty _ _ _ _ _ _ _ Hc).
 unfold SourceFixedAgreement in Same.
 destruct(get_app e(KyFun(gs_id g)))as[[a stamp]|]eqn:App;cbn[option_map fst];[|exact I].
 destruct(tn_get e x)as[now|]eqn:Now;cbn[option_map];[|exact I].
 change(match A.nlook x(fun_tr_inst Fl a)with
 |Some(Some was)=>A.canon C was=A.canon C(tr Fl(canon e now))|_=>True end).
 assert(Look:forall (aa:Elab.inst) k,A.nlook k(fun_tr_inst Fl aa)=option_map(option_map(tr Fl))(nassoc k aa)).
 {intros aa;induction aa as[|[k v]aa IH];intro j;[reflexivity|].
  cbn[A.nlook fun_tr_inst nassoc assoc map fst snd];destruct(Nat.eqb j k);[reflexivity|apply IH]. }
 rewrite Look.
 destruct(nassoc x a)as[[raw|]|]eqn:At;cbn[option_map];try exact I.
 unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ Hc).
 rewrite !tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 f_equal;unfold nassoc in At;rewrite At in Same;exact Same.
Qed.
End Copy.
