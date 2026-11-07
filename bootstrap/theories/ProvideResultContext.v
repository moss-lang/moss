(* Actual output setters preserve the body context relation. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore
 NominalSupport NominalMergeSupport CanonTypes Merge CtxWalk.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma output_env_support_app:forall W e k a stamp,
 EnvSupport W e->EnvSupport W(set_app e k a stamp).
Proof. intros W e k a stamp[T U V F M L];constructor;auto. Qed.
Lemma get_app_function_other:forall e f a stamp r m,
 get_app(set_app e(KyFn f)a stamp)(KyMth r m)=get_app e(KyMth r m).
Proof.
 intros;unfold get_app,set_app;cbn[en_apps].
 rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;reflexivity.
Qed.
Lemma get_app_function_fun:forall e f sg raw stamp u,
 get_app(set_app(set_fn e f sg)(KyFn f)raw stamp)(KyFun u)=get_app e(KyFun u).
Proof.
 intros;unfold get_app,set_app,set_fn;cbn[en_apps].
 rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;reflexivity.
Qed.
Section Context.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Definition output_sig(e:env)(sg:Elab.fsig):A.fsig:=
 A.FSig(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))(tr Fl(canon e(sg_ret sg))).
Lemma cxrel_function:forall c e F C f sg raw app stamp,
 CxRel w R Fl c e F C ->SigOK sg ->SigSupport WW sg ->
 app=map(fun q=>(fst q,option_map(tr Fl)(snd q)))raw ->
 (exists snapshot,A.SigOf WW f snapshot None(output_sig e sg)/\A.AppOf WW f snapshot app) ->
 CxRel w R Fl c(set_app(set_fn e f sg)(KyFn f)raw stamp)F
  (A.provide C(IFn f)app(A.set_fn C f(output_sig e sg))).
Proof.
 intros c e F C f sg raw app stamp H Ws Ss Eapp Snapshot.
 pose proof(cr_fn_sigs _ _ _ _ _ _ _ H)as FnSigs.
 pose proof(cr_fns _ _ _ _ _ _ _ H)as Look.
 pose proof(cr_fns_clean _ _ _ _ _ _ _ H)as Clean.
 pose proof(cr_apps _ _ _ _ _ _ _ H)as Apps.
 pose proof(cr_vapps _ _ _ _ _ _ _ H)as Va.
 pose proof(cr_fapps _ _ _ _ _ _ _ H)as Fa.
 pose proof(cr_funapps _ _ _ _ _ _ _ H)as Fu.
 pose proof(cr_funfixed _ _ _ _ _ _ _ H)as Fx.
 pose proof(cr_support _ _ _ _ _ _ _ H)as Es.
 destruct H;constructor;auto.
 - intro x;cbn[A.provide A.set_fn A.c_fns A.nlook].
   unfold Elab.set_app,Elab.set_fn;cbn[en_fns].
   rewrite nget_nset;destruct(Nat.eqb x f);cbn[option_map];[reflexivity|exact(Look x)].
 - intros x y Ey;unfold Elab.set_app,Elab.set_fn in Ey;cbn[en_fns]in Ey.
   rewrite nget_nset in Ey;destruct(Nat.eqb x f);[injection Ey as <-;exact Ws|exact(Clean _ _ Ey)].
 - apply output_env_support_app;exact(env_support_fn f Es Ss).
 - intros r m Cl;cbn[A.provide A.set_fn A.c_apps A.ilook A.mkey Audit.item_eqb].
   rewrite get_app_function_other;exact(Apps r m Cl).
 - intro x;cbn[A.provide A.c_apps A.ilook Audit.item_eqb].
   unfold get_app,Elab.set_app;cbn[en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;cbn[key_eqb];exact(Va x).
 - intro x;cbn[A.provide A.c_apps A.ilook Audit.item_eqb].
   unfold get_app,Elab.set_app;cbn[en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;cbn[key_eqb].
   destruct(Nat.eqb x f);cbn[option_map fst];[rewrite Eapp;reflexivity|exact(Fa x)].
 - intro u;cbn[A.provide A.c_apps A.ilook Audit.item_eqb].
   rewrite get_app_function_fun;exact(Fu u).
 - intros u fx x tx Lookup Hin;cbn[A.provide A.set_fn A.c_funs]in Lookup.
   destruct(Fx u fx x tx Lookup Hin)as[Fixed(a&st&ry&Ga&Gx&Eq)].
   split;[exact Fixed|];exists a,st,ry;split;[rewrite get_app_function_fun;exact Ga|auto].
 - destruct Snapshot as[tm[Sig App]]. eapply fn_snapshots_function;[exact FnSigs|exact Sig|exact App].
Qed.
End Context.

From NM Require Import CoreMoss TypeReadingInj CtxMethod MethodRows.
Lemma get_mth_output_app:forall e k a stamp r m,
 get_mth(set_app e k a stamp)r m=get_mth e r m.
Proof. reflexivity. Qed.
Lemma get_app_method_fun:forall e r m sg a stamp u,
 get_app(set_app(set_mth e r m sg)(KyMth r m)a stamp)(KyFun u)=get_app e(KyFun u).
Proof.
 intros;unfold get_app,set_app;cbn[en_apps].
 rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;reflexivity.
Qed.
Lemma output_type_eqb:forall Fl,
 (forall a b,Fl a=Fl b->a=b)->forall a b,
 ety_ok a=true->ety_ok b=true->Audit.ty_eqb(tr Fl a)(tr Fl b)=ety_eqb a b.
Proof.
 intros Fl Inj a b Oa Ob;destruct(ety_eqb a b)eqn:E.
 - apply ety_eqb_true in E;subst b;apply CoreMoss.ty_eqb_eq;reflexivity.
 - destruct(Audit.ty_eqb(tr Fl a)(tr Fl b))eqn:T;[|reflexivity].
   apply CoreMoss.ty_eqb_eq in T;apply(@tr_inj Fl Inj a b Oa Ob)in T.
   subst b;rewrite ety_eqb_refl in E;discriminate E.
Qed.
Section MethodContext.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation WW:=(BodySoundCore.W w R Fl).
Lemma cxrel_method:forall c e F C r m sg raw app stamp,
 CxRel w R Fl c e F C->canon e r=r->ety_ok r=true->
 SigOK sg->SigSupport WW sg->Support WW r->
 app=map(fun q=>(fst q,option_map(tr Fl)(snd q)))raw->
 CxRel w R Fl c(set_app(set_mth e r m sg)(KyMth r m)raw stamp)F
 (A.provide C(A.mkey(tr Fl r)m)app(A.set_mth C(tr Fl r)m(output_sig Fl e sg))).
Proof.
 intros c e F C r m sg raw app stamp H Can Or Ws Ss Sr Eapp.
 pose proof(cr_mths _ _ _ _ _ _ _ H)as Look.
 pose proof(cr_mths_clean _ _ _ _ _ _ _ H)as Clean.
 pose proof(cr_apps _ _ _ _ _ _ _ H)as Apps.
 pose proof(cr_vapps _ _ _ _ _ _ _ H)as Va.
 pose proof(cr_fapps _ _ _ _ _ _ _ H)as Fa.
 pose proof(cr_funapps _ _ _ _ _ _ _ H)as Fu.
 pose proof(cr_funfixed _ _ _ _ _ _ _ H)as Fx.
 pose proof(cr_support _ _ _ _ _ _ _ H)as Es.
 pose proof(cr_mth_rows _ _ _ _ _ _ _ H)as Rows.
 pose proof(cr_mth_domain _ _ _ _ _ _ _ H)as Domain.
 assert(Urows:MethodRowsUnique e)by(intros mid rows Lookup;exact(proj1(Rows mid rows Lookup))).
 assert(Crows:MethodRowsCanonical e)by(intros mid rows Lookup;exact(proj2(Rows mid rows Lookup))).
 destruct(method_rows_set e r m sg Urows Crows Or Can)as[NewUnique NewCanonical].
 assert(Csame:forall t,canon(set_app(set_mth e r m sg)(KyMth r m)raw stamp)t=canon e t)by reflexivity.
 destruct H;constructor;auto.
 - intros q mq Oq;rewrite Csame in Oq |- *;cbn[A.provide A.set_mth A.c_mths A.mlook].
   rewrite get_mth_output_app,get_mth_set_case.
   rewrite(output_type_eqb Fl Finj _ _ Oq Or),andb_comm.
   destruct(Nat.eqb mq m&&ety_eqb(canon e q)r);cbn[option_map];
   [reflexivity|exact(Look q mq Oq)].
 - intros q mq sq E;rewrite get_mth_output_app,get_mth_set_case in E.
   destruct(Nat.eqb mq m&&ety_eqb q r);[injection E as <-;exact Ws|exact(Clean _ _ _ E)].
 - apply output_env_support_app;eapply env_support_method;eassumption.
 - intros q mq Oq;cbn[A.provide A.c_apps A.ilook].
   cbn[A.mkey Audit.item_eqb Audit.subst_eqb].
   rewrite(output_type_eqb Fl Finj _ _ Oq Or),andb_true_r,andb_comm.
   unfold get_app,Elab.set_app;cbn[en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec.
   fold(key_eqb(KyMth q mq)(KyMth r m)).
   destruct(key_eqb(KyMth q mq)(KyMth r m));cbn[option_map fst];
   [rewrite Eapp;reflexivity|exact(Apps q mq Oq)].
 - intro x;cbn[A.provide A.c_apps A.ilook A.mkey Audit.item_eqb].
   unfold get_app,Elab.set_app;cbn[en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;exact(Va x).
 - intro x;cbn[A.provide A.c_apps A.ilook A.mkey Audit.item_eqb].
   unfold get_app,Elab.set_app;cbn[en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;exact(Fa x).
 - intro u;cbn[A.provide A.c_apps A.ilook A.mkey Audit.item_eqb].
   rewrite get_app_method_fun;exact(Fu u).
 - intros u fx x tx Lookup Hin;cbn[A.provide A.set_mth A.c_funs]in Lookup.
   destruct(Fx u fx x tx Lookup Hin)as[Fixed(a&st0&ry&Ga&Gx&Eq)].
   split;[exact Fixed|];exists a,st0,ry;split;[rewrite get_app_method_fun;exact Ga|auto].
 - intros mid rows Lookup;split;[exact(NewUnique mid rows Lookup)|exact(NewCanonical mid rows Lookup)].
 - intros rt mid result Lookup;cbn[A.provide A.set_mth A.c_mths A.mlook]in Lookup.
   destruct(ty_eqb rt(tr Fl r)&&Nat.eqb mid m)eqn:Head.
   + apply andb_true_iff in Head as[Types Methods];apply CoreMoss.ty_eqb_eq in Types.
     exists r;split;[exact Or|];split;[exact Can|symmetry;exact Types].
   + destruct(Domain rt mid result Lookup)as[original[Good[Stable Reading]]].
     exists original;split;[exact Good|];split;[exact Stable|exact Reading].
Qed.
End MethodContext.

Definition output_with_types(C:A.cx)(tm:list(nat*ty)):A.cx:=
 A.Cx tm(A.c_sub C)(A.c_vals C)(A.c_fns C)(A.c_mths C)
  (A.c_funs C)(A.c_apps C)(A.c_locals C).
Section TypeTable.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma cxrel_output_types:forall c e F C tm,
 CxRel w R Fl c e F C->
 (forall t,A.nlook t tm=A.nlook t(A.c_ty C))->
 CxRel w R Fl c e F(output_with_types C tm).
Proof.
 intros c e F C tm H Eq;pose proof(cr_ty _ _ _ _ _ _ _ H)as Ty.
 destruct H;constructor;auto.
 intro t;cbn[output_with_types A.c_ty];rewrite Eq;exact(Ty t).
Qed.
Lemma output_context_canon_idem:forall c e F C,
 CxRel w R Fl c e F C->forall tau,A.canon C(A.canon C tau)=A.canon C tau.
Proof.
 intros c e F C H tau;unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ H).
 apply suf_idem;apply tri_rk;exact(proj1(cr_uf _ _ _ _ _ _ _ H)).
Qed.
Lemma output_context_type_stable:forall c e F C t tau,
 CxRel w R Fl c e F C->A.nlook t(A.c_ty C)=Some tau->A.canon C tau=tau.
Proof.
 intros c e F C t tau H E;rewrite(cr_ty _ _ _ _ _ _ _ H)in E.
 destruct(tn_get e t)as[y|];[injection E as <-|discriminate E].
 unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ H).
 rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ H)).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ H));reflexivity.
Qed.
End TypeTable.
