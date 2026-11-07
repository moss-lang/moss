(* The ordinary snapshot facts also hold for a functor's own input context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodySoundCore BodyTr BodyTy FnSnapshot
 ContextFnSnapshots ContextFixed FunctorDecl BindFunctorCtx CtxWalk FnContextFunctors KeyTranslation.
Import ListNotations.
Module A:=Audit.Elaboration.

Lemma functx_fn_snapshots : forall W u C ns,
 A.FunCtx W u C ns -> FnSnapshots W C.
Proof.
 intros W u C ns(d&ps&a&r&body&es&tm&tm'&sigma&implicit&Decl&Parts&Items&Implicit&Types&Merge&Eq&Needs)f sg Lookup.
 rewrite Eq in Lookup|-*.
 destruct(entries_fn_row _ _ _ _ _ Lookup)as[raw[app[Row[Sig App]]]].
 destruct(items_fn_readings _ _ _ _ _ _ _ Items f raw app Row)as[rawtm[Signature Application]].
 destruct(frozen_fn_reading W sigma tm' f raw app rawtm Signature Application)as[snapshot[SG AP]].
 exists snapshot,(snapshot_inst sigma tm' app);rewrite Sig;split;[exact SG|];split;assumption.
Qed.

Lemma functx_fixed_snapshot_functional : forall W u C ns,
 A.FunCtx W u C ns -> forall f fx,A.nlook f(A.c_funs C)=Some fx ->
 forall x tx,In(x,tx)fx -> A.Fixed W f x /\ A.nlook x fx=Some tx.
Proof.
 intros W u C ns(d&ps&a&r&body&es&tm&tm'&sigma&implicit&Decl&Parts&Items&Implicit&Types&Merge&Eq&Needs)f fx Lookup x tx Hin.
 rewrite Eq in Lookup;destruct(entries_fixed_row _ _ _ _ _ Lookup)as[raw[InRow ->]].
 destruct(items_fixed_readings _ _ _ _ _ _ _ Items f raw InRow)as[fixedtm Fixed].
 destruct(fixed_subst_reading_member W f fixedtm raw sigma x tx Fixed Hin)as[Domain Cons].
 split;[exact Domain|apply nlook_consistent_member;assumption].
Qed.

Lemma functx_functor_app_exact : forall W u C ns,
 A.FunCtx W u C ns -> forall f fx,A.nlook f(A.c_funs C)=Some fx ->
 A.ilook(IFun f)(A.c_apps C)=Some(A.fixed_app fx).
Proof.
 intros W u C ns(d&ps&a&r&body&es&tm&tm'&sigma&implicit&Decl&Parts&Items&Implicit&Types&Merge&Eq&Needs)f fx Lookup.
 rewrite Eq in Lookup|-*;apply entries_functor_app_exact;exact Lookup.
Qed.

Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySoundCore.W w R Fl).
Lemma functx_functor_captures : forall u e C ns,
 A.FunCtx W u C ns ->
 (forall k,nofun k=true->key_ok k=true->
   A.ilook(key_tr Fl k)(A.c_apps C)=option_map(fun p=>tr_inst Fl(fst p))(get_app e k)) ->
 forall f fx,A.nlook f(A.c_funs C)=Some fx -> forall x tx,In(x,tx)fx ->
 A.Fixed W f x /\ exists a stamp raw,get_app e(KyFun f)=Some(a,stamp) /\
 nassoc x a=Some(Some raw) /\ tx=tr Fl raw.
Proof.
 intros u e C ns Ctx Apps f fx Lookup x tx Hin.
 destruct(functx_fixed_snapshot_functional W u C ns Ctx f fx Lookup x tx Hin)as[Fixed AtFx].
 split;[exact Fixed|].
 pose proof(Apps(KyFun f)eq_refl eq_refl)as App.
 change(A.ilook(IFun f)(A.c_apps C)=option_map(fun p=>tr_inst Fl(fst p))(get_app e(KyFun f)))in App.
 rewrite(functx_functor_app_exact W u C ns Ctx f fx Lookup)in App.
 destruct(get_app e(KyFun f))as[[a stamp]|]eqn:Recorded;[|discriminate].
 cbn[option_map fst]in App;injection App as AppEq.
 assert(Map:fun_tr_inst Fl a=A.fixed_app fx)by(symmetry;exact AppEq).
 pose proof(fixed_app_lookup fx x)as At;rewrite AtFx in At.
 rewrite<-Map,fun_inst_lookup in At.
 destruct(nassoc x a)as[[raw|]|]eqn:Read;cbn[option_map]in At;try discriminate.
 injection At as TrEq;exists a,stamp,raw;split;[reflexivity|];split;[exact Read|symmetry;exact TrEq].
Qed.
End Context.
