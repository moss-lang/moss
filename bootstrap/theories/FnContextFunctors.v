(* The input context's own fixed readings supply the functor fields needed
   when checking a function body, including exact frozen application rows. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodySoundCore BodyTr CtxWalk CtxMerge
  ContextFixed FunctorDecl BindFunctorCtx.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Theorem fn_context_functor_captures:forall f e C ns,
 A.CtxOf WW f C ns ->
 (forall k,nofun k=true->key_ok k=true->
   A.ilook(key_tr Fl k)(A.c_apps C)=option_map(fun p=>tr_inst Fl(fst p))(get_app e k))->
 forall u fx,A.nlook u(A.c_funs C)=Some fx->
 FunctorDecl WW u /\forall x tx,In(x,tx)fx->
 A.Fixed WW u x /\exists a stamp raw,
 get_app e(KyFun u)=Some(a,stamp)/\nassoc x a=Some(Some raw)/\tx=tr Fl raw.
Proof.
 intros f e C ns Ctx Apps u fx Lookup.
 split;[eapply ctxof_functor_decls;eassumption|].
 intros x tx Hin.
 destruct(ctxof_fixed_snapshot_functional WW f C ns Ctx u fx Lookup x tx Hin)as[Fixed AtFx].
 split;[exact Fixed|].
 pose proof(Apps(KyFun u)eq_refl eq_refl)as App.
 change(A.ilook(IFun u)(A.c_apps C)=option_map(fun p=>tr_inst Fl(fst p))(get_app e(KyFun u)))in App.
 rewrite(ctxof_functor_app_exact WW f C ns Ctx u fx Lookup)in App.
 destruct(get_app e(KyFun u))as[[a stamp]|]eqn:Recorded;[|discriminate].
 cbn[option_map fst]in App;injection App as AppEq.
 assert(Map:fun_tr_inst Fl a=A.fixed_app fx)by(symmetry;exact AppEq).
 pose proof(fixed_app_lookup fx x)as At;rewrite AtFx in At.
 rewrite<-Map,fun_inst_lookup in At.
 destruct(nassoc x a)as[[raw|]|]eqn:Read;cbn[option_map]in At;try discriminate.
 injection At as TrEq;exists a,stamp,raw;split;[reflexivity|];split;[exact Read|symmetry;exact TrEq].
Qed.
End Context.
