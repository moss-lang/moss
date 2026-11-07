(* Type declarations and already-defined outputs add no runtime slot. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 CtxWalk CtxMerge NominalSupport OutputScopeExec ProvideResultFold FamilyResultFold.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Lemma already_provided_shape:forall g,already_provided g=true->
 gs_kind g=KUnit\/gs_kind g=KTag\/gs_kind g=KAlias\/
 (gs_kind g=KVal/\exists n t body,gs_decl g=ADVal n t(Some body))\/
 (gs_kind g=KFn/\exists r n ps rt body,gs_decl g=ADFn r n ps rt(Some body)).
Proof.
 intros g H;unfold already_provided in H;destruct(gs_kind g)eqn:K;try discriminate H;try tauto.
 - destruct(gs_decl g)eqn:D;try discriminate H.
   repeat match type of H with context[match ?o with _=>_ end]=>destruct o eqn:? end;
   try discriminate H.
   right;right;right;left;split;[reflexivity|eexists;eexists;eexists;reflexivity].
 - unfold fn_body in H;destruct(gs_decl g)eqn:D;try discriminate H.
   repeat match type of H with context[is_some ?o]=>destruct o eqn:?;cbn[is_some]in H end;
   try discriminate H.
   right;right;right;right;split;[reflexivity|eexists;eexists;eexists;eexists;eexists;reflexivity].
Qed.
Section Leaves.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem result_type_leaf:forall c e F C mi pa a outer ob out s sub sx g,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sx)->
 A.resolve_sym W(gm_path(get_mod w mi))pa=Some(gs_id g)->
 nget(w_syms w)(gs_id g)=Some g->gs_kind g=KType->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sx->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,
 A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)None a)es(A.c_ty C)/\
 out=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 CxRel w R Fl c e F(A.output_cx C(A.c_ty C)es).
Proof.
 intros c e F C mi pa a outer ob out s sub sx g Sub Res Hg K Hi Hw Hmi L HC Stable Wo So O.
 destruct(output_scope_execution w R Fl b Hload c e F C mi a outer ob s sub sx
 Sub Hi Hw Hmi L HC Wo So O)as(bs&Br&Rest).
 exists[A.NTy(gs_id g)],[];split.
 - eapply A.OIEType;[exact Res| |exact Br].
   unfold A.kind_sym;rewrite(w_decl_rd w R Fl _ _ Hg);cbn[A.d_ast].
   pose proof(load_world_kind b w Hload _ _ Hg)as Kind;unfold KindOK in Kind;rewrite K in Kind;exact Kind.
 - split;[now rewrite app_nil_r|];split;[reflexivity|].
   rewrite output_cx_type,family_empty_self by exact Stable;exact HC.
Qed.
Theorem result_defined_leaf:forall c e F C mi pa a outer ob out s sub sx g,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sx)->
 A.resolve_sym W(gm_path(get_mod w mi))pa=Some(gs_id g)->
 nget(w_syms w)(gs_id g)=Some g->already_provided g=true->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sx->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,
 A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)None a)es(A.c_ty C)/\
 out=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 CxRel w R Fl c e F(A.output_cx C(A.c_ty C)es).
Proof.
 intros c e F C mi pa a outer ob out s sub sx g Sub Res Hg K Hi Hw Hmi L HC Stable Wo So O.
 destruct(output_scope_execution w R Fl b Hload c e F C mi a outer ob s sub sx
 Sub Hi Hw Hmi L HC Wo So O)as(bs&Br&Rest).
 exists[],[];split.
 - eapply A.OIENothing with(s:=gs_id g)(k:=kconv(gs_kind g))(bs:=bs).
   + exact Res.
   + unfold A.kind_sym;rewrite(w_decl_rd w R Fl _ _ Hg);cbn[A.d_ast].
     exact(load_world_kind b w Hload _ _ Hg).
   + exact Br.
   + destruct(already_provided_shape g K)as[U|[T|[Al|[[V(n&t&i&D)]|[Fn(r&n&ps&rt&body&D)]]]]].
     * left;rewrite U;reflexivity.
     * right;left;rewrite T;reflexivity.
     * right;right;left;rewrite Al;reflexivity.
     * right;right;right;left;split;[rewrite V;reflexivity|].
       exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),n,t,i.
       split;[exact(w_decl_rd w R Fl _ _ Hg)|exact D].
     * right;right;right;right;split;[rewrite Fn;reflexivity|].
       exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),r,n,ps,rt,body.
       split;[exact(w_decl_rd w R Fl _ _ Hg)|exact D].
 - split;[now rewrite app_nil_r|];split;[reflexivity|].
   rewrite family_empty_self by exact Stable;exact HC.
Qed.
End Leaves.
