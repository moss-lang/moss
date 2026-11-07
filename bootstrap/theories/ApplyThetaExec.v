(* The emitted application input theta reads the actual caller type table. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr BodyTy CanonTypes
 CtxMerge FunctorContext FunctorFixedExec ApplyAbstractExec EtyTyExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[ety_ty ety_ty_go].
Section Theta.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma input_argument_rel:forall e t s p s',
 input_argument e t s=Good(p,s')->Inv s->Later w P R Fl s'->
 p=(t,tr Fl(canon e(opt_default YUnit(tn_get e t)))).
Proof.
 intros e t s p s' E Hi L;unfold input_argument in E.
 apply bind_inv in E as(y&sy&Ey&E);injection E as <- <-.
 rewrite(ety_ty_tr w P R Fl _ _ _ _ Ey Hi L);reflexivity.
Qed.
Lemma ok_input_argument:forall e t,ok(input_argument e t).
Proof. intros;unfold input_argument;apply ok_bind;[apply ok_ety_ty|intro;apply ok_ret]. Qed.
Lemma input_arguments_rel:forall e ts s th s',
 mmap(input_argument e)ts s=Good(th,s')->Inv s->st_w s=w->Later w P R Fl s'->
 Forall2(fun t p=>p=(t,tr Fl(canon e(opt_default YUnit(tn_get e t)))))ts th.
Proof.
 intros e ts s th s' E Hi Hw L.
 eapply(BodyTy.mmap_ok_rel w R Fl);[apply ok_input_argument| |exact E|exact Hi|exact Hw|exact L].
 intros t Ht si p sf Et Ii Wi Li;exact(input_argument_rel e t si p sf Et Ii Li).
Qed.
Lemma input_arguments_member:forall e ts th,
 Forall2(fun t p=>p=(t,tr Fl(canon e(opt_default YUnit(tn_get e t)))))ts th->
 forall t,In t ts->In(t,tr Fl(canon e(opt_default YUnit(tn_get e t))))th.
Proof.
 intros e ts th H;induction H;intros t Hin;[contradiction|].
 destruct Hin as[<-|Hin];[left;exact H|right;auto].
Qed.
Theorem application_theta_reads:forall c e F C ef Cf mi fg sy sz sg th st,
 CxRel w R Fl c e F C->FunEnvTables Fl ef Cf->
 miter(input_guard mi e fg)(map fst(en_tn ef))sy=Good(tt,sz)->
 mmap(input_argument e)(map fst(en_tn ef))sg=Good(th,st)->
 Inv sg->st_w sg=w->Later w P R Fl st->
 forall t tt,A.nlook t(A.c_ty Cf)=Some tt->exists here,
 A.nlook t(A.c_ty C)=Some here/\In(t,A.canon C here)th.
Proof.
 intros c e F C ef Cf mi fg sy sz sg th st Hc Tables Guard Theta Hi Hw L t tt Present.
 pose proof(input_arguments_rel e _ _ _ _ Theta Hi Hw L)as Rel.
 rewrite(fet_ty _ _ _ Tables)in Present.
 destruct(tn_get ef t)as[y|]eqn:Y;[|discriminate].
 pose proof(tn_get_key ef t y Y)as Key.
 pose proof(input_guard_checked _ _ _ _ _ _ _ _ Guard Key)as Have.
 destruct(tn_get e t)as[now|]eqn:Now;[|contradiction].
 exists(tr Fl(canon e now));split.
 - rewrite(cr_ty _ _ _ _ _ _ _ Hc),Now;reflexivity.
 - unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ Hc),tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
   rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
   pose proof(input_arguments_member e _ _ Rel t Key)as Member;rewrite Now in Member;exact Member.
Qed.
End Theta.
