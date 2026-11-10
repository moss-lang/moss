(* The principal reading of a callee's requirements (Elab.principal_env)
   is a context the call is stated at (ExecutionModel.PrincipalCx). *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodySoundCore BodyTr Merge CanonTypes
 InstantiateTr NominalSupport BindType.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Principal.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Definition principal_sem(e:env)(info:fninfo):list(nat*ty):=
 map(fun q=>(fst q,tr Fl(fst(snd q))))(principal_args e info).
Definition principal_cx(e:env)(info:fninfo)(C:A.cx):A.cx:=
 fold_left(fun C q=>A.set_ty C(fst q)(snd q))(principal_sem e info)C.
Lemma principal_args_spec:forall e info q,In q(principal_args e info)->
 tn_get e(fst q)=None/\exists y y0,tn_get e y=Some y0/\fst(snd q)=canon e(instantiate e(YAbs y)).
Proof.
 intros e info q Hq. unfold principal_args in Hq. apply in_flat_map in Hq as[r[_ Hq]].
 destruct(tn_get e r)eqn:Er;[contradiction|]. cbn[is_some]in Hq.
 destruct(find _ _)as[y|]eqn:Fy;[|contradiction].
 apply find_some in Fy as[_ Fy]. apply andb_true_iff in Fy as[Iy _].
 destruct(nassoc y(en_tn e))as[[y1 st]|]eqn:Ny;[|contradiction].
 destruct Hq as[<-|[]];cbn[fst snd]. split;[exact Er|].
 destruct(tn_get e y)as[y0|]eqn:Ey;[|discriminate Iy]. exists y,y0;split;[exact Ey|reflexivity].
Qed.
Lemma principal_fold_context:forall l c e F C,
 CxRel w R Fl c e F C->
 Forall(fun q=>WFty(fst(snd q))/\Support WW(fst(snd q))/\canon e(fst(snd q))=fst(snd q))l->
 en_uf(fold_left(fun en q=>set_tn en(fst q)(fst(snd q))(snd(snd q)))l e)=en_uf e/\
 CxRel w R Fl c(fold_left(fun en q=>set_tn en(fst q)(fst(snd q))(snd(snd q)))l e)F
  (fold_left(fun C q=>A.set_ty C(fst q)(snd q))(map(fun q=>(fst q,tr Fl(fst(snd q))))l)C).
Proof.
 induction l as[|[k[y st]]rest IH];intros c e F C Hc Props;[split;[reflexivity|exact Hc]|].
 inversion Props as[|p ps[Wy[Sy Cy]]Rest];subst. cbn[fold_left map fst snd]in *.
 pose proof(cxrel_type w R Fl c e F C k y st Hc Wy Sy)as C1. rewrite Cy in C1.
 assert(Rest':Forall(fun q=>WFty(fst(snd q))/\Support WW(fst(snd q))/\canon(set_tn e k y st)(fst(snd q))=fst(snd q))rest).
 { eapply Forall_impl;[|exact Rest]. intros q[A1[A2 A3]];split;[exact A1|split;[exact A2|]].
   rewrite(canon_uf e(set_tn e k y st)eq_refl);exact A3. }
 destruct(IH c(set_tn e k y st)F(A.set_ty C k(tr Fl y))C1 Rest')as[U HC].
 split;[rewrite U;reflexivity|exact HC].
Qed.
Lemma principal_env_context:forall c e F C info Cs s,CxRel w R Fl c e F C->
 en_uf(principal_env e info)=en_uf e/\
 CxRel w R Fl c(principal_env e info)F(principal_cx e info C)/\
 A.PrincipalCx WW C Cs s(principal_cx e info C).
Proof.
 intros c e F C info Cs s Hc.
 pose proof(cr_uf _ _ _ _ _ _ _ Hc)as[Tr Uw].
 assert(Props:Forall(fun q=>WFty(fst(snd q))/\Support WW(fst(snd q))/\canon e(fst(snd q))=fst(snd q))(principal_args e info)).
 { apply Forall_forall;intros q Hq. destruct(principal_args_spec e info q Hq)as[_[y[y0[Ey ->]]]].
   split;[|split].
   - apply canon_wf;[exact Uw|]. cbn[instantiate];rewrite Ey.
     apply canon_wf;[exact Uw|exact(cr_ty_wf _ _ _ _ _ _ _ Hc y y0 Ey)].
   - apply canon_support;[exact(es_uf(cr_support _ _ _ _ _ _ _ Hc))|].
     apply instantiate_support;[exact(cr_support _ _ _ _ _ _ _ Hc)|constructor].
   - exact(canon_idem e _ Tr). }
 destruct(principal_fold_context _ c e F C Hc Props)as[U HC].
 split;[exact U|]. split;[exact HC|].
 exists(principal_sem e info). split;[reflexivity|].
 intros r t Hin. unfold principal_sem in Hin. apply in_map_iff in Hin as[q[Eq Hq]]. injection Eq as <- <-.
 destruct(principal_args_spec e info q Hq)as[Er[y[y0[Ey Ev]]]].
 split;[rewrite(cr_ty _ _ _ _ _ _ _ Hc),Er;reflexivity|].
 exists y;split;[rewrite(cr_ty _ _ _ _ _ _ _ Hc),Ey;discriminate|].
 rewrite Ev. exact(instantiate_canonical w R Fl c e F C Hc(YAbs y)).
Qed.
End Principal.

