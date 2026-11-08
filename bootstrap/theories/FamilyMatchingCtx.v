(* Temporary inferred type readings installed by an actual family application. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodySoundCore BodyTr NatAssoc BindType
 NominalSupport NominalMergeSupport CanonTypes.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition family_install_step(e:env)(q:nat*ety):M env:=
 let! stamp:=fresh in ret(set_tn e(fst q)(snd q)stamp).
Definition family_matching(Fl:string->nat)(e:env)(th:list(nat*ety)):=
 map(fun q=>(fst q,tr Fl(canon e(snd q))))th.
Definition family_restore_table {V}(old current:list(nat*V))(ps:list nat):list(nat*V):=
 fold_left(fun tn p=>match nassoc p old with
 |Some v=>aset Nat.eqb p v tn
 |None=>filter(fun q=>negb(Nat.eqb(fst q)p))tn end)ps current.
Lemma family_nassoc_remove:forall V x p (l:list(nat*V)),
 nassoc x(filter(fun q=>negb(Nat.eqb(fst q)p))l)=
 if Nat.eqb x p then None else nassoc x l.
Proof.
 intros V x p l;induction l as[|[k v]rest IH];cbn[filter fst nassoc assoc].
 - destruct(Nat.eqb x p);reflexivity.
 - destruct(Nat.eqb k p)eqn:K;cbn[negb].
   + apply Nat.eqb_eq in K;subst k;rewrite IH.
     destruct(Nat.eqb x p);reflexivity.
   + cbn[nassoc assoc];destruct(Nat.eqb x k)eqn:X.
     * apply Nat.eqb_eq in X;subst x;rewrite K;reflexivity.
     * rewrite IH;destruct(Nat.eqb x p);reflexivity.
Qed.
Lemma family_restore_table_lookup:forall V old ps (current:list(nat*V)) x,
 nassoc x(family_restore_table old current ps)=
 if nmem x ps then nassoc x old else nassoc x current.
Proof.
 intros V old ps;induction ps as[|p ps IH];intros current x.
 - reflexivity.
 - change(nassoc x(family_restore_table old
    (match nassoc p old with Some v=>aset Nat.eqb p v current
     |None=>filter(fun q=>negb(Nat.eqb(fst q)p))current end)ps)=
    if nmem x(p::ps)then nassoc x old else nassoc x current).
   rewrite IH;cbn[nmem];destruct(nmem x ps)eqn:Mem.
   + destruct(Nat.eqb x p);reflexivity.
   + destruct(Nat.eqb_spec x p)as[->|Ne].
     * destruct(nassoc p old)eqn:E;
       [rewrite nassoc_aset,Nat.eqb_refl|rewrite family_nassoc_remove,Nat.eqb_refl];reflexivity.
     * assert(Eneq:Nat.eqb x p=false)by(apply Nat.eqb_neq;exact Ne).
       destruct(nassoc p old);[rewrite nassoc_aset,Eneq|rewrite family_nassoc_remove,Eneq];reflexivity.
Qed.
Lemma family_restore_table_members:forall V old ps (current:list(nat*V)) q,
 In q(family_restore_table old current ps)->In q old\/In q current.
Proof.
 intros V old ps;induction ps as[|p ps IH];intros current q H.
 - right;exact H.
 - change(In q(family_restore_table old
    (match nassoc p old with Some v=>aset Nat.eqb p v current
     |None=>filter(fun q=>negb(Nat.eqb(fst q)p))current end)ps))in H.
   destruct(IH _ _ H)as[Ho|Hc];[left;exact Ho|].
   destruct(nassoc p old)eqn:E.
   + apply support_in_aset in Hc as[->|Hc];[left;eapply support_nassoc_in;exact E|right;exact Hc].
   + right;apply filter_In in Hc;exact(proj1 Hc).
Qed.
Lemma family_install_uf:forall th e s e' sf,
 mfold family_install_step e th s=Good(e',sf)->en_uf e'=en_uf e.
Proof.
 intros th;induction th as[|[t ty]rest IH];intros e s e' sf E.
 - injection E as <- <-;reflexivity.
 - cbn[mfold]in E;apply bind_inv in E as(en&sn&Step&Tail).
   unfold family_install_step in Step;cbn[fst snd]in Step.
   apply bind_inv in Step as(stamp&st&Fresh&Ret);injection Ret as <- <-.
   exact(IH _ _ _ _ Tail).
Qed.
Section Matching.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySoundCore.W w R Fl).
Lemma family_matching_install_at:forall th c e0 e F C s e' sf,
 mfold family_install_step e th s=Good(e',sf)->
 en_uf e=en_uf e0->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))th->Forall(fun q=>Support W(snd q))th->
 en_uf e'=en_uf e0 /\
 CxRel w R Fl c e' F
 (fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e0 th)C).
Proof.
 intros th;induction th as[|[t ty]rest IH];intros c e0 e F C s e' sf E Uf HC Wf Sp.
 - injection E as <- <-;split;assumption.
 - cbn[mfold]in E;apply bind_inv in E as(en&sn&Step&Tail).
   unfold family_install_step in Step;cbn[fst snd]in Step.
   apply bind_inv in Step as(stamp&st&Fresh&Ret);injection Ret as <- <-.
   inversion Wf as[|? ? Wt Wr];subst;inversion Sp as[|? ? St Sr];subst.
   cbn[family_matching map fst snd fold_left].
   assert(Can:canon e ty=canon e0 ty)by(apply canon_uf;exact Uf).
   rewrite <-Can.
   eapply IH;[exact Tail|exact Uf|eapply cxrel_type;[exact HC|exact Wt|exact St]|exact Wr|exact Sr].
Qed.
Theorem family_matching_install:forall th c e F C s e' sf,
 mfold family_install_step e th s=Good(e',sf)->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))th->Forall(fun q=>Support W(snd q))th->
 en_uf e'=en_uf e /\
 CxRel w R Fl c e' F
 (fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e th)C).
Proof. intros;eapply family_matching_install_at;eauto. Qed.
Lemma family_install_outside:forall th e s e' sf x,
 mfold family_install_step e th s=Good(e',sf)->~In x(map fst th)->
 nassoc x(en_tn e')=nassoc x(en_tn e).
Proof.
 intros th;induction th as[|[t ty]rest IH];intros e s e' sf x E N.
 - injection E as <- <-;reflexivity.
 - cbn[mfold]in E;apply bind_inv in E as(en&sn&Step&Tail).
   unfold family_install_step in Step;cbn[fst snd]in Step.
   apply bind_inv in Step as(stamp&st&Fresh&Ret);injection Ret as <- <-.
   rewrite(IH _ _ _ _ _ Tail ltac:(intro H;apply N;right;exact H)).
   cbn[set_tn en_tn];rewrite nassoc_aset.
   destruct(Nat.eqb_spec x t)as[->|Ne];[exfalso;apply N;left;reflexivity|reflexivity].
Qed.
Lemma family_restored_lookup:forall e er ps,
 (forall x,~In x ps->nassoc x(en_tn er)=nassoc x(en_tn e))->
 forall x,nassoc x(en_tn(restore_tn er e ps))=nassoc x(en_tn e).
Proof.
 intros e er ps Out x;change(nassoc x(family_restore_table(en_tn e)(en_tn er)ps)=nassoc x(en_tn e)).
 rewrite family_restore_table_lookup;destruct(nmem x ps)eqn:M;[reflexivity|].
 apply Out;intro H;assert(nmem x ps=true)by(apply nmem_in;exact H);congruence.
Qed.
Lemma family_restored_support:forall e er ps,
 EnvSupport W e->EnvSupport W er->EnvSupport W(restore_tn er e ps).
Proof.
 intros e er ps He Hr;destruct Hr;constructor;auto.
 intros k p H;change(In(k,p)(family_restore_table(en_tn e)(en_tn er)ps))in H.
 apply family_restore_table_members in H as[H|H].
 - exact(@NominalSupport.es_tn W e He k p H).
 - exact(es_tn k p H).
Qed.
Theorem family_restore_cxrel:forall c e er F C Cr ps,
 CxRel w R Fl c e F C->CxRel w R Fl c er F Cr->en_uf er=en_uf e->
 (forall x,~In x ps->nassoc x(en_tn er)=nassoc x(en_tn e))->
 CxRel w R Fl c(restore_tn er e ps)F(A.family_restore C Cr).
Proof.
 intros c e er F C Cr ps HC HR Uf Out.
 pose proof(family_restored_lookup e er ps Out)as Look.
 pose proof(cr_ty _ _ _ _ _ _ _ HC)as Ty.
 pose proof(cr_ty_wf _ _ _ _ _ _ _ HC)as WTy.
 pose proof(family_restored_support e er ps(cr_support _ _ _ _ _ _ _ HC)(cr_support _ _ _ _ _ _ _ HR))as ES.
 destruct HR;constructor;auto.
 - intro t;cbn[A.family_restore A.c_ty];rewrite Ty.
   unfold tn_get;rewrite Look;destruct(nassoc t(en_tn e))as[[y st]|];cbn;[|reflexivity].
   rewrite(canon_uf e er Uf);reflexivity.
 - intros t y H;apply(WTy t y);unfold tn_get in *;rewrite Look in H;exact H.
Qed.
End Matching.
