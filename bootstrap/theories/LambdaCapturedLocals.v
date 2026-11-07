(* The actual captured rows supply the model's captured-local context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySoundCore NominalSupport
 LambdaCaptureFold LambdaCaptureTables.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition captured_row_available(original:env)(q:string*nat*ety):Prop:=
 let '(n,_,t) :=q in exists mu,sassoc n(en_locals original)=Some(t,mu).
Lemma capture_rows_available:forall qs added original,
 Forall2 capture_row qs added->
 Forall(fun q=>sassoc(fst q)(en_locals original)=Some(snd q))qs->
 Forall(captured_row_available original)added.
Proof.
 intros qs added original Rel;induction Rel as[|[n[t mu]][[rn v]rt]qs added Pair Rel IH];intro Rows.
 - constructor.
 - inversion Rows as[|q rest Lookup Rest];subst;cbn[capture_row]in Pair.
   destruct Pair as[<- <-];constructor;[exists mu;exact Lookup|apply IH;exact Rest].
Qed.
Definition lp_capture_one(original:A.cx)(C:A.cx)(q:string*nat):A.cx:=
 let '(n,v) :=q in match A.slook n(A.c_locals original)with
 |Some(t,_)=>A.add_local(A.provide C(IVal v)[](A.set_val C v t))n t A.LCaptured
 |None=>C end.
Lemma capture_combine:forall rows,
 combine(map capture_name rows)(map capture_id rows)=map(fun q=>let '(n,v,_) :=q in(n,v))rows.
Proof. induction rows as[|[[n v]t]rows IH];[reflexivity|].
 change((n,v)::combine(map capture_name rows)(map capture_id rows)=
 (n,v)::map(fun q=>let '(n,v,_) :=q in(n,v))rows);f_equal;exact IH. Qed.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySoundCore.W w R Fl).
Lemma capture_full_reading:forall c original F Co rows C,
 CxRel w R Fl c original F Co->Forall(captured_row_available original)rows->
 fold_left(lp_capture_one Co)(combine(map capture_name rows)(map capture_id rows))C=
 fold_left(capture_full Fl original)rows C.
Proof.
 intros c original F Co rows C HC Rows;rewrite capture_combine.
 revert C;induction Rows as[|[[n v]t]rows Available Rows IH];intro C;[reflexivity|].
 destruct Available as[mu Lookup].
 cbn[map fold_left lp_capture_one].
 rewrite(cr_locals _ _ _ _ _ _ _ HC),Lookup;cbn[option_map fst snd].
 apply IH.
Qed.
Theorem captured_local_context:forall rows c original F Co le C,
 CxRel w R Fl c original F Co->CxRel w R Fl c le F C->en_uf le=en_uf original->
 Forall(captured_row_available original)rows->
 CxRel w R Fl c
 (fold_left(fun en q=>let '(n,_,t) :=q in set_local en n t LCaptured)rows le)F
 (fold_left(capture_local Fl original)rows C).
Proof.
 intro rows;induction rows as[|[[n v]t]rows IH];intros c original F Co le C Horig HC Uf Rows.
 - exact HC.
 - inversion Rows as[|q rest Available Tail];subst;destruct Available as[mu Lookup].
   cbn[fold_left capture_local].
   assert(Step:CxRel w R Fl c(set_local le n t LCaptured)F
    (A.add_local C n(tr Fl(canon original t))A.LCaptured)).
   {assert(E:canon le t=canon original t)by(unfold canon;now rewrite Uf).
    rewrite <-E;apply cxrel_local;[exact HC|exact(cr_locals_wf _ _ _ _ _ _ _ Horig n(t,mu)Lookup)|exact(@es_locals W original(cr_support _ _ _ _ _ _ _ Horig)n(t,mu)Lookup)]. }
   eapply IH;[exact Horig|exact Step|exact Uf|exact Tail].
Qed.
End Context.
