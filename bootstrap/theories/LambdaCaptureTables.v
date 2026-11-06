(* Captured value provisions and captured-local views update separate tables. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySound LambdaCaptureFold.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition capture_local(Fl:string->nat)(original:env)(C:A.cx)(q:string*nat*ety):A.cx:=
 let '(n,_,t) :=q in A.add_local C n(tr Fl(canon original t))A.LCaptured.
Definition capture_full(Fl:string->nat)(original:env)(C:A.cx)(q:string*nat*ety):A.cx:=
 capture_local Fl original(capture_model Fl original C q)q.
Lemma capture_value_local:forall Fl original C q r,
 capture_model Fl original(capture_local Fl original C q)r=
 capture_local Fl original(capture_model Fl original C r)q.
Proof. intros Fl original C[[n v]t][[rn rv]rt];destruct C;reflexivity. Qed.
Lemma capture_values_local:forall Fl original rows C q,
 fold_left(capture_model Fl original)rows(capture_local Fl original C q)=
 capture_local Fl original(fold_left(capture_model Fl original)rows C)q.
Proof.
 intros Fl original rows;induction rows as[|r rows IH];intros C q;[reflexivity|].
 cbn[fold_left];rewrite capture_value_local;apply IH.
Qed.
Theorem capture_tables_factor:forall Fl original rows C,
 fold_left(capture_full Fl original)rows C=
 fold_left(capture_local Fl original)rows(fold_left(capture_model Fl original)rows C).
Proof.
 intros Fl original rows;induction rows as[|q rows IH];intro C;[reflexivity|].
 cbn[fold_left capture_full];rewrite IH;unfold capture_full;rewrite capture_values_local;reflexivity.
Qed.
Definition capture_context_locals(C:A.cx)(locals:list(string*(ty*A.lmut))):A.cx:=
 A.Cx(A.c_ty C)(A.c_sub C)(A.c_vals C)(A.c_fns C)(A.c_mths C)(A.c_funs C)(A.c_apps C)locals.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Theorem cxrel_capture_locals:forall c e F C locals,
 CxRel w R Fl c e F C->
 (forall n,A.slook n locals=A.slook n(A.c_locals C))->
 CxRel w R Fl c e F(capture_context_locals C locals).
Proof.
 intros c e F C locals H Eq;pose proof(cr_locals _ _ _ _ _ _ _ H)as Look.
 destruct H;constructor;auto.
 intro n;cbn[capture_context_locals A.c_locals];rewrite Eq;exact(Look n).
Qed.
End Context.
