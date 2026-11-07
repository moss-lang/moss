(* A lambda begins with all enclosing provisions and no enclosing locals. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySoundCore NominalSupport FnSnapshot.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition lambda_clear_context(C:A.cx):A.cx:=
 A.Cx(A.c_ty C)(A.c_sub C)(A.c_vals C)(A.c_fns C)(A.c_mths C)(A.c_funs C)(A.c_apps C)[].
Section Capture.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Lemma cxrel_capture_base:forall c e F C,
 CxRel w R Fl c e F C->CxRel w R Fl c(with_locals e[])F(lambda_clear_context C).
Proof.
 intros c e F C H;pose proof(cr_support _ _ _ _ _ _ _ H)as Support.
 destruct H;constructor;auto.
 - intros n p E;cbn[with_locals en_locals]in E;discriminate E.
 - destruct Support;constructor;auto;intros n p E;cbn[with_locals en_locals]in E;discriminate E.
 - constructor.
Qed.
End Capture.
