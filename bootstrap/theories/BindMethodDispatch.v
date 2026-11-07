(* The residual callback excludes all anonymous method binds, with or without brackets. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySoundCore InfoPreserve
 BindSequence BindCopyDispatch BindFnDispatch BindLambdaDispatch BindMethodLambdaExec.
Import ListNotations.
Definition method_lambda_bind (sp : spec) (y : aexpr) : bool :=
 match sp,y with
 | Spec (Some _) (Some _) _,AELam _ _ _ => true
 | _,_ => false
 end.
Section Dispatch.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Hypothesis Finj : forall a b, Fl a = Fl b -> a = b.
Notation W := (BodySoundCore.W w R Fl).
Definition MethodRemainingBindStep (f : nat) : Prop := forall c e sp y s xs e' c' final,
 check_bind f c e sp y s = Good ((xs,e',c'),final) -> forall F C,
 simple_bind W F sp y = 0 -> abstract_copy_bind W F sp y = false ->
 named_function_bind W F sp y = false -> function_lambda_bind W F sp y = false ->
 method_lambda_bind sp y = false ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl final F ->
 BindResult w P R Fl c e F C (sp,Some y) xs e' c'.
Theorem residual_after_method_lambda : forall fuel,
 OK w P R Fl fuel -> MethodRemainingBindStep (S fuel) -> LambdaRemainingBindStep w P R Fl (S fuel).
Proof.
 intros fuel IH Other c e sp y s xs e' c' final Run F C Simple Copy Named FnClass Valid Cache Forward Caller Last.
 destruct (method_lambda_bind sp y) eqn:Class.
 - destruct sp as [[pa|] [dn|] a]; cbn [method_lambda_bind] in Class; try discriminate Class.
   destruct y; cbn [method_lambda_bind] in Class; try discriminate Class.
   eapply (@check_bind_method_lambda w P R Fl b Load Finj);
     [exact IH|exact Run|exact Valid|exact Cache|exact Forward|exact Caller|exact Last].
 - exact (Other _ _ _ _ _ _ _ _ _ Run F C Simple Copy Named FnClass Class Valid Cache Forward Caller Last).
Qed.
End Dispatch.
