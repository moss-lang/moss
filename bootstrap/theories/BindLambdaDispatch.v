(* The remaining binding callback excludes the now-proved fn-lambda branch. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySoundCore BindSequence
 BindType BindCopyDispatch BindFnDispatch BindFnLambdaExec InfoPreserve.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition function_lambda_bind(W:A.world)(F:A.frame)(sp:spec)(y:aexpr):bool:=
 match sp,y with
 |Spec(Some pp)None None,AELam _ _ _=>
  match A.resolve_sym W(A.fr_mod F)pp with
  |Some f=>match A.kind_sym W f with Some A.DKFn=>true|_=>false end
  |None=>false end
 |_,_=>false end.
Section Dispatch.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Definition LambdaRemainingBindStep(f:nat):Prop:=forall c e sp y s xs e' c' sf,
 check_bind f c e sp y s=Good((xs,e',c'),sf)->forall F C,
 simple_bind W F sp y=0->abstract_copy_bind W F sp y=false->named_function_bind W F sp y=false->
 function_lambda_bind W F sp y=false->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl sf F->
 BindResult w P R Fl c e F C(sp,Some y)xs e' c'.
Theorem residual_after_function_lambda:forall fuel,
 OK w P R Fl fuel->LambdaRemainingBindStep(S fuel)->RemainingBindStep w P R Fl(S fuel).
Proof.
 intros fuel IH Other c e sp y s xs e' c' sf Run F C Simple Copy Named I Cache Fwd HC Hp.
 destruct(function_lambda_bind W F sp y)eqn:Class.
 - destruct sp as[[pp|][dn|][br|]];cbn[function_lambda_bind]in Class;try discriminate Class.
   destruct y;cbn[function_lambda_bind]in Class;try discriminate Class.
   destruct(A.resolve_sym W(A.fr_mod F)pp)as[target|]eqn:Target;[|discriminate Class].
   destruct(A.kind_sym W target)as[kind|]eqn:Kind;[|discriminate Class].
   destruct kind;try discriminate Class.
   eapply(check_bind_function_lambda w P R Fl b Hload Finj);
   [exact IH|exact Run|exact Target|exact Kind|exact I|exact Cache|exact Fwd|exact HC|exact Hp].
 - exact(Other _ _ _ _ _ _ _ _ _ Run F C Simple Copy Named Class I Cache Fwd HC Hp).
Qed.
End Dispatch.
