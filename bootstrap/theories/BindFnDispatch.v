(* Dispatch the concrete named defined-provider case before remaining bind cases. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv InfoPreserve BodySoundCore BindSequence BindCopyDispatch BindFnDefinedExec BindFnAbstractExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition named_function_bind(W:A.world)(F:A.frame)(sp:spec)(y:aexpr):bool:=
 match sp,y with
 |Spec(Some pp)None None,AEPath pg None=>
  match A.resolve_sym W(A.fr_mod F)pp,A.resolve_sym W(A.fr_mod F)pg with
  |Some f,Some u=>match A.kind_sym W f,A.ast_of W u with
    |Some A.DKFn,Some(ADFn RPlain _ _ _ _)=>true|_,_=>false end
  |_,_=>false end
 |_,_=>false end.
Section Dispatch.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Notation WW:=(BodySoundCore.W w R Fl).
Definition RemainingBindStep(f:nat):Prop:=forall c e sp y s xs e' c' s',
 check_bind f c e sp y s=Good((xs,e',c'),s')->forall F C,
 simple_bind WW F sp y=0 ->abstract_copy_bind WW F sp y=false ->
 named_function_bind WW F sp y=false ->
 Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->
 BindResult w P R Fl c e F C(sp,Some y)xs e' c'.
Theorem residual_after_named_function:forall f,RemainingBindStep f->OtherBindStep w P R Fl f.
Proof.
 intros f Other c e sp y s xs e' c' s' E F C Simple Copy Hi Ii Ifwd Hc Hp.
 destruct(named_function_bind WW F sp y)eqn:Class.
 - destruct sp as[[pp|][dn|][br|]];cbn[named_function_bind]in Class;try discriminate.
   destruct y;try discriminate Class.
   match goal with a:sapp|-_=>destruct a end;try discriminate Class.
   destruct(A.resolve_sym WW(A.fr_mod F)pp)as[fnid|]eqn:Lf;[|discriminate].
   destruct(A.resolve_sym WW(A.fr_mod F)p)as[uid|]eqn:Lg;[|discriminate].
   destruct(A.kind_sym WW fnid)as[k|]eqn:K;[|discriminate].
   destruct k;try discriminate Class.
   destruct(A.ast_of WW uid)as[d|]eqn:D;[|discriminate].
   destruct d;try discriminate Class.
   match type of D with _=Some(ADFn ?r _ _ _ _)=>destruct r end;try discriminate Class.
   match type of D with _=Some(ADFn _ _ _ _ ?body)=>destruct body end;try discriminate Class.
   + destruct(check_bind_named_defined w (P:=P) R Fl b Hload Hworld Finj
       f c e pp p s xs e' c' s' F C fnid uid _ _ _ _ E Lf K Lg D Hi Ii Hc(proj1 Hp))as[Ec[C'[Out Bind]]].
     eapply bind_result_same;[exact Ec|exact Out|exact Bind].
   + destruct(check_bind_named_abstract w (P:=P) R Fl b Hload Hworld
       f c e pp p s xs e' c' s' F C fnid uid _ _ _ E Lf K Lg D Hi Ii Ifwd Hc(proj1 Hp))as[Ec[C'[Out Bind]]].
     eapply bind_result_same;[exact Ec|exact Out|exact Bind].
 - exact(Other c e sp y s xs e' c' s' E F C Simple Copy Class Hi Ii Ifwd Hc Hp).
Qed.
End Dispatch.
