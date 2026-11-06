(* A lambda's own frame uses its checked target return/receiver types while
   retaining the lexical module, functor application and pinned inputs. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySound NominalSupport LambdaTrace.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition lambda_body_frame(Fl:string->nat)(e:env)(F:A.frame)(sg:fsig)
 (this:option ety)(vars:list(string*nat)):A.frame:=
 A.Frame(A.fr_mod F)(tr Fl(canon e(sg_ret sg)))(option_map(fun t=>tr Fl(canon e t))this)
 false vars[](A.fr_applying F)(A.fr_pins F).
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Theorem cxrel_lambda_frame:forall c e F C ps sg this vars,
 CxRel w R Fl c e F C->WFty(sg_ret sg)->Support W(sg_ret sg)->
 opt_wf this->opt_support W this->
 CxRel w R Fl(lambda_own_context c ps sg this)e(lambda_body_frame Fl e F sg this vars)C.
Proof.
 intros c e F C ps sg this vars H Wr Sr Wthis Sthis.
 destruct H;constructor;cbn[lambda_own_context lambda_own_signature lambda_body_frame
  cx_mod cx_sig cx_this cx_loop cx_pins cx_applying cx_fresh sg_ret A.fr_mod A.fr_ret A.fr_this
  A.fr_loop A.fr_pins A.fr_applying A.fr_fresh];auto.
Qed.
End Context.
