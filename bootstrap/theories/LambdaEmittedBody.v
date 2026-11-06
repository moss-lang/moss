(* The actual fresh lambda symbol and historical emit receipt identify its
   final program body via the proved whole-program lambda history. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound
 ForwarderTrace FinalLambdas.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Body.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Theorem lambda_emitted_body:forall mi name body s pid afterPid beforeEmit compiled emitted,
 new_sym mi name KFn(ADFn RPlain name[]None(Some(ABlock[](Some body))))OLambda s=Good(pid,afterPid)->
 Fr afterPid beforeEmit->emit_fn pid compiled beforeEmit=Good(tt,emitted)->Later w P R Fl emitted->
 A.body_of P pid=Some compiled.
Proof.
 intros mi name body s pid afterPid beforeEmit compiled emitted New Frame Emit L.
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Lookup]].
 pose proof(fr_keep _ _ Frame _ _ Lookup)as Present.
 unfold emit_fn,upd in Emit;injection Emit as <-.
 eapply(lt_lambdas _ _ _ _ _ L);[left;reflexivity|exact Present|reflexivity].
Qed.
End Body.
