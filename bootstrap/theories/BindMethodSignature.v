(* Method binds read the caller's visible types, including the receiver. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore
 BodySoundCore BodyTy CanonBodyTy CanonTypes NominalSupport TySupport
 SourceClean SourceSupport NeedMapExec FnSigExec.
Import ListNotations.
Module A := Audit.Elaboration.
Section Signature.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Notation W := (BodySoundCore.W w R Fl).

Theorem fn_sig_receiver_properties : forall c e F C g this s sg final,
 fn_sig g (tymap e) e (Some this) s = Good (sg,final) ->
 Inv s -> st_w s = w -> nget (w_syms w) (gs_id g) = Some g ->
 CxRel w R Fl c e F C -> WFty this -> Support W this -> Later w P R Fl final ->
 SigOK sg /\ SigSupport W sg.
Proof.
 intros c e F C g this s sg final Read Valid World Loaded Caller ReceiverType ReceiverSupport Last.
 destruct (caller_types_ok w R Fl b Load c e F C Caller) as [UfClean VisibleClean].
 assert (TypesClean : TmClean (tymap e)).
 { intros key ty Lookup; rewrite nassoc_tymap in Lookup; exact (VisibleClean key ty Lookup). }
 assert (ReceiverClean : ety_ok this = true).
 { exact (support_clean (loaded_record_unowned w R Fl b Load) ReceiverSupport ReceiverType). }
 split.
 - exact (fn_sig_clean_at w (P:=P) R Fl b Load _ _ _ _ _ _ _ Read Valid World Loaded
     TypesClean TypesClean ReceiverClean Last).
 - eapply (fn_sig_support_at w (P:=P) R Fl b Load);
     [exact Read|exact Valid|exact World|exact (loader_mod_in w R Fl b Load _ _ Loaded)| | |
      exact (cr_support _ _ _ _ _ _ _ Caller)|exact (cr_ty_wf _ _ _ _ _ _ _ Caller)|
      split; assumption|exact Last].
   + intros key ty Lookup; rewrite nassoc_tymap in Lookup.
     exact (@tn_get_support W e key ty (cr_support _ _ _ _ _ _ _ Caller) Lookup).
   + intros key ty Lookup; rewrite nassoc_tymap in Lookup.
     exact (cr_ty_wf _ _ _ _ _ _ _ Caller key ty Lookup).
Qed.

Theorem method_bind_receiver_support : forall c e F C pa recv s resolved this typed,
 resolve_type_symbol (cx_mod c) pa s = Good (recv,resolved) ->
 symbol_type 200 (cx_mod c) recv (tymap e) e resolved = Good (this,typed) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl typed ->
 Support W this.
Proof.
 intros c e F C pa recv s resolved this typed Resolve Read Valid World Caller Last.
 destruct (rts_spec w R Fl b Load (cx_mod c) pa s recv resolved
   Valid World (cr_mod_in _ _ _ _ _ _ _ Caller) Resolve) as [Same [Target [Loaded Kind]]].
 subst resolved.
 eapply (symbol_type_support_at w (P:=P) R Fl b Load);
   [exact Read|exact Valid|exact World|exact Loaded| | |
    exact (cr_support _ _ _ _ _ _ _ Caller)|exact (cr_ty_wf _ _ _ _ _ _ _ Caller)|exact Last].
 - intros key ty Lookup; rewrite nassoc_tymap in Lookup.
   exact (@tn_get_support W e key ty (cr_support _ _ _ _ _ _ _ Caller) Lookup).
 - intros key ty Lookup; rewrite nassoc_tymap in Lookup.
   exact (cr_ty_wf _ _ _ _ _ _ _ Caller key ty Lookup).
Qed.
End Signature.
