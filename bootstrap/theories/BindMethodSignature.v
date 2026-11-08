(* Method binds read the caller's visible types, including the receiver. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore
 BodySoundCore BodyTy BodyTr Merge CanonBodyTy CanonTypes NominalSupport TySupport
 SourceClean SourceSupport NeedMapExec FnSigExec SigSound ProviderSig SigReadingSubst ProvideResultCanonical.
Import ListNotations.
Module A := ExecutionModel.Elaboration.
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

(* Bracket bindings provide a snapshot distinct from the caller's own map. *)
Theorem fn_sig_receiver_properties_at : forall c e F C g tm this s sg final,
 fn_sig g tm e (Some this) s = Good (sg,final) ->
 Inv s -> st_w s = w -> nget (w_syms w) (gs_id g) = Some g ->
 CxRel w R Fl c e F C ->
 (forall key ty, nassoc key tm = Some ty -> WFty ty) ->
 (forall key ty, nassoc key tm = Some ty -> Support W ty) ->
 WFty this -> Support W this -> Later w P R Fl final ->
 SigOK sg /\ SigSupport W sg.
Proof.
 intros c e F C g tm this s sg final Read Valid World Loaded Caller TypesWellformed TypesSupported ReceiverType ReceiverSupport Last.
 destruct (caller_types_ok w R Fl b Load c e F C Caller) as [UfClean VisibleClean].
 assert (CallerClean : TmClean (tymap e)).
 { intros key ty Lookup; rewrite nassoc_tymap in Lookup; exact (VisibleClean key ty Lookup). }
 assert (TypesClean : TmClean tm).
 { intros key ty Lookup; exact (support_clean (loaded_record_unowned w R Fl b Load)
     (TypesSupported key ty Lookup) (TypesWellformed key ty Lookup)). }
 assert (ReceiverClean : ety_ok this = true).
 { exact (support_clean (loaded_record_unowned w R Fl b Load) ReceiverSupport ReceiverType). }
 split.
 - exact (fn_sig_clean_at w (P:=P) R Fl b Load _ _ _ _ _ _ _ Read Valid World Loaded
     TypesClean CallerClean ReceiverClean Last).
 - eapply (fn_sig_support_at w (P:=P) R Fl b Load);
     [exact Read|exact Valid|exact World|exact (loader_mod_in w R Fl b Load _ _ Loaded)|
      exact TypesSupported|exact TypesWellformed|exact (cr_support _ _ _ _ _ _ _ Caller)|
      exact (cr_ty_wf _ _ _ _ _ _ _ Caller)|split; assumption|exact Last].
Qed.

Theorem fn_sig_receiver_canonical_at : forall c e F C g tm meanings this s sg final rcv name params result body,
 fn_sig g tm e (Some this) s = Good (sg,final) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C ->
 nget (w_syms w) (gs_id g) = Some g -> gs_decl g = ADFn rcv name params result body ->
 TmRel Fl tm (map (trp Fl) tm) e ->
 (forall key, A.nlook key meanings = option_map (fun ty => tr Fl (canon e ty)) (nassoc key tm)) ->
 WFty this -> Later w P R Fl final ->
 A.SigOf W (gs_id g) meanings (Some (tr Fl (canon e this))) (canonical_sig Fl e sg).
Proof.
 intros c e F C g tm meanings this s sg final rcv name params result body Read Valid World Caller Loaded Decl Types Meaning ReceiverType Last.
 destruct (fn_sig_rel_at w R Fl b Load g tm (map (trp Fl) tm) e (Some this) s sg final rcv name params result body
   Read Valid World Loaded Decl Types ReceiverType Last) as [Signature _].
 pose proof (proj1 (cr_uf _ _ _ _ _ _ _ Caller)) as Triangular.
 pose proof (sigof_subst W (suf Fl (en_uf e)) _ _ _ _ Signature meanings
   (output_subrel Fl e tm meanings Triangular Meaning)) as Canonical.
 cbn [option_map] in Canonical.
 rewrite (tr_canon Fl e this Triangular) in Canonical.
 unfold CtxWalk.tr_sig in Canonical; cbn [A.sg_params A.sg_ret] in Canonical.
 assert (Parameters : map (ExecutionModel.app_subst (suf Fl (en_uf e)))
   (map (fun p => tr Fl (snd p)) (sg_params sg)) =
   map (fun p => tr Fl (canon e (snd p))) (sg_params sg)).
 { rewrite map_map; apply map_ext; intro p; apply tr_canon; exact Triangular. }
 rewrite Parameters, (tr_canon Fl e (sg_ret sg) Triangular) in Canonical.
 exact Canonical.
Qed.

Theorem method_bind_receiver_support_at : forall c e F C pa recv tm s resolved this typed,
 resolve_type_symbol (cx_mod c) pa s = Good (recv,resolved) ->
 symbol_type 200 (cx_mod c) recv tm e resolved = Good (this,typed) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C ->
 (forall key ty, nassoc key tm = Some ty -> WFty ty) ->
 (forall key ty, nassoc key tm = Some ty -> Support W ty) ->
 Later w P R Fl typed -> Support W this.
Proof.
 intros c e F C pa recv tm s resolved this typed Resolve Read Valid World Caller TypesWellformed TypesSupported Last.
 destruct (rts_spec w R Fl b Load (cx_mod c) pa s recv resolved
   Valid World (cr_mod_in _ _ _ _ _ _ _ Caller) Resolve) as [Same [Target [Loaded Kind]]].
 subst resolved.
 eapply (symbol_type_support_at w (P:=P) R Fl b Load);
   [exact Read|exact Valid|exact World|exact Loaded|exact TypesSupported|exact TypesWellformed|
    exact (cr_support _ _ _ _ _ _ _ Caller)|exact (cr_ty_wf _ _ _ _ _ _ _ Caller)|exact Last].
Qed.
End Signature.
