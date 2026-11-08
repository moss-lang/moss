(* A prepared fresh link already supplies the target context needed to read
   its promised output; the temporary roles remain to be renamed back. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr BodyTy KeyTranslation FunctorInputDomain FunctorBodyContext
 ApplyFreshExec LinkBodySound LinkOutputContext LinkOutputProvide LinkOutputRoles
 LinkOutputKeys LambdaKeyTranslation.
Import ListNotations.
Module A := ExecutionModel.Elaboration.
Section Result.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Notation W := (BodySoundCore.W w R Fl).
Theorem prepared_link_result_view : forall fg bg h final (D : @link_body_data w P R Fl fg bg h final)
 name names args res,
 nget (w_syms w) (gs_id fg) = Some fg ->
 gs_decl fg = ADFunctor name names args res None ->
 exists Cr,
 A.ProvideResult W (gs_id fg) (@lbd_Cu _ _ _ _ _ _ _ _ D)
  (fun t => TAbs (generation_of (@lbd_roles _ _ _ _ _ _ _ _ D) t)) Cr
  (map (key_tr Fl) (@lbd_keys _ _ _ _ _ _ _ _ D)) /\
 CxRel w R Fl (functor_body_context fg []) (@lbd_promised _ _ _ _ _ _ _ _ D)
  (functor_body_frame w fg [] []) Cr.
Proof.
 intros fg bg h final D name names args res Loaded Decl.
 destruct (@lbd_output_view _ _ _ _ _ _ _ _ D) as (before&after&View&Valid&Hw&Last).
 pose proof (@lbd_target_body_context _ _ _ _ _ _ _ _ D) as Context.
 pose proof (link_output_frozen_context w R Fl _ _ _ _ Context) as Frozen.
 assert (Mentioned : forall t, A.Mentions W (gs_id fg) t ->
   tn_get (target_input_types (@lbd_ef _ _ _ _ _ _ _ _ D)) t <> None).
 { intros t Mention.
   pose proof (fun_ctx_mentions_available W (gs_id fg) (@lbd_Cu _ _ _ _ _ _ _ _ D)
    (map (key_tr Fl) (@lbd_fneeds _ _ _ _ _ _ _ _ D))
    (A.Decl (gm_path (get_mod w (gs_mod fg))) (gs_decl fg) (gs_assumes fg)) name names args res t
    (w_decl_read w R Fl _ _ Loaded) Decl (@lbd_target_context _ _ _ _ _ _ _ _ D) Mention) as Available.
   rewrite (cr_ty _ _ _ _ _ _ _ Frozen) in Available.
   destruct (tn_get (target_input_types (@lbd_ef _ _ _ _ _ _ _ _ D)) t);[discriminate|].
   exfalso;apply Available;reflexivity. }
 eapply (fresh_target_output_provide w P R Fl b Load Finj
  (functor_body_context fg []) (@lbd_ef _ _ _ _ _ _ _ _ D)
  (functor_body_frame w fg [] []) (@lbd_Cu _ _ _ _ _ _ _ _ D)
  fg name names args res before (@lbd_promised _ _ _ _ _ _ _ _ D)
  (@lbd_keys _ _ _ _ _ _ _ _ D) (@lbd_roles _ _ _ _ _ _ _ _ D) after);
 eassumption.
Qed.
Theorem prepared_link_output_keys_valid : forall fg bg h final
 (D : @link_body_data w P R Fl fg bg h final) c F C,
 CxRel w R Fl c (@lbd_promised _ _ _ _ _ _ _ _ D) F C ->
 Forall LambdaKeyValid (@lbd_keys _ _ _ _ _ _ _ _ D).
Proof.
 intros fg bg h final D c F C Caller.
 destruct (@lbd_output_view _ _ _ _ _ _ _ _ D) as (before&after&View&Valid&Hw&Last).
 destruct (fresh_target_output_phases _ _ _ _ _ _ _ View)
  as (types&typed&allocated&provided&Types&Allocate&Provide&Same&Keys&Unique&Bounds).
 subst provided;eapply output_slots_valid;[exact Caller|].
 eapply provide_result_keys_slots;exact Provide.
Qed.
End Result.
