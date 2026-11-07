(* Method receiver types are read under the caller's canonical meanings. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTy CanonBodyTy CanonTypes Merge TyOfSubst MethodBindRead MethodLookup MethodClean MethodKind.
Import ListNotations.
Module A := Audit.Elaboration.

Lemma method_receiver_canonical : forall W Fl e tm meanings home pa this,
 Tri (en_uf e) ->
 (forall key, A.nlook key meanings =
    option_map (fun y => tr Fl (canon e y)) (nassoc key tm)) ->
 A.TyOf W home (map (trp Fl) tm) None (ATRef pa None) (tr Fl this) ->
 A.TyOf W home meanings None (ATRef pa None) (tr Fl (canon e this)).
Proof.
 intros W Fl e tm meanings home pa this Triangular Meaning Read.
 assert (Related : SubRel (suf Fl (en_uf e)) (map (trp Fl) tm) meanings).
 { intro key; rewrite nlook_trp, Meaning.
   destruct (nassoc key tm); cbn [option_map];
   [rewrite tr_canon by exact Triangular|]; reflexivity. }
 pose proof (tyof_subst W (suf Fl (en_uf e)) _ _ _ _ _ Read meanings Related) as Canonical.
 cbn [option_map] in Canonical.
 rewrite tr_canon in Canonical by exact Triangular; exact Canonical.
Qed.

Section Target.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Notation W := (BodySoundCore.W w R Fl).


Theorem method_bind_target_lookup : forall c e F C pa dn s recv resolved m selected,
 resolve_type_symbol (cx_mod c) pa s = Good (recv,resolved) ->
 find_method_decl (cx_mod c) recv dn resolved = Good (Some m,selected) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C ->
 resolved = s /\ selected = s /\
 A.resolve_sym W (A.fr_mod F) pa = Some (gs_id recv) /\
 A.MethodNamed W (A.fr_mod F) (gs_id recv) dn (gs_id m) /\
 nget (w_syms w) (gs_id m) = Some m /\
 A.kind_sym W (gs_id m) = Some A.DKMeth.
Proof.
 intros c e F C pa dn s recv resolved m selected Resolve Select Valid World Context.
 pose proof (cr_mod_in _ _ _ _ _ _ _ Context) as HomeLoaded.
 destruct (rts_spec w R Fl b Load (cx_mod c) pa s recv resolved
   Valid World HomeLoaded Resolve) as [Same [Target [Loaded Kinds]]].
 subst resolved.
 destruct (find_method_decl_named b w R Fl (cx_mod c) recv dn s m selected
   Load HomeLoaded Loaded Valid World Select) as [Same Named].
 destruct (find_method_decl_loaded b w R Fl (cx_mod c) recv dn s m selected
   Load HomeLoaded Loaded Valid World Select) as [_ Method].
 pose proof (method_named_kind b w R Fl (cx_mod c) (gs_id recv) dn (gs_id m)
   Load HomeLoaded Named) as Kind.
 subst selected; split; [reflexivity|]; split; [reflexivity|].
 split; [rewrite (cr_mod _ _ _ _ _ _ _ Context); exact Target|].
 split; [rewrite (cr_mod _ _ _ _ _ _ _ Context); exact Named|].
 split; assumption.
Qed.


Theorem method_bind_target_reading : forall c e F C pa recv tm meanings s resolved this typed,
 resolve_type_symbol (cx_mod c) pa s = Good (recv,resolved) ->
 symbol_type 200 (cx_mod c) recv tm e resolved = Good (this,typed) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl typed ->
 TmRel Fl tm (map (trp Fl) tm) e ->
 (forall key, A.nlook key meanings = option_map (fun y => tr Fl (canon e y)) (nassoc key tm)) ->
 A.TyOf W (A.fr_mod F) meanings None (ATRef pa None) (tr Fl (canon e this)) /\ WFty this.
Proof.
 intros c e F C pa recv tm meanings s resolved this typed Resolve Read Valid World Context Last Types Meaning.
 destruct (method_bind_receiver w (P:=P) R Fl b Load (cx_mod c) pa recv tm
   (map (trp Fl) tm) e s resolved this typed Resolve Read Valid World
   (cr_mod_in _ _ _ _ _ _ _ Context) Types Last) as [Receiver Wellformed].
 split; [|exact Wellformed].
 rewrite <- (cr_mod _ _ _ _ _ _ _ Context) in Receiver.
 eapply method_receiver_canonical; [exact (proj1 (cr_uf _ _ _ _ _ _ _ Context))|exact Meaning|exact Receiver].
Qed.

Theorem method_bind_target_no_brackets : forall c e F C pa recv s resolved this typed,
 resolve_type_symbol (cx_mod c) pa s = Good (recv,resolved) ->
 symbol_type 200 (cx_mod c) recv (tymap e) e resolved = Good (this,typed) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl typed ->
 A.TyOf W (A.fr_mod F) (A.c_ty C) None (ATRef pa None) (tr Fl (canon e this)) /\
 WFty this.
Proof.
 intros c e F C pa recv s resolved this typed Resolve Read Valid World Context Last.
 assert (Types : TmRel Fl (tymap e) (map (trp Fl) (tymap e)) e).
 { apply tmrel_tymap; [exact (cr_ty_wf _ _ _ _ _ _ _ Context)|].
   intro key; rewrite nlook_trp, nassoc_tymap; reflexivity. }
 destruct (method_bind_receiver w (P:=P) R Fl b Load (cx_mod c) pa recv (tymap e)
   (map (trp Fl) (tymap e)) e s resolved this typed Resolve Read Valid World
   (cr_mod_in _ _ _ _ _ _ _ Context) Types Last) as [Receiver Wellformed].
 split; [|exact Wellformed].
 rewrite <- (cr_mod _ _ _ _ _ _ _ Context) in Receiver.
 eapply method_receiver_canonical; [exact (proj1 (cr_uf _ _ _ _ _ _ _ Context))| |exact Receiver].
 intro key; rewrite nassoc_tymap; exact (cr_ty _ _ _ _ _ _ _ Context key).
Qed.
End Target.
