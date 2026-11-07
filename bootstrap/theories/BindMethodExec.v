(* The complete method-binding rule, from actual execution and provider proofs. *)
From NM Require Import BodyProofTools.
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTy EtyTyExec CanonBodyTy CanonTypes Merge MethodKeyTr MethodKind
 ProviderSig ProvideResultMethod BindSequence LambdaProviderExec
 BindMethodTrace BindMethodTarget BindMethodSuffix BindMethodSignature BindMethodBrackets MethodBindRead
 BindMethodLambdaExec BindMethodProviderExec.
Import ListNotations.
Local Open Scope list_scope.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

Section Method.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Hypothesis World : A.WorldOf b (BodySoundCore.W w R Fl).
Hypothesis Finj : forall a b, Fl a = Fl b -> a = b.
Notation W := (BodySoundCore.W w R Fl).
Theorem check_bind_method : forall fuel c e pa dn a y s xs e' c' final F C,
 OK w P R Fl fuel ->
 check_bind (S fuel) c e (Spec (Some pa) (Some dn) a) y s =
   Good ((xs,e',c'),final) ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl final F ->
 BindResult w P R Fl c e F C (Spec (Some pa) (Some dn) a,Some y) xs e' c'.
Proof.
 intros fuel c e pa dn a y s xs e' c' final F C IH Run I Cache Fwd HC Last.
 destruct (ok_check_bind (S fuel) c e (Spec (Some pa) (Some dn) a) y
   _ _ _ I Run) as [_ Whole].
 assert (Hw : st_w s = w) by (rewrite <- (fr_w _ _ Whole);exact (lt_w _ _ _ _ _ (proj1 Last))).
 destruct (check_bind_method_trace fuel c e pa dn a y s xs e' c' final Run) as
  (tm & types & recv & resolved & m & selected & this & typed & sg & signed & ts & shown &
   target & pre & sm & provided & Types & Resolve & Select & Receiver & Signature & Show & Provider & Suffix).
 destruct (ok_method_bind_types e (cx_mod c) a _ _ _ I Types) as [Ib Fb].
 assert (Wtypes : st_w types = w) by (rewrite (fr_w _ _ Fb);exact Hw).
 assert (CacheTypes : InfoInv types) by (exact (proj2 (okI_method_bind_types e (cx_mod c) a) s tm types I Cache Types)).
 assert (FwdTypes : FwdInv types) by (exact (proj2 (okF_method_bind_types e (cx_mod c) a) s tm types I Fwd Types)).
 destruct (method_bind_target_lookup w R Fl b Load c e F C pa dn types recv resolved m selected
   Resolve Select Ib Wtypes HC) as [Er [Es [Target [Named [Loaded Kind]]]]].
 subst resolved selected.
 destruct (loaded_method_fn w R Fl m Loaded Kind) as (rcv & name & params & result & code & Decl).
 destruct (ok_symbol_type 200 (cx_mod c) recv tm e _ _ _ Ib Receiver) as [It Ft].
 assert (Os : ok (fn_sig m tm e (Some this))) by (unfold fn_sig;okt).
 destruct (Os _ _ _ It Signature) as [Is Fs].
 destruct (ok_show this _ _ _ Is Show) as [Ish Fsh].
 destruct (ok_method_bind_provider fuel c e y this m sg ts _ _ _ Ish Provider) as [Ip Fp].
 destruct (ok_method_bind_suffix c e tm this m sg ts target pre sm _ _ _ Ip Suffix) as [_ Fsuffix].
 pose proof (post_back w P R Fl _ _ F Fsuffix Last) as Pprovider.
 pose proof (later_back w P R Fl _ _ Fp (proj1 Pprovider)) as Lshown.
 pose proof (later_back w P R Fl _ _ Fsh Lshown) as Lsigned.
 pose proof (later_back w P R Fl _ _ Fs Lsigned) as Ltyped.
 assert (Wtyped : st_w typed = w) by (rewrite (fr_w _ _ Ft);exact Wtypes).
 assert (Wshown : st_w shown = w) by (rewrite (fr_w _ _ Fsh),(fr_w _ _ Fs);exact Wtyped).
 pose proof (later_back w P R Fl _ _ Ft Ltyped) as Ltypes.
 destruct (method_bind_types_snapshot w P R Fl b Load c e F C a s tm types
   Types I Hw HC Ltypes) as (bs & OutputBrackets & Meaning & TmWF & TmSupport & TypesRel).
 destruct (method_bind_target_reading w P R Fl b Load c e F C pa recv tm (bs ++ A.c_ty C) types types this typed
   Resolve Receiver Ib Wtypes HC Ltyped TypesRel Meaning) as [TypeReading Wthis].
 pose proof (method_bind_receiver_support_at w P R Fl b Load c e F C pa recv tm types types this typed
   Resolve Receiver Ib Wtypes HC TmWF TmSupport Ltyped) as Sthis.
 destruct (fn_sig_receiver_properties_at w P R Fl b Load c e F C m tm this typed sg signed
   Signature It Wtyped Loaded HC TmWF TmSupport Wthis Sthis Lsigned) as [Wsig Ssig].
 pose proof (fn_sig_receiver_canonical_at w P R Fl b Load c e F C m tm (bs ++ A.c_ty C) this typed sg signed
   rcv name params result code Signature It Wtyped HC Loaded Decl TypesRel Meaning Wthis Lsigned) as SigReading.
 assert (Wprovided : st_w provided = w) by (rewrite (fr_w _ _ Fp);exact Wshown).
 assert (Cshown : InfoInv shown) by cache_inv.
 assert (Fshown : FwdInv shown) by fwd_inv.
 pose proof (method_bind_provider_sound w P R Fl b Load World Finj fuel c e F C y this m sg ts
   shown target pre sm provided IH Provider Ish Cshown Fshown Wshown Loaded HC Wsig Ssig Wthis Sthis Pprovider)
   as ProviderMeaning.
 eapply (method_bind_suffix_sound w P R Fl b Load Finj c e tm this m sg ts target pre sm
   provided xs e' c' final F C pa dn a (gs_id recv) bs y rcv name params result code).
 - exact Suffix.
 - exact Ip.
 - exact Wprovided.
 - exact (proj1 Last).
 - exact HC.
 - exact Loaded.
 - exact Decl.
 - exact Kind.
 - exact TypesRel.
 - exact Meaning.
 - exact Wthis.
 - exact Sthis.
 - exact Wsig.
 - exact Ssig.
 - exact Target.
 - exact Named.
 - apply method_output_brackets; [exact Kind|exact OutputBrackets].
 - exact TypeReading.
 - exact SigReading.
 - exact ProviderMeaning.
Qed.

End Method.
