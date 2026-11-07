(* Operational link-cache facts. Cache provenance is not part of the current
   Inv/InfoInv/FwdInv/Later predicates; a hit cannot certify LinkBody. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore InfoInvariant InfoPreserve FwdInvariant BodySoundCore.
Import ListNotations.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL].
Lemma link_body_cached : forall fuel fg bg s h,
 assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b))
   (gs_id fg,gs_id bg) (st_links s) = Some h ->
 link_body (S fuel) fg bg s = Good (h,s).
Proof.
 intros fuel fg bg s h Hit; cbn [link_body].
 unfold get,mbind,ret;rewrite Hit;reflexivity.
Qed.
Lemma link_body_fresh : forall W P u g h, A.LinkBody W P u g h -> A.Fresh W h.
Proof. intros W P u g h Linked;inversion Linked;assumption. Qed.
Lemma link_body_zero_impossible : forall W P u g, ~ A.LinkBody W P u g 0.
Proof.
 intros W P u g Linked;destruct (link_body_fresh _ _ _ _ _ Linked) as [_ Nonzero];apply Nonzero;reflexivity.
Qed.
Definition poison_link_cache (fg bg : gsym) (s : st) : st :=
 s_links (fun _ => [((gs_id fg,gs_id bg),0)]) s.
Lemma poison_link_cache_hit : forall fuel fg bg s,
 link_body (S fuel) fg bg (poison_link_cache fg bg s) = Good (0,poison_link_cache fg bg s).
Proof.
 intros;apply link_body_cached;cbn [poison_link_cache s_links st_links assoc fst snd].
 now rewrite !Nat.eqb_refl.
Qed.
Lemma link_cache_update_ok : forall f, ok (upd (s_links f)).
Proof. intros;okt. Qed.
Lemma link_cache_update_frame : forall f s, Inv s -> Inv (s_links f s) /\ Fr s (s_links f s).
Proof. intros f s Valid;exact (link_cache_update_ok f s tt (s_links f s) Valid eq_refl). Qed.
Lemma link_cache_update_info : forall f s, Inv s -> InfoInv s -> InfoInv (s_links f s).
Proof.
 intros f s Valid Cache.
 pose proof (frl_of_fr _ _ (proj2 (link_cache_update_frame f s Valid))) as Frame.
 constructor.
 - intros key info Hit;apply (info_entry_keep s (s_links f s) key info);[apply Cache;exact Hit|exact Frame].
 - intros provider id Hit;apply (wrapper_entry_keep s (s_links f s) provider id);[apply (info_wrappers _ Cache);exact Hit|exact Frame].
Qed.
Lemma link_cache_update_forwarders : forall f s, Inv s -> FwdInv s -> FwdInv (s_links f s).
Proof.
 intros f s Valid Forward;eapply fwd_inv_frame;[exact Forward|exact (proj2 (link_cache_update_frame f s Valid))|reflexivity].
Qed.
Lemma link_cache_update_later : forall w P R Fl f s,
 Later w P R Fl s -> Later w P R Fl (s_links f s).
Proof. intros w P R Fl f s Last;destruct Last;constructor;assumption. Qed.
Theorem cached_link_soundness_needs_provenance : forall w P R Fl fuel fg bg s,
 Inv s -> InfoInv s -> FwdInv s -> Later w P R Fl s ->
 exists cached,
 Inv cached /\ InfoInv cached /\ FwdInv cached /\ Later w P R Fl cached /\
 link_body (S fuel) fg bg cached = Good (0,cached) /\ ~ A.LinkBody (W w R Fl) P (gs_id fg) (gs_id bg) 0.
Proof.
 intros w P R Fl fuel fg bg s Valid Cache Forward Last.
 exists (poison_link_cache fg bg s).
 split;[apply (proj1 (link_cache_update_frame _ s Valid))|].
 split;[apply link_cache_update_info;assumption|].
 split;[apply link_cache_update_forwarders;assumption|].
 split;[apply link_cache_update_later;exact Last|].
 split;[apply poison_link_cache_hit|apply link_body_zero_impossible].
Qed.
