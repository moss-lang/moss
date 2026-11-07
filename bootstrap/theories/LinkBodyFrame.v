(* State frames for the named executable stages of fresh linking. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame LinkBodyTrace.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

Lemma ok_link_trace_capture_guard : forall fg bg ef eg fneeds gneeds xf,
 ok (link_trace_capture_guard fg bg ef eg fneeds gneeds xf).
Proof. intros;unfold link_trace_capture_guard;okt. Qed.
Lemma ok_link_trace_renames : forall promised eb roles made dr,
 ok (link_trace_renames promised eb roles made dr).
Proof. intros;unfold link_trace_renames;okt. Qed.
Lemma ok_link_trace_types_guard : forall fg bg eg eb rts,
 ok (link_trace_types_guard fg bg eg eb rts).
Proof. intros;unfold link_trace_types_guard;okt. Qed.
Lemma ok_link_trace_items_guard : forall fg bg eg eb made dr,
 ok (link_trace_items_guard fg bg eg eb made dr).
Proof. intros;unfold link_trace_items_guard;okt. Qed.
Lemma ok_link_trace_readings_guard : forall fg bg promised eb made dr,
 ok (link_trace_readings_guard fg bg promised eb made dr).
Proof. intros;unfold link_trace_readings_guard;okt. Qed.
Lemma ok_link_trace_finish : forall fg bg ef promised eb rts dr roles binds,
 ok (link_trace_finish fg bg ef promised eb rts dr roles binds).
Proof. intros;unfold link_trace_finish;okt. Qed.
#[export] Hint Resolve ok_link_trace_capture_guard ok_link_trace_renames
 ok_link_trace_types_guard ok_link_trace_items_guard ok_link_trace_readings_guard
 ok_link_trace_finish : okdb.

Lemma ok_link_trace_after_body : forall fg bg ef eg promised eb xf rts dr roles xs,
 ok (link_trace_after_body fg bg ef eg promised eb xf rts dr roles xs).
Proof. intros;unfold link_trace_after_body;okt. Qed.
#[export] Hint Resolve ok_link_trace_after_body : okdb.
Lemma ok_link_trace_after_capture : forall fuel fg bg ef eg gneeds xf rts,
 ok (link_trace_after_capture fuel fg bg ef eg gneeds xf rts).
Proof. intros;unfold link_trace_after_capture;okt. Qed.
#[export] Hint Resolve ok_link_trace_after_capture : okdb.
Lemma ok_link_trace_fresh : forall fuel fg bg,
 ok (link_trace_fresh fuel fg bg).
Proof. intros;unfold link_trace_fresh;okt. Qed.
#[export] Hint Resolve ok_link_trace_fresh : okdb.

From NM Require Import InfoInvariant InfoInputs InfoPreserve FwdInvariant FwdReadonly FwdPreserve ElabKeeps.
Lemma okI_link_trace_capture_guard : forall fg bg ef eg fneeds gneeds xf,
 okI (link_trace_capture_guard fg bg ef eg fneeds gneeds xf).
Proof. intros;unfold link_trace_capture_guard;okIt. Qed.
Lemma link_trace_capture_state : forall fg bg ef eg fneeds gneeds xf s u final,
 link_trace_capture_guard fg bg ef eg fneeds gneeds xf s = Good (u,final) -> final = s.
Proof.
 intros fg bg ef eg fneeds gneeds;induction gneeds as [|k rest IH];intros xf s u final Run.
 - injection Run as _ <-;reflexivity.
 - unfold link_trace_capture_guard in Run;cbn [miter] in Run.
   apply bind_inv in Run as [unitv [mid [Check Rest]]].
   assert (Same : mid = s).
   { destruct k;cbn beta iota zeta in Check;
     match type of Check with (if ?test then _ else _) _ = _ => destruct test end;
     try (injection Check as _ <-;reflexivity);
     apply bind_inv in Check as [shown [shownState [Show Bad]]];exfalso;eapply err_not_good;exact Bad. }
   subst mid;exact (IH xf s u final Rest).
Qed.
Lemma okF_link_trace_capture_guard : forall fg bg ef eg fneeds gneeds xf,
 okF (link_trace_capture_guard fg bg ef eg fneeds gneeds xf).
Proof.
 intros;apply okF_readonly;[apply ok_link_trace_capture_guard|].
 intros s u final Run;rewrite (link_trace_capture_state _ _ _ _ _ _ _ _ _ _ Run);reflexivity.
Qed.
Lemma okI_link_output_view : forall ef fg, okI (fresh_target_output_view ef fg).
Proof. intros;apply okI_ni;[unfold fresh_target_output_view;okt|unfold fresh_target_output_view;nit|unfold fresh_target_output_view;keepst]. Qed.
Lemma okF_link_output_view : forall ef fg, okF (fresh_target_output_view ef fg).
Proof. intros;unfold fresh_target_output_view;fwt. Qed.
#[export] Hint Resolve okI_link_trace_capture_guard okI_link_output_view : okIdb.
#[export] Hint Resolve okF_link_trace_capture_guard okF_link_output_view : fwdb.
