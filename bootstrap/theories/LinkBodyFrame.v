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
