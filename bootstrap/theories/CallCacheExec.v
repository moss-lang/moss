(* The checking-only prefix of a direct call leaves fn_info's cache alone. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame InfoPreserve.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL].
Lemma ni_require_needs : forall mi e g info,NI(require_needs mi e g info).
Proof. intros;unfold require_needs,require_merges;nit. Qed.
Lemma ni_smap_at : forall ei eo ps,NI(smap_at ei eo ps).
Proof. intros;unfold smap_at;nit. Qed.
Lemma ni_fn_sig : forall g tm e this,NI(fn_sig g tm e this).
Proof. intros;unfold fn_sig;nit. Qed.
