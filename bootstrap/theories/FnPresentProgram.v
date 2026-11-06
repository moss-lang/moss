(* Whole-program lowering preserves every body already completed on entry. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnPresentStable FnPresentEmit FnPresentSynth.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma pf_lower_all:PF lower_all.
Proof. unfold lower_all;pft. Qed.
#[local]Hint Resolve pf_lower_all:pfdb.
Lemma kf_entry_zero_width_present:forall fuel e t,keeps st_fns(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kf_entry_zero_width_present:keepsdb.
Lemma kf_check_main_signature_present:forall g info,keeps st_fns(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kf_check_main_signature_present:keepsdb.
Lemma pf_elab_program:PF elab_program.
Proof. unfold elab_program;pft. Qed.
