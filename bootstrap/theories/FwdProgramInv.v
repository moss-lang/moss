(* Whole-program lowering preserves every body already completed on entry. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FwdReadonly FwdInvariant FwdPreserve FwdSynthInv.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma okF_lower_all:okF lower_all.
Proof. unfold lower_all;fwt. Qed.
#[local]Hint Resolve okF_lower_all:fwdb.
Lemma kw_entry_zero_width_present:forall fuel e t,keeps st_fwd(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kw_entry_zero_width_present:keepsdb.
Lemma kw_check_main_signature_present:forall g info,keeps st_fwd(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kw_check_main_signature_present:keepsdb.
Lemma okF_elab_program:okF elab_program.
Proof. unfold elab_program;fwt. Qed.
