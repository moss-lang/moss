(* Readonly completion phases extracted from the actual program pipeline. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore ElabKeeps
  ReadonlyHelpers ReifyHeader InfoPreserve ProgramInfo.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Definition program_tail : M xprogram := ltac:(
 let body := eval cbv delta [elab_program] in elab_program in
 lazymatch body with
 | mbind check_listed (fun _ => mbind check_contexts (fun _ =>
   mbind check_runtime_library (fun _ => mbind lower_all ?tail))) =>
   exact (tail tt)
 end).
Lemma program_split : elab_program =
  (check_listed ;; check_contexts ;; check_runtime_library ;; lower_all ;; program_tail).
Proof. reflexivity. Qed.
Local Lemma kr_entry_zero_width : forall fuel e t,keeps readonly_state(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kr_entry_zero_width:keepsdb.
Local Lemma kr_main_signature : forall g info,keeps readonly_state(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kr_main_signature:keepsdb.
Lemma program_tail_readonly : keeps readonly_state program_tail.
Proof. unfold program_tail;keepst. Qed.
Lemma program_tail_info : okI program_tail.
Proof. unfold program_tail;program_info. Qed.

Lemma program_tail_functions : keeps st_fns program_tail.
Proof. eapply keeps_projection with (p:=readonly_state) (q:=fun x=>fst(fst(fst(fst x))));exact program_tail_readonly. Qed.
Lemma program_tail_forwarders : keeps st_fwd program_tail.
Proof. eapply keeps_projection with (p:=readonly_state) (q:=fun x=>snd(fst(fst(fst x))));exact program_tail_readonly. Qed.
Lemma program_tail_headers : keeps function_headers program_tail.
Proof. eapply keeps_projection with (p:=readonly_state) (q:=snd);exact program_tail_readonly. Qed.
Lemma program_tail_ok : ok program_tail.
Proof. exact (proj1 program_tail_info). Qed.
