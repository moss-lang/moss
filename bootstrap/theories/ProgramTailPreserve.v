(* The tail allocates platform identities but no executable generated owners. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 FnPresentStable FnBound FnBoundEmit ReifyHeader ReifyHeaderStable
 LambdaCompletion LambdaCompletionOps ForwarderHistory ForwarderHistoryOps
 LambdaOrigin LambdaOriginOps LambdaRows LambdaRowsPreserve
 ProgramTail ProgramTailOrigins.
Import ListNotations.
Set Implicit Arguments.
Lemma program_tail_pf:PF program_tail.
Proof. apply pf_keeps;[exact program_tail_ok|exact program_tail_functions]. Qed.
Lemma program_tail_bf:BF program_tail.
Proof. apply bf_keeps;[exact program_tail_ok|exact program_tail_functions]. Qed.
Lemma program_tail_hs:HS program_tail.
Proof. apply hs_keeps;[exact program_tail_pf|apply header_keeps_of_rows;exact program_tail_headers]. Qed.
Lemma program_tail_no_generated:forall s a final key g,
 program_tail s=Good(a,final)->nget(st_syms final)key=Some g->
 generated_origin(gs_origin g)->nget(st_syms s)key=None->False.
Proof. intros s a final key g Run Lookup Origin Missing.
 pose proof(@program_tail_generated s a final Run key g Lookup Origin)as Old;congruence. Qed.
Lemma program_tail_ch:CH program_tail.
Proof.
 split;[exact program_tail_pf|];split;[exact program_tail_bf|].
 intros b w s a final Load I Bound W Run key owner Lookup Origin Missing.
 exfalso;eapply program_tail_no_generated;[exact Run|exact Lookup|left;exact Origin|exact Missing].
Qed.
Lemma program_tail_gh:GH program_tail.
Proof.
 split;[exact program_tail_pf|];split;[exact program_tail_bf|].
 intros b w s a final Load I Bound W Run key owner target Lookup Origin Missing.
 exfalso;eapply program_tail_no_generated;[exact Run|exact Lookup|right;exists target;exact Origin|exact Missing].
Qed.
Lemma program_tail_lh:LH program_tail.
Proof.
 split;[exact program_tail_hs|];split;[exact program_tail_bf|].
 intros b w s a final Load I Bound W Run key owner Lookup Origin Missing.
 exfalso;eapply program_tail_no_generated;[exact Run|exact Lookup|exact Origin|exact Missing].
Qed.
Lemma program_tail_lc:LC program_tail.
Proof. apply lc_keeps;[exact program_tail_bf|exact program_tail_functions]. Qed.
