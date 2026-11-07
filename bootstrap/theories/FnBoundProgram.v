(* Allocation bounds through the actual top-level source-function traversal. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnBound FnBoundEmit FnBoundSynth.
From NM Require Import ProgramTail ProgramInfo.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma bf_bind_symbol:forall A id(k:gsym->M A),
 (forall g,gs_id g=id->BF_at(gs_id g)(k g))->BF(mbind(sym id)k).
Proof.
 intros A id k K;split.
 - intros s value final I E;apply bind_inv in E as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
   exact(proj1(K got(iv_ids s I id got Lookup))_ _ _ I Rest).
 - intros s value final I Bnd E;apply bind_inv in E as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
   assert(Below:gs_id got<st_next s).
   {rewrite(iv_ids s I id got Lookup);destruct(Nat.lt_ge_cases id(st_next s));[assumption|]. rewrite(iv_above s I id H)in Lookup;discriminate. }
   exact(proj2(K got(iv_ids s I id got Lookup))_ _ _ I Bnd Below Rest).
Qed.
Lemma bf_lower_all:BF lower_all.
Proof.
 unfold lower_all;apply bf_bind;[apply bf_keeps;[okt|keepst]|]. intro s.
 apply bf_miter;intro mi;apply bf_miter;intro id.
 apply bf_bind_symbol;intros g Id.
 destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));
 [apply bf_lower_body_at|apply bfa_of_bf,bf_ret].
Qed.
#[local]Hint Resolve bf_lower_all:bfdb.
Lemma kf_entry_zero_width_bound:forall fuel e t,keeps st_fns(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kf_entry_zero_width_bound:keepsdb.
Lemma kf_check_main_signature_bound:forall g info,keeps st_fns(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kf_check_main_signature_bound:keepsdb.
Lemma bf_elab_program:BF elab_program.
Proof.
 rewrite program_split.
 apply bf_bind;[apply bf_keeps;[exact(proj1 program_info_listed)|unfold check_listed;keepst]|intro u1].
 apply bf_bind;[apply bf_keeps;[exact(proj1 program_info_contexts)|unfold check_contexts;keepst]|intro u2].
 apply bf_bind;[apply bf_keeps;[exact(proj1 program_info_runtime)|unfold check_runtime_library;keepst]|intro u3].
 apply bf_bind;[exact bf_lower_all|intro u4].
 apply bf_keeps;[exact program_tail_ok|exact program_tail_functions].
Qed.
