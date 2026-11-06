(* Actual whole-program history establishes final generated-body facts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv LoaderOrigin
 FnReadonly SymReadonly FnBound FnBoundProgram FnPresentStable FnPresentSynth FnPresentProgram
 ForwarderHistory LambdaCompletion LambdaCompletionOps LambdaCompletionEmit LambdaCompletionSynth ForwarderTrace ForwarderProgramTables.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma ch_bind_symbol:forall A id(k:gsym->M A),
 (forall g,gs_id g=id->PF(k g)/\CH_at(gs_id g)(k g))->CH(mbind(sym id)k).
Proof.
 intros A id k K;split.
 - split.
   + intros s value final I E;apply bind_inv in E as[g[mid[Read Rest]]].
     unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
     exact(proj1(proj1(K got(iv_ids s I id got Lookup)))_ _ _ I Rest).
   + intros s value final I E;apply bind_inv in E as[g[mid[Read Rest]]].
     unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
     exact(proj2(proj1(K got(iv_ids s I id got Lookup)))_ _ _ I Rest).
 - split.
   + apply bf_bind_symbol;intros g Id;exact(proj1(proj2(proj2(K g Id)))).
   + intros b w s value final Load I Bound W E;apply bind_inv in E as[g[mid[Read Rest]]].
     unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
     assert(Below:gs_id got<st_next s)by(rewrite(iv_ids s I id got Lookup);eapply symbol_below_next;eassumption).
     exact(proj2(proj2(proj2(K got(iv_ids s I id got Lookup))))b w _ _ _ Load I Bound W Below Rest).
Qed.
Lemma ch_lower_all:CH lower_all.
Proof.
 unfold lower_all;apply ch_bind;[apply ch_keeps;[okt|keepst|keepst]|]. intro s.
 apply ch_miter;intro mi;apply ch_miter;intro id.
 apply ch_bind_symbol;intros g Id.
 destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));split.
 - apply pf_lower_body.
 - apply ch_lower_body.
 - apply pf_ret.
 - apply ch_at_of_ch,ch_ret.
Qed.
#[local]Hint Resolve ch_lower_all:chdb.
Lemma kf_entry_zero_width_history:forall fuel e t,keeps st_fns(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
Lemma ks_entry_zero_width_history:forall fuel e t,keeps st_syms(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kf_entry_zero_width_history ks_entry_zero_width_history:keepsdb.
Lemma kf_check_main_signature_history:forall g info,keeps st_fns(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
Lemma ks_check_main_signature_history:forall g info,keeps st_syms(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kf_check_main_signature_history ks_check_main_signature_history:keepsdb.
Lemma ch_elab_program:CH elab_program.
Proof. unfold elab_program;cht. Qed.
