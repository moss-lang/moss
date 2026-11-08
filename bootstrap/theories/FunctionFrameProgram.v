(* Actual whole-source execution supplies frames for every reachable function. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 FnReadonly SymReadonly FnBound FnBoundEmit FnBoundSynth FnBoundProgram FnPresentStable
 FnCompleted FnCoverage ForwarderHistory ReifyHeader ReifyHeaderReadonly ReifyHeaderStable
 ReifyHeaderPresent ReifyHeaderProgram ReifyReach ReifyProgramWalk
 FunctionFrameHistory FunctionFrameOps FunctionFrameSynth.
From NM Require Import ProgramTail.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma ff_lower_one:forall fuel id,FF(lower_one fuel id).
Proof.
 intros fuel id;split;[apply hp_lower_one|];split.
 - unfold lower_one, LowerOne.lower_one;apply bf_bind_symbol;intros g Id.
   destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));
   [apply bf_lower_body_at|apply bfa_of_bf,bf_ret].
 - intros initial value final I Bound Run;unfold lower_one, LowerOne.lower_one in Run.
   apply bind_inv in Run as[g[mid[Read Run]]].
   unfold sym in Read;destruct(nget(st_syms initial)id)as[got|]eqn:At;[injection Read as <- <-|discriminate].
   destruct(is_some(fn_body got)&&(kind_eqb(gs_kind got)KFn||kind_eqb(gs_kind got)KMethod)).
   + assert(Physical:nget(st_syms initial)(gs_id got)=Some got)by(rewrite(iv_ids initial I id got At);exact At).
     assert(Below:gs_id got<st_next initial)by(eapply symbol_below_next;eassumption).
     exact(proj2(proj2(ff_lower_body_at fuel got))_ _ _ I Bound Below Physical Run).
   + injection Run as <- <-;intros f body Present Missing;congruence.
Qed.
Lemma ff_lower_all:FF lower_all.
Proof.
 unfold lower_all;apply ff_bind;[apply ff_keeps;[okt|keepst|keepst]|intro snapshot].
 apply ff_miter;intro;apply ff_miter;intro;apply ff_lower_one.
Qed.
#[local]Hint Resolve ff_lower_all:ffdb.
#[local]Hint Resolve kf_entry_zero_width_bound kf_check_main_signature_bound
 kh_entry_zero_width kh_check_main_signature:keepsdb.
Lemma ff_elab_program:FF elab_program.
Proof.
 rewrite program_split.
 apply ff_bind;[fft|intro].
 apply ff_bind;[fft|intro].
 apply ff_bind;[fft|intro].
 apply ff_bind;[exact ff_lower_all|intro].
 apply ff_keeps;[exact program_tail_ok|exact program_tail_functions|exact program_tail_headers].
Qed.
Theorem actual_final_function_frames:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 forall f body,nassoc f(st_fns final)=Some body->FunctionFrameOrigin final f body.
Proof.
 intros b w p final Load Run f body Present.
 eapply(proj2(proj2 ff_elab_program)_ _ _ (@fn_loaded_initial_inv b w Load)(@fn_bound_init w)Run _ _ Present).
 reflexivity.
Qed.
Lemma function_row_visible:forall(rows:list(nat*xexpr))f body,In(f,body)rows->exists visible,nassoc f rows=Some visible.
Proof.
 induction rows as[|[key value]rows IH];intros f body Member;[contradiction|].
 cbn[nassoc assoc];destruct(Nat.eqb f key)eqn:Eq;[eexists;reflexivity|].
 destruct Member as[Same|Member];[injection Same as <- <-;rewrite Nat.eqb_refl in Eq;discriminate|].
 exact(IH _ _ Member).
Qed.
Theorem actual_reachable_function_frames:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 Forall(function_frame_node_ready final)(reify_reachable_nodes p).
Proof.
 intros b w p final Load Run;apply Forall_forall;intros node Member.
 destruct node as[f|h];[|exact I].
 pose proof(reify_reachable_declared p(RnFunction f)Member)as Declared.
 unfold declared_reify_nodes in Declared;apply in_app_iff in Declared as[Functions|Functors].
 - apply in_map_iff in Functions as[[key body][Equal RowMember]];injection Equal as Equal;subst key.
   rewrite(@elab_program_tables _ _ _ Run)in RowMember.
   destruct(@function_row_visible _ _ _ RowMember)as[visible Present].
   exact(proj2(@actual_final_function_frames b w p final Load Run f visible Present)).
 - apply in_map_iff in Functors as[pair[Impossible _]];discriminate.
Qed.
Corollary actual_function_frame_specifications_total:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 exists result,function_frame_specifications final p=Some result.
Proof.
 intros;eapply function_frame_specifications_nodes_total.
 eapply actual_reachable_function_frames;eassumption.
Qed.
