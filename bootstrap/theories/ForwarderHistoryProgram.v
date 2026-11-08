(* Actual whole-program history establishes final generated-body facts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv LoaderOrigin
 FnReadonly SymReadonly FnBound FnBoundProgram FnPresentStable FnPresentSynth FnPresentProgram
 ForwarderHistory ForwarderHistoryOps ForwarderHistoryEmit ForwarderHistorySynth ForwarderTrace ForwarderProgramTables.
From NM Require Import ProgramTail ProgramTailPreserve.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma gh_bind_symbol:forall A id(k:gsym->M A),
 (forall g,gs_id g=id->PF(k g)/\GH_at(gs_id g)(k g))->GH(mbind(sym id)k).
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
Lemma gh_lower_all:GH lower_all.
Proof.
 unfold lower_all;apply gh_bind;[apply gh_keeps;[okt|keepst|keepst]|]. intro s.
 apply gh_miter;intro mi;apply gh_miter;intro id.
 apply gh_bind_symbol;intros g Id.
 destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));split.
 - apply pf_lower_body.
 - apply gh_lower_body_at.
 - apply pf_ret.
 - apply gh_at_of_gh,gh_ret.
Qed.
#[local]Hint Resolve gh_lower_all:ghdb.
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
Lemma gh_elab_program:GH elab_program.
Proof.
 rewrite program_split.
 apply gh_bind;[ght|intro].
 apply gh_bind;[ght|intro].
 apply gh_bind;[ght|intro].
 apply gh_bind;[exact gh_lower_all|intro].
 exact program_tail_gh.
Qed.
Lemma nlook_source_lookup:forall A key(rows:list(nat*A)),ExecutionModel.Elaboration.nlook key rows=nassoc key rows.
Proof. intros A key rows;induction rows as[|[k v]rows IH];cbn[ExecutionModel.Elaboration.nlook nassoc assoc];[reflexivity|].
 destruct(Nat.eqb key k);[reflexivity|exact IH]. Qed.
Lemma forwarder_initial_inv:forall b w,load_world b=Good w->Inv(s_init w).
Proof.
 intros b w Load;destruct(load_world_syms b w Load)as[Positive Above].
 constructor;cbn[st_next st_syms st_flds st_w st_reqs s_init].
 - exact Above.
 - split;[constructor|reflexivity].
 - exact(load_world_ids b w Load).
 - intros b' Load' id g H;exact H.
 - intros b' Load' id g Missing H;rewrite Missing in H;discriminate.
 - intros b' Load' id values H;destruct id;discriminate.
 - intros b' Load' id g Missing H;rewrite Missing in H;discriminate.
Qed.
Theorem elab_final_forwarders:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 forall key g target,nget(st_syms final)key=Some g->gs_origin g=OForwarder target->
 exists tg,LoadedAbstract w tg /\gs_id tg=target/\gs_decl g=forwarder_decl tg/\
 gs_mod g=gs_mod tg/\gs_recv g=None/\
 ExecutionModel.Elaboration.body_of p key=Some(XCallA target(map XVar(seq 0(List.length(fn_params g))))).
Proof.
 intros b w p final Load Run key g target Lookup Origin.
 assert(I:Inv(s_init w))by(eapply forwarder_initial_inv;exact Load).
 destruct(proj1(proj1 gh_elab_program)_ _ _ I Run)as[IF Frame].
 assert(Source:SourceSyms(st_syms(s_init w)))by(eapply load_world_source;exact Load).
 pose proof(proj2(proj2 gh_elab_program)b w _ _ _ Load I(@fn_bound_init w)eq_refl Run)as Hist.
 destruct(@forwarder_history_final_facts w(s_init w)final Source Frame Hist key g target Lookup Origin)as[tg[LA[Id[Decl[Home[Recv Body]]]]]].
 exists tg;split;[exact LA|]. repeat split;try assumption.
 unfold ExecutionModel.Elaboration.body_of;rewrite(@forwarder_elab_program_tables _ _ _ Run),nlook_source_lookup;exact Body.
Qed.
Theorem elab_final_forwarder_nonzero:forall b w p final key g target,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 nget(st_syms final)key=Some g->gs_origin g=OForwarder target->key<>0.
Proof.
 intros b w p final key g target Load Run Lookup Origin.
 pose proof(@forwarder_initial_inv b w Load)as I.
 destruct(proj1(proj1 gh_elab_program)_ _ _ I Run)as[IF Frame].
 pose proof(load_world_source b w Load)as Source.
 assert(Missing:nget(st_syms(s_init w))key=None).
 {destruct(nget(st_syms(s_init w))key)as[old|]eqn:E;[|reflexivity].
  pose proof(fr_keep _ _ Frame _ _ E)as Same;rewrite Lookup in Same;injection Same as <-.
  pose proof(Source _ _ E);congruence. }
 pose proof(fr_new_above _ _ Frame _ _ Missing Lookup)as Above.
 destruct(load_world_syms b w Load)as[Positive _]. cbn[s_init st_next]in Above;lia.
Qed.
