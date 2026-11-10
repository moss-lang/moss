(* Actual source function headers survive the entire successful elaboration. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 InfoInvariant InfoPreserve ProgramInfo FwdInvariant FwdPreserve FwdReadonly FwdSynthInv FwdProgramInv
 FnReadonly FnHistory FnStable FnEmitStable FnSynthStable FnCompleted
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderPresent ReifyHeaderHistory.
From NM Require Import ProgramTail.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
#[local]Hint Resolve kw_entry_zero_width_present kw_check_main_signature_present
 kf_entry_zero_width kf_check_main_signature:keepsdb.
Lemma kh_entry_zero_width:forall fuel e t,keeps function_headers(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kh_entry_zero_width:keepsdb.
Lemma kh_check_main_signature:forall g info,keeps function_headers(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kh_check_main_signature:keepsdb.
Lemma hh_lower_one:forall fuel id,HH(lower_one fuel id).
Proof.
 intros fuel id;split;[exact(proj1(fh_lower_one fuel id))|].
 intros b s value final I Cache Fwd Load Run;unfold lower_one, LowerOne.lower_one in Run.
 apply bind_inv in Run as[g[mid[Read Run]]].
 unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:At;[|discriminate].
 injection Read as <- <-.
 destruct(is_some(fn_body got)&&(kind_eqb(gs_kind got)KFn||kind_eqb(gs_kind got)KMethod)).
 - assert(Id:gs_id got=id)by exact(iv_ids s I id got At).
   destruct(nget(w_syms(st_w s))id)as[original|]eqn:Loaded.
   + pose proof(iv_loader s I b Load id original Loaded)as Same;rewrite At in Same;injection Same as <-.
     destruct value;eapply hh_lower_body_loaded;try eassumption;rewrite Id;exact Loaded.
   + intros f body Key Present.
     assert(Old:nassoc f(st_fns s)=Some body).
     {rewrite <-(proj2(nf_lower_body_unloaded fuel got)b s value final I Load ltac:(intros[owner Entry];rewrite Id,Loaded in Entry;discriminate)Run f Key);exact Present. }
     left;split;[exact Old|].
     exact(proj2(hp_lower_body fuel got)_ _ _ I Run f body(@loaded_fn_below_next b s f Load I Key)Old).
 - injection Run as <- <-;apply source_header_refl.
Qed.
Lemma hh_lower_all:HH lower_all.
Proof.
 split;[exact(proj1 fh_lower_all)|].
 intros b s value final I Cache Fwd Load Run;unfold lower_all in Run.
 apply bind_inv in Run as[s0[mid[Read Run]]];injection Read as <- <-.
 assert(Fold:HH(miter(fun mi=>miter(lower_one ELAB_FUEL)(module_function_ids(get_mod(st_w s)mi)))(w_order(st_w s)))).
 {apply hh_miter;[intro mi;apply hh_miter;[intro id;apply hh_lower_one|intro id;unfold lower_one, LowerOne.lower_one;fwt]|intro mi;apply okF_miter;intro id;unfold lower_one, LowerOne.lower_one;fwt]. }
 exact(proj2 Fold b s value final I Cache Fwd Load Run).
Qed.
Lemma hh_keeps:forall A(m:M A),okI m->keeps st_fns m->keeps function_headers m->HH m.
Proof.
 intros A m OK Functions Headers;split;[exact OK|].
 intros b s value final I Cache Fwd Load Run f body Key Present;left;split.
 - rewrite <-(Functions _ _ _ Run);exact Present.
 - repeat rewrite function_header_lookup;rewrite(Headers _ _ _ Run);reflexivity.
Qed.
Lemma hh_elab_program:HH elab_program.
Proof.
 unfold elab_program.
 apply hh_bind;[apply hh_keeps;[program_info|keepst|keepst]| |intro u1].
 1:history_fwd.
 apply hh_bind;[apply hh_keeps;[program_info|keepst|keepst]| |intro u2].
 1:history_fwd.
 apply hh_bind;[apply hh_keeps;[program_info|keepst|keepst]| |intro u3].
 1:history_fwd.
 apply hh_bind;[apply hh_lower_all|apply okF_lower_all|intro u4].
 change (HH program_tail).
 apply hh_keeps;[exact program_tail_info|exact program_tail_functions|exact program_tail_headers].
Qed.
Theorem elab_completed_source_headers:forall b w p final,
 load_world b=Good w->Inv(s_init w)->elab_program(s_init w)=Good(p,final)->
 forall f body,loaded_fn_key w f->nassoc f(ExecutionModel.xo_fns p)=Some body->
 completed_source_header w final f body.
Proof.
 intros b w p final Load I Run f body Loaded Present.
 pose proof(proj2 hh_elab_program b(s_init w)p final I(info_init w)(@fwd_inv_init w)Load Run)as Hist.
 rewrite(@elab_program_tables _ _ _ Run)in Present.
 destruct(Hist _ _ Loaded Present)as[[Impossible Header]|Done];[discriminate Impossible|exact Done].
Qed.
From NM Require Import FnCoverage.
Theorem accepted_defined_source_headers:forall b p final,
 elaborate_with_metadata b=Good(p,final)->exists w,
 load_world b=Good w/\elab_program(s_init w)=Good(p,final)/\
 forall id g receiver name parameters result source_body,
 nget(w_syms w)id=Some g->gs_decl g=ADFn receiver name parameters result(Some source_body)->
 exists lowered,nassoc id(ExecutionModel.xo_fns p)=Some lowered/\completed_source_header w final id lowered.
Proof.
 intros b p final Accepted0;destruct(elaborate_with_metadata_run b p final Accepted0)as(w&Load&Accepted).
 change(elab_program(s_init w)=Good(p,final))in Accepted.
 exists w;split;[exact Load|];split;[exact Accepted|].
 intros id g receiver name parameters result source_body Loaded Decl.
 pose proof(fn_loaded_initial_inv b w Load)as I.
 destruct(elab_program_defined_bodies_present b(s_init w)p final id g receiver name parameters result source_body
  I Load Accepted Loaded Decl)as[lowered Present].
 exists lowered;split;[exact Present|].
 eapply(@elab_completed_source_headers b w p final Load I Accepted id lowered);[exists g;exact Loaded|exact Present].
Qed.
