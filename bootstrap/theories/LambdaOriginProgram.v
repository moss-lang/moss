(* Whole elaboration preserves the real origin of every generated code row. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv LoaderOrigin
 FnReadonly SymReadonly FnBound FnBoundEmit FnBoundSynth FnPresentStable FnPresentEmit FnPresentSynth
 ForwarderHistory ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit ReifyHeaderSynth
 ReifyHeaderPresent FnCompleted LambdaOrigin LambdaOriginOps LambdaOriginEmit LambdaOriginSynth
 LambdaCompletion LambdaCompletionOps LambdaCompletionEmit LambdaCompletionSynth LambdaCompletionProgram.
From NM Require Import ProgramTail ProgramTailPreserve.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Definition LHP{A}(m:M A):Prop:=CH m/\HP m/\forall b w s value final,
 load_world b=Good w->Inv s->FnBound s->st_w s=w->m s=Good(value,final)->NewLambdaOrigins s final.
Arguments LHP:simpl never.
Lemma lh_completion:forall A(m:M A),LH m->CH m.
Proof.
 intros A m[H[B New]];split;[exact(proj1 H)|];split;[exact B|].
 intros b w s value final Load I Bound W Run key owner At Origin Missing.
 destruct(New b w _ _ _ Load I Bound W Run _ _ At (or_introl Origin) Missing)as[body[Present Fact]].
 exists body;exact Present.
Qed.
Lemma lhp_of_lh:forall A(m:M A),LH m->LHP m.
Proof.
 intros A m H;split;[apply lh_completion;exact H|];split;[apply hp_of_hs;exact(proj1 H)|exact(proj2(proj2 H))].
Qed.
Lemma lhp_ret:forall A(a:A),LHP(ret a).
Proof. intros;apply lhp_of_lh,lh_ret. Qed.
Lemma lhp_bind:forall A B(m:M A)(k:A->M B),LHP m->(forall a,LHP(k a))->LHP(mbind m k).
Proof.
 intros A B m k[C[P New]]K;split;[apply ch_bind;[exact C|intro;exact(proj1(K a))]|].
 split;[apply hp_bind;[exact P|intro;exact(proj1(proj2(K a)))]|].
 intros b w s value final Load I Bound W Run;apply bind_inv in Run as[a[mid[First Last]]].
 destruct(proj1(proj1 C)_ _ _ I First)as[Im Frame].
 assert(Bmid:FnBound mid)by exact(proj2(proj1(proj2 C))_ _ _ I Bound First).
 assert(Wmid:st_w mid=w)by(rewrite(fr_w _ _ Frame);exact W).
 destruct(proj1(proj1(proj1(K a)))_ _ _ Im Last)as[If Fend].
 eapply new_lambda_origins_trans_present;[exact Im|exact Frame|exact Fend|
 exact(proj2(proj1(proj1(K a)))_ _ _ Im Last)|
 exact(proj2(proj1(proj2(K a)))_ _ _ Im Last)|
 exact(New b w _ _ _ Load I Bound W First)|
 exact(proj2(proj2(K a))b w _ _ _ Load Im Bmid Wmid Last)].
Qed.
Lemma lhp_miter:forall A(f:A->M unit)xs,(forall x,LHP(f x))->LHP(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply lhp_ret|apply lhp_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma lhp_lower_one:forall fuel id,LHP(lower_one fuel id).
Proof.
 intros fuel id;split.
 - unfold lower_one, LowerOne.lower_one;apply ch_bind_symbol;intros g Id.
   destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));split;
   first[apply pf_lower_body|apply ch_lower_body|apply pf_ret|apply ch_at_of_ch,ch_ret].
 - split;[apply hp_lower_one|].
   intros b w s value final Load I Bound W Run;unfold lower_one, LowerOne.lower_one in Run.
   apply bind_inv in Run as[g[mid[Read Run]]];unfold sym in Read.
   destruct(nget(st_syms s)id)as[got|]eqn:At;[injection Read as <- <-|discriminate].
   destruct(is_some(fn_body got)&&(kind_eqb(gs_kind got)KFn||kind_eqb(gs_kind got)KMethod));
   [|injection Run as <- <-;apply new_lambda_origins_same_symbols;reflexivity].
   assert(Below:gs_id got<st_next s)by(rewrite(iv_ids s I id got At);exact(@symbol_below_next s id got I At)).
   exact(proj2(proj2(lh_lower_body fuel got))b w _ _ _ Load I Bound W Below Run).
Qed.
Lemma lhp_lower_all:LHP lower_all.
Proof.
 split;[apply ch_lower_all|];split;[apply hp_lower_all|].
 intros b w s value final Load I Bound W Run;unfold lower_all in Run.
 apply bind_inv in Run as[snapshot[mid[Read Run]]];injection Read as <- <-.
 assert(Loop:LHP(miter(fun mi=>miter(lower_one ELAB_FUEL)(module_function_ids(get_mod(st_w s)mi)))(w_order(st_w s)))).
 {apply lhp_miter;intro;apply lhp_miter;intro;apply lhp_lower_one. }
 exact(proj2(proj2 Loop)b w _ _ _ Load I Bound W Run).
Qed.
Lemma lhp_keeps:forall A(m:M A),ok m->keeps st_fns m->keeps st_syms m->keeps function_headers m->LHP m.
Proof. intros;apply lhp_of_lh,lh_keeps;assumption. Qed.
From NM Require Import ReifyHeaderProgram.
#[local]Hint Resolve LambdaCompletionProgram.kf_entry_zero_width_history
 LambdaCompletionProgram.ks_entry_zero_width_history
 LambdaCompletionProgram.kf_check_main_signature_history
 LambdaCompletionProgram.ks_check_main_signature_history
 ReifyHeaderProgram.kh_entry_zero_width ReifyHeaderProgram.kh_check_main_signature:keepsdb.
(* Share this leaf proof across all five declaration kinds in check_assumes. *)
Local Lemma lh_check_assumed_requirements:forall mi e k,LH(check_assumed_requirements mi e k).
Proof. intros;apply lh_keeps;[apply ok_check_assumed_requirements_frame|unfold check_assumed_requirements;keepst|unfold check_assumed_requirements;keepst|unfold check_assumed_requirements;keepst]. Qed.
#[local]Hint Resolve lh_check_assumed_requirements:lhdb.
Lemma lhp_elab_program:LHP elab_program.
Proof.
 unfold elab_program.
 apply lhp_bind;[apply lhp_keeps;[okt|keepst|keepst|keepst]|intro u1].
 apply lhp_bind;[apply lhp_keeps;[okt|keepst|keepst|keepst]|intro u2].
 apply lhp_bind;[apply lhp_keeps;[okt|keepst|keepst|keepst]|intro u3].
 apply lhp_bind;[apply lhp_lower_all|intro u4].
 change (LHP program_tail).
 apply lhp_of_lh;exact program_tail_lh.
Qed.
From NM Require Import FnCoverage.
Theorem actual_final_generated_origins:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 forall key owner,nget(st_syms final)key=Some owner->GeneratedCode owner->
 exists body,nassoc key(st_fns final)=Some body/\LambdaOrigin key body(function_header final key)(gs_origin owner).
Proof.
 intros b w p final Load Run key owner At Origin.
 assert(I:Inv(s_init w))by exact(@fn_loaded_initial_inv b w Load).
 destruct(proj1(proj1(proj1 lhp_elab_program))_ _ _ I Run)as[If Frame].
 eapply(proj2(proj2 lhp_elab_program)b w _ _ _ Load I(@fn_bound_init w)eq_refl Run _ _ At Origin).
 destruct(nget(st_syms(s_init w))key)as[old|]eqn:Initial;[|reflexivity].
 pose proof(fr_keep _ _ Frame _ _ Initial)as Kept;rewrite At in Kept;injection Kept as <-.
 change(nget(w_syms w)key=Some owner)in Initial.
 unfold GeneratedCode in Origin;rewrite(load_world_source b w Load _ _ Initial)in Origin.
 destruct Origin as[Bad|[target Bad]];discriminate.
Qed.
Theorem actual_final_lambda_origins:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 forall key owner,nget(st_syms final)key=Some owner->gs_origin owner=OLambda->
 exists body,nassoc key(st_fns final)=Some body/\LambdaOrigin key body(function_header final key)(gs_origin owner).
Proof. intros;eapply actual_final_generated_origins;[eassumption|eassumption|eassumption|left;assumption]. Qed.
