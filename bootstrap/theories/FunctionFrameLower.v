(* Whole lower_body receipts cover its real own marker/register/body sequence. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv
 FnReadonly SymReadonly FnBound FnBoundEmit FnBoundSynth FnPresentStable FnPresentEmit FnPresentSynth
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderSynth ReifyHeaderPresent
 FnBodyTrace FnBodySlots FnBodyOps ReifyBodyHeader
 LambdaOrigin ReifyFrameReady FunctionFrameHistory FunctionFrameOps.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL fn_info fn_sig].
Lemma completed_lower_frame:forall fuel g block initial final,
 nget(st_syms initial)(gs_id g)=Some g->fn_body g=Some block->nassoc(gs_id g)(st_fns initial)=None->
 gs_id g<st_next initial->Inv initial->lower_body fuel g initial=Good(tt,final)->
 exists body,nassoc(gs_id g)(st_fns final)=Some body/\FunctionFrameOrigin final(gs_id g)body.
Proof.
 intros fuel g block initial final Physical Has Absent Below I Run.
 destruct(completed_body_header _ _ _ _ _ Has Absent Below I Run)
 as(f&info&si&this&sr&sg&ss&marker&registered&x&t&bt&bodyFinal&Facts).
 destruct Facts as(Fuel&Info&Receiver&Signature&Marker&Register&Body&Present&Header).
 exists x;split;[exact Present|];split.
 - exists(gs_origin g);right;right;exists fuel,g,block,initial,final.
   repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
 - eapply function_header_signature_ready;exact Header.
Qed.
Lemma body_receiver_function_rows:forall g e,keeps st_fns(body_receiver g e).
Proof. intros;unfold body_receiver,nominal_args;keepst. Qed.
Lemma finish_body_other_row:forall g e needs sg this x initial final f,
 f<>gs_id g->finish_function_body g e needs sg this x initial=Good(tt,final)->
 nassoc f(st_fns final)=nassoc f(st_fns initial).
Proof.
 intros g e needs sg this x initial final f Other Run.
 unfold finish_function_body in Run.
 apply bind_inv in Run as[u[s1[This Run]]].
 apply bind_inv in Run as[u1[s2[Params Run]]].
 apply bind_inv in Run as[u2[s3[Result Run]]].
 apply bind_inv in Run as[u3[s4[Needs Run]]].
 assert(K1:keeps st_fns(match this with Some t=>layout_ok e t|None=>ret tt end))by keepst.
 assert(K2:keeps st_fns(miter(fun p=>layout_ok e(snd p))(sg_params sg)))by keepst.
 assert(K3:keeps st_fns(layout_ok e(sg_ret sg)))by keepst.
 assert(K4:keeps st_fns(miter(fun k=>match k with KyVal v=>match nget(en_vals e)v with Some t=>layout_ok e t|None=>ret tt end|_=>ret tt end)needs))by keepst.
 unfold emit_fn,upd in Run;injection Run as <-.
 assert(Eq:Nat.eqb f(gs_id g)=false)by(apply Nat.eqb_neq;exact Other).
 cbn[s_fns st_fns nassoc assoc];rewrite Eq.
 rewrite(K4 _ _ _ Needs),(K3 _ _ _ Result),(K2 _ _ _ Params),(K1 _ _ _ This);reflexivity.
Qed.
Lemma ff_lower_body_step:forall fuel,
 (forall c e block expected,FF(synth_block fuel c e block expected))->
 forall g,FF_bound g(lower_body(S fuel)g).
Proof.
 intros fuel Body g;split;[apply hp_lower_body|];split;[apply bf_lower_body_at|].
 intros initial value final I Bound Below Physical Run.
 destruct value;pose proof Run as WholeRun.
 cbn[lower_body]in Run;apply bind_inv in Run as[done[mid[Checked Run]]].
 cbn[lowered mbind get ret]in Checked;injection Checked as <- <-.
 destruct(nassoc(gs_id g)(st_fns initial))as[old|]eqn:Absent.
 {injection Run as <-;intros f body Present Missing;congruence. }
 destruct(fn_body g)as[block|]eqn:Has.
 2:{injection Run as <-;intros f body Present Missing;congruence. }
 destruct(lower_body_stages _ _ _ _ _ Has Absent WholeRun)
 as[f0[info[si[this[sr[sg[ss[marker[registered[x[t[bt[bodyFinal
 [Fuel[Info[Receiver[Signature[Distinct[Marker[Register[Compiled[Finish Emitted]]]]]]]]]]]]]]]]]]]]]].
 injection Fuel as Equal;subst f0.
 destruct(ok_fn_info_frame _ _ _ _ I Info)as[Ii Fi].
 destruct(ok_body_receiver _ _ _ _ _ Ii Receiver)as[Ir Frecv].
 destruct(ok_fn_sig_body _ _ _ _ _ _ _ Ir Signature)as[Is Fs].
 destruct(ok_emit_fn _ _ _ _ _ Is Marker)as[Im Fm].
 destruct(ok_register_body _ _ _ _ _ _ _ Im Register)as[Ireg Freg].
 assert(Prefix:Fr initial registered).
 {eapply fr_trans;[exact Fi|];eapply fr_trans;[exact Frecv|];eapply fr_trans;[exact Fs|];eapply fr_trans;eassumption. }
 assert(Rows:st_fns registered=(gs_id g,XUnit)::st_fns initial).
 {assert(Kreg:keeps st_fns(register_reify_code(ROFunction(gs_id g))
   (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
   (fi_needs info)(Some sg)))by(unfold register_reify_code;keepst).
  rewrite(Kreg _ _ _ Register).
  unfold emit_fn,upd in Marker;injection Marker as <-;cbn[s_fns st_fns].
  rewrite(@kf_fn_sig_input _ _ _ _ _ _ _ Signature),
   (@body_receiver_function_rows _ _ _ _ _ Receiver),(@kf_fn_info_frame _ _ _ _ Info);reflexivity. }
 assert(Breg:FnBound registered).
 {intros key emitted Member;rewrite Rows in Member;destruct Member as[Equal|Member].
  - injection Equal as <- <-;pose proof(fr_next _ _ Prefix);lia.
  - pose proof(Bound _ _ Member);pose proof(fr_next _ _ Prefix);lia. }
 assert(Block:FF(in_reify_code(ROFunction(gs_id g))
  (with_vars(body_parameter_slots this(map fst(sg_params sg)))
    (synth_block fuel(Cx(gs_mod g)sg this 0[]""[][])
      (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))block(Some(sg_ret sg)))))).
 {unfold in_reify_code;fft. }
 destruct(proj1(proj1(proj1 Block))_ _ _ Ireg Compiled)as[Ib Fb].
 destruct(ok_finish_function_body _ _ _ _ _ _ _ _ _ Ib Finish)as[If Ffinish].
 intros f body Present Missing;destruct(Nat.eq_dec f(gs_id g))as[->|Other].
 - destruct(@completed_lower_frame _ _ _ _ _ Physical Has Absent Below I WholeRun)as[own[Own Fact]].
   rewrite Present in Own;injection Own as <-;exact Fact.
 - assert(Middle:nassoc f(st_fns bodyFinal)=Some body).
   {rewrite<-(@finish_body_other_row _ _ _ _ _ _ _ _ _ Other Finish);exact Present. }
   assert(NotRegistered:nassoc f(st_fns registered)=None).
   {unfold nassoc;rewrite Rows;cbn[assoc];assert(Eq:Nat.eqb f(gs_id g)=false)by(apply Nat.eqb_neq;exact Other).
    rewrite Eq;exact Missing. }
   eapply function_frame_origin_transport;[exact Ffinish|
     exact(header_finish_body _ _ _ _ _ _ _ _ _ Finish f)|exact Middle|exact Present|].
   exact(proj2(proj2 Block)_ _ _ Ireg Breg Compiled _ _ Middle NotRegistered).
Qed.
