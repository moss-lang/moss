(* Body soundness uses the actual function scopes and actual emitted body. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 InfoInvariant InfoPreserve BodySound BodyConst BodyTr BodyTy CanonTypes ContextClean NominalSupport
 FnBodyContext FnBodyParameters FnBodyTrace FnBodyScope FnBodyReceiver ProviderSig LoadedContextSupport FnBodyOps.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ok_fn_sig_body:forall g tm e this,ok(fn_sig g tm e this).
Proof. exact FnBodyOps.ok_fn_sig_body. Qed.
Lemma okI_fn_sig_body:forall g tm e this,okI(fn_sig g tm e this).
Proof. intros;unfold fn_sig;okIt. Qed.
Lemma ok_register_body:forall owner e needs sg,ok(register_reify_code owner e needs sg).
Proof. exact FnBodyOps.ok_register_body. Qed.
Lemma okI_register_body:forall owner e needs sg,okI(register_reify_code owner e needs sg).
Proof. intros;unfold register_reify_code;okIt. Qed.
Lemma ok_metadata_body:forall A owner(m:M A),ok m->ok(in_reify_code owner m).
Proof. intros;unfold in_reify_code;okt. Qed.
Section Body.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:ExecutionModel.Elaboration.WorldOf b(BodySound.W w R Fl).
Theorem actual_body_entry:forall g s info si this sr sg ss marker registered f block x t bt bodyFinal final,
 nget(w_syms w)(gs_id g)=Some g->
 fn_info g s=Good(info,si)->body_receiver g(fi_env info)si=Good(this,sr)->
 fn_sig g(tymap(fi_env info))(fi_env info)this sr=Good(sg,ss)->
 emit_fn(gs_id g)XUnit ss=Good(tt,marker)->
 register_reify_code(ROFunction(gs_id g))
 (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
 (fi_needs info)(Some sg)marker=Good(tt,registered)->
 in_reify_code(ROFunction(gs_id g))
 (with_vars(body_parameter_slots this(map fst(sg_params sg)))
 (synth_block f(Cx(gs_mod g)sg this 0[]""[][])
 (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
 block(Some(sg_ret sg))))registered=Good((x,t,bt),bodyFinal)->
 finish_function_body g
 (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
 (fi_needs info)sg this x bodyFinal=Good(tt,final)->
 Inv s->InfoInv s->FwdInv s->st_w s=w->Later w P R Fl final->
 Inv sr/\Fr si sr/\Later w P R Fl ss/\
 (match this with None=>True|Some y=>ety_ok y=true/\Support(BodySound.W w R Fl)y end)/\
 Inv registered/\InfoInv registered/\FwdInv registered/\Later w P R Fl bodyFinal.
Proof.
 intros g s info si this sr sg ss marker registered f block x t bt bodyFinal final
 Loaded Info Receiver Signature Marker Register Body Finish Inv Cache Fwd Hw Late.
 destruct(ok_fn_info_frame _ _ _ _ Inv Info)as[Ii Fi].
 destruct(ok_body_receiver _ _ _ _ _ Ii Receiver)as[Ir Fr].
 destruct(ok_fn_sig_body _ _ _ _ _ _ _ Ir Signature)as[Is Fs].
 destruct(ok_emit_fn _ _ _ _ _ Is Marker)as[Im Fm].
 destruct(ok_register_body _ _ _ _ _ _ _ Im Register)as[Ireg Freg].
 assert(Ob:ok(in_reify_code(ROFunction(gs_id g))
 (with_vars(body_parameter_slots this(map fst(sg_params sg)))
 (synth_block f(Cx(gs_mod g)sg this 0[]""[][])
 (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))block(Some(sg_ret sg)))))).
 {apply ok_metadata_body,ok_with_vars,ok_synth_block. }
 destruct(Ob _ _ _ Ireg Body)as[Ib Fb].
 destruct(ok_finish_function_body _ _ _ _ _ _ _ _ _ Ib Finish)as[_ Ffinish].
 pose proof(later_back w P R Fl _ _ Ffinish Late)as Lbody.
 pose proof(later_back w P R Fl _ _ Fb Lbody)as Lreg.
 pose proof(later_back w P R Fl _ _ Freg Lreg)as Lmark.
 pose proof(later_back w P R Fl _ _ Fm Lmark)as Lsig.
 pose proof(later_back w P R Fl _ _ Fs Lsig)as Lrecv.
 pose proof(later_back w P R Fl _ _ Fr Lrecv)as Linfo.
 assert(Wi:st_w si=w)by(rewrite(fr_w _ _ Fi);exact Hw).
 destruct(fn_info_valid_loaded w(P:=P)R Fl b Hload Hworld _ _ _ _ Loaded Info Inv Cache Hw Linfo)
 as[Clean[Tri Supported]].
 split;[exact Ir|];split;[exact Fr|];split;[exact Lsig|];split.
 - exact(body_receiver_valid w P R Fl b Hload _ _ _ _ _ Receiver Ii Wi Clean Supported Lrecv).
 - split;[exact Ireg|];split.
   + eapply(proj2(okI_register_body _ _ _ _));[exact Im| |exact Register].
     cache_inv.
   + split;[fwd_inv|exact Lbody].
Qed.
End Body.
