(* A completed body retains the actual scope and signature registered for it. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv
 FnReadonly ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderSynth
 FnBodyParameters FnBodyTrace FnBodyReceiver FnBodyExec.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL fn_info fn_sig nominal_args].
Lemma header_finish_body:forall g e needs sg this x,
 header_keeps(finish_function_body g e needs sg this x).
Proof.
 intros;apply header_keeps_of_rows;unfold finish_function_body,emit_fn,layout_ok;keepst.
Qed.
Theorem completed_body_header:forall fuel g s final block,
 fn_body g=Some block->nassoc(gs_id g)(st_fns s)=None->
 gs_id g<st_next s->Inv s->lower_body fuel g s=Good(tt,final)->
 exists f info si this sr sg ss marker registered x t bt bodyFinal,
 fuel=S f/\fn_info g s=Good(info,si)/\
 body_receiver g(fi_env info)si=Good(this,sr)/\
 fn_sig g(tymap(fi_env info))(fi_env info)this sr=Good(sg,ss)/\
 emit_fn(gs_id g)XUnit ss=Good(tt,marker)/\
 register_reify_code(ROFunction(gs_id g))
  (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
  (fi_needs info)(Some sg)marker=Good(tt,registered)/\
 in_reify_code(ROFunction(gs_id g))
  (with_vars(body_parameter_slots this(map fst(sg_params sg)))
   (synth_block f(Cx(gs_mod g)sg this 0[]""[][])
    (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
    block(Some(sg_ret sg))))registered=Good((x,t,bt),bodyFinal)/\
 nassoc(gs_id g)(st_fns final)=Some x/\
 function_header final(gs_id g)=Some(ROFunction(gs_id g),
 reify_scope_of(fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info)),
 fi_needs info,
 Some(reify_sig(fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))sg)).
Proof.
 intros fuel g s final block HasBody Absent Bound I Run.
 destruct(lower_body_stages _ _ _ _ _ HasBody Absent Run)
 as[f[info[si[this[sr[sg[ss[marker[registered[x[t[bt[bodyFinal
 [Fuel[Info[Receiver[Signature[Distinct[Marker[Register[Body[Finish Emitted]]]]]]]]]]]]]]]]]]]]]].
 destruct(ok_fn_info_frame _ _ _ _ I Info)as[Ii Fi].
 destruct(ok_body_receiver _ _ _ _ _ Ii Receiver)as[Ir Fr].
 destruct(ok_fn_sig_body _ _ _ _ _ _ _ Ir Signature)as[Is Fs].
 destruct(ok_emit_fn _ _ _ _ _ Is Marker)as[Im Fm].
 destruct(ok_register_body _ _ _ _ _ _ _ Im Register)as[Ireg Freg].
 assert(Breg:gs_id g<st_next registered).
 {pose proof(fr_next _ _ Fi);pose proof(fr_next _ _ Fr);pose proof(fr_next _ _ Fs);
  pose proof(fr_next _ _ Fm);pose proof(fr_next _ _ Freg);lia. }
 assert(Protected:header_protected registered(gs_id g)).
 {left;exists XUnit;unfold register_reify_code,upd in Register;injection Register as <-.
  unfold emit_fn,upd in Marker;injection Marker as <-.
  cbn[s_reify st_fns s_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity. }
 pose proof(proj2(hs_in_reify_code _ (hs_with_vars _ (hs_synth_block f _ _ _ _)))
  _ _ _ Ireg Body (gs_id g) Breg Protected)as Stable.
 exists f,info,si,this,sr,sg,ss,marker,registered,x,t,bt,bodyFinal.
 repeat split;try assumption.
 rewrite(header_finish_body _ _ _ _ _ _ _ _ _ Finish (gs_id g)),Stable.
 exact(register_function_header _ _ _ _ _ _ Register).
Qed.
