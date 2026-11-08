(* A real completed loaded-body run satisfies the block judgment. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame InfoInvariant InfoPreserve
 BodySound CtxWalk CtxMerge BodyTr BodyTy CanonTypes ContextClean NominalSupport FnBodyContext FnBodyParameters
 FnBodyTrace FnBodyScope FnBodyReceiver FnBodyExec ProviderSig LibraryFacts ForwarderSlots FnBodyAttached.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Section Body.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Theorem completed_loaded_body_block:forall fuel g s final receiver name parameters result block,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn receiver name parameters result(Some block)->
 nassoc(gs_id g)(st_fns s)=None->lower_body fuel g s=Good(tt,final)->
 Inv s->InfoInv s->FwdInv s->st_w s=w->Later w P R Fl final->Pending w P R Fl->
 exists info sg this C F x out,
 A.CtxOf(BodySound.W w R Fl)(gs_id g)C(map(key_tr Fl)(fi_needs info))/\
 A.SigOf(BodySound.W w R Fl)(gs_id g)(A.c_ty C)
 (option_map(fun t=>tr Fl(canon(fi_env info)t))this)(canonical_sig Fl(fi_env info)sg)/\
 A.Numbering F(option_map(fun t=>tr Fl(canon(fi_env info)t))this)(map fst(sg_params sg))/\
 A.BlkE(BodySound.W w R Fl)P F
 (fold_left(fun acc p=>A.add_local acc(fst p)(snd p)A.LLet)(body_parameter_types Fl(fi_env info)(sg_params sg))C)
 block(Some(tr Fl(canon(fi_env info)(sg_ret sg))))x out/\
 F=initial_body_frame w Fl g(fi_env info)sg(A.fr_var F)/\sg_this sg=this/\
 map fst(sg_params sg)=map fst parameters/\
 (match receiver with RAttached _=>exists r args,A.Attached(BodySound.W w R Fl)r name(gs_id g)/\
  option_map(fun y=>tr Fl(canon(fi_env info)y))this=Some(TNom r args)/\
  Forall2(fun key q=>fst q=key/\A.nlook key(A.c_ty C)=Some(snd q))(R r)args
 | _=>this=None end)/\nassoc(gs_id g)(st_fns final)=Some x.
Proof.
 intros fuel g s final receiver name parameters result block Loaded Decl Absent Run Inv Cache Fwd Hw Late Pend.
 assert(HasBody:fn_body g=Some block)by(unfold fn_body;rewrite Decl;reflexivity).
 destruct(lower_body_stages _ _ _ _ _ HasBody Absent Run)
 as[f[info[si[this[sr[sg[ss[marker[registered[x[t[bt[bodyFinal
 [Fuel[Info[Receiver[Signature[Distinct[Marker[Register[Body[Finish Emitted]]]]]]]]]]]]]]]]]]]]]].
 destruct(actual_body_entry w P R Fl b Hload Hworld _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
 Loaded Info Receiver Signature Marker Register Body Finish Inv Cache Fwd Hw Late)
 as(Ir&Fr&Lsig&This&Ireg&Creg&Freg&Lbody).
 destruct(in_reify_checked_scope w P R Fl _ _ _ _ _ _ Ireg Creg Freg Body Lbody)
 as(outer&lastOuter&VarsRun&Iouter&Couter&Fouter&Louter&Vouter&Nouter&Wouter).
 destruct(with_vars_checked_scope w P R Fl _ _ _ _ _ _ Iouter Couter Fouter
 (ok_synth_block _ _ _ _ _)VarsRun Louter(body_parameter_shape _ _ Distinct outer))
 as(last&BodyRun&Istart&Cstart&Fstart&Llast&Shape&Frame).
 set(F:=initial_body_frame w Fl g(fi_env info)sg(st_vars last)).
 destruct(loaded_function_body_context w(P:=P)R Fl b Hload Hworld g s info si sr sg ss this(st_vars last)
 receiver name parameters result(Some block)Loaded Decl Info Signature Inv Cache Ir Fr Hw Lsig This)
 as[C[Context[SignatureOf[Relation[ReceiverOf[SigClean SigSupported]]]]]].
 destruct(ok_fn_info_frame _ _ _ _ Inv Info)as[Ii Fi].
 assert(Wi:st_w si=w)by(rewrite(fr_w _ _ Fi);exact Hw).
 destruct(ok_fn_sig_body _ _ _ _ _ _ _ Ir Signature)as[_ FrameSig].
 pose proof(later_back w P R Fl _ _ FrameSig Lsig)as Lrecv.
 pose proof(body_receiver_reading w P R Fl b Hload g receiver name parameters result(Some block)
 _ _ _ _ _ _ _ Loaded Decl Receiver Ii Wi Relation Lrecv)as ReceiverReading.
 pose proof(proj1 SigClean)as ParamsClean;rewrite Forall_map in ParamsClean.
 pose proof(body_parameter_context w R Fl _ _ F C(sg_params sg)Relation ParamsClean(proj1 SigSupported))as LocalContext.
 unfold initial_body_context in LocalContext;rewrite ReceiverOf in LocalContext.
 assert(PostBody:Post w P R Fl last F).
 {split;[exact Llast|];intros n slot At;unfold F;cbn[initial_body_frame A.fr_var];rewrite slook_sassoc;exact At. }
 assert(Expected:opt_wf(Some(sg_ret sg))).
 {apply ety_ok_wf;exact(proj1(proj2 SigClean)). }
 assert(ExpectedSupport:opt_support(BodySound.W w R Fl)(Some(sg_ret sg)))by exact(proj1(proj2 SigSupported)).
 destruct(bs_block w P R Fl _ (bodies_sound w P R Fl b Hload (fun bi x=>lang_symbol_scope b w R Fl Hload "bool" false "Bool" bi x) Pend f)
 _ _ _ _ _ _ _ _ _ BodyRun F _ Istart Cstart Fstart LocalContext Expected ExpectedSupport PostBody)
 as[Block Rest].
 cbn[option_map]in Block;rewrite !body_parameter_canon in Block.
 exists info,sg,this,C,F,x,(tr Fl(canon(fi_env info)t));split;[exact Context|];split;[exact SignatureOf|];split.
 - unfold F;eapply parameter_numbering_final with(initial:=s_vars(body_parameter_slots this(map fst(sg_params sg)))(List.length(body_parameter_slots this(map fst(sg_params sg))))outer);[reflexivity|exact Distinct|exact Shape|exact Frame].
 - split;[exact Block|];split;[reflexivity|];split;[exact ReceiverOf|];split.
   + pose proof(fn_sig_names _ _ _ _ _ _ _ Signature)as Names.
     unfold fn_params in Names;rewrite Decl in Names;exact Names.
   + split;[exact ReceiverReading|exact Emitted].
Qed.
End Body.
