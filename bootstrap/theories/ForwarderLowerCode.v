(* The real lower_body miss for a generated forwarder emits its exact call. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 ForwarderArgsCode ForwarderBlockCode ForwarderSlots ForwarderHome ForwarderCodeTrace.
Import ListNotations.
Lemma ok_forwarder_fn_sig:forall g tm e this,ok(fn_sig g tm e this).
Proof. intros;unfold fn_sig;okt. Qed.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements fn_info fn_sig].
Lemma parameter_combined_names:forall(names:list string)(slots:list nat),List.length names=List.length slots->map fst(combine names slots)=names.
Proof. induction names;intros[|slot slots]Len;cbn[List.length]in Len;try discriminate;[reflexivity|]. cbn[combine map fst];f_equal;apply IHnames;lia. Qed.
Fixpoint parameter_duplicate(names:list string):option string:=
 match names with[]=>None|n::rest=>if smem n rest then Some n else parameter_duplicate rest end.
Lemma parameter_duplicate_none:forall names,parameter_duplicate names=None->NoDup names.
Proof.
 induction names as[|n names IH];intro E;[constructor|]. cbn[parameter_duplicate]in E.
 destruct(smem n names)eqn:M;[discriminate|]. constructor;[exact(smem_false _ _ M)|apply IH;exact E].
Qed.
Ltac lower_code_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Theorem lower_forwarder_code:forall b w fuel g target tn targetps targetrt pn params rt names s final,
 load_world b=Good w->nget(w_syms w)(gs_id target)=Some target->
 gs_decl target=ADFn RPlain tn targetps targetrt None->gs_kind target=KFn->
 gs_decl g=ADFn RPlain pn params rt(Some(ABlock[](Some(AECall[tn]None(map(fun n=>AEPath[n]None)names)))))->
 map fst params=names->gs_mod g=gs_mod target->gs_recv g=None->
 Inv s->st_w s=w->nassoc(gs_id g)(st_fns s)=None->
 lower_body fuel g s=Good(tt,final)->
 nassoc(gs_id g)(st_fns final)=Some(XCallA(gs_id target)(map XVar(seq 0(List.length params)))).
Proof.
 intros b w[|f]g target tn targetps targetrt pn params rt names s final Load Loaded TargetDecl TargetKind
 Decl ParamNames Home Receiver I World Absent E;[discriminate|].
 cbn[lower_body]in E;apply bind_inv in E as[done[s0[Lowered E]]].
 cbn[lowered mbind get ret]in Lowered;rewrite Absent in Lowered;cbn[is_some]in Lowered;injection Lowered as <- <-.
 unfold fn_body in E;rewrite Decl in E;cbn in E.
 apply bind_inv in E as[info[sInfo[Info E]]].
 destruct(ok_fn_info_frame _ _ _ _ I Info)as[II FI].
 rewrite Receiver in E;cbn[mbind ret]in E.
 apply bind_inv in E as[sg[sSig[Sig E]]].
 assert(Osig:ok(fn_sig g(tymap(fi_env info))(fi_env info)None))by apply ok_forwarder_fn_sig.
 destruct(Osig _ _ _ II Sig)as[IS FS].
 pose proof(fn_sig_names _ _ _ _ _ _ _ Sig)as SigNames.
 unfold fn_params in SigNames;rewrite Decl in SigNames;cbn in SigNames;rewrite ParamNames in SigNames.
 cbn[List.app List.length]in E.
 set(vars:=combine(map fst(sg_params sg))(seq 0(List.length(sg_params sg))))in *.
 fold parameter_duplicate in E.

 destruct(parameter_duplicate(map fst vars))as[bad|]eqn:Dup;[lower_code_noerr E|].
 pose proof(parameter_duplicate_none _ Dup)as NdVars.
 assert(LenNames:List.length(map fst(sg_params sg))=List.length(seq 0(List.length(sg_params sg)))).
 {rewrite map_length,seq_length;reflexivity. }
 assert(VarNames:map fst vars=map fst(sg_params sg)).
 {unfold vars;apply parameter_combined_names;exact LenNames. }
 rewrite VarNames in NdVars.
 apply bind_inv in E as[u[sMark[Marker E]]].
 destruct(ok_emit_fn(gs_id g)XUnit _ _ _ IS Marker)as[IM FM].
 apply bind_inv in E as[u'[sReg[Register E]]].
 assert(Oreg:ok(register_reify_code(ROFunction(gs_id g))
   (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))(fi_needs info)(Some sg)))by(unfold register_reify_code;okt).
 destruct(Oreg _ _ _ IM Register)as[IR FR].
 apply bind_inv in E as[[[body tty]bty][sBody[Body E]]].
 destruct(@in_reify_vars_body_trace _ (ROFunction(gs_id g))vars _ sReg (body,tty,bty)sBody IR Body)
 as[start[last[BodyRun[IB[WB[SB[VB NB]]]]]]].
 assert(Slots:LocalSlots(fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
 names(seq 0(List.length(sg_params sg)))start).
 {rewrite <-SigNames. eapply parameter_local_slots;[reflexivity|exact NdVars|exact LenNames|exact VB]. }
 assert(Wreg:st_w sReg=w).
 {rewrite(fr_w _ _ FR),(fr_w _ _ FM),(fr_w _ _ FS),(fr_w _ _ FI);exact World. }
 assert(Wstart:st_w start=w)by(rewrite WB;exact Wreg).
 assert(Lookup:nget(st_syms start)(gs_id target)=Some target)by(eapply iv_loader;[exact IB|rewrite Wstart;exact Load|rewrite Wstart;exact Loaded]).
 assert(Resolve:rpath(gs_mod g)[tn]start=Good(Some(ESym(gs_id target)),start)).
 {rewrite Home;eapply loaded_plain_home_rpath;eassumption. }
 assert(Code:body=XCallA(gs_id target)(map XVar(seq 0(List.length(sg_params sg))))).
 {eapply synth_forwarder_block_code with(c:=Cx(gs_mod g)sg None 0 [] ""%string [] [])(g:=target);
 [exact Slots|exact IB|exact Resolve|exact Lookup|exact TargetKind|unfold fn_body;rewrite TargetDecl;reflexivity|exact BodyRun]. }
 rewrite Code in E.
 assert(Count:List.length(sg_params sg)=List.length params).
 {apply(f_equal(@List.length string))in SigNames;rewrite map_length,<-ParamNames,map_length in SigNames;exact SigNames. }
 rewrite Count in E.
 (* Everything after the body is validation; the final action emits it. *)
 repeat apply bind_inv in E as[?[?[? E]]].
 unfold emit_fn,upd in E;injection E as <-;cbn[s_fns st_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
Qed.
