(* The whole-function relation follows from the actual completed body trace. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame InfoInvariant BodySound BodyTr
 FnBodyContext FnBodyParameters FnBodyBlock ProviderSig CtxMerge FnHistory InfoCache.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Function.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Theorem completed_loaded_fn_elab:forall fuel g s final body,
 nget(w_syms w)(gs_id g)=Some g->nassoc(gs_id g)(st_fns s)=None->
 lower_body fuel g s=Good(tt,final)->Inv s->InfoInv s->FwdInv s->st_w s=w->
 Later w P R Fl final->Pending w P R Fl->
 nassoc(gs_id g)(st_fns final)=Some body->A.body_of P(gs_id g)=Some body->
 A.FnElab(BodySound.W w R Fl)P(gs_id g).
Proof.
 intros fuel g s final body Loaded Absent Run Inv Cache Fwd Hw Late Pend Emitted Body.
 intros d receiver name parameters result block Decl Ast.
 change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl(gs_id g)g Loaded)in Decl;injection Decl as <-.
 cbn[A.d_ast]in Ast.
 destruct(completed_loaded_body_block w P R Fl b Hload Hworld fuel g s final receiver name parameters result block
 Loaded Ast Absent Run Inv Cache Fwd Hw Late Pend)
 as(info&sg&this&C&F&x&out&Context&Signature&Numbering&Block&Frame&Receiver&Names&ReceiverReading&Output).
 rewrite Emitted in Output;injection Output as <-.
 exists C,(map(key_tr Fl)(fi_needs info)),(option_map(fun t=>tr Fl(canon(fi_env info)t))this),
 (canonical_sig Fl(fi_env info)sg),F,body,out.
 split;[exact Context|];split.
 - destruct receiver;[rewrite ReceiverReading;reflexivity|rewrite ReceiverReading;reflexivity|exact ReceiverReading].
 - split;[exact Signature|].
   assert(Mod:A.fr_mod F=gm_path(get_mod w(gs_mod g)))by(rewrite Frame;reflexivity).
   assert(Ret:A.fr_ret F=tr Fl(canon(fi_env info)(sg_ret sg)))by(rewrite Frame;reflexivity).
   assert(This:A.fr_this F=option_map(fun t=>tr Fl(canon(fi_env info)t))this).
   {rewrite Frame;cbn[initial_body_frame A.fr_this];rewrite Receiver;reflexivity. }
   split;[exact Mod|];split;[exact Ret|];split;[exact This|].
   split;[rewrite Frame;reflexivity|];split;[rewrite Frame;reflexivity|];
   split;[rewrite Frame;reflexivity|];split;[rewrite Frame;reflexivity|].
   split;[rewrite <-Names;exact Numbering|];split;[|exact Body].
   cbn[canonical_sig A.sg_params A.sg_ret];rewrite <-Names,<-body_parameter_types_combine;exact Block.
Qed.
Theorem completed_history_fn_elab:forall observed f body,
 completed_source_fn w observed f body->Later w P R Fl observed->Pending w P R Fl->
 A.body_of P f=Some body->A.FnElab(BodySound.W w R Fl)P f.
Proof.
 intros observed f body(fuel&g&initial&final&Loaded&Id&Inv&Cache&Fwd&Hw&Absent&Run&Lookup&Frame)Late Pend Body.
 subst f;exact(completed_loaded_fn_elab fuel g initial final body Loaded Absent Run Inv Cache Fwd Hw
 (@later_fromL w P R Fl final observed Frame Late)Pend Lookup Body).
Qed.
End Function.
