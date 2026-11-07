(* Actual capture declarations and the lambda body inside their fresh variable
   scope retain all execution invariants needed by the body induction. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv
 InfoInvariant InfoPreserve FwdInvariant FwdReadonly FwdPreserve
 BodySoundCore FnBodyScope LambdaTrace LambdaNumbering.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition lambda_capture_declaration(q:string*nat*ety):M xstmt:=
 let '(n,v,_) :=q in let!slot:=local_var n in ret(SLet slot(XVl v)).
Lemma ok_lambda_declaration:forall q,ok(lambda_capture_declaration q).
Proof. intros[[n v]t];unfold lambda_capture_declaration;okt. Qed.
Lemma cache_lambda_declaration:forall q,okI(lambda_capture_declaration q).
Proof.
 intro q;apply okI_ni;[apply ok_lambda_declaration| |].
 - destruct q as[[n v]t];unfold lambda_capture_declaration,local_var;nit.
 - destruct q as[[n v]t];unfold lambda_capture_declaration,local_var;keepst.
Qed.
Lemma fwd_lambda_declaration:forall q,okF(lambda_capture_declaration q).
Proof.
 intro q;apply okF_readonly;[apply ok_lambda_declaration|].
 destruct q as[[n v]t];unfold lambda_capture_declaration,local_var;keepst.
Qed.
Section Scope.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Theorem lambda_generated_scope:forall fuel c en ps body sg this pid lets registered compiled bodyFinal,
 lambda_generated_body fuel c en ps body sg this pid lets registered=Good(compiled,bodyFinal)->
 Inv registered->InfoInv registered->FwdInv registered->Later w P R Fl bodyFinal->
 NoDup(map fst(lambda_own_slots ps this))->
 exists outer start bodyStart last declarations block out bt,
 start=s_vars(lambda_own_slots ps this)(List.length(lambda_own_slots ps this))outer/\
 mmap lambda_capture_declaration lets start=Good(declarations,bodyStart)/\
 synth_block fuel(lambda_own_context c ps sg this)en(ABlock[](Some body))(Some(sg_ret sg))bodyStart=Good((block,out,bt),last)/\
 compiled=fold_stmts declarations block/\
 Inv start/\InfoInv start/\FwdInv start/\
 Inv bodyStart/\InfoInv bodyStart/\FwdInv bodyStart/\
 Later w P R Fl last/\VShape last/\Fr start last.
Proof.
 intros fuel c en ps body sg this pid lets registered compiled bodyFinal Run I Cache Fwd L Names.
 unfold lambda_generated_body in Run.
 destruct(in_reify_checked_scope w P R Fl _ _ _ _ _ _ I Cache Fwd Run L)
 as(outer&varsFinal&VarsRun&Io&Co&Fo&Lo&Vo&No&Wo).
 match type of VarsRun with with_vars _ ?m _ = _=>assert(Om:ok m)
 by(apply ok_bind;[apply ok_mmap;apply ok_lambda_declaration|intro];
 apply ok_bind;[apply ok_synth_block|intros[[x t]bt];apply ok_ret])end.
 destruct(with_vars_checked_scope w P R Fl _ _ _ _ _ _ Io Co Fo Om VarsRun Lo(lambda_slots_shape ps this outer Names))
 as(last&Inside&Is&Cs&Fs&Ll&Shape&Frame).
 apply bind_inv in Inside as(declarations&bodyStart&Decl&Inside).
 change(mmap lambda_capture_declaration lets(s_vars(lambda_own_slots ps this)(List.length(lambda_own_slots ps this))outer)=Good(declarations,bodyStart))in Decl.
 apply bind_inv in Inside as([[block out]bt]&bodyLast&Body&Inside);injection Inside as <- <-.
 destruct(ok_mmap _ _ lambda_capture_declaration lets ok_lambda_declaration _ _ _ Is Decl)as[Ib Fb].
 assert(Cb:InfoInv bodyStart)by exact(proj2(okI_mmap _ _ lambda_capture_declaration lets cache_lambda_declaration)_ _ _ Is Cs Decl).
 assert(Fbwd:FwdInv bodyStart)by exact(proj2(okF_mmap lambda_capture_declaration lets fwd_lambda_declaration)_ _ _ Is Fs Decl).
 exists outer,(s_vars(lambda_own_slots ps this)(List.length(lambda_own_slots ps this))outer),bodyStart,bodyLast,declarations,block,out,bt.
 split;[reflexivity|];split;[exact Decl|];split;[exact Body|];split;[reflexivity|].
 split;[exact Is|];split;[exact Cs|];split;[exact Fs|];split;[exact Ib|];
 split;[exact Cb|];split;[exact Fbwd|];split;[exact Ll|];split;assumption.
Qed.
End Scope.
