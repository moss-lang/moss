(* Capture declarations use the lambda frame's actual allocated local slots. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound
 LambdaCaptureFold LambdaCapturedLocals LambdaGeneratedScope.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition lambda_capture_let(F:A.frame)(q:string*nat*ety):xstmt:=
 let '(n,v,_) :=q in SLet(A.var_of F n)(XVl v).
Section Declarations.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Theorem lambda_declarations:forall lets s declarations sf last F,
 mmap lambda_capture_declaration lets s=Good(declarations,sf)->Inv s->Fr sf last->Post w P R Fl last F->
 declarations=map(lambda_capture_let F)lets/\Forall(A.Named F)(map capture_name lets).
Proof.
 intro lets;induction lets as[|[[n v]t]lets IH];intros s declarations sf last F Run Hi Frame Hp.
 - injection Run as <- <-;split;[reflexivity|constructor].
 - cbn[mmap]in Run;apply bind_inv in Run as(head&sh&Head&Run).
   apply bind_inv in Run as(tail&st&Tail&Run);injection Run as <- <-.
   unfold lambda_capture_declaration in Head;apply bind_inv in Head as(slot&sx&Slot&Head);injection Head as <- <-.
   assert(Ov:ok(local_var n))by okt;destruct(Ov _ _ _ Hi Slot)as[Ix Fx].
   destruct(ok_mmap _ _ lambda_capture_declaration lets ok_lambda_declaration _ _ _ Ix Tail)as[_ Ft].
   destruct(local_named w P R Fl _ _ _ _ _ F Slot(fr_trans _ _ _ Ft Frame)Hp)as[Named Eq].
   destruct(IH _ _ _ _ _ Tail Ix Frame Hp)as[Code Names].
   split;[cbn[map lambda_capture_let];rewrite Code,Eq;reflexivity|constructor;assumption].
Qed.
End Declarations.
Lemma lambda_capture_body_code:forall F lets body,
 fold_stmts(map(lambda_capture_let F)lets)body=
 fold_right(fun q x=>XLet(A.var_of F(fst q))(XVl(snd q))x)body
 (combine(map capture_name lets)(map capture_id lets)).
Proof.
 intros F lets;induction lets as[|[[n v]t]lets IH];intro body;[reflexivity|].
 change(XLet(A.var_of F n)(XVl v)(fold_stmts(map(lambda_capture_let F)lets)body)=
 XLet(A.var_of F n)(XVl v)(fold_right(fun q x=>XLet(A.var_of F(fst q))(XVl(snd q))x)body
 (combine(map capture_name lets)(map capture_id lets)))).
 now rewrite IH.
Qed.
Lemma lambda_capture_pre_code:forall F lets,
 fold_stmts(map(capture_pre F)lets)=
 (fun body=>fold_right(fun q x=>XBindVal(snd q)(XVar(A.var_of F(fst q)))x)body
 (combine(map capture_name lets)(map capture_id lets))).
Proof.
 intros F lets;induction lets as[|[[n v]t]lets IH];[reflexivity|].
 change((fun body=>XBindVal v(XVar(A.var_of F n))(fold_stmts(map(capture_pre F)lets)body))=
 (fun body=>XBindVal v(XVar(A.var_of F n))(fold_right(fun q x=>XBindVal(snd q)(XVar(A.var_of F(fst q)))x)body
 (combine(map capture_name lets)(map capture_id lets))))).
 rewrite IH;reflexivity.
Qed.
