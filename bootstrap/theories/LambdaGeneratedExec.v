(* The generated lambda body satisfies the actual block judgment under its
   exact lexical context, at the type it really inferred. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame InfoPreserve BodySound BodyTr
 CanonTypes NominalSupport LambdaTrace LambdaOwnContext LambdaCaptureTables LambdaCapturedLocals
 LambdaCaptureFold LambdaBodyContext LambdaNumbering LambdaGeneratedScope LambdaDeclarations.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma lambda_prefix_nodup:forall X(l r:list X),NoDup(l++r)->NoDup l.
Proof.
 intros X l;induction l as[|x l IH];intros r Unique;[constructor|].
 inversion Unique as[|n ns Absent Tail];subst;constructor.
 - intro Hit;apply Absent;apply in_or_app;left;exact Hit.
 - exact(IH r Tail).
Qed.
Lemma lambda_local_rows_uf:forall rows e,
 en_uf(fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))rows e)=en_uf e.
Proof. intro rows;induction rows as[|[n[t mu]]rows IH];intro e;[reflexivity|cbn[fold_left fst snd];rewrite IH;reflexivity]. Qed.
Lemma lambda_own_env_uf:forall ps sg this le lets,
 en_uf(lambda_own_env ps sg this le lets)=en_uf le.
Proof. intros;rewrite lambda_own_env_rows;rewrite !lambda_local_rows_uf;reflexivity. Qed.
Lemma lambda_checked_block_fits:forall W P F C block expected compiled inferred,
 A.BlkE W P F C block(Some expected)compiled inferred->A.fits inferred expected.
Proof. intros W P F C block expected compiled inferred E;inversion E;subst;assumption. Qed.
Section Body.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Theorem lambda_generated_sound:forall fuel c original F Co le C ps body sg this pid lets registered compiled bodyFinal,
 OK w P R Fl fuel->
 lambda_generated_body fuel c(lambda_own_env ps sg this le lets)ps body sg this pid lets registered=Good(compiled,bodyFinal)->
 Inv registered->InfoInv registered->FwdInv registered->Later w P R Fl bodyFinal->
 CxRel w R Fl c original F Co->CxRel w R Fl c le F C->en_uf le=en_uf original->
 SigOK sg->SigSupport W sg->opt_wf this->opt_support W this->
 Forall(captured_row_available original)lets->NoDup(map fst(lambda_own_slots ps this)++map capture_name lets)->
 exists vars lbody ltype,
 A.Numbering(lambda_body_frame Fl original F sg this vars)(option_map(fun t=>tr Fl(canon original t))this)(map fst ps)/\
 Forall(A.Named(lambda_body_frame Fl original F sg this vars))(map capture_name lets)/\
 A.BlkE W P(lambda_body_frame Fl original F sg this vars)
 (lambda_parameter_model Fl original ps sg(fold_left(capture_local Fl original)lets C))
 (ABlock[](Some body))(Some(tr Fl(canon original(sg_ret sg))))lbody ltype/\
 A.fits ltype(tr Fl(canon original(sg_ret sg)))/\
 compiled=fold_right(fun q x=>XLet(A.var_of(lambda_body_frame Fl original F sg this vars)(fst q))(XVl(snd q))x)lbody
 (combine(map capture_name lets)(map capture_id lets)).
Proof.
 intros fuel c original F Co le C ps body sg this pid lets registered compiled bodyFinal IH Run I Cache Fwd L Horig HC Uf Clean Supported Wthis Sthis Available Names.
 assert(OwnNames:NoDup(map fst(lambda_own_slots ps this)))by(eapply lambda_prefix_nodup;exact Names).
 destruct(lambda_generated_scope w P R Fl fuel c(lambda_own_env ps sg this le lets)ps body sg this pid lets registered compiled bodyFinal
  Run I Cache Fwd L OwnNames)
 as(outer&start&bodyStart&last&declarations&block&out&bt&Start&Decl&Body&Code&Is&Cs&Fs&Ib&Cb&Fb&Ll&Shape&Frame).
 set(Fbody:=lambda_body_frame Fl original F sg this(st_vars last)).
 pose proof(lambda_body_context w R Fl c original F Co le C ps sg this lets(st_vars last)Horig HC Uf Clean Supported Wthis Sthis Available Names)as Context.
 assert(PostBody:Post w P R Fl last Fbody).
 {split;[exact Ll|];intros n slot Lookup;unfold Fbody;cbn[lambda_body_frame A.fr_var];rewrite slook_sassoc;exact Lookup. }
 destruct(ok_synth_block fuel(lambda_own_context c ps sg this)(lambda_own_env ps sg this le lets)(ABlock[](Some body))(Some(sg_ret sg))_ _ _ Ib Body)as[_ BodyFrame].
 destruct(lambda_declarations w P R Fl lets start declarations bodyStart last Fbody Decl Is BodyFrame PostBody)as[Declarations Named].
 destruct(bs_block w P R Fl _ IH _ _ _ _ _ _ _ _ _ Body Fbody _ Ib Cb Fb Context
  (ety_ok_wf _(proj1(proj2 Clean)))(proj1(proj2 Supported))PostBody)as[Block Rest].
 assert(Canon:forall t,canon(lambda_own_env ps sg this le lets)t=canon original t).
 {intro t;unfold canon;rewrite lambda_own_env_uf,Uf;reflexivity. }
 cbn[option_map]in Block;rewrite !Canon in Block.
 exists(st_vars last),block,(tr Fl(canon original out));split.
 - eapply(lambda_numbering_final Fl original F sg ps this start last);[rewrite Start;reflexivity|exact OwnNames|exact Shape|exact Frame].
 - split;[exact Named|];split;[exact Block|];split.
   + eapply lambda_checked_block_fits;exact Block.
   + rewrite Code,Declarations;apply lambda_capture_body_code.
Qed.
End Body.
