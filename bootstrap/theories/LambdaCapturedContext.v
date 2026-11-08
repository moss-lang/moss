(* The actual capture filter and allocation fold form the exact lexical
   context requested by LP, including every fresh capture slot. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame BodySoundCore
 LambdaTrace LambdaCaptureNames LambdaCaptureContext LambdaCaptureFold LambdaCaptureTables LambdaCapturedLocals.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma captured_names_rows:forall qs rows,Forall2 capture_row qs rows->
 map capture_name rows=map fst qs.
Proof.
 intros qs rows Rel;induction Rel as[|[n[t mu]][[rn v]rt]qs rows Pair Rel IH];[reflexivity|].
 cbn[capture_row]in Pair;destruct Pair as[<- <-].
 change(n::map capture_name rows=n::map fst qs);now rewrite IH.
Qed.
Lemma lambda_source_capture_rows:forall c original ps body,
 NoDup(map fst(en_locals original))->
 Forall(fun q=>sassoc(fst q)(en_locals original)=Some(snd q))(lambda_captured c original ps body).
Proof.
 intros c original ps body Unique;apply Forall_forall;intros[n[t mu]]Hit;cbn[fst snd].
 unfold lambda_captured in Hit;destruct(cx_applying c);[|contradiction].
 apply filter_In in Hit as[Hit Test];exact(capture_local_lookup _ _ _ _ Unique Hit).
Qed.
Section Context.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem lambda_captured_context:forall c original F Co ps body s le pre lets sf bound,
 mfold(lambda_capture_one c original)(with_locals original[],[],[])(lambda_captured c original ps body)s=Good((le,pre,lets),sf)->
 Inv s->st_w s=w->nget(w_syms w)(gs_id bound)=Some bound->CxRel w R Fl c original F Co->Post w P R Fl sf F->
 exists Cl,
 (forall n,In n(map capture_name lets)<->A.Mentions_name body n/\A.slook n(A.c_locals Co)<>None/\~In n(map fst ps)/\A.fr_applying F=[])/\
 NoDup(map capture_name lets)/\Forall(A.Fresh W)(map capture_id lets)/\NoDup(map capture_id lets)/\
 Forall(A.Named F)(map capture_name lets)/\pre=map(capture_pre F)lets/\
 Cl=fold_left(lp_capture_one Co)(combine(map capture_name lets)(map capture_id lets))(lambda_clear_context Co)/\
 CxRel w R Fl c(fold_left(fun en q=>let '(n,_,t) :=q in set_local en n t LCaptured)lets le)F Cl/\
 en_uf le=en_uf original/\(forall q,In q lets->st_next s<=capture_id q<st_next sf).
Proof.
 intros c original F Co ps body s le pre lets sf bound Run Hi Hw Hg HC Hp.
 assert(Unique:NoDup(map fst(en_locals original)))by exact(cr_locals_nodup _ _ _ _ _ _ _ HC).
 assert(Rows:Forall(fun q=>sassoc(fst q)(en_locals original)=Some(snd q))(lambda_captured c original ps body)).
 {apply Forall_forall;intros[n[t mu]]Hit;cbn[fst snd].
  unfold lambda_captured in Hit;destruct(cx_applying c);[|contradiction].
  apply filter_In in Hit as[Hit Test];exact(capture_local_lookup _ _ _ _ Unique Hit). }
 destruct(lambda_capture_fold w R Fl b Hload(lambda_captured c original ps body)c original F Co(with_locals original[])
  (lambda_clear_context Co)[][]s le pre lets sf bound Run Hi Hw Hg HC(cxrel_capture_base w R Fl _ _ _ _ HC)eq_refl Rows Hp)
 as(added&Rel&Pre&Lets&Fresh&Nd&Named&Bounds&Uf&Ctx).
 cbn[List.app]in Pre,Lets;subst added.
 assert(Names:map capture_name lets=map fst(lambda_captured c original ps body))by exact(captured_names_rows _ _ Rel).
 assert(Available:Forall(captured_row_available original)lets)by exact(capture_rows_available _ _ _ Rel Rows).
 set(Cl:=fold_left(lp_capture_one Co)(combine(map capture_name lets)(map capture_id lets))(lambda_clear_context Co)).
 assert(Context:CxRel w R Fl c(fold_left(fun en q=>let '(n,_,t) :=q in set_local en n t LCaptured)lets le)F Cl).
 {unfold Cl;rewrite(capture_full_reading w R Fl _ _ _ _ _ _ HC Available),capture_tables_factor.
  eapply captured_local_context;[exact HC|exact Ctx|exact Uf|exact Available]. }
 exists Cl;split.
 - intro n;rewrite Names.
   change(In n(map fst(lambda_source_captures c original ps body))<->
    A.Mentions_name body n/\A.slook n(A.c_locals Co)<>None/\~In n(map fst ps)/\A.fr_applying F=[]).
   rewrite(lambda_capture_names_exact c original ps body Unique n),(cr_applying _ _ _ _ _ _ _ HC).
   assert(Present:sassoc n(en_locals original)<>None<->A.slook n(A.c_locals Co)<>None).
   {rewrite(cr_locals _ _ _ _ _ _ _ HC);destruct(sassoc n(en_locals original));split;intros;try discriminate;contradiction. }
   tauto.
 - split.
   + rewrite Names;exact(lambda_capture_names_nodup c original ps body Unique).
   + split;[exact Fresh|];split;[exact Nd|];split;[exact Named|];split;[exact Pre|];
     split;[reflexivity|];split;[exact Context|];split;assumption.
Qed.
Theorem lambda_capture_value_context:forall c original F Co ps body s le pre lets sf bound,
 mfold(lambda_capture_one c original)(with_locals original[],[],[])(lambda_captured c original ps body)s=Good((le,pre,lets),sf)->
 Inv s->st_w s=w->nget(w_syms w)(gs_id bound)=Some bound->CxRel w R Fl c original F Co->Post w P R Fl sf F->
 CxRel w R Fl c le F(fold_left(capture_model Fl original)lets(lambda_clear_context Co))/\
 Forall(captured_row_available original)lets/\en_uf le=en_uf original.
Proof.
 intros c original F Co ps body s le pre lets sf bound Run Hi Hw Hg HC Hp.
 pose proof(lambda_source_capture_rows c original ps body(cr_locals_nodup _ _ _ _ _ _ _ HC))as Rows.
 destruct(lambda_capture_fold w R Fl b Hload(lambda_captured c original ps body)c original F Co(with_locals original[])
 (lambda_clear_context Co)[][]s le pre lets sf bound Run Hi Hw Hg HC(cxrel_capture_base w R Fl _ _ _ _ HC)eq_refl Rows Hp)
 as(added&Rel&Pre&Lets&Fresh&Nd&Named&Bounds&Uf&Ctx).
 cbn[List.app]in Lets;subst added;split;[exact Ctx|];split;[exact(capture_rows_available _ _ _ Rel Rows)|exact Uf].
Qed.
End Context.
