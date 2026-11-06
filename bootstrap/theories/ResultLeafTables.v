(* Normalized result leaves update exactly the table produced by the actual
   source setter; occurrence order is retained by the surrounding fold. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySound BodyTy
 ProvideResultFold FamilyResultFold.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma result_value_output:forall C v ty app,
 family_types_stable C->A.canon C ty=ty->
 A.output_cx C(A.c_ty C)[A.NVal v ty app]=
 A.provide C(IVal v)app(A.set_val C v ty).
Proof. intros;rewrite output_cx_value,family_empty_self by assumption;now rewrite H0. Qed.
Lemma result_function_output:forall C f sg app,
 family_types_stable C->A.sig_subst(A.c_sub C)sg=sg->
 A.output_cx C(A.c_ty C)[A.NFn f sg app]=
 A.provide C(IFn f)app(A.set_fn C f sg).
Proof. intros;rewrite output_cx_function,family_empty_self by assumption;now rewrite H0. Qed.
Lemma result_functor_output:forall C u fx,
 family_types_stable C->A.fixed_subst(A.c_sub C)fx=fx->
 A.output_cx C(A.c_ty C)[A.NFun u fx]=A.set_fun C u fx.
Proof. intros;rewrite output_cx_functor,family_empty_self by assumption;now rewrite H0. Qed.
Section Reading.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Lemma result_reading_canonical:forall c e F C tm tmr,
 CxRel w R Fl c e F C->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 forall k ty,A.nlook k tmr=Some ty->A.canon C ty=ty.
Proof.
 intros c e F C tm tmr HC Read k ty Lookup;rewrite Read in Lookup.
 destruct(nassoc k tm)as[y|];[|discriminate Lookup].
 injection Lookup as <-;apply(family_canonical_receiver w R Fl _ _ _ _ HC).
Qed.
Lemma result_fixed_readings_canonical:forall c e F C tm tmr u fx,
 CxRel w R Fl c e F C->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 A.FixedReadings W u tmr fx->A.fixed_subst(A.c_sub C)fx=fx.
Proof.
 intros c e F C tm tmr u fx HC Read Fixed.
 unfold A.fixed_subst;rewrite <- (map_id fx)at 2.
 apply map_ext_in;intros[x ty]Inx;cbn[fst snd id].
 f_equal.
 apply(result_reading_canonical c e F C tm tmr HC Read x ty).
 exact(proj2((proj1(Fixed x ty))Inx)).
Qed.
End Reading.
