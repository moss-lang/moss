(* Families generate no type outputs; their result walk starts in the caller. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound
 NestingTypesExec ProvideResultExec FamilyShapeExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Section FamilyResults.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySound.W w R Fl).
Lemma family_result_types_empty:forall g n names args res s ts s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->result_types g s=Good(ts,s')->Inv s->st_w s=w->
 ts=[]/\s'=s.
Proof.
 intros g n names args res s ts s' Hg D Fam E Hi Hw.
 destruct(result_types_rel w R Fl b Hload g n names args res None s ts s' Hg D E Hi Hw)
 as[->[_ Types]]. split;[|reflexivity].
 destruct ts as[|t ts];[reflexivity|].
 exfalso;eapply family_no_outputs;[exact Fam|apply Types;left;reflexivity].
Qed.
Theorem family_provide_result_phases:forall g n names args res e gens s e' keys s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->provide_result e g gens s=Good((e',keys),s')->Inv s->st_w s=w->
 exists fuel sf,walk_fuel s=Good(fuel,sf)/\
 mfold(fun acc sp=>provide_spec fuel(fst acc)g(gs_mod g)sp[](snd acc))
 (e,[])res sf=Good((e',keys),s').
Proof.
 intros g n names args res e gens s e' keys s' Hg D Fam E Hi Hw.
 destruct(provide_result_phases _ _ _ _ _ _ _ E)as(ts&st&et&se&fuel&sf&Types&Install&Fuel&Walk).
 destruct(family_result_types_empty _ _ _ _ _ _ _ _ Hg D Fam Types Hi Hw)as[-> ->].
 cbn[mfold]in Install;injection Install as <- <-.
 exists fuel,sf;split;[exact Fuel|].
 unfold Elab.functor_parts in Walk;rewrite D in Walk;exact Walk.
Qed.
End FamilyResults.
