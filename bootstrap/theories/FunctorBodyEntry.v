(* Actual defined input formation supplies the complete linked-body context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr BodyTy KeyTranslation FunctorDefinedContext FunctorOwnSupport FunctorBodyContext FunctorContext.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Entry.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Load:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem defined_fun_env_body_context : forall g name names args res body s e needs final pins variables,
 A.WorldOf b W -> nget(w_syms w)(gs_id g)=Some g ->
 gs_decl g=ADFunctor name names args res(Some body) ->
 fun_env g s=Good((e,needs),final) -> Inv s -> st_w s=w -> Later w P R Fl final ->
 exists C,A.FunCtx W(gs_id g)C(map(key_tr Fl)needs) /\
 FunEnvTables Fl e C /\
 CxRel w R Fl(functor_body_context g pins)e(functor_body_frame w g pins variables)C.
Proof.
 intros g name names args res body s e needs final pins variables World Loaded Decl Run Valid Hw Last.
 destruct(defined_fun_env_context_data w P R Fl b Load g name names args res body s e needs final
  World Loaded Decl Run Valid Hw Last)as(C&Context&Tables&Triangular&Keys&Merges&raw&entries&tm&Eq&Formed&Ceq).
 pose proof(defined_fun_env_support w(P:=P)R Fl b Load g name names args res body s e needs final
  Loaded Decl Run Valid Hw Last)as Supported.
 exists C;split;[exact Context|];split;[exact Tables|].
 subst e;eapply(functx_initial_body w R Fl g raw C needs entries tm pins variables);
 try eassumption;[reflexivity|exact(loader_mod_in w R Fl b Load _ _ Loaded)].
Qed.
End Entry.
