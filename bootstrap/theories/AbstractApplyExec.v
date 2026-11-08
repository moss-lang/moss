(* The actual general abstract application, with its checked result reader. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 InfoPreserve BindSequence ApplyFreshExec AbstractApplyPlan AbstractApplyBind ApplyFunctorInduct GeneralProvideResult.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Execution.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Theorem abstract_application_sound:AbstractApplyStep w P R Fl.
Proof.
 intros fuel c e F C pa g n names args res s xs e' c' sf Hg D Resolve Run Hi Ii Fi Hw HC Post.
 destruct(abstract_application_plan w R Fl b Hload fuel c e F C g n names args res s xs e' c' sf
 Hworld Hg D Run Hi Hw(proj1 Post)HC)
 as(ef&needs&Cu&fx&rts&gens&th&extra&sm&ss&keys&so&Have&FC&Tables&Reads&Fixed&Agree&Sat&Nd&Domain&Generated&Outputs&Mentioned&Provide&Is&Ws&Lo&Pre&Ctx).
 assert(Total:forall t,A.FunOutputs(BodySoundCore.W w R Fl)(gs_id g)t->nassoc t gens<>None).
 {intros t Out;destruct(output_generations_lookup w R Fl e rts gens Generated t(proj2(Domain t)Out))as(id&Look&Case);rewrite Look;discriminate. }
 destruct(general_provide_result w R Fl b Hload Finj c e F C g n names args res gens ss e' keys so
 Hg D Provide Is Ws Lo HC Mentioned Total)as(Cout&Result&Hout).
 rewrite Pre,Ctx.
 eapply abstract_bind_from_result;[exact Resolve|exact Have|exact FC|exact Reads|exact Fixed|exact Sat|exact Outputs|exact Result|exact Hout].
Qed.
End Execution.
