(* Defined applications recurse only into genuinely smaller bind executions. *)
From Stdlib Require Import List String Bool Arith Lia Wf_nat.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy BodyDataHelpers
 InfoPreserve BindSequence BindCopyDispatch BindFnDispatch ApplyFunctorCases ApplyBodyExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition AbstractApplyStep(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat):Prop:=
 forall f c e F C pa g n names args res s xs e' c' sf,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.resolve_sym(BodySoundCore.W w R Fl)(A.fr_mod F)pa=Some(gs_id g)->
 apply_abstract f c e g s=Good((xs,e',c'),sf)->
 Inv s->InfoInv s->FwdInv s->st_w s=w->CxRel w R Fl c e F C->Post w P R Fl sf F->
 BindResult w P R Fl c e F C(Spec(Some pa)None None,None)xs e' c'.
Section Induction.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Theorem apply_from_abstract_and_remaining:
 AbstractApplyStep w P R Fl->
 (forall f,OK w P R Fl f->RemainingBindStep w P R Fl f)->
 forall f,OK w P R Fl f->ApplyBindStep w P R Fl f.
Proof.
 intros Abstract Remaining f;induction f as[f IH]using lt_wf_ind.
 intros OK c e sp s xs e' c' sf Run F C Hi Ii Fi HC Post.
 destruct f as[|fuel];[cbn[apply_functor deep fail]in Run;discriminate Run|].
 destruct(ok_apply_functor(S fuel)c e sp _ _ _ Hi Run)as[If Frame].
 assert(Hw:st_w s=w)by(rewrite<-(fr_w _ _ Frame);exact(lt_w _ _ _ _ _(proj1 Post))).
 destruct(apply_functor_cases w R Fl b Hload fuel c e sp s xs e' c' sf F C Run Hi Ii Fi Hw HC)
 as(pa&g&n&names&args&res&body&sa&Spec&Resolve&Hg&Decl&Ia&Iia&Fia&Wa&Gap&Body).
 subst sp;destruct body as[bodies|].
 - destruct fuel as[|k];[cbn[apply_body deep fail]in Body;discriminate Body|].
   pose proof(ok_down w P R Fl(S k)OK)as OK1.
   pose proof(ok_down w P R Fl k OK1)as OK0.
   assert(App:ApplyBindStep w P R Fl k)by(apply IH;[lia|exact OK0]).
   assert(Res:ResidualBindStep w P R Fl k).
   {exact(@residual_after_abstract_copy w P R Fl b Hload Hworld k
    (@residual_after_named_function w P R Fl b Hload Hworld Finj k(Remaining k OK0))). }
   pose proof(@one_from_residual w P R Fl b Hload k OK0 Res App)as Items.
   eapply(apply_body_from_items w R Fl b Hload k Items);eassumption.
 - eapply Abstract;eassumption.
Qed.
End Induction.
