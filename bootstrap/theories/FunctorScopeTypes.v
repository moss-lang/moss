(* The static typed-world fact used by the executable implicit-input scan.
   XBridge.resultmentions_type captures only WorldOf, no runtime interface. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy
 ResultMentionsExact XBridge.
Import ListNotations.
Module A:=Audit.Elaboration.
Theorem actual_result_mentions_types:forall b w R Fl P g n names args res body s found s',
 load_world b=Good w->A.WorldOf b(BodySound.W w R Fl)->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res body->
 result_mentions g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 forall t,In t found->A.kind_sym(BodySound.W w R Fl)t=Some A.DKType.
Proof.
 intros b w R Fl P g n names args res body s found s' Hl Wo Hg D H Hi Hw L t Hin.
 destruct(proj1(result_mentions_exact w (P:=P) R Fl b Hl g n names args res body s found s' Hg D H Hi Hw L t)Hin)
  as(sp&Hsp&HM).
 exact(resultmentions_type b(BodySound.W w R Fl)Wo(gs_id g)(gm_path(get_mod w(gs_mod g)))sp t
  (load_world_loaded b w Hl(gs_mod g)(loader_mod_in w R Fl b Hl _ _ Hg))HM).
Qed.
