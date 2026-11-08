(* The actual application checker establishes satisfaction of functor inputs. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr
 CtxWalk CtxMerge FunctorContext NeedSatisfies InfoShape SmapExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Execution.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Let WW:=BodySoundCore.W w R Fl.
Theorem fun_env_satisfaction:forall g n names args res s ef needs sf mi e c F C sn ps sp sm ss,
 A.WorldOf b WW->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 fun_env g s=Good((ef,needs),sf)->Inv s->st_w s=w->Later w P R Fl sf->
 require_needs mi e g (FInfo ef needs
  (map(fun k=>(k,match get_app ef k with Some(a,_)=>a|None=>[]end))needs)(merge_edges ef))sn=Good(ps,sp)->
 smap_at ef e ps sp=Good(sm,ss)->Inv sn->Later w P R Fl ss->CxRel w R Fl c e F C->
 exists Cf,A.FunCtx WW(gs_id g)Cf(map(key_tr Fl)needs)/\
 A.Satisfies C Cf(map(key_tr Fl)needs)sm.
Proof.
 intros g n names args res s ef needs sf mi e c F C sn ps sp sm ss Wo Hg D E Hi Hw L En Es In Ln Hc.
 destruct(fun_env_context_valid w R Fl b Hload g n names args res s ef needs sf Wo Hg D E Hi Hw L)
 as(Cf&FC&Tables&Ti&Keys&Edges).
 exists Cf;split;[exact FC|].
 eapply (need_satisfies_tables w R Fl b Hload) with(info:=FInfo ef needs
  (map(fun k=>(k,match get_app ef k with Some(a,_)=>a|None=>[]end))needs)(merge_edges ef));
 try exact En;try exact Es;try exact In;try exact Ln;try exact Hc.
 - exact(fet_sub _ _ _ Tables).
 - exact Edges.
 - exact Ti.
 - reflexivity.
 - exact Keys.
 - exact(fet_apps _ _ _ Tables).
Qed.
Theorem fun_env_satisfaction_tables:forall g n names args res s ef needs sf mi e c F C sn ps sp sm ss,
 A.WorldOf b WW->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 fun_env g s=Good((ef,needs),sf)->Inv s->st_w s=w->Later w P R Fl sf->
 require_needs mi e g (FInfo ef needs
  (map(fun k=>(k,match get_app ef k with Some(a,_)=>a|None=>[]end))needs)(merge_edges ef))sn=Good(ps,sp)->
 smap_at ef e ps sp=Good(sm,ss)->Inv sn->Later w P R Fl ss->CxRel w R Fl c e F C->
 exists Cf,A.FunCtx WW(gs_id g)Cf(map(key_tr Fl)needs)/\
 FunEnvTables Fl ef Cf /\ A.Satisfies C Cf(map(key_tr Fl)needs)sm.
Proof.
 intros g n names args res s ef needs sf mi e c F C sn ps sp sm ss Wo Hg D E Hi Hw L En Es In Ln Hc.
 destruct(fun_env_context_valid w R Fl b Hload g n names args res s ef needs sf Wo Hg D E Hi Hw L)
 as(Cf&FC&Tables&Ti&Keys&Edges).
 exists Cf;split;[exact FC|]. split;[exact Tables|].
 eapply (need_satisfies_tables w R Fl b Hload) with(info:=FInfo ef needs
  (map(fun k=>(k,match get_app ef k with Some(a,_)=>a|None=>[]end))needs)(merge_edges ef));
 try exact En;try exact Es;try exact In;try exact Ln;try exact Hc.
 - exact(fet_sub _ _ _ Tables).
 - exact Edges.
 - exact Ti.
 - reflexivity.
 - exact Keys.
 - exact(fet_apps _ _ _ Tables).
Qed.
End Execution.
