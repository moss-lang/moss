(* Every result item reads brackets in the current complete output scope.
   This execution package applies to values, functions, methods and contexts. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySoundCore BodyTy
 CanonBodyTy CanonTypes Merge SourceSupport NominalSupport FamilyMethodExec.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[app_subst ety_of FUEL reqs type_requirements].
Section Scope.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem output_scope_execution:forall c e F C mi a outer ob s sub sx,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sx)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sx->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists bs,
 A.OutputBrackets W(gm_path(get_mod w mi))(ob++A.c_ty C)a bs /\
 map(fun q=>(fst q,tr Fl(canon e(snd q))))sub=bs++ob /\
 Forall(fun q=>WFty(snd q))sub /\Forall(fun q=>Support W(snd q))sub /\
 TmRel Fl(sub++tymap e)(map(trp Fl)(sub++tymap e))e /\
 (forall k y,nassoc k(sub++tymap e)=Some y->Support W y) /\
 (forall k,A.nlook k(bs++ob++A.c_ty C)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k(sub++tymap e))).
Proof.
 intros c e F C mi a outer ob s sub sx Sub Hi Hw Hmi L HC Wo So O.
 assert(TR0:TmRel Fl(tymap e)(map(trp Fl)(tymap e))e).
 {apply tmrel_tymap;[exact(cr_ty_wf _ _ _ _ _ _ _ HC)|intro k;rewrite nlook_trp,nassoc_tymap;reflexivity]. }
 assert(TR:TmRel Fl(outer++tymap e)(map(trp Fl)(outer++tymap e))e).
 {rewrite map_app;apply tmrel_app;[exact TR0|reflexivity|exact Wo]. }
 assert(M:forall k,A.nlook k(ob++A.c_ty C)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k(outer++tymap e))).
 {intro k;rewrite <-O,nlook_app,family_canon_lookup,(cr_ty _ _ _ _ _ _ _ HC),nassoc_app,nassoc_tymap.
  destruct(nassoc k outer);reflexivity. }
 destruct(family_output_brackets w R Fl b Hload e mi a outer ob(A.c_ty C)s sub sx Sub Hi Hw Hmi L
  (proj1(cr_uf _ _ _ _ _ _ _ HC))TR O Wo M)as(bs&Bmap&Wsub&Br).
 assert(TRs:TmRel Fl(sub++tymap e)(map(trp Fl)(sub++tymap e))e).
 {rewrite map_app;apply tmrel_app;[exact TR0|reflexivity|exact Wsub]. }
 assert(Ms:forall k,A.nlook k(bs++ob++A.c_ty C)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k(sub++tymap e))).
 {intro k;rewrite app_assoc,<-Bmap,nlook_app,family_canon_lookup,
 (cr_ty _ _ _ _ _ _ _ HC),nassoc_app,nassoc_tymap.
 destruct(nassoc k sub);reflexivity. }
 assert(Souter:forall k y,nassoc k outer=Some y->Support W y).
 {intros k y Ey;rewrite Forall_forall in So;exact(So(k,y)(Merge.nassoc_in _ _ _ _ Ey)). }
 assert(Wouter:forall k y,nassoc k outer=Some y->WFty y).
 {intros k y Ey;rewrite Forall_forall in Wo;exact(Wo(k,y)(Merge.nassoc_in _ _ _ _ Ey)). }
 assert(Ssub:Forall(fun q=>Support W(snd q))sub).
 {destruct a as[bs0|].
  - apply bind_inv in Sub as(xs&sy&Read&Ret);injection Ret as <- <-.
    apply Forall_app;split;[|exact So].
    eapply(app_subst_support_at w R Fl b Hload);[exact Read|exact Hi|exact Hw|exact Hmi|exact Souter|exact Wouter|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|exact L].
  - injection Sub as <- _;exact So. }
 assert(Stm:forall k y,nassoc k(sub++tymap e)=Some y->Support W y).
 {intros k y Ey;rewrite nassoc_app in Ey;destruct(nassoc k sub)as[v|]eqn:N.
  - injection Ey as <-;rewrite Forall_forall in Ssub;exact(Ssub(k,v)(Merge.nassoc_in _ _ _ _ N)).
  - rewrite nassoc_tymap in Ey;exact(@tn_get_support W e k y(cr_support _ _ _ _ _ _ _ HC)Ey). }
 exists bs;split;[exact Br|];split;[exact Bmap|];split;[exact Wsub|];
 split;[exact Ssub|];split;[exact TRs|];split;assumption.
Qed.
End Scope.
