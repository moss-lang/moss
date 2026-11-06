(* Actual family applications provide exactly their declared method results. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 CtxMerge NominalSupport FamilyShapeExec FamilyResultExec FamilyResultFold FamilyResultWalk
 ProvideResultContext ProvideResultFold TySupport.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Result.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySound.W w R Fl).
Theorem family_provide_result_entries:forall c e F C g n names args res gens (out:nat->ty) s e' keys s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->provide_result e g gens s=Good((e',keys),s')->
 Inv s->st_w s=w->Later w P R Fl s'->CxRel w R Fl c e F C->
 exists es,A.OutputItemsEntries W(gm_path(get_mod w(gs_mod g)))[](family_result_tys C)res es(family_result_tys C)/\
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es/\
 A.ResultKeys(A.c_sub C)es(map(key_tr Fl)keys)/\
 CxRel w R Fl c e' F(A.output_cx C(family_result_tys C)es).
Proof.
 intros c e F C g n names args res gens out s e' keys s' Hg D Fam E Hi Hw L HC.
 destruct(family_provide_result_phases w R Fl b Hload g n names args res e gens s e' keys s'
 Hg D Fam E Hi Hw)as(fuel&sf&Fuel&Walk).
 assert(Of:ok walk_fuel)by okt.
 destruct(Of _ _ _ Hi Fuel)as[If Ff].
 assert(Wf:st_w sf=w)by(rewrite(fr_w _ _ Ff);exact Hw).
 assert(Leaves:Forall(A.FamilyResult W(gm_path(get_mod w(gs_mod g))))res).
 {destruct(proj1 Fam)as(d&n0&names0&args0&res0&Decl&Ast&Leaves).
  change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
  rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
  cbn[A.d_ast A.d_mod]in Ast,Leaves;rewrite D in Ast;injection Ast as <- <- <- <-.
  exact Leaves. }
 destruct(family_initial_context w R Fl c e F C HC)as[HC0[Stable Init]].
 pose proof(proj2(family_result_walk w (P:=P)R Fl b Hload Finj fuel))as All.
 destruct(All c e F(output_with_types C(family_result_tys C))g(gs_mod g)res[][][]sf e' keys s'
 Walk Leaves If Wf(loader_mod_in w R Fl b Hload _ _ Hg)L HC0 Stable
 (Forall_nil _) (Forall_nil _)eq_refl)as(es&fresh&Sem&Keys&RK&Support&Ctx).
 cbn[List.app]in Keys;subst fresh.
 change(CxRel w R Fl c e' F(A.output_cx(output_with_types C(family_result_tys C))(family_result_tys C)es))in Ctx.
 assert(Final:A.output_cx(output_with_types C(family_result_tys C))(family_result_tys C)es=
 A.output_cx C(family_result_tys C)es).
 {rewrite <-Init,<-output_cx_append;reflexivity. }
 rewrite Final in Ctx.
 exists es;split;[exact Sem|];split;[exact Support|];split;assumption.
Qed.
Theorem family_provide_result:forall c e F C g n names args res gens out s e' keys s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->provide_result e g gens s=Good((e',keys),s')->
 Inv s->st_w s=w->Later w P R Fl s'->CxRel w R Fl c e F C->
 exists C',A.ProvideResult W(gs_id g)C out C'(map(key_tr Fl)keys)/\CxRel w R Fl c e' F C'.
Proof.
 intros c e F C g n names args res gens out s e' keys s' Hg D Fam E Hi Hw L HC.
 destruct(family_provide_result_entries c e F C g n names args res gens out s e' keys s'
 Hg D Fam E Hi Hw L HC)as(es&Sem&Supported&RK&Ctx).
 exists(A.output_cx C(family_result_tys C)es);split;[|exact Ctx].
 exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),names,args,res,None,[],es,(family_result_tys C).
 split;[exact(w_decl_read w R Fl _ _ Hg)|].
 split;[cbn[A.d_ast A.functor_parts];rewrite D;reflexivity|].
 split.
 - intro t;split;[intros[]|intro H;exfalso;exact(family_no_outputs W(gs_id g)Fam t H)].
 - split.
   + change(A.OutputItemsEntries W(gm_path(get_mod w(gs_mod g)))[](family_result_tys C)res es(family_result_tys C))in Sem.
     exact Sem.
   + split;[exact RK|reflexivity].
Qed.
End Result.
