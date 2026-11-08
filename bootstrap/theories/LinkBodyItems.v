(* The real body run elaborates the provider's bind sequence in its own context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr BodyTy KeyTranslation InfoPreserve BindSequence ApplyBodyContext
 FunctorBodyContext FunctorBodyEntry FunctorContext FnBodyScope ReifyData.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Body.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Load:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem link_body_items : forall f,OneStep w P R Fl f ->
 forall g name names args res bodies envStart eg needs envEnd pins start xs eb exit final,
 A.WorldOf b W -> nget(w_syms w)(gs_id g)=Some g ->
 gs_decl g=ADFunctor name names args res(Some bodies) ->
 fun_env g envStart=Good((eg,needs),envEnd) ->
 Inv envStart -> st_w envStart=w -> Later w P R Fl envEnd ->
 apply_body(S f)(functor_body_context g pins)eg g start=Good((xs,eb,exit),final) ->
 Inv start -> InfoInv start -> FwdInv start -> Later w P R Fl final ->
 exists Cg Cb Fg Fb made wrap inner,
 A.FunCtx W(gs_id g)Cg(map(key_tr Fl)needs) /\
 FunEnvTables Fl eg Cg /\
 A.fr_mod Fg=gm_path(get_mod w(gs_mod g)) /\ A.fr_pins Fg=pins /\
 A.BindItems W P Fg Cg(List.concat bodies)wrap Cb Fb made /\
 (forall tail,wrap tail=fold_stmts xs tail) /\ CxRel w R Fl inner eb Fb Cb.
Proof.
 intros f Items g name names args res bodies envStart eg needs envEnd pins start xs eb exit final
  World Loaded Decl Env ValidEnv EnvWorld EnvLast Run Valid Cache Forward Last.
 destruct(defined_fun_env_body_context w(P:=P)R Fl b Load g name names args res bodies
   envStart eg needs envEnd pins(st_vars final)World Loaded Decl Env ValidEnv EnvWorld EnvLast)
   as[Cg[Context[Tables Entry]]].
 set(F:=functor_body_frame w g pins(st_vars final)).
 pose proof Run as Whole.
 cbn[apply_body]in Run.
 unfold functor_parts in Run;rewrite Decl in Run;cbn beta iota zeta in Run.
 apply bind_inv in Run as[[[ys ey]cy][sy[Walk Return]]].
 injection Return as EqXs EqEnv EqExit EqState;subst ys ey sy.
 change(mfold(bind_fold_step f)([],eg,cx_applying_in(functor_body_context g pins)g)
   (List.concat bodies)start=Good((xs,eb,cy),final))in Walk.
 assert(Inner:CxRel w R Fl(cx_applying_in(functor_body_context g pins)g)eg
   (A.with_mod F(gm_path(get_mod w(gs_mod g)))(gs_id g))Cg).
 {apply cxrel_enter_functor;[exact Entry|exact(loader_mod_in w R Fl b Load _ _ Loaded)]. }
 assert(Post:Post w P R Fl final(A.with_mod F(gm_path(get_mod w(gs_mod g)))(gs_id g))).
 {split;[exact Last|]. intros n slot At;unfold F;cbn[A.with_mod A.fr_var functor_body_frame];rewrite slook_sassoc;exact At. }
 destruct(bind_items_exec w P R Fl f Items(List.concat bodies)[]
   (cx_applying_in(functor_body_context g pins)g)eg start xs eb cy final
   (A.with_mod F(gm_path(get_mod w(gs_mod g)))(gs_id g))Cg Walk Valid Cache Forward Inner Post)
   as(zs&Fb&Cb&made&wrap&Eq&Out&Vars&Fresh&Typed&Fold).
 cbn[List.app]in Eq;subst zs.
 exists Cg,Cb,(A.with_mod F(gm_path(get_mod w(gs_mod g)))(gs_id g)),Fb,made,wrap,cy.
 split;[exact Context|];split;[exact Tables|];split;[reflexivity|];split;[reflexivity|].
 split;[exact Typed|];split;assumption.
Qed.
Theorem link_body_scoped_items : forall f,OneStep w P R Fl f ->
 forall g name names args res bodies envStart eg needs envEnd pins owner registered xs eb exit bodyFinal,
 A.WorldOf b W -> nget(w_syms w)(gs_id g)=Some g ->
 gs_decl g=ADFunctor name names args res(Some bodies) ->
 fun_env g envStart=Good((eg,needs),envEnd) ->
 Inv envStart -> st_w envStart=w -> Later w P R Fl envEnd ->
 in_reify_code owner(with_vars[](apply_body(S f)(functor_body_context g pins)eg g))registered=
   Good((xs,eb,exit),bodyFinal) ->
 Inv registered -> InfoInv registered -> FwdInv registered -> Later w P R Fl bodyFinal ->
 exists Cg Cb Fg Fb made wrap inner,
 A.FunCtx W(gs_id g)Cg(map(key_tr Fl)needs) /\ FunEnvTables Fl eg Cg /\
 A.fr_mod Fg=gm_path(get_mod w(gs_mod g)) /\ A.fr_pins Fg=pins /\
 A.BindItems W P Fg Cg(List.concat bodies)wrap Cb Fb made /\
 (forall tail,wrap tail=fold_stmts xs tail) /\ CxRel w R Fl inner eb Fb Cb.
Proof.
 intros f Items g name names args res bodies envStart eg needs envEnd pins owner registered xs eb exit bodyFinal
  World Loaded Decl Env ValidEnv EnvWorld EnvLast Run Valid Cache Forward Last.
 destruct(in_reify_checked_scope w P R Fl _ owner
  (with_vars[](apply_body(S f)(functor_body_context g pins)eg g))registered(xs,eb,exit)bodyFinal
  Valid Cache Forward Run Last)
  as(meta&metaLast&Vars&Vm&Cm&Fm&Lm&VarsEq&CountEq&WorldEq).
 assert(Shape:VShape(s_vars[]0 meta)).
 {unfold VShape;cbn;split;[constructor|split;reflexivity]. }
 destruct(with_vars_checked_scope w P R Fl _ [](apply_body(S f)(functor_body_context g pins)eg g)
  meta(xs,eb,exit)metaLast Vm Cm Fm(ok_apply_body _ _ _ _)Vars Lm Shape)
  as(final&Body&Vb&Cb&Fb&Lb&Shapeb&Frame).
 eapply(link_body_items f Items);eassumption.
Qed.
End Body.
