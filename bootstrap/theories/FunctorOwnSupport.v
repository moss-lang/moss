(* Defined functor inputs preserve source-type validity during merge rebuilding. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTy BodyTr Merge CanonTypes SourceClean ContextClean ContextSupport
 CtxWalk ContextWalkClean ContextWalkSupport ContextMergeSupport FunctorContext
 FunctorScopeTypes NominalSupport BindFunctorCtx.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Support.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Load:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Lemma own_specs_support : forall fuel mi specs e needs s e' needs' final,
 mfold(fun acc sp=>add_spec fuel(fst acc)mi sp[](snd acc))(e,needs)specs s=Good((e',needs'),final) ->
 Inv s -> st_w s=w -> In mi(w_order w) ->
 ContextClean e -> Tri(en_uf e) -> EnvSupport W e -> Later w P R Fl final ->
 ContextClean e' /\ Tri(en_uf e') /\ EnvSupport W e'.
Proof.
 intros fuel mi specs;induction specs as[|sp rest IH];intros e needs s e' needs' final Run Valid Hw Mod Clean Triangular Supported Last.
 - injection Run as <- <- <-;split;[exact Clean|];split;assumption.
 - cbn[mfold]in Run;apply bind_inv in Run as[[mid required][state[Step Rest]]].
   destruct(ok_add_spec fuel e mi sp[]needs _ _ _ Valid Step)as[Vm Fm].
   assert(Orest:ok(mfold(fun acc sp=>add_spec fuel(fst acc)mi sp[](snd acc))(mid,required)rest)).
   {apply ok_mfold;intros;apply ok_add_spec. }
   destruct(Orest _ _ _ Vm Rest)as[_ Fr].
   pose proof(later_back w P R Fl _ _ Fr Last)as Lm.
   destruct(proj2(context_walk_clean w(P:=P)R Fl b Load fuel)e mi sp[]needs s mid required state
     Step Valid Hw Mod ltac:(intros key ty Bad;discriminate Bad) Clean Triangular Lm)as[Cm Tm].
   pose proof(proj2(context_walk_support w(P:=P)R Fl b Load fuel)e mi sp[]needs s mid required state
     Step Valid Hw Mod ltac:(intros key ty Bad;discriminate Bad)(Forall_nil _)(Forall_nil _)
     Clean Triangular Supported Lm)as Sm.
   exact(IH mid required state e' needs' final Rest Vm ltac:(rewrite(fr_w _ _ Fm);exact Hw) Mod Cm Tm Sm Last).
Qed.

Lemma defined_own_merged_support : forall fuel mi args outs s e needs final,
 In mi(w_order w) ->
 merged_walk mi(own_walk fuel mi args outs[])s=Good((e,needs),final) ->
 Inv s -> st_w s=w -> Later w P R Fl final ->
 ContextClean e /\ Tri(en_uf e) /\ EnvSupport W e.
Proof.
 intros fuel mi args outs s e needs final Mod Run Valid Hw Last.
 apply(merged_walk_support w(P:=P)R Fl(own_walk fuel mi args outs[]))with(mi:=mi)(s:=s)(needs:=needs)(s':=final);
 try assumption.
 - intro start;apply ok_own_walk.
 - intros U before local required after TriU CleanU SupportU I W0 L Step.
   unfold own_walk in Step;apply bind_inv in Step as[[mid ns][state[Walk Return]]].
   injection Return as <- <- <-.
   destruct(support_edges_start W U CleanU TriU SupportU)as[C0[T0 S0]].
   eapply own_specs_support;[exact Walk|exact I|exact W0|exact Mod|exact C0|exact T0|exact S0|exact L].
Qed.

Theorem defined_fun_env_support : forall g name names args res body s e needs final,
 nget(w_syms w)(gs_id g)=Some g -> gs_decl g=ADFunctor name names args res(Some body) ->
 fun_env g s=Good((e,needs),final) -> Inv s -> st_w s=w -> Later w P R Fl final ->
 EnvSupport W e.
Proof.
 intros g name names args res body s e needs final Loaded Decl Run Valid Hw Last.
 unfold fun_env,functor_parts in Run;rewrite Decl in Run;cbn beta iota zeta in Run.
 apply bind_inv in Run as[outputs[s1[Outputs Run]]].
 assert(Ooutputs:ok(result_types g))by(unfold result_types;okt).
 destruct(Ooutputs _ _ _ Valid Outputs)as[I1 F1].
 apply bind_inv in Run as[mentions[s2[Mentions Run]]].
 assert(Omentions:ok(result_mentions g))by(unfold result_mentions;okt).
 destruct(Omentions _ _ _ I1 Mentions)as[I2 F2].
 apply bind_inv in Run as[fuel[s3[Fuel Run]]].
 destruct(ok_ctx_fuel _ _ _ I2 Fuel)as[I3 F3].
 apply bind_inv in Run as[[local required][s4[Walk Return]]].
 injection Return as <- <- <-.
 change(merged_walk(gs_mod g)(own_walk fuel(gs_mod g)args outputs[])s3=Good((local,required),s4))in Walk.
 apply freeze_all_apps_support.
 exact(proj2(proj2(defined_own_merged_support fuel(gs_mod g)args outputs s3 local required s4
   (loader_mod_in w R Fl b Load _ _ Loaded)Walk I3
   ltac:(rewrite(fr_w _ _ F3),(fr_w _ _ F2),(fr_w _ _ F1);exact Hw)Last))).
Qed.
End Support.
