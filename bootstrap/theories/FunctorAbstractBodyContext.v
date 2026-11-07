(* An abstract target's actual own input context, including implicit types. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr BodyTy Merge CanonTypes SourceClean ContextClean CtxWalk CtxMerge
 FunctorContext FunctorMerge ResultMentionsExact NestingTypesExec FunctorScopeTypes ImplicitTypeWalk
 NeedKeys NominalSupport ContextSupport ContextMergeSupport FunctorOwnSupport
 BindFunctorCtx FunctorBodyContext.
Import ListNotations.
Local Open Scope list_scope.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Lemma abstract_add_type_support : forall W e t stamp,
 EnvSupport W e -> EnvSupport W (set_tn e t (YAbs t) stamp).
Proof.
 intros W e t stamp Supported;constructor.
 - intros key value Hit;cbn [set_tn en_tn] in Hit.
   apply in_aset in Hit as [Eq|Old].
   + inversion Eq;constructor.
   + exact (es_tn Supported key value Old).
 - exact (es_uf Supported).
 - exact (es_vals Supported).
 - exact (es_fns Supported).
 - exact (es_mths Supported).
 - exact (es_locals Supported).
Qed.
Section Context.
Variable w : Elab.world. Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySoundCore.W w R Fl).
Theorem abstract_fun_env_context_data:forall g n names args res s e needs s',
 A.WorldOf b WW->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 fun_env g s=Good((e,needs),s')->Inv s->st_w s=w->Later w P R Fl s'->
 exists C,A.FunCtx WW(gs_id g)C(map(key_tr Fl)needs)/\
   FunEnvTables Fl e C /\ Tri(en_uf e) /\ Forall(fun k=>key_ok k=true)needs /\ merge_edges e=en_uf e /\
 exists raw entries tm,e=freeze_all_apps raw /\
 WS Fl(en_uf raw)raw tm entries needs /\ C=A.entries_cx(suf Fl(en_uf raw))tm(map(tr_ent Fl)entries).
Proof.
 intros g n names args res s e needs s' Wo Hg D H Hi Hw L.
 unfold fun_env in H;unfold functor_parts in H;rewrite D in H;cbn beta iota zeta in H.
 apply bind_inv in H as(outs&s1&Ho&H).
 destruct(result_types_rel w R Fl b Hload g n names args res None s outs s1 Hg D Ho Hi Hw)as[->[_ Outs]].
 apply bind_inv in H as(ms&s2&Hms&H).
 assert(Om:ok(result_mentions g))by(unfold result_mentions;okt).
 destruct(Om _ _ _ Hi Hms)as[I2 F2].
 assert(W2:st_w s2=w)by(rewrite(fr_w _ _ F2);exact Hw).
 apply bind_inv in H as(cf&s3&Hcf&H).
 destruct(ok_ctx_fuel _ _ _ I2 Hcf)as[I3 F3].
 apply bind_inv in H as([e0 ns]&s4&Hwalk&H);injection H as <- <- <-.
 change(merged_walk(gs_mod g)(own_walk cf(gs_mod g)args outs ms)s3=Good((e0,ns),s4))in Hwalk.
 assert(Owalk:ok(merged_walk(gs_mod g)(own_walk cf(gs_mod g)args outs ms))).
 { apply ok_merged_walk_frame;intro;apply ok_own_walk. }
 destruct(Owalk _ _ _ I3 Hwalk)as[_ Fw].
 assert(L2:Later w P R Fl s2)by exact(later_back w P R Fl _ _ F3(later_back w P R Fl _ _ Fw L)).
 pose proof(result_mentions_exact w (P:=P) R Fl b Hload g n names args res None s ms s2 Hg D Hms Hi Hw L2)as Ms.
 pose proof(mentions_from_lists w R Fl g n names args res ms outs Hg D Ms Outs)as Mentions.
 assert(Types:forall t,In t ms->A.kind_sym WW t=Some A.DKType).
 { exact(actual_result_mentions_types b w R Fl P g n names args res None s ms s2 Hload Wo Hg D Hms Hi Hw L2). }
 assert(Finj:forall a c,Fl a=Fl c->a=c)by exact(A.wo_fld Wo).
 destruct(own_merged w (P:=P) R Fl b Hload Finj cf(gs_mod g)args outs ms s3 e0 ns s4(loader_mod_in w R Fl b Hload _ _ Hg)Types Hwalk I3
  ltac:(rewrite(fr_w _ _ F3);exact W2)L)as(les&tm&Rd&WS0&Mg&Nd).
 destruct Rd as(aes&atm&added&IA&Member&Es&Tm).
 set(sigma:=suf Fl(en_uf e0)).
 exists(A.entries_cx sigma tm(map(tr_ent Fl)les)). split.
 {
 exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),names,args,res,None,aes,atm,tm,sigma,(rev added).
 split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split.
 - cbn[A.d_ast A.functor_parts];rewrite D;reflexivity.
 - split;[exact IA|]. split.
   + intro t;rewrite<-in_rev,Member;rewrite<-Mentions;tauto.
   + split;[exact Tm|]. split.
     * apply(proj1(mgm_append_types sigma aes added));rewrite<-Es;exact Mg.
     * split.
       -- rewrite Es,entries_cx_append_types;reflexivity.
       -- apply(proj1(needs_append_types sigma aes added(map(key_tr Fl)ns)));rewrite<-Es;exact Nd.
 }
 split.
 { constructor.
 - reflexivity.
 - intro k;exact(ctx_ty Fl(en_uf e0)e0 tm les ns WS0 eq_refl k).
 - intro v;exact(ctx_vals Fl(en_uf e0)e0 tm les ns WS0 eq_refl v).
 - intro f;exact(ctx_fns Fl(en_uf e0)e0 tm les ns WS0 eq_refl f).
 - intros k m Ok Ck;exact(ctx_mths Fl Finj(en_uf e0)e0 tm les ns WS0 eq_refl k m Ok Ck).
 - intro u;exact(ctx_funpack Fl Finj(en_uf e0)e0 tm les ns WS0 eq_refl u).
 - intros k Nk Ok;exact(ctx_apps Fl Finj(en_uf e0)e0 tm les ns WS0 eq_refl k Nk Ok).

 }
 split;[exact(ws_tri _ _ _ _ _ _ WS0)|].
 split;[exact(ws_needs_ok Fl (en_uf e0) e0 tm les ns WS0 eq_refl)|].
 split.
 - change(merge_edges e0=en_uf e0). unfold merge_edges;rewrite(tne_nil _ _ _ _ _ _ WS0);reflexivity.
 - exists e0,les,tm;split;[reflexivity|];split;[exact WS0|reflexivity].

Qed.

Lemma implicit_own_support : forall fuel mi outs ms e needs s e' needs' final,
 (forall t, In t ms -> A.kind_sym WW t = Some A.DKType) ->
 mfold (implicit_type_step fuel mi outs) (e,needs) ms s = Good ((e',needs'),final) ->
 Inv s -> st_w s = w -> ContextClean e -> Tri (en_uf e) -> EnvSupport WW e ->
 ContextClean e' /\ Tri (en_uf e') /\ EnvSupport WW e'.
Proof.
 intros fuel mi outs ms;induction ms as [|t rest IH];intros e needs s e' needs' final
  Types Run Valid Hw Clean Triangular Supported.
 - injection Run as <- <- <-;split;[exact Clean|];split;assumption.
 - cbn [mfold] in Run;apply bind_inv in Run as [[mid required] [state [Step Rest]]].
   destruct (ok_implicit_type_step fuel mi outs (e,needs) t _ _ _ Valid Step) as [I1 F1].
   assert (W1 : st_w state = w) by (rewrite (fr_w _ _ F1);exact Hw).
   destruct (type_symbol_at b w R Fl s t Hload Valid Hw (Types t (or_introl eq_refl)))
    as (g&Loaded&Id&Kind).
   destruct (implicit_type_step_shape fuel mi outs e needs t s mid required state g Loaded Id Kind Step)
    as [Same Shape];subst required.
   destruct (nmem t outs || is_some (tn_get e t)) eqn:Present.
   + subst mid;eapply IH;try eassumption;intros x Member;apply Types;right;exact Member.
   + destruct Shape as [stamp ->].
     eapply IH;[intros x Member;apply Types;right;exact Member|exact Rest|exact I1|exact W1| |exact Triangular|].
     * apply context_clean_type;exact Clean.
     * apply abstract_add_type_support;exact Supported.
Qed.

Lemma abstract_own_merged_support : forall fuel mi args outs ms s e needs final,
 In mi (w_order w) -> (forall t, In t ms -> A.kind_sym WW t = Some A.DKType) ->
 merged_walk mi (own_walk fuel mi args outs ms) s = Good ((e,needs),final) ->
 Inv s -> st_w s = w -> Later w P R Fl final ->
 ContextClean e /\ Tri (en_uf e) /\ EnvSupport WW e.
Proof.
 intros fuel mi args outs ms s e needs final Mod Types Run Valid Hw Last.
 apply (merged_walk_support w (P:=P) R Fl (own_walk fuel mi args outs ms))
  with (mi:=mi) (s:=s) (needs:=needs) (s':=final);try assumption.
 - intro start;apply ok_own_walk.
 - intros U before local required after TriU CleanU SupportU I W0 L Step.
   unfold own_walk in Step;apply bind_inv in Step as [[mid ns] [state [Walk Implicit]]].
   assert (OW : ok (mfold (fun acc sp => add_spec fuel (fst acc) mi sp [] (snd acc)) (env_with_edges [] U,[]) args)).
   { apply ok_mfold;intros;apply ok_add_spec. }
   destruct (OW _ _ _ I Walk) as [I1 F1].
   assert (OI : ok (mfold (implicit_type_step fuel mi outs) (mid,ns) ms)).
   { apply ok_mfold;intros;apply ok_implicit_type_step. }
   destruct (OI _ _ _ I1 Implicit) as [_ F2].
   destruct (support_edges_start WW U CleanU TriU SupportU) as [C0 [T0 S0]].
   destruct (own_specs_support w (P:=P) R Fl b Hload fuel mi args (env_with_edges [] U) []
    before mid ns state Walk I W0 Mod C0 T0 S0 (later_back w P R Fl _ _ F2 L)) as [CM [TM SM]].
   eapply implicit_own_support;try eassumption.
   rewrite (fr_w _ _ F1);exact W0.
Qed.

Theorem abstract_fun_env_support : forall g name names args res s e needs final,
 A.WorldOf b WW -> nget (w_syms w) (gs_id g) = Some g ->
 gs_decl g = ADFunctor name names args res None ->
 fun_env g s = Good ((e,needs),final) -> Inv s -> st_w s = w -> Later w P R Fl final ->
 EnvSupport WW e.
Proof.
 intros g name names args res s e needs final World Loaded Decl Run Valid Hw Last.
 unfold fun_env,functor_parts in Run;rewrite Decl in Run;cbn beta iota zeta in Run.
 apply bind_inv in Run as [outputs [s1 [Outputs Run]]].
 destruct (result_types_rel w R Fl b Hload g name names args res None s outputs s1
  Loaded Decl Outputs Valid Hw) as [-> [_ OutputTypes]].
 apply bind_inv in Run as [mentions [s2 [Mentions Run]]].
 assert (Om : ok (result_mentions g)) by (unfold result_mentions;okt).
 destruct (Om _ _ _ Valid Mentions) as [I2 F2].
 assert (W2 : st_w s2 = w) by (rewrite (fr_w _ _ F2);exact Hw).
 apply bind_inv in Run as [fuel [s3 [Fuel Run]]].
 destruct (ok_ctx_fuel _ _ _ I2 Fuel) as [I3 F3].
 apply bind_inv in Run as [[local required] [s4 [Walk Return]]].
 injection Return as <- <- <-.
 change (merged_walk (gs_mod g) (own_walk fuel (gs_mod g) args outputs mentions) s3 = Good ((local,required),s4)) in Walk.
 assert (OW : ok (merged_walk (gs_mod g) (own_walk fuel (gs_mod g) args outputs mentions))).
 { apply ok_merged_walk_frame;intro;apply ok_own_walk. }
 destruct (OW _ _ _ I3 Walk) as [_ FW].
 assert (L2 : Later w P R Fl s2) by exact (later_back w P R Fl _ _ F3 (later_back w P R Fl _ _ FW Last)).
 assert (Types : forall t, In t mentions -> A.kind_sym WW t = Some A.DKType).
 { exact (actual_result_mentions_types b w R Fl P g name names args res None s mentions s2
   Hload World Loaded Decl Mentions Valid Hw L2). }
 apply freeze_all_apps_support.
 exact (proj2 (proj2 (abstract_own_merged_support fuel (gs_mod g) args outputs mentions s3 local required s4
  (loader_mod_in w R Fl b Hload _ _ Loaded) Types Walk I3
  ltac:(rewrite (fr_w _ _ F3);exact W2) Last))).
Qed.

Theorem abstract_fun_env_body_context : forall g name names args res s e needs final pins variables,
 A.WorldOf b WW -> nget (w_syms w) (gs_id g) = Some g ->
 gs_decl g = ADFunctor name names args res None ->
 fun_env g s = Good ((e,needs),final) -> Inv s -> st_w s = w -> Later w P R Fl final ->
 exists C, A.FunCtx WW (gs_id g) C (map (key_tr Fl) needs) /\
 FunEnvTables Fl e C /\
 CxRel w R Fl (functor_body_context g pins) e (functor_body_frame w g pins variables) C.
Proof.
 intros g name names args res s e needs final pins variables World Loaded Decl Run Valid Hw Last.
 destruct (abstract_fun_env_context_data g name names args res s e needs final
  World Loaded Decl Run Valid Hw Last)
  as (C&Context&Tables&Triangular&Keys&Merges&raw&entries&tm&Eq&Formed&Ceq).
 pose proof (abstract_fun_env_support g name names args res s e needs final
  World Loaded Decl Run Valid Hw Last) as Supported.
 exists C;split;[exact Context|];split;[exact Tables|].
 subst e;eapply (functx_initial_body w R Fl g raw C needs entries tm pins variables);
 try eassumption;[reflexivity|exact (loader_mod_in w R Fl b Hload _ _ Loaded)].
Qed.
End Context.
