(* A defined functor's own context comes only from its declared inputs;
   result mentions do not become implicit requirements of its body. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTy Merge CanonTypes CtxWalk CtxMerge FunctorContext
 ResultMentionsExact NestingTypesExec FunctorScopeTypes NeedKeys.
Import ListNotations.
Local Open Scope list_scope.
Module A := ExecutionModel.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Section Context.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Notation W := (BodySoundCore.W w R Fl).

Theorem defined_fun_env_context_data : forall g name names args res body s e needs final,
 A.WorldOf b W -> nget (w_syms w) (gs_id g) = Some g ->
 gs_decl g = ADFunctor name names args res (Some body) ->
 fun_env g s = Good ((e,needs),final) ->
 Inv s -> st_w s = w -> Later w P R Fl final ->
 exists C, A.FunCtx W (gs_id g) C (map (key_tr Fl) needs) /\
 FunEnvTables Fl e C /\ Tri (en_uf e) /\
 Forall (fun key => key_ok key = true) needs /\ merge_edges e = en_uf e /\
 (exists raw entries tm, e=freeze_all_apps raw /\
  WS Fl (en_uf raw) raw tm entries needs /\
  C = A.entries_cx (suf Fl (en_uf raw)) tm (map (tr_ent Fl) entries)).
Proof.
 intros g name names args res body s e needs final World Loaded Decl Run Valid Hw Last.
 unfold fun_env, functor_parts in Run; rewrite Decl in Run; cbn beta iota zeta in Run.
 apply bind_inv in Run as [outputs [s1 [Outputs Run]]].
 destruct (result_types_rel w R Fl b Load g name names args res (Some body) s outputs s1
   Loaded Decl Outputs Valid Hw) as [-> [_ OutputTypes]].
 apply bind_inv in Run as [mentions [s2 [Mentions Run]]].
 assert (Om : ok (result_mentions g)) by (unfold result_mentions;okt).
 destruct (Om _ _ _ Valid Mentions) as [I2 F2].
 assert (W2 : st_w s2 = w) by (rewrite (fr_w _ _ F2);exact Hw).
 apply bind_inv in Run as [fuel [s3 [Fuel Run]]].
 destruct (ok_ctx_fuel _ _ _ I2 Fuel) as [I3 F3].
 apply bind_inv in Run as [[local required] [s4 [Walk Return]]].
 injection Return as <- <- <-.
 change (merged_walk (gs_mod g) (own_walk fuel (gs_mod g) args outputs []) s3 =
   Good ((local,required),s4)) in Walk.
 assert (Finj : forall a c, Fl a = Fl c -> a = c) by exact (A.wo_fld World).
 destruct (own_merged w (P:=P) R Fl b Load Finj fuel (gs_mod g) args outputs [] s3 local required s4
   (loader_mod_in w R Fl b Load _ _ Loaded) ltac:(intros ty Impossible;contradiction)
   Walk I3 ltac:(rewrite (fr_w _ _ F3);exact W2) Last) as
   (entries & tm & Reading & Formed & Merged & Needs).
 destruct Reading as (initial & initialTypes & added & Inputs & Added & EntryReading & TypeReading).
 assert (NoImplicit : added = []).
 { destruct added as [|first rest]; [reflexivity|].
   exfalso;destruct (proj1 (Added first) (or_introl eq_refl)) as [Impossible _]; contradiction. }
 subst added; cbn [map rev app] in EntryReading,TypeReading.
 rewrite app_nil_r in EntryReading.
 set (sigma := suf Fl (en_uf local)).
 exists (A.entries_cx sigma tm (map (tr_ent Fl) entries)); split.
 - exists (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)),
     names,args,res,(Some body),initial,initialTypes,tm,sigma, [].
   split; [exact (w_decl_rd w R Fl _ _ Loaded)|]; split.
   + cbn [A.d_ast A.functor_parts];rewrite Decl;reflexivity.
   + split; [exact Inputs|]; split.
     * intro ty;split;[contradiction|intros [Impossible _];discriminate Impossible].
     * split; [exact TypeReading|]; split.
       -- rewrite <- EntryReading;exact Merged.
       -- split; [rewrite EntryReading;reflexivity|rewrite <- EntryReading;exact Needs].
 - split.
   + constructor.
     * reflexivity.
     * intro key;exact (ctx_ty Fl (en_uf local) local tm entries required Formed eq_refl key).
     * intro value;exact (ctx_vals Fl (en_uf local) local tm entries required Formed eq_refl value).
     * intro fn;exact (ctx_fns Fl (en_uf local) local tm entries required Formed eq_refl fn).
     * intros receiver method Good Canonical;
         exact (ctx_mths Fl Finj (en_uf local) local tm entries required Formed eq_refl receiver method Good Canonical).
     * intro functor;exact (ctx_funpack Fl Finj (en_uf local) local tm entries required Formed eq_refl functor).
     * intros key NoFunctor Good;
         exact (ctx_apps Fl Finj (en_uf local) local tm entries required Formed eq_refl key NoFunctor Good).
   + split; [exact (ws_tri _ _ _ _ _ _ Formed)|].
     split; [exact (ws_needs_ok Fl (en_uf local) local tm entries required Formed eq_refl)|].
     split.
     * change(merge_edges local=en_uf local).
       unfold merge_edges;rewrite (tne_nil _ _ _ _ _ _ Formed);reflexivity.
     * exists local,entries,tm;split;[reflexivity|];split;[exact Formed|reflexivity].
Qed.
Theorem defined_fun_env_context_valid : forall g name names args res body s e needs final,
 A.WorldOf b W -> nget (w_syms w) (gs_id g) = Some g ->
 gs_decl g = ADFunctor name names args res (Some body) ->
 fun_env g s = Good ((e,needs),final) ->
 Inv s -> st_w s = w -> Later w P R Fl final ->
 exists C, A.FunCtx W (gs_id g) C (map (key_tr Fl) needs) /\
 FunEnvTables Fl e C /\ Tri (en_uf e) /\
 Forall (fun key => key_ok key = true) needs /\ merge_edges e = en_uf e.
Proof.
 intros g name names args res body s e needs final World Loaded Decl Run Valid Hw Last.
 destruct(defined_fun_env_context_data g name names args res body s e needs final
   World Loaded Decl Run Valid Hw Last)as(C&Context&Tables&Triangular&Keys&Merge&Data).
 exists C;split;[exact Context|];split;[exact Tables|];split;[exact Triangular|];split;assumption.
Qed.
End Context.
