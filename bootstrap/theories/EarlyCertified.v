(* Early runtime implementations and the public interpreter correctness binding. *)
From Stdlib Require Import List.
From NM Require Import Audit CoreMoss MossX Elab Specialization RelFacts.
From NM Require FastPath CachedClo CachedCloFacts SlotX FoldX Prune.
Import ListNotations.

Module Certified <: PROVED_NOW.
  Definition elaborate := Elab.elaborate.
  Definition interp := CoreInterp.interp.

  Definition eval_deterministic := CoreMoss.eval_deterministic.
  Definition interp_sound := CoreMoss.interp_sound.
  Definition interp_complete := CoreMoss.interp_complete.
  Definition spec_eval := Specialization.spec_eval.

  Module Prims := MossX.ConcretePrims.
  Module X := MossX.XS.
  Definition xinterp_conservative := MossX.xinterp_conservative.
  Definition prim_law := MossX.prim_sound.
  Definition prim_mlaw := MossX.prim_msound.
  Definition prim_ulaw := MossX.prim_usound.

  Definition Typed := CoreMoss.wf_prog.
  Lemma typed_safe : forall Sg main body fuel,
    Typed Sg main -> g_fn Sg main = Some ([], mkSig [] TUnit, Some body) ->
    interp fuel Sg [] [] body <> Err.
  Proof. intros Sg main body fuel H Hm. exact (proj1 (@soundness_main Sg main body fuel H Hm)). Qed.
  Lemma typed_inhabited : exists Sg main u, Typed Sg main /\ g_fun Sg u <> None.
  Proof.
    exists FunctorExample.Sg, FunctorExample.fMain, 1. split.
    - exact functor_example_ok.
    - intro H. vm_compute in H. discriminate H.
  Qed.

  Definition method_choice_unique := RelFacts.method_choice_unique.
End Certified.

Module OraclePipeline <: ORACLE_PIPELINE Certified.
  Definition slot_program := SlotX.rprog.
  Definition slot_expr := SlotX.rexpr.
  Definition slot_bind := SlotX.rbind.
  Definition code := CachedClo.code.
  Definition bind_code := CachedClo.bcode.
  Definition scalar_cache := option (nat * CachedClo.cached_env).
  Definition fast_program := FastPath.fast_program.
  Definition slot_input p := SlotX.rprog_of (Prune.oracle_program p).
  Definition slot_runtime := SlotX.rp_x.
  Definition slot_function := SlotX.rp_fn.
  Definition slot_functor := SlotX.rp_fun.
  Definition fold_expr := FoldX.guard_fold_e.
  Definition fold_bind := FoldX.guard_fold_b.
  Definition function_table := fsym -> option (code * code).
  Definition functor_table := usym -> option (tele * bind_code * scalar_cache).
  Definition compile_expr := CachedClo.cc.
  Definition compile_binds := CachedClo.ccbs.
  Definition prepare_scalars := CachedClo.prepare_scalar_cache.
  Definition engine_run := CachedClo.crun_main.
End OraclePipeline.

Module OracleCertified <: ORACLE_AGREEMENT Certified OraclePipeline.
  Module Spec := OracleSpec Certified OraclePipeline.
  Import Spec OraclePipeline.

  Lemma option_map_pair {A B C : Type} (f : A * B -> C) pair :
    option_map f pair =
      match pair with Some (a, b) => Some (f (a, b)) | None => None end.
  Proof. destruct pair as [[a b]|]; reflexivity. Qed.

  Lemma interpreter_correct : forall p argv read fuel st t,
    Prepared (fast_program p) argv read t ->
    out_of_fuel (Ref.run p argv read fuel st) \/
    stuck (Ref.run p argv read fuel st) \/
    run_prepared (fast_program p) t fuel st = Ref.run p argv read fuel st.
  Proof.
    intros p argv read fuel st t (Tb & X & Fn & Fun & Fns & Funs).
    setoid_rewrite option_map_pair in Fn. setoid_rewrite option_map_pair in Fun.
    setoid_rewrite option_map_pair in Fns. setoid_rewrite option_map_pair in Funs.
    assert (Tables : CachedCloFacts.tbl_ok (folded_program t) (functions t) (functors t)).
    { split; [exact Fns | exact Funs]. }
    exact (@CachedCloFacts.fast_engine_agrees p argv read fuel st _ _ _ _ _ _ Tb X Fn Fun Tables).
  Qed.
End OracleCertified.
