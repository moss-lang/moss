(* Bind the default closure engine to Audit's interpreter contract.
   Optional scalar folding is proved within LazyClo's expression compiler. *)
From Stdlib Require Import List.
From NM Require Import ComponentContracts.
From NM Require Import Audit.
From NM Require EarlyCertified LazyX LazyClo LazyInterpFacts LazyCloFacts FastPath Prune.

Module Type DIRECT_PIPELINE := ORACLE_PIPELINE.
Module DirectSpec := OracleSpec.

(* Extra component results support the one public interpreter contract. *)
Module Type DIRECT_AGREEMENT (C : PROVED_NOW) (Engine : DIRECT_PIPELINE C).
  Include ORACLE_AGREEMENT C Engine.
  Import Spec Engine.
  Parameter mapped_run : nat -> xprog -> fsym -> xdenv -> C.Prims.mstate -> C.X.xres.
  Axiom mapped_engine_agrees : forall fuel P main d st,
    mapped_run fuel P main d st = C.X.run_main fuel P main d st.
  Axiom compiled_engine_agrees : forall P fns funs fuel main d st,
    CompiledTables P fns funs ->
    engine_run fns fuel main d st = C.X.run_main fuel P main d st.
  Axiom fast_path_agrees : forall p argv read fuel st,
    out_of_fuel (Ref.run p argv read fuel st) \/
    Ref.run (fast_program p) argv read fuel st = Ref.run p argv read fuel st.
  Axiom engine_agrees : forall p argv read fuel st fns funs,
    Prepared p argv read fns funs ->
    stuck (Ref.run p argv read fuel st) \/
    engine_run fns fuel (xo_main p) (xo_env p) st = Ref.run p argv read fuel st.
  Axiom fast_engine_agrees : forall p argv read fuel st fns funs,
    Prepared (fast_program p) argv read fns funs ->
    out_of_fuel (Ref.run p argv read fuel st) \/
    stuck (Ref.run p argv read fuel st) \/
    engine_run fns fuel (xo_main (fast_program p)) (xo_env (fast_program p)) st =
      Ref.run p argv read fuel st.
End DIRECT_AGREEMENT.

Module Pipeline <: DIRECT_PIPELINE EarlyCertified.Certified.
  Definition code := LazyClo.code.
  Definition bind_code := LazyClo.bcode.
  Definition function_table := fsym -> option code.
  Definition functor_table := usym -> option (tele * bind_code).
  Definition compile_expr := LazyClo.cc.
  Definition compile_binds := LazyClo.ccbs.
  Definition engine_run := LazyClo.crun_main.
  Definition mapped_run := LazyX.frun_main.
  Definition fast_program := FastPath.fast_program.
  Definition prune_program := Prune.oracle_program.
End Pipeline.

Module Certified <: DIRECT_AGREEMENT EarlyCertified.Certified Pipeline.
  Module Spec := DirectSpec EarlyCertified.Certified Pipeline.
  Definition mapped_run := LazyX.frun_main.
  Definition interpreter_correct := @LazyCloFacts.fast_engine_agrees.
  Definition mapped_engine_agrees := LazyInterpFacts.frun_main_eq.
  Definition compiled_engine_agrees := @LazyCloFacts.crun_main_eq.
  Definition fast_path_agrees := FastPath.fast_run.
  Definition engine_agrees := @LazyCloFacts.engine_agrees.
  Definition fast_engine_agrees := @LazyCloFacts.fast_engine_agrees.
End Certified.
