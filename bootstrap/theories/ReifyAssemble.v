(* Final immutable program records derive both fixed tables from the same
   actual declarations used for the Core erasure signature. No fixed key
   can disappear merely because Core's declaration field is erased. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel CoreMoss ReifyData Reify ReifySig ReifyMaps.
Import ListNotations.

Definition assemble_reified_program (raw : xprogram) (main : fsym)
    (functions : list (fsym * xexpr)) (functors : list (usym * (tele * list xbind)))
    (environment : xdenv) (inputs : list (usym * list tsym))
    (exports : list (usym * smap)) (declarations : list (item * reify_decl))
    : reify_result :=
  ReifyResult (XProgram functions functors (xo_bool raw) (xo_true raw) (xo_false raw)
    main environment (xo_prims raw) (xo_cast raw) inputs exports true
    (reify_fixed_inst_table declarations) (reify_fixed_keys_table declarations)) declarations.

Lemma assembled_runtime_fixed_exact : forall raw main functions functors environment inputs exports declarations,
    let result := assemble_reified_program raw main functions functors environment inputs exports declarations in
    forall Sg argv read u,
      runtime_fixed Sg (program_of (rr_program result) argv read) u =
      reify_fixed_keys (rr_declarations result) u.
Proof.
  intros raw main functions functors environment inputs exports declarations Sg argv read u.
  unfold runtime_fixed. cbn [assemble_reified_program rr_program rr_declarations program_of xp_immutable].
  apply reify_fixed_keys_table_lookup.
Qed.
Lemma assembled_runtime_inst_exact : forall raw main functions functors environment inputs exports declarations,
    let result := assemble_reified_program raw main functions functors environment inputs exports declarations in
    forall argv read u,
      xp_fixed_inst (program_of (rr_program result) argv read) u =
      reify_fixed_inst (rr_declarations result) u.
Proof.
  intros raw main functions functors environment inputs exports declarations argv read u.
  cbn [assemble_reified_program rr_program rr_declarations program_of xp_fixed_inst].
  apply reify_fixed_inst_table_lookup.
Qed.
