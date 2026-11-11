(* Scalar and WASI arguments and results cross the concrete machine's
   value boundary here. Heap payloads are stored as exact Audit values. *)
From Stdlib Require Import List ZArith PrimInt63 Uint63.
From NM Require Import Audit AuditBase.
From NM Require ExecutionModel MossX.
Import Audit.Semantics ListNotations.
Import ExecutionModel (xvalue(..), xout(..), xprog(..), xty(..), xvty, base_primitive).

Definition legacy_view (P : program_view) : xprog :=
  mkXProg (fun _ => None) (fun _ => None) (view_bool P) (view_true P) (view_false P)
    (view_arguments P) (view_read P) [] (fun _ => []) (fun _ => []) false
    (fun _ => []) (fun _ => []).

Definition legacy_shape (shape : shape) : xty :=
  match shape with
  | ShapeI32 => XTI32 | ShapeI64 => XTI64
  | ShapeF32 => XTF32 | ShapeF64 => XTF64
  | ShapeUnit | ShapeNever => XTUnit | ShapeBool => XTBool
  | ShapeAddress => XTAddr | ShapeTag => XTTag
  end.

Fixpoint to_scalar (value : value) : xvalue :=
  match value with
  | VTag n payload => XVTag n (to_scalar payload)
  | VScalar 0 bits => XVI32 (MossX.w32 (Uint63.of_Z bits))
  | VScalar 1 bits => XVI64 (MossX.w64 bits)
  | VScalar 2 bits => XVF32 (MossX.f32_of_bits (MossX.w32 (Uint63.of_Z bits)))
  | VScalar 3 bits =>
      XVF64 (MossX.f64_of_bits (MossX.w32 (Uint63.of_Z bits))
        (MossX.w32 (Uint63.of_Z (Z.shiftr bits 32))))
  | _ => XVUnit
  end.

Fixpoint from_value (w : xvalue) : value :=
  match w with
  | XVTag n payload => VTag n (from_value payload)
  | XVI32 i => VScalar 0 (Uint63.to_Z i)
  | XVI64 bits => VScalar 1 bits
  | XVF32 f => VScalar 2 (Uint63.to_Z (MossX.f32_bits f))
  | XVF64 f =>
      let '(lo, hi) := MossX.f64_bits f in
      VScalar 3 (Z.lor (Uint63.to_Z lo) (Z.shiftl (Uint63.to_Z hi) 32))
  | XVRef a => VRef a None
  | XVStored v => v
  | _ => VUnit
  end.

Lemma to_scalar_shape : forall P v shape,
  value_shape P v shape -> xvty (legacy_view P) (to_scalar v) (legacy_shape shape).
Proof.
  intros P v shape. destruct shape, v; cbn; try tauto.
  all: repeat first
    [ solve [tauto]
    | match goal with
      | v : value |- _ => destruct v; cbn
      | |- context [match ?n with O => _ | S _ => _ end] =>
          destruct n as [|[|[|[|n]]]]; cbn
      end ].
Qed.

Lemma from_value_shape : forall P v shape,
  shape <> ShapeNever ->
  xvty (legacy_view P) v (legacy_shape shape) -> value_shape P (from_value v) shape.
Proof.
  intros P v shape NotNever. destruct shape; try (contradict NotNever; reflexivity).
  all: destruct v; cbn; try tauto.
  all: repeat first
    [ solve [tauto]
    | match goal with
      | v : xvalue |- _ => destruct v; cbn
      | |- context [MossX.f64_bits ?f] => destruct (MossX.f64_bits f); cbn
      end ].
Qed.

Definition map_signature (s : list shape * shape) : list xty * xty :=
  (map legacy_shape (fst s), legacy_shape (snd s)).

Lemma legacy_signature : forall p,
  ExecutionModel.prim_sig (base_primitive p) = option_map map_signature (prim_sig p).
Proof.
  destruct p; try reflexivity.
  all: repeat match goal with
    | x : conv |- _ => destruct x
    | x : i64un |- _ => destruct x
    | x : conv64 |- _ => destruct x
    | x : bool |- _ => destruct x
    end; reflexivity.
Qed.

Lemma legacy_method_signature : forall p,
  ExecutionModel.prim_msig (base_primitive p) = option_map map_signature (prim_msig p).
Proof. destruct p; reflexivity. Qed.

Lemma scalar_never : forall p ps, prim_sig p = Some (ps, ShapeNever) ->
  p = PUnreachable /\ ps = [].
Proof.
  destruct p; intros ps Sig; try discriminate.
  all: repeat match goal with
    | x : conv |- _ => destruct x
    | x : i64un |- _ => destruct x
    | x : conv64 |- _ => destruct x
    | x : bool |- _ => destruct x
    end; try discriminate.
  injection Sig as <-. auto.
Qed.

Lemma method_has_result : forall p ps, prim_msig p <> Some (ps, ShapeNever).
Proof. destruct p; discriminate. Qed.

Definition from_signal (out : xout) : signal :=
  match out with
  | XOk w => Normal (from_value w)
  | XRet w => Returning (from_value w)
  | XBrk => Breaking
  | XErr n => Stuck n
  | XTrap n => Trap n
  | XExit n => Exit n
  | XUns n => UnsupportedOperation n
  | XFuel => OutOfFuel
  end.

Definition run_scalar (P : program_view) (p : wasm_prim) (values : list value)
    (state : MossX.mstate) : signal * MossX.mstate :=
  let '(out, next) := MossX.run_prim_fast (legacy_view P) (base_primitive p) (map to_scalar values) state in
  (from_signal out, next).

Lemma to_scalar_args : forall P values params,
  Forall2 (value_shape P) values params ->
  Forall2 (xvty (legacy_view P)) (map to_scalar values) (map legacy_shape params).
Proof. intros P values params Args. induction Args; cbn; constructor; auto using to_scalar_shape. Qed.

Lemma scalar_result : forall P result out,
  result <> ShapeNever ->
  (match out with
  | XOk w => xvty (legacy_view P) w (legacy_shape result)
  | XTrap _ | XExit _ => True
  | _ => False
  end) ->
  AuditBase.BaseLaws.primitive_result P result (from_signal out).
Proof. intros P result out NotNever; destruct out; cbn; auto using from_value_shape. Qed.

Theorem scalar_sound : forall P p params result values state,
  prim_sig p = Some (params, result) ->
  Forall2 (value_shape P) values params ->
  AuditBase.BaseLaws.primitive_result P result (fst (run_scalar P p values state)).
Proof.
  intros P p params [| | | | | | | |] values state Sig Args; try (
    unfold run_scalar; rewrite MossX.run_prim_fast_spec;
    pose proof (legacy_signature p) as Sig'; rewrite Sig in Sig';
    pose proof (@MossX.prim_sound (legacy_view P) (base_primitive p)
      (map legacy_shape params) _ _ state Sig' (to_scalar_args P _ _ Args)) as Sound;
    destruct (MossX.run_prim _ _ _ _) as [out next]; apply scalar_result; [discriminate|exact Sound]).
  destruct (scalar_never p _ Sig) as [-> ->]. inversion Args. reflexivity.
Qed.

Theorem scalar_method_sound : forall P p params result values state,
  prim_msig p = Some (params, result) ->
  Forall2 (value_shape P) values params ->
  AuditBase.BaseLaws.primitive_result P result (fst (run_scalar P p values state)).
Proof.
  intros P p params result values state Sig Args.
  unfold run_scalar. rewrite MossX.run_prim_fast_spec.
  pose proof (legacy_method_signature p) as Sig'. rewrite Sig in Sig'.
  pose proof (@MossX.prim_msound (legacy_view P) (base_primitive p)
    (map legacy_shape params) _ _ state Sig' (to_scalar_args P _ _ Args)) as Sound.
  destruct (MossX.run_prim _ _ _ _) as [out next]. apply scalar_result; [|exact Sound].
  intros ->. exact (method_has_result p _ Sig).
Qed.
