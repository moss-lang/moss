(* Runtime tests on tags, fields and control flow use the same source
   labels and local positions after a symbol renaming. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit PublicValueTyping RuntimeRenaming.
Import Audit.Semantics ListNotations.

Module ControlRenaming (Prims : PRIMITIVES) (I : INTERP Prims).
Section Control.
Variable rename : nat -> nat.
Hypothesis injective : forall left right, rename left = rename right -> left = right.

Local Definition renamed_fields (fields : list (fld * value)) :=
  map (fun '(name, field) => (name, rename_value rename field)) fields.

Lemma rename_assoc_fld : forall field fields,
  I.assoc_fld field (renamed_fields fields) =
  option_map (rename_value rename) (I.assoc_fld field fields).
Proof.
  intros field fields. induction fields as [|[name w] rest IH]; [reflexivity|].
  unfold renamed_fields in IH. cbn [renamed_fields map I.assoc_fld].
  destruct (String.eqb field name); [reflexivity|exact IH].
Qed.

Lemma rename_get_field : forall field w,
  I.get_field field (rename_value rename w) =
  option_map (rename_value rename) (I.get_field field w).
Proof.
  intros field [|tag payload|fields|sort bits|address header]; try reflexivity.
  - destruct payload; try reflexivity. apply rename_assoc_fld.
  - apply rename_assoc_fld.
Qed.

Lemma rename_bind_fields : forall bindings fields,
  I.bind_fields bindings (renamed_fields fields) =
  option_map (rename_locals rename) (I.bind_fields bindings fields).
Proof.
  induction bindings as [|[field position] rest IH]; intros fields; [reflexivity|].
  cbn [I.bind_fields]. rewrite rename_assoc_fld, IH.
  destruct (I.assoc_fld field fields), (I.bind_fields rest fields); reflexivity.
Qed.

Definition rename_match_result (result : I.pm) : I.pm :=
  match result with
  | I.PMatch bindings => I.PMatch (rename_locals rename bindings)
  | I.PNo => I.PNo
  | I.PBad => I.PBad
  end.

Lemma rename_match_pat : forall pat w,
  I.match_pat (rename_pattern rename pat) (rename_value rename w) =
  rename_match_result (I.match_pat pat w).
Proof.
  intros [[position|]|tag binder fields] [|n payload|record|sort bits|address header];
    cbn [I.match_pat rename_pattern rename_value]; try reflexivity.
  rewrite rename_eqb by exact injective. destruct (Nat.eqb tag n); [|reflexivity].
  destruct binder as [position|], fields as [|field fields]; try reflexivity.
  all: destruct payload; cbn [rename_value]; try reflexivity.
  all: change (map (fun '(name, field) => (name, rename_value rename field)) l) with (renamed_fields l).
  all: rewrite rename_bind_fields.
  all: destruct (I.bind_fields (field :: fields) l) as [g|]; [|reflexivity].
  all: cbn [option_map rename_match_result]; unfold rename_locals; now rewrite map_app.
Qed.

Lemma rename_select_arm : forall arms w,
  I.select_arm (map (fun '(pat, body) => (rename_pattern rename pat, rename_expr rename body)) arms)
    (rename_value rename w) =
  option_map (fun '(g, body) => (rename_locals rename g, rename_expr rename body)) (I.select_arm arms w).
Proof.
  intros arms w. induction arms as [|[pat body] rest IH]; [reflexivity|].
  cbn [I.select_arm map]. rewrite rename_match_pat.
  destruct (I.match_pat pat w); [reflexivity|exact IH|reflexivity].
Qed.

Lemma rename_truthy : forall P P' w,
  views_related rename P P' ->
  I.truthy P' (rename_value rename w) = I.truthy P w.
Proof.
  intros P P' [|tag payload|fields|sort bits|address header] Views; try reflexivity.
  destruct payload as [|truth payload|fields|sort bits|address header]; try reflexivity.
  destruct payload; try reflexivity.
  cbn [I.truthy rename_value].
  rewrite (related_bool _ _ _ Views), (related_true _ _ _ Views), (related_false _ _ _ Views).
  now rewrite !rename_eqb by exact injective.
Qed.
End Control.
End ControlRenaming.
