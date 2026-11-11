(* A checked list of parameter names keeps its prefix in the slot numbering. *)
From Stdlib Require Import List String Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypesSound.
From NM Require PublicFormation WorldSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Local Definition add_name (names : list string) (n : string) :=
  if smem n names then names else n :: names.

Local Lemma fold_names_distinct : forall input names, NoDup names ->
  NoDup (fold_left add_name input names).
Proof.
  induction input as [|n input IH]; intros names Distinct; [exact Distinct|].
  cbn [fold_left]. apply IH. unfold add_name.
  destruct (smem n names) eqn:Seen; [exact Distinct|].
  constructor; [now apply WorldSound.smem_false|exact Distinct].
Qed.

Local Lemma fold_names_keeps_tail : forall input names,
  exists added, fold_left add_name input names = added ++ names.
Proof.
  induction input as [|n input IH]; intros names; [now exists []|].
  cbn [fold_left]. unfold add_name.
  destruct (smem n names); [exact (IH names)|].
  destruct (IH (n :: names)) as [added Result]. exists (added ++ [n]).
  fold add_name.
  now rewrite Result, <- app_assoc.
Qed.

Local Lemma fold_names_new_prefix : forall input names, NoDup input ->
  (forall n, In n input -> ~ In n names) ->
  fold_left add_name input names = rev input ++ names.
Proof.
  induction input as [|n input IH]; intros names Distinct New; [reflexivity|].
  inversion Distinct as [|? ? NotTail DistinctTail]; subst.
  cbn [fold_left]. unfold add_name.
  destruct (smem n names) eqn:Seen.
  { exfalso. apply smem_spec in Seen. exact (New n (or_introl eq_refl) Seen). }
  fold add_name.
  rewrite IH; [cbn [rev]; now rewrite <- app_assoc|exact DistinctTail|].
  intros x Member [Equal|Old].
  - subst x. contradiction.
  - exact (New x (or_intror Member) Old).
Qed.

Local Lemma suniq_distinct : forall input, NoDup (suniq input).
Proof. intro input. unfold suniq. apply NoDup_rev. apply fold_names_distinct. constructor. Qed.

Local Lemma suniq_parameter_prefix : forall parameters names, NoDup parameters ->
  exists tail, suniq (parameters ++ names) = parameters ++ tail.
Proof.
  intros parameters names Distinct. unfold suniq. rewrite fold_left_app.
  change (exists tail, rev (fold_left add_name names (fold_left add_name parameters [])) =
    parameters ++ tail).
  rewrite (fold_names_new_prefix parameters [] Distinct ltac:(intros ? ? [])).
  rewrite app_nil_r.
  destruct (fold_names_keeps_tail names (rev parameters)) as [tail Keep].
  exists (rev tail). now rewrite Keep, rev_app_distr, rev_involutive.
Qed.

Local Lemma numbered_names : forall names first,
  map fst (number_from first names) = names.
Proof. induction names; intros first; cbn [number_from map]; [reflexivity|now rewrite IHnames]. Qed.

Local Lemma numbered_slots : forall names first,
  map snd (number_from first names) = seq first (List.length names).
Proof. induction names; intros first; cbn [number_from map seq List.length]; [reflexivity|now rewrite IHnames]. Qed.

Local Lemma numbered_lookup : forall names first i n,
  NoDup names -> nth_error names i = Some n ->
  slook n (number_from first names) = Some (first + i).
Proof.
  induction names as [|head names IH]; intros first [|i] n Distinct Position; try discriminate.
  - injection Position as <-. cbn [number_from slook].
    now rewrite String.eqb_refl, Nat.add_0_r.
  - inversion Distinct as [|? ? NotTail DistinctTail]; subst.
    cbn [nth_error] in Position. cbn [number_from slook].
    destruct (String.eqb_spec n head) as [<-|Different].
    + exfalso. apply NotTail. eapply nth_error_In. exact Position.
    + rewrite IH with (i := i); [f_equal; lia|exact DistinctTail|exact Position].
Qed.

Lemma checked_parameters_distinct : forall names, suniq_dup names = None -> NoDup names.
Proof.
  induction names as [|n names IH]; intro Checked; [constructor|].
  cbn [suniq_dup] in Checked. destruct (smem n names) eqn:Seen; [discriminate|].
  constructor; [now apply WorldSound.smem_false|now apply IH].
Qed.

Theorem mk_frame_vars_numbering : forall F this parameters names,
  fr_var F = mk_frame_vars (is_some this) parameters names ->
  NoDup parameters -> Numbering F this parameters.
Proof.
  intros F this parameters names Vars Distinct. unfold Numbering, mk_frame_vars in *.
  rewrite Vars, numbered_names, numbered_slots.
  split; [apply suniq_distinct|split; [apply seq_NoDup|split]].
  - destruct this as [receiver|]; [|exact I].
    intros name Read. apply PublicFormation.slook_in in Read.
    apply in_map with (f := snd) in Read. rewrite numbered_slots in Read.
    apply in_seq in Read. cbn in Read. lia.
  - intros i pn Position.
    destruct (suniq_parameter_prefix parameters names Distinct) as [tail Prefix].
    assert (Read : nth_error (suniq (parameters ++ names)) i = Some pn).
    { rewrite Prefix, nth_error_app1; [exact Position|].
      assert (Defined : nth_error parameters i <> None).
      { intro Missing. change (@nth_error name parameters i = None) in Missing. rewrite Position in Missing. discriminate. }
      exact (proj1 (nth_error_Some parameters i) Defined). }
    replace (match this with Some _ => S i | None => i end)
      with ((if is_some this then 1 else 0) + i) by (destruct this; reflexivity).
    apply numbered_lookup; [apply suniq_distinct|exact Read].
Qed.
