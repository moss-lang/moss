(* Fresh input/output schema roles. Each role allocation is disjoint even
   when a source type name occurs in both the input and output interfaces. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit ReifyData Reify.
Import ListNotations.

Fixpoint allocate_type_roles (source : list tsym) (s : reify_state)
    : subst * smap * reify_state :=
  match source with
  | [] => ([],[],s)
  | t :: rest =>
      let '(slot,s1) := lower_type_temporary TUnit s in
      let '(th,sm,s2) := allocate_type_roles rest s1 in
      ((t,TAbs slot) :: th,(ITy t,ITy slot) :: sm,s2)
  end.

Lemma allocate_type_roles_domain : forall source s th sm s',
    allocate_type_roles source s = (th,sm,s') ->
    map fst th = source /\ map fst sm = map ITy source /\
    rf_next s' = rf_next s + length source /\
    rf_names s' = rf_names s /\ rf_generations s' = rf_generations s /\
    rf_items s' = rf_items s.
Proof.
  induction source as [|t rest IH]; intros s th sm s' H.
  - cbn in H. inversion H; subst. cbn. repeat split; try reflexivity; lia.
  - cbn in H. destruct (allocate_type_roles rest
      (ReifyState (S (rf_next s)) (rf_names s) (rf_generations s)
        (rf_items s) ((ITy (rf_next s),RDType) :: rf_declarations s)))
      as [[th1 sm1] s1] eqn:E.
    inversion H; subst. destruct (IH _ _ _ _ E) as [Hth [Hsm [Hnext Hscope]]].
    cbn in *. repeat split; try congruence; try lia; tauto.
Qed.

Lemma allocate_type_roles_targets : forall source s th sm s',
    allocate_type_roles source s = (th,sm,s') ->
    map snd th = map TAbs (seq (rf_next s) (length source)) /\
    map snd sm = map ITy (seq (rf_next s) (length source)).
Proof.
  induction source as [|t rest IH]; intros s th sm s' H.
  - inversion H. auto.
  - cbn in H. destruct (allocate_type_roles rest
      (ReifyState (S (rf_next s)) (rf_names s) (rf_generations s)
        (rf_items s) ((ITy (rf_next s),RDType) :: rf_declarations s)))
      as [[th1 sm1] s1] eqn:E.
    inversion H; subst. destruct (IH _ _ _ _ E) as [Hth Hsm]. cbn in *.
    split; f_equal; assumption.
Qed.

(* Generation roles have no authority to rewrite mutable source names. *)
Definition enter_type_roles (th : subst) (s : reify_state) : reify_state :=
  ReifyState (rf_next s) (rf_names s)
    (th ++ rf_generations s) (rf_items s) (rf_declarations s).

Lemma enter_type_roles_preserves_allocation : forall th s,
    rf_next (enter_type_roles th s) = rf_next s /\
    rf_declarations (enter_type_roles th s) = rf_declarations s /\
    rf_names (enter_type_roles th s) = rf_names s.
Proof. intros. repeat split; reflexivity. Qed.
