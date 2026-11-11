From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaAssumptionAtoms EaLinkSite.
From NM Require AllocationFacts EaAssumesSound EaDependencyFuel EaMentionsSound EaRootSound
  PublicCallRealize PublicContextFacts PublicTagCheck PublicTypeRead.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Lemma site_classes_spec : forall W u C needs fixed,
  FunCtx W u C needs -> forall y,
  In y (site_classes C fixed) <->
  (exists x tx, nlook x (c_ty C) = Some tx /\ In y (gens tx)) /\ lookup_subst y fixed = None.
Proof.
  intros W u C needs fixed Formed y. unfold site_classes.
  rewrite filter_In, EaMentionsSound.sort_uniq_members, EaAssumesSound.callee_leaves_spec.
  destruct (lookup_subst y fixed) as [t|] eqn:Read; cbn; [intuition discriminate|].
  destruct (PublicCallRealize.functx_scoped W u C needs Formed) as [Row _].
  split.
  - intros [[x [Member Occurs]] _].
    destruct (PublicTagCheck.keys_in _ x Member) as [tx Lookup].
    split; [|reflexivity]. exists x, tx. split; [exact Lookup|].
    now rewrite (Row x tx Lookup).
  - intros [[x [tx [Lookup Occurs]]] _]. split; [|reflexivity].
    exists x. split.
    + apply PublicTypeRead.nlook_in in Lookup. exact (in_map fst _ (x,tx) Lookup).
    + change (In y (gens (app_subst (c_sub C) (TAbs x)))). now rewrite <- (Row x tx Lookup).
Qed.

Theorem site_allocation : forall W bound C recorded private,
  (forall g d, w_decl W g = Some d -> g < bound) ->
  let before := copy_allocation_scope W (recorded ++ private) in
  let batch := map (site_atom bound (recorded ++ private)) (site_classes C (copy_fixed_readings C recorded)) in
  Allocates before batch (extend_allocation_scope before batch).
Proof.
  intros W bound C recorded private Bound before batch. split; [|reflexivity]. split.
  - unfold batch, site_classes. apply AllocationFacts.allocation_rename_nodup.
    + unfold site_atom. lia.
    + apply NoDup_filter, EaRootSound.ascending_nodup, EaDependencyFuel.sort_uniq_ascending.
  - apply Forall_forall. intros g Member.
    apply in_map_iff in Member as [y [<- Member]].
    set (support := flat_map (fun q => gens (snd q)) (recorded ++ private)).
    change (Fresh W (site_atom bound (recorded ++ private) y) /\ ~ In (site_atom bound (recorded ++ private) y) support).
    split.
    + unfold Fresh. destruct (w_decl W (site_atom bound (recorded ++ private) y)) as [d|] eqn:Declared; [exfalso|reflexivity].
      pose proof (EaAssumesSound.scope_floor_world support bound).
      pose proof (Bound _ _ Declared). unfold site_atom in *. unfold support in *. lia.
    + intros Reserved. pose proof (EaAssumesSound.scope_floor_support support bound _ Reserved).
      unfold site_atom in *. unfold support in *. lia.
Qed.
