(* Two choices for one fresh batch differ by a permutation fixing all
   previously allocated and live identities in the surrounding scope. *)
From Stdlib Require Import List Arith.
From NM Require AuditBase.
From NM Require Import Audit.
From NM Require AllocationFacts.
Import AuditBase.Semantics.Elaboration ListNotations.

Definition swap_atom (x y g : nat) : nat :=
  if Nat.eqb g x then y else if Nat.eqb g y then x else g.

Lemma swap_left : forall x y, swap_atom x y x = y.
Proof. intros x y. unfold swap_atom. now rewrite Nat.eqb_refl. Qed.

Lemma swap_right : forall x y, swap_atom x y y = x.
Proof.
  intros x y. unfold swap_atom.
  destruct (Nat.eqb_spec y x); [assumption|now rewrite Nat.eqb_refl].
Qed.

Lemma swap_fixed : forall x y g, g <> x -> g <> y -> swap_atom x y g = g.
Proof.
  intros x y g X Y. unfold swap_atom.
  destruct (Nat.eqb_spec g x), (Nat.eqb_spec g y); try contradiction; reflexivity.
Qed.

Lemma swap_involution : forall x y g, swap_atom x y (swap_atom x y g) = g.
Proof.
  intros x y g. destruct (Nat.eq_dec g x) as [->|X]; [rewrite swap_left; apply swap_right|].
  destruct (Nat.eq_dec g y) as [->|Y]; [rewrite swap_right; apply swap_left|].
  rewrite (swap_fixed x y g X Y). apply swap_fixed; assumption.
Qed.

Lemma swap_injective : forall x y a b, swap_atom x y a = swap_atom x y b -> a = b.
Proof.
  intros x y a b Equal. apply (f_equal (swap_atom x y)) in Equal.
  now rewrite !swap_involution in Equal.
Qed.

Lemma swap_permitted : forall (permitted : nat -> Prop) x y,
  permitted x -> permitted y -> forall g, permitted g <-> permitted (swap_atom x y g).
Proof.
  intros permitted x y X Y g.
  destruct (Nat.eq_dec g x) as [->|NotX]; [rewrite swap_left; split; intro; assumption|].
  destruct (Nat.eq_dec g y) as [->|NotY]; [rewrite swap_right; split; intro; assumption|].
  rewrite swap_fixed by assumption. reflexivity.
Qed.

Lemma swap_preserves_fresh : forall before x y batch,
  scope_permits before x -> scope_permits before y ->
  ~ In x (scope_allocated before ++ scope_support before) ->
  ~ In y (scope_allocated before ++ scope_support before) ->
  AllocationFresh before batch ->
  AllocationFresh before (map (swap_atom x y) batch).
Proof.
  intros before x y batch X Y AbsentX AbsentY [Distinct Fresh]. split.
  - apply AllocationFacts.allocation_rename_nodup; [apply swap_injective|exact Distinct].
  - rewrite Forall_forall in *. intros g Member.
    apply in_map_iff in Member as [old [<- Member]].
    destruct (Fresh old Member) as [Allowed Absent]. split.
    + exact (proj1 (swap_permitted (scope_permits before) x y X Y old) Allowed).
    + intro Reserved.
      assert (Fixed : swap_atom x y (swap_atom x y old) = swap_atom x y old).
      { apply swap_fixed; intro Equal; rewrite Equal in Reserved; contradiction. }
      rewrite swap_involution in Fixed. rewrite <- Fixed in Reserved. exact (Absent Reserved).
Qed.

Lemma fresh_tail : forall (before : allocation_scope nat) x xs,
  AllocationFresh before (x :: xs) ->
  AllocationFresh (extend_allocation_scope before [x]) xs.
Proof.
  intros before x xs [Distinct Fresh]. inversion Distinct as [|? ? NotIn Tail]; subst.
  split; [exact Tail|]. rewrite Forall_forall in *. intros g Member.
  destruct (Fresh g (or_intror Member)) as [Allowed Absent]. split; [exact Allowed|].
  cbn [extend_allocation_scope scope_allocated scope_support]. intro Reserved.
  apply in_app_or in Reserved as [History|Support].
  - apply in_app_or in History as [Old|[<-|[]]].
    + apply Absent, in_or_app. left. exact Old.
    + exact (NotIn Member).
  - apply Absent, in_or_app. right. exact Support.
Qed.

Definition AtomPermutation (rename : nat -> nat) : Prop :=
  (forall x y, rename x = rename y -> x = y) /\ (forall y, exists x, rename x = y).

Definition BatchRenaming (before : allocation_scope nat) (first second : list nat) (rename : nat -> nat) : Prop :=
  AtomPermutation rename /\ map rename first = second /\
  (forall g, ~ In g first -> ~ In g second -> rename g = g) /\
  (forall g, scope_permits before g <-> scope_permits before (rename g)).

Lemma fresh_batch_renaming_at : forall size (before : allocation_scope nat) first second,
  length first = size -> length second = size ->
  AllocationFresh before first -> AllocationFresh before second ->
  exists rename, BatchRenaming before first second rename.
Proof.
  induction size as [|size IH]; intros before [|x xs] [|y ys] LenX LenY FreshX FreshY; try discriminate.
  - exists (fun g => g). unfold BatchRenaming, AtomPermutation.
    repeat split; try reflexivity; try congruence. intro g. now exists g.
  - injection LenX as LenX. injection LenY as LenY.
    pose proof (Forall_inv (proj2 FreshX)) as [AllowedX AbsentX].
    pose proof (Forall_inv (proj2 FreshY)) as [AllowedY AbsentY].
    pose proof (swap_preserves_fresh before x y _ AllowedX AllowedY AbsentX AbsentY FreshX) as Swapped.
    cbn [map] in Swapped. rewrite swap_left in Swapped.
    destruct (IH (extend_allocation_scope before [y]) (map (swap_atom x y) xs) ys
      ltac:(now rewrite length_map) LenY (fresh_tail _ _ _ Swapped) (fresh_tail _ _ _ FreshY))
      as (tail & [Injective Surjective] & MapTail & FixedTail & AllowedTail).
    pose proof (proj1 Swapped) as DistinctX. pose proof (proj1 FreshY) as DistinctY.
    apply NoDup_cons_iff in DistinctX as [NotInX _].
    apply NoDup_cons_iff in DistinctY as [NotInY _].
    exists (fun g => tail (swap_atom x y g)).
    unfold BatchRenaming, AtomPermutation. split.
    + split.
      * intros a b Equal. apply (swap_injective x y), Injective. exact Equal.
      * intro g. destruct (Surjective g) as [middle <-].
        exists (swap_atom x y middle). now rewrite swap_involution.
    + split.
      * cbn [map]. rewrite swap_left, (FixedTail y NotInX NotInY), <- map_map, MapTail. reflexivity.
      * split.
        -- intros g OutsideX OutsideY.
           assert (Fixed : swap_atom x y g = g).
           { apply swap_fixed; intro Same; subst g; [apply OutsideX|apply OutsideY]; left; reflexivity. }
           rewrite Fixed. apply FixedTail.
           ++ intros Member. apply in_map_iff in Member as [old [Same Present]].
              assert (old = g) by (apply (swap_injective x y); congruence). subst old.
              apply OutsideX. right. exact Present.
           ++ intros Present. apply OutsideY. right. exact Present.
        -- intro g. transitivity (scope_permits before (swap_atom x y g)).
           ++ apply swap_permitted; assumption.
           ++ exact (AllowedTail (swap_atom x y g)).
Qed.

Theorem fresh_batches_are_renamings : forall before first second,
  length first = length second ->
  AllocationFresh before first -> AllocationFresh before second ->
  exists rename, BatchRenaming before first second rename.
Proof.
  intros before first second Len FreshFirst FreshSecond.
  eapply fresh_batch_renaming_at with (size := length second); eassumption || reflexivity.
Qed.

Theorem allocated_batches_alpha : forall before first second after_first after_second,
  length first = length second ->
  Allocates before first after_first -> Allocates before second after_second ->
  exists rename,
    BatchRenaming before first second rename /\
    AllocationFacts.rename_allocation_scope rename (scope_permits before) before = before /\
    AllocationFacts.rename_allocation_scope rename (scope_permits before) after_first = after_second.
Proof.
  intros before first second after_first after_second Len [FreshFirst ->] [FreshSecond ->].
  destruct (fresh_batches_are_renamings before first second Len FreshFirst FreshSecond)
    as [rename Renames].
  pose proof Renames as (_ & MapBatch & FixedOutside & _).
  assert (Fixed : forall g, In g (scope_allocated before ++ scope_support before) -> rename g = g).
  { intros g Reserved. apply FixedOutside;
      intros InBatch.
    - pose proof (proj2 FreshFirst) as Fresh. rewrite Forall_forall in Fresh.
      exact (proj2 (Fresh g InBatch) Reserved).
    - pose proof (proj2 FreshSecond) as Fresh. rewrite Forall_forall in Fresh.
      exact (proj2 (Fresh g InBatch) Reserved). }
  assert (MapHistory : map rename (scope_allocated before) = scope_allocated before).
  { rewrite <- (map_id (scope_allocated before)) at 2. apply map_ext_in. intros g Old. apply Fixed, in_or_app. left. exact Old. }
  assert (MapSupport : map rename (scope_support before) = scope_support before).
  { rewrite <- (map_id (scope_support before)) at 2. apply map_ext_in. intros g Live. apply Fixed, in_or_app. right. exact Live. }
  exists rename. split; [exact Renames|]. split.
  - destruct before as [permits history support].
    unfold AllocationFacts.rename_allocation_scope. cbn in *. now rewrite MapHistory, MapSupport.
  - unfold AllocationFacts.rename_allocation_scope, extend_allocation_scope. cbn.
    now rewrite map_app, MapHistory, MapSupport, MapBatch.
Qed.
