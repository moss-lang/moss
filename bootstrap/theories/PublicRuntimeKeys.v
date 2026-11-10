(* Proof interface for runtime provision keys.  The normalization belongs to
   this transitional instance: stored-key lookup can supply a later instance. *)
From Stdlib Require Import List.
From NM Require Import Audit PublicContextFacts PublicCallRealize.
From NM Require PublicMerge.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Record RuntimeKeys (C : A.cx) := {
  runtime_key : item -> item;
  runtime_key_proper : forall k k', A.KeyEq (A.c_merge C) k k' ->
    runtime_key k = runtime_key k'
}.

Definition normalized_rkey (C : A.cx) (k : item) : item :=
  A.rkey_of C (A.canon_item C k).

Lemma rkey_of_provision_id : forall C k,
  A.rkey_of C (A.provision_id k) = A.rkey_of C k.
Proof. intros C []; reflexivity. Qed.

Lemma canon_provision_id : forall C k,
  A.canon_item C (A.provision_id k) = A.provision_id (A.canon_item C k).
Proof. intros C []; reflexivity. Qed.

Lemma normalized_rkey_proper : forall C, CxSection C ->
  forall k k', A.KeyEq (A.c_merge C) k k' ->
    normalized_rkey C k = normalized_rkey C k'.
Proof.
  intros C Section k k' Equal.
  apply (proj1 (PublicMerge.section_key_eq Section k k')) in Equal.
  change (A.canon_item C (A.provision_id k) =
          A.canon_item C (A.provision_id k')) in Equal.
  rewrite !canon_provision_id in Equal.
  unfold normalized_rkey. rewrite <- (rkey_of_provision_id C (A.canon_item C k)),
    <- (rkey_of_provision_id C (A.canon_item C k')), Equal. reflexivity.
Qed.

Definition canonical_runtime_keys (C : A.cx) (Section : CxSection C) : RuntimeKeys C :=
  {| runtime_key := normalized_rkey C;
     runtime_key_proper := normalized_rkey_proper C Section |}.

(* Existing emitted keys are unchanged whenever their source identity has
   already been normalized.  Method annotations are immaterial to rkey_of. *)
Lemma normalized_rkey_stored : forall C k,
  A.canon_item C (A.provision_id k) = A.provision_id k ->
  normalized_rkey C k = A.rkey_of C k.
Proof.
  intros C k Canonical. unfold normalized_rkey.
  rewrite <- rkey_of_provision_id, <- canon_provision_id, Canonical.
  apply rkey_of_provision_id.
Qed.

Lemma normalized_rkey_eq_iff : forall C, CxSection C -> forall k k',
  normalized_rkey C k = normalized_rkey C k' <-> A.KeyEq (A.c_merge C) k k'.
Proof.
  intros C Section k k'. split; [|apply normalized_rkey_proper; exact Section].
  intro Equal. apply (proj2 (PublicMerge.section_key_eq Section k k')).
  destruct k; destruct k'; cbn [normalized_rkey A.canon_item app_subst_item
    A.provision_id A.rkey_of] in *; try discriminate; try congruence.
  unfold A.rkey in Equal. injection Equal as -> -> _. reflexivity.
Qed.

Definition runtime_lookup (C : A.cx) (keys : RuntimeKeys C)
    (k : item) (d : environment) : option provision_value :=
  lookup_provision (runtime_key C keys k) d.

Lemma runtime_lookup_proper : forall C (keys : RuntimeKeys C) d k k',
  A.KeyEq (A.c_merge C) k k' ->
  runtime_lookup C keys k d = runtime_lookup C keys k' d.
Proof.
  intros C keys d k k' Equal. unfold runtime_lookup.
  now rewrite (runtime_key_proper C keys k k' Equal).
Qed.

(* Method annotations belong to emitted keys.  The static provision-kind
   invariant is checked on their identities. *)
Definition KeysMatchWith (rk : item -> item) (provs : list (item * A.provision)) : Prop :=
  PublicCallRealize.KeysMatch
    (map (fun q => (A.provision_id (rk (fst q)), snd q)) provs).

Lemma keys_match_with_normalized : forall C provs,
  PublicCallRealize.KeysMatch provs -> KeysMatchWith (normalized_rkey C) provs.
Proof.
  intros C provs Match k p Present.
  apply in_map_iff in Present as [[k0 p0] [Equal Present]].
  cbn in Equal. injection Equal as <- <-.
  specialize (Match k0 p0 Present).
  destruct k0 as [t|v|f|r m th|u]; destruct p0;
    cbn [normalized_rkey A.canon_item app_subst_item A.rkey_of A.rkey
      A.provision_id] in *; try contradiction; try exact I; destruct th; contradiction.
Qed.

Lemma keys_match_with_normalized_iff : forall C provs,
  PublicCallRealize.KeysMatch provs <->
  (forall k p, In (k, p) provs -> PublicCallRealize.NeedShape k) /\
  KeysMatchWith (normalized_rkey C) provs.
Proof.
  intros C provs. split.
  - intro Match. split; [|apply keys_match_with_normalized; exact Match].
    intros k p Present. specialize (Match k p Present).
    destruct k as [t|v|f|r m th|u];
      cbn [PublicCallRealize.NeedShape] in *; try exact I.
    + destruct p; contradiction.
    + destruct th; [reflexivity|destruct p; contradiction].
  - intros [Shapes Match] k p Present.
    pose proof (Shapes k p Present) as Shape.
    assert (Mapped : In (A.provision_id (normalized_rkey C k), p)
      (map (fun q => (A.provision_id (normalized_rkey C (fst q)), snd q)) provs)).
    { apply in_map_iff. exists (k, p). auto. }
    specialize (Match _ _ Mapped).
    destruct k as [t|v|f|r m th|u]; destruct p;
      cbn [PublicCallRealize.NeedShape normalized_rkey A.canon_item app_subst_item
        A.rkey_of A.rkey A.provision_id] in *;
      try contradiction; try exact I; now subst th.
Qed.
