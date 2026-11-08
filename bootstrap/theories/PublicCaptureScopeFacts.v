(* Live type-capture scope facts. The supplied plan controls the captured
   rows; no source-level theorem about how that plan is formed is claimed. *)
From Stdlib Require Import List PeanoNat.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.

Lemma lookup_subst_grounded_rows : forall caller rows symbol,
  lookup_subst symbol (map (fun q => (fst q, ground caller (snd q))) rows) =
    option_map (ground caller) (lookup_subst symbol rows).
Proof.
  intros caller rows. induction rows as [|[key meaning] rest IH]; intro symbol; cbn.
  - reflexivity.
  - destruct (Nat.eqb symbol key); [reflexivity|apply IH].
Qed.

(* The equality also covers None: an absent planned generation is absent
   from the capture even if the caller or selected provisions contain it. *)
Theorem typed_capture_generation_lookup : forall caller plan provisions symbol,
  lookup_subst symbol (type_meanings (typed_capture caller plan provisions)) =
    option_map (ground caller) (lookup_subst symbol (capture_generations plan)).
Proof. intros; apply lookup_subst_grounded_rows. Qed.

Theorem typed_capture_alias_lookup : forall caller plan provisions source,
  lookup_subst source (type_aliases (typed_capture caller plan provisions)) =
    option_map (ground caller) (lookup_subst source (capture_aliases plan)).
Proof. intros; apply lookup_subst_grounded_rows. Qed.

Theorem typed_capture_ground_row : forall caller plan provisions symbol meaning,
  lookup_subst symbol (capture_generations plan) = Some meaning ->
  ground (typed_capture caller plan provisions) (TAbs symbol) = ground caller meaning.
Proof.
  intros caller plan provisions symbol meaning Planned.
  change (match lookup_subst symbol (type_meanings (typed_capture caller plan provisions)) with
    | Some actual => actual | None => TAbs symbol end = ground caller meaning).
  rewrite typed_capture_generation_lookup, Planned; reflexivity.
Qed.
