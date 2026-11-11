(* Exact lookup and finite support for the public build input. These lemmas
   concern snapshots only; they do not assume or prove loader completeness. *)
From Stdlib Require Import List ListDec String Bool.
From NM Require Import Audit.
Import ListNotations.

(* The host supplies canonical paths through path_of_string. This checked
   constructor validates the finite map rather than requiring the caller
   to manufacture its uniqueness proof. Unused invalid UTF-8 contents are
   retained; source_read applies the same per-read validation as before. *)
Definition source_from_files (files : list (file_path * string)) : option source :=
  match NoDup_dec String.string_dec (map (fun file => path_text (fst file)) files) with
  | left Unique => Some (Source files Unique)
  | right _ => None
  end.

Theorem source_from_files_sound : forall files snapshot,
  source_from_files files = Some snapshot -> source_files snapshot = files.
Proof.
  intros files snapshot Constructed. unfold source_from_files in Constructed.
  destruct (NoDup_dec _ _) as [Unique|Duplicate]; [|discriminate].
  injection Constructed as <-. reflexivity.
Qed.

Theorem source_from_files_complete : forall files,
  NoDup (map (fun file => path_text (fst file)) files) ->
  exists snapshot, source_from_files files = Some snapshot /\ source_files snapshot = files.
Proof.
  intros files Unique. unfold source_from_files.
  destruct (NoDup_dec _ _) as [Distinct|Duplicate]; [|contradiction].
  eexists; split; reflexivity.
Qed.

Lemma source_lookup_sound : forall snapshot name text,
  source_lookup snapshot name = Some text ->
  exists path, In (path, text) (source_files snapshot) /\ path_text path = name.
Proof.
  intros snapshot name text Read. unfold source_lookup in Read.
  destruct (find _ _) as [[path contents]|] eqn:Found; cbn in Read; [|discriminate].
  injection Read as <-. apply find_some in Found as [Member Equal].
  apply String.eqb_eq in Equal. exists path; split; [exact Member|symmetry; exact Equal].
Qed.

Lemma source_lookup_complete : forall snapshot path text,
  In (path, text) (source_files snapshot) ->
  source_lookup snapshot (path_text path) = Some text.
Proof.
  intros [files Unique] path text Member. unfold source_lookup; cbn in *.
  induction files as [|[head contents] rest IH]; cbn in *; [contradiction|].
  inversion Unique as [|? ? Absent Distinct]; subst.
  destruct Member as [Equal|Member].
  - injection Equal as <- <-. rewrite String.eqb_refl. reflexivity.
  - destruct (String.eqb (path_text path) (path_text head)) eqn:Equal.
    + apply String.eqb_eq in Equal. exfalso. apply Absent.
      rewrite <- Equal. apply in_map_iff. exists (path, text); auto.
    + apply IH; assumption.
Qed.

Theorem source_read_iff : forall snapshot name text,
  source_read snapshot name = Some text <->
  exists path, In (path, text) (source_files snapshot) /\
    path_text path = name /\ Syntax.string_utf8_valid text = true.
Proof.
  intros snapshot name text. split.
  - unfold source_read. destruct (source_lookup snapshot name) as [contents|] eqn:Read;
      [|discriminate].
    destruct (Syntax.string_utf8_valid contents) eqn:Valid; [|discriminate].
    intro Equal; injection Equal as <-.
    destruct (source_lookup_sound _ _ _ Read) as [path [Member Name]].
    exists path; auto.
  - intros [path [Member [<- Valid]]]. unfold source_read.
    rewrite (source_lookup_complete _ _ _ Member), Valid. reflexivity.
Qed.

Corollary source_read_finite : forall snapshot,
  exists paths : list string, NoDup paths /\
    forall name text, source_read snapshot name = Some text -> In name paths.
Proof.
  intro snapshot. exists (map (fun file => path_text (fst file)) (source_files snapshot)).
  split; [exact (source_unique snapshot)|].
  intros name text Read. apply source_read_iff in Read as [path [Member [<- _]]].
  apply in_map_iff. exists (path, text); auto.
Qed.

Corollary source_read_canonical : forall snapshot name text,
  source_read snapshot name = Some text -> canonical_path name = true.
Proof.
  intros snapshot name text Read. apply source_read_iff in Read as [path [_ [<- _]]].
  exact (path_canonical path).
Qed.
