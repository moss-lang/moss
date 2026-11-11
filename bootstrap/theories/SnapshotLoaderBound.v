(* An input-derived stabilization bound for the existing loader walk.
   It counts repeated worklist pops as well as first visits. Parse failures
   in unused files only affect the numeric bound; they are not rejected.
   This is not yet completeness of the typed bundle/dump adapter. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit SourceSnapshot.
From NM Require Sx Parse SourceNames.
Import ListNotations.
Module I := Audit.Input.
Module L := Audit.Loader.

Fixpoint unseen_count (paths seen : list string) : nat :=
  match paths with
  | [] => 0
  | path :: rest =>
      if existsb (String.eqb path) seen then unseen_count rest seen
      else S (unseen_count rest seen)
  end.

Lemma unseen_count_app : forall before after seen,
  unseen_count (before ++ after) seen = unseen_count before seen + unseen_count after seen.
Proof.
  induction before as [|path rest IH]; intros after seen; cbn [unseen_count List.app]; [reflexivity|].
  destruct (existsb (String.eqb path) seen); rewrite IH; reflexivity.
Qed.

Lemma unseen_count_add_seen : forall paths path seen,
  unseen_count paths (path :: seen) <= unseen_count paths seen.
Proof.
  induction paths as [|head rest IH]; intros path seen; cbn [unseen_count existsb]; [lia|].
  specialize (IH path seen).
  destruct (String.eqb head path); destruct (existsb (String.eqb head) seen); cbn; lia.
Qed.

Lemma unseen_count_fresh : forall paths path seen,
  In path paths -> existsb (String.eqb path) seen = false ->
  S (unseen_count paths (path :: seen)) <= unseen_count paths seen.
Proof.
  intros paths path seen Member Fresh.
  apply in_split in Member as [before [after ->]].
  rewrite !unseen_count_app. cbn [unseen_count existsb].
  rewrite String.eqb_refl, Fresh. cbn.
  pose proof (unseen_count_add_seen before path seen).
  pose proof (unseen_count_add_seen after path seen). lia.
Qed.

Lemma unseen_count_nil : forall paths, unseen_count paths [] = List.length paths.
Proof. induction paths; cbn [unseen_count existsb List.length]; congruence. Qed.

Definition snapshot_paths (snapshot : source) : list string :=
  map (fun file => path_text (fst file)) (source_files snapshot).

(* One extra edge conservatively accounts for a possible implicit prelude. *)
Definition file_degree (file : file_path * string) : nat :=
  match Parse.parse_moss (snd file) with
  | Sx.Good ast => S (List.length (I.f_imports ast))
  | Sx.Bad _ => 0
  end.

Definition snapshot_degree (snapshot : source) : nat :=
  fold_right Nat.max 0 (map file_degree (source_files snapshot)).

Lemma fold_max_member : forall values value,
  In value values -> value <= fold_right Nat.max 0 values.
Proof.
  induction values as [|head rest IH]; intros value Member; cbn in *; [contradiction|].
  destruct Member as [<-|Member]; [apply Nat.le_max_l|].
  eapply Nat.le_trans; [apply IH; exact Member|apply Nat.le_max_r].
Qed.

Lemma snapshot_read_path : forall snapshot path text,
  source_read snapshot path = Some text -> In path (snapshot_paths snapshot).
Proof.
  intros snapshot path text Read.
  apply source_read_iff in Read as [canonical [Member [<- _]]].
  apply in_map_iff. exists (canonical,text); auto.
Qed.

Lemma snapshot_read_degree : forall snapshot path text ast,
  source_read snapshot path = Some text -> Parse.parse_moss text = Sx.Good ast ->
  S (List.length (I.f_imports ast)) <= snapshot_degree snapshot.
Proof.
  intros snapshot path text ast Read Parsed.
  apply source_read_iff in Read as [canonical [Member _]].
  unfold snapshot_degree. apply fold_max_member, in_map_iff.
  exists (canonical,text). split; [unfold file_degree; cbn; rewrite Parsed; reflexivity|exact Member].
Qed.

Definition next_todo (root prelude path : string) (ast : I.afile) (todo : list string) :=
  let imports := map (fun i => L.resolve root prelude path (I.i_path i)) (I.f_imports ast) in
  if L.wants_prelude prelude path then prelude :: (imports ++ todo) else imports ++ todo.

Lemma next_todo_length : forall root prelude path ast todo,
  List.length (next_todo root prelude path ast todo) <=
  List.length todo + S (List.length (I.f_imports ast)).
Proof.
  intros. unfold next_todo. destruct (L.wants_prelude prelude path);
    cbn [List.length]; rewrite app_length, map_length; lia.
Qed.

Definition work_potential (snapshot : source) (todo seen : list string) : nat :=
  List.length todo + unseen_count (snapshot_paths snapshot) seen * S (snapshot_degree snapshot).

Lemma fresh_step_decreases : forall snapshot root prelude path ast text todo seen,
  existsb (String.eqb path) seen = false ->
  source_read snapshot path = Some text -> Parse.parse_moss text = Sx.Good ast ->
  work_potential snapshot (next_todo root prelude path ast todo) (path :: seen) <
  work_potential snapshot (path :: todo) seen.
Proof.
  intros snapshot root prelude path ast text todo seen Fresh Read Parsed.
  pose proof (snapshot_read_path snapshot path text Read) as Member.
  pose proof (snapshot_read_degree snapshot path text ast Read Parsed) as Degree.
  pose proof (unseen_count_fresh (snapshot_paths snapshot) path seen Member Fresh) as Fewer.
  pose proof (next_todo_length root prelude path ast todo) as Length.
  unfold work_potential. cbn [List.length]. nia.
Qed.

Section Stabilization.
Variable snapshot : source.
Variables root prelude : string.
Local Strategy opaque [source_read Parse.parse_moss SourceNames.wf_names].

Theorem walk_stable_above_potential : forall fuel todo seen acc extra,
  work_potential snapshot todo seen < fuel ->
  Parse.walk (fuel + extra) (source_read snapshot) root prelude todo seen acc =
  Parse.walk fuel (source_read snapshot) root prelude todo seen acc.
Proof.
  induction fuel as [|fuel IH]; intros todo seen acc extra Enough; [lia|].
  cbn [Parse.walk Nat.add]. destruct todo as [|path todo]; [reflexivity|].
  destruct (existsb (String.eqb path) seen) eqn:Seen.
  - apply IH. unfold work_potential in *; cbn [List.length] in *; lia.
  - destruct (source_read snapshot path) as [text|] eqn:Read; [|reflexivity].
    destruct (Parse.parse_moss text) as [ast|message] eqn:Parsed; [|reflexivity].
    destruct (SourceNames.wf_names ast); [|reflexivity].
    change (Parse.walk (fuel + extra) (source_read snapshot) root prelude
      (next_todo root prelude path ast todo) (path :: seen)
      (Parse.LMod path (L.wants_prelude prelude path)
        (map (fun i => L.resolve root prelude path (I.i_path i)) (I.f_imports ast))
        (Sx.Dump.dump_file ast) :: acc) =
      Parse.walk fuel (source_read snapshot) root prelude
      (next_todo root prelude path ast todo) (path :: seen)
      (Parse.LMod path (L.wants_prelude prelude path)
        (map (fun i => L.resolve root prelude path (I.i_path i)) (I.f_imports ast))
        (Sx.Dump.dump_file ast) :: acc)).
    apply IH.
    pose proof (fresh_step_decreases snapshot root prelude path ast text todo seen Seen Read Parsed).
    lia.
Qed.
End Stabilization.

Definition snapshot_fuel (snapshot : source) : nat :=
  S (1 + List.length (source_files snapshot) * S (snapshot_degree snapshot)).

Corollary snapshot_fuel_stable : forall snapshot root prelude entry extra,
  Parse.walk (snapshot_fuel snapshot + extra) (source_read snapshot) root prelude [entry] [] [] =
  Parse.walk (snapshot_fuel snapshot) (source_read snapshot) root prelude [entry] [] [].
Proof.
  intros. apply walk_stable_above_potential.
  unfold snapshot_fuel, work_potential, snapshot_paths.
  rewrite unseen_count_nil, map_length. cbn [List.length]. lia.
Qed.

Lemma walk_success_more : forall fuel read root prelude todo seen acc modules,
  Parse.walk fuel read root prelude todo seen acc = Sx.Good modules ->
  forall extra, Parse.walk (fuel + extra) read root prelude todo seen acc = Sx.Good modules.
Proof.
  induction fuel as [|fuel IH]; intros read root prelude todo seen acc modules Run extra;
    [discriminate|].
  cbn [Parse.walk Nat.add] in *. destruct todo as [|path todo]; [exact Run|].
  destruct (existsb (String.eqb path) seen).
  - eapply IH; exact Run.
  - destruct (read path) as [text|]; [|discriminate].
    destruct (Parse.parse_moss text) as [ast|message]; [|discriminate].
    destruct (SourceNames.wf_names ast); [|discriminate].
    eapply IH; exact Run.
Qed.

Theorem snapshot_success_at_bound : forall snapshot root prelude entry fuel modules,
  Parse.walk fuel (source_read snapshot) root prelude [entry] [] [] = Sx.Good modules ->
  Parse.walk (snapshot_fuel snapshot) (source_read snapshot) root prelude [entry] [] [] = Sx.Good modules.
Proof.
  intros snapshot root prelude entry fuel modules Run.
  pose proof (walk_success_more fuel (source_read snapshot) root prelude [entry] [] [] modules Run
    (snapshot_fuel snapshot)) as More.
  rewrite Nat.add_comm, snapshot_fuel_stable in More. exact More.
Qed.

(* Reuse the existing typed-bundle adapter unchanged, but choose its walk
   budget from the snapshot instead of an arbitrary module-count cap. *)
Definition load_snapshot_at (fuel : nat) (snapshot : source) (prelude entry : file_path)
    : Sx.res I.bundle :=
  Sx.rbind (Parse.walk fuel (source_read snapshot) "/" (path_text prelude)
    [path_text entry] [] [])
    (fun modules => Sx.read_bundle (Parse.bundle_sx (path_text entry) (path_text prelude) modules)).

Definition load_snapshot (snapshot : source) (prelude entry : file_path) : Sx.res I.bundle :=
  load_snapshot_at (snapshot_fuel snapshot) snapshot prelude entry.

Theorem load_snapshot_success_adequate : forall snapshot prelude entry fuel bundle,
  load_snapshot_at fuel snapshot prelude entry = Sx.Good bundle ->
  load_snapshot snapshot prelude entry = Sx.Good bundle.
Proof.
  intros snapshot prelude entry fuel bundle Run.
  unfold load_snapshot_at in Run. unfold load_snapshot, load_snapshot_at.
  destruct (Parse.walk fuel (source_read snapshot) "/" (path_text prelude)
    [path_text entry] [] []) as [modules|message] eqn:Walk; cbn in Run; [|discriminate].
  rewrite (snapshot_success_at_bound snapshot "/" (path_text prelude) (path_text entry) fuel modules Walk).
  exact Run.
Qed.

Theorem load_snapshot_stable : forall snapshot prelude entry extra,
  load_snapshot_at (snapshot_fuel snapshot + extra) snapshot prelude entry =
  load_snapshot snapshot prelude entry.
Proof.
  intros. unfold load_snapshot, load_snapshot_at. rewrite snapshot_fuel_stable. reflexivity.
Qed.
