(* Loading source files directly into typed syntax. The driver supplies
   the file reads, and the public build supplies a finite snapshot. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit SourceSnapshot SnapshotLoaderBound.
From NM Require Sx Parse FrontEnd FrontEndComplete.
Import Audit.Semantics ListNotations.
Module I := Audit.Input.
Module L := Audit.Loader.
Local Open Scope string_scope.
Local Open Scope list_scope.

Fixpoint walk (fuel : nat) (read : string -> option string) (root prelude : string)
    (todo seen : list string) (acc : list I.amodule) : Sx.res (list I.amodule) :=
  match fuel with
  | 0 => Sx.Bad "bundle: too many modules"
  | S fuel =>
      match todo with
      | [] => Sx.Good (rev acc)
      | path :: todo =>
          if existsb (String.eqb path) seen then walk fuel read root prelude todo seen acc
          else match read path with
            | None => Sx.Bad ("bundle: " ++ path ++ ": OSError: cannot read")
            | Some text => match Parse.parse_moss text with
                | Sx.Bad message => Sx.Bad ("bundle: " ++ path ++ ": " ++ message)
                | Sx.Good ast =>
                    let wants := L.wants_prelude prelude path in
                    let targets := map (fun i => L.resolve root prelude path (I.i_path i)) (I.f_imports ast) in
                    walk fuel read root prelude
                      (if wants then prelude :: (targets ++ todo) else targets ++ todo)
                      (path :: seen) (I.AModule path wants targets ast :: acc)
                end
            end
      end
  end.

Section Sound.
Variable read : string -> option string.
Variables root prelude entry : string.

Definition module_ok (m : I.amodule) : Prop :=
  exists text, read (I.am_path m) = Some text /\
    Parse.parse_moss text = Sx.Good (I.am_file m) /\
    I.am_prelude m = L.wants_prelude prelude (I.am_path m) /\
    I.am_targets m = map (fun i => L.resolve root prelude (I.am_path m) (I.i_path i))
      (I.f_imports (I.am_file m)).

Definition needs (m : I.amodule) := I.am_targets m ++ (if I.am_prelude m then [prelude] else []).
Definition invariant (todo seen : list string) (acc : list I.amodule) : Prop :=
  seen = map I.am_path acc /\ NoDup seen /\ (forall m, In m acc -> module_ok m) /\
  (forall m path, In m acc -> In path (needs m) -> In path todo \/ In path seen) /\
  (In entry todo \/ In entry seen).

Lemma walk_sound : forall fuel todo seen acc modules,
  walk fuel read root prelude todo seen acc = Sx.Good modules ->
  invariant todo seen acc ->
  NoDup (map I.am_path modules) /\ In entry (map I.am_path modules) /\
  (forall m, In m modules -> module_ok m) /\
  (forall m path, In m modules -> In path (needs m) -> In path (map I.am_path modules)).
Proof.
  induction fuel as [|fuel IH]; intros todo seen acc modules Run (Names & Unique & OK & Closed & Entry);
    cbn [walk] in Run; [discriminate|].
  destruct todo as [|path todo].
  - injection Run as <-. rewrite map_rev. subst seen. repeat split.
    + apply NoDup_rev; exact Unique.
    + destruct Entry as [[]|Entry]. rewrite <- in_rev; exact Entry.
    + intros m Member. apply OK, in_rev; exact Member.
    + intros m target Member Needed. apply in_rev in Member.
      destruct (Closed m target Member Needed) as [[]|Present]. rewrite <- in_rev; exact Present.
  - destruct (existsb (String.eqb path) seen) eqn:Seen.
    + apply (IH _ _ _ _ Run). repeat split; auto.
      * intros m target Member Needed. destruct (Closed m target Member Needed) as [[<-|Present]|Present]; auto.
        right. apply existsb_exists in Seen as [present [MemberSeen Equal]].
        apply String.eqb_eq in Equal; subst; exact MemberSeen.
      * destruct Entry as [[<-|Present]|Present]; auto.
        right. apply existsb_exists in Seen as [present [MemberSeen Equal]].
        apply String.eqb_eq in Equal; subst; exact MemberSeen.
    + destruct (read path) as [text|] eqn:Read; [|discriminate].
      destruct (Parse.parse_moss text) as [ast|message] eqn:Parsed; [|discriminate].
      apply (IH _ _ _ _ Run). subst seen.
      assert (Absent : ~ In path (map I.am_path acc)).
      { intro Present. assert (existsb (String.eqb path) (map I.am_path acc) = true).
        { apply existsb_exists. exists path; split; [exact Present|apply String.eqb_refl]. }
        congruence. }
      repeat split.
      * constructor; assumption.
      * intros m [<-|Member]; [exists text; repeat split; assumption|apply OK; exact Member].
      * intros m target [<-|Member] Needed.
        -- left. unfold needs in Needed; cbn in Needed.
           destruct (L.wants_prelude prelude path); apply in_app_or in Needed;
             destruct Needed as [Needed|Needed]; cbn in *; intuition (auto using in_or_app).
        -- destruct (Closed m target Member Needed) as [[<-|Present]|Present];
             [right; left; reflexivity|left|right; right; exact Present].
           destruct (L.wants_prelude prelude path); [right|]; apply in_or_app; right; exact Present.
      * destruct Entry as [[<-|Present]|Present]; [right;left;reflexivity|left|right;right;exact Present].
        destruct (L.wants_prelude prelude path); [right|]; apply in_or_app; right; exact Present.
Qed.
End Sound.

Section Bounds.
Variable snapshot : source.
Variables root prelude : string.
Local Strategy opaque [source_read Parse.parse_moss].

Theorem walk_stable : forall fuel todo seen acc extra,
  work_potential snapshot todo seen < fuel ->
  walk (fuel + extra) (source_read snapshot) root prelude todo seen acc =
  walk fuel (source_read snapshot) root prelude todo seen acc.
Proof.
  induction fuel as [|fuel IH]; intros todo seen acc extra Enough; [lia|].
  cbn [walk Nat.add]. destruct todo as [|path todo]; [reflexivity|].
  destruct (existsb (String.eqb path) seen) eqn:Seen.
  - apply IH. unfold work_potential in *; cbn [List.length] in *; lia.
  - destruct (source_read snapshot path) as [text|] eqn:Read; [|reflexivity].
    destruct (Parse.parse_moss text) as [ast|message] eqn:Parsed; [|reflexivity].
    apply IH.
    pose proof (fresh_step_decreases snapshot root prelude path ast text todo seen Seen Read Parsed).
    unfold next_todo in H. lia.
Qed.
End Bounds.

Definition load_paths (fuel : nat) (read : string -> option string)
    (root prelude entry : string) : Sx.res I.bundle :=
  Sx.rbind (walk fuel read root prelude [entry] [] [])
    (fun modules => Sx.Good (I.Bundle entry prelude modules)).

Definition load_from_at (fuel : nat) (read : string -> option string)
    (root prelude entry : string) : Sx.res I.bundle :=
  load_paths fuel read root (L.normalize root None prelude) (L.normalize root None entry).

Definition load_from := load_from_at 100000.

Definition load_at (fuel : nat) (snapshot : source) (prelude entry : file_path) : Sx.res I.bundle :=
  load_paths fuel (source_read snapshot) "/" (path_text prelude) (path_text entry).

Definition load (snapshot : source) (prelude entry : file_path) : Sx.res I.bundle :=
  load_at (snapshot_fuel snapshot) snapshot prelude entry.

Lemma path_normalized : forall path,
  L.normalize "/" None (path_text path) = path_text path.
Proof.
  intro path. pose proof (path_canonical path) as Valid.
  apply andb_true_iff in Valid as [_ Equal]. apply String.eqb_eq in Equal. exact Equal.
Qed.

Theorem load_from_at_sound : forall fuel read root prelude entry bundle,
  load_from_at fuel read root prelude entry = Sx.Good bundle ->
  L.Bundled read root prelude entry bundle.
Proof.
  intros fuel read root prelude entry bundle Run.
  unfold load_from_at, load_paths in Run.
  set (p := L.normalize root None prelude) in *.
  set (e := L.normalize root None entry) in *.
  destruct (walk fuel read root p [e] [] []) as [modules|message] eqn:Walk;
    cbn in Run; [|discriminate].
  injection Run as <-.
  destruct (walk_sound read root p e _ _ _ _ _ Walk) as [Unique [Entry [OK Closed]]].
  { split; [reflexivity|]. split; [constructor|].
    split; [intros m []|]. split; [intros m target []|].
    left; left; reflexivity. }
  unfold L.Bundled; cbn. fold p e. repeat split; auto.
  intros m Member. destruct (OK m Member) as [text [Read [Parsed [Wants Targets]]]].
  exists text, (I.am_file m). repeat split; auto.
  - apply FrontEnd.parse_moss_sound; exact Parsed.
  - intros target Present. apply (Closed m target Member). unfold needs. apply in_or_app; left; exact Present.
  - intro Wanted. apply (Closed m p Member). unfold needs.
    rewrite Wanted. apply in_or_app; right; cbn; auto.
Qed.

Theorem load_from_sound : forall read root prelude entry bundle,
  load_from read root prelude entry = Sx.Good bundle ->
  L.Bundled read root prelude entry bundle.
Proof. exact (load_from_at_sound 100000). Qed.

Local Lemma resolve_absolute_root_independent : forall root other prelude importer path,
  L.is_abs prelude = true -> L.is_abs importer = true ->
  L.resolve root prelude importer path = L.resolve other prelude importer path.
Proof.
  intros root other prelude importer path Prelude Importer.
  unfold L.resolve. destruct (L.is_abs path || String.eqb (L.first_segment path) "." ||
    String.eqb (L.first_segment path) "..")%bool;
    unfold L.normalize; destruct (L.is_abs path); rewrite ?Prelude, ?Importer; reflexivity.
Qed.

Local Lemma snapshot_rows_from_modules : forall read modules,
  (forall m, In m modules ->
    exists text, read (I.am_path m) = Some text /\ canonical_path (I.am_path m) = true) ->
  exists files : list (file_path * string),
    map (fun file => path_text (fst file)) files = map I.am_path modules /\
    forall m text, In m modules -> read (I.am_path m) = Some text ->
      exists path, In (path, text) files /\ path_text path = I.am_path m.
Proof.
  intros read modules. induction modules as [|m modules IH]; intro Read.
  - exists []. split; [reflexivity|intros n text []].
  - destruct (Read m (or_introl eq_refl)) as [text [Head Canonical]].
    destruct (IH ltac:(intros n Member; apply Read; now right)) as [files [Paths Included]].
    exists ((AbsolutePath (I.am_path m) Canonical, text) :: files).
    split.
    + cbn. now rewrite Paths.
    + intros n contents [<-|Member] Found.
      * rewrite Head in Found. injection Found as <-.
        exists (AbsolutePath (I.am_path m) Canonical). split; [now left|reflexivity].
      * destruct (Included n contents Member Found) as [path [Row Path]].
        exists path. split; [now right|exact Path].
Qed.

Theorem load_from_snapshot_witness : forall read root prelude_in entry_in b prelude entry,
  load_from read root prelude_in entry_in = Sx.Good b ->
  L.normalize root None prelude_in = path_text prelude ->
  L.normalize root None entry_in = path_text entry ->
  (forall m, In m (I.b_mods b) -> canonical_path (I.am_path m) = true) ->
  (forall m text, In m (I.b_mods b) -> read (I.am_path m) = Some text ->
    Syntax.string_utf8_valid text = true) ->
  exists snapshot, L.Bundled (source_read snapshot) "/" (path_text prelude) (path_text entry) b.
Proof.
  intros read root prelude_in entry_in b prelude entry Loaded Prelude Entry Canonical Utf8.
  destruct (load_from_sound _ _ _ _ _ Loaded)
    as (EntryFound & PreludeFound & Distinct & Main & Modules).
  rewrite Prelude in PreludeFound. rewrite Entry in EntryFound.
  destruct (snapshot_rows_from_modules read (I.b_mods b)) as (files & Paths & Included).
  { intros m Member.
    destruct (Modules m Member) as (text & ast & Read & _).
    exists text. split; [exact Read|now apply Canonical]. }
  assert (Unique : NoDup (map (fun file => path_text (fst file)) files)) by now rewrite Paths.
  exists (Source files Unique).
  unfold L.Bundled. rewrite !path_normalized.
  repeat split; try assumption.
  intros m Member.
  destruct (Modules m Member) as (text & ast & Read & Derived & File & Wants & Targets & Closed & Implicit).
  exists text, ast.
  assert (SnapshotRead : source_read (Source files Unique) (I.am_path m) = Some text).
  { apply source_read_iff.
    destruct (Included m text Member Read) as [path [Row Path]].
    exists path. repeat split; try assumption. exact (Utf8 m text Member Read). }
  repeat split; try assumption.
  rewrite Targets. apply map_ext. intro import.
  apply resolve_absolute_root_independent.
  - rewrite PreludeFound. pose proof (path_canonical prelude) as Absolute.
    unfold canonical_path in Absolute. now apply andb_true_iff in Absolute as [Absolute _].
  - specialize (Canonical m Member). unfold canonical_path in Canonical.
    now apply andb_true_iff in Canonical as [Absolute _].
Qed.

Theorem load_at_sound : forall fuel snapshot prelude entry bundle,
  load_at fuel snapshot prelude entry = Sx.Good bundle ->
  L.Bundled (source_read snapshot) "/" (path_text prelude) (path_text entry) bundle.
Proof.
  intros fuel snapshot prelude entry bundle Run. apply load_from_at_sound with (fuel := fuel).
  unfold load_from_at. rewrite !path_normalized. exact Run.
Qed.

Theorem load_sound : forall snapshot prelude entry bundle,
  load snapshot prelude entry = Sx.Good bundle ->
  L.Bundled (source_read snapshot) "/" (path_text prelude) (path_text entry) bundle.
Proof. intros; eapply load_at_sound; eassumption. Qed.

Lemma next_todo_app : forall root prelude path ast todo,
  next_todo root prelude path ast todo = next_todo root prelude path ast [] ++ todo.
Proof.
  intros. unfold next_todo. destruct (L.wants_prelude prelude path);
    cbn [List.app]; rewrite app_nil_r; reflexivity.
Qed.

Section Acceptance.
Variable snapshot : source.
Variables root prelude : string.
Variable paths : list string.
Hypothesis Readable : forall path, In path paths ->
  exists text ast, source_read snapshot path = Some text /\
    Parse.parse_moss text = Sx.Good ast /\
    incl (next_todo root prelude path ast []) paths.
Local Strategy opaque [source_read Parse.parse_moss].

Lemma walk_accepts_closed_witness : forall fuel todo seen acc,
  work_potential snapshot todo seen < fuel -> incl todo paths ->
  exists modules, walk fuel (source_read snapshot) root prelude todo seen acc = Sx.Good modules.
Proof.
  induction fuel as [|fuel IH]; intros todo seen acc Enough Covered; [lia|].
  cbn [walk]. destruct todo as [|path todo]; [eexists;reflexivity|].
  destruct (existsb (String.eqb path) seen) eqn:Seen.
  - apply IH.
    + unfold work_potential in *; cbn [List.length] in *; lia.
    + intros target Member. apply Covered; right; exact Member.
  - destruct (Readable path (Covered path (or_introl eq_refl)))
      as [text [ast [Read [Parsed Next]]]].
    rewrite Read, Parsed. apply IH.
    + pose proof (fresh_step_decreases snapshot root prelude path ast text todo seen Seen Read Parsed).
      unfold next_todo in H. lia.
    + change (incl (next_todo root prelude path ast todo) paths).
      rewrite next_todo_app. intros target Member. apply in_app_or in Member as [Member|Member].
      * apply Next; exact Member.
      * apply Covered; right; exact Member.
Qed.
End Acceptance.

(* Completeness is existential in the output bundle. A declarative witness
   may contain irrelevant additional modules; traversal loads only modules
   reachable from the entry and its imports/prelude. *)
Theorem load_complete : forall snapshot prelude entry witness,
  L.Bundled (source_read snapshot) "/" (path_text prelude) (path_text entry) witness ->
  exists bundle, load snapshot prelude entry = Sx.Good bundle.
Proof.
  intros snapshot prelude entry witness [Entry [Prelude [Unique [Present Modules]]]].
  rewrite path_normalized in Entry, Prelude.
  assert (Readable : forall path, In path (map I.am_path (I.b_mods witness)) ->
    exists text ast, source_read snapshot path = Some text /\
      Parse.parse_moss text = Sx.Good ast /\
      incl (next_todo "/" (path_text prelude) path ast []) (map I.am_path (I.b_mods witness))).
  { intros path Member. apply in_map_iff in Member as [m [Path Member]].
    destruct (Modules m Member) as [text [ast [Read [Derived [File [Wants [Targets [Imports Needed]]]]]]]].
    rewrite Prelude in Wants, Targets, Needed.
    exists text, ast. split; [rewrite <- Path; exact Read|].
    split; [apply FrontEndComplete.parse_moss_complete; exact Derived|].
    unfold next_todo. rewrite <- Path, <- Wants, <- Targets.
    intros target Target. destruct (I.am_prelude m) eqn:Wanted; cbn in Target.
    - destruct Target as [<-|Target]; [apply Needed; reflexivity|].
      rewrite app_nil_r in Target. apply Imports; exact Target.
    - rewrite app_nil_r in Target. apply Imports; exact Target. }
  destruct (walk_accepts_closed_witness snapshot "/" (path_text prelude)
    (map I.am_path (I.b_mods witness)) Readable (snapshot_fuel snapshot)
    [path_text entry] [] []) as [modules Run].
  - unfold snapshot_fuel, work_potential, snapshot_paths.
    rewrite unseen_count_nil, map_length. cbn [List.length]. lia.
  - intros path [<-|[]]. rewrite <- Entry; exact Present.
  - exists (I.Bundle (path_text entry) (path_text prelude) modules).
    unfold load, load_at, load_paths. rewrite Run; reflexivity.
Qed.
