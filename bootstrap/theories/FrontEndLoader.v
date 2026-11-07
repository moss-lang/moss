(* ======================================================================== *)
(* FrontEndLoader.v — the loader (Parse.load_bundle, read back as           *)
(* Parse.load) against Audit.Loader.Bundled: Part VI's LOADER.              *)
(* ======================================================================== *)

From Stdlib Require Import String Ascii List Bool Arith Lia.
From NM Require Import Audit Sx Parse FrontEnd DumpRoundTrip.
Import ListNotations.
Import Audit.Input Audit.Syntax Sx.Dump Audit.Loader.
Local Open Scope string_scope.

Section Walk.
Variables (read : string -> option string) (root prelude entry : string).

(* What the walk establishes of each module it loads. *)
Definition ModOK (m : lmod) : Prop :=
  exists text f, read (lm_path m) = Some text /\ parse_moss text = Good f /\
    lm_wants m = wants_prelude prelude (lm_path m) /\
    lm_targets m = map (fun i => resolve root prelude (lm_path m) (i_path i)) (f_imports f) /\
    lm_dump m = dump_file f /\ wf_names f = true.

Definition needs (m : lmod) : list string := lm_targets m ++ (if lm_wants m then [prelude] else []).

Definition Inv (todo seen : list string) (acc : list lmod) : Prop :=
  seen = map lm_path acc /\ NoDup seen /\ (forall m, In m acc -> ModOK m) /\
  (forall m t, In m acc -> In t (needs m) -> In t todo \/ In t seen) /\
  (In entry todo \/ In entry seen).

Lemma walk_spec : forall fuel todo seen acc ms,
  walk fuel read root prelude todo seen acc = Good ms -> Inv todo seen acc ->
  NoDup (map lm_path ms) /\ In entry (map lm_path ms) /\ (forall m, In m ms -> ModOK m) /\
  (forall m t, In m ms -> In t (needs m) -> In t (map lm_path ms)).
Proof.
  induction fuel as [|fuel IH]; intros todo seen acc ms H (Hs & Hnd & Hok & Hcl & Hen); simpl in H;
    [discriminate|].
  destruct todo as [|p todo].
  - injection H as <-. unfold rev'. rewrite <- rev_alt. rewrite map_rev. subst seen.
    repeat split.
    + apply NoDup_rev. auto.
    + destruct Hen as [[]|Hen]. rewrite <- in_rev. auto.
    + intros m Hm. apply Hok. apply in_rev. auto.
    + intros m t Hm Ht. apply in_rev in Hm. destruct (Hcl m t Hm Ht) as [[]|]. rewrite <- in_rev. auto.
  - destruct (existsb (String.eqb p) seen) eqn:Ex.
    + apply (IH _ _ _ _ H). repeat split; auto.
      * intros m t Hm Ht. destruct (Hcl m t Hm Ht) as [[<-|]|]; auto.
        right. apply existsb_exists in Ex as (q & Hq & Eq). apply String.eqb_eq in Eq. subst. auto.
      * destruct Hen as [[<-|]|]; auto.
        right. apply existsb_exists in Ex as (q & Hq & Eq). apply String.eqb_eq in Eq. subst. auto.
    + destruct (read p) as [text|] eqn:Er; [|discriminate].
      destruct (parse_moss text) as [f|msg] eqn:Ep; [|discriminate].
      destruct (wf_names f) eqn:En; [|discriminate].
      apply (IH _ _ _ _ H). subst seen.
      assert (Hnin : ~ In p (map lm_path acc)).
      { intro Hin. assert (existsb (String.eqb p) (map lm_path acc) = true).
        { apply existsb_exists. exists p. split; auto. apply String.eqb_refl. } congruence. }
      repeat split.
      * constructor; auto.
      * intros m [<-|Hm]; auto. exists text, f. repeat split; auto.
      * intros m t [<-|Hm] Ht.
        -- left. unfold needs in Ht. simpl in Ht.
           destruct (wants_prelude prelude p); apply in_app_or in Ht; destruct Ht as [Ht|Ht];
             simpl in *; intuition (auto using in_or_app).
        -- destruct (Hcl m t Hm Ht) as [[<-|Ht']|Ht']; [right; left; auto | left | right; right; auto].
           destruct (wants_prelude prelude p); [right|]; apply in_or_app; right; auto.
      * destruct Hen as [[<-|Hen]|Hen]; [right; left; auto | left | right; right; auto].
        destruct (wants_prelude prelude p); [right|]; apply in_or_app; right; auto.
Qed.
End Walk.

Definition ModRead (m : lmod) (am : amodule) : Prop :=
  am_path am = lm_path m /\ am_prelude am = lm_wants m /\ am_targets am = lm_targets m /\
  read_file (lm_dump m) = Good (am_file am).

Lemma rmap_atoms l : rmap atom (map SA l) = Good l.
Proof. induction l as [|a l IH]; simpl; [reflexivity|]. rewrite IH. reflexivity. Qed.

Lemma read_mods : forall ms ams,
  rmap read_module
       (map (fun m => SL false [SA "module"; SA (lm_path m); SA (if lm_wants m then "1" else "0");
                                 SL false (map SA (lm_targets m)); lm_dump m]) ms) = Good ams ->
  Forall2 ModRead ms ams.
Proof.
  induction ms as [|m ms IH]; intros ams H; simpl in H.
  - injection H as <-. constructor.
  - unfold read_module at 1 in H. rewrite rmap_atoms in H. simpl in H.
    destruct (read_file (lm_dump m)) as [f|msg] eqn:Ef; [|discriminate]. simpl in H.
    destruct (rmap _ _) as [ams'|msg] eqn:Er; [|discriminate]. injection H as <-.
    constructor; [|apply IH; reflexivity].
    repeat split; simpl; auto. destruct (lm_wants m); reflexivity.
Qed.

Lemma forall2_map_path ms ams : Forall2 ModRead ms ams -> map am_path ams = map lm_path ms.
Proof. induction 1 as [|m am ms ams Hr _ IH]; simpl; [reflexivity|]. destruct Hr as (E & _). congruence. Qed.

Lemma forall2_in_r ms ams am : Forall2 ModRead ms ams -> In am ams -> exists m, In m ms /\ ModRead m am.
Proof.
  induction 1 as [|m am' ms ams Hr _ IH]; intros Hin; [destruct Hin|].
  destruct Hin as [<-|Hin].
  - exists m. split; [left|]; auto.
  - destruct (IH Hin) as (x & ? & ?). exists x. split; [right|]; auto.
Qed.

(* Part VI's LOADER. *)
Theorem load_sound read root prelude entry b :
  load read root prelude entry = Good b -> Bundled read root prelude entry b.
Proof.
  unfold load, load_bundle. intro H.
  destruct (walk 100000 read root (normalize root None prelude) [normalize root None entry] [] []) as [ms|msg] eqn:Ew;
    unfold rbind in H; [|discriminate].
  set (p := normalize root None prelude) in *. set (e := normalize root None entry) in *.
  destruct (walk_spec read root p e _ _ _ _ _ Ew) as (Hnd & Hen & Hok & Hcl).
  { split; [reflexivity|]. split; [constructor|]. split; [intros m []|]. split; [intros m t []|].
    left; left; reflexivity. }
  unfold bundle_sx, read_bundle, rbind in H.
  destruct (rmap read_module _) as [ams|msg] eqn:Er; [|discriminate]. injection H as <-.
  apply read_mods in Er. pose proof (forall2_map_path _ _ Er) as Ep.
  unfold Bundled. cbn [b_entry b_prelude b_mods]. rewrite Ep. repeat split; auto.
  intros am Ham. destruct (forall2_in_r _ _ _ Er Ham) as (m & Hm & (E1 & E2 & E3 & E4)).
  destruct (Hok m Hm) as (text & f & Hr & Hp & Hw & Ht & Hd & Hnames).
  exists text, f. rewrite E1, E2, E3. repeat split; auto.
  - apply parse_moss_sound; auto.
  - eapply dump_reads; [exact Hnames|]. rewrite <- Hd. exact E4.
  - intros t Hin. apply (Hcl m t Hm). unfold needs. apply in_or_app. auto.
  - intros Hwt. apply (Hcl m p Hm). unfold needs. rewrite Hwt. apply in_or_app. right. left. auto.
Qed.
