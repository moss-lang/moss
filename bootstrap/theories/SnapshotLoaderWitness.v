(* Declarative bundles may retain extra files. They nevertheless agree on
   every reachable module, and therefore describe the same static worlds. *)
From Stdlib Require Import List String Bool.
From NM Require AuditBase.
From NM Require Import Audit SourceSnapshot SnapshotLoader.
From NM Require FrontEndComplete Parse Sx.
From NM Require Import AuditFacts.
Import Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.
Module L := Audit.Loader.

Lemma module_of_member : forall b p m,
  module_of b p = Some m -> In m (b_mods b) /\ am_path m = p.
Proof.
  intros b p m Found. unfold module_of in Found.
  apply find_some in Found as [Member Equal]. split; auto.
  apply String.eqb_eq; exact Equal.
Qed.

Lemma module_of_present : forall b p,
  In p (map am_path (b_mods b)) -> exists m, module_of b p = Some m.
Proof.
  intros b p Present. unfold module_of.
  destruct (find (fun m => String.eqb (am_path m) p) (b_mods b)) as [m|] eqn:Found;
    [eauto|].
  apply in_map_iff in Present as [m [Path Member]].
  pose proof (find_none _ _ Found m Member) as Equal.
  cbn in Equal.
  rewrite Path, String.eqb_refl in Equal. discriminate.
Qed.

Lemma bundled_module_same : forall read root prelude entry b c m n,
  L.Bundled read root prelude entry b -> L.Bundled read root prelude entry c ->
  In m (b_mods b) -> In n (b_mods c) -> am_path m = am_path n -> m = n.
Proof.
  intros read root prelude entry b c m n
    [BE [BP [BU [BI BM]]]] [CE [CP [CU [CI CM]]]] M N Path.
  destruct (BM m M) as [text [ast [Read [Derived [File [Wants [Targets Rest]]]]]]].
  destruct (CM n N) as [text' [ast' [Read' [Derived' [File' [Wants' [Targets' Rest']]]]]]].
  rewrite <- Path, Read in Read'. injection Read' as <-.
  apply FrontEndComplete.parse_moss_complete in Derived, Derived'.
  assert (ast = ast') by congruence. subst ast'.
  assert (SamePrelude : b_prelude b = b_prelude c) by congruence.
  rewrite SamePrelude in Wants, Targets.
  destruct m, n; cbn in *. subst. f_equal; congruence.
Qed.

Lemma bundled_target_present : forall read root prelude entry b p m t imp,
  L.Bundled read root prelude entry b -> module_of b p = Some m ->
  In (t, imp) (imports_of b m) -> In t (map am_path (b_mods b)).
Proof.
  intros read root prelude entry b p m t imp Bundle Module Import.
  destruct (module_of_member _ _ _ Module) as [Member Path].
  destruct Bundle as [_ [_ [_ [_ Modules]]]].
  destruct (Modules m Member) as [text [ast [Read [Derived [File [Wants [Targets [Closed Prelude]]]]]]]].
  unfold imports_of in Import. apply in_app_or in Import as [Implicit|Explicit].
  - destruct (am_prelude m) eqn:Wanted; cbn in Implicit; [|contradiction].
    destruct Implicit as [Equal|[]]. injection Equal as <- <-. apply Prelude; reflexivity.
  - apply Closed. apply in_combine_l in Explicit; exact Explicit.
Qed.

Lemma bundled_loaded_present : forall read root prelude entry b p,
  L.Bundled read root prelude entry b -> Loaded b p ->
  exists m, module_of b p = Some m.
Proof.
  intros read root prelude entry b p Bundle Loaded.
  apply module_of_present. destruct Loaded as [|p m t imp Loaded Module Import].
  - destruct Bundle as [_ [_ [_ [Present _]]]]; exact Present.
  - eapply bundled_target_present; eauto.
Qed.

Lemma bundled_loaded_forward : forall read root prelude entry b c p,
  L.Bundled read root prelude entry b -> L.Bundled read root prelude entry c ->
  Loaded b p -> Loaded c p /\ module_of b p = module_of c p.
Proof.
  intros read root prelude entry b c p B C Reach.
  assert (Entry : b_entry b = b_entry c) by (destruct B as [BE _]; destruct C as [CE _]; congruence).
  assert (Prelude : b_prelude b = b_prelude c) by
    (destruct B as [_ [BP _]]; destruct C as [_ [CP _]]; congruence).
  assert (Agree : forall p, Loaded b p -> Loaded c p -> module_of b p = module_of c p).
  { intros path RB RC. destruct (bundled_loaded_present _ _ _ _ _ _ B RB) as [m M].
    destruct (bundled_loaded_present _ _ _ _ _ _ C RC) as [n N].
    destruct (module_of_member _ _ _ M) as [Member Path].
    destruct (module_of_member _ _ _ N) as [Member' Path'].
    assert (m = n).
    { apply (bundled_module_same read root prelude entry b c m n B C Member Member'); congruence. }
    congruence. }
  induction Reach as [|p m t imp Reach [Reached Same] Module Import].
  - split; [rewrite Entry; constructor|apply Agree; [constructor|rewrite Entry;constructor]].
  - assert (LoadedTarget : Loaded c t).
    { eapply LoadImport; [exact Reached|rewrite <- Same; exact Module|].
      unfold imports_of in *. rewrite <- Prelude; exact Import. }
    split; [exact LoadedTarget|apply Agree; [eapply LoadImport; eauto|exact LoadedTarget]].
Qed.

Theorem bundled_reachable_agreement : forall read root prelude entry b c,
  L.Bundled read root prelude entry b -> L.Bundled read root prelude entry c ->
  (forall p, Loaded b p <-> Loaded c p) /\
  (forall p, Loaded b p -> module_of b p = module_of c p).
Proof.
  intros read root prelude entry b c B C. split.
  - intro p; split; intro Reach.
    + exact (proj1 (bundled_loaded_forward read root prelude entry b c p B C Reach)).
    + exact (proj1 (bundled_loaded_forward read root prelude entry c b p C B Reach)).
  - intros p Reach. exact (proj2 (bundled_loaded_forward read root prelude entry b c p B C Reach)).
Qed.

Lemma exports_transport : forall b c W,
  (forall p, Loaded b p -> module_of b p = module_of c p) ->
  forall p ns n e, Exports b W p ns n e -> Loaded b p -> Exports c W p ns n e.
Proof.
  intros b c W Agree. fix IH 5. intros p ns n e Export Reach.
  destruct Export as [p s d ns n Decl Mod Filed |
    p m t imp u e ns Module Import Glob Use Space Source |
    p m t imp a Module Import Alias].
  - eapply ExOwn; eauto.
  - assert (Target : Loaded b t).
    { eapply LoadImport; eauto using explicit_in_imports. }
    eapply ExUse; eauto.
    + rewrite <- (Agree p Reach); exact Module.
    + destruct Source as [Source|[Dot [Absent Source]]].
      * left. exact (IH _ _ _ _ Source Target).
      * right. split; [exact Dot|]. split; [exact Absent|]. exact (IH _ _ _ _ Source Target).
  - eapply ExAlias; eauto. rewrite <- (Agree p Reach); exact Module.
Qed.

Lemma ordinary_scope_transport : forall b c W,
  (forall p, Loaded b p -> module_of b p = module_of c p) ->
  forall p ns n e, Loaded b p -> OrdinaryScope b W p ns n e -> OrdinaryScope c W p ns n e.
Proof.
  intros b c W Agree p ns n e Reach Scope.
  destruct Scope as [p ns n e Export|p m t imp ns n e Module Import Glob Export].
  - apply ScExported. eapply exports_transport; eauto.
  - eapply ScGlob; eauto.
    + rewrite <- (Agree p Reach); exact Module.
    + eapply exports_transport; eauto. eapply LoadImport; eauto using explicit_in_imports.
Qed.

Section WorldTransport.
Variables b c : bundle.
Hypothesis Prelude : b_prelude b = b_prelude c.
Hypothesis Reachable : forall p, Loaded b p <-> Loaded c p.
Hypothesis Modules : forall p, Loaded b p -> module_of b p = module_of c p.

Lemma modules_reverse : forall p, Loaded c p -> module_of c p = module_of b p.
Proof. intros p Reach; symmetry; apply Modules, Reachable; exact Reach. Qed.

Lemma declared_agree : forall d, Declared b d <-> Declared c d.
Proof.
  intro d; unfold Declared. split; intros [Reach [m [Module Occurs]]].
  - split; [apply Reachable; exact Reach|]. exists m; split; auto.
    rewrite <- (Modules _ Reach); exact Module.
  - split; [apply Reachable; exact Reach|]. exists m; split; auto.
    rewrite <- (modules_reverse _ Reach); exact Module.
Qed.

Lemma exports_agree : forall W p ns n e, Loaded b p ->
  (Exports b W p ns n e <-> Exports c W p ns n e).
Proof.
  intros W p ns n e Reach; split; intro Export.
  - eapply exports_transport; eauto.
  - eapply exports_transport; [exact modules_reverse|exact Export|apply Reachable; exact Reach].
Qed.

Lemma ordinary_scope_agree : forall W p ns n e, Loaded b p ->
  (OrdinaryScope b W p ns n e <-> OrdinaryScope c W p ns n e).
Proof.
  intros W p ns n e Reach; split; intro Scope.
  - eapply ordinary_scope_transport; eauto.
  - eapply ordinary_scope_transport; [exact modules_reverse|apply Reachable; exact Reach|exact Scope].
Qed.

Lemma scope_agree : forall W p ns n e, Loaded b p ->
  (InScope b W p ns n e <-> InScope c W p ns n e).
Proof.
  intros W p ns n e Reach; split; intro Scope.
  - destruct Scope as [p ns n e Ordinary|p m ns n e Module Wants Export Absent].
    + apply ScPrimary, ordinary_scope_agree; assumption.
    + eapply ScPrelude; [rewrite <- (Modules _ Reach); exact Module|exact Wants| |].
      * rewrite <- Prelude. apply exports_agree; [|exact Export].
        eapply LoadImport; eauto using prelude_in_imports.
      * intros e' Ordinary. apply (Absent e'), ordinary_scope_agree; assumption.
  - destruct Scope as [p ns n e Ordinary|p m ns n e Module Wants Export Absent].
    + apply ScPrimary, ordinary_scope_agree; assumption.
    + eapply ScPrelude; [rewrite (Modules _ Reach); exact Module|exact Wants| |].
      * apply exports_agree; [|rewrite Prelude; exact Export].
        apply Reachable. rewrite Prelude. eapply LoadImport; [apply Reachable; exact Reach|exact Module|].
        apply prelude_in_imports; exact Wants.
      * intros e' Ordinary. apply (Absent e'), ordinary_scope_agree; assumption.
Qed.

Theorem world_transport : forall W, WorldOf b W -> WorldOf c W.
Proof.
  intros W World. destruct World as
    [Acyclic DeclSound DeclComplete Exports Scope Attached NoDup Plain None Closed Least Listed Library].
  constructor; auto.
  - destruct Acyclic as [rank Acyclic]. exists rank.
    intros p m t imp Reach Module Import.
    assert (ReachB : Loaded b p) by (apply Reachable; exact Reach).
    rewrite <- (Modules _ ReachB) in Module.
    unfold imports_of in Import. rewrite <- Prelude in Import.
    destruct (Acyclic p m t imp ReachB Module Import) as [Present Rank]. split; auto.
    assert (Target : Loaded b t) by (eapply LoadImport; eauto).
    rewrite <- (Modules _ Target); exact Present.
  - intros s d Decl. destruct (DeclSound s d Decl) as [Declared Kind]. split; auto.
    apply declared_agree; exact Declared.
  - intros d Declared Kind. apply DeclComplete; auto. apply declared_agree; exact Declared.
  - intros p ns n e Reach. rewrite (Exports p ns n e); [|apply Reachable;exact Reach].
    apply exports_agree, Reachable; exact Reach.
  - intros p ns n e Reach. rewrite (Scope p ns n e); [|apply Reachable;exact Reach].
    apply scope_agree, Reachable; exact Reach.
  - rewrite <- Prelude; exact Library.
Qed.
End WorldTransport.

Theorem bundled_world_transport : forall read root prelude entry b c W,
  L.Bundled read root prelude entry b -> L.Bundled read root prelude entry c ->
  WorldOf b W -> WorldOf c W.
Proof.
  intros read root prelude entry b c W B C World.
  destruct (bundled_reachable_agreement read root prelude entry b c B C) as [Reach Modules].
  assert (Prelude : b_prelude b = b_prelude c).
  { destruct B as [_ [BP _]]; destruct C as [_ [CP _]]; congruence. }
  exact (world_transport b c Prelude Reach Modules W World).
Qed.

Theorem load_world_complete : forall snapshot prelude entry witness W,
  L.Bundled (source_read snapshot) "/" (path_text prelude) (path_text entry) witness ->
  WorldOf witness W ->
  exists actual, SnapshotLoader.load snapshot prelude entry = Sx.Good actual /\ WorldOf actual W.
Proof.
  intros snapshot prelude entry witness W Bundle World.
  destruct (SnapshotLoader.load_complete _ _ _ _ Bundle) as [actual Run].
  exists actual; split; auto. eapply bundled_world_transport; eauto using SnapshotLoader.load_sound.
Qed.

Theorem bundled_elaborates_in_transport : forall read root prelude entry b c W P,
  L.Bundled read root prelude entry b -> L.Bundled read root prelude entry c ->
  ElaboratesIn b W P -> ElaboratesIn c W P.
Proof.
  intros read root prelude entry b c W P B C [resolve [World Rest]]. exists resolve. split.
  - exact (bundled_world_transport read root prelude entry b c W B C World).
  - assert (Entry : b_entry b = b_entry c).
    { destruct B as [BE _]; destruct C as [CE _]; congruence. }
    rewrite <- Entry; exact Rest.
Qed.

Theorem bundled_elaborates_transport : forall read root prelude entry b c P,
  L.Bundled read root prelude entry b -> L.Bundled read root prelude entry c ->
  Elaborates b P -> Elaborates c P.
Proof.
  intros read root prelude entry b c P B C [W Elaborated].
  exists W. exact (bundled_elaborates_in_transport read root prelude entry b c W P B C Elaborated).
Qed.

Theorem load_elaborates_complete : forall snapshot prelude entry witness P,
  L.Bundled (source_read snapshot) "/" (path_text prelude) (path_text entry) witness ->
  Elaborates witness P ->
  exists actual, SnapshotLoader.load snapshot prelude entry = Sx.Good actual /\ Elaborates actual P.
Proof.
  intros snapshot prelude entry witness P Bundle Elaborated.
  destruct (SnapshotLoader.load_complete _ _ _ _ Bundle) as [actual Run].
  exists actual; split; auto. eapply bundled_elaborates_transport; eauto using SnapshotLoader.load_sound.
Qed.
