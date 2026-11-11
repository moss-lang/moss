(* The checks of a closure's source binding establish its typing evidence. *)
From Stdlib Require Import List Bool String Lia.
From NM Require Import Audit EaBase EaCall EaFunctor EaLinkSite EaMergeSound EaCopyNeedSound
  EaResultBudget EaTypesSound.
From NM Require EaLinkSiteSound EaSatisfiesSound PublicCallRealize PublicTypeFacts PublicTypeRead.
Import ListNotations Audit.Semantics Audit.Semantics.Elaboration.

Local Lemma bound_in_spec : forall C t,
  bound_in C t = true <-> nlook t (c_ty C) <> None.
Proof.
  intros C t. unfold bound_in. destruct (nlook t (c_ty C)); simpl; split; congruence.
Qed.

Local Lemma link_need_scoped_spec : forall C k p,
  link_need_scoped C k p = true <-> LinkNeedScope C k p.
Proof.
  intros C k p. unfold link_need_scoped, LinkNeedScope. rewrite forallb_forall.
  split; intros Scope x Member; apply bound_in_spec, Scope, Member.
Qed.

Lemma choose_link_target_sound : forall E Cu Cg nsu rho k p error choice,
  choose_link_target E Cu Cg nsu rho k p error = Ok choice ->
  LinkNeedChoice Cu Cg nsu rho k p choice.
Proof.
  intros E Cu Cg nsu rho k p error choice Checked. unfold choose_link_target in Checked.
  destruct (link_need_scoped Cu k p && link_need_scoped Cg k p) eqn:Scope.
  - apply andb_true_iff in Scope as [InCu InCg].
    apply link_need_scoped_spec in InCu, InCg.
    destruct (existsb (fun other => item_eqb (copy_need_key Cu rho k) (copy_need_key Cu rho other)) nsu) eqn:Matching.
    + destruct (choose_copy_need E "" "" Cu Cg rho nsu k p) as [other|] eqn:Selected; [|discriminate].
      cbn [rbind] in Checked.
      destruct (ilook (provision_id other) (c_provisions Cu)) as [q|] eqn:Lookup; [|discriminate].
      destruct (link_need_scoped Cu other q) eqn:TargetScope; [|discriminate].
      injection Checked as <-. cbn [LinkNeedChoice]. split; [exact InCu|]. split; [exact InCg|]. split.
      * eapply choose_copy_need_sound. exact Selected.
      * intros target Read. rewrite Lookup in Read. injection Read as <-. now apply link_need_scoped_spec.
    + injection Checked as <-. cbn [LinkNeedChoice]. split.
      * intros _ other Member Equal. assert (Exists : existsb (fun other => item_eqb (copy_need_key Cu rho k) (copy_need_key Cu rho other)) nsu = true).
        { apply existsb_exists. exists other. split; [exact Member|].
          apply PublicCallRealize.item_eqb_spec. exact Equal. }
        congruence.
      * intros Bad. exfalso. apply Bad. auto.
  - unfold linked_need in Checked. destruct (imem (canon_item Cu k) nsu) eqn:Linked; [discriminate|].
    injection Checked as <-. cbn [LinkNeedChoice]. split.
    + intros [InCu InCg]. apply link_need_scoped_spec in InCu, InCg. rewrite InCu, InCg in Scope. discriminate.
    + intros _ Member. apply imem_spec in Member. congruence.
Qed.

Local Lemma choice_lookup : forall (decide : item -> er (option item)) needs chosen,
  rmap (fun k => let! result := decide k in Ok (k, result)) needs = Ok chosen ->
  forall k, In k needs -> exists result, decide k = Ok result /\ ilook k chosen = Some result.
Proof.
  intros decide needs. induction needs as [|first rest IH]; intros chosen Run k Member; [contradiction|].
  cbn [rmap rbind] in Run. destruct (decide first) as [result|] eqn:First; [|discriminate].
  destruct (rmap (fun k => let! result := decide k in Ok (k, result)) rest) as [tail|] eqn:Rest; [|discriminate].
  injection Run as <-. cbn [ilook]. destruct (item_eqb k first) eqn:Equal.
  - apply PublicCallRealize.item_eqb_spec in Equal. subst first. eauto.
  - destruct Member as [Same|Member].
    + subst first. assert (item_eqb k k = true) by (apply PublicCallRealize.item_eqb_spec; reflexivity). congruence.
    + exact (IH tail eq_refl k Member).
Qed.

Theorem check_link_binding_sound : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  forall u gg Cu nsu Cg nsg recorded s after target,
  FunCtx (e_W E) u Cu nsu -> bound <= es_next s ->
  check_link_binding E u gg Cu nsu Cg nsg recorded s = Ok (target, after) ->
  after = s /\ LinkBinding (e_W E) u Cu nsu Cg nsg recorded target.
Proof.
  intros E bound Bounded Budget u gg Cu nsu Cg nsg recorded s after target FCu Counter Checked.
  unfold check_link_binding, mbind, get in Checked.
  set (rho := site_theta (es_next s) Cu recorded) in *.
  set (error := (qs (e_W E) gg ++ " cannot provide " ++ qs (e_W E) u ++ ": its linked input interpretation differs (D84)")%string) in Checked.
  set (decide := link_need_choice E Cu Cg nsu rho error) in Checked.
  unfold pure in Checked.
  destruct (rmap (fun k => let! choice := decide k in Ok (k, choice)) nsg) as [choices|] eqn:Choices; [|discriminate].
  set (chosen := fun k => match ilook k choices with Some choice => choice | None => None end) in Checked.
  destruct (find (unsafe_capture Cu Cg chosen (EaTypes.fixed (e_W E) (e_metadata_fuel E) u)) nsg)
    as [offender|] eqn:Capture; [discriminate|].
  destruct (forallb (fun q => linked_type_agrees Cu Cg rho (fst q)) (c_ty Cg)) eqn:Types; [|discriminate].
  injection Checked as <- <-. split; [reflexivity|]. constructor.
  - intros k p t InNeed Captured Lookup Mentions Bound.
    pose proof (find_none _ _ Capture k InNeed) as Safe.
    unfold unsafe_capture, is_linked in Safe. rewrite Captured, Lookup in Safe.
    apply negb_false_iff in Safe.
    pose proof (proj1 (forallb_forall _ _) Safe t Mentions) as TypeSafe.
    cbn beta in TypeSafe. rewrite (proj2 (bound_in_spec Cu t) Bound) in TypeSafe.
    apply (fixed_exact _ _ Bounded _ _ Budget _), nmem_spec, TypeSafe.
  - cbv zeta. set (fixed := copy_fixed_readings Cu recorded).
    set (ys := site_classes Cu fixed). set (cg := site_atom (es_next s) recorded).
    exists ys, cg, (extend_allocation_scope (copy_allocation_scope (e_W E) recorded) (map cg ys)).
    cbv zeta. change (copy_subst fixed ys cg) with rho.
    split.
    + apply EaLinkSiteSound.site_allocation. intros g d Found. specialize (Bounded g d Found). lia.
    + split; [exact (EaLinkSiteSound.site_classes_spec _ _ _ _ _ FCu)|]. split.
      * intros k Member. destruct (choice_lookup _ _ _ Choices k Member) as [result [Selected Lookup]].
        unfold chosen. rewrite Lookup. unfold decide, link_need_choice in Selected.
        destruct (ilook (provision_id k) (c_provisions Cg)) as [p|] eqn:Provision; [|discriminate].
        exists p. split; [reflexivity|]. eapply choose_link_target_sound. exact Selected.
      * intros x tx Lookup Bound.
        pose proof (PublicTypeRead.nlook_in _ _ _ _ Lookup) as Member.
        pose proof (proj1 (forallb_forall _ _) Types (x, tx) Member) as Agrees.
        cbn [fst] in Agrees. unfold linked_type_agrees in Agrees.
        rewrite Lookup, (proj2 (bound_in_spec Cu x) Bound) in Agrees.
        apply andb_true_iff in Agrees as [Scoped Equal]. split.
        -- intros y InType. apply bound_in_spec. exact (proj1 (forallb_forall _ _) Scoped y InType).
        -- now apply PublicTypeFacts.ty_eqb_spec.
Qed.

Theorem translate_link_targets_sound : forall Cu Cg target needs,
  translate_link_targets Cu Cg target needs = link_map Cu Cg target needs.
Proof.
  intros Cu Cg target needs. unfold translate_link_targets, link_map.
  apply map_ext. intro k. now rewrite !EaSatisfiesSound.rkof_exact.
Qed.

Theorem link_need_choice_unique : forall Cu Cg nsu rho k p first second,
  LinkNeedChoice Cu Cg nsu rho k p first ->
  LinkNeedChoice Cu Cg nsu rho k p second -> first = second.
Proof.
  intros Cu Cg nsu rho k p [first|] [second|] Left Right; cbn [LinkNeedChoice] in *.
  - f_equal. eapply copy_need_choice_unique.
    + exact (proj1 (proj2 (proj2 Left))).
    + exact (proj1 (proj2 (proj2 Right))).
  - destruct Left as [InU [InG [[Member [[Same _] _]] _]]].
    exfalso. exact (proj1 Right (conj InU InG) first Member Same).
  - destruct Right as [InU [InG [[Member [[Same _] _]] _]]].
    exfalso. exact (proj1 Left (conj InU InG) second Member Same).
  - reflexivity.
Qed.
