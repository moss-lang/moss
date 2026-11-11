(* Successful result reading gives the fuel needed by both passes of the
   family classifier. Its dependency tests use the separate metadata bound. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaBudget EaTypes EaTypesSound EaFamily EaOutputSound.
From NM Require EaFamilySound EaDependencyFuel EaMentionsSound EaResultBudget EaResultCoverage EaMain.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Local Lemma ctx_spec_items_map : forall specs, ctx_spec_items (map CISpec specs) = Some specs.
Proof. induction specs; cbn [map ctx_spec_items]; [reflexivity|now rewrite IHspecs]. Qed.

Section ResultDepth.
Variable E : env.
Let W := e_W E.

Local Fixpoint ResultDepth (fuel : nat) (p : String.string) (syntax : spec) : Prop :=
  match fuel with
  | 0 => False
  | S next =>
      match syntax with
      | Spec (Some pa) None _ =>
          match resolve_sym W p pa with
          | Some s => match w_decl W s with
                      | Some d => match d_ast d with
                                  | ADCtx _ items =>
                                      Forall (ResultDepth next (d_mod d)) (flat_map citem_specs items)
                                  | _ => True
                                  end
                      | None => True
                      end
          | None => True
          end
      | _ => True
      end
  end.

Local Lemma result_depth_more : forall fuel p syntax,
  ResultDepth fuel p syntax -> ResultDepth (S fuel) p syntax.
Proof.
  induction fuel as [|fuel IH]; intros p [[pa|] [dn|] br] Read; cbn [ResultDepth] in *;
    try contradiction; try exact Read.
  destruct (resolve_sym W p pa) as [s|]; [|exact Read].
  destruct (w_decl W s) as [d|]; [|exact Read].
  destruct (d_ast d); try exact Read.
  apply Forall_forall. intros child Present. apply IH.
  apply Forall_forall with (x := child) in Read; assumption.
Qed.

Local Lemma result_depth_from_reads : forall fuel,
  (forall stack p ob tm syntax es meanings,
    output_item_entries E fuel stack p ob tm syntax = Ok (es, meanings) ->
    ResultDepth fuel p syntax) /\
  (forall stack p ob tm syntax es meanings,
    output_items_entries E fuel stack p ob tm syntax = Ok (es, meanings) ->
    Forall (ResultDepth fuel p) syntax).
Proof.
  induction fuel as [|fuel [Item Items]]; [split; intros; discriminate|].
  split.
  - intros stack p ob tm [[pa|] [dn|] br] es meanings Run; cbn [ResultDepth]; try exact I.
    cbn [output_item_entries] in Run. fold W in Run.
    destruct (resolve_sym W p pa) as [s|]; [|exact I]. cbn [rbind] in Run.
    unfold decl_of in Run. destruct (w_decl W s) as [d|]; [|exact I]. cbn [rbind] in Run.
    apply rbind_ok in Run as (bindings & Bindings & Run).
    destruct (d_ast d); try exact I.
    destruct (nmem s stack); [discriminate|]. cbn [rbind] in Run.
    destruct (ctx_spec_items l) as [specs|] eqn:ReadSpecs; [|discriminate]. cbn [rbind] in Run.
    rewrite (ctx_spec_items_sound _ _ ReadSpecs).
    assert (Flatten : flat_map citem_specs (map CISpec specs) = specs).
    { clear - specs. induction specs; cbn [map flat_map citem_specs app];
        [reflexivity|now rewrite IHspecs]. }
    rewrite Flatten.
    eapply Items. exact Run.
  - intros stack p ob tm [|head rest] es meanings Run; [constructor|].
    cbn [output_items_entries] in Run.
    apply rbind_ok in Run as ([head_entries head_meanings] & First & Run).
    apply rbind_ok in Run as ([tail_entries tail_meanings] & Rest & Run).
    constructor.
    + apply result_depth_more. eapply Item. exact First.
    + pose proof (Items _ _ _ _ _ _ _ Rest) as Tail.
      apply Forall_forall. intros child Present. apply result_depth_more.
      apply Forall_forall with (x := child) in Tail; assumption.
Qed.

Local Lemma family_result_complete_at_depth : forall fuel p syntax,
  ResultDepth fuel p syntax ->
  FamilyResult W p syntax -> family_result E fuel p syntax = true.
Proof.
  induction fuel as [|fuel IH]; intros p syntax Depth Shape; [contradiction|].
  revert Depth. destruct Shape as [p pa dn br|p pa br s d n items specs Resolve Declared Ast Syntax Children];
    intro Depth; [reflexivity|].
  subst items. cbn [ResultDepth] in Depth. cbn [family_result]. fold W.
  rewrite Resolve, Declared, Ast in *. rewrite ctx_spec_items_map.
  apply forallb_forall. intros child Present. apply (IH (d_mod d)).
  - apply Forall_forall with (x := child) in Depth.
    + exact Depth.
    + apply in_flat_map. exists (CISpec child). split; [now apply in_map|now left].
  - apply Forall_forall with (x := child) in Children; assumption.
Qed.

Section Keys.
Variable bound : nat.
Hypothesis Bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.
Hypothesis Budget : metadata_budget W bound < e_metadata_fuel E.

Local Lemma family_keyed_complete_at_depth : forall fuel u p syntax,
  ResultDepth fuel p syntax -> FamilyKeyed W u p syntax ->
  family_keyed E fuel u (poly W (e_metadata_fuel E) u) p syntax = true.
Proof.
  induction fuel as [|fuel IH]; intros u p syntax Depth Shape; [contradiction|].
  revert Depth. destruct Shape as [p pa dn br receiver method Resolve Named Keyed|
    p pa br s d n items specs Resolve Declared Ast Syntax Children]; intro Depth.
  - cbn [family_keyed]. fold W. rewrite Resolve, (EaResultCoverage.method_named_complete _ _ _ _ _ Named).
    apply forallb_forall. intros x Present.
    cbn beta zeta.
    destruct ((nmem x (sig_mentions W (e_metadata_fuel E) method) ||
      family_bracket_free E p br x) && negb (nmem x (bracket_bound W p br))) eqn:Checks;
      [|reflexivity].
    apply andb_true_iff in Checks as [Mention Unbound].
    apply nmem_spec. eapply EaDependencyFuel.free_in_complete_above.
    2: { change (1 <= e_metadata_fuel E). lia. }
    apply Keyed.
    + apply (proj1 (EaResultBudget.poly_exact _ bound Bounded _ u Budget x)), Present.
    + apply orb_true_iff in Mention as [Sig|Bracket].
      * left. apply nmem_spec in Sig. exact (EaMentionsSound.sig_mentions_sound _ _ _ _ Sig).
      * right. now apply EaFamilySound.family_bracket_free_sound.
    + intro Bound. apply negb_true_iff in Unbound.
      apply EaMentionsSound.bracket_bound_members in Bound. apply nmem_spec in Bound.
      congruence.
  - subst items. cbn [ResultDepth] in Depth. cbn [family_keyed]. fold W.
    rewrite Resolve, Declared, Ast in *. rewrite ctx_spec_items_map.
    apply forallb_forall. intros child Present. apply IH.
    + apply Forall_forall with (x := child) in Depth.
      * exact Depth.
      * apply in_flat_map. exists (CISpec child). split; [now apply in_map|now left].
    + apply Forall_forall with (x := child) in Children; assumption.
Qed.

Theorem is_family_complete_after_read : forall u C out C' keys,
  provide_result E u C out = Ok (C', keys) ->
  Family W u -> is_family E u = true.
Proof.
  intros u C out C' keys Run ((d & n & ps & a & r & Declared & Ast & Shape & Names) & Keyed).
  pose proof (Keyed _ _ _ _ _ Declared Ast) as Keys.
  unfold provide_result, functor_decl in Run. fold W in Run.
  rewrite Declared in Run. unfold functor_parts in Run. rewrite Ast in Run. cbn [rbind] in Run.
  apply rbind_ok in Run as ([entries meanings] & Read & _).
  pose proof (proj2 (result_depth_from_reads (e_fuel E)) _ _ _ _ _ _ _ Read) as Depth.
  unfold is_family. fold W. rewrite Declared, Ast.
  apply andb_true_iff. split; [apply andb_true_iff; split|].
  - apply forallb_forall. intros child Present. apply family_result_complete_at_depth;
      apply Forall_forall with (x := child) in Depth, Shape; assumption.
  - destruct ps as [names|]; [|reflexivity].
    apply forallb_forall. intros x Present. cbn beta.
    destruct (resolve_sym W (d_mod d) [x]) as [t|] eqn:Resolved; [|reflexivity].
    apply kind_is_spec. eapply Names; [reflexivity|exact Present|exact Resolved].
  - apply forallb_forall. intros child Present. apply family_keyed_complete_at_depth;
      apply Forall_forall with (x := child) in Depth, Keys; assumption.
Qed.
End Keys.
End ResultDepth.

Theorem decls_form_family_classifier : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  EaMain.decls_form E bound = Ok tt ->
  forall u, Family (e_W E) u -> is_family E u = true.
Proof.
  intros E bound Bounded Budget Checks u FamilyFormed.
  destruct FamilyFormed as ((d & n & ps & a & r & Declared & Ast & Shape & Names) & Keyed).
  assert (Present : In u (seq 0 bound)) by
    (apply in_seq; pose proof (Bounded _ _ Declared); lia).
  unfold EaMain.decls_form in Checks. apply rbind_ok in Checks as (checked & Checks & _).
  destruct (EaDependencyFuel.rmap_member_read _ _ _ _ _ Checks _ Present) as (result & Read).
  destruct (EaMain.decl_forms E bound u) as [formed|message] eqn:Formed; [|discriminate Read].
  unfold EaMain.decl_forms in Formed. rewrite Declared, Ast in Formed.
  apply rbind_ok in Formed as ([context needs] & _ & Formed).
  apply rbind_ok in Formed as ([called keys] & Provide & _).
  eapply is_family_complete_after_read; [exact Bounded|exact Budget|exact Provide|].
  split; [|exact Keyed]. exists d, n, ps, a, r. auto.
Qed.
