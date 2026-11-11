(* Canonicalization facts supplied by actual public context formation. *)
From Stdlib Require Import List Arith Bool.
From NM Require AuditFacts.
From NM Require Import Audit PublicAliasContext.
From NM Require PublicBodySafety PublicLinkAnchors PublicMerge PublicSubstFacts PublicTypeRead.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition canonical_context (C : A.cx) : Prop :=
  forall tau, A.canon C (A.canon C tau) = A.canon C tau.

Definition CxSection (C : A.cx) : Prop := A.SectionOf (A.c_merge C) (A.c_sub C).


(* A reading gives each merge class one meaning. *)
Definition ReadsMerge (C : A.cx) (read : ty -> ty) : Prop :=
  forall t u, A.MergeEq (A.c_merge C) t u -> read t = read u.

(* A source input reads at the application if the target shares it.
   Otherwise its meaning comes from the lexical capture. *)
Definition link_base_reading (Cu Cg : A.cx) (T : subst) (dc : environment) : subst :=
  map (fun q => (fst q,
    if existsb (Nat.eqb (fst q)) (map fst (A.c_ty Cu))
    then app_subst T (TAbs (fst q)) else PublicBodySafety.cmeaning dc (fst q))) (A.c_ty Cg).

Lemma link_base_reading_shared : forall Cu Cg T dc x,
  In x (map fst (A.c_ty Cu)) -> In x (map fst (A.c_ty Cg)) ->
  lookup_subst x (link_base_reading Cu Cg T dc) = Some (app_subst T (TAbs x)).
Proof.
  intros Cu Cg T dc x Shared Source. unfold link_base_reading.
  rewrite (PublicSubstFacts.lookup_on_keys (fun y =>
    if existsb (Nat.eqb y) (map fst (A.c_ty Cu))
    then app_subst T (TAbs y) else PublicBodySafety.cmeaning dc y) _ x Source).
  cbn beta.
  assert (Found : existsb (Nat.eqb x) (map fst (A.c_ty Cu)) = true).
  { apply existsb_exists. exists x. split; [exact Shared|apply Nat.eqb_refl]. }
  now rewrite Found.
Qed.

Lemma link_base_reading_private : forall Cu Cg T dc x,
  ~ In x (map fst (A.c_ty Cu)) -> In x (map fst (A.c_ty Cg)) ->
  lookup_subst x (link_base_reading Cu Cg T dc) = Some (PublicBodySafety.cmeaning dc x).
Proof.
  intros Cu Cg T dc x Private Source. unfold link_base_reading.
  rewrite (PublicSubstFacts.lookup_on_keys (fun y =>
    if existsb (Nat.eqb y) (map fst (A.c_ty Cu))
    then app_subst T (TAbs y) else PublicBodySafety.cmeaning dc y) _ x Source).
  cbn beta.
  destruct (existsb _ _) eqn:Found; [|reflexivity]. exfalso. apply Private.
  apply existsb_exists in Found as [y [Member Equal]]. apply Nat.eqb_eq in Equal. now subst y.
Qed.

Lemma link_base_reading_same_capture : forall Cu Cg T dc dc',
  (forall x, PublicBodySafety.cmeaning dc x = PublicBodySafety.cmeaning dc' x) ->
  link_base_reading Cu Cg T dc = link_base_reading Cu Cg T dc'.
Proof.
  intros Cu Cg T dc dc' Same. unfold link_base_reading.
  apply map_ext. intros [x t]. cbn [fst]. now rewrite Same.
Qed.

(* A body's canonical leaves read through the emitted input table. *)
Definition link_reading (Cu Cg : A.cx) (T : subst) (dc : environment) : subst :=
  let base := link_base_reading Cu Cg T dc in
  let anchors := A.link_input_anchors Cu Cg in
  map (fun q => (fst q, app_subst base (app_subst anchors (TAbs (fst q))))) (A.c_ty Cg).

Lemma link_reading_lookup : forall Cu Cg T dc x,
  In x (map fst (A.c_ty Cg)) ->
  lookup_subst x (link_reading Cu Cg T dc) =
  Some (app_subst (link_base_reading Cu Cg T dc) (app_subst (A.link_input_anchors Cu Cg) (TAbs x))).
Proof.
  intros Cu Cg T dc x Member. unfold link_reading.
  exact (PublicSubstFacts.lookup_on_keys (fun y => app_subst (link_base_reading Cu Cg T dc)
    (app_subst (A.link_input_anchors Cu Cg) (TAbs y))) (A.c_ty Cg) x Member).
Qed.

Lemma link_reading_out : forall Cu Cg T dc x,
  ~ In x (map fst (A.c_ty Cg)) -> lookup_subst x (link_reading Cu Cg T dc) = None.
Proof.
  intros Cu Cg T dc x Outside. unfold link_reading.
  destruct (lookup_subst x _) as [v|] eqn:Read; [|reflexivity].
  apply PublicSubstFacts.lookup_subst_in_pair in Read. apply in_map_iff in Read as [[y t] [Same Member]].
  injection Same as <- _. exfalso. apply Outside. exact (in_map fst _ (y,t) Member).
Qed.

Lemma link_reading_compose : forall Cu Cg T dc t,
  incl (A.gens t) (map fst (A.c_ty Cg)) ->
  app_subst (link_reading Cu Cg T dc) t =
  app_subst (link_base_reading Cu Cg T dc) (app_subst (A.link_input_anchors Cu Cg) t).
Proof.
  intros Cu Cg T dc t Scope. rewrite <- (PublicTypeRead.app_subst_nil t) at 1.
  apply PublicTypeRead.subst2_agree. intros x Member. cbn [app_subst lookup_subst].
  now rewrite link_reading_lookup by exact (Scope x Member).
Qed.

Lemma link_reading_shared : forall Cu Cg T dc x,
  In x (map fst (A.c_ty Cu)) -> In x (map fst (A.c_ty Cg)) ->
  lookup_subst x (link_reading Cu Cg T dc) = Some (app_subst T (TAbs x)).
Proof.
  intros Cu Cg T dc x Shared Source. rewrite link_reading_lookup by exact Source.
  cbn [app_subst]. rewrite PublicLinkAnchors.input_anchor_keeps_shared by
    (rewrite <- PublicSubstFacts.lookup_subst_nlook; now apply PublicSubstFacts.lookup_subst_in).
  cbn [app_subst]. now rewrite link_base_reading_shared.
Qed.

Lemma link_reading_anchor : forall Cu Cg T dc y x,
  lookup_subst y (A.link_input_anchors Cu Cg) = Some (TAbs x) ->
  lookup_subst y (link_reading Cu Cg T dc) = Some (app_subst T (TAbs x)).
Proof.
  intros Cu Cg T dc y x Read.
  destruct (PublicLinkAnchors.input_anchor_read _ _ _ _ Read)
    as (shared & original & input & Same & Private & Source & _ & Shared).
  injection Same as <-.
  rewrite link_reading_lookup by exact (in_map fst _ (y,original) Private).
  cbn [app_subst]. rewrite Read. cbn [app_subst]. rewrite link_base_reading_shared; [reflexivity| |exact (in_map fst _ (x,TAbs y) Source)].
  exact (in_map fst _ (x,input) (PublicTypeRead.nlook_in _ _ _ _ Shared)).
Qed.

Lemma link_reading_same_capture : forall Cu Cg T dc dc',
  (forall x, PublicBodySafety.cmeaning dc x = PublicBodySafety.cmeaning dc' x) ->
  link_reading Cu Cg T dc = link_reading Cu Cg T dc'.
Proof.
  intros Cu Cg T dc dc' Same. unfold link_reading. now rewrite (link_base_reading_same_capture _ _ _ _ _ Same).
Qed.

Lemma link_reading_adopted : forall C Cu Cg T dc t,
  app_subst (link_reading Cu Cg T dc) (app_subst (A.link_adopted_inputs C Cu Cg) t) =
  app_subst (link_reading Cu Cg T dc) t.
Proof.
  intros C Cu Cg T dc t. rewrite <- (PublicTypeRead.app_subst_nil t) at 2.
  apply PublicTypeRead.subst2_agree. intros y _. cbn [app_subst lookup_subst].
  destruct (lookup_subst y (A.link_adopted_inputs C Cu Cg)) as [value|] eqn:Adopted; [|reflexivity].
  apply PublicLinkAnchors.adopted_input_read in Adopted as [_ Anchor].
  destruct (PublicLinkAnchors.input_anchor_read _ _ _ _ Anchor)
    as (x & original & input & -> & _ & Source & _ & Shared).
  cbn [app_subst]. rewrite (link_reading_anchor _ _ _ _ _ _ Anchor).
  now rewrite link_reading_shared with (x := x) by
    first [exact (in_map fst _ (x,TAbs y) Source)|exact (in_map fst _ (x,input) (PublicTypeRead.nlook_in _ _ _ _ Shared))].
Qed.

Lemma linked_application_table : forall P d h Cu Cg outs binds sources th dc,
  view_functor P h = Some (FunctorCode (map fst (A.c_ty Cg))
    (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg)))
    (A.link_input_table Cu Cg) outs binds sources) ->
  (forall y, In y (map fst (A.c_ty Cu)) -> lookup_subst y th <> None) ->
  (forall y t, lookup_subst y (A.c_ty Cg) = Some t -> incl (A.gens t) (map fst (A.c_ty Cg))) ->
  type_meanings (application_environment P d h th dc) =
  map (fun source => (source, match lookup_subst source (A.c_ty Cg) with
    | Some t => app_subst (link_reading Cu Cg (map (fun q => (fst q, ground d (snd q))) th) dc) t
    | None => TAbs source
    end)) (map fst (A.c_ty Cg)).
Proof.
  intros P d h Cu Cg outs binds sources th dc Code Complete Scope.
  unfold application_environment. rewrite Code. cbn [type_meanings fc_inputs fc_linked fc_table].
  set (linked := filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))).
  set (base := fun source =>
    match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
    | Some meaning => ground d meaning
    | None => PublicBodySafety.cmeaning dc source
    end).
  set (arguments := map (fun q => (fst q, ground d (snd q))) th).
  assert (Base : map (fun source => (source, base source)) (map fst (A.c_ty Cg)) =
      link_base_reading Cu Cg arguments dc).
  { unfold link_base_reading. rewrite map_map. apply map_ext_in. intros [x value] Source.
    cbn [fst]. f_equal. unfold base.
    assert (Linked : existsb (Nat.eqb x) linked = existsb (Nat.eqb x) (map fst (A.c_ty Cu))).
    { destruct (existsb (Nat.eqb x) (map fst (A.c_ty Cu))) eqn:Shared.
      - apply existsb_exists. exists x. split; [|apply Nat.eqb_refl].
        apply filter_In. split; [exact (in_map fst _ (x,value) Source)|exact Shared].
      - destruct (existsb (Nat.eqb x) linked) eqn:Found; [|reflexivity].
        apply existsb_exists in Found as [y [Member Equal]]. apply Nat.eqb_eq in Equal. subst y.
        apply filter_In in Member as [_ Present]. congruence. }
    rewrite Linked. destruct (existsb (Nat.eqb x) (map fst (A.c_ty Cu))) eqn:Shared; [|reflexivity].
    assert (Member : In x (map fst (A.c_ty Cu))).
    { apply existsb_exists in Shared as [y [Member Equal]]. apply Nat.eqb_eq in Equal. now subst y. }
    unfold arguments. cbn [app_subst].
    assert (Lookup : forall rows, lookup_subst x (map (fun q => (fst q, ground d (snd q))) rows) =
      option_map (ground d) (lookup_subst x rows)).
    { induction rows as [|[y t] rest IH]; [reflexivity|]. cbn. now destruct (Nat.eqb x y). }
    rewrite Lookup. destruct (lookup_subst x th) as [found|] eqn:Read; [reflexivity|].
    exfalso. exact (Complete x Member Read). }
  change (map (fun source => (source, match lookup_subst source (A.link_input_table Cu Cg) with
    | Some t => app_subst (map (fun y => (y, base y)) (map fst (A.c_ty Cg))) t
    | None => base source end)) (map fst (A.c_ty Cg)) =
    map (fun source => (source, match lookup_subst source (A.c_ty Cg) with
      | Some t => app_subst (link_reading Cu Cg arguments dc) t
      | None => TAbs source end)) (map fst (A.c_ty Cg))).
  rewrite Base. apply map_ext_in. intros source Member. f_equal.
  destruct (lookup_subst source (A.c_ty Cg)) as [t|] eqn:Read.
  - rewrite (PublicLinkAnchors.anchored_input_table_read _ _ _ _ Read). symmetry.
    apply link_reading_compose. exact (Scope source t Read).
  - exfalso. exact (PublicSubstFacts.lookup_subst_in _ _ Member Read).
Qed.

Definition LinkedInputsRead (Cu Cg : A.cx) (caller body : ty -> ty) : Prop :=
  forall x tx, A.nlook x (A.c_ty Cg) = Some tx -> A.nlook x (A.c_ty Cu) <> None ->
    caller (TAbs x) = body tx.

Definition LinkedProvisionsRead (Cu Cg : A.cx) (nsg : list item)
    (target : item -> option item) (T G : subst) : Prop :=
  forall k p, In k nsg -> A.ilook (A.provision_id k) (A.c_provisions Cg) = Some p ->
    forall key, target k = Some key ->
    exists q, A.ilook (A.provision_id key) (A.c_provisions Cu) = Some q /\
      app_subst_item G (A.provision_id k) = app_subst_item T (A.provision_id key) /\
      A.provision_subst G p = A.provision_subst T q.

Record ClosureBinding (W : A.world) (u : nat) (Cu : A.cx) (nsu : list item)
    (Cg : A.cx) (nsg : list item) (target : item -> option item) (dc : environment) (gfx : subst) : Prop := {
  closure_bind_targets : forall k key, In k nsg -> target k = Some key -> In key nsu;
  closure_bind_inputs : forall T, ReadsMerge Cu (app_subst T) ->
    (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
    LinkedInputsRead Cu Cg (app_subst T) (app_subst (link_reading Cu Cg T dc));
  closure_bind_captures : forall T, ReadsMerge Cu (app_subst T) ->
    (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
    forall x, In x (map fst (A.c_ty Cg)) -> ~ In x (map fst (A.c_ty Cu)) ->
    output_meaning dc x <> None ->
    lookup_subst x (link_reading Cu Cg T dc) = Some (PublicBodySafety.cmeaning dc x);
  closure_bind_provisions : forall T, ReadsMerge Cu (app_subst T) ->
    (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
    LinkedProvisionsRead Cu Cg nsg target T (link_reading Cu Cg T dc)
}.

Lemma reads_empty_merge : forall C read, A.c_merge C = [] -> ReadsMerge C read.
Proof.
  intros C read Empty t u Equal. rewrite Empty in Equal.
  assert (Solves : PublicMerge.SolvesClasses [] []) by (split; intros c x y InClass; contradiction).
  pose proof (PublicMerge.merge_eq_sound Solves Equal) as Same.
  rewrite !PublicTypeRead.app_subst_nil in Same. now rewrite Same.
Qed.

Lemma reads_merge_canon_iff : forall C read, CxSection C ->
  ReadsMerge C read <-> forall t, read (A.canon C t) = read t.
Proof.
  intros C read Section. split.
  - intros Reads t. symmetry. apply Reads, PublicMerge.merge_eq_read.
    exact (proj1 Section).
  - intros Reads t u Equal.
    apply (PublicMerge.merge_section_kernel Section) in Equal.
    change (A.canon C t = A.canon C u) in Equal.
    rewrite <- (Reads t), <- (Reads u), Equal. reflexivity.
Qed.

Theorem entries_canonical : forall substitution classes meanings entries,
  A.MostGeneralMerge substitution entries ->
  canonical_context (A.entries_cx substitution classes meanings entries).
Proof. intros substitution classes meanings entries [_ [Idempotent _]]; exact Idempotent. Qed.

Lemma section_canonical : forall C, CxSection C -> canonical_context C.
Proof.
  intros C [Reads Respects] t. unfold A.canon. symmetry.
  apply Respects, PublicMerge.merge_eq_read. exact Reads.
Qed.

Theorem function_context_merge : forall W f C needs,
  A.CtxOf W f C needs -> A.Solved (A.c_merge C) /\ CxSection C.
Proof.
  intros W f C needs (d & es & tm & er & tmr & s & M & _ & _ & _ & _ & Formed & Section & -> & _).
  exact (conj (proj1 Formed) Section).
Qed.

Theorem functor_context_merge : forall W f C needs,
  A.FunCtx W f C needs -> A.Solved (A.c_merge C) /\ CxSection C.
Proof.
  intros W f C needs (d & ps & a & r & b & es & tm & tm' & s & M & implicit &
    _ & _ & _ & _ & _ & _ & Formed & Section & -> & _).
  exact (conj (proj1 Formed) Section).
Qed.

Theorem assumed_context_merge : forall W f C needs,
  A.AssumeCtx W f C needs -> A.Solved (A.c_merge C) /\ CxSection C.
Proof.
  intros W f C needs (d & es & tm & er & tmr & s & M & _ & _ & _ & _ & Formed & Section & -> & _).
  exact (conj (proj1 Formed) Section).
Qed.

Theorem function_context_canonical : forall W function C needs,
  A.CtxOf W function C needs -> canonical_context C.
Proof.
  intros W f C needs Context. apply section_canonical.
  exact (proj2 (function_context_merge W f C needs Context)).
Qed.

Theorem functor_context_canonical : forall W functor C needs,
  A.FunCtx W functor C needs -> canonical_context C.
Proof.
  intros W f C needs Context. apply section_canonical.
  exact (proj2 (functor_context_merge W f C needs Context)).
Qed.

Corollary context_canon_iff_mergeeq : forall C, CxSection C ->
  forall t u, A.canon C t = A.canon C u <-> A.MergeEq (A.c_merge C) t u.
Proof.
  intros C Section t u. symmetry. apply PublicMerge.merge_section_kernel. exact Section.
Qed.

(* Effective section rows are an executable test of the class judgment. *)
Theorem respects_section_rows_iff : forall C Cs th,
  CxSection C -> CxSection Cs ->
  (forall g t, In (g,t) (A.c_sub Cs) -> lookup_subst g (A.c_sub Cs) = Some t) ->
  A.Respects (A.c_merge Cs) th (A.c_merge C) <->
  (forall g t, In (g,t) (A.c_sub Cs) ->
    A.canon C (app_subst th (TAbs g)) = A.canon C (app_subst th t)).
Proof.
  intros C Cs th Caller Callee Effective. split.
  - intros Respect g t Row.
    apply (context_canon_iff_mergeeq C Caller).
    apply (@PublicMerge.merge_eq_substitute _ _ _ Respect).
    pose proof (proj1 Callee g) as Read. cbn [app_subst] in Read.
    rewrite (Effective g t Row) in Read. exact Read.
  - intros Rows. eapply PublicMerge.respects_section; [exact Callee|].
    intros g t Row. apply (context_canon_iff_mergeeq C Caller). exact (Rows g t Row).
Qed.

Corollary formed_callee_theta_rows_iff : forall W C Cs s ns th,
  CxSection C -> (A.CtxOf W s Cs ns \/ A.FunCtx W s Cs ns) ->
  A.CallTheta W C Cs s th <->
  map fst th = map fst (A.c_ty Cs) /\
  (forall r, In r (map fst (A.c_ty Cs)) -> A.nlook r (A.c_ty C) <> None ->
     lookup_subst r th = Some (A.canon C (app_subst (A.c_ty C) (TAbs r)))) /\
  (forall g t, In (g,t) (A.c_sub Cs) ->
    A.canon C (app_subst th (TAbs g)) = A.canon C (app_subst th t)).
Proof.
  intros W C Cs s ns th Caller Formed.
  assert (Callee : CxSection Cs /\
    (forall g t, In (g,t) (A.c_sub Cs) -> lookup_subst g (A.c_sub Cs) = Some t)).
  { destruct Formed as [Ctx|Fun].
    - destruct Ctx as (d & es & tm & er & tmr & sigma & M &
        _ & _ & _ & [_ [_ [_ [_ Effective]]]] & _ & Section & -> & _).
      exact (conj Section Effective).
    - destruct Fun as (d & ps & a & r & b & es & tm & tm' & sigma & M & implicit &
        _ & _ & _ & _ & _ & [_ [_ [_ [_ Effective]]]] & _ & Section & -> & _).
      exact (conj Section Effective). }
  destruct Callee as [Section Effective]. unfold A.CallTheta.
  now rewrite (respects_section_rows_iff C Cs th Caller Section Effective).
Qed.

Lemma section_same_fields : forall C C',
  A.c_sub C' = A.c_sub C -> A.c_merge C' = A.c_merge C ->
  CxSection C -> CxSection C'.
Proof. intros C C' Sub Merge Section. unfold CxSection in *. now rewrite Sub, Merge. Qed.

Lemma capps_ilook : forall C k, A.ilook k (A.c_apps C) = option_map A.provision_inst (A.ilook k (A.c_provisions C)).
Proof.
  intros [ty sub classes provs locs] k. unfold A.c_apps. cbn [A.c_provisions].
  induction provs as [|[k' p] r IH]; [reflexivity|]. cbn. destruct (item_eqb k k'); [|exact IH].
  destruct p; reflexivity.
Qed.

Lemma provide_merge : forall C k a C',
  A.c_merge (A.provide C k a C') = A.c_merge C'.
Proof.
  intros C k a C'. unfold A.provide.
  destruct (A.ilook k (A.c_provisions C')) as [[]|]; reflexivity.
Qed.

Lemma provided_result_merge : forall W u C out C' ks,
  A.ProvideResult W u C out C' ks -> A.c_merge C' = A.c_merge C.
Proof.
  intros W u C out C' ks (d & ps & a & r & b & outs & es & tm & _ & _ & _ & _ & _ & ->).
  reflexivity.
Qed.

Lemma family_context_merge : forall matching C,
  A.c_merge (A.family_context C matching) = A.c_merge C.
Proof.
  induction matching as [|[x t] matching IH]; intros C; [reflexivity |].
  change (A.c_merge (A.family_context (A.set_ty C x t) matching) = A.c_merge C).
  rewrite IH. reflexivity.
Qed.

Lemma family_application_merge : forall W C u matching th sm Cr ks,
  AuditFacts.FamilyApplication W C u matching th sm Cr ks -> A.c_merge Cr = A.c_merge C.
Proof.
  intros W C u matching th sm Cr ks
    (formal & needs & (fixed & _ & _ & _ & _ & _ & _ & _ & _ & Result & _) & _).
  transitivity (A.c_merge (A.family_context C matching)).
  - eapply provided_result_merge. exact Result.
  - apply family_context_merge.
Qed.

Theorem type_binding_preserves_canonical : forall C source meaning,
  canonical_context C -> canonical_context (A.set_ty C source meaning).
Proof. intros C source meaning Canonical; exact Canonical. Qed.

Theorem output_context_preserves_canonical : forall C meanings entries,
  canonical_context C -> canonical_context (A.output_cx C meanings entries).
Proof. intros C meanings entries Canonical; exact Canonical. Qed.

Theorem formed_type_binding_preserves_alias_context : forall C d source tau,
  canonical_context C -> alias_context_agrees C d ->
  alias_context_agrees (A.set_ty C source (A.canon C tau))
    (bind_type d source (A.canon C tau)).
Proof. intros; apply type_bind_preserves_alias_context; auto. Qed.

(* The world fixes each symbol's context, so every rule reading it agrees. *)
Lemma function_context_record : forall W s C ns,
  A.CtxOf W s C ns -> A.w_context W s = Some (C, ns).
Proof.
  intros W s C ns (d & es & tm & er & tmr & σ & M & _ & _ & _ & _ & _ & _ & _ & _ & Recorded).
  exact Recorded.
Qed.

Lemma functor_context_record : forall W u C ns,
  A.FunCtx W u C ns -> A.w_context W u = Some (C, ns).
Proof.
  intros W u C ns
    (d & ps & a & r & b & es & tm & tm' & s & M & implicit & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & Recorded).
  exact Recorded.
Qed.

Theorem function_context_unique : forall W s C ns C' ns',
  A.CtxOf W s C ns -> A.CtxOf W s C' ns' -> C = C' /\ ns = ns'.
Proof.
  intros W s C ns C' ns' Context Context'.
  pose proof (function_context_record W s C ns Context) as Recorded.
  pose proof (function_context_record W s C' ns' Context') as Recorded'.
  rewrite Recorded in Recorded'. injection Recorded' as <- <-. split; reflexivity.
Qed.

Theorem functor_context_unique : forall W u C ns C' ns',
  A.FunCtx W u C ns -> A.FunCtx W u C' ns' -> C = C' /\ ns = ns'.
Proof.
  intros W u C ns C' ns' Context Context'.
  pose proof (functor_context_record W u C ns Context) as Recorded.
  pose proof (functor_context_record W u C' ns' Context') as Recorded'.
  rewrite Recorded in Recorded'. injection Recorded' as <- <-. split; reflexivity.
Qed.

(* A call reads members of a callee class as one caller class. *)
Theorem call_theta_merged : forall W C Cs s th g1 g2,
  CxSection C -> A.CallTheta W C Cs s th ->
  A.MergeEq (A.c_merge Cs) (TAbs g1) (TAbs g2) ->
  In g1 (map fst (A.c_ty Cs)) -> In g2 (map fst (A.c_ty Cs)) ->
  A.nlook g1 (A.c_ty C) <> None -> A.nlook g2 (A.c_ty C) <> None ->
  A.MergeEq (A.c_merge C)
    (app_subst (A.c_ty C) (TAbs g1)) (app_subst (A.c_ty C) (TAbs g2)).
Proof.
  intros W C Cs s th g1 g2 Caller [_ [Bound Respects]] Equal R1 R2 F1 F2.
  pose proof (@PublicMerge.merge_eq_substitute _ _ _ Respects _ _ Equal) as Read.
  cbn [app_subst] in Read. rewrite (Bound g1 R1 F1), (Bound g2 R2 F2) in Read.
  apply (context_canon_iff_mergeeq C Caller).
  apply (context_canon_iff_mergeeq C Caller) in Read.
  now rewrite !(section_canonical C Caller) in Read.
Qed.
