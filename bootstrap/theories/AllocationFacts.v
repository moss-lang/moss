(* Laws for the explicit allocation scope used by abstract applications. *)
From Stdlib Require Import List Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit.
From NM Require Import LegacyBodyJudgments.
From NM Require PublicTypeRead.
Import ListNotations AuditBase.Semantics AuditBase.Semantics.Elaboration.

Lemma allocation_member : forall A (before after : allocation_scope A) batch g,
  Allocates before batch after -> In g batch ->
  scope_permits before g /\
  ~ In g (scope_allocated before ++ scope_support before) /\
  In g (scope_allocated after).
Proof.
  intros A before after batch g [[_ Fresh] ->] Member.
  rewrite Forall_forall in Fresh. destruct (Fresh g Member) as [Permitted Unused].
  repeat split; try assumption. apply in_or_app. right. exact Member.
Qed.

Theorem allocation_grows : forall A (before after : allocation_scope A) batch,
  Allocates before batch after ->
  incl (scope_allocated before) (scope_allocated after).
Proof.
  intros A before after batch [_ ->] g Member.
  apply in_or_app. left. exact Member.
Qed.

(* Live support can change between steps, including when a binding is
   shadowed. The carried history still reserves each earlier allocation. *)
Theorem allocations_disjoint : forall A (before after later : allocation_scope A) first second,
  Allocates before first after ->
  incl (scope_allocated after) (scope_allocated later) ->
  AllocationFresh later second ->
  forall g, In g first -> ~ In g second.
Proof.
  intros A before after later first second Allocation Grows [_ Fresh] g InFirst InSecond.
  rewrite Forall_forall in Fresh. apply (proj2 (Fresh g InSecond)).
  apply in_or_app. left. apply Grows.
  exact (proj2 (proj2 (allocation_member A before after first g Allocation InFirst))).
Qed.

Definition rename_allocation_scope {A B} (rename : A -> B) (permits : B -> Prop)
    (scope : allocation_scope A) : allocation_scope B :=
  AllocationScope permits (map rename (scope_allocated scope)) (map rename (scope_support scope)).

Lemma allocation_rename_nodup : forall A B (rename : A -> B),
  (forall x y, rename x = rename y -> x = y) ->
  forall xs, NoDup xs -> NoDup (map rename xs).
Proof.
  intros A B rename Injective xs Distinct. induction Distinct as [|x xs Absent Distinct IH].
  - constructor.
  - constructor; [|exact IH]. intros Present.
    apply in_map_iff in Present as [y [Equal Member]].
    apply Injective in Equal. subst y. exact (Absent Member).
Qed.

Theorem allocation_rename : forall A B (rename : A -> B) permits (before after : allocation_scope A) batch,
  (forall x y, rename x = rename y -> x = y) ->
  (forall x, scope_permits before x -> permits (rename x)) ->
  Allocates before batch after ->
  Allocates (rename_allocation_scope rename permits before) (map rename batch)
    (rename_allocation_scope rename permits after).
Proof.
  intros A B rename permits before after batch Injective Permitted [[Distinct Fresh] ->].
  split.
  - split.
    + apply allocation_rename_nodup; assumption.
    + rewrite Forall_forall in Fresh |- *.
      intros y Member. apply in_map_iff in Member as [x [<- Present]].
      destruct (Fresh x Present) as [Allowed Absent]. split; [exact (Permitted x Allowed)|].
      cbn. rewrite <- map_app. intros Collision.
      apply in_map_iff in Collision as [z [Equal Old]].
      apply Injective in Equal. subst z. exact (Absent Old).
  - unfold rename_allocation_scope, extend_allocation_scope. cbn. rewrite map_app. reflexivity.
Qed.

Theorem allocate_range : forall (before : allocation_scope nat) start count,
  (forall g, start <= g -> scope_permits before g) ->
  (forall g, In g (scope_allocated before ++ scope_support before) -> g < start) ->
  Allocates before (seq start count) (extend_allocation_scope before (seq start count)).
Proof.
  intros before start count Permitted Below. split; [|reflexivity].
  split; [apply seq_NoDup|]. rewrite Forall_forall. intros g Member.
  apply in_seq in Member. split; [apply Permitted; lia|].
  intros Old. specialize (Below g Old). lia.
Qed.

Theorem frame_allocation_transition : forall W F C batch,
  FreshFor W F C batch <->
  Allocates (body_allocation_scope W F C) batch (body_allocation_scope W (with_fresh F batch) C).
Proof.
  intros W F C batch. split; [intro Fresh; split; [exact Fresh|reflexivity]|intros [Fresh _]; exact Fresh].
Qed.

(* BApplyAbstract uses this outgoing frame. A subsequent application may
   read a different context and still cannot allocate one of these names. *)
Theorem fresh_outputs_disjoint : forall W F C C' first second,
  FreshFor W F C first ->
  FreshFor W (frame_after_allocation F (extend_allocation_scope (body_allocation_scope W F C) first)) C' second ->
  forall g, In g first -> ~ In g second.
Proof.
  intros W F C C' first second Initial Next.
  apply (allocations_disjoint nat (body_allocation_scope W F C)
    (body_allocation_scope W (with_fresh F first) C)
    (body_allocation_scope W (with_fresh F first) C') first second).
  - apply frame_allocation_transition. exact Initial.
  - apply incl_refl.
  - exact Next.
Qed.

Theorem frame_allocation_compatible : forall W F C batch,
  frame_after_allocation F (extend_allocation_scope (body_allocation_scope W F C) batch) =
  with_fresh F batch.
Proof. reflexivity. Qed.

(* CopyInterpretation allocates its hypothetical output and input readings
   in one batch. The site’s fixed meanings provide the reserved support. *)
Theorem copy_allocation_contract : forall W sigma (outs : list nat) (gen : nat -> nat)
  (ys : list nat) (cg : nat -> nat),
  AllocationFresh (copy_allocation_scope W sigma) (map gen outs ++ map cg ys) <->
  NoDup (map gen outs ++ map cg ys) /\
  (forall z, In z (flat_map (fun q => gens (snd q)) sigma) ->
     ~ In z (map gen outs ++ map cg ys)) /\
  (forall t, In t outs -> Fresh W (gen t)) /\
  (forall y, In y ys -> Fresh W (cg y)).
Proof.
  intros W sigma outs gen ys cg. unfold AllocationFresh, copy_allocation_scope.
  cbn. rewrite Forall_forall. split.
  - intros [Distinct Fresh]. repeat split; try assumption.
    + intros z Reserved Allocated. exact (proj2 (Fresh z Allocated) Reserved).
    + intros t Output.
      assert (Member : In (gen t) (map gen outs ++ map cg ys)) by
        (apply in_or_app; left; apply in_map; exact Output).
      exact (proj1 (Fresh _ Member)).
    + intros y Input.
      assert (Member : In (cg y) (map gen outs ++ map cg ys)) by
        (apply in_or_app; right; apply in_map; exact Input).
      exact (proj1 (Fresh _ Member)).
  - intros [Distinct [Reserved [Outputs Inputs]]]. split; [exact Distinct|].
    intros g Member. split.
    + apply in_app_or in Member as [Output|Input].
      * apply in_map_iff in Output as [t [<- Present]]. apply Outputs. exact Present.
      * apply in_map_iff in Input as [y [<- Present]]. apply Inputs. exact Present.
    + intros Present. exact (Reserved g Present Member).
Qed.

Lemma value_provision_keys_spec : forall entries v,
  In v (value_provision_keys entries) <-> ilook (IVal v) entries <> None.
Proof.
  intros entries v. induction entries as [|[key body] entries IH].
  - cbn [value_provision_keys flat_map ilook]. intuition.
  - destruct key as [t|s|f|r m a|u]; cbn [value_provision_keys flat_map fst app ilook item_eqb]; try exact IH.
    destruct (Nat.eqb_spec v s) as [->|Different].
    + split; [intros _; discriminate|intros _; left; reflexivity].
    + rewrite <- IH. cbn [In]. intuition congruence.
Qed.

Theorem capture_allocation_contract : forall W C batch,
  AllocationFresh (capture_allocation_scope W C) batch <->
  NoDup batch /\ Forall (Fresh W) batch /\
  Forall (fun v => ilook (IVal v) (c_provisions C) = None) batch.
Proof.
  intros W C batch. unfold AllocationFresh, capture_allocation_scope. cbn.
  rewrite !Forall_forall. split.
  - intros [Distinct Checked]. split; [exact Distinct|]. split.
    + intros v InBatch. exact (proj1 (Checked v InBatch)).
    + intros v InBatch. destruct (ilook (IVal v) (c_provisions C)) eqn:Read; [exfalso|reflexivity].
      apply (proj2 (Checked v InBatch)), value_provision_keys_spec. congruence.
  - intros (Distinct & Undeclared & Unprovided). split; [exact Distinct|].
    intros v InBatch. split; [exact (Undeclared v InBatch)|].
    intros Present. apply value_provision_keys_spec in Present.
    exact (Present (Unprovided v InBatch)).
Qed.

Theorem link_output_allocation_contract : forall W (outs : list nat) (gen : nat -> nat),
  AllocationFresh (link_output_scope W) (map gen outs) <->
  NoDup (map gen outs) /\ (forall t, In t outs -> Fresh W (gen t)).
Proof.
  intros W outs gen. unfold AllocationFresh, link_output_scope.
  cbn. rewrite Forall_forall. split.
  - intros [Distinct Permitted]. split; [exact Distinct|].
    intros t Member. exact (proj1 (Permitted (gen t) (in_map gen outs t Member))).
  - intros [Distinct Permitted]. split; [exact Distinct|].
    intros g Member. apply in_map_iff in Member as [t [<- Member]].
    split; [apply Permitted; exact Member|tauto].
Qed.

Scheme bind_items_scope_ind := Induction for BindItems Sort Prop
  with bind_item_scope_ind := Induction for BindItem Sort Prop.
Combined Scheme bind_scope_ind from bind_items_scope_ind, bind_item_scope_ind.

Theorem bind_allocation_history : forall W P,
  (forall F C items wrap C' F' made,
    BindItems W P F C items wrap C' F' made ->
    incl (fr_fresh F) (fr_fresh F')) /\
  (forall F C item wrap C' F' made,
    BindItem W P F C item wrap C' F' made ->
    incl (fr_fresh F) (fr_fresh F')).
Proof.
  intros W P. apply bind_scope_ind; intros; cbn [carry_frame_allocations frame_after_allocation
    extend_allocation_scope body_allocation_scope fr_fresh scope_allocated] in *;
    try apply incl_refl.
  - eapply incl_tran; eassumption.
  - assumption.
  - intros g Earlier. apply in_or_app. left. exact Earlier.
Qed.

Lemma generated_output_in_batch : forall choices role generation,
  In (role, GeneratedOutput generation) choices ->
  In generation (allocated_output_generations choices).
Proof.
  intros choices role generation Member.
  unfold allocated_output_generations. apply in_flat_map.
  exists (role, GeneratedOutput generation). split; [exact Member|now left].
Qed.

Lemma generated_output_allocated : forall choices role generation,
  output_choice_at choices role = GeneratedOutput generation ->
  In generation (allocated_output_generations choices).
Proof.
  intros choices role generation Choice. unfold output_choice_at in Choice.
  destruct (nlook role choices) as [chosen|] eqn:Found; [|discriminate].
  subst chosen. apply PublicTypeRead.nlook_in in Found.
  eapply generated_output_in_batch; exact Found.
Qed.

(* The encoded identities satisfy the former output conditions used by
   the runtime safety argument. *)
Theorem output_identity_conditions : forall W F C a,
  OutputIdentityAt W F C a ->
  forall t, FunOutputs W (app_functor a) t ->
    (nlook t (c_ty C) = None /\ app_identity a t = t /\
      ~ In t ((match fr_ret F with Some r => gens r | None => [] end) ++
              (match fr_this F with Some r => gens r | None => [] end)) /\
      ~ In t (context_gens C ++ key_receiver_gens C)) \/
    ((nlook t (c_ty C) <> None \/ In t (context_gens C ++ key_receiver_gens C)) /\
      In (app_identity a t) (app_allocated a)).
Proof.
  intros W F C a Typed t Output. unfold OutputIdentityAt in Typed. specialize (Typed t Output).
  unfold app_identity, output_generation_at.
  destruct (output_choice_at (app_output_choices a) t) as [|generation] eqn:Choice.
  - left. cbn [encode_output_identity]. destruct Typed as (Absent & Protected & Unmentioned).
    repeat split; assumption.
  - right. cbn [encode_output_identity]. split; [exact Typed|].
    unfold app_allocated. eapply generated_output_allocated; exact Choice.
Qed.

(* Returning through nested concrete applications preserves the identities
   already allocated by an abstract application. *)
Corollary generated_outputs_stay_reserved : forall W P F C a C1 items wrap C2 F2 made t generation,
  output_choice_at (app_output_choices a) t = GeneratedOutput generation ->
  BindItems W P
    (frame_after_allocation F (extend_allocation_scope (body_allocation_scope W F C) (app_allocated a)))
    C1 items wrap C2 F2 made ->
  forall batch, FreshFor W F2 C2 batch -> ~ In generation batch.
Proof.
  intros W P F C a C1 items wrap C2 F2 made t generation Choice Binds batch [_ Fresh] Reused.
  apply generated_output_allocated in Choice.
  apply (proj1 (bind_allocation_history W P)) in Binds.
  rewrite Forall_forall in Fresh. apply (proj2 (Fresh generation Reused)).
  apply in_or_app. left. apply Binds. cbn [frame_after_allocation extend_allocation_scope
    body_allocation_scope fr_fresh scope_allocated].
  apply in_or_app. right. exact Choice.
Qed.
