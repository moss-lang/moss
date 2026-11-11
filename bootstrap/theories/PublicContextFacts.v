(* Canonicalization facts supplied by actual public context formation. *)
From Stdlib Require Import List.
From NM Require AuditFacts.
From NM Require Import Audit PublicAliasContext.
From NM Require PublicMerge.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition canonical_context (C : A.cx) : Prop :=
  forall tau, A.canon C (A.canon C tau) = A.canon C tau.

Definition CxSection (C : A.cx) : Prop := A.SectionOf (A.c_merge C) (A.c_sub C).


(* A reading gives each merge class one meaning. *)
Definition ReadsMerge (C : A.cx) (read : ty -> ty) : Prop :=
  forall t u, A.MergeEq (A.c_merge C) t u -> read t = read u.

(* Shared inputs keep their application meanings through a linked body. *)
Definition LinkedInputsRead (Cu Cg : A.cx) (read : ty -> ty) : Prop :=
  forall x tx, A.nlook x (A.c_ty Cg) = Some tx -> A.nlook x (A.c_ty Cu) <> None ->
    (forall y, In y (A.gens tx) -> A.nlook y (A.c_ty Cu) <> None) /\
    read (TAbs x) = read tx.

Definition ClosureBinding W u Cu nsu Cg nsg gfx : Prop :=
  A.LinkBinding W u Cu nsu Cg nsg /\
  forall T, ReadsMerge Cu (app_subst T) ->
    (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
    LinkedInputsRead Cu Cg (app_subst T).

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
