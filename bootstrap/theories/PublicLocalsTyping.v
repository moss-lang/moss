(* Ground local stores: lookup, lexical scope and assignment. Source-context
   grounding and callable/provision typing remain separate obligations. *)
From Stdlib Require Import List Arith.
From NM Require AuditBase.
From NM Require Import Audit PublicValueTyping.
Import AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Section LocalStores.
Variables (W : A.world) (P : program_view).

Definition locals_typed (H : heap_types) (values : locals) (types : list (var * ty)) : Prop :=
  Forall2 (fun value typed => fst value = fst typed /\
    value_typed W P H (snd value) (snd typed)) values types.

Theorem locals_lookup : forall H values types x tau,
  locals_typed H values types -> A.nlook x types = Some tau ->
  exists value, lookup_local x values = Some value /\ value_typed W P H value tau.
Proof.
  intros H values types x tau Typed; revert x tau.
  induction Typed as [|[name value] [symbol meaning] values types [Equal Head] Tail IH];
    intros x tau Found.
  - discriminate.
  - cbn in Equal; subst name. cbn [A.nlook lookup_local] in *.
    destruct (Nat.eqb x symbol); [injection Found as <-; eauto|apply IH; exact Found].
Qed.

Theorem locals_extends : forall H H' values types,
  heap_extends H H' -> locals_typed H values types -> locals_typed H' values types.
Proof.
  intros H H' values types Extend Typed; induction Typed; constructor; auto.
  destruct H0 as [Name Value]. split; [exact Name|eapply value_typed_extends; eassumption].
Qed.

Theorem locals_push : forall H values types name value tau,
  locals_typed H values types -> value_typed W P H value tau ->
  locals_typed H ((name,value)::values) ((name,tau)::types).
Proof. intros; constructor; [split; auto|assumption]. Qed.

Theorem locals_pop : forall H values types,
  locals_typed H values types -> locals_typed H (tl values) (tl types).
Proof. intros H values types Typed; inversion Typed; subst; cbn; constructor || assumption. Qed.

Theorem locals_popn : forall n H values types,
  locals_typed H values types -> locals_typed H (skipn n values) (skipn n types).
Proof.
  induction n; intros H values types Typed; [exact Typed|].
  inversion Typed; subst; cbn; [constructor|apply IHn; assumption].
Qed.

Theorem locals_assign : forall H values types x tau value,
  locals_typed H values types -> A.nlook x types = Some tau ->
  value_typed W P H value tau ->
  exists updated, assign_local x value values = Some updated /\ locals_typed H updated types.
Proof.
  intros H values types x tau value Typed; revert x tau value.
  induction Typed as [|[name current] [symbol meaning] values types [Equal Head] Tail IH];
    intros x tau value Found New; [discriminate|].
  cbn in Equal; subst name.
  cbn [A.nlook assign_local] in *.
  destruct (Nat.eqb x symbol).
  - injection Found as <-. eexists; split; [reflexivity|]. constructor; [split; auto|exact Tail].
  - destruct (IH x tau value Found New) as [updated [Run Typed]]. rewrite Run.
    eexists; split; [reflexivity|]. constructor; [split; auto|exact Typed].
Qed.

End LocalStores.
