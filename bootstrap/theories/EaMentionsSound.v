(* Executable dependency readers imply Audit's declarative membership rules. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaTypesSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma insert_sorted_members : forall x y xs,
  In y (insert_sorted x xs) <-> y = x \/ In y xs.
Proof.
  intros x y xs. induction xs as [|z xs IH].
  - change (x = y \/ False <-> y = x \/ False). intuition congruence.
  - change (In y (if Nat.ltb x z then x :: z :: xs
      else if Nat.eqb x z then z :: xs else z :: insert_sorted x xs)
      <-> y = x \/ z = y \/ In y xs).
    destruct (Nat.ltb x z) eqn:Before.
    + change (x = y \/ z = y \/ In y xs <-> y = x \/ z = y \/ In y xs).
      intuition congruence.
    + destruct (Nat.eqb x z) eqn:Equal.
      * apply Nat.eqb_eq in Equal. subst.
        change (z = y \/ In y xs <-> y = z \/ z = y \/ In y xs).
        intuition congruence.
      * change (z = y \/ In y (insert_sorted x xs) <-> y = x \/ z = y \/ In y xs).
        rewrite IH. intuition congruence.

Qed.

Lemma sort_uniq_members : forall y xs, In y (sort_uniq xs) <-> In y xs.
Proof.
  intros y xs. unfold sort_uniq. induction xs as [|x xs IH]; cbn; [reflexivity|].
  rewrite insert_sorted_members, IH. intuition congruence.
Qed.

Section DependencyReaders.
Variable W : world.

Lemma bracket_bound_members : forall p application x,
  In x (bracket_bound W p application) <->
  exists b, In b (bracket_list application) /\ resolve_sym W p (fst b) = Some x.
Proof.
  intros p application x. unfold bracket_bound. rewrite in_flat_map. split.
  - intros [b [Present Bound]]. exists b. split; [exact Present|].
    destruct (resolve_sym W p (fst b)) as [symbol|] eqn:Resolve; [|contradiction].
    destruct Bound as [<-|[]]. reflexivity.
  - intros [b [Present Resolve]]. exists b. split; [exact Present|].
    rewrite Resolve. left. reflexivity.
Qed.

Theorem free_in_sound : forall fuel R p syntax x,
  In x (free_in W fuel R p syntax) -> FreeIn W R p syntax x.
Proof.
  induction fuel as [|fuel IH]; intros R p syntax x Present; [contradiction|].
  destruct syntax as [| | |fields|alternatives|pa application]; cbn [free_in] in Present;
    try contradiction.
  - apply in_flat_map in Present as [[label field] [Member Present]].
    eapply FRec; [exact Member|exact (IH R p field x Present)].
  - apply in_flat_map in Present as [member [Member Present]].
    eapply FUnion; [exact Member|exact (IH R p member x Present)].
  - destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|contradiction].
    apply in_app_or in Present as [Own|Bracket].
    + apply filter_In in Own as [Member Unbound].
      apply negb_true_iff in Unbound.
      assert (NotBound : ~ exists b, In b (bracket_list application) /\
        resolve_sym W p (fst b) = Some x).
      { intro Bound. apply bracket_bound_members in Bound. apply nmem_spec in Bound.
        rewrite Bound in Unbound. discriminate. }
      destruct (kind_sym W symbol) as [kind|] eqn:Kind.
      * destruct kind; try (eapply FNom; [exact Resolve|congruence|exact Member|exact NotBound]).
        destruct Member as [<-|[]]. eapply FType; eassumption.
      * eapply FNom; [exact Resolve|congruence|exact Member|exact NotBound].
    + apply in_flat_map in Bracket as [[bp [head detached bindings]] [Member Present]].
      destruct head as [sp|]; [|contradiction].
      eapply FBracket; [congruence|exact Member|exact (IH R p (ATRef sp bindings) x Present)].
Qed.

Theorem sig_mentions_sound : forall fuel symbol x,
  In x (sig_mentions W fuel symbol) -> SigMentions W symbol x.
Proof.
  intros fuel symbol x Present. unfold sig_mentions in Present.
  destruct (w_decl W symbol) as [d|] eqn:Declaration; [|contradiction].
  destruct (d_ast d) as [items declarations|name|name body|name parameters payload|name|name value_type initial|
    receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
    try contradiction.
  rewrite sort_uniq_members in Present.
  exists d, receiver, name, parameters, result, body. split; [exact Declaration|].
  split; [exact Ast|]. apply in_app_or in Present as [InParams|Result].
  - left. apply in_flat_map in InParams as [pt [Member Present]].
    exists pt. split; [exact Member|exact (free_in_sound fuel _ _ _ _ Present)].
  - right. destruct result as [syntax|]; [|contradiction].
    exists syntax. split; [reflexivity|exact (free_in_sound fuel _ _ _ _ Result)].
Qed.

Theorem spec_types_go_sound : forall fuel stack p syntax x,
  In x (spec_types_go W fuel stack p syntax) -> SpecTypes W p syntax x.
Proof.
  induction fuel as [|fuel IH]; intros stack p [head detached application] x Present;
    [contradiction|].
  destruct head as [pa|], detached as [dn|]; cbn [spec_types_go] in Present;
    try contradiction.
  destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|contradiction].
  destruct (kind_sym W symbol) as [kind|] eqn:Kind; [destruct kind|].
  all: try solve [destruct Present as [<-|[]]; eapply STType; eassumption].
  all: destruct (nmem symbol stack); [contradiction|].
  all: destruct (w_decl W symbol) as [d|] eqn:Declaration; [|contradiction].
  all: destruct (d_ast d) as [items declarations|name|name body|name parameters payload|name|name value_type initial|
    receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
    try contradiction.
  all: apply in_flat_map in Present as [sp [Member Present]];
    eapply STCtx; [exact Resolve|exact Declaration|exact Ast|exact Member|];
    exact (IH _ _ _ _ Present).

Qed.

Corollary spec_types_sound : forall fuel p syntax x,
  In x (spec_types W fuel p syntax) -> SpecTypes W p syntax x.
Proof. intros fuel p syntax x. exact (spec_types_go_sound fuel [] p syntax x). Qed.

Theorem fun_outputs_sound : forall fuel symbol x,
  In x (fun_outputs W fuel symbol) -> FunOutputs W symbol x.
Proof.
  intros fuel symbol x Present. unfold fun_outputs, functor_decl in Present.
  destruct (w_decl W symbol) as [d|] eqn:Declaration; [|contradiction].
  destruct (functor_parts (d_ast d)) as [[[[parameters arguments] results] body]|] eqn:Parts;
    [|contradiction].
  rewrite sort_uniq_members in Present. apply in_flat_map in Present as [sp [Member Present]].
  exists d, parameters, arguments, results, body, sp.
  split; [exact Declaration|]. split; [exact Parts|].
  split; [exact Member|exact (spec_types_sound fuel _ _ _ Present)].
Qed.

End DependencyReaders.
