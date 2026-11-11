(* Fuel adequacy for context-dependent type-symbol reads. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaMentionsSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Section ContextPaths.
Variable W : world.

Inductive TypePath : string -> spec -> nat -> list nat -> Prop :=
| PathType : forall p pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    TypePath p (Spec (Some pa) None a) t []
| PathContext : forall p pa a c d n its sp t steps,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    In sp (flat_map citem_specs its) -> TypePath (d_mod d) sp t steps -> ~ In c steps ->
    TypePath p (Spec (Some pa) None a) t (c :: steps).

Lemma path_context : forall p sp t steps, TypePath p sp t steps -> forall c,
  In c steps -> exists d n its child tail,
    w_decl W c = Some d /\ d_ast d = ADCtx n its /\
    In child (flat_map citem_specs its) /\ TypePath (d_mod d) child t tail /\ ~ In c tail.
Proof.
  intros p sp t steps Path. induction Path; intros symbol Member; [contradiction|].
  destruct Member as [<-|Member].
  - exists d, n, its, sp, steps. repeat split; assumption.
  - exact (IHPath symbol Member).
Qed.

Lemma path_nodup : forall p sp t steps, TypePath p sp t steps -> NoDup steps.
Proof. intros p sp t steps Path. induction Path; constructor; assumption. Qed.

(* A repeated context can be skipped by taking its later suffix. *)
Theorem spec_types_path : forall p sp t, SpecTypes W p sp t -> exists steps, TypePath p sp t steps.
Proof.
  intros p sp t Read. induction Read.
  - exists []. apply PathType; assumption.
  - destruct IHRead as [steps Path]. destruct (in_dec Nat.eq_dec c steps) as [Repeated|Fresh].
    + destruct (path_context _ _ _ _ Path c Repeated) as
        (dc & name & items & child & tail & Declaration & Ast & Member & Tail & NoRepeat).
      rewrite H0 in Declaration. injection Declaration as <-.
      rewrite H1 in Ast. injection Ast as <- <-.
      exists (c :: tail). eapply PathContext; eassumption.
    + exists (c :: steps). eapply PathContext; eassumption.
Qed.

Lemma context_kind : forall c d n its,
  w_decl W c = Some d -> d_ast d = ADCtx n its -> kind_sym W c = Some DKCtx.
Proof.
  intros c d n its Declaration Ast. unfold kind_sym. rewrite Declaration.
  cbn. rewrite Ast. reflexivity.
Qed.

Theorem path_read : forall p sp t steps, TypePath p sp t steps -> forall stack,
  (forall c, In c steps -> ~ In c stack) ->
  In t (spec_types_go W (S (List.length steps)) stack p sp).
Proof.
  intros p sp t steps Path. induction Path; intros stack Apart.
  - cbn [spec_types_go]. rewrite H, H0. left. reflexivity.
  - cbn [spec_types_go List.length]. rewrite H, (context_kind _ _ _ _ H0 H1).
    assert (NotSeen : nmem c stack = false).
    { destruct (nmem c stack) eqn:Seen; [|reflexivity].
      exfalso. apply (Apart c (or_introl eq_refl)), nmem_spec. exact Seen. }
    rewrite NotSeen, H0, H1. apply in_flat_map. exists sp. split; [exact H2|].
    apply IHPath. intros symbol Member [Equal|Seen].
    + subst. contradiction.
    + apply (Apart symbol (or_intror Member)), Seen.
Qed.

Theorem spec_types_eventually_complete : forall p sp t,
  SpecTypes W p sp t -> exists fuel, In t (spec_types W fuel p sp).
Proof.
  intros p sp t Read. destruct (spec_types_path _ _ _ Read) as [steps Path].
  exists (S (List.length steps)). unfold spec_types. apply (path_read _ _ _ _ Path []).
  intros c Member Empty. exact Empty.
Qed.

End ContextPaths.

(* A finite declaration bound gives one fuel bound for every derivation. *)
Section ContextBounds.
Variable W : world.
Variable bound : nat.
Hypothesis declarations_bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.

Lemma path_length_bound : forall p sp t steps,
  TypePath W p sp t steps -> List.length steps <= bound.
Proof.
  intros p sp t steps Path.
  replace bound with (List.length (seq 0 bound)) by apply length_seq.
  apply NoDup_incl_length; [exact (path_nodup W _ _ _ _ Path)|].
  intros c Member. apply in_seq. split; [lia|].
  destruct (path_context W _ _ _ _ Path c Member) as
    (d & n & its & child & tail & Declaration & Rest).
  exact (declarations_bounded c d Declaration).
Qed.

Lemma spec_types_go_monotone : forall fuel more,
  fuel <= more -> forall stack p syntax x,
  In x (spec_types_go W fuel stack p syntax) -> In x (spec_types_go W more stack p syntax).
Proof.
  induction fuel as [|fuel IH]; intros more Le stack p [head detached application] x Present;
    [contradiction|].
  destruct more as [|more]; [lia|]. assert (Le' : fuel <= more) by lia.
  destruct head as [pa|], detached as [dn|]; cbn [spec_types_go] in *;
    try contradiction.
  destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|contradiction].
  destruct (kind_sym W symbol) as [kind|] eqn:Kind; [destruct kind|].
  all: try exact Present.
  all: destruct (nmem symbol stack); [contradiction|].
  all: destruct (w_decl W symbol) as [d|] eqn:Declaration; [|contradiction].
  all: destruct (d_ast d) as [items declarations|name|name body|name parameters payload|name|name value_type initial|
    receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
    try contradiction.
  all: apply in_flat_map in Present as [sp [Member Present]];
    apply in_flat_map; exists sp; split; [exact Member|exact (IH more Le' _ _ _ _ Present)].
Qed.

Theorem spec_types_complete_above : forall fuel p syntax x,
  bound < fuel -> SpecTypes W p syntax x -> In x (spec_types W fuel p syntax).
Proof.
  intros fuel p syntax x Enough Read.
  destruct (spec_types_path W _ _ _ Read) as [steps Path].
  pose proof (path_length_bound _ _ _ _ Path) as Length.
  unfold spec_types. eapply spec_types_go_monotone; [|apply (path_read W _ _ _ _ Path [])].
  - lia.
  - intros c Member Empty. exact Empty.
Qed.

Theorem spec_types_exact : forall fuel p syntax,
  bound < fuel -> forall x,
  In x (spec_types W fuel p syntax) <-> SpecTypes W p syntax x.
Proof.
  intros fuel p syntax Enough x. split; [apply spec_types_sound|].
  apply spec_types_complete_above. exact Enough.
Qed.

Theorem fun_outputs_complete_above : forall fuel symbol x,
  bound < fuel -> FunOutputs W symbol x -> In x (fun_outputs W fuel symbol).
Proof.
  intros fuel symbol x Enough (d & parameters & arguments & results & body & sp & Declaration & Parts & Member & Read).
  unfold fun_outputs, functor_decl. rewrite Declaration, Parts, sort_uniq_members.
  apply in_flat_map. exists sp. split; [exact Member|].
  apply spec_types_complete_above; assumption.
Qed.

Theorem fun_outputs_exact : forall fuel symbol,
  bound < fuel -> forall x,
  In x (fun_outputs W fuel symbol) <-> FunOutputs W symbol x.
Proof.
  intros fuel symbol Enough x. split; [apply fun_outputs_sound|].
  apply fun_outputs_complete_above. exact Enough.
Qed.

End ContextBounds.
