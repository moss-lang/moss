(* The context facts used by executable expression and bind checking. *)
From Stdlib Require Import List.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaBind EaExpr.
From NM Require PublicCallRealize PublicChainSigs PublicContextFacts PublicFormation PublicMerge.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.

Definition BodyContext (W : world) (C : cx) : Prop :=
  PublicContextFacts.CxSection C /\ PublicCallRealize.KeysMatch (c_provisions C) /\
  PublicFormation.TysScoped W C.

Lemma empty_body_context : forall W, BodyContext W (Cx [] [] [] [] []).
Proof.
  intro W. split; [|split; intros ? ?; contradiction].
  unfold PublicContextFacts.CxSection. split; [intro x; apply MERefl|].
  apply PublicMerge.merge_eq_sound. split; intros c ? ? [].
Qed.

Theorem function_body_context : forall W f C needs,
  PublicFormation.MentionsTyped W -> CtxOf W f C needs -> BodyContext W C.
Proof.
  intros W f C needs Mentions Formed. split; [|split].
  - exact (proj2 (PublicContextFacts.function_context_merge _ _ _ _ Formed)).
  - exact (PublicCallRealize.ctx_keys_match _ _ _ _ Formed).
  - exact (proj2 (proj2 (PublicFormation.ctxof_cx_formed W Mentions _ _ _ Formed))).
Qed.

Theorem functor_body_context : forall W u C needs,
  PublicFormation.MentionsTyped W -> FunCtx W u C needs -> BodyContext W C.
Proof.
  intros W u C needs Mentions Formed. split; [|split].
  - exact (proj2 (PublicContextFacts.functor_context_merge _ _ _ _ Formed)).
  - destruct Formed as
      (d & ps & a & r & b & es & tm & tm' & sub & classes & implicit &
        Decl & Parts & Entries & Implied & Types & MostGeneral & Merge & Section & -> & Needs & Recorded).
    intros key provision Member. cbn [entries_cx c_provisions] in Member.
    apply in_flat_map in Member as [row [_ Member]].
    destruct row; cbn in Member; try contradiction.
    all: destruct Member as [Same|[]]; now inversion Same.
  - exact (proj2 (proj2 (PublicFormation.functx_cx_formed W Mentions _ _ _ Formed))).
Qed.

Lemma body_context_replace_locals : forall W C locals,
  BodyContext W (Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) locals) <-> BodyContext W C.
Proof. reflexivity. Qed.

Lemma body_context_add_local : forall W C name ty mode,
  BodyContext W (add_local C name ty mode) <-> BodyContext W C.
Proof. reflexivity. Qed.

Lemma body_context_add_binds : forall W C bindings,
  BodyContext W (add_binds C bindings) <-> BodyContext W C.
Proof.
  intros W C bindings. unfold add_binds. revert C.
  induction bindings as [|[name ty] rest IH]; intro C; [reflexivity|].
  cbn [fold_left fst snd]. now rewrite IH, body_context_add_local.
Qed.

Lemma body_context_set_ty : forall W C symbol ty,
  kind_sym W symbol = Some DKType ->
  BodyContext W (set_ty C symbol ty) <-> BodyContext W C.
Proof.
  intros W C symbol ty Kind. split.
  - intros [Section [Keys Scoped]]. split; [exact Section|split; [exact Keys|]].
    intros x Present. apply Scoped. cbn. now right.
  - intros [Section [Keys Scoped]]. split; [exact Section|split; [exact Keys|]].
    intros x [<-|Present]; [exact Kind|now apply Scoped].
Qed.

Lemma body_context_set_provision : forall W C key provision,
  PublicCallRealize.KeysMatch [(key, provision)] ->
  BodyContext W C -> BodyContext W (set_provision C key provision).
Proof.
  intros W C key provision Shape [Section [Keys Scoped]].
  split; [exact Section|split; [now apply PublicCallRealize.keys_match_set|exact Scoped]].
Qed.

Lemma body_context_set_val : forall W C value ty,
  BodyContext W C -> BodyContext W (set_val C value ty).
Proof. intros. apply body_context_set_provision; [intros k p [Same|[]]; now inversion Same|assumption]. Qed.

Lemma body_context_set_fn : forall W C symbol sig,
  BodyContext W C -> BodyContext W (set_fn C symbol sig).
Proof. intros. apply body_context_set_provision; [intros k p [Same|[]]; now inversion Same|assumption]. Qed.

Lemma body_context_set_mth : forall W C receiver method sig,
  BodyContext W C -> BodyContext W (set_mth C receiver method sig).
Proof. intros. apply body_context_set_provision; [intros k p [Same|[]]; now inversion Same|assumption]. Qed.

Lemma body_context_set_fun : forall W C symbol fixed,
  BodyContext W C -> BodyContext W (set_fun C symbol fixed).
Proof. intros. apply body_context_set_provision; [intros k p [Same|[]]; now inversion Same|assumption]. Qed.

Lemma body_context_provide : forall W C key arguments result,
  BodyContext W result -> BodyContext W (provide C key arguments result).
Proof.
  intros W C key arguments result Valid.
  destruct Valid as [Section [Keys Scoped]].
  unfold provide. destruct (ilook key (c_provisions result)) as [found|] eqn:Read;
    [|now split; [|split]].
  pose proof (Keys key found (PublicFormation.ilook_in_key _ _ _ _ Read)) as Shape.
  destruct key as [t|v|f|r m [|? ?]|u], found; try contradiction Shape;
    try solve [now split; [|split]].
  all: apply body_context_set_provision; [intros k p [Same|[]]; now inversion Same|now split; [|split]].
Qed.

Theorem captured_body_context : forall W C caps values,
  BodyContext W C -> BodyContext W (captured_cx C caps values).
Proof.
  intros W C caps values Valid. unfold captured_cx.
  generalize (proj2 (body_context_replace_locals W C []) Valid).
  generalize (Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) []).
  induction (combine caps values) as [|[name value] rest IH]; intros start Body; [exact Body|].
  cbn [fold_left]. apply IH.
  destruct (slook name (c_locals C)) as [[ty mode]|]; [|exact Body].
  now apply body_context_add_local, body_context_provide, body_context_set_val.
Qed.

Lemma lambda_parameter_body_context : forall W C names types,
  BodyContext W (lambda_parameter_context C names types) <-> BodyContext W C.
Proof.
  intros W C names types. change (BodyContext W (add_binds C (combine names types)) <-> BodyContext W C).
  apply body_context_add_binds.
Qed.

Theorem body_context_provide_result : forall W u C output result keys,
  BodyContext W C -> ProvideResult W u C output result keys -> BodyContext W result.
Proof.
  intros W u C output result keys [Section [Keys Scoped]] Result.
  destruct (PublicFormation.provide_result_shape W _ _ _ _ _ Result) as
    [Sub [_ (outputs & Output & TypeRows)]].
  split; [|split].
  - unfold PublicContextFacts.CxSection in *. rewrite Sub.
    now rewrite (PublicContextFacts.provided_result_merge W _ _ _ _ _ Result).
  - eapply PublicChainSigs.pr_keys_match; eassumption.
  - intros symbol Present. rewrite TypeRows, map_map in Present.
    change (In symbol (map fst (output_type_map C outputs output))) in Present.
    unfold output_type_map in Present. rewrite !map_map, map_app in Present.
    apply in_app_or in Present as [New|Old]; [|exact (Scoped _ Old)].
    rewrite !map_map in New. cbn in New. rewrite map_id in New.
    now apply (PublicFormation.outputs_typed W u), Output.
Qed.
