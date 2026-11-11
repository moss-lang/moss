(* Direct type and value binds and inline defined-functor application. *)
From Stdlib Require Import List String Bool.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaBind EaChecked EaRecSound EaControlSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Section DirectBinds.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.

Hypothesis type_scope : forall F C symbol t state,
  kind_sym W symbol = Some DKType ->
  valid F (set_ty C symbol t) state <-> valid F C state.
Hypothesis value_scope : forall F C symbol t arguments state,
  valid F C state ->
  let C' := provide C (IVal symbol) arguments (set_val C symbol t) in
  valid F C' state /\ forall final, valid F C' final -> valid F C final.
Hypothesis module_scope : forall F C path symbol state,
  valid (with_mod F path symbol) C state <-> valid F C state.
Hypothesis carry_scope : forall F nested C state,
  valid (carry_frame_allocations F nested) C state <-> valid nested C state.

Hypothesis applications : forall symbol meanings,
  AppOf W symbol meanings (app_of W metadata_fuel symbol meanings).
Hypothesis callbacks : RecSound E P valid post R.

Local Ltac read_binding_head run resolved declaration syntax :=
  unfold bind_item in run;
  fold W fuel metadata_fuel in run;
  rewrite resolved in run; cbn [mbind ret] in run;
  rewrite declaration in run; cbn [mbind ret] in run;
  rewrite syntax in run; cbn [mbind ret] in run.

Theorem type_bind_post_sound : forall F C left symbol declaration name right arguments,
  resolve_sym W (fr_mod F) left = Some symbol ->
  w_decl W symbol = Some declaration ->
  d_ast declaration = ADType name ->
  BindItemAt E P valid post R F C (Spec (Some left) None None, Some (AEPath right arguments)).
Proof.
  intros F C left symbol declaration name right arguments Resolve Decl Ast before
    [[[wrap C'] F'] made] after Run.
  read_binding_head Run Resolve Decl Ast.
  destruct (nmem symbol (fr_pins F)) eqn:Pinned; cbn [negb mbind ret] in Run; [discriminate|].
  destruct (ty_of W fuel (fr_mod F) (c_ty C) None (ATRef right arguments)) as [t|] eqn:TypeRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  injection Run as <- <- <- <- <-.
  assert (Kind : kind_sym W symbol = Some DKType) by (unfold kind_sym; fold W; now rewrite Decl, Ast).
  split; [tauto|]. intros Valid _. split.
  - split; [now apply type_scope|]. intros final Scope.
    exact (proj1 (type_scope F C symbol (canon C t) final Kind) Scope).
  - eapply BTypeBind.
    + exact Resolve.
    + exact Kind.
    + intro Member. apply nmem_spec in Member. congruence.
    + eapply ty_of_sound. exact TypeRead.
Qed.

Theorem value_bind_post_sound : forall F C left symbol declaration name annotation initializer expression,
  resolve_sym W (fr_mod F) left = Some symbol ->
  w_decl W symbol = Some declaration ->
  d_ast declaration = ADVal name annotation initializer ->
  BindItemAt E P valid post R F C (Spec (Some left) None None, Some expression).
Proof.
  intros F C left symbol declaration name annotation initializer expression Resolve Decl Ast before
    [[[wrap C'] F'] made] after Run.
  read_binding_head Run Resolve Decl Ast.
  destruct (ty_of W fuel (d_mod declaration) (c_ty C) None annotation) as [declared|] eqn:TypeRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  apply mbind_ok in Run as [[value actual] [next [ValueRead Run]]].
  destruct (fitsb actual declared) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
  injection Run as <- <- <- <- <-.
  destruct (proj1 callbacks F C expression (Some declared) before (value, actual) next ValueRead)
    as [Backward TypedValue].
  split; [exact Backward|]. intros Valid Post.
  destruct (TypedValue Valid Post) as [Final ValueTyped].
  split.
  - exact (value_scope F C symbol declared _ next Final).
  - eapply BValBind; [exact Resolve|exact Decl|exact Ast|eapply ty_of_sound; exact TypeRead|
      exact ValueTyped|now apply fitsb_exact|apply applications].
Qed.

Theorem inline_functor_post_sound : forall F C left symbol declaration name parameters inputs outputs bodies,
  resolve_sym W (fr_mod F) left = Some symbol ->
  w_decl W symbol = Some declaration ->
  d_ast declaration = ADFunctor name parameters inputs outputs (Some bodies) ->
  BindItemAt E P valid post R F C (Spec (Some left) None None, None).
Proof.
  intros F C left symbol declaration name parameters inputs outputs bodies Resolve Decl Ast before
    [[[wrap C'] F'] made] after Run.
  read_binding_head Run Resolve Decl Ast.
  destruct (nmem symbol (fr_applying F)) eqn:Recursive; cbn [negb mbind ret] in Run; [discriminate|].
  apply mbind_ok in Run as [[[[nested D] G] created] [next [BodyRead Run]]].
  unfold ret in Run. injection Run as <- <- <- <- <-.
  destruct (proj2 (proj2 callbacks) _ _ _ _ _ _ BodyRead) as [Backward TypedBody].
  split; [exact Backward|]. intros Valid Post.
  destruct (TypedBody (proj2 (module_scope F C (d_mod declaration) symbol _) Valid) Post)
    as [[Final Restore] BodyTyped].
  split.
  - split; [exact (proj2 (carry_scope F G D next) Final)|]. intros final Scope.
    apply (proj1 (module_scope F C (d_mod declaration) symbol final)).
    apply Restore. exact (proj1 (carry_scope F G D final) Scope).
  - eapply BApplyBody; [exact Resolve|exact Decl|exact Ast| |exact BodyTyped].
    intro Member. apply nmem_spec in Member. congruence.
Qed.
End DirectBinds.
