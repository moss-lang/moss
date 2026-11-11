(* Assemble the local bind proofs into the executable bind-item callback. *)
From Stdlib Require Import List String.
From NM Require Import Audit EaBase EaBodyContext EaBind EaChecked EaRecSound.
From NM Require EaApplicationSound EaBindCodeEffects EaBudget EaCodeEffects EaContextTableSound
  EaControlSound EaCopySound EaDirectBindSound EaFamily EaFunctor EaMain EaProviderBindSound
  PublicContextFacts.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Lemma body_context_path_bind : forall W P F C path provider wrap C' F' made,
  BodyContext W C ->
  BindItem W P F C (Spec (Some path) None None, Some (AEPath provider None)) wrap C' F' made ->
  BodyContext W C'.
Proof.
  intros W P F C path provider wrap C' F' made Context Typed. inversion Typed; subst.
  - apply body_context_set_ty; assumption.
  - now apply body_context_provide, body_context_set_val.
  - now apply body_context_provide, body_context_set_fn.
  - now apply body_context_set_fun.
  - now apply body_context_set_fun.
Qed.

Section BindItems.
Variables (E : env) (R : rec) (P : program) (bound : nat) (final : est).
Let W := e_W E.
Variable state_valid : est -> Prop.
Let valid (_ : frame) (C : cx) (state : est) := BodyContext W C /\ state_valid state.
Let post := GeneratedPost bound final.

Hypotheses
  (bounded : forall n d, w_decl W n = Some d -> n < bound)
  (budget : EaBudget.metadata_budget W bound < e_metadata_fuel E)
  (contexts : EaContextTableSound.ContextTableSound W)
  (declarations : EaMain.decls_form E bound = Ok tt)
  (counter : forall state, state_valid state -> bound <= es_next state)
  (application_state : forall F C u before result after,
    state_valid before -> EaFunctor.apply_abstract E F C u before = Ok (result, after) ->
    state_valid after)
  (copy_state : forall F C u h before result after,
    state_valid before -> EaFunctor.fun_copy E F C u h before = Ok (result, after) ->
    state_valid after).

Local Lemma valid_scope : forall F F' C C' state,
  valid F C state -> BodyContext W C' ->
  valid F' C' state /\ forall after, valid F' C' after -> valid F C after.
Proof.
  intros F F' C C' state [Context Before] Context'.
  split; [now split|]. intros after [_ After]. now split.
Qed.

Theorem abstract_application_post_sound : forall F C path u d name parameters inputs outputs,
  resolve_sym W (fr_mod F) path = Some u ->
  w_decl W u = Some d ->
  d_ast d = ADFunctor name parameters inputs outputs None ->
  Checked post (valid F C) (bind_scope_valid valid F C) (EaFunctor.apply_abstract E F C u)
    (fun '(wrap, C', F', made) _ =>
      BindItem W P F C (Spec (Some path) None None, None) wrap C' F' made).
Proof.
  intros F C path u d name parameters inputs outputs Resolve Decl Ast
    before [[[wrap C'] F'] made] after Run.
  pose proof (EaCodeEffects.apply_abstract_code_effect _ _ _ _ _ _ _ Run) as Change.
  split; [eapply code_transition_preserves_post; exact Change|]. intros Valid _.
  destruct Valid as [Context State].
  assert (Kind : kind_sym W u = Some DKFunctor) by (unfold kind_sym; fold W; now rewrite Decl, Ast).
  pose proof (EaApplicationSound.apply_abstract_sound E bound F C path u before wrap C' F' made after P
    bounded budget contexts Resolve Kind (counter _ State) Run) as Typed.
  fold W in Typed.
  split; [|exact Typed].
  assert (Result : BodyContext W C').
  { inversion Typed; subst.
    - match goal with
      | Read : resolve_sym W (fr_mod F) path = Some ?other,
        Declared : w_decl W ?other = Some ?body_decl,
        Syntax : d_ast ?body_decl = ADFunctor _ _ _ _ (Some _) |- _ =>
        assert (other = u) by congruence; subst other;
        assert (body_decl = d) by congruence; subst body_decl; congruence
      end.
    - match goal with
      | Application : TypesApplication W F C path ?plan |- _ =>
        unfold TypesApplication in Application; cbn zeta in Application;
        apply (body_context_provide_result W (app_functor plan) C
          (fun t => TAbs (app_identity plan t)) (app_result plan) (app_provisions plan) Context);
        tauto
      end. }
  apply valid_scope; [split; [exact Context|eapply application_state; eassumption]|exact Result].
Qed.

Theorem copy_functor_post_sound : forall F C path u provider h,
  resolve_sym W (fr_mod F) path = Some u ->
  resolve_sym W (fr_mod F) provider = Some h ->
  kind_sym W h = Some DKFunctor ->
  Checked post (valid F C)
    (fun '(wrap, C') => bind_scope_valid valid F C (wrap, C', F, []))
    (EaFunctor.fun_copy E F C u h)
    (fun '(wrap, C') _ =>
      BindItem W P F C (Spec (Some path) None None, Some (AEPath provider None)) wrap C' F []).
Proof.
  intros F C path u provider h Resolve Provider Kind before [wrap C'] after Run.
  pose proof (EaBindCodeEffects.fun_copy_code_effect _ _ _ _ _ _ _ _ Run) as Change.
  split; [eapply code_transition_preserves_post; exact Change|]. intros Valid _.
  destruct Valid as [Context State].
  pose proof (EaCopySound.fun_copy_sound E bound bounded budget P F C path u provider h before wrap C' after
    contexts declarations Resolve Provider Kind (proj1 Context) (counter _ State) Run) as Typed.
  split; [|exact Typed].
  apply valid_scope; [split; [exact Context|eapply copy_state; eassumption]|].
  eapply body_context_path_bind; eassumption.
Qed.

Hypotheses
  (applications : forall symbol meanings,
    AppOf W symbol meanings (EaTypes.app_of W (e_metadata_fuel E) symbol meanings))
  (callbacks : RecSound E P valid post R)
  (function_provider : forall F C path signature,
    Checked post (valid F C) (fun _ => valid F C) (fn_provider E "fn" F C path signature)
      (fun '(target, types, needs) _ =>
        FnProvider W P F C (AEPath path None) signature None target types needs))
  (lambda_provider_checked : forall what F C parameters ann body signature,
    Checked post (valid F C) (fun _ => valid F C)
      (lambda_provider E R what F C parameters ann body signature None)
      (fun '(target, prefix, needs) _ =>
        LambdaProvider W P F C parameters ann body signature None target prefix needs))
  (method_provider : forall method F C syntax signature receiver,
    Checked post (valid F C) (fun _ => valid F C) (meth_provider E R method F C syntax signature receiver)
      (fun '(target, types, prefix, needs) _ =>
        MethProvider W P F C syntax signature receiver target types prefix needs))
  (method_producer_detected : forall C receiver method u,
    PublicContextFacts.CxSection C ->
    FamilyProduces W C u (mkey receiver method) ->
    EaFamily.family_produces E C u (mkey receiver method) = true)
  (closure_provider : forall F C path u provider g,
    resolve_sym W (fr_mod F) path = Some u ->
    resolve_sym W (fr_mod F) provider = Some g ->
    kind_sym W g = Some DKFunctor ->
    Checked post (valid F C) (fun '(wrap, C') => bind_scope_valid valid F C (wrap, C', F, []))
      (EaFunctor.fun_closure E R F C u g)
      (fun '(wrap, C') _ =>
        BindItem W P F C (Spec (Some path) None None, Some (AEPath provider None)) wrap C' F [])).

Local Lemma return_functor_binding : forall (action : M (wrapper * cx)) F C path provider,
  Checked post (valid F C) (fun '(wrap, C') => bind_scope_valid valid F C (wrap, C', F, []))
    action
    (fun '(wrap, C') _ =>
      BindItem W P F C (Spec (Some path) None None, Some (AEPath provider None)) wrap C' F []) ->
  Checked post (valid F C) (bind_scope_valid valid F C)
    (let* (wrap, C') := action in ret (wrap, C', F, []))
    (fun '(wrap, C', F', made) _ =>
      BindItem W P F C (Spec (Some path) None None, Some (AEPath provider None)) wrap C' F' made).
Proof.
  intros action F C path provider Sound before [[[wrap C'] F'] made] after Run.
  apply EaControlSound.mbind_ok in Run as ([body result] & middle & Action & Run).
  unfold ret in Run. injection Run as <- <- <- <- <-.
  exact (Sound _ _ _ Action).
Qed.

Theorem bind_item_post_sound : BindItemSound E P valid post R.
Proof.
  intros F C [[left label brackets] rhs].
  destruct left as [path|]; [|destruct label, brackets; intros before result after Run; discriminate].
  destruct label as [method|].
  - destruct rhs as [syntax|]; [|intros before result after Run; discriminate].
    eapply EaProviderBindSound.method_bind_post_sound; try eassumption.
    + intros F' C' receiver symbol signature arguments state Valid.
      apply valid_scope; [exact Valid|].
      now apply body_context_provide, body_context_set_mth, (proj1 Valid).
    + intros F' C' state [Context _]. exact (proj1 Context).
  - destruct brackets; [intros before result after Run; discriminate|].
    intros before result after Run. pose proof Run as Original.
    unfold bind_item in Run. fold W in Run.
    destruct (resolve_sym W (fr_mod F) path) as [u|] eqn:Resolve;
      cbn [mbind ret] in Run; [|discriminate].
    destruct (w_decl W u) as [d|] eqn:Decl; cbn [mbind ret] in Run; [|discriminate].
    destruct (d_ast d) as [|name| | | |name annotation initial|recv name params output body| |
      name params inputs outputs bodies] eqn:Ast;
      try solve [destruct rhs; discriminate].
    + destruct rhs as [syntax|]; [|discriminate].
      destruct syntax as [| |target arguments| | | | | | | | | | | | |]; try discriminate.
      eapply EaDirectBindSound.type_bind_post_sound; try eassumption.
      intros F' C' symbol ty state Kind. unfold valid. now rewrite body_context_set_ty by exact Kind.
    + destruct rhs as [syntax|]; [|discriminate].
      eapply EaDirectBindSound.value_bind_post_sound; try eassumption.
      intros F' C' symbol ty args state Valid.
      apply valid_scope; [exact Valid|]. now apply body_context_provide, body_context_set_val, (proj1 Valid).
    + destruct recv; try solve [destruct rhs; discriminate].
      destruct rhs as [syntax|]; [|discriminate].
      destruct syntax as [| |provider args| | | | | | | | | | | | |parameters ann syntax];
        try discriminate.
      * destruct args; [discriminate|].
        eapply EaProviderBindSound.named_function_bind_post_sound; try eassumption.
        intros F' C' symbol signature arguments state Valid.
        apply valid_scope; [exact Valid|]. now apply body_context_provide, body_context_set_fn, (proj1 Valid).
      * eapply EaProviderBindSound.lambda_function_bind_post_sound; try eassumption.
        intros F' C' symbol signature arguments state Valid.
        apply valid_scope; [exact Valid|]. now apply body_context_provide, body_context_set_fn, (proj1 Valid).
    + destruct bodies as [bodies|], rhs as [syntax|].
      * destruct syntax; discriminate.
      * eapply EaDirectBindSound.inline_functor_post_sound; try eassumption;
          intros; reflexivity.
      * destruct syntax as [| |provider args| | | | | | | | | | | | |]; try discriminate.
        destruct args; [discriminate|].
        destruct (resolve_sym W (fr_mod F) provider) as [g|] eqn:Read;
          cbn [mbind ret] in Run; [|discriminate].
        destruct (ast_of W g) as [source|] eqn:Source; [|discriminate].
        destruct source as [| | | | | | | |pn ps pi po code]; try discriminate.
        assert (Kind : kind_sym W g = Some DKFunctor).
        { unfold ast_of, kind_sym in *. destruct (w_decl W g); [|discriminate].
          injection Source as Source. now rewrite Source. }
        destruct code as [code|].
        -- eapply (return_functor_binding (EaFunctor.fun_closure E R F C u g) F C path provider);
             [now apply closure_provider|exact Run].
        -- eapply (return_functor_binding (EaFunctor.fun_copy E F C u g) F C path provider);
             [now apply copy_functor_post_sound|exact Run].
      * eapply abstract_application_post_sound; eassumption.
Qed.

End BindItems.
