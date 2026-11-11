(* The bind wrappers validate the declared signature and install a
   provider once its own translation has been checked. *)
From Stdlib Require Import List String Bool.
From NM Require Import Audit EaBase EaBind EaTypes EaTypesSound EaFamily EaControlSound EaChecked EaRecSound.
From NM Require PublicTypeRead.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma cfun_lookup_in_keys : forall C u arguments,
  nlook u (c_funs C) = Some arguments -> In u (cfun_keys C).
Proof.
  intros C u arguments Lookup. unfold c_funs in Lookup.
  apply PublicTypeRead.nlook_in in Lookup.
  apply in_flat_map in Lookup as [[key entry] [Present Found]].
  unfold cfun_keys. apply in_flat_map. exists (key, entry). split; [exact Present|].
  destruct key, entry; cbn in *; try contradiction.
  destruct Found as [Equal|[]]. inversion Equal. now left.
Qed.

Section ProviderBinds.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.

Hypothesis applications : forall symbol meanings,
  AppOf W symbol meanings (app_of W metadata_fuel symbol meanings).
Hypothesis function_scope : forall F C symbol signature arguments state,
  valid F (provide C (IFn symbol) arguments (set_fn C symbol signature)) state <-> valid F C state.
Hypothesis method_scope : forall F C receiver method signature arguments state,
  valid F (provide C (mkey receiver method) arguments (set_mth C receiver method signature)) state
  <-> valid F C state.
Hypothesis method_producer_detected : forall C receiver method u,
  FamilyProduces W C u (mkey receiver method) ->
  family_produces E C u (mkey receiver method) = true.

Hypothesis function_provider_sound : forall F C path signature,
  Checked post (valid F C) (fun _ => valid F C) (fn_provider E "fn" F C path signature)
    (fun '(target, types, sm) _ => FnProvider W P F C (AEPath path None) signature None target types sm).

Hypothesis method_provider_sound : forall method F C syntax signature receiver,
  Checked post (valid F C) (fun _ => valid F C) (meth_provider E R method F C syntax signature receiver)
    (fun '(target, types, prefix, sm) _ =>
      MethProvider W P F C syntax signature receiver target types prefix sm).

Theorem named_function_bind_post_sound : forall F C left symbol declaration name parameters result body provider,
  resolve_sym W (fr_mod F) left = Some symbol ->
  w_decl W symbol = Some declaration ->
  d_ast declaration = ADFn RPlain name parameters result body ->
  BindItemAt E P valid post R F C (Spec (Some left) None None, Some (AEPath provider None)).
Proof.
  intros F C left symbol declaration name parameters result body provider Resolve Decl Ast
    before [[[wrap C'] F'] made] after Run.
  unfold bind_item in Run. fold W fuel metadata_fuel in Run.
  rewrite Resolve in Run. cbn [mbind ret] in Run.
  rewrite Decl in Run. cbn [mbind ret] in Run.
  rewrite Ast in Run. cbn [mbind ret] in Run.
  destruct (sig_of W fuel symbol (c_ty C) None) as [signature|] eqn:Signature;
    cbn [pure mbind ret] in Run; [|discriminate].
  apply mbind_ok in Run as [[[target types] sm] [next [Provider Run]]].
  unfold ret in Run. injection Run as <- <- <- <- <-.
  destruct (function_provider_sound F C provider signature _ _ _ Provider) as [Backward TypedProvider].
  split; [exact Backward|]. intros Valid Post.
  destruct (TypedProvider Valid Post) as [Final ProviderTyped].
  split.
  - split; [now apply function_scope|]. intros final Scope.
    exact (proj1 (function_scope F C symbol signature _ final) Scope).
  - eapply BFnBind.
    + exact Resolve.
    + unfold kind_sym. fold W. now rewrite Decl, Ast.
    + eapply sig_of_sound. exact Signature.
    + exact ProviderTyped.
    + apply applications.
Qed.

Theorem method_bind_post_sound : forall F C left method brackets_syntax syntax,
  BindItemAt E P valid post R F C (Spec (Some left) (Some method) brackets_syntax, Some syntax).
Proof.
  intros F C left method brackets_syntax syntax before [[[wrap C'] F'] made] after Run.
  unfold bind_item in Run. fold W fuel metadata_fuel in Run.
  destruct (resolve_sym W (fr_mod F) left) as [receiver|] eqn:Resolve;
    cbn [mbind ret] in Run; [|discriminate].
  destruct (method_named W (fr_mod F) receiver method) as [symbol|] eqn:Method;
    cbn [mbind ret] in Run; [|discriminate].
  destruct (match ast_of W symbol with Some (ADFn _ _ _ _ (Some _)) => false | _ => true end) eqn:Allowed;
    cbn [mbind ret] in Run; [|discriminate].
  destruct (brackets W fuel (fr_mod F) (c_ty C) symbol brackets_syntax) as [arguments|] eqn:BracketsRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  destruct (ty_of W fuel (fr_mod F) (arguments ++ c_ty C) None (ATRef left None)) as [rt|] eqn:ReceiverRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  destruct (sig_of W fuel symbol (arguments ++ c_ty C) (Some rt)) as [signature|] eqn:Signature;
    cbn [pure mbind ret] in Run; [|discriminate].
  apply mbind_ok in Run as [[[[target types] prefix] sm] [next [Provider Run]]].
  destruct (find (fun u => family_produces E C u (mkey (canon C rt) symbol)) (cfun_keys C))
    eqn:Families; cbn [mbind ret] in Run; [discriminate|].
  injection Run as <- <- <- <- <-.
  destruct (method_provider_sound symbol F C syntax signature rt _ _ _ Provider) as [Backward TypedProvider].
  split; [exact Backward|]. intros Valid Post.
  destruct (TypedProvider Valid Post) as [Final ProviderTyped].
  split.
  - split; [now apply method_scope|]. intros final Scope.
    exact (proj1 (method_scope F C (canon C rt) symbol signature _ final) Scope).
  - eapply BMethBind.
    + exact Resolve.
    + now apply method_named_sound.
    + eapply brackets_sound. exact BracketsRead.
    + eapply ty_of_sound. exact ReceiverRead.
    + eapply sig_of_sound. exact Signature.
    + exact ProviderTyped.
    + apply applications.
    + intros u Produces.
      pose proof (method_producer_detected _ _ _ _ Produces) as Detected.
      destruct Produces as [fixed [theta [C0 [keys [InForce _]]]]].
      pose proof (cfun_lookup_in_keys _ _ _ InForce) as Present.
      pose proof (find_none _ _ Families u Present) as Missing. congruence.
    + intros (rc & nm & ps & rt0 & body & Defined). fold W in Defined. rewrite Defined in Allowed. discriminate.
Qed.
End ProviderBinds.
