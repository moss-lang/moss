(* Defined providers read the same context, needs, and signature as a call. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaBind EaCall EaTypes EaTypesSound EaChecked.
From NM Require Import LegacyBodyJudgments.
From NM Require EaContextTableSound EaControlSound EaCallSound EaSatisfiesSound EaMergeSound PublicContextFacts.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma kind_of_read_declaration : forall W symbol syntax,
  ast_of W symbol = Some syntax -> kind_sym W symbol = kind_of syntax.
Proof.
  intros W symbol syntax Read. unfold ast_of, kind_sym in *.
  destruct (w_decl W symbol); [|discriminate]. now injection Read as <-.
Qed.

Lemma attached_declaration_name : forall bundle W receiver name symbol recv actual params result body,
  WorldOf bundle W ->
  w_attached W receiver name = Some symbol ->
  ast_of W symbol = Some (ADFn (RAttached recv) actual params result body) ->
  actual = name.
Proof.
  intros bundle W receiver name symbol recv actual params result body World Found Read.
  apply (proj1 (@wo_attached bundle W World receiver name symbol)) in Found.
  destruct Found as (decl & rn & ps & rt & b & Decl & Ast & _).
  unfold ast_of in Read. rewrite Decl in Read. cbn [option_map] in Read.
  rewrite Ast in Read. now injection Read.
Qed.

Lemma set_type_rows_keep_section : forall bindings C,
  PublicContextFacts.CxSection C ->
  PublicContextFacts.CxSection (fold_left (fun C q => set_ty C (fst q) (snd q)) bindings C).
Proof.
  induction bindings as [|[symbol t] bindings IH]; intros C Section; [exact Section|].
  cbn [fold_left]. apply IH. exact Section.
Qed.

Local Lemma provider_pure_read : forall A (read : er A) before value after,
  pure read before = Ok (value, after) -> read = Ok value /\ before = after.
Proof. intros A [found|] before value after Read; [now injection Read as -> ->|discriminate]. Qed.

Section DefinedProviders.
Variables (E : env) (P : program).
Let W := e_W E.
Let fuel := e_fuel E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Hypothesis contexts : EaContextTableSound.ContextTableSound W.
Hypothesis caller_section : forall F C state, valid F C state -> PublicContextFacts.CxSection C.

Local Lemma provider_context : forall g formal needs,
  (kind_sym W g = Some DKFn \/ kind_sym W g = Some DKMeth) ->
  w_context W g = Some (formal, needs) ->
  CtxOf W g formal needs.
Proof.
  intros g formal needs [Kind|Kind] Found;
    pose proof (contexts g formal needs Found) as Context; now rewrite Kind in Context.
Qed.

Theorem check_named_provider_sound : forall kind C g recv want plan,
  PublicContextFacts.CxSection C ->
  (kind_sym W g = Some DKFn \/ kind_sym W g = Some DKMeth) ->
  check_named_provider E kind C g recv want = Ok plan ->
  TypesNamedProviderCall W C g recv want plan.
Proof.
  intros kind C g recv want plan Caller Kind Read.
  unfold check_named_provider in Read. fold W fuel in Read. cbv zeta in Read.
  destruct (w_context W g) as [[formal needs]|] eqn:ContextRead; cbn [rbind] in Read; [|discriminate].
  apply rbind_ok in Read as (arguments & ThetaRead & Read).
  apply rbind_ok in Read as ([] & NeedsRead & Read).
  apply rbind_ok in Read as ([] & ForceRead & Read).
  apply rbind_ok in Read as (provided & SignatureRead & Read).
  apply rbind_ok in Read as ([] & SignatureCheck & Read).
  destruct (fsig_eqb provided want) eqn:Signature; [|discriminate].
  injection Read as <-. apply EaMergeSound.fsig_eqb_spec in Signature. subst provided.
  pose proof (provider_context g formal needs Kind ContextRead) as Context.
  destruct (PublicContextFacts.function_context_merge _ _ _ _ Context) as [Solved Formal].
  constructor; cbn [provider_symbol provider_formal provider_needs provider_arguments].
  - reflexivity.
  - exact Context.
  - eapply EaSatisfiesSound.check_needs_sound; [eapply EaSatisfiesSound.ctxof_subst_functional|]; eassumption.
  - eapply EaCallSound.call_theta_sound; eassumption.
  - eapply sig_of_sound; exact SignatureRead.
  - eapply EaCallSound.rule_b_sound; eassumption.
Qed.

Lemma translate_named_provider_sound : forall C g recv want plan,
  TypesNamedProviderCall W C g recv want plan ->
  translate_named_provider C plan =
    (g, TypeCapture (provider_arguments plan) (provider_arguments plan),
      satisfaction_map C (provider_formal plan) (provider_arguments plan) (provider_needs plan)).
Proof.
  intros C g recv want plan Typed. unfold translate_named_provider.
  now rewrite (provider_source_exact Typed), EaSatisfiesSound.translate_needs_exact.
Qed.

Theorem defined_function_provider_post_sound : forall F C path symbol name parameters result body signature kind,
  resolve_sym W (fr_mod F) path = Some symbol ->
  ast_of W symbol = Some (ADFn RPlain name parameters result (Some body)) ->
  Checked post (valid F C) (fun _ => valid F C) (fn_provider E kind F C path signature)
    (fun '(target, types, sm) _ =>
      FnProvider W P F C (AEPath path None) signature None target types sm).
Proof.
  intros F C path symbol name parameters result body signature kind Resolve Ast before [[target types] sm] after Run.
  assert (Kind : kind_sym W symbol = Some DKFn) by exact (kind_of_read_declaration W _ _ Ast).
  assert (KindChecked : kind_is W symbol DKFn = true) by (apply kind_is_spec; exact Kind).
  unfold fn_provider in Run. fold W fuel in Run.
  rewrite Resolve in Run. cbn [mbind ret] in Run.
  rewrite KindChecked in Run. cbn [mbind ret] in Run. rewrite Ast in Run.
  apply EaControlSound.mbind_ok in Run as (plan & middle & Check & Run).
  apply provider_pure_read in Check as [Check <-].
  unfold ret in Run.
  assert (Output : translate_named_provider C plan = (target, types, sm)) by congruence.
  assert (SameState : after = before) by congruence. subst after.
  split; [tauto|]. intros Valid _. split; [exact Valid|].
  pose proof (check_named_provider_sound kind C symbol None _ plan
    (caller_section _ _ _ Valid) (or_introl Kind) Check) as Typed.
  pose proof (translate_named_provider_sound _ _ _ _ _ Typed) as Emit.
  rewrite Emit in Output. injection Output as <- <- <-.
  destruct Typed as [_ Context Needs Theta Signature InForce].
  eapply FPDefined; [exact Resolve|exact Kind|eauto 8|exact Context|split; [reflexivity|exact Needs]|
    exact Theta|exact Signature|exact InForce].
Qed.

Theorem attached_method_provider_post_sound : forall bundle R method F C path name signature receiver,
  WorldOf bundle W ->
  Checked post (valid F C) (fun _ => valid F C)
    (meth_provider E R method F C (AEField (AEPath path None) name) signature receiver)
    (fun '(target, types, prefix, sm) _ =>
      MethProvider W P F C (AEField (AEPath path None) name) signature receiver target types prefix sm).
Proof.
  intros bundle R method F C path name signature receiver World before [[[target types] prefix] sm] after Run.
  unfold meth_provider in Run. fold W fuel in Run.
  destruct (resolve_sym W (fr_mod F) path) as [rg|] eqn:Resolve; cbn [mbind ret] in Run; [|discriminate].
  destruct (w_attached W rg name) as [att|] eqn:AttachedRead; cbn [mbind ret] in Run; [|discriminate].
  destruct (ast_of W att) as [ast|] eqn:Ast; [|discriminate].
  destruct ast as [| | | | | |recv actual_name parameters result body| |]; try discriminate.
  destruct recv as [| |receiver_name]; try discriminate.
  destruct body as [body|]; [|discriminate].
  assert (Name : actual_name = name) by
    (eapply attached_declaration_name; [exact World|exact AttachedRead|exact Ast]).
  subst actual_name.
  destruct (canon C receiver) as [machine|fields| |abstract|rg' readings|members] eqn:Canonical;
    try discriminate.
  destruct (Nat.eqb rg rg') eqn:Receiver; cbn [mbind ret] in Run; [|discriminate].
  apply Nat.eqb_eq in Receiver. subst rg'.
  set (caller := fold_left (fun C q => set_ty C (fst q) (snd q)) readings C) in *.
  apply EaControlSound.mbind_ok in Run as (plan & middle & Check & Run).
  apply provider_pure_read in Check as [Check <-].
  assert (Kind : kind_sym W att = Some DKMeth) by exact (kind_of_read_declaration W _ _ Ast).
  split.
  { destruct (translate_named_provider caller plan) as [[source types'] provisions].
    unfold ret in Run. injection Run as <- <- <- <- <-. tauto. }
  intros Valid _.
  assert (Caller : PublicContextFacts.CxSection caller) by
    (apply set_type_rows_keep_section; exact (caller_section _ _ _ Valid)).
  pose proof (check_named_provider_sound "attached method" caller att (Some (TNom rg readings)) _ plan
    Caller (or_intror Kind) Check) as Typed.
  pose proof (translate_named_provider_sound _ _ _ _ _ Typed) as Emit.
  rewrite Emit in Run. unfold ret in Run. injection Run as <- <- <- <- <-.
  split; [exact Valid|].
  destruct Typed as [_ Context Needs Theta Signature InForce].
  eapply MPAttached; [exact Resolve|exact AttachedRead|eauto 8|exact Canonical|reflexivity|
    exact Context|split; [reflexivity|exact Needs]|exact Theta|now rewrite Canonical|exact InForce].
Qed.
End DefinedProviders.
