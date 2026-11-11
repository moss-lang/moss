(* Defined providers read the same context, needs, and signature as a call. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaBind EaCall EaTypes EaTypesSound EaChecked.
From NM Require EaContextTableSound EaCallSound EaSatisfiesSound EaMergeSound PublicContextFacts.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
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

Local Lemma provider_call_reads_sound : forall g C formal needs who th sm,
  CtxOf W g formal needs ->
  PublicContextFacts.CxSection C ->
  call_theta W who C formal = Ok th ->
  satisfies W ("calling " ++ qs W g) who C formal th needs = Ok sm ->
  rule_b W who C formal = Ok tt ->
  Satisfies C formal th needs sm /\ CallTheta W C formal g th /\ RequirementsInForce C formal.
Proof.
  intros g C formal needs who th sm Context Caller Types Needs InForce.
  destruct (PublicContextFacts.function_context_merge _ _ _ _ Context) as [Solved Formal].
  split.
  - eapply EaSatisfiesSound.satisfies_sound; [eapply EaSatisfiesSound.ctxof_subst_functional|]; eassumption.
  - split.
    + eapply EaCallSound.call_theta_sound; eassumption.
    + eapply EaCallSound.rule_b_sound; eassumption.
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
  destruct (w_context W symbol) as [[formal needs]|] eqn:ContextRead; cbn [mbind ret] in Run; [|discriminate].
  set (who := ("binding to " ++ qs W symbol)%string) in *.
  destruct (call_theta W who C formal) as [th|] eqn:ThetaRead; cbn [pure mbind ret] in Run; [|discriminate].
  destruct (satisfies W ("calling " ++ qs W symbol) who C formal th needs) as [provisions|] eqn:NeedsRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  destruct (rule_b W who C formal) as [[]|] eqn:ForceRead; cbn [pure mbind ret] in Run; [|discriminate].
  destruct (sig_of W fuel symbol (c_ty C) None) as [provided|] eqn:SignatureRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  destruct (fsig_eqb provided (FSig (map (canon C) (sg_params signature)) (canon C (sg_ret signature))))
    eqn:SignatureCheck; cbn [mbind ret] in Run; [|discriminate].
  injection Run as <- <- <- <-.
  split; [tauto|]. intros Valid _. split; [exact Valid|].
  pose proof (provider_context symbol formal needs (or_introl Kind) ContextRead) as Context.
  destruct (provider_call_reads_sound _ _ _ _ _ _ _ Context (caller_section _ _ _ Valid)
    ThetaRead NeedsRead ForceRead) as [Satisfies [Theta InForce]].
  apply EaMergeSound.fsig_eqb_spec in SignatureCheck. subst provided.
  eapply FPDefined; [exact Resolve|exact Kind|eauto 8|exact Context|exact Satisfies|exact Theta|
    eapply sig_of_sound; exact SignatureRead|exact InForce].
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
  set (who := ("binding to " ++ qs W att)%string) in *.
  destruct (w_context W att) as [[formal needs]|] eqn:ContextRead; cbn [mbind ret] in Run; [|discriminate].
  destruct (call_theta W who caller formal) as [th|] eqn:ThetaRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  destruct (satisfies W ("calling " ++ qs W att) who caller formal th needs) as [provisions|] eqn:NeedsRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  destruct (rule_b W who caller formal) as [[]|] eqn:ForceRead; cbn [pure mbind ret] in Run; [|discriminate].
  destruct (sig_of W fuel att (c_ty caller) (Some (TNom rg readings))) as [provided|] eqn:SignatureRead;
    cbn [pure mbind ret] in Run; [|discriminate].
  destruct (fsig_eqb provided (FSig (map (canon C) (sg_params signature)) (canon C (sg_ret signature))))
    eqn:SignatureCheck; cbn [mbind ret] in Run; [|discriminate].
  injection Run as <- <- <- <- <-.
  split; [tauto|]. intros Valid _. split; [exact Valid|].
  assert (Kind : kind_sym W att = Some DKMeth) by exact (kind_of_read_declaration W _ _ Ast).
  pose proof (provider_context att formal needs (or_intror Kind) ContextRead) as Context.
  assert (Caller : PublicContextFacts.CxSection caller) by (apply set_type_rows_keep_section; exact (caller_section _ _ _ Valid)).
  destruct (provider_call_reads_sound _ _ _ _ _ _ _ Context Caller ThetaRead NeedsRead ForceRead)
    as [Satisfies [Theta InForce]].
  apply EaMergeSound.fsig_eqb_spec in SignatureCheck. subst provided.
  eapply MPAttached; [exact Resolve|exact AttachedRead|eauto 8|exact Canonical|reflexivity|
    exact Context|exact Satisfies|exact Theta|eapply sig_of_sound; rewrite Canonical; exact SignatureRead|exact InForce].
Qed.
End DefinedProviders.
