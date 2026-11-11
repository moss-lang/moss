(* Source function providers select a declaration or a forwarder handle.
   Translation assigns a slot only to the generated body. *)
From Stdlib Require Import List.
From NM Require Import Audit.
From NM Require BodyProviderTyping EaBind EaDefinedProviderSound.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Import BodyProviderTyping EaBind EaDefinedProviderSound.
Local Open Scope list_scope.

Inductive function_provider_plan :=
| NamedFunctionProvider (provider : named_provider_plan)
| AbstractFunctionForwarder (occurrence : nat) (handle : forwarder_handle).

Definition TypesFunctionProvider (W : world) (F : frame) (C : cx) (pa : path)
    (signature : fsig) (plan : function_provider_plan) : Prop :=
  match plan with
  | NamedFunctionProvider named => TypesNamedFunctionPlan W F C pa signature named
  | AbstractFunctionForwarder _ handle => TypesForwarder W F C pa signature handle
  end.

Definition function_plan_target (resolve : nat -> nat) (plan : function_provider_plan) : nat :=
  match plan with
  | NamedFunctionProvider named => provider_symbol named
  | AbstractFunctionForwarder occurrence _ => resolve occurrence
  end.

Definition function_plan_capture (plan : function_provider_plan) : type_capture :=
  match plan with
  | NamedFunctionProvider named => named_provider_capture named
  | AbstractFunctionForwarder _ _ => TypeCapture [] []
  end.

Definition function_plan_provisions (C : cx) (plan : function_provider_plan) : smap :=
  match plan with
  | NamedFunctionProvider named => named_provider_provisions C named
  | AbstractFunctionForwarder _ handle =>
      [(IFn (forwarder_source handle), IFn (forwarder_source handle))]
  end.

Definition FunctionProviderPlacement (W : world) (P : program) (resolve : nat -> nat)
    (signature : fsig) (plan : function_provider_plan) : Prop :=
  match plan with
  | NamedFunctionProvider _ => True
  | AbstractFunctionForwarder occurrence handle =>
      TranslatesForwarder W P handle signature (resolve occurrence)
  end.

Theorem function_provider_plan_translates : forall W P F C path signature plan resolve,
  TypesFunctionProvider W F C path signature plan ->
  FunctionProviderPlacement W P resolve signature plan ->
  FnProvider W P F C (AEPath path None) signature None (function_plan_target resolve plan)
    (function_plan_capture plan) (function_plan_provisions C plan).
Proof.
  intros W P F C path signature [named|occurrence handle] resolve Typed Placement.
  - now apply named_function_plan_translates.
  - now apply FPForward.
Qed.

Theorem function_provider_plan_readback : forall W P F C path signature target types provisions,
  FnProvider W P F C (AEPath path None) signature None target types provisions ->
  exists plan,
    TypesFunctionProvider W F C path signature plan /\
    FunctionProviderPlacement W P (fun occurrence => occurrence) signature plan /\
    target = function_plan_target (fun occurrence => occurrence) plan /\
    types = function_plan_capture plan /\ provisions = function_plan_provisions C plan.
Proof.
  intros W P F C path signature target types provisions Typed.
  inversion Typed as [F0 C0 path0 g0 sig0 formal needs map args Res Kind Body Context Sat Theta Signature Required
    |F0 C0 path0 sig0 handle slot Forward Placed]; subst.
  - destruct (named_function_plan_readback W P F C path signature target (TypeCapture args args)
      provisions) as (plan & TypedPlan & Source & Capture & Provisions).
    { destruct Body as (n & ps & rt & code & Ast). now exists target, n, ps, rt, code. }
    { exact Typed. }
    exists (NamedFunctionProvider plan).
    split; [exact TypedPlan|]. split; [exact I|].
    split; [exact Source|]. split; [exact Capture|exact Provisions].
  - exists (AbstractFunctionForwarder target handle).
    split; [exact Forward|]. split; [exact Placed|].
    repeat split; reflexivity.
Qed.

(* A wrapped method calls a declared function at its own context. A
   forwarder has a fresh slot, so it cannot supply that declared context. *)
Record wrapped_method_plan := WrappedMethodOf {
  wrapped_method_occurrence : nat;
  wrapped_method_provider : named_provider_plan
}.

Definition TypesWrappedMethodPlan (W : world) (F : frame) (C : cx)
    (path : path) (signature : fsig) (plan : wrapped_method_plan) : Prop :=
  TypesNamedFunctionPlan W F C path signature (wrapped_method_provider plan).

Definition wrapped_method_code (signature : fsig) (plan : wrapped_method_plan) : expr :=
  let provider := wrapped_method_provider plan in
  let formal := provider_formal provider in
  ECall (provider_symbol provider) (theta_ctx formal)
    (map (fun k => (rkey_of formal k, rkey_of formal k)) (provider_needs provider))
    (map EVar (seq 1 (length (sg_params signature)))).

Definition WrappedMethodPlacement (W : world) (P : program) (resolve : nat -> nat)
    (signature : fsig) (plan : wrapped_method_plan) : Prop :=
  let slot := resolve (wrapped_method_occurrence plan) in
  Fresh W slot /\ body_of P slot = Some (wrapped_method_code signature plan).

Local Lemma named_plan_context : forall W F C path signature plan,
  TypesNamedFunctionPlan W F C path signature plan ->
  CtxOf W (provider_symbol plan) (provider_formal plan) (provider_needs plan).
Proof.
  intros W F C path signature plan (g & _ & _ & _ & Typed).
  destruct Typed as [Source Context _ _ _ _]. now rewrite Source.
Qed.

Local Lemma context_at_symbol : forall W g C needs,
  CtxOf W g C needs -> w_context W g = Some (C, needs).
Proof.
  intros W g C needs (d & es & tm & er & tmr & subst & classes & _ & _ & _ & _ & _ & _ & _ & _ & Context).
  exact Context.
Qed.

Theorem wrapped_method_plan_translates : forall W P F C path signature receiver plan resolve,
  TypesWrappedMethodPlan W F C path signature plan ->
  WrappedMethodPlacement W P resolve signature plan ->
  MethProvider W P F C (AEPath path None) signature receiver (resolve (wrapped_method_occurrence plan))
    (named_provider_capture (wrapped_method_provider plan)) (fun body => body)
    (named_provider_provisions C (wrapped_method_provider plan)).
Proof.
  intros W P F C path signature receiver plan resolve Typed [Fresh Code].
  eapply MPWrapped.
  - eapply named_function_plan_translates. exact Typed.
  - eapply named_plan_context. exact Typed.
  - reflexivity.
  - exact Fresh.
  - exact Code.
Qed.

Theorem wrapped_method_plan_readback : forall W P F C path signature receiver target types prefix provisions,
  MethProvider W P F C (AEPath path None) signature receiver target types prefix provisions ->
  exists plan,
    TypesWrappedMethodPlan W F C path signature plan /\
    WrappedMethodPlacement W P (fun occurrence => occurrence) signature plan /\
    target = wrapped_method_occurrence plan /\
    types = named_provider_capture (wrapped_method_provider plan) /\
    prefix = (fun body => body) /\ provisions = named_provider_provisions C (wrapped_method_provider plan).
Proof.
  intros W P F C path signature receiver target types prefix provisions Typed.
  inversion Typed as [|F0 C0 path0 sg0 rt0 g sm0 w n formal needs ts Provider Context Arity FreshWrapper Body|]; subst.
  destruct (function_provider_plan_readback W P F C path signature g types provisions Provider)
    as ([named|occurrence handle] & ProviderTyped & Placed & Source & Capture & Provisions).
  - cbn [function_plan_target] in Source. subst g.
    pose proof (context_at_symbol _ _ _ _ Context) as ContextRead.
    pose proof (context_at_symbol _ _ _ _ (named_plan_context _ _ _ _ _ _ ProviderTyped)) as NamedRead.
    assert (SameFormal : formal = provider_formal named) by congruence.
    assert (SameNeeds : needs = provider_needs named) by congruence.
    subst formal needs.
    exists (WrappedMethodOf target named).
    split; [exact ProviderTyped|]. split; [split; [exact FreshWrapper|exact Body]|].
    split; [reflexivity|]. split; [exact Capture|]. split; [reflexivity|exact Provisions].
  - cbn [function_plan_target] in Source. subst g.
    destruct Placed as [ForwarderFresh _].
    destruct Context as (d & es & tm & er & tmr & subst & classes & Declared & _).
    unfold Fresh in ForwarderFresh. congruence.
Qed.
