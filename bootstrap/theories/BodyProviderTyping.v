(* Source typing for direct provider binds. A named or attached provider
   uses its declared body, so these bindings need no generated placement. *)
From Stdlib Require Import List.
From NM Require Import Audit.
From NM Require Import LegacyBodyJudgments.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Local Open Scope list_scope.

Lemma named_function_plan_translates : forall W P F C path signature plan,
  TypesNamedFunctionPlan W F C path signature plan ->
  FnProvider W P F C (AEPath path None) signature None
    (provider_symbol plan) (named_provider_capture plan) (named_provider_provisions C plan).
Proof.
  intros W P F C path signature plan (g & Resolved & Kind & Body & Typed).
  destruct Typed as [Source Context Needs Arguments Signature Required].
  rewrite Source. unfold named_provider_capture, named_provider_provisions.
  eapply FPDefined; [exact Resolved|exact Kind|exact Body|exact Context|split; [reflexivity|exact Needs]|
    exact Arguments|exact Signature|exact Required].
Qed.

Lemma attached_method_plan_translates : forall W P F C path name receiver signature site plan,
  TypesAttachedMethodPlan W F C path name receiver signature site plan ->
  MethProvider W P F C (AEField (AEPath path None) name) signature receiver
    (provider_symbol plan) (named_provider_capture plan) (fun body => body) (named_provider_provisions site plan).
Proof.
  intros W P F C path name receiver signature site plan
    (root & arguments & method & Resolved & Attached & Body & Receiver & Site & Typed).
  destruct Typed as [Source Context Needs Arguments Signature Required].
  rewrite Source. unfold named_provider_capture, named_provider_provisions.
  eapply MPAttached; [exact Resolved|exact Attached|exact Body|exact Receiver|exact Site|exact Context|
    split; [reflexivity|exact Needs]|exact Arguments|exact Signature|exact Required].
Qed.

Theorem named_function_plan_readback : forall W P F C path signature g types sm,
  (exists declared name parameters result body,
    resolve_sym W (fr_mod F) path = Some declared /\
    ast_of W declared = Some (ADFn RPlain name parameters result (Some body))) ->
  FnProvider W P F C (AEPath path None) signature None g types sm ->
  exists plan, TypesNamedFunctionPlan W F C path signature plan /\
    g = provider_symbol plan /\ types = named_provider_capture plan /\ sm = named_provider_provisions C plan.
Proof.
  intros W P F C path signature g types sm Defined Typed.
  inversion Typed as [F0 C0 path0 g0 sig0 formal needs map args Res Kind Body Context Sat Theta Signature Required
    |F0 C0 path0 sig0 handle slot Forward Placed]; subst.
  - exists (NamedProviderPlan g formal needs args).
    destruct Sat as [Map Needs]. split.
    + exists g. split; [exact Res|]. split; [exact Kind|]. split; [exact Body|].
      constructor; cbn [provider_symbol provider_formal provider_needs provider_arguments]; auto.
    + cbn [provider_symbol named_provider_capture named_provider_provisions provider_arguments provider_formal provider_needs].
      auto.
  - destruct Defined as (declared & name & parameters & result & body & Resolved & Ast).
    destruct Forward as (Forwarded & _ & (name' & ps' & rt' & Abstract) & _).
    assert (Same : declared = forwarder_source handle) by congruence.
    now rewrite <- Same, Ast in Abstract.
Qed.

Theorem attached_method_plan_readback : forall W P F C path name receiver signature g types prefix sm,
  MethProvider W P F C (AEField (AEPath path None) name) signature receiver g types prefix sm ->
  exists site plan, TypesAttachedMethodPlan W F C path name receiver signature site plan /\
    g = provider_symbol plan /\ types = named_provider_capture plan /\
    prefix = (fun body => body) /\ sm = named_provider_provisions site plan.
Proof.
  intros W P F C path name receiver signature g types prefix sm Typed.
  inversion Typed as [F0 C0 path0 name0 root att rt0 sg0 formal needs map args rargs site
    Res Attached Body Receiver Site Context Sat Theta Signature Required| |]; subst.
  exists (family_context C rargs), (NamedProviderPlan g formal needs args).
  destruct Sat as [Map Needs]. split.
  - exists root, rargs, g.
    split; [exact Res|]. split; [exact Attached|]. split; [exact Body|].
    split; [exact Receiver|]. split; [reflexivity|].
    constructor; cbn [provider_symbol provider_formal provider_needs provider_arguments]; auto.
  - cbn [provider_symbol named_provider_capture named_provider_provisions provider_arguments provider_formal provider_needs].
    auto.
Qed.
