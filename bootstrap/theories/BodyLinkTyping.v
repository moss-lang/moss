(* A linked body chooses its source scopes, binds, outputs and pins before
   translation chooses a program slot. The occurrence token distinguishes
   separate placements of the same source pair. *)
From Stdlib Require Import List.
From NM Require Import Audit.
From NM Require Import LegacyBodyJudgments.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Local Open Scope list_scope.

Lemma spliced_link_bindings : forall bindings,
  BindsOf (splice_link_bindings bindings EUnit) bindings.
Proof. induction bindings as [|binding rest IH]; cbn [splice_link_bindings fold_right]; constructor; exact IH. Qed.

Section LinkedSource.
Variable BindPlan : Type.
Variable types_binds : world -> frame -> cx -> list (spec * option aexpr) ->
  BindPlan -> cx -> frame -> list nat -> Prop.
Variable family_binds : world -> BindPlan -> Prop.

Variable emits_binds : frame -> cx -> BindPlan -> list binding -> Prop.

Theorem linked_body_plan_translates :
  (forall W P F C items plan out F' made bindings,
    types_binds W F C items plan out F' made -> emits_binds F C plan bindings ->
    BindItems W P F C items (splice_link_bindings bindings) out F' made) ->
  (forall W F C plan bindings,
    family_binds W plan -> emits_binds F C plan bindings -> FamilyBindings W bindings) ->
  forall W P Fg descriptor items result after made bindings resolve,
  TypesLinkedHeader W Fg descriptor items result made ->
  types_binds W Fg (linked_provider_context descriptor) items (linked_bind_plan descriptor) result after made ->
  (Family W (linked_target_symbol (linked_handle descriptor)) ->
    family_binds W (linked_bind_plan descriptor)) ->
  Numbering Fg None [] ->
  emits_binds Fg (linked_provider_context descriptor) (linked_bind_plan descriptor) bindings ->
  LinkedBodyPlacement W P resolve descriptor bindings ->
  LinkBody W P (linked_target_symbol (linked_handle descriptor))
    (linked_provider_symbol (linked_handle descriptor)) (resolve (linked_occurrence (linked_handle descriptor)))
    (link_plan_pins (linked_result_reading descriptor)).
Proof.
  intros Emits FamilyEmits W P Fg descriptor items result after made bindings resolve Typed Binds FamilyAllowed Numbered Emitted Placed.
  destruct Typed as [dg name parameters inputs results bodies target_needs provider_needs Cb created gen promised
    target provider Cu Cg outs reading Declared Ast Target Provider BodyModule Ret This Loop Protected
    NoInput Outputs Result].
  destruct Result as [_ Site PinsProvision ResultTypes PinsExact PinsScoped].
  pose proof (Emits _ P _ _ _ _ _ _ _ _ Binds Emitted) as Body.
  assert (FamilyBindingsOk : Family W target -> FamilyBindings W bindings).
  { intro IsFamily. eapply FamilyEmits; [exact (FamilyAllowed IsFamily)|exact Emitted]. }
  assert (Translated : TranslatesLinkedBody W P target provider
    (resolve (linked_occurrence (linked_handle descriptor))) Cu Cg (link_plan_site reading)
    outs bindings (link_plan_keys reading)).
  { exact Placed. }
  rewrite Site in PinsProvision, ResultTypes, Translated.
  eapply (@LB W P target provider _ dg name parameters inputs results bodies Cu target_needs Cg provider_needs
    Fg (splice_link_bindings bindings) Cb after created outs gen promised (link_plan_keys reading) bindings
    (link_plan_pins reading)); try eassumption.
  apply spliced_link_bindings.
Qed.

Theorem linked_body_plan_readback :
  (forall W P F C items wrap out F' made bindings,
    BindItems W P F C items wrap out F' made -> BindsOf (wrap EUnit) bindings ->
    exists plan, types_binds W F C items plan out F' made /\ emits_binds F C plan bindings) ->
  (forall W F C plan bindings,
    FamilyBindings W bindings -> emits_binds F C plan bindings -> family_binds W plan) ->
  forall W P target provider slot pins,
  LinkBody W P target provider slot pins ->
  exists Fg descriptor items result after made bindings,
    linked_handle descriptor = LinkedBodyOf target provider slot /\
    link_plan_pins (linked_result_reading descriptor) = pins /\
    TypesLinkedHeader W Fg descriptor items result made /\
    types_binds W Fg (linked_provider_context descriptor) items (linked_bind_plan descriptor) result after made /\
    (Family W target -> family_binds W (linked_bind_plan descriptor)) /\
    Numbering Fg None [] /\
    emits_binds Fg (linked_provider_context descriptor) (linked_bind_plan descriptor) bindings /\
    LinkedBodyPlacement W P (fun occurrence => occurrence) descriptor bindings.
Proof.
  intros ReadBinds FamilyRead W P target provider slot pins Linked.
  destruct Linked as [target provider slot dg name parameters inputs results bodies Cu target_needs Cg provider_needs
    Fg wrap Cb Fb made outs gen promised keys bindings pins Declared Ast Target Provider BodyModule Ret This Loop
    Numbered Protected Body NoInput Outputs PinsProvision ResultTypes Bindings FamilyAllowed Translated PinsExact PinsScoped].
  destruct (ReadBinds _ _ _ _ _ _ _ _ _ _ Body Bindings) as (plan & Typed & Emitted).
  exists Fg, (LinkedBodyDescriptor (LinkedBodyOf target provider slot) Cu Cg outs
    (LinkedResultPlan pins (link_site Cg Cb pins outs gen) keys) plan), (List.concat bodies), Cb, Fb, made, bindings.
  split; [reflexivity|]. split; [reflexivity|]. split.
  - eapply TLinkedHeader; cbn beta zeta in *; try eassumption.
    constructor; cbn [link_plan_keys link_plan_site link_plan_pins]; try eassumption; reflexivity.
  - split; [exact Typed|]. split.
    + intro IsFamily. eapply FamilyRead; [exact (FamilyAllowed IsFamily)|exact Emitted].
    + split; [exact Numbered|]. split; [exact Emitted|exact Translated].
Qed.
End LinkedSource.
