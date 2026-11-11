(* A linked body chooses its source scopes, binds, outputs and pins before
   translation chooses a program slot. The occurrence token distinguishes
   separate placements of the same source pair. *)
From Stdlib Require Import List.
From NM Require Import Audit.
From NM Require EaFunctor EaLinkBodySound.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Import EaFunctor EaLinkBodySound.
Local Open Scope list_scope.

Record linked_body_handle := LinkedBodyOf {
  linked_target_symbol : nat;
  linked_provider_symbol : nat;
  linked_occurrence : nat
}.

Record linked_body_descriptor (BindPlan : Type) := LinkedBodyDescriptor {
  linked_handle : linked_body_handle;
  linked_target_context : cx;
  linked_provider_context : cx;
  linked_output_roles : list nat;
  linked_result_reading : linked_result_plan;
  linked_bind_plan : BindPlan
}.

Arguments LinkedBodyDescriptor {BindPlan}.
Arguments linked_handle {BindPlan}.
Arguments linked_target_context {BindPlan}.
Arguments linked_provider_context {BindPlan}.
Arguments linked_output_roles {BindPlan}.
Arguments linked_result_reading {BindPlan}.
Arguments linked_bind_plan {BindPlan}.

Definition splice_link_bindings (bindings : list binding) : expr -> expr :=
  fun body => fold_right EBind body bindings.

Lemma spliced_link_bindings : forall bindings,
  BindsOf (splice_link_bindings bindings EUnit) bindings.
Proof. induction bindings as [|binding rest IH]; cbn [splice_link_bindings fold_right]; constructor; exact IH. Qed.

Section LinkedSource.
Variable BindPlan : Type.
Variable types_binds : world -> frame -> cx -> list (spec * option aexpr) ->
  BindPlan -> cx -> frame -> list nat -> Prop.
Variable family_binds : world -> BindPlan -> Prop.

Inductive TypesLinkedBody (W : world) (Fg : frame) (descriptor : linked_body_descriptor BindPlan) : Prop :=
| TLB : forall dg name parameters inputs results bodies target_needs provider_needs Cb Fb made gen promised,
    let target := linked_target_symbol (linked_handle descriptor) in
    let provider := linked_provider_symbol (linked_handle descriptor) in
    let Cu := linked_target_context descriptor in
    let Cg := linked_provider_context descriptor in
    let outs := linked_output_roles descriptor in
    let reading := linked_result_reading descriptor in
    let plan := linked_bind_plan descriptor in
    w_decl W provider = Some dg -> d_ast dg = ADFunctor name parameters inputs results (Some bodies) ->
    FunCtx W target Cu target_needs -> FunCtx W provider Cg provider_needs ->
    fr_mod Fg = d_mod dg -> fr_ret Fg = None -> fr_this Fg = None -> fr_loop Fg = false ->
    (forall x, In x (fr_pins Fg) <-> Mentions W target x /\ Poly W target x) ->
    types_binds W Fg Cg (concat bodies) plan Cb Fb made ->
    (forall t, In t made -> nlook t (c_ty Cu) = None) ->
    TypesLinkOutputs W target Cu Cb outs gen promised (link_plan_keys reading) ->
    TypesLinkedResult W target Cu Cg Cb promised made outs gen (link_plan_keys reading) reading ->
    (Family W target -> family_binds W plan) ->
    TypesLinkedBody W Fg descriptor.

Definition linked_descriptor_code (descriptor : linked_body_descriptor BindPlan) (bindings : list binding)
    : functor_code :=
  let reading := linked_result_reading descriptor in
  let site := link_plan_site reading in
  translate_linked_body (linked_target_context descriptor) (linked_provider_context descriptor)
    (linked_output_roles descriptor) bindings (map (fun key => rkey_of site (at_site site key)) (link_plan_keys reading)).

Definition LinkedBodyPlacement (W : world) (P : program) (resolve : nat -> nat)
    (descriptor : linked_body_descriptor BindPlan) (bindings : list binding) : Prop :=
  let handle := linked_handle descriptor in
  let slot := resolve (linked_occurrence handle) in
  Fresh W slot /\
  funbody_of P slot = Some (linked_descriptor_code descriptor bindings) /\
  nlook slot (linked_origins (program_origins P)) =
    Some (linked_target_symbol handle, linked_provider_symbol handle).

Variable emits_binds : frame -> cx -> BindPlan -> list binding -> Prop.

Theorem linked_body_plan_translates :
  (forall W P F C items plan out F' made bindings,
    types_binds W F C items plan out F' made -> emits_binds F C plan bindings ->
    BindItems W P F C items (splice_link_bindings bindings) out F' made) ->
  (forall W F C plan bindings,
    family_binds W plan -> emits_binds F C plan bindings -> FamilyBindings W bindings) ->
  forall W P Fg descriptor bindings resolve,
  TypesLinkedBody W Fg descriptor ->
  Numbering Fg None [] ->
  emits_binds Fg (linked_provider_context descriptor) (linked_bind_plan descriptor) bindings ->
  LinkedBodyPlacement W P resolve descriptor bindings ->
  LinkBody W P (linked_target_symbol (linked_handle descriptor))
    (linked_provider_symbol (linked_handle descriptor)) (resolve (linked_occurrence (linked_handle descriptor)))
    (link_plan_pins (linked_result_reading descriptor)).
Proof.
  intros Emits FamilyEmits W P Fg descriptor bindings resolve Typed Numbered Emitted Placed.
  destruct Typed as [dg name parameters inputs results bodies target_needs provider_needs Cb Fb made gen promised
    target provider Cu Cg outs reading plan Declared Ast Target Provider BodyModule Ret This Loop Protected
    Binds NoInput Outputs Result FamilyAllowed].
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
    Fg (splice_link_bindings bindings) Cb Fb made outs gen promised (link_plan_keys reading) bindings
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
  exists Fg descriptor bindings,
    linked_handle descriptor = LinkedBodyOf target provider slot /\
    link_plan_pins (linked_result_reading descriptor) = pins /\
    TypesLinkedBody W Fg descriptor /\ Numbering Fg None [] /\
    emits_binds Fg (linked_provider_context descriptor) (linked_bind_plan descriptor) bindings /\
    LinkedBodyPlacement W P (fun occurrence => occurrence) descriptor bindings.
Proof.
  intros ReadBinds FamilyRead W P target provider slot pins Linked.
  destruct Linked as [target provider slot dg name parameters inputs results bodies Cu target_needs Cg provider_needs
    Fg wrap Cb Fb made outs gen promised keys bindings pins Declared Ast Target Provider BodyModule Ret This Loop
    Numbered Protected Body NoInput Outputs PinsProvision ResultTypes Bindings FamilyAllowed Translated PinsExact PinsScoped].
  destruct (ReadBinds _ _ _ _ _ _ _ _ _ _ Body Bindings) as (plan & Typed & Emitted).
  exists Fg, (LinkedBodyDescriptor (LinkedBodyOf target provider slot) Cu Cg outs
    (LinkedResultPlan pins (link_site Cg Cb pins outs gen) keys) plan), bindings.
  split; [reflexivity|]. split; [reflexivity|]. split.
  - eapply TLB; cbn beta zeta in *; try eassumption.
    + constructor; cbn [link_plan_keys link_plan_site link_plan_pins]; try eassumption; reflexivity.
    + intro IsFamily. eapply FamilyRead; [exact (FamilyAllowed IsFamily)|exact Emitted].
  - split; [exact Numbered|]. split; [exact Emitted|exact Translated].
Qed.
End LinkedSource.
