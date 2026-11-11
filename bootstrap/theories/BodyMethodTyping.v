(* Method selection records the source provision and its type. Translation
   turns that provision into a runtime key, with any family application. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Inductive TypesMethodProvision (W : world) (C : cx) (receiver : ty) (method : nat)
    : option family_use -> fsig -> Prop :=
| TypesMethodIn : forall g,
    mlook receiver method (c_mths C) = Some g ->
    TypesMethodProvision W C receiver method None g
| TypesMethodFamily : forall u matching arguments formal needs result keys g,
    mlook receiver method (c_mths C) = None ->
    TypesFamilyApplication W C u matching arguments formal needs result keys ->
    FamilyReadOff W C u receiver method matching ->
    In (mkey receiver method) keys -> OnlyFamily W C u (mkey receiver method) ->
    mlook receiver method (c_mths result) = Some g -> MatchingInForce C matching ->
    TypesMethodProvision W C receiver method
      (Some (FamilyUse u matching arguments formal needs result keys)) g.

Definition emit_provided_method (C : cx) (receiver : ty) (method : nat)
    (choice : option family_use) (object : expr) (arguments : list expr) : expr :=
  match choice with
  | None => EMethod object method (rkey C receiver method) arguments
  | Some selected => translate_family_use C selected
      (EMethod object method (rkey (demand_result selected) receiver method) arguments)
  end.

Theorem provided_method_translation : forall W P F C receiver method choice signature syntax object arguments,
  TypesMethodProvision W C receiver method choice signature ->
  Args W P F C syntax (sg_params signature) arguments ->
  Provided W P F C object receiver method syntax
    (emit_provided_method C receiver method choice object arguments) (sg_ret signature).
Proof.
  intros W P F C receiver method choice signature syntax object arguments Typed Checked.
  destruct Typed; cbn [emit_provided_method demand_result]; [eapply PrIn|eapply PrFamily]; eassumption.
Qed.

Theorem provided_method_readback : forall W P F C object receiver method syntax code t,
  Provided W P F C object receiver method syntax code t ->
  exists choice signature arguments,
    TypesMethodProvision W C receiver method choice signature /\
    Args W P F C syntax (sg_params signature) arguments /\
    t = sg_ret signature /\ code = emit_provided_method C receiver method choice object arguments.
Proof.
  intros W P F C object receiver method syntax code t Typed.
  destruct Typed.
  - exists None, g, xs. split; [now constructor|auto].
  - exists (Some (FamilyUse u matching th formal needs C' ks)), g, xs.
    split; [now constructor|auto].
Qed.

Theorem literal_method_translation : forall W C t m choice x y,
  TypesMethodProvision W C t m choice (FSig [t] t) ->
  LitOp W C t m x y (emit_provided_method C t m choice x [y]).
Proof.
  intros W C t m choice x y Typed. inversion Typed; subst; cbn [emit_provided_method demand_result].
  - now apply LitOpIn.
  - eapply LitOpFamily; eassumption.
Qed.

Theorem literal_method_readback : forall W C t m x y code,
  LitOp W C t m x y code ->
  exists choice, TypesMethodProvision W C t m choice (FSig [t] t) /\
    code = emit_provided_method C t m choice x [y].
Proof.
  intros W C t m x y code Typed. destruct Typed.
  - exists None. split; [now constructor|reflexivity].
  - exists (Some (FamilyUse u matching th formal needs C' ks)).
    split; [now constructor|reflexivity].
Qed.

Inductive method_call_plan : Type :=
| SelectedProvision (method : nat) (family : option family_use)
| SelectedDefinition (method : nat) (formal : cx) (needs : list item)
    (types : subst) (demands : list family_use) (result : cx).

Inductive TypesMethodSelection (W : world) (F : frame) (C : cx) (receiver : ty)
    : path -> method_call_plan -> fsig -> Prop :=
| TypesMethodQualified : forall path m choice g,
    List.length path > 1 -> resolve_detached W (fr_mod F) path = Some m ->
    TypesMethodProvision W C receiver m choice g ->
    TypesMethodSelection W F C receiver path (SelectedProvision m choice) g
| TypesMethodProvisionWins : forall name h attached m choice g,
    head receiver = Some h -> Attached W h name attached ->
    w_scope W (fr_mod F) NSDetached name = Some (ESym m) ->
    mlook receiver m (c_mths C) <> None ->
    TypesMethodProvision W C receiver m choice g ->
    TypesMethodSelection W F C receiver [name] (SelectedProvision m choice) g
| TypesMethodAttachedDefined : forall name h attached formal needs types g demands result,
    head receiver = Some h -> Attached W h name attached ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached name = Some (ESym m) /\
      mlook receiver m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W attached = Some (ADFn (RAttached rn) name ps rt (Some b))) ->
    let receiver_types := match receiver with TNom _ rows => rows | _ => [] end in
    let caller := family_context C receiver_types in
    CtxOf W attached formal needs ->
    Demand W caller (map (at_th caller types) needs) [] caller demands result ->
    NeedsSatisfied result formal types needs -> CallTheta W result formal attached types ->
    SigOf W attached (c_ty result) (Some receiver) g ->
    RequirementsInForce caller formal ->
    TypesMethodSelection W F C receiver [name]
      (SelectedDefinition attached formal needs types demands result) g
| TypesMethodAttachedAbstract : forall name h attached choice g,
    head receiver = Some h -> Attached W h name attached ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached name = Some (ESym m) /\
      mlook receiver m (c_mths C) <> None) ->
    (exists rn ps rt, ast_of W attached = Some (ADFn (RAttached rn) name ps rt None)) ->
    TypesMethodProvision W C receiver attached choice g ->
    TypesMethodSelection W F C receiver [name] (SelectedProvision attached choice) g
| TypesMethodDetached : forall name m choice g,
    (forall h attached, head receiver = Some h -> ~ Attached W h name attached) ->
    w_scope W (fr_mod F) NSDetached name = Some (ESym m) ->
    TypesMethodProvision W C receiver m choice g ->
    TypesMethodSelection W F C receiver [name] (SelectedProvision m choice) g.

Definition emit_method_call (C : cx) (receiver : ty) (selected : method_call_plan)
    (object : expr) (arguments : list expr) : expr :=
  match selected with
  | SelectedProvision method choice => emit_provided_method C receiver method choice object arguments
  | SelectedDefinition method formal needs types demands result =>
      let receiver_types := match receiver with TNom _ rows => rows | _ => [] end in
      translate_demands (family_context C receiver_types) demands
        (translate_call result formal method types needs (object :: arguments))
  end.

Theorem method_call_translation : forall W P F C receiver path selected signature syntax object arguments,
  TypesMethodSelection W F C receiver path selected signature ->
  Args W P F C syntax (sg_params signature) arguments ->
  MethCall W P F C object receiver path syntax (emit_method_call C receiver selected object arguments)
    (sg_ret signature).
Proof.
  intros W P F C receiver path selected signature syntax object arguments Typed Checked.
  destruct Typed; cbn [emit_method_call].
  - eapply MCQualified; [eassumption|eassumption|eapply provided_method_translation; eassumption].
  - eapply MCProvisionWins; try eassumption. eapply provided_method_translation; eassumption.
  - eapply MCAttachedDefined; try eassumption. reflexivity.
  - eapply MCAttachedAbstract; try eassumption. eapply provided_method_translation; eassumption.
  - eapply MCDetached; try eassumption. eapply provided_method_translation; eassumption.
Qed.

Theorem method_call_readback : forall W P F C object receiver path syntax code t,
  MethCall W P F C object receiver path syntax code t ->
  exists selected signature arguments,
    TypesMethodSelection W F C receiver path selected signature /\
    Args W P F C syntax (sg_params signature) arguments /\
    t = sg_ret signature /\ code = emit_method_call C receiver selected object arguments.
Proof.
  intros W P F C object receiver path syntax code t Typed. destruct Typed.
  all: try match goal with
  | Read : Provided _ _ _ _ _ _ ?m _ _ _ |- _ =>
      destruct (provided_method_readback _ _ _ _ _ _ _ _ _ _ Read)
        as (choice & signature & values & Provision & Checked & -> & ->);
      exists (SelectedProvision m choice), signature, values;
      split; [econstructor; eassumption|auto]
  end.
  subst rargs. exists (SelectedDefinition att Ca ns th demands Cd), g, xs.
  split; [econstructor; eassumption|auto].
Qed.
