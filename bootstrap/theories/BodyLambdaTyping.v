(* Lambda typing chooses capture identities and a source body plan. The
   occurrence token determines where translation places the body and origin. *)
From Stdlib Require Import List.
From NM Require Import Audit.
From NM Require Import LegacyBodyJudgments.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Local Open Scope list_scope.

Definition lambda_descriptor_prefix {BodyPlan : Type} (F : frame) (descriptor : lambda_body_descriptor BodyPlan) : expr -> expr :=
  translate_lambda_captures F (lambda_descriptor_captures descriptor).

Definition LambdaBodyPlacement {BodyPlan : Type} (W : world) (P : program) (C : cx) (Fl : frame)
    (resolve : nat -> nat) (descriptor : lambda_body_descriptor BodyPlan) (emitted : expr) : Prop :=
  let slot := resolve (lambda_occurrence descriptor) in
  Fresh W slot /\ body_of P slot = Some (lambda_descriptor_code Fl descriptor emitted) /\
  nlook slot (lambda_sections (program_origins P)) = Some (c_sub C).

Section LambdaSource.
Variable BodyPlan : Type.
Variable types_body : world -> frame -> cx -> ablock -> option ty -> BodyPlan -> ty -> Prop.

Variable emits_body : frame -> cx -> BodyPlan -> expr -> Prop.

Theorem lambda_provider_plan_translates :
  (forall W P F C syntax expected plan t emitted,
    types_body W F C syntax expected plan t -> emits_body F C plan emitted ->
    BlkE W P F C syntax expected emitted t) ->
  forall W P F C ps rt body signature this Fl descriptor result emitted resolve,
  TypesLambdaHeader W F C ps rt body signature this Fl descriptor result ->
  types_body W Fl (lambda_descriptor_site signature ps descriptor) (ABlock [] (Some body))
    (Some (sg_ret signature)) (lambda_source_body descriptor) result ->
  Numbering Fl this (map fst ps) ->
  emits_body Fl (lambda_descriptor_site signature ps descriptor) (lambda_source_body descriptor) emitted ->
  LambdaBodyPlacement W P C Fl resolve descriptor emitted ->
  LambdaProvider W P F C ps rt body signature this (resolve (lambda_occurrence descriptor))
    (lambda_descriptor_prefix F descriptor) (lambda_descriptor_needs C descriptor).
Proof.
  intros Emits W P F C ps rt body signature this Fl descriptor result emitted resolve Typed Body Numbered Emitted Placed.
  destruct Typed as [caps capvals Cl Length Distinct Annotations Return Capture Context
    BodyModule Ret This Loop Keep NamedBody NamedOuter Fits Needs Unique].
  destruct Placed as [Fresh [Code Origin]].
  pose proof (Emits _ P _ _ _ _ _ _ _ Body Emitted) as Translated.
  eapply (@LP W P F C ps rt body signature this _ caps capvals Cl Fl emitted result
    (lambda_provision_needs descriptor) (lambda_descriptor_needs C descriptor)); try eassumption; reflexivity.
Qed.

Theorem lambda_provider_plan_readback :
  (forall W P F C syntax expected emitted t,
    BlkE W P F C syntax expected emitted t ->
    exists plan, types_body W F C syntax expected plan t /\ emits_body F C plan emitted) ->
  forall W P F C ps rt body signature this slot prefix sm,
  LambdaProvider W P F C ps rt body signature this slot prefix sm ->
  exists Fl descriptor result emitted,
    lambda_occurrence descriptor = slot /\
    TypesLambdaHeader W F C ps rt body signature this Fl descriptor result /\
    types_body W Fl (lambda_descriptor_site signature ps descriptor) (ABlock [] (Some body))
      (Some (sg_ret signature)) (lambda_source_body descriptor) result /\
    Numbering Fl this (map fst ps) /\
    emits_body Fl (lambda_descriptor_site signature ps descriptor) (lambda_source_body descriptor) emitted /\
    LambdaBodyPlacement W P C Fl (fun occurrence => occurrence) descriptor emitted /\
    prefix = lambda_descriptor_prefix F descriptor /\ sm = lambda_descriptor_needs C descriptor.
Proof.
  intros ReadBody W P F C ps rt body signature this slot prefix sm Typed.
  destruct Typed as [F C ps rt body signature this slot caps capvals Cl Fl emitted body_type keys sm
    Length Distinct Annotations Return Capture Fresh Context BodyModule Ret This Loop Keep Numbered
    NamedBody NamedOuter Body Fits Code Needs Unique NeedsMap Origin].
  destruct (ReadBody _ _ _ _ _ _ _ _ Body) as (plan & BodyPlanTyped & Emitted).
  exists Fl, (LambdaBodyOf slot caps capvals Cl plan keys), body_type, emitted.
  split; [reflexivity|]. split.
  - eapply TLambdaHeader; cbn beta zeta in *; eassumption.
  - split; [exact BodyPlanTyped|]. split; [exact Numbered|]. split; [exact Emitted|]. split.
    + split; [exact Fresh|]. split; [exact Code|exact Origin].
    + split; [reflexivity|exact NeedsMap].
Qed.
End LambdaSource.
