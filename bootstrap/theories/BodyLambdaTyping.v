(* Lambda typing chooses capture identities and a source body plan. The
   occurrence token determines where translation places the body and origin. *)
From Stdlib Require Import List.
From NM Require Import Audit.
From NM Require EaBind.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Import EaBind.
Local Open Scope list_scope.

Record lambda_body_descriptor (BodyPlan : Type) := LambdaBodyOf {
  lambda_occurrence : nat;
  lambda_plan_capture_names : list name;
  lambda_plan_capture_symbols : list nat;
  lambda_plan_capture_context : cx;
  lambda_source_body : BodyPlan;
  lambda_provision_needs : list item
}.

Arguments LambdaBodyOf {BodyPlan}.
Arguments lambda_occurrence {BodyPlan}.
Arguments lambda_plan_capture_names {BodyPlan}.
Arguments lambda_plan_capture_symbols {BodyPlan}.
Arguments lambda_plan_capture_context {BodyPlan}.
Arguments lambda_source_body {BodyPlan}.
Arguments lambda_provision_needs {BodyPlan}.

Section LambdaSource.
Variable BodyPlan : Type.
Variable types_body : world -> frame -> cx -> ablock -> option ty -> BodyPlan -> ty -> Prop.

Definition lambda_descriptor_site (signature : fsig) (ps : list (name * option aty))
    (descriptor : lambda_body_descriptor BodyPlan) : cx :=
  lambda_parameter_context (lambda_plan_capture_context descriptor) (map fst ps) (sg_params signature).

Inductive TypesLambdaProvider (W : world) (F : frame) (C : cx)
    (ps : list (name * option aty)) (rt : option aty) (body : aexpr) (signature : fsig)
    (this : option ty) (Fl : frame) (descriptor : lambda_body_descriptor BodyPlan) : Prop :=
| TLP : forall body_type,
    let caps := lambda_plan_capture_names descriptor in
    let capvals := lambda_plan_capture_symbols descriptor in
    let Cl := lambda_plan_capture_context descriptor in
    length ps = length (sg_params signature) -> NoDup (map fst ps) ->
    Forall2 (fun parameter t =>
      match snd parameter with
      | Some annotation => exists read,
          TyOf W (fr_mod F) (c_ty C) this annotation read /\ MergeEq (c_merge C) read t
      | None => True
      end) ps (sg_params signature) ->
    (match rt with
     | Some annotation => exists read,
         TyOf W (fr_mod F) (c_ty C) this annotation read /\ MergeEq (c_merge C) read (sg_ret signature)
     | None => True
     end) ->
    TypesLambdaCaptures W F C (map fst ps) body caps capvals ->
    Cl = captured_cx C caps capvals ->
    fr_mod Fl = fr_mod F -> fr_ret Fl = Some (sg_ret signature) -> fr_this Fl = this -> fr_loop Fl = false ->
    incl (fr_fresh F ++ context_gens C) (fr_fresh Fl) ->
    Forall (Named Fl) caps -> Forall (Named F) caps ->
    types_body W Fl (lambda_descriptor_site signature ps descriptor)
      (ABlock [] (Some body)) (Some (sg_ret signature)) (lambda_source_body descriptor) body_type ->
    fits body_type (sg_ret signature) ->
    (forall k, In k (lambda_provision_needs descriptor) <->
      provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = provision_id k) ->
    NoDup (lambda_provision_needs descriptor) ->
    TypesLambdaProvider W F C ps rt body signature this Fl descriptor.

Definition lambda_descriptor_captures (descriptor : lambda_body_descriptor BodyPlan) : list (name * nat) :=
  combine (lambda_plan_capture_names descriptor) (lambda_plan_capture_symbols descriptor).

Definition lambda_descriptor_prefix (F : frame) (descriptor : lambda_body_descriptor BodyPlan) : expr -> expr :=
  translate_lambda_captures F (lambda_descriptor_captures descriptor).

Definition lambda_descriptor_needs (C : cx) (descriptor : lambda_body_descriptor BodyPlan) : smap :=
  map (fun k => (rkey_of (lambda_plan_capture_context descriptor) k, rkey_of C k))
    (lambda_provision_needs descriptor).

Definition lambda_descriptor_code (Fl : frame) (descriptor : lambda_body_descriptor BodyPlan)
    (emitted : expr) : expr :=
  translate_lambda_body Fl (lambda_descriptor_captures descriptor) emitted.

Definition LambdaBodyPlacement (W : world) (P : program) (C : cx) (Fl : frame)
    (resolve : nat -> nat) (descriptor : lambda_body_descriptor BodyPlan) (emitted : expr) : Prop :=
  let slot := resolve (lambda_occurrence descriptor) in
  Fresh W slot /\ body_of P slot = Some (lambda_descriptor_code Fl descriptor emitted) /\
  nlook slot (lambda_sections (program_origins P)) = Some (c_sub C).

Variable emits_body : frame -> cx -> BodyPlan -> expr -> Prop.

Theorem lambda_provider_plan_translates :
  (forall W P F C syntax expected plan t emitted,
    types_body W F C syntax expected plan t -> emits_body F C plan emitted ->
    BlkE W P F C syntax expected emitted t) ->
  forall W P F C ps rt body signature this Fl descriptor emitted resolve,
  TypesLambdaProvider W F C ps rt body signature this Fl descriptor ->
  Numbering Fl this (map fst ps) ->
  emits_body Fl (lambda_descriptor_site signature ps descriptor) (lambda_source_body descriptor) emitted ->
  LambdaBodyPlacement W P C Fl resolve descriptor emitted ->
  LambdaProvider W P F C ps rt body signature this (resolve (lambda_occurrence descriptor))
    (lambda_descriptor_prefix F descriptor) (lambda_descriptor_needs C descriptor).
Proof.
  intros Emits W P F C ps rt body signature this Fl descriptor emitted resolve Typed Numbered Emitted Placed.
  destruct Typed as [body_type caps capvals Cl Length Distinct Annotations Return Capture Context
    BodyModule Ret This Loop Keep NamedBody NamedOuter Body Fits Needs Unique].
  destruct Placed as [Fresh [Code Origin]].
  pose proof (Emits _ P _ _ _ _ _ _ _ Body Emitted) as Translated.
  eapply (@LP W P F C ps rt body signature this _ caps capvals Cl Fl emitted body_type
    (lambda_provision_needs descriptor) (lambda_descriptor_needs C descriptor)); try eassumption; reflexivity.
Qed.

Theorem lambda_provider_plan_readback :
  (forall W P F C syntax expected emitted t,
    BlkE W P F C syntax expected emitted t ->
    exists plan, types_body W F C syntax expected plan t /\ emits_body F C plan emitted) ->
  forall W P F C ps rt body signature this slot prefix sm,
  LambdaProvider W P F C ps rt body signature this slot prefix sm ->
  exists Fl descriptor emitted,
    lambda_occurrence descriptor = slot /\
    TypesLambdaProvider W F C ps rt body signature this Fl descriptor /\
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
  exists Fl, (LambdaBodyOf slot caps capvals Cl plan keys), emitted.
  split; [reflexivity|]. split.
  - eapply TLP; cbn beta zeta in *; eassumption.
  - split; [exact Numbered|]. split; [exact Emitted|]. split.
    + split; [exact Fresh|]. split; [exact Code|exact Origin].
    + split; [reflexivity|exact NeedsMap].
Qed.
End LambdaSource.
