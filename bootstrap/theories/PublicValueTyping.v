(* Ground data/heap typing for the public semantics. The ghost heap stores
   immutable held types; this judgment deliberately does not read mutable
   runtime source-name bindings. Callable/environment typing and its link
   to all source derivations remain separate obligations. *)
From Stdlib Require Import List Bool String Arith.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition heap_types := address -> option (bool * ty).
Definition heap_extends (old new : heap_types) : Prop :=
  forall address cell, old address = Some cell -> new address = Some cell.

Definition type_argument (arguments : subst) (symbol : tsym) : ty :=
  match lookup_subst symbol arguments with Some meaning => meaning | None => TAbs symbol end.
Definition cast_enabled (schema : cast_spec) (arguments : subst) : bool :=
  match cast_handle schema with
  | None => true
  | Some (symbol, expected) => ty_eqb (type_argument arguments symbol) expected
  end.
Definition ordinary_tag (P : program_view) (tag : nsym) (arguments : subst) : Prop :=
  match lookup_cast tag (view_casts P) with
  | None => True
  | Some schema => cast_enabled schema arguments = false
  end.

Inductive handle_type : bool -> ty -> Prop :=
| StructHandle : handle_type false (TMachine MStructRef)
| ArrayHandle : handle_type true (TMachine MArrayRef)
| HandleUnion : forall kind member members,
    handle_type kind member -> In member members -> handle_type kind (TUnion members).

(* Bare handles occur at the host boundary. Checked handles additionally
   retain the allocation's kind and held-type header through unpacking. *)
Inductive handle_typed (H : heap_types) (kind : bool) (held : ty) : value -> Prop :=
| RawHandle : forall address,
    H address = Some (kind, held) -> handle_typed H kind held (VHandle address)
| CheckedHandle : forall address,
    H address = Some (kind, held) -> handle_typed H kind held (VCheckedHandle kind held address).

Lemma handle_typed_extends : forall H H' kind held value,
  heap_extends H H' -> handle_typed H kind held value -> handle_typed H' kind held value.
Proof. intros H H' kind held value Extend Typed; destruct Typed; constructor; eauto. Qed.

Section Values.
Variables (W : A.world) (P : program_view).

Inductive value_typed (H : heap_types) : value -> ty -> Prop :=
| TypedUnit : value_typed H VUnit TUnit
| TypedI32 : forall bits, value_typed H (VI32 bits) (TMachine MI32)
| TypedI64 : forall bits, value_typed H (VI64 bits) (TMachine MI64)
| TypedF32 : forall bits, value_typed H (VF32 bits) (TMachine MF32)
| TypedF64 : forall bits, value_typed H (VF64 bits) (TMachine MF64)
| TypedRecord : forall values fields,
    fields_typed H values fields -> value_typed H (VRecord values) (TRecord fields)
| TypedTag : forall tag arguments payload value,
    ordinary_tag P tag arguments -> A.PayloadOf W tag arguments payload ->
    value_typed H value payload -> value_typed H (VTag tag value) (TNom tag arguments)
(* A unit declaration (`tag True;`) has no payload; ExUnitTag builds it
   with no type arguments. *)
| TypedUnitTag : forall tag,
    A.kind_sym W tag = Some A.DKUnit -> value_typed H (VTag tag VUnit) (TNom tag [])
| TypedUnion : forall value member members,
    value_typed H value member -> In member members -> value_typed H value (TUnion members)
| TypedRawHandle : forall kind held value,
    handle_typed H kind held value ->
    value_typed H value (TMachine (if kind then MArrayRef else MStructRef))
| TypedHeapTag : forall tag arguments schema held payload value,
    lookup_cast tag (view_casts P) = Some schema -> cast_enabled schema arguments = true ->
    lookup_subst (cast_held schema) arguments = Some held ->
    A.PayloadOf W tag arguments payload -> handle_type (cast_array schema) payload ->
    handle_typed H (cast_array schema) held value ->
    value_typed H (VTag tag value) (TNom tag arguments)
with fields_typed (H : heap_types) : list (fld * value) -> list (fld * ty) -> Prop :=
| FieldsNil : fields_typed H [] []
| FieldsCons : forall name value tau values fields,
    value_typed H value tau -> fields_typed H values fields ->
    fields_typed H ((name,value)::values) ((name,tau)::fields).

Scheme value_typed_ind' := Induction for value_typed Sort Prop
with fields_typed_ind' := Induction for fields_typed Sort Prop.
Combined Scheme value_fields_ind from value_typed_ind', fields_typed_ind'.

Lemma typing_extends : forall H H', heap_extends H H' ->
  (forall value tau, value_typed H value tau -> value_typed H' value tau) /\
  (forall values fields, fields_typed H values fields -> fields_typed H' values fields).
Proof.
  intros H H' Extend. apply value_fields_ind; intros; eauto using value_typed, fields_typed,
    handle_typed_extends.
Qed.

Theorem value_typed_extends : forall H H' value tau,
  heap_extends H H' -> value_typed H value tau -> value_typed H' value tau.
Proof. intros H H' value tau Extend; exact (proj1 (typing_extends H H' Extend) value tau). Qed.

Theorem fields_typed_extends : forall H H' values fields,
  heap_extends H H' -> fields_typed H values fields -> fields_typed H' values fields.
Proof. intros H H' values fields Extend; exact (proj2 (typing_extends H H' Extend) values fields). Qed.

Theorem field_lookup_typed : forall H values fields name tau,
  fields_typed H values fields -> A.slook name fields = Some tau ->
  exists value, A.slook name values = Some value /\ value_typed H value tau.
Proof.
  intros H values fields name tau Typed; revert name tau.
  induction Typed; intros sought result Found; cbn [A.slook] in *; [discriminate|].
  destruct (String.eqb sought name).
  - injection Found as <-. eexists; split; [reflexivity|eassumption].
  - eauto.
Qed.

Theorem handle_payload_typed : forall H kind tau held value,
  handle_type kind tau -> handle_typed H kind held value -> value_typed H value tau.
Proof.
  intros H kind tau held value Shape; induction Shape; intro Typed.
  - exact (TypedRawHandle H false held value Typed).
  - exact (TypedRawHandle H true held value Typed).
  - eapply TypedUnion; eauto.
Qed.

Theorem empty_union_uninhabited : forall H value,
  ~ value_typed H value (TUnion []).
Proof. intros H value Typed; inversion Typed; subst; contradiction. Qed.

Theorem machine_handle_typed : forall H (kind : bool) value,
  value_typed H value (TMachine (if kind then MArrayRef else MStructRef)) ->
  exists held, handle_typed H kind held value.
Proof.
  intros H kind value Typed. destruct kind; inversion Typed; subst;
    match goal with k : bool |- _ => destruct k end; try discriminate;
    eexists; eassumption.
Qed.

Theorem checked_header_typed : forall H kind held address tau,
  value_typed H (VCheckedHandle kind held address) tau ->
  H address = Some (kind, held) /\ handle_type kind tau.
Proof.
  intros H kind held address tau Typed.
  remember (VCheckedHandle kind held address) as value eqn:Shape.
  induction Typed; inversion Shape; subst.
  - destruct (IHTyped eq_refl) as [Cell Member].
    split; [exact Cell|eapply HandleUnion; eassumption].
  - inversion H0; subst. split; [assumption|destruct kind; constructor].
Qed.

Theorem tagged_payload_typed : forall H tag arguments value,
  A.kind_sym W tag = Some A.DKTag ->
  value_typed H (VTag tag value) (TNom tag arguments) ->
  exists payload, A.PayloadOf W tag arguments payload /\ value_typed H value payload.
Proof.
  intros H tag arguments value Tag Typed. inversion Typed; subst.
  - eauto.
  - congruence.
  - eexists; split; [eassumption|eapply handle_payload_typed; eassumption].
Qed.

Theorem unit_tag_typed : forall H tag arguments value,
  A.kind_sym W tag = Some A.DKUnit ->
  value_typed H (VTag tag value) (TNom tag arguments) -> value = VUnit /\ arguments = [].
Proof.
  intros H tag arguments value Unit Typed. inversion Typed; subst; auto;
    match goal with Payload : A.PayloadOf _ _ _ _ |- _ =>
      destruct Payload as [? ? d ? ? ? ? Decl Ast _]; unfold A.kind_sym in Unit;
      rewrite Decl, Ast in Unit; discriminate end.
Qed.

End Values.

(* An interpreter instance, kept transparent: a functor over it computes
   with the instance it is given (e.g. Public's Reference), so results
   about it are results about that instance, not about a copy. *)
Module Type INTERP (Prims : PRIMITIVES).
  Include Interpreter Prims.
End INTERP.

(* A Public instance, kept transparent, for the same reason: CORRECT's
   obligations are stated inside one particular instance. *)
Module Type PUBLIC (Host : HOST).
  Include Public Host.
End PUBLIC.
