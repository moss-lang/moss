(* The representation boundary. These relations describe how the bootstrap's
   existing type/value encodings can represent the public model. They do not
   assert that its elaborator or its interpreter implements Public.CORRECT. *)
From Stdlib Require Import List String PrimInt63.
From NM Require Import Audit.
From NM Require ExecutionModel Elab Sx.
Import Audit.Semantics.
Import ListNotations.

Record representation := Representation {
  field_names : list (string * nat);
  field_names_unique : NoDup (map fst field_names);
  field_numbers_unique : NoDup (map snd field_names);
  machine_head : machine -> nat;
  machine_heads_unique : forall a b, machine_head a = machine_head b -> a = b;
  machine_heads_nonzero : forall m, machine_head m <> 0;
  heap_address : int -> address;
  heap_addresses_unique : forall a b, heap_address a = heap_address b -> a = b
}.

Inductive type_represents (R : representation) : ExecutionModel.ty -> ty -> Prop :=
| RepresentsUnit : type_represents R ExecutionModel.TUnit TUnit
| RepresentsAbstract : forall t, type_represents R (ExecutionModel.TAbs t) (TAbs t)
| RepresentsMachine : forall m,
    type_represents R (ExecutionModel.TNom (machine_head R m) []) (TMachine m)
| RepresentsRecord : forall raw fields,
    Forall2 (fun a b => In (fst b, fst a) (field_names R) /\
      type_represents R (snd a) (snd b)) raw fields ->
    type_represents R (ExecutionModel.TNom 0 raw) (TRecord fields)
| RepresentsNominal : forall n raw args,
    n <> 0 -> (forall m, n <> machine_head R m) ->
    Forall2 (fun a b => fst a = fst b /\ type_represents R (snd a) (snd b)) raw args ->
    type_represents R (ExecutionModel.TNom n raw) (TNom n args)
| RepresentsUnion : forall raw members,
    Forall2 (type_represents R) raw members ->
    type_represents R (ExecutionModel.TUnion raw) (TUnion members).

Inductive value_represents (R : representation) : ExecutionModel.xvalue -> value -> Prop :=
| RepresentsUnitValue : value_represents R ExecutionModel.XVUnit VUnit
| RepresentsTagValue : forall n raw value, value_represents R raw value ->
    value_represents R (ExecutionModel.XVTag n raw) (VTag n value)
| RepresentsI32 : forall n, value_represents R (ExecutionModel.XVI32 n) (VI32 n)
| RepresentsI64 : forall n, value_represents R (ExecutionModel.XVI64 n) (VI64 n)
| RepresentsF32 : forall n, value_represents R (ExecutionModel.XVF32 n) (VF32 n)
| RepresentsF64 : forall n, value_represents R (ExecutionModel.XVF64 n) (VF64 n)
| RepresentsRecordValue : forall raw fields,
    Forall2 (fun a b => In (fst b, fst a) (field_names R) /\
      value_represents R (snd a) (snd b)) raw fields ->
    value_represents R (ExecutionModel.XVRec raw) (VRecord fields)
| RepresentsRawHandle : forall a,
    value_represents R (ExecutionModel.XVRef a) (VHandle (heap_address R a))
| RepresentsCheckedHandle : forall array raw held a,
    type_represents R raw held ->
    value_represents R (ExecutionModel.XVRefAt array raw a)
      (VCheckedHandle array held (heap_address R a)).

(* Source syntax is shared definitionally, so the already checked front end
   needs no translation of token streams, ASTs or module bundles. *)
Definition shared_source (bundle : Audit.Input.bundle) : ExecutionModel.Input.bundle := bundle.

(* A named pending property, not a theorem or an added axiom. A proof must
   choose a concrete code representation relation and construct the same source
   reading used by platform linking; signature safety alone is insufficient. *)
Definition elaboration_refines
    (code_represents : ExecutionModel.xprogram -> program -> Prop) : Prop :=
  forall bundle raw, Elab.elaborate bundle = Sx.Good raw ->
    exists W semantic,
      Elaboration.ElaboratesIn bundle W semantic /\ Platform.Linked W semantic /\
      code_represents raw semantic.
