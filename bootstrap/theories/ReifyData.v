(* Checked source metadata consumed by immutable lowering.  These records are
   ghost data: the runtime consumes only the lowered program and export maps. *)
From Stdlib Require Import List Arith Bool.
From NM Require Import Audit.
Import ListNotations.

Record reify_functor_decl : Type := ReifyFunctorDecl {
  rfd_inputs : tele;
  rfd_outputs : tele;
  rfd_fixed : list tsym;
  (* Fixed keys: captured outer interpretations. Other keys: checked
     canonical equations among formal inputs. *)
  rfd_fixed_inst : subst;
  rfd_poly : list tsym;
  rfd_captures : tele;
  rfd_pins : subst
}.

Inductive reify_decl : Type :=
| RDType
| RDValue (inputs : tele) (value_type : ty)
| RDFunction (inputs : tele) (signature : fnsig)
| RDMethod (inputs : tele) (signature : fnsig)
| RDFunctor (schema : reify_functor_decl)
| RDOpaqueFunctor (source_identity : usym).

(* Source metadata remains in the checker's own type/key representation;
   collecting it cannot introduce a new type-conversion failure. *)
Record reify_signature (R : Type) : Type := ReifySignature {
  rsg_params : list R;
  rsg_result : R;
  rsg_receiver : option R
}.

Inductive reify_owner : Type :=
| ROFunction (id : nat)
| ROPlatform (target : nat)
| ROLink (target provider : nat).
Definition reify_owner_eqb (a b : reify_owner) : bool :=
  match a,b with
  | ROFunction x, ROFunction y | ROPlatform x, ROPlatform y => Nat.eqb x y
  | ROLink x y, ROLink z w => Nat.eqb x z && Nat.eqb y w
  | _,_ => false
  end.

Section SourceMetadata.
Context (R K : Type).
Record reify_scope : Type := ReifyScope {
  rs_names : list (tsym * R);
  rs_generations : list (tsym * R);
  rs_items : list K;
  rs_values : list (nat * R);
  rs_functions : list (nat * reify_signature R);
  rs_methods : list ((R * nat) * reify_signature R);
  rs_provisions : list (K * (list (tsym * option R) * nat))
}.
Record reify_exports : Type := ReifyExports {
  rx_types : list tsym;
  rx_items : list K;
  rx_promised : reify_scope;
  rx_actual : reify_scope;
  rx_inputs : reify_scope;
  rx_outputs : list (tsym * tsym)
}.
Inductive reify_event : Type :=
| REType (name : tsym) (meaning : R)
| REValue (name : vsym) (meaning : R) (arguments : list (tsym * R))
| REProvider (provider : fsym) (inputs : list K)
    (signature : reify_signature R) (arguments : list (tsym * R))
| REFunction (name provider : fsym)
    (signature : reify_signature R) (arguments : list (tsym * R))
| REMethod (receiver : R) (name : msym) (provider : fsym)
    (signature : reify_signature R) (arguments : list (tsym * R))
| REFunctor (name provider : usym) (inputs : list K)
    (arguments : list (tsym * R)) (copy_view : option reify_exports)
| REApply (name : usym) (inputs outputs : list K)
    (input_arguments output_arguments : list (tsym * R)) (output_scope : reify_scope).
Record reify_code : Type := ReifyCode {
  rc_owner : reify_owner;
  rc_scope : reify_scope;
  rc_inputs : list K;
  rc_signature : option (reify_signature R);
  rc_exports : option reify_exports;
  rc_events : list reify_event
}.
Record reify_metadata : Type := ReifyMetadata {
  rm_owner : option reify_owner;
  rm_codes : list (reify_owner * reify_code);
  rm_declarations : list (item * reify_decl);
  rm_constants : list (nat * list reify_event)
}.
Definition empty_reify_metadata : reify_metadata := ReifyMetadata None [] [] [].
End SourceMetadata.
Arguments ReifyMetadata {R K}.
Arguments rm_owner {R K}.
Arguments rm_codes {R K}.
Arguments rm_declarations {R K}.
Arguments rm_constants {R K}.

(* Output substitutions are a distinct suffix, never first-looked-up in the
   mixed source substitution.  The checker records this boundary directly. *)
Definition split_apply_arguments (input_count : nat) (mixed : subst)
    : subst * subst := (firstn input_count mixed, skipn input_count mixed).

Lemma split_apply_arguments_app : forall inputs outputs,
    split_apply_arguments (length inputs) (inputs ++ outputs) = (inputs, outputs).
Proof.
  intros. unfold split_apply_arguments.
  rewrite firstn_app, firstn_all, Nat.sub_diag. simpl.
  rewrite app_nil_r, skipn_app, skipn_all, Nat.sub_diag. reflexivity.
Qed.
