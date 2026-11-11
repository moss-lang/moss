(* Executable and negative checks of the public model. No model
   equivalence is assumed. *)
From Stdlib Require Import List String Arith Lia.
From NM Require Import AuditBase.
From NM Require Import LegacyBodyJudgments.
From NM Require Import Audit.
Import Audit.Semantics.
Import ListNotations Input Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Example one_is_one : literal_tree_of "1" = LOne.
Proof. reflexivity. Qed.
Example integer_zero_needs_no_arithmetic : literal_tree_of "000" = LZero.
Proof. reflexivity. Qed.
Example decimal_scale_is_ordered : literal_tree_of "1e-3" =
  LDiv (LFloat LOne)
    (LMul (LMul (LFloat (small_nat 10)) (LFloat (small_nat 10))) (LFloat (small_nat 10))).
Proof. reflexivity. Qed.
Example one_does_not_insert_a_multiply : forall W C code t,
  Literal W C (literal_tree_of "1") code t -> exists value, code = EVal value.
Proof. intros W C code t H. inversion H; subst. eauto. Qed.

Example bare_return_requires_unit : forall W P F C exp code t,
  fr_ret F = Some never -> ~ Ex W P F C (AEReturn None) exp code t.
Proof. intros W P F C exp code t Never Related. inversion Related; subst; unfold never in *; congruence. Qed.
Example receiver_slot_is_reserved : forall F receiver,
  Numbering F (Some receiver) [] -> slook "x" (fr_var F) <> Some 0.
Proof. intros F receiver [_ [_ [Reserved _]]]. exact (Reserved "x"). Qed.
Example generations_cannot_repeat : forall W F C g,
  ~ FreshFor W F C [g;g].
Proof. intros W F C g [Distinct _]. inversion Distinct; subst. apply H1. simpl; auto. Qed.

Example inner_let_does_not_capture_outer_x :
  free_names (fun _ => false)
    (AEBlock (ABlock [ASLet "x" None AEUnit] (Some (AEPath ["x"] None)))) = [].
Proof. reflexivity. Qed.
Example initializer_sees_outer_x :
  free_names (fun _ => false)
    (AEBlock (ABlock [ASLet "x" None (AEPath ["x"] None)] (Some (AEPath ["x"] None)))) = ["x"].
Proof. reflexivity. Qed.
Example match_binder_does_not_capture_outer_x :
  free_names (fun _ => false)
    (AEMatch AEUnit [(APPath ["x"], AEPath ["x"] None)]) = [].
Proof. reflexivity. Qed.

(* An ordinary binding shadows the implicit prelude in its own module and
   namespace. The negative premise in ScPrelude excludes the outer binding;
   it does not prevent the inner binding from entering scope. *)
Example ordinary_bindings_take_precedence : forall b W p ns n inner visible,
  OrdinaryScope b W p ns n inner -> InScope b W p ns n visible ->
  OrdinaryScope b W p ns n visible.
Proof.
  intros b W p ns n inner visible Inner Visible.
  destruct Visible as [p ns n visible Ordinary | p m ns n visible Module Prelude Export Absent].
  - exact Ordinary.
  - exfalso. exact (Absent inner Inner).
Qed.
Example an_unbound_name_uses_the_prelude : forall b W p m ns n outer,
  module_of b p = Some m -> am_prelude m = true ->
  Exports b W (b_prelude b) ns n outer ->
  (forall inner, ~ OrdinaryScope b W p ns n inner) -> InScope b W p ns n outer.
Proof. intros. eapply ScPrelude; eassumption. Qed.

Module Host <: HOST.
  Definition state := unit.
  Definition primitive (_ : program_view) (_ : op) (_ : list value) (s : unit) := (Trap 0, s).
  Definition heap_ok (_ : Heap.hty) (_ : unit) := True.
End Host.
Module Model := Public Host.
Module Reference := Model.Reference.
Definition unit_program : program := Program [(1,EUnit)] [] 0 0 0 1 empty_environment [] [] [] empty_code_origins.
Definition unit_view := program_of unit_program [] (fun _ => None).
Definition initial : Model.state := Model.World [] (fun _ => None) tt.

Example unit_program_runs : Model.Runs unit_program Model.default_interface initial
    (Model.Result (Model.Returned VUnit) initial).
Proof. exists 1. reflexivity. Qed.
Example type_binding_is_unconditional :
  bind_type empty_environment 7 TUnit = EnvironmentWithAliases [] [(7,TUnit)] [].
Proof. reflexivity. Qed.
Example bool_requires_its_declared_wrapper :
  Reference.truthy unit_view (VTag 8 VUnit) = None.
Proof. reflexivity. Qed.
Example records_are_structural :
  app_subst [(7,TUnit)] (TRecord [("field",TAbs 7)]) = TRecord [("field",TUnit)].
Proof. reflexivity. Qed.
Example machine_identities_do_not_need_nominal_names :
  app_subst [(7,TUnit)] (TMachine 7) = TMachine 7.
Proof. reflexivity. Qed.

Definition no_library : world :=
  World (fun _ => None) (fun _ _ _ => None) (fun _ _ _ => None)
    (fun _ _ => None) (fun _ => []) None None (fun _ => None).
Lemma no_library_symbols : forall library ns name symbol,
  ~ LangSym no_library library ns name symbol.
Proof. intros library ns name symbol [d [p [Impossible _]]]. discriminate. Qed.
Lemma no_library_casts : forall P,
  Platform.Representations no_library P -> program_casts P = [].
Proof.
  intros P [_ [reference [array [Ref [Arr Table]]]]].
  destruct Ref as [[n [t [h [Named _]]]]|[_ ->]];
    [exfalso; eapply no_library_symbols; exact Named|].
  destruct Arr as [[n [t [h [Named _]]]]|[_ ->]];
    [exfalso; eapply no_library_symbols; exact Named|].
  exact Table.
Qed.
Example an_arbitrary_cast_entry_is_rejected :
  ~ Platform.Representations no_library
    (Program [] [] 0 0 0 0 empty_environment [] [(7,CastSpec 0 8 None)] [] empty_code_origins).
Proof. intro Valid. pose proof (no_library_casts _ Valid). discriminate. Qed.

(* The oracle plan's N6 (AUDIT-CHANGES #94): a platform constant declared
   with another type than `I32` (`val i32_one: ();` in lib/wasm.moss) does
   not link. Platform.Entry asks the val's type in main's context to be the
   platform's machine type I32, an abstract type symbol, which `()` is not. *)
Example a_unit_platform_constant_is_rejected : forall W P C v w,
  nlook v (c_vals C) = Some TUnit -> ~ Platform.Entry W P C (IVal v) (ValueProvision w).
Proof.
  intros W P C v w Ty E. destruct w as [| | |sort bits|]; cbn in E; try contradiction. destruct sort; cbn in E; try contradiction.
  destruct E as (t & Ty' & symbol & _ & _ & ->). rewrite Ty in Ty'. discriminate.
Qed.

(* The previous return bug is rejected by the real elaborator too. *)
From NM Require EaBase EaMain Sx Parse.
Definition never_source :=
  "type Never = |; fn stop(): Never { return; } fn main() { match stop() {}; }".
Definition never_read (p : string) :=
  if String.eqb p "/lib/main.moss" then Some never_source else None.
Example actual_elaborator_rejects_bare_never_return :
  match Parse.load never_read "/" "/lib/prelude.moss" "/lib/main.moss" with
  | Sx.Good bundle => EaMain.elaborate bundle
  | Sx.Bad message => EaBase.Err message
  end = EaBase.Err "/lib/main.moss: `return` with no value, but the function returns `|`".
Proof. vm_compute. reflexivity. Qed.

Example no_return_without_a_function : forall W P F C exp code t,
  fr_ret F = None -> ~ Ex W P F C (AEReturn (Some AEUnit)) exp code t.
Proof. intros W P F C exp code t NoTarget Related. inversion Related; subst; congruence. Qed.

Definition output_program := Program []
  [(2,FunctorCode [] [] [] [ITy 1] [BType 1 (TMachine 0)] [])]
  0 0 0 0 empty_environment [] [] [] empty_code_origins.
Definition output_environment :=
  Environment [] [(IFun 3,FunctorClosure 2 None empty_environment None)].
Example fresh_output_meaning_is_exported :
  Reference.apply_functor 6 (program_of output_program [] (fun _ => None))
    output_environment 3 [] [(1,9)] [] [] (Model.default_interface,initial) =
  inl (EnvironmentWithAliases [(9,TMachine 0)] [(1,TMachine 0)] [],
       (Model.default_interface,initial)).
Proof. reflexivity. Qed.

Example captured_interpretations_participate_in_freshness :
  In 9 (context_gens (Cx [] [] [] [(IFn 1,ProvidedFn (FSig [] TUnit) [(7,Some (TAbs 9))])] [])).
Proof. cbn; auto. Qed.
