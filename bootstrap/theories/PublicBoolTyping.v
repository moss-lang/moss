(* Typed Bool values have exactly the shape the reference truthy test
   reads, under the platform layout of lib/bool.moss. *)
From Stdlib Require Import List String Arith.
From NM Require AuditBase.
From NM Require Import Audit PublicTypeRead PublicValueTyping.
Import AuditBase.Semantics ListNotations.
Local Open Scope string_scope.
Module A := AuditBase.Semantics.Elaboration.

Lemma handle_type_not_nominal : forall P kind n xs, ~ handle_type P kind (TNom n xs).
Proof. intros P kind n xs H; inversion H. Qed.

Lemma handle_type_nominal_union : forall P kind members,
  Forall (fun m => exists n xs, m = TNom n xs) members -> ~ handle_type P kind (TUnion members).
Proof.
  intros P kind members Nominal H. inversion H as [|? member ? Shape Member]; subst.
  rewrite Forall_forall in Nominal. destruct (Nominal member Member) as [n [xs ->]].
  exact (handle_type_not_nominal _ _ _ _ Shape).
Qed.

Section Bool.
Variables (W : A.world) (P : program_view).

Lemma unit_member_typed : forall H value unit,
  A.kind_sym W unit = Some A.DKUnit ->
  value_typed W P H value (TNom unit []) -> value = VTag unit VUnit.
Proof.
  intros H value unit Unit Typed. inversion Typed; subst.
  - rewrite (payload_tag W _ _ _ ltac:(eassumption)) in Unit. discriminate.
  - reflexivity.
Qed.

Theorem bool_value_shape : forall H bool value,
  A.LangItemsFormed W -> A.LangSym W "bool" A.NSName "Bool" bool ->
  value_typed W P H value (TNom bool []) ->
  exists truth falsity, A.LangSym W "bool" A.NSName "True" truth /\
    A.LangSym W "bool" A.NSName "False" falsity /\
    (value = VTag bool (VTag truth VUnit) \/ value = VTag bool (VTag falsity VUnit)).
Proof.
  intros H bool value BoolLayout Bool Typed.
  destruct (BoolLayout bool Bool) as [truth [falsity [True' [False' [Tag [TrueUnit [FalseUnit [_ [_ Payload]]]]]]]]].
  exists truth, falsity. split; [exact True'|]. split; [exact False'|].
  assert (Members : forall payload, A.PayloadOf W bool [] payload ->
    payload = TUnion [TNom truth []; TNom falsity []] \/ payload = TUnion [TNom falsity []; TNom truth []]).
  { intros payload Read. destruct Payload as [Layout|Layout];
      [left|right]; exact (payload_det W _ _ _ _ Read Layout). }
  inversion Typed; subst.
  - match goal with Read : A.PayloadOf W bool [] ?payload, Inner : value_typed W P H _ ?payload |- _ =>
      destruct (Members _ Read) as [->| ->]; inversion Inner; subst end;
    match goal with Mem : List.In ?member _, MemberTyped : value_typed W P H _ ?member |- _ =>
      destruct Mem as [<-|[<-|[]]];
      first [rewrite (unit_member_typed H _ _ TrueUnit MemberTyped)
            |rewrite (unit_member_typed H _ _ FalseUnit MemberTyped)]; auto end.
  - congruence.
Qed.
End Bool.

Lemma lang_sym_unique : forall W short ns n s s',
  A.LangSym W short ns n s -> A.LangSym W short ns n s' -> s = s'.
Proof.
  intros W short ns n s s' [d [p [_ [Lib Scope]]]] [d' [p' [_ [Lib' Scope']]]].
  unfold A.LibraryModule in *. destruct (A.w_library W); [|contradiction].
  subst p p'. rewrite Scope in Scope'. congruence.
Qed.

(* The program's Bool symbols, as ElaboratesIn records them, and Bool's
   layout (A.LangItemsFormed). *)
Definition BoolSymbols (W : A.world) (program : program) : Prop :=
  A.LangItemsFormed W /\ (
  A.LangSym W "bool" A.NSName "Bool" (program_bool program) /\
  A.LangSym W "bool" A.NSName "True" (program_true program) /\
  A.LangSym W "bool" A.NSName "False" (program_false program) \/
  ((forall s, ~ A.LangSym W "bool" A.NSName "Bool" s) /\
   program_bool program = 0 /\ program_true program = 0 /\ program_false program = 0)).

Lemma elaborates_bool_symbols : forall bundle W program,
  A.ElaboratesIn bundle W program -> BoolSymbols W program.
Proof. intros bundle W program [resolve (_ & _ & _ & _ & _ & Bool & Formed & _)]; exact (conj Formed Bool). Qed.

Module BoolTruthy (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module I := I0.

(* A condition elaborated by Cond has a Bool type; any typed value of it
   is read by truthy. *)
Theorem truthy_typed : forall W program argv read H bool value,
  BoolSymbols W program ->
  A.LangSym W "bool" A.NSName "Bool" bool ->
  value_typed W (program_of program argv read) H value (TNom bool []) ->
  exists bit, I.truthy (program_of program argv read) value = Some bit.
Proof.
  intros W program argv read H bool value Symbols Bool Typed.
  destruct (bool_value_shape W _ H bool value (proj1 Symbols) Bool Typed)
    as [truth [falsity [True' [False' Shape]]]].
  destruct Symbols as [_ [[Bool' [True'' False'']]|[None' _]]];
    [|exfalso; exact (None' bool Bool)].
  pose proof (lang_sym_unique _ _ _ _ _ _ Bool Bool'); subst bool.
  pose proof (lang_sym_unique _ _ _ _ _ _ True' True''); subst truth.
  pose proof (lang_sym_unique _ _ _ _ _ _ False' False''); subst falsity.
  unfold I.truthy; cbn.
  destruct Shape as [->| ->]; rewrite Nat.eqb_refl;
    [rewrite Nat.eqb_refl; eauto|].
  destruct (Nat.eqb (program_false program) (program_true program)); [eauto|rewrite Nat.eqb_refl; eauto].
Qed.
End BoolTruthy.
