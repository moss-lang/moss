From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaTypesSound.
From NM Require EaCall EaCallSound PublicContextFacts PublicMerge PublicTypeRead.
From NM Require AuditBase EaPlatform EaRootSound PublicTypeFacts.
Import ListNotations AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.
Module P := AuditBase.Platform.

Theorem entry_result_sound : forall E fuel t,
  EaPlatform.entry_result E fuel t = true -> P.EntryResult (e_W E) t.
Proof.
  intros E fuel. induction fuel as [|fuel IH]; intros t Checked; [discriminate|].
  destruct t as [machine|fields| |abstract|tag args|members];
    cbn [EaPlatform.entry_result] in Checked; try discriminate.
  - apply P.EntryRecord. apply Forall_forall. intros field Present. apply IH.
    exact (proj1 (forallb_forall _ _) Checked field Present).
  - apply P.EntryUnit.
  - destruct (payload_of (e_W E) (e_fuel E) tag args) as [payload|message] eqn:Read;
      [|discriminate].
    eapply P.EntryTag; [eapply payload_of_sound; exact Read|now apply IH].
  - destruct members; [apply P.EntryNever|discriminate].
Qed.

Lemma tag_layout_sound : forall E name parameter handle,
  EaPlatform.tag_layout E name parameter handle = Ok tt ->
  P.TagLayout (e_W E) name parameter handle.
Proof.
  intros E name parameter handle Run symbol Tag.
  unfold EaPlatform.tag_layout, EaPlatform.wasm_sym in Run.
  rewrite (EaRootSound.lang_sym_complete _ _ _ _ _ Tag) in Run.
  destruct (lang_sym (e_W E) "wasm" NSName parameter) as [param|] eqn:ParameterRead;
    [|discriminate].
  destruct (lang_sym (e_W E) "wasm" NSName handle) as [raw|] eqn:Handle;
    [|discriminate].
  apply rbind_ok in Run as (checked & Layout & Run).
  destruct (w_reqs (e_W E) symbol) as [|first rest] eqn:Requirements;
    [rewrite andb_false_r in Layout; discriminate|].
  destruct (kind_is (e_W E) symbol DKTag) eqn:Kind; [|discriminate Layout].
  destruct (Nat.eqb param first) eqn:First; [|discriminate Layout].
  apply Nat.eqb_eq in First. subst first.
  destruct (payload_of (e_W E) (e_fuel E) symbol
      (map (fun t => (t, TAbs t)) (param :: rest))) as [payload|message] eqn:Read;
      [|discriminate Run].
  destruct (ty_eqb payload (TAbs raw)) eqn:Payload; [|discriminate Run].
  apply PublicTypeFacts.ty_eqb_spec in Payload. subst payload.
  exists param, raw, rest. repeat split.
  - exact (lang_sym_sound _ _ _ _ _ ParameterRead).
  - exact (lang_sym_sound _ _ _ _ _ Handle).
  - now apply kind_is_spec.
  - eapply payload_of_sound. exact Read.
Qed.

Local Lemma matching_row_count : forall A symbol (rows : list (A * nat)),
  List.length (filter (Nat.eqb symbol) (map snd rows)) =
  List.length (filter (fun row => Nat.eqb symbol (snd row)) rows).
Proof.
  intros A symbol rows. induction rows as [|[name key] rows IH]; [reflexivity|].
  cbn [map filter snd]. destruct (Nat.eqb symbol key); cbn [List.length]; congruence.
Qed.

Theorem layouts_sound : forall E,
  EaPlatform.layouts E = Ok tt -> P.Layouts (e_W E).
Proof.
  intros E Run. unfold EaPlatform.layouts in Run. cbv zeta in Run.
  set (present := flat_map (fun nm => match EaPlatform.wasm_sym E nm with
    | Some s => [(nm, s)] | None => [] end) P.abstract_names) in *.
  apply rbind_ok in Run as (checked & Types & Run).
  destruct (find (fun q => negb (kind_is (e_W E) (snd q) DKType) ||
    negb (Nat.eqb (List.length (filter (Nat.eqb (snd q)) (map snd present))) 1)) present)
    as [[wrong symbol]|] eqn:Check; [discriminate Types|].
  apply rbind_ok in Run as (reference & Reference & Run).
  apply rbind_ok in Run as (array & Array & Address).
  assert (Present : forall name symbol, In name P.abstract_names ->
    LangSym (e_W E) "wasm" NSName name symbol -> In (name, symbol) present).
  {
    intros name symbol Listed Resolved. unfold present. apply in_flat_map.
    exists name. split; [exact Listed|].
    unfold EaPlatform.wasm_sym. rewrite (EaRootSound.lang_sym_complete _ _ _ _ _ Resolved).
    now left.
  }
  assert (Read : forall name symbol, In (name, symbol) present ->
    kind_sym (e_W E) symbol = Some DKType /\
    List.length (filter (fun row => Nat.eqb symbol (snd row)) present) = 1).
  {
    intros name symbol Member. pose proof (find_none _ _ Check (name, symbol) Member) as Read.
    cbn [snd] in Read. apply orb_false_iff in Read as [TypeCheck Count].
    apply negb_false_iff in TypeCheck, Count. apply kind_is_spec in TypeCheck. apply Nat.eqb_eq in Count.
    rewrite matching_row_count in Count. auto.
  }
  split.
  { intros name symbol Listed Resolved. exact (proj1 (Read _ _ (Present _ _ Listed Resolved))). }
  split.
  {
    intros left right symbol Left Right ReadLeft ReadRight.
    pose proof (Present _ _ Left ReadLeft) as L. pose proof (Present _ _ Right ReadRight) as R.
    pose proof (proj2 (Read _ _ L)) as Length.
    assert (LeftRow : In (left, symbol) (filter (fun row => Nat.eqb symbol (snd row)) present)).
    { apply filter_In. split; [exact L|apply Nat.eqb_refl]. }
    assert (RightRow : In (right, symbol) (filter (fun row => Nat.eqb symbol (snd row)) present)).
    { apply filter_In. split; [exact R|apply Nat.eqb_refl]. }
    destruct (filter (fun row => Nat.eqb symbol (snd row)) present) as [|row rest]; [discriminate|].
    destruct rest; [|discriminate]. destruct LeftRow as [LeftEq|[]], RightRow as [RightEq|[]]. congruence.
  }
  destruct reference, array. split; [eapply tag_layout_sound; exact Reference|].
  split; [eapply tag_layout_sound; exact Array|eapply tag_layout_sound; exact Address].
Qed.

Theorem entry_type_match_sound : forall W main C needs,
  CtxOf W main C needs ->
  EaCall.rule_b W "calling `main` from Base" (P.entry_context W) C = Ok tt ->
  forall arguments,
  EaCall.call_theta W "calling `main` from Base" (P.entry_context W) C = Ok arguments ->
  P.EntryTypeMatch W main C.
Proof.
  intros W main C needs Context Force arguments Call.
  destruct (PublicContextFacts.function_context_merge W main C needs Context) as [Solved Section].
  assert (Caller : PublicContextFacts.CxSection (P.entry_context W)).
  { split.
    - intros x. apply MERefl.
    - intros t u Equal. change (MergeEq [] t u) in Equal.
      apply (PublicMerge.merge_eq_sound (s := []) (M := [])) in Equal.
      + exact Equal.
      + split; intros c ? ? Member; contradiction. }
  split; [eapply EaCallSound.rule_b_sound; eassumption|].
  unfold EaCall.call_theta in Call.
  destruct (EaCallSound.call_theta_with_sound _ _ _ _ _ main _ Caller Section Call) as [-> Read].
  exact Read.
Qed.

Theorem tables_sound : forall E,
  EaPlatform.layouts E = Ok tt ->
  P.Tables (e_W E) (P.machine_table (e_W E)).
Proof. intros E Run. split; [now apply layouts_sound|reflexivity]. Qed.

Theorem lang_items_formed_sound : forall E,
  EaPlatform.lang_items_formed E = Ok tt -> LangItemsFormed (e_W E).
Proof.
  intros E Run b Bool. unfold EaPlatform.lang_items_formed in Run.
  rewrite (EaRootSound.lang_item_sym_complete _ _ _ Bool) in Run.
  destruct (lang_item_sym (e_W E) LITrue) as [truth|] eqn:Truth; [|discriminate].
  destruct (lang_item_sym (e_W E) LIFalse) as [falsity|] eqn:Falsity; [|discriminate].
  apply rbind_ok in Run as (checked & Formation & Run).
  destruct (w_reqs (e_W E) b) as [|need needs] eqn:Requirements.
  2: { rewrite andb_false_r in Formation. discriminate. }
  destruct (kind_is (e_W E) b DKTag) eqn:BoolKind; [|discriminate Formation].
  destruct (kind_is (e_W E) truth DKUnit) eqn:TruthKind; [|discriminate Formation].
  destruct (kind_is (e_W E) falsity DKUnit) eqn:FalsityKind; [|discriminate Formation].
  destruct (Nat.eqb truth falsity) eqn:Distinct; [discriminate Formation|].
  destruct (payload_of (e_W E) (e_fuel E) b []) as [payload|message] eqn:Read;
    [|discriminate Run].
  destruct (ty_eqb payload (TUnion [TNom truth []; TNom falsity []]) ||
    ty_eqb payload (TUnion [TNom falsity []; TNom truth []])) eqn:Payload;
    [|discriminate Run].
  apply orb_true_iff in Payload.
  exists truth, falsity.
  split; [eapply lang_item_sym_sound; exact Truth|].
  split; [eapply lang_item_sym_sound; exact Falsity|].
  split; [now apply kind_is_spec|].
  split; [now apply kind_is_spec|].
  split; [now apply kind_is_spec|].
  split; [now apply Nat.eqb_neq|].
  split; [reflexivity|].
  destruct Payload as [Payload|Payload];
    apply PublicTypeFacts.ty_eqb_spec in Payload; subst payload;
    [left|right]; eapply payload_of_sound; exact Read.
Qed.

Theorem bool_symbols_sound : forall E boolean truth falsity,
  LangItemsFormed (e_W E) ->
  EaPlatform.bool_syms E = (boolean, truth, falsity) ->
  (LangItem (e_W E) LIBool boolean /\ LangItem (e_W E) LITrue truth /\
   LangItem (e_W E) LIFalse falsity) \/
  ((forall symbol, ~ LangItem (e_W E) LIBool symbol) /\
   boolean = 0 /\ truth = 0 /\ falsity = 0).
Proof.
  intros E boolean truth falsity Formed Read.
  change ((match lang_item_sym (e_W E) LIBool, lang_item_sym (e_W E) LITrue,
      lang_item_sym (e_W E) LIFalse with
    | Some b, Some tr, Some fl => (b, tr, fl)
    | _, _, _ => (0, 0, 0)
    end) = (boolean, truth, falsity)) in Read.
  destruct (lang_item_sym (e_W E) LIBool) as [b|] eqn:Bool.
  - apply lang_item_sym_sound in Bool.
    destruct (Formed b Bool) as (tr & fl & Truth & Falsity & _).
    rewrite (EaRootSound.lang_item_sym_complete _ _ _ Truth),
      (EaRootSound.lang_item_sym_complete _ _ _ Falsity) in Read.
    injection Read as <- <- <-. left. auto.
  - injection Read as <- <- <-. right. split.
    + intros symbol Declared. apply EaRootSound.lang_item_sym_complete in Declared. congruence.
    + auto.
Qed.
