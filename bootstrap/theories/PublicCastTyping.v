(* Checked tag construction preserves ground data typing. This local result
   does not assert that source typing supplies the required ground payload. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit PublicTypeFacts PublicValueTyping.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition ground_arguments (d : environment) (arguments : subst) : subst :=
  map (fun q => (fst q, ground d (snd q))) arguments.

Lemma lookup_ground_arguments : forall arguments d symbol,
  lookup_subst symbol (ground_arguments d arguments) =
  option_map (ground d) (lookup_subst symbol arguments).
Proof.
  induction arguments as [|[name tau] rest IH]; intros d symbol; cbn; [reflexivity|].
  destruct (Nat.eqb symbol name); [reflexivity|apply IH].
Qed.

Definition runtime_guard (d : environment) (schema : cast_spec) (arguments : subst) : bool :=
  match cast_handle schema with
  | None => true
  | Some (symbol, expected) => ty_eqb (ground d (type_argument arguments symbol)) expected
  end.

Definition guard_complete (schema : cast_spec) (arguments : subst) : Prop :=
  forall symbol expected, cast_handle schema = Some (symbol, expected) ->
    exists meaning, lookup_subst symbol arguments = Some meaning.

Lemma ground_guard_agrees : forall d schema arguments,
  guard_complete schema arguments ->
  cast_enabled schema (ground_arguments d arguments) = runtime_guard d schema arguments.
Proof.
  intros d schema arguments Complete. unfold cast_enabled, runtime_guard.
  destruct (cast_handle schema) as [[symbol expected]|] eqn:Guard; [|reflexivity].
  destruct (Complete symbol expected Guard) as [meaning Found].
  unfold type_argument. rewrite lookup_ground_arguments, Found. reflexivity.
Qed.

Module CastTyping (Primitives : PRIMITIVES).
Module R := Interpreter Primitives.

Theorem check_tag_preserves : forall W P H d tag arguments value payload,
  A.PayloadOf W tag (ground_arguments d arguments) payload ->
  value_typed W P H value payload ->
  (forall schema, lookup_cast tag (view_casts P) = Some schema ->
    guard_complete schema arguments /\
    (cast_enabled schema (ground_arguments d arguments) = true ->
      handle_type (cast_array schema) payload)) ->
  match R.check_tag P d tag arguments value with
  | Normal result => value_typed W P H result (TNom tag (ground_arguments d arguments))
  | Trap _ => True
  | _ => False
  end.
Proof.
  intros W P H d tag arguments value payload Payload Typed Schemas.
  unfold R.check_tag.
  destruct (lookup_cast tag (view_casts P)) as [schema|] eqn:Schema.
  - destruct (Schemas schema eq_refl) as [Complete Shape].
    assert (Guard :
      (match cast_handle schema with
       | None => true
       | Some (symbol, expected) =>
           ty_eqb (ground d (match lookup_subst symbol arguments with
             | Some meaning => meaning | None => TAbs symbol end)) expected
       end) = cast_enabled schema (ground_arguments d arguments)).
    { symmetry; apply ground_guard_agrees; exact Complete. }
    rewrite Guard.
    destruct (cast_enabled schema (ground_arguments d arguments)) eqn:Enabled.
    + destruct (lookup_subst (cast_held schema) arguments) as [held|] eqn:Held;
        destruct value; cbn; try exact I.
      destruct (Bool.eqb (cast_array schema) b && ty_eqb (ground d held) t) eqn:Check;
        cbn; [|exact I].
      apply andb_true_iff in Check as [Kind Meaning].
      apply ty_eqb_spec in Meaning.
      assert (KindEq : cast_array schema = b).
      { destruct (cast_array schema), b; cbn in Kind; congruence. }
      assert (MeaningEq : ground d held = t) by exact Meaning.
      rewrite <- KindEq, <- MeaningEq in Typed |- *.
      destruct (checked_header_typed W P H (cast_array schema) (ground d held) a payload Typed)
        as [Cell _].
      eapply TypedHeapTag with (schema := schema) (held := ground d held) (payload := payload).
      * exact Schema.
      * exact Enabled.
      * rewrite lookup_ground_arguments, Held. reflexivity.
      * exact Payload.
      * apply Shape; reflexivity.
      * constructor; exact Cell.
    + eapply TypedTag; [unfold ordinary_tag; rewrite Schema; exact Enabled|exact Payload|exact Typed].
  - eapply TypedTag; [unfold ordinary_tag; rewrite Schema; exact I|exact Payload|exact Typed].
Qed.

End CastTyping.
