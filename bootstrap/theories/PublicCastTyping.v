(* Checked tag construction preserves ground data typing. This local result
   does not assert that source typing supplies the required ground payload. *)
From Stdlib Require Import List Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit PublicTypeFacts PublicValueTyping.
Import AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Definition ground_arguments (d : environment) (arguments : subst) : subst :=
  map (fun q => (fst q, ground d (snd q))) arguments.

Lemma lookup_ground_arguments : forall arguments d symbol,
  lookup_subst symbol (ground_arguments d arguments) =
  option_map (ground d) (lookup_subst symbol arguments).
Proof.
  induction arguments as [|[name tau] rest IH]; intros d symbol; cbn; [reflexivity|].
  destruct (Nat.eqb symbol name); [reflexivity|apply IH].
Qed.

Definition CastTableFormed (W : A.world) (machines : list (tsym * machine_class))
    (casts : list (nsym * cast_spec)) : Prop :=
  forall tag schema, lookup_cast tag casts = Some schema ->
  A.kind_sym W tag = Some A.DKTag /\
  exists raw, cast_handle schema = Some (raw, TMachine raw) /\
    A.PayloadOf W tag (map (fun t => (t, TAbs t)) (A.w_reqs W tag)) (TAbs raw) /\
    A.nlook raw machines = Some (MReference (cast_kind schema)).

Lemma formed_cast_not_unit : forall W machines casts,
  CastTableFormed W machines casts -> forall tag,
  A.kind_sym W tag = Some A.DKUnit -> lookup_cast tag casts = None.
Proof.
  intros W machines casts Formed tag Unit.
  destruct (lookup_cast tag casts) as [schema|] eqn:Found; [|reflexivity].
  pose proof (proj1 (Formed tag schema Found)) as Tag. congruence.
Qed.
