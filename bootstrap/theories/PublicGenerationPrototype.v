(* Review prototype only. This does not alter Audit, implement its
   interpreter, or assert source/interpreter refinement. Generation symbols
   are stable within an activation; aliases are already-read source/output
   meanings. A new generic activation may have another generation map. *)
From Stdlib Require Import List PeanoNat.
From NM Require Import Audit PublicCaptureFacts.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Module GenerationPrototype.

Record maps := Maps {
  generations : subst;
  aliases : subst
}.

Definition ground (d : maps) (tau : ty) : ty :=
  app_subst (generations d) tau.

(* rhs is a statically resolved generation expression, not a source name
   which this operation should resolve through the aliases map again. *)
Definition bind_alias (d : maps) (source : tsym) (rhs : ty) : maps :=
  Maps (generations d) ((source, ground d rhs) :: aliases d).

Theorem alias_preserves_generation_map : forall d source rhs,
  generations (bind_alias d source rhs) = generations d.
Proof. reflexivity. Qed.

Theorem alias_preserves_ground : forall d source rhs tau,
  ground (bind_alias d source rhs) tau = ground d tau.
Proof. reflexivity. Qed.

Theorem alias_records_ground_rhs : forall d source rhs,
  lookup_subst source (aliases (bind_alias d source rhs)) = Some (ground d rhs).
Proof. intros; cbn [bind_alias aliases lookup_subst]; rewrite Nat.eqb_refl; reflexivity. Qed.

(* An unchanged formal can be an output without an explicit alias binding.
   An explicit final alias takes precedence over that formal meaning. *)
Definition output_meaning (body : maps) (role : tsym) : option ty :=
  match lookup_subst role (aliases body) with
  | Some meaning => Some meaning
  | None => lookup_subst role (generations body)
  end.

(* Output roles and caller destinations are separate from formal inputs.
   Every read observes this one completed body snapshot. *)
Fixpoint read_outputs (body : maps) (roles : list (tsym * tsym)) : option subst :=
  match roles with
  | [] => Some []
  | (role, destination) :: rest =>
      match output_meaning body role, read_outputs body rest with
      | Some meaning, Some writes => Some ((destination, meaning) :: writes)
      | _, _ => None
      end
  end.

(* This phase consumes values already read, never type expressions which
   could be reinterpreted by an earlier write. Existing and repeated
   destinations are rejected, even when their proposed meanings agree. *)
Fixpoint install_fresh (writes : subst) (stable : subst) : option subst :=
  match writes with
  | [] => Some stable
  | (destination, meaning) :: rest =>
      match lookup_subst destination stable with
      | Some _ => None
      | None => install_fresh rest ((destination, meaning) :: stable)
      end
  end.

Definition export_snapshot (caller : maps) (writes : subst) : option maps :=
  match install_fresh writes (generations caller) with
  | Some stable => Some (Maps stable (aliases caller))
  | None => None
  end.

(* Only generation storage is transferred here. Updating the caller's
   source/output reader after an application is an outer-layer obligation;
   this prototype deliberately leaves its aliases unchanged. *)
Definition transfer_outputs (body caller : maps) (roles : list (tsym * tsym)) : option maps :=
  match read_outputs body roles with
  | Some writes => export_snapshot caller writes
  | None => None
  end.

Theorem output_reads_describe_snapshot : forall body roles writes,
  read_outputs body roles = Some writes ->
  Forall2 (fun role write => fst write = snd role /\
    output_meaning body (fst role) = Some (snd write)) roles writes.
Proof.
  intros body roles. induction roles as [|[role destination] rest IH]; intros writes Read.
  - cbn in Read. inversion Read; constructor.
  - cbn in Read. destruct (output_meaning body role) as [meaning|] eqn:Meaning;
      destruct (read_outputs body rest) as [tail|] eqn:Tail; try discriminate.
    inversion Read; subst writes. constructor.
    + cbn; split; [reflexivity|exact Meaning].
    + apply IH; reflexivity.
Qed.

Theorem reads_precede_export : forall body caller roles result,
  transfer_outputs body caller roles = Some result ->
  exists writes, read_outputs body roles = Some writes /\
    export_snapshot caller writes = Some result.
Proof.
  intros body caller roles result Transfer. unfold transfer_outputs in Transfer.
  destruct (read_outputs body roles) as [writes|] eqn:Read; [|discriminate].
  exists writes; split; [reflexivity|exact Transfer].
Qed.

Lemma install_fresh_preserves : forall writes before after,
  install_fresh writes before = Some after ->
  forall symbol meaning, lookup_subst symbol before = Some meaning ->
    lookup_subst symbol after = Some meaning.
Proof.
  induction writes as [|[destination value] rest IH]; intros before after Installed symbol meaning Found.
  - cbn in Installed; inversion Installed; subst; exact Found.
  - cbn in Installed. destruct (lookup_subst destination before) eqn:Fresh; [discriminate|].
    eapply IH; [exact Installed|]. cbn [lookup_subst].
    destruct (Nat.eqb symbol destination) eqn:Same; [|exact Found].
    apply Nat.eqb_eq in Same; subst symbol. rewrite Fresh in Found; discriminate.
Qed.

Lemma install_fresh_installs : forall writes before after,
  install_fresh writes before = Some after ->
  Forall (fun write => lookup_subst (fst write) after = Some (snd write)) writes.
Proof.
  induction writes as [|[destination value] rest IH]; intros before after Installed.
  - constructor.
  - cbn in Installed. destruct (lookup_subst destination before) eqn:Fresh; [discriminate|].
    constructor.
    + cbn. eapply install_fresh_preserves; [exact Installed|].
      cbn [lookup_subst]; rewrite Nat.eqb_refl; reflexivity.
    + eapply IH; exact Installed.
Qed.

Theorem fresh_export_preserves_existing_generation : forall caller writes result,
  export_snapshot caller writes = Some result ->
  forall symbol meaning, lookup_subst symbol (generations caller) = Some meaning ->
    lookup_subst symbol (generations result) = Some meaning.
Proof.
  intros caller writes result Export symbol meaning Found.
  unfold export_snapshot in Export.
  destruct (install_fresh writes (generations caller)) as [stable|] eqn:Installed; [|discriminate].
  inversion Export; subst result; cbn. eapply install_fresh_preserves; eassumption.
Qed.

Theorem fresh_export_installs_snapshot : forall caller writes result,
  export_snapshot caller writes = Some result ->
  Forall (fun write => lookup_subst (fst write) (generations result) = Some (snd write)) writes.
Proof.
  intros caller writes result Export. unfold export_snapshot in Export.
  destruct (install_fresh writes (generations caller)) as [stable|] eqn:Installed; [|discriminate].
  inversion Export; subst result; cbn. eapply install_fresh_installs; exact Installed.
Qed.

Theorem fresh_export_preserves_aliases : forall caller writes result,
  export_snapshot caller writes = Some result -> aliases result = aliases caller.
Proof.
  intros caller writes result Export. unfold export_snapshot in Export.
  destruct (install_fresh writes (generations caller)); inversion Export; reflexivity.
Qed.

(* Completeness is essential: adding a previously absent generation can
   change grounding of a type mentioning that absent generation. *)
Theorem fresh_export_preserves_supported_type : forall caller writes result tau,
  export_snapshot caller writes = Some result ->
  (forall symbol, In symbol (A.gens tau) ->
    exists meaning, lookup_subst symbol (generations caller) = Some meaning) ->
  ground result tau = ground caller tau.
Proof.
  intros caller writes result tau Export Complete. unfold ground.
  apply substitution_agrees_on_generations. intros symbol Member.
  destruct (Complete symbol Member) as [meaning Found].
  rewrite (fresh_export_preserves_existing_generation caller writes result Export symbol meaning Found), Found.
  reflexivity.
Qed.

(* Saved=2, Boxed=3, F64=4, I32=5. Alias RHS expressions have already been
   read by the static source context, just as BTypeBind currently emits. *)
Definition initial := Maps [(4,TMachine 4);(5,TMachine 5)] [].
Definition saved := bind_alias initial 2 (TAbs 4).
Definition rebound := bind_alias saved 4 (TAbs 5).
Definition boxed := bind_alias rebound 3 (TAbs 4).

Example saved_retains_f64 : output_meaning boxed 2 = Some (TMachine 4).
Proof. reflexivity. Qed.

Example rebound_source_reads_i32 : output_meaning boxed 4 = Some (TMachine 5).
Proof. reflexivity. Qed.

Example old_f64_generation_is_stable : ground boxed (TAbs 4) = TMachine 4.
Proof. reflexivity. Qed.

Example boxed_uses_saved_f64 : output_meaning boxed 3 = Some (TMachine 4).
Proof. reflexivity. Qed.

Example shadowing_output_has_fresh_destination :
  transfer_outputs rebound initial [(4,6)] =
    Some (Maps [(6,TMachine 5);(4,TMachine 4);(5,TMachine 5)] []).
Proof. reflexivity. Qed.

Example old_output_destination_is_rejected : transfer_outputs rebound initial [(4,4)] = None.
Proof. reflexivity. Qed.

Example duplicate_output_destination_is_rejected :
  transfer_outputs rebound initial [(4,6);(2,6)] = None.
Proof. reflexivity. Qed.

Example unchanged_formal_is_exportable :
  transfer_outputs initial (Maps [] []) [(4,6)] = Some (Maps [(6,TMachine 4)] []).
Proof. reflexivity. Qed.

(* Reading role 6 uses the completed body, even though an earlier output is
   destined for caller generation 6. Its read cannot be affected by export. *)
Definition simultaneous_body := Maps [(4,TMachine 4);(6,TMachine 5)] [].
Example outputs_read_simultaneously :
  transfer_outputs simultaneous_body (Maps [] []) [(4,6);(6,7)] =
    Some (Maps [(7,TMachine 5);(6,TMachine 4)] []).
Proof. reflexivity. Qed.

End GenerationPrototype.
