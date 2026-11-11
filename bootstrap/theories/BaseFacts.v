(* Proofs and transparent proof interfaces specific to AuditBase. *)
From Stdlib Require Import List.
From NM Require Import Audit AuditBase PublicValueTyping.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Export AuditBase.

(* A Public instance, kept transparent, for the same reason: CORRECT's
   obligations are stated inside one particular instance. *)
Module Type PUBLIC (Host : HOST).
  Include Public Host.
End PUBLIC.

(* The Base adapter changes a host result's wrapper, while retaining exactly
   the state returned by the raw primitive. *)
Section BaseCallFacts.
Import Audit.Semantics.

Lemma base_call_state : forall S
    (primitive : program_view -> op -> list value -> S -> signal * S)
    P operation held ws st,
  snd (BaseCall.call primitive P operation held ws st) =
  snd (primitive P operation
    (match decode operation with
     | Some p => BaseCall.primitive_arguments p ws
     | None => ws
     end) st).
Proof.
  intros S primitive P operation held ws st. unfold BaseCall.call.
  destruct (primitive P operation
    (match decode operation with
     | Some p => BaseCall.primitive_arguments p ws
     | None => ws end) st) as [out next].
  destruct held, (decode operation); reflexivity.
Qed.

Lemma base_call_preserves : forall S
    (primitive : program_view -> op -> list value -> S -> signal * S)
    P (ok : S -> Prop),
  (forall operation ws st, ok st -> ok (snd (primitive P operation ws st))) ->
  forall operation held ws st, ok st ->
    ok (snd (BaseCall.call primitive P operation held ws st)).
Proof.
  intros S primitive P ok Keeps operation held ws st Valid.
  rewrite base_call_state. apply Keeps. exact Valid.
Qed.
End BaseCallFacts.

(* The generic static contract regroups exactly the previous Base conditions. *)
Section StaticLinkingFacts.
Import Audit.Semantics.
Lemma platform_linked_fields : forall W P,
  Platform.Linked W P <->
  Platform.EntryPoint W P /\ Platform.Layouts W /\ Platform.Representations W P /\
  exists context needs types entries,
    CtxOf W (program_main P) context needs /\ InitialTypes (program_machines P) context types /\
    program_environment P = Environment types entries /\
    Forall (fun entry => Platform.Entry W P context (fst entry) (snd entry)) entries /\
    Forall (fun entry => Platform.PlatformEntry W P (fst entry) (snd entry)) entries.
Proof.
  intros W P. unfold Platform.Linked, Platform.Link.Linked, Platform.Static.EntryABI,
    Platform.Static.Tables, Platform.Static.Provides, Platform.EntryABI, Platform.Tables, Platform.Provides.
  split.
  - intros [ABI [[Layouts Representations] (C & ns & types & entries & Ctx & Types & Env & Provisions)]].
    split; [exact ABI|]. split; [exact Layouts|]. split; [exact Representations|].
    exists C, ns, types, entries. split; [exact Ctx|]. split; [exact Types|]. split; [exact Env|].
    rewrite Forall_forall in Provisions. split; rewrite Forall_forall; intros entry Member;
      [exact (proj1 (Provisions entry Member))|exact (proj2 (Provisions entry Member))].
  - intros (ABI & Layouts & Representations & C & ns & types & entries & Ctx & Types & Env & Entries & Native).
    split; [exact ABI|]. split; [split; assumption|].
    exists C, ns, types, entries. split; [exact Ctx|]. split; [exact Types|]. split; [exact Env|].
    rewrite Forall_forall in Entries, Native |- *. intros entry Member. split; [apply Entries|apply Native]; exact Member.
Qed.
End StaticLinkingFacts.
