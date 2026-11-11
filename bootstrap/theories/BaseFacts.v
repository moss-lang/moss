(* Proofs and transparent proof interfaces specific to AuditBase. *)
From Stdlib Require Import List.
From NM Require Import Audit AuditBase PublicValueTyping.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Export AuditBase.

Module DeferredBaseCall.
(* Base only consults the method's receiver type when allocating a
   fresh reference header. *)
Definition result_at_site (p : wasm_prim) (site : unit -> option ty) (out : signal) : signal :=
  match p with
  | PBox _ | PArrNew _ =>
      match site tt with Some t => BaseCall.allocation_result p t out | None => out end
  | _ => out
  end.

Lemma result_at_site_agrees : forall p site out,
  result_at_site p site out =
  match site tt with Some t => BaseCall.allocation_result p t out | None => out end.
Proof. intros [] site out; unfold result_at_site; destruct (site tt), out; reflexivity. Qed.

Definition call {S : Type}
    (primitive : program_view -> op -> list value -> S -> signal * S)
    (P : program_view) (operation : op) (site : unit -> option ty)
    (values : list value) (state : S) : signal * S :=
  let decoded := decode operation in
  let arguments := match decoded with Some p => BaseCall.primitive_arguments p values | None => values end in
  let '(out, next) := primitive P operation arguments state in
  (match decoded with Some p => result_at_site p site out | None => out end, next).

Theorem call_agrees : forall S
    (primitive : program_view -> op -> list value -> S -> signal * S)
    P operation site values state,
  call primitive P operation site values state =
  BaseCall.call primitive P operation (site tt) values state.
Proof.
  intros. unfold call, BaseCall.call.
  destruct (decode operation) as [p|].
  - destruct (primitive P operation (BaseCall.primitive_arguments p values) state) as [out next].
    rewrite result_at_site_agrees. now destruct (site tt).
  - destruct (primitive P operation values state) as [out next]. now destruct (site tt).
Qed.
End DeferredBaseCall.

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
