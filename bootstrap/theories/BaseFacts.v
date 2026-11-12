(* Proofs and transparent proof interfaces specific to AuditBase. *)
From Stdlib Require Import List.
From NM Require Import Audit AuditBase PublicValueTyping.
Import AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.
Export AuditBase.

Lemma heap_arguments_match_force : forall p site values,
  BaseCall.heap_arguments_match p site values =
  BaseCall.heap_arguments_match p (fun _ => site tt) values.
Proof. intros []; reflexivity. Qed.

Module DeferredBaseCall.
(* Only allocation attaches a type header to the raw host result. *)
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
  match BaseCall.cast_at_site decoded with
  | Some (array, tag) => (BaseCall.cast_result array tag (site tt) values, state)
  | _ =>
      if BaseCall.heap_call_ready decoded site values then
        let arguments := match decoded with Some p => BaseCall.arguments_at_site p site values | None => values end in
        let '(out, next) := primitive P operation arguments state in
        (match decoded with Some p => result_at_site p site out | None => out end, next)
      else (Trap 37, state)
  end.

Theorem call_agrees : forall S
    (primitive : program_view -> op -> list value -> S -> signal * S)
    P operation site values state,
  call primitive P operation site values state =
  BaseCall.call primitive P operation (site tt) values state.
Proof.
  intros. unfold call, BaseCall.call.
  destruct (decode operation) as [p|].
  - cbn [BaseCall.cast_at_site BaseCall.heap_call_ready].
    destruct (BaseCall.cast_operation p) as [[array tag]|]; [reflexivity|].
    rewrite heap_arguments_match_force. destruct (BaseCall.heap_arguments_match p _ values); [|reflexivity].
    assert (Arguments : BaseCall.arguments_at_site p site values =
      BaseCall.arguments_at_site p (fun _ => site tt) values).
    { unfold BaseCall.arguments_at_site, BaseCall.memory_arguments.
      destruct (BaseCall.memory_operation p); reflexivity. }
    rewrite Arguments.
    destruct (primitive P operation (BaseCall.arguments_at_site p (fun _ => site tt) values) state) as [out next].
    rewrite result_at_site_agrees. now destruct (site tt).
  - destruct (primitive P operation values state) as [out next]. now destruct (site tt).
Qed.
End DeferredBaseCall.

Lemma split_memory_arguments_at_site : forall p site values,
  BaseCall.split_memory_arguments p (BaseCall.memory_arguments p site values) =
  (if BaseCall.memory_operation p then site tt else None, values).
Proof.
  intros. unfold BaseCall.split_memory_arguments, BaseCall.memory_arguments.
  destruct (BaseCall.memory_operation p), (site tt); reflexivity.
Qed.

Lemma memory_operation_gc : forall p,
  Heap.is_gc p = true -> BaseCall.memory_operation p = false.
Proof. intros []; cbn; congruence. Qed.

Lemma arguments_at_site_gc : forall p site values,
  Heap.is_gc p = true ->
  BaseCall.arguments_at_site p site values = BaseCall.primitive_arguments p values.
Proof.
  intros. unfold BaseCall.arguments_at_site, BaseCall.memory_arguments.
  now rewrite memory_operation_gc.
Qed.


(* A Public instance, kept transparent, for the same reason: CORRECT's
   obligations are stated inside one particular instance. *)
Module Type PUBLIC (Host : HOST).
  Include Public Host.
End PUBLIC.

(* The Base adapter leaves state alone when it handles a call itself.
   Otherwise it keeps the state returned by the raw host primitive. *)
Section BaseCallFacts.
Import AuditBase.Semantics.

Lemma base_call_state : forall S
    (primitive : program_view -> op -> list value -> S -> signal * S)
    P operation held ws st,
  snd (BaseCall.call primitive P operation held ws st) =
  let decoded := decode operation in
  match BaseCall.cast_at_site decoded with
  | Some _ => st
  | None =>
      if BaseCall.heap_call_ready decoded (fun _ => held) ws then
        snd (primitive P operation
          (match decoded with
           | Some p => BaseCall.arguments_at_site p (fun _ => held) ws
           | None => ws
           end) st)
      else st
  end.
Proof.
  intros S primitive P operation held ws st. unfold BaseCall.call.
  destruct (decode operation) as [p|]; cbn [BaseCall.cast_at_site BaseCall.heap_call_ready].
  - destruct (BaseCall.cast_operation p) as [[array tag]|]; [reflexivity|].
    destruct (BaseCall.heap_arguments_match p _ ws); [|reflexivity].
    destruct (primitive P operation _ st), held; reflexivity.
  - destruct (primitive P operation ws st), held; reflexivity.
Qed.

Lemma base_call_preserves : forall S
    (primitive : program_view -> op -> list value -> S -> signal * S)
    P (ok : S -> Prop),
  (forall operation ws st, ok st -> ok (snd (primitive P operation ws st))) ->
  forall operation held ws st,
    ok st -> ok (snd (BaseCall.call primitive P operation held ws st)).
Proof.
  intros S primitive P ok Keeps operation held ws st Valid.
  rewrite base_call_state. destruct (decode operation) as [p|];
    cbn [BaseCall.cast_at_site BaseCall.heap_call_ready]; [|auto].
  destruct (BaseCall.cast_operation p) as [[array tag]|]; [exact Valid|].
  destruct (BaseCall.heap_arguments_match p _ ws); auto.
Qed.

End BaseCallFacts.

(* The generic static contract regroups exactly the previous Base conditions. *)
Section StaticLinkingFacts.
Import AuditBase.Semantics.
Lemma platform_linked_fields : forall W P,
  Platform.Linked W P <->
  Platform.EntryPoint W P /\ Platform.Layouts W /\ Platform.Representations W P /\
  exists context needs types entries,
    CtxOf W (program_main P) context needs /\ AuditBase.initial_types (program_machines P) context types /\
    program_environment P = Environment types entries /\
    Forall (fun entry => Platform.Entry W P context (fst entry) (snd entry)) entries /\
    Forall (fun entry => Platform.PlatformEntry W P (fst entry) (snd entry)) entries /\
    w_root_item W = Platform.root_item /\ Compilation.Admissible W P.
Proof.
  intros W P. unfold Platform.Linked, Platform.Link.Linked, Platform.Static.EntryABI,
    Platform.Static.Tables, Platform.Static.Provides, Platform.EntryABI, Platform.Tables, Platform.Provides.
  split.
  - intros (Root & ABI & Admissible & [Layouts Representations] & C & ns & types & entries & Ctx & Types & Env & Provisions).
    split; [exact ABI|]. split; [exact Layouts|]. split; [exact Representations|].
    exists C, ns, types, entries. split; [exact Ctx|]. split; [exact Types|]. split; [exact Env|].
    rewrite Forall_forall in Provisions. split.
    + rewrite Forall_forall. intros entry Member. exact (proj1 (Provisions entry Member)).
    + split; [|exact (conj Root Admissible)]. rewrite Forall_forall. intros entry Member.
      exact (proj2 (Provisions entry Member)).
  - intros (ABI & Layouts & Representations & C & ns & types & entries & Ctx & Types & Env & Entries & Native & Root & Admissible).
    split; [exact Root|]. split; [exact ABI|]. split; [exact Admissible|]. split; [split; assumption|].
    exists C, ns, types, entries. split; [exact Ctx|]. split; [exact Types|]. split; [exact Env|].
    rewrite Forall_forall in Entries, Native |- *. intros entry Member. split; [apply Entries|apply Native]; exact Member.
Qed.
End StaticLinkingFacts.
