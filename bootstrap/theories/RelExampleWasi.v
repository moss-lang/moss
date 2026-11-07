(* Main and put read Wasi after Wasm, whose type map is threaded into
   these items. Check the common derivation once and reuse its certificate. *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit CoreMoss MossX NMap Sx Elab RelSupport BxBridged.
From NM Require Import RelExampleData RelExampleWasm RelSimpleItems.
Import ListNotations.
Import Audit.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Ltac items_hook ::= scalar_items_prefix; items.

Definition wasi_specs := Eval vm_compute in
  match w_decl W_br 407 with
  | Some d => match d_ast d with ADCtx _ its => its | _ => [] end
  | None => []
  end.

Definition wasi_witness : {es : list entry & {tm : list (nat * ty) &
  ItemsEntries W_br "lib/wasip1.moss" [] wasm_tm wasi_specs es tm}}.
Proof. eexists; eexists; items. Defined.
Definition wasi_es := Eval vm_compute in projT1 wasi_witness.
Definition wasi_tm := Eval vm_compute in projT1 (projT2 wasi_witness).

Lemma wasi_entries :
  ItemsEntries W_br "lib/wasip1.moss" [] wasm_tm wasi_specs wasi_es wasi_tm.
Proof. exact (projT2 (projT2 wasi_witness)). Qed.
