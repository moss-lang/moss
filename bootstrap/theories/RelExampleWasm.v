(* Wasm's large item derivation is common to main, put and bang. Check it
   once, then expose its data and an opaque proof for the example tactics. *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit CoreMoss MossX NMap Sx Elab RelSupport BxBridged.
From NM Require Import RelExampleData RelSimpleItems.
Import ListNotations. Import Audit.Elaboration.
Local Open Scope string_scope. Local Open Scope list_scope.
Ltac items_hook ::= scalar_items_prefix; items.

Definition wasm_specs := Eval vm_compute in
 match w_decl W_br 351 with Some d => match d_ast d with ADCtx _ its => its | _ => [] end | None => [] end.
Definition wasm_witness : {es : list entry & {tm : list(nat*ty) & ItemsEntries W_br "lib/wasm.moss" [] [] wasm_specs es tm}}.
Proof. eexists;eexists;items. Defined.
Definition wasm_es := Eval vm_compute in projT1 wasm_witness.
Definition wasm_tm := Eval vm_compute in projT1(projT2 wasm_witness).
Lemma wasm_entries : ItemsEntries W_br "lib/wasm.moss" [] [] wasm_specs wasm_es wasm_tm.
Proof. exact (projT2(projT2 wasm_witness)). Qed.
