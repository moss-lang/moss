(* ------------------------------------------------------------------------ *)
(* RelExamples.v — evidence that the DRAFT relation (Audit.v, Part V) says  *)
(* what is intended: for functions of tests/wasi/bridged.moss (and the      *)
(* app.moss and bridge.moss it imports), the elaborator's output is related  *)
(* to the source.  Method: RelSupport.v.  Stated, not proved: WorldOf b W   *)
(* for the elaborator's reading W (name resolution only).                    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit CoreMoss MossX NMap Sx Elab RelSupport BxBridged RelExampleData RelExampleWasm.
Import ListNotations.
Import Audit.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ===================== tests/wasi/bridged.moss ========================== *)

(* The unchanged elaboration result is shared with the context certificates. *)
Definition bridged := RelExampleData.bridged.
Definition P_br : xprogram := RelExampleData.P_br.
Definition S_br : st := RelExampleData.S_br.
Definition W_br : Elaboration.world := RelExampleData.W_br.


(* Reuse the checked Wasm subcontext rather than rebuilding its items for
   each function. All remaining goals use the ordinary relation tactics. *)
Ltac items_hook ::= exact wasm_entries.

(* The contexts of the functions involved, formed once: main and put
   assume Wasm, Wasi; bang assumes Wasm; app assumes MiniStd. *)
Definition cx_main : {C : cx & {ns : list item & CtxOf W_br 416 C ns}}.
Proof. eexists; eexists. ctxof. Defined.
Definition cx_put : {C : cx & {ns : list item & CtxOf W_br 412 C ns}}.
Proof. eexists; eexists. ctxof. Defined.
Definition cx_bang : {C : cx & {ns : list item & CtxOf W_br 413 C ns}}.
Proof. eexists; eexists. ctxof. Defined.
Definition cx_app : {C : cx & {ns : list item & CtxOf W_br 415 C ns}}.
Proof. eexists; eexists. ctxof. Defined.

Definition C_main := Eval vm_compute in projT1 cx_main.
Definition ns_main := Eval vm_compute in projT1 (projT2 cx_main).
Definition C_put := Eval vm_compute in projT1 cx_put.
Definition ns_put := Eval vm_compute in projT1 (projT2 cx_put).
Definition C_bang := Eval vm_compute in projT1 cx_bang.
Definition ns_bang := Eval vm_compute in projT1 (projT2 cx_bang).
Definition C_app := Eval vm_compute in projT1 cx_app.
Definition ns_app := Eval vm_compute in projT1 (projT2 cx_app).

Lemma ctx_main : CtxOf W_br 416 C_main ns_main.
Proof. exact (projT2 (projT2 cx_main) <: CtxOf W_br 416 C_main ns_main). Qed.
Lemma ctx_put : CtxOf W_br 412 C_put ns_put.
Proof. exact (projT2 (projT2 cx_put) <: CtxOf W_br 412 C_put ns_put). Qed.
Lemma ctx_bang : CtxOf W_br 413 C_bang ns_bang.
Proof. exact (projT2 (projT2 cx_bang) <: CtxOf W_br 413 C_bang ns_bang). Qed.
Lemma ctx_app : CtxOf W_br 415 C_app ns_app.
Proof. exact (projT2 (projT2 cx_app) <: CtxOf W_br 415 C_app ns_app). Qed.

Ltac ctx_tac ::= first [ exact ctx_main | exact ctx_put | exact ctx_bang | exact ctx_app | ctxof ].

Definition app_mod : string := "tests/wasi/app.moss".
Example app_symbol : resolve_sym W_br app_mod ["app"] = Some 415. Proof. vm_compute. reflexivity. Qed.


(* app.moss:  assume MiniStd { fn app() { emit(greeting); emit(greeting); } }
   — [D20] the context MiniStd = Text, emit, greeting; two calls of an
   abstract fn on an assumed val. *)
Theorem app_elab : FnElab W_br P_br 415.
Proof. fnelab app_mod TUnit (@nil (Input.name * nat)). Qed.

(* bridged.moss:  assume Wasm, Wasi { fn main() { bind WasiMini; app(); } }
   — [D55] a functor with a body applied by name: its three binds run
   here, in bridge.moss's terms: [D72] `bind mini::Text = I32;` (no code;
   Text now means I32), a fn bind to the defined `put` (its needs answered
   from main's context), a val bind to a call of `bang`; then [D20] the
   call `app()` with its needs (emit, greeting) answered by the bound
   provisions, coherently (emit was provided at Text = I32, and app needs
   it at Text, read here as I32). *)
Theorem main_elab : FnElab W_br P_br 416.
Proof. fnelab "tests/wasi/bridged.moss"%string TUnit (@nil (Input.name * nat)). Qed.

(* bridge.moss:  assume Wasm { fn bang(): I32 { let one = w::i32_one;
     let two = w::i32_add(one, one); let five = w::i32_add(w::i32_add(two, two), one);
     w::i32_add(w::i32_shl(one, five), one) } }
   — locals numbered in order, platform fns called as abstract fns. *)
Theorem bang_elab : FnElab W_br P_br 413.
Proof. fnelab "tests/wasi/bridge.moss"%string (TAbs 172) [("one", 0); ("two", 1); ("five", 2)]. Qed.

(* bridge.moss:  assume Wasm, Wasi { fn put(c: I32) { … } } — a parameter
   (numbered 0) and four statements of memory instructions and fd_write. *)
Theorem put_elab : FnElab W_br P_br 412.
Proof. fnelab "tests/wasi/bridge.moss"%string TUnit [("c", 0); ("one", 1); ("four", 2); ("eight", 3)]. Qed.
