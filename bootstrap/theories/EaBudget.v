(* Compute one source budget for the executable dependency readers. *)
From Stdlib Require Import List Arith.
From NM Require AuditBase.
From NM Require Import Audit.
From NM Require Export EaDepth.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.

(* Only syntax consulted by a dependency reader contributes to this bound. *)
Definition declaration_depth (syntax : adecl) : nat :=
  match syntax with
  | ADFn _ _ parameters result _ =>
      Nat.max (largest (fun parameter => type_depth (snd parameter)) parameters)
        (match result with Some syntax => type_depth syntax | None => 0 end)
  | ADCtx _ items => largest spec_depth (ctx_specs items)
  | ADFunctor _ _ _ results _ => largest spec_depth results
  | _ => 0
  end.

Definition metadata_depth (W : world) (bound : nat) : nat :=
  Nat.max bound (largest (fun symbol => match w_decl W symbol with
    Some d => declaration_depth (d_ast d) | None => 0 end) (seq 0 bound)).

Definition metadata_budget (W : world) (bound : nat) : nat :=
  metadata_depth W bound + S bound * bound.
