(* Context method items on aliases read the alias's underlying type. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel Sx Elab.
Import ListNotations.
Module A := ExecutionModel.Elaboration.

Example recvty_alias_reads_body : forall W tm r d n body t,
  A.nlook r tm = None -> A.w_decl W r = Some d -> A.d_ast d = ADAlias n body ->
  A.TyOf W (A.d_mod d) tm None body t -> A.RecvTy W r tm t.
Proof. intros W tm r d n body t T D E H. unfold A.RecvTy. rewrite T,D,E. exact H. Qed.
