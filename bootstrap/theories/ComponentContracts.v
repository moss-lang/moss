(* The source-world evidence shared by loader and requirements proofs. *)
From Stdlib Require Import List String.
From NM Require Export ExecutionModel.
Import ListNotations.

Module SourceReading.
Local Unset Implicit Arguments.
Import Input Elaboration.

(* The fields of WorldOf about the loader: everything but requirements and
   field numbering. *)
Record LoaderReading (b : bundle) (W : world) : Prop := {
  lr_acyclic : exists rank : string -> nat, forall p m t imp,
      Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) ->
      module_of b t <> None /\ rank t < rank p;
  lr_decl_sound : forall s d, w_decl W s = Some d -> s <> 0 /\ Declared b d /\ kind_of (d_ast d) <> None;
  lr_decl_complete : forall d, Declared b d -> kind_of (d_ast d) <> None ->
      exists s, w_decl W s = Some d /\ forall s', w_decl W s' = Some d -> s' = s;
  lr_exports : forall p ns n e, Loaded b p -> (w_exp W p ns n = Some e <-> Exports b W p ns n e);
  lr_scope : forall p ns n e, Loaded b p -> (w_scope W p ns n = Some e <-> InScope b W p ns n e);
  lr_attached : forall r n s, w_attached W r n = Some s <-> AttachedDecl W r n s;
  lr_library : w_library W = Loader.library_dir (b_prelude b)
}.

(* The rest: requirements ([D71]/[D73]) and field numbering. *)
Record ReqsReading (W : world) : Prop := {
  rr_nodup : forall s, NoDup (w_reqs W s);
  rr_plain : forall s k, kind_sym W s = Some k ->
      k <> DKTag -> k <> DKAlias -> k <> DKType -> k <> DKUnit ->
      forall t, In t (w_reqs W s) <-> AssumeTypes W s t;
  rr_none : forall s, kind_sym W s = Some DKType \/ kind_sym W s = Some DKUnit -> w_reqs W s = [];
  rr_closed : RelevanceClosed W (w_reqs W);
  rr_least : forall R, RelevanceClosed W R ->
      (forall s, kind_sym W s <> Some DKTag -> kind_sym W s <> Some DKAlias -> R s = w_reqs W s) ->
      forall g, (kind_sym W g = Some DKTag \/ kind_sym W g = Some DKAlias) -> incl (w_reqs W g) (R g);
  rr_listed : forall g d ps t n, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) -> In n ps ->
      exists x, resolve_sym W (d_mod d) [n] = Some x /\ AssumeTypes W g x
}.

End SourceReading.
