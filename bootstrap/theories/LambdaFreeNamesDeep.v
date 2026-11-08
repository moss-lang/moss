(* Direct AST control: lexical capture never silently drops a deep suffix. *)
From Stdlib Require Import List String Arith.
From NM Require Import ExecutionModel Sx Elab.
Import ListNotations.
Local Open Scope string_scope.
Fixpoint deep_local_reference(depth:nat):aexpr:=
 match depth with
 |0=>AEPath["captured"]None
 |S n=>AEBlock(ABlock[](Some(deep_local_reference n)))
 end.
Lemma deep_local_reference_names:forall depth,
 free_names_complete_e(deep_local_reference depth)=["captured"].
Proof. induction depth;cbn[deep_local_reference free_names_complete_e free_names_complete_b flat_map List.app];[reflexivity|exact IHdepth]. Qed.
Example capture_beyond_old_depth_limit:
 free_names_e 10000(deep_local_reference 10001)=["captured"].
Proof. apply deep_local_reference_names. Qed.
Example compatibility_fuel_does_not_truncate:
 free_names_e 0(deep_local_reference 10001)=["captured"].
Proof. apply deep_local_reference_names. Qed.
Lemma deep_local_reference_is_mentioned:forall depth,
 ExecutionModel.Elaboration.Mentions_name(deep_local_reference depth)"captured".
Proof.
 induction depth;[apply ExecutionModel.Elaboration.MNPath|].
 eapply ExecutionModel.Elaboration.MNSub;[apply ExecutionModel.Elaboration.SubBlockT|exact IHdepth].
Qed.
