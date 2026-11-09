(* `Satisfies`'s merges clause is stated by class (criteria section 2,
   coherence; AUDIT-CHANGES #54): a callee class wholly in force at the
   caller holds of the caller's meanings.  Where every input of the callee
   is in force here (an application's, a family's, a copy's), that gives
   every equation of the callee's σ, which is the form the proofs use. *)
From Stdlib Require Import List.
From NM Require Import Audit PublicCallRealize.
From NM Require PublicOrientation.
Import Audit.Input Audit.Semantics ListNotations.
Import Audit.Semantics.Elaboration.

Theorem functx_merges : forall W u Cu ns C, FunCtx W u Cu ns ->
  (forall t, In t (map fst (c_ty Cu)) -> nlook t (c_ty C) <> None) ->
  (forall g, (forall y, (exists x, (x = g \/ In x (gens (canon Cu (TAbs g)))) /\
                          canon Cu (TAbs y) = canon Cu (TAbs x)) -> nlook y (c_ty C) <> None) ->
     canon C (app_subst (c_ty C) (TAbs g)) = canon C (app_subst (c_ty C) (canon Cu (TAbs g)))) ->
  forall g t, In (g, t) (c_sub Cu) ->
    (forall x, In x (g :: gens t) -> nlook x (c_ty C) <> None) ->
    canon C (app_subst (c_ty C) (TAbs g)) = canon C (app_subst (c_ty C) t).
Proof.
  intros W u Cu ns C FC Force Cls.
  exact (PublicOrientation.merges_by_pairs C Cu (map fst (c_ty Cu))
    (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (functx_scoped W u Cu ns FC)))))))
    (PublicOrientation.functx_sub_function W u Cu ns FC) Force Cls).
Qed.
