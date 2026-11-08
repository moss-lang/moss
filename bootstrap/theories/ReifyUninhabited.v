(* A missing functor schema has no runtime inhabitants.  This lemma is
   independent of how source-only opaque interfaces are recorded. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel CoreMoss XTyping XEnv ReifyEntries XProvision.
Import ListNotations.

Lemma provision_missing_functor_impossible : forall Sg Rp P Fb H g A d u,
  provision_environment Sg Rp P Fb H g A d ->
  In (IFun u) A -> g_fun Sg u = None -> False.
Proof.
 intros Sg Rp P Fb H g A d u Env Present Missing.
 destruct (provision_lookup Sg Rp P Fb H g A d (IFun u) Env eq_refl Present)
   as [en [Lookup Entry]].
 unfold provision_entry in Entry.
 destruct (xp_immutable P); destruct en;
 cbn [reified_entry xdentry_agree] in Entry; try contradiction;
 rewrite Missing in Entry; contradiction.
Qed.
