(* Family markers avoid every generation carried by the caller environment. *)
From Stdlib Require Import List Arith Lia Bool.
From NM Require Import ExecutionModel NMap Sx Elab ReifyData.
Import ListNotations.
Local Open Scope list_scope.
Lemma marker_max_contains:forall xs g,In g xs->g<fold_right(fun x n=>Nat.max(S x)n)0 xs.
Proof.
 intros xs;induction xs as[|x xs IH];intros g H;[contradiction|].
 cbn[fold_right];destruct H as[<-|H].
 - pose proof(Nat.le_max_l(S x)(fold_right(fun x n=>Nat.max(S x)n)0 xs));lia.
 - pose proof(IH _ H);pose proof(Nat.le_max_r(S x)(fold_right(fun x n=>Nat.max(S x)n)0 xs));lia.
Qed.
Lemma marker_max_below:forall xs bound,(forall g,In g xs->g<bound)->
 fold_right(fun x n=>Nat.max(S x)n)0 xs<=bound.
Proof.
 intros xs;induction xs as[|x xs IH];intros bound H;[cbn;lia|].
 cbn[fold_right];apply Nat.max_lub.
 - pose proof(H x(or_introl eq_refl));lia.
 - apply IH;intros g Hg;apply H;right;exact Hg.
Qed.
Lemma marker_scope_ids:forall e g,In g(reify_generation_ids e)->In g(marker_generation_ids e).
Proof. intros e g H;unfold marker_generation_ids;apply in_app_iff;left;exact H. Qed.
Lemma marker_scope_type:forall e t g,In t(reify_scope_types e)->In g(ety_gens t)->
 In g(marker_generation_ids e).
Proof.
 intros e t g Ht Hg;apply marker_scope_ids;unfold reify_generation_ids;rewrite dedup_nat_fast_eq;apply nodup_In.
 apply in_app_iff;right;apply in_app_iff;right;apply in_flat_map;exists t;split;assumption.
Qed.
Lemma marker_tn_key:forall e g,In g(map fst(en_tn e))->In g(marker_generation_ids e).
Proof.
 intros e g H;apply marker_scope_ids;unfold reify_generation_ids;rewrite dedup_nat_fast_eq;apply nodup_In.
 apply in_app_iff;left;exact H.
Qed.
Lemma marker_uf_key:forall e g,In g(map fst(en_uf e))->In g(marker_generation_ids e).
Proof.
 intros e g H;apply marker_scope_ids;unfold reify_generation_ids;rewrite dedup_nat_fast_eq;apply nodup_In.
 apply in_app_iff;right;apply in_app_iff;left;exact H.
Qed.
Lemma marker_app_receiver:forall e r mid raw stamp g,
 In(KyMth r mid,(raw,stamp))(en_apps e)->In g(ety_gens r)->In g(marker_generation_ids e).
Proof.
 intros e r mid raw stamp g H Hg;unfold marker_generation_ids;apply in_app_iff;right.
 apply in_flat_map;exists(KyMth r mid,(raw,stamp));split;[exact H|].
 cbn[fst snd];apply in_app_iff;left;exact Hg.
Qed.
Lemma marker_app_key:forall e k raw stamp g,
 In(k,(raw,stamp))(en_apps e)->In g(map fst raw)->In g(marker_generation_ids e).
Proof.
 intros e k raw stamp g H Hg;unfold marker_generation_ids;apply in_app_iff;right.
 apply in_flat_map;exists(k,(raw,stamp));split;[exact H|].
 cbn[fst snd];apply in_app_iff;right;exact Hg.
Qed.
Theorem fresh_marker_above:forall e s id sf,
 fresh_marker e s=Good(id,sf)->st_next s<=id/\st_next sf=S id/\
 forall g,In g(marker_generation_ids e)->g<id.
Proof.
 intros e s id sf E;unfold fresh_marker in E;injection E as <- <-.
 split;[apply Nat.le_max_l|];split;[reflexivity|].
 intros g H;pose proof(marker_max_contains _ _ H)as Bound.
 pose proof(Nat.le_max_r(st_next s)(marker_floor e));unfold marker_floor in *;lia.
Qed.
Corollary fresh_marker_avoids_scope:forall e s id sf,
 fresh_marker e s=Good(id,sf)->forall t,In t(reify_scope_types e)->forall g,In g(ety_gens t)->g<id.
Proof.
 intros e s id sf E t T g G;apply(proj2(proj2(fresh_marker_above _ _ _ _ E)));eapply marker_scope_type;eassumption.
Qed.
Theorem fresh_marker_old_behavior:forall e s,
 (forall g,In g(marker_generation_ids e)->g<st_next s)->fresh_marker e s=fresh s.
Proof.
 intros e s Bound;unfold fresh_marker,fresh;rewrite Nat.max_l;[reflexivity|].
 unfold marker_floor;apply marker_max_below;exact Bound.
Qed.
(* A future-looking caller generation cannot be confused with the new marker. *)
Definition marker_collision_env:env:=set_tn empty_env 1(YAbs 1000)0.
Example marker_avoids_future_caller_generation:forall s id sf,
 fresh_marker marker_collision_env s=Good(id,sf)->1000<id.
Proof.
 intros s id sf E;apply(fresh_marker_avoids_scope _ _ _ _ E(YAbs 1000));[|left;reflexivity].
 unfold reify_scope_types,marker_collision_env,tymap,set_tn,empty_env.
 cbn[en_tn en_uf map fst snd aset Nat.eqb];left;reflexivity.
Qed.

Theorem fresh_marker_for_above:forall e r s id sf,
 fresh_marker_for e r s=Good(id,sf)->
 (st_next s<=id/\st_next sf=S id/\forall g,In g(marker_generation_ids e)->g<id)/\
 forall g,In g(ety_gens r)->g<id.
Proof.
 intros e r s id sf E;unfold fresh_marker_for in E;injection E as <- <-.
 pose proof(Nat.le_max_l(st_next s)(Nat.max(marker_floor e)(fold_right(fun g top=>Nat.max(S g)top)0(ety_gens r))))as Current.
 pose proof(Nat.le_max_r(st_next s)(Nat.max(marker_floor e)(fold_right(fun g top=>Nat.max(S g)top)0(ety_gens r))))as Floor.
 pose proof(Nat.le_max_l(marker_floor e)(fold_right(fun g top=>Nat.max(S g)top)0(ety_gens r)))as Env.
 pose proof(Nat.le_max_r(marker_floor e)(fold_right(fun g top=>Nat.max(S g)top)0(ety_gens r)))as Receiver.
 split;[split;[exact Current|];split;[reflexivity|]|].
 - intros g Hin;pose proof(marker_max_contains _ _ Hin);unfold marker_floor in *;lia.
 - intros g Hin;eapply Nat.lt_le_trans;[exact(marker_max_contains _ _ Hin)|].
   eapply Nat.le_trans;[exact Receiver|exact Floor].
Qed.
Theorem fresh_marker_for_old_behavior:forall e r s,
 (forall g,In g(marker_generation_ids e)->g<st_next s)->
 (forall g,In g(ety_gens r)->g<st_next s)->fresh_marker_for e r s=fresh s.
Proof.
 intros e r s Env Receiver;unfold fresh_marker_for,fresh;rewrite Nat.max_l;[reflexivity|].
 apply Nat.max_lub;[unfold marker_floor|];apply marker_max_below;assumption.
Qed.
Example marker_avoids_receiver_outside_env:forall s id sf,
 fresh_marker_for empty_env(YAbs 1000)s=Good(id,sf)->1000<id.
Proof.
 intros s id sf E;apply(proj2(fresh_marker_for_above _ _ _ _ _ E));left;reflexivity.
Qed.
