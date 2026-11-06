(* Type-only dependency telescopes are genuine D20 inputs, not unchecked
   side conditions on newly allocated value and function provisions. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss ReifyFresh ReifyInterfaces.
Import ListNotations.
Lemma type_telescope_requirements : forall xs,tyreqs(map ITy xs)=xs.
Proof. induction xs;cbn;[reflexivity|f_equal;exact IHxs]. Qed.
Lemma context_type_is_available : forall Phi t,
 In t(tyreqs(cA Phi))->avail Phi(ITy t).
Proof.
 intros Phi t Member;unfold tyreqs in Member.
 apply in_flat_map in Member as[i[Present Member]].
 destruct i as[u|v|f|r m th|u];cbn in Member;try contradiction.
 destruct Member as[Equal|[]];subst u.
 unfold avail,avail_b;apply existsb_exists;exists(ITy t);split;[exact Present|].
 unfold item_subst_eqb;cbn;apply Nat.eqb_refl.
Qed.
Lemma type_telescope_sat0 : forall Sg Phi xs,
 NoDup xs->Forall(fun t=>avail Phi(ITy t))xs->sat0 Sg Phi(map ITy xs).
Proof.
 intros Sg Phi xs Unique Available.
 exists(mkCtx(map ITy xs)[]);split.
 - apply distinct_slot_telescope_context;rewrite map_map.
   change(NoDup(map(fun x=>x)xs));rewrite map_id;exact Unique.
 - split.
   + unfold sat_at;cbn[cA cS];split.
     * unfold id_app;rewrite map_map;cbn;rewrite map_id;reflexivity.
     * split;[|constructor]. apply Forall_forall;intros i Hin.
       apply in_map_iff in Hin as[t[<- _]];left;reflexivity.
   + cbn[cA];rewrite type_telescope_requirements;exact Available.
Qed.
Lemma value_dependency_telescope_sat0 : forall Sg Phi tau,
 wf_ty Sg Phi tau->sat0 Sg Phi(value_type_inputs tau).
Proof.
 intros Sg Phi tau Well;unfold value_type_inputs;apply type_telescope_sat0.
 - apply NoDup_nodup.
 - apply Forall_forall;intros t Member;apply context_type_is_available.
   apply nodup_In in Member;apply(wf_ty_frees Well);exact Member.
Qed.
Lemma signature_dependency_telescope_sat0 : forall Sg Phi signature,
 Forall(wf_ty Sg Phi)(fs_params signature)->wf_ty Sg Phi(fs_ret signature)->
 sat0 Sg Phi(signature_type_inputs signature).
Proof.
 intros Sg Phi signature ParamIns Result;unfold signature_type_inputs;apply type_telescope_sat0.
 - apply NoDup_nodup.
 - apply Forall_forall;intros t Member;apply nodup_In in Member.
   unfold msig_frees in Member;apply in_flat_map in Member as[tau[Present Occurs]].
   apply in_app_or in Present as[ParamIn|[<-|[]]].
   + apply context_type_is_available;apply Forall_forall with(x:=tau)in ParamIns;[|exact ParamIn].
     apply(wf_ty_frees ParamIns);exact Occurs.
   + apply context_type_is_available;apply(wf_ty_frees Result);exact Occurs.
Qed.

Lemma type_telescope_sat : forall Sg Phi xs theta,
 NoDup xs->map fst theta=xs->sat Sg Phi theta(map ITy xs).
Proof.
 intros Sg Phi xs theta Unique Domain;exists(mkCtx(map ITy xs)[]);split.
 - apply distinct_slot_telescope_context;rewrite map_map.
   change(NoDup(map(fun x=>x)xs));rewrite map_id;exact Unique.
 - unfold sat_at;cbn[cA cS];split.
   + rewrite type_telescope_requirements;exact Domain.
   + split;[|constructor]. apply Forall_forall;intros i Hin.
     apply in_map_iff in Hin as[t[<- _]];left;reflexivity.
Qed.

Lemma formed_type_telescope : forall Sg types,
 NoDup types->Forall(fun t=>g_ty Sg t=true/\t<>this_sym)types->
 wf_tele Sg empty_ctx(map ITy types)(mkCtx(map ITy types)[]).
Proof.
 intros Sg types Unique Declared.
 change(wf_tele Sg(mkCtx[][])(map ITy types)(mkCtx([]++map ITy types)[])).
 apply formed_distinct_slot_telescope.
 - cbn;rewrite map_map;change(NoDup(map(fun t=>t)types));rewrite map_id;exact Unique.
 - intros prefix item suffix Split.
   assert(Member:In item(map ITy types)).
   { rewrite Split;apply in_or_app;right;left;reflexivity. }
   apply in_map_iff in Member as[t[<- Present]].
   apply Forall_forall with(x:=t)in Declared;[|exact Present].
   destruct Declared;constructor;assumption.
Qed.
