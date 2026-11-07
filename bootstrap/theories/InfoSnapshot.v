(* An already-validated cache receipt can be read without trusting a newer
   cache value.  Installing it here is proof-only and preserves the real data. *)
From Stdlib Require Import List Arith Bool.
From NM Require Import NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve.
Definition info_snapshot(s:st)(key:nat)(info:fninfo):st:=
 s_info(fun m=>nset m key info)s.
Lemma info_snapshot_inv:forall s key info,Inv s->Inv(info_snapshot s key info).
Proof. intros s key info H;destruct H;constructor;assumption. Qed.
Lemma info_snapshot_cache:forall s key info,
 InfoInv s->InfoEntry s key info->InfoInv(info_snapshot s key info).
Proof.
 intros s key info Cache Entry;constructor.
 - intros k i Lookup;cbn[info_snapshot s_info st_info]in Lookup.
   rewrite nget_nset in Lookup;destruct(Nat.eqb_spec k key)as[->|Ne].
   + injection Lookup as <-;eapply info_entry_keep;[exact Entry|apply frl_info].
   + eapply info_entry_keep;[exact(Cache _ _ Lookup)|apply frl_info].
 - intros p id Lookup;eapply wrapper_entry_keep;[exact(info_wrappers _ Cache _ _ Lookup)|apply frl_info].
Qed.
Lemma info_snapshot_read:forall s g info,
 fn_info g(info_snapshot s(gs_id g)info)=Good(info,info_snapshot s(gs_id g)info).
Proof.
 intros;unfold fn_info,mbind,get;cbn[info_snapshot s_info st_info].
 rewrite nget_nset_same;reflexivity.
Qed.
