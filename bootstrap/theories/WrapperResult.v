(* Every successful eta-wrapper query returns the row it actually caches. *)
From Stdlib Require Import List.
From NM Require Import NMap Sx Elab ElabSound ElabFrame WorldSound InfoInvariant InfoPreserve.
Lemma wrapper_create_stored:forall pv s id sf,
 wrapper_create pv s=Good(id,sf)->nget(st_wrappers sf)(gs_id pv)=Some id.
Proof.
 intros pv s id sf Run;unfold wrapper_create in Run.
 do 6 (apply bind_inv in Run as(?&?&?&Run)).
 apply bind_inv in Run as(u&last&Update&Ret);injection Ret as <- <-.
 unfold upd in Update;injection Update as _ <-.
 cbn[s_wrappers st_wrappers];apply nget_nset_same.
Qed.
Lemma wrapper_stored:forall fuel pv s id sf,
 Inv s->wrapper fuel pv s=Good(id,sf)->nget(st_wrappers sf)(gs_id pv)=Some id.
Proof.
 intros[|fuel]pv s id sf Inv Run;[discriminate|];cbn[wrapper]in Run;unfold wrapper_cached in Run.
 apply bind_inv in Run as(ss&sm&Get&Run);injection Get as <- <-.
 destruct(nget(st_wrappers s)(gs_id pv))as[old|]eqn:Cache.
 - injection Run as <- <-;exact Cache.
 - apply bind_inv in Run as(physical&sp&Lookup&Create).
   unfold sym in Lookup;destruct(nget(st_syms s)(gs_id pv))as[g|]eqn:Physical;[|discriminate].
   injection Lookup as <- <-.
   pose proof(iv_ids _ Inv _ _ Physical)as Id.
   rewrite <-Id;exact(wrapper_create_stored _ _ _ _ Create).
Qed.

Lemma wrapper_receipt:forall fuel pv s id sf,
 Inv s->InfoInv s->wrapper fuel pv s=Good(id,sf)->WrapperEntry sf(gs_id pv)id.
Proof.
 intros fuel pv s id sf Inv Cache Run.
 pose proof(proj2(okI_wrapper fuel pv)_ _ _ Inv Cache Run)as Final.
 apply(info_wrappers _ Final);eapply wrapper_stored;eassumption.
Qed.
