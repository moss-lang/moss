(* Canonical provider lookup is observationally inert on every actual eta route. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ProviderPhysical.
Import ListNotations.
Definition legacy_wrapper(fuel:nat)(pv:gsym):M nat:=
 match fuel with 0=>deep|S _=>
 let! s:=get in match nget(st_wrappers s)(gs_id pv)with
 |Some w=>ret w|None=>wrapper_create pv end end.
Theorem wrapper_physical_unchanged:forall fuel pv s,
 PhysicalProvider s pv->wrapper fuel pv s=legacy_wrapper fuel pv s.
Proof.
 intros[|fuel]pv s Physical;[reflexivity|].
 cbn[wrapper wrapper_cached legacy_wrapper mbind get];destruct(nget(st_wrappers s)(gs_id pv));[reflexivity|].
 unfold PhysicalProvider in Physical;unfold mbind,sym;rewrite Physical;reflexivity.
Qed.
Theorem actual_eta_wrapper_unchanged:forall fuel callfuel c e y this wanted why s pv sp sm sw,
 expect_provider c e y this s=Good(pv,sp)->
 match_fn_sig callfuel c e pv wanted why sp=Good(sm,sw)->Inv s->
 wrapper fuel pv sw=legacy_wrapper fuel pv sw.
Proof.
 intros fuel callfuel c e y this wanted why s pv sp sm sw Provider Match Hi.
 destruct(expect_provider_physical _ _ _ _ _ _ _ Provider Hi)as[-> Physical].
 apply wrapper_physical_unchanged;eapply match_fn_sig_keeps_physical;eassumption.
Qed.
