(* Physical resolution authenticates the functor arguments of link_body. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv ReifyData
 InfoInvariant InfoPreserve InfoInputs FwdInvariant FwdReadonly FwdPreserve FwdSynthInv
 LinkCacheInvariant LinkReadonly LinkConditional ProviderPhysical WrapperKeyReadonly.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go].
Local Lemma kwb_ety_ty : forall t,keeps st_fwd(ety_ty t).
Proof. intro t;unfold ety_ty;apply kw_ety_ty_go. Qed.
Local Lemma klb_ety_ty : forall t,keeps st_links(ety_ty t).
Proof. intro t;unfold ety_ty;apply kl_ety_ty_go. Qed.
#[local] Hint Resolve kwb_ety_ty klb_ety_ty:keepsdb.
Local Lemma kwb_key_theta : forall e k,keeps st_fwd(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
Local Lemma klb_key_theta : forall e k,keeps st_links(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
#[local] Hint Resolve kwb_key_theta klb_key_theta:keepsdb.
Local Lemma kwb_key_item : forall e k,keeps st_fwd(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
Local Lemma klb_key_item : forall e k,keeps st_links(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
#[local] Hint Resolve kwb_key_item klb_key_item:keepsdb.
Local Ltac binding_quiet := apply okLP_of_okL;apply okL_keeps;
 [apply okI_ni;[solve[okt]|solve[nit]|solve[keepst]]|
  apply okF_readonly;[solve[okt]|solve[keepst]]|solve[keepst]].
Local Ltac binding_tac Linked := repeat(fintros;first[
 solve[apply okLP_link_body;apply Linked]|
 lazymatch goal with
 | |-okLP _ (mbind _ _)=>apply okLP_bind;[apply link_physical_stable| |]
 | |-okLP _ (mmap _ _)=>apply okLP_mmap;[apply link_physical_stable|]
 | |-okLP _ (miter _ _)=>apply okLP_miter;[apply link_physical_stable|]
 | |-okLP _ (mfold _ _ _)=>apply okLP_mfold;[apply link_physical_stable|]
 | |-okLP _ (match ?x with _=>_ end)=>destruct x
 | |-okLP _ ?m=>first[solve[binding_quiet]|
  match m with context[match ?x with _=>_ end]=>destruct x end|
  let h:=head_of m in progress unfold h]
 end]).
Definition BindFunctorPreserves(fuel:nat)(c:cx)(e:env)(fg:gsym)(y:aexpr):Prop:=
 forall s result final,Inv s ->InfoInv s ->FwdInv s ->LinkInv s ->
 PhysicalProvider s fg ->bind_functor fuel c e fg y s=Good(result,final)->LinkInv final.
Theorem bind_functor_preserves_step : forall fuel,
 (forall fg bg,LinkBodyPreserves fuel fg bg)->
 forall c e fg y,BindFunctorPreserves(S fuel)c e fg y.
Proof.
 intros fuel Linked c e fg y s result final Valid Cached Forwarded Links Target Run.
 cbn[bind_functor]in Run.
 destruct(functor_parts fg)as[[fargs fres]fbody].
 destruct fbody;[provider_no_good Run|].
 destruct y;try(provider_no_good Run).
 match goal with a:sapp|-_=>destruct a end;try(provider_no_good Run).
 apply bind_inv in Run as[found[resolved[Resolve Run]]].
 destruct(resolve_sym_physical_provider _ _ _ _ _ Resolve Valid)as[State Physical].
 subst resolved;destruct found as[bg|];[|provider_no_good Run].
 assert(Provider:PhysicalProvider s bg)by(apply Physical;reflexivity).
 lazymatch type of Run with ?m s=Good(result,final)=>
 assert(Cert:okLP(LinkPhysical fg bg)m)by(binding_tac Linked);
 exact(proj2(proj2 Cert)s result final Valid Cached Forwarded Links(conj Target Provider)Run)
 end.
Qed.
