(* Actual complete source builds start and finish with authenticated link history. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv ReifyData
 InfoInvariant InfoPreserve InfoInputs FwdInvariant FwdReadonly FwdPreserve FwdSynthInv FwdProgramInv
 LinkCacheInvariant LinkReadonly LinkPreserve LinkConditional LinkBindPreserve LinkSynthPreserve
 ProviderPhysical WrapperKeyReadonly ProgramTail ProgramInfo ReadonlyHelpers.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go].
Local Lemma kwp_ety_ty : forall t,keeps st_fwd(ety_ty t).
Proof. intro t;unfold ety_ty;apply kw_ety_ty_go. Qed.
Local Lemma klp_ety_ty : forall t,keeps st_links(ety_ty t).
Proof. intro t;unfold ety_ty;apply kl_ety_ty_go. Qed.
#[local] Hint Resolve kwp_ety_ty klp_ety_ty:keepsdb.
Local Lemma kwp_key_theta : forall e k,keeps st_fwd(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
Local Lemma klp_key_theta : forall e k,keeps st_links(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
#[local] Hint Resolve kwp_key_theta klp_key_theta:keepsdb.
Local Lemma kwp_key_item : forall e k,keeps st_fwd(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
Local Lemma klp_key_item : forall e k,keeps st_links(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
#[local] Hint Resolve kwp_key_item klp_key_item:keepsdb.
Local Ltac program_link_quiet := apply okL_keeps;
 [first[solve[auto with okIdb]|apply okI_ni;[solve[okt]|solve[nit]|solve[keepst]]]|
  first[solve[auto with fwdb]|apply okF_readonly;[solve[okt]|solve[keepst]]]|solve[keepst]].
Lemma okLP_platform_fun : forall g,okLP(fun s=>PhysicalProvider s g)(platform_fun g).
Proof.
 intro g;split;[apply okI_ni;[unfold platform_fun;okt|unfold platform_fun;nit|unfold platform_fun;keepst]|].
 split;[apply okF_readonly;[unfold platform_fun;okt|unfold platform_fun;keepst]|].
 intros s h final Valid Info Forward Links Physical Run.
 exact(platform_fun_preserves g s h final Valid Info Forward Links Physical Run).
Qed.
Local Ltac program_link_tac := repeat(fintros;first[
 solve[auto with linkdb]|
 lazymatch goal with
 | |-okL(mbind(sym _)_)=>apply okL_sym_bind
 | |-okL(mbind _ _)=>apply okL_bind
 | |-okL(mmap _ _)=>apply okL_mmap
 | |-okL(miter _ _)=>apply okL_miter
 | |-okL(mfold _ _ _)=>apply okL_mfold
 | |-okLP _ (platform_fun _)=>apply okLP_platform_fun
 | |-okLP _ (mbind _ _)=>apply okLP_bind;[apply link_single_physical_stable| |]
 | |-okLP _ (mmap _ _)=>apply okLP_mmap;[apply link_single_physical_stable|]
 | |-okLP _ (miter _ _)=>apply okLP_miter;[apply link_single_physical_stable|]
 | |-okLP _ (mfold _ _ _)=>apply okLP_mfold;[apply link_single_physical_stable|]
 | |-okLP _ (match ?x with _=>_ end)=>destruct x
 | |-okLP _ ?m=>apply okLP_of_okL
 | |-okL(match ?x with _=>_ end)=>destruct x
 | |-okL ?m=>first[solve[program_link_quiet]|
  match m with context[match ?x with _=>_ end]=>destruct x end|
  let h:=head_of m in progress unfold h]
 end]).
Lemma okL_lower_all : okL lower_all.
Proof. unfold lower_all;program_link_tac. Qed.
Lemma okL_platform_entry : forall k prims,okL(platform_entry k prims).
Proof. intros k prims;unfold platform_entry;program_link_tac. Qed.
#[local] Hint Resolve okL_lower_all okL_platform_entry : linkdb.
Lemma kl_entry_zero_width : forall fuel e t,keeps st_links(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local] Hint Resolve kl_entry_zero_width:keepsdb.
#[local] Hint Resolve kw_entry_zero_width_present kw_check_main_signature_present:keepsdb.
Lemma kl_check_main_signature : forall g info,keeps st_links(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local] Hint Resolve kl_check_main_signature:keepsdb.
Lemma okL_check_assumes : okL check_assumes.
Proof.
 apply okL_keeps;[exact okI_check_assumes| |apply kl_check_assumes].
 apply okF_readonly;[exact(proj1 okI_check_assumes)|].
 eapply keeps_projection with(p:=readonly_state)(q:=fun x=>snd(fst(fst(fst x)))).
 exact kr_check_assumes.
Qed.
#[local] Hint Resolve okL_check_assumes:linkdb.
Lemma okL_elab_program : okL elab_program.
Proof.
 rewrite program_split.
 apply okL_bind;[program_link_quiet|intros[]].
 apply okL_bind;[program_link_quiet|intros[]].
 apply okL_bind;[program_link_quiet|intros[]].
 apply okL_bind;[exact okL_lower_all|intros[]].
 unfold program_tail;program_link_tac.
Qed.
Theorem elab_init_link_inv : forall w P final,
 Inv(s_init w)->elab_program(s_init w)=Good(P,final)->LinkInv final.
Proof.
 intros w P final Valid Run.
 exact(proj2(proj2 okL_elab_program)(s_init w)P final Valid
  (@info_init w)(@fwd_inv_init w)(@link_inv_init w)Run).
Qed.
