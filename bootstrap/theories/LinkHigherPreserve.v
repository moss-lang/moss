(* Higher-order emitters preserve link history across their recursive bodies. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv ReifyData
 InfoInvariant InfoPreserve InfoInputs FwdInvariant FwdReadonly FwdPreserve FwdSynthInv
 LinkCacheInvariant LinkReadonly LinkPreserve LinkConditional ProviderPhysical WrapperKeyReadonly
 LambdaInputTrace LambdaTrace ForwarderTrace.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go].
Local Lemma kw_link_ety_ty : forall t,keeps st_fwd(ety_ty t).
Proof. intro t;unfold ety_ty;apply kw_ety_ty_go. Qed.
Local Lemma kl_link_ety_ty : forall t,keeps st_links(ety_ty t).
Proof. intro t;unfold ety_ty;apply kl_ety_ty_go. Qed.
#[local] Hint Resolve kw_link_ety_ty kl_link_ety_ty : keepsdb.
Local Lemma kw_link_key_theta : forall e k,keeps st_fwd(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
Local Lemma kl_link_key_theta : forall e k,keeps st_links(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
#[local] Hint Resolve kw_link_key_theta kl_link_key_theta : keepsdb.
Local Lemma kw_link_key_item : forall e k,keeps st_fwd(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
Local Lemma kl_link_key_item : forall e k,keeps st_links(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
#[local] Hint Resolve kw_link_key_item kl_link_key_item : keepsdb.

Local Ltac link_quiet := apply okL_keeps;
 [apply okI_ni;[solve[okt]|solve[nit]|solve[keepst]]|
  apply okF_readonly;[solve[okt]|solve[keepst]]|solve[keepst]].
Local Ltac advance Run Certificate :=
 lazymatch type of Run with ?m ?s=Good(?a,?final) =>
 let Valid:=fresh "Valid" in let Cached:=fresh "Cached" in
 let Forwarded:=fresh "Forwarded" in let Links:=fresh "Links" in let Frame:=fresh "Frame" in
 destruct(@okL_run _ m s a final Certificate ltac:(assumption) ltac:(assumption)
 ltac:(assumption) ltac:(assumption) Run)as[Valid[Cached[Forwarded[Links Frame]]]] end.
Local Ltac advance_quiet Run :=
 lazymatch type of Run with ?m ?s=Good(?a,?final) =>
 let Cert:=fresh "Quiet" in assert(Cert:okL m)by link_quiet;advance Run Cert end.
Lemma okL_wrapper : forall fuel pv,okL(wrapper fuel pv).
Proof.
 intros fuel pv;apply okL_keeps;[apply okI_wrapper|apply okF_wrapper|].
 destruct fuel;cbn[wrapper];keepst.
Qed.
Lemma okL_lambda_provider_step : forall fuel,
 (forall c e b exp,okL(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,okL(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof.
 intros fuel Blocks c e ps rt body bound sg this what.
 split;[apply okI_lambda_provider|];split;[apply okF_lambda_provider|].
 intros s result final Valid Cached Forwarded Links Run.
 destruct result as[[pid pre]sm].
 destruct(lambda_provider_stages_inputs _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Run Valid)as
 [f[le[lets[captureStart[beforePid[afterPid[registered[compiled[bodyFinal[emitted[cached
 [Fuel[Capture[[sp[Params Return]][IS[FS[Symbols[Functions[New[Register[Body[Emit[Cache[Transfer[Names[IB IR]]]]]]]]]]]]]]]]]]]]]]]]]].
 injection Fuel as <-.
 advance_quiet Params.
 advance_quiet Return.
 advance_quiet Capture.
 advance_quiet New.
 advance_quiet Register.
 assert(Generated:okL(lambda_generated_body fuel c(lambda_own_env ps sg this le lets)ps body sg this pid lets)).
 {unfold lambda_generated_body;apply okL_in_reify_code;apply okL_with_vars.
  apply okL_bind;[link_quiet|intro locals;apply okL_bind;[apply Blocks|intros[[got ty]out];apply okL_ret]]. }
 advance Body Generated.
 assert(Suffix:(emit_fn pid compiled;;upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)));;
  smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le)))bodyFinal=Good(sm,final)).
 {unfold mbind;rewrite Emit,Cache;exact Transfer. }
 assert(SuffixOK:ok(emit_fn pid compiled;;upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)));;
  smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))))by okt.
 assert(SuffixKeeps:keeps st_links(emit_fn pid compiled;;upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)));;
  smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))))by keepst.
 eapply link_inv_keep;[eassumption|exact(proj2(SuffixOK bodyFinal sm final ltac:(assumption)Suffix))|exact(SuffixKeeps _ _ _ Suffix)].
Qed.
Lemma okL_forwarder_step : forall fuel,
 (forall g,okL(lower_body fuel g))->forall tg,okL(forwarder(S fuel)tg).
Proof.
 intros fuel Lower tg;split;[apply okI_forwarder|];split;[apply okF_forwarder|].
 intros s got final Valid Cached Forwarded Links Run.
 destruct(nget(st_fwd s)(gs_id tg))as[id|]eqn:Hit.
 {pose proof(forwarder_hit_trace _ _ _ _ _ _ Hit Run)as Sym.
  destruct(sym_physical_provider _ _ _ _ Sym Valid)as[-> _];exact Links. }
 destruct(forwarder_miss_trace _ _ _ _ _ Hit Run)as
 [f[id[allocated[linked[ready[Fuel[Id[New[Install[Sym Body]]]]]]]]]].
 injection Fuel as <-.
 advance_quiet New.
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Fresh[Next Lookup]].
 assert(InstallOK:ok(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by okt.
 destruct(InstallOK allocated tt linked ltac:(assumption)Install)as[VL FL].
 assert(CL:InfoInv linked).
 {eapply info_step;[eassumption|apply frl_of_fr;exact FL| |];unfold upd in Install;injection Install as <-;reflexivity. }
 assert(LL:LinkInv linked).
 {eapply link_inv_keep;[eassumption|exact FL|];unfold upd in Install;injection Install as <-;reflexivity. }
 assert(FWL:FwdInv linked).
 {unfold upd in Install;injection Install as <-.
  eapply fwd_inv_install;[eassumption|exact Lookup|reflexivity]. }
 advance_quiet Sym.
 exact(proj2(proj2(Lower got))ready tt final ltac:(assumption)ltac:(assumption)ltac:(assumption)ltac:(assumption)Body).
Qed.
