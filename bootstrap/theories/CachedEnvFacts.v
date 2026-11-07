(* Exact projection laws for the cached environments used by CachedClo. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss MossX SlotX CachedClo.
Import ListNotations.
Set Implicit Arguments.
Record cache_valid (d : cached_env) : Prop := CacheValid {
  cache_types_exact : cache_types d = SlotX.rdtypes (cache_raw d);
  cache_rows_exact : cache_rows d = SlotX.rtype_env (cache_raw d)
}.
Lemma rdtypes_app : forall a b,
  SlotX.rdtypes (a ++ b) = SlotX.rdtypes a ++ SlotX.rdtypes b.
Proof.
  induction a as [|[i en]a IH]; intros b; [reflexivity|].
  destruct i,en; cbn [app SlotX.rdtypes]; rewrite IH; reflexivity.
Qed.
Lemma rtype_env_app : forall a b,
  SlotX.rtype_env (a ++ b) = SlotX.rtype_env a ++ SlotX.rtype_env b.
Proof.
  induction a as [|[i en]a IH]; intros b; [reflexivity|].
  destruct i,en; cbn [app SlotX.rtype_env]; rewrite IH; reflexivity.
Qed.
Lemma rdtypes_rows : forall d,
  SlotX.rdtypes (SlotX.rtype_env d) = SlotX.rdtypes d.
Proof.
  induction d as [|[i en]d IH]; [reflexivity|].
  destruct i,en; cbn [SlotX.rtype_env SlotX.rdtypes]; rewrite IH; reflexivity.
Qed.
Lemma rtype_env_rows : forall d,
  SlotX.rtype_env (SlotX.rtype_env d) = SlotX.rtype_env d.
Proof.
  induction d as [|[i en]d IH]; [reflexivity|].
  destruct i,en; cbn [SlotX.rtype_env]; rewrite IH; reflexivity.
Qed.
Lemma cache_of_valid : forall d, cache_valid (cache_of d).
Proof. intro d; constructor; reflexivity. Qed.
Lemma cache_empty_valid : cache_valid cache_empty.
Proof. constructor; reflexivity. Qed.
Lemma cpush_raw : forall entry d,
  cache_raw (cpush entry d) = entry :: cache_raw d.
Proof. intros [i en]d; destruct i,en; reflexivity. Qed.
Lemma cpush_valid : forall entry d,
  cache_valid d -> cache_valid (cpush entry d).
Proof.
  intros [i en]d [Types Rows]; constructor;
    destruct i,en; cbn [cpush cache_types cache_rows cache_raw SlotX.rdtypes SlotX.rtype_env];
    rewrite ?Types, ?Rows; reflexivity.
Qed.
Lemma cappend_raw : forall a b,
  cache_raw (cappend a b) = cache_raw a ++ cache_raw b.
Proof. reflexivity. Qed.
Lemma cappend_valid : forall a b,
  cache_valid a -> cache_valid b -> cache_valid (cappend a b).
Proof.
  intros a b [Ta Ra][Tb Rb]; constructor; cbn [cappend cache_raw cache_types cache_rows].
  - rewrite rdtypes_app, Ta, Tb; reflexivity.
  - rewrite rtype_env_app, Ra, Rb; reflexivity.
Qed.
Lemma cache_ground_exact : forall d tau,
  cache_valid d -> CachedClo.rground d tau = SlotX.rground (cache_raw d) tau.
Proof. intros d tau [Types Rows]; unfold CachedClo.rground,SlotX.rground; rewrite Types; reflexivity. Qed.
Lemma cache_close_raw : forall d dc,
  cache_valid d ->
  cache_raw (CachedClo.rclose d dc) = SlotX.rclose (cache_raw d) (cache_raw dc).
Proof. intros d dc [Types Rows]; unfold CachedClo.rclose,SlotX.rclose; cbn; rewrite Rows; reflexivity. Qed.
Lemma cache_close_valid : forall d dc,
  cache_valid d -> cache_valid dc -> cache_valid (CachedClo.rclose d dc).
Proof.
  intros d dc [Td Rd][Tc Rc]; constructor; cbn [CachedClo.rclose cache_raw cache_types cache_rows].
  - rewrite rdtypes_app, Rd, rdtypes_rows, Td, Tc; reflexivity.
  - rewrite rtype_env_app, Rd, rtype_env_rows, Rc; reflexivity.
Qed.
Definition type_rows (types : subst) : renv :=
  map (fun p => (ITy (fst p), QType (snd p))) types.
Lemma rdtypes_type_rows : forall types, SlotX.rdtypes (type_rows types) = types.
Proof. unfold type_rows; induction types as [|[k tau]rest IH]; [reflexivity|]; cbn [map SlotX.rdtypes fst snd]; rewrite IH; reflexivity. Qed.
Lemma rtype_env_type_rows : forall types, SlotX.rtype_env (type_rows types) = type_rows types.
Proof. unfold type_rows; induction types as [|[k tau]rest IH]; [reflexivity|]; cbn [map SlotX.rtype_env fst snd]; rewrite IH; reflexivity. Qed.
Lemma cache_inst_valid : forall d th, cache_valid (CachedClo.rinst d th).
Proof.
  intros d th; unfold CachedClo.rinst.
  set (types := map (fun p => (fst p,CachedClo.rground d (snd p))) th).
  change (cache_valid (CachedEnv (type_rows types) types (type_rows types))).
  constructor; cbn [cache_types cache_rows cache_raw].
  - rewrite rdtypes_type_rows; reflexivity.
  - rewrite rtype_env_type_rows; reflexivity.
Qed.
Lemma cache_inst_raw : forall d th,
  cache_valid d -> cache_raw (CachedClo.rinst d th) = SlotX.rinst (cache_raw d) th.
Proof.
  intros d th [Types Rows]; rewrite SlotX.rinst_map.
  unfold CachedClo.rinst; cbn [cache_raw]; rewrite map_map.
  apply map_ext; intros [k tau]; cbn [fst snd].
  unfold CachedClo.rground,SlotX.rground; rewrite Types; reflexivity.
Qed.
Lemma cache_call_raw : forall P d th dc,
  cache_valid d ->
  cache_raw (CachedClo.rcall_env P d th dc) =
  SlotX.rcall_env P (cache_raw d) th (cache_raw dc).
Proof.
  intros P d th dc Valid; unfold CachedClo.rcall_env,SlotX.rcall_env.
  destruct (xreify P); [rewrite cappend_raw,cache_inst_raw,cache_close_raw by exact Valid|]; reflexivity.
Qed.
Lemma cache_call_valid : forall P d th dc,
  cache_valid d -> cache_valid dc -> cache_valid (CachedClo.rcall_env P d th dc).
Proof.
  intros P d th dc Vd Vc; unfold CachedClo.rcall_env; destruct (xreify P); [|exact Vc].
  apply cappend_valid; [apply cache_inst_valid|apply cache_close_valid; assumption].
Qed.
Lemma cache_apply_raw : forall P d h th dc,
  cache_valid d ->
  cache_raw (CachedClo.rapply_env P d h th dc) =
  SlotX.rapply_env P (cache_raw d) h th (cache_raw dc).
Proof. intros; unfold CachedClo.rapply_env,SlotX.rapply_env; apply cache_call_raw; assumption. Qed.
Lemma cache_apply_valid : forall P d h th dc,
  cache_valid d -> cache_valid dc -> cache_valid (CachedClo.rapply_env P d h th dc).
Proof. intros; unfold CachedClo.rapply_env; apply cache_call_valid; assumption. Qed.

(* Renaming item keys cannot alter either type projection, even for
   malformed entries or arbitrary (non-scalar) environments. *)
Lemma rdtypes_inst_keys : forall th d,
  SlotX.rdtypes (CloScalar.instantiate_scalar th d) = SlotX.rdtypes d.
Proof.
  intros th d; unfold CloScalar.instantiate_scalar; induction d as [|[i en]d IH]; [reflexivity|].
  destruct i,en; cbn [map fst snd app_subst_item SlotX.rdtypes]; rewrite IH; reflexivity.
Qed.
Lemma rtype_env_inst_keys : forall th d,
  SlotX.rtype_env (CloScalar.instantiate_scalar th d) = SlotX.rtype_env d.
Proof.
  intros th d; unfold CloScalar.instantiate_scalar; induction d as [|[i en]d IH]; [reflexivity|].
  destruct i,en; cbn [map fst snd app_subst_item SlotX.rtype_env]; rewrite IH; reflexivity.
Qed.
Lemma cache_inst_keys_raw : forall th d,
  cache_raw (CachedClo.instantiate_scalar_fast th d) =
  CloScalar.instantiate_scalar_fast th (cache_raw d).
Proof. intros [|p th]d; reflexivity. Qed.
Lemma cache_inst_keys_valid : forall th d,
  cache_valid d -> cache_valid (CachedClo.instantiate_scalar_fast th d).
Proof.
  intros [|p th]d Valid; [exact Valid|].
  constructor; cbn [CachedClo.instantiate_scalar_fast cache_types cache_rows cache_raw];
    rewrite CloScalar.instantiate_scalar_fast_exact.
  - rewrite rdtypes_inst_keys; exact (cache_types_exact Valid).
  - rewrite rtype_env_inst_keys; exact (cache_rows_exact Valid).
Qed.

(* State the simulation on canonical caches. A valid cache is completely
   determined by its raw environment, including the order of repeated keys. *)
Lemma cache_eq_of_raw : forall d raw,
  cache_valid d -> cache_raw d = raw -> d = cache_of raw.
Proof.
  intros [d types rows] raw [Types Rows] Raw.
  cbn in Types, Rows, Raw. subst; reflexivity.
Qed.

Lemma cache_of_rlook : forall l d,
  CachedClo.rlook l (cache_of d) = SlotX.rlook l d.
Proof. reflexivity. Qed.

Lemma cache_of_cast : forall P d n th w,
  CachedClo.rtyped_cast P (cache_of d) n th w = SlotX.rtyped_cast P d n th w.
Proof. reflexivity. Qed.

Lemma cache_of_flag : forall d b,
  CachedClo.rflag (cache_of d) b = SlotX.rflag d b.
Proof. reflexivity. Qed.

Lemma cache_of_prim : forall d i p,
  CachedClo.rprim_entry (cache_of d) i p = SlotX.rprim_entry d i p.
Proof. intros d i p; destruct p, i; reflexivity. Qed.

Lemma cache_of_push : forall entry d,
  cpush entry (cache_of d) = cache_of (entry :: d).
Proof.
  intros. apply cache_eq_of_raw; [apply cpush_valid, cache_of_valid|apply cpush_raw].
Qed.

Lemma cache_of_append : forall a b,
  cappend (cache_of a) (cache_of b) = cache_of (a ++ b).
Proof.
  intros. apply cache_eq_of_raw; [apply cappend_valid; apply cache_of_valid|reflexivity].
Qed.

Lemma cache_of_compose : forall d pl,
  CachedClo.rcompose (cache_of d) pl = option_map cache_of (SlotX.rcompose d pl).
Proof. reflexivity. Qed.

Lemma cache_of_compose_dyn : forall d sm,
  CachedClo.rcompose_dyn (cache_of d) sm =
    option_map cache_of (SlotX.rcompose_dyn d sm).
Proof. reflexivity. Qed.

Lemma cache_of_chain : forall d sms,
  CachedClo.rchain (cache_of d) sms = option_map cache_of (SlotX.rchain d sms).
Proof.
  intros d sms. induction sms as [|sm rest IH]; [reflexivity|].
  cbn [CachedClo.rchain SlotX.rchain].
  rewrite IH. destruct (SlotX.rchain d rest); reflexivity.
Qed.

Lemma cache_of_close : forall d dc,
  CachedClo.rclose (cache_of d) (cache_of dc) = cache_of (SlotX.rclose d dc).
Proof.
  intros. apply cache_eq_of_raw.
  - apply cache_close_valid; apply cache_of_valid.
  - exact (@cache_close_raw (cache_of d) (cache_of dc) (cache_of_valid d)).
Qed.

Lemma cache_of_call : forall P d th dc,
  CachedClo.rcall_env P (cache_of d) th (cache_of dc) =
    cache_of (SlotX.rcall_env P d th dc).
Proof.
  intros. apply cache_eq_of_raw.
  - apply cache_call_valid; apply cache_of_valid.
  - apply cache_call_raw, cache_of_valid.
Qed.

Lemma cache_of_apply : forall P d h th dc,
  CachedClo.rapply_env P (cache_of d) h th (cache_of dc) =
    cache_of (SlotX.rapply_env P d h th dc).
Proof. intros; apply cache_of_call. Qed.

Lemma cache_of_bind_ty : forall P d t tau,
  CachedClo.rbind_ty P (cache_of d) t tau = cache_of (SlotX.rbind_ty P d t tau).
Proof.
  intros. unfold CachedClo.rbind_ty, SlotX.rbind_ty.
  destruct (xreify P); [apply cache_of_push|reflexivity].
Qed.

Lemma cache_of_result_env : forall P d Dr dr,
  CachedClo.rresult_env P (cache_of d) Dr (cache_of dr) =
    cache_of (SlotX.rresult_env P d Dr dr).
Proof.
  intros. unfold CachedClo.rresult_env, SlotX.rresult_env, SlotX.rresult_types.
  destruct (xreify P); [apply cache_of_append|reflexivity].
Qed.

Lemma cache_of_inst_keys : forall th d,
  CachedClo.instantiate_scalar_fast th (cache_of d) =
    cache_of (CloScalar.instantiate_scalar_fast th d).
Proof.
  intros. apply cache_eq_of_raw.
  - apply cache_inst_keys_valid, cache_of_valid.
  - apply cache_inst_keys_raw.
Qed.
