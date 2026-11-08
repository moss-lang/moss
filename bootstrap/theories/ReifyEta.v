(* A cached source eta wrapper can serve distinct receiver types. Immutable
   lowering clones its code at each provision and gives receiver type inputs
   fresh roles, separate from the plain provider's own formal type roles. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Elab NMap ReifyData Reify ReifySource
  ReifyRoles ReifyPlan ReifyMaps.
Import ListNotations.

Fixpoint eta_arguments_from (next : var) (arguments : list xexpr) : bool :=
  match arguments with
  | [] => true
  | XVar v :: rest => Nat.eqb v next && eta_arguments_from (S next) rest
  | _ :: _ => false end.
Definition source_eta_callee (source : st) (wrapper : fsym) : option (fsym * list xexpr) :=
  match assoc reify_owner_eqb (ROFunction wrapper) (rm_codes (st_reify source)),
        NMap.nget (st_syms source) wrapper, assoc Nat.eqb wrapper (st_fns source) with
  | Some code,Some symbol,Some (XCallD callee _ _ arguments) =>
      match rc_signature ety key code,gs_origin symbol with
      | None,OLambda => Some (callee,arguments)
      | _,_ => None end
  | _,_,_ => None end.

Record eta_wrapper_plan : Type := EtaWrapperPlan {
  ewp_code : fsym;
  ewp_inputs : tele;
  ewp_signature : fnsig;
  ewp_body : xexpr;
  ewp_receiver_map : smap;
  ewp_after : reify_state
}.
Definition reserve_eta_wrapper (receiver : ty) (provider : reify_function_frame)
    (before : reify_state) : eta_wrapper_plan :=
  let actual_receiver := ExecutionModel.app_subst (rf_generations before) receiver in
  let keys := nodup Nat.eq_dec (ty_frees actual_receiver) in
  let '(roles,receiver_map,allocated) := allocate_type_roles keys before in
  let code := rf_next allocated in
  let inputs := type_role_telescope roles ++ rff_inputs provider in
  let signature := mkSig
      (ExecutionModel.app_subst roles actual_receiver :: fs_params (rff_signature provider))
      (fs_ret (rff_signature provider)) in
  let body := XCallD (rff_code provider) [] (identity_smap (rff_inputs provider))
      (map XVar (seq 1 (length (fs_params (rff_signature provider))))) in
  EtaWrapperPlan code inputs signature body
    (map (fun p => (snd p,fst p)) receiver_map)
    (ReifyState (S code) (rf_names allocated) (rf_generations allocated) (rf_items allocated)
      ((IFn code,RDFunction inputs signature)::rf_declarations allocated)).

Lemma eta_argument_list_ignores_receiver : forall n,
    eta_arguments_from 1 (map XVar (seq 1 n)) = true.
Proof.
  assert (H : forall n k, eta_arguments_from k (map XVar (seq k n)) = true).
  { induction n; intros k; cbn; [reflexivity|]. rewrite Nat.eqb_refl. apply IHn. }
  intro n. apply H.
Qed.

Example eta_receiver_role_is_separate_from_provider_input :
  let scope := ReifyScope ety key [] [] [] [] [] [] [] in
  let provider := ReifyFunctionFrame 4 30 scope [(1,TAbs 40)] [] [ITy 40]
    (mkSig [TAbs 40] TUnit) [] (ReifyState 50 [] [] [] []) in
  let before := ReifyState 100 [] [(1,TAbs 40)] [] [] in
  let wrapper := reserve_eta_wrapper (TAbs 1) provider before in
  ewp_code wrapper = 101 /\
  ewp_inputs wrapper = [ITy 100;ITy 40] /\
  ewp_signature wrapper = mkSig [TAbs 100;TAbs 40] TUnit /\
  ewp_receiver_map wrapper = [(ITy 100,ITy 40)] /\
  ewp_body wrapper = XCallD 30 [] [(ITy 40,ITy 40)] [XVar 1].
Proof. repeat split; reflexivity. Qed.
