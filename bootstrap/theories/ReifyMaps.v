(* Full immutable maps retain type entries and allow provision slot renaming.
   Higher-order functor compatibility is supplied by the schema relation;
   it is not replaced by source symbol identity. *)
From Stdlib Require Import List.
From NM Require Import CoreMoss.
Import ListNotations.

Definition grounded_call_signature (g : subst) (S : fnsig) : list ty * ty :=
  (map (app_subst g) (fs_params S), app_subst g (fs_ret S)).

Definition method_call_signature (g : subst) (rt : ty) (th : subst)
    (S : fnsig) : list ty * ty :=
  (app_subst g rt :: map (fun t => app_subst g (app_subst (this_subst rt) (app_subst th t)))
       (fs_params S),
   app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret S)))).

Inductive full_item_map (Sg : gsig)
    (functor_schema : subst -> usym -> subst -> usym -> Prop)
    (gt gs : subst) : item -> item -> Prop :=
| FimType : forall t s,
    app_subst gt (TAbs t) = app_subst gs (TAbs s) ->
    full_item_map Sg functor_schema gt gs (ITy t) (ITy s)
| FimValue : forall v w D E t s,
    g_val Sg v = Some (D,t) -> g_val Sg w = Some (E,s) ->
    app_subst gt t = app_subst gs s ->
    full_item_map Sg functor_schema gt gs (IVal v) (IVal w)
| FimFunction : forall f h D E S T,
    g_fn Sg f = Some (D,S,None) -> g_fn Sg h = Some (E,T,None) ->
    grounded_call_signature gt S = grounded_call_signature gs T ->
    full_item_map Sg functor_schema gt gs (IFn f) (IFn h)
| FimMethod : forall rt m th ru n ph D E S T,
    g_mth Sg m = Some (D,S) -> g_mth Sg n = Some (E,T) ->
    method_call_signature gt rt th S = method_call_signature gs ru ph T ->
    full_item_map Sg functor_schema gt gs (IMth rt m th) (IMth ru n ph)
| FimFunctor : forall u v,
    functor_schema gt u gs v ->
    full_item_map Sg functor_schema gt gs (IFun u) (IFun v)
| FimOpaqueFunctor : forall u v,
    g_opaque_fun Sg u = true -> g_fun Sg u = None ->
    g_opaque_fun Sg v = true -> g_fun Sg v = None ->
    full_item_map Sg functor_schema gt gs (IFun u) (IFun v).

Definition full_map_ok (Sg : gsig)
    (functor_schema : subst -> usym -> subst -> usym -> Prop)
    (gt gs : subst) (target source : list item) (sm : smap) : Prop :=
  map fst sm = target /\
  Forall (fun p => In (snd p) source /\
    full_item_map Sg functor_schema gt gs (fst p) (snd p)) sm.

Definition map_type_agreement (gt gs : subst) (sm : smap) : Prop :=
  forall t s, In (ITy t, ITy s) sm ->
    app_subst gt (TAbs t) = app_subst gs (TAbs s).

Definition fixed_interpretation (Finst : usym -> subst) (u : usym)
    (g : subst) (t : tsym) : ty :=
  app_subst g (app_subst (Finst u) (TAbs t)).

(* The same finite interpretation table also carries checked nonfixed
   input equations.  Their RHSs are formal input types, whereas fixed RHSs
   are read in the captured outer grounding. *)
Definition formal_input_equations (Sg : gsig) (Finst : usym -> subst)
    (u : usym) (D : tele) (gin : subst) : Prop :=
  forall t tau, In (t,tau) (Finst u) ->
    In t (tyreqs D) /\
    (~ In t (g_fixed Sg u) ->
      incl (ty_frees tau) (tyreqs D) /\
      app_subst gin (TAbs t) = app_subst gin tau).

Definition schema_input_ground (Sg : gsig) (Finst : usym -> subst) (u : usym) (base : subst)
    (D : tele) (g : subst) : Prop :=
  exists Phi, tele_ctx D = Some Phi /\ grounding g /\
    covers g (tyreqs (cA Phi)) /\ absorbs g (cS Phi) /\
    (forall t, In t (g_fixed Sg u) ->
      app_subst g (TAbs t) = fixed_interpretation Finst u base t) /\
    formal_input_equations Sg Finst u (cA Phi) g.

(* Result binders shadow input names.  Preserve only inputs which are not
   existential outputs; requiring preservation of both would reject T->T. *)
Definition schema_result_ground (Din Dout : tele) (gin gout : subst) : Prop :=
  exists Phi, tele_ctx (Dout ++ Din) = Some Phi /\ grounding gout /\
    covers gout (tyreqs (cA Phi)) /\ absorbs gout (cS Phi) /\
    (forall t, In t (tyreqs Din) -> ~ In t (tyreqs Dout) ->
      app_subst gout (TAbs t) = app_subst gin (TAbs t)).

Definition identity_smap (D : tele) : smap := map (fun i => (i,i)) D.

(* Plain composition copies functor entries without adapting their chains.
   Therefore copied functors must retain their entire runtime key interface. *)
CoInductive identity_functor_schema (Sg : gsig) (Finst : usym -> subst)
    : subst -> usym -> subst -> usym -> Prop :=
| IfsRelated : forall gt u gs v D Dr,
    g_fun Sg u = Some (D,Dr,None) ->
    g_fun Sg v = Some (D,Dr,None) -> NoDup D -> NoDup Dr ->
    (forall s, In s (g_fixed Sg v) -> exists t,
      In t (g_fixed Sg u) /\
      fixed_interpretation Finst u gt t = fixed_interpretation Finst v gs s) ->
    (forall gt', schema_input_ground Sg Finst u gt D gt' -> exists gs',
      schema_input_ground Sg Finst v gs D gs' /\
      map_type_agreement gs' gt' (identity_smap D)) ->
    (forall gt' gs',
      schema_input_ground Sg Finst u gt D gt' ->
      schema_input_ground Sg Finst v gs D gs' ->
      map_type_agreement gs' gt' (identity_smap D) ->
      full_map_ok Sg (identity_functor_schema Sg Finst) gs' gt' D D (identity_smap D) /\
      (forall gsout, schema_result_ground D Dr gs' gsout -> exists gtout,
        schema_result_ground D Dr gt' gtout /\
        map_type_agreement gtout gsout (identity_smap Dr) /\
        full_map_ok Sg (identity_functor_schema Sg Finst) gtout gsout Dr Dr (identity_smap Dr))) ->
    identity_functor_schema Sg Finst gt u gs v.

Inductive functor_link (Sg : gsig) (Finst : usym -> subst)
    : subst -> usym -> subst -> usym -> smap -> smap -> Prop :=
| FsRelated : forall gt u gs v Du Dru Dv Drv inputs outputs,
    g_fun Sg u = Some (Du,Dru,None) ->
    g_fun Sg v = Some (Dv,Drv,None) ->
    map fst inputs = Dv -> map fst outputs = Dru ->
    NoDup (map fst inputs) -> NoDup (map fst outputs) ->
    (* Every captured source-fixed meaning is supplied by a target-fixed
       meaning.  There is no unconstrained, hidden fixed type. *)
    (forall s, In s (g_fixed Sg v) -> exists t,
      In t (g_fixed Sg u) /\
      fixed_interpretation Finst u gt t = fixed_interpretation Finst v gs s) ->
    (* Constructive input totality rules out vacuous compatibility guards. *)
    (forall gt', schema_input_ground Sg Finst u gt Du gt' -> exists gs',
      schema_input_ground Sg Finst v gs Dv gs' /\
      map_type_agreement gs' gt' inputs) ->
    (forall gt' gs',
      schema_input_ground Sg Finst u gt Du gt' ->
      schema_input_ground Sg Finst v gs Dv gs' ->
      map_type_agreement gs' gt' inputs ->
      full_map_ok Sg (identity_functor_schema Sg Finst) gs' gt' Dv Du inputs /\
      (* Covariant generative results must also be constructively total. *)
      (forall gsout, schema_result_ground Dv Drv gs' gsout -> exists gtout,
        schema_result_ground Du Dru gt' gtout /\
        map_type_agreement gtout gsout outputs /\
        full_map_ok Sg (identity_functor_schema Sg Finst) gtout gsout Dru Drv outputs)) ->
    functor_link Sg Finst gt u gs v inputs outputs.

Definition functor_schema (Sg : gsig) (Finst : usym -> subst) (gt : subst) (u : usym)
    (gs : subst) (v : usym) : Prop :=
  exists inputs outputs, functor_link Sg Finst gt u gs v inputs outputs.

Lemma identity_smap_domain : forall D, map fst (identity_smap D) = D.
Proof. intro D. unfold identity_smap. rewrite map_map. apply map_id. Qed.

Lemma identity_type_agreement : forall gt gs D,
    map_type_agreement gt gs (identity_smap D) <->
    (forall t, In (ITy t) D ->
      app_subst gt (TAbs t) = app_subst gs (TAbs t)).
Proof.
  intros gt gs D. split.
  - intros H t Hin. apply (H t t). unfold identity_smap.
    apply in_map_iff. exists (ITy t). split; [reflexivity|exact Hin].
  - intros H t s Hin. unfold identity_smap in Hin.
    apply in_map_iff in Hin as (i & E & Hin).
    inversion E; subst. assert (t = s) by congruence. subst s. apply H. exact Hin.
Qed.

(* Changing only the interpretation of functor fixed keys does not change
   the first-order fields checked by a complete map. Higher-order meaning
   is supplied explicitly as the same relation on both sides. *)
Lemma full_item_map_runtime_signature : forall Sg P relation gt gs i j,
    full_item_map (runtime_signature Sg P) relation gt gs i j <->
    full_item_map Sg relation gt gs i j.
Proof.
  intros Sg P relation gt gs i j. split; intro H; inversion H; subst;
    try solve [econstructor; eauto]; eapply FimOpaqueFunctor; eauto.
Qed.
Lemma full_map_ok_runtime_signature : forall Sg P relation gt gs target source sm,
    full_map_ok (runtime_signature Sg P) relation gt gs target source sm <->
    full_map_ok Sg relation gt gs target source sm.
Proof.
  intros Sg P relation gt gs target source sm. unfold full_map_ok. split.
  - intros [Dom Rows]. split; [exact Dom|]. apply Forall_forall. intros [i j] Hin.
    destruct (proj1 (Forall_forall _ _) Rows (i,j) Hin) as [InJ Match].
    split; [exact InJ|]. apply (proj1 (full_item_map_runtime_signature Sg P relation gt gs i j)). exact Match.
  - intros [Dom Rows]. split; [exact Dom|]. apply Forall_forall. intros [i j] Hin.
    destruct (proj1 (Forall_forall _ _) Rows (i,j) Hin) as [InJ Match].
    split; [exact InJ|]. apply (proj2 (full_item_map_runtime_signature Sg P relation gt gs i j)). exact Match.
Qed.

Lemma runtime_fixed_immutable : forall Sg P u,
    xp_immutable P = true -> runtime_fixed Sg P u = xp_fixed_keys P u.
Proof. intros. unfold runtime_fixed. rewrite H. reflexivity. Qed.
Lemma runtime_fixed_legacy : forall Sg P u,
    xp_immutable P = false -> runtime_fixed Sg P u = g_fixed Sg u.
Proof. intros. unfold runtime_fixed. rewrite H. reflexivity. Qed.
Lemma runtime_signature_legacy : forall Sg P,
    xp_immutable P = false -> runtime_signature Sg P = Sg.
Proof.
  intros Sg P H. destruct Sg. unfold runtime_signature,runtime_fixed. cbn.
  rewrite H. reflexivity.
Qed.
