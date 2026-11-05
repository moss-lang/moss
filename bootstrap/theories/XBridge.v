(* ------------------------------------------------------------------------ *)
(* XBridge.v — step 1 of `elaborated_xtyped` (Track B): the declarations    *)
(* table Σ, the representation table Rp and the functor-body table Fb that  *)
(* XTyping's judgments read, built from a reading W of the bundle           *)
(* (Elaboration.WorldOf) and an elaborated program.                          *)
(*                                                                           *)
(* Part V relates a bundle to its elaboration through relations (CtxOf,     *)
(* SigOf, PayloadOf, ProvideResult, LinkBody); Σ is a table of functions.   *)
(* Without the axiom of choice a function is not had from `for every        *)
(* symbol there is a context`, so what Track A's proofs give as functions,  *)
(* and the telescope lemma that ties Part V's contexts to Core Moss's, are  *)
(* the Hypotheses of section `FromWorld` (BRIDGE-A1 … A6), to be discharged  *)
(* from elaborate_sound and the elaborator's own tables.  The rest is       *)
(* definitions and what follows from them.                                  *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Permutation String.

From NM Require Import Audit CoreMoss XTyping MergeWf.
Import ListNotations.
Import Input Elaboration.

Section FromWorld.

Variable b : Input.bundle.
Variable W : world.
Hypothesis Hwo : WorldOf b W.
Variable P : xprogram.

(* ---- What Track A is asked for. -------------------------------------- *)

(* The symbols whose assume nesting forms a context. *)
Definition has_ctx (k : dkind) : bool :=
  match k with DKVal | DKFn | DKMeth | DKFunctor | DKCtx => true | _ => false end.

Definition kind_is (s : nat) (k : dkind) : bool :=
  match kind_sym W s, k with
  | Some DKType, DKType | Some DKUnit, DKUnit | Some DKAlias, DKAlias
  | Some DKTag, DKTag | Some DKVal, DKVal | Some DKFn, DKFn | Some DKCtx, DKCtx
  | Some DKMeth, DKMeth | Some DKFunctor, DKFunctor => true
  | _, _ => false
  end.

(* BRIDGE-A1 (item 2: CtxOf as a function).  Every symbol with an assume
   nesting has its context and needs, as functions of the symbol: e.g. the
   elaborator's own `ctx_of`, with elaborate_sound's proof that it is a
   CtxOf.  (Totality of CtxOf over the world's finitely many declarations
   would do as well: finite choice is constructive.) *)
Variable ctx_of : nat -> cx.
Variable needs_of : nat -> list item.
Hypothesis ctx_of_ok : forall s k, kind_sym W s = Some k -> has_ctx k = true ->
  CtxOf W s (ctx_of s) (needs_of s).

(* BRIDGE-A2 (item 3: the telescope lemma).  The context CtxOf forms is
   Core Moss's merged context of a telescope: s's requirements as type
   items (w_reqs's order) followed by its needs in a fixed order, each at
   its run-time key (rkey_of; a method need's θ is not mkey's []), merging
   (CoreMoss.merge_tele, [D43]) exactly as Part V's σ does.  This is the
   one place Part V's contexts (cx: meanings c_ty, merge c_sub, needs as a
   set) meet Core Moss's (ctx: items cA, merge cS). *)
Variable needs_order : nat -> list item.
(* A method need is read at its run-time key (note 28: θ, from c_apps):
   that is the item bodies are typed against (XtMeth reads θ from it). *)
Definition tele_of (s : nat) : tele :=
  map ITy (w_reqs W s) ++ map (rkey_of (ctx_of s)) (needs_order s).
Hypothesis tele_of_ok : forall s k, kind_sym W s = Some k -> has_ctx k = true ->
  exists PhiD, tele_ctx (tele_of s) = Some PhiD
  /\ Permutation (needs_order s) (needs_of s)
  /\ tyreqs (cA PhiD) = w_reqs W s
  (* Core's merge is Part V's σ, and a requirement's meaning is its own
     generation under it *)
  /\ (forall t, app_subst (cS PhiD) t = canon (ctx_of s) t)
  /\ (forall r, In r (w_reqs W s) -> nlook r (c_ty (ctx_of s)) = Some (canon (ctx_of s) (TAbs r)))
  (* and the context gives meanings to its requirements only (a bracket
     binds only while the item it is on is read: Audit note 71) *)
  /\ (forall t x, nlook t (c_ty (ctx_of s)) = Some x -> In t (w_reqs W s)).

(* BRIDGE-A3: declared types as functions.  A fn's or method's signature
   read in its own context (SigOf; a method's receiver is Core's
   `TAbs this_sym`), a val's type, and a tag's payload at its own
   requirements (PayloadOf at the identity). *)
Variable sig_of : nat -> fsig.
Hypothesis sig_of_ok : forall s, kind_is s DKFn = true \/ kind_is s DKMeth = true ->
  SigOf W s (c_ty (ctx_of s)) (if kind_is s DKMeth then Some (TAbs this_sym) else None) (sig_of s).
Variable val_ty_of : nat -> ty.
Hypothesis val_ty_of_ok : forall s d n t, w_decl W s = Some d -> d_ast d = ADVal n t None ->
  TyOf W (d_mod d) (c_ty (ctx_of s)) None t (val_ty_of s).
Definition id_args (s : nat) : list (nat * ty) := map (fun r => (r, TAbs r)) (w_reqs W s).
Variable pay_of : nat -> ty.
Hypothesis pay_of_ok : forall s, kind_is s DKTag = true -> PayloadOf W s (id_args s) (pay_of s).

(* BRIDGE-A4: an abstract functor's argument telescope, from FunCtx (not
   its assume nesting: an application satisfies FunCtx's needs, BApply),
   laid out as a symbol's is — its input types, then its needs at their
   run-time keys in a fixed order; its result telescope in its own terms
   (ProvideResult at FunCtx's context, outputs as their own generations),
   in a fixed order; and its fixed inputs X(F) as a list. *)
Variable fun_ctx_of : nat -> cx.
Variable fun_needs_of fun_needs_order : nat -> list item.
Variable fun_tys_of : nat -> list nat.
Definition fun_args_of (u : nat) : tele :=
  map ITy (fun_tys_of u) ++ map (rkey_of (fun_ctx_of u)) (fun_needs_order u).
Hypothesis fun_ctx_of_ok : forall u d n ps a r, w_decl W u = Some d -> d_ast d = ADFunctor n ps a r None ->
  FunCtx W u (fun_ctx_of u) (fun_needs_of u)
  /\ Permutation (fun_needs_order u) (fun_needs_of u)
  /\ (forall t, In t (fun_tys_of u) -> kind_sym W t = Some DKType).
Variable fun_res_of : nat -> tele.
Variable fixed_of : nat -> list tsym.
Hypothesis fixed_of_ok : forall u t, In t (fixed_of u) <-> Fixed W u t.

(* BRIDGE-A5: a functor with a body made by linking (LinkBody: a fresh
   symbol h, `funbody_of P h`): its linked arguments, the inputs it
   captures and its pins, as functions of h. *)
Variable fb_of : usym -> option (tele * tele * subst).
Hypothesis fb_of_body : forall h x, fb_of h = Some x -> funbody_of P h <> None.

(* BRIDGE-A6: nominal symbols for the platform's scalars.  `type I32;` and
   its kin are abstract types in lib/wasm.moss (TAbs in every context that
   assumes them); main's grounding sends them to nominals Σ gives the
   scalar reps.  They must be symbols no declaration has (and not the
   record head 0): the elaborator's symbol counter has such (ElabFrame's
   frame keeps new symbols above it). *)
Variables nI32 nI64 nF32 nF64 : nsym.
Hypothesis scalars_fresh : forall n, In n [nI32; nI64; nF32; nF64] -> n <> rec_head /\ w_decl W n = None.
Hypothesis scalars_nodup : NoDup [nI32; nI64; nF32; nF64].

(* ---- Σ. -------------------------------------------------------------- *)

Definition is_scalar (n : nsym) : bool :=
  Nat.eqb n nI32 || Nat.eqb n nI64 || Nat.eqb n nF32 || Nat.eqb n nF64.

(* The platform's GC reference tags (lib/wasm.moss's `tag Ref[Boxed]
   (StructRef)` and `tag Array[Arrayed](ArrayRef)`), by the module the
   elaborator reads them from. *)
Variable wasm_ref wasm_arr : option nat.
Hypothesis wasm_ref_ok : forall r, wasm_ref = Some r ->
  LangSym W "wasm"%string NSName "Ref"%string r /\ w_reqs W r <> [].
Hypothesis wasm_arr_ok : forall r, wasm_arr = Some r ->
  LangSym W "wasm"%string NSName "Array"%string r /\ w_reqs W r <> [].

Definition is_some_eq (o : option nat) (n : nat) : bool :=
  match o with Some r => Nat.eqb r n | None => false end.

Definition Rp_of (n : nsym) : option rep :=
  if Nat.eqb n nI32 then Some RI32
  else if Nat.eqb n nI64 then Some RI64
  else if Nat.eqb n nF32 then Some RF32
  else if Nat.eqb n nF64 then Some RF64
  else if is_some_eq wasm_ref n then Some (RRef (hd 0 (w_reqs W n)))
  else if is_some_eq wasm_arr n then Some (RArr (hd 0 (w_reqs W n)))
  else None.

Definition g_tag_of (n : nsym) : option (tele * ty) :=
  if is_scalar n then Some ([], TUnit)
  else if kind_is n DKUnit then Some ([], TUnit)
  else if kind_is n DKTag then
    Some (map ITy (w_reqs W n),
          match Rp_of n with Some _ => TUnit | None => pay_of n end)
  else None.

Definition Sg_of : gsig := mkGsig
  (fun t => kind_is t DKType)
  g_tag_of
  (fun v => match w_decl W v with
            | Some d => match d_ast d with
                        | ADVal _ _ None => Some (tele_of v, val_ty_of v)
                        | _ => None end
            | None => None end)
  (fun f => if kind_is f DKFn
            then Some (tele_of f, mkSig (sg_params (sig_of f)) (sg_ret (sig_of f)), None)
            else None)
  (fun m => if kind_is m DKMeth
            then Some (tele_of m, mkSig (sg_params (sig_of m)) (sg_ret (sig_of m)))
            else None)
  (fun u => if kind_is u DKFunctor
            then match w_decl W u with
                 | Some d => match d_ast d with
                             | ADFunctor _ _ _ _ None => Some (fun_args_of u, fun_res_of u, None)
                             | _ => None end
                 | None => None end
            else None)
  fixed_of
  (fun _ => [])
  (fun _ => ([], [])).

Definition Fb_of : usym -> option (tele * tele * subst) := fb_of.

(* ---- What follows by construction. ------------------------------------ *)

Lemma kind_is_sym : forall s k, kind_is s k = true -> kind_sym W s = Some k.
Proof.
  intros s k H. unfold kind_is in H.
  destruct (kind_sym W s) as [[]|]; destruct k; try discriminate; reflexivity.
Qed.

Lemma kind_is_decl : forall s k, kind_is s k = true -> exists d, w_decl W s = Some d.
Proof.
  intros s k H. apply kind_is_sym in H. unfold kind_sym in H.
  destruct (w_decl W s) as [d|]; [exists d; reflexivity | discriminate].
Qed.

(* xwf_no_core_bodies: Σ holds no fn bodies (they are in P). *)
Lemma Sg_of_no_core_bodies : forall f D S bd, g_fn Sg_of f = Some (D, S, bd) -> bd = None.
Proof.
  intros f D S bd H. simpl in H. destruct (kind_is f DKFn); [| discriminate].
  injection H as _ _ <-. reflexivity.
Qed.

(* xwf_fun_abstract: Σ declares only abstract functors. *)
Lemma Sg_of_fun_abstract : forall u Du Dr bd, g_fun Sg_of u = Some (Du, Dr, bd) -> bd = None.
Proof.
  intros u Du Dr bd H. simpl in H. destruct (kind_is u DKFunctor); [| discriminate].
  destruct (w_decl W u) as [d|]; [| discriminate].
  destruct (d_ast d); try discriminate. destruct o0; [discriminate|].
  injection H as _ _ <-. reflexivity.
Qed.

(* No declared symbol is a scalar nominal. *)
Lemma scalar_not_declared : forall n k, kind_is n k = true -> is_scalar n = false.
Proof.
  intros n k H. destruct (kind_is_decl _ _ H) as [d Hd].
  unfold is_scalar.
  destruct (Nat.eqb n nI32) eqn:E1;
    [apply Nat.eqb_eq in E1; subst; destruct (scalars_fresh nI32 (or_introl eq_refl)) as [_ E];
     rewrite E in Hd; discriminate|].
  destruct (Nat.eqb n nI64) eqn:E2;
    [apply Nat.eqb_eq in E2; subst; destruct (scalars_fresh nI64 (or_intror (or_introl eq_refl))) as [_ E];
     rewrite E in Hd; discriminate|].
  destruct (Nat.eqb n nF32) eqn:E3;
    [apply Nat.eqb_eq in E3; subst;
     destruct (scalars_fresh nF32 (or_intror (or_intror (or_introl eq_refl)))) as [_ E];
     rewrite E in Hd; discriminate|].
  destruct (Nat.eqb n nF64) eqn:E4;
    [apply Nat.eqb_eq in E4; subst;
     destruct (scalars_fresh nF64 (or_intror (or_intror (or_intror (or_introl eq_refl))))) as [_ E];
     rewrite E in Hd; discriminate|].
  reflexivity.
Qed.

(* The scalar nominals are scalars to Rp and unit tags to Σ. *)
Lemma Rp_of_I32 : Rp_of nI32 = Some RI32.
Proof. unfold Rp_of. rewrite Nat.eqb_refl. reflexivity. Qed.

Lemma g_tag_of_scalar : forall n, is_scalar n = true -> g_tag_of n = Some ([], TUnit).
Proof. intros n H. unfold g_tag_of. rewrite H. reflexivity. Qed.

(* A tag Rp does not represent is a plain tag over its payload. *)
Lemma g_tag_of_plain : forall n, kind_is n DKTag = true -> Rp_of n = None ->
  g_tag_of n = Some (map ITy (w_reqs W n), pay_of n).
Proof.
  intros n Hk Hr. unfold g_tag_of. rewrite (scalar_not_declared _ _ Hk).
  assert (Hu : kind_is n DKUnit = false).
  { unfold kind_is in *. destruct (kind_sym W n) as [[]|]; try discriminate; reflexivity. }
  rewrite Hu, Hk, Hr. reflexivity.
Qed.

(* The record head is no symbol's (xwf_rec): 0 is never declared
   (wo_decl_sound) and no scalar nominal. *)
Lemma kind_is_zero : forall k, kind_is rec_head k = false.
Proof.
  intros k. destruct (kind_is rec_head k) eqn:E; [| reflexivity].
  destruct (kind_is_decl _ _ E) as [d Hd].
  destruct (wo_decl_sound Hwo rec_head Hd) as [Hz _]. exfalso. apply Hz. reflexivity.
Qed.

Lemma scalar_zero : is_scalar rec_head = false.
Proof.
  unfold is_scalar.
  assert (Hne : forall n, In n [nI32; nI64; nF32; nF64] -> Nat.eqb rec_head n = false).
  { intros n Hn. apply Nat.eqb_neq. intros E. destruct (scalars_fresh n Hn) as [H0 _].
    apply H0. symmetry. exact E. }
  rewrite (Hne nI32), (Hne nI64), (Hne nF32), (Hne nF64); simpl; auto 6.
Qed.

Lemma Sg_of_rec : Rp_of rec_head = None /\ g_tag Sg_of rec_head = None.
Proof.
  split.
  - unfold Rp_of.
    assert (Hs := scalar_zero). unfold is_scalar in Hs.
    repeat match goal with |- context [Nat.eqb rec_head ?x] =>
      let E := fresh in destruct (Nat.eqb rec_head x) eqn:E; [simpl in Hs; rewrite ?orb_true_r in Hs; discriminate|] end.
    unfold is_some_eq.
    destruct wasm_ref as [r|] eqn:Er.
    + destruct (Nat.eqb r rec_head) eqn:E.
      * apply Nat.eqb_eq in E. subst r. destruct (wasm_ref_ok rec_head eq_refl) as [[d [Hd _]] _].
        destruct (wo_decl_sound Hwo rec_head Hd) as [Hz _]. exfalso. apply Hz. reflexivity.
      * destruct wasm_arr as [a|] eqn:Ea; [| reflexivity].
        destruct (Nat.eqb a rec_head) eqn:E2; [| reflexivity].
        apply Nat.eqb_eq in E2. subst a. destruct (wasm_arr_ok rec_head eq_refl) as [[d [Hd _]] _].
        destruct (wo_decl_sound Hwo rec_head Hd) as [Hz _]. exfalso. apply Hz. reflexivity.
    + destruct wasm_arr as [a|] eqn:Ea; [| reflexivity].
      destruct (Nat.eqb a rec_head) eqn:E2; [| reflexivity].
      apply Nat.eqb_eq in E2. subst a. destruct (wasm_arr_ok rec_head eq_refl) as [[d [Hd _]] _].
      destruct (wo_decl_sound Hwo rec_head Hd) as [Hz _]. exfalso. apply Hz. reflexivity.
  - simpl. unfold g_tag_of. rewrite scalar_zero, !kind_is_zero. reflexivity.
Qed.

(* ---- Requirements are declared types, so tag telescopes form. -------- *)

Lemma spec_types_type : forall p sp t, SpecTypes W p sp t -> kind_sym W t = Some DKType.
Proof. intros p sp t H. induction H; assumption. Qed.

Lemma assume_types_type : forall s t, AssumeTypes W s t -> kind_sym W t = Some DKType.
Proof. intros s t [d [pa [_ [_ H]]]]. exact (spec_types_type _ _ _ H). Qed.

Definition is_type (t : nat) : bool := kind_is t DKType.

Lemma kind_is_type : forall t, kind_sym W t = Some DKType -> kind_is t DKType = true.
Proof. intros t H. unfold kind_is. rewrite H. reflexivity. Qed.

Lemma dkind_eq : forall a c : dkind, {a = c} + {a <> c}.
Proof. decide equality. Qed.

(* What a module's names denote: a declared symbol, or a loaded module. *)
Definition ent_ok (e : ent) : Prop :=
  match e with ESym x => exists d, w_decl W x = Some d | EMod t => Loaded b t end.

Lemma exports_ok : forall p ns n e, Exports b W p ns n e -> Loaded b p -> ent_ok e.
Proof.
  fix IH 5. intros p ns n e H Hl.
  destruct H as [p x d ns n Hd _ _ | p m t imp u e ns Hm Hi _ _ _ Hor | p m t imp a Hm Hi _].
  - exists d. exact Hd.
  - assert (Ht : Loaded b t) by (eapply LoadImport; eassumption).
    destruct Hor as [He | [_ [_ He]]]; exact (IH _ _ _ _ He Ht).
  - eapply LoadImport; eassumption.
Qed.

Lemma scope_ok : forall p ns n e, InScope b W p ns n e -> Loaded b p -> ent_ok e.
Proof.
  intros p ns n e H Hl. destruct H as [p ns n e He | p m t imp ns n e Hm Hi _ He].
  - exact (exports_ok _ _ _ _ He Hl).
  - apply (exports_ok _ _ _ _ He). eapply LoadImport; eassumption.
Qed.

Lemma resolve_rest_ok : forall segs e e', ent_ok e -> resolve_rest W e segs = Some e' -> ent_ok e'.
Proof.
  induction segs as [|seg r IH]; intros e e' He Hr; simpl in Hr.
  - injection Hr as <-. exact He.
  - destruct e as [x|t]; [discriminate|]. simpl in He.
    destruct (w_exp W t NSName seg) as [e1|] eqn:E1.
    + apply (IH e1); [| exact Hr]. apply (exports_ok _ _ _ _ (proj1 (wo_exports Hwo NSName seg e1 He) E1) He).
    + destruct (w_exp W t NSAlias seg) as [e1|] eqn:E2; [| discriminate].
      apply (IH e1); [| exact Hr]. apply (exports_ok _ _ _ _ (proj1 (wo_exports Hwo NSAlias seg e1 He) E2) He).
Qed.

(* A path resolved in a loaded module denotes a declared symbol. *)
Lemma resolve_sym_declared : forall p pa x, Loaded b p -> resolve_sym W p pa = Some x ->
  exists d, w_decl W x = Some d.
Proof.
  intros p pa x Hl H. unfold resolve_sym in H.
  destruct (resolve W p pa) as [[y|]|] eqn:E; try discriminate. injection H as <-.
  unfold resolve in E. destruct pa as [|hd rest]; [discriminate|].
  change (ent_ok (ESym y)).
  destruct (w_scope W p NSName hd) as [e|] eqn:E1.
  - apply (resolve_rest_ok rest e); [| exact E].
    exact (scope_ok _ _ _ _ (proj1 (wo_scope Hwo NSName hd e Hl) E1) Hl).
  - destruct (w_scope W p NSAlias hd) as [e|] eqn:E2; [| discriminate].
    apply (resolve_rest_ok rest e); [| exact E].
    exact (scope_ok _ _ _ _ (proj1 (wo_scope Hwo NSAlias hd e Hl) E2) Hl).
Qed.

Lemma decl_loaded : forall s d, w_decl W s = Some d -> Loaded b (d_mod d).
Proof. intros s d H. exact (proj1 (proj1 (proj2 (wo_decl_sound Hwo s H)))). Qed.

(* Requirements of a symbol that is neither a tag nor an alias are types. *)
Lemma plain_reqs_types : forall s k, kind_sym W s = Some k -> k <> DKTag -> k <> DKAlias ->
  forall t, In t (w_reqs W s) -> kind_sym W t = Some DKType.
Proof.
  intros s k Hk Ht Ha t Hin.
  destruct (dkind_eq k DKType) as [->|Hty].
  { rewrite (wo_reqs_none Hwo s (or_introl Hk)) in Hin. destruct Hin. }
  destruct (dkind_eq k DKUnit) as [->|Hun].
  { rewrite (wo_reqs_none Hwo s (or_intror Hk)) in Hin. destruct Hin. }
  apply (assume_types_type s t).
  exact (proj1 (wo_reqs_plain Hwo s Hk Ht Ha Hty Hun t) Hin).
Qed.

Definition nominal_kind (g : nat) : bool := kind_is g DKTag || kind_is g DKAlias.

(* The type-only part of the requirements of tags and aliases. *)
Definition Rtype (g : nat) : list nat :=
  if nominal_kind g then filter is_type (w_reqs W g) else w_reqs W g.

Lemma Rtype_incl : forall g, incl (Rtype g) (w_reqs W g).
Proof.
  intros g x Hx. unfold Rtype in Hx. destruct (nominal_kind g); [| exact Hx].
  apply filter_In in Hx. exact (proj1 Hx).
Qed.

Lemma freein_mono : forall R R' p t x, (forall g, incl (R g) (R' g)) ->
  FreeIn W R p t x -> FreeIn W R' p t x.
Proof.
  intros R R' p t x Hi H. induction H.
  - eapply FRec; eassumption.
  - eapply FUnion; eassumption.
  - eapply FType; eassumption.
  - eapply FNom; [eassumption | eassumption | apply Hi; eassumption | eassumption].
  - eapply FBracket; eassumption.
Qed.

Lemma kind_not_nominal : forall g k, kind_sym W g = Some k -> nominal_kind g = false ->
  k <> DKTag /\ k <> DKAlias.
Proof.
  intros g k Hk Hn. unfold nominal_kind, kind_is in Hn. rewrite Hk in Hn.
  destruct k; simpl in Hn; try discriminate; split; discriminate.
Qed.

(* What FreeIn reads at Rtype, in a loaded module, is a type. *)
Lemma freein_type : forall p t x, Loaded b p -> FreeIn W Rtype p t x -> kind_sym W x = Some DKType.
Proof.
  intros p t x Hl H. induction H as [| | pa a t Hr Hk _ | pa a g x Hr Hk Hin _ | ]; auto.
  destruct (resolve_sym_declared p pa g Hl Hr) as [d Hd].
  unfold Rtype in Hin. destruct (nominal_kind g) eqn:En.
  - apply filter_In in Hin. destruct Hin as [_ Hx]. unfold is_type in Hx.
    exact (kind_is_sym _ _ Hx).
  - assert (Hk' : exists k, kind_sym W g = Some k).
    { unfold kind_sym. rewrite Hd. destruct (wo_decl_sound Hwo g Hd) as [_ [_ Hkd]].
      destruct (kind_of (d_ast d)) as [k|]; [exists k; reflexivity | congruence]. }
    destruct Hk' as [k Hk'].
    destruct (kind_not_nominal g k Hk' En) as [Ht Ha].
    exact (plain_reqs_types g k Hk' Ht Ha x Hin).
Qed.

Lemma nominal_body_kind : forall g d ps t, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) ->
  nominal_kind g = true.
Proof.
  intros g d ps t Hd Hn. unfold nominal_kind, kind_is, kind_sym. rewrite Hd.
  destruct (d_ast d); simpl in Hn; try discriminate; reflexivity.
Qed.

Lemma Rtype_closed : RelevanceClosed W Rtype.
Proof.
  intros g d ps t Hd Hn. pose proof (nominal_body_kind g d ps t Hd Hn) as Hg.
  destruct (wo_reqs_closed Hwo g Hd Hn) as [Hlist Hfree].
  unfold Rtype. rewrite Hg. split.
  - intros n x Hin Hr. apply filter_In. split; [exact (Hlist n x Hin Hr) |].
    destruct (wo_listed Hwo g n Hd Hn Hin) as [x' [Hr' Hat]].
    rewrite Hr in Hr'. injection Hr' as <-. unfold is_type.
    apply kind_is_type. exact (assume_types_type _ _ Hat).
  - intros x Hx. apply filter_In. split.
    + apply Hfree. exact (freein_mono _ _ _ _ _ Rtype_incl Hx).
    + unfold is_type. apply kind_is_type. exact (freein_type _ _ _ (decl_loaded g d Hd) Hx).
Qed.

(* Every requirement of every declared symbol is a declared `type`.  For a
   tag or an alias this is the leastness of its requirements
   (wo_reqs_least): their type-only part is itself relevance-closed. *)
Lemma reqs_are_types : forall s k, kind_sym W s = Some k ->
  forall t, In t (w_reqs W s) -> kind_sym W t = Some DKType.
Proof.
  intros s k Hk t Hin.
  destruct (nominal_kind s) eqn:En.
  - assert (Hk2 : kind_sym W s = Some DKTag \/ kind_sym W s = Some DKAlias).
    { unfold nominal_kind, kind_is in En. rewrite Hk in En.
      destruct k; simpl in En; try discriminate; [right | left]; exact Hk. }
    assert (Hagree : forall s', kind_sym W s' <> Some DKTag -> kind_sym W s' <> Some DKAlias ->
                       Rtype s' = w_reqs W s').
    { intros s' H1 H2. unfold Rtype, nominal_kind, kind_is.
      destruct (kind_sym W s') as [[]|]; try reflexivity; congruence. }
    pose proof (wo_reqs_least Hwo Rtype_closed Hagree s Hk2 t Hin) as Hr.
    unfold Rtype in Hr. rewrite En in Hr. apply filter_In in Hr.
    exact (kind_is_sym _ _ (proj2 Hr)).
  - destruct (kind_not_nominal s k Hk En) as [Ht Ha].
    exact (plain_reqs_types s k Hk Ht Ha t Hin).
Qed.



Lemma find_same_key_none : forall sb A t, ~ In (ITy t) A -> find_same_key sb A (ITy t) = None.
Proof.
  intros sb A t. induction A as [|j A IH]; intros Hn; simpl; [reflexivity|].
  destruct j as [u| | | |]; simpl;
    try (apply IH; intros H; apply Hn; right; exact H).
  destruct (Nat.eqb u t) eqn:E.
  - apply Nat.eqb_eq in E. subst. exfalso. apply Hn. left. reflexivity.
  - apply IH. intros H. apply Hn. right. exact H.
Qed.

(* Distinct declared types, appended to a context with none of them, form. *)
Lemma wf_tele_types : forall l A, NoDup l ->
  (forall t, In t l -> kind_sym W t = Some DKType) ->
  (forall t, In t l -> ~ In (ITy t) A) ->
  wf_tele Sg_of (mkCtx A []) (map ITy l) (mkCtx (A ++ map ITy l) []).
Proof.
  intros l. induction l as [|t l IH]; intros A Hnd Hty Hfr; simpl.
  - rewrite app_nil_r. constructor.
  - inversion Hnd as [|? ? Hnin Hnd']; subst.
    eapply WCons.
    + constructor.
      * simpl. apply kind_is_type. apply Hty. left. reflexivity.
      * intros E. destruct (kind_is_decl t DKType (kind_is_type _ (Hty t (or_introl eq_refl)))) as [d Hd].
        destruct (wo_decl_sound Hwo t Hd) as [Hn0 _]. apply Hn0. rewrite E. reflexivity.
    + unfold merge_item. simpl. rewrite find_same_key_none by (apply Hfr; left; reflexivity).
      reflexivity.
    + replace (A ++ ITy t :: map ITy l) with ((A ++ [ITy t]) ++ map ITy l)
        by (rewrite <- app_assoc; reflexivity).
      apply IH; [exact Hnd' | intros u Hu; apply Hty; right; exact Hu |].
      intros u Hu Hin. apply in_app_or in Hin. destruct Hin as [Hin | [E | []]].
      * exact (Hfr u (or_intror Hu) Hin).
      * injection E as ->. exact (Hnin Hu).
Qed.

(* A tag's (or any symbol's) requirement telescope forms. *)
Lemma reqs_tele_forms : forall s k, kind_sym W s = Some k ->
  wf_tele Sg_of empty_ctx (map ITy (w_reqs W s)) (mkCtx (map ITy (w_reqs W s)) []).
Proof.
  intros s k Hk. apply (wf_tele_types (w_reqs W s) []).
  - apply (wo_reqs_nodup Hwo).
  - exact (reqs_are_types s k Hk).
  - intros t _ [].
Qed.

(* ---- Every type the relation reads is formed in Σ. --------------------- *)

Lemma avail_ty_of_in : forall Phi t, In (ITy t) (cA Phi) -> avail Phi (ITy t).
Proof.
  intros Phi t H. unfold avail, avail_b. apply existsb_exists. exists (ITy t).
  split; [exact H|]. unfold item_subst_eqb. apply item_eqb_eq. reflexivity.
Qed.

Lemma nlook_app : forall (l1 l2 : list (nat * ty)) t x,
  nlook t (l1 ++ l2) = Some x -> nlook t l1 = Some x \/ nlook t l2 = Some x.
Proof.
  induction l1 as [|[k v] l1 IH]; intros l2 t x H; simpl in *; [right; exact H|].
  destruct (Nat.eqb t k); [left; exact H | exact (IH l2 t x H)].
Qed.

Lemma nlook_in : forall (l : list (nat * ty)) t x, nlook t l = Some x -> In (t, x) l.
Proof.
  induction l as [|[k v] l IH]; intros t x H; simpl in *; [discriminate|].
  destruct (Nat.eqb t k) eqn:E; [apply Nat.eqb_eq in E; subst; injection H as <-; left; reflexivity |].
  right. exact (IH t x H).
Qed.

Lemma nlook_rev_in : forall (l : list (nat * ty)) t x, nlook t (rev l) = Some x -> In (t, x) l.
Proof. intros l t x H. apply in_rev. exact (nlook_in _ _ _ H). Qed.

(* A tag's or unit's declaration in Σ, with its telescope formed. *)
Lemma g_tag_of_nominal : forall g k, kind_sym W g = Some k -> (k = DKTag \/ k = DKUnit) ->
  exists taup, g_tag Sg_of g = Some (map ITy (w_reqs W g), taup).
Proof.
  intros g k Hk Hkk. simpl. unfold g_tag_of.
  assert (Hki : kind_is g k = true) by (unfold kind_is; rewrite Hk; destruct Hkk; subst; reflexivity).
  rewrite (scalar_not_declared _ _ Hki).
  destruct Hkk as [-> | ->].
  - assert (Hu : kind_is g DKUnit = false) by (unfold kind_is; rewrite Hk; reflexivity).
    rewrite Hu, Hki. eexists. reflexivity.
  - rewrite Hki. rewrite (wo_reqs_none Hwo g (or_intror Hk)). eexists. reflexivity.
Qed.

Lemma tele_ctx_reqs : forall s k, kind_sym W s = Some k ->
  tele_ctx (map ITy (w_reqs W s)) = Some (mkCtx (map ITy (w_reqs W s)) []).
Proof. intros s k Hk. exact (wf_tele_ctx (reqs_tele_forms s k Hk)). Qed.

Lemma tyreqs_map_ITy' : forall l, tyreqs (map ITy l) = l.
Proof. induction l as [|a l IH]; simpl; [reflexivity | rewrite IH; reflexivity]. Qed.

(* `TyOf W p tm th a τ`, with every meaning in tm (and `this`) formed in Φ,
   reads a formed type. *)
Definition meanings_wf (Phi : ctx) (tm : list (nat * ty)) : Prop :=
  forall t x, nlook t tm = Some x -> wf_ty Sg_of Phi x.

Fixpoint tyof_wf p tm th a tau (H : TyOf W p tm th a tau) {struct H} :
  forall Phi, Loaded b p -> meanings_wf Phi tm ->
  (forall x, th = Some x -> wf_ty Sg_of Phi x) -> wf_ty Sg_of Phi tau
with brackets_wf p tm g a bs (H : Brackets W p tm g a bs) {struct H} :
  forall Phi, Loaded b p -> meanings_wf Phi tm -> meanings_wf Phi bs.
Proof.
  - destruct H as [p tm th | p tm th | p tm t | p tm th fs ts Hf | p tm th ms ts flat Hf Ef Hnom Hnd
                  | p tm th pa t tau Hr Hk Hnl | p tm th pa a g k bs args Hr Hk Hkk Hb Hargs
                  | p tm th pa a g d n body bs tau Hr Hd Ha Hb Hbody ];
      intros Phi Hl Hm Ht.
    + (* never *) apply WfUnion; constructor.
    + constructor.
    + exact (Ht t eq_refl).
    + (* a record *) apply WfRec; [exact (proj2 Sg_of_rec) |].
      induction Hf as [|nt ft fs ts [_ Hy] Hf' IHf]; constructor; [| exact IHf].
      exact (tyof_wf _ _ _ _ _ Hy Phi Hl Hm Ht).
    + (* a union *)
      assert (Hts : Forall (wf_ty Sg_of Phi) ts).
      { clear Ef Hnom Hnd. induction Hf as [|m t ms ts Hy Hf' IHf]; constructor; [| exact IHf].
        exact (tyof_wf _ _ _ _ _ Hy Phi Hl Hm Ht). }
      subst flat. apply WfUnion.
      * rewrite Forall_forall in Hts |- *. intros x Hx. apply in_flat_map in Hx.
        destruct Hx as [t [Ht0 Hx]]. pose proof (Hts t Ht0) as Hw.
        destruct t as [| | n th0 | ms0]; simpl in Hx;
          try (destruct Hx as [<- | []]; exact Hw).
        inversion Hw; subst. rewrite Forall_forall in *. auto.
      * eapply Forall_impl; [| exact Hnom]. intros m [n [xs [-> _]]]. exists n, xs. reflexivity.
      * replace (heads_of (cS Phi) (flat_map members ts)) with (map head (flat_map members ts));
          [exact Hnd |].
        unfold heads_of. apply map_ext_in. intros m Hm0.
        rewrite Forall_forall in Hnom. destruct (Hnom m Hm0) as [n [xs [-> _]]]. reflexivity.
    + (* a type symbol: its meaning *) exact (Hm t tau Hnl).
    + (* a nominal *)
      destruct (g_tag_of_nominal g k Hk Hkk) as [taup Hg].
      assert (Hbs : meanings_wf Phi bs) by exact (brackets_wf _ _ _ _ _ Hb Phi Hl Hm).
      assert (Hdom : map fst args = w_reqs W g).
      { clear -Hargs. induction Hargs as [|r ra rs ras [E _] _ IH]; simpl; [reflexivity | rewrite E, IH; reflexivity]. }
      eapply WfTag; [exact Hg | |].
      * exists (mkCtx (map ITy (w_reqs W g)) []). split; [exact (tele_ctx_reqs g k Hk) |].
        split; [simpl; rewrite tyreqs_map_ITy'; exact Hdom |].
        split; [| constructor].
        apply Forall_forall. intros i Hi. left. apply in_map_iff in Hi. destruct Hi as [x [<- _]]. reflexivity.
      * rewrite Forall_forall. intros [r x] Hin.
        assert (Hl' : nlook r (bs ++ tm) = Some x).
        { clear -Hargs Hin. induction Hargs as [|r0 ra rs ras [E Hn] _ IH]; [destruct Hin|].
          destruct Hin as [E2 | Hin]; [subst ra; simpl in E, Hn; subst r0; exact Hn | exact (IH Hin)]. }
        destruct (nlook_app _ _ _ _ Hl') as [H1 | H1]; [exact (Hbs _ _ H1) | exact (Hm _ _ H1)].
    + (* an alias: its body, read with the brackets *)
      apply (tyof_wf _ _ _ _ _ Hbody Phi).
      * exact (decl_loaded g d Hd).
      * intros t x Hx. destruct (nlook_app _ _ _ _ Hx) as [H1 | H1];
          [exact (brackets_wf _ _ _ _ _ Hb Phi Hl Hm _ _ H1) | exact (Hm _ _ H1)].
      * intros x E. discriminate E.
  - destruct H as [p tm g | p tm g bs l Hf]; intros Phi Hl Hm t x Hx; [discriminate Hx|].
    apply nlook_rev_in in Hx.
    induction Hf as [|b0 tb bs l [sp [sa [_ [_ [_ [_ Hy]]]]]] Hf' IHf]; [destruct Hx|].
    destruct Hx as [E0 | Hx]; [| exact (IHf Hx)].
    subst tb. exact (tyof_wf _ _ _ _ _ Hy Phi Hl Hm (fun x E => ltac:(discriminate E))).
Qed.

(* So every tag's declaration in Σ is formed. *)
Lemma tag_decl_wf : forall n D taup, g_tag Sg_of n = Some (D, taup) ->
  exists PhiD, wf_tele Sg_of empty_ctx D PhiD /\ wf_ty Sg_of PhiD taup.
Proof.
  intros n D taup H. simpl in H. unfold g_tag_of in H.
  destruct (is_scalar n); [injection H as <- <-; exists empty_ctx; split; constructor |].
  destruct (kind_is n DKUnit); [injection H as <- <-; exists empty_ctx; split; constructor |].
  destruct (kind_is n DKTag) eqn:Ek; [| discriminate].
  injection H as <- <-.
  pose proof (kind_is_sym _ _ Ek) as Hk.
  exists (mkCtx (map ITy (w_reqs W n)) []).
  split; [exact (reqs_tele_forms n DKTag Hk) |].
  destruct (Rp_of n); [constructor |].
  pose proof (pay_of_ok n Ek) as Hp.
  inversion Hp as [s xs d n' ps t pay Hd Ha Hty E1 E2 E3]. subst s xs pay.
  apply (tyof_wf _ _ _ _ _ Hty).
  - exact (decl_loaded n d Hd).
  - intros r x Hx. unfold id_args in Hx.
    assert (Hin : In (r, x) (map (fun r => (r, TAbs r)) (w_reqs W n))) by exact (nlook_in _ _ _ Hx).
    apply in_map_iff in Hin. destruct Hin as [r' [E Hr']]. injection E as <- <-.
    apply WfNeed. apply avail_ty_of_in. simpl. apply in_map. exact Hr'.
  - intros x E. discriminate E.
Qed.

(* ---- What an assume nesting introduces is declared. -------------------- *)

Lemma kind_sym_decl : forall s d, w_decl W s = Some d -> kind_sym W s = kind_of (d_ast d).
Proof. intros s d H. unfold kind_sym. rewrite H. reflexivity. Qed.

(* A detached method name denotes a method. *)
Fixpoint exports_detached p ns n e (H : Exports b W p ns n e) {struct H} :
  forall m, ns = NSDetached -> e = ESym m -> kind_sym W m = Some DKMeth.
Proof.
  destruct H as [p x d ns n Hd _ Hf | p m0 t imp u e ns _ _ _ _ Hns Hor | p m0 t imp a _ _ _];
    intros m Ens Ee.
  - injection Ee as <-. subst ns. rewrite (kind_sym_decl _ _ Hd).
    destruct (d_ast d) as [| | | | | | [| |] | |]; simpl in Hf; try discriminate; reflexivity.
  - destruct e as [y|t']; [| discriminate]. injection Ee as <-. simpl in Ens. subst ns.
    destruct Hor as [He | [Hdot _]].
    + exact (exports_detached _ _ _ _ He y Ens eq_refl).
    + rewrite Hdot in Ens. discriminate.
  - discriminate.
Qed.

Lemma named_meth : forall p r n m, Loaded b p -> MethodNamed W p r n m -> kind_sym W m = Some DKMeth.
Proof.
  intros p r n m Hl [Ha | [_ Hs]].
  - unfold Attached in Ha. apply (wo_attached Hwo) in Ha.
    destruct Ha as [d [rn [ps [rt [body [Hd [Ha _]]]]]]].
    rewrite (kind_sym_decl _ _ Hd), Ha. reflexivity.
  - apply (wo_scope Hwo NSDetached n (ESym m) Hl) in Hs.
    inversion Hs; subst; (eapply exports_detached; [eassumption | reflexivity | reflexivity]).
Qed.

Definition entry_ok (e : entry) : Prop :=
  match e with
  | NTy t => kind_sym W t = Some DKType
  | NVal v _ _ => exists d n t, w_decl W v = Some d /\ d_ast d = ADVal n t None
  | NFn f _ _ => kind_sym W f = Some DKFn
  | NMth _ m _ _ => kind_sym W m = Some DKMeth
  | NFun u => exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None
  end.

Fixpoint entries_ok p ob tm sp es tm' (H : ItemEntries W p ob tm sp es tm') {struct H} :
  Loaded b p -> Forall entry_ok es
with entries_ok_l p ob tm sps es tm' (H : ItemsEntries W p ob tm sps es tm') {struct H} :
  Loaded b p -> Forall entry_ok es.
Proof.
  - destruct H as [p ob tm pa t Hr Hk | p ob tm pa s k Hr Hk Hkk
                  | p ob tm a pa s d n t ty0 bs ap Hr Hd Ha _ _ _
                  | p ob tm a pa s g bs ap Hr Hk _ _ _
                  | p ob tm pa u d n ps args res Hr Hd Ha
                  | p ob tm a pa dn r m rt g bs ap Hr Hm _ _ _ _
                  | p ob tm a pa c d n its bs es tm' Hr Hd Ha _ Hits];
      intros Hl.
    + constructor; [exact Hk | constructor].
    + constructor.
    + constructor; [exists d, n, t; auto | constructor].
    + constructor; [exact Hk | constructor].
    + constructor; [exists d, n, ps, args, res; auto | constructor].
    + constructor; [exact (named_meth _ _ _ _ Hl Hm) | constructor].
    + apply (entries_ok_l _ _ _ _ _ _ Hits). exact (decl_loaded c d Hd).
  - destruct H as [p ob tm | p ob tm sp r es1 tm1 es2 tm2 H1 H2]; intros Hl; [constructor |].
    apply Forall_app. split; [exact (entries_ok _ _ _ _ _ _ H1 Hl) | exact (entries_ok_l _ _ _ _ _ _ H2 Hl)].
Qed.

(* ---- Vals, fns and methods: wf_gsig's other clauses, against (A2'). -- *)

Definition declared_need (n : item) : Prop :=
  match n with
  | ITy _ => False
  | IVal v => exists d nm t, w_decl W v = Some d /\ d_ast d = ADVal nm t None
  | IFn f => kind_sym W f = Some DKFn
  | IMth _ m _ => kind_sym W m = Some DKMeth
  | IFun u => exists d nm ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor nm ps a r None
  end.

(* Needs come from entries (Needs), which are declared (entries_ok). *)
Lemma needs_declared : forall p sps es tm sg ns, Loaded b p ->
  ItemsEntries W p [] [] sps es tm -> Needs sg es ns -> forall n, In n ns -> declared_need n.
Proof.
  intros p sps es tm sg ns Hl Hes [_ Hneeds] n Hn.
  destruct (proj1 (Hneeds n) Hn) as [e [He Hkey]].
  pose proof (entries_ok_l _ _ _ _ _ _ Hes Hl) as Hok.
  rewrite Forall_forall in Hok. specialize (Hok e He).
  destruct e; simpl in Hkey, Hok; try discriminate; injection Hkey as <-; exact Hok.
Qed.

(* `main`'s root context (RootCtx, Audit note 54) adds Gc's abstract
   functors, declared as such, and the types they are fixed in. *)
Definition entry_ok' (e : entry) : Prop :=
  match e with NTy _ => True | _ => entry_ok e end.

Lemma root_items_ok : forall q es0 tm its es tm', RootItems W q es0 tm its es tm' -> Forall entry_ok' es.
Proof.
  intros q es0 tm its es tm' H.
  induction H as [es0 tm | es0 tm p a rest u du n ps args res xs es tm' Hr Hd Ha _ _ _ _ IH
                 | es0 tm sp rest es tm' _ _ IH].
  - constructor.
  - apply Forall_app. split.
    + apply Forall_forall. intros e He. apply in_map_iff in He as (x & <- & _). exact I.
    + constructor; [exists du, n, ps, args, res; auto | exact IH].
  - exact IH.
Qed.

Lemma root_ok : forall s es0 tm er tmr, RootCtx W s es0 tm er tmr -> Forall entry_ok' er.
Proof.
  intros s es0 tm er tmr H.
  destruct H as [_ | gc d n its es tm' _ _ _ _ Hi]; [constructor | exact (root_items_ok _ _ _ _ _ _ Hi)].
Qed.

Lemma needs_declared_root : forall p sps es tm er sg ns, Loaded b p ->
  ItemsEntries W p [] [] sps es tm -> Forall entry_ok' er -> Needs sg (es ++ er) ns ->
  forall n, In n ns -> declared_need n.
Proof.
  intros p sps es tm er sg ns Hl Hes Her [_ Hneeds] n Hn.
  destruct (proj1 (Hneeds n) Hn) as [e [He Hkey]].
  assert (Hok : entry_ok' e).
  { apply in_app_or in He as [He|He].
    - pose proof (entries_ok_l _ _ _ _ _ _ Hes Hl) as Hok. rewrite Forall_forall in Hok.
      specialize (Hok e He). destruct e; first [exact I | exact Hok].
    - rewrite Forall_forall in Her. exact (Her e He). }
  destruct e; simpl in Hkey, Hok; try discriminate; injection Hkey as <-; exact Hok.
Qed.

Lemma need_declared : forall s k, kind_sym W s = Some k -> has_ctx k = true ->
  forall n, In n (needs_of s) -> declared_need n.
Proof.
  intros s k Hk Hc.
  destruct (ctx_of_ok s k Hk Hc) as [d [es [tm [er [tmr [sg [Hd [Hes [Hroot [_ [_ Hneeds]]]]]]]]]]].
  exact (needs_declared_root _ _ _ _ _ _ _ (decl_loaded s d Hd) Hes (root_ok _ _ _ _ _ Hroot) Hneeds).
Qed.

(* BRIDGE-A2' (what the elaborator does not check: report of this branch).
   Each need of s's telescope is formed where it stands, with only the
   items to its left in force (Φᵢ, the merge of a prefix of tele_of s):
   a val or fn's own type requirements are in force there (Core's
   OkVal/OkFn; not its needs, which are answered where it is provided), a
   method key's receiver and θ are formed and θ gives the requirements the
   method's signature mentions, and an abstract functor's fixed inputs are
   in force.  That the needs are declared as such is proved
   (need_declared). *)
Definition need_ok (Phi : ctx) (i : item) : Prop :=
  match i with
  | ITy _ => True
  | IVal x | IFn x => Forall (fun t => avail Phi (ITy t)) (w_reqs W x)
  | IMth rt m th => wf_ty Sg_of Phi rt /\ Forall (fun p => wf_ty Sg_of Phi (snd p)) th
      /\ msat th (tele_of m) (mkSig (sg_params (sig_of m)) (sg_ret (sig_of m)))
  | IFun u => Forall (fun x => avail Phi (ITy x)) (fixed_of u)
  end.

Hypothesis tele_needs_ok : forall s k, kind_sym W s = Some k -> has_ctx k = true ->
  forall D1 i D2, tele_of s = D1 ++ i :: D2 ->
  forall Phi, tele_ctx D1 = Some Phi -> need_ok Phi i.

(* A telescope forms when it merges and each item is formed at its prefix. *)
Lemma wf_tele_prefixes : forall D Phi Phi', merge_tele Phi D = Some Phi' ->
  (forall D1 i D2, D = D1 ++ i :: D2 -> forall Phi1, merge_tele Phi D1 = Some Phi1 ->
     item_ok Sg_of Phi1 i) ->
  wf_tele Sg_of Phi D Phi'.
Proof.
  induction D as [|i D IH]; intros Phi Phi' Hm Hok; simpl in Hm.
  - injection Hm as <-. constructor.
  - destruct (merge_item Phi i) as [Phi1|] eqn:Ei; [| discriminate].
    eapply WCons; [exact (Hok [] i D eq_refl Phi eq_refl) | exact Ei |].
    apply IH; [exact Hm |]. intros D1 j D2 E Phi2 H2. apply (Hok (i :: D1) j D2).
    + rewrite E. reflexivity.
    + simpl. rewrite Ei. exact H2.
Qed.

Lemma kind_is_of : forall s k, kind_sym W s = Some k -> kind_is s k = true.
Proof. intros s k H. unfold kind_is. rewrite H. destruct k; reflexivity. Qed.

Lemma g_val_of : forall v d nm t, w_decl W v = Some d -> d_ast d = ADVal nm t None ->
  g_val Sg_of v = Some (tele_of v, val_ty_of v).
Proof. intros v d nm t Hd Ha. simpl. rewrite Hd, Ha. reflexivity. Qed.

Lemma g_fn_of : forall f, kind_sym W f = Some DKFn ->
  g_fn Sg_of f = Some (tele_of f, mkSig (sg_params (sig_of f)) (sg_ret (sig_of f)), None).
Proof. intros f H. simpl. rewrite (kind_is_of _ _ H). reflexivity. Qed.

Lemma g_mth_of : forall m, kind_sym W m = Some DKMeth ->
  g_mth Sg_of m = Some (tele_of m, mkSig (sg_params (sig_of m)) (sg_ret (sig_of m))).
Proof. intros m H. simpl. rewrite (kind_is_of _ _ H). reflexivity. Qed.

(* A telescope laid out as types then declared needs has those types. *)
Lemma tyreqs_laid : forall (tys : list nat) (C : cx) (no : list item),
  (forall n, In n no -> declared_need n) -> tyreqs (map ITy tys ++ map (rkey_of C) no) = tys.
Proof.
  intros tys C no Hd. rewrite tyreqs_app, tyreqs_map_ITy'.
  replace (tyreqs (map (rkey_of C) no)) with (@nil nat); [apply app_nil_r |].
  induction no as [|n no IH]; [reflexivity |].
  pose proof (Hd n (or_introl eq_refl)) as Hn.
  rewrite IH by (intros n' H; apply Hd; right; exact H).
  destruct n; simpl in Hn |- *; [destruct Hn | reflexivity .. ].
Qed.

Lemma tyreqs_tele_of : forall s k, kind_sym W s = Some k -> has_ctx k = true ->
  tyreqs (tele_of s) = w_reqs W s.
Proof.
  intros s k Hk Hc. apply tyreqs_laid. intros n Hn.
  destruct (tele_of_ok s k Hk Hc) as [_ [_ [Hperm _]]].
  exact (need_declared s k Hk Hc n (Permutation_in _ Hperm Hn)).
Qed.

(* An item of a telescope laid out as types then needs (at C's run-time
   keys), formed where A2' says, is formed. *)
Lemma laid_item_ok : forall (tys : list nat) (C : cx) (no : list item) D1 i D2 Phi,
  (forall t, In t tys -> kind_sym W t = Some DKType) ->
  (forall n, In n no -> declared_need n) ->
  map ITy tys ++ map (rkey_of C) no = D1 ++ i :: D2 ->
  need_ok Phi i -> item_ok Sg_of Phi i.
Proof.
  intros tys C no D1 i D2 Phi Hty Hdecl E Hn.
  assert (Hin : In i (map ITy tys ++ map (rkey_of C) no))
    by (rewrite E; apply in_or_app; right; left; reflexivity).
  apply in_app_or in Hin. destruct Hin as [Hin | Hin].
  - apply in_map_iff in Hin. destruct Hin as [r [<- Hr]].
    pose proof (Hty r Hr) as Ht. constructor.
    + exact (kind_is_type _ Ht).
    + intros E0. destruct (kind_is_decl r DKType (kind_is_type _ Ht)) as [d Hd].
      destruct (wo_decl_sound Hwo r Hd) as [Hn0 _]. apply Hn0. exact E0.
  - apply in_map_iff in Hin. destruct Hin as [n [<- Hn0]].
    pose proof (Hdecl n Hn0) as Hd0.
    destruct n as [t | v | f | rt m th | u]; simpl in Hn, Hd0 |- *.
    + destruct Hd0.
    + destruct Hd0 as [d [nm [t [Hd Ha]]]]. eapply OkVal; [exact (g_val_of _ _ _ _ Hd Ha) |].
      assert (Hk : kind_sym W v = Some DKVal) by (rewrite (kind_sym_decl _ _ Hd), Ha; reflexivity).
      rewrite (tyreqs_tele_of v DKVal Hk eq_refl). exact Hn.
    + eapply OkFn; [exact (g_fn_of _ Hd0) |].
      rewrite (tyreqs_tele_of f DKFn Hd0 eq_refl). exact Hn.
    + unfold rkey in Hn |- *. destruct Hn as [Hrt [Hth Hms]].
      eapply OkMth; [exact (g_mth_of _ Hd0) | exact Hrt | exact Hms | exact Hth].
    + destruct Hd0 as [d [nm [ps [a [r [Hd Ha]]]]]].
      assert (Hku : kind_sym W u = Some DKFunctor) by (rewrite (kind_sym_decl _ _ Hd), Ha; reflexivity).
      eapply OkFun; [| exact Hn].
      simpl. rewrite (kind_is_of _ _ Hku), Hd, Ha. reflexivity.
Qed.

Lemma laid_tele_wf : forall (tys : list nat) (C : cx) (no : list item) PhiD,
  (forall t, In t tys -> kind_sym W t = Some DKType) ->
  (forall n, In n no -> declared_need n) ->
  tele_ctx (map ITy tys ++ map (rkey_of C) no) = Some PhiD ->
  (forall D1 i D2, map ITy tys ++ map (rkey_of C) no = D1 ++ i :: D2 ->
     forall Phi, tele_ctx D1 = Some Phi -> need_ok Phi i) ->
  wf_tele Sg_of empty_ctx (map ITy tys ++ map (rkey_of C) no) PhiD.
Proof.
  intros tys C no PhiD Hty Hdecl Htc Hn.
  apply wf_tele_prefixes; [exact Htc |].
  intros D1 i D2 E Phi1 H1. exact (laid_item_ok tys C no D1 i D2 Phi1 Hty Hdecl E (Hn D1 i D2 E Phi1 H1)).
Qed.

(* So every symbol's telescope forms, in Core, at its merged context. *)
Lemma tele_of_wf : forall s k, kind_sym W s = Some k -> has_ctx k = true ->
  exists PhiD, tele_ctx (tele_of s) = Some PhiD /\ wf_tele Sg_of empty_ctx (tele_of s) PhiD.
Proof.
  intros s k Hk Hc. destruct (tele_of_ok s k Hk Hc) as [PhiD [Htc _]].
  destruct (tele_of_ok s k Hk Hc) as [_ [_ [Hperm _]]].
  exists PhiD. split; [exact Htc |].
  apply laid_tele_wf; [exact (reqs_are_types s k Hk) | | exact Htc | exact (tele_needs_ok s k Hk Hc)].
  intros n Hn. exact (need_declared s k Hk Hc n (Permutation_in _ Hperm Hn)).
Qed.

(* Every type meaning of s's context is formed in its merged context: by
   A2 the meanings are its requirements read through σ, and every binding
   of a formed telescope's σ is formed (MergeWf: the unifier binds only
   σ-images of the receivers and θs A2' forms). *)
Lemma Sg_of_tags_typeonly : tags_typeonly Sg_of.
Proof.
  intros n D tau0 H. simpl in H. unfold g_tag_of in H.
  destruct (is_scalar n); [injection H as <- _; constructor |].
  destruct (kind_is n DKUnit); [injection H as <- _; constructor |].
  destruct (kind_is n DKTag); [| discriminate]. injection H as <- _.
  rewrite Forall_map. apply Forall_forall. intros x _. reflexivity.
Qed.

Lemma meanings_ok : forall s k, kind_sym W s = Some k -> has_ctx k = true ->
  forall PhiD, tele_ctx (tele_of s) = Some PhiD -> meanings_wf PhiD (c_ty (ctx_of s)).
Proof.
  intros s k Hk Hc PhiD Htc t x Hx.
  destruct (tele_of_wf s k Hk Hc) as [PhiD' [Htc' Hw]].
  rewrite Htc in Htc'. injection Htc' as <-.
  destruct (tele_of_ok s k Hk Hc) as [PhiD' [Htc' [_ [Hty [Hcan [Hmean Hkeys]]]]]].
  rewrite Htc in Htc'. injection Htc' as <-.
  pose proof (Hkeys t x Hx) as Hr.
  rewrite (Hmean t Hr) in Hx. injection Hx as <-.
  rewrite <- (Hcan (TAbs t)).
  apply (tele_canon_wf Sg_of Sg_of_tags_typeonly (tele_of s) PhiD Hw).
  rewrite Hty. exact Hr.
Qed.

(* Formation is kept by adding an item (σ unchanged). *)
Lemma avail_add : forall i Phi j, avail Phi j -> avail (add_item i Phi) j.
Proof. intros i Phi j H. unfold avail, avail_b in *. simpl. rewrite H. apply orb_true_r. Qed.

Lemma sat_add : forall i Phi th D, sat Sg_of Phi th D -> sat Sg_of (add_item i Phi) th D.
Proof.
  intros i Phi th D [PhiD [Htc [Hdom [Hall Hmr]]]]. exists PhiD. split; [exact Htc |].
  split; [exact Hdom |]. split; [| exact Hmr].
  eapply Forall_impl; [| exact Hall]. intros j [Hj | [Hav Hco]]; [left; exact Hj |].
  right. split; [exact (avail_add _ _ _ Hav) | exact Hco].
Qed.

Lemma wf_ty_add : forall i Phi tau, wf_ty Sg_of Phi tau -> wf_ty Sg_of (add_item i Phi) tau.
Proof.
  intros i Phi tau H. induction H using wf_ty_ind'.
  - constructor.
  - apply WfNeed. exact (avail_add _ _ _ H).
  - eapply WfTag; [exact H | exact (sat_add _ _ _ _ H0) | exact H2].
  - apply WfUnion; [exact H0 | exact H1 | exact H2].
  - apply WfRec; [exact H | exact H1].
Qed.

Lemma sig_wf : forall s k this Phi, kind_sym W s = Some k ->
  SigOf W s (c_ty (ctx_of s)) this (sig_of s) ->
  meanings_wf Phi (c_ty (ctx_of s)) -> (forall x, this = Some x -> wf_ty Sg_of Phi x) ->
  Forall (wf_ty Sg_of Phi) (sg_params (sig_of s)) /\ wf_ty Sg_of Phi (sg_ret (sig_of s)).
Proof.
  intros s k this Phi Hk [d [rcv [n [ps [rt [body [Hd [_ [Hps Hrt]]]]]]]]] Hm Ht.
  pose proof (decl_loaded s d Hd) as Hl. split.
  - clear Hrt. induction Hps as [|pt t ps ts Hy _ IH]; constructor; [| exact IH].
    exact (tyof_wf _ _ _ _ _ Hy Phi Hl Hm Ht).
  - destruct rt as [r|]; [exact (tyof_wf _ _ _ _ _ Hrt Phi Hl Hm Ht) | rewrite Hrt; constructor].
Qed.

Lemma Sg_of_val_wf : forall v D tauv, g_val Sg_of v = Some (D, tauv) ->
  exists PhiD, wf_tele Sg_of empty_ctx D PhiD /\ wf_ty Sg_of PhiD tauv.
Proof.
  intros v D tauv H. simpl in H. destruct (w_decl W v) as [d|] eqn:Hd; [| discriminate].
  destruct (d_ast d) as [| | | | | n t [i|] | | |] eqn:Ha; try discriminate.
  injection H as <- <-.
  assert (Hk : kind_sym W v = Some DKVal) by (rewrite (kind_sym_decl _ _ Hd), Ha; reflexivity).
  destruct (tele_of_wf v DKVal Hk eq_refl) as [PhiD [Htc Hw]].
  exists PhiD. split; [exact Hw |].
  apply (tyof_wf _ _ _ _ _ (val_ty_of_ok v d n t Hd Ha) PhiD (decl_loaded v d Hd)).
  - exact (meanings_ok v DKVal Hk eq_refl PhiD Htc).
  - intros x E. discriminate E.
Qed.

Lemma Sg_of_fn_wf : forall f D S bd, g_fn Sg_of f = Some (D, S, bd) ->
  exists PhiD, wf_tele Sg_of empty_ctx D PhiD
  /\ Forall (wf_ty Sg_of PhiD) (fs_params S)
  /\ wf_ty Sg_of PhiD (fs_ret S)
  /\ (forall body, bd = Some body -> has_ty Sg_of PhiD (body_env S) body (fs_ret S)).
Proof.
  intros f D S bd H. simpl in H. destruct (kind_is f DKFn) eqn:Ek; [| discriminate].
  injection H as <- <- <-. pose proof (kind_is_sym _ _ Ek) as Hk.
  destruct (tele_of_wf f DKFn Hk eq_refl) as [PhiD [Htc Hw]].
  pose proof (sig_of_ok f (or_introl Ek)) as Hsig.
  assert (Hm : kind_is f DKMeth = false) by (unfold kind_is; rewrite Hk; reflexivity).
  rewrite Hm in Hsig.
  destruct (sig_wf f DKFn None PhiD Hk Hsig (meanings_ok f DKFn Hk eq_refl PhiD Htc)
              (fun x E => ltac:(discriminate E))) as [Hps Hret].
  exists PhiD. split; [exact Hw |]. split; [exact Hps |]. split; [exact Hret |].
  intros body E. discriminate E.
Qed.

Lemma Sg_of_mth_wf : forall m D S, g_mth Sg_of m = Some (D, S) ->
  exists PhiD, wf_tele Sg_of empty_ctx D PhiD
  /\ Forall (wf_ty Sg_of (add_item (ITy this_sym) PhiD)) (fs_params S)
  /\ wf_ty Sg_of (add_item (ITy this_sym) PhiD) (fs_ret S).
Proof.
  intros m D S H. simpl in H. destruct (kind_is m DKMeth) eqn:Ek; [| discriminate].
  injection H as <- <-. pose proof (kind_is_sym _ _ Ek) as Hk.
  destruct (tele_of_wf m DKMeth Hk eq_refl) as [PhiD [Htc Hw]].
  pose proof (sig_of_ok m (or_intror Ek)) as Hsig. rewrite Ek in Hsig.
  assert (Hmw : meanings_wf (add_item (ITy this_sym) PhiD) (c_ty (ctx_of m))).
  { intros t x Hx. apply wf_ty_add. exact (meanings_ok m DKMeth Hk eq_refl PhiD Htc t x Hx). }
  destruct (sig_wf m DKMeth (Some (TAbs this_sym)) _ Hk Hsig Hmw) as [Hps Hret].
  { intros x E. injection E as <-. apply WfNeed. apply avail_ty_of_in. left. reflexivity. }
  exists PhiD. split; [exact Hw |]. split; [exact Hps | exact Hret].
Qed.

(* ---- Abstract functors: wf_gsig's last clause. ------------------------ *)

(* BRIDGE-A2' for an abstract functor u: its argument telescope merges,
   each need formed at its prefix as for a symbol's (need_ok); X(F) are
   among its inputs and read by its result (Core's form of D84 rule 1:
   X(φ) ⊆ M(φ)); and its result forms over its arguments, every item
   fresh.  (Track A: from FunCtx, Fixed and ProvideResult.) *)
Hypothesis fun_decl_ok : forall u d n ps a r, w_decl W u = Some d -> d_ast d = ADFunctor n ps a r None ->
  exists PhiU, tele_ctx (fun_args_of u) = Some PhiU
  /\ (forall D1 i D2, fun_args_of u = D1 ++ i :: D2 ->
        forall Phi, tele_ctx D1 = Some Phi -> need_ok Phi i)
  /\ incl (fixed_of u) (tyreqs (cA PhiU))
  /\ incl (fixed_of u)
          (flat_map (fun i => dep_tys Sg_of i ++
             match i with
             | IMth rt _ th => ty_frees rt ++ flat_map (fun p => ty_frees (snd p)) th
             | _ => [] end) (fun_res_of u))
  /\ wf_tele Sg_of PhiU (fun_res_of u) (mkCtx (cA PhiU ++ fun_res_of u) (cS PhiU)).

Lemma Sg_of_fun_wf : forall u Du Dr ob, g_fun Sg_of u = Some (Du, Dr, ob) ->
  exists PhiD, wf_tele Sg_of empty_ctx Du PhiD
  /\ (ob = None ->
        g_cap Sg_of u = ([], [])
        /\ incl (g_fixed Sg_of u) (tyreqs (cA PhiD))
        /\ incl (g_fixed Sg_of u)
                (flat_map (fun i => dep_tys Sg_of i ++
                   match i with
                   | IMth rt _ th => ty_frees rt ++ flat_map (fun p => ty_frees (snd p)) th
                   | _ => [] end) Dr)
        /\ wf_tele Sg_of PhiD Dr (mkCtx (cA PhiD ++ Dr) (cS PhiD)))
  /\ (forall bs, ob = Some bs ->
        exists PhiC, wf_tele Sg_of empty_ctx (fst (g_cap Sg_of u)) PhiC
        /\ pins_ok (snd (g_cap Sg_of u)) PhiC
        /\ wf_tele Sg_of (fun_ctx PhiD PhiC (snd (g_cap Sg_of u))) Dr
             (mkCtx (cA (fun_ctx PhiD PhiC (snd (g_cap Sg_of u))) ++ Dr)
                    (cS (fun_ctx PhiD PhiC (snd (g_cap Sg_of u)))))
        /\ exists PhiB, binds_ty Sg_of (fun_ctx PhiD PhiC (snd (g_cap Sg_of u))) bs PhiB
           /\ Forall (fun i => In i (cA PhiB)) Dr).
Proof.
  intros u Du Dr ob H. simpl in H. destruct (kind_is u DKFunctor); [| discriminate].
  destruct (w_decl W u) as [d|] eqn:Hd; [| discriminate].
  destruct (d_ast d) as [| | | | | | | | n ps a r [bd|]] eqn:Ha; try discriminate.
  injection H as <- <- <-.
  destruct (fun_decl_ok u d n ps a r Hd Ha) as [PhiU [Htc [Hn [Hx [Hm Hr]]]]].
  destruct (fun_ctx_of_ok u d n ps a r Hd Ha) as [Hfc [Hperm Hty]].
  exists PhiU. split; [| split].
  - apply laid_tele_wf; [exact Hty | | exact Htc | exact Hn].
    intros k Hk. apply Permutation_in with (l' := fun_needs_of u) in Hk; [| exact Hperm].
    destruct Hfc as [d' [ps' [a' [r' [b' [es [tm [tm' [sg [imp [Hd' [Hfp [Hes [_ [_ [_ [_ Hneeds]]]]]]]]]]]]]]]]].
    rewrite Hd in Hd'. injection Hd' as <-. rewrite Ha in Hfp. injection Hfp as <- <- <- <-.
    exact (needs_declared _ _ _ _ _ _ (decl_loaded u d Hd) Hes Hneeds k Hk).
  - intros _. split; [reflexivity |]. split; [exact Hx |]. split; [exact Hm | exact Hr].
  - intros bs E. discriminate E.
Qed.

(* Σ's declarations are well-formed (xwf_decls), given A2'. *)
Lemma Sg_of_wf : wf_gsig Sg_of.
Proof.
  constructor.
  - exact tag_decl_wf.
  - exact Sg_of_val_wf.
  - exact Sg_of_fn_wf.
  - exact Sg_of_mth_wf.
  - exact Sg_of_fun_wf.
Qed.

(* ---- Functor bodies (xwf_fb). ------------------------------------------ *)

(* BRIDGE-A5': what fb_of gives for a body h that LinkBody W P u gg h
   made: its linked arguments Dh and its captured inputs Dc, each laid out
   as types, then needs of gg's own context (FunCtx) at their run-time
   keys.  (The elaborator's link_body splits gg's needs by whether u takes
   them; its pins are the equations it reads X(F) through.) *)
Hypothesis fb_of_ok : forall h Dh Dc pins, fb_of h = Some (Dh, Dc, pins) ->
  exists u gg Cg nsg tl nl tc nc,
    LinkBody W P u gg h /\ FunCtx W gg Cg nsg
    /\ Dh = map ITy tl ++ map (rkey_of Cg) nl /\ Dc = map ITy tc ++ map (rkey_of Cg) nc
    /\ incl nl nsg /\ incl nc nsg
    /\ (forall t, In t (tl ++ tc) -> kind_sym W t = Some DKType).

(* BRIDGE-A2' for bodies: Dh and Dc merge, each need formed at its prefix
   (need_ok); the pins are on captured types; and the result F promises
   forms over the body's context. *)
Hypothesis fb_forms : forall h Dh Dc pins, fb_of h = Some (Dh, Dc, pins) ->
  exists PhiD PhiC, tele_ctx Dh = Some PhiD /\ tele_ctx Dc = Some PhiC
  /\ (forall D1 i D2, Dh = D1 ++ i :: D2 -> forall Phi, tele_ctx D1 = Some Phi -> need_ok Phi i)
  /\ (forall D1 i D2, Dc = D1 ++ i :: D2 -> forall Phi, tele_ctx D1 = Some Phi -> need_ok Phi i)
  /\ pins_ok pins PhiC
  /\ (forall Dr bs, funbody_of P h = Some (Dr, bs) ->
        wf_tele Sg_of (fun_ctx PhiD PhiC pins) Dr
          (mkCtx (cA (fun_ctx PhiD PhiC pins) ++ Dr) (cS (fun_ctx PhiD PhiC pins)))).

(* Step 3 at binds (BindItems ⇒ xbinds_ty): h's binds, checked once in
   its own context, leave F's result in force. *)
Hypothesis fb_binds : forall argv rd h Dh Dc pins PhiD PhiC Dr bs,
  fb_of h = Some (Dh, Dc, pins) -> tele_ctx Dh = Some PhiD -> tele_ctx Dc = Some PhiC ->
  funbody_of P h = Some (Dr, bs) ->
  exists PhiB, xbinds_ty Sg_of Rp_of (program_of P argv rd) Fb_of (fun_ctx PhiD PhiC pins) bs PhiB
  /\ Forall (fun i => In i (cA PhiB)) Dr.

Lemma Sg_of_fb : forall argv rd h Dh Dc pins Dr bs,
  Fb_of h = Some (Dh, Dc, pins) -> xp_fun (program_of P argv rd) h = Some (Dr, bs) ->
  exists PhiD PhiC, wf_tele Sg_of empty_ctx Dh PhiD
  /\ wf_tele Sg_of empty_ctx Dc PhiC
  /\ pins_ok pins PhiC
  /\ wf_tele Sg_of (fun_ctx PhiD PhiC pins) Dr
       (mkCtx (cA (fun_ctx PhiD PhiC pins) ++ Dr) (cS (fun_ctx PhiD PhiC pins)))
  /\ exists PhiB, xbinds_ty Sg_of Rp_of (program_of P argv rd) Fb_of (fun_ctx PhiD PhiC pins) bs PhiB
     /\ Forall (fun i => In i (cA PhiB)) Dr.
Proof.
  intros argv rd h Dh Dc pins Dr bs Hfb Hfun. unfold Fb_of in Hfb.
  change (funbody_of P h = Some (Dr, bs)) in Hfun.
  destruct (fb_of_ok h Dh Dc pins Hfb)
    as [u [gg [Cg [nsg [tl [nl [tc [nc [Hlb [Hfc [EDh [EDc [Hnl [Hnc Hty]]]]]]]]]]]]]].
  destruct (fb_forms h Dh Dc pins Hfb) as [PhiD [PhiC [HtD [HtC [HnD [HnC [Hpins Hres]]]]]]].
  assert (Hdecl : forall n, In n nsg -> declared_need n).
  { destruct Hfc as [d [ps [a [r [b' [es [tm [tm' [sg [imp [Hd [_ [Hes [_ [_ [_ [_ Hneeds]]]]]]]]]]]]]]]]].
    exact (needs_declared _ _ _ _ _ _ (decl_loaded gg d Hd) Hes Hneeds). }
  exists PhiD, PhiC. split; [| split; [| split; [exact Hpins | split]]].
  - subst Dh. apply laid_tele_wf; [| | exact HtD | exact HnD].
    + intros t Ht. apply Hty. apply in_or_app. left. exact Ht.
    + intros n Hn. exact (Hdecl n (Hnl n Hn)).
  - subst Dc. apply laid_tele_wf; [| | exact HtC | exact HnC].
    + intros t Ht. apply Hty. apply in_or_app. right. exact Ht.
    + intros n Hn. exact (Hdecl n (Hnc n Hn)).
  - exact (Hres Dr bs Hfun).
  - exact (fb_binds argv rd h Dh Dc pins PhiD PhiC Dr bs Hfb HtD HtC Hfun).
Qed.

(* ---- Bool (xwf_bool). ----------------------------------------------- *)

(* BRIDGE-HBOOL (Track A's Hbool): lib/bool.moss declares `tag Bool(True |
   False)` over the units True and False, which the relation's LangSym does
   not fix by itself. *)
Hypothesis Hbool :
  kind_is (xo_bool P) DKTag = true /\ w_reqs W (xo_bool P) = []
  /\ pay_of (xo_bool P) = TUnion [TNom (xo_true P) []; TNom (xo_false P) []]
  /\ kind_is (xo_true P) DKUnit = true /\ kind_is (xo_false P) DKUnit = true.

(* A declared symbol without requirements has no representation. *)
Lemma Rp_of_noreqs : forall n k, kind_is n k = true -> w_reqs W n = [] -> Rp_of n = None.
Proof.
  intros n k Hk Hr. pose proof (scalar_not_declared _ _ Hk) as Hs. unfold is_scalar in Hs.
  apply orb_false_iff in Hs as [Hs H4]. apply orb_false_iff in Hs as [Hs H3].
  apply orb_false_iff in Hs as [H1 H2].
  unfold Rp_of. rewrite H1, H2, H3, H4. unfold is_some_eq.
  destruct wasm_ref as [r|].
  - destruct (Nat.eqb r n) eqn:E.
    + apply Nat.eqb_eq in E. subst r. destruct (wasm_ref_ok n eq_refl) as [_ Hne]. contradiction.
    + destruct wasm_arr as [a|]; [| reflexivity].
      destruct (Nat.eqb a n) eqn:E2; [| reflexivity].
      apply Nat.eqb_eq in E2. subst a. destruct (wasm_arr_ok n eq_refl) as [_ Hne]. contradiction.
  - destruct wasm_arr as [a|]; [| reflexivity].
    destruct (Nat.eqb a n) eqn:E2; [| reflexivity].
    apply Nat.eqb_eq in E2. subst a. destruct (wasm_arr_ok n eq_refl) as [_ Hne]. contradiction.
Qed.

Lemma unit_noreqs : forall n, kind_is n DKUnit = true -> w_reqs W n = [].
Proof. intros n H. apply (wo_reqs_none Hwo n). right. exact (kind_is_sym _ _ H). Qed.

Lemma g_tag_of_unit : forall n, kind_is n DKUnit = true -> g_tag_of n = Some ([], TUnit).
Proof. intros n H. unfold g_tag_of. rewrite (scalar_not_declared _ _ H), H. reflexivity. Qed.

Lemma Sg_of_bool : forall argv rd, bool_ok Sg_of Rp_of (program_of P argv rd).
Proof.
  intros argv rd. destruct Hbool as [Hb [Hbr [Hpay [Ht Hf]]]]. unfold bool_ok. cbn [xp_bool xp_true xp_false program_of].
  split; [exact (Rp_of_noreqs _ _ Hb Hbr) |].
  split; [exact (Rp_of_noreqs _ _ Ht (unit_noreqs _ Ht)) |].
  split; [exact (Rp_of_noreqs _ _ Hf (unit_noreqs _ Hf)) |].
  split; [| split; exists []; simpl; apply g_tag_of_unit; assumption].
  exists []. simpl. unfold g_tag_of. rewrite (scalar_not_declared _ _ Hb).
  assert (Hu : kind_is (xo_bool P) DKUnit = false).
  { unfold kind_is in *. destruct (kind_sym W (xo_bool P)) as [[]|]; try discriminate; reflexivity. }
  rewrite Hu, Hb, (Rp_of_noreqs _ _ Hb Hbr), Hbr, Hpay. reflexivity.
Qed.

End FromWorld.

(* Where step 2 stands.  Proved here from WorldOf, BRIDGE-A1…A6 and
   A2': Σ's declarations are well-formed (Sg_of_wf: xwf_decls).  Tags:
   every type the relation reads (TyOf) is formed in Σ when the meanings
   it reads are (tyof_wf), so every tag declaration is (tag_decl_wf).
   Vals, fns, methods: each symbol's telescope forms (tele_of_wf), item by
   item at its prefix (wf_tele_prefixes) — the needs are declared as such
   (need_declared, from the entries CtxOf walks), and formed where A2'
   says; signatures by tyof_wf in it, whose meanings are formed
   (meanings_ok: A2 makes them the requirements read through σ, and
   MergeWf.tele_canon_wf forms those for any formed telescope).  Abstract
   functors: their argument
   telescopes (from FunCtx) the same way, the rest from fun_decl_ok.
   xwf_bool is Sg_of_bool (BRIDGE-HBOOL), xwf_rec Sg_of_rec.

   A2' is not what the elaborator checks: a val or fn need whose own needs
   or type requirements are not in force where it is assumed is accepted
   and runs (the report of this branch; scratch probes).  Core's OkVal/OkFn
   now ask only the type part (all their proofs read), so A2' asks a val
   or fn need's needs nothing; its type requirements remain (dep_tys).

   With brackets that do not outlive their item (Audit note 71), the
   IFun clause of need_ok fails for an accepted program whose functor's
   fixed input is in force only through a bracket (scratch probe f1:
   `assume X { context C = F; }`, `context O = C[X=Char];` for F fixed in
   X); open, with the designer.

   Remaining:
   - discharge A2' (a val or fn need's type part: open, see dep_tys), A4's
     FunCtx reading and fun_decl_ok, from elaborate_sound (Track A).
   - xwf_fb: Sg_of_fb proves it from BRIDGE-A5' (fb_of as LinkBody's
     pieces, laid out from gg's FunCtx), fb_forms (A2' for Dh, Dc, pins
     and the result) and fb_binds, which is step 3's lemma for BindItems
     (h's binds are elaborated by the same rules as a body's): Dh and Dc
     form item by item as a symbol's telescope does.
   - xwf_bodies: step 3, the expression bridge. *)
