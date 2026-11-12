(* Applying a platform family (T15): `bind F;` of an abstract F whose
   closure is a platform family (Platform.Family), possibly through
   copies, is a safe binding step, PublicBodySlice's obligation
   PlatformApplyOk (through PublicCopyApply's ChainPlatformOk).

   The run is computed here: the application selects F's inputs, runs the
   family's code, which only installs primitive closures (BPrimitive), and
   files position i of F's declared result under the caller's run-time key
   for it, read from the family's item at the chain's source for i
   (Audit.result_selection; a later position wins). The caller's context
   after the application is then realized: its values, fns and functors
   are untouched, and its new methods are the family's primitives.

   Two premises remain, each named and stated once:
   - ChainPrimsOk (primitive behaviour, T11/T17): a primitive the family
     installs is callable at its AtKey signature in the application's
     environment. PublicEntry discharges it for the reference host
     (platform_prims_public, through chain_prims_of_ground).
   - ChainForwardSigs (static, T15/T16): at the caller's last position for
     a key, the caller's method and the family's item there have agreeing
     signatures. The keys are the caller's by construction. PublicChainFwd
     proves the empty chain and names the copy case ChainForwardCopySigs.

   The same holds of a family application of a platform family
   (PublicFamilyStep's FamilyPlatformApplyOk, `family_platform_ok`), from
   FamilyKeysTyped (a formation fact giving the family-application form of
   the result agreement, position by position: family_items_pos, and item
   by item: family_items_agree) and PrimsGroundOk (the primitives callable
   wherever the fixed machine inputs read as the machine types). *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require AuditFacts.
From NM Require Import LegacyBodyJudgments.
From NM Require Import AuditBase PublicPrimitiveFamily.
From NM Require PublicContextFacts.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicBodySlice PublicTagCheck PublicCallRealize.
From NM Require PublicFormation PublicCopyApply PublicFamilyStep PublicFamilyAgree PublicChainAgree PublicCallerKeys EagerCopy.
From NM Require PublicMerge.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

(* ---- a function context records its fixed readings as the symbols ----------- *)

Section FixedSelf.
Variable W : A.world.

(* An assumed functor's fixed inputs are read in a table where each
   symbol means itself (IdForm), so as the symbols themselves. *)
Definition FunSelf (e : A.entry) : Prop :=
  match e with A.NFun _ fx => forall x t, In (x, t) fx -> t = TAbs x | _ => True end.

Lemma entries_self :
  (forall p ob tm sp es tm', A.ItemEntries W p ob tm sp es tm' ->
     IdForm tm -> Scoped tm (tm_gens ob) -> Forall FunSelf es) /\
  (forall p ob tm sps es tm', A.ItemsEntries W p ob tm sps es tm' ->
     IdForm tm -> Scoped tm (tm_gens ob) -> Forall FunSelf es).
Proof.
  apply (ies_mut W
    (fun p ob tm sp es tm' _ => IdForm tm -> Scoped tm (tm_gens ob) -> Forall FunSelf es)
    (fun p ob tm sps es tm' _ => IdForm tm -> Scoped tm (tm_gens ob) -> Forall FunSelf es)).
  - intros. repeat constructor.
  - intros. repeat constructor.
  - intros. repeat constructor.
  - intros p ob tm pa u d n ps args res fx R D Ast Fixed FR Id Ob. constructor; [|constructor].
    cbn. intros x t I. destruct (proj1 (FR x t) I) as [_ L]. exact (idform_nlook _ _ _ Id L).
  - intros. repeat constructor.
  - intros p ob tm a pa c d n its bs es tm' R D Ast B Rest IH Id Ob.
    pose proof (brackets_scoped W p ob tm c a bs Id Ob B) as Bs.
    apply IH; [exact Id|]. intros g Ig. apply tm_gens_app in Ig as [Ig|Ig]; [exact (Bs g Ig)|exact (Ob g Ig)].
  - intros. constructor.
  - intros p ob tm sp r es1 tm1 es2 tm2 H1 IH1 H2 IH2 Id Ob.
    destruct (proj1 (entries_formed W) _ _ _ _ _ _ H1 Id Ob) as [Id1 [Sub1 _]].
    apply Forall_app. split; [exact (IH1 Id Ob)|exact (IH2 Id1 (fun g Ig => Sub1 g (Ob g Ig)))].
  - intros p ob tm l r es1 tm1 tl tr rest es2 tm2 H1 IH1 Tl Tr H2 IH2 Id Ob.
    destruct (proj2 (entries_formed W) _ _ _ _ _ _ H1 Id Ob) as [Id1 [Sub1 _]].
    apply Forall_app. split; [exact (IH1 Id Ob)|constructor; [exact Logic.I|exact (IH2 Id1 (fun g Ig => Sub1 g (Ob g Ig)))]].
Qed.

Lemma root_self : forall q es0 tm its es tm', A.RootItems W q es0 tm its es tm' -> IdForm tm -> Forall FunSelf es.
Proof.
  intros q es0 tm its es tm' Root. induction Root as
    [es0 tm|es0 tm p a rest u du n ps args res xs fx es tm' R D Ast Fresh Nd Xs FR Rest IH|es0 tm sp rest es tm' Skip Rest IH];
    intros Id; [constructor| |exact (IH Id)].
  destruct (add_tys_formed xs tm Id) as [IdA _].
  apply Forall_app. split; [apply Forall_forall; intros e Ie; apply in_map_iff in Ie as [x [<- _]]; exact Logic.I|].
  constructor; [|exact (IH IdA)]. cbn. intros x t I. destruct (proj1 (FR x t) I) as [_ L].
  exact (idform_nlook _ _ _ IdA L).
Qed.

(* So a function context's functor provision reads each fixed input as
   the symbol's canonical self. *)
Theorem ctx_fun_self : forall f C ns, A.CtxOf W f C ns -> forall u fx, In (IFun u, A.ProvidedFunctor fx) (A.c_provisions C) ->
  forall x t, In (x, t) fx -> t = app_subst (A.c_sub C) (TAbs x).
Proof.
  intros f C ns Ctx. destruct Ctx as (d & es & tm & er & tmr & σ & classes & D & IE & Root & MGM & _ & _ & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT _].
  pose proof (proj2 entries_self _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as SelfE.
  assert (SelfR : Forall FunSelf er).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI]; [constructor|exact (root_self _ _ _ _ _ _ RI IdT)]. }
  intros u fx I x t It. cbn in I. apply in_flat_map in I as [e [Ie I]].
  assert (Se : FunSelf e).
  { apply in_app_or in Ie as [Ie|Ie]; [exact (proj1 (Forall_forall _ _) SelfE e Ie)|exact (proj1 (Forall_forall _ _) SelfR e Ie)]. }
  destruct e as [t0|v t0 a|f' g a|r m g a|u' fx'|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; try discriminate.
  injection E as <- <-. unfold A.fixed_subst in It. apply in_map_iff in It as [[y t0] [E It]]. cbn in E.
  injection E as -> <-. cbn in Se. rewrite (Se x t0 It). reflexivity.
Qed.
End FixedSelf.

Module PlatformApply (Prims : PRIMITIVES) (I0 : INTERP Prims).
(* The slice as the family and copy steps (PublicFamilyStep,
   PublicCopyApply) instantiate it. *)
Module FS := PublicFamilyStep.FamilyStep Prims I0.

(* ---- the family's code runs its installs ------------------------------------- *)

Definition IsPrim := PublicPrimitiveFamily.IsPrim.

Fixpoint installs (d : environment) (bs : list binding) : environment :=
  match bs with
  | [] => d
  | BPrimitive k op :: r => installs (install_provision k (primitive_closure d k op) d) r
  | _ :: r => installs d r
  end.

(* The last operation installed under k. *)
Fixpoint last_prim (k : item) (bs : list binding) : option op :=
  match bs with
  | [] => None
  | BPrimitive k1 op :: r =>
      match last_prim k r with Some o => Some o | None => if item_eqb k k1 then Some op else None end
  | _ :: r => last_prim k r
  end.

Lemma installs_meanings : forall bs d, type_meanings (installs d bs) = type_meanings d.
Proof.
  induction bs as [|b r IH]; intros d; [reflexivity|].
  destruct b; cbn [installs]; rewrite ?IH; reflexivity.
Qed.

Lemma installs_lookup : forall bs d k,
  lookup_provision k (installs d bs) =
  match last_prim k bs with Some op => Some (primitive_closure d k op) | None => lookup_provision k d end.
Proof.
  induction bs as [|b r IH]; intros d k; [reflexivity|].
  destruct b as [t tau|v e|f f' ty sm|tau m th f' ty sm|u h sm smc|u u' sm|u th outs sm|k1 op1];
    cbn [installs last_prim]; try exact (IH d k).
  rewrite IH. destruct (last_prim k r) as [o|]; [reflexivity|].
  rewrite FS.CopyA.S.lookup_install. destruct (item_eqb k k1) eqn:E; [|reflexivity].
  apply item_eqb_spec in E. subst k1. reflexivity.
Qed.

Lemma last_prim_in : forall k bs op, last_prim k bs = Some op -> In (BPrimitive k op) bs.
Proof.
  intros k. induction bs as [|b r IH]; intros op L; [discriminate|].
  destruct b as [t tau|v e|f f' ty sm|tau m th f' ty sm|u h sm smc|u u' sm|u th outs sm|k1 op1];
    cbn [last_prim] in L; try (right; exact (IH op L)).
  destruct (last_prim k r) as [o|] eqn:Lr; [injection L as <-; right; apply IH; reflexivity|].
  destruct (item_eqb k k1) eqn:E; [|discriminate]. apply item_eqb_spec in E. subst k1.
  injection L as <-. left. reflexivity.
Qed.

Lemma last_prim_some : forall k bs op, In (BPrimitive k op) bs -> exists op', last_prim k bs = Some op'.
Proof.
  intros k. induction bs as [|b r IH]; intros op I; [contradiction|].
  destruct I as [->|I].
  - cbn [last_prim]. destruct (last_prim k r) as [o|]; [eauto|].
    rewrite (proj2 (item_eqb_spec k k) eq_refl). eauto.
  - destruct (IH op I) as [o L].
    destruct b as [t tau|v e|f f' ty sm|tau m th f' ty sm|u h sm smc|u u' sm|u th outs sm|k1 op1];
      cbn [last_prim]; rewrite ?L; eauto.
Qed.

Section Run.
Variables (P : program_view).

Lemma installs_run : forall j bs d g st, Forall IsPrim bs ->
  FS.CopyA.S.AR.O.I.eval_bindings j P d g st bs = inl (installs d bs, g, st) \/
  FS.CopyA.S.AR.O.I.eval_bindings j P d g st bs = inr (FS.CopyA.S.AR.O.I.Evaluation OutOfFuel g st).
Proof.
  induction j as [|j IH]; intros bs d g st Prim; [right; reflexivity|].
  destruct bs as [|b r]; [left; reflexivity|].
  inversion Prim as [|b' r' [k [op ->]] Rest]; subst.
  exact (IH r _ g st Rest).
Qed.
End Run.

(* ---- small facts ---------------------------------------------------------------- *)

Lemma ilook_app : forall X k (l1 l2 : list (item * X)),
  A.ilook k (l1 ++ l2) = match A.ilook k l1 with Some p => Some p | None => A.ilook k l2 end.
Proof.
  intros X k l1 l2. induction l1 as [|[k' p] r IH]; [reflexivity|]. cbn.
  destruct (item_eqb k k'); [reflexivity|exact IH].
Qed.

Lemma sm_find_in : forall k sm s, sm_find k sm = Some s -> In (k, s) sm.
Proof.
  intros k sm. induction sm as [|[r s'] rest IH]; intros s F; [discriminate|]. cbn in F.
  destruct (item_eqb k r) eqn:E; [apply item_eqb_spec in E; subst r; injection F as <-; left; reflexivity|].
  right. exact (IH s F).
Qed.

Lemma sm_find_some : forall k sm s, In (k, s) sm -> exists s', sm_find k sm = Some s'.
Proof.
  intros k sm. induction sm as [|[r s'] rest IH]; intros s I; [contradiction|]. cbn.
  destruct (item_eqb k r) eqn:E; [eauto|]. destruct I as [Eq|I].
  - injection Eq as <- _. rewrite (proj2 (item_eqb_spec r r) eq_refl) in E. discriminate.
  - exact (IH s I).
Qed.

Lemma sm_find_none : forall k sm, (forall s, ~ In (k, s) sm) -> sm_find k sm = None.
Proof.
  intros k sm Out. destruct (sm_find k sm) as [s|] eqn:F; [|reflexivity].
  exfalso. exact (Out s (sm_find_in _ _ _ F)).
Qed.

Section Platform.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Let P := program_of Prog argv read.
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Notation Pre := (FS.CopyA.S.Pre W Prog argv read state_ok).
Notation CallableOk := (FS.CopyA.S.CallableOk W Prog argv read state_ok).

(* A platform family, its parts named (Platform.Family's body). *)
Definition FamilyData (u code : usym) (inputs : A.cx) (needs : list item) (outputs : A.cx)
    (keys : list item) (bindings : list binding) : Prop :=
  A.FunCtx W u inputs needs /\
  (forall t, ~ A.FunOutputs W u t) /\
  A.ProvideResult W u inputs (fun t => TAbs t) outputs keys /\
  A.funbody_of Prog code = Some (FunctorCode (map fst (A.c_ty inputs)) (map fst (A.c_ty inputs)) []
    [] bindings (map (A.rkey_of outputs) keys)) /\
  Forall2 (fun key binding => exists operation,
    binding = BPrimitive (A.rkey_of outputs key) operation /\ Platform.MethodPrimitive W key operation /\
    Platform.AtKey W outputs key operation /\ Platform.MachinesFixed W Prog u inputs outputs key /\
    Platform.HandlesRead W outputs key) keys bindings /\
  Platform.MethodsDistinct W keys /\
  (forall dl ps a r b, A.w_decl W u = Some dl -> A.functor_parts (A.d_ast dl) = Some (ps, a, r, b) -> b = None) /\
  NoDup (map A.provision_id keys).

Lemma family_data : forall u code, Platform.Family W Prog u code ->
  exists inputs needs outputs keys bindings, FamilyData u code inputs needs outputs keys bindings.
Proof. intros u code (i & n & o & k & b & D). exists i, n, o, k, b. exact D. Qed.

Lemma family_data_primitives : forall u code inputs needs outputs keys bindings,
  FamilyData u code inputs needs outputs keys bindings -> Forall IsPrim bindings.
Proof.
  intros u code inputs needs outputs keys bindings (_ & _ & _ & _ & Rows & _).
  induction Rows as [|key binding keys' tail [operation [-> _]] _ IH]; constructor;
    [do 2 eexists; reflexivity|exact IH].
Qed.
Lemma family_primitive_code : forall u code,
  Platform.Family W Prog u code -> PrimitiveFamilyCode Prog code.
Proof.
  intros u code Family.
  destruct (family_data u code Family) as (inputs & needs & outputs & keys & bindings & Data).
  pose proof Data as (_ & _ & _ & Body & _).
  eexists. split; [exact Body|]. cbn [fc_bindings].
  exact (family_data_primitives _ _ _ _ _ _ _ Data).
Qed.

(* The environment a family's code runs in. *)
Definition app_env (d : environment) (code : usym) (th : subst) (d1 : environment) : environment :=
  application_environment P d code th (append_environment d1 (EnvironmentWithAliases [] [] [])).

(* SigAgree and ItemsAgree: PublicFamilyAgree's (the agreement a
   family's run needs, shared by the chain and the family application). *)
Notation SigAgree := PublicFamilyAgree.SigAgree.
Notation ItemsAgree := PublicFamilyAgree.ItemsAgree.

(* The chain's agreement is read at the ground meanings of the state d
   the application runs from: a copy step relates its two results only up
   to the copy site's readings of the fixed inputs (CopyInterpretation), which the
   state grounds as the application's (FixedMeans), not syntactically. *)
Definition SigAgreeG (d : environment) (Ch0 : A.cx) (th : subst) (tc tf : ty) : Prop :=
  incl (A.gens tf) (map fst (A.c_ty Ch0)) /\ ground d tc = ground d (app_subst th tf).

(* PREMISE (static correspondence, T15/T16). Applying F at θ from a state
   d realizing C, where F's closure (in d) is a copy chain down to a
   platform family h0 (u = h0 for the empty chain): the application files
   each position i of F's result under the caller's key ks[i], read from
   the family's item at the chain's source for i (Audit.result_selection).
   At the last position of a caller key (the one the selection keeps, and
   the one the caller's context reads, output_cx), the caller's method
   and the family's item there have the same name and agreeing
   signatures, at the ground meanings of d. The keys are the caller's by
   construction; this is the signature half only. *)
Definition ChainForwardSigs : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code sms rms gfx, lookup_provision (IFun u) d = Some (FunctorClosure code sms FS.CopyA.empty_env rms) ->
  FS.CopyA.S.FixedMeans W u d fx gfx ->
  forall h0 gf0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 None None ->
  forall Ch0 needs outputs keys bindings, FamilyData h0 code Ch0 needs outputs keys bindings ->
  forall i r m g key, nth_error ks i = Some (A.mkey r m) ->
  (forall i', i < i' -> nth_error ks i' <> Some (A.mkey r m)) ->
  A.mlook r m (A.c_mths C') = Some g ->
  In key keys -> nth_error (closure_sources rms (map (A.rkey_of outputs) keys)) i = Some (A.rkey_of outputs key) ->
  exists rf a gf, key = IMth rf m a /\ A.mlook rf m (A.c_mths outputs) = Some gf /\
    Forall2 (SigAgreeG d Ch0 th) (A.sg_ret g :: r :: A.sg_params g) (A.sg_ret gf :: rf :: A.sg_params gf).

(* PREMISE (primitive behaviour, T11/T17). A primitive a platform family
   installs is callable, at its AtKey signature, in the environment the
   family's code runs in at an application of F, F's closure a copy chain
   down to the family's (CopyChain, the root's ClosureOk). The slice knows
   nothing of primitives; PublicEntry discharges it for the reference host
   (platform_prims_public). *)
Definition ChainPrimsOk : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code sms rms gfx, lookup_provision (IFun u) d = Some (FunctorClosure code sms FS.CopyA.empty_env rms) ->
  FS.CopyA.S.FixedMeans W u d fx gfx ->
  forall h0 gf0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 None None ->
  FS.CopyA.S.ClosureOk W Prog argv read state_ok rr H h0 gf0 (FunctorClosure code None FS.CopyA.empty_env None) ->
  forall Ch0 needs outputs keys bindings, FamilyData h0 code Ch0 needs outputs keys bindings ->
  forall e key op rf m a gf, In key keys -> In (BPrimitive (A.rkey_of outputs key) op) bindings ->
  Platform.MethodPrimitive W key op -> Platform.AtKey W outputs key op ->
  key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf ->
  CallableOk rr H (app_env d code th e) (rf :: A.sg_params gf) (A.sg_ret gf)
    (primitive_closure (app_env d code th e) (A.rkey_of outputs key) op).

(* A platform family's code with a primitive is no linked body's code: a
   linked body's binds come from source items, none a primitive
   (platform_not_linked). Needed only to tell a closure's root apart. *)
Definition PlatformNotLinked : Prop :=
  forall h0 code u gg pins fc, Platform.Family W Prog h0 code -> LegacyBodyJudgments.LinkBody W Prog u gg code pins ->
  A.funbody_of Prog code = Some fc -> fc_bindings fc = [].

(* The concrete family contract supplies the generic code shape. *)
Lemma platform_not_linked : PlatformNotLinked.
Proof.
  intros h0 code u gg pins fc Family Linked View.
  eapply primitive_code_not_linked; [apply (family_primitive_code h0 code Family)|exact Linked|exact View].
Qed.

Lemma forall2_in_l : forall X Y (R : X -> Y -> Prop) l l' x, Forall2 R l l' -> In x l -> exists y, In y l' /\ R x y.
Proof.
  intros X Y R l l' x F. induction F as [|a b l l' Rab F IH]; intros I; [contradiction|].
  destruct I as [<-|I]; [exists b; split; [left; reflexivity|exact Rab]|].
  destruct (IH I) as [y [Iy Ry]]. exists y. split; [right; exact Iy|exact Ry].
Qed.

(* The family's code reads a type of its inputs as the caller's θ does. *)
Lemma app_env_ground : forall d code th e (Ch0 Cu : A.cx) (f : nat -> ty) Dr bs srcs,
  A.funbody_of Prog code = Some (FunctorCode (map fst (A.c_ty Ch0)) (map fst (A.c_ty Ch0)) [] Dr bs srcs) ->
  th = map (fun q => (fst q, f (fst q))) (A.c_ty Cu) -> incl (map fst (A.c_ty Ch0)) (map fst (A.c_ty Cu)) ->
  forall t, incl (A.gens t) (map fst (A.c_ty Ch0)) -> ground (app_env d code th e) t = ground d (app_subst th t).
Proof.
  intros d code th e Ch0 Cu f Dr bs srcs FB -> Kin t Sc.
  unfold app_env, application_environment. change (view_functor P code) with (A.funbody_of Prog code). rewrite FB.
  cbn [fc_linked fc_inputs fc_table lookup_subst]. unfold ground. cbn [type_meanings].
  match goal with |- app_subst ?m t = _ => rewrite <- (PublicTypeRead.app_subst_nil (app_subst m t)) end.
  apply subst2_agree. intros x Ix. pose proof (Sc x Ix) as Kx.
  assert (E : existsb (Nat.eqb x) (map fst (A.c_ty Ch0)) = true)
    by (apply existsb_exists; exists x; split; [exact Kx|apply Nat.eqb_refl]).
  cbn [app_subst]. erewrite FS.CopyA.S.lookup_map_nat by exact Kx. cbv beta.
  rewrite E, (FS.CopyA.S.lookup_on_keys f _ x (Kin x Kx)). apply PublicTypeRead.app_subst_nil.
Qed.

(* A copy chain's root reads only inputs its top has. *)
Lemma chain_keys : forall u gfx sms rms h0 gf0 sms0 rms0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 sms0 rms0 ->
  forall Cu nsu Ch0 nsh0, A.FunCtx W u Cu nsu -> A.FunCtx W h0 Ch0 nsh0 ->
  incl (map fst (A.c_ty Ch0)) (map fst (A.c_ty Cu)).
Proof.
  intros u gfx sms rms h0 gf0 sms0 rms0 Ch.
  induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 sms0 rms0 _ IH FCu FCh _ _ _ Sub _ _];
    intros Cu' nsu' Ch0 nsh0 FCu' FC0.
  - destruct (FS.CopyA.S.functx_unique W _ _ _ _ _ FCu' FC0) as [-> _]. intros x I. exact I.
  - destruct (FS.CopyA.S.functx_unique W _ _ _ _ _ FCu FCu') as [<- _].
    intros x I. exact (FS.CopyA.keys_sub _ _ Sub x (IH Ch nsh Ch0 nsh0 FCh FC0 x I)).
Qed.

(* Down a copy chain the needs are selected again by each copy's smap. *)
Lemma chain_adapt : forall u gfx sms rms h0 gf0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 None None ->
  forall rr H d f Cu nsu e1, A.FunCtx W u Cu nsu ->
  FS.CopyA.S.Realizes W Prog argv read state_ok rr Cu (FS.CopyA.S.callee_env d (FS.CopyA.thof f Cu) e1) H ->
  (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))) ->
  exists e2, adapt_provisions e1 sms = Some e2.
Proof.
  intros u gfx sms rms h0 gf0 Ch. remember None as sms0 eqn:E0. remember (@None (list (option nat))) as rms0 eqn:E1.
  induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 sms0 rms0 _ IH FCu FCh Incl Fx _ Sub _ Site];
    intros rr H d f Cu' nsu' e1 FCu' Real Fixd.
  - subst sms. exists e1. reflexivity.
  - destruct (FS.CopyA.S.functx_unique W _ _ _ _ _ FCu FCu') as [<- <-].
    pose proof (FS.CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Reads.
    destruct (FS.CopyA.copy_step_reads W d f e1 u Cu nsu h Ch nsh rm target gfx gfh FCu FCh Sub Site Fixd Reads) as [RdT NdT].
    destruct (FS.CopyA.copy_step_realizes W Prog argv read state_ok rr H d f e1 u Cu nsu h Ch nsh target
      FCu FCh Incl RdT NdT Sub Real) as [e1' [Sel RealH]].
    assert (FixH : forall x, A.Fixed W h x -> lookup_subst x gfh = Some (ground d (f x))).
    { intros x Fh. destruct (Fx x Fh) as [Fu E]. rewrite E. exact (Fixd x Fu). }
    destruct (IH E0 E1 rr H d f Ch nsh e1' FCh RealH FixH) as [e2 A2].
    exists e2. exact (EagerCopy.adapt_compose _ _ _ _ _ Sel A2).
Qed.

(* At an application of a closure whose copy chain ends at a platform
   family, the family's code reads each fixed machine-type input of the
   root as its machine type: the root closure records it (PlatformFixed),
   the chain keeps the root's fixed inputs as F's (chain_fixed), the
   caller's θ reads a fixed input as where the closure was provided
   (FixedMeans, the application's fixed agreement), and the code reads its
   inputs through θ. *)
Lemma chain_fixed_ground : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code sms rms gfx, FS.CopyA.S.FixedMeans W u d fx gfx ->
  forall h0 gf0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 None None ->
  FS.CopyA.S.ClosureOk W Prog argv read state_ok rr H h0 gf0 (FunctorClosure code None FS.CopyA.empty_env None) ->
  forall Ch0 needs outputs keys bindings, FamilyData h0 code Ch0 needs outputs keys bindings ->
  forall e x, In x (map fst (program_machines Prog)) ->
  A.Fixed W h0 x -> In x (map fst (A.c_ty Ch0)) ->
  ground (app_env d code th e) (TAbs x) = TRigid x.
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0 code sms rms gfx [Cov Vm]
    h0 gf0 Chn Ok0 Ch0 needs outputs keys bindings FD e x Inm F0 Ix0.
  pose proof AP as (R & Lf & FC & Inp & Th & Fixed & _).
  pose proof FD as (FC0 & _ & _ & FB & _).
  pose proof Pre0 as (_ & _ & _ & _ & [_ _] & _).
  pose proof (FS.CopyA.S.pre_absorbs_canon W Prog argv read state_ok _ _ _ _ _ _ _ _ Pre0) as Abs.
  pose proof (chain_keys _ _ _ _ _ _ _ _ Chn _ _ _ _ FC FC0) as Kin.
  pose proof (Kin x Ix0) as Ix.
  destruct (FS.CopyA.chain_fixed W _ _ _ _ _ _ _ _ Chn x F0) as [Fx Egf].
  destruct (FS.CopyA.S.closure_cases _ _ _ _ _ _ _ _ _ _ Ok0) as [CL|[CC|CP]].
  - destruct CL as (gg & h & Cu' & nsu & Cg & nsg & linktarget & dc & E & _). injection E; intros; discriminate.
  - destruct CC as (h & Cu' & nsu & Ch & nsh & target0 & code' & sms' & dc & rms' & rm' & gfh & E & _). discriminate E.
  - destruct CP as (code' & _ & _ & Mach).
    destruct (Cov x Fx) as [tx Itx]. pose proof (Vm x tx Itx) as Gx.
    rewrite <- Egf, (Mach x Inm F0) in Gx.
    injection Gx as Gx. destruct (Fixed x tx Itx) as [here [Lh Eh]].
    rewrite (app_env_ground d code th e Ch0 Cu (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ _ _ FB Th Kin (TAbs x)
      ltac:(intros y [<-|[]]; exact Ix0)).
    unfold ground. change (app_subst th (TAbs x)) with (match lookup_subst x th with Some t => t | None => TAbs x end).
    assert (Lt : lookup_subst x th = Some (A.canon C (app_subst (A.c_ty C) (TAbs x))))
      by (rewrite Th; exact (FS.CopyA.S.lookup_on_keys (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ x Ix)).
    rewrite Lt.
    change (app_subst (A.c_ty C) (TAbs x)) with (match lookup_subst x (A.c_ty C) with Some t => t | None => TAbs x end).
    rewrite lookup_subst_nlook, Lh. fold (ground d (A.canon C here)). rewrite Eh, (Abs tx). exact (eq_sym Gx).
Qed.

(* One application of a closure whose code is a platform family's (h0's,
   reached through sms, its positions through rms): a safe binding step,
   given that each position's source is one of the family's items, the
   caller's last position for each key agrees with its item, the needs
   re-selected through sms, and the primitives callable. *)
Lemma family_core : forall k, FS.CopyA.S.Below W Prog argv read state_ok k ->
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr, rr <= S k -> forall j d types H g st, S j <= rr -> Pre rr F C d types H g st ->
  forall code sms rms h0 Ch0 needs outputs keys bindings,
  lookup_provision (IFun u) d = Some (FunctorClosure code sms FS.CopyA.empty_env rms) ->
  FamilyData h0 code Ch0 needs outputs keys bindings ->
  incl (map fst (A.c_ty Ch0)) (map fst (A.c_ty Cu)) ->
  (forall t, ~ A.FunOutputs W u t) ->
  (exists news, A.c_provisions C' = (news ++ A.c_provisions C)%list /\
     (forall k p, In (k, p) news -> exists r m g a, k = A.mkey r m /\ p = A.ProvidedMethod g a)) ->
  (forall s, In s (closure_sources rms (map (A.rkey_of outputs) keys)) -> In s (map (A.rkey_of outputs) keys)) ->
  List.length (closure_sources rms (map (A.rkey_of outputs) keys)) = List.length ks ->
  (forall i r m g key, nth_error ks i = Some (A.mkey r m) ->
    (forall i', i < i' -> nth_error ks i' <> Some (A.mkey r m)) ->
    A.mlook r m (A.c_mths C') = Some g ->
    In key keys -> nth_error (closure_sources rms (map (A.rkey_of outputs) keys)) i = Some (A.rkey_of outputs key) ->
    exists rf a gf, key = IMth rf m a /\ A.mlook rf m (A.c_mths outputs) = Some gf /\
      Forall2 (SigAgreeG d Ch0 th) (A.sg_ret g :: r :: A.sg_params g) (A.sg_ret gf :: rf :: A.sg_params gf)) ->
  (forall d1, select_provisions d sm = Some d1 ->
     FS.CopyA.S.Realizes W Prog argv read state_ok rr Cu (FS.CopyA.S.callee_env d th d1) H ->
     exists d2, adapt_provisions d1 sms = Some d2 /\
     forall key op rf m a gf, In key keys -> In (BPrimitive (A.rkey_of outputs key) op) bindings ->
       Platform.MethodPrimitive W key op -> Platform.AtKey W outputs key op ->
       key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf ->
       CallableOk rr H (app_env d code th d2) (rf :: A.sg_params gf) (A.sg_ret gf)
         (primitive_closure (app_env d code th d2) (A.rkey_of outputs key) op)) ->
  FS.CopyA.S.StepOut W Prog argv read state_ok rr F (A.with_fresh F newgens) C' False (map gensof outs)
    (BApply u th (map (fun t => (t, gensof t)) outs) sm (map (A.rkey_of C') ks)) j d types H g st.
Proof.
  intros k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0
    code sms rms h0 Ch0 needs outputs keys bindings Look FD Kin NoOut [news [Eprov NewM]] SrcIn SrcLen Sigs Adapt rest.
  pose proof AP as (R & Fx & FC & Inp & Th & Fixed & Sat & FreshG & Uniq & Outs & NdO & OutsIff & PR).
  pose proof FD as (FC0 & _ & PRf & FB & Binds & _).
  assert (Onil : outs = []).
  { destruct outs as [|t0 r0]; [reflexivity|]. exfalso. exact (NoOut t0 (proj1 (OutsIff t0) (or_introl eq_refl))). }
  subst outs.
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [AbsM [[Vals [Fns [Mths Funs]]] [Section KM]]].
  pose proof (PublicContextFacts.section_canonical C Section) as Idem.
  pose proof (FS.CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Abs.
  rewrite FS.CopyA.S.bindings_apply_eq.
  destruct j as [|j1].
  { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  rewrite FS.CopyA.S.apply_functor_eq, Look. change (view_functor (program_of Prog argv read) code) with (A.funbody_of Prog code).
  rewrite FB. cbn [fc_bindings fc_outputs fc_sources].
  pose proof Th as Th'. subst th.
  destruct (FS.CopyA.S.static_realizes_le W Prog argv read state_ok rr rr C d H u Cu ns sm _ (le_n _) Real
    (FS.CopyA.S.functx_static W u Cu ns FC) Sat (FS.CopyA.S.apply_theta W C u Cu ns sm FC Section Inp Sat)) as [d1 [Sel [_ RealU]]].
  set (th := map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu)) in *.
  destruct (Adapt d1 Sel RealU) as [d2 [Ad Prims]].
  rewrite Sel, Ad.
  change (application_environment (program_of Prog argv read) d code th
    (append_environment d2 FS.CopyA.empty_env)) with (app_env d code th d2).
  set (de := app_env d code th d2).
  pose proof (family_data_primitives _ _ _ _ _ _ _ FD) as AllPrim.
  destruct (installs_run (program_of Prog argv read) j1 bindings de [] st AllPrim) as [Run|Run]; rewrite Run.
  2: { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  set (Dr := map (A.rkey_of outputs) keys) in *.
  set (rs := map (A.rkey_of C') ks).
  set (sel := result_selection rs rms Dr).
  assert (LenRs : List.length (closure_sources rms Dr) = List.length rs) by (unfold rs; rewrite length_map; exact SrcLen).
  (* the family's items, each installed *)
  assert (SrcKey : forall s, In s Dr -> exists key op, In key keys /\ s = A.rkey_of outputs key /\ last_prim s bindings = Some op).
  { intros s Is. unfold Dr in Is. apply in_map_iff in Is as [key [<- Ik]].
    destruct (forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [Ib [op [-> _]]]].
    destruct (last_prim_some _ _ _ Ib) as [op' Lp]. exists key, op'. split; [exact Ik|]. split; [reflexivity|exact Lp]. }
  destruct (select_ok (installs de bindings) sel) as [dr Sdr].
  { intros [a b] Iq. cbn [snd]. destruct (PublicCallerKeys.selection_in _ _ _ _ _ Iq) as [_ Ib].
    destruct (SrcKey b (SrcIn b Ib)) as (key & op & _ & _ & Lp). rewrite installs_lookup, Lp. discriminate. }
  change (select_provisions (installs de bindings) (result_selection (map (A.rkey_of C') ks) rms Dr)) with
    (select_provisions (installs de bindings) sel).
  rewrite Sdr. cbn [map result_environment read_result_types fresh_result_types].
  left. set (d' := append_environment (EnvironmentWithAliases [] [] (provision_values dr)) d).
  exists d', g, st, H. split; [apply FS.CopyA.S.extends_refl|]. split; [apply FS.CopyA.S.meanings_grow_refl; reflexivity|].
  split; [|reflexivity].
  (* the caller's keys: new methods *)
  assert (KsM : forall k0, In k0 ks -> exists r m, k0 = A.mkey r m).
  { intros k0 Ik. destruct (PublicCallerKeys.provide_result_key_news W u C _ C' ks k0 PR Ik news Eprov) as [p0 Ip].
    destruct (NewM k0 p0 Ip) as (r0 & m0 & _ & _ & -> & _). eauto. }
  assert (SmKeys : forall K s, In (K, s) sel -> exists r m, K = A.rkey C' r m /\ In (A.mkey r m) ks).
  { intros K s Iq. destruct (PublicCallerKeys.selection_in _ _ _ _ _ Iq) as [IK _]. unfold rs in IK.
    apply in_map_iff in IK as [k0 [<- Ik]]. destruct (KsM k0 Ik) as (r0 & m0 & ->). exists r0, m0. split; [reflexivity|exact Ik]. }
  assert (Nm : forall k0, (forall r m th0, k0 <> IMth r m th0) -> lookup_provision k0 d' = lookup_provision k0 d).
  { intros k0 Nk. unfold d'. rewrite FS.CopyA.S.lookup_append_env.
    change (lookup_provision k0 (EnvironmentWithAliases [] [] (provision_values dr))) with (lookup_provision k0 dr).
    rewrite (select_lookup _ _ _ k0 Sdr), sm_find_none; [reflexivity|].
    intros s Iq. destruct (SmKeys k0 s Iq) as (r0 & m0 & -> & _). exact (Nk _ _ _ eq_refl). }
  assert (Fd : forall k0 s, sm_find k0 sel = Some s -> lookup_provision k0 d' = lookup_provision s (installs de bindings)).
  { intros k0 s Fs. unfold d'. rewrite FS.CopyA.S.lookup_append_env.
    change (lookup_provision k0 (EnvironmentWithAliases [] [] (provision_values dr))) with (lookup_provision k0 dr).
    rewrite (select_lookup _ _ _ k0 Sdr), Fs.
    destruct (PublicCallerKeys.selection_in _ _ _ _ _ (sm_find_in _ _ _ Fs)) as [_ Is].
    destruct (SrcKey s (SrcIn s Is)) as (key & op & _ & _ & Lp). rewrite installs_lookup, Lp. reflexivity. }
  assert (Fd0 : forall k0, sm_find k0 sel = None -> lookup_provision k0 d' = lookup_provision k0 d).
  { intros k0 Fs. unfold d'. rewrite FS.CopyA.S.lookup_append_env.
    change (lookup_provision k0 (EnvironmentWithAliases [] [] (provision_values dr))) with (lookup_provision k0 dr).
    rewrite (select_lookup _ _ _ k0 Sdr), Fs. reflexivity. }
  (* the caller's new provisions *)
  destruct (PublicFormation.provide_result_shape W u C _ C' ks PR) as [Esub [Eloc [outs' [OI Ety]]]].
  assert (Onil' : outs' = []).
  { destruct outs' as [|t0 r0]; [reflexivity|]. exfalso. exact (NoOut t0 (proj1 (OI t0) (or_introl eq_refl))). }
  subst outs'.
  assert (KMn : KeysMatch news).
  { intros k0 p0 I0. destruct (NewM k0 p0 I0) as (r0 & m0 & g0 & a0 & -> & ->). exact Logic.I. }
  assert (KM' : KeysMatch (A.c_provisions C')).
  { rewrite Eprov. intros k0 p0 I0. apply in_app_or in I0 as [I0|I0]; [exact (KMn _ _ I0)|exact (KM _ _ I0)]. }
  assert (NewNone : forall k0, (forall r m, k0 <> A.mkey r m) -> A.ilook k0 news = None).
  { intros k0 Nk. clear -NewM Nk. revert NewM. induction news as [|[k1 p1] rest IH]; intros NewM; [reflexivity|]. cbn.
    destruct (NewM k1 p1 (or_introl eq_refl)) as (r1 & m1 & _ & _ & -> & _).
    destruct (item_eqb k0 (A.mkey r1 m1)) eqn:E; [apply item_eqb_spec in E; exfalso; exact (Nk r1 m1 E)|].
    apply IH. intros k p I0. exact (NewM k p (or_intror I0)). }
  assert (Old : forall k0, (forall r m, k0 <> A.mkey r m) -> A.ilook k0 (A.c_provisions C') = A.ilook k0 (A.c_provisions C)).
  { intros k0 Nk. rewrite Eprov, ilook_app, (NewNone k0 Nk). reflexivity. }
  assert (RealC' : FS.CopyA.S.Realizes W Prog argv read state_ok rr C' d' H).
  { assert (Emerge : A.c_merge C' = A.c_merge C)
      by (eapply PublicContextFacts.provided_result_merge; exact PR).
    split; [intros left right Eq; apply AbsM; now rewrite Emerge in Eq |].
    split; [|split; [|exact KM']].
    2: { eapply PublicContextFacts.section_same_fields; eassumption. }
    split; [|split; [|split]].
    - intros v t Found. change (A.c_vals C') with (vals_of (A.c_provisions C')) in Found.
      rewrite vals_by_ilook in Found by exact KM'. rewrite Old in Found by (intros r m E; discriminate).
      rewrite <- vals_by_ilook in Found by exact KM. destruct (Vals v t Found) as [w [L T]].
      exists w. split; [|exact T]. rewrite Nm; [exact L|intros r m th0 E; discriminate].
    - intros f g0 Found. change (A.c_fns C') with (fns_of (A.c_provisions C')) in Found.
      rewrite fns_by_ilook in Found by exact KM'. rewrite Old in Found by (intros r m E; discriminate).
      rewrite <- fns_by_ilook in Found by exact KM. destruct (Fns f g0 Found) as [pv [L Ok]].
      exists pv. split; [rewrite Nm; [exact L|intros r m th0 E; discriminate]|].
      exact (FS.CopyA.S.callable_transfer W Prog argv read state_ok rr rr H d d' _ _ _ _ pv (le_n _) eq_refl eq_refl Ok).
    - intros r m g0 Found. pose proof Found as FoundC'.
      change (A.c_mths C') with (mths_of (A.c_provisions C')) in Found.
      rewrite mths_by_ilook in Found by exact KM'. rewrite Eprov, ilook_app in Found.
      destruct (A.ilook (A.mkey r m) news) as [p0|] eqn:In0.
      + (* a new method: a family primitive, read at its last position *)
        assert (Ik : In (A.mkey r m) ks)
          by exact (PublicCallerKeys.provide_result_news_key W u C _ C' ks PR news Eprov _ p0 (FS.CopyA.S.ilook_in _ _ _ In0)).
        destruct (In_nth_error _ _ Ik) as [i0 E0].
        assert (R0 : nth_error rs i0 = Some (A.rkey C' r m)) by (unfold rs; rewrite nth_error_map, E0; reflexivity).
        destruct (PublicCallerKeys.selection_at rs rms Dr i0 _ R0 LenRs) as [s Fs].
        destruct (PublicCallerKeys.selection_last rs rms Dr _ s Fs LenRs) as [i [Ri [Si Last]]].
        unfold rs in Ri. rewrite nth_error_map in Ri.
        destruct (nth_error ks i) as [ki|] eqn:Ki; [|discriminate]. injection Ri as Eki.
        assert (Eq : ki = A.mkey r m).
        { pose proof (PublicCallerKeys.rkey_of_inj C' ki (A.mkey r m) Eki) as Pid.
          rewrite (PublicCallerKeys.result_key_id W u C _ C' ks ki PR (nth_error_In _ _ Ki)) in Pid. exact Pid. }
        subst ki.
        assert (Last' : forall i', i < i' -> nth_error ks i' <> Some (A.mkey r m)).
        { intros i' Lt' E'. apply (Last i' Lt'). unfold rs. rewrite nth_error_map, E'. reflexivity. }
        destruct (SrcKey s (SrcIn s (nth_error_In _ _ Si))) as (key & op & Ikey & Es & Lp).
        destruct (Sigs i r m g0 key Ki Last' FoundC' Ikey ltac:(rewrite <- Es; exact Si)) as (rf & a & gf & Ek & Mgf & Agr).
        destruct (FS.CopyA.S.Forall2_In_r _ _ _ _ _ _ Binds (last_prim_in _ _ _ Lp)) as [key2 [Ik2 [op' [Eb [MP [AK _]]]]]].
        injection Eb as Es2 <-.
        assert (Ek2 : key2 = key).
        { pose proof (PublicCallerKeys.rkey_of_inj outputs key2 key ltac:(rewrite <- Es2; exact Es)) as Pid.
          rewrite (PublicCallerKeys.result_key_id W h0 Ch0 _ outputs keys key2 PRf Ik2),
            (PublicCallerKeys.result_key_id W h0 Ch0 _ outputs keys key PRf Ikey) in Pid. exact Pid. }
        subst key2.
        pose proof (Prims key op rf m a gf Ikey ltac:(rewrite <- Es2; exact (last_prim_in _ _ _ Lp))
          MP AK Ek Mgf) as Call.
        assert (GE : forall tc tf, SigAgreeG d Ch0 th tc tf -> ground de tf = ground d' tc).
        { intros tc tf [Sc Ec]. unfold de.
          rewrite (app_env_ground d code th d2 Ch0 Cu (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ _ _ FB eq_refl Kin tf Sc).
          change (ground d' tc) with (ground d tc). exact (eq_sym Ec). }
        exists (primitive_closure de (A.rkey_of outputs key) op). split.
        * rewrite (Fd _ s Fs), installs_lookup, Lp, Es. reflexivity.
        * inversion Agr as [|t1 t2 l1 l2 Aret Rest]; subst.
          apply (FS.CopyA.S.callable_transfer W Prog argv read state_ok rr rr H de d' (rf :: A.sg_params gf) (r :: A.sg_params g0)
            (A.sg_ret gf) (A.sg_ret g0) _ (le_n _)); [|exact (GE _ _ Aret)|exact Call].
          clear -Rest GE. induction Rest as [|t1 t2 l1 l2 A1 _ IH]; [reflexivity|]. cbn [map]. rewrite (GE _ _ A1). f_equal. exact IH.
      + (* an old method, which the selection does not shadow *)
        assert (Mo : A.mlook r m (A.c_mths C) = Some g0)
          by (change (A.c_mths C) with (mths_of (A.c_provisions C)); rewrite mths_by_ilook by exact KM; exact Found).
        destruct (Mths r m g0 Mo) as [pv [L Ok]].
        assert (Rk : A.rkey C' r m = A.rkey C r m).
        { unfold A.rkey. rewrite !FS.CopyA.S.capps_ilook, Eprov, ilook_app, In0. unfold A.theta_of, A.canon. rewrite Esub. reflexivity. }
        exists pv. rewrite Rk. split.
        * rewrite Fd0; [exact L|]. apply sm_find_none. intros s Iq. rewrite <- Rk in Iq.
          destruct (SmKeys _ s Iq) as (r' & m' & Ek' & Ik').
          unfold A.rkey in Ek'. injection Ek' as Er Emm _. subst r' m'.
          destruct (PublicCallerKeys.provide_result_key_news W u C _ C' ks _ PR Ik' news Eprov) as [p1 Ip1].
          destruct (PublicCallerKeys.ilook_of_in _ _ _ _ Ip1) as [p2 L2]. congruence.
        * exact (FS.CopyA.S.callable_transfer W Prog argv read state_ok rr rr H d d' _ _ _ _ pv (le_n _) eq_refl eq_refl Ok).
    - intros u0 fx0 Found. change (A.c_funs C') with (funs_of (A.c_provisions C')) in Found.
      rewrite funs_by_ilook in Found by exact KM'. rewrite Old in Found by (intros r m E; discriminate).
      rewrite <- funs_by_ilook in Found by exact KM. destruct (Funs u0 fx0 Found) as [pv [gfx [L [Ok [Cov Vm]]]]].
      exists pv, gfx. split; [rewrite Nm; [exact L|intros r m th0 E; discriminate]|].
      split; [exact Ok|split; [exact Cov|exact Vm]]. }
  split; [exact Frame|]. split.
  { destruct Ctx as [Locals [Recv Al]]. split; [|split].
    - intros n0 t mu S0 Nn. rewrite Eloc in S0. exact (Locals n0 t mu S0 Nn).
    - exact Recv.
    - intros x tx Lx. rewrite <- lookup_subst_nlook in Lx. rewrite Ety in Lx.
      unfold A.output_type_map in Lx. cbn [map app] in Lx. rewrite !FS.CopyA.S.lookup_map_snd in Lx.
      destruct (lookup_subst x (A.c_ty C)) as [t0|] eqn:L0; [|discriminate]. cbn [option_map] in Lx.
      injection Lx as <-. rewrite lookup_subst_nlook in L0.
      change (output_meaning d x = Some (ground d (A.canon C (A.canon C t0)))). rewrite !Abs. exact (Al x t0 L0). }
  split; [exact Lt|]. split; [exact St|]. split; [exact RealC'|].
  split; [|split; [exact (proj1 (proj2 Fmd))|exact (PublicScoping.apply_abstract_cxscoped W F _ C u gensof C' ks PR eq_refl (proj2 (proj2 Fmd)))]].
  apply (PublicFormation.apply_abstract_formed W C u gensof C' ks (proj1 Fmd)); [|exact PR].
  intros t Ft. exfalso. exact (NoOut t Ft).
Qed.

(* A copy chain keeps its root's leaves both ways, and so its outputs. *)
Lemma chain_leaves_back : forall u gfx sms rms h0 gf0 sms0 rms0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 sms0 rms0 ->
  forall l, A.ResultLeaf W u l -> A.ResultLeaf W h0 l.
Proof.
  intros u gfx sms rms h0 gf0 sms0 rms0 Ch.
  induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 sms0 rms0 _ IH _ _ _ _ _ _ Same _];
    intros l L; [exact L|]. exact (IH l (proj1 (Same l) L)).
Qed.

Lemma chain_no_outputs : forall u gfx sms rms h0 gf0 sms0 rms0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 sms0 rms0 ->
  (forall t, ~ A.FunOutputs W h0 t) -> forall t, ~ A.FunOutputs W u t.
Proof.
  intros u gfx sms rms h0 gf0 sms0 rms0 Ch.
  induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 sms0 rms0 _ IH _ _ _ _ _ _ Same _];
    intros N t Ft; [exact (N t Ft)|]. exact (IH N t (proj1 (FS.CopyA.S.same_result_outputs W u h Same t) Ft)).
Qed.

Hypothesis chain_sigs : ChainForwardSigs.
Hypothesis chain_prims : ChainPrimsOk.

(* What family_core asks of the chain, from the chain itself, the
   family's data and ChainForwardSigs. *)
Lemma chain_core_premises : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code sms rms gfx, lookup_provision (IFun u) d = Some (FunctorClosure code sms FS.CopyA.empty_env rms) ->
  FS.CopyA.S.FixedMeans W u d fx gfx ->
  forall h0 gf0, FS.CopyA.CopyChain W u gfx sms rms h0 gf0 None None ->
  forall Ch0 needs outputs keys bindings, FamilyData h0 code Ch0 needs outputs keys bindings ->
  incl (map fst (A.c_ty Ch0)) (map fst (A.c_ty Cu)) /\
  (forall t, ~ A.FunOutputs W u t) /\
  (exists news, A.c_provisions C' = (news ++ A.c_provisions C)%list /\
     (forall k p, In (k, p) news -> exists r m g a, k = A.mkey r m /\ p = A.ProvidedMethod g a)) /\
  (forall s, In s (closure_sources rms (map (A.rkey_of outputs) keys)) -> In s (map (A.rkey_of outputs) keys)) /\
  List.length (closure_sources rms (map (A.rkey_of outputs) keys)) = List.length ks /\
  (forall i r m g key, nth_error ks i = Some (A.mkey r m) ->
    (forall i', i < i' -> nth_error ks i' <> Some (A.mkey r m)) ->
    A.mlook r m (A.c_mths C') = Some g ->
    In key keys -> nth_error (closure_sources rms (map (A.rkey_of outputs) keys)) i = Some (A.rkey_of outputs key) ->
    exists rf a gf, key = IMth rf m a /\ A.mlook rf m (A.c_mths outputs) = Some gf /\
      Forall2 (SigAgreeG d Ch0 th) (A.sg_ret g :: r :: A.sg_params g) (A.sg_ret gf :: rf :: A.sg_params gf)).
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0 code sms rms gfx Look Fm
    h0 gf0 Chn Ch0 needs outputs keys bindings FD.
  pose proof AP as (_ & _ & FC & _ & _ & _ & _ & _ & _ & _ & _ & _ & PR).
  pose proof FD as (FC0 & NoH & PRf & _ & Binds & _).
  pose proof (chain_no_outputs _ _ _ _ _ _ _ _ Chn NoH) as NoU.
  destruct (FS.CopyA.chain_sources_range W _ _ _ _ _ _ _ _ Chn eq_refl (map (A.rkey_of outputs) keys)
    ltac:(intros C0 o C0' ks0 P0; rewrite length_map; exact (PublicCallerKeys.provide_result_length W h0 _ _ _ _ _ _ _ _ PRf P0)))
    as [LenU InU].
  split; [exact (chain_keys _ _ _ _ _ _ _ _ Chn _ _ _ _ FC FC0)|].
  split; [exact NoU|].
  split.
  { apply (PublicChainAgree.copy_news_methods W u h0 (chain_leaves_back _ _ _ _ _ _ _ _ Chn) NoU NoH Ch0 _ outputs keys PRf
      ltac:(intros key Ik; destruct (forall2_in_l _ _ _ _ _ _ Binds Ik) as [bd [_ [op [_ [MP _]]]]];
            destruct MP as (r0 & m0 & a0 & _ & _ & _ & _ & _ & _ & -> & _); eauto)
      C _ C' ks PR). }
  split; [exact InU|]. split; [exact (LenU _ _ _ _ PR)|].
  exact (chain_sigs F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0 code sms rms gfx Look Fm
    h0 gf0 Chn Ch0 needs outputs keys bindings FD).
Qed.

(* ChainPlatformOk (PublicCopyApply): applying a closure whose copy chain
   ends at a platform family is a safe binding step. The root is read off
   the closure's derivation (chain_entry): a platform family, whose
   primitives the code installs, or a linked body sharing the family's
   code, which then has no bindings. *)
Theorem chain_platform_ok : FS.CopyA.ChainPlatformOk W Prog argv read state_ok.
Proof.
  intros k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0
    code sms rms gfx h0 gf0 Look Ok Fm Chn Fam.
  pose proof AP as (R & Lf & FC & Inp & Th & Fixed & Sat & _).
  pose proof Pre0 as (_ & _ & _ & _ & Real & _).
  pose proof Real as [_ [_ [Section _]]].
  pose proof (PublicContextFacts.section_canonical C Section) as Idem.
  pose proof (FS.CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Abs.
  set (f := fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))).
  assert (Thf : th = FS.CopyA.thof f Cu) by exact Th.
  assert (CT : A.CallTheta W C Cu u th) by (rewrite Th; exact (FS.CopyA.S.apply_theta W C u Cu ns sm FC Section Inp ltac:(rewrite <- Th; exact Sat))).
  destruct (FS.CopyA.S.static_realizes_le W Prog argv read state_ok rr rr C d H u Cu ns sm th (le_n _) Real
    (FS.CopyA.S.functx_static W u Cu ns FC) Sat CT) as [d1 [Sel [_ RealU]]].
  assert (Fixd : forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))).
  { intros x Fx. destruct Fm as [Cov Vm]. destruct (Cov x Fx) as [tx Itx]. rewrite (Vm x tx Itx).
    destruct (Fixed x tx Itx) as [here [Lh Eh]]. unfold f.
    change (app_subst (A.c_ty C) (TAbs x)) with (match lookup_subst x (A.c_ty C) with Some t => t | None => TAbs x end).
    rewrite lookup_subst_nlook, Lh. f_equal. change (ground d tx = ground d (A.canon C here)).
    rewrite Eh, (Abs tx). reflexivity. }
  assert (RealF : FS.CopyA.S.Realizes W Prog argv read state_ok rr Cu (FS.CopyA.S.callee_env d (FS.CopyA.thof f Cu) d1) H)
    by (rewrite <- Thf; exact RealU).
  destruct (FS.CopyA.chain_entry W Prog argv read state_ok rr H u gfx _ Ok code sms FS.CopyA.empty_env rms eq_refl
    d f Cu ns d1 FC RealF Fixd)
    as [(h0' & gf0' & gg & Ch0' & nsh0 & Cg & nsg & linktarget & e2 & Chn' & Ok0 & LB & FC0' & FCg & Keys & A2 & RealG)
       |(h0' & gf0' & Chn' & _ & Fam' & Ok0)].
  - (* a linked root: the family's code has no bindings *)
    destruct (family_data h0 code Fam) as (Ch0 & needs & outputs & keys & bindings & FD).
    pose proof FD as (FC0 & _ & _ & FB & Binds & _).
    destruct LB as [pins [LB _]].
    pose proof (platform_not_linked h0 code h0' gg pins _ Fam LB FB) as Nb. cbn [fc_bindings] in Nb. subst bindings.
    inversion Binds; subst keys.
    destruct (chain_core_premises F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0 code sms rms gfx
      Look Fm h0 gf0 Chn Ch0 needs outputs [] [] FD) as (Kin & NoOut & News & SrcIn & SrcLen & Sigs).
    apply (family_core k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0
      code sms rms h0 Ch0 needs outputs [] [] Look FD Kin NoOut News SrcIn SrcLen Sigs).
    intros d1' Sel' _. rewrite Sel in Sel'. injection Sel' as <-. exists e2. split; [exact A2|].
    intros key op rf m a gf [].
  - (* a platform root *)
    destruct (family_data h0' code Fam') as (Ch0 & needs & outputs & keys & bindings & FD).
    destruct (chain_core_premises F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0 code sms rms gfx
      Look Fm h0' gf0' Chn' Ch0 needs outputs keys bindings FD) as (Kin & NoOut & News & SrcIn & SrcLen & Sigs).
    apply (family_core k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0
      code sms rms h0' Ch0 needs outputs keys bindings Look FD Kin NoOut News SrcIn SrcLen Sigs).
    intros d1' Sel' RealU'. rewrite Thf in RealU'.
    destruct (chain_adapt _ _ _ _ _ _ Chn' rr H d f Cu ns d1' FC RealU' Fixd) as [d2 A2].
    exists d2. split; [exact A2|].
    intros key op rf m a gf Ik Ib MP AK Ek Mgf.
    exact (chain_prims F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0
      code sms rms gfx Look Fm h0' gf0' Chn' Ok0 Ch0 needs outputs keys bindings FD d2 key op rf m a gf Ik Ib MP AK Ek Mgf).
Qed.

(* ---- a platform family applied as a family (FamilyPlatformApplyOk) ----------- *)

Section FamilyPlatform.

(* The primitive half of ChainPrimsOk, for any environment de the code
   runs in that reads each fixed machine-type input of the family as its
   machine type. ChainPrimsOk follows (chain_prims_of_ground, with
   chain_fixed_ground), and so does the family application's. PublicEntry
   discharges it for the reference host (platform_prims_public). *)
Definition PrimsGroundOk : Prop :=
  forall rr H de code h0 Ch0 needs outputs keys bindings,
  FamilyData h0 code Ch0 needs outputs keys bindings ->
  (forall x, In x (map fst (program_machines Prog)) ->
     A.Fixed W h0 x -> In x (map fst (A.c_ty Ch0)) -> ground de (TAbs x) = TRigid x) ->
  forall key op rf m a gf, In key keys -> In (BPrimitive (A.rkey_of outputs key) op) bindings ->
  Platform.MethodPrimitive W key op -> Platform.AtKey W outputs key op ->
  key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf ->
  CallableOk rr H de (rf :: A.sg_params gf) (A.sg_ret gf) (primitive_closure de (A.rkey_of outputs key) op).

Lemma chain_prims_of_ground : PrimsGroundOk -> ChainPrimsOk.
Proof.
  intros PG F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0 code sms rms gfx Look Fm
    h0 gf0 Chn Ok0 Ch0 needs outputs keys bindings FD e.
  apply (PG rr H (app_env d code th e) code h0 Ch0 needs outputs keys bindings FD).
  intros x Inm Fx Ix.
  exact (chain_fixed_ground F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0
    code sms rms gfx Fm h0 gf0 Chn Ok0 Ch0 needs outputs keys bindings FD e x Inm Fx Ix).
Qed.

(* PREMISE (formation, T29). An extra key of a family context's table
   (one the family's own inputs lack) that a path names is a type symbol:
   a context's table keys are type symbols or fresh generations, which no
   path names (PublicFormation.TysScoped says so of a formed context; the
   family application's context C need not be formed here). With it, the
   family's result read at its own inputs and at the family context agree
   (PublicFamilyAgree.family_items_agree): a receiver is a table key on
   both sides or on neither. *)
Definition FamilyKeysTyped : Prop := forall C u matching th sm Cr ks, PublicFormation.TysScoped W C ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks -> forall Cu needs, A.FunCtx W u Cu needs ->
  forall q pa x, A.resolve_sym W q pa = Some x -> A.nlook x (A.c_ty (A.family_context C matching)) <> None ->
  A.nlook x (A.c_ty Cu) = None -> A.kind_sym W x = Some A.DKType.

(* The agreement family_platform_ok needs (FamilyResultAgrees, formerly a
   premise), from FamilyKeysTyped and the family context's idempotent
   merge. *)
Lemma family_result_agrees : FamilyKeysTyped -> forall C u matching th sm Cr ks, PublicFormation.TysScoped W C ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks -> (forall t, A.canon C (A.canon C t) = A.canon C t) ->
  forall code Cu needs outputs keys bindings, FamilyData u code Cu needs outputs keys bindings ->
  ItemsAgree (A.family_context C matching) Cr Cu outputs th
    (fun r m g => In (A.mkey r m) ks /\ A.mlook r m (A.c_mths Cr) = Some g) keys.
Proof.
  intros KT C u matching th sm Cr ks Ts FA Idem code Cu needs outputs keys bindings (FC & NoOut & PRf & _ & Binds & _).
  apply (PublicFamilyAgree.family_items_agree W C u matching th sm Cr ks Cu needs outputs keys FA FC NoOut PRf).
  - intros key Ik. destruct (forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [_ [op [_ [MP _]]]]].
    destruct MP as (r0 & m0 & a0 & d0 & form & nm & pars & res & body & -> & Dm & Ast & Form).
    exists r0, m0, a0. split; [reflexivity|]. unfold A.kind_sym. rewrite Dm, Ast. cbn.
    destruct form; [contradiction|reflexivity|reflexivity].
  - exact Idem.
  - exact (KT C u matching th sm Cr ks Ts FA Cu needs FC).
Qed.

Hypothesis prims_ground : PrimsGroundOk.
Hypothesis family_keys : FamilyKeysTyped.

(* FamilyPlatformApplyOk (PublicFamilyStep): a family application of a
   platform family selects u's inputs at θ, runs the code's installs, and
   files them under the run-time keys of Cr's new methods, each callable at
   its signature. The machine types are grounded as in chain_fixed_ground,
   the fixed reading agreement coming from AuditFacts.FamilyApplication's
   FixedAgreement at the family context. *)
Theorem family_platform_ok : FS.FamilyPlatformApplyOk W Prog argv read state_ok.
Proof.
  intros k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0 code Look Fam.
  pose proof FA as (Cu & ns & (fx & Fx & _ & Poly & FC & Th & InF & FAg & TypedNeeds & PR & Nd) & EmittedNeeds).
  pose proof (conj EmittedNeeds TypedNeeds) as Sat.
  destruct (family_data u code Fam) as (Ch0 & needs & outputs & keys & bindings & FD).
  pose proof FD as (FC0 & _ & PRf & FB & Binds & _).
  destruct (FS.CopyA.S.functx_unique W _ _ _ _ _ FC FC0) as [<- <-].
  destruct (PublicFormation.family_context_shape matching C) as [Ety [EsubF [EprF EloF]]].
  set (Cf := A.family_context C matching) in *.
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [_ [[_ [_ [_ Funs]]] [Section KM]]].
  pose proof (PublicContextFacts.section_canonical C0 Section) as Idem.
  pose proof (FS.CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Abs.
  assert (FxC0 : A.nlook u (A.c_funs C0) = Some fx) by (unfold A.c_funs; rewrite <- Ep; exact Fx).
  assert (CanonEq : forall t, A.canon Cf t = A.canon C0 t) by (intros t; unfold A.canon; rewrite EsubF, Es; reflexivity).
  destruct (Funs u fx FxC0) as [pv [gfx [Look' [Ok Fm]]]].
  rewrite Look in Look'. injection Look' as <-.
  destruct (FS.CopyA.S.closure_cases _ _ _ _ _ _ _ _ _ _ Ok) as [CL|[CC|CP]].
  { destruct CL as (gg & h & Cu' & nsu & Cg & nsg & linktarget & dc & E & _). injection E; intros; discriminate. }
  { destruct CC as (h & Cu' & nsu & Ch & nsh & target0 & code' & sms' & dc & rms' & rm' & gfh & E & _). injection E; intros.
    destruct sms'; discriminate. }
  destruct CP as (code' & _ & _ & Mach).
  pose proof (family_result_agrees family_keys C u matching th sm Cr ks Ts FA
    ltac:(intros t; unfold A.canon; rewrite Es; exact (Idem t)) code Cu ns outputs keys bindings FD) as [NewSig NewAll].
  intros rest. rewrite FS.CopyA.S.bindings_apply_eq.
  destruct j as [|j1].
  { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  rewrite FS.CopyA.S.apply_functor_eq, Look. change (view_functor (program_of Prog argv read) code) with (A.funbody_of Prog code).
  rewrite FB. cbn [fc_bindings fc_outputs fc_sources].
  subst th. set (th := map (fun q => (fst q, A.canon Cf (app_subst (A.c_ty Cf) (TAbs (fst q))))) (A.c_ty Cu)) in *.
  set (f := fun x => A.canon Cf (app_subst (A.c_ty Cf) (TAbs x))).
  assert (Thf : th = map (fun q => (fst q, f (fst q))) (A.c_ty Cu)) by reflexivity.
  (* the state realizes the family context, which differs from C0 only in its table *)
  assert (RealF : FS.CopyA.S.Realizes W Prog argv read state_ok rr Cf d H)
    by (apply (FS.CopyA.S.realizes_ty_free W Prog argv read state_ok rr C0 Cf d H); [rewrite EsubF; exact Es|unfold Cf; rewrite (PublicContextFacts.family_context_merge matching C); exact Emg|
      rewrite EprF; exact Ep|exact Real]).
  assert (Inp : forall t tt, A.nlook t (A.c_ty Cu) = Some tt -> exists here, A.nlook t (A.c_ty Cf) = Some here).
  { intros t tt L. apply nlook_in in L. destruct (A.nlook t (A.c_ty Cf)) as [here|] eqn:Lh; [exists here; reflexivity|].
    exfalso. exact (InF t (in_map fst _ _ L) Lh). }
  destruct (FS.CopyA.S.static_realizes_le W Prog argv read state_ok rr rr Cf d H u Cu ns sm th (le_n _) RealF
    (FS.CopyA.S.functx_static W _ _ _ FC) Sat (FS.CopyA.S.apply_theta W Cf u Cu ns sm FC (proj1 (proj2 (proj2 RealF))) Inp Sat)) as [d1 [Sel [_ _]]].
  rewrite Sel. cbn [adapt_provisions].
  change (application_environment (program_of Prog argv read) d code th
    (append_environment d1 (EnvironmentWithAliases [] [] []))) with (app_env d code th d1).
  set (de := app_env d code th d1).
  (* the code reads u's fixed machine-type inputs as the machine types *)
  assert (KMf : PublicCallRealize.KeysMatch (A.c_provisions Cf)) by (rewrite EprF, Ep; exact KM).
  assert (FxF : A.nlook u (A.c_funs Cf) = Some fx) by (unfold A.c_funs; rewrite EprF; exact Fx).
  assert (MachG : forall x, In x (map fst (program_machines Prog)) ->
     A.Fixed W u x -> In x (map fst (A.c_ty Cu)) -> ground de (TAbs x) = TRigid x).
  { intros x Inm Fx' Ix. unfold de.
    rewrite (app_env_ground d code th d1 Cu Cu f _ _ _ FB Thf (incl_refl _) (TAbs x)
      ltac:(intros y [<-|[]]; exact Ix)).
    change (app_subst th (TAbs x)) with (match lookup_subst x th with Some t => t | None => TAbs x end).
    assert (Lx : lookup_subst x th = Some (f x)) by (rewrite Thf; exact (FS.CopyA.S.lookup_on_keys f _ x Ix)). rewrite Lx. unfold f.
    destruct Fm as [Cov Vm]. destruct (Cov x Fx') as [tx Ixt]. pose proof (Vm x tx Ixt) as Gx.
    rewrite (Mach x Inm Fx') in Gx. injection Gx as Gx.
    destruct (A.nlook x (A.c_ty Cf)) as [now|] eqn:Ln; [|exfalso; exact (InF x Ix Ln)].
    replace (app_subst (A.c_ty Cf) (TAbs x)) with now by (cbn [app_subst]; rewrite lookup_subst_nlook', Ln; reflexivity).
    destruct (FS.CopyA.S.nlook_fixed_app fx x (Cov x Fx')) as [was [Nw Iw]].
    pose proof (FAg x Fx') as Ag.
    rewrite (FS.CopyA.S.ilook_apps_fun Cf u fx (funs_ilook _ u fx KMf FxF)), Ln, Nw in Ag.
    assert (E1 : Some (ground d tx) = Some (ground d was)) by (rewrite <- (Vm x tx Ixt), <- (Vm x was Iw); reflexivity).
    injection E1 as E1.
    rewrite Gx, E1. symmetry.
    apply (proj1 RealF). eapply A.METrans; [exact Ag|].
    apply PublicMerge.merge_eq_read. exact (proj1 (proj1 (proj2 (proj2 RealF)))). }
  pose proof (family_data_primitives _ _ _ _ _ _ _ FD) as AllPrim.
  destruct (installs_run (program_of Prog argv read) j1 bindings de [] st AllPrim) as [Run|Run]; rewrite Run.
  2: { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  assert (KeysM : forall key, In key keys -> exists r m a op, key = IMth r m a /\
     In (BPrimitive (A.rkey_of outputs key) op) bindings).
  { intros key Ik. destruct (forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [Ib [op [-> [MP _]]]]].
    destruct MP as (r0 & m0 & a0 & _ & _ & _ & _ & _ & _ & -> & _). exists r0, m0, a0, op. split; [reflexivity|exact Ib]. }
  set (Dr := map (A.rkey_of outputs) keys) in *.
  set (rs := map (A.rkey_of Cr) ks).
  set (sel := result_selection rs None Dr).
  (* position by position, the family's run-time key at θ is the caller's *)
  pose proof (PublicFamilyAgree.family_items_pos W C u matching th sm Cr ks Cu ns outputs keys FA FC
    (proj1 (proj2 FD)) PRf
    ltac:(intros key Ik; destruct (forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [_ [op [_ [MP _]]]]];
          destruct MP as (r0 & m0 & a0 & d0 & form & nm & pars & res & body & -> & Dm & Ast & Form);
          exists r0, m0, a0; split; [reflexivity|]; unfold A.kind_sym; rewrite Dm, Ast; cbn;
          destruct form; [contradiction|reflexivity|reflexivity])
    ltac:(intros t; unfold A.canon; rewrite Es; exact (Idem t))
    (family_keys C u matching th sm Cr ks Ts FA Cu ns FC)) as Pos.
  assert (LenRs : List.length (closure_sources None Dr) = List.length rs)
    by (unfold rs, Dr; cbn [closure_sources]; rewrite !length_map; exact (eq_sym (Forall2_length Pos))).
  assert (Inst : forall s, In s Dr -> exists op, last_prim s bindings = Some op).
  { intros s Is. unfold Dr in Is. apply in_map_iff in Is as [key [<- Ik]].
    destruct (KeysM key Ik) as (r0 & m0 & a0 & op & _ & Ib). exact (last_prim_some _ _ _ Ib). }
  destruct (select_ok (installs de bindings) sel) as [dr Sdr].
  { intros [a b] Iq. cbn [snd]. destruct (PublicCallerKeys.selection_in _ _ _ _ _ Iq) as [_ Ib].
    destruct (Inst b Ib) as [op Lp]. rewrite installs_lookup, Lp. discriminate. }
  change (select_provisions (installs de bindings) (result_selection (map (A.rkey_of Cr) ks) None Dr)) with
    (select_provisions (installs de bindings) sel).
  rewrite Sdr. cbn [map result_environment read_result_types fresh_result_types].
  pose proof PR as PRc.
  (* the caller's keys are methods *)
  assert (KsM : forall k0, In k0 ks -> exists r m, k0 = A.mkey r m).
  { intros k0 Ik. destruct (In_nth_error _ _ Ik) as [i E].
    destruct (PublicCallerKeys.nth_error_same_length _ _ ks keys i k0 (Forall2_length Pos) E) as [key Ek].
    pose proof (PublicCallerKeys.forall2_nth _ _ _ _ _ _ _ _ Pos E Ek) as Eq. cbv beta in Eq.
    destruct (KeysM key (nth_error_In _ _ Ek)) as (r0 & m0 & a0 & _ & -> & _).
    rewrite <- (PublicCallerKeys.result_key_id W u Cf _ Cr ks k0 PRc Ik).
    destruct k0 as [?|?|?|r1 m1 a1|?]; cbn in Eq; try discriminate. exists r1, m1. reflexivity. }
  left. exists (EnvironmentWithAliases [] [] (provision_values dr)), st, H.
  split; [apply FS.CopyA.S.extends_refl|]. split; [exact St|]. split; [|reflexivity].
  split; [reflexivity|]. split; [reflexivity|]. split.
  - (* only the new methods' run-time keys *)
    intros K pv0 L.
    change (lookup_provision K (EnvironmentWithAliases [] [] (provision_values dr))) with (lookup_provision K dr) in L.
    rewrite (select_lookup _ _ _ K Sdr) in L. destruct (sm_find K sel) as [s|] eqn:Fs; [|discriminate].
    destruct (PublicCallerKeys.selection_in _ _ _ _ _ (sm_find_in _ _ _ Fs)) as [IK _]. unfold rs in IK.
    apply in_map_iff in IK as [k0 [<- Ik]]. destruct (KsM k0 Ik) as (r & m & ->). exists r, m. split; [exact Ik|reflexivity].
  - (* each a family primitive, callable at its signature *)
    intros r m g0 Ik Mg. destruct (NewSig r m g0 (conj Ik Mg)) as [_ AllSig].
    destruct (In_nth_error _ _ Ik) as [i0 E0].
    assert (R0 : nth_error rs i0 = Some (A.rkey Cr r m)) by (unfold rs; rewrite nth_error_map, E0; reflexivity).
    destruct (PublicCallerKeys.selection_at rs None Dr i0 _ R0 LenRs) as [s Fs].
    destruct (PublicCallerKeys.selection_last rs None Dr _ s Fs LenRs) as [i [Ri [Si _]]].
    cbn [closure_sources] in Si. unfold Dr in Si. rewrite nth_error_map in Si.
    destruct (nth_error keys i) as [key1|] eqn:Ek1; [|discriminate]. injection Si as Es'.
    unfold rs in Ri. rewrite nth_error_map in Ri.
    destruct (nth_error ks i) as [ki|] eqn:Ki; [|discriminate]. injection Ri as Eki.
    pose proof (PublicCallerKeys.forall2_nth _ _ _ _ _ _ _ _ Pos Ki Ek1) as Kq. cbv beta in Kq. rewrite Eki in Kq.
    destruct (Inst s ltac:(rewrite <- Es'; apply in_map; exact (nth_error_In _ _ Ek1))) as [op Lp].
    destruct (FS.CopyA.S.Forall2_In_r _ _ _ _ _ _ Binds (last_prim_in _ _ _ Lp)) as [key2 [Ik2 [op' [Eb [MP [AK _]]]]]].
    injection Eb as Es2 <-.
    assert (Ek12 : key2 = key1).
    { pose proof (PublicCallerKeys.rkey_of_inj outputs key2 key1 ltac:(rewrite <- Es2; exact (eq_sym Es'))) as Pid.
      rewrite (PublicCallerKeys.result_key_id W u Cu _ outputs keys key2 PRf Ik2),
        (PublicCallerKeys.result_key_id W u Cu _ outputs keys key1 PRf (nth_error_In _ _ Ek1)) in Pid. exact Pid. }
    subst key2.
    pose proof MP as (rf & m2 & a2 & dd & form & nm & pars & res & body & Ek2 & _). pose proof AK as AK'.
    rewrite Ek2 in AK'. destruct AK' as [gf [Mgf PS]].
    assert (Em : m2 = m).
    { rewrite Ek2 in Kq. cbn [A.rkey_of A.rkey app_subst_item] in Kq. unfold A.rkey in Kq.
      injection Kq as _ Em _. exact Em. }
    subst m2.
    pose proof (AllSig key1 rf a2 gf (nth_error_In _ _ Ek1) Ek2 Mgf Kq) as Agr.
    pose proof (prims_ground rr H de code u Cu ns outputs keys bindings FD MachG key1 op rf m a2 gf (nth_error_In _ _ Ek1)
      ltac:(rewrite <- Es2; exact (last_prim_in _ _ _ Lp)) MP AK Ek2 Mgf) as Call.
    assert (GE : forall tc tf, SigAgree Cf Cu th tc tf -> ground de tf = ground d tc).
    { intros tc tf [Sc Ec]. unfold de.
      rewrite (app_env_ground d code th d1 Cu Cu f _ _ _ FB Thf (incl_refl _) tf Sc).
      rewrite <- (Abs (app_subst th tf)), <- CanonEq, <- Ec, CanonEq, (Abs tc). reflexivity. }
    exists (primitive_closure de (A.rkey_of outputs key1) op). split.
    + change (lookup_provision (A.rkey Cr r m) (EnvironmentWithAliases [] [] (provision_values dr)))
        with (lookup_provision (A.rkey Cr r m) dr).
      rewrite (select_lookup _ _ _ _ Sdr). unfold sel. rewrite Fs, installs_lookup, Lp, Es2. reflexivity.
    + inversion Agr as [|t1 t2 l1 l2 Aret Rest]; subst.
      apply (FS.CopyA.S.callable_transfer W Prog argv read state_ok rr rr H de d (rf :: A.sg_params gf) (r :: A.sg_params g0)
        (A.sg_ret gf) (A.sg_ret g0) _ (le_n _)); [|exact (GE _ _ Aret)|exact Call].
      clear -Rest GE. induction Rest as [|t1 t2 l1 l2 A1 _ IH]; [reflexivity|]. cbn [map]. rewrite (GE _ _ A1). f_equal. exact IH.
Qed.

End FamilyPlatform.

End Platform.
End PlatformApply.
