(* Applying a platform family (T15): `bind F;` of an abstract F whose
   closure is a platform family (Platform.Family) is a safe binding step,
   PublicBodySlice's obligation PlatformApplyOk.

   The run is computed here: the application selects F's inputs, runs the
   family's code, which only installs primitive closures (BPrimitive), and
   returns them filed under the caller's run-time keys (inst_smap at θ).
   The caller's context after the application is then realized: its
   values, fns and functors are untouched, and its new methods are the
   family's primitives.

   Two premises remain, each named and stated once:
   - PlatformPrimsOk (primitive behaviour, T11/T17): a primitive the family
     installs is callable at its AtKey signature in the application's
     environment. The slice knows nothing of primitives; PublicEntry
     discharges it for the reference host (platform_prims_public).
   - PlatformResultAgrees (static, T15/T16): the caller's ProvideResult
     (read at θ) and the family's own (read at its inputs) list the same
     items: the caller's new provisions are exactly methods, each filed
     under the run-time key the family's item gets at θ, with the item's
     signature read at θ. Proving it needs a substitution lemma for the
     result readings (OutputItemsEntries, RecvTy, SigOf, AppOf) between
     the two type tables, which Audit's readings do not yet have. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicBodySlice PublicTagCheck PublicCallRealize.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

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
  - intros. constructor.
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
  intros f C ns Ctx. destruct Ctx as (d & es & tm & er & tmr & σ & D & IE & Root & MGM & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT _].
  pose proof (proj2 entries_self _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as SelfE.
  assert (SelfR : Forall FunSelf er).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI]; [constructor|exact (root_self _ _ _ _ _ _ RI IdT)]. }
  intros u fx I x t It. cbn in I. apply in_flat_map in I as [e [Ie I]].
  assert (Se : FunSelf e).
  { apply in_app_or in Ie as [Ie|Ie]; [exact (proj1 (Forall_forall _ _) SelfE e Ie)|exact (proj1 (Forall_forall _ _) SelfR e Ie)]. }
  destruct e as [t0|v t0 a|f' g a|r m g a|u' fx']; cbn in I; try contradiction; destruct I as [E|[]]; try discriminate.
  injection E as <- <-. unfold A.fixed_subst in It. apply in_map_iff in It as [[y t0] [E It]]. cbn in E.
  injection E as -> <-. cbn in Se. rewrite (Se x t0 It). reflexivity.
Qed.
End FixedSelf.

Module PlatformApply (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module S := BodySlice Prims I0.

(* ---- the family's code runs its installs ------------------------------------- *)

Definition IsPrim (b : binding) : Prop := exists k op, b = BPrimitive k op.

Fixpoint installs (d : environment) (bs : list binding) : environment :=
  match bs with
  | [] => d
  | BPrimitive k op :: r => installs (install_provision k (primitive_closure d k op) d) r
  | _ :: r => installs d r
  end.

(* The last operation installed under k. *)
Fixpoint last_prim (k : item) (bs : list binding) : option prim :=
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
  rewrite S.lookup_install. destruct (item_eqb k k1) eqn:E; [|reflexivity].
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
  S.I.eval_bindings j P d g st bs = inl (installs d bs, g, st) \/
  S.I.eval_bindings j P d g st bs = inr (S.I.Evaluation OutOfFuel g st).
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

Notation Pre := (S.Pre W Prog argv read state_ok).
Notation CallableOk := (S.CallableOk W Prog argv read state_ok).

(* A platform family, its parts named (Platform.Family's body). *)
Definition FamilyData (u code : usym) (inputs : A.cx) (needs : list item) (outputs : A.cx)
    (keys : list item) (bindings : list binding) : Prop :=
  A.FunCtx W u inputs needs /\
  (forall t, ~ A.FunOutputs W u t) /\
  A.ProvideResult W u inputs (fun t => TAbs t) outputs keys /\
  A.funbody_of Prog code = Some (FunctorCode (map fst (A.c_ty inputs)) (map fst (A.c_ty inputs))
    (map (A.rkey_of outputs) keys) bindings) /\
  Forall2 (fun key binding => exists operation,
    binding = BPrimitive (A.rkey_of outputs key) operation /\ Platform.MethodPrimitive W key operation /\
    Platform.AtKey W outputs key operation) keys bindings.

Lemma family_data : forall u code, Platform.Family W Prog u code ->
  exists inputs needs outputs keys bindings, FamilyData u code inputs needs outputs keys bindings.
Proof. intros u code (i & n & o & k & b & D). exists i, n, o, k, b. exact D. Qed.

(* The environment a family's code runs in. *)
Definition app_env (d : environment) (code : usym) (th : subst) (d1 : environment) : environment :=
  application_environment P d code th (append_environment d1 (EnvironmentWithAliases [] [] [])).

(* PREMISE (primitive behaviour, T11/T17). A primitive a platform family
   installs is callable, at its AtKey signature, in the environment the
   family's code runs in at an application. *)
Definition PlatformPrimsOk : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code, lookup_provision (IFun u) d = Some (FunctorClosure code [] (EnvironmentWithAliases [] [] [])) ->
  forall needs outputs keys bindings, FamilyData u code Cu needs outputs keys bindings ->
  forall d1, select_provisions d sm = Some d1 ->
  forall key op rf m a gf, In key keys -> In (BPrimitive (A.rkey_of outputs key) op) bindings ->
  Platform.MethodPrimitive W key op -> Platform.AtKey W outputs key op ->
  key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf ->
  CallableOk rr H (app_env d code th d1) (rf :: A.sg_params gf) (A.sg_ret gf)
    (primitive_closure (app_env d code th d1) (A.rkey_of outputs key) op).

(* A caller's signature type and a family's, read at θ. *)
Definition SigAgree (C Cu : A.cx) (th : subst) (tc tf : ty) : Prop :=
  incl (A.gens tf) (map fst (A.c_ty Cu)) /\ A.canon C tc = A.canon C (app_subst th tf).

(* PREMISE (static correspondence, T15/T16). Applying F at θ, the caller's
   result C' (ProvideResult at the caller, outputs fresh) and the family's
   own result (ProvideResult at F's inputs Cu) agree: C' adds to C's
   provisions methods only (news; the first conjunct is output_cx's shape),
   each filed under the run-time key a family item gets at θ and read with
   that item's signature at θ, and every family item is one of them. *)
Definition PlatformResultAgrees : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall code needs outputs keys bindings, FamilyData u code Cu needs outputs keys bindings ->
  exists news, A.c_provisions C' = (news ++ A.c_provisions C)%list /\
    (forall k p, In (k, p) news -> exists r m g a, k = A.mkey r m /\ p = A.ProvidedMethod g a) /\
    (forall r m g, A.mlook r m (mths_of news) = Some g ->
       (exists key, In key keys /\ app_subst_item th (A.rkey_of outputs key) = A.rkey C' r m) /\
       (forall key rf a gf, In key keys -> key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf ->
          app_subst_item th (A.rkey_of outputs key) = A.rkey C' r m ->
          Forall2 (SigAgree C Cu th) (A.sg_ret g :: r :: A.sg_params g) (A.sg_ret gf :: rf :: A.sg_params gf))) /\
    (forall key, In key keys -> exists r m g,
       A.mlook r m (mths_of news) = Some g /\ app_subst_item th (A.rkey_of outputs key) = A.rkey C' r m).

Lemma forall2_in_l : forall X Y (R : X -> Y -> Prop) l l' x, Forall2 R l l' -> In x l -> exists y, In y l' /\ R x y.
Proof.
  intros X Y R l l' x F. induction F as [|a b l l' Rab F IH]; intros I; [contradiction|].
  destruct I as [<-|I]; [exists b; split; [left; reflexivity|exact Rab]|].
  destruct (IH I) as [y [Iy Ry]]. exists y. split; [right; exact Iy|exact Ry].
Qed.

(* The family's code reads a type of its inputs as the caller's θ does. *)
Lemma app_env_ground : forall d code th d1 (Cu : A.cx) (f : nat -> ty) Dr bs,
  A.funbody_of Prog code = Some (FunctorCode (map fst (A.c_ty Cu)) (map fst (A.c_ty Cu)) Dr bs) ->
  th = map (fun q => (fst q, f (fst q))) (A.c_ty Cu) ->
  forall t, incl (A.gens t) (map fst (A.c_ty Cu)) -> ground (app_env d code th d1) t = ground d (app_subst th t).
Proof.
  intros d code th d1 Cu f Dr bs FB -> t Sc.
  unfold app_env, application_environment. change (view_functor P code) with (A.funbody_of Prog code). rewrite FB.
  cbn [fc_linked fc_inputs]. unfold ground. cbn [type_meanings].
  match goal with |- app_subst ?m t = _ => rewrite <- (PublicTypeRead.app_subst_nil (app_subst m t)) end.
  apply subst2_agree. intros x Ix. pose proof (Sc x Ix) as Kx.
  assert (E : existsb (Nat.eqb x) (map fst (A.c_ty Cu)) = true)
    by (apply existsb_exists; exists x; split; [exact Kx|apply Nat.eqb_refl]).
  cbn [app_subst]. erewrite S.lookup_map_nat by exact Kx. cbv beta.
  rewrite E, (S.lookup_on_keys f _ x Kx). apply PublicTypeRead.app_subst_nil.
Qed.

(* At an application of a platform closure, the family's code reads each
   fixed machine-type input of F as its machine type: the closure records
   it (PlatformFixed), the caller's θ reads a fixed input as it was read
   where the closure was provided (FixedMeans, the application's fixed
   agreement), and the code reads its inputs through θ. *)
Lemma platform_fixed_ground : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code, lookup_provision (IFun u) d = Some (FunctorClosure code [] (EnvironmentWithAliases [] [] [])) ->
  forall needs outputs keys bindings, FamilyData u code Cu needs outputs keys bindings ->
  forall d1 name mach x, In (name, mach) A.initial_machines -> A.LangSym W "wasm" A.NSName name x ->
  A.Fixed W u x -> In x (map fst (A.c_ty Cu)) ->
  ground (app_env d code th d1) (TAbs x) = TMachine mach.
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0 code Look
    needs outputs keys bindings FD d1 name mach x Inm Lang Fx Ix.
  pose proof AP as (R & Lf & FC & Inp & Th & Fixed & _).
  pose proof FD as (_ & _ & _ & FB & _).
  pose proof Pre0 as (_ & _ & _ & _ & [Abs [[_ [_ [_ Funs]]] _]] & _).
  destruct (Funs u fx Lf) as [pv [gfx [Look' [Ok [Cov Vm]]]]]. rewrite Look in Look'. injection Look' as <-.
  destruct (S.closure_cases _ _ _ _ _ _ _ _ _ _ Ok) as [CL|[CC|CP]].
  - destruct CL as (gg & h & Cu' & nsu & Cg & nsg & dc & E & _). injection E as _ E _. discriminate.
  - destruct CC as (h & Cu' & nsu & Ch & nsh & code' & sms & dc & gfh & E & _). injection E as _ E _.
    destruct sms; discriminate.
  - destruct CP as (code' & _ & _ & Mach).
    destruct (Cov x Fx) as [tx Itx]. pose proof (Vm x tx Itx) as Gx. rewrite (Mach name mach x Inm Lang Fx) in Gx.
    injection Gx as Gx. destruct (Fixed x tx Itx) as [here [Lh Eh]].
    rewrite (app_env_ground d code th d1 Cu (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ _ FB Th (TAbs x)
      ltac:(intros y [<-|[]]; exact Ix)).
    unfold ground. change (app_subst th (TAbs x)) with (match lookup_subst x th with Some t => t | None => TAbs x end).
    assert (Lt : lookup_subst x th = Some (A.canon C (app_subst (A.c_ty C) (TAbs x))))
      by (rewrite Th; exact (S.lookup_on_keys (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ x Ix)).
    rewrite Lt.
    change (app_subst (A.c_ty C) (TAbs x)) with (match lookup_subst x (A.c_ty C) with Some t => t | None => TAbs x end).
    rewrite lookup_subst_nlook, Lh. fold (ground d (A.canon C here)). rewrite Eh, (Abs tx). exact (eq_sym Gx).
Qed.

Hypothesis prims_ok : PlatformPrimsOk.
Hypothesis result_agrees : PlatformResultAgrees.

Theorem platform_apply_ok : S.PlatformApplyOk W Prog argv read state_ok.
Proof.
  intros k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0 code Look Fam rest.
  pose proof AP as (R & Fx & FC & Inp & Th & Fixed & Sat & FreshG & Uniq & Outs & NdO & OutsIff & PR).
  destruct (family_data u code Fam) as (Cu0 & needs & outputs & keys & bindings & FD).
  pose proof FD as (FCu & NoOut & PRf & FB & Binds).
  destruct (S.functx_unique W u Cu ns Cu0 needs FC FCu) as [<- <-].
  assert (Onil : outs = []).
  { destruct outs as [|t0 r0]; [reflexivity|]. exfalso. exact (NoOut t0 (proj1 (OutsIff t0) (or_introl eq_refl))). }
  subst outs.
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]].
  rewrite S.bindings_apply_eq.
  destruct j as [|j1].
  { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  rewrite S.apply_functor_eq, Look. change (view_functor (program_of Prog argv read) code) with (A.funbody_of Prog code).
  rewrite FB. cbn [fc_bindings fc_outputs].
  pose proof Th as Th'. subst th.
  destruct (S.static_realizes_le W Prog argv read state_ok rr rr C d H u Cu ns sm _ (le_n _) Real
    (S.functx_static W u Cu ns FC) Sat (S.apply_theta W C u Cu ns sm FC Idem Inp Sat)) as [d1 [Sel _]].
  set (th := map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu)) in *.
  rewrite Sel. cbn [adapt_provisions].
  change (application_environment (program_of Prog argv read) d code th
    (append_environment d1 (EnvironmentWithAliases [] [] []))) with (app_env d code th d1).
  set (de := app_env d code th d1).
  assert (AllPrim : Forall IsPrim bindings).
  { clear -Binds. induction Binds as [|key b keys' bs' [op [-> _]] _ IH]; constructor; [do 2 eexists; reflexivity|exact IH]. }
  destruct (installs_run (program_of Prog argv read) j1 bindings de [] st AllPrim) as [Run|Run]; rewrite Run.
  2: { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  (* the family's items: methods, each installed *)
  assert (KeysM : forall key, In key keys -> exists r m a op, key = IMth r m a /\
     In (BPrimitive (A.rkey_of outputs key) op) bindings).
  { intros key Ik. destruct (forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [Ib [op [-> [MP _]]]]].
    destruct MP as (r0 & m0 & a0 & _ & _ & _ & _ & _ & _ & -> & _). exists r0, m0, a0, op. split; [reflexivity|exact Ib]. }
  set (Dr := map (A.rkey_of outputs) keys).
  set (sm' := map (fun i => (app_subst_item th i, i)) Dr).
  assert (Filt : filter (fun i => negb (is_ty_item i)) Dr = Dr).
  { unfold Dr. clear -KeysM. induction keys as [|key r IH]; [reflexivity|]. cbn [map filter].
    destruct (KeysM key (or_introl eq_refl)) as (r0 & m0 & a0 & _ & -> & _). cbn.
    f_equal. apply IH. intros key' I. exact (KeysM key' (or_intror I)). }
  assert (Inst : forall s, In s Dr -> exists op, last_prim s bindings = Some op).
  { intros s Is. unfold Dr in Is. apply in_map_iff in Is as [key [<- Ik]].
    destruct (KeysM key Ik) as (r0 & m0 & a0 & op & _ & Ib). exact (last_prim_some _ _ _ Ib). }
  destruct (select_ok (installs de bindings) sm') as [dr Sdr].
  { intros q Iq. unfold sm' in Iq. apply in_map_iff in Iq as [s [<- Is]]. cbn [snd].
    destruct (Inst s Is) as [op Lp]. rewrite installs_lookup, Lp. discriminate. }
  replace (inst_smap (result_substitution th (map (fun t : nat => (t, gensof t)) [])) Dr) with sm'
    by (unfold inst_smap, sm'; cbn [map result_substitution app]; rewrite Filt; reflexivity).
  rewrite Sdr. cbn [map result_environment read_result_types fresh_result_types].
  left. set (d' := append_environment (EnvironmentWithAliases [] [] (provision_values dr)) d).
  exists d', g, st, H. split; [apply S.extends_refl|]. split; [apply S.meanings_grow_refl; reflexivity|].
  split; [|reflexivity].
  (* the result is filed under method keys only *)
  assert (SmKeys : forall k0 s, In (k0, s) sm' -> exists key r0 m0 a0, In key keys /\ s = A.rkey_of outputs key /\
     key = IMth r0 m0 a0 /\ k0 = app_subst_item th s).
  { intros k0 s Iq. unfold sm', Dr in Iq. apply in_map_iff in Iq as [s' [E Is]]. injection E as <- <-.
    apply in_map_iff in Is as [key [<- Ik]]. destruct (KeysM key Ik) as (r0 & m0 & a0 & _ & -> & _).
    exists (IMth r0 m0 a0), r0, m0, a0. split; [exact Ik|]. split; [reflexivity|]. split; reflexivity. }
  assert (Nm : forall k0, (forall r m th0, k0 <> IMth r m th0) -> lookup_provision k0 d' = lookup_provision k0 d).
  { intros k0 Nk. unfold d'. rewrite S.lookup_append_env.
    change (lookup_provision k0 (EnvironmentWithAliases [] [] (provision_values dr))) with (lookup_provision k0 dr).
    rewrite (select_lookup _ _ _ k0 Sdr), sm_find_none; [reflexivity|].
    intros s Iq. destruct (SmKeys k0 s Iq) as (key & r0 & m0 & a0 & _ & -> & -> & ->).
    exact (Nk _ _ _ eq_refl). }
  assert (Fd : forall k0 s, sm_find k0 sm' = Some s -> lookup_provision k0 d' = lookup_provision s (installs de bindings)).
  { intros k0 s Fs. unfold d'. rewrite S.lookup_append_env.
    change (lookup_provision k0 (EnvironmentWithAliases [] [] (provision_values dr))) with (lookup_provision k0 dr).
    rewrite (select_lookup _ _ _ k0 Sdr), Fs.
    destruct (SmKeys k0 s (sm_find_in _ _ _ Fs)) as (key & r0 & m0 & a0 & Ik & Es & _ & _).
    destruct (Inst s ltac:(rewrite Es; apply in_map, Ik)) as [op Lp]. rewrite installs_lookup, Lp. reflexivity. }
  assert (Fd0 : forall k0, sm_find k0 sm' = None -> lookup_provision k0 d' = lookup_provision k0 d).
  { intros k0 Fs. unfold d'. rewrite S.lookup_append_env.
    change (lookup_provision k0 (EnvironmentWithAliases [] [] (provision_values dr))) with (lookup_provision k0 dr).
    rewrite (select_lookup _ _ _ k0 Sdr), Fs. reflexivity. }
  (* the caller's new provisions *)
  destruct (result_agrees F C pa u fx Cu ns sm th gensof C' ks newgens [] AP code ns outputs keys bindings FD)
    as (news & Eprov & NewM & NewSig & NewAll).
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
  assert (Gt : forall pf ps, map (ground d) ps = map (ground d') ps /\ ground d pf = ground d' pf) by (split; reflexivity).
  assert (RealC' : S.Realizes W Prog argv read state_ok rr C' d' H).
  { split; [intros t; unfold A.canon; rewrite Esub; exact (Abs t)|].
    split; [|split; [intros t; unfold A.canon; rewrite Esub; exact (Idem t)|exact KM']].
    split; [|split; [|split]].
    - intros v t Found. change (A.c_vals C') with (vals_of (A.c_provisions C')) in Found.
      rewrite vals_by_ilook in Found by exact KM'. rewrite Old in Found by (intros r m E; discriminate).
      rewrite <- vals_by_ilook in Found by exact KM. destruct (Vals v t Found) as [w [L T]].
      exists w. split; [|exact T]. rewrite Nm; [exact L|intros r m th0 E; discriminate].
    - intros f g0 Found. change (A.c_fns C') with (fns_of (A.c_provisions C')) in Found.
      rewrite fns_by_ilook in Found by exact KM'. rewrite Old in Found by (intros r m E; discriminate).
      rewrite <- fns_by_ilook in Found by exact KM. destruct (Fns f g0 Found) as [pv [L Ok]].
      exists pv. split; [rewrite Nm; [exact L|intros r m th0 E; discriminate]|].
      exact (S.callable_transfer W Prog argv read state_ok rr rr H d d' _ _ _ _ pv (le_n _) eq_refl eq_refl Ok).
    - intros r m g0 Found. change (A.c_mths C') with (mths_of (A.c_provisions C')) in Found.
      rewrite mths_by_ilook in Found by exact KM'. rewrite Eprov, ilook_app in Found.
      destruct (A.ilook (A.mkey r m) news) as [p0|] eqn:In0.
      + (* a new method: a family primitive *)
        assert (Mn : A.mlook r m (mths_of news) = Some g0) by (rewrite mths_by_ilook by exact KMn; rewrite In0; exact Found).
        destruct (NewSig r m g0 Mn) as [[key0 [Ik0 Ek0]] AllSig].
        assert (Iq0 : In (A.rkey C' r m, A.rkey_of outputs key0) sm').
        { unfold sm', Dr. rewrite <- Ek0. apply (in_map (fun i => (app_subst_item th i, i))), in_map, Ik0. }
        destruct (sm_find_some _ _ _ Iq0) as [s Fs].
        destruct (SmKeys _ s (sm_find_in _ _ _ Fs)) as (key1 & r1 & m1 & a1 & Ik1 & Es & _ & Ek1).
        destruct (Inst s ltac:(rewrite Es; apply in_map, Ik1)) as [op Lp].
        destruct (S.Forall2_In_r _ _ _ _ _ _ Binds (last_prim_in _ _ _ Lp)) as [key2 [Ik2 [op' [Eb [MP AK]]]]].
        injection Eb as Es2 <-.
        pose proof MP as (rf & m2 & a2 & dd & form & nm & pars & res & body & Ek2 & _). pose proof AK as AK'.
        rewrite Ek2 in AK'. destruct AK' as [gf [Mgf PS]].
        assert (Em : m2 = m).
        { rewrite Ek1, Es2, Ek2 in *. cbn [A.rkey_of A.rkey app_subst_item] in Ek1. unfold A.rkey in Ek1.
          injection Ek1 as _ Em _. exact (eq_sym Em). }
        subst m2.
        pose proof (AllSig key2 rf a2 gf Ik2 Ek2 Mgf ltac:(rewrite <- Es2, <- Ek1; reflexivity)) as Agr.
        pose proof (prims_ok F C pa u fx Cu ns sm th gensof C' ks newgens [] AP rr d types H g st Pre0
          code Look ns outputs keys bindings FD d1 Sel key2 op rf m a2 gf Ik2 ltac:(rewrite <- Es2; exact (last_prim_in _ _ _ Lp))
          MP AK Ek2 Mgf) as Call.
        assert (GE : forall tc tf, SigAgree C Cu th tc tf -> ground de tf = ground d' tc).
        { intros tc tf [Sc Ec]. unfold de.
          rewrite (app_env_ground d code th d1 Cu (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ _ FB eq_refl tf Sc).
          change (ground d' tc) with (ground d tc). rewrite <- (Abs (app_subst th tf)), <- Ec, (Abs tc). reflexivity. }
        exists (primitive_closure de (A.rkey_of outputs key2) op). split.
        * rewrite (Fd _ s Fs), installs_lookup, Lp, Es2. reflexivity.
        * inversion Agr as [|t1 t2 l1 l2 Aret Rest]; subst.
          apply (S.callable_transfer W Prog argv read state_ok rr rr H de d' (rf :: A.sg_params gf) (r :: A.sg_params g0)
            (A.sg_ret gf) (A.sg_ret g0) _ (le_n _)); [|exact (GE _ _ Aret)|exact Call].
          clear -Rest GE. induction Rest as [|t1 t2 l1 l2 A1 _ IH]; [reflexivity|]. cbn [map]. rewrite (GE _ _ A1). f_equal. exact IH.
      + (* an old method, which no family item shadows *)
        assert (Mo : A.mlook r m (A.c_mths C) = Some g0)
          by (change (A.c_mths C) with (mths_of (A.c_provisions C)); rewrite mths_by_ilook by exact KM; exact Found).
        destruct (Mths r m g0 Mo) as [pv [L Ok]].
        assert (Rk : A.rkey C' r m = A.rkey C r m).
        { unfold A.rkey. rewrite !S.capps_ilook, Eprov, ilook_app, In0. unfold A.theta_of, A.canon. rewrite Esub. reflexivity. }
        exists pv. rewrite Rk. split.
        * rewrite Fd0; [exact L|]. apply sm_find_none. intros s Iq.
          destruct (SmKeys _ s Iq) as (key1 & r1 & m1 & a1 & Ik1 & Es & _ & Ek1).
          destruct (NewAll key1 Ik1) as (r' & m' & g' & Mn & Ek'). rewrite <- Es, <- Ek1, <- Rk in Ek'.
          unfold A.rkey in Ek'. injection Ek' as Er Emm _. subst r' m'.
          rewrite mths_by_ilook, In0 in Mn by exact KMn. discriminate.
        * exact (S.callable_transfer W Prog argv read state_ok rr rr H d d' _ _ _ _ pv (le_n _) eq_refl eq_refl Ok).
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
      unfold A.output_type_map in Lx. cbn [map app] in Lx. rewrite !S.lookup_map_snd in Lx.
      destruct (lookup_subst x (A.c_ty C)) as [t0|] eqn:L0; [|discriminate]. cbn [option_map] in Lx.
      injection Lx as <-. rewrite lookup_subst_nlook in L0.
      change (cmeaning d x = ground d (A.canon C (A.canon C t0))). rewrite !Abs. exact (Al x t0 L0). }
  split; [exact Lt|]. split; [exact St|]. split; [exact RealC'|].
  split; [|exact (proj2 Fmd)].
  apply (PublicFormation.apply_abstract_formed W C u gensof C' ks (proj1 Fmd)); [|exact PR].
  intros t Ft. exfalso. exact (NoOut t Ft).
Qed.

End Platform.
End PlatformApply.
