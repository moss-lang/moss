(* ------------------------------------------------------------------------ *)
(* D84.v — the unified design entry D84 against the Rule A calculus.         *)
(*                                                                           *)
(*  1-2. Rule 7, members of a family on demand: a call at a member key with  *)
(*     no provision in force elaborates to `bind F;` at θ read off the key, *)
(*     in a scope of its own (`on_demand`).  No new safety lemma: the       *)
(*     elaborated term is a TApplyR derivation whose extra premises the    *)
(*     family conditions discharge, plus weakening of the variable          *)
(*     environment (`on_demand_elaboration`), so `safety` covers it.  θ is  *)
(*     a function of the key on what the member mentions                    *)
(*     (`on_demand_theta_determined`).  The example applies the family      *)
(*     Arrays[Item] at Item = A and Item = B (θ ≠ id) in one well-formed    *)
(*     program (`arrays_on_demand_ok`).                                     *)
(*  3. The two counterexamples just outside rule 7's conditions: a fn in a  *)
(*     family's result (result coherence fails, the run goes wrong:         *)
(*     `family_fn_result_incoherent`), and a val bind in a family's binding *)
(*     (two placements, both well typed, one returns and one diverges:      *)
(*     `family_val_placement`) — and, a finding, a binding that binds no    *)
(*     val but applies a functor whose binding does is just as observable,  *)
(*     with D84's conditions all met (`family_apply_placement`).            *)
(*  4. Rule 3d: no per-binding widening of X — probe held_narrow in the     *)
(*     core goes wrong, and the capture clause of cap_ok is the one premise *)
(*     that refuses it (`held_narrow_rejected`).                            *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.
From NM Require Import CoreMoss.

Set Implicit Arguments.

(* ===================== 1. Variable-environment weakening ================= *)

Definition tenv_ext (G G' : tenv) : Prop :=
  forall x t, lookup_tenv x G = Some t -> lookup_tenv x G' = Some t.

Lemma tenv_ext_cons : forall G G' x t,
  tenv_ext G G' -> tenv_ext ((x, t) :: G) ((x, t) :: G').
Proof. intros G G' x t H y ty. simpl. destruct (Nat.eqb y x); auto. Qed.

Lemma has_ty_tenv_ext : forall Sg Phi G e tau,
  has_ty Sg Phi G e tau -> forall G', tenv_ext G G' -> has_ty Sg Phi G' e tau.
Proof.
  fix IH 6.
  intros Sg Phi G e tau H G' X.
  destruct H.
  - apply TVarR. apply X. assumption.
  - apply TUnitR.
  - eapply TSubR; [exact (IH _ _ _ _ _ H _ X) | assumption | assumption].
  - eapply TValR; eassumption.
  - eapply TTagR; [eassumption | eassumption | eassumption | exact (IH _ _ _ _ _ H2 _ X)].
  - eapply TNeedR; [eassumption | eassumption |].
    exact ((fix F l1 l2 (Hl : Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau)) l1 l2)
              {struct Hl} : Forall2 (fun e tau => has_ty Sg Phi G' e (app_subst (cS Phi) tau)) l1 l2 :=
              match Hl with
              | Forall2_nil _ => Forall2_nil _
              | Forall2_cons _ _ Hh Ht => Forall2_cons _ _ (IH _ _ _ _ _ Hh _ X) (F _ _ Ht)
              end) _ _ H1).
  - eapply TCallR; [eassumption | eassumption | eassumption | eassumption |].
    exact ((fix F l1 l2 (Hl : Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau))) l1 l2)
              {struct Hl} : Forall2 (fun e tau => has_ty Sg Phi G' e (app_subst (cS Phi) (app_subst th tau))) l1 l2 :=
              match Hl with
              | Forall2_nil _ => Forall2_nil _
              | Forall2_cons _ _ Hh Ht => Forall2_cons _ _ (IH _ _ _ _ _ Hh _ X) (F _ _ Ht)
              end) _ _ H3).
  - eapply TMethR; [eassumption | exact (IH _ _ _ _ _ H0 _ X) | eassumption | eassumption
                   | eassumption |].
    exact ((fix F l1 l2 (Hl : Forall2 (fun e tau => has_ty Sg Phi G e
                              (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau)) l1 l2)
              {struct Hl} : Forall2 (fun e tau => has_ty Sg Phi G' e
                              (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau)) l1 l2 :=
              match Hl with
              | Forall2_nil _ => Forall2_nil _
              | Forall2_cons _ _ Hh Ht => Forall2_cons _ _ (IH _ _ _ _ _ Hh _ X) (F _ _ Ht)
              end) _ _ H4).
  - eapply TMatchR; [exact (IH _ _ _ _ _ H _ X) | eassumption | eassumption | eassumption
                    | eassumption |].
    exact ((fix F l1 l2 (Hl : Forall2 (fun mem arm => has_ty Sg Phi
                              ((arm_var arm, member_payload Sg mem) :: G) (arm_body arm) tau) l1 l2)
              {struct Hl} : Forall2 (fun mem arm => has_ty Sg Phi
                              ((arm_var arm, member_payload Sg mem) :: G') (arm_body arm) tau) l1 l2 :=
              match Hl with
              | Forall2_nil _ => Forall2_nil _
              | Forall2_cons _ _ Hh Ht =>
                  Forall2_cons _ _ (IH _ _ _ _ _ Hh _ (tenv_ext_cons _ _ X)) (F _ _ Ht)
              end) _ _ H4).
  - eapply TLetR; [exact (IH _ _ _ _ _ H _ X) | exact (IH _ _ _ _ _ H0 _ (tenv_ext_cons _ _ X))].
  - eapply TBindTyR; [eassumption | eassumption | eassumption | eassumption
                     | exact (IH _ _ _ _ _ H3 _ X) | eassumption].
  - eapply TBindValR; [eassumption | eassumption | exact (IH _ _ _ _ _ H1 _ X)
                      | exact (IH _ _ _ _ _ H2 _ X)].
  - eapply TBindFnR; [eassumption | eassumption | eassumption | eassumption | eassumption
                     | eassumption | exact (IH _ _ _ _ _ H5 _ X)].
  - eapply TBindMthR; [eassumption | eassumption | eassumption | eassumption | eassumption
                      | eassumption | eassumption | eassumption | eassumption | eassumption
                      | eassumption | exact (IH _ _ _ _ _ H10 _ X)].
  - eapply TBindFunR; [eassumption | eassumption | eassumption | eassumption | eassumption
                      | exact (IH _ _ _ _ _ H4 _ X)].
  - eapply TBindFunAR; [eassumption | eassumption | eassumption | eassumption | eassumption
                       | eassumption | exact (IH _ _ _ _ _ H5 _ X)].
  - eapply TApplyR; [eassumption | eassumption | eassumption | eassumption | eassumption
                    | eassumption | eassumption | eassumption | exact (IH _ _ _ _ _ H7 _ X)
                    | eassumption].
Qed.

(* ===================== 2. Rule 7: members of a family on demand ========== *)

(* The elaboration of a method call `e0.m(args)` whose key is a member of a
   family φ with no provision in force: the receiver and the arguments are
   evaluated where the call is (bound to fresh variables), and the call
   itself runs in a scope of its own opened by `bind φ;` at θ — P(φ) read
   off the key, every other input as it is here. *)
Fixpoint lets (xs : list var) (es : list expr) (body : expr) : expr :=
  match xs, es with
  | x :: xr, e :: er => ELet x e (lets xr er body)
  | _, _ => body
  end.

Definition on_demand (u : usym) (th : subst) (sm : smap) (x0 : var) (e0 : expr)
    (m : msym) (prov : item) (xs : list var) (args : list expr) : expr :=
  ELet x0 e0 (lets xs args (EApply u th sm (EMeth (EVar x0) m prov (map EVar xs)))).

Lemma lookup_tenv_app_in : forall L G x t,
  NoDup (map fst L) -> In (x, t) L -> lookup_tenv x (L ++ G) = Some t.
Proof.
  induction L as [|[y s] L IH]; simpl; intros G x t Hnd Hin; [destruct Hin |].
  inversion Hnd as [| ? ? Hy Hnd']; subst.
  destruct Hin as [Heq | Hin].
  - injection Heq as <- <-. rewrite Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E. subst y. exfalso. apply Hy.
      apply in_map_iff. exists (x, t). split; [reflexivity | exact Hin].
    + apply IH; assumption.
Qed.

Lemma lookup_tenv_app_notin : forall L G x,
  ~ In x (map fst L) -> lookup_tenv x (L ++ G) = lookup_tenv x G.
Proof.
  induction L as [|[y s] L IH]; simpl; intros G x Hn; [reflexivity |].
  destruct (Nat.eqb x y) eqn:E.
  - apply Nat.eqb_eq in E. subst. exfalso. apply Hn. left. reflexivity.
  - apply IH. intros H. apply Hn. right. exact H.
Qed.

Lemma map_fst_combine : forall (A B : Type) (xs : list A) (ys : list B),
  length xs = length ys -> map fst (combine xs ys) = xs.
Proof.
  induction xs as [|x xs IH]; intros [|y ys] H; simpl in *; try discriminate; [reflexivity |].
  f_equal. apply IH. lia.
Qed.

Lemma lets_typed : forall Sg Phi xs args taus G body tau,
  Forall2 (has_ty Sg Phi G) args taus ->
  length xs = length args ->
  NoDup xs -> Forall (fun x => lookup_tenv x G = None) xs ->
  has_ty Sg Phi (rev (combine xs taus) ++ G) body tau ->
  has_ty Sg Phi G (lets xs args body) tau.
Proof.
  induction xs as [|x xs IH]; intros args taus G body tau Hargs Hlen Hnd Hfr Hb.
  - destruct args; [| discriminate]. inversion Hargs; subst. exact Hb.
  - destruct args as [|a args]; [discriminate |]. inversion Hargs as [| ? t ? taus' Ha Hr]; subst.
    inversion Hnd as [| ? ? Hx Hnd']; subst. inversion Hfr as [| ? ? HxG Hfr']; subst.
    simpl. eapply TLetR; [exact Ha |].
    assert (X : tenv_ext G ((x, t) :: G)).
    { intros y ty Hy. simpl. destruct (Nat.eqb y x) eqn:E; [| exact Hy].
      apply Nat.eqb_eq in E. subst. rewrite HxG in Hy. discriminate. }
    apply IH with (taus := taus').
    + clear -Hr X. induction Hr; constructor; [exact (has_ty_tenv_ext H X) | exact IHHr].
    + simpl in Hlen. lia.
    + exact Hnd'.
    + apply Forall_forall. intros y Hy. simpl.
      destruct (Nat.eqb y x) eqn:E.
      * apply Nat.eqb_eq in E. subst. contradiction.
      * rewrite Forall_forall in Hfr'. exact (Hfr' y Hy).
    + simpl in Hb. rewrite <- app_assoc in Hb. exact Hb.
Qed.

(* No new safety lemma: the elaborated call is a TApplyR derivation whose
   premises rule 7's conditions discharge — `fixed_ok` because θ is the
   identity off P(φ) and X(φ) = M(φ) ∖ P(φ) (rule 1), `res_coherent` and
   the freshness and escape premises because the result is methods only.
   What the call needs beyond the surface check is weakening: the receiver
   and arguments, checked where the call is, still type after the fresh
   variables are bound (has_ty_tenv_ext).  The two hypotheses on the key
   are rule 7's "no provision of it is in force" (with "a member of a family
   in scope cannot be bound directly"), and that the family's own members do
   not collide at the key under θ (a condition D84 leaves implicit). *)
Theorem on_demand_elaboration : forall Sg, wf_d84 Sg ->
  forall Phi G u Du Dr th sm i rt0 m th0 Dm S e0 tau0 x0 xs args,
    family Sg u ->
    g_fun Sg u = Some (Du, Dr, None) ->
    avail Phi (IFun u) ->
    (* θ: P(φ) read off the key, every other input as it is here *)
    sat Sg Phi th Du ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Du ->
    (forall x, ~ In x (g_poly Sg u) -> app_subst th (TAbs x) = TAbs x) ->
    (* the call's key is a member of φ's result, read at θ *)
    In i Dr -> app_subst_item th i = IMth rt0 m th0 ->
    g_mth Sg m = Some (Dm, S) ->
    has_ty Sg Phi G e0 tau0 ->
    app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
    (* no provision of the key is in force *)
    (forall i1, In i1 (cA Phi) -> ~ is_prov_at (cS Phi) tau0 m i1) ->
    (* φ's members do not collide at the key *)
    (forall j, In j Dr -> is_prov_at (cS Phi) tau0 m (app_subst_item th j) ->
       app_subst_item (cS Phi) (app_subst_item th j)
       = app_subst_item (cS Phi) (IMth rt0 m th0)) ->
    Forall2 (fun e tau =>
               has_ty Sg Phi G e (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params S) ->
    (* the elaboration's variables are fresh *)
    length xs = length args ->
    NoDup (x0 :: xs) ->
    Forall (fun x => lookup_tenv x G = None) (x0 :: xs) ->
    has_ty Sg Phi G (on_demand u th sm x0 e0 m (IMth rt0 m th0) xs args)
           (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S)).
Proof.
  intros Sg W Phi G u Du Dr th sm i rt0 m th0 Dm S e0 tau0 x0 xs args
         [Du' [Dr' [Hu' [Hmeth Hkey]]]] Hu Hav Hsat Hwth Hsm Hid Hi Hith Hm He0 Hrt
         Hnone Hcoll Hargs Hlen Hnd Hfr.
  rewrite Hu in Hu'. injection Hu' as <- <-. clear Hkey.
  inversion Hnd as [| ? ? Hx0 Hndx]; subst. inversion Hfr as [| ? ? Hx0G Hfrx]; subst.
  set (taus := map (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0)) (fs_params S)).
  assert (Hlt : length xs = length taus).
  { unfold taus. rewrite length_map. rewrite Hlen. exact (Forall2_length Hargs). }
  assert (Hnd2 : NoDup (map fst (rev (combine xs taus)))).
  { rewrite map_rev, (map_fst_combine _ _ Hlt). apply NoDup_rev. exact Hndx. }
  (* the result is methods only: no type items, no dependencies *)
  assert (Hty : tyreqs Dr = []).
  { clear -Hmeth. induction Hmeth as [| i' D [rt [m' [th' ->]]] _ IH]; [reflexivity | exact IH]. }
  unfold on_demand. eapply TLetR; [exact He0 |].
  assert (X0 : tenv_ext G ((x0, tau0) :: G)).
  { intros y ty Hy. simpl. destruct (Nat.eqb y x0) eqn:E; [| exact Hy].
    apply Nat.eqb_eq in E. subst. rewrite Hx0G in Hy. discriminate. }
  apply lets_typed with (taus := taus).
  - unfold taus. clear -Hargs X0. induction Hargs; simpl; constructor;
      [exact (has_ty_tenv_ext H X0) | exact IHHargs].
  - exact Hlen.
  - exact Hndx.
  - apply Forall_forall. intros y Hy. simpl.
    destruct (Nat.eqb y x0) eqn:E.
    + apply Nat.eqb_eq in E. subst. contradiction.
    + rewrite Forall_forall in Hfrx. exact (Hfrx y Hy).
  - eapply TApplyR with (Du := Du) (Dr := Dr);
      [exact Hu | exact Hav | exact Hsat | exact Hwth | exact Hsm | | | | | ].
    + (* fixed_ok: θ is the identity on X(φ), which is disjoint from P(φ) *)
      unfold fixed_ok. apply Forall_forall. intros x Hx.
      destruct (d84_fixed W _ Hu) as [_ HX].
      destruct (proj1 (HX x) Hx) as [_ HnP]. rewrite (Hid x HnP). reflexivity.
    + (* res_coherent: a method item depends on no symbol *)
      unfold res_coherent. apply Forall_forall. intros j Hj.
      rewrite Forall_forall in Hmeth. destruct (Hmeth j Hj) as [rt [m' [th' ->]]].
      constructor.
    + rewrite Hty. constructor.
    + match goal with |- has_ty _ ?P _ _ _ =>
        replace (cS Phi) with (cS P) by reflexivity end.
      eapply TMethR with (Dm := Dm) (S := S) (tau0 := tau0).
      * exact Hm.
      * apply TVarR. simpl.
        rewrite lookup_tenv_app_notin; [simpl; rewrite Nat.eqb_refl; reflexivity |].
        rewrite map_rev, (map_fst_combine _ _ Hlt). rewrite <- in_rev. exact Hx0.
      * simpl. apply in_or_app. left. rewrite <- Hith. apply in_app_tele. exact Hi.
      * exact Hrt.
      * intros i1 Hi1 Hp. simpl in Hi1. apply in_app_or in Hi1. destruct Hi1 as [Hi1 | Hi1].
        -- unfold app_tele in Hi1. apply in_map_iff in Hi1. destruct Hi1 as [j [<- Hj]].
           exact (Hcoll j Hj Hp).
        -- exfalso. exact (Hnone i1 Hi1 Hp).
      * simpl. assert (Hlk : forall x t, In (x, t) (combine xs taus) ->
                  lookup_tenv x (rev (combine xs taus) ++ (x0, tau0) :: G) = Some t).
        { intros x t Hin. apply lookup_tenv_app_in; [exact Hnd2 | rewrite <- in_rev; exact Hin]. }
        clear -Hlk Hlt. unfold taus in *.
        revert Hlk Hlt. generalize (rev (combine xs (map (interp_m (cS Phi) th0
                          (app_subst (cS Phi) tau0)) (fs_params S))) ++ (x0, tau0) :: G).
        intros G2. revert xs. induction (fs_params S) as [| p ps IH]; intros [| x xs] Hlk Hlt;
          simpl in Hlt; try discriminate; simpl; constructor.
        -- apply TVarR. apply Hlk. left. reflexivity.
        -- apply IH; [| lia]. intros y t Hin. apply Hlk. right. exact Hin.
    + rewrite Hty. constructor.
Qed.

(* θ is read off the key: two instantiations that agree on a member's key
   agree on every bracketed symbol the member mentions, so the elaborated
   call — the member's signature at θ — is a function of the key (rule 7:
   "it searches nothing").  This is what the family condition "each item's
   key determines every P(φ) symbol it mentions" is for. *)
Lemma map_eq_in : forall (A B : Type) (f g : A -> B) l a,
  map f l = map g l -> In a l -> f a = g a.
Proof.
  intros A B f g l a. induction l as [|b l IH]; simpl; intros H Hin; [destruct Hin |].
  injection H as H1 H2. destruct Hin as [-> | Hin]; [exact H1 | exact (IH H2 Hin)].
Qed.

Lemma app_subst_eq_frees : forall s1 s2 t x,
  app_subst s1 t = app_subst s2 t -> In x (ty_frees t) ->
  app_subst s1 (TAbs x) = app_subst s2 (TAbs x).
Proof.
  intros s1 s2 t. induction t as [| a | n th IH | ms IH] using ty_ind'; intros x Heq Hx.
  - destruct Hx.
  - destruct Hx as [<- | []]. exact Heq.
  - rewrite !app_subst_nom in Heq. injection Heq as Heq.
    destruct (wf_ty_frees_aux1 _ _ _ Hx) as [p [Hp Hxp]].
    rewrite Forall_forall in IH.
    pose proof (map_eq_in _ _ _ _ Heq Hp) as E. simpl in E. injection E as E.
    exact (IH p Hp x E Hxp).
  - rewrite !app_subst_union in Heq. injection Heq as Heq.
    destruct (wf_ty_frees_aux2 _ _ Hx) as [t [Ht Hxt]].
    rewrite Forall_forall in IH. apply (IH t Ht x); [| exact Hxt].
    exact (map_eq_in _ _ _ _ Heq Ht).
Qed.

Theorem on_demand_theta_determined : forall Sg u Du Dr i th th',
  family Sg u -> g_fun Sg u = Some (Du, Dr, None) -> In i Dr ->
  app_subst_item th i = app_subst_item th' i ->
  forall x, In x (g_poly Sg u) -> In x (res_reads Sg [i]) ->
  app_subst th (TAbs x) = app_subst th' (TAbs x).
Proof.
  intros Sg u Du Dr i th th' [Du' [Dr' [Hu' [Hmeth Hkey]]]] Hu Hi Heq x HxP Hxr.
  rewrite Hu in Hu'. injection Hu' as <- <-.
  rewrite Forall_forall in Hmeth, Hkey.
  pose proof (Hkey i Hi x HxP Hxr) as Hx.
  destruct (Hmeth i Hi) as [rt [m [thi ->]]].
  simpl in Heq. injection Heq as Hrt Hth. simpl in Hx.
  apply in_app_or in Hx. destruct Hx as [Hx | Hx].
  - exact (@app_subst_eq_frees th th' rt x Hrt Hx).
  - apply in_flat_map in Hx. destruct Hx as [p [Hp Hxp]].
    pose proof (map_eq_in _ _ _ _ Hth Hp) as E. simpl in E. injection E as E.
    exact (@app_subst_eq_frees th th' (snd p) x E Hxp).
Qed.

(* ---- The θ ≠ id example: ArraysExample on demand. -------------------------
   ArraysExample's program with neither `use` nor any `bind Arrays;`: main
   binds Int = Num, the platform idx, and Arrays = Gc, and then calls
   A.get(Num) and B.get(Num).  Neither key is provided, both are members of
   the family Arrays[Item], so each call elaborates (rule 7) to `bind
   Arrays;` at Item = A, resp. Item = B, read off the key, Int as it is —
   stale_twin_rejected's program without its Int rebind. *)
Module ArraysOnDemand.
  Import ArraysExample.

  Definition th_at (n : nsym) : subst := [(sInt, TAbs sInt); (sItem, TNom n [])].
  Definition key_at (n : nsym) : item := IMth (TNom n []) mGet [(sI, TAbs sInt)].

  Definition od (n : nsym) : expr :=
    on_demand uArrays (th_at n) [] 20 (ETag n [] EUnit) mGet (key_at n)
              [21] [ETag nNum [] EUnit].

  Definition od_body : expr := prelude (ELet 10 (od nA) (od nB)).
  Definition SgOD : gsig := SgOf od_body.

  Example key_is_member : forall n, app_subst_item (th_at n) get_item = key_at n.
  Proof. reflexivity. Qed.

  Example runs : interp 1000 SgOD [] [] od_body = Ok (VTag nB VUnit).
  Proof. vm_compute. reflexivity. Qed.

  Example erased_runs : interp 1000 SgOD [] [] (erase od_body) = Ok (VTag nB VUnit).
  Proof. vm_compute. reflexivity. Qed.

  Lemma d84_of : forall mainb, wf_d84 (SgOf mainb).
  Proof.
    intros mainb. constructor.
    - intros u Du Dr H.
      destruct u as [|[|[|u]]]; simpl in H; try discriminate; injection H as <- <-.
      split; [intros x [<- | []]; simpl; tauto |].
      intros x. vm_compute. intuition (subst; discriminate).
    - intros u Du Dr bs H.
      destruct u as [|[|[|u]]]; simpl in H; try discriminate; split; reflexivity.
    - intros u Du Dr h Dh bs _ Hu Hh.
      destruct h as [|[|[|h]]]; simpl in Hh; try discriminate.
      injection Hh as <- <- <-. repeat constructor.
  Qed.

  Lemma family_arrays : forall mainb, family (SgOf mainb) uArrays.
  Proof.
    intros mainb. exists [ITy sInt; ITy sItem], arr_res. split; [reflexivity |].
    split.
    - constructor; [| constructor]. do 3 eexists. reflexivity.
    - constructor; [| constructor]. intros x [<- | []] _. simpl. tauto.
  Qed.

  Definition Phi0 : ctx := add_item (IFun uArrays) PhiBind.

  Lemma od_typed : forall G n, 3 <= n <= 4 ->
    lookup_tenv 20 G = None -> lookup_tenv 21 G = None ->
    has_ty SgOD Phi0 G (od n) (TNom n []).
  Proof.
    intros G n Hn H20 H21.
    eapply conv.
    - eapply (@on_demand_elaboration SgOD (d84_of od_body) Phi0 G uArrays
                [ITy sInt; ITy sItem] arr_res (th_at n) [] get_item (TNom n []) mGet
                [(sI, TAbs sInt)] [ITy sI] (mkSig [TAbs sI] (TAbs this_sym))
                (ETag n [] EUnit) (TNom n []) 20 [21] [ETag nNum [] EUnit]).
      + apply family_arrays.
      + reflexivity.
      + reflexivity.
      + exists PhiArr. split; [reflexivity |]. split; [reflexivity |].
        split; [repeat constructor; left; reflexivity | constructor].
      + constructor; [apply WfNeed; reflexivity |].
        constructor; [apply wf_tag0; lia | constructor].
      + exists PhiArr. split; [reflexivity | split; [reflexivity | constructor]].
      + intros x Hx. simpl.
        destruct (Nat.eqb x sInt) eqn:E1; [apply Nat.eqb_eq in E1; subst; reflexivity |].
        destruct (Nat.eqb x sItem) eqn:E2; [| reflexivity].
        apply Nat.eqb_eq in E2. subst. exfalso. apply Hx. simpl. left. reflexivity.
      + left. reflexivity.
      + reflexivity.
      + reflexivity.
      + apply tag0. lia.
      + reflexivity.
      + intros i1 Hi1 [th' Hp]. simpl in Hi1.
        destruct Hi1 as [<- | [<- | [<- | []]]]; discriminate.
      + intros j [<- | []] _. reflexivity.
      + constructor; [| constructor]. eapply conv; [apply tag0; unfold nNum; lia | reflexivity].
      + reflexivity.
      + constructor; [simpl; intros [H | []]; discriminate | repeat constructor; simpl; tauto].
      + constructor; [exact H20 | constructor; [exact H21 | constructor]].
    - reflexivity.
  Qed.

  Lemma main_od_typed : has_ty SgOD empty_ctx (body_env (mkSig [] TUnit))
                          (ELet 11 od_body EUnit) TUnit.
  Proof.
    eapply TLetR with (tau1 := TNom nB []); [| constructor].
    apply prelude_typed; [| apply wf_tag0; unfold nB; lia].
    eapply TLetR; [apply od_typed; [unfold nA; lia | reflexivity | reflexivity] |].
    apply od_typed; [unfold nB; lia | reflexivity | reflexivity].
  Qed.
End ArraysOnDemand.

Theorem arrays_on_demand_ok :
  wf_prog ArraysOnDemand.SgOD ArraysExample.fMain
  /\ wf_d84 ArraysOnDemand.SgOD
  /\ interp 1000 ArraysOnDemand.SgOD [] [] ArraysOnDemand.od_body
     = Ok (VTag ArraysExample.nB VUnit).
Proof.
  split; [split; [exact (ArraysExample.wf_sig_of ArraysOnDemand.main_od_typed)
                 | eexists; reflexivity] |].
  split; [apply ArraysOnDemand.d84_of | exact ArraysOnDemand.runs].
Qed.

Corollary arrays_on_demand_safe : forall fuel,
  interp fuel ArraysOnDemand.SgOD [] [] (ELet 11 ArraysOnDemand.od_body EUnit) <> Err.
Proof.
  intros fuel. exact (proj1 (soundness_main fuel (proj1 arrays_on_demand_ok) eq_refl)).
Qed.

(* ===================== 3. Just outside rule 7's conditions =============== *)

(* (a) A fn in a family's result.  φ[Item]: Item, K.mk[Q = Item] -> mkf,
   with `fn mkf(): Item` — an `int_list()`-style constructor, so φ is not a
   family (its result is not methods only).  Applied on demand at a key that
   reads Item = A where Item is bound to B, every premise of the application
   but result coherence holds, the scope reads mkf() as a B, and the run
   goes wrong: the shape of res_coherence_needed, for a fn.  A method item
   names its instantiation in its key, which is why rule 7 admits only
   methods. *)
Module FamilyFn.
  Definition sItem : tsym := 1.
  Definition sQ : tsym := 2.
  Definition nK : nsym := 1.
  Definition nA : nsym := 2.
  Definition nB : nsym := 3.
  Definition fMkf : fsym := 1.
  Definition fMkA : fsym := 2.
  Definition fProv : fsym := 3.
  Definition fMain : fsym := 4.
  Definition mMk : msym := 1.
  Definition uF : usym := 1.
  Definition uH : usym := 2.

  Definition kty : ty := TNom nK [].
  Definition mk_item : item := IMth kty mMk [(sQ, TAbs sItem)].
  Definition th : subst := [(sItem, TNom nA [])].
  Definition Du : tele := [ITy sItem; mk_item].
  Definition Dr : tele := [IFn fMkf].
  Definition sm : smap := [(mk_item, app_subst_item th mk_item)].

  Definition scope (n : nsym) : expr := EMatch (ECallA fMkf []) [(n, 1, EUnit)].

  Definition main_with (n : nsym) : expr :=
    EBindTy sItem (TNom nB [])
    (EBindMth kty mMk [(sQ, TNom nA [])] fMkA []
    (EBindFun uF uH [(mk_item, mk_item)] []
    (EApply uF th sm (scope n)))).

  Definition main_body : expr := main_with nB.

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sItem || Nat.eqb t sQ || Nat.eqb t this_sym)
    (fun n => if Nat.leb 1 n && Nat.leb n 3 then Some ([], TUnit) else None)
    (fun _ => None)
    (fun f => if Nat.eqb f fMkf then Some ([ITy sItem], mkSig [] (TAbs sItem), None)
              else if Nat.eqb f fMkA then
                Some ([], mkSig [kty] (TNom nA []), Some (ETag nA [] EUnit))
              else if Nat.eqb f fProv then
                Some (Du, mkSig [] (TAbs sItem),
                      Some (EMeth (ETag nK [] EUnit) mMk mk_item []))
              else if Nat.eqb f fMain then
                Some ([], mkSig [] TUnit, Some (ELet 9 main_body EUnit))
              else None)
    (fun m => if Nat.eqb m mMk then Some ([ITy sQ], mkSig [] (TAbs sQ)) else None)
    (fun u => if Nat.eqb u uF then Some (Du, Dr, None)
              else if Nat.eqb u uH then
                Some (Du, Dr, Some [BFn fMkf fProv [(mk_item, mk_item)]])
              else None)
    (fun _ => []) (fun u => if Nat.eqb u uF then [sItem] else [])
    (fun _ => ([], [])).

  Example goes_wrong : interp 100 Sg [] [] main_body = Err.
  Proof. vm_compute. reflexivity. Qed.

  (* Control: read as the A it is, the same run succeeds. *)
  Example control : interp 100 Sg [] [] (main_with nA) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  Definition PhiAt : ctx :=
    add_item (IFun uF) (add_item (IMth kty mMk [(sQ, TNom nA [])])
                                 (bind_ty sItem (TNom nB []) empty_ctx)).

  Lemma premises : sat Sg PhiAt th Du
    /\ smap_ok PhiAt th sm Du
    /\ fixed_ok Sg PhiAt uF th
    /\ ~ res_coherent Sg PhiAt th Dr.
  Proof.
    split; [| split; [| split]].
    - exists (mkCtx Du []). split; [reflexivity |]. split; [reflexivity |].
      split; [| constructor].
      constructor; [left; reflexivity |]. constructor; [| constructor].
      right. split; [reflexivity | constructor].
    - exists (mkCtx Du []). split; [reflexivity | split; [reflexivity |]].
      constructor; [| constructor]. split; [simpl; tauto | reflexivity].
    - constructor.
    - unfold res_coherent. intros H. inversion H as [| i l Hi _]. inversion Hi as [| x l' Hx _].
      vm_compute in Hx. discriminate Hx.
  Qed.

  Lemma not_family : ~ family Sg uF.
  Proof.
    intros [Du' [Dr' [Hu [Hm _]]]]. simpl in Hu. injection Hu as <- <-.
    inversion Hm as [| i l [rt [m [th' Heq]]] _]. discriminate Heq.
  Qed.
End FamilyFn.

Theorem family_fn_result_incoherent :
  interp 100 FamilyFn.Sg [] [] FamilyFn.main_body = Err
  /\ sat FamilyFn.Sg FamilyFn.PhiAt FamilyFn.th FamilyFn.Du
  /\ smap_ok FamilyFn.PhiAt FamilyFn.th FamilyFn.sm FamilyFn.Du
  /\ fixed_ok FamilyFn.Sg FamilyFn.PhiAt FamilyFn.uF FamilyFn.th
  /\ ~ res_coherent FamilyFn.Sg FamilyFn.PhiAt FamilyFn.th FamilyFn.Dr
  /\ ~ family FamilyFn.Sg FamilyFn.uF
  /\ ~ wf_prog FamilyFn.Sg FamilyFn.fMain.
Proof.
  destruct FamilyFn.premises as [H1 [H2 [H3 H4]]].
  split; [exact FamilyFn.goes_wrong |]. split; [exact H1 |]. split; [exact H2 |].
  split; [exact H3 |]. split; [exact H4 |]. split; [exact FamilyFn.not_family |].
  intros Hwf. apply (proj1 (soundness_main 101 Hwf eq_refl)). vm_compute. reflexivity.
Qed.

(* (b) A val bind in a family's binding.  The family Ms[]: -> K.m (methods
   only, no brackets) is bound to H, whose body evaluates a val first —
   `Countdown`-style; here the val's expression runs forever.  The surface
   program `bind Ms = H; match A { A => (), B => K.m() }` needs the member
   only in the arm not taken.  Rule 7 places the application in a scope of
   its own around the call (`placed`); placing it anywhere else that covers
   the call — here around the match (`hoisted`) — is just as well typed,
   and the two runs differ: one returns, the other never does.  So where an
   elaborator puts the application is observable as soon as a family's
   binding binds a val, which is what rule 7's "bindings bind no vals"
   excludes (`d84_family_binding`). *)
Module FamilyVal.
  Definition nK : nsym := 1.
  Definition nA : nsym := 2.
  Definition nB : nsym := 3.
  Definition vV : vsym := 1.
  Definition fLoop : fsym := 1.
  Definition fProv : fsym := 2.
  Definition fMain : fsym := 3.
  Definition mM : msym := 1.
  Definition uMs : usym := 1.
  Definition uH : usym := 2.

  Definition kty : ty := TNom nK [].
  Definition uty : ty := TUnion [TNom nA []; TNom nB []].
  Definition key : item := IMth kty mM [].
  Definition loop : expr := ECallD fLoop [] [] [].
  Definition h_body : list bnd := [BVal vV loop; BMth kty mM [] fProv []].

  Definition call : expr := EMeth (ETag nK [] EUnit) mM key [].
  Definition scrut : expr := ETag nA [] EUnit.

  Definition placed : expr :=
    EBindFun uMs uH [] []
      (EMatch scrut [(nA, 1, EUnit); (nB, 1, EApply uMs [] [] call)]).
  Definition hoisted : expr :=
    EBindFun uMs uH [] []
      (EApply uMs [] [] (EMatch scrut [(nA, 1, EUnit); (nB, 1, call)])).

  Definition SgOf (mainb : expr) : gsig := mkGsig
    (fun t => Nat.eqb t this_sym)
    (fun n => if Nat.leb 1 n && Nat.leb n 3 then Some ([], TUnit) else None)
    (fun v => if Nat.eqb v vV then Some ([], TUnit) else None)
    (fun f => if Nat.eqb f fLoop then Some ([], mkSig [] TUnit, Some loop)
              else if Nat.eqb f fProv then Some ([], mkSig [kty] TUnit, Some EUnit)
              else if Nat.eqb f fMain then Some ([], mkSig [] TUnit, Some (ELet 9 mainb EUnit))
              else None)
    (fun m => if Nat.eqb m mM then Some ([], mkSig [] TUnit) else None)
    (fun u => if Nat.eqb u uMs then Some ([], [key], None)
              else if Nat.eqb u uH then Some ([], [key], Some h_body)
              else None)
    (fun _ => []) (fun _ => []) (fun _ => ([], [])).

  Lemma loop_diverges : forall mainb n d g, interp n (SgOf mainb) d g loop = OutOfFuel.
  Proof.
    intros mainb n. induction n as [| n IH]; intros d g; [reflexivity |].
    destruct n as [| n]; [reflexivity |].
    cbn -[interp]. exact (IH [] []).
  Qed.

  Example placed_returns : forall mainb, interp 100 (SgOf mainb) [] [] placed = Ok VUnit.
  Proof. intros mainb. reflexivity. Qed.

  Lemma hoisted_diverges : forall mainb n, interp n (SgOf mainb) [] [] hoisted = OutOfFuel.
  Proof.
    intros mainb n.
    destruct n as [|[|[|[|n]]]]; try reflexivity.
    unfold hoisted. simpl. rewrite loop_diverges. reflexivity.
  Qed.
  (* ---- Both placements are well typed, in a well-formed program. -------- *)
  Lemma conv : forall S0 Phi G e t t', has_ty S0 Phi G e t -> t = t' -> has_ty S0 Phi G e t'.
  Proof. intros; subst; assumption. Qed.

  Section Wf.
    Variable mainb : expr.
    Let S0 := SgOf mainb.

    Lemma sat_nil : forall Phi, sat S0 Phi [] [].
    Proof.
      intros Phi. exists empty_ctx. split; [reflexivity |].
      split; [reflexivity | split; constructor].
    Qed.

    Lemma sat0_nil : forall Phi, sat0 S0 Phi [].
    Proof.
      intros Phi. exists empty_ctx. split; [reflexivity |].
      split; [split; [reflexivity | split; constructor] | constructor].
    Qed.

    Lemma smap_nil : forall Phi th, smap_ok Phi th [] [].
    Proof. intros Phi th. exists empty_ctx. split; [reflexivity | split; [reflexivity | constructor]]. Qed.

    Lemma wf_tag : forall Phi n, 1 <= n <= 3 -> wf_ty S0 Phi (TNom n []).
    Proof.
      intros Phi n Hn. eapply WfTag; [| apply sat_nil | constructor].
      destruct n as [|[|[|[|n]]]]; try lia; reflexivity.
    Qed.

    Lemma tag_typed : forall Phi G n, 1 <= n <= 3 ->
      has_ty S0 Phi G (ETag n [] EUnit) (app_subst (cS Phi) (TNom n [])).
    Proof.
      intros Phi G n Hn. eapply TTagR with (D := []) (tau0 := TUnit);
        [| apply sat_nil | constructor | apply conv with (t := TUnit); [constructor | reflexivity]].
      destruct n as [|[|[|[|n]]]]; try lia; reflexivity.
    Qed.

    Definition Phi1 : ctx := add_item (IFun uMs) empty_ctx.

    Lemma wf_uty : forall Phi, wf_ty S0 Phi uty.
    Proof.
      intros Phi. apply WfUnion.
      - constructor; [apply wf_tag; unfold nA; lia | constructor; [apply wf_tag; unfold nB; lia | constructor]].
      - constructor; [do 2 eexists; reflexivity | constructor; [do 2 eexists; reflexivity | constructor]].
      - simpl. constructor; [simpl; intros [H | []]; discriminate | constructor; [intros [] | constructor]].
    Qed.

    Lemma scrut_typed : forall G, has_ty S0 Phi1 G scrut uty.
    Proof.
      intros G. eapply TSubR; [apply tag_typed; unfold nA; lia | | apply wf_uty].
      eapply SubInj with (tau' := TNom nA []); [left; reflexivity | reflexivity].
    Qed.

    Definition Phi2 : ctx := open_res (app_tele [] [key]) Phi1.

    Lemma call_typed : forall G, has_ty S0 Phi2 G call TUnit.
    Proof.
      intros G. eapply conv.
      - eapply TMethR with (Dm := []) (S := mkSig [] TUnit) (tau0 := kty) (th0 := []).
        + reflexivity.
        + apply tag_typed. unfold nK. lia.
        + left. reflexivity.
        + reflexivity.
        + intros i1 [<- | [<- | []]] Hp; [reflexivity |].
          destruct Hp as [th' Hp]. discriminate Hp.
        + constructor.
      - reflexivity.
    Qed.

    Lemma apply_typed : forall G e, has_ty S0 Phi2 G e TUnit ->
      has_ty S0 Phi1 G (EApply uMs [] [] e) TUnit.
    Proof.
      intros G e He. eapply TApplyR with (Du := []) (Dr := [key]);
        [reflexivity | reflexivity | apply sat_nil | constructor | apply smap_nil
        | constructor | | constructor | exact He | constructor].
      unfold res_coherent. repeat constructor.
    Qed.

    Lemma match_typed : forall G e Phi,
      has_ty S0 Phi G scrut uty -> cS Phi = [] ->
      has_ty S0 Phi ((1, TUnit) :: G) e TUnit ->
      has_ty S0 Phi G (EMatch scrut [(nA, 1, EUnit); (nB, 1, e)]) TUnit.
    Proof.
      intros G e Phi Hs HS He.
      eapply TMatchR with (tau0 := uty) (ms := [TNom nA []; TNom nB []]).
      - exact Hs.
      - rewrite HS. apply wf_uty.
      - rewrite HS. reflexivity.
      - simpl. constructor; [simpl; intros [H | []]; discriminate | constructor; [intros [] | constructor]].
      - constructor; [split; [reflexivity | do 2 eexists; reflexivity] |].
        constructor; [split; [reflexivity | do 2 eexists; reflexivity] | constructor].
      - constructor; [constructor |]. constructor; [exact He | constructor].
    Qed.

    Lemma bind_typed : forall e, has_ty S0 Phi1 [] e TUnit ->
      has_ty S0 empty_ctx [] (EBindFun uMs uH [] [] e) TUnit.
    Proof.
      intros e He. eapply TBindFunR with (Du := []) (Dr := [key]) (Dh := []);
        [reflexivity | reflexivity | | | constructor | exact He].
      - exists empty_ctx. split; [reflexivity | split; [apply sat0_nil | apply smap_nil]].
      - apply (proj2 (@empty_capture_conservative S0 empty_ctx [] [key] uMs uH [] eq_refl)). reflexivity.
    Qed.

    Lemma placed_typed : has_ty S0 empty_ctx [] placed TUnit.
    Proof.
      apply bind_typed. apply match_typed; [apply scrut_typed | reflexivity |].
      apply apply_typed. apply call_typed.
    Qed.

    Lemma hoisted_typed : has_ty S0 empty_ctx [] hoisted TUnit.
    Proof.
      apply bind_typed. apply apply_typed. apply match_typed; [| reflexivity | apply call_typed].
      eapply TSubR; [apply tag_typed; unfold nA; lia | | apply wf_uty].
      eapply SubInj with (tau' := TNom nA []); [left; reflexivity | reflexivity].
    Qed.

    Lemma loop_typed : has_ty S0 empty_ctx [] loop TUnit.
    Proof.
      eapply conv; [eapply TCallR with (Df := []) (S := mkSig [] TUnit);
                    [reflexivity | apply sat_nil | constructor | apply smap_nil | constructor]
                   | reflexivity].
    Qed.

    Lemma key_ok : forall Phi, item_ok S0 Phi key.
    Proof.
      intros Phi. eapply OkMth; [reflexivity | apply wf_tag; unfold nK; lia | apply (sat_msat (Sg := S0) (Phi := Phi)); apply sat_nil | constructor].
    Qed.

    Lemma wf_sig_of : has_ty S0 empty_ctx (body_env (mkSig [] TUnit)) (ELet 9 mainb EUnit) TUnit ->
      wf_gsig S0.
    Proof.
      intros Hmain. constructor.
      - intros n D taup H.
        destruct n as [|[|[|[|n]]]]; simpl in H; try discriminate;
          injection H as <- <-; exists empty_ctx; split; constructor.
      - intros v D tauv H. destruct v as [|[|v]]; simpl in H; try discriminate.
        injection H as <- <-. exists empty_ctx. split; constructor.
      - intros f D S b H.
        destruct f as [|[|[|[|f]]]]; simpl in H; try discriminate; injection H as <- <- <-;
          exists empty_ctx; (split; [constructor |]).
        + split; [constructor |]. split; [constructor |].
          intros body Hb. injection Hb as <-. exact loop_typed.
        + split; [constructor; [apply wf_tag; unfold nK; lia | constructor] |].
          split; [constructor |]. intros body Hb. injection Hb as <-. constructor.
        + split; [constructor |]. split; [constructor |].
          intros body Hb. injection Hb as <-. exact Hmain.
      - intros m D S H. destruct m as [|[|m]]; simpl in H; try discriminate.
        injection H as <- <-. exists empty_ctx. split; [constructor |].
        split; constructor.
      - intros u Du Dr ob H.
        destruct u as [|[|[|u]]]; simpl in H; try discriminate; injection H as <- <- <-;
          exists empty_ctx; (split; [constructor |]).
        + split; [| intros bs Hbs; discriminate Hbs].
          intros _. split; [reflexivity |]. split; [intros x [] |]. split; [intros x [] |].
          eapply WCons; [apply key_ok | reflexivity | constructor].
        + split; [intros Hn; discriminate Hn |].
          intros bs Hbs. injection Hbs as <-.
          exists empty_ctx. split; [constructor |].
          split; [split; [intros x [] | constructor] |].
          split; [eapply WCons; [apply key_ok | reflexivity | constructor] |].
          eexists. split.
          * eapply BtVal; [reflexivity | apply sat0_nil | exact loop_typed |].
            eapply BtMth with (Dm := []) (S := mkSig [] TUnit) (D' := []) (S' := mkSig [kty] TUnit)
                              (tau0' := kty) (rest := []);
              [reflexivity | apply wf_tag; unfold nK; lia | apply sat_nil | constructor
              | reflexivity | apply sat0_nil | apply smap_nil | reflexivity | reflexivity
              | reflexivity | reflexivity | apply BtNil].
          * constructor; [simpl; tauto | constructor].
    Qed.
  End Wf.

  Lemma prog_ok : forall mainb, has_ty (SgOf mainb) empty_ctx [] mainb TUnit ->
    wf_prog (SgOf mainb) fMain.
  Proof.
    intros mainb H. split; [| eexists; reflexivity].
    apply wf_sig_of. eapply TLetR; [exact H | constructor].
  Qed.

  Lemma family_ms : forall mainb, family (SgOf mainb) uMs.
  Proof.
    intros mainb. exists [], [key]. split; [reflexivity |].
    split; [constructor; [do 3 eexists; reflexivity | constructor] |].
    constructor; [intros x [] | constructor].
  Qed.

  Lemma binds_a_val : ~ binds_no_val h_body.
  Proof. intros H. inversion H as [| b r Hb _]. exact Hb. Qed.
End FamilyVal.

Theorem family_val_placement :
  family (FamilyVal.SgOf FamilyVal.placed) FamilyVal.uMs
  /\ ~ binds_no_val FamilyVal.h_body
  /\ ~ wf_d84 (FamilyVal.SgOf FamilyVal.placed)
  /\ wf_prog (FamilyVal.SgOf FamilyVal.placed) FamilyVal.fMain
  /\ wf_prog (FamilyVal.SgOf FamilyVal.hoisted) FamilyVal.fMain
  /\ interp 100 (FamilyVal.SgOf FamilyVal.placed) [] [] FamilyVal.placed = Ok VUnit
  /\ (forall fuel, interp fuel (FamilyVal.SgOf FamilyVal.hoisted) [] []
                     FamilyVal.hoisted = OutOfFuel).
Proof.
  split; [apply FamilyVal.family_ms |].
  split; [exact FamilyVal.binds_a_val |].
  split.
  { intros W. apply FamilyVal.binds_a_val.
    exact (@d84_family_binding _ W FamilyVal.uMs [] [FamilyVal.key] FamilyVal.uH []
             FamilyVal.h_body (FamilyVal.family_ms _) eq_refl eq_refl). }
  split; [apply FamilyVal.prog_ok, FamilyVal.placed_typed |].
  split; [apply FamilyVal.prog_ok, FamilyVal.hoisted_typed |].
  split; [apply FamilyVal.placed_returns | apply FamilyVal.hoisted_diverges].
Qed.

(* ===================== 4. Rule 3d: no per-binding widening of X ========== *)

(* Probe held_narrow in the core.  `functor F[]: T, K.mk[Q = T] -> getk;`
   with `fn getk(): K` (K an implicit input): M(F) = {K}, P(F) = ∅, so
   X(F) = {K}.  G takes T and K.mk[Q = T] (linked) and pins K = Wrap[T];
   the pin mentions T, so G must capture T as well — the bootstrap's
   "held" T.  main binds T = A, provides K.mk at Q = A and at Q = B, binds
   K = Wrap[T], binds F = G, and applies F at T = B: K is unchanged, so
   the application's premises all hold, but the body's K.mk is now the
   B one while the scope reads getk() as a Wrap[A] — a B read as an A.
   The capture clause of cap_ok (a captured input of φ must be in X(φ)) is
   the one premise that fails, and rule 1 leaves no way to put T in X: T
   is not in M(F).  (K.mk is a method rather than D84's `mk` fn so that it
   can be provided at two instantiations at once; a fn item names none.) *)
Module HeldNarrow.
  Definition sT : tsym := 1.
  Definition sK : tsym := 2.
  Definition sQ : tsym := 3.
  Definition nK0 : nsym := 1.
  Definition nWrap : nsym := 2.
  Definition nA : nsym := 3.
  Definition nB : nsym := 4.
  Definition fGetk : fsym := 1.
  Definition fGk : fsym := 2.
  Definition fMkA : fsym := 3.
  Definition fMkB : fsym := 4.
  Definition fMain : fsym := 5.
  Definition mMk : msym := 1.
  Definition uF : usym := 1.
  Definition uG : usym := 2.

  Definition kty : ty := TNom nK0 [].
  Definition wrapT : ty := TNom nWrap [(sT, TAbs sT)].
  Definition mk_item : item := IMth kty mMk [(sQ, TAbs sT)].
  Definition Du : tele := [ITy sK; ITy sT; mk_item].
  Definition Dh : tele := [ITy sT; mk_item].
  Definition Dr : tele := [IFn fGetk].
  Definition cap : tele := [ITy sT; ITy sK].
  Definition pins : subst := [(sK, wrapT)].
  Definition sm_link : smap := [(mk_item, mk_item)].
  Definition th : subst := [(sK, TAbs sK); (sT, TNom nB [])].
  Definition sm_app : smap := [(mk_item, IMth kty mMk [(sQ, TNom nB [])])].

  Definition scope (n : nsym) : expr :=
    EMatch (ECallA fGetk []) [(nWrap, 1, EMatch (EVar 1) [(n, 2, EUnit)])].

  Definition main_with (n : nsym) : expr :=
    EBindTy sT (TNom nA [])
    (EBindMth kty mMk [(sQ, TNom nA [])] fMkA []
    (EBindMth kty mMk [(sQ, TNom nB [])] fMkB []
    (EBindTy sK wrapT
    (EBindFun uF uG sm_link []
    (EApply uF th sm_app (scope n)))))).

  Definition main_body : expr := main_with nA.

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sT || Nat.eqb t sK || Nat.eqb t sQ || Nat.eqb t this_sym)
    (fun n => if Nat.eqb n nWrap then Some ([ITy sT], TAbs sT)
              else if Nat.leb 1 n && Nat.leb n 4 then Some ([], TUnit) else None)
    (fun _ => None)
    (fun f => if Nat.eqb f fGetk then Some ([ITy sK], mkSig [] (TAbs sK), None)
              else if Nat.eqb f fGk then
                Some (Dh, mkSig [] wrapT,
                      Some (ETag nWrap [(sT, TAbs sT)] (EMeth (ETag nK0 [] EUnit) mMk mk_item [])))
              else if Nat.eqb f fMkA then
                Some ([], mkSig [kty] (TNom nA []), Some (ETag nA [] EUnit))
              else if Nat.eqb f fMkB then
                Some ([], mkSig [kty] (TNom nB []), Some (ETag nB [] EUnit))
              else if Nat.eqb f fMain then
                Some ([], mkSig [] TUnit, Some (ELet 9 main_body EUnit))
              else None)
    (fun m => if Nat.eqb m mMk then Some ([ITy sQ], mkSig [] (TAbs sQ)) else None)
    (fun u => if Nat.eqb u uF then Some (Du, Dr, None)
              else if Nat.eqb u uG then Some (Dh, Dr, Some [BFn fGetk fGk sm_link])
              else None)
    (fun u => if Nat.eqb u uF then [sK] else [])
    (fun _ => [])
    (fun u => if Nat.eqb u uG then (cap, pins) else ([], [])).

  Example goes_wrong : interp 200 Sg [] [] main_body = Err.
  Proof. vm_compute. reflexivity. Qed.

  (* Control: what getk() wraps is the B the body made. *)
  Example control : interp 200 Sg [] [] (main_with nB) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  Definition PhiB : ctx :=
    bind_ty sK wrapT
      (add_item (IMth kty mMk [(sQ, TNom nB [])])
        (add_item (IMth kty mMk [(sQ, TNom nA [])])
          (bind_ty sT (TNom nA []) empty_ctx))).
  Definition PhiApp : ctx := add_item (IFun uF) PhiB.

  Lemma d84 : wf_d84 Sg.
  Proof.
    constructor.
    - intros u Du' Dr' H. destruct u as [|[|[|u]]]; simpl in H; try discriminate.
      injection H as <- <-. split; [intros x [] |].
      intros x. vm_compute. intuition (subst; discriminate).
    - intros u Du' Dr' bs H. destruct u as [|[|[|u]]]; simpl in H; try discriminate.
      split; reflexivity.
    - intros u Du' Dr' h Dh' bs [Du0 [Dr0 [Hu [Hm _]]]].
      destruct u as [|[|[|u]]]; simpl in Hu; try discriminate.
      injection Hu as <- <-. inversion Hm as [| i l [rt [m [th' E]]] _]. discriminate E.
  Qed.

  Lemma t_not_mentioned : ~ In sT (fun_mentions Sg Dr).
  Proof. vm_compute. intuition discriminate. Qed.

  Lemma bind_premises :
    fun_link Sg Dh sm_link Du
    /\ avail PhiB (ITy sK)
    /\ sat0 Sg PhiB cap
    /\ smap_ok PhiB (id_app cap) [] cap
    /\ Forall (fun p => app_subst (cS PhiB) (TAbs (fst p)) = app_subst (cS PhiB) (snd p)) pins
    /\ ~ cap_ok Sg PhiB Du Dr uF uG [].
  Proof.
    split; [| split; [| split; [| split; [| split]]]].
    - exists (mkCtx Du []). split; [reflexivity |]. split.
      + exists (mkCtx Dh []). split; [reflexivity |]. split.
        * split; [reflexivity |]. split; [| constructor].
          constructor; [left; reflexivity |]. constructor; [| constructor].
          right. split; [reflexivity | constructor].
        * constructor; [reflexivity | constructor].
      + exists (mkCtx Dh []). split; [reflexivity | split; [reflexivity |]].
        constructor; [| constructor]. split; [simpl; tauto | reflexivity].
    - reflexivity.
    - exists (mkCtx cap []). split; [reflexivity |]. split.
      + split; [reflexivity |]. split; [repeat constructor; left; reflexivity | constructor].
      + constructor; [reflexivity | constructor; [reflexivity | constructor]].
    - exists (mkCtx cap []). split; [reflexivity | split; [reflexivity | constructor]].
    - constructor; [reflexivity | constructor].
    - intros [_ [_ [_ Hx]]]. simpl in Hx.
      assert (H : In sT [sK]) by (apply Hx; simpl; tauto).
      destruct H as [H | []]. discriminate H.
  Qed.

  Lemma apply_premises :
    sat Sg PhiApp th Du
    /\ smap_ok PhiApp th sm_app Du
    /\ fixed_ok Sg PhiApp uF th
    /\ res_coherent Sg PhiApp th Dr.
  Proof.
    split; [| split; [| split]].
    - exists (mkCtx Du []). split; [reflexivity |]. split; [reflexivity |].
      split; [| constructor].
      constructor; [left; reflexivity |]. constructor; [left; reflexivity |].
      constructor; [| constructor]. right. split; [reflexivity | constructor].
    - exists (mkCtx Du []). split; [reflexivity | split; [reflexivity |]].
      constructor; [| constructor]. split; [simpl; tauto | reflexivity].
    - constructor; [reflexivity | constructor].
    - unfold res_coherent. repeat constructor.
  Qed.
End HeldNarrow.

Theorem held_narrow_rejected :
  interp 200 HeldNarrow.Sg [] [] HeldNarrow.main_body = Err
  /\ wf_d84 HeldNarrow.Sg
  /\ ~ In HeldNarrow.sT (fun_mentions HeldNarrow.Sg HeldNarrow.Dr)
  /\ (* the bind: everything holds but the capture clause *)
     fun_link HeldNarrow.Sg HeldNarrow.Dh HeldNarrow.sm_link HeldNarrow.Du
  /\ avail HeldNarrow.PhiB (ITy HeldNarrow.sK)
  /\ sat0 HeldNarrow.Sg HeldNarrow.PhiB HeldNarrow.cap
  /\ smap_ok HeldNarrow.PhiB (id_app HeldNarrow.cap) [] HeldNarrow.cap
  /\ Forall (fun p => app_subst (cS HeldNarrow.PhiB) (TAbs (fst p))
                      = app_subst (cS HeldNarrow.PhiB) (snd p)) HeldNarrow.pins
  /\ ~ cap_ok HeldNarrow.Sg HeldNarrow.PhiB HeldNarrow.Du HeldNarrow.Dr
              HeldNarrow.uF HeldNarrow.uG []
  /\ (* the application: every premise holds *)
     sat HeldNarrow.Sg HeldNarrow.PhiApp HeldNarrow.th HeldNarrow.Du
  /\ smap_ok HeldNarrow.PhiApp HeldNarrow.th HeldNarrow.sm_app HeldNarrow.Du
  /\ fixed_ok HeldNarrow.Sg HeldNarrow.PhiApp HeldNarrow.uF HeldNarrow.th
  /\ res_coherent HeldNarrow.Sg HeldNarrow.PhiApp HeldNarrow.th HeldNarrow.Dr
  /\ ~ wf_prog HeldNarrow.Sg HeldNarrow.fMain.
Proof.
  destruct HeldNarrow.bind_premises as [B1 [B2 [B3 [B4 [B5 B6]]]]].
  destruct HeldNarrow.apply_premises as [A1 [A2 [A3 A4]]].
  split; [exact HeldNarrow.goes_wrong |].
  split; [exact HeldNarrow.d84 |].
  split; [exact HeldNarrow.t_not_mentioned |].
  repeat (split; [assumption |]).
  intros Hwf. apply (proj1 (soundness_main 201 Hwf eq_refl)). vm_compute. reflexivity.
Qed.

(* (c) ... and "bind no vals" is not enough.  The same family, now bound to
   H whose body binds no val but applies an abstract V that H captured; V
   is bound to W, whose body binds the diverging val.  H satisfies rule 7's
   condition as D84 states it (`wf_d84` holds), yet the two placements again
   differ: an application of a family runs whatever its binding applies.
   So D84's "placement is unobservable because a family's bindings bind no
   vals" needs the condition closed under application: a family's binding
   may apply no functor whose bindings may bind vals (for instance: none
   but families). *)
Module FamilyApply.
  Definition nK : nsym := 1.
  Definition nA : nsym := 2.
  Definition nB : nsym := 3.
  Definition vV : vsym := 1.
  Definition fLoop : fsym := 1.
  Definition fProv : fsym := 2.
  Definition fMain : fsym := 3.
  Definition mM : msym := 1.
  Definition uMs : usym := 1.
  Definition uH : usym := 2.
  Definition uV : usym := 3.
  Definition uW : usym := 4.

  Definition kty : ty := TNom nK [].
  Definition uty : ty := TUnion [TNom nA []; TNom nB []].
  Definition key : item := IMth kty mM [].
  Definition loop : expr := ECallD fLoop [] [] [].
  Definition h_body : list bnd := [BApply uV [] []; BMth kty mM [] fProv []].
  Definition w_body : list bnd := [BVal vV loop].

  Definition call : expr := EMeth (ETag nK [] EUnit) mM key [].
  Definition scrut : expr := ETag nA [] EUnit.
  Definition binds (e : expr) : expr :=
    EBindFun uV uW [] [] (EBindFun uMs uH [] [(IFun uV, IFun uV)] e).

  Definition placed : expr :=
    binds (EMatch scrut [(nA, 1, EUnit); (nB, 1, EApply uMs [] [] call)]).
  Definition hoisted : expr :=
    binds (EApply uMs [] [] (EMatch scrut [(nA, 1, EUnit); (nB, 1, call)])).

  Definition SgOf (mainb : expr) : gsig := mkGsig
    (fun t => Nat.eqb t this_sym)
    (fun n => if Nat.leb 1 n && Nat.leb n 3 then Some ([], TUnit) else None)
    (fun v => if Nat.eqb v vV then Some ([], TUnit) else None)
    (fun f => if Nat.eqb f fLoop then Some ([], mkSig [] TUnit, Some loop)
              else if Nat.eqb f fProv then Some ([], mkSig [kty] TUnit, Some EUnit)
              else if Nat.eqb f fMain then Some ([], mkSig [] TUnit, Some (ELet 9 mainb EUnit))
              else None)
    (fun m => if Nat.eqb m mM then Some ([], mkSig [] TUnit) else None)
    (fun u => if Nat.eqb u uMs then Some ([], [key], None)
              else if Nat.eqb u uH then Some ([], [key], Some h_body)
              else if Nat.eqb u uV then Some ([], [IVal vV], None)
              else if Nat.eqb u uW then Some ([], [IVal vV], Some w_body)
              else None)
    (fun _ => []) (fun _ => [])
    (fun u => if Nat.eqb u uH then ([IFun uV], []) else ([], [])).

  Lemma loop_diverges : forall mainb n d g, interp n (SgOf mainb) d g loop = OutOfFuel.
  Proof.
    intros mainb n. induction n as [| n IH]; intros d g; [reflexivity |].
    destruct n as [| n]; [reflexivity |].
    cbn -[interp]. exact (IH [] []).
  Qed.

  Example placed_returns : forall mainb, interp 100 (SgOf mainb) [] [] placed = Ok VUnit.
  Proof. intros mainb. reflexivity. Qed.

  Lemma hoisted_diverges : forall mainb n, interp n (SgOf mainb) [] [] hoisted = OutOfFuel.
  Proof.
    intros mainb n.
    destruct n as [|[|[|[|[|[|[|n]]]]]]]; try reflexivity.
    unfold hoisted, binds. simpl. rewrite loop_diverges. reflexivity.
  Qed.

  Lemma conv : forall S0 Phi G e t t', has_ty S0 Phi G e t -> t = t' -> has_ty S0 Phi G e t'.
  Proof. intros; subst; assumption. Qed.

  Section Wf.
    Variable mainb : expr.
    Let S0 := SgOf mainb.

    Lemma sat_nil : forall Phi, sat S0 Phi [] [].
    Proof.
      intros Phi. exists empty_ctx. split; [reflexivity |].
      split; [reflexivity | split; constructor].
    Qed.

    Lemma sat0_nil : forall Phi, sat0 S0 Phi [].
    Proof.
      intros Phi. exists empty_ctx. split; [reflexivity |].
      split; [split; [reflexivity | split; constructor] | constructor].
    Qed.

    Lemma smap_nil : forall Phi th, smap_ok Phi th [] [].
    Proof. intros Phi th. exists empty_ctx. split; [reflexivity | split; [reflexivity | constructor]]. Qed.

    Lemma wf_tag : forall Phi n, 1 <= n <= 3 -> wf_ty S0 Phi (TNom n []).
    Proof.
      intros Phi n Hn. eapply WfTag; [| apply sat_nil | constructor].
      destruct n as [|[|[|[|n]]]]; try lia; reflexivity.
    Qed.

    Lemma tag_typed : forall Phi G n, 1 <= n <= 3 ->
      has_ty S0 Phi G (ETag n [] EUnit) (app_subst (cS Phi) (TNom n [])).
    Proof.
      intros Phi G n Hn. eapply TTagR with (D := []) (tau0 := TUnit);
        [| apply sat_nil | constructor | apply conv with (t := TUnit); [constructor | reflexivity]].
      destruct n as [|[|[|[|n]]]]; try lia; reflexivity.
    Qed.

    Definition Phi1 : ctx := add_item (IFun uMs) (add_item (IFun uV) empty_ctx).

    Lemma wf_uty : forall Phi, wf_ty S0 Phi uty.
    Proof.
      intros Phi. apply WfUnion.
      - constructor; [apply wf_tag; unfold nA; lia | constructor; [apply wf_tag; unfold nB; lia | constructor]].
      - constructor; [do 2 eexists; reflexivity | constructor; [do 2 eexists; reflexivity | constructor]].
      - simpl. constructor; [simpl; intros [H | []]; discriminate | constructor; [intros [] | constructor]].
    Qed.

    Lemma scrut_typed : forall Phi G, cS Phi = [] -> has_ty S0 Phi G scrut uty.
    Proof.
      intros Phi G HS. eapply TSubR; [apply tag_typed; unfold nA; lia | | apply wf_uty].
      eapply SubInj with (tau' := TNom nA []); [left; reflexivity | rewrite HS; reflexivity].
    Qed.

    Definition Phi2 : ctx := open_res (app_tele [] [key]) Phi1.

    Lemma call_typed : forall G, has_ty S0 Phi2 G call TUnit.
    Proof.
      intros G. eapply conv.
      - eapply TMethR with (Dm := []) (S := mkSig [] TUnit) (tau0 := kty) (th0 := []).
        + reflexivity.
        + apply tag_typed. unfold nK. lia.
        + left. reflexivity.
        + reflexivity.
        + intros i1 [<- | [<- | [<- | []]]] Hp; [reflexivity | |];
            destruct Hp as [th' Hp]; discriminate Hp.
        + constructor.
      - reflexivity.
    Qed.

    Lemma apply_typed : forall G e, has_ty S0 Phi2 G e TUnit ->
      has_ty S0 Phi1 G (EApply uMs [] [] e) TUnit.
    Proof.
      intros G e He. eapply TApplyR with (Du := []) (Dr := [key]);
        [reflexivity | reflexivity | apply sat_nil | constructor | apply smap_nil
        | constructor | | constructor | exact He | constructor].
      unfold res_coherent. repeat constructor.
    Qed.

    Lemma match_typed : forall G e Phi,
      cS Phi = [] ->
      has_ty S0 Phi ((1, TUnit) :: G) e TUnit ->
      has_ty S0 Phi G (EMatch scrut [(nA, 1, EUnit); (nB, 1, e)]) TUnit.
    Proof.
      intros G e Phi HS He.
      eapply TMatchR with (tau0 := uty) (ms := [TNom nA []; TNom nB []]).
      - exact (@scrut_typed Phi G HS).
      - rewrite HS. apply wf_uty.
      - rewrite HS. reflexivity.
      - simpl. constructor; [simpl; intros [H | []]; discriminate | constructor; [intros [] | constructor]].
      - constructor; [split; [reflexivity | do 2 eexists; reflexivity] |].
        constructor; [split; [reflexivity | do 2 eexists; reflexivity] | constructor].
      - constructor; [constructor |]. constructor; [exact He | constructor].
    Qed.

    Definition PhiV : ctx := add_item (IFun uV) empty_ctx.

    Lemma cap_h : cap_ok S0 PhiV [] [key] uMs uH [(IFun uV, IFun uV)].
    Proof.
      split; [| split; [| split]].
      - exists (mkCtx [IFun uV] []). split; [reflexivity |].
        split; [split; [reflexivity | split; [| constructor]] | constructor].
        constructor; [| constructor]. right. split; [reflexivity | constructor].
      - exists (mkCtx [IFun uV] []). split; [reflexivity | split; [reflexivity |]].
        constructor; [| constructor]. split; [left; reflexivity | reflexivity].
      - constructor.
      - intros x [].
    Qed.

    Lemma binds_typed : forall e, has_ty S0 Phi1 [] e TUnit ->
      has_ty S0 empty_ctx [] (binds e) TUnit.
    Proof.
      intros e He. eapply TBindFunR with (Du := []) (Dr := [IVal vV]) (Dh := []);
        [reflexivity | reflexivity | | | constructor |].
      - exists empty_ctx. split; [reflexivity | split; [apply sat0_nil | apply smap_nil]].
      - apply (proj2 (@empty_capture_conservative S0 empty_ctx [] [IVal vV] uV uW [] eq_refl)).
        reflexivity.
      - eapply TBindFunR with (Du := []) (Dr := [key]) (Dh := []);
          [reflexivity | reflexivity | | exact cap_h | constructor | exact He].
        exists empty_ctx. split; [reflexivity | split; [apply sat0_nil | apply smap_nil]].
    Qed.

    Lemma placed_typed : has_ty S0 empty_ctx [] placed TUnit.
    Proof.
      apply binds_typed. apply match_typed; [reflexivity |].
      apply apply_typed. apply call_typed.
    Qed.

    Lemma hoisted_typed : has_ty S0 empty_ctx [] hoisted TUnit.
    Proof.
      apply binds_typed. apply apply_typed. apply match_typed; [reflexivity | apply call_typed].
    Qed.

    Lemma loop_typed : has_ty S0 empty_ctx [] loop TUnit.
    Proof.
      eapply conv; [eapply TCallR with (Df := []) (S := mkSig [] TUnit);
                    [reflexivity | apply sat_nil | constructor | apply smap_nil | constructor]
                   | reflexivity].
    Qed.

    Lemma key_ok : forall Phi, item_ok S0 Phi key.
    Proof.
      intros Phi. eapply OkMth; [reflexivity | apply wf_tag; unfold nK; lia | apply (sat_msat (Sg := S0) (Phi := Phi)); apply sat_nil | constructor].
    Qed.

    Lemma v_ok : forall Phi, item_ok S0 Phi (IVal vV).
    Proof. intros Phi. eapply OkVal; [reflexivity | constructor]. Qed.

    Lemma wf_sig_of : has_ty S0 empty_ctx (body_env (mkSig [] TUnit)) (ELet 9 mainb EUnit) TUnit ->
      wf_gsig S0.
    Proof.
      intros Hmain. constructor.
      - intros n D taup H.
        destruct n as [|[|[|[|n]]]]; simpl in H; try discriminate;
          injection H as <- <-; exists empty_ctx; split; constructor.
      - intros v D tauv H. destruct v as [|[|v]]; simpl in H; try discriminate.
        injection H as <- <-. exists empty_ctx. split; constructor.
      - intros f D S b H.
        destruct f as [|[|[|[|f]]]]; simpl in H; try discriminate; injection H as <- <- <-;
          exists empty_ctx; (split; [constructor |]).
        + split; [constructor |]. split; [constructor |].
          intros body Hb. injection Hb as <-. exact loop_typed.
        + split; [constructor; [apply wf_tag; unfold nK; lia | constructor] |].
          split; [constructor |]. intros body Hb. injection Hb as <-. constructor.
        + split; [constructor |]. split; [constructor |].
          intros body Hb. injection Hb as <-. exact Hmain.
      - intros m D S H. destruct m as [|[|m]]; simpl in H; try discriminate.
        injection H as <- <-. exists empty_ctx. split; [constructor |].
        split; constructor.
      - intros u Du Dr ob H.
        destruct u as [|[|[|[|[|u]]]]]; simpl in H; try discriminate; injection H as <- <- <-;
          exists empty_ctx; (split; [constructor |]).
        + split; [| intros bs Hbs; discriminate Hbs].
          intros _. split; [reflexivity |]. split; [intros x [] |]. split; [intros x [] |].
          eapply WCons; [apply key_ok | reflexivity | constructor].
        + split; [intros Hn; discriminate Hn |].
          intros bs Hbs. injection Hbs as <-.
          exists (mkCtx [IFun uV] []). split.
          { eapply WCons; [eapply OkFun; [reflexivity | constructor] | reflexivity | constructor]. }
          split; [split; [intros x [] | constructor] |].
          split; [eapply WCons; [apply key_ok | reflexivity | constructor] |].
          eexists. split.
          * eapply BtApply with (Du := []) (Dr := [IVal vV]);
              [reflexivity | reflexivity | apply sat_nil | constructor | apply smap_nil
              | constructor | unfold res_coherent; repeat constructor | constructor |].
            eapply BtMth with (Dm := []) (S := mkSig [] TUnit) (D' := []) (S' := mkSig [kty] TUnit)
                              (tau0' := kty) (rest := []);
              [reflexivity | apply wf_tag; unfold nK; lia | apply sat_nil | constructor
              | reflexivity | apply sat0_nil | apply smap_nil | reflexivity | reflexivity
              | reflexivity | reflexivity | apply BtNil].
          * constructor; [simpl; tauto | constructor].
        + split; [| intros bs Hbs; discriminate Hbs].
          intros _. split; [reflexivity |]. split; [intros x [] |]. split; [intros x [] |].
          eapply WCons; [apply v_ok | reflexivity | constructor].
        + split; [intros Hn; discriminate Hn |].
          intros bs Hbs. injection Hbs as <-.
          exists empty_ctx. split; [constructor |].
          split; [split; [intros x [] | constructor] |].
          split; [eapply WCons; [apply v_ok | reflexivity | constructor] |].
          eexists. split.
          * eapply BtVal; [reflexivity | apply sat0_nil | exact loop_typed | apply BtNil].
          * constructor; [simpl; tauto | constructor].
    Qed.

    Lemma d84_of : wf_d84 S0.
    Proof.
      constructor.
      - intros u Du Dr H.
        destruct u as [|[|[|[|[|u]]]]]; simpl in H; try discriminate; injection H as <- <-;
          (split; [intros x [] | intros x; vm_compute; intuition]).
      - intros u Du Dr bs H.
        destruct u as [|[|[|[|[|u]]]]]; simpl in H; try discriminate; split; reflexivity.
      - intros u Du Dr h Dh bs [Du0 [Dr0 [Hu [Hm _]]]] Hu' Hh.
        rewrite Hu in Hu'. injection Hu' as <- <-.
        destruct u as [|[|[|[|[|u]]]]]; simpl in Hu; try discriminate; injection Hu as <- <-.
        + destruct h as [|[|[|[|[|h]]]]]; simpl in Hh; try discriminate.
          injection Hh as <- <-. repeat constructor.
        + inversion Hm as [| i l [rt [m [th' E]]] _]. discriminate E.
    Qed.
  End Wf.

  Lemma prog_ok : forall mainb, has_ty (SgOf mainb) empty_ctx [] mainb TUnit ->
    wf_prog (SgOf mainb) fMain.
  Proof.
    intros mainb H. split; [| eexists; reflexivity].
    apply wf_sig_of. eapply TLetR; [exact H | constructor].
  Qed.

  Lemma family_ms : forall mainb, family (SgOf mainb) uMs.
  Proof.
    intros mainb. exists [], [key]. split; [reflexivity |].
    split; [constructor; [do 3 eexists; reflexivity | constructor] |].
    constructor; [intros x [] | constructor].
  Qed.
End FamilyApply.

Theorem family_apply_placement :
  family (FamilyApply.SgOf FamilyApply.placed) FamilyApply.uMs
  /\ wf_d84 (FamilyApply.SgOf FamilyApply.placed)
  /\ wf_d84 (FamilyApply.SgOf FamilyApply.hoisted)
  /\ wf_prog (FamilyApply.SgOf FamilyApply.placed) FamilyApply.fMain
  /\ wf_prog (FamilyApply.SgOf FamilyApply.hoisted) FamilyApply.fMain
  /\ interp 100 (FamilyApply.SgOf FamilyApply.placed) [] [] FamilyApply.placed = Ok VUnit
  /\ (forall fuel, interp fuel (FamilyApply.SgOf FamilyApply.hoisted) [] []
                     FamilyApply.hoisted = OutOfFuel).
Proof.
  split; [apply FamilyApply.family_ms |].
  split; [apply FamilyApply.d84_of |].
  split; [apply FamilyApply.d84_of |].
  split; [apply FamilyApply.prog_ok, FamilyApply.placed_typed |].
  split; [apply FamilyApply.prog_ok, FamilyApply.hoisted_typed |].
  split; [apply FamilyApply.placed_returns | apply FamilyApply.hoisted_diverges].
Qed.
