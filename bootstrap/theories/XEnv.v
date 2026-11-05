(* ------------------------------------------------------------------------ *)
(* XEnv.v — runtime environments agreeing with MossX contexts (Track B).     *)
(*                                                                           *)
(* CoreMoss.v §11b's semantic invariant (`dentry_agree`, `denv_agree`),      *)
(* restated over MossX's runtime entries and values (`xvt`), with two new   *)
(* entry forms: a primitive provides a fn or method whose signature, read   *)
(* (an unmodelled primitive, PUnsupported, provides any: running it stops   *)
(* the program with XUns),                                                   *)
(* at the grounding, is the primitive's (Part IV, `prim_sig`); and, at this *)
(* tier, a functor binding is a closure over its captured inputs (Rule A). *)
(* The lemmas are CoreMoss.v's, ported: the compose lemmas, the           *)
(* retyping of an entry, and the callee's environment at a call.           *)
(* A functor entry is a closure over what its body captured (Rule A).     *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit CoreMoss XTyping.
Import ListNotations.

Set Implicit Arguments.

Section XEnv.

Variable Sg : gsig.
Variable Rp : nsym -> option rep.
Variable P : xprog.
Variable Fb : usym -> option (tele * tele * subst).
(* the store typing values are read at *)
Variable Hst : sty.

(* A binding's chain of links from a functor with a body (linked arguments
   Dh) out to the abstract functor u: the first link into the abstract w it
   was bound to, then CoreMoss.chain_ok among abstract functors. *)
Definition body_chain (Dh : tele) (sms : list smap) (u : usym) : Prop :=
  exists sm r w Dw Drw, sms = sm :: r /\ g_fun Sg w = Some (Dw, Drw, None)
  /\ fun_link Sg Dh sm Dw /\ chain_ok Sg w r u.

Local Notation xvt := (xvt Sg Rp Hst).
Local Notation pty_ok := (pty_ok Sg Rp P).

(* Entry-level agreement, CoreMoss.dentry_agree's clauses for vals and
   closures (a closure's code is a fn P has a body for), plus primitives. *)
Fixpoint xdentry_agree (g : subst) (i : item) (en : xdentry) {struct en} : Prop :=
  match i, en with
  | IVal v, XDVal w =>
      match g_val Sg v with
      | Some (_, tauv) => xvt w (app_subst g tauv)
      | None => False
      end
  | IFn f, XDClo f' d' =>
      match g_fn Sg f, g_fn Sg f', xp_fn P f' with
      | Some (_, Sf, None), Some (D', S', None), Some _ =>
          exists PhiD' g',
            tele_ctx D' = Some PhiD' /\
            grounding g' /\ covers g' (tyreqs (cA PhiD')) /\
            absorbs g' (cS PhiD') /\
            map (app_subst g') (fs_params S') = map (app_subst g) (fs_params Sf) /\
            app_subst g' (fs_ret S') = app_subst g (fs_ret Sf) /\
            Forall (fun i => is_ty_item i = true \/
                             exists en, lookup_xdenv i d' = Some en) (cA PhiD') /\
            Forall (fun p =>
              incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiD'))) d' /\
            (fix all (dd : xdenv) : Prop :=
               match dd with
               | [] => True
               | p :: r => xdentry_agree g' (fst p) (snd p) /\ all r
               end) d'
      | _, _, _ => False
      end
  | IMth rt m th, XDClo f' d' =>
      match g_mth Sg m, g_fn Sg f', xp_fn P f' with
      | Some (_, Sm), Some (D', S', None), Some _ =>
          exists PhiD' g',
            tele_ctx D' = Some PhiD' /\
            grounding g' /\ covers g' (tyreqs (cA PhiD')) /\
            absorbs g' (cS PhiD') /\
            map (app_subst g') (fs_params S')
              = app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm) /\
            app_subst g' (fs_ret S')
              = app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm))) /\
            Forall (fun i => is_ty_item i = true \/
                             exists en, lookup_xdenv i d' = Some en) (cA PhiD') /\
            Forall (fun p =>
              incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiD'))) d' /\
            (fix all (dd : xdenv) : Prop :=
               match dd with
               | [] => True
               | p :: r => xdentry_agree g' (fst p) (snd p) /\ all r
               end) d'
      | _, _, _ => False
      end
  | IFn f, XDPrim p =>
      match g_fn Sg f with
      | Some (_, Sf, None) =>
          prim_ok Sg Rp P p (map (app_subst g) (fs_params Sf)) (app_subst g (fs_ret Sf))
      | _ => False
      end
  | IMth rt m th, XDPrim p =>
      match g_mth Sg m with
      | Some (_, Sm) =>
          prim_ok Sg Rp P p
               (app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm))
               (app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm))))
      | None => False
      end
  (* A functor binding [D78] under Rule A (CoreMoss.dentry_agree's DFun
     clause): a functor with a body at φ's result, its chain of links out to
     φ's arguments, and what it captured where it was bound, at that bind's
     grounding g', which reads every captured symbol h's linked arguments or
     result read as the caller's g does: each such symbol is a fixed input of
     φ, so the entry depends on g only through X(φ). *)
  | IFun u, XDFun h sms dc =>
      match g_fun Sg u, Fb h, xp_fun P h with
      | Some (_, Dr, None), Some (Dh, Dc, pins), Some (Drp, _) =>
          Drp = Dr /\ body_chain Dh sms u /\
          exists PhiC g',
            tele_ctx Dc = Some PhiC /\
            grounding g' /\ covers g' (tyreqs (cA PhiC)) /\
            absorbs g' (cS PhiC) /\
            absorbs g' pins /\
            Forall (fun i => is_ty_item i = true \/
                             exists en, lookup_xdenv i dc = Some en) (cA PhiC) /\
            Forall (fun p =>
              incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiC))) dc /\
            (forall x, In x (tyreqs (cA PhiC)) ->
                       In x (tyreqs Dh ++ res_reads Sg Dr) ->
                       In x (g_fixed Sg u)
                       /\ app_subst g' (TAbs x) = app_subst g (TAbs x)) /\
            (fix all (dd : xdenv) : Prop :=
               match dd with
               | [] => True
               | p :: r => xdentry_agree g' (fst p) (snd p) /\ all r
               end) dc
      | _, _, _ => False
      end
  | _, _ => False
  end.

(* The strong form: every entry of the environment agrees.  It holds of
   what a closure captures and of what a call or an application composes;
   a running scope only needs the relaxed form (`xdenv_agree`, below). *)
Definition xdenv_strong (g : subst) (A : list item) (d : xdenv) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_xdenv i d = Some en) A
  /\ Forall (fun p => xdentry_agree g (fst p) (snd p)) d
  /\ Forall (fun i =>
       incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
            (tyreqs A)) A
  /\ Forall (fun p =>
       incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
             ++ method_item_frees Sg (fst p)) (tyreqs A)) d.

(* The variables: position by position (MossX's environment is a stack that
   `let`, patterns and calls push and pop, and assignment updates in place),
   each value at its type read through the grounding. *)
Definition venv_ok (g : subst) (A : list item) (G : tenv) (ge : xvenv) : Prop :=
  Forall2 (fun p q => fst p = fst q /\ xvt (snd q) (app_subst g (snd p))) G ge
  /\ Forall (fun p => incl (ty_frees (snd p)) (tyreqs A)) G.

Lemma xdentry_all_Forall : forall g dd,
  (fix all (dd : xdenv) : Prop :=
     match dd with
     | [] => True
     | p :: r => xdentry_agree g (fst p) (snd p) /\ all r
     end) dd
  <-> Forall (fun p => xdentry_agree g (fst p) (snd p)) dd.
Proof.
  intros g dd. induction dd as [|p r IH]; split; intro H.
  - constructor.
  - exact I.
  - destruct H as [H1 H2]. constructor; [exact H1 | apply IH; exact H2].
  - inversion H; subst. split; [assumption | apply IH; assumption].
Qed.

Lemma lookup_xdenv_in : forall i d en,
  lookup_xdenv i d = Some en -> In (i, en) d.
Proof.
  induction d as [| [i' en'] r IH]; intros en H.
  - simpl in H. discriminate.
  - simpl in H. destruct (item_eqb i i') eqn:Heq.
    + apply item_eqb_eq in Heq. subst. injection H as ->.
      left. reflexivity.
    + right. apply IH. exact H.
Qed.

Lemma xcompose_defined : forall d sm,
  Forall (fun p => exists en, lookup_xdenv (snd p) d = Some en) sm ->
  exists d', xcompose d sm = Some d'.
Proof.
  intros d sm H. induction sm as [|[req s] r IH]; simpl.
  - exists []. reflexivity.
  - inversion H as [|p0 r0 Hhd Htl]; subst.
    destruct Hhd as [en He]. simpl in He.
    destruct (IH Htl) as [d' Hd'].
    rewrite He, Hd'. exists ((req, en) :: d'). reflexivity.
Qed.

Lemma xcompose_keys : forall d sm d',
  xcompose d sm = Some d' -> map fst d' = map fst sm.
Proof.
  intros d sm. induction sm as [|[req s] r IH]; simpl; intros d' H.
  - injection H as H; subst. reflexivity.
  - destruct (lookup_xdenv s d) as [en|] eqn:He; try discriminate.
    destruct (xcompose d r) as [dr|] eqn:Hr; try discriminate.
    injection H as H; subst. simpl. f_equal. apply IH. reflexivity.
Qed.

Lemma xcompose_entry : forall d sm d' p,
  xcompose d sm = Some d' -> In p d' ->
  exists sat', In (fst p, sat') sm /\ lookup_xdenv sat' d = Some (snd p).
Proof.
  intros d sm. induction sm as [|[req s] r IH]; simpl; intros d' p H Hin.
  - injection H as H; subst. destruct Hin.
  - destruct (lookup_xdenv s d) as [en|] eqn:He; try discriminate.
    destruct (xcompose d r) as [dr|] eqn:Hr; try discriminate.
    injection H as H; subst. simpl in Hin.
    destruct Hin as [Heq | Hin].
    + subst p. simpl. exists s. split; [left; reflexivity | exact He].
    + destruct (IH dr p (eq_refl) Hin) as [sat' [Hin' Hlk]].
      exists sat'. split; [right; exact Hin' | exact Hlk].
Qed.

Lemma xcompose_first : forall d sm d' req sat',
  xcompose d sm = Some d' ->
  lookup_smap req sm = Some sat' ->
  exists en, lookup_xdenv sat' d = Some en /\ lookup_xdenv req d' = Some en.
Proof.
  intros d sm. induction sm as [|[a b] r IH]; simpl; intros d' req sat' H Hl.
  - discriminate.
  - destruct (lookup_xdenv b d) as [en|] eqn:He; try discriminate.
    destruct (xcompose d r) as [dr|] eqn:Hr; try discriminate.
    injection H as H; subst d'.
    destruct (item_eqb req a) eqn:Ha.
    + injection Hl as Hl; subst sat'.
      exists en. split; [exact He|]. simpl. rewrite Ha. reflexivity.
    + destruct (IH dr req sat' (eq_refl) Hl) as [en' [H1 H2]].
      exists en'. split; [exact H1|]. simpl. rewrite Ha. exact H2.
Qed.

(* CoreMoss.dentry_agree_retype, for MossX entries. *)
Lemma xdentry_agree_retype : forall g1 i1 g2 i2 en,
  wf_gsig Sg ->
  xdentry_agree g1 i1 en ->
  app_subst_item g1 i1 = app_subst_item g2 i2 ->
  (forall t, In t (dep_tys Sg i1) ->
             app_subst g1 (TAbs t) = app_subst g2 (TAbs t)) ->
  (forall rt m th rt' th' Dm Sm tau,
      i1 = IMth rt m th -> i2 = IMth rt' m th' ->
      g_mth Sg m = Some (Dm, Sm) ->
      In tau (fs_params Sm) \/ tau = fs_ret Sm ->
      interp_m g1 th rt tau = interp_m g2 th' rt' tau) ->
  xdentry_agree g2 i2 en.
Proof.
  intros g1 i1 g2 i2 en WF Hag Hit Hdep Hm.
  destruct i1 as [t1|v1|f1|rt1 m1 th1|u1];
    destruct i2 as [t2|v2|f2|rt2 m2 th2|u2];
    try (simpl in Hit; discriminate);
    [ destruct en; simpl in Hag; contradiction | | | | ].
  - (* val *)
    simpl in Hit. injection Hit as Hv. subst v2.
    destruct en as [w| | | |]; simpl in Hag |- *; try contradiction.
    destruct (g_val Sg v1) as [[Dv tauv]|] eqn:E; [|destruct Hag].
    simpl in Hdep. rewrite E in Hdep.
    destruct (wf_val_decl WF _ E) as [PhiDv [Wtv Hwf]].
    apply wf_ty_frees in Hwf. simpl in Hwf.
    rewrite <- (dentry_agree_retype_aux1 g1 g2 tauv); [exact Hag|].
    intros t Ht. apply Hdep.
    destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtv t) (Hwf t Ht)) as [Hin0 | Hin];
      [destruct Hin0 | exact Hin].
  - (* fn *)
    simpl in Hit. injection Hit as Hf. subst f2.
    simpl in Hdep.
    destruct (g_fn Sg f1) as [[[Df Sf] b]|] eqn:E;
      [| destruct en; simpl in Hag; rewrite ?E in Hag; simpl in Hag; contradiction].
    destruct (wf_fn_decl WF _ E) as [PhiDf [Wtf [Hps [Hrt _]]]].
    assert (Hcnv : forall t, In t (tyreqs (cA PhiDf)) -> In t (tyreqs Df)).
    { intros t Ht.
      destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtf t) Ht) as [Hin0 | Hin];
        [destruct Hin0 | exact Hin]. }
    assert (Hpar' : map (app_subst g1) (fs_params Sf) = map (app_subst g2) (fs_params Sf)).
    { apply map_ext_in. intros tau Htau.
      apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      rewrite Forall_forall in Hps. specialize (Hps tau Htau).
      apply wf_ty_frees in Hps. simpl in Hps. apply Hcnv. apply Hps. exact Ht. }
    assert (Hret' : app_subst g1 (fs_ret Sf) = app_subst g2 (fs_ret Sf)).
    { apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      apply wf_ty_frees in Hrt. simpl in Hrt. apply Hcnv. apply Hrt. exact Ht. }
    destruct en as [w|f' d'|hf sms dc|p|hs]; simpl in Hag |- *; rewrite ?E in Hag |- *;
      try contradiction.
    + destruct b; [destruct Hag|].
      destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
      destruct b'; [destruct Hag|].
      destruct (xp_fn P f') as [body|]; [|destruct Hag].
      destruct Hag as [PhiD' [g' [Htc [Hg [Hc [Habs [Hpar [Hret Hrest]]]]]]]].
      exists PhiD', g'. split; [exact Htc|]. split; [exact Hg|].
      split; [exact Hc|]. split; [exact Habs|].
      split; [rewrite Hpar; exact Hpar'|].
      split; [rewrite Hret; exact Hret' | exact Hrest].
    + destruct b; [destruct Hag|].
      rewrite <- Hpar', <- Hret'. exact Hag.
  - (* method *)
    simpl in Hit. injection Hit as Hrt Hmm Hth. subst m2.
    destruct (g_mth Sg m1) as [[Dm Sm]|] eqn:E;
      [| destruct en; simpl in Hag; rewrite ?E in Hag; simpl in Hag; contradiction].
    assert (Hm' : forall tau, In tau (fs_params Sm) \/ tau = fs_ret Sm ->
              interp_m g1 th1 rt1 tau = interp_m g2 th2 rt2 tau)
      by (intros tau HIn; eapply Hm; eauto).
    unfold interp_m in Hm'.
    assert (Hpar' : map (fun tau => app_subst g1 (app_subst (this_subst rt1) (app_subst th1 tau)))
                        (fs_params Sm)
                  = map (fun tau => app_subst g2 (app_subst (this_subst rt2) (app_subst th2 tau)))
                        (fs_params Sm)).
    { apply map_ext_in. intros tau Htau. apply Hm'. left; exact Htau. }
    destruct en as [w|f' d'|hf sms dc|p|hs]; simpl in Hag |- *; rewrite ?E in Hag |- *;
      try contradiction.
    + destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
      destruct b'; [destruct Hag|].
      destruct (xp_fn P f') as [body|]; [|destruct Hag].
      destruct Hag as [PhiD' [g' [Htc [Hg [Hc [Habs [Hpar [Hret Hrest]]]]]]]].
      exists PhiD', g'. split; [exact Htc|]. split; [exact Hg|].
      split; [exact Hc|]. split; [exact Habs|].
      split.
      * rewrite Hpar, Hrt, Hpar'. reflexivity.
      * split; [|exact Hrest]. rewrite Hret. apply Hm'. right; reflexivity.
    + rewrite <- Hrt, <- Hpar', <- (Hm' (fs_ret Sm) (or_intror eq_refl)). exact Hag.
  - (* functor: the grounding is read only at the fixed inputs *)
    simpl in Hit. injection Hit as Hu. subst u2.
    destruct en as [| | hf sms dc | |]; simpl in Hag |- *; try contradiction.
    simpl in Hdep.
    destruct (g_fun Sg u1) as [[[Du Dr] [b|]]|]; try exact Hag.
    destruct (Fb hf) as [[[Dh Dc] pins]|]; try exact Hag.
    destruct (xp_fun P hf) as [[Drp bs]|]; try exact Hag.
    destruct Hag as [E1 [Hch [PhiC [g' [H1 [H2 [H3 [H4 [H5 [H6 [H7 [Hfx H8]]]]]]]]]]]].
    split; [exact E1 |]. split; [exact Hch |].
    exists PhiC, g'. repeat (split; [assumption |]).
    split; [| exact H8].
    intros x Hx Hx'. destruct (Hfx x Hx Hx') as [Hin Heq].
    split; [exact Hin |]. rewrite Heq. apply Hdep. exact Hin.
Qed.

(* The callee's environment at a call (CoreMoss.callee_env_agree). *)

Lemma xcallee_env_strong : forall g Phi th sm D d PhiD,
  wf_gsig Sg ->
  wf_tele Sg empty_ctx D PhiD ->
  sat Sg Phi th D ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  smap_ok Phi th sm D ->
  ctx_grounded g Phi ->
  xdenv_strong g (cA Phi) d ->
  exists d', xcompose d sm = Some d'
    /\ ctx_grounded (callee_grounding g Phi th (cA PhiD)) PhiD
    /\ xdenv_strong (callee_grounding g Phi th (cA PhiD)) (cA PhiD) d'.
Proof.
  intros g Phi th sm D d PhiD WF Wtel Hsat Hth Hsm Hcg Hd.
  pose proof (wf_tele_ctx Wtel) as Htc.
  destruct Hsat as [PhiD1 [Htc1 [Hthdom [Hsat Hmerge]]]].
  assert (E1 : PhiD1 = PhiD) by congruence. subst PhiD1.
  destruct Hsm as [PhiD2 [Htc2 [Hsmdom Hsm]]].
  assert (E2 : PhiD2 = PhiD) by congruence. subst PhiD2.
  destruct Hcg as [Hground [Habs [Hcovers [Hsigma Hreserved]]]].
  destruct Hd as [Hdcover [Hdagree [HAreg Hdreg]]].
  (* Accumulator premises of the wf_tele-indexed lemmas, trivially satisfied
     at empty_ctx. *)
  assert (Hacc0 : forall i, In i (cA empty_ctx) ->
      incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
           (tyreqs (cA empty_ctx)))
    by (intros i Hi; destruct Hi).
  assert (Hdep0 : forall i, In i (cA empty_ctx) -> is_ty_item i = false ->
      incl (dep_tys Sg i) (tyreqs (cA empty_ctx)))
    by (intros i Hi; destruct Hi).
  assert (Hmth0 : forall rt0 m0 ap0 Dm0 Sm0,
      In (IMth rt0 m0 ap0) (cA empty_ctx) -> g_mth Sg m0 = Some (Dm0, Sm0) ->
      msat ap0 Dm0 Sm0)
    by (intros rt0 m0 ap0 Dm0 Sm0 Hi; destruct Hi).
  assert (Hsmlook :
      Forall (fun p => exists en, lookup_xdenv (snd p) d = Some en) sm).
  { rewrite Forall_forall. intros [req sat'] Hin.
    rewrite Forall_forall in Hsm. destruct (Hsm (req, sat') Hin) as [Hina Hmatch].
    rewrite Forall_forall in Hdcover.
    specialize (Hdcover sat' Hina).
    destruct Hdcover as [Hty|[en Hen]].
    - assert (Hreqin : In req (filter (fun i => negb (is_ty_item i)) (cA PhiD))).
      { rewrite <- Hsmdom. apply in_map_iff.
        exists (req, sat'). split; [reflexivity|exact Hin]. }
      apply filter_In in Hreqin. destruct Hreqin as [_ Hreqty].
      apply Bool.negb_true_iff in Hreqty.
      pose proof (@callee_env_agree_aux4 (cS Phi) th req sat'
                    Hreqty Hmatch) as Hsatty.
      rewrite Hsatty in Hty. discriminate.
    - exists en. exact Hen. }
  destruct (@xcompose_defined d sm Hsmlook) as [d' Hcompose].
  exists d'. split; [exact Hcompose|].
  assert (Hcgground : grounding (callee_grounding g Phi th (cA PhiD))).
  { unfold grounding, callee_grounding.
    apply Forall_map. rewrite Forall_forall. intros t Htin.
    assert (Htth : In t (map fst th)) by (rewrite Hthdom; exact Htin).
    destruct (subst_chain_aux1 t th Htth) as [a Ha].
    assert (Hpa : In (t, a) th) by (apply callee_env_agree_aux1; exact Ha).
    rewrite Forall_forall in Hth.
    assert (Hwa := Hth (t, a) Hpa).
    simpl in Hwa.
    simpl. rewrite Ha.
    rewrite (absorbs_all Habs a).
    apply closed_app_subst_ground; [exact Hground|].
    intros u Hu.
    apply wf_ty_frees in Hwa. simpl in Hwa.
    destruct (Hcovers u (Hwa u Hu)) as [b Hb].
    pose proof (@callee_env_agree_aux1 u g b Hb) as Hpair.
    apply (in_map (@fst tsym ty)) in Hpair.
    simpl in Hpair. exact Hpair. }
  assert (HDreserved : ~ In this_sym (tyreqs (cA PhiD))).
  { apply (@callee_env_agree_aux13 Sg empty_ctx D PhiD Wtel).
    intro Hbad. exact Hbad. }
  assert (HsigmaD : sigma_regular (cS PhiD) (cA PhiD)).
  { apply (@tele_sigma_regular Sg empty_ctx D PhiD Wtel).
    split; [intros x Hx; destruct Hx | constructor]. }
  assert (HabsD : absorbs (callee_grounding g Phi th (cA PhiD)) (cS PhiD)).
  { intros t tau Hlk.
    assert (Hpair : In (t, tau) (cS PhiD))
      by (apply callee_env_agree_aux1; exact Hlk).
    assert (Htin : In t (tyreqs (cA PhiD))).
    { apply (proj1 HsigmaD). apply in_map_iff.
      exists (t, tau). split; [reflexivity|exact Hpair]. }
    assert (Htaufree : incl (ty_frees tau) (tyreqs (cA PhiD))).
    { destruct HsigmaD as [_ Hrng]. rewrite Forall_forall in Hrng.
      exact (Hrng (t, tau) Hpair). }
    rewrite (@callee_grounding_lookup g Phi th (cA PhiD) t Htin).
    rewrite (@callee_grounding_reads g Phi th (cA PhiD) tau Htaufree).
    f_equal.
    unfold merge_respects in Hmerge. rewrite Forall_forall in Hmerge.
    exact (f_equal (app_subst g) (Hmerge (t, tau) Hpair)). }
  assert (Hctx : ctx_grounded (callee_grounding g Phi th (cA PhiD)) PhiD).
  { split; [exact Hcgground|].
    split; [exact HabsD|].
    split.
    - intros t Hin. eexists.
      apply callee_grounding_lookup. exact Hin.
    - split; [exact HsigmaD|exact HDreserved]. }
  split; [exact Hctx|].
  assert (Hkeys : map fst d' = filter (fun i => negb (is_ty_item i)) (cA PhiD)).
  { rewrite (@xcompose_keys d sm d' Hcompose). exact Hsmdom. }
  split.
  - rewrite Forall_forall. intros i Hin.
    destruct (is_ty_item i) eqn:Hty.
    + left. reflexivity.
    + right.
      assert (Hif : In i (filter (fun j => negb (is_ty_item j)) (cA PhiD))).
      { apply filter_In. split; [exact Hin|].
        apply Bool.negb_true_iff. exact Hty. }
      assert (Himap : In i (map fst sm)) by (rewrite Hsmdom; exact Hif).
      destruct (callee_env_agree_aux3 i sm Himap) as [sat' Hlookup].
      destruct (@xcompose_first d sm d' i sat' Hcompose Hlookup)
        as [en [_ Hen]].
      exists en. exact Hen.
  - split.
    + rewrite Forall_forall. intros [req en] Hin.
      destruct (@xcompose_entry d sm d' (req, en) Hcompose Hin)
        as [sat' [Hsmin Hden]].
      simpl in Hsmin, Hden.
      rewrite Forall_forall in Hsm.
      destruct (Hsm (req, sat') Hsmin) as [Hsata Hmatch].
      rewrite Forall_forall in Hdagree.
      assert (Hae : xdentry_agree g sat' en).
      { apply (Hdagree (sat', en)). apply lookup_xdenv_in. exact Hden. }
      assert (HreqD : In req (cA PhiD)).
      { assert (Hinmap : In req (map fst d')).
        { apply in_map_iff. exists (req, en). split; [reflexivity|exact Hin]. }
        rewrite Hkeys in Hinmap. apply filter_In in Hinmap. exact (proj1 Hinmap). }
      assert (HreqNon : is_ty_item req = false).
      { assert (Hinmap : In req (map fst d')).
        { apply in_map_iff. exists (req, en). split; [reflexivity|exact Hin]. }
        rewrite Hkeys in Hinmap. apply filter_In in Hinmap.
        apply Bool.negb_true_iff. exact (proj2 Hinmap). }
      eapply xdentry_agree_retype; [exact WF | exact Hae | | | ].
      * symmetry. eapply callee_env_agree_aux6; [exact Habs| |exact Hmatch].
        intros u Hu.
        exact (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0 req HreqD u
                 (in_or_app _ _ _ (or_intror (in_or_app _ _ _ (or_introl Hu))))).
      * intros t Hdep.
        assert (Hdep' : In t (dep_tys Sg req)).
        { pose proof (@callee_env_agree_aux7 Sg (cS Phi) th req sat'
                        Hmatch) as Heqdep.
          simpl in Heqdep. rewrite Heqdep. exact Hdep. }
        rewrite callee_grounding_reads.
        -- rewrite Forall_forall in Hsat.
           specialize (Hsat req).
           destruct (Hsat HreqD) as [Hty|[_ Hcoh]].
           ++ rewrite HreqNon in Hty. discriminate.
           ++ rewrite Forall_forall in Hcoh.
              rewrite (Hcoh t Hdep'). symmetry. apply absorbs_all. exact Habs.
        -- intros u Hu. simpl in Hu. destruct Hu as [Heq|Hu]; [subst u|contradiction].
           exact (@tele_deps_closed Sg empty_ctx D PhiD Wtel Hdep0 req
                    HreqD HreqNon t Hdep').
      * intros rt m ap rt' ap' Dm Sm tau Hi Hj Hm Htau.
        simpl in Hi, Hj.
        subst sat'. subst req.
        simpl in Hmatch.
        injection Hmatch as Hrt Hap.
        destruct (@callee_env_agree_aux11 Sg empty_ctx D PhiD rt' m ap' Dm Sm
                    Wtel Hmth0 HreqD Hm) as [PhiDm [HtcDm [Hapinc Hapcov]]].
        assert (Hdomeq : map fst ap = map fst ap').
        { pose proof (f_equal (map (@fst tsym ty)) Hap) as E.
          unfold map_snd in E. rewrite !map_map in E. simpl in E.
          symmetry. exact E. }
        assert (Hwf_tau :
          wf_ty Sg (add_item (ITy this_sym) PhiDm) tau).
        { destruct (wf_mth_decl WF _ Hm) as [PhiDm0 [WtDm [Hps Hret]]].
          assert (EDm : PhiDm0 = PhiDm).
          { pose proof (wf_tele_ctx WtDm) as HtcDm0. congruence. }
          subst PhiDm0.
          destruct Htau as [HtauIn|Heq].
          - rewrite Forall_forall in Hps. exact (Hps tau HtauIn).
          - subst tau. exact Hret. }
        assert (Hfree_tau :
          incl (ty_frees tau) (this_sym :: tyreqs (cA PhiDm))).
        { apply wf_ty_frees in Hwf_tau. simpl in Hwf_tau. exact Hwf_tau. }
        apply callee_env_agree_aux5.
        intros t Ht.
        specialize (Hfree_tau t Ht).
        unfold interp_m. simpl.
        destruct (lookup_subst t ap) as [b|] eqn:Hb;
        destruct (lookup_subst t ap') as [a|] eqn:Ha.
        -- assert (Hreqreg :=
               @tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
                 (IMth rt' m ap') HreqD).
           assert (Hafree : incl (ty_frees a) (tyreqs (cA PhiD))).
           { intros u Hu. apply Hreqreg. simpl.
             apply in_or_app. left. apply in_or_app. right.
             apply in_flat_map. exists (t, a). split.
             - apply callee_env_agree_aux1. exact Ha.
             - exact Hu. }
           assert (Hbfree : incl (ty_frees b) (tyreqs (cA Phi))).
           { rewrite Forall_forall in HAreg.
             specialize (HAreg (IMth rt m ap) Hsata).
             intros u Hu. apply HAreg. simpl.
             apply in_or_app. left. apply in_or_app. right.
             apply in_flat_map. exists (t, b). split.
             - apply callee_env_agree_aux1. exact Hb.
             - exact Hu. }
           rewrite (callee_env_agree_aux14 rt b).
           2: { intro Hbad. exact (Hreserved (Hbfree _ Hbad)). }
           rewrite (callee_env_agree_aux14 rt' a).
           2: { intro Hbad. exact (HDreserved (Hafree _ Hbad)). }
           rewrite callee_grounding_reads by exact Hafree.
           assert (Hapeq :
             map_snd
               (fun x => app_subst (cS Phi) (app_subst th x)) ap' =
             map_snd (app_subst (cS Phi)) ap).
           { unfold map_snd in Hap |- *. rewrite map_map in Hap. exact Hap. }
           destruct (@callee_env_agree_aux12
             (fun x => app_subst (cS Phi) (app_subst th x))
             (app_subst (cS Phi))
             ap' ap t a Hapeq Ha) as [b' [Hb' Hab]].
           rewrite Hb in Hb'. injection Hb' as Heq. subst b'.
           rewrite <- (absorbs_all Habs b).
           exact (f_equal (app_subst g) (eq_sym Hab)).
        -- exfalso.
           assert (Hinap : In t (map fst ap)).
           { pose proof (@callee_env_agree_aux1 t ap b Hb) as Hp.
             apply (in_map (@fst tsym ty)) in Hp. simpl in Hp. exact Hp. }
           rewrite Hdomeq in Hinap.
           destruct (subst_chain_aux1 t ap' Hinap) as [x Hx].
           rewrite Ha in Hx. discriminate.
        -- exfalso.
           assert (Hinap : In t (map fst ap')).
           { pose proof (@callee_env_agree_aux1 t ap' a Ha) as Hp.
             apply (in_map (@fst tsym ty)) in Hp. simpl in Hp. exact Hp. }
           rewrite <- Hdomeq in Hinap.
           destruct (subst_chain_aux1 t ap Hinap) as [x Hx].
           rewrite Hb in Hx. discriminate.
        -- assert (Htthis : t = this_sym).
           { destruct Hfree_tau as [Heq|HinD]; [symmetry; exact Heq|].
             exfalso. apply (Hapcov t (@msig_frees_in _ _ _ Htau Ht)) in HinD. rewrite <- Hdomeq in HinD.
             destruct (subst_chain_aux1 t ap HinD) as [x Hx].
             rewrite Hb in Hx. discriminate. }
           subst t. simpl.
           assert (Hreqreg :=
             @tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
               (IMth rt' m ap') HreqD).
           assert (Hrtfree : incl (ty_frees rt') (tyreqs (cA PhiD))).
           { intros u Hu. apply Hreqreg. simpl.
             apply in_or_app. left. apply in_or_app. left.
             exact Hu. }
           rewrite callee_grounding_reads by exact Hrtfree.
           rewrite Hrt. symmetry. apply absorbs_all. exact Habs.
    + split.
      * rewrite Forall_forall. intros i Hin.
        pose proof (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
                      i Hin) as Hreg.
        simpl in Hreg. exact Hreg.
      * rewrite Forall_forall. intros [i en] Hin.
        assert (HiD : In i (cA PhiD)).
        { assert (Hi : In i (map fst d')).
          { apply in_map_iff. exists (i, en). split; [reflexivity|exact Hin]. }
          rewrite Hkeys in Hi. apply filter_In in Hi. exact (proj1 Hi). }
        pose proof (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
                      i HiD) as Hreg.
        simpl in Hreg. exact Hreg.
Qed.




Section XWorld.
Hypothesis WF : wf_gsig Sg.

  Lemma safety_case_bindty_aux1 : forall A u,
      In u (tyreqs A) -> In (ITy u) A.
  Proof.
    intros A u H. unfold tyreqs in H.
    apply in_flat_map in H. destruct H as [i [Hi Hu]].
    destruct i; simpl in Hu; try contradiction.
    destruct Hu as [E | []]. subst. exact Hi.
  Qed.

  Lemma safety_case_bindty_aux2 : forall g t tau0 A u,
      ~ In (ITy t) A ->
      In u (tyreqs A) ->
      lookup_subst u ((t, tau0) :: g) = lookup_subst u g.
  Proof.
    intros g t tau0 A u Hfresh Hu. simpl.
    destruct (Nat.eqb u t) eqn:E; [|reflexivity].
    apply Nat.eqb_eq in E. subst u.
    exfalso. apply Hfresh. apply safety_case_bindty_aux1. exact Hu.
  Qed.

  Lemma safety_case_bindty_aux3 : forall s tau A,
      incl (ty_frees tau) (tyreqs A) ->
      Forall (fun p => incl (ty_frees (snd p)) (tyreqs A)) s ->
      incl (ty_frees (app_subst s tau)) (tyreqs A).
  Proof.
    intros s tau A Htau Hs u Hu.
    destruct (@frees_app_subst_bound s tau u Hu)
      as [[p [Hp Hup]] | [Hut _]].
    - rewrite Forall_forall in Hs. exact (Hs p Hp u Hup).
    - exact (Htau u Hut).
  Qed.

  Lemma safety_case_bindty_aux4 : forall t s tau,
      lookup_subst t s = Some tau -> In t (map fst s).
  Proof.
    intros t s. induction s as [|[u a] r IH]; intros tau H; simpl in *.
    - discriminate.
    - destruct (Nat.eqb t u) eqn:E.
      + apply Nat.eqb_eq in E. subst u. left. reflexivity.
      + right. apply IH with (tau := tau). exact H.
  Qed.

  Lemma safety_case_bindty_aux5 : forall t s tau,
      lookup_subst t s = Some tau -> In (t, tau) s.
  Proof.
    intros t s. induction s as [|[u a] r IH]; intros tau H; simpl in H.
    - discriminate.
    - destruct (Nat.eqb t u) eqn:E.
      + apply Nat.eqb_eq in E. subst u. injection H as ->.
        left. reflexivity.
      + right. apply IH. exact H.
  Qed.

  Lemma safety_case_bindty_aux6 : forall x G tau,
      lookup_tenv x G = Some tau -> In (x, tau) G.
  Proof.
    intros x G. induction G as [|[y a] r IH]; intros tau H; simpl in H.
    - discriminate.
    - destruct (Nat.eqb x y) eqn:E.
      + apply Nat.eqb_eq in E. subst y. injection H as ->.
        left. reflexivity.
      + right. apply IH. exact H.
  Qed.

  (* The world a type bind leaves: the grounding extended at the fresh
     symbol, every old reading unchanged.  Shared by the expression-level
     rule and a functor body's bind. *)
  Lemma world_bindty_s : forall Phi t tau' d g,
      t <> this_sym ->
      ~ In (ITy t) (cA Phi) ->
      wf_ty Sg Phi tau' ->
      ctx_grounded g Phi ->
      xdenv_strong g (cA Phi) d ->
      ctx_grounded ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
                   (bind_ty t tau' Phi)
      /\ xdenv_strong ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
                    (cA (bind_ty t tau' Phi)) d
      /\ (forall u, In u (tyreqs (cA Phi)) ->
            lookup_subst u ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
            = lookup_subst u g).
  Proof.
    intros Phi t tau' d g Hneq Hfresh Htau' Hg Hd.
    destruct Hg as [Hground [Habs [Hcover Hsig]]].
    destruct Hsig as [Hsig Hreserved].
    destruct Hsig as [Hdom Hran].
    set (tau2 := app_subst g (app_subst (cS Phi) tau')).
    set (g2 := (t, tau2) :: g).
    assert (Hnot : ~ In t (tyreqs (cA Phi))).
    { intro H. apply Hfresh. apply safety_case_bindty_aux1. exact H. }
    assert (Hlook : forall u, In u (tyreqs (cA Phi)) ->
             lookup_subst u g2 = lookup_subst u g).
    { intros u Hu. unfold g2.
      apply safety_case_bindty_aux2 with (A := cA Phi); assumption. }
    assert (Hfree' : incl (ty_frees tau') (tyreqs (cA Phi)))
      by (apply wf_ty_frees in Htau'; exact Htau').
    assert (Hclosed2 : closed_ty tau2).
    { unfold tau2.
      rewrite (absorbs_all Habs).
      apply closed_app_subst_ground; [exact Hground|].
      intros u Hu. destruct (Hcover u (Hfree' u Hu)) as [a Ha].
      apply safety_case_bindty_aux4 in Ha. exact Ha. }
    assert (Hg2ground : grounding g2).
    { unfold g2. constructor; [exact Hclosed2|exact Hground]. }
    assert (Hg2abs : absorbs g2
             ((t, app_subst (cS Phi) tau') :: cS Phi)).
    { intros u a Hl. simpl in Hl.
      destruct (Nat.eqb u t) eqn:E.
      - injection Hl as Hl. subst a. apply Nat.eqb_eq in E. subst u.
        simpl. rewrite Nat.eqb_refl.
        unfold tau2. f_equal. symmetry.
        apply app_subst_frees_agree. intros z Hz. apply Hlook.
        eapply safety_case_bindty_aux3; eauto.
      - assert (Hu : In u (map fst (cS Phi))).
        { apply safety_case_bindty_aux4 in Hl. exact Hl. }
        assert (Hur : In u (tyreqs (cA Phi))) by (apply Hdom; exact Hu).
        unfold g2. simpl. rewrite E.
        rewrite (app_subst_frees_agree ((t, tau2) :: g) g a).
        + exact (Habs u a Hl).
        + intros z Hz. change (lookup_subst z g2 = lookup_subst z g).
          apply Hlook. rewrite Forall_forall in Hran.
          apply (Hran (u, a)); [|exact Hz].
          apply safety_case_bindty_aux5. exact Hl. }
    assert (Hg2cover : covers g2 (tyreqs (cA (bind_ty t tau' Phi)))).
    { intros u Hu. simpl in Hu. destruct Hu as [E | Hu].
      - subst u. exists tau2. simpl. rewrite Nat.eqb_refl. reflexivity.
      - destruct (Hcover u Hu) as [a Ha]. exists a.
        rewrite Hlook; assumption. }
    assert (Hg2sig : sigma_regular (cS (bind_ty t tau' Phi))
                                      (cA (bind_ty t tau' Phi))).
    { split.
      - simpl. intros u [E | Hu].
        + subst. left. reflexivity.
        + right. apply Hdom. exact Hu.
      - simpl. constructor.
        + intros u Hu. right.
          eapply safety_case_bindty_aux3; eauto.
        + eapply Forall_impl; [|exact Hran].
          intros [u a] Ha z Hz. right. exact (Ha z Hz). }
    assert (Hg2 : ctx_grounded g2 (bind_ty t tau' Phi)).
    { split; [exact Hg2ground|]. split; [exact Hg2abs|].
      split; [exact Hg2cover|]. split; [exact Hg2sig|].
      simpl. intros [Heq|Hin].
      - apply Hneq. exact Heq.
      - exact (Hreserved Hin). }
    assert (Hitem : forall i,
        incl (dep_tys Sg i ++ item_ty_frees i) (tyreqs (cA Phi)) ->
        app_subst_item g i = app_subst_item g2 i).
    { intros [u|v|f|rt m th|uf] Hi; simpl; try reflexivity.
      f_equal.
      - symmetry. apply app_subst_frees_agree. intros z Hz.
        apply Hlook. apply Hi. apply in_or_app. right.
        apply in_or_app. left. exact Hz.
      - unfold map_snd. apply map_ext_in. intros [z a] Hin.
        simpl. f_equal. symmetry. apply app_subst_frees_agree.
        intros q Hq. apply Hlook. apply Hi.
        apply in_or_app. right. apply in_or_app. right.
        apply in_flat_map. exists (z, a). split; assumption. }
    assert (Hd2 : xdenv_strong g2 (cA (bind_ty t tau' Phi)) d).
    { destruct Hd as [Hcov [Hentries [Hregs Hkeys]]].
      split.
      - simpl. constructor; [left; reflexivity|exact Hcov].
      - split.
        + rewrite Forall_forall in Hentries |- *.
          intros [i en] Hin. specialize (Hentries (i, en) Hin).
          rewrite Forall_forall in Hkeys.
          assert (Hiall :
              incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
                   (tyreqs (cA Phi))).
          { exact (Hkeys (i, en) Hin). }
          assert (Hi : incl (dep_tys Sg i ++ item_ty_frees i)
                            (tyreqs (cA Phi))).
          { intros u Hu. apply Hiall. apply in_app_or in Hu.
            destruct Hu as [Hu | Hu].
            - apply in_or_app. left. exact Hu.
            - apply in_or_app. right. apply in_or_app. left. exact Hu. }
          eapply (@xdentry_agree_retype g i g2 i en).
          * exact WF.
          * exact Hentries.
          * apply Hitem. exact Hi.
          * intros u Hu.
            change
              ((match lookup_subst u g with Some a => a | None => TAbs u end) =
               (match lookup_subst u g2 with Some a => a | None => TAbs u end)).
            rewrite (Hlook u); [reflexivity|].
            apply Hi. apply in_or_app. left. exact Hu.
          * intros rt m th rt' th' Dm Sm a Ei Ei' Hm Ha.
            subst i. simpl in Ei'. injection Ei' as Er Eth.
            subst rt' th'.
            unfold interp_m.
            apply app_subst_frees_agree. intros u Hu.
            symmetry. apply Hlook.
            apply Hiall. apply in_or_app. right. apply in_or_app. right.
            unfold method_item_frees. rewrite Hm.
            apply in_flat_map.
            exists (app_subst (this_subst rt) (app_subst th a)).
            split.
            -- apply in_map_iff. exists a. split; [reflexivity|].
               apply in_or_app. destruct Ha as [Ha | ->].
               ++ left. exact Ha.
               ++ right. simpl. left. reflexivity.
            -- exact Hu.
        + split.
          * simpl. constructor.
            -- intros u Hu. contradiction.
            -- eapply Forall_impl; [|exact Hregs].
               intros i Hi z Hz. right. exact (Hi z Hz).
          * eapply Forall_impl; [|exact Hkeys].
            intros [i en] Hi z Hz. right. exact (Hi z Hz). }
    split; [exact Hg2 | split; [exact Hd2 | exact Hlook]].
  Qed.

  Lemma world_bindval_s : forall Phi v Dv tauv w1 d g,
      g_val Sg v = Some (Dv, tauv) ->
      sat0 Sg Phi Dv ->
      xvt w1 (app_subst g tauv) ->
      ctx_grounded g Phi ->
      xdenv_strong g (cA Phi) d ->
      ctx_grounded g (add_item (IVal v) Phi)
      /\ xdenv_strong g (cA (add_item (IVal v) Phi)) ((IVal v, XDVal w1) :: d).
  Proof.
    intros Phi v Dv tauv w1 d g Hgv Hsat S1 Hg Hd.
    destruct Hsat as [PhiDv [Htc [_ Hreq]]].
    rewrite Forall_forall in Hreq.
    assert (Hreq' : forall u, In u (tyreqs Dv) -> avail Phi (ITy u)).
    { intros u Hu. apply Hreq. apply (tele_ctx_tyreqs _ Htc). exact Hu. }
    assert (Hd' : xdenv_strong g (cA (add_item (IVal v) Phi))
                     ((IVal v, XDVal w1) :: d)).
    { destruct Hd as [Hcov [Hentries [Hreg Hkeys]]].
      split.
      - simpl.
        constructor.
        + right. exists (XDVal w1). simpl.
          rewrite Nat.eqb_refl. reflexivity.
        + eapply Forall_impl; [| exact Hcov].
          intros i [Hty | [en Hen]].
          * left; exact Hty.
          * right. simpl.
            destruct (item_eqb i (IVal v)) eqn:Hi.
            -- exists (XDVal w1). reflexivity.
            -- exists en. exact Hen.
      - split.
        + constructor.
          * change (match g_val Sg v with
                    | Some (_, tv) => xvt w1 (app_subst g tv)
                    | None => False
                    end).
            rewrite Hgv. exact S1.
          * exact Hentries.
        + split.
          * simpl. constructor.
            -- intros u Hu.
               simpl in Hu. rewrite Hgv in Hu. simpl in Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | []].
               apply wf_ty_frees_aux3.
               apply avail_ty_in. exact (Hreq' u Hu).
            -- exact Hreg.
          * simpl. constructor.
            -- intros u Hu.
               simpl in Hu. rewrite Hgv in Hu. simpl in Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | []].
               apply wf_ty_frees_aux3.
               apply avail_ty_in. exact (Hreq' u Hu).
            -- exact Hkeys.
    }
    assert (Hg' : ctx_grounded g (add_item (IVal v) Phi)).
    { destruct Hg as [Hgr [Hab2 Hcv]].
      split; [exact Hgr |]. split; simpl; assumption. }
    split; [exact Hg' | exact Hd'].
  Qed.

  Lemma safety_case_bindfn_aux1 : forall (l : list tsym) (t : tsym) (tau : ty),
      lookup_subst t (map (fun t0 => (t0, TAbs t0)) l) = Some tau ->
      tau = TAbs t.
  Proof.
    intros l. induction l as [| a l IH]; intros t tau H; simpl in H.
    - discriminate.
    - destruct (Nat.eqb t a) eqn:E.
      + apply Nat.eqb_eq in E. subst a. injection H as H. subst tau. reflexivity.
      + apply IH. exact H.
  Qed.

  (* Private helper: reading any type through an identity application is a
     no-op (on covered symbols by aux1, elsewhere because the lookup fails). *)
  Lemma safety_case_bindfn_aux2 : forall (l : list tsym) (tau : ty),
      app_subst (map (fun t0 => (t0, TAbs t0)) l) tau = tau.
  Proof.
    intros l tau. induction tau using ty_ind'; simpl; try reflexivity.
    - destruct (lookup_subst t (map (fun t0 => (t0, TAbs t0)) l)) as [tau'|] eqn:E.
      + apply safety_case_bindfn_aux1 in E. exact E.
      + reflexivity.
    - f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. destruct x; reflexivity.
    - f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. reflexivity.
  Qed.

  Lemma safety_case_bindfn_aux3 : forall (D : tele) (tau : ty),
      app_subst (id_app D) tau = tau.
  Proof.
    intros D tau. unfold id_app. apply safety_case_bindfn_aux2.
  Qed.

  (* Private helper: the fn entry's agreement, assembled from the closure's
     own grounding, the two signature equations, and the callee world's
     agreement (whose two conjuncts are exactly tele_covered and the inner
     entry-wise recursion). *)
  Lemma safety_case_bindfn_aux4 :
    forall (g0 : subst) (f0 : fsym) (Df0 : tele) (Sf0 : fnsig) (f'0 : fsym)
           (D0 : tele) (S0 : fnsig) (body0 : xexpr) (PhiD0 : ctx)
           (gc0 : subst) (dc0 : xdenv),
      g_fn Sg f0 = Some (Df0, Sf0, None) ->
      g_fn Sg f'0 = Some (D0, S0, None) -> xp_fn P f'0 = Some body0 ->
      tele_ctx D0 = Some PhiD0 ->
      grounding gc0 ->
      covers gc0 (tyreqs (cA PhiD0)) ->
      absorbs gc0 (cS PhiD0) ->
      map (app_subst gc0) (fs_params S0) = map (app_subst g0) (fs_params Sf0) ->
      app_subst gc0 (fs_ret S0) = app_subst g0 (fs_ret Sf0) ->
      xdenv_strong gc0 (cA PhiD0) dc0 ->
      xdentry_agree g0 (IFn f0) (XDClo f'0 dc0).
  Proof.
    intros g0 f0 Df0 Sf0 f'0 D0 S0 body0 PhiD0 gc0 dc0
           Hf0 Hf'0 Hb0 Htc Hgr Hcov Habs Hp Hr Hda.
    simpl. rewrite Hf0, Hf'0, Hb0. simpl.
    exists PhiD0, gc0.
    split; [exact Htc |].
    split; [exact Hgr |].
    split; [exact Hcov |].
    split; [exact Habs |].
    split; [exact Hp |].
    split; [exact Hr |].
    destruct Hda as [Hc1 [Hc2 [Hreg Hkeys]]].
    split; [exact Hc1 |].
    split; [exact Hkeys |].
    revert Hc2. generalize dc0. intros dd.
    induction dd as [| p r IH]; intros Hall.
    - exact I.
    - simpl. inversion Hall as [| q qs Hq Hqs Heq]; subst.
      split; [exact Hq | apply IH; exact Hqs].
  Qed.

  Lemma safety_case_bindfn_aux5 : forall (D : tele) (i : item),
      app_subst_item (id_app D) i = i.
  Proof.
    intros D i. destruct i as [t | v | f | rt m th | uf]; simpl; try reflexivity.
    f_equal.
    - apply safety_case_bindfn_aux3.
    - unfold map_snd. induction th as [| [a b] r IH]; simpl; [reflexivity |].
      rewrite safety_case_bindfn_aux3, IH. reflexivity.
  Qed.

  (* The world a fn bind leaves: shared by the expression-level rule and a
     functor body's bind. *)
  Lemma world_bindfn_s : forall Phi f Df Sf f' D' S' body sm d g,
      g_fn Sg f = Some (Df, Sf, None) ->
      g_fn Sg f' = Some (D', S', None) -> xp_fn P f' = Some body ->
      sat0 Sg Phi Df ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      sig_eq (cS Phi) S' Sf ->
      ctx_grounded g Phi ->
      xdenv_strong g (cA Phi) d ->
      exists dc, xcompose d sm = Some dc
      /\ ctx_grounded g (add_item (IFn f) Phi)
      /\ xdenv_strong g (cA (add_item (IFn f) Phi)) ((IFn f, XDClo f' dc) :: d).
  Proof.
    intros Phi f Df Sf f' D' S' body sm d g
           Hf Hf' Hb Hsf Hs' Hsm Hsig Hg Hd.
    pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth Wfun].
    destruct (Wfn f' D' S' None Hf') as [PhiD [Wt' [Wps' [Wrt' _]]]].
    pose proof (wf_tele_ctx Wt') as Htc.
    destruct Hs' as [PhiD1 [Htc1 [Hsat' Hav']]].
    rewrite Htc in Htc1. injection Htc1 as Htc1. subst PhiD1.
    destruct Hsm as [PhiD2 [Htc2 [Hdom Hmm]]].
    rewrite Htc in Htc2. injection Htc2 as Htc2. subst PhiD2.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    (* the identity application's entries are wf: its range is the available
       type symbols of the provider's merged telescope (sat0's third
       conjunct) *)
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app (cA PhiD))).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    (* satisfaction and the map's correctness, restated at the identity
       application of the *merged* items — extensionally the same
       substitution as id_app D' *)
    assert (Hsat : sat Sg Phi (id_app (cA PhiD)) D').
    { exists PhiD. split; [exact Htc | exact Hsat']. }
    assert (Hsm' : smap_ok Phi (id_app (cA PhiD)) sm D').
    { exists PhiD. split; [exact Htc |]. split; [exact Hdom |].
      eapply Forall_impl; [| exact Hmm].
      intros p [Hin Hmt]. split; [exact Hin |].
      unfold item_matches in Hmt |- *.
      rewrite safety_case_bindfn_aux5 in Hmt.
      rewrite safety_case_bindfn_aux5. exact Hmt. }
    (* the captured world *)
    assert (Hcall : exists dc, xcompose d sm = Some dc
              /\ ctx_grounded
                   (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD)) PhiD
              /\ xdenv_strong
                   (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD))
                   (cA PhiD) dc).
    { eapply xcallee_env_strong;
        [ exact WF | exact Wt' | exact Hsat | exact Hwfth | exact Hsm'
        | exact Hg | exact Hd ]. }
    destruct Hcall as [dc [Hcomp [Hcgc Hdc]]].
    (* reading a PhiD-covered type through the closure's grounding is reading
       it through σ and then g: the application is the identity *)
    assert (Hgcr : forall t0, incl (ty_frees t0) (tyreqs (cA PhiD)) ->
              app_subst (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD)) t0
              = app_subst g (app_subst (cS Phi) t0)).
    { intros t0 Hincl. rewrite callee_grounding_reads by exact Hincl.
      rewrite safety_case_bindfn_aux3. reflexivity. }
    assert (Hpar : map (app_subst
                          (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD)))
                       (fs_params S')
                   = map (app_subst g) (fs_params Sf)).
    { transitivity (map (app_subst g) (map (app_subst (cS Phi)) (fs_params S'))).
      - rewrite map_map. apply map_ext_in. intros tau0 Hin.
        apply Hgcr. rewrite Forall_forall in Wps'.
        pose proof (Wps' tau0 Hin) as Hw0. apply wf_ty_frees in Hw0. exact Hw0.
      - destruct Hsig as [Hsp _]. rewrite Hsp, map_map.
        apply map_ext_in. intros tau0 _. apply Habs. }
    assert (Hrt : app_subst
                    (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD))
                    (fs_ret S')
                  = app_subst g (fs_ret Sf)).
    { assert (Hfr : incl (ty_frees (fs_ret S')) (tyreqs (cA PhiD))).
      { pose proof Wrt' as Hw. apply wf_ty_frees in Hw. exact Hw. }
      rewrite Hgcr by exact Hfr.
      destruct Hsig as [_ Hsr]. rewrite Hsr. apply Habs. }
    assert (Hnew : xdentry_agree g (IFn f) (XDClo f' dc)).
    { destruct Hcgc as [Hgr0 [Hab0 [Hcov0 _]]].
      eapply safety_case_bindfn_aux4;
        [ exact Hf | exact Hf' | exact Hb | exact Htc | exact Hgr0 | exact Hcov0
        | exact Hab0 | exact Hpar | exact Hrt | exact Hdc ]. }
    assert (Hg' : ctx_grounded g (add_item (IFn f) Phi)).
    { destruct Hg as [Hgr [Hab [Hcv [Hsr Hth]]]].
      split; [exact Hgr |]. split; [exact Hab |].
      split; [exact Hcv |]. split; [exact Hsr | exact Hth]. }
    destruct Hsf as [PhiDf [Htcf [_ Havf]]].
    assert (Hdepf : incl (dep_tys Sg (IFn f) ++ item_ty_frees (IFn f)
                          ++ method_item_frees Sg (IFn f))
                         (tyreqs (cA Phi))).
    { intros u Hu. simpl in Hu. rewrite Hf in Hu. simpl in Hu.
      apply in_app_or in Hu. destruct Hu as [Hu | Hu];
        [| simpl in Hu; contradiction].
      apply wf_ty_frees_aux3. apply avail_ty_in.
      rewrite Forall_forall in Havf. apply Havf.
      apply (proj2 (tele_ctx_tyreqs Df Htcf u)). exact Hu. }
    assert (Hd' : xdenv_strong g (cA (add_item (IFn f) Phi))
                             ((IFn f, XDClo f' dc) :: d)).
    { destruct Hd as [Hdcov [Hdentries [Hdreg Hdkeys]]].
      split.
      - simpl. constructor.
        + right. exists (XDClo f' dc). simpl.
          rewrite Nat.eqb_refl. reflexivity.
        + eapply Forall_impl; [| exact Hdcov].
          intros i [Hty | [en Hen]].
          * left; exact Hty.
          * right. simpl.
            destruct (item_eqb i (IFn f)) eqn:Hi.
            -- exists (XDClo f' dc). reflexivity.
            -- exists en. exact Hen.
      - split.
        + constructor; [exact Hnew | exact Hdentries].
        + split.
          * simpl. constructor; [exact Hdepf | exact Hdreg].
          * simpl. constructor; [exact Hdepf | exact Hdkeys] . }
    exists dc. split; [exact Hcomp | split; [exact Hg' | exact Hd']].
  Qed.

  Lemma safety_case_bindmth_aux1 : forall (s : subst) (tau0 tau : ty),
      app_subst s (app_subst (this_subst tau0) tau)
      = app_subst ((this_sym, app_subst s tau0) :: s) tau.
  Proof.
    intros s tau0 tau. unfold this_subst.
    induction tau using ty_ind'.
    - reflexivity.
    - simpl. destruct (Nat.eqb t this_sym); reflexivity.
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite IHForall. destruct x as [t0 tau1]; simpl in *. rewrite H. reflexivity.
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. reflexivity.
  Qed.

  (* Private helper: two receivers with the same reading through th and then
     g give the same This-extended reading of any method type whose frees the
     application (extended at this_sym) covers. *)
  Lemma safety_case_bindmth_aux2 :
    forall (g0 th0 : subst) (r1 r2 tau : ty),
      incl (ty_frees tau) (this_sym :: map fst th0) ->
      app_subst g0 (app_subst th0 r1) = app_subst g0 (app_subst th0 r2) ->
      app_subst g0 (app_subst th0 (app_subst (this_subst r1) tau))
      = app_subst g0 (app_subst th0 (app_subst (this_subst r2) tau)).
  Proof.
    intros g0 th0 r1 r2 tau Hincl HE.
    rewrite (safety_case_bindmth_aux1 th0 r1 tau).
    rewrite (safety_case_bindmth_aux1 th0 r2 tau).
    rewrite (app_subst_chain g0 ((this_sym, app_subst th0 r1) :: th0) tau)
      by (simpl; exact Hincl).
    rewrite (app_subst_chain g0 ((this_sym, app_subst th0 r2) :: th0) tau)
      by (simpl; exact Hincl).
    unfold map_snd. simpl. rewrite HE. reflexivity.
  Qed.

  Lemma safety_case_bindmth_auxN : forall g A B,
      app_subst g A = app_subst g B ->
      forall X, app_subst g (app_subst (this_subst A) X)
              = app_subst g (app_subst (this_subst B) X).
  Proof.
    intros g A B HE X. unfold this_subst.
    induction X using ty_ind'.
    - reflexivity.
    - simpl. destruct (Nat.eqb t this_sym); [exact HE | reflexivity].
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite IHForall. destruct x as [t0 tau0]; simpl in *.
      rewrite H. reflexivity.
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. reflexivity.
  Qed.

  Lemma safety_case_bindmth_aux_regular :
    forall Phi rt m Dm Sm th,
      g_mth Sg m = Some (Dm, Sm) ->
      wf_ty Sg Phi rt ->
      msat th Dm Sm ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      incl (method_item_frees Sg (IMth rt m th))
           (tyreqs (cA Phi)).
  Proof.
    intros Phi rt m Dm Sm th Hm Hrt Hsat Hth.
    exact (tele_items_regular_aux2 WF (@OkMth Sg Phi rt m Dm Sm th Hm Hrt Hsat Hth)).
  Qed.

  (* Private helper: the method entry's agreement, assembled from the
     provider closure's grounding, the receiver/argument/result equations and
     the callee world's agreement. *)
  Lemma safety_case_bindmth_aux3 :
    forall (g0 : subst) (rt : ty) (m0 : msym) (th0 : subst) (Dm0 : tele)
           (Sm0 : fnsig) (f'0 : fsym) (D0 : tele) (S0 : fnsig) (body0 : xexpr)
           (PhiD0 : ctx) (gc0 : subst) (dc0 : xdenv),
      g_mth Sg m0 = Some (Dm0, Sm0) ->
      g_fn Sg f'0 = Some (D0, S0, None) -> xp_fn P f'0 = Some body0 ->
      tele_ctx D0 = Some PhiD0 ->
      grounding gc0 ->
      covers gc0 (tyreqs (cA PhiD0)) ->
      absorbs gc0 (cS PhiD0) ->
      map (app_subst gc0) (fs_params S0)
        = app_subst g0 rt
          :: map (fun tau => app_subst g0
                    (app_subst (this_subst rt) (app_subst th0 tau)))
                 (fs_params Sm0) ->
      app_subst gc0 (fs_ret S0)
        = app_subst g0
            (app_subst (this_subst rt) (app_subst th0 (fs_ret Sm0))) ->
      xdenv_strong gc0 (cA PhiD0) dc0 ->
      xdentry_agree g0 (IMth rt m0 th0) (XDClo f'0 dc0).
  Proof.
    intros g0 rt m0 th0 Dm0 Sm0 f'0 D0 S0 body0 PhiD0 gc0 dc0
           Hm0 Hf'0 Hb0 Htc0 Hgr Hcov Hab Hp Hr Hda.
    simpl. rewrite Hm0, Hf'0, Hb0. simpl.
    exists PhiD0, gc0.
    split; [exact Htc0 |].
    split; [exact Hgr |].
    split; [exact Hcov |].
    split; [exact Hab |].
    split; [exact Hp |].
    split; [exact Hr |].
    destruct Hda as [Hc1 [Hc2 [Hreg Hkeys]]].
    split; [exact Hc1 |].
    split; [exact Hkeys |].
    revert Hc2. generalize dc0. intros dd.
    induction dd as [| p r IH]; intros Hall.
    - exact I.
    - simpl. inversion Hall as [| q qs Hq Hqs Heq]; subst.
      split; [exact Hq | apply IH; exact Hqs].
  Qed.

  (* Private helpers: the extended world's lookups (kept generic so that
     the method item's boolean equality is not unfolded). *)
  Lemma safety_case_bindmth_aux4 : forall (i : item) (en : xdentry) (dd : xdenv),
      lookup_xdenv i ((i, en) :: dd) = Some en.
  Proof.
    intros i en dd. simpl.
    rewrite (proj2 (item_eqb_eq i i) eq_refl). reflexivity.
  Qed.

  Lemma safety_case_bindmth_aux5 :
    forall (i i0 : item) (en0 en : xdentry) (dd : xdenv),
      lookup_xdenv i dd = Some en ->
      exists en', lookup_xdenv i ((i0, en0) :: dd) = Some en'.
  Proof.
    intros i i0 en0 en dd H. simpl.
    destruct (item_eqb i i0).
    - exists en0. reflexivity.
    - exists en. exact H.
  Qed.

  Lemma safety_case_bindmth_aux6 : forall (D : tele) (s : subst),
      map_snd (app_subst (id_app D)) s = s.
  Proof.
    intros D s. unfold map_snd. induction s as [| [t a] r IH]; simpl.
    - reflexivity.
    - rewrite safety_case_bindfn_aux3. unfold map_snd in IH. rewrite IH.
      reflexivity.
  Qed.

  Lemma safety_case_bindmth_aux8 : forall (Phi : ctx) (D1 D2 : tele)
                                          (sm : smap) (D : tele),
      smap_ok Phi (id_app D1) sm D -> smap_ok Phi (id_app D2) sm D.
  Proof.
    intros Phi D1 D2 sm D [PhiD [Htc [Hdom Hall]]].
    exists PhiD. split; [exact Htc |]. split; [exact Hdom |].
    eapply Forall_impl; [| exact Hall].
    intros p [Hin Hmatch]. split; [exact Hin |].
    unfold item_matches in Hmatch |- *.
    rewrite safety_case_bindfn_aux5 in Hmatch.
    rewrite safety_case_bindfn_aux5. exact Hmatch.
  Qed.

  Lemma world_bindmth_s : forall Phi tauk m Dm Sf th f' D' S' body sm
                                     tau0' rest d g,
      g_mth Sg m = Some (Dm, Sf) ->
      wf_ty Sg Phi tauk ->
      msat th Dm Sf ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      g_fn Sg f' = Some (D', S', None) -> xp_fn P f' = Some body ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      fs_params S' = tau0' :: rest ->
      ty_eq (cS Phi) tau0' tauk ->
      map (app_subst (cS Phi)) rest
        = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params Sf) ->
      app_subst (cS Phi) (fs_ret S')
        = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret Sf) ->
      ctx_grounded g Phi ->
      xdenv_strong g (cA Phi) d ->
      exists dc, xcompose d sm = Some dc
      /\ ctx_grounded g (add_item (IMth tauk m th) Phi)
      /\ xdenv_strong g (cA (add_item (IMth tauk m th) Phi))
                    ((IMth tauk m th, XDClo f' dc) :: d).
  Proof.
    intros Phi tauk m Dm Sf th f' D' S' body sm tau0' rest d g
           Hm Hwk Hsat Hthwf Hf' Hb Hs' Hsm Hpar0 Hteq Hprest Hpret Hg Hd.
    pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth Wfun].
    destruct (Wfn f' D' S' None Hf') as [PhiD' [Wt' [Wps' [Wrt' _]]]].
    pose proof (wf_tele_ctx Wt') as Htc'.
    (* the provider telescope's own satisfaction, at its merged identity *)
    destruct Hs' as [PhiD'2 [Htc'2 [Hsat' Hav']]].
    assert (EP : PhiD'2 = PhiD') by congruence. subst PhiD'2.
    assert (Hsatfull : sat Sg Phi (id_app (cA PhiD')) D')
      by (exists PhiD'; split; [exact Htc' | exact Hsat']).
    pose proof (@safety_case_bindmth_aux8 Phi D' (cA PhiD') sm D' Hsm) as Hsm2.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    (* the identity application's range is well-formed: S-Pass availability *)
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p))
                           (id_app (cA PhiD'))).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    (* the captured world, and the provider's own grounding *)
    destruct (@xcallee_env_strong g Phi (id_app (cA PhiD')) sm D' d PhiD'
                WF Wt' Hsatfull Hwfth Hsm2 Hg Hd)
      as [dc [Hcomp [Hcgc Hdc]]].
    assert (Hgcr : forall t0, incl (ty_frees t0) (tyreqs (cA PhiD')) ->
              app_subst (callee_grounding g Phi (id_app (cA PhiD')) (cA PhiD'))
                        t0
              = app_subst g (app_subst (cS Phi) t0)).
    { intros t0 Hincl. rewrite callee_grounding_reads by exact Hincl.
      rewrite safety_case_bindfn_aux3. reflexivity. }
    (* the provider's parameter list splits into receiver and arguments *)
    rewrite Hpar0 in Wps'.
    inversion Wps' as [| tt rr Wp0 Wpr Heqw]; subst.
    (* the two signature equations of the new entry *)
    assert (Hpar : map (app_subst (callee_grounding g Phi
                          (id_app (cA PhiD')) (cA PhiD')))
                       (fs_params S')
                   = app_subst g tauk
                     :: map (fun tau1 => app_subst g
                               (app_subst (this_subst tauk)
                                          (app_subst th tau1)))
                            (fs_params Sf)).
    { rewrite Hpar0. simpl. f_equal.
      - rewrite Hgcr by (exact (wf_ty_frees Wp0)).
        unfold ty_eq in Hteq. rewrite Hteq. apply Habs.
      - transitivity (map (app_subst g) (map (app_subst (cS Phi)) rest)).
        + rewrite map_map. apply map_ext_in. intros t1 Hin. apply Hgcr.
          rewrite Forall_forall in Wpr. exact (wf_ty_frees (Wpr t1 Hin)).
        + rewrite Hprest, map_map. apply map_ext_in. intros t1 Hin.
          unfold interp_m. rewrite Habs.
          apply safety_case_bindmth_auxN. apply Habs. }
    assert (Hrt : app_subst (callee_grounding g Phi
                     (id_app (cA PhiD')) (cA PhiD')) (fs_ret S')
                  = app_subst g
                      (app_subst (this_subst tauk)
                                 (app_subst th (fs_ret Sf)))).
    { rewrite Hgcr by (exact (wf_ty_frees Wrt')).
      rewrite Hpret. unfold interp_m. rewrite Habs.
      apply safety_case_bindmth_auxN. apply Habs. }
    assert (Hnew : xdentry_agree g (IMth tauk m th) (XDClo f' dc)).
    { destruct Hcgc as [Hgr0 [Hab0 [Hcov0 _]]].
      eapply safety_case_bindmth_aux3;
        [ exact Hm | exact Hf' | exact Hb | exact Htc' | exact Hgr0 | exact Hcov0
        | exact Hab0 | exact Hpar | exact Hrt | exact Hdc ]. }
    assert (Hg' : ctx_grounded g (add_item (IMth tauk m th) Phi)).
    { destruct Hg as [Hgr [Hab Hcv]].
      split; [exact Hgr |]. split; [exact Hab | exact Hcv]. }
    assert (Hmethodreg :
        incl (method_item_frees Sg (IMth tauk m th))
             (tyreqs (cA Phi))).
    { eapply safety_case_bindmth_aux_regular.
      - exact Hm.
      - exact Hwk.
      - exact Hsat.
      - exact Hthwf. }
    assert (Hd' : xdenv_strong g (cA (add_item (IMth tauk m th) Phi))
                             ((IMth tauk m th, XDClo f' dc) :: d)).
    { destruct Hd as [Hdcov [Hdentries [Hdreg Hdkeys]]].
      split.
      - simpl. constructor.
        + right. exists (XDClo f' dc). apply safety_case_bindmth_aux4.
        + eapply Forall_impl; [| exact Hdcov].
          intros i [Hty | [en Hen]].
          * left; exact Hty.
          * right. eapply safety_case_bindmth_aux5. exact Hen.
      - split.
        + constructor; [exact Hnew | exact Hdentries].
        + split.
          * simpl. constructor.
            -- intros u Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | Hu].
               ++ simpl in Hu. destruct Hu.
               ++ apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                  ** apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                     --- exact (wf_ty_frees Hwk u Hu).
                     --- apply in_flat_map in Hu.
                         destruct Hu as [p [Hp Hu]].
                         rewrite Forall_forall in Hthwf.
                         exact (wf_ty_frees (Hthwf p Hp) u Hu).
                  ** exact (Hmethodreg u Hu).
            -- exact Hdreg.
          * simpl. constructor.
            -- intros u Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | Hu].
               ++ simpl in Hu. destruct Hu.
               ++ apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                  ** apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                     --- exact (wf_ty_frees Hwk u Hu).
                     --- apply in_flat_map in Hu.
                         destruct Hu as [p [Hp Hu]].
                         rewrite Forall_forall in Hthwf.
                         exact (wf_ty_frees (Hthwf p Hp) u Hu).
                  ** exact (Hmethodreg u Hu).
            -- exact Hdkeys. }
    exists dc. split; [exact Hcomp | split; [exact Hg' | exact Hd']].
  Qed.

  Lemma xdenv_strong_push : forall g A d i en,
      xdenv_strong g A d ->
      xdentry_agree g i en ->
      incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
           (tyreqs A) ->
      xdenv_strong g (i :: A) ((i, en) :: d).
  Proof.
    intros g A d i en [Hcov [Hent [Hreg Hkeys]]] Hen Hi.
    assert (Hw : incl (tyreqs A) (tyreqs (i :: A))).
    { intros t Ht. unfold tyreqs. simpl. apply in_or_app. right. exact Ht. }
    split; [| split; [| split]].
    - constructor.
      + right. exists en. apply safety_case_bindmth_aux4.
      + eapply Forall_impl; [| exact Hcov].
        intros j [Hty | [en' Hen']]; [left; exact Hty | right].
        eapply safety_case_bindmth_aux5. exact Hen'.
    - constructor; [exact Hen | exact Hent].
    - constructor.
      + intros t Ht. apply Hw. apply Hi. exact Ht.
      + eapply Forall_impl; [| exact Hreg].
        intros j Hj t Ht. apply Hw. exact (Hj t Ht).
    - constructor.
      + intros t Ht. apply Hw. apply Hi. exact Ht.
      + eapply Forall_impl; [| exact Hkeys].
        intros [j e'] Hj t Ht. apply Hw. exact (Hj t Ht).
  Qed.

  (* An entry agrees under any grounding that reads its type material the
     same way. *)
  Lemma xdentry_agree_reground : forall g1 g2 i en,
      xdentry_agree g1 i en ->
      (forall x, In x (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
                 app_subst g1 (TAbs x) = app_subst g2 (TAbs x)) ->
      xdentry_agree g2 i en.
  Proof.
    intros g1 g2 i en Hag Hx.
    apply (@xdentry_agree_retype g1 i g2 i en WF Hag).
    - destruct i as [t|v|f|rt m th|uf]; simpl; try reflexivity.
      f_equal.
      + apply dentry_agree_retype_aux1. intros x Hin. apply Hx.
        apply in_or_app. right. apply in_or_app. left. simpl.
        apply in_or_app. left. exact Hin.
      + unfold map_snd. apply map_ext_in. intros [z a] Hin. simpl. f_equal.
        apply dentry_agree_retype_aux1. intros x Hx'. apply Hx.
        apply in_or_app. right. apply in_or_app. left. simpl.
        apply in_or_app. right. apply in_flat_map. exists (z, a).
        split; assumption.
    - intros x Hin. apply Hx. apply in_or_app. left. exact Hin.
    - intros rt m th rt' th' Dm Sm tau E1 E2 Hm Htau.
      rewrite E1 in E2. injection E2 as <- <-. subst i.
      unfold interp_m. apply dentry_agree_retype_aux1. intros x Hin. apply Hx.
      apply in_or_app. right. apply in_or_app. right. simpl. rewrite Hm.
      apply in_flat_map.
      exists (app_subst (this_subst rt) (app_subst th tau)).
      split; [| exact Hin].
      apply in_map_iff. exists tau. split; [reflexivity |].
      apply in_or_app.
      destruct Htau as [Htau | ->]; [left; exact Htau | right; left; reflexivity].
  Qed.

End XWorld.

(* ===================== The relaxed invariant ============================== *)

(* What a scope needs of its environment: each item in force is covered, and
   the entry a lookup of a non-type item in force finds agrees.  Entries the
   context does not reach (shadowed ones, or ones for items not in force,
   such as the platform's provisions main does not assume) are unconstrained. *)
Definition xdenv_agree (g : subst) (A : list item) (d : xdenv) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_xdenv i d = Some en) A
  /\ (forall i en, In i A -> is_ty_item i = false ->
        lookup_xdenv i d = Some en -> xdentry_agree g i en)
  /\ Forall (fun i =>
       incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
            (tyreqs A)) A.

Definition agree_on (A : list item) (d1 d2 : xdenv) : Prop :=
  forall i, In i A -> is_ty_item i = false -> lookup_xdenv i d1 = lookup_xdenv i d2.

Lemma strong_agree : forall g A d, xdenv_strong g A d -> xdenv_agree g A d.
Proof.
  intros g A d [H1 [H2 [H3 _]]]. split; [exact H1 | split; [| exact H3]].
  intros i en _ _ Hl. rewrite Forall_forall in H2. exact (H2 _ (lookup_xdenv_in _ _ Hl)).
Qed.

Lemma agree_ext : forall g A d1 d2,
  xdenv_agree g A d1 -> agree_on A d1 d2 -> xdenv_agree g A d2.
Proof.
  intros g A d1 d2 [H1 [H2 H3]] He. split; [| split; [| exact H3]].
  - rewrite Forall_forall in H1 |- *. intros i Hi.
    destruct (H1 i Hi) as [Ht | [en Hen]]; [left; exact Ht |].
    destruct (is_ty_item i) eqn:Ht; [left; reflexivity |].
    right. exists en. rewrite <- (He i Hi Ht). exact Hen.
  - intros i en Hi Ht Hl. rewrite <- (He i Hi Ht) in Hl. exact (H2 i en Hi Ht Hl).
Qed.

Lemma agree_on_cons : forall A i en d1 d2,
  agree_on A d1 d2 -> agree_on (i :: A) ((i, en) :: d1) ((i, en) :: d2).
Proof.
  intros A i en d1 d2 H j [E | Hj] Ht; simpl.
  - subst i. rewrite (proj2 (item_eqb_eq j j) eq_refl). reflexivity.
  - destruct (item_eqb j i); [reflexivity | exact (H j Hj Ht)].
Qed.

(* A scope's environment, restricted to what its context reaches. *)
Lemma agree_restrict : forall g A d,
  xdenv_agree g A d -> exists dA, xdenv_strong g A dA /\ agree_on A dA d.
Proof.
  intros g A d [H1 [H2 H3]].
  assert (Hdef : Forall (fun p => exists en, lookup_xdenv (snd p) d = Some en) (id_smap A)).
  { apply Forall_forall. intros [a b] Hp. destruct (id_smap_in _ _ _ Hp) as [<- [Ha Hn]].
    rewrite Forall_forall in H1. destruct (H1 a Ha) as [Ht | Hen]; [congruence | exact Hen]. }
  destruct (@xcompose_defined d (id_smap A) Hdef) as [dA HdA].
  assert (Hag : agree_on A dA d).
  { intros i Hi Ht.
    destruct (@xcompose_first d (id_smap A) dA i i HdA (@lookup_smap_id A i Hi Ht)) as [en [E1 E2]].
    rewrite E1, E2. reflexivity. }
  exists dA. split; [| exact Hag]. split; [| split; [| split; [exact H3 |]]].
  - rewrite Forall_forall in H1 |- *. intros i Hi.
    destruct (is_ty_item i) eqn:Ht; [left; reflexivity |].
    right. destruct (H1 i Hi) as [Ht' | [en Hen]]; [congruence |].
    exists en. rewrite (Hag i Hi Ht). exact Hen.
  - rewrite Forall_forall. intros [i en] Hp.
    destruct (@xcompose_entry d (id_smap A) dA (i, en) HdA Hp) as [s' [Hs Hl]].
    simpl in Hs, Hl. destruct (id_smap_in _ _ _ Hs) as [<- [Hi Hn]].
    exact (H2 i en Hi Hn Hl).
  - rewrite Forall_forall. intros [i en] Hp.
    destruct (@xcompose_entry d (id_smap A) dA (i, en) HdA Hp) as [s' [Hs _]].
    simpl in Hs. destruct (id_smap_in _ _ _ Hs) as [<- [Hi _]].
    rewrite Forall_forall in H3. exact (H3 i Hi).
Qed.

(* A satisfaction map reads only non-type items in force. *)
Lemma xcompose_agree_on : forall Phi th sm D d1 d2,
  smap_ok Phi th sm D -> agree_on (cA Phi) d1 d2 -> xcompose d1 sm = xcompose d2 sm.
Proof.
  intros Phi th sm D d1 d2 [PhiD [_ [Hdom Hall]]] Hag.
  assert (Hreq : forall p, In p sm -> is_ty_item (fst p) = false).
  { intros p Hp. assert (Hin : In (fst p) (map fst sm)) by (apply in_map; exact Hp).
    rewrite Hdom in Hin. apply filter_In in Hin. apply Bool.negb_true_iff. exact (proj2 Hin). }
  clear Hdom. induction sm as [|[req s'] r IH]; simpl; [reflexivity |].
  inversion Hall as [|? ? [Hin Hm] Hr]; subst.
  pose proof (callee_env_agree_aux4 (Hreq (req, s') (or_introl eq_refl)) Hm) as Hn.
  simpl in Hin, Hn. rewrite (Hag s' Hin Hn).
  rewrite IH; [reflexivity | exact Hr | intros p Hp; apply Hreq; right; exact Hp].
Qed.

Lemma xcallee_env_agree : forall g Phi th sm D d PhiD,
  wf_gsig Sg ->
  wf_tele Sg empty_ctx D PhiD ->
  sat Sg Phi th D ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  smap_ok Phi th sm D ->
  ctx_grounded g Phi ->
  xdenv_agree g (cA Phi) d ->
  exists d', xcompose d sm = Some d'
    /\ ctx_grounded (callee_grounding g Phi th (cA PhiD)) PhiD
    /\ xdenv_strong (callee_grounding g Phi th (cA PhiD)) (cA PhiD) d'.
Proof.
  intros g Phi th sm D d PhiD WF Wt Hsat Hth Hsm Hcg Hd.
  destruct (agree_restrict Hd) as [dA [HdA Hag]].
  rewrite <- (xcompose_agree_on Hsm Hag).
  exact (xcallee_env_strong WF Wt Hsat Hth Hsm Hcg HdA).
Qed.

Section XWorldR.
Hypothesis WF : wf_gsig Sg.

Lemma xdenv_agree_push : forall g A d i en,
    xdenv_agree g A d ->
    xdentry_agree g i en ->
    incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) (tyreqs A) ->
    xdenv_agree g (i :: A) ((i, en) :: d).
Proof.
  intros g A d i en Hd Hen Hi.
  destruct (agree_restrict Hd) as [dA [HdA Hag]].
  eapply agree_ext; [exact (strong_agree (xdenv_strong_push i en HdA Hen Hi)) |].
  apply agree_on_cons. exact Hag.
Qed.

Lemma world_bindty : forall Phi t tau' d g,
    t <> this_sym ->
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    ctx_grounded g Phi ->
    xdenv_agree g (cA Phi) d ->
    ctx_grounded ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (bind_ty t tau' Phi)
    /\ xdenv_agree ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
                   (cA (bind_ty t tau' Phi)) d
    /\ (forall u, In u (tyreqs (cA Phi)) ->
          lookup_subst u ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
          = lookup_subst u g).
Proof.
  intros Phi t tau' d g Hneq Hfresh Htau' Hg Hd.
  destruct (agree_restrict Hd) as [dA [HdA Hag]].
  destruct (world_bindty_s WF Hneq Hfresh Htau' Hg HdA) as [Hg2 [Hd2 Hl]].
  split; [exact Hg2 |]. split; [| exact Hl].
  eapply agree_ext; [exact (strong_agree Hd2) |].
  intros j [E | Hj] Ht; [subst j; discriminate Ht | exact (Hag j Hj Ht)].
Qed.

Lemma world_bindval : forall Phi v Dv tauv w1 d g,
    g_val Sg v = Some (Dv, tauv) ->
    sat0 Sg Phi Dv ->
    xvt w1 (app_subst g tauv) ->
    ctx_grounded g Phi ->
    xdenv_agree g (cA Phi) d ->
    ctx_grounded g (add_item (IVal v) Phi)
    /\ xdenv_agree g (cA (add_item (IVal v) Phi)) ((IVal v, XDVal w1) :: d).
Proof.
  intros Phi v Dv tauv w1 d g Hgv Hsat S1 Hg Hd.
  destruct (agree_restrict Hd) as [dA [HdA Hag]].
  destruct (world_bindval_s v Hgv Hsat S1 Hg HdA) as [Hg' Hd'].
  split; [exact Hg' |].
  eapply agree_ext; [exact (strong_agree Hd') |]. apply agree_on_cons. exact Hag.
Qed.

Lemma world_bindfn : forall Phi f Df Sf f' D' S' body sm d g,
    g_fn Sg f = Some (Df, Sf, None) ->
    g_fn Sg f' = Some (D', S', None) -> xp_fn P f' = Some body ->
    sat0 Sg Phi Df ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    sig_eq (cS Phi) S' Sf ->
    ctx_grounded g Phi ->
    xdenv_agree g (cA Phi) d ->
    exists dc, xcompose d sm = Some dc
    /\ ctx_grounded g (add_item (IFn f) Phi)
    /\ xdenv_agree g (cA (add_item (IFn f) Phi)) ((IFn f, XDClo f' dc) :: d).
Proof.
  intros Phi f Df Sf f' D' S' body sm d g Hf Hf' Hb Hsf Hs' Hsm Hsig Hg Hd.
  destruct (agree_restrict Hd) as [dA [HdA Hag]].
  destruct (world_bindfn_s WF f f' Hf Hf' Hb Hsf Hs' Hsm Hsig Hg HdA) as [dc [Hc [Hg' Hd']]].
  exists dc. split; [rewrite (xcompose_agree_on Hsm Hag) in Hc; exact Hc |].
  split; [exact Hg' |].
  eapply agree_ext; [exact (strong_agree Hd') |]. apply agree_on_cons. exact Hag.
Qed.

Lemma world_bindmth : forall Phi tauk m Dm Sf th f' D' S' body sm tau0' rest d g,
    g_mth Sg m = Some (Dm, Sf) ->
    wf_ty Sg Phi tauk ->
    msat th Dm Sf ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    g_fn Sg f' = Some (D', S', None) -> xp_fn P f' = Some body ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    fs_params S' = tau0' :: rest ->
    ty_eq (cS Phi) tau0' tauk ->
    map (app_subst (cS Phi)) rest
      = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params Sf) ->
    app_subst (cS Phi) (fs_ret S')
      = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret Sf) ->
    ctx_grounded g Phi ->
    xdenv_agree g (cA Phi) d ->
    exists dc, xcompose d sm = Some dc
    /\ ctx_grounded g (add_item (IMth tauk m th) Phi)
    /\ xdenv_agree g (cA (add_item (IMth tauk m th) Phi))
                  ((IMth tauk m th, XDClo f' dc) :: d).
Proof.
  intros Phi tauk m Dm Sf th f' D' S' body sm tau0' rest d g
         Hm Hwk Hsat Hthwf Hf' Hb Hs' Hsm Hpar0 Hteq Hprest Hpret Hg Hd.
  destruct (agree_restrict Hd) as [dA [HdA Hag]].
  destruct (world_bindmth_s WF m f' Hm Hwk Hsat Hthwf Hf' Hb Hs' Hsm Hpar0 Hteq Hprest Hpret Hg HdA)
    as [dc [Hc [Hg' Hd']]].
  exists dc. split; [rewrite (xcompose_agree_on Hsm Hag) in Hc; exact Hc |].
  split; [exact Hg' |].
  eapply agree_ext; [exact (strong_agree Hd') |]. apply agree_on_cons. exact Hag.
Qed.

End XWorldR.

End XEnv.

(* ===================== Growing the store typing =========================== *)

Lemma xvt_mono : forall Sg Rp H H', sext H H' ->
  forall w t, xvt Sg Rp H w t -> xvt Sg Rp H' w t.
Proof.
  intros Sg Rp H H' Hext. fix IH 3. intros w t D.
  destruct D as [ | | | | | | | fts fs Hr1 Hr0 Hk HF | | ].
  - constructor.
  - eapply XvTag; eauto.
  - eapply XvInj; eauto.
  - apply XvI32; assumption.
  - apply XvI64; assumption.
  - apply XvF32; assumption.
  - apply XvF64; assumption.
  - eapply XvRec; [eassumption | eassumption | eassumption |].
    clear Hk Hr0 Hr1. revert fts fs HF. fix go 3. intros l1 l2 F.
    destruct F as [|x y l1' l2' Hxy F']; constructor.
    + apply IH. exact Hxy.
    + apply go. exact F'.
  - eapply XvRef; [eassumption | apply Hext; eassumption].
  - eapply XvArr; [eassumption | apply Hext; eassumption].
Qed.

Lemma xdentry_agree_mono : forall Sg Rp P Fb H H', sext H H' ->
  forall en g i, xdentry_agree Sg Rp P Fb H g i en -> xdentry_agree Sg Rp P Fb H' g i en.
Proof.
  intros Sg Rp P Fb H H' Hext. fix IH 1.
  assert (Hall : forall g dd,
    (fix all (dd : xdenv) : Prop :=
       match dd with [] => True | p :: r => xdentry_agree Sg Rp P Fb H g (fst p) (snd p) /\ all r end) dd ->
    (fix all (dd : xdenv) : Prop :=
       match dd with [] => True | p :: r => xdentry_agree Sg Rp P Fb H' g (fst p) (snd p) /\ all r end) dd).
  { intros g. fix go 1. intros [|[i en] r] A; [exact I |].
    destruct A as [A1 A2]. split; [apply IH; exact A1 | apply go; exact A2]. }
  intros en g i A. destruct en as [w|f' d'|h sms dc|p|hs]; destruct i; simpl in A |- *; try exact A.
  - destruct (g_val Sg v) as [[? ?]|]; [exact (@xvt_mono Sg Rp H H' Hext _ _ A) | exact A].
  - destruct (g_fn Sg f) as [[[? ?] [?|]]|]; try exact A.
    destruct (g_fn Sg f') as [[[? ?] [?|]]|]; try exact A.
    destruct (xp_fn P f'); try exact A.
    destruct A as [PhiD' [g' [A1 [A2 [A3 [A4 [A5 [A6 [A7 [A8 A9]]]]]]]]]].
    exists PhiD', g'. repeat (split; [assumption |]). apply Hall. exact A9.
  - destruct (g_mth Sg m) as [[? ?]|]; try exact A.
    destruct (g_fn Sg f') as [[[? ?] [?|]]|]; try exact A.
    destruct (xp_fn P f'); try exact A.
    destruct A as [PhiD' [g' [A1 [A2 [A3 [A4 [A5 [A6 [A7 [A8 A9]]]]]]]]]].
    exists PhiD', g'. repeat (split; [assumption |]). apply Hall. exact A9.
  - destruct (g_fun Sg u) as [[[? ?] [?|]]|]; try exact A.
    destruct (Fb h) as [[[? ?] ?]|]; try exact A.
    destruct (xp_fun P h) as [[? ?]|]; try exact A.
    destruct A as [E1 [Hch [PhiC [g' [A1 [A2 [A3 [A4 [A5 [A6 [A7 [A8 A9]]]]]]]]]]]].
    split; [exact E1 |]. split; [exact Hch |]. exists PhiC, g'.
    repeat (split; [assumption |]). apply Hall. exact A9.
Qed.

Lemma xdenv_agree_mono : forall Sg Rp P Fb H H' g A d, sext H H' ->
  xdenv_agree Sg Rp P Fb H g A d -> xdenv_agree Sg Rp P Fb H' g A d.
Proof.
  intros Sg Rp P Fb H H' g A d Hext [H1 [H2 H3]]. split; [exact H1 | split; [| exact H3]].
  intros i en Hi Ht Hl. exact (@xdentry_agree_mono Sg Rp P Fb H H' Hext _ _ _ (H2 i en Hi Ht Hl)).
Qed.

Lemma xdenv_strong_mono : forall Sg Rp P Fb H H' g A d, sext H H' ->
  xdenv_strong Sg Rp P Fb H g A d -> xdenv_strong Sg Rp P Fb H' g A d.
Proof.
  intros Sg Rp P Fb H H' g A d Hext [H1 [H2 [H3 H4]]].
  split; [exact H1 | split; [| split; assumption]].
  eapply Forall_impl; [| exact H2]. intros [i en] Hp. exact (@xdentry_agree_mono Sg Rp P Fb H H' Hext _ _ _ Hp).
Qed.

Lemma venv_ok_mono : forall Sg Rp H H' g A G ge, sext H H' ->
  venv_ok Sg Rp H g A G ge -> venv_ok Sg Rp H' g A G ge.
Proof.
  intros Sg Rp H H' g A G ge Hext [H1 H2]. split; [| exact H2]. clear H2.
  induction H1 as [|p q G ge [Hk Hw] Hr IH]; [constructor |].
  constructor; [| exact IH].
  split; [exact Hk | exact (@xvt_mono Sg Rp H H' Hext _ _ Hw)].
Qed.

Lemma sext_refl : forall H, sext H H.
Proof. intros H kk c E. exact E. Qed.

Lemma sext_trans : forall H1 H2 H3, sext H1 H2 -> sext H2 H3 -> sext H1 H3.
Proof. intros H1 H2 H3 A B kk c E. exact (B _ _ (A _ _ E)). Qed.
