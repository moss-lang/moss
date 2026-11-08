(* ------------------------------------------------------------------------ *)
(* XSafety.v — type safety for MossX (Track B).                              *)
(*                                                                           *)
(* A program well-formed for XTyping.v's judgment (`xwf`) never gets stuck: *)
(* for every fuel, a typed expression run by `xinterp` in an environment    *)
(* agreeing with its context never produces XErr; a value it produces has   *)
(* its type, a `return` the enclosing fn's result type, and a `break` only  *)
(* happens inside a loop.  Traps, exits and running out of fuel are         *)
(* allowed (XUns only from an unmodelled primitive).  The primitive       *)
(* semantics is any one satisfying Part IV's laws (`prim_law`, `prim_mlaw`, *)
(* `prim_ulaw`, section hypotheses) and the GC heap's (XHeap.HEAP_LAWS, a  *)
(* functor parameter); MossX's concrete one does.  Value typing is read at *)
(* a store typing that grows as the run allocates (Kripke-style: `xsafe`). *)
(*                                                                           *)
(* The proof is CoreMoss.v's semantic, step-indexed one (§11b), extended:  *)
(* induction on fuel, and within it on the typing derivation only through  *)
(* the rules that keep the term (subsumption, the implicit type bind).      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Import ExecutionModel CoreMoss XTyping XEnv XHeap HTrie XCastTyping XRuntimeEnv XRuntimeCall PayloadInst PayloadInstCore XProvision XProvisionBind ReifyEntries ReifyContracts ReifyMaps ReifyUninhabited.
Import ListNotations.

Set Implicit Arguments.

(* MossX's interpreter over a primitive semantics, as a module type, so that
   the development applies to a given instance (MossX.XS, the oracle's)
   rather than to a fresh copy of XSem. *)
Module Type XSEM (Prims : PRIM_SEMANTICS).
  Include XSem Prims.
End XSEM.

Module XSafety (Import Prims : PRIM_SEMANTICS) (X : XSEM Prims) (HL : HEAP_LAWS Prims).
Import X.

Section Safety.

Variable Sg : gsig.
Variable Rp : nsym -> option rep.
Variable P : xprog.
Variable Fb : usym -> option (tele * tele * subst).

Hypothesis prim_law : forall P p ps r ws st,
  prim_sig p = Some (ps, r) -> Forall2 (xvty P) ws ps ->
  match fst (run_prim P p ws st) with
  | XOk w => xvty P w r
  | XTrap _ | XExit _ => True
  | _ => False
  end.

Hypothesis prim_mlaw : forall P p ps r ws st,
  prim_msig p = Some (ps, r) -> Forall2 (xvty P) ws ps ->
  match fst (run_prim P p ws st) with
  | XOk w => xvty P w r
  | XTrap _ | XExit _ => True
  | _ => False
  end.

(* an unmodelled primitive stops the program *)
Hypothesis prim_ulaw : forall P c ws st,
  fst (run_prim P (PUnsupported c) ws st) = XUns c.

Hypothesis WFX : xwf Sg Rp P Fb.

Local Notation pty_ok := (pty_ok Sg Rp P).
Local Notation xhas := (xhas Sg Rp P Fb).

Lemma WF : wf_gsig Sg.
Proof. exact (xwf_decls WFX). Qed.

Lemma xcast_ordinary : forall d n th w, Rp n = None ->
 xcast P d n th w = XOk (XVTag n w).
Proof.
 intros d n th w Hr; unfold xcast.
 destruct (lookup_cast n (xp_cast P)) as [spec|] eqn:E; [|reflexivity].
 pose proof (@xwf_cast Sg Rp P Fb WFX n spec E) as Hrep.
 rewrite Hr in Hrep; discriminate.
Qed.

(* ===================== 1. At a fixed store typing ======================== *)

(* Everything up to the cases reads values at one store typing Hs. *)
Section Fixed.
Variable Hst : sty.
Local Notation xvt := (XTyping.xvt Sg Rp P Hst).
Local Notation xdentry_agree := (XEnv.xdentry_agree Sg Rp P Fb Hst).
Local Notation xdenv_agree := (XEnv.xdenv_agree Sg Rp P Fb Hst).
Local Notation xdenv_strong := (XEnv.xdenv_strong Sg Rp P Fb Hst).
Local Notation xdenv_values := (XEnv.xdenv_values Sg Rp P Fb Hst).
Local Notation venv_ok := (XEnv.venv_ok Sg Rp P Hst).

(* What a run may end in, read at the context it started in. *)
Definition xsafe0 (g : subst) (Phi : ctx) (G : tenv) (R : option ty) (L : bool)
    (tau : ty) (r : xres) : Prop :=
  match xr_out r with
  | XOk w => xvt w (app_subst g (app_subst (cS Phi) tau)) /\ venv_ok g (cA Phi) G (xr_g r)
  | XRet w => exists rt, R = Some rt /\ xvt w (app_subst g rt)
  | XBrk => L = true /\ venv_ok g (cA Phi) G (xr_g r)
  | XErr _ => False
  | _ => True
  end.

(* The same for a run that did not produce a value (a list's, a body's). *)
Definition xsafe0_ctl (g : subst) (Phi : ctx) (G : tenv) (R : option ty) (L : bool)
    (r : xres) : Prop :=
  match xr_out r with
  | XOk _ => False
  | XRet w => exists rt, R = Some rt /\ xvt w (app_subst g rt)
  | XBrk => L = true /\ venv_ok g (cA Phi) G (xr_g r)
  | XErr _ => False
  | _ => True
  end.

Lemma xsafe0_ctl_of : forall g Phi G R L tau r,
  xsafe0 g Phi G R L tau r -> (forall w, xr_out r <> XOk w) -> xsafe0_ctl g Phi G R L r.
Proof.
  intros g Phi G R L tau [o ge st] H Hn. unfold xsafe0, xsafe0_ctl in *. simpl in *.
  destruct o; auto. exact (Hn x eq_refl).
Qed.

Lemma xsafe0_of_ctl : forall g Phi G R L tau r,
  xsafe0_ctl g Phi G R L r -> xsafe0 g Phi G R L tau r.
Proof.
  intros g Phi G R L tau [o ge st] H. unfold xsafe0, xsafe0_ctl in *. simpl in *.
  destruct o; auto. contradiction.
Qed.

(* A result that is not a value passes through any context-preserving
   construct unchanged. *)
Lemma xsafe0_pass : forall g Phi G R L tau tau' r,
  xsafe0 g Phi G R L tau r -> (forall w, xr_out r <> XOk w) -> xsafe0 g Phi G R L tau' r.
Proof.
  intros. apply xsafe0_of_ctl. eapply xsafe0_ctl_of; eauto.
Qed.

(* ===================== 2. Values and variables =========================== *)

Lemma xvt_subty : forall Phi g tau' tau w,
  subty Phi tau' tau ->
  xvt w (app_subst g (app_subst (cS Phi) tau')) ->
  xvt w (app_subst g (app_subst (cS Phi) tau)).
Proof.
  intros Phi g tau' tau w Hsub Hv.
  destruct Hsub as [t1 t2 Heq | n th ms tauw Hin Heq].
  - unfold ty_eq in Heq. rewrite <- Heq. exact Hv.
  - unfold ty_eq in Heq.
    rewrite app_subst_union, app_subst_union.
    apply XvInj with (tau' := app_subst g (app_subst (cS Phi) tauw)).
    + rewrite <- Heq. exact Hv.
    + apply in_map. apply in_map. exact Hin.
Qed.

Lemma venv_lookup : forall g A G ge x tau,
  venv_ok g A G ge -> lookup_tenv x G = Some tau ->
  exists w, lookup_xvenv x ge = Some w /\ xvt w (app_subst g tau).
Proof.
  intros g A G ge x tau [H _]. revert ge H.
  induction G as [|[y a] G IH]; intros ge H Hx; simpl in Hx; [discriminate|].
  inversion H as [|p q G' ge' [Hk Hw] Hr]; subst. destruct q as [y' w]. simpl in *. subst y'.
  simpl. destruct (Nat.eqb x y).
  - injection Hx as <-. eauto.
  - apply (IH ge' Hr Hx).
Qed.

Lemma venv_assign : forall g A G ge x tau w,
  venv_ok g A G ge -> lookup_tenv x G = Some tau -> xvt w (app_subst g tau) ->
  exists ge', assign_xvenv x w ge = Some ge' /\ venv_ok g A G ge'.
Proof.
  intros g A G ge x tau w [H Hreg] Hx Hw. revert ge H.
  induction G as [|[y a] G IH]; intros ge H; simpl in Hx; [discriminate|].
  inversion H as [|p q G' ge' [Hk Hq] Hr]; subst. destruct q as [y' w']. simpl in *. subst y'.
  inversion Hreg as [|? ? Hreg1 Hreg2]; subst.
  simpl. destruct (Nat.eqb x y) eqn:E.
  - injection Hx as <-. eexists. split; [reflexivity|].
    split; [constructor; [split; [reflexivity | exact Hw] | exact Hr] | exact Hreg].
  - destruct (IH Hreg2 Hx ge' Hr) as [ge2 [Ha [Hok _]]].
    rewrite Ha. eexists. split; [reflexivity|].
    split; [constructor; [split; [reflexivity | exact Hq] | exact Hok] | exact Hreg].
Qed.

Lemma venv_cons : forall g A G ge x tau w,
  venv_ok g A G ge -> xvt w (app_subst g tau) -> incl (ty_frees tau) (tyreqs A) ->
  venv_ok g A ((x, tau) :: G) ((x, w) :: ge).
Proof.
  intros g A G ge x tau w [H1 H2] Hw Hi. split; constructor; auto.
Qed.

Lemma venv_tail : forall g A G ge p,
  venv_ok g A (p :: G) ge -> venv_ok g A G (tl ge).
Proof.
  intros g A G ge p [H1 H2]. inversion H1; subst. inversion H2; subst.
  split; assumption.
Qed.

Lemma venv_app : forall g A Gb G bs ge,
  Forall2 (fun p q => fst p = fst q /\ xvt (snd q) (app_subst g (snd p))) Gb bs ->
  Forall (fun p => incl (ty_frees (snd p)) (tyreqs A)) Gb ->
  venv_ok g A G ge -> venv_ok g A (Gb ++ G) (bs ++ ge).
Proof.
  intros g A Gb G bs ge H Hr [H1 H2]. split.
  - apply Forall2_app; assumption.
  - apply Forall_app; split; assumption.
Qed.

Lemma venv_skip : forall g A Gb G ge,
  venv_ok g A (Gb ++ G) ge -> venv_ok g A G (skipn (List.length Gb) ge).
Proof.
  intros g A Gb. induction Gb as [|p Gb IH]; intros G ge H; [exact H|].
  simpl. destruct ge as [|q ge].
  - destruct H as [H _]. inversion H.
  - apply IH. exact (venv_tail H).
Qed.

(* The body of a call: parameters 0, 1, ... *)
Lemma venv_body : forall g A ws taus,
  Forall2 (fun w tau => xvt w (app_subst g tau)) ws taus ->
  Forall (fun tau => incl (ty_frees tau) (tyreqs A)) taus ->
  venv_ok g A (body_env (mkSig taus TUnit)) (mk_xvenv ws).
Proof.
  intros g A ws taus H Hreg. unfold body_env, mk_xvenv. simpl.
  rewrite <- (Forall2_length H).
  generalize 0. induction H as [|w tau ws taus Hh Ht IH]; intros s.
  - split; constructor.
  - inversion Hreg; subst. simpl.
    destruct (IH H2 (S s)) as [A1 A2].
    split; constructor; auto.
Qed.

Lemma venv_body' : forall g A ws S,
  Forall2 (fun w tau => xvt w (app_subst g tau)) ws (fs_params S) ->
  Forall (fun tau => incl (ty_frees tau) (tyreqs A)) (fs_params S) ->
  venv_ok g A (body_env S) (mk_xvenv ws).
Proof.
  intros g A ws [ps r] H Hreg. exact (@venv_body g A ws ps H Hreg).
Qed.

(* A context's type symbols only grow: the stored types still read the same. *)
Lemma venv_reground : forall g g' A A' G ge,
  venv_ok g A G ge ->
  incl (tyreqs A) (tyreqs A') ->
  (forall x, In x (tyreqs A) -> lookup_subst x g' = lookup_subst x g) ->
  venv_ok g' A' G ge.
Proof.
  intros g g' A A' G ge [H1 H2] Hincl Hlook. split.
  - clear Hincl. induction H1 as [|p q G ge [Hk Hw] Hr IH]; constructor;
      inversion H2 as [|? ? Hp Hps]; subst.
    + split; [exact Hk|].
      rewrite (app_subst_frees_agree g' g (snd p)); [exact Hw|].
      intros u Hu. apply Hlook. apply Hp. exact Hu.
    + apply IH. exact Hps.
  - eapply Forall_impl; [| exact H2]. intros p Hp u Hu. apply Hincl. exact (Hp u Hu).
Qed.

Lemma venv_weaken : forall g A A' G ge,
  venv_ok g A G ge -> incl (tyreqs A) (tyreqs A') -> venv_ok g A' G ge.
Proof.
  intros. eapply venv_reground; eauto.
Qed.

(* ===================== 3. Primitives ===================================== *)

Lemma pty_ok_subst : forall s tau t, pty_ok tau t -> pty_ok (app_subst s tau) t.
Proof.
  intros s tau t H. destruct t; simpl in *;
    try (destruct H as [n [th [-> Hn]]]; exists n; eexists; split; [reflexivity | exact Hn]).
  - subst. reflexivity.
  - destruct H as [Hnz ->]; split; [exact Hnz|].
    unfold bool_ty; destruct (Nat.eqb (xp_bool P) 0); reflexivity.
  - destruct H as [n [th [D [m [thm [-> [Hn [Hg Hm]]]]]]]].
    exists n, (map_snd (app_subst s) th), D, m, thm. auto.
Qed.

Lemma xvt_unit_inv : forall w, xvt w TUnit -> w = XVUnit.
Proof. intros w H. inversion H; reflexivity. Qed.

Lemma xvt_union_inv : forall w ms, xvt w (TUnion ms) -> exists t, In t ms /\ xvt w t.
Proof. intros w ms H. inversion H; subst. eauto. Qed.

Lemma xvt_tag_inv : forall w n th, xvt w (TNom n th) -> Rp n = None -> g_tag Sg n <> None ->
  exists D taup w', w = XVTag n w' /\ g_tag Sg n = Some (D, taup) /\ xvt w' (ordinary_payload P th taup).
Proof. intros w n th H Hn Hg. inversion H; subst; try congruence. eauto 6. Qed.

Lemma xvt_tagged_shape : forall w n th,
 xvt w (TNom n th) ->
 (Rp n = None \/ exists (k : bool) x, Rp n = Some (if k then RArr x else RRef x)) ->
 g_tag Sg n <> None -> exists pay, w = XVTag n pay.
Proof.
 intros w n th Hw [Hr|[k [x Hr]]] Hg; inversion Hw; subst; try congruence;
 try (destruct k; congruence); eauto.
Qed.

Lemma xvt_rec_inv : forall w fts, xvt w (TNom rec_nom fts) ->
  Rp rec_nom = None -> g_tag Sg rec_nom = None ->
  exists fs, w = XVRec fs /\ map fst fs = map fst fts
  /\ Forall2 (fun ft fw => xvt (snd fw) (snd ft)) fts fs.
Proof. intros w fts H Hn Hg. inversion H; subst; try congruence; eauto. Qed.

Lemma bool_value : forall w,
  bool_ok Sg Rp P -> xvt w (bool_ty P) ->
  w = XVTag (xp_bool P) (XVTag (xp_true P) XVUnit)
  \/ w = XVTag (xp_bool P) (XVTag (xp_false P) XVUnit).
Proof.
  intros w Bok H. unfold bool_ty in H.
  destruct (Nat.eqb (xp_bool P) 0) eqn:Ezero.
  { destruct (xvt_union_inv H) as [t [Hin _]]; contradiction. }
  destruct Bok as [[Ez _]|[Hb [Ht [Hf [[Db [ms [Hgb [Hmembers [Htrue Hfalse]]]]] [[Dt Hgt] [Df Hgf]]]]]]].
  { rewrite Ez, Nat.eqb_refl in Ezero; discriminate. }
  destruct (xvt_tag_inv H Hb ltac:(rewrite Hgb; discriminate)) as [D [taup [w' [-> [Hg Hw']]]]].
  rewrite Hgb in Hg. injection Hg as <- <-.
  assert (Epayload : ordinary_payload P [] (TUnion ms) = TUnion ms).
  { unfold ordinary_payload. destruct (xp_immutable P); [|apply app_subst_nil].
    cbn [payload_inst]. unfold payload_join.
    assert (Emap : map (payload_inst []) ms = ms).
    { rewrite <- (map_id ms) at 2. apply map_ext_in. intros t Hin.
      apply Hmembers in Hin. destruct Hin as [<-|[<-|[]]]; reflexivity. }
    rewrite Emap. f_equal. apply payload_flat_atoms_identity.
    rewrite Forall_forall. intros t Hin inner Eq. apply Hmembers in Hin.
    destruct Hin as [<-|[<-|[]]]; discriminate. }
  rewrite Epayload in Hw'.
  destruct (xvt_union_inv Hw') as [t [Hin Ht']].
  apply Hmembers in Hin. destruct Hin as [<- | [<- | []]].
  - destruct (xvt_tag_inv Ht' Ht ltac:(rewrite Hgt; discriminate)) as [D' [tp [w'' [-> [Hg' Hw'']]]]].
    rewrite Hgt in Hg'. injection Hg' as <- <-.
    assert (Eunit : ordinary_payload P [] TUnit = TUnit) by
      (unfold ordinary_payload; destruct (xp_immutable P); reflexivity).
    rewrite Eunit in Hw''.
    rewrite (xvt_unit_inv Hw''). left. reflexivity.
  - destruct (xvt_tag_inv Ht' Hf ltac:(rewrite Hgf; discriminate)) as [D' [tp [w'' [-> [Hg' Hw'']]]]].
    rewrite Hgf in Hg'. injection Hg' as <- <-.
    assert (Eunit : ordinary_payload P [] TUnit = TUnit) by
      (unfold ordinary_payload; destruct (xp_immutable P); reflexivity).
    rewrite Eunit in Hw''.
    rewrite (xvt_unit_inv Hw''). right. reflexivity.
Qed.

Lemma pty_xvty : forall tau t w,
  bool_ok Sg Rp P -> pty_ok tau t -> xvt w tau -> xvty P w t.
Proof.
  intros tau t w Hb Hp Hw. destruct t; simpl in Hp.
  - destruct Hp as [n [th [-> Hn]]]. inversion Hw; subst; simpl; try exact I; try congruence;
      match goal with Hgc : _ = Some (if ?k then RArr _ else RRef _) |- _ => destruct k; simpl in *; congruence end.
  - destruct Hp as [n [th [-> Hn]]]. inversion Hw; subst; simpl; try exact I; try congruence;
      match goal with Hgc : _ = Some (if ?k then RArr _ else RRef _) |- _ => destruct k; simpl in *; congruence end.
  - destruct Hp as [n [th [-> Hn]]]. inversion Hw; subst; simpl; try exact I; try congruence;
      match goal with Hgc : _ = Some (if ?k then RArr _ else RRef _) |- _ => destruct k; simpl in *; congruence end.
  - destruct Hp as [n [th [-> Hn]]]. inversion Hw; subst; simpl; try exact I; try congruence;
      match goal with Hgc : _ = Some (if ?k then RArr _ else RRef _) |- _ => destruct k; simpl in *; congruence end.
  - subst. rewrite (xvt_unit_inv Hw). exact I.
  - destruct Hp as [Hnz ->]. destruct (bool_value Hb Hw) as [-> | ->]; simpl; auto.
  - destruct Hp as [n [th [D [m [thm [-> [Hn [Hg Hm]]]]]]]].
    destruct (xvt_tag_inv Hw Hn ltac:(rewrite Hg; discriminate)) as [D' [taup [w' [-> [Hg' Hw']]]]].
    rewrite Hg in Hg'. injection Hg' as <- <-.
    unfold ordinary_payload in Hw'; destruct (xp_immutable P); cbn [payload_inst] in Hw';
    inversion Hw'; subst; simpl; try exact I; try congruence;
      match goal with Hgc : _ = Some (if ?k then RArr _ else RRef _) |- _ => destruct k; simpl in *; congruence end.
  - destruct Hp as [n [th [-> [Hn Hg]]]].
    destruct (xvt_tag_inv Hw Hn Hg) as [D' [taup [w' [-> _]]]]. exact I.
Qed.

Lemma xvty_pty : forall tau t w,
  bool_ok Sg Rp P -> t <> XTAddr -> t <> XTTag -> pty_ok tau t -> xvty P w t -> xvt w tau.
Proof.
  intros tau t w Hb Hna Hnt Hp Hw.
  destruct t; simpl in Hp.
  - destruct Hp as [n [th [-> Hn]]]. destruct w; simpl in Hw; try contradiction. constructor; exact Hn.
  - destruct Hp as [n [th [-> Hn]]]. destruct w; simpl in Hw; try contradiction. constructor; exact Hn.
  - destruct Hp as [n [th [-> Hn]]]. destruct w; simpl in Hw; try contradiction. constructor; exact Hn.
  - destruct Hp as [n [th [-> Hn]]]. destruct w; simpl in Hw; try contradiction. constructor; exact Hn.
  - subst. destruct w; simpl in Hw; try contradiction. constructor.
  - destruct Hp as [Hnz ->].
    destruct Hb as [[Ez _]|[Hb [Ht [Hf [[Db [ms [Hgb [Hmembers [Htrue Hfalse]]]]] [[Dt Hgt] [Df Hgf]]]]]]].
    { contradiction. }
    destruct w as [| b w | | | | | | |]; simpl in Hw; try contradiction.
    destruct w as [| t' w | | | | | | |]; try contradiction.
    destruct w; try contradiction. destruct Hw as [-> Ht'].
    unfold bool_ty. assert (Ez : Nat.eqb (xp_bool P) 0 = false) by (apply Nat.eqb_neq; exact Hnz).
    rewrite Ez. eapply XvTag; [exact Hb | exact Hgb |].
    rewrite ordinary_payload_nil_shape_identity.
    2: { constructor; rewrite Forall_forall; intros t Hin; apply Hmembers in Hin;
      destruct Hin as [<-|[<-|[]]]; try (constructor; constructor); do 2 eexists; reflexivity. }
    destruct Ht' as [-> | ->].
    + eapply XvInj; [|exact Htrue]. eapply XvTag; [exact Ht | exact Hgt |].
      rewrite ordinary_payload_nil_shape_identity; constructor.
    + eapply XvInj; [|exact Hfalse]. eapply XvTag; [exact Hf | exact Hgf |].
      rewrite ordinary_payload_nil_shape_identity; constructor.
  - contradiction.
  - contradiction.
Qed.

Lemma pty_args : forall ws taus ps,
  bool_ok Sg Rp P ->
  Forall2 xvt ws taus -> Forall2 pty_ok taus ps -> Forall2 (xvty P) ws ps.
Proof.
  intros ws taus ps Hb H1. revert ps.
  induction H1 as [|w tau ws taus Hw Hr IH]; intros ps H2; inversion H2; subst; constructor.
  - eapply pty_xvty; eauto.
  - apply IH. assumption.
Qed.

(* Running a typed primitive: a value of its result type, a trap or an exit. *)
Lemma prim_sigs_res : forall p ps r, prim_sigs p ps r -> r <> XTAddr /\ r <> XTTag.
Proof.
  intros p ps r [H | H]; destruct p; simpl in H;
    repeat match type of H with context [match ?o with _ => _ end] => destruct o end;
    try discriminate; injection H as _ <-; split; discriminate.
Qed.

Lemma prim_run : forall p ps r ws taus rty st,
  prim_sigs p ps r ->
  Forall2 xvt ws taus -> Forall2 pty_ok taus ps -> pty_ok rty r ->
  match fst (run_prim P p ws st) with
  | XOk w => xvt w rty
  | XTrap _ | XExit _ => True
  | _ => False
  end.
Proof.
  intros p ps r ws taus rty st Hsig Hws Hps Hr.
  pose proof (xwf_bool WFX) as Hb.
  destruct (prim_sigs_res Hsig) as [Hna Hnt].
  assert (H : match fst (run_prim P p ws st) with
              | XOk w => xvty P w r
              | XTrap _ | XExit _ => True
              | _ => False
              end).
  { destruct Hsig as [Hsig | Hsig].
    - exact (@prim_law P p ps r ws st Hsig (pty_args Hb Hws Hps)).
    - exact (@prim_mlaw P p ps r ws st Hsig (pty_args Hb Hws Hps)). }
  destruct (fst (run_prim P p ws st)); auto.
  eapply xvty_pty; eauto.
Qed.

(* ===================== 4. Bool ============================================ *)

Lemma truthy_typed : forall w,
  xvt w (bool_ty P) -> exists b, truthy P w = Some b.
Proof.
  intros w H. destruct (bool_value (xwf_bool WFX) H) as [-> | ->]; simpl; eauto.
Qed.

(* ===================== 5. Structs ======================================== *)

Lemma assoc_typed_F : forall (F : ty -> ty) fts fs f tf,
  map fst fs = map fst fts ->
  Forall2 (fun ft fw => xvt (snd fw) (F (snd ft))) fts fs ->
  assoc_ty f fts = Some tf ->
  exists v, assoc_fld f fs = Some v /\ xvt v (F tf).
Proof.
  intros F fts fs f tf Hk H. revert Hk.
  induction H as [|ft fw fts fs Hh Hr IH]; intros Hk Ha; simpl in *; [discriminate|].
  destruct ft as [f1 t1], fw as [f2 w]. simpl in *.
  injection Hk as Hf Hk. subst f2.
  destruct (Nat.eqb f f1).
  - injection Ha as <-. eauto.
  - apply IH; assumption.
Qed.

Lemma assoc_typed : forall th fts fs f tf,
  map fst fs = map fst fts ->
  Forall2 (fun ft fw => xvt (snd fw) (app_subst th (snd ft))) fts fs ->
  assoc_ty f fts = Some tf ->
  exists v, assoc_fld f fs = Some v /\ xvt v (app_subst th tf).
Proof. intros th. exact (assoc_typed_F (app_subst th)). Qed.


(* ===================== 6. Small facts about results ======================= *)

Lemma F2_map_r : forall (A B : Type) (Rl : A -> B -> Prop) (F : B -> B) xs ys,
  Forall2 Rl xs (map F ys) -> Forall2 (fun x y => Rl x (F y)) xs ys.
Proof.
  intros A B Rl F xs ys. revert xs. induction ys as [|y ys IH]; intros xs H;
    inversion H; subst; constructor; auto.
Qed.

Lemma F2_map_l : forall (A B : Type) (Rl : A -> B -> Prop) (F : B -> B) xs ys,
  Forall2 (fun x y => Rl x (F y)) xs ys -> Forall2 Rl xs (map F ys).
Proof.
  intros A B Rl F xs ys H. induction H; simpl; constructor; auto.
Qed.

Lemma F2_impl : forall (A B : Type) (R1 R2 : A -> B -> Prop) xs ys,
  (forall x y, R1 x y -> R2 x y) -> Forall2 R1 xs ys -> Forall2 R2 xs ys.
Proof.
  intros A B R1 R2 xs ys Himp H. induction H; constructor; auto.
Qed.

Lemma map_fst_combine : forall (A B : Type) (l1 : list A) (l2 : list B),
  List.length l1 = List.length l2 -> map fst (combine l1 l2) = l1.
Proof.
  intros A B l1. induction l1 as [|a l1 IH]; intros [|b l2] H; simpl in *; try discriminate;
    [reflexivity | f_equal; apply IH; congruence].
Qed.

Lemma F2_conv : forall ws taus (F1 F2 : ty -> ty),
  (forall tau, In tau taus -> F1 tau = F2 tau) ->
  Forall2 (fun w tau => xvt w (F1 tau)) ws taus ->
  Forall2 (fun w tau => xvt w (F2 tau)) ws taus.
Proof.
  intros ws taus F1 F2 Heq H. induction H as [|w tau ws taus Hh Ht IH]; constructor.
  - rewrite <- Heq by (left; reflexivity). exact Hh.
  - apply IH. intros tau' Hin. apply Heq. right. exact Hin.
Qed.

(* A fn body's outcome, seen by its caller through `ret_call`. *)
Lemma ret_call_safe : forall gb Phib Gb rt r g Phi G R L tau g1,
  xsafe0 gb Phib Gb (Some rt) false rt r ->
  absorbs gb (cS Phib) ->
  app_subst gb rt = app_subst g (app_subst (cS Phi) tau) ->
  venv_ok g (cA Phi) G g1 ->
  xsafe0 g Phi G R L tau (ret_call g1 r).
Proof.
  intros gb Phib Gb rt [o gr str] g Phi G R L tau g1 H Hab Heq Hv.
  unfold xsafe0 in *. destruct o; simpl in *; auto.
  - destruct H as [Hw _]. rewrite (absorbs_all Hab) in Hw. rewrite <- Heq. auto.
  - destruct H as [rt' [E Hw]]. injection E as <-. rewrite <- Heq. auto.
  - destruct H as [E _]. discriminate.
Qed.

(* Reading a result in a wider context (more items, more groundings) back
   in the narrower one it started in. *)
Lemma xsafe0_back : forall g2 Phi2 g Phi G R L tau r,
  xsafe0 g2 Phi2 G R L tau r ->
  app_subst g2 (app_subst (cS Phi2) tau) = app_subst g (app_subst (cS Phi) tau) ->
  (forall rt, R = Some rt -> app_subst g2 rt = app_subst g rt) ->
  (forall ge, venv_ok g2 (cA Phi2) G ge -> venv_ok g (cA Phi) G ge) ->
  xsafe0 g Phi G R L tau r.
Proof.
  intros g2 Phi2 g Phi G R L tau [o gr str] H Ht HR Hv.
  unfold xsafe0 in *. destruct o; simpl in *; auto.
  - destruct H as [Hw Hg]. rewrite <- Ht. auto.
  - destruct H as [rt [E Hw]]. exists rt. split; [exact E|]. rewrite <- (HR rt E). exact Hw.
  - destruct H as [E Hg]. auto.
Qed.


(* A type bind (written or implicit): the world it opens, and the way back. *)
Lemma bindty_world : forall R L Phi G t tau' tau d ge g,
    t <> this_sym ->
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    wf_ty Sg Phi tau ->
    ret_wf Sg Phi R ->
    ctx_grounded g Phi ->
    xdenv_agree g (cA Phi) d ->
    venv_ok g (cA Phi) G ge ->
    ctx_grounded ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (bind_ty t tau' Phi)
    /\ xdenv_agree ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (cA (bind_ty t tau' Phi)) (xbind_ty P d t tau')
    /\ venv_ok ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (cA (bind_ty t tau' Phi)) G ge
    /\ forall r, xsafe0 ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (bind_ty t tau' Phi) G R L tau r
                -> xsafe0 g Phi G R L tau r.
Proof.
  intros R L Phi G t tau' tau d ge g Hneq Hfresh Htau' Htau HR Hg Hd Hv.
  destruct (world_bindty WF Hneq Hfresh Htau' Hg Hd) as [Hg2 [Hd2 Hlook]].
  set (g2 := (t, app_subst g (app_subst (cS Phi) tau')) :: g) in *.
  assert (Hnot : ~ In t (tyreqs (cA Phi))).
  { intro H. apply Hfresh. apply safety_case_bindty_aux1. exact H. }
  assert (Hran : Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA Phi))) (cS Phi)).
  { destruct Hg as [_ [_ [_ [[_ Hran] _]]]]. exact Hran. }
  assert (Hincl : incl (tyreqs (cA Phi)) (tyreqs (cA (bind_ty t tau' Phi)))).
  { intros u Hu. simpl. right. exact Hu. }
  (* a type well-formed outside reads the same inside *)
  assert (Hsame : forall a, incl (ty_frees a) (tyreqs (cA Phi)) ->
            app_subst g2 a = app_subst g a).
  { intros a Ha. apply app_subst_frees_agree. intros u Hu. apply Hlook. exact (Ha u Hu). }
  split; [exact Hg2|]. split; [exact Hd2|]. split.
  - eapply venv_reground; [exact Hv | exact Hincl | exact Hlook].
  - intros r Hr. eapply xsafe0_back; [exact Hr | | |].
    + assert (Hfree : incl (ty_frees tau) (tyreqs (cA Phi))) by exact (wf_ty_frees Htau).
      rewrite (app_subst_frees_agree (cS (bind_ty t tau' Phi)) (cS Phi) tau).
      * apply Hsame. apply safety_case_bindty_aux3; assumption.
      * intros u Hu. simpl. destruct (Nat.eqb u t) eqn:E; [|reflexivity].
        apply Nat.eqb_eq in E. subst. exfalso. apply Hnot. exact (Hfree t Hu).
    + intros rt E. subst R. apply Hsame. exact (wf_ty_frees HR).
    + intros ge0 [H1 _]. split; [| exact (proj2 Hv)].
      pose proof (proj2 Hv) as HGreg. clear - H1 HGreg Hsame.
      induction H1 as [|p q G0 ge1 [Hk Hw] Hr0 IH]; constructor;
        inversion HGreg as [|? ? Hp Hps]; subst.
      * split; [exact Hk|]. rewrite <- (Hsame _ Hp). exact Hw.
      * apply IH. exact Hps.
Qed.


(* ===================== 6b. Members and patterns ============================ *)

Lemma map_snd_fst : forall (F : ty -> ty) (l : list (fld * ty)), map fst (map_snd F l) = map fst l.
Proof. intros F l. induction l as [|[a b] l IH]; simpl; [reflexivity | rewrite IH; reflexivity]. Qed.

Lemma Forall2_map_snd_l : forall (F : ty -> ty) (l : list (fld * ty)) (r : list (fld * xvalue)),
  Forall2 (fun ft fw => xvt (snd fw) (snd ft)) (map_snd F l) r ->
  Forall2 (fun ft fw => xvt (snd fw) (F (snd ft))) l r.
Proof.
  intros F l. induction l as [|[a b] l IH]; intros r H; simpl in H;
    inversion H; subst; constructor; [exact H2 | apply IH; exact H4].
Qed.

Lemma assoc_ty_in : forall f fts t, assoc_ty f fts = Some t -> In (f, t) fts.
Proof.
  intros f fts t. induction fts as [|[f' t'] r IH]; simpl; intros H; [discriminate|].
  destruct (Nat.eqb f f') eqn:E.
  - injection H as <-. apply Nat.eqb_eq in E. subst. left. reflexivity.
  - right. apply IH. exact H.
Qed.

Lemma map_fst_map_snd : forall (F : ty -> ty) (th : subst), map fst (map_snd F th) = map fst th.
Proof. intros F th. unfold map_snd. rewrite map_map. reflexivity. Qed.

Lemma nodup_heads_same : forall ms n a b,
  NoDup (map member_head ms) -> In (TNom n a) ms -> In (TNom n b) ms -> a = b.
Proof.
  intros ms n a b H. induction ms as [|m ms IH]; intros Ha Hb; [destruct Ha|].
  inversion H as [|? ? Hnot Hnd]; subst.
  destruct Ha as [Ea | Ha]; destruct Hb as [Eb | Hb].
  - rewrite Ea in Eb. injection Eb as ->. reflexivity.
  - exfalso. apply Hnot. subst m. simpl. apply in_map_iff. exists (TNom n b). auto.
  - exfalso. apply Hnot. subst m. simpl. apply in_map_iff. exists (TNom n a). auto.
  - apply IH; assumption.
Qed.

(* The members of a well-formed scrutinee type, and what their applications
   cover. *)
Lemma wf_union_inv : forall Phi ms,
  wf_ty Sg Phi (TUnion ms) -> Forall (wf_ty Sg Phi) ms /\ Forall is_nom ms.
Proof. intros Phi ms H. inversion H; subst. split; assumption. Qed.

Lemma wf_tag_inv : forall Phi n th,
  wf_ty Sg Phi (TNom n th) ->
  (exists D taup, g_tag Sg n = Some (D, taup) /\ sat Sg Phi th D)
  \/ g_tag Sg n = None.
Proof. intros Phi n th H;inversion H;subst;[left|right|right];eauto. Qed.

(* A payload-bearing nominal: neither a record nor an opaque type head. *)
Lemma wf_tag_inv' : forall Phi n th, g_tag Sg n <> None ->
  wf_ty Sg Phi (TNom n th) -> exists D taup, g_tag Sg n = Some (D, taup) /\ sat Sg Phi th D.
Proof.
  intros Phi n th Hn H;destruct(wf_tag_inv H)as[Known|Absent];[exact Known|congruence].
Qed.

Lemma rec_payload_scoped : forall Phi fts,
  incl (ty_frees (TNom rec_nom fts)) (tyreqs (cA Phi)) ->
  Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA Phi))) fts.
Proof.
  intros Phi fts Hscope. apply Forall_forall. intros [f tau] Hin t Ht.
  apply Hscope. cbn. apply in_flat_map. exists (f,tau). auto.
Qed.

Lemma member_wf : forall Phi ts ms mem,
  wf_ty Sg Phi ts -> members ts = Some ms -> In mem ms ->
  exists n th, mem = TNom n th /\
  ((exists D taup PhiD, g_tag Sg n = Some (D, taup)
    /\ tele_ctx D = Some PhiD /\ map fst th = tyreqs (cA PhiD))
   \/ g_tag Sg n = None).
Proof.
  intros Phi ts ms mem Hwf Hm Hin.
  assert (Hw : wf_ty Sg Phi mem /\ is_nom mem).
  { destruct ts as [| | n th | ms0]; simpl in Hm; try discriminate; injection Hm as <-.
    - destruct Hin as [<- | []]. split; [exact Hwf | exists n, th; reflexivity].
    - destruct (wf_union_inv Hwf) as [Hall Hnom]. rewrite Forall_forall in Hall, Hnom.
      split; [apply Hall | apply Hnom]; exact Hin. }
  destruct Hw as [Hw [n [th ->]]].
  exists n, th. split; [reflexivity|].
  destruct (wf_tag_inv Hw) as [[D [taup [Hg [PhiD [Htc [Hdom _]]]]]] | Hg].
  - left. exists D, taup, PhiD. auto.
  - right. exact Hg.
Qed.

Lemma member_of_value : forall Phi g ts ms w,
  wf_ty Sg Phi ts -> members ts = Some ms -> xvt w (app_subst g ts) ->
  exists n th, In (TNom n th) ms /\ xvt w (TNom n (map_snd (app_subst g) th)).
Proof.
  intros Phi g ts ms w Hwf Hm Hw.
  destruct ts as [| | n th | ms0]; simpl in Hm; try discriminate; injection Hm as <-.
  - exists n, th. split; [left; reflexivity | exact Hw].
  - rewrite app_subst_union in Hw. destruct (xvt_union_inv Hw) as [t [Hin Ht]].
    apply in_map_iff in Hin. destruct Hin as [mem [<- Hmem]].
    destruct (@member_wf _ _ _ _ Hwf eq_refl Hmem) as [n [th [-> _]]].
    exists n, th. split; [exact Hmem | exact Ht].
Qed.

(* Reading a declaration's type through a member's application, grounded. *)
Lemma read_member : forall g th t PhiD,
  map fst th = tyreqs (cA PhiD) -> incl (ty_frees t) (tyreqs (cA PhiD)) ->
  app_subst (map_snd (app_subst g) th) t = app_subst g (app_subst th t).
Proof.
  intros g th t PhiD Hdom Hfr. symmetry. apply app_subst_chain. rewrite Hdom. exact Hfr.
Qed.

Lemma bind_fields_typed : forall th' fts r fs tys (Fg : ty -> ty),
  map fst r = map fst fts ->
  Forall2 (fun ft fw => xvt (snd fw) (app_subst th' (snd ft))) fts r ->
  Forall2 (fun fx t => assoc_ty (fst fx) fts = Some t) fs tys ->
  (forall t v, In t (map snd fts) -> xvt v (app_subst th' t) -> xvt v (Fg t)) ->
  exists bs, bind_fields fs r = Some bs
  /\ Forall2 (fun p q => fst p = fst q /\ xvt (snd q) (snd p))
             (combine (map snd fs) (map Fg tys)) bs.
Proof.
  intros th' fts r fs tys Fg Hk Hr Hfs Hconv.
  induction Hfs as [|[f x] t fs tys Ha Hrest IH]; simpl.
  - exists []. split; [reflexivity | constructor].
  - simpl in Ha. destruct (@assoc_typed th' fts r f t Hk Hr Ha) as [v [Hv Hvt]].
    destruct IH as [bs [Hb Hbs]].
    rewrite Hv, Hb. eexists. split; [reflexivity|].
    constructor; [| exact Hbs]. simpl. split; [reflexivity|].
    apply (Hconv t v); [| exact Hvt]. apply in_map_iff. exists (f, t).
    split; [reflexivity | exact (assoc_ty_in _ _ Ha)].
Qed.

Lemma bind_fields_typed_F : forall (F0 : ty -> ty) fts r fs tys (Fg : ty -> ty),
  map fst r = map fst fts ->
  Forall2 (fun ft fw => xvt (snd fw) (F0 (snd ft))) fts r ->
  Forall2 (fun fx t => assoc_ty (fst fx) fts = Some t) fs tys ->
  (forall t v, In t (map snd fts) -> xvt v (F0 t) -> xvt v (Fg t)) ->
  exists bs, bind_fields fs r = Some bs
  /\ Forall2 (fun p q => fst p = fst q /\ xvt (snd q) (snd p))
             (combine (map snd fs) (map Fg tys)) bs.
Proof.
  intros F0 fts r fs tys Fg Hk Hr Hfs Hconv.
  induction Hfs as [|[f x] t fs tys Ha Hrest IH]; simpl.
  - exists []. split; [reflexivity | constructor].
  - simpl in Ha. destruct (@assoc_typed_F F0 fts r f t Hk Hr Ha) as [v [Hv Hvt]].
    destruct IH as [bs [Hb Hbs]].
    rewrite Hv, Hb. eexists. split; [reflexivity|].
    constructor; [| exact Hbs]. simpl. split; [reflexivity|].
    apply (Hconv t v); [| exact Hvt]. apply in_map_iff. exists (f, t).
    split; [reflexivity | exact (assoc_ty_in _ _ Ha)].
Qed.

(* A typed pattern never fails on a value of the scrutinee's type, and binds
   what it says. *)
Lemma pat_sound : forall Phi g tau0 p Gb w,
  ctx_grounded g Phi ->
  wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
  (forall ms, members (app_subst (cS Phi) tau0) = Some ms -> NoDup (map member_head ms)) ->
  pat_ok Sg Rp P tau0 (app_subst (cS Phi) tau0) p Gb ->
  xvt w (app_subst g (app_subst (cS Phi) tau0)) ->
  match match_pat p w with
  | PMatch bs => Forall2 (fun q b => fst q = fst b /\ xvt (snd b) (app_subst g (snd q))) Gb bs
  | PNo => True
  | PBad => False
  end.
Proof.
  intros Phi g tau0 p Gb w Hg Hwf Hnd Hp Hw.
  assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau) = app_subst g tau)
    by (destruct Hg as [_ [Hab _]]; exact (absorbs_all Hab)).
  destruct Hp as [ | x | n thn ms D taup ob Hm Hin Hrp Hg0 Hpayload
                 | n thn ms D taup ob xc k0 Hm Hin Hrp Hg0
                 | n thn ms D fts fs tys Hm Hin Hrp Hg0 Hpayload Hfs ]; simpl.
  - constructor.
  - constructor; [| constructor]. split; [reflexivity|]. simpl. rewrite <- Hcol. exact Hw.
  - destruct (@member_of_value _ _ _ _ _ Hwf Hm Hw) as [n1 [th1 [Hin1 Hw1]]].
    destruct w as [| n' pay | | | | | | |]; try exact I.
    destruct (Nat.eqb n n') eqn:E; [| exact I]. apply Nat.eqb_eq in E. subst n'.
    assert (Hn1 : n1 = n) by (inversion Hw1; reflexivity). subst n1.
    rewrite (@nodup_heads_same _ _ _ _ (Hnd ms Hm) Hin Hin1) in *.
    + destruct (xvt_tag_inv Hw1 Hrp ltac:(rewrite Hg0; discriminate)) as [D1 [taup1 [pay' [E1 [Hg1 Hpay]]]]].
      injection E1; intros; subst pay'.
      rewrite Hg0 in Hg1. injection Hg1 as <- <-.
      destruct (@member_wf _ _ _ _ Hwf Hm Hin1) as [n2 [th2 [E2 Halt]]].
      injection E2 as <- <-.
      destruct Halt as [[D2 [taup2 [PhiD [Hg2 [Htc Hdom]]]]] | Hr0];
        [| rewrite Hr0 in Hg0; discriminate].
      rewrite Hg0 in Hg2. injection Hg2 as <- <-.
      destruct (wf_tag_decl WF _ Hg0) as [PhiD' [Wt Hwt]].
      rewrite (wf_tele_ctx Wt) in Htc. injection Htc as <-.
      destruct ob as [x|]; [| constructor].
      constructor; [| constructor]. split; [reflexivity|]. simpl.
      rewrite ordinary_payload_commutes; [exact Hpay| |exact Hpayload].
      apply payload_covered_from_frees. intros t Ht.
      apply subst_chain_aux1. rewrite Hdom. exact (Hwt t Ht).
  - destruct (@member_of_value _ _ _ _ _ Hwf Hm Hw) as [n1 [th1 [Hin1 Hw1]]].
    destruct w as [| n' pay | | | | | | |]; try exact I.
    destruct (Nat.eqb n n') eqn:E; [| exact I]. apply Nat.eqb_eq in E. subst n'.
    assert (Hn1 : n1 = n) by (inversion Hw1; reflexivity). subst n1.
    rewrite (@nodup_heads_same _ _ _ _ (Hnd ms Hm) Hin Hin1) in *.
    + pose proof (@cast_tag_payload Sg Rp P Hst n (map_snd (app_subst g) th1) pay D taup k0 xc Hrp Hg0 Hw1) as Hpay.
      destruct (@member_wf _ _ _ _ Hwf Hm Hin1) as [n2 [th2 [E2 Halt]]].
      injection E2 as <- <-.
      destruct Halt as [[D2 [taup2 [PhiD [Hg2 [Htc Hdom]]]]] | Hr0];
        [| rewrite Hr0 in Hg0; discriminate].
      rewrite Hg0 in Hg2. injection Hg2 as <- <-.
      destruct (wf_tag_decl WF _ Hg0) as [PhiD' [Wt Hwt]].
      rewrite (wf_tele_ctx Wt) in Htc. injection Htc as <-.
      destruct ob as [x|]; [| constructor].
      constructor; [| constructor]. split; [reflexivity|]. simpl.
      rewrite <- (@read_member g th1 taup _ Hdom Hwt). exact Hpay.
  - destruct (@member_of_value _ _ _ _ _ Hwf Hm Hw) as [n1 [th1 [Hin1 Hw1]]].
    destruct w as [| n' pay | | | | | | |]; try exact I.
    destruct (Nat.eqb n n') eqn:E; [| exact I]. apply Nat.eqb_eq in E. subst n'.
    assert (Hn1 : n1 = n) by (inversion Hw1; reflexivity). subst n1.
    rewrite (@nodup_heads_same _ _ _ _ (Hnd ms Hm) Hin Hin1) in *.
    destruct (xvt_tag_inv Hw1 Hrp ltac:(rewrite Hg0; discriminate)) as [D1 [taup1 [pay' [E1 [Hg1 Hpay]]]]].
    injection E1; intros; subst pay'.
    rewrite Hg0 in Hg1. injection Hg1 as <- <-.
    rewrite ordinary_payload_nom in Hpay.
    destruct (@xwf_rec Sg Rp P Fb WFX) as [Hrr Hgr].
    destruct (xvt_rec_inv Hpay Hrr Hgr) as [r [-> [Hk Hr]]].
    assert (Hk' : map fst r = map fst fts) by (rewrite Hk; exact (map_snd_fst _ _)).
    clear Hk; rename Hk' into Hk. apply Forall2_map_snd_l in Hr.
    destruct (@member_wf _ _ _ _ Hwf Hm Hin1) as [n2 [th2 [E2 Halt]]].
    injection E2 as <- <-.
    destruct Halt as [[D2 [taup2 [PhiD [Hg2 [Htc Hdom]]]]] | Hr0];
      [| rewrite Hr0 in Hg0; discriminate].
    rewrite Hg0 in Hg2. injection Hg2 as <- <-.
    destruct (wf_tag_decl WF _ Hg0) as [PhiD' [Wt Hwt]].
    pose proof (rec_payload_scoped _ _ Hwt) as Hfts.
    rewrite (wf_tele_ctx Wt) in Htc. injection Htc as <-.
    destruct (@bind_fields_typed_F (ordinary_payload P (map_snd (app_subst g) th1)) fts r fs tys
                (fun t => app_subst g (ordinary_payload P th1 t)) Hk Hr Hfs) as [bs [Hb Hbs]].
    { intros t v Ht Hv. apply in_map_iff in Ht.
      destruct Ht as [[f t'] [<- Hin']].
      rewrite ordinary_payload_commutes; [exact Hv| |].
      - apply payload_covered_from_frees. intros u Hu. apply subst_chain_aux1.
        rewrite Hdom. rewrite Forall_forall in Hfts. exact (Hfts _ Hin' u Hu).
      - eapply ordinary_payload_nom_parameter_ok; eassumption. }
    destruct fs as [|fx fs'].
    + simpl. inversion Hfs; subst. constructor.
    + rewrite Hb. rewrite app_nil_r.
      replace (combine (map snd (fx :: fs')) (map (ordinary_payload P th1) tys))
        with (map (fun q => (fst q, ordinary_payload P th1 (snd q))) (combine (map snd (fx :: fs')) tys)).
      * clear -Hbs. revert Hbs. generalize (fx :: fs'). intros l.
        revert tys bs. induction l as [|a l IH]; intros tys bs H; simpl in *.
        -- inversion H; subst. constructor.
        -- destruct tys as [|t tys]; simpl in *; [inversion H; constructor|].
           inversion H as [|? ? ? ? [Hk1 Hv1] Hr1]; subst.
           constructor; [split; assumption | apply IH; exact Hr1].
      * clear. generalize (map snd (fx :: fs')). intros xs. revert tys.
        induction xs as [|x xs IH]; intros [|t tys]; simpl; try reflexivity.
        rewrite IH. reflexivity.
Qed.


Lemma catches : forall Phi g tau0 arms w,
  wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
  exhaustive Sg Rp (app_subst (cS Phi) tau0) arms ->
  xvt w (app_subst g (app_subst (cS Phi) tau0)) ->
  exists a, In a arms /\ match_pat (fst a) w <> PNo.
Proof.
  intros Phi g tau0 arms w Hwf [[ob [eb Hin]] | [ms [Hm Hall]]] Hw.
  - exists (XPAny ob, eb). split; [exact Hin|]. simpl. destruct ob; discriminate.
  - destruct (@member_of_value _ _ _ _ _ Hwf Hm Hw) as [n1 [th1 [Hin1 Hw1]]].
    rewrite Forall_forall in Hall. destruct (Hall _ Hin1) as [n [th [E [Hrp [Hgn [ob [fs [eb Ha]]]]]]]].
    injection E as <- <-.
    exists (XPHead n1 ob fs, eb). split; [exact Ha|]. simpl.
    assert (Hshape : exists pay, w = XVTag n1 pay).
    { eapply xvt_tagged_shape; eauto. }
    destruct Hshape as [pay ->]. rewrite Nat.eqb_refl.
    destruct fs; [discriminate|]. destruct pay; try discriminate.
    destruct (bind_fields _ _); discriminate.
Qed.

Lemma select_typed : forall Phi g tau0 G R L tau arms Gbs w,
  ctx_grounded g Phi ->
  wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
  (forall ms, members (app_subst (cS Phi) tau0) = Some ms -> NoDup (map member_head ms)) ->
  (exists a, In a arms /\ match_pat (fst a) w <> PNo) ->
  Forall2 (fun a Gb => pat_ok Sg Rp P tau0 (app_subst (cS Phi) tau0) (fst a) Gb) arms Gbs ->
  Forall (fun Gb => Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA Phi))) Gb) Gbs ->
  Forall2 (fun a Gb => xhas R L Phi (Gb ++ G) (snd a) tau) arms Gbs ->
  xvt w (app_subst g (app_subst (cS Phi) tau0)) ->
  exists bs eb Gb, select_arm arms w = Some (bs, eb)
  /\ xhas R L Phi (Gb ++ G) eb tau
  /\ Forall2 (fun q b => fst q = fst b /\ xvt (snd b) (app_subst g (snd q))) Gb bs
  /\ Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA Phi))) Gb.
Proof.
  intros Phi g tau0 G R L tau arms Gbs w Hg Hwf Hnd Hc Hpats Hregs Hbodies Hw.
  induction Hpats as [|a Gb arms Gbs Hp Hps IH]; [destruct Hc as [? [[] _]]|].
  inversion Hbodies as [|? ? ? ? Hb Hbs]; subst.
  inversion Hregs as [|? ? Hr Hrs]; subst.
  pose proof (@pat_sound Phi g tau0 (fst a) Gb w Hg Hwf Hnd Hp Hw) as Hs.
  destruct a as [pa eb]. simpl in *.
  destruct (match_pat pa w) as [bs | |] eqn:Em; [| | contradiction].
  - exists bs, eb, Gb. auto.
  - apply IH; auto.
    destruct Hc as [a' [[E | Hin] Hne]]; [| eauto].
    subst a'. simpl in Hne. contradiction.
Qed.

Lemma xsafe0_pop1 : forall g Phi x t G R L tau r,
  xsafe0 g Phi ((x, t) :: G) R L tau r -> xsafe0 g Phi G R L tau (pop1 r).
Proof.
  intros g Phi x t G R L tau [o gr st] H. unfold xsafe0 in *. simpl in *.
  destruct o; auto.
  - destruct H as [Hw Hv]. split; [exact Hw | exact (@venv_tail g (cA Phi) G gr _ Hv)].
  - destruct H as [E Hv]. split; [exact E | exact (@venv_tail g (cA Phi) G gr _ Hv)].
Qed.

Lemma xsafe0_popn : forall g Phi Gb G R L tau r,
  xsafe0 g Phi (Gb ++ G) R L tau r -> xsafe0 g Phi G R L tau (popn (List.length Gb) r).
Proof.
  intros g Phi Gb G R L tau [o gr st] H. unfold xsafe0 in *. simpl in *.
  destruct o; auto.
  - destruct H as [Hw Hv]. split; [exact Hw | exact (@venv_skip g (cA Phi) Gb G gr Hv)].
  - destruct H as [E Hv]. split; [exact E | exact (@venv_skip g (cA Phi) Gb G gr Hv)].
Qed.

Lemma xsafe0_sub : forall g Phi G R L tau' tau r,
  xsafe0 g Phi G R L tau' r -> subty Phi tau' tau -> xsafe0 g Phi G R L tau r.
Proof.
  intros g Phi G R L tau' tau [o gr st] H Hs. unfold xsafe0 in *. simpl in *.
  destruct o; auto. destruct H as [Hw Hv]. split; [exact (@xvt_subty Phi g tau' tau x Hs Hw) | exact Hv].
Qed.


(* ===================== 6c. Functor worlds (CoreMoss §11b, Rule A) ======== *)

Lemma lookup_xdenv_app : forall i d1 d2,
  lookup_xdenv i (d1 ++ d2) =
  match lookup_xdenv i d1 with Some en => Some en | None => lookup_xdenv i d2 end.
Proof.
  intros i d1 d2. induction d1 as [| [j en] r IH]; simpl; [reflexivity |].
  destruct (item_eqb i j); [reflexivity | exact IH].
Qed.

(* One link: satisfying a telescope at the identity application hands it an
   agreeing world. *)
Lemma xlink_agree : forall Phi Din PhiIn sm d g,
    wf_tele Sg empty_ctx Din PhiIn ->
    sat0 Sg Phi Din ->
    smap_ok Phi (id_app Din) sm Din ->
    ctx_grounded g Phi ->
    xdenv_values g (cA Phi) d ->
    exists d' g', xcompose d sm = Some d'
    /\ ctx_grounded g' PhiIn
    /\ xdenv_strong g' (cA PhiIn) d'
    /\ (forall x, In x (tyreqs (cA PhiIn)) ->
          app_subst g' (TAbs x) = app_subst g (TAbs x))
    /\ incl (tyreqs (cA PhiIn)) (tyreqs (cA Phi)).
Proof.
  intros Phi Din PhiIn sm d g Wt Hs0 Hsm Hg Hd.
  pose proof (wf_tele_ctx Wt) as Htc.
  pose proof (sat0_tyreqs_incl Hs0 Htc) as Hincl.
  destruct Hs0 as [PhiD1 [Htc1 [Hsat' Hav']]].
  rewrite Htc in Htc1. injection Htc1 as <-.
  assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0) = app_subst g t0).
  { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
  assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app (cA PhiIn))).
  { apply Forall_forall. intros p Hp. unfold id_app in Hp.
    apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
    apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
  assert (Hsat : sat Sg Phi (id_app (cA PhiIn)) Din)
    by (exists PhiIn; split; [exact Htc | exact Hsat']).
  pose proof (@safety_case_bindmth_aux8 Phi Din (cA PhiIn) sm Din Hsm) as Hsm2.
  destruct (@xcallee_env_values Sg Rp P Fb Hst g Phi (id_app (cA PhiIn)) sm Din d PhiIn
              WF Wt Hsat Hwfth Hsm2 Hg Hd) as [d' [Hcomp [Hcg Hda]]].
  exists d', (callee_grounding g Phi (id_app (cA PhiIn)) (cA PhiIn)).
  split; [exact Hcomp |]. split; [exact Hcg |]. split; [exact Hda |].
  split; [| exact Hincl].
  intros x Hx.
  rewrite callee_grounding_reads by (intros y [Hy | []]; subst y; exact Hx).
  rewrite safety_case_bindfn_aux3. apply Habs.
Qed.

(* A binding's chain, link after link. *)
Lemma xchain_agree : forall h sms u, chain_ok Sg h sms u ->
    forall Du Dru bu PhiU d g,
    g_fun Sg u = Some (Du, Dru, bu) ->
    wf_tele Sg empty_ctx Du PhiU ->
    ctx_grounded g PhiU ->
    xdenv_values g (cA PhiU) d ->
    exists Dh Drh bh PhiH d' g',
      g_fun Sg h = Some (Dh, Drh, bh)
      /\ wf_tele Sg empty_ctx Dh PhiH
      /\ xrun_chain d sms = Some d'
      /\ ctx_grounded g' PhiH
      /\ xdenv_values g' (cA PhiH) d'
      /\ (forall x, In x (tyreqs (cA PhiH)) ->
            app_subst g' (TAbs x) = app_subst g (TAbs x))
      /\ incl (tyreqs (cA PhiH)) (tyreqs (cA PhiU)).
Proof.
  intros h sms u H.
  induction H as [u | h sm r w u Dh Drh bh Dw Drw bw Hh Hw Hl Hch IH];
    intros Du Dru bu PhiU d g Hu Wu Hg Hd.
  - exists Du, Dru, bu, PhiU, d, g.
    split; [exact Hu |]. split; [exact Wu |]. split; [reflexivity |].
    split; [exact Hg |]. split; [exact Hd |].
    split; [intros; reflexivity | intros y Hy; exact Hy].
  - destruct (IH Du Dru bu PhiU d g Hu Wu Hg Hd)
      as [Dw' [Drw' [bw' [PhiW [dW [gW [Hw' [Ww [Hrc [HgW [HdW [HrW HiW]]]]]]]]]]]].
    rewrite Hw in Hw'. injection Hw' as <- <- <-.
    destruct (wf_fun_decl WF _ Hh) as [PhiH [Wh _]].
    destruct Hl as [PhiOut [HtcW [Hs0 Hsm]]].
    rewrite (wf_tele_ctx Ww) in HtcW. injection HtcW as <-.
    destruct (@xlink_agree PhiW Dh PhiH sm dW gW Wh Hs0 Hsm HgW HdW)
      as [d' [g' [Hcomp [Hg' [Hd' [Hr' Hi']]]]]].
    exists Dh, Drh, bh, PhiH, d', g'.
    split; [exact Hh |]. split; [exact Wh |].
    split; [simpl; rewrite Hrc; exact Hcomp |].
    split; [exact Hg' |]. split; [exact (strong_values Hd') |].
    split.
    + intros x Hx. rewrite (Hr' x Hx). apply HrW. apply Hi'. exact Hx.
    + intros x Hx. apply HiW. apply Hi'. exact Hx.
Qed.

Lemma fixed_regular : forall Phi u,
  Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
  incl (dep_tys Sg (IFun u) ++ item_ty_frees (IFun u) ++ method_item_frees Sg (IFun u))
       (tyreqs (cA Phi)).
Proof.
  intros Phi u Hfx x Hx. simpl in Hx. rewrite app_nil_r in Hx.
  rewrite Forall_forall in Hfx. apply wf_ty_frees_aux3. apply avail_ty_in.
  exact (Hfx x Hx).
Qed.

(* A binding's chain from a functor with a body: the first link, then the
   chain among abstract functors. *)
Lemma xbchain_agree : forall Dh sms u, body_chain Sg Dh sms u ->
    forall Du Dru PhiU PhiH d g,
    g_fun Sg u = Some (Du, Dru, None) ->
    wf_tele Sg empty_ctx Du PhiU ->
    wf_tele Sg empty_ctx Dh PhiH ->
    ctx_grounded g PhiU ->
    xdenv_values g (cA PhiU) d ->
    exists d' g',
      xrun_chain d sms = Some d'
      /\ ctx_grounded g' PhiH
      /\ xdenv_strong g' (cA PhiH) d'
      /\ (forall x, In x (tyreqs (cA PhiH)) ->
            app_subst g' (TAbs x) = app_subst g (TAbs x))
      /\ incl (tyreqs (cA PhiH)) (tyreqs (cA PhiU)) /\ typefree d'.
Proof.
  intros Dh sms u [sm [r [w [Dw [Drw [-> [Hw [Hl Hch]]]]]]]]
         Du Dru PhiU PhiH d g Hu Wu Wh Hg Hd.
  destruct (@xchain_agree w r u Hch Du Dru None PhiU d g Hu Wu Hg Hd)
    as [Dw' [Drw' [bw' [PhiW [dW [gW [Hw' [Ww [Hrc [HgW [HdW [HrW HiW]]]]]]]]]]]].
  rewrite Hw in Hw'. injection Hw' as <- <- <-.
  destruct Hl as [PhiOut [HtcW [Hs0 Hsm]]].
  rewrite (wf_tele_ctx Ww) in HtcW. injection HtcW as <-.
  destruct (@xlink_agree PhiW Dh PhiH sm dW gW Wh Hs0 Hsm HgW HdW)
    as [d' [g' [Hcomp [Hg' [Hd' [Hr' Hi']]]]]].
  exists d', g'.
  split; [simpl; rewrite Hrc; exact Hcomp |].
  split; [exact Hg' |]. split; [exact Hd' |].
  split.
  - intros x Hx. rewrite (Hr' x Hx). apply HrW. apply Hi'. exact Hx.
  - split.
    + intros x Hx. apply HiW. apply Hi'. exact Hx.
    + eapply smap_composed_typefree; [exact Hsm|exact Hcomp].
Qed.

Lemma body_chain_snoc : forall Dh sms w u sm Dw Drw Du Dru,
  body_chain Sg Dh sms w ->
  g_fun Sg w = Some (Dw, Drw, None) ->
  g_fun Sg u = Some (Du, Dru, None) ->
  fun_link Sg Dw sm Du ->
  body_chain Sg Dh (sms ++ [sm]) u.
Proof.
  intros Dh sms w u sm Dw Drw Du Dru [sm0 [r [w0 [Dw0 [Drw0 [-> [Hw0 [Hl0 Hch]]]]]]]] Hw Hu Hl.
  exists sm0, (r ++ [sm]), w0, Dw0, Drw0. split; [reflexivity |].
  split; [exact Hw0 |]. split; [exact Hl0 |].
  eapply chain_ok_snoc; [exact Hch | exact Hw | exact Hu | exact Hl].
Qed.

(* A functor with a body (CoreMoss.fun_body_regular, from xwf): its linked
   arguments, captured inputs, pins, body, and the regularity of its result
   formed over them. *)
Lemma fb_regular : xp_immutable P=false ->forall h Dh Dc pins Dr bs,
  Fb h = Some (Dh, Dc, pins) -> xp_fun P h = Some (Dr, bs) ->
  exists PhiD PhiC, tele_ctx Dh = Some PhiD
  /\ wf_tele Sg empty_ctx Dh PhiD
  /\ wf_tele Sg empty_ctx Dc PhiC
  /\ pins_ok pins PhiC
  /\ wf_tele Sg (fun_ctx PhiD PhiC pins) Dr
       (mkCtx (cA (fun_ctx PhiD PhiC pins) ++ Dr) (cS (fun_ctx PhiD PhiC pins)))
  /\ (exists PhiB, xbinds_ty Sg Rp P Fb (fun_ctx PhiD PhiC pins) bs PhiB
        /\ Forall (fun i => In i (cA PhiB)) Dr)
  /\ (forall i, In i (cA PhiD ++ cA PhiC ++ Dr) ->
        incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
             (tyreqs (cA PhiD ++ cA PhiC ++ Dr))).
Proof.
  intros Legacy h Dh Dc pins Dr bs Hh Hb.
  destruct (@xwf_fb Sg Rp P Fb WFX Legacy h Dh Dc pins Dr bs Hh Hb)
    as [PhiD [PhiC [Wa [Wc [Hp [Wr Hbody]]]]]].
  exists PhiD, PhiC. split; [exact (wf_tele_ctx Wa) |]. split; [exact Wa |].
  split; [exact Wc |]. split; [exact Hp |]. split; [exact Wr |].
  split; [exact Hbody |].
  intros i Hi. rewrite app_assoc in Hi. rewrite (app_assoc (cA PhiD)).
  pose proof (@tele_items_regular Sg _ Dr _ WF Wr) as R. simpl in R.
  refine (R _ i Hi).
  intros j Hj. rewrite tyreqs_app.
  apply in_app_or in Hj. destruct Hj as [Hj | Hj]; intros x Hx; apply in_or_app.
  - left. exact (@tele_items_regular Sg empty_ctx Dh PhiD WF Wa
                   (fun k Hk => match Hk with end) j Hj x Hx).
  - right. exact (@tele_items_regular Sg empty_ctx _ PhiC WF Wc
                   (fun k Hk => match Hk with end) j Hj x Hx).
Qed.

(* Where a symbol of a functor's linked arguments, or of what its result
   reads, comes from: φ's arguments or φ's outputs. *)
Lemma link_reads_bound : xp_immutable P=false ->forall u Du Dr h Dh Dc pins bs sm x,
    g_fun Sg u = Some (Du, Dr, None) ->
    Fb h = Some (Dh, Dc, pins) -> xp_fun P h = Some (Dr, bs) ->
    fun_link Sg Dh sm Du ->
    In x (tyreqs Dh ++ res_reads Sg Dr) ->
    In x (tyreqs Du ++ tyreqs Dr).
Proof.
  intros Legacy u Du Dr h Dh Dc pins bs sm x Hu Hh Hb Hl Hx.
  destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiU [HtcU [WU [_ HregU]]]].
  destruct (@fb_regular Legacy h Dh Dc pins Dr bs Hh Hb) as [PhiH [_ [HtcH _]]].
  apply in_app_or in Hx. destruct Hx as [Hx | Hx]; apply in_or_app.
  - left. destruct Hl as [PhiOut [HtcO [Hs0 _]]].
    rewrite HtcU in HtcO. injection HtcO as <-.
    apply (proj1 (tele_ctx_tyreqs _ HtcU x)).
    apply (sat0_tyreqs_incl Hs0 HtcH).
    apply (proj2 (tele_ctx_tyreqs _ HtcH x)). exact Hx.
  - unfold res_reads in Hx. apply in_flat_map in Hx. destruct Hx as [i [Hi Hx]].
    assert (Hin : In i (cA PhiU ++ Dr)) by (apply in_or_app; right; exact Hi).
    specialize (HregU i Hin x Hx). rewrite tyreqs_app in HregU.
    apply in_app_or in HregU. destruct HregU as [H | H].
    + left. apply (proj1 (tele_ctx_tyreqs _ HtcU x)). exact H.
    + right. exact H.
Qed.

(* `bind φ = h;` captures h's captured inputs here: a closure. *)
Lemma world_bindfun : xp_immutable P=false ->forall Phi u Du Dr h Dh Dc pins sm smc d g,
    g_fun Sg u = Some (Du, Dr, None) ->
    Fb h = Some (Dh, Dc, pins) ->
    (exists bs, xp_fun P h = Some (Dr, bs)) ->
    fun_link Sg Dh sm Du ->
    xcap_ok Sg Phi Du Dr u Dc pins smc ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    ctx_grounded g Phi ->
    xdenv_agree g (cA Phi) d ->
    exists dc, xcompose d smc = Some dc
    /\ ctx_grounded g (add_item (IFun u) Phi)
    /\ xdenv_agree g (cA (add_item (IFun u) Phi)) ((IFun u, XDFun h [sm] (xclose d dc)) :: d).
Proof.
  intros Legacy Phi u Du Dr h Dh Dc pins sm smc d g Hu Hh [bs Hb] Hl [Hs0 [Hsmc [Hpins Hstat]]] Hfx Hg Hd.
  destruct (@fb_regular Legacy h Dh Dc pins Dr bs Hh Hb)
    as [PhiH [PhiC [_ [_ [Wc [[Hpd Hpr] _]]]]]].
  destruct (@xlink_agree Phi _ PhiC smc d g Wc Hs0 Hsmc Hg (proj1 Hd))
    as [dc [g' [Hcomp [Hg' [Hd' [Hr' HiC]]]]]].
  assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0) = app_subst g t0).
  { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
  assert (Hclosed : xdenv_strong g' (cA PhiC) (xclose d dc)) by (apply xdenv_strong_close; exact Hd').
  assert (Hcapture : runtime_types P g' (cA PhiC) (xclose d dc)).
  { eapply runtime_types_capture_incl; [exact (proj2 Hd)|exact HiC|exact Hr'|].
    eapply smap_composed_typefree; eauto. }
  assert (Hnew : xdentry_agree g (IFun u) (XDFun h [sm] (xclose d dc))).
  { simpl. rewrite Hu, Hh, Hb. split; [reflexivity |].
    split; [exists sm, [], u, Du, Dr; split; [reflexivity |];
            split; [exact Hu |]; split; [exact Hl | apply ChNil] |].
    destruct Hg' as [Hgr [Hab' [Hcov _]]].
    destruct Hclosed as [Hdc1 [Hdc2 [_ Hdc4]]].
    exists PhiC, g'. split; [exact (wf_tele_ctx Wc) |].
    split; [exact Hgr |]. split; [exact Hcov |]. split; [exact Hab' |].
    split.
    { (* the pins hold where h is bound *)
      intros t tau Hlk.
      pose proof (callee_env_agree_aux1 _ _ Hlk) as Hin.
      assert (Ht : In t (tyreqs (cA PhiC)))
        by (apply Hpd; apply in_map_iff; exists (t, tau); split; [reflexivity | exact Hin]).
      destruct (Hcov t Ht) as [a Ha]. rewrite Ha. f_equal.
      transitivity (app_subst g' (TAbs t)); [simpl; rewrite Ha; reflexivity |].
      rewrite (Hr' t Ht).
      rewrite Forall_forall in Hpins. pose proof (Hpins (t, tau) Hin) as Hp.
      cbn [fst snd] in Hp.
      transitivity (app_subst g (app_subst (cS Phi) (TAbs t))); [symmetry; apply Habs |].
      rewrite Hp. rewrite Habs. symmetry. apply dentry_agree_retype_aux1.
      intros z Hz. apply Hr'. rewrite Forall_forall in Hpr.
      exact (Hpr (t, tau) Hin z Hz). }
    split; [exact Hdc1 |]. split; [exact Hdc4 |].
    split; [|split; [exact Hcapture|apply xdentry_all_Forall; exact Hdc2]].
    intros x Hx Hxr. split; [| exact (Hr' x Hx)].
    apply Hstat; [apply (proj1 (tele_ctx_tyreqs _ (wf_tele_ctx Wc) x)); exact Hx |].
    exact (@link_reads_bound Legacy u Du Dr h Dh Dc pins bs sm x Hu Hh Hb Hl Hxr). }
  exists dc. split; [exact Hcomp |]. split; [exact Hg |].
  apply xdenv_agree_push; [exact Hd |reflexivity| exact Hnew | exact (@fixed_regular Phi u Hfx)].
Qed.

Lemma world_bindfuna : forall Phi u Du Dr w Dw sm d g,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg w = Some (Dw, Dr, None) ->
    avail Phi (IFun w) ->
    fun_link Sg Dw sm Du ->
    incl (g_fixed Sg w) (g_fixed Sg u) ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    ctx_grounded g Phi ->
    xdenv_agree g (cA Phi) d ->
    exists h sms dc, lookup_xdenv (IFun w) d = Some (XDFun h sms dc)
    /\ ctx_grounded g (add_item (IFun u) Phi)
    /\ xdenv_agree g (cA (add_item (IFun u) Phi))
                  ((IFun u, XDFun h (sms ++ [sm]) dc) :: d).
Proof.
  intros Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hg Hd.
  pose proof (proj1 Hd) as [Hcov [Hent _]].
  assert (Hin : In (IFun w) (cA Phi)) by (apply avail_fun_in; exact Hav).
  rewrite Forall_forall in Hcov.
  destruct (Hcov _ Hin) as [Hbad | [en Hlk]]; [discriminate Hbad |].
  pose proof (Hent _ _ Hin eq_refl Hlk) as Hag.
  destruct en as [wv | fc dc0 | h sms dc | p | hs | ty | p held]; simpl in Hag; try contradiction.
  rewrite Hw in Hag.
  destruct (Fb h) as [[[Dh Dc] pins]|] eqn:Hh; try contradiction.
  destruct (xp_fun P h) as [[Drp bs]|] eqn:Ep; try contradiction.
  destruct Hag as [E1 [Hch [PhiC [g' [H1 [H2 [H3 [H4 [H5 [H6 [H7 [HK [Htypes H8]]]]]]]]]]]]].
  subst Drp.
  exists h, sms, dc. split; [exact Hlk |]. split; [exact Hg |].
  apply xdenv_agree_push; [exact Hd |reflexivity| | exact (@fixed_regular Phi u Hfx)].
  simpl. rewrite Hu, Hh, Ep. split; [reflexivity |].
  split; [exact (@body_chain_snoc Dh sms w u sm Dw Dr Du Dr Hch Hw Hu Hl) |].
  exists PhiC, g'. repeat (split; [assumption |]). split; [|split; assumption].
  intros x Hx Hxr. destruct (HK x Hx Hxr) as [Hxw Heq].
  split; [apply Hxx; exact Hxw | exact Heq].
Qed.


End Fixed.

(* ===================== 6d. Across store typings =========================== *)

Local Notation xvt := (XTyping.xvt Sg Rp P).
Local Notation xdentry_agree := (XEnv.xdentry_agree Sg Rp P Fb).
Local Notation xdenv_agree := (provision_environment Sg Rp P Fb).
Lemma shared_mono : forall H H' g A d,sext H H' ->
 xdenv_agree H g A d ->xdenv_agree H' g A d.
Proof. intros;eapply (@provision_environment_mono Sg Rp P Fb);eassumption. Qed.
Arguments shared_mono {H H' g A d} _ _.
Local Notation xdenv_strong := (XEnv.xdenv_strong Sg Rp P Fb).
Local Notation xdenv_values := (XEnv.xdenv_values Sg Rp P Fb).
Local Notation venv_ok := (XEnv.venv_ok Sg Rp P).

(* The heap typing a store typing H reads: an address's contents satisfy
   value typing at H's type for it. *)
Definition PsiH (H : sty) : hty :=
  fun kk => match H kk with
            | Some (k, T) => Some (k, fun v => xvt H v T)
            | None => None
            end.

(* What a run may end in, from store typing H: some extension H' of H the
   machine it ends with agrees with, and the outcome as xsafe0 reads it at
   H'. *)
Definition xsafe (H : sty) (g : subst) (Phi : ctx) (G : tenv) (R : option ty) (L : bool)
    (tau : ty) (r : xres) : Prop :=
  exists H', sext H H' /\ HL.heap_ok (PsiH H') (xr_st r) /\ xsafe0 H' g Phi G R L tau r.

Definition xsafe_ctl (H : sty) (g : subst) (Phi : ctx) (G : tenv) (R : option ty) (L : bool)
    (r : xres) : Prop :=
  exists H', sext H H' /\ HL.heap_ok (PsiH H') (xr_st r) /\ xsafe0_ctl H' g Phi G R L r.

Definition XSAFE (fuel : nat) : Prop :=
  forall R L Phi G e tau d ge g st H,
    xhas R L Phi G e tau ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    venv_ok H g (cA Phi) G ge ->
    HL.heap_ok (PsiH H) st ->
    xsafe H g Phi G R L tau (xinterp fuel P d ge st e).

Lemma xsafe_ext : forall H H1 g Phi G R L tau r,
  sext H H1 -> xsafe H1 g Phi G R L tau r -> xsafe H g Phi G R L tau r.
Proof.
  intros H H1 g Phi G R L tau r Hx [H' [Hx' [Hh R0]]].
  exists H'. split; [exact (sext_trans Hx Hx') | split; assumption].
Qed.

Lemma xsafe_ctl_ext : forall H H1 g Phi G R L r,
  sext H H1 -> xsafe_ctl H1 g Phi G R L r -> xsafe_ctl H g Phi G R L r.
Proof.
  intros H H1 g Phi G R L r Hx [H' [Hx' [Hh R0]]].
  exists H'. split; [exact (sext_trans Hx Hx') | split; assumption].
Qed.

Lemma xsafe_map : forall H g Phi G R L tau g' Phi' G' R' L' tau' r r',
  xr_st r' = xr_st r ->
  (forall H', sext H H' -> xsafe0 H' g Phi G R L tau r -> xsafe0 H' g' Phi' G' R' L' tau' r') ->
  xsafe H g Phi G R L tau r -> xsafe H g' Phi' G' R' L' tau' r'.
Proof.
  intros H g Phi G R L tau g' Phi' G' R' L' tau' r r' E F [H' [Hx [Hh R0]]].
  exists H'. rewrite E. split; [exact Hx | split; [exact Hh | exact (F H' Hx R0)]].
Qed.

Lemma xsafe_pass : forall H g Phi G R L tau tau' r,
  xsafe H g Phi G R L tau r -> (forall w, xr_out r <> XOk w) -> xsafe H g Phi G R L tau' r.
Proof.
  intros H g Phi G R L tau tau' r S0 Hn.
  eapply xsafe_map; [reflexivity | | exact S0].
  intros H' _ R0. eapply xsafe0_pass; eassumption.
Qed.

Lemma xsafe_of_ctl : forall H g Phi G R L tau r,
  xsafe_ctl H g Phi G R L r -> xsafe H g Phi G R L tau r.
Proof.
  intros H g Phi G R L tau r [H' [Hx [Hh R0]]].
  exists H'. split; [exact Hx | split; [exact Hh | eapply xsafe0_of_ctl; exact R0]].
Qed.

Lemma xsafe_ctl_of : forall H g Phi G R L tau r,
  xsafe H g Phi G R L tau r -> (forall w, xr_out r <> XOk w) -> xsafe_ctl H g Phi G R L r.
Proof.
  intros H g Phi G R L tau r [H' [Hx [Hh R0]]] Hn.
  exists H'. split; [exact Hx | split; [exact Hh | eapply xsafe0_ctl_of; eassumption]].
Qed.

(* A value-producing result, at an extension. *)
Lemma xsafe_ok : forall H H' g Phi G R L tau w ge st,
  sext H H' -> HL.heap_ok (PsiH H') st ->
  xvt H' w (app_subst g (app_subst (cS Phi) tau)) -> venv_ok H' g (cA Phi) G ge ->
  xsafe H g Phi G R L tau (XR (XOk w) ge st).
Proof.
  intros H H' g Phi G R L tau w ge st Hx Hh Hw Hv.
  exists H'. split; [exact Hx | split; [exact Hh |]]. split; assumption.
Qed.

(* ---- Primitives, with the heap. ---- *)

Lemma contents_subst : forall s th x,
  contents (map_snd (app_subst s) th) x = app_subst s (contents th x).
Proof.
  intros s th x. unfold contents. rewrite lookup_map_snd.
  destruct (lookup_subst x th); reflexivity.
Qed.

Lemma is_ref_subst : forall s k t c,
  is_ref Sg Rp P k t c -> is_ref Sg Rp P k (app_subst s t) (app_subst s c).
Proof.
 intros s k t c [n [th [x [-> [Hn [<- Hschema]]]]]].
 exists n, (map_snd (app_subst s) th), x; split; [reflexivity|]; split; [exact Hn|]; split.
 - apply contents_subst.
 - eapply gc_schema_subst; eauto.
Qed.

Lemma pty_ok_subst' : forall s tau t, pty_ok tau t -> pty_ok (app_subst s tau) t.
Proof. intros. apply pty_ok_subst. assumption. Qed.

Lemma prim_ok_subst : forall s p taus ret,
  prim_ok Sg Rp P p taus ret -> prim_ok Sg Rp P p (map (app_subst s) taus) (app_subst s ret).
Proof.
  intros s p taus ret [Hu | [[ps [r [Hsig [Hps Hr]]]] | Hgc]].
  - left. exact Hu.
  - right. left. exists ps, r. split; [exact Hsig |]. split.
    + clear -Hps. induction Hps; simpl; constructor; [apply pty_ok_subst' |]; assumption.
    + apply pty_ok_subst'. exact Hr.
  - right. right.
    destruct p; simpl in Hgc |- *; try contradiction;
      repeat (destruct taus as [|? taus]; simpl in Hgc |- *; try contradiction).
    all: repeat match goal with
         | Hx : _ /\ _ |- _ => destruct Hx
         | Hx : exists _, _ |- _ => destruct Hx
         end; subst; repeat split.
    all: try first
         [ reflexivity
         | (apply pty_ok_subst'; assumption)
         | (apply is_ref_subst; assumption)
         | (eexists; apply is_ref_subst; eassumption)
         | (eexists; split; apply is_ref_subst; eassumption)
         | (eexists; eexists; split; [apply app_subst_nom | split; [eassumption | apply contents_subst]])
         | (eexists; eexists; split; [apply app_subst_nom | eassumption]) ].
  all: eexists; eexists; split; [apply app_subst_nom|]; split; [eassumption|]; split;
    [apply contents_subst|eapply gc_schema_subst; eauto].
Qed.

Lemma PsiH_some : forall H kk k T, H kk = Some (k, T) -> PsiH H kk = Some (k, fun v => xvt H v T).
Proof. intros H kk k T E. unfold PsiH. rewrite E. reflexivity. Qed.

Lemma ref_typed : forall H k w t c,
  xvt H w t -> is_ref Sg Rp P k t c ->
  exists n a, xgc_receiver w = XVTag n (XVRef a) /\ H (key a) = Some (k, c)
  /\ href (PsiH H) k (xgc_receiver w) (fun v => xvt H v c).
Proof.
 intros H k w t c Hw Hr; destruct (gc_receiver_typed Hw Hr) as [n [a [He Ha]]].
 exists n,a; split; [exact He|]; split; [exact Ha|].
 exists n,a; split; [exact He|exact (PsiH_some _ _ Ha)].
Qed.

Lemma i32_typed : forall H w t, xvt H w t -> pty_ok t XTI32 -> exists i, w = XVI32 i.
Proof.
  intros H w t Hw [n [th [-> Hn]]]. inversion Hw; subst; try congruence; try solve[eauto].
  match goal with Hgc : _ = Some (if ?k then RArr _ else RRef _) |- _ => destruct k; simpl in *; congruence end.
Qed.

Definition supd (H : sty) (kk : list bool) (c : bool * ty) : sty :=
  fun kk' => if list_eq_dec bool_dec kk kk' then Some c else H kk'.

(* After a GC primitive that allocates nothing: the same store typing. *)
Lemma gc_same : forall H Psi' st',
  hext (PsiH H) Psi' -> HL.heap_ok Psi' st' -> HL.heap_ok (PsiH H) st'.
Proof.
  intros H Psi' st' Hx Hh. apply (HL.heap_mono _ _ _ Hh).
  intros a k Q' E. exists Q'. split; [exact (Hx _ _ _ E) | auto].
Qed.

(* After an allocation at a: the store typing extended there. *)
Lemma gc_alloc : forall H Psi' st' a k c,
  hext (PsiH H) Psi' -> PsiH H (key a) = None ->
  Psi' (key a) = Some (k, fun v => xvt H v c) -> HL.heap_ok Psi' st' ->
  sext H (supd H (key a) (k, c)) /\ HL.heap_ok (PsiH (supd H (key a) (k, c))) st'.
Proof.
  intros H Psi' st' a k c Hx Hf Ha Hh.
  assert (HN : H (key a) = None).
  { unfold PsiH in Hf. destruct (H (key a)) as [[? ?]|]; [discriminate | reflexivity]. }
  assert (Hext : sext H (supd H (key a) (k, c))).
  { intros kk c0 E. unfold supd. destruct (list_eq_dec bool_dec (key a) kk) as [<- | _];
      [congruence | exact E]. }
  split; [exact Hext |].
  apply (HL.heap_mono _ _ _ Hh). intros kk k' Q' E. unfold PsiH, supd in E.
  destruct (list_eq_dec bool_dec (key a) kk) as [<- | Hne].
  - injection E as <- <-. exists (fun v => xvt H v c). split; [exact Ha |].
    intros v Hv. exact (xvt_mono Hext Hv).
  - destruct (H kk) as [[k0 T]|] eqn:Ek; [| discriminate]. injection E as <- <-.
    exists (fun v => xvt H v T). split; [exact (Hx _ _ _ (PsiH_some _ _ Ek)) |].
    intros v Hv. exact (xvt_mono Hext Hv).
Qed.

Lemma supd_here : forall H kk c, supd H kk c kk = Some c.
Proof. intros. unfold supd. destruct (list_eq_dec bool_dec kk kk); congruence. Qed.

(* A GC primitive at its shape: a value of its result type at an extension,
   or a trap. *)
Lemma gc_run : forall H p taus rty ws st,
  gc_shape Sg Rp P p taus rty -> Forall2 (xvt H) ws taus -> HL.heap_ok (PsiH H) st ->
  match run_prim P p (xgc_args p ws) st with
  | (XOk w, st') => exists H', sext H H' /\ HL.heap_ok (PsiH H') st' /\ xvt H' w rty
  | (XTrap _, st') => HL.heap_ok (PsiH H) st'
  | _ => False
  end.
Proof.
  intros H p taus rty ws st Hs Hws Hh.
  pose proof (xwf_bool WFX) as Hb.
  assert (Gen : forall Q, gc_pre (PsiH H) p (xgc_args p ws) Q ->
    (forall Psi' w, hext (PsiH H) Psi' -> HL.heap_ok Psi' (snd (run_prim P p (xgc_args p ws) st)) ->
       gc_post (PsiH H) Psi' p Q w ->
       exists H', sext H H' /\ HL.heap_ok (PsiH H') (snd (run_prim P p (xgc_args p ws) st)) /\ xvt H' w rty) ->
    match run_prim P p (xgc_args p ws) st with
    | (XOk w, st') => exists H', sext H H' /\ HL.heap_ok (PsiH H') st' /\ xvt H' w rty
    | (XTrap _, st') => HL.heap_ok (PsiH H) st'
    | _ => False
    end).
  { intros Q Hpre Hpost. pose proof (HL.heap_gc P p (xgc_args p ws) st (PsiH H) Q Hh Hpre) as Hg.
    destruct (run_prim P p (xgc_args p ws) st) as [o st'] eqn:Er. simpl in Hpost.
    destruct o; try exact Hg.
    destruct Hg as [Psi' [Hh' Hpost']].
    exact (Hpost Psi' x (proj1 Hpost') Hh' Hpost'). }
  destruct p; simpl in Hs; try contradiction;
    repeat (destruct taus as [|? taus]; simpl in Hs; try contradiction);
    repeat match goal with
           | Hx : Forall2 _ _ (_ :: _) |- _ => inversion Hx; subst; clear Hx
           | Hx : Forall2 _ _ [] |- _ => inversion Hx; subst; clear Hx
           end.
  all: cbn [xgc_args] in Gen |- *.
  all: repeat match goal with Hx : _ /\ _ |- _ => destruct Hx end;
    repeat match goal with
           | Hw : xvt _ ?w ?t, Hi : pty_ok ?t XTI32 |- _ =>
               let i := fresh "i" in destruct (i32_typed Hw Hi) as [i ->]; clear Hi
           | Hw : xvt _ ?w ?t, Hi : exists n th, ?t = TNom n th /\ Rp n = Some RI32 |- _ =>
               let i := fresh "i" in destruct (i32_typed Hw Hi) as [i ->]; clear Hi
           end;
    repeat match goal with
           | Hw : xvt _ ?w ?t, Hr : is_ref Sg Rp P ?k ?t ?c |- _ =>
               let n := fresh "n" in let a := fresh "a" in let Ha := fresh "Ha" in
               let Hrr := fresh "Hrr" in
               destruct (ref_typed Hw Hr) as [n [a [Hreceiver [Ha Hrr]]]];
               rewrite Hreceiver in Gen,Hrr |- *; clear Hr Hreceiver
           end.
  - (* box *)
    destruct Hs as [thb [xb [-> [Hrf [Ec Hschema]]]]].
    match goal with Hv : xvt _ ?v ?c |- _ =>
      apply (Gen (fun v => xvt H v c)); [simpl; exact Hv |] end.
    intros Psi' w Hx Hh' [_ [a [-> [Hf Ha]]]].
    destruct (@gc_alloc H Psi' _ a false _ Hx Hf Ha Hh') as [Hext Hok].
    eexists. split; [exact Hext |]. split; [exact Hok |].
    destruct Hschema as [spec [Hlook [Hkind [Hheld [Hgate [D [pay [Htag Hpayload]]]]]]]].
    eapply XvRef; [exact Hrf| |exact Htag|exact Hpayload]. rewrite supd_here, Ec. reflexivity.
  - (* deref *)
    apply (Gen (fun v => xvt H v rty)); [assumption |].
    intros Psi' w Hx Hh' [_ Hw]. exists H. split; [apply sext_refl |].
    split; [exact (gc_same Hx Hh') | exact Hw].
  - (* ref set *)
    subst rty. match goal with Hr : href _ false _ (fun v => xvt H v ?c) |- _ =>
      apply (Gen (fun v => xvt H v c)); [simpl; split; assumption |] end.
    intros Psi' w Hx Hh' [_ Hw]. subst w. exists H. split; [apply sext_refl |].
    split; [exact (gc_same Hx Hh') | constructor].
  - (* array new *)
    match goal with Hs : ex _ |- _ => destruct Hs as [thb [xb [-> [Hrf [Ec Hschema]]]]] end.
    match goal with Hv : xvt _ ?v ?c, Ec : contents _ _ = ?c |- _ =>
      apply (Gen (fun v => xvt H v c)); [simpl; exact Hv |] end.
    intros Psi' w Hx Hh' [_ [a [-> [Hf Ha]]]].
    destruct (@gc_alloc H Psi' _ a true _ Hx Hf Ha Hh') as [Hext Hok].
    eexists. split; [exact Hext |]. split; [exact Hok |].
    destruct Hschema as [spec [Hlook [Hkind [Hheld [Hgate [D [pay [Htag Hpayload]]]]]]]].
    eapply XvArr; [exact Hrf| |exact Htag|exact Hpayload]. rewrite supd_here, Ec. reflexivity.
  - (* array length *)
    match goal with Hs : exists c, is_ref Sg Rp P true _ c |- _ => destruct Hs as [c Hr0] end.
    match goal with Hw : xvt _ ?w ?t |- _ =>
      destruct (ref_typed Hw Hr0) as [n [a [Hreceiver [Ha Hr]]]];
      rewrite Hreceiver in Gen,Hr |- * end.
    apply (Gen (fun v => xvt H v c)); [exact Hr |].
    intros Psi' w Hx Hh' [_ [m ->]]. exists H. split; [apply sext_refl |].
    split; [exact (gc_same Hx Hh') |].
    apply (@xvty_pty H _ XTI32 _ Hb); [discriminate | discriminate | assumption | exact I].
  - (* array get *)
    apply (Gen (fun v => xvt H v rty)); [simpl; assumption |].
    intros Psi' w Hx Hh' [_ Hw]. exists H. split; [apply sext_refl |].
    split; [exact (gc_same Hx Hh') | exact Hw].
  - (* array set *)
    subst rty. match goal with Hr : href _ true _ (fun v => xvt H v ?c) |- _ =>
      apply (Gen (fun v => xvt H v c)); [simpl; split; assumption |] end.
    intros Psi' w Hx Hh' [_ Hw]. subst w. exists H. split; [apply sext_refl |].
    split; [exact (gc_same Hx Hh') | constructor].
  - (* array fill *)
    subst rty. match goal with Hr : href _ true _ (fun v => xvt H v ?c) |- _ =>
      apply (Gen (fun v => xvt H v c)); [simpl; split; assumption |] end.
    intros Psi' w Hx Hh' [_ Hw]. subst w. exists H. split; [apply sext_refl |].
    split; [exact (gc_same Hx Hh') | constructor].
  - (* array copy *)
    subst rty.
    match goal with Hs : exists c, is_ref Sg Rp P true _ c /\ is_ref Sg Rp P true _ c |- _ =>
      destruct Hs as [c [Hr0 Hr2]] end.
    repeat match goal with
           | Hw : xvt _ ?w ?t, Hr : is_ref Sg Rp P true ?t c |- _ =>
               let n := fresh "n" in let a := fresh "a" in let Ha := fresh "Ha" in
               let Hrr := fresh "Hrr" in
               destruct (ref_typed Hw Hr) as [n [a [Hreceiver [Ha Hrr]]]];
               rewrite Hreceiver in Gen,Hrr |- *; clear Hr Hreceiver
           end.
    apply (Gen (fun v => xvt H v c)).
    + simpl. split; [eassumption |]. exists (fun v => xvt H v c). split; [eassumption | auto].
    + intros Psi' w Hx Hh' [_ Hw]. subst w. exists H. split; [apply sext_refl |].
      split; [exact (gc_same Hx Hh') | constructor].
Qed.

Lemma prim_sigs_not_gc : forall p ps r, prim_sigs p ps r -> is_gc p = false.
Proof. intros p ps r [E | E]; destruct p; simpl in E |- *; congruence. Qed.

Lemma xgc_args_not_gc : forall p ws, is_gc p = false -> xgc_args p ws = ws.
Proof. intros p ws H; destruct p; simpl in H |- *; try discriminate; reflexivity. Qed.

(* Any primitive at a signature it provides. *)
Lemma prim_run_h : forall H p taus rty ws st,
  prim_ok Sg Rp P p taus rty -> Forall2 (xvt H) ws taus -> HL.heap_ok (PsiH H) st ->
  match run_prim_at P p None ws st with
  | (XOk w, st') => exists H', sext H H' /\ HL.heap_ok (PsiH H') st' /\ xvt H' w rty
  | (XTrap _, st') | (XExit _, st') | (XUns _, st') => HL.heap_ok (PsiH H) st'
  | _ => False
  end.
Proof.
  unfold run_prim_at.
  intros H p taus rty ws st [[c ->] | [[ps [r [Hsig [Hps Hr]]]] | Hgc]] Hws Hh.
  - cbn [xgc_args]. pose proof (prim_ulaw P c ws st) as Hu.
    pose proof (HL.heap_other P (PUnsupported c) ws st (PsiH H) eq_refl Hh) as Hk.
    destruct (run_prim P (PUnsupported c) ws st) as [o st']. simpl in Hu, Hk. subst o. exact Hk.
  - rewrite xgc_args_not_gc by exact (prim_sigs_not_gc Hsig).
    pose proof (@prim_run H p ps r ws taus rty st Hsig Hws Hps Hr) as Hrun.
    pose proof (HL.heap_other P p ws st (PsiH H) (prim_sigs_not_gc Hsig) Hh) as Hk.
    destruct (run_prim P p ws st) as [o st']. simpl in Hrun, Hk.
    destruct o; try contradiction; try exact Hk.
    exists H. split; [apply sext_refl | split; [exact Hk | exact Hrun]].
  - pose proof (@gc_run H p taus rty ws st Hgc Hws Hh) as R0.
    destruct (run_prim P p (xgc_args p ws) st) as [[] st']; first [exact R0 | contradiction].
Qed.

Lemma xallocation_typed : forall H p held taus ret w,
 prim_at_ok Sg Rp P p held taus ret -> xvt H w ret ->
 match xallocation p held (XOk w) with XOk v => xvt H v ret | _ => False end.
Proof.
 intros H p held taus ret w [Hp Hheld] Hw.
 destruct p; simpl in Hheld; try contradiction.
 all: destruct taus as [|c ts]; [contradiction|].
 all: subst held; destruct Hp as [[code Bad]|[[ps [r [Bad Rest]]]|Shape]];
   try discriminate; try (destruct Bad; discriminate).
 - destruct ts as [|c0 ts]; simpl in Shape; try contradiction.
   destruct Shape as [th [x [-> [Hr [Hc Hschema]]]]].
   unfold xallocation; destruct w; try exact Hw.
   destruct w; try exact Hw.
   destruct (@gc_receiver_typed Sg Rp P H (XVTag n0 (XVRef i)) (TNom n th) false c Hw)
     as [n1 [a [He Hcell]]].
   + exists n,th,x; repeat split; auto.
   + simpl in He; inversion He; subst.
     assert (n1 = n) as -> by (inversion Hw; reflexivity).
     destruct Hschema as [spec [Hl [Hk [Hx [Hg [D [pay [Ht Hpay]]]]]]]].
     eapply XvRefAt; eauto.
 - destruct ts as [|i32 ts]; simpl in Shape; try contradiction.
   destruct ts; simpl in Shape; try contradiction.
   destruct Shape as [Hi [th [x [-> [Hr [Hc Hschema]]]]]].
   unfold xallocation; destruct w; try exact Hw.
   destruct w; try exact Hw.
   destruct (@gc_receiver_typed Sg Rp P H (XVTag n0 (XVRef i)) (TNom n th) true c Hw)
     as [n1 [a [He Hcell]]].
   + exists n,th,x; repeat split; auto.
   + simpl in He; inversion He; subst.
     assert (n1 = n) as -> by (inversion Hw; reflexivity).
     destruct Hschema as [spec [Hl [Hk [Hx [Hg [D [pay [Ht Hpay]]]]]]]].
     eapply XvArrAt; eauto.
Qed.

Lemma prim_run_at_h : forall H p held taus rty ws st,
 prim_at_ok Sg Rp P p held taus rty -> Forall2 (xvt H) ws taus -> HL.heap_ok (PsiH H) st ->
 match run_prim_at P p (Some held) ws st with
 | (XOk w,st') => exists H', sext H H' /\ HL.heap_ok (PsiH H') st' /\ xvt H' w rty
 | (XTrap _,st') | (XExit _,st') | (XUns _,st') => HL.heap_ok (PsiH H) st'
 | _ => False end.
Proof.
 intros H p held taus rty ws st Hpa Hws Hheap.
 pose proof (@prim_run_h H p taus rty ws st (proj1 Hpa) Hws Hheap) as Base.
 unfold run_prim_at in Base |- *.
 destruct (run_prim P p (xgc_args p ws) st) as [o st']; simpl in Base |- *.
 destruct o; try contradiction.
 - destruct Base as [H' [Hext [Hok Htyped]]].
   pose proof (@xallocation_typed H' p held taus rty x Hpa Htyped) as Ha.
   destruct (xallocation p held (XOk x)); try contradiction.
   exists H'; auto.
 - destruct p; exact Base.
 - destruct p; exact Base.
 - destruct p; exact Base.
Qed.

(* A primitive call's outcome, as the calling expression's. *)
Lemma prim_result : forall H g Phi G R L tau rty o g1 st',
  match (o, st') with
  | (XOk w, st') => exists H', sext H H' /\ HL.heap_ok (PsiH H') st' /\ xvt H' w rty
  | (XTrap _, st') | (XExit _, st') | (XUns _, st') => HL.heap_ok (PsiH H) st'
  | _ => False
  end ->
  rty = app_subst g (app_subst (cS Phi) tau) ->
  venv_ok H g (cA Phi) G g1 ->
  xsafe H g Phi G R L tau (XR o g1 st').
Proof.
  intros H g Phi G R L tau rty o g1 st' R0 E Hv.
  destruct o; simpl in R0; try contradiction;
    try (exists H; split; [apply sext_refl | split; [exact R0 | exact I]]).
  destruct R0 as [H' [Hx [Hh Hw]]]. subst rty.
  exact (@xsafe_ok H H' g Phi G R L tau x g1 st' Hx Hh Hw (venv_ok_mono Hx Hv)).
Qed.

Lemma this_read : forall (g0 : subst) (r1 r2 tau : ty),
  app_subst g0 r1 = app_subst g0 r2 ->
  app_subst g0 (app_subst (this_subst r1) tau)
  = app_subst g0 (app_subst (this_subst r2) tau).
Proof.
  intros g0 r1 r2 tau HE. unfold this_subst.
  induction tau using ty_ind'.
  - reflexivity.
  - simpl. destruct (Nat.eqb t this_sym); [ exact HE | reflexivity ].
  - simpl. f_equal. induction H; simpl; [reflexivity|].
    rewrite IHForall. destruct x as [t0 tau1]; simpl in *.
    rewrite H. reflexivity.
  - simpl. f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. reflexivity.
Qed.

(* ===================== 7. The cases ======================================= *)

Section Cases.
Variable k : nat.
Hypothesis IHfuel : forall j, j <= k -> XSAFE j.

Lemma xsafe_args : forall j, j <= k ->
  forall R L Phi G d g args taus ge st H,
    Forall2 (fun e tau => xhas R L Phi G e tau) args taus ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    venv_ok H g (cA Phi) G ge ->
    HL.heap_ok (PsiH H) st ->
    match xinterp_list j P d ge st args with
    | inl (ws, g1, st1) =>
        exists H1, sext H H1 /\ HL.heap_ok (PsiH H1) st1
        /\ Forall2 (fun w tau => xvt H1 w (app_subst g (app_subst (cS Phi) tau))) ws taus
        /\ venv_ok H1 g (cA Phi) G g1
    | inr r => xsafe_ctl H g Phi G R L r
    end.
Proof.
  intros j Hj R L Phi G d g args taus ge st H HF Hcg. revert j Hj ge st H.
  induction HF as [|e tau args taus Hty HF IH]; intros j Hj ge st H Hd Hv Hh.
  - destruct j; simpl.
    + exists H. split; [apply sext_refl | split; [exact Hh |]]. exact I.
    + exists H. split; [apply sext_refl | split; [exact Hh |]]. split; [constructor | exact Hv].
  - destruct j as [|j']; simpl.
    { exists H. split; [apply sext_refl | split; [exact Hh |]]. exact I. }
    assert (Hj' : j' <= k) by lia.
    pose proof (@IHfuel j' Hj' R L Phi G e tau d ge g st H Hty Hcg Hd Hv Hh) as Hres.
    destruct (xinterp j' P d ge st e) as [o g1 st1] eqn:E.
    destruct o;
      try (eapply xsafe_ctl_of; [exact Hres | intros ? ?; discriminate]).
    destruct Hres as [H1 [Hx1 [Hh1 [Hw Hv1]]]]. simpl in Hh1, Hv1.
    specialize (IH j' (Nat.le_trans _ _ _ (Nat.le_succ_diag_r j') Hj) g1 st1 H1
                  (shared_mono Hx1 Hd) Hv1 Hh1).
    destruct (xinterp_list j' P d g1 st1 args) as [[[ws g2] st2] | rr].
    + destruct IH as [H2 [Hx2 [Hh2 [Hws Hv2]]]].
      exists H2. split; [exact (sext_trans Hx1 Hx2) |]. split; [exact Hh2 |].
      split; [constructor; [exact (xvt_mono Hx2 Hw) | exact Hws] | exact Hv2].
    + exact (xsafe_ctl_ext Hx1 IH).
Qed.

(* the common shape of a call's argument list *)
Lemma xsafe_args_at : forall R L Phi G d g args (F : ty -> ty) taus ge st H,
    Forall2 (fun e tau => xhas R L Phi G e (F tau)) args taus ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    venv_ok H g (cA Phi) G ge ->
    HL.heap_ok (PsiH H) st ->
    match xinterp_list k P d ge st args with
    | inl (ws, g1, st1) =>
        exists H1, sext H H1 /\ HL.heap_ok (PsiH H1) st1
        /\ Forall2 (fun w tau => xvt H1 w (app_subst g (app_subst (cS Phi) (F tau)))) ws taus
        /\ venv_ok H1 g (cA Phi) G g1
    | inr r => xsafe_ctl H g Phi G R L r
    end.
Proof.
  intros R L Phi G d g args F taus ge st H HF Hcg Hd Hv Hh.
  pose proof (@xsafe_args k (le_n k) R L Phi G d g args (map F taus) ge st H
                (@F2_map_l _ _ (fun e tau => xhas R L Phi G e tau) F _ _ HF) Hcg Hd Hv Hh) as H0.
  destruct (xinterp_list k P d ge st args) as [[[ws g1] st1] | rr]; [|exact H0].
  destruct H0 as [H1 [Hx1 [Hh1 [W1 W2]]]]. exists H1. split; [exact Hx1 |]. split; [exact Hh1 |].
  split; [| exact W2].
  exact (@F2_map_r _ _ (fun w tau => xvt H1 w (app_subst g (app_subst (cS Phi) tau))) F _ _ W1).
Qed.

(* The world inside a closure, from its agreement. *)
Lemma closure_world : xp_immutable P=false ->forall H f' D' S' gc dc PhiD',
  g_fn Sg f' = Some (D', S', None) ->
  tele_ctx D' = Some PhiD' ->
  grounding gc -> covers gc (tyreqs (cA PhiD')) -> absorbs gc (cS PhiD') ->
  Forall (fun i => is_ty_item i = true \/ exists en, lookup_xdenv i dc = Some en) (cA PhiD') ->
  Forall (fun p => incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
                         ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiD'))) dc ->
  XRuntimeEnv.runtime_types P gc (cA PhiD') dc ->
  Forall (fun p => xdentry_agree H gc (fst p) (snd p)) dc ->
  exists PhiD, wf_tele Sg empty_ctx D' PhiD /\ PhiD = PhiD'
  /\ Forall (wf_ty Sg PhiD) (fs_params S') /\ wf_ty Sg PhiD (fs_ret S')
  /\ ctx_grounded gc PhiD /\ xdenv_agree H gc (cA PhiD) dc.
Proof.
  intros Legacy H f' D' S' gc dc PhiD' Hf' Htc Hgr Hcov Hab Htcov Hkreg Htypes Hall.
  destruct (wf_fn_decl WF _ Hf') as [PhiD [Wt [Wp [Wr _]]]].
  pose proof (wf_tele_ctx Wt) as Htc2. rewrite Htc in Htc2. injection Htc2 as E. subst PhiD.
  exists PhiD'. split; [exact Wt|]. split; [reflexivity|]. split; [exact Wp|]. split; [exact Wr|].
  split.
  - split; [exact Hgr | split; [exact Hab | split; [exact Hcov | split]]].
    + apply (@tele_sigma_regular Sg empty_ctx D' PhiD' Wt).
      split; [intros x Hx; destruct Hx | constructor].
    + apply (@callee_env_agree_aux13 Sg empty_ctx D' PhiD' Wt). intros Hx. destruct Hx.
  - unfold provision_environment;rewrite Legacy.
    apply strong_agree; [|exact Htypes].
    split; [exact Htcov | split; [exact Hall | split; [| exact Hkreg]]].
    rewrite Forall_forall. intros i Hi.
    apply (@tele_items_regular Sg empty_ctx D' PhiD' WF Wt); [| exact Hi].
    intros i0 Hi0. destruct Hi0.
Qed.

(* The body of a defined fn, typed at its declaration. *)
Lemma body_typed : forall f' D' S' body PhiD,
  g_fn Sg f' = Some (D', S', None) -> xp_fn P f' = Some body ->
  wf_tele Sg empty_ctx D' PhiD ->
  xhas (Some (fs_ret S')) false PhiD (body_env S') body (fs_ret S').
Proof.
  intros f' D' S' body PhiD Hf Hb Wt.
  destruct (@xwf_bodies Sg Rp P Fb WFX f' D' S' body Hf Hb) as [PhiD0 [Wt0 Hty]].
  apply wf_tele_ctx in Wt. apply wf_tele_ctx in Wt0. rewrite Wt in Wt0.
  injection Wt0 as ->. exact Hty.
Qed.

Lemma ret_call_safe_h : forall H gb Phib Gb rt r g Phi G R L tau g1,
  xsafe H gb Phib Gb (Some rt) false rt r ->
  absorbs gb (cS Phib) ->
  app_subst gb rt = app_subst g (app_subst (cS Phi) tau) ->
  venv_ok H g (cA Phi) G g1 ->
  xsafe H g Phi G R L tau (ret_call g1 r).
Proof.
  intros H gb Phib Gb rt r g Phi G R L tau g1 S0 Hab Heq Hv.
  apply (@xsafe_map H gb Phib Gb (Some rt) false rt g Phi G R L tau r (ret_call g1 r));
    [destruct r; reflexivity | | exact S0].
  intros H' Hx R0. eapply ret_call_safe; [exact R0 | exact Hab | exact Heq | exact (venv_ok_mono Hx Hv)].
Qed.

Lemma case_cast_tag : forall R L Phi G n D tau0 th e spec d ge g st H,
 lookup_cast n (xp_cast P) = Some spec ->
 Rp n = Some (if xc_array spec then RArr (xc_held spec) else RRef (xc_held spec)) ->
 g_tag Sg n = Some (D,tau0) -> sat Sg Phi th D ->
 Forall (fun p => wf_ty Sg Phi (snd p)) th ->
 xhas R L Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
 ctx_grounded g Phi -> xdenv_agree H g (cA Phi) d ->
 venv_ok H g (cA Phi) G ge -> HL.heap_ok (PsiH H) st ->
 xsafe H g Phi G R L (app_subst (cS Phi) (TNom n th))
  (xinterp (S k) P d ge st (XTag n th e)).
Proof.
 intros R L Phi G n D tau0 th e spec d ge g st H Hl Hr Htag Hsat Hth Hty Hcg Hd Hv Hhp.
 assert (Hcg0 := Hcg); destruct Hcg0 as [Gr [Abs [Cov Rest]]].
 assert (Hcol : forall a, app_subst g (app_subst (cS Phi) a) = app_subst g a)
   by (apply absorbs_all; exact Abs).
 assert (Reify : xreify P = true).
 { unfold xreify; destruct (xp_cast P) eqn:E; [simpl in Hl; discriminate|apply orb_true_r]. }
 set (logical := map_snd (app_subst g) th).
 assert (Hground : forall t a, lookup_subst t th = Some a -> xground d a = contents logical t).
 { intros t a Ha; unfold logical,contents; rewrite lookup_map_snd,Ha.
   eapply types_match_reads; [exact (@provision_types Sg Rp P Fb H g (cA Phi) d Hd Reify)|exact Cov|].
   rewrite Forall_forall in Hth.
   exact (wf_ty_frees (Hth (t,a) (@callee_env_agree_aux1 t th a Ha))). }
 assert (Hkeys : cast_keys spec th) by (eapply cast_keys_sat; eauto).
 destruct (wf_tag_decl WF _ Htag) as [PhiD [Wt Wpay]].
 assert (Hfr : incl (ty_frees tau0) (map fst th)).
 { destruct Hsat as [PhiD' [Htc [Hdom _]]].
   rewrite (wf_tele_ctx Wt) in Htc; inversion Htc; subst PhiD'.
   rewrite Hdom; exact Wpay. }
 pose proof (@IHfuel k (le_n k) R L Phi G e _ d ge g st H Hty Hcg Hd Hv Hhp) as S0.
 cbn [xinterp]; destruct (xinterp k P d ge st e) as [o ge1 st1] eqn:E.
 destruct o; try (eapply xsafe_pass; [exact S0|intros; discriminate]).
 destruct S0 as [H1 [Hx [Hheap [Hw Hv1]]]].
 assert (Hpay : xvt H1 x (app_subst logical tau0)).
 { unfold logical; rewrite <- app_subst_chain by exact Hfr.
   rewrite !Hcol in Hw; exact Hw. }
 pose proof (@xcast_typed Sg Rp P H1 d n th logical spec D tau0 x Hl Hr Htag Hkeys Hground Hpay) as Cast.
 change (match xcast P d n th x with
  | XOk v => xvt H1 v (TNom n logical) | XTrap _ => True | _ => False end) in Cast.
 destruct (xcast P d n th x); simpl in Cast; try contradiction.
 - exists H1; split; [exact Hx|]; split; [exact Hheap|].
   split; [|exact Hv1]. rewrite !Hcol, app_subst_nom; exact Cast.
 - exists H1; split; [exact Hx|]; split; [exact Hheap|exact I].
Qed.

Lemma case_need : forall R L Phi G f Df Sf args d ge g st H,
    g_fn Sg f = Some (Df, Sf, None) ->
    avail Phi (IFn f) ->
    Forall2 (fun e tau => xhas R L Phi G e (app_subst (cS Phi) tau)) args (fs_params Sf) ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    venv_ok H g (cA Phi) G ge ->
    HL.heap_ok (PsiH H) st ->
    xsafe H g Phi G R L (app_subst (cS Phi) (fs_ret Sf))
          (xinterp (S k) P d ge st (XCallA f args)).
Proof.
  intros R L Phi G f Df Sf args d ge g st H Hf Hav Hargs Hcg Hd Hv Hh.
  assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov [Hsreg Hthis]]]].
  assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau) = app_subst g tau)
    by (exact (@absorbs_all g (cS Phi) Hab)).
  assert (Hin := @avail_fn_in Phi f Hav).
  destruct(@provision_lookup Sg Rp P Fb H g(cA Phi)d(IFn f) Hd eq_refl Hin)
    as[en[Hlook Hda0]].
  pose proof (@xsafe_args_at R L Phi G d g args (app_subst (cS Phi)) (fs_params Sf) ge st H
                Hargs Hcg Hd Hv Hh) as Hsa.
  simpl. rewrite Hlook.
  destruct en as[w0|f' df|hh ss dc|p|hs|ty|p held];
    try solve[unfold provision_entry in Hda0;destruct(xp_immutable P);
      cbn[reified_entry xdentry_agree]in Hda0;contradiction].
  - (* a closure *)
    destruct(@provision_function_code Sg Rp P Fb H g f Df Sf f' df Hf Hda0)
      as[D'[S'[body[Hf' Hb]]]].
    rewrite Hb.
    destruct (xinterp_list k P d ge st args) as [[[ws g1] st1] | rr]; [| apply xsafe_of_ctl; exact Hsa].
    destruct Hsa as [H1 [Hx1 [Hh1 [Hws Hv1]]]].
    pose proof(@provision_entry_mono Sg Rp P Fb H H1 Hx1 g(IFn f)(XDClo f' df)Hda0)as Hda1.
    destruct(@provision_function_closure Sg Rp P Fb H1 g f f' df Df Sf D' S' body Hf Hf' Hb Hda1)
      as[PhiD'[g'[Htc'[Hgr'[Hcov'[Habs'[Hpar[Hret Hdb]]]]]]]].
    destruct(wf_fn_decl WF _ Hf')as[PhiD[Wt[Wp[Wr Rest]]]].
    pose proof(wf_tele_ctx Wt)as TD;rewrite Htc' in TD;injection TD as <-.
    pose proof(@provision_capture_grounded Sg D' PhiD' g' Wt Hgr' Hcov' Habs')as Hcgb.
    assert (Hws' : Forall2 (fun w tau => xvt H1 w (app_subst g' tau)) ws (fs_params S')).
    { assert (H2 : Forall2 (xvt H1) ws (map (app_subst g) (fs_params Sf))).
      { apply (@F2_map_l _ _ (xvt H1) (app_subst g)).
        refine (@F2_impl _ _ _ _ _ _ _ Hws). intros w tau Hw. rewrite !Hcol in Hw. exact Hw. }
      rewrite <- Hpar in H2. exact (@F2_map_r _ _ (xvt H1) (app_subst g') _ _ H2). }
    assert (Hvb : venv_ok H1 g' (cA PhiD') (body_env S') (mk_xvenv ws)).
    { apply venv_body'; [exact Hws'|].
      eapply Forall_impl; [| exact Wp]. intros tau Hwf. exact (wf_ty_frees Hwf). }
    pose proof (@IHfuel k (le_n k) _ _ _ _ _ _ df (mk_xvenv ws) g' st1 H1
                  (@body_typed f' D' S' body PhiD' Hf' Hb Wt) Hcgb Hdb Hvb Hh1) as Hres.
    apply (xsafe_ext Hx1).
    eapply ret_call_safe_h; [exact Hres | exact Habs' | | exact Hv1].
    rewrite Hret, !Hcol. reflexivity.
  - (* a primitive *)
    pose proof(@provision_function_primitive Sg Rp P Fb H g f Df Sf p Hf Hda0)as Hda.
    destruct (xinterp_list k P d ge st args) as [[[ws g1] st1] | rr]; [| apply xsafe_of_ctl; exact Hsa].
    destruct Hsa as [H1 [Hx1 [Hh1 [Hws Hv1]]]].
    assert (Hws' : Forall2 (xvt H1) ws (map (app_subst g) (fs_params Sf))).
    { apply (@F2_map_l _ _ (xvt H1) (app_subst g)).
      refine (@F2_impl _ _ _ _ _ _ _ Hws). intros w tau Hw. rewrite !Hcol in Hw. exact Hw. }
    pose proof (@prim_run_h H1 p _ _ ws st1 Hda Hws' Hh1) as Hrun.
    apply (xsafe_ext Hx1).
    destruct (run_prim_at P p None ws st1) as [o st2].
    eapply prim_result; [exact Hrun | rewrite !Hcol; reflexivity | exact Hv1].
  - (* a primitive with an allocation type header *)
    pose proof(@provision_function_primitive_at Sg Rp P Fb H g f Df Sf p held Hf Hda0)as Hda.
    destruct (xinterp_list k P d ge st args) as [[[ws g1] st1] | rr]; [| apply xsafe_of_ctl; exact Hsa].
    destruct Hsa as [H1 [Hx1 [Hh1 [Hws Hv1]]]].
    assert (Hws' : Forall2 (xvt H1) ws (map (app_subst g) (fs_params Sf))).
    { apply (@F2_map_l _ _ (xvt H1) (app_subst g)).
      refine (@F2_impl _ _ _ _ _ _ _ Hws). intros w tau Hw. rewrite !Hcol in Hw. exact Hw. }
    pose proof (@prim_run_at_h H1 p held _ _ ws st1 Hda Hws' Hh1) as Hrun.
    apply (xsafe_ext Hx1).
    destruct (run_prim_at P p (Some held) ws st1) as [o st2].
    eapply prim_result; [exact Hrun | rewrite !Hcol; reflexivity | exact Hv1].
Qed.

Lemma case_call : xp_immutable P=false ->forall R L Phi G f Df Sf body th th0 sm args d ge g st H,
    g_fn Sg f = Some (Df, Sf, None) ->
    xp_fn P f = Some body ->
    sat Sg Phi th Df ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Df ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th0 ->
    (forall t, In t (tyreqs Df) ->
      app_subst (cS Phi) (app_subst th0 (TAbs t)) =
      app_subst (cS Phi) (app_subst th (TAbs t))) ->
    Forall2 (fun e tau => xhas R L Phi G e (app_subst (cS Phi) (app_subst th tau)))
            args (fs_params Sf) ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    venv_ok H g (cA Phi) G ge ->
    HL.heap_ok (PsiH H) st ->
    xsafe H g Phi G R L (app_subst (cS Phi) (app_subst th (fs_ret Sf)))
          (xinterp (S k) P d ge st (XCallD f th0 sm args)).
Proof.
  intros Legacy R L Phi G f Df Sf body th th0 sm args d ge g st H
         Hf Hb Hsat Hth Hsm Hth0 Htheta Hargs Hcg Hd Hv Hh.
  assert(HdLegacy:XEnv.xdenv_agree Sg Rp P Fb H g(cA Phi)d).
  { unfold provision_environment in Hd;rewrite Legacy in Hd;exact Hd. }
  assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov [Hsreg Hthis]]]].
  assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau) = app_subst g tau)
    by (exact (@absorbs_all g (cS Phi) Hab)).
  destruct (wf_fn_decl WF _ Hf) as [PhiD [Wt [Wp [Wr _]]]].
  assert (Hread : forall tau, incl (ty_frees tau) (tyreqs (cA PhiD)) ->
            app_subst (callee_grounding g Phi th (cA PhiD)) tau
            = app_subst g (app_subst (cS Phi) (app_subst th tau))).
  { intros tau0 Hi. apply callee_grounding_reads. exact Hi. }
  assert (Hfp : forall tau, In tau (fs_params Sf) -> incl (ty_frees tau) (tyreqs (cA PhiD))).
  { intros tau0 Hin. rewrite Forall_forall in Wp. exact (wf_ty_frees (Wp tau0 Hin)). }
  assert (Hfr : incl (ty_frees (fs_ret Sf)) (tyreqs (cA PhiD))) by exact (wf_ty_frees Wr).
  pose proof (@xsafe_args_at R L Phi G d g args
                (fun tau => app_subst (cS Phi) (app_subst th tau)) (fs_params Sf) ge st H
                Hargs Hcg Hd Hv Hh) as Hsa.
  simpl. rewrite Hb.
  destruct (xinterp_list k P d ge st args) as [[[ws g1] st1] | rr]; [| apply xsafe_of_ctl; exact Hsa].
  destruct Hsa as [H1 [Hx1 [Hh1 [Hws Hv1]]]].
  destruct (xcallee_env_agree WF Wt Hsat Hth Hsm Hcg (xdenv_agree_mono Hx1 HdLegacy))
    as [d' [Hcomp [Hcgc Hdc]]].
  rewrite Hcomp.
  set (cg := callee_grounding g Phi th (cA PhiD)) in *.
  assert (Hws' : Forall2 (fun w tau => xvt H1 w (app_subst cg tau)) ws (fs_params Sf)).
  { refine (@F2_conv H1 ws (fs_params Sf) _ _ _ Hws). intros tau Hin.
    rewrite (Hread tau (Hfp tau Hin)), !Hcol. reflexivity. }
  assert (Hactual : xdenv_agree H1 cg (cA PhiD) (xcall_env P d th0 d')).
  { unfold provision_environment;rewrite Legacy.
    apply strong_agree; [apply xdenv_strong_call; exact Hdc|].
    unfold cg. eapply runtime_types_call; [exact (proj2 HdLegacy)|exact Hcg|exact Wt|
      exact Hsat|exact Hth|exact Hth0|exact Htheta|].
    intro t; apply typefree_lookup; eapply smap_composed_typefree; eauto. }
  destruct Hcgc as [Hgr' [Hab' Hrest']].
  assert (Hvb : venv_ok H1 cg (cA PhiD) (body_env Sf) (mk_xvenv ws)).
  { apply venv_body'; [exact Hws'|]. rewrite Forall_forall. exact Hfp. }
  pose proof (@IHfuel k (le_n k) _ _ _ _ _ _ (xcall_env P d th0 d') (mk_xvenv ws) cg st1 H1
                (@body_typed f Df Sf body PhiD Hf Hb Wt) (conj Hgr' (conj Hab' Hrest'))
                Hactual Hvb Hh1) as Hres.
  apply (xsafe_ext Hx1).
  eapply ret_call_safe_h; [exact Hres | exact Hab' | | exact Hv1].
  rewrite (Hread _ Hfr), !Hcol. reflexivity.
Qed.

Lemma case_call_r : forall R L Phi G f D S body PhiD slot sm args d ge g st H,
 xp_immutable P=true ->g_fn Sg f=Some(D,S,None) ->xp_fn P f=Some body ->
 tele_ctx D=Some PhiD ->reified_scope_closed Sg P(cA PhiD) ->
 callable_map_contract Sg P Phi PhiD sm slot S ->
 Forall2(fun e tau=>xhas R L Phi G e(app_subst(cS Phi)tau))args(fs_params slot) ->
 ctx_grounded g Phi ->xdenv_agree H g(cA Phi)d ->
 venv_ok H g(cA Phi)G ge ->HL.heap_ok(PsiH H)st ->
 xsafe H g Phi G R L(app_subst(cS Phi)(fs_ret slot))
   (xinterp(Datatypes.S k)P d ge st(XCallD f [] sm args)).
Proof.
 intros R L Phi G f D S body PhiD slot sm args d ge g st H
 Mode Fn Code Tele Closed Contract Args Ground Env Vars Heap.
 pose proof(@absorbs_all g(cS Phi)(proj1(proj2 Ground)))as Col.
 destruct(wf_fn_decl WF _ Fn)as[PhiD0[Wt[Params[Ret Rest]]]].
 pose proof(wf_tele_ctx Wt)as Tele0;rewrite Tele in Tele0;injection Tele0 as <-.
 pose proof(@xsafe_args_at R L Phi G d g args(app_subst(cS Phi))(fs_params slot)
 ge st H Args Ground Env Vars Heap)as ArgumentsSafe.
 cbn[xinterp];rewrite Code.
 destruct(xinterp_list k P d ge st args)as[[[ws ge1]st1]|rr]eqn:Run;
 [|apply xsafe_of_ctl;exact ArgumentsSafe].
 destruct ArgumentsSafe as[H1[Ext[Heap1[Values Vars1]]]].
 pose proof(shared_mono Ext Env)as Env1.
 unfold provision_environment in Env1;rewrite Mode in Env1;destruct Env1 as[Env1 Scope].
 destruct(@reified_direct_call_environment Sg Rp P Fb H1 Phi PhiD g d sm slot S
 Mode Ground Env1 Contract)as[gc[dc[Compose[GroundC[Signature EnvC]]]]].
 rewrite Compose.
 unfold grounded_call_signature in Signature;injection Signature as Ps Rt.
 assert(ValuesC:Forall2(fun w tau=>xvt H1 w(app_subst gc tau))ws(fs_params S)).
 { apply F2_map_r;rewrite Ps.
   apply (@F2_map_l _ _ (xvt H1)(app_subst g)).
   refine(@F2_impl _ _ _ _ _ _ _ Values).
   intros w tau Value;rewrite !Col in Value;exact Value. }
 assert(VarsC:venv_ok H1 gc(cA PhiD)(body_env S)(mk_xvenv ws)).
 { apply venv_body';[exact ValuesC|].
   eapply Forall_impl;[|exact Params];intros tau T;exact(wf_ty_frees T). }
 assert(BodyEnv:xdenv_agree H1 gc(cA PhiD)(xcall_env P d [] dc)).
 { unfold provision_environment;rewrite Mode;auto. }
 pose proof(@IHfuel k(le_n k)_ _ _ _ _ _(xcall_env P d [] dc)(mk_xvenv ws)gc st1 H1
 (@body_typed f D S body PhiD Fn Code Wt)GroundC BodyEnv VarsC Heap1)as SafeBody.
 apply(xsafe_ext Ext);eapply ret_call_safe_h;
 [exact SafeBody|exact(proj1(proj2 GroundC))| |exact Vars1].
 rewrite Rt,!Col;reflexivity.
Qed.

Lemma case_meth : forall R L Phi G e0 tau0 m Dm Sf rt0 th0 args d ge g st H,
    g_mth Sg m = Some (Dm, Sf) ->
    xhas R L Phi G e0 tau0 ->
    In (IMth rt0 m th0) (cA Phi) ->
    app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
    Forall2 (fun e tau =>
               xhas R L Phi G e (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params Sf) ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    venv_ok H g (cA Phi) G ge ->
    HL.heap_ok (PsiH H) st ->
    xsafe H g Phi G R L (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret Sf))
          (xinterp (S k) P d ge st (XMeth e0 m (IMth rt0 m th0) args)).
Proof.
  intros R L Phi G e0 tau0 m Dm Sf rt0 th0 args d ge g st H
         Hm Hty0 Hin Hrec Hargs Hcg Hd Hv Hh.
  assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov Hsig]]].
  assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau) = app_subst g tau)
    by (exact (@absorbs_all g (cS Phi) Hab)).
  assert (HR0 : app_subst g rt0 = app_subst g (app_subst (cS Phi) tau0)).
  { rewrite <- (Hcol rt0). rewrite Hrec. reflexivity. }
  assert (Hcv : forall tau,
             app_subst g (app_subst (cS Phi)
               (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
             = app_subst g (app_subst (this_subst rt0) (app_subst th0 tau))).
  { intros tau. unfold interp_m. rewrite Hcol, Hcol.
    apply this_read. symmetry. exact HR0. }
  destruct(@provision_lookup Sg Rp P Fb H g(cA Phi)d(IMth rt0 m th0)Hd eq_refl Hin)
    as[en[Hen Hag0]].
  pose proof (@IHfuel k (le_n k) R L Phi G e0 tau0 d ge g st H Hty0 Hcg Hd Hv Hh) as Hrcv.
  (* the arguments, read as the entry reads them *)
  assert (Hargs_at : forall H0 g0 st0, sext H H0 -> venv_ok H0 g (cA Phi) G g0 ->
    HL.heap_ok (PsiH H0) st0 ->
    match xinterp_list k P d g0 st0 args with
    | inl (ws, g1, st1) =>
        exists H1, sext H0 H1 /\ HL.heap_ok (PsiH H1) st1
        /\ Forall2 (fun w tau => xvt H1 w (app_subst g (app_subst (this_subst rt0) (app_subst th0 tau))))
                ws (fs_params Sf)
        /\ venv_ok H1 g (cA Phi) G g1
    | inr r => xsafe_ctl H0 g Phi G R L r
    end).
  { intros H0 g0 st0 Hx0 Hv0 Hh0.
    pose proof (@xsafe_args_at R L Phi G d g args
                  (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0)) (fs_params Sf) g0 st0 H0
                  Hargs Hcg (shared_mono Hx0 Hd) Hv0 Hh0) as Hsa.
    destruct (xinterp_list k P d g0 st0 args) as [[[ws g1] st1] | rr]; [| exact Hsa].
    destruct Hsa as [H1 [Hx1 [Hh1 [Hws Hv1]]]]. exists H1. split; [exact Hx1 |].
    split; [exact Hh1 |]. split; [| exact Hv1].
    refine (@F2_conv H1 ws (fs_params Sf) _ _ _ Hws). intros tau _. apply Hcv. }
  simpl. rewrite Hen.
  destruct en as[wv|f' dc|hh ss dc|p|hs|ty|p held];
    try solve[unfold provision_entry in Hag0;destruct(xp_immutable P);
      cbn[reified_entry xdentry_agree]in Hag0;contradiction].
  - (* a provider closure *)
    destruct(@provision_method_code Sg Rp P Fb H g rt0 m th0 Dm Sf f' dc Hm Hag0)
      as[D'[S'[body[Ef' Hb]]]].
    rewrite Hb.
    destruct (xinterp k P d ge st e0) as [o0 g0 st0] eqn:E0.
    destruct o0 as [w0 | | | | | | |];
      try (apply (@xsafe_pass H g Phi G R L tau0); [exact Hrcv | intros w Hw; discriminate]).
    destruct Hrcv as [H0 [Hx0 [Hh0 [Hw0 Hv0]]]]. simpl in Hv0, Hh0.
    specialize (Hargs_at H0 g0 st0 Hx0 Hv0 Hh0).
    destruct (xinterp_list k P d g0 st0 args) as [[[ws g1] st1] | rr];
      [| apply xsafe_of_ctl; exact (xsafe_ctl_ext Hx0 Hargs_at)].
    destruct Hargs_at as [H1 [Hx1 [Hh1 [Hws Hv1]]]].
    pose proof(@provision_entry_mono Sg Rp P Fb H H1(sext_trans Hx0 Hx1)
      g(IMth rt0 m th0)(XDClo f' dc)Hag0)as Hag1.
    destruct(@provision_method_closure Sg Rp P Fb H1 g rt0 m th0 f' dc Dm Sf D' S' body
      Hm Ef' Hb Hag1)
      as[PhiD'[gc[Htc[Hgrc[Hcovc[Habc[Hpar[Hret Hdb]]]]]]]].
    destruct(wf_fn_decl WF _ Ef')as[PhiD[Wt[Wp[Wr Rest]]]].
    pose proof(wf_tele_ctx Wt)as TD;rewrite Htc in TD;injection TD as <-.
    pose proof(@provision_capture_grounded Sg D' PhiD' gc Wt Hgrc Hcovc Habc)as Hcgb.
    remember (fs_params S') as psl eqn:Eps.
    destruct psl as [| p0 rest]; [ simpl in Hpar; discriminate |].
    simpl in Hpar. injection Hpar as Hp0 Hprest.
    assert (Hws' : Forall2 (fun w tau => xvt H1 w (app_subst gc tau)) (w0 :: ws) (p0 :: rest)).
    { constructor.
      - rewrite Hp0, HR0. exact (xvt_mono Hx1 Hw0).
      - assert (H2 : Forall2 (xvt H1) ws (map (app_subst gc) rest)).
        { rewrite Hprest.
          apply (@F2_map_l _ _ (xvt H1) (fun tau => app_subst g (app_subst (this_subst rt0) (app_subst th0 tau)))).
          exact Hws. }
        exact (@F2_map_r _ _ (xvt H1) (app_subst gc) _ _ H2). }
    assert (Hvb : venv_ok H1 gc (cA PhiD') (body_env S') (mk_xvenv (w0 :: ws))).
    { apply venv_body'; [rewrite <- Eps; exact Hws'|]. rewrite <- Eps.
      eapply Forall_impl; [| exact Wp]. intros tau Hwf. exact (wf_ty_frees Hwf). }
    pose proof (@IHfuel k (le_n k) _ _ _ _ _ _ dc (mk_xvenv (w0 :: ws)) gc st1 H1
                  (@body_typed f' D' S' body PhiD' Ef' Hb Wt) Hcgb Hdb Hvb Hh1) as Hres.
    apply (xsafe_ext (sext_trans Hx0 Hx1)).
    eapply ret_call_safe_h; [exact Hres | exact Habc | | exact Hv1].
    rewrite Hret, Hcv. reflexivity.
  - (* a primitive provision *)
    pose proof(@provision_method_primitive Sg Rp P Fb H g rt0 m th0 Dm Sf p Hm Hag0)as Hag.
    destruct (xinterp k P d ge st e0) as [o0 g0 st0] eqn:E0.
    destruct o0 as [w0 | | | | | | |];
      try (apply (@xsafe_pass H g Phi G R L tau0); [exact Hrcv | intros w Hw; discriminate]).
    destruct Hrcv as [H0 [Hx0 [Hh0 [Hw0 Hv0]]]]. simpl in Hv0, Hh0.
    specialize (Hargs_at H0 g0 st0 Hx0 Hv0 Hh0).
    destruct (xinterp_list k P d g0 st0 args) as [[[ws g1] st1] | rr];
      [| apply xsafe_of_ctl; exact (xsafe_ctl_ext Hx0 Hargs_at)].
    destruct Hargs_at as [H1 [Hx1 [Hh1 [Hws Hv1]]]].
    assert (Hws' : Forall2 (xvt H1) (w0 :: ws)
                     (app_subst g rt0
                      :: map (fun tau => app_subst g (app_subst (this_subst rt0) (app_subst th0 tau)))
                             (fs_params Sf))).
    { constructor; [rewrite HR0; exact (xvt_mono Hx1 Hw0) |].
      apply (@F2_map_l _ _ (xvt H1) (fun tau => app_subst g (app_subst (this_subst rt0) (app_subst th0 tau)))).
      exact Hws. }
    pose proof (@prim_run_h H1 p _ _ (w0 :: ws) st1 Hag Hws' Hh1) as Hrun.
    apply (xsafe_ext (sext_trans Hx0 Hx1)).
    destruct (run_prim_at P p None (w0 :: ws) st1) as [o st2].
    eapply prim_result; [exact Hrun | rewrite Hcv; reflexivity | exact Hv1].
  - (* a primitive provision with an allocation type header *)
    pose proof(@provision_method_primitive_at Sg Rp P Fb H g rt0 m th0 Dm Sf p held Hm Hag0)as Hag.
    destruct (xinterp k P d ge st e0) as [o0 g0 st0] eqn:E0.
    destruct o0 as [w0 | | | | | | |];
      try (apply (@xsafe_pass H g Phi G R L tau0); [exact Hrcv | intros w Hw; discriminate]).
    destruct Hrcv as [H0 [Hx0 [Hh0 [Hw0 Hv0]]]]. simpl in Hv0, Hh0.
    specialize (Hargs_at H0 g0 st0 Hx0 Hv0 Hh0).
    destruct (xinterp_list k P d g0 st0 args) as [[[ws g1] st1] | rr];
      [| apply xsafe_of_ctl; exact (xsafe_ctl_ext Hx0 Hargs_at)].
    destruct Hargs_at as [H1 [Hx1 [Hh1 [Hws Hv1]]]].
    assert (Hws' : Forall2 (xvt H1) (w0 :: ws)
                     (app_subst g rt0
                      :: map (fun tau => app_subst g (app_subst (this_subst rt0) (app_subst th0 tau)))
                             (fs_params Sf))).
    { constructor; [rewrite HR0; exact (xvt_mono Hx1 Hw0) |].
      apply (@F2_map_l _ _ (xvt H1) (fun tau => app_subst g (app_subst (this_subst rt0) (app_subst th0 tau)))).
      exact Hws. }
    pose proof (@prim_run_at_h H1 p held _ _ (w0 :: ws) st1 Hag Hws' Hh1) as Hrun.
    apply (xsafe_ext (sext_trans Hx0 Hx1)).
    destruct (run_prim_at P p (Some held) (w0 :: ws) st1) as [o st2].
    eapply prim_result; [exact Hrun | rewrite Hcv; reflexivity | exact Hv1].
Qed.

Lemma colg : forall g Phi, ctx_grounded g Phi ->
  forall t, app_subst g (app_subst (cS Phi) t) = app_subst g t.
Proof. intros g Phi [_ [Hab _]]. exact (absorbs_all Hab). Qed.

Lemma venv_meanings_reground : forall H g g' A A' G ge,
 venv_ok H g A G ge ->incl(tyreqs A)(tyreqs A') ->meanings_on A g' g ->
 venv_ok H g' A' G ge.
Proof.
 intros H g g' A A' G ge[Values Scope]Incl Mean;split.
 - clear Incl;induction Values as[|p q G ge[Key Value]Tail IH];constructor;
   inversion Scope as[|? ? Head Rest];subst.
   + split;[exact Key|]. rewrite(@meanings_on_type A g' g(snd p)Mean Head);exact Value.
   + apply IH;exact Rest.
 - eapply Forall_impl;[|exact Scope];intros p Free t Hin;apply Incl,Free;exact Hin.
Qed.

Lemma shared_bindty_world : forall Hs R L Phi G t tau' tau d ge g,
    t <> this_sym ->
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    wf_ty Sg Phi tau ->
    ret_wf Sg Phi R ->
    ctx_grounded g Phi ->
    xdenv_agree Hs g (cA Phi) d ->
    venv_ok Hs g (cA Phi) G ge ->
    ctx_grounded ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (bind_ty t tau' Phi)
    /\ xdenv_agree Hs ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (cA (bind_ty t tau' Phi)) (xbind_ty P d t tau')
    /\ venv_ok Hs ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (cA (bind_ty t tau' Phi)) G ge
    /\ forall r, xsafe0 Hs ((t, app_subst g (app_subst (cS Phi) tau')) :: g) (bind_ty t tau' Phi) G R L tau r
                -> xsafe0 Hs g Phi G R L tau r.
Proof.
  intros Hs R L Phi G t tau' tau d ge g Hneq Hfresh Htau' Htau HR Hg Hd Hv.
  destruct(@provision_bind_type Sg Rp P Fb Hs Phi t tau' d g WF Hneq Hfresh Htau' Hg Hd)as[Hg2 Hd2].
  destruct(@reified_bindty_ground Sg Phi t tau' g Hneq Hfresh Htau' Hg)as[_ Hlook].
  set (g2 := (t, app_subst g (app_subst (cS Phi) tau')) :: g) in *.
  assert (Hnot : ~ In t (tyreqs (cA Phi))).
  { intro H. apply Hfresh. apply safety_case_bindty_aux1. exact H. }
  assert (Hran : Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA Phi))) (cS Phi)).
  { destruct Hg as [_ [_ [_ [[_ Hran] _]]]]. exact Hran. }
  assert (Hincl : incl (tyreqs (cA Phi)) (tyreqs (cA (bind_ty t tau' Phi)))).
  { intros u Hu. simpl. right. exact Hu. }
  (* a type well-formed outside reads the same inside *)
  assert (Hsame : forall a, incl (ty_frees a) (tyreqs (cA Phi)) ->
            app_subst g2 a = app_subst g a).
  { intros a Ha. apply app_subst_frees_agree. intros u Hu. apply Hlook. exact (Ha u Hu). }
  split; [exact Hg2|]. split; [exact Hd2|]. split.
  - eapply venv_reground; [exact Hv | exact Hincl | exact Hlook].
  - intros r Hr. eapply xsafe0_back; [exact Hr | | |].
    + assert (Hfree : incl (ty_frees tau) (tyreqs (cA Phi))) by exact (wf_ty_frees Htau).
      rewrite (app_subst_frees_agree (cS (bind_ty t tau' Phi)) (cS Phi) tau).
      * apply Hsame. apply safety_case_bindty_aux3; assumption.
      * intros u Hu. simpl. destruct (Nat.eqb u t) eqn:E; [|reflexivity].
        apply Nat.eqb_eq in E. subst. exfalso. apply Hnot. exact (Hfree t Hu).
    + intros rt E. subst R. apply Hsame. exact (wf_ty_frees HR).
    + intros ge0 [H1 _]. split; [| exact (proj2 Hv)].
      pose proof (proj2 Hv) as HGreg. clear - H1 HGreg Hsame.
      induction H1 as [|p q G0 ge1 [Hk Hw] Hr0 IH]; constructor;
        inversion HGreg as [|? ? Hp Hps]; subst.
      * split; [exact Hk|]. rewrite <- (Hsame _ Hp). exact Hw.
      * apply IH. exact Hps.
Qed.

Ltac pass H := eapply xsafe_pass; [exact H | intros ? ?; discriminate].



(* The worlds the primitive binds leave. *)
Lemma alloc_prim_reify : forall p taus ret,
  prim_ok Sg Rp P p taus ret ->
  (match p with PBox _ | PArrNew _ => True | _ => False end) -> xreify P = true.
Proof.
  intros p taus ret Hp Alloc. destruct p; try contradiction.
  all: destruct Hp as [[code Bad]|[[ps [r [Bad Rest]]]|Shape]];
    try discriminate; try (destruct Bad; discriminate).
  - destruct taus as [|c ts]; simpl in Shape; try contradiction.
    destruct ts; simpl in Shape; try contradiction.
    destruct Shape as [th [x [Er [Hr [Hc Hschema]]]]].
    destruct Hschema as [spec [Hl Hrest]]. unfold xreify.
    destruct (xp_cast P); [discriminate Hl|apply orb_true_r].
  - destruct taus as [|c ts]; simpl in Shape; try contradiction.
    destruct ts as [|i ts]; simpl in Shape; try contradiction.
    destruct ts; simpl in Shape; try contradiction.
    destruct Shape as [Hi [th [x [Er [Hr [Hc Hschema]]]]]].
    destruct Hschema as [spec [Hl Hrest]]. unfold xreify.
    destruct (xp_cast P); [discriminate Hl|apply orb_true_r].
Qed.

Lemma world_bindprim : forall H Phi f Df S p d g,
    g_fn Sg f = Some (Df, S, None) ->
    sat0 Sg Phi Df ->
    prim_ok Sg Rp P p (map (app_subst (cS Phi)) (fs_params S)) (app_subst (cS Phi) (fs_ret S)) ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    xdenv_agree H g (cA (add_item (IFn f) Phi)) ((IFn f, xprim_entry d (IFn f) p) :: d).
Proof.
  intros H Phi f Df S p d g Hf Hsf Hok Hcg Hd.
  assert (Ep : xprim_entry d (IFn f) p = XDPrim p) by (destruct p; reflexivity). rewrite Ep.
  pose proof (colg Hcg) as Hcol.
  assert (Hen : xdentry_agree H g (IFn f) (XDPrim p)).
  { simpl. rewrite Hf.
    assert (E : map (app_subst g) (fs_params S)
                = map (app_subst g) (map (app_subst (cS Phi)) (fs_params S))).
    { rewrite map_map. apply map_ext. intros a. symmetry. apply Hcol. }
    rewrite E, <- Hcol. apply prim_ok_subst. exact Hok. }
  assert (Hdep : incl (dep_tys Sg (IFn f) ++ item_ty_frees (IFn f) ++ method_item_frees Sg (IFn f))
                      (tyreqs (cA Phi))).
  { destruct Hsf as [PhiDf [Htcf [_ Havf]]].
    intros u Hu. simpl in Hu. rewrite Hf in Hu. simpl in Hu.
    apply in_app_or in Hu. destruct Hu as [Hu | Hu]; [| simpl in Hu; contradiction].
    apply wf_ty_frees_aux3. apply avail_ty_in.
    rewrite Forall_forall in Havf. apply Havf.
    apply (proj2 (tele_ctx_tyreqs Df Htcf u)). exact Hu. }
  eapply(@provision_push_non_type Sg Rp P Fb H g(cA Phi)d(IFn f)(XDPrim p));
    [exact Hd|reflexivity| |exact Hdep].
  apply(proj2(@provision_primitive_agreement Sg Rp P Fb H g(IFn f)p));exact Hen.
Qed.

Lemma world_bindprimm : forall H Phi tauk m Dm S th p d g,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    msat th Dm S ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    prim_ok Sg Rp P p (app_subst (cS Phi) tauk
               :: map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S))
            (interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S)) ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    xdenv_agree H g (cA (add_item (IMth tauk m th) Phi)) ((IMth tauk m th, xprim_entry d (IMth tauk m th) p) :: d).
Proof.
  intros H Phi tauk m Dm S th p d g Hm Hwk Hsat Hthwf Hok Hcg Hd.
  pose proof (colg Hcg) as Hcol.
  assert (Hrk : app_subst g (app_subst (cS Phi) tauk) = app_subst g tauk) by apply Hcol.
  assert (Hcv : forall a, app_subst g (interp_m (cS Phi) th (app_subst (cS Phi) tauk) a)
                = app_subst g (app_subst (this_subst tauk) (app_subst th a))).
  { intros a. unfold interp_m. rewrite Hcol. apply this_read. exact Hrk. }
  assert (Hen : xdentry_agree H g (IMth tauk m th) (XDPrim p)).
  { simpl. rewrite Hm.
    assert (E : app_subst g tauk
                :: map (fun a => app_subst g (app_subst (this_subst tauk) (app_subst th a))) (fs_params S)
                = map (app_subst g) (app_subst (cS Phi) tauk
                    :: map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S))).
    { simpl. rewrite Hrk. f_equal. rewrite map_map. apply map_ext. intros a. symmetry. apply Hcv. }
    rewrite E, <- Hcv. apply prim_ok_subst. exact Hok. }
  assert (Hok' : item_ok Sg Phi (IMth tauk m th)) by (eapply OkMth; eassumption).
  assert (Hdep : incl (dep_tys Sg (IMth tauk m th) ++ item_ty_frees (IMth tauk m th)
                       ++ method_item_frees Sg (IMth tauk m th)) (tyreqs (cA Phi))).
  { intros u Hu. simpl in Hu. apply in_app_or in Hu. destruct Hu as [Hu | Hu].
    - exact (@tele_items_regular_aux1 Sg Phi _ Hok' u Hu).
    - exact (@tele_items_regular_aux2 Sg Phi _ WF Hok' u Hu). }
  assert (HenActual : xdentry_agree H g (IMth tauk m th) (xprim_entry d (IMth tauk m th) p)).
  { assert (Ground : prim_ok Sg Rp P p
        (app_subst g tauk :: map (fun a => app_subst g (app_subst (this_subst tauk) (app_subst th a))) (fs_params S))
        (app_subst g (app_subst (this_subst tauk) (app_subst th (fs_ret S))))).
    { cbn [xdentry_agree] in Hen. rewrite Hm in Hen. exact Hen. }
    destruct p; cbn [xprim_entry]; try exact Hen.
    all: cbn [xdentry_agree]; rewrite Hm; split; [exact Ground|].
    all: change (xground d tauk = app_subst g tauk).
    all: eapply types_match_reads.
    all: try exact (wf_ty_frees Hwk).
    all: try exact (proj1 (proj2 (proj2 Hcg))).
    all: apply (@provision_types Sg Rp P Fb H g(cA Phi)d Hd); eapply alloc_prim_reify; [exact Ground|exact I]. }
  eapply(@provision_push_non_type Sg Rp P Fb H g(cA Phi)d(IMth tauk m th)
    (xprim_entry d(IMth tauk m th)p));[exact Hd|reflexivity| |exact Hdep].
  destruct p;cbn[xprim_entry]in HenActual|-*;
    first[apply(proj2(@provision_primitive_agreement Sg Rp P Fb H g _ _));exact HenActual|
          apply(proj2(@provision_header_primitive_agreement Sg Rp P Fb H g _ _ _));exact HenActual].
Qed.

(* ---- What a body's run, and an application, promise. ---- *)

(* A run that stops (trap, exit, unmodelled primitive, fuel), with the
   machine it stops with agreeing with some extension of H. *)
Definition ok_ctl (H : sty) (r : xres) : Prop :=
  match xr_out r with
  | XTrap _ | XExit _ | XUns _ | XFuel =>
      exists H', sext H H' /\ HL.heap_ok (PsiH H') (xr_st r)
  | _ => False
  end.

Definition binds_post (H : sty) (Phi Phi' : ctx) (g : subst)
    (r : (xdenv * xvenv * mstate) + xres) : Prop :=
  match r with
  | inl (d', ge', st') => ge' = [] /\ exists H' g', sext H H' /\ HL.heap_ok (PsiH H') st'
              /\ ctx_grounded g' Phi'
              /\ xdenv_agree H' g' (cA Phi') d'
              /\ meanings_on(cA Phi)g' g
  | inr rr => ok_ctl H rr
  end.

Definition apply_post (H : sty) (Phi : ctx) (Dr : tele) (g : subst) (d : xdenv)
    (r : (xdenv * mstate) + xres) : Prop :=
  match r with
  | inl (dr, st') => exists H' g', sext H H' /\ HL.heap_ok (PsiH H') st'
              /\ ctx_grounded g' (open_res Dr Phi)
              /\ xdenv_agree H' g' (cA (open_res Dr Phi)) (dr ++ d)
              /\ (forall x, ~ In x (tyreqs Dr) -> lookup_subst x g' = lookup_subst x g)
  | inr rr => ok_ctl H rr
  end.

Definition apply_r_post(H:sty)(Phi:ctx)(Dr:tele)(g:subst)(d:xdenv)
 (r:(xdenv*mstate)+xres):Prop:=
 match r with
 | inl(dr,st')=>exists H' g',sext H H' /\HL.heap_ok(PsiH H')st' /\
 ctx_grounded g'(open_res Dr Phi) /\xdenv_agree H' g'(cA(open_res Dr Phi))(dr++d) /\
 meanings_on(cA Phi)g' g
 | inr rr=>ok_ctl H rr
 end.

Lemma ok_ctl_ext : forall H H1 r, sext H H1 -> ok_ctl H1 r -> ok_ctl H r.
Proof.
  intros H H1 [o gr st] Hx R0. unfold ok_ctl in *. simpl in *.
  destruct o; simpl in R0 |- *; try contradiction;
    destruct R0 as [H' [Hx' Hh]]; exists H'; exact (conj (sext_trans Hx Hx') Hh).
Qed.

Lemma binds_post_step : forall H H1 Phi Phi1 Phi' g g1 r,
    binds_post H1 Phi1 Phi' g1 r -> sext H H1 ->
    incl (tyreqs (cA Phi)) (tyreqs (cA Phi1)) ->
    meanings_on(cA Phi)g1 g ->
    binds_post H Phi Phi' g r.
Proof.
  intros H H1 Phi Phi1 Phi' g g1 r R0 Hx Hincl Hl.
  destruct r as [[[d' ge'] st'] | rr]; simpl in *; [| exact (@ok_ctl_ext H H1 rr Hx R0)].
  destruct R0 as [E [H' [g' [Hx' [Hh [H2 [H3 H4]]]]]]]. split; [exact E|]. exists H', g'.
  split; [exact (sext_trans Hx Hx') |]. split; [exact Hh |].
  split; [exact H2 |]. split; [exact H3 |].
  intros x Hx0. rewrite (H4 x (Hincl x Hx0)). apply Hl. exact Hx0.
Qed.

(* ---- Abstract functors: an application, and a body's run. ---- *)

Lemma xapply_r_step : forall j,
 (forall Phi bs Phi',xbinds_ty Sg Rp P Fb Phi bs Phi' ->forall d g st H,
 ctx_grounded g Phi ->xdenv_agree H g(cA Phi)d ->HL.heap_ok(PsiH H)st ->
 binds_post H Phi Phi' g(xinterp_binds j P d [] st bs)) ->
 xp_immutable P=true ->forall H Phi g d st u Du Dr results inputs outputs,
 g_fun Sg u=Some(Du,Dr,None)->In(IFun u)(cA Phi)->
 ctx_grounded g Phi ->xdenv_agree H g(cA Phi)d ->
 application_input_contract Sg P Phi u Du inputs ->
 result_map_contract Sg P Phi u Du Dr results outputs ->
 reified_scope_closed Sg P(cA(open_res results Phi))->HL.heap_ok(PsiH H)st ->
 apply_r_post H Phi results g d(xinterp_apply_r(S j)P d u inputs outputs st).
Proof.
 intros j IHb Mode H Phi g d st u Du Dr results inputs outputs Decl Hin Ground Env Inputs Outputs ClosedResult Heap.
 destruct(@provision_lookup Sg Rp P Fb H g (cA Phi) d (IFun u) Env eq_refl Hin)as[entry[Lookup Entry]].
 unfold provision_entry in Entry;rewrite Mode in Entry.
 destruct entry as[w|f dc|h sms dc|p|sym|ty|p ty];
 try solve[cbn[reified_entry]in Entry;contradiction].
 (* Keep the original closure relation intact while exposing its shape. *)
 destruct(Inputs g Ground)as[gin[Input InputMap]].
 unfold provision_environment in Env;rewrite Mode in Env.
 destruct Env as[Env Closed].
 destruct(@reified_map_composes Sg Rp P Fb H gin g Du (cA Phi) inputs d Env InputMap)as[din[Compose[Keys[InEnv All]]]].
 destruct(@reified_application_body_ready Sg Rp P Fb H g u h sms dc Du Dr gin din
 Mode WF Decl Entry Input InEnv Keys((@xwf_reified_fun_inputs Sg Rp P Fb WFX Mode h)))
 as[Dh[Dc[pins[code[PhiD[PhiC[gb[dh[FB[Code[TD[TC[GB[Run[BodyEnv[Hdecl Back]]]]]]]]]]]]]]]].
 destruct code as[bodyresults bs].
 destruct(@xwf_fb_r Sg Rp P Fb WFX Mode h Dh Dc pins Dr bodyresults bs FB Hdecl Code)
 as[PD[PC[PhiEnd[WD[WC[Pins[ClosedBody[Typed Exports]]]]]]]].
 pose proof(wf_tele_ctx WD)as TD0;rewrite TD in TD0;injection TD0 as <-.
 pose proof(wf_tele_ctx WC)as TC0;rewrite TC in TC0;injection TC0 as <-.
 assert(EnvBody:xdenv_agree H gb(cA(fun_ctx PhiD PhiC pins))(dh++dc)).
 { unfold provision_environment;rewrite Mode;auto. }
 pose proof(IHb _ _ _ Typed(dh++dc)gb st H GB EnvBody Heap)as BodySafe.
 unfold apply_r_post.
 cbn[xinterp_apply_r];rewrite Lookup,Code,Compose,Run.
 destruct(xinterp_binds j P(dh++dc)[]st bs)as[[[bodyenv ge]st']|rr]eqn:Eval.
 - cbn in BodySafe;destruct BodySafe as[Ge[H'[bodyout[Ext[Heap'[GroundOut[EnvOut Mean]]]]]]].
   unfold provision_environment in EnvOut;rewrite Mode in EnvOut.
   destruct EnvOut as[EnvOut ClosedOut].
   destruct(@reified_body_exports Sg Rp P Fb H' _ _ Dh Dr _ gb bodyout bodyenv Exports GB GroundOut Mean EnvOut)
   as[canonical[returned[Schema[Export[ReturnKeys ReturnEnv]]]]].
   destruct(Back canonical Schema)as[gout[Result Trail]].
   pose proof(@reified_result_transport Sg Rp P Fb H' gout canonical Dr Trail returned ReturnEnv)as Returned.
   assert(OldEnv:reified_environment Sg Rp P Fb H' g(cA Phi)d).
   { eapply (@reified_environment_mono Sg Rp P Fb H H');eassumption. }
   destruct(@reified_application_results Sg Rp P Fb H' Phi g d u Du Dr results outputs gin gout returned Mode WF Closed Ground OldEnv Outputs Input Result Returned)
   as[gcaller[dr[GroundCaller[ComposeOut[KeysOut[EnvCaller OldMean]]]]]].
   rewrite Export,ComposeOut;cbn.
   exists H',gcaller;split;[exact Ext|]. split;[exact Heap'|].
   split;[exact GroundCaller|]. split;[|exact OldMean].
   unfold provision_environment;rewrite Mode;split;[exact EnvCaller|].
   exact ClosedResult.
 - exact BodySafe.
Qed.

Lemma xapply_step : xp_immutable P=false ->forall j,
    (forall Phi bs Phi', xbinds_ty Sg Rp P Fb Phi bs Phi' ->
       forall d g st H, ctx_grounded g Phi -> xdenv_agree H g (cA Phi) d ->
       HL.heap_ok (PsiH H) st ->
       binds_post H Phi Phi' g (xinterp_binds j P d [] st bs)) ->
    forall Phi u Du Dr th sm,
    g_fun Sg u = Some (Du, Dr, None) ->
    avail Phi (IFun u) ->
    sat Sg Phi th Du ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Du ->
    fixed_ok Sg Phi u th ->
    res_coherent Sg Phi th Dr ->
    Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
    forall d g st Hstt, ctx_grounded g Phi -> xdenv_agree Hstt g (cA Phi) d ->
    HL.heap_ok (PsiH Hstt) st ->
    apply_post Hstt Phi (app_tele th Dr) g d (xinterp_apply (S j) P d u th sm st).
Proof.
  intros Legacy j IHb Phi u Du Dr th sm Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh d g st Hstt Hg Hd Hhp.
  unfold provision_environment in Hd;rewrite Legacy in Hd.
  simpl xinterp_apply.
  pose proof (proj1 Hd) as [Hcov0 [Hent0 _]].
  assert (Hin : In (IFun u) (cA Phi)) by (apply avail_fun_in; exact Hav).
  rewrite Forall_forall in Hcov0.
  destruct (Hcov0 _ Hin) as [Hbad | [en Hlk]]; [discriminate Hbad |].
  pose proof (Hent0 _ _ Hin eq_refl Hlk) as Hag.
  rewrite Hlk.
  destruct (agree_restrict Hd) as [dA [HdA HagA]].
  pose proof HdA as [Hcov [Hent [HAreg Hkeys]]].
  rewrite Forall_forall in Hent, Hcov.
  destruct en as [wv | fc dc0 | h sms dc | p | hs | ty | p held]; simpl in Hag; try contradiction.
  rewrite Hu in Hag.
  destruct (Fb h) as [[[Dh Dc] pins]|] eqn:Hh; try contradiction.
  destruct (xp_fun P h) as [[Drp bs]|] eqn:Ep; try contradiction.
  destruct Hag as [E1 [Hch [PhiC [gc [HtcC [Hgrc [Hcovc [Habsc [Hpinc
                   [Htcov [Hkeysc [HK [HtypesC Hallc]]]]]]]]]]]]].
  subst Drp.
  destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiU [HtcU [WU [WrU HregU]]]].
  destruct (@fb_regular Legacy h Dh Dc pins Dr bs Hh Ep)
    as [PhiH [PhiC' [HtcH [WH [WC [Hpok [WrF [[PhiB [Hbt Hprov]] HregF]]]]]]]].
  assert (EC : PhiC' = PhiC) by (rewrite (wf_tele_ctx WC) in HtcC; congruence).
  subst PhiC'.
  destruct (@xcallee_env_agree Sg Rp P Fb Hstt g Phi th sm Du d PhiU WF WU Hsat Hthw Hsm Hg Hd)
    as [d1 [Hc1 [Hg1 Hd1]]].
  rewrite Hc1.
  destruct (@xbchain_agree Hstt Dh sms u Hch Du Dr PhiU PhiH d1 _ Hu WU WH Hg1 (strong_values Hd1))
    as [d2 [g2 [Hrc [Hg2 [Hd2 [Hr2 [Hi2 Hfree2]]]]]]].
  rewrite Hrc.
  assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0) = app_subst g t0).
  { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
  assert (Hdomth : map fst th = tyreqs (cA PhiU)).
  { destruct Hsat as [PhiD' [Htc' [Hdom _]]]. rewrite HtcU in Htc'.
    injection Htc' as <-. exact Hdom. }
  assert (HgPhi : forall x, In x (tyreqs (cA PhiH)) ->
            app_subst g2 (TAbs x) = app_subst g (app_subst (cS Phi) (app_subst th (TAbs x)))).
  { intros x Hx. rewrite (Hr2 x Hx). apply callee_grounding_reads.
    intros y [<- | []]. apply Hi2. exact Hx. }
  (* the fixed-input premise, at work: what h captured agrees with the
     application wherever the two meet *)
  assert (HfixK : forall x, In x (tyreqs (cA PhiC)) ->
            In x (tyreqs Dh ++ res_reads Sg Dr) ->
            app_subst gc (TAbs x) = app_subst g (app_subst (cS Phi) (app_subst th (TAbs x)))).
  { intros x Hx Hxr. destruct (HK x Hx Hxr) as [HxX Heq].
    transitivity (app_subst g (TAbs x)); [exact Heq |].
    unfold fixed_ok in Hfix. rewrite Forall_forall in Hfix.
    rewrite (Hfix x HxX). symmetry. apply Habs. }
  (* the body's grounding: linked arguments from the application, the rest
     from the capture *)
  set (gH := map (fun t => (t, app_subst g2 (TAbs t))) (tyreqs (cA PhiH)) ++ gc).
  assert (HgHH : forall x, In x (tyreqs (cA PhiH)) ->
            app_subst gH (TAbs x) = app_subst g2 (TAbs x)).
  { intros x Hx. unfold gH. rewrite app_subst_TAbs.
    rewrite (lookup_prefix_in (fun t => app_subst g2 (TAbs t)) _ x gc Hx). reflexivity. }
  assert (HgHC : forall x, In x (tyreqs (cA PhiC)) ->
            app_subst gH (TAbs x) = app_subst gc (TAbs x)).
  { intros x Hx. destruct (in_dec Nat.eq_dec x (tyreqs (cA PhiH))) as [HxH | HxH].
    - rewrite (HgHH x HxH), (HgPhi x HxH). symmetry. apply HfixK; [exact Hx |].
      apply in_or_app. left. apply (proj1 (tele_ctx_tyreqs _ (wf_tele_ctx WH) x)).
      exact HxH.
    - unfold gH. rewrite !app_subst_TAbs. rewrite (lookup_prefix_notin _ _ x gc HxH). reflexivity. }
  destruct Hg2 as [Hgr2 [Hab2 [Hcv2 [[Hsd2 Hsr2] Hth2]]]].
  assert (HgHcov : covers gH (tyreqs (cA PhiH ++ cA PhiC))).
  { intros x Hx. rewrite tyreqs_app in Hx.
    destruct (in_dec Nat.eq_dec x (tyreqs (cA PhiH))) as [HxH | HxH].
    - eexists. unfold gH. apply lookup_prefix_in. exact HxH.
    - unfold gH. rewrite (lookup_prefix_notin _ _ x gc HxH). apply Hcovc.
      apply in_app_or in Hx. destruct Hx as [Hx | Hx]; [contradiction | exact Hx]. }
  assert (HgHlk : forall x, In x (tyreqs (cA PhiH ++ cA PhiC)) ->
            lookup_subst x gH = Some (app_subst gH (TAbs x))).
  { intros x Hx. destruct (HgHcov x Hx) as [a Ha]. rewrite Ha. simpl. rewrite Ha.
    reflexivity. }
  assert (HsrC : sigma_regular (cS PhiC) (cA PhiC)).
  { apply (tele_sigma_regular WC). split; [intros z [] | constructor]. }
  destruct HsrC as [HsdC HsrC].
  destruct Hpok as [Hpd Hpr].
  assert (HgH : ctx_grounded gH (fun_ctx PhiH PhiC pins)).
  { split; [| split; [| split; [| split]]].
    - apply Forall_app. split; [| exact Hgrc].
      apply Forall_forall. intros p Hp. apply in_map_iff in Hp.
      destruct Hp as [t [<- Ht]]. simpl.
      destruct (Hcv2 t Ht) as [a Ha]. rewrite Ha.
      exact (@grounding_lookup_closed g2 t a Hgr2 Ha).
    - intros t tau Hl. simpl in Hl. rewrite !lookup_subst_app in Hl.
      destruct (lookup_subst t (cS PhiH)) as [a|] eqn:E1.
      + injection Hl as <-.
        assert (HtH : In t (tyreqs (cA PhiH))).
        { apply Hsd2. apply in_map_iff. exists (t, a).
          split; [reflexivity | apply callee_env_agree_aux1; exact E1]. }
        rewrite (HgHlk t ltac:(rewrite tyreqs_app; apply in_or_app; left; exact HtH)).
        f_equal. rewrite (HgHH t HtH). simpl. rewrite (Hab2 t a E1).
        apply dentry_agree_retype_aux1. intros z Hz. symmetry. apply HgHH.
        rewrite Forall_forall in Hsr2.
        exact (Hsr2 (t, a) (callee_env_agree_aux1 _ _ E1) z Hz).
      + destruct (lookup_subst t pins) as [a|] eqn:E2.
        * injection Hl as <-.
          assert (HtC : In t (tyreqs (cA PhiC))).
          { apply Hpd. apply in_map_iff. exists (t, a).
            split; [reflexivity | apply callee_env_agree_aux1; exact E2]. }
          rewrite (HgHlk t ltac:(rewrite tyreqs_app; apply in_or_app; right; exact HtC)).
          f_equal. rewrite (HgHC t HtC). simpl. rewrite (Hpinc t a E2).
          apply dentry_agree_retype_aux1. intros z Hz. symmetry. apply HgHC.
          rewrite Forall_forall in Hpr.
          exact (Hpr (t, a) (callee_env_agree_aux1 _ _ E2) z Hz).
        * assert (HtC : In t (tyreqs (cA PhiC))).
          { apply HsdC. apply in_map_iff. exists (t, tau).
            split; [reflexivity | apply callee_env_agree_aux1; exact Hl]. }
          rewrite (HgHlk t ltac:(rewrite tyreqs_app; apply in_or_app; right; exact HtC)).
          f_equal. rewrite (HgHC t HtC). simpl. rewrite (Habsc t tau Hl).
          apply dentry_agree_retype_aux1. intros z Hz. symmetry. apply HgHC.
          rewrite Forall_forall in HsrC.
          exact (HsrC (t, tau) (callee_env_agree_aux1 _ _ Hl) z Hz).
    - exact HgHcov.
    - simpl. split.
      + rewrite !map_app, tyreqs_app. intros z Hz. apply in_app_or in Hz.
        destruct Hz as [Hz | Hz]; [apply in_or_app; left; exact (Hsd2 z Hz) |].
        apply in_app_or in Hz.
        destruct Hz as [Hz | Hz]; apply in_or_app; right; [exact (Hpd z Hz) | exact (HsdC z Hz)].
      + rewrite tyreqs_app. apply Forall_app. split.
        { eapply Forall_impl; [| exact Hsr2]. intros p Hp z Hz.
          apply in_or_app. left. exact (Hp z Hz). }
        apply Forall_app. split.
        { eapply Forall_impl; [| exact Hpr]. intros p Hp z Hz.
          apply in_or_app. right. exact (Hp z Hz). }
        { eapply Forall_impl; [| exact HsrC]. intros p Hp z Hz.
          apply in_or_app. right. exact (Hp z Hz). }
    - simpl. rewrite tyreqs_app. intros Hz. apply in_app_or in Hz.
      destruct Hz as [Hz | Hz]; [exact (Hth2 Hz) |].
      exact (this_notin_wf_tele WC (fun H => H) Hz). }
  assert (HdH : xdenv_strong Hstt gH (cA (fun_ctx PhiH PhiC pins)) (d2 ++ dc)).
  { destruct Hd2 as [Hc2 [He2 [Hrg2 Hk2]]].
    simpl. split; [| split; [| split]].
    - apply Forall_app. split.
      + eapply Forall_impl; [| exact Hc2]. intros i [Ht | [en He]]; [left; exact Ht | right].
        rewrite lookup_xdenv_app, He. eexists; reflexivity.
      + eapply Forall_impl; [| exact Htcov].
        intros i [Ht | [en He]]; [left; exact Ht | right].
        rewrite lookup_xdenv_app. destruct (lookup_xdenv i d2) as [en'|];
          [eexists; reflexivity | eexists; exact He].
    - apply Forall_app. split.
      + rewrite Forall_forall in He2, Hk2 |- *. intros p Hp.
        apply (@xdentry_agree_reground Sg Rp P Fb Hstt WF g2 gH (fst p) (snd p) (He2 p Hp)).
        intros x Hx. symmetry. apply HgHH. exact (Hk2 p Hp x Hx).
      + pose proof (proj1 (xdentry_all_Forall _ _ _ _ _ gc dc) Hallc) as Hallc'.
        rewrite Forall_forall in Hallc', Hkeysc |- *. intros p Hp.
        apply (@xdentry_agree_reground Sg Rp P Fb Hstt WF gc gH (fst p) (snd p) (Hallc' p Hp)).
        intros x Hx. symmetry. apply HgHC. exact (Hkeysc p Hp x Hx).
    - rewrite tyreqs_app. apply Forall_app. split.
      + eapply Forall_impl; [| exact Hrg2]. intros i Hi x Hx.
        apply in_or_app. left. exact (Hi x Hx).
      + apply Forall_forall. intros i Hi x Hx. apply in_or_app. right.
        exact (@tele_items_regular Sg empty_ctx _ PhiC WF WC
                 (fun k Hk => match Hk with end) i Hi x Hx).
    - rewrite tyreqs_app. apply Forall_app. split.
      + eapply Forall_impl; [| exact Hk2]. intros p Hp x Hx.
        apply in_or_app. left. exact (Hp x Hx).
      + eapply Forall_impl; [| exact Hkeysc]. intros p Hp x Hx.
        apply in_or_app. right. exact (Hp x Hx). }
  assert (Hactual : xdenv_agree Hstt gH (cA (fun_ctx PhiH PhiC pins))
    (xapply_env P d h th (d2 ++ dc))).
  { unfold provision_environment;rewrite Legacy;split.
    - apply strong_values. unfold xapply_env. apply xdenv_strong_call; exact HdH.
    - eapply runtime_types_apply.
      + exact (proj2 Hd).
      + exact Hg.
      + exact Hthw.
      + intros t Ht. apply call_lookup_defined. rewrite Hdomth. apply Hi2; exact Ht.
      + intro t. rewrite (@xwf_inputs Sg Rp P Fb WFX h Dh Dc pins Hh t).
        symmetry. apply tele_ctx_tyreqs; exact HtcH.
      + intros t Ht. rewrite (HgHH t Ht). apply HgPhi; exact Ht.
      + exact HgHC.
      + exact HtypesC.
      + exact Hfree2. }
  (* the body, checked once, at its actual restricted type application *)
  pose proof (IHb _ _ _ Hbt (xapply_env P d h th (d2 ++ dc)) gH st Hstt HgH Hactual Hhp) as B.
  destruct (xinterp_binds j P (xapply_env P d h th (d2 ++ dc)) [] st bs) as [[[d3 ge3] st3] | rr] eqn:Eb;
    [| exact B].
  destruct B as [_ [HB [gB [HxB [HhB [HgB [HdB HlB]]]]]]].
  unfold provision_environment in HdB;rewrite Legacy in HdB.
  assert (Hres : exists dr, xcompose d3 (inst_smap th Dr) = Some dr).
  { apply xcompose_defined. apply Forall_forall. intros [a b] Hp.
    destruct (inst_smap_in _ _ _ _ Hp) as [_ [Hb Hnt]]. simpl.
    destruct (proj1 HdB) as [HcB _]. rewrite Forall_forall in HcB, Hprov.
    destruct (HcB b (Hprov b Hb)) as [Hty | Hen]; [| exact Hen].
    rewrite Hty in Hnt. discriminate Hnt. }
  destruct Hres as [dr Hdr]. rewrite Hdr.
  set (gx := map (fun t => (t, app_subst gB (TAbs t))) (tyreqs Dr)).
  set (g' := gx ++ g).
  assert (Hlg : forall x, ~ In x (tyreqs Dr) -> lookup_subst x g' = lookup_subst x g).
  { intros x Hx. unfold g', gx. apply lookup_prefix_notin. exact Hx. }
  assert (Hfr : forall x, In x (tyreqs (cA Phi)) -> ~ In x (tyreqs Dr)).
  { intros x Hx HxDr. rewrite Forall_forall in Hfresh.
    apply (Hfresh x HxDr). apply safety_case_bindty_aux1. exact Hx. }
  assert (Hreads : forall x, In x (tyreqs (cA Phi)) ->
                    app_subst g (TAbs x) = app_subst g' (TAbs x)).
  { intros x Hx. simpl. rewrite (Hlg x (Hfr x Hx)). reflexivity. }
  assert (HDrB : forall t, In t (tyreqs Dr) -> In t (tyreqs (cA PhiB))).
  { intros t Ht. apply wf_ty_frees_aux3. rewrite Forall_forall in Hprov.
    apply Hprov. apply safety_case_bindty_aux1. exact Ht. }
  destruct Hg as [Hgr [Habg [Hcv [[Hdom Hran] Hthis]]]].
  assert (Hab' : absorbs g' (cS Phi)).
  { intros x a Hl.
    assert (HxPhi : In x (tyreqs (cA Phi))).
    { apply Hdom. apply safety_case_bindty_aux4 with (tau := a). exact Hl. }
    rewrite (Hlg x (Hfr x HxPhi)), (Habg x a Hl). f_equal.
    apply app_subst_frees_agree. intros z Hz. symmetry. apply Hlg.
    apply Hfr. rewrite Forall_forall in Hran.
    exact (Hran (x, a) (safety_case_bindty_aux5 _ _ Hl) z Hz). }
  assert (Hout_U : forall t, In t (tyreqs Dr) -> ~ In t (tyreqs (cA PhiU))).
  { intros t Ht Hin'. apply (wf_tele_append_fresh WrU eq_refl t Ht).
    apply safety_case_bindty_aux1. exact Hin'. }
  assert (HthisU : ~ In this_sym (tyreqs (cA PhiU)))
    by exact (this_notin_wf_tele WU (fun H => H)).
  assert (HthisR : ~ In this_sym (tyreqs Dr)).
  { intros Hin'. apply safety_case_bindty_aux1 in Hin'.
    destruct (wf_tele_item_ok WrU _ Hin') as [Phi0 Hok].
    inversion Hok; subst. congruence. }
  assert (Hth_read : forall x, In x (tyreqs (cA PhiU)) ->
            app_subst g' (app_subst th (TAbs x))
            = app_subst g (app_subst (cS Phi) (app_subst th (TAbs x)))).
  { intros x Hx. rewrite Habs.
    rewrite <- Hdomth in Hx. destruct (subst_chain_aux1 x th Hx) as [tx Htx].
    simpl. rewrite Htx. symmetry. apply dentry_agree_retype_aux1.
    intros z Hz. apply Hreads. rewrite Forall_forall in Hthw.
    exact (wf_ty_frees (Hthw (x, tx) (callee_env_agree_aux1 _ _ Htx)) z Hz). }
  assert (Hagree : forall i, In i Dr -> forall x,
            In x (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
            app_subst gB (TAbs x) = app_subst g' (app_subst th (TAbs x))).
  { intros i Hi x Hx.
    assert (HiF : In i (cA PhiH ++ cA PhiC ++ Dr))
      by (apply in_or_app; right; apply in_or_app; right; exact Hi).
    pose proof (HregF i HiF x Hx) as HxF. rewrite !tyreqs_app in HxF.
    destruct (in_dec Nat.eq_dec x (tyreqs Dr)) as [HxD | HxD].
    - assert (Hnth : ~ In x (map fst th)) by (rewrite Hdomth; exact (Hout_U x HxD)).
      rewrite (app_subst_none th (TAbs x))
        by (intros z [<- | []]; apply lookup_subst_none; exact Hnth).
      unfold g', gx. rewrite !app_subst_TAbs.
      rewrite (lookup_prefix_in (fun t => app_subst gB (TAbs t)) _ x g HxD).
      reflexivity.
    - assert (HxF' : In x (tyreqs (cA PhiH)) \/ In x (tyreqs (cA PhiC))).
      { apply in_app_or in HxF. destruct HxF as [H | H]; [left; exact H |].
        apply in_app_or in H. destruct H as [H | H]; [right; exact H | contradiction]. }
      assert (HlBx : app_subst gB (TAbs x) = app_subst gH (TAbs x)).
      { apply HlB. simpl. rewrite tyreqs_app.
        destruct HxF' as [H | H]; apply in_or_app; [left | right]; exact H. }
      rewrite HlBx.
      assert (HxU : In x (tyreqs (cA PhiU))).
      { assert (HiU : In i (cA PhiU ++ Dr)) by (apply in_or_app; right; exact Hi).
        pose proof (HregU i HiU x Hx) as H. rewrite tyreqs_app in H.
        apply in_app_or in H. destruct H as [H | H]; [exact H | contradiction]. }
      rewrite (Hth_read x HxU).
      destruct HxF' as [HxH | HxC].
      + rewrite (HgHH x HxH). apply HgPhi. exact HxH.
      + rewrite (HgHC x HxC). apply HfixK; [exact HxC |].
        apply in_or_app. right. unfold res_reads. apply in_flat_map.
        exists i. split; assumption. }
  assert (Habs' : forall t0, app_subst g' (app_subst (cS Phi) t0) = app_subst g' t0)
    by (apply (absorbs_all Hab')).
  assert (Hthis_reads : forall i, In i Dr -> forall x,
            In x (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
            x <> this_sym).
  { intros i Hi x Hx ->.
    assert (HiU : In i (cA PhiU ++ Dr)) by (apply in_or_app; right; exact Hi).
    pose proof (HregU i HiU this_sym Hx) as H. rewrite tyreqs_app in H.
    apply in_app_or in H. destruct H as [H | H]; [exact (HthisU H) | exact (HthisR H)]. }
  assert (Htrans : forall i en, In i Dr -> xdentry_agree HB gB i en ->
             xdentry_agree HB g' (app_subst_item th i) en).
  { intros i en Hi Hag.
    destruct (wf_tele_item_ok WrU i Hi) as [Phi0 Hok].
    apply (@xdentry_agree_retype Sg Rp P Fb HB gB i g' (app_subst_item th i) en WF Hag).
    - destruct i as [t|v|f|rt m th0|w]; simpl; try reflexivity.
      f_equal.
      + apply comp_agree. intros z Hz. apply (Hagree _ Hi). simpl.
        apply in_or_app. left. apply in_or_app. left. exact Hz.
      + unfold map_snd. rewrite map_map. apply map_ext_in. intros [a b] Hab. simpl.
        f_equal. apply comp_agree. intros z Hz. apply (Hagree _ Hi). simpl.
        apply in_or_app. left. apply in_or_app. right. apply in_flat_map.
        exists (a, b). split; assumption.
    - intros t Ht.
      assert (Hdep_in : In t (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i))
        by (apply in_or_app; left; exact Ht).
      rewrite (Hagree i Hi t Hdep_in).
      unfold res_coherent in Hcoh. rewrite Forall_forall in Hcoh.
      specialize (Hcoh i Hi). rewrite Forall_forall in Hcoh.
      rewrite <- Habs', (Hcoh t Ht), Habs'. reflexivity.
    - intros rt m th0 rt' th' Dm Sm tau E1 E2 Hm Htau. subst i.
      simpl in E2. injection E2 as <- <-.
      destruct (callee_env_agree_aux11_aux1 Hok Hm) as [PhiDm [HtcDm [Hdinc Hdcov]]].
      destruct (wf_mth_decl WF _ Hm) as [PhiDm' [Wt [Hps Hret]]].
      rewrite (wf_tele_ctx Wt) in HtcDm. injection HtcDm as <-.
      assert (Wtau : wf_ty Sg (add_item (ITy this_sym) PhiDm') tau).
      { destruct Htau as [Htau | ->]; [rewrite Forall_forall in Hps; exact (Hps tau Htau) | exact Hret]. }
      assert (Hrt : app_subst gB rt = app_subst g' (app_subst th rt)).
      { apply comp_agree. intros z Hz. apply (Hagree _ Hi). simpl.
        apply in_or_app. left. apply in_or_app. left. exact Hz. }
      unfold interp_m. apply chain3_agree. intros z Hz.
      pose proof (wf_ty_frees Wtau z Hz) as Hz0. simpl in Hz0.
      destruct Hz0 as [Hz0 | Hz0].
      + subst z.
        assert (Hn0 : lookup_subst this_sym th0 = None).
        { apply lookup_subst_none. intros Hinth.
          exact (this_notin_wf_tele Wt (fun H => H) (Hdinc _ Hinth)). }
        simpl. rewrite Hn0, lookup_map_snd, Hn0. simpl. exact Hrt.
      + apply (Hdcov z (@msig_frees_in _ _ _ Htau Hz)) in Hz0. destruct (subst_chain_aux1 z th0 Hz0) as [tz Htz].
        simpl. rewrite Htz, lookup_map_snd, Htz.
        assert (Hnt : forall y, In y (ty_frees tz) -> y <> this_sym).
        { intros y Hy. apply (Hthis_reads _ Hi y). simpl.
          apply in_or_app. left. apply in_or_app. right. apply in_flat_map.
          exists (z, tz). split; [apply callee_env_agree_aux1; exact Htz | exact Hy]. }
        rewrite (app_subst_none (this_subst rt) tz)
          by (intros y Hy; simpl; destruct (Nat.eqb y this_sym) eqn:E;
              [apply Nat.eqb_eq in E; exfalso; exact (Hnt y Hy E) | reflexivity]).
        rewrite (app_subst_none (this_subst (app_subst th rt)) (app_subst th tz)).
        * apply comp_agree. intros y Hy. apply (Hagree _ Hi). simpl.
          apply in_or_app. left. apply in_or_app. right. apply in_flat_map.
          exists (z, tz). split; [apply callee_env_agree_aux1; exact Htz | exact Hy].
        * intros y Hy. simpl. destruct (Nat.eqb y this_sym) eqn:E; [| reflexivity].
          apply Nat.eqb_eq in E. subst y. exfalso.
          destruct (frees_app_subst_bound _ _ _ Hy) as [[p [Hp Hyp]] | [Hy' _]].
          -- rewrite Forall_forall in Hthw. apply Hthis.
             exact (wf_ty_frees (Hthw p Hp) this_sym Hyp).
          -- exact (Hnt this_sym Hy' eq_refl). }
  exists HB, g'. split; [exact HxB |]. split; [exact HhB |].
  split; [| split].
  - split; [| split; [| split; [| split]]].
    + apply Forall_app. split; [| exact Hgr].
      unfold gx. apply Forall_forall. intros p Hp.
      apply in_map_iff in Hp. destruct Hp as [t [<- Ht]]. simpl.
      destruct HgB as [HgrB [_ [HcvB _]]].
      destruct (HcvB t (HDrB t Ht)) as [a Ha]. rewrite Ha.
      exact (@grounding_lookup_closed gB t a HgrB Ha).
    + exact Hab'.
    + intros x Hx. simpl in Hx. rewrite tyreqs_app, tyreqs_app_tele in Hx.
      apply in_app_or in Hx. destruct Hx as [Hx | Hx].
      * exists (app_subst gB (TAbs x)). unfold g', gx.
        apply lookup_prefix_in. exact Hx.
      * rewrite (Hlg x (Hfr x Hx)). exact (Hcv x Hx).
    + eapply sigma_regular_weaken; [split; [exact Hdom | exact Hran] |].
      apply open_res_tyreqs.
    + simpl. rewrite tyreqs_app, tyreqs_app_tele. intros Hin'. apply in_app_or in Hin'.
      destruct Hin' as [Hin' | Hin']; [exact (HthisR Hin') | exact (Hthis Hin')].
  - unfold provision_environment;rewrite Legacy;split.
    + assert (Vresult : xdenv_values HB g' (cA (open_res (app_tele th Dr) Phi)) (dr ++ d)).
      {
    apply (@values_ext Sg Rp P Fb HB g' _ (dr ++ dA)).
    2: { intros jj Hj Ht. rewrite !lookup_xdenv_app.
         destruct (lookup_xdenv jj dr) eqn:Ej; [reflexivity |].
         simpl in Hj. apply in_app_or in Hj. destruct Hj as [Hj | Hj]; [| exact (HagA jj Hj Ht)].
         exfalso. unfold app_tele in Hj. apply in_map_iff in Hj. destruct Hj as [i [<- Hi]].
         assert (Hti : is_ty_item i = false) by (destruct i; simpl in Ht |- *; congruence).
         destruct (@lookup_smap_inst th Dr i Hi Hti) as [i'' Hl''].
         destruct (@xcompose_first d3 (inst_smap th Dr) dr _ i'' Hdr Hl'') as [en [_ Hen]].
         congruence. }
    apply strong_values.
    simpl. split; [| split; [| split]].
    + apply Forall_app. split.
      * apply Forall_forall. intros i' Hi'.
        unfold app_tele in Hi'. apply in_map_iff in Hi'. destruct Hi' as [i [<- Hi]].
        destruct (is_ty_item i) eqn:Hti.
        { left. destruct i; try discriminate Hti. reflexivity. }
        right.
        destruct (@lookup_smap_inst th Dr i Hi Hti) as [i'' Hl''].
        destruct (@xcompose_first d3 (inst_smap th Dr) dr _ i'' Hdr Hl'')
          as [en [_ Hen]].
        exists en. rewrite lookup_xdenv_app, Hen. reflexivity.
      * apply Forall_forall. intros i Hi.
        destruct (Hcov i Hi) as [Hty | [en Hen]]; [left; exact Hty | right].
        rewrite lookup_xdenv_app.
        destruct (lookup_xdenv i dr) as [en'|]; [exists en'; reflexivity |].
        exists en. exact Hen.
    + apply Forall_app. split.
      * apply Forall_forall. intros [i en] Hp.
        destruct (@xcompose_entry d3 (inst_smap th Dr) dr _ Hdr Hp) as [sat' [Hs Hl']].
        simpl in Hs, Hl'.
        destruct (inst_smap_in _ _ _ _ Hs) as [-> [Hi Hnt]].
        destruct (proj1 HdB) as [_ [HeB _]].
        assert (HiB : In sat' (cA PhiB)) by (rewrite Forall_forall in Hprov; exact (Hprov _ Hi)).
        pose proof (HeB sat' en HiB Hnt Hl') as HagB.
        exact (Htrans sat' en Hi HagB).
      * apply Forall_forall. intros [i en] Hp.
        pose proof (Hent (i, en) Hp) as Hag'. simpl in Hag' |- *.
        apply (@xdentry_agree_reground Sg Rp P Fb HB WF g g' i en
                 (@xdentry_agree_mono Sg Rp P Fb Hstt HB HxB _ _ _ Hag')).
        intros x Hx. apply Hreads. rewrite Forall_forall in Hkeys.
        exact (Hkeys (i, en) Hp x Hx).
    + apply Forall_app. split.
      * apply Forall_forall. intros i Hi.
        exact (@open_res_th_regular Sg Phi u Du Dr th WF Hu Hsat Hthw Hcoh
                 (conj Hdom Hran) i Hi).
      * eapply Forall_impl; [| exact HAreg].
        intros i Hi x Hx. apply open_res_tyreqs. exact (Hi x Hx).
    + apply Forall_app. split.
      * apply Forall_forall. intros [i en] Hp.
        destruct (@xcompose_entry d3 (inst_smap th Dr) dr _ Hdr Hp) as [sat' [Hs _]].
        simpl in Hs. destruct (inst_smap_in _ _ _ _ Hs) as [-> [Hi _]]. simpl.
        exact (@open_res_th_regular Sg Phi u Du Dr th WF Hu Hsat Hthw Hcoh
                 (conj Hdom Hran) _ (in_app_tele th Dr sat' Hi)).
      * eapply Forall_impl; [| exact Hkeys].
        intros p Hp x Hx. apply open_res_tyreqs. exact (Hp x Hx).
      }
      eapply values_ext; [exact Vresult|].
      intros i Hi Nt. symmetry. apply lookup_xresult_non_type; exact Nt.
    + eapply runtime_types_result.
      * exact (proj2 Hd).
      * exact (proj2 HdB).
      * exact HDrB.
      * intros t Ht. unfold g',gx. rewrite app_subst_TAbs.
        rewrite (lookup_prefix_in (fun t => app_subst gB (TAbs t)) (tyreqs Dr) t g Ht). reflexivity.
      * intros t Ht. symmetry; apply Hreads; exact Ht.
      * eapply inst_smap_typefree; exact Hdr.
      * apply runtime_result_generations_empty. intros t Hout.
        apply lookup_subst_none. intro Input. rewrite Hdomth in Input.
        apply (Hout_U t (proj2 (call_type_item Dr t) Hout) Input).
  - intros x Hx. rewrite tyreqs_app_tele in Hx. exact (Hlg x Hx).
Qed.

Lemma xsafety_binds_apply : forall j, j <= k ->
    (forall Phi bs Phi', xbinds_ty Sg Rp P Fb Phi bs Phi' ->
       forall d g st H, ctx_grounded g Phi -> xdenv_agree H g (cA Phi) d ->
       HL.heap_ok (PsiH H) st ->
       binds_post H Phi Phi' g (xinterp_binds j P d [] st bs))
    /\ (xp_immutable P=false ->forall Phi u Du Dr th sm,
          g_fun Sg u = Some (Du, Dr, None) ->
          avail Phi (IFun u) ->
          sat Sg Phi th Du ->
          Forall (fun p => wf_ty Sg Phi (snd p)) th ->
          smap_ok Phi th sm Du ->
          fixed_ok Sg Phi u th ->
          res_coherent Sg Phi th Dr ->
          Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
          forall d g st H, ctx_grounded g Phi -> xdenv_agree H g (cA Phi) d ->
          HL.heap_ok (PsiH H) st ->
          apply_post H Phi (app_tele th Dr) g d (xinterp_apply j P d u th sm st))
    /\ (xp_immutable P=true ->forall H Phi g d st u Du Dr results inputs outputs,
      g_fun Sg u=Some(Du,Dr,None)->In(IFun u)(cA Phi)->
      ctx_grounded g Phi ->xdenv_agree H g(cA Phi)d ->
      application_input_contract Sg P Phi u Du inputs ->
      result_map_contract Sg P Phi u Du Dr results outputs ->
      reified_scope_closed Sg P(cA(open_res results Phi))->HL.heap_ok(PsiH H)st ->
      apply_r_post H Phi results g d(xinterp_apply_r j P d u inputs outputs st)).
Proof.
  induction j as [| j IHj]; intros Hj.
  { split;[|split].
    all:(intros;cbn;eexists;split;[apply sext_refl|eassumption]). }
  assert (Hj' : j <= k) by lia.
  destruct (IHj Hj') as [IHb [IHa IHra]].
  split.
  2:{ split;[intros Legacy;exact(@xapply_step Legacy j IHb)|exact(@xapply_r_step j IHb)]. }

  intros Phi bs Phi' Hb.
  destruct Hb as [Phi
    | Phi t tau' r Phi' Hgt Hneq Hfresh Htau' Hr
    | Phi v Dv tauv e1 r Phi' Hgv Hs0 He1 Hr
    | Phi f Df Sf f' D' S' body sm r Phi' Hlegacy Hf Hf' Hbd Hsf Hs' Hsm Hsig Hr
    | Phi tauk m Dm Sf th f' D' S' body sm tau0' rest r Phi'
        Hlegacy Hm Hwk Hsat Hthwf Hf' Hbd Hs' Hsm Hpar0 Hteq Hprest Hpret Hr
    | Phi f Df S p r Phi' Hf Hsf Hok Hr
    | Phi tauk m Dm S th p r Phi' Hm Hwk Hsat Hthwf Hok Hr
    | Phi u Du Dr h Dh Dc pins sm smc r Phi' Hlegacy Hu Hh Hbh Hl Hcap Hfx Hr
    | Phi u Du Dr w Dw sm r Phi' Hlegacy Hu Hw Hav Hl Hxx Hfx Hr
    | Phi u Du Dr th sm r Phi' Hlegacy Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh Hr
    | Phi f Df Sf code Dc Sc body PhiC sm r Phi' Mode Slot Code Body Tele Closed Contract ClosedNext Hr
    | Phi rt m th Dm Sm code Dc Sc body PhiC sm r Phi' Mode Slot Wrt Sat Wth Code Body Tele Closed Contract ClosedNext Hr
    | Phi u h Du Dh Dr Dc pins PhiD PhiC sm smc code r Phi' Mode U Hdcl FB Code TD TC Closed Contract ClosedNext Hr
    | Phi u w Du Dw Dr sm r Phi' Mode U W Available Link ClosedNext Hr
    | Phi u Du Dr results inputs outputs r Phi' Mode U Available Inputs Outputs ClosedNext Hr
    | Phi bs Phi' u Mode Available Missing];
    intros d g st H Hg Hd Hhp; simpl xinterp_binds.
  - split; [reflexivity |]. exists H, g. split; [apply sext_refl |]. split; [exact Hhp |].
    split; [exact Hg |]. split; [exact Hd |]. apply meanings_on_refl.
  - destruct(@provision_bind_type Sg Rp P Fb H Phi t tau' d g WF Hneq Hfresh Htau' Hg Hd)as[Hg2 Hd2].
    destruct(@reified_bindty_ground Sg Phi t tau' g Hneq Hfresh Htau' Hg)as[_ Hlook].
    eapply binds_post_step; [exact (IHb _ _ _ Hr _ _ st H Hg2 Hd2 Hhp) | apply sext_refl | | ].
    + intros x Hx. simpl. right. exact Hx.
    + intros x Hx;change(match lookup_subst x ((t,app_subst g(app_subst(cS Phi)tau'))::g)with Some a=>a|None=>TAbs x end= app_subst g(TAbs x)).
      rewrite(Hlook x Hx);reflexivity.
  - assert (Hv0 : venv_ok H g (cA Phi) [] []) by (split; constructor).
    pose proof (@IHfuel j Hj' None false Phi [] e1 _ d [] g st H He1 Hg Hd Hv0 Hhp) as S1.
    destruct (xinterp j P d [] st e1) as [o g1 st1] eqn:E1.
    destruct S1 as [H1 [Hx1 [Hh1 S1]]]. simpl in Hh1.
    unfold xsafe0 in S1. simpl in S1.
    destruct o; simpl;
      try (destruct S1 as [rt [E _]]; discriminate);
      try (destruct S1 as [E _]; discriminate); try contradiction;
      try (exists H1; split; [exact Hx1 | exact Hh1]).
    destruct S1 as [Hw [Hv1 _]]. inversion Hv1; subst.
    rewrite (colg Hg), (colg Hg) in Hw.
    destruct (@provision_bind_value Sg Rp P Fb H1 Phi g d v Dv tauv x Hgv Hs0 Hw Hg (shared_mono Hx1 Hd)) as [Hg1 Hd1].
    eapply binds_post_step;
      [ exact (IHb _ _ _ Hr _ _ st1 H1 Hg1 Hd1 Hh1) | exact Hx1 | intros y Hy; exact Hy
      | apply meanings_on_refl ].
  - unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (world_bindfn WF f f' Hf Hf' Hbd Hsf Hs' Hsm Hsig Hg Hd) as [dc [Hcomp [Hg1 Hd1]]].
    rewrite Hcomp.
    eapply binds_post_step;
      [ exact (IHb _ _ _ Hr _ _ st H Hg1 ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd1) Hhp) | apply sext_refl | intros y Hy; exact Hy
      | apply meanings_on_refl ].
  - unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (world_bindmth WF m f' Hm Hwk Hsat Hthwf Hf' Hbd Hs' Hsm Hpar0 Hteq Hprest Hpret Hg Hd)
      as [dc [Hcomp [Hg1 Hd1]]].
    rewrite Hcomp.
    eapply binds_post_step;
      [ exact (IHb _ _ _ Hr _ _ st H Hg1 ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd1) Hhp) | apply sext_refl | intros y Hy; exact Hy
      | apply meanings_on_refl ].
  - eapply binds_post_step;
      [ exact (IHb _ _ _ Hr _ _ st H Hg (@world_bindprim H Phi f Df S p d g Hf Hsf Hok Hg Hd) Hhp)
      | apply sext_refl | intros y Hy; exact Hy | apply meanings_on_refl ].
  - eapply binds_post_step;
      [ exact (IHb _ _ _ Hr _ _ st H Hg
                 (@world_bindprimm H Phi tauk m Dm S th p d g Hm Hwk Hsat Hthwf Hok Hg Hd) Hhp)
      | apply sext_refl | intros y Hy; exact Hy | apply meanings_on_refl ].
  - unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (@world_bindfun H Hlegacy Phi u Du Dr h Dh Dc pins sm smc d g Hu Hh Hbh Hl Hcap Hfx Hg Hd)
      as [dc [Hcomp [Hg1 Hd1]]].
    rewrite Hcomp.
    eapply binds_post_step;
      [ exact (IHb _ _ _ Hr _ _ st H Hg1 ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd1) Hhp) | apply sext_refl | intros y Hy; exact Hy
      | apply meanings_on_refl ].
  - unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (@world_bindfuna H Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hg Hd)
      as [h [sms [dc [Hlk [Hg1 Hd1]]]]].
    rewrite Hlk.
    eapply binds_post_step;
      [ exact (IHb _ _ _ Hr _ _ st H Hg1 ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd1) Hhp) | apply sext_refl | intros y Hy; exact Hy
      | apply meanings_on_refl ].
  - pose proof (IHa Hlegacy Phi u Du Dr th sm Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh d g st H Hg Hd Hhp) as A1.
    destruct (xinterp_apply j P d u th sm st) as [[dr st1] | rr] eqn:Ea.
    + destruct A1 as [H1 [g1 [Hx1 [Hh1 [Hg1 [Hd1 Hl1]]]]]].
      eapply binds_post_step;
        [ exact (IHb _ _ _ Hr _ _ st1 H1 Hg1 Hd1 Hh1) | exact Hx1 | apply open_res_tyreqs | ].
      intros x Hx. change(match lookup_subst x g1 with Some a=>a|None=>TAbs x end=app_subst g(TAbs x)).
      rewrite Hl1;[reflexivity|]. intros Hin. rewrite tyreqs_app_tele in Hin.
      rewrite Forall_forall in Hfresh. apply (Hfresh x Hin).
      apply safety_case_bindty_aux1. exact Hx.
    + destruct rr as [o g1 st1]. exact A1.
  - destruct(@provision_bind_function Sg Rp P Fb H Mode Phi g d f code Df Sf Dc Sc PhiC sm body
      Slot Code Body Tele Hg Hd Closed Contract ClosedNext)as[dc[Compose[GroundNext EnvNext]]].
    rewrite Compose;eapply binds_post_step;
    [exact(IHb _ _ _ Hr _ _ st H GroundNext EnvNext Hhp)|apply sext_refl|
     intros t Hin;exact Hin|apply meanings_on_refl].
  - destruct(@provision_bind_method Sg Rp P Fb H Mode Phi g d rt m th code Dm Sm Dc Sc PhiC sm body
      Slot Code Body Tele Hg Hd Closed Contract ClosedNext)as[dc[Compose[GroundNext EnvNext]]].
    rewrite Compose;eapply binds_post_step;
    [exact(IHb _ _ _ Hr _ _ st H GroundNext EnvNext Hhp)|apply sext_refl|
     intros t Hin;exact Hin|apply meanings_on_refl].
  - destruct(@provision_bind_functor Sg Rp P Fb H Mode Phi g d u h Du Dh Dr Dc pins PhiD PhiC sm smc code
      U Hdcl FB Code TD TC Hg Hd Closed Contract ClosedNext)as[dc[Compose[GroundNext EnvNext]]].
    rewrite Compose;eapply binds_post_step;
    [exact(IHb _ _ _ Hr _ _ st H GroundNext EnvNext Hhp)|apply sext_refl|
     intros t Hin;exact Hin|apply meanings_on_refl].
  - destruct(@provision_link_functor Sg Rp P Fb H Mode Phi g d u w Du Dw Dr sm U W
      (avail_fun_in Available)Hg Hd(Link g Hg)ClosedNext)
      as[h[sms[dc[Lookup[GroundNext EnvNext]]]]].
    rewrite Lookup;eapply binds_post_step;
    [exact(IHb _ _ _ Hr _ _ st H GroundNext EnvNext Hhp)|apply sext_refl|
     intros t Hin;exact Hin|apply meanings_on_refl].
  - pose proof(IHra Mode H Phi g d st u Du Dr results inputs outputs U(avail_fun_in Available)
      Hg Hd Inputs Outputs ClosedNext Hhp)as Result.
    destruct(xinterp_apply_r j P d u inputs outputs st)as[[dr st1]|rr]eqn:Run.
    + destruct Result as[H1[g1[Ext[Heap1[GroundNext[EnvNext Mean]]]]]].
      eapply binds_post_step;
      [exact(IHb _ _ _ Hr _ _ st1 H1 GroundNext EnvNext Heap1)|exact Ext|
       apply open_res_tyreqs|exact Mean].
    + destruct rr;exact Result.

  - exfalso;eapply(@provision_missing_functor_impossible Sg Rp P Fb H g(cA Phi)d u);
    [exact Hd|apply avail_fun_in;exact Available|exact Missing].

Qed.

Lemma rec_fields_typed : forall H1 (g : subst) (Fc : ty -> ty) (fts : list (fld * ty)) (fs : list (fld * xexpr)) (ws : list xvalue),
  (forall t, app_subst g (Fc t) = app_subst g t) ->
  map fst fs = map fst fts ->
  Forall2 (fun w tau => xvt H1 w (app_subst g (Fc tau))) ws (map snd fts) ->
  Forall2 (fun ft fw => xvt H1 (snd fw) (snd ft))
          (map_snd (app_subst g) fts) (combine (map fst fs) ws).
Proof.
  intros H1 g Fc fts. induction fts as [|[f t] fts IH]; intros fs ws Hc Hk Hws.
  - destruct fs; [| discriminate]. inversion Hws; subst. constructor.
  - destruct fs as [|[f' e] fs]; [discriminate|]. simpl in Hk. injection Hk as -> Hk.
    inversion Hws as [|w t' ws' ts' Hw Hws']; subst. simpl.
    constructor; [simpl; rewrite <- Hc; exact Hw | apply IH; assumption].
Qed.

Lemma case_rec : forall R L Phi G fs fts d ge g st H,
    map fst fs = map fst fts ->
    Forall2 (fun fe ft => xhas R L Phi G (snd fe) (snd ft)) fs fts ->
    ctx_grounded g Phi ->
    xdenv_agree H g (cA Phi) d ->
    venv_ok H g (cA Phi) G ge ->
    HL.heap_ok (PsiH H) st ->
    xsafe H g Phi G R L (TNom rec_nom fts) (xinterp (S k) P d ge st (XRec fs)).
Proof.
  intros R L Phi G fs fts d ge g st H Hk Hfs Hcg Hd Hv Hhp.
  pose proof (colg Hcg) as Hcol.
  destruct (@xwf_rec Sg Rp P Fb WFX) as [Hrr Hgr].
  assert (Hargs : Forall2 (fun e tau => xhas R L Phi G e tau) (map snd fs) (map snd fts)).
  { clear -Hfs. induction Hfs; simpl; constructor; assumption. }
  pose proof (@xsafe_args k (le_n k) R L Phi G d g (map snd fs) _ ge st H Hargs Hcg Hd Hv Hhp) as Hsa.
  change (xinterp (S k) P d ge st (XRec fs))
    with (match xinterp_list k P d ge st (map snd fs) with
          | inl (ws, g1, st1) => XR (XOk (XVRec (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end).
  destruct (xinterp_list k P d ge st (map snd fs)) as [[[ws g1] st1] | rr].
  - destruct Hsa as [H1 [Hx1 [Hh1 [Hws Hv1]]]]. exists H1. split; [exact Hx1 |]. split; [exact Hh1 |].
    unfold xsafe0. cbn [xr_out xr_g]. split; [| exact Hv1].
    rewrite Hcol, app_subst_nom.
    assert (Hlen : List.length ws = List.length fs).
    { rewrite (Forall2_length Hws), length_map, <- (length_map fst fts), <- Hk, length_map.
      reflexivity. }
    eapply XvRec; [exact Hrr | exact Hgr | |].
    + rewrite map_fst_combine, map_snd_fst; [exact Hk | rewrite length_map; symmetry; exact Hlen].
    + exact (@rec_fields_typed H1 g (app_subst (cS Phi)) fts fs ws Hcol Hk Hws).
  - destruct rr as [o g1 st1]. destruct o; apply xsafe_of_ctl; exact Hsa.
Qed.

Lemma xsafe_here : forall H g Phi G R L tau r,
  HL.heap_ok (PsiH H) (xr_st r) -> xsafe0 H g Phi G R L tau r -> xsafe H g Phi G R L tau r.
Proof. intros. exists H. split; [apply sext_refl | split; assumption]. Qed.

Lemma xsafe_sub_h : forall H g Phi G R L tau' tau r,
  xsafe H g Phi G R L tau' r -> subty Phi tau' tau -> xsafe H g Phi G R L tau r.
Proof.
  intros H g Phi G R L tau' tau r S0 Hs. eapply xsafe_map; [reflexivity | | exact S0].
  intros H' _ R0. eapply xsafe0_sub; eassumption.
Qed.

Lemma xsafe_pop1_h : forall H g Phi x t G R L tau r,
  xsafe H g Phi ((x, t) :: G) R L tau r -> xsafe H g Phi G R L tau (pop1 r).
Proof.
  intros H g Phi x t G R L tau r S0.
  apply (@xsafe_map H g Phi ((x, t) :: G) R L tau g Phi G R L tau r (pop1 r));
    [destruct r; reflexivity | | exact S0].
  intros H' _ R0. eapply xsafe0_pop1; eassumption.
Qed.

Lemma xsafe_popn_h : forall H g Phi Gb G R L tau r,
  xsafe H g Phi (Gb ++ G) R L tau r -> xsafe H g Phi G R L tau (popn (List.length Gb) r).
Proof.
  intros H g Phi Gb G R L tau r S0.
  apply (@xsafe_map H g Phi (Gb ++ G) R L tau g Phi G R L tau r (popn (List.length Gb) r));
    [destruct r; reflexivity | | exact S0].
  intros H' _ R0. eapply xsafe0_popn; eassumption.
Qed.

Lemma xsafe_const_boundary : forall R L Phi G initializer tau id d ge g st H,
  xsafe H g Phi G R L tau (xinterp k P d ge st initializer) ->
  HL.heap_ok(PsiH H)st ->
  xsafe H g Phi G R L tau (xinterp(S k)P d ge st(XConst id initializer)).
Proof.
 intros R L Phi G initializer tau id d ge g st H Safe Heap.
 cbn[xinterp];destruct k;[|exact Safe].
 exists H;split;[apply sext_refl|];split;[exact Heap|exact I].
Qed.

Lemma xsafe_step : XSAFE (S k).
Proof.
  pose proof (IHfuel (le_n k)) as SK.
  unfold XSAFE. fix IH 12.
  intros R L Phi G e tau d ge g st H Hty.
  destruct Hty as
    [ L Phi G x tau Hx
    | L Phi G
    | L Phi G e tau' tau Hty Hsub Hwf
    | L Phi G v Dv tauv Hgv Hav
    | L Phi G n D tau0 th th0 e Hrp Hgt Hsat Hth Hpayload Hty
    | L Phi G n D tau0 th e spec Hcast Hrp Hgt Hsat Hth Hty
    | L Phi G f Df S args Hf Hav Hargs
    | L Phi G f Df S body th th0 sm args Hlegacy Hf Hb Hsat Hth Hsm Hth0 Htheta Hargs
    | L Phi G e0 tau0 m Dm S rt0 th0 args Hm Hty0 Hin Hrec Huniq Hargs
    | L Phi G x e1 e2 tau1 tau Hty1 Hfr Hty2
    | L Phi G t tau' e tau Hgt Hne Hfresh Htau' Hty Htau HR
    | L Phi G t tau' e tau Hplain Hgt Hne Hfresh Htau' Hty Htau HR
    | L Phi G v Dv tauv e1 e tau Hgv Hsat Hty1 Hty
    | L Phi G f Df S f' D' S' body sm e tau Hlegacy Hf Hf' Hb Hsf Hs' Hsm Hsig Hty
    | L Phi G tauk m Dm S th f' D' S' body sm tau0' rest e tau
        Hlegacy Hm Hwk Hsat Hthwf Hf' Hb Hs' Hsm Hpar0 Hteq Hprest Hpret Hty
    | L Phi G f Df S p e tau Hf Hsf Hok Hty
    | L Phi G tauk m Dm S th p e tau Hm Hwk Hsat Hthwf Hok Hty
    | L Phi G u Du Dr h Dh Dc pins sm smc e tau Hlegacy Hu Hh Hbh Hl Hcap Hfx Hty
    | L Phi G u Du Dr w Dw sm e tau Hlegacy Hu Hw Hav Hl Hxx Hfx Hty
    | L Phi G u Du Dr th sm e tau Hlegacy Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh Hty Hesc HRf
    | L Phi G f D S body PhiD slot sm args Mode Fn Code Tele Closed Contract Args
    | L Phi G f Df Sf code Dc Sc body PhiC sm e tau Mode Slot Code Body Tele Closed Contract ClosedNext Hty
    | L Phi G rt m th Dm Sm code Dc Sc body PhiC sm e tau Mode Slot Wrt Sat Wth Code Body Tele Closed Contract ClosedNext Hty
    | L Phi G u h Du Dh Dr Dc pins PhiD PhiC sm smc code e tau Mode U Hdcl FB Code TD TC Closed Contract ClosedNext Hty
    | L Phi G u w Du Dw Dr sm e tau Mode U W Available Link ClosedNext Hty
    | L Phi G u Du Dr results inputs outputs e tau Mode U Available Inputs Outputs ClosedNext Hty Htau HR
    | L Phi G fs fts Hk Hfs
    | L Phi G e tau fts f tf Hty Hwf Heq Ha
    | L Phi G e tau n th D fts f tf Hty Hwf Heq Hrp Hg0 Hpayload Ha
    | L Phi G e1 e2 tau1 tau Hty1 Hty2
    | L Phi G x e tx Hx Hty
    | L Phi G c e1 e2 tau Hc Hty1 Hty2
    | L Phi G c b tb Hc Hb
    | L Phi G b tb Hb
    | L Phi G e rt tau HR Hty
    | Phi G tau
    | L Phi G e0 tau0 arms Gbs tau Hty0 Hwf Hnd Hex Hpats Hregs Hbodies
    | L Phi G id initializer tau Hty
    | L Phi G e tau' tau Mode Hty GroundEq Wtau
    | L Phi G e tau u Mode Available Missing ];
    intros Hcg Hd Hv Hhp; pose proof (colg Hcg) as Hcol.
  - (* var *)
    destruct (@venv_lookup H g (cA Phi) G ge x tau Hv Hx) as [w [Hl Hw]].
    apply xsafe_here; simpl; rewrite Hl; [exact Hhp |].
    simpl. split; [rewrite Hcol; exact Hw | exact Hv].
  - (* unit *)
    apply xsafe_here; [exact Hhp |]. simpl. split; [constructor | exact Hv].
  - (* subsumption: the same term, at the same fuel *)
    eapply xsafe_sub_h; [exact (IH R L Phi G e tau' d ge g st H Hty Hcg Hd Hv Hhp) | exact Hsub].
  - (* val *)
    assert (Hin : In (IVal v) (cA Phi)) by (apply avail_val_in; exact Hav).
    destruct(@provision_value Sg Rp P Fb H g(cA Phi)d v Dv tauv Hd Hin Hgv)
      as[w[Hlook Hen]].
    apply xsafe_here; simpl; rewrite Hlook; [exact Hhp |].
    simpl. split; [rewrite Hcol, Hcol; exact Hen | exact Hv].
  - (* tag *)
    pose proof (SK R L Phi G e _ d ge g st H Hty Hcg Hd Hv Hhp) as Hres.
    assert (Hincl : incl (ty_frees tau0) (map fst th)).
    { destruct Hsat as [PhiD [Htc [Hdom _]]]. rewrite Hdom.
      destruct (wf_tag_decl WF _ Hgt) as [PhiD' [Wt Hwf]].
      rewrite (wf_tele_ctx Wt) in Htc. injection Htc as <-.
      exact Hwf. }
    cbn [xinterp]. destruct (xinterp k P d ge st e) as [o g1 st1] eqn:E.
    destruct o; try pass Hres.
    destruct Hres as [H1 [Hx1 [Hh1 [Hw Hv1]]]].
    erewrite xcast_ordinary by exact Hrp.
    exists H1. split; [exact Hx1 |]. split; [exact Hh1 |].
    unfold xsafe0. cbn [xr_out xr_g]. split; [| exact Hv1].
    rewrite Hcol, Hcol in Hw. rewrite Hcol, Hcol, app_subst_nom.
    eapply XvTag; [exact Hrp | exact Hgt |].
    rewrite <- ordinary_payload_commutes; [exact Hw| |exact Hpayload].
    apply payload_covered_from_frees. intros t Ht. apply subst_chain_aux1. exact (Hincl t Ht).
  - exact (@case_cast_tag R L Phi G n D tau0 th e spec d ge g st H Hcast Hrp Hgt Hsat Hth Hty Hcg Hd Hv Hhp).
  - exact (@case_need R L Phi G f Df S args d ge g st H Hf Hav Hargs Hcg Hd Hv Hhp).
  - exact (@case_call Hlegacy R L Phi G f Df S body th th0 sm args d ge g st H Hf Hb Hsat Hth Hsm Hth0 Htheta Hargs Hcg Hd Hv Hhp).
  - exact (@case_meth R L Phi G e0 tau0 m Dm S rt0 th0 args d ge g st H Hm Hty0 Hin Hrec Hargs Hcg Hd Hv Hhp).
  - (* let *)
    pose proof (SK R L Phi G e1 tau1 d ge g st H Hty1 Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e1) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]]. rewrite Hcol in Hw.
    apply (xsafe_ext Hx1).
    apply (@xsafe_pop1_h H1 g Phi x tau1 G R L tau).
    apply (SK R L Phi ((x, tau1) :: G) e2 tau d ((x, x0) :: g1) g st1 H1 Hty2 Hcg
              (shared_mono Hx1 Hd)); [| exact Hh1].
    exact (@venv_cons H1 g (cA Phi) G g1 x tau1 x0 Hv1 Hw Hfr).
  - (* type bind *)
    destruct (@shared_bindty_world H R L Phi G t tau' tau d ge g Hne Hfresh Htau' Htau HR Hcg Hd Hv)
      as [Hg2 [Hd2 [Hv2 _]]].
    simpl. pose proof (SK R L _ G e tau (xbind_ty P d t tau') ge _ st H Hty Hg2 Hd2 Hv2 Hhp) as R0.
    eapply xsafe_map; [reflexivity | | exact R0].
    intros H' Hx R1.
    destruct (@shared_bindty_world H' R L Phi G t tau' tau d ge g Hne Hfresh Htau' Htau HR Hcg
                (shared_mono Hx Hd) (venv_ok_mono Hx Hv)) as [_ [_ [_ Hback]]].
    exact (Hback _ R1).
  - (* implicit type bind: the same term, at the same fuel *)
    destruct (@shared_bindty_world H R L Phi G t tau' tau d ge g Hne Hfresh Htau' Htau HR Hcg Hd Hv)
      as [Hg2 [Hd2 [Hv2 _]]].
    unfold xbind_ty in Hd2; rewrite Hplain in Hd2.
    pose proof (IH R L _ G e tau d ge _ st H Hty Hg2 Hd2 Hv2 Hhp) as R0.
    eapply xsafe_map; [reflexivity | | exact R0].
    intros H' Hx R1.
    destruct (@shared_bindty_world H' R L Phi G t tau' tau d ge g Hne Hfresh Htau' Htau HR Hcg
                (shared_mono Hx Hd) (venv_ok_mono Hx Hv)) as [_ [_ [_ Hback]]].
    exact (Hback _ R1).
  - (* val bind *)
    pose proof (SK R L Phi G e1 _ d ge g st H Hty1 Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e1) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]]. rewrite Hcol, Hcol in Hw.
    destruct (@provision_bind_value Sg Rp P Fb H1 Phi g d v Dv tauv x Hgv Hsat Hw Hcg (shared_mono Hx1 Hd)) as [Hg' Hd'].
    apply (xsafe_ext Hx1). exact (SK R L _ G e tau _ g1 g st1 H1 Hty Hg' Hd' Hv1 Hh1).
  - (* fn bind *)
    unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (world_bindfn WF f f' Hf Hf' Hb Hsf Hs' Hsm Hsig Hcg Hd) as [dc [Hcomp [Hg' Hd']]].
    simpl. rewrite Hcomp. exact (SK R L _ G e tau _ ge g st H Hty Hg' ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd') Hv Hhp).
  - (* method bind *)
    unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (world_bindmth WF m f' Hm Hwk Hsat Hthwf Hf' Hb Hs' Hsm Hpar0 Hteq Hprest Hpret Hcg Hd)
      as [dc [Hcomp [Hg' Hd']]].
    simpl. rewrite Hcomp. exact (SK R L _ G e tau _ ge g st H Hty Hg' ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd') Hv Hhp).
  - (* primitive bind *)
    simpl. exact (SK R L (add_item (IFn f) Phi) G e tau _ ge g st H Hty Hcg
                    (@world_bindprim H Phi f Df S p d g Hf Hsf Hok Hcg Hd) Hv Hhp).
  - (* primitive method bind *)
    simpl. exact (SK R L (add_item (IMth tauk m th) Phi) G e tau _ ge g st H Hty Hcg
                    (@world_bindprimm H Phi tauk m Dm S th p d g Hm Hwk Hsat Hthwf Hok Hcg Hd) Hv Hhp).
  - (* functor bind *)
    unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (@world_bindfun H Hlegacy Phi u Du Dr h Dh Dc pins sm smc d g Hu Hh Hbh Hl Hcap Hfx Hcg Hd)
      as [dc [Hcomp [Hg' Hd']]].
    simpl. rewrite Hcomp. exact (SK R L _ G e tau _ ge g st H Hty Hg' ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd') Hv Hhp).
  - (* functor copy *)
    unfold provision_environment in Hd;rewrite Hlegacy in Hd.
    destruct (@world_bindfuna H Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hcg Hd)
      as [h [sms [dc [Hlk [Hg' Hd']]]]].
    simpl. rewrite Hlk. exact (SK R L _ G e tau _ ge g st H Hty Hg' ltac:(unfold provision_environment;rewrite Hlegacy;exact Hd') Hv Hhp).
  - (* application: the existential's unpack *)
    destruct (xsafety_binds_apply (le_n k)) as [_ [SA _]].
    pose proof (SA Hlegacy Phi u Du Dr th sm Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh d g st H Hcg Hd Hhp) as A1.
    simpl. destruct (xinterp_apply k P d u th sm st) as [[dr st1] | rr] eqn:Ea.
    2: { destruct rr as [o g1 st1]. unfold ok_ctl in A1. simpl in A1.
         destruct o; try contradiction; destruct A1 as [H' [Hx Hh']];
           exists H'; (split; [exact Hx | split; [exact Hh' | exact I]]). }
    destruct A1 as [H1 [g' [Hx1 [Hh1 [Hg' [Hd' Hl']]]]]].
    assert (Hfr : forall x, In x (tyreqs (cA Phi)) -> ~ In x (tyreqs Dr)).
    { intros x Hx HxDr. rewrite Forall_forall in Hfresh.
      apply (Hfresh x HxDr). apply safety_case_bindty_aux1. exact Hx. }
    assert (Hl'' : forall x, ~ In x (tyreqs Dr) -> lookup_subst x g' = lookup_subst x g).
    { intros x Hx. apply Hl'. rewrite tyreqs_app_tele. exact Hx. }
    assert (Hv' : venv_ok H1 g' (cA (open_res (app_tele th Dr) Phi)) G ge).
    { eapply venv_reground; [exact (venv_ok_mono Hx1 Hv) | apply open_res_tyreqs |].
      intros x Hx. apply Hl''. apply Hfr. exact Hx. }
    pose proof (SK R L _ G e tau (dr ++ d) ge g' st1 H1 Hty Hg' Hd' Hv' Hh1) as Rs.
    apply (xsafe_ext Hx1).
    eapply xsafe_map; [reflexivity | | exact Rs].
    intros H' _ R1.
    eapply xsafe0_back; [exact R1 | | |].
    + change (cS (open_res (app_tele th Dr) Phi)) with (cS Phi).
      apply app_subst_frees_agree. intros x Hx. apply Hl''. intros HxDr.
      destruct (frees_app_subst_bound _ _ _ Hx) as [[p [Hp Hxp]] | [Hxt _]].
      * destruct Hcg as [_ [_ [_ [[_ Hran] _]]]]. rewrite Forall_forall in Hran.
        exact (Hfr x (Hran p Hp x Hxp) HxDr).
      * rewrite Forall_forall in Hesc. exact (Hesc x HxDr Hxt).
    + intros rt E. subst R. simpl in HRf. apply app_subst_frees_agree.
      intros x Hx. apply Hl''. intros HxDr. rewrite Forall_forall in HRf. exact (HRf x HxDr Hx).
    + intros ge0 [Hge _]. split; [| exact (proj2 Hv)].
      pose proof (proj2 Hv) as HGreg. clear - Hge HGreg Hl'' Hfr.
      induction Hge as [|p q G0 ge1 [Hk Hw] Hr0 IH]; constructor;
        inversion HGreg as [|? ? Hp Hps]; subst.
      * split; [exact Hk|]. rewrite <- (app_subst_frees_agree g' g (snd p)); [exact Hw|].
        intros x Hx. apply Hl''. apply Hfr. exact (Hp x Hx).
      * apply IH. exact Hps.
  - exact(@case_call_r R L Phi G f D S body PhiD slot sm args d ge g st H
      Mode Fn Code Tele Closed Contract Args Hcg Hd Hv Hhp).
  - destruct(@provision_bind_function Sg Rp P Fb H Mode Phi g d f code Df Sf Dc Sc PhiC sm body
      Slot Code Body Tele Hcg Hd Closed Contract ClosedNext)as[dc[Compose[GroundNext EnvNext]]].
    cbn[xinterp];rewrite Compose;exact(SK R L _ G e tau _ ge g st H Hty GroundNext EnvNext Hv Hhp).
  - destruct(@provision_bind_method Sg Rp P Fb H Mode Phi g d rt m th code Dm Sm Dc Sc PhiC sm body
      Slot Code Body Tele Hcg Hd Closed Contract ClosedNext)as[dc[Compose[GroundNext EnvNext]]].
    cbn[xinterp];rewrite Compose;exact(SK R L _ G e tau _ ge g st H Hty GroundNext EnvNext Hv Hhp).
  - destruct(@provision_bind_functor Sg Rp P Fb H Mode Phi g d u h Du Dh Dr Dc pins PhiD PhiC sm smc code
      U Hdcl FB Code TD TC Hcg Hd Closed Contract ClosedNext)as[dc[Compose[GroundNext EnvNext]]].
    cbn[xinterp];rewrite Compose;exact(SK R L _ G e tau _ ge g st H Hty GroundNext EnvNext Hv Hhp).
  - destruct(@provision_link_functor Sg Rp P Fb H Mode Phi g d u w Du Dw Dr sm U W
      (avail_fun_in Available)Hcg Hd(Link g Hcg)ClosedNext)
      as[h[sms[dc[Lookup[GroundNext EnvNext]]]]].
    cbn[xinterp];rewrite Lookup;exact(SK R L _ G e tau _ ge g st H Hty GroundNext EnvNext Hv Hhp).
  - destruct(xsafety_binds_apply(le_n k))as[_[_ Apply]].
    pose proof(Apply Mode H Phi g d st u Du Dr results inputs outputs U(avail_fun_in Available)
      Hcg Hd Inputs Outputs ClosedNext Hhp)as Applied.
    cbn[xinterp];destruct(xinterp_apply_r k P d u inputs outputs st)as[[dr st1]|rr]eqn:Run.
    2:{ destruct rr as[o ge1 st1];unfold ok_ctl in Applied;cbn in Applied;
        destruct o;try contradiction;destruct Applied as[H1[Ext Heap]];
        exists H1;(split;[exact Ext|split;[exact Heap|exact I]]). }
    destruct Applied as[H1[g1[Ext[Heap1[GroundNext[EnvNext Mean]]]]]].
    assert(VarsNext:venv_ok H1 g1(cA(open_res results Phi))G ge).
    { eapply venv_meanings_reground;[exact(venv_ok_mono Ext Hv)|apply open_res_tyreqs|exact Mean]. }
    pose proof(SK R L _ G e tau(dr++d)ge g1 st1 H1 Hty GroundNext EnvNext VarsNext Heap1)as Tail.
    apply(xsafe_ext Ext);eapply xsafe_map;[reflexivity| |exact Tail].
    intros H2 Ext2 Safe;eapply xsafe0_back;[exact Safe| | |].
    + rewrite(colg GroundNext),Hcol;eapply meanings_on_type;[exact Mean|exact(wf_ty_frees Htau)].
    + intros rt ER;subst R;eapply meanings_on_type;[exact Mean|exact(wf_ty_frees HR)].
    + intros ge0[Values _];split;[|exact(proj2 Hv)].
      pose proof(proj2 Hv)as Scope;clear - Values Scope Mean.
      induction Values as[|p q G ge0[Key Value]Rest IH];constructor;
      inversion Scope as[|? ? Free More];subst.
      * split;[exact Key|]. rewrite <-(@meanings_on_type(cA Phi)g1 g(snd p)Mean Free);exact Value.
      * apply IH;exact More.
  - exact (@case_rec R L Phi G fs fts d ge g st H Hk Hfs Hcg Hd Hv Hhp).
  - (* a record's field *)
    pose proof (SK R L Phi G e tau d ge g st H Hty Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]]. rewrite Heq, app_subst_nom in Hw.
    destruct (@xwf_rec Sg Rp P Fb WFX) as [Hrr Hgr].
    destruct (xvt_rec_inv Hw Hrr Hgr) as [r [-> [Hk Hr]]].
    assert (Hk' : map fst r = map fst fts) by (rewrite Hk; exact (map_snd_fst _ _)).
    clear Hk; rename Hk' into Hk. apply Forall2_map_snd_l in Hr.
    destruct (@assoc_typed_F H1 (app_subst g) fts r f tf Hk Hr Ha) as [v [Hv' Hvt]].
    simpl. rewrite Hv'. exists H1. split; [exact Hx1 |]. split; [exact Hh1 |].
    simpl. split; [| exact Hv1]. rewrite Hcol. exact Hvt.
  - (* a struct's field *)
    pose proof (SK R L Phi G e tau d ge g st H Hty Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]]. rewrite Heq, app_subst_nom in Hw.
    destruct (xvt_tag_inv Hw Hrp ltac:(rewrite Hg0; discriminate)) as [D1 [taup1 [w' [-> [Hg1 Hw']]]]].
    rewrite Hg0 in Hg1. injection Hg1 as <- <-. rewrite ordinary_payload_nom in Hw'.
    destruct (@xwf_rec Sg Rp P Fb WFX) as [Hrr Hgr].
    destruct (xvt_rec_inv Hw' Hrr Hgr) as [r [-> [Hk Hr]]].
    assert (Hk' : map fst r = map fst fts) by (rewrite Hk; exact (map_snd_fst _ _)).
    clear Hk; rename Hk' into Hk. apply Forall2_map_snd_l in Hr.
    destruct (@assoc_typed_F H1 _ fts r f tf Hk Hr Ha) as [v [Hv' Hvt]].
    simpl. rewrite Hv'. exists H1. split; [exact Hx1 |]. split; [exact Hh1 |].
    simpl. split; [| exact Hv1].
    rewrite Heq in Hwf.
    destruct (wf_tag_inv' (ltac:(rewrite Hg0; discriminate) : g_tag Sg n <> None) Hwf)
      as [D2 [taup2 [Hg2 [PhiD [Htc [Hdom _]]]]]].
    rewrite Hg0 in Hg2. injection Hg2 as <- <-.
    destruct (wf_tag_decl WF _ Hg0) as [PhiD' [Wt Hwt]].
    rewrite (wf_tele_ctx Wt) in Htc. injection Htc as <-.
    pose proof (rec_payload_scoped _ _ Hwt) as Hfts.
    rewrite Hcol. rewrite ordinary_payload_commutes; [exact Hvt| |].
    + apply payload_covered_from_frees. intros t Ht. apply subst_chain_aux1.
      rewrite Hdom. rewrite Forall_forall in Hfts.
      exact (Hfts _ (assoc_ty_in _ _ Ha) t Ht).
    + eapply ordinary_payload_nom_parameter_ok; [exact Hpayload|exact (assoc_ty_in _ _ Ha)].
  - (* sequence *)
    pose proof (SK R L Phi G e1 tau1 d ge g st H Hty1 Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e1) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [_ Hv1]]]].
    apply (xsafe_ext Hx1).
    exact (SK R L Phi G e2 tau d g1 g st1 H1 Hty2 Hcg (shared_mono Hx1 Hd) Hv1 Hh1).
  - (* assignment *)
    pose proof (SK R L Phi G e tx d ge g st H Hty Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]]. rewrite Hcol in Hw.
    destruct (@venv_assign H1 g (cA Phi) G g1 x tx x0 Hv1 Hx Hw) as [g2 [Ha Hv2]].
    rewrite Ha. exists H1. split; [exact Hx1 |]. split; [exact Hh1 |].
    simpl. split; [constructor | exact Hv2].
  - (* if *)
    pose proof (SK R L Phi G c _ d ge g st H Hc Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st c) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]].
    rewrite !bool_ty_subst in Hw.
    destruct (@truthy_typed H1 x Hw) as [bb Hb]. rewrite Hb.
    apply (xsafe_ext Hx1).
    destruct bb; [exact (SK R L Phi G e1 tau d g1 g st1 H1 Hty1 Hcg (shared_mono Hx1 Hd) Hv1 Hh1)
                 | exact (SK R L Phi G e2 tau d g1 g st1 H1 Hty2 Hcg (shared_mono Hx1 Hd) Hv1 Hh1)].
  - (* while *)
    pose proof (SK R L Phi G c _ d ge g st H Hc Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st c) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]].
    rewrite !bool_ty_subst in Hw.
    destruct (@truthy_typed H1 x Hw) as [bb Hbb]. rewrite Hbb.
    apply (xsafe_ext Hx1).
    destruct bb; [| exists H1; split; [apply sext_refl | split; [exact Hh1 |]];
                    simpl; split; [constructor | exact Hv1]].
    pose proof (SK R true Phi G b tb d g1 g st1 H1 Hb Hcg (shared_mono Hx1 Hd) Hv1 Hh1) as R2.
    destruct (xinterp k P d g1 st1 b) as [o2 g2 st2] eqn:E2.
    destruct R2 as [H2 [Hx2 [Hh2 R2]]]. simpl in Hh2.
    unfold xsafe0 in R2. simpl in R2.
    destruct o2; simpl;
      try (exists H2; split; [exact Hx2 | split; [exact Hh2 | exact R2]]).
    + destruct R2 as [_ Hv2]. apply (xsafe_ext Hx2).
      exact (SK R L Phi G (XWhile c b) TUnit d g2 g st2 H2
               (@XtWhile Sg Rp P Fb R L Phi G c b tb Hc Hb) Hcg
               (shared_mono (sext_trans Hx1 Hx2) Hd) Hv2 Hh2).
    + destruct R2 as [_ Hv2]. exists H2. split; [exact Hx2 |]. split; [exact Hh2 |].
      simpl. split; [constructor | exact Hv2].
  - (* loop *)
    pose proof (SK R true Phi G b tb d ge g st H Hb Hcg Hd Hv Hhp) as R2.
    simpl. destruct (xinterp k P d ge st b) as [o2 g2 st2] eqn:E2.
    destruct R2 as [H2 [Hx2 [Hh2 R2]]]. simpl in Hh2.
    unfold xsafe0 in R2. simpl in R2.
    destruct o2; simpl;
      try (exists H2; split; [exact Hx2 | split; [exact Hh2 | exact R2]]).
    + destruct R2 as [_ Hv2]. apply (xsafe_ext Hx2).
      exact (SK R L Phi G (XLoop b) TUnit d g2 g st2 H2
               (@XtLoop Sg Rp P Fb R L Phi G b tb Hb) Hcg (shared_mono Hx2 Hd) Hv2 Hh2).
    + destruct R2 as [_ Hv2]. exists H2. split; [exact Hx2 |]. split; [exact Hh2 |].
      simpl. split; [constructor | exact Hv2].
  - (* return *)
    pose proof (SK R L Phi G e rt d ge g st H Hty Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw _]]]]. exists H1. split; [exact Hx1 |]. split; [exact Hh1 |].
    simpl. exists rt. split; [exact HR|].
    rewrite Hcol in Hw. exact Hw.
  - (* break *)
    apply xsafe_here; [exact Hhp |]. simpl. split; [reflexivity | exact Hv].
  - (* match *)
    pose proof (SK R L Phi G e0 tau0 d ge g st H Hty0 Hcg Hd Hv Hhp) as R1.
    simpl. destruct (xinterp k P d ge st e0) as [o g1 st1] eqn:E.
    destruct o; try pass R1.
    destruct R1 as [H1 [Hx1 [Hh1 [Hw Hv1]]]].
    destruct (@select_typed H1 Phi g tau0 G R L tau arms Gbs x Hcg Hwf Hnd
                (@catches H1 Phi g tau0 arms x Hwf Hex Hw) Hpats Hregs Hbodies Hw)
      as [bs [eb [Gb [Hsel [Heb [Hbs Hreg]]]]]].
    rewrite Hsel.
    rewrite <- (Forall2_length Hbs).
    apply (xsafe_ext Hx1).
    apply xsafe_popn_h.
    apply (SK R L Phi (Gb ++ G) eb tau d (bs ++ g1) g st1 H1 Heb Hcg (shared_mono Hx1 Hd));
      [| exact Hh1].
    exact (@venv_app H1 g (cA Phi) Gb G bs g1 Hbs Hreg Hv1).
  - (* constant initializer boundary *)
    apply xsafe_const_boundary;[exact(SK R L Phi G initializer tau d ge g st H Hty Hcg Hd Hv Hhp)|exact Hhp].
  - (* Immutable checked scopes may identify types through their grounding equations. *)
    pose proof(IH R L Phi G e tau' d ge g st H Hty Hcg Hd Hv Hhp)as Safe.
    eapply xsafe_map;[reflexivity| |exact Safe].
    intros H' Ext Safe0;unfold xsafe0 in *.
    destruct(xr_out(xinterp(S k)P d ge st e));try exact Safe0.
    rewrite Hcol in Safe0 |- *;rewrite(GroundEq g Hcg)in Safe0;exact Safe0.

  - exfalso;eapply(@provision_missing_functor_impossible Sg Rp P Fb H g(cA Phi)d u);
    [exact Hd|apply avail_fun_in;exact Available|exact Missing].

Qed.

End Cases.


(* ===================== 8. The theorems ==================================== *)

(* Type safety of MossX, for every fuel. *)
Theorem xsafety : forall fuel, XSAFE fuel.
Proof.
  intros fuel. induction fuel as [fuel IH] using lt_wf_ind.
  destruct fuel as [| k].
  - intros R L Phi G e tau d ge g st H _ _ _ _ Hh.
    exists H. split; [apply sext_refl | split; [exact Hh | exact I]].
  - apply xsafe_step. intros j Hj. apply IH. lia.
Qed.

(* What a caller sees of a typed body: never XErr. *)
Lemma ret_call_not_err : forall H gb Phib Gb rt r g1,
  xsafe H gb Phib Gb (Some rt) false rt r -> forall c, xr_out (ret_call g1 r) <> XErr c.
Proof.
  intros H gb Phib Gb rt [o gr st] g1 [H' [_ [_ R0]]] c. unfold xsafe0 in R0. simpl in *.
  destruct o; simpl; try discriminate.
  - destruct R0 as [E _]. discriminate.
  - contradiction.
Qed.

(* Running `main`: if its body is typed (xwf), and the initial environment
   agrees with what main assumes at some grounding of main's type
   requirements and some store typing the initial machine's heap agrees
   with, then the run never gets stuck, whatever the fuel. *)
Theorem xrun_main_safe : forall main Dm Sm body PhiD g0 d0 H0,
  g_fn Sg main = Some (Dm, Sm, None) ->
  fs_params Sm = [] ->
  xp_fn P main = Some body ->
  wf_tele Sg empty_ctx Dm PhiD ->
  ctx_grounded g0 PhiD ->
  xdenv_agree H0 g0 (cA PhiD) d0 ->
  forall fuel st0 c, HL.heap_ok (PsiH H0) st0 -> xr_out (run_main fuel P main d0 st0) <> XErr c.
Proof.
  intros main Dm Sm body PhiD g0 d0 H0 Hm Hps Hb Wt Hg Hd fuel st0 c Hh.
  unfold run_main. rewrite Hb.
  destruct (@xwf_bodies Sg Rp P Fb WFX main Dm Sm body Hm Hb) as [PhiD0 [Wt0 Hty]].
  apply wf_tele_ctx in Wt0. rewrite (wf_tele_ctx Wt) in Wt0.
  injection Wt0 as E. subst PhiD0.
  assert (Hv : venv_ok H0 g0 (cA PhiD) (body_env Sm) []).
  { unfold body_env. rewrite Hps. split; constructor. }
  pose proof (@xsafety fuel _ _ _ _ _ _ d0 [] g0 st0 H0 Hty Hg Hd Hv Hh) as R0.
  exact (@ret_call_not_err _ _ _ _ _ _ [] R0 c).
Qed.

(* The same from an empty store typing, which every machine agrees with. *)
Corollary xrun_main_safe0 : forall main Dm Sm body PhiD g0 d0,
  g_fn Sg main = Some (Dm, Sm, None) ->
  fs_params Sm = [] ->
  xp_fn P main = Some body ->
  wf_tele Sg empty_ctx Dm PhiD ->
  ctx_grounded g0 PhiD ->
  xdenv_agree (fun _ => None) g0 (cA PhiD) d0 ->
  forall fuel st0 c, xr_out (run_main fuel P main d0 st0) <> XErr c.
Proof.
  intros main Dm Sm body PhiD g0 d0 Hm Hps Hb Wt Hg Hd fuel st0 c.
  exact (@xrun_main_safe main Dm Sm body PhiD g0 d0 (fun _ => None) Hm Hps Hb Wt Hg Hd
           fuel st0 c (HL.heap_empty st0)).
Qed.

End Safety.
End XSafety.


(* ===================== 9. MossX's concrete primitives ===================== *)

From NM Require MossX XHeapC.

Module XSafetyC := XSafety MossX.ConcretePrims MossX.XS XHeapC.ConcreteHeap.

(* Type safety of the interpreter the oracle runs (MossX.XS, over the
   concrete primitives, whose laws are MossX.prim_sound, prim_msound,
   prim_usound and XHeapC.ConcreteHeap's heap laws), GC heap included. *)
Theorem mossx_typed_safe : forall Sg Rp P Fb, xwf Sg Rp P Fb ->
  forall R L Phi G e tau d ge g st H,
    xhas Sg Rp P Fb R L Phi G e tau ->
    ctx_grounded g Phi ->
    provision_environment Sg Rp P Fb H g (cA Phi) d ->
    venv_ok Sg Rp P H g (cA Phi) G ge ->
    XHeapC.ConcreteHeap.heap_ok (XSafetyC.PsiH Sg Rp P H) st ->
    forall fuel, XSafetyC.xsafe Sg Rp P H g Phi G R L tau
                   (MossX.XS.xinterp fuel P d ge st e).
Proof.
  intros Sg Rp P Fb Hwf R L Phi G e tau d ge g st H Hty Hg Hd Hv Hh fuel.
  exact (@XSafetyC.xsafety Sg Rp P Fb MossX.prim_sound MossX.prim_msound MossX.prim_usound Hwf
           fuel R L Phi G e tau d ge g st H Hty Hg Hd Hv Hh).
Qed.

Theorem mossx_run_main_safe : forall Sg Rp P Fb, xwf Sg Rp P Fb ->
  forall main Dm Sm body PhiD g0 d0,
    g_fn Sg main = Some (Dm, Sm, None) ->
    fs_params Sm = [] ->
    xp_fn P main = Some body ->
    wf_tele Sg empty_ctx Dm PhiD ->
    ctx_grounded g0 PhiD ->
    provision_environment Sg Rp P Fb (fun _ => None) g0 (cA PhiD) d0 ->
    forall fuel st0 c, MossX.XS.xr_out (MossX.XS.run_main fuel P main d0 st0) <> XErr c.
Proof.
  intros Sg Rp P Fb Hwf main Dm Sm body PhiD g0 d0 Hm Hps Hb Wt Hg Hd fuel st0 c.
  exact (@XSafetyC.xrun_main_safe0 Sg Rp P Fb MossX.prim_sound MossX.prim_msound MossX.prim_usound
           Hwf main Dm Sm body PhiD g0 d0 Hm Hps Hb Wt Hg Hd fuel st0 c).
Qed.
