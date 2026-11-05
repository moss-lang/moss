(* ------------------------------------------------------------------------ *)
(* BodyConst.v — a defined val's inlined initializer against ExConst       *)
(* (Track A, elaborate_sound step 2; BodySound.v's Pending, pd_const).     *)
(*                                                                           *)
(* const_step is pd_const: from OK f to ConstOK at f+1.  A val elaborated  *)
(* before is read from const_val's cache, whose entries the final state    *)
(* keeps (Elab's cache only grows: ElabFrame's fx_consts) and BodySound's  *)
(* Later reads as ConstFact (lt_consts).  A val elaborated now is its      *)
(* initializer, synthesized at fuel f in the frame of a fresh numbering.   *)
(*                                                                           *)
(* What this file assumes (section hypotheses):                             *)
(*   Hty   BodySound.Pending's pd_ty: elab_type against TyOf (the other    *)
(*         half of Track A, BodyTy.v on that branch).                       *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySound BodyData BodyTag.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

Tactic Notation "bi" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Ltac nogood H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

Section Const.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

(* [type lemma, the other half of Track A] Pending's pd_ty *)
Hypothesis Hty : forall ty c e s d s', ety_of ty (cx_mod c) (tymap e) e (cx_this c) s = Good (d, s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> Later w P R Fl s' ->
  A.TyOf WW (A.fr_mod F) (A.c_ty C) (A.fr_this F) ty (trF d) /\ WFty d.

Definition cx0 : A.cx := A.Cx [] [] [] [] [] [] [] [].

(* the empty environment, in a module's frame *)
Lemma cxrel_empty : forall mi sg F, In mi (w_order w) -> A.fr_mod F = gm_path (get_mod w mi) ->
  A.fr_ret F = trF (sg_ret sg) -> WFty (sg_ret sg) -> A.fr_this F = None -> A.fr_loop F = false ->
  CxRel w Fl (Cx mi sg None 0 [] "" [] []) empty_env F cx0.
Proof.
  intros mi sg F Hmi Hm Hr Wr Ht Hl.
  constructor; cbn; auto; try (intros; discriminate); try (rewrite Ht; reflexivity); try (rewrite Hl; reflexivity).
Qed.

(* what Later reads of a state, the locals aside *)
Lemma later_vars : forall vs n s, Later w P R Fl (s_vars vs n s) <-> Later w P R Fl s.
Proof. intros vs n s. split; intros [L1 L2 L3 L4 L5]; constructor; assumption. Qed.

Lemma inv_vars : forall vs n s, Inv s -> Inv (s_vars vs n s).
Proof. intros vs n s Hi. destruct Hi; constructor; assumption. Qed.

(* A val elaborated now: its initializer, synthesized at fuel f from the
   declared type's state, is ExConst's premises, given what the state
   after the synthesis keeps (ElabConsts.v uses this at the cache write). *)
Lemma const_fresh : forall f, OK w P R Fl f -> forall g n t0 i s declared s2 v0 vt s8,
  nget (w_syms w) (gs_id g) = Some g -> gs_assumes g = [] -> gs_decl g = ADVal n t0 (Some i) -> Inv s ->
  ety_of t0 (gs_mod g) [] empty_env None s = Good (declared, s2) ->
  synth f (Cx (gs_mod g) (FSig [] declared None) None 0 [] "" [] []) empty_env i (Some declared)
        (s_vars [] 0 s2) = Good ((v0, vt), s8) ->
  fits empty_env vt declared = true -> Later w P R Fl s8 ->
  ConstFact w P R Fl (gs_id g) v0 declared.
Proof.
  intros f IH g n t0 i s declared s2 v0 vt s8 Hg Eas Edecl Hi Hdecl Hsyn Efit L8 d n' t0' i' Hd Ha.
  pose proof (w_decl_read w R Fl _ g Hg) as Hd'. unfold W in Hd. rewrite Hd' in Hd. injection Hd as <-.
  cbn [A.d_ast A.d_assumes A.d_mod] in Ha |- *. rewrite Edecl in Ha. injection Ha as <- <- <-.
  pose proof (ok_ety_of t0 (gs_mod g) [] empty_env None s declared s2 Hi Hdecl) as [Hi2 Fs2].
  set (c := Cx (gs_mod g) (FSig [] declared None) None 0 [] "" [] []) in Hsyn.
  pose proof (inv_vars [] 0 s2 Hi2) as Hi5.
  destruct (ok_synth f c empty_env i (Some declared) _ _ _ Hi5 Hsyn) as [Hi8 Fs8].
  pose proof (later_back w P R Fl _ _ Fs8 L8) as L5. apply later_vars in L5 as L2.
  assert (Hmi : In (gs_mod g) (w_order w)) by exact (proj1 (load_world_symfacts w b Hload _ g Hg)).
  (* the declared type, read in the val's module *)
  set (F1 := A.Frame (gm_path (get_mod w (gs_mod g))) TUnit None false [] [] [] []).
  destruct (Hty t0 (Cx (gs_mod g) (FSig [] YUnit None) None 0 [] "" [] []) empty_env s declared s2 Hdecl F1 cx0 Hi
              (cxrel_empty _ (FSig [] YUnit None) F1 Hmi eq_refl eq_refl eq_refl eq_refl eq_refl) L2) as [Ty Wd].
  (* the initializer, in a frame numbering its locals as the run did *)
  set (F0 := A.Frame (gm_path (get_mod w (gs_mod g))) (trF declared) None false (st_vars s8) [] [] []).
  assert (Hv : VarsIn s8 F0) by (intros n0 k Hk; cbn; rewrite slook_sassoc; exact Hk).
  destruct (bs_synth _ _ _ _ _ IH c empty_env i (Some declared) _ v0 vt s8 Hsyn F0 cx0 Hi5
              (cxrel_empty _ (FSig [] declared None) F0 Hmi eq_refl eq_refl Wd eq_refl eq_refl) Wd (conj L8 Hv)) as [Ex Wt].
  rewrite Eas. split; [reflexivity|]. split; [exact Ty|]. split; [exact Wd|].
  exists F0, (trF vt). split; [exact Ex|]. split; [exact (fits_tr Fl empty_env vt declared eq_refl Efit) | reflexivity].
Qed.

Lemma const_step : forall f, OK w P R Fl f -> ConstOK w P R Fl (S f).
Proof.
  intros f IH g s v t s' H Hi L Hg d n t0 i Hd Ha.
  cbn [const_val] in H.
  bi H as s0 s1 Hget. unfold get in Hget. injection Hget as <- <-.
  destruct (nget (st_consts s) (gs_id g)) as [[x0 t1]|] eqn:Ec.
  { (* elaborated before: the cache's entry *)
    injection H as -> -> ->. exact (lt_consts _ _ _ _ _ L (gs_id g) v t Ec d n t0 i Hd Ha). }
  pose proof (w_decl_read w R Fl _ g Hg) as Hd'. unfold W in Hd. rewrite Hd' in Hd.
  pose proof Hd as Hd0. injection Hd0 as Ed.
  cbn [A.d_ast A.d_assumes A.d_mod] in Ha. rewrite <- Ed in Ha. cbn [A.d_ast] in Ha.
  destruct (gs_assumes g) as [|a0 r0] eqn:Eas; [|nogood H].
  rewrite Ha in H.
  bi H as declared s2 Hdecl.
  bi H as r3 s3 Hwv. destruct r3 as [v0 vt].
  destruct (fits empty_env vt declared) eqn:Efit; [|nogood H].
  bi H as u s4 Hupd. injection H as <- <- <-.
  unfold with_vars in Hwv. bi Hwv as s5 s6 Hg5. unfold get in Hg5. injection Hg5 as <- <-.
  bi Hwv as u5 s7 Hu5. unfold upd in Hu5. injection Hu5 as <- <-.
  bi Hwv as r8 s8 Hsyn. bi Hwv as u8 s9 Hu8. unfold upd in Hu8. injection Hu8 as <- <-. injection Hwv as -> <-.
  pose proof (ok_ety_of t0 (gs_mod g) [] empty_env None s declared s2 Hi Hdecl) as [Hi2 Fs2].
  pose proof (inv_vars [] 0 s2 Hi2) as Hi5.
  destruct (ok_synth f _ empty_env i (Some declared) _ _ _ Hi5 Hsyn) as [Hi8 Fs8].
  pose proof (inv_vars (st_vars s2) (st_nvar s2) s8 Hi8) as Hi9.
  destruct (ok_consts_add (gs_id g) (v0, declared) _ _ _ Hi9 Hupd) as [_ Fs4].
  pose proof (later_back w P R Fl _ _ Fs4 L) as L9. apply later_vars in L9 as L8.
  rewrite <- Hd' in Hd. fold (W w R Fl) in Hd.
  exact (const_fresh f IH g n t0 i s declared s2 v0 vt s8 Hg Eas Ha Hi Hdecl Hsyn Efit L8 d n t0 i Hd
           ltac:(rewrite <- Ed; exact Ha)).
Qed.

End Const.
