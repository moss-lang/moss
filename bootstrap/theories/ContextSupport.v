(* Actual context signature reading preserves declaration support. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore
 BodyTy ContextClean NominalSupport SourceSupport.
Import ListNotations.
Section Source.
Variable w : Elab.world. Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySoundCore.W w R Fl).
Lemma fn_sig_support : forall g tm e this s sg s',
 fn_sig g tm e this s = Good (sg,s') -> Inv s -> st_w s = w -> In (gs_mod g) (w_order w) ->
 Forall (fun p => Support WW (snd p)) tm -> Forall (fun p => WFty (snd p)) tm ->
 EnvSupport WW e -> (forall k y, tn_get e k = Some y -> WFty y) ->
 (match this with None => True | Some y => Support WW y /\ WFty y end) ->
 Later w P R Fl s' -> SigSupport WW sg.
Proof.
 intros g tm e this s sg s' H Hi Hw Hmi St Wt Se We Sthis L.
 unfold fn_sig in H. apply bind_inv in H as (ps & s1 & Hp & H).
 apply bind_inv in H as (rt & s2 & Hr & H). injection H as <- <-.
 assert (Op : forall (p : name * aty), ok (let! y := ety_of (snd p) (gs_mod g) tm e this in ret (fst p,y))).
 { intro p. apply ok_bind; [apply ok_ety_of|intro; apply ok_ret]. }
 destruct (ok_mmap _ _ _ _ Op _ _ _ Hi Hp) as [Hi1 Fp].
 assert (Or : ok (match fn_ret g with Some t => ety_of t (gs_mod g) tm e this | None => ret YUnit end)).
 { destruct (fn_ret g); [apply ok_ety_of|apply ok_ret]. }
 destruct (Or _ _ _ Hi1 Hr) as [_ Fr].
 assert (Sp : Forall2 (fun p q => Support WW (snd q)) (fn_params g) ps).
 { eapply (mmap_ok_rel w R Fl); [exact Op| |exact Hp|exact Hi|exact Hw|exact (later_back w P R Fl _ _ Fr L)].
   intros p _ si q sf E Ii Wi Li. apply bind_inv in E as (y & sy & Ey & E). injection E as <- <-.
   eapply (ety_of_support w R Fl b Hload); eassumption. }
 unfold SigSupport. cbn [sg_params sg_ret sg_this]. split.
 - clear -Sp. induction Sp; constructor; auto.
 - split.
   + destruct (fn_ret g) as [t|]; [|injection Hr as <- _; constructor].
     eapply (ety_of_support w R Fl b Hload); try eassumption.
     exact (fr_w' w _ _ Fp Hw).
   + destruct this; [exact (proj1 Sthis)|exact I].
Qed.
End Source.

Lemma env_support_set_app : forall W e k a stamp,
 EnvSupport W e -> EnvSupport W (set_app e k a stamp).
Proof. intros W e k a stamp [T U V F M L]; constructor; assumption. Qed.
Lemma setdefault_support : forall W e k a s e' s',
 EnvSupport W e -> setdefault_app e k a s = Good(e',s') -> EnvSupport W e'.
Proof.
 intros W e k a s e' s' S H. unfold setdefault_app in H. destruct (get_app e k).
 - injection H as <- _. exact S.
 - apply bind_inv in H as (stamp & si & E & H). injection H as <- _.
   apply env_support_set_app. exact S.
Qed.
Lemma tymap_support : forall W e,
 EnvSupport W e -> Forall (fun p => Support W (snd p)) (tymap e).
Proof.
 intros W e H. unfold tymap. apply Forall_map. rewrite Forall_forall.
 intros [k [y stamp]] Hin. exact (@es_tn W e H k (y,stamp) Hin).
Qed.

Lemma context_tymap_wf : forall e,
 ContextClean.ContextClean e -> Forall (fun p => WFty (snd p)) (tymap e).
Proof.
 intros e C. eapply Forall_impl; [|exact (CanonTypes.tymap_ok e (ContextClean.cc_types e C))].
 intros p O. exact (ety_ok_wf _ O).
Qed.
Lemma support_tm_lookup : forall W tm k y,
 Forall (fun p => Support W (snd p)) tm -> nassoc k tm = Some y -> Support W y.
Proof.
 intros W tm k y H E. rewrite Forall_forall in H.
 exact (H (k,y) (@support_nassoc_in ety k tm y E)).
Qed.
