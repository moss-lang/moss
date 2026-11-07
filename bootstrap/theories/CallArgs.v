From NM Require Import BodyProofTools.
(* Sequential argument checking, from the actual recursive expression step. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import InfoPreserve Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySoundCore CanonTypes NominalSupport.
Import ListNotations.
Set Implicit Arguments.
Local Open Scope string_scope.
Module CA := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL].

Definition call_arg_step f c e nm (pa : (string * ety) * arg) : M xexpr :=
 let '((pn,pty),a) := pa in
 let! (v,t) := synth_arg (S f) c e a (Some pty) in
 if fits e t pty then ret v else
 let! x := show t in let! y := show pty in
 err (cx_mod c) ("argument `" ++ pn ++ "` of `" ++ nm ++ "` is `" ++ x ++ "`, expected `" ++ y ++ "`").
Lemma ok_call_arg_step : forall f c e nm pa, ok (call_arg_step f c e nm pa).
Proof. intros f c e nm [[pn pty]a]; unfold call_arg_step; okt. Qed.

Lemma check_args_one_aa : forall c e nm sg args s xs s',
 check_args 1 c e nm sg (map AA args) s = Good(xs,s') ->
 args=[] /\ sg_params sg=[] /\ xs=[] /\ s'=s.
Proof.
 intros c e nm sg args s xs s' H.
 cbn [check_args] in H.
 destruct (negb (Nat.eqb (List.length (map AA args)) (List.length (sg_params sg)))) eqn:Len.
 - exfalso; exact (err_not_good _ _ _ _ _ _ H).
 - apply negb_false_iff in Len; apply Nat.eqb_eq in Len; rewrite map_length in Len.
   destruct args as [|a args], (sg_params sg) as [|[pn pty] ps] eqn:Ps; cbn in Len; try discriminate.
   + cbn [map combine mmap] in H; injection H as <- <-; auto.

Qed.

Section Args.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySoundCore.W w R Fl).
Notation tc := (fun e t => tr Fl (canon e t)).
Variable f : nat.
(* This is the recursive expression induction step, restricted to an argument. *)
Hypothesis Synth : forall c e a pty s v vt s',
 synth f c e a (Some pty) s = Good((v,vt),s') ->
 forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> WFty pty -> Support WW pty -> Post w P R Fl s' F ->
 CA.Ex WW P F C a (Some(tc e pty)) v (tc e vt).

Lemma call_canon_fixed : forall c e F C, CxRel w R Fl c e F C -> forall t,
 CA.canon C (tc e t) = tc e t.
Proof.
 intros c e F C Hc t; unfold CA.canon.
 rewrite (cr_sub _ _ _ _ _ _ _ Hc).
 rewrite (tr_canon Fl e (canon e t) (proj1 (cr_uf _ _ _ _ _ _ _ Hc))).
 rewrite (canon_idem e t (proj1 (cr_uf _ _ _ _ _ _ _ Hc))); reflexivity.
Qed.

Lemma check_args_aa_loop : forall c e nm ps args s xs s' F C,
 List.length args = List.length ps ->
 Forall (fun p => WFty (snd p) /\ Support WW (snd p)) ps ->
 mmap (call_arg_step f c e nm) (combine ps (map AA args)) s = Good(xs,s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
 CA.Args WW P F C args (map (fun p => tc e (snd p)) ps) xs.
Proof.
 intros c e nm ps; induction ps as [|[pn pty] ps IH]; intros args s xs s' F C Len Wps H Hi Hii Hfi Hc Hp;
 destruct args as [|a args]; cbn in Len; try discriminate.
 - cbn [combine map mmap] in H; injection H as <- _; constructor.
 - injection Len as LenTail. cbn [combine map mmap] in H.
   apply bind_inv in H as (v & s1 & Hhead & Htail0).
   apply bind_inv in Htail0 as (vs & s2 & Htail & Hret).
   inversion Hret; subst xs s2.
   destruct (@ok_call_arg_step f c e nm ((pn,pty),AA a) s v s1 Hi Hhead) as [Hi1 Fs1].
   pose proof (proj2 (@ok_mmap ((string * ety) * arg)%type xexpr (call_arg_step f c e nm) (combine ps (map AA args)) (ok_call_arg_step f c e nm) s1 vs s' Hi1 Htail)) as Frtail.
   pose proof (post_back w P R Fl _ _ F Frtail Hp) as Hp1.
   unfold call_arg_step in Hhead.
   apply bind_inv in Hhead as ([v0 vt] & ss & Hsynth & Hhead).
   cbn [synth_arg] in Hsynth.
   destruct (fits e vt pty) eqn:Efit.
   2: { exfalso; apply bind_inv in Hhead as (x & sx & Hshow & Hhead).
        apply bind_inv in Hhead as (y & sy & Hshow' & Hbad); exact (err_not_good _ _ _ _ _ _ Hbad). }
   inversion Hhead; subst v ss.
   inversion Wps as [|p ps0 [Wpty Spty] Wtail]; subst.
   eapply CA.ArgsCons with (at' := tc e vt).
   + exact (@Synth c e a pty s v0 vt s1 Hsynth F C Hi Hii Hfi Hc Wpty Spty Hp1).
   + rewrite !(@call_canon_fixed c e F C Hc).
     exact (fits_canon_tr Fl e vt pty Efit).
   + apply IH with (s:=s1) (s':=s'); auto; first[cache_inv|fwd_inv].
Qed.
Lemma check_args_aa : forall c e nm sg args s xs s' F C,
 Forall (fun p => WFty (snd p) /\ Support WW (snd p)) (sg_params sg) ->
 check_args (S(S f)) c e nm sg (map AA args) s = Good(xs,s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
 CA.Args WW P F C args (map (fun p => tc e (snd p)) (sg_params sg)) xs.
Proof.
 intros c e nm sg args s xs s' F C Wps H Hi Hii Hfi Hc Hp.
 cbn [check_args] in H.
 destruct (negb (Nat.eqb (List.length (map AA args)) (List.length (sg_params sg)))) eqn:Len.
 - exfalso; exact (err_not_good _ _ _ _ _ _ H).
 - apply negb_false_iff in Len; apply Nat.eqb_eq in Len; rewrite map_length in Len.
   eapply check_args_aa_loop; eauto.
Qed.
End Args.

Section AllFuel.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Lemma check_args_aa_ok : forall fuel,
 OK w P R Fl fuel -> forall c e nm sg args s xs s' F C,
 Forall (fun p => WFty(snd p) /\ Support (BodySoundCore.W w R Fl)(snd p)) (sg_params sg) ->
 check_args fuel c e nm sg (map AA args) s = Good(xs,s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
 CA.Args (BodySoundCore.W w R Fl) P F C args
 (map (fun p => tr Fl (canon e (snd p))) (sg_params sg)) xs.
Proof.
 intros [|[|f]] HOK c e nm sg args s xs s' F C Wps H Hi Hii Hfi Hc Hp.
 - discriminate H.
 - destruct (@check_args_one_aa c e nm sg args s xs s' H) as (-> & Ps & -> & ->).
   rewrite Ps; constructor.
 - eapply (@check_args_aa w P R Fl f); try eassumption.
   intros c0 e0 a pty s0 v vt s0' Hs F0 C0 Hi0 Hii0 Hfi0 Hc0 Wpty Spty Hp0.
   pose proof ((proj1 (mono_group f)) c0 e0 a (Some pty) s0 ((v,vt),s0') Hs) as Hs1.
   pose proof ((proj1 (mono_group (S f))) c0 e0 a (Some pty) s0 ((v,vt),s0') Hs1) as Hs2.
   exact (proj1 (bs_synth _ _ _ _ _ HOK c0 e0 a (Some pty) s0 v vt s0' Hs2 F0 C0 Hi0 Hii0 Hfi0 Hc0 Wpty Spty Hp0)).
Qed.
End AllFuel.
