(* Ordinary calls of a method already provided in the checked context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import InfoPreserve ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabMono BodySoundCore EtyTyExec
 CanonTypes NominalSupport TySupport SourceSupport CallArgs MethodKeyTr.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque [Nat.of_num_uint FUEL ety_ty canon].
Module PD := ExecutionModel.Elaboration.
Section Direct.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySoundCore.W w R Fl).
Notation tc := (fun e t => tr Fl (canon e t)).
Lemma provided_direct_step : forall fuel, OK w P R Fl fuel -> forall c e ox r m args sg s x t s' F C,
 get_mth e (canon e r) (gs_id m) = Some sg ->
 provided_call (S fuel) c e ox r m (map AA args) s = Good((x,t),s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> ety_ok(canon e r)=true ->
 PD.ilook (PD.mkey (tc e r) (gs_id m)) (PD.c_apps C) =
 option_map (fun p => read_inst Fl (fst p)) (get_app e (KyMth (canon e r) (gs_id m))) ->
 Post w P R Fl s' F ->
 PD.Provided WW P F C ox (tc e r) (gs_id m) args x (tc e t) /\ WFty t /\ Support WW t.
Proof.
 intros fuel HOK c e ox r m args sg s x t s' F C Lookup H Hi Hii Hfi Hc Clean Apps Hp.
 destruct (@provided_call_direct_exec fuel c e ox r m args sg s x t s' Lookup H)
  as (xs & s1 & it & s2 & u & Hargs & Hitem & Hlayout & -> & ->).
 destruct (ok_check_args fuel c e (gs_name m) sg (map AA args) s xs s1 Hi Hargs) as [Hi1 F1].
 destruct (@ok_key_item_at_frame e (KyMth (canon e r) (gs_id m)) s1 it s2 Hi1 Hitem) as [Hi2 F2].
 destruct (ok_layout_ok e (sg_ret sg) s2 u s' Hi2 Hlayout) as [Hi3 F3].
 pose proof (post_back w P R Fl _ _ F F3 Hp) as Hp2.
 pose proof (post_back w P R Fl _ _ F F2 Hp2) as Hp1.
 pose proof (cr_mths_clean _ _ _ _ _ _ _ Hc _ _ _ Lookup) as Sc.
 destruct Sc as [Wps [Wret Wthis]].
 assert (Wp : Forall (fun p => WFty(snd p)) (sg_params sg)).
 { apply Forall_forall; intros p InP; apply ety_ok_wf.
   exact (proj1 (Forall_forall _ _) Wps (snd p) (in_map snd _ _ InP)). }
 pose proof (ety_ok_wf _ Wret) as Wr.
 destruct (@es_mths WW e (cr_support _ _ _ _ _ _ _ Hc) _ _ _ Lookup) as [_ [Sp [Sr Sthis]]].
 split; [|auto].
 pose proof (@check_args_aa_ok w P R Fl fuel HOK c e (gs_name m) sg args s xs s1 F C) as Args.
 assert (Both : Forall (fun p => WFty(snd p) /\ Support WW(snd p)) (sg_params sg)).
 { apply Forall_forall; intros p InP; split;
   [exact (proj1 (Forall_forall _ _) Wp p InP)|exact (proj1 (Forall_forall _ _) Sp p InP)]. }
 specialize (Args Both Hargs Hi Hii Hfi Hc Hp1).
 rewrite (@key_item_at_tr w P R Fl e C (canon e r) (gs_id m) s1 it s2 Hitem Hi1 (proj1 Hp2)
  (cr_sub _ _ _ _ _ _ _ Hc) (proj1(cr_uf _ _ _ _ _ _ _ Hc)) Apps).
 eapply PD.PrIn with (g:=PD.FSig (map (fun p => tc e (snd p)) (sg_params sg)) (tc e (sg_ret sg))); [|exact Args].
 rewrite (cr_mths _ _ _ _ _ _ _ Hc r (gs_id m) Clean), Lookup; reflexivity.
Qed.
Lemma provided_direct_sound : forall fuel, OK w P R Fl fuel -> forall c e ox r m args sg s x t s' F C,
 get_mth e (canon e r) (gs_id m) = Some sg ->
 provided_call fuel c e ox r m (map AA args) s = Good((x,t),s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> ety_ok(canon e r)=true ->
 PD.ilook (PD.mkey (tc e r) (gs_id m)) (PD.c_apps C) =
 option_map (fun p => read_inst Fl (fst p)) (get_app e (KyMth (canon e r) (gs_id m))) ->
 Post w P R Fl s' F ->
 PD.Provided WW P F C ox (tc e r) (gs_id m) args x (tc e t) /\ WFty t /\ Support WW t.
Proof.
 intros fuel HOK c e ox r m args sg s x t s' F C Lookup H Hi Hii Hfi Hc Clean Apps Hp.
 pose proof (mono_group fuel) as Mon; do 15 (apply proj2 in Mon); apply proj1 in Mon.
 eapply (@provided_direct_step fuel HOK c e ox r m args sg s x t s' F C Lookup);
 [exact (Mon c e ox r m (map AA args) s ((x,t),s') H)|eassumption..].
Qed.

Variable b : bundle.
Hypothesis Hload : load_world b=Good w.
Theorem provided_direct_from_context : forall fuel, OK w P R Fl fuel ->
 forall c e ox r m args sg s x t s' F C,
 get_mth e (canon e r) (gs_id m)=Some sg ->
 provided_call fuel c e ox r m (map AA args) s=Good((x,t),s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> WFty r -> Support WW r -> Post w P R Fl s' F ->
 PD.Provided WW P F C ox (tc e r) (gs_id m) args x (tc e t) /\ WFty t /\ Support WW t.
Proof.
 intros fuel HOK c e ox r m args sg s x t s' F C Lookup H Hi Hii Hfi Hc Wr Sr Hp.
 assert (Clean : ety_ok(canon e r)=true).
 { apply (@support_clean WW (canon e r)).
   - exact (loaded_record_unowned w R Fl b Hload).
   - apply canon_support; [exact (@es_uf WW e (cr_support _ _ _ _ _ _ _ Hc))|exact Sr].
   - exact (canon_wf e r (proj2(cr_uf _ _ _ _ _ _ _ Hc)) Wr). }
 eapply provided_direct_sound; [exact HOK|exact Lookup|exact H|exact Hi|exact Hii|exact Hfi|exact Hc|exact Clean| |exact Hp].
 exact (cr_apps _ _ _ _ _ _ _ Hc (canon e r) (gs_id m) Clean).
Qed.

Definition ProvidedFamilyStep (fuel : nat) : Prop :=
 forall c e ox r m args s x t s',
 provided_call fuel c e ox r m (map AA args) s=Good((x,t),s') ->
 forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> WFty r -> Support WW r ->
 get_mth e (canon e r) (gs_id m)=None ->
 PD.Provided WW P F C ox (tc e r) (gs_id m) args x (tc e t) /\ WFty t /\ Support WW t.

Theorem provided_step : forall fuel, OK w P R Fl fuel -> ProvidedFamilyStep fuel ->
 forall c e ox r m args s x t s',
 provided_call fuel c e ox r m (map AA args) s=Good((x,t),s') ->
 forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> WFty r -> Support WW r ->
 PD.Provided WW P F C ox (tc e r) (gs_id m) args x (tc e t) /\ WFty t /\ Support WW t.
Proof.
 intros fuel HOK Family c e ox r m args s x t s' H F C Hi Hii Hfi Hc Hp Wr Sr.
 destruct (get_mth e (canon e r) (gs_id m)) as [sg|] eqn:Lookup.
 - eapply provided_direct_from_context; eassumption.
 - eapply Family; eassumption.
Qed.
End Direct.
