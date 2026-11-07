(* Literal arithmetic at a method already in force. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import InfoPreserve Audit NMap Sx Elab ElabSound ElabFrame ElabMono BodySoundCore BodyLit
 EtyTyExec CanonTypes NominalSupport TySupport SourceSupport MethodKeyTr.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque [Nat.of_num_uint FUEL ety_ty canon].
Module LD := Audit.Elaboration.
Section Direct.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b=Good w.
Notation WW := (BodySoundCore.W w R Fl).
Notation tc := (fun e t => tr Fl (canon e t)).
Theorem literal_direct : forall fuel c e ox T oy m sg s x t s' F C,
 get_mth e (canon e T) (gs_id m)=Some sg ->
 provided_call fuel c e ox T m [AB oy T] s=Good((x,t),s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
 LD.LitOp WW C (tc e T) (gs_id m) ox oy x.
Proof.
 intros fuel c e ox T oy m sg s x t s' F C Lookup H Hi Hii Hfi Hc Hp.
 pose proof(mono_group fuel)as Mon; do 15(apply proj2 in Mon); apply proj1 in Mon.
 pose proof(Mon c e ox T m [AB oy T] s ((x,t),s') H)as Hup.
 destruct (@provided_call_direct_ab_exec fuel c e ox T oy m sg s x t s' Lookup Hup)
  as (ul & sl & xs & s1 & it & s2 & u & Hlit & Hargs & Hitem & Hlayout & -> & ->).
 destruct (ok_check_literal_sig e (cx_mod c) m T [AB oy T] sg s ul sl Hi Hlit) as [Hil Flit].
 destruct (ok_check_args fuel c e (gs_name m) sg [AB oy T] sl xs s1 Hil Hargs) as [Hi1 Fargs].
 destruct (@ok_key_item_at_frame e (KyMth(canon e T)(gs_id m)) s1 it s2 Hi1 Hitem) as [Hi2 Fitem].
 destruct (ok_layout_ok e (sg_ret sg) s2 u s' Hi2 Hlayout) as [Hi3 Flayout].
 pose proof(post_back w P R Fl _ _ F Flayout Hp)as Hp2.
 destruct (@literal_sig_ab_info e (cx_mod c) m T oy sg s ul sl Hlit)as (pn &pty &Ps &EqP &EqR).
 destruct (cr_mths_clean _ _ _ _ _ _ _ Hc _ _ _ Lookup) as [_ [Wr _]].
 destruct (@es_mths WW e (cr_support _ _ _ _ _ _ _ Hc) _ _ _ Lookup) as [_ [_ [Sr _]]].
 assert (Clean : ety_ok(canon e T)=true).
 { rewrite <- EqR; apply (@support_clean WW (canon e(sg_ret sg))).
   - exact(loaded_record_unowned w R Fl b Hload).
   - apply canon_support; [exact(@es_uf WW e(cr_support _ _ _ _ _ _ _ Hc))|exact Sr].
   - exact(canon_wf e (sg_ret sg)(proj2(cr_uf _ _ _ _ _ _ _ Hc))(ety_ok_wf _ Wr)). }
 rewrite (@key_item_at_tr w P R Fl e C (canon e T)(gs_id m) s1 it s2 Hitem Hi1(proj1 Hp2)
  (cr_sub _ _ _ _ _ _ _ Hc)(proj1(cr_uf _ _ _ _ _ _ _ Hc))
  (cr_apps _ _ _ _ _ _ _ Hc (canon e T)(gs_id m) Clean)).
 rewrite (check_args_ab fuel c e (gs_name m) sg oy T sl xs s1 Hargs).
 apply LD.LitOpIn.
 rewrite (cr_mths _ _ _ _ _ _ _ Hc T (gs_id m) Clean),Lookup; cbn[option_map].
 rewrite Ps; cbn[map snd]; rewrite EqP,EqR; reflexivity.
Qed.

Definition LiteralFamilyStep (fuel : nat) : Prop :=
 forall c e ox T oy m s x t s',
 provided_call fuel c e ox T m [AB oy T] s=Good((x,t),s') ->
 forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->WFty T ->Support WW T ->
 get_mth e (canon e T)(gs_id m)=None -> LD.LitOp WW C (tc e T)(gs_id m) ox oy x.

Theorem literal_step : forall fuel, LiteralFamilyStep fuel ->
 forall c e ox T oy m s x t s',
 provided_call fuel c e ox T m [AB oy T] s=Good((x,t),s') ->
 forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->WFty T ->Support WW T ->
 LD.LitOp WW C (tc e T)(gs_id m) ox oy x.
Proof.
 intros fuel Family c e ox T oy m s x t s' H F C Hi Hii Hfi Hc Hp WT ST.
 destruct(get_mth e(canon e T)(gs_id m))as[sg|]eqn:Lookup.
 - eapply literal_direct; eassumption.
 - eapply Family; eassumption.
Qed.
End Direct.
