(* Runtime provision keys read the actual checked instantiation table. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound EtyTyExec CanonTypes Merge.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque [Nat.of_num_uint FUEL ety_ty canon].

Definition key_theta_step (e : env) (q : nat * option ety) : M subst :=
 match snd q with Some y => let! t := ety_ty (canon e y) in ret [(fst q,t)]
 | None => ret [] end.
Lemma ok_key_theta_step : forall e q, ok (key_theta_step e q).
Proof.
 intros e [n [t|]]; unfold key_theta_step; cbn [snd fst].
 - apply ok_bind; [apply ok_ety_ty|intro; apply ok_ret].
 - apply ok_ret.
Qed.

Lemma ok_key_theta_frame : forall e k, ok (key_theta e k).
Proof.
 intros e k; unfold key_theta; destruct (get_app e k) as [[a stamp]|].
 - apply ok_bind; [apply ok_mmap; intro q; exact (ok_key_theta_step e q)|intro;apply ok_ret].
 - apply ok_ret.
Qed.
Lemma ok_key_item_at_frame : forall e k, ok (key_item_at e k).
Proof.
 intros e [t|v|r m|u]; unfold key_item_at; try (unfold key_item; apply ok_ret).
 apply ok_bind; [apply ok_ety_ty|intro].
 apply ok_bind; [apply ok_key_theta_frame|intro;apply ok_ret].
Qed.

Lemma method_key_value : forall e mi r m s k s',
 method_key e mi r m s = Good(k,s') -> k = KyMth (canon e r) (gs_id m) /\ s'=s.
Proof.
 intros e mi r m s k s' H; unfold method_key in H.
 destruct (head (canon e r)), (gs_recv m);
 try (injection H as <- <-; split;reflexivity).
 exfalso; apply bind_inv in H as (v & sx & Hshow & Hbad).
 exact (err_not_good _ _ _ _ _ _ Hbad).
Qed.

Lemma literal_sig_ok_aa : forall e r args sg,
 literal_sig_ok e r (map AA args) sg = true.
Proof. intros e r [|a [|b args]] sg; reflexivity. Qed.

Lemma provided_call_direct_exec : forall fuel c e ox r m args sg s x t s',
 get_mth e (canon e r) (gs_id m) = Some sg ->
 provided_call (S fuel) c e ox r m (map AA args) s = Good((x,t),s') ->
 exists xs s1 it s2 u,
 check_args fuel c e (gs_name m) sg (map AA args) s = Good(xs,s1) /\
 key_item_at e (KyMth (canon e r) (gs_id m)) s1 = Good(it,s2) /\
 layout_ok e (sg_ret sg) s2 = Good(u,s') /\
 x = XMeth ox (gs_id m) it xs /\ t = sg_ret sg.
Proof.
 intros fuel c e ox r m args sg s x t s' Lookup H.
 cbn [provided_call] in H.
 apply bind_inv in H as (k & sk & Hkey & H).
 destruct (@method_key_value e (cx_mod c) r m s k sk Hkey) as [-> ->].
 rewrite Lookup in H.
 apply bind_inv in H as (lit & sl & Hlit & H).
 unfold check_literal_sig in Hlit; rewrite literal_sig_ok_aa in Hlit.
 injection Hlit as <- <-.
 apply bind_inv in H as (xs & s1 & Hargs & H).
 apply bind_inv in H as (it & s2 & Hitem & H).
 apply bind_inv in H as (u & s3 & Hlayout & Hret).
 assert (Et : (match map AA args with [AB _ _] => r | _ => sg_ret sg end) = sg_ret sg).
 { destruct args as [|a [|b args]]; reflexivity. }
 rewrite Et in Hret; injection Hret as <- <- <-.
 exists xs,s1,it,s2,u; auto.
Qed.

Lemma literal_sig_ab_info : forall e mi m T oy sg s u s',
 check_literal_sig e mi m T [AB oy T] sg s=Good(u,s') ->
 exists pn pty, sg_params sg=[(pn,pty)] /\ canon e pty=canon e T /\ canon e(sg_ret sg)=canon e T.
Proof.
 intros e mi m T oy sg s u s' H; unfold check_literal_sig in H.
 destruct (literal_sig_ok e T [AB oy T] sg) eqn:Ok; [|exfalso;exact(err_not_good _ _ _ _ _ _ H)].
 unfold literal_sig_ok in Ok; destruct (sg_params sg) as [|[pn pty] [|p ps]] eqn:Ps; try discriminate.
 apply andb_true_iff in Ok as [Ok Ret]; apply andb_true_iff in Ok as [_ Param].
 exists pn,pty; repeat split; try reflexivity; apply ety_eqb_true; assumption.
Qed.

Lemma provided_call_direct_ab_exec : forall fuel c e ox T oy m sg s x t s',
 get_mth e (canon e T) (gs_id m)=Some sg ->
 provided_call (S fuel) c e ox T m [AB oy T] s=Good((x,t),s') ->
 exists ul sl xs s1 it s2 u,
 check_literal_sig e (cx_mod c) m T [AB oy T] sg s=Good(ul,sl) /\
 check_args fuel c e (gs_name m) sg [AB oy T] sl=Good(xs,s1) /\
 key_item_at e (KyMth (canon e T)(gs_id m)) s1=Good(it,s2) /\
 layout_ok e (sg_ret sg) s2=Good(u,s') /\ x=XMeth ox (gs_id m) it xs /\ t=T.
Proof.
 intros fuel c e ox T oy m sg s x t s' Lookup H; cbn [provided_call] in H.
 apply bind_inv in H as (k & sk & Hkey & H).
 destruct (@method_key_value e (cx_mod c) T m s k sk Hkey) as [-> ->].
 rewrite Lookup in H.
 apply bind_inv in H as (ul & sl & Hlit & H).
 apply bind_inv in H as (xs & s1 & Hargs & H).
 apply bind_inv in H as (it & s2 & Hitem & H).
 apply bind_inv in H as (u & s3 & Hlayout & Hret); injection Hret as <- <- <-.
 exists ul,sl,xs,s1,it,s2,u; repeat split; assumption || reflexivity.
Qed.

Section Keys.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Definition read_known (e : env) (a : Elab.inst) : subst :=
 flat_map (fun q => match snd q with Some y => [(fst q,tr Fl (canon e y))] | None => [] end) a.

Lemma key_theta_loop : forall a e s l s',
 mmap (key_theta_step e) a s = Good(l,s') -> Inv s -> Later w P R Fl s' ->
 List.concat l = read_known e a.
Proof.
 induction a as [|[n [t|]] a IH]; intros e s l s' H Hi L;
 cbn [mmap] in H.
 - injection H as <- _; reflexivity.
 - apply bind_inv in H as (v & s1 & Hhead & H).
   apply bind_inv in H as (vs & s2 & Htail & Hret).
   inversion Hret; subst l s2.
   destruct (@ok_key_theta_step e (n,Some t) s v s1 Hi Hhead) as [Hi1 F1].
   pose proof (proj2 (@ok_mmap (nat * option ety)%type subst (key_theta_step e) a (ok_key_theta_step e) s1 vs s' Hi1 Htail)) as Ft.
   unfold key_theta_step in Hhead; cbn [snd fst] in Hhead.
   apply bind_inv in Hhead as (ty & sx & Hty & Hret1).
   injection Hret1 as <- <-.
   rewrite (ety_ty_tr w P R Fl _ _ _ _ Hty Hi (later_back w P R Fl _ _ Ft L)).
   cbn [List.concat read_known flat_map fst snd].
   rewrite (IH e sx vs s' Htail Hi1 L); reflexivity.
 - apply bind_inv in H as (v & s1 & Hhead & H).
   apply bind_inv in H as (vs & s2 & Htail & Hret).
   unfold key_theta_step in Hhead; cbn in Hhead; injection Hhead as <- <-.
   inversion Hret; subst l s2.
   cbn [List.concat read_known flat_map fst snd].
   exact (IH e s vs s' Htail Hi L).
Qed.

Lemma key_theta_tr : forall e k s th s',
 key_theta e k s = Good(th,s') -> Inv s -> Later w P R Fl s' ->
 th = match get_app e k with Some(a,_) => read_known e a | None => [] end.
Proof.
 intros e k s th s' H Hi L; unfold key_theta in H.
 destruct (get_app e k) as [[a stamp]|]; [|injection H as <- _;reflexivity].
 apply bind_inv in H as (l & sx & Hmap & Hret); injection Hret as <- <-.
 exact (@key_theta_loop a e s l sx Hmap Hi L).
Qed.

Definition read_inst (a : Elab.inst) : Audit.Elaboration.inst :=
 map (fun q => (fst q,option_map (tr Fl) (snd q))) a.

Lemma read_known_ctx : forall e C a,
 Audit.Elaboration.c_sub C = suf Fl (en_uf e) -> Tri (en_uf e) ->
 read_known e a = Audit.Elaboration.theta_of C (read_inst a).
Proof.
 intros e C a Sub TriU; induction a as [|[n [t|]] a IH];
 unfold read_known, read_inst, Audit.Elaboration.theta_of, Audit.Elaboration.known in *;
 cbn [flat_map map fst snd option_map List.app] in *;
 rewrite ?IH; try reflexivity.
 unfold Audit.Elaboration.canon; rewrite Sub, (tr_canon Fl e t TriU); reflexivity.
Qed.

Lemma key_item_at_tr : forall e C r m s it s',
 key_item_at e (KyMth r m) s = Good(it,s') -> Inv s -> Later w P R Fl s' ->
 Audit.Elaboration.c_sub C = suf Fl (en_uf e) -> Tri (en_uf e) ->
 Audit.Elaboration.ilook (Audit.Elaboration.mkey (tr Fl r) m) (Audit.Elaboration.c_apps C) =
 option_map (fun p => read_inst (fst p)) (get_app e (KyMth r m)) ->
 it = Audit.Elaboration.rkey C (tr Fl r) m.
Proof.
 intros e C r m s it s' H Hi L Sub TriU Apps.
 unfold key_item_at in H.
 apply bind_inv in H as (rt & s1 & Hrt & H).
 apply bind_inv in H as (th & s2 & Hth & Hret); injection Hret as <- <-.
 destruct (ok_ety_ty r s rt s1 Hi Hrt) as [Hi1 F1].
 assert (Kok : ok (key_theta e (KyMth r m))).
 { unfold key_theta; destruct (get_app e (KyMth r m)) as [[a stamp]|];
   [apply ok_bind; [apply ok_mmap; intro q; exact (ok_key_theta_step e q)|intro;apply ok_ret]|apply ok_ret]. }
 pose proof (proj2 (Kok s1 th s2 Hi1 Hth)) as F2.
 rewrite (ety_ty_tr w P R Fl _ _ _ _ Hrt Hi (later_back w P R Fl _ _ F2 L)).
 rewrite (@key_theta_tr e (KyMth r m) s1 th s2 Hth Hi1 L).
 unfold Audit.Elaboration.rkey; rewrite Apps.
 destruct (get_app e (KyMth r m)) as [[a stamp]|]; cbn [option_map fst]; [|reflexivity].
 rewrite (read_known_ctx e C a Sub TriU); reflexivity.
Qed.
End Keys.
