(* Declaration support and complete nominal type arguments, independent of
   shape validity. This property is preserved by canonical type resolution. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab BodyTr.
Import ListNotations.
Set Implicit Arguments.
Module SA := ExecutionModel.Elaboration.

Inductive Support (W : SA.world) : ety -> Prop :=
| SupportUnit : Support W YUnit
| SupportNever : Support W YNever
| SupportAbs : forall g, Support W (YAbs g)
| SupportNom : forall n xs,
    (SA.kind_sym W n = Some SA.DKUnit \/ SA.kind_sym W n = Some SA.DKTag) ->
    map fst xs = SA.w_reqs W n ->
    Forall (fun p => Support W (snd p)) xs -> Support W (YNom n xs)
| SupportUnion : forall ms, Forall (Support W) ms -> Support W (YUnion ms)
| SupportRec : forall fs, Forall (fun p => Support W (snd p)) fs -> Support W (YRec fs).

Definition UfSupport W (uf : list (nat * ety)) := forall g t, In (g,t) uf -> Support W t.
Definition SigSupport W (sg : Elab.fsig) :=
 Forall (fun p => Support W (snd p)) (sg_params sg) /\ Support W (sg_ret sg) /\
 match sg_this sg with None => True | Some t => Support W t end.
Record EnvSupport W (e : env) : Prop := {
 es_tn : forall k p, In (k,p) (en_tn e) -> Support W (fst p);
 es_uf : UfSupport W (en_uf e);
 es_vals : forall k t, nget (en_vals e) k = Some t -> Support W t;
 es_fns : forall k sg, nget (en_fns e) k = Some sg -> SigSupport W sg;
 es_mths : forall r m sg, get_mth e r m = Some sg -> Support W r /\ SigSupport W sg;
 es_locals : forall n p, sassoc n (en_locals e) = Some p -> Support W (fst p)
}.

Lemma support_nominal : forall W n xs, Support W (YNom n xs) ->
 (SA.kind_sym W n = Some SA.DKUnit \/ SA.kind_sym W n = Some SA.DKTag) /\
 map fst xs = SA.w_reqs W n /\ Forall (fun p => Support W (snd p)) xs.
Proof. intros W n xs H; inversion H; subst; auto. Qed.

Lemma support_nassoc_in : forall V k (l : list (nat * V)) v,
 nassoc k l = Some v -> In (k,v) l.
Proof.
 intros V k l v; induction l as [|[a w] r IH]; cbn; [discriminate|].
 destruct (Nat.eqb_spec k a) as [->|]; [intros [=<-]; left; reflexivity|intro H; right; exact (IH H)].
Qed.

Lemma canon_go_support : forall W uf, UfSupport W uf -> forall fuel t,
 Support W t -> Support W (canon_go fuel uf t).
Proof.
 intros W uf U fuel; induction fuel as [|fuel IHf]; intros t Ht; [exact Ht|].
 cbn [canon_go]; revert Ht.
 induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intro Ht.
 - constructor.
 - constructor.
 - destruct (nassoc g uf) as [t'|] eqn:E; [|constructor].
   destruct (ety_eqb t' (YAbs g)); [constructor|]; apply IHf.
   apply (U g t'), (@support_nassoc_in ety g uf t' E).
 - inversion Ht; subst. eapply SupportNom; [eassumption| |].
   + rewrite map_map; change (map fst xs = SA.w_reqs W n); assumption.
   + apply Forall_map; rewrite Forall_forall in *; intros p Hp; apply IH; auto.
 - inversion Ht; subst. constructor; apply Forall_map; rewrite Forall_forall in *; auto.
 - inversion Ht; subst. constructor; apply Forall_map; rewrite Forall_forall in *.
   intros p Hp; cbn [snd]; apply IH; auto.
Qed.
Lemma canon_support : forall W e t, UfSupport W (en_uf e) -> Support W t -> Support W (canon e t).
Proof. intros; apply canon_go_support; auto. Qed.

Lemma tn_get_support : forall W e t y,
 EnvSupport W e -> tn_get e t = Some y -> Support W y.
Proof.
 intros W e t y E H; unfold tn_get in H.
 destruct (nassoc t (en_tn e)) as [p|] eqn:Ep; [|discriminate].
 inversion H; subst; apply (es_tn E t p); eapply support_nassoc_in; exact Ep.
Qed.
Lemma inst_go_support : forall W e, EnvSupport W e -> forall fuel t,
 Support W t -> Support W (inst_go fuel e t).
Proof.
 intros W e E fuel; induction fuel as [|fuel IH]; intros t H; [exact H|].
 destruct t as [| |g|n xs|ms|fs]; cbn [inst_go].
 - constructor.
 - constructor.
 - destruct (tn_get e g) as [y|] eqn:Ey; apply canon_support; [exact (es_uf E)| |exact (es_uf E)|constructor].
   eapply tn_get_support; eauto.
 - inversion H; subst; eapply SupportNom; [eassumption| |].
   + rewrite map_map; change (map fst xs = SA.w_reqs W n); assumption.
   + apply Forall_map; eapply Forall_impl; [|eassumption]; intros p Hp; cbn [snd]; apply IH; exact Hp.
 - inversion H; subst; constructor; apply Forall_map; eapply Forall_impl; [|eassumption]; auto.
 - inversion H; subst; constructor; apply Forall_map; eapply Forall_impl; [|eassumption]; intros p Hp; cbn [snd]; apply IH; exact Hp.
Qed.
Lemma instantiate_support : forall W e t,
 EnvSupport W e -> Support W t -> Support W (instantiate e t).
Proof.
 intros W e t E. induction t as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intro H;cbn[instantiate].
 - constructor.
 - constructor.
 - destruct(tn_get e g)as[y|]eqn:Ey;apply canon_support;try exact(es_uf E);[eapply tn_get_support;eauto|constructor].
 - inversion H;subst. apply SupportNom;[assumption| |].
   + rewrite map_map. change(map fst xs=SA.w_reqs W n). assumption.
   + apply Forall_map. rewrite Forall_forall in *. intros p Hp. cbn[snd]. apply IH;auto.
 - inversion H;subst. apply SupportUnion. apply Forall_map. rewrite Forall_forall in *. auto.
 - inversion H;subst. apply SupportRec. apply Forall_map. rewrite Forall_forall in *. intros p Hp. cbn[snd]. apply IH;auto.
Qed.
