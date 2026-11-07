(* Nominal schemas keep their source parameter keys. Immutable lowering
   renames the argument meanings, not the nominal constructor interface. *)
From Stdlib Require Import List.
From NM Require Import CoreMoss Specialization MergeWf ReifyFormation ReifyFresh.
Import ListNotations.
Definition nominal_type_parameters(Sg:gsig):Prop:=
 forall n D payload,g_tag Sg n=Some(D,payload)->exists xs,D=map ITy xs /\NoDup xs.
Lemma nominal_heads_renaming : forall members source_sigma target_sigma rho,
 Forall is_nom members->heads_of target_sigma(map(app_subst rho)members)=heads_of source_sigma members.
Proof.
 intros members source_sigma target_sigma rho H;induction H as[|t rest Nom Rest IH];[reflexivity|].
 destruct Nom as[n[theta ->]];cbn;f_equal;exact IH.
Qed.
Lemma nominal_members_renaming : forall members rho,
 Forall is_nom members->Forall is_nom(map(app_subst rho)members).
Proof.
 intros members rho H;induction H as[|t rest Nom Rest IH];cbn;[constructor|].
 constructor;[|exact IH]. destruct Nom as[n[theta ->]];exists n,(map(fun p=>(fst p,app_subst rho(snd p)))theta);reflexivity.
Qed.
Lemma wf_type_renaming : forall source target Phi Psi rho tau,
 nominal_type_parameters source->
 (forall n,g_tag target n=g_tag source n)->
 (forall n,g_opaque target n=g_opaque source n)->
 (forall t,avail Phi(ITy t)->wf_ty target Psi(app_subst rho(TAbs t)))->
 wf_ty source Phi tau->wf_ty target Psi(app_subst rho tau).
Proof.
 intros source target Phi Psi rho tau Parameters Tags Opaque Types Well.
 induction Well using wf_ty_ind'.
 - constructor.
 - apply Types;exact H.
 - destruct(Parameters n D tau0 H)as[xs[-> Unique]].
   rewrite app_subst_nom;unfold map_snd;eapply WfTag.
   + rewrite Tags;exact H.
   + apply type_telescope_sat;[exact Unique|].
     rewrite map_map;cbn.
     destruct H0 as[PhiD[Tele[Domain Rest]]].
     pose proof(distinct_slot_telescope_context(map ITy xs))as Exact.
     assert(N:NoDup(map reify_item_slot(map ITy xs))).
     { rewrite map_map;change(NoDup(map(fun x=>x)xs));rewrite map_id;exact Unique. }
     specialize(Exact N);rewrite Exact in Tele;inversion Tele;subst PhiD.
     cbn[cA]in Domain;rewrite type_telescope_requirements in Domain;exact Domain.
   + apply Forall_forall;intros p Member.
     apply in_map_iff in Member as[q[<- Present]].
     apply Forall_forall with(x:=q)in H2;[exact H2|exact Present].
 - rewrite app_subst_union;apply WfUnion.
   + apply Forall_forall;intros t Member;apply in_map_iff in Member as[raw[<- Present]].
     apply Forall_forall with(x:=raw)in H0;[exact H0|exact Present].
   + apply nominal_members_renaming;exact H1.
   + rewrite(nominal_heads_renaming _ (cS Phi)(cS Psi)rho H1);exact H2.
 - rewrite app_subst_nom;unfold map_snd;apply WfRec.
   + rewrite Tags;exact H.
   + apply Forall_forall;intros p Member.
     apply in_map_iff in Member as[q[<- Present]].
     apply Forall_forall with(x:=q)in H1;[exact H1|exact Present].
 - rewrite app_subst_nom;eapply WfOpaque.
   + rewrite Tags;exact H.
   + rewrite Opaque;exact H0.
   + eapply sat_typeonly_between;[exact H2|exact H1|]. apply MergeWf.map_fst_map_snd.
   + exact H2.
   + unfold map_snd;apply Forall_forall;intros p Member.
     apply in_map_iff in Member as[q[<- Present]].
     apply Forall_forall with(x:=q)in H4;[exact H4|exact Present].
Qed.

Definition interpretation_respects_context(Phi Psi:ctx)(rho:subst):Prop:=
 forall t,app_subst(cS Psi)(app_subst rho(TAbs t))=
   app_subst(cS Psi)(app_subst rho(app_subst(cS Phi)(TAbs t))).
Lemma interpretation_respects_types : forall Phi Psi rho,
 interpretation_respects_context Phi Psi rho->forall tau,
 app_subst(cS Psi)(app_subst rho tau)=
   app_subst(cS Psi)(app_subst rho(app_subst(cS Phi)tau)).
Proof.
 intros Phi Psi rho Respects tau.
 rewrite !app_subst_compose.
 apply dentry_agree_retype_aux1;intros t _.
 rewrite <-!app_subst_compose;apply Respects.
Qed.
Lemma interpreted_type_equality : forall Phi Psi rho a b,
 interpretation_respects_context Phi Psi rho->ty_eq(cS Phi)a b->
 ty_eq(cS Psi)(app_subst rho a)(app_subst rho b).
Proof.
 intros Phi Psi rho a b Respects Equal;unfold ty_eq in *.
 rewrite(interpretation_respects_types _ _ _ Respects a),
   (interpretation_respects_types _ _ _ Respects b),Equal;reflexivity.
Qed.
Lemma subtyping_renaming : forall Phi Psi rho a b,
 interpretation_respects_context Phi Psi rho->subty Phi a b->
 subty Psi(app_subst rho a)(app_subst rho b).
Proof.
 intros Phi Psi rho a b Respects Sub;destruct Sub as[a b Equal|n theta members member Present Equal].
 - apply SubEq;eapply interpreted_type_equality;eassumption.
 - rewrite app_subst_nom,app_subst_union;eapply SubInj with(tau':=app_subst rho member).
   + apply in_map;exact Present.
   + change(ty_eq(cS Psi)(app_subst rho(TNom n theta))(app_subst rho member)).
     eapply interpreted_type_equality;eassumption.
Qed.
