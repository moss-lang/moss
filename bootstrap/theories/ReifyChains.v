(* Runtime functor links retain their actual ordered maps and fixed meanings. *)
From Stdlib Require Import List.
From NM Require Import Audit CoreMoss ReifyMaps.
Import ListNotations.
Set Implicit Arguments.

Inductive reify_chain (Sg:gsig)(Finst:usym->subst) : subst -> usym -> list smap -> subst -> usym -> Prop :=
| RcId : forall g u, reify_chain Sg Finst g u [] g u
| RcInterface : forall gt u gm v sms gs h,
 identity_functor_schema Sg Finst gt u gm v -> reify_chain Sg Finst gm v sms gs h ->
 reify_chain Sg Finst gt u sms gs h
| RcRebase : forall gt gt' u sms gs h,
 (forall t,In t(g_fixed Sg u)->fixed_interpretation Finst u gt' t=fixed_interpretation Finst u gt t) ->
 reify_chain Sg Finst gt u sms gs h ->reify_chain Sg Finst gt' u sms gs h
| RcLink : forall gt u gm v sms gs h Du Dv Dr sm,
 g_fun Sg u=Some(Du,Dr,None) ->g_fun Sg v=Some(Dv,Dr,None) ->
 functor_link Sg Finst gt u gm v sm(identity_smap Dr) ->
 reify_chain Sg Finst gm v sms gs h -> reify_chain Sg Finst gt u(sms++[sm])gs h.

Lemma identity_schema_fixed : forall Sg Finst gt u gs v,
 identity_functor_schema Sg Finst gt u gs v ->forall s,In s(g_fixed Sg v)->
 exists t,In t(g_fixed Sg u) /\ fixed_interpretation Finst u gt t=fixed_interpretation Finst v gs s.
Proof. intros Sg Finst gt u gs v H;destruct H;assumption. Qed.
Lemma functor_link_fixed : forall Sg Finst gt u gs v inputs outputs,
 functor_link Sg Finst gt u gs v inputs outputs ->forall s,In s(g_fixed Sg v)->
 exists t,In t(g_fixed Sg u) /\ fixed_interpretation Finst u gt t=fixed_interpretation Finst v gs s.
Proof. intros Sg Finst gt u gs v inputs outputs H;destruct H;assumption. Qed.

Theorem reify_chain_fixed : forall Sg Finst gt u sms gs h,
 reify_chain Sg Finst gt u sms gs h ->forall s,In s(g_fixed Sg h)->
 exists t,In t(g_fixed Sg u) /\ fixed_interpretation Finst u gt t=fixed_interpretation Finst h gs s.
Proof.
 intros Sg Finst gt u sms gs h H;induction H;intros s Hin.
 - exists s;auto.
 - destruct(IHreify_chain s Hin)as[t[Ht Eq]].
   destruct(identity_schema_fixed H t Ht)as[a[Ha Ea]].
   exists a;split;[exact Ha|congruence].
 - destruct(IHreify_chain s Hin)as[t[Ht Eq]].
   exists t;split;[exact Ht|rewrite(H t Ht);exact Eq].
 - destruct(IHreify_chain s Hin)as[t[Ht Eq]].
   destruct(functor_link_fixed H1 t Ht)as[a[Ha Ea]].
   exists a;split;[exact Ha|congruence].
Qed.

Lemma identity_schema_declarations : forall Sg Finst gt u gs v,
 identity_functor_schema Sg Finst gt u gs v ->forall Du Dr,
 g_fun Sg u=Some(Du,Dr,None)->g_fun Sg v=Some(Du,Dr,None).
Proof. intros Sg Finst gt u gs v H;destruct H;intros Du Dr0 E;rewrite H in E;injection E as <- <-;assumption. Qed.

Theorem reify_chain_results : forall Sg Finst gt u sms gs h,
 reify_chain Sg Finst gt u sms gs h ->forall Du Dr,
 g_fun Sg u=Some(Du,Dr,None)->exists Dh,g_fun Sg h=Some(Dh,Dr,None).
Proof.
 intros Sg Finst gt u sms gs h H;induction H;intros Du0 Dr0 Decl.
 - eauto.
 - eapply IHreify_chain;exact(identity_schema_declarations H Decl).
 - eapply IHreify_chain;exact Decl.
 - rewrite H in Decl;injection Decl as <- <-.
   eapply IHreify_chain;exact H0.
Qed.

Lemma identity_schema_link : forall Sg Finst gt u gs v,
 identity_functor_schema Sg Finst gt u gs v ->exists D Dr,
 g_fun Sg u=Some(D,Dr,None) /\ g_fun Sg v=Some(D,Dr,None) /\
 functor_link Sg Finst gt u gs v(identity_smap D)(identity_smap Dr).
Proof.
 intros Sg Finst gt u gs v H;destruct H as[gt u gs v D Dr Dt Ds Nd Nr Fixed Total Future].
 exists D,Dr;split;[exact Dt|]. split;[exact Ds|].
 econstructor;try eassumption;rewrite identity_smap_domain;assumption || reflexivity.
Qed.

Lemma functor_link_instantiation : forall Sg Finst gt u gs v inputs Du Dv Dr gin,
 functor_link Sg Finst gt u gs v inputs(identity_smap Dr) ->
 g_fun Sg u=Some(Du,Dr,None) ->g_fun Sg v=Some(Dv,Dr,None) ->
 schema_input_ground Sg Finst u gt Du gin ->exists gm,
 schema_input_ground Sg Finst v gs Dv gm /\
 full_map_ok Sg(identity_functor_schema Sg Finst)gm gin Dv Du inputs /\
 (forall gmout,schema_result_ground Dv Dr gm gmout ->exists gout,
  schema_result_ground Du Dr gin gout /\
  full_map_ok Sg(identity_functor_schema Sg Finst)gout gmout Dr Dr(identity_smap Dr)).
Proof.
 intros Sg Finst gt u gs v inputs Du Dv Dr gin Link Dt Ds Input.
 destruct Link as[gt u gs v Du0 Dru Dv0 Drv inputs outputs
  DeclT DeclS DomI DomO NI NO Fixed Total Future].
 rewrite Dt in DeclT;injection DeclT as <- <-.
 rewrite Ds in DeclS;injection DeclS as <- <-.
 destruct(Total gin Input)as[gm[Mid Equal]].
 destruct(Future gin gm Input Mid Equal)as[Map Out].
 exists gm;split;[exact Mid|]. split;[exact Map|].
 intros gmout Legal;destruct(Out gmout Legal)as[gout[Result[EqualOut MapOut]]].
 exists gout;auto.
Qed.

Inductive chain_inputs (Sg:gsig)(Finst:usym->subst) : subst -> tele -> list smap -> subst -> tele -> Prop :=
| CiId : forall g D,chain_inputs Sg Finst g D [] g D
| CiInterface : forall ga gb D sms gc Dc,
 full_map_ok Sg(identity_functor_schema Sg Finst)gb ga D D(identity_smap D) ->
 chain_inputs Sg Finst gb D sms gc Dc ->chain_inputs Sg Finst ga D sms gc Dc
| CiLink : forall ga gb Da Db sm sms gc Dc,
 full_map_ok Sg(identity_functor_schema Sg Finst)gb ga Db Da sm ->
 chain_inputs Sg Finst gb Db sms gc Dc ->chain_inputs Sg Finst ga Da(sms++[sm])gc Dc.

Inductive result_meanings (Sg:gsig)(Finst:usym->subst) : subst -> subst -> tele -> Prop :=
| RmId : forall g D,result_meanings Sg Finst g g D
| RmStep : forall ga gb gc D,
 full_map_ok Sg(identity_functor_schema Sg Finst)ga gb D D(identity_smap D) ->
 result_meanings Sg Finst gb gc D ->result_meanings Sg Finst ga gc D.

Theorem reify_chain_instantiation : forall Sg Finst gt u sms gs h,
 reify_chain Sg Finst gt u sms gs h ->forall Du Dh Dr gin,
 g_fun Sg u=Some(Du,Dr,None) ->g_fun Sg h=Some(Dh,Dr,None) ->
 schema_input_ground Sg Finst u gt Du gin ->exists gh,
 schema_input_ground Sg Finst h gs Dh gh /\chain_inputs Sg Finst gin Du sms gh Dh /\
 (forall ghout,schema_result_ground Dh Dr gh ghout ->exists gout,
  schema_result_ground Du Dr gin gout /\result_meanings Sg Finst gout ghout Dr).
Proof.
 intros Sg Finst gt u sms gs h Chain;induction Chain;intros Du0 Dh0 Dr0 gin Dt Dh Input.
 - rewrite Dt in Dh;injection Dh as <-.
   exists gin;split;[exact Input|]. split;[constructor|]. intros ghout Out;exists ghout;split;[exact Out|constructor].
 - destruct(identity_schema_link H)as[D[Dr[DeclT[DeclV Link]]]].
   rewrite Dt in DeclT;injection DeclT as <- <-.
   destruct(@functor_link_instantiation Sg Finst gt u gm v(identity_smap Du0)Du0 Du0 Dr0 gin Link Dt DeclV Input)
    as[gmid[Mid[Map Back]]].
   destruct(IHChain Du0 Dh0 Dr0 gmid DeclV Dh Mid)as[gh[InH[Plan OutH]]].
   exists gh;split;[exact InH|]. split;[eapply CiInterface;eassumption|].
   intros ghout Legal;destruct(OutH ghout Legal)as[gmidout[MidOut Trail]].
   destruct(Back gmidout MidOut)as[gout[Out MapOut]].
   exists gout;split;[exact Out|eapply RmStep;eassumption].
 - apply IHChain;try assumption.
   destruct Input as[Phi[Tele[Ground[Cover[Abs [Fixed Equations]]]]]].
   exists Phi;split;[exact Tele|]. split;[exact Ground|]. split;[exact Cover|]. split;[exact Abs|].
   split;[intros t Hin;rewrite <- (H t Hin);exact(Fixed t Hin)|exact Equations].
 - rewrite H in Dt;injection Dt as <- <-.
   destruct(@functor_link_instantiation Sg Finst gt u gm v sm Du Dv Dr gin H1 H H0 Input)
    as[gmid[Mid[Map Back]]].
   destruct(IHChain Dv Dh0 Dr gmid H0 Dh Mid)as[gh[InH[Plan OutH]]].
   exists gh;split;[exact InH|]. split;[eapply CiLink;eassumption|].
   intros ghout Legal;destruct(OutH ghout Legal)as[gmidout[MidOut Trail]].
   destruct(Back gmidout MidOut)as[gout[Out MapOut]].
   exists gout;split;[exact Out|eapply RmStep;eassumption].
Qed.
