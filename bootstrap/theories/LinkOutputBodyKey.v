(* A result receiver is read at the completed body site, generation by
   generation. The pin check supplies the unchanged input readings. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodySoundCore BodyTr BodyTy Merge CanonTypes
 CoreMoss KeyTranslation InstantiateTr LinkBodyTrace.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL canon instantiate tr].
Lemma link_output_tn_edges:forall tm g,tn_get(env_with_edges tm[])g=nassoc g tm.
Proof.
 intros tm g;unfold tn_get,env_with_edges;cbn[en_tn].
 induction tm as[|[k y]tm IH];[reflexivity|].
 cbn[map nassoc assoc fst snd];destruct(Nat.eqb g k);[reflexivity|exact IH].
Qed.
Lemma link_output_instantiate_abs:forall e g,
 instantiate e(YAbs g)=canon e(opt_default(YAbs g)(tn_get e g)).
Proof. intros;cbn[instantiate];destruct(tn_get e g);reflexivity. Qed.
Lemma link_output_instantiate_translation:forall Fl tm t,
 tr Fl(instantiate(env_with_edges tm[])t)=ExecutionModel.app_subst(map(trp Fl)tm)(tr Fl t).
Proof.
 intros Fl tm t;induction t as[| |g|n xs IH|ms IH|fs IH]using ety_ind';cbn[instantiate tr];try reflexivity.
 - cbn[ExecutionModel.app_subst];rewrite link_output_tn_edges.
   rewrite lookup_nlook,nlook_trp.
   destruct(nassoc g tm)as[y|]eqn:Read;cbn[option_map];rewrite canon_nil by reflexivity;reflexivity.
 - rewrite app_subst_nom;unfold map_snd;rewrite !map_map;f_equal;apply map_ext_in;intros[k y]Hit;cbn[fst snd];f_equal.
   rewrite Forall_forall in IH;exact(IH(k,y)Hit).
 - rewrite app_subst_union;rewrite !map_map;f_equal;apply map_ext_in;intros y Hit.
   rewrite Forall_forall in IH;exact(IH y Hit).
 - rewrite app_subst_nom;unfold map_snd;rewrite !map_map;f_equal;apply map_ext_in;intros[k y]Hit;cbn[fst snd];f_equal.
   rewrite Forall_forall in IH;exact(IH(k,y)Hit).
Qed.
Lemma link_output_substitution_readings:forall sigma made types restore tau,
 (forall g,In g(A.gens tau)->
  ExecutionModel.app_subst sigma(ExecutionModel.app_subst made(TAbs g))=
  ExecutionModel.app_subst sigma(ExecutionModel.app_subst types(ExecutionModel.app_subst restore(TAbs g))))->
 ExecutionModel.app_subst sigma(ExecutionModel.app_subst made tau)=
 ExecutionModel.app_subst sigma(ExecutionModel.app_subst types(ExecutionModel.app_subst restore tau)).
Proof.
 intros sigma made types restore tau;induction tau as[|g|n xs IH|ms IH]using ty_ind';intro Agreement.
 - reflexivity.
 - apply Agreement;left;reflexivity.
 - rewrite !app_subst_nom;unfold map_snd;rewrite !map_map;f_equal;apply map_ext_in;intros[k y]Hit;cbn[fst snd];f_equal.
   rewrite Forall_forall in IH;apply(IH(k,y)Hit);intros g Hg;apply Agreement.
   cbn[A.gens];apply in_flat_map;exists(k,y);split;assumption.
 - rewrite !app_subst_union;rewrite !map_map;f_equal;apply map_ext_in;intros y Hit.
   rewrite Forall_forall in IH;apply(IH y Hit);intros g Hg;apply Agreement.
   cbn[A.gens];apply in_flat_map;exists y;split;assumption.
Qed.
Definition link_output_generation_reading(Fl:string->nat)(eb:env)(Cb:A.cx)(made:list(nat*ety))(roles:list(nat*nat))(g:nat):Prop:=
 tr Fl(canon eb(instantiate(env_with_edges made[])(YAbs g)))=
 A.canon Cb(ExecutionModel.app_subst(A.c_ty Cb)
  (ExecutionModel.app_subst(map(fun p=>(snd p,TAbs(fst p)))roles)(TAbs g))).
Definition link_output_source_role(roles:list(nat*nat))(g:nat):nat:=
 opt_default g(nassoc g(map(fun p=>(snd p,fst p))roles)).
Lemma link_output_inverse_type:forall roles g,
 ExecutionModel.app_subst(map(fun p=>(snd p,TAbs(fst p)))roles)(TAbs g)=TAbs(link_output_source_role roles g).
Proof.
 intros roles g;unfold link_output_source_role;cbn[ExecutionModel.app_subst].
 induction roles as[|[t h]roles IH];[reflexivity|].
 cbn[map fst snd lookup_subst nassoc assoc];destruct(Nat.eqb g h);[reflexivity|exact IH].
Qed.
Section Body.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Lemma link_output_generation_from_reading:forall c eb F Cb made roles g,
 CxRel w R Fl c eb F Cb->
 canon eb(opt_default(YAbs g)(nassoc g made))=
 canon eb(opt_default(YAbs(link_output_source_role roles g))(tn_get eb(link_output_source_role roles g)))->
 link_output_generation_reading Fl eb Cb made roles g.
Proof using w R Fl.
 intros c eb F Cb made roles g HC Reading.
 unfold link_output_generation_reading;rewrite link_output_inverse_type.
 pose proof(instantiate_canonical w R Fl c eb F Cb HC(YAbs(link_output_source_role roles g)))as Site.
 cbn[tr]in Site;rewrite <-Site.
 rewrite !link_output_instantiate_abs,link_output_tn_edges.
 rewrite(canon_nil(env_with_edges made[])(opt_default(YAbs g)(nassoc g made))eq_refl).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 rewrite Reading;reflexivity.
Qed.
Lemma link_output_receiver_at_site:forall c eb F Cb made roles r,
 CxRel w R Fl c eb F Cb->
 (forall g,In g(ety_gens r)->link_output_generation_reading Fl eb Cb made roles g)->
 tr Fl(canon eb(instantiate(env_with_edges made[])r))=
 A.canon Cb(ExecutionModel.app_subst(A.c_ty Cb)
  (ExecutionModel.app_subst(map(fun p=>(snd p,TAbs(fst p)))roles)(tr Fl r))).
Proof using w R Fl.
 intros c eb F Cb made roles r HC Agreement.
 rewrite <-(tr_canon Fl eb _ (proj1(cr_uf _ _ _ _ _ _ _ HC))),link_output_instantiate_translation.
 rewrite <-(cr_sub _ _ _ _ _ _ _ HC);unfold A.canon.
 apply link_output_substitution_readings;intros g Hit.
 specialize(Agreement g ltac:(rewrite <-(gens_tr Fl r);exact Hit)).
 unfold link_output_generation_reading,A.canon in Agreement.
 rewrite <-(tr_canon Fl eb _ (proj1(cr_uf _ _ _ _ _ _ _ HC))),link_output_instantiate_translation in Agreement.
 rewrite <-(cr_sub _ _ _ _ _ _ _ HC)in Agreement;exact Agreement.
Qed.
Lemma link_output_body_key_at_site:forall c eb F Cb made roles k,
 CxRel w R Fl c eb F Cb->
 (match k with KyMth r _=>forall g,In g(ety_gens r)->link_output_generation_reading Fl eb Cb made roles g|_=>True end)->
 key_tr Fl(link_trace_body_key eb made k)=A.at_site Cb(output_role_item roles(key_tr Fl k)).
Proof using w R Fl.
 intros c eb F Cb made roles k HC Readings;destruct k;try reflexivity.
 cbn[link_trace_body_key key_tr output_role_item ExecutionModel.app_subst_item A.at_site].
 assert(Identity:map(fun q:nat*ty=>(fst q,snd q))(A.c_ty Cb)=A.c_ty Cb).
 {induction(A.c_ty Cb)as[|[k v]rows IH];cbn;congruence. }
 rewrite Identity;f_equal;eapply link_output_receiver_at_site;[exact HC|exact Readings].
Qed.
End Body.
