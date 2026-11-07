(* Captured and prototype signature readings agree on their shared meanings.
   Unused requirement names need not be present in both snapshots. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit Sx SourceReadingExtend TyOfSubst SigReadingSubst CtxMerge.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition LookupCompatible (left right:list(nat*ty)):Prop:=
 forall key x y,A.nlook key left=Some x->A.nlook key right=Some y->x=y.
Lemma lookup_compatible_sym:forall left right,LookupCompatible left right->LookupCompatible right left.
Proof. intros left right H k x y X Y;symmetry;eapply H;eassumption. Qed.
Lemma lookup_extends_prefix:forall left right,LookupExtends left(left++right).
Proof. intros left right k x H;rewrite nlook_app',H;reflexivity. Qed.
Lemma lookup_extends_compatible_suffix:forall left right,
 LookupCompatible left right->LookupExtends right(left++right).
Proof.
 intros left right H k y Y;rewrite nlook_app'.
 destruct(A.nlook k left)as[x|]eqn:X;[f_equal;eapply H;eassumption|exact Y].
Qed.
Theorem signature_compatible:forall W f left right th sg1 sg2,
 LookupCompatible left right->A.SigOf W f left th sg1->A.SigOf W f right th sg2->sg1=sg2.
Proof.
 intros W f left right th sg1 sg2 H S1 S2.
 eapply(sigof_det W f(left++right)th).
 - exact(signature_extends W _ _ _ _ S1 _ (lookup_extends_prefix left right)).
 - exact(signature_extends W _ _ _ _ S2 _ (lookup_extends_compatible_suffix left right H)).
Qed.
Lemma appof_function_member:forall W f tm a key value,
 A.kind_sym W f<>Some A.DKMeth->A.AppOf W f tm a->
 In(key,value)a->A.nlook key tm=value.
Proof.
 intros W f tm a key value NotMethod[rs[Incl[-> [All Filter]]]] Member.
 apply in_map_iff in Member as[r[Eq Member]].
 injection Eq as <- <-;reflexivity.
Qed.
Lemma appof_function_complete:forall W f tm a key,
 A.kind_sym W f<>Some A.DKMeth->A.AppOf W f tm a->
 In key(A.w_reqs W f)->In(key,A.nlook key tm)a.
Proof.
 intros W f tm a key NotMethod[rs[Incl[-> [All Filter]]]] Member.
 rewrite(All NotMethod);apply in_map_iff;exists key;split;[reflexivity|exact Member].
Qed.
(* Coherence of the recorded provision determines every prototype key that
   the declaration reader can observe, including captured older generations. *)
Lemma compatible_from_recorded_application:forall W f prototype snapshot recorded,
 A.kind_sym W f<>Some A.DKMeth->A.AppOf W f snapshot recorded->
 (forall key value,A.nlook key prototype=Some value->In(key,Some value)recorded)->
 LookupCompatible prototype snapshot.
Proof.
 intros W f prototype snapshot recorded NotMethod App Coherent key x y X Y.
 pose proof(appof_function_member W f snapshot recorded key(Some x)NotMethod App(Coherent _ _ X))as Read.
 rewrite Y in Read;congruence.
Qed.
Theorem signature_from_recorded_application:forall W f prototype snapshot recorded sg1 sg2,
 A.kind_sym W f<>Some A.DKMeth->A.AppOf W f snapshot recorded->
 (forall key value,A.nlook key prototype=Some value->In(key,Some value)recorded)->
 A.SigOf W f prototype None sg1->A.SigOf W f snapshot None sg2->sg1=sg2.
Proof.
 intros W f prototype snapshot recorded sg1 sg2 Kind App Coherent S1 S2.
 eapply signature_compatible;[eapply compatible_from_recorded_application;eassumption|exact S1|exact S2].
Qed.

Definition substituted_signature (sb:Audit.subst)(sg:A.fsig):A.fsig:=
 A.FSig(map(Audit.app_subst sb)(A.sg_params sg))(Audit.app_subst sb(A.sg_ret sg)).
Lemma signature_substituted_table:forall W f tm sg sb,
 A.SigOf W f tm None sg->
 A.SigOf W f(smap sb tm)None(substituted_signature sb sg).
Proof.
 intros W f tm sg sb S.
 exact(sigof_subst W sb f tm None sg S(smap sb tm)(fun k=>nlook_smap sb k tm)).
Qed.
Lemma signature_substituted_twice:forall W f tm sg first second,
 A.SigOf W f tm None sg->
 A.SigOf W f(smap second(smap first tm))None
 (substituted_signature second(substituted_signature first sg)).
Proof. intros;eapply signature_substituted_table;eapply signature_substituted_table;eassumption. Qed.
Definition requirement_snapshot (requirements:list nat):list(nat*ty):=
 map(fun r=>(r,TAbs r))requirements.
Lemma requirement_snapshot_lookup:forall requirements key,
 A.nlook key(requirement_snapshot requirements)=
 if existsb(Nat.eqb key)requirements then Some(TAbs key)else None.
Proof.
 intros requirements;induction requirements as[|r rs IH];intro key;[reflexivity|].
 cbn[requirement_snapshot map A.nlook existsb fst snd].
 destruct(Nat.eqb_spec key r)as[->|Ne];[reflexivity|exact(IH key)].
Qed.
Lemma lookup_subst_nlook:forall key tm,Audit.lookup_subst key tm=A.nlook key tm.
Proof. intros key tm;induction tm as[|[k v]tm IH];[reflexivity|].
 cbn[Audit.lookup_subst A.nlook];destruct(Nat.eqb key k);[reflexivity|exact IH]. Qed.
Lemma current_requirement_compatible:forall requirements meanings solved,
 (forall key value,A.nlook key meanings=Some value->Audit.app_subst solved value=value)->
 LookupCompatible(smap solved(smap meanings(requirement_snapshot requirements)))meanings.
Proof.
 intros requirements meanings solved Fixed key x y X Y.
 rewrite !nlook_smap,requirement_snapshot_lookup in X.
 destruct(existsb(Nat.eqb key)requirements);[|discriminate].
 cbn[option_map]in X.
 assert(L:Audit.lookup_subst key meanings=Some y)by(rewrite lookup_subst_nlook;exact Y).
 cbn[Audit.app_subst]in X;rewrite L in X.
 rewrite(Fixed _ _ Y)in X;congruence.
Qed.
Theorem captured_signature_matches_current:forall W f requirements meanings solved snapshot recorded prototype_sig current_sig captured_sig,
 A.kind_sym W f<>Some A.DKMeth->A.AppOf W f snapshot recorded->
 (forall key value,A.nlook key meanings=Some value->Audit.app_subst solved value=value)->
 (forall key value,A.nlook key(smap solved(smap meanings(requirement_snapshot requirements)))=Some value->
   In(key,Some value)recorded)->
 A.SigOf W f(requirement_snapshot requirements)None prototype_sig->
 A.SigOf W f meanings None current_sig->A.SigOf W f snapshot None captured_sig->
 current_sig=captured_sig.
Proof.
 intros W f requirements meanings solved snapshot recorded prototype_sig current_sig captured_sig Kind App Fixed Coherent Proto Current Captured.
 pose proof(signature_substituted_twice W f _ _ meanings solved Proto)as Transformed.
 transitivity(substituted_signature solved(substituted_signature meanings prototype_sig)).
 - symmetry;eapply signature_compatible;[apply current_requirement_compatible;exact Fixed|exact Transformed|exact Current].
 - eapply signature_from_recorded_application;[exact Kind|exact App|exact Coherent|exact Transformed|exact Captured].
Qed.
Lemma signature_parameter_arity:forall W f tm th sg recv name ps rt body,
 A.SigOf W f tm th sg->A.ast_of W f=Some(ADFn recv name ps rt body)->
 List.length(A.sg_params sg)=List.length ps.
Proof.
 intros W f tm th sg recv name ps rt body[d[r[n[params[result[impl[Decl[Ast[Args Ret]]]]]]]]] Read.
 unfold A.ast_of in Read;rewrite Decl in Read;cbn[option_map]in Read;rewrite Ast in Read.
 injection Read as <- <- <- <- <-.
 symmetry;exact(Forall2_length Args).
Qed.
