(* The checked raw reconstruction is the semantic matching substitution. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySound BodyTr Merge CanonTypes
 TyOfSubst PMatchComplete PMatchExec PMatchSemanticImage FamilyCandidateComplete FamilyCandidateWitness
 FamilyMarkedSubst FamilyMatchingCtx FamilyMarkerSpecialize FamilyScanSpecialize.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma family_set_types_member:forall xs C p y,
 NoDup(map fst xs)->A.nlook p xs=Some y->
 A.nlook p(A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(snd q))xs C))=Some y.
Proof.
 intro xs;induction xs as[|[q t]xs IH];intros C p y Nd Read;[discriminate Read|].
 apply NoDup_cons_iff in Nd as[Absent Tail];cbn[A.nlook fst snd]in Read.
 cbn[fold_left fst snd];destruct(Nat.eqb_spec p q)as[->|Ne].
 - injection Read as <-.
   assert(Outside:Forall(fun z=>fst z<>q)xs).
   {apply Forall_forall;intros z Hin Eq;apply Absent;apply in_map_iff;exists z;split;[exact Eq|exact Hin]. }
   rewrite(family_matching_outside xs _ q Outside);cbn[A.set_ty A.c_ty A.nlook];rewrite Nat.eqb_refl;reflexivity.
 - eapply IH;[exact Tail|exact Read].
Qed.
Lemma family_used_marker_bound:forall (marks:list(nat*nat)) (matching:list(nat*ety)) pat g p,
 forallb(fun q=>is_some(nassoc(fst q)matching))(filter(fun q=>nmem(snd q)(ety_gens pat))marks)=true->
 nassoc g(family_marker_back marks)=Some p->In g(ety_gens pat)->exists raw,nassoc p matching=Some raw.
Proof.
 intros marks matching pat g p Mentioned Back Gen.
 apply pmatch_lookup_member in Back;unfold family_marker_back in Back.
 apply in_map_iff in Back as([p0 g0]&Eq&Hin);cbn[fst snd]in Eq;injection Eq as Eg Ep;subst g0;subst p0.
 rewrite forallb_forall in Mentioned.
 pose proof(Mentioned(p,g)ltac:(apply filter_In;split;[exact Hin|apply nmem_in;exact Gen]))as Present.
 cbn[fst snd]in Present;destruct(nassoc p matching)as[raw|]eqn:Read;[exists raw;reflexivity|discriminate Present].
Qed.
Lemma family_raw_inst_back:forall marks matching,
 family_raw_inst marks matching=map(fun q=>(fst q,opt_default(YAbs(fst q))(nassoc(snd q)matching)))(family_marker_back marks).
Proof. intros;unfold family_raw_inst,family_marker_back;rewrite map_map;reflexivity. Qed.
Lemma family_mark_type_canonical:forall uf pat back theta,
 Canon uf pat->(forall p,Canon uf(theta p))->Canon uf(mark_type back theta pat).
Proof.
 intros uf pat;induction pat as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intros back theta Root Values;
 cbn[mark_type];try exact Root.
 - destruct(nassoc g back);[apply Values|exact Root].
 - intros h Gen;apply gens_nom in Gen as(q&Hq&Gen);apply in_map_iff in Hq as(old&Eq&Hin);subst q;cbn[snd]in Gen.
   rewrite Forall_forall in IH;apply(IH old Hin back theta);[|exact Values|exact Gen].
   intros k Hk;apply Root,gens_nom;exists old;split;assumption.
 - intros h Gen;apply gens_union in Gen as(q&Hq&Gen);apply in_map_iff in Hq as(old&Eq&Hin);subst q.
   rewrite Forall_forall in IH;apply(IH old Hin back theta);[|exact Values|exact Gen].
   intros k Hk;apply Root,gens_union;exists old;split;assumption.
 - intros h Gen;apply gens_rec in Gen as(q&Hq&Gen);apply in_map_iff in Hq as(old&Eq&Hin);subst q;cbn[snd]in Gen.
   rewrite Forall_forall in IH;apply(IH old Hin back theta);[|exact Values|exact Gen].
   intros k Hk;apply Root,gens_rec;exists old;split;assumption.
Qed.
Section Reconstruct.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat).
Lemma family_target_matching_member:forall c e F C matching p raw,
 CxRel w R Fl c e F C->NoDup(map fst matching)->
 Forall(fun q=>canon e(snd q)=snd q)matching->nassoc p matching=Some raw->
 A.nlook p(family_semantic_target C(family_matching Fl e matching))=Some(tr Fl raw).
Proof.
 intros c e F C matching p raw HC Nd Can Read;unfold family_semantic_target;rewrite nlook_smap.
 assert(Norm:A.nlook p(family_matching Fl e matching)=Some(tr Fl(canon e raw))).
 {change(A.nlook p(family_norm_types Fl e matching)=Some(tr Fl(canon e raw))).
  rewrite family_norm_lookup,Read;reflexivity. }
 assert(Ndup:NoDup(map fst(family_matching Fl e matching))).
 {unfold family_matching;rewrite map_map;exact Nd. }
 rewrite(family_set_types_member _ _ _ _ Ndup Norm);cbn[option_map];f_equal.
 rewrite(cr_sub _ _ _ _ _ _ _ HC),tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 f_equal;rewrite Forall_forall in Can;exact(Can(p,raw)(pmatch_lookup_member _ _ _ _ Read)).
Qed.
Theorem family_match_reconstructs:forall c e F C marks matching pat r,
 CxRel w R Fl c e F C->NoDup(map snd marks)->NoDup(map fst matching)->
 Forall(fun q=>canon e(snd q)=snd q)matching->canon e pat=pat->
 forallb(fun q=>is_some(nassoc(fst q)matching))(filter(fun q=>nmem(snd q)(ety_gens pat))marks)=true->
 canon e(ety_subst_full(family_raw_inst marks matching)pat)=r->
 Audit.app_subst(family_marker_subst marks(family_semantic_target C(family_matching Fl e matching)))(tr Fl pat)=tr Fl r.
Proof.
 intros c e F C marks matching pat r HC Marks Unique Values Pat Mentioned Checked.
 set(theta:=fun p=>opt_default YUnit(nassoc p matching)).
 assert(Bounded:forall g p,nassoc g(family_marker_back marks)=Some p->In g(ety_gens pat)->exists raw,nassoc p matching=Some raw).
 {intros; eapply family_used_marker_bound;eassumption. }
 assert(RawEq:ety_subst_full(family_raw_inst marks matching)pat=mark_type(family_marker_back marks)theta pat).
 {rewrite <-(ety_subst_full_mark_type pat(family_marker_back marks)theta).
  unfold ety_subst_full;apply ety_subst_lookup_extensional;intros g Gen.
  rewrite family_raw_inst_back.
  rewrite(match_nassoc_back_map _ _ (fun g p=>opt_default(YAbs g)(nassoc p matching))).
  rewrite(match_nassoc_back_map _ _ (fun _ p=>theta p)).
  destruct(nassoc g(family_marker_back marks))as[p|]eqn:Back;cbn[option_map];[|reflexivity].
  destruct(Bounded g p Back Gen)as(raw&Read);unfold theta;rewrite Read;reflexivity. }
 assert(Cpat:Canon(en_uf e)pat)by(pose proof(canon_tri e pat(proj1(cr_uf _ _ _ _ _ _ _ HC)))as H;rewrite Pat in H;exact H).
 assert(Ctheta:forall p,Canon(en_uf e)(theta p)).
 {intro p;unfold theta;destruct(nassoc p matching)as[raw|]eqn:Read;cbn[opt_default].
  - pose proof(canon_tri e raw(proj1(cr_uf _ _ _ _ _ _ _ HC)))as H.
    rewrite Forall_forall in Values;pose proof(Values(p,raw)(pmatch_lookup_member _ _ _ _ Read))as FixedRaw;cbn[snd]in FixedRaw;rewrite FixedRaw in H;exact H.
  - intros g Gen;contradiction. }
 rewrite RawEq in Checked.
 assert(Fixed:canon e(mark_type(family_marker_back marks)theta pat)=mark_type(family_marker_back marks)theta pat).
 {unfold canon;apply canon_fix,family_mark_type_canonical;assumption. }
 rewrite Fixed in Checked.
 rewrite <-Checked;apply match_mark_type_translation;intros g Gen.
 destruct(nassoc g(family_marker_back marks))as[p|]eqn:Back.
 - rewrite(family_marker_subst_back marks _ Marks g p Back).
   destruct(Bounded g p Back Gen)as(raw&Read).
   rewrite(family_target_matching_member c e F C matching p raw HC Unique Values Read).
   unfold theta;rewrite Read;reflexivity.
 - exact(family_marker_subst_other marks _ g Back).
Qed.
End Reconstruct.
