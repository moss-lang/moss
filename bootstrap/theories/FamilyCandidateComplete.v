(* A genuine structural solution cannot be skipped by the actual candidate filters. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodyTr FamilySelectTrace PMatchComplete.
Import ListNotations.
Local Open Scope list_scope.
Definition family_marker_back(marks:list(nat*nat)):=map(fun q=>(snd q,fst q))marks.
Lemma family_back_lookup:forall marks p g,
 NoDup(map snd marks)->In(p,g)marks->nassoc g(family_marker_back marks)=Some p.
Proof.
 intros marks p g Nd Hin;apply match_nassoc_member.
 - unfold family_marker_back;rewrite map_map;exact Nd.
 - unfold family_marker_back;apply in_map_iff;exists(p,g);split;[reflexivity|exact Hin].
Qed.
Lemma family_candidate_mentioned_complete:forall marks pat theta,
 NoDup(map snd marks)->MatchUnique pat->
 forallb(fun q=>is_some(nassoc(fst q)(pmatch_full(family_marker_back marks)pat
   (mark_type(family_marker_back marks)theta pat)[])))
 (filter(fun q=>nmem(snd q)(ety_gens pat))marks)=true.
Proof.
 intros marks pat theta Nd Unique;apply forallb_forall;intros[p g]Hin;apply filter_In in Hin as[Hin Gen].
 cbn[fst snd]in Gen|-*;apply nmem_in in Gen.
 rewrite(pmatch_full_complete pat(family_marker_back marks)theta Unique g p Gen(family_back_lookup marks p g Nd Hin));reflexivity.
Qed.
Lemma family_candidate_receiver_complete:forall e marks pat theta,
 MatchUnique pat->canon e(mark_type(family_marker_back marks)theta pat)=mark_type(family_marker_back marks)theta pat->
 canon e(ety_subst_full
  (map(fun q=>(snd q,opt_default(YAbs(snd q))
   (nassoc(fst q)(pmatch_full(family_marker_back marks)pat(mark_type(family_marker_back marks)theta pat)[]))))marks)pat)=
 mark_type(family_marker_back marks)theta pat.
Proof.
 intros e marks pat theta Unique Can.
 assert(Sub:map(fun q=>(snd q,opt_default(YAbs(snd q))
   (nassoc(fst q)(pmatch_full(family_marker_back marks)pat(mark_type(family_marker_back marks)theta pat)[]))))marks=
 inferred_marker_subst(family_marker_back marks)(pmatch_full(family_marker_back marks)pat(mark_type(family_marker_back marks)theta pat)[])).
 {unfold inferred_marker_subst,family_marker_back;rewrite map_map;reflexivity. }
 rewrite Sub,pmatch_full_reconstructs by exact Unique;exact Can.
Qed.
Theorem family_candidate_retains_solution:forall mi e em r mid u fg marks found lmi rg m sub s pat sp found' sf theta,
 family_candidate_step mi e em r mid u fg marks(family_marker_back marks)found(lmi,rg,m,sub)s=Good(found',sf)->
 receiver_at em lmi rg sub s=Good(pat,sp)->gs_id m=mid->gs_id fg=u->
 NoDup(map snd marks)->MatchUnique pat->r=mark_type(family_marker_back marks)theta pat->canon e r=r->
 exists other th,In(other,th)found'/\gs_id other=gs_id fg.
Proof.
 intros mi e em r mid u fg marks found lmi rg m sub s pat sp found' sf theta
 E Read Mid Fid Nd Unique Target Can;subst r.
 unfold family_candidate_step in E;cbn beta iota zeta in E;rewrite Mid,Nat.eqb_refl in E;cbn[negb]in E.
 apply bind_inv in E as(pat0&sp0&Read0&E);rewrite Read in Read0;injection Read0 as <- <-.
 rewrite(family_candidate_mentioned_complete marks pat theta Nd Unique)in E;cbn[negb]in E.
 rewrite(family_candidate_receiver_complete e marks pat theta Unique Can),ety_eqb_refl in E;cbn[negb]in E.
 destruct(filter(fun q=>Nat.eqb(gs_id(fst q))u)found)as[|[old th0]rest]eqn:Seen.
 - injection E as <- <-.
   exists fg,(pmatch_full(family_marker_back marks)pat(mark_type(family_marker_back marks)theta pat)[]).
   split;[apply in_app_iff;right;left;reflexivity|reflexivity].
 - destruct(forallb(fun q=>match nassoc(fst q)th0 with Some y=>ety_eqb y(snd q)|None=>false end)
    (pmatch_full(family_marker_back marks)pat(mark_type(family_marker_back marks)theta pat)[])&&
    Nat.eqb(List.length(pmatch_full(family_marker_back marks)pat(mark_type(family_marker_back marks)theta pat)[]))(List.length th0))eqn:Same.
   + injection E as <- <-.
     assert(Hin:In(old,th0)(filter(fun q=>Nat.eqb(gs_id(fst q))u)found)).
     {rewrite Seen;left;reflexivity. }
     apply filter_In in Hin as[Hin Id];cbn[fst]in Id;apply Nat.eqb_eq in Id.
     exists old,th0;split;[exact Hin|congruence].
   + apply bind_inv in E as(name&sn&Show&E);exfalso;eapply err_not_good;exact E.
Qed.
