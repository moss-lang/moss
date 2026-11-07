(* A newly found family identifier records an actual successful pattern match. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodyTr FamilySelectTrace.
Import ListNotations.
Definition family_found_ids(found:list family_candidate):list nat:=map(fun q=>gs_id(fst q))found.
Definition family_raw_inst(marks:list(nat*nat))(matching:list(nat*ety)):=
 map(fun q=>(snd q,opt_default(YAbs(snd q))(nassoc(fst q)matching)))marks.
Theorem family_candidate_new_id:forall mi e em r mid u fg marks back before lmi rg mg sub s after sf target,
 family_candidate_step mi e em r mid u fg marks back before(lmi,rg,mg,sub)s=Good(after,sf)->
 ~In target(family_found_ids before)->In target(family_found_ids after)->
 exists pat sp,receiver_at em lmi rg sub s=Good(pat,sp)/\
 after=before++[(fg,pmatch_full back pat r[])]/\sf=sp/\target=gs_id fg/\gs_id mg=mid/\
 forallb(fun q=>is_some(nassoc(fst q)(pmatch_full back pat r[])))
  (filter(fun q=>nmem(snd q)(ety_gens pat))marks)=true/\
 canon e(ety_subst_full(family_raw_inst marks(pmatch_full back pat r[]))pat)=r.
Proof.
 intros mi e em r mid u fg marks back before lmi rg mg sub s after sf target Run Old New.
 unfold family_candidate_step in Run;cbn beta iota zeta in Run.
 destruct(negb(Nat.eqb(gs_id mg)mid))eqn:BadMid.
 {injection Run as <- <-;contradiction. }
 apply negb_false_iff in BadMid;apply Nat.eqb_eq in BadMid.
 apply bind_inv in Run as(pat&sp&Read&Run).
 destruct(negb(forallb(fun q=>is_some(nassoc(fst q)(pmatch_full back pat r[])))
 (filter(fun q=>nmem(snd q)(ety_gens pat))marks)))eqn:BadMention.
 {injection Run as <- <-;contradiction. }
 apply negb_false_iff in BadMention.
 fold(family_raw_inst marks(pmatch_full back pat r[]))in Run.
 destruct(negb(ety_eqb(canon e(ety_subst_full(family_raw_inst marks(pmatch_full back pat r[]))pat))r))eqn:BadAt.
 {injection Run as <- <-;contradiction. }
 apply negb_false_iff in BadAt;apply ety_eqb_true in BadAt.
 destruct(filter(fun q=>Nat.eqb(gs_id(fst q))u)before)as[|[other theta]rest].
 - injection Run as <- <-.
   unfold family_found_ids in New;rewrite map_app in New;apply in_app_iff in New as[Was|Added];[contradiction|].
   cbn[map fst snd In]in Added;destruct Added as[Id|[]].
   exists pat,sp;split;[exact Read|];split;[reflexivity|];split;[reflexivity|];
   split;[symmetry;exact Id|];split;[exact BadMid|];split;assumption.
 - destruct(forallb(fun q=>match nassoc(fst q)theta with Some y=>ety_eqb y(snd q)|None=>false end)
  (pmatch_full back pat r[])&&Nat.eqb(length(pmatch_full back pat r[]))(length theta)).
   + injection Run as <- <-;contradiction.
   + apply bind_inv in Run as(text&ss&Show&Run);exfalso;eapply err_not_good;exact Run.
Qed.
