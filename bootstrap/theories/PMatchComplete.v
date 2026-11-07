(* Complete structural inference from a receiver pattern at a known solution. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab BodyTr PMatchExec FamilyMatchBudget.
Import ListNotations.
Local Open Scope list_scope.
Definition MatchAgrees(theta:nat->ety)(acc:list(nat*ety)):Prop:=
 forall p y,nassoc p acc=Some y->y=theta p.
Fixpoint mark_type(back:list(nat*nat))(theta:nat->ety)(pat:ety):ety:=
 match pat with
 |YAbs g=>match nassoc g back with Some p=>theta p|None=>pat end
 |YNom n xs=>YNom n(map(fun q=>(fst q,mark_type back theta(snd q)))xs)
 |YUnion ms=>YUnion(map(mark_type back theta)ms)
 |YRec fs=>YRec(map(fun q=>(fst q,mark_type back theta(snd q)))fs)
 |_=>pat end.
Inductive MatchUnique:ety->Prop:=
|MUUnit:MatchUnique YUnit
|MUNever:MatchUnique YNever
|MUAbs:forall g,MatchUnique(YAbs g)
|MUNom:forall n xs,NoDup(map fst xs)->Forall(fun q=>MatchUnique(snd q))xs->MatchUnique(YNom n xs)
|MUUnion:forall ms,Forall MatchUnique ms->MatchUnique(YUnion ms)
|MURecord:forall fs,Forall(fun q=>MatchUnique(snd q))fs->MatchUnique(YRec fs).
Lemma match_unique_nom:forall n xs,MatchUnique(YNom n xs)->NoDup(map fst xs)/\Forall(fun q=>MatchUnique(snd q))xs.
Proof. intros n xs H;inversion H;subst;auto. Qed.
Lemma match_unique_union:forall ms,MatchUnique(YUnion ms)->Forall MatchUnique ms.
Proof. intros ms H;inversion H;subst;auto. Qed.
Lemma match_unique_record:forall fs,MatchUnique(YRec fs)->Forall(fun q=>MatchUnique(snd q))fs.
Proof. intros fs H;inversion H;subst;auto. Qed.

Lemma match_nassoc_app:forall X k (xs ys:list(nat*X)),
 nassoc k(xs++ys)=match nassoc k xs with Some y=>Some y|None=>nassoc k ys end.
Proof.
 intros X k xs;induction xs as[|[j v]xs IH];intro ys;[reflexivity|].
 cbn[nassoc assoc List.app];destruct(Nat.eqb k j);[reflexivity|exact(IH ys)].
Qed.
Lemma pmatch_lookup_preserved:forall fuel back pat t acc p y,
 nassoc p acc=Some y->nassoc p(pmatch fuel back pat t acc)=Some y.
Proof.
 intros fuel back pat t acc p y Look.
 eapply(pmatch_invariant(fun _=>True)(fun xs=>nassoc p xs=Some y)back);
 [split;[|split];intros;apply Forall_forall;intros;exact I| |exact I|exact Look].
 intros xs q z Have _ From Absent;rewrite match_nassoc_app,Have;reflexivity.
Qed.
Lemma match_fold_preserves:forall X Y(step:Y->X->Y)(K:Y->Prop)xs acc,
 K acc->(forall x a,K a->K(step a x))->K(fold_left step xs acc).
Proof.
 intros X Y step K xs;induction xs as[|x xs IH];intros acc H Step;[exact H|].
 cbn[fold_left];apply IH;[apply Step;exact H|exact Step].
Qed.
Lemma match_fold_covers:forall X Y(step:Y->X->Y)(J:Y->Prop)(K:X->Y->Prop)xs acc,
 J acc->
 (forall x,In x xs->forall a,J a->J(step a x)/\K x(step a x))->
 (forall x y a,K x a->K x(step a y))->
 J(fold_left step xs acc)/\forall x,In x xs->K x(fold_left step xs acc).
Proof.
 intros X Y step J K xs;induction xs as[|x xs IH];intros acc H Step Keep.
 - split;[exact H|intros z[]].
 - cbn[fold_left];destruct(Step x(or_introl eq_refl)acc H)as[J1 K1].
   destruct(IH _ J1 ltac:(intros z Hz;apply Step;right;exact Hz)Keep)as[Jf Kf].
   split;[exact Jf|];intros z[<-|Hz].
   + eapply match_fold_preserves;[exact K1|];intros a v;apply Keep.
   + exact(Kf z Hz).
Qed.
Lemma match_nassoc_member:forall X (xs:list(nat*X))k t,
 NoDup(map fst xs)->In(k,t)xs->nassoc k xs=Some t.
Proof.
 intros X xs;induction xs as[|[j v]xs IH];intros k t Nd Hin;[contradiction|].
 cbn[map fst]in Nd;apply NoDup_cons_iff in Nd as[Absent Nd].
 destruct Hin as[Eq|Hin];[injection Eq as <- <-;cbn[nassoc assoc];rewrite Nat.eqb_refl;reflexivity|].
 cbn[nassoc assoc];destruct(Nat.eqb_spec k j)as[->|Ne].
 - exfalso;apply Absent;apply in_map_iff;exists(j,t);split;[reflexivity|exact Hin].
 - exact(IH k t Nd Hin).
Qed.
Lemma match_nassoc_mapped:forall back theta xs k,
 nassoc k(map(fun q=>(fst q,mark_type back theta(snd q)))xs)=
 option_map(mark_type back theta)(nassoc k xs).
Proof.
 intros back theta xs;induction xs as[|[j v]xs IH];intro k;[reflexivity|].
 cbn[map fst snd nassoc assoc];destruct(Nat.eqb k j);[reflexivity|exact(IH k)].
Qed.
Lemma match_fold_map:forall X Y Z(f:X->Y)(step:Z->Y->Z)xs acc,
 fold_left step(map f xs)acc=fold_left(fun a x=>step a(f x))xs acc.
Proof.
 intros X Y Z f step xs;induction xs as[|x xs IH];intro acc;[reflexivity|].
 cbn[map fold_left];apply IH.
Qed.
Lemma match_combine_map:forall X Y(f:X->Y)xs,
 combine xs(map f xs)=map(fun x=>(x,f x))xs.
Proof. intros X Y f xs;induction xs;cbn[combine map];[reflexivity|rewrite IHxs;reflexivity]. Qed.
Lemma match_agrees_append:forall theta acc p,
 MatchAgrees theta acc->MatchAgrees theta(acc++[(p,theta p)]).
Proof.
 intros theta acc p H q y Look;rewrite match_nassoc_app in Look.
 destruct(nassoc q acc)as[v|]eqn:Old.
 - injection Look as <-;exact(H q v Old).
 - cbn[nassoc assoc]in Look;destruct(Nat.eqb_spec q p)as[->|Ne];[injection Look as <-;reflexivity|discriminate Look].
Qed.
Theorem pmatch_complete_at:forall pat,
 MatchUnique pat->forall fuel back theta acc,
 ety_depth pat<=fuel->MatchAgrees theta acc->
 MatchAgrees theta(pmatch fuel back pat(mark_type back theta pat)acc)/\
 forall g p,In g(ety_gens pat)->nassoc g back=Some p->
 nassoc p(pmatch fuel back pat(mark_type back theta pat)acc)=Some(theta p).
Proof.
 intros pat;induction pat as[| |id|head xs IH|ms IH|fs IH]using ety_ind';
 intros Unique[|fuel]back theta acc Depth Agree;try solve[cbn[ety_depth]in Depth;lia].
 - split;[exact Agree|intros g p[]].
 - split;[exact Agree|intros g p[]].
 - cbn[mark_type pmatch].
   destruct(nassoc id back)as[key|]eqn:Back.
   + destruct(nassoc key acc)as[y|]eqn:Old;cbn[is_some].
     * split;[exact Agree|];intros g p[Eq|[]]Read;subst g;rewrite Back in Read;injection Read as <-.
       rewrite Old;f_equal;exact(Agree key y Old).
     * split;[apply match_agrees_append;exact Agree|].
       intros g p[Eq|[]]Read;subst g;rewrite Back in Read;injection Read as <-.
       rewrite match_nassoc_app,Old;cbn[nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
   + split;[exact Agree|];intros g p[Eq|[]]Read;subst g;rewrite Back in Read;discriminate Read.
 - destruct(match_unique_nom head xs Unique)as[Nd Parts].
   cbn[mark_type pmatch];rewrite Nat.eqb_refl.
   set(step:=fun acc q=>match nassoc(fst q)(map(fun q=>(fst q,mark_type back theta(snd q)))xs)with
      Some y=>pmatch fuel back(snd q)y acc|None=>acc end).
   assert(Step:forall q,In q xs->forall a,MatchAgrees theta a->
    MatchAgrees theta(step a q)/\forall g p,In g(ety_gens(snd q))->nassoc g back=Some p->nassoc p(step a q)=Some(theta p)).
   {intros[k t]Hin a A0;unfold step;cbn[fst snd];rewrite match_nassoc_mapped,(match_nassoc_member _ xs k t Nd Hin);cbn[option_map].
    rewrite Forall_forall in IH,Parts;apply(IH(k,t)Hin(Parts(k,t)Hin)fuel back theta a);[|exact A0].
    pose proof(family_depth_nom head xs(k,t)Hin);cbn[snd]in *;lia. }
   destruct(match_fold_covers _ _ step(MatchAgrees theta)
     (fun q a=>forall g p,In g(ety_gens(snd q))->nassoc g back=Some p->nassoc p a=Some(theta p))xs acc Agree Step
     ltac:(intros q q0 a H g p Hg Read;unfold step;destruct(nassoc(fst q0)(map(fun q=>(fst q,mark_type back theta(snd q)))xs));
       [apply pmatch_lookup_preserved|];exact(H g p Hg Read)))as[A0 Covered].
   split;[exact A0|];intros g p G B.
   change(In g(flat_map(fun q=>ety_gens(snd q))xs))in G;apply in_flat_map in G as(q&Hq&Gq).
   exact(Covered q Hq g p Gq B).
 - pose proof(match_unique_union ms Unique)as Parts.
   cbn[mark_type pmatch];rewrite map_length,Nat.eqb_refl,match_combine_map.
   rewrite match_fold_map.
   set(step:=fun a t=>pmatch fuel back t(mark_type back theta t)a).
   assert(Step:forall t,In t ms->forall a,MatchAgrees theta a->
    MatchAgrees theta(step a t)/\forall g p,In g(ety_gens t)->nassoc g back=Some p->nassoc p(step a t)=Some(theta p)).
   {intros t Hin a A0;unfold step;rewrite Forall_forall in IH,Parts;apply(IH t Hin(Parts t Hin)fuel back theta a);[|exact A0].
    pose proof(family_depth_union ms t Hin);lia. }
   destruct(match_fold_covers _ _ step(MatchAgrees theta)
     (fun t a=>forall g p,In g(ety_gens t)->nassoc g back=Some p->nassoc p a=Some(theta p))ms acc Agree Step
     ltac:(intros t t0 a H g p Hg Read;unfold step;apply pmatch_lookup_preserved;exact(H g p Hg Read)))as[A0 Covered].
   split;[exact A0|];intros g p G B.
   change(In g(flat_map ety_gens ms))in G;apply in_flat_map in G as(t&Ht&Gt).
   exact(Covered t Ht g p Gt B).
 - pose proof(match_unique_record fs Unique)as Parts.
   cbn[mark_type pmatch];rewrite map_length,Nat.eqb_refl,map_map.
   replace(map(fun q=>snd(fst q,mark_type back theta(snd q)))fs)
    with(map(mark_type back theta)(map snd fs))by(rewrite map_map;reflexivity).
   rewrite match_combine_map,match_fold_map.
   set(step:=fun a t=>pmatch fuel back t(mark_type back theta t)a).
   assert(Step:forall t,In t(map snd fs)->forall a,MatchAgrees theta a->
    MatchAgrees theta(step a t)/\forall g p,In g(ety_gens t)->nassoc g back=Some p->nassoc p(step a t)=Some(theta p)).
   {intros t Hin a A0;apply in_map_iff in Hin as(q&Eq&Hq);subst t.
    unfold step;rewrite Forall_forall in IH,Parts;apply(IH q Hq(Parts q Hq)fuel back theta a);[|exact A0].
    pose proof(family_depth_record fs q Hq);lia. }
   destruct(match_fold_covers _ _ step(MatchAgrees theta)
     (fun t a=>forall g p,In g(ety_gens t)->nassoc g back=Some p->nassoc p a=Some(theta p))(map snd fs)acc Agree Step
     ltac:(intros t t0 a H g p Hg Read;unfold step;apply pmatch_lookup_preserved;exact(H g p Hg Read)))as[A0 Covered].
   split;[exact A0|];intros g p G B.
   change(In g(flat_map(fun q=>ety_gens(snd q))fs))in G;apply in_flat_map in G as(q&Hq&Gq).
   assert(Hmap:In(snd q)(map snd fs))by(apply in_map_iff;exists q;split;[reflexivity|exact Hq]).
   exact(Covered(snd q)Hmap g p Gq B).
Qed.
Corollary pmatch_full_complete:forall pat back theta,
 MatchUnique pat->
 forall g p,In g(ety_gens pat)->nassoc g back=Some p->
 nassoc p(pmatch_full back pat(mark_type back theta pat)[])=Some(theta p).
Proof.
 intros pat back theta Unique;unfold pmatch_full.
 apply(proj2(pmatch_complete_at pat Unique(ety_depth pat)back theta[]ltac:(lia)ltac:(intros p y H;discriminate H))).
Qed.
From NM Require Import NominalSupport.
Lemma support_match_unique:forall W,
 (forall n,NoDup(Audit.Elaboration.w_reqs W n))->forall pat,Support W pat->MatchUnique pat.
Proof.
 intros W Reqs pat;induction pat as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intro Sup.
 - constructor.
 - constructor.
 - constructor.
 - destruct(@support_nominal W n xs Sup)as[Kind[Domain Parts]].
   constructor;[rewrite Domain;apply Reqs|].
   rewrite Forall_forall in IH,Parts|-*;intros q Hq;exact(IH q Hq(Parts q Hq)).
 - assert(Parts:Forall(Support W)ms)by(inversion Sup;assumption).
   constructor;rewrite Forall_forall in IH,Parts|-*;intros t Ht;exact(IH t Ht(Parts t Ht)).
 - assert(Parts:Forall(fun q=>Support W(snd q))fs)by(inversion Sup;assumption).
   constructor;rewrite Forall_forall in IH,Parts|-*;intros q Hq;exact(IH q Hq(Parts q Hq)).
Qed.
Lemma match_nassoc_back_map:forall X Y(f:nat->X->Y)back g,
 nassoc g(map(fun q=>(fst q,f(fst q)(snd q)))back)=option_map(f g)(nassoc g back).
Proof.
 intros X Y f back;induction back as[|[k x]back IH];intro g;[reflexivity|].
 cbn[map fst snd nassoc assoc];destruct(Nat.eqb_spec g k)as[->|Ne];[reflexivity|exact(IH g)].
Qed.
Lemma match_gens_nom:forall n xs,ety_gens(YNom n xs)=flat_map(fun q=>ety_gens(snd q))xs.
Proof. reflexivity. Qed.
Lemma match_gens_union:forall ms,ety_gens(YUnion ms)=flat_map ety_gens ms.
Proof. reflexivity. Qed.
Lemma match_gens_record:forall fs,ety_gens(YRec fs)=flat_map(fun q=>ety_gens(snd q))fs.
Proof. reflexivity. Qed.
Lemma ety_subst_lookup_extensional:forall fuel pat sub1 sub2,
 (forall g,In g(ety_gens pat)->nassoc g sub1=nassoc g sub2)->
 ety_subst fuel sub1 pat=ety_subst fuel sub2 pat.
Proof.
 intro fuel;induction fuel as[|fuel IH];intros pat sub1 sub2 Eq;[reflexivity|].
 destruct pat as[| |g|n xs|ms|fs];cbn[ety_subst];try reflexivity.
 - rewrite(Eq g(or_introl eq_refl));reflexivity.
 - f_equal;apply map_ext_in;intros q Hq;f_equal;apply IH.
   intros g Hg;apply Eq;rewrite match_gens_nom;apply in_flat_map;exists q;split;assumption.
 - f_equal;apply map_ext_in;intros t Ht;apply IH.
   intros g Hg;apply Eq;rewrite match_gens_union;apply in_flat_map;exists t;split;assumption.
 - f_equal;apply map_ext_in;intros q Hq;f_equal;apply IH.
   intros g Hg;apply Eq;rewrite match_gens_record;apply in_flat_map;exists q;split;assumption.
Qed.
Theorem ety_subst_full_mark_type:forall pat back theta,
 ety_subst_full(map(fun q=>(fst q,theta(snd q)))back)pat=mark_type back theta pat.
Proof.
 assert(All:forall pat fuel back theta,ety_depth pat<=fuel->
  ety_subst fuel(map(fun q=>(fst q,theta(snd q)))back)pat=mark_type back theta pat).
 {intro pat;induction pat as[| |g|n xs IH|ms IH|fs IH]using ety_ind';
  intros[|fuel]back theta Depth;try solve[cbn[ety_depth]in Depth;lia];
  cbn[ety_subst mark_type];try reflexivity.
  - rewrite(match_nassoc_back_map _ _ (fun _ p=>theta p));destruct(nassoc g back);reflexivity.
  - f_equal;apply map_ext_in;intros q Hq;f_equal;rewrite Forall_forall in IH;apply(IH q Hq);
    pose proof(family_depth_nom n xs q Hq);lia.
  - f_equal;apply map_ext_in;intros t Ht;rewrite Forall_forall in IH;apply(IH t Ht);
    pose proof(family_depth_union ms t Ht);lia.
  - f_equal;apply map_ext_in;intros q Hq;f_equal;rewrite Forall_forall in IH;apply(IH q Hq);
    pose proof(family_depth_record fs q Hq);lia. }
 intros;unfold ety_subst_full;apply All;lia.
Qed.
Definition inferred_marker_subst(back:list(nat*nat))(got:list(nat*ety)):=
 map(fun q=>(fst q,opt_default(YAbs(fst q))(nassoc(snd q)got)))back.
Theorem pmatch_full_reconstructs:forall pat back theta,
 MatchUnique pat->
 let target:=mark_type back theta pat in
 ety_subst_full(inferred_marker_subst back(pmatch_full back pat target[]))pat=target.
Proof.
 intros pat back theta Unique;cbn zeta;rewrite <-(ety_subst_full_mark_type pat back theta)at 2.
 unfold ety_subst_full at 1 2;apply ety_subst_lookup_extensional.
 intros g Hg;unfold inferred_marker_subst.
 rewrite(match_nassoc_back_map _ _ (fun g p=>opt_default(YAbs g)(nassoc p(pmatch_full back pat(mark_type back theta pat)[])))back g).
 rewrite(match_nassoc_back_map _ _ (fun _ p=>theta p)back g).
 destruct(nassoc g back)as[p|]eqn:Read;[|reflexivity].
 cbn[option_map];rewrite(pmatch_full_complete pat back theta Unique g p Hg Read);reflexivity.
Qed.
