(* Inferred family substitutions preserve supported types and unique keys. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodyTr NominalSupport TypeReadingInj.
Import ListNotations.
Local Open Scope list_scope.
Lemma pmatch_fold_invariant:forall X Y(step:Y->X->Y)(J:Y->Prop)xs acc,
 J acc->(forall x,In x xs->forall a,J a->J(step a x))->J(fold_left step xs acc).
Proof.
 intros X Y step J xs;induction xs as[|x xs IH];intros acc H Step;[exact H|].
 cbn[fold_left];apply IH;[apply Step;[left;reflexivity|exact H]|].
 intros y Hy;apply Step;right;exact Hy.
Qed.
Lemma pmatch_combine_right:forall X Y(xs:list X)(ys:list Y)p,
 In p(combine xs ys)->In(snd p)ys.
Proof.
 intros X Y xs;induction xs as[|x xs IH];intros[|y ys][a b]H;cbn in H|-*;try contradiction.
 destruct H as[Eq|H];[injection Eq as <- <-;left;reflexivity|right;exact(IH ys(a,b)H)].
Qed.
Lemma pmatch_lookup_member:forall X k (xs:list(nat*X))y,nassoc k xs=Some y->In(k,y)xs.
Proof.
 intros X k xs;induction xs as[|[j v]xs IH];intros y H;[discriminate H|].
 cbn[nassoc assoc]in H;destruct(Nat.eqb k j)eqn:E.
 - apply Nat.eqb_eq in E;subst j;injection H as <-;left;reflexivity.
 - right;exact(IH _ H).
Qed.
Definition PmatchParts(P:ety->Prop):Prop:=
 (forall n xs,P(YNom n xs)->Forall(fun q=>P(snd q))xs)/\
 (forall ms,P(YUnion ms)->Forall P ms)/\
 (forall fs,P(YRec fs)->Forall(fun q=>P(snd q))fs).
Theorem pmatch_invariant:forall(P:ety->Prop)(J:list(nat*ety)->Prop)back,
 PmatchParts P->
 (forall acc p t,J acc->P t->(exists g,nassoc g back=Some p)->nassoc p acc=None->J(acc++[(p,t)]))->
 forall fuel pat t acc,P t->J acc->J(pmatch fuel back pat t acc).
Proof.
 intros P J back[Nom[Union Record]]Append fuel;induction fuel as[|f IH];intros pat t acc Pt Ja;[exact Ja|].
 destruct pat as[| |g|n xs|ms|fs];cbn[pmatch];try exact Ja.
 - destruct(nassoc g back)as[p|]eqn:B;[|exact Ja].
   destruct(nassoc p acc)as[y|]eqn:A;cbn[is_some];[exact Ja|].
   apply Append;[exact Ja|exact Pt|exists g;exact B|exact A].
 - destruct t as[| |g|n0 ys|ns|gs];try exact Ja.
   destruct(Nat.eqb n n0);[|exact Ja].
   eapply pmatch_fold_invariant;[exact Ja|].
   intros[k a]Hin cur Jcur;cbn[fst snd];destruct(nassoc k ys)as[y|]eqn:Y;[|exact Jcur].
   eapply IH;[|exact Jcur].
   pose proof(Nom _ _ Pt)as Parts;rewrite Forall_forall in Parts.
   exact(Parts(k,y)(pmatch_lookup_member _ _ _ _ Y)).
 - destruct t as[| |g|n ys|ns|gs];try exact Ja.
   destruct(Nat.eqb(List.length ms)(List.length ns));[|exact Ja].
   eapply pmatch_fold_invariant;[exact Ja|].
   intros[a b]Hin cur Jcur;cbn[fst snd];eapply IH;[|exact Jcur].
   pose proof(Union _ Pt)as Parts;rewrite Forall_forall in Parts.
   exact(Parts _ (pmatch_combine_right _ _ _ _ _ Hin)).
 - destruct t as[| |g|n ys|ns|gs];try exact Ja.
   destruct(Nat.eqb(List.length fs)(List.length gs));[|exact Ja].
   eapply pmatch_fold_invariant;[exact Ja|].
   intros[a b]Hin cur Jcur;cbn[fst snd];eapply IH;[|exact Jcur].
   pose proof(Record _ Pt)as Parts;rewrite Forall_forall in Parts.
   pose proof(pmatch_combine_right _ _ _ _ _ Hin)as Inb.
   apply in_map_iff in Inb as(q&Eq&Inq);cbn[fst snd]in Eq;rewrite <-Eq;exact(Parts _ Inq).
Qed.
Lemma pmatch_wf_parts:PmatchParts WFty.
Proof.
 split;[|split].
 - intros n xs H;unfold WFty in H;cbn[ety_wf]in H;apply forallb_Forall;exact H.
 - intros ms H;unfold WFty in H;destruct ms as[|m ms];[discriminate H|].
   cbn[ety_wf]in H;apply forallb_Forall in H.
   eapply Forall_impl;[|exact H];intros y Hy;destruct y;try discriminate Hy;exact Hy.
 - intros fs H;unfold WFty in H;cbn[ety_wf]in H;apply forallb_Forall;exact H.
Qed.
Lemma pmatch_support_parts:forall W,PmatchParts(Support W).
Proof.
 intro W;split;[|split];intros;inversion H;subst;assumption.
Qed.
Corollary pmatch_types_wf:forall fuel back pat t acc,
 WFty t->Forall(fun q=>WFty(snd q))acc->Forall(fun q=>WFty(snd q))(pmatch fuel back pat t acc).
Proof.
 intros fuel back pat t acc W A;eapply(pmatch_invariant WFty(fun xs=>Forall(fun q=>WFty(snd q))xs)back);
 [exact pmatch_wf_parts| |exact W|exact A].
 intros a p y H Wy From Fresh;apply Forall_app;split;[exact H|constructor;[exact Wy|constructor]].
Qed.
Corollary pmatch_types_support:forall W fuel back pat t acc,
 Support W t->Forall(fun q=>Support W(snd q))acc->Forall(fun q=>Support W(snd q))(pmatch fuel back pat t acc).
Proof.
 intros W fuel back pat t acc S A;eapply(pmatch_invariant(Support W)(fun xs=>Forall(fun q=>Support W(snd q))xs)back);
 [apply pmatch_support_parts| |exact S|exact A].
 intros a p y H Sy From Fresh;apply Forall_app;split;[exact H|constructor;[exact Sy|constructor]].
Qed.
Lemma pmatch_key_absent:forall k(xs:list(nat*ety)),nassoc k xs=None->~In k(map fst xs).
Proof.
 intros k xs;induction xs as[|[j y]xs IH];intros H Hin;[exact Hin|].
 cbn[nassoc assoc]in H;cbn[map fst snd In]in Hin.
 destruct(Nat.eqb k j)eqn:E;[discriminate H|].
 destruct Hin as[Eq|Hin];[subst j;rewrite Nat.eqb_refl in E;discriminate E|exact(IH H Hin)].
Qed.
Corollary pmatch_keys_nodup:forall fuel back pat t acc,
 NoDup(map fst acc)->NoDup(map fst(pmatch fuel back pat t acc)).
Proof.
 intros fuel back pat t acc N;eapply(pmatch_invariant(fun _=>True)(fun xs=>NoDup(map fst xs))back);
 [split;[|split];intros;apply Forall_forall;intros;exact I| |exact I|exact N].
 intros a p y H _ From Fresh;rewrite map_app;cbn[map fst snd].
 apply NoDup_app;[exact H|constructor;[intros[]|constructor]|].
 intros x Hin Hx;destruct Hx as[<-|[]];exact(pmatch_key_absent _ _ Fresh Hin).
Qed.
Corollary pmatch_keys_domain:forall fuel back pat t acc,
 Forall(fun q=>exists g,nassoc g back=Some(fst q))acc->
 Forall(fun q=>exists g,nassoc g back=Some(fst q))(pmatch fuel back pat t acc).
Proof.
 intros fuel back pat t acc D;eapply(pmatch_invariant(fun _=>True)(fun xs=>Forall(fun q=>exists g,nassoc g back=Some(fst q))xs)back);
 [split;[|split];intros;apply Forall_forall;intros;exact I| |exact I|exact D].
 intros a p y H _ From Fresh;apply Forall_app;split;[exact H|constructor;[exact From|constructor]].
Qed.
