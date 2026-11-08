(* Canonicalization uses only generations in the input and merge ranges. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab BodyTr NominalSupport.
Import ListNotations.
Definition GensIn(P:nat->Prop)(t:ety):Prop:=forall g,In g(ety_gens t)->P g.
Lemma gens_nom_parts:forall P n xs,
 GensIn P(YNom n xs)<->Forall(fun q=>GensIn P(snd q))xs.
Proof.
 intros P n xs;split.
 - intros H;apply Forall_forall;intros q Hq g Hg;apply H.
   change(In g(flat_map(fun q=>ety_gens(snd q))xs));apply in_flat_map;exists q;split;assumption.
 - intros H g Hg;change(In g(flat_map(fun q=>ety_gens(snd q))xs))in Hg.
   apply in_flat_map in Hg as(q&Hq&Hg);rewrite Forall_forall in H;exact(H q Hq g Hg).
Qed.
Lemma gens_union_parts:forall P ms,
 GensIn P(YUnion ms)<->Forall(GensIn P)ms.
Proof.
 intros P ms;split.
 - intros H;apply Forall_forall;intros t Ht g Hg;apply H.
   change(In g(flat_map ety_gens ms));apply in_flat_map;exists t;split;assumption.
 - intros H g Hg;change(In g(flat_map ety_gens ms))in Hg.
   apply in_flat_map in Hg as(t&Ht&Hg);rewrite Forall_forall in H;exact(H t Ht g Hg).
Qed.
Lemma gens_record_parts:forall P fs,
 GensIn P(YRec fs)<->Forall(fun q=>GensIn P(snd q))fs.
Proof.
 intros P fs;split.
 - intros H;apply Forall_forall;intros q Hq g Hg;apply H.
   change(In g(flat_map(fun q=>ety_gens(snd q))fs));apply in_flat_map;exists q;split;assumption.
 - intros H g Hg;change(In g(flat_map(fun q=>ety_gens(snd q))fs))in Hg.
   apply in_flat_map in Hg as(q&Hq&Hg);rewrite Forall_forall in H;exact(H q Hq g Hg).
Qed.
Theorem canon_go_generations:forall P uf,
 (forall k y,In(k,y)uf->GensIn P y)->forall fuel t,GensIn P t->GensIn P(canon_go fuel uf t).
Proof.
 intros P uf U fuel;induction fuel as[|fuel IHfuel];intros t Ht;[exact Ht|].
 revert Ht;induction t as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intro Ht;
 cbn[canon_go];try exact Ht.
 - destruct(nassoc g uf)as[y|]eqn:Read;[|exact Ht].
   destruct(ety_eqb y(YAbs g));[exact Ht|apply IHfuel].
   apply(U g y);eapply support_nassoc_in;exact Read.
 - apply gens_nom_parts;apply gens_nom_parts in Ht.
   rewrite Forall_forall in IH,Ht|-*;intros q Hq;apply in_map_iff in Hq as(p&<-&Hp).
   cbn[snd];exact(IH p Hp(Ht p Hp)).
 - apply gens_union_parts;apply gens_union_parts in Ht.
   rewrite Forall_forall in IH,Ht|-*;intros y Hy;apply in_map_iff in Hy as(t&<-&Ht0).
   exact(IH t Ht0(Ht t Ht0)).
 - apply gens_record_parts;apply gens_record_parts in Ht.
   rewrite Forall_forall in IH,Ht|-*;intros q Hq;apply in_map_iff in Hq as(p&<-&Hp).
   cbn[snd];exact(IH p Hp(Ht p Hp)).
Qed.
Corollary canon_generations:forall P e,
 (forall k y,In(k,y)(en_uf e)->GensIn P y)->forall t,GensIn P t->GensIn P(canon e t).
Proof. intros;unfold canon;eapply canon_go_generations;eauto. Qed.
