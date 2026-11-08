(* Family matching traverses the entire canonical receiver, at any depth. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab BodyTr.
Import ListNotations.
Local Open Scope list_scope.
Lemma family_depth_max:forall xs n,In n xs->n<=fold_right Nat.max 0 xs.
Proof.
 intros xs;induction xs as[|x xs IH];intros n H;[contradiction|].
 cbn[fold_right];destruct H as[<-|H];[apply Nat.le_max_l|].
 eapply Nat.le_trans;[exact(IH _ H)|apply Nat.le_max_r].
Qed.
Lemma family_depth_nom:forall head xs q,In q xs->ety_depth(snd q)<ety_depth(YNom head xs).
Proof.
 intros head xs q H;cbn[ety_depth].
 assert(ety_depth(snd q)<=fold_right Nat.max 0(map(fun q=>ety_depth(snd q))xs)).
 {apply family_depth_max;apply in_map_iff;exists q;split;[reflexivity|exact H]. }lia.
Qed.
Lemma family_depth_union:forall xs q,In q xs->ety_depth q<ety_depth(YUnion xs).
Proof.
 intros xs q H;cbn[ety_depth].
 assert(ety_depth q<=fold_right Nat.max 0(map ety_depth xs)).
 {apply family_depth_max;apply in_map_iff;exists q;split;[reflexivity|exact H]. }lia.
Qed.
Lemma family_depth_record:forall xs q,In q xs->ety_depth(snd q)<ety_depth(YRec xs).
Proof.
 intros xs q H;cbn[ety_depth].
 assert(ety_depth(snd q)<=fold_right Nat.max 0(map(fun q=>ety_depth(snd q))xs)).
 {apply family_depth_max;apply in_map_iff;exists q;split;[reflexivity|exact H]. }lia.
Qed.
Lemma family_fold_ext:forall X Y(f g:Y->X->Y)xs acc,
 (forall x,In x xs->forall a,f a x=g a x)->fold_left f xs acc=fold_left g xs acc.
Proof.
 intros X Y f g xs;induction xs as[|x xs IH];intros acc Eq;[reflexivity|].
 cbn[fold_left];rewrite(Eq x(or_introl eq_refl)acc);apply IH.
 intros z Hz;apply Eq;right;exact Hz.
Qed.
Lemma family_combine_left:forall X Y(xs:list X)(ys:list Y)p,
 In p(combine xs ys)->In(fst p)xs.
Proof.
 intros X Y xs;induction xs as[|x xs IH];intros[|y ys][a b]H;cbn in H|-*;try contradiction.
 destruct H as[Eq|H];[injection Eq as <- <-;left;reflexivity|right;exact(IH ys(a,b)H)].
Qed.
Theorem pmatch_fuel_stable:forall pat n m back t acc,
 ety_depth pat<=n->ety_depth pat<=m->pmatch n back pat t acc=pmatch m back pat t acc.
Proof.
 intros pat;induction pat as[| |id|head xs IH|ms IH|fs IH]using ety_ind';
 intros[|n][|m]back t acc N M;try solve[cbn[ety_depth]in N,M;lia];
 cbn[pmatch];try reflexivity.
 - destruct t as[| |id|head0 ys|ns|gs];try reflexivity.
   destruct(Nat.eqb head head0);[|reflexivity].
   apply family_fold_ext;intros q Hq a;destruct(nassoc(fst q)ys);[|reflexivity].
   rewrite Forall_forall in IH;apply(IH q Hq);
   pose proof(family_depth_nom head xs q Hq);lia.
 - destruct t as[| |id|head ys|ns|gs];try reflexivity.
   destruct(Nat.eqb(List.length ms)(List.length ns));[|reflexivity].
   apply family_fold_ext;intros q Hq a.
   pose proof(family_combine_left _ _ _ _ _ Hq)as Hp.
   rewrite Forall_forall in IH;apply(IH(fst q)Hp);
   pose proof(family_depth_union ms(fst q)Hp);lia.
 - destruct t as[| |id|head ys|ns|gs];try reflexivity.
   destruct(Nat.eqb(List.length fs)(List.length gs));[|reflexivity].
   apply family_fold_ext;intros q Hq a.
   pose proof(family_combine_left _ _ _ _ _ Hq)as Hp.
   apply in_map_iff in Hp as(p&Eq&Hp).
   rewrite Forall_forall in IH;rewrite <-Eq;apply(IH p Hp);
   pose proof(family_depth_record fs p Hp);lia.
Qed.
Theorem ety_subst_fuel_stable:forall t n m sub,
 ety_depth t<=n->ety_depth t<=m->ety_subst n sub t=ety_subst m sub t.
Proof.
 intros t;induction t as[| |id|head xs IH|ms IH|fs IH]using ety_ind';
 intros[|n][|m]sub N M;try solve[cbn[ety_depth]in N,M;lia];
 cbn[ety_subst];try reflexivity;f_equal;apply map_ext_in;intros q Hq.
 - f_equal;rewrite Forall_forall in IH;apply(IH q Hq);
   pose proof(family_depth_nom head xs q Hq);lia.
 - rewrite Forall_forall in IH;apply(IH q Hq);
   pose proof(family_depth_union ms q Hq);lia.
 - f_equal;rewrite Forall_forall in IH;apply(IH q Hq);
   pose proof(family_depth_record fs q Hq);lia.
Qed.
Corollary pmatch_full_preserves_bounded:forall pat n back t acc,
 ety_depth pat<=n->pmatch_full back pat t acc=pmatch n back pat t acc.
Proof. intros;unfold pmatch_full;apply pmatch_fuel_stable;lia. Qed.
Corollary ety_subst_full_preserves_bounded:forall t n sub,
 ety_depth t<=n->ety_subst_full sub t=ety_subst n sub t.
Proof. intros;unfold ety_subst_full;apply ety_subst_fuel_stable;lia. Qed.
Fixpoint family_deep(n:nat)(leaf:ety):ety:=
 match n with 0=>leaf|S n=>YNom 7[(1,family_deep n leaf)]end.
Lemma family_deep_depth:forall n leaf,ety_depth(family_deep n leaf)=n+ety_depth leaf.
Proof.
 intros n;induction n as[|n IH];intro leaf;[reflexivity|].
 cbn[family_deep ety_depth map fold_right snd];rewrite Nat.max_0_r,IH;lia.
Qed.
Lemma family_match_truncated:forall n,
 pmatch n[(9,3)](family_deep n(YAbs 9))(family_deep n YUnit)[]=[].
Proof.
 induction n as[|n IH];[reflexivity|].
 cbn[family_deep pmatch Nat.eqb fold_left fst snd nassoc assoc];exact IH.
Qed.
Lemma family_match_complete:forall n,
 pmatch(S n)[(9,3)](family_deep n(YAbs 9))(family_deep n YUnit)[]=[(3,YUnit)].
Proof.
 induction n as[|n IH];[reflexivity|].
 cbn[family_deep pmatch Nat.eqb fold_left fst snd nassoc assoc];exact IH.
Qed.
Example family_match_depth5000_was_truncated:
 pmatch 5000[(9,3)](family_deep 5000(YAbs 9))(family_deep 5000 YUnit)[]=[].
Proof. apply family_match_truncated. Qed.
Example family_match_depth5000_now_complete:
 pmatch_full[(9,3)](family_deep 5000(YAbs 9))(family_deep 5000 YUnit)[]=[(3,YUnit)].
Proof.
 unfold pmatch_full;rewrite family_deep_depth.
 replace(5000+ety_depth(YAbs 9))with(S 5000)by reflexivity.
 apply family_match_complete.
Qed.
Lemma family_subst_complete:forall n,
 ety_subst(S n)[(9,YUnit)](family_deep n(YAbs 9))=family_deep n YUnit.
Proof.
 induction n as[|n IH];[reflexivity|].
 change(YNom 7[(1,ety_subst(S n)[(9,YUnit)](family_deep n(YAbs 9)))]=YNom 7[(1,family_deep n YUnit)]).
 rewrite IH;reflexivity.
Qed.
Lemma family_subst_truncated:forall n,
 ety_subst n[(9,YUnit)](family_deep n(YAbs 9))=family_deep n(YAbs 9).
Proof.
 induction n as[|n IH];[reflexivity|].
 change(YNom 7[(1,ety_subst n[(9,YUnit)](family_deep n(YAbs 9)))]=YNom 7[(1,family_deep n(YAbs 9))]).
 rewrite IH;reflexivity.
Qed.
Example family_subst_depth5000_now_complete:
 ety_subst_full[(9,YUnit)](family_deep 5000(YAbs 9))=family_deep 5000 YUnit.
Proof.
 unfold ety_subst_full;rewrite family_deep_depth.
 replace(5000+ety_depth(YAbs 9))with(S 5000)by reflexivity.
 apply family_subst_complete.
Qed.
