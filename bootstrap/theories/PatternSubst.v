(* Pattern and payload judgments commute with type substitution. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit TyOfSubst.
Import ListNotations.
Module A := Audit.Elaboration.

Lemma payload_subst : forall W sb n xs pay,
  A.PayloadOf W n xs pay ->
  A.PayloadOf W n (smap sb xs) (Audit.app_subst sb pay).
Proof.
  intros W sb n xs pay H. destruct H as [s xs d name ps t pay Hd Da Ty].
  econstructor; [exact Hd|exact Da|].
  apply (tyof_subst W sb _ _ _ _ _ Ty (smap sb xs)).
  intro k. apply nlook_smap.
Qed.

Lemma nominal_member_subst : forall sb st n xs,
  In (TNom n xs) (A.members st) ->
  In (TNom n (smap sb xs)) (A.members (Audit.app_subst sb st)).
Proof.
  intros sb st n xs H. destruct st as [|g|m ys|ms]; cbn [A.members Audit.app_subst] in *.
  - destruct H as [H|[]]; discriminate.
  - destruct H as [H|[]]; discriminate.
  - destruct H as [H|[]]. injection H as -> ->. left. apply asub_nom.
  - change (In (TNom n (smap sb xs)) (map (Audit.app_subst sb) ms)).
    rewrite <- asub_nom. apply in_map. exact H.
Qed.

Lemma fieldpats_subst : forall W F fs pats bs xs sb,
  A.FieldPats W F fs pats bs xs ->
  A.FieldPats W F (smap sb fs) pats (map (fun q => (fst q,Audit.app_subst sb (snd q))) bs) xs.
Proof.
  intros W F fs pats bs xs sb H. induction H; cbn [map fst snd].
  - constructor.
  - econstructor; [rewrite nlook_smap, H; reflexivity|exact H0|exact IHFieldPats].
  - econstructor; [rewrite nlook_smap, H; reflexivity|exact IHFieldPats].
  - econstructor; [rewrite nlook_smap, H; reflexivity|exact H0|exact H1|exact IHFieldPats].
Qed.

Theorem pattern_subst : forall W F st p xp bs hs ca sb,
  A.PatOf W F st p xp bs hs ca ->
  A.PatOf W F (Audit.app_subst sb st) p xp
    (map (fun q => (fst q,Audit.app_subst sb (snd q))) bs) hs ca.
Proof.
  intros W F st p xp bs hs ca sb H.
  destruct H as [|n Hr Hn|pa s k xs Hr Hk K Hin|pa s xs y pay Hr Hk Hin Hp Hy Hn|pa s xs fs ftys bs fxs Hr Hk Hin Hp Hf]; cbn [map fst snd].
  - constructor.
  - constructor; assumption.
  - eapply A.PUnit with (xs:=smap sb xs); [exact Hr|exact Hk|exact K|].
    exact (nominal_member_subst sb st s xs Hin).
  - eapply A.PTagBind with (xs:=smap sb xs); [exact Hr|exact Hk|exact (nominal_member_subst sb st s xs Hin)|apply payload_subst; exact Hp|exact Hy|exact Hn].
  - eapply A.PRecord with (xs:=smap sb xs); [exact Hr|exact Hk|exact (nominal_member_subst sb st s xs Hin)| |apply fieldpats_subst; exact Hf].
    pose proof (payload_subst _ sb _ _ _ Hp) as Hps. rewrite asub_nom in Hps. exact Hps.
Qed.
