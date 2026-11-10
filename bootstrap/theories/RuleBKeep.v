(* The rule B checks (Elab.requirements_in_force, family_inputs_in_force)
   change no state when they succeed. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Sx Elab ElabSound.
Import ListNotations.
(* The rule B checks change no state when they succeed. *)
Lemma miter_keep:forall X(f:X->M unit)l,(forall x s u s',f x s=Good(u,s')->s'=s)->
 forall s u s',miter f l s=Good(u,s')->s'=s.
Proof.
 intros X f l Hf. induction l as[|x l IH];intros s u s' E;cbn[miter]in E;[injection E as _ <-;reflexivity|].
 apply bind_inv in E as[u1[s1[E1 E]]]. rewrite(IH _ _ _ E). exact(Hf _ _ _ _ E1).
Qed.
Lemma check_keep:forall(b:bool)(m:M string)mi(msg:string->string)s u s',
 (if b then ret tt else let! x:=m in err mi(msg x))s=Good(u,s')->s'=s.
Proof.
 intros b m mi msg s u s' E. destruct b;[cbn in E;injection E as _ <-;reflexivity|].
 apply bind_inv in E as[x[s1[_ E]]]. destruct(err_not_good _ _ _ _ _ _ E).
Qed.
Lemma requirements_in_force_keep:forall mi e g info what s u s',
 requirements_in_force mi e g info what s=Good(u,s')->s'=s.
Proof.
 intros mi e g info what s u s' E. unfold requirements_in_force in E. eapply miter_keep;[|exact E].
 intros r s0 u0 s0' E0. cbn beta in E0. destruct(in_force_through e info r);[cbn in E0;injection E0 as _ <-;reflexivity|]. idtac.
 apply bind_inv in E0 as[x[s1[_ E0]]]. destruct(err_not_good _ _ _ _ _ _ E0).
Qed.
Lemma family_inputs_in_force_keep:forall mi e fg th s u s',
 family_inputs_in_force mi e fg th s=Good(u,s')->s'=s.
Proof.
 intros mi e fg th s u s' E. unfold family_inputs_in_force in E. eapply miter_keep;[|exact E].
 intros q s0 u0 s0' E0. eapply miter_keep;[|exact E0].
 intros t s1 u1 s1' E1. cbn beta in E1. destruct(_||_);[cbn in E1;injection E1 as _ <-;reflexivity|].
 apply bind_inv in E1 as[x[s2[_ E1]]]. apply bind_inv in E1 as[y[s3[_ E1]]]. destruct(err_not_good _ _ _ _ _ _ E1).
Qed.
