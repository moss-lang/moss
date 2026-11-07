(* Actual fixed-input guards, independent of body induction callbacks. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Sx Elab ElabSound ElabFrame BodyTr.
Import ListNotations.
Local Open Scope list_scope.
Lemma copy_guard_miter : forall X (f:X->M unit) xs s s',
 miter f xs s=Good(tt,s') ->
 (forall x a b,In x xs ->f x a=Good(tt,b) ->a=b) ->
 s'=s /\ forall x,In x xs ->f x s=Good(tt,s).
Proof.
 intros X f xs;induction xs as[|x rest IH]; intros s s' H Pure;cbn[miter]in H.
 - injection H as <-;split;[reflexivity|intros y F;contradiction].
 - apply bind_inv in H as(u&s1&A&B). destruct u.
   assert(E:s=s1)by(eapply Pure;[left; reflexivity|exact A]); subst s1.
   destruct(IH s s' B)as[E All].
   + intros y a b Iny Hf;eapply Pure;[right;exact Iny|exact Hf].
   + split;[exact E|]. intros y[<-|Iny]; [exact A|apply All;exact Iny].
Qed.
Lemma bind_fixed_present_success : forall mi e fg xs s s',
 bind_fixed_present mi e fg xs s=Good(tt,s') ->
 s'=s /\ forall x,In x xs ->tn_get e x<>None.
Proof.
 intros mi e fg xs s s' H;unfold bind_fixed_present in H.
 destruct(copy_guard_miter _ _ _ _ _ H)as[E All].
 - intros x a b _ Hx;destruct(tn_get e x)eqn:T;cbn[is_some]in Hx;
   [injection Hx as <-; reflexivity|].
   apply bind_inv in Hx as(g&st&S&Bad);discriminate.
 - split;[exact E|]. intros x Inx;specialize(All x Inx).
   destruct(tn_get e x)eqn:T;[discriminate|]. cbn[is_some]in All.
   apply bind_inv in All as(g&st&S&Bad);discriminate.
Qed.
Definition SourceFixedAgreement(e:env)(u x:nat):Prop :=
 match get_app e(KyFun u),tn_get e x with
 |Some(a,_),Some now=>match assoc Nat.eqb x a with
    |Some(Some was)=>canon e was=canon e now|_=>True end
 |_,_=>True end.
Lemma copy_fixed_current_success : forall mi e fg xs s s',
 copy_fixed_current mi e fg xs s=Good(tt,s') ->
 s'=s /\ forall x,In x xs ->SourceFixedAgreement e(gs_id fg)x.
Proof.
 intros mi e fg xs s s' H;unfold copy_fixed_current in H.
 destruct(copy_guard_miter _ _ _ _ _ H)as[E All].
 - intros x a b _ Hx.
   destruct(get_app e(KyFun(gs_id fg)))as[[app stamp]|]eqn:G;
   destruct(assoc Nat.eqb x _)as[[was|]|]eqn:A;
   destruct(tn_get e x)as[now|]eqn:N;try(injection Hx as <-; reflexivity).
   all:destruct(ety_eqb(canon e was)(canon e now))eqn:C;
     [injection Hx as <-; reflexivity|apply bind_inv in Hx as(g&st&S&Bad);discriminate].
 - split;[exact E|]. intros x Inx;specialize(All x Inx).
   unfold SourceFixedAgreement.
   destruct(get_app e(KyFun(gs_id fg)))as[[app stamp]|]eqn:G;[|exact I].
   destruct(assoc Nat.eqb x app)as[[was|]|]eqn:A;[|destruct(tn_get e x);exact I|destruct(tn_get e x);exact I].
   destruct(tn_get e x)as[now|]eqn:N;[|exact I].
   destruct(ety_eqb(canon e was)(canon e now))eqn:C;[apply ety_eqb_true;exact C|].
   apply bind_inv in All as(g&st&S&Bad);discriminate.
Qed.
