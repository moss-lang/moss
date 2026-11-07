(* The post-body pin check preserves state and the target's result inputs. *)
From Stdlib Require Import List String Bool.
From NM Require Import Audit NMap Sx Elab ElabSound BodyTr.
Import ListNotations.
Theorem check_link_pins_success : forall mi e pins s final,
 check_link_pins mi e pins s=Good(tt,final)->
 final=s /\ forall t,In t pins ->
 canon e(opt_default(YAbs t)(tn_get e t))=canon e(YAbs t).
Proof.
 intros mi e pins;induction pins as[|pin pins IH];intros s final Run.
 - cbn[check_link_pins miter]in Run;injection Run as <-;split;[reflexivity|intros;contradiction].
 - cbn[check_link_pins miter]in Run.
   apply bind_inv in Run as[u[mid[First Tail]]].
   destruct(ety_eqb(canon e(opt_default(YAbs pin)(tn_get e pin)))(canon e(YAbs pin)))eqn:Same.
   + injection First as <- <-.
     destruct(IH _ _ Tail)as[-> Remaining];split;[reflexivity|].
     intros t[<-|Member];[apply ety_eqb_true;exact Same|apply Remaining;exact Member].
   + apply bind_inv in First as[g[bad[Lookup Error]]].
     exfalso;exact(err_not_good _ _ _ _ _ _ Error).
Qed.
Corollary check_link_pins_state : forall mi e pins s final,
 check_link_pins mi e pins s=Good(tt,final)->final=s.
Proof. intros;eapply check_link_pins_success;eassumption. Qed.
