(* Cache and forwarder invariant tools, independent of the body correctness induction. *)
From Stdlib Require Import List.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore ReqsSolve.
From NM Require Export BodySoundCore InfoPreserve ElabInv FwdReadonly FwdInvariant FwdPreserve FwdSynthInv.
Import ListNotations.

Lemma info_vars : forall vs n s,InfoInv s->InfoInv(s_vars vs n s).
Proof.
 intros vs n s I. apply(info_step s);[exact I| |reflexivity|reflexivity].
 constructor;cbn;auto;[exists[];rewrite app_nil_r;reflexivity|apply ext_refl].
Qed.

Ltac cache_inv :=
 first [assumption|match goal with
 | |-InfoInv(s_vars ?vs ?n ?s)=>apply info_vars;cache_inv
 | E : ?m ?si = Good (?a,?sf) |- InfoInv ?sf =>
   tryif constr_eq si sf then fail else idtac;
   let O:=fresh "OI" in assert(O:okI m)by(first[solve[auto with okIdb]|okIt]);
   eapply(proj2 O si a sf);[eassumption|cache_inv|exact E]
 end].

Lemma fwd_vars : forall vs n s,FwdInv s->FwdInv(s_vars vs n s).
Proof. intros vs n s I;exact I. Qed.
Ltac fwd_inv :=
 first [assumption|match goal with
 | |-FwdInv(s_vars ?vs ?n ?s)=>apply fwd_vars;fwd_inv
 | E : ?m ?si = Good (?a,?sf), F : Fr ?si ?sf |- FwdInv ?sf =>
   tryif constr_eq si sf then fail else idtac;
   let K:=fresh "KF" in assert(K:keeps st_fwd m)by(first[solve[auto with keepsdb]|keepst]);
   eapply fwd_inv_frame;[fwd_inv|exact F|exact(K si a sf E)]
 | E : ?m ?si = Good (?a,?sf) |- FwdInv ?sf =>
   tryif constr_eq si sf then fail else idtac;
   let O:=fresh "OF" in assert(O:okF m)by(first[solve[auto with fwdb]|fwt]);
   eapply(proj2 O si a sf);[eassumption|fwd_inv|exact E]
 end].

