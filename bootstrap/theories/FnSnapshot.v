(* A function provision retains the declaration reading at its own capture.
   Its type-name snapshot is deliberately independent of the current c_ty. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition FnSnapshots(W:A.world)(C:A.cx):Prop:=forall f sg,
 A.nlook f(A.c_fns C)=Some sg->exists tm a,
 A.SigOf W f tm None sg /\A.AppOf W f tm a /\A.ilook(IFn f)(A.c_apps C)=Some a.
Lemma fn_snapshots_tables:forall W C C',FnSnapshots W C->A.c_fns C'=A.c_fns C->
 (forall f,A.ilook(IFn f)(A.c_apps C')=A.ilook(IFn f)(A.c_apps C))->FnSnapshots W C'.
Proof. intros W C C' H Fns Apps f sg E;rewrite Fns in E;destruct(H _ _ E)as[tm[a[Sig[App Look]]]].
 exists tm,a;split;[exact Sig|]. split;[exact App|rewrite Apps;exact Look]. Qed.
Lemma fn_snapshots_local:forall W C n t mu,FnSnapshots W C->FnSnapshots W(A.add_local C n t mu).
Proof. intros;eapply fn_snapshots_tables;[eassumption|reflexivity|intro;reflexivity]. Qed.
Lemma fn_snapshots_ty:forall W C t tau,FnSnapshots W C->FnSnapshots W(A.set_ty C t tau).
Proof. intros;eapply fn_snapshots_tables;[eassumption|reflexivity|intro;reflexivity]. Qed.
Lemma fn_snapshots_val:forall W C v tau,FnSnapshots W C->FnSnapshots W(A.set_val C v tau).
Proof. intros;eapply fn_snapshots_tables;[eassumption|reflexivity|intro;reflexivity]. Qed.
Lemma fn_snapshots_mth:forall W C r m sg,FnSnapshots W C->FnSnapshots W(A.set_mth C r m sg).
Proof. intros;eapply fn_snapshots_tables;[eassumption|reflexivity|intro;reflexivity]. Qed.
Lemma fn_snapshots_fun:forall W C u fx,FnSnapshots W C->FnSnapshots W(A.set_fun C u fx).
Proof. intros;eapply fn_snapshots_tables;[eassumption|reflexivity|intro;reflexivity]. Qed.
Lemma fn_snapshots_provide_other:forall W C k a C',FnSnapshots W C'->
 (forall f,item_eqb(IFn f)k=false)->FnSnapshots W(A.provide C k a C').
Proof. intros W C k a C' H Ne;eapply fn_snapshots_tables;[exact H|reflexivity|].
 intro f;cbn[A.provide A.c_apps A.ilook];rewrite Ne;reflexivity. Qed.
Lemma fn_snapshots_function:forall W C f sg tm a,FnSnapshots W C->
 A.SigOf W f tm None sg->A.AppOf W f tm a->
 FnSnapshots W(A.provide C(IFn f)a(A.set_fn C f sg)).
Proof.
 intros W C f sg tm a H Sig App g actual E;cbn[A.provide A.set_fn A.c_fns A.nlook]in E.
 destruct(Nat.eqb g f)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst g;injection E as <-;exists tm,a;split;[exact Sig|]. split;[exact App|].
   cbn[A.provide A.set_fn A.c_apps A.ilook item_eqb];rewrite Nat.eqb_refl;reflexivity.
 - destruct(H _ _ E)as[snapshot[old[SG[AP LK]]]]. exists snapshot,old;split;[exact SG|]. split;[exact AP|].
   cbn[A.provide A.set_fn A.c_apps A.ilook item_eqb];rewrite Eq;exact LK.
Qed.
