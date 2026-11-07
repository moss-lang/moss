(* Bind-item derivations contain only core bind wrappers. *)
From Stdlib Require Import List.
From NM Require Import Audit Sx.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Binds.
Variables(W:A.world)(P:xprogram).
Lemma capture_prefix_binds : forall F qs tail endbinds,A.BindsOf tail endbinds ->
 exists out,A.BindsOf(fold_right(fun q x=>XBindVal(snd q)(XVar(A.var_of F(fst q)))x)tail qs)out.
Proof.
 intros F qs;induction qs as[|[name value]rest IH];intros tail endbinds Tail;cbn.
 - exists endbinds;exact Tail.
 - destruct(IH tail endbinds Tail)as[out Done];eexists;apply A.BOVal;exact Done.
Qed.
Lemma lambda_prefix_binds : forall F C ps rt body sg this pid pre sm,
 A.LambdaProvider W P F C ps rt body sg this pid pre sm ->
 forall tail endbinds,A.BindsOf tail endbinds -> exists out,A.BindsOf(pre tail)out.
Proof.
 intros F C ps rt body sg this pid pre sm H;destruct H;intros tail endbinds Tail.
 cbn beta;eapply capture_prefix_binds;exact Tail.
Qed.
Lemma method_prefix_binds : forall F C y sg rt target pre sm,
 A.MethProvider W P F C y sg rt target pre sm ->
 forall tail endbinds,A.BindsOf tail endbinds -> exists out,A.BindsOf(pre tail)out.
Proof.
 intros F C y sg rt target pre sm H;destruct H;intros tail endbinds Tail.
 - exists endbinds;exact Tail.
 - exists endbinds;exact Tail.
 - eapply lambda_prefix_binds;eassumption.
Qed.

Fixpoint binditems_binds F C its wrap Cout Fout made
 (H:A.BindItems W P F C its wrap Cout Fout made){struct H}:
 forall tail endbinds,A.BindsOf tail endbinds -> exists out,A.BindsOf(wrap tail)out
with binditem_binds F C it wrap Cout Fout made
 (H:A.BindItem W P F C it wrap Cout Fout made){struct H}:
 forall tail endbinds,A.BindsOf tail endbinds -> exists out,A.BindsOf(wrap tail)out.
Proof.
 - destruct H;intros tail endbinds Tail.
   + exists endbinds;exact Tail.
   + destruct(binditems_binds _ _ _ _ _ _ _ H0 tail endbinds Tail)as[mid M].
     exact(binditem_binds _ _ _ _ _ _ _ H _ mid M).
 - destruct H;intros tail endbinds Tail;
   try(solve[eexists;constructor;exact Tail]).
   + eapply lambda_prefix_binds;[eassumption|]. apply A.BOFn;exact Tail.
   + eapply method_prefix_binds;[eassumption|]. apply A.BOMth;exact Tail.
   + eapply binditems_binds;eassumption.
Qed.
Theorem binditems_have_binds : forall F C its wrap Cout Fout made,
 A.BindItems W P F C its wrap Cout Fout made ->
 exists out,A.BindsOf(wrap XUnit)out.
Proof. intros;eapply binditems_binds;[eassumption|constructor]. Qed.
End Binds.
