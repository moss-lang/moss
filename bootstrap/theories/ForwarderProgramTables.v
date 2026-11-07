(* Exact final function table, independent of cache/body soundness. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Definition ForwarderProgramTables(m:M xprogram):Prop:=forall s p final,
 m s=Good(p,final)->Audit.xo_fns p=st_fns final.
Arguments ForwarderProgramTables:simpl never.
Lemma forwarder_tables_bind:forall A(m:M A)(k:A->M xprogram),
 (forall a,ForwarderProgramTables(k a))->ForwarderProgramTables(mbind m k).
Proof.
 intros A m k K s p final Run;apply bind_inv in Run as[a[mid[First Last]]];exact(K a mid p final Last).
Qed.
Lemma forwarder_tables_snapshot:forall A(m:st->M A)(make:st->A->xprogram),
 (forall s,keeps st_fns(m s))->
 (forall s a,Audit.xo_fns(make s a)=st_fns s)->
 ForwarderProgramTables(mbind get(fun s=>mbind(m s)(fun a=>ret(make s a)))).
Proof.
 intros A m make Keep Shape s p final Run.
 apply bind_inv in Run as[snapshot[mid[Read Run]]];injection Read as <- <-.
 apply bind_inv in Run as[a[next[Body Run]]];injection Run as <- <-.
 rewrite Shape; symmetry;exact(Keep s s a next Body).
Qed.
Ltac forwarder_tables_t:=repeat(fintros;first[
 solve[eapply forwarder_tables_snapshot;[intro;keepst|intros;reflexivity]]|
 lazymatch goal with
 | |-ForwarderProgramTables(fun _=>Bad _)=>unfold ForwarderProgramTables;intros;discriminate
 | |-ForwarderProgramTables(mbind _ _)=>apply forwarder_tables_bind
 | |-ForwarderProgramTables ?m=>first[
    match m with context[match ?x with _=>_ end]=>destruct x end|
    let h:=head_of m in progress unfold h]
 end]).
Lemma forwarder_elab_program_tables:ForwarderProgramTables elab_program.
Proof. unfold elab_program;forwarder_tables_t. Qed.
