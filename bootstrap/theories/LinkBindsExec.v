(* The actual statement-to-bind visitor agrees with the declarative wrappers. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabSound BindItemBinds.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition link_statement_binds(xs:list xstmt):M(list xbind):=
 mmap(fun x=>match stmt_bind x with Some b=>ret b|None=>fail "internal: a functor body made a statement" end)xs.
Lemma link_statement_binds_exact : forall xs s raw final,
 link_statement_binds xs s=Good(raw,final) ->
 forall typed,A.BindsOf(fold_stmts xs XUnit)typed -> raw=typed.
Proof.
 intros xs;induction xs as[|head rest IH];intros s raw final Run typed Typed.
 - injection Run as <- _;inversion Typed;reflexivity.
 - unfold link_statement_binds in Run;cbn[mmap]in Run.
   apply bind_inv in Run as[first[mid[Head Run]]].
   apply bind_inv in Run as[tail[last[Tail Ret]]];injection Ret as <- <-.
   change(link_statement_binds rest mid=Good(tail,last))in Tail.
   destruct head;cbn[stmt_bind]in Head;try discriminate Head;
    injection Head as <- <-;inversion Typed;subst;f_equal;eapply IH;eassumption.
Qed.
Theorem actual_link_binds : forall W P F C its wrap Cout Fout made xs s raw final,
 A.BindItems W P F C its wrap Cout Fout made ->
 (forall tail,wrap tail=fold_stmts xs tail) ->
 link_statement_binds xs s=Good(raw,final) ->
 A.BindsOf(wrap XUnit)raw.
Proof.
 intros W P F C its wrap Cout Fout made xs s raw final Typed Fold Run.
 destruct(binditems_have_binds W P _ _ _ _ _ _ _ Typed)as[binds Binds].
 assert(raw=binds)by(eapply link_statement_binds_exact;[exact Run|rewrite <-Fold;exact Binds]).
 subst raw;exact Binds.
Qed.
