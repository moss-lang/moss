(* The executable method aliases are precisely the declarative refile operation. *)
From Stdlib Require Import List Bool.
From NM Require Import ExecutionModel NMap Sx Elab LinkBodyTrace.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition link_output_rename_pairs(pairs:list(item*item)):list(ty*nat*(ty*subst)):=
 flat_map(fun p=>match p with
 |(IMth t m _,IMth t0 _ th0)=>[(t,m,(t0,th0))]|_=>[] end)pairs.
Lemma link_output_flat_map_compose:forall X Y Z(f:Y->list Z)(g:X->list Y)xs,
 flat_map f(flat_map g xs)=flat_map(fun x=>flat_map f(g x))xs.
Proof.
 intros X Y Z f g xs;induction xs as[|x xs IH];[reflexivity|].
 cbn[flat_map];rewrite flat_map_app,IH;reflexivity.
Qed.
Lemma link_output_refile_one:forall pairs b,
 link_trace_rebinds[b](link_output_rename_pairs pairs)=A.refile pairs b.
Proof.
 intros pairs b;destruct b;try reflexivity.
 cbn[link_trace_rebinds flat_map List.app A.refile];rewrite app_nil_r;f_equal.
 unfold link_output_rename_pairs;rewrite link_output_flat_map_compose.
 apply flat_map_ext;intros[source target];destruct source;destruct target;cbn[flat_map];try rewrite app_nil_r;reflexivity.
Qed.
Lemma link_output_refile:forall raw pairs,
 link_trace_rebinds raw(link_output_rename_pairs pairs)=flat_map(A.refile pairs)raw.
Proof.
 intros raw pairs;unfold link_trace_rebinds;apply flat_map_ext;intro b.
 pose proof(link_output_refile_one pairs b)as One.
 unfold link_trace_rebinds in One;cbn[flat_map]in One;rewrite app_nil_r in One;exact One.
Qed.
