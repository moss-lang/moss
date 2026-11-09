(* Substitution of output bracket readings, without input relevance gates. *)
From Stdlib Require Import List Bool.
From NM Require Import ExecutionModel TyOfSubst.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma output_brackets_subst:forall W sb p tm a bs,
 A.OutputBrackets W p tm a bs ->forall tm',SubRel sb tm tm' ->
 A.OutputBrackets W p tm' a(smap sb bs).
Proof.
 intros W sb p tm a bs H tm' SR;destruct H as[|bs l F].
 - constructor.
 - unfold smap;rewrite map_rev;apply A.OBrSome.
   induction F as[|b tb bs l(sp&sa&E&R&K&T)F IH];cbn[map];constructor;[|exact IH].
   exists sp,sa;split;[exact E|]. split;[exact R|]. split;[exact K|].
   exact(tyof_subst W sb _ _ _ _ _ T tm' SR).
Qed.

Definition output_app_subst(sb:subst)(a:A.inst):A.inst:=
 map(fun q=>(fst q,option_map(ExecutionModel.app_subst sb)(snd q)))a.
Lemma output_appof_subst:forall W sb g tm a,
 A.AppOf W g tm a->forall tm',SubRel sb tm tm'->
 A.AppOf W g tm'(output_app_subst sb a).
Proof.
 intros W sb g tm a(rs&Eq&Ord&Meth)tm' SR.
 exists rs;split;[|split;assumption].
 unfold output_app_subst;rewrite Eq,map_map;apply map_ext;intro r;cbn[fst snd].
 f_equal;symmetry;apply SR.
Qed.

Lemma output_receiver_args_subst:forall sb rs args tm tm',
 Forall2(fun x xa=>fst xa=x /\ A.nlook x tm=Some(snd xa))rs args->
 SubRel sb tm tm'->
 Forall2(fun x xa=>fst xa=x /\ A.nlook x tm'=Some(snd xa))rs(smap sb args).
Proof.
 intros sb rs args tm tm' F SR;induction F as[|x[y ty]rs args[Key Read]F IH];cbn[smap map].
 - constructor.
 - constructor;[split;[exact Key|rewrite SR,Read;reflexivity]|exact IH].
Qed.
Lemma output_receiver_subst:forall W sb r tm tau,
 A.RecvTy W r tm tau->forall tm',SubRel sb tm tm'->
 A.RecvTy W r tm'(ExecutionModel.app_subst sb tau).
Proof.
 intros W sb r tm tau R tm' SR;unfold A.RecvTy in R|-*;rewrite SR.
 destruct(A.nlook r tm)as[t|]eqn:L;cbn[option_map].
 - subst tau;reflexivity.
 - destruct(A.w_decl W r)as[d|];[destruct(A.d_ast d)|];cbn in R|-*.
   all:try(destruct R as(args&Eq&Rel);subst tau;exists(smap sb args);split;
     [reflexivity|eapply output_receiver_args_subst;eassumption]).
   exact(tyof_subst W sb _ _ _ _ _ R tm' SR).
Qed.
