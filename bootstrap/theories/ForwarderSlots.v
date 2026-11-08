(* Parameter names and numbered slots are obtained from the actual reader. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ForwarderArgsCode.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL ety_of elab_type app_subst symbol_type].
Lemma fn_sig_names:forall g tm e this s sg final,
 fn_sig g tm e this s=Good(sg,final)->map fst(sg_params sg)=map fst(fn_params g).
Proof.
 intros g tm e this s sg final E;unfold fn_sig in E.
 apply bind_inv in E as[ps[mid[Params E]]].
 assert(Names:map fst ps=map fst(fn_params g)).
 {clear E;revert s ps mid Params;induction(fn_params g)as[|[pn pt]params IH];intros s ys mid Run.
  - cbn[mmap]in Run;injection Run as <- <-;reflexivity.
  - cbn[mmap]in Run;apply bind_inv in Run as[p[s1[Read Run]]].
    apply bind_inv in Read as[y[s2[Ty Read]]];injection Read as <- <-.
    apply bind_inv in Run as[rest[s3[Rest Run]]];injection Run as <- <-;cbn[map fst].
    f_equal;exact(IH _ _ _ Rest). }
 apply bind_inv in E as[retTy[last[Ret E]]];injection E as <- <-;exact Names.
Qed.
Lemma combined_slots_lookup:forall(names:list string)(slots:list nat),NoDup names ->List.length names=List.length slots ->
 Forall2(fun n slot=>sassoc n(combine names slots)=Some slot)names slots.
Proof.
 intros names;induction names as[|n names IH];intros slots Nd Len.
 - destruct slots;[constructor|discriminate].
 - destruct slots as[|slot slots];[discriminate|]. inversion Nd as[|n0 ns NotIn Tail];subst.
   constructor.
   + cbn[combine sassoc assoc];rewrite String.eqb_refl;reflexivity.
   + pose proof(IH slots Tail ltac:(cbn[List.length]in Len;lia))as F.
     eapply Forall2_impl;[|exact F]. intros name idx E.
     cbn[combine sassoc assoc]. destruct(String.eqb name n)eqn:Eq;[|exact E].
     apply String.eqb_eq in Eq;subst name;exfalso;apply NotIn.
     apply in_combine_l with(l':=slots)(y:=idx). exact(sassoc_in _ _ _ _ E).
Qed.
Lemma fold_locals_present_old:forall ps e n t mu,
 sassoc n(en_locals e)=Some(t,mu)->exists t' mu',
 sassoc n(en_locals(fold_left(fun acc p=>set_local acc(fst p)(snd p)LLet)ps e))=Some(t',mu').
Proof.
 intros ps;induction ps as[|[pn pt]ps IH];intros e n t mu E;[exists t,mu;exact E|].
 cbn[fold_left];destruct(String.eqb n pn)eqn:Eq.
 - eapply IH with(t:=pt)(mu:=LLet). cbn[set_local en_locals];rewrite sassoc_aset;cbn[fst snd];rewrite Eq;reflexivity.
 - eapply IH with(t:=t)(mu:=mu). cbn[set_local en_locals];rewrite sassoc_aset;cbn[fst snd];rewrite Eq;exact E.
Qed.
Lemma fold_locals_present:forall ps e n,In n(map fst ps)->exists t mu,
 sassoc n(en_locals(fold_left(fun acc p=>set_local acc(fst p)(snd p)LLet)ps e))=Some(t,mu).
Proof.
 intros ps;induction ps as[|[pn pt]ps IH];intros e n E;[contradiction|].
 cbn[map fst]in E;cbn[fold_left]. destruct E as[<-|E].
 - eapply fold_locals_present_old with(t:=pt)(mu:=LLet).
   cbn[set_local en_locals];rewrite sassoc_aset,String.eqb_refl;reflexivity.
 - apply IH;exact E.
Qed.
Lemma parameter_local_slots:forall e params names slots s,
 map fst params=names->NoDup names->List.length names=List.length slots->
 st_vars s=combine names slots->
 LocalSlots(fold_left(fun acc p=>set_local acc(fst p)(snd p)LLet)params e)names slots s.
Proof.
 intros e params names slots s Names Nd Len Vars.
 pose proof(combined_slots_lookup names slots Nd Len)as F.
 eapply Forall2_impl;[|exact F]. intros n slot E.
 assert(In n(map fst params)).
 {rewrite Names;apply in_combine_l with(l':=slots)(y:=slot);exact(sassoc_in _ _ _ _ E). }
 destruct(fold_locals_present params e n H)as[t[mu L]]. exists t,mu;split;[exact L|rewrite Vars;exact E].
Qed.
