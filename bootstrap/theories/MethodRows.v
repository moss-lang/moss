(* The physical method row invariant needed by full provision enumeration.
   It is separate from canonical lookup, which cannot constrain hidden rows. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodyTr.
Import ListNotations.
Lemma method_key_eqb_spec:forall a b,ety_eqb a b=true<->a=b.
Proof. intros;split;[apply ety_eqb_true|intros<-;apply ety_eqb_refl]. Qed.
Lemma method_aset_key_member:forall V receiver(value:V)rows key,
 In key(map fst(aset ety_eqb receiver value rows))->key=receiver\/In key(map fst rows).
Proof.
 intros V receiver value rows;induction rows as[|[r old]rows IH];intros key Member.
 - cbn[aset map fst]in Member;destruct Member as[Eq|[]];left;symmetry;exact Eq.
 - cbn[aset]in Member;destruct(ety_eqb receiver r)eqn:Eq.
   + apply method_key_eqb_spec in Eq;subst r;cbn[map fst]in Member.
     destruct Member as[Eq|Member];[left;symmetry;exact Eq|right;right;exact Member].
   + cbn[map fst]in Member;destruct Member as[Eq0|Member];[right;left;exact Eq0|].
     destruct(IH _ Member)as[Eq0|Old];[left;exact Eq0|right;right;exact Old].
Qed.
Lemma method_aset_keys_unique:forall V receiver(value:V)rows,
 NoDup(map fst rows)->NoDup(map fst(aset ety_eqb receiver value rows)).
Proof.
 intros V receiver value rows;induction rows as[|[r old]rows IH];intro Unique.
 - cbn[aset map fst];constructor;[intros[]|constructor].
 - inversion Unique;subst;cbn[aset];destruct(ety_eqb receiver r)eqn:Eq.
   + apply method_key_eqb_spec in Eq;subst r;exact Unique.
   + cbn[map fst];constructor;[|apply IH;assumption].
     intro Member;destruct(method_aset_key_member V receiver value rows r Member)as[Same|Old];[subst r;rewrite ety_eqb_refl in Eq;discriminate|contradiction].
Qed.
Lemma method_aset_keys_property:forall V(P:ety->Prop)receiver(value:V)rows,
 P receiver->Forall(fun q=>P(fst q))rows->Forall(fun q=>P(fst q))(aset ety_eqb receiver value rows).
Proof.
 intros V P receiver value rows Good Old;apply Forall_forall;intros q Hit.
 assert(Key:In(fst q)(map fst(aset ety_eqb receiver value rows)))by(apply in_map;exact Hit).
 destruct(method_aset_key_member V receiver value rows(fst q)Key)as[Same|Before];[rewrite Same;exact Good|].
 apply in_map_iff in Before as([r v]&Eq&Before);cbn[fst]in Eq;rewrite<-Eq.
 rewrite Forall_forall in Old;exact(Old(r,v)Before).
Qed.
Definition MethodRowsUnique(e:env):Prop:=forall m rows,
 nget(en_mths e)m=Some rows->NoDup(map fst rows).
Definition MethodRowsCanonical(e:env):Prop:=forall m rows,
 nget(en_mths e)m=Some rows->Forall(fun q=>ety_ok(fst q)=true/\canon e(fst q)=fst q)rows.
Lemma method_rows_empty:MethodRowsUnique empty_env/\MethodRowsCanonical empty_env.
Proof. split;intros m rows E;discriminate E. Qed.
Lemma method_rows_same:forall e e',en_mths e'=en_mths e->en_uf e'=en_uf e->
 MethodRowsUnique e->MethodRowsCanonical e->MethodRowsUnique e'/\MethodRowsCanonical e'.
Proof.
 intros e e' Table Uf Unique Clean;split.
 - intros m rows E;rewrite Table in E;exact(Unique _ _ E).
 - intros m rows E;rewrite Table in E;pose proof(Clean _ _ E)as Good.
   unfold canon in *;rewrite Uf;exact Good.
Qed.
Theorem method_rows_set:forall e r m sg,
 MethodRowsUnique e->MethodRowsCanonical e->ety_ok r=true->canon e r=r->
 MethodRowsUnique(set_mth e r m sg)/\MethodRowsCanonical(set_mth e r m sg).
Proof.
 intros e r m sg Unique Clean Wr Cr;split;intros mid rows E;
 unfold set_mth in E;cbn[en_mths]in E;rewrite nget_nset in E;
 destruct(Nat.eqb mid m)eqn:Eq.
 - injection E as <-;apply method_aset_keys_unique.
   destruct(nget(en_mths e)m)as[old|]eqn:Get;[exact(Unique _ _ Get)|constructor].
 - exact(Unique _ _ E).
 - injection E as <-.
   change(Forall(fun q=>ety_ok(fst q)=true/\canon e(fst q)=fst q)(aset ety_eqb r sg(opt_default[](nget(en_mths e)m)))).
   apply(method_aset_keys_property fsig(fun t=>ety_ok t=true/\canon e t=t));[split;assumption|].
   destruct(nget(en_mths e)m)as[old|]eqn:Get;[exact(Clean _ _ Get)|constructor].
 - exact(Clean _ _ E).
Qed.
