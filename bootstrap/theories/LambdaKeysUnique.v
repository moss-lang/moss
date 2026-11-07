(* Full source provision enumeration has one key per physical provision. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound NMapEnum NMapEnumUnique MethodRows.
Import ListNotations.
Local Open Scope list_scope.
Lemma keys_map_injective:forall X Y(f:X->Y),
 (forall a b,f a=f b->a=b)->forall xs,NoDup xs->NoDup(map f xs).
Proof.
 intros X Y f Inj xs Nd;induction Nd;cbn[map];constructor;[|assumption].
 intro Hit;apply in_map_iff in Hit as[y[Eq Hit]];apply H;apply Inj in Eq;now subst y.
Qed.
Lemma keys_column_unique:forall X Y(rows:list(X*Y))Z(tag:X->Z),
 NoDup(map fst rows)->(forall a b,tag a=tag b->a=b)->
 NoDup(map(fun q=>tag(fst q))rows).
Proof. intros;rewrite<-(map_map fst tag rows);apply keys_map_injective;assumption. Qed.
Lemma method_chunks_unique:forall rows:list(nat*list(ety*fsig)),
 NoDup(map fst rows)->(forall mid xs,In(mid,xs)rows->NoDup(map fst xs))->
 NoDup(flat_map(fun p=>map(fun q=>KyMth(fst q)(fst p))(snd p))rows).
Proof.
 intro rows;induction rows as[|[mid xs]rows IH];intros Outer Inner;[constructor|].
 inversion Outer as[|x rest Absent Tail];subst.
 cbn[flat_map fst snd];apply NoDup_app.
 - apply(keys_column_unique ety fsig xs key(fun r=>KyMth r mid));[apply(Inner mid xs);left;reflexivity|].
   intros a b Eq;injection Eq;auto.
 - apply IH;[exact Tail|intros m ys Hit;apply(Inner m ys);right;exact Hit].
 - intros key First Rest;apply in_map_iff in First as([r sg]&Eq&Hit);cbn[fst]in Eq;subst key.
   apply in_flat_map in Rest as([m ys]&Row&Rest);cbn[fst snd]in Rest.
   apply in_map_iff in Rest as([r' sg']&Eq&Hit');cbn[fst]in Eq;injection Eq as <- <-.
   apply Absent;apply in_map_iff;exists(m,ys);split;[reflexivity|exact Row].
Qed.
Ltac distinct_key_classes:=
 intros key Left Right;
 repeat first[
 match goal with H:In _(_++_)|-_=>apply in_app_or in H;destruct H end
 |match goal with H:In _(flat_map _ _)|-_=>apply in_flat_map in H as(?&?&H) end
 |match goal with H:In _(map _ _)|-_=>apply in_map_iff in H as(?&?&?) end
 |progress subst key];try discriminate.
Theorem all_keys_unique:forall e,MethodRowsUnique e->NoDup(all_keys e).
Proof.
 intros e Rows;unfold all_keys.
 apply NoDup_app.
 - apply(keys_column_unique _ _ _ _ KyVal);[apply elems_keys_nodup|intros a b Eq;injection Eq;auto].
 - apply NoDup_app.
   + apply(keys_column_unique _ _ _ _ KyFn);[apply elems_keys_nodup|intros a b Eq;injection Eq;auto].
   + apply NoDup_app.
     * apply method_chunks_unique;[apply elems_keys_nodup|intros m xs Hit;apply(Rows m xs);apply elems_lookup;exact Hit].
     * apply(keys_column_unique _ _ _ _ KyFun);[apply elems_keys_nodup|intros a b Eq;injection Eq;auto].
     * distinct_key_classes.
   + distinct_key_classes.
 - distinct_key_classes.
Qed.
