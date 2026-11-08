(* Physical provision enumeration agrees with the actual environment readers. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound NMapEnum MethodRows.
Import ListNotations.
Local Open Scope list_scope.
Definition LambdaKeyAvailable(e:env)(k:key):Prop:=
 match k with
 |KyVal v=>nget(en_vals e)v<>None|KyFn f=>nget(en_fns e)f<>None
 |KyMth r m=>get_mth e r m<>None|KyFun u=>nget(en_funs e)u<>None end.
Lemma method_lookup_member:forall r(rows:list(ety*fsig))sg,
 assoc ety_eqb r rows=Some sg->In(r,sg)rows.
Proof.
 intros r rows;induction rows as[|[k old]rows IH];intros sg Read;[discriminate Read|].
 cbn[assoc]in Read;destruct(ety_eqb r k)eqn:Eq.
 - apply method_key_eqb_spec in Eq;subst k;injection Read as <-;left;reflexivity.
 - right;exact(IH _ Read).
Qed.
Lemma method_member_lookup:forall(rows:list(ety*fsig))r sg,
 NoDup(map fst rows)->In(r,sg)rows->assoc ety_eqb r rows=Some sg.
Proof.
 intro rows;induction rows as[|[k old]rows IH];intros r sg Unique Hit;[contradiction|].
 inversion Unique as[|x xs Absent Tail];subst;cbn[assoc].
 destruct(ety_eqb r k)eqn:Eq.
 - apply method_key_eqb_spec in Eq;subst k;destruct Hit as[Eq|Hit];[injection Eq as <-;reflexivity|].
   exfalso;apply Absent;apply in_map_iff;exists(r,sg);split;[reflexivity|exact Hit].
 - destruct Hit as[Eq0|Hit];[injection Eq0 as <-;rewrite ety_eqb_refl in Eq;discriminate|exact(IH _ _ Tail Hit)].
Qed.
Theorem all_keys_available:forall e,MethodRowsUnique e->forall k,
 In k(all_keys e)<->LambdaKeyAvailable e k.
Proof.
 intros e Unique k;split.
 - intro Hit;unfold all_keys in Hit;repeat apply in_app_or in Hit as[Hit|Hit].
   + apply in_map_iff in Hit as([v ty]&Eq&Row);cbn[fst]in Eq;subst k.
     unfold LambdaKeyAvailable;rewrite(proj1(elems_lookup _ _ _ _)Row);discriminate.
   + apply in_map_iff in Hit as([f sg]&Eq&Row);cbn[fst]in Eq;subst k.
     unfold LambdaKeyAvailable;rewrite(proj1(elems_lookup _ _ _ _)Row);discriminate.
   + apply in_flat_map in Hit as([m rows]&Row&Hit);cbn[fst snd]in Hit.
     apply in_map_iff in Hit as([r sg]&Eq&Hit);cbn[fst]in Eq;subst k.
     unfold LambdaKeyAvailable,get_mth;pose proof(proj1(elems_lookup _ _ _ _)Row)as Get;rewrite Get.
     rewrite(method_member_lookup rows r sg(Unique _ _ Get)Hit);discriminate.
   + apply in_map_iff in Hit as([u stamp]&Eq&Row);cbn[fst]in Eq;subst k.
     unfold LambdaKeyAvailable;rewrite(proj1(elems_lookup _ _ _ _)Row);discriminate.
 - destruct k as[v|f|r m|u];unfold LambdaKeyAvailable;intro Available;unfold all_keys.
   + destruct(nget(en_vals e)v)as[t|]eqn:Get;[|contradiction].
     apply in_or_app;left;apply in_map_iff;exists(v,t);split;[reflexivity|apply elems_lookup;exact Get].
   + destruct(nget(en_fns e)f)as[sg|]eqn:Get;[|contradiction].
     apply in_or_app;right;apply in_or_app;left;apply in_map_iff;exists(f,sg);split;[reflexivity|apply elems_lookup;exact Get].
   + unfold get_mth in Available;destruct(nget(en_mths e)m)as[rows|]eqn:Get;[|contradiction].
     destruct(assoc ety_eqb r rows)as[sg|]eqn:Lookup;[|contradiction].
     apply in_or_app;right;apply in_or_app;right;apply in_or_app;left.
     apply in_flat_map;exists(m,rows);split;[apply elems_lookup;exact Get|].
     cbn[fst snd];apply in_map_iff;exists(r,sg);split;[reflexivity|apply method_lookup_member;exact Lookup].
   + destruct(nget(en_funs e)u)as[stamp|]eqn:Get;[|contradiction].
     apply in_or_app;right;apply in_or_app;right;apply in_or_app;right.
     apply in_map_iff;exists(u,stamp);split;[reflexivity|apply elems_lookup;exact Get].
Qed.
