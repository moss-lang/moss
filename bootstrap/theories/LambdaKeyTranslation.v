(* Receiver validity makes the source provision-key translation injective. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodyTr CtxMerge TypeReadingInj
 NMapEnum MethodRows LambdaKeysUnique.
Import ListNotations.
Local Open Scope list_scope.
Definition LambdaKeyValid(k:key):Prop:=match k with KyMth r _=>ety_ok r=true|_=>True end.
Lemma all_keys_valid:forall e,MethodRowsCanonical e->Forall LambdaKeyValid(all_keys e).
Proof.
 intros e Clean;apply Forall_forall;intros k Member;unfold all_keys in Member.
 repeat apply in_app_or in Member as[Member|Member].
 - apply in_map_iff in Member as(q&Eq&Hit);subst k;exact I.
 - apply in_map_iff in Member as(q&Eq&Hit);subst k;exact I.
 - apply in_flat_map in Member as([mid rows]&Outer&Member);cbn[fst snd]in Member.
   apply in_map_iff in Member as([r sg]&Eq&Hit);cbn[fst]in Eq;subst k.
   unfold LambdaKeyValid;pose proof(Clean mid rows(proj1(elems_lookup _ _ _ _)Outer))as Good.
   rewrite Forall_forall in Good;exact(proj1(Good(r,sg)Hit)).
 - apply in_map_iff in Member as(q&Eq&Hit);subst k;exact I.
Qed.
Lemma key_translation_injective:forall Fl,
 (forall a b,Fl a=Fl b->a=b)->forall a b,
 LambdaKeyValid a->LambdaKeyValid b->key_tr Fl a=key_tr Fl b->a=b.
Proof.
 intros Fl Finj a b Wa Wb Eq.
 destruct a as[v|f|r m|u],b as[w|g|s n|h];cbn[key_tr LambdaKeyValid]in *;try discriminate Eq;try congruence.
 injection Eq as EqTy EqMid;subst n;f_equal.
 exact(tr_inj Fl Finj r s Wa Wb EqTy).
Qed.
Lemma keys_map_valid:forall X Y(P:X->Prop)(f:X->Y),
 (forall a b,P a->P b->f a=f b->a=b)->forall xs,NoDup xs->Forall P xs->NoDup(map f xs).
Proof.
 intros X Y P f Inj xs Unique;induction Unique;intro Valid;[constructor|].
 inversion Valid as[|first rest Good Tail];subst;cbn[map];constructor;[|apply IHUnique;exact Tail].
 intro Member;apply in_map_iff in Member as(y&Eq&Member).
 rewrite Forall_forall in Tail;apply H;apply(Inj y x(Tail y Member)Good)in Eq;now subst y.
Qed.
Theorem all_keys_translation_unique:forall Fl,
 (forall a b,Fl a=Fl b->a=b)->forall e,
 MethodRowsUnique e->MethodRowsCanonical e->NoDup(map(key_tr Fl)(all_keys e)).
Proof.
 intros Fl Finj e Unique Clean;apply(keys_map_valid key item LambdaKeyValid(key_tr Fl)).
 - exact(key_translation_injective Fl Finj).
 - apply all_keys_unique;exact Unique.
 - apply all_keys_valid;exact Clean.
Qed.
