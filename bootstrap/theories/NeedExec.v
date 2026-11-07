(* Actual successful requirement checks retain their checked equations. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr Merge CanonTypes InstantiateTr.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Lemma miter_checked : forall X(f:X->M unit) l s u s',
 miter f l s=Good(u,s')->forall x,In x l->exists sx ux sx',f x sx=Good(ux,sx').
Proof.
 intros X f l;induction l as[|a l IH];intros s u s' E x Hx;[contradiction|].
 cbn[miter]in E. apply bind_inv in E as[ua[sa[Ea E]]].
 destruct Hx as[<-|Hx];[eauto|exact(IH _ _ _ E _ Hx)].
Qed.
Lemma require_merges_checked : forall mi e g info s u s' k v,
 require_merges mi e g info s=Good(u,s')->In(k,v)(fi_merges info)->
 (forall t,In t(k::ety_gens(canon(env_with_edges[](fi_merges info))(YAbs k)))->tn_get e t<>None)->
 canon e(instantiate e(YAbs k))=canon e(instantiate e(canon(env_with_edges[](fi_merges info))(YAbs k))).
Proof.
 intros mi e g info s u s' k v E Hin Hav.
 unfold require_merges in E.
 destruct(miter_checked _ _ _ _ _ _ E _ Hin)as[sx[ux[sx' Ex]]].
 cbn beta iota zeta in Ex. cbn[fst]in Ex.
 set(rhs:=canon(env_with_edges[](fi_merges info))(YAbs k))in *;fold rhs in Ex.
 assert(All:forallb(fun t=>is_some(tn_get e t))(k::ety_gens rhs)=true).
 { apply forallb_forall. intros t Ht. specialize(Hav t Ht). destruct(tn_get e t);[reflexivity|contradiction]. }
 rewrite All in Ex.
 destruct(ety_eqb(canon e(instantiate e(YAbs k)))(canon e(instantiate e rhs)))eqn:Eq.
 - exact(ety_eqb_true _ _ Eq).
 - apply bind_inv in Ex as[a[sa[_ Ex]]]. apply bind_inv in Ex as[b[sb[_ Ex]]].
   apply bind_inv in Ex as[c[sc[_ Ex]]]. apply bind_inv in Ex as[d[sd[_ Ex]]].
   exfalso;exact(err_not_good _ _ _ _ _ _ Ex).
Qed.
Section Semantic.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma require_merges_semantic : forall mi e callee info s u s' c F C Cs,
 require_merges mi e callee info s=Good(u,s')->
 CxRel w R Fl c e F C -> A.c_sub Cs=suf Fl(fi_merges info)->
 forall g t,In(g,t)(A.c_sub Cs)->
 (forall x,In x(g::A.gens t)->A.nlook x(A.c_ty C)<>None)->
 A.canon C(Audit.app_subst(A.c_ty C)(TAbs g))=
 A.canon C(Audit.app_subst(A.c_ty C)t).
Proof.
 intros mi e callee info s u s' c F C Cs E Hc Sub g t Hin Hav.
 rewrite Sub in Hin. unfold suf in Hin.
 apply in_map_iff in Hin as[[k v][Eq Hin]].
 change((k,tr Fl(canon(env_with_edges[](fi_merges info))(YAbs k)))=(g,t))in Eq.
 injection Eq as <- <-.
 pose proof(require_merges_checked _ _ _ _ _ _ _ _ _ E Hin)as Same.
 assert(Avail:forall x,In x(k::ety_gens(canon(env_with_edges[](fi_merges info))(YAbs k)))->tn_get e x<>None).
 { intros x Hx En. specialize(Hav x). rewrite(gens_tr Fl _)in Hav.
   specialize(Hav Hx). rewrite(cr_ty _ _ _ _ _ _ _ Hc),En in Hav. contradiction. }
 specialize(Same Avail).
 change(A.canon C(Audit.app_subst(A.c_ty C)(tr Fl(YAbs k)))=A.canon C(Audit.app_subst(A.c_ty C)(tr Fl(canon(env_with_edges[](fi_merges info))(YAbs k))))).
 rewrite <-(instantiate_canonical w R Fl c e F C Hc(YAbs k)),
   <-(instantiate_canonical w R Fl c e F C Hc(canon(env_with_edges[](fi_merges info))(YAbs k))).
 now rewrite Same.
Qed.
End Semantic.
