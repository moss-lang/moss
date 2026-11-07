(* An empty emitted demand prefix cannot have applied a family. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements ety_ty].
Ltac unpack_success H :=
 repeat match type of H with
 | mbind ?m ?k ?s=Good ?v =>
   let a:=fresh "a" in let si:=fresh "si" in let E:=fresh "E" in
   apply bind_inv in H as[a[si[E H]]];
   repeat match type of a with (?X*?Y)%type=>destruct a as[a ?b]end;
   cbn beta iota zeta in H
 end.
Lemma apply_abstract_nonempty : forall f c e g pre e' c' s s',
 apply_abstract f c e g s=Good((pre,e',c'),s')->pre<>[].
Proof.
 intros[|f]c e g pre e' c' s s' E;[discriminate E|]. intro Empty;subst pre.
 cbn[apply_abstract]in E.
 destruct(negb(is_some(nget(en_funs e)(gs_id g))));[exact(err_not_good _ _ _ _ _ _ E)|].
 unpack_success E. discriminate E.
Qed.
Lemma apply_family_nonempty : forall f c e g th want pre e' s s',
 apply_family f c e g th want s=Good((pre,e'),s')->pre<>[].
Proof.
 intros[|f]c e g th want pre e' s s' E;[discriminate E|]. cbn[apply_family]in E.
 apply bind_inv in E as[e1[s1[E1 E]]].
 apply bind_inv in E as[[[ys e2]c2][s2[E2 E]]].
 pose proof(apply_abstract_nonempty _ _ _ _ _ _ _ _ _ E2)as Non.
 apply bind_inv in E as[mem[s3[Em E]]].
 apply bind_inv in E as[u[s4[Eu E]]]. injection E as <- _ _;exact Non.
Qed.
Definition demand_apply_step f c(acc:list xstmt*env)(q:(gsym*list(nat*ety))*list item):M(list xstmt*env):=
 let '(ss,e):=acc in
 let! (ys,e'):=apply_family f c e(fst(fst q))(snd(fst q))(snd q)in ret(ss++ys,e').
Lemma demand_fold_empty : forall apps f c ss e s pre e' s',
 mfold(demand_apply_step f c)(ss,e)apps s=Good((pre,e'),s')->pre=[]->
 apps=[]/\ss=[]/\e'=e.
Proof.
 intros apps;induction apps as[|q apps IH];intros f c ss e s pre e' s' E Empty.
 - injection E as <- <- _;auto.
 - cbn[mfold]in E. apply bind_inv in E as[[ss1 e1][s1[E1 E]]].
   unfold demand_apply_step in E1. apply bind_inv in E1 as[[ys e2][s2[Ey E1]]]. injection E1 as <- <- <-.
   destruct(IH _ _ _ _ _ _ _ _ E Empty)as[_[Zero _]].
   apply app_eq_nil in Zero as[_ Zero]. exfalso;exact(apply_family_nonempty _ _ _ _ _ _ _ _ _ _ Ey Zero).
Qed.
Theorem demand_needs_empty_env : forall f c e info s e' s',
 demand_needs f c e info s=Good(([],e'),s')->e'=e.
Proof.
 intros[|f]c e info s e' s' E;[discriminate E|]. cbn[demand_needs]in E.
 apply bind_inv in E as[apps[sa[Eapps E]]].
 exact(proj2(proj2(demand_fold_empty _ _ _ _ _ _ _ _ _ E eq_refl))).
Qed.
