(* Retain the source merge action in private generation space.  The name
   table is still materialized; these edges act only on generation leaves. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodyTr Merge CanonTypes InputGenerationScope.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

Definition private_input_edges(rho:generation_substitution)(e:env):list(nat*ety):=
 map(fun p=>(scoped_generation rho(fst p),scoped_type rho(canon e(snd p))))(en_uf e).
Definition private_input_with_merges(rho:generation_substitution)(e:env):env:=
 with_uf(private_input_env rho e)(private_input_edges rho e).
(* A finite support covers edge domains and their fully resolved ranges. *)
Definition private_merge_support(e:env):list nat:=
 map fst(en_uf e) ++ flat_map(fun p=>ety_gens(canon e(snd p)))(en_uf e).

Lemma scope_generation_eqb : forall rho back g h,
 scoped_generation back(scoped_generation rho g)=g->
 scoped_generation back(scoped_generation rho h)=h->
 Nat.eqb(scoped_generation rho g)(scoped_generation rho h)=Nat.eqb g h.
Proof.
 intros rho back g h G H;destruct(Nat.eqb g h)eqn:E.
 - apply Nat.eqb_eq in E;subst;apply Nat.eqb_refl.
 - destruct(Nat.eqb(scoped_generation rho g)(scoped_generation rho h))eqn:F;[|reflexivity].
   apply Nat.eqb_eq in F;apply(f_equal(scoped_generation back))in F.
   rewrite G,H in F;subst;rewrite Nat.eqb_refl in E;discriminate.
Qed.
Lemma private_edge_lookup : forall rho back e g,
 scope_inverse_on rho back(private_merge_support e)->
 scoped_generation back(scoped_generation rho g)=g->
 nassoc(scoped_generation rho g)(private_input_edges rho e)=
 option_map(fun t=>scoped_type rho(canon e t))(nassoc g(en_uf e)).
Proof.
 intros rho back e g Inverse G;unfold private_input_edges,nassoc.
 apply(scope_assoc_keys nat ety ety Nat.eqb(scoped_generation rho)
  (fun t=>scoped_type rho(canon e t))g(en_uf e));intros [h rhs] Hit;cbn.
 apply scope_generation_eqb with(back:=back);[exact G|apply Inverse].
 unfold private_merge_support;apply in_or_app;left.
 apply in_map with(f:=@fst nat ety)in Hit;exact Hit.
Qed.
Lemma private_normal_form : forall rho back e t,
 scope_inverse_on rho back(private_merge_support e)->
 scope_inverse_on rho back(ety_gens t)->
 Canon(en_uf e)t->Canon(private_input_edges rho e)(scoped_type rho t).
Proof.
 intros rho back e t Edges Types Normal g Hit.
 rewrite scoped_type_generations in Hit;apply in_map_iff in Hit as[h[<- Hit]].
 rewrite(private_edge_lookup rho back e h Edges(Types h Hit)),(Normal h Hit);reflexivity.
Qed.
Lemma private_edge_range_inverse : forall rho back e g rhs,
 scope_inverse_on rho back(private_merge_support e)->In(g,rhs)(en_uf e)->
 scope_inverse_on rho back(ety_gens(canon e rhs)).
Proof.
 intros rho back e g rhs Inverse Hit h H;apply Inverse.
 unfold private_merge_support;apply in_or_app;right.
 apply in_flat_map;exists(g,rhs);split;[exact Hit|exact H].
Qed.
Lemma private_edge_range_normal : forall rho back e g rhs,
 Tri(en_uf e)->scope_inverse_on rho back(private_merge_support e)->
 In(g,rhs)(en_uf e)->Canon(private_input_edges rho e)(scoped_type rho(canon e rhs)).
Proof.
 intros rho back e g rhs Tri Inv Hit.
 apply private_normal_form with(back:=back);[exact Inv|eapply private_edge_range_inverse;eauto|].
 apply canon_tri;exact Tri.
Qed.

Lemma private_canon_go_read : forall rho back e,
 Tri(en_uf e)->scope_inverse_on rho back(private_merge_support e)->
 forall fuel t,scope_inverse_on rho back(ety_gens t)->
 canon_go(S fuel)(private_input_edges rho e)(scoped_type rho t)=
 scoped_type rho(canon e t).
Proof.
 intros rho back e Tri Edges fuel t;induction t as[| |g|h args IH|ts IH|fs IH]using ety_ind';intro Types.
 - reflexivity.
 - reflexivity.
 - change(canon_go(S fuel)(private_input_edges rho e)(YAbs(scoped_generation rho g))=
   scoped_type rho(Res(en_uf e)(YAbs g))).
   rewrite(res_abs _ (tri_rk _ Tri)).
   cbn[canon_go].
   rewrite(private_edge_lookup rho back e g Edges(Types g(or_introl eq_refl))).
   destruct(nassoc g(en_uf e))as[rhs|]eqn:Read;cbn[option_map];[|reflexivity].
   destruct(ety_eqb(scoped_type rho(canon e rhs))(YAbs(scoped_generation rho g)))eqn:Self.
   + apply ety_eqb_true in Self;symmetry;exact Self.
   + apply canon_fix.
     eapply private_edge_range_normal;[exact Tri|exact Edges|].
     eapply Merge.nassoc_in;exact Read.
 - change(canon_go(S fuel)(private_input_edges rho e)(scoped_type rho(YNom h args))=
   scoped_type rho(Res(en_uf e)(YNom h args))).
   rewrite res_nom;cbn[scoped_type canon_go].
   f_equal;revert Types;induction IH as[|[label t]rest H IH IHr];intro Types;cbn;[reflexivity|].
   f_equal.
   + f_equal;apply H;intros g G;apply Types;cbn;apply in_or_app;left;exact G.
   + apply IHr;intros g G;apply Types;cbn;apply in_or_app;right;exact G.
 - change(canon_go(S fuel)(private_input_edges rho e)(scoped_type rho(YUnion ts))=
   scoped_type rho(Res(en_uf e)(YUnion ts))).
   rewrite res_union;cbn[scoped_type canon_go].
   f_equal;revert Types;induction IH as[|t rest H IH IHr];intro Types;cbn;[reflexivity|].
   f_equal.
   + apply H;intros g G;apply Types;cbn;apply in_or_app;left;exact G.
   + apply IHr;intros g G;apply Types;cbn;apply in_or_app;right;exact G.
 - change(canon_go(S fuel)(private_input_edges rho e)(scoped_type rho(YRec fs))=
   scoped_type rho(Res(en_uf e)(YRec fs))).
   rewrite res_rec;cbn[scoped_type canon_go].
   f_equal;revert Types;induction IH as[|[label t]rest H IH IHr];intro Types;cbn;[reflexivity|].
   f_equal.
   + f_equal;apply H;intros g G;apply Types;cbn;apply in_or_app;left;exact G.
   + apply IHr;intros g G;apply Types;cbn;apply in_or_app;right;exact G.
Qed.
Theorem private_canon_read : forall rho back e t,
 Tri(en_uf e)->scope_inverse_on rho back(private_merge_support e)->
 scope_inverse_on rho back(ety_gens t)->
 canon(private_input_with_merges rho e)(scoped_type rho t)=
 scoped_type rho(canon e t).
Proof.
 intros;unfold canon at 1;apply private_canon_go_read with(back:=back);assumption.
Qed.

Lemma private_rebind_canon : forall rho e name meaning stamp t,
 canon(set_tn(private_input_with_merges rho e)name meaning stamp)t=
 canon(private_input_with_merges rho e)t.
Proof. reflexivity. Qed.
Lemma private_rebind_read : forall rho back e name meaning stamp t,
 Tri(en_uf e)->scope_inverse_on rho back(private_merge_support e)->
 scope_inverse_on rho back(ety_gens t)->
 canon(set_tn(private_input_with_merges rho e)name meaning stamp)(scoped_type rho t)=
 scoped_type rho(canon e t).
Proof. intros;rewrite private_rebind_canon;eapply private_canon_read;eauto. Qed.
Lemma private_merge_name_read : forall rho e name,
 tn_get(private_input_with_merges rho e)name=
 option_map(fun t=>scoped_type rho(canon e t))(tn_get e name).
Proof. intros;apply private_name_read. Qed.

Lemma private_edges_domain : forall rho e,
 map fst(private_input_edges rho e)=map(scoped_generation rho)(map fst(en_uf e)).
Proof.
 intros;unfold private_input_edges;rewrite !map_map.
 apply map_ext;intros [g rhs];reflexivity.
Qed.
Lemma private_canonical_name_read : forall rho back e name original,
 tn_get e name=Some original->Tri(en_uf e)->
 scope_inverse_on rho back(private_merge_support e)->
 scope_inverse_on rho back(ety_gens(canon e original))->
 exists frozen,tn_get(private_input_with_merges rho e)name=Some frozen /\
 frozen=scoped_type rho(canon e original) /\
 canon(private_input_with_merges rho e)frozen=frozen.
Proof.
 intros rho back e name original Read Tri Edges Types.
 exists(scoped_type rho(canon e original));split.
 - rewrite private_merge_name_read,Read;reflexivity.
 - split;[reflexivity|].
   rewrite(private_canon_read rho back e(canon e original)Tri Edges Types).
   rewrite canon_idem by exact Tri;reflexivity.
Qed.

(* The old input merge also acts on a foreign target's raw generation.
   Rebinding the source name to a new output never changes that old reading. *)
Definition private_merge_example_source:env:=
 Env [(2,(YAbs 2,7))] [(2,YAbs 3)] NE NE NE NE [] [].
Definition private_merge_example_substitution:generation_substitution:=[(2,10);(3,11)].
Example private_merge_retains_old_action:
 canon(private_input_with_merges private_merge_example_substitution private_merge_example_source)(YAbs 10)=YAbs 11 /\
 canon(set_tn(private_input_with_merges private_merge_example_substitution private_merge_example_source)
  2(YNom 99[])8)(YAbs 10)=YAbs 11 /\
 tn_get(private_input_with_merges private_merge_example_substitution private_merge_example_source)2=Some(YAbs 11) /\
 canon(private_input_env private_merge_example_substitution private_merge_example_source)(YAbs 10)=YAbs 10.
Proof. repeat split;reflexivity. Qed.
