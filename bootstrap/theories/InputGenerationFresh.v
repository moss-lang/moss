(* A finite private generation scope, allocated without changing any source
   symbol. The caller reserves [start,next) before using the resulting map. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab BodyTr InputGenerationScope InputGenerationMerge.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

Fixpoint allocate_private_generations(start:nat)(support:list nat):generation_substitution:=
 match support with
 | []=>[]
 | g::rest=>(g,start)::allocate_private_generations(S start)rest
 end.
Definition fresh_input_generations(start:nat)(support:list nat):generation_substitution:=
 allocate_private_generations start(dedup_nat_fast support).
Definition private_generation_next(start:nat)(support:list nat):nat:=
 start+List.length(dedup_nat_fast support).
Definition private_generation_plan(start:nat)(support:list nat):generation_substitution*nat:=
 (fresh_input_generations start support,private_generation_next start support).

Lemma allocated_generation_domains : forall support start,
 map fst(allocate_private_generations start support)=support.
Proof. induction support;intro start;cbn;[reflexivity|rewrite IHsupport;reflexivity]. Qed.
Lemma allocated_generation_ranges : forall support start,
 map snd(allocate_private_generations start support)=seq start(List.length support).
Proof. induction support;intro start;cbn;[reflexivity|rewrite IHsupport;reflexivity]. Qed.
Lemma allocated_generation_length : forall support start,
 List.length(allocate_private_generations start support)=List.length support.
Proof. induction support;intro start;cbn;[reflexivity|rewrite IHsupport;reflexivity]. Qed.
Lemma fresh_generation_domains : forall start support,
 map fst(fresh_input_generations start support)=dedup_nat_fast support.
Proof. intros;apply allocated_generation_domains. Qed.
Lemma fresh_generation_ranges : forall start support,
 map snd(fresh_input_generations start support)=
 seq start(List.length(dedup_nat_fast support)).
Proof. intros;apply allocated_generation_ranges. Qed.
Lemma fresh_generation_domain_unique : forall start support,
 NoDup(map fst(fresh_input_generations start support)).
Proof. intros;rewrite fresh_generation_domains,dedup_nat_fast_eq;apply NoDup_nodup. Qed.
Lemma fresh_generation_range_unique : forall start support,
 NoDup(map snd(fresh_input_generations start support)).
Proof. intros;rewrite fresh_generation_ranges;apply seq_NoDup. Qed.
Lemma fresh_generation_domain_exact : forall start support g,
 In g(map fst(fresh_input_generations start support))<->In g support.
Proof. intros;rewrite fresh_generation_domains,dedup_nat_fast_eq;apply nodup_In. Qed.
Lemma fresh_generation_pair_bounds : forall start support old fresh,
 In(old,fresh)(fresh_input_generations start support)->
 start<=fresh<private_generation_next start support.
Proof.
 intros start support old fresh Hit.
 apply in_map with(f:=@snd nat nat)in Hit;cbn in Hit.
 rewrite fresh_generation_ranges in Hit;apply in_seq in Hit;exact Hit.
Qed.
Lemma fresh_generation_read : forall start support g,
 In g support->exists fresh,
 nassoc g(fresh_input_generations start support)=Some fresh /\
 scoped_generation(fresh_input_generations start support)g=fresh /\
 start<=fresh<private_generation_next start support.
Proof.
 intros start support g Hit.
 apply (proj2(fresh_generation_domain_exact start support g))in Hit.
 apply in_map_iff in Hit as[[old fresh][Eq Hit]];cbn in Eq;subst old.
 exists fresh;assert(Read:nassoc g(fresh_input_generations start support)=Some fresh).
 { apply scope_pair_read;[apply fresh_generation_domain_unique|exact Hit]. }
 split;[exact Read|];split.
 - unfold scoped_generation;rewrite Read;reflexivity.
 - eapply fresh_generation_pair_bounds;exact Hit.
Qed.
Lemma fresh_generation_inverse : forall start support,
 scope_inverse_on(fresh_input_generations start support)
  (inverse_generation_substitution(fresh_input_generations start support))support.
Proof.
 intros start support g Hit.
 apply scope_inverse_pairs;[apply fresh_generation_domain_unique|apply fresh_generation_range_unique|].
 apply fresh_generation_domain_exact;exact Hit.
Qed.
Lemma fresh_generation_injective : forall start support a b,
 In a support->In b support->
 scoped_generation(fresh_input_generations start support)a=
 scoped_generation(fresh_input_generations start support)b->a=b.
Proof.
 intros start support a b A B Equal.
 apply(f_equal(scoped_generation(inverse_generation_substitution(fresh_input_generations start support))))in Equal.
 rewrite(fresh_generation_inverse start support a A),(fresh_generation_inverse start support b B)in Equal.
 exact Equal.
Qed.
Lemma fresh_generation_disjoint : forall start support g,
 (forall old,In old support->old<start)->In g support->
 ~In(scoped_generation(fresh_input_generations start support)g)support.
Proof.
 intros start support g Old Hit FreshHit.
 destruct(fresh_generation_read start support g Hit)as[fresh[Read[Value Bounds]]].
 rewrite Value in FreshHit;specialize(Old fresh FreshHit);lia.
Qed.
Lemma fresh_generation_avoids_earlier : forall start support g old,
 In g support->old<start->
 scoped_generation(fresh_input_generations start support)g<>old.
Proof.
 intros start support g old Hit Before.
 destruct(fresh_generation_read start support g Hit)as[fresh[Read[Value Bounds]]].
 rewrite Value;lia.
Qed.
Lemma fresh_generation_next_monotone : forall start support,
 start<=private_generation_next start support.
Proof. intros;unfold private_generation_next;lia. Qed.
Lemma fresh_generation_reads_outside : forall start support g,
 ~In g support->scoped_generation(fresh_input_generations start support)g=g.
Proof.
 intros start support g Absent;unfold scoped_generation.
 assert(Read:nassoc g(fresh_input_generations start support)=None).
 { destruct(nassoc g(fresh_input_generations start support))as[fresh|]eqn:Lookup;[|reflexivity].
   exfalso;apply Absent;apply(fresh_generation_domain_exact start support g).
   apply Merge.nassoc_in in Lookup;apply in_map with(f:=@fst nat nat)in Lookup;exact Lookup. }
 rewrite Read;reflexivity.
Qed.

Lemma fresh_type_inverse : forall start support t,
 incl(ety_gens t)support->
 scope_inverse_on(fresh_input_generations start support)
  (inverse_generation_substitution(fresh_input_generations start support))(ety_gens t).
Proof. intros start support t Covered g Hit;apply fresh_generation_inverse,Covered;exact Hit. Qed.
Lemma fresh_type_roundtrip : forall start support t,
 incl(ety_gens t)support->
 scoped_type(inverse_generation_substitution(fresh_input_generations start support))
  (scoped_type(fresh_input_generations start support)t)=t.
Proof. intros;apply scoped_type_roundtrip,fresh_type_inverse;assumption. Qed.
Lemma fresh_type_injective : forall start support a b,
 incl(ety_gens a)support->incl(ety_gens b)support->
 scoped_type(fresh_input_generations start support)a=
 scoped_type(fresh_input_generations start support)b->a=b.
Proof.
 intros start support a b A B Equal.
 apply(f_equal(scoped_type(inverse_generation_substitution(fresh_input_generations start support))))in Equal.
 rewrite(fresh_type_roundtrip start support a A),(fresh_type_roundtrip start support b B)in Equal;exact Equal.
Qed.
Lemma fresh_type_generations_bounds : forall start support t g,
 incl(ety_gens t)support->
 In g(ety_gens(scoped_type(fresh_input_generations start support)t))->
 start<=g<private_generation_next start support.
Proof.
 intros start support t g Covered Hit;rewrite scoped_type_generations in Hit.
 apply in_map_iff in Hit as[old[<- Hit]].
 destruct(fresh_generation_read start support old(Covered old Hit))as[fresh[Read[Value Bounds]]].
 rewrite Value;exact Bounds.
Qed.
Theorem fresh_input_canon_read : forall start support e t,
 Merge.Tri(en_uf e)->incl(private_merge_support e)support->incl(ety_gens t)support->
 canon(private_input_with_merges(fresh_input_generations start support)e)
  (scoped_type(fresh_input_generations start support)t)=
 scoped_type(fresh_input_generations start support)(canon e t).
Proof.
 intros start support e t Tri Edges Types.
 apply private_canon_read with(back:=inverse_generation_substitution(fresh_input_generations start support));[exact Tri| |].
 - intros g Hit;apply fresh_generation_inverse,Edges;exact Hit.
 - apply fresh_type_inverse;exact Types.
Qed.
Theorem fresh_input_rebind_read : forall start support e t name meaning stamp,
 Merge.Tri(en_uf e)->incl(private_merge_support e)support->incl(ety_gens t)support->
 canon(set_tn(private_input_with_merges(fresh_input_generations start support)e)name meaning stamp)
  (scoped_type(fresh_input_generations start support)t)=
 scoped_type(fresh_input_generations start support)(canon e t).
Proof. intros;rewrite private_rebind_canon;apply fresh_input_canon_read;assumption. Qed.
Lemma private_generation_plan_spec : forall start support rho next,
 private_generation_plan start support=(rho,next)->
 NoDup(map fst rho)/\NoDup(map snd rho)/\
 (forall g,In g(map fst rho)<->In g support)/\
 map snd rho=seq start(next-start)/\
 (forall g,In g support->start<=scoped_generation rho g<next).
Proof.
 intros start support rho next Plan;injection Plan as <- <-.
 split;[apply fresh_generation_domain_unique|];split;[apply fresh_generation_range_unique|].
 split;[apply fresh_generation_domain_exact|];split.
 - rewrite fresh_generation_ranges;unfold private_generation_next.
   replace(start+List.length(dedup_nat_fast support)-start)with(List.length(dedup_nat_fast support))by lia;reflexivity.
 - intros g Hit;destruct(fresh_generation_read start support g Hit)as[fresh[Read[Value Bounds]]].
   rewrite Value;exact Bounds.
Qed.

Example duplicate_support_allocates_one_generation_per_source:
 private_generation_plan 10[2;3;2]=([(3,10);(2,11)],12) /\
 scoped_generation(fresh_input_generations 10[2;3;2])2=11 /\
 scoped_generation(fresh_input_generations 10[2;3;2])3=10.
Proof. repeat split;reflexivity. Qed.
