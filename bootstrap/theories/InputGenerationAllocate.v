(* Reserve the pure input-generation plan in the actual symbol table. The
   existing linker does not call this preparation step yet. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore ForwarderTrace
 InputGenerationScope InputGenerationMerge InputGenerationFresh InputGenerationPlan.
Import ListNotations.
Local Open Scope list_scope.
Local Open Scope string_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

Definition reserve_input_generation_list(mi owner:nat)(support:list nat)
 :M generation_substitution:=
 mmap(fun old=>
  let! private:=new_sym mi "@input" KType(ADType "@input")(OFresh old owner) in
  ret(old,private))support.
Definition reserve_input_generations(mi owner:nat)(support:list nat)
 :M generation_substitution:=
 reserve_input_generation_list mi owner(dedup_nat_fast support).
Definition reserve_link_inputs(mi owner:nat)(target provider:env)
 :M generation_substitution:=
 reserve_input_generations mi owner(link_input_support target provider).

Lemma reserved_input_plan : forall mi owner support s rho final,
 reserve_input_generation_list mi owner support s=Good(rho,final)->
 rho=allocate_private_generations(st_next s)support /\
 st_next final=st_next s+List.length support.
Proof.
 intros mi owner support;induction support as[|old support IH];intros s rho final Run.
 - injection Run as <- <-;split;[reflexivity|cbn;lia].
 - unfold reserve_input_generation_list in Run;cbn[mmap]in Run.
   apply bind_inv in Run as[p[allocated[First Run]]].
   apply bind_inv in Run as[rest[tail[Rest Run]]];injection Run as <- <-.
   apply bind_inv in First as[id[fresh[New Return]]];injection Return as <- <-.
   destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Symbol]].
   destruct(IH fresh rest tail Rest)as[Plan Count].
   split.
   + cbn[allocate_private_generations];rewrite Plan,Next,Id;reflexivity.
   + rewrite Count,Next,Id;cbn;lia.
Qed.
Theorem reserved_private_input_plan : forall mi owner support s rho final,
 reserve_input_generations mi owner support s=Good(rho,final)->
 rho=fresh_input_generations(st_next s)support /\
 st_next final=private_generation_next(st_next s)support.
Proof. intros;apply reserved_input_plan in H;exact H. Qed.
Theorem reserved_link_input_plan : forall mi owner target provider s rho final,
 reserve_link_inputs mi owner target provider s=Good(rho,final)->
 rho=link_input_generations(st_next s)target provider /\
 st_next final=private_generation_next(st_next s)(link_input_support target provider).
Proof. intros;apply reserved_private_input_plan in H;exact H. Qed.

Lemma reserve_input_generation_list_ok : forall mi owner support,
 ok(reserve_input_generation_list mi owner support).
Proof. intros;unfold reserve_input_generation_list;okt. Qed.
Theorem reserve_input_generations_ok : forall mi owner support,
 ok(reserve_input_generations mi owner support).
Proof. intros;apply reserve_input_generation_list_ok. Qed.

Lemma reserved_input_symbols : forall mi owner support s rho final,
 Inv s->reserve_input_generation_list mi owner support s=Good(rho,final)->
 forall old private,In(old,private)rho->
 nget(st_syms final)private=Some(GSym private mi "@input" KType(ADType "@input")None[](OFresh old owner)).
Proof.
 intros mi owner support;induction support as[|old support IH];intros s rho final Valid Run source private Member.
 - injection Run as <- <-;contradiction.
 - unfold reserve_input_generation_list in Run;cbn[mmap]in Run.
   apply bind_inv in Run as[p[allocated[First Run]]].
   apply bind_inv in Run as[rest[tail[Rest Run]]];injection Run as <- <-.
   apply bind_inv in First as[id[fresh[New Return]]];injection Return as <- <-.
   assert(OK:ok(new_sym mi "@input" KType(ADType "@input")(OFresh old owner)))by okt.
   destruct(OK _ _ _ Valid New)as[FreshValid _].
   destruct Member as[Same|Member].
   + injection Same as <- <-.
     pose proof(proj2(proj2(new_sym_shape _ _ _ _ _ _ _ _ New)))as Symbol.
     destruct(reserve_input_generation_list_ok mi owner support fresh rest tail FreshValid Rest)as[_ Frame].
     exact(fr_keep _ _ Frame _ _ Symbol).
   + exact(IH fresh rest tail FreshValid Rest source private Member).
Qed.
Theorem reserved_private_scope : forall mi owner support s rho final,
 reserve_input_generations mi owner support s=Good(rho,final)->
 NoDup(map fst rho) /\ NoDup(map snd rho) /\
 scope_inverse_on rho(inverse_generation_substitution rho)support /\
 (forall old private,In(old,private)rho->st_next s<=private<st_next final).
Proof.
 intros mi owner support s rho final Run.
 destruct(reserved_private_input_plan _ _ _ _ _ _ Run)as[Plan Next];subst rho.
 split;[apply fresh_generation_domain_unique|];split;[apply fresh_generation_range_unique|].
 split;[apply fresh_generation_inverse|].
 intros old private Member;rewrite Next;apply fresh_generation_pair_bounds with(old:=old);exact Member.
Qed.
