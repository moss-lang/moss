(* Complete only the transitive interfaces actually required by reachable
   profiles. A missing interface is checked from its real declaration on an
   isolated source snapshot, with distinct output generations. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss NMap Sx Elab ReifyData ReifyFunctorProfiles.
Import ListNotations.
Local Strategy opaque[FUEL ELAB_FUEL fun_env fresh_target_output_view result_types].
Definition declared_functor_profile(source:st)(original:usym)
    :option(reify_exports ety key):=
 match sym original source with
 | Bad _=>None
 | Good(symbol,s1)=>match fun_env symbol s1 with
   | Bad _=>None
   | Good((inputs,_),s2)=>match result_types symbol s2 with
     | Bad _=>None
     | Good(types,s3)=>match fresh_target_output_view inputs symbol s3 with
       | Bad _=>None
       | Good(((outputs,keys),roles),_)=>
         Some(ReifyExports ety key types keys(reify_scope_of outputs)
           (reify_scope_of outputs)(reify_scope_of inputs)roles)
       end
     end
   end
 end.
Definition profile_children(profile:reify_exports ety key):list usym:=
 flat_map(fun k=>match k with KyFun u=>[u]|_=>[]end)
   (rs_items ety key(rx_inputs ety key profile)++rx_items ety key profile).
Fixpoint ensure_functor_profiles(source:st)(needed:list usym)
 (profiles:list(usym*reify_exports ety key)):option(list(usym*reify_exports ety key)):=
 match needed with
 | []=>Some profiles
 | original::rest=>match assoc Nat.eqb original profiles with
   | Some _=>ensure_functor_profiles source rest profiles
   | None=>match declared_functor_profile source original with
     | Some profile=>ensure_functor_profiles source rest(profiles++[(original,profile)])
     | None=>None end
   end
 end.
Fixpoint expand_functor_profiles(fuel:nat)(source:st)(todo seen:list usym)
 (profiles:list(usym*reify_exports ety key)):option(list(usym*reify_exports ety key)):=
 match todo with
 | []=>Some profiles
 | original::rest=>match fuel with
   | 0=>None
   | S remaining=>match assoc Nat.eqb original profiles with
     | None=>None
     | Some profile=>
       let children:=filter(fun u=>negb(existsb(Nat.eqb u)(original::seen++rest)))
         (nodup Nat.eq_dec(profile_children profile))in
       match ensure_functor_profiles source children profiles with
       | None=>None
       | Some complete=>expand_functor_profiles remaining source(rest++children)
           (original::seen)complete
       end
     end
   end
 end.
Definition complete_functor_profile_graph(source:st)
 (profiles:list(usym*reify_exports ety key)):option(list(usym*reify_exports ety key)):=
 expand_functor_profiles(S(st_next source))source(nodup Nat.eq_dec(map fst profiles))[]profiles.

Lemma declared_functor_profile_receipt : forall source original profile,
 declared_functor_profile source original=Some profile->
 exists symbol s1 inputs needs s2 types s3 outputs keys roles final,
   sym original source=Good(symbol,s1) /\
   fun_env symbol s1=Good((inputs,needs),s2) /\
   result_types symbol s2=Good(types,s3) /\
   fresh_target_output_view inputs symbol s3=Good(((outputs,keys),roles),final) /\
   profile=ReifyExports ety key types keys(reify_scope_of outputs)
     (reify_scope_of outputs)(reify_scope_of inputs)roles.
Proof.
 intros source original profile Read;unfold declared_functor_profile in Read.
 destruct(sym original source)as[[symbol s1]|]eqn:Symbol;[|discriminate].
 destruct(fun_env symbol s1)as[[[inputs needs]s2]|]eqn:Inputs;[|discriminate].
 destruct(result_types symbol s2)as[[types s3]|]eqn:Types;[|discriminate].
 destruct(fresh_target_output_view inputs symbol s3)as[[[[outputs keys]roles]final]|]eqn:Outputs;
 [|discriminate].
 inversion Read;subst;do 11 eexists;repeat split;eassumption.
Qed.
Definition interface_profile_origin(source:st)(initial:list(usym*reify_exports ety key))
 (entry:usym*reify_exports ety key):Prop:=
 In entry initial \/ declared_functor_profile source(fst entry)=Some(snd entry).
Lemma ensured_profiles_have_origins : forall source todo before after initial,
 ensure_functor_profiles source todo before=Some after->
 Forall(interface_profile_origin source initial)before->
 Forall(interface_profile_origin source initial)after.
Proof.
 intros source todo;induction todo as[|original rest IH];intros before after initial Run Origins.
 - inversion Run;subst;exact Origins.
 - cbn[ensure_functor_profiles]in Run.
   destruct(assoc Nat.eqb original before)as[known|];[eapply IH;eauto|].
   destruct(declared_functor_profile source original)as[profile|]eqn:Receipt;[|discriminate].
   eapply IH;[exact Run|];apply Forall_app;split;[exact Origins|].
   constructor;[right;exact Receipt|constructor].
Qed.
Lemma expanded_profiles_have_origins : forall fuel source todo seen before after initial,
 expand_functor_profiles fuel source todo seen before=Some after->
 Forall(interface_profile_origin source initial)before->
 Forall(interface_profile_origin source initial)after.
Proof.
 induction fuel as[|fuel IH];intros source todo seen before after initial Run Origins;
 destruct todo as[|original rest];cbn[expand_functor_profiles]in Run;
 try(inversion Run;subst;exact Origins);try discriminate.
 destruct(assoc Nat.eqb original before)as[profile|];[|discriminate].
 destruct(ensure_functor_profiles source _ before)as[complete|]eqn:Complete;[|discriminate].
 eapply IH;[exact Run|];eapply ensured_profiles_have_origins;eauto.
Qed.
Theorem completed_profiles_have_origins : forall source initial complete,
 complete_functor_profile_graph source initial=Some complete->
 Forall(interface_profile_origin source initial)complete.
Proof.
 intros source initial complete Run;unfold complete_functor_profile_graph in Run.
 eapply expanded_profiles_have_origins;[exact Run|].
 apply Forall_forall;intros entry Member;left;exact Member.
Qed.

Lemma profile_lookup_append : forall (profiles extra:list(usym*reify_exports ety key)) original profile,
 assoc Nat.eqb original profiles=Some profile->
 assoc Nat.eqb original(profiles++extra)=Some profile.
Proof.
 induction profiles as[|[key value]rest IH];intros extra original profile Read;[discriminate|].
 cbn in Read|-*;destruct(Nat.eqb original key);[exact Read|eapply IH;exact Read].
Qed.
Lemma ensured_profiles_preserve_lookup : forall source todo before after original profile,
 ensure_functor_profiles source todo before=Some after->
 assoc Nat.eqb original before=Some profile->assoc Nat.eqb original after=Some profile.
Proof.
 intros source todo;induction todo as[|next rest IH];intros before after original profile Run Read.
 - inversion Run;subst;exact Read.
 - cbn[ensure_functor_profiles]in Run.
   destruct(assoc Nat.eqb next before)as[known|];[eapply IH;eauto|].
   destruct(declared_functor_profile source next)as[new|];[|discriminate].
   eapply IH;[exact Run|];apply profile_lookup_append;exact Read.
Qed.
Lemma expanded_profiles_preserve_lookup : forall fuel source todo seen before after original profile,
 expand_functor_profiles fuel source todo seen before=Some after->
 assoc Nat.eqb original before=Some profile->assoc Nat.eqb original after=Some profile.
Proof.
 induction fuel as[|fuel IH];intros source todo seen before after original profile Run Read;
 destruct todo as[|next rest];cbn[expand_functor_profiles]in Run;
 try(inversion Run;subst;exact Read);try discriminate.
 destruct(assoc Nat.eqb next before)as[known|];[|discriminate].
 destruct(ensure_functor_profiles source _ before)as[complete|]eqn:Complete;[|discriminate].
 eapply IH;[exact Run|];eapply ensured_profiles_preserve_lookup;eauto.
Qed.
Theorem completed_profile_graph_prefers_existing : forall source initial complete original profile,
 complete_functor_profile_graph source initial=Some complete->
 assoc Nat.eqb original initial=Some profile->assoc Nat.eqb original complete=Some profile.
Proof.
 intros source initial complete original profile Run Read.
 unfold complete_functor_profile_graph in Run;eapply expanded_profiles_preserve_lookup;eauto.
Qed.
