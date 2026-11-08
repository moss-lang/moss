(* A shared private input frame for both sides of a reusable link.  This is
   preparation data: FunCtx remains the source-context judgment. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab BodyTr Merge
 InputGenerationScope InputGenerationMerge InputGenerationFresh InputGenerationRuntime.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

Definition input_environment_support(e:env):list nat:=
 reify_generation_ids e ++ private_merge_support e ++
 flat_map(fun t=>ety_gens(canon e t))(reify_scope_types e) ++
 (* Application receipts may retain a method receiver absent from all slots. *)
 flat_map(fun p=>scope_key_gens(fst p))(en_apps e).
Definition link_input_support(target provider:env):list nat:=
 input_environment_support target ++ input_environment_support provider.
Definition link_input_generations(start:nat)(target provider:env):generation_substitution:=
 fresh_input_generations start(link_input_support target provider).

Definition generation_input_transfer(rho:generation_substitution):subst:=
 map(fun p=>(snd p,TAbs(fst p)))rho.
Definition source_input_frame(Fl:string->nat)(rho:generation_substitution)
 (target provider:env):link_input_frame:=
 LinkInputFrame
  (generation_input_transfer(filter(fun p=>nmem(fst p)(map fst(en_tn target)))rho))
  (generation_input_transfer(filter(fun p=>negb(nmem(fst p)(map fst(en_tn target))))rho))
  (map(fun p=>(fst p,tr Fl(scoped_type rho(canon provider(snd p)))))(tymap provider))
  (semantic_generation_substitution rho).

Lemma input_support_merge : forall e g,
 In g(private_merge_support e)->In g(input_environment_support e).
Proof. intros;unfold input_environment_support;apply in_or_app;right;apply in_or_app;left;assumption. Qed.
Lemma input_support_type : forall e t g,
 In t(reify_scope_types e)->In g(ety_gens t)->In g(input_environment_support e).
Proof.
 intros e t g T G;unfold input_environment_support;apply in_or_app;left.
 unfold reify_generation_ids;rewrite dedup_nat_fast_eq;apply nodup_In.
 apply in_or_app;right;apply in_or_app;right.
 apply in_flat_map;exists t;auto.
Qed.
Lemma input_support_canonical_type : forall e t g,
 In t(reify_scope_types e)->In g(ety_gens(canon e t))->In g(input_environment_support e).
Proof.
 intros e t g T G;unfold input_environment_support;apply in_or_app;right;apply in_or_app;right;apply in_or_app;left.
 apply in_flat_map;exists t;auto.
Qed.
Lemma input_support_application_key : forall e k record,
 In(k,record)(en_apps e)->incl(scope_key_gens k)(input_environment_support e).
Proof.
 intros e k record Member g G;unfold input_environment_support.
 apply in_or_app;right;apply in_or_app;right;apply in_or_app;right.
 apply in_flat_map;exists(k,record);split;[exact Member|exact G].
Qed.
Lemma link_support_side : forall target provider e,
 (e=target \/ e=provider)->forall g,
 In g(input_environment_support e)->In g(link_input_support target provider).
Proof. intros target provider e [Same|Same] g G;subst e;unfold link_input_support;apply in_or_app;auto. Qed.

Theorem link_input_canonical_read : forall start target provider e t,
 (e=target \/ e=provider)->Tri(en_uf e)->In t(reify_scope_types e)->
 canon(private_input_with_merges(link_input_generations start target provider)e)
  (scoped_type(link_input_generations start target provider)t)=
 scoped_type(link_input_generations start target provider)(canon e t).
Proof.
 intros start target provider e t Side Normal Member;unfold link_input_generations.
 apply fresh_input_canon_read;[exact Normal| |].
 - intros g G;apply(link_support_side target provider e Side);apply input_support_merge;exact G.
 - intros g G;apply(link_support_side target provider e Side);eapply input_support_type;eassumption.
Qed.
Theorem link_input_readback : forall Fl start target provider e t,
 (e=target \/ e=provider)->In t(reify_scope_types e)->
 ExecutionModel.app_subst
  (semantic_generation_substitution(inverse_generation_substitution(link_input_generations start target provider)))
  (tr Fl(scoped_type(link_input_generations start target provider)(canon e t)))=
 tr Fl(canon e t).
Proof.
 intros Fl start target provider e t Side Member.
 rewrite <-scoped_type_read.
 rewrite scoped_type_roundtrip;[reflexivity|].
 intros g G;apply fresh_generation_inverse.
 apply(link_support_side target provider e Side);eapply input_support_canonical_type;eassumption.
Qed.

Lemma generation_input_transfer_domains : forall rho,
 map fst(generation_input_transfer rho)=map snd rho.
Proof. intros;unfold generation_input_transfer;rewrite map_map;apply map_ext;intros[g v];reflexivity. Qed.
Lemma generation_input_transfer_read : forall rho g v,
 NoDup(map snd rho)->In(g,v)rho->lookup_subst v(generation_input_transfer rho)=Some(TAbs g).
Proof.
 intros rho g v Unique Member;apply frame_lookup_member.
 - rewrite generation_input_transfer_domains;exact Unique.
 - unfold generation_input_transfer;apply in_map_iff;exists(g,v);auto.
Qed.
Lemma source_frame_domains : forall Fl rho target provider,
 map fst(link_frame_inputs(source_input_frame Fl rho target provider)) ++
 map fst(link_frame_captures(source_input_frame Fl rho target provider)) =
 map snd(filter(fun p=>nmem(fst p)(map fst(en_tn target)))rho) ++
 map snd(filter(fun p=>negb(nmem(fst p)(map fst(en_tn target))))rho).
Proof. intros;unfold source_input_frame,generation_input_transfer;cbn;rewrite !map_map;reflexivity. Qed.
Lemma source_frame_name_read : forall Fl rho target provider name t,
 tn_get provider name=Some t->
 lookup_subst name(link_frame_names(source_input_frame Fl rho target provider))=
 Some(tr Fl(scoped_type rho(canon provider t))).
Proof.
 intros Fl rho target provider name t Read.
 change(lookup_subst name(map(fun p=>(fst p,tr Fl(scoped_type rho(canon provider(snd p)))))(tymap provider))=Some(tr Fl(scoped_type rho(canon provider t)))).
 unfold tn_get in Read;unfold tymap.
 rewrite map_map.
 assert(MapRead:forall entries:list(nat*(ety*nat)),
 lookup_subst name(map(fun p=>(fst p,tr Fl(scoped_type rho(canon provider(fst(snd p))))))entries)=
 option_map(fun p=>tr Fl(scoped_type rho(canon provider(fst p))))(nassoc name entries)).
 { intro entries;induction entries as[|[n[t0 stamp]]entries IH];cbn;[reflexivity|].
   destruct(Nat.eqb name n);[reflexivity|exact IH]. }
 rewrite MapRead;destruct(nassoc name(en_tn provider))as[[old stamp]|]eqn:Entry;cbn in Read;try discriminate.
 injection Read as <-;reflexivity.
Qed.
Lemma type_name_in_scope_types : forall e name t,
 tn_get e name=Some t->In t(reify_scope_types e).
Proof.
 intros e name t Read;unfold tn_get in Read.
 destruct(nassoc name(en_tn e))as[[old stamp]|]eqn:Entry;cbn in Read;try discriminate.
 injection Read as Same;subst old.
 unfold reify_scope_types;apply in_or_app;left.
 apply in_map_iff;exists(name,t);split;[reflexivity|].
 unfold tymap;apply in_map_iff;exists(name,(t,stamp));split;[reflexivity|].
 apply Merge.nassoc_in;exact Entry.
Qed.
Theorem source_frame_name_readback : forall Fl start target provider name t,
 tn_get provider name=Some t->
 let rho:=link_input_generations start target provider in
 lookup_subst name(link_frame_names(source_input_frame Fl rho target provider))=
 Some(tr Fl(scoped_type rho(canon provider t))) /\
 ExecutionModel.app_subst(semantic_generation_substitution(inverse_generation_substitution rho))
  (tr Fl(scoped_type rho(canon provider t)))=tr Fl(canon provider t).
Proof.
 intros;split;[eapply source_frame_name_read;eassumption|].
 apply link_input_readback;[right;reflexivity|eapply type_name_in_scope_types;eassumption].
Qed.
