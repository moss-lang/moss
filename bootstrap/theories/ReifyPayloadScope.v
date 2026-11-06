(* Successful generic payload extraction can only return generations read
   from its explicit formal-parameter meanings. This is actual execution,
   not an assumed payload-formation condition. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit CoreMoss NMap Sx Elab ElabSound BodyTr ReifyPayload ReifyNominalSource.
Import ListNotations.
Local Strategy opaque[FUEL ELAB_FUEL reqs].
Definition generations_within(names:list nat)(t:ety):Prop:=incl(ety_gens t)names.
Definition meanings_within(names:list nat)(tm:list(nat*ety)):Prop:=
 Forall(fun p=>generations_within names(snd p))tm.
Lemma translated_type_generations : forall fields t,
 ty_frees(tr fields t)=ety_gens t.
Proof.
 intros fields t;induction t as[| |key|head args IH|members IH|members IH]using ety_ind';
 cbn[tr ty_frees ety_gens];try reflexivity.
 all:induction IH;cbn in *;[reflexivity|rewrite H,IHIH;reflexivity].
Qed.
Lemma gens_union_members : forall members,
 ety_gens(YUnion members)=flat_map ety_gens members.
Proof. induction members;cbn;[reflexivity|f_equal;exact IHmembers]. Qed.
Lemma gens_nominal_arguments : forall head args,
 ety_gens(YNom head args)=flat_map(fun p=>ety_gens(snd p))args.
Proof. intros head args;induction args;cbn;[reflexivity|f_equal;exact IHargs]. Qed.
Lemma gens_record_fields : forall fields,
 ety_gens(YRec fields)=flat_map(fun p=>ety_gens(snd p))fields.
Proof. induction fields;cbn;[reflexivity|f_equal;exact IHfields]. Qed.
Lemma flat_generations_within : forall X(f:X->ety)names xs,
 Forall(fun x=>generations_within names(f x))xs->
 incl(flat_map(fun x=>ety_gens(f x))xs)names.
Proof.
 intros X f names xs Bound t Present;apply in_flat_map in Present as[x[Member Present]].
 apply Forall_forall with(x:=x)in Bound;[apply Bound;exact Present|exact Member].
Qed.
Lemma member_generations_within : forall names t,
 generations_within names t->Forall(generations_within names)(Elab.members t).
Proof.
 intros names t Bound;destruct t;try(constructor;[exact Bound|constructor]);try constructor.
 apply Forall_forall;intros m Member t Present;apply Bound.
 rewrite gens_union_members;apply in_flat_map;exists m;auto.
Qed.
Lemma union_flatten_generations_within : forall names operands,
 Forall(generations_within names)operands->
 generations_within names(YUnion(flat_map Elab.members operands)).
Proof.
 intros names operands Bound;unfold generations_within;rewrite gens_union_members.
 apply(flat_generations_within ety(fun x=>x)names).
 apply Forall_forall;intros t Present;apply in_flat_map in Present as[operand[Member Present]].
 apply Forall_forall with(x:=operand)in Bound;[|exact Member].
 apply member_generations_within in Bound;apply Forall_forall with(x:=t)in Bound;assumption.
Qed.
Lemma mmap_outputs_property : forall X Y(f:X->M Y)(P:Y->Prop),
 (forall x s y after,f x s=Good(y,after)->P y)->
 forall xs s ys after,mmap f xs s=Good(ys,after)->Forall P ys.
Proof.
 intros X Y f P Each xs;induction xs as[|x rest IH];intros s ys after Run;cbn in Run.
 - inversion Run;constructor.
 - apply bind_inv in Run as[y[middle[Head Run]]].
   apply bind_inv in Run as[tail[final[Tail Run]]];inversion Run;subst.
   constructor;[eapply Each;exact Head|eapply IH;exact Tail].
Qed.
Lemma nassoc_meaning_within : forall names tm key value,
 meanings_within names tm->nassoc key tm=Some value->generations_within names value.
Proof.
 intros names tm;induction tm as[|[name meaning]rest IH];intros key value Bound Read;[discriminate|].
 inversion Bound;subst;cbn[nassoc assoc]in Read;destruct(Nat.eqb key name)eqn:Same.
 - inversion Read;subst;assumption.
 - eapply IH;eassumption.
Qed.
Lemma nominal_arguments_within : forall names tm mi symbol s args after,
 meanings_within names tm->nominal_args mi symbol tm s=Good(args,after)->
 Forall(fun p=>generations_within names(snd p))args.
Proof.
 intros names tm mi symbol s args after Bound Run;unfold nominal_args in Run.
 apply bind_inv in Run as[requirements[middle[Read Run]]].
 eapply mmap_outputs_property;[|exact Run].
 intros requirement state pair final Step.
 cbn beta in Step;destruct(nassoc requirement tm)as[value|]eqn:Meaning in Step.
 - injection Step as <- _;cbn;eapply nassoc_meaning_within;[exact Bound|exact Meaning].
 - apply bind_inv in Step as[other[last[Symbol Error]]];exfalso;eapply err_not_good;exact Error.
Qed.

Definition skeleton_type_scoped(fuel:nat):Prop:=
 forall syntax mi tm receiver names s out after,
 meanings_within names tm->
 (forall t,receiver=Some t->generations_within names t)->
 skeleton_type fuel syntax mi tm empty_env receiver s=Good(out,after)->
 generations_within names out.
Definition skeleton_arguments_scoped(fuel:nat):Prop:=
 forall mi brackets outer names s out after,
 meanings_within names outer->
 skeleton_arguments fuel empty_env mi brackets outer s=Good(out,after)->meanings_within names out.
Definition skeleton_symbol_scoped(fuel:nat):Prop:=
 forall mi symbol tm names s out after,
 meanings_within names tm->
 skeleton_symbol fuel mi symbol tm empty_env s=Good(out,after)->generations_within names out.
Theorem successful_skeletons_scoped : forall fuel,
 skeleton_type_scoped fuel/\skeleton_arguments_scoped fuel/\skeleton_symbol_scoped fuel.
Proof.
 induction fuel as[|fuel[TypeScoped[ArgsScoped SymbolScoped]]].
 - repeat split;unfold skeleton_type_scoped,skeleton_arguments_scoped,skeleton_symbol_scoped;
   intros;discriminate.
 - split;[|split].
   + intros syntax mi tm receiver names s out after Bound This Run.
     destruct syntax as[| | |fields|members|path brackets];cbn[skeleton_type]in Run.
     * inversion Run;subst;unfold generations_within,incl;cbn;contradiction.
     * destruct receiver as[value|];[inversion Run;subst;apply This;reflexivity|].
       exfalso;eapply err_not_good;exact Run.
     * inversion Run;subst;unfold generations_within,incl;cbn;contradiction.
     * apply bind_inv in Run as[values[middle[Fields Run]]];inversion Run;subst.
       unfold generations_within;rewrite gens_record_fields;apply flat_generations_within.
       eapply mmap_outputs_property;[|exact Fields].
       intros field state pair final Step;cbn beta in Step.
       apply bind_inv in Step as[value[last[Typed Step]]];injection Step as <- _;cbn.
       eapply TypeScoped;eauto.
     * apply bind_inv in Run as[values[middle[Members Run]]];inversion Run;subst.
       apply union_flatten_generations_within;eapply mmap_outputs_property;[|exact Members].
       intros member state value final Step;exact(TypeScoped member mi tm receiver names state value final Bound This Step).
     * apply bind_inv in Run as[symbol[s1[Resolved Run]]].
       destruct brackets as[bindings|].
       -- apply bind_inv in Run as[u[s2[Relevant Run]]].
          apply bind_inv in Run as[arguments[s3[Arguments Run]]].
          apply bind_inv in Run as[v[s4[Total Run]]].
          destruct(kind_eqb(gs_kind symbol)KType).
          ++ exfalso;eapply err_not_good;exact Run.
          ++ eapply SymbolScoped;[|exact Run];apply Forall_app;split;[|exact Bound].
             eapply ArgsScoped;eauto.
       -- eapply SymbolScoped;eauto.
   + intros mi brackets outer names s out after Bound Run;cbn[skeleton_arguments]in Run.
     apply bind_inv in Run as[bindings[middle[Bindings Run]]];inversion Run;subst.
     unfold meanings_within;apply Forall_rev;eapply mmap_outputs_property;[|exact Bindings].
     intros bracket state pair final Step;cbn beta in Step.
     apply bind_inv in Step as[target[s1[Path Step]]].
     apply bind_inv in Step as[symbol[s2[Picked Step]]].
     destruct target as[[id|module]|].
     2,3:exfalso;eapply err_not_good;exact Picked.
     apply bind_inv in Picked as[found[s3[Found Picked]]].
     destruct(kind_eqb(gs_kind found)KType).
     2:exfalso;eapply err_not_good;exact Picked.
     injection Picked as <- <-.
     destruct(snd bracket)as[[path|][rename|]args];
       try(exfalso;eapply err_not_good;exact Step).
     apply bind_inv in Step as[value[last[Typed Step]]];injection Step as <- _;cbn.
     eapply TypeScoped with(receiver:=None);[|intros t Impossible;discriminate|exact Typed].
     cbn[tymap empty_env en_tn map];rewrite app_nil_r;exact Bound.
   + intros mi symbol tm names s out after Bound Run;cbn[skeleton_symbol]in Run.
     destruct(gs_kind symbol).
     * destruct(nassoc(gs_id symbol)tm)as[value|]eqn:Meaning in Run.
       -- injection Run as <- _;eapply nassoc_meaning_within;eauto.
       -- exfalso;eapply err_not_good;exact Run.
     * apply bind_inv in Run as[arguments[middle[Arguments Run]]];inversion Run;subst.
       unfold generations_within;rewrite gens_nominal_arguments;apply flat_generations_within.
       eapply nominal_arguments_within;eauto.
     * destruct(gs_decl symbol);try discriminate.
       eapply TypeScoped with(receiver:=None);[exact Bound|intros t Impossible;discriminate|exact Run].
     * apply bind_inv in Run as[arguments[middle[Arguments Run]]];inversion Run;subst.
       unfold generations_within;rewrite gens_nominal_arguments;apply flat_generations_within.
       eapply nominal_arguments_within;eauto.
     * exfalso;eapply err_not_good;exact Run.
     * exfalso;eapply err_not_good;exact Run.
     * exfalso;eapply err_not_good;exact Run.
     * exfalso;eapply err_not_good;exact Run.
     * exfalso;eapply err_not_good;exact Run.
Qed.

Local Strategy opaque[skeleton_type skeleton_arguments skeleton_symbol].
Lemma cached_skeleton_payload_scoped : forall source head parameters payload after fields,
 nget(st_reqs source)head=Some parameters->
 skeleton_tag_payload head source=Good(payload,after)->
 incl(ty_frees(tr fields payload))parameters.
Proof.
 intros source head parameters payload after fields Cached Run.
 rewrite translated_type_generations;unfold skeleton_tag_payload in Run.
 apply bind_inv in Run as[symbol[middle[Symbol Run]]].
 unfold sym in Symbol;destruct(nget(st_syms source)head)as[actual|]eqn:Read;[|discriminate].
 injection Symbol as <- <-.
 destruct(gs_decl actual)eqn:Declaration in Run;
   try(inversion Run;subst;unfold incl;cbn;contradiction).
 apply bind_inv in Run as[actualParameters[s1[Requirements Run]]].
 rewrite(actual_cached_requirements_read _ _ _ Cached)in Requirements;injection Requirements as <- <-.
 eapply(proj1(successful_skeletons_scoped 200))with(receiver:=None);[|intros t Impossible;discriminate|exact Run].
 apply Forall_forall;intros pair Member;apply in_map_iff in Member as[t[<- Present]].
 cbn;unfold generations_within,incl;cbn;intros key[<-|[]];exact Present.
Qed.
