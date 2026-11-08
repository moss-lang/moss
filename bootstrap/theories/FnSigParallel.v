(* Parameter spellings do not change the type reader or its actual effects. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ForwarderTrace.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL ety_of].
Definition read_parameter(mi:nat)(tm:list(nat*ety))(e:env)(this:option ety)(p:name*aty):M(name*ety):=
 let! y:=ety_of(snd p)mi tm e this in ret(fst p,y).
Lemma parameter_types_parallel:forall mi tm e this ps1 ps2 s qs1 final,
 map snd ps1=map snd ps2->mmap(read_parameter mi tm e this)ps1 s=Good(qs1,final)->
 exists qs2,mmap(read_parameter mi tm e this)ps2 s=Good(qs2,final)/\map snd qs1=map snd qs2.
Proof.
 intros mi tm e this ps1;induction ps1 as[|[n1 t1]ps1 IH];intros[|[n2 t2]ps2]s qs1 final Types E;
 cbn[map snd]in Types;try discriminate Types.
 - cbn[mmap]in E;injection E as <- <-;exists[];split;reflexivity.
 - injection Types as Ty RestTypes;subst t2.
   cbn[mmap]in E;apply bind_inv in E as[p[mid[Read E]]].
   unfold read_parameter in Read;apply bind_inv in Read as[y[after[TyRead Read]]];injection Read as <- <-;cbn[fst snd]in TyRead.
   apply bind_inv in E as[tail[last[Tail E]]];injection E as <- <-.
   destruct(IH ps2 _ _ _ RestTypes Tail)as[qs2[Run Eq]].
   exists((n2,y)::qs2);split.
   + assert(Head:read_parameter mi tm e this(n2,t1)s=Good((n2,y),after)).
     {unfold read_parameter;cbn[fst snd];unfold mbind;rewrite TyRead;reflexivity. }
     cbn[mmap];unfold mbind at 1;rewrite Head;cbn[mbind];unfold mbind at 1;rewrite Run;reflexivity.
   + cbn[map snd];f_equal;exact Eq.
Qed.
Theorem fn_sig_parallel_types:forall g1 g2 tm e this s sg1 final,
 gs_mod g1=gs_mod g2->map snd(fn_params g1)=map snd(fn_params g2)->fn_ret g1=fn_ret g2->
 fn_sig g1 tm e this s=Good(sg1,final)->exists sg2,
 fn_sig g2 tm e this s=Good(sg2,final)/\map snd(sg_params sg1)=map snd(sg_params sg2)/\
 sg_ret sg1=sg_ret sg2/\sg_this sg1=sg_this sg2.
Proof.
 intros g1 g2 tm e this s sg1 final Home Params Return E;unfold fn_sig in E.
 apply bind_inv in E as[ps[mid[Read E]]].
 change(mmap(read_parameter(gs_mod g1)tm e this)(fn_params g1)s=Good(ps,mid))in Read.
 destruct(parameter_types_parallel _ _ _ _ _ _ _ _ _ Params Read)as[ps2[Read2 Types]].
 apply bind_inv in E as[r[last[Ret E]]];injection E as <- <-.
 exists(FSig ps2 r this);split.
 - unfold fn_sig;fold(read_parameter(gs_mod g2)tm e this).
   rewrite <-Home;unfold mbind at 1;rewrite Read2.
   cbn[mbind]. rewrite <-Return;unfold mbind at 1;rewrite Ret;reflexivity.
 - cbn[sg_params sg_ret sg_this];repeat split;assumption||reflexivity.
Qed.
Lemma copied_parameter_types:forall A(names:list string)(types:list A),
 List.length names=List.length types->map snd(combine names types)=types.
Proof. intros A names;induction names;intros[|t types]Len;cbn[List.length]in Len;try discriminate;[reflexivity|].
 cbn[combine map snd];f_equal;apply IHnames;lia. Qed.
Corollary fn_sig_forwarder_parallel:forall target provider tm e this s sg final,
 gs_decl provider=forwarder_decl target->gs_mod provider=gs_mod target->
 fn_sig provider tm e this s=Good(sg,final)->exists target_sig,
 fn_sig target tm e this s=Good(target_sig,final)/\map snd(sg_params sg)=map snd(sg_params target_sig)/\
 sg_ret sg=sg_ret target_sig/\sg_this sg=sg_this target_sig.
Proof.
 intros target provider tm e this s sg final Decl Home E.
 eapply fn_sig_parallel_types;[exact Home| | |exact E].
 - unfold fn_params at 1;rewrite Decl;unfold forwarder_decl;cbn[fn_params gs_decl snd].
   apply copied_parameter_types;rewrite map_length,seq_length,map_length;reflexivity.
 - unfold fn_ret at 1;rewrite Decl;unfold forwarder_decl;reflexivity.
Qed.
