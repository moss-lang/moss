(* Actual source unification preserves full physical type formation, not
   merely the shape predicate used by the source soundness proof. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Sx Elab ElabSound BodyTr CanonTypes Merge ReifyCanonFormation.
Import ListNotations.
Section Formation.
Variables(Sg:gsig)(Phi:ctx)(Fl:String.string->nat)(rho:subst).
Hypothesis Tags:MergeWf.tags_typeonly Sg.
Definition source_fully_formed(raw:ety):Prop:=
 ety_ok raw=true /\wf_ty Sg Phi(source_type_read Fl rho raw).
Definition source_uf_formed(e:env):Prop:=
 forall generation raw,In(generation,raw)(en_uf e)->source_fully_formed raw.
Lemma source_uf_shape : forall e,source_uf_formed e->UfOK(en_uf e).
Proof. intros e U g t Member;exact(proj1(U g t Member)). Qed.
Lemma canonical_fully_formed : forall e raw,
 source_uf_formed e->source_fully_formed raw->source_fully_formed(canon e raw).
Proof.
 intros e raw U[Shape Formed];split.
 - apply canon_ok;[apply source_uf_shape;exact U|exact Shape].
 - unfold canon;eapply canonical_source_type_formed;[exact Tags| | |apply ety_ok_wf;exact Shape|exact Formed].
   + intros g t Read;apply ety_ok_wf;exact(proj1(U g t(Merge.nassoc_in _ _ _ _ Read))).
   + intros g t Read;exact(proj2(U g t(Merge.nassoc_in _ _ _ _ Read))).
Qed.
Lemma set_uf_fully_formed : forall e g t,
 source_uf_formed e->source_fully_formed t->source_uf_formed(set_uf e g t).
Proof.
 intros e g t U Typed;unfold source_uf_formed,set_uf in *;cbn[en_uf].
 induction(en_uf e)as[|[a y]uf IH];intros h v Member;cbn in Member.
 - destruct Member as[Same|[]];injection Same as <- <-;exact Typed.
 - destruct(Nat.eqb g a).
   + destruct Member as[Same|Member];[injection Same as <- <-;exact Typed|].
     apply(U h v);right;exact Member.
   + destruct Member as[Same|Member].
     * injection Same as <- <-;apply(U a y);left;reflexivity.
     * apply(IH(fun h v Member=>U h v(or_intror Member))h v Member).
Qed.
Definition pair_fully_formed(pair:ety*ety):Prop:=
 source_fully_formed(fst pair)/\source_fully_formed(snd pair).
Lemma combine_fully_formed : forall xs ys,
 Forall source_fully_formed xs->Forall source_fully_formed ys->
 Forall pair_fully_formed(combine xs ys).
Proof.
 induction xs as[|x xs IH];intros[|y ys]Left Right;try solve[constructor].
 inversion Left;inversion Right;subst;constructor;[split;assumption|apply IH;assumption].
Qed.
Lemma mfold_uf_fully_formed : forall(step:env->ety*ety->M env),
 (forall e pair s after final,source_uf_formed e->pair_fully_formed pair->
   step e pair s=Good(after,final)->source_uf_formed after)->
 forall pairs e s after final,Forall pair_fully_formed pairs->source_uf_formed e->
 mfold step e pairs s=Good(after,final)->source_uf_formed after.
Proof.
 intros step Step pairs;induction pairs as[|pair rest IH];intros e s after final Pairs U Run;cbn[mfold]in Run.
 - injection Run as <- _;exact U.
 - inversion Pairs;subst;apply bind_inv in Run as[middle[mid[First Rest]]].
   eapply IH;[eassumption| |exact Rest];eapply Step;eassumption.
Qed.
Lemma nominal_arguments_fully_formed : forall n args,
 source_fully_formed(YNom n args)->Forall source_fully_formed(map snd args).
Proof.
 intros n args[Shape Formed];pose proof(nominal_args_ok _ _ Shape)as Shapes.
 rewrite Forall_map,Forall_forall in *;intros pair Member;split;[exact(Shapes pair Member)|].
 rewrite source_nominal_read in Formed.
 eapply(@formed_nominal_argument Sg Phi n(source_arguments_read Fl rho args)
   (fst pair,source_type_read Fl rho(snd pair)));[exact Formed|].
 unfold source_arguments_read;apply in_map_iff;exists pair;split;[reflexivity|exact Member].
Qed.
Lemma union_members_fully_formed : forall members,
 source_fully_formed(YUnion members)->Forall source_fully_formed members.
Proof.
 intros members[Shape Formed];pose proof(union_members_ok _ Shape)as Shapes.
 rewrite source_union_read in Formed;inversion Formed;subst.
 apply Forall_forall;intros raw Member;split;[apply(proj1(Forall_forall _ _)Shapes);exact Member|].
 match goal with Known:Forall(wf_ty Sg Phi)(map(source_type_read Fl rho)members)|-_=>
   apply(proj1(Forall_forall _ _)Known);apply in_map;exact Member end.
Qed.
Lemma record_fields_fully_formed : forall fields,
 source_fully_formed(YRec fields)->Forall source_fully_formed(map snd fields).
Proof.
 intros fields[Shape Formed];pose proof(record_fields_ok _ Shape)as Shapes.
 rewrite Forall_map,Forall_forall in *;intros pair Member;split;[exact(Shapes pair Member)|].
 rewrite source_record_read in Formed.
 eapply(@formed_nominal_argument Sg Phi rec_nom
   (map(fun p=>(Fl(fst p),source_type_read Fl rho(snd p)))fields)
   (Fl(fst pair),source_type_read Fl rho(snd pair)));[exact Formed|].
 apply in_map_iff;exists pair;split;[reflexivity|exact Member].
Qed.
Theorem actual_unify_preserves_full_formation : forall fuel e mi a b why s after final,
 source_uf_formed e->source_fully_formed a->source_fully_formed b->
 unify fuel e mi a b why s=Good(after,final)->source_uf_formed after.
Proof.
 induction fuel as[|fuel IH];intros e mi a b why s after final U Left Right Run;[discriminate|].
 cbn[unify]in Run.
 pose proof(canonical_fully_formed e a U Left)as Ca.
 pose proof(canonical_fully_formed e b U Right)as Cb.
 remember(canon e a)as ca;remember(canon e b)as cb.
 destruct(ety_eqb ca cb);[injection Run as <- _;exact U|].
 destruct ca as[| |ga|na xa|xa|xa];destruct cb as[| |gb|nb xb|xb|xb];
   try solve[Merge.noerr Run].
 all:try(match type of Run with
   | (if nmem ?g(ety_gens ?t)then _ else _) _=_=>
      destruct(nmem g(ety_gens t));[Merge.noerr Run|];
      injection Run as <- _;apply set_uf_fully_formed;assumption end).
 - destruct(Nat.eqb na nb);[|Merge.noerr Run].
   destruct(list_eq_dec Nat.eq_dec(map fst xa)(map fst xb));[|Merge.noerr Run].
   eapply mfold_uf_fully_formed;[| |exact U|exact Run].
   + intros start[left right]initial finish finished Inv[LA LB]Step;cbn[fst snd]in *.
     exact(IH start mi left right why initial finish finished Inv LA LB Step).
   + apply combine_fully_formed;[exact(nominal_arguments_fully_formed na xa Ca)|exact(nominal_arguments_fully_formed nb xb Cb)].
 - destruct(Nat.eqb(List.length xa)(List.length xb));[|Merge.noerr Run].
   eapply mfold_uf_fully_formed;[| |exact U|exact Run].
   + intros start[left right]initial finish finished Inv[LA LB]Step;cbn[fst snd]in *.
     exact(IH start mi left right why initial finish finished Inv LA LB Step).
   + apply combine_fully_formed;apply union_members_fully_formed;eassumption.
 - destruct(list_eq_dec String.string_dec(map fst xa)(map fst xb));[|Merge.noerr Run].
   eapply mfold_uf_fully_formed;[| |exact U|exact Run].
   + intros start[left right]initial finish finished Inv[LA LB]Step;cbn[fst snd]in *.
     exact(IH start mi left right why initial finish finished Inv LA LB Step).
   + apply combine_fully_formed;apply record_fields_fully_formed;eassumption.
Qed.
Definition signature_fully_formed(signature:fsig):Prop:=
 Forall source_fully_formed(map snd(sg_params signature))/\
 source_fully_formed(sg_ret signature)/\
 match sg_this signature with Some receiver=>source_fully_formed receiver|None=>True end.
Theorem actual_unify_signature_preserves_full_formation : forall e mi a b name s after final,
 source_uf_formed e->signature_fully_formed a->signature_fully_formed b->
 unify_sig e mi a b name s=Good(after,final)->source_uf_formed after.
Proof.
 intros e mi a b name s after final U[Pa[Ra Ta]][Pb[Rb Tb]]Run.
 unfold unify_sig in Run;destruct(negb(Nat.eqb _ _));[Merge.noerr Run|].
 apply bind_inv in Run as[middle[mid[Params Run]]].
 assert(Um:source_uf_formed middle).
 { eapply mfold_uf_fully_formed;[|exact(combine_fully_formed _ _ Pa Pb)|exact U|exact Params].
   intros start[left right]initial finish finished Inv[LA LB]Step;cbn[fst snd]in Step.
   exact(actual_unify_preserves_full_formation 200 start mi left right _ initial finish finished Inv LA LB Step). }
 apply bind_inv in Run as[last[stlast[Result Run]]].
 assert(Ul:source_uf_formed last).
 { exact(actual_unify_preserves_full_formation 200 middle mi(sg_ret a)(sg_ret b)_ mid last stlast Um Ra Rb Result). }
 destruct(sg_this a)as[ta|],(sg_this b)as[tb|];cbn in Ta,Tb.
 - exact(actual_unify_preserves_full_formation 200 last mi ta tb _ stlast after final Ul Ta Tb Run).
 - injection Run as <- _;exact Ul.
 - injection Run as <- _;exact Ul.
 - injection Run as <- _;exact Ul.
Qed.

End Formation.
