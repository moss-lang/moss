(* Real physical method rows and the genuine semantic receiver domain close
   the finite provision-key interface required by a lambda. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodySoundCore BodyTr CtxMerge NMapEnum
 MethodRows LambdaKeysAvailable.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition LambdaKeyCanonical(e:env)(k:key):Prop:=
 match k with KyMth r _=>ety_ok r=true/\canon e r=r|_=>True end.
Lemma all_keys_canonical:forall e,MethodRowsCanonical e->Forall(LambdaKeyCanonical e)(all_keys e).
Proof.
 intros e Clean;apply Forall_forall;intros k Member;unfold all_keys in Member.
 repeat apply in_app_or in Member as[Member|Member].
 - apply in_map_iff in Member as(q&Eq&Hit);subst k;exact I.
 - apply in_map_iff in Member as(q&Eq&Hit);subst k;exact I.
 - apply in_flat_map in Member as([mid rows]&Outer&Member);cbn[fst snd]in Member.
   apply in_map_iff in Member as([r sg]&Eq&Hit);cbn[fst]in Eq;subst k.
   unfold LambdaKeyCanonical;pose proof(Clean mid rows(proj1(elems_lookup _ _ _ _)Outer))as Good.
   rewrite Forall_forall in Good;exact(Good(r,sg)Hit).
 - apply in_map_iff in Member as(q&Eq&Hit);subst k;exact I.
Qed.
Definition LambdaMethodDomain(Fl:string->nat)(e:env)(C:A.cx):Prop:=
 forall receiver mid sg,A.mlook receiver mid(A.c_mths C)=Some sg->
 exists raw,ety_ok raw=true/\canon e raw=raw/\tr Fl raw=receiver.
Section Interface.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Lemma key_provides:forall c e F C k,
 CxRel w R Fl c e F C->LambdaKeyCanonical e k->LambdaKeyAvailable e k->
 A.provides C(key_tr Fl k).
Proof.
 intros c e F C k HC Valid Available.
 destruct k as[v|f|r mid|u];cbn[key_tr A.provides LambdaKeyAvailable LambdaKeyCanonical]in *.
 - rewrite(cr_vals _ _ _ _ _ _ _ HC);destruct(nget(en_vals e)v);[discriminate|contradiction].
 - rewrite(cr_fns _ _ _ _ _ _ _ HC);destruct(nget(en_fns e)f);[discriminate|contradiction].
 - destruct Valid as[Good Stable].
   pose proof(cr_mths _ _ _ _ _ _ _ HC r mid ltac:(now rewrite Stable))as Read.
   cbn beta in Read;rewrite Stable in Read;change(A.mlook(tr Fl r)mid(A.c_mths C)<>None);rewrite Read;destruct(get_mth e r mid);[discriminate|contradiction].
 - pose proof(cr_funspresent _ _ _ _ _ _ _ HC u)as Present.
   destruct(nget(en_funs e)u);[|contradiction].
   destruct(A.nlook u(A.c_funs C));[discriminate|discriminate Present].
Qed.
Theorem all_keys_provides:forall c e F C,
 CxRel w R Fl c e F C->MethodRowsUnique e->MethodRowsCanonical e->LambdaMethodDomain Fl e C->
 forall key,In key(map(key_tr Fl)(all_keys e))<->
 A.provides C key/\(match key with ITy _=>False|_=>True end)/\key=ExecutionModel.Elaboration.provision_id key.
Proof.
 intros c e F C HC Unique Clean Domain key;split.
 - intro Member;apply in_map_iff in Member as[k[Eq Hit]];subst key.
   pose proof(all_keys_canonical e Clean)as Canonical;rewrite Forall_forall in Canonical.
   split.
   + eapply key_provides;[exact HC|exact(Canonical k Hit)|apply(all_keys_available e Unique k);exact Hit].
   + destruct k;(split;[exact I|reflexivity]).
 - intros[Provided[NotType Identity]];destruct key as[t|v|f|r mid theta|u];[contradiction| | | |].
   + apply in_map_iff;exists(KyVal v);split;[reflexivity|].
     apply(all_keys_available e Unique);unfold LambdaKeyAvailable.
     cbn[A.provides]in Provided;rewrite(cr_vals _ _ _ _ _ _ _ HC)in Provided.
     destruct(nget(en_vals e)v);[discriminate|contradiction].
   + apply in_map_iff;exists(KyFn f);split;[reflexivity|].
     apply(all_keys_available e Unique);unfold LambdaKeyAvailable.
     cbn[A.provides]in Provided;rewrite(cr_fns _ _ _ _ _ _ _ HC)in Provided.
     destruct(nget(en_fns e)f);[discriminate|contradiction].
   + assert(Empty:theta=[])by(injection Identity;auto);subst theta.
     cbn[A.provides]in Provided;destruct(A.mlook r mid(A.c_mths C))as[sg|]eqn:Lookup;[|contradiction].
     destruct(Domain r mid sg Lookup)as[raw[Good[Stable Translation]]].
     apply in_map_iff;exists(KyMth raw mid);split;[cbn[key_tr];rewrite Translation;reflexivity|].
     apply(all_keys_available e Unique);unfold LambdaKeyAvailable.
     pose proof(cr_mths _ _ _ _ _ _ _ HC raw mid ltac:(now rewrite Stable))as Read.
     cbn beta in Read;rewrite Stable,Translation,Lookup in Read;destruct(get_mth e raw mid);[discriminate|discriminate Read].
   + apply in_map_iff;exists(KyFun u);split;[reflexivity|].
     apply(all_keys_available e Unique);unfold LambdaKeyAvailable.
     cbn[A.provides]in Provided;pose proof(cr_funspresent _ _ _ _ _ _ _ HC u)as Present.
     destruct(A.nlook u(A.c_funs C));[|contradiction].
     destruct(nget(en_funs e)u);[discriminate|discriminate Present].
Qed.
End Interface.
