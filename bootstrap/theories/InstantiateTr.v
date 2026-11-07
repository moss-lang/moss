(* Complete declaration rereading agrees with semantic caller substitution. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySoundCore BodyTr Merge CanonTypes KeyTranslation.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma lookup_nlook : forall g s, Audit.lookup_subst g s=A.nlook g s.
Proof. intros g s;induction s as[|[k v]s IH];cbn;[reflexivity|]. destruct(Nat.eqb g k);auto. Qed.
Lemma instantiate_normal : forall e t,Tri(en_uf e)->canon e(instantiate e t)=instantiate e t.
Proof.
 intros e t T. induction t as[| |g|n xs IH|ms IH|fs IH]using ety_ind';cbn[instantiate].
 - reflexivity.
 - reflexivity.
 - destruct(tn_get e g);apply canon_idem;exact T.
 - change(YNom n(map(fun p=>(fst p,canon e(snd p)))(map(fun p=>(fst p,instantiate e(snd p)))xs))=YNom n(map(fun p=>(fst p,instantiate e(snd p)))xs)).
   rewrite map_map. f_equal. apply map_ext_in. intros[k v]Hin;cbn.
   rewrite Forall_forall in IH. specialize(IH (k,v) Hin). cbn in IH. now rewrite IH.
 - change(YUnion(map(canon e)(map(instantiate e)ms))=YUnion(map(instantiate e)ms)).
   rewrite map_map. f_equal. apply map_ext_in. intros v Hin.
   rewrite Forall_forall in IH. exact(IH _ Hin).
 - change(YRec(map(fun p=>(fst p,canon e(snd p)))(map(fun p=>(fst p,instantiate e(snd p)))fs))=YRec(map(fun p=>(fst p,instantiate e(snd p)))fs)).
   rewrite map_map. f_equal. apply map_ext_in. intros[k v]Hin;cbn.
   rewrite Forall_forall in IH. specialize(IH (k,v) Hin). cbn in IH. now rewrite IH.
Qed.
Lemma instantiate_ok : forall e,
 UfOK(en_uf e)->(forall g y,tn_get e g=Some y->ety_ok y=true)->
 forall t,ety_ok t=true->ety_ok(instantiate e t)=true.
Proof.
 intros e U T t. induction t as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intro Ot;try reflexivity.
 - cbn[instantiate]. destruct(tn_get e g)as[y|]eqn:Eg;apply canon_ok;auto;try reflexivity;eauto.
 - cbn[instantiate ety_ok]in *. apply andb_true_iff in Ot as[On Ox].
   apply andb_true_iff;split;[exact On|]. apply forallb_forall;intros p Hp.
   apply in_map_iff in Hp as[q[<- Hq]];cbn[snd]. rewrite Forall_forall in IH.
   apply IH;[exact Hq|exact(proj1(forallb_forall _ _)Ox q Hq)].
 - destruct ms as[|m ms];[discriminate Ot|]. cbn[instantiate ety_ok]in Ot|-*.
   apply forallb_forall;intros q Hq. apply in_map_iff in Hq as[v[<- Hv]].
   pose proof(proj1(forallb_forall _ _)Ot v Hv)as Ov.
   rewrite Forall_forall in IH. destruct v;try discriminate Ov.
   exact(IH _ Hv Ov).
 - cbn[instantiate ety_ok]in *. apply forallb_forall;intros p Hp.
   apply in_map_iff in Hp as[q[<- Hq]];cbn[snd]. rewrite Forall_forall in IH.
   apply IH;[exact Hq|exact(proj1(forallb_forall _ _)Ot q Hq)].
Qed.
Section Read.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma instantiate_canonical : forall c e F C,
 CxRel w R Fl c e F C ->forall t,
 tr Fl(canon e(instantiate e t))=
 A.canon C(Audit.app_subst(A.c_ty C)(tr Fl t)).
Proof.
 intros c e F C H t.
 unfold A.canon. rewrite(cr_sub _ _ _ _ _ _ _ H).
 rewrite <-(tr_canon Fl e (instantiate e t)(proj1(cr_uf _ _ _ _ _ _ _ H))).
 induction t as[| |g|n xs IH|ms IH|fs IH]using ety_ind';cbn[instantiate tr Audit.app_subst];try reflexivity.
 - rewrite lookup_nlook.
   rewrite(cr_ty _ _ _ _ _ _ _ H g). destruct(tn_get e g)as[y|]eqn:E;cbn[option_map].
   + reflexivity.
   + rewrite(tr_canon Fl e (canon e(YAbs g))(proj1(cr_uf _ _ _ _ _ _ _ H))).
     rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ H)).
     symmetry;change(Audit.app_subst(suf Fl(en_uf e))(tr Fl(YAbs g))=tr Fl(canon e(YAbs g)));apply tr_canon;exact(proj1(cr_uf _ _ _ _ _ _ _ H)).
 - f_equal. induction IH as[|[k v]xs Hv Hr IHr];cbn in *;[reflexivity|].
   rewrite Hv,IHr;reflexivity.
 - f_equal. induction IH as[|v xs Hv Hr IHr];cbn in *;[reflexivity|].
   rewrite Hv,IHr;reflexivity.
 - f_equal. induction IH as[|[k v]xs Hv Hr IHr];cbn in *;[reflexivity|].
   rewrite Hv,IHr;reflexivity.
Qed.
Definition site_key(e:env)(k:key):key:=match k with
 |KyMth r m=>KyMth(canon e(instantiate e r))m|_=>k end.
Lemma key_tr_site : forall c e F C k,
 CxRel w R Fl c e F C ->
 key_tr Fl(site_key e k)=A.at_site C(key_tr Fl k).
Proof.
 intros c e F C k H. destruct k;cbn[site_key key_tr A.at_site];try reflexivity.
 assert(Map:map(fun q:nat*ty=>(fst q,snd q))(A.c_ty C)=A.c_ty C).
 { induction(A.c_ty C)as[|[a b]l IH];cbn;congruence. }
 rewrite Map, (instantiate_canonical c e F C H e0). reflexivity.
Qed.
End Read.
