(* Used semantic marker values are genuine subtypes of the raw receiver. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab CoreMoss BodyTr.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Inductive MatchTyWithin:ty->ty->Prop:=
|MTWHere:forall t,MatchTyWithin t t
|MTWNom:forall t n xs q,In q xs->MatchTyWithin t(snd q)->MatchTyWithin t(TNom n xs)
|MTWUnion:forall t ms m,In m ms->MatchTyWithin t m->MatchTyWithin t(TUnion ms).
Fixpoint match_raw_subtypes(t:ety):list ety:=t::match t with
 |YNom _ xs=>flat_map(fun q=>match_raw_subtypes(snd q))xs
 |YUnion ms=>flat_map match_raw_subtypes ms
 |YRec fs=>flat_map(fun q=>match_raw_subtypes(snd q))fs
 |_=>[]end.
Lemma match_subst_range_within:forall pat sub g value,
 In g(A.gens pat)->lookup_subst g sub=Some value->MatchTyWithin value(ExecutionModel.app_subst sub pat).
Proof.
 intro pat;induction pat as[|id|n xs IH|ms IH]using ty_ind';intros sub g value Gen Lookup.
 - contradiction.
 - cbn[A.gens]in Gen;destruct Gen as[<-|[]];cbn[ExecutionModel.app_subst];rewrite Lookup;constructor.
 - change(In g(flat_map(fun q=>A.gens(snd q))xs))in Gen.
   apply in_flat_map in Gen as(q&Hq&Gen);rewrite Forall_forall in IH.
   eapply MTWNom with(q:=(fst q,ExecutionModel.app_subst sub(snd q))).
   + apply in_map_iff;exists q;split;[reflexivity|exact Hq].
   + exact(IH q Hq sub g value Gen Lookup).
 - change(In g(flat_map A.gens ms))in Gen;apply in_flat_map in Gen as(t&Ht&Gen).
   rewrite Forall_forall in IH;eapply MTWUnion with(m:=ExecutionModel.app_subst sub t).
   + apply in_map_iff;exists t;split;[reflexivity|exact Ht].
   + exact(IH t Ht sub g value Gen Lookup).
Qed.
Lemma match_within_generations:forall sub parent,
 MatchTyWithin sub parent->forall g,In g(A.gens sub)->In g(A.gens parent).
Proof.
 intros sub parent H;induction H;intros g Gen;[exact Gen| |].
 - change(In g(flat_map(fun q=>A.gens(snd q))xs));apply in_flat_map;exists q;split;[exact H|apply IHMatchTyWithin;exact Gen].
 - change(In g(flat_map A.gens ms));apply in_flat_map;exists m;split;[exact H|apply IHMatchTyWithin;exact Gen].
Qed.
Theorem match_within_raw_image:forall Fl receiver value,
 MatchTyWithin value(tr Fl receiver)->
 exists raw,In raw(match_raw_subtypes receiver)/\tr Fl raw=value.
Proof.
 intros Fl receiver;induction receiver as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intros value Within.
 - inversion Within;subst;exists YUnit;split;[left;reflexivity|reflexivity].
 - inversion Within;subst.
   + exists YNever;split;[left;reflexivity|reflexivity].
   + contradiction.
 - inversion Within;subst;exists(YAbs g);split;[left;reflexivity|reflexivity].
 - inversion Within;subst.
   + exists(YNom n xs);split;[left;reflexivity|reflexivity].
   + match goal with Hq:In ?q(map _ xs)|-_=>apply in_map_iff in Hq as(p&Eq&Hp);subst q end.
     rewrite Forall_forall in IH;cbn[snd]in *.
     match goal with H:MatchTyWithin value(tr Fl(snd p))|-_=>
       destruct(IH p Hp value H)as(raw&Hraw&Eq)end.
     exists raw;split;[right;apply in_flat_map;exists p;split;assumption|exact Eq].
 - inversion Within;subst.
   + exists(YUnion ms);split;[left;reflexivity|reflexivity].
   + match goal with Hm:In ?m(map _ ms)|-_=>apply in_map_iff in Hm as(t&Eq&Ht);subst m end.
     rewrite Forall_forall in IH;cbn[snd]in *.
     match goal with H:MatchTyWithin value(tr Fl t)|-_=>
       destruct(IH t Ht value H)as(raw&Hraw&Eq)end.
     exists raw;split;[right;apply in_flat_map;exists t;split;assumption|exact Eq].
 - inversion Within;subst.
   + exists(YRec fs);split;[left;reflexivity|reflexivity].
   + match goal with Hq:In ?q(map _ fs)|-_=>apply in_map_iff in Hq as(p&Eq&Hp);subst q end.
     rewrite Forall_forall in IH;cbn[snd]in *.
     match goal with H:MatchTyWithin value(tr Fl(snd p))|-_=>
       destruct(IH p Hp value H)as(raw&Hraw&Eq)end.
     exists raw;split;[right;apply in_flat_map;exists p;split;assumption|exact Eq].
Qed.
Definition match_decode(Fl:string->nat)(receiver:ety)(value:ty):option ety:=
 find(fun raw=>ty_eqb(tr Fl raw)value)(match_raw_subtypes receiver).
Theorem match_decode_image:forall Fl receiver value,
 MatchTyWithin value(tr Fl receiver)->
 exists raw,match_decode Fl receiver value=Some raw/\tr Fl raw=value/\In raw(match_raw_subtypes receiver).
Proof.
 intros Fl receiver value Within.
 destruct(match_within_raw_image Fl receiver value Within)as(witness&InWitness&Eq).
 unfold match_decode;destruct(find(fun raw=>ty_eqb(tr Fl raw)value)(match_raw_subtypes receiver))as[raw|]eqn:Found.
 - apply find_some in Found as[Hin Same];exists raw;split;[reflexivity|];split;[apply ty_eqb_eq;exact Same|exact Hin].
 - pose proof(find_none _ _ Found witness InWitness)as Bad.
   cbn beta in Bad;rewrite Eq in Bad;assert(ty_eqb value value=true)by(apply ty_eqb_eq;reflexivity);congruence.
Qed.
From NM Require Import PMatchComplete TypeReadingInj.
Lemma match_raw_subtypes_ok:forall receiver,
 ety_ok receiver=true->Forall(fun raw=>ety_ok raw=true)(match_raw_subtypes receiver).
Proof.
 intro receiver;induction receiver as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intro Clean.
 all:(constructor;[exact Clean|]).
 all:try constructor.
 - apply andb_true_iff in Clean as[Head Parts];apply forallb_Forall in Parts.
   apply Forall_flat_map;rewrite Forall_forall in IH,Parts|-*;intros q Hq;exact(IH q Hq(Parts q Hq)).
 - assert(Parts:Forall(fun raw=>ety_ok raw=true)ms).
   {destruct ms as[|m ms];[discriminate Clean|].
    cbn[ety_ok]in Clean;apply forallb_Forall in Clean.
    eapply Forall_impl;[|exact Clean];intros raw H;destruct raw;try discriminate H;exact H. }
   apply Forall_flat_map;rewrite Forall_forall in IH,Parts|-*;intros t Ht;exact(IH t Ht(Parts t Ht)).
 - apply forallb_Forall in Clean;apply Forall_flat_map.
   rewrite Forall_forall in IH,Clean|-*;intros q Hq;exact(IH q Hq(Clean q Hq)).
Qed.
Lemma match_decode_ok:forall Fl receiver value,
 ety_ok receiver=true->ety_ok(opt_default YUnit(match_decode Fl receiver value))=true.
Proof.
 intros Fl receiver value Clean;unfold match_decode;destruct(find(fun raw=>ty_eqb(tr Fl raw)value)(match_raw_subtypes receiver))as[raw|]eqn:Found;
 [|reflexivity].
 apply find_some in Found as[Hin Same];cbn[opt_default].
 pose proof(match_raw_subtypes_ok receiver Clean)as All;rewrite Forall_forall in All;exact(All raw Hin).
Qed.
Definition match_decoded_theta(Fl:string->nat)(receiver:ety)(table:list(nat*ty))(p:nat):ety:=
 match A.nlook p table with Some value=>opt_default YUnit(match_decode Fl receiver value)|None=>YUnit end.
Lemma match_decoded_theta_ok:forall Fl receiver table,
 ety_ok receiver=true->forall p,ety_ok(match_decoded_theta Fl receiver table p)=true.
Proof. intros;unfold match_decoded_theta;destruct(A.nlook p table);[apply match_decode_ok;exact H|reflexivity]. Qed.
Lemma match_forallb_true:forall X(f:X->bool)xs,
 Forall(fun x=>f x=true)xs->forallb f xs=true.
Proof. intros X f xs H;rewrite forallb_forall;rewrite Forall_forall in H;exact H. Qed.
Lemma match_mark_type_ok:forall pat back theta,
 ety_ok pat=true->(forall p,ety_ok(theta p)=true)->ety_ok(mark_type back theta pat)=true.
Proof.
 intro pat;induction pat as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intros back theta Clean Values;
 cbn[mark_type ety_ok];try reflexivity.
 - destruct(nassoc g back);[apply Values|reflexivity].
 - cbn[ety_ok]in Clean;apply andb_true_iff in Clean as[Head Parts];rewrite Head;cbn[andb].
   apply forallb_Forall in Parts;apply match_forallb_true;apply Forall_map.
   rewrite Forall_forall in IH,Parts|-*;intros q Hq;cbn[snd];exact(IH q Hq back theta(Parts q Hq)Values).
 - destruct ms as[|m ms];[discriminate Clean|];cbn[map ety_ok]in *.
   apply forallb_Forall in Clean;apply match_forallb_true.
   change(Forall(fun raw=>match raw with YNom _ _=>ety_ok raw|_=>false end=true)(map(mark_type back theta)(m::ms))).
   apply Forall_map;rewrite Forall_forall in IH,Clean|-*;intros raw Hin.
   pose proof(Clean raw Hin)as Cr;destruct raw;try discriminate Cr.
   exact(IH(YNom n l)Hin back theta Cr Values).
 - apply forallb_Forall in Clean;apply match_forallb_true;apply Forall_map.
   rewrite Forall_forall in IH,Clean|-*;intros q Hq;cbn[snd];exact(IH q Hq back theta(Clean q Hq)Values).
Qed.
Lemma match_mark_type_translation:forall Fl pat back theta sub,
 (forall g,In g(ety_gens pat)->
   match nassoc g back with Some p=>lookup_subst g sub=Some(tr Fl(theta p))|None=>lookup_subst g sub=None end)->
 ExecutionModel.app_subst sub(tr Fl pat)=tr Fl(mark_type back theta pat).
Proof.
 intros Fl pat;induction pat as[| |g|n xs IH|ms IH|fs IH]using ety_ind';intros back theta sub Read;
 cbn[tr mark_type ExecutionModel.app_subst];try reflexivity.
 - pose proof(Read g(or_introl eq_refl))as R;destruct(nassoc g back);rewrite R;reflexivity.
 - f_equal;revert Read;induction IH as[|[k t]xs HT Hxs IHL];intro Read;[reflexivity|].
   cbn[map fst snd];f_equal.
   + f_equal;apply HT;intros g Gen;apply Read;cbn[ety_gens fst snd];apply in_app_iff;left;exact Gen.
   + apply IHL;intros g Gen;apply Read;cbn[ety_gens fst snd];apply in_app_iff;right;exact Gen.
 - f_equal;revert Read;induction IH as[|t ms HT Hms IHL];intro Read;[reflexivity|].
   cbn[map];f_equal.
   + apply HT;intros g Gen;apply Read;cbn[ety_gens];apply in_app_iff;left;exact Gen.
   + apply IHL;intros g Gen;apply Read;cbn[ety_gens];apply in_app_iff;right;exact Gen.
 - f_equal;revert Read;induction IH as[|[k t]fs HT Hfs IHL];intro Read;[reflexivity|].
   cbn[map fst snd];f_equal.
   + f_equal;apply HT;intros g Gen;apply Read;cbn[ety_gens fst snd];apply in_app_iff;left;exact Gen.
   + apply IHL;intros g Gen;apply Read;cbn[ety_gens fst snd];apply in_app_iff;right;exact Gen.
Qed.
Theorem match_semantic_raw_solution:forall Fl pat receiver back table sub,
 (forall x y,Fl x=Fl y->x=y)->ety_ok pat=true->ety_ok receiver=true->
 (forall g p,nassoc g back=Some p->lookup_subst g sub=
   Some(match A.nlook p table with Some t=>t|None=>TAbs g end))->
 (forall g,nassoc g back=None->lookup_subst g sub=None)->
 (forall g p,nassoc g back=Some p->~In g(ety_gens receiver))->
 ExecutionModel.app_subst sub(tr Fl pat)=tr Fl receiver->
 mark_type back(match_decoded_theta Fl receiver table)pat=receiver.
Proof.
 intros Fl pat receiver back table sub Finj PatOk ReceiverOk Marked Other Avoid Eq.
 assert(Read:forall g,In g(ety_gens pat)->
   match nassoc g back with Some p=>lookup_subst g sub=Some(tr Fl(match_decoded_theta Fl receiver table p))
   |None=>lookup_subst g sub=None end).
 {intros g Gen;destruct(nassoc g back)as[p|]eqn:Back;[|apply Other;exact Back].
  pose proof(Marked g p Back)as Lookup;destruct(A.nlook p table)as[value|]eqn:Value.
  - assert(Within:MatchTyWithin value(tr Fl receiver)).
    {rewrite <-Eq;eapply match_subst_range_within;[rewrite gens_tr;exact Gen|exact Lookup]. }
    destruct(match_decode_image Fl receiver value Within)as(raw&Decode&Image&Hin).
    unfold match_decoded_theta;rewrite Value,Decode;cbn[opt_default];rewrite Image;exact Lookup.
  - exfalso;apply(Avoid g p Back).
    rewrite <-(gens_tr Fl receiver),<-Eq.
    eapply match_within_generations;[eapply match_subst_range_within;[rewrite gens_tr;exact Gen|exact Lookup]|].
    left;reflexivity. }
 pose proof(match_mark_type_translation Fl pat back(match_decoded_theta Fl receiver table)sub Read)as Translate.
 eapply tr_inj;[exact Finj| |exact ReceiverOk|].
 - apply match_mark_type_ok;[exact PatOk|apply match_decoded_theta_ok;exact ReceiverOk].
 - rewrite <-Translate;exact Eq.
Qed.
