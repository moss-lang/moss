(* The first forwarder walk installs requirement names at their own meanings. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodyTy CanonTypes InfoCache FnSnapshotAlign BodyTr.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma seed_lookup_set:forall V n k(v:V)l,
 nassoc n(aset Nat.eqb k v l)=if Nat.eqb n k then Some v else nassoc n l.
Proof.
 intros V n k v l;induction l as[|[j x]rest IH];cbn.
 - destruct(Nat.eqb n k);reflexivity.
 - destruct(Nat.eqb_spec k j)as[<-|Ne];cbn.
   + destruct(Nat.eqb n k);reflexivity.
   + destruct(Nat.eqb_spec n j)as[->|Ne2];[|exact IH].
     destruct(Nat.eqb_spec j k);[congruence|reflexivity].
Qed.
Record PrototypeSeed(e:env)(keys:list nat):Prop:={
 seed_identity:Forall(fun p=>fst(snd p)=YAbs(fst p))(en_tn e);
 seed_domain:forall x,(exists y,tn_get e x=Some y)<->In x keys;
 seed_uf:en_uf e=[];
 seed_fns:en_fns e=NE;
 seed_apps:en_apps e=[]
}.
Lemma seed_tn_read:forall e keys x y,PrototypeSeed e keys->tn_get e x=Some y->y=YAbs x.
Proof.
 intros e keys x y H E;unfold tn_get in E.
 destruct(nassoc x(en_tn e))as[[v stamp]|]eqn:Look;[|discriminate].
 injection E as <-.
 pose proof(seed_identity _ _ H)as F.
 induction(en_tn e)as[|[k [t st]]rest IH];[discriminate|].
 inversion F;subst;cbn[nassoc assoc fst snd]in Look.
 destruct(Nat.eqb_spec x k)as[->|Ne];[injection Look as <-;assumption|].
 apply IH;assumption.
Qed.
Lemma seed_empty:PrototypeSeed empty_env[].
Proof. constructor;try reflexivity;[constructor|]. intro x;cbn[tn_get empty_env en_tn nassoc assoc];split;[intros[y E];discriminate|contradiction]. Qed.
Lemma seed_existing:forall e keys x y,PrototypeSeed e keys->tn_get e x=Some y->PrototypeSeed e(x::keys).
Proof.
 intros e keys x y H E;destruct H as[Self Domain Uf Fns Apps];constructor;try assumption.
 intro k;rewrite Domain;split;[intro K;right;exact K|intros[->|K];[apply Domain;exists y;exact E|exact K]].
Qed.
Lemma seed_set_type:forall e keys x stamp,PrototypeSeed e keys->
 PrototypeSeed(set_tn e x(YAbs x)stamp)(x::keys).
Proof.
 intros e keys x stamp H;destruct H as[Self Domain Uf Fns Apps];constructor;cbn[set_tn en_tn en_uf en_fns en_apps];try assumption.
 - induction(en_tn e)as[|[k [v st]]rest IH];cbn[aset];[constructor;[reflexivity|constructor]|].
   inversion Self;subst;destruct(Nat.eqb_spec x k)as[->|Ne];constructor;cbn[fst snd];try assumption;try reflexivity;apply IH;assumption.
 - intro k;unfold tn_get;cbn[set_tn en_tn];rewrite seed_lookup_set.
   destruct(Nat.eqb_spec k x)as[->|Ne];cbn[option_map fst].
   + split;[intro;left;reflexivity|intro;exists(YAbs x);reflexivity].
   + change((exists y,tn_get e k=Some y)<->In k(x::keys));rewrite Domain.
     split;[intro K;right;exact K|intros[Eq|K];[congruence|exact K]].
Qed.
Theorem add_requirement_seed:forall fuel e mi g needs s e' needs' final keys,
 gs_kind g=KType->PrototypeSeed e keys->
 add_symbol_item fuel e mi g[]needs s=Good((e',needs'),final)->
 PrototypeSeed e'(gs_id g::keys)/\needs'=needs.
Proof.
 intros[|fuel]e mi g needs s e' needs' final keys Kind Seed Run;[discriminate|].
 cbn[add_symbol_item]in Run;apply bind_inv in Run as[u[s1[Check Run]]].
 rewrite Kind in Run;cbn[app]in Run.
 destruct(tn_get e(gs_id g))as[y|]eqn:Present.
 - injection Run as <- <- _;split;[eapply seed_existing;eassumption|reflexivity].
 - apply bind_inv in Run as[stamp[s2[Fresh Run]]];injection Run as <- <- _.
   split;[apply seed_set_type;exact Seed|reflexivity].
Qed.
Lemma seed_keys_equiv:forall e keys keys',PrototypeSeed e keys->
 (forall x,In x keys<->In x keys')->PrototypeSeed e keys'.
Proof. intros e keys keys' H Eq;destruct H;constructor;try assumption;intro x;rewrite seed_domain0;apply Eq. Qed.

Section Walk.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Lemma forwarder_requirements_seed:forall cf mi rs keys e needs s e' needs' final,
 PrototypeSeed e keys->Inv s->st_w s=w->
 (forall r,In r rs->exists rg,nget(w_syms w)r=Some rg/\gs_kind rg=KType)->
 mfold(fun acc r=>let! rg:=sym r in add_symbol_item cf(fst acc)mi rg[](snd acc))
 (e,needs)rs s=Good((e',needs'),final)->
 PrototypeSeed e'(rs++keys)/\needs'=needs.
Proof.
 intros cf mi rs;induction rs as[|r rs IH];intros keys e needs s e' needs' final Seed Hi Hw Types Run.
 - cbn[mfold]in Run;injection Run as <- <- _;split;[exact Seed|reflexivity].
 - cbn[mfold]in Run;apply bind_inv in Run as[[e1 n1][s1[Head Tail]]].
   apply bind_inv in Head as[rg[s0[Symbol Add]]].
   destruct(Types r(or_introl eq_refl))as[actual[Loaded Kind]].
   destruct(sym_loader_st w b Hload _ _ _ _ _ Hi Hw Loaded Symbol)as[-> ->].
   destruct(ok_add_symbol_item cf e mi actual[]needs _ _ _ Hi Add)as[I1 F1].
   destruct(add_requirement_seed cf e mi actual needs s e1 n1 s1 keys Kind Seed Add)as[Seed1 ->].
   rewrite(load_world_ids b w Hload r actual Loaded)in Seed1.
   destruct(IH(r::keys)e1 needs s1 e' needs' final Seed1 I1
    ltac:(rewrite(fr_w _ _ F1);exact Hw)(fun q Inq=>Types q(or_intror Inq))Tail)as[Final Eq].
   split;[|exact Eq]. eapply seed_keys_equiv;[exact Final|].
   intro x;rewrite !in_app_iff;cbn[In];tauto.
Qed.
End Walk.

Lemma seed_read_exact:forall e keys x,PrototypeSeed e keys->
 tn_get e x=if existsb(Nat.eqb x)keys then Some(YAbs x)else None.
Proof.
 intros e keys x Seed;destruct(existsb(Nat.eqb x)keys)eqn:Member.
 - apply existsb_exists in Member as[k[InK Eq]];apply Nat.eqb_eq in Eq;subst k.
   destruct(proj2(seed_domain _ _ Seed x)InK)as[y Read].
   rewrite Read;f_equal;exact(seed_tn_read _ _ _ _ Seed Read).
 - destruct(tn_get e x)as[y|]eqn:Read;[|reflexivity].
   exfalso;assert(InX:In x keys)by(apply(seed_domain _ _ Seed x);exists y;exact Read).
   assert(existsb(Nat.eqb x)keys=true)by(apply existsb_exists;exists x;split;[exact InX|apply Nat.eqb_refl]).
   congruence.
Qed.
Lemma seed_tmrel:forall Fl e keys,PrototypeSeed e keys->
 TmRel Fl(tymap e)(requirement_snapshot keys)e.
Proof.
 intros Fl e keys Seed;apply tmrel_tymap.
 - intros k y Read;rewrite(seed_tn_read _ _ _ _ Seed Read);constructor.
 - intro k;rewrite requirement_snapshot_lookup,(seed_read_exact _ _ k Seed).
   destruct(existsb(Nat.eqb k)keys);reflexivity.
Qed.
