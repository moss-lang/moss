(* Inferred matching values are canonical subtypes of the canonical receiver. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabSound BodyTr Merge CanonTypes PMatchExec FamilySelectTrace.
Import ListNotations.
Lemma pmatch_canonical_parts:forall uf,PmatchParts(Canon uf).
Proof.
 intro uf;split;[|split].
 - intros n xs Root;apply Forall_forall;intros q Hin h Gen;apply Root,gens_nom;exists q;split;assumption.
 - intros ms Root;apply Forall_forall;intros t Hin h Gen;apply Root,gens_union;exists t;split;assumption.
 - intros fs Root;apply Forall_forall;intros q Hin h Gen;apply Root,gens_rec;exists q;split;assumption.
Qed.
Lemma pmatch_types_canonical:forall uf fuel back pat r acc,
 Canon uf r->Forall(fun q=>Canon uf(snd q))acc->Forall(fun q=>Canon uf(snd q))(pmatch fuel back pat r acc).
Proof.
 intros uf fuel back pat r acc Root Parts.
 eapply(pmatch_invariant(Canon uf)(fun xs=>Forall(fun q=>Canon uf(snd q))xs)back);
 [apply pmatch_canonical_parts| |exact Root|exact Parts].
 intros old p t Before Ct From Fresh;apply Forall_app;split;[exact Before|constructor;[exact Ct|constructor]].
Qed.
Definition CanonicalMatch(e:env)(q:family_candidate):Prop:=Forall(fun p=>Canon(en_uf e)(snd p))(snd q).
Lemma family_search_step_canonical:forall mi e r mid found u s found' sf,
 family_search_step mi e r mid found u s=Good(found',sf)->Canon(en_uf e)r->
 FamilyFound(CanonicalMatch e)found->FamilyFound(CanonicalMatch e)found'.
Proof.
 intros mi e r mid found u s found' sf Run Root Old;unfold family_search_step in Run.
 apply bind_inv in Run as(fg&sg&Sym&Run);apply bind_inv in Run as(fam&sc&Class&Run).
 destruct(negb fam);[injection Run as <- <-;exact Old|].
 apply bind_inv in Run as(ps&sp&Params&Run);apply bind_inv in Run as(marks&sm&Markers&Run).
 apply bind_inv in Run as(em&se&Env&Run);destruct(functor_parts fg)as[[args results]body].
 cbn beta iota zeta in Run;apply bind_inv in Run as(fuel&sn&Fuel&Run).
 apply bind_inv in Run as(leaves&sl&Leaves&Run).
 eapply family_candidates_preserve;[exact Run|exact Old|].
 intro pat;unfold CanonicalMatch;cbn[snd];unfold pmatch_full.
 apply pmatch_types_canonical;[exact Root|constructor].
Qed.
Lemma family_search_canonical:forall mi e r mid ids found s found' sf,
 mfold(family_search_step mi e r mid)found ids s=Good(found',sf)->Canon(en_uf e)r->
 FamilyFound(CanonicalMatch e)found->FamilyFound(CanonicalMatch e)found'.
Proof.
 intros mi e r mid ids;induction ids as[|u ids IH];intros found s found' sf Run Root Old;
 [injection Run as <- <-;exact Old|].
 cbn[mfold]in Run;apply bind_inv in Run as(next&sn&Step&Tail).
 eapply IH;[exact Tail|exact Root|eapply family_search_step_canonical;eassumption].
Qed.
Theorem family_member_matching_canonical:forall mi e r mid s g matching sf,
 family_member mi e r mid s=Good(Some(g,matching),sf)->Tri(en_uf e)->canon e r=r->
 Forall(fun q=>canon e(snd q)=snd q)matching.
Proof.
 intros mi e r mid s g matching sf Run T Fixed.
 pose proof(family_member_search _ _ _ _ _ _ _ _ Run)as Search;unfold family_search in Search.
 pose proof(canon_tri e r T)as Root;rewrite Fixed in Root.
 pose proof(family_search_canonical _ _ _ _ _ _ _ _ _ Search Root(Forall_nil _))as All.
 inversion All as[|q rest Head Tail];subst;unfold CanonicalMatch in Head;cbn[snd]in Head.
 eapply Forall_impl;[|exact Head];intros q Cq;unfold canon;apply canon_fix;exact Cq.
Qed.
