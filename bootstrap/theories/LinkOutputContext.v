(* Freezing input meanings before installing roles preserves their semantic context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodySoundCore BodyTr BodyTy Merge CanonTypes NominalSupport.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma link_output_frozen_type:forall e t,
 tn_get(target_input_types e)t=option_map(canon e)(tn_get e t).
Proof.
 intros e t;unfold tn_get,target_input_types;cbn[en_tn].
 induction(en_tn e)as[|[key[raw stamp]]rows IH];[reflexivity|].
 cbn[map nassoc assoc fst snd];destruct(Nat.eqb t key);[reflexivity|exact IH].
Qed.
Lemma link_output_frozen_support:forall W e,
 EnvSupport W e->EnvSupport W(target_input_types e).
Proof.
 intros W e Sup;constructor.
 - intros k p Hit;apply in_map_iff in Hit as[[key[raw stamp]][Eq Hit]].
   injection Eq as <- <-;cbn[fst];apply canon_support;[exact(es_uf Sup)|exact(es_tn Sup key(raw,stamp)Hit)].
 - exact(es_uf Sup).
 - exact(es_vals Sup).
 - exact(es_fns Sup).
 - exact(es_mths Sup).
 - exact(es_locals Sup).
Qed.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Lemma link_output_frozen_context:forall c e F C,
 CxRel w R Fl c e F C->CxRel w R Fl c(target_input_types e)F C.
Proof using w R Fl.
 intros c e F C HC.
 pose proof(cr_ty _ _ _ _ _ _ _ HC)as Types.
 pose proof(cr_ty_wf _ _ _ _ _ _ _ HC)as Wf.
 pose proof(cr_uf _ _ _ _ _ _ _ HC)as[T U].
 pose proof(cr_support _ _ _ _ _ _ _ HC)as Sup.
 destruct HC;constructor;try assumption;try exact(conj T U).
 - intro t;rewrite link_output_frozen_type.
   change(A.nlook t(A.c_ty C)=option_map(fun raw=>tr Fl(canon e raw))(option_map(canon e)(tn_get e t))).
   rewrite Types.
   destruct(tn_get e t)as[raw|];cbn[option_map];[rewrite canon_idem by exact T|];reflexivity.
 - intros t raw Lookup;rewrite link_output_frozen_type in Lookup.
   destruct(tn_get e t)as[old|]eqn:Old;[injection Lookup as <-|discriminate].
   apply canon_wf;[exact U|exact(Wf t old Old)].
 - apply link_output_frozen_support;exact Sup.
Qed.
End Context.
