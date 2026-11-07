(* Derive a declarative result read from the actual fresh output-view visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 CtxMerge CanonTypes GeneralProvideResult NestingTypesExec ApplyFreshExec LinkOutputRoles.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma link_output_role_total:forall (roles:list(nat*nat)) t,
 In t(map fst roles)->nassoc t roles<>None.
Proof.
 intros roles;induction roles as[|[key value]roles IH];intros t Hit;[contradiction|].
 cbn[nassoc assoc fst]in *;destruct(Nat.eqb t key)eqn:Eq;[discriminate|].
 apply IH;destruct Hit as[<-|Hit];[rewrite Nat.eqb_refl in Eq;discriminate|exact Hit].
Qed.
Lemma ok_link_output_allocate:forall fg ts,ok(link_output_allocate fg ts).
Proof. intros;unfold link_output_allocate;okt. Qed.
Section Provide.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Load:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Theorem fresh_target_output_provide:forall c e F C fg name names args res s promised keys roles final,
 nget(w_syms w)(gs_id fg)=Some fg->gs_decl fg=ADFunctor name names args res None->
 fresh_target_output_view e fg s=Good((promised,keys,roles),final)->
 Inv s->st_w s=w->Later w P R Fl final->CxRel w R Fl c(target_input_types e)F C->
 (forall t,A.Mentions W(gs_id fg)t->tn_get(target_input_types e)t<>None)->
 exists Cr,A.ProvideResult W(gs_id fg)C(fun t=>TAbs(generation_of roles t))Cr(map(key_tr Fl)keys)/\
 CxRel w R Fl c promised F Cr.
Proof using w P R Fl b Load Finj.
 intros c e F C fg name names args res s promised keys roles final Loaded Decl Run Hi Hw Last HC Mentioned.
 destruct(fresh_target_output_phases e fg s promised keys roles final Run)
 as(ts&typed&allocated&provided&Types&Allocate&Provide&Same&Keys&Distinct&Bounds);subst provided.
 destruct(result_types_rel w R Fl b Load fg name names args res None s ts typed Loaded Decl Types Hi Hw)
 as[-> [NoDup Domain]].
 destruct(ok_link_output_allocate fg ts _ _ _ Hi Allocate)as[Ia Fa].
 assert(Wa:st_w allocated=w)by(rewrite(fr_w _ _ Fa);exact Hw).
 eapply(general_provide_result w (P:=P) R Fl b Load Finj c(target_input_types e)F C fg name names args res roles allocated promised keys final);
 try eassumption.
 intros t Output;apply link_output_role_total;rewrite Keys;apply Domain;exact Output.
Qed.
End Provide.
