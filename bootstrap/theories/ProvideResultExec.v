(* Output keys are occurrences, in source order; unlike input needs, they
   need not be distinct.  These execution facts do not assume uniqueness. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame.
Import ListNotations.
Local Open Scope list_scope.

Lemma provide_fold_prefix : forall A (step : env * list key -> A -> M (env * list key)) its,
 (forall e out x s e' out' s', step(e,out)x s=Good((e',out'),s') ->
   exists added,out'=out++added) ->
 forall e out s e' out' s',mfold step(e,out)its s=Good((e',out'),s') ->
 exists added,out'=out++added.
Proof.
 intros A step its H;induction its as[|x xs IH];intros e out s e' out' s' E.
 - cbn[mfold ret]in E;injection E as <- <- <-. exists[];now rewrite app_nil_r.
 - cbn[mfold]in E. apply bind_inv in E as([e1 out1]&s1&Ex&E).
   destruct(H _ _ _ _ _ _ _ Ex)as[a Ea].
   destruct(IH _ _ _ _ _ _ E)as[b Eb].
   exists(a++b);rewrite Eb,Ea,app_assoc;reflexivity.
Qed.

Local Opaque app_subst resolve_type_symbol find_method_decl symbol_type fn_sig
 method_key provided_at resolve_sym already_provided ety_of fresh functor_fixed.
Lemma provide_spec_prefix : forall fuel e fg mi sp outer out s e' out' s',
 provide_spec fuel e fg mi sp outer out s=Good((e',out'),s') ->
 exists added,out'=out++added.
Proof.
 induction fuel as[|f IH];intros e fg mi [p d a] outer out s e' out' s' E.
 - discriminate E.
 - cbn[provide_spec]in E. apply bind_inv in E as(sub&sx&Es&E).
   destruct d as[dn|],p as[pp|];try discriminate E.
   + apply bind_inv in E as(r&sr&Er&E).
     apply bind_inv in E as(mo&sm&Em&E). destruct mo as[m|];[|discriminate E].
     apply bind_inv in E as(this&st&Et&E).
     apply bind_inv in E as(sg&sgs&Esg&E).
     apply bind_inv in E as(k&sk&Ek&E).
     apply bind_inv in E as(ep&se&Ep&E).
     injection E as <- <- <-. exists[k];reflexivity.
   + apply bind_inv in E as(tg&sg&Eg&E). destruct tg as[g|];[|discriminate E].
     destruct(gs_kind g)eqn:K;destruct(gs_decl g);cbn beta iota zeta in E;
       try (destruct(already_provided g);[injection E as <- <- <-;exists[];now rewrite app_nil_r|]);
       try discriminate E.
     all: try first[
       eapply provide_fold_prefix in E;[exact E|intros; eapply IH;eassumption]
       | repeat apply bind_inv in E as(? &? &? &E);
         injection E as <- <- <-;eexists;reflexivity
       | injection E as <- <- <-;exists[];now rewrite app_nil_r].
     destruct o0;[discriminate E|].
     repeat apply bind_inv in E as(? &? &? &E).
     injection E as <- <- <-;eexists;reflexivity.
Qed.

(* Installing output values, functions and methods cannot change any type
   meaning or local binding.  Nested context output lists have the same frame. *)
Definition provision_frame(e e':env):Prop :=
 en_tn e'=en_tn e /\ en_uf e'=en_uf e /\ en_locals e'=en_locals e.
Lemma provision_frame_refl:forall e,provision_frame e e.
Proof. intros;repeat split. Qed.
Lemma provision_frame_trans:forall e1 e2 e3,
 provision_frame e1 e2 ->provision_frame e2 e3 ->provision_frame e1 e3.
Proof. intros e1 e2 e3[A[B C]][D[E F]];unfold provision_frame;rewrite D,E,F;auto. Qed.
Lemma provide_fold_frame : forall A (step : env * list key -> A -> M (env * list key)) its,
 (forall e out x s e' out' s', step(e,out)x s=Good((e',out'),s') ->
   provision_frame e e') ->
 forall e out s e' out' s',mfold step(e,out)its s=Good((e',out'),s') ->
 provision_frame e e'.
Proof.
 intros A step its H;induction its as[|x xs IH];intros e out s e' out' s' E.
 - cbn[mfold ret]in E;injection E as <- <- <-;unfold provision_frame;repeat split.
 - cbn[mfold]in E. apply bind_inv in E as([e1 out1]&s1&Ex&E).
   eapply provision_frame_trans;[eapply H;exact Ex|eapply IH;exact E].
Qed.
Local Opaque provision_app.
Local Transparent provided_at.
Lemma provided_at_frame:forall e k g tm meth s e' s',
 provided_at e k g tm meth s=Good(e',s')->provision_frame e e'.
Proof.
 intros e k g tm meth s e' s' E. unfold provided_at in E.
 apply bind_inv in E as(a&sa&Ea&E). apply bind_inv in E as(stamp&ss&Es&E).
 injection E as <- <-. unfold provision_frame;repeat split.
Qed.
Local Opaque provided_at.
Lemma provide_spec_frame:forall fuel e fg mi sp outer out s e' out' s',
 provide_spec fuel e fg mi sp outer out s=Good((e',out'),s')->
 provision_frame e e'.
Proof.
 induction fuel as[|f IH];intros e fg mi [p d a] outer out s e' out' s' E.
 - discriminate E.
 - cbn[provide_spec]in E. apply bind_inv in E as(sub&sx&Es&E).
   destruct d as[dn|],p as[pp|];try discriminate E.
   + apply bind_inv in E as(r&sr&Er&E).
     apply bind_inv in E as(mo&sm&Em&E). destruct mo as[m|];[|discriminate E].
     apply bind_inv in E as(this&st&Et&E).
     apply bind_inv in E as(sg&sgs&Esg&E).
     apply bind_inv in E as(k&sk&Ek&E).
     apply bind_inv in E as(ep&se&Ep&E). injection E as <- <- <-.
     eapply provision_frame_trans;[|eapply provided_at_frame;exact Ep].
     destruct k;unfold provision_frame;repeat split.
   + apply bind_inv in E as(tg&sg&Eg&E). destruct tg as[g|];[|discriminate E].
     destruct(gs_kind g)eqn:K;destruct(gs_decl g);cbn beta iota zeta in E;
       try (destruct(already_provided g);[injection E as <- <- <-;unfold provision_frame;repeat split|]);
       try discriminate E.
     all: try first[
       eapply provide_fold_frame in E;[exact E|intros;eapply IH;eassumption]
       | apply bind_inv in E as(y&sy&Et&E);
         apply bind_inv in E as(ep&se&Ep&E);injection E as <- <- <-;
         apply provided_at_frame in Ep;exact Ep
       | injection E as <- <- <-;unfold provision_frame;repeat split].

     destruct o0;[discriminate E|].
     repeat apply bind_inv in E as(? &? &? &E).
     injection E as <- <- <-;unfold provision_frame;repeat split.
Qed.

Corollary provide_spec_tymap:forall fuel e fg mi sp outer out s e' out' s',
 provide_spec fuel e fg mi sp outer out s=Good((e',out'),s')->
 tymap e'=tymap e /\ forall t,canon e' t=canon e t.
Proof.
 intros;destruct(provide_spec_frame _ _ _ _ _ _ _ _ _ _ _ H)as[T[U L]].
 split;[unfold tymap;now rewrite T|intro t;unfold canon;now rewrite U].
Qed.

Definition install_output_type(fresh_of:list(nat*nat))(e:env)(t:nat):M env:=
 match nassoc t fresh_of with
 |Some g=>let!stamp:=fresh in ret(set_tn e t(YAbs g)stamp)
 |None=>ret e
 end.
Lemma provide_result_phases:forall e fg gens s e' keys s',
 provide_result e fg gens s=Good((e',keys),s')->
 exists ts st et se n sn,
 result_types fg s=Good(ts,st) /\
 mfold(install_output_type gens)e ts st=Good(et,se) /\
 walk_fuel se=Good(n,sn) /\
 mfold(fun acc sp=>provide_spec n(fst acc)fg(gs_mod fg)sp[](snd acc))
   (et,[])(let '(_,res,_) :=functor_parts fg in res)sn=Good((e',keys),s').
Proof.
 intros e fg gens s e' keys s' E;unfold provide_result in E.
 apply bind_inv in E as(ts&st&Et&E).
 apply bind_inv in E as(et&se&Ee&E).
 destruct(functor_parts fg)as[[args res]body]eqn:Fp.
 apply bind_inv in E as(n&sn&En&E).
 exists ts,st,et,se,n,sn;repeat split;try assumption.
Qed.

Module A:=Audit.Elaboration.
Scheme output_entry_mut := Minimality for A.OutputItemEntries Sort Prop
 with output_entries_mut := Minimality for A.OutputItemsEntries Sort Prop.
Lemma output_items_types:forall W p ob tm specs es tm',
 A.OutputItemsEntries W p ob tm specs es tm' ->tm'=tm.
Proof.
 intro W. apply(output_entries_mut W
   (fun p ob tm sp es tm'=>tm'=tm)
   (fun p ob tm sps es tm'=>tm'=tm));intros;congruence.
Qed.
Lemma output_item_types:forall W p ob tm sp es tm',
 A.OutputItemEntries W p ob tm sp es tm' ->tm'=tm.
Proof.
 intro W. apply(output_entry_mut W
   (fun p ob tm sp es tm'=>tm'=tm)
   (fun p ob tm sps es tm'=>tm'=tm));intros;congruence.
Qed.
Lemma result_keys_app:forall sigma es1 es2 ks1 ks2,
 A.ResultKeys sigma es1 ks1 ->A.ResultKeys sigma es2 ks2 ->
 A.ResultKeys sigma(es1++es2)(ks1++ks2).
Proof. intros sigma es1 es2 ks1 ks2->->;unfold A.ResultKeys;symmetry;apply flat_map_app. Qed.
