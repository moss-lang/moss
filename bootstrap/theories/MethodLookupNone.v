(* Successful absence agrees with the attached-first semantic lookup. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame MethodLookup MethodKind.
Import ListNotations.
Module A:=Audit.Elaboration.
Theorem find_method_decl_none:forall b w R Fl mi recv n s s',
 load_world b=Good w->In mi(w_order w)->
 nget(w_syms w)(gs_id recv)=Some recv->st_w s=w->
 find_method_decl mi recv n s=Good(None,s')->
 s'=s/\forall m,~A.MethodNamed(W_read w R Fl)(gm_path(get_mod w mi))(gs_id recv)n m.
Proof.
 intros b w R Fl mi recv n s s' Hl Hmi Hr Hw H.
 unfold find_method_decl in H.
 apply bind_inv in H as(hm&s1&E1&H). unfold the_mod in E1;injection E1 as <- <-.
 apply bind_inv in H as(here&s1&E1&H). unfold the_mod in E1;injection E1 as <- <-. rewrite Hw in H.
 destruct(assoc attached_eqb(gs_id recv,n)(gm_attached(get_mod w(gs_mod recv))))as[mid|]eqn:Ea.
 - apply bind_inv in H as(g&s1&E1&H). discriminate H.
 - destruct(sassoc n(gm_detached(get_mod w mi)))as[mid|]eqn:Ed.
   + apply bind_inv in H as(g&s1&E1&H). discriminate H.
   + injection H as <-. split;[reflexivity|]. intros m [Ha|[_ Hs]].
     * unfold A.Attached in Ha. cbn[A.w_attached W_read]in Ha. rewrite Hr,Ea in Ha. discriminate.
     * rewrite(loaded_scope_table b w R Fl Hl mi A.NSDetached n Hmi)in Hs.
       cbn[tbl]in Hs. rewrite Ed in Hs. discriminate.
Qed.

Lemma method_named_unique:forall W p r n m m',
 A.MethodNamed W p r n m->A.MethodNamed W p r n m'->m=m'.
Proof.
 intros W p r n m m'[Ha|[Na Sa]][Hb|[Nb Sb]].
 - unfold A.Attached in *;congruence.
 - exfalso;exact(Nb m Ha).
 - exfalso;exact(Na m' Hb).
 - congruence.
Qed.
Lemma find_method_decl_symbol:forall mi recv n s m s',
 Inv s->find_method_decl mi recv n s=Good(Some m,s')->
 s'=s/\nget(st_syms s)(gs_id m)=Some m.
Proof.
 intros mi recv n s m s' Hi H;unfold find_method_decl in H.
 apply bind_inv in H as(hm&s1&E1&H);unfold the_mod in E1;injection E1 as <- <-.
 apply bind_inv in H as(here&s1&E1&H);unfold the_mod in E1;injection E1 as <- <-.
 destruct(assoc attached_eqb(gs_id recv,n)(gm_attached(get_mod(st_w s)(gs_mod recv))))as[mid|]eqn:Ea.
 - apply bind_inv in H as(g&s1&Es&H);injection H as <- <-.
   unfold sym in Es;destruct(nget(st_syms s)mid)as[g0|]eqn:Eg;[|discriminate].
   injection Es as <- <-. split;[reflexivity|]. rewrite(iv_ids _ Hi mid _ Eg);exact Eg.
 - destruct(sassoc n(gm_detached(get_mod(st_w s)mi)))as[mid|]eqn:Ed;[|discriminate H].
   apply bind_inv in H as(g&s1&Es&H);injection H as <- <-.
   unfold sym in Es;destruct(nget(st_syms s)mid)as[g0|]eqn:Eg;[|discriminate].
   injection Es as <- <-. split;[reflexivity|]. rewrite(iv_ids _ Hi mid _ Eg);exact Eg.
Qed.
Theorem find_method_decl_loaded:forall b w (R:nat->list nat)(Fl:string->nat) mi recv n s m s',
 load_world b=Good w->In mi(w_order w)->
 nget(w_syms w)(gs_id recv)=Some recv->Inv s->st_w s=w->
 find_method_decl mi recv n s=Good(Some m,s')->
 s'=s/\nget(w_syms w)(gs_id m)=Some m/\
 exists rcv nm ps rt body,gs_decl m=ADFn rcv nm ps rt body.
Proof.
 intros b w R Fl mi recv n s m s' Hl Hmi Hr Hi Hw H.
 destruct(find_method_decl_named b w R Fl mi recv n s m s' Hl Hmi Hr Hi Hw H)as[-> Hn].
 destruct(find_method_decl_symbol mi recv n s m s Hi H)as[_ Hsym].
 pose proof(method_named_kind b w R Fl mi(gs_id recv)n(gs_id m)Hl Hmi Hn)as K.
 assert(Hm:nget(w_syms w)(gs_id m)=Some m).
 { pose proof K as K0;unfold A.kind_sym in K0;cbn[A.w_decl W_read]in K0.
   destruct(nget(w_syms w)(gs_id m))as[g|]eqn:Hg;[|discriminate K0].
   pose proof(iv_loader _ Hi b ltac:(rewrite Hw;exact Hl)(gs_id m)g ltac:(rewrite Hw;exact Hg))as Hkeep.
   rewrite Hkeep in Hsym;injection Hsym as <-;reflexivity. }
 split;[reflexivity|]. split;[exact Hm|]. exact(loaded_method_fn w R Fl m Hm K).
Qed.
