(* Actual attached-method lookup, including its redundant local-table fallback. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy MethodLookup MethodLookupNone MethodKind.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL].
Theorem local_attached_receiver:forall b w,
 load_world b=Good w->forall mi r n att,
 assoc attached_eqb(r,n)(gm_attached(get_mod w mi))=Some att->
 exists gr,nget(w_syms w)r=Some gr/\gs_mod gr=mi.
Proof.
 intros b w Hl mi r n att Ha.
 unfold load_world in Hl;apply rbind_good in Hl as([en st]&Hs&Hl);injection Hl as <-.
 destruct(load_spec b _ _ _ _ _ _ Hs(linv_init b))as[L _].
 destruct(nget(ls_mods st)mi)as[m|]eqn:Em.
 2:{unfold get_mod in Ha;cbn[w_mods]in Ha;rewrite Em in Ha;discriminate Ha. }
 rewrite(fin_get_mod b st en mi m Em)in Ha.
 destruct(li_attached _ _ L mi m r n att Em Ha)as(g&rn&ps&rt&body&Hg&Mg&Dg&Hrn).
 destruct(li_symfacts _ _ L att g Hg)as(m1&p1&am1&Em1&Hp1&Hma&Hocc&Hkind&Hname&Hdet&Hatt).
 rewrite Mg in Em1;rewrite Em in Em1;injection Em1 as <-.
 destruct(Hatt rn n ps rt body Dg)as(r'&gr&Er&Hgr&Eowner&Ea).
 rewrite Hrn in Er;injection Er as <-.
 exists gr;split;[exact Hgr|rewrite Mg in Eowner;exact Eowner].
Qed.
Theorem local_attached_owner:forall b w,
 load_world b=Good w->forall mi r gr n att,
 In mi(w_order w)->nget(w_syms w)r=Some gr->
 assoc attached_eqb(r,n)(gm_attached(get_mod w mi))=Some att->gs_mod gr=mi.
Proof.
 intros b w Hl mi r gr n att Hmi Hr Ha.
 destruct(local_attached_receiver b w Hl mi r n att Ha)as[gr'[Hg Gm]].
 rewrite Hr in Hg;injection Hg as <-;exact Gm.
Qed.
Definition attached_call_lookup(mi hid:nat)(nm:name):M(option gsym):=
 let! here:=the_mod mi in
 let! hg:=sym hid in
 let! hm:=the_mod(gs_mod hg)in
 match assoc attached_eqb(hid,nm)(gm_attached hm)with
 | Some s=>let! g:=sym s in ret(Some g)
 | None=>match assoc attached_eqb(hid,nm)(gm_attached here)with
   |Some s=>let! g:=sym s in ret(Some g)|None=>ret None end
 end.
Lemma ok_attached_call_lookup:forall mi hid nm,ok(attached_call_lookup mi hid nm).
Proof. intros;unfold attached_call_lookup;okt. Qed.
Section Selection.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Lemma attached_call_lookup_exact:forall mi hid hg nm s mg s',
 In mi(w_order w)->nget(w_syms w)hid=Some hg->
 Inv s->st_w s=w->attached_call_lookup mi hid nm s=Good(mg,s')->
 s'=s/\match mg with
 |Some g=>A.Attached WW hid nm(gs_id g)/\nget(w_syms w)(gs_id g)=Some g
 |None=>forall m,~A.Attached WW hid nm m end.
Proof.
 intros mi hid hg nm s mg s' Hmi Hg Hi Hw H.
 unfold attached_call_lookup in H.
 apply bind_inv in H as(here&s1&Eh&H);unfold the_mod in Eh;injection Eh as <- <-.
 apply bind_inv in H as(g0&s1&Eg&H).
 assert(G:nget(st_syms s)hid=Some hg)by exact(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)hid hg ltac:(rewrite Hw;exact Hg)).
 unfold sym in Eg;rewrite G in Eg;injection Eg as <- <-.
 apply bind_inv in H as(hm&s1&Em&H);unfold the_mod in Em;injection Em as <- <-.
 rewrite Hw in H.
 destruct(assoc attached_eqb(hid,nm)(gm_attached(get_mod w(gs_mod hg))))as[mid|]eqn:Ea.
 - apply bind_inv in H as(g&s1&Es&H);injection H as <- <-.
   assert(Hatt:A.Attached WW hid nm mid)by(unfold A.Attached;change(match nget(w_syms w)hid with Some gr=>assoc attached_eqb(hid,nm)(gm_attached(get_mod w(gs_mod gr)))|None=>None end=Some mid);rewrite Hg;exact Ea).
   pose proof(method_named_kind b w R Fl mi hid nm mid Hload Hmi(or_introl Hatt))as K.
   unfold A.kind_sym in K;cbn[A.w_decl BodySoundCore.W W_read]in K.
   destruct(nget(w_syms w)mid)as[g1|]eqn:Hg1;[|discriminate K].
   assert(G1:nget(st_syms s)mid=Some g1)by exact(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)mid g1 ltac:(rewrite Hw;exact Hg1)).
   unfold sym in Es;rewrite G1 in Es;injection Es as <- <-.
   rewrite(load_world_ids b w Hload mid g1 Hg1).
   split;[reflexivity|split;assumption].
 - destruct(assoc attached_eqb(hid,nm)(gm_attached(get_mod w mi)))as[mid|]eqn:El.
   + pose proof(local_attached_owner b w Hload mi hid hg nm mid Hmi Hg El)as Owner.
     rewrite Owner,El in Ea;discriminate Ea.
   + injection H as <- <-;split;[reflexivity|]. intros m Ha;unfold A.Attached in Ha.
     change(match nget(w_syms w)hid with Some gr=>assoc attached_eqb(hid,nm)(gm_attached(get_mod w(gs_mod gr)))|None=>None end=Some m)in Ha;rewrite Hg,Ea in Ha;discriminate Ha.
Qed.
Lemma attached_call_lookup_any:forall mi hid nm s mg s',
 In mi(w_order w)->Inv s->st_w s=w->attached_call_lookup mi hid nm s=Good(mg,s')->
 s'=s/\match mg with
 |Some g=>A.Attached WW hid nm(gs_id g)/\nget(w_syms w)(gs_id g)=Some g
 |None=>forall m,~A.Attached WW hid nm m end.
Proof.
 intros mi hid nm s mg s' Hmi Hi Hw H.
 destruct(nget(w_syms w)hid)as[hg|]eqn:Hg.
 - exact(attached_call_lookup_exact mi hid hg nm s mg s' Hmi Hg Hi Hw H).
 - unfold attached_call_lookup in H.
   apply bind_inv in H as(here&s1&Eh&H);unfold the_mod in Eh;injection Eh as <- <-.
   apply bind_inv in H as(hg&s1&Es&H).
   unfold sym in Es;destruct(nget(st_syms s)hid)as[g|]eqn:Eg;[|discriminate Es].
   injection Es as <- <-.
   apply bind_inv in H as(hm&s1&Em&H);unfold the_mod in Em;injection Em as <- <-.
   rewrite Hw in H.
   assert(Empty:forall j,assoc attached_eqb(hid,nm)(gm_attached(get_mod w j))=None).
   {intro j;destruct(assoc attached_eqb(hid,nm)(gm_attached(get_mod w j)))as[a|]eqn:E;[|reflexivity].
    destruct(local_attached_receiver b w Hload j hid nm a E)as[gr[Hgr _]].
    rewrite Hg in Hgr;discriminate Hgr. }
   rewrite!Empty in H;injection H as <- <-.
   split;[reflexivity|]. intros m Ha;unfold A.Attached in Ha;change(match nget(w_syms w)hid with Some gr=>assoc attached_eqb(hid,nm)(gm_attached(get_mod w(gs_mod gr)))|None=>None end=Some m)in Ha.
   rewrite Hg in Ha;discriminate Ha.
Qed.
End Selection.
Definition attached_here_lookup(here:gmod)(hid:nat)(nm:name):M(option gsym):=
 let! hg:=sym hid in
 let! hm:=the_mod(gs_mod hg)in
 match assoc attached_eqb(hid,nm)(gm_attached hm)with
 |Some mid=>let! g:=sym mid in ret(Some g)
 |None=>match assoc attached_eqb(hid,nm)(gm_attached here)with
   |Some mid=>let! g:=sym mid in ret(Some g)|None=>ret None end
 end.
Lemma attached_here_state:forall here h n s a s',
 attached_here_lookup here h n s=Good(a,s')->s'=s.
Proof.
 intros here h n s a s' H;unfold attached_here_lookup in H.
 apply bind_inv in H as(hg&s1&Hs&H).
 unfold sym in Hs;destruct(nget(st_syms s)h);[|discriminate Hs];injection Hs as <- <-.
 apply bind_inv in H as(hm&s1&Hm&H);unfold the_mod in Hm;injection Hm as <- <-.
 destruct(assoc attached_eqb(h,n)(gm_attached(get_mod(st_w s)(gs_mod g))));
 [|destruct(assoc attached_eqb(h,n)(gm_attached here))];
 try(apply bind_inv in H as(g1&s1&Hg&H);unfold sym in Hg;destruct(nget(st_syms s)n0);[|discriminate Hg];injection Hg as <- <-);
 injection H;auto.
Qed.
Lemma attached_here_call:forall mi h n s,
 attached_here_lookup(get_mod(st_w s)mi)h n s=attached_call_lookup mi h n s.
Proof. reflexivity. Qed.
