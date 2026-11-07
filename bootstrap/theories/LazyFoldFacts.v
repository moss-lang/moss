(* Exact scalar folding, followed by correctness of the gathered-entry cache.
   No type-erasure hypothesis: ordinary source lookups commute with call-time
   type instantiation, and checked casts remain outside the translation. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit MossX LazyX LazyEnvFacts LazyFold.
From NM Require FastPath.
Import ListNotations.
Set Implicit Arguments.
Definition remaps(pl:option plan)(d d':fenv):Prop:=
 forall i,ordinary i=true->read d(remapo pl i)=flookup i d'.
Lemma remaps_identity : forall d,remaps None d d.
Proof. intros d i Hi;reflexivity. Qed.
Lemma call_ordinary : forall P d th dc i,ordinary i=true->
 flookup i(fcall_env P d th dc)=flookup i dc.
Proof.
 intros P d th [values types more]i Hi;destruct i;try discriminate Hi;
 unfold fcall_env;destruct(xreify P);reflexivity.
Qed.
Lemma remaps_call : forall pl base dc P caller th,remaps pl base dc->
 remaps pl base(fcall_env P caller th dc).
Proof. intros pl base dc P caller th H i Hi;rewrite call_ordinary by exact Hi;apply H;exact Hi. Qed.
Lemma remaps_compose : forall pl sm base d dc,
 remaps pl base d->ordinary_sources sm=true->fcompose d sm=Some dc->
 remaps(Some(remap_plan pl sm))base dc.
Proof.
 intros pl sm;induction sm as[|[req src]sm IH];intros base d dc Hr Hs Hc.
 - cbn[fcompose]in Hc;injection Hc as <-;intros i Hi;destruct i;try discriminate Hi;reflexivity.
 - cbn[ordinary_sources forallb snd]in Hs;apply andb_true_iff in Hs as[Src Rest].
   cbn[fcompose]in Hc;destruct(flookup src d)as[en|]eqn:Read;[|discriminate].
   destruct(fcompose d sm)as[tail|]eqn:Tail;[|discriminate];injection Hc as <-.
   intros i Hi;cbn[remapo remap_plan map fst snd remap];rewrite flookup_fadd.
   destruct(item_eqb i req).
   + rewrite(Hr src Src),Read;reflexivity.
   + exact(IH base d tail Hr Rest Tail i Hi).
Qed.
Lemma checks_compose : forall pl sm base d,
 remaps pl base d->ordinary_sources sm=true->checks base(remap_plan pl sm)=true->
 exists dc,fcompose d sm=Some dc.
Proof.
 intros pl sm;induction sm as[|[req src]sm IH];intros base d Hr Hs Hc;[exists fempty;reflexivity|].
 cbn[ordinary_sources forallb snd]in Hs;apply andb_true_iff in Hs as[Src Rest].
 cbn[checks remap_plan map forallb fst snd]in Hc;apply andb_true_iff in Hc as[Head Tail].
 destruct(read base(remapo pl src))as[en|]eqn:Read;[|discriminate].
 destruct(IH base d Hr Rest Tail)as[dc E];exists(fadd req en dc).
 cbn[fcompose];rewrite <-(Hr src Src),Read,E;reflexivity.
Qed.
Lemma sev_prim : forall P d g st i args,sev P d g st(SPrim i args)=
 match read d i with
 |Some(FPrim p)=>match sevl P d g args st with Some(ws,st1)=>match run_prim_at P p None ws st1 with
   |(XOk w,st2)=>Some(w,st2)|_=>None end|None=>None end
 |Some(FPrimAt p tau)=>match sevl P d g args st with Some(ws,st1)=>match run_prim_at P p(Some tau)ws st1 with
   |(XOk w,st2)=>Some(w,st2)|_=>None end|None=>None end
 |_=>None end.
Proof. reflexivity. Qed.
Lemma sev_call : forall P d g st guard args body,sev P d g st(SCall guard args body)=
 if checks d guard then match sevl P d g args st with
 |Some(ws,st1)=>sev P d(mk_xvenv ws)st1 body|None=>None end else None.
Proof. reflexivity. Qed.
Lemma depth_prim : forall i args,sdepth(SPrim i args)=S(needed args).
Proof. reflexivity. Qed.
Lemma depth_call : forall guard args b,sdepth(SCall guard args b)=S(Nat.max(needed args)(sdepth b)).
Proof. reflexivity. Qed.
Lemma fcast_plain : forall P d n th w,lookup_cast n(xp_cast P)=None->
 fcast P d n th w=XOk(XVTag n w).
Proof. intros;unfold fcast;rewrite H;reflexivity. Qed.

Section Sound.
Variable P:xprog.
Definition SOUND(k:nat):Prop:=
 (forall m n pl e s d d' g st w st',tos P m n pl e=Some s->remaps pl d d'->
   sev P d g st s=Some(w,st')->sdepth s<=k->fxinterp k P d' g st e=XR(XOk w)g st') /\
 (forall m n pl es ss d d' g st ws st',tosl P m n pl es=Some ss->remaps pl d d'->
   sevl P d g ss st=Some(ws,st')->needed ss<=k->fxinterp_list k P d' g st es=inl(ws,g,st')).
Lemma sound_step : forall k,SOUND k->SOUND(S k).
Proof.
 intros k[IHe IHl];split.
 - intros m n pl e s d d' g st w st' Ht Hr Hs Hk.
   destruct m as[|m];[discriminate|];cbn[tos]in Ht.
   destruct e as[x| |v|tag theta inner|f theta sm args|f args|a method it args|a arms|x a b|
    t tau body|v a b|f f' sm body|tau method theta f sm body|u h link sm body|
    u u' sm body|u theta sm body|u inputs outputs body|a b|x a|a b c|a b|a|a| |
    fields|a field|inner arms|u h body|u u' body|u branches body|it prim body|v body];try discriminate.
   + inversion Ht;subst;cbn[sev]in Hs;cbn[fxinterp].
     destruct(lookup_xvenv x g);inversion Hs;subst;reflexivity.
   + inversion Ht;subst;cbn[sev]in Hs;inversion Hs;subst;reflexivity.
   + inversion Ht;subst;cbn[sev]in Hs.
     rewrite(Hr(IVal v)eq_refl)in Hs;cbn[fxinterp].
     destruct(flookup(IVal v)d')as[[]|];inversion Hs;subst;reflexivity.
   + destruct(lookup_cast tag(xp_cast P))as[spec|]eqn:Cast;[discriminate|].
     destruct(tos P m n pl inner)as[s1|]eqn:Ei;[|discriminate].
     inversion Ht;subst;cbn[sev]in Hs;destruct(sev P d g st s1)as[[w1 st1]|]eqn:Es;[|discriminate].
     inversion Hs;subst;cbn[fxinterp];cbn[sdepth]in Hk.
     rewrite(IHe _ _ _ _ _ _ _ _ _ _ _ Ei Hr Es)by lia.
     rewrite fcast_plain by exact Cast;reflexivity.
   + destruct(ordinary_sources sm)eqn:Ord;[|discriminate].
     destruct n as[|n'];[discriminate|];destruct(xp_fn P f)as[body|]eqn:Fn;[|discriminate].
     destruct(tosl P m(S n')pl args)as[ss|]eqn:El;[|discriminate].
     destruct(tos P m n'(Some(remap_plan pl sm))body)as[sb|]eqn:Eb;[|discriminate].
     inversion Ht;subst;rewrite sev_call in Hs;rewrite depth_call in Hk.
     destruct(checks d(remap_plan pl sm))eqn:Check;[|discriminate].
     destruct(sevl P d g ss st)as[[ws st1]|]eqn:Esl;[|discriminate].
     destruct(@checks_compose pl sm d d' Hr Ord Check)as[d2 Compose].
     pose proof(@remaps_call (Some(remap_plan pl sm)) d d2 P d' theta (@remaps_compose pl sm d d' d2 Hr Ord Compose))as Frame.
     cbn[fxinterp];rewrite Fn,(IHl _ _ _ _ _ _ _ _ _ _ _ El Hr Esl)by lia.
     rewrite Compose,(IHe _ _ _ _ _ _ _ _ _ _ _ Eb Frame Hs)by lia;reflexivity.
   + destruct(tosl P m n pl args)as[ss|]eqn:El;[|discriminate];inversion Ht;subst.
     rewrite sev_prim,(Hr(IFn f)eq_refl)in Hs;rewrite depth_prim in Hk;cbn[fxinterp].
     destruct(flookup(IFn f)d')as[[w0|f0 dc0|h0 sms0 dc0|p0|h0|tau0|p0 tau0]|];try discriminate.
     all:destruct(sevl P d g ss st)as[[ws st1]|]eqn:Esl;[|discriminate].
     all:rewrite(IHl _ _ _ _ _ _ _ _ _ _ _ El Hr Esl)by lia.
     * destruct(run_prim_at P p0 None ws st1)as[[]st2];inversion Hs;subst;reflexivity.
     * destruct(run_prim_at P p0(Some tau0)ws st1)as[[]st2];inversion Hs;subst;reflexivity.
   + destruct arms as[|[pat body][|]];try discriminate.
     destruct(tos P m n pl inner)as[s0|]eqn:E0;[|discriminate].
     destruct(tos P m n pl body)as[sb|]eqn:Eb;[|discriminate];inversion Ht;subst.
     cbn[sev]in Hs;cbn[sdepth]in Hk.
     destruct(sev P d g st s0)as[[w0 st1]|]eqn:Es;[|discriminate].
     cbn[fxinterp];rewrite(IHe _ _ _ _ _ _ _ _ _ _ _ E0 Hr Es)by lia.
     cbn[select_arm];destruct(match_pat pat w0)as[bs| |]eqn:Match;try discriminate.
     rewrite(IHe _ _ _ _ _ _ _ _ _ _ _ Eb Hr Hs)by lia.
     cbn;rewrite skipn_app,Nat.sub_diag,skipn_all;reflexivity.
 - intros m n pl es ss d d' g st ws st' Ht Hr Hs Hk.
   destruct m as[|m];[discriminate|];cbn[tosl]in Ht;destruct es as[|e es].
   + inversion Ht;subst;cbn[sevl]in Hs;inversion Hs;subst;reflexivity.
   + destruct(tos P m n pl e)as[s|]eqn:Ee;[|discriminate].
     destruct(tosl P m n pl es)as[rest|]eqn:Er;[|discriminate];inversion Ht;subst.
     cbn[sevl]in Hs;cbn[needed]in Hk.
     destruct(sev P d g st s)as[[w0 st1]|]eqn:Es;[|discriminate].
     destruct(sevl P d g rest st1)as[[ws0 st2]|]eqn:Esl;[|discriminate];inversion Hs;subst.
     cbn[fxinterp_list];rewrite(IHe _ _ _ _ _ _ _ _ _ _ _ Ee Hr Es)by lia.
     rewrite(IHl _ _ _ _ _ _ _ _ _ _ _ Er Hr Esl)by lia;reflexivity.
Qed.
Theorem scalar_sound : forall fuel,SOUND fuel.
Proof.
 induction fuel as[|k IH].
 - split;intros;[destruct s|destruct ss];cbn[sdepth needed]in *;lia.
 - apply sound_step;exact IH.
Qed.
End Sound.

Lemma lk_eqb_refl : forall i,lk_eqb i i=true.
Proof. intros[i|];[apply FastPath.item_eqb_refl|reflexivity]. Qed.
Lemma lk_eqb_eq : forall i j,lk_eqb i j=true->i=j.
Proof. intros[i|][j|];cbn[lk_eqb];try discriminate;[intro E;apply CoreMoss.item_eqb_eq in E;subst;reflexivity|reflexivity]. Qed.
Lemma gather_lookup : forall keys d slots,gather d keys=Some slots->forall i,In i keys->
 cache_lookup keys i slots=read d i.
Proof.
 induction keys as[|j keys IH];intros d slots H i InKeys;[contradiction|].
 cbn[gather]in H;destruct(read d j)as[en|]eqn:Read;[|discriminate].
 destruct(gather d keys)as[tail|]eqn:Tail;[|discriminate];injection H as <-.
 unfold cache_lookup;cbn[key_pos];destruct(lk_eqb i j)eqn:Eq.
 - apply lk_eqb_eq in Eq;subst i;cbn;rewrite Read;reflexivity.
 - destruct InKeys as[<-|InTail];[rewrite lk_eqb_refl in Eq;discriminate|].
   specialize(IH d tail Tail i InTail);unfold cache_lookup in IH.
   destruct(key_pos i keys);exact IH.
Qed.
Lemma dedup_keys_in : forall xs i,In i xs->
 In i(fold_right(fun key acc=>if existsb(lk_eqb key)acc then acc else key::acc)[]xs).
Proof.
 intros xs;induction xs as[|j xs IH];intros i Hin;[contradiction|].
 cbn[fold_right];destruct(existsb(lk_eqb j)(fold_right(fun key acc=>if existsb(lk_eqb key)acc then acc else key::acc)[]xs))eqn:Present.
 - destruct Hin as[<-|Hin];[|apply IH;exact Hin].
   apply existsb_exists in Present as[k[Hin Eq]];apply lk_eqb_eq in Eq;subst k;exact Hin.
 - destruct Hin as[<-|Hin];[left;reflexivity|right;apply IH;exact Hin].
Qed.
Lemma gather_reads : forall s d slots,gather d(read_keys s)=Some slots->forall i,In i(reads s)->
 cache_lookup(read_keys s)i slots=read d i.
Proof. intros s d slots H i Hi;apply(@gather_lookup (read_keys s) d slots H i);unfold read_keys;apply dedup_keys_in;exact Hi. Qed.
Lemma mkv_eq : forall ws,LazyFold.mkv ws=mk_xvenv ws.
Proof. unfold LazyFold.mkv,mk_xvenv;intro ws;generalize 0;induction ws;intro i;cbn;[reflexivity|f_equal;apply IHws]. Qed.
Definition klc P keys args:=fold_right(fun e cr=>let c:=kc P keys e in kcons c cr)knil args.
Lemma gather_present : forall keys d slots,gather d keys=Some slots->
 forall i,In i keys->exists en,read d i=Some en.
Proof.
 induction keys as[|j keys IH];intros d slots H i Hi;[contradiction|].
 cbn[gather]in H;destruct(read d j)as[en|]eqn:Read;[|discriminate].
 destruct(gather d keys)as[tail|]eqn:Tail;[|discriminate].
 destruct Hi as[<-|Hi];[exists en;exact Read|eapply IH;eassumption].
Qed.
Lemma present_checks : forall d pl,
 (forall i,In i(map snd pl)->exists en,read d i=Some en)->checks d pl=true.
Proof.
 intros d pl;induction pl as[|[req src]pl IH];intro H;[reflexivity|].
 destruct(H src(or_introl eq_refl))as[en Read].
 cbn[checks forallb snd];rewrite Read;cbn;apply IH.
 intros i Hi;apply H;right;exact Hi.
Qed.

Lemma cached_compile_n : forall P n s keys slots d g st,
 sdepth s<=n->
 (forall i,In i(reads s)->cache_lookup keys i slots=read d i)->
 (forall i,In i(reads s)->exists en,read d i=Some en)->
 kc P keys s slots g st=sev P d g st s.
Proof.
 intros P n;induction n as[|n IH];intros s keys slots d g st Bound Source Present.
 - destruct s;cbn[sdepth]in Bound;lia.
 - assert(Args:forall args g st,needed args<=S n->
    (forall i,In i(flat_map reads args)->cache_lookup keys i slots=read d i)->
    (forall i,In i(flat_map reads args)->exists en,read d i=Some en)->
    klc P keys args slots g st=sevl P d g args st).
   {intros args;induction args as[|a args Tail];intros g0 st0 B R Q;[reflexivity|].
    cbn[needed]in B.
    change(klc P keys(a::args))with(kcons(kc P keys a)(klc P keys args)).
    unfold kcons;cbn[sevl].
    rewrite(IH a keys slots d g0 st0)by(first[lia|intros i Hi;apply R;apply in_or_app;left;exact Hi|intros i Hi;apply Q;apply in_or_app;left;exact Hi]).
    destruct(sev P d g0 st0 a)as[[w st1]|];[|reflexivity].
    rewrite(Tail g0 st1)by(first[lia|intros i Hi;apply R;apply in_or_app;right;exact Hi|intros i Hi;apply Q;apply in_or_app;right;exact Hi]);reflexivity. }
   destruct s as[x| |i|tag a|i args|guard args body|a pat body];cbn[reads]in Source,Present.
   + reflexivity.
   + reflexivity.
   + cbn[kc sev];rewrite(Source i(or_introl eq_refl));reflexivity.
   + cbn[sdepth]in Bound;cbn[kc sev].
     rewrite(IH a keys slots d g st)by assumption||lia;reflexivity.
   + change(S(needed args)<=S n)in Bound.
     cbn[kc];fold(klc P keys args);rewrite sev_prim.
     rewrite(Source i(or_introl eq_refl)).
     rewrite(Args args g st)by(first[lia|intros j Hj;apply Source;right;exact Hj|intros j Hj;apply Present;right;exact Hj]);reflexivity.
   + change(S(Nat.max(needed args)(sdepth body))<=S n)in Bound.
     cbn[kc];fold(klc P keys args);rewrite sev_call.
     rewrite(present_checks d guard)by(intros i Hi;apply Present;apply in_or_app;left;exact Hi).
     rewrite(Args args g st)by(first[lia|intros i Hi;apply Source;apply in_or_app;right;apply in_or_app;left;exact Hi|intros i Hi;apply Present;apply in_or_app;right;apply in_or_app;left;exact Hi]).
     destruct(sevl P d g args st)as[[ws st1]|];[|reflexivity].
     rewrite mkv_eq;apply IH;[lia| |].
     * intros i Hi;apply Source;apply in_or_app;right;apply in_or_app;right;exact Hi.
     * intros i Hi;apply Present;apply in_or_app;right;apply in_or_app;right;exact Hi.
   + cbn[sdepth]in Bound;cbn[kc sev].
     rewrite(IH a keys slots d g st)by(first[lia|intros i Hi;apply Source;apply in_or_app;left;exact Hi|intros i Hi;apply Present;apply in_or_app;left;exact Hi]).
     destruct(sev P d g st a)as[[w st1]|];[|reflexivity].
     destruct(match_pat pat w)as[bs| |];try reflexivity.
     apply IH;[lia| |];intros i Hi;[apply Source|apply Present];apply in_or_app;right;exact Hi.
Qed.
Theorem cached_compile : forall P s keys slots d g st,
 (forall i,In i(reads s)->cache_lookup keys i slots=read d i)->
 (forall i,In i(reads s)->exists en,read d i=Some en)->
 kc P keys s slots g st=sev P d g st s.
Proof. intros;apply cached_compile_n with(n:=sdepth s);[lia|assumption|assumption]. Qed.
Theorem fold_call_agrees : forall P e fallback n d g st,
 fallback n d g st=fxinterp n P d g st e->
 fold_call P e fallback n d g st=fxinterp n P d g st e.
Proof.
 intros P e fallback n d g st Slow;unfold fold_call.
 destruct(tos P 48 6 None e)as[s|]eqn:T;[|exact Slow].
 destruct(Nat.leb(sdepth s)n)eqn:Enough;[|exact Slow].
 destruct(gather d(read_keys s))as[slots|]eqn:Gather;[|exact Slow].
 rewrite(@cached_compile P s (read_keys s) slots d g st (@gather_reads s d slots Gather)).
 2: {intros i Hi;apply(@gather_present (read_keys s) d slots Gather);unfold read_keys;apply dedup_keys_in;exact Hi. }
 destruct(sev P d g st s)as[[w st1]|]eqn:Run;[|exact Slow].
 symmetry;eapply(proj1(scalar_sound P n));[exact T|apply remaps_identity|exact Run|apply Nat.leb_le;exact Enough].
Qed.
