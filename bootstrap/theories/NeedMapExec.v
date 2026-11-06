(* Successful need resolution preserves exact caller keys and coherence checks. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound BodyTr CtxWalk CtxMerge Merge CanonTypes TySupport SourceSupport NominalSupport NeedExec InstantiateTr MethodKeyTr.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Lemma mmap_forall2_inv : forall X Y(f:X->M Y)(P:X->Y->Prop) l s ys s',
 (forall x,ok(f x))->
 (forall x sx y sx',Inv sx->f x sx=Good(y,sx')->P x y)->
 Inv s->mmap f l s=Good(ys,s')->Forall2 P l ys.
Proof.
 intros X Y f P l;induction l as[|x l IH];intros s ys s' Of Hf Hi E;cbn[mmap]in E.
 - injection E as <- _;constructor.
 - apply bind_inv in E as[y[sy[Ey E]]]. apply bind_inv in E as[z[sz[Ez E]]].
   injection E as <- _. constructor;[exact(Hf _ _ _ _ Hi Ey)|].
   exact(IH _ _ _ Of Hf (proj1(Of _ _ _ _ Hi Ey))Ez).
Qed.
Lemma sym_id_checked : forall n s g s',Inv s->sym n s=Good(g,s')->gs_id g=n.
Proof.
 intros n s g s' Hi E. unfold sym in E. destruct(nget(st_syms s)n)as[h|]eqn:Eh;[|discriminate].
 injection E as <- <-. exact(iv_ids _ Hi _ _ Eh).
Qed.
Theorem require_needs_all : forall mi e g info s ps s',
 Inv s->require_needs mi e g info s=Good(ps,s')->
 ps=map(fun need=>(need,site_key e need))(fi_needs info) /\
 Forall(fun need=>key_present e(site_key e need)=true)(fi_needs info) /\
 Forall(fun need=>incoherent e info need(site_key e need)=false)(fi_needs info).
Proof.
 intros mi e g info s ps s' Hi E. unfold require_needs in E.
 apply bind_inv in E as[u[su[Eu E]]].
 assert(Om:ok(require_merges mi e g info))by(unfold require_merges;okt).
 destruct(Om _ _ _ Hi Eu)as[Iu Fu].
 apply bind_inv in E as[pairs[sp[Ep E]]].
 set(P:=fun(need:key)(p:key*key*bool)=>
 let '(a,k,chk):=p in a=need /\k=site_key e need /\
 key_present e k=true /\chk=true).
 assert(HP:Forall2 P(fi_needs info)pairs).
 { eapply mmap_forall2_inv;[| |exact Iu|exact Ep].
   - intro need. destruct need;okt.
   - intros need sx y sx' Ix Ey. destruct need as[v|f|r m|u0];cbn beta iota in Ey.
     + destruct(key_present e(KyVal v))eqn:K;[injection Ey as <- _;repeat split;auto|].
       apply bind_inv in Ey as[x[ss[_ Ey]]];exfalso;exact(err_not_good _ _ _ _ _ _ Ey).
     + destruct(key_present e(KyFn f))eqn:K;[injection Ey as <- _;repeat split;auto|].
       apply bind_inv in Ey as[x[ss[_ Ey]]];exfalso;exact(err_not_good _ _ _ _ _ _ Ey).
     + apply bind_inv in Ey as[mg[sa[Es Ey]]].
       pose proof(sym_id_checked _ _ _ _ Ix Es)as Id.
       apply bind_inv in Ey as[k[sb[Ek Ey]]].
       destruct(@method_key_value _ _ _ _ _ _ _ Ek)as[-> _]. rewrite Id in Ey.
       destruct(key_present e(KyMth(canon e(instantiate e r))m))eqn:K.
       * injection Ey as <- _. repeat split;auto.
       * apply bind_inv in Ey as[x[ss[_ Ey]]];exfalso;exact(err_not_good _ _ _ _ _ _ Ey).
     + destruct(key_present e(KyFun u0))eqn:K;[injection Ey as <- _;repeat split;auto|].
       apply bind_inv in Ey as[x[ss[_ Ey]]];exfalso;exact(err_not_good _ _ _ _ _ _ Ey). }
 destruct(filter _ pairs)as[|[[need k]chk]rest]eqn:Bad.
 - injection E as <- _. clear Ep.
   assert(Clean:forall p,In p pairs->let '(a,k,chk):=p in chk&&incoherent e info a k=false).
   { intros[[a k]chk]Hin. destruct(chk&&incoherent e info a k)eqn:B;[|reflexivity].
     assert(In((a,k),chk)(filter(fun p=>let '(a,k,chk):=p in chk&&incoherent e info a k)pairs)).
     { apply filter_In;auto. } rewrite Bad in H;contradiction. }
   clear Bad. revert Clean. induction HP as[|need[[a k]chk]ns pairs Hhead HP IH];intro Clean;cbn;[repeat split;constructor|].
   unfold P in Hhead. destruct Hhead as[->[-> [K Ch]]].
   specialize(IH ltac:(intros p H;apply Clean;right;exact H)).
   destruct IH as[Eq[Pr Co]]. split;[f_equal;exact Eq|]. split;constructor;auto.
   specialize(Clean _ (or_introl eq_refl)). rewrite Ch in Clean.
   destruct need;cbn in *;auto.
 - apply bind_inv in E as[x[ss[_ E]]];exfalso;exact(err_not_good _ _ _ _ _ _ E).
Qed.
Theorem require_needs_exact : forall mi e g info s ps s',
 Inv s->require_needs mi e g info s=Good(ps,s')->
 ps=map(fun need=>(need,site_key e need))(fi_needs info) /\
 Forall(fun need=>key_present e(site_key e need)=true)(fi_needs info) /\
 Forall(fun need=>match need with KyFun _=>True|_=>incoherent e info need(site_key e need)=false end)(fi_needs info).
Proof.
 intros mi e g info s ps s' Hi E.
 destruct(require_needs_all _ _ _ _ _ _ _ Hi E)as[Pairs[Present All]].
 split;[exact Pairs|]. split;[exact Present|].
 eapply Forall_impl;[|exact All]. intros k H;destruct k;auto.
Qed.
Lemma existsb_false_member : forall X(f:X->bool)l,
 existsb f l=false->forall x,In x l->f x=false.
Proof.
 intros X f l;induction l as[|a l IH];intros E x Hx;[contradiction|].
 cbn in E. apply orb_false_iff in E as[Ea El]. destruct Hx as[<-|Hx];auto.
Qed.
Lemma incoherent_some : forall e info need k have stamp t y,
 incoherent e info need k=false ->get_app e k=Some(have,stamp)->
 In(t,Some y)(opt_default[](assoc key_eqb need(fi_apps info)))->
 In(t,Some(instantiate e y))have.
Proof.
 intros e info need k have stamp t y E Eh Hin. unfold incoherent in E. rewrite Eh in E.
 pose proof(existsb_false_member _ _ _ E _ Hin)as Test.
 cbn[fst snd]in Test. apply negb_false_iff in Test.
 unfold oety_eqb in Test. destruct(assoc Nat.eqb t have)as[[h|]|]eqn:Ha;try discriminate Test.
 apply ety_eqb_true in Test. subst h.
 clear E Eh Hin. induction have as[|[n v]have IH];cbn[assoc]in Ha;[discriminate|].
 destruct(Nat.eqb t n)eqn:En.
 - apply Nat.eqb_eq in En. subst n. injection Ha as <-. left;reflexivity.
 - right;exact(IH Ha).
Qed.

Section Coherence.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma incoherent_semantic : forall e info need k have stamp c F C,
 CxRel w R Fl c e F C ->incoherent e info need k=false ->
 get_app e k=Some(have,stamp)->
 forall t want,In(t,Some want)
 (map(fun q=>(fst q,option_map(tr Fl)(snd q)))
 (opt_default[](assoc key_eqb need(fi_apps info))))->
 In(t,Some(Audit.Elaboration.canon C(Audit.app_subst(Audit.Elaboration.c_ty C)want)))
 (map(fun q=>(fst q,option_map(tr Fl)(snd q)))have).
Proof.
 intros e info need k have stamp c F C Hc E Eh t want Hin.
 apply in_map_iff in Hin as[[n [y|]][Eq Hin]];cbn[fst snd option_map]in Eq;[|discriminate].
 injection Eq as <- <-.
 pose proof(incoherent_some _ _ _ _ _ _ _ _ E Eh Hin)as Hhave.
 rewrite <-(instantiate_canonical w R Fl c e F C Hc y),
   (instantiate_normal e y(proj1(cr_uf _ _ _ _ _ _ _ Hc))).
 apply in_map_iff. exists(n,Some(instantiate e y));split;[reflexivity|exact Hhave].
Qed.
End Coherence.

Section Caller.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Lemma caller_types_ok : forall c e F C,CxRel w R Fl c e F C ->
 UfOK(en_uf e)/\(forall g y,tn_get e g=Some y->ety_ok y=true).
Proof.
 intros c e F C Hc. pose proof(loaded_record_unowned w R Fl b Hload)as Rec.
 split.
 - intros g y Hin. eapply support_clean;[exact Rec| |].
   + exact(@es_uf _ _ (cr_support _ _ _ _ _ _ _ Hc) _ _ Hin).
   + exact(proj2(cr_uf _ _ _ _ _ _ _ Hc) _ _ Hin).
 - intros g y Eg. eapply support_clean;[exact Rec| |].
   + exact(@tn_get_support _ _ _ _ (cr_support _ _ _ _ _ _ _ Hc)Eg).
   + exact(cr_ty_wf _ _ _ _ _ _ _ Hc _ _ Eg).
Qed.
Lemma site_key_clean : forall c e F C k,CxRel w R Fl c e F C ->
 key_ok k=true->key_ok(site_key e k)=true.
Proof.
 intros c e F C k Hc Ok. destruct k;try reflexivity.
 cbn[key_ok site_key]in *. destruct(caller_types_ok _ _ _ _ Hc)as[U T].
 apply canon_ok;[exact U|]. exact(instantiate_ok e U T e0 Ok).
Qed.
Lemma key_present_site_provides : forall c e F C k,
 CxRel w R Fl c e F C ->key_ok k=true->key_present e(site_key e k)=true->
 Audit.Elaboration.provides C(Audit.Elaboration.at_site C(key_tr Fl k)).
Proof.
 intros c e F C k Hc Ok Present.
 rewrite <-(key_tr_site w R Fl c e F C k Hc).
 destruct k as[v|f|r m|u];cbn[site_key key_tr Audit.Elaboration.provides key_present]in *.
 - rewrite(cr_vals _ _ _ _ _ _ _ Hc). destruct(nget(en_vals e)v);[discriminate|discriminate Present].
 - rewrite(cr_fns _ _ _ _ _ _ _ Hc). destruct(nget(en_fns e)f);[discriminate|discriminate Present].
 - pose proof(site_key_clean c e F C(KyMth r m)Hc Ok)as Clean.
   cbn[site_key key_ok]in Clean.
   pose proof(cr_mths _ _ _ _ _ _ _ Hc(canon e(instantiate e r))m)as Mh.
   repeat rewrite canon_idem in Mh by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
   specialize(Mh Clean). rewrite Mh.
   destruct(get_mth e(canon e(instantiate e r))m);[discriminate|discriminate Present].
 - pose proof(cr_funspresent _ _ _ _ _ _ _ Hc u)as Fu.
   rewrite Present in Fu. destruct(Audit.Elaboration.nlook u(Audit.Elaboration.c_funs C));[discriminate|discriminate Fu].
Qed.
Lemma caller_key_apps : forall c e F C k,
 CxRel w R Fl c e F C ->key_ok k=true->
 match site_key e k with KyMth r m=>
 Audit.Elaboration.ilook(Audit.Elaboration.mkey(tr Fl r)m)(Audit.Elaboration.c_apps C)=
 option_map(fun p=>map(fun q=>(fst q,option_map(tr Fl)(snd q)))(fst p))(get_app e(site_key e k))
 |_=>True end.
Proof.
 intros c e F C k Hc Ok. destruct k;try exact I.
 exact(cr_apps _ _ _ _ _ _ _ Hc _ _ (site_key_clean c e F C(KyMth e0 n)Hc Ok)).
Qed.
End Caller.
