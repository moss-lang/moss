(* The selected runtime Bool is a supported source nominal. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrame BodySound NominalSupport LibraryFacts.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Definition runtime_symbol_value(s:st)(q:string*string):option nat:=
 match sassoc(fst q)(st_lang s)with
 |None=>None|Some mi=>sassoc(snd q)(gm_names(get_mod(st_w s)mi))end.
Lemma runtime_symbols_exact:forall s,
 runtime_symbols s=Good(map(fun q=>(q,runtime_symbol_value s q))runtime_names,s).
Proof.
 intro s. unfold runtime_symbols.
 set(f:=fun q:string*string => let! lm:=lang(fst q)in match lm with
 |None=>ret(q,None)|Some mi=>let! m:=the_mod mi in ret(q,sassoc(snd q)(gm_names m))end).
 change(mmap f runtime_names s=Good(map(fun q=>(q,runtime_symbol_value s q))runtime_names,s)).
 assert(Hf:forall q,f q s=Good((q,runtime_symbol_value s q),s)).
 { intro q. unfold f,lang,the_mod,get,mbind,ret,runtime_symbol_value.
   destruct(sassoc(fst q)(st_lang s));reflexivity. }
 generalize runtime_names as qs. intro qs. induction qs as[|q qs IH];[reflexivity|].
 cbn[mmap]. unfold mbind at 1. rewrite Hf. cbn[mbind ret]. unfold mbind at 1. rewrite IH. reflexivity.
Qed.
Lemma runtime_bool_id:forall s,
 runtime_id(map(fun q=>(q,runtime_symbol_value s q))runtime_names) "bool" "Bool"=
 runtime_symbol_value s("bool","Bool").
Proof. reflexivity. Qed.

Section Validator.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Ltac impossible H := exfalso; solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(?&?&?&H)])].
Lemma check_runtime_bool_support:forall sc x g s u s',
 runtime_id sc "bool" "Bool"=Some x -> nget(w_syms w)x=Some g ->
 check_runtime_bool sc s=Good(u,s') -> Inv s -> st_w s=w ->
 (forall k rs,nget(st_reqs s')k=Some rs->R k=rs) -> Support WW(YNom x []).
Proof.
 intros sc x g s u s' Eb Hg H Hi Hw Hfinal.
 unfold check_runtime_bool in H. rewrite Eb in H.
 apply bind_inv in H as(g0&s0&Eg&H). apply sym_inv in Eg as[-> Eg].
 pose proof(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)x g ltac:(rewrite Hw;exact Hg))as G.
 rewrite G in Eg. injection Eg as <-.
 apply bind_inv in H as(rs&s1&Er&H).
 destruct(ok_reqs_frame x s rs s1 Hi Er)as[Hi1 Fr].
 assert(Cache:nget(st_reqs s1)x=Some rs).
 { eapply(reqs_cached 200 x s rs s1);[intros g0 G0;exact(iv_ids _ Hi x g0 G0)|rewrite reqs_frame_eq in Er; exact Er]. }
 destruct(runtime_id sc "bool" "True")as[t|],(runtime_id sc "bool" "False")as[f|],rs as[|a rs];try impossible H.
 apply bind_inv in H as(gt&s2&Egt&H). apply sym_inv in Egt as[-> Egt].
 apply bind_inv in H as(gf&s3&Egf&H). apply sym_inv in Egf as[-> Egf].
 destruct(kind_eqb(gs_kind g)KTag&&kind_eqb(gs_kind gt)KUnit&&kind_eqb(gs_kind gf)KUnit&&negb(Nat.eqb t f))eqn:K;[|impossible H].
 apply andb_true_iff in K as[K _]. apply andb_true_iff in K as[K _]. apply andb_true_iff in K as[Kg _].
 apply kind_eqb_spec in Kg.
 apply bind_inv in H as(pay&s4&Epay&H).
 assert(Op:ok(payload_type(YNom x [])empty_env))by(unfold payload_type;okt).
 destruct(Op _ _ _ Hi1 Epay)as[_ Fpay].
 destruct(ety_eqb pay(YUnion[YNom t [];YNom f []])||ety_eqb pay(YUnion[YNom f [];YNom t []]));[|impossible H].
 injection H as <- <-.
 apply SupportNom.
 - right. change(ExecutionModel.Elaboration.kind_sym (W_read w R Fl) x=Some ExecutionModel.Elaboration.DKTag). rewrite(kind_sym_read b w R Fl Hload x g Hg),Kg. reflexivity.
 - cbn. unfold BodySound.W. cbn[ExecutionModel.Elaboration.w_reqs W_read].
   symmetry. apply Hfinal. exact(fr_ext _ _ Fpay x [] Cache).
 - constructor.
Qed.
End Validator.

Theorem selected_runtime_bool_support:forall b w R Fl sc bi x s0 s u s',
 load_world b=Good w -> runtime_symbols s0=Good(sc,s0) ->
 st_w s0=w -> st_lang s0=lang_modules w -> st_w s=w ->
 sassoc "bool"(lang_modules w)=Some bi -> sassoc "Bool"(gm_names(get_mod w bi))=Some x ->
 check_runtime_bool sc s=Good(u,s') -> Inv s ->
 (forall k rs,nget(st_reqs s')k=Some rs->R k=rs) ->
 Support(BodySound.W w R Fl)(YNom x []).
Proof.
 intros b w R Fl sc bi x s0 s u s' Hload Esc Hw0 Hl Hw El Ex H Hi Hfinal.
 rewrite runtime_symbols_exact in Esc. injection Esc as <-.
 assert(Eb:runtime_id(map(fun q=>(q,runtime_symbol_value s0 q))runtime_names) "bool" "Bool"=Some x).
 { rewrite runtime_bool_id. unfold runtime_symbol_value. cbn[fst snd]. rewrite Hl,El,Hw0,Ex. reflexivity. }
 destruct(lang_symbol_scope b w R Fl Hload "bool" false "Bool" bi x El Ex)as(d&p&Hd&_).
 unfold W_read in Hd. cbn[ExecutionModel.Elaboration.w_decl]in Hd.
 destruct(nget(w_syms w)x)as[g|]eqn:Hg;[|discriminate Hd].
 exact(check_runtime_bool_support w R Fl b Hload _ _ _ _ _ _ Eb Hg H Hi Hw Hfinal).
Qed.

Lemma runtime_tag_frame:forall sc n p h,ok(check_runtime_tag sc n p h).
Proof. intros. unfold check_runtime_tag;okt. Qed.
Lemma runtime_abstracts_frame:forall sc,ok(check_runtime_abstracts sc).
Proof. intros. unfold check_runtime_abstracts;okt. Qed.
Theorem check_runtime_library_bool_support:forall b w R Fl s u s',
 load_world b=Good w ->check_runtime_library s=Good(u,s')->
 Inv s->st_w s=w->st_lang s=lang_modules w->
 (forall k rs,nget(st_reqs s')k=Some rs->R k=rs)->
 forall bi x,sassoc "bool"(lang_modules w)=Some bi->
 sassoc "Bool"(gm_names(get_mod w bi))=Some x->Support(BodySound.W w R Fl)(YNom x []).
Proof.
 intros b w R Fl s u s' Hload H Hi Hw Hl Hfinal bi x El Ex.
 unfold check_runtime_library in H.
 apply bind_inv in H as(sc&s0&Esc&H). rewrite runtime_symbols_exact in Esc.
 injection Esc as <- <-.
 apply bind_inv in H as(ua&sa&Ea&H).
 destruct(runtime_abstracts_frame _ _ _ _ Hi Ea)as[Ia Fa].
 apply bind_inv in H as(ub&sb&Eb&H).
 assert(Ob:ok(check_runtime_bool(map(fun q=>(q,runtime_symbol_value s q))runtime_names))).
 { unfold check_runtime_bool;okt. }
 destruct(Ob _ _ _ Ia Eb)as[Ib Fb].
 assert(Otail:ok(check_runtime_tag(map(fun q=>(q,runtime_symbol_value s q))runtime_names) "Ref" "Boxed" "StructRef";;
 check_runtime_tag(map(fun q=>(q,runtime_symbol_value s q))runtime_names) "Array" "Arrayed" "ArrayRef";;
 check_runtime_tag(map(fun q=>(q,runtime_symbol_value s q))runtime_names) "Addr" "Memory" "I32")).
 { apply ok_bind;[apply runtime_tag_frame|intro;apply ok_bind;[apply runtime_tag_frame|intro;apply runtime_tag_frame]]. }
 destruct(Otail _ _ _ Ib H)as[_ Ft].
 eapply(selected_runtime_bool_support b w R Fl _ bi x s sa ub sb Hload (runtime_symbols_exact s) Hw Hl).
 - rewrite(fr_w _ _ Fa). exact Hw.
 - exact El.
 - exact Ex.
 - exact Eb.
 - exact Ia.
 - intros k rs E. apply Hfinal. exact(fr_ext _ _ Ft k rs E).
Qed.
