From NM Require Import BodyProofTools.
(* Actual value provisions retain the body context and checked application. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore
 NominalSupport NominalMergeSupport CanonTypes CtxWalk.
From NM Require Import InfoPreserve.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Lemma get_app_value_other : forall e v a stamp r m,
 get_app(set_app e(KyVal v)a stamp)(KyMth r m)=get_app e(KyMth r m).
Proof.
 intros e v a stamp r m. unfold get_app,set_app. cbn[en_apps].
 induction(en_apps e)as[|[k p]l IH];[reflexivity|]. destruct k;cbn[aset key_eqb assoc];
 try exact IH.
 - destruct(Nat.eqb v n);[reflexivity|exact IH].
 - destruct(Nat.eqb m n&&ety_eqb r e0);[reflexivity|exact IH].
Qed.
Lemma get_app_value_fun:forall e v ty raw stamp u,
 get_app(set_app(set_val e v ty)(KyVal v)raw stamp)(KyFun u)=get_app e(KyFun u).
Proof.
 intros;unfold get_app,set_app,set_val;cbn[en_apps].
 rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec;reflexivity.
Qed.
Lemma env_support_app : forall W e k a stamp,
 EnvSupport W e ->EnvSupport W(set_app e k a stamp).
Proof. intros W e k a stamp[T U V F M L];constructor;auto. Qed.
Section Context.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Lemma cxrel_value : forall c e F C v ty raw app stamp,
 CxRel w R Fl c e F C ->WFty ty ->Support WW ty ->
 app=map(fun q=>(fst q,option_map(tr Fl)(snd q)))raw ->
 CxRel w R Fl c(set_app(set_val e v ty)(KyVal v)raw stamp)F
  (A.provide C(IVal v)app(A.set_val C v(tr Fl(canon e ty)))).
Proof.
 intros c e F C v ty raw app stamp H Wt St Eapp.
 pose proof(cr_vals _ _ _ _ _ _ _ H)as Vlook.
 pose proof(cr_vals_wf _ _ _ _ _ _ _ H)as Vwf.
 pose proof(cr_apps _ _ _ _ _ _ _ H)as Apps.
 pose proof(cr_vapps _ _ _ _ _ _ _ H)as Va.
 pose proof(cr_fapps _ _ _ _ _ _ _ H)as Fa.
 pose proof(cr_funapps _ _ _ _ _ _ _ H)as Fu.
 pose proof(cr_funfixed _ _ _ _ _ _ _ H)as Fx.
 pose proof(cr_support _ _ _ _ _ _ _ H)as Es.
 destruct H. constructor;auto.
 - intro x. cbn[A.provide A.set_val A.c_vals A.nlook]. unfold Elab.set_app,Elab.set_val. cbn[en_vals].
   rewrite nget_nset. destruct(Nat.eqb x v);cbn[option_map];[reflexivity|exact(Vlook x)].
 - intros x y Ey. unfold Elab.set_app,Elab.set_val in Ey. cbn[en_vals]in Ey.
   rewrite nget_nset in Ey. destruct(Nat.eqb x v);[injection Ey as <-;exact Wt|exact(Vwf _ _ Ey)].
 - apply env_support_app. exact(env_support_val v Es St).
 - intros r m Clean. cbn[A.provide A.c_apps A.ilook A.mkey ExecutionModel.item_eqb].
   rewrite get_app_value_other. exact(Apps r m Clean).
 - intro x. cbn[A.provide A.c_apps A.ilook ExecutionModel.item_eqb].
   unfold get_app,Elab.set_app. cbn[en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec.
   cbn[key_eqb]. destruct(Nat.eqb x v);cbn[option_map fst];[rewrite Eapp;reflexivity|exact(Va x)].
 - intro x. cbn[A.provide A.c_apps A.ilook ExecutionModel.item_eqb].
   unfold get_app,Elab.set_app. cbn[en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec.
   cbn[key_eqb]. exact(Fa x).
 - intro u;cbn[A.provide A.c_apps A.ilook ExecutionModel.item_eqb].
   rewrite get_app_value_fun;exact(Fu u).
 - intros u fx x tx Lookup Hin.
   cbn[A.provide A.set_val A.c_funs]in Lookup.
   destruct(Fx u fx x tx Lookup Hin)as[Fixed(a&stamp0&raw0&Ga&Gx&Eq)].
   split;[exact Fixed|]. exists a,stamp0,raw0;split;[rewrite get_app_value_fun;exact Ga|split;assumption].
Qed.
End Context.

From NM Require Import BodyTy CanonBodyTy SourceSupport.
Definition bind_inst(Fl:string->nat)(e:env)(a:Elab.inst):A.inst :=
 map(fun p=>(fst p,option_map(tr Fl)(snd p)))(freeze_app e a).
Section AppReading.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Lemma provision_app_canonical : forall c e F C g s a s',
 provision_app g(tymap e)false s=Good(a,s') ->Inv s ->
 CxRel w R Fl c e F C ->Later w P R Fl s' ->
 A.kind_sym WW(gs_id g)<>Some A.DKMeth ->
 A.AppOf WW(gs_id g)(A.c_ty C)(bind_inst Fl e a).
Proof.
 intros c e F C g s a s' E Hi Hc L Nk.
 unfold provision_app in E. apply bind_inv in E as(rs & s1 & Er & E).
 cbn beta iota in E. apply bind_inv in E as(rs0 & s2 & Eret & E).
 injection Eret as <- <-. injection E as <- <-.
 pose proof(reqs_R w R Fl _ _ _ _ Hi Er L)as Req.
 exists rs.
 split.
 - unfold bind_inst,freeze_app. rewrite !map_map. apply map_ext. intro r. cbn[fst snd].
   f_equal. rewrite(cr_ty _ _ _ _ _ _ _ Hc),<-nassoc_tymap.
   destruct(nassoc r(tymap e));reflexivity.
 - split;[intros _;exact(eq_sym Req)|intro K;contradiction].
Qed.
End AppReading.

Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Ltac noerr_value H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Section Execution.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem check_bind_value_sound_full : forall f,OK w P R Fl f ->
 forall c e pp y s xs e' c' s' F C,
 check_bind(S f)c e(Spec(Some pp)None None)y s=Good((xs,e',c'),s') ->
 Inv s ->InfoInv s -> FwdInv s ->CxRel w R Fl c e F C ->Post w P R Fl s' F ->
 (exists v,A.resolve_sym WW(A.fr_mod F)pp=Some v /\A.kind_sym WW v=Some A.DKVal) ->
 c'=c /\ exists C',CxRel w R Fl c' e' F C' /\
 A.BindItem WW P F C(Spec(Some pp)None None,Some y)(fun rest=>fold_stmts xs rest)C' F [].
Proof.
 intros f IH c e pp y s xs e' c' s' F C E Hi Ii Ifwd Hc Hp(v0 & Hrs & Hkv).
 destruct(ok_check_bind(S f)c e(Spec(Some pp)None None)y _ _ _ Hi E)as[_ Fs].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Fs);exact(lt_w _ _ _ _ _(proj1 Hp))).
 cbn[check_bind]in E. apply bind_inv in E as(r & s1 & Er & E).
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Er)as[-> Hres].
 destruct r as[[v|mi]|];[|noerr_value E|noerr_value E].
 cbn[option_map ent_of]in Hres. apply bind_inv in E as(g & s2 & Eg & E).
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Hres Eg)as[-> Hg].
 assert(Ev:v0=v).
 { unfold A.resolve_sym in Hrs. rewrite(cr_mod _ _ _ _ _ _ _ Hc),Hres in Hrs. injection Hrs as <-;reflexivity. }
 subst v0. pose proof(kind_sym_read b w R Fl Hload v g Hg)as Kd.
 change(A.kind_sym WW v=Some(kconv(gs_kind g)))in Kd.
 assert(Kg:gs_kind g=KVal)by(rewrite Kd in Hkv;destruct(gs_kind g);try discriminate Hkv;reflexivity).
 rewrite Kg in E. destruct(gs_decl g)as[asm ds|nt|na aty0|nt ps body|nu|nv aty0 init|recv nf ps rt body|nc its|nf pp0 args res body]eqn:D;try noerr_value E.
 apply bind_inv in E as(declared & s2 & Et & E).
 destruct(ok_ety_of _ _ _ _ _ _ _ _ Hi Et)as[I2 F2].
 apply bind_inv in E as(res & s3 & Ey & E). destruct res as[x vt].
 destruct(ok_synth f c e y(Some declared)_ _ _ I2 Ey)as[I3 F3].
 destruct(negb(fits e vt declared))eqn:Efit;[noerr_value E|]. apply negb_false_iff in Efit.
 apply bind_inv in E as(eout & s4 & Eprov & E).
 apply bind_inv in E as(umeta & se & Eevent & E). injection E as <- <- <- <-.
 assert(Op:ok(provided_at(set_val e v declared)(KyVal v)g(tymap(set_val e v declared))false)).
 { unfold provided_at,provision_app;okt. }
 destruct(Op _ _ _ I3 Eprov)as[Iout F4].
 match type of Eevent with ?op _ = _ =>assert(Oevent:ok op)by(unfold record_reify_event;apply ok_reify_update)end.
 destruct(Oevent _ _ _ Iout Eevent)as[_ Fevent].
 pose proof(post_back w P R Fl _ _ F F4(post_back w P R Fl _ _ F Fevent Hp))as Hpy.
 pose proof(later_back w P R Fl _ _ F3(proj1 Hpy))as L2.
 destruct(ety_of_canonical w R Fl b Hload _ _ _ _ _ _ _ _ Et Hi Hw
   (loader_mod_in w R Fl b Hload v g Hg)(proj1(cr_uf _ _ _ _ _ _ _ Hc))
   (cr_ty_wf _ _ _ _ _ _ _ Hc)I(cr_ty _ _ _ _ _ _ _ Hc)L2)as[Ty Wd].
 assert(Sd:Support WW declared).
 { eapply ety_of_support_at;[exact Hload|exact Et|exact Hi|exact Hw|exact(loader_mod_in w R Fl b Hload v g Hg)| | |exact(cr_support _ _ _ _ _ _ _ Hc)|exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|exact I|exact L2].
   - intros k ty Ek. rewrite nassoc_tymap in Ek. exact(@tn_get_support WW e k ty(cr_support _ _ _ _ _ _ _ Hc)Ek).
   - intros k ty Ek. rewrite nassoc_tymap in Ek. exact(cr_ty_wf _ _ _ _ _ _ _ Hc k ty Ek). }
 destruct(bs_synth _ _ _ _ _ IH c e y(Some declared)s2 x vt s3 Ey F C I2 ltac:(cache_inv) ltac:(fwd_inv) Hc Wd Sd Hpy)as[Ex[Wv Sv]].
 unfold provided_at in Eprov. apply bind_inv in Eprov as(a & sa & Ea & Eprov).
 apply bind_inv in Eprov as(stamp & s5 & Efresh & Eprov). injection Eprov as <- <-.
 assert(Oa:ok(provision_app g(tymap(set_val e v declared))false))by(unfold provision_app;okt).
 destruct(Oa _ _ _ I3 Ea)as[I4 Fa].
 pose proof(later_back w P R Fl _ _ (proj2(ok_fresh _ _ _ I4 Efresh))(later_back w P R Fl _ _ Fevent(proj1 Hp)))as La.
 pose proof(load_world_ids b w Hload v g Hg)as Eid.
 pose proof(provision_app_canonical w R Fl c e F C g s3 a sa Ea I3 Hc La
   ltac:(rewrite Eid,Hkv;discriminate))as App.
 rewrite Eid in App.
 split;[reflexivity|].
 exists(A.provide C(IVal v)(bind_inst Fl e a)(A.set_val C v(tr Fl(canon e declared)))). split.
 - exact(cxrel_value w R Fl _ _ _ _ v declared _ _ stamp Hc Wd Sd eq_refl).
 - cbn[fold_stmts]. eapply A.BValBind with(d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g));
   [exact Hrs|exact(w_decl_read w R Fl v g Hg)|exact D|exact Ty|exact Ex|
    exact(fits_tr Fl e vt declared(cr_uf _ _ _ _ _ _ _ Hc)Efit)|exact App].
Qed.
Theorem check_bind_value_sound : forall f,OK w P R Fl f ->
 forall c e pp y s xs e' c' s' F C,
 check_bind(S f)c e(Spec(Some pp)None None)y s=Good((xs,e',c'),s') ->
 Inv s ->InfoInv s -> FwdInv s ->CxRel w R Fl c e F C ->Post w P R Fl s' F ->
 (exists v,A.resolve_sym WW(A.fr_mod F)pp=Some v /\A.kind_sym WW v=Some A.DKVal) ->
 exists C',CxRel w R Fl c' e' F C' /\
 A.BindItem WW P F C(Spec(Some pp)None None,Some y)(fun rest=>fold_stmts xs rest)C' F [].
Proof.
 intros f IH c e pp y s xs e1 c1 s1 F C E Hi Ii Ifwd Hc Hp Hk.
 exact(proj2(check_bind_value_sound_full f IH c e pp y s xs e1 c1 s1 F C E Hi Ii Ifwd Hc Hp Hk)).
Qed.
End Execution.
