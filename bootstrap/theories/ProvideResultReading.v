(* The actual result bracket reader, retaining type/dependency checks while
   allowing bindings that the result target itself does not mention. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTy.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Reading.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Lemma output_subst_rel:forall a e mi outer s subst0 s' tm ob,
 (match a with
 |Some bs=>let!l:=app_subst 200 e mi bs outer in ret(l++outer)
 |None=>ret outer end)s=Good(subst0,s')->
 Inv s ->st_w s=w ->In mi(w_order w)->Later w P R Fl s' ->
 TmRel Fl(outer++tymap e)(ob++tm)e ->
 map(trp Fl)outer=ob ->Forall(fun q=>WFty(snd q))outer ->
 exists bsr,map(trp Fl)subst0=bsr++ob /\
 Forall(fun q=>WFty(snd q))subst0 /\
 A.OutputBrackets WW(gm_path(get_mod w mi))(ob++tm)a bsr.
Proof.
 intros a e mi outer s subst0 s' tm ob E Hi Hw Hmi L T O W.
 destruct a as[bs|].
 - apply bind_inv in E as(l&sl&El&E). injection E as <- <-.
   destruct(proj1(proj2(ty_all w R Fl b Hload 200))
     e mi bs outer s l sl(ob++tm)El Hi Hw Hmi T L)as(lr&Rel&Eq&Wl).
   exists(rev lr);split;[rewrite map_app,Eq,O;reflexivity|].
   split;[apply Forall_app;auto|apply A.OBrSome;exact Rel].
 - injection E as <- <-. exists[];split;[exact O|split;[exact W|constructor]].
Qed.
End Reading.

Local Opaque reqs type_requirements.
Section Provision.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Definition output_inst(a:Elab.inst):A.inst:=
 map(fun q=>(fst q,option_map(tr Fl)(snd q)))a.
Lemma ordinary_provision_rel:forall g tm tmr e s a s',
 provision_app g tm false s=Good(a,s')->Inv s->Later w P R Fl s'->
 TmRel Fl tm tmr e ->A.kind_sym WW(gs_id g)<>Some A.DKMeth ->
 A.AppOf WW(gs_id g)tmr(output_inst a).
Proof.
 intros g tm tmr e s a s' E Hi L T K.
 unfold provision_app in E;apply bind_inv in E as(rs&sr&Er&E).
 cbn beta iota in E;apply bind_inv in E as(rs'&ss&Es&E).
 injection Es as <- <-;injection E as <- <-.
 pose proof(reqs_R w R Fl _ _ _ _ Hi Er L)as Eq.
 exists rs;split;[change(incl rs(R(gs_id g)));rewrite Eq;apply incl_refl|].
 split;[|split;[intros _;symmetry;exact Eq|intro X;contradiction]].
 unfold output_inst;rewrite map_map;apply map_ext;intro t;cbn[fst snd].
 f_equal;symmetry;apply(proj1 T).
Qed.
End Provision.
Lemma output_provision_ok:forall g tm meth,ok(provision_app g tm meth).
Proof. intros;unfold provision_app;okt. Qed.
Local Opaque provision_app ety_of.

Definition provide_value_code(e:env)(g:gsym)(t:aty)(tm:list(nat*ety))(out:list key)
 :M(env*list key):=
 let!y:=ety_of t(gs_mod g)tm e None in
 let!ep:=provided_at(set_val e(gs_id g)y)(KyVal(gs_id g))g tm false in
 ret(ep,out++[KyVal(gs_id g)]).
Section Value.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Local Opaque reqs type_requirements.
Theorem provide_value_read:forall e g t tm tmr out s e' keys s',
 provide_value_code e g t tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->In(gs_mod g)(w_order w)->Later w P R Fl s'->
 TmRel Fl tm tmr e->A.kind_sym WW(gs_id g)<>Some A.DKMeth->
 exists y a stamp,
 A.TyOf WW(gm_path(get_mod w(gs_mod g)))tmr None t(tr Fl y) /\ WFty y /\
 A.AppOf WW(gs_id g)tmr(output_inst Fl a) /\
 e'=set_app(set_val e(gs_id g)y)(KyVal(gs_id g))(freeze_app(set_val e(gs_id g)y)a)stamp /\
 keys=out++[KyVal(gs_id g)].
Proof.
 intros e g t tm tmr out s e' keys s' E Hi Hw Hmi L T K.
 unfold provide_value_code in E;apply bind_inv in E as(y&sy&Et&E).
 apply bind_inv in E as(ep&se&Ep&E);injection E as <- <- <-.
 unfold provided_at in Ep;apply bind_inv in Ep as(a&sa&Ea&Ep).
 apply bind_inv in Ep as(stamp&ss&Es&Ep);injection Ep as <- <-.
 destruct(ok_ety_of t(gs_mod g)tm e None _ _ _ Hi Et)as[Iy Fy].
 pose proof(output_provision_ok g tm false)as Oa.
 destruct(Oa _ _ _ Iy Ea)as[Ia Fa].
 destruct(ok_fresh _ _ _ Ia Es)as[_ Fs].
 pose proof(later_back w P R Fl _ _ Fs L)as La.
 pose proof(later_back w P R Fl _ _ Fa La)as Ly.
 destruct(proj1(ty_all w R Fl b Hload 200)t(gs_mod g)tm e None s y sy tmr
   ltac:(now rewrite <-ety_of_eq)Hi Hw Hmi T I Ly)as[Ty Wy].
 exists y,a,stamp;repeat split;try exact Ty;try exact Wy;try reflexivity.
 eapply ordinary_provision_rel;[exact Ea|exact Iy|exact La|exact T|exact K].
Qed.
End Value.
