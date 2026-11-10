(* The target provision in a first forwarder walk has an identity application. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy BodyTr
 CanonTypes CanonBodyTy CtxWalk SigSound InfoCache ForwarderPrototypeSeed FnSnapshotAlign.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section PrototypeFn.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem add_forwarder_function:forall fuel e mi target needs s e' needs' final n ps rt body,
 PrototypeSeed e(R(gs_id target))->nget(w_syms w)(gs_id target)=Some target->
 gs_kind target=KFn->gs_decl target=ADFn RPlain n ps rt body->
 Inv s->st_w s=w->Later w P R Fl final->
 add_symbol_item fuel e mi target[]needs s=Good((e',needs'),final)->
 exists sg stamp,
 e'=set_app(set_fn e(gs_id target)sg)(KyFn(gs_id target))
  (map(fun r=>(r,Some(YAbs r)))(R(gs_id target)))stamp /\
 A.SigOf WW(gs_id target)(requirement_snapshot(R(gs_id target)))None(tr_sig Fl sg) /\
 needs'=add_need needs(KyFn(gs_id target)).
Proof.
 intros[|fuel]e mi target needs s e' needs' final n ps rt body Seed Loaded Kind Decl Hi Hw L Run;
 [discriminate|].
 cbn[add_symbol_item]in Run;apply bind_inv in Run as[u[s1[Check Run]]].
 destruct(ok_check_totality mi target[]e _ _ _ Hi Check)as[I1 F1].
 rewrite Kind in Run;cbn[app]in Run.
 apply bind_inv in Run as[sg[s2[Signature Run]]].
 assert(Osig:ok(fn_sig target(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Osig _ _ _ I1 Signature)as[I2 F2].
 rewrite(seed_fns _ _ Seed)in Run;cbn[nget mbind ret]in Run.
 apply bind_inv in Run as[a[s3[Application Run]]].
 assert(Oapp:ok(provision_app target(tymap e)false))by(unfold provision_app;okt).
 destruct(Oapp _ _ _ I2 Application)as[I3 F3].
 cbn[is_memory_instruction mbind ret]in Run.
 apply bind_inv in Run as[ms[s4[Memory Run]]].
 assert(Om:ok memory_sym)by(unfold memory_sym;okt).
 destruct(Om _ _ _ I3 Memory)as[I4 F4].
 apply bind_inv in Run as[eu[su[Uapp Run]]].
 unfold unify_app in Uapp;unfold get_app in Uapp.
 cbn[set_fn en_apps]in Uapp;rewrite(seed_apps _ _ Seed)in Uapp;cbn[assoc]in Uapp;injection Uapp as <- <-.
 apply bind_inv in Run as[stored[s5[Store Run]]];injection Run as <- <- <-.
 unfold setdefault_app in Store;unfold get_app in Store.
 cbn[set_fn en_apps]in Store;rewrite(seed_apps _ _ Seed)in Store;cbn[assoc]in Store.
 apply bind_inv in Store as[stamp[s6[Fresh Store]]];injection Store as <- <-.
 assert(Ostore:ok(setdefault_app(set_fn e(gs_id target)sg)(KyFn(gs_id target))a))
 by(unfold setdefault_app;okt).
 assert(After:Fr s3 s6).
 {eapply fr_trans;[exact F4|]. unfold fresh in Fresh;destruct(ok_fresh _ _ _ I4 Fresh)as[Ifresh Ffresh];exact Ffresh. }
 unfold provision_app in Application;apply bind_inv in Application as[rs[sr[Req Application]]].
 cbn[mbind ret]in Application;injection Application as <- <-.
 assert(Rs:R(gs_id target)=rs).
 {eapply(reqs_R w (P:=P) R Fl);[exact I2|exact Req|].
  exact(later_back w P R Fl _ _ After L). }
 subst rs.
 assert(Map:map(fun t=>(t,nassoc t(tymap e)))(R(gs_id target))=
  map(fun t=>(t,Some(YAbs t)))(R(gs_id target))).
 {apply map_ext_in;intros r InR;f_equal;rewrite nassoc_tymap.
  destruct(proj2(seed_domain _ _ Seed r)InR)as[y Read];rewrite Read.
  f_equal;exact(seed_tn_read _ _ _ _ Seed Read). }
 rewrite Map.
 exists sg,stamp;split;[reflexivity|]. split;[|reflexivity].
 exact(proj1(fn_sig_rel_at w (P:=P) R Fl b Hload target(tymap e)
 (requirement_snapshot(R(gs_id target)))e None s1 sg s2 RPlain n ps rt body Signature I1
 ltac:(rewrite(fr_w _ _ F1);exact Hw)Loaded Decl(seed_tmrel Fl _ _ Seed)I
 (later_back w P R Fl _ _ (fr_trans _ _ _ F3 After)L))).
Qed.
End PrototypeFn.
