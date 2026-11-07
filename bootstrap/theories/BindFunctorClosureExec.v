(* Source dispatch for the single remaining defined-provider closure case. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 SignatureLeavesExec FunctorClosureExec BindSequence InfoPreserve.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Section Copy.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem check_bind_defined_closure:forall fuel c e pa pg s xs e' c' s' F C u h
 fn fps fargs fres gn gps gargs gres bodies,
 A.WorldOf b WW ->
 check_bind fuel c e(Spec(Some pa)None None)(AEPath pg None)s=Good((xs,e',c'),s') ->
 Inv s ->CxRel w R Fl c e F C ->Later w P R Fl s' ->
 A.resolve_sym WW(A.fr_mod F)pa=Some u -> A.resolve_sym WW(A.fr_mod F)pg=Some h ->
 A.ast_of WW u=Some(ADFunctor fn fps fargs fres None) ->
 A.ast_of WW h=Some(ADFunctor gn gps gargs gres(Some bodies)) ->
 c'=c /\ exists C' code before after fg bg,
 nget(w_syms w)u=Some fg /\ nget(w_syms w)h=Some bg /\
 link_body(pred(pred fuel))fg bg before=Good(code,after) /\
 Inv before /\ Later w P R Fl after /\ CxRel w R Fl c' e' F C' /\
 (A.LinkBody WW P u h code ->
 A.BindItem WW P F C(Spec(Some pa)None None,Some(AEPath pg None))
   (fun tail=>fold_stmts xs tail)C' F[]).
Proof.
 intros fuel c e pa pg s xs e' c' s' F C u h fn fps fargs fres gn gps gargs gres bodies
 Wo E Hi Hc L PathF PathH Fdecl Gdecl.
 destruct(ok_check_bind fuel c e(Spec(Some pa)None None)(AEPath pg None)_ _ _ Hi E)as[_ Frun].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Frun);exact(lt_w _ _ _ _ _ L)).
 destruct fuel as[|fuel];[discriminate E|]. cbn[check_bind]in E.
 apply bind_inv in E as(r&s1&Path&E).
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Path)as[-> Res].
 destruct r as[[t|modid]|];try(solve[exfalso;repeat apply bind_inv in E as(?&?&?&E);discriminate E]).
 cbn[option_map ent_of]in Res;apply bind_inv in E as(fg&s2&Sym&E).
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Res Sym)as[-> Fg].
 assert(IdF:u=t).
 {unfold A.resolve_sym in PathF;rewrite(cr_mod _ _ _ _ _ _ _ Hc),Res in PathF;congruence. }
 subst u.
 assert(ModelF:A.w_decl WW t=Some(A.Decl(gm_path(get_mod w(gs_mod fg)))(gs_decl fg)(gs_assumes fg)))
   by exact(w_decl_read w R Fl t fg Fg).
 assert(FD:gs_decl fg=ADFunctor fn fps fargs fres None).
 {unfold A.ast_of in Fdecl;rewrite ModelF in Fdecl;injection Fdecl as <-;reflexivity. }
 assert(Kind:gs_kind fg=KFunctor).
 {pose proof(signature_loaded_kind w R Fl b Hload _ _ Fg)as K;unfold KindOK in K;rewrite FD in K;
  destruct(gs_kind fg);cbn[A.kind_of kconv]in K;congruence. }
 rewrite Kind in E;apply bind_inv in E as([ys ey]&s3&Bind&E);injection E as <- <- <- <-.
 pose proof Bind as Whole.
 destruct fuel as[|fuel];[discriminate Bind|]. cbn[bind_functor]in Bind;unfold functor_parts in Bind;rewrite FD in Bind.
 apply bind_inv in Bind as(bgopt&s0&Lookup&Rest).
 destruct(esym_rel w R Fl b Hload(cx_mod c)pg s bgopt s0 Hi Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Lookup)as[-> Read].
 destruct bgopt as[bg|];[|discriminate Rest]. destruct Read as[PathBg Bg].
 assert(IdH:h=gs_id bg)by(rewrite(cr_mod _ _ _ _ _ _ _ Hc),PathBg in PathH;congruence).
 subst h.
 assert(ModelG:A.w_decl WW(gs_id bg)=Some(A.Decl(gm_path(get_mod w(gs_mod bg)))(gs_decl bg)(gs_assumes bg)))
   by exact(w_decl_read w R Fl _ _ Bg).
 assert(GD:gs_decl bg=ADFunctor gn gps gargs gres(Some bodies)).
 {unfold A.ast_of in Gdecl;rewrite ModelG in Gdecl;injection Gdecl as <-;reflexivity. }
 pose proof(load_world_ids b w Hload _ _ Fg)as IdFg.
 assert(Fg':nget(w_syms w)(gs_id fg)=Some fg)by(now rewrite IdFg).
 destruct(defined_closure_exec_code w (P:=P) R Fl b Hload (S fuel)c e fg bg fn fps fargs fres gn gps gargs gres bodies
    pa pg s ys ey s3 F C Wo Fg' FD Bg GD Lookup
    ltac:(now rewrite IdFg)Whole Hi Hw L Hc)as(C'&code&before&after&Linked&ValidBefore&LastAfter&Out&Typed).
 split;[reflexivity|]. exists C',code,before,after,fg,bg.
 split;[exact Fg|];split;[exact Bg|];split;[exact Linked|].
 split;[exact ValidBefore|];split;[exact LastAfter|];split;[exact Out|].
 intro Body;apply Typed;now rewrite IdFg.
Qed.
End Copy.
