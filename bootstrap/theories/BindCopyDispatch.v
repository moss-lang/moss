(* A checked copy is dispatched from real loaded abstract declarations. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy
 SignatureLeavesExec FunctorCopyExec BindSequence InfoPreserve.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Definition abstract_functor_id(W:A.world)(home:string)(pa:path):option nat:=
 match A.resolve_sym W home pa with
 |Some u=>match A.ast_of W u with
          |Some(ADFunctor _ _ _ _ None)=>Some u|_=>None end
 |None=>None end.
Definition abstract_copy_bind(W:A.world)(F:A.frame)(sp:spec)(y:aexpr):bool:=
 match sp,y with
 |Spec(Some pa)None None,AEPath pg None=>
   is_some(abstract_functor_id W(A.fr_mod F)pa)&&is_some(abstract_functor_id W(A.fr_mod F)pg)
 |_,_=>false end.
Lemma abstract_functor_id_some:forall W home pa u,
 abstract_functor_id W home pa=Some u ->A.resolve_sym W home pa=Some u /\
 exists n names args res,A.ast_of W u=Some(ADFunctor n names args res None).
Proof.
 intros W home pa u H;unfold abstract_functor_id in H.
 destruct(A.resolve_sym W home pa)as[v|]eqn:R;[|discriminate].
 destruct(A.ast_of W v)as[d|]eqn:D;[|discriminate].
 destruct d;try discriminate.
 match goal with o:option(list(list(spec*option aexpr)))|-_=>destruct o end;[discriminate|].
 injection H as <-;split;[reflexivity|]. repeat eexists;exact D.
Qed.
Section Copy.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Theorem check_bind_abstract_copy:forall fuel c e pa pg s xs e' c' s' F C,
 A.WorldOf b WW ->
 check_bind fuel c e(Spec(Some pa)None None)(AEPath pg None)s=Good((xs,e',c'),s') ->
 Inv s ->CxRel w R Fl c e F C ->Later w P R Fl s' ->
 abstract_copy_bind WW F(Spec(Some pa)None None)(AEPath pg None)=true ->
 c'=c /\exists C',CxRel w R Fl c' e' F C' /\
 A.BindItem WW P F C(Spec(Some pa)None None,Some(AEPath pg None))
 (fun tail=>fold_stmts xs tail)C' F[].
Proof.
 intros fuel c e pa pg s xs e' c' s' F C Wo E Hi Hc L Class.
 destruct(ok_check_bind fuel c e(Spec(Some pa)None None)(AEPath pg None)_ _ _ Hi E)as[_ Frun].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Frun);exact(lt_w _ _ _ _ _ L)).
 unfold abstract_copy_bind in Class;apply andb_true_iff in Class as[FClass GClass].
 destruct(abstract_functor_id WW(A.fr_mod F)pa)as[u|]eqn:Fu;[|discriminate].
 destruct(abstract_functor_id WW(A.fr_mod F)pg)as[h|]eqn:Hu;[|discriminate].
 destruct(abstract_functor_id_some _ _ _ _ Fu)as[PathF(fn&fps&fargs&fres&Fdecl)].
 destruct(abstract_functor_id_some _ _ _ _ Hu)as[PathH(gn&gps&gargs&gres&Gdecl)].
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
 assert(GD:gs_decl bg=ADFunctor gn gps gargs gres None).
 {unfold A.ast_of in Gdecl;rewrite ModelG in Gdecl;injection Gdecl as <-;reflexivity. }
 pose proof(load_world_ids b w Hload _ _ Fg)as IdFg.
 assert(Fg':nget(w_syms w)(gs_id fg)=Some fg)by(now rewrite IdFg).
 destruct(abstract_copy_exec_code w (P:=P) R Fl b Hload (S fuel)c e fg bg fn fps fargs fres gn gps gargs gres
    pa pg s ys ey s3 F C Wo Fg' FD Bg GD Lookup
    ltac:(now rewrite IdFg)Whole Hi Hw L Hc)as(C'&wrap&B&Out&Fold&sm&Code&Wrap).
 split;[reflexivity|]. exists C';split;[exact Out|].
 subst ys wrap;exact B.
Qed.
End Copy.

Section Residual.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Definition OtherBindStep(f:nat):Prop:=forall c e sp y s xs e' c' s',
 check_bind f c e sp y s=Good((xs,e',c'),s')->forall F C,
 simple_bind(BodySound.W w R Fl)F sp y=0 ->
 abstract_copy_bind(BodySound.W w R Fl)F sp y=false ->
 Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->
 BindResult w P R Fl c e F C(sp,Some y)xs e' c'.
Theorem residual_after_abstract_copy:forall f,OtherBindStep f->ResidualBindStep w P R Fl f.
Proof.
 intros f Other c e sp y s xs e' c' s' E F C Simple Hi Ii Ifwd Hc Hp.
 destruct(abstract_copy_bind(BodySound.W w R Fl)F sp y)eqn:Class.
 - destruct sp as[[pa|][dn|][br|]];cbn[abstract_copy_bind]in Class;try discriminate.
   destruct y;try discriminate Class.
   match goal with a:sapp|-_=>destruct a end;try discriminate Class.
   destruct(check_bind_abstract_copy w (P:=P) R Fl b Hload f c e pa p s xs e' c' s' F C
     Hworld E Hi Hc(proj1 Hp)Class)as[Ec[C'[Out Bind]]].
   eapply bind_result_same;[exact Ec|exact Out|exact Bind].
 - exact(Other c e sp y s xs e' c' s' E F C Simple Class Hi Ii Ifwd Hc Hp).
Qed.
End Residual.
