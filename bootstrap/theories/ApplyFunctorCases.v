From NM Require Import WrapperKeyReadonly.
(* The actual application dispatcher resolves a loaded functor and preserves its inputs. *)
From Stdlib Require Import List String Bool.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr
 InfoPreserve FwdInvariant FwdReadonly.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Cases.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem apply_functor_cases:forall f c e sp s xs e' c' sf F C,
 apply_functor(S f)c e sp s=Good((xs,e',c'),sf)->
 Inv s->InfoInv s->FwdInv s->st_w s=w->CxRel w R Fl c e F C->
 exists pa g n names args res body sa,
 sp=Spec(Some pa)None None/\A.resolve_sym W(A.fr_mod F)pa=Some(gs_id g)/\
 nget(w_syms w)(gs_id g)=Some g/\gs_decl g=ADFunctor n names args res body/\
 Inv sa/\InfoInv sa/\FwdInv sa/\st_w sa=w/\Fr s sa/\
 match body with
 |None=>apply_abstract f c e g sa=Good((xs,e',c'),sf)
 |Some _=>apply_body f c e g sa=Good((xs,e',c'),sf)
 end.
Proof.
 intros f c e sp s xs e' c' sf F C Run Hi Ii Fi Hw HC.
 destruct sp as[[pa|][dn|][br|]];cbn[apply_functor]in Run;
 try solve[exfalso;eapply err_not_good;exact Run].
 apply bind_inv in Run as(go&sr&Path&Run).
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ HC)Path)as[-> Resolved].
 destruct go as[[u|mi]|];cbn[option_map ent_of]in Resolved;
 try solve[exfalso;eapply err_not_good;exact Run].
 apply bind_inv in Run as(g&sg&Sym&Run).
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Resolved Sym)as[-> Hg].
 pose proof(load_world_ids b w Hload u g Hg)as Id;subst u.
 destruct(kind_eqb(gs_kind g)KFunctor)eqn:Kind;cbn[negb]in Run;
 [|exfalso;eapply err_not_good;exact Run].
 apply kind_eqb_spec in Kind.
 destruct(nmem(gs_id g)(cx_applying c));[exfalso;eapply err_not_good;exact Run|].
 assert(Parts:exists n names args res body,gs_decl g=ADFunctor n names args res body).
 {pose proof(kind_sym_read b w R Fl Hload(gs_id g)g Hg)as K.
 unfold A.kind_sym in K;rewrite(w_decl_read w R Fl _ _ Hg)in K;cbn[A.d_ast]in K.
 rewrite Kind in K;cbn[kconv]in K.
 destruct(gs_decl g);cbn[A.kind_of]in K;try discriminate;
 try(match goal with r:fnrecv|-_=>destruct r;discriminate end).
 repeat eexists;reflexivity. }
 destruct Parts as(n&names&args&res&body&Decl).
 unfold functor_parts in Run;rewrite Decl in Run;cbn beta iota in Run.
 assert(Resolve:A.resolve_sym W(A.fr_mod F)pa=Some(gs_id g)).
 {unfold A.resolve_sym;rewrite(cr_mod _ _ _ _ _ _ _ HC),Resolved;reflexivity. }
 destruct body as[bodies|].
 - apply bind_inv in Run as([]&sa&Total&Body).
   destruct(ok_functor_total 64 g _ _ _ Hi Total)as[Ia Frame].
   exists pa,g,n,names,args,res,(Some bodies),sa.
   split;[reflexivity|];split;[exact Resolve|];split;[exact Hg|];split;[exact Decl|].
   split;[exact Ia|];split.
   + exact(info_step _ _ Ii(frl_of_fr _ _ Frame)(ni_functor_total _ _ _ _ _ Total)ltac:(wrapper_cache_same)).
   + split.
     * exact(proj2(@okF_readonly _ (functor_total 64 g)(ok_functor_total 64 g)(kw_functor_total 64 g))_ _ _ Hi Fi Total).
     * split;[rewrite(fr_w _ _ Frame);exact Hw|];split;assumption.
 - exists pa,g,n,names,args,res,None,s.
   split;[reflexivity|];split;[exact Resolve|];split;[exact Hg|];split;[exact Decl|].
   split;[exact Hi|];split;[exact Ii|];split;[exact Fi|];split;[exact Hw|];split;[apply fr_refl|exact Run].
Qed.
End Cases.
