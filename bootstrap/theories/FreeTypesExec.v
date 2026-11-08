(* Method provision applications reuse the context-independent visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrameCore BodySoundCore BodyTy CtxWalk.
From NM Require Export FreeVisitorCore.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Visitor.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Notation path_of mi:=(gm_path(get_mod w mi)).
Theorem method_provision_app_rel:forall g rcv n ps rt body tm tmr e s a s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn rcv n ps rt body->
 A.kind_sym WW(gs_id g)=Some A.DKMeth->
 provision_app g tm true s=Good(a,s')->Inv s->st_w s=w->Later w P R Fl s'->
 TmRel Fl tm tmr e->A.AppOf WW(gs_id g)tmr(tr_inst Fl a).
Proof.
 intros g rcv n ps rt body tm tmr e s a s' Hg D Hmeth H Hi Hw L TR.
 unfold provision_app in H. apply bind_inv in H as(rs&s1&H1&H). cbn beta iota in H.
 apply bind_inv in H as(rs'&s2&H2&H). injection H as <- <-.
 destruct(ok_reqs_frame _ _ _ _ Hi H1)as[I1 F1].
 destruct(ok_signature_syms_frame g rs _ _ _ I1 H2)as[_ F2].
 pose proof(reqs_R w R Fl _ s rs s1 Hi H1(later_back w P R Fl _ _ F2 L))as Er.
 destruct(signature_type_syms_filter w R Fl b Hload g rcv n ps rt body rs s1 rs' s2 Hg D H2 I1
  ltac:(rewrite(fr_w _ _ F1);exact Hw)L)as(test&Etest&Mtest).
 exists rs'. split.
 - change(incl rs'(R(gs_id g))). rewrite Er,Etest. intros x Hx. apply filter_In in Hx as[Hx _]. exact Hx.
 - split.
   + unfold tr_inst. rewrite map_map. apply map_ext. intro r. cbn[fst snd]. f_equal. symmetry;apply(proj1 TR).
   + split;[intro Hnot;exfalso;apply Hnot;exact Hmeth|intros _].
     change(A.w_reqs WW)with R. rewrite Er. exists(map test rs). split.
     * rewrite filter_mask. exact Etest.
     * clear -Mtest. induction rs as[|r rs IH];[constructor|]. constructor;[exact(Mtest r)|exact IH].
Qed.
End Visitor.
