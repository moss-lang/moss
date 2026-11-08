(* The actual body receiver has the receiver required by FnElab. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 CanonTypes CanonBodyTy TagArgsExec FnBodyTrace LoaderReceiver.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nominal_args].
Section Receiver.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Theorem body_receiver_reading:forall g receiver name parameters result body c e F C s this final,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn receiver name parameters result body->
 body_receiver g e s=Good(this,final)->Inv s->st_w s=w->CxRel w R Fl c e F C->
 Later w P R Fl final->
 match receiver with
 |RAttached _=>exists r args,A.Attached(BodySound.W w R Fl)r name(gs_id g)/\
  option_map(fun t=>tr Fl(canon e t))this=Some(TNom r args)/\
  Forall2(fun x xa=>fst xa=x/\A.nlook x(A.c_ty C)=Some(snd xa))(R r)args
 |_=>this=None end.
Proof.
 intros g receiver name parameters result body c e F C s this final Loaded Decl E I Hw Context Late.
 destruct receiver as[| |rn].
 - pose proof(loaded_nonattached_receiver b w(gs_id g)g RPlain name parameters result body Hload Loaded Decl ltac:(intros;discriminate))as Recv.
   unfold body_receiver in E;rewrite Recv in E;injection E as <- <-;reflexivity.
 - pose proof(loaded_nonattached_receiver b w(gs_id g)g RDetached name parameters result body Hload Loaded Decl ltac:(intros;discriminate))as Recv.
   unfold body_receiver in E;rewrite Recv in E;injection E as <- <-;reflexivity.
 - destruct(loaded_attached_receiver b w R Fl(gs_id g)g rn name parameters result body Hload Loaded Decl)
   as[r[gr[Recv[Gr[Owner Attached]]]]].
   unfold body_receiver in E;rewrite Recv in E.
   apply bind_inv in E as[rg[s1[Read E]]].
   assert(At:nget(st_syms s)r=Some gr)by(eapply iv_loader;[exact I|rewrite Hw;exact Hload|rewrite Hw;exact Gr]).
   unfold sym in Read;rewrite At in Read;injection Read as <- <-.
   destruct(kind_eqb(gs_kind gr)KUnit||kind_eqb(gs_kind gr)KTag);[|body_trace_noerr E].
   apply bind_inv in E as[xs[s2[Args E]]];injection E as <- <-.
   assert(Id:gs_id gr=r)by exact(iv_ids s I r gr At).
   assert(Tables:forall key y,nassoc key(tymap e)=Some y->WFty y/\A.nlook key(A.c_ty C)=Some(tr Fl(canon e y))).
   {intros key y Lookup;rewrite nassoc_tymap in Lookup;split.
    - exact(cr_ty_wf _ _ _ _ _ _ _ Context key y Lookup).
    - rewrite(cr_ty _ _ _ _ _ _ _ Context key),Lookup;reflexivity. }
   destruct(nominal_args_lookup w R Fl (gs_mod g)gr(tymap e)s xs s2(A.c_ty C)(fun y=>tr Fl(canon e y))Args I Late Tables)
    as[Readings Wf].
   exists r,(map(fun p=>(fst p,tr Fl(canon e(snd p))))xs);split;[exact Attached|];split.
   + cbn[option_map];rewrite tr_canon_nom;reflexivity.
   + rewrite Id in Readings;exact Readings.
Qed.
End Receiver.
