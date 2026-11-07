(* Actual fn_info cache entries retain their exact provision-application view. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore InfoCache.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Definition AppsShape(i:fninfo):Prop:=fi_apps i=
 map(fun k=>(k,match get_app(fi_env i)k with Some(a,_)=>a|None=>[]end))(fi_needs i).
Theorem fn_info_apps_shape:forall b w g s i s',
 load_world b=Good w->st_w s=w->nget(w_syms w)(gs_id g)=Some g->InfoInv s->
 fn_info g s=Good(i,s')->AppsShape i.
Proof.
 intros b w g s i s' Hl Hw Hg Ic E.
 unfold fn_info in E. apply bind_inv in E as(x &sx &Eg &E).
 unfold get in Eg. injection Eg as <- <-.
 destruct(nget(st_info s)(gs_id g))as[j|]eqn:Ej.
 - injection E as <- <-. destruct(Ic _ _ Ej)as[(g0 &s0 &e &needs &s1 &Eg0 &I0 &Ew &-> &F)|[N _]];
   [reflexivity|].
   specialize(N b ltac:(rewrite Hw;exact Hl)). rewrite Hw,Hg in N;discriminate.
 - apply bind_inv in E as(g0 &sg &Egs &E).
   apply bind_inv in E as(res &sm &Walk &E). destruct res as[e needs].
   apply bind_inv in E as(u &su &Upd &E). injection E as <- _;reflexivity.
Qed.
Lemma fn_info_app_lookup : forall i need,
 AppsShape i->In need(fi_needs i)->
 assoc key_eqb need(fi_apps i)=
 Some(match get_app(fi_env i)need with Some(a,_)=>a|None=>[]end).
Proof.
 intros i need Sh Hin. unfold AppsShape in Sh. rewrite Sh. clear Sh.
 induction(fi_needs i)as[|k ns IH];[contradiction|].
 cbn[map assoc fst snd]. destruct(key_eqb need k)eqn:Eq.
 - apply CtxWalk.key_eqb_spec in Eq. subst k;reflexivity.
 - apply IH. destruct Hin as[<-|Hin];[|exact Hin].
   rewrite (proj2(CtxWalk.key_eqb_spec k k)eq_refl)in Eq;discriminate.
Qed.
