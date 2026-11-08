(* Completed source runs retain their own checked headers in later states. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv InfoInvariant
 InfoPreserve FwdInvariant FnHistory FnCompleted FnStable ReifyHeader ReifyHeaderPresent.
Import ListNotations.
Set Implicit Arguments.
Definition completed_source_header(w:Elab.world)(observed:st)(f:nat)(body:xexpr):Prop:=
 exists fuel g initial completed,
 nget(w_syms w)f=Some g/\gs_id g=f/\
 Inv initial/\InfoInv initial/\FwdInv initial/\st_w initial=w/\
 nassoc f(st_fns initial)=None/\lower_body fuel g initial=Good(tt,completed)/\
 nassoc f(st_fns completed)=Some body/\FrL completed observed/\
 function_header observed f=function_header completed f.
Definition source_header_history(w:Elab.world)(initial final:st):Prop:=
 forall f body,loaded_fn_key w f->nassoc f(st_fns final)=Some body->
 (nassoc f(st_fns initial)=Some body/\function_header final f=function_header initial f)\/
 completed_source_header w final f body.
Lemma source_header_refl:forall w s,source_header_history w s s.
Proof. intros w s f body Loaded Present;left;split;[exact Present|reflexivity]. Qed.
Lemma source_header_trans:forall w a b c,
 source_header_history w a b->source_header_history w b c->FrL b c->source_header_history w a c.
Proof.
 intros w a b c AB BC Frame f body Loaded Present.
 destruct(BC f body Loaded Present)as[[Old Header]|Done];[|right;exact Done].
 destruct(AB f body Loaded Old)as[[Earlier Header0]|(fuel&g&initial&completed&Loaded0&Id&Inv0&Info0&Fwd0&World&Absent&Run&Body&Before&Header0)].
 - left;split;[exact Earlier|rewrite Header,Header0;reflexivity].
 - right;exists fuel,g,initial,completed;repeat first[assumption|lazymatch goal with |- _ /\ _=>split end];[eapply frl_trans;eassumption|rewrite Header,Header0;reflexivity].
Qed.
Definition HH{A}(m:M A):Prop:=okI m/\forall b s value final,
 Inv s->InfoInv s->FwdInv s->load_world b=Good(st_w s)->m s=Good(value,final)->
 source_header_history(st_w s)s final.
Arguments HH:simpl never.
Lemma hh_ret:forall A(a:A),HH(ret a).
Proof.
 intros A a;split;[apply okI_ret|];intros b s value final I Cache Fwd Load Run.
 injection Run as <- <-;apply source_header_refl.
Qed.
Lemma hh_bind:forall A B(m:M A)(k:A->M B),HH m->okF m->(forall a,HH(k a))->HH(mbind m k).
Proof.
 intros A B m k[OK Hist]Fwd K;split;[apply okI_bind;[exact OK|intro;exact(proj1(K a))]|].
 intros b s value final I Cache Forward Load Run;apply bind_inv in Run as[a[mid[First Last]]].
 destruct(proj1 OK _ _ _ I First)as[Im Frame].
 assert(LoadMid:load_world b=Good(st_w mid))by(rewrite(fr_w _ _ Frame);exact Load).
 pose proof(proj2(K a)b mid value final Im(proj2 OK _ _ _ I Cache First)
  (proj2 Fwd _ _ _ I Forward First)LoadMid Last)as Tail.
 rewrite(fr_w _ _ Frame)in Tail.
 eapply source_header_trans;[exact(Hist b s a mid I Cache Forward Load First)|exact Tail|].
 apply frl_of_fr;exact(proj2(proj1(proj1(K a))_ _ _ Im Last)).
Qed.
Lemma hh_miter:forall A(f:A->M unit)xs,(forall x,HH(f x))->(forall x,okF(f x))->HH(miter f xs).
Proof.
 intros A f xs K Fwd;induction xs;cbn[miter];[apply hh_ret|].
 apply hh_bind;[apply K|apply Fwd|intro;exact IHxs].
Qed.
Lemma hh_lower_body_loaded:forall fuel g b s final,
 nget(w_syms(st_w s))(gs_id g)=Some g->Inv s->InfoInv s->FwdInv s->
 load_world b=Good(st_w s)->lower_body fuel g s=Good(tt,final)->
 source_header_history(st_w s)s final.
Proof.
 intros fuel g b s final Loaded I Cache Fwd Load Run f body Key Present.
 destruct(Nat.eq_dec f(gs_id g))as[Same|Other].
 - subst f;destruct(nassoc(gs_id g)(st_fns s))as[old|]eqn:Before.
   + pose proof(@lower_body_already_present fuel g s old final Before Run)as Eq;subst final.
     left;split;[rewrite <-Before;exact Present|reflexivity].
   + right;exists fuel,g,s,final.
     repeat first[assumption|reflexivity|solve[apply frl_of_fr,fr_refl]|split].
 - assert(Old:nassoc f(st_fns s)=Some body).
   {rewrite <-(proj2(ne_lower_body fuel g)b s tt final I Load Run f Key Other);exact Present. }
   left;split;[exact Old|].
   exact(proj2(hp_lower_body fuel g)_ _ _ I Run f body(@loaded_fn_below_next b s f Load I Key)Old).
Qed.
Lemma hh_stable:forall A(m:M A),okI m->NF m->HP m->HH m.
Proof.
 intros A m OK Stable Header;split;[exact OK|].
 intros b s value final I Cache Fwd Load Run f body Key Present.
 assert(Old:nassoc f(st_fns s)=Some body)by(rewrite <-(proj2 Stable b s value final I Load Run f Key);exact Present).
 left;split;[exact Old|].
 exact(proj2 Header _ _ _ I Run f body(@loaded_fn_below_next b s f Load I Key)Old).
Qed.
