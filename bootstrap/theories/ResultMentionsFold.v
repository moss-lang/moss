(* Actual left folds preserve and collect lower-bound mention facts. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore BodySoundCore ReqsSolve.
Import ListNotations.
Lemma mentions_addl:forall l out t,
 In t(fold_left(fun o x=>if nmem x o then o else o++[x])l out)<->In t out\/In t l.
Proof.
 intros l out t.
 change(In t(fold_left(fun o x=>if nmem x []||nmem x o then o else o++[x])l out)<->In t out\/In t l).
 rewrite fold_add_spec. cbn[In]. tauto.
Qed.
Section Fold.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Theorem mentions_fold:forall X(step:list nat->X->M(list nat))(Pred:X->nat->Prop)xs,
 (forall out x,ok(step out x))->
 (forall x,In x xs->forall out s found s',step out x s=Good(found,s')->Inv s->Later w P R Fl s'->
  forall t,In t out\/Pred x t->In t found)->
 forall out s found s',mfold step out xs s=Good(found,s')->Inv s->Later w P R Fl s'->
 forall t,In t out\/(exists x,In x xs/\Pred x t)->In t found.
Proof.
 intros X step Pred xs O HS. induction xs as[|x rest IH];intros out s found s' H Hi L t Ht.
 - cbn[mfold]in H. injection H as <- <-. destruct Ht as[Ht|(x&[]&_)];exact Ht.
 - cbn[mfold]in H. apply bind_inv in H as(mid&s1&H1&H).
   destruct(O out x s mid s1 Hi H1)as[I1 F1].
   destruct(ok_mfold _ _ _ rest mid(fun o a=>O o a)s1 found s' I1 H)as[_ Ft].
   apply(IH(fun a Ha=>HS a(or_intror Ha))mid s1 found s' H I1 L t).
   destruct Ht as[Ht|(a&[<-|Ha]&Hp)].
   + left. exact(HS x(or_introl eq_refl)out s mid s1 H1 Hi(later_back w P R Fl _ _ Ft L)t(or_introl Ht)).
   + left. exact(HS x(or_introl eq_refl)out s mid s1 H1 Hi(later_back w P R Fl _ _ Ft L)t(or_intror Hp)).
   + right. exists a;split;assumption.
Qed.
End Fold.
