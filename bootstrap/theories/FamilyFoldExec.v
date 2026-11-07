(* Select a genuine visit, including the prefix and suffix executions. *)
From Stdlib Require Import List.
From NM Require Import Sx Elab ElabSound ElabFrameCore.
Import ListNotations.
Theorem family_fold_visit:forall X A(step:A->X->M A),
 (forall a x,ok(step a x))->forall xs a s z sf,
 mfold step a xs s=Good(z,sf)->Inv s->forall x,In x xs->
 exists prefix suffix before after sb sa,
 xs=prefix++x::suffix/\mfold step a prefix s=Good(before,sb)/\
 step before x sb=Good(after,sa)/\mfold step after suffix sa=Good(z,sf)/\Inv sb.
Proof.
 intros X A step O xs;induction xs as[|head rest IH];intros a s z sf Run Hi x Hin;[contradiction|].
 cbn[mfold]in Run;apply bind_inv in Run as(next&sn&Step&Tail).
 destruct(O _ _ _ _ _ Hi Step)as[Inext Fn].
 destruct Hin as[<-|Hin].
 - exists[],rest,a,next,s,sn;split;[reflexivity|];split;[reflexivity|];split;[exact Step|];split;assumption.
 - destruct(IH next sn z sf Tail Inext x Hin)as(prefix&suffix&before&after&sb&sa&Eq&Pre&One&Post&Ib).
   exists(head::prefix),suffix,before,after,sb,sa;split;[cbn;rewrite Eq;reflexivity|].
   split;[|split;[exact One|];split;assumption].
   cbn[mfold];unfold mbind;rewrite Step;exact Pre.
Qed.
Theorem family_fold_new_id_visit:forall X Y(id:Y->nat)(step:list Y->X->M(list Y)),
 (forall before x,ok(step before x))->forall xs before s after sf u,
 mfold step before xs s=Good(after,sf)->Inv s->
 ~In u(map id before)->In u(map id after)->
 exists prefix x suffix first next sb sn,
 xs=prefix++x::suffix/\mfold step before prefix s=Good(first,sb)/\
 step first x sb=Good(next,sn)/\mfold step next suffix sn=Good(after,sf)/\
 Inv sb/\~In u(map id first)/\In u(map id next).
Proof.
 intros X Y id step O xs;induction xs as[|head rest IH];intros before s after sf u Run Hi Absent Present.
 - injection Run as <- <-;contradiction.
 - cbn[mfold]in Run;apply bind_inv in Run as(next&sn&One&Tail).
   destruct(O _ _ _ _ _ Hi One)as[Inext Fn].
   destruct(in_dec PeanoNat.Nat.eq_dec u(map id next))as[New|NoNew].
   + exists[],head,rest,before,next,s,sn;split;[reflexivity|];split;[reflexivity|];
     split;[exact One|];split;[exact Tail|];split;[exact Hi|];split;assumption.
   + destruct(IH next sn after sf u Tail Inext NoNew Present)
     as(prefix&x&suffix&first&last&sb&sl&Eq&Pre&Visit&Post&Ib&Old&New).
     exists(head::prefix),x,suffix,first,last,sb,sl;split;[cbn;rewrite Eq;reflexivity|].
     split.
     * cbn[mfold];unfold mbind;rewrite One;exact Pre.
     * split;[exact Visit|];split;[exact Post|];split;[exact Ib|];split;assumption.
Qed.
