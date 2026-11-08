(* Shared source tag argument reader; separated from body construct proofs. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel Sx Elab ElabSound ElabFrameCore.
Import ListNotations.
Definition tag_args(c:cx)(e:env)(g:gsym)(a:sapp):M(list(nat*ety)) :=
 let! tm:=match a with
 |Some bs=>check_relevant(cx_mod c)g bs;;
   let! sb:=app_subst 200 e(cx_mod c)bs(tymap e)in ret(sb++tymap e)
 |None=>ret(tymap e)
 end in nominal_args(cx_mod c)g tm.
Lemma ok_tag_args:forall c e g a,ok(tag_args c e g a).
Proof. intros;unfold tag_args,check_relevant,nominal_args;okt. Qed.
