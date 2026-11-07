(* Operational annotation checks, independent of body judgments. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame.
Import ListNotations.
Local Open Scope string_scope.
Lemma ok_ety_of_trace:forall t mi tm e this,ok(ety_of t mi tm e this).
Proof. intros;unfold ety_of;apply ok_elab_type. Qed.
Local Strategy opaque[Nat.of_num_uint FUEL ety_of].
Definition lambda_parameter_check(c:cx)(e:env)(this:option ety)(what:string)
 (q:(name*option aty)*(name*ety)):M unit:=
 let '((pn,pt),(_,want)):=q in
 match pt with
 |Some t=>let! actual:=ety_of t(cx_mod c)(tymap e)e this in
   if ety_eqb(canon e actual)(canon e want)then ret tt else
   let! a:=show actual in let! b:=show want in
   err(cx_mod c)("parameter `"++pn++"` of the anonymous function bound to "++what++
   " is annotated `"++a++"`, but "++what++" declares `"++b++"` there (D70)")
 |None=>ret tt end.
Lemma ok_lambda_parameter_check:forall c e this what q,ok(lambda_parameter_check c e this what q).
Proof. intros c e this what[[pn[ty|]][n want]];unfold lambda_parameter_check;okt. all:apply ok_ety_of_trace. Qed.
Definition lambda_return_check(c:cx)(e:env)(this:option ety)(what:string)(rt:option aty)(want:ety):M unit:=
 match rt with
 |Some t=>let! actual:=ety_of t(cx_mod c)(tymap e)e this in
   if ety_eqb(canon e actual)(canon e want)then ret tt else
   let! a:=show actual in let! b:=show want in
   err(cx_mod c)("the anonymous function bound to "++what++" is annotated to return `"++a++
   "`, but "++what++" returns `"++b++"` (D70)")
 |None=>ret tt end.
