(* Static translation of executable provision keys. *)
From Stdlib Require Import String.
From NM Require Import Audit Sx Elab BodyTr.
Definition key_tr (Fl:string->nat)(k:key):item:=
 match k with
 |KyVal v=>IVal v|KyFn f=>IFn f|KyMth r m=>IMth(tr Fl r)m nil|KyFun u=>IFun u end.
Definition key_ok(k:key):bool:=match k with KyMth t _=>ety_ok t|_=>true end.
