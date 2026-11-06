(* fn_info's returned value is the entry it actually keeps. *)
From Stdlib Require Import List.
From NM Require Import NMap Sx Elab ElabSound WorldSound FnInfoCore.
Lemma fn_info_stored:forall pv s info sf,
 fn_info pv s=Good(info,sf)->nget(st_info sf)(gs_id pv)=Some info.
Proof.
 intros pv s info sf Run;pose proof Run as Full;unfold fn_info in Run.
 apply bind_inv in Run as[ss[sm[Get Run]]];injection Get as <- <-.
 destruct(nget(st_info s)(gs_id pv))as[cached|]eqn:Cache.
 - injection Run as <- <-;exact Cache.
 - destruct(fn_info_miss pv s info sf Full Cache)as(g&e&needs&s1&Lookup&Walk&Value&State).
   rewrite State;cbn[s_info st_info];apply nget_nset_same.
Qed.
