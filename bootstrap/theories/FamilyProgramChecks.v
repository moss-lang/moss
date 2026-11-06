(* The actual whole-program prefix discharges the family's finite context check. *)
From NM Require Import Sx Elab ElabSound ElabFrame.
Lemma family_check_contexts_identity:forall s sf,
 check_contexts s=Good(tt,sf)->sf=s.
Proof.
 intros s sf E;unfold check_contexts in E;cbn[mbind get]in E;unfold lift in E.
 destruct(let fix go(ids:list nat):res unit:=match ids with
 |nil=>Good tt|cons x rest=>let* _:=match NMap.nget(w_syms(st_w s))x with
 |Some g=>context_acyclic(S(w_nsyms(st_w s)))(st_w s)nil g|None=>Good tt end in go rest end
 in go(List.seq 0(w_nsyms(st_w s))))as[[]|error];[injection E;auto|discriminate E].
Qed.
Theorem family_program_contexts:forall w p sf,
 elab_program(s_init w)=Good(p,sf)->Inv(s_init w)->
 exists sc,check_contexts sc=Good(tt,sc)/\st_w sc=w.
Proof.
 intros w p sf E Hi;unfold elab_program in E.
 apply bind_inv in E as([]&sc&Listed&E);apply bind_inv in E as([]&ss&Contexts&Tail).
 pose proof(family_check_contexts_identity _ _ Contexts)as ->.
 assert(O:ok check_listed)by(unfold check_listed;okt).
 destruct(O _ _ _ Hi Listed)as[_ Frame].
 exists sc;split;[exact Contexts|rewrite(fr_w _ _ Frame);reflexivity].
Qed.
