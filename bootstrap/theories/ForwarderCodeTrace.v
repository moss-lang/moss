(* Unwrap real metadata/parameter scopes without postulating body execution. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame.
Import ListNotations.
Set Implicit Arguments.
Lemma with_vars_body_trace:forall A init(m:M A)s a final,
 with_vars init m s=Good(a,final)->exists bodyFinal,
 m(s_vars init(List.length init)s)=Good(a,bodyFinal).
Proof.
 intros A init m s a final E;unfold with_vars in E.
 apply bind_inv in E as[saved[s0[Get E]]];injection Get as <- <-.
 apply bind_inv in E as[u[s1[Change E]]];unfold upd in Change;injection Change as <- <-.
 apply bind_inv in E as[value[bodyFinal[Body E]]].
 apply bind_inv in E as[u'[s2[Restore E]]];injection E as <- <-;exists bodyFinal;exact Body.
Qed.
Lemma in_reify_body_trace:forall A owner(m:M A)s a final,
 Inv s->in_reify_code owner m s=Good(a,final)->exists bodyStart bodyFinal,
 m bodyStart=Good(a,bodyFinal)/\Inv bodyStart/\st_w bodyStart=st_w s/\
 st_syms bodyStart=st_syms s/\st_vars bodyStart=st_vars s/\st_next bodyStart=st_next s.
Proof.
 intros A owner m s a final I E;unfold in_reify_code in E.
 apply bind_inv in E as[saved[s0[Get E]]];injection Get as <- <-.
 apply bind_inv in E as[u[s1[Change E]]];unfold upd in Change;injection Change as <- <-.
 apply bind_inv in E as[value[bodyFinal[Body E]]].
 apply bind_inv in E as[u'[s2[Restore E]]];injection E as <- <-.
 eexists;exists bodyFinal;split;[exact Body|]. split.
 - destruct I;constructor;assumption.
 - repeat split;reflexivity.
Qed.
Lemma in_reify_vars_body_trace:forall A owner init(m:M A)s a final,
 Inv s->in_reify_code owner(with_vars init m)s=Good(a,final)->exists bodyStart bodyFinal,
 m bodyStart=Good(a,bodyFinal)/\Inv bodyStart/\st_w bodyStart=st_w s/\
 st_syms bodyStart=st_syms s/\st_vars bodyStart=init/\st_next bodyStart=st_next s.
Proof.
 intros A owner init m s a final I E.
 destruct(@in_reify_body_trace A owner(with_vars init m)s a final I E)as[start[last[Vars[IS[W[Sy[Vs N]]]]]]].
 destruct(@with_vars_body_trace A init m start a last Vars)as[bodyFinal Body].
 exists(s_vars init(List.length init)start),bodyFinal;split;[exact Body|]. split.
 - destruct IS;constructor;assumption.
 - cbn[s_vars st_w st_syms st_vars st_next];repeat split;assumption||reflexivity.
Qed.
