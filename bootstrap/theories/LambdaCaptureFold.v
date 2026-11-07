(* The actual capture fold allocates distinct fresh slots and preserves the
   exact enclosing-local types, names, and generated binding order. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv BodySoundCore
 LambdaTrace LambdaCaptureStep LambdaCaptureContext NominalSupport.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Definition capture_id(q:string*nat*ety):nat:=let '(_,v,_) :=q in v.
Definition capture_name(q:string*nat*ety):string:=let '(n,_,_) :=q in n.
Definition capture_model(Fl:string->nat)(original:env)(C:A.cx)(q:string*nat*ety):A.cx:=
 let '(_,v,t) :=q in A.provide C(IVal v)[](A.set_val C v(tr Fl(canon original t))).
Definition capture_pre(F:A.frame)(q:string*nat*ety):xstmt:=
 let '(n,v,_) :=q in SBindVal v(XVar(A.var_of F n)).
Definition capture_row(q:string*(ety*lmut))(r:string*nat*ety):Prop:=
 let '(n,(t,_)) :=q in let '(rn,_,rt) :=r in n=rn/\t=rt.
Section Capture.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem lambda_capture_fold:forall qs c original F Co le C pre lets s le' pre' lets' sf bound,
 mfold(lambda_capture_one c original)(le,pre,lets)qs s=Good((le',pre',lets'),sf)->
 Inv s->st_w s=w->nget(w_syms w)(gs_id bound)=Some bound->
 CxRel w R Fl c original F Co->CxRel w R Fl c le F C->en_uf le=en_uf original->
 Forall(fun q=>sassoc(fst q)(en_locals original)=Some(snd q))qs->Post w P R Fl sf F->
 exists added,
 Forall2 capture_row qs added/\pre'=pre++map(capture_pre F)added/\lets'=lets++added/\
 Forall(A.Fresh W)(map capture_id added)/\NoDup(map capture_id added)/\
 Forall(A.Named F)(map capture_name added)/\
 (forall q,In q added->st_next s<=capture_id q<st_next sf)/\
 en_uf le'=en_uf original/\
 CxRel w R Fl c le' F(fold_left(capture_model Fl original)added C).
Proof.
 intro qs;induction qs as[|[n[t mu]]rest IH];
 intros c original F Co le C pre lets s le' pre' lets' sf bound Run Hi Hw Hg Horig HC Uf Rows Hp.
 - injection Run as <- <- <- <-.
   exists[];split;[constructor|];split;[now rewrite app_nil_r|];
   split;[now rewrite app_nil_r|];split;[constructor|];split;[constructor|];
   split;[constructor|];split;[intros q[]|];split;assumption.
 - apply Forall_cons_iff in Rows as[Local Rows].
   cbn[mfold]in Run;apply bind_inv in Run as([ [l1 p1] a1]&s1&Step&Tail).
   destruct(ok_lambda_capture_one c original(le,pre,lets)(n,(t,mu))_ _ _ Hi Step)as[I1 F1].
   assert(Otail:ok(mfold(lambda_capture_one c original)(l1,p1,a1)rest)).
   {apply ok_mfold;intros;apply ok_lambda_capture_one. }
   destruct(Otail _ _ _ I1 Tail)as[_ Ft].
   destruct(lambda_capture_step w R Fl b Hload c original F Co le C pre lets n t mu s l1 p1 a1 s1 bound
     Step Hi Hw Hg Horig HC Local(post_back w P R Fl _ _ _ Ft Hp))
   as(v&stamp&slot&Fresh&Vnext&Growth&Named&Number&Env&Pre&Lets&Ctx).
   subst l1 p1 a1;rewrite <-Number in *.
   assert(U1:en_uf(set_app(set_val le v t)(KyVal v)[]stamp)=en_uf original)by exact Uf.
   assert(Canon:canon le t=canon original t)by(unfold canon;now rewrite Uf).
   rewrite Canon in Ctx.
   destruct(IH c original F Co(set_app(set_val le v t)(KyVal v)[]stamp)
    (A.provide C(IVal v)[](A.set_val C v(tr Fl(canon original t))))
     (pre++[SBindVal v(XVar(A.var_of F n))])(lets++[(n,v,t)])s1 le' pre' lets' sf bound
     Tail I1 ltac:(rewrite(fr_w _ _ F1);exact Hw)Hg Horig Ctx U1 Rows Hp)
    as(added&Rel&PreTail&LetsTail&FreshTail&NdTail&NamedTail&Bounds&UfTail&CtxTail).
   exists((n,v,t)::added);split;[constructor;[split;reflexivity|exact Rel]|].
   split.
   + rewrite PreTail,<-app_assoc;reflexivity.
   + split.
     * rewrite LetsTail,<-app_assoc;reflexivity.
     * split;[constructor;assumption|].
       split.
       -- constructor;[|exact NdTail].
          intro Hit;apply in_map_iff in Hit as(q&Eq&Hit).
          pose proof(Bounds q Hit);cbn[capture_id]in Eq;lia.
       -- split;[constructor;assumption|].
       split.
          ++ intros q[Eq|Hit].
             ** subst q;cbn[capture_id].
             pose proof(fr_next _ _ Ft);lia.
             ** pose proof(Bounds q Hit);pose proof(fr_next _ _ F1);lia.
          ++ split;[exact UfTail|exact CtxTail].
Qed.
End Capture.
