(* A capture allocates a genuinely fresh value, binds the enclosing local,
   and extends the lambda's provision context by that exact type. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 BodySoundCore BindValue NominalSupport ApplyFreshExec LambdaTrace.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Capture.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem lambda_capture_step:forall c original F Co le C pre lets n ty mu s le' pre' lets' sf bound,
 lambda_capture_one c original(le,pre,lets)(n,(ty,mu))s=Good((le',pre',lets'),sf)->
 Inv s->st_w s=w->nget(w_syms w)(gs_id bound)=Some bound->
 CxRel w R Fl c original F Co->CxRel w R Fl c le F C->
 sassoc n(en_locals original)=Some(ty,mu)->Post w P R Fl sf F->
 exists v stamp slot,
 A.Fresh W v/\v=st_next s/\st_next s<st_next sf/\A.Named F n/\A.var_of F n=slot/\
 le'=set_app(set_val le v ty)(KyVal v)[]stamp/\
 pre'=pre++[SBindVal v(XVar slot)]/\lets'=lets++[(n,v,ty)]/\
 CxRel w R Fl c le' F(A.provide C(IVal v)[](A.set_val C v(tr Fl(canon le ty)))).
Proof.
 intros c original F Co le C pre lets n ty mu s le' pre' lets' sf bound Run Hi Hw Hg Horig HC Local Hp.
 unfold lambda_capture_one in Run.
 apply bind_inv in Run as(v&sv&New&Run).
 destruct(ok_new_sym(cx_mod c)n KVal(ADVal n ATUnit None)OCaptured ltac:(discriminate)ltac:(split;discriminate)_ _ _ Hi New)as[Iv Fv].
 pose proof(new_sym_fresh b w R Fl Hload s bound(cx_mod c)n KVal(ADVal n ATUnit None)OCaptured v sv Hi Hw Hg New)as Fresh.
 apply bind_inv in Run as(stamp&ss&Stamp&Run).
 destruct(ok_fresh _ _ _ Iv Stamp)as[Is Fs].
 apply bind_inv in Run as(slot&sl&Slot&Run).
 assert(Ol:ok(local_var n))by okt;destruct(Ol _ _ _ Is Slot)as[Il Flocal].
 apply bind_inv in Run as(u&sm&Meta&Run);destruct u;injection Run as <- <- <- <-.
 match type of Meta with ?m _ = _=>assert(Om:ok m)by okt end.
 destruct(Om _ _ _ Il Meta)as[_ Fmeta].
 destruct(local_named w P R Fl _ _ _ _ _ F Slot Fmeta Hp)as[Named Number].
 assert(Vnext:v=st_next s)by exact(new_sym_id _ _ _ _ _ _ _ _ New).
 assert(Snext:st_next ss=S(st_next sv)).
 {unfold fresh in Stamp;injection Stamp as <- <-;reflexivity. }
 assert(Growth:st_next s<st_next sm).
 {pose proof(fr_next _ _ Fv);pose proof(fr_next _ _ Flocal);pose proof(fr_next _ _ Fmeta);lia. }
 exists v,stamp,slot;split;[exact Fresh|];split;[exact Vnext|];split;[exact Growth|];split;[exact Named|];split;[exact Number|].
 split;[reflexivity|];split;[reflexivity|];split;[reflexivity|].
 eapply(cxrel_value w R Fl);[exact HC| | |reflexivity].
 - exact(cr_locals_wf _ _ _ _ _ _ _ Horig n(ty,mu)Local).
 - exact(@es_locals W original(cr_support _ _ _ _ _ _ _ Horig)n(ty,mu)Local).
Qed.
End Capture.
