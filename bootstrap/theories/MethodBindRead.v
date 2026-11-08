(* Actual method-bind target readings.  Method brackets admit every type
   key, while their right sides keep the caller's canonical readings. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy
 Merge CanonBodyTy CanonTypes TyOfSubst ProvideResultReading ProvideResultCanonical FamilyMethodExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma method_output_brackets:forall W p tm m a bs,
 A.kind_sym W m=Some A.DKMeth->A.OutputBrackets W p tm a bs->A.Brackets W p tm m a bs.
Proof.
 intros W p tm m a bs Kind H;destruct H as[|bs l Rel];[constructor|].
 apply A.BrSome. induction Rel as[|q v qs vs One Rest IH];constructor;[|exact IH].
 destruct One as(sp&sa&Espec&Path&Ktype&Read);exists sp,sa.
 split;[exact Espec|]. split;[exact Path|]. split;[exact Ktype|].
 split;[intro N;contradiction|exact Read].
Qed.
Lemma elab_method_type_ref:forall fuel pa mi tm e,
 elab_type(S fuel)(ATRef pa None)mi tm e None =
 (let!g:=resolve_type_symbol mi pa in symbol_type fuel mi g tm e).
Proof. reflexivity. Qed.
Local Strategy opaque[elab_type symbol_type app_subst].
Section Reading.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySound.W w R Fl).
Theorem method_bind_receiver:forall mi pa recv tm tmr e s sr this sf,
 resolve_type_symbol mi pa s=Good(recv,sr)->
 symbol_type 200 mi recv tm e sr=Good(this,sf)->
 Inv s->st_w s=w->In mi(w_order w)->TmRel Fl tm tmr e->Later w P R Fl sf->
 A.TyOf W(gm_path(get_mod w mi))tmr None(ATRef pa None)(tr Fl this)/\WFty this.
Proof.
 intros mi pa recv tm tmr e s sr this sf Resolve Read Hi Hw Hmi TR Last.
 assert(Run:elab_type 201(ATRef pa None)mi tm e None s=Good(this,sf)).
 {rewrite elab_method_type_ref;unfold mbind;rewrite Resolve;exact Read. }
 exact(proj1(ty_all w R Fl b Hload 201)(ATRef pa None)mi tm e None s this sf tmr
  Run Hi Hw Hmi TR I Last).
Qed.
Theorem method_bind_brackets:forall c e F C a m s tm sf,
 (match a with Some bs=>let!sb:=app_subst 200 e(cx_mod c)bs[]in ret(sb++tymap e)
 |None=>ret(tymap e)end)s=Good(tm,sf)->
 Inv s->st_w s=w->CxRel w R Fl c e F C->Later w P R Fl sf->
 A.kind_sym W m=Some A.DKMeth->
 exists bs,
 (forall k,A.nlook k(bs++A.c_ty C)=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))/\
 (forall k y,nassoc k tm=Some y->WFty y)/\A.Brackets W(A.fr_mod F)(A.c_ty C)m a bs.
Proof.
 intros c e F C a m s tm sf Run Hi Hw HC Last Kind.
 assert(Tri:Tri(en_uf e))by exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 assert(TR:TmRel Fl(tymap e)(map(trp Fl)(tymap e))e).
 {apply tmrel_tymap;[exact(cr_ty_wf _ _ _ _ _ _ _ HC)|]. intro k;rewrite nlook_trp,nassoc_tymap;reflexivity. }
 assert(Meaning:forall k,A.nlook k(A.c_ty C)=option_map(fun y=>tr Fl(canon e y))(nassoc k(tymap e))).
 {intro k;rewrite nassoc_tymap;exact(cr_ty _ _ _ _ _ _ _ HC k). }
 destruct a as[app|].
 - apply bind_inv in Run as(sb&sx&Read&Run);injection Run as <- <-.
   destruct(output_brackets_canonical w R Fl b Hload e(cx_mod c)app[]s sb sx(A.c_ty C)
     Read Hi Hw(cr_mod_in _ _ _ _ _ _ _ HC)Last Tri TR Meaning)as[Br Wsb].
   exists(map(fun q=>(fst q,tr Fl(canon e(snd q))))sb);split.
   + intro k;rewrite BodyTy.nlook_app,BodyTy.nassoc_app,Meaning.
     clear -sb k e Fl. induction sb as[|[x y]sb IH];cbn[map A.nlook nassoc assoc fst snd];[reflexivity|].
     destruct(Nat.eqb k x);[reflexivity|exact IH].
   + split.
     * intros k y Lookup;rewrite BodyTy.nassoc_app in Lookup.
       destruct(nassoc k sb)as[z|]eqn:Lsb.
       -- injection Lookup as <-. rewrite Forall_forall in Wsb.
          exact(Wsb(k,z)(Merge.nassoc_in _ _ _ _ Lsb)).
       -- rewrite nassoc_tymap in Lookup;exact(cr_ty_wf _ _ _ _ _ _ _ HC _ _ Lookup).
     * rewrite(cr_mod _ _ _ _ _ _ _ HC).
       (* The bracket right sides are read in the caller, before insertion. *)
       apply method_output_brackets;[exact Kind|].
       exact Br.
 - injection Run as <- <-;exists[]. split;[exact Meaning|]. split.
   + intros k y Lookup;rewrite nassoc_tymap in Lookup;exact(cr_ty_wf _ _ _ _ _ _ _ HC _ _ Lookup).
   + constructor.
Qed.
End Reading.
