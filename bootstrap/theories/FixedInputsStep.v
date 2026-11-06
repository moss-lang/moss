(* The fixed-input half of the actual mutually recursive visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy NestingTypesExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Section FixedStep.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Definition MentionCovers(f:nat):Prop:=forall self mi specs out s found s',
 result_mentions_go f self mi specs out s=Good(found,s')->Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall sp t,In sp specs->A.ResultMentions WW self(gm_path(get_mod w mi))sp t->In t found.
Lemma names_absent:forall p names keys t,
 Forall2(fun name key=>A.resolve_sym WW p[name]=Some key/\A.kind_sym WW key=Some A.DKType)names keys->
 ~(exists name,In name names/\A.resolve_sym WW p[name]=Some t)->~In t keys.
Proof.
 intros p names keys t H;induction H as[|n k names keys[Hr Hk]F IH];intros N Ht;[exact Ht|].
 destruct Ht as[<-|Ht].
 - apply N;exists n;split;[left;reflexivity|exact Hr].
 - apply IH;[intros(n'&Hin&Hs);apply N;exists n';split;[right;exact Hin|exact Hs]|exact Ht].
Qed.
Theorem fixed_go_step:forall f,MentionCovers f->forall g n names args res s found s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n(Some names)args res None->
 functor_fixed_go(S f)g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 forall t,A.Fixed WW(gs_id g)t->In t found.
Proof.
 intros f IH g n names args res s found s' Hg D H Hi Hw L t Hfixed.
 cbn[functor_fixed_go]in H. apply bind_inv in H as(ps&s1&Hp&H).
 destruct ps as[keys|].
 2: {unfold functor_params,functor_brackets in Hp;rewrite D in Hp.
    apply bind_inv in Hp as(ks&s2&E&Hp);discriminate Hp. }
 destruct(functor_params_names w R Fl b Hload g n names args res None s keys s1 Hg D Hp Hi Hw)as[-> Pnames].
 unfold functor_parts in H;rewrite D in H;cbn beta iota zeta in H.
 apply bind_inv in H as(ms&s2&Hm&H). apply bind_inv in H as(rts&s3&Ho&H).
 injection H as <- <-.
 destruct(ok_result_mentions_go f(gs_id g)(gs_mod g)res [] s ms s2 Hi Hm)as[I2 F2].
 pose proof(fr_w' w _ _ F2 Hw)as W2.
 destruct(result_types_rel w R Fl b Hload g n(Some names)args res None s2 rts s3 Hg D Ho I2 W2)
  as[->[_ Outs]].
 destruct(proj1(fixed_named_iff w R Fl g n names args res t Hg D)Hfixed)
  as[(sp&Hin&HM)[Nout Nnames]].
 apply filter_In. split.
 - exact(IH(gs_id g)(gs_mod g)res [] s ms s2 Hm Hi Hw(loader_mod_in w R Fl b Hload _ _ Hg)L sp t Hin HM).
 - apply andb_true_iff;split;apply negb_true_iff;apply Bool.not_true_is_false;intro Mem;apply nmem_in in Mem.
   + exact(names_absent _ _ _ _ Pnames Nnames Mem).
   + apply Nout,(proj1(Outs t));exact Mem.
Qed.
End FixedStep.
