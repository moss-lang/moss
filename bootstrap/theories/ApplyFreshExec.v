(* New output generations are genuinely fresh in the loaded source world. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy ApplyAbstractExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma new_sym_id:forall mi n k d o s id s',new_sym mi n k d o s=Good(id,s')->id=st_next s.
Proof.
 intros mi n k d o s id s' E;unfold new_sym in E.
 apply bind_inv in E as(i&si&Ei&E);unfold fresh in Ei;injection Ei as <- <-.
 apply bind_inv in E as(u&su&Eu&E);injection E as <- _;reflexivity.
Qed.
Section Source.
Variables(b:bundle)(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Hypothesis Hload:load_world b=Good w.
Lemma loaded_next_positive:forall s g,Inv s->st_w s=w->
 nget(w_syms w)(gs_id g)=Some g->0<st_next s.
Proof.
 intros s g Hi Hw Hg.
 pose proof(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)(gs_id g)g ltac:(rewrite Hw;exact Hg))as Has.
 assert(N:~st_next s<=gs_id g)by(intro H;rewrite(iv_above _ Hi _ H)in Has;discriminate).
 lia.
Qed.
Theorem new_sym_fresh:forall s g mi n k d o id s',
 Inv s->st_w s=w->nget(w_syms w)(gs_id g)=Some g->
 new_sym mi n k d o s=Good(id,s')->A.Fresh(BodySoundCore.W w R Fl)id.
Proof.
 intros s g mi n k d o id s' Hi Hw Hg E.
 pose proof(new_sym_id _ _ _ _ _ _ _ _ E)as ->.
 split.
 - change(match nget(w_syms w)(st_next s)with Some g=>Some(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))|None=>None end=None).
   destruct(nget(w_syms w)(st_next s))as[h|]eqn:H;[|reflexivity].
   pose proof(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)(st_next s)h ltac:(rewrite Hw;exact H))as Has.
   rewrite(iv_above _ Hi _ (le_n _))in Has;discriminate.
 - pose proof(loaded_next_positive s g Hi Hw Hg);lia.
Qed.
End Source.

Section Generations.
Variables(b:bundle)(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Hypothesis Hload:load_world b=Good w.
Lemma output_generation_properties:forall e fg t s p s',
 nget(w_syms w)(gs_id fg)=Some fg->Inv s->st_w s=w->
 output_generation e fg t s=Good(p,s')->
 fst p=t /\ ((tn_get e t=None/\snd p=t)\/
 (tn_get e t<>None/\A.Fresh(BodySoundCore.W w R Fl)(snd p)/\snd p<>t)).
Proof.
 intros e fg t s p s' Hg Hi Hw E. unfold output_generation in E.
 destruct(tn_get e t)eqn:Here.
 - apply bind_inv in E as(tgg&st&Et&E).
   unfold sym in Et;destruct(nget(st_syms s)t)as[tg|]eqn:Get;[|discriminate].
   injection Et as <- <-.
   apply bind_inv in E as(id&sn&En&E);injection E as <- _.
   split;[reflexivity|]. right;split;[discriminate|]. split.
   + exact(new_sym_fresh b w R Fl Hload s fg _ _ _ _ _ id sn Hi Hw Hg En).
   + pose proof(new_sym_id _ _ _ _ _ _ _ _ En)as ->.
     cbn[fst snd];intro Eq;rewrite <-Eq in Get;rewrite(iv_above _ Hi _ (le_n _))in Get;discriminate.
 - injection E as <- _;split;[reflexivity|]. left;auto.
Qed.
End Generations.

Lemma ok_output_generation:forall e fg t,ok(output_generation e fg t).
Proof. intros;unfold output_generation;okt. Qed.
Section GeneratedList.
Variables(b:bundle)(w:Elab.world)(R:nat->list nat)(Fl:string->nat). Context{P:xprogram}.
Hypothesis Hload:load_world b=Good w.
Let W:=BodySoundCore.W w R Fl.
Definition GenRel(e:env)(t:nat)(p:nat*nat):Prop:=fst p=t /\
 ((tn_get e t=None/\snd p=t)\/(tn_get e t<>None/\A.Fresh W(snd p)/\snd p<>t)).
Theorem output_generations_rel:forall e fg ts s gens s',
 nget(w_syms w)(gs_id fg)=Some fg->mmap(output_generation e fg)ts s=Good(gens,s')->
 Inv s->st_w s=w->Later w P R Fl s'->Forall2(GenRel e)ts gens.
Proof.
 intros e fg ts s gens s' Hg E Hi Hw L.
 eapply(BodyTy.mmap_ok_rel w R Fl);[apply ok_output_generation| |exact E|exact Hi|exact Hw|exact L].
 intros t Ht si p sf Et Ii Wi Li.
 exact(output_generation_properties b w R Fl Hload e fg t si p sf Hg Ii Wi Et).
Qed.
Lemma output_generations_keys:forall e ts gens,Forall2(GenRel e)ts gens->map fst gens=ts.
Proof.
 intros e ts gens H;induction H as[|t p ts gens H Hs IH];[reflexivity|].
 cbn;rewrite(proj1 H),IH;reflexivity.
Qed.
Lemma output_generations_lookup:forall e ts gens,
 Forall2(GenRel e)ts gens->forall t,In t ts->exists g,nassoc t gens=Some g /\
 ((tn_get e t=None/\g=t)\/(tn_get e t<>None/\A.Fresh W g/\g<>t)).
Proof.
 intros e ts gens H;induction H as[|x[k g]ts gens H Hs IH];intros t Hin;[contradiction|].
 destruct H as[Key Case];cbn[fst snd]in Key,Case;subst k.
 cbn[nassoc assoc fst snd]. destruct(Nat.eqb t x)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst t;exists g;auto.
 - destruct Hin as[<-|Hin];[rewrite Nat.eqb_refl in Eq;discriminate|].
   exact(IH t Hin).
Qed.
End GeneratedList.

Lemma mmap_ret_value:forall X Y(f:X->Y)xs s ys s',
 mmap(fun x=>ret(f x))xs s=Good(ys,s')->ys=map f xs/\s'=s.
Proof.
 intros X Y f xs;induction xs as[|x xs IH];intros s ys s' E;cbn[mmap]in E.
 - injection E as <- <-;auto.
 - apply bind_inv in E as(y&sy&Ey&E);injection Ey as <- <-.
   apply bind_inv in E as(zs&sz&Ez&E);injection E as <- <-.
   destruct(IH _ _ _ Ez)as[-> ->];auto.
Qed.
Lemma nassoc_some_in:forall X t(x:X)xs,nassoc t xs=Some x->In(t,x)xs.
Proof.
 intros X t x xs;induction xs as[|[k v]xs IH];[discriminate|].
 cbn[nassoc assoc fst snd]. destruct(Nat.eqb t k)eqn:E.
 - apply Nat.eqb_eq in E;subst k;intro H;injection H as <-;left;reflexivity.
 - intro H;right;auto.
Qed.
Lemma output_argument_lookup:forall gens t g,
 nassoc t gens=Some g->g<>t->
 lookup_subst t(map(fun p=>(fst p,TAbs(snd p)))
   (filter(fun p=>negb(Nat.eqb(fst p)(snd p)))gens))=Some(TAbs g).
Proof.
 intros gens;induction gens as[|[key value]gens IH];intros t g Read Different;[discriminate|].
 cbn[nassoc assoc fst snd]in Read.
 destruct(Nat.eqb t key)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst key;injection Read as <-.
   assert(Ne:Nat.eqb t value=false)by(apply Nat.eqb_neq;congruence).
   cbn[filter map fst snd];rewrite Ne;cbn[negb map lookup_subst fst snd];rewrite Nat.eqb_refl;reflexivity.
 - cbn[filter map fst snd];destruct(Nat.eqb key value);cbn[negb map lookup_subst fst snd];
   try rewrite Eq;apply IH;assumption.
Qed.
Theorem output_arguments_override_inputs:forall gens s fresh final inputs t g,
 output_arguments gens s=Good(fresh,final)->nassoc t gens=Some g->g<>t->
 lookup_subst t(fresh++inputs)=Some(TAbs g).
Proof.
 intros gens s fresh final inputs t g Run Read Different;unfold output_arguments in Run.
 destruct(mmap_ret_value _ _ _ _ _ _ _ Run)as[-> _].
 rewrite CoreMoss.lookup_subst_app,(output_argument_lookup gens t g Read Different);reflexivity.
Qed.
Theorem output_arguments_contains:forall gens s th s' t g,
 output_arguments gens s=Good(th,s')->nassoc t gens=Some g->g<>t->In(t,TAbs g)th.
Proof.
 intros gens s th s' t g E Look Ne. unfold output_arguments in E.
 destruct(mmap_ret_value _ _ _ _ _ _ _ E)as[-> _].
 apply in_map_iff;exists(t,g);split;[reflexivity|].
 apply filter_In;split;[exact(nassoc_some_in _ _ _ _ Look)|].
 cbn[fst snd];apply Bool.negb_true_iff;apply Nat.eqb_neq;congruence.
Qed.

Section ModelOutputs.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat).
Let W:=BodySoundCore.W w R Fl.
Definition generation_of(gens:list(nat*nat))(t:nat):nat:=opt_default t(nassoc t gens).
Theorem generation_output_premise:forall c e F C rts gens ss th2 ss' th,
 CxRel w R Fl c e F C->Forall2(GenRel w R Fl e)rts gens->
 output_arguments gens ss=Good(th2,ss')->forall t,In t rts->
 (A.nlook t(A.c_ty C)=None/\generation_of gens t=t)\/
 (A.nlook t(A.c_ty C)<>None/\A.Fresh W(generation_of gens t)/\
 In(generation_of gens t)(map snd gens)/\In(t,TAbs(generation_of gens t))(th2++th)).
Proof.
 intros c e F C rts gens ss th2 ss' th Hc Rel Out t Hin.
 destruct(output_generations_lookup w R Fl e rts gens Rel t Hin)as(g&Look&Case).
 unfold generation_of;rewrite Look;cbn[opt_default].
 destruct Case as[[Absent Eq]|[Present[Fresh Ne]]].
 - left;split;[rewrite(cr_ty _ _ _ _ _ _ _ Hc),Absent;reflexivity|exact Eq].
 - right;split.
   + rewrite(cr_ty _ _ _ _ _ _ _ Hc);destruct(tn_get e t);[discriminate|contradiction].
   + split;[exact Fresh|]. split.
     * apply in_map_iff;exists(t,g);split;[reflexivity|exact(nassoc_some_in _ _ _ _ Look)].
     * apply in_or_app;left;eapply output_arguments_contains;eauto.
Qed.
End ModelOutputs.
