(* Declaration-cycle validation supplies the actual depth of family walks. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame.
Import ListNotations.
Definition context_check_specs(fuel:nat)(w:Elab.world)(stack:list nat)(mi:nat):list spec->res unit:=
 fix go(specs:list spec):res unit:=match specs with
 |[]=>Good tt
 |Spec(Some p)None _::rest=>
   let* target:=resolve_path w mi p in
   let* _:=match target with
     |Some(ESym x)=>match nget(w_syms w)x with
       |Some h=>if kind_eqb(gs_kind h)KCtx then context_acyclic fuel w stack h else Good tt
       |None=>Good tt end
     |_=>Good tt end in go rest
 |_::rest=>go rest end.
Definition context_check_roots(w:Elab.world):list nat->res unit:=
 fix go(ids:list nat):res unit:=match ids with
 |[]=>Good tt
 |x::rest=>let* _:=match nget(w_syms w)x with
     |Some g=>context_acyclic(S(w_nsyms w))w[]g|None=>Good tt end in
   go rest end.
Lemma context_check_roots_member:forall w ids x g,
 context_check_roots w ids=Good tt->In x ids->nget(w_syms w)x=Some g->
 context_acyclic(S(w_nsyms w))w[]g=Good tt.
Proof.
 intros w ids;induction ids as[|i ids IH];intros x g E H Hg;[contradiction|].
 cbn[context_check_roots]in E;apply rbind_good in E as([]&Head&Tail).
 destruct H as[<-|H];[rewrite Hg in Head;exact Head|eapply IH;eauto].
Qed.
Theorem check_contexts_loaded:forall b w s sf g,
 load_world b=Good w->check_contexts s=Good(tt,sf)->st_w s=w->
 nget(w_syms w)(gs_id g)=Some g->
 sf=s/\context_acyclic(S(w_nsyms w))w[]g=Good tt.
Proof.
 intros b w s sf g Hl E Hw Hg.
 unfold check_contexts in E;cbn[mbind get]in E;rewrite Hw in E.
 change(lift(context_check_roots w(seq 0(w_nsyms w)))s=Good(tt,sf))in E.
 unfold lift in E;destruct(context_check_roots w(seq 0(w_nsyms w)))as[[]|err]eqn:Check;
 [|discriminate E]. injection E as <-;split;[reflexivity|].
 eapply context_check_roots_member;[exact Check| |exact Hg].
 apply in_seq;split;[lia|].
 destruct(load_world_syms b w Hl)as[Pos Bound].
 destruct(Nat.lt_ge_cases(gs_id g)(w_nsyms w));[assumption|].
 rewrite(Bound _ H)in Hg;discriminate Hg.
Qed.
Lemma context_check_specs_member:forall fuel w stack mi specs p a h,
 context_check_specs fuel w stack mi specs=Good tt->In(Spec(Some p)None a)specs->
 resolve_path w mi p=Good(Some(ESym(gs_id h)))->nget(w_syms w)(gs_id h)=Some h->
 gs_kind h=KCtx->context_acyclic fuel w stack h=Good tt.
Proof.
 intros fuel w stack mi specs;induction specs as[|[pa dn br]rest IH];intros p a h E Hin Rp Hg K;
 [contradiction|].
 destruct Hin as[Eq|Hin].
 - inversion Eq;subst;cbn[context_check_specs]in E.
   rewrite Rp in E;cbn[rbind]in E;rewrite Hg,K in E;cbn[kind_eqb]in E;apply rbind_good in E as([]&Read&Tail);exact Read.
 - destruct pa as[pp|],dn as[dd|];cbn[context_check_specs]in E.
   + eapply IH;eauto.
   + apply rbind_good in E as(target&Read&E);apply rbind_good in E as([]&Head&Tail).
     eapply IH;eauto.
   + eapply IH;eauto.
   + eapply IH;eauto.
Qed.
Theorem context_acyclic_child:forall fuel w stack g name specs p a h,
 context_acyclic(S fuel)w stack g=Good tt->gs_kind g=KCtx->gs_decl g=ADCtx name specs->
 In(Spec(Some p)None a)specs->resolve_path w(gs_mod g)p=Good(Some(ESym(gs_id h)))->
 nget(w_syms w)(gs_id h)=Some h->gs_kind h=KCtx->
 context_acyclic fuel w(gs_id g::stack)h=Good tt.
Proof.
 intros fuel w stack g name specs p a h E K D Hin Rp Hg Kh.
 cbn[context_acyclic]in E;destruct(nmem(gs_id g)stack);[discriminate E|].
 rewrite K,D in E.
 change(context_check_specs fuel w(gs_id g::stack)(gs_mod g)specs=Good tt)in E.
 eapply context_check_specs_member;eauto.
Qed.
Lemma context_acyclic_notin:forall fuel w stack g,
 context_acyclic fuel w stack g=Good tt->~In(gs_id g)stack.
Proof.
 intros fuel w stack g E H;assert(nmem(gs_id g)stack=true)by(apply nmem_in;exact H).
 destruct fuel;cbn[context_acyclic]in E;rewrite H0 in E;discriminate E.
Qed.
Lemma context_acyclic_specs:forall fuel w stack g name specs,
 context_acyclic(S fuel)w stack g=Good tt->gs_kind g=KCtx->gs_decl g=ADCtx name specs->
 context_check_specs fuel w(gs_id g::stack)(gs_mod g)specs=Good tt.
Proof.
 intros fuel w stack g name specs E K D.
 cbn[context_acyclic]in E;destruct(nmem(gs_id g)stack);[discriminate E|].
 rewrite K,D in E;exact E.
Qed.
Lemma context_resolve_path:forall mi pa s g sf,
 Inv s->resolve_sym mi pa s=Good(Some g,sf)->
 sf=s/\resolve_path(st_w s)mi pa=Good(Some(ESym(gs_id g))).
Proof.
 intros mi pa s g sf Hi E;unfold resolve_sym in E;apply bind_inv in E as(r&sr&Rp&E).
 unfold rpath in Rp;destruct(resolve_path(st_w s)mi pa)as[target|err]eqn:Path;[|discriminate Rp].
 injection Rp as <- <-;destruct target as[[id|i]|];try discriminate E.
 apply bind_inv in E as(g0&ss&Read&E);injection E as <- <-.
 unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate Read].
 injection Read as <- <-;split;[reflexivity|].
 rewrite(iv_ids _ Hi id got Lookup);reflexivity.
Qed.
