(* Provenance of visible source-function bodies. Recursive XUnit markers are
   deliberately not treated as completed bodies or as a unique table row. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame InfoInvariant FwdInvariant ReqsSolve.
Import ListNotations.
Set Implicit Arguments.

Definition loaded_fn_key(w:Elab.world)(f:nat):Prop:=
 exists g,nget(w_syms w)f=Some g.
Definition completed_source_fn(w:Elab.world)(observed:st)(f:nat)(body:xexpr):Prop:=
 exists fuel g initial final,
 nget(w_syms w)f=Some g /\gs_id g=f /\
 Inv initial /\InfoInv initial /\FwdInv initial /\st_w initial=w /\
 nassoc f(st_fns initial)=None /\
 lower_body fuel g initial=Good(tt,final) /\
 nassoc f(st_fns final)=Some body /\FrL final observed.

Lemma completed_source_fn_extend:forall w a b f body,
 completed_source_fn w a f body->FrL a b->completed_source_fn w b f body.
Proof.
 intros w a b f body(fuel&g&initial&final&Loaded&Id&Inv&Info&Fwd&World&Absent&Run&Lookup&Frame)Later.
 exists fuel,g,initial,final;repeat first[assumption|solve[eapply frl_trans;eassumption]|split].
Qed.
Lemma frl_emit_body:forall s f body,FrL s(s_fns(fun fs=>(f,body)::fs)s).
Proof. intros;constructor;cbn;auto;[exists[];rewrite app_nil_r;reflexivity|apply ext_refl]. Qed.

Definition fn_history(w:Elab.world)(initial final:st):Prop:=
 forall f body,loaded_fn_key w f ->nassoc f(st_fns final)=Some body ->
 nassoc f(st_fns initial)=Some body \/completed_source_fn w final f body.
Definition fn_history_except(w:Elab.world)(owner:nat)(initial final:st):Prop:=
 forall f body,loaded_fn_key w f ->f<>owner ->nassoc f(st_fns final)=Some body ->
 nassoc f(st_fns initial)=Some body \/completed_source_fn w final f body.

Lemma fn_history_refl:forall w s,fn_history w s s.
Proof. intros w s f body Loaded Lookup;left;exact Lookup. Qed.
Lemma fn_history_trans:forall w a b c,
 fn_history w a b ->fn_history w b c ->FrL b c->fn_history w a c.
Proof.
 intros w a b c AB BC Frame f body Loaded Lookup.
 destruct(BC f body Loaded Lookup)as[Old|Done];[|right;exact Done].
 destruct(AB f body Loaded Old)as[Earlier|Done];[left;exact Earlier|right;eapply completed_source_fn_extend;eassumption].
Qed.
Lemma fn_history_same:forall w a b,st_fns b=st_fns a ->fn_history w a b.
Proof. intros w a b E f body Loaded Lookup;left;rewrite <-E;exact Lookup. Qed.
Lemma fn_history_except_refl:forall w owner s,fn_history_except w owner s s.
Proof. intros w owner s f body Loaded Other Lookup;left;exact Lookup. Qed.
Lemma fn_history_except_trans:forall w owner a b c,
 fn_history_except w owner a b ->fn_history_except w owner b c ->FrL b c->fn_history_except w owner a c.
Proof.
 intros w owner a b c AB BC Frame f body Loaded Other Lookup.
 destruct(BC f body Loaded Other Lookup)as[Old|Done];[|right;exact Done].
 destruct(AB f body Loaded Other Old)as[Earlier|Done];[left;exact Earlier|right;eapply completed_source_fn_extend;eassumption].
Qed.
Lemma fn_history_drop_owner:forall w owner a b,fn_history w a b ->fn_history_except w owner a b.
Proof. intros w owner a b Hist f body Loaded Other Lookup;apply Hist;assumption. Qed.

Lemma loaded_fn_below_next:forall b s f,
 load_world b=Good(st_w s)->Inv s ->loaded_fn_key(st_w s)f ->f<st_next s.
Proof.
 intros b s f Load InvS[g Loaded].
 pose proof(iv_loader s InvS b Load f g Loaded)as Present.
 destruct(Nat.lt_ge_cases f(st_next s));[assumption|].
 rewrite(iv_above s InvS f H)in Present;discriminate.
Qed.

Lemma fn_history_emit_fresh:forall w s key body,
 ~loaded_fn_key w key ->fn_history w s(s_fns(fun fs=>(key,body)::fs)s).
Proof.
 intros w s key body Fresh f value Loaded Lookup.
 cbn[s_fns st_fns nassoc assoc]in Lookup.
 destruct(Nat.eqb f key)eqn:Same.
 - apply Nat.eqb_eq in Same;subst f;contradiction.
 - left;exact Lookup.
Qed.
Lemma fn_history_emit_generated:forall b s key body,
 load_world b=Good(st_w s)->Inv s ->st_next s<=key ->
 fn_history(st_w s)s(s_fns(fun fs=>(key,body)::fs)s).
Proof.
 intros b s key body Load InvS Fresh;apply fn_history_emit_fresh.
 intro Loaded;pose proof(@loaded_fn_below_next b s key Load InvS Loaded);lia.
Qed.

Lemma fn_history_emit_owner:forall w owner s body,
 fn_history_except w owner s(s_fns(fun fs=>(owner,body)::fs)s).
Proof.
 intros w owner s body f value Loaded Other Lookup.
 cbn[s_fns st_fns nassoc assoc]in Lookup.
 destruct(Nat.eqb f owner)eqn:Same;[apply Nat.eqb_eq in Same;contradiction|left;exact Lookup].
Qed.

(* Finalization replaces the temporary marker for its own key. Only the
   other keys need a recursive history; the owner has a real completed run. *)
Lemma fn_history_finalize:forall w owner initial middle body,
 fn_history_except w owner initial middle ->completed_source_fn w(s_fns(fun fs=>(owner,body)::fs)middle)owner body ->
 fn_history w initial(s_fns(fun fs=>(owner,body)::fs)middle).
Proof.
 intros w owner initial middle body Hist Done f value Loaded Lookup.
 cbn[s_fns st_fns nassoc assoc]in Lookup.
 destruct(Nat.eqb f owner)eqn:Same.
 - apply Nat.eqb_eq in Same;subst f;injection Lookup as <-;right;exact Done.
 - destruct(Hist f value Loaded (proj1(Nat.eqb_neq _ _)Same)Lookup)as[Old|Earlier];[left;exact Old|].
   right;eapply completed_source_fn_extend;[exact Earlier|apply frl_emit_body].
Qed.

Lemma fn_history_from_empty:forall w initial final,
 st_fns initial=[] ->fn_history w initial final ->forall f body,
 loaded_fn_key w f ->nassoc f(st_fns final)=Some body ->completed_source_fn w final f body.
Proof.
 intros w initial final Empty Hist f body Loaded Lookup.
 destruct(Hist f body Loaded Lookup)as[Old|Done];[rewrite Empty in Old;discriminate|exact Done].
Qed.
