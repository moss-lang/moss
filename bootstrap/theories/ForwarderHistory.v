(* Completion history only for NEW forwarder symbols. Existing rows are
   transported by the checked present-body frame, including generated rows. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame LoaderOrigin
 FnPresentStable ForwarderTrace.
Import ListNotations.
Set Implicit Arguments.
Definition LoadedAbstract(w:Elab.world)(g:gsym):Prop:=
 nget(w_syms w)(gs_id g)=Some g /\gs_kind g=KFn /\
 exists n ps rt,gs_decl g=ADFn RPlain n ps rt None.
Definition ForwarderFact(w:Elab.world)(key:nat)(g:gsym)(target:nat)(s:st):Prop:=
 exists tg,LoadedAbstract w tg /\gs_id tg=target /\
 gs_decl g=forwarder_decl tg /\gs_mod g=gs_mod tg /\gs_recv g=None /\
 nassoc key(st_fns s)=Some(XCallA target(map XVar(seq 0(List.length(fn_params g))))).
Definition forwarder_history(w:Elab.world)(initial final:st):Prop:=
 forall key g target,nget(st_syms final)key=Some g->gs_origin g=OForwarder target->
 nget(st_syms initial)key=None->ForwarderFact w key g target final.
Lemma symbol_below_next:forall s key g,Inv s->nget(st_syms s)key=Some g->key<st_next s.
Proof. intros s key g I E;destruct(Nat.lt_ge_cases key(st_next s));[assumption|]. rewrite(iv_above s I key H)in E;discriminate. Qed.
Lemma forwarder_fact_present:forall w key g target a b,
 ForwarderFact w key g target a->fn_present_stable a b->key<st_next a->ForwarderFact w key g target b.
Proof.
 intros w key g target a b[tg[Loaded[Id[Decl[Home[Recv Body]]]]]] Stable Below.
 exists tg;split;[exact Loaded|]. repeat split;try assumption;eapply Stable;eassumption.
Qed.
Lemma forwarder_history_refl:forall w s,forwarder_history w s s.
Proof. intros w s key g target Lookup Origin Missing;congruence. Qed.
Lemma forwarder_history_same_symbols:forall w s s',st_syms s'=st_syms s->forwarder_history w s s'.
Proof. intros w s s' Eq key g target Lookup Origin Missing;rewrite Eq,Missing in Lookup;discriminate. Qed.
Lemma forwarder_history_trans:forall w a b c,
 Inv b->Fr a b->Fr b c->fn_present_stable b c->
 forwarder_history w a b->forwarder_history w b c->forwarder_history w a c.
Proof.
 intros w a b c IB AB BC Stable First Last key g target Lookup Origin Missing.
 destruct(nget(st_syms b)key)as[old|]eqn:Middle.
 - pose proof(fr_keep _ _ BC _ _ Middle)as Old;rewrite Lookup in Old;injection Old as <-.
   eapply forwarder_fact_present;[eapply First;eassumption|exact Stable|eapply symbol_below_next;eassumption].
 - eapply Last;eassumption.
Qed.
Lemma forwarder_history_trans_except:forall w owner a b c,
 Inv b->Fr a b->Fr b c->owner<st_next a->
 (forall key body,key<>owner->key<st_next b->nassoc key(st_fns b)=Some body->nassoc key(st_fns c)=Some body)->
 forwarder_history w a b->forwarder_history w b c->forwarder_history w a c.
Proof.
 intros w owner a b c IB AB BC Bound Stable First Last key g target Lookup Origin Missing.
 destruct(nget(st_syms b)key)as[old|]eqn:Middle.
 - pose proof(fr_keep _ _ BC _ _ Middle)as Old;rewrite Lookup in Old;injection Old as <-.
   destruct(First key g target Middle Origin Missing)as[tg[Loaded[Id[Decl[Home[Recv Body]]]]]].
   exists tg;split;[exact Loaded|]. repeat split;try assumption.
   apply Stable with(body:=XCallA target(map XVar(seq 0(List.length(fn_params g))))).
   + pose proof(fr_new_above _ _ AB _ _ Missing Middle);lia.
   + eapply symbol_below_next;eassumption.
   + exact Body.
 - eapply Last;eassumption.
Qed.
Lemma forwarder_history_final_facts:forall w initial final,
 SourceSyms(st_syms initial)->Fr initial final->forwarder_history w initial final->
 forall key g target,nget(st_syms final)key=Some g->gs_origin g=OForwarder target->
 ForwarderFact w key g target final.
Proof.
 intros w initial final Source Frame Hist key g target Lookup Origin.
 apply Hist;[exact Lookup|exact Origin|].
 destruct(nget(st_syms initial)key)as[old|]eqn:Old;[|reflexivity].
 pose proof(fr_keep _ _ Frame _ _ Old)as Same;rewrite Lookup in Same;injection Same as <-.
 pose proof(Source _ _ Old);congruence.
Qed.
