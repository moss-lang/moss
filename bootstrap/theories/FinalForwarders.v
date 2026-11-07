(* Static final-program meaning of symbols generated as abstract-fn forwarders.
   Unlike a current table lookup this fact remains true at recursion markers. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabFrameCore ForwarderHistory ForwarderTrace.
Import ListNotations.
Definition FinalForwarders(w:Elab.world)(P:xprogram)(s:st):Prop:=
 forall key g target,nget(st_syms s)key=Some g->gs_origin g=OForwarder target->key<>0 /\
 exists tg,LoadedAbstract w tg /\gs_id tg=target/\gs_decl g=forwarder_decl tg/\
 gs_mod g=gs_mod tg/\gs_recv g=None/\
 Audit.Elaboration.body_of P key=Some(XCallA target(map XVar(seq 0(List.length(fn_params g))))).
Lemma final_forwarders_keep:forall w P s final,
 (forall key g,nget(st_syms s)key=Some g->nget(st_syms final)key=Some g)->
 FinalForwarders w P final->FinalForwarders w P s.
Proof. intros w P s final Keep Fact key g target Lookup Origin;eapply Fact;[eapply Keep;exact Lookup|exact Origin]. Qed.
Lemma final_forwarders_back:forall w P s final,
 Fr s final->FinalForwarders w P final->FinalForwarders w P s.
Proof. intros w P s final Frame Fact;eapply final_forwarders_keep;[exact(fr_keep _ _ Frame)|exact Fact]. Qed.
