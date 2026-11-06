(* Settled actual context needs have structurally clean method receivers. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodySound CtxWalk CtxMerge.
Import ListNotations.
Lemma ws_needs_ok:forall Fl U e tm les needs,
 WS Fl U e tm les needs->en_uf e=U->Forall(fun k=>key_ok k=true)needs.
Proof.
 intros Fl U e tm les needs W E. apply Forall_forall. intros k Hk.
 destruct(proj1(ws_needs _ _ _ _ _ _ W E k)Hk)as(l&Hin&Ek).
 destruct l as[t|v y a|f sg a|r m sg a|u];cbn[lkey]in Ek;try discriminate Ek;
 injection Ek as <-;cbn[key_ok];try reflexivity.
 exact(proj1(ws_mths _ _ _ _ _ _ W E r m sg a Hin)).
Qed.
