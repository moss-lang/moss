(* Every functor entry has its recorded fixed-input app, including the empty app. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound CtxWalk.
Import ListNotations.
Lemma lfirst_fun_present:forall e u les,
 lfirst e(KyFun u)les<>None<->exists fx,In(LFun u fx)les.
Proof.
 intros e u les;induction les as[|l les IH].
 - split;[intro H;exfalso;exact(H eq_refl)|intros[fx[]]].
 - destruct l as[t|v y a|f g a|r m sg a|v fx];cbn[lfirst lapp key_eqb];
   try solve[rewrite IH;split;
    [intros[fx H];exists fx;right;exact H|intros[fx[H|H]];[discriminate H|exists fx;exact H]]].
   destruct(Nat.eqb_spec u v)as[->|Ne].
   + split;[intros _;exists fx;left;reflexivity|intros _;discriminate].
   + rewrite IH;split.
     * intros[fx0 H];exists fx0;right;exact H.
     * intros[fx0[Eq|H]];[injection Eq as Eq _;congruence|exists fx0;exact H].
Qed.
Lemma ws_fun_app_presence:forall Fl U e tm les needs,
 WS Fl U e tm les needs->en_uf e=U->forall u,
 is_some(get_app e(KyFun u))=is_some(nget(en_funs e)u).
Proof.
 intros Fl U e tm les needs W E u.
 pose proof(ws_apps _ _ _ _ _ _ W E(KyFun u)eq_refl)as App.
 destruct(get_app e(KyFun u))eqn:G;destruct(nget(en_funs e)u)eqn:F;try reflexivity.
 - assert(Has:lfirst e(KyFun u)les<>None)by(rewrite<-App;discriminate).
   apply(proj1(lfirst_fun_present e u les))in Has;apply(proj2(ws_funs _ _ _ _ _ _ W u))in Has.
   rewrite F in Has;exfalso;exact(Has eq_refl).
 - assert(Has:exists fx,In(LFun u fx)les).
   { apply(proj1(ws_funs _ _ _ _ _ _ W u));rewrite F;discriminate. }
   apply(proj2(lfirst_fun_present e u les))in Has;rewrite<-App in Has;exfalso;exact(Has eq_refl).
Qed.
