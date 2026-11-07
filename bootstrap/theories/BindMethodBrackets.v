(* Actual left-hand method brackets extend the caller's complete type scope. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTy NominalSupport CanonBodyTy CanonTypes Merge OutputScopeExec MethodBindRead
 InfoPreserve ElabKeeps FwdReadonly FwdInvariant BindMethodTrace.
Import ListNotations.
Local Open Scope list_scope.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements app_subst].

Lemma ok_method_bind_types : forall e mi a, ok (method_bind_types e mi a).
Proof.
 intros e mi [bs|]; unfold method_bind_types; [|apply ok_ret].
 apply ok_bind; [apply ok_app_subst|intro; apply ok_ret].
Qed.

Lemma okI_method_bind_types : forall e mi a, okI (method_bind_types e mi a).
Proof.
 intros e mi [bs|]; unfold method_bind_types; [|apply okI_ret].
 apply okI_bind; [apply okI_substitution_read|intro; apply okI_ret].
Qed.

Lemma okF_method_bind_types : forall e mi a, okF (method_bind_types e mi a).
Proof.
 intros e mi a; apply okF_readonly; [apply ok_method_bind_types|].
 destruct a as [bs|]; unfold method_bind_types; [|apply keeps_ret].
 apply keeps_bind; [apply kw_app_subst|intro;apply keeps_ret].
Qed.

Section Snapshot.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Notation W := (BodySoundCore.W w R Fl).
Theorem method_bind_types_snapshot : forall c e F C a s tm final,
 method_bind_types e (cx_mod c) a s = Good (tm,final) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl final ->
 exists bs,
 A.OutputBrackets W (A.fr_mod F) (A.c_ty C) a bs /\
 (forall key, A.nlook key (bs ++ A.c_ty C) =
    option_map (fun y => tr Fl (canon e y)) (nassoc key tm)) /\
 (forall key ty, nassoc key tm = Some ty -> WFty ty) /\
 (forall key ty, nassoc key tm = Some ty -> Support W ty) /\
 TmRel Fl tm (map (trp Fl) tm) e.
Proof.
 intros c e F C a s tm final Run Valid World Caller Last.
 assert (Raw : exists sub, tm = sub ++ tymap e /\
   (match a with
    | Some brackets => let! xs := app_subst 200 e (cx_mod c) brackets [] in ret (xs ++ [])
    | None => ret []
    end) s = Good (sub,final)).
 { destruct a as [brackets|].
   - unfold method_bind_types in Run.
     apply bind_inv in Run as [sub [middle [Read Return]]].
     injection Return as <- <-.
     exists sub; split; [reflexivity|].
     unfold mbind; rewrite Read; cbn [ret]; now rewrite app_nil_r.
   - unfold method_bind_types in Run; injection Run as <- <-.
     exists []; split; reflexivity. }
 destruct Raw as [sub [-> Scope]].
 destruct (output_scope_execution w (P:=P) R Fl b Load c e F C (cx_mod c) a [] [] s sub final
   Scope Valid World (cr_mod_in _ _ _ _ _ _ _ Caller) Last Caller
   (Forall_nil _) (Forall_nil _) eq_refl) as
   (bs & Brackets & Mapping & Wellformed & Supported & Types & Support & Meaning).
 exists bs; split.
 - rewrite (cr_mod _ _ _ _ _ _ _ Caller); exact Brackets.
 - split; [exact Meaning|]. split; [exact (proj1 (proj2 Types))|].
   split; assumption.
Qed.

Theorem method_bind_types_brackets : forall c e F C a m s tm final,
 method_bind_types e (cx_mod c) a s = Good (tm,final) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl final ->
 A.kind_sym W m = Some A.DKMeth ->
 exists bs,
 A.Brackets W (A.fr_mod F) (A.c_ty C) m a bs /\
 (forall key, A.nlook key (bs ++ A.c_ty C) =
    option_map (fun y => tr Fl (canon e y)) (nassoc key tm)) /\
 (forall key ty, nassoc key tm = Some ty -> WFty ty) /\
 (forall key ty, nassoc key tm = Some ty -> Support W ty) /\
 TmRel Fl tm (map (trp Fl) tm) e.
Proof.
 intros c e F C a m s tm final Run Valid World Caller Last Kind.
 destruct (method_bind_types_snapshot c e F C a s tm final Run Valid World Caller Last)
   as (bs & Brackets & Meaning & Wellformed & Supported & Types).
 exists bs; split; [apply method_output_brackets; assumption|].
 split; [exact Meaning|]; split; [exact Wellformed|]; split; assumption.
Qed.
End Snapshot.
