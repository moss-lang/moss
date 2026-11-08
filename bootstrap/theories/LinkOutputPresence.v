(* Actual made-item checks name provisions present in the completed body. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodySoundCore BodyTr KeyTranslation
 ContextAppsProvided LinkOutputGuards LinkBodyTrace CtxMerge CtxWalk.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Presence.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Lemma runtime_app_provided : forall c e F C key app stamp,
 CxRel w R Fl c e F C -> AppsProvided C -> key_ok key=true ->
 get_app e key=Some(app,stamp) -> A.provides C(key_tr Fl key).
Proof.
 intros c e F C key app stamp Caller Provided Clean Recorded.
 assert(Read:A.ilook(key_tr Fl key)(A.c_apps C)=Some(tr_inst Fl app)).
 {destruct key as[v|f|r m|u];cbn[key_tr tr_inst]in *.
  - rewrite(cr_vapps _ _ _ _ _ _ _ Caller),Recorded;reflexivity.
  - rewrite(cr_fapps _ _ _ _ _ _ _ Caller),Recorded;reflexivity.
  - change(A.ilook(A.mkey(tr Fl r)m)(A.c_apps C)=Some(tr_inst Fl app)).
    rewrite(cr_apps _ _ _ _ _ _ _ Caller r m Clean),Recorded;reflexivity.
  - rewrite(cr_funapps _ _ _ _ _ _ _ Caller),Recorded;reflexivity. }
 eapply context_app_lookup_provided;eassumption.
Qed.
Lemma made_output_provided : forall c eg eb F C key,
 CxRel w R Fl c eb F C -> AppsProvided C -> key_ok key=true ->
 link_output_item_made eg eb key=true -> A.provides C(key_tr Fl key).
Proof.
 intros c eg eb F C key Caller Provided Clean Made.
 unfold link_output_item_made in Made.
 destruct(get_app eb key)as[[app stamp]|]eqn:Recorded.
 - eapply runtime_app_provided;eassumption.
 - destruct key;cbn[key_tr A.provides]in *;try discriminate Made.
   pose proof(cr_funspresent _ _ _ _ _ _ _ Caller n)as Present.
   destruct(nget(en_funs eb)n);[|discriminate Made].
   destruct(A.nlook n(A.c_funs C));[discriminate|discriminate Present].
Qed.
Theorem checked_body_outputs_provided : forall fg bg c eg eb F Cg Cb ns its wrap Fg Fb made tys keys s u final,
 A.FunCtx(BodySoundCore.W w R Fl)(gs_id bg)Cg ns ->
 A.BindItems(BodySoundCore.W w R Fl)P Fg Cg its wrap Cb Fb made ->
 CxRel w R Fl c eb F Cb ->
 link_trace_items_guard fg bg eg eb tys keys s=Good(u,final) ->
 forall key,In key keys -> key_ok(link_trace_body_key eb tys key)=true ->
 A.provides Cb(key_tr Fl(link_trace_body_key eb tys key)).
Proof.
 intros fg bg c eg eb F Cg Cb ns its wrap Fg Fb made tys keys s u final Context Typed Caller Run key Member Clean.
 eapply made_output_provided;[exact Caller| |exact Clean|].
 - eapply binditems_apps;[exact Typed|eapply functx_apps_provided;exact Context].
 - exact(link_output_items_checked fg bg eg eb tys keys s u final Run key Member).
Qed.
End Presence.
