(* A wrapper's retained fn_info receipt supplies its own declaration context,
   even if later cache activity has installed a different snapshot. *)
From Stdlib Require Import List String Bool.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 InfoInvariant InfoPreserve InfoSnapshot BodySound FnBodyContext LoadedContextSupport
 InfoCache CtxWalk CtxMerge ContextClean NominalSupport Merge CanonTypes BodyTy BodyTr
 NeedInfoExec MainContextInfo MainRoot SourceSatisfiesAny LoaderOrigin.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma info_snapshot_later:forall w P R Fl s key info,
 Later w P R Fl s->Later w P R Fl(info_snapshot s key info).
Proof. intros;destruct H;constructor;assumption. Qed.
Section Context.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Lemma retained_info_context:forall g s info,
 nget(w_syms w)(gs_id g)=Some g->InfoEntry s(gs_id g)info->
 Inv s->InfoInv s->st_w s=w->Later w P R Fl s->
 FnCtx w R Fl g info /\ContextClean(fi_env info)/\
 Tri(en_uf(fi_env info))/\EnvSupport(BodySound.W w R Fl)(fi_env info).
Proof.
 intros g s info Loaded Entry Inv Cache Hw Late.
 pose proof(info_snapshot_inv s(gs_id g)info Inv)as Iv.
 pose proof(info_snapshot_cache s(gs_id g)info Cache Entry)as Ic.
 pose proof(info_snapshot_read s g info)as Run.
 pose proof(info_snapshot_later w P R Fl s(gs_id g)info Late)as Last.
 split.
 - eapply fn_info_context_loaded;[exact Hload|exact Hworld|exact Loaded|exact Run|
     exact Iv|exact Ic|exact Hw|exact Last].
 - eapply fn_info_valid_loaded;[exact Hload|exact Hworld|exact Loaded|exact Run|
     exact Iv|exact Ic|exact Hw|exact Last].
Qed.
Lemma retained_info_keys:forall g s info,
 nget(w_syms w)(gs_id g)=Some g->InfoEntry s(gs_id g)info->
 Inv s->InfoInv s->st_w s=w->Later w P R Fl s->
 Forall(fun k=>key_ok k=true)(fi_needs info).
Proof.
 intros g s info Loaded Entry Iv Cache Hw Late.
 set(snapshot:=info_snapshot s(gs_id g)info).
 assert(Is:Inv snapshot)by(apply info_snapshot_inv;exact Iv).
 assert(Ic:InfoInv snapshot)by(apply info_snapshot_cache;assumption).
 assert(Run:fn_info g snapshot=Good(info,snapshot))by(apply info_snapshot_read).
 assert(Last:Later w P R Fl snapshot)by(apply info_snapshot_later;exact Late).
 destruct(is_main_total g snapshot)as[main Main].
 pose proof(is_main_spec w R Fl b Hload g snapshot main snapshot Loaded Hw Main)as Spec.
 destruct main.
 - assert(IsMain:A.IsMainFn(BodySound.W w R Fl)(gs_id g))by(apply(proj1 Spec);reflexivity).
   exact(proj2(proj2(fn_info_main w(P:=P)R Fl b Hload Hworld(@A.wo_fld b _ Hworld)
     g snapshot info snapshot Loaded IsMain Run Is Ic Hw Last))).
 - assert(NotMain:~A.IsMainFn(BodySound.W w R Fl)(gs_id g))by(intro M;apply(proj2 Spec)in M;discriminate M).
   exact(fn_info_needs_ok_any_nonroot w R Fl b Hload g snapshot info snapshot Loaded
     (loaded_no_forwarder b w g Hload Loaded)(not_main w R Fl b Hload g Loaded NotMain)
     Run Is Ic Hw Last).
Qed.
Lemma retained_info_cxrel:forall g s info,
 nget(w_syms w)(gs_id g)=Some g->InfoEntry s(gs_id g)info->
 Inv s->InfoInv s->st_w s=w->Later w P R Fl s->
 exists C,A.CtxOf(BodySound.W w R Fl)(gs_id g)C(map(key_tr Fl)(fi_needs info))/\
 CxRel w R Fl(initial_body_context g(FSig[]YUnit None))(fi_env info)
   (initial_body_frame w Fl g(fi_env info)(FSig[]YUnit None)[])C.
Proof.
 intros g s info Loaded Entry Iv Cache Hw Late.
 destruct(retained_info_context g s info Loaded Entry Iv Cache Hw Late)as[Ctx[Clean[TriU Supported]]].
 assert(Locals:en_locals(fi_env info)=[]).
 {exact(fn_info_loaded_locals_empty b w g(info_snapshot s(gs_id g)info)info
   (info_snapshot s(gs_id g)info)Hload Hw Loaded
   (info_snapshot_cache _ _ _ Cache Entry)(info_snapshot_read s g info)). }
 eapply fn_ctx_initial_body;[exact Ctx|exact Clean|exact TriU|exact Supported|exact Locals|
   exact(BodyTy.loader_mod_in w R Fl b Hload _ _ Loaded)| |].
 - unfold SigOK;cbn;split;[constructor|auto].
 - unfold SigSupport;cbn;split;[constructor|split;[constructor|exact I]].
Qed.
End Context.
