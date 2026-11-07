(* Declaration support through actual assumption walking. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy
 CtxWalk Merge CanonTypes SourceClean ContextClean MethodClean ContextWalkClean
 NominalSupport NominalMergeSupport SourceSupport ContextSupport.
Import ListNotations.
Section Walk.
Variable w : Elab.world. Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySoundCore.W w R Fl).
Ltac impossible H := exfalso; solve [repeat (cbn beta iota zeta in H;
  first [exact (err_not_good _ _ _ _ _ _ H) | discriminate H |
         apply bind_inv in H as (? & ? & ? & H)])].

(* Each extracted prefix preserves the state invariant; its remaining suffix
   supplies the eventual declaration cache reading needed by source typing. *)
Ltac cstep H Hi Hw L a si E Ii Wi Li :=
  match type of H with mbind ?m ?k ?s = Good (?v, ?sf) =>
    let O := fresh "O" in assert (O : ok m) by okt;
    apply bind_inv in H as (a & si & E & H);
    let F := fresh "F" in destruct (O _ _ _ Hi E) as [Ii F];
    pose proof (fr_w' w _ _ F Hw) as Wi;
    let Q := fresh "Q" in assert (Q : ok (k a)) by okt;
    let It := fresh "It" in let Ft := fresh "Ft" in destruct (Q _ _ _ Ii H) as [It Ft];
    pose proof (later_back w P R Fl _ _ Ft L) as Li;
    clear O Q It Ft
  end.

Lemma method_context_support : forall e mi recv n subst needs s e' needs' s',
 add_method_item e mi recv n subst needs s = Good ((e',needs'),s') ->
 Inv s -> st_w s = w -> In mi (w_order w) -> nget (w_syms w) (gs_id recv) = Some recv ->
 TmClean subst -> Forall (fun p => Support WW (snd p)) subst ->
 Forall (fun p => WFty (snd p)) subst -> ContextClean e -> Tri (en_uf e) ->
 EnvSupport WW e -> Later w P R Fl s' -> EnvSupport WW e'.
Proof.
 intros e mi recv n subst needs s e' needs' s' H Hi Hw Hmi Hr Ts Ss Ws C T S L.
 unfold add_method_item in H. apply bind_inv in H as (mo & s1 & E1 & H).
 destruct mo as [m|]; [|impossible H].
 destruct (find_method_decl_loaded b w R Fl mi recv n s m s1 Hload Hmi Hr Hi Hw E1) as [-> Hm].
 assert (Tm : TmClean (subst ++ tymap e)) by (apply tmclean_app; [exact Ts|exact (context_clean_tymap _ C)]).
 assert (Stm : Forall (fun p => Support WW (snd p)) (subst ++ tymap e)).
 { apply Forall_app; split; [exact Ss|exact (tymap_support WW e S)]. }
 assert (Wtm : Forall (fun p => WFty (snd p)) (subst ++ tymap e)).
 { apply Forall_app; split; [exact Ws|exact (context_tymap_wf e C)]. }
 cstep H Hi Hw L this s1 Ethis I1 W1 L1.
 assert (Sthis : Support WW this).
 { destruct (nassoc (gs_id recv) (subst ++ tymap e)) eqn:Nt.
   - injection Ethis as <- _. exact (support_tm_lookup WW _ _ _ Stm Nt).
   - eapply (symbol_type_support w R Fl b Hload); try eassumption.
     intros k y Ey. exact (context_clean_types_wf e k y C Ey). }
 assert (Othis : ety_ok this = true).
 { destruct (nassoc (gs_id recv) (subst ++ tymap e)) eqn:Nt.
   - injection Ethis as <- _. exact (Tm _ _ Nt).
   - exact (proj2 (proj2 (source_clean w R Fl b Hload 200)) mi recv (subst ++ tymap e) e s this s1 Ethis Hi Hw Hr Tm (cc_types _ C) L1). }
 cstep H I1 W1 L sg s2 Esg I2 W2 L2.
 assert (Ssg : SigSupport WW sg).
 { eapply (fn_sig_support w R Fl b Hload); try eassumption.
   - exact (loader_mod_in w R Fl b Hload _ _ Hm).
   - intros k y Ey. exact (context_clean_types_wf e k y C Ey).
   - split; [exact Sthis|exact (ety_ok_wf _ Othis)]. }
 cstep H I2 W2 L k s3 Ek I3 W3 L3.
 assert (K : k = KyMth (canon e this) (gs_id m)).
 { unfold method_key in Ek. destruct (head (canon e this)), (gs_recv m); try (injection Ek as <- _; reflexivity).
   impossible Ek. }
 subst k.
 cstep H I3 W3 L ec s4 Eec I4 W4 L4.
 assert (Sc : EnvSupport WW ec).
 { destruct (get_mth e (canon e this) (gs_id m)) as [old|] eqn:Old.
   - eapply (@unify_sig_env_support WW Fl); [exact S|exact T|exact (proj2 (@es_mths WW e S _ _ _ Old))|exact Ssg|exact Eec].
   - injection Eec as <- _. apply env_support_method; [exact S| |exact Ssg].
     apply canon_support; [exact (@es_uf WW e S)|exact Sthis]. }
 cstep H I4 W4 L a s5 Ea I5 W5 L5.
 apply bind_inv in H as (ee & s6 & Eee & H). injection H as <- <- <-.
 exact (setdefault_support WW ec _ a s5 ee s6 Sc Eee).
Qed.

Definition SymWalkSupport f : Prop := forall e mi g subst needs s e' needs' s',
 add_symbol_item f e mi g subst needs s = Good((e',needs'),s') ->
 Inv s -> st_w s=w -> In mi(w_order w) -> nget(w_syms w)(gs_id g)=Some g ->
 TmClean subst -> Forall(fun p=>Support WW(snd p))subst -> Forall(fun p=>WFty(snd p))subst ->
 ContextClean e -> Tri(en_uf e) -> EnvSupport WW e -> Later w P R Fl s' -> EnvSupport WW e'.
Definition SpecWalkSupport f : Prop := forall e mi sp subst needs s e' needs' s',
 add_spec f e mi sp subst needs s = Good((e',needs'),s') ->
 Inv s -> st_w s=w -> In mi(w_order w) ->
 TmClean subst -> Forall(fun p=>Support WW(snd p))subst -> Forall(fun p=>WFty(snd p))subst ->
 ContextClean e -> Tri(en_uf e) -> EnvSupport WW e -> Later w P R Fl s' -> EnvSupport WW e'.
Lemma sym_walk_support_step : forall f, SpecWalkSupport f -> SymWalkSupport(S f).
Proof.
 intros f IH e mi g subst needs s e' needs' s' H Hi Hw Hmi Hg Ts Ss Ws C T S L.
 cbn[add_symbol_item]in H. cstep H Hi Hw L u s1 Etotal I1 W1 L1. cbn beta zeta in H.
 assert(Stm:Forall(fun p=>Support WW(snd p))(subst++tymap e)).
 { apply Forall_app;split;[exact Ss|exact(tymap_support WW e S)]. }
 assert(Wtm:Forall(fun p=>WFty(snd p))(subst++tymap e)).
 { apply Forall_app;split;[exact Ws|exact(context_tymap_wf e C)]. }
 pose proof(loader_mod_in w R Fl b Hload _ _ Hg)as Hgm.
 destruct(gs_kind g).
 - destruct(tn_get e(gs_id g));[injection H as <- <- <-;exact S|].
   apply bind_inv in H as(stamp&s2&E&H). injection H as <- <- <-.
   apply env_support_type;[exact S|constructor].
 - injection H as <- <- <-;exact S.
 - injection H as <- <- <-;exact S.
 - injection H as <- <- <-;exact S.
 - destruct(gs_decl g)as[| | | | |n t init| | |];try impossible H.
   destruct init;[injection H as <- <- <-;exact S|].
   cstep H I1 W1 L y s2 Ey I2 W2 L2.
   assert(Sy:Support WW y).
   { eapply(ety_of_support w R Fl b Hload);try eassumption.
     - intros k v E. exact(context_clean_types_wf e k v C E).
     - exact I. }
   cstep H I2 W2 L ec s3 Eec I3 W3 L3.
   assert(Sc:EnvSupport WW ec).
   { destruct(nget(en_vals e)(gs_id g))as[old|]eqn:Old.
     - eapply(@unify_env_support WW Fl);[exact S|exact T|exact(@es_vals WW e S _ _ Old)|exact Sy|exact Eec].
     - injection Eec as <- _. apply env_support_val;assumption. }
   cstep H I3 W3 L a s4 Ea I4 W4 L4.
   apply bind_inv in H as(ee&s5&Eee&H). injection H as <- <- <-.
   exact(setdefault_support WW ec _ a s4 ee s5 Sc Eee).
 - cstep H I1 W1 L sg s2 Esg I2 W2 L2.
   assert(Ssg:SigSupport WW sg).
   { eapply(fn_sig_support w R Fl b Hload);try eassumption.
     - intros k v E. exact(context_clean_types_wf e k v C E).
     - exact I. }
   cstep H I2 W2 L ec s3 Eec I3 W3 L3.
   assert(Sc:EnvSupport WW ec).
   { destruct(nget(en_fns e)(gs_id g))as[old|]eqn:Old.
     - eapply(@unify_sig_env_support WW Fl);[exact S|exact T|exact(@es_fns WW e S _ _ Old)|exact Ssg|exact Eec].
     - injection Eec as <- _. apply env_support_fn;assumption. }
   cstep H I3 W3 L a s4 Ea I4 W4 L4.
   cstep H I4 W4 L mem s5 Emem I5 W5 L5.
   cstep H I5 W5 L ms s6 Ems I6 W6 L6. cbn beta zeta in H.
   apply bind_inv in H as(ee&s7&Eee&H). injection H as <- <- <-.
   eapply setdefault_support;[exact Sc|exact Eee].
 - destruct(gs_decl g)as[| | | | | | |n its|];try impossible H.
   assert(Gen:forall its0 acc s0 res s3,
    mfold(fun acc sp=>add_spec f(fst acc)(gs_mod g)sp subst(snd acc))acc its0 s0=Good(res,s3)->
    Inv s0->st_w s0=w->ContextClean(fst acc)->Tri(en_uf(fst acc))->EnvSupport WW(fst acc)->
    Later w P R Fl s3->EnvSupport WW(fst res)).
   { induction its0 as[|sp rest IR];intros[ce ns]s0[re nr]s3 Hf Ii Wi Ci Ti Si Li.
     - injection Hf as <- <- <-;exact Si.
     - cbn[mfold]in Hf. cstep Hf Ii Wi Li a si Ea Ia Wa La. destruct a as[ae an];cbn in *.
       pose proof(IH ce(gs_mod g)sp subst ns s0 ae an si Ea Ii Wi Hgm Ts Ss Ws Ci Ti Si La)as Sa.
       destruct(proj2(context_walk_clean w R Fl b Hload f)ce(gs_mod g)sp subst ns s0 ae an si Ea Ii Wi Hgm Ts Ci Ti La)as[Ca Ta].
       exact(IR(ae,an)si(re,nr)s3 Hf Ia Wa Ca Ta Sa Li). }
   exact(Gen its(e,needs)s1(e',needs')s' H I1 W1 C T S L).
 - impossible H.
 - destruct(gs_decl g)as[| | | | | | | |n ps args res body];try impossible H.
   destruct body;[impossible H|].
   cstep H I1 W1 L stamp s2 Estamp I2 W2 L2.
   cstep H I2 W2 L xs s3 Exs I3 W3 L3.
   cstep H I3 W3 L u0 s4 Eu0 I4 W4 L4. cbn beta zeta in H.
   apply bind_inv in H as(ee&s5&Eee&H). injection H as <- <- <-.
   eapply setdefault_support;[apply env_support_fun;exact S|exact Eee].
Qed.
Lemma spec_walk_support_step : forall f, SymWalkSupport f -> SpecWalkSupport(S f).
Proof.
 intros f IH e mi[p d a]outer needs s e' needs' s' H Hi Hw Hmi Ts Ss Ws C T S L.
 cbn[add_spec]in H. cstep H Hi Hw L subst0 s1 Esub I1 W1 L1.
 assert(Ts0:TmClean subst0).
 { destruct a as[bs|].
   - apply bind_inv in Esub as(ys&sy&Eys&Esub). injection Esub as <- <-.
     apply tmclean_app;[apply tmclean_of_forall|exact Ts].
     exact(proj1(proj2(source_clean w R Fl b Hload 200))e mi bs outer s ys sy Eys Hi Hw Hmi Ts(cc_types _ C)L1).
   - injection Esub as <- _;exact Ts. }
 assert(Ss0:Forall(fun p=>Support WW(snd p))subst0).
 { destruct a as[bs|].
   - apply bind_inv in Esub as(ys&sy&Eys&Esub). injection Esub as <- <-.
     apply Forall_app;split;[|exact Ss].
     eapply(app_subst_support w R Fl b Hload);try eassumption.
     intros k y E. exact(context_clean_types_wf e k y C E).
   - injection Esub as <- _;exact Ss. }
 assert(Ws0:Forall(fun p=>WFty(snd p))subst0).
 { destruct a as[bs|].
   - apply bind_inv in Esub as(ys&sy&Eys&Esub). injection Esub as <- <-.
     apply Forall_app;split;[|exact Ws].
     eapply Forall_impl;[|exact(proj1(proj2(source_clean w R Fl b Hload 200))e mi bs outer s ys sy Eys Hi Hw Hmi Ts(cc_types _ C)L1)].
     intros q O. exact(ety_ok_wf _ O).
   - injection Esub as <- _;exact Ws. }
 cbn beta zeta in H. destruct d as[dn|],p as[pp|];try impossible H.
 - apply bind_inv in H as(r&s2&Er&H).
   destruct(rts_spec w R Fl b Hload mi pp s1 r s2 I1 W1 Hmi Er)as(->&_&Hr&_).
   exact(method_context_support e mi r dn subst0 needs s1 e' needs' s' H I1 W1 Hmi Hr Ts0 Ss0 Ws0 C T S L).
 - apply bind_inv in H as(tg&s2&Er&H).
   destruct(rpath_rel w R Fl b Hload mi pp s1 tg s2 W1 Hmi Er)as[-> Hres].
   destruct tg as[[x|j]|];try impossible H.
   apply bind_inv in H as(g&s3&Eg&H).
   destruct(sym_loader w R Fl b Hload _ _ x s1 g s3 I1 W1 Hres Eg)as[-> Hg].
   pose proof(load_world_ids b w Hload x g Hg)as Eid. rewrite <-Eid in Hg.
   cstep H I1 W1 L u s4 Eu I4 W4 L4.
   destruct(is_some a&&match gs_kind g,gs_decl g with
    |KCtx,_|KFn,_|KVal,ADVal _ _ None=>false|_,_=>true end);[impossible H|].
   exact(IH e mi g subst0 needs s4 e' needs' s' H I4 W4 Hmi Hg Ts0 Ss0 Ws0 C T S L).
Qed.
Theorem context_walk_support : forall f, SymWalkSupport f /\ SpecWalkSupport f.
Proof.
 induction f as[|f[IS IT]].
 - split;intros until 1;discriminate.
 - exact(conj(sym_walk_support_step f IT)(spec_walk_support_step f IS)).
Qed.

Lemma assume_item_context_support:forall e mi it needs s e' needs' s',
 add_assume_item e mi it needs s=Good((e',needs'),s')->Inv s->st_w s=w->In mi(w_order w)->
 ContextClean e->Tri(en_uf e)->EnvSupport WW e->Later w P R Fl s'->
 ContextClean e' /\ Tri(en_uf e') /\ EnvSupport WW e'.
Proof.
 intros e mi[p[dn|]]needs s e' needs' s' H Hi Hw Hmi C T S L;cbn[add_assume_item]in H.
 - apply bind_inv in H as(r&s1&Er&H).
   destruct(rts_spec w R Fl b Hload mi p s r s1 Hi Hw Hmi Er)as(->&_&Hr&_).
   destruct(method_context_clean w R Fl b Hload e mi r dn [] needs s e' needs' s' H Hi Hw Hmi Hr
     (ltac:(intros k y E;discriminate E)) C T L)as[Ce Te].
   split;[exact Ce|]. split;[exact Te|].
   exact(method_context_support e mi r dn [] needs s e' needs' s' H Hi Hw Hmi Hr
     (ltac:(intros k y E;discriminate E))(Forall_nil _)(Forall_nil _)C T S L).
 - apply bind_inv in H as(tg&s1&Er&H).
   destruct(rpath_rel w R Fl b Hload mi p s tg s1 Hw Hmi Er)as[-> Hres].
   destruct tg as[[x|j]|];try impossible H.
   apply bind_inv in H as(g&s2&Eg&H).
   destruct(sym_loader w R Fl b Hload _ _ x s g s2 Hi Hw Hres Eg)as[-> Hg].
   pose proof(load_world_ids b w Hload x g Hg)as Eid. rewrite <-Eid in Hg.
   cstep H Hi Hw L cf s3 Ecf I3 W3 L3.
   destruct(proj1(context_walk_clean w R Fl b Hload cf)e mi g [] needs s3 e' needs' s' H I3 W3 Hmi Hg
     (ltac:(intros k y E;discriminate E)) C T L)as[Ce Te].
   split;[exact Ce|]. split;[exact Te|].
   exact(proj1(context_walk_support cf)e mi g [] needs s3 e' needs' s' H I3 W3 Hmi Hg
     (ltac:(intros k y E;discriminate E))(Forall_nil _)(Forall_nil _)C T S L).
Qed.
#[local] Hint Resolve ok_add_assume_item : okdb.
Lemma assume_items_context_support:forall mi its e needs s e' needs' s',
 mfold(fun acc it=>add_assume_item(fst acc)mi it(snd acc))(e,needs)its s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->ContextClean e->Tri(en_uf e)->EnvSupport WW e->Later w P R Fl s'->
 ContextClean e' /\ Tri(en_uf e') /\ EnvSupport WW e'.
Proof.
 intros mi its. induction its as[|it rest IH];intros e needs s e' needs' s' H Hi Hw Hmi C T S L.
 - injection H as <- <- <-. auto.
 - cbn[mfold]in H. cstep H Hi Hw L a s1 Ea I1 W1 L1. destruct a as[ea na].
   destruct(assume_item_context_support e mi it needs s ea na s1 Ea Hi Hw Hmi C T S L1)as[Ca[Ta Sa]].
   exact(IH ea na s1 e' needs' s' H I1 W1 Hmi Ca Ta Sa L).
Qed.
End Walk.
