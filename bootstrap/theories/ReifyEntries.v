(* Runtime entries for immutable, completely mapped provision scopes. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss XTyping XEnv XRuntimeEnv XRuntimeCall XCast
 ReifyMaps ReifyChains ReifyContracts XReifyEnv.
Import ListNotations.
Set Implicit Arguments.
Section Entries.
Variables (Sg:gsig)(Rp:nsym->option rep)(P:xprog)
 (Fb:usym->option(tele*tele*subst))(Hst:sty).
Local Notation xvt := (xvt Sg Rp P Hst).
Local Notation pty_ok := (pty_ok Sg Rp P).
Fixpoint reified_entry (g : subst) (i : item) (en : xdentry) {struct en} : Prop :=
  match i, en with
  | ITy _, XDType _ => True
  | IVal v, XDVal w =>
      match g_val Sg v with
      | Some (_, tauv) => xvt w (app_subst g tauv)
      | None => False
      end
  | IFn f, XDClo f' d' =>
      match g_fn Sg f, g_fn Sg f', xp_fn P f' with
      | Some (_, Sf, None), Some (D', S', None), Some _ =>
          exists PhiD' g',
            tele_ctx D' = Some PhiD' /\
            grounding g' /\ covers g' (tyreqs (cA PhiD')) /\
            absorbs g' (cS PhiD') /\
            map (app_subst g') (fs_params S') = map (app_subst g) (fs_params Sf) /\
            app_subst g' (fs_ret S') = app_subst g (fs_ret Sf) /\
            Forall (fun i => is_ty_item i = true \/
                             exists en, lookup_xdenv i d' = Some en) (cA PhiD') /\
            Forall (fun p =>
              incl (runtime_dep_tys Sg P (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiD'))) d' /\
            runtime_types P g' (cA PhiD') d' /\
            (fix all (dd : xdenv) : Prop :=
               match dd with
               | [] => True
               | p :: r => reified_entry g' (fst p) (snd p) /\ all r
               end) d'
      | _, _, _ => False
      end
  | IMth rt m th, XDClo f' d' =>
      match g_mth Sg m, g_fn Sg f', xp_fn P f' with
      | Some (_, Sm), Some (D', S', None), Some _ =>
          exists PhiD' g',
            tele_ctx D' = Some PhiD' /\
            grounding g' /\ covers g' (tyreqs (cA PhiD')) /\
            absorbs g' (cS PhiD') /\
            map (app_subst g') (fs_params S')
              = app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm) /\
            app_subst g' (fs_ret S')
              = app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm))) /\
            Forall (fun i => is_ty_item i = true \/
                             exists en, lookup_xdenv i d' = Some en) (cA PhiD') /\
            Forall (fun p =>
              incl (runtime_dep_tys Sg P (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiD'))) d' /\
            runtime_types P g' (cA PhiD') d' /\
            (fix all (dd : xdenv) : Prop :=
               match dd with
               | [] => True
               | p :: r => reified_entry g' (fst p) (snd p) /\ all r
               end) d'
      | _, _, _ => False
      end
  | IFn f, XDPrim p =>
      match g_fn Sg f with
      | Some (_, Sf, None) =>
          prim_ok Sg Rp P p (map (app_subst g) (fs_params Sf)) (app_subst g (fs_ret Sf))
      | _ => False
      end
  | IMth rt m th, XDPrim p =>
      match g_mth Sg m with
      | Some (_, Sm) =>
          prim_ok Sg Rp P p
               (app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm))
               (app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm))))
      | None => False
      end
  | IFn f, XDPrimAt p held =>
      match g_fn Sg f with
      | Some (_, Sf, None) =>
          prim_at_ok Sg Rp P p held (map (app_subst g) (fs_params Sf)) (app_subst g (fs_ret Sf))
      | _ => False
      end
  | IMth rt m th, XDPrimAt p held =>
      match g_mth Sg m with
      | Some (_, Sm) =>
          prim_at_ok Sg Rp P p held
               (app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm))
               (app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm))))
      | None => False
      end
  | IFun u, XDFun h sms dc =>
      match g_fun Sg u, g_fun Sg h, Fb h, xp_fun P h with
      | Some (_, _, None), Some (Dh, _, None), Some (Dh', Dc, pins), Some _ =>
          Dh' = Dh /\
          exists PhiD PhiC captured,
            tele_ctx Dh = Some PhiD /\
            tele_ctx Dc = Some PhiC /\
            grounding captured /\ covers captured (tyreqs (cA PhiC)) /\
            absorbs captured (cS PhiC) /\
            reify_chain (runtime_signature Sg P) (xp_fixed_inst P) g u sms captured h /\
            body_ground_contract Sg P h PhiD PhiC pins captured /\
            Forall (fun i => exists en, lookup_xdenv i dc = Some en) (cA PhiC) /\
            Forall (fun p =>
              incl (runtime_dep_tys Sg P (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiC))) dc /\
            runtime_types P captured (cA PhiC) dc /\
            (fix all (dd : xdenv) : Prop :=
               match dd with
               | [] => True
               | p :: r => reified_entry captured (fst p) (snd p) /\ all r
               end) dc
      | _, _, _, _ => False
      end
  | _, _ => False
  end.

Lemma reified_entry_transport : forall gt gs i j en,
 full_item_map Sg (identity_functor_schema (runtime_signature Sg P) (xp_fixed_inst P)) gt gs i j ->
 reified_entry gs j en -> reified_entry gt i en.
Proof.
 intros gt gs i j en M H; inversion M; subst.
 - destruct en;cbn[reified_entry]in H|-*;tauto.
 - destruct en;cbn[reified_entry]in H|-*;try contradiction.
   rewrite H0;rewrite H1 in H;rewrite H2;exact H.
 - unfold grounded_call_signature in H2;injection H2 as Ep Er.
   destruct en;cbn[reified_entry]in H|-*;try contradiction;
   rewrite H0;rewrite H1 in H;rewrite Ep,Er;try exact H.
 - unfold method_call_signature in H2;injection H2 as Erecv Eparams Eret.
   destruct en;cbn[reified_entry]in H|-*;try contradiction;
   rewrite H0;rewrite H1 in H;rewrite Erecv,Eparams,Eret;exact H.
 - destruct en as [val|f denv|h sms dc|p|hs|ty|p held];cbn[reified_entry]in H|-*;try contradiction.
   (* Copying a functor preserves its actual closure and map sequence;
      the checked identity schema changes only the interface view. *)
   assert (Decl : exists D Dr, g_fun Sg u=Some(D,Dr,None) /\ g_fun Sg v=Some(D,Dr,None)).
   { match goal with Sh:identity_functor_schema _ _ _ _ _ _ |- _ =>
       inversion Sh; subst; eauto end. }
   destruct Decl as [D0 [Dr0 [U V]]]; rewrite U;rewrite V in H.
   destruct (g_fun Sg h) as [[[Dh Drh] [bh|]]|] eqn:BH;try contradiction.
   destruct (Fb h) as [[[Dh' Dc] pins]|] eqn:FB;try contradiction.
   destruct (xp_fun P h) as [code|] eqn:Code;try contradiction.
   destruct H as [Same [PhiD [PhiC [captured [D [C [G [Cover [Abs [Chain Rest]]]]]]]]]].
   split;[exact Same|]. exists PhiD,PhiC,captured.
   repeat (split;[assumption|]).
   split;[eapply RcInterface; eassumption|exact Rest].
 - destruct en;cbn[reified_entry]in H;try contradiction;rewrite H3 in H;contradiction.
Qed.

Lemma reified_functor_link : forall gt gs u w Du Dw Dr h sms dc sm,
 g_fun Sg u=Some(Du,Dr,None) ->g_fun Sg w=Some(Dw,Dr,None) ->
 functor_link (runtime_signature Sg P)(xp_fixed_inst P)gt u gs w sm(identity_smap Dr) ->
 reified_entry gs(IFun w)(XDFun h sms dc) ->
 reified_entry gt(IFun u)(XDFun h(sms++[sm])dc).
Proof.
 intros gt gs u w Du Dw Dr h sms dc sm U W Link Entry.
 cbn[reified_entry]in Entry|-*;rewrite U;rewrite W in Entry.
 destruct(g_fun Sg h)as[[[Dh Drh][bh|]]|];try exact Entry.
 destruct(Fb h)as[[[Dh' Dc]pins]|];try exact Entry.
 destruct(xp_fun P h)as[code|];try exact Entry.
 destruct Entry as[Same[PhiD[PhiC[captured[D[C[G[Cover[Abs[Chain Rest]]]]]]]]]].
 split;[exact Same|]. exists PhiD,PhiC,captured.
 repeat(split;[assumption|]). split;[eapply RcLink;eassumption|exact Rest].
Qed.

Lemma reified_entry_retype : forall g1 i1 g2 i2 en,
  xp_immutable P = true -> wf_gsig Sg ->
  reified_entry g1 i1 en ->
  app_subst_item g1 i1 = app_subst_item g2 i2 ->
  (forall t, In t (runtime_dep_tys Sg P i1) ->
             app_subst g1 (TAbs t) = app_subst g2 (TAbs t)) ->
  (forall rt m th rt' th' Dm Sm tau,
      i1 = IMth rt m th -> i2 = IMth rt' m th' ->
      g_mth Sg m = Some (Dm, Sm) ->
      In tau (fs_params Sm) \/ tau = fs_ret Sm ->
      interp_m g1 th rt tau = interp_m g2 th' rt' tau) ->
  reified_entry g2 i2 en.
Proof.
  intros g1 i1 g2 i2 en Immutable WF Hag Hit Hdep Hm.
  destruct i1 as [t1|v1|f1|rt1 m1 th1|u1];
    destruct i2 as [t2|v2|f2|rt2 m2 th2|u2];
    try (simpl in Hit; discriminate);
    [ destruct en; simpl in Hag |- *; try contradiction; exact I | | | | ].
  - (* val *)
    simpl in Hit. injection Hit as Hv. subst v2.
    destruct en as [w| | | | | |]; simpl in Hag |- *; try contradiction.
    destruct (g_val Sg v1) as [[Dv tauv]|] eqn:E; [|destruct Hag].
    cbn [runtime_dep_tys dep_tys] in Hdep. rewrite E in Hdep.
    destruct (wf_val_decl WF _ E) as [PhiDv [Wtv Hwf]].
    apply wf_ty_frees in Hwf. simpl in Hwf.
    rewrite <- (dentry_agree_retype_aux1 g1 g2 tauv); [exact Hag|].
    intros t Ht. apply Hdep.
    destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtv t) (Hwf t Ht)) as [Hin0 | Hin];
      [destruct Hin0 | exact Hin].
  - (* fn *)
    simpl in Hit. injection Hit as Hf. subst f2.
    cbn [runtime_dep_tys dep_tys] in Hdep.
    destruct (g_fn Sg f1) as [[[Df Sf] b]|] eqn:E;
      [| destruct en; simpl in Hag; rewrite ?E in Hag; simpl in Hag; contradiction].
    destruct (wf_fn_decl WF _ E) as [PhiDf [Wtf [Hps [Hrt _]]]].
    assert (Hcnv : forall t, In t (tyreqs (cA PhiDf)) -> In t (tyreqs Df)).
    { intros t Ht.
      destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtf t) Ht) as [Hin0 | Hin];
        [destruct Hin0 | exact Hin]. }
    assert (Hpar' : map (app_subst g1) (fs_params Sf) = map (app_subst g2) (fs_params Sf)).
    { apply map_ext_in. intros tau Htau.
      apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      rewrite Forall_forall in Hps. specialize (Hps tau Htau).
      apply wf_ty_frees in Hps. simpl in Hps. apply Hcnv. apply Hps. exact Ht. }
    assert (Hret' : app_subst g1 (fs_ret Sf) = app_subst g2 (fs_ret Sf)).
    { apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      apply wf_ty_frees in Hrt. simpl in Hrt. apply Hcnv. apply Hrt. exact Ht. }
    destruct en as [w|f' d'|hf sms dc|p|hs|ty|p held]; simpl in Hag |- *; rewrite ?E in Hag |- *;
      try contradiction.
    + destruct b; [destruct Hag|].
      destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
      destruct b'; [destruct Hag|].
      destruct (xp_fn P f') as [body|]; [|destruct Hag].
      destruct Hag as [PhiD' [g' [Htc [Hg [Hc [Habs [Hpar [Hret Hrest]]]]]]]].
      exists PhiD', g'. split; [exact Htc|]. split; [exact Hg|].
      split; [exact Hc|]. split; [exact Habs|].
      split; [rewrite Hpar; exact Hpar'|].
      split; [rewrite Hret; exact Hret' | exact Hrest].
    + destruct b; [destruct Hag|].
      rewrite <- Hpar', <- Hret'. exact Hag.
    + destruct b; [destruct Hag|]. rewrite <- Hpar', <- Hret'. exact Hag.
  - (* method *)
    simpl in Hit. injection Hit as Hrt Hmm Hth. subst m2.
    destruct (g_mth Sg m1) as [[Dm Sm]|] eqn:E;
      [| destruct en; simpl in Hag; rewrite ?E in Hag; simpl in Hag; contradiction].
    assert (Hm' : forall tau, In tau (fs_params Sm) \/ tau = fs_ret Sm ->
              interp_m g1 th1 rt1 tau = interp_m g2 th2 rt2 tau)
      by (intros tau HIn; eapply Hm; eauto).
    unfold interp_m in Hm'.
    assert (Hpar' : map (fun tau => app_subst g1 (app_subst (this_subst rt1) (app_subst th1 tau)))
                        (fs_params Sm)
                  = map (fun tau => app_subst g2 (app_subst (this_subst rt2) (app_subst th2 tau)))
                        (fs_params Sm)).
    { apply map_ext_in. intros tau Htau. apply Hm'. left; exact Htau. }
    destruct en as [w|f' d'|hf sms dc|p|hs|ty|p held]; simpl in Hag |- *; rewrite ?E in Hag |- *;
      try contradiction.
    + destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
      destruct b'; [destruct Hag|].
      destruct (xp_fn P f') as [body|]; [|destruct Hag].
      destruct Hag as [PhiD' [g' [Htc [Hg [Hc [Habs [Hpar [Hret Hrest]]]]]]]].
      exists PhiD', g'. split; [exact Htc|]. split; [exact Hg|].
      split; [exact Hc|]. split; [exact Habs|].
      split.
      * rewrite Hpar, Hrt, Hpar'. reflexivity.
      * split; [|exact Hrest]. rewrite Hret. apply Hm'. right; reflexivity.
    + rewrite <- Hrt, <- Hpar', <- (Hm' (fs_ret Sm) (or_intror eq_refl)). exact Hag.
    + rewrite <- Hrt, <- Hpar', <- (Hm' (fs_ret Sm) (or_intror eq_refl)). exact Hag.
  - (* An immutable closure depends on the interpreted fixed inputs. *)
    simpl in Hit. injection Hit as Hu. subst u2.
    destruct en as [| | h sms dc | | | |]; cbn [reified_entry] in Hag |- *; try contradiction.
    destruct (g_fun Sg u1) as [[[Du Dr] [b|]]|]; try exact Hag.
    destruct (g_fun Sg h) as [[[Dh Drh] [bh|]]|]; try exact Hag.
    destruct (Fb h) as [[[Dh' Dc] pins]|]; try exact Hag.
    destruct (xp_fun P h) as [code|]; try exact Hag.
    destruct Hag as [Same [PhiD [PhiC [captured [D [C [G [Cover [Abs [Chain Rest]]]]]]]]]].
    split;[exact Same|]. exists PhiD,PhiC,captured.
    repeat (split;[assumption|]). split;[|exact Rest].
    eapply RcRebase;[|exact Chain]. intros t Hin.
    unfold fixed_interpretation; apply dentry_agree_retype_aux1.
    intros x Hx;symmetry;apply Hdep.
    unfold runtime_dep_tys;rewrite Immutable;apply in_flat_map.
    exists t;auto.
Qed.

  Lemma reified_entry_reground : forall g1 g2 i en,
      xp_immutable P=true -> wf_gsig Sg ->
      reified_entry g1 i en ->
      (forall x, In x (runtime_dep_tys Sg P i ++ item_ty_frees i ++ method_item_frees Sg i) ->
                 app_subst g1 (TAbs x) = app_subst g2 (TAbs x)) ->
      reified_entry g2 i en.
  Proof.
    intros g1 g2 i en Immutable WF Hag Hx.
    apply (@reified_entry_retype g1 i g2 i en Immutable WF Hag).
    - destruct i as [t|v|f|rt m th|uf]; simpl; try reflexivity.
      f_equal.
      + apply dentry_agree_retype_aux1. intros x Hin. apply Hx.
        apply in_or_app. right. apply in_or_app. left. simpl.
        apply in_or_app. left. exact Hin.
      + unfold map_snd. apply map_ext_in. intros [z a] Hin. simpl. f_equal.
        apply dentry_agree_retype_aux1. intros x Hx'. apply Hx.
        apply in_or_app. right. apply in_or_app. left. simpl.
        apply in_or_app. right. apply in_flat_map. exists (z, a).
        split; assumption.
    - intros x Hin. apply Hx. apply in_or_app. left. exact Hin.
    - intros rt m th rt' th' Dm Sm tau E1 E2 Hm Htau.
      rewrite E1 in E2. injection E2 as <- <-. subst i.
      unfold interp_m. apply dentry_agree_retype_aux1. intros x Hin. apply Hx.
      apply in_or_app. right. apply in_or_app. right. simpl. rewrite Hm.
      apply in_flat_map.
      exists (app_subst (this_subst rt) (app_subst th tau)).
      split; [| exact Hin].
      apply in_map_iff. exists tau. split; [reflexivity |].
      apply in_or_app.
      destruct Htau as [Htau | ->]; [left; exact Htau | right; left; reflexivity].
  Qed.

(* Every in-scope item, including each physical type slot, is present and
   agrees at the current grounding. Shadowed entries need not be read. *)
Definition reified_environment (g:subst)(A:list item)(d:xdenv):Prop :=
 (forall i, In i A -> exists en, lookup_xdenv i d=Some en) /\
 (forall i en, In i A -> lookup_xdenv i d=Some en -> reified_entry g i en) /\
 types_match g A d.

Theorem reified_map_composes : forall gt gs target source sm d,
 reified_environment gs source d ->
 full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))gt gs target source sm ->
 exists dr,xcompose d sm=Some dr /\ map fst dr=target /\
 reified_environment gt target dr /\
 Forall(fun p=>reified_entry gt(fst p)(snd p))dr.
Proof.
 intros gt gs target source sm d [Available [Agree Types]] [Dom Maps].
 assert(Avail:Forall(fun p=>exists en,lookup_xdenv(snd p)d=Some en)sm).
 { apply Forall_forall;intros p InP;apply Available.
   exact(proj1(proj1(Forall_forall _ _)Maps p InP)). }
 destruct(@xcompose_defined d sm Avail)as[dr Compose].
 assert(Keys:map fst dr=target)by(rewrite(@xcompose_keys d sm dr Compose);exact Dom).
 assert(Entries:Forall(fun p=>reified_entry gt(fst p)(snd p))dr).
 { apply Forall_forall;intros[i en]InP.
   destruct(@xcompose_entry d sm dr(i,en)Compose InP)as[j[InM Lookup]].
   cbn[fst snd]in InM,Lookup|-*.
   destruct(proj1(Forall_forall _ _)Maps(i,j)InM)as[InJ Rel].
   cbn[fst snd]in InJ,Rel.
   eapply reified_entry_transport;[exact Rel|eapply Agree;eassumption]. }
 assert(TargetTypes:types_match gt target dr).
 { intros t InT.
   destruct(lookup_xdenv_keys dr(ITy t)ltac:(rewrite Keys;exact InT))as[en Lookup].
   destruct(@xcompose_entry d sm dr(ITy t,en)Compose(@lookup_xdenv_in (ITy t) dr en Lookup))as[j[InM LookJ]].
   cbn[fst snd]in InM,LookJ.
   destruct(proj1(Forall_forall _ _)Maps(ITy t,j)InM)as[InJ Rel].
   cbn[fst snd]in InJ,Rel.
   inversion Rel;subst.
   destruct(Types s InJ)as[tau[LookupS Eq]].
   rewrite LookJ in LookupS;injection LookupS as EqEn;subst en.
   exists tau;split;[exact Lookup|rewrite Eq;congruence]. }
 exists dr;split;[exact Compose|]. split;[exact Keys|]. split;[|exact Entries].
 split.
 - intros i Hi;apply lookup_xdenv_keys;rewrite Keys;exact Hi.
 - split;[|exact TargetTypes]. intros i en Hi Lookup.
   exact(proj1(Forall_forall _ _)Entries(i,en)(lookup_xdenv_in i dr Lookup)).
Qed.

Lemma reified_all_Forall : forall g dd,
 (fix all(dd:xdenv):Prop:=match dd with []=>True|p::r=>reified_entry g(fst p)(snd p)/\all r end)dd <->
 Forall(fun p=>reified_entry g(fst p)(snd p))dd.
Proof.
 intros g dd;induction dd as[|p r IH];split;intro H.
 - constructor.
 - exact I.
 - destruct H as[H1 H2];constructor;[exact H1|apply IH;exact H2].
 - inversion H;subst;split;[assumption|apply IH;assumption].
Qed.

Lemma reified_capture_environment : forall g A d,
 Forall(fun i=>exists en,lookup_xdenv i d=Some en)A ->
 Forall(fun p=>reified_entry g(fst p)(snd p))d ->
 Forall(fun p=>incl(runtime_dep_tys Sg P(fst p)++item_ty_frees(fst p)++method_item_frees Sg(fst p))(tyreqs A))d ->
 types_match g A d ->reified_environment g A d /\reified_scope_closed Sg P A.
Proof.
 intros g A d Available Entries Closed Types;split.
 - split;[intros i Hin;exact(proj1(Forall_forall _ _)Available i Hin)|].
   split;[|exact Types]. intros i en Hin Lookup.
   exact(proj1(Forall_forall _ _)Entries(i,en)(lookup_xdenv_in i d Lookup)).
 - apply Forall_forall;intros i Hin.
   destruct(proj1(Forall_forall _ _)Available i Hin)as[en Lookup].
   exact(proj1(Forall_forall _ _)Closed(i,en)(lookup_xdenv_in i d Lookup)).
Qed.

Lemma reified_environment_restrict : forall g A B d,
 incl A B ->reified_environment g B d ->reified_environment g A d.
Proof.
 intros g A B d Sub [Available [Agree Types]].
 split;[intros i Hin;apply Available,Sub;exact Hin|].
 split;[intros i en Hin Lookup;eapply Agree;[apply Sub;exact Hin|exact Lookup]|].
 intros t Hin;apply Types,Sub;exact Hin.
Qed.

Lemma merge_tele_item_incl : forall D Phi PhiD,
 merge_tele Phi D=Some PhiD ->incl(cA PhiD)(cA Phi++D).
Proof.
 induction D as[|head tail IH];intros Phi PhiD Run j Hin;cbn[merge_tele]in Run.
 - injection Run as <-;apply in_or_app;left;exact Hin.
 - destruct(merge_item Phi head)as[Mid|]eqn:M;[|discriminate].
   pose proof(IH Mid PhiD Run j Hin)as H.
   destruct(merge_item_cA _ _ M)as[E|E];rewrite E in H;
   repeat rewrite in_app_iff in *;cbn in *;tauto.
Qed.

Lemma reified_environment_reground : forall ga gb A d,
 xp_immutable P=true ->wf_gsig Sg ->
 reified_scope_closed Sg P A ->meanings_on A ga gb ->
 reified_environment ga A d ->reified_environment gb A d.
Proof.
 intros ga gb A d Immutable WF Closed Eq [Available [Agree Types]].
 split;[exact Available|]. split.
 - intros i en Hin Lookup;eapply reified_entry_reground;[exact Immutable|exact WF|eapply Agree;eassumption|].
   intros t Ht;apply Eq;exact(proj1(Forall_forall _ _)Closed i Hin t Ht).
 - intros t Hin;destruct(Types t Hin)as[tau[Lookup Et]].
   exists tau;split;[exact Lookup|rewrite Et;apply Eq,wf_ty_frees_aux3;exact Hin].
Qed.

Lemma reified_environment_identity_transport : forall ga gb D d,
 full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))gb ga D D(identity_smap D) ->
 reified_environment ga D d ->reified_environment gb D d.
Proof.
 intros ga gb D d [Dom Maps] [Available [Agree Types]].
 assert(Rel:forall i,In i D ->
 full_item_map Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))gb ga i i).
 { intros i Hin.
   exact(proj2(proj1(Forall_forall _ _)Maps(i,i)(in_map(fun i=>(i,i)) D i Hin))). }
 split;[exact Available|]. split.
 - intros i en Hin Lookup;eapply reified_entry_transport;[apply Rel;exact Hin|eapply Agree;eassumption].
 - intros t Hin;destruct(Types t Hin)as[tau[Lookup Eq]].
   exists tau;split;[exact Lookup|].
   pose proof(Rel(ITy t)Hin)as R;inversion R;subst;congruence.
Qed.

Lemma xrun_chain_snoc : forall sms d sm,
 xrun_chain d(sms++[sm])=
 match xcompose d sm with Some dr=>xrun_chain dr sms|None=>None end.
Proof.
 induction sms as[|head tail IH];intros d sm;cbn[xrun_chain app];[destruct(xcompose d sm);reflexivity|].
 rewrite IH;destruct(xcompose d sm)as[dr|];[|reflexivity].
 destruct(xrun_chain dr tail);reflexivity.
Qed.

Theorem reified_chain_runs : forall ga Da sms gb Db,
 chain_inputs (runtime_signature Sg P)(xp_fixed_inst P)ga Da sms gb Db ->forall d,
 reified_environment ga Da d ->exists dr,
 xrun_chain d sms=Some dr /\reified_environment gb Db dr.
Proof.
 intros ga Da sms gb Db Chain;induction Chain;intros d Env.
 - exists d;split;[reflexivity|exact Env].
 - apply IHChain;eapply reified_environment_identity_transport;[|exact Env].
   apply (proj1 (full_map_ok_runtime_signature Sg P _ _ _ _ _ _)); exact H.
 - pose proof (proj1 (full_map_ok_runtime_signature Sg P _ _ _ _ _ _) H) as CoreMap.
   destruct(reified_map_composes Env CoreMap)as[dm[Compose[Keys[Mid Entries]]]].
   destruct(IHChain dm Mid)as[dr[Run Final]].
   exists dr;split;[rewrite xrun_chain_snoc,Compose;exact Run|exact Final].
Qed.

Lemma reified_result_transport : forall ga gb D,
 result_meanings (runtime_signature Sg P)(xp_fixed_inst P)ga gb D ->forall d,
 reified_environment gb D d ->reified_environment ga D d.
Proof.
 intros ga gb D Trail;induction Trail;intros d Env;[exact Env|].
 eapply reified_environment_identity_transport;[|apply IHTrail;exact Env].
 apply (proj1 (full_map_ok_runtime_signature Sg P _ _ _ _ _ _)); exact H.
Qed.

Lemma reified_chain_keys : forall ga Da sms gb Db,
 chain_inputs (runtime_signature Sg P)(xp_fixed_inst P)ga Da sms gb Db ->forall d dr,
 map fst d=Da ->xrun_chain d sms=Some dr ->map fst dr=Db.
Proof.
 intros ga Da sms gb Db Chain;induction Chain;intros d dr Keys Run.
 - cbn[xrun_chain]in Run;injection Run as <-;exact Keys.
 - eapply IHChain;eassumption.
 - rewrite xrun_chain_snoc in Run.
   destruct(xcompose d sm)as[dm|]eqn:Compose;[|discriminate].
   eapply IHChain;[|exact Run].
   rewrite(@xcompose_keys d sm dm Compose);exact(proj1 H).
Qed.

Lemma reified_environment_app : forall g A B da db,
 map fst da=A ->reified_environment g A da ->reified_environment g B db ->
 reified_environment g(A++B)(da++db).
Proof.
 intros g A B da db Keys [AvailA [AgreeA TypesA]] [AvailB [AgreeB TypesB]].
 assert(KeyIn:forall i en,lookup_xdenv i da=Some en ->In i A).
 { intros i en Lookup;rewrite <-Keys.
   exact(in_map fst da(i,en)(lookup_xdenv_in i da Lookup)). }
 split.
 - intros i Hin;rewrite xenv_lookup_app.
   destruct(lookup_xdenv i da)as[en|]eqn:L;[eauto|].
   apply in_app_iff in Hin;destruct Hin as[Hin|Hin].
   + destruct(AvailA i Hin)as[en E];rewrite E in L;discriminate.
   + apply AvailB;exact Hin.
 - split.
   + intros i en Hin Lookup;rewrite xenv_lookup_app in Lookup.
     destruct(lookup_xdenv i da)as[ea|]eqn:L.
     * injection Lookup as <-;eapply AgreeA;[eapply KeyIn;exact L|exact L].
     * eapply AgreeB;[|exact Lookup].
       apply in_app_iff in Hin;destruct Hin as[Hin|Hin];[|exact Hin].
       destruct(AvailA i Hin)as[ea E];rewrite E in L;discriminate.
   + intros t Hin;rewrite xenv_lookup_app.
     destruct(lookup_xdenv(ITy t)da)as[en|]eqn:L.
     * destruct(TypesA t(KeyIn _ _ L))as[tau[E Eq]].
       rewrite L in E;exists tau;split;[exact E|exact Eq].
     * apply in_app_iff in Hin;destruct Hin as[Hin|Hin].
       -- destruct(AvailA(ITy t)Hin)as[ea E];rewrite E in L;discriminate.
       -- apply TypesB;exact Hin.
Qed.

Theorem reified_application_body_ready : forall outer u h sms dc Du Dr gin din,
 xp_immutable P=true ->wf_gsig Sg ->
 g_fun Sg u=Some(Du,Dr,None) ->
 reified_entry outer(IFun u)(XDFun h sms dc) ->
 schema_input_ground (runtime_signature Sg P)(xp_fixed_inst P)u outer Du gin ->
 reified_environment gin Du din ->map fst din=Du ->
 (forall D R,g_fun Sg h=Some(D,R,None)->reified_scope_closed Sg P D) ->
 exists Dh Dc pins code PhiD PhiC gb dh,
 Fb h=Some(Dh,Dc,pins) /\xp_fun P h=Some code /\
 tele_ctx Dh=Some PhiD /\tele_ctx Dc=Some PhiC /\
 ctx_grounded gb(fun_ctx PhiD PhiC pins) /\
 xrun_chain din sms=Some dh /\
 reified_environment gb(cA(fun_ctx PhiD PhiC pins))(dh++dc) /\
 g_fun Sg h=Some(Dh,Dr,None) /\
 (forall gout,schema_result_ground Dh Dr gb gout ->exists final,
   schema_result_ground Du Dr gin final /\
   result_meanings (runtime_signature Sg P)(xp_fixed_inst P)final gout Dr).
Proof.
 intros outer u h sms dc Du Dr gin din Immutable WF Decl Entry Input Env Keys ClosedD.
 cbn[reified_entry]in Entry;rewrite Decl in Entry.
 destruct(g_fun Sg h)as[[[Dh Drh][bh|]]|]eqn:Hdecl;try contradiction.
 destruct(Fb h)as[[[Dh' Dc]pins]|]eqn:FB;try contradiction.
 destruct(xp_fun P h)as[code|]eqn:Code;try contradiction.
 destruct Entry as[Same[PhiD[PhiC[captured[D[C[G[Cover[Abs[Chain[Body[Available[Closed[Types Entries]]]]]]]]]]]]]].
 subst Dh'.
 destruct(reify_chain_results Chain Decl)as[Dh0 Hsame].
 change (g_fun Sg h=Some(Dh0,Dr,None)) in Hsame.
 rewrite Hdecl in Hsame;injection Hsame as <- <-.
 destruct(reify_chain_instantiation Chain Decl Hdecl Input)
 as[gh[InH[Plan Results]]].
 destruct(reified_chain_runs Plan Env)as[dh[Run DhEnv]].
 pose proof(@reified_chain_keys gin Du sms gh Dh Plan din dh Keys Run)as DhKeys.
 destruct(Body Dh Drh gh Hdecl InH)as[gb[Ground[OnD OnC]]].
 assert(Reify:xreify P=true)by(unfold xreify;rewrite Immutable;reflexivity).
 destruct(reified_capture_environment Available (proj1(reified_all_Forall captured dc)Entries) Closed (Types Reify))as[CapEnv CapClosed].
 assert(MeanD:meanings_on Dh gh gb).
 { intros t Hin;symmetry;apply OnD;apply(proj2(tele_ctx_tyreqs Dh D t));exact Hin. }
 assert(MeanC:meanings_on(cA PhiC)captured gb)by(apply meanings_on_sym;exact OnC).
 pose proof(reified_environment_reground Immutable WF (ClosedD Dh Drh eq_refl) MeanD DhEnv)as DhBody.
 pose proof(reified_environment_reground Immutable WF CapClosed MeanC CapEnv)as CapBody.
 pose proof(reified_environment_app DhKeys DhBody CapBody)as Both.
 exists Dh,Dc,pins,code,PhiD,PhiC,gb,dh.
 split;[reflexivity|]. split;[reflexivity|]. split;[exact D|]. split;[exact C|].
 split;[exact Ground|]. split;[exact Run|].
 split.
 2:{ split;[reflexivity|]. intros gout Out;apply Results.
     eapply schema_result_input_ext;[apply meanings_on_sym;exact MeanD|exact Out]. }
 eapply reified_environment_restrict;[|exact Both].
 intros i Hin;cbn[fun_ctx cA]in Hin.
 apply in_app_iff in Hin;apply in_app_iff;destruct Hin as[Hin|Hin];[left|right;exact Hin].
 exact(@merge_tele_item_incl Dh empty_ctx PhiD D i Hin).
Qed.
Lemma reified_environment_cons : forall g A d i en,
 reified_environment g A d ->reified_entry g i en ->
 types_match g(i::A)((i,en)::d) ->
 reified_environment g(i::A)((i,en)::d).
Proof.
 intros g A d i en [Available[Agree Types]]Entry NewTypes.
 split.
 - intros j Hin;cbn[lookup_xdenv].
   destruct(item_eqb j i)eqn:E;[eauto|].
   destruct Hin as[<-|Hin].
   + rewrite(proj2(item_eqb_eq i i)eq_refl)in E;discriminate.
   + apply Available;exact Hin.
 - split;[|exact NewTypes]. intros j value Hin Lookup.
   cbn[lookup_xdenv]in Lookup;destruct(item_eqb j i)eqn:E.
   + apply item_eqb_eq in E;subst j;injection Lookup as <-;exact Entry.
   + destruct Hin as[<-|Hin].
     * rewrite(proj2(item_eqb_eq i i)eq_refl)in E;discriminate.
     * eapply Agree;eassumption.
Qed.

Lemma reified_environment_cons_non_type : forall g A d i en,
 is_ty_item i=false ->reified_entry g i en ->
 reified_environment g A d ->reified_environment g(i::A)((i,en)::d).
Proof.
 intros g A d i en Nt Entry Env;apply reified_environment_cons;[exact Env|exact Entry|].
 intros t Hin;destruct Hin as[Eq|Hin];[subst i;discriminate Nt|].
 destruct(proj2(proj2 Env)t Hin)as[tau[Lookup Eq]].
 exists tau;split;[|exact Eq]. destruct i;cbn in *;try discriminate;exact Lookup.
Qed.

Lemma reified_ground_add_non_type : forall g Phi i,
 is_ty_item i=false ->ctx_grounded g Phi ->ctx_grounded g(add_item i Phi).
Proof.
 intros g Phi i Nt Ground;destruct i;cbn in Nt;try discriminate;
 destruct Ground as[G[Abs[Cover[Regular Reserved]]]];
 repeat(split;[assumption|]);exact Reserved.
Qed.

Lemma reified_bindty_ground : forall Phi t tau' g,
 t<>this_sym ->~In(ITy t)(cA Phi) ->wf_ty Sg Phi tau' ->ctx_grounded g Phi ->
 ctx_grounded((t,app_subst g(app_subst(cS Phi)tau'))::g)(bind_ty t tau' Phi) /\
 (forall u,In u(tyreqs(cA Phi))->
 lookup_subst u((t,app_subst g(app_subst(cS Phi)tau'))::g)=lookup_subst u g).
Proof.
    intros Phi t tau' g Hneq Hfresh Htau' Hg.
    destruct Hg as [Hground [Habs [Hcover Hsig]]].
    destruct Hsig as [Hsig Hreserved].
    destruct Hsig as [Hdom Hran].
    set (tau2 := app_subst g (app_subst (cS Phi) tau')).
    set (g2 := (t, tau2) :: g).
    assert (Hnot : ~ In t (tyreqs (cA Phi))).
    { intro H. apply Hfresh. apply safety_case_bindty_aux1. exact H. }
    assert (Hlook : forall u, In u (tyreqs (cA Phi)) ->
             lookup_subst u g2 = lookup_subst u g).
    { intros u Hu. unfold g2.
      apply safety_case_bindty_aux2 with (A := cA Phi); assumption. }
    assert (Hfree' : incl (ty_frees tau') (tyreqs (cA Phi)))
      by (apply wf_ty_frees in Htau'; exact Htau').
    assert (Hclosed2 : closed_ty tau2).
    { unfold tau2.
      rewrite (absorbs_all Habs).
      apply closed_app_subst_ground; [exact Hground|].
      intros u Hu. destruct (Hcover u (Hfree' u Hu)) as [a Ha].
      apply safety_case_bindty_aux4 in Ha. exact Ha. }
    assert (Hg2ground : grounding g2).
    { unfold g2. constructor; [exact Hclosed2|exact Hground]. }
    assert (Hg2abs : absorbs g2
             ((t, app_subst (cS Phi) tau') :: cS Phi)).
    { intros u a Hl. simpl in Hl.
      destruct (Nat.eqb u t) eqn:E.
      - injection Hl as Hl. subst a. apply Nat.eqb_eq in E. subst u.
        simpl. rewrite Nat.eqb_refl.
        unfold tau2. f_equal. symmetry.
        apply app_subst_frees_agree. intros z Hz. apply Hlook.
        eapply safety_case_bindty_aux3; eauto.
      - assert (Hu : In u (map fst (cS Phi))).
        { apply safety_case_bindty_aux4 in Hl. exact Hl. }
        assert (Hur : In u (tyreqs (cA Phi))) by (apply Hdom; exact Hu).
        unfold g2. simpl. rewrite E.
        rewrite (app_subst_frees_agree ((t, tau2) :: g) g a).
        + exact (Habs u a Hl).
        + intros z Hz. change (lookup_subst z g2 = lookup_subst z g).
          apply Hlook. rewrite Forall_forall in Hran.
          apply (Hran (u, a)); [|exact Hz].
          apply safety_case_bindty_aux5. exact Hl. }
    assert (Hg2cover : covers g2 (tyreqs (cA (bind_ty t tau' Phi)))).
    { intros u Hu. simpl in Hu. destruct Hu as [E | Hu].
      - subst u. exists tau2. simpl. rewrite Nat.eqb_refl. reflexivity.
      - destruct (Hcover u Hu) as [a Ha]. exists a.
        rewrite Hlook; assumption. }
    assert (Hg2sig : sigma_regular (cS (bind_ty t tau' Phi))
                                      (cA (bind_ty t tau' Phi))).
    { split.
      - simpl. intros u [E | Hu].
        + subst. left. reflexivity.
        + right. apply Hdom. exact Hu.
      - simpl. constructor.
        + intros u Hu. right.
          eapply safety_case_bindty_aux3; eauto.
        + eapply Forall_impl; [|exact Hran].
          intros [u a] Ha z Hz. right. exact (Ha z Hz). }
    assert (Hg2 : ctx_grounded g2 (bind_ty t tau' Phi)).
    { split; [exact Hg2ground|]. split; [exact Hg2abs|].
      split; [exact Hg2cover|]. split; [exact Hg2sig|].
      simpl. intros [Heq|Hin].
      - apply Hneq. exact Heq.
      - exact (Hreserved Hin). }
    split;[exact Hg2|exact Hlook].
Qed.

Theorem reified_bind_type : forall Phi t tau d g,
 xp_immutable P=true ->wf_gsig Sg ->reified_scope_closed Sg P(cA Phi) ->
 t<>this_sym ->~In(ITy t)(cA Phi) ->wf_ty Sg Phi tau ->
 ctx_grounded g Phi ->reified_environment g(cA Phi)d ->
 ctx_grounded((t,app_subst g(app_subst(cS Phi)tau))::g)(bind_ty t tau Phi) /\
 reified_environment((t,app_subst g(app_subst(cS Phi)tau))::g)
 (cA(bind_ty t tau Phi))(xbind_ty P d t tau).
Proof.
 intros Phi t tau d g Immutable WF Closed NotThis Fresh Wtau Ground Env.
 destruct(reified_bindty_ground NotThis Fresh Wtau Ground)as[GroundNew Lookup].
 split;[exact GroundNew|].
 assert(Eq:meanings_on(cA Phi)g((t,app_subst g(app_subst(cS Phi)tau))::g)).
 { intros u Hin;change(app_subst g(TAbs u)=app_subst((t,app_subst g(app_subst(cS Phi)tau))::g)(TAbs u)).
   cbn[app_subst];rewrite(Lookup u Hin);reflexivity. }
 pose proof(reified_environment_reground Immutable WF Closed Eq Env)as EnvNew.
 assert(Reify:xreify P=true)by(unfold xreify;rewrite Immutable;reflexivity).
 pose proof(@runtime_types_bind_ty Sg P g Phi d t tau (fun _=>proj2(proj2 Env)) Ground Wtau Reify)as TypesNew.
 unfold xbind_ty in TypesNew|-*;rewrite Reify in TypesNew|-*;cbn[cA bind_ty].
 eapply reified_environment_cons;[exact EnvNew|exact I|exact TypesNew].
Qed.

Lemma reified_environment_ext : forall g A d1 d2,
 reified_environment g A d1 ->
 (forall i,In i A->lookup_xdenv i d1=lookup_xdenv i d2) ->
 reified_environment g A d2.
Proof.
 intros g A d1 d2 [Available[Agree Types]]Eq.
 split;[intros i Hin;rewrite <-(Eq i Hin);apply Available;exact Hin|].
 split;[intros i en Hin Lookup;eapply Agree;[exact Hin|rewrite(Eq i Hin);exact Lookup]|].
 intros t Hin;destruct(Types t Hin)as[tau[Lookup Meaning]].
 exists tau;split;[rewrite <-(Eq(ITy t)Hin);exact Lookup|exact Meaning].
Qed.

Lemma reified_environment_close : forall g A d dc,
 reified_environment g A dc ->reified_environment g A(xclose d dc).
Proof.
 intros g A d dc Env;eapply reified_environment_ext;[exact Env|].
 intros i Hin;unfold xclose;rewrite xenv_lookup_app.
 destruct(proj1 Env i Hin)as[en Lookup];rewrite Lookup;reflexivity.
Qed.

Lemma reified_type_capture_entries : forall g d,
 Forall(fun p=>reified_entry g(fst p)(snd p))(xtype_env d).
Proof.
 intros g d;induction d as[|[i en]d IH];cbn;[constructor|].
 destruct i,en;cbn;auto;constructor;auto;exact I.
Qed.
Lemma reified_type_capture_closed : forall A d,
 Forall(fun p=>incl(runtime_dep_tys Sg P(fst p)++item_ty_frees(fst p)++method_item_frees Sg(fst p))(tyreqs A))(xtype_env d).
Proof.
 intros A d;induction d as[|[i en]d IH];cbn;[constructor|].
 destruct i,en;cbn;auto;constructor;auto;unfold incl;intros x Hx;contradiction.
Qed.

Theorem reified_function_capture : forall Phi g d f code Df Sf Dc Sc PhiC sm body,
 g_fn Sg f=Some(Df,Sf,None) ->g_fn Sg code=Some(Dc,Sc,None) ->
 xp_fn P code=Some body ->tele_ctx Dc=Some PhiC ->
 ctx_grounded g Phi ->reified_environment g(cA Phi)d ->
 reified_scope_closed Sg P(cA PhiC) ->
 callable_map_contract Sg P Phi PhiC sm Sf Sc ->
 exists dc,xcompose d sm=Some dc /\reified_entry g(IFn f)(XDClo code(xclose d dc)).
Proof.
 intros Phi g d f code Df Sf Dc Sc PhiC sm body Slot Code Body Tele Ground Env Closed Contract.
 destruct(Contract g Ground)as[gc[GroundC[Map Signature]]].
 destruct(reified_map_composes Env Map)as[dc[Compose[Keys[EnvC Entries]]]].
 pose proof(reified_environment_close d EnvC)as EnvClosed.
 exists dc;split;[exact Compose|].
 cbn[reified_entry];rewrite Slot,Code,Body.
 destruct GroundC as[G[Abs[Cover Rest]]].
 unfold grounded_call_signature in Signature;injection Signature as Params Result.
 exists PhiC,gc;split;[exact Tele|]. split;[exact G|]. split;[exact Cover|].
 split;[exact Abs|]. split;[exact Params|]. split;[exact Result|].
 split.
 - apply Forall_forall;intros i Hin;right;apply(proj1 EnvClosed);exact Hin.
 - split.
   + unfold xclose;apply Forall_app;split;[|apply reified_type_capture_closed].
     apply Forall_forall;intros[i en]Hin.
     apply(proj1(Forall_forall _ _)Closed i).
     rewrite <-Keys;exact(in_map fst dc(i,en)Hin).
   + split;[intros _;exact(proj2(proj2 EnvClosed))|].
     apply reified_all_Forall;unfold xclose;apply Forall_app;split;[exact Entries|apply reified_type_capture_entries].
Qed.

Definition reified_method_signature (rt:ty)(th:subst)(S:fnsig):fnsig :=
 mkSig (rt::map(fun tau=>app_subst(this_subst rt)(app_subst th tau))(fs_params S))
 (app_subst(this_subst rt)(app_subst th(fs_ret S))).

Theorem reified_method_capture : forall Phi g d rt m th code Dm Sm Dc Sc PhiC sm body,
 g_mth Sg m=Some(Dm,Sm) ->g_fn Sg code=Some(Dc,Sc,None) ->
 xp_fn P code=Some body ->tele_ctx Dc=Some PhiC ->
 ctx_grounded g Phi ->reified_environment g(cA Phi)d ->
 reified_scope_closed Sg P(cA PhiC) ->
 callable_map_contract Sg P Phi PhiC sm(reified_method_signature rt th Sm)Sc ->
 exists dc,xcompose d sm=Some dc /\reified_entry g(IMth rt m th)(XDClo code(xclose d dc)).
Proof.
 intros Phi g d rt m th code Dm Sm Dc Sc PhiC sm body Slot Code Body Tele Ground Env Closed Contract.
 destruct(Contract g Ground)as[gc[GroundC[Map Signature]]].
 destruct(reified_map_composes Env Map)as[dc[Compose[Keys[EnvC Entries]]]].
 pose proof(reified_environment_close d EnvC)as EnvClosed.
 exists dc;split;[exact Compose|].
 cbn[reified_entry];rewrite Slot,Code,Body.
 destruct GroundC as[G[Abs[Cover Rest]]].
 unfold grounded_call_signature,reified_method_signature in Signature;cbn[fs_params fs_ret map]in Signature.
 injection Signature as Params Result.
 exists PhiC,gc;split;[exact Tele|]. split;[exact G|]. split;[exact Cover|].
 split;[exact Abs|]. split.
 - rewrite Params,map_map;reflexivity.
 - split;[exact Result|]. split.
   + apply Forall_forall;intros i Hin;right;apply(proj1 EnvClosed);exact Hin.
   + split.
     * unfold xclose;apply Forall_app;split;[|apply reified_type_capture_closed].
       apply Forall_forall;intros[i en]Hin.
       apply(proj1(Forall_forall _ _)Closed i).
       rewrite <-Keys;exact(in_map fst dc(i,en)Hin).
     * split;[intros _;exact(proj2(proj2 EnvClosed))|].
       apply reified_all_Forall;unfold xclose;apply Forall_app;split;[exact Entries|apply reified_type_capture_entries].
Qed.

Theorem reified_functor_capture : forall Phi g d u h Du Dh Dr Dc pins PhiD PhiC sm smc code,
 g_fun Sg u=Some(Du,Dr,None) ->g_fun Sg h=Some(Dh,Dr,None) ->
 Fb h=Some(Dh,Dc,pins) ->xp_fun P h=Some code ->
 tele_ctx Dh=Some PhiD ->tele_ctx Dc=Some PhiC ->
 ctx_grounded g Phi ->reified_environment g(cA Phi)d ->
 reified_scope_closed Sg P(cA PhiC) ->
 functor_capture_contract Sg P Phi PhiD PhiC u h Dr pins sm smc ->
 exists dc,xcompose d smc=Some dc /\
 reified_entry g(IFun u)(XDFun h[sm](xclose d dc)).
Proof.
 intros Phi g d u h Du Dh Dr Dc pins PhiD PhiC sm smc code U Hdecl FB Code D C Ground Env Closed Contract.
 destruct(Contract g Ground)as[captured[GroundC[Map[Body Link]]]].
 destruct(reified_map_composes Env Map)as[dc[Compose[Keys[EnvC Entries]]]].
 pose proof(reified_environment_close d EnvC)as EnvClosed.
 exists dc;split;[exact Compose|].
 cbn[reified_entry];rewrite U,Hdecl,FB,Code.
 split;[reflexivity|]. exists PhiD,PhiC,captured.
 destruct GroundC as[G[Abs[Cover Rest]]].
 split;[exact D|]. split;[exact C|]. split;[exact G|]. split;[exact Cover|]. split;[exact Abs|].
 split.
 - change(reify_chain (runtime_signature Sg P)(xp_fixed_inst P)g u([]++[sm])captured h).
   eapply RcLink;[exact U|exact Hdecl|exact Link|apply RcId].
 - split;[exact Body|]. split.
   + apply Forall_forall;intros i Hin;apply(proj1 EnvClosed);exact Hin.
   + split.
     * unfold xclose;apply Forall_app;split;[|apply reified_type_capture_closed].
       apply Forall_forall;intros[i en]Hin.
       apply(proj1(Forall_forall _ _)Closed i).
       rewrite <-Keys;exact(in_map fst dc(i,en)Hin).
     * split;[intros _;exact(proj2(proj2 EnvClosed))|].
       apply reified_all_Forall;unfold xclose;apply Forall_app;split;[exact Entries|apply reified_type_capture_entries].
Qed.

Theorem reified_application_results : forall Phi base caller u Du Dr results outputs gin gout returned,
 xp_immutable P=true ->wf_gsig Sg ->reified_scope_closed Sg P(cA Phi) ->
 ctx_grounded base Phi ->reified_environment base(cA Phi)caller ->
 result_map_contract Sg P Phi u Du Dr results outputs ->
 schema_input_ground (runtime_signature Sg P)(xp_fixed_inst P)u base Du gin ->
 schema_result_ground Du Dr gin gout ->reified_environment gout Dr returned ->
 exists gcaller dr,ctx_grounded gcaller(open_res results Phi) /\
 xcompose returned outputs=Some dr /\map fst dr=results /\
 reified_environment gcaller(cA(open_res results Phi))(dr++caller) /\
 meanings_on(cA Phi)gcaller base.
Proof.
 intros Phi base caller u Du Dr results outputs gin gout returned Immutable WF Closed Ground Env Contract Input Result Returned.
 destruct(Contract base gin gout Ground Input Result)as[gcaller[GroundNew[OldMean Map]]].
 destruct(reified_map_composes Returned Map)as[dr[Compose[Keys[ResultEnv Entries]]]].
 pose proof(reified_environment_reground Immutable WF Closed(meanings_on_sym OldMean)Env)as OldEnv.
 pose proof(reified_environment_app Keys ResultEnv OldEnv)as Combined.
 exists gcaller,dr;split;[exact GroundNew|]. split;[exact Compose|]. split;[exact Keys|].
 split;[|exact OldMean].
 eapply reified_environment_restrict;[|exact Combined].
 intros i Hin;cbn[open_res cA]in Hin;apply in_app_iff in Hin;apply in_app_iff;tauto.
Qed.

Theorem reified_direct_call_environment : forall Phi PhiC g d sm slot code,
 xp_immutable P=true ->ctx_grounded g Phi ->reified_environment g(cA Phi)d ->
 callable_map_contract Sg P Phi PhiC sm slot code ->
 exists gc dc,xcompose d sm=Some dc /\ctx_grounded gc PhiC /\
 grounded_call_signature gc code=grounded_call_signature g slot /\
 reified_environment gc(cA PhiC)(xcall_env P d [] dc).
Proof.
 intros Phi PhiC g d sm slot code Immutable Ground Env Contract.
 destruct(Contract g Ground)as[gc[GroundC[Map Signature]]].
 destruct(reified_map_composes Env Map)as[dc[Compose[Keys[EnvC Entries]]]].
 exists gc,dc;split;[exact Compose|]. split;[exact GroundC|]. split;[exact Signature|].
 unfold xcall_env,xinst,xreify;rewrite Immutable;cbn.
 apply reified_environment_close;exact EnvC.
Qed.
End Entries.

Lemma reified_entry_mono : forall Sg Rp P Fb H H', sext H H' ->
  forall en g i, reified_entry Sg Rp P Fb H g i en -> reified_entry Sg Rp P Fb H' g i en.
Proof.
  intros Sg Rp P Fb H H' Hext. fix IH 1.
  assert (Hall : forall g dd,
    (fix all (dd : xdenv) : Prop :=
       match dd with [] => True | p :: r => reified_entry Sg Rp P Fb H g (fst p) (snd p) /\ all r end) dd ->
    (fix all (dd : xdenv) : Prop :=
       match dd with [] => True | p :: r => reified_entry Sg Rp P Fb H' g (fst p) (snd p) /\ all r end) dd).
  { intros g. fix go 1. intros [|[i en] r] A; [exact I |].
    destruct A as [A1 A2]. split; [apply IH; exact A1 | apply go; exact A2]. }
  intros en g i A. destruct en as [w|f' d'|h sms dc|p|hs|ty|p held]; destruct i; simpl in A |- *; try exact A.
  - destruct (g_val Sg v) as [[? ?]|]; [exact (@xvt_mono Sg Rp P H H' Hext _ _ A) | exact A].
  - destruct (g_fn Sg f) as [[[? ?] [?|]]|]; try exact A.
    destruct (g_fn Sg f') as [[[? ?] [?|]]|]; try exact A.
    destruct (xp_fn P f'); try exact A.
    destruct A as [PhiD' [g' [A1 [A2 [A3 [A4 [A5 [A6 [A7 [A8 A9]]]]]]]]]].
    destruct A9 as [A9 A10].
    exists PhiD', g'. repeat (split; [assumption |]). apply Hall. exact A10.
  - destruct (g_mth Sg m) as [[? ?]|]; try exact A.
    destruct (g_fn Sg f') as [[[? ?] [?|]]|]; try exact A.
    destruct (xp_fn P f'); try exact A.
    destruct A as [PhiD' [g' [A1 [A2 [A3 [A4 [A5 [A6 [A7 [A8 A9]]]]]]]]]].
    destruct A9 as [A9 A10].
    exists PhiD', g'. repeat (split; [assumption |]). apply Hall. exact A10.
  - destruct (g_fun Sg u) as [[[? ?] [?|]]|]; try exact A.
    destruct (g_fun Sg h) as [[[? ?] [?|]]|]; try exact A.
    destruct (Fb h) as [[[? ?] ?]|]; try exact A.
    destruct (xp_fun P h) as [code|]; try exact A.
    destruct A as [Same [PhiD [PhiC [captured [D [C [G [Cover [Abs [Chain [Body [Available [Closed [Types Entries]]]]]]]]]]]]]].
    split;[exact Same|]. exists PhiD,PhiC,captured.
    repeat (split;[assumption|]). apply Hall;exact Entries.
Qed.

Lemma reified_environment_mono : forall Sg Rp P Fb H H',sext H H' ->forall g A d,
 reified_environment Sg Rp P Fb H g A d ->reified_environment Sg Rp P Fb H' g A d.
Proof.
 intros Sg Rp P Fb H H' Extend g A d [Available[Agree Types]].
 split;[exact Available|]. split;[|exact Types].
 intros i en Hin Lookup;eapply reified_entry_mono;[exact Extend|eapply Agree;eassumption].
Qed.

(* A successful body exports through the same complete-map operation used by
   the interpreter. The semantic premise is solely its static export map. *)
Theorem reified_body_exports : forall Sg Rp P Fb H PhiStart PhiEnd
 Din Dout exports gin bodyout bodyenv,
 body_export_contract Sg P PhiStart PhiEnd Din Dout exports ->
 ctx_grounded gin PhiStart ->ctx_grounded bodyout PhiEnd ->
 meanings_on(cA PhiStart)bodyout gin ->
 reified_environment Sg Rp P Fb H bodyout(cA PhiEnd)bodyenv ->
 exists result returned,schema_result_ground Din Dout gin result /\
 xcompose bodyenv exports=Some returned /\map fst returned=Dout /\
 reified_environment Sg Rp P Fb H result Dout returned.
Proof.
 intros Sg Rp P Fb H PhiStart PhiEnd Din Dout exports gin bodyout bodyenv
 Contract GroundStart GroundEnd Mean Env.
 destruct(Contract gin bodyout GroundStart GroundEnd Mean)as[result[Schema Map]].
 destruct(reified_map_composes Env Map)as[returned[Compose[Keys[Result Entries]]]].
 exists result,returned;auto.
Qed.
