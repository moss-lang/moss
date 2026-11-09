(* PublicPlatformApply's ChainForwardSigs, split by the length of the copy
   chain. The empty chain (F is the platform family itself, `bind Heap;`)
   is proved here from PublicFamilyAgree.items_agree_pos and
   items_agree_gen, F's result read at θ in the caller's context against
   its result at its own inputs. A chain with a copy is
   ChainForwardCopySigs, a separate premise (the signature half only:
   the keys are the caller's by construction, Audit.result_selection). *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicTagCheck
  PublicCallRealize PublicBodySlice PublicCopyApply
  PublicPlatformApply PublicFamilyAgree PublicChainNew.
From NM Require PublicFormation PublicTypeFacts PublicCallerKeys PublicOrientation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Static.
Variable W : A.world.

(* A new method of a result is one of the result's keys. *)
Lemma news_method_key : forall u C out C' ks news, A.ProvideResult W u C out C' ks ->
  A.c_provisions C' = (news ++ A.c_provisions C)%list ->
  forall r m g, A.mlook r m (mths_of news) = Some g -> In (A.mkey r m) ks.
Proof.
  intros u C out C' ks news (d & ps & a & rs & b & outs & es & tm' & _ & _ & _ & _ & RK & ->) En r m g L.
  cbn [A.output_cx A.c_provisions] in En. apply app_inv_tail in En. subst news.
  unfold A.ResultKeys in RK. subst ks.
  assert (Ex : exists q, In q (flat_map (A.entry_provision (A.c_sub C) (fun a => a)) (rev es)) /\
    exists a0, q = (IMth r m [], A.ProvidedMethod g a0) \/ (exists th, q = (IMth r m th, A.ProvidedMethod g a0))).
  { revert L. generalize (flat_map (A.entry_provision (A.c_sub C) (fun a => a)) (rev es)) as l.
    induction l as [|[k p] rest IH]; intros L; [discriminate|].
    unfold mths_of in L. cbn [flat_map] in L.
    destruct k as [| | | r' m' th' |]; destruct p as [|?|g' a'|];
      try (destruct (IH L) as [q [Iq E]]; exists q; split; [right; exact Iq|exact E]).
    cbn [app] in L. cbn [A.mlook] in L.
    destruct (ty_eqb r r' && Nat.eqb m m') eqn:Eq.
    - apply andb_true_iff in Eq as [Er Em]. apply PublicTypeFacts.ty_eqb_spec in Er. apply Nat.eqb_eq in Em. subst r' m'.
      injection L as Eg. subst g'. exists (IMth r m th', A.ProvidedMethod g a'). split; [left; reflexivity|].
      exists a'. right. exists th'. reflexivity.
    - destruct (IH L) as [q [Iq E]]. exists q. split; [right; exact Iq|exact E]. }
  destruct Ex as [q [Iq [a0 Eq]]].
  apply in_flat_map in Iq as [e [Ie Iq]]. apply in_rev in Ie.
  apply in_flat_map. exists e. split; [exact Ie|].
  destruct e as [t|v t a1|f g1 a1|r1 m1 g1 a1|u1 fx]; cbn in Iq; try contradiction;
    destruct Iq as [Eq'|[]]; subst q; destruct Eq as [Eq|[th Eq]]; try discriminate.
  all: injection Eq; intros; subst; cbn; left; reflexivity.
Qed.

Lemma mlook_news : forall C C' news r m g, A.c_provisions C' = (news ++ A.c_provisions C)%list ->
  A.mlook r m (mths_of news) = Some g -> A.mlook r m (A.c_mths C') = Some g.
Proof.
  intros C C' news r m g En L.
  change (A.c_mths C') with (mths_of (A.c_provisions C')). rewrite En.
  unfold mths_of. rewrite flat_map_app. rewrite PublicFamilyAgree.mlook_app'.
  fold (mths_of news). rewrite L. reflexivity.
Qed.

End Static.

Module ChainFwd (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module CN := PublicChainNew.ChainNew Prims I0.

Section Fwd.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Notation Pre := (CN.CC.LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok).

(* (Proved: PublicChainSigs.chain_forward_copy_sigs.) ChainForwardSigs for a copy
   chain of at least one copy (sms nonempty). The caller's last position
   for a key reads, through each copy's positions, an item of the root
   family; the caller's method there and the item have agreeing
   signatures. It is the signature half of the former
   ChainForwardCopyKeys, whose key half (false:
   examples/linked-copy-bracket.moss) the caller-keyed install makes true
   by construction (scratch/orch/callerkeys2-AUDIT.md). *)
Definition ChainForwardCopySigs : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  CN.CC.LR.PA.FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code sms rms gfx, lookup_provision (IFun u) d = Some (FunctorClosure code sms CN.CC.LR.PA.FS.CopyA.empty_env rms) ->
  sms <> [] ->
  CN.CC.LR.PA.FS.CopyA.S.FixedMeans W u d fx gfx ->
  forall h0 gf0, CN.CC.LR.PA.FS.CopyA.CopyChain W u gfx sms rms h0 gf0 [] [] ->
  forall Ch0 needs outputs keys bindings, CN.CC.LR.PA.FamilyData W Prog h0 code Ch0 needs outputs keys bindings ->
  forall i r m g key, nth_error ks i = Some (A.mkey r m) ->
  (forall i', i < i' -> nth_error ks i' <> Some (A.mkey r m)) ->
  A.mlook r m (A.c_mths C') = Some g ->
  In key keys -> nth_error (chain_sources rms (map (A.rkey_of outputs) keys)) i = Some (A.rkey_of outputs key) ->
  exists rf a gf, key = IMth rf m a /\ A.mlook rf m (A.c_mths outputs) = Some gf /\
    Forall2 (CN.CC.LR.PA.SigAgreeG d Ch0 th) (A.sg_ret g :: r :: A.sg_params g) (A.sg_ret gf :: rf :: A.sg_params gf).

(* PlatformApply's FamilyKeysTyped at an application (AppPremises)
   instead of a family application: an extra key of the caller's table
   (one F's own inputs lack) that a path names is a type symbol, since a
   formed table's keys are declared type symbols (PublicFormation.TysScoped).
   Proved: `app_keys_typed`. *)
Definition AppKeysTyped : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs, PublicFormation.TysScoped W C ->
  CN.CC.LR.PA.FS.CopyA.S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall q pa' x, A.resolve_sym W q pa' = Some x -> A.nlook x (A.c_ty C) <> None ->
  A.nlook x (A.c_ty Cu) = None -> A.kind_sym W x = Some A.DKType.

Lemma chain_nil : forall u gfx sms rms h0 gf0 sms0 rms0, CN.CC.LR.PA.FS.CopyA.CopyChain W u gfx sms rms h0 gf0 sms0 rms0 -> sms = [] ->
  u = h0 /\ sms0 = [] /\ rms = rms0.
Proof.
  intros u gfx sms rms h0 gf0 sms0 rms0 Ch. destruct Ch; intros E; [subst; split; [reflexivity|split; reflexivity]|].
  exfalso. destruct sms; discriminate.
Qed.

Lemma app_keys_typed : AppKeysTyped.
Proof. intros F C pa u fx Cu ns sm th gensof C' ks newgens outs Ts _ q pa' x _ Lx _.
  destruct (A.nlook x (A.c_ty C)) as [v|] eqn:L; [|congruence]. apply Ts. apply nlook_in in L.
  change x with (fst (x, v)). apply in_map. exact L.
Qed.

(* ChainForwardSigs: the empty chain (F is the platform family itself)
   from PublicFamilyAgree's readings of F's result at θ and at its own
   inputs, position by position (items_agree_pos) and item by item
   (items_agree_gen); a chain with a copy is ChainForwardCopySigs. *)
Theorem chain_forward_sigs : AppKeysTyped ->
  ChainForwardCopySigs -> CN.CC.LR.PA.ChainForwardSigs W Prog argv read state_ok.
Proof.
  intros AKT Copy F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0
    code sms rms gfx Look Fm h0 gf0 Chn Ch0 needs outputs keys bindings FD i r m g0 key Ki Last Mg Ikey Si.
  destruct sms as [|s0 sms'].
  2: { exact (Copy F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0
         code (s0 :: sms') rms gfx Look ltac:(discriminate) Fm h0 gf0 Chn Ch0 needs outputs keys bindings FD
         i r m g0 key Ki Last Mg Ikey Si). }
  destruct (chain_nil _ _ _ _ _ _ _ _ Chn eq_refl) as [<- [_ ->]].
  pose proof FD as (FC0 & NoH & PRf & FB & Binds & MD & Abst & KD).
  pose proof AP as (_ & _ & FC & Inp & Th & _ & Sat & _ & _ & _ & _ & _ & PR).
  destruct (CN.CC.LR.PA.FS.CopyA.S.functx_unique W _ _ _ _ _ FC FC0) as [<- <-].
  pose proof Pre0 as (_ & _ & _ & _ & [Abs [_ [Idem _]]] & Fmd).
  assert (KeysM : forall key, In key keys -> exists r m a, key = IMth r m a /\ A.kind_sym W m = Some A.DKMeth).
  { intros key0 Ik. destruct (CN.CC.LR.PA.forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [_ [op [_ [MP _]]]]].
    destruct MP as (r0 & m0 & a0 & d0 & form & nm & pars & res & body & -> & Dm & Ast & Form).
    exists r0, m0, a0. split; [reflexivity|]. unfold A.kind_sym. rewrite Dm, Ast. cbn.
    destruct form; [contradiction|reflexivity|reflexivity]. }
  destruct Sat as [_ [_ [_ [_ Merge0]]]].
  assert (Force : forall t, In t (map fst (A.c_ty Cu)) -> A.nlook t (A.c_ty C) <> None).
  { intros t It. destruct (keys_in _ t It) as [v Lv]. destruct (Inp t v Lv) as [here Lh]. rewrite Lh. discriminate. }
  pose proof (PublicOrientation.merges_by_pairs C Cu (map fst (A.c_ty Cu))
    (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (PublicCallRealize.functx_scoped W u Cu ns FC)))))))
    (PublicOrientation.functx_sub_function W u Cu ns FC) Force Merge0) as Merge.
  assert (Parts : th = map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu) /\
    (forall t, In t (map fst (A.c_ty Cu)) -> A.nlook t (A.c_ty C) <> None) /\
    (forall g t, In (g, t) (A.c_sub Cu) -> (forall x, In x (g :: A.gens t) -> A.nlook x (A.c_ty C) <> None) ->
       A.canon C (app_subst (A.c_ty C) (TAbs g)) = A.canon C (app_subst (A.c_ty C) t)) /\
    (forall d ps a r b, A.w_decl W u = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None) /\
    A.ProvideResult W u C (fun t => TAbs (gensof t)) C' ks).
  { split; [exact Th|]. split.
    { intros t It. destruct (keys_in _ t It) as [v Lv]. destruct (Inp t v Lv) as [here Lh]. rewrite Lh. discriminate. }
    split; [exact Merge|]. split; [exact Abst|exact PR]. }
  pose proof (AKT F C pa u fx Cu ns sm th gensof C' ks newgens outs (proj2 (proj2 (proj1 Fmd))) AP) as KT.
  pose proof (PublicFamilyAgree.items_agree_pos W C u th C (fun t => TAbs (gensof t)) C' ks Cu ns outputs keys
    eq_refl Parts (or_intror (conj KD MD)) FC0 NoH PRf KeysM Idem KT) as Pos.
  pose proof (PublicFamilyAgree.items_agree_gen W C u th C (fun t => TAbs (gensof t)) C' ks Cu ns outputs keys
    eq_refl Parts (or_intror (conj KD MD)) FC0 NoH PRf KeysM Idem KT) as [IA _].
  (* the item at position i is key *)
  cbn [chain_sources fold_left] in Si. rewrite nth_error_map in Si.
  destruct (nth_error keys i) as [key1|] eqn:Ek1; [|discriminate]. injection Si as Es.
  assert (E1 : key1 = key).
  { pose proof (PublicCallerKeys.rkey_of_inj outputs key1 key Es) as Pid.
    rewrite (PublicCallerKeys.result_key_id W u Cu _ outputs keys key1 PRf (nth_error_In _ _ Ek1)),
      (PublicCallerKeys.result_key_id W u Cu _ outputs keys key PRf Ikey) in Pid. exact Pid. }
  subst key1.
  pose proof (PublicCallerKeys.forall2_nth _ _ _ _ _ _ _ _ Pos Ki Ek1) as Kq. cbv beta in Kq.
  destruct (KeysM key Ikey) as (rf & m' & a & Ek & _).
  assert (Em : m' = m).
  { rewrite Ek in Kq. cbn [A.rkey_of A.rkey app_subst_item A.mkey] in Kq. unfold A.rkey in Kq. injection Kq as _ Em _. exact Em. }
  subst m'.
  destruct (CN.CC.LR.PA.forall2_in_l _ _ _ _ _ _ Binds Ikey) as [b [_ [op [_ [_ [AK _]]]]]].
  rewrite Ek in AK. destruct AK as [gf [Mgf _]].
  destruct (IA r m g0 (conj (nth_error_In _ _ Ki) Mg)) as [_ Sig].
  exists rf, a, gf. split; [exact Ek|]. split; [exact Mgf|].
  pose proof (Sig key rf a gf Ikey Ek Mgf Kq) as S2. revert S2. apply Forall2_impl. intros tc tf [Sc Eq]. split; [exact Sc|].
  rewrite <- (Abs tc), Eq. apply Abs.
Qed.

End Fwd.
End ChainFwd.
