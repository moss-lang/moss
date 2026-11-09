(* PublicPlatformApply's ChainForwardKeys, split by the length of the copy
   chain. The empty chain (F is the platform family itself, `bind Heap;`)
   is proved here from PublicFamilyAgree.items_agree_gen, F's result read
   at θ in the caller's context against its result at its own inputs. A
   chain with a copy is ChainForwardCopyKeys, a separate premise, which
   is false as stated (examples/linked-copy-bracket.moss; see the
   definition). *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicTagCheck
  PublicCallRealize PublicBodySlice PublicCopyApply
  PublicPlatformApply PublicFamilyAgree PublicChainNew.
From NM Require PublicFormation PublicTypeFacts.
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
Module PA := CN.LR.PA.
Module CopyA := PA.CopyA.
Module S := CopyA.S.

Section Fwd.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Notation Pre := (S.Pre W Prog argv read state_ok).

(* PREMISE (static correspondence, T15/T16): ChainForwardKeys for a copy
   chain of at least one copy (sms nonempty). FALSE AS STATED:
   examples/linked-copy-bracket.moss is a copy of Arrays with
   `Array.length[I32 = Z]`, applied inside a linked body. There the
   caller's run-time key carries `I32 = Z` while the root installs it at
   `θ(I32) = I32`, and Z and I32 are distinct symbols. The oracle
   elaborates it and is stuck; native prints 42
   (scratch/orch/chainfwd-STATUS.md). The fix is semantic, pending a
   design (installing under the caller's keys for F's declared result). *)
Definition ChainForwardCopyKeys : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st, Pre rr F C d types H g st ->
  forall code sms gfx, lookup_provision (IFun u) d = Some (FunctorClosure code sms CopyA.empty_env) ->
  sms <> [] ->
  S.FixedMeans W u d fx gfx ->
  forall h0 gf0, CopyA.CopyChain W u gfx sms h0 gf0 [] ->
  forall Ch0 needs outputs keys bindings, PA.FamilyData W Prog h0 code Ch0 needs outputs keys bindings ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list ->
  (forall k p, In (k, p) news -> exists r m g a, k = A.mkey r m /\ p = A.ProvidedMethod g a) ->
  forall r m g, A.mlook r m (mths_of news) = Some g ->
    (exists key, In key keys /\ app_subst_item th (A.rkey_of outputs key) = A.rkey C' r m) /\
    (forall key rf a gf, In key keys -> key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf ->
       app_subst_item th (A.rkey_of outputs key) = A.rkey C' r m ->
       Forall2 (PA.SigAgreeG d Ch0 th) (A.sg_ret g :: r :: A.sg_params g) (A.sg_ret gf :: rf :: A.sg_params gf)).

(* PREMISE (platform world, T16): a platform family is an abstract
   functor (lib/wasm.moss's Heap, Mutable, Arrays and Memories have no
   body; the oracle builds platform families only for these, Elab.v
   platform_fun). *)
Definition PlatformAbstract : Prop :=
  forall u code, Platform.Family W Prog u code ->
  forall dl ps a r b, A.w_decl W u = Some dl -> A.functor_parts (A.d_ast dl) = Some (ps, a, r, b) -> b = None.

(* PREMISE (platform world, T16): a platform family's items are distinct
   (each is its own declaration in lib/wasm.moss), as MethodsDistinct
   says of their methods. *)
Definition PlatformKeysDistinct : Prop :=
  forall u code Ch0 needs outputs keys bindings, PA.FamilyData W Prog u code Ch0 needs outputs keys bindings ->
  NoDup (map A.provision_id keys).

(* PREMISE (formation, T29): PlatformApply's FamilyKeysTyped at an
   application (AppPremises) instead of a family application: an extra
   key of the caller's table (one F's own inputs lack) that a path names
   is a type symbol, since a table's keys are type symbols or fresh
   generations, which no path names. *)
Definition AppKeysTyped : Prop :=
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall q pa' x, A.resolve_sym W q pa' = Some x -> A.nlook x (A.c_ty C) <> None ->
  A.nlook x (A.c_ty Cu) = None -> A.kind_sym W x = Some A.DKType.

Lemma chain_nil : forall u gfx sms h0 gf0 sms0, CopyA.CopyChain W u gfx sms h0 gf0 sms0 -> sms = [] -> u = h0.
Proof.
  intros u gfx sms h0 gf0 sms0 Ch. destruct Ch; intros E; [reflexivity|].
  exfalso. destruct sms; discriminate.
Qed.

Theorem chain_forward_keys : PlatformAbstract -> PlatformKeysDistinct -> AppKeysTyped ->
  ChainForwardCopyKeys -> PA.ChainForwardKeys W Prog argv read state_ok.
Proof.
  intros PAbs PKD AKT Copy F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0
    code sms gfx Look Fm h0 gf0 Chn Ch0 needs outputs keys bindings FD news En NewM r m g0 L.
  destruct sms as [|s0 sms'].
  2: { exact (Copy F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0
         code (s0 :: sms') gfx Look ltac:(discriminate) Fm h0 gf0 Chn Ch0 needs outputs keys bindings FD
         news En NewM r m g0 L). }
  pose proof (chain_nil _ _ _ _ _ _ Chn eq_refl) as <-.
  pose proof FD as (FC0 & NoH & PRf & FB & Binds & MD).
  pose proof AP as (_ & _ & FC & Inp & Th & _ & Sat & _ & _ & _ & _ & _ & PR).
  destruct (S.functx_unique W _ _ _ _ _ FC FC0) as [<- <-].
  pose proof Pre0 as (_ & _ & _ & _ & [Abs [_ [Idem _]]] & _).
  assert (Fam : Platform.Family W Prog u code) by (exists Cu, ns, outputs, keys, bindings; exact FD).
  assert (KeysM : forall key, In key keys -> exists r m a, key = IMth r m a /\ A.kind_sym W m = Some A.DKMeth).
  { intros key Ik. destruct (PA.forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [_ [op [_ [MP _]]]]].
    destruct MP as (r0 & m0 & a0 & d0 & form & nm & pars & res & body & -> & Dm & Ast & Form).
    exists r0, m0, a0. split; [reflexivity|]. unfold A.kind_sym. rewrite Dm, Ast. cbn.
    destruct form; [contradiction|reflexivity|reflexivity]. }
  destruct Sat as [_ [_ [_ [_ Merge]]]].
  assert (IA : PublicFamilyAgree.ItemsAgree C C' Cu outputs th
    (fun r m g => In (A.mkey r m) ks /\ A.mlook r m (A.c_mths C') = Some g) keys).
  { apply (PublicFamilyAgree.items_agree_gen W C u th C (fun t => TAbs (gensof t)) C' ks Cu ns outputs keys).
    - reflexivity.
    - split; [exact Th|]. split.
      { intros t It. destruct (keys_in _ t It) as [v Lv]. destruct (Inp t v Lv) as [here Lh]. rewrite Lh. discriminate. }
      split; [exact Merge|]. split; [exact (PAbs u code Fam)|exact PR].
    - right. split; [exact (PKD u code Cu ns outputs keys bindings FD)|exact MD].
    - exact FC0.
    - exact NoH.
    - exact PRf.
    - exact KeysM.
    - exact Idem.
    - exact (AKT F C pa u fx Cu ns sm th gensof C' ks newgens outs AP). }
  destruct IA as [IA _].
  destruct (IA r m g0 (conj (news_method_key W u C _ C' ks news PR En r m g0 L) (mlook_news C C' news r m g0 En L)))
    as [Key Sig].
  split; [exact Key|].
  intros key rf a gf Ik Ek Mgf Kq.
  pose proof (Sig key rf a gf Ik Ek Mgf Kq) as S2. revert S2. apply Forall2_impl. intros tc tf [Sc Eq]. split; [exact Sc|].
  rewrite <- (Abs tc), Eq. apply Abs.
Qed.

End Fwd.
End ChainFwd.
