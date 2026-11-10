(* The program entry (T10/T20): main's body runs under the slice's
   invariant, so run_main never reaches a Stuck signal. This file reduces
   the entry to the slice's safety (`Safe`) and main's initial realization
   (`Pre` at main's frame and context). *)
From Stdlib Require Import List String Arith Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicBodySlice
  PublicSafety PublicContextFacts PublicCallRealize PublicTagCheck PublicPrimitiveSafety PublicBoolTyping
  PublicCopyApply PublicFamilyStep PublicPlatformApply PublicLinkResults.
From NM Require PublicFormation PublicWorldFormation PublicFamilySig PublicChainCore PublicChainNew PublicChainFwd PublicLinkMerges PublicFamilyCoherent PublicFamilyLink PublicChainSigs PublicChainRealize PublicFamilyCopy.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Module EntrySafety (Host : HOST) (M0 : PUBLIC Host).
Module SR := SafetyReduction Host M0.
Module PS := PrimitiveSafety Host M0.
Module M := M0.
(* The slice, as the family, copy and platform-family steps instantiate it.
   Its parts are named by full path (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.S is
   PublicBodySlice), here and in every functor of the chain: inside a
   functor, `Module S := CC.S.` is not an alias but a copy of S, and since
   each level would copy every level below it, such abbreviations made the
   .vo files of the chain grow to hundreds of megabytes and its build take
   minutes. *)
Module FCp := PublicFamilyCopy.FamilyCopy M.ReferenceHost M.Reference.

Lemma machine_lookup : forall W name mach x, Platform.Layouts W -> In (name, mach) A.initial_machines ->
  A.LangSym W "wasm" A.NSName name x -> lookup_subst x (A.initial_machine_subst W) = Some (TMachine mach).
Proof.
  intros W name mach x [_ [Uniq _]] Inm Lang.
  pose proof Lang as (d & p & Decl & Lib & Scope).
  unfold A.LibraryModule in Lib. unfold A.initial_machine_subst.
  destruct (A.w_library W) as [dir|] eqn:Ld; [|contradiction]. subst p.
  change (String.append dir (String.append "wasm" ".moss")) with (String.append dir "wasm.moss") in Scope.
  assert (Abs : forall n' m', In (n', m') A.initial_machines -> In n' Platform.abstract_names)
    by (intros n' m' I; cbn in I; repeat (destruct I as [E|I]; [injection E as <- _; cbn; tauto|]); contradiction).
  assert (Names : NoDup (map fst A.initial_machines)) by (cbn; repeat constructor; cbn; intuition discriminate).
  assert (Gen : forall l, incl l A.initial_machines -> NoDup (map fst l) -> In (name, mach) l ->
    lookup_subst x (flat_map (fun q => match A.w_scope W (String.append dir "wasm.moss") A.NSName (fst q) with
      | Some (A.ESym symbol) => [(symbol, TMachine (snd q))] | _ => [] end) l) = Some (TMachine mach)).
  { induction l as [|[n' m'] rest IH]; intros Sub Nd Il; [contradiction|].
    inversion Nd as [|? ? Out Nd']; subst. cbn [flat_map fst snd].
    destruct (A.w_scope W (String.append dir "wasm.moss") A.NSName n') as [[s'|t']|] eqn:E.
    - cbn [app lookup_subst]. destruct (Nat.eqb_spec x s') as [->|Ne].
      + assert (L' : A.LangSym W "wasm" A.NSName n' s') by (exists d, (String.append dir "wasm.moss");
          split; [exact Decl|split; [unfold A.LibraryModule; rewrite Ld; reflexivity|exact E]]).
        destruct Il as [Eq|Il]; [injection Eq as -> ->; reflexivity|].
        exfalso. apply Out. replace n' with name; [change name with (fst (name, mach)); apply in_map, Il|].
        exact (Uniq name n' s' (Abs name mach (Sub _ (or_intror Il))) (Abs n' m' (Sub _ (or_introl eq_refl))) Lang L').
      + destruct Il as [Eq|Il]; [injection Eq as -> ->; congruence|].
        apply IH; [intros q Iq; apply Sub; right; exact Iq|exact Nd'|exact Il].
    - cbn [app]. destruct Il as [Eq|Il]; [injection Eq as -> ->; congruence|].
      apply IH; [intros q Iq; apply Sub; right; exact Iq|exact Nd'|exact Il].
    - cbn [app]. destruct Il as [Eq|Il]; [injection Eq as -> ->; congruence|].
      apply IH; [intros q Iq; apply Sub; right; exact Iq|exact Nd'|exact Il]. }
  apply Gen; [intros q Iq; exact Iq|exact Names|exact Inm].
Qed.

Section Entry.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Let P := program_of Prog argv read.
Variable state_ok : heap_types -> M.ReferenceHost.mstate -> Prop.

(* A substitution that fixes a type fixes each generation it mentions. *)
Lemma subst_fixed_gens : forall u s, app_subst s u = u -> forall y, In y (A.gens u) -> app_subst s (TAbs y) = TAbs y.
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms] s Fix y Iy; cbn [A.gens] in Iy; try contradiction.
  - rewrite subst_record in Fix. injection Fix as Fix.
    induction fields as [|[f t] rest Tail]; [contradiction|]. cbn in Fix, Iy. injection Fix as Ft Fr.
    apply in_app_or in Iy as [Iy|Iy]; [exact (IH t s Ft y Iy)|exact (Tail Fr Iy)].
  - destruct Iy as [<-|[]]. exact Fix.
  - rewrite subst_nom in Fix. injection Fix as Fix. unfold PublicTypeRead.smap in Fix.
    induction th as [|[x t] rest Tail]; [contradiction|]. cbn in Fix, Iy. injection Fix as Ft Fr.
    apply in_app_or in Iy as [Iy|Iy]; [exact (IH t s Ft y Iy)|exact (Tail Fr Iy)].
  - rewrite subst_union in Fix. injection Fix as Fix.
    induction ms as [|t rest Tail]; [contradiction|]. cbn in Fix, Iy. injection Fix as Ft Fr.
    apply in_app_or in Iy as [Iy|Iy]; [exact (IH t s Ft y Iy)|exact (Tail Fr Iy)].
Qed.

(* The initial type table: each type of main's context as it reads there,
   with the machine types' representations substituted. *)
Lemma entry_absorbs : forall f C ns ms platform, A.CtxOf W f C ns ->
  FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Absorbs C (Environment (map (fun q => (fst q, app_subst ms (snd q))) (A.c_ty C)) platform).
Proof.
  intros f C ns ms platform Ctx t. unfold ground, A.canon. cbn [type_meanings Environment].
  destruct (ctxof_formed W f C ns Ctx) as [Form Sub].
  pose proof (function_context_canonical W f C ns Ctx) as Idem.
  set (types := map (fun q => (fst q, app_subst ms (snd q))) (A.c_ty C)).
  assert (Lk : forall y, lookup_subst y types = option_map (app_subst ms) (A.nlook y (A.c_ty C))).
  { intros y. unfold types. rewrite FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_map_snd, lookup_subst_nlook. reflexivity. }
  rewrite <- (app_subst_nil t) at 2. apply subst2_agree. intros x _. cbn [app_subst lookup_subst].
  destruct (lookup_subst x (A.c_sub C)) as [u|] eqn:L; [|reflexivity].
  destruct (Sub x u (lookup_subst_in_pair _ _ _ L)) as [Ix Su].
  destruct (keys_in (A.c_ty C) x Ix) as [v Lv].
  pose proof (Form x v Lv) as Ev. cbn [app_subst] in Ev. rewrite L in Ev. subst v.
  rewrite Lk, Lv. cbn [option_map].
  assert (Fix : app_subst (A.c_sub C) u = u).
  { pose proof (Idem (TAbs x)) as I. unfold A.canon in I. cbn [app_subst] in I. rewrite L in I. exact I. }
  apply subst_agree_on. intros y Iy. cbn [app_subst]. rewrite Lk.
  destruct (keys_in (A.c_ty C) y (Su y Iy)) as [vy Ly]. rewrite Ly. cbn [option_map].
  rewrite (Form y vy Ly), (subst_fixed_gens u _ Fix y Iy). reflexivity.
Qed.

Lemma lookup_entry_some : forall key (l : list (item * provision_value)) pv,
  lookup_entry key l = Some pv -> In (key, pv) l.
Proof.
  intros key l pv. induction l as [|[k v] r IH]; intros L; [discriminate|]. cbn in L.
  destruct (item_eqb key k) eqn:E; [apply item_eqb_spec in E; subst k; injection L as ->; left; reflexivity|].
  right. exact (IH L).
Qed.

Lemma lookup_entry_in : forall key (l : list (item * provision_value)), In key (map fst l) ->
  exists pv, lookup_entry key l = Some pv /\ In (key, pv) l.
Proof.
  intros key l. induction l as [|[k v] r IH]; intros I; [contradiction|]. cbn.
  destruct (item_eqb key k) eqn:E.
  - apply item_eqb_spec in E. subst k. exists v. split; [reflexivity|left; reflexivity].
  - destruct I as [Ek|I]; [cbn in Ek; subst k; rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E; discriminate|].
    destruct (IH I) as [pv [L Ipv]]. exists pv. split; [exact L|right; exact Ipv].
Qed.

(* A context's val types are canonical: read under its merges. *)
Lemma ctx_val_canonical : forall f C ns v t a, A.CtxOf W f C ns ->
  In (IVal v, A.ProvidedVal t a) (A.c_provisions C) -> A.canon C t = t.
Proof.
  intros f C ns v t a Ctx I. pose proof (function_context_canonical W f C ns Ctx) as Idem.
  destruct Ctx as (d & es & tm & er & tmr & σ & _ & _ & _ & _ & -> & _ & _).
  cbn [A.c_provisions A.entries_cx] in I. apply in_flat_map in I as [e [_ Ie]].
  destruct e as [x|v' t0 a0|f' g a0|r m g a0|u fx]; cbn in Ie; try (destruct Ie as [E|[]]; discriminate); [contradiction|].
  destruct Ie as [E|[]]. injection E as -> <- _.
  exact (Idem t0).
Qed.

Lemma i32_machine : forall s, A.LangSym W "wasm" A.NSName "I32" s ->
  lookup_subst s (A.initial_machine_subst W) = Some (TMachine MI32).
Proof.
  intros s (d & p & _ & Lib & Scope). unfold A.LibraryModule in Lib. unfold A.initial_machine_subst.
  destruct (A.w_library W) as [dir|]; [|contradiction]. subst p. cbn [flat_map A.initial_machines fst snd].
  change (String.append dir (String.append "wasm" ".moss")) with (String.append dir "wasm.moss") in Scope.
  rewrite Scope. cbn. rewrite Nat.eqb_refl. reflexivity.
Qed.

(* A fixed machine type's class is the type alone, so a canonical context
   reads it, and the fixed meaning, as itself. *)
Lemma fixed_machine_self : forall C t x, canonical_context C ->
  A.canon C t = A.canon C (TAbs x) -> (exists z, A.canon C (TAbs x) = TAbs z) ->
  (forall y, A.canon C (TAbs y) = A.canon C (TAbs x) -> y = x) ->
  A.canon C t = TAbs x.
Proof.
  intros C t x Can Et [z Ez] Only. rewrite Et, Ez. f_equal. apply Only.
  rewrite <- Ez at 1. exact (Can (TAbs x)).
Qed.

(* The platform's provisions realize main's context, given the machine
   types' meanings and its primitives' callability. *)
Lemma entry_realizes : forall n C ns types platform H0, Platform.Layouts W ->
  A.CtxOf W (program_main Prog) C ns -> A.InitialTypes W C types ->
  map fst platform = map (A.rkey_of C) ns ->
  Forall (fun ie => Platform.Entry W Prog C (fst ie) (snd ie)) platform ->
  (forall f op sg, In (IFn f, PrimitiveClosure op None) platform ->
     A.nlook f (A.c_fns C) = Some sg -> Platform.AtKey W C (IFn f) op ->
     FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CallableOk W Prog argv read state_ok n H0 (Environment types platform) (A.sg_params sg) (A.sg_ret sg)
       (PrimitiveClosure op None)) ->
  FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Realizes W Prog argv read state_ok n C (Environment types platform) H0.
Proof.
  intros n C ns types platform H0 Lay Ctx Types Keys Entries Prims.
  pose proof (ctx_keys_match W _ C ns Ctx) as KM.
  destruct (ctx_needs W _ C ns Ctx) as [_ [ProvNs _]].
  rewrite Forall_forall in Entries.
  assert (Find : forall k, In k ns -> exists pv, lookup_entry (A.rkey_of C k) platform = Some pv /\
     Platform.Entry W Prog C (A.rkey_of C k) pv).
  { intros k Ik. assert (Im : In (A.rkey_of C k) (map fst platform)) by (rewrite Keys; apply in_map, Ik).
    destruct (lookup_entry_in _ _ Im) as [pv [L Ipv]]. exists pv. split; [exact L|exact (Entries _ Ipv)]. }
  unfold A.InitialTypes in Types. subst types.
  split; [exact (entry_absorbs _ C ns _ platform Ctx)|].
  split; [|split; [exact (function_context_canonical W _ C ns Ctx)|exact KM]].
  split; [|split; [|split]].
  - intros v t Found. destruct (vals_ilook _ v t KM Found) as [a Il].
    destruct (Find (IVal v) (ProvNs _ _ (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn [A.rkey_of] in L, En. destruct pv as [w|f' df|u sms dc rms0|p held]; cbn in En; try contradiction.
    destruct w; cbn in En; try contradiction. destruct En as [t' [Lt Mt]].
    rewrite Found in Lt. injection Lt as <-. exists (VI32 i). split; [exact L|].
    destruct Mt as [sym [Lang [_ ->]]].
    pose proof (ctx_val_canonical _ C ns v _ a Ctx (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il)) as Canon.
    destruct (ctx_provision_scoped W _ C ns Ctx _ _ (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il)) as [Sc _].
    assert (Isym : In sym (map fst (A.c_ty C))) by (apply Sc; cbn; left; reflexivity).
    destruct (ctxof_formed W _ C ns Ctx) as [Form _].
    destruct (keys_in (A.c_ty C) sym Isym) as [vs Ls]. pose proof (Form sym vs Ls) as Ev.
    change (app_subst (A.c_sub C) (TAbs sym)) with (A.canon C (TAbs sym)) in Ev. rewrite Canon in Ev. subst vs.
    unfold ground. cbn [type_meanings Environment app_subst].
    rewrite FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_map_snd, lookup_subst_nlook, Ls. cbn [option_map app_subst].
    rewrite (i32_machine sym Lang). constructor.
  - intros f g Found. destruct (fns_ilook _ f g KM Found) as [a Il].
    destruct (Find (IFn f) (ProvNs _ _ (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn [A.rkey_of] in L, En. destruct pv as [w|f' df|u sms dc rms0|p held]; cbn in En; try contradiction.
    destruct held; cbn in En; try contradiction. exists (PrimitiveClosure p None). split; [exact L|].
    exact (Prims f p g (lookup_entry_some _ _ _ L) Found (proj1 En)).
  - intros r m g Found. destruct (mths_ilook _ r m g KM Found) as [a Il].
    destruct (Find (A.mkey r m) (ProvNs _ _ (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn in En. contradiction.
  - intros u fx Found. pose proof (funs_ilook _ u fx KM Found) as Il.
    destruct (Find (IFun u) (ProvNs _ _ (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn [A.rkey_of] in L, En. destruct pv as [w|f' df|u' sms dc rms0|p held]; cbn in En; try contradiction.
    destruct sms; [contradiction|]. destruct dc as [[|? ?] [|? ?] [|? ?]]; try contradiction. destruct rms0; [contradiction|].
    exists (FunctorClosure u' None (EnvironmentWithAliases [] [] []) None),
      (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ground_fixed (map (fun q => (fst q, app_subst (A.initial_machine_subst W) (snd q))) (A.c_ty C)) fx).
    destruct (ctx_fun_fixed W _ C ns Ctx u fx (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il)) as [Cov [Sc Fun]].
    split; [exact L|]. split; [apply FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CPlatform; [exact (proj1 En)| |]|].
    { (* main's table reads a fixed machine input as its machine type *)
      intros name mach x Inm Lang Fx. destruct (Cov x Fx) as [tx Itx].
      rewrite (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_ground_fixed _ fx x tx Fun Itx).
      pose proof (ctx_fun_self W _ C ns Ctx u fx (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il) x tx Itx) as Etx.
      destruct En as [_ FMR]. destruct (FMR fx Found name mach x tx Inm Lang Itx) as (Ct0 & Str & Only).
      pose proof (fixed_machine_self C tx x (function_context_canonical W _ C ns Ctx) Ct0 Str Only) as Ct.
      assert (Cx : A.canon C (TAbs x) = TAbs x).
      { rewrite Etx in Ct. change (app_subst (A.c_sub C) (TAbs x)) with (A.canon C (TAbs x)) in Ct.
        rewrite (function_context_canonical W _ C ns Ctx) in Ct. exact Ct. }
      change (app_subst (A.c_sub C) (TAbs x)) with (A.canon C (TAbs x)) in Etx. rewrite Cx in Etx. subst tx.
      assert (Ix : In x (map fst (A.c_ty C))) by (apply (Sc x (TAbs x) Itx); left; reflexivity).
      destruct (keys_in (A.c_ty C) x Ix) as [v Lv].
      destruct (ctxof_formed W _ C ns Ctx) as [Form _]. pose proof (Form x v Lv) as Ev.
      change (app_subst (A.c_sub C) (TAbs x)) with (A.canon C (TAbs x)) in Ev. rewrite Cx in Ev. subst v.
      cbn [app_subst]. rewrite FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_map_snd, lookup_subst_nlook, Lv. cbn [option_map app_subst].
      rewrite (machine_lookup W name mach x Lay Inm Lang). reflexivity. }
    { (* the same, syntactically: main records a fixed machine input as itself *)
      exists fx, (map (fun q => (fst q, app_subst (A.initial_machine_subst W) (snd q))) (A.c_ty C)).
      split; [reflexivity|]. split; [exact Cov|]. split; [exact Fun|].
      intros name mach x tx Inm Lang Itx.
      pose proof (ctx_fun_self W _ C ns Ctx u fx (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il) x tx Itx) as Etx.
      destruct En as [_ FMR]. destruct (FMR fx Found name mach x tx Inm Lang Itx) as (Ct0 & Str & Only).
      pose proof (fixed_machine_self C tx x (function_context_canonical W _ C ns Ctx) Ct0 Str Only) as Ct.
      assert (Cx : A.canon C (TAbs x) = TAbs x).
      { rewrite Etx in Ct. change (app_subst (A.c_sub C) (TAbs x)) with (A.canon C (TAbs x)) in Ct.
        rewrite (function_context_canonical W _ C ns Ctx) in Ct. exact Ct. }
      change (app_subst (A.c_sub C) (TAbs x)) with (A.canon C (TAbs x)) in Etx. rewrite Cx in Etx. exact Etx. }
    split; [exact Cov|].
    intros x tx I. exact (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_ground_fixed _ fx x tx Fun I).
Qed.

(* main's body runs from its frame and context, with no locals. *)
Theorem main_not_stuck : forall fuel d0 st0 H0,
  (forall k, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Safe W Prog argv read state_ok k) ->
  (forall dcl n rt b, A.w_decl W (program_main Prog) = Some dcl -> A.d_ast dcl = ADFn RPlain n [] rt (Some b) ->
     A.FnElab W Prog (program_main Prog)) ->
  (exists dcl n rt b, A.w_decl W (program_main Prog) = Some dcl /\ A.d_ast dcl = ADFn RPlain n [] rt (Some b)) ->
  (forall C ns, A.CtxOf W (program_main Prog) C ns -> forall F g, FrameOk F -> A.fr_this F = None ->
     A.fr_ret F = Some (A.sg_ret g) -> A.SigOf W (program_main Prog) (A.c_ty C) None g ->
     FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok fuel F C d0 [] H0 [] st0) ->
  ~ SR.stuck_signal (M.Reference.evaluation_signal (M.Reference.run_main fuel P (program_main Prog) d0 st0)).
Proof.
  intros fuel d0 st0 H0 Safe Elab [dcl [n [rt [b [Decl Ast]]]]] Entry.
  destruct (Elab dcl n rt b Decl Ast dcl RPlain n [] rt b Decl Ast)
    as (C & ns & this & g & F & x & t & Ctx & This & Sig & _ & Ret & FThis & Loop & _ & _ & _ & Num & Body & View).
  cbn in This. subst this.
  unfold M.Reference.run_main. change (view_function P (program_main Prog)) with (A.body_of Prog (program_main Prog)).
  rewrite View.
  assert (Frame : FrameOk F) by (apply (numbering_frame F []); rewrite FThis; exact Num).
  pose proof (Entry C ns Ctx F g Frame FThis Ret Sig) as Pre0.
  pose proof (Safe fuel) as Sf. unfold FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Safe in Sf. destruct Sf as [_ [_ [SBlkE _]]].
  pose proof (SBlkE F C b (Some (A.sg_ret g)) x t Body d0 [] H0 [] st0 Pre0) as Out.
  unfold FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.Outcome in Out. unfold P. change (M.Reference.eval fuel (program_of Prog argv read) d0 [] st0 x) with (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.eval fuel (program_of Prog argv read) d0 [] st0 x).
  destruct (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.eval fuel (program_of Prog argv read) d0 [] st0 x) as [[] g1 st1]; cbn in Out |- *; auto.
  all: intros _; try contradiction; destruct Out as [_ [L _]]; congruence.
Qed.

(* main's initial state: its frame, no locals, the platform's provisions. *)
Lemma entry_pre : forall n F C ns g types platform H0 st0,
  PublicFormation.MergeFormed W -> PublicFormation.MentionsTyped W ->
  A.CtxOf W (program_main Prog) C ns -> FrameOk F -> A.fr_this F = None ->
  A.fr_ret F = Some (A.sg_ret g) -> A.SigOf W (program_main Prog) (A.c_ty C) None g ->
  state_ok H0 st0 -> A.InitialTypes W C types -> FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Realizes W Prog argv read state_ok n C (Environment types platform) H0 ->
  FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok n F C (Environment types platform) [] H0 [] st0.
Proof.
  intros n F C ns g types platform H0 st0 Mf Mt Ctx Frame This Ret Sig St Types Real.
  destruct (PublicFormation.fn_body_formed W _ C ns None g [] Mf Mt Ctx Sig (PublicFormation.optok_none W)) as [Sg Fc].
  split; [exact Frame|]. split; [split|split; [apply Forall2_nil|split; [exact St|split; [exact Real|]]]].
  3: { split; [exact Fc|]. split; [split; [rewrite Ret; apply PublicFormation.optok_some; exact (proj2 Sg)|rewrite This; apply PublicFormation.optok_none]|].
       exact (PublicScoping.fn_body_cxscoped W _ C ns F None g [] Ctx Sig This (PublicScoping.optscoped_none W C)). }
  - intros m t mu Found. rewrite (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ctxof_locals W _ C ns Ctx) in Found. discriminate.
  - split; [intros t E; congruence|]. apply FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.alias_none; [reflexivity|exact (proj1 Real)| |].
    + exact (proj1 (ctxof_formed W _ C ns Ctx)).
    + unfold A.InitialTypes in Types. subst types. exact (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.keys_meant_map C _ (A.c_ty C) eq_refl).
Qed.
End Entry.

(* ---- the platform's primitives at the entry ----------------------------------- *)


(* A context's fn signatures are canonical: read under its merges. *)
Lemma ctx_fn_canonical : forall W f C ns g sg a, A.CtxOf W f C ns ->
  In (IFn g, A.ProvidedFn sg a) (A.c_provisions C) ->
  forall t, In t (A.sg_ret sg :: A.sg_params sg) -> A.canon C t = t.
Proof.
  intros W f C ns g sg a Ctx I t It. pose proof (function_context_canonical W f C ns Ctx) as Idem.
  destruct Ctx as (d & es & tm & er & tmr & σ & _ & _ & _ & _ & -> & _ & _).
  cbn [A.c_provisions A.entries_cx] in I. apply in_flat_map in I as [e [_ Ie]].
  destruct e as [x|v' t0 a0|f' g0 a0|r m g0 a0|u fx]; cbn in Ie; try (destruct Ie as [E|[]]; discriminate); [contradiction|].
  destruct Ie as [E|[]]. injection E as -> <- _. cbn [A.sg_ret A.sg_params] in It.
  destruct It as [<-|It]; [exact (Idem _)|]. apply in_map_iff in It as [u [<- _]]. exact (Idem u).
Qed.

Lemma lookup_app : forall x (l m : subst),
  lookup_subst x (l ++ m)%list = match lookup_subst x l with Some v => Some v | None => lookup_subst x m end.
Proof. intros x l m. rewrite !lookup_subst_nlook. apply nlook_app'. Qed.

(* A platform primitive in main's initial environment is a typed call. *)
Lemma entry_primitive : forall W Prog argv read iface C ns types platform n H0 f op sg,
  Platform.Layouts W -> Platform.Representations W Prog -> BoolSymbols W Prog ->
  M.interface_laws (program_of Prog argv read) iface -> M.supports iface op = true ->
  A.CtxOf W (program_main Prog) C ns -> A.InitialTypes W C types ->
  A.nlook f (A.c_fns C) = Some sg -> Platform.AtKey W C (IFn f) op ->
  Forall (HandleFormed W) (A.sg_ret sg :: A.sg_params sg) ->
  FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CallableOk W Prog argv read (PS.state_ok W (program_of Prog argv read) iface) n H0
    (Environment types platform) (A.sg_params sg) (A.sg_ret sg) (PrimitiveClosure op None).
Proof.
  intros W Prog argv read iface C ns types platform n H0 f op sg Lay Reps Bools Laws Supp Ctx Types Found At Formed.
  destruct At as [sg' [Found' Sig]]. rewrite Found in Found'. injection Found' as <-.
  pose proof (ctx_keys_match W _ C ns Ctx) as KM.
  destruct (fns_ilook _ f sg KM Found) as [a Il]. pose proof (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il) as InP.
  destruct (ctx_provision_scoped W _ C ns Ctx _ _ InP) as [Sc _]. unfold provision_sig_gens in Sc. cbn in Sc.
  pose proof (ctx_fn_canonical W _ C ns f sg a Ctx InP) as Canon.
  destruct (ctxof_formed W _ C ns Ctx) as [Form _].
  set (d0 := Environment types platform).
  set (K := A.sig_gens sg).
  set (m' := (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.capture_meanings d0 K ++ A.initial_machine_subst W)%list).
  assert (OnK : forall x, In x K -> app_subst m' (TAbs x) = ground d0 (TAbs x)).
  { intros x Ix. cbn [app_subst]. unfold m'. rewrite lookup_app, (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_capture d0 K x Ix). reflexivity. }
  assert (SigK : forall t, In t (A.sg_ret sg :: A.sg_params sg) -> incl (A.gens t) K).
  { intros t It x Ix. unfold K, A.sig_gens. apply in_flat_map. exists t. split; [exact It|exact Ix]. }
  assert (G : forall t, In t (A.sg_ret sg :: A.sg_params sg) -> app_subst m' t = ground d0 t).
  { intros t It. unfold ground. apply subst_agree_on. intros x Ix. exact (OnK x (SigK t It x Ix)). }
  assert (Ground : MachinesGround W m').
  { intros name mach sym Inm Lang. destruct (in_dec Nat.eq_dec sym K) as [Ik|Ok].
    - rewrite (OnK sym Ik). unfold d0, ground. unfold A.InitialTypes in Types. subst types.
      cbn [type_meanings Environment app_subst]. rewrite FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_map_snd, lookup_subst_nlook.
      assert (Ikey : In sym (map fst (A.c_ty C))) by exact (Sc sym Ik).
      destruct (keys_in (A.c_ty C) sym Ikey) as [v Lv]. rewrite Lv. cbn [option_map].
      unfold K, A.sig_gens in Ik. apply in_flat_map in Ik as [t [It Ix]].
      pose proof (subst_fixed_gens t (A.c_sub C) (Canon t It) sym Ix) as Fix.
      rewrite (Form sym v Lv), Fix. cbn [app_subst]. rewrite (machine_lookup W name mach sym Lay Inm Lang). reflexivity.
    - cbn [app_subst]. unfold m'. rewrite lookup_app.
      assert (Out : lookup_subst sym (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.capture_meanings d0 K) = None).
      { unfold FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.capture_meanings. clear -Ok. induction K as [|y r IH]; [reflexivity|]. cbn.
        destruct (Nat.eqb_spec sym y) as [->|_]; [exfalso; apply Ok; left; reflexivity|].
        apply IH. intro I; apply Ok; right; exact I. }
      rewrite Out, (machine_lookup W name mach sym Lay Inm Lang). reflexivity. }
  cbn [FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CallableOk]. intros H st ws Ext St Args.
  rewrite <- (G (A.sg_ret sg) (or_introl eq_refl)).
  apply (PS.primitive_safe W Prog argv read iface op None (A.sg_params sg) (A.sg_ret sg) m' H st ws
    Lay Reps Bools Laws Supp Sig Ground Formed I St).
  clear -Args G. induction Args as [|w t ws ts T Rest IH]; constructor.
  - rewrite G; [exact T|right; left; reflexivity].
  - apply IH. intros t' It'. apply G. destruct It' as [<-|It']; [left; reflexivity|right; right; exact It'].
Qed.

(* The AtKey signatures of a platform family's primitives. *)
Definition FamilySig (W : A.world) (Prog : program) (u : nat) (inputs outputs : A.cx) (keys : list item)
    (P : A.fsig -> ty -> Prop) : Prop :=
  forall code needs bindings, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FamilyData W Prog u code inputs needs outputs keys bindings ->
  forall key op rf m a gf, In key keys -> Platform.AtKey W outputs key op ->
  key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf -> P gf rf.

(* A platform family's primitive signatures (AtKey in its result) have
   their handle tags formed (platform_handles_formed), as main's platform
   fns' are (handles_formed). *)
Definition PlatformHandlesFormed (W : A.world) (Prog : program) : Prop :=
  forall u inputs outputs keys, FamilySig W Prog u inputs outputs keys
    (fun gf rf => Forall (HandleFormed W) (A.sg_ret gf :: rf :: A.sg_params gf)).

(* A machine type symbol a platform family's signature mentions is a fixed
   input of the family's functor, in its context's table
   (Platform.MachinesFixed). *)
Definition PlatformSigsFixed (W : A.world) (Prog : program) : Prop :=
  forall u inputs outputs keys, FamilySig W Prog u inputs outputs keys (fun gf rf =>
    forall name mach sym, In (name, mach) A.initial_machines -> A.LangSym W "wasm" A.NSName name sym ->
    In sym (flat_map A.gens (A.sg_ret gf :: rf :: A.sg_params gf)) ->
    A.Fixed W u sym /\ In sym (map fst (A.c_ty inputs))).

Lemma platform_sigs_fixed : forall W Prog, PlatformSigsFixed W Prog.
Proof.
  intros W Prog u inputs outputs keys code needs bindings FD key op rf m a gf Ik AK Ek Mgf name mach sym Inm Lang Is.
  pose proof FD as (_ & _ & _ & _ & Binds & _).
  destruct (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [_ [op' [_ [_ [_ [MF _]]]]]]].
  rewrite Ek in MF. exact (MF gf Mgf name mach sym Inm Lang Is).
Qed.

Lemma mlook_in_key : forall r m (l : list ((ty * nat) * A.fsig)) g, A.mlook r m l = Some g -> In ((r, m), g) l.
Proof.
  intros r m l g. induction l as [|[[r' m'] g'] rest IH]; intros Found; [discriminate|]. cbn in Found.
  destruct (ty_eqb r r') eqn:Er; destruct (Nat.eqb_spec m m') as [->|_]; cbn in Found;
    try (right; exact (IH Found)).
  apply PublicTypeFacts.ty_eqb_spec in Er. subst r'. injection Found as <-. left. reflexivity.
Qed.

(* A method's receiver in a formed context is ok. *)
Lemma mths_recv_ok : forall W C r m g, PublicFormation.CxFormed W C -> A.mlook r m (A.c_mths C) = Some g ->
  PublicFormation.TyOk W r.
Proof.
  intros W C r m g Cf L. pose proof (mlook_in_key _ _ _ _ L) as I. unfold A.c_mths in I.
  apply in_flat_map in I as [[k p] [Iq Ik]].
  destruct (PublicFormation.cf_prov _ _ Cf k p Iq) as [Kk _].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as -> _ _. exact Kk.
Qed.

(* A platform family's signatures have their handles formed: each item
   reads its raw handles as themselves (Platform.HandlesRead), and is
   formed, since the family's result context is. *)
Lemma platform_handles_formed : forall W Prog, (exists b, A.WorldOf b W) -> PlatformHandlesFormed W Prog.
Proof.
  intros W Prog [b Wo] u inputs outputs keys code needs bindings FD key op rf m a gf Ik AK Ek Mgf.
  pose proof FD as (FC & _ & PRf & _ & Binds & _).
  destruct (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.forall2_in_l _ _ _ _ _ _ Binds Ik) as [bd [_ [op' [_ [_ [_ [_ HR]]]]]]].
  rewrite Ek in HR. pose proof (HR gf Mgf) as Raws.
  pose proof (PublicFormation.functx_cx_formed W (PublicWorldFormation.merge_formed_of W)
    (PublicWorldFormation.mentions_typed_of b W Wo) u inputs needs FC) as Fi.
  pose proof (PublicFormation.apply_abstract_formed W inputs u (fun t => t) outputs keys Fi
    (fun t _ => or_introl eq_refl) PRf) as Fo.
  pose proof (PublicFormation.mths_ok W outputs rf m gf (proj1 Fo) Mgf) as [Fp Fr].
  pose proof (mths_recv_ok W outputs rf m gf (proj1 Fo) Mgf) as Fv.
  assert (Formed : Forall (PublicFormation.TyOk W) (A.sg_ret gf :: rf :: A.sg_params gf)) by (constructor; [exact Fr|constructor; [exact Fv|exact Fp]]).
  rewrite Forall_forall in Formed, Raws |- *. intros t It name param handle symbol args Tag Sym ->.
  split.
  - destruct (Formed _ It) as [Ft _]. exact (proj1 (proj1 (PublicFormation.nom_formed W symbol args) Ft)).
  - apply (Raws _ It name handle symbol args); [|exact Sym|reflexivity].
    cbn in Tag. destruct Tag as [E|[E|[E|[]]]]; injection E as <- <- <-; cbn; auto.
Qed.

(* A platform family's primitives are typed calls (PrimsGroundOk, so
   ChainPrimsOk and the family application's) for the reference host,
   from primitive_safe, as main's are (entry_primitive). *)
Lemma platform_prims_public : forall W Prog argv read iface,
  Platform.Layouts W -> Platform.Representations W Prog -> BoolSymbols W Prog ->
  M.interface_laws (program_of Prog argv read) iface ->
  (forall op, In op (required_primitives Prog) -> M.supports iface op = true) ->
  PlatformHandlesFormed W Prog -> PlatformSigsFixed W Prog ->
  FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.PrimsGroundOk W Prog argv read (PS.state_ok W (program_of Prog argv read) iface).
Proof.
  intros W Prog argv read iface Lay Reps Bools Laws Supp Handles Fixed
    rr H de code h0 Ch0 needs outputs keys bindings FD MachG key op rf m a gf
    Ik Ib MP AK Ek Mgf.
  pose proof (Handles h0 Ch0 outputs keys code needs bindings FD key op rf m a gf Ik AK Ek Mgf) as Formed.
  assert (Mach : forall name mach sym, In (name, mach) A.initial_machines -> A.LangSym W "wasm" A.NSName name sym ->
    In sym (flat_map A.gens (A.sg_ret gf :: rf :: A.sg_params gf)) ->
    ground de (TAbs sym) = TMachine mach).
  { intros name mach sym Inm Lang Is.
    destruct (Fixed h0 Ch0 outputs keys code needs bindings FD key op rf m a gf Ik AK Ek Mgf name mach sym Inm Lang Is) as [Fx Ix].
    exact (MachG name mach sym Inm Lang Fx Ix). }
  pose proof AK as AK'. rewrite Ek in AK'. destruct AK' as [sg' [M' Sig]]. rewrite Mgf in M'. injection M' as <-.
  set (K := flat_map A.gens (A.sg_ret gf :: rf :: A.sg_params gf)).
  set (m' := (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.capture_meanings de K ++ A.initial_machine_subst W)%list).
  assert (OnK : forall x, In x K -> app_subst m' (TAbs x) = ground de (TAbs x)).
  { intros x Ix. cbn [app_subst]. unfold m'. rewrite lookup_app, (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_capture de K x Ix). reflexivity. }
  assert (G : forall t, In t (A.sg_ret gf :: rf :: A.sg_params gf) -> app_subst m' t = ground de t).
  { intros t It. unfold ground. apply subst_agree_on. intros x Ix. apply OnK.
    unfold K. apply in_flat_map. exists t. split; [exact It|exact Ix]. }
  assert (Ground : MachinesGround W m').
  { intros name mach sym Inm Lang. destruct (in_dec Nat.eq_dec sym K) as [Ik'|Ok].
    - rewrite (OnK sym Ik'). exact (Mach name mach sym Inm Lang Ik').
    - cbn [app_subst]. unfold m'. rewrite lookup_app.
      assert (Out : lookup_subst sym (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.capture_meanings de K) = None).
      { unfold FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.capture_meanings. clear -Ok. induction K as [|y r IH]; [reflexivity|]. cbn.
        destruct (Nat.eqb_spec sym y) as [->|_]; [exfalso; apply Ok; left; reflexivity|].
        apply IH. intro I; apply Ok; right; exact I. }
      rewrite Out, (machine_lookup W name mach sym Lay Inm Lang). reflexivity. }
  assert (Supported : M.supports iface op = true).
  { apply Supp. unfold required_primitives. apply in_or_app. right. apply in_or_app. left.
    pose proof FD as (_ & _ & _ & FB & _). unfold A.funbody_of in FB.
    apply in_flat_map. exists (code, FunctorCode (map fst (A.c_ty Ch0)) (map fst (A.c_ty Ch0)) []
      [] bindings (map (A.rkey_of outputs) keys)). split; [exact (nlook_in _ _ _ _ FB)|].
    cbn [snd fc_bindings]. apply in_flat_map. exists (BPrimitive (A.rkey_of outputs key) op).
    split; [exact Ib|left; reflexivity]. }
  assert (Held : HeldAt m' (rf :: A.sg_params gf)
    (match op, A.rkey_of outputs key with
     | PBox _, IMth tau _ _ | PArrNew _, IMth tau _ _ => Some (ground de tau)
     | _, _ => None end)).
  { rewrite Ek. cbn [A.rkey_of A.rkey]. unfold A.rkey.
    destruct op; cbn; try exact I; exists rf, (A.sg_params gf); (split; [reflexivity|]);
      symmetry; apply G; right; left; reflexivity. }
  unfold primitive_closure. cbn [FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CallableOk]. intros H1 st1 ws Ext St Args.
  rewrite <- (G (A.sg_ret gf) (or_introl eq_refl)).
  apply (PS.primitive_safe W Prog argv read iface op _ (rf :: A.sg_params gf) (A.sg_ret gf) m' H1 st1 ws
    Lay Reps Bools Laws Supported Sig Ground Formed Held St).
  assert (G' : forall t, In t (rf :: A.sg_params gf) -> app_subst m' t = ground de t)
    by (intros t It; apply G; right; exact It).
  revert Args G'. generalize (rf :: A.sg_params gf) as ps. clearbody m'. intros ps Args G'.
  clear -Args G'. induction Args as [|w t ws ts T Rest IH]; constructor.
  - rewrite G'; [exact T|left; reflexivity].
  - apply IH. intros t' It'. apply G'. right. exact It'.
Qed.

(* T20: an admissible program never reaches a Stuck entry signal, given the
   slice's safety, an initial ghost heap the state realizes, and callable
   platform primitives. *)
Theorem entry_safe : forall prelude
    (state_ok : A.world -> program_view -> M.interface -> heap_types -> M.ReferenceHost.mstate -> Prop),
  (forall Prog iface initial bundle W, M.Compatible Prog iface ->
     A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
     (forall k, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Safe W Prog (M.arguments initial) (M.read_file initial) (state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface) k) /\
     (PublicFormation.MergeFormed W /\ PublicFormation.MentionsTyped W) /\
     exists H0, state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface H0 (iface, initial) /\
       forall n C ns types platform, A.CtxOf W (program_main Prog) C ns -> A.InitialTypes W C types ->
         program_environment Prog = Environment types platform ->
         forall f op sg, In (IFn f, PrimitiveClosure op None) platform ->
         A.nlook f (A.c_fns C) = Some sg -> Platform.AtKey W C (IFn f) op ->
         FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CallableOk W Prog (M.arguments initial) (M.read_file initial)
           (state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface) n H0
           (Environment types platform) (A.sg_params sg) (A.sg_ret sg) (PrimitiveClosure op None)) ->
  SR.entry_never_stuck prelude.
Proof.
  intros prelude state_ok Remaining Prog iface initial fuel [source [entry [bundle [Wd [_ [Elab Link]]]]]] Compat.
  destruct (Remaining Prog iface initial bundle Wd Compat Elab Link) as [Safe [[Mf Mt] [H0 [St Prims]]]].
  pose proof Link as (_ & Lay & _).
  pose proof Elab as (_ & AllFn & _ & MainDecl & (C & ns & types & platform & Ctx & Types & Env & Keys & _) & _).
  destruct Link as (_ & _ & _ & C' & ns' & types' & entries & Ctx' & _ & Env' & Entries).
  destruct (function_context_unique Wd _ C' ns' C ns Ctx' Ctx) as [-> ->].
  rewrite Env in Env'. injection Env' as <- <-.
  unfold M.runtime. rewrite Env.
  apply (main_not_stuck Wd Prog (M.arguments initial) (M.read_file initial) (state_ok Wd (program_of Prog (M.arguments initial) (M.read_file initial)) iface) fuel
    (Environment types platform) (iface, initial) H0 Safe).
  - intros dcl n rt b D Ast. exact (AllFn _ dcl RPlain n [] rt b D Ast).
  - exact MainDecl.
  - intros C0 ns0 Ctx0 F g Frame This Ret Sig.
    destruct (function_context_unique Wd _ C0 ns0 C ns Ctx0 Ctx) as [-> ->].
    apply (entry_pre Wd Prog _ _ _ fuel F C ns g types platform H0 _ Mf Mt Ctx Frame This Ret Sig St Types).
    apply (entry_realizes Wd Prog _ _ _ fuel C ns types platform H0 Lay Ctx Types Keys Entries).
    intros f op sg Ip Found At. exact (Prims fuel C ns types platform Ctx Types Env f op sg Ip Found At).
Qed.
(* The entry with the reference host's state invariant and its primitives:
   what remains is the slice's safety and the platform signatures' handle
   formation (T29). *)
Theorem entry_safe_public : forall prelude,
  (forall Prog iface initial bundle W, M.Compatible Prog iface ->
     A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
     (forall k, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Safe W Prog (M.arguments initial) (M.read_file initial)
        (PS.state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface) k) /\
     (PublicFormation.MergeFormed W /\ PublicFormation.MentionsTyped W) /\
     (forall C ns f sg op, A.CtxOf W (program_main Prog) C ns -> A.nlook f (A.c_fns C) = Some sg ->
        Platform.AtKey W C (IFn f) op -> Forall (HandleFormed W) (A.sg_ret sg :: A.sg_params sg))) ->
  SR.entry_never_stuck prelude.
Proof.
  intros prelude Remaining. apply (entry_safe prelude PS.state_ok).
  intros Prog iface initial bundle W Compat Elab Link.
  destruct (Remaining Prog iface initial bundle W Compat Elab Link) as [Safe [MM Formed]].
  split; [exact Safe|]. split; [exact MM|]. exists (fun _ => None).
  pose proof Compat as [_ [Supports Laws]].
  split; [apply PS.state_ok_initial; apply Laws|].
  pose proof Link as (_ & Lay & Reps & _).
  intros n C ns types platform Ctx Types Env f op sg Ip Found At.
  apply (entry_primitive W Prog _ _ iface C ns types platform n _ f op sg Lay Reps
    (elaborates_bool_symbols bundle W Prog Elab) (Laws _ _)); [|exact Ctx|exact Types|exact Found|exact At|
    exact (Formed C ns f sg op Ctx Found At)].
  apply Supports. unfold required_primitives. apply in_or_app. right. apply in_or_app. right.
  rewrite Env. cbn [provision_values Environment]. apply in_flat_map.
  exists (IFn f, PrimitiveClosure op None). split; [exact Ip|left; reflexivity].
Qed.

(* G05, conditionally: no Wrong result. *)
Theorem safety_public : forall prelude,
  (forall Prog iface initial bundle W, M.Compatible Prog iface ->
     A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
     (forall k, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Safe W Prog (M.arguments initial) (M.read_file initial)
        (PS.state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface) k) /\
     (PublicFormation.MergeFormed W /\ PublicFormation.MentionsTyped W) /\
     (forall C ns f sg op, A.CtxOf W (program_main Prog) C ns -> A.nlook f (A.c_fns C) = Some sg ->
        Platform.AtKey W C (IFn f) op -> Forall (HandleFormed W) (A.sg_ret sg :: A.sg_params sg))) ->
  forall program interface initial,
    M.Admissible prelude program -> M.Compatible program interface ->
    forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final).
Proof. intros prelude Remaining. exact (SR.safety_from_entry prelude (entry_safe_public prelude Remaining)). Qed.
(* The platform signatures' handles are formed (T29): a fn main assumes is
   a platform entry, whose signature reads each raw handle as itself
   (Platform.HandlesRead), and is formed, since main's context is. *)
Theorem handles_formed : forall bundle W Prog, A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
  forall C ns f sg op, A.CtxOf W (program_main Prog) C ns -> A.nlook f (A.c_fns C) = Some sg ->
    Platform.AtKey W C (IFn f) op -> Forall (HandleFormed W) (A.sg_ret sg :: A.sg_params sg).
Proof.
  intros bundle W Prog Elab Link C ns f sg op Ctx Found _.
  pose proof Elab as (Wo & _ & _ & _ & (C0 & ns0 & types & platform & Ctx0 & _ & Env & Keys & _) & _).
  destruct Link as (_ & _ & _ & C' & ns' & types' & entries & Ctx' & _ & Env' & Entries).
  destruct (function_context_unique W _ C0 ns0 C ns Ctx0 Ctx) as [-> ->].
  destruct (function_context_unique W _ C' ns' C ns Ctx' Ctx) as [-> ->].
  rewrite Env in Env'. injection Env' as <- <-.
  pose proof (PublicFormation.ctxof_cx_formed W (PublicWorldFormation.merge_formed_of W)
    (PublicWorldFormation.mentions_typed_of bundle W Wo) _ C ns Ctx) as Fc.
  pose proof (PublicFormation.fns_ok W C f sg (proj1 Fc) Found) as [Fp Fr].
  assert (Formed : Forall (PublicFormation.TyOk W) (A.sg_ret sg :: A.sg_params sg)) by (constructor; assumption).
  pose proof (ctx_keys_match W _ C ns Ctx) as KM.
  destruct (ctx_needs W _ C ns Ctx) as [_ [ProvNs _]].
  destruct (fns_ilook _ f sg KM Found) as [a Il].
  assert (Im : In (IFn f) (map fst platform))
    by (rewrite Keys; change (IFn f) with (A.rkey_of C (IFn f)); apply in_map; exact (ProvNs _ _ (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_in _ _ _ Il))).
  apply in_map_iff in Im as [[k pv] [Ek Ie]]. cbn in Ek. subst k.
  rewrite Forall_forall in Entries. pose proof (Entries _ Ie) as En. cbn in En.
  destruct pv as [w|f' df|u sms dc rms0|p held]; try contradiction. destruct held; try contradiction.
  destruct En as [_ Raw]. pose proof (Raw sg Found) as Raws.
  rewrite Forall_forall in Formed, Raws |- *. intros t It name param handle symbol args Tag Sym ->.
  split.
  - destruct (Formed _ It) as [Ft _]. exact (proj1 (proj1 (PublicFormation.nom_formed W symbol args) Ft)).
  - apply (Raws _ It name handle symbol args); [|exact Sym|reflexivity].
    cbn in Tag. destruct Tag as [E|[E|[E|[]]]]; injection E as <- <- <-; cbn; auto.
Qed.

(* ---- scoping of elaborated types (PublicScoping) ------------------------------ *)

Lemma ex_scoped_ok : forall W Prog, (exists b, A.WorldOf b W) -> FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ExScopedOk W Prog.
Proof.
  intros W Prog [b Wo] F C a x t D [Fc [Ff Cs]].
  exact (PublicScoping.ex_scoped W Prog (PublicFormation.units_bare_of W b Wo) (PublicFamilySig.family_sig_ok_readoff W)
    F C a None x t D Fc Ff Cs (PublicScoping.optscoped_none W C)).
Qed.

Lemma lit_scoped_ok : forall W, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.LitScopedOk W.
Proof. intros W F C tr x t L [_ [_ Cs]]. exact (PublicScoping.literal_scoped W C tr x t L (proj1 (proj2 Cs))). Qed.

Lemma demand_scoped_ok : forall W b, A.WorldOf b W -> FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.DemandScopedOk W.
Proof.
  intros W b Wo F C X f Cf ns th r m [Fc _] Tin Es Ctx CT RB I.
  exact (PublicScoping.demand_at_th_scoped W C X f Cf ns th r m (PublicWorldFormation.merge_formed_of W)
    (PublicWorldFormation.mentions_typed_of b W Wo) Fc Es Tin Ctx CT RB I).
Qed.

(* ---- what a functor closure captures (PublicLinkMerges) ---------------------- *)

Lemma captured_in_force_ok : forall W, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CapturedInForceOk W.
Proof.
  intros W C gg Cu nsu Cg nsg smc FCg Ks Sat k p x Ik Il Ix Ig Ou.
  exact (PublicLinkMerges.captured_mentioned W C gg Cu nsu Cg nsg smc FCg Ks Sat k p x Ik Il Ix Ig Ou).
Qed.

(* ---- table keys a path names are types (PublicFormation) --------------------- *)

Lemma family_keys_typed_ok : forall W, FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FamilyKeysTyped W.
Proof.
  intros W C u matching th sm Cr ks Ts FA Cu needs FC q pa x _ Lx Nu.
  exact (PublicFormation.family_keys_typed W C u matching th sm Cr ks Ts FA Cu needs FC x Lx Nu).
Qed.

(* ---- a family agrees with the code's context (PublicFamilyCoherent) ---------- *)

Lemma family_coherent_ok : forall W b, A.WorldOf b W -> FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.FamilyCoherentOk W.
Proof.
  intros W b Wo F C Fm C1 u matching th sm Cr ks Es Inc FA r m g a g' a' Ik Il L Eth.
  exact (PublicFamilyCoherent.family_coherent b W Wo F C Fm C1 u matching th sm Cr ks Es Inc FA r m g a g' a' Ik Il L Eth).
Qed.

(* G05, with no remaining obligation: the slice's safety follows from the
   proved obligations, with the elaboration and link facts it assumes read
   off ElaboratesIn and Linked. Applying a copy, and the slice's
   LinkResultsOk and PlatformApplyOk, follow from the obligations about a
   closure's root read through its copy chain (PublicCopyApply). Where each
   obligation is proved:
   - ChainResultsOk, from PublicChainCore.chain_results_of_new and
     PublicChainNew.chain_new_ok: the new provisions of the caller's result
     are realized (the keys are the caller's by construction,
     Audit.result_selection), proved in PublicChainRealize.chain_realize_ok;
   - ChainPlatformOk, from PublicPlatformApply.chain_platform_ok, its
     ChainForwardSigs from PublicChainFwd.chain_forward_sigs: the empty
     chain is proved; a chain with a copy is ChainForwardCopySigs, proved
     in PublicChainSigs.chain_forward_copy_sigs;
   - FamilyStepOk, from PublicFamilyStep.family_step_ok;
   - FamilyLinkResultsOk, from PublicChainCore.family_link_results_ok, its
     FamilyLinkNewOk from PublicFamilyLink.family_link_new_ok;
   - FamilyCopyRealizeOk, from PublicFamilyCopy.family_copy_realize_ok;
   - FamilyPlatformApplyOk, from PublicPlatformApply.family_platform_ok (its
     FamilyKeysTyped proved: family_keys_typed_ok);
   - FamilyCoherentOk, from family_coherent_ok. *)
Theorem safety_obligations : forall prelude program interface initial,
    M.Admissible prelude program -> M.Compatible program interface ->
    forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final).
Proof.
  intros prelude. apply (safety_public prelude).
  intros Prog iface initial bundle W Compat Elab Link.
  assert (CRz : FCp.CRz.CSg.FL.CF.CN.ChainRealizeOk W Prog (M.arguments initial) (M.read_file initial)
    (PS.state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface))
    by exact (FCp.CRz.chain_realize_ok W Prog _ _ _ (ex_intro _ bundle (proj1 Elab)) (PublicWorldFormation.merge_formed_of W)
      (PublicWorldFormation.mentions_typed_of bundle W (proj1 Elab))).

  assert (RAc : FCp.CRz.CSg.FL.CF.ChainForwardCopySigs W Prog (M.arguments initial) (M.read_file initial)
    (PS.state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface))
    by exact (FCp.CRz.CSg.chain_forward_copy_sigs W (PublicWorldFormation.merge_formed_of W)
      (PublicWorldFormation.mentions_typed_of bundle W (proj1 Elab)) Prog _ _ _).
  pose proof (family_coherent_ok W bundle (proj1 Elab)) as FCo.
  pose proof (FCp.CRz.CSg.FL.CF.app_keys_typed W) as AKT.
  pose proof (family_keys_typed_ok W) as FRA.
  pose proof (captured_in_force_ok W) as CIF.
  (* a kept output is unmentioned by the context: BApplyAbstract asks it
     (scratch/orch/chainfwd-AUDIT.md section 2) *)
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.LR.kept_outputs_unmentioned W) as KU.
  pose proof (FCp.CRz.CSg.FL.CF.chain_forward_sigs W Prog _ _ _ AKT RAc) as RA.
  pose proof (ex_scoped_ok W Prog (ex_intro _ bundle (proj1 Elab))) as ExS.
  pose proof (lit_scoped_ok W) as LitS.
  pose proof (demand_scoped_ok W bundle (proj1 Elab)) as DemS.
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.chain_results_of_new W Prog _ _ _ (FCp.CRz.CSg.FL.CF.CN.chain_new_ok W Prog _ _ _ CRz) KU) as CR.
  pose proof Elab as (Wo & AllFn & _).
  pose proof (FCp.CRz.CSg.FL.family_link_new_ok W Prog (M.arguments initial) (M.read_file initial)
    (PS.state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface)
    (ex_intro _ bundle Wo) (PublicWorldFormation.merge_formed_of W) (PublicWorldFormation.mentions_typed_of bundle W Wo)) as FLN.
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.family_link_results_ok W Prog _ _ _ FLN) as FLR.
  pose proof Link as (_ & Lay & Reps & _).
  pose proof (PublicWorldFormation.merge_formed_of W) as Mf.
  pose proof (PublicWorldFormation.mentions_typed_of bundle W Wo) as Mt.
  pose proof (PublicFamilySig.family_sig_ok_readoff W) as Fs.
  pose proof Compat as [_ [Supports Laws]].
  pose proof (platform_prims_public W Prog (M.arguments initial) (M.read_file initial) iface Lay Reps
    (elaborates_bool_symbols bundle W Prog Elab) (Laws _ _) Supports (platform_handles_formed W Prog (ex_intro _ bundle Wo))
    (platform_sigs_fixed W Prog)) as PG.
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.chain_platform_ok W Prog _ _ _ RA (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.chain_prims_of_ground W Prog _ _ _ PG)) as CP.
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.family_platform_ok W Prog _ _ _ PG FRA) as FPA.
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.link_results_of_chain W Prog _ _ _ CR) as LR.
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.copy_apply_ok W Prog _ _ _ Mf Mt Fs AllFn (ex_intro _ bundle Wo) CIF CR CP) as CA.
  pose proof (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.platform_apply_of_chain W Prog _ _ _ CP) as PA.
  pose proof (FCp.family_copy_realize_ok W Prog _ _ _ (ex_intro _ bundle Wo) Mf Mt Fs AllFn CIF LR CA PA PG) as FCA.
  assert (FSt : FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.FamilyStepOk W Prog _ _ _)
    by exact (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.family_step_ok W Prog _ _ _ Mf Mt Fs AllFn (ex_intro _ bundle Wo) CIF LR CA PA FLR FCA FPA).
  split; [|split; [split; assumption|exact (handles_formed bundle W Prog Elab Link)]].
  exact (FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.safe_all W Prog _ _ _ Lay Mf Mt Fs (elaborates_bool_symbols bundle W Prog Elab) Reps FSt FCo ExS LitS DemS AllFn
    (ex_intro _ bundle Wo) CIF LR CA PA).
Qed.

(* Audit.Public's CORRECT.safety, for every prelude, with no hypotheses. *)
Theorem public_safety : forall prelude program interface initial,
  M.Admissible prelude program ->
  M.Compatible program interface ->
  forall code final,
    ~ M.Runs program interface initial (M.Result (M.Wrong code) final).
Proof. exact safety_obligations. Qed.

End EntrySafety.
