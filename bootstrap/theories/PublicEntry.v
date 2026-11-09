(* The program entry (T10/T20): main's body runs under the slice's
   invariant, so run_main never reaches a Stuck signal. This file reduces
   the entry to the slice's safety (`Safe`) and main's initial realization
   (`Pre` at main's frame and context). *)
From Stdlib Require Import List String Arith Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicBodySlice
  PublicSafety PublicContextFacts PublicCallRealize PublicTagCheck.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Module EntrySafety (Host : HOST).
Module SR := SafetyReduction Host.
Module M := SR.M.
Module S := BodySlice M.ReferenceHost M.Reference.

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
  S.Absorbs C (Environment (map (fun q => (fst q, app_subst ms (snd q))) (A.c_ty C)) platform).
Proof.
  intros f C ns ms platform Ctx t. unfold ground, A.canon. cbn [type_meanings Environment].
  destruct (ctxof_formed W f C ns Ctx) as [Form Sub].
  pose proof (function_context_canonical W f C ns Ctx) as Idem.
  set (types := map (fun q => (fst q, app_subst ms (snd q))) (A.c_ty C)).
  assert (Lk : forall y, lookup_subst y types = option_map (app_subst ms) (A.nlook y (A.c_ty C))).
  { intros y. unfold types. rewrite S.lookup_map_snd, lookup_subst_nlook. reflexivity. }
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

(* The platform's provisions realize main's context, given the machine
   types' meanings and its primitives' callability. *)
Lemma entry_realizes : forall n C ns types platform H0,
  A.CtxOf W (program_main Prog) C ns -> A.InitialTypes W C types ->
  map fst platform = map (A.rkey_of C) ns ->
  Forall (fun ie => Platform.Entry W Prog C (fst ie) (snd ie)) platform ->
  (forall f op sg, A.nlook f (A.c_fns C) = Some sg -> Platform.AtKey W C (IFn f) op ->
     S.CallableOk W Prog argv read state_ok n H0 (Environment types platform) (A.sg_params sg) (A.sg_ret sg)
       (PrimitiveClosure op None)) ->
  S.Realizes W Prog argv read state_ok n C (Environment types platform) H0.
Proof.
  intros n C ns types platform H0 Ctx Types Keys Entries Prims.
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
    destruct (Find (IVal v) (ProvNs _ _ (S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn [A.rkey_of] in L, En. destruct pv as [w|f' df|u sms dc|p held]; cbn in En; try contradiction.
    destruct w; cbn in En; try contradiction. destruct En as [t' [Lt Mt]].
    rewrite Found in Lt. injection Lt as <-. exists (VI32 i). split; [exact L|].
    destruct Mt as [sym [Lang [_ ->]]].
    pose proof (ctx_val_canonical _ C ns v _ a Ctx (S.ilook_in _ _ _ Il)) as Canon.
    destruct (ctx_provision_scoped W _ C ns Ctx _ _ (S.ilook_in _ _ _ Il)) as [Sc _].
    assert (Isym : In sym (map fst (A.c_ty C))) by (apply Sc; cbn; left; reflexivity).
    destruct (ctxof_formed W _ C ns Ctx) as [Form _].
    destruct (keys_in (A.c_ty C) sym Isym) as [vs Ls]. pose proof (Form sym vs Ls) as Ev.
    change (app_subst (A.c_sub C) (TAbs sym)) with (A.canon C (TAbs sym)) in Ev. rewrite Canon in Ev. subst vs.
    unfold ground. cbn [type_meanings Environment app_subst].
    rewrite S.lookup_map_snd, lookup_subst_nlook, Ls. cbn [option_map app_subst].
    rewrite (i32_machine sym Lang). constructor.
  - intros f g Found. destruct (fns_ilook _ f g KM Found) as [a Il].
    destruct (Find (IFn f) (ProvNs _ _ (S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn [A.rkey_of] in L, En. destruct pv as [w|f' df|u sms dc|p held]; cbn in En; try contradiction.
    destruct held; cbn in En; try contradiction. exists (PrimitiveClosure p None). split; [exact L|].
    exact (Prims f p g Found En).
  - intros r m g Found. destruct (mths_ilook _ r m g KM Found) as [a Il].
    destruct (Find (A.mkey r m) (ProvNs _ _ (S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn in En. contradiction.
  - intros u fx Found. pose proof (funs_ilook _ u fx KM Found) as Il.
    destruct (Find (IFun u) (ProvNs _ _ (S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn [A.rkey_of] in L, En. destruct pv as [w|f' df|u' sms dc|p held]; cbn in En; try contradiction.
    exists u', sms, dc. exact L.
Qed.

(* main's body runs from its frame and context, with no locals. *)
Theorem main_not_stuck : forall fuel d0 st0 H0,
  (forall k, S.Safe W Prog argv read state_ok k) ->
  (forall dcl n rt b, A.w_decl W (program_main Prog) = Some dcl -> A.d_ast dcl = ADFn RPlain n [] rt (Some b) ->
     A.FnElab W Prog (program_main Prog)) ->
  (exists dcl n rt b, A.w_decl W (program_main Prog) = Some dcl /\ A.d_ast dcl = ADFn RPlain n [] rt (Some b)) ->
  (forall C ns, A.CtxOf W (program_main Prog) C ns -> forall F, FrameOk F -> A.fr_this F = None ->
     S.Pre W Prog argv read state_ok fuel F C d0 [] H0 [] st0) ->
  ~ SR.stuck_signal (M.Reference.evaluation_signal (M.Reference.run_main fuel P (program_main Prog) d0 st0)).
Proof.
  intros fuel d0 st0 H0 Safe Elab [dcl [n [rt [b [Decl Ast]]]]] Entry.
  destruct (Elab dcl n rt b Decl Ast dcl RPlain n [] rt b Decl Ast)
    as (C & ns & this & g & F & x & t & Ctx & This & Sig & _ & Ret & FThis & Loop & _ & _ & _ & Num & Body & View).
  cbn in This. subst this.
  unfold M.Reference.run_main. change (view_function P (program_main Prog)) with (A.body_of Prog (program_main Prog)).
  rewrite View.
  assert (Frame : FrameOk F) by (apply (numbering_frame F []); rewrite FThis; exact Num).
  pose proof (Entry C ns Ctx F Frame FThis) as Pre0.
  pose proof (Safe fuel) as Sf. unfold S.Safe in Sf. destruct Sf as [_ [_ [SBlkE _]]].
  pose proof (SBlkE F C b (Some (A.sg_ret g)) x t Body d0 [] H0 [] st0 Pre0) as Out.
  unfold S.O.Outcome in Out. unfold P. change (M.Reference.eval fuel (program_of Prog argv read) d0 [] st0 x) with (S.I.eval fuel (program_of Prog argv read) d0 [] st0 x).
  destruct (S.I.eval fuel (program_of Prog argv read) d0 [] st0 x) as [[] g1 st1]; cbn in Out |- *; auto.
  all: intros _; try contradiction; destruct Out as [_ [L _]]; congruence.
Qed.

(* main's initial state: its frame, no locals, the platform's provisions. *)
Lemma entry_pre : forall n F C ns types platform H0 st0,
  A.CtxOf W (program_main Prog) C ns -> FrameOk F -> A.fr_this F = None ->
  state_ok H0 st0 -> S.Realizes W Prog argv read state_ok n C (Environment types platform) H0 ->
  S.Pre W Prog argv read state_ok n F C (Environment types platform) [] H0 [] st0.
Proof.
  intros n F C ns types platform H0 st0 Ctx Frame This St Real.
  split; [exact Frame|]. split; [split|split; [constructor|split; [exact St|exact Real]]].
  - intros m t mu Found. rewrite (S.ctxof_locals W _ C ns Ctx) in Found. discriminate.
  - intros t E. congruence.
Qed.
End Entry.

(* T20: an admissible program never reaches a Stuck entry signal, given the
   slice's safety, an initial ghost heap the state realizes, and callable
   platform primitives. *)
Theorem entry_safe : forall prelude (state_ok : M.interface -> heap_types -> M.ReferenceHost.mstate -> Prop),
  (forall Prog iface initial bundle W, M.Compatible Prog iface ->
     A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
     (forall k, S.Safe W Prog (M.arguments initial) (M.read_file initial) (state_ok iface) k) /\
     exists H0, state_ok iface H0 (iface, initial) /\
       forall n C f op sg d0, A.nlook f (A.c_fns C) = Some sg -> Platform.AtKey W C (IFn f) op ->
         S.CallableOk W Prog (M.arguments initial) (M.read_file initial) (state_ok iface) n H0 d0
           (A.sg_params sg) (A.sg_ret sg) (PrimitiveClosure op None)) ->
  SR.entry_never_stuck prelude.
Proof.
  intros prelude state_ok Remaining Prog iface initial fuel [source [entry [bundle [Wd [_ [Elab Link]]]]]] Compat.
  destruct (Remaining Prog iface initial bundle Wd Compat Elab Link) as [Safe [H0 [St Prims]]].
  pose proof Elab as (_ & AllFn & _ & MainDecl & (C & ns & types & platform & Ctx & Types & Env & Keys & _) & _).
  destruct Link as (_ & _ & _ & C' & ns' & types' & entries & Ctx' & _ & Env' & Entries).
  destruct (function_context_unique Wd _ C' ns' C ns Ctx' Ctx) as [-> ->].
  rewrite Env in Env'. injection Env' as <- <-.
  unfold M.runtime. rewrite Env.
  apply (main_not_stuck Wd Prog (M.arguments initial) (M.read_file initial) (state_ok iface) fuel
    (Environment types platform) (iface, initial) H0 Safe).
  - intros dcl n rt b D Ast. exact (AllFn _ dcl RPlain n [] rt b D Ast).
  - exact MainDecl.
  - intros C0 ns0 Ctx0 F Frame This.
    destruct (function_context_unique Wd _ C0 ns0 C ns Ctx0 Ctx) as [-> ->].
    apply (entry_pre Wd Prog _ _ _ fuel F C ns types platform H0 _ Ctx Frame This St).
    apply (entry_realizes Wd Prog _ _ _ fuel C ns types platform H0 Ctx Types Keys Entries).
    intros f op sg Found At. exact (Prims fuel C f op sg _ Found At).
Qed.
End EntrySafety.
