(* The program entry (T10/T20): main's body runs under the slice's
   invariant, so run_main never reaches a Stuck signal. This file reduces
   the entry to the slice's safety (`Safe`) and main's initial realization
   (`Pre` at main's frame and context). *)
From Stdlib Require Import List String Arith Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicBodySlice
  PublicSafety PublicContextFacts PublicCallRealize PublicTagCheck PublicPrimitiveSafety PublicBoolTyping
  PublicFamilyStep.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Module EntrySafety (Host : HOST) (M0 : PUBLIC Host).
Module SR := SafetyReduction Host M0.
Module PS := PrimitiveSafety Host M0.
Module M := M0.
Module FS := FamilyStep M.ReferenceHost M.Reference.
Module S := FS.BS.

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

(* The platform's provisions realize main's context, given the machine
   types' meanings and its primitives' callability. *)
Lemma entry_realizes : forall n C ns types platform H0,
  A.CtxOf W (program_main Prog) C ns -> A.InitialTypes W C types ->
  map fst platform = map (A.rkey_of C) ns ->
  Forall (fun ie => Platform.Entry W Prog C (fst ie) (snd ie)) platform ->
  (forall f op sg, In (IFn f, PrimitiveClosure op None) platform ->
     A.nlook f (A.c_fns C) = Some sg -> Platform.AtKey W C (IFn f) op ->
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
    exact (Prims f p g (lookup_entry_some _ _ _ L) Found En).
  - intros r m g Found. destruct (mths_ilook _ r m g KM Found) as [a Il].
    destruct (Find (A.mkey r m) (ProvNs _ _ (S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn in En. contradiction.
  - intros u fx Found. pose proof (funs_ilook _ u fx KM Found) as Il.
    destruct (Find (IFun u) (ProvNs _ _ (S.ilook_in _ _ _ Il))) as [pv [L En]].
    cbn [A.rkey_of] in L, En. destruct pv as [w|f' df|u' sms dc|p held]; cbn in En; try contradiction.
    destruct sms; [|contradiction]. destruct dc as [[|? ?] [|? ?] [|? ?]]; try contradiction.
    eexists (FunctorClosure u' [] (EnvironmentWithAliases [] [] [])), (S.ground_fixed _ fx).
    split; [exact L|]. split; [apply S.CPlatform; exact En|].
    destruct (ctx_fun_fixed W _ C ns Ctx u fx (S.ilook_in _ _ _ Il)) as [Cov [_ Fun]]. split; [exact Cov|].
    intros x tx I. exact (S.lookup_ground_fixed _ fx x tx Fun I).
Qed.

(* main's body runs from its frame and context, with no locals. *)
Theorem main_not_stuck : forall fuel d0 st0 H0,
  (forall k, S.Safe W Prog argv read state_ok k) ->
  (forall dcl n rt b, A.w_decl W (program_main Prog) = Some dcl -> A.d_ast dcl = ADFn RPlain n [] rt (Some b) ->
     A.FnElab W Prog (program_main Prog)) ->
  (exists dcl n rt b, A.w_decl W (program_main Prog) = Some dcl /\ A.d_ast dcl = ADFn RPlain n [] rt (Some b)) ->
  (forall C ns, A.CtxOf W (program_main Prog) C ns -> forall F g, FrameOk F -> A.fr_this F = None ->
     A.fr_ret F = Some (A.sg_ret g) -> A.SigOf W (program_main Prog) (A.c_ty C) None g ->
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
  pose proof (Entry C ns Ctx F g Frame FThis Ret Sig) as Pre0.
  pose proof (Safe fuel) as Sf. unfold S.Safe in Sf. destruct Sf as [_ [_ [SBlkE _]]].
  pose proof (SBlkE F C b (Some (A.sg_ret g)) x t Body d0 [] H0 [] st0 Pre0) as Out.
  unfold S.O.Outcome in Out. unfold P. change (M.Reference.eval fuel (program_of Prog argv read) d0 [] st0 x) with (S.I.eval fuel (program_of Prog argv read) d0 [] st0 x).
  destruct (S.I.eval fuel (program_of Prog argv read) d0 [] st0 x) as [[] g1 st1]; cbn in Out |- *; auto.
  all: intros _; try contradiction; destruct Out as [_ [L _]]; congruence.
Qed.

(* main's initial state: its frame, no locals, the platform's provisions. *)
Lemma entry_pre : forall n F C ns g types platform H0 st0,
  PublicFormation.MergeFormed W -> PublicFormation.MentionsTyped W ->
  A.CtxOf W (program_main Prog) C ns -> FrameOk F -> A.fr_this F = None ->
  A.fr_ret F = Some (A.sg_ret g) -> A.SigOf W (program_main Prog) (A.c_ty C) None g ->
  state_ok H0 st0 -> S.Realizes W Prog argv read state_ok n C (Environment types platform) H0 ->
  S.Pre W Prog argv read state_ok n F C (Environment types platform) [] H0 [] st0.
Proof.
  intros n F C ns g types platform H0 st0 Mf Mt Ctx Frame This Ret Sig St Real.
  destruct (PublicFormation.fn_body_formed W _ C ns None g [] Mf Mt Ctx Sig (PublicFormation.optok_none W)) as [Sg Fc].
  split; [exact Frame|]. split; [split|split; [apply Forall2_nil|split; [exact St|split; [exact Real|]]]].
  3: { split; [exact Fc|]. split; [rewrite Ret; apply PublicFormation.optok_some; exact (proj2 Sg)|rewrite This; apply PublicFormation.optok_none]. }
  - intros m t mu Found. rewrite (S.ctxof_locals W _ C ns Ctx) in Found. discriminate.
  - split; [intros t E; congruence|]. apply S.alias_none; [reflexivity|exact (proj1 Real)|].
    exact (proj1 (ctxof_formed W _ C ns Ctx)).
Qed.
End Entry.

(* ---- the platform's primitives at the entry ----------------------------------- *)

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
  S.CallableOk W Prog argv read (PS.state_ok W (program_of Prog argv read) iface) n H0
    (Environment types platform) (A.sg_params sg) (A.sg_ret sg) (PrimitiveClosure op None).
Proof.
  intros W Prog argv read iface C ns types platform n H0 f op sg Lay Reps Bools Laws Supp Ctx Types Found At Formed.
  destruct At as [sg' [Found' Sig]]. rewrite Found in Found'. injection Found' as <-.
  pose proof (ctx_keys_match W _ C ns Ctx) as KM.
  destruct (fns_ilook _ f sg KM Found) as [a Il]. pose proof (S.ilook_in _ _ _ Il) as InP.
  destruct (ctx_provision_scoped W _ C ns Ctx _ _ InP) as [Sc _]. unfold provision_sig_gens in Sc. cbn in Sc.
  pose proof (ctx_fn_canonical W _ C ns f sg a Ctx InP) as Canon.
  destruct (ctxof_formed W _ C ns Ctx) as [Form _].
  set (d0 := Environment types platform).
  set (K := A.sig_gens sg).
  set (m' := (S.capture_meanings d0 K ++ A.initial_machine_subst W)%list).
  assert (OnK : forall x, In x K -> app_subst m' (TAbs x) = ground d0 (TAbs x)).
  { intros x Ix. cbn [app_subst]. unfold m'. rewrite lookup_app, (S.lookup_capture d0 K x Ix). reflexivity. }
  assert (SigK : forall t, In t (A.sg_ret sg :: A.sg_params sg) -> incl (A.gens t) K).
  { intros t It x Ix. unfold K, A.sig_gens. apply in_flat_map. exists t. split; [exact It|exact Ix]. }
  assert (G : forall t, In t (A.sg_ret sg :: A.sg_params sg) -> app_subst m' t = ground d0 t).
  { intros t It. unfold ground. apply subst_agree_on. intros x Ix. exact (OnK x (SigK t It x Ix)). }
  assert (Ground : MachinesGround W m').
  { intros name mach sym Inm Lang. destruct (in_dec Nat.eq_dec sym K) as [Ik|Ok].
    - rewrite (OnK sym Ik). unfold d0, ground. unfold A.InitialTypes in Types. subst types.
      cbn [type_meanings Environment app_subst]. rewrite S.lookup_map_snd, lookup_subst_nlook.
      assert (Ikey : In sym (map fst (A.c_ty C))) by exact (Sc sym Ik).
      destruct (keys_in (A.c_ty C) sym Ikey) as [v Lv]. rewrite Lv. cbn [option_map].
      unfold K, A.sig_gens in Ik. apply in_flat_map in Ik as [t [It Ix]].
      pose proof (subst_fixed_gens t (A.c_sub C) (Canon t It) sym Ix) as Fix.
      rewrite (Form sym v Lv), Fix. cbn [app_subst]. rewrite (machine_lookup W name mach sym Lay Inm Lang). reflexivity.
    - cbn [app_subst]. unfold m'. rewrite lookup_app.
      assert (Out : lookup_subst sym (S.capture_meanings d0 K) = None).
      { unfold S.capture_meanings. clear -Ok. induction K as [|y r IH]; [reflexivity|]. cbn.
        destruct (Nat.eqb_spec sym y) as [->|_]; [exfalso; apply Ok; left; reflexivity|].
        apply IH. intro I; apply Ok; right; exact I. }
      rewrite Out, (machine_lookup W name mach sym Lay Inm Lang). reflexivity. }
  cbn [S.CallableOk]. intros H st ws Ext St Args.
  rewrite <- (G (A.sg_ret sg) (or_introl eq_refl)).
  apply (PS.primitive_safe W Prog argv read iface op None (A.sg_params sg) (A.sg_ret sg) m' H st ws
    Lay Reps Bools Laws Supp Sig Ground Formed I St).
  clear -Args G. induction Args as [|w t ws ts T Rest IH]; constructor.
  - rewrite G; [exact T|right; left; reflexivity].
  - apply IH. intros t' It'. apply G. destruct It' as [<-|It']; [left; reflexivity|right; right; exact It'].
Qed.

(* T20: an admissible program never reaches a Stuck entry signal, given the
   slice's safety, an initial ghost heap the state realizes, and callable
   platform primitives. *)
Theorem entry_safe : forall prelude
    (state_ok : A.world -> program_view -> M.interface -> heap_types -> M.ReferenceHost.mstate -> Prop),
  (forall Prog iface initial bundle W, M.Compatible Prog iface ->
     A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
     (forall k, S.Safe W Prog (M.arguments initial) (M.read_file initial) (state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface) k) /\
     (PublicFormation.MergeFormed W /\ PublicFormation.MentionsTyped W) /\
     exists H0, state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface H0 (iface, initial) /\
       forall n C ns types platform, A.CtxOf W (program_main Prog) C ns -> A.InitialTypes W C types ->
         program_environment Prog = Environment types platform ->
         forall f op sg, In (IFn f, PrimitiveClosure op None) platform ->
         A.nlook f (A.c_fns C) = Some sg -> Platform.AtKey W C (IFn f) op ->
         S.CallableOk W Prog (M.arguments initial) (M.read_file initial)
           (state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface) n H0
           (Environment types platform) (A.sg_params sg) (A.sg_ret sg) (PrimitiveClosure op None)) ->
  SR.entry_never_stuck prelude.
Proof.
  intros prelude state_ok Remaining Prog iface initial fuel [source [entry [bundle [Wd [_ [Elab Link]]]]]] Compat.
  destruct (Remaining Prog iface initial bundle Wd Compat Elab Link) as [Safe [[Mf Mt] [H0 [St Prims]]]].
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
    apply (entry_pre Wd Prog _ _ _ fuel F C ns g types platform H0 _ Mf Mt Ctx Frame This Ret Sig St).
    apply (entry_realizes Wd Prog _ _ _ fuel C ns types platform H0 Ctx Types Keys Entries).
    intros f op sg Ip Found At. exact (Prims fuel C ns types platform Ctx Types Env f op sg Ip Found At).
Qed.
(* The entry with the reference host's state invariant and its primitives:
   what remains is the slice's safety and the platform signatures' handle
   formation (T29). *)
Theorem entry_safe_public : forall prelude,
  (forall Prog iface initial bundle W, M.Compatible Prog iface ->
     A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
     (forall k, S.Safe W Prog (M.arguments initial) (M.read_file initial)
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
     (forall k, S.Safe W Prog (M.arguments initial) (M.read_file initial)
        (PS.state_ok W (program_of Prog (M.arguments initial) (M.read_file initial)) iface) k) /\
     (PublicFormation.MergeFormed W /\ PublicFormation.MentionsTyped W) /\
     (forall C ns f sg op, A.CtxOf W (program_main Prog) C ns -> A.nlook f (A.c_fns C) = Some sg ->
        Platform.AtKey W C (IFn f) op -> Forall (HandleFormed W) (A.sg_ret sg :: A.sg_params sg))) ->
  forall program interface initial,
    M.Admissible prelude program -> M.Compatible program interface ->
    forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final).
Proof. intros prelude Remaining. exact (SR.safety_from_entry prelude (entry_safe_public prelude Remaining)). Qed.
(* G05 from the remaining obligations alone: the slice's safety follows
   from them, with the elaboration and link facts it assumes read off
   ElaboratesIn and Linked. *)
Theorem safety_obligations : forall prelude,
  (forall Prog iface initial bundle W, M.Compatible Prog iface ->
     A.ElaboratesIn bundle W Prog -> Platform.Linked W Prog ->
     let argv := M.arguments initial in let read := M.read_file initial in
     let so := PS.state_ok W (program_of Prog argv read) iface in
     (PublicFormation.MergeFormed W /\ PublicFormation.MentionsTyped W /\ PublicFormation.FamilySigOk W) /\
     (forall C ns f sg op, A.CtxOf W (program_main Prog) C ns -> A.nlook f (A.c_fns C) = Some sg ->
        Platform.AtKey W C (IFn f) op -> Forall (HandleFormed W) (A.sg_ret sg :: A.sg_params sg)) /\
     S.LinkMergesOk W Prog /\ S.BindMergesOk W Prog /\ S.CapturedInForceOk /\
     S.RefileRunOk W Prog argv read so /\ S.LinkResultsOk W Prog argv read so /\
     S.CopyApplyOk W Prog argv read so /\ S.PlatformApplyOk W Prog argv read so /\
     (* FamilyStepOk, from PublicFamilyStep.family_step_ok *)
     FS.FamilyLinkResultsOk W Prog argv read so /\ FS.FamilyCopyApplyOk W Prog argv read so /\
     FS.FamilyPlatformApplyOk W Prog argv read so /\
     (* a family's receiver is in scope, and agrees with the code's context *)
     S.FamilyCoherentOk W /\ S.ExScopedOk W Prog /\ S.LitScopedOk W /\ S.DemandScopedOk W) ->
  forall program interface initial,
    M.Admissible prelude program -> M.Compatible program interface ->
    forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final).
Proof.
  intros prelude Obl. apply (safety_public prelude).
  intros Prog iface initial bundle W Compat Elab Link.
  destruct (Obl Prog iface initial bundle W Compat Elab Link)
    as [[Mf [Mt Fs]] [Handles [LM [BM [CIF [RR [LR [CA [PA [FLR [FCA [FPA [FCo [ExS [LitS DemS]]]]]]]]]]]]]]].
  pose proof Elab as (Wo & AllFn & _).
  pose proof Link as (_ & Lay & Reps & _).
  assert (FSt : S.FamilyStepOk W Prog _ _ _)
    by exact (FS.family_step_ok W Prog _ _ _ Mf Mt Fs AllFn (ex_intro _ bundle Wo) LM BM CIF RR LR CA PA FLR FCA FPA).
  split; [|split; [split; assumption|exact Handles]].
  exact (S.safe_all W Prog _ _ _ Lay Mf Mt Fs (elaborates_bool_symbols bundle W Prog Elab) Reps FSt FCo ExS LitS DemS AllFn
    (ex_intro _ bundle Wo) LM BM CIF RR LR CA PA).
Qed.

End EntrySafety.
