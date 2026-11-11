(* ------------------------------------------------------------------------ *)
(* ProgramFastPath.v — applying a platform family keeping only the members    *)
(* used, for Audit's programs (FastPath.v's pass, ported from XIR).          *)
(*                                                                           *)
(* `bind F;` (BApply F θ [] [] rs) runs F's whole body and installs every   *)
(* member of its result at every application, though the code after it     *)
(* reads one or two members: for the platform's `Memories` (27 members),   *)
(* at every memory access.  For a functor F whose entry in main's initial  *)
(* environment is the platform's closure of a body H made of primitives    *)
(* only (no Box/ArrNew, so Base ignores their sites), every application   *)
(* of F installs, under each caller key r of the selection                 *)
(* Q = rev (combine rs (fc_sources H)) (Audit's result_selection: the last *)
(* position wins), `PrimitiveClosure p site` for the primitive p the body  *)
(* binds at the matching source. The check preserves sites that Base uses. *)
(* so replacing the application by BPrimitive bindings keeps behavior:     *)
(*   - `EBind (BApply F θ [] [] rs) e` becomes a chain of EBind/BPrimitive *)
(*     for the pairs of Q whose requirement `e` may look up (`uses e`; if  *)
(*     none, the first pair, so that the chain is never empty);            *)
(*   - in a functor body, `BApply F θ [] [] rs` becomes the BPrimitive     *)
(*     bindings of all of Q.                                                *)
(* The syntactic check (`ok_program`) makes F's entry the platform's       *)
(* wherever it is found: F is never bound, copied onto, keyed by a         *)
(* primitive or a result, and every satisfaction map moves F's entry only  *)
(* to F's key.  This is the same transformation as the old audit-run       *)
(* translation of FastPath's XIR branches (tr.ml, branch_prims, deleted     *)
(* with the old oracle), done on Audit programs and proved.                 *)
(*                                                                           *)
(* Fuel.  Unlike pruning, the transformed program does not use the same    *)
(* fuel: a chain of m bindings costs m levels on its continuation, while   *)
(* the application costs |H|+3 levels on a side branch.  So the simulation *)
(* is stated with an "eventually" fuel on the other side, in both          *)
(* directions:                                                               *)
(* - Sim.forward: if the original run at fuel k is not Stuck or out of     *)
(*   fuel, the transformed run is equal to it at every large enough fuel;  *)
(* - Sim.backward: if the transformed run at fuel k is not out of fuel,    *)
(*   the original run is eventually Stuck, or eventually equal to it.      *)
(* FastRuns.runs_fast: for a program that never runs to Wrong (what        *)
(* public_safety gives), Runs of the transformed program iff Runs of the   *)
(* original, for code-blind interfaces (as ProgramPrune.PruneRuns).          *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import BaseFacts.
From NM Require Import AuditFacts PublicPrimitiveSites.
From NM Require Import Audit PublicTypeFacts PublicValueTyping.
From NM Require PublicCopySubst.
From NM Require ProgramPrune.
From Stdlib Require PrimInt63.
Import AuditBase.Semantics ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

Notation item_eqb_true := ProgramPrune.item_eqb_true.
Notation lookup_entry_app := ProgramPrune.lookup_entry_app.
Notation lookup_entry_filter := ProgramPrune.lookup_entry_filter.

(* ===================== 1. What an expression looks up ==================== *)

Definition srcs (sm : smap) : list item := map snd sm.

(* Every item `e` may look up in the environment it starts in (an
   over-approximation, as FastPath.uses). *)
Fixpoint uses (e : expr) : list item :=
  match e with
  | EVar _ | EUnit | EBreak => []
  | EVal v => [IVal v]
  | ETag _ _ e1 | EAssign _ e1 | ELoop e1 | EReturn e1 | EField e1 _ => uses e1
  | ECall _ _ sm args => srcs sm ++ flat_map uses args
  | EInvoke f args => IFn f :: flat_map uses args
  | EMethod e0 _ it args => it :: uses e0 ++ flat_map uses args
  | ELet _ a b | ESeq a b | EWhile a b => uses a ++ uses b
  | EBind b e1 => buses b ++ uses e1
  | EIf a b c => uses a ++ uses b ++ uses c
  | ERecord fs => flat_map (fun p => uses (snd p)) fs
  | EMatch e0 arms => uses e0 ++ flat_map (fun p => uses (snd p)) arms
  end
with buses (b : binding) : list item :=
  match b with
  | BType _ _ | BPrimitive _ _ => []
  | BValue _ e1 => uses e1
  | BFunction _ _ _ sm | BMethod _ _ _ _ _ sm => srcs sm
  | BFunctor _ _ _ _ smc => srcs smc
  | BCopyFunctor _ u' _ _ => [IFun u']
  | BApply u _ _ sm _ => IFun u :: srcs sm
  end.

Definition inW (W : list item) (i : item) : bool := existsb (item_eqb i) W.

(* ===================== 2. The pass, for one functor ====================== *)

Definition prim_ok (p : op) : bool :=
  match decode p with Some (PBox _) | Some (PArrNew _) => false | _ => true end.
Definition body_prim (b : binding) : bool := match b with BPrimitive _ p => prim_ok p | _ => false end.
Definition binds (s : item) (b : binding) : bool :=
  match b with BPrimitive j _ => item_eqb j s | _ => false end.
(* Canonical entry used to select an operation; runtime_bent records its site. *)
Definition bent (b : binding) : item * provision_value :=
  match b with BPrimitive i p => (i, PrimitiveClosure p None) | _ => (ITy 0, ValueProvision VUnit) end.

Definition osm_ok (f : AuditBase.Semantics.smap -> bool) (o : option AuditBase.Semantics.smap) : bool :=
  match o with Some sm => f sm | None => true end.

Section Pass.
(* F's symbol, and the code of the body H its root entry closes *)
Variable U : usym.
Variable cH : functor_code.

Definition isU (i : item) : bool := item_eqb i (IFun U).
(* A satisfaction map never moves F's entry to another key, nor another
   entry to F's. *)
Definition sm_ok (sm : smap) : bool := forallb (fun p => Bool.eqb (isU (fst p)) (isU (snd p))) sm.
Lemma smap_source_isU : forall sm s x, sm_ok sm = true -> smap_source s sm = Some x -> isU x = isU s.
Proof.
  induction sm as [|[r t] rest IH]; intros s x Hok Hs; [discriminate|].
  cbn [smap_source] in Hs. unfold sm_ok in Hok. cbn [forallb] in Hok.
  apply andb_true_iff in Hok as [Hp Hrest].
  destruct (item_eqb s r) eqn:E.
  - injection Hs as <-. apply ProgramPrune.item_eqb_true in E. subst r.
    apply Bool.eqb_prop in Hp. cbn in Hp. symmetry. exact Hp.
  - exact (IH s x Hrest Hs).
Qed.
Lemma osm_ok_compose : forall a sm, osm_ok sm_ok a = true -> sm_ok sm = true ->
  osm_ok sm_ok (compose_adaptation a sm) = true.
Proof.
  intros [c|] sm Ha Hsm; cbn in *; [|exact Hsm].
  pose proof Hsm as Hsm0.
  unfold sm_ok in Ha |- *. rewrite forallb_forall in *. intros [r t] Hin.
  apply in_map_iff in Hin as [[r0 s0] [Eq Hin]]. cbn in Eq. injection Eq as <- <-.
  pose proof (Ha _ Hin) as H0. cbn in H0 |- *.
  destruct (smap_source s0 sm) as [x|] eqn:Es; [|exact H0].
  rewrite (@smap_source_isU sm s0 x Hsm0 Es). exact H0.
Qed.

Definition rs_ok (rs : list item) : bool := forallb (fun i => negb (isU i)) rs.

Fixpoint ok_e (e : expr) : bool :=
  match e with
  | EVar _ | EUnit | EBreak | EVal _ => true
  | ETag _ _ e1 | EAssign _ e1 | ELoop e1 | EReturn e1 | EField e1 _ => ok_e e1
  | ECall _ _ sm args => sm_ok sm && forallb ok_e args
  | EInvoke _ args => forallb ok_e args
  | EMethod e0 _ _ args => ok_e e0 && forallb ok_e args
  | ELet _ a b | ESeq a b | EWhile a b => ok_e a && ok_e b
  | EBind b e1 => ok_b b && ok_e e1
  | EIf a b c => ok_e a && ok_e b && ok_e c
  | ERecord fs => forallb (fun p => ok_e (snd p)) fs
  | EMatch e0 arms => ok_e e0 && forallb (fun p => ok_e (snd p)) arms
  end
with ok_b (b : binding) : bool :=
  match b with
  | BType _ _ => true
  | BValue _ e1 => ok_e e1
  | BFunction _ _ _ sm | BMethod _ _ _ _ _ sm => sm_ok sm
  | BFunctor u _ _ sm smc => negb (Nat.eqb u U) && sm_ok sm && sm_ok smc
  | BCopyFunctor u _ sm _ => negb (Nat.eqb u U) && sm_ok sm
  | BApply _ _ _ sm rs => sm_ok sm && rs_ok rs
  | BPrimitive i _ => negb (isU i)
  end.

(* An entry found under F's key is the platform's closure of H. *)
Variable H : usym.
Definition kok (i : item) (a : provision_value) : bool :=
  if isU i then match a with FunctorClosure h None _ None => Nat.eqb h H | _ => false end else true.
(* Main's initial environment: every entry, at every depth, is kok, and
   every functor closure's adaptations are sm_ok. *)
Fixpoint pv_ok (a : provision_value) : bool :=
  match a with
  | ValueProvision _ | PrimitiveClosure _ _ => true
  | FunctionClosure _ d =>
      match d with EnvironmentWithAliases _ _ ps =>
        (fix go (l : list (item * provision_value)) : bool :=
           match l with [] => true | (i, x) :: r => kok i x && pv_ok x && go r end) ps
      end
  | FunctorClosure _ sms d _ =>
      osm_ok sm_ok sms &&
      match d with EnvironmentWithAliases _ _ ps =>
        (fix go (l : list (item * provision_value)) : bool :=
           match l with [] => true | (i, x) :: r => kok i x && pv_ok x && go r end) ps
      end
  end.
Definition env_ok (d : environment) : bool :=
  forallb (fun q => kok (fst q) (snd q) && pv_ok (snd q)) (provision_values d).

(* H's body: nonallocating primitives, none at F's key, and every
   source of its result bound by one of them. *)
Definition body_ok : bool :=
  forallb body_prim (fc_bindings cH)
  && forallb (fun s => existsb (binds s) (fc_bindings cH)) (fc_sources cH).

(* What H's body binds at s: its last primitive binding of s. *)
Definition hent (s : item) : provision_value :=
  match lookup_entry s (rev (map bent (fc_bindings cH))) with
  | Some a => a | None => ValueProvision VUnit end.
Definition pick (s : item) : op :=
  match hent s with PrimitiveClosure p _ => p | _ => encode PUnreachable end.
(* What an application installs for the pair q. *)
Definition qent (q : item * item) : item * provision_value :=
  (fst q, PrimitiveClosure (pick (snd q)) None).

(* An application's selection (result_selection with no copies). *)
Definition sel (rs : list item) : list (item * item) := rev (combine rs (fc_sources cH)).
(* The bindings that install the pairs of Q: the first pair last. *)
Definition prims_of (Q : list (item * item)) : list binding :=
  rev (map (fun q => BPrimitive (fst q) (pick (snd q))) Q).
Definition chain (bs : list binding) (e : expr) : expr := fold_right EBind e bs.

(* Which pairs to keep before a continuation that looks up W: those it may
   read; if none, the first. *)
Definition keepf (W : list item) (Q : list (item * item)) : item -> bool :=
  if existsb (fun q => inW W (fst q)) Q then inW W
  else match Q with q :: _ => item_eqb (fst q) | [] => fun _ => false end.

Definition supplies_type (th : subst) (symbol : tsym) : bool :=
  existsb (Nat.eqb symbol) (fc_inputs cH) &&
  existsb (Nat.eqb symbol) (fc_linked cH) &&
  match lookup_subst symbol th with Some _ => true | None => false end.

(* Inlining installs the primitive at the caller's key. For a primitive
   that uses its receiver type, check that this key gives the same type
   as the source key in the functor application. *)
Definition preserves_site (th : subst) (q : item * item) : bool :=
  match decode (pick (snd q)) with
  | Some primitive =>
      if BaseCall.uses_site primitive then
        match fst q, snd q, fc_table cH with
        | IMth requested _ _, IMth provided _ _, [] =>
            forallb (supplies_type th) (AuditBase.Semantics.Elaboration.gens provided) &&
            ty_eqb (app_subst th provided) requested
        | _, _, _ => false
        end
      else true
  | None => true
  end.

Definition fast_apply (b : binding) : option (list (item * item)) :=
  match b with
  | BApply u th [] [] rs =>
      if Nat.eqb u U then
        match sel rs with
        | [] => None
        | Q => if forallb (preserves_site th) Q then Some Q else None
        end
      else None
  | _ => None
  end.

Fixpoint fp_e (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVal v => EVal v
  | ETag n th e1 => ETag n th (fp_e e1)
  | ECall f th sm args => ECall f th sm (map fp_e args)
  | EInvoke f args => EInvoke f (map fp_e args)
  | EMethod e0 m it args => EMethod (fp_e e0) m it (map fp_e args)
  | ELet x e1 e2 => ELet x (fp_e e1) (fp_e e2)
  | EBind b e1 =>
      match fast_apply b with
      | Some Q =>
          (* computed once per application, in the extracted code *)
          let kp := keepf (uses e1) Q in
          chain (prims_of (filter (fun q => kp (fst q)) Q)) (fp_e e1)
      | None => EBind (fp_b b) (fp_e e1)
      end
  | ESeq e1 e2 => ESeq (fp_e e1) (fp_e e2)
  | EAssign x e1 => EAssign x (fp_e e1)
  | EIf c e1 e2 => EIf (fp_e c) (fp_e e1) (fp_e e2)
  | EWhile c b => EWhile (fp_e c) (fp_e b)
  | ELoop b => ELoop (fp_e b)
  | EReturn e1 => EReturn (fp_e e1)
  | EBreak => EBreak
  | ERecord fs => ERecord (map (fun p => (fst p, fp_e (snd p))) fs)
  | EField e1 f => EField (fp_e e1) f
  | EMatch e0 arms => EMatch (fp_e e0) (map (fun p => (fst p, fp_e (snd p))) arms)
  end
with fp_b (b : binding) : binding :=
  match b with
  | BValue v e1 => BValue v (fp_e e1)
  | b => b
  end.

Definition fp_bs (bs : list binding) : list binding :=
  flat_map (fun b => match fast_apply b with Some Q => prims_of Q | None => [fp_b b] end) bs.

Definition fp_code (c : functor_code) : functor_code :=
  FunctorCode (fc_inputs c) (fc_linked c) (fc_table c) (fc_outputs c)
    (fp_bs (fc_bindings c)) (fc_sources c).

End Pass.

Definition ok_program (U H : usym) (p : program) : bool :=
  match Elaboration.nlook H (program_functors p) with
  | Some c =>
      body_ok c
      && forallb (fun fb => ok_e U (snd fb)) (program_functions p)
      && forallb (fun ub => forallb (ok_b U) (fc_bindings (snd ub))) (program_functors p)
      && env_ok U H (program_environment p)
  | None => false
  end.

Definition fp_program (U : usym) (c : functor_code) (p : program) : program :=
  Program (map (fun fb => (fst fb, fp_e U c (snd fb))) (program_functions p))
          (map (fun ub => (fst ub, fp_code U c (snd ub))) (program_functors p))
          (program_bool p) (program_true p) (program_false p) (program_main p)
          (program_environment p) (program_unsupported p) (program_casts p) (program_machines p) (program_origins p).

Definition fast_one (U H : usym) (p : program) : program :=
  match Elaboration.nlook H (program_functors p) with
  | Some c => if ok_program U H p then fp_program U c p else p
  | None => p
  end.

(* The candidates: main's root entries for platform functors. *)
Definition candidates (p : program) : list (usym * usym) :=
  flat_map (fun ie => match ie with
                      | (IFun u, FunctorClosure h None _ None) => [(u, h)]
                      | _ => []
                      end) (provision_values (program_environment p)).

(* The pass: each candidate the check accepts, in turn. *)
Definition fastpath (p : program) : program :=
  fold_left (fun p uh => fast_one (fst uh) (snd uh) p) (candidates p) p.

Definition site_qent (cH : functor_code) (d : environment) (source : bool) (q : item * item) :=
  (fst q, primitive_closure d (if source then snd q else fst q) (pick cH (snd q))).

(* ===================== 3. Agreement ===================================== *)

Lemma lookup_subst_input : forall symbol inputs (read : tsym -> ty),
  In symbol inputs ->
  lookup_subst symbol (map (fun input => (input, read input)) inputs) = Some (read symbol).
Proof.
  intros symbol inputs read. induction inputs as [|input rest IH]; intros InInputs; [contradiction|].
  cbn [map lookup_subst]. destruct (Nat.eqb_spec symbol input) as [<-|Different]; [reflexivity|].
  apply IH. destruct InInputs; congruence.
Qed.

Lemma supplied_site_type : forall P h c caller captured th source,
  view_functor P h = Some c -> fc_table c = [] ->
  forallb (supplies_type c th) (AuditBase.Semantics.Elaboration.gens source) = true ->
  ground (application_environment P caller h th captured) source =
  ground caller (app_subst th source).
Proof.
  intros P h c caller captured th source Code Table Supplied.
  unfold ground. symmetry. apply PublicCopySubst.ty_agree.
  intros symbol Used. apply forallb_forall with (x := symbol) in Supplied; [|exact Used].
  unfold supplies_type in Supplied. apply andb_true_iff in Supplied as [Domains Supply].
  apply andb_true_iff in Domains as [Inputs Linked].
  apply existsb_exists in Inputs as [input [Member Equal]].
  apply Nat.eqb_eq in Equal. subst input.
  destruct (lookup_subst symbol th) as [argument|] eqn:Argument; [|discriminate].
  cbn [app_subst]. rewrite Argument.
  unfold application_environment. rewrite Code, Table. cbn [type_meanings lookup_subst].
  rewrite lookup_subst_input by exact Member.
  now rewrite Linked, Argument.
Qed.

Lemma retained_site_qent : forall P h c caller next captured th q,
  view_functor P h = Some c ->
  type_meanings caller = type_meanings next ->
  preserves_site c th q = true ->
  let operation := pick c (snd q) in
  let site (entry : provision_value) :=
    match entry with PrimitiveClosure _ site => BaseSites.retained_site operation site | _ => None end in
  site (snd (site_qent c (application_environment P caller h th captured) true q)) =
  site (snd (site_qent c next false q)).
Proof.
  intros P h c caller next captured th [requested provided] Code Meanings Preserves.
  unfold preserves_site, site_qent, primitive_closure, BaseSites.retained_site in *.
  cbn [fst snd] in Preserves |- *. destruct (decode (pick c provided)) as [primitive|] eqn:Decoded; [|reflexivity].
  destruct (BaseCall.uses_site primitive); [|reflexivity].
  destruct requested, provided; try discriminate.
  destruct (fc_table c) eqn:Table; try discriminate.
  apply andb_true_iff in Preserves as [Supplied Equal].
  apply ty_eqb_spec in Equal. subst t.
  rewrite (@supplied_site_type _ _ _ _ _ _ _ Code Table Supplied).
  unfold ground. now rewrite Meanings.
Qed.

Section Rel.
Variables U H : usym.

(* The invariant: an entry under F's key is the platform's closure of H. *)
Definition kgood (i : item) (a : provision_value) : Prop :=
  isU U i = true -> exists dc, a = FunctorClosure H None dc None.

(* Entries agree up to captured environments and Base-ignored primitive
   sites. Sites used for allocation or memory still agree exactly. Captures agree entry for
   entry (closures are never narrowed: their environments are
   selected by satisfaction maps, whose sources the code looks up). *)
Inductive prel : provision_value -> provision_value -> Prop :=
| PVal : forall w, prel (ValueProvision w) (ValueProvision w)
| PClo : forall f d d', lrel d d' -> prel (FunctionClosure f d) (FunctionClosure f d')
| PFun : forall h sms d d' rms, osm_ok (sm_ok U) sms = true -> lrel d d' ->
    prel (FunctorClosure h sms d rms) (FunctorClosure h sms d' rms)
| PPrim : forall p ot ot', BaseSites.retained_site p ot = BaseSites.retained_site p ot' ->
    prel (PrimitiveClosure p ot) (PrimitiveClosure p ot')
with lrel : environment -> environment -> Prop :=
| LRel : forall d d', type_meanings d = type_meanings d' -> type_aliases d = type_aliases d' ->
    lrel2 (provision_values d) (provision_values d') -> lrel d d'
with lrel2 : list (item * provision_value) -> list (item * provision_value) -> Prop :=
| L2Nil : lrel2 [] []
| L2Cons : forall i a a' l l', prel a a' -> kgood i a -> lrel2 l l' ->
    lrel2 ((i, a) :: l) ((i, a') :: l').

Definition orel (i : item) (o o' : option provision_value) : Prop :=
  match o, o' with
  | Some a, Some a' => prel a a' /\ kgood i a
  | None, None => True
  | _, _ => False
  end.

(* The environments agree on the items in S (after a narrowed application,
   the transformed one lacks the members its continuation does not read). *)
Record agree (S : item -> Prop) (d d' : environment) : Prop := Agree {
  ag_tm : type_meanings d = type_meanings d';
  ag_ta : type_aliases d = type_aliases d';
  ag_look : forall i, S i -> orel i (lookup_provision i d) (lookup_provision i d')
}.

Definition lk (S : item -> Prop) (l : list item) : Prop := forall i, In i l -> S i.
Definition full (_ : item) : Prop := True.

Lemma lrel_tm : forall d d', lrel d d' -> type_meanings d = type_meanings d'.
Proof. intros d d' Hd. inversion Hd; assumption. Qed.
Lemma lrel_ta : forall d d', lrel d d' -> type_aliases d = type_aliases d'.
Proof. intros d d' Hd. inversion Hd; assumption. Qed.
Lemma lrel_pv : forall d d', lrel d d' -> lrel2 (provision_values d) (provision_values d').
Proof. intros d d' Hd. inversion Hd; assumption. Qed.

Lemma lrel2_lookup : forall l l' i, lrel2 l l' -> orel i (lookup_entry i l) (lookup_entry i l').
Proof.
  intros l l' i Hl. induction Hl; simpl; auto.
  destruct (item_eqb i i0) eqn:E; auto. apply item_eqb_true in E. subst. simpl. auto.
Qed.

Lemma lrel_agree : forall S d d', lrel d d' -> agree S d d'.
Proof.
  intros S d d' Hd. constructor; [exact (lrel_tm Hd)|exact (lrel_ta Hd)|].
  intros i _. apply lrel2_lookup, lrel_pv, Hd.
Qed.

Lemma agree_make : forall S d d' ts als, agree S d d' ->
  agree S (EnvironmentWithAliases ts als (provision_values d))
          (EnvironmentWithAliases ts als (provision_values d')).
Proof. intros S d d' ts als Hd. constructor; auto. intros i Hi. exact (ag_look Hd i Hi). Qed.
Lemma lrel_make : forall d d' ts als, lrel d d' ->
  lrel (EnvironmentWithAliases ts als (provision_values d))
       (EnvironmentWithAliases ts als (provision_values d')).
Proof. intros d d' ts als Hd. constructor; auto. exact (lrel_pv Hd). Qed.

Lemma agree_install : forall S d d' i a a', prel a a' -> kgood i a -> agree S d d' ->
  agree S (install_provision i a d) (install_provision i a' d').
Proof.
  intros S d d' i a a' Ha Hg Hd. constructor; simpl; [exact (ag_tm Hd)|exact (ag_ta Hd)|].
  intros j Hj. unfold lookup_provision. simpl.
  destruct (item_eqb j i) eqn:E; [apply item_eqb_true in E; subst; simpl; auto|].
  exact (ag_look Hd j Hj).
Qed.
Lemma lrel_install : forall d d' i a a', prel a a' -> kgood i a -> lrel d d' ->
  lrel (install_provision i a d) (install_provision i a' d').
Proof.
  intros d d' i a a' Ha Hg Hd. constructor; simpl; [exact (lrel_tm Hd)|exact (lrel_ta Hd)|].
  constructor; auto. exact (lrel_pv Hd).
Qed.

Lemma lrel2_app : forall a a' b b', lrel2 a a' -> lrel2 b b' -> lrel2 (a ++ b) (a' ++ b').
Proof. intros a a' b b' Ha Hb. induction Ha; simpl; auto. constructor; auto. Qed.

Lemma agree_append : forall S a a' b b', lrel a a' -> agree S b b' ->
  agree S (append_environment a b) (append_environment a' b').
Proof.
  intros S a a' b b' Ha Hb. constructor; simpl;
    [rewrite (lrel_tm Ha), (ag_tm Hb)|rewrite (lrel_ta Ha), (ag_ta Hb)|]; auto.
  intros i Hi. unfold lookup_provision. simpl. rewrite !lookup_entry_app.
  pose proof (lrel2_lookup i (lrel_pv Ha)) as Q.
  destruct (lookup_entry i (provision_values a)), (lookup_entry i (provision_values a'));
    simpl in Q; try contradiction; auto.
  exact (ag_look Hb i Hi).
Qed.
Lemma lrel_append : forall a a' b b', lrel a a' -> lrel b b' ->
  lrel (append_environment a b) (append_environment a' b').
Proof.
  intros a a' b b' Ha Hb. constructor; simpl;
    [rewrite (lrel_tm Ha), (lrel_tm Hb)|rewrite (lrel_ta Ha), (lrel_ta Hb)|]; auto.
  apply lrel2_app; apply lrel_pv; assumption.
Qed.

Lemma lrel_empty : lrel empty_environment empty_environment.
Proof. constructor; auto. constructor. Qed.

Lemma kgood_move : forall s req a, kgood s a -> Bool.eqb (isU U req) (isU U s) = true -> kgood req a.
Proof.
  intros s req a Hg Hq Hr. apply Bool.eqb_prop in Hq. apply Hg. congruence.
Qed.

Definition oprel {A} (R : A -> A -> Prop) (o o' : option A) : Prop :=
  match o, o' with
  | Some x, Some x' => R x x'
  | None, None => True
  | _, _ => False
  end.

Lemma all_select : forall S d d' sm, agree S d d' -> lk S (srcs sm) -> sm_ok U sm = true ->
  oprel lrel (select_provisions d sm) (select_provisions d' sm).
Proof.
  intros S d d' sm Hd. induction sm as [|[req s] r IH]; simpl; intros Hs Hok.
  - exact lrel_empty.
  - apply andb_true_iff in Hok as [Hq Hok].
    assert (Hr : lk S (srcs r)) by (intros x Hx; apply Hs; simpl; auto).
    specialize (IH Hr Hok). pose proof (ag_look Hd s (Hs s (or_introl eq_refl))) as Ho.
    destruct (lookup_provision s d) as [a|], (lookup_provision s d') as [a'|];
      simpl in Ho; try contradiction;
      destruct (select_provisions d r), (select_provisions d' r); simpl in IH |- *;
      try contradiction; auto.
    destruct Ho as [Ha Hg]. apply lrel_install; auto. eapply kgood_move; eauto.
Qed.

Lemma lrel_select : forall d d' sm, lrel d d' -> sm_ok U sm = true ->
  oprel lrel (select_provisions d sm) (select_provisions d' sm).
Proof. intros d d' sm Hd Hok. apply (all_select (S := full)); auto. apply lrel_agree, Hd. intros ? ?; exact I. Qed.

Lemma lrel_adapt : forall sms d d', lrel d d' -> osm_ok (sm_ok U) sms = true ->
  oprel lrel (adapt_provisions d sms) (adapt_provisions d' sms).
Proof.
  intros [sm|] d d' Hd Hok; simpl in *; [apply lrel_select; auto | exact Hd].
Qed.

Lemma agree_ground : forall S d d', agree S d d' -> forall tau, ground d tau = ground d' tau.
Proof. intros S d d' Hd tau. unfold ground. rewrite (ag_tm Hd). reflexivity. Qed.

Lemma lrel_capture : forall S d d' types dc dc', agree S d d' -> lrel dc dc' ->
  lrel (typed_capture d types dc) (typed_capture d' types dc').
Proof.
  intros S d d' types dc dc' Hd Hc. unfold typed_capture.
  replace (ground d') with (ground d) by (unfold ground; rewrite (ag_tm Hd); reflexivity).
  apply lrel_make; exact Hc.
Qed.

Lemma lrel_call : forall S d d' th dc dc', agree S d d' -> lrel dc dc' ->
  lrel (call_environment d th dc) (call_environment d' th dc').
Proof.
  intros S d d' th dc dc' Hd Hc. unfold call_environment.
  apply lrel_append; [|exact Hc]. unfold instantiate.
  replace (ground d') with (ground d) by (unfold ground; rewrite (ag_tm Hd); reflexivity).
  constructor; auto. constructor.
Qed.

Lemma agree_bind_type : forall S d d' t tau, agree S d d' ->
  agree S (bind_type d t tau) (bind_type d' t tau).
Proof.
  intros S d d' t tau Hd. unfold bind_type, ground. rewrite (ag_tm Hd), (ag_ta Hd).
  apply agree_make; exact Hd.
Qed.

Lemma prim_closure_eq : forall S d d' i p, agree S d d' ->
  primitive_closure d i p = primitive_closure d' i p.
Proof. intros S d d' i p Hd. unfold primitive_closure, ground. rewrite (ag_tm Hd). reflexivity. Qed.

Lemma prel_prim_closure : forall d i p, prel (primitive_closure d i p) (primitive_closure d i p).
Proof. intros. unfold primitive_closure. constructor. reflexivity. Qed.
Lemma kgood_nonU : forall i a, isU U i = false -> kgood i a.
Proof. intros i a Hi Hu. congruence. Qed.

Lemma result_env_rel : forall S outputs d3 d3' Dr dr dr', agree S d3 d3' -> lrel dr dr' ->
  oprel lrel (result_environment outputs d3 Dr dr) (result_environment outputs d3' Dr dr').
Proof.
  intros S outputs d3 d3' Dr dr dr' Hd Hr. unfold result_environment.
  rewrite (ProgramPrune.read_result_types_eq _ _ (ag_tm Hd) (ag_ta Hd)).
  destruct (read_result_types d3' Dr outputs) as [[w a]|]; simpl; auto.
  constructor; auto. exact (lrel_pv Hr).
Qed.

(* The root: a checked environment agrees with itself. *)
Lemma kok_good : forall i a, kok U H i a = true -> kgood i a.
Proof.
  intros i a Hk Hu. unfold kok in Hk. rewrite Hu in Hk.
  destruct a as [| |h sms dc rms|]; try discriminate.
  destruct sms; try discriminate; destruct rms; try discriminate.
  apply Nat.eqb_eq in Hk. subst. eauto.
Qed.

Lemma pv_ok_refl : forall a, pv_ok U H a = true -> prel a a.
Proof.
  fix IH 1. intros [w|f [ts als ps]|h sms [ts als ps] rms|p ot] Hok; simpl in Hok.
  - constructor.
  - constructor. constructor; auto. simpl. induction ps as [|[j x] r IR]; [constructor|].
    apply andb_true_iff in Hok as [Hok Hr]. apply andb_true_iff in Hok as [Hk Hx].
    constructor; [apply IH, Hx|apply kok_good, Hk|apply IR, Hr].
  - apply andb_true_iff in Hok as [Hsms Hok]. constructor; auto. constructor; auto. simpl.
    induction ps as [|[j x] r IR]; [constructor|].
    apply andb_true_iff in Hok as [Hok Hr]. apply andb_true_iff in Hok as [Hk Hx].
    constructor; [apply IH, Hx|apply kok_good, Hk|apply IR, Hr].
  - constructor. reflexivity.
Qed.

Lemma env_ok_refl : forall d, env_ok U H d = true -> lrel d d.
Proof.
  intros [ts als ps] Hok. unfold env_ok in Hok. simpl in Hok. constructor; auto. simpl.
  induction ps as [|[j x] r IR]; simpl in Hok; [constructor|].
  apply andb_true_iff in Hok as [Hok Hr]. apply andb_true_iff in Hok as [Hk Hx].
  constructor; [apply pv_ok_refl, Hx|apply kok_good, Hk|apply IR, Hr].
Qed.

End Rel.

(* ===================== 4. Static facts ================================== *)

Lemma lookup_entry_in : forall i l a, lookup_entry i l = Some a -> exists k, In (k, a) l.
Proof.
  induction l as [|[k b] r IH]; simpl; intros a E; [discriminate|].
  destruct (item_eqb i k); [injection E as <-; eauto|]. destruct (IH a E) as [k' Hk]; eauto.
Qed.

Lemma lookup_entry_some : forall i a l, In (i, a) l -> exists b, lookup_entry i l = Some b.
Proof.
  induction l as [|[k b] r IH]; simpl; intros Hin; [contradiction|].
  destruct (item_eqb i k) eqn:E; eauto. destruct Hin as [Hin|Hin]; auto.
  injection Hin as -> ->. assert (item_eqb i i = true) by (apply item_eqb_true; reflexivity). congruence.
Qed.

Lemma lookup_entry_filter_none : forall (f : item * provision_value -> bool) i l,
  lookup_entry i l = None -> lookup_entry i (filter f l) = None.
Proof.
  induction l as [|[k b] r IH]; simpl; intros E; auto.
  destruct (item_eqb i k) eqn:Ek; [discriminate|]. destruct (f (k, b)); simpl; [rewrite Ek|]; auto.
Qed.

Lemma lookup_entry_none : forall i l, existsb (item_eqb i) (map fst l) = false -> lookup_entry i l = None.
Proof.
  induction l as [|[k b] r IH]; simpl; intros E; auto.
  apply orb_false_iff in E as [E1 E2]. rewrite E1. auto.
Qed.

Section Body.
Variable U : usym.
Variable cH : functor_code.
Notation bsH := (fc_bindings cH).

Lemma prim_ent : forall bs k a, forallb body_prim bs = true -> In (k, a) (rev (map bent bs)) ->
  exists p, a = PrimitiveClosure p None /\ prim_ok p = true.
Proof.
  intros bs k a Hp Hin. rewrite <- in_rev in Hin. apply in_map_iff in Hin as [b [Eb Hb]].
  rewrite forallb_forall in Hp. specialize (Hp b Hb). destruct b; simpl in Hp, Eb; try discriminate.
  injection Eb as _ <-. eauto.
Qed.

Lemma pick_ok : forallb body_prim bsH = true -> forall s, prim_ok (pick cH s) = true.
Proof.
  intros Hp s. unfold pick, hent.
  destruct (lookup_entry s (rev (map bent bsH))) as [a|] eqn:E; [|reflexivity].
  apply lookup_entry_in in E as [k Hin]. destruct (prim_ent _ _ _ Hp Hin) as [p [-> Hq]]. exact Hq.
Qed.

Lemma hent_bound : forall s, forallb body_prim bsH = true -> existsb (binds s) bsH = true ->
  lookup_entry s (rev (map bent bsH)) = Some (PrimitiveClosure (pick cH s) None).
Proof.
  intros s Hp He. apply existsb_exists in He as [b [Hb Hbs]].
  destruct b as [| | | | | | |k p]; simpl in Hbs; try discriminate.
  apply item_eqb_true in Hbs. subst.
  assert (Hin : In (s, PrimitiveClosure p None) (rev (map bent bsH))).
  { rewrite <- in_rev. apply in_map_iff. exists (BPrimitive s p). auto. }
  destruct (lookup_entry_some _ _ _ Hin) as [a Ha].
  pose proof Ha as Ha'. apply lookup_entry_in in Ha' as [k Hk].
  destruct (prim_ent _ _ _ Hp Hk) as [q [-> _]].
  unfold pick, hent. rewrite Ha. reflexivity.
Qed.

Lemma sel_in : forall rs q, In q (sel cH rs) -> In (fst q) rs /\ In (snd q) (fc_sources cH).
Proof.
  intros rs [a b] Hq. unfold sel in Hq. rewrite <- in_rev in Hq. simpl.
  split; [eapply in_combine_l|eapply in_combine_r]; eauto.
Qed.

Lemma rs_ok_in : forall rs i, rs_ok U rs = true -> In i rs -> isU U i = false.
Proof.
  intros rs i Hr Hi. unfold rs_ok in Hr. rewrite forallb_forall in Hr.
  apply negb_true_iff, Hr, Hi.
Qed.

Lemma prims_of_prim : forallb body_prim bsH = true -> forall Q, forallb body_prim (prims_of cH Q) = true.
Proof.
  intros Hp Q. unfold prims_of. apply forallb_forall. intros b Hb.
  rewrite <- in_rev in Hb. apply in_map_iff in Hb as [q [<- _]]. simpl. apply pick_ok, Hp.
Qed.

Lemma prims_of_ents : forall Q, rev (map bent (prims_of cH Q)) = map (qent cH) Q.
Proof. intros Q. unfold prims_of. rewrite map_rev, rev_involutive, map_map. reflexivity. Qed.

Lemma prims_of_length : forall Q, length (prims_of cH Q) = length Q.
Proof. intros Q. unfold prims_of. rewrite length_rev, length_map. reflexivity. Qed.

Lemma keepf_sound : forall W Q i, In i W -> existsb (item_eqb i) (map fst Q) = true ->
  keepf W Q i = true.
Proof.
  intros W Q i Hi Hq. unfold keepf.
  assert (Hw : inW W i = true) by (apply existsb_exists; exists i; split; auto; apply item_eqb_true; auto).
  destruct (existsb (fun q => inW W (fst q)) Q) eqn:E; auto.
  exfalso. apply existsb_exists in Hq as [j [Hj Ej]]. apply item_eqb_true in Ej. subst j.
  apply in_map_iff in Hj as [q [Eq Hq]]. subst.
  assert (existsb (fun q => inW W (fst q)) Q = true) by (apply existsb_exists; eauto). congruence.
Qed.

Lemma keepf_nonempty : forall W Q, Q <> [] -> filter (fun q => keepf W Q (fst q)) Q <> [].
Proof.
  intros W Q HQ. unfold keepf.
  destruct (existsb (fun q => inW W (fst q)) Q) eqn:E.
  - apply existsb_exists in E as [q [Hq Eq]]. intro Hf.
    assert (In q (filter (fun q => inW W (fst q)) Q)) by (apply filter_In; auto). rewrite Hf in H. auto.
  - destruct Q as [|q r]; [contradiction|]. simpl.
    assert (item_eqb (fst q) (fst q) = true) by (apply item_eqb_true; auto). rewrite H. discriminate.
Qed.

Lemma fast_apply_some : forall b Q, fast_apply U cH b = Some Q ->
  exists th rs, b = BApply U th [] [] rs /\ Q = sel cH rs /\ Q <> [] /\
    forallb (preserves_site cH th) Q = true.
Proof.
  intros b Q E. destruct b as [| | | | | |u th outputs sm rs|]; cbn [fast_apply] in E; try discriminate.
  destruct outputs; try discriminate. destruct sm; try discriminate.
  destruct (Nat.eqb u U) eqn:Eu; try discriminate. apply Nat.eqb_eq in Eu. subst.
  destruct (sel cH rs) as [|head tail] eqn:Es; try discriminate.
  destruct (forallb (preserves_site cH th) (head :: tail)) eqn:Sites; try discriminate.
  injection E as <-. exists th, rs. repeat split; auto. discriminate.
Qed.

Lemma fp_bs_cons : forall b r, fp_bs U cH (b :: r) =
  match fast_apply U cH b with Some Q => prims_of cH Q | None => [fp_b U cH b] end ++ fp_bs U cH r.
Proof. reflexivity. Qed.

Lemma map_site_qent_filter : forall d source (kp : item -> bool) Q,
  map (site_qent cH d source) (filter (fun q => kp (fst q)) Q) =
  filter (fun entry => kp (fst entry)) (map (site_qent cH d source) Q).
Proof. intros d source kp. induction Q as [|q tail IH]; cbn; [reflexivity|]. destruct (kp (fst q)); cbn; congruence. Qed.

Lemma lookup_site_qent : forall d source Q key,
  lookup_entry key (map (site_qent cH d source) Q) =
  option_map (fun q => snd (site_qent cH d source q))
    (find (fun q => item_eqb key (fst q)) Q).
Proof.
  intros d source Q key. induction Q as [|q rest IH]; [reflexivity|].
  cbn [map lookup_entry find site_qent fst snd].
  now destruct (item_eqb key (fst q)).
Qed.

Lemma related_site_qent : forall H P caller next captured th q,
  view_functor P H = Some cH ->
  type_meanings caller = type_meanings next ->
  preserves_site cH th q = true ->
  prel U H (snd (site_qent cH (application_environment P caller H th captured) true q))
    (snd (site_qent cH next false q)).
Proof.
  intros H P caller next captured th q Code Meanings Site.
  pose proof (@retained_site_qent P H cH caller next captured th q Code Meanings Site) as Equal.
  unfold site_qent, primitive_closure in Equal |- *. cbn [snd] in Equal |- *.
  now constructor.
Qed.

(* After a narrowed application, the environments agree at the retained
   keys and at the caller's previously available keys. *)
Lemma agree_narrow_sites : forall H P S d d' Q kp captured th,
  view_functor P H = Some cH ->
  agree U H S d d' -> (forall q, In q Q -> isU U (fst q) = false) ->
  forallb (preserves_site cH th) Q = true ->
  agree U H (fun i => if existsb (item_eqb i) (map fst Q) then kp i = true else S i)
    (append_environment (EnvironmentWithAliases [] []
      (map (site_qent cH (application_environment P d H th captured) true) Q)) d)
    (EnvironmentWithAliases (type_meanings d') (type_aliases d')
       (filter (fun entry => kp (fst entry)) (map (site_qent cH d' false) Q) ++ provision_values d')).
Proof.
  intros H P S d d' Q kp captured th Code Related NonU Sites.
  constructor; cbn; [exact (ag_tm Related)|exact (ag_ta Related)|].
  intros key Used. unfold lookup_provision. cbn [append_environment provision_values].
  rewrite !lookup_entry_app, lookup_site_qent.
  destruct (existsb (item_eqb key) (map fst Q)) eqn:HasKey.
  - rewrite lookup_entry_filter by exact Used. rewrite lookup_site_qent.
    destruct (find (fun q => item_eqb key (fst q)) Q) as [q|] eqn:Found.
    + apply find_some in Found as [InQ Equal]. apply item_eqb_true in Equal. subst key.
      cbn [option_map orel]. split.
      * eapply related_site_qent; [exact Code|exact (ag_tm Related)|].
        exact (proj1 (forallb_forall _ _) Sites _ InQ).
      * apply kgood_nonU, NonU, InQ.
    + apply existsb_exists in HasKey as [requested [InKeys Equal]].
      apply in_map_iff in InKeys as [q [<- InQ]].
      pose proof (find_none _ _ Found q InQ) as Missing. congruence.
  - assert (Absent : lookup_entry key (map (site_qent cH d' false) Q) = None).
    { apply lookup_entry_none. rewrite map_map. exact HasKey. }
    rewrite (lookup_entry_filter_none _ _ _ Absent).
    rewrite lookup_site_qent in Absent.
    destruct (find (fun q => item_eqb key (fst q)) Q); [discriminate|].
    exact (ag_look Related key Used).
Qed.

Lemma lrel_site_qents : forall H P d d' captured th Q,
  view_functor P H = Some cH ->
  type_meanings d = type_meanings d' ->
  (forall q, In q Q -> isU U (fst q) = false) ->
  forallb (preserves_site cH th) Q = true ->
  lrel U H (EnvironmentWithAliases [] []
    (map (site_qent cH (application_environment P d H th captured) true) Q))
    (EnvironmentWithAliases [] [] (map (site_qent cH d' false) Q)).
Proof.
  intros H P d d' captured th Q Code Meanings NonU Sites. constructor; try reflexivity.
  induction Q as [|q tail IH]; cbn [map site_qent fst snd provision_values]; [constructor|].
  cbn [forallb] in Sites. apply andb_true_iff in Sites as [Head Tail].
  constructor.
  - eapply related_site_qent; eassumption.
  - apply kgood_nonU, (NonU q). now left.
  - apply IH; [intros q' Member; apply NonU; now right|exact Tail].
Qed.

End Body.

(* ===================== 5. Simulation ===================================== *)

Module Sim (Prims : PRIMITIVES) (I : INTERP Prims).
Import Prims I.
Local Unset Implicit Arguments.
Module PS := ProgramPrune.Sim Prims I.

Definition oof (r : evaluation) : Prop := evaluation_signal r = OutOfFuel.
Definition stuck (r : evaluation) : Prop := exists c, evaluation_signal r = Stuck c.
Definition bad (r : evaluation) : Prop := stuck r \/ oof r.
Definition badl {A} (x : A + evaluation) : Prop := match x with inr r => bad r | inl _ => False end.
Definition oofl {A} (x : A + evaluation) : Prop := match x with inr r => oof r | inl _ => False end.
Definition stuckl {A} (x : A + evaluation) : Prop := match x with inr r => stuck r | inl _ => False end.
(* "eventually": at every large enough fuel *)
Definition evf (Q : nat -> Prop) : Prop := exists k0, forall j, k0 <= j -> Q j.

Definition recorded_site (d : environment) (key : item) : option ty :=
  match key with IMth receiver _ _ => Some (ground d receiver) | _ => None end.
Definition record_value (d : environment) (key : item) (entry : provision_value) : provision_value :=
  match entry with PrimitiveClosure operation _ => primitive_closure d key operation | _ => entry end.
Definition record_entry (d : environment) (entry : item * provision_value) :=
  (fst entry, record_value d (fst entry) (snd entry)).
Lemma lookup_recorded_entries : forall d entries key,
  lookup_entry key (map (record_entry d) entries) = option_map (record_value d key) (lookup_entry key entries).
Proof.
  intros d entries key. induction entries as [|[source entry] tail IH]; cbn [map record_entry fst snd lookup_entry]; [reflexivity|].
  destruct (item_eqb key source) eqn:Equal; [apply item_eqb_true in Equal; subst; reflexivity|exact IH].
Qed.

Definition runtime_bent (d : environment) (binding : binding) : item * provision_value :=
  match binding with
  | BPrimitive key operation => (key, primitive_closure d key operation)
  | _ => (ITy 0, ValueProvision VUnit)
  end.
Definition installs (d : environment) (bs : list binding) : environment :=
  EnvironmentWithAliases (type_meanings d) (type_aliases d)
    (rev (map (runtime_bent d) bs) ++ provision_values d).

Lemma installs_step : forall d key operation bindings,
  installs (install_provision key (primitive_closure d key operation) d) bindings =
  installs d (BPrimitive key operation :: bindings).
Proof.
  intros [meanings aliases entries] key operation bindings.
  unfold installs, install_provision. cbn [type_meanings type_aliases provision_values map rev runtime_bent].
  rewrite <- app_assoc. reflexivity.
Qed.

Lemma runtime_entries : forall d bindings,
  rev (map (runtime_bent d) bindings) = map (record_entry d) (rev (map bent bindings)).
Proof.
  intros d bindings. rewrite map_rev, map_map. f_equal.
  apply map_ext. intros binding. destruct binding; reflexivity.
Qed.
Lemma runtime_lookup : forall d bindings key,
  lookup_entry key (rev (map (runtime_bent d) bindings)) =
  option_map (record_value d key) (lookup_entry key (rev (map bent bindings))).
Proof. intros. rewrite runtime_entries. apply lookup_recorded_entries. Qed.
Lemma lookup_installs : forall s d bs, lookup_provision s (installs d bs) =
  match lookup_entry s (rev (map (runtime_bent d) bs)) with Some a => Some a | None => lookup_provision s d end.
Proof. intros. unfold lookup_provision, installs. simpl. apply lookup_entry_app. Qed.

Lemma binds_prims : forall P0 bs j d g st, forallb body_prim bs = true ->
  eval_bindings j P0 d g st bs =
  if Nat.ltb (length bs) j then inl (installs d bs, g, st) else inr (Evaluation OutOfFuel g st).
Proof.
  intros P0. induction bs as [|b r IH]; intros [|j] d g st Hp; simpl; auto.
  - destruct d; reflexivity.
  - destruct b; simpl in Hp; try discriminate. apply andb_true_iff in Hp as [Hp1 Hp].
    rewrite IH by exact Hp. rewrite installs_step. reflexivity.
Qed.

Lemma prims_app : forall P0 bs x d g st rest, forallb body_prim bs = true ->
  eval_bindings (length bs + x) P0 d g st (bs ++ rest) = eval_bindings x P0 (installs d bs) g st rest.
Proof.
  intros P0. induction bs as [|b r IH]; intros x d g st rest Hp; simpl.
  - unfold installs. destruct d; reflexivity.
  - destruct b; simpl in Hp; try discriminate. apply andb_true_iff in Hp as [Hp1 Hp].
    rewrite IH by exact Hp. rewrite installs_step. reflexivity.
Qed.

Lemma prims_app_le : forall P0 bs z d g st rest, forallb body_prim bs = true -> z <= length bs ->
  eval_bindings z P0 d g st (bs ++ rest) = inr (Evaluation OutOfFuel g st).
Proof.
  intros P0. induction bs as [|b r IH]; intros [|z] d g st rest Hp Hz; simpl in *; auto; try lia.
  destruct b; simpl in Hp; try discriminate. apply andb_true_iff in Hp as [Hp1 Hp].
  apply IH; auto. lia.
Qed.

Lemma ebind_prim : forall P0 n d g st i p e, 2 <= n ->
  eval (S n) P0 d g st (EBind (BPrimitive i p) e) = eval n P0 (install_provision i (primitive_closure d i p) d) g st e.
Proof. intros P0 [|[|n]] d g st i p e Hn; try lia. reflexivity. Qed.

Lemma chain_eval : forall P0 bs x d g st e, forallb body_prim bs = true -> 2 <= x ->
  eval (length bs + x) P0 d g st (chain bs e) = eval x P0 (installs d bs) g st e.
Proof.
  intros P0. induction bs as [|b r IH]; intros x d g st e Hp Hx.
  - simpl. unfold installs. destruct d; reflexivity.
  - destruct b as [| | | | | | |i p]; simpl in Hp; try discriminate. apply andb_true_iff in Hp as [Hp1 Hp].
    change (length (BPrimitive i p :: r) + x) with (S (length r + x)).
    change (chain (BPrimitive i p :: r) e) with (EBind (BPrimitive i p) (chain r e)).
    rewrite ebind_prim by lia.
    rewrite IH by auto. rewrite installs_step. reflexivity.
Qed.

Lemma chain_short : forall P0 bs z d g st e, forallb body_prim bs = true -> z < length bs + 2 ->
  bs <> [] -> oof (eval z P0 d g st (chain bs e)).
Proof.
  intros P0. induction bs as [|b r IH]; intros z d g st e Hp Hz Hne; [contradiction|].
  destruct b as [| | | | | | |i p]; simpl in Hp; try discriminate. apply andb_true_iff in Hp as [Hp1 Hp].
  destruct z as [|[|[|z]]]; try reflexivity.
  change (chain (BPrimitive i p :: r) e) with (EBind (BPrimitive i p) (chain r e)).
  rewrite ebind_prim by lia.
  destruct r as [|b' r']; [simpl in Hz; lia|].
  apply IH; auto; [simpl in *; lia|discriminate].
Qed.

Lemma ret_call_bad : forall g r, bad r -> bad (ret_call g r).
Proof.
  intros g [o g0 s0] [[c Hc]|Hc]; unfold oof in *; simpl in *; subst; [left; eexists|right]; reflexivity.
Qed.

(* If a sub-evaluation of the reference is stuck or out of fuel, so is the whole. *)
Ltac bk :=
  match goal with
  | Hs : bad ?r |- _ =>
      let o0 := fresh "o" in let g0 := fresh "g" in let s0 := fresh "s" in let c0 := fresh "c" in
      left; revert Hs; destruct r as [o0 g0 s0]; unfold bad, stuck, oof; simpl;
      intros [[c0 ->]| ->]; simpl; [left; eexists; reflexivity|right; reflexivity]
  | Hs : badl ?r |- _ =>
      let o0 := fresh "o" in let g0 := fresh "g" in let s0 := fresh "s" in let c0 := fresh "c" in
      left; revert Hs; destruct r as [?|[o0 g0 s0]]; simpl; [intros []|];
      unfold bad, stuck, oof; simpl; intros [[c0 ->]| ->]; simpl;
      [left; eexists; reflexivity|right; reflexivity]
  end.

Ltac sumk acc :=
  match goal with
  | Hx : forall j, ?k <= j -> _ |- _ =>
      lazymatch acc with context [k] => fail | _ => sumk constr:(k + acc) end
  | _ => acc
  end.
Ltac rw_facts :=
  repeat match goal with
  | Hx : lookup_provision ?i ?dd = _ |- context [lookup_provision ?i ?dd] => rewrite Hx
  | Hx : select_provisions ?dd ?sm = _ |- context [select_provisions ?dd ?sm] => rewrite Hx
  | Hx : adapt_provisions ?dd ?sms = _ |- context [adapt_provisions ?dd ?sms] => rewrite Hx
  | Hx : result_environment ?o ?dd ?D ?r = _ |- context [result_environment ?o ?dd ?D ?r] => rewrite Hx
  | Hx : view_function ?PP ?f = _ |- context [view_function ?PP ?f] => rewrite Hx
  | Hx : view_functor ?PP ?f = _ |- context [view_functor ?PP ?f] => rewrite Hx
  end.
Ltac rw_evs :=
  repeat match goal with Hx : forall j, _ <= j -> _ |- _ => rewrite Hx by lia end.
Ltac red1 := cbn [eval eval_arguments eval_bindings apply_functor fp_e fp_code
  fc_bindings fc_sources fc_outputs fc_inputs fc_linked fc_table ret_call pop1 popn with_g
  evaluation_signal evaluation_locals evaluation_state map app].
Ltac rwall := repeat (progress (rw_facts; rw_evs; red1)).
(* the transformed run, at every large fuel *)
Ltac evgo :=
  let s := sumk 0 in exists (S s); let j := fresh "j" in let Hj := fresh "Hj" in
  intros j Hj; destruct j as [|j]; [lia|]; red1; rwall.
Ltac err := solve [right; evgo; reflexivity].
Ltac errq := solve [right; eexists; split; cycle 1; [evgo; reflexivity|cbn; auto]].
Ltac stuck_here := left; cbn; unfold bad, stuck; left; eexists; reflexivity.
Ltac oof_here := left; cbn; unfold bad, oof; right; reflexivity.
Ltac andb_split :=
  repeat match goal with H : (_ && _) = true |- _ => apply andb_true_iff in H as [? ?] end.

Section SimSec.
Variables (U H : usym) (cH : functor_code).
Variables P P' : program_view.
Variable ok : mstate -> Prop.
Hypothesis Hfn : forall f, view_function P' f = option_map (fp_e U cH) (view_function P f).
Hypothesis Hfun : forall h, view_functor P' h = option_map (fp_code U cH) (view_functor P h).
Hypothesis Hprim : forall q site ws st, ok st -> run_prim P' q site ws st = run_prim P q site ws st.
Hypothesis Hok : forall q site ws st, ok st -> ok (snd (run_prim P q site ws st)).
Hypothesis Hsites : forall operation site ws st,
  run_prim P operation site ws st =
  run_prim P operation (BaseSites.retained_site operation site) ws st.
Hypothesis Hbool : view_bool P' = view_bool P.
Hypothesis Htrue : view_true P' = view_true P.
Hypothesis Hfalse : view_false P' = view_false P.
Hypothesis Hcasts : view_casts P' = view_casts P.
Hypothesis Hokf : forall f b, view_function P f = Some b -> ok_e U b = true.
Hypothesis Hoku : forall h c, view_functor P h = Some c -> forallb (ok_b U) (fc_bindings c) = true.
Hypothesis HH : view_functor P H = Some cH.
Hypothesis Hbody : body_ok cH = true.

Notation T := (fp_e U cH).
Notation Tb := (fp_b U cH).
Notation Tbs := (fp_bs U cH).
Notation agree := (agree U H).
Notation lrel := (lrel U H).
Notation bsH := (fc_bindings cH).

Lemma Hprims : forallb body_prim bsH = true.
Proof. unfold body_ok in Hbody. apply andb_true_iff in Hbody. tauto. Qed.
Lemma Hcover : forall s, In s (fc_sources cH) -> existsb (binds s) bsH = true.
Proof.
  intros s Hs. unfold body_ok in Hbody. apply andb_true_iff in Hbody as [_ Hc].
  rewrite forallb_forall in Hc. auto.
Qed.

Lemma Hok' : forall q site ws st, ok st -> ok (snd (run_prim P' q site ws st)).
Proof. intros. rewrite Hprim by assumption. auto. Qed.

Lemma inv_e : forall P0, (forall q site ws st, ok st -> ok (snd (run_prim P0 q site ws st))) ->
  forall k d g st e, ok st -> ok (evaluation_state (eval k P0 d g st e)).
Proof. intros P0 Hk k. exact (proj1 (@PS.inv_all P0 ok Hk k)). Qed.
Lemma inv_l : forall P0, (forall q site ws st, ok st -> ok (snd (run_prim P0 q site ws st))) ->
  forall k d g st l, ok st -> PS.okl (ok := ok) (eval_arguments k P0 d g st l).
Proof. intros P0 Hk k. exact (proj1 (proj2 (@PS.inv_all P0 ok Hk k))). Qed.
Lemma inv_b : forall P0, (forall q site ws st, ok st -> ok (snd (run_prim P0 q site ws st))) ->
  forall k d g st bs, ok st -> PS.okl (ok := ok) (eval_bindings k P0 d g st bs).
Proof. intros P0 Hk k. exact (proj1 (proj2 (proj2 (@PS.inv_all P0 ok Hk k)))). Qed.
Lemma inv_a : forall P0, (forall q site ws st, ok st -> ok (snd (run_prim P0 q site ws st))) ->
  forall k d u th outputs sm rs st, ok st -> PS.oka (ok := ok) (apply_functor k P0 d u th outputs sm rs st).
Proof. intros P0 Hk k. exact (proj2 (proj2 (proj2 (@PS.inv_all P0 ok Hk k)))). Qed.

Lemma truthy_eq : forall w, truthy P' w = truthy P w.
Proof. intros. unfold truthy. rewrite Hbool, Htrue, Hfalse. reflexivity. Qed.
Lemma prim_at_eq : forall p first second ws st,
  BaseSites.retained_site p first = BaseSites.retained_site p second -> ok st ->
  run_prim_at P' p second ws st = run_prim_at P p first ws st.
Proof.
  intros p first second ws st Sites Valid. unfold run_prim_at.
  rewrite Hprim by exact Valid. rewrite (Hsites p first), (Hsites p second), Sites. reflexivity.
Qed.

Lemma app_env_rel : forall S d d' h th dc dc', agree S d d' -> lrel dc dc' ->
  lrel (application_environment P d h th dc) (application_environment P' d' h th dc').
Proof.
  intros S d d' h th dc dc' Hd Hc. unfold application_environment. rewrite Hfun.
  replace (ground d') with (ground d) by (unfold ground; rewrite (ag_tm Hd); reflexivity).
  replace (output_meaning dc') with (output_meaning dc).
  2:{ unfold output_meaning. rewrite (lrel_tm Hc), (lrel_ta Hc). reflexivity. }
  destruct (view_functor P h) as [code|]; simpl; apply lrel_make; exact Hc.
Qed.

Lemma rsel_nonU : forall rs rms sources q, rs_ok U rs = true ->
  In q (result_selection rs rms sources) -> isU U (fst q) = false.
Proof.
  intros rs rms sources [a b] Hr Hq. unfold result_selection in Hq. rewrite <- in_rev in Hq.
  apply in_combine_l in Hq. exact (@rs_ok_in U rs a Hr Hq).
Qed.

Lemma select_nonU : forall S d d' sm, agree S d d' -> lk S (srcs sm) ->
  (forall q, In q sm -> isU U (fst q) = false) ->
  oprel lrel (select_provisions d sm) (select_provisions d' sm).
Proof.
  intros S d d' sm Hd. induction sm as [|[req s] r IH]; simpl; intros Hs Hk.
  - exact (lrel_empty U H).
  - assert (Hr : lk S (srcs r)) by (intros x Hx; apply Hs; simpl; auto).
    specialize (IH Hr (fun q Hq => Hk q (or_intror Hq))).
    pose proof (ag_look Hd s (Hs s (or_introl eq_refl))) as Ho.
    destruct (lookup_provision s d) as [a|], (lookup_provision s d') as [a'|];
      simpl in Ho; try contradiction;
      destruct (select_provisions d r), (select_provisions d' r); simpl in IH |- *;
      try contradiction; auto.
    destruct Ho as [Ha _]. apply lrel_install; auto. apply kgood_nonU. exact (Hk _ (or_introl eq_refl)).
Qed.

Lemma lk_app_l : forall S a b, lk S (a ++ b) -> lk S a.
Proof. intros S a b Hl x Hx. apply Hl, in_or_app; auto. Qed.
Lemma lk_app_r : forall S a b, lk S (a ++ b) -> lk S b.
Proof. intros S a b Hl x Hx. apply Hl, in_or_app; auto. Qed.
Lemma lk_full : forall l, lk full l.
Proof. intros l x _. exact I. Qed.

Ltac lks := repeat match goal with
  | Hl : lk _ (_ ++ _) |- _ =>
      let H1 := fresh "Hl" in let H2 := fresh "Hl" in
      pose proof (lk_app_l _ _ _ Hl) as H1; pose proof (lk_app_r _ _ _ Hl) as H2; clear Hl
  | Hl : lk ?S (?x :: ?l) |- _ =>
      let H1 := fresh "Hl" in let H2 := fresh "Hl" in
      assert (H1 : S x) by (apply Hl; left; reflexivity);
      assert (H2 : lk S l) by (intros ? ?; apply Hl; right; assumption); clear Hl
  end.

(* What the original's application of F computes. *)
Lemma uapp : forall j d th rs st dc,
  lookup_provision (IFun U) d = Some (FunctorClosure H None dc None) ->
  apply_functor (S j) P d U th [] [] rs st =
  if Nat.ltb (length bsH) j then inl (EnvironmentWithAliases [] [] (map (site_qent cH (application_environment P d H th (append_environment empty_environment dc)) true) (sel cH rs)), st)
  else inr (Evaluation OutOfFuel [] st).
Proof.
  intros j d th rs st dc Hl. cbn [apply_functor]. rewrite Hl, HH. cbn [select_provisions adapt_provisions].
  rewrite (binds_prims P bsH j _ [] st Hprims).
  destruct (Nat.ltb (length bsH) j); [|reflexivity].
  unfold result_selection. cbn [closure_sources]. fold (sel cH rs).
  assert (Hs : forall Q, (forall q, In q Q -> In (snd q) (fc_sources cH)) ->
    select_provisions (installs (application_environment P d H th (append_environment empty_environment dc)) bsH) Q
    = Some (EnvironmentWithAliases [] [] (map (site_qent cH (application_environment P d H th (append_environment empty_environment dc)) true) Q))).
  { induction Q as [|[rq sq] r IH]; intros HQ; [reflexivity|]. cbn [select_provisions].
    rewrite lookup_installs.
    rewrite runtime_lookup. rewrite (hent_bound _ sq Hprims (Hcover _ (HQ (rq, sq) (or_introl eq_refl)))).
    rewrite IH by (intros; apply HQ; simpl; auto). reflexivity. }
  rewrite Hs by (intros q Hq; exact (proj2 (@sel_in cH _ q Hq))). reflexivity.
Qed.

Lemma orig_app_fun : forall k d g st th rs e1 dc,
  lookup_provision (IFun U) d = Some (FunctorClosure H None dc None) ->
  eval (S (S (S k))) P d g st (EBind (BApply U th [] [] rs) e1) =
  if Nat.ltb (length bsH) k
  then eval (S (S k)) P (append_environment (EnvironmentWithAliases [] [] (map (site_qent cH (application_environment P d H th (append_environment empty_environment dc)) true) (sel cH rs))) d) g st e1
  else Evaluation OutOfFuel g st.
Proof.
  intros k d g st th rs e1 dc Hl. cbn [eval eval_bindings]. rewrite (@uapp k d th rs st dc Hl).
  destruct (Nat.ltb (length bsH) k); reflexivity.
Qed.

Lemma orig_app_none : forall k d g st th rs e1,
  lookup_provision (IFun U) d = None ->
  eval (S (S (S k))) P d g st (EBind (BApply U th [] [] rs) e1) = Evaluation (Stuck e_fun) g st.
Proof. intros k d g st th rs e1 Hl. cbn [eval eval_bindings apply_functor]. rewrite Hl. reflexivity. Qed.

Lemma orig_app_small : forall k d g st th rs e1, k <= 2 ->
  oof (eval k P d g st (EBind (BApply U th [] [] rs) e1)).
Proof. intros [|[|[|k]]] d g st th rs e1 Hk; try lia; reflexivity. Qed.

Lemma orig_bapp_fun : forall k d g st th rs r dc,
  lookup_provision (IFun U) d = Some (FunctorClosure H None dc None) ->
  eval_bindings (S (S k)) P d g st (BApply U th [] [] rs :: r) =
  if Nat.ltb (length bsH) k
  then eval_bindings (S k) P (append_environment (EnvironmentWithAliases [] [] (map (site_qent cH (application_environment P d H th (append_environment empty_environment dc)) true) (sel cH rs))) d) g st r
  else inr (Evaluation OutOfFuel g st).
Proof.
  intros k d g st th rs r dc Hl. cbn [eval_bindings]. rewrite (@uapp k d th rs st dc Hl).
  destruct (Nat.ltb (length bsH) k); reflexivity.
Qed.

Lemma orig_bapp_none : forall k d g st th rs r,
  lookup_provision (IFun U) d = None ->
  eval_bindings (S (S k)) P d g st (BApply U th [] [] rs :: r) = inr (Evaluation (Stuck e_fun) g st).
Proof. intros k d g st th rs r Hl. cbn [eval_bindings apply_functor]. rewrite Hl. reflexivity. Qed.

Lemma installs_qents : forall d Q,
  installs d (prims_of cH Q) = EnvironmentWithAliases (type_meanings d) (type_aliases d) (map (site_qent cH d false) Q ++ provision_values d).
Proof. intros d Q. unfold installs, prims_of. rewrite map_rev, rev_involutive, map_map. reflexivity. Qed.

Lemma sel_nonU : forall rs, rs_ok U rs = true -> forall q, In q (sel cH rs) -> isU U (fst q) = false.
Proof. intros rs Hr q Hq. exact (@rs_ok_in U rs _ Hr (proj1 (@sel_in cH rs q Hq))). Qed.

Lemma select_arm_fp : forall arms w,
  select_arm (map (fun p => (fst p, T (snd p))) arms) w
  = option_map (fun q => (fst q, T (snd q))) (select_arm arms w).
Proof.
  induction arms as [|[pt eb] r IH]; intros w; simpl; auto.
  destruct (match_pat pt w); simpl; auto.
Qed.
Lemma select_arm_in : forall (arms : list (pattern * expr)) w bs eb,
  select_arm arms w = Some (bs, eb) -> exists pt, In (pt, eb) arms.
Proof.
  induction arms as [|[pt b] r IH]; intros w bs eb Ha; simpl in *; [discriminate|].
  destruct (match_pat pt w); [injection Ha as <- <-; eauto| |discriminate].
  destruct (IH w bs eb Ha) as [pt' Hin]; eauto.
Qed.
Lemma arms_ok : forall (arms : list (pattern * expr)) pt eb, In (pt, eb) arms ->
  forallb (fun p => ok_e U (snd p)) arms = true -> ok_e U eb = true.
Proof.
  intros arms pt eb Hin Hc. rewrite forallb_forall in Hc. exact (Hc _ Hin).
Qed.
Lemma arms_lk : forall S (arms : list (pattern * expr)) pt eb, In (pt, eb) arms ->
  lk S (flat_map (fun p => uses (snd p)) arms) -> lk S (uses eb).
Proof.
  intros S arms pt eb Hin Hl x Hx. apply Hl, in_flat_map. exists (pt, eb). auto.
Qed.
Lemma map_snd_fp : forall (fs : list (fld * expr)),
  map snd (map (fun p => (fst p, T (snd p))) fs) = map T (map snd fs).
Proof. intros. rewrite !map_map. reflexivity. Qed.
Lemma map_fst_fp : forall (fs : list (fld * expr)),
  map fst (map (fun p => (fst p, T (snd p))) fs) = map fst fs.
Proof. intros. rewrite map_map. reflexivity. Qed.
Lemma forallb_snd : forall (fs : list (fld * expr)),
  forallb (fun p => ok_e U (snd p)) fs = forallb (ok_e U) (map snd fs).
Proof. intros. induction fs; simpl; congruence. Qed.
Lemma flat_map_snd : forall (fs : list (fld * expr)),
  flat_map (fun p => uses (snd p)) fs = flat_map uses (map snd fs).
Proof. intros. induction fs; simpl; congruence. Qed.

Lemma isU_U : isU U (IFun U) = true.
Proof. unfold isU. simpl. apply Nat.eqb_refl. Qed.
Lemma isU_fun : forall u, Nat.eqb u U = false -> isU U (IFun u) = false.
Proof. intros u E. unfold isU. simpl. exact E. Qed.

Lemma lk_single : forall S l, lk S l -> lk S (l ++ []).
Proof. intros S l Hl. rewrite app_nil_r. exact Hl. Qed.

Lemma fp_e_bind : forall b e1, T (EBind b e1) =
  match fast_apply U cH b with
  | Some Q => chain (prims_of cH (filter (fun q => keepf (uses e1) Q (fst q)) Q)) (T e1)
  | None => EBind (Tb b) (T e1)
  end.
Proof. reflexivity. Qed.

Lemma fp_b_value : forall v e1, Tb (BValue v e1) = BValue v (T e1).
Proof. reflexivity. Qed.

Lemma Tbs_none : forall b, fast_apply U cH b = None -> Tbs [b] = [Tb b].
Proof. intros b E. unfold fp_bs. simpl. rewrite E. reflexivity. Qed.

Definition evq {A B} (R : A -> B -> Prop) (f : nat -> A) (b : B) : Prop :=
  exists a, R a b /\ evf (fun j => f j = a).

Definition brel (S : item -> Prop) (a b : (environment * locals * mstate) + evaluation) : Prop :=
  match a, b with
  | inl (d', g', st'), inl (d, g, st) => agree S d d' /\ g' = g /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.
Definition arel (a b : (environment * mstate) + evaluation) : Prop :=
  match a, b with
  | inl (d', st'), inl (d, st) => lrel d d' /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition FWD (k : nat) : Prop :=
  (forall W d d' g st e, ok_e U e = true -> lk W (uses e) -> agree W d d' -> ok st ->
     bad (eval k P d g st e) \/ evf (fun j => eval j P' d' g st (T e) = eval k P d g st e))
  /\ (forall W d d' g st l, forallb (ok_e U) l = true -> lk W (flat_map uses l) -> agree W d d' -> ok st ->
     badl (eval_arguments k P d g st l)
     \/ evf (fun j => eval_arguments j P' d' g st (map T l) = eval_arguments k P d g st l))
  /\ (forall W d d' g st bs, forallb (ok_b U) bs = true -> lk W (flat_map buses bs) -> agree W d d' -> ok st ->
     badl (eval_bindings k P d g st bs)
     \/ evq (brel W) (fun j => eval_bindings j P' d' g st (Tbs bs)) (eval_bindings k P d g st bs))
  /\ (forall W d d' u th outputs sm rs st, W (IFun u) -> lk W (srcs sm) -> sm_ok U sm = true ->
     rs_ok U rs = true -> agree W d d' -> ok st ->
     badl (apply_functor k P d u th outputs sm rs st)
     \/ evq arel (fun j => apply_functor j P' d' u th outputs sm rs st) (apply_functor k P d u th outputs sm rs st)).

(* one binding step of the transformed run, then the rest *)
Ltac step_rest x Hx kr Hkr :=
  right; exists x; split; [exact Hx|]; evgo; reflexivity.

Lemma fwd_step : forall k, FWD k -> FWD (S k).
Proof.
  intros k [IHe [IHl [IHb IHa]]].
  split; [|split; [|split]].
  - intros W d d' g st e Hc Hl Hd Hst.
    destruct e as [x| |v|n th e1|f th sm args|f args|e0 m it args|x e1 e2|bd e1|e1 e2|x e1
                  |c e1 e2|c e1|e1|e1| |fs|e1 fl|e0 arms];
      cbn [ok_e] in Hc; andb_split; cbn [uses] in Hl; lks.
    + (* EVar *) err.
    + (* EUnit *) err.
    + (* EVal *)
      pose proof (ag_look Hd (IVal v) ltac:(assumption)) as Ho. red1.
      destruct (lookup_provision (IVal v) d) as [a|] eqn:El,
               (lookup_provision (IVal v) d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|err].
      destruct Ho as [Ha _]. inversion Ha; subst; err.
    + (* ETag *)
      destruct (IHe W d d' g st e1 Hc ltac:(assumption) Hd Hst) as [Hs|[k1 Hk1]]; [red1; bk|].
      red1. destruct (eval k P d g st e1) as [[w| | | | | | |] g1 st1]; err.
    + (* ECall *)
      red1. destruct (view_function P f) as [body|] eqn:Ef.
      2:{ assert (Ef' : view_function P' f = None) by (rewrite Hfn, Ef; reflexivity). err. }
      assert (Ef' : view_function P' f = Some (T body)) by (rewrite Hfn, Ef; reflexivity).
      destruct (IHl W d d' g st args ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[ka Hka]]; [bk|].
      pose proof (inv_l P Hok k d g st args Hst) as Hn.
      destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|r1]; simpl in Hn; [|err].
      pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
      destruct (select_provisions d sm) as [dc|] eqn:Ec, (select_provisions d' sm) as [dc'|] eqn:Ec';
        simpl in Hsel; try contradiction; [|err].
      destruct (IHe full (call_environment d th dc) (call_environment d' th dc') (parameter_values ws) st1 body
                  (Hokf _ _ Ef) (lk_full _) (lrel_agree full (lrel_call th Hd Hsel)) Hn) as [Hs|[kb Hkb]].
      * left. apply ret_call_bad. exact Hs.
      * err.
    + (* EInvoke *)
      pose proof (ag_look Hd (IFn f) ltac:(assumption)) as Ho. red1.
      destruct (lookup_provision (IFn f) d) as [a|] eqn:El,
               (lookup_provision (IFn f) d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|err].
      destruct Ho as [Ha _]. inversion Ha as [w|f' df df' Hdf|h sms dc dc' rms Hsm Hdc|p ot ot' Hsite]; subst; try err.
      * destruct (view_function P f') as [body|] eqn:Ef.
        2:{ assert (Ef' : view_function P' f' = None) by (rewrite Hfn, Ef; reflexivity). err. }
        assert (Ef' : view_function P' f' = Some (T body)) by (rewrite Hfn, Ef; reflexivity).
        destruct (IHl W d d' g st args ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[ka Hka]]; [bk|].
        pose proof (inv_l P Hok k d g st args Hst) as Hn.
        destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|r1]; simpl in Hn; [|err].
        destruct (IHe full df df' (parameter_values ws) st1 body (Hokf _ _ Ef) (lk_full _)
                    (lrel_agree full Hdf) Hn) as [Hs|[kb Hkb]].
        -- left. apply ret_call_bad. exact Hs.
        -- err.
      * destruct (IHl W d d' g st args ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[ka Hka]]; [bk|].
        pose proof (inv_l P Hok k d g st args Hst) as Hn.
        destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|r1]; simpl in Hn; [|err].
        right. evgo. rewrite (prim_at_eq p ot ot' ws st1 Hsite Hn). reflexivity.
    + (* EMethod *)
      pose proof (ag_look Hd it ltac:(assumption)) as Ho. red1.
      destruct (lookup_provision it d) as [a|] eqn:El,
               (lookup_provision it d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|err].
      destruct Ho as [Ha _]. inversion Ha as [w|f' df df' Hdf|h sms dc dc' rms Hsm Hdc|p ot ot' Hsite]; subst; try err.
      * destruct (view_function P f') as [body|] eqn:Ef.
        2:{ assert (Ef' : view_function P' f' = None) by (rewrite Hfn, Ef; reflexivity). err. }
        assert (Ef' : view_function P' f' = Some (T body)) by (rewrite Hfn, Ef; reflexivity).
        destruct (IHe W d d' g st e0 ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k0 Hk0]]; [bk|].
        pose proof (inv_e P Hok k d g st e0 Hst) as Hn0.
        destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try err.
        destruct (IHl W d d' g0 st0 args ltac:(eassumption) ltac:(eassumption) Hd Hn0) as [Hs|[ka Hka]]; [bk|].
        pose proof (inv_l P Hok k d g0 st0 args Hn0) as Hn.
        destruct (eval_arguments k P d g0 st0 args) as [[[ws g1] st1]|r1]; simpl in Hn; [|err].
        destruct (IHe full df df' (parameter_values (w0 :: ws)) st1 body (Hokf _ _ Ef) (lk_full _)
                    (lrel_agree full Hdf) Hn) as [Hs|[kb Hkb]].
        -- left. apply ret_call_bad. exact Hs.
        -- err.
      * destruct (IHe W d d' g st e0 ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k0 Hk0]]; [bk|].
        pose proof (inv_e P Hok k d g st e0 Hst) as Hn0.
        destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try err.
        destruct (IHl W d d' g0 st0 args ltac:(eassumption) ltac:(eassumption) Hd Hn0) as [Hs|[ka Hka]]; [bk|].
        pose proof (inv_l P Hok k d g0 st0 args Hn0) as Hn.
        destruct (eval_arguments k P d g0 st0 args) as [[[ws g1] st1]|r1]; simpl in Hn; [|err].
        right. evgo. rewrite (prim_at_eq p ot ot' (w0 :: ws) st1 Hsite Hn). reflexivity.
    + (* ELet *)
      red1.
      destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|].
      pose proof (inv_e P Hok k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      destruct (IHe W d d' ((x, w0) :: g0) st0 e2 ltac:(eassumption) ltac:(eassumption) Hd Hn)
        as [Hs|[k2 Hk2]]; [bk|err].
    + (* EBind *)
      destruct (fast_apply U cH bd) as [Q|] eqn:Efa.
      * (* an application of F, narrowed *)
        destruct (fast_apply_some _ _ _ Efa) as [th [rs [-> [-> [HQ Sites]]]]].
        cbn [ok_b] in *. andb_split. cbn [buses srcs map] in *. lks.
        cbn [fp_e]. rewrite Efa.
        pose proof (ag_look Hd (IFun U) ltac:(assumption)) as Ho.
        destruct k as [|[|k]]; [left; right; reflexivity|left; right; reflexivity|].
        destruct (lookup_provision (IFun U) d) as [a|] eqn:El.
        2:{ left. left. rewrite (orig_app_none k d g st th rs e1 El). eexists; reflexivity. }
        destruct (lookup_provision (IFun U) d') as [a'|]; simpl in Ho; try contradiction.
        destruct Ho as [_ Hg]. destruct (Hg isU_U) as [dc ->].
        rewrite (orig_app_fun k d g st th rs e1 dc El).
        destruct (Nat.ltb (length bsH) k) eqn:Elt; [|left; right; reflexivity].
        set (Q := sel cH rs) in *.
        set (kp := keepf (uses e1) Q).
        pose proof (@agree_narrow_sites U cH H P W d d' Q kp
          (append_environment empty_environment dc) th HH
          Hd (sel_nonU rs ltac:(assumption)) Sites) as Hd1.
        assert (Hlk1 : lk (fun i => if existsb (item_eqb i) (map fst Q) then kp i = true else W i) (uses e1)).
        { intros i Hi. destruct (existsb (item_eqb i) (map fst Q)) eqn:E.
          - apply keepf_sound; assumption.
          - match goal with Hx : lk W (uses e1) |- _ => apply Hx, Hi end. }
        destruct (IHe _ _ _ g st e1 ltac:(assumption) Hlk1 Hd1 Hst) as [Hs|[k1 Hk1]]; [left; exact Hs|].
        right. set (bs1 := prims_of cH (filter (fun q => kp (fst q)) Q)).
        exists (S (S (k1 + length bs1))). intros j Hj.
        replace j with (length bs1 + (j - length bs1)) by lia.
        rewrite chain_eval; [|unfold bs1; apply prims_of_prim, Hprims|clear - Hj; lia].
        unfold bs1 in *. rewrite installs_qents, map_site_qent_filter. apply Hk1. clear - Hj; lia.
      * (* any other binding *)
        cbn [fp_e]. rewrite Efa. red1.
        destruct (IHb W d d' g st [bd] ltac:(cbn; rewrite andb_true_r; assumption)
                    ltac:(cbn [flat_map]; apply lk_single; assumption) Hd Hst)
          as [Hs|[x [Hx [kb Hkb]]]]; [bk|].
        rewrite (Tbs_none _ Efa) in Hkb.
        pose proof (inv_b P Hok k d g st [bd] Hst) as Hn.
        destruct x as [[[d1' g1'] st1']|r1'], (eval_bindings k P d g st [bd]) as [[[d1 g1] st1]|r1];
          simpl in Hx, Hn; try contradiction.
        -- destruct Hx as [Hd1 [-> ->]].
           destruct (IHe W d1 d1' g1 st1 e1 ltac:(assumption) ltac:(assumption) Hd1 Hn)
             as [Hs|[k1 Hk1]]; [left; exact Hs|err].
        -- subst. err.
    + (* ESeq *)
      red1.
      destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|].
      pose proof (inv_e P Hok k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      destruct (IHe W d d' g0 st0 e2 ltac:(eassumption) ltac:(eassumption) Hd Hn) as [Hs|[k2 Hk2]];
        [left; exact Hs|err].
    + (* EAssign *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|err].
    + (* EIf *)
      red1.
      destruct (IHe W d d' g st c ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k0 Hk0]]; [bk|].
      pose proof (inv_e P Hok k d g st c Hst) as Hn.
      destruct (eval k P d g st c) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      destruct (truthy P w0) as [[]|] eqn:Et.
      * destruct (IHe W d d' g0 st0 e1 ltac:(eassumption) ltac:(eassumption) Hd Hn) as [Hs|[k1 Hk1]];
          [left; exact Hs|].
        right. evgo. rewrite truthy_eq, Et. rwall. reflexivity.
      * destruct (IHe W d d' g0 st0 e2 ltac:(eassumption) ltac:(eassumption) Hd Hn) as [Hs|[k1 Hk1]];
          [left; exact Hs|].
        right. evgo. rewrite truthy_eq, Et. rwall. reflexivity.
      * right. evgo. rewrite truthy_eq, Et. reflexivity.
    + (* EWhile *)
      red1.
      destruct (IHe W d d' g st c ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k0 Hk0]]; [bk|].
      pose proof (inv_e P Hok k d g st c Hst) as Hn.
      destruct (eval k P d g st c) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      destruct (truthy P w0) as [[]|] eqn:Et.
      * destruct (IHe W d d' g0 st0 e1 ltac:(eassumption) ltac:(eassumption) Hd Hn) as [Hs|[k1 Hk1]]; [bk|].
        pose proof (inv_e P Hok k d g0 st0 e1 Hn) as Hn1.
        destruct (eval k P d g0 st0 e1) as [[w1| | | | | | |] g1 st1]; simpl in Hn1;
          try (right; evgo; rewrite truthy_eq, Et; rwall; reflexivity).
        destruct (IHe W d d' g1 st1 (EWhile c e1) ltac:(cbn; rewrite andb_true_iff; auto)
                    ltac:(cbn [uses]; intros ? Hx; apply in_app_or in Hx as [Hx|Hx]; auto) Hd Hn1)
          as [Hs|[k2 Hk2]]; [left; exact Hs|].
        cbn [fp_e] in Hk2.
        right. evgo. rewrite truthy_eq, Et. rwall. reflexivity.
      * right. evgo. rewrite truthy_eq, Et. reflexivity.
      * right. evgo. rewrite truthy_eq, Et. reflexivity.
    + (* ELoop *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[k0 Hk0]]; [bk|].
      pose proof (inv_e P Hok k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      destruct (IHe W d d' g0 st0 (ELoop e1) Hc ltac:(assumption) Hd Hn) as [Hs|[k1 Hk1]]; [left; exact Hs|].
      cbn [fp_e] in Hk1. err.
    + (* EReturn *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|err].
    + (* EBreak *) err.
    + (* ERecord *)
      red1. rewrite forallb_snd in Hc. rewrite flat_map_snd in *.
      destruct (IHl W d d' g st (map snd fs) Hc ltac:(assumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|].
      right. evgo. rewrite map_snd_fp, map_fst_fp. rwall. reflexivity.
    + (* EField *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|err].
    + (* EMatch *)
      red1.
      destruct (IHe W d d' g st e0 ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k0 Hk0]]; [bk|].
      pose proof (inv_e P Hok k d g st e0 Hst) as Hn.
      destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      destruct (select_arm arms w0) as [[bs eb]|] eqn:Ea.
      * destruct (select_arm_in _ _ _ _ Ea) as [pt Hin].
        destruct (IHe W d d' (bs ++ g0) st0 eb (arms_ok _ _ _ Hin ltac:(eassumption))
                    (arms_lk _ _ _ _ Hin ltac:(eassumption)) Hd Hn) as [Hs|[k1 Hk1]]; [bk|].
        right. evgo. rewrite select_arm_fp, Ea. cbn. rwall. reflexivity.
      * right. evgo. rewrite select_arm_fp, Ea. reflexivity.
  - (* eval_arguments *)
    intros W d d' g st l Hc Hl Hd Hst. destruct l as [|e1 r]; cbn [forallb flat_map] in Hc, Hl; [err|].
    andb_split. lks. red1.
    destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|].
    pose proof (inv_e P Hok k d g st e1 Hst) as Hn.
    destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
    destruct (IHl W d d' g0 st0 r ltac:(eassumption) ltac:(eassumption) Hd Hn) as [Hs|[k2 Hk2]]; [bk|].
    destruct (eval_arguments k P d g0 st0 r) as [[[ws g1] st1]|]; err.
  - (* eval_bindings *)
    intros W d d' g st bs Hc Hl Hd Hst. destruct bs as [|b r].
    { right. exists (inl (d', g, st)). split; [cbn; auto|]. exists 1. intros [|j] Hj; [lia|]. reflexivity. }
    cbn [forallb flat_map] in Hc, Hl. andb_split. lks. rewrite fp_bs_cons.
    destruct (fast_apply U cH b) as [Q|] eqn:Efa.
    + (* an application of F in a functor body: all of its members *)
      destruct (fast_apply_some _ _ _ Efa) as [th [rs [-> [-> [HQ Sites]]]]].
      cbn [ok_b] in *. andb_split. cbn [buses srcs map] in *. lks.
      pose proof (ag_look Hd (IFun U) ltac:(assumption)) as Ho.
      destruct k as [|k]; [left; cbn; right; reflexivity|].
      destruct (lookup_provision (IFun U) d) as [a|] eqn:El.
      2:{ left. rewrite (orig_bapp_none k d g st th rs r El). cbn. left. eexists; reflexivity. }
      destruct (lookup_provision (IFun U) d') as [a'|]; simpl in Ho; try contradiction.
      destruct Ho as [_ Hg]. destruct (Hg isU_U) as [dc ->].
      rewrite (orig_bapp_fun k d g st th rs r dc El).
      destruct (Nat.ltb (length bsH) k) eqn:Elt; [|left; cbn; right; reflexivity].
      assert (Hd1 : agree W
        (append_environment (EnvironmentWithAliases [] []
          (map (site_qent cH (application_environment P d H th (append_environment empty_environment dc)) true) (sel cH rs))) d)
        (append_environment (EnvironmentWithAliases [] [] (map (site_qent cH d' false) (sel cH rs))) d')).
      { apply agree_append; [|exact Hd]. eapply lrel_site_qents;
        [exact HH|exact (ag_tm Hd)|apply sel_nonU; assumption|exact Sites]. }
      destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption) Hd1 Hst) as [Hs|[x [Hx [kr Hkr]]]];
        [left; exact Hs|].
      right. exists x. split; [exact Hx|].
      exists (S (S (kr + length (prims_of cH (sel cH rs))))). intros j Hj.
      replace j with (length (prims_of cH (sel cH rs)) + (j - length (prims_of cH (sel cH rs)))) by lia.
      rewrite prims_app by (apply prims_of_prim, Hprims).
      rewrite installs_qents. apply Hkr. lia.
    + cbn [app].
      destruct b as [t tau|v e1|f f' types sm|tk m th f' types sm|u h types sm smc|u u' sm rm
                    |u th outputs sm rs|i p]; cbn [ok_b buses fp_b] in *; andb_split; lks.
      * (* BType *)
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption) (agree_bind_type t tau Hd) Hst)
          as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
        right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BValue *)
        red1.
        destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst) as [Hs|[k1 Hk1]]; [bk|].
        pose proof (inv_e P Hok k d g st e1 Hst) as Hn.
        destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errq.
        destruct (IHb W _ _ g0 st0 r ltac:(assumption) ltac:(assumption)
                    (agree_install (PVal U H w0) (kgood_nonU U H (IVal v) _ eq_refl) Hd) Hn)
          as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
        right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BFunction *)
        red1. pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
        destruct (select_provisions d sm) as [dc|] eqn:Ec, (select_provisions d' sm) as [dc'|] eqn:Ec';
          simpl in Hsel; try contradiction; [|stuck_here].
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (PClo f' (lrel_capture types Hd Hsel)) (kgood_nonU U H (IFn f) _ eq_refl) Hd) Hst)
          as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
        right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BMethod *)
        red1. pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
        destruct (select_provisions d sm) as [dc|] eqn:Ec, (select_provisions d' sm) as [dc'|] eqn:Ec';
          simpl in Hsel; try contradiction; [|stuck_here].
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (PClo f' (lrel_capture types Hd Hsel)) (kgood_nonU U H (IMth tk m th) _ eq_refl) Hd) Hst)
          as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
        right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BFunctor *)
        red1. pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
        destruct (select_provisions d smc) as [dc|] eqn:Ec, (select_provisions d' smc) as [dc'|] eqn:Ec';
          simpl in Hsel; try contradiction; [|stuck_here].
        assert (Hu : isU U (IFun u) = false) by (apply isU_fun, negb_true_iff; assumption).
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (@PFun U H h (Some sm) _ _ None
                                     ltac:(cbn; assumption) (lrel_capture types Hd Hsel))
                       (kgood_nonU U H _ _ Hu) Hd) Hst)
          as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
        right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BCopyFunctor *)
        red1. pose proof (ag_look Hd (IFun u') ltac:(assumption)) as Ho.
        destruct (lookup_provision (IFun u') d) as [a|] eqn:El,
                 (lookup_provision (IFun u') d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|stuck_here].
        destruct Ho as [Ha _].
        inversion Ha as [w|f0 df df' Hdf|h sms dc dc' rms Hsm Hdc|p ot ot' Hsite]; subst; try stuck_here.
        assert (Hu : isU U (IFun u) = false) by (apply isU_fun, negb_true_iff; assumption).
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (@PFun U H h (compose_adaptation sms sm) _ _ (compose_positions rms rm)
                                     ltac:(apply osm_ok_compose; assumption) Hdc)
                       (kgood_nonU U H _ _ Hu) Hd) Hst)
          as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
        right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BApply, any other *)
        red1.
        destruct (IHa W d d' u th outputs sm rs st ltac:(assumption) ltac:(assumption) ltac:(assumption)
                    ltac:(assumption) Hd Hst) as [Hs|[x [Hx [ka Hka]]]]; [bk|].
        pose proof (inv_a P Hok k d u th outputs sm rs st Hst) as Hn.
        destruct x as [[dr' st1']|r1'], (apply_functor k P d u th outputs sm rs st) as [[dr st1]|r1];
          simpl in Hx, Hn; try contradiction.
        -- destruct Hx as [Hdr ->].
           destruct (IHb W _ _ g st1 r ltac:(assumption) ltac:(assumption) (agree_append Hdr Hd) Hn)
             as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
           right. exists x. split; [exact Hx|]. evgo. reflexivity.
        -- subst. errq.
      * (* BPrimitive *)
        red1. pose proof (prim_closure_eq i p Hd) as Epc.
        assert (Hpc : prel U H (primitive_closure d i p) (primitive_closure d' i p))
          by (rewrite <- Epc; apply prel_prim_closure).
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install Hpc
                       (kgood_nonU U H i _ ltac:(apply negb_true_iff; assumption)) Hd) Hst)
          as [Hs|[x [Hx [kr Hkr]]]]; [left; exact Hs|].
        right. exists x. split; [exact Hx|]. evgo. reflexivity.
  - (* apply_functor *)
    intros W d d' u th outputs sm rs st Hu Hs Hsm Hrs Hd Hst. red1.
    pose proof (ag_look Hd (IFun u) Hu) as Ho.
    destruct (lookup_provision (IFun u) d) as [a|] eqn:El,
             (lookup_provision (IFun u) d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|errq].
    destruct Ho as [Ha _]. inversion Ha as [w|f0 df df' Hdf|h sms dc dc' rms Hsms Hdc|p ot ot' Hsite]; subst; try errq.
    destruct (view_functor P h) as [code|] eqn:Eh.
    2:{ assert (Eh' : view_functor P' h = None) by (rewrite Hfun, Eh; reflexivity). errq. }
    assert (Eh' : view_functor P' h = Some (fp_code U cH code)) by (rewrite Hfun, Eh; reflexivity).
    pose proof (all_select _ Hd Hs Hsm) as Hs1.
    destruct (select_provisions d sm) as [d1|] eqn:E1, (select_provisions d' sm) as [d1'|] eqn:E1';
      simpl in Hs1; try contradiction; [|errq].
    pose proof (lrel_adapt sms Hs1 Hsms) as Hs2.
    destruct (adapt_provisions d1 sms) as [d2|] eqn:E2, (adapt_provisions d1' sms) as [d2'|] eqn:E2';
      simpl in Hs2; try contradiction; [|errq].
    assert (Henv : lrel (application_environment P d h th (append_environment d2 dc))
                        (application_environment P' d' h th (append_environment d2' dc'))).
    { eapply app_env_rel; [exact Hd|]. apply lrel_append; assumption. }
    destruct (IHb full _ _ [] st (fc_bindings code) (Hoku _ _ Eh) (lk_full _) (lrel_agree full Henv) Hst)
      as [Hb|[x [Hx [kb Hkb]]]]; [bk|].
    destruct x as [[[d3' g3'] st3']|r'],
             (eval_bindings k P (application_environment P d h th (append_environment d2 dc)) [] st
                (fc_bindings code)) as [[[d3 g3] st3]|r]; simpl in Hx; try contradiction.
    + destruct Hx as [Hd3 [_ ->]].
      pose proof (select_nonU full d3 d3' (result_selection rs rms (fc_sources code)) Hd3 (lk_full _)
                  (fun q Hq => rsel_nonU rs rms (fc_sources code) q Hrs Hq)) as Hr.
      destruct (select_provisions d3 (result_selection rs rms (fc_sources code))) as [dr|] eqn:Er,
               (select_provisions d3' (result_selection rs rms (fc_sources code))) as [dr'|] eqn:Er';
        simpl in Hr; try contradiction; [|errq].
      pose proof (result_env_rel outputs (fc_outputs code) Hd3 Hr) as Q.
      destruct (result_environment outputs d3 (fc_outputs code) dr) as [x|] eqn:Ex,
               (result_environment outputs d3' (fc_outputs code) dr') as [x'|] eqn:Ex';
        simpl in Q; try contradiction; [|errq].
      right. exists (inl (x', st3)). split; [cbn; auto|]. evgo. reflexivity.
    + subst. errq.
Qed.

Lemma fwd_all : forall k, FWD k.
Proof.
  induction k as [|k IH]; [|exact (fwd_step k IH)].
  repeat split; intros; left; cbn; unfold bad, oof; right; reflexivity.
Qed.

(* ----- the other direction: the transformed run at a fixed fuel ----- *)

Definition evo {B} (R : B -> Prop) (f : nat -> B) : Prop := exists b, R b /\ evf (fun j => f j = b).

Definition BWD (k : nat) : Prop :=
  (forall W d d' g st e, ok_e U e = true -> lk W (uses e) -> agree W d d' -> ok st ->
     oof (eval k P' d' g st (T e))
     \/ evf (fun j => stuck (eval j P d g st e))
     \/ evf (fun j => eval j P d g st e = eval k P' d' g st (T e)))
  /\ (forall W d d' g st l, forallb (ok_e U) l = true -> lk W (flat_map uses l) -> agree W d d' -> ok st ->
     oofl (eval_arguments k P' d' g st (map T l))
     \/ evf (fun j => stuckl (eval_arguments j P d g st l))
     \/ evf (fun j => eval_arguments j P d g st l = eval_arguments k P' d' g st (map T l)))
  /\ (forall W d d' g st bs, forallb (ok_b U) bs = true -> lk W (flat_map buses bs) -> agree W d d' -> ok st ->
     oofl (eval_bindings k P' d' g st (Tbs bs))
     \/ evf (fun j => stuckl (eval_bindings j P d g st bs))
     \/ evo (brel W (eval_bindings k P' d' g st (Tbs bs))) (fun j => eval_bindings j P d g st bs))
  /\ (forall W d d' u th outputs sm rs st, W (IFun u) -> lk W (srcs sm) -> sm_ok U sm = true ->
     rs_ok U rs = true -> agree W d d' -> ok st ->
     oofl (apply_functor k P' d' u th outputs sm rs st)
     \/ evf (fun j => stuckl (apply_functor j P d u th outputs sm rs st))
     \/ evo (arel (apply_functor k P' d' u th outputs sm rs st)) (fun j => apply_functor j P d u th outputs sm rs st)).

(* the transformed sub-evaluation ran out of fuel: so did the whole *)
Ltac ofB :=
  match goal with
  | Hs : oof ?r |- _ =>
      let o0 := fresh "o" in let g0 := fresh "g" in let s0 := fresh "s" in
      left; revert Hs; destruct r as [o0 g0 s0]; unfold oof; simpl; intros ->; reflexivity
  | Hs : oofl ?r |- _ =>
      let o0 := fresh "o" in let g0 := fresh "g" in let s0 := fresh "s" in
      left; revert Hs; destruct r as [?|[o0 g0 s0]]; simpl; [intros []|];
      unfold oof; simpl; intros ->; reflexivity
  end.
(* the original's sub-evaluation is eventually stuck: so is the whole *)
Ltac skB :=
  match goal with
  | Hs : forall j, ?ks <= j -> stuck _ |- _ =>
      right; left; let s := sumk 0 in exists (S s);
      let j := fresh "j" in let Hj := fresh "Hj" in
      intros j Hj; destruct j as [|j]; [lia|]; red1; rwall;
      specialize (Hs j ltac:(lia));
      match type of Hs with stuck ?r =>
        let o0 := fresh "o" in let g0 := fresh "g" in let s0 := fresh "s" in let c0 := fresh "c" in
        revert Hs; destruct r as [o0 g0 s0]; unfold stuck; simpl; intros [c0 ->]; simpl;
        eexists; reflexivity end
  | Hs : forall j, ?ks <= j -> stuckl _ |- _ =>
      right; left; let s := sumk 0 in exists (S s);
      let j := fresh "j" in let Hj := fresh "Hj" in
      intros j Hj; destruct j as [|j]; [lia|]; red1; rwall;
      specialize (Hs j ltac:(lia));
      match type of Hs with stuckl ?r =>
        let o0 := fresh "o" in let g0 := fresh "g" in let s0 := fresh "s" in let c0 := fresh "c" in
        revert Hs; destruct r as [?|[o0 g0 s0]]; simpl; [intros []|];
        unfold stuck; simpl; intros [c0 ->]; simpl; eexists; reflexivity end
  end.
(* the original is eventually equal to the transformed result *)
Ltac errB := solve [right; right; evgo; reflexivity].
Ltac errBq := solve [right; right; eexists; split; cycle 1; [evgo; reflexivity|cbn; auto]].
Ltac stuckB := solve [right; left; evgo; cbn; unfold stuck; cbn; eexists; reflexivity].

Lemma bwd_step : forall k, (forall k', k' <= k -> BWD k') -> BWD (S k).
Proof.
  intros k IHall. destruct (IHall k (le_n k)) as [IHe [IHl [IHb IHa]]].
  split; [|split; [|split]].
  - intros W d d' g st e Hc Hl Hd Hst.
    destruct e as [x| |v|n th e1|f th sm args|f args|e0 m it args|x e1 e2|bd e1|e1 e2|x e1
                  |c e1 e2|c e1|e1|e1| |fs|e1 fl|e0 arms];
      cbn [ok_e] in Hc; andb_split; cbn [uses] in Hl; lks.
    + (* EVar *) errB.
    + (* EUnit *) errB.
    + (* EVal *)
      pose proof (ag_look Hd (IVal v) ltac:(assumption)) as Ho. red1.
      destruct (lookup_provision (IVal v) d) as [a|] eqn:El,
               (lookup_provision (IVal v) d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|errB].
      destruct Ho as [Ha _]. inversion Ha; subst; errB.
    + (* ETag *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(assumption) Hd Hst) as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|].
      destruct (eval k P' d' g st (T e1)) as [[w| | | | | | |] g1 st1]; errB.
    + (* ECall *)
      red1. rewrite Hfn. destruct (view_function P f) as [body|] eqn:Ef; cbn [option_map]; [|errB].
      destruct (IHl W d d' g st args ltac:(eassumption) ltac:(eassumption) Hd Hst)
        as [Hs|[[ks Hs]|[ka Hka]]]; [ofB|skB|].
      pose proof (inv_l P' Hok' k d' g st (map T args) Hst) as Hn.
      destruct (eval_arguments k P' d' g st (map T args)) as [[[ws g1] st1]|r1]; simpl in Hn; [|errB].
      pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
      destruct (select_provisions d sm) as [dc|] eqn:Ec, (select_provisions d' sm) as [dc'|] eqn:Ec';
        simpl in Hsel; try contradiction; [|errB].
      destruct (IHe full (call_environment d th dc) (call_environment d' th dc') (parameter_values ws) st1 body
                  (Hokf _ _ Ef) (lk_full _) (lrel_agree full (lrel_call th Hd Hsel)) Hn)
        as [Hs|[[ks Hs]|[kb Hkb]]]; [ofB|skB|errB].
    + (* EInvoke *)
      pose proof (ag_look Hd (IFn f) ltac:(assumption)) as Ho. red1.
      destruct (lookup_provision (IFn f) d) as [a|] eqn:El,
               (lookup_provision (IFn f) d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|errB].
      destruct Ho as [Ha _]. inversion Ha as [w|f' df df' Hdf|h sms dc dc' rms Hsm Hdc|p ot ot' Hsite]; subst; try errB.
      * rewrite Hfn. destruct (view_function P f') as [body|] eqn:Ef; cbn [option_map]; [|errB].
        destruct (IHl W d d' g st args ltac:(eassumption) ltac:(eassumption) Hd Hst)
          as [Hs|[[ks Hs]|[ka Hka]]]; [ofB|skB|].
        pose proof (inv_l P' Hok' k d' g st (map T args) Hst) as Hn.
        destruct (eval_arguments k P' d' g st (map T args)) as [[[ws g1] st1]|r1]; simpl in Hn; [|errB].
        destruct (IHe full df df' (parameter_values ws) st1 body (Hokf _ _ Ef) (lk_full _)
                    (lrel_agree full Hdf) Hn) as [Hs|[[ks Hs]|[kb Hkb]]]; [ofB|skB|errB].
      * destruct (IHl W d d' g st args ltac:(eassumption) ltac:(eassumption) Hd Hst)
          as [Hs|[[ks Hs]|[ka Hka]]]; [ofB|skB|].
        pose proof (inv_l P' Hok' k d' g st (map T args) Hst) as Hn.
        destruct (eval_arguments k P' d' g st (map T args)) as [[[ws g1] st1]|r1]; simpl in Hn; [|errB].
        right. right. evgo. rewrite (prim_at_eq p ot ot' ws st1 Hsite Hn). reflexivity.
    + (* EMethod *)
      pose proof (ag_look Hd it ltac:(assumption)) as Ho. red1.
      destruct (lookup_provision it d) as [a|] eqn:El,
               (lookup_provision it d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|errB].
      destruct Ho as [Ha _]. inversion Ha as [w|f' df df' Hdf|h sms dc dc' rms Hsm Hdc|p ot ot' Hsite]; subst; try errB.
      * rewrite Hfn. destruct (view_function P f') as [body|] eqn:Ef; cbn [option_map]; [|errB].
        destruct (IHe W d d' g st e0 ltac:(eassumption) ltac:(eassumption) Hd Hst)
          as [Hs|[[ks Hs]|[k0 Hk0]]]; [ofB|skB|].
        pose proof (inv_e P' Hok' k d' g st (T e0) Hst) as Hn0.
        destruct (eval k P' d' g st (T e0)) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try errB.
        destruct (IHl W d d' g0 st0 args ltac:(eassumption) ltac:(eassumption) Hd Hn0)
          as [Hs|[[ks Hs]|[ka Hka]]]; [ofB|skB|].
        pose proof (inv_l P' Hok' k d' g0 st0 (map T args) Hn0) as Hn.
        destruct (eval_arguments k P' d' g0 st0 (map T args)) as [[[ws g1] st1]|r1]; simpl in Hn; [|errB].
        destruct (IHe full df df' (parameter_values (w0 :: ws)) st1 body (Hokf _ _ Ef) (lk_full _)
                    (lrel_agree full Hdf) Hn) as [Hs|[[ks Hs]|[kb Hkb]]]; [ofB|skB|errB].
      * destruct (IHe W d d' g st e0 ltac:(eassumption) ltac:(eassumption) Hd Hst)
          as [Hs|[[ks Hs]|[k0 Hk0]]]; [ofB|skB|].
        pose proof (inv_e P' Hok' k d' g st (T e0) Hst) as Hn0.
        destruct (eval k P' d' g st (T e0)) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try errB.
        destruct (IHl W d d' g0 st0 args ltac:(eassumption) ltac:(eassumption) Hd Hn0)
          as [Hs|[[ks Hs]|[ka Hka]]]; [ofB|skB|].
        pose proof (inv_l P' Hok' k d' g0 st0 (map T args) Hn0) as Hn.
        destruct (eval_arguments k P' d' g0 st0 (map T args)) as [[[ws g1] st1]|r1]; simpl in Hn; [|errB].
        right. right. evgo. rewrite (prim_at_eq p ot ot' (w0 :: ws) st1 Hsite Hn). reflexivity.
    + (* ELet *)
      red1.
      destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst)
        as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|].
      pose proof (inv_e P' Hok' k d' g st (T e1) Hst) as Hn.
      destruct (eval k P' d' g st (T e1)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errB.
      destruct (IHe W d d' ((x, w0) :: g0) st0 e2 ltac:(eassumption) ltac:(eassumption) Hd Hn)
        as [Hs|[[ks Hs]|[k2 Hk2]]]; [ofB|skB|errB].
    + (* EBind *)
      destruct (fast_apply U cH bd) as [Q|] eqn:Efa.
      * (* an application of F, narrowed *)
        destruct (fast_apply_some _ _ _ Efa) as [th [rs [-> [-> [HQ Sites]]]]].
        cbn [ok_b] in *. andb_split. cbn [buses srcs map] in *. lks.
        rewrite fp_e_bind, Efa.
        set (Q := sel cH rs) in *.
        set (kp := keepf (uses e1) Q).
        set (bs1 := prims_of cH (filter (fun q => kp (fst q)) Q)).
        assert (Hbs1 : forallb body_prim bs1 = true) by (apply prims_of_prim, Hprims).
        assert (Hne : bs1 <> []).
        { unfold bs1, prims_of. intro E. apply (f_equal (@rev _)) in E. rewrite rev_involutive in E.
          apply map_eq_nil in E. exact (keepf_nonempty (uses e1) HQ E). }
        assert (Hlen : 1 <= length bs1) by (destruct bs1; [contradiction|simpl; lia]).
        destruct (Nat.ltb (S k) (length bs1 + 2)) eqn:Eshort.
        { left. apply chain_short; auto. apply Nat.ltb_lt, Eshort. }
        apply Nat.ltb_ge in Eshort.
        replace (S k) with (length bs1 + (S k - length bs1)) by lia.
        rewrite chain_eval by (auto; lia).
        unfold bs1. rewrite installs_qents, map_site_qent_filter. fold bs1.
        assert (Hlk1 : lk (fun i => if existsb (item_eqb i) (map fst Q) then kp i = true else W i) (uses e1)).
        { intros i Hi. destruct (existsb (item_eqb i) (map fst Q)) eqn:E.
          - apply keepf_sound; assumption.
          - match goal with Hx : lk W (uses e1) |- _ => apply Hx, Hi end. }
        destruct (IHall (S k - length bs1) ltac:(lia)) as [IHe' _].
        pose proof (ag_look Hd (IFun U) ltac:(assumption)) as Ho.
        destruct (lookup_provision (IFun U) d) as [a|] eqn:El.
        2:{ right. left. exists 3. intros [|[|[|j]]] Hj; try lia.
            rewrite (orig_app_none j d g st th rs e1 El). eexists; reflexivity. }
        destruct (lookup_provision (IFun U) d') as [a'|]; simpl in Ho; try contradiction.
        destruct Ho as [_ Hg]. destruct (Hg isU_U) as [dc ->].
        pose proof (@agree_narrow_sites U cH H P W d d' Q kp
          (append_environment empty_environment dc) th HH
          Hd (sel_nonU rs ltac:(assumption)) Sites) as Hd1.
        destruct (IHe' _ _ _ g st e1 ltac:(assumption) Hlk1 Hd1 Hst) as [Hs|[[ks Hs]|[k1 Hk1]]].
        -- left. exact Hs.
        -- right. left. exists (S (S (S (S (ks + length bsH))))). intros j Hj.
           replace j with (S (S (S (j - 3)))) by (clear - Hj; lia).
           rewrite (orig_app_fun (j - 3) d g st th rs e1 dc El).
           replace (Nat.ltb (length bsH) (j - 3)) with true by (symmetry; apply Nat.ltb_lt; clear - Hj; lia).
           apply Hs. clear - Hj; lia.
        -- right. right. exists (S (S (S (S (k1 + length bsH))))). intros j Hj.
           replace j with (S (S (S (j - 3)))) by (clear - Hj; lia).
           rewrite (orig_app_fun (j - 3) d g st th rs e1 dc El).
           replace (Nat.ltb (length bsH) (j - 3)) with true by (symmetry; apply Nat.ltb_lt; clear - Hj; lia).
           apply Hk1. clear - Hj; lia.
      * (* any other binding *)
        rewrite fp_e_bind, Efa. red1.
        destruct (IHb W d d' g st [bd] ltac:(cbn; rewrite andb_true_r; assumption)
                    ltac:(cbn [flat_map]; apply lk_single; assumption) Hd Hst)
          as [Hs|[[ks Hs]|[x [Hx [kb Hkb]]]]]; try rewrite (Tbs_none _ Efa) in *; [ofB|skB|].
        pose proof (inv_b P' Hok' k d' g st [Tb bd] Hst) as Hn.
        destruct x as [[[d1 g1] st1]|r1], (eval_bindings k P' d' g st [Tb bd]) as [[[d1' g1'] st1']|r1'];
          simpl in Hx, Hn; try contradiction.
        -- destruct Hx as [Hd1 [-> ->]].
           destruct (IHe W d1 d1' g1 st1 e1 ltac:(assumption) ltac:(assumption) Hd1 Hn)
             as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|errB].
        -- subst. errB.
    + (* ESeq *)
      red1.
      destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst)
        as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|].
      pose proof (inv_e P' Hok' k d' g st (T e1) Hst) as Hn.
      destruct (eval k P' d' g st (T e1)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errB.
      destruct (IHe W d d' g0 st0 e2 ltac:(eassumption) ltac:(eassumption) Hd Hn)
        as [Hs|[[ks Hs]|[k2 Hk2]]]; [left; exact Hs|skB|errB].
    + (* EAssign *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|errB].
    + (* EIf *)
      red1.
      destruct (IHe W d d' g st c ltac:(eassumption) ltac:(eassumption) Hd Hst)
        as [Hs|[[ks Hs]|[k0 Hk0]]]; [ofB|skB|].
      pose proof (inv_e P' Hok' k d' g st (T c) Hst) as Hn.
      destruct (eval k P' d' g st (T c)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errB.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|] eqn:Et.
      * destruct (IHe W d d' g0 st0 e1 ltac:(eassumption) ltac:(eassumption) Hd Hn)
          as [Hs|[[ks Hs]|[k1 Hk1]]]; [left; exact Hs| |].
        -- right. left. evgo. rewrite Et. rwall. apply Hs. lia.
        -- right. right. evgo. rewrite Et. rwall. reflexivity.
      * destruct (IHe W d d' g0 st0 e2 ltac:(eassumption) ltac:(eassumption) Hd Hn)
          as [Hs|[[ks Hs]|[k1 Hk1]]]; [left; exact Hs| |].
        -- right. left. evgo. rewrite Et. rwall. apply Hs. lia.
        -- right. right. evgo. rewrite Et. rwall. reflexivity.
      * right. right. evgo. rewrite Et. reflexivity.
    + (* EWhile *)
      red1.
      destruct (IHe W d d' g st c ltac:(eassumption) ltac:(eassumption) Hd Hst)
        as [Hs|[[ks Hs]|[k0 Hk0]]]; [ofB|skB|].
      pose proof (inv_e P' Hok' k d' g st (T c) Hst) as Hn.
      destruct (eval k P' d' g st (T c)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errB.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|] eqn:Et.
      * destruct (IHe W d d' g0 st0 e1 ltac:(eassumption) ltac:(eassumption) Hd Hn)
          as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB| |].
        { right. left. evgo. rewrite Et. rwall.
          specialize (Hs j ltac:(lia)). revert Hs.
          destruct (eval j P d g0 st0 e1) as [o1 g1 s1]. unfold stuck. simpl.
          intros [c1 ->]. eexists; reflexivity. }
        pose proof (inv_e P' Hok' k d' g0 st0 (T e1) Hn) as Hn1.
        destruct (eval k P' d' g0 st0 (T e1)) as [[w1| | | | | | |] g1 st1]; simpl in Hn1;
          try (right; right; evgo; rewrite Et; rwall; reflexivity).
        destruct (IHe W d d' g1 st1 (EWhile c e1) ltac:(cbn; rewrite andb_true_iff; auto)
                    ltac:(cbn [uses]; intros ? Hx; apply in_app_or in Hx as [Hx|Hx]; auto) Hd Hn1)
          as [Hs|[[ks Hs]|[k2 Hk2]]]; [left; exact Hs| |].
        -- right. left. evgo. rewrite Et. rwall. apply Hs. lia.
        -- cbn [fp_e] in Hk2. right. right. evgo. rewrite Et. rwall. reflexivity.
      * right. right. evgo. rewrite Et. reflexivity.
      * right. right. evgo. rewrite Et. reflexivity.
    + (* ELoop *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[[ks Hs]|[k0 Hk0]]]; [ofB|skB|].
      pose proof (inv_e P' Hok' k d' g st (T e1) Hst) as Hn.
      destruct (eval k P' d' g st (T e1)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errB.
      destruct (IHe W d d' g0 st0 (ELoop e1) Hc ltac:(assumption) Hd Hn) as [Hs|[[ks Hs]|[k1 Hk1]]];
        cbn [fp_e] in *; [left; exact Hs| |].
      -- right. left. evgo. apply Hs. lia.
      -- errB.
    + (* EReturn *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|errB].
    + (* EBreak *) errB.
    + (* ERecord *)
      red1. rewrite forallb_snd in Hc. rewrite flat_map_snd in *. rewrite map_snd_fp, map_fst_fp.
      destruct (IHl W d d' g st (map snd fs) Hc ltac:(assumption) Hd Hst) as [Hs|[[ks Hs]|[k1 Hk1]]];
        [ofB|skB|errB].
    + (* EField *)
      red1.
      destruct (IHe W d d' g st e1 Hc ltac:(eassumption) Hd Hst) as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|errB].
    + (* EMatch *)
      red1.
      destruct (IHe W d d' g st e0 ltac:(eassumption) ltac:(eassumption) Hd Hst)
        as [Hs|[[ks Hs]|[k0 Hk0]]]; [ofB|skB|].
      pose proof (inv_e P' Hok' k d' g st (T e0) Hst) as Hn.
      destruct (eval k P' d' g st (T e0)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errB.
      rewrite select_arm_fp. destruct (select_arm arms w0) as [[bs eb]|] eqn:Ea; cbn [option_map fst snd].
      * destruct (select_arm_in _ _ _ _ Ea) as [pt Hin].
        destruct (IHe W d d' (bs ++ g0) st0 eb (arms_ok _ _ _ Hin ltac:(eassumption))
                    (arms_lk _ _ _ _ Hin ltac:(eassumption)) Hd Hn) as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB| |].
        -- right. left. evgo. rewrite Ea. cbn.
           specialize (Hs j ltac:(lia)). revert Hs.
           destruct (eval j P d (bs ++ g0) st0 eb) as [o1 g1 s1]. unfold stuck. simpl.
           intros [c1 ->]. eexists; reflexivity.
        -- right. right. evgo. rewrite Ea. cbn. rwall. reflexivity.
      * right. right. evgo. rewrite Ea. reflexivity.
  - (* eval_arguments *)
    intros W d d' g st l Hc Hl Hd Hst. destruct l as [|e1 r]; cbn [forallb flat_map] in Hc, Hl; [errB|].
    andb_split. lks. red1.
    destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst)
      as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|].
    pose proof (inv_e P' Hok' k d' g st (T e1) Hst) as Hn.
    destruct (eval k P' d' g st (T e1)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errB.
    destruct (IHl W d d' g0 st0 r ltac:(eassumption) ltac:(eassumption) Hd Hn)
      as [Hs|[[ks Hs]|[k2 Hk2]]]; [ofB|skB|].
    destruct (eval_arguments k P' d' g0 st0 (map T r)) as [[[ws g1] st1]|]; errB.
  - (* eval_bindings *)
    intros W d d' g st bs Hc Hl Hd Hst. destruct bs as [|b r].
    { right. right. exists (inl (d, g, st)). split; [cbn; auto|]. exists 1. intros [|j] Hj; [lia|]. reflexivity. }
    cbn [forallb flat_map] in Hc, Hl. andb_split. lks. rewrite fp_bs_cons.
    destruct (fast_apply U cH b) as [Q|] eqn:Efa.
    + (* an application of F in a functor body: all of its members *)
      destruct (fast_apply_some _ _ _ Efa) as [th [rs [-> [-> [HQ Sites]]]]].
      cbn [ok_b] in *. andb_split. cbn [buses srcs map] in *. lks.
      set (bs1 := prims_of cH (sel cH rs)).
      assert (Hbs1 : forallb body_prim bs1 = true) by (apply prims_of_prim, Hprims).
      assert (Hlen : 1 <= length bs1).
      { unfold bs1. rewrite prims_of_length. destruct (sel cH rs); [contradiction|simpl; lia]. }
      destruct (Nat.leb (S k) (length bs1)) eqn:Eshort.
      { left. apply Nat.leb_le in Eshort. rewrite prims_app_le by auto. reflexivity. }
      apply Nat.leb_gt in Eshort.
      replace (S k) with (length bs1 + (S k - length bs1)) by lia.
      rewrite prims_app by auto. unfold bs1. rewrite installs_qents. fold bs1.
      destruct (IHall (S k - length bs1) ltac:(lia)) as [_ [_ [IHb' _]]].
      pose proof (ag_look Hd (IFun U) ltac:(assumption)) as Ho.
      destruct (lookup_provision (IFun U) d) as [a|] eqn:El.
      2:{ right. left. exists 2. intros [|[|j]] Hj; try lia.
          rewrite (orig_bapp_none j d g st th rs r El). eexists; reflexivity. }
      destruct (lookup_provision (IFun U) d') as [a'|]; simpl in Ho; try contradiction.
      destruct Ho as [_ Hg]. destruct (Hg isU_U) as [dc ->].
      assert (Hd1 : agree W
        (append_environment (EnvironmentWithAliases [] []
          (map (site_qent cH (application_environment P d H th (append_environment empty_environment dc)) true) (sel cH rs))) d)
        (append_environment (EnvironmentWithAliases [] [] (map (site_qent cH d' false) (sel cH rs))) d')).
      { apply agree_append; [|exact Hd]. eapply lrel_site_qents;
        [exact HH|exact (ag_tm Hd)|apply sel_nonU; assumption|exact Sites]. }
      destruct (IHb' W _ _ g st r ltac:(assumption) ltac:(assumption) Hd1 Hst)
        as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]].
      * left. exact Hs.
      * right. left. exists (S (S (S (S (ks + length bsH))))). intros j Hj.
        replace j with (S (S (j - 2))) by (clear - Hj; lia).
        rewrite (orig_bapp_fun (j - 2) d g st th rs r dc El).
        replace (Nat.ltb (length bsH) (j - 2)) with true by (symmetry; apply Nat.ltb_lt; clear - Hj; lia).
        apply Hs. clear - Hj; lia.
      * right. right. exists x. split; [exact Hx|].
        exists (S (S (S (S (kr + length bsH))))). intros j Hj.
        replace j with (S (S (j - 2))) by (clear - Hj; lia).
        rewrite (orig_bapp_fun (j - 2) d g st th rs r dc El).
        replace (Nat.ltb (length bsH) (j - 2)) with true by (symmetry; apply Nat.ltb_lt; clear - Hj; lia).
        apply Hkr. clear - Hj; lia.
    + cbn [app].
      destruct b as [t tau|v e1|f f' types sm|tk m th f' types sm|u h types sm smc|u u' sm rm
                    |u th outputs sm rs|i p]; cbn [ok_b buses] in *; rewrite ?fp_b_value; cbn [fp_b]; andb_split; lks; red1.
      * (* BType *)
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption) (agree_bind_type t tau Hd) Hst)
          as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
        -- right. left. evgo. apply Hs. lia.
        -- right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BValue *)
        destruct (IHe W d d' g st e1 ltac:(eassumption) ltac:(eassumption) Hd Hst)
          as [Hs|[[ks Hs]|[k1 Hk1]]]; [ofB|skB|].
        pose proof (inv_e P' Hok' k d' g st (T e1) Hst) as Hn.
        destruct (eval k P' d' g st (T e1)) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try errBq.
        destruct (IHb W _ _ g0 st0 r ltac:(assumption) ltac:(assumption)
                    (agree_install (PVal U H w0) (kgood_nonU U H (IVal v) _ eq_refl) Hd) Hn)
          as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
        -- right. left. evgo. apply Hs. lia.
        -- right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BFunction *)
        pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
        destruct (select_provisions d sm) as [dc|] eqn:Ec, (select_provisions d' sm) as [dc'|] eqn:Ec';
          simpl in Hsel; try contradiction; [|stuckB].
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (PClo f' (lrel_capture types Hd Hsel)) (kgood_nonU U H (IFn f) _ eq_refl) Hd) Hst)
          as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
        -- right. left. evgo. apply Hs. lia.
        -- right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BMethod *)
        pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
        destruct (select_provisions d sm) as [dc|] eqn:Ec, (select_provisions d' sm) as [dc'|] eqn:Ec';
          simpl in Hsel; try contradiction; [|stuckB].
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (PClo f' (lrel_capture types Hd Hsel)) (kgood_nonU U H (IMth tk m th) _ eq_refl) Hd) Hst)
          as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
        -- right. left. evgo. apply Hs. lia.
        -- right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BFunctor *)
        pose proof (all_select _ Hd ltac:(eassumption) ltac:(eassumption)) as Hsel.
        destruct (select_provisions d smc) as [dc|] eqn:Ec, (select_provisions d' smc) as [dc'|] eqn:Ec';
          simpl in Hsel; try contradiction; [|stuckB].
        assert (Hu : isU U (IFun u) = false) by (apply isU_fun, negb_true_iff; assumption).
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (@PFun U H h (Some sm) _ _ None
                                     ltac:(cbn; assumption) (lrel_capture types Hd Hsel))
                       (kgood_nonU U H _ _ Hu) Hd) Hst)
          as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
        -- right. left. evgo. apply Hs. lia.
        -- right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BCopyFunctor *)
        pose proof (ag_look Hd (IFun u') ltac:(assumption)) as Ho.
        destruct (lookup_provision (IFun u') d) as [a|] eqn:El,
                 (lookup_provision (IFun u') d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|stuckB].
        destruct Ho as [Ha _].
        inversion Ha as [w|f0 df df' Hdf|h sms dc dc' rms Hsm Hdc|p ot ot' Hsite]; subst; try stuckB.
        assert (Hu : isU U (IFun u) = false) by (apply isU_fun, negb_true_iff; assumption).
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install (@PFun U H h (compose_adaptation sms sm) _ _ (compose_positions rms rm)
                                     ltac:(apply osm_ok_compose; assumption) Hdc)
                       (kgood_nonU U H _ _ Hu) Hd) Hst)
          as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
        -- right. left. evgo. apply Hs. lia.
        -- right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
      * (* BApply, any other *)
        destruct (IHa W d d' u th outputs sm rs st ltac:(assumption) ltac:(assumption) ltac:(assumption)
                    ltac:(assumption) Hd Hst) as [Hs|[[ks Hs]|[x [Hx [ka Hka]]]]]; [ofB|skB|].
        pose proof (inv_a P' Hok' k d' u th outputs sm rs st Hst) as Hn.
        destruct x as [[dr st1]|r1], (apply_functor k P' d' u th outputs sm rs st) as [[dr' st1']|r1'];
          simpl in Hx, Hn; try contradiction.
        -- destruct Hx as [Hdr ->].
           destruct (IHb W _ _ g st1 r ltac:(assumption) ltac:(assumption) (agree_append Hdr Hd) Hn)
             as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
           ++ right. left. evgo. apply Hs. lia.
           ++ right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
        -- subst. errBq.
      * (* BPrimitive *)
        pose proof (prim_closure_eq i p Hd) as Epc.
        assert (Hpc : prel U H (primitive_closure d i p) (primitive_closure d' i p))
          by (rewrite <- Epc; apply prel_prim_closure).
        destruct (IHb W _ _ g st r ltac:(assumption) ltac:(assumption)
                    (agree_install Hpc
                       (kgood_nonU U H i _ ltac:(apply negb_true_iff; assumption)) Hd) Hst)
          as [Hs|[[ks Hs]|[x [Hx [kr Hkr]]]]]; [left; exact Hs| |].
        -- right. left. evgo. apply Hs. lia.
        -- right. right. exists x. split; [exact Hx|]. evgo. reflexivity.
  - (* apply_functor *)
    intros W d d' u th outputs sm rs st Hu Hs Hsm Hrs Hd Hst. red1.
    pose proof (ag_look Hd (IFun u) Hu) as Ho.
    destruct (lookup_provision (IFun u) d) as [a|] eqn:El,
             (lookup_provision (IFun u) d') as [a'|] eqn:El'; simpl in Ho; try contradiction; [|errBq].
    destruct Ho as [Ha _]. inversion Ha as [w|f0 df df' Hdf|h sms dc dc' rms Hsms Hdc|p ot ot' Hsite]; subst; try errBq.
    rewrite Hfun. destruct (view_functor P h) as [code|] eqn:Eh; cbn [option_map]; [|errBq].
    pose proof (all_select _ Hd Hs Hsm) as Hs1.
    destruct (select_provisions d sm) as [d1|] eqn:E1, (select_provisions d' sm) as [d1'|] eqn:E1';
      simpl in Hs1; try contradiction; [|errBq].
    pose proof (lrel_adapt sms Hs1 Hsms) as Hs2.
    destruct (adapt_provisions d1 sms) as [d2|] eqn:E2, (adapt_provisions d1' sms) as [d2'|] eqn:E2';
      simpl in Hs2; try contradiction; [|errBq].
    assert (Henv : lrel (application_environment P d h th (append_environment d2 dc))
                        (application_environment P' d' h th (append_environment d2' dc'))).
    { eapply app_env_rel; [exact Hd|]. apply lrel_append; assumption. }
    red1.
    destruct (IHb full _ _ [] st (fc_bindings code) (Hoku _ _ Eh) (lk_full _) (lrel_agree full Henv) Hst)
      as [Hb|[[ks Hb]|[x [Hx [kb Hkb]]]]]; [ofB|skB|].
    destruct x as [[[d3 g3] st3]|r],
             (eval_bindings k P' (application_environment P' d' h th (append_environment d2' dc')) [] st
                (Tbs (fc_bindings code))) as [[[d3' g3'] st3']|r']; simpl in Hx; try contradiction.
    + destruct Hx as [Hd3 [_ ->]].
      pose proof (select_nonU full d3 d3' (result_selection rs rms (fc_sources code)) Hd3 (lk_full _)
                  (fun q Hq => rsel_nonU rs rms (fc_sources code) q Hrs Hq)) as Hr.
      destruct (select_provisions d3 (result_selection rs rms (fc_sources code))) as [dr|] eqn:Er,
               (select_provisions d3' (result_selection rs rms (fc_sources code))) as [dr'|] eqn:Er';
        simpl in Hr; try contradiction; [|errBq].
      pose proof (result_env_rel outputs (fc_outputs code) Hd3 Hr) as Q.
      destruct (result_environment outputs d3 (fc_outputs code) dr) as [x|] eqn:Ex,
               (result_environment outputs d3' (fc_outputs code) dr') as [x'|] eqn:Ex';
        simpl in Q; try contradiction; [|errBq].
      right. right. exists (inl (x, st3)). split; [cbn; auto|]. evgo. reflexivity.
    + subst. errBq.
Qed.

Lemma bwd_all : forall k, BWD k.
Proof.
  assert (Hle : forall n k, k <= n -> BWD k).
  { induction n as [|n IH]; intros k Hk.
    - assert (k = 0) by lia. subst. repeat split; intros; left; reflexivity.
    - destruct (Nat.eq_dec k (S n)) as [->|Hne]; [|apply IH; lia].
      apply bwd_step. exact IH. }
  intros k. exact (Hle k k (le_n k)).
Qed.

End SimSec.

Lemma fp_view : forall U cH p argv rd,
  let P' := program_of (fp_program U cH p) argv rd in let P := program_of p argv rd in
  view_bool P' = view_bool P /\ view_true P' = view_true P /\ view_false P' = view_false P
  /\ view_arguments P' = view_arguments P /\ view_read P' = view_read P
  /\ view_casts P' = view_casts P.
Proof. intros. repeat split. Qed.

Section RunMain.
Variables (U H : usym) (p : program) (cH : functor_code).
Variables (argv : list (list PrimInt63.int)) (rd : list PrimInt63.int -> option (list PrimInt63.int)).
Hypothesis HcH : Elaboration.nlook H (program_functors p) = Some cH.
Hypothesis Hchk : ok_program U H p = true.
Let P := program_of p argv rd.
Let P' := program_of (fp_program U cH p) argv rd.
Variable ok : mstate -> Prop.
Hypothesis Hprim : forall q site ws st, ok st -> run_prim P' q site ws st = run_prim P q site ws st.
Hypothesis Hok : forall q site ws st, ok st -> ok (snd (run_prim P q site ws st)).
Hypothesis Hsites : forall operation site ws st,
  run_prim P operation site ws st =
  run_prim P operation (BaseSites.retained_site operation site) ws st.

Lemma chk_parts : body_ok cH = true
  /\ (forall f b, view_function P f = Some b -> ok_e U b = true)
  /\ (forall h c, view_functor P h = Some c -> forallb (ok_b U) (fc_bindings c) = true)
  /\ env_ok U H (program_environment p) = true.
Proof.
  unfold ok_program in Hchk. rewrite HcH in Hchk. andb_split. repeat split; auto.
  - intros f b Hf. apply PS.nlook_in in Hf.
    match goal with Hx : forallb _ (program_functions p) = true |- _ =>
      rewrite forallb_forall in Hx; exact (Hx (f, b) Hf) end.
  - intros h c Hf. apply PS.nlook_in in Hf.
    match goal with Hx : forallb _ (program_functors p) = true |- _ =>
      rewrite forallb_forall in Hx; exact (Hx (h, c) Hf) end.
Qed.

Lemma views : (forall f, view_function P' f = option_map (fp_e U cH) (view_function P f))
  /\ (forall h, view_functor P' h = option_map (fp_code U cH) (view_functor P h)).
Proof. split; intro; unfold P, P', program_of, fp_program; simpl; apply PS.nlook_map. Qed.

(* Running `main`: if the original run at this fuel is neither stuck nor out
   of fuel, the transformed run is equal to it at every large enough fuel. *)
Theorem run_main_forward : forall fuel st0, ok st0 ->
  bad (run_main fuel P (program_main p) (program_environment p) st0)
  \/ evf (fun j => run_main j P' (program_main p) (program_environment p) st0
                   = run_main fuel P (program_main p) (program_environment p) st0).
Proof.
  intros fuel st0 Hst. destruct chk_parts as [Hb [Hf [Hu He]]]. destruct views as [Hfn Hfun].
  destruct (@fwd_all U H cH P P' ok Hfn Hfun Hprim Hok Hsites eq_refl eq_refl eq_refl Hf Hu HcH Hb fuel)
    as [Hsim _].
  unfold run_main. rewrite Hfn.
  destruct (view_function P (program_main p)) as [body|] eqn:Em; simpl.
  - destruct (Hsim full _ _ [] st0 body (Hf _ _ Em) (lk_full _)
                (lrel_agree full (env_ok_refl U H _ He)) Hst) as [Hs|[k1 Hk1]].
    + left. apply ret_call_bad, Hs.
    + right. exists k1. intros j Hj. rewrite Hk1 by exact Hj. reflexivity.
  - right. exists 0. intros. reflexivity.
Qed.

(* If the transformed run at this fuel does not run out of fuel, the
   original run is eventually stuck, or eventually equal to it. *)
Theorem run_main_backward : forall fuel st0, ok st0 ->
  oof (run_main fuel P' (program_main p) (program_environment p) st0)
  \/ evf (fun j => stuck (run_main j P (program_main p) (program_environment p) st0))
  \/ evf (fun j => run_main j P (program_main p) (program_environment p) st0
                   = run_main fuel P' (program_main p) (program_environment p) st0).
Proof.
  intros fuel st0 Hst. destruct chk_parts as [Hb [Hf [Hu He]]]. destruct views as [Hfn Hfun].
  assert (Hok' : forall q site ws st, ok st -> ok (snd (run_prim P' q site ws st)))
    by (intros; rewrite Hprim by assumption; auto).
  destruct (@bwd_all U H cH P P' ok Hfn Hfun Hprim Hok Hsites eq_refl eq_refl eq_refl Hf Hu HcH Hb fuel)
    as [Hsim _].
  unfold run_main. rewrite Hfn.
  destruct (view_function P (program_main p)) as [body|] eqn:Em; simpl.
  - destruct (Hsim full _ _ [] st0 body (Hf _ _ Em) (lk_full _)
                (lrel_agree full (env_ok_refl U H _ He)) Hst) as [Hs|[[ks Hs]|[k1 Hk1]]].
    + left. revert Hs. unfold oof. destruct (eval fuel P' _ _ _ _) as [o g s]. simpl. intros ->. reflexivity.
    + right. left. exists ks. intros j Hj. specialize (Hs j Hj). revert Hs. unfold stuck.
      destruct (eval j P _ _ _ _) as [o g s]. simpl. intros [c ->]. eexists; reflexivity.
    + right. right. exists k1. intros j Hj. rewrite Hk1 by exact Hj. reflexivity.
  - right. right. exists 0. intros. reflexivity.
Qed.
End RunMain.
End Sim.

(* ===================== 6. Public runs ==================================== *)

Module FastRuns (Host : HOST) (M : PUBLIC Host).
Module FS := Sim M.ReferenceHost M.Reference.
Module PR := ProgramPrune.PruneRuns Host M.
Local Unset Implicit Arguments.

Lemma observe_none : forall r, FS.oof r -> M.observe r = None.
Proof. intros [o g s] E. unfold FS.oof in E. simpl in E. subst. reflexivity. Qed.
Lemma observe_stuck : forall r, FS.stuck r -> exists c, M.observe r = Some (M.Result (M.Wrong c) (snd (M.Reference.evaluation_state r))).
Proof. intros [o g s] [c E]. simpl in E. subst. exists c. reflexivity. Qed.

Section One.
Variables (program : program) (interface : M.interface) (initial : M.state).
Hypothesis Hblind : PR.code_blind interface.
Hypothesis Hsafe : forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final).

(* One candidate, F with body H: the same Runs. *)
Theorem runs_fast_one : forall U H result,
  M.Runs (fast_one U H program) interface initial result <-> M.Runs program interface initial result.
Proof.
  intros U H result. unfold fast_one.
  destruct (Elaboration.nlook H (program_functors program)) as [cH|] eqn:Ec; [|tauto].
  destruct (ok_program U H program) eqn:Eok; [|tauto].
  set (ok := fun st : M.ReferenceHost.mstate => fst st = interface).
  assert (Hprim : forall q site ws st, ok st ->
    M.ReferenceHost.run_prim (program_of (fp_program U cH program) (M.arguments initial) (M.read_file initial)) q site ws st
    = M.ReferenceHost.run_prim (program_of program (M.arguments initial) (M.read_file initial)) q site ws st).
  { intros q site ws [i s] Hi. unfold ok in Hi. simpl in Hi. subst i. unfold M.ReferenceHost.run_prim, Base.call, BaseCall.call, M.ReferenceHost.raw_primitive.
    destruct (FS.fp_view U cH program (M.arguments initial) (M.read_file initial)) as [B [T [F [A [Rd C]]]]].
    rewrite (Hblind _ _ B T F A Rd C). reflexivity. }
  assert (Hok : forall q site ws st, ok st ->
    ok (snd (M.ReferenceHost.run_prim (program_of program (M.arguments initial) (M.read_file initial)) q site ws st))).
  { intros q site ws st Hi. unfold M.ReferenceHost.run_prim.
    eapply base_call_preserves; [|exact Hi].
    intros operation arguments [i s] Valid. unfold M.ReferenceHost.raw_primitive.
    destruct (M.supports i operation); [|exact Valid].
    destruct (M.primitive i _ operation arguments (M.external s)); exact Valid. }
  assert (Hsites : forall operation site ws st,
    M.ReferenceHost.run_prim (program_of program (M.arguments initial) (M.read_file initial)) operation site ws st =
    M.ReferenceHost.run_prim (program_of program (M.arguments initial) (M.read_file initial)) operation
      (BaseSites.retained_site operation site) ws st).
  { intros. unfold M.ReferenceHost.run_prim. apply BaseSites.erased_primitive_call. }
  unfold M.Runs, M.runtime. simpl. split.
  - intros [fuel E].
    destruct (FS.run_main_backward U H program cH (M.arguments initial) (M.read_file initial) Ec Eok ok
                Hprim Hok Hsites fuel (interface, initial) eq_refl) as [Hs|[[ks Hs]|[k1 Hk1]]].
    + rewrite (observe_none _ Hs) in E. discriminate.
    + exfalso. destruct (observe_stuck _ (Hs ks (le_n ks))) as [c Ec'].
      apply (Hsafe c (snd (M.Reference.evaluation_state (M.Reference.run_main ks
        (program_of program (M.arguments initial) (M.read_file initial))
        (program_main program) (program_environment program) (interface, initial))))).
      exists ks. unfold M.runtime. exact Ec'.
    + exists k1. rewrite (Hk1 k1 (le_n k1)). exact E.
  - intros [fuel E].
    destruct (FS.run_main_forward U H program cH (M.arguments initial) (M.read_file initial) Ec Eok ok
                Hprim Hok Hsites fuel (interface, initial) eq_refl) as [[Hs|Hs]|[k1 Hk1]].
    + exfalso. destruct (observe_stuck _ Hs) as [c Ec'].
      apply (Hsafe c (snd (M.Reference.evaluation_state (M.Reference.run_main fuel
        (program_of program (M.arguments initial) (M.read_file initial))
        (program_main program) (program_environment program) (interface, initial))))).
      exists fuel. unfold M.runtime. exact Ec'.
    + rewrite (observe_none _ Hs) in E. discriminate.
    + exists k1. rewrite (Hk1 k1 (le_n k1)). exact E.
Qed.
End One.

(* The pass: every candidate in turn. *)
Theorem runs_fastpath : forall program interface initial result,
  PR.code_blind interface ->
  (forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final)) ->
  M.Runs (fastpath program) interface initial result <-> M.Runs program interface initial result.
Proof.
  intros program interface initial result Hblind. unfold fastpath.
  generalize (candidates program). intros l. revert program.
  induction l as [|[u h] r IH]; intros program Hsafe; simpl; [tauto|].
  rewrite IH.
  - apply runs_fast_one; assumption.
  - intros code final Hr. apply (Hsafe code final). apply (runs_fast_one program interface initial Hblind Hsafe u h). exact Hr.
Qed.

End FastRuns.
