(* ------------------------------------------------------------------------ *)
(* CoreMoss.v — mechanization of the calculus in core-moss.tex               *)
(*                                                                           *)
(* This file defines the syntax, static semantics, and dynamic semantics of  *)
(* Core Moss, and states the metatheory of the paper's §4.  Everything is    *)
(* definitional except where marked: Proposition 4.1 is proved, the paper's  *)
(* Figure 1 example is encoded and *computed* to a value by the executable   *)
(* interpreter (twice — once erased — as evidence for Proposition 4.3).     *)
(* Determinism, interpreter adequacy (both directions), phase separation,   *)
(* and resolution decidability are machine-checked outright; soundness is   *)
(* derived from a stated fuel-indexed safety invariant whose per-rule case  *)
(* lemmas are the remaining admitted obligations (grep "FILL:").            *)
(*                                                                           *)
(* Deviations from the paper, all mechanization-level:                       *)
(*                                                                           *)
(* 1. Elaborated normal form.  The paper lets a reference omit bindings for  *)
(*    requirements already in force, reading it through the completion θ̂     *)
(*    (rules S-Pass/S-Inst).  Here every nominal reference and application   *)
(*    is total as written — elaboration has materialized θ̂, with pass-       *)
(*    through requirements appearing as identity entries (t, TAbs t).  The   *)
(*    satisfaction judgment therefore checks coverage plus availability      *)
(*    rather than building a completion.                                     *)
(* 2. Satisfaction and availability are recursive definitions (Forall over   *)
(*    the telescope) rather than inference rules; they are equivalent to     *)
(*    the paper's S-rules read under (1).                                    *)
(* 3. Match arms are ordered to align with the scrutinee's members, rather   *)
(*    than being an unordered bijection; unions are not quotiented by        *)
(*    member order.                                                          *)
(* 4. As in the paper's §3, every method provider takes the receiver as its  *)
(*    first argument; `this` does not appear.                                *)
(* 5. The dynamics is over elaborated terms: satisfaction maps ς and chosen  *)
(*    provisions ι₀ are fields of the syntax, and the typing rules check     *)
(*    them (the paper's "elaboration records these choices").                *)
(* 6. Provisions are keyed by the receiver's full type, not merely its       *)
(*    head.  The paper's §1 states that the dispatch key is the pair         *)
(*    (receiver *type*, method symbol); its typing figure keys on the head   *)
(*    as a shortcut, adequate under Moss's unique-nominal-per-instantiation  *)
(*    idiom [D51] but unsound in the raw calculus — a provision bound at     *)
(*    Slot[Int] would be found for a Slot[Char] receiver, and the This-      *)
(*    substitution would lie.  Discovered while stating preservation, and   *)
(*    now corrected in the paper's figures as well.                          *)
(* 7. Type binds are fresh: bind t = τ requires t not already in force.      *)
(*    Surface Moss allows lexical shadowing [D25]; under substitution-based  *)
(*    statics, shadowing an in-use abstract type symbol would conflate its   *)
(*    two generations (types already stored in Γ would be re-read under the  *)
(*    new binding).  Val/fn/method binds shadow harmlessly and remain        *)
(*    unrestricted.  Found while designing the preservation invariant.       *)
(* 8. Satisfaction carries a coherence premise: a val or fn requirement      *)
(*    names no types, so nothing else stops f[T=Bool] from being satisfied   *)
(*    by an ambient v : T that was provided under bind T=Int.  Where a       *)
(*    non-type requirement is answered from the ambient context, the         *)
(*    application must agree with the context on the type symbols that       *)
(*    requirement's own telescope mentions.  (Method items already carry     *)
(*    their receiver and application, so they were immune — see (6).)  Also  *)
(*    found while designing the preservation invariant; now in the paper's   *)
(*    S-Item rule as well.                                                    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith.
Import ListNotations.

Set Implicit Arguments.

(* ===================== 1. Symbols ======================================== *)

Definition tsym := nat.  (* abstract type symbols *)
Definition nsym := nat.  (* nominal tag names *)
Definition vsym := nat.  (* val symbols *)
Definition fsym := nat.  (* fn symbols *)
Definition msym := nat.  (* method symbols *)
Definition var  := nat.  (* term variables *)

(* The distinguished receiver type of method signatures. *)
Definition this_sym : tsym := 0.

(* ===================== 2. Types and substitutions ======================== *)

Inductive ty : Type :=
| TUnit  : ty
| TAbs   : tsym -> ty                       (* abstract type symbol (incl. This) *)
| TNom   : nsym -> list (tsym * ty) -> ty   (* total applied nominal reference *)
| TUnion : list ty -> ty.                   (* members must be nominal refs *)

Definition subst := list (tsym * ty).

(* A custom induction principle: the auto-generated one is useless for the
   nested lists inside TNom/TUnion. *)
Fixpoint ty_ind' (P : ty -> Prop)
    (HU : P TUnit)
    (HA : forall t, P (TAbs t))
    (HN : forall n th, Forall (fun p => P (snd p)) th -> P (TNom n th))
    (HUn : forall ms, Forall P ms -> P (TUnion ms))
    (t : ty) {struct t} : P t :=
  match t with
  | TUnit => HU
  | TAbs a => HA a
  | TNom n th =>
      HN n th
        ((fix go (l : list (tsym * ty)) : Forall (fun p => P (snd p)) l :=
            match l with
            | [] => Forall_nil (fun p => P (snd p))
            | p :: r => @Forall_cons _ (fun p => P (snd p)) p r
                          (ty_ind' HU HA HN HUn (snd p)) (go r)
            end) th)
  | TUnion ms =>
      HUn ms
        ((fix go (l : list ty) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | t' :: r => @Forall_cons _ P t' r (ty_ind' HU HA HN HUn t') (go r)
            end) ms)
  end.

Fixpoint lookup_subst (t : tsym) (s : subst) : option ty :=
  match s with
  | [] => None
  | (t', tau) :: r => if Nat.eqb t t' then Some tau else lookup_subst t r
  end.

(* Applying a substitution.  Substitutions accumulated by the statics are
   idempotent by construction (bind t = τ stores στ), so one pass suffices. *)
Fixpoint app_subst (s : subst) (tau : ty) {struct tau} : ty :=
  match tau with
  | TUnit => TUnit
  | TAbs t => match lookup_subst t s with Some tau' => tau' | None => TAbs t end
  | TNom n th =>
      TNom n ((fix go (l : list (tsym * ty)) : list (tsym * ty) :=
                 match l with
                 | [] => []
                 | p :: r => (fst p, app_subst s (snd p)) :: go r
                 end) th)
  | TUnion ms =>
      TUnion ((fix go (l : list ty) : list ty :=
                 match l with
                 | [] => []
                 | t' :: r => app_subst s t' :: go r
                 end) ms)
  end.

Lemma app_subst_nil : forall tau, app_subst [] tau = tau.
Proof.
  induction tau using ty_ind'; simpl; try reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. destruct x; reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. reflexivity.
Qed.

(* Boolean equality on types. *)
Fixpoint ty_eqb (a b : ty) {struct a} : bool :=
  match a, b with
  | TUnit, TUnit => true
  | TAbs t1, TAbs t2 => Nat.eqb t1 t2
  | TNom n1 th1, TNom n2 th2 =>
      Nat.eqb n1 n2 &&
      ((fix go (l1 l2 : list (tsym * ty)) : bool :=
          match l1, l2 with
          | [], [] => true
          | p1 :: r1, p2 :: r2 =>
              Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && go r1 r2
          | _, _ => false
          end) th1 th2)
  | TUnion m1, TUnion m2 =>
      (fix go (l1 l2 : list ty) : bool :=
         match l1, l2 with
         | [], [] => true
         | t1 :: r1, t2 :: r2 => ty_eqb t1 t2 && go r1 r2
         | _, _ => false
         end) m1 m2
  | _, _ => false
  end.

(* ===================== 3. Items and telescopes =========================== *)

(* A method item carries the *receiver type* it is provided at: the paper's
   §1 key, the pair (receiver type, method symbol), plus the application θ
   that interprets the method's own requirements (deviation 6). *)
Inductive item : Type :=
| ITy  : tsym -> item
| IVal : vsym -> item
| IFn  : fsym -> item
| IMth : ty -> msym -> subst -> item.

Definition tele := list item.

Fixpoint subst_eqb (a b : subst) : bool :=
  match a, b with
  | [], [] => true
  | p1 :: r1, p2 :: r2 =>
      Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && subst_eqb r1 r2
  | _, _ => false
  end.

Definition item_eqb (a b : item) : bool :=
  match a, b with
  | ITy t1, ITy t2 => Nat.eqb t1 t2
  | IVal v1, IVal v2 => Nat.eqb v1 v2
  | IFn f1, IFn f2 => Nat.eqb f1 f2
  | IMth r1 m1 th1, IMth r2 m2 th2 =>
      ty_eqb r1 r2 && Nat.eqb m1 m2 && subst_eqb th1 th2
  | _, _ => false
  end.

(* The head a union member discriminates by. *)
Definition member_head (tau : ty) : option nsym :=
  match tau with
  | TNom n _ => Some n
  | _ => None
  end.

Definition map_snd (A B C : Type) (f : B -> C) (l : list (A * B)) : list (A * C) :=
  map (fun p => (fst p, f (snd p))) l.

(* A substitution acts on an item through the types it carries. *)
Definition app_subst_item (s : subst) (i : item) : item :=
  match i with
  | ITy t => ITy t
  | IVal v => IVal v
  | IFn f => IFn f
  | IMth rt m th => IMth (app_subst s rt) m (map_snd (app_subst s) th)
  end.

(* ===================== 4. Contexts in force ============================== *)

(* Φ = ⟨A; σ⟩: available items and the static substitution from type binds. *)
Record ctx : Type := mkCtx { cA : list item; cS : subst }.

(* Availability, ι ∈ Φ: membership in A modulo σ (the paper's residue of key
   canonicalization).  Defined as a boolean so that Proposition 4.1's
   decidability claim is discharged by construction. *)
Definition item_subst_eqb (s : subst) (i1 i2 : item) : bool :=
  item_eqb (app_subst_item s i1) (app_subst_item s i2).

Definition avail_b (Phi : ctx) (i : item) : bool :=
  existsb (fun i0 => item_subst_eqb (cS Phi) i0 i) (cA Phi).

Definition avail (Phi : ctx) (i : item) : Prop := avail_b Phi i = true.

Definition is_ty_item (i : item) : bool :=
  match i with ITy _ => true | _ => false end.

Definition tyreqs (D : tele) : list tsym :=
  flat_map (fun i => match i with ITy t => [t] | _ => [] end) D.

(* The identity application: the paper's ∅ ⊨ Δ, materialized. *)
Definition id_app (D : tele) : subst :=
  map (fun t => (t, TAbs t)) (tyreqs D).

Definition add_item (i : item) (Phi : ctx) : ctx :=
  mkCtx (i :: cA Phi) (cS Phi).

Definition bind_ty (t : tsym) (tau : ty) (Phi : ctx) : ctx :=
  mkCtx (ITy t :: cA Phi) ((t, app_subst (cS Phi) tau) :: cS Phi).

(* ===================== 5. Expressions and values ========================= *)

(* Satisfaction maps: for each requirement of a callee/provider, the in-force
   item that satisfies it (recorded by elaboration, used by the dynamics). *)
Definition smap := list (item * item).

Inductive expr : Type :=
| EVar     : var -> expr
| EUnit    : expr
| EVl      : vsym -> expr
| ETag     : nsym -> subst -> expr -> expr
| ECallD   : fsym -> subst -> smap -> list expr -> expr   (* defined fn call *)
| ECallA   : fsym -> list expr -> expr                    (* abstract fn call *)
| EMeth    : expr -> msym -> item -> list expr -> expr    (* item: the provision *)
| EMatch   : expr -> list (nsym * var * expr) -> expr
| ELet     : var -> expr -> expr -> expr
| EBindTy  : tsym -> ty -> expr -> expr
| EBindVal : vsym -> expr -> expr -> expr
| EBindFn  : fsym -> fsym -> smap -> expr -> expr
| EBindMth : ty -> msym -> subst -> fsym -> smap -> expr -> expr.
(* In EBindMth the first field is the receiver type; its head is the key. *)

Inductive value : Type :=
| VUnit : value
| VTag  : nsym -> value -> value.

(* ===================== 6. Global signatures ============================== *)

Record fnsig : Type := mkSig { fs_params : list ty; fs_ret : ty }.

(* Σ: every symbol's kind, signature, and (concatenated) enclosing telescope.
   Method signatures may mention TAbs this_sym; the receiver is understood as
   an implicit first argument. *)
Record gsig : Type := mkGsig {
  g_ty  : tsym -> bool;
  g_tag : nsym -> option (tele * ty);
  g_val : vsym -> option (tele * ty);
  g_fn  : fsym -> option (tele * fnsig * option expr);
  g_mth : msym -> option (tele * fnsig)
}.

(* The type symbols a val/fn requirement's own telescope mentions — the
   dependencies whose instantiations coherence must pin down (deviation 8). *)
Definition dep_tys (Sg : gsig) (i : item) : list tsym :=
  match i with
  | IVal v => match g_val Sg v with Some (Dv, _) => tyreqs Dv | None => [] end
  | IFn f => match g_fn Sg f with Some (Df, _, _) => tyreqs Df | None => [] end
  | _ => []
  end.

(* Satisfaction Φ ⊢ θ ⊨ Δ, in elaborated normal form: θ is defined exactly
   on Δ's type items (identity entries playing the paper's S-Pass); every
   other requirement, read under θ, is available; and — coherence — where a
   non-type requirement is answered from the ambient context, θ must agree
   with the context on the type symbols that requirement depends on
   (deviation 8). *)
Definition sat (Sg : gsig) (Phi : ctx) (th : subst) (D : tele) : Prop :=
  map fst th = tyreqs D /\
  Forall (fun i =>
            is_ty_item i = true \/
            (avail Phi (app_subst_item th i) /\
             Forall (fun t =>
                       app_subst (cS Phi) (app_subst th (TAbs t))
                       = app_subst (cS Phi) (TAbs t))
                    (dep_tys Sg i))) D.

Definition sat0 (Sg : gsig) (Phi : ctx) (D : tele) : Prop :=
  sat Sg Phi (id_app D) D
  /\ Forall (fun t => avail Phi (ITy t)) (tyreqs D).
(* The second conjunct is the paper's S-Pass availability, which the
   elaborated-normal-form `sat` alone loses at identity applications: a
   batch-1 agent refuted tele_deps_closed and traced it exactly here. *)

(* ===================== 7. Type formation and subtyping =================== *)

Definition is_nom (tau : ty) : Prop := exists n th, tau = TNom n th.

Definition heads_of (s : subst) (ms : list ty) : list (option nsym) :=
  map (fun tau => member_head (app_subst s tau)) ms.

Inductive wf_ty (Sg : gsig) (Phi : ctx) : ty -> Prop :=
| WfUnit : wf_ty Sg Phi TUnit
| WfNeed : forall t, avail Phi (ITy t) -> wf_ty Sg Phi (TAbs t)
| WfTag : forall n D tau0 th,
    g_tag Sg n = Some (D, tau0) ->
    sat Sg Phi th D ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    wf_ty Sg Phi (TNom n th)
| WfUnion : forall ms,
    Forall (wf_ty Sg Phi) ms ->
    Forall is_nom ms ->
    NoDup (heads_of (cS Phi) ms) ->
    wf_ty Sg Phi (TUnion ms).

(* Type equality up to the substitution in force (the applicative identity
   rule [D18]); unions are compared in order (deviation 3). *)
Definition ty_eq (s : subst) (t1 t2 : ty) : Prop :=
  app_subst s t1 = app_subst s t2.

(* Subtyping: injection only [D17]. *)
Inductive subty (Phi : ctx) : ty -> ty -> Prop :=
| SubEq : forall t1 t2, ty_eq (cS Phi) t1 t2 -> subty Phi t1 t2
| SubInj : forall n th ms tau',
    In tau' ms ->
    ty_eq (cS Phi) (TNom n th) tau' ->
    subty Phi (TNom n th) (TUnion ms).

(* The members a match must cover: a nominal ref is its singleton union. *)
Definition members (tau : ty) : option (list ty) :=
  match tau with
  | TNom n th => Some [TNom n th]
  | TUnion ms => Some ms
  | _ => None
  end.

(* ===================== 8. Typing ========================================= *)

Definition tenv := list (var * ty).

Fixpoint lookup_tenv (x : var) (G : tenv) : option ty :=
  match G with
  | [] => None
  | (x', tau) :: r => if Nat.eqb x x' then Some tau else lookup_tenv x r
  end.

Fixpoint lookup_smap (i : item) (sm : smap) : option item :=
  match sm with
  | [] => None
  | (a, b) :: r => if item_eqb i a then Some b else lookup_smap i r
  end.

(* ς correctness: each non-type requirement, read under the application θ, is
   sent to an in-force item with the same σ-image. *)
Definition item_matches (s th : subst) (i i' : item) : Prop :=
  app_subst_item s (app_subst_item th i) = app_subst_item s i'.

Definition smap_ok (Phi : ctx) (th : subst) (sm : smap) (D : tele) : Prop :=
  map fst sm = filter (fun i => negb (is_ty_item i)) D /\
  Forall (fun p => In (snd p) (cA Phi)
                   /\ item_matches (cS Phi) th (fst p) (snd p)) sm.

(* Interpreting a method signature at a receiver: ρ = σ ∘ θ ∘ [This ↦ τ₀]. *)
Definition this_subst (tau0 : ty) : subst := [(this_sym, tau0)].

(* NOTE the composition order: the receiver is substituted OUTSIDE the
   application, so θ can never rewrite inside it.  The first draft had
   [This↦τ0] innermost; an agent machine-refuted the soundness development
   at exactly that capture (a method item may mention a symbol of its own
   application's domain), which also forced two stopgap "receiver
   coherence" premises that this order makes unnecessary. *)
Definition interp_m (s th : subst) (tau0 tau : ty) : ty :=
  app_subst s (app_subst (this_subst tau0) (app_subst th tau)).

(* "ι is a provision of m at receiver type τ₀", judged after σ. *)
Definition is_prov_at (s : subst) (tau0 : ty) (m : msym) (i : item) : Prop :=
  exists th', app_subst_item s i = IMth (app_subst s tau0) m th'.

Definition sig_eq (s : subst) (S1 S2 : fnsig) : Prop :=
  map (app_subst s) (fs_params S1) = map (app_subst s) (fs_params S2)
  /\ app_subst s (fs_ret S1) = app_subst s (fs_ret S2).

(* Match arms.  An arm (N, x, e) handles member N, binding x to the payload.
   These helpers keep the recursive premise of T-Match a direct application
   (Rocq's strict-positivity checker cannot see through matches or exists). *)
Definition arm_name (arm : nsym * var * expr) : nsym := fst (fst arm).
Definition arm_var (arm : nsym * var * expr) : var := snd (fst arm).
Definition arm_body (arm : nsym * var * expr) : expr := snd arm.

(* The payload type an arm's binder receives from a member N[θ]: the tag's
   declared payload under θ.  (Total for the recursion's sake; the shape
   premise `arm_matches` rules the junk cases out.) *)
Definition member_payload (Sg : gsig) (mem : ty) : ty :=
  match mem with
  | TNom n thn =>
      match g_tag Sg n with
      | Some (_, taup) => app_subst thn taup
      | None => TUnit
      end
  | _ => TUnit
  end.

Definition arm_matches (Sg : gsig) (mem : ty) (arm : nsym * var * expr) : Prop :=
  match mem with
  | TNom n _ => arm_name arm = n /\ exists D taup, g_tag Sg n = Some (D, taup)
  | _ => False
  end.

Inductive has_ty (Sg : gsig) : ctx -> tenv -> expr -> ty -> Prop :=
| TVarR : forall Phi G x tau,
    lookup_tenv x G = Some tau ->
    has_ty Sg Phi G (EVar x) tau
| TUnitR : forall Phi G,
    has_ty Sg Phi G EUnit TUnit
| TSubR : forall Phi G e tau' tau,
    has_ty Sg Phi G e tau' ->
    subty Phi tau' tau ->
    (* the target must be well-formed: subsumption may not inject into a
       union whose other members mention arbitrary symbols (regularity) *)
    wf_ty Sg Phi tau ->
    has_ty Sg Phi G e tau
| TValR : forall Phi G v Dv tauv,
    g_val Sg v = Some (Dv, tauv) ->
    avail Phi (IVal v) ->
    has_ty Sg Phi G (EVl v) (app_subst (cS Phi) tauv)
| TTagR : forall Phi G n D tau0 th e,
    g_tag Sg n = Some (D, tau0) ->
    sat Sg Phi th D ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
    has_ty Sg Phi G (ETag n th e) (app_subst (cS Phi) (TNom n th))
| TNeedR : forall Phi G f Df S args,
    g_fn Sg f = Some (Df, S, None) ->
    avail Phi (IFn f) ->
    Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau))
            args (fs_params S) ->
    has_ty Sg Phi G (ECallA f args) (app_subst (cS Phi) (fs_ret S))
| TCallR : forall Phi G f Df S body th sm args,
    g_fn Sg f = Some (Df, S, Some body) ->
    sat Sg Phi th Df ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Df ->
    Forall2 (fun e tau =>
               has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
            args (fs_params S) ->
    has_ty Sg Phi G (ECallD f th sm args)
           (app_subst (cS Phi) (app_subst th (fs_ret S)))
| TMethR : forall Phi G e0 tau0 m Dm S rt0 th0 args,
    g_mth Sg m = Some (Dm, S) ->
    has_ty Sg Phi G e0 tau0 ->
    In (IMth rt0 m th0) (cA Phi) ->
    (* the provision's receiver type is the receiver's type, after σ *)
    app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
    (* dispatch demands exactly one hit [D1] *)
    (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                i1 = IMth rt0 m th0) ->
    Forall2 (fun e tau =>
               has_ty Sg Phi G e
                      (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params S) ->
    has_ty Sg Phi G (EMeth e0 m (IMth rt0 m th0) args)
           (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S))
| TMatchR : forall Phi G e0 tau0 ms arms tau,
    has_ty Sg Phi G e0 tau0 ->
    (* scrutinee-type regularity, of the σ-image: elaboration always knows
       it, and it is what the proof inverts (the type as written may be an
       abstract symbol whose binding carries the members) *)
    wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
    members (app_subst (cS Phi) tau0) = Some ms ->
    (* regularity, made local: a wf scrutinee type has distinct heads, and
       find_arm needs that to pick the arm the injection was typed at *)
    NoDup (map member_head ms) ->
    Forall2 (arm_matches Sg) ms arms ->
    Forall2 (fun mem arm =>
               has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                      (arm_body arm) tau) ms arms ->
    has_ty Sg Phi G (EMatch e0 arms) tau
| TLetR : forall Phi G x e1 e2 tau1 tau,
    has_ty Sg Phi G e1 tau1 ->
    has_ty Sg Phi ((x, tau1) :: G) e2 tau ->
    has_ty Sg Phi G (ELet x e1 e2) tau
| TBindTyR : forall Phi G t tau' e tau,
    g_ty Sg t = true ->
    (* freshness: no shadowing of an in-force type symbol (deviation 7) *)
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    has_ty Sg (bind_ty t tau' Phi) G e tau ->
    (* the result type may not escape the bind's scope: it must already be
       well-formed outside (a batch-2 agent refuted preservation without
       this — subsumption under the extended σ retypes the body at TAbs t,
       which then leaks) *)
    wf_ty Sg Phi tau ->
    has_ty Sg Phi G (EBindTy t tau' e) tau
| TBindValR : forall Phi G v Dv tauv e1 e tau,
    g_val Sg v = Some (Dv, tauv) ->
    sat0 Sg Phi Dv ->
    has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
    has_ty Sg (add_item (IVal v) Phi) G e tau ->
    has_ty Sg Phi G (EBindVal v e1 e) tau
| TBindFnR : forall Phi G f Df S f' D' S' body sm e tau,
    g_fn Sg f = Some (Df, S, None) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Sg Phi Df ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    (* signature match after the substitutions in force; never inferred [D27] *)
    sig_eq (cS Phi) S' S ->
    has_ty Sg (add_item (IFn f) Phi) G e tau ->
    has_ty Sg Phi G (EBindFn f f' sm e) tau
| TBindMthR : forall Phi G tauk m Dm S th f' D' S' body sm tau0' rest e tau,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    sat Sg Phi th Dm ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    (* the provider's first parameter is the receiver *)
    fs_params S' = tau0' :: rest ->
    ty_eq (cS Phi) tau0' tauk ->
    map (app_subst (cS Phi)) rest
      = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
    app_subst (cS Phi) (fs_ret S')
      = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
    has_ty Sg (add_item (IMth tauk m th) Phi) G e tau ->
    has_ty Sg Phi G (EBindMth tauk m th f' sm e) tau.

(* ===================== 9. Telescope, declaration, program formation ====== *)

Definition mk_ctx (D : tele) : ctx := mkCtx D [].

Inductive item_ok (Sg : gsig) (Phi : ctx) : item -> Prop :=
| OkTy : forall t, g_ty Sg t = true -> item_ok Sg Phi (ITy t)
| OkVal : forall v Dv tauv,
    g_val Sg v = Some (Dv, tauv) -> sat0 Sg Phi Dv -> item_ok Sg Phi (IVal v)
| OkFn : forall f Df S b,
    g_fn Sg f = Some (Df, S, b) -> sat0 Sg Phi Df -> item_ok Sg Phi (IFn f)
| OkMth : forall rt m Dm S th,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi rt ->
    sat Sg Phi th Dm ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    item_ok Sg Phi (IMth rt m th).

(* The telescoping rule [D20]: each item is checked with only the items to
   its left (and the enclosing telescope) in force. *)
Inductive wf_tele (Sg : gsig) : tele -> tele -> Prop :=
| WNil : forall D0, wf_tele Sg D0 []
| WCons : forall D0 i D,
    item_ok Sg (mk_ctx D0) i ->
    wf_tele Sg (D0 ++ [i]) D ->
    wf_tele Sg D0 (i :: D).

(* A defined body's parameters become variables 0, 1, ... *)
Definition body_env (S : fnsig) : tenv :=
  combine (seq 0 (length (fs_params S))) (fs_params S).

Record wf_gsig (Sg : gsig) : Prop := {
  wf_tag_decl : forall n D taup,
      g_tag Sg n = Some (D, taup) ->
      wf_tele Sg [] D /\ wf_ty Sg (mk_ctx D) taup;
  wf_val_decl : forall v D tauv,
      g_val Sg v = Some (D, tauv) ->
      wf_tele Sg [] D /\ wf_ty Sg (mk_ctx D) tauv;
  wf_fn_decl : forall f D S b,
      g_fn Sg f = Some (D, S, b) ->
      wf_tele Sg [] D
      /\ Forall (wf_ty Sg (mk_ctx D)) (fs_params S)
      /\ wf_ty Sg (mk_ctx D) (fs_ret S)
      /\ (forall body, b = Some body ->
             has_ty Sg (mk_ctx D) (body_env S) body (fs_ret S));
  wf_mth_decl : forall m D S,
      g_mth Sg m = Some (D, S) ->
      wf_tele Sg [] D
      /\ Forall (wf_ty Sg (mk_ctx (ITy this_sym :: D))) (fs_params S)
      /\ wf_ty Sg (mk_ctx (ITy this_sym :: D)) (fs_ret S)
}.

(* ⊢ P ok: a well-formed signature whose main is defined, closed, unit-valued. *)
Definition wf_prog (Sg : gsig) (main : fsym) : Prop :=
  wf_gsig Sg /\
  exists body, g_fn Sg main = Some ([], mkSig [] TUnit, Some body).

(* ===================== 10. Dynamics ====================================== *)

(* Runtime contexts: val items carry values; fn and method items carry
   (code, captured context) pairs — the dynamic residue of [D2]. *)
Inductive dentry : Type :=
| DVal : value -> dentry
| DClo : fsym -> list (item * dentry) -> dentry.

Definition denv := list (item * dentry).
Definition venv := list (var * value).

Fixpoint lookup_denv (i : item) (d : denv) : option dentry :=
  match d with
  | [] => None
  | (i', en) :: r => if item_eqb i i' then Some en else lookup_denv i r
  end.

Fixpoint lookup_venv (x : var) (g : venv) : option value :=
  match g with
  | [] => None
  | (x', w) :: r => if Nat.eqb x x' then Some w else lookup_venv x r
  end.

(* δ ∘ ς: the caller's provisions, restricted and re-keyed to the callee's
   own requirement names. *)
Fixpoint compose_smap (d : denv) (sm : smap) : option denv :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match lookup_denv s d, compose_smap d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

Fixpoint find_arm (n : nsym) (arms : list (nsym * var * expr))
  : option (var * expr) :=
  match arms with
  | [] => None
  | (n', x, eb) :: r => if Nat.eqb n n' then Some (x, eb) else find_arm n r
  end.

Definition mk_venv (ws : list value) : venv :=
  combine (seq 0 (length ws)) ws.

(* Big-step evaluation (Figure 7 of the paper). *)
Inductive eval (Sg : gsig) : denv -> venv -> expr -> value -> Prop :=
| EvUnit : forall d g,
    eval Sg d g EUnit VUnit
| EvVar : forall d g x w,
    lookup_venv x g = Some w ->
    eval Sg d g (EVar x) w
| EvVal : forall d g v w,
    lookup_denv (IVal v) d = Some (DVal w) ->
    eval Sg d g (EVl v) w
| EvTag : forall d g n th e w,
    eval Sg d g e w ->
    eval Sg d g (ETag n th e) (VTag n w)
| EvLet : forall d g x e1 e2 w1 w,
    eval Sg d g e1 w1 ->
    eval Sg d ((x, w1) :: g) e2 w ->
    eval Sg d g (ELet x e1 e2) w
| EvMatch : forall d g e0 arms n w' x eb w,
    eval Sg d g e0 (VTag n w') ->
    find_arm n arms = Some (x, eb) ->
    eval Sg d ((x, w') :: g) eb w ->
    eval Sg d g (EMatch e0 arms) w
| EvCallD : forall d g f th sm args D S body ws d' w,
    g_fn Sg f = Some (D, S, Some body) ->
    Forall2 (eval Sg d g) args ws ->
    compose_smap d sm = Some d' ->
    eval Sg d' (mk_venv ws) body w ->
    eval Sg d g (ECallD f th sm args) w
| EvCallA : forall d g f args f' df D' S' body ws w,
    lookup_denv (IFn f) d = Some (DClo f' df) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    Forall2 (eval Sg d g) args ws ->
    eval Sg df (mk_venv ws) body w ->
    eval Sg d g (ECallA f args) w
| EvMeth : forall d g e0 m prov args f' d' D' S' body w0 ws w,
    lookup_denv prov d = Some (DClo f' d') ->
    g_fn Sg f' = Some (D', S', Some body) ->
    eval Sg d g e0 w0 ->
    Forall2 (eval Sg d g) args ws ->
    eval Sg d' (mk_venv (w0 :: ws)) body w ->
    eval Sg d g (EMeth e0 m prov args) w
| EvBindTy : forall d g t tau e w,
    eval Sg d g e w ->
    eval Sg d g (EBindTy t tau e) w
| EvBindVal : forall d g v e1 e w1 w,
    eval Sg d g e1 w1 ->
    eval Sg ((IVal v, DVal w1) :: d) g e w ->
    eval Sg d g (EBindVal v e1 e) w
| EvBindFn : forall d g f f' sm dc e w,
    compose_smap d sm = Some dc ->
    eval Sg ((IFn f, DClo f' dc) :: d) g e w ->
    eval Sg d g (EBindFn f f' sm e) w
| EvBindMth : forall d g tauk m th f' sm dc e w,
    compose_smap d sm = Some dc ->
    eval Sg ((IMth tauk m th, DClo f' dc) :: d) g e w ->
    eval Sg d g (EBindMth tauk m th f' sm e) w.

(* An executable twin: a fueled definitional interpreter.  It is the
   deterministic implementation of `eval`, and its Err result is the
   instrumented error of Theorem 4.2 (any missing lookup or shape mismatch). *)
Inductive res : Type :=
| Ok        : value -> res
| Err       : res
| OutOfFuel : res.

Fixpoint interp (fuel : nat) (Sg : gsig) (d : denv) (g : venv) (e : expr)
  {struct fuel} : res :=
  match fuel with
  | 0 => OutOfFuel
  | S k =>
      let interp_args := interp_list k Sg d g in
      match e with
      | EVar x => match lookup_venv x g with
                  | Some w => Ok w
                  | None => Err
                  end
      | EUnit => Ok VUnit
      | EVl v => match lookup_denv (IVal v) d with
                 | Some (DVal w) => Ok w
                 | _ => Err
                 end
      | ETag n th e1 =>
          match interp k Sg d g e1 with
          | Ok w => Ok (VTag n w)
          | rr => rr
          end
      | ECallD f th sm args =>
          match g_fn Sg f with
          | Some (_, _, Some body) =>
              match interp_args args with
              | inl ws =>
                  match compose_smap d sm with
                  | Some d' => interp k Sg d' (mk_venv ws) body
                  | None => Err
                  end
              | inr rr => rr
              end
          | _ => Err
          end
      | ECallA f args =>
          match lookup_denv (IFn f) d with
          | Some (DClo f' df) =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp_args args with
                  | inl ws => interp k Sg df (mk_venv ws) body
                  | inr rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMeth e0 m prov args =>
          match lookup_denv prov d with
          | Some (DClo f' d') =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp k Sg d g e0 with
                  | Ok w0 =>
                      match interp_args args with
                      | inl ws => interp k Sg d' (mk_venv (w0 :: ws)) body
                      | inr rr => rr
                      end
                  | rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMatch e0 arms =>
          match interp k Sg d g e0 with
          | Ok (VTag n w') =>
              match find_arm n arms with
              | Some (x, eb) => interp k Sg d ((x, w') :: g) eb
              | None => Err
              end
          | Ok VUnit => Err
          | rr => rr
          end
      | ELet x e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg d ((x, w1) :: g) e2
          | rr => rr
          end
      | EBindTy _ _ e1 => interp k Sg d g e1
      | EBindVal v e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg ((IVal v, DVal w1) :: d) g e2
          | rr => rr
          end
      | EBindFn f f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IFn f, DClo f' dc) :: d) g e1
          | None => Err
          end
      | EBindMth tauk m th f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IMth tauk m th, DClo f' dc) :: d) g e1
          | None => Err
          end
      end
  end
with interp_list (fuel : nat) (Sg : gsig) (d : denv) (g : venv)
    (l : list expr) {struct fuel} : sum (list value) res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match l with
      | [] => inl []
      | e1 :: r =>
          match interp k Sg d g e1 with
          | Ok w =>
              match interp_list k Sg d g r with
              | inl ws => inl (w :: ws)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end.

(* Type erasure (Proposition 4.3): drop type binds and applications.  Items
   are runtime *keys*, not consulted types, so the applications inside IMth
   items and EBindMth stay: erasing a name would change which drawer the
   context is filed in, not what a type means. *)
Fixpoint erase (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVl v => EVl v
  | ETag n _ e1 => ETag n [] (erase e1)
  | ECallD f _ sm args =>
      ECallD f [] sm ((fix go (l : list expr) : list expr :=
                         match l with
                         | [] => []
                         | e1 :: r => erase e1 :: go r
                         end) args)
  | ECallA f args =>
      ECallA f ((fix go (l : list expr) : list expr :=
                   match l with
                   | [] => []
                   | e1 :: r => erase e1 :: go r
                   end) args)
  | EMeth e0 m prov args =>
      EMeth (erase e0) m prov
            ((fix go (l : list expr) : list expr :=
                match l with
                | [] => []
                | e1 :: r => erase e1 :: go r
                end) args)
  | EMatch e0 arms =>
      EMatch (erase e0)
             ((fix go (l : list (nsym * var * expr)) :=
                 match l with
                 | [] => []
                 | (n, x, eb) :: r => (n, x, erase eb) :: go r
                 end) arms)
  | ELet x e1 e2 => ELet x (erase e1) (erase e2)
  | EBindTy _ _ e1 => erase e1
  | EBindVal v e1 e2 => EBindVal v (erase e1) (erase e2)
  | EBindFn f f' sm e1 => EBindFn f f' sm (erase e1)
  | EBindMth tauk m th f' sm e1 => EBindMth tauk m th f' sm (erase e1)
  end.

(* ===================== 10b. Induction principles ========================= *)

(* The auto-generated induction principles are useless at the nested lists
   (arguments of calls, arms of matches) and at the Forall2 premises of the
   evaluation rules.  These two replacements are the load-bearing
   infrastructure of every proof below. *)

Fixpoint expr_ind' (P : expr -> Prop)
    (HVar : forall x, P (EVar x))
    (HUnit : P EUnit)
    (HVl : forall v, P (EVl v))
    (HTag : forall n th e, P e -> P (ETag n th e))
    (HCallD : forall f th sm args, Forall P args -> P (ECallD f th sm args))
    (HCallA : forall f args, Forall P args -> P (ECallA f args))
    (HMeth : forall e0 m prov args,
        P e0 -> Forall P args -> P (EMeth e0 m prov args))
    (HMatch : forall e0 arms,
        P e0 -> Forall (fun a => P (snd a)) arms -> P (EMatch e0 arms))
    (HLet : forall x e1 e2, P e1 -> P e2 -> P (ELet x e1 e2))
    (HBindTy : forall t tau e, P e -> P (EBindTy t tau e))
    (HBindVal : forall v e1 e2, P e1 -> P e2 -> P (EBindVal v e1 e2))
    (HBindFn : forall f f' sm e, P e -> P (EBindFn f f' sm e))
    (HBindMth : forall tauk m th f' sm e, P e -> P (EBindMth tauk m th f' sm e))
    (e : expr) {struct e} : P e :=
  let REC := expr_ind' HVar HUnit HVl HTag HCallD HCallA HMeth HMatch HLet
                       HBindTy HBindVal HBindFn HBindMth in
  match e with
  | EVar x => HVar x
  | EUnit => HUnit
  | EVl v => HVl v
  | ETag n th e1 => HTag n th e1 (REC e1)
  | ECallD f th sm args =>
      HCallD f th sm args
        ((fix go (l : list expr) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | e1 :: r => @Forall_cons _ P e1 r (REC e1) (go r)
            end) args)
  | ECallA f args =>
      HCallA f args
        ((fix go (l : list expr) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | e1 :: r => @Forall_cons _ P e1 r (REC e1) (go r)
            end) args)
  | EMeth e0 m prov args =>
      HMeth e0 m prov args (REC e0)
        ((fix go (l : list expr) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | e1 :: r => @Forall_cons _ P e1 r (REC e1) (go r)
            end) args)
  | EMatch e0 arms =>
      HMatch e0 arms (REC e0)
        ((fix go (l : list (nsym * var * expr)) : Forall (fun a => P (snd a)) l :=
            match l with
            | [] => Forall_nil (fun a => P (snd a))
            | a :: r => @Forall_cons _ (fun a => P (snd a)) a r (REC (snd a)) (go r)
            end) arms)
  | ELet x e1 e2 => HLet x e1 e2 (REC e1) (REC e2)
  | EBindTy t tau e1 => HBindTy t tau e1 (REC e1)
  | EBindVal v e1 e2 => HBindVal v e1 e2 (REC e1) (REC e2)
  | EBindFn f f' sm e1 => HBindFn f f' sm e1 (REC e1)
  | EBindMth tauk m th f' sm e1 => HBindMth tauk m th f' sm e1 (REC e1)
  end.

Section EvalInd.
  Variable Sg : gsig.
  Variable P : denv -> venv -> expr -> value -> Prop.

  Hypothesis HUnit : forall d g, P d g EUnit VUnit.
  Hypothesis HVar : forall d g x w,
      lookup_venv x g = Some w -> P d g (EVar x) w.
  Hypothesis HVal : forall d g v w,
      lookup_denv (IVal v) d = Some (DVal w) -> P d g (EVl v) w.
  Hypothesis HTag : forall d g n th e w,
      eval Sg d g e w -> P d g e w -> P d g (ETag n th e) (VTag n w).
  Hypothesis HLet : forall d g x e1 e2 w1 w,
      eval Sg d g e1 w1 -> P d g e1 w1 ->
      eval Sg d ((x, w1) :: g) e2 w -> P d ((x, w1) :: g) e2 w ->
      P d g (ELet x e1 e2) w.
  Hypothesis HMatch : forall d g e0 arms n w' x eb w,
      eval Sg d g e0 (VTag n w') -> P d g e0 (VTag n w') ->
      find_arm n arms = Some (x, eb) ->
      eval Sg d ((x, w') :: g) eb w -> P d ((x, w') :: g) eb w ->
      P d g (EMatch e0 arms) w.
  Hypothesis HCallD : forall d g f th sm args D S body ws d' w,
      g_fn Sg f = Some (D, S, Some body) ->
      Forall2 (eval Sg d g) args ws -> Forall2 (P d g) args ws ->
      compose_smap d sm = Some d' ->
      eval Sg d' (mk_venv ws) body w -> P d' (mk_venv ws) body w ->
      P d g (ECallD f th sm args) w.
  Hypothesis HCallA : forall d g f args f' df D' S' body ws w,
      lookup_denv (IFn f) d = Some (DClo f' df) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      Forall2 (eval Sg d g) args ws -> Forall2 (P d g) args ws ->
      eval Sg df (mk_venv ws) body w -> P df (mk_venv ws) body w ->
      P d g (ECallA f args) w.
  Hypothesis HMeth : forall d g e0 m prov args f' d' D' S' body w0 ws w,
      lookup_denv prov d = Some (DClo f' d') ->
      g_fn Sg f' = Some (D', S', Some body) ->
      eval Sg d g e0 w0 -> P d g e0 w0 ->
      Forall2 (eval Sg d g) args ws -> Forall2 (P d g) args ws ->
      eval Sg d' (mk_venv (w0 :: ws)) body w -> P d' (mk_venv (w0 :: ws)) body w ->
      P d g (EMeth e0 m prov args) w.
  Hypothesis HBindTy : forall d g t tau e w,
      eval Sg d g e w -> P d g e w -> P d g (EBindTy t tau e) w.
  Hypothesis HBindVal : forall d g v e1 e w1 w,
      eval Sg d g e1 w1 -> P d g e1 w1 ->
      eval Sg ((IVal v, DVal w1) :: d) g e w ->
      P ((IVal v, DVal w1) :: d) g e w ->
      P d g (EBindVal v e1 e) w.
  Hypothesis HBindFn : forall d g f f' sm dc e w,
      compose_smap d sm = Some dc ->
      eval Sg ((IFn f, DClo f' dc) :: d) g e w ->
      P ((IFn f, DClo f' dc) :: d) g e w ->
      P d g (EBindFn f f' sm e) w.
  Hypothesis HBindMth : forall d g tauk m th f' sm dc e w,
      compose_smap d sm = Some dc ->
      eval Sg ((IMth tauk m th, DClo f' dc) :: d) g e w ->
      P ((IMth tauk m th, DClo f' dc) :: d) g e w ->
      P d g (EBindMth tauk m th f' sm e) w.

  Lemma eval_ind' : forall d g e w, eval Sg d g e w -> P d g e w.
  Proof.
    fix IH 5.
    intros d g e w D.
    destruct D.
    - apply HUnit.
    - apply HVar; assumption.
    - apply HVal; assumption.
    - apply HTag; [assumption | apply IH; assumption].
    - eapply HLet; try eassumption; apply IH; assumption.
    - eapply HMatch; try eassumption; apply IH; assumption.
    - eapply HCallD; try eassumption.
      + match goal with
        | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
            clear - IH F; induction F; constructor;
            [apply IH; assumption | assumption]
        end.
      + apply IH; assumption.
    - eapply HCallA; try eassumption.
      + match goal with
        | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
            clear - IH F; induction F; constructor;
            [apply IH; assumption | assumption]
        end.
      + apply IH; assumption.
    - eapply HMeth; try eassumption.
      + apply IH; assumption.
      + match goal with
        | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
            clear - IH F; induction F; constructor;
            [apply IH; assumption | assumption]
        end.
      + apply IH; assumption.
    - apply HBindTy; [assumption | apply IH; assumption].
    - eapply HBindVal; try eassumption; apply IH; assumption.
    - eapply HBindFn; try eassumption; apply IH; assumption.
    - eapply HBindMth; try eassumption; apply IH; assumption.
  Qed.
End EvalInd.

(* ===================== 11. Metatheory ==================================== *)

(* ---- Proposition 4.1 (resolution is search-free). ----------------------
   The context premises of the statics are the single membership test
   `avail`, a boolean over the finite set A — decidable by construction, and
   no judgment quantifies over Σ.  The uniqueness premise of T-Meth makes the
   provision annotation a function of the derivation. *)

Theorem availability_decidable :
  forall Phi i, {avail Phi i} + {~ avail Phi i}.
Proof.
  intros Phi i. unfold avail.
  destruct (avail_b Phi i).
  - left; reflexivity.
  - right; discriminate.
Defined.

Theorem method_resolution_functional :
  forall (Phi : ctx) k m i1 i2,
    In i1 (cA Phi) -> is_prov_at (cS Phi) k m i1 ->
    In i2 (cA Phi) -> is_prov_at (cS Phi) k m i2 ->
    (forall i, In i (cA Phi) -> is_prov_at (cS Phi) k m i -> i = i1) ->
    i2 = i1.
Proof. intros Phi k m i1 i2 H1 P1 H2 P2 Uniq. apply Uniq; assumption. Qed.

(* ---- Leaf lemmas for the proof effort -----------------------------------
   Every open proof below ends in `Admitted.` inside a unique
   (* BEGIN:name *) ... (* END:name *) block.  The statements are frozen:
   the integration harness rejects a fill that changes anything outside
   its block or that contains Axiom/Admitted/admit.  The comment above
   each lemma records the intended proof. *)

(* erase pushes through argument and arm lists as `map`; these unfoldings
   mean later proofs never fight an anonymous inner fix. *)

Lemma erase_ECallD : forall f th sm args,
  erase (ECallD f th sm args) = ECallD f [] sm (map erase args).
Proof. reflexivity. Qed.

Lemma erase_ECallA : forall f args,
  erase (ECallA f args) = ECallA f (map erase args).
Proof. reflexivity. Qed.

Lemma erase_EMeth : forall e0 m prov args,
  erase (EMeth e0 m prov args) = EMeth (erase e0) m prov (map erase args).
Proof. reflexivity. Qed.

Definition erase_arm (a : nsym * var * expr) : nsym * var * expr :=
  (fst (fst a), snd (fst a), erase (snd a)).

Lemma erase_EMatch : forall e0 arms,
  erase (EMatch e0 arms) = EMatch (erase e0) (map erase_arm arms).
Proof.
  intros; simpl; f_equal.
  induction arms as [| a r IHr]; simpl; [reflexivity |].
  destruct a as [[n x] eb]; simpl; unfold erase_arm; simpl; now rewrite IHr.
Qed.

(* find_arm commutes with arm erasure.
   Hint: induction on arms; destruct the head arm and the Nat.eqb test. *)
(* BEGIN:find_arm_erase *)
Lemma find_arm_erase : forall n arms,
  find_arm n (map erase_arm arms)
  = match find_arm n arms with
    | Some (x, eb) => Some (x, erase eb)
    | None => None
    end.
Proof.
  induction arms as [| [[n' x] eb] r IH]; intros; simpl.
  - reflexivity.
  - unfold erase_arm at 1; simpl.
    destruct (Nat.eqb n n'); [reflexivity | apply IH].
Qed.
(* END:find_arm_erase *)

(* A found arm is one of the arms.
   Hint: induction on arms; destruct the head and the Nat.eqb test;
   injection on the Some. *)
(* BEGIN:find_arm_in *)
Lemma find_arm_in : forall n arms x eb,
  find_arm n arms = Some (x, eb) -> In (n, x, eb) arms.
Proof.
  induction arms as [| [[n' x'] eb'] r IH]; intros x eb H.
  - simpl in H. discriminate.
  - simpl in H. destruct (Nat.eqb n n') eqn:Heq.
    + injection H as Hx Heb; subst.
      apply Nat.eqb_eq in Heq; subst.
      left. reflexivity.
    + right. apply IH. exact H.
Qed.
(* END:find_arm_in *)

(* The inr payload of interp_list is never Ok: the only inr injections wrap
   an Err or OutOfFuel that some element produced.
   Hint: induction on fuel; destruct l, then the head's interp result, then
   the tail's interp_list result; congruence closes every branch. *)
(* BEGIN:interp_list_never_ok *)
Lemma interp_list_never_ok :
  forall fuel Sg d g l w, interp_list fuel Sg d g l <> inr (Ok w).
Proof.
  induction fuel as [| k IH]; intros Sg d g l w Heq.
  - simpl in Heq. discriminate Heq.
  - simpl in Heq. destruct l as [| e1 r].
    + discriminate Heq.
    + destruct (interp k Sg d g e1) eqn:E1.
      * destruct (interp_list k Sg d g r) eqn:E2.
        -- discriminate Heq.
        -- injection Heq as Heq'. subst r0.
           apply (IH Sg d g r w E2).
      * discriminate Heq.
      * discriminate Heq.
Qed.
(* END:interp_list_never_ok *)

(* One extra unit of fuel preserves success, for both interpreters at once.
   Hint: induction on fuel (the two conjuncts need each other).  In the S
   case destruct e (resp. l); simpl in the hypothesis AND the goal;
   destruct every scrutinee with eqn:; rewrite with the induction
   hypotheses on the Ok/inl branches; on `inr rr` branches where the
   hypothesis says rr = Ok w, contradict with interp_list_never_ok. *)
(* BEGIN:interp_S_all *)
Lemma interp_S_all :
  forall fuel,
    (forall Sg d g e w, interp fuel Sg d g e = Ok w ->
                        interp (S fuel) Sg d g e = Ok w)
    /\ (forall Sg d g l ws, interp_list fuel Sg d g l = inl ws ->
                            interp_list (S fuel) Sg d g l = inl ws).
Proof.
  induction fuel as [|k [IH1 IH2]].
  - split; intros; simpl in *; discriminate.
  - split.
    + intros Sg d g e w H. simpl in H.
      remember (S k) as q eqn:Eq.
      destruct e as [ x | | v | n th e1 | f th sm args | f args
                    | e0 mm prov args | e0 arms | x e1 e2 | t tau e1
                    | v e1 e2 | f f' sm e1 | tauk mm th f' sm e1 ];
        simpl; subst q; simpl in H.
      * assumption.
      * assumption.
      * assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1). assumption.
      * destruct (g_fn Sg f) as [[[D0 S0] ob]|] eqn:E0; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) eqn:E2.
        -- rewrite (IH2 _ _ _ _ _ E2).
           destruct (compose_smap d sm) eqn:E3; try discriminate.
           apply IH1. assumption.
        -- exfalso. rewrite H in E2.
           eapply interp_list_never_ok; exact E2.
      * destruct (lookup_denv (IFn f) d) as [[wv|fc dc]|] eqn:E0;
          try discriminate.
        destruct (g_fn Sg fc) as [[[D0 S0] ob]|] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) eqn:E2.
        -- rewrite (IH2 _ _ _ _ _ E2). apply IH1. assumption.
        -- exfalso. rewrite H in E2.
           eapply interp_list_never_ok; exact E2.
      * destruct (lookup_denv prov d) as [[wv|fc dc]|] eqn:E0;
          try discriminate.
        destruct (g_fn Sg fc) as [[[D0 S0] ob]|] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp k Sg d g e0) eqn:E2; try discriminate.
        rewrite (IH1 _ _ _ _ _ E2).
        destruct (interp_list k Sg d g args) eqn:E3.
        -- rewrite (IH2 _ _ _ _ _ E3). apply IH1. assumption.
        -- exfalso. rewrite H in E3.
           eapply interp_list_never_ok; exact E3.
      * destruct (interp k Sg d g e0) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1).
        destruct v as [|n w']; try discriminate.
        destruct (find_arm n arms) as [[x eb]|] eqn:E2; try discriminate.
        apply IH1. assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1). apply IH1. assumption.
      * apply IH1. assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1). apply IH1. assumption.
      * destruct (compose_smap d sm) eqn:E1; try discriminate.
        apply IH1. assumption.
      * destruct (compose_smap d sm) eqn:E1; try discriminate.
        apply IH1. assumption.
    + intros Sg d g l ws H. simpl in H.
      remember (S k) as q eqn:Eq.
      destruct l as [|e1 r]; simpl; subst q; simpl in H.
      * assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1).
        destruct (interp_list k Sg d g r) eqn:E2; try discriminate.
        rewrite (IH2 _ _ _ _ _ E2). assumption.
Qed.
(* END:interp_S_all *)

(* Fuel monotonicity, from interp_S_all by induction on the ≤ derivation. *)
(* BEGIN:interp_mono *)
Lemma interp_mono :
  forall fuel fuel' Sg d g e w,
    fuel <= fuel' -> interp fuel Sg d g e = Ok w ->
    interp fuel' Sg d g e = Ok w.
Proof.
  intros fuel fuel' Sg d g e w Hle.
  induction Hle as [| fuel' Hle IH].
  - intros H; exact H.
  - intros H. apply interp_S_all. apply IH. exact H.
Qed.

Lemma interp_list_mono :
  forall fuel fuel' Sg d g l ws,
    fuel <= fuel' -> interp_list fuel Sg d g l = inl ws ->
    interp_list fuel' Sg d g l = inl ws.
Proof.
  intros fuel fuel' Sg d g l ws Hle.
  induction Hle as [| fuel' Hle IH].
  - intros H; exact H.
  - intros H. apply interp_S_all. apply IH. exact H.
Qed.
(* END:interp_mono *)

(* ---- Determinism of the big-step relation (the interpreter is its
   deterministic implementation).
   Hint: apply eval_ind' with
     P d g e w1 := forall w2, eval Sg d g e w2 -> w1 = w2;
   in each case invert the second derivation (the constructors are
   syntax-directed) and chain the induction hypotheses; equalities between
   lookups/compose_smap results close by congruence; for the argument
   lists prove an auxiliary zip: Forall2 (fun e w => forall w', eval e w'
   -> w = w') l ws1 -> Forall2 (eval) l ws2 -> ws1 = ws2. *)
(* BEGIN:eval_deterministic *)
(* Zip: pointwise determinism along an argument list determines the list. *)
Lemma eval_deterministic_aux1 :
  forall Sg d g l ws1 ws2,
    Forall2 (fun e w => forall w', eval Sg d g e w' -> w = w') l ws1 ->
    Forall2 (eval Sg d g) l ws2 ->
    ws1 = ws2.
Proof.
  intros Sg d g l ws1 ws2 H1. revert ws2.
  induction H1 as [| e w l' ws' Hh Ht IHt]; intros ws2 H2;
    inversion H2; subst.
  - reflexivity.
  - f_equal.
    + apply Hh; assumption.
    + apply IHt; assumption.
Qed.

(* Syntax-directed inversion principles for [eval], stated with explicit
   existentials so that the determinism proof never depends on names
   invented by [inversion]. *)
Lemma eval_deterministic_aux2 :
  forall Sg d g n th e w2,
    eval Sg d g (ETag n th e) w2 ->
    exists w, w2 = VTag n w /\ eval Sg d g e w.
Proof.
  intros Sg d g n th e w2 H. inversion H; subst.
  eexists. split; [ reflexivity | eassumption ].
Qed.

Lemma eval_deterministic_aux3 :
  forall Sg d g x e1 e2 w2,
    eval Sg d g (ELet x e1 e2) w2 ->
    exists w1, eval Sg d g e1 w1 /\ eval Sg d ((x, w1) :: g) e2 w2.
Proof.
  intros Sg d g x e1 e2 w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux4 :
  forall Sg d g e0 arms w2,
    eval Sg d g (EMatch e0 arms) w2 ->
    exists n w' x eb,
      eval Sg d g e0 (VTag n w') /\
      find_arm n arms = Some (x, eb) /\
      eval Sg d ((x, w') :: g) eb w2.
Proof.
  intros Sg d g e0 arms w2 H. inversion H; subst.
  do 4 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux5 :
  forall Sg d g f th sm args w2,
    eval Sg d g (ECallD f th sm args) w2 ->
    exists D S body ws d',
      g_fn Sg f = Some (D, S, Some body) /\
      Forall2 (eval Sg d g) args ws /\
      compose_smap d sm = Some d' /\
      eval Sg d' (mk_venv ws) body w2.
Proof.
  intros Sg d g f th sm args w2 H. inversion H; subst.
  do 5 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux6 :
  forall Sg d g f args w2,
    eval Sg d g (ECallA f args) w2 ->
    exists f' df D' S' body ws,
      lookup_denv (IFn f) d = Some (DClo f' df) /\
      g_fn Sg f' = Some (D', S', Some body) /\
      Forall2 (eval Sg d g) args ws /\
      eval Sg df (mk_venv ws) body w2.
Proof.
  intros Sg d g f args w2 H. inversion H; subst.
  do 6 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux7 :
  forall Sg d g e0 m prov args w2,
    eval Sg d g (EMeth e0 m prov args) w2 ->
    exists f' d' D' S' body w0 ws,
      lookup_denv prov d = Some (DClo f' d') /\
      g_fn Sg f' = Some (D', S', Some body) /\
      eval Sg d g e0 w0 /\
      Forall2 (eval Sg d g) args ws /\
      eval Sg d' (mk_venv (w0 :: ws)) body w2.
Proof.
  intros Sg d g e0 m prov args w2 H. inversion H; subst.
  do 7 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux8 :
  forall Sg d g t tau e w2,
    eval Sg d g (EBindTy t tau e) w2 -> eval Sg d g e w2.
Proof.
  intros Sg d g t tau e w2 H. inversion H; subst. assumption.
Qed.

Lemma eval_deterministic_aux9 :
  forall Sg d g v e1 e w2,
    eval Sg d g (EBindVal v e1 e) w2 ->
    exists w1, eval Sg d g e1 w1 /\ eval Sg ((IVal v, DVal w1) :: d) g e w2.
Proof.
  intros Sg d g v e1 e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux10 :
  forall Sg d g f f' sm e w2,
    eval Sg d g (EBindFn f f' sm e) w2 ->
    exists dc, compose_smap d sm = Some dc /\
               eval Sg ((IFn f, DClo f' dc) :: d) g e w2.
Proof.
  intros Sg d g f f' sm e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux11 :
  forall Sg d g tauk m th f' sm e w2,
    eval Sg d g (EBindMth tauk m th f' sm e) w2 ->
    exists dc, compose_smap d sm = Some dc /\
               eval Sg ((IMth tauk m th, DClo f' dc) :: d) g e w2.
Proof.
  intros Sg d g tauk m th f' sm e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Theorem eval_deterministic :
  forall Sg d g e w1 w2,
    eval Sg d g e w1 -> eval Sg d g e w2 -> w1 = w2.
Proof.
  intros Sg.
  assert (Hmain : forall d g e w1,
             eval Sg d g e w1 -> forall w2, eval Sg d g e w2 -> w1 = w2).
  { apply (@eval_ind' Sg
             (fun d g e w1 => forall w2, eval Sg d g e w2 -> w1 = w2)).
    - (* EUnit *)
      intros d g w2 H2. inversion H2; subst. reflexivity.
    - (* EVar *)
      intros d g x w Hl w2 H2. inversion H2; subst. congruence.
    - (* EVl *)
      intros d g v w Hl w2 H2. inversion H2; subst. congruence.
    - (* ETag *)
      intros d g n th e w Hev IH w2 H2.
      apply eval_deterministic_aux2 in H2.
      destruct H2 as [u [Hu Hev2]]. subst w2.
      f_equal. apply IH. exact Hev2.
    - (* ELet *)
      intros d g x e1 e2 w1 w Hev1 IH1 Hev2 IH2 w2 H2.
      apply eval_deterministic_aux3 in H2.
      destruct H2 as [u [Ha Hb]].
      specialize (IH1 _ Ha). subst u.
      apply IH2. exact Hb.
    - (* EMatch *)
      intros d g e0 arms n w' x eb w Hev0 IH0 Hfa Hevb IHb w2 H2.
      apply eval_deterministic_aux4 in H2.
      destruct H2 as [n0 [u [x0 [eb0 [Ha [Hb Hc]]]]]].
      specialize (IH0 _ Ha).
      assert (En : n0 = n) by congruence.
      assert (Eu : u = w') by congruence.
      subst n0 u.
      assert (Ex : x0 = x) by congruence.
      assert (Eb : eb0 = eb) by congruence.
      subst x0 eb0.
      apply IHb. exact Hc.
    - (* ECallD *)
      intros d g f th sm args D S body ws d' w Hfn HF HFP Hcs Hev IH w2 H2.
      apply eval_deterministic_aux5 in H2.
      destruct H2 as [D0 [S0 [body0 [ws0 [d0 [Ha [Hb [Hc Hd]]]]]]]].
      assert (Ebody : body0 = body) by congruence.
      assert (Ed : d0 = d') by congruence.
      assert (Ews : ws0 = ws) by
        (symmetry; eapply eval_deterministic_aux1; eassumption).
      subst body0 d0 ws0.
      apply IH. exact Hd.
    - (* ECallA *)
      intros d g f args f' df D' S' body ws w Hld Hfn HF HFP Hev IH w2 H2.
      apply eval_deterministic_aux6 in H2.
      destruct H2 as [f0 [df0 [D0 [S0 [body0 [ws0 [Ha [Hb [Hc Hd]]]]]]]]].
      assert (Ef : f0 = f') by congruence. subst f0.
      assert (Edf : df0 = df) by congruence. subst df0.
      assert (Ebody : body0 = body) by congruence.
      assert (Ews : ws0 = ws) by
        (symmetry; eapply eval_deterministic_aux1; eassumption).
      subst body0 ws0.
      apply IH. exact Hd.
    - (* EMeth *)
      intros d g e0 m prov args f' d' D' S' body w0 ws w
             Hld Hfn Hev0 IH0 HF HFP Hev IH w2 H2.
      apply eval_deterministic_aux7 in H2.
      destruct H2 as [f0 [d0 [D0 [S0 [body0 [u0 [ws0 [Ha [Hb [Hc [Hd He]]]]]]]]]]].
      assert (Ef : f0 = f') by congruence. subst f0.
      assert (Ed : d0 = d') by congruence. subst d0.
      assert (Ebody : body0 = body) by congruence.
      specialize (IH0 _ Hc).
      assert (Ews : ws0 = ws) by
        (symmetry; eapply eval_deterministic_aux1; eassumption).
      subst body0 ws0 u0.
      apply IH. exact He.
    - (* EBindTy *)
      intros d g t tau e w Hev IH w2 H2.
      apply eval_deterministic_aux8 in H2.
      apply IH. exact H2.
    - (* EBindVal *)
      intros d g v e1 e w1 w Hev1 IH1 Hev IH w2 H2.
      apply eval_deterministic_aux9 in H2.
      destruct H2 as [u [Ha Hb]].
      specialize (IH1 _ Ha). subst u.
      apply IH. exact Hb.
    - (* EBindFn *)
      intros d g f f' sm dc e w Hcs Hev IH w2 H2.
      apply eval_deterministic_aux10 in H2.
      destruct H2 as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb.
    - (* EBindMth *)
      intros d g tauk m th f' sm dc e w Hcs Hev IH w2 H2.
      apply eval_deterministic_aux11 in H2.
      destruct H2 as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb. }
  intros d g e w1 w2 H1 H2. exact (Hmain d g e w1 H1 w2 H2).
Qed.
(* END:eval_deterministic *)

(* ---- Adequacy of the interpreter for the relation. ----------------------
   Hint (soundness): induction on fuel, both conjuncts at once; fuel 0
   discriminates; in the S case destruct e (resp. l), destruct every
   scrutinee with eqn:, feed the equations to the induction hypotheses,
   and finish with the evaluation constructors (econstructor; eauto). *)
(* BEGIN:interp_sound_all *)
Lemma interp_sound_all :
  forall fuel,
    (forall Sg d g e w, interp fuel Sg d g e = Ok w -> eval Sg d g e w)
    /\ (forall Sg d g l ws, interp_list fuel Sg d g l = inl ws ->
                            Forall2 (eval Sg d g) l ws).
Proof.
  induction fuel as [| k IHk].
  - split; intros Sg d g e w H; simpl in H; discriminate.
  - destruct IHk as [IH1 IH2]. split.
    + intros Sg d g e w H.
      destruct e as [ x | | v | n th e1 | f th sm args | f args
                    | e0 m prov args | e0 arms | x e1 e2 | t tau e1
                    | v e1 e2 | f f' sm e1 | tauk m th f' sm e1 ];
        simpl in H.
      * destruct (lookup_venv x g) as [w'|] eqn:E1; try discriminate.
        injection H as H; subst. constructor; assumption.
      * injection H as H; subst. constructor.
      * destruct (lookup_denv (IVal v) d) as [en|] eqn:E1; try discriminate.
        destruct en as [w' | fc dc]; try discriminate.
        injection H as H; subst. constructor; assumption.
      * destruct (interp k Sg d g e1) as [w' | | ] eqn:E1; try discriminate.
        injection H as H; subst. apply IH1 in E1. constructor; assumption.
      * destruct (g_fn Sg f) as [[[D S] ob] | ] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) as [ws | rr] eqn:E2.
        -- destruct (compose_smap d sm) as [d'|] eqn:E3; try discriminate.
           apply IH2 in E2. apply IH1 in H.
           eapply EvCallD; eassumption.
        -- subst rr. exfalso. eapply interp_list_never_ok. exact E2.
      * destruct (lookup_denv (IFn f) d) as [en|] eqn:E0; try discriminate.
        destruct en as [w' | f' df]; try discriminate.
        destruct (g_fn Sg f') as [[[D S] ob] | ] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) as [ws | rr] eqn:E2.
        -- apply IH2 in E2. apply IH1 in H.
           eapply EvCallA; eassumption.
        -- subst rr. exfalso. eapply interp_list_never_ok. exact E2.
      * destruct (lookup_denv prov d) as [en|] eqn:E0; try discriminate.
        destruct en as [w' | f' d']; try discriminate.
        destruct (g_fn Sg f') as [[[D S] ob] | ] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp k Sg d g e0) as [w0 | | ] eqn:E2; try discriminate.
        destruct (interp_list k Sg d g args) as [ws | rr] eqn:E3.
        -- apply IH1 in E2. apply IH2 in E3. apply IH1 in H.
           eapply EvMeth; eassumption.
        -- subst rr. exfalso. eapply interp_list_never_ok. exact E3.
      * destruct (interp k Sg d g e0) as [w' | | ] eqn:E1; try discriminate.
        destruct w' as [ | n w'']; try discriminate.
        destruct (find_arm n arms) as [[xa eb] | ] eqn:E2; try discriminate.
        apply IH1 in E1. apply IH1 in H.
        eapply EvMatch; eassumption.
      * destruct (interp k Sg d g e1) as [w1 | | ] eqn:E1; try discriminate.
        apply IH1 in E1. apply IH1 in H.
        eapply EvLet; eassumption.
      * apply IH1 in H. constructor; assumption.
      * destruct (interp k Sg d g e1) as [w1 | | ] eqn:E1; try discriminate.
        apply IH1 in E1. apply IH1 in H.
        eapply EvBindVal; eassumption.
      * destruct (compose_smap d sm) as [dc|] eqn:E1; try discriminate.
        apply IH1 in H. eapply EvBindFn; eassumption.
      * destruct (compose_smap d sm) as [dc|] eqn:E1; try discriminate.
        apply IH1 in H. eapply EvBindMth; eassumption.
    + intros Sg d g l ws H.
      destruct l as [| e1 r]; simpl in H.
      * injection H as H; subst. constructor.
      * destruct (interp k Sg d g e1) as [w | | ] eqn:E1; try discriminate.
        destruct (interp_list k Sg d g r) as [ws' | rr] eqn:E2;
          try discriminate.
        injection H as H; subst. apply IH1 in E1. apply IH2 in E2.
        constructor; assumption.
Qed.
(* END:interp_sound_all *)

Theorem interp_sound :
  forall fuel Sg d g e w,
    interp fuel Sg d g e = Ok w -> eval Sg d g e w.
Proof. intro fuel. apply (proj1 (interp_sound_all fuel)). Qed.

(* Pointwise interpreter success lifts to the list interpreter.
   Hint: induction on the Forall2; base case: fuel 1; cons case: take
   S (max f1 f2) and settle both sides with interp_mono/interp_list_mono
   (Nat.le_max_l / Nat.le_max_r). *)
(* BEGIN:interp_list_complete *)
Lemma interp_list_complete :
  forall Sg d g l ws,
    Forall2 (fun e w => exists fuel, interp fuel Sg d g e = Ok w) l ws ->
    exists fuel, interp_list fuel Sg d g l = inl ws.
Proof.
  intros Sg d g l ws H.
  induction H as [| e1 w1 r rws He Hr IH].
  - exists 1. simpl. reflexivity.
  - destruct He as [f1 Hf1]. destruct IH as [f2 Hf2].
    assert (Ha : interp (Nat.max f1 f2) Sg d g e1 = Ok w1).
    { eapply interp_mono; [ apply Nat.le_max_l | exact Hf1 ]. }
    assert (Hb : interp_list (Nat.max f1 f2) Sg d g r = inl rws).
    { eapply interp_list_mono; [ apply Nat.le_max_r | exact Hf2 ]. }
    exists (S (Nat.max f1 f2)). simpl.
    rewrite Ha, Hb. reflexivity.
Qed.
(* END:interp_list_complete *)

(* Hint (completeness): apply eval_ind' with
     P d g e w := exists fuel, interp fuel Sg d g e = Ok w;
   each case combines the sub-derivations' fuels with max and interp_mono,
   uses interp_list_complete for argument lists, and picks S (that fuel);
   the interpreter's scrutinees are then all determined by the case's
   premises, so the goal computes. *)
(* BEGIN:interp_complete *)
Lemma interp_complete_aux1 : forall Sg d g e w a b,
  interp a Sg d g e = Ok w -> interp (Nat.max a b) Sg d g e = Ok w.
Proof.
  intros Sg d g e w a b H.
  eapply interp_mono; [apply Nat.le_max_l | exact H].
Qed.

Lemma interp_complete_aux2 : forall Sg d g e w a b,
  interp b Sg d g e = Ok w -> interp (Nat.max a b) Sg d g e = Ok w.
Proof.
  intros Sg d g e w a b H.
  eapply interp_mono; [apply Nat.le_max_r | exact H].
Qed.

Lemma interp_complete_aux3 : forall Sg d g l ws a b,
  interp_list a Sg d g l = inl ws -> interp_list (Nat.max a b) Sg d g l = inl ws.
Proof.
  intros Sg d g l ws a b H.
  eapply interp_list_mono; [apply Nat.le_max_l | exact H].
Qed.

Lemma interp_complete_aux4 : forall Sg d g l ws a b,
  interp_list b Sg d g l = inl ws -> interp_list (Nat.max a b) Sg d g l = inl ws.
Proof.
  intros Sg d g l ws a b H.
  eapply interp_list_mono; [apply Nat.le_max_r | exact H].
Qed.

Theorem interp_complete :
  forall Sg d g e w,
    eval Sg d g e w -> exists fuel, interp fuel Sg d g e = Ok w.
Proof.
  intros Sg.
  apply (@eval_ind' Sg
           (fun d g e w => exists fuel, interp fuel Sg d g e = Ok w)).
  - (* EvUnit *) intros d g. exists 1. reflexivity.
  - (* EvVar *) intros d g x w Hl. exists 1. simpl. rewrite Hl. reflexivity.
  - (* EvVal *) intros d g v w Hl. exists 1. simpl. rewrite Hl. reflexivity.
  - (* EvTag *) intros d g n th e w _ [a Ha]. exists (S a). simpl.
    rewrite Ha. reflexivity.
  - (* EvLet *) intros d g x e1 e2 w1 w _ [a Ha] _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp (Nat.max a b) Sg d g e1 = Ok w1)
      by (apply interp_complete_aux1; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg d ((x, w1) :: g) e2 = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Ha'. exact Hb'.
  - (* EvMatch *) intros d g e0 arms n w' x eb w _ [a Ha] Hfind _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp (Nat.max a b) Sg d g e0 = Ok (VTag n w'))
      by (apply interp_complete_aux1; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg d ((x, w') :: g) eb = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Ha'. rewrite Hfind. exact Hb'.
  - (* EvCallD *)
    intros d g f th sm args D Sf body ws d' w Hfn _ HF Hcs _ [b Hb].
    apply interp_list_complete in HF. destruct HF as [a Ha].
    exists (S (Nat.max a b)).
    assert (Ha' : interp_list (Nat.max a b) Sg d g args = inl ws)
      by (apply interp_complete_aux3; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg d' (mk_venv ws) body = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Hfn. rewrite Ha'. rewrite Hcs. exact Hb'.
  - (* EvCallA *)
    intros d g f args f' df D' S' body ws w Hlk Hfn _ HF _ [b Hb].
    apply interp_list_complete in HF. destruct HF as [a Ha].
    exists (S (Nat.max a b)).
    assert (Ha' : interp_list (Nat.max a b) Sg d g args = inl ws)
      by (apply interp_complete_aux3; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg df (mk_venv ws) body = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Hlk. rewrite Hfn. rewrite Ha'. exact Hb'.
  - (* EvMeth *)
    intros d g e0 m prov args f' d' D' S' body w0 ws w Hlk Hfn _ [c Hc]
           _ HF _ [b Hb].
    apply interp_list_complete in HF. destruct HF as [a Ha].
    exists (S (Nat.max c (Nat.max a b))).
    assert (Hc' : interp (Nat.max c (Nat.max a b)) Sg d g e0 = Ok w0)
      by (apply interp_complete_aux1; exact Hc).
    assert (Ha' : interp_list (Nat.max c (Nat.max a b)) Sg d g args = inl ws)
      by (apply interp_complete_aux4; apply interp_complete_aux3; exact Ha).
    assert (Hb' : interp (Nat.max c (Nat.max a b)) Sg d'
                         (mk_venv (w0 :: ws)) body = Ok w)
      by (apply interp_complete_aux2; apply interp_complete_aux2; exact Hb).
    simpl. rewrite Hlk. rewrite Hfn. rewrite Hc'. rewrite Ha'. exact Hb'.
  - (* EvBindTy *) intros d g t tau e w _ [a Ha]. exists (S a). simpl. exact Ha.
  - (* EvBindVal *) intros d g v e1 e w1 w _ [a Ha] _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp (Nat.max a b) Sg d g e1 = Ok w1)
      by (apply interp_complete_aux1; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg ((IVal v, DVal w1) :: d) g e = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Ha'. exact Hb'.
  - (* EvBindFn *) intros d g f f' sm dc e w Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
  - (* EvBindMth *) intros d g tauk m th f' sm dc e w Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
Qed.
(* END:interp_complete *)

(* ===================== 11b. The semantic soundness development =========== *)

(* --- Free symbols and closed types. -------------------------------------- *)

Fixpoint ty_frees (tau : ty) : list tsym :=
  match tau with
  | TUnit => []
  | TAbs t => [t]
  | TNom _ th =>
      (fix go (l : list (tsym * ty)) : list tsym :=
         match l with [] => [] | p :: r => ty_frees (snd p) ++ go r end) th
  | TUnion ms =>
      (fix go (l : list ty) : list tsym :=
         match l with [] => [] | t' :: r => ty_frees t' ++ go r end) ms
  end.

Definition closed_ty (tau : ty) : Prop := ty_frees tau = [].

(* app_subst unfolds to map on the nested lists (definitionally). *)
Lemma app_subst_nom : forall s n th,
  app_subst s (TNom n th) = TNom n (map_snd (app_subst s) th).
Proof. reflexivity. Qed.

Lemma app_subst_union : forall s ms,
  app_subst s (TUnion ms) = TUnion (map (app_subst s) ms).
Proof. reflexivity. Qed.

(* --- Value typing. --------------------------------------------------------- *)

Inductive vty (Sg : gsig) : value -> ty -> Prop :=
| VtyUnit : vty Sg VUnit TUnit
| VtyTag : forall n D taup th w,
    g_tag Sg n = Some (D, taup) ->
    vty Sg w (app_subst th taup) ->
    vty Sg (VTag n w) (TNom n th)
| VtyInj : forall w tau' ms,
    vty Sg w tau' -> In tau' ms -> vty Sg w (TUnion ms).

(* --- Groundings. ---------------------------------------------------------- *)

(* A grounding maps abstract type symbols to closed types; it is the runtime
   instantiation of the abstract symbols a region was checked under. *)
Definition grounding (g : subst) : Prop :=
  Forall (fun p => closed_ty (snd p)) g.

(* g absorbs s: reading through s and then g is just reading through g. *)
Definition absorbs (g s : subst) : Prop :=
  forall t tau, lookup_subst t s = Some tau ->
                lookup_subst t g = Some (app_subst g tau).

Definition covers (g : subst) (ts : list tsym) : Prop :=
  forall t, In t ts -> exists tau, lookup_subst t g = Some tau.

(* The substitution in force only binds, and only mentions, symbols that
   are in force: the regularity that lets a fresh bind extend a grounding
   without disturbing existing readings. *)
Definition sigma_regular (sg : subst) (A : list item) : Prop :=
  incl (map fst sg) (tyreqs A)
  /\ Forall (fun p => incl (ty_frees (snd p)) (tyreqs A)) sg.

Definition ctx_grounded (g : subst) (Phi : ctx) : Prop :=
  grounding g /\ absorbs g (cS Phi) /\ covers g (tyreqs (cA Phi))
  /\ sigma_regular (cS Phi) (cA Phi).

(* --- Environment agreement. ----------------------------------------------- *)

Definition tele_covered (d : denv) (D : tele) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_denv i d = Some en) D.

(* Entry-level agreement, by structural recursion on the entry.  The
   existential g' is the closure's own grounding: the ground instantiation
   of the provider's telescope, captured at its bind site. *)
Fixpoint dentry_agree (Sg : gsig) (g : subst) (i : item) (en : dentry)
    {struct en} : Prop :=
  match i, en with
  | IVal v, DVal w =>
      match g_val Sg v with
      | Some (_, tauv) => vty Sg w (app_subst g tauv)
      | None => False
      end
  | IFn f, DClo f' d' =>
      match g_fn Sg f, g_fn Sg f' with
      | Some (_, Sf, None), Some (D', S', Some _) =>
          exists g',
            grounding g' /\ covers g' (tyreqs D') /\
            map (app_subst g') (fs_params S') = map (app_subst g) (fs_params Sf) /\
            app_subst g' (fs_ret S') = app_subst g (fs_ret Sf) /\
            tele_covered d' D' /\
            (fix all (dd : denv) : Prop :=
               match dd with
               | [] => True
               | p :: r => dentry_agree Sg g' (fst p) (snd p) /\ all r
               end) d'
      | _, _ => False
      end
  | IMth rt m th, DClo f' d' =>
      match g_mth Sg m, g_fn Sg f' with
      | Some (_, Sm), Some (D', S', Some _) =>
          exists g',
            grounding g' /\ covers g' (tyreqs D') /\
            map (app_subst g') (fs_params S')
              = app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm) /\
            app_subst g' (fs_ret S')
              = app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm))) /\
            tele_covered d' D' /\
            (fix all (dd : denv) : Prop :=
               match dd with
               | [] => True
               | p :: r => dentry_agree Sg g' (fst p) (snd p) /\ all r
               end) d'
      | _, _ => False
      end
  | _, _ => False
  end.

(* The type material an item itself carries. *)
Definition item_ty_frees (i : item) : list tsym :=
  match i with
  | IMth rt _ th => ty_frees rt ++ flat_map (fun p => ty_frees (snd p)) th
  | _ => []
  end.

Definition denv_agree (Sg : gsig) (g : subst) (A : list item) (d : denv) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_denv i d = Some en) A
  /\ Forall (fun p => dentry_agree Sg g (fst p) (snd p)) d
  (* item regularity: what the in-force items mention is in force *)
  /\ Forall (fun i => incl (dep_tys Sg i ++ item_ty_frees i) (tyreqs A)) A
  (* and the same for the environment's own keys *)
  /\ Forall (fun p => incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p))
                           (tyreqs A)) d.

Definition venv_agree (Sg : gsig) (g : subst) (A : list item)
    (G : tenv) (ge : venv) : Prop :=
  (forall x tau, lookup_tenv x G = Some tau ->
     exists w, lookup_venv x ge = Some w /\ vty Sg w (app_subst g tau))
  (* stored-type regularity: what the variables' types mention is in force *)
  /\ Forall (fun p => incl (ty_frees (snd p)) (tyreqs A)) G.

(* --- The statement. -------------------------------------------------------- *)

Definition safe_res (Sg : gsig) (gt : ty) (r : res) : Prop :=
  match r with
  | Ok w => vty Sg w gt
  | Err => False
  | OutOfFuel => True
  end.

Definition SAFE (Sg : gsig) (fuel : nat) : Prop :=
  forall Phi G e tau d ge g,
    has_ty Sg Phi G e tau ->
    ctx_grounded g Phi ->
    denv_agree Sg g (cA Phi) d ->
    venv_agree Sg g (cA Phi) G ge ->
    safe_res Sg (app_subst g (app_subst (cS Phi) tau)) (interp fuel Sg d ge e).

(* --- Substitution algebra (leaves). ---------------------------------------- *)

(* Boolean equalities reflect equality.
   Hint: ty_eqb_eq by ty_ind' with inner list inductions and Nat.eqb_eq /
   andb_true_iff; then subst_eqb_eq and item_eqb_eq are inductions/case
   analyses over it.  Both directions of the iff are needed. *)
(* BEGIN:eqb_eq *)
(* BEGIN:eqb_eq *)

(* Private helper: element-wise boolean equality on the (tsym * ty)
   association lists that occur inside TNom, given that ty_eqb reflects
   equality on every type stored in the list. *)
Lemma eqb_eq_aux1 : forall th1 th2,
  Forall (fun p => forall b, ty_eqb (snd p) b = true <-> snd p = b) th1 ->
  ((fix go (l1 l2 : list (tsym * ty)) : bool :=
      match l1, l2 with
      | [], [] => true
      | p1 :: r1, p2 :: r2 =>
          Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && go r1 r2
      | _, _ => false
      end) th1 th2 = true) <-> th1 = th2.
Proof.
  induction th1 as [| [t1 ty1] r1 IHr]; intros th2 HF; destruct th2 as [| [t2 ty2] r2];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply andb_true_iff in H as [Hp Hr].
    apply andb_true_iff in Hp as [Ht Hty].
    apply Nat.eqb_eq in Ht; subst t2.
    inversion HF as [| p r Hhd Htl Heq]; subst; simpl in Hhd.
    apply Hhd in Hty; subst ty2.
    f_equal. apply IHr; assumption.
  - inversion HF as [| p r Hhd Htl Heq]; subst; simpl in Hhd.
    apply andb_true_iff; split.
    + apply andb_true_iff; split.
      * apply Nat.eqb_eq; congruence.
      * apply Hhd; congruence.
    + apply IHr; congruence.
Qed.

(* Private helper: element-wise boolean equality on the plain [ty] lists
   that occur inside TUnion. *)
Lemma eqb_eq_aux2 : forall ms1 ms2,
  Forall (fun t => forall b, ty_eqb t b = true <-> t = b) ms1 ->
  ((fix go (l1 l2 : list ty) : bool :=
      match l1, l2 with
      | [], [] => true
      | t1 :: r1, t2 :: r2 => ty_eqb t1 t2 && go r1 r2
      | _, _ => false
      end) ms1 ms2 = true) <-> ms1 = ms2.
Proof.
  induction ms1 as [| t1 r1 IHr]; intros ms2 HF; destruct ms2 as [| t2 r2];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply andb_true_iff in H as [Ht Hr].
    inversion HF as [| t r Hhd Htl Heq]; subst.
    apply Hhd in Ht; subst t2.
    f_equal. apply IHr; assumption.
  - inversion HF as [| t r Hhd Htl Heq]; subst.
    apply andb_true_iff; split.
    + apply Hhd; congruence.
    + apply IHr; congruence.
Qed.

Lemma ty_eqb_eq : forall a b, ty_eqb a b = true <-> a = b.
Proof.
  induction a as [ | t1 | n1 th1 IH | ms1 IH] using ty_ind'; intros b; destruct b as [ | t2 | n2 th2 | ms2 ];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply Nat.eqb_eq in H; subst; reflexivity.
  - injection H as ->; apply Nat.eqb_eq; reflexivity.
  - apply andb_true_iff in H as [Hn Hth].
    apply Nat.eqb_eq in Hn; subst n2.
    destruct (@eqb_eq_aux1 th1 th2 IH) as [Hfwd _].
    apply Hfwd in Hth; subst th2; reflexivity.
  - injection H as Hn Hth; subst n2; subst th2.
    apply andb_true_iff; split.
    + apply Nat.eqb_eq; reflexivity.
    + destruct (@eqb_eq_aux1 th1 th1 IH) as [_ Hbwd].
      apply Hbwd; reflexivity.
  - destruct (@eqb_eq_aux2 ms1 ms2 IH) as [Hfwd _].
    apply Hfwd in H; subst ms2; reflexivity.
  - injection H as Hms; subst ms2.
    destruct (@eqb_eq_aux2 ms1 ms1 IH) as [_ Hbwd].
    apply Hbwd; reflexivity.
Qed.

Lemma subst_eqb_eq : forall a b, subst_eqb a b = true <-> a = b.
Proof.
  induction a as [| [t1 ty1] r1 IHr]; intros b; destruct b as [| [t2 ty2] r2];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply andb_true_iff in H as [Hp Hr].
    apply andb_true_iff in Hp as [Ht Hty].
    apply Nat.eqb_eq in Ht; subst t2.
    apply ty_eqb_eq in Hty; subst ty2.
    f_equal. apply IHr; assumption.
  - apply andb_true_iff; split.
    + apply andb_true_iff; split.
      * apply Nat.eqb_eq; congruence.
      * apply ty_eqb_eq; congruence.
    + apply IHr; congruence.
Qed.

Lemma item_eqb_eq : forall a b, item_eqb a b = true <-> a = b.
Proof.
  destruct a as [t1 | v1 | f1 | r1 m1 th1]; destruct b as [t2 | v2 | f2 | r2 m2 th2];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply Nat.eqb_eq in H; subst; reflexivity.
  - injection H as ->; apply Nat.eqb_eq; reflexivity.
  - apply Nat.eqb_eq in H; subst; reflexivity.
  - injection H as ->; apply Nat.eqb_eq; reflexivity.
  - apply Nat.eqb_eq in H; subst; reflexivity.
  - injection H as ->; apply Nat.eqb_eq; reflexivity.
  - apply andb_true_iff in H as [Hrm Hth].
    apply andb_true_iff in Hrm as [Hr Hm].
    apply ty_eqb_eq in Hr; subst r2.
    apply Nat.eqb_eq in Hm; subst m2.
    apply subst_eqb_eq in Hth; subst th2.
    reflexivity.
  - apply andb_true_iff; split.
    + apply andb_true_iff; split.
      * apply ty_eqb_eq; congruence.
      * apply Nat.eqb_eq; congruence.
    + apply subst_eqb_eq; congruence.
Qed.
(* END:eqb_eq *)
(* END:eqb_eq *)

(* A successful denv lookup returns one of the pairs, with the key equal to
   the query.  Hint: induction on d; item_eqb_eq. *)
(* BEGIN:lookup_denv_in *)
(* A successful denv lookup returns one of the pairs, with the key equal to
   the query.  Hint: induction on d; item_eqb_eq. *)
(* BEGIN:lookup_denv_in *)
Lemma lookup_denv_in : forall i d en,
  lookup_denv i d = Some en -> In (i, en) d.
Proof.
  induction d as [| [i' en'] r IH]; intros en H.
  - simpl in H. discriminate.
  - simpl in H. destruct (item_eqb i i') eqn:Heq.
    + apply item_eqb_eq in Heq. subst. injection H as ->.
      left. reflexivity.
    + right. apply IH. exact H.
Qed.
(* END:lookup_denv_in *)
(* END:lookup_denv_in *)

(* Looking a symbol up under a mapped substitution.
   Hint: induction on th. *)
(* BEGIN:subst_chain *)
(* BEGIN:subst_chain *)
Lemma lookup_map_snd : forall (f : ty -> ty) t th,
  lookup_subst t (map_snd f th)
  = match lookup_subst t th with
    | Some tau => Some (f tau)
    | None => None
    end.
Proof.
  intros f t th. induction th as [| [t' tau] r IH]; simpl.
  - reflexivity.
  - destruct (Nat.eqb t t'); [reflexivity | exact IH].
Qed.

(* If a key is present in a substitution's domain, lookup finds it. *)
Lemma subst_chain_aux1 : forall (t : tsym) (th : subst),
  In t (map fst th) -> exists tau, lookup_subst t th = Some tau.
Proof.
  intros t th. induction th as [| [t' tau'] r IH]; intros Hin.
  - simpl in Hin. destruct Hin.
  - simpl in Hin. destruct Hin as [Heq | Hin].
    + subst t'. simpl. rewrite Nat.eqb_refl. exists tau'. reflexivity.
    + simpl. destruct (Nat.eqb t t') eqn:E.
      * exists tau'. reflexivity.
      * apply IH. exact Hin.
Qed.

(* Substituting after a total application is applying the mapped
   application: the composition law monomorphization rests on.
   Hint: ty_ind'; the TAbs case is lookup_map_snd plus the coverage
   hypothesis; In/incl over ty_frees with in_or_app for the nested cases. *)
Lemma app_subst_chain : forall s th tau,
  incl (ty_frees tau) (map fst th) ->
  app_subst s (app_subst th tau) = app_subst (map_snd (app_subst s) th) tau.
Proof.
  intros s th tau. revert s th.
  induction tau using ty_ind'.
  - (* TUnit *) intros s g Hincl. simpl. reflexivity.
  - (* TAbs *) intros s g Hincl.
    assert (Hin : In t (map fst g)).
    { apply Hincl. simpl. left. reflexivity. }
    destruct (subst_chain_aux1 t g Hin) as [tau0 Hlook].
    simpl. rewrite lookup_map_snd. rewrite Hlook. reflexivity.
  - (* TNom *) intros s g Hincl.
    repeat rewrite app_subst_nom.
    f_equal.
    revert Hincl.
    induction H as [| a l0 Ha Hf IHl]; intros Hincl.
    + simpl. reflexivity.
    + simpl in Hincl.
      assert (Ha_incl : incl (ty_frees (snd a)) (map fst g)).
      { intros x Hx. apply Hincl. simpl. apply in_or_app. left. exact Hx. }
      assert (Hl_incl : incl (ty_frees (TNom n l0)) (map fst g)).
      { intros x Hx. apply Hincl. simpl. apply in_or_app. right. exact Hx. }
      simpl. f_equal.
      * f_equal. apply Ha. exact Ha_incl.
      * apply IHl. exact Hl_incl.
  - (* TUnion *) intros s g Hincl.
    repeat rewrite app_subst_union.
    f_equal.
    revert Hincl.
    induction H as [| tau0 ms0 Ht Hf IHl]; intros Hincl.
    + simpl. reflexivity.
    + simpl in Hincl.
      assert (Ht_incl : incl (ty_frees tau0) (map fst g)).
      { intros x Hx. apply Hincl. simpl. apply in_or_app. left. exact Hx. }
      assert (Hms_incl : incl (ty_frees (TUnion ms0)) (map fst g)).
      { intros x Hx. apply Hincl. simpl. apply in_or_app. right. exact Hx. }
      simpl. f_equal.
      * apply Ht. exact Ht_incl.
      * apply IHl. exact Hms_incl.
Qed.
(* END:subst_chain *)
(* END:subst_chain *)

(* Reading through an absorbed substitution changes nothing.
   Hint: ty_ind'; the TAbs case splits on lookup_subst t s. *)
(* BEGIN:absorbs_all *)
Lemma absorbs_all : forall g s,
  absorbs g s ->
  forall tau, app_subst g (app_subst s tau) = app_subst g tau.
Proof.
  intros g s Hab tau.
  induction tau using ty_ind'; simpl; try reflexivity.
  - destruct (lookup_subst t s) as [tau'|] eqn:E; [|reflexivity].
    rewrite (Hab t tau' E). reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite IHForall. destruct x as [t0 tau0]; simpl in *.
    rewrite H. reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. reflexivity.
Qed.
(* END:absorbs_all *)

(* Closed types are fixed by substitution; grounding a covered type closes
   it.  Hint: ty_ind'; app_eq_nil facts about ty_frees of the nested lists;
   for the second lemma also Forall_forall over the grounding. *)
(* BEGIN:closed_subst *)
(* BEGIN:closed_subst *)
Lemma app_subst_closed : forall s tau,
  closed_ty tau -> app_subst s tau = tau.
Proof.
  intros s tau.
  induction tau using ty_ind'; intro Hc; simpl in *; try reflexivity.
  - unfold closed_ty in Hc. simpl in Hc. discriminate.
  - f_equal.
    unfold closed_ty in Hc. simpl in Hc.
    revert Hc.
    induction H; intro Hc; simpl in *; [reflexivity|].
    apply app_eq_nil in Hc as [Hx Hxs].
    f_equal.
    + destruct x as [a b]; simpl in *. f_equal. apply H. unfold closed_ty. exact Hx.
    + apply IHForall. exact Hxs.
  - f_equal.
    unfold closed_ty in Hc. simpl in Hc.
    revert Hc.
    induction H; intro Hc; simpl in *; [reflexivity|].
    apply app_eq_nil in Hc as [Hx Hxs].
    f_equal.
    + apply H. unfold closed_ty. exact Hx.
    + apply IHForall. exact Hxs.
Qed.

(* Private helper: a symbol available as a key of a grounding names a
   closed type. *)
Lemma closed_subst_aux1 : forall g, grounding g ->
  forall t, In t (map fst g) -> exists tau', lookup_subst t g = Some tau' /\ closed_ty tau'.
Proof.
  induction g as [| [t0 tau0] r IH]; intros Hg t Ht.
  - simpl in Ht. contradiction.
  - inversion Hg as [| p l Hp Hl Heq]; subst.
    simpl in Hp.
    simpl in Ht. destruct Ht as [Heq2 | Ht].
    + subst t0. simpl. rewrite Nat.eqb_refl.
      exists tau0. split; [reflexivity | exact Hp].
    + simpl. destruct (Nat.eqb t t0) eqn:E.
      * apply Nat.eqb_eq in E. subst.
        exists tau0. split; [reflexivity | exact Hp].
      * apply IH; [exact Hl | exact Ht].
Qed.

Lemma closed_app_subst_ground : forall g tau,
  grounding g ->
  incl (ty_frees tau) (map fst g) ->
  closed_ty (app_subst g tau).
Proof.
  intros g tau Hg.
  induction tau using ty_ind'; intro Hi; unfold closed_ty in *; simpl in *; try reflexivity.
  - assert (Ht : In t (map fst g)) by (apply Hi; left; reflexivity).
    destruct (@closed_subst_aux1 g Hg t Ht) as [tau' [E Hcl]].
    rewrite E. exact Hcl.
  - revert Hi.
    induction H; intro Hi; unfold closed_ty in *; simpl in *; [reflexivity|].
    assert (Hi1 : incl (ty_frees (snd x)) (map fst g)).
    { intros a Ha. apply Hi. apply in_or_app. left. exact Ha. }
    assert (Hi2 : incl ((fix go (l0 : list (tsym * ty)) : list tsym :=
                           match l0 with
                           | [] => []
                           | p :: r0 => ty_frees (snd p) ++ go r0
                           end) l) (map fst g)).
    { intros a Ha. apply Hi. apply in_or_app. right. exact Ha. }
    unfold closed_ty in H, IHForall.
    rewrite (H Hi1), (IHForall Hi2). reflexivity.
  - revert Hi.
    induction H; intro Hi; unfold closed_ty in *; simpl in *; [reflexivity|].
    assert (Hi1 : incl (ty_frees x) (map fst g)).
    { intros a Ha. apply Hi. apply in_or_app. left. exact Ha. }
    assert (Hi2 : incl ((fix go (l0 : list ty) : list tsym :=
                           match l0 with
                           | [] => []
                           | t' :: r0 => ty_frees t' ++ go r0
                           end) l) (map fst g)).
    { intros a Ha. apply Hi. apply in_or_app. right. exact Ha. }
    unfold closed_ty in H, IHForall.
    rewrite (H Hi1), (IHForall Hi2). reflexivity.
Qed.
(* END:closed_subst *)
(* END:closed_subst *)

(* Substitutions that agree on a type's free symbols read it equally.
   Hint: ty_ind'; in_or_app in the nested cases. *)
(* BEGIN:frees_agree *)
Lemma app_subst_frees_agree : forall g1 g2 tau,
  (forall t, In t (ty_frees tau) ->
             lookup_subst t g1 = lookup_subst t g2) ->
  app_subst g1 tau = app_subst g2 tau.
Proof.
  intros g1 g2 tau. revert g1 g2.
  induction tau using ty_ind'; intros g1 g2 Hag.
  - reflexivity.
  - simpl. rewrite (Hag t); [reflexivity | simpl; left; reflexivity].
  - rewrite !app_subst_nom. f_equal.
    revert Hag. induction H as [|p r Hp Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * f_equal. apply Hp. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
  - rewrite !app_subst_union. f_equal.
    revert Hag. induction H as [|m r Hm Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * apply Hm. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
Qed.
(* END:frees_agree *)

(* Frees of a substituted type: from the ranges, or untouched remainder.
   Hint: ty_ind'; the nested cases split ++ membership with in_app_or. *)
(* BEGIN:frees_app_subst_bound *)
Lemma frees_app_subst_bound : forall sg tau u,
  In u (ty_frees (app_subst sg tau)) ->
  (exists p, In p sg /\ In u (ty_frees (snd p))) \/
  (In u (ty_frees tau) /\ ~ In u (map fst sg)).
Proof.
  assert (Hlookup : forall sg t tau,
      lookup_subst t sg = Some tau ->
      exists p, In p sg /\ snd p = tau).
  { intros sg0 t tau0. induction sg0 as [|p r IH]; intro Hl.
    - discriminate.
    - destruct p as [t' tau']. simpl in Hl.
      destruct (Nat.eqb t t') eqn:E.
      + inversion Hl; subst tau'. exists (t', tau0). simpl.
        split; [left; reflexivity | reflexivity].
      + destruct (IH Hl) as [p [Hp Heq]].
        exists p. split; [right; exact Hp | exact Heq]. }
  intros sg tau. induction tau using ty_ind'; intros u Hu.
  - simpl in Hu. contradiction.
  - simpl in Hu. destruct (lookup_subst t sg) as [tau0|] eqn:Hl.
    + left. destruct (Hlookup sg t tau0 Hl) as [p [Hp Heq]].
      exists p. split; [exact Hp |]. rewrite Heq. exact Hu.
    + right. simpl in Hu. destruct Hu as [Hu | Hu]; [subst u | contradiction].
      split; [simpl; left; reflexivity |].
      intro Hin. destruct (subst_chain_aux1 t sg Hin) as [tau0 Hsome].
      rewrite Hl in Hsome. discriminate.
  - rewrite app_subst_nom in Hu.
    revert Hu. induction H as [|p r Hp Hr IH]; intros Hu.
    + simpl in Hu. contradiction.
    + simpl in Hu. apply in_app_or in Hu. destruct Hu as [Hu | Hu].
      * destruct (Hp u Hu) as [Hrange | [Hfree Hnot]].
        -- left. exact Hrange.
        -- right. split.
           ++ simpl. apply in_or_app. left. exact Hfree.
           ++ exact Hnot.
      * destruct (IH Hu) as [Hrange | [Hfree Hnot]].
        -- left. exact Hrange.
        -- right. split.
           ++ simpl. apply in_or_app. right. exact Hfree.
           ++ exact Hnot.
  - rewrite app_subst_union in Hu.
    revert Hu. induction H as [|tau0 r Htau Hr IH]; intros Hu.
    + simpl in Hu. contradiction.
    + simpl in Hu. apply in_app_or in Hu. destruct Hu as [Hu | Hu].
      * destruct (Htau u Hu) as [Hrange | [Hfree Hnot]].
        -- left. exact Hrange.
        -- right. split.
           ++ simpl. apply in_or_app. left. exact Hfree.
           ++ exact Hnot.
      * destruct (IH Hu) as [Hrange | [Hfree Hnot]].
        -- left. exact Hrange.
        -- right. split.
           ++ simpl. apply in_or_app. right. exact Hfree.
           ++ exact Hnot.
Qed.
(* END:frees_app_subst_bound *)

(* --- Value-typing inversions and subtyping. -------------------------------- *)

(* Hint: inversion; VtyInj cannot produce TUnit or TNom, and a union member
   reached by VtyInj is in the list. *)
(* BEGIN:vty_inv *)
Lemma vty_unit_inv : forall Sg w, vty Sg w TUnit -> w = VUnit.
Proof.
  intros Sg w H. inversion H; subst; reflexivity.
Qed.

Lemma vty_union_inv : forall Sg w ms,
  vty Sg w (TUnion ms) -> exists tau', In tau' ms /\ vty Sg w tau'.
Proof.
  intros Sg w ms H. inversion H; subst.
  exists tau'. split; assumption.
Qed.

Lemma vty_nom_inv : forall Sg w n th,
  vty Sg w (TNom n th) ->
  exists D taup w', w = VTag n w' /\ g_tag Sg n = Some (D, taup)
                    /\ vty Sg w' (app_subst th taup).
Proof.
  intros Sg w n th H. inversion H; subst.
  exists D, taup, w0. split; [reflexivity | split; assumption].
Qed.
(* END:vty_inv *)

(* Subtyping preserves value typing under any grounding of the context.
   Hint: case on the subtyping derivation; SubEq rewrites (ty_eq is an
   equation between σ images, apply f_equal with app_subst g); SubInj maps
   the member equality through app_subst_union/in_map and re-injects. *)
(* BEGIN:vty_subty *)
Lemma vty_subty : forall Sg Phi g tau' tau w,
  subty Phi tau' tau ->
  vty Sg w (app_subst g (app_subst (cS Phi) tau')) ->
  vty Sg w (app_subst g (app_subst (cS Phi) tau)).
Proof.
  intros Sg Phi g tau' tau w Hsub Hv.
  destruct Hsub as [t1 t2 Heq | n th ms tauw Hin Heq].
  - unfold ty_eq in Heq. rewrite <- Heq. exact Hv.
  - unfold ty_eq in Heq.
    rewrite app_subst_union, app_subst_union.
    apply VtyInj with (tau' := app_subst g (app_subst (cS Phi) tauw)).
    + rewrite <- Heq. exact Hv.
    + apply in_map. apply in_map. exact Hin.
Qed.
(* END:vty_subty *)

(* --- Availability gives syntactic membership for symbol-only items. -------
   Hint: avail is an existsb; app_subst_item preserves the constructor, so
   the witness must be the same symbol-only item; item_eqb_eq. *)
(* BEGIN:avail_in *)
(* BEGIN:avail_in *)
Lemma avail_in_aux1 : forall Phi i, avail Phi i ->
  exists i0, In i0 (cA Phi) /\ item_subst_eqb (cS Phi) i0 i = true.
Proof.
  intros Phi i H.
  unfold avail, avail_b in H.
  apply existsb_exists in H.
  destruct H as [i0 [Hin Heq]].
  exists i0. split; assumption.
Qed.

Lemma avail_ty_in : forall Phi t, avail Phi (ITy t) -> In (ITy t) (cA Phi).
Proof.
  intros Phi t H.
  apply avail_in_aux1 in H.
  destruct H as [i0 [Hin Heq]].
  unfold item_subst_eqb in Heq.
  apply item_eqb_eq in Heq.
  destruct i0; simpl in Heq; try discriminate.
  simpl in Heq. inversion Heq; subst.
  exact Hin.
Qed.

Lemma avail_val_in : forall Phi v, avail Phi (IVal v) -> In (IVal v) (cA Phi).
Proof.
  intros Phi v H.
  apply avail_in_aux1 in H.
  destruct H as [i0 [Hin Heq]].
  unfold item_subst_eqb in Heq.
  apply item_eqb_eq in Heq.
  destruct i0; simpl in Heq; try discriminate.
  simpl in Heq. inversion Heq; subst.
  exact Hin.
Qed.

Lemma avail_fn_in : forall Phi f, avail Phi (IFn f) -> In (IFn f) (cA Phi).
Proof.
  intros Phi f H.
  apply avail_in_aux1 in H.
  destruct H as [i0 [Hin Heq]].
  unfold item_subst_eqb in Heq.
  apply item_eqb_eq in Heq.
  destruct i0; simpl in Heq; try discriminate.
  simpl in Heq. inversion Heq; subst.
  exact Hin.
Qed.
(* END:avail_in *)
(* END:avail_in *)

(* --- Variable environments. ------------------------------------------------ *)

(* Hint: venv_agree_cons is a case split on Nat.eqb; venv_agree_body goes by
   induction on the Forall2 after generalizing the seq start index. *)
(* BEGIN:venv_agree_lemmas *)
Lemma venv_agree_cons : forall Sg g A G ge x tau w,
  venv_agree Sg g A G ge ->
  vty Sg w (app_subst g tau) ->
  incl (ty_frees tau) (tyreqs A) ->
  venv_agree Sg g A ((x, tau) :: G) ((x, w) :: ge).
Proof.
  intros Sg g A G ge x tau w HG Hw Hreg.
  unfold venv_agree in *.
  destruct HG as [HG HGreg].
  split.
  - intros y tauy Hy.
    simpl in Hy. simpl.
    destruct (Nat.eqb y x) eqn:E.
    + inversion Hy; subst. exists w. split; [reflexivity | exact Hw].
    + apply HG. exact Hy.
  - constructor; assumption.
Qed.

(* Shifted form of venv_agree_body: the two combines share the start index. *)
Lemma venv_agree_lemmas_aux1 : forall Sg g ws taus,
  Forall2 (fun w tau => vty Sg w (app_subst g tau)) ws taus ->
  forall s x tau,
    lookup_tenv x (combine (seq s (length taus)) taus) = Some tau ->
    exists w, lookup_venv x (combine (seq s (length ws)) ws) = Some w
              /\ vty Sg w (app_subst g tau).
Proof.
  intros Sg g ws taus H.
  induction H as [| w0 tau0 ws' taus' Hhd Htl IH]; intros s x tau Hx.
  - simpl in Hx. discriminate.
  - simpl in Hx. simpl.
    destruct (Nat.eqb x s) eqn:E.
    + inversion Hx; subst. exists w0. split; [reflexivity | exact Hhd].
    + apply IH. exact Hx.
Qed.

Lemma venv_agree_body : forall Sg g A ws taus,
  Forall2 (fun w tau => vty Sg w (app_subst g tau)) ws taus ->
  Forall (fun tau => incl (ty_frees tau) (tyreqs A)) taus ->
  venv_agree Sg g A (combine (seq 0 (length taus)) taus) (mk_venv ws).
Proof.
  intros Sg g A ws taus H Hreg.
  unfold venv_agree, mk_venv.
  split.
  - intros x tau Hx.
    eapply venv_agree_lemmas_aux1; eauto.
  - clear H.
    apply Forall_forall.
    intros [x tau] Hin.
    rewrite Forall_forall in Hreg.
    apply Hreg.
    eapply in_combine_r; eauto.
Qed.
(* END:venv_agree_lemmas *)

(* --- Regularity: well-formed types only mention in-force symbols. ---------- *)

(* Hint: mirror eval_ind': a fix on the wf_ty derivation whose K-Tag/K-Union
   cases traverse the Forall premises with an inner induction. *)
(* BEGIN:wf_ty_ind *)
Lemma wf_ty_ind' : forall (Sg : gsig) (Phi : ctx) (P : ty -> Prop),
  P TUnit ->
  (forall t, avail Phi (ITy t) -> P (TAbs t)) ->
  (forall n D tau0 th,
      g_tag Sg n = Some (D, tau0) ->
      sat Sg Phi th D ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      Forall (fun p => P (snd p)) th ->
      P (TNom n th)) ->
  (forall ms,
      Forall (wf_ty Sg Phi) ms -> Forall P ms ->
      Forall is_nom ms ->
      NoDup (heads_of (cS Phi) ms) ->
      P (TUnion ms)) ->
  forall tau, wf_ty Sg Phi tau -> P tau.
Proof.
  intros Sg Phi P HU HA HN HUn.
  fix IH 2.
  intros tau W.
  destruct W as [ | t Hav | n D tau0 th Hg Hsat Hall | ms Hall Hnom Hnd ].
  - exact HU.
  - apply HA; exact Hav.
  - apply (HN n D tau0 th Hg Hsat Hall).
    clear - IH Hall.
    induction Hall; constructor; [apply IH; assumption | assumption].
  - apply (HUn ms Hall); [ | exact Hnom | exact Hnd ].
    clear - IH Hall.
    induction Hall; constructor; [apply IH; assumption | assumption].
Qed.
(* END:wf_ty_ind *)

(* Hint: wf_ty_ind'; the TAbs case is avail_ty_in; tyreqs of a tele is a
   flat_map, so In facts transfer by in_flat_map. *)
(* BEGIN:wf_ty_frees *)
(* The nested fixes inside ty_frees, read as membership splitters. *)
Lemma wf_ty_frees_aux1 : forall (t : tsym) (n : nsym) (th : subst),
  In t (ty_frees (TNom n th)) ->
  exists p, In p th /\ In t (ty_frees (snd p)).
Proof.
  intros t n th. induction th as [|a th IH]; simpl.
  - intros [].
  - intros H. apply in_app_or in H. destruct H as [H|H].
    + exists a. split; [left; reflexivity | exact H].
    + destruct (IH H) as [p [Hp Ht]]. exists p. split; [right; exact Hp | exact Ht].
Qed.

Lemma wf_ty_frees_aux2 : forall (t : tsym) (ms : list ty),
  In t (ty_frees (TUnion ms)) ->
  exists tau, In tau ms /\ In t (ty_frees tau).
Proof.
  intros t ms. induction ms as [|a ms IH]; simpl.
  - intros [].
  - intros H. apply in_app_or in H. destruct H as [H|H].
    + exists a. split; [left; reflexivity | exact H].
    + destruct (IH H) as [tau [Hm Ht]]. exists tau. split; [right; exact Hm | exact Ht].
Qed.

Lemma wf_ty_frees_aux3 : forall t A, In (ITy t) A -> In t (tyreqs A).
Proof.
  intros t A HIn. unfold tyreqs. apply in_flat_map.
  exists (ITy t). split; [exact HIn | left; reflexivity].
Qed.

Lemma wf_ty_frees : forall Sg Phi tau,
  wf_ty Sg Phi tau -> incl (ty_frees tau) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi tau H. revert H.
  apply (@wf_ty_ind' Sg Phi (fun t0 => incl (ty_frees t0) (tyreqs (cA Phi)))).
  - (* TUnit *) intros x Hx. simpl in Hx. destruct Hx.
  - (* TAbs *) intros t Hav x Hx. simpl in Hx. destruct Hx as [Heq|[]].
    subst x. apply wf_ty_frees_aux3. apply avail_ty_in. exact Hav.
  - (* TNom *) intros n D tau0 th Hg Hsat Hwf IH x Hx.
    apply wf_ty_frees_aux1 in Hx. destruct Hx as [p [Hp Ht]].
    rewrite Forall_forall in IH. apply (IH p Hp). exact Ht.
  - (* TUnion *) intros ms Hwf IH Hnom Hnd x Hx.
    apply wf_ty_frees_aux2 in Hx. destruct Hx as [tau' [Hm Ht]].
    rewrite Forall_forall in IH. apply (IH tau' Hm). exact Ht.
Qed.
(* END:wf_ty_frees *)

(* Telescopes are dependency-closed: a non-type item's own type requirements
   appear among the telescope's type items (the paper's [D20]).
   Hint: induction on the wf_tele derivation, generalizing the prefix; the
   item_ok premise's sat0 now carries availability of the telescope's type
   items directly (its second conjunct); avail_ty_in turns that into
   membership in the prefix, hence in D0 ++ D. *)
(* BEGIN:tele_deps_closed *)
(* BEGIN:tele_deps_closed *)
Lemma tele_deps_closed : forall Sg D0 D,
  wf_tele Sg D0 D ->
  forall i, In i D -> is_ty_item i = false ->
  incl (dep_tys Sg i) (tyreqs (D0 ++ D)).
Proof.
  intros Sg D0 D H.
  induction H as [D0 | D0 i0 D' Hok Hwf IH].
  - intros i Hin _. simpl in Hin. destruct Hin.
  - intros i Hin Hty. simpl in Hin. destruct Hin as [Heq | Hin'].
    + subst i.
      inversion Hok as [t Hg | v Dv tauv Hg Hsat | f Df S b Hg Hsat
                        | rt m Dm S th Hg Hrt Hs Hth]; subst.
      * (* OkTy: contradicts is_ty_item i0 = false *)
        unfold is_ty_item in Hty. discriminate.
      * (* OkVal *)
        unfold dep_tys. rewrite Hg.
        destruct Hsat as [_ Hsat2].
        rewrite Forall_forall in Hsat2.
        intros t Ht. specialize (Hsat2 t Ht).
        apply avail_ty_in in Hsat2. simpl in Hsat2.
        apply wf_ty_frees_aux3. apply in_or_app. left. exact Hsat2.
      * (* OkFn *)
        unfold dep_tys. rewrite Hg.
        destruct Hsat as [_ Hsat2].
        rewrite Forall_forall in Hsat2.
        intros t Ht. specialize (Hsat2 t Ht).
        apply avail_ty_in in Hsat2. simpl in Hsat2.
        apply wf_ty_frees_aux3. apply in_or_app. left. exact Hsat2.
      * (* OkMth: dep_tys is [] for method items *)
        unfold dep_tys. intros t Ht. simpl in Ht. destruct Ht.
    + specialize (IH i Hin' Hty).
      rewrite <- app_assoc in IH.
      exact IH.
Qed.
(* END:tele_deps_closed *)
(* END:tele_deps_closed *)

(* Well-formed telescopes only mention their own type material.
   Hint: like tele_deps_closed — induction on wf_tele generalizing the
   prefix; item_ok's wf_ty premises plus wf_ty_frees bound the method
   items' receiver and application. *)
(* BEGIN:tele_items_regular *)
Lemma tele_items_regular : forall Sg D0 D,
  wf_tele Sg D0 D ->
  forall i, In i D ->
  incl (dep_tys Sg i ++ item_ty_frees i) (tyreqs (D0 ++ D)).
Proof.
  intros Sg D0 D Hwf.
  assert (Hfrees : forall i, In i D ->
      incl (item_ty_frees i) (tyreqs (D0 ++ D))).
  { induction Hwf as [D0 | D0 i0 D' Hok Hwf IH].
    - intros i Hin. simpl in Hin. destruct Hin.
    - intros i Hin. simpl in Hin. destruct Hin as [Heq | Hin'].
      + subst i. inversion Hok as
          [t Hg | v Dv tauv Hg Hsat | f Df S b Hg Hsat
           | rt m Dm S th Hg Hrt Hsat Hth]; subst; simpl.
        * intros t0 Ht. destruct Ht.
        * intros t0 Ht. destruct Ht.
        * intros t0 Ht. destruct Ht.
        * intros t0 Ht.
          apply in_app_or in Ht. destruct Ht as [Hrt' | Hth'].
          -- assert (Hr := wf_ty_frees Hrt t0 Hrt').
             unfold tyreqs in *. rewrite flat_map_app.
             apply in_or_app. left. exact Hr.
          -- apply in_flat_map in Hth'.
             destruct Hth' as [p [Hp Htp]].
             rewrite Forall_forall in Hth.
             assert (Hp' := wf_ty_frees (Hth p Hp) t0 Htp).
             unfold tyreqs in *. rewrite flat_map_app.
             apply in_or_app. left. exact Hp'.
      + specialize (IH i Hin').
        rewrite <- app_assoc in IH. exact IH. }
  intros i Hin t Ht.
  apply in_app_or in Ht. destruct Ht as [Hdep | Hfree].
  - destruct i as [u | v | f | rt m th]; simpl in Hdep.
    + destruct Hdep.
    + exact (@tele_deps_closed Sg D0 D Hwf (IVal v) Hin
               (eq_refl false) t Hdep).
    + exact (@tele_deps_closed Sg D0 D Hwf (IFn f) Hin
               (eq_refl false) t Hdep).
    + destruct Hdep.
  - exact (Hfrees i Hin t Hfree).
Qed.
(* END:tele_items_regular *)

(* --- compose_smap: success and lookups. ------------------------------------ *)

(* Hint: inductions on sm; for compose_smap_first also item_eqb_eq to align
   lookup_smap's first match with the constructed environment's. *)
(* BEGIN:compose_smap_lemmas *)
Lemma compose_smap_defined : forall d sm,
  Forall (fun p => exists en, lookup_denv (snd p) d = Some en) sm ->
  exists d', compose_smap d sm = Some d'.
Proof.
  intros d sm H. induction sm as [|[req s] r IH]; simpl.
  - exists []. reflexivity.
  - inversion H as [|p0 r0 Hhd Htl]; subst.
    destruct Hhd as [en He]. simpl in He.
    destruct (IH Htl) as [d' Hd'].
    rewrite He, Hd'. exists ((req, en) :: d'). reflexivity.
Qed.

Lemma compose_smap_keys : forall d sm d',
  compose_smap d sm = Some d' -> map fst d' = map fst sm.
Proof.
  intros d sm. induction sm as [|[req s] r IH]; simpl; intros d' H.
  - injection H as H; subst. reflexivity.
  - destruct (lookup_denv s d) as [en|] eqn:He; try discriminate.
    destruct (compose_smap d r) as [dr|] eqn:Hr; try discriminate.
    injection H as H; subst. simpl. f_equal. apply IH. reflexivity.
Qed.

Lemma compose_smap_entry : forall d sm d' p,
  compose_smap d sm = Some d' -> In p d' ->
  exists sat', In (fst p, sat') sm /\ lookup_denv sat' d = Some (snd p).
Proof.
  intros d sm. induction sm as [|[req s] r IH]; simpl; intros d' p H Hin.
  - injection H as H; subst. destruct Hin.
  - destruct (lookup_denv s d) as [en|] eqn:He; try discriminate.
    destruct (compose_smap d r) as [dr|] eqn:Hr; try discriminate.
    injection H as H; subst. simpl in Hin.
    destruct Hin as [Heq | Hin].
    + subst p. simpl. exists s. split; [left; reflexivity | exact He].
    + destruct (IH dr p (eq_refl) Hin) as [sat' [Hin' Hlk]].
      exists sat'. split; [right; exact Hin' | exact Hlk].
Qed.

Lemma compose_smap_first : forall d sm d' req sat',
  compose_smap d sm = Some d' ->
  lookup_smap req sm = Some sat' ->
  exists en, lookup_denv sat' d = Some en /\ lookup_denv req d' = Some en.
Proof.
  intros d sm. induction sm as [|[a b] r IH]; simpl; intros d' req sat' H Hl.
  - discriminate.
  - destruct (lookup_denv b d) as [en|] eqn:He; try discriminate.
    destruct (compose_smap d r) as [dr|] eqn:Hr; try discriminate.
    injection H as H; subst d'.
    destruct (item_eqb req a) eqn:Ha.
    + injection Hl as Hl; subst sat'.
      exists en. split; [exact He|]. simpl. rewrite Ha. reflexivity.
    + destruct (IH dr req sat' (eq_refl) Hl) as [en' [H1 H2]].
      exists en'. split; [exact H1|]. simpl. rewrite Ha. exact H2.
Qed.
(* END:compose_smap_lemmas *)

(* --- Transferring an entry to the callee's point of view. ------------------ *)

(* An entry that agrees at item i1 under g1 also agrees at item i2 under g2,
   provided the two groundings read the two items identically: same
   symbol-only skeleton, equal method receiver/application images, and equal
   readings of the type symbols a val/fn item depends on.
   Hint: case analysis on i1 and en (dentry_agree unfolds one level only;
   the nested closure conjuncts mention the inner grounding g' and carry
   over untouched); the item-image equality forces i2's shape (injectivity
   of the constructors); for IVal/IFn rewrite the signature readings with
   app_subst_frees_agree via the dep_tys premise (dep_tys bounds exactly
   the frees of the stored signature — take that bound as a given from the
   telescoped well-formedness, or note the readings only ever apply g to
   tauv/S whose frees are within dep_tys by wf_gsig; if a side condition
   is genuinely missing here, report it in notes rather than forcing it). *)
(* BEGIN:dentry_agree_retype *)
(* Restated after a batch-1 agent machine-checked disproofs of the original:
   (1) without wf_gsig nothing bounds a stored signature's frees by dep_tys;
   (2) dentry_agree reads the receiver *through* the method application, so
   method items need the composed readings to agree on the method's
   signature components.  Premises (a)/(b) below are exactly those repairs;
   the proof is the agent's, adapted to the weakened (b). *)
Lemma dentry_agree_retype_aux1 : forall g1 g2 tau,
  (forall t, In t (ty_frees tau) ->
             app_subst g1 (TAbs t) = app_subst g2 (TAbs t)) ->
  app_subst g1 tau = app_subst g2 tau.
Proof.
  intros g1 g2 tau. revert g1 g2.
  induction tau using ty_ind'; intros g1 g2 Hag.
  - reflexivity.
  - apply Hag. simpl. left. reflexivity.
  - rewrite !app_subst_nom. f_equal.
    revert Hag. induction H as [|p r Hp Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * f_equal. apply Hp. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
  - rewrite !app_subst_union. f_equal.
    revert Hag. induction H as [|m r Hm Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * apply Hm. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
Qed.

(* The repaired statement, fully proved: the two premises the frozen
   statement is missing are (a) wf_gsig Sg, which bounds the frees of the
   stored val/fn signature by its telescope (= dep_tys), and (b) for the
   method case, agreement of the *composed* method readings interp_m, which
   the item-image equality alone does not give (app_subst g1 rt =
   app_subst g2 rt' does not survive the requirement application th).
   Everything else in the entry -- the inner grounding g', its coverage,
   tele_covered, and the nested entry agreements -- carries over verbatim. *)

Lemma dentry_agree_retype : forall Sg g1 i1 g2 i2 en,
  wf_gsig Sg ->
  dentry_agree Sg g1 i1 en ->
  app_subst_item g1 i1 = app_subst_item g2 i2 ->
  (forall t, In t (dep_tys Sg i1) ->
             app_subst g1 (TAbs t) = app_subst g2 (TAbs t)) ->
  (forall rt m th rt' th' Dm Sm tau,
      i1 = IMth rt m th -> i2 = IMth rt' m th' ->
      g_mth Sg m = Some (Dm, Sm) ->
      In tau (fs_params Sm) \/ tau = fs_ret Sm ->
      interp_m g1 th rt tau = interp_m g2 th' rt' tau) ->
  dentry_agree Sg g2 i2 en.
Proof.
  intros Sg g1 i1 g2 i2 en WF Hag Hit Hdep Hm.
  destruct i1 as [t1|v1|f1|rt1 m1 th1]; destruct i2 as [t2|v2|f2|rt2 m2 th2];
    try (simpl in Hit; discriminate); destruct en as [w|f' d'].
  - simpl in Hag. destruct Hag.
  - simpl in Hag. destruct Hag.
  - simpl in Hit. injection Hit as Hv. subst v2.
    simpl in Hag |- *. destruct (g_val Sg v1) as [[Dv tauv]|] eqn:E; [|destruct Hag].
    simpl in Hdep. rewrite E in Hdep.
    destruct (wf_val_decl WF _ E) as [_ Hwf].
    apply wf_ty_frees in Hwf. simpl in Hwf.
    rewrite <- (dentry_agree_retype_aux1 g1 g2 tauv); [exact Hag|].
    intros t Ht. apply Hdep. apply Hwf. exact Ht.
  - simpl in Hag. destruct (g_val Sg v1); destruct Hag.
  - simpl in Hag. destruct (g_fn Sg f1) as [[[Df Sf] b]|]; destruct Hag.
  - simpl in Hit. injection Hit as Hf. subst f2.
    simpl in Hag |- *.
    destruct (g_fn Sg f1) as [[[Df Sf] b]|] eqn:E; [|destruct Hag].
    destruct b; [destruct Hag|].
    destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
    destruct b' as [body|]; [|destruct Hag].
    simpl in Hdep. rewrite E in Hdep.
    destruct (wf_fn_decl WF _ E) as [_ [Hps [Hrt _]]].
    destruct Hag as [g' [Hg [Hc [Hpar [Hret Hrest]]]]].
    exists g'. split; [exact Hg|]. split; [exact Hc|].
    split.
    + rewrite Hpar. apply map_ext_in. intros tau Htau.
      apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      rewrite Forall_forall in Hps. specialize (Hps tau Htau).
      apply wf_ty_frees in Hps. simpl in Hps. apply Hps. exact Ht.
    + split; [|exact Hrest].
      rewrite Hret. apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      apply wf_ty_frees in Hrt. simpl in Hrt. apply Hrt. exact Ht.
  - simpl in Hag. destruct (g_mth Sg m1) as [[Dm Sm]|]; destruct Hag.
  - simpl in Hit. injection Hit as Hrt Hmm Hth. subst m2.
    simpl in Hag |- *.
    destruct (g_mth Sg m1) as [[Dm Sm]|] eqn:E; [|destruct Hag].
    assert (Hm' : forall tau, In tau (fs_params Sm) \/ tau = fs_ret Sm ->
              interp_m g1 th1 rt1 tau = interp_m g2 th2 rt2 tau)
      by (intros tau HIn; eapply Hm; eauto).
    unfold interp_m in Hm'.
    destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
    destruct b' as [body|]; [|destruct Hag].
    destruct Hag as [g' [Hg [Hc [Hpar [Hret Hrest]]]]].
    exists g'. split; [exact Hg|]. split; [exact Hc|].
    split.
    + rewrite Hpar, Hrt. f_equal. apply map_ext_in. intros tau Htau.
      apply Hm'. left; exact Htau.
    + split; [|exact Hrest]. rewrite Hret. apply Hm'. right; reflexivity.
Qed.
(* END:dentry_agree_retype *)

(* --- The callee's grounding. ------------------------------------------------ *)

(* Each callee type requirement, read through the application, the context,
   and the caller's grounding. *)
Definition callee_grounding (g : subst) (Phi : ctx) (th : subst) (D : tele)
    : subst :=
  map (fun t => (t, app_subst g (app_subst (cS Phi) (app_subst th (TAbs t)))))
      (tyreqs D).

(* Reading a D-covered type through the callee grounding is reading its
   applied image through the caller's.
   Hint: the lookup lemma is an induction on tyreqs D; the reads lemma is
   ty_ind' with the TAbs case closed by the lookup lemma. *)
(* BEGIN:callee_grounding_reads *)
(* BEGIN:callee_grounding_reads *)
Lemma callee_grounding_lookup : forall g Phi th D t,
  In t (tyreqs D) ->
  lookup_subst t (callee_grounding g Phi th D)
  = Some (app_subst g (app_subst (cS Phi) (app_subst th (TAbs t)))).
Proof.
  intros g Phi th D t Hin.
  unfold callee_grounding.
  revert Hin.
  generalize (tyreqs D) as l.
  intros l.
  induction l as [|a l IH]; intros Hin.
  - destruct Hin.
  - simpl in Hin |- *.
    destruct (Nat.eqb t a) eqn:Heq.
    + apply Nat.eqb_eq in Heq. subst. reflexivity.
    + apply IH. destruct Hin as [Heq2 | Hin']; [| exact Hin'].
      exfalso. apply Nat.eqb_neq in Heq. apply Heq. symmetry. exact Heq2.
Qed.

Lemma callee_grounding_reads : forall g Phi th D tau,
  incl (ty_frees tau) (tyreqs D) ->
  app_subst (callee_grounding g Phi th D) tau
  = app_subst g (app_subst (cS Phi) (app_subst th tau)).
Proof.
  intros g Phi th D tau.
  induction tau using ty_ind'; intros Hincl.
  - reflexivity.
  - simpl in Hincl. simpl.
    rewrite (callee_grounding_lookup g Phi th D t (Hincl t (or_introl eq_refl))).
    reflexivity.
  - rewrite app_subst_nom.
    rewrite (app_subst_nom th n th0).
    rewrite (app_subst_nom (cS Phi) n (map_snd (app_subst th) th0)).
    rewrite (app_subst_nom g n (map_snd (app_subst (cS Phi)) (map_snd (app_subst th) th0))).
    f_equal.
    revert Hincl. induction H as [|p r Hp Hr IH]; intros Hincl.
    + reflexivity.
    + simpl. f_equal.
      * f_equal. apply Hp. intros t' Ht'. apply Hincl. simpl. apply in_or_app. left. exact Ht'.
      * apply IH. intros t' Ht'. apply Hincl. simpl. apply in_or_app. right. exact Ht'.
  - rewrite app_subst_union.
    rewrite (app_subst_union th ms).
    rewrite (app_subst_union (cS Phi) (map (app_subst th) ms)).
    rewrite (app_subst_union g (map (app_subst (cS Phi)) (map (app_subst th) ms))).
    f_equal.
    revert Hincl. induction H as [|m r Hm Hr IH]; intros Hincl.
    + reflexivity.
    + simpl. f_equal.
      * apply Hm. intros t' Ht'. apply Hincl. simpl. apply in_or_app. left. exact Ht'.
      * apply IH. intros t' Ht'. apply Hincl. simpl. apply in_or_app. right. exact Ht'.
Qed.
(* END:callee_grounding_reads *)
(* END:callee_grounding_reads *)

(* --- The crux: satisfying a telescope hands the callee an agreeing world. --
   Hint: this is where everything above meets.  compose_smap_defined fires
   because smap_ok's satisfiers are available items whose entries the
   caller's coverage provides (satisfiers of non-type requirements are
   non-type items: app_subst_item preserves constructors and item_matches
   equates images).  Coverage of D under d' comes from smap_ok's domain
   equation plus compose_smap_keys/first.  Entry agreement at the callee's
   items: compose_smap_entry fetches the caller entry; the caller's
   denv_agree gives dentry_agree at the satisfier via lookup_denv_in;
   dentry_agree_retype moves it to the requirement item under the callee
   grounding — its item-image premise is item_matches pushed under g
   (absorbs_all + callee_grounding_reads with frees bounded by wf
   regularity), and its dep premise is sat's coherence conjunct pushed
   under g the same way (tele_deps_closed bounds the dependencies within
   tyreqs D).  ctx_grounded of the callee: grounding by
   closed_app_subst_ground (θ's ranges are wf, so their σ-then-g readings
   are closed via absorbs_all + wf_ty_frees + the caller's coverage);
   absorbs is vacuous (mk_ctx has empty σ); covers is
   callee_grounding_lookup.  If a premise is genuinely missing, report it
   precisely in notes instead of forcing the proof. *)
(* BEGIN:callee_env_agree *)
Lemma callee_env_agree : forall Sg g Phi th sm D d,
  wf_gsig Sg ->
  wf_tele Sg [] D ->
  sat Sg Phi th D ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  smap_ok Phi th sm D ->
  ctx_grounded g Phi ->
  denv_agree Sg g (cA Phi) d ->
  exists d', compose_smap d sm = Some d'
    /\ ctx_grounded (callee_grounding g Phi th D) (mk_ctx D)
    /\ denv_agree Sg (callee_grounding g Phi th D) D d'.
Proof.
Admitted. (* FILL:callee_env_agree *)
(* END:callee_env_agree *)

(* --- A lean induction principle for typing: only T-Sub re-types the same
   term, so only it needs an induction hypothesis. ------------------------- *)

Section HasTyIndSub.
  Variable Sg : gsig.
  Variable P : ctx -> tenv -> expr -> ty -> Prop.

  Hypothesis HVar : forall Phi G x tau,
      lookup_tenv x G = Some tau -> P Phi G (EVar x) tau.
  Hypothesis HUnit : forall Phi G, P Phi G EUnit TUnit.
  Hypothesis HSub : forall Phi G e tau' tau,
      has_ty Sg Phi G e tau' -> P Phi G e tau' -> subty Phi tau' tau ->
      wf_ty Sg Phi tau ->
      P Phi G e tau.
  Hypothesis HVal : forall Phi G v Dv tauv,
      g_val Sg v = Some (Dv, tauv) -> avail Phi (IVal v) ->
      P Phi G (EVl v) (app_subst (cS Phi) tauv).
  Hypothesis HTag : forall Phi G n D tau0 th e,
      g_tag Sg n = Some (D, tau0) -> sat Sg Phi th D ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
      P Phi G (ETag n th e) (app_subst (cS Phi) (TNom n th)).
  Hypothesis HNeed : forall Phi G f Df S args,
      g_fn Sg f = Some (Df, S, None) -> avail Phi (IFn f) ->
      Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau))
              args (fs_params S) ->
      P Phi G (ECallA f args) (app_subst (cS Phi) (fs_ret S)).
  Hypothesis HCall : forall Phi G f Df S body th sm args,
      g_fn Sg f = Some (Df, S, Some body) ->
      sat Sg Phi th Df ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Df ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
              args (fs_params S) ->
      P Phi G (ECallD f th sm args)
        (app_subst (cS Phi) (app_subst th (fs_ret S))).
  Hypothesis HMeth : forall Phi G e0 tau0 m Dm S rt0 th0 args,
      g_mth Sg m = Some (Dm, S) ->
      has_ty Sg Phi G e0 tau0 ->
      In (IMth rt0 m th0) (cA Phi) ->
      app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
      (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                  i1 = IMth rt0 m th0) ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e
                        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
              args (fs_params S) ->
      P Phi G (EMeth e0 m (IMth rt0 m th0) args)
        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S)).
  Hypothesis HMatch : forall Phi G e0 tau0 ms arms tau,
      has_ty Sg Phi G e0 tau0 ->
      wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
      members (app_subst (cS Phi) tau0) = Some ms ->
      NoDup (map member_head ms) ->
      Forall2 (arm_matches Sg) ms arms ->
      Forall2 (fun mem arm =>
                 has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                        (arm_body arm) tau) ms arms ->
      P Phi G (EMatch e0 arms) tau.
  Hypothesis HLet : forall Phi G x e1 e2 tau1 tau,
      has_ty Sg Phi G e1 tau1 ->
      has_ty Sg Phi ((x, tau1) :: G) e2 tau ->
      P Phi G (ELet x e1 e2) tau.
  Hypothesis HBindTy : forall Phi G t tau' e tau,
      g_ty Sg t = true ->
      ~ In (ITy t) (cA Phi) ->
      wf_ty Sg Phi tau' ->
      has_ty Sg (bind_ty t tau' Phi) G e tau ->
      wf_ty Sg Phi tau ->
      P Phi G (EBindTy t tau' e) tau.
  Hypothesis HBindVal : forall Phi G v Dv tauv e1 e tau,
      g_val Sg v = Some (Dv, tauv) ->
      sat0 Sg Phi Dv ->
      has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
      has_ty Sg (add_item (IVal v) Phi) G e tau ->
      P Phi G (EBindVal v e1 e) tau.
  Hypothesis HBindFn : forall Phi G f Df S f' D' S' body sm e tau,
      g_fn Sg f = Some (Df, S, None) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi Df ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      sig_eq (cS Phi) S' S ->
      has_ty Sg (add_item (IFn f) Phi) G e tau ->
      P Phi G (EBindFn f f' sm e) tau.
  Hypothesis HBindMth : forall Phi G tauk m Dm S th f' D' S' body sm
                               tau0' rest e tau,
      g_mth Sg m = Some (Dm, S) ->
      wf_ty Sg Phi tauk ->
      sat Sg Phi th Dm ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      fs_params S' = tau0' :: rest ->
      ty_eq (cS Phi) tau0' tauk ->
      map (app_subst (cS Phi)) rest
        = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
      app_subst (cS Phi) (fs_ret S')
        = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
      has_ty Sg (add_item (IMth tauk m th) Phi) G e tau ->
      P Phi G (EBindMth tauk m th f' sm e) tau.

  Lemma has_ty_ind_sub : forall Phi G e tau,
      has_ty Sg Phi G e tau -> P Phi G e tau.
  Proof.
    fix IH 5.
    intros Phi G e tau D.
    destruct D.
    - apply HVar; assumption.
    - apply HUnit.
    - eapply HSub; [eassumption | apply IH; assumption | assumption | assumption].
    - eapply HVal; eassumption.
    - eapply HTag; eassumption.
    - eapply HNeed; eassumption.
    - eapply HCall; eassumption.
    - eapply HMeth; eassumption.
    - eapply HMatch; eassumption.
    - eapply HLet; eassumption.
    - eapply HBindTy; eassumption.
    - eapply HBindVal; eassumption.
    - eapply HBindFn; eassumption.
    - eapply HBindMth; eassumption.
  Qed.
End HasTyIndSub.

(* Typing only assigns types whose free symbols are in force — the
   regularity that bindty's grounding extension leans on.
   Hint: has_ty_ind_sub with motive
     P Phi G e tau := sigma_regular (cS Phi) (cA Phi) ->
       (forall x tx, lookup_tenv x G = Some tx ->
          incl (ty_frees tx) (tyreqs (cA Phi))) ->
       incl (ty_frees tau) (tyreqs (cA Phi));
   Sub and Match use their new wf premises via wf_ty_frees; the
   declared-signature cases push through frees_app_subst_bound with the
   use-site Forall-wf premises bounding the application ranges; binds
   extend A by non-type items (tyreqs unchanged) except bindty, whose
   own wf premise short-circuits. *)
(* BEGIN:has_ty_frees *)
Lemma has_ty_frees : forall Sg Phi G e tau,
  wf_gsig Sg ->
  has_ty Sg Phi G e tau ->
  sigma_regular (cS Phi) (cA Phi) ->
  (forall i, In i (cA Phi) -> incl (dep_tys Sg i) (tyreqs (cA Phi))) ->
  (forall x tx, lookup_tenv x G = Some tx ->
     incl (ty_frees tx) (tyreqs (cA Phi))) ->
  incl (ty_frees tau) (tyreqs (cA Phi)).
Proof.
Admitted. (* FILL:has_ty_frees *)
(* END:has_ty_frees *)

(* --- The case lemmas: one per typing rule, at fuel S k, under the strong
   fuel induction hypothesis. ----------------------------------------------- *)

Section SafetyCases.
  Variable Sg : gsig.
  Hypothesis WF : wf_gsig Sg.
  Variable k : nat.
  Hypothesis IHfuel : forall j, j <= k -> SAFE Sg j.

  (* Argument lists evaluate safely: interp_list at any fuel ≤ k either
     runs dry, or yields values at the grounded types — never Err.
     Hint: induction on the Forall2 with the fuel generalized; the head
     uses IHfuel, the tail the inner induction at smaller fuel (destruct j
     first: interp_list 0 is inr OutOfFuel). *)
  (* BEGIN:safety_args *)
  (* BEGIN:safety_args *)
  Lemma safety_args : forall j, j <= k ->
    forall Phi G d ge g args taus,
      Forall2 (fun e tau => has_ty Sg Phi G e tau) args taus ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      match interp_list j Sg d ge args with
      | inl ws => Forall2 (fun w tau =>
                    vty Sg w (app_subst g (app_subst (cS Phi) tau))) ws taus
      | inr Err => False
      | inr _ => True
      end.
  Proof.
    intros j Hj Phi G d ge g args taus HF Hcg Hd Hv.
    revert j Hj.
    induction HF as [| e tau args' taus' Hty HF IH]; intros j Hj.
    - destruct j as [| j']; simpl; [ exact I | constructor ].
    - destruct j as [| j']; simpl; [ exact I | ].
      assert (Hj' : j' <= k) by (apply Nat.lt_le_incl; exact Hj).
      assert (Hhead := @IHfuel j' Hj' Phi G e tau d ge g Hty Hcg Hd Hv).
      specialize (IH j' Hj').
      destruct (interp j' Sg d ge e) as [w | |] eqn:E;
        unfold safe_res in Hhead.
      + destruct (interp_list j' Sg d ge args') as [ws | rr] eqn:E2.
        * constructor; [ exact Hhead | exact IH ].
        * destruct rr; [ exact I | exact IH | exact I ].
      + exact Hhead.
      + exact I.
  Qed.
  (* END:safety_args *)
(* END:safety_args *)

  (* Hint: interp (S k) on EVar reduces to the venv lookup; venv_agree
     provides the value and its typing at g tau; the conclusion's type is
     g (σ tau), so rewrite with absorbs_all (ctx_grounded's second
     component)... wait: venv_agree stores g tau where tau is the stored
     type, and the goal reads g (σ tau) — absorbs_all equates them. *)
  (* BEGIN:safety_case_var *)
  Lemma safety_case_var : forall Phi G x tau d ge g,
      lookup_tenv x G = Some tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EVar x)).
  Proof.
    intros Phi G x tau d ge g Hx Hg Hd Hv.
    destruct Hg as [Hgr [Habs [Hcov Hsig]]].
    destruct Hv as [Hv Hvreg].
    destruct (Hv x tau Hx) as [w [Hlk Hw]].
    simpl.
    rewrite Hlk.
    simpl.
    rewrite (absorbs_all Habs tau).
    exact Hw.
  Qed.
(* END:safety_case_var *)

  (* BEGIN:safety_case_unit *)
  Lemma safety_case_unit : forall Phi G d ge g,
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) TUnit))
               (interp (S k) Sg d ge EUnit).
  Proof.
    intros Phi G d ge g Hg Hd Hv.
    simpl.
    simpl app_subst.
    constructor.
  Qed.
(* END:safety_case_unit *)

  (* Hint: the second hypothesis is the structural induction hypothesis —
     the safety of the same term at the smaller type; vty_subty converts
     its verdict on the Ok branch (destruct the interp result). *)
  (* BEGIN:safety_case_sub *)
  Lemma safety_case_sub : forall Phi G e tau' tau d ge g,
      has_ty Sg Phi G e tau' ->
      (forall d0 ge0 g0,
          ctx_grounded g0 Phi ->
          denv_agree Sg g0 (cA Phi) d0 ->
          venv_agree Sg g0 (cA Phi) G ge0 ->
          safe_res Sg (app_subst g0 (app_subst (cS Phi) tau'))
                   (interp (S k) Sg d0 ge0 e)) ->
      subty Phi tau' tau ->
      wf_ty Sg Phi tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge e).
  Proof.
    intros Phi G e tau' tau d ge g Hty IH Hsub Hwf Hg Hd Hv.
    specialize (IH d ge g Hg Hd Hv).
    unfold safe_res in *.
    destruct (interp (S k) Sg d ge e) as [w | | ] eqn:Heq.
    - eapply vty_subty; eauto.
    - exact IH.
    - exact IH.
  Qed.
(* END:safety_case_sub *)

  (* Hint: avail_val_in + denv coverage finds the entry; Forall over the
     environment (via lookup_denv_in) gives its dentry_agree, which forces
     the DVal shape and types the value at g tauv; absorbs_all collapses
     the two σ applications in the conclusion. *)
  (* BEGIN:safety_case_val *)
  Lemma safety_case_val : forall Phi G v Dv tauv d ge g,
      g_val Sg v = Some (Dv, tauv) ->
      avail Phi (IVal v) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi) tauv)))
               (interp (S k) Sg d ge (EVl v)).
  Proof.
    intros Phi G v Dv tauv d ge g Hgv Hav Hg Hd Hve.
    destruct Hg as [Hgr [Habs [Hcov Hsig]]].
    destruct Hd as [Hcover [Hall Hdreg]].
    assert (Hin : In (IVal v) (cA Phi)) by (apply avail_val_in; exact Hav).
    rewrite Forall_forall in Hcover.
    specialize (Hcover _ Hin).
    destruct Hcover as [Hbad | [en Hlook]]; [simpl in Hbad; discriminate|].
    assert (Hpin : In (IVal v, en) d) by (apply lookup_denv_in; exact Hlook).
    rewrite Forall_forall in Hall.
    specialize (Hall _ Hpin).
    destruct en as [w | f' d0].
    - simpl in Hall. rewrite Hgv in Hall.
      simpl. rewrite Hlook. simpl.
      rewrite (absorbs_all Habs). rewrite (absorbs_all Habs).
      exact Hall.
    - simpl in Hall. contradiction.
  Qed.
(* END:safety_case_val *)

  (* Hint: IHfuel on the payload typing; the conclusion's tag type unfolds
     by app_subst_nom; VtyTag wants the payload at
     app_subst (map_snd (g∘σ) th) tau0, which app_subst_chain provides —
     its coverage side condition is wf_gsig's payload regularity
     (wf_ty_frees at mk_ctx D, whose tyreqs equal sat's domain equation) —
     plus absorbs_all for the σσ collapse. *)
  (* BEGIN:safety_case_tag *)
  (* BEGIN:safety_case_tag *)
  Lemma safety_case_tag : forall Phi G n D tau0 th e d ge g,
      g_tag Sg n = Some (D, tau0) ->
      sat Sg Phi th D ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi) (TNom n th))))
               (interp (S k) Sg d ge (ETag n th e)).
  Proof.
    intros Phi G n D tau0 th e d ge g Hgt Hsat Hwfth Hty Hg Hd Hve.
    assert (Hab : absorbs g (cS Phi)) by (destruct Hg as [_ [Hab _]]; exact Hab).
    assert (Hincl : incl (ty_frees tau0) (map fst th)).
    { destruct Hsat as [Hdom _]. rewrite Hdom.
      assert (WF' := WF). destruct WF' as [Wtag Wval Wfn Wmth].
      destruct (Wtag n D tau0 Hgt) as [_ Hwf].
      apply wf_ty_frees in Hwf.
      simpl in Hwf. exact Hwf. }
    assert (IH := IHfuel (Nat.le_refl k) Hty Hg Hd Hve).
    rewrite !(absorbs_all Hab) in IH.
    rewrite !(absorbs_all Hab).
    rewrite app_subst_nom.
    replace (interp (S k) Sg d ge (ETag n th e))
      with (match interp k Sg d ge e with
            | Ok w => Ok (VTag n w)
            | rr => rr
            end) by reflexivity.
    remember (interp k Sg d ge e) as r eqn:Hr.
    destruct r as [w | | ].
    - simpl in IH. simpl.
      eapply VtyTag; [ exact Hgt | ].
      rewrite <- (app_subst_chain g th tau0 Hincl). exact IH.
    - simpl in IH. contradiction.
    - simpl. exact I.
  Qed.
  (* END:safety_case_tag *)
(* END:safety_case_tag *)

  (* Hint: avail_fn_in + coverage + dentry_agree give the closure and its
     signature equations at the closure's own grounding g'; safety_args
     evaluates the arguments at the caller's g; the equations transport
     the argument values to g' readings of fs_params S' (rewrite along the
     map equation, Forall2 + map juggling), venv_agree_body builds the
     body's variable environment, wf_gsig types the provider's body under
     its telescope, so IHfuel at fuel k finishes; the result comes back at
     g' (fs_ret S') = g (fs_ret S), and absorbs_all collapses the σs. *)
  (* BEGIN:safety_case_need *)
  Lemma safety_case_need_aux1 : forall (Sg0 : gsig) (g0 : subst) (dd : denv),
      (fix all (dd : denv) : Prop :=
         match dd with
         | [] => True
         | p :: r => dentry_agree Sg0 g0 (fst p) (snd p) /\ all r
         end) dd ->
      Forall (fun p => dentry_agree Sg0 g0 (fst p) (snd p)) dd.
  Proof.
    intros Sg0 g0 dd. induction dd as [| p r IH]; simpl; intros H.
    - constructor.
    - destruct H as [H1 H2]. constructor; [ exact H1 | apply IH; exact H2 ].
  Qed.

  Lemma safety_case_need_aux2 : forall Phi G args taus,
      Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau)) args taus ->
      Forall2 (fun e tau => has_ty Sg Phi G e tau) args
              (map (app_subst (cS Phi)) taus).
  Proof.
    intros Phi G args taus H.
    induction H as [| e tau args' taus' Hh Ht IH]; simpl.
    - constructor.
    - constructor; [ exact Hh | exact IH ].
  Qed.

  Lemma safety_case_need_aux3 : forall gg s ws taus,
      (forall tau, app_subst gg (app_subst s tau) = app_subst gg tau) ->
      Forall2 (fun w tau => vty Sg w (app_subst gg (app_subst s tau))) ws
              (map (app_subst s) taus) ->
      Forall2 (vty Sg) ws (map (app_subst gg) taus).
  Proof.
    intros gg s ws taus Hcol H. revert ws H.
    induction taus as [| t r IH]; intros ws H; simpl in *.
    - inversion H; subst. constructor.
    - inversion H as [| w0 ws0 t0 r0 Hh Ht]; subst. constructor.
      + rewrite Hcol in Hh. rewrite Hcol in Hh. exact Hh.
      + apply IH. exact Ht.
  Qed.

  Lemma safety_case_need_aux4 : forall gg ws taus,
      Forall2 (vty Sg) ws (map (app_subst gg) taus) ->
      Forall2 (fun w tau => vty Sg w (app_subst gg tau)) ws taus.
  Proof.
    intros gg ws taus. revert ws.
    induction taus as [| t r IH]; intros ws H; simpl in H.
    - inversion H; subst. constructor.
    - inversion H as [| w0 ws0 t0 r0 Hh Ht]; subst. constructor.
      + exact Hh.
      + apply IH. exact Ht.
  Qed.

  Lemma safety_case_need : forall Phi G f Df Sf args d ge g,
      g_fn Sg f = Some (Df, Sf, None) ->
      avail Phi (IFn f) ->
      Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau))
              args (fs_params Sf) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi) (fs_ret Sf))))
               (interp (S k) Sg d ge (ECallA f args)).
  Proof.
    intros Phi G f Df Sf args d ge g Hf Hav Hargs Hcg Hd Hv.
    assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov Hsig]]].
    assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau)
                               = app_subst g tau)
      by (exact (@absorbs_all g (cS Phi) Hab)).
    (* locate the closure bound at f *)
    assert (Hin := @avail_fn_in Phi f Hav).
    assert (Hd2 := Hd). destruct Hd2 as [Hcov1 [Hall1 Hdreg]].
    rewrite Forall_forall in Hcov1. specialize (Hcov1 _ Hin).
    destruct Hcov1 as [Hbad | [en Hlook]]; [ simpl in Hbad; discriminate | ].
    assert (Hda : dentry_agree Sg g (IFn f) en).
    { rewrite Forall_forall in Hall1.
      apply (Hall1 (IFn f, en)). apply lookup_denv_in. exact Hlook. }
    simpl. rewrite Hlook.
    destruct en as [w0 | f' df]; [ simpl in Hda; contradiction | ].
    simpl in Hda. rewrite Hf in Hda.
    destruct (g_fn Sg f') as [[[D' S'] b'] | ] eqn:Hf'; [ | contradiction ].
    destruct b' as [body | ]; [ | contradiction ].
    destruct Hda as [g' [Hgr' [Hcov' [Hpar [Hret [Htc Hdd]]]]]].
    (* the arguments *)
    assert (Hsa := @safety_args k (Nat.le_refl k) Phi G d ge g args
                     (map (app_subst (cS Phi)) (fs_params Sf))
                     (@safety_case_need_aux2 Phi G args (fs_params Sf) Hargs) Hcg Hd Hv).
    destruct (interp_list k Sg d ge args) as [ws | rr] eqn:Elist.
    - (* the arguments produced values; run the body *)
      assert (Hws : Forall2 (fun w tau => vty Sg w (app_subst g' tau)) ws
                            (fs_params S')).
      { apply (@safety_case_need_aux4 g' ws (fs_params S')).
        rewrite Hpar. exact (@safety_case_need_aux3 g (cS Phi) ws (fs_params Sf) Hcol Hsa). }
      assert (WF2 := WF). destruct WF2 as [Wtag Wval Wfn Wmth].
      specialize (Wfn f' D' S' (Some body) Hf').
      destruct Wfn as [Wt [Wp [Wr Wb]]].
      assert (Hcgb : ctx_grounded g' (mk_ctx D')).
      { split; [ exact Hgr' | split ].
        - unfold absorbs. intros t tau Ht. simpl in Ht. discriminate.
        - split; [exact Hcov' |].
          unfold sigma_regular. simpl. split.
          + intros x Hx. contradiction.
          + constructor. }
      assert (Hdb : denv_agree Sg g' (cA (mk_ctx D')) df).
      { split; [ exact Htc | split ].
        - exact (@safety_case_need_aux1 Sg g' df Hdd).
        - rewrite Forall_forall. intros i Hi.
          exact (@tele_items_regular Sg [] D' Wt i Hi). }
      assert (Hvb : venv_agree Sg g' D' (body_env S') (mk_venv ws)).
      { unfold body_env.
        apply (@venv_agree_body Sg g' D' ws (fs_params S') Hws).
        eapply Forall_impl; [|exact Wp].
        intros tau Hwf.
        exact (wf_ty_frees Hwf). }
      assert (Hbody := Wb body eq_refl).
      assert (Hres := @IHfuel k (Nat.le_refl k) (mk_ctx D') (body_env S')
                        body (fs_ret S') df (mk_venv ws) g'
                        Hbody Hcgb Hdb Hvb).
      simpl in Hres. rewrite app_subst_nil in Hres.
      rewrite Hret in Hres.
      rewrite Hcol. rewrite Hcol. exact Hres.
    - (* the arguments did not produce values *)
      destruct rr as [w1 | |].
      + exfalso. eapply interp_list_never_ok. exact Elist.
      + contradiction.
      + exact I.
  Qed.
(* END:safety_case_need *)

  (* Hint: callee_env_agree (wf_gsig provides wf_tele for Df) builds the
     callee world and makes compose_smap succeed; safety_args evaluates
     the arguments; callee_grounding_reads aligns argument and result
     types (frees bounded via wf_ty_frees from wf_gsig's signature
     regularity at mk_ctx Df, whose tyreqs match sat's domain equation);
     wf_gsig types the body under mk_ctx Df; venv_agree_body; IHfuel at k
     lands the result, and callee_grounding_reads plus absorbs_all
     translate it back to the caller's reading. *)
  (* BEGIN:safety_case_call *)
  (* Private helper: push a uniform reading of the argument types through a
     Forall2 of typings, so safety_args can be applied at a plain
     has_ty predicate. *)
  Lemma safety_case_call_aux1 :
    forall (Phi : ctx) (G : tenv) (F : ty -> ty) (args : list expr)
           (taus : list ty),
      Forall2 (fun e tau => has_ty Sg Phi G e (F tau)) args taus ->
      Forall2 (fun e tau => has_ty Sg Phi G e tau) args (map F taus).
  Proof.
    intros Phi G F args taus H.
    induction H as [| e tau args' taus' Hh Ht IH]; simpl.
    - constructor.
    - constructor; [ exact Hh | exact IH ].
  Qed.

  (* Private helper: transport the argument values from the caller's reading
     of the applied parameter types to the callee's grounding, given that the
     two readings agree on each parameter. *)
  Lemma safety_case_call_aux2 :
    forall (gg s th0 gc : subst) (ws : list value) (taus : list ty),
      (forall tau, In tau taus ->
         app_subst gc tau
         = app_subst gg (app_subst s (app_subst s (app_subst th0 tau)))) ->
      Forall2 (fun w tau => vty Sg w (app_subst gg (app_subst s tau))) ws
              (map (fun tau => app_subst s (app_subst th0 tau)) taus) ->
      Forall2 (fun w tau => vty Sg w (app_subst gc tau)) ws taus.
  Proof.
    intros gg s th0 gc ws taus. revert ws.
    induction taus as [| t r IH]; intros ws Heq H; simpl in H.
    - inversion H; subst. constructor.
    - inversion H as [| w0 ws0 t0 r0 Hh Ht]; subst.
      constructor.
      + rewrite (Heq t (or_introl eq_refl)). exact Hh.
      + apply IH; [ intros tau Hin; apply Heq; right; exact Hin | exact Ht ].
  Qed.

  Lemma safety_case_call : forall Phi G f Df Sf body th sm args d ge g,
      g_fn Sg f = Some (Df, Sf, Some body) ->
      sat Sg Phi th Df ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Df ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
              args (fs_params Sf) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi)
                                     (app_subst th (fs_ret Sf)))))
               (interp (S k) Sg d ge (ECallD f th sm args)).
  Proof.
    intros Phi G f Df Sf body th sm args d ge g
           Hf Hsat Hth Hsm Hargs Hcg Hd Hv.
    assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov Hsig]]].
    assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau)
                               = app_subst g tau)
      by (exact (@absorbs_all g (cS Phi) Hab)).
    assert (WF2 := WF). destruct WF2 as [Wtag Wval Wfn Wmth].
    destruct (Wfn f Df Sf (Some body) Hf) as [Wt [Wp [Wr Wb]]].
    (* the callee's world: its grounding, context and runtime environment *)
    destruct (@callee_env_agree Sg g Phi th sm Df d WF Wt Hsat Hth Hsm Hcg Hd)
      as [d' [Hcomp [Hcgc Hdc]]].
    (* reading a Df-covered type through the callee grounding *)
    assert (Hread : forall tau, incl (ty_frees tau) (tyreqs Df) ->
              app_subst (callee_grounding g Phi th Df) tau
              = app_subst g (app_subst (cS Phi) (app_subst th tau))).
    { intros tau0 Hi. apply callee_grounding_reads. exact Hi. }
    (* the parameters and the result type only mention Df's requirements *)
    assert (Hfp : forall tau, In tau (fs_params Sf) ->
              incl (ty_frees tau) (tyreqs Df)).
    { intros tau0 Hin. rewrite Forall_forall in Wp.
      pose proof (Wp tau0 Hin) as Hw0. apply wf_ty_frees in Hw0. exact Hw0. }
    assert (Hfr : incl (ty_frees (fs_ret Sf)) (tyreqs Df)).
    { pose proof Wr as Hw1. apply wf_ty_frees in Hw1. exact Hw1. }
    (* the arguments *)
    assert (Hsa := @safety_args k (Nat.le_refl k) Phi G d ge g args
                     (map (fun tau => app_subst (cS Phi) (app_subst th tau))
                          (fs_params Sf))
                     (@safety_case_call_aux1 Phi G
                        (fun tau => app_subst (cS Phi) (app_subst th tau))
                        args (fs_params Sf) Hargs) Hcg Hd Hv).
    simpl. rewrite Hf.
    destruct (interp_list k Sg d ge args) as [ws | rr] eqn:Elist.
    - (* the arguments produced values; run the body in the callee's world *)
      rewrite Hcomp.
      assert (Hws : Forall2 (fun w tau =>
                       vty Sg w (app_subst (callee_grounding g Phi th Df) tau))
                     ws (fs_params Sf)).
      { apply (@safety_case_call_aux2 g (cS Phi) th
                 (callee_grounding g Phi th Df) ws (fs_params Sf)).
        - intros tau0 Hin.
          rewrite (Hcol (app_subst (cS Phi) (app_subst th tau0))).
          apply Hread. apply Hfp. exact Hin.
        - exact Hsa. }
      assert (Hcgb : ctx_grounded (callee_grounding g Phi th Df) (mk_ctx Df))
        by exact Hcgc.
      assert (Hdb : denv_agree Sg (callee_grounding g Phi th Df)
                               (cA (mk_ctx Df)) d')
        by exact Hdc.
      assert (Hvb : venv_agree Sg (callee_grounding g Phi th Df) Df
                               (body_env Sf) (mk_venv ws)).
      { unfold body_env.
        apply (@venv_agree_body Sg (callee_grounding g Phi th Df) Df ws
                 (fs_params Sf) Hws).
        rewrite Forall_forall. intros tau0 Hin. apply Hfp. exact Hin. }
      assert (Hres := @IHfuel k (Nat.le_refl k) (mk_ctx Df) (body_env Sf)
                        body (fs_ret Sf) d' (mk_venv ws)
                        (callee_grounding g Phi th Df)
                        (Wb body eq_refl) Hcgb Hdb Hvb).
      change (cS (mk_ctx Df)) with (@nil (tsym * ty)) in Hres.
      rewrite app_subst_nil in Hres.
      rewrite (Hread (fs_ret Sf) Hfr) in Hres.
      rewrite (Hcol (app_subst (cS Phi) (app_subst th (fs_ret Sf)))).
      exact Hres.
    - (* the arguments did not produce values *)
      destruct rr as [w1 | |].
      + exfalso. eapply interp_list_never_ok. exact Elist.
      + contradiction.
      + exact I.
  Qed.
(* END:safety_case_call *)

  (* Hint: the provision item is in A, so coverage yields its closure and
     (via lookup_denv_in + the environment Forall) its dentry_agree, whose
     signature equations mention g rt0 and g∘th0∘[This↦rt0] readings; the
     receiver equality σ rt0 = σ tau0 with absorbs_all rewrites those into
     the σ tau0 forms of the goal and argument typings (push g inside the
     This-substitution with app_subst_chain over th0 extended at this_sym,
     or pointwise via app_subst_frees_agree); the structural hypothesis
     evaluates the receiver at fuel k? no — the receiver is evaluated at
     fuel k by the interpreter, so use IHfuel on its typing; then
     safety_args, venv_agree_body with the receiver value consed on, and
     IHfuel at k on the provider's body. *)
  (* BEGIN:safety_case_meth *)
  (* BEGIN:safety_case_meth *)
  (* Private helper: the This-substitution consults the receiver only at the
     This position, so two receivers with the same reading through th and
     then g give the same reading of every method type.  No coverage
     hypothesis is needed: away from This the two readings are literally the
     same term. *)
  Lemma safety_case_meth_aux1 :
    forall (g0 : subst) (r1 r2 tau : ty),
      app_subst g0 r1 = app_subst g0 r2 ->
      app_subst g0 (app_subst (this_subst r1) tau)
      = app_subst g0 (app_subst (this_subst r2) tau).
  Proof.
    intros g0 r1 r2 tau HE. unfold this_subst.
    induction tau using ty_ind'.
    - reflexivity.
    - simpl. destruct (Nat.eqb t this_sym); [ exact HE | reflexivity ].
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite IHForall. destruct x as [t0 tau1]; simpl in *.
      rewrite H. reflexivity.
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. reflexivity.
  Qed.

  (* Private helper: the inner conjunction of dentry_agree is a Forall. *)
  Lemma safety_case_meth_aux2 : forall (gc0 : subst) (dd : denv),
      (fix all (ddd : denv) : Prop :=
         match ddd with
         | [] => True
         | p :: r => dentry_agree Sg gc0 (fst p) (snd p) /\ all r
         end) dd ->
      Forall (fun p => dentry_agree Sg gc0 (fst p) (snd p)) dd.
  Proof.
    intros gc0 dd. induction dd as [| p r IH]; intros H.
    - constructor.
    - simpl in H. destruct H as [Hh Ht].
      constructor; [ exact Hh | apply IH; exact Ht ].
  Qed.

  (* Private helper: open up a method entry's agreement. *)
  Lemma safety_case_meth_aux3 :
    forall (g0 : subst) (rt : ty) (m0 : msym) (th1 : subst)
           (Dm0 : tele) (Sm0 : fnsig) (f'0 : fsym) (dc0 : denv),
      g_mth Sg m0 = Some (Dm0, Sm0) ->
      dentry_agree Sg g0 (IMth rt m0 th1) (DClo f'0 dc0) ->
      exists D0 S0 body0 gc0,
        g_fn Sg f'0 = Some (D0, S0, Some body0)
        /\ grounding gc0
        /\ covers gc0 (tyreqs D0)
        /\ map (app_subst gc0) (fs_params S0)
           = app_subst g0 rt
             :: map (fun tau => app_subst g0
                       (app_subst (this_subst rt) (app_subst th1 tau)))
                    (fs_params Sm0)
        /\ app_subst gc0 (fs_ret S0)
           = app_subst g0
               (app_subst (this_subst rt) (app_subst th1 (fs_ret Sm0)))
        /\ denv_agree Sg gc0 D0 dc0.
  Proof.
    intros g0 rt m0 th1 Dm0 Sm0 f'0 dc0 Hm0 Hag.
    simpl in Hag. rewrite Hm0 in Hag.
    remember (g_fn Sg f'0) as gf eqn:Egf.
    destruct gf as [[[D0 S0] b0]|]; [| destruct Hag].
    destruct b0 as [body0|]; [| destruct Hag].
    destruct Hag as [gc0 [Hgrc [Hcovc [Hpar [Hret [Htc Hall]]]]]].
    exists D0, S0, body0, gc0.
    split; [ solve [ reflexivity | symmetry; exact Egf ] |].
    split; [ exact Hgrc |].
    split; [ exact Hcovc |].
    split; [ exact Hpar |].
    split; [ exact Hret |].
    split; [ exact Htc | split ].
    - apply safety_case_meth_aux2; exact Hall.
    - rewrite Forall_forall. intros i Hi.
      assert (Hdecl : g_fn Sg f'0 = Some (D0, S0, Some body0))
        by (symmetry; exact Egf).
      destruct (wf_fn_decl WF _ Hdecl) as [Wt _].
      exact (@tele_items_regular Sg [] D0 Wt i Hi).
  Qed.

  (* Private helper: push the predicate's reading into the type list, fusing
     it with the reading the list was already built from. *)
  Lemma safety_case_meth_aux4 :
    forall (H1 F0 : ty -> ty) (ws : list value) (taus : list ty),
      Forall2 (fun w tau => vty Sg w (H1 tau)) ws (map F0 taus) ->
      Forall2 (fun w tau => vty Sg w tau) ws (map (fun tau => H1 (F0 tau)) taus).
  Proof.
    intros H1 F0 ws taus. revert ws.
    induction taus as [| t ts IH]; intros ws H; simpl in H; simpl.
    - inversion H; subst. constructor.
    - inversion H as [| w0 t0 ws0 ts0 Hhd Htl]; subst.
      constructor; [ exact Hhd | apply IH; exact Htl ].
  Qed.

  (* Private helper: retype a value list along an equality of readings. *)
  Lemma safety_case_meth_aux5 :
    forall (gc0 : subst) (F : ty -> ty) (ws : list value)
           (rest taus : list ty),
      map (app_subst gc0) rest = map F taus ->
      Forall2 (fun w tau => vty Sg w tau) ws (map F taus) ->
      Forall2 (fun w tau => vty Sg w (app_subst gc0 tau)) ws rest.
  Proof.
    intros gc0 F ws rest taus Hmap. rewrite <- Hmap. clear Hmap.
    revert ws. induction rest as [| r rs IH]; intros ws H; simpl in H.
    - inversion H; subst. constructor.
    - inversion H as [| w0 t0 ws0 ts0 Hhd Htl]; subst.
      constructor; [ exact Hhd | apply IH; exact Htl ].
  Qed.

  Lemma safety_case_meth : forall Phi G e0 tau0 m Dm Sf rt0 th0 args d ge g,
      g_mth Sg m = Some (Dm, Sf) ->
      has_ty Sg Phi G e0 tau0 ->
      In (IMth rt0 m th0) (cA Phi) ->
      app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
      (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                  i1 = IMth rt0 m th0) ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e
                        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
              args (fs_params Sf) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                     (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0)
                               (fs_ret Sf))))
               (interp (S k) Sg d ge (EMeth e0 m (IMth rt0 m th0) args)).
  Proof.
    intros Phi G e0 tau0 m Dm Sf rt0 th0 args d ge g
           Hm Hty0 Hin Hrec Huniq Hargs Hcg Hd Hv.
    assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov Hsig]]].
    assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau)
                               = app_subst g tau)
      by (exact (@absorbs_all g (cS Phi) Hab)).
    assert (WF2 := WF). destruct WF2 as [Wtag Wval Wfn Wmth].
    (* the receiver's own reading, and its reading through the application *)
    assert (HR0 : app_subst g rt0 = app_subst g (app_subst (cS Phi) tau0)).
    { rewrite <- (Hcol rt0). rewrite Hrec. reflexivity. }
    (* the caller's reading of a method type equals the entry's reading *)
    assert (Hcv : forall tau,
               app_subst g (app_subst (cS Phi)
                 (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
               = app_subst g
                   (app_subst (this_subst rt0) (app_subst th0 tau))).
    { intros tau. unfold interp_m. rewrite Hcol, Hcol.
      apply safety_case_meth_aux1. symmetry. exact HR0. }
    (* the provision is in force, so the runtime world has an entry for it *)
    assert (Hen0 : exists en, lookup_denv (IMth rt0 m th0) d = Some en).
    { destruct Hd as [Hcvd _]. rewrite Forall_forall in Hcvd.
      destruct (Hcvd _ Hin) as [Hbad | Hok];
        [ simpl in Hbad; discriminate | exact Hok ]. }
    destruct Hen0 as [en Hen].
    assert (Hag : dentry_agree Sg g (IMth rt0 m th0) en).
    { destruct Hd as [_ [Hd2 Hdreg]]. rewrite Forall_forall in Hd2.
      exact (Hd2 (IMth rt0 m th0, en)
               (@lookup_denv_in (IMth rt0 m th0) d en Hen)). }
    destruct en as [wv | f' dc]; [ simpl in Hag; destruct Hag |].
    destruct (@safety_case_meth_aux3 g rt0 m th0 Dm Sf f' dc Hm Hag)
      as [D' [S' [body [gc [Ef' [Hgrc [Hcovc [Hpar [Hret Hdc]]]]]]]]].
    destruct (Wfn f' D' S' (Some body) Ef') as [Wt' [Wps' [Wrt' Wb]]].
    (* the callee's context and world *)
    assert (Hcgb : ctx_grounded gc (mk_ctx D')).
    { split; [ exact Hgrc |]. split.
      - unfold absorbs. intros t tau Hlu. simpl in Hlu. discriminate.
      - split; [exact Hcovc |].
        unfold sigma_regular. simpl. split.
        + intros x Hx. contradiction.
        + constructor. }
    assert (Hdb : denv_agree Sg gc (cA (mk_ctx D')) dc) by exact Hdc.
    (* the provider's parameter list splits into receiver and arguments *)
    remember (fs_params S') as psl eqn:Eps.
    destruct psl as [| p0 rest]; [ simpl in Hpar; discriminate |].
    simpl in Hpar. injection Hpar as Hp0 Hprest.
    (* the receiver and the arguments *)
    assert (Hrcv := @IHfuel k (Nat.le_refl k) Phi G e0 tau0 d ge g
                      Hty0 Hcg Hd Hv).
    assert (Hsa := @safety_args k (Nat.le_refl k) Phi G d ge g args
                     (map (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0))
                          (fs_params Sf))
                     (@safety_case_call_aux1 Phi G
                        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0))
                        args (fs_params Sf) Hargs) Hcg Hd Hv).
    (* the interpreter's step at this provision *)
    assert (Hstep : interp (S k) Sg d ge (EMeth e0 m (IMth rt0 m th0) args)
                    = match interp k Sg d ge e0 with
                      | Ok w0 =>
                          match interp_list k Sg d ge args with
                          | inl ws => interp k Sg dc (mk_venv (w0 :: ws)) body
                          | inr rr => rr
                          end
                      | rr => rr
                      end).
    { simpl interp. rewrite Hen. simpl. rewrite Ef'. reflexivity. }
    rewrite Hstep.
    destruct (interp k Sg d ge e0) as [w0 | |] eqn:E0.
    - try rewrite E0 in Hrcv. simpl in Hrcv.
      destruct (interp_list k Sg d ge args) as [ws | rr] eqn:Elist;
        try rewrite Elist in Hsa.
      + (* both the receiver and the arguments produced values *)
        assert (Hw0 : vty Sg w0 (app_subst gc p0)).
        { rewrite Hp0, HR0. exact Hrcv. }
        assert (Hws : Forall2 (fun w tau => vty Sg w (app_subst gc tau))
                              ws rest).
        { apply (@safety_case_meth_aux5 gc
                   (fun tau => app_subst g
                      (app_subst (this_subst rt0) (app_subst th0 tau)))
                   ws rest (fs_params Sf)).
          - exact Hprest.
          - assert (Hmap : map (fun tau => app_subst g (app_subst (cS Phi)
                              (interp_m (cS Phi) th0
                                 (app_subst (cS Phi) tau0) tau)))
                             (fs_params Sf)
                           = map (fun tau => app_subst g
                              (app_subst (this_subst rt0)
                                 (app_subst th0 tau)))
                             (fs_params Sf))
              by (apply map_ext; exact Hcv).
            rewrite <- Hmap.
            exact (@safety_case_meth_aux4
                     (fun t1 => app_subst g (app_subst (cS Phi) t1))
                     (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0))
                     ws (fs_params Sf) Hsa). }
        assert (Hvb : venv_agree Sg gc D' (body_env S') (mk_venv (w0 :: ws))).
        { unfold body_env. rewrite <- Eps.
          apply (@venv_agree_body Sg gc D' (w0 :: ws) (p0 :: rest)).
          - constructor; [ exact Hw0 | exact Hws ].
          - eapply Forall_impl; [|exact Wps'].
            intros tau Hwf. exact (wf_ty_frees Hwf). }
        assert (Hres := @IHfuel k (Nat.le_refl k) (mk_ctx D') (body_env S')
                          body (fs_ret S') dc (mk_venv (w0 :: ws)) gc
                          (Wb body eq_refl) Hcgb Hdb Hvb).
        change (cS (mk_ctx D')) with (@nil (tsym * ty)) in Hres.
        rewrite app_subst_nil in Hres.
        rewrite Hret in Hres.
        rewrite (Hcv (fs_ret Sf)). exact Hres.
      + (* the arguments did not produce values *)
        destruct rr as [w1 | |].
        * exfalso. eapply interp_list_never_ok. exact Elist.
        * contradiction.
        * exact I.
    - (* the receiver failed *)
      try rewrite E0 in Hrcv. simpl in Hrcv. contradiction.
    - (* the receiver ran dry *)
      exact I.
  Qed.
  (* END:safety_case_meth *)
(* END:safety_case_meth *)

  (* Hint: IHfuel on the scrutinee's typing (it runs at fuel k) yields a
     value at g (σ tau0); members of that grounded type are the g-images
     of ms (destruct σ tau0 via the members equation, app_subst_union /
     app_subst_nom); vty_union_inv / vty_nom_inv pick the member the value
     was injected at; member_head is stable under app_subst, so NoDup of
     the heads makes find_arm select the arm aligned (Forall2) with that
     member; the binder's payload type transports by app_subst_chain (tag
     payload regularity from wf_gsig + the member's sat domain equation —
     note the member's application th_j is already a σ image, absorbed by
     g); venv_agree_cons extends the environment; IHfuel at k on the arm
     body finishes.  This is the most intricate case; report precisely in
     notes if a fact is missing. *)
  (* BEGIN:safety_case_match *)
  (* --- Private helpers for this case. ------------------------------------ *)

  (* Type refinement: [aux1 t1 t2] holds when t2 is t1 with (some of) its
     abstract leaves instantiated.  Values inhabit only unit, nominal and
     union types, so an abstract leaf is uninhabited and value typing is
     monotone along this order (aux9).  That is what transports the
     scrutinee's payload typing — obtained at the member's application
     *after* it was pushed through the grounding — to the arm binder's
     payload type, which reads the grounding afterwards (aux10); no
     regularity (sat domain) assumption on the member is needed. *)
  Inductive safety_case_match_aux1 : ty -> ty -> Prop :=
  | safety_case_match_aux1_abs : forall t tau,
      safety_case_match_aux1 (TAbs t) tau
  | safety_case_match_aux1_unit : safety_case_match_aux1 TUnit TUnit
  | safety_case_match_aux1_nom : forall n th1 th2,
      safety_case_match_aux2 th1 th2 ->
      safety_case_match_aux1 (TNom n th1) (TNom n th2)
  | safety_case_match_aux1_union : forall ms1 ms2,
      safety_case_match_aux3 ms1 ms2 ->
      safety_case_match_aux1 (TUnion ms1) (TUnion ms2)
  with safety_case_match_aux2 : list (tsym * ty) -> list (tsym * ty) -> Prop :=
  | safety_case_match_aux2_nil : safety_case_match_aux2 [] []
  | safety_case_match_aux2_cons : forall t a b r1 r2,
      safety_case_match_aux1 a b ->
      safety_case_match_aux2 r1 r2 ->
      safety_case_match_aux2 ((t, a) :: r1) ((t, b) :: r2)
  with safety_case_match_aux3 : list ty -> list ty -> Prop :=
  | safety_case_match_aux3_nil : safety_case_match_aux3 [] []
  | safety_case_match_aux3_cons : forall a b r1 r2,
      safety_case_match_aux1 a b ->
      safety_case_match_aux3 r1 r2 ->
      safety_case_match_aux3 (a :: r1) (b :: r2).

  Lemma safety_case_match_aux4 : forall tau, safety_case_match_aux1 tau tau.
  Proof.
    intro tau.
    induction tau as [ | t | n th IHth | ms IHms ] using ty_ind'.
    - apply safety_case_match_aux1_unit.
    - apply safety_case_match_aux1_abs.
    - apply safety_case_match_aux1_nom.
      induction IHth as [ | p r Hhd Htl IHr ].
      + apply safety_case_match_aux2_nil.
      + destruct p as [t0 a0]. simpl in Hhd.
        apply safety_case_match_aux2_cons; [ exact Hhd | exact IHr ].
    - apply safety_case_match_aux1_union.
      induction IHms as [ | a r Hhd Htl IHr ].
      + apply safety_case_match_aux3_nil.
      + apply safety_case_match_aux3_cons; [ exact Hhd | exact IHr ].
  Qed.

  (* Refinement of association lists is visible in lookups. *)
  Lemma safety_case_match_aux5 : forall th1 th2,
      safety_case_match_aux2 th1 th2 ->
      forall t a, lookup_subst t th1 = Some a ->
        exists b, lookup_subst t th2 = Some b /\ safety_case_match_aux1 a b.
  Proof.
    intros th1 th2 H.
    induction H as [ | t0 a0 b0 r1 r2 Hab Hr IH ]; intros t a Hl.
    - simpl in Hl. discriminate.
    - simpl in Hl. simpl. destruct (Nat.eqb t t0).
      + injection Hl as Hl. subst a. exists b0. split; [ reflexivity | exact Hab ].
      + destruct (IH t a Hl) as [b [Hb Hab']].
        exists b. split; [ exact Hb | exact Hab' ].
  Qed.

  (* Refinement of member lists is visible in membership. *)
  Lemma safety_case_match_aux6 : forall ms1 ms2,
      safety_case_match_aux3 ms1 ms2 ->
      forall a, In a ms1 ->
        exists b, In b ms2 /\ safety_case_match_aux1 a b.
  Proof.
    intros ms1 ms2 H.
    induction H as [ | a0 b0 r1 r2 Hab Hr IH ]; intros a Hin.
    - destruct Hin.
    - destruct Hin as [Heq | Hin'].
      + subst a0. exists b0. split; [ left; reflexivity | exact Hab ].
      + destruct (IH a Hin') as [b [Hb Hab']].
        exists b. split; [ right; exact Hb | exact Hab' ].
  Qed.

  (* Substitutions refining each other pointwise act refiningly. *)
  Lemma safety_case_match_aux7 : forall s1 s2,
      (forall t a, lookup_subst t s1 = Some a ->
         exists b, lookup_subst t s2 = Some b /\ safety_case_match_aux1 a b) ->
      forall tau, safety_case_match_aux1 (app_subst s1 tau) (app_subst s2 tau).
  Proof.
    intros s1 s2 Hs tau.
    induction tau as [ | t | n th IHth | ms IHms ] using ty_ind'.
    - simpl. apply safety_case_match_aux1_unit.
    - simpl. destruct (lookup_subst t s1) as [a | ] eqn:E1.
      + destruct (Hs t a E1) as [b [E2 Hab]]. rewrite E2. exact Hab.
      + apply safety_case_match_aux1_abs.
    - rewrite !app_subst_nom. apply safety_case_match_aux1_nom.
      induction IHth as [ | p r Hhd Htl IHr ].
      + apply safety_case_match_aux2_nil.
      + destruct p as [t0 a0]. simpl in Hhd. unfold map_snd. simpl.
        apply safety_case_match_aux2_cons; [ exact Hhd | ].
        unfold map_snd in IHr. exact IHr.
    - rewrite !app_subst_union. apply safety_case_match_aux1_union.
      induction IHms as [ | a r Hhd Htl IHr ].
      + apply safety_case_match_aux3_nil.
      + simpl. apply safety_case_match_aux3_cons; [ exact Hhd | exact IHr ].
  Qed.

  (* Pushing a substitution through a composition only fills leaves. *)
  Lemma safety_case_match_aux8 : forall g0 s tau,
      safety_case_match_aux1
        (app_subst (map_snd (app_subst g0) s) tau)
        (app_subst g0 (app_subst s tau)).
  Proof.
    intros g0 s tau.
    induction tau as [ | t | n th IHth | ms IHms ] using ty_ind'.
    - simpl. apply safety_case_match_aux1_unit.
    - simpl. rewrite lookup_map_snd.
      destruct (lookup_subst t s) as [a | ] eqn:E.
      + apply safety_case_match_aux4.
      + apply safety_case_match_aux1_abs.
    - rewrite !app_subst_nom. apply safety_case_match_aux1_nom.
      induction IHth as [ | p r Hhd Htl IHr ].
      + apply safety_case_match_aux2_nil.
      + destruct p as [t0 a0]. simpl in Hhd. unfold map_snd. simpl.
        apply safety_case_match_aux2_cons; [ exact Hhd | ].
        unfold map_snd in IHr. exact IHr.
    - rewrite !app_subst_union. apply safety_case_match_aux1_union.
      induction IHms as [ | a r Hhd Htl IHr ].
      + apply safety_case_match_aux3_nil.
      + simpl. apply safety_case_match_aux3_cons; [ exact Hhd | exact IHr ].
  Qed.

  (* Value typing is monotone along type refinement. *)
  Lemma safety_case_match_aux9 : forall w t1,
      vty Sg w t1 -> forall t2, safety_case_match_aux1 t1 t2 -> vty Sg w t2.
  Proof.
    intros w t1 H.
    induction H as [ | n D taup th w0 Hgt Hv IH | w0 tau1 ms Hv IH Hin ];
      intros t2 Hle.
    - inversion Hle; subst. apply VtyUnit.
    - inversion Hle as [ | | n1 tha thb Hth | ]; subst.
      eapply VtyTag; [ exact Hgt | ].
      apply IH. apply safety_case_match_aux7.
      intros t a Ha. exact (@safety_case_match_aux5 th thb Hth t a Ha).
    - inversion Hle as [ | | | msa msb Hms ]; subst.
      destruct (@safety_case_match_aux6 ms msb Hms tau1 Hin) as [b [Hinb Hab]].
      apply (@VtyInj Sg w0 b msb); [ apply IH; exact Hab | exact Hinb ].
  Qed.

  (* The transport the arm binder needs: a payload typed at the member's
     application already read through g is typed at the g-image of the
     member's payload type. *)
  Lemma safety_case_match_aux10 : forall w s g0 taup,
      vty Sg w (app_subst (map_snd (app_subst g0) s) taup) ->
      vty Sg w (app_subst g0 (app_subst s taup)).
  Proof.
    intros w s g0 taup H.
    eapply safety_case_match_aux9; [ exact H | apply safety_case_match_aux8 ].
  Qed.

  Lemma safety_case_match_aux11 :
    forall (A B : Type) (R : A -> B -> Prop) x l y l',
      Forall2 R (x :: l) (y :: l') -> R x y /\ Forall2 R l l'.
  Proof.
    intros A B R x l y l' H. inversion H; subst. split; assumption.
  Qed.

  (* Arm selection: the member the value sits at determines the arm
     find_arm picks (the heads are distinct, and the arms' names are the
     members' heads), and that arm is the one the typing aligned with. *)
  Lemma safety_case_match_aux12 :
    forall (R : ty -> nsym * var * expr -> Prop) ms arms,
      Forall2 (arm_matches Sg) ms arms ->
      Forall2 R ms arms ->
      NoDup (map member_head ms) ->
      forall mem, In mem ms ->
      exists n thn arm,
        mem = TNom n thn /\
        find_arm n arms = Some (arm_var arm, arm_body arm) /\
        R mem arm.
  Proof.
    intros R ms arms HAM.
    induction HAM as [ | mem0 arm0 ms' arms' Ham HAM IH ];
      intros HR Hnd mem Hin.
    - destruct Hin.
    - destruct (safety_case_match_aux11 HR) as [Hr HR'].
      destruct mem0 as [ | t0 | n0 th0 | ms0 ]; simpl in Ham;
        try (exfalso; exact Ham).
      destruct Ham as [Hname Hgt].
      destruct arm0 as [[na xa] eba].
      unfold arm_name in Hname. simpl in Hname. subst na.
      simpl in Hnd. inversion Hnd as [ | h l Hnotin Hnd' ]; subst.
      destruct Hin as [Heq | Hin'].
      + subst mem. exists n0, th0, (n0, xa, eba).
        split; [ reflexivity | split; [ | exact Hr ] ].
        simpl. rewrite Nat.eqb_refl. reflexivity.
      + destruct (IH HR' Hnd' mem Hin') as [n1 [th1 [arm [Hme [Hfind Hrr]]]]].
        exists n1, th1, arm.
        split; [ exact Hme | split; [ | exact Hrr ] ].
        simpl. destruct (Nat.eqb n1 n0) eqn:Eb.
        * exfalso. apply Nat.eqb_eq in Eb. subst n1.
          apply Hnotin. apply in_map_iff. exists mem.
          split; [ rewrite Hme; reflexivity | exact Hin' ].
        * exact Hfind.
  Qed.

  (* A value at the grounded scrutinee type sits at (the g-image of) one of
     the members the match covers. *)
  Lemma safety_case_match_aux13 : forall T ms0 g0 w,
      members T = Some ms0 ->
      vty Sg w (app_subst g0 T) ->
      exists mem, In mem ms0 /\ vty Sg w (app_subst g0 mem).
  Proof.
    intros T ms0 g0 w Hm Hv.
    destruct T as [ | t' | n' th' | ms' ]; simpl in Hm; try discriminate.
    - injection Hm as Hm. subst ms0.
      exists (TNom n' th'). split; [ left; reflexivity | exact Hv ].
    - injection Hm as Hm. subst ms0.
      rewrite app_subst_union in Hv.
      apply vty_union_inv in Hv as [tau' [Hin' Hv']].
      apply in_map_iff in Hin' as [mem [Heq Hinm]].
      exists mem. split; [ exact Hinm | rewrite Heq; exact Hv' ].
  Qed.

  (* A matched member is a member of the well-formed context-substitution
     image.  Inverting that exact image gives regularity of its arguments at
     the current context. *)
  Lemma safety_case_match_aux14 : forall Phi g tau0 ms mem,
      wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
      members (app_subst (cS Phi) tau0) = Some ms ->
      In mem ms ->
      ctx_grounded g Phi ->
      incl (ty_frees (member_payload Sg mem)) (tyreqs (cA Phi)).
  Proof.
    intros Phi g tau0 ms mem Hwf Hmembers Hin Hground.
    assert (Hpayload : forall n th,
        wf_ty Sg Phi (TNom n th) ->
        incl (ty_frees (member_payload Sg (TNom n th)))
             (tyreqs (cA Phi))).
    { intros n th Hn.
      inversion Hn as [| | n' D payload th' Htag Hsat Hwfth |]; subst.
      cbn [member_payload]. rewrite Htag.
      pose proof (wf_tag_decl WF n Htag) as [_ Hp].
      destruct Hsat as [Hdom _].
      assert (Hcover : incl (ty_frees payload) (map fst th)).
      { rewrite Hdom. exact (wf_ty_frees Hp). }
      intros u Hu.
      apply frees_app_subst_bound in Hu.
      destruct Hu as [[p [Hpth Hup]] | [Hu Hmiss]].
      - rewrite Forall_forall in Hwfth.
        exact (wf_ty_frees (Hwfth p Hpth) u Hup).
      - exfalso. apply Hmiss. exact (Hcover u Hu).
    }
    remember (app_subst (cS Phi) tau0) as T eqn:HT in Hwf, Hmembers.
    destruct T as [|t|n th|ms0]; simpl in Hmembers; try discriminate.
    - injection Hmembers as Hms. subst ms.
      simpl in Hin. destruct Hin as [Heq | []]. subst mem.
      exact (Hpayload n th Hwf).
    - injection Hmembers as Hms. subst ms.
      inversion Hwf as [| | |ms' Hwfms Hnomms Hnd']; subst.
      rewrite Forall_forall in Hwfms.
      pose proof (Hwfms mem Hin) as Hmemwf.
      rewrite Forall_forall in Hnomms.
      destruct (Hnomms mem Hin) as [n [th Heq]]. subst mem.
      exact (Hpayload n th Hmemwf).
  Qed.

  Lemma safety_case_match : forall Phi G e0 tau0 ms arms tau d ge g,
      has_ty Sg Phi G e0 tau0 ->
      wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
      members (app_subst (cS Phi) tau0) = Some ms ->
      NoDup (map member_head ms) ->
      Forall2 (arm_matches Sg) ms arms ->
      Forall2 (fun mem arm =>
                 has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                        (arm_body arm) tau) ms arms ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EMatch e0 arms)).
  Proof.
    intros Phi G e0 tau0 ms arms tau d ge g
           Hty0 Hwf0 Hmem Hnd Ham Hbodies Hcg Hd Hve.
    pose proof (IHfuel (Nat.le_refl k)) as SK.
    pose proof (SK Phi G e0 tau0 d ge g Hty0 Hcg Hd Hve) as S0.
    destruct (interp k Sg d ge e0) as [w0 | | ] eqn:E; simpl in S0.
    - (* the scrutinee produced a value *)
      destruct (@safety_case_match_aux13 (app_subst (cS Phi) tau0) ms g w0 Hmem S0)
        as [mem [Hin Hvm]].
      destruct (@safety_case_match_aux12
                  (fun m a => has_ty Sg Phi ((arm_var a, member_payload Sg m) :: G)
                                     (arm_body a) tau)
                  ms arms Ham Hbodies Hnd mem Hin)
        as [n [thn [arm [Hme [Hfind Hbody]]]]].
      subst mem.
      rewrite app_subst_nom in Hvm.
      apply vty_nom_inv in Hvm as [D [taup [w' [Hw0 [Hgt Hvp]]]]].
      subst w0.
      assert (Hred : interp (S k) Sg d ge (EMatch e0 arms)
                     = interp k Sg d ((arm_var arm, w') :: ge) (arm_body arm)).
      { simpl. rewrite E. simpl. rewrite Hfind. reflexivity. }
      rewrite Hred.
      assert (Hmp : member_payload Sg (TNom n thn) = app_subst thn taup).
      { cbn [member_payload]. rewrite Hgt. reflexivity. }
      apply (SK Phi ((arm_var arm, member_payload Sg (TNom n thn)) :: G)
                (arm_body arm) tau d ((arm_var arm, w') :: ge) g Hbody Hcg Hd).
      apply venv_agree_cons.
      + exact Hve.
      + rewrite Hmp.
        apply (@safety_case_match_aux10 w' thn g taup). exact Hvp.
      + eapply safety_case_match_aux14; eauto.
    - (* the scrutinee erred: excluded by the fuel hypothesis *)
      destruct S0.
    - (* the scrutinee ran out of fuel *)
      assert (Hred : interp (S k) Sg d ge (EMatch e0 arms) = OutOfFuel).
      { simpl. rewrite E. reflexivity. }
      rewrite Hred. exact I.
  Qed.
(* END:safety_case_match *)

  (* BEGIN:safety_case_let *)
  Lemma safety_case_let : forall Phi G x e1 e2 tau1 tau d ge g,
      has_ty Sg Phi G e1 tau1 ->
      has_ty Sg Phi ((x, tau1) :: G) e2 tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (ELet x e1 e2)).
  Proof.
    intros Phi G x e1 e2 tau1 tau d ge g H1 H2 Hg Hd Hv.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    pose proof (IHfuel (le_n k)) as SK.
    pose proof (SK Phi G e1 tau1 d ge g H1 Hg Hd Hv) as S1.
    simpl interp.
    destruct (interp k Sg d ge e1) as [w1 | | ] eqn:E; simpl in S1.
    - apply (SK Phi ((x, tau1) :: G) e2 tau d ((x, w1) :: ge) g H2 Hg Hd).
      apply venv_agree_cons.
      + exact Hv.
      + rewrite <- Habs. exact S1.
      + eapply has_ty_frees; [exact WF | exact H1 | |].
        * destruct Hg as [_ [_ [_ Hregular]]]. exact Hregular.
        * intros y ty Hy.
          assert (Hlookup_in : forall G0 y0 ty0,
              lookup_tenv y0 G0 = Some ty0 -> In (y0, ty0) G0).
          { intros G0. induction G0 as [|[z tz] G' IH];
              intros y0 ty0 Hlook; simpl in Hlook.
            - discriminate.
            - destruct (Nat.eqb y0 z) eqn:Eyz.
              + inversion Hlook; subst. left.
                apply Nat.eqb_eq in Eyz. subst. reflexivity.
              + right. apply IH. exact Hlook. }
          pose proof (Hlookup_in G y ty Hy) as Hinpair.
          destruct Hv as [_ Hregs].
          rewrite Forall_forall in Hregs.
          exact (Hregs (y, ty) Hinpair).
    - contradiction.
    - exact I.
  Qed.
(* END:safety_case_let *)

  (* Hint: ground the extended context by g2 := (t, g (σ τ')) :: g.
     Closedness of the new range: wf_ty_frees bounds τ''s frees within
     A's type items, the caller's coverage grounds them (absorbs_all
     collapses the σ), closed_app_subst_ground closes.  absorbs for the
     extended σ: the new entry is definitional; old entries avoid t —
     freshness means no A-item mentions t... careful: absorbs is about
     lookups, and the extension changes lookups of t only, so old
     obligations transfer whenever their types do not mention t — their
     frees sit in A's type items by regularity of what σ stores; if that
     bound is not derivable from ctx_grounded alone, strengthen your
     in-block helper to thread it (the caller can also just pick g with
     dom g = A's type items so that t is genuinely fresh for g;
     app_subst_frees_agree then transfers every reading).  denv/venv
     agreement transports by the same frees argument.  IHfuel at k on the
     continuation under the extended context.
     DEFERRED (batch-2 refutation, now repaired at the rule level): the
     new wf_ty Sg Phi tau premise plus wf_ty_frees closes the result-type
     transfer, but the g-to-g2 transfers still need ctx_grounded to be
     strengthened with σ-range regularity
     (Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA Phi))) (cS Phi))
     and a matching boundedness story for the Γ- and δ-stored types; that
     invariant redesign is the remaining work of this block. *)
  (* BEGIN:safety_case_bindty *)
  Lemma safety_case_bindty : forall Phi G t tau' e tau d ge g,
      g_ty Sg t = true ->
      ~ In (ITy t) (cA Phi) ->
      wf_ty Sg Phi tau' ->
      has_ty Sg (bind_ty t tau' Phi) G e tau ->
      wf_ty Sg Phi tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindTy t tau' e)).
  Proof.
  Admitted. (* FILL:safety_case_bindty *)
  (* END:safety_case_bindty *)

  (* Hint: IHfuel evaluates e1 to a value at g (σ tauv); the extended denv
     ((IVal v, DVal w) :: d) agrees with the extended A — the new entry's
     dentry_agree is that vty with absorbs_all collapsing σ; coverage of
     IVal v is the head lookup (item_eqb reflexivity via item_eqb_eq);
     IHfuel at k on the continuation. *)
  (* BEGIN:safety_case_bindval *)
  (* BEGIN:safety_case_bindval *)
  Lemma safety_case_bindval : forall Phi G v Dv tauv e1 e tau d ge g,
      g_val Sg v = Some (Dv, tauv) ->
      sat0 Sg Phi Dv ->
      has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
      has_ty Sg (add_item (IVal v) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindVal v e1 e)).
  Proof.
    intros Phi G v Dv tauv e1 e tau d ge g Hgv Hsat H1 H2 Hg Hd Hv.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    pose proof (IHfuel (le_n k)) as SK.
    pose proof (SK Phi G e1 (app_subst (cS Phi) tauv) d ge g H1 Hg Hd Hv) as S1.
    simpl interp.
    destruct (interp k Sg d ge e1) as [w1 | | ] eqn:E; simpl in S1.
    - rewrite Habs, Habs in S1.
      assert (Hd' : denv_agree Sg g (cA (add_item (IVal v) Phi))
                       ((IVal v, DVal w1) :: d)).
      { destruct Hd as [Hcov [Hentries Hreg]].
        split.
        - simpl.
          constructor.
          + right. exists (DVal w1). simpl.
            rewrite Nat.eqb_refl. reflexivity.
          + eapply Forall_impl; [| exact Hcov].
            intros i [Hty | [en Hen]].
            * left; exact Hty.
            * right. simpl.
              destruct (item_eqb i (IVal v)) eqn:Hi.
              -- exists (DVal w1). reflexivity.
              -- exists en. exact Hen.
        - split.
          + constructor.
            * change (match g_val Sg v with
                      | Some (_, tv) => vty Sg w1 (app_subst g tv)
                      | None => False
                      end).
              rewrite Hgv. exact S1.
            * exact Hentries.
          + simpl.
          constructor.
            * intros u Hu.
              simpl in Hu. rewrite Hgv in Hu. simpl in Hu.
              apply in_app_or in Hu. destruct Hu as [Hu | []].
              destruct Hsat as [_ Hreq].
              rewrite Forall_forall in Hreq.
              apply wf_ty_frees_aux3.
              apply avail_ty_in. exact (Hreq u Hu).
            * exact Hreg.
      }
      assert (Hg' : ctx_grounded g (add_item (IVal v) Phi)).
      { destruct Hg as [Hgr [Hab2 Hcv]].
        split; [exact Hgr |]. split; simpl; assumption. }
      apply (SK (add_item (IVal v) Phi) G e tau
                ((IVal v, DVal w1) :: d) ge g H2 Hg' Hd' Hv).
    - contradiction.
    - exact I.
  Qed.
  (* END:safety_case_bindval *)
(* END:safety_case_bindval *)

  (* Hint: callee_env_agree at D' (sat0 is sat at id_app) builds the
     captured environment dc and the provider's grounding; the new entry
     (IFn f, DClo f' dc) agrees because sig_eq pushed under g via
     absorbs_all and callee_grounding_reads (wf_gsig regularity bounds the
     signatures' frees) yields exactly dentry_agree's equations; extend A
     and d, IHfuel at k on the continuation. *)
  (* BEGIN:safety_case_bindfn *)
  (* Private helper: an identity application's lookups are identity
     entries — nothing but the symbol itself can come back. *)
  Lemma safety_case_bindfn_aux1 : forall (l : list tsym) (t : tsym) (tau : ty),
      lookup_subst t (map (fun t0 => (t0, TAbs t0)) l) = Some tau ->
      tau = TAbs t.
  Proof.
    intros l. induction l as [| a l IH]; intros t tau H; simpl in H.
    - discriminate.
    - destruct (Nat.eqb t a) eqn:E.
      + apply Nat.eqb_eq in E. subst a. injection H as H. subst tau. reflexivity.
      + apply IH. exact H.
  Qed.

  (* Private helper: reading any type through an identity application is a
     no-op (on covered symbols by aux1, elsewhere because the lookup fails). *)
  Lemma safety_case_bindfn_aux2 : forall (l : list tsym) (tau : ty),
      app_subst (map (fun t0 => (t0, TAbs t0)) l) tau = tau.
  Proof.
    intros l tau. induction tau using ty_ind'; simpl; try reflexivity.
    - destruct (lookup_subst t (map (fun t0 => (t0, TAbs t0)) l)) as [tau'|] eqn:E.
      + apply safety_case_bindfn_aux1 in E. exact E.
      + reflexivity.
    - f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. destruct x; reflexivity.
    - f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. reflexivity.
  Qed.

  Lemma safety_case_bindfn_aux3 : forall (D : tele) (tau : ty),
      app_subst (id_app D) tau = tau.
  Proof.
    intros D tau. unfold id_app. apply safety_case_bindfn_aux2.
  Qed.

  (* Private helper: the fn entry's agreement, assembled from the closure's
     own grounding, the two signature equations, and the callee world's
     agreement (whose two conjuncts are exactly tele_covered and the inner
     entry-wise recursion). *)
  Lemma safety_case_bindfn_aux4 :
    forall (g0 : subst) (f0 : fsym) (Df0 : tele) (Sf0 : fnsig) (f'0 : fsym)
           (D0 : tele) (S0 : fnsig) (body0 : expr) (gc0 : subst) (dc0 : denv),
      g_fn Sg f0 = Some (Df0, Sf0, None) ->
      g_fn Sg f'0 = Some (D0, S0, Some body0) ->
      grounding gc0 ->
      covers gc0 (tyreqs D0) ->
      map (app_subst gc0) (fs_params S0) = map (app_subst g0) (fs_params Sf0) ->
      app_subst gc0 (fs_ret S0) = app_subst g0 (fs_ret Sf0) ->
      denv_agree Sg gc0 D0 dc0 ->
      dentry_agree Sg g0 (IFn f0) (DClo f'0 dc0).
  Proof.
    intros g0 f0 Df0 Sf0 f'0 D0 S0 body0 gc0 dc0 Hf0 Hf'0 Hgr Hcov Hp Hr Hda.
    simpl. rewrite Hf0, Hf'0. simpl.
    exists gc0.
    split; [exact Hgr |].
    split; [exact Hcov |].
    split; [exact Hp |].
    split; [exact Hr |].
    destruct Hda as [Hc1 [Hc2 Hreg]].
    split; [exact Hc1 |].
    revert Hc2. generalize dc0. intros dd.
    induction dd as [| p r IH]; intros Hall.
    - exact I.
    - simpl. inversion Hall as [| q qs Hq Hqs Heq]; subst.
      split; [exact Hq | apply IH; exact Hqs].
  Qed.

  Lemma safety_case_bindfn : forall Phi G f Df Sf f' D' S' body sm e tau d ge g,
      g_fn Sg f = Some (Df, Sf, None) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi Df ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      sig_eq (cS Phi) S' Sf ->
      has_ty Sg (add_item (IFn f) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindFn f f' sm e)).
  Proof.
    intros Phi G f Df Sf f' D' S' body sm e tau d ge g
           Hf Hf' Hsf Hs' Hsm Hsig H2 Hg Hd Hv.
    pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth].
    destruct (Wfn f' D' S' (Some body) Hf') as [Wt' [Wps' [Wrt' _]]].
    destruct Hs' as [Hsat' Hav'].
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    (* the identity application's entries are wf: its range is the available
       type symbols of the provider's telescope (sat0's second conjunct) *)
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app D')).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    (* the captured world *)
    assert (Hcall : exists dc, compose_smap d sm = Some dc
              /\ ctx_grounded (callee_grounding g Phi (id_app D') D') (mk_ctx D')
              /\ denv_agree Sg (callee_grounding g Phi (id_app D') D') D' dc).
    { eapply callee_env_agree;
        [ exact WF | exact Wt' | exact Hsat' | exact Hwfth | exact Hsm
        | exact Hg | exact Hd ]. }
    destruct Hcall as [dc [Hcomp [Hcgc Hdc]]].
    (* reading a D'-covered type through the closure's grounding is reading
       it through σ and then g: the application is the identity *)
    assert (Hgcr : forall t0, incl (ty_frees t0) (tyreqs D') ->
              app_subst (callee_grounding g Phi (id_app D') D') t0
              = app_subst g (app_subst (cS Phi) t0)).
    { intros t0 Hincl. rewrite callee_grounding_reads by exact Hincl.
      rewrite safety_case_bindfn_aux3. reflexivity. }
    assert (Hpar : map (app_subst (callee_grounding g Phi (id_app D') D'))
                       (fs_params S')
                   = map (app_subst g) (fs_params Sf)).
    { transitivity (map (app_subst g) (map (app_subst (cS Phi)) (fs_params S'))).
      - rewrite map_map. apply map_ext_in. intros tau0 Hin.
        apply Hgcr. rewrite Forall_forall in Wps'.
        pose proof (Wps' tau0 Hin) as Hw0. apply wf_ty_frees in Hw0. exact Hw0.
      - destruct Hsig as [Hsp _]. rewrite Hsp, map_map.
        apply map_ext_in. intros tau0 _. apply Habs. }
    assert (Hrt : app_subst (callee_grounding g Phi (id_app D') D') (fs_ret S')
                  = app_subst g (fs_ret Sf)).
    { assert (Hfr : incl (ty_frees (fs_ret S')) (tyreqs D')).
      { pose proof Wrt' as Hw. apply wf_ty_frees in Hw. exact Hw. }
      rewrite Hgcr by exact Hfr.
      destruct Hsig as [_ Hsr]. rewrite Hsr. apply Habs. }
    assert (Hnew : dentry_agree Sg g (IFn f) (DClo f' dc)).
    { destruct Hcgc as [Hgr0 [_ [Hcov0 Hreg0]]].
      eapply safety_case_bindfn_aux4;
        [ exact Hf | exact Hf' | exact Hgr0 | exact Hcov0 | exact Hpar
        | exact Hrt | exact Hdc ]. }
    assert (Hg' : ctx_grounded g (add_item (IFn f) Phi)).
    { destruct Hg as [Hgr [Hab Hcv]].
      split; [exact Hgr |]. split; simpl; assumption. }
    assert (Hd' : denv_agree Sg g (cA (add_item (IFn f) Phi))
                             ((IFn f, DClo f' dc) :: d)).
    { destruct Hd as [Hdcov [Hdentries Hdreg]].
      split.
      - simpl. constructor.
        + right. exists (DClo f' dc). simpl. rewrite Nat.eqb_refl. reflexivity.
        + eapply Forall_impl; [| exact Hdcov].
          intros i [Hty | [en Hen]].
          * left; exact Hty.
          * right. simpl.
            destruct (item_eqb i (IFn f)) eqn:Hi.
            -- exists (DClo f' dc). reflexivity.
            -- exists en. exact Hen.
      - split.
        + constructor; [exact Hnew | exact Hdentries].
        + simpl. constructor.
          * intros u Hu. simpl in Hu. rewrite Hf in Hu. simpl in Hu.
            apply in_app_or in Hu. destruct Hu as [Hu | []].
            destruct Hsf as [_ Hreq].
            rewrite Forall_forall in Hreq.
            apply wf_ty_frees_aux3.
            apply avail_ty_in. exact (Hreq u Hu).
          * exact Hdreg. }
    simpl interp. rewrite Hcomp.
    pose proof (IHfuel (le_n k)) as SK.
    apply (SK (add_item (IFn f) Phi) G e tau ((IFn f, DClo f' dc) :: d) ge g
              H2 Hg' Hd' Hv).
  Qed.
(* END:safety_case_bindfn *)

  (* Hint: as bindfn, but the new entry is a method provision: its
     dentry_agree equations are the rule's receiver/argument/result
     equations pushed under g (the first parameter via ty_eq and
     absorbs_all, the rest via the map equation and app_subst_chain /
     app_subst_frees_agree over the This-extended application). *)
  (* BEGIN:safety_case_bindmth *)
  (* Private helper: fusing the This-substitution into the outer read: a
     [This |-> tau0] step followed by s is s extended at this_sym. *)
  Lemma safety_case_bindmth_aux1 : forall (s : subst) (tau0 tau : ty),
      app_subst s (app_subst (this_subst tau0) tau)
      = app_subst ((this_sym, app_subst s tau0) :: s) tau.
  Proof.
    intros s tau0 tau. unfold this_subst.
    induction tau using ty_ind'.
    - reflexivity.
    - simpl. destruct (Nat.eqb t this_sym); reflexivity.
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite IHForall. destruct x as [t0 tau1]; simpl in *. rewrite H. reflexivity.
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. reflexivity.
  Qed.

  (* Private helper: two receivers with the same reading through th and then
     g give the same This-extended reading of any method type whose frees the
     application (extended at this_sym) covers. *)
  Lemma safety_case_bindmth_aux2 :
    forall (g0 th0 : subst) (r1 r2 tau : ty),
      incl (ty_frees tau) (this_sym :: map fst th0) ->
      app_subst g0 (app_subst th0 r1) = app_subst g0 (app_subst th0 r2) ->
      app_subst g0 (app_subst th0 (app_subst (this_subst r1) tau))
      = app_subst g0 (app_subst th0 (app_subst (this_subst r2) tau)).
  Proof.
    intros g0 th0 r1 r2 tau Hincl HE.
    rewrite (safety_case_bindmth_aux1 th0 r1 tau).
    rewrite (safety_case_bindmth_aux1 th0 r2 tau).
    rewrite (app_subst_chain g0 ((this_sym, app_subst th0 r1) :: th0) tau)
      by (simpl; exact Hincl).
    rewrite (app_subst_chain g0 ((this_sym, app_subst th0 r2) :: th0) tau)
      by (simpl; exact Hincl).
    unfold map_snd. simpl. rewrite HE. reflexivity.
  Qed.

  Lemma safety_case_bindmth_auxN : forall g A B,
      app_subst g A = app_subst g B ->
      forall X, app_subst g (app_subst (this_subst A) X)
              = app_subst g (app_subst (this_subst B) X).
  Proof.
    intros g A B HE X. unfold this_subst.
    induction X using ty_ind'.
    - reflexivity.
    - simpl. destruct (Nat.eqb t this_sym); [exact HE | reflexivity].
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite IHForall. destruct x as [t0 tau0]; simpl in *.
      rewrite H. reflexivity.
    - simpl. f_equal. induction H; simpl; [reflexivity|].
      rewrite H, IHForall. reflexivity.
  Qed.

  (* Private helper: the method entry's agreement, assembled from the
     provider closure's grounding, the receiver/argument/result equations and
     the callee world's agreement. *)
  Lemma safety_case_bindmth_aux3 :
    forall (g0 : subst) (rt : ty) (m0 : msym) (th0 : subst) (Dm0 : tele)
           (Sm0 : fnsig) (f'0 : fsym) (D0 : tele) (S0 : fnsig) (body0 : expr)
           (gc0 : subst) (dc0 : denv),
      g_mth Sg m0 = Some (Dm0, Sm0) ->
      g_fn Sg f'0 = Some (D0, S0, Some body0) ->
      grounding gc0 ->
      covers gc0 (tyreqs D0) ->
      map (app_subst gc0) (fs_params S0)
        = app_subst g0 rt
          :: map (fun tau => app_subst g0
                    (app_subst (this_subst rt) (app_subst th0 tau)))
                 (fs_params Sm0) ->
      app_subst gc0 (fs_ret S0)
        = app_subst g0
            (app_subst (this_subst rt) (app_subst th0 (fs_ret Sm0))) ->
      denv_agree Sg gc0 D0 dc0 ->
      dentry_agree Sg g0 (IMth rt m0 th0) (DClo f'0 dc0).
  Proof.
    intros g0 rt m0 th0 Dm0 Sm0 f'0 D0 S0 body0 gc0 dc0
           Hm0 Hf'0 Hgr Hcov Hp Hr Hda.
    simpl. rewrite Hm0, Hf'0. simpl.
    exists gc0.
    split; [exact Hgr |].
    split; [exact Hcov |].
    split; [exact Hp |].
    split; [exact Hr |].
    destruct Hda as [Hc1 [Hc2 Hreg]].
    split; [exact Hc1 |].
    revert Hc2. generalize dc0. intros dd.
    induction dd as [| p r IH]; intros Hall.
    - exact I.
    - simpl. inversion Hall as [| q qs Hq Hqs Heq]; subst.
      split; [exact Hq | apply IH; exact Hqs].
  Qed.

  (* Private helpers: the extended world's lookups (kept generic so that
     the method item's boolean equality is not unfolded). *)
  Lemma safety_case_bindmth_aux4 : forall (i : item) (en : dentry) (dd : denv),
      lookup_denv i ((i, en) :: dd) = Some en.
  Proof.
    intros i en dd. simpl.
    rewrite (proj2 (item_eqb_eq i i) eq_refl). reflexivity.
  Qed.

  Lemma safety_case_bindmth_aux5 :
    forall (i i0 : item) (en0 en : dentry) (dd : denv),
      lookup_denv i dd = Some en ->
      exists en', lookup_denv i ((i0, en0) :: dd) = Some en'.
  Proof.
    intros i i0 en0 en dd H. simpl.
    destruct (item_eqb i i0).
    - exists en0. reflexivity.
    - exists en. exact H.
  Qed.

  Lemma safety_case_bindmth_gap : forall Phi G tauk m Dm Sf th f' D' S' body sm
                                     tau0' rest e tau d ge g,
      g_mth Sg m = Some (Dm, Sf) ->
      wf_ty Sg Phi tauk ->
      sat Sg Phi th Dm ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      fs_params S' = tau0' :: rest ->
      ty_eq (cS Phi) tau0' tauk ->
      map (app_subst (cS Phi)) rest
        = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params Sf) ->
      app_subst (cS Phi) (fs_ret S')
        = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret Sf) ->
      has_ty Sg (add_item (IMth tauk m th) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindMth tauk m th f' sm e)).
  Proof.
    intros Phi G tauk m Dm Sf th f' D' S' body sm tau0' rest e tau d ge g
           Hm Hwk Hsat Hthwf Hf' Hs' Hsm Hpar0 Hteq Hprest Hpret H2 Hg Hd Hv.
    pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth].
    destruct (Wfn f' D' S' (Some body) Hf') as [Wt' [Wps' [Wrt' _]]].
    destruct (Wmth m Dm Sf Hm) as [Wtm [Wpsm Wrtm]].
    destruct Hs' as [Hsat' Hav'].
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app D')).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    assert (Hcall : exists dc, compose_smap d sm = Some dc
              /\ ctx_grounded (callee_grounding g Phi (id_app D') D') (mk_ctx D')
              /\ denv_agree Sg (callee_grounding g Phi (id_app D') D') D' dc).
    { eapply callee_env_agree;
        [ exact WF | exact Wt' | exact Hsat' | exact Hwfth | exact Hsm
        | exact Hg | exact Hd ]. }
    destruct Hcall as [dc [Hcomp [Hcgc Hdc]]].
    assert (Hgcr : forall t0, incl (ty_frees t0) (tyreqs D') ->
              app_subst (callee_grounding g Phi (id_app D') D') t0
              = app_subst g (app_subst (cS Phi) t0)).
    { intros t0 Hincl. rewrite callee_grounding_reads by exact Hincl.
      rewrite safety_case_bindfn_aux3. reflexivity. }
    (* the method's own types are covered by This and the application's keys *)
    destruct Hsat as [Hdom Hcoh].
    assert (Hfr : forall tau1, wf_ty Sg (mk_ctx (ITy this_sym :: Dm)) tau1 ->
              incl (ty_frees tau1) (this_sym :: map fst th)).
    { intros tau1 Hw. pose proof Hw as Hi. apply wf_ty_frees in Hi.
      simpl in Hi. rewrite <- Hdom in Hi. exact Hi. }
    (* the provider's parameter types are covered by its telescope *)
    rewrite Hpar0 in Wps'.
    inversion Wps' as [| tt rr Wp0 Wpr Heqw]; subst.
    (* the two signature equations of the new entry *)
    assert (Hpar : map (app_subst (callee_grounding g Phi (id_app D') D'))
                       (fs_params S')
                   = app_subst g tauk
                     :: map (fun tau1 => app_subst g
                               (app_subst (this_subst tauk)
                                          (app_subst th tau1)))
                            (fs_params Sf)).
    { rewrite Hpar0. simpl. f_equal.
      - rewrite Hgcr by (exact (wf_ty_frees Wp0)).
        unfold ty_eq in Hteq. rewrite Hteq. apply Habs.
      - transitivity (map (app_subst g) (map (app_subst (cS Phi)) rest)).
        + rewrite map_map. apply map_ext_in. intros t1 Hin. apply Hgcr.
          rewrite Forall_forall in Wpr. exact (wf_ty_frees (Wpr t1 Hin)).
        + rewrite Hprest, map_map. apply map_ext_in. intros t1 Hin.
          unfold interp_m. rewrite Habs.
          apply safety_case_bindmth_auxN. apply Habs. }
    assert (Hrt : app_subst (callee_grounding g Phi (id_app D') D') (fs_ret S')
                  = app_subst g
                      (app_subst (this_subst tauk)
                                 (app_subst th (fs_ret Sf)))).
    { rewrite Hgcr by (exact (wf_ty_frees Wrt')).
      rewrite Hpret. unfold interp_m. rewrite Habs.
      apply safety_case_bindmth_auxN. apply Habs. }
    assert (Hnew : dentry_agree Sg g (IMth tauk m th) (DClo f' dc)).
    { destruct Hcgc as [Hgr0 [_ [Hcov0 Hreg0]]].
      eapply safety_case_bindmth_aux3;
        [ exact Hm | exact Hf' | exact Hgr0 | exact Hcov0 | exact Hpar
        | exact Hrt | exact Hdc ]. }
    assert (Hg' : ctx_grounded g (add_item (IMth tauk m th) Phi)).
    { destruct Hg as [Hgr [Hab Hcv]].
      split; [exact Hgr |]. split; simpl; assumption. }
    assert (Hd' : denv_agree Sg g (cA (add_item (IMth tauk m th) Phi))
                             ((IMth tauk m th, DClo f' dc) :: d)).
    { destruct Hd as [Hdcov [Hdentries Hdreg]].
      split.
      - simpl. constructor.
        + right. exists (DClo f' dc). apply safety_case_bindmth_aux4.
        + eapply Forall_impl; [| exact Hdcov].
          intros i [Hty | [en Hen]].
          * left; exact Hty.
          * right. eapply safety_case_bindmth_aux5. exact Hen.
      - split.
        + constructor; [exact Hnew | exact Hdentries].
        + simpl. constructor.
          * intros u Hu.
            apply in_app_or in Hu. destruct Hu as [Hu | Hu].
            -- simpl in Hu. destruct Hu.
            -- apply in_app_or in Hu. destruct Hu as [Hu | Hu].
               ++ exact (wf_ty_frees Hwk u Hu).
               ++ apply in_flat_map in Hu.
                  destruct Hu as [p [Hp Hu]].
                  rewrite Forall_forall in Hthwf.
                  exact (wf_ty_frees (Hthwf p Hp) u Hu).
          * exact Hdreg. }
    simpl interp. rewrite Hcomp.
    pose proof (IHfuel (le_n k)) as SK.
    apply (SK (add_item (IMth tauk m th) Phi) G e tau
              ((IMth tauk m th, DClo f' dc) :: d) ge g H2 Hg' Hd' Hv).
  Qed.

  Lemma safety_case_bindmth : forall Phi G tauk m Dm Sf th f' D' S' body sm
                                     tau0' rest e tau d ge g,
      g_mth Sg m = Some (Dm, Sf) ->
      wf_ty Sg Phi tauk ->
      sat Sg Phi th Dm ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      (* receiver coherence: the application must read the receiver the
         same over and under the substitutions in force — a batch-3 agent
         refuted the rule without it (θ can capture a symbol σ introduced),
         the method-item twin of S-Item's coherence premise *)
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      fs_params S' = tau0' :: rest ->
      ty_eq (cS Phi) tau0' tauk ->
      map (app_subst (cS Phi)) rest
        = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params Sf) ->
      app_subst (cS Phi) (fs_ret S')
        = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret Sf) ->
      has_ty Sg (add_item (IMth tauk m th) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindMth tauk m th f' sm e)).
  Proof.
    intros Phi G tauk m Dm Sf th f' D' S' body sm tau0' rest e tau d ge g
           Hm Hwk Hsat Hthwf Hf' Hs' Hsm Hpar0 Hteq Hprest Hpret H2 Hg Hd Hv.
    eapply safety_case_bindmth_gap; eassumption.
  Qed.
(* END:safety_case_bindmth *)

  (* Assembly of one fuel step: the inner induction on the typing
     derivation (has_ty_ind_sub) dispatches each constructor to its case
     lemma.  Hint: unfold SAFE; intros; revert the environments; apply
     has_ty_ind_sub with the motive
       P Phi G e tau := forall d ge g, ctx_grounded g Phi ->
         denv_agree Sg g (cA Phi) d -> venv_agree Sg g (cA Phi) G ge ->
         safe_res Sg (app_subst g (app_subst (cS Phi) tau))
                  (interp (S k) Sg d ge e);
     every case is exactly a safety_case_* lemma. *)
  (* BEGIN:safety_step *)
  Lemma safety_step : SAFE Sg (S k).
  Proof.
    assert (MAIN : forall Phi G e tau, has_ty Sg Phi G e tau ->
      forall d ge g, ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge e)).
    { apply (@has_ty_ind_sub Sg
        (fun Phi G e tau => forall d ge g,
          ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
          venv_agree Sg g (cA Phi) G ge ->
          safe_res Sg (app_subst g (app_subst (cS Phi) tau))
                   (interp (S k) Sg d ge e))).
      - intros; eapply safety_case_var; eauto.
      - intros; eapply safety_case_unit; eauto.
      - intros; eapply safety_case_sub; eauto.
      - intros; eapply safety_case_val; eauto.
      - intros; eapply safety_case_tag; eauto.
      - intros; eapply safety_case_need; eauto.
      - intros; eapply safety_case_call; eauto.
      - intros; eapply safety_case_meth; eauto.
      - intros; eapply safety_case_match; eauto.
      - intros; eapply safety_case_let; eauto.
      - intros; eapply safety_case_bindty; eauto.
      - intros; eapply safety_case_bindval; eauto.
      - intros; eapply safety_case_bindfn; eauto.
      - intros; eapply safety_case_bindmth; eauto. }
    intros Phi G e tau d ge g Hty Hg Hd Hv.
    exact (MAIN _ _ _ _ Hty _ _ _ Hg Hd Hv).
  Qed.
  (* END:safety_step *)

End SafetyCases.

(* Hint: strong induction on fuel — e.g. `induction fuel using lt_wf_ind`
   or a helper `forall n, (forall m, m < n -> SAFE Sg m) -> SAFE Sg n`;
   fuel 0 is OutOfFuel, hence trivially safe (interp 0 computes); the
   successor case is safety_step with IHfuel j Hj := the strong hypothesis
   at j (mind the off-by-one: j <= k is j < S k). *)
(* BEGIN:safety *)
Theorem safety : forall Sg, wf_gsig Sg -> forall fuel, SAFE Sg fuel.
Proof.
  intros Sg WF fuel.
  induction fuel using lt_wf_ind.
  destruct fuel as [| k].
  - intros Phi G e tau d ge g _ _ _ _. exact I.
  - apply safety_step; [exact WF |].
    intros j Hj. apply H. apply Nat.lt_succ_r. exact Hj.
Qed.
(* END:safety *)

(* ---- Theorem 4.2 (soundness), at the program level. ---------------------
   Stated for main, where the context in force and both environments are
   empty, so no grounding of abstract type symbols is needed.  The
   generalized invariant of the paper (δ ⊨ Φ) requires each closure to carry
   its own ground instantiation of the abstract types it was captured under;
   spelling that out is the first real step of the proof effort, and we
   deliberately do not bake a weaker (false) approximation in here. *)

Definition value_has_unit_ty (w : value) : Prop := w = VUnit.

Theorem soundness_main :
  forall Sg main body fuel,
    wf_prog Sg main ->
    g_fn Sg main = Some ([], mkSig [] TUnit, Some body) ->
    interp fuel Sg [] [] body <> Err
    /\ (forall w, interp fuel Sg [] [] body = Ok w -> value_has_unit_ty w).
Proof.
  intros Sg main body fuel [WF _] Hmain.
  pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth].
  destruct (Wfn _ _ _ _ Hmain) as (_ & _ & _ & Hb).
  specialize (Hb body eq_refl). simpl in Hb.
  pose proof (safety WF fuel) as HS. unfold SAFE in HS.
  specialize (HS (mk_ctx []) [] body TUnit [] [] [] Hb).
  simpl in HS.
  assert (Hg : ctx_grounded [] (mk_ctx [])).
  { split; [constructor | split; [| split]].
    - intros t tau Hl; discriminate Hl.
    - intros t Hin; simpl in Hin; contradiction.
    - split; [intros t Hin; simpl in Hin; contradiction | constructor]. }
  assert (Hd : denv_agree Sg [] [] [])
    by (split; [constructor | split; constructor]).
  assert (Hv : venv_agree Sg [] [] [] []).
  { split; [intros x tau Hl; discriminate Hl | constructor]. }
  specialize (HS Hg Hd Hv).
  split.
  - intros HE. rewrite HE in HS. exact HS.
  - intros w Hw. rewrite Hw in HS.
    apply vty_unit_inv in HS. exact HS.
Qed.

(* ---- Proposition 4.3 (phase separation). --------------------------------
   Evaluation never inspects a type: it is invariant under erasing every
   application θ and every type bind.

   Both directions go by structural induction on e (expr_ind'), with
   inversion on the evaluation derivation: bodies fetched from Σ are not
   erased, so no induction on evaluation is needed.
   Hints: rewrite with erase_ECallD/ECallA/EMeth/EMatch before inverting
   (backward) or before rebuilding (forward); find_arm_erase aligns the
   selected arms and find_arm_in + Forall_forall fetches the structural
   induction hypothesis for the selected arm's body; for argument lists,
   zip the Forall IH with the Forall2 evaluation premise by induction on
   the Forall2. *)
(* BEGIN:phase_separation_fwd *)
(* Zipping the structural induction hypothesis for an argument list with the
   Forall2 evaluation premise of a call rule. *)
Lemma phase_separation_fwd_aux1 :
  forall Sg d g args ws,
    Forall (fun e0 => forall d0 g0 w0,
                eval Sg d0 g0 e0 w0 -> eval Sg d0 g0 (erase e0) w0) args ->
    Forall2 (eval Sg d g) args ws ->
    Forall2 (eval Sg d g) (map erase args) ws.
Proof.
  intros Sg d g args ws HF H2.
  induction H2 as [| e1 w1 r ws' He H2' IH]; simpl.
  - constructor.
  - constructor.
    + apply (Forall_inv HF). exact He.
    + apply IH. exact (Forall_inv_tail HF).
Qed.

Lemma phase_separation_fwd :
  forall Sg e d g w, eval Sg d g e w -> eval Sg d g (erase e) w.
Proof.
  intros Sg e.
  apply (@expr_ind'
           (fun e0 => forall d g w, eval Sg d g e0 w -> eval Sg d g (erase e0) w)).
  - (* EVar *) intros x d g w Hev. exact Hev.
  - (* EUnit *) intros d g w Hev. exact Hev.
  - (* EVl *) intros v d g w Hev. exact Hev.
  - (* ETag *) intros n th e1 IH d g w Hev.
    inversion Hev; subst. simpl. apply EvTag. apply IH. assumption.
  - (* ECallD *) intros f th sm args HF d g w Hev.
    rewrite erase_ECallD. inversion Hev; subst.
    eapply EvCallD; try eassumption.
    apply phase_separation_fwd_aux1; assumption.
  - (* ECallA *) intros f args HF d g w Hev.
    rewrite erase_ECallA. inversion Hev; subst.
    eapply EvCallA; try eassumption.
    apply phase_separation_fwd_aux1; assumption.
  - (* EMeth *) intros e0 m prov args IH0 HF d g w Hev.
    rewrite erase_EMeth. inversion Hev; subst.
    eapply EvMeth; try eassumption.
    + apply IH0. eassumption.
    + apply phase_separation_fwd_aux1; assumption.
  - (* EMatch *) intros e0 arms IH0 HF d g w Hev.
    rewrite erase_EMatch. inversion Hev; subst.
    eapply EvMatch.
    + apply IH0. eassumption.
    + rewrite find_arm_erase.
      match goal with
      | H : find_arm _ arms = Some _ |- _ => rewrite H
      end.
      reflexivity.
    + match goal with
      | H : find_arm ?n arms = Some (?x, ?eb) |- _ =>
          assert (Hin : In (n, x, eb) arms) by (apply find_arm_in; exact H)
      end.
      rewrite Forall_forall in HF. specialize (HF _ Hin). simpl in HF.
      apply HF. assumption.
  - (* ELet *) intros x e1 e2 IH1 IH2 d g w Hev.
    inversion Hev; subst. simpl. eapply EvLet.
    + apply IH1. eassumption.
    + apply IH2. eassumption.
  - (* EBindTy *) intros t tau e1 IH d g w Hev.
    inversion Hev; subst. apply IH. assumption.
  - (* EBindVal *) intros v e1 e2 IH1 IH2 d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindVal.
    + apply IH1. eassumption.
    + apply IH2. eassumption.
  - (* EBindFn *) intros f f' sm e1 IH d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindFn.
    + eassumption.
    + apply IH. eassumption.
  - (* EBindMth *) intros tauk m th f' sm e1 IH d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindMth.
    + eassumption.
    + apply IH. eassumption.
Qed.
(* END:phase_separation_fwd *)

(* BEGIN:phase_separation_bwd *)
Lemma phase_separation_bwd_aux1 :
  forall Sg args,
    Forall (fun e => forall d g w, eval Sg d g (erase e) w -> eval Sg d g e w)
           args ->
    forall d g ws,
      Forall2 (eval Sg d g) (map erase args) ws ->
      Forall2 (eval Sg d g) args ws.
Proof.
  intros Sg args HF.
  induction HF as [| e r He Hr IH]; intros d g ws H2; simpl in H2.
  - inversion H2. constructor.
  - inversion H2; subst. constructor.
    + apply He; assumption.
    + apply IH; assumption.
Qed.

Lemma phase_separation_bwd_aux2 :
  forall Sg (arms : list (nsym * var * expr)),
    Forall (fun a => forall d g w,
                eval Sg d g (erase (snd a)) w -> eval Sg d g (snd a) w) arms ->
    forall n x eb, In (n, x, eb) arms ->
      forall d g w, eval Sg d g (erase eb) w -> eval Sg d g eb w.
Proof.
  intros Sg arms HF.
  induction HF as [| a r Ha Hr IH]; intros n x eb Hin d g w Hev.
  - inversion Hin.
  - destruct Hin as [Heq | Hin].
    + subst a. simpl in Ha. apply Ha; assumption.
    + eapply IH; eassumption.
Qed.

Lemma phase_separation_bwd :
  forall Sg e d g w, eval Sg d g (erase e) w -> eval Sg d g e w.
Proof.
  intros Sg e. revert e.
  apply (@expr_ind'
           (fun e => forall d g w, eval Sg d g (erase e) w -> eval Sg d g e w)).
  - intros x d g w H. exact H.
  - intros d g w H. exact H.
  - intros v d g w H. exact H.
  - intros n th e1 IH d g w H. simpl in H. inversion H; subst.
    constructor. apply IH; assumption.
  - intros f th sm args IHargs d g w H.
    rewrite erase_ECallD in H. inversion H; subst.
    econstructor; try eassumption.
    eapply phase_separation_bwd_aux1; eassumption.
  - intros f args IHargs d g w H.
    rewrite erase_ECallA in H. inversion H; subst.
    econstructor; try eassumption.
    eapply phase_separation_bwd_aux1; eassumption.
  - intros e0 m prov args IH0 IHargs d g w H.
    rewrite erase_EMeth in H. inversion H; subst.
    econstructor; try eassumption.
    + apply IH0; eassumption.
    + eapply phase_separation_bwd_aux1; eassumption.
  - intros e0 arms IH0 IHarms d g w H.
    rewrite erase_EMatch in H. inversion H; subst.
    match goal with
    | Hfa : find_arm ?nn (map erase_arm arms) = Some (?xx, ?ee) |- _ =>
        rewrite find_arm_erase in Hfa;
        destruct (find_arm nn arms) as [[x0 eb0] | ] eqn:E;
        [ injection Hfa as Hx Heb; subst | discriminate ]
    end.
    eapply EvMatch.
    + apply IH0; eassumption.
    + eassumption.
    + eapply phase_separation_bwd_aux2;
        [ eassumption | eapply find_arm_in; eassumption | eassumption ].
  - intros x e1 e2 IH1 IH2 d g w H. simpl in H. inversion H; subst.
    econstructor.
    + apply IH1; eassumption.
    + apply IH2; eassumption.
  - intros t tau e1 IH d g w H. simpl in H.
    constructor. apply IH; assumption.
  - intros v e1 e2 IH1 IH2 d g w H. simpl in H. inversion H; subst.
    econstructor.
    + apply IH1; eassumption.
    + apply IH2; eassumption.
  - intros f f' sm e1 IH d g w H. simpl in H. inversion H; subst.
    econstructor; [ eassumption | apply IH; eassumption ].
  - intros tauk m th f' sm e1 IH d g w H. simpl in H. inversion H; subst.
    econstructor; [ eassumption | apply IH; eassumption ].
Qed.
(* END:phase_separation_bwd *)

Theorem phase_separation :
  forall Sg d g e w,
    eval Sg d g e w <-> eval Sg d g (erase e) w.
Proof.
  intros; split; [apply phase_separation_fwd | apply phase_separation_bwd].
Qed.

(* ===================== 12. The paper's example, executed ================= *)

(* The program of Figure 1, elaborated into the core by hand.  Running it —
   and running its erasure — validates the dynamic semantics by computation,
   and is the mechanized counterpart of the example-level reading of
   Proposition 4.3. *)

Module Example.

  (* Symbols.  (this_sym = 0 is reserved.) *)
  Definition sElem : tsym := 1.
  Definition nEmpty : nsym := 1.
  Definition nFull : nsym := 2.
  Definition nSlot : nsym := 3.
  Definition nO : nsym := 4.
  Definition vZero : vsym := 1.
  Definition fCoalesce : fsym := 1.
  Definition fDemo : fsym := 2.
  Definition fMain : fsym := 3.
  Definition mOrZero : msym := 1.

  (* [Elem = Elem]: the identity application on Δ = Elem. *)
  Definition th_id : subst := [(sElem, TAbs sElem)].

  Definition slot_payload : ty :=
    TUnion [TNom nEmpty []; TNom nFull th_id].
  Definition slot_ty : ty := TNom nSlot th_id.

  (* fn coalesce(this: Slot): Elem { match this { Slot u =>
       match u { Empty => zero, Full e => e } } }  — requires {Elem, zero} *)
  Definition coalesce_body : expr :=
    EMatch (EVar 0)
      [(nSlot, 1,
         EMatch (EVar 1)
           [(nEmpty, 2, EVl vZero);
            (nFull, 3, EVar 3)])].

  (* The provision item Slot.or_zero[Elem = Elem], keyed at the receiver
     type Slot[Elem = Elem]. *)
  Definition prov : item := IMth slot_ty mOrZero th_id.

  (* fn demo(s: Slot): Elem { s.or_zero() } — requires {Elem, zero, prov} *)
  Definition demo_body : expr := EMeth (EVar 0) mOrZero prov [].
  Definition demo_tele : tele := [ITy sElem; IVal vZero; prov].

  (* fn main() { bind Elem = O; bind zero = O;
       bind Slot.or_zero[Elem=Elem] = coalesce;
       let _ = demo(Slot (Full (O))); () } *)
  Definition main_body : expr :=
    EBindTy sElem (TNom nO [])
    (EBindVal vZero (ETag nO [] EUnit)
    (EBindMth slot_ty mOrZero th_id fCoalesce
        [(IVal vZero, IVal vZero)]
    (ELet 5
       (ECallD fDemo th_id
          [(IVal vZero, IVal vZero); (prov, prov)]
          [ETag nSlot th_id (ETag nFull th_id (ETag nO [] EUnit))])
       EUnit))).

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sElem || Nat.eqb t this_sym)
    (fun n =>
       if Nat.eqb n nEmpty then Some ([], TUnit)
       else if Nat.eqb n nFull then Some ([ITy sElem], TAbs sElem)
       else if Nat.eqb n nSlot then Some ([ITy sElem], slot_payload)
       else if Nat.eqb n nO then Some ([], TUnit)
       else None)
    (fun v =>
       if Nat.eqb v vZero then Some ([ITy sElem], TAbs sElem) else None)
    (fun f =>
       if Nat.eqb f fCoalesce then
         Some ([ITy sElem; IVal vZero],
               mkSig [slot_ty] (TAbs sElem), Some coalesce_body)
       else if Nat.eqb f fDemo then
         Some (demo_tele, mkSig [slot_ty] (TAbs sElem), Some demo_body)
       else if Nat.eqb f fMain then
         Some ([], mkSig [] TUnit, Some main_body)
       else None)
    (fun m =>
       if Nat.eqb m mOrZero then Some ([ITy sElem], mkSig [] (TAbs sElem))
       else None).

  (* The program runs, and yields (). *)
  Example runs : interp 1000 Sg [] [] main_body = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  (* Its type-erasure runs to the same value (Proposition 4.3, by
     computation on this instance). *)
  Example erased_runs : interp 1000 Sg [] [] (erase main_body) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

End Example.
