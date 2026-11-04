(* ------------------------------------------------------------------------ *)
(* CoreMoss.v — mechanization of the calculus in core-moss.tex               *)
(*                                                                           *)
(* This file defines the syntax, static semantics, and dynamic semantics of  *)
(* Core Moss, and states the metatheory of the paper's §4.  Everything is    *)
(* definitional except where marked: Proposition 4.1 is proved, the paper's  *)
(* Figure 1 example is encoded and *computed* to a value by the executable   *)
(* interpreter (twice — once erased — as evidence for Proposition 4.3).     *)
(* The metatheory is fully machine-checked: determinism, interpreter        *)
(* adequacy (both directions), phase separation, resolution decidability,   *)
(* and type soundness all report "Closed under the global context" — with   *)
(* abstract functors [D78] included (deviation 11; the polymorphic-provider *)
(* lemma is `safety_binds_apply`, and §12c runs an allocator example).      *)
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
(*    The surface rule is now [D72]: a type bind starts a new generation,    *)
(*    elaborated to a fresh symbol, so this premise holds by construction.   *)
(* 8. Satisfaction carries a coherence premise: a val or fn requirement      *)
(*    names no types, so nothing else stops f[T=Bool] from being satisfied   *)
(*    by an ambient v : T that was provided under bind T=Int.  Where a       *)
(*    non-type requirement is answered from the ambient context, the         *)
(*    application must agree with the context on the type symbols that       *)
(*    requirement's own telescope mentions.  (Method items already carry     *)
(*    their receiver and application, so they were immune — see (6).)  Also  *)
(*    found while designing the preservation invariant; now in the paper's   *)
(*    S-Item rule as well.                                                    *)
(* 9. Telescope merging [D43] is oriented and verified.  The paper merges    *)
(*    duplicate keys at telescope formation by symmetric unification; here   *)
(*    the merge function keeps the earlier occurrence as representative and  *)
(*    returns substitution entries mapping later symbols onto it, riding     *)
(*    the existing σ machinery unchanged.  The unifier is fuel-indexed (a    *)
(*    generous quadratic budget) and its candidate is *verified* by          *)
(*    formation — colliding items identified, σ regular and idempotent (the  *)
(*    latter subsumes the occurs check, a finding: D43 is silent on cyclic   *)
(*    merges) — so no property of the unifier enters the metatheory: a       *)
(*    defective or fuel-starved candidate can only reject a telescope,       *)
(*    never mis-merge one.  Upgrade to a congruence formulation only if      *)
(*    representative-invariance ever matters.  Method items are keyed by     *)
(*    the receiver's *type* (the [D18]/[D62] identity), not its head: head   *)
(*    keying would falsely clash Slot[Elem=Int].get with                     *)
(*    Slot[Elem=Char].get at formation.                                      *)
(* 10. Method dispatch uniqueness is judged modulo σ [D62]: T-Meth demands   *)
(*    that all provisions at the receiver's type have one *image* under the  *)
(*    substitution in force — provisions are keyed by the receiver's         *)
(*    (declaration, static bindings) identity compared through the merged    *)
(*    σ, so two spellings of one merged atom are one provision rather than   *)
(*    an ambiguity.  (The receiver-*type* keying itself is deviation 6,      *)
(*    already shared with the paper.)                                        *)
(* 11. Abstract functors [D78] are in the core (the paper's §3.4): an item  *)
(*    IFun, the binds EBindFun (to a functor with a body) and EBindFunA (to *)
(*    another abstract functor in force), and the application EApply.  A    *)
(*    functor applied by name is still elaborated away into its binds       *)
(*    [D55]; a functor body survives only as what an abstract functor is    *)
(*    bound to, as a run of binds without continuation (`bnd`, checked by   *)
(*    `binds_ty` against the functor's own arguments alone).  A binding's   *)
(*    runtime entry DFun is the functor plus the chain of re-keyings from   *)
(*    its arguments out to the abstract functor's, one link per copy.       *)
(* 12. An application's type outputs are fresh, as a type bind's are        *)
(*    (deviation 7): each type of the result must not be in force, so a    *)
(*    second application of one functor in the scope of the first needs     *)
(*    its result named apart by elaboration — the two generations [D72]    *)
(*    D78 asks for.  They are rigid (no σ entry), and the scope's type may  *)
(*    not mention them: the existential's unpack, with the escape premise.  *)
(* 13. A functor's result telescope is formed over its arguments without    *)
(*    merging (every result item is appended fresh): what a functor         *)
(*    provides is never an item it takes.  Results are compared as one     *)
(*    telescope (the elaborated, flattened leaf items D78 compares).        *)
(* 14. Rule A (D84, the unified entry).  An abstract functor has fixed      *)
(*    inputs g_fixed u and brackets g_poly u (P(φ)); D84 rule 1 makes X(φ)  *)
(*    exactly M(φ) ∖ P(φ) — `fun_mentions`, closed through relevance, less   *)
(*    the result's types — which `wf_d84` states (kept apart from wf_gsig,  *)
(*    whose X(φ) ⊆ M(φ) is all soundness needs).  IFun u is a context item   *)
(*    at X(φ) (dep_tys).  A body functor h declares captured inputs (any    *)
(*    item, vals by value) and pins (g_cap h; a pin is a manifest argument, *)
(*    Leroy 1994): `bind φ = h;` (EBindFun u h sm smc) answers them from the *)
(*    bind site and DFun becomes a closure; a captured input of φ must be   *)
(*    in X(φ) (cap_ok), so X is never widened per binding (D84 3d; D84.v,   *)
(*    `held_narrow_rejected`).  EApply carries θ: fixed inputs must be at   *)
(*    the binding's generation (fixed_ok) and the result's non-method items *)
(*    coherent (res_coherent).  θ keys the result entries, so erase keeps   *)
(*    it, as it keeps IMth keys.  Members of a family on demand (D84 rule 7)*)
(*    are an elaboration into EApply at θ read off the key (D84.v).         *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Export Audit.
Import ListNotations.

Set Implicit Arguments.

(* [A_symbols] moved to Audit.v *)

(* [B_ty] moved to Audit.v *)

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

(* [C_app_subst] moved to Audit.v *)

Lemma app_subst_nil : forall tau, app_subst [] tau = tau.
Proof.
  induction tau using ty_ind'; simpl; try reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. destruct x; reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. reflexivity.
Qed.

(* [D_ty_eqb] moved to Audit.v *)

(* The abstract type symbols a type mentions.  (Used by the soundness
   development below, and by the merging unifier's occurs check.) *)
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

(* The size of a type, counting nodes: the merging unifier's fuel budget is
   computed from it. *)
Fixpoint ty_size (tau : ty) : nat :=
  match tau with
  | TUnit => 1
  | TAbs _ => 1
  | TNom _ th =>
      S ((fix go (l : list (tsym * ty)) : nat :=
            match l with [] => 0 | p :: r => S (ty_size (snd p) + go r) end) th)
  | TUnion ms =>
      S ((fix go (l : list ty) : nat :=
            match l with [] => 0 | t' :: r => S (ty_size t' + go r) end) ms)
  end.

(* [E_items] moved to Audit.v *)

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

(* [F_is_ty_item] moved to Audit.v *)

Definition tyreqs (D : tele) : list tsym :=
  flat_map (fun i => match i with ITy t => [t] | _ => [] end) D.

(* The identity application: the paper's ∅ ⊨ Δ, materialized. *)
Definition id_app (D : tele) : subst :=
  map (fun t => (t, TAbs t)) (tyreqs D).

Definition add_item (i : item) (Phi : ctx) : ctx :=
  mkCtx (i :: cA Phi) (cS Phi).

Definition bind_ty (t : tsym) (tau : ty) (Phi : ctx) : ctx :=
  mkCtx (ITy t :: cA Phi) ((t, app_subst (cS Phi) tau) :: cS Phi).

(* ===================== 4b. Consistent merging [D43] ====================== *)

(* Telescope formation merges duplicate keys [D43]: a flattened telescope
   may mention one key twice, and formation unifies the colliding bindings,
   failing exactly when two distinct concrete heads would be identified.
   The design is symmetric — neither occurrence is primary — but the
   mechanization orients it (deviation 9): the earlier item is kept as the
   representative, and the unifier emits substitution entries mapping later
   symbols onto earlier material.  Those entries ride the existing σ
   machinery of `ctx` unchanged. *)

(* The key an item is stored under: the symbol itself; for a method
   requirement, the method symbol paired with the receiver's *type*, read
   through the substitution accumulated so far — the [D18] identity
   (declaration, static bindings), which is exactly the key [D62] repairs
   dispatch to use, and the core's residue of the bootstrap's "method keys
   canonicalize through the union-find" [D43].
   Keying on the receiver's *head* instead would be wrong in the same way
   head-keyed dispatch is: it would give Slot[Elem=Int].get and
   Slot[Elem=Char].get one key, so a telescope legitimately requiring the
   method at two instantiations would try to identify Int with Char and be
   rejected at formation.  With type keying those are distinct atoms, while
   D43's own example still merges — there the receiver (A) is shared and it
   is the method's own application that differs. *)
Definition same_key (s : subst) (i j : item) : bool :=
  match i, j with
  | ITy t, ITy u => Nat.eqb t u
  | IVal v, IVal w => Nat.eqb v w
  | IFn f, IFn h => Nat.eqb f h
  | IMth rt m _, IMth rt' m' _ =>
      Nat.eqb m m' && ty_eqb (app_subst s rt) (app_subst s rt')
  | IFun u, IFun u' => Nat.eqb u u'
  | _, _ => false
  end.

(* The occurs check.  D43 is silent on cyclic merges; without this, merging
   t with Full[Elem=t] would accept a requirement no instantiation can
   ground (a finding of this mechanization, recorded in the paper). *)
Definition occurs_b (t : tsym) (tau : ty) : bool :=
  existsb (Nat.eqb t) (ty_frees tau).

(* Extending an idempotent substitution: apply the new entry to the
   existing ranges, then append it.  With the occurs check this keeps every
   range fixed under the whole substitution, so the one-pass app_subst
   remains the fixpoint reading. *)
Definition extend_subst (acc : subst) (t : tsym) (tau : ty) : subst :=
  map_snd (app_subst [(t, tau)]) acc ++ [(t, tau)].

(* The oriented unifier, fuel-indexed (deviation 9).  Both sides are read
   through acc at every step; when two abstract symbols meet, the left
   (earlier) one becomes the representative.  Fuel bounds the total number
   of steps; merge_item computes a generous budget below, and formation
   *verifies* the unifier's output rather than trusting it, so fuel
   exhaustion (like any other defect of the candidate) can only reject a
   telescope, never mis-merge one. *)
Fixpoint unify_tys (fuel : nat) (acc : subst) (a b : ty) {struct fuel}
    : option subst :=
  match fuel with
  | O => None
  | S k =>
      match app_subst acc a, app_subst acc b with
      | TAbs t, TAbs u =>
          if Nat.eqb t u then Some acc
          else Some (extend_subst acc u (TAbs t))
      | TAbs t, b' =>
          if occurs_b t b' then None else Some (extend_subst acc t b')
      | a', TAbs u =>
          if occurs_b u a' then None else Some (extend_subst acc u a')
      | TUnit, TUnit => Some acc
      | TNom n th, TNom n' th' =>
          if Nat.eqb n n' then unify_rows k acc th th' else None
      | TUnion ms, TUnion ms' => unify_alls k acc ms ms'
      | _, _ => None
      end
  end
with unify_rows (fuel : nat) (acc : subst) (th th' : list (tsym * ty))
    {struct fuel} : option subst :=
  match fuel with
  | O => None
  | S k =>
      match th, th' with
      | [], [] => Some acc
      | p :: r, p' :: r' =>
          if Nat.eqb (fst p) (fst p')
          then match unify_tys k acc (snd p) (snd p') with
               | Some acc' => unify_rows k acc' r r'
               | None => None
               end
          else None
      | _, _ => None
      end
  end
with unify_alls (fuel : nat) (acc : subst) (ms ms' : list ty)
    {struct fuel} : option subst :=
  match fuel with
  | O => None
  | S k =>
      match ms, ms' with
      | [], [] => Some acc
      | a :: r, b :: r' =>
          match unify_tys k acc a b with
          | Some acc' => unify_alls k acc' r r'
          | None => None
          end
      | _, _ => None
      end
  end.

(* Unifying two same-key items: symbols must coincide; colliding method
   bindings — the receiver and the application — unify pointwise. *)
Definition item_size (i : item) : nat :=
  match i with
  | ITy _ | IVal _ | IFn _ | IFun _ => 1
  | IMth rt _ th =>
      S (ty_size rt +
         fold_right (fun p n => S (ty_size (snd p) + n)) 0 th)
  end.

Definition subst_size (s : subst) : nat :=
  fold_right (fun p n => S (ty_size (snd p) + n)) 0 s.

Definition merge_fuel (s : subst) (i j : item) : nat :=
  let n := item_size i + item_size j + subst_size s + 4 in n * n.

Definition unify_items (fuel : nat) (acc : subst) (i j : item)
    : option subst :=
  match i, j with
  | ITy t, ITy u => if Nat.eqb t u then Some acc else None
  | IVal v, IVal w => if Nat.eqb v w then Some acc else None
  | IFn f, IFn h => if Nat.eqb f h then Some acc else None
  | IFun u, IFun u' => if Nat.eqb u u' then Some acc else None
  | IMth rt m th, IMth rt' m' th' =>
      if Nat.eqb m m'
      then match unify_tys fuel acc rt rt' with
           | Some acc' => unify_rows fuel acc' th th'
           | None => None
           end
      else None
  | _, _ => None
  end.

(* Verification of a merge candidate.  Formation checks the unifier's
   output — the colliding items really are identified, the substitution
   only binds and only mentions the telescope's own type symbols, and its
   ranges are fixed under it (idempotence, which also subsumes the occurs
   check) — so no property of the unifier itself enters the metatheory: a
   defective candidate fails formation instead of weakening it. *)
Definition inclb (xs ys : list nat) : bool :=
  forallb (fun x => existsb (Nat.eqb x) ys) xs.

Definition sigma_regular_b (sg : subst) (A : list item) : bool :=
  inclb (map fst sg) (tyreqs A)
  && forallb (fun p => inclb (ty_frees (snd p)) (tyreqs A)) sg.

Definition idempotent_b (s : subst) : bool :=
  forallb (fun p => ty_eqb (app_subst s (snd p)) (snd p)) s.

Fixpoint find_same_key (s : subst) (A : list item) (i : item)
    : option item :=
  match A with
  | [] => None
  | j :: r => if same_key s j i then Some j else find_same_key s r i
  end.

(* One item arriving at a context under formation: dedup against an
   existing same-key item by unifying their images, or append.  On a merge
   the item list is unchanged — the earlier occurrence is the
   representative — and only σ grows. *)
Definition merge_item (Phi : ctx) (i : item) : option ctx :=
  match find_same_key (cS Phi) (cA Phi) i with
  | None => Some (mkCtx (cA Phi ++ [i]) (cS Phi))
  | Some j =>
      match unify_items (merge_fuel (cS Phi) j i) (cS Phi) j i with
      | Some s' =>
          if item_subst_eqb s' j i
             && sigma_regular_b s' (cA Phi)
             && idempotent_b s'
          then Some (mkCtx (cA Phi) s')
          else None
      | None => None
      end
  end.

Fixpoint merge_tele (Phi : ctx) (D : tele) : option ctx :=
  match D with
  | [] => Some Phi
  | i :: r =>
      match merge_item Phi i with
      | Some Phi' => merge_tele Phi' r
      | None => None
      end
  end.

(* The context a telescope denotes: its deduplicated items and its merge
   substitution.  On a telescope with distinct keys this is ⟨D; ∅⟩, the
   pre-merging reading. *)
Definition empty_ctx : ctx := mkCtx [] [].

Definition tele_ctx (D : tele) : option ctx := merge_tele empty_ctx D.

(* [G_syntax] moved to Audit.v *)

(* The type symbols a val/fn requirement's own telescope mentions — the
   dependencies whose instantiations coherence must pin down (deviation 8). *)
Definition dep_tys (Sg : gsig) (i : item) : list tsym :=
  match i with
  | IVal v => match g_val Sg v with Some (Dv, _) => tyreqs Dv | None => [] end
  | IFn f => match g_fn Sg f with Some (Df, _, _) => tyreqs Df | None => [] end
  (* Rule A: an abstract functor is at an instantiation of its fixed inputs *)
  | IFun u => g_fixed Sg u
  | _ => []
  end.

(* θ respects a telescope's merge equations: symbols the telescope merged
   must be instantiated consistently — "binding one binds them all" [D43],
   read at a use site.  (For the identity application this collapses to:
   the ambient σ itself validates the equations.) *)
Definition merge_respects (Phi : ctx) (th : subst) (sD : subst) : Prop :=
  Forall (fun p => app_subst (cS Phi) (app_subst th (TAbs (fst p)))
                   = app_subst (cS Phi) (app_subst th (snd p))) sD.

(* Satisfaction Φ ⊢ θ ⊨ Δ, in elaborated normal form, judged against the
   telescope's merged context ⟨Δ̂; σ_Δ⟩ (§4b): θ is defined exactly on the
   deduplicated type items (identity entries playing the paper's S-Pass;
   totality is per equivalence class); every other deduplicated
   requirement, read under θ, is available; θ respects the merge
   equations; and — coherence — where a non-type requirement is answered
   from the ambient context, θ must agree with the context on the type
   symbols that requirement depends on (deviation 8). *)
Definition sat_at (Sg : gsig) (Phi : ctx) (th : subst) (PhiD : ctx) : Prop :=
  map fst th = tyreqs (cA PhiD) /\
  Forall (fun i =>
            is_ty_item i = true \/
            (avail Phi (app_subst_item th i) /\
             Forall (fun t =>
                       app_subst (cS Phi) (app_subst th (TAbs t))
                       = app_subst (cS Phi) (TAbs t))
                    (dep_tys Sg i))) (cA PhiD) /\
  merge_respects Phi th (cS PhiD).

Definition sat (Sg : gsig) (Phi : ctx) (th : subst) (D : tele) : Prop :=
  exists PhiD, tele_ctx D = Some PhiD /\ sat_at Sg Phi th PhiD.

Definition sat0 (Sg : gsig) (Phi : ctx) (D : tele) : Prop :=
  exists PhiD, tele_ctx D = Some PhiD
  /\ sat_at Sg Phi (id_app (cA PhiD)) PhiD
  /\ Forall (fun t => avail Phi (ITy t)) (tyreqs (cA PhiD)).
(* The last conjunct is the paper's S-Pass availability, which the
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

(* ς's domain is the deduplicated telescope's non-type items: elaborated
   bodies reference the representatives, so those are the keys the callee's
   runtime context is built under. *)
Definition smap_ok (Phi : ctx) (th : subst) (sm : smap) (D : tele) : Prop :=
  exists PhiD, tele_ctx D = Some PhiD /\
  map fst sm = filter (fun i => negb (is_ty_item i)) (cA PhiD) /\
  Forall (fun p => In (snd p) (cA Phi)
                   /\ item_matches (cS Phi) th (fst p) (snd p)) sm.

(* Abstract functors [D78].  A binding `bind φ = g;` is checked once,
   against signatures alone: g's arguments must be met by φ's — every item
   of args(g) available, at the identity application, in φ's merged argument
   context, with ς recording which item of args(φ) answers each — and g's
   result must be φ's.  Nothing is inferred, only checked [D27]. *)
Definition fun_link (Sg : gsig) (Din : tele) (sm : smap) (Dout : tele) : Prop :=
  exists PhiOut, tele_ctx Dout = Some PhiOut
  /\ sat0 Sg PhiOut Din /\ smap_ok PhiOut (id_app Din) sm Din.

(* A runtime binding of φ is a functor h with a body plus a chain of such
   links, from args(h) out to args(φ): one link per `bind ψ = ...;` it was
   copied through.  (The chain is the dynamic residue of `bind φ = ψ;`:
   copying a binding composes the re-keying and keeps what it captured.) *)
Inductive chain_ok (Sg : gsig) : usym -> list smap -> usym -> Prop :=
| ChNil : forall u, chain_ok Sg u [] u
| ChCons : forall h sm r w u Dh Drh bh Dw Drw bw,
    g_fun Sg h = Some (Dh, Drh, bh) ->
    g_fun Sg w = Some (Dw, Drw, bw) ->
    fun_link Sg Dh sm Dw ->
    chain_ok Sg w r u ->
    chain_ok Sg h (sm :: r) u.

(* The context an application of φ leaves in force for its scope: φ's
   result, keyed as declared, over what was in force.  Its type items stay
   *rigid* (no σ entry): each is the fresh generation [D72] the existential
   unpack introduces, which the scope knows nothing about. *)
Definition open_res (Dr : tele) (Phi : ctx) : ctx :=
  mkCtx (Dr ++ cA Phi) (cS Phi).

(* ---- Rule A (draft D84): fixed inputs, capture at the bind. ---------------- *)

(* A telescope read at an application θ: method items carry it in their
   receiver and application; the other items are symbols. *)
Definition app_tele (th : subst) (D : tele) : tele := map (app_subst_item th) D.

(* The fixed-input premise of an application: each fixed input is at the
   generation the item in force carries, i.e. θ is the identity on it modulo
   the context's σ. *)
Definition fixed_ok (Sg : gsig) (Phi : ctx) (u : usym) (th : subst) : Prop :=
  Forall (fun x => app_subst (cS Phi) (app_subst th (TAbs x))
                   = app_subst (cS Phi) (TAbs x)) (g_fixed Sg u).

(* Result coherence (deviation 8, applied to an application's result): a val,
   fn or functor item of the result names no instantiation, so θ must agree
   with the context on the symbols it depends on.  Vacuous at the identity.
   Not in the draft; machine-found (see `res_coherence_needed` below). *)
Definition res_coherent (Sg : gsig) (Phi : ctx) (th : subst) (Dr : tele) : Prop :=
  Forall (fun i => Forall (fun x => app_subst (cS Phi) (app_subst th (TAbs x))
                                    = app_subst (cS Phi) (TAbs x))
                          (dep_tys Sg i)) Dr.


(* Pins are equations on captured types only. *)
Definition pins_ok (pins : subst) (PhiC : ctx) : Prop :=
  incl (map fst pins) (tyreqs (cA PhiC))
  /\ Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA PhiC))) pins.

(* The context a functor body is checked in: its linked arguments, its
   captured inputs, their merge equations and its pins. *)
Definition fun_ctx (PhiD PhiC : ctx) (pins : subst) : ctx :=
  mkCtx (cA PhiD ++ cA PhiC) (cS PhiD ++ pins ++ cS PhiC).

(* Capture at `bind φ = h;`: h's captured inputs are available here and are
   answered from here (ς_c), its pins hold here, and a captured symbol that
   is also an input (or output) of φ must be one of φ's fixed inputs — a
   polymorphic input can't be pinned or captured. *)
Definition cap_ok (Sg : gsig) (Phi : ctx) (Du Dr : tele) (u h : usym)
    (smc : smap) : Prop :=
  sat0 Sg Phi (fst (g_cap Sg h))
  /\ smap_ok Phi (id_app (fst (g_cap Sg h))) smc (fst (g_cap Sg h))
  /\ Forall (fun p => app_subst (cS Phi) (TAbs (fst p))
                      = app_subst (cS Phi) (snd p)) (snd (g_cap Sg h))
  /\ (forall x, In x (tyreqs (fst (g_cap Sg h))) ->
                In x (tyreqs Du ++ tyreqs Dr) -> In x (g_fixed Sg u)).

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
    (* dispatch demands exactly one hit [D1], judged modulo σ: provisions
       are keyed by the receiver's (declaration, static bindings) identity
       compared through the merged substitution [D62] — two spellings of
       one merged atom are one provision, not an ambiguity *)
    (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                app_subst_item (cS Phi) i1
                = app_subst_item (cS Phi) (IMth rt0 m th0)) ->
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
    (* This is reserved: binding the receiver symbol itself would let a
       program capture the receiver substitution (machine-refuted end to
       end: a well-typed main evaluating to Err) *)
    t <> this_sym ->
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
    has_ty Sg Phi G (EBindMth tauk m th f' sm e) tau
(* bind φ = h;  h a functor with a body, checked against φ's signature
   alone: args(h) ⊆ args(φ) (the link), res(h) = res(φ) (one telescope).
   Like a fn bind it is static; unlike one it needs nothing in force — a
   binding serves every instantiation of φ's inputs [D78]. *)
| TBindFunR : forall Phi G u Du Dr h Dh bs sm smc e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg h = Some (Dh, Dr, Some bs) ->
    fun_link Sg Dh sm Du ->
    (* Rule A: the captured inputs, the pins, and X(φ) in force here *)
    cap_ok Sg Phi Du Dr u h smc ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    has_ty Sg (add_item (IFun u) Phi) G e tau ->
    has_ty Sg Phi G (EBindFun u h sm smc e) tau
(* bind φ = ψ;  ψ an abstract functor in force: φ is bound to whatever ψ
   is bound to here. *)
| TBindFunAR : forall Phi G u Du Dr w Dw sm e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg w = Some (Dw, Dr, None) ->
    avail Phi (IFun w) ->
    fun_link Sg Dw sm Du ->
    (* Rule A: what ψ's binding holds fixed, φ's applications must too *)
    incl (g_fixed Sg w) (g_fixed Sg u) ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    has_ty Sg (add_item (IFun u) Phi) G e tau ->
    has_ty Sg Phi G (EBindFunA u w sm e) tau
(* bind φ;  the application: φ in force, its arguments available at the
   identity application (they are the inputs as they are here), and the
   scope checked under φ's result.  Each type in the result is a fresh,
   rigid symbol — the existential's unpack — so it must not already be in
   force (as TBindTyR's freshness, deviation 7), and the scope's type must
   not mention it: no value of such a type leaves the block [D78]. *)
| TApplyR : forall Phi G u Du Dr th sm e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    avail Phi (IFun u) ->
    (* Rule A: the inputs read at θ ... *)
    sat Sg Phi th Du ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Du ->
    (* ... the fixed ones at the generation of the binding in force ... *)
    fixed_ok Sg Phi u th ->
    (* ... and the result's non-method items coherent at θ *)
    res_coherent Sg Phi th Dr ->
    Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
    has_ty Sg (open_res (app_tele th Dr) Phi) G e tau ->
    Forall (fun t => ~ In t (ty_frees tau)) (tyreqs Dr) ->
    has_ty Sg Phi G (EApply u th sm e) tau.

(* ===================== 9. Telescope, declaration, program formation ====== *)

Definition mk_ctx (D : tele) : ctx := mkCtx D [].

Inductive item_ok (Sg : gsig) (Phi : ctx) : item -> Prop :=
| OkTy : forall t, g_ty Sg t = true -> t <> this_sym ->
    item_ok Sg Phi (ITy t)
| OkVal : forall v Dv tauv,
    g_val Sg v = Some (Dv, tauv) -> sat0 Sg Phi Dv -> item_ok Sg Phi (IVal v)
| OkFn : forall f Df S b,
    g_fn Sg f = Some (Df, S, b) -> sat0 Sg Phi Df -> item_ok Sg Phi (IFn f)
| OkMth : forall rt m Dm S th,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi rt ->
    sat Sg Phi th Dm ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    item_ok Sg Phi (IMth rt m th)
(* An abstract functor is assumed like an abstract fn [D78]; it is at no
   instantiation, so nothing about its inputs need be in force. *)
| OkFun : forall u Du Dr,
    g_fun Sg u = Some (Du, Dr, None) ->
    (* Rule A: listing φ requires X(φ) in force [D20] *)
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    item_ok Sg Phi (IFun u).

(* The telescoping rule [D20], now with merging [D43]: each item is checked
   with only the items to its left in force (under the substitution merged
   so far), and then folded in — appended if its key is fresh, unified with
   the earlier occurrence otherwise.  The judgment relates a starting
   context to the context the whole telescope leaves in force. *)
Inductive wf_tele (Sg : gsig) : ctx -> tele -> ctx -> Prop :=
| WNil : forall Phi, wf_tele Sg Phi [] Phi
| WCons : forall Phi i D Phi' Phi'',
    item_ok Sg Phi i ->
    merge_item Phi i = Some Phi' ->
    wf_tele Sg Phi' D Phi'' ->
    wf_tele Sg Phi (i :: D) Phi''.

(* Formation computes exactly the merged context of §4b. *)
Lemma wf_tele_merge : forall Sg Phi D Phi',
  wf_tele Sg Phi D Phi' -> merge_tele Phi D = Some Phi'.
Proof.
  intros Sg Phi D Phi' H.
  induction H as [Phi | Phi i D Phi' Phi'' _ Hm _ IH]; simpl.
  - reflexivity.
  - rewrite Hm. exact IH.
Qed.

Lemma wf_tele_ctx : forall Sg D PhiD,
  wf_tele Sg empty_ctx D PhiD -> tele_ctx D = Some PhiD.
Proof. intros Sg D PhiD H. exact (wf_tele_merge H). Qed.

(* A functor body's binds, checked in order, each with the earlier ones in
   force: the premises are exactly those of the expression-level rules
   (TBindTyR, TBindValR — under no variables —, TBindFnR, TBindMthR,
   TBindFunR, TBindFunAR, TApplyR), relating the context the body starts
   in to the one it leaves in force. *)
Inductive binds_ty (Sg : gsig) : ctx -> list bnd -> ctx -> Prop :=
| BtNil : forall Phi, binds_ty Sg Phi [] Phi
| BtTy : forall Phi t tau' r Phi',
    g_ty Sg t = true ->
    t <> this_sym ->
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    binds_ty Sg (bind_ty t tau' Phi) r Phi' ->
    binds_ty Sg Phi (BTy t tau' :: r) Phi'
| BtVal : forall Phi v Dv tauv e1 r Phi',
    g_val Sg v = Some (Dv, tauv) ->
    sat0 Sg Phi Dv ->
    has_ty Sg Phi [] e1 (app_subst (cS Phi) tauv) ->
    binds_ty Sg (add_item (IVal v) Phi) r Phi' ->
    binds_ty Sg Phi (BVal v e1 :: r) Phi'
| BtFn : forall Phi f Df S f' D' S' body sm r Phi',
    g_fn Sg f = Some (Df, S, None) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Sg Phi Df ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    sig_eq (cS Phi) S' S ->
    binds_ty Sg (add_item (IFn f) Phi) r Phi' ->
    binds_ty Sg Phi (BFn f f' sm :: r) Phi'
| BtMth : forall Phi tauk m Dm S th f' D' S' body sm tau0' rest r Phi',
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
    binds_ty Sg (add_item (IMth tauk m th) Phi) r Phi' ->
    binds_ty Sg Phi (BMth tauk m th f' sm :: r) Phi'
| BtFun : forall Phi u Du Dr h Dh bs sm smc r Phi',
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg h = Some (Dh, Dr, Some bs) ->
    fun_link Sg Dh sm Du ->
    cap_ok Sg Phi Du Dr u h smc ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    binds_ty Sg (add_item (IFun u) Phi) r Phi' ->
    binds_ty Sg Phi (BFun u h sm smc :: r) Phi'
| BtFunA : forall Phi u Du Dr w Dw sm r Phi',
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg w = Some (Dw, Dr, None) ->
    avail Phi (IFun w) ->
    fun_link Sg Dw sm Du ->
    incl (g_fixed Sg w) (g_fixed Sg u) ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    binds_ty Sg (add_item (IFun u) Phi) r Phi' ->
    binds_ty Sg Phi (BFunA u w sm :: r) Phi'
| BtApply : forall Phi u Du Dr th sm r Phi',
    g_fun Sg u = Some (Du, Dr, None) ->
    avail Phi (IFun u) ->
    sat Sg Phi th Du ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Du ->
    fixed_ok Sg Phi u th ->
    res_coherent Sg Phi th Dr ->
    Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
    binds_ty Sg (open_res (app_tele th Dr) Phi) r Phi' ->
    binds_ty Sg Phi (BApply u th sm :: r) Phi'.

(* A defined body's parameters become variables 0, 1, ... *)
Definition body_env (S : fnsig) : tenv :=
  combine (seq 0 (length (fs_params S))) (fs_params S).

(* The type material an item itself carries. *)
Definition item_ty_frees (i : item) : list tsym :=
  match i with
  | IMth rt _ th => ty_frees rt ++ flat_map (fun p => ty_frees (snd p)) th
  | _ => []
  end.

Definition method_item_frees (Sg : gsig) (i : item) : list tsym :=
  match i with
  | IMth rt m th =>
      match g_mth Sg m with
      | Some (_, Sf) =>
          flat_map ty_frees
            (map (fun tau =>
                    app_subst (this_subst rt) (app_subst th tau))
                 (fs_params Sf ++ [fs_ret Sf]))
      | None => []
      end
  | _ => []
  end.

(* What a telescope's items read: the type symbols they depend on or carry. *)
Definition res_reads (Sg : gsig) (D : tele) : list tsym :=
  flat_map (fun i => dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) D.

(* D84 rule 1: M(F), the type symbols the items of a result mention — a val's
   or fn's dependencies (its telescope: the signature closed through
   relevance), a method's receiver, its brackets' right-hand sides and its
   signature read at them (so less the bracketed symbols), an abstract
   functor item's X — less the result's own type outputs. *)
Definition fun_mentions (Sg : gsig) (Dr : tele) : list tsym :=
  filter (fun x => negb (existsb (Nat.eqb x) (tyreqs Dr))) (res_reads Sg Dr).

(* Declarations are checked under their telescope's *merged* context: the
   deduplicated items with the merge substitution in force. *)
Record wf_gsig (Sg : gsig) : Prop := {
  wf_tag_decl : forall n D taup,
      g_tag Sg n = Some (D, taup) ->
      exists PhiD, wf_tele Sg empty_ctx D PhiD /\ wf_ty Sg PhiD taup;
  wf_val_decl : forall v D tauv,
      g_val Sg v = Some (D, tauv) ->
      exists PhiD, wf_tele Sg empty_ctx D PhiD /\ wf_ty Sg PhiD tauv;
  wf_fn_decl : forall f D S b,
      g_fn Sg f = Some (D, S, b) ->
      exists PhiD, wf_tele Sg empty_ctx D PhiD
      /\ Forall (wf_ty Sg PhiD) (fs_params S)
      /\ wf_ty Sg PhiD (fs_ret S)
      /\ (forall body, b = Some body ->
             has_ty Sg PhiD (body_env S) body (fs_ret S));
  wf_mth_decl : forall m D S,
      g_mth Sg m = Some (D, S) ->
      exists PhiD, wf_tele Sg empty_ctx D PhiD
      /\ Forall (wf_ty Sg (add_item (ITy this_sym) PhiD)) (fs_params S)
      /\ wf_ty Sg (add_item (ITy this_sym) PhiD) (fs_ret S);
  (* A functor [D55]/[D78]: its result is formed over its arguments and
     adds every item fresh (no merge with an argument: what a functor
     provides is not something it takes); a body is checked once, against
     its own arguments alone, and must leave every item of the result in
     force — types included, which it binds. *)
  wf_fun_decl : forall u Du Dr ob,
      g_fun Sg u = Some (Du, Dr, ob) ->
      exists PhiD, wf_tele Sg empty_ctx Du PhiD
      /\ (ob = None ->
            (* an abstract functor captures nothing; X(φ) are inputs *)
            g_cap Sg u = ([], [])
            /\ incl (g_fixed Sg u) (tyreqs (cA PhiD))
            (* corrected Rule A: X(φ) ⊆ M(φ), what the result reads *)
            /\ incl (g_fixed Sg u)
                    (flat_map (fun i => dep_tys Sg i ++
                       match i with
                       | IMth rt _ th => ty_frees rt ++ flat_map (fun p => ty_frees (snd p)) th
                       | _ => [] end) Dr)

            /\ wf_tele Sg PhiD Dr (mkCtx (cA PhiD ++ Dr) (cS PhiD)))
      /\ (forall bs, ob = Some bs ->
             (* Rule A: a body is checked once, under its linked arguments,
                its captured inputs and its pins *)
             exists PhiC, wf_tele Sg empty_ctx (fst (g_cap Sg u)) PhiC
             /\ pins_ok (snd (g_cap Sg u)) PhiC

             /\ wf_tele Sg (fun_ctx PhiD PhiC (snd (g_cap Sg u))) Dr
                  (mkCtx (cA (fun_ctx PhiD PhiC (snd (g_cap Sg u))) ++ Dr)
                         (cS (fun_ctx PhiD PhiC (snd (g_cap Sg u)))))
             /\ exists PhiB, binds_ty Sg (fun_ctx PhiD PhiC (snd (g_cap Sg u))) bs PhiB
                /\ Forall (fun i => In i (cA PhiB)) Dr)
}.

(* ⊢ P ok: a well-formed signature whose main is defined, closed, unit-valued. *)
Definition wf_prog (Sg : gsig) (main : fsym) : Prop :=
  wf_gsig Sg /\
  exists body, g_fn Sg main = Some ([], mkSig [] TUnit, Some body).

(* ---- D84's declaration-level conditions. --------------------------------
   Kept apart from wf_gsig: the soundness development needs only X(φ) ⊆ M(φ)
   (wf_fun_decl), so `safety` and every theorem over wf_gsig keep their
   exact strength; these are what D84 rules 1 and 7 add on top, and what the
   on-demand elaboration (§11, rule 7) relies on. *)

(* D84 rule 7: a family is an abstract functor whose result is method items
   only (so holds no type), each of whose items' key — its receiver and
   brackets, the type material the item itself carries — mentions every
   bracketed symbol the item mentions at all. *)
Definition family (Sg : gsig) (u : usym) : Prop :=
  exists Du Dr, g_fun Sg u = Some (Du, Dr, None)
  /\ Forall (fun i => exists rt m th, i = IMth rt m th) Dr
  /\ Forall (fun i => forall x, In x (g_poly Sg u) -> In x (res_reads Sg [i]) ->
                               In x (item_ty_frees i)) Dr.

Definition binds_no_val (bs : list bnd) : Prop :=
  Forall (fun b => match b with BVal _ _ => False | _ => True end) bs.

Record wf_d84 (Sg : gsig) : Prop := {
  (* rule 1, exactly: X(F) = M(F) ∖ P(F), P(F) among F's inputs (the
     elaborator adds an unlisted bracket as an implicit input) *)
  d84_fixed : forall u Du Dr,
      g_fun Sg u = Some (Du, Dr, None) ->
      incl (g_poly Sg u) (tyreqs Du)
      /\ (forall x, In x (g_fixed Sg u)
                    <-> In x (fun_mentions Sg Dr) /\ ~ In x (g_poly Sg u));
  (* brackets are on an abstract functor only *)
  d84_body_unbracketed : forall u Du Dr bs,
      g_fun Sg u = Some (Du, Dr, Some bs) ->
      g_poly Sg u = [] /\ g_fixed Sg u = [];
  (* rule 7: a family's bindings bind no vals — every functor with a body
     that could be bound to a family (it makes the family's result) *)
  d84_family_binding : forall u Du Dr h Dh bs,
      family Sg u -> g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg h = Some (Dh, Dr, Some bs) -> binds_no_val bs
}.

(* rule 7: at most one family in a context produces a given method key (a
   collision is an error at the context's declaration, [D44]): the key names
   the family. *)
Definition families_disjoint (Sg : gsig) (A : list item) : Prop :=
  forall u1 u2 Du1 Dr1 Du2 Dr2 i1 i2 th1 th2 rt m,
    In (IFun u1) A -> In (IFun u2) A -> family Sg u1 -> family Sg u2 ->
    g_fun Sg u1 = Some (Du1, Dr1, None) -> g_fun Sg u2 = Some (Du2, Dr2, None) ->
    In i1 Dr1 -> In i2 Dr2 ->
    is_prov_at [] rt m (app_subst_item th1 i1) ->
    is_prov_at [] rt m (app_subst_item th2 i2) -> u1 = u2.

(* [H_dynamics] moved to Audit.v *)

(* Type erasure (Proposition 4.3): drop type binds and applications.  Items
   are runtime *keys*, not consulted types, so the applications inside IMth
   items and EBindMth stay (and, under Rule A, an application's θ, which
   keys its result): erasing a name would change which drawer the
   context is filed in, not what a type means.  Functor binds and
   applications [D78] stay too: they provide code and vals, and a functor
   body, like a fn body, lives in Σ and is not erased. *)
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
  | EBindFun u h sm smc e1 => EBindFun u h sm smc (erase e1)
  | EBindFunA u u' sm e1 => EBindFunA u u' sm (erase e1)
  | EApply u th sm e1 => EApply u th sm (erase e1)
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
    (HBindFun : forall u h sm smc e, P e -> P (EBindFun u h sm smc e))
    (HBindFunA : forall u u' sm e, P e -> P (EBindFunA u u' sm e))
    (HApply : forall u th sm e, P e -> P (EApply u th sm e))
    (e : expr) {struct e} : P e :=
  let REC := expr_ind' HVar HUnit HVl HTag HCallD HCallA HMeth HMatch HLet
                       HBindTy HBindVal HBindFn HBindMth
                       HBindFun HBindFunA HApply in
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
  | EBindFun u h sm smc e1 => HBindFun u h sm smc e1 (REC e1)
  | EBindFunA u u' sm e1 => HBindFunA u u' sm e1 (REC e1)
  | EApply u th sm e1 => HApply u th sm e1 (REC e1)
  end.

Section EvalInd.
  Variable Sg : gsig.
  Variable P : denv -> venv -> expr -> value -> Prop.
  Variable Pb : denv -> list bnd -> denv -> Prop.
  Variable Pa : denv -> usym -> subst -> smap -> denv -> Prop.

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
  Hypothesis HBindFun : forall d g u h sm smc dc e w,
      compose_smap d smc = Some dc ->
      eval Sg ((IFun u, DFun h [sm] dc) :: d) g e w ->
      P ((IFun u, DFun h [sm] dc) :: d) g e w ->
      P d g (EBindFun u h sm smc e) w.
  Hypothesis HBindFunA : forall d g u u' sm h sms dc e w,
      lookup_denv (IFun u') d = Some (DFun h sms dc) ->
      eval Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) g e w ->
      P ((IFun u, DFun h (sms ++ [sm]) dc) :: d) g e w ->
      P d g (EBindFunA u u' sm e) w.
  Hypothesis HApply : forall d g u th sm dr e w,
      run_apply Sg d u th sm dr -> Pa d u th sm dr ->
      eval Sg (dr ++ d) g e w -> P (dr ++ d) g e w ->
      P d g (EApply u th sm e) w.
  Hypothesis HRbNil : forall d, Pb d [] d.
  Hypothesis HRbTy : forall d t tau r d',
      run_binds Sg d r d' -> Pb d r d' -> Pb d (BTy t tau :: r) d'.
  Hypothesis HRbVal : forall d v e1 w1 r d',
      eval Sg d [] e1 w1 -> P d [] e1 w1 ->
      run_binds Sg ((IVal v, DVal w1) :: d) r d' ->
      Pb ((IVal v, DVal w1) :: d) r d' ->
      Pb d (BVal v e1 :: r) d'.
  Hypothesis HRbFn : forall d f f' sm dc r d',
      compose_smap d sm = Some dc ->
      run_binds Sg ((IFn f, DClo f' dc) :: d) r d' ->
      Pb ((IFn f, DClo f' dc) :: d) r d' ->
      Pb d (BFn f f' sm :: r) d'.
  Hypothesis HRbMth : forall d tauk m th f' sm dc r d',
      compose_smap d sm = Some dc ->
      run_binds Sg ((IMth tauk m th, DClo f' dc) :: d) r d' ->
      Pb ((IMth tauk m th, DClo f' dc) :: d) r d' ->
      Pb d (BMth tauk m th f' sm :: r) d'.
  Hypothesis HRbFun : forall d u h sm smc dc r d',
      compose_smap d smc = Some dc ->
      run_binds Sg ((IFun u, DFun h [sm] dc) :: d) r d' ->
      Pb ((IFun u, DFun h [sm] dc) :: d) r d' ->
      Pb d (BFun u h sm smc :: r) d'.
  Hypothesis HRbFunA : forall d u u' sm h sms dc r d',
      lookup_denv (IFun u') d = Some (DFun h sms dc) ->
      run_binds Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) r d' ->
      Pb ((IFun u, DFun h (sms ++ [sm]) dc) :: d) r d' ->
      Pb d (BFunA u u' sm :: r) d'.
  Hypothesis HRbApply : forall d u th sm dr r d',
      run_apply Sg d u th sm dr -> Pa d u th sm dr ->
      run_binds Sg (dr ++ d) r d' -> Pb (dr ++ d) r d' ->
      Pb d (BApply u th sm :: r) d'.
  Hypothesis HRunApply : forall d u th sm h sms dc Dh Dr bs d1 d2 d3 dr,
      lookup_denv (IFun u) d = Some (DFun h sms dc) ->
      g_fun Sg h = Some (Dh, Dr, Some bs) ->
      compose_smap d sm = Some d1 ->
      run_chain d1 sms = Some d2 ->
      run_binds Sg (d2 ++ dc) bs d3 -> Pb (d2 ++ dc) bs d3 ->
      compose_smap d3 (inst_smap th Dr) = Some dr ->
      Pa d u th sm dr.

  (* One mutual induction over evaluation, a body's run, and an
     application. *)
  Lemma eval_ind' : forall d g e w, eval Sg d g e w -> P d g e w
  with run_binds_ind' : forall d bs d', run_binds Sg d bs d' -> Pb d bs d'
  with run_apply_ind' : forall d u th sm dr, run_apply Sg d u th sm dr -> Pa d u th sm dr.
  Proof.
    - intros d g e w D.
      destruct D.
      + apply HUnit.
      + apply HVar; assumption.
      + apply HVal; assumption.
      + apply HTag; [assumption | apply eval_ind'; assumption].
      + eapply HLet; try eassumption; apply eval_ind'; assumption.
      + eapply HMatch; try eassumption; apply eval_ind'; assumption.
      + eapply HCallD; try eassumption.
        * match goal with
          | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
              clear - eval_ind' F; induction F; constructor;
              [apply eval_ind'; assumption | assumption]
          end.
        * apply eval_ind'; assumption.
      + eapply HCallA; try eassumption.
        * match goal with
          | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
              clear - eval_ind' F; induction F; constructor;
              [apply eval_ind'; assumption | assumption]
          end.
        * apply eval_ind'; assumption.
      + eapply HMeth; try eassumption.
        * apply eval_ind'; assumption.
        * match goal with
          | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
              clear - eval_ind' F; induction F; constructor;
              [apply eval_ind'; assumption | assumption]
          end.
        * apply eval_ind'; assumption.
      + apply HBindTy; [assumption | apply eval_ind'; assumption].
      + eapply HBindVal; try eassumption; apply eval_ind'; assumption.
      + eapply HBindFn; try eassumption; apply eval_ind'; assumption.
      + eapply HBindMth; try eassumption; apply eval_ind'; assumption.
      + eapply HBindFun; try eassumption; apply eval_ind'; assumption.
      + eapply HBindFunA; try eassumption; apply eval_ind'; assumption.
      + eapply HApply; try eassumption;
          [apply run_apply_ind'; assumption | apply eval_ind'; assumption].
    - intros d bs d' D.
      destruct D.
      + apply HRbNil.
      + apply HRbTy; [assumption | apply run_binds_ind'; assumption].
      + eapply HRbVal; try eassumption;
          [apply eval_ind'; assumption | apply run_binds_ind'; assumption].
      + eapply HRbFn; try eassumption; apply run_binds_ind'; assumption.
      + eapply HRbMth; try eassumption; apply run_binds_ind'; assumption.
      + eapply HRbFun; try eassumption; apply run_binds_ind'; assumption.
      + eapply HRbFunA; try eassumption; apply run_binds_ind'; assumption.
      + eapply HRbApply; try eassumption;
          [apply run_apply_ind'; assumption | apply run_binds_ind'; assumption].
    - intros d u th sm dr D.
      destruct D.
      eapply HRunApply; try eassumption.
      apply run_binds_ind'; assumption.
  Qed.
End EvalInd.

(* ===================== 11. Metatheory ==================================== *)

(* ---- Proposition 4.1 (resolution is search-free). ----------------------
   The context premises of the statics are the single membership test
   `avail`, a boolean over the finite set A — decidable by construction, and
   no judgment quantifies over Σ.  The uniqueness premise of T-Meth makes
   the provision annotation a function of the derivation up to σ: provisions
   are keyed by the receiver's (declaration, static bindings) identity,
   compared through the merged substitution [D62]. *)

Theorem availability_decidable :
  forall Phi i, {avail Phi i} + {~ avail Phi i}.
Proof.
  intros Phi i. unfold avail.
  destruct (avail_b Phi i).
  - left; reflexivity.
  - right; discriminate.
Defined.

(* What a well-typed method call carries: its provision is in force, keyed
   at m, and is the only provision of its own key modulo σ (T-Meth's
   uniqueness premise, read back through any subsumption). *)
Lemma meth_typed_unique : forall Sg Phi G e0 m i args t,
  has_ty Sg Phi G (EMeth e0 m i args) t ->
  exists rt0 th0, i = IMth rt0 m th0 /\ In i (cA Phi)
  /\ forall i', In i' (cA Phi) -> is_prov_at (cS Phi) rt0 m i' ->
       app_subst_item (cS Phi) i' = app_subst_item (cS Phi) i.
Proof.
  intros Sg Phi G e0 m i args t H.
  remember (EMeth e0 m i args) as E eqn:HE. revert e0 m i args HE.
  induction H; intros ee mm ii aa HE; try discriminate HE.
  - exact (IHhas_ty _ _ _ _ HE).
  - injection HE as <- <- <- <-.
    exists rt0, th0. split; [reflexivity |]. split; [assumption |].
    intros i' Hin [th' Hp]. apply H3; [exact Hin |].
    exists th'. rewrite Hp, H2. reflexivity.
Qed.

(* Elaboration's choice of provision is a function of the context and the
   key: two well-typed calls of m in one context whose provisions are at
   one receiver type (modulo σ) carry the same provision modulo σ — derived
   from the typing derivations, not assumed.  (The previous statement took
   the uniqueness it concluded as a hypothesis; see the theorem audit.) *)
Theorem method_resolution_functional :
  forall Sg (Phi : ctx) G1 G2 e1 e2 m i1 i2 args1 args2 t1 t2 k,
    has_ty Sg Phi G1 (EMeth e1 m i1 args1) t1 ->
    has_ty Sg Phi G2 (EMeth e2 m i2 args2) t2 ->
    is_prov_at (cS Phi) k m i1 -> is_prov_at (cS Phi) k m i2 ->
    app_subst_item (cS Phi) i1 = app_subst_item (cS Phi) i2.
Proof.
  intros Sg Phi G1 G2 e1 e2 m i1 i2 args1 args2 t1 t2 k H1 H2 [th1 P1] P2.
  destruct (meth_typed_unique H1) as [rt1 [th01 [-> [Hin1 U1]]]].
  destruct (meth_typed_unique H2) as [rt2 [th02 [-> [Hin2 _]]]].
  symmetry. apply U1; [exact Hin2 |].
  destruct P2 as [th2 P2]. exists th2. rewrite P2.
  simpl in P1. injection P1 as Hrt _. rewrite Hrt. reflexivity.
Qed.

(* ---- Leaf lemmas for the proof effort -----------------------------------
   The statements below are the proof-development interface. *)

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
(* Likewise a body's run and an application [D78]: their inr payloads only
   ever pass an Err or OutOfFuel along. *)
Lemma interp_binds_never_ok :
  forall fuel,
    (forall Sg d bs w, interp_binds fuel Sg d bs <> inr (Ok w))
    /\ (forall Sg d u th sm w, interp_apply fuel Sg d u th sm <> inr (Ok w)).
Proof.
  induction fuel as [| k [IH1 IH2]].
  - split; intros; simpl; discriminate.
  - split.
    + intros Sg d bs w Heq. simpl in Heq.
      destruct bs as [| b r]; [discriminate Heq |].
      destruct b as [t tau | v e1 | f f' sm | tauk mm th f' sm | u h sm smc
                    | u u' sm | u th sm].
      * exact (IH1 _ _ _ _ Heq).
      * destruct (interp k Sg d [] e1) eqn:E1;
          [exact (IH1 _ _ _ _ Heq) | discriminate Heq | discriminate Heq].
      * destruct (compose_smap d sm); [exact (IH1 _ _ _ _ Heq) | discriminate Heq].
      * destruct (compose_smap d sm); [exact (IH1 _ _ _ _ Heq) | discriminate Heq].
      * destruct (compose_smap d smc); [exact (IH1 _ _ _ _ Heq) | discriminate Heq].
      * destruct (lookup_denv (IFun u') d) as [[wv|fc dc|hh ss dc]|];
          try discriminate Heq.
        exact (IH1 _ _ _ _ Heq).
      * destruct (interp_apply k Sg d u th sm) as [dr | rr] eqn:E1.
        -- exact (IH1 _ _ _ _ Heq).
        -- injection Heq as ->. exact (IH2 _ _ _ _ _ _ E1).
    + intros Sg d u th sm w Heq. simpl in Heq.
      destruct (lookup_denv (IFun u) d) as [[wv|fc dc|h sms dc]|];
        try discriminate Heq.
      destruct (g_fun Sg h) as [[[Dh Dr] [bs|]]|]; try discriminate Heq.
      destruct (compose_smap d sm) as [d1|]; try discriminate Heq.
      destruct (run_chain d1 sms) as [d2|]; try discriminate Heq.
      destruct (interp_binds k Sg (d2 ++ dc) bs) as [d3 | rr] eqn:E1.
      * destruct (compose_smap d3 (inst_smap th Dr)); discriminate Heq.
      * injection Heq as ->. exact (IH1 _ _ _ _ E1).
Qed.

(* BEGIN:interp_S_all *)
Lemma interp_S_all :
  forall fuel,
    (forall Sg d g e w, interp fuel Sg d g e = Ok w ->
                        interp (S fuel) Sg d g e = Ok w)
    /\ (forall Sg d g l ws, interp_list fuel Sg d g l = inl ws ->
                            interp_list (S fuel) Sg d g l = inl ws)
    /\ (forall Sg d bs d', interp_binds fuel Sg d bs = inl d' ->
                           interp_binds (S fuel) Sg d bs = inl d')
    /\ (forall Sg d u th sm dr, interp_apply fuel Sg d u th sm = inl dr ->
                             interp_apply (S fuel) Sg d u th sm = inl dr).
Proof.
  induction fuel as [|k [IH1 [IH2 [IH3 IH4]]]].
  - repeat split; intros; simpl in *; discriminate.
  - split; [| split; [| split]].
    + intros Sg d g e w H. simpl in H.
      remember (S k) as q eqn:Eq.
      destruct e as [ x | | v | n th e1 | f th sm args | f args
                    | e0 mm prov args | e0 arms | x e1 e2 | t tau e1
                    | v e1 e2 | f f' sm e1 | tauk mm th f' sm e1
                    | u h sm smc e1 | u u' sm e1 | u th sm e1 ];
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
      * destruct (lookup_denv (IFn f) d) as [[wv|fc dc|hh ss]|] eqn:E0;
          try discriminate.
        destruct (g_fn Sg fc) as [[[D0 S0] ob]|] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) eqn:E2.
        -- rewrite (IH2 _ _ _ _ _ E2). apply IH1. assumption.
        -- exfalso. rewrite H in E2.
           eapply interp_list_never_ok; exact E2.
      * destruct (lookup_denv prov d) as [[wv|fc dc|hh ss]|] eqn:E0;
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
      * destruct (compose_smap d smc) eqn:E1; try discriminate.
        apply IH1. assumption.
      * destruct (lookup_denv (IFun u') d) as [[wv|fc dc|hh ss dc]|] eqn:E0;
          try discriminate.
        apply IH1. assumption.
      * destruct (interp_apply k Sg d u th sm) as [dr | rr] eqn:E1.
        -- rewrite (IH4 _ _ _ _ _ _ E1). apply IH1. assumption.
        -- exfalso. subst rr.
           exact (proj2 (interp_binds_never_ok k) _ _ _ _ _ _ E1).
    + intros Sg d g l ws H. simpl in H.
      remember (S k) as q eqn:Eq.
      destruct l as [|e1 r]; simpl; subst q; simpl in H.
      * assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1).
        destruct (interp_list k Sg d g r) eqn:E2; try discriminate.
        rewrite (IH2 _ _ _ _ _ E2). assumption.
    + intros Sg d bs d' H. simpl in H.
      remember (S k) as q eqn:Eq.
      destruct bs as [| b r]; simpl; subst q; simpl in H; [assumption |].
      destruct b as [t tau | v e1 | f f' sm | tauk mm th f' sm | u h sm smc
                    | u u' sm | u th sm].
      * apply IH3. assumption.
      * destruct (interp k Sg d [] e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1). apply IH3. assumption.
      * destruct (compose_smap d sm) eqn:E1; try discriminate.
        apply IH3. assumption.
      * destruct (compose_smap d sm) eqn:E1; try discriminate.
        apply IH3. assumption.
      * destruct (compose_smap d smc) eqn:E1; try discriminate.
        apply IH3. assumption.
      * destruct (lookup_denv (IFun u') d) as [[wv|fc dc|hh ss dc]|] eqn:E0;
          try discriminate.
        apply IH3. assumption.
      * destruct (interp_apply k Sg d u th sm) as [dr | rr] eqn:E1;
          try discriminate.
        rewrite (IH4 _ _ _ _ _ _ E1). apply IH3. assumption.
    + intros Sg d u th sm dr H. simpl in H.
      remember (S k) as q eqn:Eq.
      simpl; subst q.
      destruct (lookup_denv (IFun u) d) as [[wv|fc dc|h sms dc]|];
        try discriminate.
      destruct (g_fun Sg h) as [[[Dh Dr] [bs|]]|]; try discriminate.
      destruct (compose_smap d sm) as [d1|]; try discriminate.
      destruct (run_chain d1 sms) as [d2|]; try discriminate.
      destruct (interp_binds k Sg (d2 ++ dc) bs) as [d3 | rr] eqn:E1; try discriminate.
      rewrite (IH3 _ _ _ _ E1). exact H.
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

Lemma interp_binds_mono :
  forall fuel fuel' Sg d bs d',
    fuel <= fuel' -> interp_binds fuel Sg d bs = inl d' ->
    interp_binds fuel' Sg d bs = inl d'.
Proof.
  intros fuel fuel' Sg d bs d' Hle.
  induction Hle as [| fuel' Hle IH].
  - intros H; exact H.
  - intros H. apply interp_S_all. apply IH. exact H.
Qed.

Lemma interp_apply_mono :
  forall fuel fuel' Sg d u th sm dr,
    fuel <= fuel' -> interp_apply fuel Sg d u th sm = inl dr ->
    interp_apply fuel' Sg d u th sm = inl dr.
Proof.
  intros fuel fuel' Sg d u th sm dr Hle.
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

Lemma eval_deterministic_aux12 :
  forall Sg d g u h sm smc e w2,
    eval Sg d g (EBindFun u h sm smc e) w2 ->
    exists dc, compose_smap d smc = Some dc /\
               eval Sg ((IFun u, DFun h [sm] dc) :: d) g e w2.
Proof.
  intros Sg d g u h sm smc e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux13 :
  forall Sg d g u u' sm e w2,
    eval Sg d g (EBindFunA u u' sm e) w2 ->
    exists h sms dc, lookup_denv (IFun u') d = Some (DFun h sms dc) /\
                  eval Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) g e w2.
Proof.
  intros Sg d g u u' sm e w2 H. inversion H; subst.
  do 3 eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux14 :
  forall Sg d g u th sm e w2,
    eval Sg d g (EApply u th sm e) w2 ->
    exists dr, run_apply Sg d u th sm dr /\ eval Sg (dr ++ d) g e w2.
Proof.
  intros Sg d g u th sm e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

(* The same inversions for a body's run and an application. *)
Lemma run_binds_inv : forall Sg d b r d2,
    run_binds Sg d (b :: r) d2 ->
    match b with
    | BTy _ _ => run_binds Sg d r d2
    | BVal v e1 => exists w1, eval Sg d [] e1 w1 /\
                     run_binds Sg ((IVal v, DVal w1) :: d) r d2
    | BFn f f' sm => exists dc, compose_smap d sm = Some dc /\
                     run_binds Sg ((IFn f, DClo f' dc) :: d) r d2
    | BMth tauk m th f' sm => exists dc, compose_smap d sm = Some dc /\
                     run_binds Sg ((IMth tauk m th, DClo f' dc) :: d) r d2
    | BFun u h sm smc => exists dc, compose_smap d smc = Some dc /\
                     run_binds Sg ((IFun u, DFun h [sm] dc) :: d) r d2
    | BFunA u u' sm => exists h sms dc,
                     lookup_denv (IFun u') d = Some (DFun h sms dc) /\
                     run_binds Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) r d2
    | BApply u th sm => exists dr, run_apply Sg d u th sm dr /\
                     run_binds Sg (dr ++ d) r d2
    end.
Proof.
  intros Sg d b r d2 H. inversion H; subst.
  all: first [ assumption | (eexists; split; eassumption)
             | (do 3 eexists; split; eassumption) ].
Qed.

Lemma run_apply_inv : forall Sg d u th sm dr,
    run_apply Sg d u th sm dr ->
    exists h sms dc Dh Dr bs d1 d2 d3,
      lookup_denv (IFun u) d = Some (DFun h sms dc) /\
      g_fun Sg h = Some (Dh, Dr, Some bs) /\
      compose_smap d sm = Some d1 /\
      run_chain d1 sms = Some d2 /\
      run_binds Sg (d2 ++ dc) bs d3 /\
      compose_smap d3 (inst_smap th Dr) = Some dr.
Proof.
  intros Sg d u th sm dr H. inversion H; subst.
  do 9 eexists. repeat split; eassumption.
Qed.

Theorem eval_deterministic :
  forall Sg d g e w1 w2,
    eval Sg d g e w1 -> eval Sg d g e w2 -> w1 = w2.
Proof.
  intros Sg.
  assert (Hmain : forall d g e w1,
             eval Sg d g e w1 -> forall w2, eval Sg d g e w2 -> w1 = w2).
  { apply (@eval_ind' Sg
             (fun d g e w1 => forall w2, eval Sg d g e w2 -> w1 = w2)
             (fun d bs d1 => forall d2, run_binds Sg d bs d2 -> d1 = d2)
             (fun d u th sm dr => forall dr', run_apply Sg d u th sm dr' -> dr = dr')).
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
      apply IH. exact Hb.
    - (* EBindFun *)
      intros d g u h sm smc dc e w Hcs Hev IH w2 H2.
      apply eval_deterministic_aux12 in H2.
      destruct H2 as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb.
    - (* EBindFunA *)
      intros d g u u' sm h sms dc e w Hl Hev IH w2 H2.
      apply eval_deterministic_aux13 in H2.
      destruct H2 as [h0 [sms0 [dc0 [Ha Hb]]]].
      rewrite Hl in Ha. injection Ha as <- <- <-.
      apply IH. exact Hb.
    - (* EApply *)
      intros d g u th sm dr e w Hra IHa Hev IH w2 H2.
      apply eval_deterministic_aux14 in H2.
      destruct H2 as [dr0 [Ha Hb]].
      specialize (IHa _ Ha). subst dr0.
      apply IH. exact Hb.
    - (* RbNil *)
      intros d d2 H. inversion H; subst. reflexivity.
    - (* RbTy *)
      intros d t tau r d' Hr IH d2 H.
      apply run_binds_inv in H. apply IH. exact H.
    - (* RbVal *)
      intros d v e1 w1 r d' Hev IHe Hr IH d2 H.
      apply run_binds_inv in H. destruct H as [w0 [Ha Hb]].
      specialize (IHe _ Ha). subst w0. apply IH. exact Hb.
    - (* RbFn *)
      intros d f f' sm dc r d' Hcs Hr IH d2 H.
      apply run_binds_inv in H. destruct H as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb.
    - (* RbMth *)
      intros d tauk m th f' sm dc r d' Hcs Hr IH d2 H.
      apply run_binds_inv in H. destruct H as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb.
    - (* RbFun *)
      intros d u h sm smc dc r d' Hcs Hr IH d2 H.
      apply run_binds_inv in H. destruct H as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb.
    - (* RbFunA *)
      intros d u u' sm h sms dc r d' Hl Hr IH d2 H.
      apply run_binds_inv in H. destruct H as [h0 [sms0 [dc0 [Ha Hb]]]].
      rewrite Hl in Ha. injection Ha as <- <- <-.
      apply IH. exact Hb.
    - (* RbApply *)
      intros d u th sm dr r d' Hra IHa Hr IH d2 H.
      apply run_binds_inv in H. destruct H as [dr0 [Ha Hb]].
      specialize (IHa _ Ha). subst dr0.
      apply IH. exact Hb.
    - (* RunApply *)
      intros d u th sm h sms dc Dh Dr bs d1 d2 d3 dr Hl Hg H1 H2 Hb IHb H3 dr' H.
      apply run_apply_inv in H.
      destruct H as [h0 [sms0 [dc0 [Dh0 [Dr0 [bs0 [d10 [d20 [d30
                      [Ha [Hb' [Hc [Hd [He Hf]]]]]]]]]]]]]].
      rewrite Hl in Ha. injection Ha as <- <- <-.

      rewrite Hg in Hb'. injection Hb' as <- <- <-.
      rewrite H1 in Hc. injection Hc as <-.
      rewrite H2 in Hd. injection Hd as <-.
      specialize (IHb _ He). subst d30.
      congruence. }
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
                            Forall2 (eval Sg d g) l ws)
    /\ (forall Sg d bs d', interp_binds fuel Sg d bs = inl d' ->
                           run_binds Sg d bs d')
    /\ (forall Sg d u th sm dr, interp_apply fuel Sg d u th sm = inl dr ->
                             run_apply Sg d u th sm dr).
Proof.
  induction fuel as [| k IHk].
  - repeat split; intros; simpl in *; discriminate.
  - destruct IHk as [IH1 [IH2 [IH3 IH4]]]. split; [| split; [| split]].
    + intros Sg d g e w H.
      destruct e as [ x | | v | n th e1 | f th sm args | f args
                    | e0 m prov args | e0 arms | x e1 e2 | t tau e1
                    | v e1 e2 | f f' sm e1 | tauk m th f' sm e1
                    | u h sm smc e1 | u u' sm e1 | u th sm e1 ];
        simpl in H.
      * destruct (lookup_venv x g) as [w'|] eqn:E1; try discriminate.
        injection H as H; subst. constructor; assumption.
      * injection H as H; subst. constructor.
      * destruct (lookup_denv (IVal v) d) as [en|] eqn:E1; try discriminate.
        destruct en as [w' | fc dc | hh ss dd]; try discriminate.
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
        destruct en as [w' | f' df | hh ss dd]; try discriminate.
        destruct (g_fn Sg f') as [[[D S] ob] | ] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) as [ws | rr] eqn:E2.
        -- apply IH2 in E2. apply IH1 in H.
           eapply EvCallA; eassumption.
        -- subst rr. exfalso. eapply interp_list_never_ok. exact E2.
      * destruct (lookup_denv prov d) as [en|] eqn:E0; try discriminate.
        destruct en as [w' | f' d' | hh ss dd]; try discriminate.
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
      * destruct (compose_smap d smc) as [dc|] eqn:E1; try discriminate.
        apply IH1 in H. eapply EvBindFun; eassumption.
      * destruct (lookup_denv (IFun u') d) as [[wv | fc dc | hh ss dd]|] eqn:E1;
          try discriminate.
        apply IH1 in H. eapply EvBindFunA; eassumption.
      * destruct (interp_apply k Sg d u th sm) as [dr | rr] eqn:E1.
        -- apply IH4 in E1. apply IH1 in H. eapply EvApply; eassumption.
        -- subst rr. exfalso.
           exact (proj2 (interp_binds_never_ok k) _ _ _ _ _ _ E1).
    + intros Sg d g l ws H.
      destruct l as [| e1 r]; simpl in H.
      * injection H as H; subst. constructor.
      * destruct (interp k Sg d g e1) as [w | | ] eqn:E1; try discriminate.
        destruct (interp_list k Sg d g r) as [ws' | rr] eqn:E2;
          try discriminate.
        injection H as H; subst. apply IH1 in E1. apply IH2 in E2.
        constructor; assumption.
    + intros Sg d bs d' H.
      destruct bs as [| b r]; simpl in H.
      * injection H as H; subst. constructor.
      * destruct b as [t tau | v e1 | f f' sm | tauk mm th f' sm | u h sm smc
                      | u u' sm | u th sm].
        -- apply IH3 in H. constructor; assumption.
        -- destruct (interp k Sg d [] e1) as [w1 | | ] eqn:E1; try discriminate.
           apply IH1 in E1. apply IH3 in H. eapply RbVal; eassumption.
        -- destruct (compose_smap d sm) as [dc|] eqn:E1; try discriminate.
           apply IH3 in H. eapply RbFn; eassumption.
        -- destruct (compose_smap d sm) as [dc|] eqn:E1; try discriminate.
           apply IH3 in H. eapply RbMth; eassumption.
        -- destruct (compose_smap d smc) as [dc|] eqn:E1; try discriminate.
           apply IH3 in H. eapply RbFun; eassumption.
        -- destruct (lookup_denv (IFun u') d) as [[wv | fc dc | hh ss dd]|] eqn:E1;
             try discriminate.
           apply IH3 in H. eapply RbFunA; eassumption.
        -- destruct (interp_apply k Sg d u th sm) as [dr | rr] eqn:E1;
             try discriminate.
           apply IH4 in E1. apply IH3 in H. eapply RbApply; eassumption.
    + intros Sg d u th sm dr H. simpl in H.
      destruct (lookup_denv (IFun u) d) as [[wv | fc dc | h sms dc]|] eqn:E1;
        try discriminate.
      destruct (g_fun Sg h) as [[[Dh Dr] [bs|]]|] eqn:E2; try discriminate.
      destruct (compose_smap d sm) as [d1|] eqn:E3; try discriminate.
      destruct (run_chain d1 sms) as [d2|] eqn:E4; try discriminate.
      destruct (interp_binds k Sg (d2 ++ dc) bs) as [d3 | rr] eqn:E5; try discriminate.
      destruct (compose_smap d3 (inst_smap th Dr)) as [dr'|] eqn:E6; try discriminate.
      injection H as <-. apply IH3 in E5.
      eapply RunApply; eassumption.
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
           (fun d g e w => exists fuel, interp fuel Sg d g e = Ok w)
           (fun d bs d' => exists fuel, interp_binds fuel Sg d bs = inl d')
           (fun d u th sm dr => exists fuel, interp_apply fuel Sg d u th sm = inl dr)).
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
  - (* EvBindFun *) intros d g u h sm smc dc e w Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
  - (* EvBindFunA *) intros d g u u' sm h sms dc e w Hl _ [a Ha].
    exists (S a). simpl. rewrite Hl. exact Ha.
  - (* EvApply *) intros d g u th sm dr e w _ [a Ha] _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp_apply (Nat.max a b) Sg d u th sm = inl dr)
      by (eapply interp_apply_mono; [apply Nat.le_max_l | exact Ha]).
    assert (Hb' : interp (Nat.max a b) Sg (dr ++ d) g e = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Ha'. exact Hb'.
  - (* RbNil *) intros d. exists 1. reflexivity.
  - (* RbTy *) intros d t tau r d' _ [a Ha]. exists (S a). simpl. exact Ha.
  - (* RbVal *) intros d v e1 w1 r d' _ [a Ha] _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp (Nat.max a b) Sg d [] e1 = Ok w1)
      by (apply interp_complete_aux1; exact Ha).
    assert (Hb' : interp_binds (Nat.max a b) Sg ((IVal v, DVal w1) :: d) r
                  = inl d')
      by (eapply interp_binds_mono; [apply Nat.le_max_r | exact Hb]).
    simpl. rewrite Ha'. exact Hb'.
  - (* RbFn *) intros d f f' sm dc r d' Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
  - (* RbMth *) intros d tauk m th f' sm dc r d' Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
  - (* RbFun *) intros d u h sm smc dc r d' Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
  - (* RbFunA *) intros d u u' sm h sms dc r d' Hl _ [a Ha].
    exists (S a). simpl. rewrite Hl. exact Ha.
  - (* RbApply *) intros d u th sm dr r d' _ [a Ha] _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp_apply (Nat.max a b) Sg d u th sm = inl dr)
      by (eapply interp_apply_mono; [apply Nat.le_max_l | exact Ha]).
    assert (Hb' : interp_binds (Nat.max a b) Sg (dr ++ d) r = inl d')
      by (eapply interp_binds_mono; [apply Nat.le_max_r | exact Hb]).
    simpl. rewrite Ha'. exact Hb'.
  - (* RunApply *)
    intros d u th sm h sms dc Dh Dr bs d1 d2 d3 dr Hl Hg H1 H2 _ [a Ha] H3.

    exists (S a). simpl. rewrite Hl, Hg, H1, H2, Ha, H3. reflexivity.
Qed.
(* END:interp_complete *)

(* ===================== 11b. The semantic soundness development =========== *)

(* --- Free symbols and closed types. --------------------------------------
   (ty_frees itself lives in §2: the merging unifier's occurs check needs
   it.) *)

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
  /\ sigma_regular (cS Phi) (cA Phi)
  /\ ~ In this_sym (tyreqs (cA Phi)).

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
          exists PhiD' g',
            tele_ctx D' = Some PhiD' /\
            grounding g' /\ covers g' (tyreqs (cA PhiD')) /\
            (* the closure's grounding respects the provider telescope's
               merge equations *)
            absorbs g' (cS PhiD') /\
            map (app_subst g') (fs_params S') = map (app_subst g) (fs_params Sf) /\
            app_subst g' (fs_ret S') = app_subst g (fs_ret Sf) /\
            tele_covered d' (cA PhiD') /\
            (* the captured environment's keys are the telescope's items:
               their type material is bounded by the telescope *)
            Forall (fun p =>
              incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiD'))) d' /\
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
          exists PhiD' g',
            tele_ctx D' = Some PhiD' /\
            grounding g' /\ covers g' (tyreqs (cA PhiD')) /\
            (* the closure's grounding respects the provider telescope's
               merge equations *)
            absorbs g' (cS PhiD') /\
            map (app_subst g') (fs_params S')
              = app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm) /\
            app_subst g' (fs_ret S')
              = app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm))) /\
            tele_covered d' (cA PhiD') /\
            (* the captured environment's keys are the telescope's items:
               their type material is bounded by the telescope *)
            Forall (fun p =>
              incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiD'))) d' /\
            (fix all (dd : denv) : Prop :=
               match dd with
               | [] => True
               | p :: r => dentry_agree Sg g' (fst p) (snd p) /\ all r
               end) d'
      | _, _ => False
      end
  (* A functor binding [D78]: h has a body and φ's result, and its chain of
     links leads from h's arguments out to φ's.  Rule A makes it a closure
     (cf. DClo): g' is the grounding of h's captured inputs where it was
     bound, the captured entries dc agree at g', g' validates h's pins, and
     — the real change — g' reads every captured symbol that h's linked
     arguments or its result also read as the caller's g does; each such
     symbol is a fixed input of φ.  So the entry depends on g only through
     X(φ) = dep_tys (IFun φ). *)
  | IFun u, DFun h sms dc =>
      match g_fun Sg u, g_fun Sg h with
      | Some (_, Dr, None), Some (Dh, Drh, Some _) =>
          Drh = Dr /\ chain_ok Sg h sms u /\
          exists PhiC g',
            tele_ctx (fst (g_cap Sg h)) = Some PhiC /\
            grounding g' /\ covers g' (tyreqs (cA PhiC)) /\
            absorbs g' (cS PhiC) /\
            absorbs g' (snd (g_cap Sg h)) /\
            tele_covered dc (cA PhiC) /\
            Forall (fun p =>
              incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
                    ++ method_item_frees Sg (fst p)) (tyreqs (cA PhiC))) dc /\
            (forall x, In x (tyreqs (cA PhiC)) ->
                       In x (tyreqs Dh ++ res_reads Sg Dr) ->
                       In x (g_fixed Sg u)
                       /\ app_subst g' (TAbs x) = app_subst g (TAbs x)) /\
            (fix all (dd : denv) : Prop :=
               match dd with
               | [] => True
               | p :: r => dentry_agree Sg g' (fst p) (snd p) /\ all r
               end) dc
      | _, _ => False
      end

  | _, _ => False
  end.

Definition denv_agree (Sg : gsig) (g : subst) (A : list item) (d : denv) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_denv i d = Some en) A
  /\ Forall (fun p => dentry_agree Sg g (fst p) (snd p)) d
  (* item regularity: what the in-force items mention is in force *)
  /\ Forall (fun i =>
       incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
            (tyreqs A)) A
  (* and the same for the environment's own keys *)
  /\ Forall (fun p =>
       incl (dep_tys Sg (fst p) ++ item_ty_frees (fst p)
             ++ method_item_frees Sg (fst p)) (tyreqs A)) d.

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
  destruct a as [t1 | v1 | f1 | r1 m1 th1 | u1];
    destruct b as [t2 | v2 | f2 | r2 m2 th2 | u2];
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
  - apply Nat.eqb_eq in H; subst; reflexivity.
  - injection H as ->; apply Nat.eqb_eq; reflexivity.
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

Lemma avail_fun_in : forall Phi u, avail Phi (IFun u) -> In (IFun u) (cA Phi).
Proof.
  intros Phi u H.
  apply avail_in_aux1 in H.
  destruct H as [i0 [Hin Heq]].
  unfold item_subst_eqb in Heq.
  apply item_eqb_eq in Heq.
  destruct i0; simpl in Heq; try discriminate.
  inversion Heq; subst. exact Hin.
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

(* --- The merge's shape and its verified output (leaf lemmas). ------------
   merge_item either appends a fresh-key item (σ unchanged) or unifies a
   colliding one (items unchanged, σ verified).  The boolean checks it ran
   are the source of the σ-regularity the soundness invariant carries.
   Hint: unfold merge_item and case on find_same_key / unify_items / the
   verification conjunction; the bridging lemmas are inductions on lists
   with existsb/forallb via Nat.eqb_eq and ty_eqb_eq. *)
(* BEGIN:merge_item_lemmas *)
Lemma inclb_incl : forall xs ys, inclb xs ys = true -> incl xs ys.
Proof.
  intros xs ys H. unfold inclb in H. rewrite forallb_forall in H.
  intros x Hx. specialize (H x Hx).
  apply existsb_exists in H. destruct H as [y [Hy He]].
  apply Nat.eqb_eq in He. subst y. exact Hy.
Qed.

Lemma sigma_regular_b_sound : forall sg A,
  sigma_regular_b sg A = true -> sigma_regular sg A.
Proof.
  intros sg A H. unfold sigma_regular_b in H.
  apply andb_prop in H. destruct H as [H1 H2].
  split.
  - apply inclb_incl. exact H1.
  - rewrite forallb_forall in H2. rewrite Forall_forall.
    intros p Hp. apply inclb_incl. exact (H2 p Hp).
Qed.

Lemma merge_item_cA : forall Phi i Phi',
  merge_item Phi i = Some Phi' ->
  cA Phi' = cA Phi ++ [i] \/ cA Phi' = cA Phi.
Proof.
  intros Phi i Phi' H. unfold merge_item in H.
  destruct (find_same_key (cS Phi) (cA Phi) i).
  - destruct (unify_items (merge_fuel (cS Phi) i0 i) (cS Phi) i0 i);
      [| discriminate].
    destruct (item_subst_eqb s i0 i && sigma_regular_b s (cA Phi)
              && idempotent_b s); [| discriminate].
    injection H as <-. right. reflexivity.
  - injection H as <-. left. reflexivity.
Qed.

Lemma merge_item_tyreqs : forall Phi i Phi',
  merge_item Phi i = Some Phi' ->
  incl (tyreqs (cA Phi)) (tyreqs (cA Phi')).
Proof.
  intros Phi i Phi' H.
  destruct (merge_item_cA _ _ H) as [-> | ->].
  - unfold tyreqs. rewrite flat_map_app.
    intros t Ht. apply in_or_app. left. exact Ht.
  - intros t Ht. exact Ht.
Qed.

Lemma merge_item_sigma : forall Phi i Phi',
  merge_item Phi i = Some Phi' ->
  sigma_regular (cS Phi) (cA Phi) ->
  sigma_regular (cS Phi') (cA Phi').
Proof.
  intros Phi i Phi' H Hreg. unfold merge_item in H.
  destruct (find_same_key (cS Phi) (cA Phi) i).
  - destruct (unify_items (merge_fuel (cS Phi) i0 i) (cS Phi) i0 i)
      as [s'|]; [| discriminate].
    destruct (item_subst_eqb s' i0 i) eqn:E1; simpl in H; [| discriminate].
    destruct (sigma_regular_b s' (cA Phi)) eqn:E2; simpl in H;
      [| discriminate].
    destruct (idempotent_b s') eqn:E3; [| discriminate].
    injection H as <-. simpl.
    apply sigma_regular_b_sound. exact E2.
  - injection H as <-. simpl.
    destruct Hreg as [Hdom Hrng].
    split.
    + intros t Ht. specialize (Hdom t Ht).
      unfold tyreqs. rewrite flat_map_app.
      apply in_or_app. left. exact Hdom.
    + rewrite Forall_forall in Hrng |- *. intros p Hp.
      intros t Ht. specialize (Hrng p Hp t Ht).
      unfold tyreqs. rewrite flat_map_app.
      apply in_or_app. left. exact Hrng.
Qed.

Lemma tele_sigma_regular : forall Sg Phi D PhiD,
  wf_tele Sg Phi D PhiD ->
  sigma_regular (cS Phi) (cA Phi) ->
  sigma_regular (cS PhiD) (cA PhiD).
Proof.
  intros Sg Phi D PhiD H.
  induction H as [Phi | Phi i D Phi' Phi'' _ Hm _ IH]; intros Hreg.
  - exact Hreg.
  - apply IH. exact (merge_item_sigma _ _ Hm Hreg).
Qed.

Lemma find_same_key_in : forall s A i j,
  find_same_key s A i = Some j -> In j A /\ same_key s j i = true.
Proof.
  intros s A i. induction A as [|a r IH]; intros j H; simpl in H.
  - discriminate.
  - destruct (same_key s a i) eqn:E.
    + injection H as <-. split; [left; reflexivity | exact E].
    + destruct (IH j H) as [Hin Hk].
      split; [right; exact Hin | exact Hk].
Qed.

(* Merging neither invents nor loses type requirements: a dropped duplicate
   ITy shares its key — the symbol itself — with the occurrence kept. *)
Lemma merge_item_tyreqs_iff : forall Phi i Phi',
  merge_item Phi i = Some Phi' ->
  forall t, In t (tyreqs (cA Phi')) <->
            In t (tyreqs (cA Phi)) \/ In t (tyreqs [i]).
Proof.
  intros Phi i Phi' H t. unfold merge_item in H.
  destruct (find_same_key (cS Phi) (cA Phi) i) as [j|] eqn:Efind.
  - destruct (unify_items (merge_fuel (cS Phi) j i) (cS Phi) j i);
      [| discriminate].
    destruct (item_subst_eqb s j i && sigma_regular_b s (cA Phi)
              && idempotent_b s); [| discriminate].
    injection H as <-. simpl.
    split; [intros Hin; left; exact Hin |].
    intros [Hin | Hi]; [exact Hin |].
    destruct i as [u|v|f|rt m th|uf]; simpl in Hi; try contradiction.
    destruct Hi as [<- | []].
    destruct (find_same_key_in _ _ _ Efind) as [Hj Hk].
    destruct j as [u'|v'|f'|rt' m' th'|uf']; simpl in Hk; try discriminate.
    apply Nat.eqb_eq in Hk. subst u'.
    unfold tyreqs. apply in_flat_map. exists (ITy u).
    split; [exact Hj | left; reflexivity].
  - injection H as <-. simpl.
    unfold tyreqs. rewrite flat_map_app.
    split.
    + intros Hin. apply in_app_or in Hin. exact Hin.
    + intros Hin. apply in_or_app. exact Hin.
Qed.

Lemma merge_tele_tyreqs : forall D Phi PhiD,
  merge_tele Phi D = Some PhiD ->
  forall t, In t (tyreqs (cA PhiD)) <->
            In t (tyreqs (cA Phi)) \/ In t (tyreqs D).
Proof.
  induction D as [|i D IH]; intros Phi PhiD H t; simpl in H.
  - injection H as <-. simpl. tauto.
  - destruct (merge_item Phi i) as [Phi'|] eqn:Em; [| discriminate].
    rewrite (IH _ _ H t).
    rewrite (merge_item_tyreqs_iff _ _ Em t).
    assert (Hsplit : In t (tyreqs (i :: D))
                     <-> In t (tyreqs [i]) \/ In t (tyreqs D)).
    { unfold tyreqs. simpl. rewrite app_nil_r. apply in_app_iff. }
    rewrite Hsplit. tauto.
Qed.

(* A telescope and its merged context require the same type symbols: dedup
   only drops literal repeats. *)
Lemma tele_ctx_tyreqs : forall D PhiD,
  tele_ctx D = Some PhiD ->
  forall t, In t (tyreqs (cA PhiD)) <-> In t (tyreqs D).
Proof.
  intros D PhiD H t.
  rewrite (merge_tele_tyreqs _ _ H t). simpl. tauto.
Qed.

Lemma wf_tele_tyreqs : forall Sg Phi D PhiD,
  wf_tele Sg Phi D PhiD ->
  forall t, In t (tyreqs (cA PhiD)) <->
            In t (tyreqs (cA Phi)) \/ In t (tyreqs D).
Proof.
  intros Sg Phi D PhiD H t.
  exact (merge_tele_tyreqs _ _ (wf_tele_merge H) t).
Qed.
(* END:merge_item_lemmas *)

(* Telescopes are dependency-closed: a non-type item's own type requirements
   appear among the merged telescope's type items (the paper's [D20]).
   Hint: induction on the wf_tele derivation, carrying the invariant for
   the accumulator; merge_item_cA splits the step, merge_item_tyreqs
   transports the bound; the item_ok premise's sat0 carries availability of
   the telescope's type items directly, and avail_ty_in turns that into
   membership in the accumulator's items. *)
Lemma tele_deps_closed_aux1 : forall Sg Phi i,
  item_ok Sg Phi i ->
  incl (dep_tys Sg i) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi i Hok.
  assert (Hs : forall D, sat0 Sg Phi D -> incl (tyreqs D) (tyreqs (cA Phi))).
  { intros D Hsat t Ht.
    destruct Hsat as [PhiD [Hctx [_ Hav]]].
    rewrite Forall_forall in Hav.
    apply (proj2 (tele_ctx_tyreqs _ Hctx t)) in Ht.
    apply wf_ty_frees_aux3. apply avail_ty_in. exact (Hav t Ht). }
  destruct Hok as [t Hg Hne | v Dv tauv Hg Hsat | f Df S b Hg Hsat
                   | rt m Dm S th Hg Hrt Hsat Hth | uf Du Dr Hg Hfx]; simpl.
  - intros x Hx. destruct Hx.
  - rewrite Hg. exact (Hs Dv Hsat).
  - rewrite Hg. exact (Hs Df Hsat).
  - intros x Hx. destruct Hx.
  - intros x Hx. rewrite Forall_forall in Hfx.
    apply wf_ty_frees_aux3. apply avail_ty_in. exact (Hfx x Hx).
Qed.


Lemma tele_items_regular_aux1 : forall Sg Phi i,
  item_ok Sg Phi i ->
  incl (item_ty_frees i) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi i Hok.
  destruct Hok as [t Hg Hne | v Dv tauv Hg Hsat | f Df S b Hg Hsat
                   | rt m Dm S th Hg Hrt Hsat Hth | uf Du Dr Hg Hfx]; simpl;
    [ intros x Hx; destruct Hx .. | | intros x Hx; destruct Hx ].
  - intros u Hu. apply in_app_or in Hu. destruct Hu as [Hrt' | Hth'].
    + exact (wf_ty_frees Hrt u Hrt').
    + apply in_flat_map in Hth'. destruct Hth' as [p [Hp Hup]].
      rewrite Forall_forall in Hth.
      exact (wf_ty_frees (Hth p Hp) u Hup).
Qed.

Lemma tele_items_regular_aux2 : forall Sg Phi i,
  wf_gsig Sg ->
  item_ok Sg Phi i ->
  incl (method_item_frees Sg i) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi i WF Hok.
  destruct Hok as [t Hg Hne | v Dv tauv Hg Hsat | f Df S b Hg Hsat
                   | rt m Dm Sm th Hg Hrt Hsat Hth | uf Du Dr Hg Hfx]; simpl;
    [ intros x Hx; destruct Hx .. | | intros x Hx; destruct Hx ].
  - intros u Hu. rewrite Hg in Hu.
    apply in_flat_map in Hu. destruct Hu as [rho [Hrho Hu]].
    apply in_map_iff in Hrho. destruct Hrho as [tau [Heq Htau]]. subst rho.
    apply frees_app_subst_bound in Hu.
    destruct Hu as [[p [Hp Hup]] | [Hu HnotThis]].
    + simpl in Hp. destruct Hp as [Hp|[]]. inversion Hp; subst p.
      simpl in Hup. exact (wf_ty_frees Hrt u Hup).
    + apply frees_app_subst_bound in Hu.
      destruct Hu as [[p [Hp Hup]] | [Hu HnotTh]].
      * rewrite Forall_forall in Hth. exact (wf_ty_frees (Hth p Hp) u Hup).
      * destruct (wf_mth_decl WF _ Hg) as [PhiDm [Wt [Hps Hret]]].
        assert (Wtau : wf_ty Sg (add_item (ITy this_sym) PhiDm) tau).
        { apply in_app_or in Htau. destruct Htau as [Htau|[Heq|[]]].
          - rewrite Forall_forall in Hps. exact (Hps tau Htau).
          - subst tau. exact Hret. }
        pose proof (wf_ty_frees Wtau u Hu) as Hu0.
        simpl in Hu0. destruct Hu0 as [Hu0|Hu0].
        -- subst u. exfalso. apply HnotThis. simpl. left. reflexivity.
        -- destruct Hsat as [PhiDm' [Htc [Hdom _]]].
           pose proof (wf_tele_ctx Wt) as Htc'.
           rewrite Htc' in Htc. injection Htc as <-.
           exfalso. apply HnotTh. rewrite Hdom. exact Hu0.
Qed.

Lemma callee_env_agree_aux11_aux1 : forall Sg Phi rt m ap Dm Sm,
  item_ok Sg Phi (IMth rt m ap) ->
  g_mth Sg m = Some (Dm, Sm) ->
  exists PhiDm, tele_ctx Dm = Some PhiDm /\ map fst ap = tyreqs (cA PhiDm).
Proof.
  intros Sg Phi rt m ap Dm Sm Hok Hg.
  inversion Hok; subst.
  match goal with
  | H0 : g_mth Sg m = Some (?DX, ?SX), Hs : sat Sg Phi ap ?DX |- _ =>
      rewrite Hg in H0; injection H0 as HD HS; subst;
      destruct Hs as [PhiDm [Hctx Hsat]];
      exists PhiDm; split; [exact Hctx | exact (proj1 Hsat)]
  end.
Qed.

(* BEGIN:tele_deps_closed *)
Lemma tele_deps_closed : forall Sg Phi D PhiD,
  wf_tele Sg Phi D PhiD ->
  (forall i, In i (cA Phi) -> is_ty_item i = false ->
     incl (dep_tys Sg i) (tyreqs (cA Phi))) ->
  forall i, In i (cA PhiD) -> is_ty_item i = false ->
  incl (dep_tys Sg i) (tyreqs (cA PhiD)).
Proof.
  intros Sg Phi D PhiD H.
  induction H as [Phi | Phi i D Phi' Phi'' Hok Hm Hwf IH]; intros Hacc.
  - exact Hacc.
  - apply IH. intros j Hj Hty.
    pose proof (merge_item_tyreqs _ _ Hm) as Hincl.
    destruct (merge_item_cA _ _ Hm) as [Heq | Heq]; rewrite Heq in Hj.
    + apply in_app_or in Hj. destruct Hj as [Hj | Hj].
      * intros t Ht. apply Hincl. exact (Hacc j Hj Hty t Ht).
      * destruct Hj as [Hji | []]. subst j.
        intros t Ht. apply Hincl.
        exact (tele_deps_closed_aux1 Hok t Ht).
    + intros t Ht. apply Hincl. exact (Hacc j Hj Hty t Ht).
Qed.
(* END:tele_deps_closed *)

(* Well-formed telescopes only mention their own type material.
   Hint: like tele_deps_closed — induction on wf_tele carrying the
   accumulator invariant; item_ok's wf_ty premises plus wf_ty_frees bound
   the method items' receiver and application, and wf_mth_decl plus the
   item's sat premise bound the substituted signature (method_item_frees);
   merge_item_cA / merge_item_tyreqs transport the bound across a step. *)
(* BEGIN:tele_items_regular *)
Lemma tele_items_regular : forall Sg Phi D PhiD,
  wf_gsig Sg ->
  wf_tele Sg Phi D PhiD ->
  (forall i, In i (cA Phi) ->
     incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
          (tyreqs (cA Phi))) ->
  forall i, In i (cA PhiD) ->
  incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
       (tyreqs (cA PhiD)).
Proof.
  intros Sg Phi D PhiD WF H.
  induction H as [Phi | Phi i D Phi' Phi'' Hok Hm Hwf IH]; intros Hacc.
  - exact Hacc.
  - apply IH. intros j Hj.
    pose proof (merge_item_tyreqs _ _ Hm) as Hincl.
    destruct (merge_item_cA _ _ Hm) as [Heq | Heq]; rewrite Heq in Hj.
    + apply in_app_or in Hj. destruct Hj as [Hj | Hj].
      * intros t Ht. apply Hincl. exact (Hacc j Hj t Ht).
      * destruct Hj as [Hji | []]. subst j.
        intros t Ht. apply Hincl.
        apply in_app_or in Ht. destruct Ht as [Ht | Ht].
        -- exact (tele_deps_closed_aux1 Hok t Ht).
        -- apply in_app_or in Ht. destruct Ht as [Ht | Ht].
           ++ exact (tele_items_regular_aux1 Hok t Ht).
           ++ exact (tele_items_regular_aux2 WF Hok t Ht).
    + intros t Ht. apply Hincl. exact (Hacc j Hj t Ht).
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
  destruct i1 as [t1|v1|f1|rt1 m1 th1|u1];
    destruct i2 as [t2|v2|f2|rt2 m2 th2|u2];
    try (simpl in Hit; discriminate); destruct en as [w|f' d'|hf sms];
    try (simpl in Hag;
         repeat match type of Hag with
                | context [match ?x with _ => _ end] => destruct x
                end;
         contradiction).
  - simpl in Hit. injection Hit as Hv. subst v2.
    simpl in Hag |- *. destruct (g_val Sg v1) as [[Dv tauv]|] eqn:E; [|destruct Hag].
    simpl in Hdep. rewrite E in Hdep.
    destruct (wf_val_decl WF _ E) as [PhiDv [Wtv Hwf]].
    apply wf_ty_frees in Hwf. simpl in Hwf.
    rewrite <- (dentry_agree_retype_aux1 g1 g2 tauv); [exact Hag|].
    intros t Ht. apply Hdep.
    destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtv t) (Hwf t Ht)) as [Hin0 | Hin];
      [destruct Hin0 | exact Hin].
  - simpl in Hit. injection Hit as Hf. subst f2.
    simpl in Hag |- *.
    destruct (g_fn Sg f1) as [[[Df Sf] b]|] eqn:E; [|destruct Hag].
    destruct b; [destruct Hag|].
    destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
    destruct b' as [body|]; [|destruct Hag].
    simpl in Hdep. rewrite E in Hdep.
    destruct (wf_fn_decl WF _ E) as [PhiDf [Wtf [Hps [Hrt _]]]].
    destruct Hag as [PhiD' [g' [Htc [Hg [Hc [Habs [Hpar [Hret Hrest]]]]]]]].
    exists PhiD', g'. split; [exact Htc|]. split; [exact Hg|].
    split; [exact Hc|]. split; [exact Habs|].
    assert (Hcnv : forall t, In t (tyreqs (cA PhiDf)) -> In t (tyreqs Df)).
    { intros t Ht.
      destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtf t) Ht) as [Hin0 | Hin];
        [destruct Hin0 | exact Hin]. }
    split.
    + rewrite Hpar. apply map_ext_in. intros tau Htau.
      apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      rewrite Forall_forall in Hps. specialize (Hps tau Htau).
      apply wf_ty_frees in Hps. simpl in Hps. apply Hcnv. apply Hps. exact Ht.
    + split; [|exact Hrest].
      rewrite Hret. apply dentry_agree_retype_aux1. intros t Ht. apply Hdep.
      apply wf_ty_frees in Hrt. simpl in Hrt. apply Hcnv. apply Hrt. exact Ht.
  - simpl in Hit. injection Hit as Hrt Hmm Hth. subst m2.
    simpl in Hag |- *.
    destruct (g_mth Sg m1) as [[Dm Sm]|] eqn:E; [|destruct Hag].
    assert (Hm' : forall tau, In tau (fs_params Sm) \/ tau = fs_ret Sm ->
              interp_m g1 th1 rt1 tau = interp_m g2 th2 rt2 tau)
      by (intros tau HIn; eapply Hm; eauto).
    unfold interp_m in Hm'.
    destruct (g_fn Sg f') as [[[D' S'] b']|] eqn:E'; [|destruct Hag].
    destruct b' as [body|]; [|destruct Hag].
    destruct Hag as [PhiD' [g' [Htc [Hg [Hc [Habs [Hpar [Hret Hrest]]]]]]]].
    exists PhiD', g'. split; [exact Htc|]. split; [exact Hg|].
    split; [exact Hc|]. split; [exact Habs|].
    split.
    + rewrite Hpar, Hrt. f_equal. apply map_ext_in. intros tau Htau.
      apply Hm'. left; exact Htau.
    + split; [|exact Hrest]. rewrite Hret. apply Hm'. right; reflexivity.
  - simpl in Hit. injection Hit as Hu. subst u2.
    simpl in Hag |- *. simpl in Hdep.
    destruct (g_fun Sg u1) as [[[Du Dr] [b|]]|]; try exact Hag.
    destruct (g_fun Sg hf) as [[[Dh Drh] [b|]]|]; try exact Hag.
    destruct Hag as [E1 [Hch [PhiC [g' [H1 [H2 [H3 [H4 [H5 [H6 [H7 [Hfx H8]]]]]]]]]]]].
    split; [exact E1 |]. split; [exact Hch |].
    exists PhiC, g'. repeat (split; [assumption |]).
    split; [| exact H8].
    intros x Hx Hx'. destruct (Hfx x Hx Hx') as [Hin Heq].
    split; [exact Hin |]. rewrite Heq. apply Hdep. exact Hin.
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
Lemma callee_env_agree_aux1 : forall t s tau,
  lookup_subst t s = Some tau -> In (t, tau) s.
Proof.
  intros t s. induction s as [|[u a] r IH]; intros tau H; simpl in H.
  - discriminate.
  - destruct (Nat.eqb t u) eqn:E.
    + apply Nat.eqb_eq in E. subst u. injection H as ->.
      left. reflexivity.
    + right. apply IH. exact H.
Qed.

Lemma callee_env_agree_aux2 : forall t s,
  ~ In t (map fst s) -> lookup_subst t s = None.
Proof.
  intros t s. induction s as [|[u a] r IH]; intro H; simpl.
  - reflexivity.
  - destruct (Nat.eqb t u) eqn:E.
    + apply Nat.eqb_eq in E. subst u. exfalso. apply H. left. reflexivity.
    + apply IH. intro Hin. apply H. right. exact Hin.
Qed.

Lemma callee_env_agree_aux3 : forall i sm,
  In i (map fst sm) -> exists j, lookup_smap i sm = Some j.
Proof.
  intros i sm. induction sm as [|[a b] r IH]; intro Hin; simpl in Hin |- *.
  - contradiction.
  - destruct Hin as [Heq|Hin].
    + subst a. rewrite (proj2 (item_eqb_eq i i) eq_refl).
      exists b. reflexivity.
    + destruct (item_eqb i a); [eexists; reflexivity|].
      apply IH. exact Hin.
Qed.

Lemma callee_env_agree_aux4 : forall s th i j,
  is_ty_item i = false ->
  item_matches s th i j ->
  is_ty_item j = false.
Proof.
  intros s th i j Hty Hm.
  destruct i; destruct j; simpl in *; try discriminate; reflexivity.
Qed.

Lemma callee_env_agree_aux5 :
  forall s1 th1 r1 s2 th2 r2 tau,
    (forall t, In t (ty_frees tau) ->
       interp_m s1 th1 r1 (TAbs t) =
       interp_m s2 th2 r2 (TAbs t)) ->
    interp_m s1 th1 r1 tau = interp_m s2 th2 r2 tau.
Proof.
  intros s1 th1 r1 s2 th2 r2 tau. revert s1 th1 r1 s2 th2 r2.
  induction tau using ty_ind'; intros s1 th1 r1 s2 th2 r2 Hag.
  - reflexivity.
  - apply Hag. simpl. left. reflexivity.
  - unfold interp_m in *. repeat rewrite app_subst_nom. f_equal.
    revert Hag. induction H as [|p r Hp Hr IH]; intro Hag.
    + reflexivity.
    + simpl. f_equal.
      * f_equal. apply Hp. intros u Hu. apply Hag. simpl.
        apply in_or_app. left. exact Hu.
      * apply IH. intros u Hu. apply Hag. simpl.
        apply in_or_app. right. exact Hu.
  - unfold interp_m in *. repeat rewrite app_subst_union. f_equal.
    revert Hag. induction H as [|a r Ha Hr IH]; intro Hag.
    + reflexivity.
    + simpl. f_equal.
      * apply Ha. intros u Hu. apply Hag. simpl.
        apply in_or_app. left. exact Hu.
      * apply IH. intros u Hu. apply Hag. simpl.
        apply in_or_app. right. exact Hu.
Qed.

Lemma callee_env_agree_aux6 : forall g Phi th D i j,
  absorbs g (cS Phi) ->
  incl (item_ty_frees i) (tyreqs D) ->
  item_matches (cS Phi) th i j ->
  app_subst_item (callee_grounding g Phi th D) i =
  app_subst_item g j.
Proof.
  intros g Phi th D i j Habs Hreg Hmatch.
  destruct i as [t|v|f|rt m ap|uf];
    destruct j as [t'|v'|f'|rt' m' ap'|uf'];
    simpl in Hmatch |- *; try discriminate;
    try (injection Hmatch as ->; reflexivity).
  - injection Hmatch as Hrt Hm Hap. subst m'.
    f_equal.
    + rewrite callee_grounding_reads.
      * rewrite <- (absorbs_all Habs rt').
        f_equal. exact Hrt.
      * intros u Hu. apply Hreg. simpl. apply in_or_app. left. exact Hu.
    + transitivity
        (map_snd (app_subst g)
          (map_snd (app_subst (cS Phi)) (map_snd (app_subst th) ap))).
      * unfold map_snd.
        rewrite !map_map. apply map_ext_in. intros [u a] Hin. simpl.
        f_equal. apply callee_grounding_reads.
        intros x Hx. apply Hreg. simpl. apply in_or_app. right.
        apply in_flat_map. exists (u, a). split; assumption.
      * rewrite Hap. unfold map_snd. rewrite !map_map.
        apply map_ext_in. intros [u a] Hin. simpl. f_equal.
        apply absorbs_all. exact Habs.
Qed.

Lemma callee_env_agree_aux7 : forall Sg s th i j,
  item_matches s th i j -> dep_tys Sg i = dep_tys Sg j.
Proof.
  intros Sg s th i j H.
  destruct i; destruct j; simpl in H |- *; try discriminate; try reflexivity;
    injection H as ->; reflexivity.
Qed.

Lemma callee_env_agree_aux8 : forall t a b,
  lookup_subst t (a ++ b) =
  match lookup_subst t a with
  | Some x => Some x
  | None => lookup_subst t b
  end.
Proof.
  intros t a. induction a as [|[u x] r IH]; intro b; simpl.
  - reflexivity.
  - destruct (Nat.eqb t u); [reflexivity|exact (IH b)].
Qed.

Definition callee_env_agree_aux9 (s : subst) (rt : ty) (ap : subst) : subst :=
  map_snd (fun a => app_subst s (app_subst (this_subst rt) a)) ap
  ++ [(this_sym, app_subst s rt)].

Lemma callee_env_agree_aux10 : forall s rt ap tau,
  incl (ty_frees tau) (this_sym :: map fst ap) ->
  interp_m s ap rt tau = app_subst (callee_env_agree_aux9 s rt ap) tau.
Proof.
  intros s rt ap tau. revert s rt ap.
  induction tau using ty_ind'; intros s rt ap Hfree.
  - reflexivity.
  - unfold interp_m, callee_env_agree_aux9. simpl.
    rewrite callee_env_agree_aux8, lookup_map_snd.
    destruct (lookup_subst t ap) as [a|] eqn:E.
    + reflexivity.
    + assert (Ht : t = this_sym).
      { specialize (Hfree t (or_introl eq_refl)).
        destruct Hfree as [Heq|Hin]; [symmetry; exact Heq|].
        exfalso. destruct (subst_chain_aux1 t ap Hin) as [a Ha].
        rewrite E in Ha. discriminate. }
      subst t. simpl. reflexivity.
  - unfold interp_m in *. repeat rewrite app_subst_nom. f_equal.
    revert Hfree. induction H as [|p r Hp Hr IH]; intro Hfree.
    + reflexivity.
    + simpl. f_equal.
      * f_equal. apply Hp. intros u Hu. apply Hfree. simpl.
        apply in_or_app. left. exact Hu.
      * apply IH. intros u Hu. apply Hfree. simpl.
        apply in_or_app. right. exact Hu.
  - unfold interp_m in *. repeat rewrite app_subst_union. f_equal.
    revert Hfree. induction H as [|a r Ha Hr IH]; intro Hfree.
    + reflexivity.
    + simpl. f_equal.
      * apply Ha. intros u Hu. apply Hfree. simpl.
        apply in_or_app. left. exact Hu.
      * apply IH. intros u Hu. apply Hfree. simpl.
        apply in_or_app. right. exact Hu.
Qed.

Lemma callee_env_agree_aux11 : forall Sg Phi0 D PhiD rt m ap Dm Sm,
  wf_tele Sg Phi0 D PhiD ->
  (forall rt' m' ap' Dm' Sm',
     In (IMth rt' m' ap') (cA Phi0) -> g_mth Sg m' = Some (Dm', Sm') ->
     exists PhiDm', tele_ctx Dm' = Some PhiDm'
                    /\ map fst ap' = tyreqs (cA PhiDm')) ->
  In (IMth rt m ap) (cA PhiD) ->
  g_mth Sg m = Some (Dm, Sm) ->
  exists PhiDm, tele_ctx Dm = Some PhiDm /\ map fst ap = tyreqs (cA PhiDm).
Proof.
  intros Sg Phi0 D PhiD rt m ap Dm Sm Hwf. revert rt m ap Dm Sm.
  induction Hwf as [Phi | Phi i D Phi' Phi'' Hok Hm Hwf IH];
    intros rt m ap Dm Sm Hacc Hin Hg.
  - exact (Hacc rt m ap Dm Sm Hin Hg).
  - refine (IH rt m ap Dm Sm _ Hin Hg).
    clear rt m ap Dm Sm Hin Hg.
    intros rt' m' ap' Dm' Sm' Hin' Hg'.
    destruct (merge_item_cA _ _ Hm) as [Heq | Heq]; rewrite Heq in Hin'.
    + apply in_app_or in Hin'. destruct Hin' as [Hin' | Hin'].
      * exact (Hacc rt' m' ap' Dm' Sm' Hin' Hg').
      * destruct Hin' as [Hii | []]. subst i.
        exact (callee_env_agree_aux11_aux1 Hok Hg').
    + exact (Hacc rt' m' ap' Dm' Sm' Hin' Hg').
Qed.

Lemma callee_env_agree_aux12 : forall (f1 f2 : ty -> ty) (a b : subst)
                                      (t : tsym) (x : ty),
  map_snd f1 a = map_snd f2 b ->
  lookup_subst t a = Some x ->
  exists y, lookup_subst t b = Some y /\ f1 x = f2 y.
Proof.
  intros f1 f2 a b t x Heq Hlook.
  assert (Hmapped : lookup_subst t (map_snd f1 a) = Some (f1 x)).
  { rewrite lookup_map_snd, Hlook. reflexivity. }
  rewrite Heq, lookup_map_snd in Hmapped.
  destruct (lookup_subst t b) as [y|] eqn:E; [|discriminate].
  injection Hmapped as Hy. exists y. split; [reflexivity|symmetry; exact Hy].
Qed.

Lemma callee_env_agree_aux13 : forall Sg Phi0 D PhiD,
  wf_tele Sg Phi0 D PhiD ->
  ~ In this_sym (tyreqs (cA Phi0)) ->
  ~ In this_sym (tyreqs (cA PhiD)).
Proof.
  intros Sg Phi0 D PhiD H.
  induction H as [Phi | Phi i D Phi' Phi'' Hok Hm Hwf IH]; intros Hno.
  - exact Hno.
  - apply IH.
    destruct (merge_item_cA _ _ Hm) as [Heq | Heq]; rewrite Heq; [| exact Hno].
    unfold tyreqs. rewrite flat_map_app. intro Hin.
    apply in_app_or in Hin. destruct Hin as [Hin | Hin].
    + exact (Hno Hin).
    + destruct i as [t|v|f|rt m th|uf]; simpl in Hin; try contradiction.
      destruct Hin as [Heq' | []]. subst t.
      inversion Hok; subst. congruence.
Qed.

Lemma callee_env_agree_aux14 : forall rt tau,
  ~ In this_sym (ty_frees tau) ->
  app_subst (this_subst rt) tau = tau.
Proof.
  intros rt tau Hfree.
  rewrite <- app_subst_nil.
  apply app_subst_frees_agree.
  intros t Ht. unfold this_subst. simpl.
  destruct (Nat.eqb t this_sym) eqn:E; [|reflexivity].
  apply Nat.eqb_eq in E. subst t. contradiction.
Qed.

Lemma callee_env_agree : forall Sg g Phi th sm D d PhiD,
  wf_gsig Sg ->
  wf_tele Sg empty_ctx D PhiD ->
  sat Sg Phi th D ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  smap_ok Phi th sm D ->
  ctx_grounded g Phi ->
  denv_agree Sg g (cA Phi) d ->
  exists d', compose_smap d sm = Some d'
    /\ ctx_grounded (callee_grounding g Phi th (cA PhiD)) PhiD
    /\ denv_agree Sg (callee_grounding g Phi th (cA PhiD)) (cA PhiD) d'.
Proof.
  intros Sg g Phi th sm D d PhiD WF Wtel Hsat Hth Hsm Hcg Hd.
  pose proof (wf_tele_ctx Wtel) as Htc.
  destruct Hsat as [PhiD1 [Htc1 [Hthdom [Hsat Hmerge]]]].
  assert (E1 : PhiD1 = PhiD) by congruence. subst PhiD1.
  destruct Hsm as [PhiD2 [Htc2 [Hsmdom Hsm]]].
  assert (E2 : PhiD2 = PhiD) by congruence. subst PhiD2.
  destruct Hcg as [Hground [Habs [Hcovers [Hsigma Hreserved]]]].
  destruct Hd as [Hdcover [Hdagree [HAreg Hdreg]]].
  (* Accumulator premises of the wf_tele-indexed lemmas, trivially satisfied
     at empty_ctx. *)
  assert (Hacc0 : forall i, In i (cA empty_ctx) ->
      incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
           (tyreqs (cA empty_ctx)))
    by (intros i Hi; destruct Hi).
  assert (Hdep0 : forall i, In i (cA empty_ctx) -> is_ty_item i = false ->
      incl (dep_tys Sg i) (tyreqs (cA empty_ctx)))
    by (intros i Hi; destruct Hi).
  assert (Hmth0 : forall rt0 m0 ap0 Dm0 Sm0,
      In (IMth rt0 m0 ap0) (cA empty_ctx) -> g_mth Sg m0 = Some (Dm0, Sm0) ->
      exists PhiDm0, tele_ctx Dm0 = Some PhiDm0
                     /\ map fst ap0 = tyreqs (cA PhiDm0))
    by (intros rt0 m0 ap0 Dm0 Sm0 Hi; destruct Hi).
  assert (Hsmlook :
      Forall (fun p => exists en, lookup_denv (snd p) d = Some en) sm).
  { rewrite Forall_forall. intros [req sat'] Hin.
    rewrite Forall_forall in Hsm. destruct (Hsm (req, sat') Hin) as [Hina Hmatch].
    rewrite Forall_forall in Hdcover.
    specialize (Hdcover sat' Hina).
    destruct Hdcover as [Hty|[en Hen]].
    - assert (Hreqin : In req (filter (fun i => negb (is_ty_item i)) (cA PhiD))).
      { rewrite <- Hsmdom. apply in_map_iff.
        exists (req, sat'). split; [reflexivity|exact Hin]. }
      apply filter_In in Hreqin. destruct Hreqin as [_ Hreqty].
      apply Bool.negb_true_iff in Hreqty.
      pose proof (@callee_env_agree_aux4 (cS Phi) th req sat'
                    Hreqty Hmatch) as Hsatty.
      rewrite Hsatty in Hty. discriminate.
    - exists en. exact Hen. }
  destruct (@compose_smap_defined d sm Hsmlook) as [d' Hcompose].
  exists d'. split; [exact Hcompose|].
  assert (Hcgground : grounding (callee_grounding g Phi th (cA PhiD))).
  { unfold grounding, callee_grounding.
    apply Forall_map. rewrite Forall_forall. intros t Htin.
    assert (Htth : In t (map fst th)) by (rewrite Hthdom; exact Htin).
    destruct (subst_chain_aux1 t th Htth) as [a Ha].
    assert (Hpa : In (t, a) th) by (apply callee_env_agree_aux1; exact Ha).
    rewrite Forall_forall in Hth.
    assert (Hwa := Hth (t, a) Hpa).
    simpl in Hwa.
    simpl. rewrite Ha.
    rewrite (absorbs_all Habs a).
    apply closed_app_subst_ground; [exact Hground|].
    intros u Hu.
    apply wf_ty_frees in Hwa. simpl in Hwa.
    destruct (Hcovers u (Hwa u Hu)) as [b Hb].
    pose proof (@callee_env_agree_aux1 u g b Hb) as Hpair.
    apply (in_map (@fst tsym ty)) in Hpair.
    simpl in Hpair. exact Hpair. }
  assert (HDreserved : ~ In this_sym (tyreqs (cA PhiD))).
  { apply (@callee_env_agree_aux13 Sg empty_ctx D PhiD Wtel).
    intro Hbad. exact Hbad. }
  assert (HsigmaD : sigma_regular (cS PhiD) (cA PhiD)).
  { apply (@tele_sigma_regular Sg empty_ctx D PhiD Wtel).
    split; [intros x Hx; destruct Hx | constructor]. }
  assert (HabsD : absorbs (callee_grounding g Phi th (cA PhiD)) (cS PhiD)).
  { intros t tau Hlk.
    assert (Hpair : In (t, tau) (cS PhiD))
      by (apply callee_env_agree_aux1; exact Hlk).
    assert (Htin : In t (tyreqs (cA PhiD))).
    { apply (proj1 HsigmaD). apply in_map_iff.
      exists (t, tau). split; [reflexivity|exact Hpair]. }
    assert (Htaufree : incl (ty_frees tau) (tyreqs (cA PhiD))).
    { destruct HsigmaD as [_ Hrng]. rewrite Forall_forall in Hrng.
      exact (Hrng (t, tau) Hpair). }
    rewrite (@callee_grounding_lookup g Phi th (cA PhiD) t Htin).
    rewrite (@callee_grounding_reads g Phi th (cA PhiD) tau Htaufree).
    f_equal.
    unfold merge_respects in Hmerge. rewrite Forall_forall in Hmerge.
    exact (f_equal (app_subst g) (Hmerge (t, tau) Hpair)). }
  assert (Hctx : ctx_grounded (callee_grounding g Phi th (cA PhiD)) PhiD).
  { split; [exact Hcgground|].
    split; [exact HabsD|].
    split.
    - intros t Hin. eexists.
      apply callee_grounding_lookup. exact Hin.
    - split; [exact HsigmaD|exact HDreserved]. }
  split; [exact Hctx|].
  assert (Hkeys : map fst d' = filter (fun i => negb (is_ty_item i)) (cA PhiD)).
  { rewrite (@compose_smap_keys d sm d' Hcompose). exact Hsmdom. }
  split.
  - rewrite Forall_forall. intros i Hin.
    destruct (is_ty_item i) eqn:Hty.
    + left. reflexivity.
    + right.
      assert (Hif : In i (filter (fun j => negb (is_ty_item j)) (cA PhiD))).
      { apply filter_In. split; [exact Hin|].
        apply Bool.negb_true_iff. exact Hty. }
      assert (Himap : In i (map fst sm)) by (rewrite Hsmdom; exact Hif).
      destruct (callee_env_agree_aux3 i sm Himap) as [sat' Hlookup].
      destruct (@compose_smap_first d sm d' i sat' Hcompose Hlookup)
        as [en [_ Hen]].
      exists en. exact Hen.
  - split.
    + rewrite Forall_forall. intros [req en] Hin.
      destruct (@compose_smap_entry d sm d' (req, en) Hcompose Hin)
        as [sat' [Hsmin Hden]].
      simpl in Hsmin, Hden.
      rewrite Forall_forall in Hsm.
      destruct (Hsm (req, sat') Hsmin) as [Hsata Hmatch].
      rewrite Forall_forall in Hdagree.
      assert (Hae : dentry_agree Sg g sat' en).
      { apply (Hdagree (sat', en)). apply lookup_denv_in. exact Hden. }
      assert (HreqD : In req (cA PhiD)).
      { assert (Hinmap : In req (map fst d')).
        { apply in_map_iff. exists (req, en). split; [reflexivity|exact Hin]. }
        rewrite Hkeys in Hinmap. apply filter_In in Hinmap. exact (proj1 Hinmap). }
      assert (HreqNon : is_ty_item req = false).
      { assert (Hinmap : In req (map fst d')).
        { apply in_map_iff. exists (req, en). split; [reflexivity|exact Hin]. }
        rewrite Hkeys in Hinmap. apply filter_In in Hinmap.
        apply Bool.negb_true_iff. exact (proj2 Hinmap). }
      eapply dentry_agree_retype; [exact WF | exact Hae | | | ].
      * symmetry. eapply callee_env_agree_aux6; [exact Habs| |exact Hmatch].
        intros u Hu.
        exact (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0 req HreqD u
                 (in_or_app _ _ _ (or_intror (in_or_app _ _ _ (or_introl Hu))))).
      * intros t Hdep.
        assert (Hdep' : In t (dep_tys Sg req)).
        { pose proof (@callee_env_agree_aux7 Sg (cS Phi) th req sat'
                        Hmatch) as Heqdep.
          simpl in Heqdep. rewrite Heqdep. exact Hdep. }
        rewrite callee_grounding_reads.
        -- rewrite Forall_forall in Hsat.
           specialize (Hsat req).
           destruct (Hsat HreqD) as [Hty|[_ Hcoh]].
           ++ rewrite HreqNon in Hty. discriminate.
           ++ rewrite Forall_forall in Hcoh.
              rewrite (Hcoh t Hdep'). symmetry. apply absorbs_all. exact Habs.
        -- intros u Hu. simpl in Hu. destruct Hu as [Heq|Hu]; [subst u|contradiction].
           exact (@tele_deps_closed Sg empty_ctx D PhiD Wtel Hdep0 req
                    HreqD HreqNon t Hdep').
      * intros rt m ap rt' ap' Dm Sm tau Hi Hj Hm Htau.
        simpl in Hi, Hj.
        subst sat'. subst req.
        simpl in Hmatch.
        injection Hmatch as Hrt Hap.
        destruct (@callee_env_agree_aux11 Sg empty_ctx D PhiD rt' m ap' Dm Sm
                    Wtel Hmth0 HreqD Hm) as [PhiDm [HtcDm Hapdom]].
        assert (Hapdom' : map fst ap = tyreqs (cA PhiDm)).
        { pose proof (f_equal (map (@fst tsym ty)) Hap) as E.
          unfold map_snd in E. rewrite !map_map in E. simpl in E.
          rewrite <- Hapdom. symmetry. exact E. }
        assert (Hwf_tau :
          wf_ty Sg (add_item (ITy this_sym) PhiDm) tau).
        { destruct (wf_mth_decl WF _ Hm) as [PhiDm0 [WtDm [Hps Hret]]].
          assert (EDm : PhiDm0 = PhiDm).
          { pose proof (wf_tele_ctx WtDm) as HtcDm0. congruence. }
          subst PhiDm0.
          destruct Htau as [HtauIn|Heq].
          - rewrite Forall_forall in Hps. exact (Hps tau HtauIn).
          - subst tau. exact Hret. }
        assert (Hfree_tau :
          incl (ty_frees tau) (this_sym :: tyreqs (cA PhiDm))).
        { apply wf_ty_frees in Hwf_tau. simpl in Hwf_tau. exact Hwf_tau. }
        apply callee_env_agree_aux5.
        intros t Ht.
        specialize (Hfree_tau t Ht).
        unfold interp_m. simpl.
        destruct (lookup_subst t ap) as [b|] eqn:Hb;
        destruct (lookup_subst t ap') as [a|] eqn:Ha.
        -- assert (Hreqreg :=
               @tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
                 (IMth rt' m ap') HreqD).
           assert (Hafree : incl (ty_frees a) (tyreqs (cA PhiD))).
           { intros u Hu. apply Hreqreg. simpl.
             apply in_or_app. left. apply in_or_app. right.
             apply in_flat_map. exists (t, a). split.
             - apply callee_env_agree_aux1. exact Ha.
             - exact Hu. }
           assert (Hbfree : incl (ty_frees b) (tyreqs (cA Phi))).
           { rewrite Forall_forall in HAreg.
             specialize (HAreg (IMth rt m ap) Hsata).
             intros u Hu. apply HAreg. simpl.
             apply in_or_app. left. apply in_or_app. right.
             apply in_flat_map. exists (t, b). split.
             - apply callee_env_agree_aux1. exact Hb.
             - exact Hu. }
           rewrite (callee_env_agree_aux14 rt b).
           2: { intro Hbad. exact (Hreserved (Hbfree _ Hbad)). }
           rewrite (callee_env_agree_aux14 rt' a).
           2: { intro Hbad. exact (HDreserved (Hafree _ Hbad)). }
           rewrite callee_grounding_reads by exact Hafree.
           assert (Hapeq :
             map_snd
               (fun x => app_subst (cS Phi) (app_subst th x)) ap' =
             map_snd (app_subst (cS Phi)) ap).
           { unfold map_snd in Hap |- *. rewrite map_map in Hap. exact Hap. }
           destruct (@callee_env_agree_aux12
             (fun x => app_subst (cS Phi) (app_subst th x))
             (app_subst (cS Phi))
             ap' ap t a Hapeq Ha) as [b' [Hb' Hab]].
           rewrite Hb in Hb'. injection Hb' as Heq. subst b'.
           rewrite <- (absorbs_all Habs b).
           exact (f_equal (app_subst g) (eq_sym Hab)).
        -- exfalso.
           assert (Hinap : In t (map fst ap)).
           { pose proof (@callee_env_agree_aux1 t ap b Hb) as Hp.
             apply (in_map (@fst tsym ty)) in Hp. simpl in Hp. exact Hp. }
           rewrite Hapdom' in Hinap. rewrite <- Hapdom in Hinap.
           destruct (subst_chain_aux1 t ap' Hinap) as [x Hx].
           rewrite Ha in Hx. discriminate.
        -- exfalso.
           assert (Hinap : In t (map fst ap')).
           { pose proof (@callee_env_agree_aux1 t ap' a Ha) as Hp.
             apply (in_map (@fst tsym ty)) in Hp. simpl in Hp. exact Hp. }
           rewrite Hapdom in Hinap. rewrite <- Hapdom' in Hinap.
           destruct (subst_chain_aux1 t ap Hinap) as [x Hx].
           rewrite Hb in Hx. discriminate.
        -- assert (Htthis : t = this_sym).
           { destruct Hfree_tau as [Heq|HinD]; [symmetry; exact Heq|].
             exfalso. rewrite <- Hapdom' in HinD.
             destruct (subst_chain_aux1 t ap HinD) as [x Hx].
             rewrite Hb in Hx. discriminate. }
           subst t. simpl.
           assert (Hreqreg :=
             @tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
               (IMth rt' m ap') HreqD).
           assert (Hrtfree : incl (ty_frees rt') (tyreqs (cA PhiD))).
           { intros u Hu. apply Hreqreg. simpl.
             apply in_or_app. left. apply in_or_app. left.
             exact Hu. }
           rewrite callee_grounding_reads by exact Hrtfree.
           rewrite Hrt. symmetry. apply absorbs_all. exact Habs.
    + split.
      * rewrite Forall_forall. intros i Hin.
        pose proof (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
                      i Hin) as Hreg.
        simpl in Hreg. exact Hreg.
      * rewrite Forall_forall. intros [i en] Hin.
        assert (HiD : In i (cA PhiD)).
        { assert (Hi : In i (map fst d')).
          { apply in_map_iff. exists (i, en). split; [reflexivity|exact Hin]. }
          rewrite Hkeys in Hi. apply filter_In in Hi. exact (proj1 Hi). }
        pose proof (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
                      i HiD) as Hreg.
        simpl in Hreg. exact Hreg.
Qed.
(* END:callee_env_agree *)

(* --- A lean induction principle for typing: only T-Sub re-types the same
   term, so only it needs an induction hypothesis. ------------------------- *)

Section HasTyIndSub.
  Variable Sg : gsig.
  Variable P : ctx -> tenv -> expr -> ty -> Prop.

  Variable HVar : forall Phi G x tau,
      lookup_tenv x G = Some tau -> P Phi G (EVar x) tau.
  Variable HUnit : forall Phi G, P Phi G EUnit TUnit.
  Variable HSub : forall Phi G e tau' tau,
      has_ty Sg Phi G e tau' -> P Phi G e tau' -> subty Phi tau' tau ->
      wf_ty Sg Phi tau ->
      P Phi G e tau.
  Variable HVal : forall Phi G v Dv tauv,
      g_val Sg v = Some (Dv, tauv) -> avail Phi (IVal v) ->
      P Phi G (EVl v) (app_subst (cS Phi) tauv).
  Variable HTag : forall Phi G n D tau0 th e,
      g_tag Sg n = Some (D, tau0) -> sat Sg Phi th D ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
      P Phi G (ETag n th e) (app_subst (cS Phi) (TNom n th)).
  Variable HNeed : forall Phi G f Df S args,
      g_fn Sg f = Some (Df, S, None) -> avail Phi (IFn f) ->
      Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau))
              args (fs_params S) ->
      P Phi G (ECallA f args) (app_subst (cS Phi) (fs_ret S)).
  Variable HCall : forall Phi G f Df S body th sm args,
      g_fn Sg f = Some (Df, S, Some body) ->
      sat Sg Phi th Df ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Df ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
              args (fs_params S) ->
      P Phi G (ECallD f th sm args)
        (app_subst (cS Phi) (app_subst th (fs_ret S))).
  Variable HMeth : forall Phi G e0 tau0 m Dm S rt0 th0 args,
      g_mth Sg m = Some (Dm, S) ->
      has_ty Sg Phi G e0 tau0 ->
      In (IMth rt0 m th0) (cA Phi) ->
      app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
      (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                  app_subst_item (cS Phi) i1
                  = app_subst_item (cS Phi) (IMth rt0 m th0)) ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e
                        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
              args (fs_params S) ->
      P Phi G (EMeth e0 m (IMth rt0 m th0) args)
        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S)).
  Variable HMatch : forall Phi G e0 tau0 ms arms tau,
      has_ty Sg Phi G e0 tau0 ->
      wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
      members (app_subst (cS Phi) tau0) = Some ms ->
      NoDup (map member_head ms) ->
      Forall2 (arm_matches Sg) ms arms ->
      Forall2 (fun mem arm =>
                 has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                        (arm_body arm) tau) ms arms ->
      P Phi G (EMatch e0 arms) tau.
  Variable HLet : forall Phi G x e1 e2 tau1 tau,
      has_ty Sg Phi G e1 tau1 ->
      has_ty Sg Phi ((x, tau1) :: G) e2 tau ->
      P Phi G (ELet x e1 e2) tau.
  Variable HBindTy : forall Phi G t tau' e tau,
      g_ty Sg t = true ->
      t <> this_sym ->
      ~ In (ITy t) (cA Phi) ->
      wf_ty Sg Phi tau' ->
      has_ty Sg (bind_ty t tau' Phi) G e tau ->
      wf_ty Sg Phi tau ->
      P Phi G (EBindTy t tau' e) tau.
  Variable HBindVal : forall Phi G v Dv tauv e1 e tau,
      g_val Sg v = Some (Dv, tauv) ->
      sat0 Sg Phi Dv ->
      has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
      has_ty Sg (add_item (IVal v) Phi) G e tau ->
      P Phi G (EBindVal v e1 e) tau.
  Variable HBindFn : forall Phi G f Df S f' D' S' body sm e tau,
      g_fn Sg f = Some (Df, S, None) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi Df ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      sig_eq (cS Phi) S' S ->
      has_ty Sg (add_item (IFn f) Phi) G e tau ->
      P Phi G (EBindFn f f' sm e) tau.
  Variable HBindMth : forall Phi G tauk m Dm S th f' D' S' body sm
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
  Variable HBindFun : forall Phi G u Du Dr h Dh bs sm smc e tau,
      g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg h = Some (Dh, Dr, Some bs) ->
      fun_link Sg Dh sm Du ->
      cap_ok Sg Phi Du Dr u h smc ->
      Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
      has_ty Sg (add_item (IFun u) Phi) G e tau ->
      P Phi G (EBindFun u h sm smc e) tau.
  Variable HBindFunA : forall Phi G u Du Dr w Dw sm e tau,
      g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg w = Some (Dw, Dr, None) ->
      avail Phi (IFun w) ->
      fun_link Sg Dw sm Du ->
      incl (g_fixed Sg w) (g_fixed Sg u) ->
      Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
      has_ty Sg (add_item (IFun u) Phi) G e tau ->
      P Phi G (EBindFunA u w sm e) tau.
  Variable HApply : forall Phi G u Du Dr th sm e tau,
      g_fun Sg u = Some (Du, Dr, None) ->
      avail Phi (IFun u) ->
      sat Sg Phi th Du ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Du ->
      fixed_ok Sg Phi u th ->
      res_coherent Sg Phi th Dr ->
      Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
      has_ty Sg (open_res (app_tele th Dr) Phi) G e tau ->
      Forall (fun t => ~ In t (ty_frees tau)) (tyreqs Dr) ->
      P Phi G (EApply u th sm e) tau.


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
    - eapply HBindFun; eassumption.
    - eapply HBindFunA; eassumption.
    - eapply HApply; eassumption.
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
  wf_ty Sg Phi tau ->
  incl (ty_frees tau) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi G e tau WF HT Hwf.
  exact (wf_ty_frees Hwf).
Qed.
(* END:has_ty_frees *)

Lemma has_ty_frees_success_aux1 : forall s tau X,
  incl (ty_frees tau) X ->
  Forall (fun p => incl (ty_frees (snd p)) X) s ->
  incl (ty_frees (app_subst s tau)) X.
Proof.
  intros s tau X Htau Hs u Hu.
  apply frees_app_subst_bound in Hu.
  destruct Hu as [[p [Hp Hu]] | [Hu _]].
  - rewrite Forall_forall in Hs. exact (Hs p Hp u Hu).
  - exact (Htau u Hu).
Qed.

Lemma has_ty_frees_success_aux2 : forall s tau X,
  incl (ty_frees tau) (map fst s) ->
  Forall (fun p => incl (ty_frees (snd p)) X) s ->
  incl (ty_frees (app_subst s tau)) X.
Proof.
  intros s tau X Hdom Hs u Hu.
  apply frees_app_subst_bound in Hu.
  destruct Hu as [[p [Hp Hu]] | [Hu Hmiss]].
  - rewrite Forall_forall in Hs. exact (Hs p Hp u Hu).
  - exfalso. apply Hmiss. exact (Hdom u Hu).
Qed.

Lemma has_ty_frees_success_aux3 : forall A B (R : A -> B -> Prop) xs ys y,
  Forall2 R xs ys -> In y ys -> exists x, In x xs /\ R x y.
Proof.
  intros A B R xs ys y H.
  induction H as [|x y0 xs ys Hxy Hrest IH]; intros Hin.
  - contradiction.
  - destruct Hin as [Heq|Hin].
    + subst y0. exists x. split; [left; reflexivity|exact Hxy].
    + destruct (IH Hin) as [x' [Hin' HR]].
      exists x'. split; [right; exact Hin'|exact HR].
Qed.

Lemma has_ty_frees_success_aux4 : forall Sg Phi tau0 ms mem,
  wf_gsig Sg ->
  wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
  members (app_subst (cS Phi) tau0) = Some ms ->
  In mem ms ->
  incl (ty_frees (member_payload Sg mem)) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi tau0 ms mem WF Hwf Hmembers Hin.
  assert (Hpayload : forall n th,
      wf_ty Sg Phi (TNom n th) ->
      incl (ty_frees (member_payload Sg (TNom n th)))
           (tyreqs (cA Phi))).
  { intros n th Hn.
    inversion Hn as [| |n' D payload th' Htag Hsat Hwfth|]; subst.
    cbn [member_payload]. rewrite Htag.
    destruct (wf_tag_decl WF n Htag) as [PhiD [Wt Hp]].
    destruct Hsat as [PhiD' [Htc [Hdom _]]].
    pose proof (wf_tele_ctx Wt) as Htc'.
    rewrite Htc' in Htc. injection Htc as <-.
    apply has_ty_frees_success_aux2 with (s := th).
    - rewrite Hdom. exact (wf_ty_frees Hp).
    - eapply Forall_impl; [|exact Hwfth].
      intros p Hwp. exact (wf_ty_frees Hwp). }
  remember (app_subst (cS Phi) tau0) as T eqn:HT in Hwf, Hmembers.
  destruct T as [|t|n th|ms0]; simpl in Hmembers; try discriminate.
  - injection Hmembers as Hms. subst ms.
    simpl in Hin. destruct Hin as [Heq|[]]. subst mem.
    exact (Hpayload n th Hwf).
  - injection Hmembers as Hms. subst ms.
    inversion Hwf as [| | |ms' Hwfms Hnomms Hnd]; subst.
    rewrite Forall_forall in Hwfms.
    pose proof (Hwfms mem Hin) as Hmemwf.
    rewrite Forall_forall in Hnomms.
    destruct (Hnomms mem Hin) as [n [th Heq]]. subst mem.
    exact (Hpayload n th Hmemwf).
Qed.

Lemma has_ty_frees_success_aux5 : forall rt tau u,
  u <> this_sym ->
  In u (ty_frees tau) ->
  In u (ty_frees (app_subst (this_subst rt) tau)).
Proof.
  intros rt tau u Hneq.
  induction tau using ty_ind'; intros Hu.
  - contradiction.
  - simpl in Hu. destruct Hu as [Heq|[]]. subst t.
    simpl. destruct (Nat.eqb u this_sym) eqn:E.
    + apply Nat.eqb_eq in E. contradiction.
    + left. reflexivity.
  - simpl in Hu |- *.
    apply in_flat_map in Hu. destruct Hu as [p [Hp Hu]].
    apply in_flat_map. exists (fst p, app_subst (this_subst rt) (snd p)).
    split.
    + unfold map_snd. apply in_map_iff. exists p.
      destruct p. simpl in *. split; [reflexivity|exact Hp].
    + rewrite Forall_forall in H.
      exact (H p Hp Hu).
  - simpl in Hu |- *.
    apply in_flat_map in Hu. destruct Hu as [a [Ha Hu]].
    apply in_flat_map. exists (app_subst (this_subst rt) a).
    split.
    + apply in_map. exact Ha.
    + rewrite Forall_forall in H. exact (H a Ha Hu).
Qed.

Lemma has_ty_frees_success_aux5b : forall s tau u,
  lookup_subst u s = None ->
  In u (ty_frees tau) ->
  In u (ty_frees (app_subst s tau)).
Proof.
  intros s tau u Hlook.
  induction tau using ty_ind'; intros Hu.
  - contradiction.
  - simpl in Hu. destruct Hu as [Heq|[]]. subst t.
    simpl. rewrite Hlook. left. reflexivity.
  - simpl in Hu |- *.
    apply in_flat_map in Hu. destruct Hu as [p [Hp Hu]].
    apply in_flat_map. exists (fst p, app_subst s (snd p)).
    split.
    + unfold map_snd. apply in_map_iff. exists p.
      destruct p. simpl in *. split; [reflexivity|exact Hp].
    + rewrite Forall_forall in H. exact (H p Hp Hu).
  - simpl in Hu |- *.
    apply in_flat_map in Hu. destruct Hu as [a [Ha Hu]].
    apply in_flat_map. exists (app_subst s a).
    split.
    + apply in_map. exact Ha.
    + rewrite Forall_forall in H. exact (H a Ha Hu).
Qed.

Lemma has_ty_frees_success_aux6 : forall Sg Phi rt m th Dm Sm tau R,
  wf_gsig Sg ->
  g_mth Sg m = Some (Dm, Sm) ->
  In tau (fs_params Sm) \/ tau = fs_ret Sm ->
  incl (ty_frees R) (tyreqs (cA Phi)) ->
  (forall i, In i (cA Phi) ->
     incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
          (tyreqs (cA Phi))) ->
  In (IMth rt m th) (cA Phi) ->
  sigma_regular (cS Phi) (cA Phi) ->
  incl (ty_frees (interp_m (cS Phi) th R tau))
       (tyreqs (cA Phi)).
Proof.
  intros Sg Phi rt m th Dm Sm tau R WF Hm Htau HR Hitems Hin Hsig.
  destruct Hsig as [_ Hranges].
  assert (Hbase :
      incl (ty_frees (app_subst th tau)) (this_sym :: tyreqs (cA Phi))).
  { intros u Hu. apply frees_app_subst_bound in Hu.
    destruct Hu as [[p [Hp Hu]]|[Hu Hmiss]].
    - right.
      specialize (Hitems (IMth rt m th) Hin).
      apply Hitems. simpl.
      apply in_or_app. left. apply in_or_app. right.
      apply in_flat_map. exists p. split; assumption.
    - destruct (Nat.eq_dec u this_sym) as [Heq|Hneq].
      + left. symmetry. exact Heq.
      + right.
        specialize (Hitems (IMth rt m th) Hin).
        apply Hitems. simpl. apply in_or_app. right.
        unfold method_item_frees. rewrite Hm.
        apply in_flat_map.
        exists (app_subst (this_subst rt) (app_subst th tau)).
        split.
        * apply in_map_iff. exists tau. split; [reflexivity|].
          apply in_or_app. destruct Htau as [Hp|Heq].
          -- left. exact Hp.
          -- right. simpl. left. symmetry. exact Heq.
        * apply has_ty_frees_success_aux5; [exact Hneq|].
          apply has_ty_frees_success_aux5b; [|exact Hu].
          apply callee_env_agree_aux2. exact Hmiss. }
  unfold interp_m.
  apply has_ty_frees_success_aux1.
  - intros u Hu. apply frees_app_subst_bound in Hu.
    destruct Hu as [[p [Hp Hu]]|[Hu Hmiss]].
    + simpl in Hp. destruct Hp as [Hp|[]]. inversion Hp; subst p.
      simpl in Hu. exact (HR u Hu).
    + specialize (Hbase u Hu). destruct Hbase as [Heq|HuX].
      * exfalso. apply Hmiss. subst u. simpl. left. reflexivity.
      * exact HuX.
  - exact Hranges.
Qed.

(* The method-item case of the frees discipline, with OkMth's premises
   spelled out: an instance of tele_items_regular_aux2 above. *)
Lemma has_ty_frees_success_aux7 : forall Sg Phi rt m Dm Sm th,
  wf_gsig Sg ->
  g_mth Sg m = Some (Dm, Sm) ->
  wf_ty Sg Phi rt ->
  sat Sg Phi th Dm ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  incl (method_item_frees Sg (IMth rt m th))
       (tyreqs (cA Phi)).
Proof.
  intros Sg Phi rt m Dm Sm th WF Hm Hrt Hsat Hth.
  exact (tele_items_regular_aux2 WF (@OkMth Sg Phi rt m Dm Sm th Hm Hrt Hsat Hth)).
Qed.

(* --- Functor declarations: what a result telescope mentions [D78]. ------- *)

Lemma tyreqs_app : forall A B, tyreqs (A ++ B) = tyreqs A ++ tyreqs B.
Proof. intros A B. unfold tyreqs. apply flat_map_app. Qed.

(* What a satisfied telescope requires is in force. *)
Lemma sat0_tyreqs_incl : forall Sg Phi D PhiD,
  sat0 Sg Phi D -> tele_ctx D = Some PhiD ->
  incl (tyreqs (cA PhiD)) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi D PhiD [PhiD' [Htc [_ Hav]]] Htc' t Ht.
  rewrite Htc' in Htc. injection Htc as <-.
  rewrite Forall_forall in Hav.
  apply wf_ty_frees_aux3. apply avail_ty_in. exact (Hav t Ht).
Qed.

(* A functor's arguments-then-result telescope only mentions its own type
   material (tele_items_regular, twice). *)
Lemma fun_decl_regular : forall Sg u Du Dr,
  wf_gsig Sg ->
  g_fun Sg u = Some (Du, Dr, None) ->
  exists PhiD, tele_ctx Du = Some PhiD
  /\ wf_tele Sg empty_ctx Du PhiD
  /\ wf_tele Sg PhiD Dr (mkCtx (cA PhiD ++ Dr) (cS PhiD))
  /\ (forall i, In i (cA PhiD ++ Dr) ->
        incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
             (tyreqs (cA PhiD ++ Dr))).
Proof.
  intros Sg u Du Dr WF Hu.
  destruct (wf_fun_decl WF _ Hu) as [PhiD [Wa [Wr _]]].
  destruct (Wr eq_refl) as [_ [_ [_ Wr']]].
  exists PhiD. split; [exact (wf_tele_ctx Wa) |]. split; [exact Wa |].
  split; [exact Wr' |].
  intros i Hi.
  apply (@tele_items_regular Sg PhiD Dr _ WF Wr'); [| exact Hi].
  apply (@tele_items_regular Sg empty_ctx Du PhiD WF Wa).
  intros j Hj. destruct Hj.
Qed.

(* Rule A: a functor with a body — its linked arguments, its captured
   inputs and pins, the body's context, and the regularity of the result
   formed over that context. *)
Lemma fun_body_regular : forall Sg h Dh Dr bs,
  wf_gsig Sg ->
  g_fun Sg h = Some (Dh, Dr, Some bs) ->
  exists PhiD PhiC, tele_ctx Dh = Some PhiD
  /\ wf_tele Sg empty_ctx Dh PhiD
  /\ wf_tele Sg empty_ctx (fst (g_cap Sg h)) PhiC
  /\ pins_ok (snd (g_cap Sg h)) PhiC
  /\ wf_tele Sg (fun_ctx PhiD PhiC (snd (g_cap Sg h))) Dr
       (mkCtx (cA (fun_ctx PhiD PhiC (snd (g_cap Sg h))) ++ Dr)
              (cS (fun_ctx PhiD PhiC (snd (g_cap Sg h)))))
  /\ (exists PhiB, binds_ty Sg (fun_ctx PhiD PhiC (snd (g_cap Sg h))) bs PhiB
        /\ Forall (fun i => In i (cA PhiB)) Dr)
  /\ (forall i, In i (cA PhiD ++ cA PhiC ++ Dr) ->
        incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
             (tyreqs (cA PhiD ++ cA PhiC ++ Dr))).
Proof.
  intros Sg h Dh Dr bs WF Hh.
  destruct (wf_fun_decl WF _ Hh) as [PhiD [Wa [_ Wb]]].
  destruct (Wb bs eq_refl) as [PhiC [Wc [Hp [Wr Hbody]]]].
  exists PhiD, PhiC. split; [exact (wf_tele_ctx Wa) |]. split; [exact Wa |].
  split; [exact Wc |]. split; [exact Hp |]. split; [exact Wr |].
  split; [exact Hbody |].
  intros i Hi. rewrite app_assoc in Hi. rewrite (app_assoc (cA PhiD)).
  pose proof (@tele_items_regular Sg _ Dr _ WF Wr) as R. simpl in R.
  refine (R _ i Hi).
  intros j Hj. rewrite tyreqs_app.
  apply in_app_or in Hj. destruct Hj as [Hj | Hj]; intros x Hx; apply in_or_app.
  - left. exact (@tele_items_regular Sg empty_ctx Dh PhiD WF Wa
                   (fun k Hk => match Hk with end) j Hj x Hx).
  - right. exact (@tele_items_regular Sg empty_ctx _ PhiC WF Wc
                   (fun k Hk => match Hk with end) j Hj x Hx).
Qed.

(* The items of an application's result are regular in the context the
   application opens. *)
Lemma open_res_regular : forall Sg Phi u Du Dr,
  wf_gsig Sg ->
  g_fun Sg u = Some (Du, Dr, None) ->
  sat0 Sg Phi Du ->
  forall i, In i Dr ->
  incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
       (tyreqs (cA (open_res Dr Phi))).
Proof.
  intros Sg Phi u Du Dr WF Hu Hsat i Hi x Hx.
  destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiD [Htc [_ [_ Hreg]]]].

  assert (Hin : In i (cA PhiD ++ Dr)) by (apply in_or_app; right; exact Hi).
  specialize (Hreg i Hin x Hx).
  rewrite tyreqs_app in Hreg. simpl. rewrite tyreqs_app.
  apply in_app_or in Hreg. destruct Hreg as [Hx' | Hx'].
  - apply in_or_app. right. exact (sat0_tyreqs_incl Hsat Htc x Hx').
  - apply in_or_app. left. exact Hx'.
Qed.

Lemma sigma_regular_weaken : forall s A B,
  sigma_regular s A -> incl (tyreqs A) (tyreqs B) -> sigma_regular s B.
Proof.
  intros s A B [Hdom Hran] Hincl. split.
  - intros t Ht. apply Hincl. apply Hdom. exact Ht.
  - eapply Forall_impl; [| exact Hran].
    intros p Hp t Ht. apply Hincl. apply Hp. exact Ht.
Qed.

Lemma open_res_tyreqs : forall Dr Phi,
  incl (tyreqs (cA Phi)) (tyreqs (cA (open_res Dr Phi))).
Proof.
  intros Dr Phi t Ht. simpl. rewrite tyreqs_app.
  apply in_or_app. right. exact Ht.
Qed.

Lemma wf_tele_item_ok : forall Sg Phi D Phi',
  wf_tele Sg Phi D Phi' -> forall i, In i D -> exists Phi0, item_ok Sg Phi0 i.
Proof.
  intros Sg Phi D Phi' H.
  induction H as [Phi | Phi i D Phi' Phi'' Hok _ _ IH]; intros j Hj.
  - destruct Hj.
  - destruct Hj as [<- | Hj]; [exists Phi; exact Hok | exact (IH j Hj)].
Qed.

(* ---- Rule A: results read at θ. ------------------------------------------ *)

Lemma tyreqs_app_tele : forall th D, tyreqs (app_tele th D) = tyreqs D.
Proof.
  intros th D. induction D as [| i r IH]; [reflexivity |].
  destruct i; simpl; unfold tyreqs in *; simpl; rewrite ?IH; reflexivity.
Qed.

Lemma in_app_tele : forall th D i, In i D -> In (app_subst_item th i) (app_tele th D).
Proof. intros th D i H. unfold app_tele. apply in_map. exact H. Qed.

(* A well-formed type's σ-image mentions only symbols in force. *)
Lemma frees_sigma_wf : forall Sg Phi tau,
  wf_ty Sg Phi tau -> sigma_regular (cS Phi) (cA Phi) ->
  incl (ty_frees (app_subst (cS Phi) tau)) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi tau Hwf [_ Hran] x Hx.
  destruct (frees_app_subst_bound _ _ _ Hx) as [[p [Hp Hxp]] | [Hxt _]].
  - rewrite Forall_forall in Hran. exact (Hran p Hp x Hxp).
  - exact (wf_ty_frees Hwf x Hxt).
Qed.

(* What an item read at θ reads: what it depends on, θ's ranges, or what
   the item itself reads outside θ's domain. *)
Lemma reads_inst : forall Sg Phi0 th i x,
  wf_gsig Sg -> item_ok Sg Phi0 i ->
  In x (dep_tys Sg (app_subst_item th i) ++ item_ty_frees (app_subst_item th i)
        ++ method_item_frees Sg (app_subst_item th i)) ->
  In x (dep_tys Sg i)
  \/ (exists p, In p th /\ In x (ty_frees (snd p)))
  \/ (In x (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
      /\ ~ In x (map fst th)).
Proof.
  intros Sg Phi0 th i x WF Hok Hx.
  assert (Hb : forall tau, In x (ty_frees (app_subst th tau)) ->
                 (exists p, In p th /\ In x (ty_frees (snd p)))
                 \/ (In x (ty_frees tau) /\ ~ In x (map fst th))).
  { intros tau H. exact (frees_app_subst_bound _ _ _ H). }
  destruct i as [t|v|f|rt m ap|w]; simpl in Hx |- *;
    try (left; rewrite app_nil_r in Hx; exact Hx).
  - destruct Hx.
  - right. apply in_app_or in Hx. destruct Hx as [Hx | Hx].
    + (* the item's own type material *)
      apply in_app_or in Hx. destruct Hx as [Hx | Hx].
      * destruct (Hb rt Hx) as [H | [H1 H2]]; [left; exact H |].
        right. split; [| exact H2]. apply in_or_app. left. apply in_or_app. left. exact H1.
      * unfold map_snd in Hx. rewrite flat_map_concat_map, map_map in Hx.
        rewrite <- flat_map_concat_map in Hx.
        apply in_flat_map in Hx. destruct Hx as [p [Hp Hx]]. simpl in Hx.
        destruct (Hb (snd p) Hx) as [H | [H1 H2]]; [left; exact H |].
        right. split; [| exact H2]. apply in_or_app. left. apply in_or_app. right.
        apply in_flat_map. exists p. split; assumption.
    + (* the method signature read at the item *)
      destruct (g_mth Sg m) as [[Dm Sm]|] eqn:Hm; [| destruct Hx].
      apply in_flat_map in Hx. destruct Hx as [rho [Hrho Hx]].
      apply in_map_iff in Hrho. destruct Hrho as [tau [<- Htau]].
      apply frees_app_subst_bound in Hx.
      destruct Hx as [[p [Hp Hxp]] | [Hx HnotThis]].
      * simpl in Hp. destruct Hp as [<- | []]. simpl in Hxp.
        destruct (Hb rt Hxp) as [H | [H1 H2]]; [left; exact H |].
        right. split; [| exact H2]. apply in_or_app. left. apply in_or_app. left. exact H1.
      * apply frees_app_subst_bound in Hx.
        destruct Hx as [[p [Hp Hxp]] | [Hx HnotTh]].
        -- unfold map_snd in Hp. apply in_map_iff in Hp.
           destruct Hp as [q [<- Hq]]. simpl in Hxp.
           destruct (Hb (snd q) Hxp) as [H | [H1 H2]]; [left; exact H |].
           right. split; [| exact H2]. apply in_or_app. left. apply in_or_app.
           right. apply in_flat_map. exists q. split; assumption.
        -- exfalso.
           inversion Hok; subst.
           match goal with
           | H0 : g_mth Sg m = Some (?DX, ?SX), Hs : sat Sg _ ap ?DX |- _ =>
               rewrite Hm in H0; injection H0 as <- <-;
               destruct Hs as [PhiDm' [Htc [Hdom _]]]
           end.
           destruct (wf_mth_decl WF _ Hm) as [PhiDm [Wt [Hps Hret]]].
           rewrite (wf_tele_ctx Wt) in Htc. injection Htc as <-.
           assert (Wtau : wf_ty Sg (add_item (ITy this_sym) PhiDm) tau).
           { apply in_app_or in Htau. destruct Htau as [Htau|[Heq|[]]].
             - rewrite Forall_forall in Hps. exact (Hps tau Htau).
             - subst tau. exact Hret. }
           pose proof (wf_ty_frees Wtau x Hx) as Hx0. simpl in Hx0.
           destruct Hx0 as [Hx0 | Hx0].
           ++ subst x. apply HnotThis. simpl. left. reflexivity.
           ++ apply HnotTh. unfold map_snd. rewrite map_map. simpl.
              rewrite (map_ext (fun x0 : tsym * ty => fst x0) fst) by reflexivity.
              rewrite Hdom. exact Hx0.
Qed.

(* The items of an application at θ's result are regular in the context it
   opens — given result coherence, which keeps the symbols a non-method
   result item depends on in force. *)
Lemma open_res_th_regular : forall Sg Phi u Du Dr th,
  wf_gsig Sg ->
  g_fun Sg u = Some (Du, Dr, None) ->
  sat Sg Phi th Du ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  res_coherent Sg Phi th Dr ->
  sigma_regular (cS Phi) (cA Phi) ->
  forall i, In i (app_tele th Dr) ->
  incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
       (tyreqs (cA (open_res (app_tele th Dr) Phi))).
Proof.
  intros Sg Phi u Du Dr th WF Hu Hsat Hwf Hcoh Hsig i' Hi' x Hx.
  unfold app_tele in Hi'. apply in_map_iff in Hi'. destruct Hi' as [i [<- Hi]].
  destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiD [Htc [_ [Wr Hreg]]]].
  destruct (wf_tele_item_ok Wr i Hi) as [Phi0 Hok].
  assert (Hin : In i (cA PhiD ++ Dr)) by (apply in_or_app; right; exact Hi).
  destruct Hsat as [PhiD' [Htc' [Hdom _]]].
  rewrite Htc in Htc'. injection Htc' as <-.
  simpl. rewrite tyreqs_app, tyreqs_app_tele.
  assert (Hout : forall y, In y (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
                   ~ In y (map fst th) -> In y (tyreqs Dr)).
  { intros y Hy Hn. specialize (Hreg i Hin y Hy). rewrite tyreqs_app in Hreg.
    apply in_app_or in Hreg. destruct Hreg as [H | H]; [| exact H].
    exfalso. apply Hn. rewrite Hdom. exact H. }
  destruct (@reads_inst Sg Phi0 th i x WF Hok Hx) as [Hd | [[p [Hp Hxp]] | [Hr Hn]]].
  - destruct (in_dec Nat.eq_dec x (map fst th)) as [Hth | Hth].
    + apply in_or_app. right.
      unfold res_coherent in Hcoh. rewrite Forall_forall in Hcoh. specialize (Hcoh i Hi).
      rewrite Forall_forall in Hcoh. specialize (Hcoh x Hd).
      destruct Hsig as [Hsd Hsr].
      destruct (in_dec Nat.eq_dec x (map fst (cS Phi))) as [Hs | Hs];
        [exact (Hsd x Hs) |].
      assert (E : app_subst (cS Phi) (TAbs x) = TAbs x).
      { simpl. destruct (lookup_subst x (cS Phi)) eqn:El; [| reflexivity].
        exfalso. apply Hs. clear -El. induction (cS Phi) as [|[a b] r IH];
          simpl in El; [discriminate |].
        destruct (Nat.eqb x a) eqn:E; [apply Nat.eqb_eq in E; left; symmetry; exact E |].
        right. exact (IH El). }
      rewrite E in Hcoh.
      destruct (subst_chain_aux1 x th Hth) as [tx Htx].
      simpl in Hcoh. rewrite Htx in Hcoh.
      assert (Hwx : wf_ty Sg Phi tx).
      { rewrite Forall_forall in Hwf. apply (Hwf (x, tx)).
        apply callee_env_agree_aux1. exact Htx. }
      apply (frees_sigma_wf Hwx (conj Hsd Hsr)). rewrite Hcoh. simpl. left. reflexivity.
    + apply in_or_app. left. apply Hout; [apply in_or_app; left; exact Hd | exact Hth].
  - apply in_or_app. right. rewrite Forall_forall in Hwf.
    exact (wf_ty_frees (Hwf p Hp) x Hxp).
  - apply in_or_app. left. exact (Hout x Hr Hn).
Qed.


Lemma has_ty_frees_success : forall Sg Phi G e tau,
  wf_gsig Sg ->
  has_ty Sg Phi G e tau ->
  sigma_regular (cS Phi) (cA Phi) ->
  (forall i, In i (cA Phi) ->
     incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
          (tyreqs (cA Phi))) ->
  (forall x tx, lookup_tenv x G = Some tx ->
     incl (ty_frees tx) (tyreqs (cA Phi))) ->
  forall fuel d ge w, interp fuel Sg d ge e = Ok w ->
  incl (ty_frees tau) (tyreqs (cA Phi)).
Proof.
  intros Sg Phi G e tau WF HT Hsig Hitems Hvars fuel d ge w E.
  revert Phi G e tau d ge w HT Hsig Hitems Hvars E.
  induction fuel as [|k IH]; intros Phi G e tau d ge w HT Hsig Hitems Hvars E.
  - simpl in E. discriminate.
  - destruct e as [x| |v|n th e0|f th sm args|f args
                   |e0 m prov args|e0 arms|x e1 e2|t tau' e0
                   |v e1 e2|f f' sm e0|rt m th f' sm e0
                   |u h sm smc e0|u u' sm e0|u th sm e0].
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      apply Hvars with (x := x). assumption.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      intros u Hu. contradiction.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      match goal with H : g_val Sg v = Some _ |- _ => rename H into Hg end.
      match goal with
      | H : avail Phi (IVal v) |- _ => rename H into Hav
      end.
      destruct (wf_val_decl WF _ Hg) as [PhiDv [Wtv Hwt]].
      destruct Hsig as [_ Hr].
      apply has_ty_frees_success_aux1.
      * intros u Hu.
        apply (Hitems (IVal v) (@avail_val_in Phi v Hav)).
        simpl. rewrite Hg. simpl. apply in_or_app. left.
        apply (proj1 (tele_ctx_tyreqs _ (wf_tele_ctx Wtv) u)).
        exact (wf_ty_frees Hwt u Hu).
      * exact Hr.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      match goal with H : g_tag Sg n = Some _ |- _ => rename H into Htag end.
      match goal with H : sat Sg Phi th _ |- _ => rename H into Hsat end.
      match goal with
      | H : Forall (fun p => wf_ty Sg Phi (snd p)) th |- _ =>
          rename H into Hth
      end.
      match type of Htag with
      | g_tag Sg n = Some (?D, ?tp) =>
          destruct Hsig as [_ Hr];
          apply has_ty_frees_success_aux1;
          [ exact (wf_ty_frees (@WfTag Sg Phi n D tp th Htag Hsat Hth))
          | exact Hr ]
      end.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      match goal with
      | Hg : g_fn Sg f = Some (?Df, ?Sf, Some ?body),
        Hsat : sat Sg Phi th ?Df,
        Hth : Forall (fun p => wf_ty Sg Phi (snd p)) th |- _ =>
          destruct (wf_fn_decl WF _ Hg) as [PhiDf [Wtf [_ [Hret _]]]];
          destruct Hsat as [PhiDf' [Htc [Hdom _]]];
          pose proof (wf_tele_ctx Wtf) as Htc';
          rewrite Htc' in Htc; injection Htc as <-;
          destruct Hsig as [_ Hr];
          apply has_ty_frees_success_aux1;
          [ apply has_ty_frees_success_aux2 with (s := th);
            [ rewrite Hdom; exact (wf_ty_frees Hret)
            | eapply Forall_impl; [|exact Hth];
              intros p Hwp; exact (wf_ty_frees Hwp) ]
          | exact Hr ]
      end.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      match goal with H : g_fn Sg f = Some _ |- _ => rename H into Hfn end.
      match goal with H : avail Phi (IFn f) |- _ => rename H into Hav end.
      destruct (wf_fn_decl WF _ Hfn) as [PhiDf [Wtf [_ [Hret _]]]].
      destruct Hsig as [_ Hr].
      apply has_ty_frees_success_aux1.
      * intros u Hu.
        apply (Hitems (IFn f) (@avail_fn_in Phi f Hav)).
        simpl. rewrite Hfn. simpl. apply in_or_app. left.
        apply (proj1 (tele_ctx_tyreqs _ (wf_tele_ctx Wtf) u)).
        exact (wf_ty_frees Hret u Hu).
      * exact Hr.
    + simpl in E.
      destruct (lookup_denv prov d) as [en|] eqn:Hen; try discriminate.
      destruct en as [wv|fc dc|hh ss]; try discriminate.
      destruct (g_fn Sg fc) as [[[Dc Sc] ob]|] eqn:Hfc; try discriminate.
      destruct ob as [body|]; try discriminate.
      destruct (interp k Sg d ge e0) as [w0| |] eqn:E0; try discriminate.
      inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      match goal with
      | Hm : g_mth Sg m = Some (?Dm, ?Sm),
        Hrecv : has_ty Sg Phi G e0 ?tau0,
        Hin : In (IMth ?rt0 m ?th0) (cA Phi) |- _ =>
          assert (HR0 : incl (ty_frees tau0) (tyreqs (cA Phi)))
            by (eapply IH; eauto);
          assert (HR : incl (ty_frees (app_subst (cS Phi) tau0))
                            (tyreqs (cA Phi)));
          [ destruct Hsig as [_ Hr];
            eapply has_ty_frees_success_aux1; eauto
          | eapply has_ty_frees_success_aux6;
            [ exact WF | exact Hm | right; reflexivity | exact HR
            | exact Hitems | exact Hin | exact Hsig ] ]
      end.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (interp k Sg d ge e0) as [w0| |] eqn:E0; try discriminate.
      destruct w0 as [|n0 wp]; try discriminate.
      destruct (find_arm n0 arms) as [[xa eb]|] eqn:Hfind; try discriminate.
      assert (Harm : In (n0, xa, eb) arms)
        by (eapply find_arm_in; exact Hfind).
      match goal with
      | H : wf_ty Sg Phi (app_subst (cS Phi) _) |- _ =>
          rename H into Hwf0
      end.
      match goal with
      | H : members (app_subst (cS Phi) _) = Some _ |- _ =>
          rename H into Hmem
      end.
      match goal with H : Forall2 _ _ arms |- _ => rename H into Hb end.
      destruct (@has_ty_frees_success_aux3 ty
        (nsym * var * expr)
        (fun mem arm =>
          has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                 (arm_body arm) tau)
        ms arms (n0, xa, eb) Hb Harm) as [mem [Hmemin Hbody]].
      simpl in Hbody.
      eapply IH with
        (Phi := Phi)
        (G := (xa, member_payload Sg mem) :: G)
        (e := eb) (d := d) (ge := (xa, wp) :: ge) (w := w).
      * exact Hbody.
      * exact Hsig.
      * exact Hitems.
      * intros y ty Hy. simpl in Hy.
        destruct (Nat.eqb y xa) eqn:Hyx.
        -- injection Hy as Heq. subst ty.
           eapply has_ty_frees_success_aux4; eauto.
        -- eapply Hvars; eauto.
      * exact E.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (interp k Sg d ge e1) as [w1| |] eqn:E1; try discriminate.
      match goal with
      | Ht1 : has_ty Sg Phi G e1 ?tau1,
        Ht2 : has_ty Sg Phi ((x, ?tau1) :: G) e2 tau |- _ =>
          assert (Hr1 : incl (ty_frees tau1) (tyreqs (cA Phi)))
            by (eapply IH; eauto);
          eapply IH with (Phi := Phi) (G := (x, tau1) :: G)
            (e := e2) (d := d) (ge := (x, w1) :: ge) (w := w);
          [ exact Ht2 | exact Hsig | exact Hitems | | exact E ];
          intros y ty Hy; simpl in Hy;
          destruct (Nat.eqb y x) eqn:Hyx;
          [ injection Hy as ->; exact Hr1 | eapply Hvars; eauto ]
      end.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (interp k Sg d ge e1) as [w1| |] eqn:E1; try discriminate.
      match goal with
      | Hg : g_val Sg v = Some (?Dv, ?tv),
        Hsat : sat0 Sg Phi ?Dv,
        Hbody : has_ty Sg (add_item (IVal v) Phi) G e2 tau |- _ =>
          eapply IH with (Phi := add_item (IVal v) Phi) (G := G)
            (e := e2) (d := (IVal v, DVal w1) :: d) (ge := ge) (w := w);
          [ exact Hbody | simpl; exact Hsig | | simpl; exact Hvars | exact E ];
          intros i Hi; simpl in Hi; destruct Hi as [Heq|Hi];
          [ subst i; simpl; rewrite Hg; simpl;
            intros u Hu; apply in_app_or in Hu; destruct Hu as [Hu|[]];
            destruct Hsat as [PhiDv [Htc [_ Hav]]];
            rewrite Forall_forall in Hav;
            apply wf_ty_frees_aux3; apply avail_ty_in;
            apply Hav; apply (proj2 (tele_ctx_tyreqs _ Htc u)); exact Hu
          | exact (Hitems i Hi) ]
      end.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (compose_smap d sm) as [dc|] eqn:Hcomp; try discriminate.
      match goal with
      | Hg : g_fn Sg f = Some (?Df, ?Sf, None),
        Hsat : sat0 Sg Phi ?Df,
        Hbody : has_ty Sg (add_item (IFn f) Phi) G e0 tau |- _ =>
          eapply IH with (Phi := add_item (IFn f) Phi) (G := G)
            (e := e0) (d := (IFn f, DClo f' dc) :: d) (ge := ge) (w := w);
          [ exact Hbody | simpl; exact Hsig | | simpl; exact Hvars | exact E ];
          intros i Hi; simpl in Hi; destruct Hi as [Heq|Hi];
          [ subst i; simpl; rewrite Hg; simpl;
            intros u Hu; apply in_app_or in Hu; destruct Hu as [Hu|[]];
            destruct Hsat as [PhiDf [Htc [_ Hav]]];
            rewrite Forall_forall in Hav;
            apply wf_ty_frees_aux3; apply avail_ty_in;
            apply Hav; apply (proj2 (tele_ctx_tyreqs _ Htc u)); exact Hu
          | exact (Hitems i Hi) ]
      end.
    + inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (compose_smap d sm) as [dc|] eqn:Hcomp; try discriminate.
      match goal with
      | Hm : g_mth Sg m = Some (?Dm, ?Sm),
        Hrt : wf_ty Sg Phi rt,
        Hsat : sat Sg Phi th ?Dm,
        Hth : Forall (fun p => wf_ty Sg Phi (snd p)) th,
        Hbody : has_ty Sg (add_item (IMth rt m th) Phi) G e0 tau |- _ =>
          eapply IH with (Phi := add_item (IMth rt m th) Phi) (G := G)
            (e := e0) (d := (IMth rt m th, DClo f' dc) :: d)
            (ge := ge) (w := w);
          [ exact Hbody | simpl; exact Hsig | | simpl; exact Hvars | exact E ];
          intros i Hi; simpl in Hi; destruct Hi as [Heq|Hi];
          [ subst i; simpl; intros u Hu;
            apply in_app_or in Hu; destruct Hu as [Hu|Hu];
            [ apply in_app_or in Hu; destruct Hu as [Hu|Hu];
              [ exact (wf_ty_frees Hrt u Hu)
              | apply in_flat_map in Hu; destruct Hu as [p [Hp Hu]];
                rewrite Forall_forall in Hth;
                exact (wf_ty_frees (Hth p Hp) u Hu) ]
            | exact (@has_ty_frees_success_aux7 Sg Phi rt m Dm Sm th
                WF Hm Hrt Hsat Hth u Hu) ]
          | exact (Hitems i Hi) ]
      end.
    + (* EBindFun *)
      inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (compose_smap d smc) as [dc|]; try discriminate.
      match goal with
      | Hbody : has_ty Sg (add_item (IFun u) Phi) G e0 tau,
        Hfx : Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) |- _ =>
          eapply IH with (Phi := add_item (IFun u) Phi) (G := G)
            (e := e0) (d := (IFun u, DFun h [sm] dc) :: d) (ge := ge) (w := w);
          [ exact Hbody | simpl; exact Hsig | | simpl; exact Hvars | exact E ];
          intros i Hi; simpl in Hi; destruct Hi as [Heq|Hi];
          [ subst i; intros x Hx; simpl in Hx; rewrite app_nil_r in Hx;
            rewrite Forall_forall in Hfx; simpl;
            apply wf_ty_frees_aux3; apply avail_ty_in; exact (Hfx x Hx)
          | exact (Hitems i Hi) ]
      end.
    + (* EBindFunA *)
      inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (lookup_denv (IFun u') d) as [[wv|fc dc|hh ss dd]|];
        try discriminate.
      match goal with
      | Hbody : has_ty Sg (add_item (IFun u) Phi) G e0 tau,
        Hfx : Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) |- _ =>
          eapply IH with (Phi := add_item (IFun u) Phi) (G := G)
            (e := e0) (d := (IFun u, DFun hh (ss ++ [sm]) dd) :: d) (ge := ge)
            (w := w);
          [ exact Hbody | simpl; exact Hsig | | simpl; exact Hvars | exact E ];
          intros i Hi; simpl in Hi; destruct Hi as [Heq|Hi];
          [ subst i; intros x Hx; simpl in Hx; rewrite app_nil_r in Hx;
            rewrite Forall_forall in Hfx; simpl;
            apply wf_ty_frees_aux3; apply avail_ty_in; exact (Hfx x Hx)
          | exact (Hitems i Hi) ]
      end.
    + (* EApply: the scope's type is bounded by the opened context, and the
         escape premise removes the result's fresh types from the bound. *)
      inversion HT; subst;
        try match goal with
        | Hwf : wf_ty Sg ?P ?T |- incl (ty_frees ?T) _ =>
            exact (wf_ty_frees Hwf)
        end.
      simpl in E.
      destruct (interp_apply k Sg d u th sm) as [dr | rr] eqn:Ea;
        [| exfalso; subst rr;
           exact (proj2 (interp_binds_never_ok k) _ _ _ _ _ _ Ea) ].
      match goal with
      | Hu : g_fun Sg u = Some (?Du', ?Dr', None),
        Hs0 : sat Sg Phi th ?Du',
        Hthw : Forall (fun p => wf_ty Sg Phi (snd p)) th,
        Hco : res_coherent Sg Phi th ?Dr',
        Hbody : has_ty Sg (open_res (app_tele th ?Dr') Phi) G e0 tau,
        Hesc : Forall (fun t => ~ In t (ty_frees tau)) (tyreqs ?Dr') |- _ =>
          rename Dr' into Dr; rename Du' into Du;
          rename Hu into Hfu; rename Hs0 into Hsat0;
          rename Hbody into Hb; rename Hesc into Hes; rename Hthw into Hthw0; rename Hco into Hco0
      end.
      assert (Hr : incl (ty_frees tau) (tyreqs (cA (open_res (app_tele th Dr) Phi)))).
      { eapply IH with (Phi := open_res (app_tele th Dr) Phi) (G := G) (e := e0)
                       (d := dr ++ d) (ge := ge) (w := w);
          [ exact Hb | | | | exact E ].
        - eapply sigma_regular_weaken; [exact Hsig | apply open_res_tyreqs].
        - intros i Hi. simpl in Hi. apply in_app_or in Hi.
          destruct Hi as [Hi | Hi].
          + exact (@open_res_th_regular Sg Phi u Du Dr th WF Hfu Hsat0 Hthw0 Hco0 Hsig i Hi).
          + intros x Hx. apply open_res_tyreqs. exact (Hitems i Hi x Hx).
        - intros y ty Hy x Hx. apply open_res_tyreqs.
          exact (Hvars y ty Hy x Hx). }
      intros x Hx. specialize (Hr x Hx). simpl in Hr.
      rewrite tyreqs_app, tyreqs_app_tele in Hr. apply in_app_or in Hr.
      destruct Hr as [Hr | Hr]; [| exact Hr].
      exfalso. rewrite Forall_forall in Hes. exact (Hes x Hr Hx).
Qed.

(* --- The case lemmas: one per typing rule, at fuel S k, under the strong
   fuel induction hypothesis. ----------------------------------------------- *)

(* --- Abstract functors [D78]: generic leaves. ------------------------------ *)

(* Copying a binding through `bind φ = ψ;` extends its chain by one link. *)
Lemma chain_ok_snoc : forall Sg h sms w,
  chain_ok Sg h sms w ->
  forall u sm Dw Drw bw Du Dru bu,
  g_fun Sg w = Some (Dw, Drw, bw) ->
  g_fun Sg u = Some (Du, Dru, bu) ->
  fun_link Sg Dw sm Du ->
  chain_ok Sg h (sms ++ [sm]) u.
Proof.
  intros Sg h sms w H.
  induction H as [w | h s r w' u' Dh Drh bh Dw' Drw' bw' Hh Hw' Hl Hch IH];
    intros u sm Dw Drw bw Du Dru bu Hw Hu Hl2.
  - simpl. eapply ChCons; [exact Hw | exact Hu | exact Hl2 | apply ChNil].
  - simpl. eapply ChCons; [exact Hh | exact Hw' | exact Hl |].
    eapply IH; eassumption.
Qed.

Lemma lookup_denv_app : forall i d1 d2,
  lookup_denv i (d1 ++ d2) =
  match lookup_denv i d1 with Some en => Some en | None => lookup_denv i d2 end.
Proof.
  intros i d1 d2. induction d1 as [| [j en] r IH]; simpl; [reflexivity |].
  destruct (item_eqb i j); [reflexivity | exact IH].
Qed.

(* The identity re-keying of a result: its pairs are (i, i) for the result's
   non-type items, and its first match for such an item is the item. *)
Lemma id_smap_in : forall D a b,
  In (a, b) (id_smap D) -> a = b /\ In a D /\ is_ty_item a = false.
Proof.
  intros D a b H. unfold id_smap in H. apply in_map_iff in H.
  destruct H as [i [Heq Hi]]. injection Heq as <- <-.
  apply filter_In in Hi. destruct Hi as [Hi Hnt].
  split; [reflexivity | split; [exact Hi |]].
  destruct (is_ty_item i); [discriminate Hnt | reflexivity].
Qed.

(* ---- Rule A leaves. --------------------------------------------------------- *)

Lemma inst_smap_in : forall th D a b,
  In (a, b) (inst_smap th D) ->
  a = app_subst_item th b /\ In b D /\ is_ty_item b = false.
Proof.
  intros th D a b H. unfold inst_smap in H. apply in_map_iff in H.
  destruct H as [i [Heq Hi]]. injection Heq as <- <-.
  apply filter_In in Hi. destruct Hi as [Hi Hnt].
  split; [reflexivity | split; [exact Hi |]].
  destruct (is_ty_item i); [discriminate Hnt | reflexivity].
Qed.

Lemma lookup_smap_inst : forall th D i,
  In i D -> is_ty_item i = false ->
  exists i', lookup_smap (app_subst_item th i) (inst_smap th D) = Some i'.
Proof.
  intros th D i. unfold inst_smap. induction D as [| a r IH]; intros Hi Hnt.
  - destruct Hi.
  - simpl. destruct (negb (is_ty_item a)) eqn:Ea.
    + simpl. destruct (item_eqb (app_subst_item th i) (app_subst_item th a)) eqn:E.
      * exists a. reflexivity.
      * apply IH; [| exact Hnt].
        destruct Hi as [-> | Hi]; [| exact Hi].
        rewrite (proj2 (item_eqb_eq _ _) eq_refl) in E. discriminate E.
    + apply IH; [| exact Hnt].
      destruct Hi as [-> | Hi]; [| exact Hi].
      rewrite Hnt in Ea. discriminate Ea.
Qed.

(* Reading through θ then g2 agrees with reading through g1, pointwise. *)
Lemma comp_agree : forall g1 g2 s tau,
  (forall z, In z (ty_frees tau) ->
     app_subst g1 (TAbs z) = app_subst g2 (app_subst s (TAbs z))) ->
  app_subst g1 tau = app_subst g2 (app_subst s tau).
Proof.
  intros g1 g2 s tau. revert g1 g2 s.
  induction tau using ty_ind'; intros g1 g2 s Hag.
  - reflexivity.
  - apply Hag. simpl. left. reflexivity.
  - rewrite !app_subst_nom. f_equal. unfold map_snd. rewrite !map_map.
    revert Hag. induction H as [|p r Hp Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * f_equal. apply Hp. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
  - rewrite !app_subst_union. f_equal. rewrite !map_map.
    revert Hag. induction H as [|m r Hm Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * apply Hm. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
Qed.

Lemma app_subst_none : forall s tau,
  (forall z, In z (ty_frees tau) -> lookup_subst z s = None) ->
  app_subst s tau = tau.
Proof.
  intros s tau H. rewrite (app_subst_frees_agree s [] tau); [apply app_subst_nil |].
  intros z Hz. rewrite (H z Hz). reflexivity.
Qed.

Lemma lookup_subst_none : forall z s, ~ In z (map fst s) -> lookup_subst z s = None.
Proof.
  intros z s H. destruct (lookup_subst z s) eqn:E; [| reflexivity].
  exfalso. apply H. apply in_map_iff. exists (z, t). split; [reflexivity |].
  apply callee_env_agree_aux1. exact E.
Qed.

Lemma wf_tele_length : forall Sg Phi D Phi',
  wf_tele Sg Phi D Phi' -> length (cA Phi') <= length (cA Phi) + length D.
Proof.
  intros Sg Phi D Phi' H. induction H as [Phi | Phi i D Phi1 Phi'' _ Hm _ IH].
  - simpl. rewrite Nat.add_0_r. reflexivity.
  - destruct (merge_item_cA _ _ Hm) as [E | E]; rewrite E in IH;
      [rewrite length_app in IH; simpl in *|]; simpl; lia.
Qed.

(* A telescope whose formation appended every item: its type items were
   all fresh. *)
Lemma wf_tele_append_fresh : forall Sg Phi D Phi',
  wf_tele Sg Phi D Phi' -> cA Phi' = cA Phi ++ D ->
  forall t, In t (tyreqs D) -> ~ In (ITy t) (cA Phi).
Proof.
  intros Sg Phi D Phi' H. induction H as [Phi | Phi i D Phi1 Phi'' _ Hm Hw IH];
    intros E t Ht.
  - destruct Ht.
  - pose proof (wf_tele_length Hw) as Hlen. rewrite E in Hlen.
    rewrite length_app in Hlen. simpl in Hlen.
    destruct (merge_item_cA _ _ Hm) as [E1 | E1].
    + assert (Hfind : find_same_key (cS Phi) (cA Phi) i = None).
      { unfold merge_item in Hm.
        destruct (find_same_key (cS Phi) (cA Phi) i) as [j|] eqn:Hf; [| reflexivity].
        destruct (unify_items _ _ j i) as [s'|]; [| discriminate].
        destruct (item_subst_eqb s' j i && sigma_regular_b s' (cA Phi) && idempotent_b s');
          [| discriminate].
        injection Hm as <-. simpl in E1.
        exfalso. assert (L := f_equal (@length item) E1).
        rewrite length_app in L. simpl in L. lia. }
      assert (Hnot : forall j, In j (cA Phi) -> same_key (cS Phi) j i = false).
      { clear -Hfind. induction (cA Phi) as [|a r IHr]; intros j Hj; [destruct Hj|].
        simpl in Hfind. destruct (same_key (cS Phi) a i) eqn:Ea; [discriminate|].
        destruct Hj as [<- | Hj]; [exact Ea | exact (IHr Hfind j Hj)]. }
      unfold tyreqs in Ht. simpl in Ht. apply in_app_or in Ht.
      destruct Ht as [Ht | Ht].
      * destruct i; simpl in Ht; try contradiction. destruct Ht as [<- | []].
        intros Hin. specialize (Hnot _ Hin). simpl in Hnot.
        rewrite Nat.eqb_refl in Hnot. discriminate.
      * intros Hin. apply (IH ltac:(rewrite E1, E, <- app_assoc; reflexivity) t Ht).
        rewrite E1. apply in_or_app. left. exact Hin.
    + exfalso. rewrite E1 in Hlen. lia.
Qed.

(* The receiver symbol is never a telescope's. *)
Lemma this_notin_wf_tele : forall Sg Phi D Phi',
  wf_tele Sg Phi D Phi' -> ~ In this_sym (tyreqs (cA Phi)) ->
  ~ In this_sym (tyreqs (cA Phi')).
Proof.
  intros Sg Phi D Phi' H. induction H as [Phi | Phi i D Phi1 Phi'' Hok Hm _ IH];
    intros Hn; [exact Hn |].
  apply IH. destruct (merge_item_cA _ _ Hm) as [E | E]; rewrite E; [| exact Hn].
  rewrite tyreqs_app. intros Hin. apply in_app_or in Hin. destruct Hin as [Hin | Hin];
    [exact (Hn Hin) |].
  destruct i; simpl in Hin; try contradiction. destruct Hin as [Ht | []].
  inversion Hok; subst. congruence.
Qed.

Lemma app_subst_TAbs : forall s x,
  app_subst s (TAbs x) = match lookup_subst x s with Some a => a | None => TAbs x end.
Proof. reflexivity. Qed.

Lemma lookup_subst_app : forall t a b,
  lookup_subst t (a ++ b)
  = match lookup_subst t a with Some x => Some x | None => lookup_subst t b end.
Proof.
  intros t a b. induction a as [| [x y] r IH]; simpl; [reflexivity |].
  destruct (Nat.eqb t x); [reflexivity | exact IH].
Qed.

(* Three-deep substitution chains agree when they agree pointwise. *)
Lemma chain3_agree : forall a1 a2 a3 b1 b2 b3 tau,
  (forall z, In z (ty_frees tau) ->
     app_subst a1 (app_subst a2 (app_subst a3 (TAbs z)))
     = app_subst b1 (app_subst b2 (app_subst b3 (TAbs z)))) ->
  app_subst a1 (app_subst a2 (app_subst a3 tau))
  = app_subst b1 (app_subst b2 (app_subst b3 tau)).
Proof.
  intros a1 a2 a3 b1 b2 b3 tau.
  induction tau using ty_ind'; intros Hag.
  - reflexivity.
  - apply Hag. simpl. left. reflexivity.
  - rewrite !app_subst_nom. f_equal. unfold map_snd. rewrite !map_map.
    revert Hag. induction H as [|p r Hp Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * f_equal. apply Hp. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
  - rewrite !app_subst_union. f_equal. rewrite !map_map.
    revert Hag. induction H as [|m r Hm Hr IH]; intros Hag.
    + reflexivity.
    + simpl. f_equal.
      * apply Hm. intros t Ht. apply Hag. simpl.
        apply in_or_app. left. exact Ht.
      * apply IH. intros t Ht. apply Hag. simpl.
        apply in_or_app. right. exact Ht.
Qed.

Lemma lookup_smap_id : forall D i,

  In i D -> is_ty_item i = false -> lookup_smap i (id_smap D) = Some i.
Proof.
  intros D i. unfold id_smap. induction D as [| a r IH]; intros Hi Hnt.
  - destruct Hi.
  - simpl. destruct (negb (is_ty_item a)) eqn:Ea.
    + simpl. destruct (item_eqb i a) eqn:E.
      * apply item_eqb_eq in E. subst a. reflexivity.
      * apply IH; [| exact Hnt].
        destruct Hi as [-> | Hi]; [| exact Hi].
        rewrite (proj2 (item_eqb_eq i i) eq_refl) in E. discriminate E.
    + apply IH; [| exact Hnt].
      destruct Hi as [-> | Hi]; [| exact Hi].
      rewrite Hnt in Ea. discriminate Ea.
Qed.

(* The fresh types of an application, read through the body's grounding. *)
Lemma lookup_prefix_in : forall (f : tsym -> ty) ts x g,
  In x ts -> lookup_subst x (map (fun t => (t, f t)) ts ++ g) = Some (f x).
Proof.
  intros f ts x g. induction ts as [| a r IH]; intros Hx; [destruct Hx |].
  simpl. destruct (Nat.eqb x a) eqn:E.
  - apply Nat.eqb_eq in E. subst a. reflexivity.
  - apply IH. destruct Hx as [<- | Hx]; [| exact Hx].
    rewrite Nat.eqb_refl in E. discriminate E.
Qed.

Lemma lookup_prefix_notin : forall (f : tsym -> ty) ts x g,
  ~ In x ts -> lookup_subst x (map (fun t => (t, f t)) ts ++ g)
               = lookup_subst x g.
Proof.
  intros f ts x g. induction ts as [| a r IH]; intros Hx; [reflexivity |].
  simpl. destruct (Nat.eqb x a) eqn:E.
  - apply Nat.eqb_eq in E. subst a. exfalso. apply Hx. left. reflexivity.
  - apply IH. intros H. apply Hx. right. exact H.
Qed.

Lemma grounding_lookup_closed : forall g t a,
  grounding g -> lookup_subst t g = Some a -> closed_ty a.
Proof.
  intros g t a Hg. induction g as [| [u b] r IH]; intros Hl; [discriminate Hl |].
  simpl in Hl. inversion Hg as [| p q Hp Hq]; subst.
  destruct (Nat.eqb t u); [injection Hl as <-; exact Hp | exact (IH Hq Hl)].
Qed.

(* Every item of a formed telescope was checked. *)

Section SafetyCases.
  Variable Sg : gsig.
  Variable WF : wf_gsig Sg.
  Variable k : nat.
  Variable IHfuel : forall j, j <= k -> SAFE Sg j.

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
    destruct Hd as [Hcover [Hall [Hdreg Hdkeys]]].
    assert (Hin : In (IVal v) (cA Phi)) by (apply avail_val_in; exact Hav).
    rewrite Forall_forall in Hcover.
    specialize (Hcover _ Hin).
    destruct Hcover as [Hbad | [en Hlook]]; [simpl in Hbad; discriminate|].
    assert (Hpin : In (IVal v, en) d) by (apply lookup_denv_in; exact Hlook).
    rewrite Forall_forall in Hall.
    specialize (Hall _ Hpin).
    destruct en as [w | f' d0 | hh ss].
    - simpl in Hall. rewrite Hgv in Hall.
      simpl. rewrite Hlook. simpl.
      rewrite (absorbs_all Habs). rewrite (absorbs_all Habs).
      exact Hall.
    - simpl in Hall. contradiction.
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
    { destruct Hsat as [PhiD [Htc [Hdom _]]]. rewrite Hdom.
      assert (WF' := WF). destruct WF' as [Wtag Wval Wfn Wmth].
      destruct (Wtag n D tau0 Hgt) as [PhiD' [Wt Hwf]].
      apply wf_tele_ctx in Wt.
      assert (HP : PhiD' = PhiD) by congruence. subst PhiD'.
      exact (wf_ty_frees Hwf). }
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
    assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov [Hsreg Hthis]]]].
    assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau)
                               = app_subst g tau)
      by (exact (@absorbs_all g (cS Phi) Hab)).
    (* locate the closure bound at f *)
    assert (Hin := @avail_fn_in Phi f Hav).
    assert (Hd2 := Hd). destruct Hd2 as [Hcov1 [Hall1 [Hdreg Hdkeys]]].
    rewrite Forall_forall in Hcov1. specialize (Hcov1 _ Hin).
    destruct Hcov1 as [Hbad | [en Hlook]]; [ simpl in Hbad; discriminate | ].
    assert (Hda : dentry_agree Sg g (IFn f) en).
    { rewrite Forall_forall in Hall1.
      apply (Hall1 (IFn f, en)). apply lookup_denv_in. exact Hlook. }
    simpl. rewrite Hlook.
    destruct en as [w0 | f' df | hh ss];
      [ simpl in Hda; contradiction | | simpl in Hda; contradiction ].
    simpl in Hda. rewrite Hf in Hda.
    destruct (g_fn Sg f') as [[[D' S'] b'] | ] eqn:Hf'; [ | contradiction ].
    destruct b' as [body | ]; [ | contradiction ].
    destruct Hda as
      [PhiD' [g' [Htc' [Hgr' [Hcov' [Habs' [Hpar [Hret [Htcov [Hkreg Hdd]]]]]]]]]].
    (* the provider's declaration is well-formed under its merged context *)
    assert (WF2 := WF). destruct WF2 as [Wtag Wval Wfn Wmth].
    destruct (Wfn f' D' S' (Some body) Hf') as [PhiD [Wt [Wp [Wr Wb]]]].
    (* the two merged contexts for D' coincide *)
    pose proof (wf_tele_ctx Wt) as Htc2.
    rewrite Htc' in Htc2. injection Htc2 as Heq. subst PhiD'.
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
      assert (Hcgb : ctx_grounded g' PhiD).
      { split; [ exact Hgr' | split; [ exact Habs' | split; [ exact Hcov' | split ] ] ].
        - apply (@tele_sigma_regular Sg empty_ctx D' PhiD Wt).
          split; [ intros x Hx; destruct Hx | constructor ].
        - apply (@callee_env_agree_aux13 Sg empty_ctx D' PhiD Wt).
          intros Hx. destruct Hx. }
      assert (Hdb : denv_agree Sg g' (cA PhiD) df).
      { split; [ exact Htcov | split ].
        - exact (@safety_case_need_aux1 Sg g' df Hdd).
        - split.
          + rewrite Forall_forall. intros i Hi.
            apply (@tele_items_regular Sg empty_ctx D' PhiD WF Wt); [ | exact Hi ].
            intros i0 Hi0. destruct Hi0.
          + exact Hkreg. }
      assert (Hvb : venv_agree Sg g' (cA PhiD) (body_env S') (mk_venv ws)).
      { unfold body_env.
        apply (@venv_agree_body Sg g' (cA PhiD) ws (fs_params S') Hws).
        eapply Forall_impl; [|exact Wp].
        intros tau Hwf.
        exact (wf_ty_frees Hwf). }
      assert (Hbody := Wb body eq_refl).
      assert (Hres := @IHfuel k (Nat.le_refl k) PhiD (body_env S')
                        body (fs_ret S') df (mk_venv ws) g'
                        Hbody Hcgb Hdb Hvb).
      rewrite (@absorbs_all g' (cS PhiD) Habs' (fs_ret S')) in Hres.
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
    assert (Hcg2 := Hcg). destruct Hcg2 as [Hgr [Hab [Hcov [Hsreg Hthis]]]].
    assert (Hcol : forall tau, app_subst g (app_subst (cS Phi) tau)
                               = app_subst g tau)
      by (exact (@absorbs_all g (cS Phi) Hab)).
    assert (WF2 := WF). destruct WF2 as [Wtag Wval Wfn Wmth].
    destruct (Wfn f Df Sf (Some body) Hf) as [PhiD [Wt [Wp [Wr Wb]]]].
    (* the callee's world: its grounding, merged context, and environment *)
    destruct (@callee_env_agree Sg g Phi th sm Df d PhiD WF Wt Hsat Hth Hsm Hcg Hd)
      as [d' [Hcomp [Hcgc Hdc]]].
    (* reading a PhiD-covered type through the callee grounding *)
    assert (Hread : forall tau, incl (ty_frees tau) (tyreqs (cA PhiD)) ->
              app_subst (callee_grounding g Phi th (cA PhiD)) tau
              = app_subst g (app_subst (cS Phi) (app_subst th tau))).
    { intros tau0 Hi. apply callee_grounding_reads. exact Hi. }
    (* the parameters and the result type only mention PhiD's requirements *)
    assert (Hfp : forall tau, In tau (fs_params Sf) ->
              incl (ty_frees tau) (tyreqs (cA PhiD))).
    { intros tau0 Hin. rewrite Forall_forall in Wp.
      pose proof (Wp tau0 Hin) as Hw0. apply wf_ty_frees in Hw0. exact Hw0. }
    assert (Hfr : incl (ty_frees (fs_ret Sf)) (tyreqs (cA PhiD))).
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
                       vty Sg w (app_subst (callee_grounding g Phi th (cA PhiD)) tau))
                     ws (fs_params Sf)).
      { apply (@safety_case_call_aux2 g (cS Phi) th
                 (callee_grounding g Phi th (cA PhiD)) ws (fs_params Sf)).
        - intros tau0 Hin.
          rewrite (Hcol (app_subst (cS Phi) (app_subst th tau0))).
          apply Hread. apply Hfp. exact Hin.
        - exact Hsa. }
      assert (Hcgb := Hcgc).
      destruct Hcgb as [Hgr' [Hab' _]].
      assert (Hvb : venv_agree Sg (callee_grounding g Phi th (cA PhiD)) (cA PhiD)
                               (body_env Sf) (mk_venv ws)).
      { unfold body_env.
        apply (@venv_agree_body Sg (callee_grounding g Phi th (cA PhiD))
                 (cA PhiD) ws (fs_params Sf) Hws).
        rewrite Forall_forall. intros tau0 Hin. apply Hfp. exact Hin. }
      assert (Hres := @IHfuel k (Nat.le_refl k) PhiD (body_env Sf)
                        body (fs_ret Sf) d' (mk_venv ws)
                        (callee_grounding g Phi th (cA PhiD))
                        (Wb body eq_refl) Hcgc Hdc Hvb).
      rewrite (@absorbs_all (callee_grounding g Phi th (cA PhiD)) (cS PhiD)
                 Hab' (fs_ret Sf)) in Hres.
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

  (* Private helper: open up a method entry's agreement.  The closure's
     grounding is now stated against the provider telescope's *merged*
     context PhiD0: coverage of the merged requirements, absorption of the
     merge substitution, and environment agreement at the merged items. *)
  Lemma safety_case_meth_aux3 :
    forall (g0 : subst) (rt : ty) (m0 : msym) (th1 : subst)
           (Dm0 : tele) (Sm0 : fnsig) (f'0 : fsym) (dc0 : denv),
      g_mth Sg m0 = Some (Dm0, Sm0) ->
      dentry_agree Sg g0 (IMth rt m0 th1) (DClo f'0 dc0) ->
      exists D0 S0 body0 PhiD0 gc0,
        g_fn Sg f'0 = Some (D0, S0, Some body0)
        /\ tele_ctx D0 = Some PhiD0
        /\ grounding gc0
        /\ covers gc0 (tyreqs (cA PhiD0))
        /\ absorbs gc0 (cS PhiD0)
        /\ map (app_subst gc0) (fs_params S0)
           = app_subst g0 rt
             :: map (fun tau => app_subst g0
                       (app_subst (this_subst rt) (app_subst th1 tau)))
                    (fs_params Sm0)
        /\ app_subst gc0 (fs_ret S0)
           = app_subst g0
               (app_subst (this_subst rt) (app_subst th1 (fs_ret Sm0)))
        /\ denv_agree Sg gc0 (cA PhiD0) dc0.
  Proof.
    intros g0 rt m0 th1 Dm0 Sm0 f'0 dc0 Hm0 Hag.
    simpl in Hag. rewrite Hm0 in Hag.
    remember (g_fn Sg f'0) as gf eqn:Egf.
    destruct gf as [[[D0 S0] b0]|]; [| destruct Hag].
    destruct b0 as [body0|]; [| destruct Hag].
    destruct Hag as
      [PhiD0 [gc0 [Htc [Hgrc [Hcovc [Habc [Hpar [Hret [Htcov [Hkeys Hall]]]]]]]]]].
    exists D0, S0, body0, PhiD0, gc0.
    split; [ solve [ reflexivity | symmetry; exact Egf ] |].
    split; [ exact Htc |].
    split; [ exact Hgrc |].
    split; [ exact Hcovc |].
    split; [ exact Habc |].
    split; [ exact Hpar |].
    split; [ exact Hret |].
    split; [ exact Htcov | split ].
    - apply safety_case_meth_aux2; exact Hall.
    - split.
      + rewrite Forall_forall. intros i Hi.
        assert (Hdecl : g_fn Sg f'0 = Some (D0, S0, Some body0))
          by (symmetry; exact Egf).
        destruct (wf_fn_decl WF _ Hdecl) as [PhiDf [Wt _]].
        assert (EPhi : PhiDf = PhiD0).
        { apply wf_tele_ctx in Wt. rewrite Wt in Htc.
          injection Htc as EPhi. exact EPhi. }
        subst PhiDf.
        refine (@tele_items_regular Sg empty_ctx D0 PhiD0 WF Wt _ i Hi).
        intros j Hj. destruct Hj.
      + exact Hkeys.
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
                  app_subst_item (cS Phi) i1
                  = app_subst_item (cS Phi) (IMth rt0 m th0)) ->
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
    { destruct Hd as [_ [Hd2 [Hdreg Hdkeys]]]. rewrite Forall_forall in Hd2.
      exact (Hd2 (IMth rt0 m th0, en)
               (@lookup_denv_in (IMth rt0 m th0) d en Hen)). }
    destruct en as [wv | f' dc | hh ss];
      [ simpl in Hag; destruct Hag | | simpl in Hag; destruct Hag ].
    destruct (@safety_case_meth_aux3 g rt0 m th0 Dm Sf f' dc Hm Hag)
      as [D' [S' [body [PhiD' [gc
            [Ef' [Htc [Hgrc [Hcovc [Habc [Hpar [Hret Hdc]]]]]]]]]]]].
    destruct (Wfn f' D' S' (Some body) Ef') as [PhiDf [Wt' [Wps' [Wrt' Wb]]]].
    (* the two merged contexts of the provider telescope are one *)
    assert (EPhi : PhiDf = PhiD').
    { apply wf_tele_ctx in Wt'. rewrite Wt' in Htc.
      injection Htc as EPhi. exact EPhi. }
    subst PhiDf.
    (* the callee's context and world *)
    assert (Hcgb : ctx_grounded gc PhiD').
    { split; [ exact Hgrc |].
      split; [ exact Habc |].
      split; [ exact Hcovc |].
      split.
      - apply (@tele_sigma_regular Sg empty_ctx D' PhiD' Wt').
        split; [ intros x Hx; destruct Hx | constructor ].
      - apply (@callee_env_agree_aux13 Sg empty_ctx D' PhiD' Wt').
        simpl. intros Hbad; exact Hbad. }
    assert (Hdb : denv_agree Sg gc (cA PhiD') dc) by exact Hdc.
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
        assert (Hvb : venv_agree Sg gc (cA PhiD') (body_env S')
                        (mk_venv (w0 :: ws))).
        { unfold body_env. rewrite <- Eps.
          apply (@venv_agree_body Sg gc (cA PhiD') (w0 :: ws) (p0 :: rest)).
          - constructor; [ exact Hw0 | exact Hws ].
          - eapply Forall_impl; [|exact Wps'].
            intros tau Hwf. exact (wf_ty_frees Hwf). }
        assert (Hres := @IHfuel k (Nat.le_refl k) PhiD' (body_env S')
                          body (fs_ret S') dc (mk_venv (w0 :: ws)) gc
                          (Wb body eq_refl) Hcgb Hdb Hvb).
        rewrite (@absorbs_all gc (cS PhiD') Habc (fs_ret S')) in Hres.
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
      pose proof (wf_tag_decl WF n Htag) as [PhiD' [Wt Hp]].
      destruct Hsat as [PhiD [Htc [Hdom _]]].
      apply wf_tele_ctx in Wt.
      assert (HP : PhiD' = PhiD) by congruence. subst PhiD'.
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
      + eapply has_ty_frees_success; [exact WF | exact H1 | | | | exact E].
        * destruct Hg as [_ [_ [_ [Hregular _]]]]. exact Hregular.
        * intros i Hi.
          destruct Hd as [_ [_ [Hregular _]]].
          rewrite Forall_forall in Hregular.
          exact (Hregular i Hi).
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
  Lemma safety_case_bindty_aux1 : forall A u,
      In u (tyreqs A) -> In (ITy u) A.
  Proof.
    intros A u H. unfold tyreqs in H.
    apply in_flat_map in H. destruct H as [i [Hi Hu]].
    destruct i; simpl in Hu; try contradiction.
    destruct Hu as [E | []]. subst. exact Hi.
  Qed.

  Lemma safety_case_bindty_aux2 : forall g t tau0 A u,
      ~ In (ITy t) A ->
      In u (tyreqs A) ->
      lookup_subst u ((t, tau0) :: g) = lookup_subst u g.
  Proof.
    intros g t tau0 A u Hfresh Hu. simpl.
    destruct (Nat.eqb u t) eqn:E; [|reflexivity].
    apply Nat.eqb_eq in E. subst u.
    exfalso. apply Hfresh. apply safety_case_bindty_aux1. exact Hu.
  Qed.

  Lemma safety_case_bindty_aux3 : forall s tau A,
      incl (ty_frees tau) (tyreqs A) ->
      Forall (fun p => incl (ty_frees (snd p)) (tyreqs A)) s ->
      incl (ty_frees (app_subst s tau)) (tyreqs A).
  Proof.
    intros s tau A Htau Hs u Hu.
    destruct (@frees_app_subst_bound s tau u Hu)
      as [[p [Hp Hup]] | [Hut _]].
    - rewrite Forall_forall in Hs. exact (Hs p Hp u Hup).
    - exact (Htau u Hut).
  Qed.

  Lemma safety_case_bindty_aux4 : forall t s tau,
      lookup_subst t s = Some tau -> In t (map fst s).
  Proof.
    intros t s. induction s as [|[u a] r IH]; intros tau H; simpl in *.
    - discriminate.
    - destruct (Nat.eqb t u) eqn:E.
      + apply Nat.eqb_eq in E. subst u. left. reflexivity.
      + right. apply IH with (tau := tau). exact H.
  Qed.

  Lemma safety_case_bindty_aux5 : forall t s tau,
      lookup_subst t s = Some tau -> In (t, tau) s.
  Proof.
    intros t s. induction s as [|[u a] r IH]; intros tau H; simpl in H.
    - discriminate.
    - destruct (Nat.eqb t u) eqn:E.
      + apply Nat.eqb_eq in E. subst u. injection H as ->.
        left. reflexivity.
      + right. apply IH. exact H.
  Qed.

  Lemma safety_case_bindty_aux6 : forall x G tau,
      lookup_tenv x G = Some tau -> In (x, tau) G.
  Proof.
    intros x G. induction G as [|[y a] r IH]; intros tau H; simpl in H.
    - discriminate.
    - destruct (Nat.eqb x y) eqn:E.
      + apply Nat.eqb_eq in E. subst y. injection H as ->.
        left. reflexivity.
      + right. apply IH. exact H.
  Qed.

  (* The world a type bind leaves: the grounding extended at the fresh
     symbol, every old reading unchanged.  Shared by the expression-level
     rule and a functor body's bind. *)
  Lemma world_bindty : forall Phi t tau' d g,
      t <> this_sym ->
      ~ In (ITy t) (cA Phi) ->
      wf_ty Sg Phi tau' ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      ctx_grounded ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
                   (bind_ty t tau' Phi)
      /\ denv_agree Sg ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
                    (cA (bind_ty t tau' Phi)) d
      /\ (forall u, In u (tyreqs (cA Phi)) ->
            lookup_subst u ((t, app_subst g (app_subst (cS Phi) tau')) :: g)
            = lookup_subst u g).
  Proof.
    intros Phi t tau' d g Hneq Hfresh Htau' Hg Hd.
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
    assert (Hitem : forall i,
        incl (dep_tys Sg i ++ item_ty_frees i) (tyreqs (cA Phi)) ->
        app_subst_item g i = app_subst_item g2 i).
    { intros [u|v|f|rt m th|uf] Hi; simpl; try reflexivity.
      f_equal.
      - symmetry. apply app_subst_frees_agree. intros z Hz.
        apply Hlook. apply Hi. apply in_or_app. right.
        apply in_or_app. left. exact Hz.
      - unfold map_snd. apply map_ext_in. intros [z a] Hin.
        simpl. f_equal. symmetry. apply app_subst_frees_agree.
        intros q Hq. apply Hlook. apply Hi.
        apply in_or_app. right. apply in_or_app. right.
        apply in_flat_map. exists (z, a). split; assumption. }
    assert (Hd2 : denv_agree Sg g2 (cA (bind_ty t tau' Phi)) d).
    { destruct Hd as [Hcov [Hentries [Hregs Hkeys]]].
      split.
      - simpl. constructor; [left; reflexivity|exact Hcov].
      - split.
        + rewrite Forall_forall in Hentries |- *.
          intros [i en] Hin. specialize (Hentries (i, en) Hin).
          rewrite Forall_forall in Hkeys.
          assert (Hiall :
              incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
                   (tyreqs (cA Phi))).
          { exact (Hkeys (i, en) Hin). }
          assert (Hi : incl (dep_tys Sg i ++ item_ty_frees i)
                            (tyreqs (cA Phi))).
          { intros u Hu. apply Hiall. apply in_app_or in Hu.
            destruct Hu as [Hu | Hu].
            - apply in_or_app. left. exact Hu.
            - apply in_or_app. right. apply in_or_app. left. exact Hu. }
          eapply (@dentry_agree_retype Sg g i g2 i en).
          * exact WF.
          * exact Hentries.
          * apply Hitem. exact Hi.
          * intros u Hu.
            change
              ((match lookup_subst u g with Some a => a | None => TAbs u end) =
               (match lookup_subst u g2 with Some a => a | None => TAbs u end)).
            rewrite (Hlook u); [reflexivity|].
            apply Hi. apply in_or_app. left. exact Hu.
          * intros rt m th rt' th' Dm Sm a Ei Ei' Hm Ha.
            subst i. simpl in Ei'. injection Ei' as Er Eth.
            subst rt' th'.
            unfold interp_m.
            apply app_subst_frees_agree. intros u Hu.
            symmetry. apply Hlook.
            apply Hiall. apply in_or_app. right. apply in_or_app. right.
            unfold method_item_frees. rewrite Hm.
            apply in_flat_map.
            exists (app_subst (this_subst rt) (app_subst th a)).
            split.
            -- apply in_map_iff. exists a. split; [reflexivity|].
               apply in_or_app. destruct Ha as [Ha | ->].
               ++ left. exact Ha.
               ++ right. simpl. left. reflexivity.
            -- exact Hu.
        + split.
          * simpl. constructor.
            -- intros u Hu. contradiction.
            -- eapply Forall_impl; [|exact Hregs].
               intros i Hi z Hz. right. exact (Hi z Hz).
          * eapply Forall_impl; [|exact Hkeys].
            intros [i en] Hi z Hz. right. exact (Hi z Hz). }
    split; [exact Hg2 | split; [exact Hd2 | exact Hlook]].
  Qed.

  Lemma safety_case_bindty : forall Phi G t tau' e tau d ge g,
      g_ty Sg t = true ->
      t <> this_sym ->
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
    intros Phi G t tau' e tau d ge g Hgt Hneq Hfresh Htau' He Htau
           Hg Hd Hv.
    destruct (@world_bindty Phi t tau' d g Hneq Hfresh Htau' Hg Hd)
      as [Hg2 [Hd2 Hlook]].
    set (g2 := (t, app_subst g (app_subst (cS Phi) tau')) :: g) in *.
    assert (Hnot : ~ In t (tyreqs (cA Phi))).
    { intro H. apply Hfresh. apply safety_case_bindty_aux1. exact H. }
    destruct Hg as [_ [_ [_ [[_ Hran] _]]]].
    assert (Hv2 : venv_agree Sg g2 (cA (bind_ty t tau' Phi)) G ge).
    { destruct Hv as [Hvals Hregs].
      split.
      - intros x a Hx. destruct (Hvals x a Hx) as [w [Hw Hwt]].
        exists w. split; [exact Hw|].
        rewrite (app_subst_frees_agree g2 g a); [exact Hwt|].
        intros u Hu. apply Hlook.
        rewrite Forall_forall in Hregs. apply (Hregs (x, a)).
        + apply safety_case_bindty_aux6. exact Hx.
        + exact Hu.
      - eapply Forall_impl; [|exact Hregs].
        intros [x a] Ha u Hu. right. exact (Ha u Hu). }
    simpl interp.
    pose proof (@IHfuel k (le_n k) (bind_ty t tau' Phi) G e tau
                  d ge g2 He Hg2 Hd2 Hv2) as R.
    assert (Hfree : incl (ty_frees tau) (tyreqs (cA Phi))).
    { apply wf_ty_frees in Htau. exact Htau. }
    rewrite (app_subst_frees_agree (cS (bind_ty t tau' Phi))
                                   (cS Phi) tau) in R.
    - rewrite (app_subst_frees_agree g2 g
                 (app_subst (cS Phi) tau)) in R.
      + exact R.
      + intros u Hu. rewrite Hlook; [reflexivity|].
        apply safety_case_bindty_aux3 with (s := cS Phi) (tau := tau).
        * exact Hfree.
        * exact Hran.
        * exact Hu.
    - intros u Hu. simpl.
      destruct (Nat.eqb u t) eqn:E; [|reflexivity].
      apply Nat.eqb_eq in E. subst. exfalso. apply Hnot. exact (Hfree t Hu).
  Qed.
  (* END:safety_case_bindty *)

  (* Hint: IHfuel evaluates e1 to a value at g (σ tauv); the extended denv
     ((IVal v, DVal w) :: d) agrees with the extended A — the new entry's
     dentry_agree is that vty with absorbs_all collapsing σ; coverage of
     IVal v is the head lookup (item_eqb reflexivity via item_eqb_eq);
     IHfuel at k on the continuation. *)
  (* BEGIN:safety_case_bindval *)
  (* BEGIN:safety_case_bindval *)
  (* The world a val bind leaves, given the value. *)
  Lemma world_bindval : forall Phi v Dv tauv w1 d g,
      g_val Sg v = Some (Dv, tauv) ->
      sat0 Sg Phi Dv ->
      vty Sg w1 (app_subst g tauv) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      ctx_grounded g (add_item (IVal v) Phi)
      /\ denv_agree Sg g (cA (add_item (IVal v) Phi)) ((IVal v, DVal w1) :: d).
  Proof.
    intros Phi v Dv tauv w1 d g Hgv Hsat S1 Hg Hd.
    destruct Hsat as [PhiDv [Htc [_ Hreq]]].
    rewrite Forall_forall in Hreq.
    assert (Hreq' : forall u, In u (tyreqs Dv) -> avail Phi (ITy u)).
    { intros u Hu. apply Hreq. apply (tele_ctx_tyreqs _ Htc). exact Hu. }
    assert (Hd' : denv_agree Sg g (cA (add_item (IVal v) Phi))
                     ((IVal v, DVal w1) :: d)).
    { destruct Hd as [Hcov [Hentries [Hreg Hkeys]]].
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
        + split.
          * simpl. constructor.
            -- intros u Hu.
               simpl in Hu. rewrite Hgv in Hu. simpl in Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | []].
               apply wf_ty_frees_aux3.
               apply avail_ty_in. exact (Hreq' u Hu).
            -- exact Hreg.
          * simpl. constructor.
            -- intros u Hu.
               simpl in Hu. rewrite Hgv in Hu. simpl in Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | []].
               apply wf_ty_frees_aux3.
               apply avail_ty_in. exact (Hreq' u Hu).
            -- exact Hkeys.
    }
    assert (Hg' : ctx_grounded g (add_item (IVal v) Phi)).
    { destruct Hg as [Hgr [Hab2 Hcv]].
      split; [exact Hgr |]. split; simpl; assumption. }
    split; [exact Hg' | exact Hd'].
  Qed.

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
    pose proof Hsat as Hsat0.
    destruct Hsat as [PhiDv [Htc [_ Hreq]]].
    rewrite Forall_forall in Hreq.
    assert (Hreq' : forall u, In u (tyreqs Dv) -> avail Phi (ITy u)).
    { intros u Hu. apply Hreq. apply (tele_ctx_tyreqs _ Htc). exact Hu. }
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    pose proof (IHfuel (le_n k)) as SK.
    pose proof (SK Phi G e1 (app_subst (cS Phi) tauv) d ge g H1 Hg Hd Hv) as S1.
    simpl interp.
    destruct (interp k Sg d ge e1) as [w1 | | ] eqn:E; simpl in S1.
    - rewrite Habs, Habs in S1.
      destruct (@world_bindval Phi v Dv tauv w1 d g Hgv Hsat0 S1 Hg Hd)
        as [Hg' Hd'].
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
           (D0 : tele) (S0 : fnsig) (body0 : expr) (PhiD0 : ctx)
           (gc0 : subst) (dc0 : denv),
      g_fn Sg f0 = Some (Df0, Sf0, None) ->
      g_fn Sg f'0 = Some (D0, S0, Some body0) ->
      tele_ctx D0 = Some PhiD0 ->
      grounding gc0 ->
      covers gc0 (tyreqs (cA PhiD0)) ->
      absorbs gc0 (cS PhiD0) ->
      map (app_subst gc0) (fs_params S0) = map (app_subst g0) (fs_params Sf0) ->
      app_subst gc0 (fs_ret S0) = app_subst g0 (fs_ret Sf0) ->
      denv_agree Sg gc0 (cA PhiD0) dc0 ->
      dentry_agree Sg g0 (IFn f0) (DClo f'0 dc0).
  Proof.
    intros g0 f0 Df0 Sf0 f'0 D0 S0 body0 PhiD0 gc0 dc0
           Hf0 Hf'0 Htc Hgr Hcov Habs Hp Hr Hda.
    simpl. rewrite Hf0, Hf'0. simpl.
    exists PhiD0, gc0.
    split; [exact Htc |].
    split; [exact Hgr |].
    split; [exact Hcov |].
    split; [exact Habs |].
    split; [exact Hp |].
    split; [exact Hr |].
    destruct Hda as [Hc1 [Hc2 [Hreg Hkeys]]].
    split; [exact Hc1 |].
    split; [exact Hkeys |].
    revert Hc2. generalize dc0. intros dd.
    induction dd as [| p r IH]; intros Hall.
    - exact I.
    - simpl. inversion Hall as [| q qs Hq Hqs Heq]; subst.
      split; [exact Hq | apply IH; exact Hqs].
  Qed.

  Lemma safety_case_bindfn_aux5 : forall (D : tele) (i : item),
      app_subst_item (id_app D) i = i.
  Proof.
    intros D i. destruct i as [t | v | f | rt m th | uf]; simpl; try reflexivity.
    f_equal.
    - apply safety_case_bindfn_aux3.
    - unfold map_snd. induction th as [| [a b] r IH]; simpl; [reflexivity |].
      rewrite safety_case_bindfn_aux3, IH. reflexivity.
  Qed.

  (* The world a fn bind leaves: shared by the expression-level rule and a
     functor body's bind. *)
  Lemma world_bindfn : forall Phi f Df Sf f' D' S' body sm d g,
      g_fn Sg f = Some (Df, Sf, None) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi Df ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      sig_eq (cS Phi) S' Sf ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      exists dc, compose_smap d sm = Some dc
      /\ ctx_grounded g (add_item (IFn f) Phi)
      /\ denv_agree Sg g (cA (add_item (IFn f) Phi)) ((IFn f, DClo f' dc) :: d).
  Proof.
    intros Phi f Df Sf f' D' S' body sm d g
           Hf Hf' Hsf Hs' Hsm Hsig Hg Hd.
    pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth Wfun].
    destruct (Wfn f' D' S' (Some body) Hf') as [PhiD [Wt' [Wps' [Wrt' _]]]].
    pose proof (wf_tele_ctx Wt') as Htc.
    destruct Hs' as [PhiD1 [Htc1 [Hsat' Hav']]].
    rewrite Htc in Htc1. injection Htc1 as Htc1. subst PhiD1.
    destruct Hsm as [PhiD2 [Htc2 [Hdom Hmm]]].
    rewrite Htc in Htc2. injection Htc2 as Htc2. subst PhiD2.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    (* the identity application's entries are wf: its range is the available
       type symbols of the provider's merged telescope (sat0's third
       conjunct) *)
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app (cA PhiD))).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    (* satisfaction and the map's correctness, restated at the identity
       application of the *merged* items — extensionally the same
       substitution as id_app D' *)
    assert (Hsat : sat Sg Phi (id_app (cA PhiD)) D').
    { exists PhiD. split; [exact Htc | exact Hsat']. }
    assert (Hsm' : smap_ok Phi (id_app (cA PhiD)) sm D').
    { exists PhiD. split; [exact Htc |]. split; [exact Hdom |].
      eapply Forall_impl; [| exact Hmm].
      intros p [Hin Hmt]. split; [exact Hin |].
      unfold item_matches in Hmt |- *.
      rewrite safety_case_bindfn_aux5 in Hmt.
      rewrite safety_case_bindfn_aux5. exact Hmt. }
    (* the captured world *)
    assert (Hcall : exists dc, compose_smap d sm = Some dc
              /\ ctx_grounded
                   (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD)) PhiD
              /\ denv_agree Sg
                   (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD))
                   (cA PhiD) dc).
    { eapply callee_env_agree;
        [ exact WF | exact Wt' | exact Hsat | exact Hwfth | exact Hsm'
        | exact Hg | exact Hd ]. }
    destruct Hcall as [dc [Hcomp [Hcgc Hdc]]].
    (* reading a PhiD-covered type through the closure's grounding is reading
       it through σ and then g: the application is the identity *)
    assert (Hgcr : forall t0, incl (ty_frees t0) (tyreqs (cA PhiD)) ->
              app_subst (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD)) t0
              = app_subst g (app_subst (cS Phi) t0)).
    { intros t0 Hincl. rewrite callee_grounding_reads by exact Hincl.
      rewrite safety_case_bindfn_aux3. reflexivity. }
    assert (Hpar : map (app_subst
                          (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD)))
                       (fs_params S')
                   = map (app_subst g) (fs_params Sf)).
    { transitivity (map (app_subst g) (map (app_subst (cS Phi)) (fs_params S'))).
      - rewrite map_map. apply map_ext_in. intros tau0 Hin.
        apply Hgcr. rewrite Forall_forall in Wps'.
        pose proof (Wps' tau0 Hin) as Hw0. apply wf_ty_frees in Hw0. exact Hw0.
      - destruct Hsig as [Hsp _]. rewrite Hsp, map_map.
        apply map_ext_in. intros tau0 _. apply Habs. }
    assert (Hrt : app_subst
                    (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD))
                    (fs_ret S')
                  = app_subst g (fs_ret Sf)).
    { assert (Hfr : incl (ty_frees (fs_ret S')) (tyreqs (cA PhiD))).
      { pose proof Wrt' as Hw. apply wf_ty_frees in Hw. exact Hw. }
      rewrite Hgcr by exact Hfr.
      destruct Hsig as [_ Hsr]. rewrite Hsr. apply Habs. }
    assert (Hnew : dentry_agree Sg g (IFn f) (DClo f' dc)).
    { destruct Hcgc as [Hgr0 [Hab0 [Hcov0 _]]].
      eapply safety_case_bindfn_aux4;
        [ exact Hf | exact Hf' | exact Htc | exact Hgr0 | exact Hcov0
        | exact Hab0 | exact Hpar | exact Hrt | exact Hdc ]. }
    assert (Hg' : ctx_grounded g (add_item (IFn f) Phi)).
    { destruct Hg as [Hgr [Hab [Hcv [Hsr Hth]]]].
      split; [exact Hgr |]. split; [exact Hab |].
      split; [exact Hcv |]. split; [exact Hsr | exact Hth]. }
    destruct Hsf as [PhiDf [Htcf [_ Havf]]].
    assert (Hdepf : incl (dep_tys Sg (IFn f) ++ item_ty_frees (IFn f)
                          ++ method_item_frees Sg (IFn f))
                         (tyreqs (cA Phi))).
    { intros u Hu. simpl in Hu. rewrite Hf in Hu. simpl in Hu.
      apply in_app_or in Hu. destruct Hu as [Hu | Hu];
        [| simpl in Hu; contradiction].
      apply wf_ty_frees_aux3. apply avail_ty_in.
      rewrite Forall_forall in Havf. apply Havf.
      apply (proj2 (tele_ctx_tyreqs Df Htcf u)). exact Hu. }
    assert (Hd' : denv_agree Sg g (cA (add_item (IFn f) Phi))
                             ((IFn f, DClo f' dc) :: d)).
    { destruct Hd as [Hdcov [Hdentries [Hdreg Hdkeys]]].
      split.
      - simpl. constructor.
        + right. exists (DClo f' dc). simpl.
          rewrite Nat.eqb_refl. reflexivity.
        + eapply Forall_impl; [| exact Hdcov].
          intros i [Hty | [en Hen]].
          * left; exact Hty.
          * right. simpl.
            destruct (item_eqb i (IFn f)) eqn:Hi.
            -- exists (DClo f' dc). reflexivity.
            -- exists en. exact Hen.
      - split.
        + constructor; [exact Hnew | exact Hdentries].
        + split.
          * simpl. constructor; [exact Hdepf | exact Hdreg].
          * simpl. constructor; [exact Hdepf | exact Hdkeys] . }
    exists dc. split; [exact Hcomp | split; [exact Hg' | exact Hd']].
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
    destruct (@world_bindfn Phi f Df Sf f' D' S' body sm d g
                Hf Hf' Hsf Hs' Hsm Hsig Hg Hd) as [dc [Hcomp [Hg' Hd']]].
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

  Lemma safety_case_bindmth_aux_regular :
    forall Phi rt m Dm Sm th,
      g_mth Sg m = Some (Dm, Sm) ->
      wf_ty Sg Phi rt ->
      sat Sg Phi th Dm ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      incl (method_item_frees Sg (IMth rt m th))
           (tyreqs (cA Phi)).
  Proof.
    intros Phi rt m Dm Sm th Hm Hrt Hsat Hth u Hu.
    unfold method_item_frees in Hu. rewrite Hm in Hu.
    apply in_flat_map in Hu. destruct Hu as [rho [Hrho Hu]].
    apply in_map_iff in Hrho. destruct Hrho as [tau [Heq Htau]].
    subst rho.
    destruct Hsat as [PhiDm [Htcm [Hdom _]]].
    assert (Wtau : wf_ty Sg (add_item (ITy this_sym) PhiDm) tau).
    { destruct (wf_mth_decl WF _ Hm) as [PhiDm2 [Wtm [Wps Wret]]].
      pose proof (wf_tele_ctx Wtm) as Htcm2.
      rewrite Htcm in Htcm2. injection Htcm2 as Heq2. subst PhiDm2.
      apply in_app_or in Htau. destruct Htau as [Htau | Htau].
      - rewrite Forall_forall in Wps. exact (Wps tau Htau).
      - simpl in Htau. destruct Htau as [Htau | []].
        subst tau. exact Wret. }
    apply frees_app_subst_bound in Hu.
    destruct Hu as [[p [Hp Hup]] | [Hu HnotThis]].
    - simpl in Hp. destruct Hp as [Hp | []]. inversion Hp; subst p.
      simpl in Hup. exact (wf_ty_frees Hrt u Hup).
    - apply frees_app_subst_bound in Hu.
      destruct Hu as [[p [Hp Hup]] | [Hu HnotTh]].
      + rewrite Forall_forall in Hth.
        exact (wf_ty_frees (Hth p Hp) u Hup).
      + pose proof (wf_ty_frees Wtau u Hu) as Hu0.
        simpl in Hu0. destruct Hu0 as [Hu0 | Hu0].
        * subst u. exfalso. apply HnotThis. simpl. left. reflexivity.
        * exfalso. apply HnotTh. rewrite Hdom. exact Hu0.
  Qed.

  (* Private helper: the method entry's agreement, assembled from the
     provider closure's grounding, the receiver/argument/result equations and
     the callee world's agreement. *)
  Lemma safety_case_bindmth_aux3 :
    forall (g0 : subst) (rt : ty) (m0 : msym) (th0 : subst) (Dm0 : tele)
           (Sm0 : fnsig) (f'0 : fsym) (D0 : tele) (S0 : fnsig) (body0 : expr)
           (PhiD0 : ctx) (gc0 : subst) (dc0 : denv),
      g_mth Sg m0 = Some (Dm0, Sm0) ->
      g_fn Sg f'0 = Some (D0, S0, Some body0) ->
      tele_ctx D0 = Some PhiD0 ->
      grounding gc0 ->
      covers gc0 (tyreqs (cA PhiD0)) ->
      absorbs gc0 (cS PhiD0) ->
      map (app_subst gc0) (fs_params S0)
        = app_subst g0 rt
          :: map (fun tau => app_subst g0
                    (app_subst (this_subst rt) (app_subst th0 tau)))
                 (fs_params Sm0) ->
      app_subst gc0 (fs_ret S0)
        = app_subst g0
            (app_subst (this_subst rt) (app_subst th0 (fs_ret Sm0))) ->
      denv_agree Sg gc0 (cA PhiD0) dc0 ->
      dentry_agree Sg g0 (IMth rt m0 th0) (DClo f'0 dc0).
  Proof.
    intros g0 rt m0 th0 Dm0 Sm0 f'0 D0 S0 body0 PhiD0 gc0 dc0
           Hm0 Hf'0 Htc0 Hgr Hcov Hab Hp Hr Hda.
    simpl. rewrite Hm0, Hf'0. simpl.
    exists PhiD0, gc0.
    split; [exact Htc0 |].
    split; [exact Hgr |].
    split; [exact Hcov |].
    split; [exact Hab |].
    split; [exact Hp |].
    split; [exact Hr |].
    destruct Hda as [Hc1 [Hc2 [Hreg Hkeys]]].
    split; [exact Hc1 |].
    split; [exact Hkeys |].
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

  Lemma safety_case_bindmth_aux6 : forall (D : tele) (s : subst),
      map_snd (app_subst (id_app D)) s = s.
  Proof.
    intros D s. unfold map_snd. induction s as [| [t a] r IH]; simpl.
    - reflexivity.
    - rewrite safety_case_bindfn_aux3. unfold map_snd in IH. rewrite IH.
      reflexivity.
  Qed.

  Lemma safety_case_bindmth_aux8 : forall (Phi : ctx) (D1 D2 : tele)
                                          (sm : smap) (D : tele),
      smap_ok Phi (id_app D1) sm D -> smap_ok Phi (id_app D2) sm D.
  Proof.
    intros Phi D1 D2 sm D [PhiD [Htc [Hdom Hall]]].
    exists PhiD. split; [exact Htc |]. split; [exact Hdom |].
    eapply Forall_impl; [| exact Hall].
    intros p [Hin Hmatch]. split; [exact Hin |].
    unfold item_matches in Hmatch |- *.
    rewrite safety_case_bindfn_aux5 in Hmatch.
    rewrite safety_case_bindfn_aux5. exact Hmatch.
  Qed.

  Lemma world_bindmth : forall Phi tauk m Dm Sf th f' D' S' body sm
                                     tau0' rest d g,
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
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      exists dc, compose_smap d sm = Some dc
      /\ ctx_grounded g (add_item (IMth tauk m th) Phi)
      /\ denv_agree Sg g (cA (add_item (IMth tauk m th) Phi))
                    ((IMth tauk m th, DClo f' dc) :: d).
  Proof.
    intros Phi tauk m Dm Sf th f' D' S' body sm tau0' rest d g
           Hm Hwk Hsat Hthwf Hf' Hs' Hsm Hpar0 Hteq Hprest Hpret Hg Hd.
    pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth Wfun].
    destruct (Wfn f' D' S' (Some body) Hf') as [PhiD' [Wt' [Wps' [Wrt' _]]]].
    pose proof (wf_tele_ctx Wt') as Htc'.
    (* the provider telescope's own satisfaction, at its merged identity *)
    destruct Hs' as [PhiD'2 [Htc'2 [Hsat' Hav']]].
    assert (EP : PhiD'2 = PhiD') by congruence. subst PhiD'2.
    assert (Hsatfull : sat Sg Phi (id_app (cA PhiD')) D')
      by (exists PhiD'; split; [exact Htc' | exact Hsat']).
    pose proof (@safety_case_bindmth_aux8 Phi D' (cA PhiD') sm D' Hsm) as Hsm2.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    (* the identity application's range is well-formed: S-Pass availability *)
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p))
                           (id_app (cA PhiD'))).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    (* the captured world, and the provider's own grounding *)
    destruct (@callee_env_agree Sg g Phi (id_app (cA PhiD')) sm D' d PhiD'
                WF Wt' Hsatfull Hwfth Hsm2 Hg Hd)
      as [dc [Hcomp [Hcgc Hdc]]].
    assert (Hgcr : forall t0, incl (ty_frees t0) (tyreqs (cA PhiD')) ->
              app_subst (callee_grounding g Phi (id_app (cA PhiD')) (cA PhiD'))
                        t0
              = app_subst g (app_subst (cS Phi) t0)).
    { intros t0 Hincl. rewrite callee_grounding_reads by exact Hincl.
      rewrite safety_case_bindfn_aux3. reflexivity. }
    (* the provider's parameter list splits into receiver and arguments *)
    rewrite Hpar0 in Wps'.
    inversion Wps' as [| tt rr Wp0 Wpr Heqw]; subst.
    (* the two signature equations of the new entry *)
    assert (Hpar : map (app_subst (callee_grounding g Phi
                          (id_app (cA PhiD')) (cA PhiD')))
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
    assert (Hrt : app_subst (callee_grounding g Phi
                     (id_app (cA PhiD')) (cA PhiD')) (fs_ret S')
                  = app_subst g
                      (app_subst (this_subst tauk)
                                 (app_subst th (fs_ret Sf)))).
    { rewrite Hgcr by (exact (wf_ty_frees Wrt')).
      rewrite Hpret. unfold interp_m. rewrite Habs.
      apply safety_case_bindmth_auxN. apply Habs. }
    assert (Hnew : dentry_agree Sg g (IMth tauk m th) (DClo f' dc)).
    { destruct Hcgc as [Hgr0 [Hab0 [Hcov0 _]]].
      eapply safety_case_bindmth_aux3;
        [ exact Hm | exact Hf' | exact Htc' | exact Hgr0 | exact Hcov0
        | exact Hab0 | exact Hpar | exact Hrt | exact Hdc ]. }
    assert (Hg' : ctx_grounded g (add_item (IMth tauk m th) Phi)).
    { destruct Hg as [Hgr [Hab Hcv]].
      split; [exact Hgr |]. split; [exact Hab | exact Hcv]. }
    assert (Hmethodreg :
        incl (method_item_frees Sg (IMth tauk m th))
             (tyreqs (cA Phi))).
    { eapply safety_case_bindmth_aux_regular.
      - exact Hm.
      - exact Hwk.
      - exact Hsat.
      - exact Hthwf. }
    assert (Hd' : denv_agree Sg g (cA (add_item (IMth tauk m th) Phi))
                             ((IMth tauk m th, DClo f' dc) :: d)).
    { destruct Hd as [Hdcov [Hdentries [Hdreg Hdkeys]]].
      split.
      - simpl. constructor.
        + right. exists (DClo f' dc). apply safety_case_bindmth_aux4.
        + eapply Forall_impl; [| exact Hdcov].
          intros i [Hty | [en Hen]].
          * left; exact Hty.
          * right. eapply safety_case_bindmth_aux5. exact Hen.
      - split.
        + constructor; [exact Hnew | exact Hdentries].
        + split.
          * simpl. constructor.
            -- intros u Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | Hu].
               ++ simpl in Hu. destruct Hu.
               ++ apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                  ** apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                     --- exact (wf_ty_frees Hwk u Hu).
                     --- apply in_flat_map in Hu.
                         destruct Hu as [p [Hp Hu]].
                         rewrite Forall_forall in Hthwf.
                         exact (wf_ty_frees (Hthwf p Hp) u Hu).
                  ** exact (Hmethodreg u Hu).
            -- exact Hdreg.
          * simpl. constructor.
            -- intros u Hu.
               apply in_app_or in Hu. destruct Hu as [Hu | Hu].
               ++ simpl in Hu. destruct Hu.
               ++ apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                  ** apply in_app_or in Hu. destruct Hu as [Hu | Hu].
                     --- exact (wf_ty_frees Hwk u Hu).
                     --- apply in_flat_map in Hu.
                         destruct Hu as [p [Hp Hu]].
                         rewrite Forall_forall in Hthwf.
                         exact (wf_ty_frees (Hthwf p Hp) u Hu).
                  ** exact (Hmethodreg u Hu).
            -- exact Hdkeys. }
    exists dc. split; [exact Hcomp | split; [exact Hg' | exact Hd']].
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
    destruct (@world_bindmth Phi tauk m Dm Sf th f' D' S' body sm tau0' rest d g
                Hm Hwk Hsat Hthwf Hf' Hs' Hsm Hpar0 Hteq Hprest Hpret Hg Hd)
      as [dc [Hcomp [Hg' Hd']]].
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

  (* ---- Abstract functors [D78]. -------------------------------------------
     The binding forms leave worlds like a fn bind's; the application is the
     new case.  Its soundness is the "polymorphic provider" argument: the
     caller's world, re-keyed through the application's ς and the binding's
     chain of links, is an agreeing world for the bound functor's own
     arguments (callee_env_agree, once per link, at the identity
     application); the body, checked once against those arguments alone,
     runs there safely (by the same world lemmas as the expression-level
     binds); and what it leaves, restricted to the result, agrees with the
     caller's grounding extended at the result's types by whatever the body
     made them — the existential's witness. *)

  (* Pushing one non-type entry onto an agreeing environment. *)
  Lemma denv_agree_push : forall g A d i en,
      denv_agree Sg g A d ->
      dentry_agree Sg g i en ->
      incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
           (tyreqs A) ->
      denv_agree Sg g (i :: A) ((i, en) :: d).
  Proof.
    intros g A d i en [Hcov [Hent [Hreg Hkeys]]] Hen Hi.
    assert (Hw : incl (tyreqs A) (tyreqs (i :: A))).
    { intros t Ht. unfold tyreqs. simpl. apply in_or_app. right. exact Ht. }
    split; [| split; [| split]].
    - constructor.
      + right. exists en. apply safety_case_bindmth_aux4.
      + eapply Forall_impl; [| exact Hcov].
        intros j [Hty | [en' Hen']]; [left; exact Hty | right].
        eapply safety_case_bindmth_aux5. exact Hen'.
    - constructor; [exact Hen | exact Hent].
    - constructor.
      + intros t Ht. apply Hw. apply Hi. exact Ht.
      + eapply Forall_impl; [| exact Hreg].
        intros j Hj t Ht. apply Hw. exact (Hj t Ht).
    - constructor.
      + intros t Ht. apply Hw. apply Hi. exact Ht.
      + eapply Forall_impl; [| exact Hkeys].
        intros [j e'] Hj t Ht. apply Hw. exact (Hj t Ht).
  Qed.

  (* An entry agrees under any grounding that reads its type material the
     same way. *)
  Lemma dentry_agree_reground : forall g1 g2 i en,
      dentry_agree Sg g1 i en ->
      (forall x, In x (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
                 app_subst g1 (TAbs x) = app_subst g2 (TAbs x)) ->
      dentry_agree Sg g2 i en.
  Proof.
    intros g1 g2 i en Hag Hx.
    apply (@dentry_agree_retype Sg g1 i g2 i en WF Hag).
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

  Lemma venv_agree_reground : forall g g' A A' G ge,
      venv_agree Sg g A G ge ->
      incl (tyreqs A) (tyreqs A') ->
      (forall x, In x (tyreqs A) -> lookup_subst x g' = lookup_subst x g) ->
      venv_agree Sg g' A' G ge.
  Proof.
    intros g g' A A' G ge [Hvals Hregs] Hincl Hlook. split.
    - intros x a Hx. destruct (Hvals x a Hx) as [w [Hw Hwt]].
      exists w. split; [exact Hw |].
      rewrite (app_subst_frees_agree g' g a); [exact Hwt |].
      intros u Hu. apply Hlook. rewrite Forall_forall in Hregs.
      apply (Hregs (x, a)); [apply safety_case_bindty_aux6; exact Hx | exact Hu].
    - eapply Forall_impl; [| exact Hregs].
      intros [x a] Ha u Hu. apply Hincl. exact (Ha u Hu).
  Qed.

  (* One link: satisfying a telescope at the identity application hands it
     an agreeing world, whose grounding reads each of its type symbols as
     the caller's does. *)
  Lemma link_agree : forall Phi Din PhiIn sm d g,
      wf_tele Sg empty_ctx Din PhiIn ->
      sat0 Sg Phi Din ->
      smap_ok Phi (id_app Din) sm Din ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      exists d' g', compose_smap d sm = Some d'
      /\ ctx_grounded g' PhiIn
      /\ denv_agree Sg g' (cA PhiIn) d'
      /\ (forall x, In x (tyreqs (cA PhiIn)) ->
            app_subst g' (TAbs x) = app_subst g (TAbs x))
      /\ incl (tyreqs (cA PhiIn)) (tyreqs (cA Phi)).
  Proof.
    intros Phi Din PhiIn sm d g Wt Hs0 Hsm Hg Hd.
    pose proof (wf_tele_ctx Wt) as Htc.
    pose proof (sat0_tyreqs_incl Hs0 Htc) as Hincl.
    destruct Hs0 as [PhiD1 [Htc1 [Hsat' Hav']]].
    rewrite Htc in Htc1. injection Htc1 as <-.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                              = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app (cA PhiIn))).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    assert (Hsat : sat Sg Phi (id_app (cA PhiIn)) Din)
      by (exists PhiIn; split; [exact Htc | exact Hsat']).
    pose proof (@safety_case_bindmth_aux8 Phi Din (cA PhiIn) sm Din Hsm) as Hsm2.
    destruct (@callee_env_agree Sg g Phi (id_app (cA PhiIn)) sm Din d PhiIn
                WF Wt Hsat Hwfth Hsm2 Hg Hd) as [d' [Hcomp [Hcg Hda]]].
    exists d', (callee_grounding g Phi (id_app (cA PhiIn)) (cA PhiIn)).
    split; [exact Hcomp |]. split; [exact Hcg |]. split; [exact Hda |].
    split; [| exact Hincl].
    intros x Hx.
    rewrite callee_grounding_reads
      by (intros y [Hy | []]; subst y; exact Hx).
    rewrite safety_case_bindfn_aux3. apply Habs.
  Qed.

  (* A binding's chain: link after link, from φ's arguments in to the bound
     functor's own. *)
  Lemma chain_agree : forall h sms u, chain_ok Sg h sms u ->
      forall Du Dru bu PhiU d g,
      g_fun Sg u = Some (Du, Dru, bu) ->
      wf_tele Sg empty_ctx Du PhiU ->
      ctx_grounded g PhiU ->
      denv_agree Sg g (cA PhiU) d ->
      exists Dh Drh bh PhiH d' g',
        g_fun Sg h = Some (Dh, Drh, bh)
        /\ wf_tele Sg empty_ctx Dh PhiH
        /\ run_chain d sms = Some d'
        /\ ctx_grounded g' PhiH
        /\ denv_agree Sg g' (cA PhiH) d'
        /\ (forall x, In x (tyreqs (cA PhiH)) ->
              app_subst g' (TAbs x) = app_subst g (TAbs x))
        /\ incl (tyreqs (cA PhiH)) (tyreqs (cA PhiU)).
  Proof.
    intros h sms u H.
    induction H as [u | h sm r w u Dh Drh bh Dw Drw bw Hh Hw Hl Hch IH];
      intros Du Dru bu PhiU d g Hu Wu Hg Hd.
    - exists Du, Dru, bu, PhiU, d, g.
      split; [exact Hu |]. split; [exact Wu |]. split; [reflexivity |].
      split; [exact Hg |]. split; [exact Hd |].
      split; [intros; reflexivity | intros x Hx; exact Hx].
    - destruct (IH Du Dru bu PhiU d g Hu Wu Hg Hd)
        as [Dw' [Drw' [bw' [PhiW [dW [gW
             [Hw' [Ww [Hrc [HgW [HdW [HrW HiW]]]]]]]]]]]].
      rewrite Hw in Hw'. injection Hw' as <- <- <-.
      destruct (wf_fun_decl WF _ Hh) as [PhiH [Wh _]].
      destruct Hl as [PhiOut [HtcW [Hs0 Hsm]]].
      rewrite (wf_tele_ctx Ww) in HtcW. injection HtcW as <-.
      destruct (@link_agree PhiW Dh PhiH sm dW gW Wh Hs0 Hsm HgW HdW)
        as [d' [g' [Hcomp [Hg' [Hd' [Hr' Hi']]]]]].
      exists Dh, Drh, bh, PhiH, d', g'.
      split; [exact Hh |]. split; [exact Wh |].
      split; [simpl; rewrite Hrc; exact Hcomp |].
      split; [exact Hg' |]. split; [exact Hd' |].
      split.
      + intros x Hx. rewrite (Hr' x Hx). apply HrW. apply Hi'. exact Hx.
      + intros x Hx. apply HiW. apply Hi'. exact Hx.
  Qed.


  (* Rule A helpers: the two readings of a closure's captured entries. *)
  Lemma all_of_Forall : forall (gc0 : subst) (dd : denv),
      Forall (fun p => dentry_agree Sg gc0 (fst p) (snd p)) dd ->
      (fix all (ddd : denv) : Prop :=
         match ddd with
         | [] => True
         | p :: r => dentry_agree Sg gc0 (fst p) (snd p) /\ all r
         end) dd.
  Proof.
    intros gc0 dd. induction dd as [| p r IH]; intros H; [exact I |].
    inversion H; subst. split; [assumption | apply IH; assumption].
  Qed.

  (* Where a symbol of a functor's linked arguments, or of what its result
     reads, comes from: φ's arguments or φ's outputs. *)
  Lemma link_reads_bound : forall u Du Dr h Dh bs sm x,
      g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg h = Some (Dh, Dr, Some bs) ->
      fun_link Sg Dh sm Du ->
      In x (tyreqs Dh ++ res_reads Sg Dr) ->
      In x (tyreqs Du ++ tyreqs Dr).
  Proof.
    intros u Du Dr h Dh bs sm x Hu Hh Hl Hx.
    destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiU [HtcU [WU [_ HregU]]]].
    destruct (@fun_body_regular Sg h Dh Dr bs WF Hh) as [PhiH [_ [HtcH _]]].
    apply in_app_or in Hx. destruct Hx as [Hx | Hx]; apply in_or_app.
    - left. destruct Hl as [PhiOut [HtcO [Hs0 _]]].
      rewrite HtcU in HtcO. injection HtcO as <-.
      apply (proj1 (tele_ctx_tyreqs _ HtcU x)).
      apply (sat0_tyreqs_incl Hs0 HtcH).
      apply (proj2 (tele_ctx_tyreqs _ HtcH x)). exact Hx.
    - unfold res_reads in Hx. apply in_flat_map in Hx. destruct Hx as [i [Hi Hx]].
      assert (Hin : In i (cA PhiU ++ Dr)) by (apply in_or_app; right; exact Hi).
      specialize (HregU i Hin x Hx). rewrite tyreqs_app in HregU.
      apply in_app_or in HregU. destruct HregU as [H | H].
      + left. apply (proj1 (tele_ctx_tyreqs _ HtcU x)). exact H.
      + right. exact H.
  Qed.

  (* The worlds the two binding forms leave.  Rule A: `bind φ = h;` captures
     h's captured inputs here — the analogue of a fn bind. *)
  Lemma world_bindfun : forall Phi u Du Dr h Dh bs sm smc d g,
      g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg h = Some (Dh, Dr, Some bs) ->
      fun_link Sg Dh sm Du ->
      cap_ok Sg Phi Du Dr u h smc ->
      Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      exists dc, compose_smap d smc = Some dc
      /\ dentry_agree Sg g (IFun u) (DFun h [sm] dc)
      /\ ctx_grounded g (add_item (IFun u) Phi)
      /\ denv_agree Sg g (cA (add_item (IFun u) Phi))
                    ((IFun u, DFun h [sm] dc) :: d).
  Proof.
    intros Phi u Du Dr h Dh bs sm smc d g Hu Hh Hl [Hs0 [Hsmc [Hpins Hstat]]] Hfx Hg Hd.
    destruct (@fun_body_regular Sg h Dh Dr bs WF Hh)
      as [PhiH [PhiC [_ [_ [Wc [[Hpd Hpr] _]]]]]].
    destruct (@link_agree Phi _ PhiC smc d g Wc Hs0 Hsmc Hg Hd)
      as [dc [g' [Hcomp [Hg' [Hd' [Hr' _]]]]]].
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0) = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    assert (Hnew : dentry_agree Sg g (IFun u) (DFun h [sm] dc)).
    { simpl. rewrite Hu, Hh. split; [reflexivity |].
      split; [eapply ChCons; [exact Hh | exact Hu | exact Hl | apply ChNil] |].
      destruct Hg' as [Hgr [Hab' [Hcov _]]].
      destruct Hd' as [Hdc1 [Hdc2 [_ Hdc4]]].
      exists PhiC, g'. split; [exact (wf_tele_ctx Wc) |].
      split; [exact Hgr |]. split; [exact Hcov |]. split; [exact Hab' |].
      split.
      { (* the pins hold where h is bound *)
        intros t tau Hlk.
        pose proof (callee_env_agree_aux1 _ _ Hlk) as Hin.
        assert (Ht : In t (tyreqs (cA PhiC)))
          by (apply Hpd; apply in_map_iff; exists (t, tau); split; [reflexivity | exact Hin]).
        destruct (Hcov t Ht) as [a Ha]. rewrite Ha. f_equal.
        transitivity (app_subst g' (TAbs t)); [simpl; rewrite Ha; reflexivity |].
        rewrite (Hr' t Ht).
        rewrite Forall_forall in Hpins. pose proof (Hpins (t, tau) Hin) as Hp.
        cbn [fst snd] in Hp.
        transitivity (app_subst g (app_subst (cS Phi) (TAbs t))); [symmetry; apply Habs |].
        rewrite Hp. rewrite Habs. symmetry. apply dentry_agree_retype_aux1.
        intros z Hz. apply Hr'. rewrite Forall_forall in Hpr.
        exact (Hpr (t, tau) Hin z Hz). }
      split; [exact Hdc1 |]. split; [exact Hdc4 |].
      split; [| exact (@all_of_Forall g' dc Hdc2)].
      intros x Hx Hxr. split; [| exact (Hr' x Hx)].
      apply Hstat; [apply (proj1 (tele_ctx_tyreqs _ (wf_tele_ctx Wc) x)); exact Hx |].
      exact (@link_reads_bound u Du Dr h Dh bs sm x Hu Hh Hl Hxr). }
    exists dc. split; [exact Hcomp |]. split; [exact Hnew |].
    split; [exact Hg |].
    apply denv_agree_push; [exact Hd | exact Hnew |].
    intros x Hx. simpl in Hx. rewrite app_nil_r in Hx.
    rewrite Forall_forall in Hfx. apply wf_ty_frees_aux3. apply avail_ty_in.
    exact (Hfx x Hx).
  Qed.

  Lemma world_bindfuna : forall Phi u Du Dr w Dw sm d g,
      g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg w = Some (Dw, Dr, None) ->
      avail Phi (IFun w) ->
      fun_link Sg Dw sm Du ->
      incl (g_fixed Sg w) (g_fixed Sg u) ->
      Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      exists h sms dc, lookup_denv (IFun w) d = Some (DFun h sms dc)
      /\ ctx_grounded g (add_item (IFun u) Phi)
      /\ denv_agree Sg g (cA (add_item (IFun u) Phi))
                    ((IFun u, DFun h (sms ++ [sm]) dc) :: d).
  Proof.
    intros Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hg Hd.
    pose proof Hd as [Hcov [Hent _]].
    assert (Hin : In (IFun w) (cA Phi)) by (apply avail_fun_in; exact Hav).
    rewrite Forall_forall in Hcov.
    destruct (Hcov _ Hin) as [Hbad | [en Hlk]]; [discriminate Hbad |].
    rewrite Forall_forall in Hent.
    pose proof (Hent _ (lookup_denv_in _ _ Hlk)) as Hag.
    destruct en as [wv | fc dc | h sms dc];
      [ simpl in Hag; contradiction | simpl in Hag; contradiction | ].
    simpl in Hag. rewrite Hw in Hag.
    destruct (g_fun Sg h) as [[[Dh Drh] [bs|]]|] eqn:Hh; try contradiction.
    destruct Hag as [EDr [Hch [PhiC [g' [H1 [H2 [H3 [H4 [H5 [H6 [H7 [HK H8]]]]]]]]]]]].
    subst Drh.
    exists h, sms, dc. split; [exact Hlk |]. split; [exact Hg |].
    apply denv_agree_push; [exact Hd | |].
    - simpl. rewrite Hu, Hh. split; [reflexivity |].
      split; [eapply chain_ok_snoc; [exact Hch | exact Hw | exact Hu | exact Hl] |].
      exists PhiC, g'. repeat (split; [assumption |]). split; [| exact H8].
      intros x Hx Hxr. destruct (HK x Hx Hxr) as [Hxw Heq].
      split; [apply Hxx; exact Hxw | exact Heq].
    - intros x Hx. simpl in Hx. rewrite app_nil_r in Hx.
      rewrite Forall_forall in Hfx. apply wf_ty_frees_aux3. apply avail_ty_in.
      exact (Hfx x Hx).
  Qed.

  (* What a body's run, and an application, promise. *)
  Definition binds_post (Phi Phi' : ctx) (g : subst) (r : sum denv res) : Prop :=
    match r with
    | inl d' => exists g', ctx_grounded g' Phi'
                /\ denv_agree Sg g' (cA Phi') d'
                /\ (forall x, In x (tyreqs (cA Phi)) ->
                      lookup_subst x g' = lookup_subst x g)
    | inr Err => False
    | inr _ => True
    end.

  Definition apply_post (Phi : ctx) (Dr : tele) (g : subst) (d : denv)
      (r : sum denv res) : Prop :=
    match r with
    | inl dr => exists g', ctx_grounded g' (open_res Dr Phi)
                /\ denv_agree Sg g' (cA (open_res Dr Phi)) (dr ++ d)
                /\ (forall x, ~ In x (tyreqs Dr) ->
                      lookup_subst x g' = lookup_subst x g)
    | inr Err => False
    | inr _ => True
    end.

  Lemma binds_post_step : forall Phi Phi1 Phi' g g1 r,
      binds_post Phi1 Phi' g1 r ->
      incl (tyreqs (cA Phi)) (tyreqs (cA Phi1)) ->
      (forall x, In x (tyreqs (cA Phi)) -> lookup_subst x g1 = lookup_subst x g) ->
      binds_post Phi Phi' g r.
  Proof.
    intros Phi Phi1 Phi' g g1 r H Hincl Hl.
    destruct r as [d' | [w | |]]; simpl in *; try assumption.
    destruct H as [g' [H1 [H2 H3]]]. exists g'.
    split; [exact H1 |]. split; [exact H2 |].
    intros x Hx. rewrite (H3 x (Hincl x Hx)). apply Hl. exact Hx.
  Qed.

  (* ---- Rule A: an application at θ, of a binding that captured. ----------
     The caller's world, re-keyed at θ to φ's arguments and down the chain to
     h's linked ones, joined with what h captured where it was bound; the
     fixed-input premise is exactly what makes the two agree where they
     overlap.  The result is filed under its items read at θ. *)
  Lemma apply_step : forall j,
      (forall Phi bs Phi', binds_ty Sg Phi bs Phi' ->
         forall d g, ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
         binds_post Phi Phi' g (interp_binds j Sg d bs)) ->
      forall Phi u Du Dr th sm,
      g_fun Sg u = Some (Du, Dr, None) ->
      avail Phi (IFun u) ->
      sat Sg Phi th Du ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Du ->
      fixed_ok Sg Phi u th ->
      res_coherent Sg Phi th Dr ->
      Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
      forall d g, ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
      apply_post Phi (app_tele th Dr) g d (interp_apply (S j) Sg d u th sm).
  Proof.
    intros j IHb Phi u Du Dr th sm Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh d g Hg Hd.
    simpl interp_apply.
    pose proof Hd as [Hcov [Hent [HAreg Hkeys]]].
    assert (Hin : In (IFun u) (cA Phi)) by (apply avail_fun_in; exact Hav).
    rewrite Forall_forall in Hcov.
    destruct (Hcov _ Hin) as [Hbad | [en Hlk]]; [discriminate Hbad |].
    rewrite Forall_forall in Hent.
    pose proof (Hent _ (lookup_denv_in _ _ Hlk)) as Hag.
    rewrite Hlk.
    destruct en as [wv | fc dc0 | h sms dc];
      [ simpl in Hag; contradiction | simpl in Hag; contradiction | ].
    simpl in Hag. rewrite Hu in Hag.
    destruct (g_fun Sg h) as [[[Dh Drh] [bs|]]|] eqn:Hh; try contradiction.
    destruct Hag as [EDr [Hch [PhiC [gc [HtcC [Hgrc [Hcovc [Habsc [Hpinc
                     [Htcov [Hkeysc [HK Hallc]]]]]]]]]]]].
    subst Drh.
    (* φ's side *)
    destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiU [HtcU [WU [WrU HregU]]]].
    (* the application's own link, at θ *)
    destruct (@callee_env_agree Sg g Phi th sm Du d PhiU WF WU Hsat Hthw Hsm Hg Hd)
      as [d1 [Hc1 [Hg1 Hd1]]].
    rewrite Hc1.
    destruct (@chain_agree h sms u Hch Du Dr None PhiU d1 _ Hu WU Hg1 Hd1)
      as [Dh' [Drh' [bh' [PhiH [d2 [g2 [Hh' [WH [Hrc [Hg2 [Hd2 [Hr2 Hi2]]]]]]]]]]]].
    rewrite Hh in Hh'. injection Hh' as <- <- <-.
    rewrite Hrc.
    destruct (@fun_body_regular Sg h Dh Dr bs WF Hh)
      as [PhiH' [PhiC' [HtcH [WH' [WC [Hpok [WrF [[PhiB [Hbt Hprov]] HregF]]]]]]]].
    assert (EH : PhiH' = PhiH) by (rewrite (wf_tele_ctx WH) in HtcH; congruence).
    subst PhiH'.
    assert (EC : PhiC' = PhiC) by (rewrite (wf_tele_ctx WC) in HtcC; congruence).
    subst PhiC'.
    assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0) = app_subst g t0).
    { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
    assert (Hdomth : map fst th = tyreqs (cA PhiU)).
    { destruct Hsat as [PhiD' [Htc' [Hdom _]]]. rewrite HtcU in Htc'.
      injection Htc' as <-. exact Hdom. }
    assert (HgPhi : forall x, In x (tyreqs (cA PhiH)) ->
              app_subst g2 (TAbs x) = app_subst g (app_subst (cS Phi) (app_subst th (TAbs x)))).
    { intros x Hx. rewrite (Hr2 x Hx). apply callee_grounding_reads.
      intros y [<- | []]. apply Hi2. exact Hx. }
    (* the fixed-input premise, at work: what h captured agrees with the
       application wherever the two meet *)
    assert (HfixK : forall x, In x (tyreqs (cA PhiC)) ->
              In x (tyreqs Dh ++ res_reads Sg Dr) ->
              app_subst gc (TAbs x) = app_subst g (app_subst (cS Phi) (app_subst th (TAbs x)))).
    { intros x Hx Hxr. destruct (HK x Hx Hxr) as [HxX Heq].
      transitivity (app_subst g (TAbs x)); [exact Heq |].
      unfold fixed_ok in Hfix. rewrite Forall_forall in Hfix.
      rewrite (Hfix x HxX). symmetry. apply Habs. }
    (* the body's grounding: linked arguments from the application, the rest
       from the capture *)
    set (gH := map (fun t => (t, app_subst g2 (TAbs t))) (tyreqs (cA PhiH)) ++ gc).
    assert (HgHH : forall x, In x (tyreqs (cA PhiH)) ->
              app_subst gH (TAbs x) = app_subst g2 (TAbs x)).
    { intros x Hx. unfold gH. rewrite app_subst_TAbs.
      rewrite (lookup_prefix_in (fun t => app_subst g2 (TAbs t)) _ x gc Hx). reflexivity. }
    assert (HgHC : forall x, In x (tyreqs (cA PhiC)) ->
              app_subst gH (TAbs x) = app_subst gc (TAbs x)).
    { intros x Hx. destruct (in_dec Nat.eq_dec x (tyreqs (cA PhiH))) as [HxH | HxH].
      - rewrite (HgHH x HxH), (HgPhi x HxH). symmetry. apply HfixK; [exact Hx |].
        apply in_or_app. left. apply (proj1 (tele_ctx_tyreqs _ (wf_tele_ctx WH) x)).
        exact HxH.
      - unfold gH. rewrite !app_subst_TAbs. rewrite (lookup_prefix_notin _ _ x gc HxH). reflexivity. }
    destruct Hg2 as [Hgr2 [Hab2 [Hcv2 [[Hsd2 Hsr2] Hth2]]]].
    assert (HgHcov : covers gH (tyreqs (cA PhiH ++ cA PhiC))).
    { intros x Hx. rewrite tyreqs_app in Hx.
      destruct (in_dec Nat.eq_dec x (tyreqs (cA PhiH))) as [HxH | HxH].
      - eexists. unfold gH. apply lookup_prefix_in. exact HxH.
      - unfold gH. rewrite (lookup_prefix_notin _ _ x gc HxH). apply Hcovc.
        apply in_app_or in Hx. destruct Hx as [Hx | Hx]; [contradiction | exact Hx]. }
    assert (HgHlk : forall x, In x (tyreqs (cA PhiH ++ cA PhiC)) ->
              lookup_subst x gH = Some (app_subst gH (TAbs x))).
    { intros x Hx. destruct (HgHcov x Hx) as [a Ha]. rewrite Ha. simpl. rewrite Ha.
      reflexivity. }
    assert (HsrC : sigma_regular (cS PhiC) (cA PhiC)).
    { apply (tele_sigma_regular WC). split; [intros z [] | constructor]. }
    destruct HsrC as [HsdC HsrC].
    destruct Hpok as [Hpd Hpr].
    assert (HgH : ctx_grounded gH (fun_ctx PhiH PhiC (snd (g_cap Sg h)))).
    { split; [| split; [| split; [| split]]].
      - apply Forall_app. split; [| exact Hgrc].
        apply Forall_forall. intros p Hp. apply in_map_iff in Hp.
        destruct Hp as [t [<- Ht]]. simpl.
        destruct (Hcv2 t Ht) as [a Ha]. rewrite Ha.
        exact (@grounding_lookup_closed g2 t a Hgr2 Ha).
      - intros t tau Hl. simpl in Hl. rewrite !lookup_subst_app in Hl.
        destruct (lookup_subst t (cS PhiH)) as [a|] eqn:E1.
        + injection Hl as <-.
          assert (HtH : In t (tyreqs (cA PhiH))).
          { apply Hsd2. apply in_map_iff. exists (t, a).
            split; [reflexivity | apply callee_env_agree_aux1; exact E1]. }
          rewrite (HgHlk t ltac:(rewrite tyreqs_app; apply in_or_app; left; exact HtH)).
          f_equal. rewrite (HgHH t HtH). simpl. rewrite (Hab2 t a E1).
          apply dentry_agree_retype_aux1. intros z Hz. symmetry. apply HgHH.
          rewrite Forall_forall in Hsr2.
          exact (Hsr2 (t, a) (callee_env_agree_aux1 _ _ E1) z Hz).
        + destruct (lookup_subst t (snd (g_cap Sg h))) as [a|] eqn:E2.
          * injection Hl as <-.
            assert (HtC : In t (tyreqs (cA PhiC))).
            { apply Hpd. apply in_map_iff. exists (t, a).
              split; [reflexivity | apply callee_env_agree_aux1; exact E2]. }
            rewrite (HgHlk t ltac:(rewrite tyreqs_app; apply in_or_app; right; exact HtC)).
            f_equal. rewrite (HgHC t HtC). simpl. rewrite (Hpinc t a E2).
            apply dentry_agree_retype_aux1. intros z Hz. symmetry. apply HgHC.
            rewrite Forall_forall in Hpr.
            exact (Hpr (t, a) (callee_env_agree_aux1 _ _ E2) z Hz).
          * assert (HtC : In t (tyreqs (cA PhiC))).
            { apply HsdC. apply in_map_iff. exists (t, tau).
              split; [reflexivity | apply callee_env_agree_aux1; exact Hl]. }
            rewrite (HgHlk t ltac:(rewrite tyreqs_app; apply in_or_app; right; exact HtC)).
            f_equal. rewrite (HgHC t HtC). simpl. rewrite (Habsc t tau Hl).
            apply dentry_agree_retype_aux1. intros z Hz. symmetry. apply HgHC.
            rewrite Forall_forall in HsrC.
            exact (HsrC (t, tau) (callee_env_agree_aux1 _ _ Hl) z Hz).
      - exact HgHcov.
      - simpl. split.
        + rewrite !map_app, tyreqs_app. intros z Hz. apply in_app_or in Hz.
          destruct Hz as [Hz | Hz]; [apply in_or_app; left; exact (Hsd2 z Hz) |].
          apply in_app_or in Hz.
          destruct Hz as [Hz | Hz]; apply in_or_app; right; [exact (Hpd z Hz) | exact (HsdC z Hz)].
        + rewrite tyreqs_app. apply Forall_app. split.
          { eapply Forall_impl; [| exact Hsr2]. intros p Hp z Hz.
            apply in_or_app. left. exact (Hp z Hz). }
          apply Forall_app. split.
          { eapply Forall_impl; [| exact Hpr]. intros p Hp z Hz.
            apply in_or_app. right. exact (Hp z Hz). }
          { eapply Forall_impl; [| exact HsrC]. intros p Hp z Hz.
            apply in_or_app. right. exact (Hp z Hz). }
      - simpl. rewrite tyreqs_app. intros Hz. apply in_app_or in Hz.
        destruct Hz as [Hz | Hz]; [exact (Hth2 Hz) |].
        exact (this_notin_wf_tele WC (fun H => H) Hz). }
    assert (HdH : denv_agree Sg gH (cA (fun_ctx PhiH PhiC (snd (g_cap Sg h)))) (d2 ++ dc)).
    { destruct Hd2 as [Hc2 [He2 [Hrg2 Hk2]]].
      simpl. split; [| split; [| split]].
      - apply Forall_app. split.
        + eapply Forall_impl; [| exact Hc2]. intros i [Ht | [en He]]; [left; exact Ht | right].
          rewrite lookup_denv_app, He. eexists; reflexivity.
        + unfold tele_covered in Htcov. eapply Forall_impl; [| exact Htcov].
          intros i [Ht | [en He]]; [left; exact Ht | right].
          rewrite lookup_denv_app. destruct (lookup_denv i d2) as [en'|];
            [eexists; reflexivity | eexists; exact He].
      - apply Forall_app. split.
        + rewrite Forall_forall in He2, Hk2 |- *. intros p Hp.
          apply (@dentry_agree_reground g2 gH (fst p) (snd p) (He2 p Hp)).
          intros x Hx. symmetry. apply HgHH. exact (Hk2 p Hp x Hx).
        + pose proof (@safety_case_meth_aux2 gc dc Hallc) as Hallc'.
          rewrite Forall_forall in Hallc', Hkeysc |- *. intros p Hp.
          apply (@dentry_agree_reground gc gH (fst p) (snd p) (Hallc' p Hp)).
          intros x Hx. symmetry. apply HgHC. exact (Hkeysc p Hp x Hx).
      - rewrite tyreqs_app. apply Forall_app. split.
        + eapply Forall_impl; [| exact Hrg2]. intros i Hi x Hx.
          apply in_or_app. left. exact (Hi x Hx).
        + apply Forall_forall. intros i Hi x Hx. apply in_or_app. right.
          exact (@tele_items_regular Sg empty_ctx _ PhiC WF WC
                   (fun k Hk => match Hk with end) i Hi x Hx).
      - rewrite tyreqs_app. apply Forall_app. split.
        + eapply Forall_impl; [| exact Hk2]. intros p Hp x Hx.
          apply in_or_app. left. exact (Hp x Hx).
        + eapply Forall_impl; [| exact Hkeysc]. intros p Hp x Hx.
          apply in_or_app. right. exact (Hp x Hx). }
    (* the body, checked once *)
    pose proof (IHb _ _ _ Hbt (d2 ++ dc) gH HgH HdH) as B.
    destruct (interp_binds j Sg (d2 ++ dc) bs) as [d3 | rr] eqn:Eb.
    2: { destruct rr as [w | | ]; simpl in B |- *; [exact I | exact B | exact I]. }
    destruct B as [gB [HgB [HdB HlB]]].
    assert (Hres : exists dr, compose_smap d3 (inst_smap th Dr) = Some dr).
    { apply compose_smap_defined. apply Forall_forall. intros [a b] Hp.
      destruct (inst_smap_in _ _ _ _ Hp) as [_ [Hb Hnt]]. simpl.
      destruct HdB as [HcB _]. rewrite Forall_forall in HcB, Hprov.
      destruct (HcB b (Hprov b Hb)) as [Hty | Hen]; [| exact Hen].
      rewrite Hty in Hnt. discriminate Hnt. }
    destruct Hres as [dr Hdr]. rewrite Hdr.
    (* the scope's grounding: the caller's, extended at the result's fresh
       types by what the body made them *)
    set (gx := map (fun t => (t, app_subst gB (TAbs t))) (tyreqs Dr)).
    set (g' := gx ++ g).
    assert (Hlg : forall x, ~ In x (tyreqs Dr) -> lookup_subst x g' = lookup_subst x g).
    { intros x Hx. unfold g', gx. apply lookup_prefix_notin. exact Hx. }
    assert (Hfr : forall x, In x (tyreqs (cA Phi)) -> ~ In x (tyreqs Dr)).
    { intros x Hx HxDr. rewrite Forall_forall in Hfresh.
      apply (Hfresh x HxDr). apply safety_case_bindty_aux1. exact Hx. }
    assert (Hreads : forall x, In x (tyreqs (cA Phi)) ->
                      app_subst g (TAbs x) = app_subst g' (TAbs x)).
    { intros x Hx. simpl. rewrite (Hlg x (Hfr x Hx)). reflexivity. }
    assert (HDrB : forall t, In t (tyreqs Dr) -> In t (tyreqs (cA PhiB))).
    { intros t Ht. apply wf_ty_frees_aux3. rewrite Forall_forall in Hprov.
      apply Hprov. apply safety_case_bindty_aux1. exact Ht. }
    destruct Hg as [Hgr [Habg [Hcv [[Hdom Hran] Hthis]]]].
    assert (Hab' : absorbs g' (cS Phi)).
    { intros x a Hl.
      assert (HxPhi : In x (tyreqs (cA Phi))).
      { apply Hdom. apply safety_case_bindty_aux4 with (tau := a). exact Hl. }
      rewrite (Hlg x (Hfr x HxPhi)), (Habg x a Hl). f_equal.
      apply app_subst_frees_agree. intros z Hz. symmetry. apply Hlg.
      apply Hfr. rewrite Forall_forall in Hran.
      exact (Hran (x, a) (safety_case_bindty_aux5 _ _ Hl) z Hz). }
    (* outputs are not φ's inputs; the receiver symbol is in neither *)
    assert (Hout_U : forall t, In t (tyreqs Dr) -> ~ In t (tyreqs (cA PhiU))).
    { intros t Ht Hin'. apply (wf_tele_append_fresh WrU eq_refl t Ht).
      apply safety_case_bindty_aux1. exact Hin'. }
    assert (HthisU : ~ In this_sym (tyreqs (cA PhiU)))
      by exact (this_notin_wf_tele WU (fun H => H)).
    assert (HthisR : ~ In this_sym (tyreqs Dr)).
    { intros Hin'. apply safety_case_bindty_aux1 in Hin'.
      destruct (wf_tele_item_ok WrU _ Hin') as [Phi0 Hok].
      inversion Hok; subst. congruence. }
    assert (Hth_read : forall x, In x (tyreqs (cA PhiU)) ->
              app_subst g' (app_subst th (TAbs x))
              = app_subst g (app_subst (cS Phi) (app_subst th (TAbs x)))).
    { intros x Hx. rewrite Habs.
      rewrite <- Hdomth in Hx. destruct (subst_chain_aux1 x th Hx) as [tx Htx].
      simpl. rewrite Htx. symmetry. apply dentry_agree_retype_aux1.
      intros z Hz. apply Hreads. rewrite Forall_forall in Hthw.
      exact (wf_ty_frees (Hthw (x, tx) (callee_env_agree_aux1 _ _ Htx)) z Hz). }
    (* the body's grounding reads each result item's material as the scope
       reads it through θ *)
    assert (Hagree : forall i, In i Dr -> forall x,
              In x (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
              app_subst gB (TAbs x) = app_subst g' (app_subst th (TAbs x))).
    { intros i Hi x Hx.
      assert (HiF : In i (cA PhiH ++ cA PhiC ++ Dr))
        by (apply in_or_app; right; apply in_or_app; right; exact Hi).
      pose proof (HregF i HiF x Hx) as HxF. rewrite !tyreqs_app in HxF.
      destruct (in_dec Nat.eq_dec x (tyreqs Dr)) as [HxD | HxD].
      - assert (Hnth : ~ In x (map fst th)) by (rewrite Hdomth; exact (Hout_U x HxD)).
        rewrite (app_subst_none th (TAbs x))
          by (intros z [<- | []]; apply lookup_subst_none; exact Hnth).
        unfold g', gx. rewrite !app_subst_TAbs.
        rewrite (lookup_prefix_in (fun t => app_subst gB (TAbs t)) _ x g HxD).
        reflexivity.
      - assert (HxF' : In x (tyreqs (cA PhiH)) \/ In x (tyreqs (cA PhiC))).
        { apply in_app_or in HxF. destruct HxF as [H | H]; [left; exact H |].
          apply in_app_or in H. destruct H as [H | H]; [right; exact H | contradiction]. }
        assert (HlBx : app_subst gB (TAbs x) = app_subst gH (TAbs x)).
        { simpl. rewrite (HlB x); [reflexivity |]. simpl. rewrite tyreqs_app.
          destruct HxF' as [H | H]; apply in_or_app; [left | right]; exact H. }
        rewrite HlBx.
        assert (HxU : In x (tyreqs (cA PhiU))).
        { assert (HiU : In i (cA PhiU ++ Dr)) by (apply in_or_app; right; exact Hi).
          pose proof (HregU i HiU x Hx) as H. rewrite tyreqs_app in H.
          apply in_app_or in H. destruct H as [H | H]; [exact H | contradiction]. }
        rewrite (Hth_read x HxU).
        destruct HxF' as [HxH | HxC].
        + rewrite (HgHH x HxH). apply HgPhi. exact HxH.
        + rewrite (HgHC x HxC). apply HfixK; [exact HxC |].
          apply in_or_app. right. unfold res_reads. apply in_flat_map.
          exists i. split; assumption. }
    assert (Habs' : forall t0, app_subst g' (app_subst (cS Phi) t0) = app_subst g' t0)
      by (apply (absorbs_all Hab')).
    assert (Hthis_reads : forall i, In i Dr -> forall x,
              In x (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
              x <> this_sym).
    { intros i Hi x Hx ->.
      assert (HiU : In i (cA PhiU ++ Dr)) by (apply in_or_app; right; exact Hi).
      pose proof (HregU i HiU this_sym Hx) as H. rewrite tyreqs_app in H.
      apply in_app_or in H. destruct H as [H | H]; [exact (HthisU H) | exact (HthisR H)]. }
    (* each result entry, moved to its key read at θ *)
    assert (Htrans : forall i en, In i Dr -> dentry_agree Sg gB i en ->
               dentry_agree Sg g' (app_subst_item th i) en).
    { intros i en Hi Hag.
      destruct (wf_tele_item_ok WrU i Hi) as [Phi0 Hok].
      apply (@dentry_agree_retype Sg gB i g' (app_subst_item th i) en WF Hag).
      - destruct i as [t|v|f|rt m th0|w]; simpl; try reflexivity.
        f_equal.
        + apply comp_agree. intros z Hz. apply (Hagree _ Hi). simpl.
          apply in_or_app. left. apply in_or_app. left. exact Hz.
        + unfold map_snd. rewrite map_map. apply map_ext_in. intros [a b] Hab. simpl.
          f_equal. apply comp_agree. intros z Hz. apply (Hagree _ Hi). simpl.
          apply in_or_app. left. apply in_or_app. right. apply in_flat_map.
          exists (a, b). split; assumption.
      - intros t Ht.
        assert (Hdep_in : In t (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i))
          by (apply in_or_app; left; exact Ht).
        rewrite (Hagree i Hi t Hdep_in).
        unfold res_coherent in Hcoh. rewrite Forall_forall in Hcoh.
        specialize (Hcoh i Hi). rewrite Forall_forall in Hcoh.
        rewrite <- Habs', (Hcoh t Ht), Habs'. reflexivity.
      - intros rt m th0 rt' th' Dm Sm tau E1 E2 Hm Htau. subst i.
        simpl in E2. injection E2 as <- <-.
        destruct (callee_env_agree_aux11_aux1 Hok Hm) as [PhiDm [HtcDm Hdth0]].
        destruct (wf_mth_decl WF _ Hm) as [PhiDm' [Wt [Hps Hret]]].
        rewrite (wf_tele_ctx Wt) in HtcDm. injection HtcDm as <-.
        assert (Wtau : wf_ty Sg (add_item (ITy this_sym) PhiDm') tau).
        { destruct Htau as [Htau | ->]; [rewrite Forall_forall in Hps; exact (Hps tau Htau) | exact Hret]. }
        assert (Hrt : app_subst gB rt = app_subst g' (app_subst th rt)).
        { apply comp_agree. intros z Hz. apply (Hagree _ Hi). simpl.
          apply in_or_app. left. apply in_or_app. left. exact Hz. }
        unfold interp_m. apply chain3_agree. intros z Hz.
        pose proof (wf_ty_frees Wtau z Hz) as Hz0. simpl in Hz0.
        destruct Hz0 as [Hz0 | Hz0].
        + subst z.
          assert (Hn0 : lookup_subst this_sym th0 = None).
          { apply lookup_subst_none. rewrite Hdth0.
            exact (this_notin_wf_tele Wt (fun H => H)). }
          simpl. rewrite Hn0, lookup_map_snd, Hn0. simpl. exact Hrt.
        + rewrite <- Hdth0 in Hz0. destruct (subst_chain_aux1 z th0 Hz0) as [tz Htz].
          simpl. rewrite Htz, lookup_map_snd, Htz.
          assert (Hnt : forall y, In y (ty_frees tz) -> y <> this_sym).
          { intros y Hy. apply (Hthis_reads _ Hi y). simpl.
            apply in_or_app. left. apply in_or_app. right. apply in_flat_map.
            exists (z, tz). split; [apply callee_env_agree_aux1; exact Htz | exact Hy]. }
          rewrite (app_subst_none (this_subst rt) tz)
            by (intros y Hy; simpl; destruct (Nat.eqb y this_sym) eqn:E;
                [apply Nat.eqb_eq in E; exfalso; exact (Hnt y Hy E) | reflexivity]).
          rewrite (app_subst_none (this_subst (app_subst th rt)) (app_subst th tz)).
          * apply comp_agree. intros y Hy. apply (Hagree _ Hi). simpl.
            apply in_or_app. left. apply in_or_app. right. apply in_flat_map.
            exists (z, tz). split; [apply callee_env_agree_aux1; exact Htz | exact Hy].
          * intros y Hy. simpl. destruct (Nat.eqb y this_sym) eqn:E; [| reflexivity].
            apply Nat.eqb_eq in E. subst y. exfalso.
            destruct (frees_app_subst_bound _ _ _ Hy) as [[p [Hp Hyp]] | [Hy' _]].
            -- rewrite Forall_forall in Hthw. apply Hthis.
               exact (wf_ty_frees (Hthw p Hp) this_sym Hyp).
            -- exact (Hnt this_sym Hy' eq_refl). }
    exists g'.
    split; [| split].
    - (* the extended grounding grounds the opened context *)
      split; [| split; [| split; [| split]]].
      + apply Forall_app. split; [| exact Hgr].
        unfold gx. apply Forall_forall. intros p Hp.
        apply in_map_iff in Hp. destruct Hp as [t [<- Ht]]. simpl.
        destruct HgB as [HgrB [_ [HcvB _]]].
        destruct (HcvB t (HDrB t Ht)) as [a Ha]. rewrite Ha.
        exact (@grounding_lookup_closed gB t a HgrB Ha).
      + exact Hab'.
      + intros x Hx. simpl in Hx. rewrite tyreqs_app, tyreqs_app_tele in Hx.
        apply in_app_or in Hx. destruct Hx as [Hx | Hx].
        * exists (app_subst gB (TAbs x)). unfold g', gx.
          apply lookup_prefix_in. exact Hx.
        * rewrite (Hlg x (Hfr x Hx)). exact (Hcv x Hx).
      + eapply sigma_regular_weaken; [split; [exact Hdom | exact Hran] |].
        apply open_res_tyreqs.
      + simpl. rewrite tyreqs_app, tyreqs_app_tele. intros Hin'. apply in_app_or in Hin'.
        destruct Hin' as [Hin' | Hin']; [exact (HthisR Hin') | exact (Hthis Hin')].
    - (* exactly the result, read at θ, over what was in force *)
      simpl. split; [| split; [| split]].
      + apply Forall_app. split.
        * apply Forall_forall. intros i' Hi'.
          unfold app_tele in Hi'. apply in_map_iff in Hi'. destruct Hi' as [i [<- Hi]].
          destruct (is_ty_item i) eqn:Hti.
          { left. destruct i; try discriminate Hti. reflexivity. }
          right.
          destruct (@lookup_smap_inst th Dr i Hi Hti) as [i'' Hl''].
          destruct (@compose_smap_first d3 (inst_smap th Dr) dr _ i'' Hdr Hl'')
            as [en [_ Hen]].
          exists en. rewrite lookup_denv_app, Hen. reflexivity.
        * apply Forall_forall. intros i Hi.
          destruct (Hcov i Hi) as [Hty | [en Hen]]; [left; exact Hty | right].
          rewrite lookup_denv_app.
          destruct (lookup_denv i dr) as [en'|]; [exists en'; reflexivity |].
          exists en. exact Hen.
      + apply Forall_app. split.
        * apply Forall_forall. intros [i en] Hp.
          destruct (@compose_smap_entry d3 (inst_smap th Dr) dr _ Hdr Hp)
            as [sat' [Hs Hl']].
          simpl in Hs, Hl'.
          destruct (inst_smap_in _ _ _ _ Hs) as [-> [Hi _]].
          destruct HdB as [_ [HeB _]]. rewrite Forall_forall in HeB.
          pose proof (HeB (sat', en) (lookup_denv_in _ _ Hl')) as HagB.
          exact (Htrans sat' en Hi HagB).
        * apply Forall_forall. intros [i en] Hp.
          pose proof (Hent (i, en) Hp) as Hag. simpl in Hag |- *.
          apply (@dentry_agree_reground g g' i en Hag).
          intros x Hx. apply Hreads. rewrite Forall_forall in Hkeys.
          exact (Hkeys (i, en) Hp x Hx).
      + apply Forall_app. split.
        * apply Forall_forall. intros i Hi.
          exact (@open_res_th_regular Sg Phi u Du Dr th WF Hu Hsat Hthw Hcoh
                   (conj Hdom Hran) i Hi).
        * eapply Forall_impl; [| exact HAreg].
          intros i Hi x Hx. apply open_res_tyreqs. exact (Hi x Hx).
      + apply Forall_app. split.
        * apply Forall_forall. intros [i en] Hp.
          destruct (@compose_smap_entry d3 (inst_smap th Dr) dr _ Hdr Hp)
            as [sat' [Hs _]].
          simpl in Hs. destruct (inst_smap_in _ _ _ _ Hs) as [-> [Hi _]]. simpl.
          exact (@open_res_th_regular Sg Phi u Du Dr th WF Hu Hsat Hthw Hcoh
                   (conj Hdom Hran) _ (in_app_tele th Dr sat' Hi)).
        * eapply Forall_impl; [| exact Hkeys].
          intros p Hp x Hx. apply open_res_tyreqs. exact (Hp x Hx).
    - intros x Hx. rewrite tyreqs_app_tele in Hx. exact (Hlg x Hx).
  Qed.

  (* The polymorphic provider [D78] under Rule A, proved with a body's run:
     at any fuel within the strong hypothesis, a well-typed body runs from
     an agreeing world without error to an agreeing world, and an
     application at θ yields exactly its result read at θ, agreeing at the
     caller's grounding extended by the types the body made. *)
  Lemma safety_binds_apply : forall j, j <= k ->
      (forall Phi bs Phi', binds_ty Sg Phi bs Phi' ->
         forall d g, ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
         binds_post Phi Phi' g (interp_binds j Sg d bs))
      /\ (forall Phi u Du Dr th sm,
            g_fun Sg u = Some (Du, Dr, None) ->
            avail Phi (IFun u) ->
            sat Sg Phi th Du ->
            Forall (fun p => wf_ty Sg Phi (snd p)) th ->
            smap_ok Phi th sm Du ->
            fixed_ok Sg Phi u th ->
            res_coherent Sg Phi th Dr ->
            Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
            forall d g, ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
            apply_post Phi (app_tele th Dr) g d (interp_apply j Sg d u th sm)).
  Proof.
    induction j as [| j IHj]; intros Hj.
    { split; intros; simpl; exact I. }
    assert (Hj' : j <= k) by (apply Nat.lt_le_incl; exact Hj).
    destruct (IHj Hj') as [IHb IHa].
    split; [| exact (@apply_step j IHb)].
    (* a body's run: one bind, then the rest at one less fuel *)
    intros Phi bs Phi' Hb.
    destruct Hb as [Phi
      | Phi t tau' r Phi' Hgt Hneq Hfresh Htau' Hr
      | Phi v Dv tauv e1 r Phi' Hgv Hs0 He1 Hr
      | Phi f Df Sf f' D' S' body sm r Phi' Hf Hf' Hsf Hs' Hsm Hsig Hr
      | Phi tauk m Dm Sf th f' D' S' body sm tau0' rest r Phi'
          Hm Hwk Hsat Hthwf Hf' Hs' Hsm Hpar0 Hteq Hprest Hpret Hr
      | Phi u Du Dr h Dh bs sm smc r Phi' Hu Hh Hl Hcap Hfx Hr
      | Phi u Du Dr w Dw sm r Phi' Hu Hw Hav Hl Hxx Hfx Hr
      | Phi u Du Dr th sm r Phi' Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh Hr];
      intros d g Hg Hd; simpl interp_binds.
    - exists g. split; [exact Hg |]. split; [exact Hd |]. intros; reflexivity.
    - destruct (@world_bindty Phi t tau' d g Hneq Hfresh Htau' Hg Hd)
        as [Hg2 [Hd2 Hlook]].
      eapply binds_post_step; [exact (IHb _ _ _ Hr _ _ Hg2 Hd2) | | exact Hlook].
      intros x Hx. simpl. right. exact Hx.
    - assert (Hv0 : venv_agree Sg g (cA Phi) [] []).
      { split; [intros x tau Hx; discriminate Hx | constructor]. }
      pose proof (@IHfuel j Hj' Phi [] e1 (app_subst (cS Phi) tauv) d [] g
                    He1 Hg Hd Hv0) as S1.
      destruct (interp j Sg d [] e1) as [w1 | | ] eqn:E1; simpl in S1.
      + assert (Habs : forall t0, app_subst g (app_subst (cS Phi) t0)
                                  = app_subst g t0).
        { destruct Hg as [_ [Hab _]]. apply (absorbs_all Hab). }
        rewrite Habs, Habs in S1.
        destruct (@world_bindval Phi v Dv tauv w1 d g Hgv Hs0 S1 Hg Hd)
          as [Hg1 Hd1].
        eapply binds_post_step;
          [ exact (IHb _ _ _ Hr _ _ Hg1 Hd1)
          | intros x Hx; exact Hx | intros; reflexivity ].
      + contradiction.
      + exact I.
    - destruct (@world_bindfn Phi f Df Sf f' D' S' body sm d g
                  Hf Hf' Hsf Hs' Hsm Hsig Hg Hd) as [dc [Hcomp [Hg1 Hd1]]].
      rewrite Hcomp.
      eapply binds_post_step;
        [ exact (IHb _ _ _ Hr _ _ Hg1 Hd1)
        | intros x Hx; exact Hx | intros; reflexivity ].
    - destruct (@world_bindmth Phi tauk m Dm Sf th f' D' S' body sm tau0' rest
                  d g Hm Hwk Hsat Hthwf Hf' Hs' Hsm Hpar0 Hteq Hprest Hpret
                  Hg Hd) as [dc [Hcomp [Hg1 Hd1]]].
      rewrite Hcomp.
      eapply binds_post_step;
        [ exact (IHb _ _ _ Hr _ _ Hg1 Hd1)
        | intros x Hx; exact Hx | intros; reflexivity ].
    - destruct (@world_bindfun Phi u Du Dr h Dh bs sm smc d g Hu Hh Hl Hcap Hfx Hg Hd)
        as [dc [Hcomp [_ [Hg1 Hd1]]]].
      rewrite Hcomp.
      eapply binds_post_step;
        [ exact (IHb _ _ _ Hr _ _ Hg1 Hd1)
        | intros x Hx; exact Hx | intros; reflexivity ].
    - destruct (@world_bindfuna Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hg Hd)
        as [h [sms [dc [Hlk [Hg1 Hd1]]]]].
      rewrite Hlk.
      eapply binds_post_step;
        [ exact (IHb _ _ _ Hr _ _ Hg1 Hd1)
        | intros x Hx; exact Hx | intros; reflexivity ].
    - pose proof (IHa Phi u Du Dr th sm Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh d g Hg Hd)
        as A1.
      destruct (interp_apply j Sg d u th sm) as [dr | rr] eqn:Ea.
      + destruct A1 as [g1 [Hg1 [Hd1 Hl1]]].
        eapply binds_post_step;
          [ exact (IHb _ _ _ Hr _ _ Hg1 Hd1) | apply open_res_tyreqs | ].
        intros x Hx. apply Hl1. intros Hin. rewrite tyreqs_app_tele in Hin.
        rewrite Forall_forall in Hfresh. apply (Hfresh x Hin).
        apply safety_case_bindty_aux1. exact Hx.
      + destruct rr as [w | | ]; simpl in A1 |- *; [exact I | exact A1 | exact I].
  Qed.

  (* The case lemmas of the three new rules. *)
  Lemma safety_case_bindfun : forall Phi G u Du Dr h Dh bs sm smc e tau d ge g,
      g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg h = Some (Dh, Dr, Some bs) ->
      fun_link Sg Dh sm Du ->
      cap_ok Sg Phi Du Dr u h smc ->
      Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
      has_ty Sg (add_item (IFun u) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindFun u h sm smc e)).
  Proof.
    intros Phi G u Du Dr h Dh bs sm smc e tau d ge g Hu Hh Hl Hcap Hfx He Hg Hd Hv.
    destruct (@world_bindfun Phi u Du Dr h Dh bs sm smc d g Hu Hh Hl Hcap Hfx Hg Hd)
      as [dc [Hcomp [_ [Hg' Hd']]]].
    simpl interp. rewrite Hcomp.
    exact (@IHfuel k (le_n k) (add_item (IFun u) Phi) G e tau
             ((IFun u, DFun h [sm] dc) :: d) ge g He Hg' Hd' Hv).
  Qed.

  Lemma safety_case_bindfuna : forall Phi G u Du Dr w Dw sm e tau d ge g,
      g_fun Sg u = Some (Du, Dr, None) ->
      g_fun Sg w = Some (Dw, Dr, None) ->
      avail Phi (IFun w) ->
      fun_link Sg Dw sm Du ->
      incl (g_fixed Sg w) (g_fixed Sg u) ->
      Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
      has_ty Sg (add_item (IFun u) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindFunA u w sm e)).
  Proof.
    intros Phi G u Du Dr w Dw sm e tau d ge g Hu Hw Hav Hl Hxx Hfx He Hg Hd Hv.
    destruct (@world_bindfuna Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hg Hd)
      as [h [sms [dc [Hlk [Hg' Hd']]]]].
    simpl interp. rewrite Hlk.
    exact (@IHfuel k (le_n k) (add_item (IFun u) Phi) G e tau
             ((IFun u, DFun h (sms ++ [sm]) dc) :: d) ge g He Hg' Hd' Hv).
  Qed.

  (* The unpack: the scope runs in the opened world, and its result, whose
     type does not mention the fresh types, is read the same way under the
     extended grounding as under the caller's. *)
  Lemma safety_case_apply : forall Phi G u Du Dr th sm e tau d ge g,
      g_fun Sg u = Some (Du, Dr, None) ->
      avail Phi (IFun u) ->
      sat Sg Phi th Du ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Du ->
      fixed_ok Sg Phi u th ->
      res_coherent Sg Phi th Dr ->
      Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
      has_ty Sg (open_res (app_tele th Dr) Phi) G e tau ->
      Forall (fun t => ~ In t (ty_frees tau)) (tyreqs Dr) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g (cA Phi) G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EApply u th sm e)).
  Proof.
    intros Phi G u Du Dr th sm e tau d ge g Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh He Hesc
           Hg Hd Hv.
    destruct (safety_binds_apply (le_n k)) as [_ SA].
    pose proof (SA Phi u Du Dr th sm Hu Hav Hsat Hthw Hsm Hfix Hcoh Hfresh d g Hg Hd) as A1.
    simpl interp.
    destruct (interp_apply k Sg d u th sm) as [dr | rr] eqn:Ea.
    2: { destruct rr as [w | | ]; simpl in A1 |- *; [| exact A1 | exact I].
         exfalso. exact (proj2 (interp_binds_never_ok k) _ _ _ _ _ _ Ea). }
    destruct A1 as [g' [Hg' [Hd' Hl']]].
    assert (Hfr : forall x, In x (tyreqs (cA Phi)) -> ~ In x (tyreqs Dr)).
    { intros x Hx HxDr. rewrite Forall_forall in Hfresh.
      apply (Hfresh x HxDr). apply safety_case_bindty_aux1. exact Hx. }
    assert (Hv' : venv_agree Sg g' (cA (open_res (app_tele th Dr) Phi)) G ge).
    { eapply venv_agree_reground;
        [exact Hv | apply open_res_tyreqs |].
      intros x Hx; apply Hl'; rewrite tyreqs_app_tele; apply Hfr; exact Hx. }
    pose proof (@IHfuel k (le_n k) (open_res (app_tele th Dr) Phi) G e tau (dr ++ d) ge g'
                  He Hg' Hd' Hv') as R.
    change (cS (open_res (app_tele th Dr) Phi)) with (cS Phi) in R.
    rewrite (app_subst_frees_agree g' g (app_subst (cS Phi) tau)) in R;
      [exact R |].
    intros x Hx. apply Hl'. rewrite tyreqs_app_tele. intros HxDr.
    destruct (frees_app_subst_bound _ _ _ Hx) as [[p [Hp Hxp]] | [Hxt _]].
    - destruct Hg as [_ [_ [_ [[_ Hran] _]]]]. rewrite Forall_forall in Hran.
      exact (Hfr x (Hran p Hp x Hxp) HxDr).
    - rewrite Forall_forall in Hesc. exact (Hesc x HxDr Hxt).
  Qed.

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
      - intros; eapply safety_case_bindmth; eauto.
      - intros; eapply safety_case_bindfun; eauto.
      - intros; eapply safety_case_bindfuna; eauto.
      - intros; eapply safety_case_apply; eauto. }
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
  pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth Wfun].
  destruct (Wfn _ _ _ _ Hmain) as [PhiD [Wt (_ & _ & Hb)]].
  (* The empty telescope merges to the empty context. *)
  inversion Wt; subst PhiD.
  specialize (Hb body eq_refl). simpl in Hb.
  pose proof (safety WF fuel) as HS. unfold SAFE in HS.
  specialize (HS empty_ctx [] body TUnit [] [] [] Hb).
  simpl in HS.
  assert (Hg : ctx_grounded [] empty_ctx).
  { split; [constructor|]. split.
    - intros t tau Hl; discriminate Hl.
    - split.
      + intros t Hin; simpl in Hin; contradiction.
      + split.
        * split; [intros t Hin; simpl in Hin; contradiction | constructor].
        * simpl. intros Hin. contradiction. }
  assert (Hd : denv_agree Sg [] [] [])
    by (repeat split; constructor).
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
  - (* EBindFun *) intros u h sm smc e1 IH d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindFun; [eassumption | apply IH; eassumption].
  - (* EBindFunA *) intros u u' sm e1 IH d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindFunA.
    + eassumption.
    + apply IH. eassumption.
  - (* EApply: the body comes from Σ, unerased *) intros u th sm e1 IH d g w Hev.
    inversion Hev; subst. simpl. eapply EvApply.
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
  - intros u h sm smc e1 IH d g w H. simpl in H. inversion H; subst.
    econstructor; [ eassumption | apply IH; eassumption ].
  - intros u u' sm e1 IH d g w H. simpl in H. inversion H; subst.
    econstructor; [ eassumption | apply IH; eassumption ].
  - intros u th sm e1 IH d g w H. simpl in H. inversion H; subst.
    econstructor; [ eassumption | apply IH; eassumption ].
Qed.
(* END:phase_separation_bwd *)

Theorem phase_separation :
  forall Sg d g e w,
    eval Sg d g e w <-> eval Sg d g (erase e) w.
Proof.
  intros; split; [apply phase_separation_fwd | apply phase_separation_bwd].
Qed.


(* ===================== 11c. Rule A (draft D84): the theorems ============= *)

(* A1.  The polymorphic provider with fixed inputs: at any fuel, a body's
   run from an agreeing world ends in an agreeing world (now with the
   capturing BtFun), and an application at θ — fixed inputs at the binding's
   generation, result coherent — yields exactly its result read at θ.  This
   is `safety_binds_apply` discharged by Theorem 4.2's own induction. *)
Theorem polymorphic_provider_fixed : forall Sg, wf_gsig Sg -> forall j,
    (forall Phi bs Phi', binds_ty Sg Phi bs Phi' ->
       forall d g, ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
       binds_post Sg Phi Phi' g (interp_binds j Sg d bs))
    /\ (forall Phi u Du Dr th sm,
          g_fun Sg u = Some (Du, Dr, None) ->
          avail Phi (IFun u) ->
          sat Sg Phi th Du ->
          Forall (fun p => wf_ty Sg Phi (snd p)) th ->
          smap_ok Phi th sm Du ->
          fixed_ok Sg Phi u th ->
          res_coherent Sg Phi th Dr ->
          Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
          forall d g, ctx_grounded g Phi -> denv_agree Sg g (cA Phi) d ->
          apply_post Sg Phi (app_tele th Dr) g d (interp_apply j Sg d u th sm)).
Proof.
  intros Sg WF j.
  exact (@safety_binds_apply Sg WF j (fun i _ => safety WF i) j (le_n j)).
Qed.

(* A2.  Capture at the bind: `bind φ = h;` records exactly h's captured
   inputs as they are here, and the new entry agrees — the analogue of the
   fn bind; and a copy `bind φ' = φ;` carries the capture unchanged. *)
Theorem capture_at_bind : forall Sg, wf_gsig Sg ->
  forall Phi u Du Dr h Dh bs sm smc d g,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg h = Some (Dh, Dr, Some bs) ->
    fun_link Sg Dh sm Du ->
    cap_ok Sg Phi Du Dr u h smc ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    ctx_grounded g Phi ->
    denv_agree Sg g (cA Phi) d ->
    exists dc, compose_smap d smc = Some dc
    /\ dentry_agree Sg g (IFun u) (DFun h [sm] dc).
Proof.
  intros Sg WF Phi u Du Dr h Dh bs sm smc d g Hu Hh Hl Hc Hfx Hg Hd.
  destruct (@world_bindfun Sg WF
              Phi u Du Dr h Dh bs sm smc d g Hu Hh Hl Hc Hfx Hg Hd)
    as [dc [Hcomp [Hag _]]].
  exists dc. split; assumption.
Qed.

Theorem capture_through_copy : forall Sg, wf_gsig Sg ->
  forall Phi u Du Dr w Dw sm d g,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg w = Some (Dw, Dr, None) ->
    avail Phi (IFun w) ->
    fun_link Sg Dw sm Du ->
    incl (g_fixed Sg w) (g_fixed Sg u) ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    ctx_grounded g Phi ->
    denv_agree Sg g (cA Phi) d ->
    exists h sms dc, lookup_denv (IFun w) d = Some (DFun h sms dc)
    /\ dentry_agree Sg g (IFun u) (DFun h (sms ++ [sm]) dc).
Proof.
  intros Sg WF Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hg Hd.
  destruct (@world_bindfuna Sg
              Phi u Du Dr w Dw sm d g Hu Hw Hav Hl Hxx Hfx Hg Hd)
    as [h [sms [dc [Hlk [_ [_ [He _]]]]]]].
  exists h, sms, dc. split; [exact Hlk |]. inversion He; subst. assumption.
Qed.

(* A3.  Fixed inputs are never stale (the designer's question): wherever a
   well-typed application finds a binding, the grounding that binding
   captured reads each fixed input it captured exactly as the application
   holds it — a pinned body runs only at the generation it was checked at.
   (A fixed input the binding did not capture is not read from the capture
   at all; the corrected rule's X(φ) ⊆ M(φ) is what lets this be stated for
   every captured fixed input, with no relevance side condition.) *)
Theorem fixed_inputs_never_stale : forall Sg, wf_gsig Sg ->
  forall Phi G u th sm e tau d g h sms dc,
    has_ty Sg Phi G (EApply u th sm e) tau ->
    ctx_grounded g Phi ->
    denv_agree Sg g (cA Phi) d ->
    lookup_denv (IFun u) d = Some (DFun h sms dc) ->
    exists PhiC g', tele_ctx (fst (g_cap Sg h)) = Some PhiC
    /\ grounding g'
    /\ Forall (fun p => dentry_agree Sg g' (fst p) (snd p)) dc
    /\ absorbs g' (snd (g_cap Sg h))
    /\ (forall x, In x (g_fixed Sg u) -> In x (tyreqs (cA PhiC)) ->
          app_subst g' (TAbs x)
          = app_subst g (app_subst (cS Phi) (app_subst th (TAbs x)))).
Proof.
  intros Sg WF Phi G u th sm e tau d g h sms dc HT Hg Hd Hlk.
  remember (EApply u th sm e) as E eqn:HE.
  induction HT; try discriminate HE.
  - exact (IHHT HE Hg Hd).
  - injection HE as <- <- <- <-.
    destruct Hd as [_ [Hent _]]. rewrite Forall_forall in Hent.
    pose proof (Hent _ (lookup_denv_in _ _ Hlk)) as Hag. simpl in Hag.
    rewrite H in Hag.
    destruct (g_fun Sg h) as [[[Dh Drh] [bs|]]|] eqn:Hh; try contradiction.
    destruct Hag as [EDr [_ [PhiC [g' [HtcC [Hgr [_ [_ [Hpin [_ [_ [HK Hall]]]]]]]]]]]].
    subst Drh.
    exists PhiC, g'. split; [exact HtcC |]. split; [exact Hgr |].
    split; [exact (@safety_case_meth_aux2 Sg g' dc Hall) |].
    split; [exact Hpin |].
    intros x Hx HxC.
    (* X(φ) ⊆ M(φ): a fixed input is read by the result *)
    destruct (wf_fun_decl WF _ H) as [PhiU [_ [Habs_ _]]].
    destruct (Habs_ eq_refl) as [_ [_ [HXM _]]].
    assert (Hr : In x (tyreqs Dh ++ res_reads Sg Dr)).
    { apply in_or_app. right. unfold res_reads. specialize (HXM x Hx).
      apply in_flat_map in HXM. destruct HXM as [i [Hi Hxi]].
      apply in_flat_map. exists i. split; [exact Hi |].
      apply in_app_or in Hxi. destruct Hxi as [Hxi | Hxi];
        apply in_or_app; [left; exact Hxi | right; apply in_or_app; left].
      destruct i; exact Hxi. }
    destruct (HK x HxC Hr) as [_ Heq].
    transitivity (app_subst g (TAbs x)); [exact Heq |].
    unfold fixed_ok in H4. rewrite Forall_forall in H4. rewrite (H4 x Hx).
    destruct Hg as [_ [Hab _]]. symmetry. apply (absorbs_all Hab).
Qed.

(* A6.  Search-freedom: Rule A's new premises are equalities of σ-images
   (and the capture's static condition a list inclusion), so they are
   decidable, and none quantifies over Σ. *)
Lemma ty_eq_dec : forall a b : ty, {a = b} + {a <> b}.
Proof.
  intros a b. destruct (ty_eqb a b) eqn:E.
  - left. apply ty_eqb_eq. exact E.
  - right. intros H. apply ty_eqb_eq in H. congruence.
Defined.

Theorem fixed_ok_decidable : forall Sg Phi u th,
  {fixed_ok Sg Phi u th} + {~ fixed_ok Sg Phi u th}.
Proof.
  intros Sg Phi u th. unfold fixed_ok.
  apply Forall_dec. intros x. apply ty_eq_dec.
Defined.

Theorem res_coherent_decidable : forall Sg Phi th Dr,
  {res_coherent Sg Phi th Dr} + {~ res_coherent Sg Phi th Dr}.
Proof.
  intros Sg Phi th Dr. unfold res_coherent.
  apply Forall_dec. intros i. apply Forall_dec. intros x. apply ty_eq_dec.
Defined.

Theorem pins_hold_decidable : forall (Phi : ctx) (pins : subst),
  {Forall (fun p => app_subst (cS Phi) (TAbs (fst p)) = app_subst (cS Phi) (snd p)) pins}
  + {~ Forall (fun p => app_subst (cS Phi) (TAbs (fst p)) = app_subst (cS Phi) (snd p)) pins}.
Proof. intros Phi pins. apply Forall_dec. intros p. apply ty_eq_dec. Defined.

Theorem capture_fixed_decidable : forall (Dc Du Dr : tele) (X : list tsym),
  {forall x, In x (tyreqs Dc) -> In x (tyreqs Du ++ tyreqs Dr) -> In x X}
  + {~ (forall x, In x (tyreqs Dc) -> In x (tyreqs Du ++ tyreqs Dr) -> In x X)}.
Proof.
  intros Dc Du Dr X.
  destruct (Forall_dec (fun x => In x (tyreqs Du ++ tyreqs Dr) -> In x X)
              (fun x => match in_dec Nat.eq_dec x (tyreqs Du ++ tyreqs Dr) with
                        | left H1 => match in_dec Nat.eq_dec x X with
                                     | left H2 => left (fun _ => H2)
                                     | right H2 => right (fun H => H2 (H H1))
                                     end
                        | right H1 => left (fun H => False_ind _ (H1 H))
                        end) (tyreqs Dc)) as [H | H].
  - left. rewrite Forall_forall in H. exact H.
  - right. intros H'. apply H. rewrite Forall_forall. exact H'.
Defined.


(* A4 (rule level).  Rule A is conservative over D78 rule by rule: an
   application at the identity satisfies exactly the old premises (the
   fixed-input and result-coherence premises are then vacuous, whatever
   X(φ) is); a binding that captures nothing has exactly the old bind
   premise; and at the identity the result is filed under the old keys, and a
   body runs over the old environment (`d2 ++ []`).  With g_fixed ≡ [] the
   other changed definitions (dep_tys, OkFun, BindFunA's inclusion) coincide
   with the old ones too, which is why every pre-existing theorem and example
   re-checked unchanged. *)
Theorem identity_application_conservative : forall Sg Phi u Du Dr PhiU sm,
  tele_ctx Du = Some PhiU ->
  (sat0 Sg Phi Du /\ smap_ok Phi (id_app Du) sm Du)
  <-> (sat Sg Phi (id_app (cA PhiU)) Du
       /\ Forall (fun p => wf_ty Sg Phi (snd p)) (id_app (cA PhiU))
       /\ smap_ok Phi (id_app (cA PhiU)) sm Du
       /\ fixed_ok Sg Phi u (id_app (cA PhiU))
       /\ res_coherent Sg Phi (id_app (cA PhiU)) Dr).
Proof.
  intros Sg Phi u Du Dr PhiU sm Htc. split.
  - intros [[PhiD [Htc' [Hsat Hav]]] Hsm].
    rewrite Htc in Htc'. injection Htc' as <-.
    split; [exists PhiU; split; [exact Htc | exact Hsat] |].
    split.
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t [<- Ht]]. simpl.
      apply WfNeed. rewrite Forall_forall in Hav. exact (Hav t Ht). }
    split; [exact (@safety_case_bindmth_aux8 Phi _ _ sm Du Hsm) |].
    split.
    + unfold fixed_ok. apply Forall_forall. intros x _.
      rewrite safety_case_bindfn_aux3. reflexivity.
    + unfold res_coherent. apply Forall_forall. intros i _.
      apply Forall_forall. intros x _. rewrite safety_case_bindfn_aux3. reflexivity.
  - intros [[PhiD [Htc' Hsat]] [Hwf [Hsm _]]].
    rewrite Htc in Htc'. injection Htc' as <-.
    split; [| exact (@safety_case_bindmth_aux8 Phi _ _ sm Du Hsm)].
    exists PhiU. split; [exact Htc |]. split; [exact Hsat |].
    apply Forall_forall. intros t Ht. rewrite Forall_forall in Hwf.
    assert (Hp : In (t, TAbs t) (id_app (cA PhiU)))
      by (unfold id_app; apply in_map_iff; exists t; split; [reflexivity | exact Ht]).
    pose proof (Hwf _ Hp) as Hw. simpl in Hw. inversion Hw; subst. assumption.
Qed.

Theorem empty_capture_conservative : forall Sg Phi Du Dr u h smc,
  g_cap Sg h = ([], []) ->
  (cap_ok Sg Phi Du Dr u h smc <-> smc = []).
Proof.
  intros Sg Phi Du Dr u h smc Hc. unfold cap_ok. rewrite Hc. simpl. split.
  - intros [_ [[PhiD [Htc [Hdom _]]] _]]. simpl in Htc. injection Htc as <-.
    simpl in Hdom. destruct smc; [reflexivity | discriminate Hdom].
  - intros ->. split.
    + exists empty_ctx. split; [reflexivity |].
      split; [split; [reflexivity | split; constructor] | constructor].
    + split; [exists empty_ctx; split; [reflexivity | split; [reflexivity | constructor]] |].
      split; [constructor | intros x []].
Qed.

Theorem identity_keys_conservative : forall A D,
  inst_smap (id_app A) D = id_smap D /\ app_tele (id_app A) D = D.
Proof.
  intros A D. split.
  - unfold inst_smap, id_smap. apply map_ext. intros i.
    rewrite safety_case_bindfn_aux5. reflexivity.
  - unfold app_tele. rewrite (map_ext _ (fun i => i)) by apply safety_case_bindfn_aux5.
    apply map_id.
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
       else None)
    (fun _ => None)
     (fun _ => []) (fun _ => []) (fun _ => ([], [])).

  (* The program runs, and yields (). *)
  Example runs : interp 1000 Sg [] [] main_body = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  (* Its type-erasure runs to the same value (Proposition 4.3, by
     computation on this instance). *)
  Example erased_runs : interp 1000 Sg [] [] (erase main_body) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

End Example.

(* ===================== 12b. Consistent merging, executed ================= *)

(* The decision log's [D43] example, elaborated into the core by hand and
   *computed*: the flattened concatenation of
       context Ctx1 = A, B, A.gimme[Foo=B];
       context Ctx2 = A, C, A.gimme[Foo=C];
   merges — the duplicate `A` dedups, and unifying the two `A.gimme`
   bindings makes B and C the same type — plus the two failure modes
   (concrete clash; cyclic merge) and a merged program run end to end. *)

Module MergeExample.

  (* Symbols.  (this_sym = 0 is reserved.) *)
  Definition sA : tsym := 1.
  Definition sB : tsym := 2.
  Definition sC : tsym := 3.
  Definition sFoo : tsym := 4.     (* gimme's own requirement *)
  Definition sWElem : tsym := 5.   (* Wrap's payload requirement *)
  Definition nInt : nsym := 1.
  Definition nChar : nsym := 2.
  Definition nWrap : nsym := 3.
  Definition nO : nsym := 4.
  Definition fGive : fsym := 1.
  Definition fDemo : fsym := 2.
  Definition fMain : fsym := 3.
  Definition mGimme : msym := 1.

  (* The two spellings of the colliding requirement: A.gimme[Foo=B] from
     Ctx1 and A.gimme[Foo=C] from Ctx2. *)
  Definition prov_B : item := IMth (TAbs sA) mGimme [(sFoo, TAbs sB)].
  Definition prov_C : item := IMth (TAbs sA) mGimme [(sFoo, TAbs sC)].

  (* Ctx3 = Ctx1, Ctx2, flattened. *)
  Definition d43_tele : tele :=
    [ITy sA; ITy sB; prov_B; ITy sA; ITy sC; prov_C].

  (* ---- 1. The [D43] merge, computed. ------------------------------------ *)

  (* Formation succeeds; the duplicate ITy sA dedups silently; the two
     gimme bindings share the key (mGimme at head A, judged through σ) and
     unify, keeping the earlier occurrence prov_B as the representative
     (deviation 9's orientation) and emitting exactly the merge equation
     C ↦ B — [D43]'s "it just additionally requires B and C to be the
     same type". *)
  Definition d43_merged : ctx :=
    mkCtx [ITy sA; ITy sB; prov_B; ITy sC] [(sC, TAbs sB)].

  Example d43_merges : tele_ctx d43_tele = Some d43_merged.
  Proof. vm_compute. reflexivity. Qed.

  (* Under the merged context the OTHER spelling is available too: prov_C's
     σ-image coincides with the kept prov_B, so the two spellings are one
     atom — [D43]'s "binding one binds them all", and the identity-keying
     payoff of [D62] (two spellings of one merged atom are one provision,
     not an ambiguity). *)
  Example other_spelling_avail : avail_b d43_merged prov_C = true.
  Proof. vm_compute. reflexivity. Qed.

  (* ---- 2. The clash, computed. ------------------------------------------ *)

  (* A merge that would identify two *distinct concrete* heads —
     A.gimme[Foo=Int] with A.gimme[Foo=Char] — is an error at formation,
     and only there ([D43]'s answer to "where do collisions error"). *)
  Definition clash_tele : tele :=
    [ITy sA;
     IMth (TAbs sA) mGimme [(sFoo, TNom nInt [])];
     IMth (TAbs sA) mGimme [(sFoo, TNom nChar [])]].

  Example clash_rejected : tele_ctx clash_tele = None.
  Proof. vm_compute. reflexivity. Qed.

  (* ---- 3. The occurs check, computed. ------------------------------------ *)

  (* A cyclic merge: unifying A.gimme[Foo=B] with A.gimme[Foo=Wrap[Elem=B]]
     would require B ≡ Wrap[Elem=B], a requirement no instantiation can
     ground.  [D43] is silent on this case — the occurs check is a finding
     of this mechanization (deviation 9) — and formation rejects it. *)
  Definition cyclic_tele : tele :=
    [ITy sA; ITy sB;
     IMth (TAbs sA) mGimme [(sFoo, TAbs sB)];
     IMth (TAbs sA) mGimme [(sFoo, TNom nWrap [(sWElem, TAbs sB)])]].

  Example cyclic_rejected : tele_ctx cyclic_tele = None.
  Proof. vm_compute. reflexivity. Qed.

  (* ---- 4. A merged program, run. ----------------------------------------- *)

  (* fn demo(a: A): B { a.gimme() } — declared under the flattened Ctx3, so
     its telescope carries the duplicate key; the elaborated body calls the
     method through the KEPT spelling prov_B, the representative the merged
     context stores. *)
  Definition give_body : expr := ETag nO [] EUnit.  (* fn give(this: O): O *)
  Definition demo_body : expr := EMeth (EVar 0) mGimme prov_B [].
  Definition demo_tele : tele := d43_tele.

  (* ς's domain is the *deduplicated* non-type items (smap_ok reads it off
     filter non-ty (cA PhiD)): here the single representative prov_B — not
     one entry per spelling. *)
  Example smap_domain :
    filter (fun i => negb (is_ty_item i)) (cA d43_merged) = [prov_B].
  Proof. reflexivity. Qed.

  (* The application main passes to demo: defined on the deduplicated type
     requirements [A; B; C] (= tyreqs (cA d43_merged)), sending C to B so
     that merge_respects holds for the equation C ↦ B — binding B bound C
     with it.  (The merge σ story lives in demo's own telescope; main's
     context never mentions C.) *)
  Definition th_demo : subst :=
    [(sA, TAbs sA); (sB, TAbs sB); (sC, TAbs sB)].

  (* fn main() { bind A = O; bind B = O;         -- one bind covers B and C
       bind A.gimme[Foo=B] = give;               -- ONE provision, kept key
       let _ = demo(O()); () } *)
  Definition main_body : expr :=
    EBindTy sA (TNom nO [])
    (EBindTy sB (TNom nO [])
    (EBindMth (TAbs sA) mGimme [(sFoo, TAbs sB)] fGive []
    (ELet 1
       (ECallD fDemo th_demo [(prov_B, prov_B)]
          [ETag nO [] EUnit])
       EUnit))).

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sA || Nat.eqb t sB || Nat.eqb t sC
              || Nat.eqb t sFoo || Nat.eqb t sWElem || Nat.eqb t this_sym)
    (fun n =>
       if Nat.eqb n nInt then Some ([], TUnit)
       else if Nat.eqb n nChar then Some ([], TUnit)
       else if Nat.eqb n nWrap then Some ([ITy sWElem], TAbs sWElem)
       else if Nat.eqb n nO then Some ([], TUnit)
       else None)
    (fun v => None)
    (fun f =>
       if Nat.eqb f fGive then
         Some ([], mkSig [TNom nO []] (TNom nO []), Some give_body)
       else if Nat.eqb f fDemo then
         Some (demo_tele, mkSig [TAbs sA] (TAbs sB), Some demo_body)
       else if Nat.eqb f fMain then
         Some ([], mkSig [] TUnit, Some main_body)
       else None)
    (fun m =>
       if Nat.eqb m mGimme then Some ([ITy sFoo], mkSig [] (TAbs sFoo))
       else None)
    (fun _ => None)
     (fun _ => []) (fun _ => []) (fun _ => ([], [])).

  (* The merged program runs, and yields (): one provision under the kept
     key answers a requirement stated twice, through ς keyed on the
     deduplicated representative. *)
  Example merged_runs : interp 1000 Sg [] [] main_body = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  (* Its type-erasure runs to the same value (Proposition 4.3 on this
     instance): merging is resolved entirely by the statics — at runtime
     only the recorded representative keys remain. *)
  Example merged_erased_runs : interp 1000 Sg [] [] (erase main_body) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  (* Merge keys are the receiver's *type*, not its head [D18, D62].  Two
     instantiations of one nominal receiver are therefore distinct atoms: a
     telescope may require a method at both, and formation keeps them apart
     instead of trying to identify the payloads.  (Under head keying this
     telescope was rejected — the merge would have unified Int with Char.) *)
  Definition nSlot : nsym := 5.
  Definition slot_at (payload : ty) : ty := TNom nSlot [(sWElem, payload)].
  Definition get_int : item := IMth (slot_at (TNom nInt [])) mGimme [].
  Definition get_char : item := IMth (slot_at (TNom nChar [])) mGimme [].

  Example distinct_instantiations_are_distinct_atoms :
    same_key [] get_int get_char = false.
  Proof. vm_compute. reflexivity. Qed.

  Example distinct_instantiations_coexist :
    tele_ctx [get_int; get_char] = Some (mkCtx [get_int; get_char] []).
  Proof. vm_compute. reflexivity. Qed.

End MergeExample.

(* ===================== 12c. Abstract functors, executed [D78] ============= *)

(* An abstract allocator with a type in its result, applied inside a
   function generic over it, and bound to two functors that make the handle
   two different types — the D78 amendment's allocator, distilled:

       functor Alloc: Boxed -> Handle, alloc, Handle.load;
       functor Wrap: Boxed -> Handle, alloc, Handle.load {
         bind Handle = Box[Elem = Boxed];  bind alloc = box_it;
         bind Handle.load = unbox;  bind secret = O;   # not in the result
       }
       functor Bare: Boxed -> Handle, alloc, Handle.load {
         bind Handle = Boxed;  bind alloc = id;  bind Handle.load = id_m;
       }
       assume Boxed, Alloc {
         fn roundtrip(x: Boxed): Boxed { bind Alloc; alloc(x).load() }
       }
       fn main() {
         bind Boxed = O;
         bind Alloc = Wrap;   let y = roundtrip(O);
         bind Alloc2 = Bare;  bind Alloc = Alloc2;   roundtrip(y)
       }

   `roundtrip` is checked once, against a rigid Handle; each run takes the
   handle type its binding makes. *)

Module FunctorExample.

  Definition sBoxed : tsym := 1.
  Definition sHandle : tsym := 2.
  Definition sElem : tsym := 3.
  Definition nBox : nsym := 1.
  Definition nO : nsym := 2.
  Definition vSecret : vsym := 1.
  Definition fAlloc : fsym := 1.
  Definition fBoxIt : fsym := 2.
  Definition fUnbox : fsym := 3.
  Definition fId : fsym := 4.
  Definition fRound : fsym := 5.
  Definition fMain : fsym := 6.
  Definition mLoad : msym := 1.
  Definition uAlloc : usym := 1.
  Definition uAlloc2 : usym := 2.
  Definition uWrap : usym := 3.
  Definition uBare : usym := 4.

  Definition th_boxed : subst := [(sBoxed, TAbs sBoxed)].
  Definition box_ty : ty := TNom nBox [(sElem, TAbs sBoxed)].
  Definition load_item : item := IMth (TAbs sHandle) mLoad th_boxed.

  (* Alloc's (and every binding's) result. *)
  Definition alloc_res : tele := [ITy sHandle; IFn fAlloc; load_item].

  Definition wrap_body : list bnd :=
    [ BTy sHandle box_ty;
      BFn fAlloc fBoxIt [];
      BMth (TAbs sHandle) mLoad th_boxed fUnbox [];
      BVal vSecret (ETag nO [] EUnit) ].

  Definition bare_body : list bnd :=
    [ BTy sHandle (TAbs sBoxed);
      BFn fAlloc fId [];
      BMth (TAbs sHandle) mLoad th_boxed fId [] ].

  Definition round_body : expr :=
    EApply uAlloc th_boxed [] (EMeth (ECallA fAlloc [EVar 0]) mLoad load_item []).

  Definition round_call (arg : expr) : expr :=
    ECallD fRound th_boxed [(IFun uAlloc, IFun uAlloc)] [arg].

  Definition main_body : expr :=
    EBindTy sBoxed (TNom nO [])
    (EBindFun uAlloc uWrap [] []
    (ELet 10 (round_call (ETag nO [] EUnit))
    (EBindFun uAlloc2 uBare [] []
    (EBindFunA uAlloc uAlloc2 []
    (round_call (EVar 10)))))).

  (* `main` itself is unit-valued: it runs the above and drops the value. *)
  Definition main_unit : expr := ELet 11 main_body EUnit.

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sBoxed || Nat.eqb t sHandle || Nat.eqb t sElem
              || Nat.eqb t this_sym)
    (fun n =>
       if Nat.eqb n nBox then Some ([ITy sElem], TAbs sElem)
       else if Nat.eqb n nO then Some ([], TUnit)
       else None)
    (fun v => if Nat.eqb v vSecret then Some ([], TNom nO []) else None)
    (fun f =>
       if Nat.eqb f fAlloc then
         Some ([ITy sBoxed; ITy sHandle], mkSig [TAbs sBoxed] (TAbs sHandle), None)
       else if Nat.eqb f fBoxIt then
         Some ([ITy sBoxed], mkSig [TAbs sBoxed] box_ty,
               Some (ETag nBox [(sElem, TAbs sBoxed)] (EVar 0)))
       else if Nat.eqb f fUnbox then
         Some ([ITy sBoxed], mkSig [box_ty] (TAbs sBoxed),
               Some (EMatch (EVar 0) [(nBox, 1, EVar 1)]))
       else if Nat.eqb f fId then
         Some ([ITy sBoxed], mkSig [TAbs sBoxed] (TAbs sBoxed), Some (EVar 0))
       else if Nat.eqb f fRound then
         Some ([ITy sBoxed; IFun uAlloc], mkSig [TAbs sBoxed] (TAbs sBoxed),
               Some round_body)
       else if Nat.eqb f fMain then
         Some ([], mkSig [] TUnit, Some main_unit)
       else None)
    (fun m =>
       if Nat.eqb m mLoad then Some ([ITy sBoxed], mkSig [] (TAbs sBoxed))
       else None)
    (fun u =>
       if Nat.eqb u uAlloc then Some ([ITy sBoxed], alloc_res, None)
       else if Nat.eqb u uAlloc2 then Some ([ITy sBoxed], alloc_res, None)
       else if Nat.eqb u uWrap then Some ([ITy sBoxed], alloc_res, Some wrap_body)
       else if Nat.eqb u uBare then Some ([ITy sBoxed], alloc_res, Some bare_body)
       else None)
     (fun _ => [])
     (fun u => if Nat.eqb u uAlloc || Nat.eqb u uAlloc2 then [sBoxed] else [])
     (fun _ => ([], [])).

  (* Both runs give the value back: once through a Box handle, once through
     the bare one, with one generic `roundtrip`. *)
  Example runs : interp 1000 Sg [] [] main_body = Ok (VTag nO VUnit).
  Proof. vm_compute. reflexivity. Qed.

  Example erased_runs :
    interp 1000 Sg [] [] (erase main_body) = Ok (VTag nO VUnit).
  Proof. vm_compute. reflexivity. Qed.

  (* An application keeps exactly the result: Wrap's `secret` is bound in its
     body and gone after `bind Alloc;`, whichever binding answers. *)
  Example only_the_result :
    match interp_apply 100 Sg [(IFun uAlloc, DFun uWrap [[]] [])] uAlloc th_boxed [] with
    | inl dr => map fst dr
    | inr _ => []
    end = [IFn fAlloc; load_item].
  Proof. vm_compute. reflexivity. Qed.

  (* A copied binding carries its chain: one link per `bind φ = ψ;`. *)
  Example copied_binding :
    interp_apply 100 Sg [(IFun uAlloc, DFun uBare [[]; []] [])] uAlloc th_boxed []
    = interp_apply 100 Sg [(IFun uAlloc, DFun uBare [[]] [])] uAlloc th_boxed [].
  Proof. vm_compute. reflexivity. Qed.


  (* ---- The program is well-formed. ----------------------------------------
     A typing derivation for the whole signature — the bodies of both
     bindings against `Boxed` alone, `roundtrip`'s application with its rigid
     `Handle`, and `main`'s binds, copy and calls — so the new rules are
     satisfiable on a program that uses every form but a nested application,
     and the soundness theorem covers its runs. *)

  Definition PhiA : ctx := mkCtx [ITy sBoxed] [].

  Lemma wf_box : forall Phi, avail Phi (ITy sBoxed) -> wf_ty Sg Phi box_ty.
  Proof.
    intros Phi H. eapply WfTag; [reflexivity | | ].
    - eexists. split; [reflexivity |]. split; [reflexivity |]. split.
      + repeat constructor; left; reflexivity.
      + constructor.
    - constructor; [apply WfNeed; exact H | constructor].
  Qed.

  Lemma wf_O : forall Phi, wf_ty Sg Phi (TNom nO []).
  Proof.
    intros Phi. eapply WfTag; [reflexivity | | constructor].
    exists empty_ctx. split; [reflexivity |]. split; [reflexivity |]. split; constructor.
  Qed.

  Lemma sat0_boxed : forall Phi, avail Phi (ITy sBoxed) -> sat0 Sg Phi [ITy sBoxed].
  Proof.
    intros Phi H. exists PhiA. split; [reflexivity |]. split.
    - split; [reflexivity |]. split; [repeat constructor; left; reflexivity | constructor].
    - constructor; [exact H | constructor].
  Qed.

  Lemma sat0_nil : forall Phi, sat0 Sg Phi [].
  Proof.
    intros Phi. exists empty_ctx. split; [reflexivity |].
    split; [split; [reflexivity | split; constructor] | constructor].
  Qed.

  Lemma tele_boxed : wf_tele Sg empty_ctx [ITy sBoxed] PhiA.
  Proof.
    eapply WCons; [| reflexivity | constructor].
    constructor; [reflexivity | discriminate].
  Qed.

  Lemma sat_boxed_id : forall Phi, avail Phi (ITy sBoxed) ->
    sat Sg Phi th_boxed [ITy sBoxed] /\ Forall (fun p => wf_ty Sg Phi (snd p)) th_boxed.
  Proof.
    intros Phi H. split.
    - exists PhiA. split; [reflexivity |]. split; [reflexivity |].
      split; [repeat constructor; left; reflexivity | constructor].
    - constructor; [apply WfNeed; exact H | constructor].
  Qed.

  Lemma tele_res : wf_tele Sg PhiA alloc_res (mkCtx (cA PhiA ++ alloc_res) (cS PhiA)).
  Proof.
    eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
    eapply WCons; [| reflexivity |].
    { eapply OkFn; [reflexivity |].
      exists (mkCtx [ITy sBoxed; ITy sHandle] []). split; [reflexivity |].
      split; [split; [reflexivity | split; [repeat constructor; left; reflexivity | constructor]]
             | repeat constructor]. }
    eapply WCons; [| reflexivity | constructor].
    destruct (@sat_boxed_id (mkCtx [ITy sBoxed; ITy sHandle; IFn fAlloc] []) eq_refl)
      as [Hs Hw].
    eapply OkMth; [reflexivity | apply WfNeed; reflexivity | exact Hs | exact Hw].
  Qed.

  Lemma conv : forall Phi G e t t',
    has_ty Sg Phi G e t -> t = t' -> has_ty Sg Phi G e t'.
  Proof. intros; subst; assumption. Qed.

  Lemma tag_O : forall Phi G, has_ty Sg Phi G (ETag nO [] EUnit) (TNom nO []).
  Proof.
    intros Phi G. eapply conv.
    - eapply TTagR with (D := []) (tau0 := TUnit); [reflexivity | | constructor |].
      + exists empty_ctx. split; [reflexivity |]. split; [reflexivity | split; constructor].
      + apply conv with (t := TUnit); [constructor | reflexivity].
    - unfold app_subst. simpl. destruct Phi. reflexivity.
  Qed.

  Definition PhiW1 : ctx := bind_ty sHandle box_ty PhiA.

  Lemma wrap_ok : exists PhiB, binds_ty Sg PhiA wrap_body PhiB
                  /\ Forall (fun i => In i (cA PhiB)) alloc_res.
  Proof.
    eexists. split.
    - eapply BtTy; [reflexivity | discriminate | simpl; intros [H | []]; discriminate
                   | apply wf_box; reflexivity |].
      eapply BtFn with (Df := [ITy sBoxed; ITy sHandle]) (D' := [ITy sBoxed]);
        [reflexivity | reflexivity | | apply sat0_boxed; reflexivity | | |].
      + exists (mkCtx [ITy sBoxed; ITy sHandle] []). split; [reflexivity |].
        split; [split; [reflexivity | split; [repeat constructor; left; reflexivity | constructor]]
               | repeat constructor].
      + exists PhiA. split; [reflexivity |]. split; [reflexivity | constructor].
      + split; reflexivity.
      + destruct (@sat_boxed_id (add_item (IFn fAlloc) PhiW1) eq_refl) as [Hs Hw].
        eapply BtMth with (Dm := [ITy sBoxed]) (D' := [ITy sBoxed]) (tau0' := box_ty)
                          (rest := []);
          [reflexivity | apply WfNeed; reflexivity | exact Hs | exact Hw | reflexivity
          | apply sat0_boxed; reflexivity
          | exists PhiA; split; [reflexivity | split; [reflexivity | constructor]]
          | reflexivity | reflexivity | reflexivity | reflexivity |].
        eapply BtVal with (Dv := []);
          [reflexivity | apply sat0_nil | apply tag_O | apply BtNil].
    - simpl. repeat constructor; simpl; tauto.
  Qed.

  Lemma bare_ok : exists PhiB, binds_ty Sg PhiA bare_body PhiB
                  /\ Forall (fun i => In i (cA PhiB)) alloc_res.
  Proof.
    eexists. split.
    - eapply BtTy; [reflexivity | discriminate | simpl; intros [H | []]; discriminate
                   | apply WfNeed; reflexivity |].
      eapply BtFn with (Df := [ITy sBoxed; ITy sHandle]) (D' := [ITy sBoxed]);
        [reflexivity | reflexivity | | apply sat0_boxed; reflexivity | | |].
      + exists (mkCtx [ITy sBoxed; ITy sHandle] []). split; [reflexivity |].
        split; [split; [reflexivity | split; [repeat constructor; left; reflexivity | constructor]]
               | repeat constructor].
      + exists PhiA. split; [reflexivity |]. split; [reflexivity | constructor].
      + split; reflexivity.
      + destruct (@sat_boxed_id
                    (add_item (IFn fAlloc) (bind_ty sHandle (TAbs sBoxed) PhiA)) eq_refl)
          as [Hs Hw].
        eapply BtMth with (Dm := [ITy sBoxed]) (D' := [ITy sBoxed]) (tau0' := TAbs sBoxed)
                          (rest := []);
          [reflexivity | apply WfNeed; reflexivity | exact Hs | exact Hw | reflexivity
          | apply sat0_boxed; reflexivity
          | exists PhiA; split; [reflexivity | split; [reflexivity | constructor]]
          | reflexivity | reflexivity | reflexivity | reflexivity | apply BtNil].
    - simpl. repeat constructor; simpl; tauto.
  Qed.

  Definition PhiR : ctx := mkCtx [ITy sBoxed; IFun uAlloc] [].

  Lemma round_typed :
    has_ty Sg PhiR (body_env (mkSig [TAbs sBoxed] (TAbs sBoxed))) round_body (TAbs sBoxed).
  Proof.
    eapply TApplyR with (Du := [ITy sBoxed]) (Dr := alloc_res).
    - reflexivity.
    - reflexivity.
    - exact (proj1 (@sat_boxed_id PhiR eq_refl)).
    - exact (proj2 (@sat_boxed_id PhiR eq_refl)).
    - exists PhiA. split; [reflexivity | split; [reflexivity | constructor]].
    - constructor.
    - unfold res_coherent. repeat constructor.
    - simpl. constructor; [| constructor].
      intros [H | [H | []]]; discriminate.
    - eapply conv.
      + eapply TMethR with (Dm := [ITy sBoxed]) (S := mkSig [] (TAbs sBoxed))
                           (tau0 := TAbs sHandle).
        * reflexivity.
        * eapply conv.
          -- eapply TNeedR with (Df := [ITy sBoxed; ITy sHandle])
                                (S := mkSig [TAbs sBoxed] (TAbs sHandle));
               [reflexivity | reflexivity |].
             constructor; [| constructor]. constructor. reflexivity.
          -- reflexivity.
        * simpl. right. right. left. reflexivity.
        * reflexivity.
        * intros i1 Hin [th' Hp]. simpl in Hin.
          destruct Hin as [<- | [<- | [<- | [<- | [<- | []]]]]]; try discriminate.
          reflexivity.
        * constructor.
      + reflexivity.
    - simpl. constructor; [| constructor]. intros [H | []]; discriminate.
  Qed.

  Lemma boxit_typed :
    has_ty Sg PhiA (body_env (mkSig [TAbs sBoxed] box_ty))
           (ETag nBox [(sElem, TAbs sBoxed)] (EVar 0)) box_ty.
  Proof.
    eapply conv.
    - eapply TTagR with (D := [ITy sElem]) (tau0 := TAbs sElem); [reflexivity | | |].
      + eexists. split; [reflexivity |]. split; [reflexivity |].
        split; [repeat constructor; left; reflexivity | constructor].
      + constructor; [apply WfNeed; reflexivity | constructor].
      + constructor. reflexivity.
    - reflexivity.
  Qed.

  Lemma unbox_typed :
    has_ty Sg PhiA (body_env (mkSig [box_ty] (TAbs sBoxed)))
           (EMatch (EVar 0) [(nBox, 1, EVar 1)]) (TAbs sBoxed).
  Proof.
    eapply TMatchR with (tau0 := box_ty) (ms := [box_ty]).
    - constructor. reflexivity.
    - apply wf_box. reflexivity.
    - reflexivity.
    - constructor; [intros [] | constructor].
    - constructor; [| constructor]. split; [reflexivity |]. eexists; eexists; reflexivity.
    - constructor; [| constructor]. constructor. reflexivity.
  Qed.

  Lemma id_typed :
    has_ty Sg PhiA (body_env (mkSig [TAbs sBoxed] (TAbs sBoxed))) (EVar 0) (TAbs sBoxed).
  Proof. constructor. reflexivity. Qed.

  Lemma link_boxed : fun_link Sg [ITy sBoxed] [] [ITy sBoxed].
  Proof.
    exists PhiA. split; [reflexivity |]. split.
    - apply sat0_boxed. reflexivity.
    - exists PhiA. split; [reflexivity | split; [reflexivity | constructor]].
  Qed.

  Lemma call_round : forall Phi G arg,
    avail Phi (ITy sBoxed) -> In (IFun uAlloc) (cA Phi) ->
    app_subst (cS Phi) (TAbs sBoxed) = TNom nO [] ->
    has_ty Sg Phi G arg (TNom nO []) ->
    has_ty Sg Phi G (round_call arg) (TNom nO []).
  Proof.
    intros Phi G arg Hb Hu Hs Harg.
    assert (Hau : avail Phi (IFun uAlloc)).
    { unfold avail, avail_b. apply existsb_exists. exists (IFun uAlloc).
      split; [exact Hu | reflexivity]. }
    eapply conv.
    - eapply TCallR with (Df := [ITy sBoxed; IFun uAlloc])
                         (S := mkSig [TAbs sBoxed] (TAbs sBoxed)) (body := round_body).
      + reflexivity.
      + eexists. split; [reflexivity |]. split; [reflexivity |].
        split; [| constructor].
        constructor; [left; reflexivity |].
        constructor; [| constructor].
        right. split; [exact Hau | constructor].
      + constructor; [apply WfNeed; exact Hb | constructor].
      + eexists. split; [reflexivity |]. split; [reflexivity |].
        constructor; [| constructor]. split; [exact Hu | reflexivity].
      + constructor; [| constructor]. eapply conv; [exact Harg |]. simpl. symmetry. exact Hs.
    - simpl. exact Hs.
  Qed.

  (* Neither binding captures anything (Rule A's degenerate case). *)
  Lemma cap_nil : forall Phi Du Dr u h smc, smc = [] -> g_cap Sg h = ([], []) ->
    cap_ok Sg Phi Du Dr u h smc.
  Proof.
    intros Phi Du Dr u h smc -> Hc. unfold cap_ok. rewrite Hc. simpl.
    split; [apply sat0_nil |]. split.
    - exists empty_ctx. split; [reflexivity | split; [reflexivity | constructor]].
    - split; [constructor | intros x []].
  Qed.

  Lemma main_typed :
    has_ty Sg empty_ctx (body_env (mkSig [] TUnit)) main_unit TUnit.
  Proof.
    eapply TLetR with (tau1 := TNom nO []); [| constructor].
    eapply TBindTyR; [reflexivity | discriminate | simpl; tauto | apply wf_O | | apply wf_O].
    eapply TBindFunR with (Du := [ITy sBoxed]) (Dr := alloc_res) (Dh := [ITy sBoxed]);
      [reflexivity | reflexivity | exact link_boxed | apply cap_nil; reflexivity | constructor |].
    eapply TLetR.
    { apply call_round; [reflexivity | simpl; tauto | reflexivity | apply tag_O]. }
    eapply TBindFunR with (Du := [ITy sBoxed]) (Dr := alloc_res) (Dh := [ITy sBoxed]);
      [reflexivity | reflexivity | exact link_boxed | apply cap_nil; reflexivity | constructor |].
    eapply TBindFunAR with (Du := [ITy sBoxed]) (Dr := alloc_res) (Dw := [ITy sBoxed]);
      [reflexivity | reflexivity | reflexivity | exact link_boxed
      | intros x [] | constructor |].
    apply call_round; [reflexivity | simpl; tauto | reflexivity |].
    constructor. reflexivity.
  Qed.

  Lemma tele_round : wf_tele Sg empty_ctx [ITy sBoxed; IFun uAlloc] PhiR.
  Proof.
    eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
    eapply WCons; [eapply OkFun; [reflexivity | constructor] | reflexivity | constructor].
  Qed.

  Lemma tele_alloc : wf_tele Sg empty_ctx [ITy sBoxed; ITy sHandle]
                       (mkCtx [ITy sBoxed; ITy sHandle] []).
  Proof.
    eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
    eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
  Qed.

  Lemma wf_sig : wf_gsig Sg.
  Proof.
    constructor.
    - intros n D taup H.
      destruct n as [|[|[|n]]]; simpl in H; try discriminate;
        injection H as <- <-.
      + exists (mkCtx [ITy sElem] []). split.
        * eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
        * apply WfNeed. reflexivity.
      + exists empty_ctx. split; constructor.
    - intros v D tauv H.
      destruct v as [|[|v]]; simpl in H; try discriminate; injection H as <- <-.
      exists empty_ctx. split; [constructor | apply wf_O].
    - intros f D S b H.
      destruct f as [|[|[|[|[|[|[|f]]]]]]]; simpl in H; try discriminate;
        injection H as <- <- <-.
      + exists (mkCtx [ITy sBoxed; ITy sHandle] []). split; [exact tele_alloc |].
        split; [constructor; [apply WfNeed; reflexivity | constructor] |].
        split; [apply WfNeed; reflexivity | intros body Hb; discriminate Hb].
      + exists PhiA. split; [exact tele_boxed |].
        split; [constructor; [apply WfNeed; reflexivity | constructor] |].
        split; [apply wf_box; reflexivity |].
        intros body Hb. injection Hb as <-. exact boxit_typed.
      + exists PhiA. split; [exact tele_boxed |].
        split; [constructor; [apply wf_box; reflexivity | constructor] |].
        split; [apply WfNeed; reflexivity |].
        intros body Hb. injection Hb as <-. exact unbox_typed.
      + exists PhiA. split; [exact tele_boxed |].
        split; [constructor; [apply WfNeed; reflexivity | constructor] |].
        split; [apply WfNeed; reflexivity |].
        intros body Hb. injection Hb as <-. exact id_typed.
      + exists PhiR. split; [exact tele_round |].
        split; [constructor; [apply WfNeed; reflexivity | constructor] |].
        split; [apply WfNeed; reflexivity |].
        intros body Hb. injection Hb as <-. exact round_typed.
      + exists empty_ctx. split; [constructor |].
        split; [constructor |]. split; [constructor |].
        intros body Hb. injection Hb as <-. exact main_typed.
    - intros m D S H.
      destruct m as [|[|m]]; simpl in H; try discriminate; injection H as <- <-.
      exists PhiA. split; [exact tele_boxed |].
      split; [constructor | apply WfNeed; reflexivity].
    - intros u Du Dr ob H.
      destruct u as [|[|[|[|[|u]]]]]; simpl in H; try discriminate;
        injection H as <- <- <-; exists PhiA;
        (split; [exact tele_boxed |]).
      + split; [| intros bs Hbs; discriminate Hbs].
        intros _. split; [reflexivity |]. split; [intros x [] |].
        split; [intros x [] | exact tele_res].
      + split; [| intros bs Hbs; discriminate Hbs].
        intros _. split; [reflexivity |]. split; [intros x [] |].
        split; [intros x [] | exact tele_res].
      + split; [intros Hn; discriminate Hn |].
        intros bs Hbs. injection Hbs as <-.
        exists empty_ctx. split; [constructor |].
        split; [split; [intros x [] | constructor] |].
        split; [exact tele_res | exact wrap_ok].
      + split; [intros Hn; discriminate Hn |].
        intros bs Hbs. injection Hbs as <-.
        exists empty_ctx. split; [constructor |].
        split; [split; [intros x [] | constructor] |].
        split; [exact tele_res | exact bare_ok].
  Qed.

End FunctorExample.

Theorem functor_example_ok :
  wf_prog FunctorExample.Sg FunctorExample.fMain.
Proof.
  split; [exact FunctorExample.wf_sig | eexists; reflexivity].
Qed.

(* So no run of it goes wrong, by Theorem 4.2. *)
Corollary functor_example_safe : forall fuel,
  interp fuel FunctorExample.Sg [] [] FunctorExample.main_unit <> Err.
Proof.
  intros fuel.
  exact (proj1 (soundness_main fuel functor_example_ok eq_refl)).
Qed.


(* ===================== 12d. Rule A: a pinned provider, executed ========== *)

(* The draft's array example, distilled.  `Arrays[Item]` is polymorphic in
   Item and fixed in Int; `Gc` is bound to it once, where Int = Num, and
   pins Int = Num (a manifest argument) and captures the platform fn `idx`
   (Wasm's index conversion), which its `get` calls on the index:

       fn idx(n: Num): Num;                       # abstract, platform
       functor Arrays[Item]: Int, Item -> Item.get[I = Int];
       functor Gc[Item]: Item -> Item.get[I = Int] {   # captures Int, idx
         bind Int = Num;                          # the pin
         bind Item.get[I = Int] = gc_get;         # gc_get(x: Item, i: Num): Item
       }
       assume Int, Item, Arrays {
         fn use(x: Item, i: Int): Item { bind Arrays; x.get(i) }
       }
       fn main() {
         bind Int = Num;  bind idx = idx_num;     # idx_num matches on Num
         bind Arrays = Gc;
         use[Item = A](A, Num);  use[Item = B](B, Num)
       }

   `use` is checked once, generically; it is called at two Items; the pin
   is checked once, at the bind.  The stale twin binds, then rebinds Int to
   Long (a fresh generation, D72) and applies `Arrays` there: Rule A's
   fixed-input premise refuses it, and without it the run goes wrong. *)

Module ArraysExample.

  Definition sInt : tsym := 1.
  Definition sItem : tsym := 2.
  Definition sI : tsym := 3.
  Definition sInt2 : tsym := 4.
  Definition nNum : nsym := 1.
  Definition nLong : nsym := 2.
  Definition nA : nsym := 3.
  Definition nB : nsym := 4.
  Definition fIdx : fsym := 1.
  Definition fIdxNum : fsym := 2.
  Definition fGcGet : fsym := 3.
  Definition fUse : fsym := 4.
  Definition fMain : fsym := 5.
  Definition mGet : msym := 1.
  Definition uArrays : usym := 1.
  Definition uGc : usym := 2.

  Definition num : ty := TNom nNum [].
  Definition get_item : item := IMth (TAbs sItem) mGet [(sI, TAbs sInt)].
  Definition arr_res : tele := [get_item].
  Definition th_id : subst := [(sInt, TAbs sInt); (sItem, TAbs sItem)].
  Definition cap_items : tele := [ITy sInt; IFn fIdx].
  Definition smc : smap := [(IFn fIdx, IFn fIdx)].

  Definition gc_body : list bnd :=
    [ BMth (TAbs sItem) mGet [(sI, TAbs sInt)] fGcGet [(IFn fIdx, IFn fIdx)] ].

  Definition use_body : expr :=
    EApply uArrays th_id [] (EMeth (EVar 0) mGet get_item [EVar 1]).

  Definition use_call (n : nsym) : expr :=
    ECallD fUse [(sInt, TAbs sInt); (sItem, TNom n [])] [(IFun uArrays, IFun uArrays)]
           [ETag n [] EUnit; ETag nNum [] EUnit].

  Definition prelude (rest : expr) : expr :=
    EBindTy sInt num (EBindFn fIdx fIdxNum [] (EBindFun uArrays uGc [] smc rest)).

  Definition main_body : expr :=
    prelude (ELet 10 (use_call nA) (use_call nB)).

  (* The twin: Int rebound (a fresh symbol, as D72 elaborates it), and
     Arrays applied at the new generation. *)
  Definition th_stale : subst := [(sInt, TAbs sInt2); (sItem, TNom nA [])].
  Definition stale_body : expr :=
    prelude (EBindTy sInt2 (TNom nLong [])
      (EApply uArrays th_stale []
         (EMeth (ETag nA [] EUnit) mGet (app_subst_item th_stale get_item)
                [ETag nLong [] EUnit]))).

  Definition SgOf (mainb : expr) : gsig := mkGsig
    (fun t => Nat.eqb t sInt || Nat.eqb t sItem || Nat.eqb t sI || Nat.eqb t sInt2
              || Nat.eqb t this_sym)
    (fun n => if Nat.leb 1 n && Nat.leb n 4 then Some ([], TUnit) else None)
    (fun _ => None)
    (fun f =>
       if Nat.eqb f fIdx then Some ([], mkSig [num] num, None)
       else if Nat.eqb f fIdxNum then
         Some ([], mkSig [num] num, Some (EMatch (EVar 0) [(nNum, 1, EVar 0)]))
       else if Nat.eqb f fGcGet then
         Some ([ITy sItem; IFn fIdx], mkSig [TAbs sItem; num] (TAbs sItem),
               Some (ELet 2 (ECallA fIdx [EVar 1]) (EVar 0)))
       else if Nat.eqb f fUse then
         Some ([ITy sInt; ITy sItem; IFun uArrays],
               mkSig [TAbs sItem; TAbs sInt] (TAbs sItem), Some use_body)
       else if Nat.eqb f fMain then Some ([], mkSig [] TUnit, Some (ELet 11 mainb EUnit))
       else None)
    (fun m => if Nat.eqb m mGet then Some ([ITy sI], mkSig [TAbs sI] (TAbs this_sym))
              else None)
    (fun u => if Nat.eqb u uArrays then Some ([ITy sInt; ITy sItem], arr_res, None)
              else if Nat.eqb u uGc then Some ([ITy sItem], arr_res, Some gc_body)
              else None)
    (fun u => if Nat.eqb u uArrays then [sInt] else [])
    (fun u => if Nat.eqb u uArrays then [sItem] else [])
    (fun u => if Nat.eqb u uGc then (cap_items, [(sInt, num)]) else ([], [])).

  Definition Sg := SgOf main_body.
  Definition SgStale := SgOf stale_body.

  (* Both runs give their element back: once at Item = A, once at B. *)
  Example runs : interp 1000 Sg [] [] main_body = Ok (VTag nB VUnit).
  Proof. vm_compute. reflexivity. Qed.

  Example erased_runs : interp 1000 Sg [] [] (erase main_body) = Ok (VTag nB VUnit).
  Proof. vm_compute. reflexivity. Qed.

  (* The binding's entry holds what it captured: the platform `idx`. *)
  Example captured :
    interp 1000 Sg [] [] (prelude (EBindTy sInt2 (TNom nLong []) EUnit)) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  (* The twin goes wrong: Gc's get, checked at Int = Num, is handed a Long. *)
  Example stale_goes_wrong : interp 1000 SgStale [] [] stale_body = Err.
  Proof. vm_compute. reflexivity. Qed.

  (* ---- Well-formedness of the positive program. ------------------------- *)

  Lemma conv : forall S0 Phi G e t t', has_ty S0 Phi G e t -> t = t' -> has_ty S0 Phi G e t'.
  Proof. intros; subst; assumption. Qed.

  Section Wf.
    Variable mainb : expr.
    Let S0 := SgOf mainb.

    Lemma wf_tag0 : forall Phi n, 1 <= n <= 4 -> wf_ty S0 Phi (TNom n []).
    Proof.
      intros Phi n Hn. eapply WfTag; [| | constructor].
      - destruct n as [|[|[|[|[|n]]]]]; try lia; reflexivity.
      - exists empty_ctx. split; [reflexivity |]. split; [reflexivity | split; constructor].
    Qed.

    Lemma tag0 : forall Phi G n, 1 <= n <= 4 -> has_ty S0 Phi G (ETag n [] EUnit) (TNom n []).
    Proof.
      intros Phi G n Hn. eapply conv.
      - eapply TTagR with (D := []) (tau0 := TUnit); [| | constructor |].
        + destruct n as [|[|[|[|[|n]]]]]; try lia; reflexivity.
        + exists empty_ctx. split; [reflexivity |]. split; [reflexivity | split; constructor].
        + apply conv with (t := TUnit); [constructor | reflexivity].
      - unfold app_subst. simpl. destruct Phi. reflexivity.
    Qed.

    Lemma sat0_nil : forall Phi, sat0 S0 Phi [].
    Proof.
      intros Phi. exists empty_ctx. split; [reflexivity |].
      split; [split; [reflexivity | split; constructor] | constructor].
    Qed.

    Lemma idxnum_typed : has_ty S0 empty_ctx (body_env (mkSig [num] num))
                           (EMatch (EVar 0) [(nNum, 1, EVar 0)]) num.
    Proof.
      eapply TMatchR with (tau0 := num) (ms := [num]).
      - constructor. reflexivity.
      - apply wf_tag0. unfold nNum. lia.
      - reflexivity.
      - constructor; [intros [] | constructor].
      - constructor; [| constructor]. split; [reflexivity |]. eexists; eexists; reflexivity.
      - constructor; [| constructor]. constructor. reflexivity.
    Qed.

    Definition PhiG : ctx := mkCtx [ITy sItem; IFn fIdx] [].

    Lemma gcget_typed : has_ty S0 PhiG (body_env (mkSig [TAbs sItem; num] (TAbs sItem)))
                          (ELet 2 (ECallA fIdx [EVar 1]) (EVar 0)) (TAbs sItem).
    Proof.
      eapply TLetR with (tau1 := num).
      - eapply conv.
        + eapply TNeedR with (Df := []) (S := mkSig [num] num); [reflexivity | reflexivity |].
          constructor; [| constructor]. constructor. reflexivity.
        + reflexivity.
      - constructor. reflexivity.
    Qed.

    Definition PhiU : ctx := mkCtx [ITy sInt; ITy sItem; IFun uArrays] [].

    Lemma use_typed : has_ty S0 PhiU (body_env (mkSig [TAbs sItem; TAbs sInt] (TAbs sItem)))
                        use_body (TAbs sItem).
    Proof.
      eapply TApplyR with (Du := [ITy sInt; ITy sItem]) (Dr := arr_res).
      - reflexivity.
      - reflexivity.
      - exists (mkCtx [ITy sInt; ITy sItem] []). split; [reflexivity |].
        split; [reflexivity | split; [repeat constructor; left; reflexivity | constructor]].
      - constructor; [apply WfNeed; reflexivity | constructor; [apply WfNeed; reflexivity | constructor]].
      - exists (mkCtx [ITy sInt; ITy sItem] []). split; [reflexivity | split; [reflexivity | constructor]].
      - constructor; [reflexivity | constructor].
      - unfold res_coherent. repeat constructor.
      - constructor.
      - eapply conv.
        + eapply TMethR with (Dm := [ITy sI]) (S := mkSig [TAbs sI] (TAbs this_sym))
                             (tau0 := TAbs sItem).
          * reflexivity.
          * constructor. reflexivity.
          * simpl. left. reflexivity.
          * reflexivity.
          * intros i1 Hin [th' Hp]. simpl in Hin.
            destruct Hin as [<- | [<- | [<- | [<- | []]]]]; try discriminate. reflexivity.
          * constructor; [| constructor]. constructor. reflexivity.
        + reflexivity.
      - constructor.
    Qed.

    Definition PhiArr : ctx := mkCtx [ITy sInt; ITy sItem] [].

    Lemma tele_arr : wf_tele S0 empty_ctx [ITy sInt; ITy sItem] PhiArr.
    Proof.
      eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
      eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
    Qed.

    Lemma get_ok : forall Phi, avail Phi (ITy sItem) -> avail Phi (ITy sInt) ->
      item_ok S0 Phi get_item.
    Proof.
      intros Phi H1 H2. eapply OkMth; [reflexivity | apply WfNeed; exact H1 | |].
      - exists (mkCtx [ITy sI] []). split; [reflexivity |]. split; [reflexivity |].
        split; [repeat constructor; left; reflexivity | constructor].
      - constructor; [apply WfNeed; exact H2 | constructor].
    Qed.

    Definition PhiC : ctx := mkCtx cap_items [].
    Definition PhiF : ctx := fun_ctx (mkCtx [ITy sItem] []) PhiC [(sInt, num)].

    Lemma gc_ok : exists PhiB, binds_ty S0 PhiF gc_body PhiB
                  /\ Forall (fun i => In i (cA PhiB)) arr_res.
    Proof.
      eexists. split.
      - eapply BtMth with (Dm := [ITy sI]) (S := mkSig [TAbs sI] (TAbs this_sym))
                          (D' := [ITy sItem; IFn fIdx])
                          (S' := mkSig [TAbs sItem; num] (TAbs sItem))
                          (tau0' := TAbs sItem) (rest := [num]);
          [reflexivity | apply WfNeed; reflexivity | | | reflexivity | | | reflexivity
          | reflexivity | reflexivity | reflexivity | apply BtNil].
        + exists (mkCtx [ITy sI] []). split; [reflexivity |]. split; [reflexivity |].
          split; [repeat constructor; left; reflexivity | constructor].
        + constructor; [apply WfNeed; reflexivity | constructor].
        + exists PhiG. split; [reflexivity |]. split.
          * split; [reflexivity |]. split; [| constructor].
            constructor; [left; reflexivity |].
            constructor; [| constructor]. right. split; [reflexivity | constructor].
          * constructor; [reflexivity | constructor].
        + exists PhiG. split; [reflexivity |]. split; [reflexivity |].
          constructor; [| constructor]. split; [simpl; tauto | reflexivity].
      - simpl. repeat constructor; simpl; tauto.
    Qed.

    Lemma tele_use : wf_tele S0 empty_ctx [ITy sInt; ITy sItem; IFun uArrays] PhiU.
    Proof.
      eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
      eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
      eapply WCons; [| reflexivity | constructor].
      eapply OkFun; [reflexivity | constructor; [reflexivity | constructor]].
    Qed.

    Lemma wf_sig_of : has_ty S0 empty_ctx (body_env (mkSig [] TUnit)) (ELet 11 mainb EUnit) TUnit ->
      wf_gsig S0.
    Proof.
      intros Hmain. constructor.
      - intros n D taup H.
        destruct n as [|[|[|[|[|n]]]]]; simpl in H; try discriminate;
          injection H as <- <-; exists empty_ctx; split; constructor.
      - intros v D tauv H. discriminate H.
      - intros f D S b H.
        destruct f as [|[|[|[|[|[|f]]]]]]; simpl in H; try discriminate;
          injection H as <- <- <-.
        + exists empty_ctx. split; [constructor |].
          split; [constructor; [apply wf_tag0; unfold nNum; lia | constructor] |].
          split; [apply wf_tag0; unfold nNum; lia | intros body Hb; discriminate Hb].
        + exists empty_ctx. split; [constructor |].
          split; [constructor; [apply wf_tag0; unfold nNum; lia | constructor] |].
          split; [apply wf_tag0; unfold nNum; lia |].
          intros body Hb. injection Hb as <-. exact idxnum_typed.
        + exists PhiG. split.
          { eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
            eapply WCons; [| reflexivity | constructor].
            eapply OkFn; [reflexivity | apply sat0_nil]. }
          split; [constructor; [apply WfNeed; reflexivity | constructor;
                   [apply wf_tag0; unfold nNum; lia | constructor]] |].
          split; [apply WfNeed; reflexivity |].
          intros body Hb. injection Hb as <-. exact gcget_typed.
        + exists PhiU. split; [exact tele_use |].
          split; [constructor; [apply WfNeed; reflexivity | constructor;
                   [apply WfNeed; reflexivity | constructor]] |].
          split; [apply WfNeed; reflexivity |].
          intros body Hb. injection Hb as <-. exact use_typed.
        + exists empty_ctx. split; [constructor |].
          split; [constructor |]. split; [constructor |].
          intros body Hb. injection Hb as <-. exact Hmain.
      - intros m D S H.
        destruct m as [|[|m]]; simpl in H; try discriminate; injection H as <- <-.
        exists (mkCtx [ITy sI] []). split.
        + eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
        + split; [constructor; [apply WfNeed; reflexivity | constructor] |].
          apply WfNeed. reflexivity.
      - intros u Du Dr ob H.
        destruct u as [|[|[|u]]]; simpl in H; try discriminate;
          injection H as <- <- <-.
        + exists PhiArr. split; [exact tele_arr |]. split; [| intros bs Hbs; discriminate Hbs].
          intros _. split; [reflexivity |].
          split; [intros x [<- | []]; simpl; tauto |].
          split; [intros x [<- | []]; simpl; tauto |].
          eapply WCons; [apply get_ok; reflexivity | reflexivity | constructor].
        + exists (mkCtx [ITy sItem] []). split.
          { eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor]. }
          split; [intros Hn; discriminate Hn |].
          intros bs Hbs. injection Hbs as <-.
          exists PhiC. split.
          { eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
            eapply WCons; [| reflexivity | constructor].
            eapply OkFn; [reflexivity | apply sat0_nil]. }
          split; [split; [intros x [<- | []]; simpl; tauto | constructor; [intros x [] | constructor]] |].
          split; [eapply WCons; [apply get_ok; reflexivity | reflexivity | constructor] |].
          exact gc_ok.
    Qed.

    (* The bind: Gc's captured inputs are here, its pin holds here, and the
       one captured input that is also an input of Arrays is fixed. *)
    Definition PhiBind : ctx := add_item (IFn fIdx) (bind_ty sInt num empty_ctx).

    Lemma cap_here : cap_ok S0 PhiBind [ITy sInt; ITy sItem] arr_res uArrays uGc smc.
    Proof.
      unfold cap_ok. simpl. split; [| split; [| split]].
      - exists PhiC. split; [reflexivity |]. split.
        + split; [reflexivity |]. split; [| constructor].
          constructor; [left; reflexivity |].
          constructor; [| constructor]. right. split; [reflexivity | constructor].
        + constructor; [reflexivity | constructor].
      - exists PhiC. split; [reflexivity |]. split; [reflexivity |].
        constructor; [| constructor]. split; [simpl; tauto | reflexivity].
      - constructor; [reflexivity | constructor].
      - intros x [<- | []] _. simpl. tauto.
    Qed.

    Lemma link_gc : fun_link S0 [ITy sItem] [] [ITy sInt; ITy sItem].
    Proof.
      exists PhiArr. split; [reflexivity |]. split.
      - exists (mkCtx [ITy sItem] []). split; [reflexivity |].
        split; [split; [reflexivity | split; [repeat constructor; left; reflexivity | constructor]]
               | repeat constructor].
      - exists (mkCtx [ITy sItem] []). split; [reflexivity | split; [reflexivity | constructor]].
    Qed.

    Lemma prelude_typed : forall rest tau,
      has_ty S0 (add_item (IFun uArrays) PhiBind) (body_env (mkSig [] TUnit)) rest tau ->
      wf_ty S0 empty_ctx tau ->
      has_ty S0 empty_ctx (body_env (mkSig [] TUnit)) (prelude rest) tau.
    Proof.
      intros rest tau Hr Hw.
      eapply TBindTyR; [reflexivity | discriminate | simpl; tauto
                        | apply wf_tag0; unfold nNum; lia | | exact Hw].
      eapply TBindFnR with (Df := []) (D' := []) (S := mkSig [num] num) (S' := mkSig [num] num);
        [reflexivity | reflexivity | apply sat0_nil | apply sat0_nil | | split; reflexivity |].
      { exists empty_ctx. split; [reflexivity | split; [reflexivity | constructor]]. }
      eapply TBindFunR with (Du := [ITy sInt; ITy sItem]) (Dr := arr_res) (Dh := [ITy sItem]);
        [reflexivity | reflexivity | exact link_gc | exact cap_here
        | constructor; [reflexivity | constructor] | exact Hr].
    Qed.
  End Wf.

  Lemma call_use : forall G n, 3 <= n <= 4 ->
    has_ty Sg (add_item (IFun uArrays) PhiBind) G (use_call n) (TNom n []).
  Proof.
    intros G n Hn. unfold use_call. eapply conv.
    - eapply TCallR with (Df := [ITy sInt; ITy sItem; IFun uArrays])
                         (S := mkSig [TAbs sItem; TAbs sInt] (TAbs sItem)) (body := use_body).
      + reflexivity.
      + exists PhiU. split; [reflexivity |]. split; [reflexivity |].
        split; [| constructor].
        constructor; [left; reflexivity |]. constructor; [left; reflexivity |].
        constructor; [| constructor]. right. split; [reflexivity |].
        constructor; [reflexivity | constructor].
      + constructor; [apply WfNeed; reflexivity |].
        constructor; [apply wf_tag0; lia | constructor].
      + exists PhiU. split; [reflexivity |]. split; [reflexivity |].
        constructor; [| constructor]. split; [simpl; tauto | reflexivity].
      + constructor; [| constructor; [| constructor]].
        * eapply conv; [apply tag0; lia | reflexivity].
        * eapply conv; [apply tag0; unfold nNum; lia | reflexivity].
    - reflexivity.
  Qed.

  Lemma main_typed : has_ty Sg empty_ctx (body_env (mkSig [] TUnit))
                       (ELet 11 main_body EUnit) TUnit.
  Proof.
    eapply TLetR with (tau1 := TNom nB []); [| constructor].
    apply prelude_typed; [| apply wf_tag0; unfold nB; lia].
    eapply TLetR; [apply call_use; unfold nA; lia |].
    apply call_use; unfold nB; lia.
  Qed.

  (* At the twin's application: everything Rule A asks holds except the
     fixed-input premise — Int is applied at Long's generation, the binding
     was made at Num's. *)
  Definition PhiStale : ctx :=
    bind_ty sInt2 (TNom nLong []) (add_item (IFun uArrays) PhiBind).

  Lemma stale_only_fixed_fails :
    sat SgStale PhiStale th_stale [ITy sInt; ITy sItem]
    /\ Forall (fun p => wf_ty SgStale PhiStale (snd p)) th_stale
    /\ smap_ok PhiStale th_stale [] [ITy sInt; ITy sItem]
    /\ res_coherent SgStale PhiStale th_stale arr_res
    /\ ~ fixed_ok SgStale PhiStale uArrays th_stale.
  Proof.
    split; [| split; [| split; [| split]]].
    - exists PhiArr. split; [reflexivity |]. split; [reflexivity |].
      split; [repeat constructor; left; reflexivity | constructor].
    - constructor; [apply WfNeed; reflexivity |].
      constructor; [apply wf_tag0; unfold nA; lia | constructor].
    - exists PhiArr. split; [reflexivity | split; [reflexivity | constructor]].
    - unfold res_coherent. repeat constructor.
    - unfold fixed_ok. intros H. inversion H as [| x l Hx _]. vm_compute in Hx. discriminate Hx.
  Qed.

End ArraysExample.

Theorem arrays_example_ok :
  wf_prog ArraysExample.Sg ArraysExample.fMain.
Proof.
  split; [exact (ArraysExample.wf_sig_of ArraysExample.main_typed) | eexists; reflexivity].
Qed.

Corollary arrays_example_safe : forall fuel,
  interp fuel ArraysExample.Sg [] [] (ELet 11 ArraysExample.main_body EUnit) <> Err.
Proof.
  intros fuel. exact (proj1 (soundness_main fuel arrays_example_ok eq_refl)).
Qed.

(* The stale twin is not a well-formed program — by soundness, since it goes
   wrong — and the premise it fails is exactly the fixed-input one. *)
Theorem stale_twin_rejected :
  ~ wf_prog ArraysExample.SgStale ArraysExample.fMain.
Proof.
  intros Hwf.
  pose proof (proj1 (soundness_main 1001 Hwf eq_refl)) as H.
  apply H. vm_compute. reflexivity.
Qed.


(* ---- Why TApplyR needs result coherence (machine-found). -----------------
   The draft's TApplyR at θ (sat, smap_ok, the fixed-input premise) lets a
   val/fn/functor item of the result through unchanged — such an item names
   no instantiation, so the scope reads it at its own symbols, while the
   body made it at θ.  Here φ[Item] takes K.mk[Q = Item] and returns
   v : Item; it is applied at Item = A where Item is bound to B.  Every
   premise of the draft's rule holds, the scope matches v as a B, and the
   run goes wrong.  `res_coherent` (deviation 8 applied to results) is the
   premise that refuses it. *)
Module ResCoherence.
  Definition sItem : tsym := 1.
  Definition sQ : tsym := 2.
  Definition nK : nsym := 1.
  Definition nA : nsym := 2.
  Definition nB : nsym := 3.
  Definition vV : vsym := 1.
  Definition fMkA : fsym := 1.
  Definition fMain : fsym := 2.
  Definition mMk : msym := 1.
  Definition uF : usym := 1.
  Definition uH : usym := 2.

  Definition kty : ty := TNom nK [].
  Definition mk_item : item := IMth kty mMk [(sQ, TAbs sItem)].
  Definition th : subst := [(sItem, TNom nA [])].
  Definition Du : tele := [ITy sItem; mk_item].

  Definition main_body : expr :=
    EBindTy sItem (TNom nB [])
    (EBindMth kty mMk [(sQ, TNom nA [])] fMkA []
    (EBindFun uF uH [(mk_item, mk_item)] []
    (EApply uF th [(mk_item, app_subst_item th mk_item)]
       (EMatch (EVl vV) [(nB, 1, EUnit)])))).


  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sItem || Nat.eqb t sQ || Nat.eqb t this_sym)
    (fun n => if Nat.leb 1 n && Nat.leb n 3 then Some ([], TUnit) else None)
    (fun v => if Nat.eqb v vV then Some ([ITy sItem], TAbs sItem) else None)
    (fun f => if Nat.eqb f fMkA then Some ([], mkSig [kty] (TNom nA []), Some (ETag nA [] EUnit))
              else if Nat.eqb f fMain then Some ([], mkSig [] TUnit, Some (ELet 9 main_body EUnit))
              else None)
    (fun m => if Nat.eqb m mMk then Some ([ITy sQ], mkSig [] (TAbs sQ)) else None)
    (fun u => if Nat.eqb u uF then Some (Du, [IVal vV], None)
              else if Nat.eqb u uH then
                Some (Du, [IVal vV], Some [BVal vV (EMeth (ETag nK [] EUnit) mMk mk_item [])])
              else None)
    (fun _ => []) (fun u => if Nat.eqb u uF then [sItem] else []) (fun _ => ([], [])).

  Example goes_wrong : interp 100 Sg [] [] main_body = Err.
  Proof. vm_compute. reflexivity. Qed.

  (* The control: read as the A it is, the same run succeeds — so the run
     above goes wrong exactly at the incoherent reading, not at a missing
     entry.  (An earlier version bound φ with an empty link ς, so its run
     went wrong at the body's lookup of K.mk whatever the scope did.) *)
  Example control :
    interp 100 Sg [] []
      (EBindTy sItem (TNom nB [])
      (EBindMth kty mMk [(sQ, TNom nA [])] fMkA []
      (EBindFun uF uH [(mk_item, mk_item)] []
      (EApply uF th [(mk_item, app_subst_item th mk_item)]
         (EMatch (EVl vV) [(nA, 1, EUnit)]))))) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  Definition PhiAt : ctx :=
    add_item (IFun uF) (add_item (IMth kty mMk [(sQ, TNom nA [])])
                                 (bind_ty sItem (TNom nB []) empty_ctx)).

  Lemma premises : sat Sg PhiAt th Du
    /\ smap_ok PhiAt th [(mk_item, app_subst_item th mk_item)] Du
    /\ fixed_ok Sg PhiAt uF th
    /\ ~ res_coherent Sg PhiAt th [IVal vV].
  Proof.
    split; [| split; [| split]].
    - exists (mkCtx Du []). split; [reflexivity |]. split; [reflexivity |].
      split; [| constructor].
      constructor; [left; reflexivity |]. constructor; [| constructor].
      right. split; [reflexivity | constructor].
    - exists (mkCtx Du []). split; [reflexivity | split; [reflexivity |]].
      constructor; [| constructor]. split; [simpl; tauto | reflexivity].
    - constructor.
    - unfold res_coherent. intros H. inversion H as [| i l Hi _]. inversion Hi as [| x l' Hx _].
      vm_compute in Hx. discriminate Hx.
  Qed.
End ResCoherence.

(* So the draft's TApplyR (without result coherence) admits a program that
   goes wrong; with it, the program is refused (by soundness). *)
Theorem res_coherence_needed :
  interp 100 ResCoherence.Sg [] [] ResCoherence.main_body = Err
  /\ sat ResCoherence.Sg ResCoherence.PhiAt ResCoherence.th ResCoherence.Du
  /\ fixed_ok ResCoherence.Sg ResCoherence.PhiAt ResCoherence.uF ResCoherence.th
  /\ ~ res_coherent ResCoherence.Sg ResCoherence.PhiAt ResCoherence.th [IVal ResCoherence.vV]
  /\ ~ wf_prog ResCoherence.Sg ResCoherence.fMain.
Proof.
  destruct ResCoherence.premises as [H1 [_ [H3 H4]]].
  split; [exact ResCoherence.goes_wrong |]. split; [exact H1 |].
  split; [exact H3 |]. split; [exact H4 |].
  intros Hwf. apply (proj1 (soundness_main 101 Hwf eq_refl)). vm_compute. reflexivity.
Qed.

(* ===================== 13. Assumptions audit ============================= *)

(* The gate that `coqc` alone does not provide: coqc accepts `Admitted`, so a
   clean compile is not evidence that the metatheory is unconditional.  Each
   directive below must print "Closed under the global context".  The flake
   check enforces this mechanically, and additionally that the set of names
   audited here is exactly the set of top-level Theorems in this file, so a
   theorem cannot be added without being audited. *)

Print Assumptions availability_decidable.
Print Assumptions method_resolution_functional.
Print Assumptions eval_deterministic.
Print Assumptions interp_sound.
Print Assumptions interp_complete.
Print Assumptions safety.
Print Assumptions soundness_main.
Print Assumptions phase_separation.
Print Assumptions functor_example_ok.
Print Assumptions functor_example_safe.
Print Assumptions polymorphic_provider_fixed.
Print Assumptions capture_at_bind.
Print Assumptions capture_through_copy.
Print Assumptions fixed_inputs_never_stale.
Print Assumptions fixed_ok_decidable.
Print Assumptions res_coherent_decidable.
Print Assumptions pins_hold_decidable.
Print Assumptions capture_fixed_decidable.
Print Assumptions identity_application_conservative.
Print Assumptions empty_capture_conservative.
Print Assumptions identity_keys_conservative.
Print Assumptions arrays_example_ok.
Print Assumptions arrays_example_safe.
Print Assumptions stale_twin_rejected.
Print Assumptions res_coherence_needed.
