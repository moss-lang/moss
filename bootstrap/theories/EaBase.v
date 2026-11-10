(* ------------------------------------------------------------------------ *)
(* EaBase.v — the executable Audit elaborator ("Ea"): monad, state,         *)
(* strategy, read-only environment, and the open-recursion record.          *)
(*                                                                           *)
(* The elaborator is written directly against Audit.v's elaboration         *)
(* relations (scratch/ELAB-DESIGN.md, architecture (a)): one function per   *)
(* Audit judgment, each premise checked in the rule's order using Audit's   *)
(* own helper definitions.  Every choice Audit leaves open is made at an    *)
(* explicit choice point (the `strategy`, the fresh counter) and validated  *)
(* locally, so soundness is meant to hold for every strategy.               *)
(*                                                                           *)
(* Files, one judgment group each (so proofs can proceed in parallel):      *)
(*   EaBase      this file                                                  *)
(*   EaTypes     TyOf/Brackets, SigOf, AppOf, RecvTy, MethodNamed, PayloadOf,*)
(*               FreeIn/SigMentions, SpecTypes, FunOutputs, ResultMentions, *)
(*               Mentions/Poly/Fixed, FixedReadings                         *)
(*   EaCtx       ItemEntries, RootCtx, MostGeneralMerge, CtxOf, FunCtx,     *)
(*               the w_context table                                        *)
(*   EaCall      at_site/at_th, CallTheta, Satisfies                         *)
(*   EaFamily    OutputItemEntries, ProvideResult, Family, FamilyProduces,  *)
(*               OnlyFamily, FamilyReadOff, FamilyApplication, Demand, LitOp*)
(*   EaExpr      Ex, Args, Cond, Branch, Arms, MethCall, Provided, BlkE,    *)
(*               Blk, PatOf, Literal                                        *)
(*   EaBind      BindItems, BindItem (BTypeBind .. BApplyBody), FnProvider, *)
(*               MethProvider, LambdaProvider                               *)
(*   EaFunctor   BApplyAbstract, BFunCopy (+CopyInterpretation),            *)
(*               BFunClosure (+LinkBody): the rules lang-b/lang-q rewrite   *)
(*   EaDecide    deciders of negative premises (Breaks, Mentions_name, ...) *)
(*   EaPlatform  main's environment, PlatformEntry, platform family code,   *)
(*               Platform.Linked, casts, Bool                               *)
(*   EaMain      the knot (fuel), FnElab for every defined fn, assembly,    *)
(*               `elaborate`                                                *)
(*                                                                           *)
(* No proofs yet.  Soundness statements are written as comments          *)
(* "SOUND:" on each function, in the form "if it returns X, then J holds".  *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ======================================================================== *)
(* 1. Results and the state monad                                           *)
(* ======================================================================== *)

Inductive er (A : Type) : Type := Ok (a : A) | Err (m : string).
Arguments Ok {A} a.
Arguments Err {A} m.

(* The global state.  INVARIANTS (to be proved as `Inv`):
   - I1 (counter) every symbol in es_fns/es_funs keys, in any context,
     frame or generated body, and every source symbol, is < es_next;
     symbols >= es_next are Fresh and occur nowhere.
   - I2 (bodies) es_fns/es_funs only grow, and each entry satisfies its
     judgment (FPForward/MPWrapped/LambdaProvider/LinkBody/Platform.Family)
     against the final program (P-monotonicity).
   - I3 (caches) es_fwd g = w -> body of w is FPForward's for g;
     es_wrap (g,n) = w -> MPWrapped's body; es_link (u,gg) = h -> LinkBody u gg h;
     es_const s = (x,t) -> ExConst's premises for s at x, t. *)
Record est := Est {
  es_next : nat;
  es_fns : nmap expr;
  es_funs : nmap functor_code;
  es_fwd : nmap nat;
  es_wrap : list ((nat * nat) * nat);
  es_link : list ((nat * nat) * nat);
  es_const : nmap (expr * ty);
  es_unsupported : list string;
  es_lamcx : nmap cx;                   (* a lambda provider's context (NodeCx) *)
  es_pins : nmap subst;                  (* what a link body pins fixed inputs to (LinkBody's pins, Q6) *)
  es_names : nmap string                 (* a fresh generation's name, for messages only ("T@F") *)
}.

(* A symbol with no declaration (a fresh generation) prints as a marker,
   its number between two \001 characters; a message raised in the
   monad reads each marker through es_names (`resolve_names`), so a
   message names a generation `T@F` as the old oracle did. *)
Definition name_mark : Ascii.ascii := Ascii.ascii_of_nat 1.
Fixpoint take_marked (r : string) (acc : nat) : option (nat * string) :=
  match r with
  | EmptyString => None
  | String c r' => if Ascii.eqb c name_mark then Some (acc, r')
                   else take_marked r' (acc * 10 + (Ascii.nat_of_ascii c - 48))
  end.
Fixpoint resolve_go (n : nat) (names : nmap string) (m : string) : string :=
  match n with
  | 0 => m
  | S n' =>
      match m with
      | EmptyString => EmptyString
      | String c r =>
          if Ascii.eqb c name_mark then
            match take_marked r 0 with
            | Some (g, rest) => String.append (match nget names g with Some x => x | None => "?"%string end) (resolve_go n' names rest)
            | None => m
            end
          else String c (resolve_go n' names r)
      end
  end.
Definition resolve_names (names : nmap string) (m : string) : string := resolve_go (String.length m) names m.

Definition M (A : Type) : Type := est -> er (A * est).

Definition ret {A} (a : A) : M A := fun s => Ok (a, s).
Definition fail {A} (m : string) : M A := fun s => Err (resolve_names (es_names s) m).
Definition mbind {A B} (x : M A) (f : A -> M B) : M B :=
  fun s => match x s with Ok (a, s') => f a s' | Err m => Err m end.
Notation "'let*' p ':=' c 'in' b" := (mbind c (fun p => b))
  (at level 200, p pattern, c at level 100, b at level 200, right associativity).
Definition get : M est := fun s => Ok (s, s).
Definition put (s : est) : M unit := fun _ => Ok (tt, s).
(* lift, check, rcheck and ropt are notations, not functions, so that
   their message (which may print types) is computed only on failure:
   extracted OCaml is strict. *)
Notation lift o m := (match o with Some a => ret a | None => fail m end).
Notation check b m := (if b then ret tt else fail m).
(* try a computation; on failure, roll back the state and return None *)
Definition attempt {A} (x : M A) : M (option A) :=
  fun s => match x s with Ok (a, s') => Ok (Some a, s') | Err _ => Ok (None, s) end.
Definition orelse {A} (x : M A) (y : M A) : M A :=
  fun s => match x s with Ok r => Ok r | Err _ => y s end.
Definition map_err {A} (f : string -> string) (x : M A) : M A :=
  fun s => match x s with Ok r => Ok r | Err m => Err (f m) end.
Definition with_msg {A} (pre : string) (x : M A) : M A :=
  fun s => match x s with Ok r => Ok r | Err m => Err (pre ++ m) end.

Fixpoint mmap {A B} (f : A -> M B) (l : list A) : M (list B) :=
  match l with
  | [] => ret []
  | x :: r => let* y := f x in let* ys := mmap f r in ret (y :: ys)
  end.
Fixpoint mfold {A B} (f : B -> A -> M B) (acc : B) (l : list A) : M B :=
  match l with [] => ret acc | x :: r => let* acc' := f acc x in mfold f acc' r end.
Fixpoint miter {A} (f : A -> M unit) (l : list A) : M unit :=
  match l with [] => ret tt | x :: r => let* _ := f x in miter f r end.

(* Pure results (no state) used by the readers. *)
Definition rbind {A B} (x : er A) (f : A -> er B) : er B :=
  match x with Ok a => f a | Err m => Err m end.
Notation "'let!' p ':=' c 'in' b" := (rbind c (fun p => b))
  (at level 200, p pattern, c at level 100, b at level 200, right associativity).
Notation rcheck b m := (if b then Ok tt else Err m).
Notation ropt o m := (match o with Some a => Ok a | None => Err m end).
Fixpoint rmap {A B} (f : A -> er B) (l : list A) : er (list B) :=
  match l with [] => Ok [] | x :: r => let! y := f x in let! ys := rmap f r in Ok (y :: ys) end.
Definition pure {A} (x : er A) : M A := fun s => match x with Ok a => Ok (a, s) | Err m => Err (resolve_names (es_names s) m) end.

(* ======================================================================== *)
(* 2. The strategy: every free choice of the relation                      *)
(* ======================================================================== *)

(* SOUNDNESS is to be proved for EVERY strategy; completeness (later) for
   the default one.  Each choice is validated where it is used.
   - s_orient a b: merging two generations TAbs a and TAbs b (a <> b),
     true sends a to b (MostGeneralMerge leaves the representative free).
   - s_pick i n: among n candidates (family items that match a demanded
     key), the index chosen (FamilyReadOff checks they agree).
   Orders the relation leaves free (Needs, RootItems' xs, implicit inputs,
   outs, caps, keys) are fixed by convention in the code: first occurrence
   for Needs/caps/keys, ascending for xs/implicit/outs/marks. *)
Record strategy := Strategy {
  s_orient : nat -> nat -> bool;
  s_pick : nat -> nat -> nat
}.
Definition default_strategy : strategy := Strategy (fun _ _ => true) (fun _ _ => 0).

(* ======================================================================== *)
(* 3. The read-only environment                                             *)
(* ======================================================================== *)

(* e_W: the world (ElabWorld.bundle_world, with w_context the table EaCtx
   computes); e_mods: the loaded module paths; e_mains: the symbols some
   module's scope names `main` (IsMainFn); e_fuel: the depth bound for
   jumps (aliases, contexts, constants, functor bodies). *)
Record env := Env {
  e_W : world;
  e_strat : strategy;
  e_mods : list string;
  e_mains : list nat;
  e_fuel : nat
}.

(* ======================================================================== *)
(* 4. Small decidable helpers, mirroring Audit's definitions               *)
(* ======================================================================== *)

Fixpoint nmem (x : nat) (l : list nat) : bool :=
  match l with [] => false | y :: r => Nat.eqb x y || nmem x r end.
Fixpoint nodupb (l : list nat) : bool :=
  match l with [] => true | x :: r => negb (nmem x r) && nodupb r end.
(* first-occurrence dedup *)
Definition uniq (l : list nat) : list nat :=
  rev (fold_left (fun acc x => if nmem x acc then acc else x :: acc) l []).
Fixpoint insert_sorted (x : nat) (l : list nat) : list nat :=
  match l with [] => [x] | y :: r => if Nat.ltb x y then x :: l else if Nat.eqb x y then l else y :: insert_sorted x r end.
Definition sort_uniq (l : list nat) : list nat := fold_right insert_sorted [] l.
Fixpoint imem (x : item) (l : list item) : bool :=
  match l with [] => false | y :: r => item_eqb x y || imem x r end.
Definition iuniq (l : list item) : list item :=
  rev (fold_left (fun acc x => if imem x acc then acc else x :: acc) l []).
Fixpoint inodupb (l : list item) : bool :=
  match l with [] => true | x :: r => negb (imem x r) && inodupb r end.
Fixpoint smem (x : string) (l : list string) : bool :=
  match l with [] => false | y :: r => String.eqb x y || smem x r end.
Fixpoint snodupb (l : list string) : bool :=
  match l with [] => true | x :: r => negb (smem x r) && snodupb r end.

Fixpoint nlist_eqb (a b : list nat) : bool :=
  match a, b with [], [] => true | x :: r, y :: q => Nat.eqb x y && nlist_eqb r q | _, _ => false end.
Fixpoint slist_eqb (a b : list string) : bool :=
  match a, b with [], [] => true | x :: r, y :: q => String.eqb x y && slist_eqb r q | _, _ => false end.
Fixpoint ilist_eqb (a b : list item) : bool :=
  match a, b with [], [] => true | x :: r, y :: q => item_eqb x y && ilist_eqb r q | _, _ => false end.
Definition oty_eqb (a b : option ty) : bool :=
  match a, b with Some x, Some y => ty_eqb x y | None, None => true | _, _ => false end.
Fixpoint tys_eqb (a b : list ty) : bool :=
  match a, b with [], [] => true | x :: r, y :: q => ty_eqb x y && tys_eqb r q | _, _ => false end.
Definition fsig_eqb (a b : fsig) : bool := tys_eqb (sg_params a) (sg_params b) && ty_eqb (sg_ret a) (sg_ret b).
Definition ofsig_eqb (a b : option fsig) : bool :=
  match a, b with Some x, Some y => fsig_eqb x y | None, None => true | _, _ => false end.
Fixpoint inst_eqb (a b : inst) : bool :=
  match a, b with
  | [], [] => true
  | (x, o) :: r, (y, o') :: q => Nat.eqb x y && oty_eqb o o' && inst_eqb r q
  | _, _ => false
  end.
Definition provision_eqb (a b : provision) : bool :=
  match a, b with
  | ProvidedVal t x, ProvidedVal u y => ty_eqb t u && inst_eqb x y
  | ProvidedFn g x, ProvidedFn h y => fsig_eqb g h && inst_eqb x y
  | ProvidedMethod g x, ProvidedMethod h y => fsig_eqb g h && inst_eqb x y
  | ProvidedFunctor f, ProvidedFunctor g => subst_eqb f g
  | _, _ => false
  end.
Definition oprovision_eqb (a b : option provision) : bool :=
  match a, b with Some x, Some y => provision_eqb x y | None, None => true | _, _ => false end.
Definition item_kind_eqb (a b : item) : bool :=
  match a, b with
  | ITy _, ITy _ | IVal _, IVal _ | IFn _, IFn _ | IMth _ _ _, IMth _ _ _ | IFun _, IFun _ => true
  | _, _ => false end.

Definition is_some {A} (o : option A) : bool := match o with Some _ => true | None => false end.

(* `fits` ([D17]) decided *)
Definition fitsb (t expected : ty) : bool :=
  ty_eqb t expected || ty_eqb t never ||
  match expected with
  | TUnion ms => forallb (fun m => existsb (ty_eqb m) ms) (members t)
  | _ => false
  end.
Definition opt_fitsb (t : ty) (exp : option ty) : bool :=
  match exp with Some e => fitsb t e | None => true end.


(* Fast lookups, each equal to Audit's lookup through the derived tables
   (c_vals, c_fns, c_mths, c_funs, c_apps), which are recomputed by
   flat_map at every use.  SOUND: cval C v = nlook v (c_vals C), etc. *)
Fixpoint cval_go (v : nat) (l : list (item * provision)) : option ty :=
  match l with [] => None | (IVal v', ProvidedVal t _) :: r => if Nat.eqb v v' then Some t else cval_go v r | _ :: r => cval_go v r end.
Fixpoint cfn_go (f : nat) (l : list (item * provision)) : option fsig :=
  match l with [] => None | (IFn f', ProvidedFn g _) :: r => if Nat.eqb f f' then Some g else cfn_go f r | _ :: r => cfn_go f r end.
Fixpoint cmth_go (t : ty) (m : nat) (l : list (item * provision)) : option fsig :=
  match l with [] => None | (IMth t' m' _, ProvidedMethod g _) :: r => if Nat.eqb m m' && ty_eqb t t' then Some g else cmth_go t m r | _ :: r => cmth_go t m r end.
Fixpoint cfun_go (u : nat) (l : list (item * provision)) : option subst :=
  match l with [] => None | (IFun u', ProvidedFunctor fx) :: r => if Nat.eqb u u' then Some fx else cfun_go u r | _ :: r => cfun_go u r end.
Definition cval (C : cx) (v : nat) := cval_go v (c_provisions C).
Definition cfn (C : cx) (f : nat) := cfn_go f (c_provisions C).
Definition cmth (C : cx) (t : ty) (m : nat) := cmth_go t m (c_provisions C).
Definition cfun (C : cx) (u : nat) := cfun_go u (c_provisions C).
Definition cfun_keys (C : cx) : list nat :=
  flat_map (fun q => match q with (IFun u, ProvidedFunctor _) => [u] | _ => [] end) (c_provisions C).
Definition capp (C : cx) (k : item) : option inst := option_map provision_inst (ilook k (c_provisions C)).
(* rkey and rkey_of, through capp. SOUND: rk C r m = rkey C r m; rkof C k = rkey_of C k *)
Definition rk (C : cx) (r : ty) (m : nat) : item :=
  IMth r m (match capp C (mkey r m) with Some a => theta_of C a | None => [] end).
Definition rkof (C : cx) (k : item) : item := match k with IMth r m _ => rk C r m | _ => k end.

(* An index of a context's provisions by the item's symbol: bucket lists
   in the context's order, so the first match in a bucket is the first
   match overall.  SOUND: plook (pindex l) k = ilook k l. *)
Definition item_slot (k : item) : nat :=
  match k with
  | ITy t => 5 * t | IVal v => 5 * v + 1 | IFn f => 5 * f + 2 | IMth _ m _ => 5 * m + 3 | IFun u => 5 * u + 4
  end.
Definition pindex {A} (l : list (item * A)) : nmap (list (item * A)) :=
  fold_right (fun q m => let i := item_slot (fst q) in
                         nset m i (q :: match nget m i with Some b => b | None => [] end)) NE l.
Definition plook {A} (ix : nmap (list (item * A))) (k : item) : option A :=
  match nget ix (item_slot k) with Some b => ilook k b | None => None end.
(* iuniq, through a slot index of what was seen. SOUND: iuniq_fast l = iuniq l *)
Definition iuniq_fast (l : list item) : list item :=
  rev (fst (fold_left (fun acc k =>
    let '(out, seen) := acc in
    let i := item_slot k in
    let b := match nget seen i with Some b => b | None => [] end in
    if imem k b then acc else (k :: out, nset seen i (k :: b))) l ([], NE))).
(* A provision index: the list itself (small contexts or few lookups) or
   the slot index.  SOUND: pfind (pix_of b l) k = ilook k l. *)
Inductive pix := PList (l : list (item * provision)) | PIdx (m : nmap (list (item * provision))).
Definition pix_of (big : bool) (l : list (item * provision)) : pix := if big then PIdx (pindex l) else PList l.
Definition pfind (ix : pix) (k : item) : option provision :=
  match ix with PList l => ilook k l | PIdx m => plook m k end.
Definition pbucket (ix : pix) (slot : nat) : list (item * provision) :=
  match ix with PList l => l | PIdx m => match nget m slot with Some b => b | None => [] end end.
Definition capp_i (ix : pix) (k : item) : option inst := option_map provision_inst (pfind ix k).
Definition rk_i (C : cx) (ix : pix) (r : ty) (m : nat) : item :=
  IMth r m (match capp_i ix (mkey r m) with Some a => theta_of C a | None => [] end).
Definition rkof_i (C : cx) (ix : pix) (k : item) : item :=
  match k with IMth r m _ => rk_i C ix r m | _ => k end.
Definition providesb_i (C : cx) (ix : pix) (i : item) : bool :=
  match i with
  | IMth r m _ => is_some (cmth_go r m (pbucket ix (5 * m + 3)))
  | ITy t => is_some (nlook t (c_ty C))
  | IVal v => is_some (cval_go v (pbucket ix (5 * v + 1)))
  | IFn f => is_some (cfn_go f (pbucket ix (5 * f + 2)))
  | IFun u => is_some (cfun_go u (pbucket ix (5 * u + 4)))
  end.

(* `provides C i` decided *)
Definition providesb (C : cx) (i : item) : bool :=
  match i with
  | IVal v => is_some (cval C v)
  | IFn f => is_some (cfn C f)
  | IMth r m _ => is_some (cmth C r m)
  | IFun u => is_some (cfun C u)
  | ITy t => match nlook t (c_ty C) with Some _ => true | None => false end
  end.


(* ======================================================================== *)
(* 5. The open recursion                                                    *)
(* ======================================================================== *)

(* The mutual judgments (Ex .. LinkBody) are split across files.  Each file
   takes the others through this record of callbacks, one call level
   lower; EaMain ties the knot by fuel.  A `wrap` is the binding context
   a bind item puts around what follows it.

   SOUND (callback hypothesis, `RecSound E P R`, for the final program P):
   - r_ex F C e exp = Ok (x, t) -> Ex W P F C e exp x t
   - r_blke F C b exp = Ok (x, t) -> BlkE W P F C b exp x t
   - r_bind_items F C its = Ok (wrap, C', F', made) -> BindItems W P F C its wrap C' F' made *)
Definition wrapper := expr -> expr.
Record rec := Rec {
  r_ex : frame -> cx -> aexpr -> option ty -> M (expr * ty);
  r_blke : frame -> cx -> ablock -> option ty -> M (expr * ty);
  r_bind_items : frame -> cx -> list (spec * option aexpr) -> M (wrapper * cx * frame * list nat)
}.

(* ======================================================================== *)
(* 6. Fresh symbols and generated bodies                                    *)
(* ======================================================================== *)

(* SOUND: returns a symbol w with w_decl W w = None (I1), never returned
   before, and above every symbol in sight. *)
Definition fresh : M nat :=
  fun s => Ok (es_next s, Est (S (es_next s)) (es_fns s) (es_funs s) (es_fwd s) (es_wrap s)
                             (es_link s) (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
Fixpoint fresh_n (n : nat) : M (list nat) :=
  match n with 0 => ret [] | S k => let* g := fresh in let* gs := fresh_n k in ret (g :: gs) end.
Definition set_fn_body (f : nat) (x : expr) : M unit :=
  fun s => Ok (tt, Est (es_next s) (nset (es_fns s) f x) (es_funs s) (es_fwd s) (es_wrap s)
                      (es_link s) (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
Definition set_fun_body (h : nat) (c : functor_code) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (nset (es_funs s) h c) (es_fwd s) (es_wrap s)
                      (es_link s) (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
Definition get_fwd (g : nat) : M (option nat) := fun s => Ok (nget (es_fwd s) g, s).
Definition put_fwd (g w : nat) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (es_funs s) (nset (es_fwd s) g w) (es_wrap s)
                      (es_link s) (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
Fixpoint pair_look {A} (k : nat * nat) (l : list ((nat * nat) * A)) : option A :=
  match l with [] => None | (k', a) :: r => if Nat.eqb (fst k) (fst k') && Nat.eqb (snd k) (snd k') then Some a else pair_look k r end.
Definition get_wrap (k : nat * nat) : M (option nat) := fun s => Ok (pair_look k (es_wrap s), s).
Definition put_wrap (k : nat * nat) (w : nat) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (es_funs s) (es_fwd s) ((k, w) :: es_wrap s)
                      (es_link s) (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
Definition get_link (k : nat * nat) : M (option nat) := fun s => Ok (pair_look k (es_link s), s).
Definition put_link (k : nat * nat) (h : nat) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (es_funs s) (es_fwd s) (es_wrap s)
                      ((k, h) :: es_link s) (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
Definition get_const (c : nat) : M (option (expr * ty)) := fun s => Ok (nget (es_const s) c, s).
Definition put_const (c : nat) (v : expr * ty) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (es_funs s) (es_fwd s) (es_wrap s)
                      (es_link s) (nset (es_const s) c v) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
(* the index of an unsupported primitive's name, added on first use *)
Definition unsupported_index (n : string) : M nat :=
  fun s =>
    let fix find (l : list string) (i : nat) : option nat :=
      match l with [] => None | x :: r => if String.eqb x n then Some i else find r (S i) end in
    match find (es_unsupported s) 0 with
    | Some i => Ok (i, s)
    | None => Ok (List.length (es_unsupported s),
                  Est (es_next s) (es_fns s) (es_funs s) (es_fwd s) (es_wrap s) (es_link s)
                      (es_const s) (es_unsupported s ++ [n]) (es_lamcx s) (es_pins s) (es_names s))
    end.

Definition init_state (next : nat) : est := Est next NE NE NE [] [] NE [] NE NE NE.
Definition put_pins (h : nat) (p : subst) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (es_funs s) (es_fwd s) (es_wrap s) (es_link s)
                      (es_const s) (es_unsupported s) (es_lamcx s) (nset (es_pins s) h p) (es_names s)).
Definition put_name (g : nat) (n : string) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (es_funs s) (es_fwd s) (es_wrap s) (es_link s)
                      (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (nset (es_names s) g n)).
Definition get_pins (h : nat) : M subst := fun s => Ok (match nget (es_pins s) h with Some p => p | None => [] end, s).
Definition put_lamcx (pid : nat) (C : cx) : M unit :=
  fun s => Ok (tt, Est (es_next s) (es_fns s) (es_funs s) (es_fwd s) (es_wrap s) (es_link s)
                      (es_const s) (es_unsupported s) (nset (es_lamcx s) pid C) (es_pins s) (es_names s)).

(* ======================================================================== *)
(* 7. Frames                                                                *)
(* ======================================================================== *)

(* A frame's numbering: parameters first (after `this`, which is slot 0),
   then every other name in `names`, in first-occurrence order.
   SOUND: Numbering (mk_frame_vars this ps names) this ps when ps has no
   duplicates (the caller checks NoDup ps). *)
Fixpoint number_from (k : nat) (l : list string) : list (string * nat) :=
  match l with [] => [] | n :: r => (n, k) :: number_from (S k) r end.
Definition suniq (l : list string) : list string :=
  rev (fold_left (fun acc x => if smem x acc then acc else x :: acc) l []).
Definition mk_frame_vars (this : bool) (ps names : list string) : list (string * nat) :=
  number_from (if this then 1 else 0) (suniq (ps ++ names)).

(* ======================================================================== *)
(* 8. Printers for diagnostics                                              *)
(* ======================================================================== *)

(* Names, types, keys and signatures, read from Audit's representations
   and printed as the old oracle (Elab.v's `show`, `show_key`) printed
   them, so that messages name the culprit as they did: a type symbol or
   a nominal type without arguments by its name, `A[X=I32]`, `A | B`, `|`
   for never, `{a: I32}`; a detached method `.m`, an attached one `R.m`;
   a method key `T.m`.  Nothing here is read by a judgment. *)
Local Open Scope string_scope.
Section Print.
Variable W : world.

(* a symbol's declared name (a method's without its dot) *)
Definition sym_name (s : nat) : string :=
  match w_decl W s with
  | Some d => match filed_name (d_ast d) with Some (_, n) => n | None =>
                match d_ast d with ADFn _ n _ _ _ => n | _ => "?" end end
  | None => String name_mark (String.append (Syntax.show_nat s) (String name_mark EmptyString)) end.
Definition show_path (pa : path) : string := String.concat "::" pa.

(* a symbol as the old oracle named it: `.m` for a detached method,
   `R.m` for one attached to R *)
Definition dname (s : nat) : string :=
  match w_decl W s with
  | Some d => match d_ast d with
              | ADFn RDetached n _ _ _ => "." ++ n
              | ADFn (RAttached r) n _ _ _ => r ++ "." ++ n
              | _ => sym_name s
              end
  | None => sym_name s end.
(* the same, in backquotes *)
Definition bq (s : string) : string := "`" ++ s ++ "`".
Definition qs (s : nat) : string := bq (dname s).

Fixpoint show_ty_go (n : nat) (t : ty) : string :=
  match n with 0 => "…" | S n' =>
  match t with
  | TAbs g => sym_name g
  | TNom g [] => sym_name g
  | TNom g xs => sym_name g ++ "[" ++ String.concat ", " (map (fun q => sym_name (fst q) ++ "=" ++ show_ty_go n' (snd q)) xs) ++ "]"
  | TUnit => "()"
  | TMachine m => sym_name m
  | TRecord fs => "{" ++ String.concat ", " (map (fun q : string * ty => String.append (fst q) (String.append ": " (show_ty_go n' (snd q)))) fs) ++ "}"
  | TUnion [] => "|"
  | TUnion ms => String.concat " | " (map (show_ty_go n') ms)
  end end.
Definition show_ty (n : nat) (t : ty) : string := show_ty_go (n + 44) t.
Definition sty (t : ty) : string := show_ty_go 50 t.
Definition qty (t : ty) : string := bq (sty t).

(* a method's name as a key part: `.m` *)
Definition mname (m : nat) : string := "." ++ sym_name m.

(* an item as the old oracle's show_key: `v`, `f`, `T.m`, `F` *)
Definition show_key (k : item) : string :=
  match k with
  | ITy t => sym_name t
  | IVal v => sym_name v
  | IFn f => sym_name f
  | IMth r m _ => sty r ++ mname m
  | IFun u => sym_name u
  end.
Definition qkey (k : item) : string := bq (show_key k).

(* an item with its kind, for the new messages *)
Definition show_item (k : item) : string :=
  match k with
  | ITy t => "type " ++ sym_name t
  | IVal v => "val " ++ sym_name v
  | IFn f => "fn " ++ sym_name f
  | IMth r m _ => sty r ++ mname m
  | IFun u => "functor " ++ sym_name u
  end.

Definition show_sig (g : fsig) : string :=
  "(" ++ String.concat ", " (map sty (sg_params g)) ++ ") -> " ++ sty (sg_ret g).

(* a list of names: `A`, `B` *)
Definition qnames (l : list nat) : string := String.concat ", " (map qs l).

End Print.

Fixpoint suniq_dup (l : list string) : option string :=
  match l with [] => None | x :: r => if smem x r then Some x else suniq_dup r end.
Definition nat_str (n : nat) : string := Syntax.show_nat n.
