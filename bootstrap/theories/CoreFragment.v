(* Supporting core calculus for the component proofs. *)
From Stdlib Require Import String List Bool Arith.
From NM Require Export Audit.
Import ListNotations.
Local Open Scope list_scope.
Local Open Scope nat_scope.

(* The distinguished receiver type of core method signatures. *)
Definition this_sym : tsym := 0.

(* The head a union member discriminates by. *)
Definition member_head (tau : ty) : option nsym :=
  match tau with
  | TNom n _ => Some n
  | _ => None
  end.

(* Signature support for the core proof fragment. Executable function and
   functor bodies use Audit's xexpr and xbind; gsig also retains the core
   fragment's bodies for its separate safety and embedding proofs. *)
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
| EBindMth : ty -> msym -> subst -> fsym -> smap -> expr -> expr
(* Abstract functors [D78].  `bind φ = g;` binds φ to a functor g with a
   body (ς: g's argument items to φ's); `bind φ = ψ;` copies the binding of
   an abstract ψ in force (ς: ψ's argument items to φ's); `bind φ;` applies
   whatever φ is bound to (ς: φ's argument items to the items in force),
   and is an existential unpack when φ's result holds types. *)
| EBindFun  : usym -> usym -> smap -> smap -> expr -> expr
| EBindFunA : usym -> usym -> smap -> expr -> expr
| EApply    : usym -> subst -> smap -> expr -> expr.
(* Rule A: EBindFun u h sm smc e also records smc, the bind site's items
   answering h's captured inputs; EApply u θ sm e applies at θ. *)
(* In EBindMth the first field is the receiver type; its head is the key. *)

(* A functor body [D55]: a run of binds, the same five forms as the
   expression-level binds plus an application, with no continuation — the
   body's provisions are what it leaves in force.  Functors are elaborated
   away where they are applied by name (deviation 5); a body survives into
   the core only as the binding of an abstract functor [D78]. *)
Inductive bnd : Type :=
| BTy    : tsym -> ty -> bnd
| BVal   : vsym -> expr -> bnd
| BFn    : fsym -> fsym -> smap -> bnd
| BMth   : ty -> msym -> subst -> fsym -> smap -> bnd
| BFun   : usym -> usym -> smap -> smap -> bnd
| BFunA  : usym -> usym -> smap -> bnd
| BApply : usym -> subst -> smap -> bnd.

Inductive value : Type :=
| VUnit : value
| VTag  : nsym -> value -> value.

(* ===================== 6. Global signatures ============================== *)

Record fnsig : Type := mkSig { fs_params : list ty; fs_ret : ty }.

(* Σ: every symbol's kind, signature, and (concatenated) enclosing telescope.
   Method signatures may mention TAbs this_sym; the receiver is understood as
   an implicit first argument. *)
Record gsig : Type := mkGsigInterfaces {
  g_ty  : tsym -> bool;
  g_tag : nsym -> option (tele * ty);
  g_val : vsym -> option (tele * ty);
  g_fn  : fsym -> option (tele * fnsig * option expr);
  g_mth : msym -> option (tele * fnsig);
  (* a functor: its argument and result telescopes, and its body — None
     for an abstract functor `functor F: Args -> Result;` [D78] *)
  g_fun : usym -> option (tele * tele * option (list bnd));
  (* Rule A (D84 rule 1).  An abstract functor's *fixed* type inputs X(F).
     wf_gsig (wf_fun_fixed) makes it exactly M(F) ∖ P(F): the type symbols
     its result mentions (`fun_mentions`, closed through relevance), less
     its type outputs, less its brackets. *)
  g_fixed : usym -> list tsym;
  (* D84 rule 1: the brackets P(F) of an abstract functor `F[P…]`, read at
     each application.  An unbracketed `functor F: …;` elaborates to
     P(F) = M(F), so X(F) = ∅; a functor with a body has no brackets. *)
  g_poly : usym -> list tsym;
  (* A functor with a body: the inputs it captures where it is bound, and
     its pins — manifest equations on captured types (Leroy 1994): `bind
     Int = Num` in GcArrays's body elaborates to the pin (Int, Num).  Any
     item may be captured, vals included (D84 3b: vals by value); that a
     family's bindings bind no vals is a condition on families (rule 7,
     `family_binding`), not one the calculus imposes on capture. *)
  g_cap : usym -> tele * subst;
  (* A genuine nominal type interface whose payload is not observed. *)
  g_opaque : nsym -> option tele;
  (* A source interface with no well-formed callable schema. It can be
     assumed in a telescope, but has no runtime inhabitants. *)
  g_opaque_fun : usym -> bool
}.

Definition mkGsigFull ty tag val fn mth functor fixed poly cap opaque : gsig :=
  mkGsigInterfaces ty tag val fn mth functor fixed poly cap opaque (fun _ => false).

(* Existing calculi declare only payload-bearing nominal types. *)
Definition mkGsig ty tag val fn mth functor fixed poly cap : gsig :=
  mkGsigInterfaces ty tag val fn mth functor fixed poly cap (fun _ => None) (fun _ => false).

(* Core declarations use erased fixed dependencies. Immutable provisioning
   reads the actual fixed keys and their captured interpretations here. *)
Definition runtime_fixed (Sg : gsig) (P : xprog) (u : usym) : list tsym :=
  if xp_immutable P then xp_fixed_keys P u else g_fixed Sg u.
Definition runtime_signature (Sg : gsig) (P : xprog) : gsig :=
  mkGsigInterfaces (g_ty Sg) (g_tag Sg) (g_val Sg) (g_fn Sg) (g_mth Sg) (g_fun Sg)
    (runtime_fixed Sg P) (g_poly Sg) (g_cap Sg) (g_opaque Sg) (g_opaque_fun Sg).


(* ######################################################################## *)
(* Supporting core calculus                                                  *)
(* ######################################################################## *)

(* The executable language in Audit is MossX. This smaller calculus and its
   embedding support the core adequacy, specialization and safety contracts.
   They form a proof fragment rather than another compilation stage. *)

(* Core environments and evaluation. *)

(* ===================== 10. Dynamics ====================================== *)

(* Runtime contexts: val items carry values; fn and method items carry
   (code, captured context) pairs — the dynamic residue of [D2]. *)
Inductive dentry : Type :=
| DVal : value -> dentry
| DClo : fsym -> list (item * dentry) -> dentry
(* a functor binding [D78]: the functor with a body that answers, and the
   chain of re-keyings out to the abstract functor's arguments — no
   captured context: the arguments are supplied at each application *)
| DFun : usym -> list smap -> list (item * dentry) -> dentry.
(* Rule A: DFun h sms dc also holds dc, the entries h captured where it was
   bound — a closure, as DClo's. *)

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

(* Running a binding's chain of re-keyings: the outermost link (the last)
   first, so the result is keyed by the bound functor's own arguments. *)
Fixpoint run_chain (d : denv) (sms : list smap) : option denv :=
  match sms with
  | [] => Some d
  | sm :: r =>
      match run_chain d r with
      | Some d' => compose_smap d' sm
      | None => None
      end
  end.

(* The identity re-keying of a result telescope's non-type items: running
   it restricts an environment to exactly the result [D78]. *)
Definition id_smap (D : tele) : smap :=
  map (fun i => (i, i)) (filter (fun i => negb (is_ty_item i)) D).

Fixpoint find_arm (n : nsym) (arms : list (nsym * var * expr))
  : option (var * expr) :=
  match arms with
  | [] => None
  | (n', x, eb) :: r => if Nat.eqb n n' then Some (x, eb) else find_arm n r
  end.

Definition mk_venv (ws : list value) : venv :=
  combine (seq 0 (length ws)) ws.

(* Big-step evaluation (Figure 7 of the paper), with a functor body's run
   and an application [D78] defined alongside: an application looks up the
   binding, re-keys the caller's environment to the bound functor's own
   arguments, runs its binds there, and keeps exactly the result. *)
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
    eval Sg d g (EBindMth tauk m th f' sm e) w
| EvBindFun : forall d g u h sm smc dc e w,
    compose_smap d smc = Some dc ->
    eval Sg ((IFun u, DFun h [sm] dc) :: d) g e w ->
    eval Sg d g (EBindFun u h sm smc e) w
| EvBindFunA : forall d g u u' sm h sms dc e w,
    lookup_denv (IFun u') d = Some (DFun h sms dc) ->
    eval Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) g e w ->
    eval Sg d g (EBindFunA u u' sm e) w
| EvApply : forall d g u th sm dr e w,
    run_apply Sg d u th sm dr ->
    eval Sg (dr ++ d) g e w ->
    eval Sg d g (EApply u th sm e) w
with run_binds (Sg : gsig) : denv -> list bnd -> denv -> Prop :=
| RbNil : forall d, run_binds Sg d [] d
| RbTy : forall d t tau r d',
    run_binds Sg d r d' ->
    run_binds Sg d (BTy t tau :: r) d'
| RbVal : forall d v e1 w1 r d',
    eval Sg d [] e1 w1 ->
    run_binds Sg ((IVal v, DVal w1) :: d) r d' ->
    run_binds Sg d (BVal v e1 :: r) d'
| RbFn : forall d f f' sm dc r d',
    compose_smap d sm = Some dc ->
    run_binds Sg ((IFn f, DClo f' dc) :: d) r d' ->
    run_binds Sg d (BFn f f' sm :: r) d'
| RbMth : forall d tauk m th f' sm dc r d',
    compose_smap d sm = Some dc ->
    run_binds Sg ((IMth tauk m th, DClo f' dc) :: d) r d' ->
    run_binds Sg d (BMth tauk m th f' sm :: r) d'
| RbFun : forall d u h sm smc dc r d',
    compose_smap d smc = Some dc ->
    run_binds Sg ((IFun u, DFun h [sm] dc) :: d) r d' ->
    run_binds Sg d (BFun u h sm smc :: r) d'
| RbFunA : forall d u u' sm h sms dc r d',
    lookup_denv (IFun u') d = Some (DFun h sms dc) ->
    run_binds Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) r d' ->
    run_binds Sg d (BFunA u u' sm :: r) d'
| RbApply : forall d u th sm dr r d',
    run_apply Sg d u th sm dr ->
    run_binds Sg (dr ++ d) r d' ->
    run_binds Sg d (BApply u th sm :: r) d'
with run_apply (Sg : gsig) : denv -> usym -> subst -> smap -> denv -> Prop :=
(* Rule A: the body runs over its linked arguments, then what its binding
   captured; the result is filed under its items read at θ. *)
| RunApply : forall d u th sm h sms dc Dh Dr bs d1 d2 d3 dr,
    lookup_denv (IFun u) d = Some (DFun h sms dc) ->
    g_fun Sg h = Some (Dh, Dr, Some bs) ->
    compose_smap d sm = Some d1 ->
    run_chain d1 sms = Some d2 ->
    run_binds Sg (d2 ++ dc) bs d3 ->
    compose_smap d3 (inst_smap th Dr) = Some dr ->
    run_apply Sg d u th sm dr.

(* Outcomes of the fueled interpreter. Err denotes a missing lookup or
   shape mismatch, the instrumented error of Theorem 4.2. *)
Inductive res : Type :=
| Ok        : value -> res
| Err       : res
| OutOfFuel : res.

(* Specialization of code (moved from Specialization.v): substitute a
   grounding g of the type symbols into an elaborated term's type
   annotations, following type binds.  Provision keys and θ of
   applications stay (they are runtime keys). *)
Fixpoint spec (g : subst) (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVl v => EVl v
  | ETag n th e1 => ETag n (map_snd (app_subst g) th) (spec g e1)
  | ECallD f th sm args =>
      ECallD f (map_snd (app_subst g) th) sm
        ((fix go (l : list expr) : list expr :=
            match l with [] => [] | a :: r => spec g a :: go r end) args)
  | ECallA f args =>
      ECallA f ((fix go (l : list expr) : list expr :=
                   match l with [] => [] | a :: r => spec g a :: go r end) args)
  | EMeth e0 m prov args =>
      EMeth (spec g e0) m prov
        ((fix go (l : list expr) : list expr :=
            match l with [] => [] | a :: r => spec g a :: go r end) args)
  | EMatch e0 arms =>
      EMatch (spec g e0)
        ((fix go (l : list (nsym * var * expr)) :=
            match l with
            | [] => []
            | (n, x, eb) :: r => (n, x, spec g eb) :: go r
            end) arms)
  | ELet x e1 e2 => ELet x (spec g e1) (spec g e2)
  | EBindTy t tau e1 =>
      EBindTy t (app_subst g tau) (spec ((t, app_subst g tau) :: g) e1)
  | EBindVal v e1 e2 => EBindVal v (spec g e1) (spec g e2)
  | EBindFn f f' sm e1 => EBindFn f f' sm (spec g e1)
  | EBindMth tk m th f' sm e1 => EBindMth tk m th f' sm (spec g e1)
  | EBindFun u h sm smc e1 => EBindFun u h sm smc (spec g e1)
  | EBindFunA u w sm e1 => EBindFunA u w sm (spec g e1)
  (* Rule A: an application's θ keys its result, so it stays a label, as
     provision keys do *)
  | EApply u th sm e1 => EApply u th sm (spec g e1)
  end.


(* The core inside MossX: the embedding of
   Core Moss terms, runtime entries and programs into MossX, and when a
   core result and a MossX outcome agree.  The obligation
   ComponentContracts.PROVED_NOW.xinterp_conservative says MossX runs embedded core programs
   exactly as Core Moss does. ---- *)

Fixpoint emb_value (w : value) : xvalue :=
  match w with
  | VUnit => XVUnit
  | VTag n w1 => XVTag n (emb_value w1)
  end.

(* The embedding of the core: syntax, runtime entries, environments. *)
Fixpoint emb_expr (e : expr) : xexpr :=
  match e with
  | EVar x => XVar x
  | EUnit => XUnit
  | EVl v => XVl v
  | ETag n th e1 => XTag n th (emb_expr e1)
  | ECallD f th sm args => XCallD f th sm (map emb_expr args)
  | ECallA f args => XCallA f (map emb_expr args)
  | EMeth e0 m prov args => XMeth (emb_expr e0) m prov (map emb_expr args)
  | EMatch e0 arms =>
      XMatch (emb_expr e0)
             (map (fun a => match a with (n, x, eb) => (n, x, emb_expr eb) end) arms)
  | ELet x e1 e2 => XLet x (emb_expr e1) (emb_expr e2)
  | EBindTy t tau e1 => XBindTy t tau (emb_expr e1)
  | EBindVal v e1 e2 => XBindVal v (emb_expr e1) (emb_expr e2)
  | EBindFn f f' sm e1 => XBindFn f f' sm (emb_expr e1)
  | EBindMth tauk m th f' sm e1 => XBindMth tauk m th f' sm (emb_expr e1)
  | EBindFun u h sm smc e1 => XBindFun u h sm smc (emb_expr e1)
  | EBindFunA u u' sm e1 => XBindFunA u u' sm (emb_expr e1)
  | EApply u th sm e1 => XApply u th sm (emb_expr e1)
  end.

Definition emb_arms (arms : list (nsym * var * expr)) : list (nsym * var * xexpr) :=
  map (fun a => match a with (n, x, eb) => (n, x, emb_expr eb) end) arms.

Definition emb_bnd (b : bnd) : xbind :=
  match b with
  | BTy t tau => XBTy t tau
  | BVal v e1 => XBVal v (emb_expr e1)
  | BFn f f' sm => XBFn f f' sm
  | BMth tauk m th f' sm => XBMth tauk m th f' sm
  | BFun u h sm smc => XBFun u h sm smc
  | BFunA u u' sm => XBFunA u u' sm
  | BApply u th sm => XBApply u th sm
  end.

Fixpoint emb_dentry (en : dentry) : xdentry :=
  match en with
  | DVal w => XDVal (emb_value w)
  | DClo f d => XDClo f (map (fun p => match p with (i, en') => (i, emb_dentry en') end) d)
  | DFun h sms dc =>
      XDFun h sms (map (fun p => match p with (i, en') => (i, emb_dentry en') end) dc)
  end.

Definition emb_denv (d : denv) : xdenv :=
  map (fun p => match p with (i, en) => (i, emb_dentry en) end) d.
Definition emb_venv (g : venv) : xvenv :=
  map (fun p => match p with (x, w) => (x, emb_value w) end) g.

(* the core program as an extended one: the same bodies, embedded *)
Definition emb_prog (Sg : gsig) : xprog :=
  mkXProg (fun f => match g_fn Sg f with
                    | Some (_, _, Some b) => Some (emb_expr b)
                    | _ => None
                    end)
          (fun u => match g_fun Sg u with
                    | Some (_, Dr, Some bs) => Some (Dr, map emb_bnd bs)
                    | _ => None
                    end)
          0 0 0 [] (fun _ => None) [] (fun _ => []) (fun _ => []) false (fun _ => []) (fun _ => []).

Definition res_agree (r : res) (o : xout) : Prop :=
  match r, o with
  | Ok w, XOk w' => w' = emb_value w
  | Err, XErr _ => True
  | OutOfFuel, XFuel => True
  | _, _ => False
  end.


