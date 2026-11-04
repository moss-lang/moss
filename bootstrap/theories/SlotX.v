(* ------------------------------------------------------------------------ *)
(* SlotX.v — MossX's interpreter on a pre-resolved program.                  *)
(*                                                                           *)
(* Audit.v's `XSem.xinterp` finds every runtime entry by scanning the       *)
(* environment (an association list) with `item_eqb`, finds every fn body   *)
(* by symbol, and composes every callee's context the same way.  In the     *)
(* self-hosted compiler's run that is about a quarter of the time           *)
(* (item_eqb, ty_eqb, lookup_xdenv), but the *keys* of the environment at   *)
(* most program points are known before the run: a fn body starts in       *)
(* `xcompose d sm`, whose keys are exactly `map fst sm`, and every bind     *)
(* conses a known key.  So:                                                 *)
(*                                                                           *)
(*  - `cx sh ks e` compiles an expression, given the keys `ks` of the       *)
(*    environment it will run in (None: unknown), into `rexpr`, where each  *)
(*    lookup is pre-resolved to a position (`LAt j`), to a certain miss     *)
(*    (`LMiss`), or left dynamic (`LDyn i`, when the keys are unknown: after*)
(*    an `XApply`, in a functor body, in a fn entered with other keys);     *)
(*  - every fn body is compiled twice: for the keys `sh f` (an untrusted    *)
(*    guess, `shapes`) and for unknown keys.  A call uses the first when    *)
(*    the call's satisfaction map has exactly the keys `sh f` (decided at   *)
(*    compile time for direct calls, and recorded in the closure entry      *)
(*    `QClo b f env` for closures), else the second;                       *)
(*  - `rinterp` mirrors `xinterp` clause for clause, fuel for fuel, on      *)
(*    environments with the same keys and related entries.                  *)
(*                                                                           *)
(* Proved (kernel primitives only):                                         *)
(*  - rinterp_agrees: on related environments whose keys are as compiled    *)
(*    for, `rinterp` returns exactly what `XS.xinterp` returns (the very    *)
(*    same `xres`), for every fuel;                                         *)
(*  - rrun_main_eq: `rrun_main` = `XS.run_main` for every rprog that is the *)
(*    compilation of the xprog (rp_ok), every main, initial environment,    *)
(*    machine and fuel;                                                     *)
(*  - slot_run_main: the same for `rprog_of p` (the compiled `xprogram`)    *)
(*    against `program_of p`, whatever `shapes` guessed.                    *)
(* Nothing here touches Audit.v's definitions; the guess `shapes` is        *)
(* untrusted (a wrong guess only means the dynamic body runs).              *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia PrimInt63.
From NM Require Import Audit CoreMoss MossX FastX Elab Prune.
Import ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* ===================== 1. Pre-resolved lookups ========================== *)

Inductive rlk : Type :=
| LAt (j : nat)      (* the j-th entry *)
| LMiss              (* certainly absent *)
| LDyn (i : item).   (* scan for i, as lookup_xdenv does *)

Fixpoint pos (i : item) (K : list item) : option nat :=
  match K with
  | [] => None
  | k :: r => if item_eqb i k then Some 0 else option_map S (pos i r)
  end.

Definition mklk (ks : option (list item)) (i : item) : rlk :=
  match ks with
  | Some K => match pos i K with Some j => LAt j | None => LMiss end
  | None => LDyn i
  end.

Definition kpush (i : item) (ks : option (list item)) : option (list item) :=
  option_map (cons i) ks.

Definition plan := list (item * rlk).

Definition mkplan (ks : option (list item)) (sm : smap) : plan :=
  map (fun p => (fst p, mklk ks (snd p))) sm.

Fixpoint keys_eqb (a b : list item) : bool :=
  match a, b with
  | [], [] => true
  | x :: r, y :: s => item_eqb x y && keys_eqb r s
  | _, _ => false
  end.

(* ===================== 2. The pre-resolved program ====================== *)

(* Runtime entries: as xdentry, but a closure records whether its
   environment has the keys its fn's first body was compiled for. *)
Inductive rentry : Type :=
| QVal    : xvalue -> rentry
| QClo    : bool -> fsym -> list (item * rentry) -> rentry
| QFun    : usym -> list smap -> list (item * rentry) -> rentry
| QPrim   : prim -> rentry
| QFunSym : usym -> rentry.

Definition renv := list (item * rentry).

Fixpoint rlookup (i : item) (d : renv) : option rentry :=
  match d with
  | [] => None
  | (i', en) :: r => if item_eqb i i' then Some en else rlookup i r
  end.

Fixpoint rnth (j : nat) (d : renv) : option rentry :=
  match d with
  | [] => None
  | (_, en) :: r => match j with 0 => Some en | S j' => rnth j' r end
  end.

(* rnth with its first four steps unrolled, so that the extracted lookup of
   the (most frequent) first positions is inlined, without a call per step *)
Definition rnth2 (j : nat) (d : renv) : option rentry :=
  match d with
  | [] => None
  | (_, e0) :: r0 =>
      match j with
      | 0 => Some e0
      | S j1 =>
          match r0 with
          | [] => None
          | (_, e1) :: r1 =>
              match j1 with
              | 0 => Some e1
              | S j2 =>
                  match r1 with
                  | [] => None
                  | (_, e2) :: r2 =>
                      match j2 with
                      | 0 => Some e2
                      | S j3 =>
                          match r2 with
                          | [] => None
                          | (_, e3) :: r3 => match j3 with 0 => Some e3 | S j4 => rnth j4 r3 end
                          end
                      end
                  end
              end
          end
      end
  end.

Lemma rnth2_eq : forall j d, rnth2 j d = rnth j d.
Proof.
  intros j [|[i0 e0] [|[i1 e1] [|[i2 e2] [|[i3 e3] r]]]];
    destruct j as [|[|[|[|j]]]]; reflexivity.
Qed.

(* mk_xvenv without combine/seq: the same list *)
Fixpoint mkv_go (i : nat) (ws : list xvalue) : xvenv :=
  match ws with [] => [] | w :: r => (i, w) :: mkv_go (S i) r end.
Definition mkv (ws : list xvalue) : xvenv := mkv_go 0 ws.

Lemma mkv_eq : forall ws, mkv ws = mk_xvenv ws.
Proof.
  unfold mkv, mk_xvenv. intro ws. generalize 0.
  induction ws as [|w r IH]; intro i; simpl; auto. f_equal. apply IH.
Qed.

Definition rlook (l : rlk) (d : renv) : option rentry :=
  match l with
  | LAt j => rnth2 j d
  | LMiss => None
  | LDyn i => rlookup i d
  end.

(* ===================== 1b. Folded calls (FoldX.v) ======================= *)

(* A call to a small fn whose body only reads, tags, matches one pattern,
   applies primitives and calls such fns, folded at compile time into one
   expression over the *caller's* environment (FoldX.tos): its lookups are
   remapped through the call's satisfaction map.  `sev` evaluates it
   without fuel and without building the callee's environment; when it
   cannot (a lookup that is not a primitive, a primitive that does not
   return, a failed match) the call runs as before. *)
Inductive sexpr : Type :=
| SVar    : var -> sexpr
| SVl     : rlk -> sexpr
| STag    : nsym -> sexpr -> sexpr
| SPrim   : rlk -> list sexpr -> sexpr
| SCall   : nat -> list sexpr -> sexpr -> sexpr     (* guard position, args, body *)
| SMatch1 : sexpr -> xpat -> sexpr -> sexpr.

Section Sev.
Variable P : xprog.
Variable d : renv.

Fixpoint sev (g : xvenv) (st : mstate) (e : sexpr) {struct e} : option (xvalue * mstate) :=
  match e with
  | SVar x => match lookup_xvenv x g with Some w => Some (w, st) | None => None end
  | SVl l => match rlook l d with Some (QVal w) => Some (w, st) | _ => None end
  | STag t e1 => match sev g st e1 with Some (w, st1) => Some (XVTag t w, st1) | None => None end
  | SPrim l args =>
      match rlook l d with
      | Some (QPrim p) =>
          match (fix go (l : list sexpr) (st : mstate) : option (list xvalue * mstate) :=
                   match l with
                   | [] => Some ([], st)
                   | a :: r => match sev g st a with
                               | Some (w, st1) => match go r st1 with
                                                  | Some (ws, st2) => Some (w :: ws, st2)
                                                  | None => None end
                               | None => None end
                   end) args st with
          | Some (ws, st1) => match run_prim P p ws st1 with (XOk w, st2) => Some (w, st2) | _ => None end
          | None => None
          end
      | _ => None
      end
  | SCall j args body =>
      match rnth2 j d with
      | Some _ =>
          match (fix go (l : list sexpr) (st : mstate) : option (list xvalue * mstate) :=
                   match l with
                   | [] => Some ([], st)
                   | a :: r => match sev g st a with
                               | Some (w, st1) => match go r st1 with
                                                  | Some (ws, st2) => Some (w :: ws, st2)
                                                  | None => None end
                               | None => None end
                   end) args st with
          | Some (ws, st1) => sev (mkv ws) st1 body
          | None => None
          end
      | None => None
      end
  | SMatch1 e0 p body =>
      match sev g st e0 with
      | Some (w, st1) => match match_pat p w with PMatch bs => sev (bs ++ g) st1 body | _ => None end
      | None => None
      end
  end.

Definition sevl (g : xvenv) : list sexpr -> mstate -> option (list xvalue * mstate) :=
  fix go (l : list sexpr) (st : mstate) : option (list xvalue * mstate) :=
    match l with
    | [] => Some ([], st)
    | a :: r => match sev g st a with
                | Some (w, st1) => match go r st1 with
                                   | Some (ws, st2) => Some (w :: ws, st2)
                                   | None => None end
                | None => None end
    end.
End Sev.


Inductive rexpr : Type :=
| RVar      : var -> rexpr
| RUnit     : rexpr
| RVl       : rlk -> rexpr
| RTag      : nsym -> rexpr -> rexpr
| RCallD    : fsym -> bool -> plan -> list rexpr -> rexpr
| RCallA    : rlk -> list rexpr -> rexpr
| RMeth     : rexpr -> rlk -> list rexpr -> rexpr
| RMatch    : rexpr -> list (nsym * var * rexpr) -> rexpr
| RLet      : var -> rexpr -> rexpr -> rexpr
| RBindTy   : rexpr -> rexpr
| RBindVal  : vsym -> rexpr -> rexpr -> rexpr
| RBindClo  : item -> fsym -> bool -> plan -> rexpr -> rexpr
| RBindFun  : usym -> usym -> smap -> plan -> rexpr -> rexpr
| RBindFunA : usym -> rlk -> smap -> rexpr -> rexpr
| RApply    : rlk -> subst -> plan -> rexpr -> rexpr
| RSeq      : rexpr -> rexpr -> rexpr
| RAssign   : var -> rexpr -> rexpr
| RIf       : rexpr -> rexpr -> rexpr -> rexpr
| RWhile    : rexpr -> rexpr -> rexpr
| RLoop     : rexpr -> rexpr
| RReturn   : rexpr -> rexpr
| RBreak    : rexpr
| RRec      : list (fld * rexpr) -> rexpr
| RField    : rexpr -> fld -> rexpr
| RMatchP   : rexpr -> list (xpat * rexpr) -> rexpr
| RBindFunSym  : usym -> usym -> rexpr -> rexpr
| RBindFunCopy : usym -> rlk -> rexpr -> rexpr
| RApplyBr  : rlk -> list (usym * list rbind * plan) -> rexpr -> rexpr
| RBindPrim : item -> prim -> rexpr -> rexpr
(* FoldX: `RCallD f true pl args`, with the fuel D the call needs below it
   and its folded arguments and body; produced only by FoldX.fold_e *)
| RCallF    : fsym -> plan -> list rexpr -> nat -> list sexpr -> sexpr -> rexpr
with rbind : Type :=
| RBTy    : rbind
| RBVal   : vsym -> rexpr -> rbind
| RBClo   : item -> fsym -> bool -> plan -> rbind
| RBFun   : usym -> usym -> smap -> plan -> rbind
| RBFunA  : usym -> rlk -> smap -> rbind
| RBApply : rlk -> subst -> plan -> rbind
| RBFunSym  : usym -> usym -> rbind
| RBFunCopy : usym -> rlk -> rbind
| RBApplyBr : rlk -> list (usym * list rbind * plan) -> rbind
| RBPrim  : item -> prim -> rbind.

Record rprog : Type := mkRProg {
  rp_x   : xprog;                                (* prims, Bool, argv, files *)
  rp_fn  : fsym -> option (rexpr * rexpr);       (* (for sh f, for unknown keys) *)
  rp_fun : usym -> option (tele * list rbind)    (* for unknown keys *)
}.

Fixpoint rcompose (d : renv) (pl : plan) : option renv :=
  match pl with
  | [] => Some []
  | (req, l) :: r =>
      match rlook l d, rcompose d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

(* A folded call: its arguments in the caller's environment, then its body
   in the callee's environment (composed once), with fresh locals. *)
Definition fastcall (P : xprog) (d : renv) (g : xvenv) (st : mstate)
    (pl : plan) (sargs : list sexpr) (sbody : sexpr) : option (xvalue * mstate) :=
  match sevl P d g sargs st with
  | Some (ws, st1) => match rcompose d pl with
                      | Some d' => sev P d' (mkv ws) st1 sbody
                      | None => None
                      end
  | None => None
  end.

Fixpoint rcompose_dyn (d : renv) (sm : smap) : option renv :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match rlookup s d, rcompose_dyn d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

Fixpoint rchain (d : renv) (sms : list smap) : option renv :=
  match sms with
  | [] => Some d
  | sm :: r =>
      match rchain d r with
      | Some d' => rcompose_dyn d' sm
      | None => None
      end
  end.

Fixpoint rfind_arm (n : nsym) (arms : list (nsym * var * rexpr)) : option (var * rexpr) :=
  match arms with
  | [] => None
  | (n', x, eb) :: r => if Nat.eqb n n' then Some (x, eb) else rfind_arm n r
  end.

Fixpoint rselect_arm (arms : list (xpat * rexpr)) (w : xvalue) : option (xvenv * rexpr) :=
  match arms with
  | [] => None
  | (p, eb) :: r =>
      match match_pat p w with
      | PMatch bs => Some (bs, eb)
      | PNo => rselect_arm r w
      | PBad => None
      end
  end.

Fixpoint rfind_branch (h : usym) (brs : list (usym * list rbind * plan))
  : option (list rbind * plan) :=
  match brs with
  | [] => None
  | (h', bs, pairs) :: r => if Nat.eqb h h' then Some (bs, pairs) else rfind_branch h r
  end.

(* ===================== 3. The interpreter ================================ *)

Fixpoint rinterp (fuel : nat) (R : rprog) (d : renv) (g : xvenv) (st : mstate)
    (e : rexpr) {struct fuel} : xres :=
  match fuel with
  | 0 => XR XFuel g st
  | S k =>
      match e with
      | RVar x => match lookup_xvenv x g with
                  | Some w => XR (XOk w) g st
                  | None => XR (XErr e_var) g st
                  end
      | RUnit => XR (XOk XVUnit) g st
      | RVl l => match rlook l d with
                 | Some (QVal w) => XR (XOk w) g st
                 | _ => XR (XErr e_val) g st
                 end
      | RTag n e1 =>
          match rinterp k R d g st e1 with
          | XR (XOk w) g1 st1 => XR (XOk (XVTag n w)) g1 st1
          | r => r
          end
      | RCallD f b pl args =>
          match rp_fn R f with
          | Some (bs, bd) =>
              match rinterp_list k R d g st args with
              | inl (ws, g1, st1) =>
                  match rcompose d pl with
                  | Some d' => ret_call g1 (rinterp k R d' (mkv ws) st1 (if b then bs else bd))
                  | None => XR (XErr e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => XR (XErr e_nobody) g st
          end
      | RCallA l args =>
          match rlook l d with
          | Some (QClo b f' df) =>
              match rp_fn R f' with
              | Some (bs, bd) =>
                  match rinterp_list k R d g st args with
                  | inl (ws, g1, st1) =>
                      ret_call g1 (rinterp k R df (mkv ws) st1 (if b then bs else bd))
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (QPrim p) =>
              match rinterp_list k R d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim (rp_x R) p ws st1 in XR o g1 st2
              | inr r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | RMeth e0 l args =>
          match rlook l d with
          | Some (QClo b f' d') =>
              match rp_fn R f' with
              | Some (bs, bd) =>
                  match rinterp k R d g st e0 with
                  | XR (XOk w0) g0 st0 =>
                      match rinterp_list k R d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (rinterp k R d' (mkv (w0 :: ws)) st1 (if b then bs else bd))
                      | inr r => r
                      end
                  | r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (QPrim p) =>
              match rinterp k R d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match rinterp_list k R d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim (rp_x R) p (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | RMatch e0 arms =>
          match rinterp k R d g st e0 with
          | XR (XOk (XVTag n w')) g1 st1 =>
              match rfind_arm n arms with
              | Some (x, eb) => pop1 (rinterp k R d ((x, w') :: g1) st1 eb)
              | None => XR (XErr e_match) g1 st1
              end
          | XR (XOk _) g1 st1 => XR (XErr e_match) g1 st1
          | r => r
          end
      | RLet x e1 e2 =>
          match rinterp k R d g st e1 with
          | XR (XOk w1) g1 st1 => pop1 (rinterp k R d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | RBindTy e1 => rinterp k R d g st e1
      | RBindVal v e1 e2 =>
          match rinterp k R d g st e1 with
          | XR (XOk w1) g1 st1 => rinterp k R ((IVal v, QVal w1) :: d) g1 st1 e2
          | r => r
          end
      | RBindClo i f' b pl e1 =>
          match rcompose d pl with
          | Some dc => rinterp k R ((i, QClo b f' dc) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | RBindFun u h sm pl e1 =>
          match rcompose d pl with
          | Some dc => rinterp k R ((IFun u, QFun h [sm] dc) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | RBindFunA u l sm e1 =>
          match rlook l d with
          | Some (QFun h sms dc) =>
              rinterp k R ((IFun u, QFun h (sms ++ [sm]) dc) :: d) g st e1
          | _ => XR (XErr e_fun) g st
          end
      | RApply l th pl e1 =>
          match rinterp_apply k R d l th pl st with
          | inl (dr, st1) => rinterp k R (dr ++ d) g st1 e1
          | inr r => with_g g r
          end
      | RSeq e1 e2 =>
          match rinterp k R d g st e1 with
          | XR (XOk _) g1 st1 => rinterp k R d g1 st1 e2
          | r => r
          end
      | RAssign x e1 =>
          match rinterp k R d g st e1 with
          | XR (XOk w) g1 st1 =>
              match assign_xvenv x w g1 with
              | Some g2 => XR (XOk XVUnit) g2 st1
              | None => XR (XErr e_assign) g1 st1
              end
          | r => r
          end
      | RIf c e1 e2 =>
          match rinterp k R d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy (rp_x R) w with
              | Some true => rinterp k R d g1 st1 e1
              | Some false => rinterp k R d g1 st1 e2
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | RWhile c b =>
          match rinterp k R d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy (rp_x R) w with
              | Some true =>
                  match rinterp k R d g1 st1 b with
                  | XR (XOk _) g2 st2 => rinterp k R d g2 st2 (RWhile c b)
                  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
                  | r => r
                  end
              | Some false => XR (XOk XVUnit) g1 st1
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | RLoop b =>
          match rinterp k R d g st b with
          | XR (XOk _) g2 st2 => rinterp k R d g2 st2 (RLoop b)
          | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
          | r => r
          end
      | RReturn e1 =>
          match rinterp k R d g st e1 with
          | XR (XOk w) g1 st1 => XR (XRet w) g1 st1
          | r => r
          end
      | RBreak => XR XBrk g st
      | RRec fs =>
          match rinterp_list k R d g st (map snd fs) with
          | inl (ws, g1, st1) => XR (XOk (XVRec (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | RField e1 f =>
          match rinterp k R d g st e1 with
          | XR (XOk w) g1 st1 =>
              match get_field f w with
              | Some v => XR (XOk v) g1 st1
              | None => XR (XErr e_field) g1 st1
              end
          | r => r
          end
      | RMatchP e0 arms =>
          match rinterp k R d g st e0 with
          | XR (XOk w) g1 st1 =>
              match rselect_arm arms w with
              | Some (bs, eb) =>
                  popn (List.length bs) (rinterp k R d (bs ++ g1) st1 eb)
              | None => XR (XErr e_pat) g1 st1
              end
          | r => r
          end
      | RBindFunSym u h e1 => rinterp k R ((IFun u, QFunSym h) :: d) g st e1
      | RBindFunCopy u l e1 =>
          match rlook l d with
          | Some en => rinterp k R ((IFun u, en) :: d) g st e1
          | None => XR (XErr e_fun) g st
          end
      | RApplyBr l brs e1 =>
          match rinterp_branch k R d g st l brs with
          | inl (dr, g1, st1) => rinterp k R (dr ++ d) g1 st1 e1
          | inr r => r
          end
      | RBindPrim i p e1 => rinterp k R ((i, QPrim p) :: d) g st e1
      | RCallF f pl args D sargs sb =>
          match (if Nat.leb D k then fastcall (rp_x R) d g st pl sargs sb else None) with
          | Some (w, st2) => XR (XOk w) g st2
          | None =>
              match rp_fn R f with
              | Some (bs, bd) =>
                  match rinterp_list k R d g st args with
                  | inl (ws, g1, st1) =>
                      match rcompose d pl with
                      | Some d' => ret_call g1 (rinterp k R d' (mkv ws) st1 bs)
                      | None => XR (XErr e_compose) g1 st1
                      end
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          end
      end
  end
with rinterp_list (fuel : nat) (R : rprog) (d : renv) (g : xvenv) (st : mstate)
    (l : list rexpr) {struct fuel} : (list xvalue * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match rinterp k R d g st e1 with
          | XR (XOk w) g1 st1 =>
              match rinterp_list k R d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with rinterp_binds (fuel : nat) (R : rprog) (d : renv) (g : xvenv) (st : mstate)
    (bs : list rbind) {struct fuel} : (renv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | RBTy :: r => rinterp_binds k R d g st r
      | RBVal v e1 :: r =>
          match rinterp k R d g st e1 with
          | XR (XOk w1) g1 st1 => rinterp_binds k R ((IVal v, QVal w1) :: d) g1 st1 r
          | rr => inr rr
          end
      | RBClo i f' b pl :: r =>
          match rcompose d pl with
          | Some dc => rinterp_binds k R ((i, QClo b f' dc) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | RBFun u h sm pl :: r =>
          match rcompose d pl with
          | Some dc => rinterp_binds k R ((IFun u, QFun h [sm] dc) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | RBFunA u l sm :: r =>
          match rlook l d with
          | Some (QFun h sms dc) =>
              rinterp_binds k R ((IFun u, QFun h (sms ++ [sm]) dc) :: d) g st r
          | _ => inr (XR (XErr e_fun) g st)
          end
      | RBApply l th pl :: r =>
          match rinterp_apply k R d l th pl st with
          | inl (dr, st1) => rinterp_binds k R (dr ++ d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | RBFunSym u h :: r => rinterp_binds k R ((IFun u, QFunSym h) :: d) g st r
      | RBFunCopy u l :: r =>
          match rlook l d with
          | Some en => rinterp_binds k R ((IFun u, en) :: d) g st r
          | None => inr (XR (XErr e_fun) g st)
          end
      | RBApplyBr l brs :: r =>
          match rinterp_branch k R d g st l brs with
          | inl (dr, g1, st1) => rinterp_binds k R (dr ++ d) g1 st1 r
          | inr rr => inr rr
          end
      | RBPrim i p :: r => rinterp_binds k R ((i, QPrim p) :: d) g st r
      end
  end
with rinterp_apply (fuel : nat) (R : rprog) (d : renv) (l : rlk) (th : subst)
    (pl : plan) (st : mstate) {struct fuel} : (renv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match rlook l d with
      | Some (QFun h sms dc) =>
          match rp_fun R h with
          | Some (Dr, bs) =>
              match rcompose d pl with
              | Some d1 =>
                  match rchain d1 sms with
                  | Some d2 =>
                      match rinterp_binds k R (d2 ++ dc) [] st bs with
                      | inl (d3, _, st3) =>
                          match rcompose_dyn d3 (inst_smap th Dr) with
                          | Some dr => inl (dr, st3)
                          | None => inr (XR (XErr e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end
with rinterp_branch (fuel : nat) (R : rprog) (d : renv) (g : xvenv) (st : mstate)
    (l : rlk) (brs : list (usym * list rbind * plan)) {struct fuel}
    : (renv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match rlook l d with
      | Some (QFunSym h) =>
          match rfind_branch h brs with
          | Some (bs, pairs) =>
              match rinterp_binds k R d g st bs with
              | inl (d3, g3, st3) =>
                  match rcompose d3 pairs with
                  | Some dr => inl (dr, g3, st3)
                  | None => inr (XR (XErr e_compose) g3 st3)
                  end
              | inr rr => inr rr
              end
          | None => inr (XR (XErr e_branch) g st)
          end
      | _ => inr (XR (XErr e_fun) g st)
      end
  end.

(* ===================== 4. The compiler ================================== *)

Section Compile.
(* sh f: the keys f's first body is compiled for (any guess is sound) *)
Variable sh : fsym -> option (list item).

Definition shape_ok (f : fsym) (sm : smap) : bool :=
  match sh f with Some K => keys_eqb K (map fst sm) | None => false end.

(* the keys a branch list leaves, if every branch leaves the same *)
Definition brkeys (brs : list (usym * list xbind * smap)) : option (list item) :=
  match brs with
  | [] => None
  | (_, _, p) :: r =>
      if forallb (fun b => match b with (_, _, p') => keys_eqb (map fst p) (map fst p') end) r
      then Some (map fst p) else None
  end.

Definition kbr (ks : option (list item)) (brs : list (usym * list xbind * smap)) : option (list item) :=
  match ks, brkeys brs with
  | Some K, Some B => Some (B ++ K)
  | _, _ => None
  end.

(* the keys after a bind *)
Definition kafter (ks : option (list item)) (b : xbind) : option (list item) :=
  match b with
  | XBTy _ _ => ks
  | XBVal v _ => kpush (IVal v) ks
  | XBFn f _ _ => kpush (IFn f) ks
  | XBMth t m th _ _ => kpush (IMth t m th) ks
  | XBFun u _ _ _ => kpush (IFun u) ks
  | XBFunA u _ _ => kpush (IFun u) ks
  | XBApply _ _ _ => None
  | XBFunSym u _ => kpush (IFun u) ks
  | XBFunCopy u _ => kpush (IFun u) ks
  | XBApplyBr _ brs => kbr ks brs
  | XBPrim i _ => kpush i ks
  end.

Definition kfold (ks : option (list item)) (bs : list xbind) : option (list item) :=
  fold_left kafter bs ks.

Fixpoint cx (ks : option (list item)) (e : xexpr) {struct e} : rexpr :=
  match e with
  | XVar x => RVar x
  | XUnit => RUnit
  | XVl v => RVl (mklk ks (IVal v))
  | XTag n _ e1 => RTag n (cx ks e1)
  | XCallD f _ sm args => RCallD f (shape_ok f sm) (mkplan ks sm) (map (cx ks) args)
  | XCallA f args => RCallA (mklk ks (IFn f)) (map (cx ks) args)
  | XMeth e0 _ prov args => RMeth (cx ks e0) (mklk ks prov) (map (cx ks) args)
  | XMatch e0 arms =>
      RMatch (cx ks e0) (map (fun a => match a with (n, x, eb) => (n, x, cx ks eb) end) arms)
  | XLet x e1 e2 => RLet x (cx ks e1) (cx ks e2)
  | XBindTy _ _ e1 => RBindTy (cx ks e1)
  | XBindVal v e1 e2 => RBindVal v (cx ks e1) (cx (kpush (IVal v) ks) e2)
  | XBindFn f f' sm e1 =>
      RBindClo (IFn f) f' (shape_ok f' sm) (mkplan ks sm) (cx (kpush (IFn f) ks) e1)
  | XBindMth t m th f' sm e1 =>
      RBindClo (IMth t m th) f' (shape_ok f' sm) (mkplan ks sm) (cx (kpush (IMth t m th) ks) e1)
  | XBindFun u h sm smc e1 => RBindFun u h sm (mkplan ks smc) (cx (kpush (IFun u) ks) e1)
  | XBindFunA u u' sm e1 => RBindFunA u (mklk ks (IFun u')) sm (cx (kpush (IFun u) ks) e1)
  | XApply u th sm e1 => RApply (mklk ks (IFun u)) th (mkplan ks sm) (cx None e1)
  | XSeq e1 e2 => RSeq (cx ks e1) (cx ks e2)
  | XAssign x e1 => RAssign x (cx ks e1)
  | XIf c e1 e2 => RIf (cx ks c) (cx ks e1) (cx ks e2)
  | XWhile c b => RWhile (cx ks c) (cx ks b)
  | XLoop b => RLoop (cx ks b)
  | XReturn e1 => RReturn (cx ks e1)
  | XBreak => RBreak
  | XRec fs => RRec (map (fun p => match p with (f, e1) => (f, cx ks e1) end) fs)
  | XField e1 f => RField (cx ks e1) f
  | XMatchP e0 arms =>
      RMatchP (cx ks e0) (map (fun a => match a with (p, eb) => (p, cx ks eb) end) arms)
  | XBindFunSym u h e1 => RBindFunSym u h (cx (kpush (IFun u) ks) e1)
  | XBindFunCopy u u' e1 => RBindFunCopy u (mklk ks (IFun u')) (cx (kpush (IFun u) ks) e1)
  | XApplyBr u brs e1 =>
      RApplyBr (mklk ks (IFun u))
        (map (fun br => match br with (h, bs, pairs) =>
                (h, (fix go (ks0 : option (list item)) (l : list xbind) : list rbind :=
                       match l with [] => [] | b :: r => cxb ks0 b :: go (kafter ks0 b) r end) ks bs,
                 mkplan (kfold ks bs) pairs) end) brs)
        (cx (kbr ks brs) e1)
  | XBindPrim i p e1 => RBindPrim i p (cx (kpush i ks) e1)
  end
with cxb (ks : option (list item)) (b : xbind) {struct b} : rbind :=
  match b with
  | XBTy _ _ => RBTy
  | XBVal v e1 => RBVal v (cx ks e1)
  | XBFn f f' sm => RBClo (IFn f) f' (shape_ok f' sm) (mkplan ks sm)
  | XBMth t m th f' sm => RBClo (IMth t m th) f' (shape_ok f' sm) (mkplan ks sm)
  | XBFun u h sm smc => RBFun u h sm (mkplan ks smc)
  | XBFunA u u' sm => RBFunA u (mklk ks (IFun u')) sm
  | XBApply u th sm => RBApply (mklk ks (IFun u)) th (mkplan ks sm)
  | XBFunSym u h => RBFunSym u h
  | XBFunCopy u u' => RBFunCopy u (mklk ks (IFun u'))
  | XBApplyBr u brs =>
      RBApplyBr (mklk ks (IFun u))
        (map (fun br => match br with (h, bs, pairs) =>
                (h, (fix go (ks0 : option (list item)) (l : list xbind) : list rbind :=
                       match l with [] => [] | b :: r => cxb ks0 b :: go (kafter ks0 b) r end) ks bs,
                 mkplan (kfold ks bs) pairs) end) brs)
  | XBPrim i p => RBPrim i p
  end.

Fixpoint cxbs (ks : option (list item)) (l : list xbind) : list rbind :=
  match l with [] => [] | b :: r => cxb ks b :: cxbs (kafter ks b) r end.

Definition cbrs (ks : option (list item)) (brs : list (usym * list xbind * smap))
  : list (usym * list rbind * plan) :=
  map (fun br => match br with (h, bs, pairs) => (h, cxbs ks bs, mkplan (kfold ks bs) pairs) end) brs.

End Compile.

(* ===================== 5. Representation ================================ *)

Section Rel.
Variable sh : fsym -> option (list item).

(* Entries are related structurally; a closure flagged `true` has exactly
   the keys its fn's first body was compiled for. *)
Inductive erel : rentry -> xdentry -> Prop :=
| EVal : forall w, erel (QVal w) (XDVal w)
| EClo : forall b f re d, envrel re d -> (b = true -> sh f = Some (map fst d)) ->
         erel (QClo b f re) (XDClo f d)
| EFun : forall h sms re d, envrel re d -> erel (QFun h sms re) (XDFun h sms d)
| EPrim : forall p, erel (QPrim p) (XDPrim p)
| EFunSym : forall h, erel (QFunSym h) (XDFunSym h)
with envrel : renv -> xdenv -> Prop :=
| ENil : envrel [] []
| ECons : forall i a b re d, erel a b -> envrel re d -> envrel ((i, a) :: re) ((i, b) :: d).

Definition orel (a : option rentry) (b : option xdentry) : Prop :=
  match a, b with
  | Some x, Some y => erel x y
  | None, None => True
  | _, _ => False
  end.

Definition crel (a : option renv) (b : option xdenv) : Prop :=
  match a, b with
  | Some x, Some y => envrel x y
  | None, None => True
  | _, _ => False
  end.

(* the keys an environment is compiled for, if known *)
Definition kok (ks : option (list item)) (K : list item) : Prop :=
  match ks with Some K' => K' = K | None => True end.

Lemma envrel_keys : forall re d, envrel re d -> map fst re = map fst d.
Proof. induction 1; simpl; congruence. Qed.

Lemma rlookup_rel : forall i re d, envrel re d -> orel (rlookup i re) (lookup_xdenv i d).
Proof.
  intros i re d H. induction H as [|j a b re d Hab H IH]; simpl; auto.
  destruct (item_eqb i j); simpl; auto.
Qed.

Lemma rlookup_pos : forall i re,
  rlookup i re = match pos i (map fst re) with Some j => rnth j re | None => None end.
Proof.
  intros i re. induction re as [|[j en] r IH]; simpl; auto.
  destruct (item_eqb i j); simpl; auto. rewrite IH.
  destruct (pos i (map fst r)); reflexivity.
Qed.

Lemma rlook_mklk : forall ks i re d, envrel re d -> kok ks (map fst d) ->
  rlook (mklk ks i) re = rlookup i re.
Proof.
  intros [K|] i re d H Hk; simpl in *; auto. subst K.
  rewrite <- (envrel_keys H), rlookup_pos. destruct (pos i (map fst re)); [apply rnth2_eq|reflexivity].
Qed.

Lemma rcompose_mkplan : forall ks sm re d, envrel re d -> kok ks (map fst d) ->
  rcompose re (mkplan ks sm) = rcompose_dyn re sm.
Proof.
  intros ks sm re d H Hk. induction sm as [|[req s] r IH]; simpl; auto.
  rewrite (@rlook_mklk ks s re d H Hk), IH. reflexivity.
Qed.

Lemma rcompose_dyn_rel : forall sm re d, envrel re d -> crel (rcompose_dyn re sm) (xcompose d sm).
Proof.
  intros sm re d H. induction sm as [|[req s] r IH]; simpl; [constructor|].
  pose proof (rlookup_rel s H) as Hl. unfold orel in Hl.
  destruct (rlookup s re), (lookup_xdenv s d); try contradiction;
  destruct (rcompose_dyn re r), (xcompose d r); simpl in *; try contradiction; auto.
  constructor; auto.
Qed.

Lemma xcompose_keys : forall sm d d', xcompose d sm = Some d' -> map fst d' = map fst sm.
Proof.
  induction sm as [|[req s] r IH]; simpl; intros d d' H.
  - inversion H; reflexivity.
  - destruct (lookup_xdenv s d); [|discriminate].
    destruct (xcompose d r) as [d0|] eqn:E; [|discriminate].
    inversion H; subst. simpl. f_equal. eapply IH; eauto.
Qed.

Lemma envrel_app : forall a a' b b', envrel a a' -> envrel b b' -> envrel (a ++ b) (a' ++ b').
Proof. intros a a' b b' H. induction H; simpl; auto. intro. constructor; auto. Qed.

Lemma rchain_rel : forall sms re d, envrel re d -> crel (rchain re sms) (xrun_chain d sms).
Proof.
  induction sms as [|sm r IH]; intros re d H; simpl; auto.
  specialize (IH re d H).
  destruct (rchain re r), (xrun_chain d r); simpl in *; try contradiction; auto.
  apply rcompose_dyn_rel; auto.
Qed.

Lemma keys_eqb_eq : forall a b, keys_eqb a b = true -> a = b.
Proof.
  induction a as [|x a IH]; destruct b as [|y b]; simpl; intros H; try discriminate; auto.
  apply andb_true_iff in H as [H1 H2]. apply item_eqb_eq in H1. subst. f_equal; auto.
Qed.

Lemma shape_ok_true : forall f sm, shape_ok sh f sm = true -> sh f = Some (map fst sm).
Proof.
  unfold shape_ok. intros f sm H. destruct (sh f); [|discriminate].
  apply keys_eqb_eq in H. congruence.
Qed.

Lemma kok_push : forall ks K i, kok ks K -> kok (kpush i ks) (i :: K).
Proof. intros [K'|] K i H; simpl in *; auto. congruence. Qed.

Lemma rfind_arm_cx : forall ks n arms,
  rfind_arm n (map (fun a => match a with (n, x, eb) => (n, x, cx sh ks eb) end) arms)
  = match find_xarm n arms with Some (x, eb) => Some (x, cx sh ks eb) | None => None end.
Proof.
  intros ks n arms. induction arms as [|[[n' x] eb] r IH]; simpl; auto.
  destruct (Nat.eqb n n'); auto.
Qed.

Lemma rselect_arm_cx : forall ks arms w,
  rselect_arm (map (fun a => match a with (p, eb) => (p, cx sh ks eb) end) arms) w
  = match select_arm arms w with Some (bs, eb) => Some (bs, cx sh ks eb) | None => None end.
Proof.
  intros ks arms w. induction arms as [|[p eb] r IH]; simpl; auto.
  destruct (match_pat p w); auto.
Qed.

Lemma rec_snd : forall ks (fs : list (fld * xexpr)),
  map snd (map (fun p => match p with (f, e1) => (f, cx sh ks e1) end) fs) = map (cx sh ks) (map snd fs).
Proof. induction fs as [|[f e] r IH]; simpl; congruence. Qed.

Lemma rec_fst : forall ks (fs : list (fld * xexpr)),
  map fst (map (fun p => match p with (f, e1) => (f, cx sh ks e1) end) fs) = map fst fs.
Proof. induction fs as [|[f e] r IH]; simpl; congruence. Qed.

Lemma rfind_branch_cbrs : forall ks h brs,
  rfind_branch h (cbrs sh ks brs)
  = match find_branch h brs with
    | Some (bs, pairs) => Some (cxbs sh ks bs, mkplan (kfold ks bs) pairs)
    | None => None end.
Proof.
  intros ks h brs. induction brs as [|[[h' bs] pairs] r IH]; simpl; auto.
  destruct (Nat.eqb h h'); auto.
Qed.

Lemma find_branch_in : forall h brs bs pairs, find_branch h brs = Some (bs, pairs) ->
  exists h', In (h', bs, pairs) brs.
Proof.
  intros h brs. induction brs as [|[[h' bs'] p'] r IH]; simpl; intros bs pairs H; [discriminate|].
  destruct (Nat.eqb h h').
  - inversion H; subst. eauto.
  - destruct (IH _ _ H) as [h0 Hi]. eauto.
Qed.

Lemma find_branch_keys : forall h brs bs pairs B,
  find_branch h brs = Some (bs, pairs) -> brkeys brs = Some B -> map fst pairs = B.
Proof.
  intros h brs bs pairs B Hf Hb. destruct brs as [|[[h0 bs0] p0] r]; simpl in *; [discriminate|].
  match type of Hb with context [if ?c then _ else _] => destruct c eqn:Ef end;
    inversion Hb; subst B; clear Hb.
  destruct (Nat.eqb h h0).
  - inversion Hf; subst; auto.
  - destruct (find_branch_in _ _ Hf) as [h' Hi].
    rewrite forallb_forall in Ef. specialize (Ef _ Hi). simpl in Ef.
    symmetry. apply keys_eqb_eq. exact Ef.
Qed.

Lemma cx_applybr : forall ks u brs e1,
  cx sh ks (XApplyBr u brs e1) = RApplyBr (mklk ks (IFun u)) (cbrs sh ks brs) (cx sh (kbr ks brs) e1).
Proof. reflexivity. Qed.

Lemma cxb_applybr : forall ks u brs,
  cxb sh ks (XBApplyBr u brs) = RBApplyBr (mklk ks (IFun u)) (cbrs sh ks brs).
Proof. reflexivity. Qed.

End Rel.

(* ===================== 6. Agreement ====================================== *)

Section Agree.
Variable sh : fsym -> option (list item).
Variable R : rprog.
(* R is P compiled: every body compiled (twice) by `cx` *)
Local Notation P := (rp_x R).
Hypothesis Hfn : forall f, rp_fn R f =
  match xp_fn P f with Some b => Some (cx sh (sh f) b, cx sh None b) | None => None end.
Hypothesis Hfun : forall h, rp_fun R h =
  match xp_fun P h with Some (Dr, bs) => Some (Dr, cxbs sh None bs) | None => None end.

Definition brel (ks : option (list item)) (a : (renv * xvenv * mstate) + xres)
    (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (re, g, st), inl (d, g', st') => envrel sh re d /\ kok ks (map fst d) /\ g = g' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition arel (a : (renv * mstate) + xres) (b : (xdenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (l, st), inl (l', st') => envrel sh l l' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition rrel (brs : list (usym * list xbind * smap)) (a : (renv * xvenv * mstate) + xres)
    (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (l, g, st), inl (l', g', st') =>
      envrel sh l l' /\ g = g' /\ st = st' /\ (forall B, brkeys brs = Some B -> map fst l' = B)
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition AGREE (k : nat) : Prop :=
  (forall ks re d g st e, envrel sh re d -> kok ks (map fst d) ->
     rinterp k R re g st (cx sh ks e) = xinterp k P d g st e)
  /\ (forall ks re d g st l, envrel sh re d -> kok ks (map fst d) ->
     rinterp_list k R re g st (map (cx sh ks) l) = xinterp_list k P d g st l)
  /\ (forall ks re d g st bs, envrel sh re d -> kok ks (map fst d) ->
     brel (kfold ks bs) (rinterp_binds k R re g st (cxbs sh ks bs)) (xinterp_binds k P d g st bs))
  /\ (forall ks re d u th sm st, envrel sh re d -> kok ks (map fst d) ->
     arel (rinterp_apply k R re (mklk ks (IFun u)) th (mkplan ks sm) st) (xinterp_apply k P d u th sm st))
  /\ (forall ks re d g st u brs, envrel sh re d -> kok ks (map fst d) ->
     rrel brs (rinterp_branch k R re g st (mklk ks (IFun u)) (cbrs sh ks brs))
              (xinterp_branch k P d g st u brs)).

(* a lookup in related environments *)
Ltac lk H Hk i :=
  rewrite (@rlook_mklk sh _ i _ _ H Hk);
  let Hl := fresh "Hl" in
  pose proof (@rlookup_rel sh i _ _ H) as Hl; unfold orel in Hl;
  destruct (rlookup i _), (lookup_xdenv i _); try contradiction;
  try (inversion Hl; subst; clear Hl).

(* a composition in related environments *)
Ltac cp H Hk sm :=
  rewrite (@rcompose_mkplan sh _ sm _ _ H Hk);
  let Hc := fresh "Hc" in let Ex := fresh "Ex" in
  pose proof (@rcompose_dyn_rel sh sm _ _ H) as Hc; unfold crel in Hc;
  destruct (rcompose_dyn _ sm), (xcompose _ sm) eqn:Ex; try contradiction.

Ltac out IHe H Hk x :=
  rewrite (IHe _ _ _ _ _ x H Hk); destruct (xinterp _ _ _ _ _ x) as [[?| | | | | | |] ? ?]; auto.

Lemma body_kok : forall b f sm dc d',
  xcompose dc sm = Some d' -> (b = shape_ok sh f sm) ->
  kok (if b then sh f else None) (map fst d').
Proof.
  intros b f sm dc d' Ex ->. destruct (shape_ok sh f sm) eqn:E; simpl; auto.
  rewrite (shape_ok_true _ _ _ E). simpl. symmetry. eapply xcompose_keys; eauto.
Qed.

Lemma clo_kok : forall b f (d0 : xdenv), (b = true -> sh f = Some (map fst d0)) ->
  kok (if b then sh f else None) (map fst d0).
Proof. intros [] f d0 H; simpl; auto. rewrite H by reflexivity. reflexivity. Qed.

Lemma clo_flag : forall f sm dc d', xcompose dc sm = Some d' ->
  shape_ok sh f sm = true -> sh f = Some (map fst d').
Proof.
  intros f sm dc d' Ex E. rewrite (shape_ok_true _ _ _ E). f_equal. symmetry. eapply xcompose_keys; eauto.
Qed.

Lemma kbr_app : forall ks brs (d dr' : xdenv), kok ks (map fst d) ->
  (forall B, brkeys brs = Some B -> map fst dr' = B) -> kok (kbr ks brs) (map fst (dr' ++ d)).
Proof.
  intros [K|] brs d dr' Hk HB; unfold kbr; simpl; auto.
  destruct (brkeys brs) as [B|]; simpl; auto. rewrite map_app, (HB B eq_refl). simpl in Hk. congruence.
Qed.

Lemma if_cx : forall (b : bool) X Y e,
  (if b then cx sh X e else cx sh Y e) = cx sh (if b then X else Y) e.
Proof. intros []; reflexivity. Qed.

Lemma agree_step : forall k, AGREE k -> AGREE (S k).
Proof.
  intros k [IHe [IHl [IHb [IHa IHr]]]].
  split; [|split; [|split; [|split]]].
  - intros ks re d g st e H Hk.
    destruct e as [x| |v|n th e1|f th sm l|f l|e0 m prov l|e0 arms|x e1 e2|t ty e1|v e1 e2
                  |f f' sm e1|t m th f' sm e1|u h sm smc e1|u u' sm e1|u th sm e1|e1 e2|x e1
                  |c e1 e2|c b|b|e1| |fs|e1 f|e0 arms|u h e1|u u' e1|u brs e1|i p e1].
    + (* XVar *) reflexivity.
    + reflexivity.
    + (* XVl *) cbn [cx rinterp xinterp]. lk H Hk (IVal v); auto.
    + (* XTag *) cbn [cx rinterp xinterp]. rewrite (IHe _ _ _ _ _ e1 H Hk). reflexivity.
    + (* XCallD *)
      cbn [cx rinterp xinterp]. rewrite Hfn. destruct (xp_fn P f) as [body|]; auto.
      rewrite (IHl _ _ _ _ _ l H Hk).
      destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
      cp H Hk sm; auto. f_equal. rewrite mkv_eq, if_cx.
      apply IHe; auto. eapply body_kok; eauto.
    + (* XCallA *)
      cbn [cx rinterp xinterp]. lk H Hk (IFn f); auto.
      * rewrite Hfn. match goal with |- context [xp_fn P ?x] => destruct (xp_fn P x) as [body|] end; auto.
        rewrite (IHl _ _ _ _ _ l H Hk).
        destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
        f_equal. rewrite mkv_eq, if_cx. apply IHe; auto. apply clo_kok; auto.
      * rewrite (IHl _ _ _ _ _ l H Hk). reflexivity.
    + (* XMeth *)
      cbn [cx rinterp xinterp]. lk H Hk prov; auto.
      * rewrite Hfn. match goal with |- context [xp_fn P ?x] => destruct (xp_fn P x) as [body|] end; auto.
        out IHe H Hk e0.
        rewrite (IHl _ _ _ _ _ l H Hk).
        match goal with |- context [xinterp_list k P d ?g0 ?st0 l] =>
          destruct (xinterp_list k P d g0 st0 l) as [[[ws g1] st1]|] end; auto.
        f_equal. rewrite mkv_eq, if_cx. apply IHe; auto. apply clo_kok; auto.
      * out IHe H Hk e0. rewrite (IHl _ _ _ _ _ l H Hk). reflexivity.
    + (* XMatch *)
      cbn [cx rinterp xinterp]. out IHe H Hk e0.
      match goal with |- match ?w with _ => _ end = _ => destruct w end; auto.
      rewrite rfind_arm_cx.
      match goal with |- context [find_xarm ?n arms] => destruct (find_xarm n arms) as [[x' eb]|] end; auto.
      rewrite (IHe _ _ _ _ _ eb H Hk). reflexivity.
    + (* XLet *)
      cbn [cx rinterp xinterp]. out IHe H Hk e1. rewrite (IHe _ _ _ _ _ e2 H Hk). reflexivity.
    + (* XBindTy *) cbn [cx rinterp xinterp]. apply IHe; auto.
    + (* XBindVal *)
      cbn [cx rinterp xinterp]. out IHe H Hk e1.
      apply IHe; [constructor; auto; constructor | apply kok_push; auto].
    + (* XBindFn *)
      cbn [cx rinterp xinterp]. cp H Hk sm; auto.
      apply IHe; [|apply kok_push; auto]. constructor; auto. constructor; auto.
      intro E. eapply clo_flag; eauto.
    + (* XBindMth *)
      cbn [cx rinterp xinterp]. cp H Hk sm; auto.
      apply IHe; [|apply kok_push; auto]. constructor; auto. constructor; auto.
      intro E. eapply clo_flag; eauto.
    + (* XBindFun *)
      cbn [cx rinterp xinterp]. cp H Hk smc; auto.
      apply IHe; [|apply kok_push; auto]. constructor; auto. constructor; auto.
    + (* XBindFunA *)
      cbn [cx rinterp xinterp]. lk H Hk (IFun u'); auto.
      apply IHe; [|apply kok_push; auto]. constructor; auto. constructor; auto.
    + (* XApply *)
      cbn [cx rinterp xinterp].
      pose proof (IHa ks re d u th sm st H Hk) as Ha.
      destruct (rinterp_apply k R re (mklk ks (IFun u)) th (mkplan ks sm) st) as [[dr st1]|r1],
               (xinterp_apply k P d u th sm st) as [[dr' st1']|r1']; simpl in Ha; try contradiction.
      * destruct Ha as [Hdr ->]. apply IHe; simpl; auto. apply envrel_app; auto.
      * subst; reflexivity.
    + (* XSeq *)
      cbn [cx rinterp xinterp]. out IHe H Hk e1.
    + (* XAssign *)
      cbn [cx rinterp xinterp]. out IHe H Hk e1.
    + (* XIf *)
      cbn [cx rinterp xinterp]. out IHe H Hk c.
      match goal with |- context [truthy P ?w] => destruct (truthy P w) as [[]|] end; auto.
    + (* XWhile *)
      cbn [cx rinterp xinterp]. out IHe H Hk c.
      match goal with |- context [truthy P ?w] => destruct (truthy P w) as [[]|] end; auto.
      out IHe H Hk b.
      * change (RWhile (cx sh ks c) (cx sh ks b)) with (cx sh ks (XWhile c b)). apply IHe; auto.
    + (* XLoop *)
      cbn [cx rinterp xinterp]. out IHe H Hk b.
      change (RLoop (cx sh ks b)) with (cx sh ks (XLoop b)). apply IHe; auto.
    + (* XReturn *) cbn [cx rinterp xinterp]. out IHe H Hk e1.
    + (* XBreak *) reflexivity.
    + (* XRec *)
      cbn [cx rinterp xinterp]. rewrite rec_snd, rec_fst, (IHl _ _ _ _ _ _ H Hk). reflexivity.
    + (* XField *) cbn [cx rinterp xinterp]. out IHe H Hk e1.
    + (* XMatchP *)
      cbn [cx rinterp xinterp]. out IHe H Hk e0.
      rewrite rselect_arm_cx.
      match goal with |- context [select_arm arms ?w] => destruct (select_arm arms w) as [[bs eb]|] end; auto.
      rewrite (IHe _ _ _ _ _ eb H Hk). reflexivity.
    + (* XBindFunSym *)
      cbn [cx rinterp xinterp]. apply IHe; [|apply kok_push; auto]. constructor; auto. constructor.
    + (* XBindFunCopy *)
      cbn [cx rinterp xinterp]. rewrite (@rlook_mklk sh _ (IFun u') _ _ H Hk).
      pose proof (@rlookup_rel sh (IFun u') _ _ H) as Hl; unfold orel in Hl.
      destruct (rlookup (IFun u') re), (lookup_xdenv (IFun u') d); try contradiction; auto.
      apply IHe; [|apply kok_push; auto]. constructor; auto.
    + (* XApplyBr *)
      rewrite cx_applybr. cbn [rinterp xinterp].
      pose proof (IHr ks re d g st u brs H Hk) as Hr.
      destruct (rinterp_branch k R re g st (mklk ks (IFun u)) (cbrs sh ks brs)) as [[[dr g1] st1]|r1],
               (xinterp_branch k P d g st u brs) as [[[dr' g1'] st1']|r1']; simpl in Hr; try contradiction.
      * destruct Hr as [Hdr [-> [-> HB]]]. apply IHe.
        -- apply envrel_app; auto.
        -- apply kbr_app; auto.
      * subst; reflexivity.
    + (* XBindPrim *)
      cbn [cx rinterp xinterp]. apply IHe; [|apply kok_push; auto]. constructor; auto. constructor.
  - intros ks re d g st l H Hk. destruct l as [|e r]; cbn [map rinterp_list xinterp_list]; auto.
    out IHe H Hk e. rewrite (IHl _ _ _ _ _ r H Hk). reflexivity.
  - intros ks re d g st bs H Hk. destruct bs as [|b r]; cbn [cxbs rinterp_binds xinterp_binds].
    { simpl. auto. }
    unfold kfold; cbn [fold_left]; fold (kfold (kafter ks b) r).
    destruct b as [t ty|v e1|f f' sm|t m th f' sm|u h sm smc|u u' sm|u th sm|u h|u u'|u brs|i p].
    + apply IHb; auto.
    + cbn [cxb rinterp_binds]. rewrite (IHe _ _ _ _ _ e1 H Hk).
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl; auto.
      apply IHb; [constructor; auto; constructor | apply kok_push; auto].
    + cbn [cxb rinterp_binds]. cp H Hk sm; simpl; auto.
      apply IHb; [|apply kok_push; auto]. constructor; auto. constructor; auto.
      intro E. eapply clo_flag; eauto.
    + cbn [cxb rinterp_binds]. cp H Hk sm; simpl; auto.
      apply IHb; [|apply kok_push; auto]. constructor; auto. constructor; auto.
      intro E. eapply clo_flag; eauto.
    + cbn [cxb rinterp_binds]. cp H Hk smc; simpl; auto.
      apply IHb; [|apply kok_push; auto]. constructor; auto. constructor; auto.
    + cbn [cxb rinterp_binds]. lk H Hk (IFun u'); simpl; auto.
      apply IHb; [|apply kok_push; auto]. constructor; auto. constructor; auto.
    + cbn [cxb rinterp_binds].
      pose proof (IHa ks re d u th sm st H Hk) as Ha.
      destruct (rinterp_apply k R re (mklk ks (IFun u)) th (mkplan ks sm) st) as [[dr st1]|r1],
               (xinterp_apply k P d u th sm st) as [[dr' st1']|r1']; simpl in Ha; try contradiction.
      * destruct Ha as [Hdr ->]. apply IHb; simpl; auto. apply envrel_app; auto.
      * subst; reflexivity.
    + cbn [cxb rinterp_binds]. apply IHb; [|apply kok_push; auto]. constructor; auto. constructor.
    + cbn [cxb rinterp_binds]. rewrite (@rlook_mklk sh _ (IFun u') _ _ H Hk).
      pose proof (@rlookup_rel sh (IFun u') _ _ H) as Hl; unfold orel in Hl.
      destruct (rlookup (IFun u') re), (lookup_xdenv (IFun u') d); try contradiction; simpl; auto.
      apply IHb; [|apply kok_push; auto]. constructor; auto.
    + rewrite cxb_applybr. cbn [rinterp_binds].
      pose proof (IHr ks re d g st u brs H Hk) as Hr.
      destruct (rinterp_branch k R re g st (mklk ks (IFun u)) (cbrs sh ks brs)) as [[[dr g1] st1]|r1],
               (xinterp_branch k P d g st u brs) as [[[dr' g1'] st1']|r1']; simpl in Hr; try contradiction.
      * destruct Hr as [Hdr [-> [-> HB]]]. apply IHb.
        -- apply envrel_app; auto.
        -- apply kbr_app; auto.
      * subst; reflexivity.
    + cbn [cxb rinterp_binds]. apply IHb; [|apply kok_push; auto]. constructor; auto. constructor.
  - intros ks re d u th sm st H Hk. cbn [rinterp_apply xinterp_apply].
    lk H Hk (IFun u); simpl; auto.
    rewrite Hfun. destruct (xp_fun P h) as [[Dr bs]|]; simpl; auto.
    cp H Hk sm; simpl; auto.
    match goal with
    | Hc : envrel sh ?a ?b |- context [rchain ?a ?sms] =>
        pose proof (@rchain_rel sh sms _ _ Hc) as Hch; unfold crel in Hch;
        destruct (rchain a sms) as [l1|], (xrun_chain b sms) as [l2|];
        try contradiction; simpl; auto
    end.
    match goal with
    | Hd : envrel sh ?r0 ?d0 |- context [rinterp_binds k R (l1 ++ ?r0) [] st ?cbs] =>
        pose proof (IHb None (l1 ++ r0) (l2 ++ d0) [] st bs (envrel_app Hch Hd) I) as Hb;
        destruct (rinterp_binds k R (l1 ++ r0) [] st cbs) as [[[d3 g3] st3]|r3],
                 (xinterp_binds k P (l2 ++ d0) [] st bs) as [[[d3' g3'] st3']|r3'];
        simpl in Hb; try contradiction; auto
    end.
    destruct Hb as [Hd3 [_ [-> ->]]].
    pose proof (@rcompose_dyn_rel sh (inst_smap th Dr) _ _ Hd3) as Hc3; unfold crel in Hc3.
    destruct (rcompose_dyn d3 (inst_smap th Dr)), (xcompose d3' (inst_smap th Dr)); try contradiction; simpl; auto.
  - intros ks re d g st u brs H Hk. cbn [rinterp_branch xinterp_branch].
    lk H Hk (IFun u); simpl; auto.
    rewrite rfind_branch_cbrs.
    destruct (find_branch h brs) as [[bs pairs]|] eqn:Ef; simpl; auto.
    pose proof (IHb ks re d g st bs H Hk) as Hb.
    destruct (rinterp_binds k R re g st (cxbs sh ks bs)) as [[[d3 g3] st3]|r3],
             (xinterp_binds k P d g st bs) as [[[d3' g3'] st3']|r3']; simpl in Hb; try contradiction; auto.
    destruct Hb as [Hd3 [Hk3 [-> ->]]].
    cp Hd3 Hk3 pairs; simpl; auto.
    repeat split; auto. intros B HB.
    rewrite (xcompose_keys _ _ Ex). eapply find_branch_keys; eauto.
Qed.

Theorem rinterp_agrees : forall fuel ks re d g st e,
  envrel sh re d -> kok ks (map fst d) ->
  rinterp fuel R re g st (cx sh ks e) = xinterp fuel P d g st e.
Proof.
  intros fuel. assert (H : AGREE fuel).
  { induction fuel as [|k IH].
    - repeat split; intros; simpl; auto.
    - apply agree_step, IH. }
  apply H.
Qed.

End Agree.

(* ===================== 7. Running main =================================== *)

(* main's initial environment, with every closure flagged `false` (its
   dynamic body), which needs nothing of its keys *)
Fixpoint conv_entry (en : xdentry) : rentry :=
  match en with
  | XDVal w => QVal w
  | XDClo f d =>
      QClo false f ((fix go (l : list (item * xdentry)) : renv :=
                       match l with [] => [] | (i, e) :: r => (i, conv_entry e) :: go r end) d)
  | XDFun h sms d =>
      QFun h sms ((fix go (l : list (item * xdentry)) : renv :=
                     match l with [] => [] | (i, e) :: r => (i, conv_entry e) :: go r end) d)
  | XDPrim p => QPrim p
  | XDFunSym h => QFunSym h
  end.

Fixpoint conv_env (l : list (item * xdentry)) : renv :=
  match l with [] => [] | (i, e) :: r => (i, conv_entry e) :: conv_env r end.

Lemma conv_rel : forall sh en, erel sh (conv_entry en) en.
Proof.
  intro sh. fix IH 1. intros [w|f d|h sms d|p|h]; simpl; constructor.
  all: try (intro; discriminate).
  all: induction d as [|[i e] r IHd]; constructor; [apply IH|exact IHd].
Qed.

Lemma conv_env_rel : forall sh d, envrel sh (conv_env d) d.
Proof.
  intros sh d. induction d as [|[i e] r IH]; simpl; constructor; auto. apply conv_rel.
Qed.

Definition rrun_main (fuel : nat) (R : rprog) (main : fsym) (d0 : xdenv) (st0 : mstate) : xres :=
  match rp_fn R main with
  | Some (_, bd) => ret_call [] (rinterp fuel R (conv_env d0) [] st0 bd)
  | None => XR (XErr e_nobody) [] st0
  end.

(* R is the compilation of P under the guess sh *)
Definition rp_ok (sh : fsym -> option (list item)) (R : rprog) (P : xprog) : Prop :=
  rp_x R = P
  /\ (forall f, rp_fn R f =
        match xp_fn P f with Some b => Some (cx sh (sh f) b, cx sh None b) | None => None end)
  /\ (forall h, rp_fun R h =
        match xp_fun P h with Some (Dr, bs) => Some (Dr, cxbs sh None bs) | None => None end).

Theorem rrun_main_eq : forall sh R P fuel main d0 st0, rp_ok sh R P ->
  rrun_main fuel R main d0 st0 = run_main fuel P main d0 st0.
Proof.
  intros sh R P fuel main d0 st0 [Hx [Hfn Hfun]]. subst P.
  unfold rrun_main, run_main. rewrite Hfn. destruct (xp_fn (rp_x R) main) as [body|]; auto.
  rewrite (@rinterp_agrees sh R Hfn Hfun fuel None (conv_env d0) d0 [] st0 body (conv_env_rel sh d0) I). reflexivity.
Qed.

(* ===================== 8. The compiled program =========================== *)

(* The guess: for each fn, the keys of the first satisfaction map it is
   called or closed with (untrusted: any answer is sound). *)
Definition note (m : tmap (list item)) (f : fsym) (sm : smap) : tmap (list item) :=
  match tget m f with Some _ => m | None => tset m f (map fst sm) end.

Fixpoint sh_e (m : tmap (list item)) (e : xexpr) {struct e} : tmap (list item) :=
  match e with
  | XVar _ | XUnit | XVl _ | XBreak => m
  | XTag _ _ e1 | XBindTy _ _ e1 | XLoop e1 | XReturn e1 | XAssign _ e1 | XField e1 _
  | XBindFunSym _ _ e1 | XBindFunCopy _ _ e1 | XBindPrim _ _ e1 | XBindFun _ _ _ _ e1
  | XBindFunA _ _ _ e1 | XApply _ _ _ e1 => sh_e m e1
  | XCallD f _ sm args => fold_left sh_e args (note m f sm)
  | XCallA _ args => fold_left sh_e args m
  | XMeth e0 _ _ args => fold_left sh_e args (sh_e m e0)
  | XMatch e0 arms => fold_left (fun m a => match a with (_, _, eb) => sh_e m eb end) arms (sh_e m e0)
  | XLet _ e1 e2 | XBindVal _ e1 e2 | XSeq e1 e2 | XWhile e1 e2 => sh_e (sh_e m e1) e2
  | XIf c e1 e2 => sh_e (sh_e (sh_e m c) e1) e2
  | XBindFn _ f' sm e1 | XBindMth _ _ _ f' sm e1 => sh_e (note m f' sm) e1
  | XRec fs => fold_left (fun m p => match p with (_, e1) => sh_e m e1 end) fs m
  | XMatchP e0 arms => fold_left (fun m a => match a with (_, eb) => sh_e m eb end) arms (sh_e m e0)
  | XApplyBr _ brs e1 =>
      sh_e (fold_left (fun m br => match br with (_, bs, _) => fold_left sh_b bs m end) brs m) e1
  end
with sh_b (m : tmap (list item)) (b : xbind) {struct b} : tmap (list item) :=
  match b with
  | XBVal _ e1 => sh_e m e1
  | XBFn _ f' sm | XBMth _ _ _ f' sm => note m f' sm
  | XBApplyBr _ brs => fold_left (fun m br => match br with (_, bs, _) => fold_left sh_b bs m end) brs m
  | _ => m
  end.

Definition shapes (p : xprogram) : tmap (list item) :=
  fold_left (fun m ub => fold_left sh_b (snd (snd ub)) m) (xo_funs p)
    (fold_left (fun m fb => sh_e m (snd fb)) (xo_fns p) TE).

Definition compile_fns (sh : fsym -> option (list item)) (p : xprogram) : list (nat * (rexpr * rexpr)) :=
  map (fun fb => (fst fb, (cx sh (sh (fst fb)) (snd fb), cx sh None (snd fb)))) (xo_fns p).

Definition compile_funs (sh : fsym -> option (list item)) (p : xprogram) : list (nat * (tele * list rbind)) :=
  map (fun ub => (fst ub, (fst (snd ub), cxbs sh None (snd (snd ub))))) (xo_funs p).

(* What the driver builds (with arrays in place of nlook, as it does for
   program_of's xp_fn): *)
Definition rprog_of (p : xprogram) (argv : list (list int)) (rd : list int -> option (list int)) : rprog :=
  let m := shapes p in
  let sh := fun f => tget m f in
  mkRProg (program_of p argv rd)
          (fun f => Elaboration.nlook f (compile_fns sh p))
          (fun h => Elaboration.nlook h (compile_funs sh p)).

Lemma nlook_map_k : forall A B (F : nat -> A -> B) k (l : list (nat * A)),
  Elaboration.nlook k (map (fun p => (fst p, F (fst p) (snd p))) l) = option_map (F k) (Elaboration.nlook k l).
Proof.
  induction l as [|[k' a] r IH]; simpl; auto.
  destruct (Nat.eqb k k') eqn:E; simpl; auto. apply Nat.eqb_eq in E. subst. reflexivity.
Qed.

Theorem slot_run_main : forall p argv rd fuel st0,
  rrun_main fuel (rprog_of p argv rd) (xo_main p) (xo_env p) st0
  = run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0.
Proof.
  intros. apply (rrun_main_eq (sh := fun f => tget (shapes p) f)).
  split; [reflexivity|split].
  - intro f. unfold rprog_of, compile_fns. simpl.
    rewrite (nlook_map_k (fun f b => (cx (fun f0 => tget (shapes p) f0) (tget (shapes p) f) b,
                                       cx (fun f0 => tget (shapes p) f0) None b))).
    destruct (Elaboration.nlook f (xo_fns p)); reflexivity.
  - intro h. unfold rprog_of, compile_funs. simpl.
    rewrite (nlook_map_k (fun _ (q : tele * list xbind) =>
               (fst q, cxbs (fun f0 => tget (shapes p) f0) None (snd q)))).
    destruct (Elaboration.nlook h (xo_funs p)) as [[Dr bs]|]; reflexivity.
Qed.

(* With pruning (Prune.oracle_program), what the driver's default engine
   computes is Audit's semantics of the elaborated program, unless the
   reference run gets stuck (Prune.oracle_agrees). *)
Theorem slot_oracle_agrees : forall p argv rd fuel st0,
  let P := program_of p argv rd in
  stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ rrun_main fuel (rprog_of (oracle_program p) argv rd) (xo_main p) (xo_env p) st0
     = run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 P.
  assert (Hm : xo_main (oracle_program p) = xo_main p /\ xo_env (oracle_program p) = xo_env p).
  { unfold oracle_program. destruct (compute_R 64 p TE); split; reflexivity. }
  destruct Hm as [Hm He].
  pose proof (slot_run_main (oracle_program p) argv rd fuel st0) as Hs. rewrite Hm, He in Hs.
  rewrite Hs. destruct (oracle_agrees p argv rd fuel st0) as [H|H]; [left; exact H|right].
  rewrite frun_main_eq in H. exact H.
Qed.
