(* ExecutionModel.v — the current bootstrap IR and its checked component model.
   Audit.v owns the source syntax and the public language semantics. This file
   retains the executable representation used by the existing proof development;
   its source relation is a component specification, not the public Builds.
   No equivalence with the revised public semantics is assumed. *)
From Stdlib Require Import PrimInt63 Uint63 PrimFloat ZArith.
From Stdlib Require Import String Ascii List Bool Arith.
From NM Require Export Audit.
Import ListNotations.
Module Input := Audit.Input.
Module Syntax := Audit.Syntax.
Module Loader := Audit.Loader.

(* ######################################################################## *)
(* Part II. Elaborated programs                                              *)
(* ######################################################################## *)

Set Implicit Arguments.

(* Shared symbols, types and provision keys. *)


(* ===================== Symbols =========================================== *)

Definition tsym := nat.  (* abstract type symbols *)
Definition nsym := nat.  (* nominal tag names *)
Definition vsym := nat.  (* val symbols *)
Definition fsym := nat.  (* fn symbols *)
Definition msym := nat.  (* method symbols *)
Definition var  := nat.  (* term variables *)
Definition usym := nat.  (* functor symbols [D55]/[D78] *)

(* ===================== Types and substitutions =========================== *)

Inductive ty : Type :=
| TUnit  : ty
| TAbs   : tsym -> ty                       (* abstract type symbol (incl. This) *)
| TNom   : nsym -> list (tsym * ty) -> ty   (* total applied nominal reference *)
| TUnion : list ty -> ty.                   (* members must be nominal refs *)

Definition subst := list (tsym * ty).

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

(* ===================== Items and telescopes ============================== *)

(* A method item carries the *receiver type* it is provided at: the paper's
   §1 key, the pair (receiver type, method symbol), plus the application θ
   that interprets the method's own requirements (deviation 6). *)
Inductive item : Type :=
| ITy  : tsym -> item
| IVal : vsym -> item
| IFn  : fsym -> item
| IMth : ty -> msym -> subst -> item
| IFun : usym -> item.   (* an abstract functor [D78]: at no instantiation *)

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
  | IFun u1, IFun u2 => Nat.eqb u1 u2
  | _, _ => false
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
  | IFun u => IFun u
  end.

Definition is_ty_item (i : item) : bool :=
  match i with ITy _ => true | _ => false end.

(* Satisfaction maps: for each requirement of a callee/provider, the in-force
   item that satisfies it (recorded by elaboration, used by the dynamics). *)
Definition smap := list (item * item).

(* MossX: the elaborator's output and the interpreter's language. *)

Local Open Scope nat_scope.

(* ===================== Values ============================================ *)

Definition fld := nat.  (* record field names, interned *)

Inductive xvalue : Type :=
| XVUnit : xvalue
| XVTag  : nsym -> xvalue -> xvalue
| XVI32  : int -> xvalue                (* invariant kept by every op: < 2^32 *)
| XVF64  : float -> xvalue
| XVRec  : list (fld * xvalue) -> xvalue
| XVF32  : float -> xvalue              (* a binary64 holding a binary32 value *)
| XVI64  : Z -> xvalue                  (* invariant kept by every op: [0, 2^64) *)
| XVRef  : int -> xvalue                (* legacy primitive heap address *)
| XVRefAt : bool -> ty -> int -> xvalue. (* allocation kind and ground held type *)

(* ===================== Primitives ======================================== *)

Inductive i32bin := IAdd | ISub | IMul | IDivS | IDivU | IRemS | IRemU
                  | IAnd | IOr | IXor | IShl | IShrS | IShrU | IRotl | IRotr.
Inductive i32cmp := CEq | CNe | CLtS | CLtU | CGtS | CGtU | CLeS | CLeU
                  | CGeS | CGeU.
Inductive i32un  := UEqz | UClz | UCtz | UPopcnt | UExt8 | UExt16.
Inductive f64bin := FAdd | FSub | FMul | FDiv | FMin | FMax | FCopysign.
Inductive f64un  := FAbs | FNeg | FCeil | FFloor | FTrunc | FNearest | FSqrt.
Inductive f64cmp := FEq | FNe | FLt | FGt | FLe | FGe.
Inductive conv   := CTruncS | CTruncU | CTruncSatS | CTruncSatU
                  | CConvS | CConvU.     (* i32 <-> f64 *)
Inductive loadk  := L32 | L8S | L8U | L16S | L16U.
Inductive storek := S32 | S8 | S16.
Inductive i64un  := U64Eqz | U64Clz | U64Ctz | U64Popcnt | U64Ext8 | U64Ext16 | U64Ext32.
Inductive load64k := L64 | L64_8S | L64_8U | L64_16S | L64_16U | L64_32S | L64_32U.
Inductive store64k := S64 | S64_8 | S64_16 | S64_32.
(* i64 <-> floats: trunc (s/u, sat or not) from f64/f32, convert to f64/f32 *)
Inductive conv64 := C64Trunc (from32 : bool) (signed : bool) (sat : bool)
                  | C64Conv (to32 : bool) (signed : bool).

Inductive prim : Type :=
| PI32Bin  : i32bin -> prim
| PI32Cmp  : i32cmp -> prim
| PI32Un   : i32un -> prim
| PF64Bin  : f64bin -> prim
| PF64Cmp  : f64cmp -> prim
| PF64Un   : f64un -> prim
| PConv    : conv -> prim
| PLoad    : loadk -> prim
| PStore   : storek -> prim
| PF64Load | PF64Store
| PMemSize | PMemGrow | PMemCopy | PMemFill
| PUnreachable
| PFdWrite | PProcExit | PArgsSizesGet | PArgsGet | PPathOpen | PFdRead | PFdClose
| PI32Bool
(* f32: every result rounded to binary32 *)
| PF32Bin  : f64bin -> prim
| PF32Cmp  : f64cmp -> prim
| PF32Un   : f64un -> prim
| PConv32  : conv -> prim               (* i32 <-> f32 *)
| PF32Load | PF32Store | PDemote | PPromote | PI32ReinterpF32 | PF32ReinterpI32
(* i64 *)
| PI64Bin  : i32bin -> prim
| PI64Cmp  : i32cmp -> prim
| PI64Un   : i64un -> prim
| PWrap | PExtendS | PExtendU
| PI64Load : load64k -> prim
| PI64Store : store64k -> prim
| PConv64  : conv64 -> prim
| PI64ReinterpF64 | PF64ReinterpI64
(* D75/D81: what `bind Heap;`, `bind Mutable;`, `bind Arrays;` provide;
   PBox and PArrNew carry the `Ref` and `Array` tags *)
| PBox : nsym -> prim | PDeref | PRefSet
| PArrNew : nsym -> prim | PArrLen | PArrGet | PArrSet | PArrFill | PArrCopy
(* declared by the library but not modelled: running one stops the
   program with XUns, which the oracle reports as "unsupported" *)
| PUnsupported : nat -> prim.

(* ===================== Expressions and bindings ========================== *)

Inductive xpat : Type :=
| XPAny  : option var -> xpat                          (* _ or a binder *)
| XPHead : nsym -> option var -> list (fld * var) -> xpat.

(* Expressions include calls, provision binding, records and control flow.
   Satisfaction maps identify the provisions passed to a call. In XApplyBr,
   each (out, key) exports the branch's key under the name out. *)
Inductive xexpr : Type :=
| XVar      : var -> xexpr
| XUnit     : xexpr
| XVl       : vsym -> xexpr
| XTag      : nsym -> subst -> xexpr -> xexpr
| XCallD    : fsym -> subst -> smap -> list xexpr -> xexpr
| XCallA    : fsym -> list xexpr -> xexpr
| XMeth     : xexpr -> msym -> item -> list xexpr -> xexpr
| XMatch    : xexpr -> list (nsym * var * xexpr) -> xexpr
| XLet      : var -> xexpr -> xexpr -> xexpr
| XBindTy   : tsym -> ty -> xexpr -> xexpr
| XBindVal  : vsym -> xexpr -> xexpr -> xexpr
| XBindFn   : fsym -> fsym -> smap -> xexpr -> xexpr
| XBindMth  : ty -> msym -> subst -> fsym -> smap -> xexpr -> xexpr
| XBindFun  : usym -> usym -> smap -> smap -> xexpr -> xexpr
| XBindFunA : usym -> usym -> smap -> xexpr -> xexpr
| XApply    : usym -> subst -> smap -> xexpr -> xexpr
| XApplyR   : usym -> smap -> smap -> xexpr -> xexpr
(* extensions *)
| XSeq      : xexpr -> xexpr -> xexpr
| XAssign   : var -> xexpr -> xexpr
| XIf       : xexpr -> xexpr -> xexpr -> xexpr
| XWhile    : xexpr -> xexpr -> xexpr
| XLoop     : xexpr -> xexpr
| XReturn   : xexpr -> xexpr
| XBreak    : xexpr
| XRec      : list (fld * xexpr) -> xexpr
| XField    : xexpr -> fld -> xexpr
| XMatchP   : xexpr -> list (xpat * xexpr) -> xexpr
| XBindFunSym  : usym -> usym -> xexpr -> xexpr   (* bind F = H (by name) *)
| XBindFunCopy : usym -> usym -> xexpr -> xexpr   (* bind F = G (abstract) *)
| XApplyBr  : usym -> list (usym * list xbind * smap) -> xexpr -> xexpr
| XBindPrim : item -> prim -> xexpr -> xexpr
| XConst : vsym -> xexpr -> xexpr
with xbind : Type :=
| XBTy    : tsym -> ty -> xbind
| XBVal   : vsym -> xexpr -> xbind
| XBFn    : fsym -> fsym -> smap -> xbind
| XBMth   : ty -> msym -> subst -> fsym -> smap -> xbind
| XBFun   : usym -> usym -> smap -> smap -> xbind
| XBFunA  : usym -> usym -> smap -> xbind
| XBApply : usym -> subst -> smap -> xbind
| XBApplyR : usym -> smap -> smap -> xbind
| XBFunSym  : usym -> usym -> xbind
| XBFunCopy : usym -> usym -> xbind
| XBApplyBr : usym -> list (usym * list xbind * smap) -> xbind
| XBPrim  : item -> prim -> xbind.

(* ===================== Runtime structures ================================ *)

Inductive xdentry : Type :=
| XDVal    : xvalue -> xdentry
| XDClo    : fsym -> list (item * xdentry) -> xdentry
| XDFun    : usym -> list smap -> list (item * xdentry) -> xdentry
| XDPrim   : prim -> xdentry
| XDFunSym : usym -> xdentry
| XDType : ty -> xdentry
| XDPrimAt : prim -> ty -> xdentry.

Definition xdenv := list (item * xdentry).
Definition xvenv := list (var * xvalue).

(* Ground types are retained at runtime because native GC casts compare the
   allocation's generated Wasm type. All GC types are distinct projections of
   the emitter's single recursive group, keyed by the ground held type. *)
Fixpoint xdtypes (d : xdenv) : subst :=
  match d with
  | [] => []
  | (ITy t, XDType tau) :: r => (t, tau) :: xdtypes r
  | _ :: r => xdtypes r
  end.
Definition xground (d : xdenv) (tau : ty) : ty := app_subst (xdtypes d) tau.
Fixpoint xtype_env (d : xdenv) : xdenv :=
  match d with
  | [] => []
  | (ITy t, XDType tau) :: r => (ITy t, XDType tau) :: xtype_env r
  | _ :: r => xtype_env r
  end.
Definition xinst (d : xdenv) (th : subst) : xdenv :=
  match th with
  | [] => []
  | _ :: _ =>
      let types := xdtypes d in
      map (fun p => (ITy (fst p), XDType (app_subst types (snd p)))) th
  end.
Definition xclose (d dc : xdenv) : xdenv := dc ++ xtype_env d.
Definition xprim_entry (d : xdenv) (i : item) (p : prim) : xdentry :=
  match p, i with
  | PBox _, IMth tau _ _ | PArrNew _, IMth tau _ _ => XDPrimAt p (xground d tau)
  | _, _ => XDPrim p
  end.

Fixpoint lookup_xdenv (i : item) (d : xdenv) : option xdentry :=
  match d with
  | [] => None
  | (i', en) :: r => if item_eqb i i' then Some en else lookup_xdenv i r
  end.

Fixpoint lookup_xvenv (x : var) (g : xvenv) : option xvalue :=
  match g with
  | [] => None
  | (x', w) :: r => if Nat.eqb x x' then Some w else lookup_xvenv x r
  end.

Fixpoint assign_xvenv (x : var) (w : xvalue) (g : xvenv) : option xvenv :=
  match g with
  | [] => None
  | (x', w') :: r =>
      if Nat.eqb x x' then Some ((x', w) :: r)
      else match assign_xvenv x w r with
           | Some r' => Some ((x', w') :: r')
           | None => None
           end
  end.

Fixpoint xcompose (d : xdenv) (sm : smap) : option xdenv :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match lookup_xdenv s d, xcompose d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

Fixpoint xrun_chain (d : xdenv) (sms : list smap) : option xdenv :=
  match sms with
  | [] => Some d
  | sm :: r =>
      match xrun_chain d r with
      | Some d' => xcompose d' sm
      | None => None
      end
  end.

Fixpoint find_xarm (n : nsym) (arms : list (nsym * var * xexpr))
  : option (var * xexpr) :=
  match arms with
  | [] => None
  | (n', x, eb) :: r => if Nat.eqb n n' then Some (x, eb) else find_xarm n r
  end.

Definition mk_xvenv (ws : list xvalue) : xvenv := combine (seq 0 (List.length ws)) ws.

Fixpoint find_branch (h : usym) (brs : list (usym * list xbind * smap))
  : option (list xbind * smap) :=
  match brs with
  | [] => None
  | (h', bs, pairs) :: r => if Nat.eqb h h' then Some (bs, pairs) else find_branch h r
  end.

(* Runtime tables and external inputs. Function and functor bodies use
   the same MossX syntax as the elaborator's output. *)
Record xcastspec : Type := XCastSpec {
  xc_array : bool;
  xc_held : tsym;
  (* Ref and Array are ordinary wrappers when their respective raw
     handle payload is rebound to a different representation. *)
  xc_handle : option (tsym * ty)
}.
(* What the elaborator outputs (moved from Elab.v): the bodies of every
   defined fn (and of the fns elaboration synthesizes: forwarders, lambda
   providers, wrappers), the core functor bodies `bind F = G;` made, Bool's
   symbols, `main`, main's initial runtime environment (the platform's
   provisions for what main assumes), and the names of primitives the
   model does not implement (PUnsupported i is the i-th). *)
Record xprogram : Type := XProgram {
  xo_fns : list (nat * xexpr);
  xo_funs : list (nat * (tele * list xbind));
  xo_bool : nat; xo_true : nat; xo_false : nat;
  xo_main : nat;
  xo_env : list (item * xdentry);
  xo_prims : list string;
  xo_cast : list (nsym * xcastspec);
  xo_inputs : list (usym * list tsym);
  xo_exports : list (usym * smap);
  xo_immutable : bool;
  xo_fixed_inst : list (usym * subst);
  xo_fixed_keys : list (usym * list tsym)
}.

(* Execution view: symbol lookups, runtime inputs and representation data.
   program_of constructs this view from xprogram and the host inputs. *)
Record xprog : Type := mkXProg {
  xp_fn    : fsym -> option xexpr;
  xp_fun   : usym -> option (tele * list xbind);
  xp_bool  : nsym;
  xp_true  : nsym;
  xp_false : nsym;
  xp_argv  : list (list int);
  (* the host filesystem, an input like argv: a path's bytes, if readable *)
  xp_read  : list int -> option (list int);
  (* selected Ref/Array tag -> (array?, held-type parameter) *)
  xp_cast : list (nsym * xcastspec);
  xp_inputs : usym -> list tsym;
  xp_exports : usym -> smap;
  xp_immutable : bool;
  xp_fixed_inst : usym -> subst;
  (* Actual immutable fixed inputs; their meanings are xp_fixed_inst. *)
  xp_fixed_keys : usym -> list tsym
}.

Fixpoint lookup_cast (n : nsym) (cs : list (nsym * xcastspec)) : option xcastspec :=
  match cs with
  | [] => None
  | (n', c) :: r => if Nat.eqb n n' then Some c else lookup_cast n r
  end.
Definition xreify (P : xprog) : bool :=
  xp_immutable P || match xp_cast P with [] => false | _ => true end.
Definition xbind_ty (P : xprog) (d : xdenv) (t : tsym) (tau : ty) : xdenv :=
  if xreify P then (ITy t, XDType (xground d tau)) :: d else d.
Definition xcall_env (P : xprog) (d : xdenv) (th : subst) (dc : xdenv) : xdenv :=
  if xreify P then xinst d th ++ xclose d dc else dc.

(* A linked provider receives only its own formal type inputs. Extra
   abstract-functor inputs must not override lexical captured meanings. *)
Definition xapply_env (P : xprog) (d : xdenv) (h : usym) (th : subst) (dc : xdenv) : xdenv :=
  xcall_env P d
    (filter (fun p => existsb (Nat.eqb (fst p)) (xp_inputs P h)) th) dc.

(* Applying a functor exports the type meanings its result declares, beside
   its executable non-type provisions. Captured input types remain private. *)
Definition xresult_types (Dr : tele) (d3 : xdenv) : xdenv :=
  filter (fun p => existsb (item_eqb (fst p)) Dr) (xtype_env d3).
(* A result type also has a fresh name in the caller. Transfer its actual
   meaning to that generation before the caller uses it in a GC cast. *)
Definition xresult_generations (th : subst) (Dr : tele) : subst :=
  flat_map (fun i => match i with
    | ITy source => match lookup_subst source th with
      | Some (TAbs fresh) => [(fresh, TAbs source)]
      | _ => [] end
    | _ => [] end) Dr.
Definition xresult_env (P : xprog) (th : subst) (d3 : xdenv) (Dr : tele) (dr : xdenv) : xdenv :=
  if xreify P then dr ++ xinst d3 (xresult_generations th Dr) ++ xresult_types Dr d3 else dr.

(* Outcomes.  XOk/XErr/XFuel are the core's Ok/Err/OutOfFuel; XRet and XBrk
   are control transfers; XTrap is a Wasm trap (well-defined failure),
   XExit a WASI proc_exit, XUns a primitive this model does not implement.
   The nat in XErr/XTrap/XUns is a diagnostic code only. *)
Inductive xout : Type :=
| XOk   : xvalue -> xout
| XRet  : xvalue -> xout
| XBrk  : xout
| XErr  : nat -> xout
| XTrap : nat -> xout
| XExit : int -> xout
| XUns  : nat -> xout
| XFuel : xout.

(* ######################################################################## *)
(* Part III. Dynamic semantics                                               *)
(* ######################################################################## *)

(* Rule A: an application at θ files each result entry under the result
   item read at θ — the key its scope refers to it by. *)
Definition inst_smap (th : subst) (D : tele) : smap :=
  map (fun i => (app_subst_item th i, i)) (filter (fun i => negb (is_ty_item i)) D).

(* Reference execution is parameterized by the machine state and primitive
   operations. MossX.v supplies the concrete machine, including linear
   memory, numeric instructions, WASI and the GC heap. The primitive
   signatures and Heap.HEAP_LAWS state the laws used by the safety proofs. *)
Module Type PRIM_SEMANTICS.
  Parameter mstate : Type.
  Parameter run_prim : xprog -> prim -> list xvalue -> mstate -> xout * mstate.
End PRIM_SEMANTICS.

Module XSem (Import Prims : PRIM_SEMANTICS).
Record xres : Type := XR { xr_out : xout; xr_g : xvenv; xr_st : mstate }.

(* ===================== Reference execution =============================== *)

(* Diagnostic codes for XErr (stuck states) *)
Definition e_var := 1.      Definition e_val := 2.      Definition e_compose := 3.
Definition e_nobody := 4.   Definition e_break := 5.    Definition e_entry := 6.
Definition e_match := 7.    Definition e_chain := 8.    Definition e_assign := 9.
Definition e_bool := 10.    Definition e_field := 11.   Definition e_branch := 12.
Definition e_fun := 13.     Definition e_pat := 14.

Definition ret_call (g : xvenv) (r : xres) : xres :=
  match r with
  | XR o _ st => XR (match o with XRet w => XOk w | XBrk => XErr e_break | o => o end) g st
  end.

Definition pop1 (r : xres) : xres :=
  match r with XR o g st => XR o (tl g) st end.

Definition popn (n : nat) (r : xres) : xres :=
  match r with XR o g st => XR o (skipn n g) st end.

Definition with_g (g : xvenv) (r : xres) : xres :=
  match r with XR o _ st => XR o g st end.

(* A checked source wrapper is the native ref.cast_null operation. The
   metadata is the allocation's runtime type header, carried by an opaque
   handle; extracting/repacking a handle never changes that header. *)
Definition xcast (P : xprog) (d : xdenv) (n : nsym) (th : subst) (w : xvalue) : xout :=
  match lookup_cast n (xp_cast P) with
  | None => XOk (XVTag n w)
  | Some spec =>
      let enabled := match xc_handle spec with
                     | None => true
                     | Some (h, expected) => ty_eqb (xground d (match lookup_subst h th with Some t => t | None => TAbs h end)) expected
                     end in
      if enabled then
        match lookup_subst (xc_held spec) th, w with
        | Some tau, XVRefAt k' tau' _ =>
            if Bool.eqb (xc_array spec) k' && ty_eqb (xground d tau) tau'
            then XOk (XVTag n w) else XTrap 37
        | _, _ => XTrap 37
        end
      else XOk (XVTag n w)
  end.
Definition xgc_receiver (w : xvalue) : xvalue :=
  match w with
  | XVTag n (XVRefAt _ _ a) => XVTag n (XVRef a)
  | _ => w
  end.
Definition xgc_args (p : prim) (ws : list xvalue) : list xvalue :=
  match p, ws with
  | PArrCopy, [r; i; s; j; count] => [xgc_receiver r; i; xgc_receiver s; j; count]
  | PDeref, r :: args | PRefSet, r :: args | PArrLen, r :: args
  | PArrGet, r :: args | PArrSet, r :: args | PArrFill, r :: args => xgc_receiver r :: args
  | _, _ => ws
  end.
Definition xallocation (p : prim) (tau : ty) (o : xout) : xout :=
  match p, o with
  | PBox _, XOk (XVTag n (XVRef a)) => XOk (XVTag n (XVRefAt false tau a))
  | PArrNew _, XOk (XVTag n (XVRef a)) => XOk (XVTag n (XVRefAt true tau a))
  | _, _ => o
  end.
Definition run_prim_at (P : xprog) (p : prim) (ot : option ty)
    (ws : list xvalue) (st : mstate) : xout * mstate :=
  let '(o, st') := run_prim P p (xgc_args p ws) st in
  (match ot with Some tau => xallocation p tau o | None => o end, st').

Definition truthy (P : xprog) (w : xvalue) : option bool :=
  match w with
  | XVTag n XVUnit => Some (Nat.eqb n (xp_true P))
  | XVTag _ (XVTag n XVUnit) => Some (Nat.eqb n (xp_true P))
  | _ => None
  end.

Fixpoint assoc_fld (f : fld) (fs : list (fld * xvalue)) : option xvalue :=
  match fs with
  | [] => None
  | (f', w) :: r => if Nat.eqb f f' then Some w else assoc_fld f r
  end.

Definition get_field (f : fld) (w : xvalue) : option xvalue :=
  match w with
  | XVRec fs => assoc_fld f fs
  | XVTag _ (XVRec fs) => assoc_fld f fs
  | _ => None
  end.

Inductive pm : Type := PMatch (bs : xvenv) | PNo | PBad.

Fixpoint bind_fields (fs : list (fld * var)) (r : list (fld * xvalue)) : option xvenv :=
  match fs with
  | [] => Some []
  | (f, x) :: rest =>
      match assoc_fld f r, bind_fields rest r with
      | Some w, Some bs => Some ((x, w) :: bs)
      | _, _ => None
      end
  end.

Definition match_pat (p : xpat) (w : xvalue) : pm :=
  match p with
  | XPAny None => PMatch []
  | XPAny (Some x) => PMatch [(x, w)]
  | XPHead n ob fs =>
      match w with
      | XVTag n' pay =>
          if Nat.eqb n n' then
            let b1 := match ob with Some x => [(x, pay)] | None => [] end in
            match fs with
            | [] => PMatch b1
            | _ => match pay with
                   | XVRec r => match bind_fields fs r with
                                | Some bs => PMatch (bs ++ b1)
                                | None => PBad
                                end
                   | _ => PBad
                   end
            end
          else PNo
      | _ => PNo
      end
  end.

Fixpoint select_arm (arms : list (xpat * xexpr)) (w : xvalue)
  : option (xvenv * xexpr) :=
  match arms with
  | [] => None
  | (p, eb) :: r =>
      match match_pat p w with
      | PMatch bs => Some (bs, eb)
      | PNo => select_arm r w
      | PBad => None
      end
  end.

Fixpoint xinterp (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (e : xexpr) {struct fuel} : xres :=
  match fuel with
  | 0 => XR XFuel g st
  | S k =>
      match e with
      | XVar x => match lookup_xvenv x g with
                  | Some w => XR (XOk w) g st
                  | None => XR (XErr e_var) g st
                  end
      | XUnit => XR (XOk XVUnit) g st
      | XVl v => match lookup_xdenv (IVal v) d with
                 | Some (XDVal w) => XR (XOk w) g st
                 | _ => XR (XErr e_val) g st
                 end
      | XTag n th e1 =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (xcast P d n th w) g1 st1
          | r => r
          end
      | XCallD f th sm args =>
          match xp_fn P f with
          | Some body =>
              match xinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  match xcompose d sm with
                  | Some d' => ret_call g1 (xinterp k P (xcall_env P d th d') (mk_xvenv ws) st1 body)
                  | None => XR (XErr e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => XR (XErr e_nobody) g st
          end
      | XCallA f args =>
          match lookup_xdenv (IFn f) d with
          | Some (XDClo f' df) =>
              match xp_fn P f' with
              | Some body =>
                  match xinterp_list k P d g st args with
                  | inl (ws, g1, st1) => ret_call g1 (xinterp k P df (mk_xvenv ws) st1 body)
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (XDPrim p) =>
              match xinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p None ws st1 in XR o g1 st2
              | inr r => r
              end
          | Some (XDPrimAt p tau) =>
              match xinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p (Some tau) ws st1 in XR o g1 st2
              | inr r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMeth e0 m prov args =>
          match lookup_xdenv prov d with
          | Some (XDClo f' d') =>
              match xp_fn P f' with
              | Some body =>
                  match xinterp k P d g st e0 with
                  | XR (XOk w0) g0 st0 =>
                      match xinterp_list k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (xinterp k P d' (mk_xvenv (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (XDPrim p) =>
              match xinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match xinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p None (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | Some (XDPrimAt p tau) =>
              match xinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match xinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p (Some tau) (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMatch e0 arms =>
          match xinterp k P d g st e0 with
          | XR (XOk (XVTag n w')) g1 st1 =>
              match find_xarm n arms with
              | Some (x, eb) => pop1 (xinterp k P d ((x, w') :: g1) st1 eb)
              | None => XR (XErr e_match) g1 st1
              end
          | XR (XOk _) g1 st1 => XR (XErr e_match) g1 st1
          | r => r
          end
      | XLet x e1 e2 =>
          match xinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => pop1 (xinterp k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | XBindTy t tau e1 => xinterp k P (xbind_ty P d t tau) g st e1
      | XBindVal v e1 e2 =>
          match xinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => xinterp k P ((IVal v, XDVal w1) :: d) g1 st1 e2
          | r => r
          end
      | XBindFn f f' sm e1 =>
          match xcompose d sm with
          | Some dc => xinterp k P ((IFn f, XDClo f' (xclose d dc)) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindMth tauk m th f' sm e1 =>
          match xcompose d sm with
          | Some dc => xinterp k P ((IMth tauk m th, XDClo f' (xclose d dc)) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFun u h sm smc e1 =>
          match xcompose d smc with
          | Some dc => xinterp k P ((IFun u, XDFun h [sm] (xclose d dc)) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFunA u u' sm e1 =>
          match lookup_xdenv (IFun u') d with
          | Some (XDFun h sms dc) =>
              xinterp k P ((IFun u, XDFun h (sms ++ [sm]) dc) :: d) g st e1
          | _ => XR (XErr e_fun) g st
          end
      | XApply u th sm e1 =>
          match xinterp_apply k P d u th sm st with
          | inl (dr, st1) => xinterp k P (dr ++ d) g st1 e1
          | inr r => with_g g r
          end
      | XApplyR u inputs outputs e1 =>
          match xinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => xinterp k P (dr ++ d) g st1 e1
          | inr rr => with_g g rr
          end
      (* extensions *)
      | XSeq e1 e2 =>
          match xinterp k P d g st e1 with
          | XR (XOk _) g1 st1 => xinterp k P d g1 st1 e2
          | r => r
          end
      | XAssign x e1 =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match assign_xvenv x w g1 with
              | Some g2 => XR (XOk XVUnit) g2 st1
              | None => XR (XErr e_assign) g1 st1
              end
          | r => r
          end
      | XIf c e1 e2 =>
          match xinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true => xinterp k P d g1 st1 e1
              | Some false => xinterp k P d g1 st1 e2
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XWhile c b =>
          match xinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true =>
                  match xinterp k P d g1 st1 b with
                  | XR (XOk _) g2 st2 => xinterp k P d g2 st2 (XWhile c b)
                  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
                  | r => r
                  end
              | Some false => XR (XOk XVUnit) g1 st1
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XLoop b =>
          match xinterp k P d g st b with
          | XR (XOk _) g2 st2 => xinterp k P d g2 st2 (XLoop b)
          | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
          | r => r
          end
      | XReturn e1 =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (XRet w) g1 st1
          | r => r
          end
      | XBreak => XR XBrk g st
      | XRec fs =>
          match xinterp_list k P d g st (map snd fs) with
          | inl (ws, g1, st1) => XR (XOk (XVRec (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | XField e1 f =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match get_field f w with
              | Some v => XR (XOk v) g1 st1
              | None => XR (XErr e_field) g1 st1
              end
          | r => r
          end
      | XMatchP e0 arms =>
          match xinterp k P d g st e0 with
          | XR (XOk w) g1 st1 =>
              match select_arm arms w with
              | Some (bs, eb) =>
                  popn (List.length bs) (xinterp k P d (bs ++ g1) st1 eb)
              | None => XR (XErr e_pat) g1 st1
              end
          | r => r
          end
      | XBindFunSym u h e1 => xinterp k P ((IFun u, XDFunSym h) :: d) g st e1
      | XBindFunCopy u u' e1 =>
          match lookup_xdenv (IFun u') d with
          | Some en => xinterp k P ((IFun u, en) :: d) g st e1
          | None => XR (XErr e_fun) g st
          end
      | XApplyBr u brs e1 =>
          match xinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => xinterp k P (dr ++ d) g1 st1 e1
          | inr r => r
          end
      | XBindPrim i p e1 => xinterp k P ((i, xprim_entry d i p) :: d) g st e1
      | XConst _ initializer => match k with
          | O => XR XFuel g st
          | S _ => xinterp k P d g st initializer end
      end
  end
with xinterp_list (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (l : list xexpr) {struct fuel} : (list xvalue * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match xinterp_list k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with xinterp_binds (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (bs : list xbind) {struct fuel} : (xdenv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | XBTy t tau :: r => xinterp_binds k P (xbind_ty P d t tau) g st r
      | XBVal v e1 :: r =>
          match xinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => xinterp_binds k P ((IVal v, XDVal w1) :: d) g1 st1 r
          | rr => inr rr
          end
      | XBFn f f' sm :: r =>
          match xcompose d sm with
          | Some dc => xinterp_binds k P ((IFn f, XDClo f' (xclose d dc)) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBMth tauk m th f' sm :: r =>
          match xcompose d sm with
          | Some dc => xinterp_binds k P ((IMth tauk m th, XDClo f' (xclose d dc)) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFun u h sm smc :: r =>
          match xcompose d smc with
          | Some dc => xinterp_binds k P ((IFun u, XDFun h [sm] (xclose d dc)) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFunA u u' sm :: r =>
          match lookup_xdenv (IFun u') d with
          | Some (XDFun h sms dc) =>
              xinterp_binds k P ((IFun u, XDFun h (sms ++ [sm]) dc) :: d) g st r
          | _ => inr (XR (XErr e_fun) g st)
          end
      | XBApply u th sm :: r =>
          match xinterp_apply k P d u th sm st with
          | inl (dr, st1) => xinterp_binds k P (dr ++ d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBApplyR u inputs outputs :: r =>
          match xinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => xinterp_binds k P (dr ++ d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBFunSym u h :: r => xinterp_binds k P ((IFun u, XDFunSym h) :: d) g st r
      | XBFunCopy u u' :: r =>
          match lookup_xdenv (IFun u') d with
          | Some en => xinterp_binds k P ((IFun u, en) :: d) g st r
          | None => inr (XR (XErr e_fun) g st)
          end
      | XBApplyBr u brs :: r =>
          match xinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => xinterp_binds k P (dr ++ d) g1 st1 r
          | inr rr => inr rr
          end
      | XBPrim i p :: r => xinterp_binds k P ((i, xprim_entry d i p) :: d) g st r
      end
  end
with xinterp_apply (fuel : nat) (P : xprog) (d : xdenv) (u : usym) (th : subst)
    (sm : smap) (st : mstate) {struct fuel} : (xdenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match lookup_xdenv (IFun u) d with
      | Some (XDFun h sms dc) =>
          match xp_fun P h with
          | Some (Dr, bs) =>
              match xcompose d sm with
              | Some d1 =>
                  match xrun_chain d1 sms with
                  | Some d2 =>
                      match xinterp_binds k P (xapply_env P d h th (d2 ++ dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match xcompose d3 (inst_smap th Dr) with
                          | Some dr => inl (xresult_env P th d3 Dr dr, st3)
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
(* Immutable application maps carry every type and provision explicitly. *)
with xinterp_apply_r (fuel : nat) (P : xprog) (d : xdenv) (u : usym)
    (inputs outputs : smap) (st : mstate) {struct fuel} : (xdenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match lookup_xdenv (IFun u) d with
      | Some (XDFun h sms dc) =>
          match xp_fun P h with
          | Some (_, bs) =>
              match xcompose d inputs with
              | Some d1 =>
                  match xrun_chain d1 sms with
                  | Some d2 =>
                      match xinterp_binds k P (d2 ++ dc) [] st bs with
                      | inl (d3, _, st3) =>
                          match xcompose d3 (xp_exports P h) with
                          | Some named =>
                              match xcompose named outputs with
                              | Some dr => inl (dr, st3)
                              | None => inr (XR (XErr e_compose) [] st3)
                              end
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
(* the bootstrap IR's `bind F;`: run the branch of the functor F is bound to,
   in a scope of its own, and keep exactly the result's entries *)
with xinterp_branch (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (u : usym) (brs : list (usym * list xbind * smap)) {struct fuel}
    : (xdenv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match lookup_xdenv (IFun u) d with
      | Some (XDFunSym h) =>
          match find_branch h brs with
          | Some (bs, pairs) =>
              match xinterp_binds k P d g st bs with
              | inl (d3, g3, st3) =>
                  match xcompose d3 pairs with
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

(* Running `main`: its body in the initial environment (the platform's
   provisions for main's needs) and the initial machine. *)
Definition run_main (fuel : nat) (P : xprog) (main : fsym) (d0 : xdenv)
    (st0 : mstate) : xres :=
  match xp_fn P main with
  | Some body => ret_call [] (xinterp fuel P d0 [] st0 body)
  | None => XR (XErr e_nobody) [] st0
  end.
End XSem.

(* ######################################################################## *)
(* Part IV. The primitives' signatures                                       *)
(* ######################################################################## *)

Inductive xty : Type := XTI32 | XTI64 | XTF32 | XTF64 | XTUnit | XTBool
(* Track B: an address as `Memories`' members receive it (an `Addr`, a tag
   over an I32), and any tag (the memory a member is a method of) *)
| XTAddr | XTTag.

Definition xvty (P : xprog) (w : xvalue) (t : xty) : Prop :=
  match t, w with
  | XTI32, XVI32 _ | XTI64, XVI64 _ | XTF32, XVF32 _ | XTF64, XVF64 _ => True
  | XTUnit, XVUnit => True
  | XTBool, XVTag b (XVTag t' XVUnit) => b = xp_bool P /\ (t' = xp_true P \/ t' = xp_false P)
  | XTAddr, XVTag _ (XVI32 _) => True
  | XTTag, XVTag _ _ => True
  | _, _ => False
  end.

(* The signatures of lib/wasm.moss, lib/wasip1.moss and wasistd's i32_bool
   (Moss's I32 / I64 / F32 / F64 / () / Bool).  The GC primitives and the
   unmodelled ones have none here: their results depend on the heap. *)
Definition prim_sig (p : prim) : option (list xty * xty) :=
  match p with
  | PI32Bin _ | PI32Cmp _ => Some ([XTI32; XTI32], XTI32)
  | PI32Un _ => Some ([XTI32], XTI32)
  | PF64Bin _ => Some ([XTF64; XTF64], XTF64)
  | PF64Cmp _ => Some ([XTF64; XTF64], XTI32)
  | PF64Un _ => Some ([XTF64], XTF64)
  | PConv (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([XTF64], XTI32)
  | PConv (CConvS | CConvU) => Some ([XTI32], XTF64)
  | PLoad _ => Some ([XTI32], XTI32)
  | PStore _ => Some ([XTI32; XTI32], XTUnit)
  | PF64Load => Some ([XTI32], XTF64)
  | PF64Store => Some ([XTI32; XTF64], XTUnit)
  | PMemSize => Some ([], XTI32)
  | PMemGrow => Some ([XTI32], XTI32)
  | PMemCopy | PMemFill => Some ([XTI32; XTI32; XTI32], XTUnit)
  | PUnreachable => Some ([], XTUnit)
  | PFdWrite => Some ([XTI32; XTI32; XTI32; XTI32], XTI32)
  | PProcExit => Some ([XTI32], XTUnit)
  | PArgsSizesGet | PArgsGet => Some ([XTI32; XTI32], XTI32)
  | PPathOpen => Some ([XTI32; XTI32; XTI32; XTI32; XTI32; XTI64; XTI64; XTI32; XTI32], XTI32)
  | PFdRead => Some ([XTI32; XTI32; XTI32; XTI32], XTI32)
  | PFdClose => Some ([XTI32], XTI32)
  | PI32Bool => Some ([XTI32], XTBool)
  | PF32Bin _ => Some ([XTF32; XTF32], XTF32)
  | PF32Cmp _ => Some ([XTF32; XTF32], XTI32)
  | PF32Un _ => Some ([XTF32], XTF32)
  | PConv32 (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([XTF32], XTI32)
  | PConv32 (CConvS | CConvU) => Some ([XTI32], XTF32)
  | PF32Load => Some ([XTI32], XTF32)
  | PF32Store => Some ([XTI32; XTF32], XTUnit)
  | PDemote => Some ([XTF64], XTF32)
  | PPromote => Some ([XTF32], XTF64)
  | PI32ReinterpF32 => Some ([XTF32], XTI32)
  | PF32ReinterpI32 => Some ([XTI32], XTF32)
  | PI64Bin _ => Some ([XTI64; XTI64], XTI64)
  | PI64Cmp _ => Some ([XTI64; XTI64], XTI32)
  | PI64Un U64Eqz => Some ([XTI64], XTI32)
  | PI64Un _ => Some ([XTI64], XTI64)
  | PWrap => Some ([XTI64], XTI32)
  | PExtendS | PExtendU => Some ([XTI32], XTI64)
  | PI64Load _ => Some ([XTI32], XTI64)
  | PI64Store _ => Some ([XTI32; XTI64], XTUnit)
  | PConv64 (C64Trunc f32 _ _) => Some ([if f32 then XTF32 else XTF64], XTI64)
  | PConv64 (C64Conv to32 _) => Some ([XTI64], if to32 then XTF32 else XTF64)
  | PI64ReinterpF64 => Some ([XTF64], XTI64)
  | PF64ReinterpI64 => Some ([XTI64], XTF64)
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy | PUnsupported _ => None
  end.

(* Track B.  The memory primitives as the platform provides them, as
   methods of `Memories` (D79 as amended, DRAFT NOTE 23): an address
   argument is an `Addr`, and memory.size and memory.grow receive the
   memory they are a method of.  MossX's primitives read the bare address
   (MossX.mem_args); interface_laws includes the law at these
   signatures. *)
Definition prim_msig (p : prim) : option (list xty * xty) :=
  match p with
  | PLoad _ => Some ([XTAddr], XTI32)
  | PStore _ => Some ([XTAddr; XTI32], XTUnit)
  | PF64Load => Some ([XTAddr], XTF64)
  | PF64Store => Some ([XTAddr; XTF64], XTUnit)
  | PF32Load => Some ([XTAddr], XTF32)
  | PF32Store => Some ([XTAddr; XTF32], XTUnit)
  | PI64Load _ => Some ([XTAddr], XTI64)
  | PI64Store _ => Some ([XTAddr; XTI64], XTUnit)
  | PMemCopy => Some ([XTAddr; XTAddr; XTI32], XTUnit)
  | PMemFill => Some ([XTAddr; XTI32; XTI32], XTUnit)
  | PMemSize => Some ([XTTag], XTI32)
  | PMemGrow => Some ([XTTag; XTI32], XTI32)
  | _ => None
  end.

(* Which library fn is which primitive: the elaborator gives `main`'s need of
   a lib/wasm.moss or lib/wasip1.moss fn named n the primitive
   `prim_of_name n` (moved from Elab.v).  An auditor checks this table
   against lib/wasm.moss's and lib/wasip1.moss's names. *)
Section PrimNames.
Local Open Scope string_scope.
Definition strip_prefix (pre s : string) : option string :=
  if String.prefix pre s then Some (substring (String.length pre) (String.length s - String.length pre) s) else None.

Definition i32bin_of (s : string) : option i32bin :=
  match s with
  | "add" => Some IAdd | "sub" => Some ISub | "mul" => Some IMul | "div_s" => Some IDivS
  | "div_u" => Some IDivU | "rem_s" => Some IRemS | "rem_u" => Some IRemU | "and" => Some IAnd
  | "or" => Some IOr | "xor" => Some IXor | "shl" => Some IShl | "shr_s" => Some IShrS
  | "shr_u" => Some IShrU | "rotl" => Some IRotl | "rotr" => Some IRotr | _ => None
  end.
Definition i32cmp_of (s : string) : option i32cmp :=
  match s with
  | "eq" => Some CEq | "ne" => Some CNe | "lt_s" => Some CLtS | "lt_u" => Some CLtU
  | "gt_s" => Some CGtS | "gt_u" => Some CGtU | "le_s" => Some CLeS | "le_u" => Some CLeU
  | "ge_s" => Some CGeS | "ge_u" => Some CGeU | _ => None
  end.
Definition i32un_of (s : string) : option i32un :=
  match s with
  | "eqz" => Some UEqz | "clz" => Some UClz | "ctz" => Some UCtz | "popcnt" => Some UPopcnt
  | "extend8_s" => Some UExt8 | "extend16_s" => Some UExt16 | _ => None
  end.
Definition f64bin_of (s : string) : option f64bin :=
  match s with
  | "add" => Some FAdd | "sub" => Some FSub | "mul" => Some FMul | "div" => Some FDiv
  | "min" => Some FMin | "max" => Some FMax | "copysign" => Some FCopysign | _ => None
  end.
Definition f64un_of (s : string) : option f64un :=
  match s with
  | "abs" => Some FAbs | "neg" => Some FNeg | "ceil" => Some FCeil | "floor" => Some FFloor
  | "trunc" => Some FTrunc | "nearest" => Some FNearest | "sqrt" => Some FSqrt | _ => None
  end.
Definition f64cmp_of (s : string) : option f64cmp :=
  match s with
  | "eq" => Some FEq | "ne" => Some FNe | "lt" => Some FLt | "gt" => Some FGt
  | "le" => Some FLe | "ge" => Some FGe | _ => None
  end.

Definition prim_other (s : string) : option prim :=
  match s with
  | "i32_load" => Some (PLoad L32) | "i32_load8_s" => Some (PLoad L8S) | "i32_load8_u" => Some (PLoad L8U)
  | "i32_load16_s" => Some (PLoad L16S) | "i32_load16_u" => Some (PLoad L16U)
  | "i32_store" => Some (PStore S32) | "i32_store8" => Some (PStore S8) | "i32_store16" => Some (PStore S16)
  | "f64_load" => Some PF64Load | "f64_store" => Some PF64Store
  | "memory_size" => Some PMemSize | "memory_grow" => Some PMemGrow
  | "memory_copy" => Some PMemCopy | "memory_fill" => Some PMemFill
  | "unreachable" => Some PUnreachable | "fd_write" => Some PFdWrite | "proc_exit" => Some PProcExit
  | "args_sizes_get" => Some PArgsSizesGet | "args_get" => Some PArgsGet
  | "path_open" => Some PPathOpen | "fd_read" => Some PFdRead | "fd_close" => Some PFdClose
  | "i32_bool" => Some PI32Bool
  | "i32_trunc_f64_s" => Some (PConv CTruncS) | "i32_trunc_f64_u" => Some (PConv CTruncU)
  | "i32_trunc_sat_f64_s" => Some (PConv CTruncSatS) | "i32_trunc_sat_f64_u" => Some (PConv CTruncSatU)
  | "f64_convert_i32_s" => Some (PConv CConvS) | "f64_convert_i32_u" => Some (PConv CConvU)
  | "i32_trunc_f32_s" => Some (PConv32 CTruncS) | "i32_trunc_f32_u" => Some (PConv32 CTruncU)
  | "i32_trunc_sat_f32_s" => Some (PConv32 CTruncSatS) | "i32_trunc_sat_f32_u" => Some (PConv32 CTruncSatU)
  | "f32_convert_i32_s" => Some (PConv32 CConvS) | "f32_convert_i32_u" => Some (PConv32 CConvU)
  | "f32_load" => Some PF32Load | "f32_store" => Some PF32Store | "f32_demote_f64" => Some PDemote
  | "f64_promote_f32" => Some PPromote | "i32_reinterpret_f32" => Some PI32ReinterpF32
  | "f32_reinterpret_i32" => Some PF32ReinterpI32
  | "i64_eqz" => Some (PI64Un U64Eqz) | "i64_clz" => Some (PI64Un U64Clz) | "i64_ctz" => Some (PI64Un U64Ctz)
  | "i64_popcnt" => Some (PI64Un U64Popcnt) | "i64_extend8_s" => Some (PI64Un U64Ext8)
  | "i64_extend16_s" => Some (PI64Un U64Ext16) | "i64_extend32_s" => Some (PI64Un U64Ext32)
  | "i32_wrap_i64" => Some PWrap | "i64_extend_i32_s" => Some PExtendS | "i64_extend_i32_u" => Some PExtendU
  | "i64_load" => Some (PI64Load L64) | "i64_load8_s" => Some (PI64Load L64_8S) | "i64_load8_u" => Some (PI64Load L64_8U)
  | "i64_load16_s" => Some (PI64Load L64_16S) | "i64_load16_u" => Some (PI64Load L64_16U)
  | "i64_load32_s" => Some (PI64Load L64_32S) | "i64_load32_u" => Some (PI64Load L64_32U)
  | "i64_store" => Some (PI64Store S64) | "i64_store8" => Some (PI64Store S64_8)
  | "i64_store16" => Some (PI64Store S64_16) | "i64_store32" => Some (PI64Store S64_32)
  | "i64_reinterpret_f64" => Some PI64ReinterpF64 | "f64_reinterpret_i64" => Some PF64ReinterpI64
  | "i64_trunc_f64_s" => Some (PConv64 (C64Trunc false true false))
  | "i64_trunc_f64_u" => Some (PConv64 (C64Trunc false false false))
  | "i64_trunc_f32_s" => Some (PConv64 (C64Trunc true true false))
  | "i64_trunc_f32_u" => Some (PConv64 (C64Trunc true false false))
  | "i64_trunc_sat_f64_s" => Some (PConv64 (C64Trunc false true true))
  | "i64_trunc_sat_f64_u" => Some (PConv64 (C64Trunc false false true))
  | "i64_trunc_sat_f32_s" => Some (PConv64 (C64Trunc true true true))
  | "i64_trunc_sat_f32_u" => Some (PConv64 (C64Trunc true false true))
  | "f64_convert_i64_s" => Some (PConv64 (C64Conv false true))
  | "f64_convert_i64_u" => Some (PConv64 (C64Conv false false))
  | "f32_convert_i64_s" => Some (PConv64 (C64Conv true true))
  | "f32_convert_i64_u" => Some (PConv64 (C64Conv true false))
  | _ => None
  end.

Definition prim_of_name (s : string) : option prim :=
  match prim_other s with
  | Some p => Some p
  | None =>
      let try_ {A} (pre : string) (f : string -> option A) (k : A -> prim) : option prim :=
        match strip_prefix pre s with
        | Some r => option_map k (f r)
        | None => None
        end in
      let first (l : list (option prim)) := fold_right (fun o acc => match o with Some _ => o | None => acc end) None l in
      first [try_ "i32_" i32bin_of PI32Bin; try_ "i32_" i32cmp_of PI32Cmp; try_ "i32_" i32un_of PI32Un;
             try_ "f64_" f64bin_of PF64Bin; try_ "f64_" f64cmp_of PF64Cmp; try_ "f64_" f64un_of PF64Un;
             try_ "f32_" f64bin_of PF32Bin; try_ "f32_" f64cmp_of PF32Cmp; try_ "f32_" f64un_of PF32Un;
             try_ "i64_" i32bin_of PI64Bin; try_ "i64_" i32cmp_of PI64Cmp]
  end.

End PrimNames.

(* ######################################################################## *)
(* Part V. The declarative elaboration relation — DRAFT                      *)
(* ######################################################################## *)

(* STATUS: DRAFT, for the designer's review.  The elaborator (Elab.v) is
   neither shown sound nor complete for it. Both are pending obligations
   in ComponentContracts.v. Proved toward soundness
   (WorldSound.v, bound as SourceCertified.loader_reads): the loader's reading of
   an accepted bundle meets every condition of WorldOf (V.1) except the
   requirement lists and the field numbering.

   WHAT IT SAYS.  `Elaborates b p` relates a bundle `b` (Part I) to an
   elaborated program `p` (Part II): `p` is *an* elaboration of `b` if
   there is a static reading `W` of the bundle (which declaration each
   symbol number is, what each name in each module denotes, each symbol's
   requirements) satisfying the conditions of V.1, and every defined
   function's body in `p` is related to its source by the typed judgment
   of V.4–V.6 under the context its assumes form (V.3).

   WHAT MAKES IT DECLARATIVE.  It fixes no algorithm, no traversal order,
   no fuel, and no numbering.  Symbol numbers, local-variable numbers,
   record-field numbers, fresh generations and synthesized symbols are all
   existentially chosen (subject only to being distinct where that is
   observable).  Name resolution is stated as "this name denotes that
   symbol iff it is declared or imported so" (D44 then follows: if two
   symbols qualify there is no reading `W`).  Merging (D43) is a most
   general unifier, not a union-find run.  Requirement and need lists are
   sets.  Which provision a call uses (D61/D62/D77) is a case analysis on
   what is declared and what the context provides, and the context holds at
   most one provision per key, so there is nothing to search (D1).

   HOW TO READ IT.  V.1 the static world; V.2 types; V.3 context formation;
   V.4 calls, needs and coherence; V.5 functors (D84); V.6 expressions,
   statements, binds; V.7 the program.  Each rule cites the decision it
   encodes. Historical differences, counterexamples and open design
   questions are recorded in docs/design/audit-notes.md.
*)

Module Elaboration.
Import Input.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ======================================================================== *)
(* V.0 Small helpers: association lists, as the context's tables            *)
(* ======================================================================== *)

Fixpoint nlook {A : Type} (k : nat) (l : list (nat * A)) : option A :=
  match l with [] => None | (k', a) :: r => if Nat.eqb k k' then Some a else nlook k r end.

Fixpoint slook {A : Type} (k : string) (l : list (string * A)) : option A :=
  match l with [] => None | (k', a) :: r => if String.eqb k k' then Some a else slook k r end.

Fixpoint ilook {A : Type} (k : item) (l : list (item * A)) : option A :=
  match l with [] => None | (k', a) :: r => if item_eqb k k' then Some a else ilook k r end.

(* An instantiation: for each type symbol, what it meant (None: not in force). *)
Definition inst := list (nat * option ty).

(* A signature as elaboration reads it: parameter types and result. *)
Record fsig := FSig { sg_params : list ty; sg_ret : ty }.

Definition sig_subst (s : subst) (g : fsig) : fsig :=
  FSig (map (app_subst s) (sg_params g)) (app_subst s (sg_ret g)).

(* The generations (abstract type symbols) a type mentions. *)
Fixpoint gens (t : ty) : list nat :=
  match t with
  | TUnit => []
  | TAbs g => [g]
  | TNom _ xs => (fix go (l : list (tsym * ty)) := match l with [] => [] | p :: r => gens (snd p) ++ go r end) xs
  | TUnion ms => (fix go (l : list ty) := match l with [] => [] | m :: r => gens m ++ go r end) ms
  end.

(* Records have no declaration: their elaborated type is a reserved head 0
   (never a symbol) applied to their fields, by interned field number. *)
Definition rec_head : nat := 0.

Definition head (t : ty) : option nat :=
  match t with TNom n _ => Some n | TAbs g => Some g | _ => None end.

Definition members (t : ty) : list ty :=
  match t with TUnion ms => ms | _ => [t] end.

Definition never : ty := TUnion [].

(* D17: a value of type t is accepted where `expected` is wanted: equal,
   divergent, or injected into a union that has all its members. *)
Definition fits (t expected : ty) : Prop :=
  t = expected \/ t = never \/
  (exists ms, expected = TUnion ms /\ forall m, In m (members t) -> In m ms).

Definition opt_fits (t : ty) (exp : option ty) : Prop :=
  match exp with Some e => fits t e | None => True end.

(* ======================================================================== *)
(* V.1 The static world: what each symbol is, what each name denotes        *)
(* ======================================================================== *)

(* The modules of a bundle, and the imports of one (the prelude first, when
   the module wants it, as an unqualified glob import). *)
Definition module_of (b : bundle) (p : string) : option amodule :=
  find (fun m => String.eqb (am_path m) p) (b_mods b).

Definition explicit_imports (m : amodule) : list (string * aimport) :=
  combine (am_targets m) (f_imports (am_file m)).

Definition imports_of (b : bundle) (m : amodule) : list (string * aimport) :=
  (if am_prelude m then [(b_prelude b, AImport (b_prelude b) None true [])] else [])
  ++ combine (am_targets m) (f_imports (am_file m)).

Lemma explicit_in_imports : forall b m t imp,
  In (t, imp) (explicit_imports m) -> In (t, imp) (imports_of b m).
Proof.
  intros b m t imp H. unfold imports_of, explicit_imports in *.
  apply in_or_app. right. exact H.
Qed.

Lemma prelude_in_imports : forall b m, am_prelude m = true ->
  In (b_prelude b, AImport (b_prelude b) None true []) (imports_of b m).
Proof. intros b m H. unfold imports_of. rewrite H. left. reflexivity. Qed.

Inductive Loaded (b : bundle) : string -> Prop :=
| LoadEntry : Loaded b (b_entry b)
| LoadImport : forall p m t imp,
    Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) -> Loaded b t.

(* A declaration together with where it is: its module and the assume
   items enclosing it, outermost first ([D20]: requirements nest by union). *)
Record decl := Decl { d_mod : string; d_ast : adecl; d_assumes : list aitem }.

Inductive OccursIn : list adecl -> list aitem -> adecl -> Prop :=
| OccHere : forall ds d,
    In d ds -> (forall its inner, d <> ADAssume its inner) -> OccursIn ds [] d
| OccNested : forall ds its inner asm d,
    In (ADAssume its inner) ds -> OccursIn inner asm d -> OccursIn ds (its ++ asm) d.

Definition Declared (b : bundle) (d : decl) : Prop :=
  Loaded b (d_mod d) /\
  exists m, module_of b (d_mod d) = Some m /\ OccursIn (f_decls (am_file m)) (d_assumes d) (d_ast d).

Inductive dkind := DKType | DKUnit | DKAlias | DKTag | DKVal | DKFn | DKCtx | DKMeth | DKFunctor.

Definition kind_of (d : adecl) : option dkind :=
  match d with
  | ADType _ => Some DKType | ADUnit _ => Some DKUnit | ADAlias _ _ => Some DKAlias
  | ADTag _ _ _ => Some DKTag | ADVal _ _ _ => Some DKVal | ADCtx _ _ => Some DKCtx
  | ADFunctor _ _ _ _ _ => Some DKFunctor
  | ADFn RPlain _ _ _ _ => Some DKFn
  | ADFn _ _ _ _ _ => Some DKMeth
  | ADAssume _ _ => None
  end.

(* The name a declaration is filed under: plain names (types, units, tags,
   aliases, vals, fns, contexts, functors) and detached methods `.m`; an
   attached method `R.m` is filed under its receiver instead ([D61]). *)
Inductive nspace := NSName | NSDetached | NSAlias.

Definition filed_name (d : adecl) : option (nspace * name) :=
  match d with
  | ADFn RDetached n _ _ _ => Some (NSDetached, n)
  | ADFn (RAttached _) _ _ _ _ => None
  | ADAssume _ _ => None
  | _ => match d with
         | ADType n | ADUnit n | ADAlias n _ | ADTag n _ _ | ADVal n _ _ | ADCtx n _
         | ADFunctor n _ _ _ _ | ADFn _ n _ _ _ => Some (NSName, n)
         | _ => None
         end
  end.

(* What a name in a module's scope denotes: a symbol, or (an alias) a module. *)
Inductive ent := ESym (s : nat) | EMod (p : string).

(* The static world: a reading of the bundle.  It is a witness: the
   relation asks only that one exist satisfying `WorldOf`. *)
Record world := World {
  w_decl  : nat -> option decl;                      (* symbol -> declaration *)
  w_scope : string -> nspace -> name -> option ent;  (* module, namespace, name -> denotation *)
  w_exp   : string -> nspace -> name -> option ent;  (* what a module exports *)
  w_attached : nat -> name -> option nat;            (* receiver symbol, name -> attached method *)
  w_reqs  : nat -> list nat;                         (* a symbol's requirements (type symbols) *)
  w_fld   : string -> nat;                           (* record field numbering *)
  w_library : option string                         (* prelude's directory prefix *)
}.

Definition kind_sym (W : world) (s : nat) : option dkind :=
  match w_decl W s with Some d => kind_of (d_ast d) | None => None end.

Definition ast_of (W : world) (s : nat) : option adecl := option_map d_ast (w_decl W s).
Definition mod_of (W : world) (s : nat) : string :=
  match w_decl W s with Some d => d_mod d | None => "" end.

(* [D44]/[D61]/[D64]: what a module exports, and what its scope holds.
   A module exports its own declarations and what it imported by `use`
   (with the use's local name), not what a glob import brought in.  Its
   ordinary scope holds what it exports and everything its explicit glob
   imports export. The implicit prelude fills only unbound names. *)
Inductive Exports (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ExOwn : forall p s d ns n,
    w_decl W s = Some d -> d_mod d = p -> filed_name (d_ast d) = Some (ns, n) ->
    Exports b W p ns n (ESym s)
| ExUse : forall p m t imp u e ns,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_glob imp = false ->
    In u (i_uses imp) ->
    ns = (if u_dot u then NSDetached else NSName) ->
    (* a plain name is the symbol t exports under it, else the module alias
       (as path resolution reads it): note 25 *)
    (Exports b W t ns (u_name u) e \/
     (u_dot u = false /\ w_exp W t NSName (u_name u) = None /\ Exports b W t NSAlias (u_name u) e)) ->
    Exports b W p (match e with EMod _ => NSAlias | _ => ns end)
            (match u_alias u with Some (_, a) => a | None => u_name u end) e
| ExAlias : forall p m t imp a,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_alias imp = Some a ->
    Exports b W p NSAlias a (EMod t).

Inductive OrdinaryScope (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ScExported : forall p ns n e, Exports b W p ns n e -> OrdinaryScope b W p ns n e
| ScGlob : forall p m t imp ns n e,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_glob imp = true ->
    Exports b W t ns n e -> OrdinaryScope b W p ns n e.

(* The prelude contributes only where no declaration or explicit import
   already supplies this namespace/name. It does not contribute exports. *)
Inductive InScope (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ScPrimary : forall p ns n e,
    OrdinaryScope b W p ns n e -> InScope b W p ns n e
| ScPrelude : forall p m ns n e,
    module_of b p = Some m -> am_prelude m = true ->
    Exports b W (b_prelude b) ns n e ->
    (forall e', ~ OrdinaryScope b W p ns n e') -> InScope b W p ns n e.

(* Every visible binding still originates in an export, either here or
   in a dependency. Clients that only need declaration provenance need
   not depend on the priority rules used to select that binding. *)
Lemma scope_exports : forall b W p ns n e, InScope b W p ns n e ->
  Exports b W p ns n e \/ exists m t imp, module_of b p = Some m /\
    In (t, imp) (imports_of b m) /\ Exports b W t ns n e.
Proof.
  intros b W p ns n e H. destruct H as [p ns n e Hordinary | p m ns n e Hm Hpre Hexp Hnone].
  - destruct Hordinary as [p ns n e Hexp | p m t imp ns n e Hm Hin Hg Hexp].
    + left. exact Hexp.
    + right. exists m, t, imp. split; [exact Hm|].
      split; [apply explicit_in_imports; exact Hin|exact Hexp].
  - right. exists m, (b_prelude b), (AImport (b_prelude b) None true []).
    split; [exact Hm|]. split; [apply prelude_in_imports; exact Hpre|exact Hexp].
Qed.

(* Path resolution: the first segment in scope, the rest through exports. *)
Fixpoint resolve_rest (W : world) (e : ent) (segs : list name) : option ent :=
  match segs with
  | [] => Some e
  | seg :: r =>
      match e with
      | ESym _ => None
      | EMod t =>
          match w_exp W t NSName seg with
          | Some e' => resolve_rest W e' r
          | None => match w_exp W t NSAlias seg with Some e' => resolve_rest W e' r | None => None end
          end
      end
  end.

Definition resolve (W : world) (p : string) (pa : path) : option ent :=
  match pa with
  | [] => None
  | hd :: rest =>
      match w_scope W p NSName hd with
      | Some e => resolve_rest W e rest
      | None => match w_scope W p NSAlias hd with Some e => resolve_rest W e rest | None => None end
      end
  end.

Definition resolve_sym (W : world) (p : string) (pa : path) : option nat :=
  match resolve W p pa with Some (ESym s) => Some s | _ => None end.

(* A detached method name: `.m` in scope, or `mod::m` ([D61]). *)
Definition resolve_detached (W : world) (p : string) (pa : path) : option nat :=
  match rev pa with
  | [] => None
  | [n] => match w_scope W p NSDetached n with Some (ESym s) => Some s | _ => None end
  | n :: rpre => match resolve W p (rev rpre) with
                 | Some (EMod t) => match w_exp W t NSDetached n with Some (ESym s) => Some s | _ => None end
                 | _ => None
                 end
  end.

(* [D61]: an attached method `R.m` is declared in R's own file. *)
Definition AttachedDecl (W : world) (r : nat) (n : name) (s : nat) : Prop :=
  exists d rn ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn (RAttached rn) n ps rt body /\
    d_mod d = mod_of W r /\ w_scope W (d_mod d) NSName rn = Some (ESym r).

(* The attached method of receiver r named n, as the world files it. *)
Definition Attached (W : world) (r : nat) (n : name) (s : nat) : Prop := w_attached W r n = Some s.

(* [D71]/[D73]: a nominal's requirements are its *relevant* parameters: the
   ones it lists, and the type symbols its payload mentions, closed through
   the nominals it mentions (less what brackets bind there).  `FreeIn R`
   reads a type expression's free type symbols against a candidate R. *)
Inductive FreeIn (W : world) (R : nat -> list nat) (p : string) : aty -> nat -> Prop :=
| FRec : forall fs n t x, In (n, t) fs -> FreeIn W R p t x -> FreeIn W R p (ATRec fs) x
| FUnion : forall ms t x, In t ms -> FreeIn W R p t x -> FreeIn W R p (ATUnion ms) x
| FType : forall pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    FreeIn W R p (ATRef pa a) t
| FNom : forall pa a g x,
    resolve_sym W p pa = Some g -> kind_sym W g <> Some DKType -> In x (R g) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some x) ->
    FreeIn W R p (ATRef pa a) x
| FBracket : forall pa a bpath sp d sa x,
    (* the head resolves to a symbol (note 31) *)
    resolve_sym W p pa <> None ->
    In (bpath, Spec (Some sp) d sa) (match a with Some bs => bs | None => [] end) ->
    FreeIn W R p (ATRef sp sa) x -> FreeIn W R p (ATRef pa a) x.

(* [D20]/[D21]: the type symbols an assume nesting requires, through the
   contexts it names, with those its equations write ([D90]).  For a val,
   fn, method, context or functor these are its requirements. *)
Inductive SpecTypes (W : world) : string -> spec -> nat -> Prop :=
| STType : forall p pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType -> SpecTypes W p (Spec (Some pa) None a) t
| STCtx : forall p pa a c d n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    In sp (flat_map citem_specs its) -> SpecTypes W (d_mod d) sp t -> SpecTypes W p (Spec (Some pa) None a) t.

Definition AssumeTypes (W : world) (s : nat) (t : nat) : Prop :=
  exists d sp, w_decl W s = Some d /\ In sp (flat_map aitem_specs (d_assumes d)) /\
               SpecTypes W (d_mod d) sp t.

Definition nominal_body (d : adecl) : option (list name * aty) :=
  match d with ADTag _ ps t => Some (ps, t) | ADAlias _ t => Some ([], t) | _ => None end.

Definition RelevanceClosed (W : world) (R : nat -> list nat) : Prop :=
  forall g d ps t, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) ->
    (forall n x, In n ps -> resolve_sym W (d_mod d) [n] = Some x -> In x (R g)) /\
    (forall x, FreeIn W R (d_mod d) t x -> In x (R g)).

(* The conditions on a reading W of b. *)
Record WorldOf (b : bundle) (W : world) : Prop := {
  (* imports are acyclic and every module imported is in the bundle *)
  wo_acyclic : exists rank : string -> nat, forall p m t imp,
      Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) ->
      module_of b t <> None /\ rank t < rank p;
  (* every symbol is a declaration of the bundle, and every declaration is
     exactly one symbol (0 is never a symbol: records use it, V.0) *)
  wo_decl_sound : forall s d, w_decl W s = Some d -> s <> 0 /\ Declared b d /\ kind_of (d_ast d) <> None;
  wo_decl_complete : forall d, Declared b d -> kind_of (d_ast d) <> None ->
      exists s, w_decl W s = Some d /\ forall s', w_decl W s' = Some d -> s' = s;
  (* [D44]: scopes and exports are exactly what declarations and imports
     put there; since w_scope is a function, two symbols under one name in
     one scope make the program have no reading at all *)
  wo_exports : forall p ns n e, Loaded b p -> (w_exp W p ns n = Some e <-> Exports b W p ns n e);
  wo_scope : forall p ns n e, Loaded b p -> (w_scope W p ns n = Some e <-> InScope b W p ns n e);
  (* an attached method is filed under its receiver, a type of its own
     file, and the table holds exactly the declared ones ([D61]) *)
  wo_attached : forall r n s, w_attached W r n = Some s <-> AttachedDecl W r n s;
  (* requirements: of a val/fn/method/context/functor, its assume nesting's
     type symbols; of a type or unit, none; of a tag or alias, the least
     relevance-closed sets ([D71]), every listed name a type of its nesting
     ([D73]).  Lists have no repeats; their order is free, but fixed per
     symbol, which fixes the order of a nominal's arguments in `TNom`. *)
  wo_reqs_nodup : forall s, NoDup (w_reqs W s);
  wo_reqs_plain : forall s k, kind_sym W s = Some k ->
      k <> DKTag -> k <> DKAlias -> k <> DKType -> k <> DKUnit ->
      forall t, In t (w_reqs W s) <-> AssumeTypes W s t;
  wo_reqs_none : forall s, kind_sym W s = Some DKType \/ kind_sym W s = Some DKUnit -> w_reqs W s = [];
  wo_reqs_closed : RelevanceClosed W (w_reqs W);
  wo_reqs_least : forall R, RelevanceClosed W R ->
      (* R agrees with the requirements fixed above, of everything but a
         tag or alias (note 31) *)
      (forall s, kind_sym W s <> Some DKTag -> kind_sym W s <> Some DKAlias -> R s = w_reqs W s) ->
      forall g, (kind_sym W g = Some DKTag \/ kind_sym W g = Some DKAlias) -> incl (w_reqs W g) (R g);
  wo_listed : forall g d ps t n, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) -> In n ps ->
      exists x, resolve_sym W (d_mod d) [n] = Some x /\ AssumeTypes W g x;
  (* record fields: distinct names get distinct numbers *)
  wo_fld : forall a c, w_fld W a = w_fld W c -> a = c;
  wo_library : w_library W = Loader.library_dir (b_prelude b)
}.

(* Library symbols the language itself refers to ([D45], [D68]): the
   symbol in scope as `n` in `<prelude directory>/<short>.moss`.
   Reexports are ordinary scope entries, including here (note 68). *)
Definition LibraryModule (W : world) (short p : string) : Prop :=
  match w_library W with
  | Some dir => p = String.append dir (String.append short ".moss")
  | None => False
  end.

Definition LangSym (W : world) (short : string) (ns : nspace) (n : name) (s : nat) : Prop :=
  exists d p, w_decl W s = Some d /\ LibraryModule W short p /\
              w_scope W p ns n = Some (ESym s).

(* ======================================================================== *)
(* V.2 Types                                                                 *)
(* ======================================================================== *)

(* `TyOf W p tm this t τ`: the type expression t, written in module p,
   denotes τ, where `tm` gives each type symbol in force its meaning here
   ([D72]: a type symbol's meaning is read where the type is written;
   rebinding it later does not change τ) and `this` the receiver type
   inside a method declaration.  A nominal reference is total: each of its
   requirements is bound in brackets or in force ([D22] as clarified). *)
Inductive TyOf (W : world) : string -> list (nat * ty) -> option ty -> aty -> ty -> Prop :=
| TyNever : forall p tm th, TyOf W p tm th ATNever never
| TyUnit  : forall p tm th, TyOf W p tm th ATUnit TUnit
| TyThis  : forall p tm t, TyOf W p tm (Some t) ATThis t
| TyRec   : forall p tm th fs ts,
    Forall2 (fun nt ft => fst ft = w_fld W (fst nt) /\ TyOf W p tm th (snd nt) (snd ft)) fs ts ->
    TyOf W p tm th (ATRec fs) (TNom rec_head ts)
| TyUnion : forall p tm th ms ts flat,
    Forall2 (TyOf W p tm th) ms ts ->
    flat = flat_map members ts ->
    (* [D15]: members are nominal (a record's reserved head is not: note
       34), and no two share a head (note 33), so each appears once *)
    Forall (fun m => exists n xs, m = TNom n xs /\ n <> rec_head) flat -> NoDup (map head flat) ->
    TyOf W p tm th (ATUnion ms) (TUnion flat)
| TyType  : forall p tm th pa t tau,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType -> nlook t tm = Some tau ->
    TyOf W p tm th (ATRef pa None) tau
| TyNom   : forall p tm th pa a g k bs args,
    resolve_sym W p pa = Some g -> kind_sym W g = Some k -> (k = DKTag \/ k = DKUnit) ->
    Brackets W p tm g a bs ->
    (* the arguments: each requirement, in its fixed order, as bound or in force *)
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ tm) = Some (snd ra)) (w_reqs W g) args ->
    TyOf W p tm th (ATRef pa a) (TNom g args)
| TyAlias : forall p tm th pa a g d n body bs tau,
    resolve_sym W p pa = Some g -> w_decl W g = Some d -> d_ast d = ADAlias n body ->
    Brackets W p tm g a bs ->
    TyOf W (d_mod d) (bs ++ tm) None body tau ->
    TyOf W p tm th (ATRef pa a) tau
(* `Brackets W p tm g a bs`: the bracket application `a` on g binds each
   left side (a type symbol among g's requirements, [D71]; on a method,
   any type symbol: note 56) to its right side read here; later bindings
   override earlier ones. *)
with Brackets (W : world) : string -> list (nat * ty) -> nat -> option (list (path * spec)) -> list (nat * ty) -> Prop :=
| BrNone : forall p tm g, Brackets W p tm g None []
| BrSome : forall p tm g bs l,
    Forall2 (fun b tb =>
               exists sp sa, snd b = Spec (Some sp) None sa /\
                 resolve_sym W p (fst b) = Some (fst tb) /\ kind_sym W (fst tb) = Some DKType /\
                 (kind_sym W g <> Some DKMeth -> In (fst tb) (w_reqs W g)) /\
                 TyOf W p tm None (ATRef sp sa) (snd tb)) bs l ->
    Brackets W p tm g (Some bs) (rev l).

(* ======================================================================== *)
(* V.3 Contexts in force, and their formation with merging                  *)
(* ======================================================================== *)

(* A context in force at a point of a body.  Every table is keyed by
   identity ([D62]): a val, fn or functor by its symbol, a method by its
   receiver's canonical *type* and its symbol; a lookup takes the latest
   entry.  `c_sub` is the merging substitution ([D43]) on generations;
   types in the other tables are canonical (c_sub applied).  `c_apps`
   records, for every provision, the instantiation it was made at ([D62],
   [D71] coherence).  `c_funs` records, for each abstract functor in
   force, what its fixed inputs X(F) denoted where it was provided ([D84]
   rules 2 and 5). *)
Inductive lmut := LLet | LVar | LCaptured.

Record cx := Cx {
  c_ty     : list (nat * ty);
  c_sub    : subst;
  c_vals   : list (nat * ty);
  c_fns    : list (nat * fsig);
  c_mths   : list ((ty * nat) * fsig);
  c_funs   : list (nat * list (nat * ty));
  c_apps   : list (item * inst);
  c_locals : list (name * (ty * lmut))
}.

Fixpoint mlook (r : ty) (m : nat) (l : list ((ty * nat) * fsig)) : option fsig :=
  match l with
  | [] => None
  | ((r', m'), g) :: rest => if ty_eqb r r' && Nat.eqb m m' then Some g else mlook r m rest
  end.

Definition canon (C : cx) (t : ty) : ty := app_subst (c_sub C) t.

(* The item a provision is filed under, and its runtime key ([D62]): the
   elaborated items carry θ = [] at this milestone (DRAFT NOTE 3). *)
Definition mkey (r : ty) (m : nat) : item := IMth r m [].

(* Note 28: a method provision is filed (c_mths, c_apps) under its
   receiver and method, mkey; its run-time key carries θ, the
   instantiation it was made at (its known entries, canonical). *)
Definition known (a : inst) : subst :=
  flat_map (fun q => match snd q with Some t => [(fst q, t)] | None => [] end) a.
Definition theta_of (C : cx) (a : inst) : subst := map (fun q => (fst q, canon C (snd q))) (known a).
Definition rkey (C : cx) (r : ty) (m : nat) : item :=
  IMth r m (match ilook (mkey r m) (c_apps C) with Some a => theta_of C a | None => [] end).
Definition rkey_of (C : cx) (k : item) : item :=
  match k with IMth r m _ => rkey C r m | _ => k end.

(* A provision has one identity; method type arguments are read separately. *)
Definition provision_id (it : item) : item :=
  match it with IMth r m _ => IMth r m [] | _ => it end.

Definition provides (C : cx) (i : item) : Prop :=
  match i with
  | IVal v => nlook v (c_vals C) <> None
  | IFn f => nlook f (c_fns C) <> None
  | IMth r m _ => mlook r m (c_mths C) <> None
  | IFun u => nlook u (c_funs C) <> None
  | ITy t => nlook t (c_ty C) <> None
  end.

(* A raw context entry, as an item of a telescope introduces it. *)
Inductive entry :=
| NTy  (t : nat)
| NVal (v : nat) (t : ty) (a : inst)
| NFn  (f : nat) (g : fsig) (a : inst)
| NMth (r : ty) (m : nat) (g : fsig) (a : inst)
| NFun (u : nat) (fx : subst)
(* an equation item's two sides, read ([D90]): no key, only a merge *)
| NEq  (l r : ty).

(* `SigOf W s tm this g`: the declared signature of fn/method s read with
   the type meanings tm (and receiver type `this`). *)
Definition SigOf (W : world) (s : nat) (tm : list (nat * ty)) (this : option ty) (g : fsig) : Prop :=
  exists d rcv n ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    Forall2 (fun pt t => TyOf W (d_mod d) tm this (snd pt) t) ps (sg_params g) /\
    match rt with Some r => TyOf W (d_mod d) tm this r (sg_ret g) | None => sg_ret g = TUnit end.

(* A type symbol t that the signature of fn or method s (a parameter's or
   the result's type) mentions (note 51). *)
Definition SigMentions (W : world) (s : nat) (t : nat) : Prop :=
  exists d rcv n ps rt body, w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    ((exists pt, In pt ps /\ FreeIn W (w_reqs W) (d_mod d) (snd pt) t) \/
     (exists r, rt = Some r /\ FreeIn W (w_reqs W) (d_mod d) r t)).

(* The instantiation a provision of s is made at: each requirement's meaning
   (for a method, exactly the requirements its signature mentions, in
   their fixed order: note 51). *)
(* [D20], sigclose: a method's signature is closed over its requirements,
   which include every type symbol it mentions, so θ records each. The
   mentioned symbols outside its assume nesting (w_reqs) follow the
   nesting's, ascending: a list fixed by its members. *)
Fixpoint Ascending (l : list nat) : Prop :=
  match l with
  | x :: ((y :: _) as r) => x < y /\ Ascending r
  | _ => True
  end.
Definition SigOutside (W : world) (s : nat) (extra : list nat) : Prop :=
  Ascending extra /\ forall t, In t extra <-> SigMentions W s t /\ ~ In t (w_reqs W s).
Lemma ascending_tail : forall x r, Ascending (x :: r) -> Ascending r.
Proof. intros x [|y r] H; [exact I | exact (proj2 H)]. Qed.
Lemma ascending_lt : forall r x, Ascending (x :: r) -> forall y, In y r -> x < y.
Proof.
  induction r as [|z r IH]; intros x H y Hy; [destruct Hy|]. destruct H as [Hxz Hr].
  destruct Hy as [<-|Hy]; [exact Hxz | exact (Nat.lt_trans _ _ _ Hxz (IH z Hr y Hy))].
Qed.
(* An ascending list is fixed by its members. *)
Lemma ascending_det : forall l1 l2, Ascending l1 -> Ascending l2 ->
  (forall t, In t l1 <-> In t l2) -> l1 = l2.
Proof.
  induction l1 as [|x r1 IH]; intros [|y r2] A1 A2 E.
  - reflexivity.
  - exfalso. exact (proj2 (E y) (or_introl eq_refl)).
  - exfalso. exact (proj1 (E x) (or_introl eq_refl)).
  - assert (Exy : x = y).
    { destruct (proj1 (E x) (or_introl eq_refl)) as [->|Hx]; [reflexivity|].
      destruct (proj2 (E y) (or_introl eq_refl)) as [->|Hy]; [reflexivity|].
      exfalso. exact (Nat.lt_asymm _ _ (ascending_lt r2 y A2 x Hx) (ascending_lt r1 x A1 y Hy)). }
    subst y. f_equal. apply IH; [exact (ascending_tail _ _ A1) | exact (ascending_tail _ _ A2)|].
    intro t. split; intro Ht.
    + destruct (proj1 (E t) (or_intror Ht)) as [Ex|H]; [|exact H].
      exfalso. rewrite <- Ex in Ht. exact (Nat.lt_irrefl _ (ascending_lt r1 x A1 x Ht)).
    + destruct (proj2 (E t) (or_intror Ht)) as [Ex|H]; [|exact H].
      exfalso. rewrite <- Ex in Ht. exact (Nat.lt_irrefl _ (ascending_lt r2 x A2 x Ht)).
Qed.
Lemma sig_outside_det : forall W s e1 e2, SigOutside W s e1 -> SigOutside W s e2 -> e1 = e2.
Proof.
  intros W s e1 e2 [A1 M1] [A2 M2]. apply ascending_det; [exact A1 | exact A2|].
  intro t. rewrite M1, M2. reflexivity.
Qed.
Definition AppOf (W : world) (s : nat) (tm : list (nat * ty)) (a : inst) : Prop :=
  exists rs, a = map (fun r => (r, nlook r tm)) rs /\
    (kind_sym W s <> Some DKMeth -> rs = w_reqs W s) /\
    (kind_sym W s = Some DKMeth -> exists ms extra, rs = map snd (filter fst (combine ms (w_reqs W s))) ++ extra /\
       Forall2 (fun m r => m = true <-> SigMentions W s r) ms (w_reqs W s) /\ SigOutside W s extra).

(* The receiver type of a method item `R.m`: R's meaning if R is a type
   symbol, else R as a nominal at its requirements' meanings. *)
Definition RecvTy (W : world) (r : nat) (tm : list (nat * ty)) (t : ty) : Prop :=
  match nlook r tm with
  | Some t' => t = t'
  | None =>
      match w_decl W r with
      | Some d =>
          match d_ast d with
          | ADAlias _ body => TyOf W (d_mod d) tm None body t
          | _ => exists args, t = TNom r args /\
                   Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) (w_reqs W r) args
          end
      | None => exists args, t = TNom r args /\
                  Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) (w_reqs W r) args
      end
  end.

(* The method a name `.n` on receiver symbol r means in module p's items:
   the attached one, else the detached one in scope ([D61]). *)
Definition MethodNamed (W : world) (p : string) (r : nat) (n : name) (m : nat) : Prop :=
  Attached W r n m \/
  ((forall m', ~ Attached W r n m') /\ w_scope W p NSDetached n = Some (ESym m)).

(* The fixed inputs of a functor ([D84] rule 1), here because assuming an
   abstract functor asks them in force (ItemEntries, note 70). *)

Definition functor_parts (d : adecl) : option (option (list name) * list spec * list spec * option (list (list (spec * option aexpr)))) :=
  match d with ADFunctor _ ps a r b => Some (ps, a, r, b) | _ => None end.

(* The type outputs of F: the type symbols its result lists (through contexts). *)
Definition FunOutputs (W : world) (u : nat) (t : nat) : Prop :=
  exists d ps a r b sp, w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    In sp r /\ SpecTypes W (d_mod d) sp t.

(* M(F) ([D84] rule 1, with DECISIONS.md's correction): the type symbols the
   result items mention — a val's or fn's requirements (its dependencies,
   for a fn its whole telescope), a method's receiver, and every type
   symbol a fn's or method's signature mentions (sigclose, #12b), less
   what brackets bind, plus the brackets' right sides; closed through
   relevance (w_reqs); less F's outputs. *)
Inductive ResultMentions (W : world) : nat -> string -> spec -> nat -> Prop :=
| RMValFn : forall self p pa a s t,
    resolve_sym W p pa = Some s -> (kind_sym W s = Some DKVal \/ kind_sym W s = Some DKFn) ->
    (In t (w_reqs W s) \/ kind_sym W s = Some DKFn /\ SigMentions W s t) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t
| RMReceiver : forall self p pa dn a r t,
    resolve_sym W p pa = Some r ->
    (t = r /\ kind_sym W r = Some DKType \/
     kind_sym W r <> Some DKType /\ In t (w_reqs W r)) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) (Some dn) a) t
| RMMeth : forall self p pa dn a r m t,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    SigMentions W m t ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) (Some dn) a) t
| RMBracket : forall self p pa d a bp sp dr sa t,
    resolve_sym W p pa <> None ->
    In (bp, Spec (Some sp) dr sa) (match a with Some bs => bs | None => [] end) ->
    FreeIn W (w_reqs W) p (ATRef sp sa) t ->
    ResultMentions W self p (Spec (Some pa) d a) t
| RMCtx : forall self p pa a c dc n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some dc -> d_ast dc = ADCtx n its ->
    In (CISpec sp) its -> ResultMentions W self (d_mod dc) sp t ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t
| RMFunctor : forall self p pa a u du n names args res sp t,
    resolve_sym W p pa = Some u -> w_decl W u = Some du ->
    d_ast du = ADFunctor n (Some names) args res None -> u <> self ->
    In sp res -> ResultMentions W u (d_mod du) sp t ->
    ~ FunOutputs W u t ->
    ~ (exists name, In name names /\ resolve_sym W (d_mod du) [name] = Some t) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t.

Definition Mentions (W : world) (u : nat) (t : nat) : Prop :=
  exists d ps a r b sp, w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    In sp r /\ ResultMentions W u (d_mod d) sp t /\ ~ FunOutputs W u t.

(* P(F): the brackets; an unbracketed abstract functor has P(F) = M(F). *)
Definition Poly (W : world) (u : nat) (t : nat) : Prop :=
  exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None /\
    match ps with
    | None => Mentions W u t
    | Some names => exists x, In x names /\ resolve_sym W (d_mod d) [x] = Some t
    end.

(* X(F) = M(F) ∖ P(F), the fixed inputs ([D84] rule 1). *)
Definition Fixed (W : world) (u : nat) (t : nat) : Prop := Mentions W u t /\ ~ Poly W u t.

(* A functor provision retains its fixed inputs as read where provided. *)
Definition FixedReadings (W : world) (u : nat) (tm : list (nat * ty)) (fx : subst) : Prop :=
  forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x tm = Some tx.
Definition fixed_subst (sigma : subst) (fx : subst) : subst :=
  map (fun q => (fst q, app_subst sigma (snd q))) fx.
Definition fixed_app (fx : subst) : inst :=
  map (fun q => (fst q, Some (snd q))) fx.

(* Each of xs in force as its own generation, unless it is already. *)
Definition add_tys (xs : list nat) (tm : list (nat * ty)) : list (nat * ty) :=
  fold_left (fun tm x => match nlook x tm with Some _ => tm | None => (x, TAbs x) :: tm end) xs tm.

(* `ItemEntries W p ob tm sp es tm'`: the context item `sp`, written in
   module p, under the enclosing items' bracket bindings `ob`, introduces
   entries `es`; the type meanings in force grow from tm to tm' (each
   assumed type is its own generation, TAbs t).  An item is read at
   `ob ++ tm`: a bracket binds only while the item it is on is read, and a
   type item is put in force as its own generation even where a bracket
   binds it (note 71).  [D20]: a type the item mentions must already be in
   force or bound (TyOf fails otherwise). *)
Inductive ItemEntries (W : world) : string -> list (nat * ty) -> list (nat * ty) -> spec -> list entry -> list (nat * ty) -> Prop :=
| IEType : forall p ob tm pa t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [NTy t]
                (match nlook t tm with Some _ => tm | None => (t, TAbs t) :: tm end)
| IENothing : forall p ob tm pa s k,
    resolve_sym W p pa = Some s -> kind_sym W s = Some k ->
    (k = DKUnit \/ k = DKTag \/ k = DKAlias \/
     (k = DKVal /\ exists d n t i, w_decl W s = Some d /\ d_ast d = ADVal n t (Some i))) ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [] tm
| IEVal : forall p ob tm a pa s d n t ty0 bs ap,
    resolve_sym W p pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t None ->
    Brackets W p (ob ++ tm) s a bs ->
    TyOf W (d_mod d) (bs ++ ob ++ tm) None t ty0 -> AppOf W s (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) None a) [NVal s ty0 ap] tm
| IEFn : forall p ob tm a pa s g bs ap,
    resolve_sym W p pa = Some s -> kind_sym W s = Some DKFn ->
    Brackets W p (ob ++ tm) s a bs ->
    SigOf W s (bs ++ ob ++ tm) None g -> AppOf W s (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) None a) [NFn s g ap] tm
| IEFun : forall p ob tm pa u d n ps args res fx,
    resolve_sym W p pa = Some u -> w_decl W u = Some d -> d_ast d = ADFunctor n ps args res None ->
    (* [D84] rule 2, [D20]: F is at an instantiation of X(F), so assuming
       it asks X(F) in force (note 70) *)
    (forall x, Fixed W u x -> nlook x tm <> None) ->
    FixedReadings W u tm fx ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [NFun u fx] tm
| IEMeth : forall p ob tm a pa dn r m rt g bs ap,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    Brackets W p (ob ++ tm) m a bs ->
    RecvTy W r (bs ++ ob ++ tm) rt ->
    SigOf W m (bs ++ ob ++ tm) (Some rt) g -> AppOf W m (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) (Some dn) a) [NMth rt m g ap] tm
| IECtx : forall p ob tm a pa c d n its bs es tm',
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    Brackets W p (ob ++ tm) c a bs ->
    (* [D61]: the items of a context are read in the module that wrote them *)
    ItemsEntries W (d_mod d) (bs ++ ob) tm its es tm' ->
    ItemEntries W p ob tm (Spec (Some pa) None a) es tm'
with ItemsEntries (W : world) : string -> list (nat * ty) -> list (nat * ty) -> list citem -> list entry -> list (nat * ty) -> Prop :=
| IsNil : forall p ob tm, ItemsEntries W p ob tm [] [] tm
| IsCons : forall p ob tm sp r es1 tm1 es2 tm2,
    ItemEntries W p ob tm sp es1 tm1 -> ItemsEntries W p ob tm1 r es2 tm2 ->
    ItemsEntries W p ob tm (CISpec sp :: r) (es1 ++ es2) tm2
(* [D90]: an equation names the references its sides write, each read as
   an unbracketed item (a type symbol among them is put in force), and
   merges its two sides *)
| IsEq : forall p ob tm l r es1 tm1 tl tr rest es2 tm2,
    ItemsEntries W p ob tm (map CISpec (ty_heads l ++ ty_heads r)) es1 tm1 ->
    TyOf W p (ob ++ tm1) None l tl -> TyOf W p (ob ++ tm1) None r tr ->
    ItemsEntries W p ob tm1 rest es2 tm2 ->
    ItemsEntries W p ob tm (CIEq l r :: rest) (es1 ++ NEq tl tr :: es2) tm2.

(* An assume item `p` or `R.m` is the context item `p` or `R.m` unbracketed;
   an equation is itself. *)
Definition assume_spec (it : aitem) : citem :=
  match it with
  | AIHead h => CISpec (Spec (Some (fst h)) (snd h) None)
  | AIEq l r => CIEq l r
  end.

(* The key an entry is filed under, read under σ. *)
Definition entry_key (s : subst) (e : entry) : option item :=
  match e with
  | NTy _ => None
  | NVal v _ _ => Some (IVal v)
  | NFn f _ _ => Some (IFn f)
  | NMth r m _ _ => Some (mkey (app_subst s r) m)
  | NFun u _ => Some (IFun u)
  | NEq _ _ => None
  end.

Definition entry_sig (s : subst) (e : entry) : option fsig :=
  match e with
  | NVal _ t _ => Some (FSig [] (app_subst s t))
  | NFn _ g _ | NMth _ _ g _ => Some (sig_subst s g)
  | _ => None
  end.

(* An entry's instantiation, read under σ.  A requirement not in force
   where the item was read (None) is read as itself, as the context's
   freeze completes it (each assumed type is its own generation). *)
Definition inst_read (s : subst) (a : inst) : list (nat * ty) :=
  map (fun q => (fst q, app_subst s (match snd q with Some t => t | None => TAbs (fst q) end))) a.
Definition entry_inst (s : subst) (e : entry) : option (list (nat * ty)) :=
  match e with
  | NVal _ _ a | NFn _ _ a | NMth _ _ _ a => Some (inst_read s a)
  | _ => None
  end.

(* [D43] Merging.  σ *unifies* the entries if any two entries filed under
   one key (read under σ) have one signature and one instantiation under
   σ (Δ4: merging two entries of one item unifies their instantiations).
   The merge of a
   telescope is a most general such σ that is idempotent (no cyclic merge:
   a finding of the mechanization, [D43] calculus note), mapping only
   generations of the telescope.  A merge identifying two distinct
   concrete heads has no unifier, so the telescope does not form. *)
Definition Unifies (s : subst) (es : list entry) : Prop :=
  (forall e1 e2 k, In e1 es -> In e2 es -> entry_key s e1 = Some k -> entry_key s e2 = Some k ->
    entry_sig s e1 = entry_sig s e2 /\ entry_inst s e1 = entry_inst s e2) /\
  (* [D90]: an equation's sides are one type under σ *)
  (forall l r, In (NEq l r) es -> app_subst s l = app_subst s r).

Definition MostGeneralMerge (s : subst) (es : list entry) : Prop :=
  Unifies s es /\
  (forall t, app_subst s (app_subst s t) = app_subst s t) /\
  (forall s', Unifies s' es -> (forall t, app_subst s' (app_subst s t) = app_subst s' t)).

(* The context a merged telescope makes: its tables are the entries read
   under σ.  Instantiations keep None for a requirement not in force where
   the item was written, completed from the context (the elaborator's
   "freeze"). *)
Definition entries_cx (s : subst) (tm : list (nat * ty)) (es : list entry) : cx :=
  let fz (a : inst) : inst :=
    map (fun q => (fst q, option_map (app_subst s)
                            (match snd q with Some t => Some t | None => nlook (fst q) tm end))) a in
  Cx (map (fun q => (fst q, app_subst s (snd q))) tm) s
     (flat_map (fun e => match e with NVal v t _ => [(v, app_subst s t)] | _ => [] end) es)
     (flat_map (fun e => match e with NFn f g _ => [(f, sig_subst s g)] | _ => [] end) es)
     (flat_map (fun e => match e with NMth r m g _ => [((app_subst s r, m), sig_subst s g)] | _ => [] end) es)
     (flat_map (fun e => match e with NFun u fx => [(u, fixed_subst s fx)] | _ => [] end) es)
     (flat_map (fun e => match e, entry_key s e with
                         | NVal _ _ a, Some k | NFn _ _ a, Some k | NMth _ _ _ a, Some k => [(k, fz a)]
                         | NFun u fx, Some k => [(k, fixed_app (fixed_subst s fx))]
                         | _, _ => [] end) es)
     [].

(* The needs of a telescope: its non-type keys, as a set (order is free). *)
Definition Needs (s : subst) (es : list entry) (ns : list item) : Prop :=
  NoDup ns /\ forall k, In k ns <-> exists e, In e es /\ entry_key s e = Some k.

(* [D84] (8), [D37] (note 54): `main` runs in the platform's root context.
   Beside what it assumes, any function in scope as `main` has lib/wasm.moss's
   `Gc`: each abstract functor `Gc` lists that is not already in force,
   after the types it is fixed in (X(F), [D84] rule 1), each in force as
   its own generation unless it is already. *)
Definition IsMainFn (W : world) (s : nat) : Prop :=
  exists d n ps rt body p, w_decl W s = Some d /\ d_ast d = ADFn RPlain n ps rt body /\
    w_scope W p NSName "main" = Some (ESym s).


(* `RootItems W q es0 tm its es tm'`: Gc's items `its` (written in module
   q), after the entries es0 and with meanings tm, add the entries es. *)
Inductive RootItems (W : world) (q : string) : list entry -> list (nat * ty) -> list citem -> list entry -> list (nat * ty) -> Prop :=
| RINil : forall es0 tm, RootItems W q es0 tm [] [] tm
| RIFun : forall es0 tm p a rest u du n ps args res xs fx es tm',
    resolve_sym W q p = Some u -> w_decl W u = Some du -> d_ast du = ADFunctor n ps args res None ->
    ~ (exists fx0, In (NFun u fx0) es0) -> NoDup xs -> (forall x, In x xs <-> Fixed W u x) ->
    FixedReadings W u (add_tys xs tm) fx ->
    RootItems W q (es0 ++ map NTy xs ++ [NFun u fx]) (add_tys xs tm) rest es tm' ->
    RootItems W q es0 tm (CISpec (Spec (Some p) None a) :: rest) (map NTy xs ++ [NFun u fx] ++ es) tm'
| RISkip : forall es0 tm sp rest es tm',
    match sp with
    | CISpec (Spec (Some p) None _) => forall u, resolve_sym W q p = Some u -> kind_sym W u = Some DKFunctor -> exists fx, In (NFun u fx) es0
    | _ => True
    end ->
    RootItems W q es0 tm rest es tm' -> RootItems W q es0 tm (sp :: rest) es tm'.

Inductive RootCtx (W : world) (s : nat) (es0 : list entry) (tm : list (nat * ty)) : list entry -> list (nat * ty) -> Prop :=
| RootNone :
    (~ IsMainFn W s \/ forall gc d n its, LangSym W "wasm" NSName "Gc" gc -> w_decl W gc = Some d -> d_ast d <> ADCtx n its) ->
    RootCtx W s es0 tm [] tm
| RootGc : forall gc d n its es tm',
    IsMainFn W s -> LangSym W "wasm" NSName "Gc" gc -> w_decl W gc = Some d -> d_ast d = ADCtx n its ->
    RootItems W (d_mod d) es0 tm its es tm' -> RootCtx W s es0 tm es tm'.

(* `CtxOf W s C ns`: the context the assume nesting of symbol s forms (and,
   for `main`, the root context: note 54), and its needs (what a caller
   must provide, [D20]). *)
Definition CtxOf (W : world) (s : nat) (C : cx) (ns : list item) : Prop :=
  exists d es tm er tmr σ,
    w_decl W s = Some d /\
    ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) es tm /\
    RootCtx W s es tm er tmr /\
    MostGeneralMerge σ (es ++ er) /\ C = entries_cx σ tmr (es ++ er) /\ Needs σ (es ++ er) ns.

(* ======================================================================== *)
(* V.4 Calls: needs, satisfaction maps and coherence                        *)
(* ======================================================================== *)

(* A need of the callee, read at the call site: a method need's receiver
   is re-read with the caller's type meanings ([D21]: an unapplied
   reference shares the symbols in force). *)
Definition at_site (C : cx) (k : item) : item :=
  match k with
  | IMth r m th => IMth (canon C (app_subst (map (fun q => (fst q, snd q)) (c_ty C)) r)) m th
  | _ => k
  end.

(* θ of a direct call of s from context C (note 27): each of s's
   requirements, in their fixed order (w_reqs), read at the site — its
   meaning in C, or itself where C gives it none — canonically. *)
Definition theta_at (W : world) (C : cx) (s : nat) : subst :=
  map (fun r => (r, canon C (app_subst (c_ty C) (TAbs r)))) (w_reqs W s).

(* [D20]/[D62]/[D71]: a call of callee s (assume context Cs, needs ns)
   from context C is answered by satisfaction map sm: sm sends each need
   (exactly once) to the caller's provision of it read here, which must be
   in force, at the instantiation the callee needs it at (coherence); and
   the callee's merges ([D43]) must hold here. *)
Definition Satisfies (C : cx) (Cs : cx) (ns : list item) (sm : smap) : Prop :=
  (* each need, by its run-time key in the callee, to the caller's
     provision of it read here, by its run-time key here (note 28) *)
  sm = map (fun k => (rkey_of Cs k, rkey_of C (at_site C k))) ns /\
  (forall k, In k ns -> provides C (at_site C k)) /\
  (* coherence: where both say what a requirement means, they agree *)
  (forall k a, In k ns -> ilook k (c_apps Cs) = Some a ->
     forall t want, In (t, Some want) a ->
       match ilook (at_site C k) (c_apps C) with
       | Some have => In (t, Some (canon C (app_subst (c_ty C) want))) have
       | None => True
       end) /\
  (* merges: every equation of the callee's σ holds of the caller's meanings *)
  (forall g t, In (g, t) (c_sub Cs) ->
     (forall x, In x (g :: gens t) -> nlook x (c_ty C) <> None) ->
     canon C (app_subst (c_ty C) (TAbs g)) = canon C (app_subst (c_ty C) t)).

(* Arguments checked against parameter types ([D17] fits), elaborated in order. *)

(* ======================================================================== *)
(* V.5 Functors ([D55], [D78], [D84] with DECISIONS.md)                       *)
(* ======================================================================== *)

(* F's own terms: the context its argument telescope forms, plus its
   implicit mentioned inputs (symbols the explicit inputs do not list). *)
Definition FunCtx (W : world) (u : nat) (C : cx) (ns : list item) : Prop :=
  exists d ps a r b es tm tm' σ implicit,
    w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    ItemsEntries W (d_mod d) [] [] (map CISpec a) es tm /\
    (* implicit inputs: only an abstract functor has them *)
    (forall t, In t implicit <-> (b = None /\ Mentions W u t /\ nlook t tm = None)) /\
    tm' = map (fun t => (t, TAbs t)) implicit ++ tm /\
    MostGeneralMerge σ es /\ C = entries_cx σ tm' es /\ Needs σ es ns.

(* F's result, provided in context C with each output type t read as
   `out t` (a fresh generation at an application): the context grows by
   the result's items, each at the instantiation it is made at, and `ks`
   are its non-type keys. *)
(* Output brackets are bindings at the point an output is installed.  Unlike
   an input telescope application, the output reader permits a binding that
   the target does not mention.  The right-hand types still use [TyOf]. *)
Inductive OutputBrackets (W : world) (p : string) (tm : list (nat * ty)) :
    option (list (path * spec)) -> list (nat * ty) -> Prop :=
| OBrNone : OutputBrackets W p tm None []
| OBrSome : forall bs l,
    Forall2 (fun b tb =>
      exists sp sa, snd b = Spec (Some sp) None sa /\
        resolve_sym W p (fst b) = Some (fst tb) /\
        kind_sym W (fst tb) = Some DKType /\
        TyOf W p tm None (ATRef sp sa) (snd tb)) bs l ->
    OutputBrackets W p tm (Some bs) (rev l).

(* Output installation follows the result list in order.  Type outputs have
   already received their generations; the item walk leaves their meanings
   alone.  A defined function or value is already available and adds no slot. *)
Inductive OutputItemEntries (W : world) : string -> list (nat * ty) ->
    list (nat * ty) -> spec -> list entry -> list (nat * ty) -> Prop :=
| OIEType : forall p ob tm pa a bs t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    OutputBrackets W p (ob ++ tm) a bs ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NTy t] tm
| OIENothing : forall p ob tm pa a bs s k,
    resolve_sym W p pa = Some s -> kind_sym W s = Some k ->
    OutputBrackets W p (ob ++ tm) a bs ->
    (k = DKUnit \/ k = DKTag \/ k = DKAlias \/
      (k = DKVal /\ exists d n t i, w_decl W s = Some d /\ d_ast d = ADVal n t (Some i)) \/
      (k = DKFn /\ exists d r n ps rt b, w_decl W s = Some d /\
         d_ast d = ADFn r n ps rt (Some b))) ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [] tm
| OIEVal : forall p ob tm a pa s d n t ty0 bs ap,
    resolve_sym W p pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    TyOf W (d_mod d) (bs ++ ob ++ tm) None t ty0 ->
    AppOf W s (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NVal s ty0 ap] tm
| OIEFn : forall p ob tm a pa s g bs ap d rcv n ps rt,
    resolve_sym W p pa = Some s -> kind_sym W s = Some DKFn ->
    w_decl W s = Some d -> d_ast d = ADFn rcv n ps rt None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    SigOf W s (bs ++ ob ++ tm) None g -> AppOf W s (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NFn s g ap] tm
| OIEFun : forall p ob tm a bs pa u d n ps args res fx,
    resolve_sym W p pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADFunctor n ps args res None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    (forall x, Fixed W u x -> nlook x (bs ++ ob ++ tm) <> None) ->
    FixedReadings W u (bs ++ ob ++ tm) fx ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NFun u fx] tm
| OIEMeth : forall p ob tm a pa dn r m rt g bs ap,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    OutputBrackets W p (ob ++ tm) a bs -> RecvTy W r (bs ++ ob ++ tm) rt ->
    SigOf W m (bs ++ ob ++ tm) (Some rt) g -> AppOf W m (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) (Some dn) a) [NMth rt m g ap] tm
(* a result's context holds no equation ([D90]) *)
| OIECtx : forall p ob tm a pa c d n its sps bs es tm',
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    its = map CISpec sps ->
    OutputBrackets W p (ob ++ tm) a bs ->
    OutputItemsEntries W (d_mod d) (bs ++ ob) tm sps es tm' ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) es tm'
with OutputItemsEntries (W : world) : string -> list (nat * ty) ->
    list (nat * ty) -> list spec -> list entry -> list (nat * ty) -> Prop :=
| OIsNil : forall p ob tm, OutputItemsEntries W p ob tm [] [] tm
| OIsCons : forall p ob tm sp rest es1 tm1 es2 tm2,
    OutputItemEntries W p ob tm sp es1 tm1 ->
    OutputItemsEntries W p ob tm1 rest es2 tm2 ->
    OutputItemsEntries W p ob tm (sp :: rest) (es1 ++ es2) tm2.

(* Result slots follow the written output occurrences.  Repeated output keys
   remain repeated slots, as in both evaluators; input [Needs] stays set-valued. *)
Definition ResultKeys (s : subst) (es : list entry) (ks : list item) : Prop :=
  ks = flat_map (fun e => match entry_key s e with Some k => [k] | None => [] end) es.

(* Replace the output roles before normalizing through the caller's merges.
   Output provisions and their recorded applications use these completed
   meanings, as [provided_at] freezes the corresponding source readings. *)
Definition output_type_map (C : cx) (outs : list nat) (out : nat -> ty) : list (nat * ty) :=
  map (fun q => (fst q, canon C (snd q))) (map (fun t => (t, out t)) outs ++ c_ty C).

Definition output_cx (C : cx) (tm : list (nat * ty)) (es : list entry) : cx :=
  Cx (map (fun q => (fst q, canon C (snd q))) tm) (c_sub C)
            (flat_map (fun e => match e with NVal v t _ => [(v, canon C t)] | _ => [] end) (rev es) ++ c_vals C)
            (flat_map (fun e => match e with NFn f g _ => [(f, sig_subst (c_sub C) g)] | _ => [] end) (rev es) ++ c_fns C)
            (flat_map (fun e => match e with NMth r m g _ => [((canon C r, m), sig_subst (c_sub C) g)] | _ => [] end) (rev es) ++ c_mths C)
            (flat_map (fun e => match e with NFun u' fx => [(u', fixed_subst (c_sub C) fx)] | _ => [] end) (rev es) ++ c_funs C)
            (flat_map (fun e => match e, entry_key (c_sub C) e with
                                | NVal _ _ a0, Some k | NFn _ _ a0, Some k | NMth _ _ _ a0, Some k => [(k, a0)]
                                | NFun u fx, Some k => [(k, fixed_app (fixed_subst (c_sub C) fx))]
                                | _, _ => [] end) (rev es) ++ c_apps C)
            (c_locals C).

Definition ProvideResult (W : world) (u : nat) (C : cx) (out : nat -> ty) (C' : cx) (ks : list item) : Prop :=
  exists d ps a r b outs es tm',
    w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    (forall t, In t outs <-> FunOutputs W u t) /\
    OutputItemsEntries W (d_mod d) [] (output_type_map C outs out) r es tm' /\
    ResultKeys (c_sub C) es ks /\
    C' = output_cx C tm' es.

(* [D84] rule 7: a family is an abstract functor whose result is method
   items only and holds no type, each item's key determining every P(F)
   symbol it mentions; its bindings bind no vals and apply no functor but
   a family (DECISIONS.md). *)
Inductive FamilyResult (W : world) : string -> spec -> Prop :=
| FRMethod : forall mi pa dn br,
    FamilyResult W mi (Spec (Some pa) (Some dn) br)
| FRContext : forall mi pa br u d n its sps,
    resolve_sym W mi pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADCtx n its -> its = map CISpec sps ->
    Forall (FamilyResult W (d_mod d)) sps ->
    FamilyResult W mi (Spec (Some pa) None br).

Definition FamilyShape (W : world) (u : nat) : Prop :=
  exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None /\
    Forall (FamilyResult W (d_mod d)) r.

Definition FamilyBracketBound (W : world) (p : string)
    (a : option (list (path * spec))) (t : nat) : Prop :=
  exists b, In b (match a with Some bs => bs | None => [] end) /\
    resolve_sym W p (fst b) = Some t.
Definition FamilyBracketFree (W : world) (p : string)
    (a : option (list (path * spec))) (t : nat) : Prop :=
  exists bp pa dn bs, In (bp, Spec (Some pa) dn bs)
    (match a with Some brackets => brackets | None => [] end) /\
    FreeIn W (w_reqs W) p (ATRef pa bs) t.

(* Every polymorphic signature/bracket dependency not replaced by the leaf's
   brackets is determined by its receiver key, as the actual D84 classifier
   checks.  A method-only abstract functor need not be a family. *)
Inductive FamilyKeyed (W : world) (u : nat) : string -> spec -> Prop :=
| FKMethod : forall p pa dn a r m,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    (forall t, Poly W u t ->
       (SigMentions W m t \/ FamilyBracketFree W p a t) ->
       ~ FamilyBracketBound W p a t ->
       FreeIn W (w_reqs W) p (ATRef pa None) t) ->
    FamilyKeyed W u p (Spec (Some pa) (Some dn) a)
| FKContext : forall p pa a c d n its sps,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    its = map CISpec sps ->
    Forall (FamilyKeyed W u (d_mod d)) sps ->
    FamilyKeyed W u p (Spec (Some pa) None a).

Definition FamilyKeys (W : world) (u : nat) : Prop :=
  forall d n ps a r, w_decl W u = Some d -> d_ast d = ADFunctor n ps a r None ->
    Forall (FamilyKeyed W u (d_mod d)) r.
Definition Family (W : world) (u : nat) : Prop :=
  FamilyShape W u /\ FamilyKeys W u.

(* ======================================================================== *)
(* V.6 Bodies: expressions, blocks, binds                                    *)
(* ======================================================================== *)

(* The frame of a body: its module, return type, receiver type (a method),
   whether a `break` has a loop, the numbering of local names (injective;
   parameters, after `this`, are numbered from 0 in order: the runtime
   passes arguments so), and the generations `bind F;` made in it so far
   (a value of their types may not leave the block that made them, [D78]). *)
Record frame := Frame {
  fr_mod : string; fr_ret : ty; fr_this : option ty; fr_loop : bool;
  fr_var : list (name * nat); fr_fresh : list nat;
  fr_applying : list nat;   (* functors being applied by name: [D55] no cycle *)
  fr_pins : list nat        (* in a body bound to F: M(F), which it may not rebind ([D84] 3d) *)
}.

(* a local's number, for a name the numbering covers ([Named]) *)
Definition var_of (F : frame) (n : name) : nat :=
  match slook n (fr_var F) with Some k => k | None => 0 end.
Definition Named (F : frame) (n : name) : Prop := slook n (fr_var F) <> None.
(* the numbering is injective, and numbers the parameters 0, 1, … after `this` *)
Definition Numbering (F : frame) (this : option ty) (params : list name) : Prop :=
  NoDup (map fst (fr_var F)) /\ NoDup (map snd (fr_var F)) /\
  (forall i pn, nth_error params i = Some pn ->
     slook pn (fr_var F) = Some (match this with Some _ => S i | None => i end)).

Definition with_loop (F : frame) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) true (fr_var F) (fr_fresh F) (fr_applying F) (fr_pins F).
Definition with_mod (F : frame) (p : string) (u : nat) : frame :=
  Frame p (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F) (u :: fr_applying F) (fr_pins F).
Definition with_fresh (F : frame) (gs : list nat) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F ++ gs) (fr_applying F) (fr_pins F).

Definition add_local (C : cx) (n : name) (t : ty) (mu : lmut) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) ((n, (t, mu)) :: c_locals C).
Definition set_ty (C : cx) (t : nat) (tau : ty) : cx :=
  Cx ((t, tau) :: c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) (c_locals C).
(* The principal context of a call of s (assume context Cs) from C
   (criteria, principality; Audit #50, E24): each requirement r of s not
   in force in C, whose class in Cs's merge ([D43]) has a member y in
   force in C, reads as y does here, for the whole call: its needs (family
   demands included), its signature and θ.  (Elab.principal_env.)  The
   relation states only that each such reading is a meaning in force
   here; which member, and that it is one of r's class, is the
   elaborator's choice (Audit's CallTheta states the class). *)
Definition PrincipalCx (W : world) (C Cs : cx) (s : nat) (Cp : cx) : Prop :=
  exists pargs : list (nat * ty),
    Cp = fold_left (fun C q => set_ty C (fst q) (snd q)) pargs C /\
    forall r t, In (r, t) pargs -> nlook r (c_ty C) = None /\
      exists y, nlook y (c_ty C) <> None /\ t = canon C (app_subst (c_ty C) (TAbs y)).
Definition provide (C : cx) (k : item) (a : inst) (C1 : cx) : cx :=
  Cx (c_ty C1) (c_sub C1) (c_vals C1) (c_fns C1) (c_mths C1) (c_funs C1) ((k, a) :: c_apps C1) (c_locals C1).
Definition set_val (C : cx) (v : nat) (t : ty) : cx :=
  Cx (c_ty C) (c_sub C) ((v, t) :: c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) (c_locals C).
Definition set_fn (C : cx) (f : nat) (g : fsig) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) ((f, g) :: c_fns C) (c_mths C) (c_funs C) (c_apps C) (c_locals C).
Definition set_mth (C : cx) (r : ty) (m : nat) (g : fsig) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (((r, m), g) :: c_mths C) (c_funs C) (c_apps C) (c_locals C).
Definition set_fun (C : cx) (u : nat) (fx : list (nat * ty)) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) ((u, fx) :: c_funs C) ((IFun u, fixed_app fx) :: c_apps C) (c_locals C).

Definition body_of (P : xprogram) (f : nat) : option xexpr := nlook f (xo_fns P).
Definition funbody_of (P : xprogram) (h : nat) : option (tele * list xbind) := nlook h (xo_funs P).

(* A symbol elaboration makes (a lambda's provider, a forwarder, a wrapper,
   a captured local's val, a generation, a linked functor body) is not a
   source symbol. *)
Definition Fresh (W : world) (s : nat) : Prop := w_decl W s = None /\ s <> 0.

(* The meaning of a decimal literal ([D68]): digits, an optional fraction
   and an optional exponent, as numerator / denominator. *)
Fixpoint digits_val (l : list ascii) (acc : nat) : nat :=
  match l with [] => acc | c :: r => digits_val r (10 * acc + (nat_of_ascii c - 48)) end.

Fixpoint split_at (c : ascii) (l : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match l with
  | [] => (rev acc, None)
  | x :: r => if Ascii.eqb x c then (rev acc, Some r) else split_at c r (x :: acc)
  end.

Definition lit_value (text : string) : nat * nat :=
  let '(mant, ex) := split_at "e"%char (list_ascii_of_string text) [] in
  let '(whole, frac) := split_at "."%char mant [] in
  let fr := match frac with Some f => f | None => [] end in
  let n := digits_val (whole ++ fr) 0 in
  let '(eneg, emag) := match ex with
                       | Some ("-"%char :: d) => (true, digits_val d 0)
                       | Some ("+"%char :: d) => (false, digits_val d 0)
                       | Some d => (false, digits_val d 0)
                       | None => (false, 0) end in
  if eneg then (n, 10 ^ (emag + length fr))
  else if Nat.leb (length fr) emag then (n * 10 ^ (emag - length fr), 1)
  else (n, 10 ^ (length fr - emag)).

Definition is_float_lit (text : string) : bool :=
  existsb (fun c => Ascii.eqb c "."%char || Ascii.eqb c "e"%char) (list_ascii_of_string text).

(* [D84] rule 7 / DECISIONS.md: at most one family in force produces a key.
   Here θ is the matching substitution over P(F), not the full emitted
   application argument list; fixed caller meanings cannot be overridden. *)
Definition FixedAgreement (W : world) (C : cx) (u : nat) : Prop :=
  forall x, Fixed W u x ->
    match ilook (IFun u) (c_apps C), nlook x (c_ty C) with
    | Some recorded, Some now =>
        match nlook x recorded with
        | Some (Some was) => canon C was = canon C now
        | _ => True
        end
    | _, _ => True
    end.

Definition FamilyProduces (W : world) (C : cx) (u : nat) (k : item) : Prop :=
  exists fx th C' ks, nlook u (c_funs C) = Some fx /\ Family W u /\
    Forall (fun q => Poly W u (fst q)) th /\
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) th C) (fun t => TAbs t) C' ks /\
    In k ks.

Definition OnlyFamily (W : world) (C : cx) (u : nat) (k : item) : Prop :=
  forall u', u' <> u -> ~ FamilyProduces W C u' k.

(* A literal's binary arithmetic call uses a provision already in force,
   or a unique family applied in a scope around that call ([D84] rule 7).
   Its one parameter and result are exactly the literal's own type. *)
Inductive LitOp (W : world) (C : cx) (t : ty) (m : nat) (ox oy : xexpr) : xexpr -> Prop :=
| LitOpIn :
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitOp W C t m ox oy (XMeth ox m (rkey C t m) [oy])
| LitOpFamily : forall u fx matching th sm Cu nsu C' ks,
    mlook t m (c_mths C) = None -> nlook u (c_funs C) = Some fx -> Family W u ->
    Forall (fun q => Poly W u (fst q)) matching ->
    FunCtx W u Cu nsu ->
    (forall a, Poly W u a -> nlook a (c_ty Cu) <> None -> exists tt, In (a, tt) th) ->
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here,
       nlook t (c_ty (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)) = Some here /\
       In (t, canon (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) here) th) ->
    FixedAgreement W (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) u ->
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) (fun a => TAbs a) C' ks ->
    In (mkey t m) ks -> OnlyFamily W C u (mkey t m) ->
    Satisfies (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) Cu nsu sm ->
    mlook t m (c_mths C') = Some (FSig [t] t) ->
    LitOp W C t m ox oy (XApply u th sm (XMeth ox m (rkey C' t m) [oy])).

(* [D68] A literal is arithmetic over fixed library symbols: lib/int.moss's
   `zero` and `one` (as provided here), num.moss's `.add`/`.mul`/`.div`
   provided at their type, and lib/float.moss's `float`.  `LitTerm` says a
   term denotes the rational n/d in exact arithmetic; the literal's
   elaboration is *some* such term at the literal's type.  (The draft does
   not fix the term's shape; DRAFT NOTE 6.) *)
Inductive LitTerm (W : world) (C : cx) (t : ty) : xexpr -> nat -> nat -> Prop :=
| LitZero : forall z, LangSym W "int" NSName "zero" z -> nlook z (c_vals C) = Some t ->
    LitTerm W C t (XVl z) 0 1
| LitOne : forall o, LangSym W "int" NSName "one" o -> nlook o (c_vals C) = Some t ->
    LitTerm W C t (XVl o) 1 1
| LitAdd : forall m x y a b c d, LangSym W "num" NSDetached "add" m ->
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t (XMeth x m (rkey C t m) [y]) (a * d + c * b) (b * d)
| LitMul : forall m x y a b c d, LangSym W "num" NSDetached "mul" m ->
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t (XMeth x m (rkey C t m) [y]) (a * c) (b * d)
| LitDiv : forall m x y a b c d, LangSym W "num" NSDetached "div" m ->
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitTerm W C t x a b -> LitTerm W C t y c d -> c <> 0 ->
    LitTerm W C t (XMeth x m (rkey C t m) [y]) (a * d) (b * c)
| LitFloat : forall f ti tp x a b, LangSym W "float" NSName "float" f ->
    nlook f (c_fns C) = Some (FSig [tp] t) -> fits ti tp -> LitTerm W C ti x a b ->
    LitTerm W C t (XCallA f [x]) a b
| LitAddOp : forall m x y result a b c d,
    LangSym W "num" NSDetached "add" m -> LitOp W C t m x y result ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t result (a * d + c * b) (b * d)
| LitMulOp : forall m x y result a b c d,
    LangSym W "num" NSDetached "mul" m -> LitOp W C t m x y result ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t result (a * c) (b * d)
| LitDivOp : forall m x y result a b c d,
    LangSym W "num" NSDetached "div" m -> LitOp W C t m x y result ->
    LitTerm W C t x a b -> LitTerm W C t y c d -> c <> 0 ->
    LitTerm W C t result (a * d) (b * c).

(* Operators ([D68], [D77] rule 2): fixed to num.moss's detached methods;
   the `for.` operators of [D85]'s `for` sugar (Parse.for_desugar) to
   access.moss's (note 60).  The library module's short name, and the
   method's name in it. *)
Definition operator_method (op : string) (unary : bool) : option (string * string) :=
  if unary then
    match op with
    | "-" => Some ("num", "neg") | "!" => Some ("num", "not")
    | "for.begin" => Some ("access", "begin") | _ => None
    end
  else
    match op with
    | "*" => Some ("num", "mul") | "/" => Some ("num", "div") | "%" => Some ("num", "rem")
    | "+" => Some ("num", "add") | "-" => Some ("num", "sub") | "<<" => Some ("num", "shl")
    | ">>" => Some ("num", "shr") | "==" => Some ("num", "eq") | "!=" => Some ("num", "ne")
    | "<" => Some ("num", "lt") | ">" => Some ("num", "gt") | "<=" => Some ("num", "le")
    | ">=" => Some ("num", "ge") | "&" => Some ("num", "and") | "^" => Some ("num", "xor")
    | "|" => Some ("num", "or")
    | "for.more" => Some ("access", "more") | "for.get" => Some ("access", "get")
    | "for.after" => Some ("access", "after")
    | _ => None
    end.

(* the payload type of a tag at its arguments *)
Inductive PayloadOf (W : world) : nat -> list (nat * ty) -> ty -> Prop :=
| PayTag : forall s xs d n ps t pay,
    w_decl W s = Some d -> d_ast d = ADTag n ps t -> TyOf W (d_mod d) xs None t pay ->
    PayloadOf W s xs pay.

(* The fields of a record pattern `P{…}` against the payload's fields
   `ftys`, in the order written: `x` binds the field x to a local x, `x =
   y` to a local y, `x = _` binds nothing (note 35). *)
Inductive FieldPats (W : world) (F : frame) (ftys : list (nat * ty))
    : list (name * option apat) -> list (name * ty) -> list (fld * var) -> Prop :=
| FPNil : FieldPats W F ftys [] [] []
| FPPun : forall n t r bs xs,
    nlook (w_fld W n) ftys = Some t -> Named F n -> FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, None) :: r) ((n, t) :: bs) ((w_fld W n, var_of F n) :: xs)
| FPWild : forall n t r bs xs,
    nlook (w_fld W n) ftys = Some t -> FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, Some APWild) :: r) bs xs
| FPBind : forall n y t r bs xs,
    nlook (w_fld W n) ftys = Some t -> resolve W (fr_mod F) [y] = None -> Named F y ->
    FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, Some (APPath [y])) :: r) ((y, t) :: bs) ((w_fld W n, var_of F y) :: xs).

(* The pattern judgment: a source pattern, in frame F, against scrutinee
   type `st` compiles to an xpat, binding locals; `hs` are the heads it
   covers and `catch` whether it matches everything.  A head names the one
   member of st with that head ([D15], note 33). *)
Inductive PatOf (W : world) (F : frame) (st : ty) : apat -> xpat -> list (name * ty) -> list nat -> bool -> Prop :=
| PWild : PatOf W F st APWild (XPAny None) [] [] true
| PBinder : forall n,
    resolve W (fr_mod F) [n] = None -> Named F n ->
    PatOf W F st (APPath [n]) (XPAny (Some (var_of F n))) [(n, st)] [] true
| PUnit : forall pa s k xs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some k -> (k = DKUnit \/ k = DKTag) ->
    In (TNom s xs) (members st) ->
    PatOf W F st (APPath pa) (XPHead s None []) [] [s] false
(* `S(y)`: y is the payload, at the member's arguments; a struct tag's is a
   bare record type (note 35) *)
| PTagBind : forall pa s xs y pay,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag -> In (TNom s xs) (members st) ->
    PayloadOf W s xs pay ->
    resolve W (fr_mod F) [y] = None -> Named F y ->
    PatOf W F st (APTag pa (APPath [y])) (XPHead s (Some (var_of F y)) []) [(y, pay)] [s] false
(* `S{x, x = y, x = _}`: S's payload is a record, its fields bound by name *)
| PRecord : forall pa s xs fs ftys bs fxs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag -> In (TNom s xs) (members st) ->
    PayloadOf W s xs (TNom rec_head ftys) ->
    FieldPats W F ftys fs bs fxs ->
    PatOf W F st (APRec (Some pa) fs) (XPHead s None fxs) bs [s] false.

(* `let` binds an immutable local, `var` an assignable one. *)
Inductive LetStmt : astmt -> name -> option aty -> aexpr -> lmut -> Prop :=
| LSLet : forall n a y, LetStmt (ASLet n a y) n a y LLet
| LSVar : forall n a y, LetStmt (ASVar n a y) n a y LVar.

(* Whether a `break` in an expression, block or statement leaves the
   innermost loop around it: one not inside a nested `while` or `loop`
   (note 42). *)
Inductive Breaks : aexpr -> Prop :=
| BkBreak : Breaks AEBreak
| BkCall : forall p a args y, In y args -> Breaks y -> Breaks (AECall p a args)
| BkField : forall o n, Breaks o -> Breaks (AEField o n)
| BkMethRecv : forall o p args, Breaks o -> Breaks (AEMeth o p args)
| BkMethArg : forall o p args y, In y args -> Breaks y -> Breaks (AEMeth o p args)
| BkRec : forall p a fs n y, In (n, Some y) fs -> Breaks y -> Breaks (AERec p a fs)
| BkIfC : forall c b e, Breaks c -> Breaks (AEIf c b e)
| BkIfB : forall c b e, BlockBreaks b -> Breaks (AEIf c b e)
| BkIfE : forall c b y, Breaks y -> Breaks (AEIf c b (Some y))
| BkMatchS : forall s arms, Breaks s -> Breaks (AEMatch s arms)
| BkMatchA : forall s arms pt y, In (pt, y) arms -> Breaks y -> Breaks (AEMatch s arms)
| BkReturn : forall y, Breaks y -> Breaks (AEReturn (Some y))
| BkBlock : forall b, BlockBreaks b -> Breaks (AEBlock b)
| BkUnary : forall op y, Breaks y -> Breaks (AEUnary op y)
| BkBinL : forall op l r, Breaks l -> Breaks (AEBinary op l r)
| BkBinR : forall op l r, Breaks r -> Breaks (AEBinary op l r)
| BkLam : forall ps r y, Breaks y -> Breaks (AELam ps r y)
with BlockBreaks : ablock -> Prop :=
| BBStmt : forall ss tl s, In s ss -> StmtBreaks s -> BlockBreaks (ABlock ss tl)
| BBTail : forall ss y, Breaks y -> BlockBreaks (ABlock ss (Some y))
with StmtBreaks : astmt -> Prop :=
| SBLet : forall n t y, Breaks y -> StmtBreaks (ASLet n t y)
| SBVar : forall n t y, Breaks y -> StmtBreaks (ASVar n t y)
| SBAssign : forall n y, Breaks y -> StmtBreaks (ASAssign n y)
| SBExpr : forall y, Breaks y -> StmtBreaks (ASExpr y)
| SBBind : forall its sp y, In (sp, Some y) its -> Breaks y -> StmtBreaks (ASBind its).

(* The type a block has as a branch of an `if` or a `match` arm: its
   tail's, and `()` without a tail, even if it cannot fall through
   (note 42). *)
Definition branch_ty (b : ablock) (t : ty) : ty :=
  match b with ABlock _ None => TUnit | ABlock _ (Some _) => t end.

(* Auxiliary judgments (not mutually recursive with the main ones). *)

Inductive Join : ty -> ty -> option ty -> ty -> Prop :=
| JoinExp : forall a b t, Join a b (Some t) t
| JoinLeftNever : forall b, Join never b None b
| JoinSame : forall a b, (b = never \/ a = b) -> a <> never -> Join a b None a.

(* D78 compares raw signature leaves, recursively through contexts. Types
   remain leaves; units, tags, aliases, and concrete values/functions do
   not require a provision. Brackets affect later interpretations, not
   this preliminary leaf-identity check. *)
Definition result_already_provided (d : adecl) : bool :=
  match d with
  | ADUnit _ | ADTag _ _ _ | ADAlias _ _ => true
  | ADVal _ _ (Some _) | ADFn RPlain _ _ _ (Some _) => true
  | _ => false
  end.
Inductive ResultSpecLeaf (W : world) : string -> spec -> (nat * option name) -> Prop :=
| RSLMethod : forall p pa dn br s,
    resolve_sym W p pa = Some s ->
    ResultSpecLeaf W p (Spec (Some pa) (Some dn) br) (s,Some dn)
| RSLPlain : forall p pa br s d,
    resolve_sym W p pa = Some s -> w_decl W s = Some d ->
    (forall n its, d_ast d <> ADCtx n its) ->
    result_already_provided (d_ast d) = false ->
    ResultSpecLeaf W p (Spec (Some pa) None br) (s,None)
| RSLContext : forall p pa br c d n its sp k,
    resolve_sym W p pa = Some c -> w_decl W c = Some d ->
    d_ast d = ADCtx n its -> In sp (flat_map citem_specs its) ->
    ResultSpecLeaf W (d_mod d) sp k ->
    ResultSpecLeaf W p (Spec (Some pa) None br) k.
Inductive ResultLeaf (W : world) : nat -> (nat * option name) -> Prop :=
| RL : forall u d n ps a r b sp k,
    w_decl W u = Some d -> d_ast d = ADFunctor n ps a r b ->
    In sp r -> ResultSpecLeaf W (d_mod d) sp k -> ResultLeaf W u k.

(* [D78]/[D84] 3e: G's result is F's, leaf by leaf. *)
Definition SameResult (W : world) (u g : nat) : Prop :=
  forall k, ResultLeaf W u k <-> ResultLeaf W g k.

(* Restore fresh output generations to their declared symbols when filing
   the reusable code body. Input meanings remain those of the target. *)
Definition output_renaming (outs : list nat) (gen : nat -> nat) (k : item) : item :=
  app_subst_item (map (fun t => (gen t, TAbs t)) outs) k.

(* a method bind, filed also under each key it answers as another spelling
   (pairs: the body's key, the key as declared) *)
Definition refile (pairs : list (item * item)) (b : xbind) : list xbind :=
  match b with
  | XBMth t m th f sm =>
      b :: flat_map (fun p => match p with
                              | (IMth t' m' _, IMth t0 _ th0) =>
                                  if ty_eqb t t' && Nat.eqb m m' && negb (item_eqb (IMth t0 m th0) (IMth t m th))
                                  then [XBMth t0 m th0 f sm] else []
                              | _ => [] end) pairs
  | _ => [b]
  end.

Inductive BindsOf : xexpr -> list xbind -> Prop :=
| BOEnd : BindsOf XUnit []
| BOTy : forall t tau x r, BindsOf x r -> BindsOf (XBindTy t tau x) (XBTy t tau :: r)
| BOVal : forall v e x r, BindsOf x r -> BindsOf (XBindVal v e x) (XBVal v e :: r)
| BOFn : forall f g sm x r, BindsOf x r -> BindsOf (XBindFn f g sm x) (XBFn f g sm :: r)
| BOMth : forall t m th f sm x r, BindsOf x r -> BindsOf (XBindMth t m th f sm x) (XBMth t m th f sm :: r)
| BOFun : forall u h sm smc x r, BindsOf x r -> BindsOf (XBindFun u h sm smc x) (XBFun u h sm smc :: r)
| BOFunA : forall u h sm x r, BindsOf x r -> BindsOf (XBindFunA u h sm x) (XBFunA u h sm :: r)
| BOApply : forall u th sm x r, BindsOf x r -> BindsOf (XApply u th sm x) (XBApply u th sm :: r).

Inductive Mentions_name : aexpr -> name -> Prop :=
| MNPath : forall n, Mentions_name (AEPath [n] None) n
| MNSub : forall e e' n, Sub_expr e' e -> Mentions_name e' n -> Mentions_name e n

with Sub_expr : aexpr -> aexpr -> Prop :=
| SubCallArg : forall p a args y, In y args -> Sub_expr y (AECall p a args)
| SubField : forall o n, Sub_expr o (AEField o n)
| SubMethRecv : forall o p args, Sub_expr o (AEMeth o p args)
| SubMethArg : forall o p args y, In y args -> Sub_expr y (AEMeth o p args)
| SubRec : forall p a fs n y, In (n, Some y) fs -> Sub_expr y (AERec p a fs)
| SubRecPun : forall p a fs n, In (n, None) fs -> Sub_expr (AEPath [n] None) (AERec p a fs)
| SubIfC : forall c b e, Sub_expr c (AEIf c b e)
| SubIfB : forall c ss tl e y, (exists st, In st ss /\ Stmt_sub y st) \/ tl = Some y -> Sub_expr y (AEIf c (ABlock ss tl) e)
| SubIfE : forall c b y, Sub_expr y (AEIf c b (Some y))
| SubMatchS : forall s arms, Sub_expr s (AEMatch s arms)
| SubMatchA : forall s arms pt y, In (pt, y) arms -> Sub_expr y (AEMatch s arms)
| SubReturn : forall y, Sub_expr y (AEReturn (Some y))
| SubBlockT : forall ss y, Sub_expr y (AEBlock (ABlock ss (Some y)))
| SubBlockS : forall ss tl s y, In s ss -> Stmt_sub y s -> Sub_expr y (AEBlock (ABlock ss tl))
| SubUnary : forall op y, Sub_expr y (AEUnary op y)
| SubBinL : forall op l r, Sub_expr l (AEBinary op l r)
| SubBinR : forall op l r, Sub_expr r (AEBinary op l r)
| SubLam : forall ps r y, Sub_expr y (AELam ps r y)

with Stmt_sub : aexpr -> astmt -> Prop :=
| SSLet : forall n t y, Stmt_sub y (ASLet n t y)
| SSVar : forall n t y, Stmt_sub y (ASVar n t y)
| SSAssign : forall n y, Stmt_sub y (ASAssign n y)
| SSAssignName : forall n y, Stmt_sub (AEPath [n] None) (ASAssign n y)
| SSExpr : forall y, Stmt_sub y (ASExpr y)
| SSWhileC : forall c b, Stmt_sub c (ASWhile c b)
| SSWhileB : forall c b, Stmt_sub (AEBlock b) (ASWhile c b)
| SSLoop : forall b, Stmt_sub (AEBlock b) (ASLoop b)
| SSBind : forall its sp y, In (sp, Some y) its -> Stmt_sub y (ASBind its).

(* Automatic family demand checks an initial missing method key, then applies
   its unique family in the context extended by preceding demands.  Type
   arguments are temporary; a family provides methods only. *)
Definition family_restore (C Cr : cx) : cx :=
  Cx (c_ty C) (c_sub Cr) (c_vals Cr) (c_fns Cr) (c_mths Cr)
     (c_funs Cr) (c_apps Cr) (c_locals Cr).


Inductive FamilyStep (W : world) (C0 : cx) (wanted : list item)
    : cx -> nat -> subst -> subst -> smap -> cx -> Prop :=
| FamilyStepApply : forall C ot m u fx matching th sm Cu nsu Cr ks,
    In (mkey ot m) wanted -> mlook ot m (c_mths C0) = None ->
    nlook u (c_funs C0) = Some fx -> nlook u (c_funs C) = Some fx ->
    Family W u -> OnlyFamily W C0 u (mkey ot m) ->
    Forall (fun q => Poly W u (fst q)) matching ->
    FunCtx W u Cu nsu ->
    (forall t, Poly W u t -> nlook t (c_ty Cu) <> None -> exists tt, In (t, tt) th) ->
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here,
       nlook t (c_ty (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)) = Some here /\
       In (t, canon (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) here) th) ->
    FixedAgreement W (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) u ->
    Satisfies (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) Cu nsu sm ->
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)
       (fun t => TAbs t) Cr ks ->
    In (mkey ot m) ks ->
    FamilyStep W C0 wanted C u matching th sm (family_restore C Cr).

(* Group identity is the inferred matching substitution.  Distinct groups
   may emit equal complete type arguments after reading the other inputs. *)
Inductive Demand (W : world) (C0 : cx) (wanted : list item)
    : list (nat * subst) -> cx -> (xexpr -> xexpr) -> cx -> Prop :=
| DemandDone : forall used C,
    Demand W C0 wanted used C (fun x => x) C
| DemandApply : forall used C u matching th sm Cm wrap Cf,
    ~ In (u, matching) used -> FamilyStep W C0 wanted C u matching th sm Cm ->
    Demand W C0 wanted ((u, matching) :: used) Cm wrap Cf ->
    Demand W C0 wanted used C (fun x => XApply u th sm (wrap x)) Cf.

(* D89: the bracket application on a call of the fn f: each binding names
   a type symbol among f's requirements and binds it to a type by name. *)
Definition CallBrackets (W : world) (p : string) (f : nat) (bs : list (path * spec)) : Prop :=
  Forall (fun b => (exists x, resolve_sym W p (fst b) = Some x /\ kind_sym W x = Some DKType /\
                              In x (w_reqs W f)) /\
                   exists rp ra, snd b = Spec (Some rp) None ra) bs.

(* Main judgments.
   `Ex W P F C e exp x t`: in frame F and context C, the expression e,
   checked against the expected type `exp` if any, elaborates to x of type t.
   `Blk W P F C ss tl exp x t`: a block (statements ss, optional tail).
   `Call W P F C callee args x t` etc. are its pieces. *)
Inductive Ex (W : world) (P : xprogram) : frame -> cx -> aexpr -> option ty -> xexpr -> ty -> Prop :=
| ExUnit : forall F C exp, Ex W P F C AEUnit exp XUnit TUnit
| ExThis : forall F C exp t, fr_this F = Some t -> Ex W P F C AEThis exp (XVar 0) t
| ExLocal : forall F C exp n t mu,
    slook n (c_locals C) = Some (t, mu) -> Named F n ->
    Ex W P F C (AEPath [n] None) exp (XVar (var_of F n)) t
| ExUnitTag : forall F C exp pa s,
    (forall n, pa = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKUnit ->
    Ex W P F C (AEPath pa None) exp (XTag s [] XUnit) (TNom s [])
| ExVal : forall F C exp pa s d n t0 t,
    (forall n', pa = [n'] -> slook n' (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t0 None ->
    nlook s (c_vals C) = Some t ->
    Ex W P F C (AEPath pa None) exp (XVl s) t
(* a defined val (no assumptions yet, [D50]) is its initializer, inlined,
   elaborated in an empty context *)
| ExConst : forall F C exp pa s d n t0 i t x,
    (forall n', pa = [n'] -> slook n' (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t0 (Some i) ->
    d_assumes d = [] -> TyOf W (d_mod d) [] None t0 t ->
    (exists F0 t', Ex W P F0 (Cx [] [] [] [] [] [] [] []) i (Some t) x t' /\ fits t' t /\
                   fr_mod F0 = d_mod d) ->
    Ex W P F C (AEPath pa None) exp (XConst s x) t
(* a tag applied to its payload *)
| ExTag : forall F C exp pa a args s bs xs pay y v ty,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) s a bs ->
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) (w_reqs W s) xs ->
    PayloadOf W s xs pay ->
    args = [y] -> Ex W P F C y (Some pay) v ty -> fits ty pay ->
    Ex W P F C (AECall pa a args) exp (XTag s xs v) (TNom s xs)
(* [D20]/[D21] a defined fn: its needs answered from here (sm), its
   signature read with the meanings here *)
| ExCallD : forall F C exp pa args f Cf ns sm g xs Cp,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f Cf ns -> PrincipalCx W C Cf f Cp -> Satisfies Cp Cf ns sm ->
    SigOf W f (c_ty Cp) None g ->
    (* Q4: the arguments in the caller's context, as at every call *)
    Args W P F C args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp (XCallD f (theta_at W Cp f) sm xs) (sg_ret g)
(* Defined calls may first demand missing methods from unique families.
   Their arguments are checked in the caller's context (Q4), as at a
   defined attached call. *)
| ExCallDDemand : forall F C exp pa args f Cf ns sm g xs wrap C' Cp,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f Cf ns -> PrincipalCx W C Cf f Cp ->
    Demand W Cp (map (at_site Cp) ns) [] Cp wrap C' ->
    Satisfies C' Cf ns sm -> SigOf W f (c_ty C') None g ->
    (* Q4: the arguments in the caller's context, without the demands *)
    Args W P F C args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp
       (wrap (XCallD f (theta_at W C' f) sm xs)) (sg_ret g)
(* an abstract fn: whatever provides it here *)
| ExCallA : forall F C exp pa args f g xs,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt, ast_of W f = Some (ADFn RPlain n ps rt None)) ->
    nlook f (c_fns C) = Some g ->
    Args W P F C args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp (XCallA f xs) (sg_ret g)
(* D89: a fn applied to brackets is the call under those binds, its
   arguments evaluated first (Syntax.call_block) *)
| ExCallBr : forall F C exp pa bs args f x t,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    CallBrackets W (fr_mod F) f bs ->
    Ex W P F C (Syntax.call_block pa bs args) None x t ->
    Ex W P F C (AECall pa (Some bs) args) exp x t
| ExMeth : forall F C exp o pa args ox ot x t,
    Ex W P F C o None ox ot ->
    MethCall W P F C ox (canon C ot) pa args x t ->
    Ex W P F C (AEMeth o pa args) exp x t
(* [D68] operators: the receiver is the (left) operand ([D85]: and the
   `for.` operators, note 60) *)
| ExUnary : forall F C exp op y lib n m ox ot x t,
    operator_method op true = Some (lib, n) -> LangSym W lib NSDetached n m ->
    Ex W P F C y None ox ot -> head (canon C ot) <> None ->
    Provided W P F C ox (canon C ot) m [] x t ->
    Ex W P F C (AEUnary op y) exp x t
| ExBinary : forall F C exp op l r lib n m ox ot x t,
    operator_method op false = Some (lib, n) -> LangSym W lib NSDetached n m ->
    Ex W P F C l None ox ot -> head (canon C ot) <> None ->
    Provided W P F C ox (canon C ot) m [r] x t ->
    Ex W P F C (AEBinary op l r) exp x t
(* [D68] literals *)
| ExLit : forall F C exp text x t num den,
    (* its type: `one`'s, `zero`'s (`0` needs only `zero`: note 63), or a float's *)
    ((exists o, LangSym W "int" NSName "one" o /\ nlook o (c_vals C) = Some t) \/
     (exists z, LangSym W "int" NSName "zero" z /\ nlook z (c_vals C) = Some t) \/
     is_float_lit text = true) ->
    lit_value text = (num, den) ->
    LitTerm W C t x num den ->
    Ex W P F C (AELit text) exp x t
| ExIfThen : forall F C exp c b cx' bx bt,
    Cond W P F C c cx' -> BlkE W P F C b exp bx bt ->
    opt_fits TUnit exp -> (bt = TUnit \/ bt = never) ->
    Ex W P F C (AEIf c b None) exp (XIf cx' bx XUnit) TUnit
| ExIfElse : forall F C exp c b y cx' bx bt ex' et t,
    Cond W P F C c cx' -> BlkE W P F C b exp bx bt ->
    Branch W P F C y exp ex' et -> Join (branch_ty b bt) et exp t ->
    Ex W P F C (AEIf c b (Some y)) exp (XIf cx' bx ex') t
| ExMatch : forall F C exp s arms sx st xarms t,
    Ex W P F C s None sx st ->
    Arms W P F C (canon C st) arms exp xarms t ->
    Ex W P F C (AEMatch s arms) exp (XMatchP sx xarms) t
| ExBlock : forall F C exp b x t,
    BlkE W P F C b exp x t -> Ex W P F C (AEBlock b) exp x (match exp with Some e => e | None => t end)
| ExReturnUnit : forall F C exp,
    fr_ret F = TUnit ->
    Ex W P F C (AEReturn None) exp (XReturn XUnit) never
| ExReturn : forall F C exp e v t',
    Ex W P F C e (Some (fr_ret F)) v t' -> fits t' (fr_ret F) ->
    Ex W P F C (AEReturn (Some e)) exp (XReturn v) never
| ExBreak : forall F C exp, fr_loop F = true -> Ex W P F C AEBreak exp XBreak never
| ExRecord : forall F C exp pa a fs s bs xs pay ftys xfs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) s a bs ->
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) (w_reqs W s) xs ->
    PayloadOf W s xs pay -> pay = TNom rec_head ftys ->
    (* every field given once, in any order; the record holds them in the payload's order *)
    NoDup (map fst fs) -> length fs = length ftys ->
    Forall2 (fun ft xf => fst xf = fst ft /\
               exists n v vt, w_fld W n = fst ft /\ In (n, v) fs /\
                 Ex W P F C (match v with Some y => y | None => AEPath [n] None end) (Some (snd ft)) (snd xf) vt /\
                 fits vt (snd ft)) ftys xfs ->
    Ex W P F C (AERec pa a fs) exp (XTag s xs (XRec xfs)) (TNom s xs)
| ExField : forall F C exp o n ox ot rec t,
    Ex W P F C o None ox ot ->
    (* a tag's payload, or a bare record (note 44) *)
    ((exists s xs, ot = TNom s xs /\ kind_sym W s = Some DKTag /\ PayloadOf W s xs rec) \/ rec = ot) ->
    (exists ftys, rec = TNom rec_head ftys /\ nlook (w_fld W n) ftys = Some t) ->
    Ex W P F C (AEField o n) exp (XField ox (w_fld W n)) t

(* the payload type of a tag at its arguments *)

with Args (W : world) (P : xprogram) : frame -> cx -> list aexpr -> list ty -> list xexpr -> Prop :=
| ArgsNil : forall F C, Args W P F C [] [] []
| ArgsCons : forall F C a r t ts x xs at',
    Ex W P F C a (Some t) x at' -> fits (canon C at') (canon C t) ->
    Args W P F C r ts xs -> Args W P F C (a :: r) (t :: ts) (x :: xs)

(* [D45] a condition is a Bool (lib/bool.moss's) *)
with Cond (W : world) (P : xprogram) : frame -> cx -> aexpr -> xexpr -> Prop :=
| CondBool : forall F C c x t b,
    LangSym W "bool" NSName "Bool" b ->
    Ex W P F C c (Some (TNom b [])) x t -> fits t (TNom b []) -> Cond W P F C c x

(* a branch of an `if` (its `else`) or a match arm: a block has its
   branch type, or the expected one (note 42) *)
with Branch (W : world) (P : xprogram) : frame -> cx -> aexpr -> option ty -> xexpr -> ty -> Prop :=
| BranchBlock : forall F C b exp x t,
    BlkE W P F C b exp x t ->
    Branch W P F C (AEBlock b) exp x (match exp with Some e => e | None => branch_ty b t end)
| BranchExpr : forall F C y exp x t,
    (forall b, y <> AEBlock b) -> Ex W P F C y exp x t -> Branch W P F C y exp x t

(* branches agree, or the expected type decides *)

with Arms (W : world) (P : xprogram) : frame -> cx -> ty -> list (apat * aexpr) -> option ty -> list (xpat * xexpr) -> ty -> Prop :=
| ArmsEmptyNever : forall F C exp, Arms W P F C never [] exp [] (match exp with Some e => e | None => never end)
| ArmsSome : forall F C st arms exp xarms covered catch t,
    st <> never ->
    ArmsList W P F C st arms exp xarms covered catch t ->
    (* exhaustive: a catch-all arm, or every member's head covered *)
    (catch = true \/ (members st <> [] /\ forall m h, In m (members st) -> head m = Some h -> In h covered)) ->
    Arms W P F C st arms exp xarms t

with ArmsList (W : world) (P : xprogram) : frame -> cx -> ty -> list (apat * aexpr) -> option ty -> list (xpat * xexpr) -> list nat -> bool -> ty -> Prop :=
| ArmsOne : forall F C st pat body exp xp binds hs ca bx bt,
    PatOf W F st pat xp binds hs ca ->
    Branch W P F (fold_left (fun C q => add_local C (fst q) (snd q) LLet) binds C) body exp bx bt ->
    opt_fits bt exp ->
    ArmsList W P F C st [(pat, body)] exp [(xp, bx)] hs ca bt
| ArmsMore : forall F C st pat body rest exp xp binds hs ca bx bt xr hr cr tr t,
    rest <> [] ->
    PatOf W F st pat xp binds hs ca ->
    Branch W P F (fold_left (fun C q => add_local C (fst q) (snd q) LLet) binds C) body exp bx bt ->
    opt_fits bt exp ->
    ArmsList W P F C st rest exp xr hr cr tr ->
    Join bt tr exp t ->
    ArmsList W P F C st ((pat, body) :: rest) exp ((xp, bx) :: xr) (hs ++ hr) (ca || cr) t

(* [D61]/[D77] which method a call `o.p(args)` means.  `ot` is the
   receiver's canonical type. *)
with MethCall (W : world) (P : xprogram) : frame -> cx -> xexpr -> ty -> path -> list aexpr -> xexpr -> ty -> Prop :=
(* `o.mod::m(...)`: the detached method reached through its module *)
| MCQualified : forall F C ox ot pa args m x t,
    length pa > 1 -> resolve_detached W (fr_mod F) pa = Some m ->
    Provided W P F C ox ot m args x t ->
    MethCall W P F C ox ot pa args x t
(* [D77] rule 4: the provision of the detached name in scope wins over an
   attached method, wherever the context provides it at the receiver *)
| MCProvisionWins : forall F C ox ot n args h att m x t,
    head ot = Some h -> Attached W h n att ->
    w_scope W (fr_mod F) NSDetached n = Some (ESym m) -> mlook ot m (c_mths C) <> None ->
    Provided W P F C ox ot m args x t ->
    MethCall W P F C ox ot [n] args x t
(* [D61] rule 1, a defined attached method: a direct call, at the
   receiver's instantiation ([D74]) *)
| MCAttachedDefined : forall F C ox ot n args h att Ca ns sm g xs rargs Cp,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
    rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
    CtxOf W att Ca ns ->
    PrincipalCx W (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) Ca att Cp ->
    Satisfies Cp Ca ns sm ->
    SigOf W att (c_ty Cp) (Some ot) g ->
    Args W P F C args (sg_params g) xs ->
    MethCall W P F C ox ot [n] args (XCallD att (theta_at W Cp att) sm (ox :: xs)) (sg_ret g)
(* Defined attached calls can demand methods after installing receiver arguments.
   Their source arguments are still checked in the original caller context. *)
| MCAttachedDefinedDemand : forall F C ox ot n args h att Ca ns sm g xs rargs wrap Cd Cp,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
    rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
    CtxOf W att Ca ns ->
    PrincipalCx W (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) Ca att Cp ->
    Demand W Cp (map (at_site Cp) ns) [] Cp wrap Cd ->
    Satisfies Cd Ca ns sm -> SigOf W att (c_ty Cd) (Some ot) g ->
    Args W P F C args (sg_params g) xs ->
    MethCall W P F C ox ot [n] args
      (wrap (XCallD att (theta_at W Cd att) sm (ox :: xs))) (sg_ret g)
(* an abstract attached method: a key the context provides ([D36]) *)
| MCAttachedAbstract : forall F C ox ot n args h att x t,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt, ast_of W att = Some (ADFn (RAttached rn) n ps rt None)) ->
    Provided W P F C ox ot att args x t ->
    MethCall W P F C ox ot [n] args x t
(* [D61] rule 2: no attached method; the detached name in scope *)
| MCDetached : forall F C ox ot n args m x t,
    (forall h att, head ot = Some h -> ~ Attached W h n att) ->
    w_scope W (fr_mod F) NSDetached n = Some (ESym m) ->
    Provided W P F C ox ot m args x t ->
    MethCall W P F C ox ot [n] args x t

(* [D62] a call of method m on a receiver of type ot: the context's one
   provision at key (ot, m), whatever it is bound to *)
with Provided (W : world) (P : xprogram) : frame -> cx -> xexpr -> ty -> nat -> list aexpr -> xexpr -> ty -> Prop :=
| PrIn : forall F C ox ot m args g xs,
    mlook ot m (c_mths C) = Some g ->
    Args W P F C args (sg_params g) xs ->
    Provided W P F C ox ot m args (XMeth ox m (rkey C ot m) xs) (sg_ret g)
(* [D84] rule 7: no provision here, but a family F in force produces the
   key at θ (read off the key: θ instantiates P(F) so that a result item's
   receiver becomes ot); the call is elaborated as if preceded by `bind F;`
   at θ, in a scope of its own: `XApply F θ sm` around the call.  No search:
   at most one family in a context produces a key (DECISIONS.md, rule 7). *)
| PrFamily : forall F C ox ot m args u fx matching th sm Cu nsu C' ks g xs,
    mlook ot m (c_mths C) = None ->
    nlook u (c_funs C) = Some fx -> Family W u ->
    Forall (fun q => Poly W u (fst q)) matching ->
    FunCtx W u Cu nsu ->
    (* θ covers the polymorphic inputs actually present in F's own context. *)
    (forall t, Poly W u t -> nlook t (c_ty Cu) <> None -> exists tt, In (t, tt) th) ->
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here,
       nlook t (c_ty (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)) = Some here /\
       In (t, canon (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) here) th) ->
    FixedAgreement W (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) u ->
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) (fun t => TAbs t) C' ks ->
    In (mkey ot m) ks ->
    (* uniqueness: no other family in force produces this key *)
    OnlyFamily W C u (mkey ot m) ->
    Satisfies (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) Cu nsu sm ->
    mlook ot m (c_mths C') = Some g ->
    Args W P F C args (sg_params g) xs ->
    Provided W P F C ox ot m args (XApply u th sm (XMeth ox m (rkey C' ot m) xs)) (sg_ret g)

(* a block whose type is checked against `exp` and may not let a value of
   a generation it made escape ([D78]) *)
with BlkE (W : world) (P : xprogram) : frame -> cx -> ablock -> option ty -> xexpr -> ty -> Prop :=
| BlkECheck : forall F C ss tl exp x t made,
    Blk W P F C ss tl exp x t made ->
    (forall g, In g made -> ~ In g (gens t)) ->
    opt_fits t exp ->
    BlkE W P F C (ABlock ss tl) exp x t

(* `Blk ... x t made`: `made` are the generations its binds made *)
with Blk (W : world) (P : xprogram) : frame -> cx -> list astmt -> option aexpr -> option ty -> xexpr -> ty -> list nat -> Prop :=
| BlkTail : forall F C y exp x t, Ex W P F C y exp x t -> Blk W P F C [] (Some y) exp x t []
| BlkEmpty : forall F C exp, Blk W P F C [] None exp XUnit TUnit []
| BlkLet : forall F C st n ann y rest tl exp declared v vt t0 xr t made mu,
    LetStmt st n ann y mu ->
    match ann with
    | Some a => exists d, TyOf W (fr_mod F) (c_ty C) (fr_this F) a d /\ declared = Some d
    | None => declared = None
    end ->
    Ex W P F C y declared v vt ->
    (match declared with Some d => fits vt d /\ t0 = d | None => t0 = vt end) ->
    Named F n ->
    Blk W P F (add_local C n t0 mu) rest tl exp xr t made ->
    Blk W P F C (st :: rest) tl exp (XLet (var_of F n) v xr) t made
| BlkAssign : forall F C n y rest tl exp tv v vt xr t made,
    slook n (c_locals C) = Some (tv, LVar) ->
    Ex W P F C y (Some tv) v vt -> fits vt tv ->
    Blk W P F C rest tl exp xr t made ->
    Named F n ->
    Blk W P F C (ASAssign n y :: rest) tl exp (XSeq (XAssign (var_of F n) v) xr) t made
| BlkExprLast : forall F C y exp v vt,
    Ex W P F C y None v vt ->
    Blk W P F C [ASExpr y] None exp (XSeq v XUnit) (if ty_eqb vt never then never else TUnit) []
| BlkExpr : forall F C y rest tl exp v vt xr t made,
    (rest <> [] \/ tl <> None) ->
    Ex W P F C y None v vt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASExpr y :: rest) tl exp (XSeq v xr) t made
| BlkWhile : forall F C c b rest tl exp cx' bx bt xr t made,
    Cond W P F C c cx' -> BlkE W P (with_loop F) C b None bx bt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASWhile c b :: rest) tl exp (XSeq (XWhile cx' bx) xr) t made
| BlkLoop : forall F C b rest tl exp bx bt xr t made,
    (rest <> [] \/ tl <> None \/ BlockBreaks b) ->
    BlkE W P (with_loop F) C b None bx bt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASLoop b :: rest) tl exp (XSeq (XLoop bx) xr) t made
(* a block whose last statement is a `loop` no `break` leaves cannot fall
   through (note 42) *)
| BlkLoopLast : forall F C b exp bx bt,
    BlkE W P (with_loop F) C b None bx bt -> ~ BlockBreaks b ->
    Blk W P F C [ASLoop b] None exp (XSeq (XLoop bx) XUnit) never []
(* a bind statement: its items in order, each wrapping the rest *)
| BlkBind : forall F C its rest tl exp wrap C' F' xr t made made',
    BindItems W P F C its wrap C' F' made ->
    Blk W P F' C' rest tl exp xr t made' ->
    Blk W P F C (ASBind its :: rest) tl exp (wrap xr) t (made ++ made')

(* `BindItems ... wrap C' F' made`: a run of bind items elaborates to the
   binds `wrap` puts around whatever follows them. *)
with BindItems (W : world) (P : xprogram) : frame -> cx -> list (spec * option aexpr) -> (xexpr -> xexpr) -> cx -> frame -> list nat -> Prop :=
| BINil : forall F C, BindItems W P F C [] (fun x => x) C F []
| BICons : forall F C it r w1 C1 F1 m1 w2 C2 F2 m2,
    BindItem W P F C it w1 C1 F1 m1 -> BindItems W P F1 C1 r w2 C2 F2 m2 ->
    BindItems W P F C (it :: r) (fun x => w1 (w2 x)) C2 F2 (m1 ++ m2)

with BindItem (W : world) (P : xprogram) : frame -> cx -> (spec * option aexpr) -> (xexpr -> xexpr) -> cx -> frame -> list nat -> Prop :=
(* [D72] `bind T = τ;` starts a new generation: from here T means τ (read
   here); what was typed before keeps the old meaning.  Nothing at run time.
   [D84] 3d: in a body bound to F, a type of M(F) may not be rebound. *)
| BTypeBind : forall F C pa t rp ra tau,
    resolve_sym W (fr_mod F) pa = Some t -> kind_sym W t = Some DKType ->
    ~ In t (fr_pins F) ->
    TyOf W (fr_mod F) (c_ty C) None (ATRef rp ra) tau ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath rp ra)) (XBindTy t (canon C tau))
             (set_ty C t (canon C tau)) F []
(* `bind v = e;` *)
| BValBind : forall F C pa v d n t0 i declared y x xt a,
    resolve_sym W (fr_mod F) pa = Some v -> w_decl W v = Some d -> d_ast d = ADVal n t0 i ->
    TyOf W (d_mod d) (c_ty C) None t0 declared ->
    Ex W P F C y (Some declared) x xt -> fits xt declared ->
    AppOf W v (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some y) (XBindVal v x)
             (provide C (IVal v) a (set_val C v declared)) F []
(* `bind f = g;`: g a defined fn whose signature, read here, is f's *)
| BFnBind : forall F C pa f g sg pg sm a,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None sg ->
    FnProvider W P F C (AEPath pg None) sg None g sm ->
    AppOf W f (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (XBindFn f g sm)
             (provide C (IFn f) a (set_fn C f sg)) F []
(* [D70] `bind f = |ps| body;`: see LambdaProvider *)
| BFnLambda : forall F C pa f sg ps rt body pid pre sm a,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None sg ->
    LambdaProvider W P F C ps rt body sg None pid pre sm ->
    AppOf W f (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some (AELam ps rt body)) (fun x => pre (XBindFn f pid sm x))
             (provide C (IFn f) a (set_fn C f sg)) F []
(* [D61] `bind R.m[brackets] = provider;`: the key is R read here (under
   the brackets), the provider an attached defined method of R's head, a
   defined fn (eta-expanded to take the receiver, [D54]), or a lambda *)
| BMethBind : forall F C pa dn a y r m bs rt sg target pre sm ap,
    resolve_sym W (fr_mod F) pa = Some r -> MethodNamed W (fr_mod F) r dn m ->
    Brackets W (fr_mod F) (c_ty C) m a bs ->
    TyOf W (fr_mod F) (bs ++ c_ty C) None (ATRef pa None) rt ->
    SigOf W m (bs ++ c_ty C) (Some rt) sg ->
    MethProvider W P F C y sg rt target pre sm ->
    AppOf W m (bs ++ c_ty C) ap ->
    BindItem W P F C (Spec (Some pa) (Some dn) a, Some y)
             (fun x => pre (XBindMth (canon C rt) m (theta_of C ap) target sm x))
             (provide C (mkey (canon C rt) m) ap (set_mth C (canon C rt) m sg)) F []
(* [D55] `bind F;` for F with a body: F's binds, here, read in F's module *)
| BApplyBody : forall F C pa u d n ps a r bodies wrap C' F' made,
    resolve_sym W (fr_mod F) pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADFunctor n ps a r (Some bodies) -> ~ In u (fr_applying F) ->
    BindItems W P (with_mod F (d_mod d) u) C (concat bodies) wrap C' F' made ->
    BindItem W P F C (Spec (Some pa) None None, None) wrap C'
             (Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F') (fr_applying F) (fr_pins F)) made
(* [D78]/[D84] rule 5, `bind F;` for F abstract and in force: an
   application at θ = F's inputs read here; each type output already in
   force here becomes a fresh rigid generation ([D72]); F's fixed inputs
   X(F) must denote what they did where F was provided, else a static error;
   exactly F's result is then in force, its items at the application's
   generations. *)
| BApplyAbstract : forall F C pa u fx Cu ns sm th gensof C' ks newgens,
    resolve_sym W (fr_mod F) pa = Some u -> nlook u (c_funs C) = Some fx ->
    FunCtx W u Cu ns ->
    (* inputs in force here, and θ reads them *)
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here, nlook t (c_ty C) = Some here /\
        In (t, canon C here) th) ->
    (* rule 5: fixed inputs unchanged since the provision *)
    (forall x tx, In (x, tx) fx -> exists here,
      nlook x (c_ty C) = Some here /\ canon C here = canon C tx) ->
    Satisfies C Cu ns sm ->
    (* outputs: fresh where the symbol is already in force *)
    (forall t, FunOutputs W u t ->
       (nlook t (c_ty C) = None /\ gensof t = t) \/
       (nlook t (c_ty C) <> None /\ Fresh W (gensof t) /\ In (gensof t) newgens /\ In (t, TAbs (gensof t)) th)) ->
    ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
    BindItem W P F C (Spec (Some pa) None None, None) (XApply u th sm) C' (with_fresh F newgens) newgens
(* [D78]/[D84] rule 4: `bind F = H;`, H abstract in force: H's inputs
   are among F's, and F's binding is H's *)
| BFunCopy : forall F C pa u pg h Cu nsu Ch nsh sm fx,
    resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some h ->
    nlook h (c_funs C) <> None ->
    FunCtx W u Cu nsu -> FunCtx W h Ch nsh -> incl nsh nsu ->
    (* Copying requires the target fixed inputs now and preserves every
       known fixed interpretation carried by the existing source binding. *)
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    (forall x, Fixed W h x -> Fixed W u x) ->
    FixedAgreement W C h ->
    SameResult W u h ->
    sm = map (fun k => (rkey_of Ch k, rkey_of Cu k)) nsh ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (XBindFunA u h sm) (set_fun C u fx) F []
(* [D84] rule 3, `bind F = G;`, G with a body: a closure.  G's inputs
   that F takes are linked (supplied by each application), the rest are
   captured here (vals by value) and must be in force here; G's body is
   checked once, in its own terms, as the core functor body h (LinkBody). *)
| BFunClosure : forall F C pa u pg gg Cu nsu Cg nsg h sm smc fx,
    resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some gg ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) ->
    (exists n ps a r b, ast_of W gg = Some (ADFunctor n ps a r (Some b))) ->
    FunCtx W u Cu nsu -> FunCtx W gg Cg nsg ->
    (* 3a: X(F) in force here *)
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    SameResult W u gg ->
    LinkBody W P u gg h ->
    sm = map (fun k => (rkey_of Cg k, rkey_of Cu k)) (filter (fun k => existsb (item_eqb k) nsu) nsg) ->
    smc = map (fun k => (rkey_of Cg k, rkey_of C (at_site C k))) (filter (fun k => negb (existsb (item_eqb k) nsu)) nsg) ->
    (forall k k', In (k, k') smc -> provides C k') ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (XBindFun u h sm smc) (set_fun C u fx) F []

(* [D66] a fn provider: a defined fn (its needs answered here), or an
   abstract fn in force, through a fresh defined forwarder whose body calls
   it; its signature read here must be the bound fn's. *)
with FnProvider (W : world) (P : xprogram) : frame -> cx -> aexpr -> fsig -> option ty -> nat -> smap -> Prop :=
| FPDefined : forall F C pa g sg Cg ns sm,
    resolve_sym W (fr_mod F) pa = Some g -> kind_sym W g = Some DKFn ->
    (exists n ps rt b, ast_of W g = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W g Cg ns -> Satisfies C Cg ns sm ->
    SigOf W g (c_ty C) None (FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg))) ->
    FnProvider W P F C (AEPath pa None) sg None g sm
| FPForward : forall F C pa g sg w n,
    resolve_sym W (fr_mod F) pa = Some g -> kind_sym W g = Some DKFn ->
    (exists nm ps rt, ast_of W g = Some (ADFn RPlain nm ps rt None)) ->
    nlook g (c_fns C) = Some sg -> n = length (sg_params sg) ->
    Fresh W w -> body_of P w = Some (XCallA g (map XVar (seq 0 n))) ->
    FnProvider W P F C (AEPath pa None) sg None w [(IFn g, IFn g)]

(* a method provider (the receiver is the provider's first argument) *)
with MethProvider (W : world) (P : xprogram) : frame -> cx -> aexpr -> fsig -> ty -> nat -> (xexpr -> xexpr) -> smap -> Prop :=
| MPAttached : forall F C rp n rg att rt sg Ca ns sm,
    resolve_sym W (fr_mod F) rp = Some rg -> Attached W rg n att ->
    (exists rn ps r b, ast_of W att = Some (ADFn (RAttached rn) n ps r (Some b))) ->
    head rt = Some rg ->
    CtxOf W att Ca ns -> Satisfies C Ca ns sm ->
    SigOf W att (c_ty C) None
      (FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg))) ->
    MethProvider W P F C (AEField (AEPath rp None) n) sg rt att (fun x => x) sm
| MPWrapped : forall F C pa sg rt g sm w n Cg nsg,
    FnProvider W P F C (AEPath pa None) sg None g sm ->
    CtxOf W g Cg nsg -> n = length (sg_params sg) ->
    Fresh W w ->
    body_of P w = Some (XCallD g (theta_at W Cg g) (map (fun k => (rkey_of Cg k, rkey_of Cg k)) nsg) (map XVar (seq 1 n))) ->
    MethProvider W P F C (AEPath pa None) sg rt w (fun x => x) sm
| MPLambda : forall F C ps r body sg rt pid pre sm,
    LambdaProvider W P F C ps r body sg (Some rt) pid pre sm ->
    MethProvider W P F C (AELam ps r body) sg rt pid pre sm

(* [D70] an anonymous function at a bind: its parameters are the bound
   signature's (annotations, if any, must agree); each local of the
   enclosing body it mentions is captured by value — a fresh val bound here
   to the local's value; the function is a fresh defined fn assuming
   everything in force here (with the captured vals), its body read with
   the captured names as those vals (`let x = v;` in front); its needs are
   answered by the same items here. *)
with LambdaProvider (W : world) (P : xprogram) : frame -> cx -> list (name * option aty) -> option aty -> aexpr -> fsig -> option ty -> nat -> (xexpr -> xexpr) -> smap -> Prop :=
| LP : forall F C ps rt body sg this pid caps capvals Cl Fl lbody ltype keys sm,
    length ps = length (sg_params sg) -> NoDup (map fst ps) ->
    Forall2 (fun pq t => match snd pq with
                         | Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ canon C t' = canon C t
                         | None => True end) ps (sg_params sg) ->
    match rt with Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ canon C t' = canon C (sg_ret sg)
                 | None => True end ->
    (* captured: the enclosing locals it mentions, less its parameters *)
    (forall n, In n caps <-> (Mentions_name body n /\ slook n (c_locals C) <> None /\ ~ In n (map fst ps) /\ fr_applying F = [])) ->
    NoDup caps -> length capvals = length caps -> Forall (Fresh W) capvals -> NoDup capvals ->
    Fresh W pid ->
    (* the provider's own context: everything here, the captured vals in
       force (as LCaptured locals: not assignable), no other locals *)
    Cl = fold_left (fun C' q => let '(n, v) := q in
                      match slook n (c_locals C) with
                      | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                      | None => C' end)
                   (combine caps capvals)
                   (Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) []) ->
    (* its frame: numbered afresh, parameters (after `this`) from 0 *)
    fr_mod Fl = fr_mod F -> fr_ret Fl = sg_ret sg -> fr_this Fl = this -> fr_loop Fl = false ->
    Numbering Fl this (map fst ps) -> Forall (Named Fl) caps -> Forall (Named F) caps ->
    BlkE W P Fl (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
                           (combine (map fst ps) (sg_params sg)) Cl)
         (ABlock [] (Some body)) (Some (sg_ret sg)) lbody ltype ->
    fits ltype (sg_ret sg) ->
    body_of P pid = Some (fold_right (fun q x => XLet (var_of Fl (fst q)) (XVl (snd q)) x) lbody
                                    (combine caps capvals)) ->
    (* its needs: every key in force here, each answered by itself *)
    (forall k, In k keys <-> (provides Cl k /\
       (match k with ITy _ => False | _ => True end) /\ k = provision_id k)) ->
    NoDup keys -> sm = map (fun k => (rkey_of Cl k, rkey_of C k)) keys ->
    LambdaProvider W P F C ps rt body sg this pid
      (fun x => fold_right (fun q x => XBindVal (snd q) (XVar (var_of F (fst q))) x) x (combine caps capvals))
      sm

(* [D78]/[D84] 3e: G's result is F's, item by item: the same leaves
   (keys, as written, through contexts). *)

(* [D84] 3b–3e: G's binds checked once, in G's own terms — G's context
   formed from its own inputs, each output of F as itself — producing the
   core functor body h: its result telescope (F's output types, then F's
   result items), and its binds.  3b: no captured item mentions a type
   that applications supply; 3d: pins may not rebind M(F); 3e: every item
   of F's result is made by the binds, at the types F promises. *)
with LinkBody (W : world) (P : xprogram) : nat -> nat -> nat -> Prop :=
| LB : forall u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 binds,
    w_decl W gg = Some dg -> d_ast dg = ADFunctor n ps a r (Some bodies) ->
    FunCtx W u Cu nsu -> FunCtx W gg Cg nsg ->
    (* 3b: a captured value may mention fixed or privately captured types,
       but no target input type supplied afresh by each application. *)
    (forall k t, In k nsg -> ~ In k nsu ->
       match k with
       | IVal v => forall vt, nlook v (c_vals Cg) = Some vt -> In t (gens vt) ->
           nlook t (c_ty Cu) <> None -> Fixed W u t
       | _ => True end) ->
    fr_mod Fg = d_mod dg -> fr_pins Fg = [] \/ (forall x, In x (fr_pins Fg) <-> Mentions W u x) ->
    BindItems W P Fg Cg (concat bodies) wrap Cb Fb made ->
    (forall t, In t outs <-> FunOutputs W u t) ->
    NoDup (map gen outs) ->
    (forall t, In t outs -> Fresh W (gen t)) ->
    ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks ->
    (* 3e: each result key is provided by the body *)
    (forall k, In k ks -> provides Cb (at_site Cb (output_renaming outs gen k))) ->
    (* h's body, as a run of binds, is what `wrap` puts around nothing *)
    BindsOf (wrap XUnit) binds0 ->
    (* a result method item is filed under its key as F declares it too
       (receiver and θ in F's terms, note 28) *)
    binds = flat_map (refile (map (fun k =>
      (at_site Cb (output_renaming outs gen k),
       output_renaming outs gen (rkey_of Cr k))) ks)) binds0 ->
    Fresh W h ->
    funbody_of P h = Some (map ITy outs ++ map (fun k => output_renaming outs gen (rkey_of Cr k)) ks, binds) ->
    LinkBody W P u gg h.

(* ======================================================================== *)
(* V.7 Programs                                                              *)
(* ======================================================================== *)

(* A defined fn or method f: its body, under the context its assumes form
   (and, for an attached method, the receiver as `this` at its
   requirements' generations), with the parameters numbered from 0 (after
   `this`), elaborates to `body_of P f`. *)
Definition FnElab (W : world) (P : xprogram) (f : nat) : Prop :=
  forall d rcv n ps rt b,
    w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some b) ->
  exists C ns this g F x t,
    CtxOf W f C ns /\
    (match rcv with
     | RAttached _ => exists r args, Attached W r n f /\ this = Some (TNom r args) /\
                        Forall2 (fun x xa => fst xa = x /\ nlook x (c_ty C) = Some (snd xa)) (w_reqs W r) args
     | _ => this = None end) /\
    SigOf W f (c_ty C) this g /\
    fr_mod F = d_mod d /\ fr_ret F = sg_ret g /\ fr_this F = this /\ fr_loop F = false /\
    fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
    Numbering F this (map fst ps) /\
    BlkE W P F (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine (map fst ps) (sg_params g)) C)
         b (Some (sg_ret g)) x t /\
    body_of P f = Some x.

(* [D79]/platform: main's initial environment.  Each need of main must be
   something the platform provides: lib/wasm.moss's and lib/wasip1.moss's
   fns are primitives (`prim_of_name`, Part IV), wasistd's `i32_zero`/
   `i32_one` the I32 constants, and an abstract functor of those modules a
   platform binding (DRAFT NOTE 9). *)
Definition platform_mod (W : world) (p : string) : Prop :=
  LibraryModule W "wasm" p \/ LibraryModule W "wasip1" p \/ LibraryModule W "wasistd" p.

Inductive PlatformEntry (W : world) (P : xprogram) : item -> xdentry -> Prop :=
| PEZero : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_zero") -> PlatformEntry W P (IVal v) (XDVal (XVI32 0%uint63))
| PEOne : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_one") -> PlatformEntry W P (IVal v) (XDVal (XVI32 1%uint63))
| PEPrim : forall f d n ps rt body pr, w_decl W f = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFn RPlain n ps rt body -> prim_of_name n = Some pr ->
    PlatformEntry W P (IFn f) (XDPrim pr)
| PEUnsupported : forall f d n ps rt body i, w_decl W f = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFn RPlain n ps rt body -> prim_of_name n = None -> nth_error (xo_prims P) i = Some n ->
    PlatformEntry W P (IFn f) (XDPrim (PUnsupported i))
| PEFun : forall u d n ps a r h, w_decl W u = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFunctor n ps a r None -> funbody_of P h <> None ->
    PlatformEntry W P (IFun u) (XDFun h [] []).

(* The runtime type prefix is a representation of the source context,
   separate from the values and code supplied by the platform. Its six
   machine heads are fresh and selected from the actual library scope. *)
Definition initial_machine_names : list name :=
 ["I32";"I64";"F32";"F64";"StructRef";"ArrayRef"].
Definition initial_machine_subst(W:world)(base:nat):subst:=
 flat_map(fun q=>match w_library W with
 |None=>[]
 |Some dir=>match w_scope W(String.append dir "wasm.moss")NSName(snd q)with
   |Some(ESym symbol)=>[(symbol,TNom(base+fst q)[])]
   |_=>[]end end)(combine(seq 0 6)initial_machine_names).
Definition InitialTypes(W:world)(C:cx)(types:subst):Prop:=
 exists base,(forall slot,slot<6->Fresh W(base+slot))/\
 forall key,nlook key types=option_map(app_subst(initial_machine_subst W base))(nlook key(c_ty C)).

(* The relation.  [D16]: every defined fn is elaborated, reached or not. *)
Definition ElaboratesIn (b : bundle) (W : world) (P : xprogram) : Prop :=
  WorldOf b W /\
    (forall f d rcv n ps rt body, w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) ->
       FnElab W P f) /\
    (* main: the entry's `main`, a defined fn assuming only what the
       platform provides *)
    w_scope W (b_entry b) NSName "main" = Some (ESym (xo_main P)) /\
    (exists d n ps rt body, w_decl W (xo_main P) = Some d /\ d_ast d = ADFn RPlain n ps rt (Some body)) /\
    (exists C ns types platform, CtxOf W (xo_main P) C ns /\ InitialTypes W C types /\
       xo_env P = xinst [] types ++ platform /\ map fst platform = map (rkey_of C) ns /\
       Forall (fun ie => PlatformEntry W P (fst ie) (snd ie)) platform) /\
    (* Bool's symbols, when lib/bool.moss is loaded ([D45]) *)
    (LangSym W "bool" NSName "Bool" (xo_bool P) /\ LangSym W "bool" NSName "True" (xo_true P) /\
     LangSym W "bool" NSName "False" (xo_false P)
     \/ ((forall s, ~ LangSym W "bool" NSName "Bool" s) /\ xo_bool P = 0 /\ xo_true P = 0 /\ xo_false P = 0)).

Definition Elaborates (b : bundle) (P : xprogram) : Prop :=
  exists W, ElaboratesIn b W P.

End Elaboration.


(* The platform's source signatures. These checks relate library declarations
   to the machine operations supplied at linking, using the same world as
   elaboration. A matching name alone is insufficient. *)
Module Platform.
Import Input Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition MachineType (W : world) (name : string) (t : ty) : Prop :=
  exists symbol, LangSym W "wasm" NSName name symbol /\
    kind_sym W symbol = Some DKType /\ t = TAbs symbol.
Definition NominalType (W : world) (library name : string) (t : ty) : Prop :=
  exists symbol args, LangSym W library NSName name symbol /\ t = TNom symbol args.

Definition ValueType (W : world) (t : ty) (shape : xty) : Prop :=
  match shape with
  | XTI32 => MachineType W "I32" t
  | XTI64 => MachineType W "I64" t
  | XTF32 => MachineType W "F32" t
  | XTF64 => MachineType W "F64" t
  | XTUnit => t = TUnit
  | XTBool => exists symbol, LangSym W "bool" NSName "Bool" symbol /\ t = TNom symbol []
  | XTAddr => NominalType W "wasm" "Addr" t
  | XTTag => exists symbol args, t = TNom symbol args
  end.
Definition Signature (W : world) (params : list ty) (result : ty)
    (signature : option (list xty * xty)) : Prop :=
  exists inputs output, signature = Some (inputs, output) /\
    Forall2 (ValueType W) params inputs /\ ValueType W result output.

Definition ReferenceType (W : world) (array : bool) (t held : ty) : Prop :=
  exists symbol parameter args,
    LangSym W "wasm" NSName (if array then "Array" else "Ref") symbol /\
    LangSym W "wasm" NSName (if array then "Arrayed" else "Boxed") parameter /\
    t = TNom symbol args /\ nlook parameter args = Some held.

Definition GCSignature (W : world) (operation : prim) (params : list ty)
    (result : ty) : Prop :=
  match operation, params with
  | PBox symbol, [held] => LangSym W "wasm" NSName "Ref" symbol /\ ReferenceType W false result held
  | PDeref, [reference] => ReferenceType W false reference result
  | PRefSet, [reference; held] => ReferenceType W false reference held /\ result = TUnit
  | PArrNew symbol, [held; count] => LangSym W "wasm" NSName "Array" symbol /\
      MachineType W "I32" count /\ ReferenceType W true result held
  | PArrLen, [array] => MachineType W "I32" result /\ exists held, ReferenceType W true array held
  | PArrGet, [array; index] => MachineType W "I32" index /\ ReferenceType W true array result
  | PArrSet, [array; index; held] => MachineType W "I32" index /\
      ReferenceType W true array held /\ result = TUnit
  | PArrFill, [array; index; held; count] => MachineType W "I32" index /\
      MachineType W "I32" count /\ ReferenceType W true array held /\ result = TUnit
  | PArrCopy, [dst; di; src; si; count] => MachineType W "I32" di /\
      MachineType W "I32" si /\ MachineType W "I32" count /\ result = TUnit /\
      exists held, ReferenceType W true dst held /\ ReferenceType W true src held
  | _, _ => False
  end.

Definition PrimitiveSignature (W : world) (operation : prim)
    (params : list ty) (result : ty) : Prop :=
  match operation with
  (* These operations never return a value, so impose no result type. *)
  | PUnsupported _ => True
  | PUnreachable => params = []
  | _ => Signature W params result (prim_sig operation) \/
      Signature W params result (prim_msig operation) \/ GCSignature W operation params result
  end.

Definition AtKey (W : world) (C : cx) (key : item) (operation : prim) : Prop :=
  match key with
  | IFn f => exists signature, nlook f (c_fns C) = Some signature /\
      PrimitiveSignature W operation (sg_params signature) (sg_ret signature)
  | IMth receiver method _ => exists signature,
      mlook receiver method (c_mths C) = Some signature /\
      PrimitiveSignature W operation (receiver :: sg_params signature) (sg_ret signature)
  | _ => False
  end.

(* The platform families provide GC methods or methods of Addr. *)
Definition MethodPrimitive (W : world) (key : item) (operation : prim) : Prop :=
  exists receiver method theta d form name params result body,
    key = IMth receiver method theta /\ w_decl W method = Some d /\
    d_ast d = ADFn form name params result body /\
    match form with
    | RDetached =>
        (name = "boxed" /\ exists symbol, LangSym W "wasm" NSName "Ref" symbol /\ operation = PBox symbol) \/
        (name = "arrayed" /\ exists symbol, LangSym W "wasm" NSName "Array" symbol /\ operation = PArrNew symbol)
    | RAttached path => exists symbol, resolve_sym W (d_mod d) [path] = Some symbol /\
        ((LangSym W "wasm" NSName "Ref" symbol /\
            ((name = "deref" /\ operation = PDeref) \/ (name = "set" /\ operation = PRefSet))) \/
         (LangSym W "wasm" NSName "Array" symbol /\
            ((name = "length" /\ operation = PArrLen) \/ (name = "get" /\ operation = PArrGet) \/
             (name = "set" /\ operation = PArrSet) \/ (name = "fill" /\ operation = PArrFill) \/
             (name = "copy" /\ operation = PArrCopy))) \/
         (LangSym W "wasm" NSName "Addr" symbol /\ prim_of_name name = Some operation))
    | RPlain => False
    end.

Definition Family (W : world) (P : xprogram) (source code : usym) : Prop :=
  exists inputs needs outputs keys bindings,
    FunCtx W source inputs needs /\
    ProvideResult W source inputs (fun t => TAbs t) outputs keys /\
    funbody_of P code = Some (keys, bindings) /\
    Forall2 (fun key binding => exists operation,
      binding = XBPrim key operation /\ MethodPrimitive W key operation /\
      AtKey W outputs key operation) keys bindings.

Definition Entry (W : world) (P : xprogram) (C : cx)
    (key : item) (value : xdentry) : Prop :=
  match key, value with
  | IVal value, XDVal (XVI32 _) => exists t,
      nlook value (c_vals C) = Some t /\ MachineType W "I32" t
  | IFn _, XDPrim operation => AtKey W C key operation
  | IFun source, XDFun code [] [] => Family W P source code
  | _, _ => False
  end.

(* These source types have fixed runtime representations. Check their
   declarations as well as the signatures that mention them. *)
Definition abstract_names : list string :=
  ["I32"; "I64"; "F32"; "F64"; "StructRef"; "ArrayRef"; "Memory"; "Boxed"; "Arrayed"].
Definition TagLayout (W : world) (name parameter handle : string) : Prop :=
  forall symbol, LangSym W "wasm" NSName name symbol ->
  exists param raw rest,
    LangSym W "wasm" NSName parameter param /\
    LangSym W "wasm" NSName handle raw /\ kind_sym W symbol = Some DKTag /\
    w_reqs W symbol = param :: rest /\
    PayloadOf W symbol (map (fun t => (t, TAbs t)) (w_reqs W symbol)) (TAbs raw).
Definition Layouts (W : world) : Prop :=
  (forall name symbol, In name abstract_names -> LangSym W "wasm" NSName name symbol ->
    kind_sym W symbol = Some DKType) /\
  (forall left right symbol, In left abstract_names -> In right abstract_names ->
    LangSym W "wasm" NSName left symbol -> LangSym W "wasm" NSName right symbol -> left = right) /\
  TagLayout W "Ref" "Boxed" "StructRef" /\
  TagLayout W "Array" "Arrayed" "ArrayRef" /\
  TagLayout W "Addr" "Memory" "I32" /\
  (forall symbol, LangSym W "bool" NSName "Bool" symbol ->
    exists truth falsity,
      LangSym W "bool" NSName "True" truth /\ LangSym W "bool" NSName "False" falsity /\
      kind_sym W symbol = Some DKTag /\ kind_sym W truth = Some DKUnit /\
      kind_sym W falsity = Some DKUnit /\ truth <> falsity /\ w_reqs W symbol = [] /\
      (PayloadOf W symbol [] (TUnion [TNom truth []; TNom falsity []]) \/
       PayloadOf W symbol [] (TUnion [TNom falsity []; TNom truth []]))).

Definition Linked (W : world) (P : xprogram) : Prop :=
  Layouts W /\
  exists context needs types entries,
    CtxOf W (xo_main P) context needs /\ InitialTypes W context types /\
    xo_env P = xinst [] types ++ entries /\
    Forall (fun entry => Entry W P context (fst entry) (snd entry)) entries.

Definition Ready (P : xprogram) : Prop :=
  exists bundle world, ElaboratesIn bundle world P /\ Linked world P.
End Platform.

(* The operations installed anywhere in a program, including captured
   environments and platform-family bodies. Extra interface operations are
   harmless; an interface must supply at least these requirements. *)
Fixpoint expression_primitives (expression : xexpr) : list prim :=
  match expression with
  | XVar _ | XUnit | XVl _ | XBreak => []
  | XTag _ _ e | XBindTy _ _ e | XBindFn _ _ _ e | XBindMth _ _ _ _ _ e
  | XBindFun _ _ _ _ e | XBindFunA _ _ _ e | XApply _ _ _ e | XApplyR _ _ _ e
  | XAssign _ e | XLoop e | XReturn e | XField e _ | XBindFunSym _ _ e
  | XBindFunCopy _ _ e | XConst _ e => expression_primitives e
  | XCallD _ _ _ args | XCallA _ args => flat_map expression_primitives args
  | XMeth receiver _ _ args => expression_primitives receiver ++ flat_map expression_primitives args
  | XMatch e arms => expression_primitives e ++ flat_map (fun arm => match arm with (_, _, body) => expression_primitives body end) arms
  | XLet _ a b | XBindVal _ a b | XSeq a b | XWhile a b => expression_primitives a ++ expression_primitives b
  | XIf a b c => expression_primitives a ++ expression_primitives b ++ expression_primitives c
  | XRec fields => flat_map (fun field => expression_primitives (snd field)) fields
  | XMatchP e arms => expression_primitives e ++ flat_map (fun arm => expression_primitives (snd arm)) arms
  | XApplyBr _ branches e =>
      flat_map (fun branch => match branch with (_, bindings, _) => flat_map binding_primitives bindings end) branches
      ++ expression_primitives e
  | XBindPrim _ operation e => operation :: expression_primitives e
  end
with binding_primitives (binding : xbind) : list prim :=
  match binding with
  | XBVal _ e => expression_primitives e
  | XBApplyBr _ branches =>
      flat_map (fun branch => match branch with (_, bindings, _) => flat_map binding_primitives bindings end) branches
  | XBPrim _ operation => [operation]
  | _ => []
  end.
Fixpoint entry_primitives (entry : xdentry) : list prim :=
  match entry with
  | XDPrim operation | XDPrimAt operation _ => [operation]
  | XDClo _ captured | XDFun _ _ captured => flat_map (fun entry => entry_primitives (snd entry)) captured
  | _ => []
  end.
Definition required_primitives (program : xprogram) : list prim :=
  flat_map (fun fn => expression_primitives (snd fn)) (xo_fns program) ++
  flat_map (fun fn => flat_map binding_primitives (snd (snd fn))) (xo_funs program) ++
  flat_map (fun entry => entry_primitives (snd entry)) (xo_env program).

Definition program_of (p : xprogram) (argv : list (list int)) (rd : list int -> option (list int)) : xprog :=
  mkXProg (fun f => Elaboration.nlook f (xo_fns p)) (fun u => Elaboration.nlook u (xo_funs p))
          (xo_bool p) (xo_true p) (xo_false p) argv rd (xo_cast p)
    (fun h => match Elaboration.nlook h (xo_inputs p) with Some ts => ts | None => [] end)
    (fun h => match Elaboration.nlook h (xo_exports p) with Some sm => sm | None => [] end)
    (xo_immutable p)
    (fun h => match Elaboration.nlook h (xo_fixed_inst p) with Some th => th | None => [] end)
    (fun h => match Elaboration.nlook h (xo_fixed_keys p) with Some keys => keys | None => [] end).


Module HeapAddress.
Local Open Scope uint63_scope.

(* The bits of a key, low first, up to the last 1. *)
Fixpoint key_go (fuel : nat) (a : int) : list bool :=
  match fuel with
  | O => []
  | S f => if a =? 0 then [] else (a land 1 =? 1) :: key_go f (a >> 1)
  end.

Definition key (a : int) : list bool := key_go 63 a.

End HeapAddress.

Module Heap.
Local Unset Implicit Arguments.
Import HeapAddress.
(* A store typing: an address's kind (true: an array) and what its
   contents (a box's value, an array's elements) satisfy.  It is keyed by
   an address's bits (HeapAddress.key), so that no law needs a fact about
   machine-integer equality. *)
Definition hty : Type := list bool -> option (bool * (xvalue -> Prop)).

(* Ψ' extends Ψ: every address Ψ types, Ψ' types the same way. *)
Definition hext (Psi Psi' : hty) : Prop :=
  forall a k Q, Psi a = Some (k, Q) -> Psi' a = Some (k, Q).

(* w is a reference (a tag over a heap address) of kind k whose contents
   satisfy Q. *)
Definition href (Psi : hty) (k : bool) (w : xvalue) (Q : xvalue -> Prop) : Prop :=
  exists n a, w = XVTag n (XVRef a) /\ Psi (key a) = Some (k, Q).

(* What a GC primitive needs of its arguments, for contents satisfying Q. *)
Definition gc_pre (Psi : hty) (p : prim) (ws : list xvalue) (Q : xvalue -> Prop) : Prop :=
  match p, ws with
  | PBox _, [v] => Q v
  | PDeref, [r] => href Psi false r Q
  | PRefSet, [r; v] => href Psi false r Q /\ Q v
  | PArrNew _, [v; XVI32 _] => Q v
  | PArrLen, [r] => href Psi true r Q
  | PArrGet, [r; XVI32 _] => href Psi true r Q
  | PArrSet, [r; XVI32 _; v] => href Psi true r Q /\ Q v
  | PArrFill, [r; XVI32 _; v; XVI32 _] => href Psi true r Q /\ Q v
  | PArrCopy, [r; XVI32 _; r2; XVI32 _; XVI32 _] =>
      href Psi true r Q /\ exists Q2, href Psi true r2 Q2 /\ forall x, Q2 x -> Q x
  | _, _ => False
  end.

(* What it gives back, when it gives a value w, with Ψ' its store typing after. *)
Definition gc_post (Psi Psi' : hty) (p : prim) (Q : xvalue -> Prop) (w : xvalue) : Prop :=
  hext Psi Psi' /\
  match p with
  | PBox rf => exists a, w = XVTag rf (XVRef a) /\ Psi (key a) = None /\ Psi' (key a) = Some (false, Q)
  | PArrNew ar => exists a, w = XVTag ar (XVRef a) /\ Psi (key a) = None /\ Psi' (key a) = Some (true, Q)
  | PDeref | PArrGet => Q w
  | PArrLen => exists n, w = XVI32 n
  | _ => w = XVUnit
  end.

Definition is_gc (p : prim) : bool :=
  match p with
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy => true
  | _ => false
  end.


End Heap.

(* Internal runtime interface and laws used by DefaultRuntime. The public
   source/build/run contract is defined only by Audit.Public.CORRECT. *)
Definition program := xprogram.

Module Type HOST.
  Parameter state : Type.
  Parameter primitive : xprog -> prim -> list xvalue -> state -> xout * state.
  Parameter heap_ok : Heap.hty -> state -> Prop.
End HOST.

Module Public (Host : HOST).
  Record state := World {
    arguments : list (list int);
    read_file : list int -> option (list int);
    external : Host.state
  }.

  Record interface := HostInterface {
    supports : prim -> bool;
    primitive : xprog -> prim -> list xvalue -> Host.state -> xout * Host.state;
    heap_ok : Heap.hty -> Host.state -> Prop
  }.

  (* Unsupported operations have a defined stopping outcome in the existing
     primitive model; providing them does not promise a returning result. *)
  Definition default_interface : interface :=
    HostInterface (fun _ => true) Host.primitive Host.heap_ok.

  Definition runtime (program : program) (initial : state) : xprog :=
    program_of program (arguments initial) (read_file initial).

  Definition primitive_result P result (out : xout) : Prop :=
    match out with
    | XOk value => xvty P value result
    | XTrap _ | XExit _ => True
    | _ => False
    end.

  (* Laws of supplied operations, under the platform's audited signatures.
     The source-side agreement with these signatures is Platform.Linked. *)
  Definition interface_laws (P : xprog) (interface : interface) : Prop :=
    let step := primitive interface P in
    let valid_heap := heap_ok interface in
    (forall p params result values state,
      supports interface p = true -> prim_sig p = Some (params, result) ->
      Forall2 (xvty P) values params ->
      primitive_result P result (fst (step p values state))) /\
    (forall p params result values state,
      supports interface p = true -> prim_msig p = Some (params, result) ->
      Forall2 (xvty P) values params ->
      primitive_result P result (fst (step p values state))) /\
    (forall code values state, supports interface (PUnsupported code) = true ->
      fst (step (PUnsupported code) values state) = XUns code) /\
    (forall typing weaker state,
      valid_heap typing state ->
      (forall address kind predicate,
        weaker address = Some (kind, predicate) ->
        exists original, typing address = Some (kind, original) /\
          forall value, original value -> predicate value) ->
      valid_heap weaker state) /\
    (forall state, valid_heap (fun _ => None) state) /\
    (forall p values state typing,
      supports interface p = true ->
      Heap.is_gc p = false -> valid_heap typing state ->
      valid_heap typing (snd (step p values state))) /\
    (forall p values state typing predicate,
      supports interface p = true ->
      valid_heap typing state -> Heap.gc_pre typing p values predicate ->
      match step p values state with
      | (XOk value, next) =>
          exists extended, valid_heap extended next /\
            Heap.gc_post typing extended p predicate value
      | (XTrap _, next) => valid_heap typing next
      | _ => False
      end).

  Definition Compatible (program : program) (interface : interface) : Prop :=
    Platform.Ready program /\
    (forall operation, In operation (required_primitives program) -> supports interface operation = true) /\
    forall argv read, interface_laws (program_of program argv read) interface.

  Definition ValidState (interface : interface) (state : state) : Prop :=
    heap_ok interface (fun _ => None) (external state).

  (* Traps, exits and unsupported primitives are explicit outcomes. Wrong
     represents an internal error, including a missing interface operation. *)
  Inductive outcome :=
  | Returned : xvalue -> outcome
  | Exited : int -> outcome
  | Trapped : nat -> outcome
  | Unsupported : nat -> outcome
  | Wrong : nat -> outcome.

  Record execution := Result {
    execution_outcome : outcome;
    final_state : state
  }.

  (* None means this finite observation has not produced an outcome. *)
  Definition partial (A : Type) := nat -> option A.
  Definition e_missing_primitive := 15.

  Module ReferenceHost <: PRIM_SEMANTICS.
    Definition mstate := (interface * state)%type.
    Definition run_prim P p values (machine : mstate) :=
      let '(interface, state) := machine in
      if supports interface p then
        let '(out, next) := primitive interface P p values (external state) in
        (out, (interface, World (arguments state) (read_file state) next))
      else (XErr e_missing_primitive, machine).
  End ReferenceHost.
  Module Reference := XSem ReferenceHost.

  Definition observe (result : Reference.xres) : option execution :=
    let state := snd (Reference.xr_st result) in
    match Reference.xr_out result with
    | XOk value | XRet value => Some (Result (Returned value) state)
    | XExit code => Some (Result (Exited code) state)
    | XTrap code => Some (Result (Trapped code) state)
    | XUns code => Some (Result (Unsupported code) state)
    | XErr code => Some (Result (Wrong code) state)
    | XBrk => Some (Result (Wrong Reference.e_break) state)
    | XFuel => None
    end.

  Definition Runs (program : program) (interface : interface)
      (initial : state) (result : execution) : Prop :=
    exists fuel,
      observe (Reference.run_main fuel (runtime program initial)
        (xo_main program) (xo_env program) (interface, initial)) = Some result.

End Public.
