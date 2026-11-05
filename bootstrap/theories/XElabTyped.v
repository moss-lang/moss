(* ------------------------------------------------------------------------ *)
(* XElabTyped.v — the shape of `elaborated_typed` for MossX (Track B, TODO). *)
(*                                                                           *)
(* Nothing is proved here; the statements are against Audit.v Part V as of  *)
(* e5c2601 (Track A owns that draft), and are what would carry XSafety.v's  *)
(* `mossx_run_main_safe` to Audit.v's `accepted_never_err`.                 *)
(* ------------------------------------------------------------------------ *)

From NM Require Import Audit CoreMoss XTyping XEnv XSafety.

(* Section elaborated_typed_todo.

   The bridge, for the elaboration relation:

     Theorem elaborated_xtyped : forall b p,
       Elaboration.Elaborates b p ->
       forall argv rd,
       exists Sg Rp Fb Dm PhiD g0,
         xwf Sg Rp (program_of p argv rd) Fb
         /\ g_fn Sg (xo_main p) = Some (Dm, mkSig [] TUnit, None)
         /\ xp_fn (program_of p argv rd) (xo_main p) <> None
         /\ wf_tele Sg empty_ctx Dm PhiD
         /\ ctx_grounded g0 PhiD
         /\ xdenv_agree Sg Rp (program_of p argv rd) Fb g0 (cA PhiD) (xo_env p).

   With XSafety.mossx_run_main_safe it gives, for the oracle's primitives,

     forall b p, Elaboration.Elaborates b p ->
       forall fuel argv rd st0 c,
         xr_out (run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0) <> XErr c

   and with elaborate_sound, accepted_never_err.

   What the proof of elaborated_xtyped needs, and what blocks it today:
   (1) Σ is built from the world W (WorldOf b W): each declaration's
       telescope (CtxOf), its signature read into `ty`, Moss's I32/I64/F32/
       F64 as nominals with Rp, structs as RStruct.  This reading is not yet
       part of the draft relation (it relates terms only), so it is the
       first thing to agree with Track A.
   (2) Each FnElab W P f must yield a derivation of `xhas` for f's body.
       The rules take θ of XTag/XCallD from the derivation and admit
       erased type binds (XtBindTyI), because the elaborator writes θ = []
       and emits no XBindTy (DRAFT NOTE 3).  A method key IMth τ m [] with a
       method whose own telescope has type requirements is NOT typable
       (XtMeth reads θ from the key): either the elaboration contract
       records θ in method keys, or such methods' type requirements are
       read off the receiver.
   (3) Abstract functors are typed (XtBindFun, XtBindFunA, XtApply), with
       Rule A's captures and pins: a functor with a body has its linked
       arguments, captured inputs and pins in the table Fb beside Σ (so
       the bridge also builds Fb), and its body in P.  XApplyBr/XBindFunSym (FastPath.fast_program's output)
       are not typed: the bridge is to the elaborated program, and
       FastPath.fast_run carries the result to the fast one.
   (4) The platform's entries (PlatformEntry): PEZero/PEOne agree as I32
       vals; PEPrim agrees when the fn's declared signature reads to the
       primitive's prim_sig or (for `Memories`' members) prim_msig;
       PEUnsupported agrees at any signature (prim_ulaw: it stops with
       XUns).  xdenv_agree constrains only the entries main's context
       reaches.  PEFun's platform functors are bodies of primitive binds
       (Elab.platform_fun); those of `Heap`, `Mutable` and `Arrays` bind
       the GC primitives, typed by their shapes (XTyping.gc_shape) at
       reference and array types (reps RRef/RArr), so Σ must give `Ref`
       and `Array` those reps.  The initial store typing is empty
       (XSafety.xrun_main_safe0).

   End elaborated_typed_todo. *)
