(* A narrowed scalar application must leave type instantiation, type exports
   from other families, and checked casts unchanged. *)
From Stdlib Require Import List PrimInt63.
From NM Require Import Audit MossX FastPath.
Import ListNotations.
Import MossX.XS.
Local Open Scope nat_scope.

Definition typed_family_example (held : ty) : xprogram :=
  XProgram
    [(0, XApply 1 [(3,TNom 99 [])] []
           (XApply 6 [(3,TAbs 2)] []
             (XCallD 2 [(8,TAbs 9)] [(IVal 4,IVal 4)] [])));
     (2, XTag 7 [(8,TAbs 8)] (XVl 4))]
    [(10, ([IFn 5], [XBPrim (IFn 5) PMemSize]));
     (11, ([ITy 9], [XBTy 9 (TAbs 3)]))]
    20 21 22 0
    [(IFun 1,XDFun 10 [] []); (IFun 6,XDFun 11 [] []);
     (ITy 2,XDType TUnit); (IVal 4,XDVal (XVRefAt false held 0%uint63))]
    [] [(7,XCastSpec false 8 None)] [(11,[3])] [] true [] [].

Example typed_scalar_family_accepted :
  ok_program 1 10 (typed_family_example TUnit) = true.
Proof. reflexivity. Qed.

Example type_exporting_family_rejected :
  ok_program 6 11 (typed_family_example TUnit) = false.
Proof. reflexivity. Qed.

Example allocating_family_rejected :
  body_ok 1 [IFn 5] [XBPrim (IFn 5) (PBox 7)] = false.
Proof. reflexivity. Qed.

Definition typed_family_out (p : xprogram) (st : mstate) : xout :=
  xr_out (run_of p [] (fun _ => None) 40 st).

Example typed_family_cast_succeeds : forall st,
  typed_family_out (fast_program (typed_family_example TUnit)) st =
    XOk (XVTag 7 (XVRefAt false TUnit 0%uint63)).
Proof. intros st; reflexivity. Qed.

Example typed_family_cast_traps : forall st,
  typed_family_out (fast_program (typed_family_example (TNom 42 []))) st = XTrap 37.
Proof. intros st; reflexivity. Qed.

Example typed_family_original_cast_succeeds : forall st,
  typed_family_out (typed_family_example TUnit) st =
    XOk (XVTag 7 (XVRefAt false TUnit 0%uint63)).
Proof. intros st; reflexivity. Qed.

Example typed_family_original_cast_traps : forall st,
  typed_family_out (typed_family_example (TNom 42 [])) st = XTrap 37.
Proof. intros st; reflexivity. Qed.
