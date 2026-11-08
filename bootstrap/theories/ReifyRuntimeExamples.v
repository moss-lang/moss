(* Concrete execution controls for complete immutable application maps. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel MossX.
Import ListNotations.
Import XS.
Definition reified_control : xprog :=
 mkXProg (fun _=>None)
 (fun h=>if Nat.eqb h 2 then Some([ITy 7;IVal 8],[XBTy 7 TUnit;XBVal 8 XUnit])else None)
 0 0 0 [] (fun _=>None) [] (fun _=>[])
 (fun h=>if Nat.eqb h 2 then [(ITy 3,ITy 7);(IVal 4,IVal 8)]else[]) true (fun _=>[]) (fun _=>[]).
Definition reified_control_env : xdenv := [(IFun 1,XDFun 2 [[]] [])].
Definition reified_control_body : xexpr :=
 XApplyR 1 [] [(ITy 9,ITy 3);(IVal 10,IVal 4)] (XVl 10).
Example complete_application_exports_type_and_value : forall st,
 xinterp 20 reified_control reified_control_env [] st reified_control_body=XR(XOk XVUnit)[]st.
Proof. reflexivity. Qed.
Example immutable_aliases_preserve_prior_type_meaning : forall st,
 xinterp_binds 5 reified_control [] [] st
 [XBTy 11(TNom 21[]);XBTy 12(TNom 22[])]=
 inl([(ITy 12,XDType(TNom 22[]));(ITy 11,XDType(TNom 21[]))],[],st).
Proof. reflexivity. Qed.
Example malformed_complete_input_map_reports_error : forall st,
 xinterp 20 reified_control reified_control_env [] st
 (XApplyR 1 [(IVal 55,IVal 99)] [] XUnit)=XR(XErr e_compose)[]st.
Proof. reflexivity. Qed.
