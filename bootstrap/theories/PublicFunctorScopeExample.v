(* Dynamic regressions for lexical functor activation. These fixtures follow
   the two source patterns below; they do not assert a full Admissible
   derivation or executable-elaboration refinement. *)
From Stdlib Require Import List PeanoNat.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.

Module Prims <: PRIMITIVES.
Definition mstate := unit.
Definition run_prim (_ : program_view) (_ : prim) (_ : list value) (st : unit) := (Trap 0,st).
End Prims.
Module R := Interpreter Prims.

(* F=1, T=2, Outer=3, U=4, Concrete=99. *)
Definition concrete := TNom 99 [].
Definition maker := FunctorCode [] [] [] [ITy 2] [BType 2 concrete] [].
Definition no_input_provider := FunctorCode [] [] [] [ITy 2] [BApply 1 [] [(2,2)] [] []] [].
Definition private_input_provider := FunctorCode [2] [2] [] [ITy 4] [BType 4 (TAbs 2)] [].
Definition runtime := ProgramView (fun _ => None)
  (fun h => if Nat.eqb h 20 then Some maker else
    if Nat.eqb h 21 then Some no_input_provider else
    if Nat.eqb h 22 then Some private_input_provider else None)
  0 0 0 [] (fun _ => None) [].

(* type T; tag Concrete;
   functor F: -> T; functor Maker: -> T { bind T=Concrete; }
   functor Outer: -> T; functor Provider: F -> T { bind F; }
   fn main() { bind F=Maker; bind F; bind Outer=Provider; bind Outer; }

   Provider's own inputs contain F but no type T. Its captured provision F
   remains usable; the ambient stable T must not occupy its local output. *)
Definition initial := Environment [] [(IFun 1,FunctorClosure 20 [] empty_environment [])].
Definition no_input_setup :=
  match R.eval_bindings 30 runtime initial [] tt
    [BApply 1 [] [(2,2)] [] []; BFunctor 3 21 (TypeCapture [] []) [] [(IFun 1,IFun 1)]] with
  | inl (d,_,_) => d
  | inr _ => empty_environment
  end.

Example no_input_setup_has_ambient_type :
  lookup_subst 2 (type_meanings no_input_setup) = Some concrete.
Proof. reflexivity. Qed.

Example no_input_provider_ignores_ambient_generations :
  type_meanings (application_environment runtime no_input_setup 21 [] no_input_setup) = [].
Proof. reflexivity. Qed.

Example no_input_provider_application_succeeds :
  R.apply_functor 40 runtime no_input_setup 3 [] [(2,7)] [] [] tt =
  inl (EnvironmentWithAliases [(7,concrete)] [(2,concrete)] [],tt).
Proof. reflexivity. Qed.

(* type T; type U; tag Concrete;
   functor Outer: -> U; functor Provider: T -> U { bind U=T; }
   fn main() { bind T=Concrete; bind Outer=Provider; bind Outer; }

   T is a private provider input, captured as an already-read source alias
   (main's `lexical_capture`, whose table reads T as Concrete).
   A new activation initializes the formal role T from that alias. *)
Definition private_alias := bind_type empty_environment 2 concrete.
Definition private_input_setup :=
  match R.eval_bindings 15 runtime private_alias [] tt [BFunctor 3 22 (TypeCapture [] [(2,concrete)]) [] []] with
  | inl (d,_,_) => d
  | inr _ => empty_environment
  end.

Example captured_private_type_has_only_an_alias :
  type_meanings private_alias = [] /\ output_meaning private_alias 2 = Some concrete.
Proof. split; reflexivity. Qed.

Example private_formal_receives_captured_meaning :
  ground (application_environment runtime empty_environment 22 [] private_alias) (TAbs 2) = concrete.
Proof. reflexivity. Qed.

Example private_input_provider_application_exports_concrete_meaning :
  R.apply_functor 30 runtime private_input_setup 3 [] [(4,7)] [] [] tt =
  inl (EnvironmentWithAliases [(7,concrete)] [(4,concrete)] [],tt).
Proof. reflexivity. Qed.

Example supplied_formal_takes_precedence_over_capture :
  ground (application_environment runtime empty_environment 22 [(2,TMachine MF64)] private_alias)
    (TAbs 2) = TMachine MF64.
Proof. reflexivity. Qed.
