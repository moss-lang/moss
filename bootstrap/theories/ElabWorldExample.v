(* ElabWorld's `bundle_world`, computed (vm_compute) on two small programs
   in a library directory (no prelude), loaded as the oracle loads files
   (Parse.load). The first has a reading; the second's tag lists a type
   its nesting does not require (D73), so it has none. *)
From Stdlib Require Import List String.
From NM Require AuditBase.
From NM Require Import Audit Sx Parse ElabWorld.
Import ListNotations.
Local Open Scope string_scope.
Module AE := AuditBase.Semantics.Elaboration.

Definition example_source : string := "type A;
type B;
context C = A, B;
assume C {
  tag Pair(A);
  tag Box[B](Pair);
  fn f(x: Pair): Pair { x }
}
fn main() {}
".

Definition unlisted_source : string := "type A;
type B;
assume A {
  tag Box[B](A);
}
fn main() {}
".

Definition read_one (text : string) (p : string) : option string :=
  if String.eqb p "/lib/main.moss" then Some text else None.

Definition load_one (text : string) : res Input.bundle :=
  Parse.load (read_one text) "/" "/lib/prelude.moss" "/lib/main.moss".

Definition bundle_of (r : res Input.bundle) : Input.bundle :=
  match r with Good b => b | Bad _ => Input.Bundle "" "" [] end.

Definition example_bundle : Input.bundle := bundle_of (load_one example_source).

(* Symbols 1-7: A, B, C, Pair, Box, f, main. Pair requires A (its payload);
   Box lists B and reads A through Pair; f requires its nesting C's types. *)
Example example_requirements :
  option_map (fun W => map (AE.w_reqs W) (seq 1 7)) (bundle_world example_bundle) =
    Some [[]; []; []; [1]; [2; 1]; [1; 2]; []].
Proof. vm_compute. reflexivity. Qed.

Example example_world : exists W, AE.WorldOf example_bundle W.
Proof.
  destruct (bundle_world example_bundle) as [W|] eqn:Run.
  - exists W. exact (bundle_world_sound _ _ Run).
  - vm_compute in Run. discriminate.
Qed.

Example unlisted_rejected : bundle_world (bundle_of (load_one unlisted_source)) = None.
Proof. vm_compute. reflexivity. Qed.

(* the rejection is check_listed's: the program loads *)
Example unlisted_loads :
  match load_one unlisted_source with
  | Good b => match LoadWorld.load_world b with Good _ => true | Bad _ => false end
  | Bad _ => false
  end = true.
Proof. vm_compute. reflexivity. Qed.
