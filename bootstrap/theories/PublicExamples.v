(* Executable checks of the public boundary, including the distinction
   between an unfinished observation and an internal error. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
Import ListNotations.
Local Open Scope string_scope.

Example absolute_path_accepted : exists path, path_of_string "/main.moss" = Some path.
Proof. eexists; reflexivity. Qed.
Example relative_path_rejected : path_of_string "main.moss" = None.
Proof. reflexivity. Qed.
Example unnormalized_path_rejected : path_of_string "/lib/../main.moss" = None.
Proof. reflexivity. Qed.

Module Host <: HOST_STATE.
  Definition State := nat.
End Host.
Module Model := Public Host.
Import Model.

Definition interface : Interface :=
  HostInterface [] (fun _ => None)
    (fun _ _ _ state => (XUns 0, state)) (fun _ _ => True).
Definition unit_program : Program :=
  XProgram [(1, XUnit)] [] 0 0 0 1 [] [] [] [] [] false [] [].
Definition missing_main : Program :=
  XProgram [] [] 0 0 0 1 [] [] [] [] [] false [] [].

Example finite_observation : Runs unit_program interface 7 (Result (Returned XVUnit) 7).
Proof. exists 2; reflexivity. Qed.
Example fuel_exhaustion :
  observe (Reference.run_main 0 (runtime unit_program interface)
    1 [] (interface, 7)) = None.
Proof. reflexivity. Qed.
Example internal_error_is_explicit :
  Runs missing_main interface 7 (Result (Wrong Reference.e_nobody) 7).
Proof. exists 0; reflexivity. Qed.
