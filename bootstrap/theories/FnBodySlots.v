(* Operational parameter slots, independent of body semantics. *)
From Stdlib Require Import List String.
From NM Require Import Audit Elab.
Import ListNotations.
Definition body_parameter_slots(this:option ety)(names:list string):list(string*nat):=
 let prefix:=match this with Some _=>[("this"%string,0)]|None=>[]end in
 prefix++combine names(seq(List.length prefix)(List.length names)).
