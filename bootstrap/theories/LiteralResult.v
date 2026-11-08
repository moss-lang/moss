(* A checked arithmetic call keeps its literal receiver's source type. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements show].

Ltac lit_noerr H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
    first [exact (err_not_good _ _ _ _ _ _ H)|discriminate H
          |apply bind_inv in H as (? & ? & ? & H)])].

Ltac lit_peel H :=
  cbn beta iota zeta in H;
  first [solve [lazymatch type of H with ret _ _ = _ => inversion H; subst; reflexivity end]
    |lazymatch type of H with
      | err _ _ _ = _ => lit_noerr H
      | fail _ _ = _ => discriminate H
      | no_provision _ _ _ _ _ _ _ = _ => unfold no_provision in H; lit_peel H
      | mbind _ _ _ = _ => apply bind_inv in H as (? & ? & ? & H); lit_peel H
      | context [match ?v with _ => _ end] => destruct v eqn:?; lit_peel H
      end].

Theorem provided_call_literal_type : forall fuel c e ox T oy m s x t s',
  provided_call fuel c e ox T m [AB oy T] s = Good ((x,t),s') -> t=T.
Proof.
  intros [|fuel] c e ox T oy m s x t s' H; [discriminate H|].
  cbn [provided_call] in H. lit_peel H.
Qed.

Local Open Scope string_scope.
Definition merged_literal_env :=
  set_mth (set_uf (set_uf empty_env 1 YUnit) 2 YUnit) YUnit 5
    (FSig [("x",YAbs 1)] (YAbs 2) None).
Definition merged_literal_method :=
  GSym 5 0 "add" KMethod (ADFn RDetached "add" [] None None) None [] OSource.
Definition merged_literal_cx := Cx 0 (FSig [] YUnit None) None 0 [] "" [] [].

Example literal_result_keeps_receiver_representative : forall s,
  provided_call 4 merged_literal_cx merged_literal_env XUnit (YAbs 1)
    merged_literal_method [AB XUnit (YAbs 1)] s =
  Good ((XMeth XUnit 5 (IMth TUnit 5 []) [XUnit],YAbs 1),s).
Proof. reflexivity. Qed.
