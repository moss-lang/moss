(* Historical functor rows, including linked closures, retain their final
   program bodies. The all-row formulation transports backwards by Fr. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabFrameCore ElabKeeps MainProgramTrace.
Import ListNotations.
Definition FinalLinks (P : xprogram) (s : st) : Prop := forall key code,
 In (key,code) (st_funs s) -> ExecutionModel.Elaboration.funbody_of P key = Some code.
Definition FunctorRows (s : st) : Prop :=
 forall key code, In (key,code) (st_funs s) -> nassoc key (st_funs s) = Some code.
Definition FunctorBound (s : st) : Prop :=
 forall key code, In (key,code) (st_funs s) -> key < st_next s.
Lemma final_links_keep : forall P s final,
 (forall row, In row (st_funs s) -> In row (st_funs final)) ->
 FinalLinks P final -> FinalLinks P s.
Proof. intros P s final Rows Last key code Row;apply Last;apply Rows;exact Row. Qed.
Lemma final_links_back : forall P s final, Fr s final -> FinalLinks P final -> FinalLinks P s.
Proof.
 intros P s final Frame Last;apply (final_links_keep P s final);[|exact Last].
 intros row Row;destruct (fx_funs _ _ (fr_x _ _ Frame)) as [prefix Tables].
 rewrite Tables;apply in_or_app;right;exact Row.
Qed.
Lemma functor_nlook_nassoc : forall A n (l:list(nat*A)),
 ExecutionModel.Elaboration.nlook n l = nassoc n l.
Proof. intros A n l;induction l as[|[k v]xs IH];[reflexivity|];cbn;rewrite IH;reflexivity. Qed.
Lemma final_links_table : forall P s,
 xo_funs P = st_funs s -> FunctorRows s -> FinalLinks P s.
Proof.
 intros P s Tables Rows key code Row.
 unfold ExecutionModel.Elaboration.funbody_of;rewrite functor_nlook_nassoc,Tables;apply Rows;exact Row.
Qed.
Lemma functor_rows_init : forall w, FunctorRows (s_init w).
Proof. intros w key code Row;contradiction. Qed.
Lemma functor_bound_init : forall w, FunctorBound (s_init w).
Proof. intros w key code Row;contradiction. Qed.
Lemma functor_rows_keeps : forall s final,
 st_funs final = st_funs s -> FunctorRows s -> FunctorRows final.
Proof. intros s final Eq Rows key code Row;rewrite Eq in *;apply Rows;exact Row. Qed.
Lemma functor_bound_keeps : forall s final,
 Fr s final -> st_funs final = st_funs s -> FunctorBound s -> FunctorBound final.
Proof.
 intros s final Frame Eq Bound key code Row;rewrite Eq in Row.
 pose proof (Bound key code Row);pose proof (fr_next _ _ Frame);lia.
Qed.
Lemma functor_bound_absent : forall s key,
 FunctorBound s -> st_next s <= key -> ~ In key (map fst (st_funs s)).
Proof.
 intros s key Bound Above Member;apply in_map_iff in Member as [[owner code] [Eq Row]].
 cbn in Eq;subst owner;pose proof (Bound key code Row);lia.
Qed.
Lemma functor_rows_emit : forall s key code,
 FunctorRows s -> ~ In key (map fst (st_funs s)) ->
 FunctorRows (s_funs (fun rows => (key,code)::rows) s).
Proof.
 intros s key code Rows Absent owner value Row.
 cbn [s_funs st_funs] in Row|-*;destruct Row as [Eq|Row].
 - injection Eq as <- <-;cbn [nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
 - cbn [nassoc assoc];destruct (Nat.eqb owner key) eqn:Eq.
   + apply Nat.eqb_eq in Eq;subst owner;exfalso;apply Absent;apply in_map with (f:=@fst nat (tele*list xbind)) in Row;exact Row.
   + exact (Rows owner value Row).
Qed.
Lemma functor_bound_emit : forall s key code,
 FunctorBound s -> key < st_next s -> FunctorBound (s_funs (fun rows => (key,code)::rows) s).
Proof.
 intros s key code Bound Below owner value Row;cbn [s_funs st_funs] in Row|-*.
 destruct Row as [Eq|Row];[injection Eq as <- <-;exact Below|exact (Bound owner value Row)].
Qed.
