(* Exact simulation: the standalone reader returns the legacy checker’s
   complete result, including its first diagnostic, without legacy state. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import LoadListed LoadWorld NMap Sx Elab ElabFrameCore.
Import ListNotations.
Local Open Scope string_scope.

Definition attach {A} (s : Elab.st) (r : res A) : res (A * Elab.st) :=
  match r with Good a => Good (a, s) | Bad msg => Bad msg end.

Lemma fold_equal : forall A B (f : A -> B -> Elab.M A) (g : A -> B -> res A) xs s,
  (forall a x, f a x s = attach s (g a x)) ->
  forall a, Elab.mfold f a xs s = attach s (LoadListed.fold g a xs).
Proof.
  intros A B f g xs s H. induction xs as [|x xs IH]; intro a; [reflexivity|].
  cbn [Elab.mfold LoadListed.fold]. unfold Elab.mbind. rewrite H.
  destruct (g a x); cbn [attach rbind]; [apply IH|reflexivity].
Qed.

Lemma resolve_sym_equal : forall mi p s,
  Elab.resolve_sym mi p s =
  attach s (LoadListed.resolve_sym (st_w s) (st_syms s) mi p).
Proof.
  intros mi p s. unfold Elab.resolve_sym, Elab.rpath, Elab.mbind,
    LoadListed.resolve_sym.
  destruct (LoadWorld.resolve_path (st_w s) mi p) as [[[i|mi']|]|msg];
    cbn [rbind Elab.sym Elab.ret attach]; try reflexivity.
  unfold Elab.sym, Elab.ret, attach. destruct (nget (st_syms s) i); reflexivity.
Qed.

Lemma walk_specs_equal : forall fuel mi specs acc s,
  Elab.walk_specs fuel mi specs acc s =
  attach s (LoadListed.walk_specs (st_w s) (st_syms s) fuel mi specs acc).
Proof.
  induction fuel as [|fuel IH]; intros mi specs acc s; [reflexivity|].
  cbn [Elab.walk_specs LoadListed.walk_specs]. apply fold_equal.
  intros a [[p|] [n|] args]; try reflexivity.
  unfold Elab.mbind. rewrite resolve_sym_equal.
  destruct (LoadListed.resolve_sym (st_w s) (st_syms s) mi p) as [[g|]|msg];
    cbn [attach rbind Elab.ret]; try reflexivity.
  destruct (kind_eqb (gs_kind g) KType); [reflexivity|].
  destruct (kind_eqb (gs_kind g) KCtx); [|reflexivity].
  destruct (nmem (gs_id g) (fst a)); [reflexivity|].
  destruct (gs_decl g); try reflexivity. apply IH.
Qed.

Lemma assume_type_syms_equal : forall g s,
  Elab.assume_type_syms g s =
  attach s (LoadListed.assume_type_syms (st_w s) (st_syms s) g).
Proof.
  intros g s. unfold Elab.assume_type_syms, Elab.nesting_types,
    Elab.walk_fuel, Elab.assume_specs, LoadListed.assume_type_syms.
  unfold Elab.mbind, Elab.get, Elab.ret. cbn beta iota zeta. rewrite walk_specs_equal.
  destruct (LoadListed.walk_specs (st_w s) (st_syms s) (S (w_nsyms (st_w s)))
    (gs_mod g) (LoadWorld.assume_specs (gs_assumes g)) ([], [])); reflexivity.
Qed.

Lemma listed_params_equal : forall fuel g s,
  Elab.listed_params fuel g s =
  attach s (LoadListed.listed_params (st_w s) (st_syms s) g).
Proof.
  intros fuel g s.
  assert (E : Elab.listed_params fuel g = Elab.listed_params 0 g) by (destruct fuel; reflexivity).
  rewrite E. cbn [Elab.listed_params]. unfold LoadListed.listed_params.
  destruct (gs_decl g) as [| | |name [|n names] arity ty| | | | |]; try reflexivity.
  unfold Elab.mbind. rewrite assume_type_syms_equal.
  destruct (LoadListed.assume_type_syms (st_w s) (st_syms s) g) as [nesting|msg];
    cbn [attach rbind]; try reflexivity.
  apply fold_equal. intros out symbol.
  unfold Elab.mbind. rewrite resolve_sym_equal.
  destruct (LoadListed.resolve_sym (st_w s) (st_syms s) (gs_mod g) [symbol]) as [[t|]|msg];
    cbn [attach rbind Elab.ret]; try reflexivity;
    try (destruct (nmem (gs_id t) nesting); [reflexivity|]); reflexivity.
Qed.

Lemma iter_fold_equal : forall A (f : A -> Elab.M unit) xs s,
  Elab.miter f xs s = Elab.mfold (fun (_ : unit) x => f x) tt xs s.
Proof. intros A f xs. induction xs; intros s; [reflexivity|].
  cbn [Elab.miter Elab.mfold]. unfold Elab.mbind. destruct (f a s) as [[[] s']|msg];
    [apply IHxs|reflexivity]. Qed.

Theorem check_listed_equal : forall s,
  Elab.check_listed s =
  attach s (LoadListed.check_with (st_w s) (st_syms s)).
Proof.
  intro s. unfold Elab.check_listed. cbn [Elab.mbind Elab.get].
  rewrite iter_fold_equal. unfold LoadListed.check_with.
  assert (E : forall a, Elab.mfold
    (fun (_ : unit) i => match nget (w_syms (st_w s)) i with
     | Some g => if kind_eqb (gs_kind g) KTag then
         Elab.mbind (Elab.listed_params 0 g) (fun _ => Elab.ret tt)
       else Elab.ret tt
     | None => Elab.ret tt end) a (seq 0 (w_nsyms (st_w s))) s =
    attach s (LoadListed.fold
    (fun (_ : unit) i => match nget (w_syms (st_w s)) i with
     | Some g => if kind_eqb (gs_kind g) KTag then
         rbind (LoadListed.listed_params (st_w s) (st_syms s) g) (fun _ => Good tt)
       else Good tt
     | None => Good tt end) a (seq 0 (w_nsyms (st_w s))))).
  { apply fold_equal. intros a i. destruct (nget (w_syms (st_w s)) i) as [g|]; [|reflexivity].
    destruct (kind_eqb (gs_kind g) KTag); [|reflexivity].
    unfold Elab.mbind. rewrite listed_params_equal.
    destruct (LoadListed.listed_params (st_w s) (st_syms s) g); reflexivity. }
  rewrite E. destruct (LoadListed.fold _ tt _); cbn [attach rbind].
  - destruct u. reflexivity.
  - reflexivity.
Qed.

Theorem check_initial_equal : forall w,
  Elab.check_listed (ElabFrameCore.s_init w) =
  attach (ElabFrameCore.s_init w) (LoadListed.check w).
Proof. intro w. apply check_listed_equal. Qed.
