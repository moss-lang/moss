(* ------------------------------------------------------------------------ *)
(* RelNegative.v — two ill-formed programs of tests/wasi are not related by  *)
(* the DRAFT relation (and the elaborator rejects both).                     *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit CoreMoss MossX NMap Sx Elab RelSupport BxMisfit BxNonex.
Import ListNotations.
Import Audit.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* Name facts, read off the elaborator's world by computation. *)
Ltac crunch :=
  repeat match goal with
  | H : ?l = Some ?x |- _ => is_var x; let v := eval vm_compute in l in
                             match v with
                             | Some _ => change l with v in H; injection H as H; subst x
                             | None => change l with v in H; discriminate H
                             end
  | H : (?k = ?a) \/ (?k = ?b) |- _ => exfalso; destruct H as [H|H]; discriminate H
  | H : d_ast ?d = _ |- _ => let v := eval vm_compute in (d_ast d) in
                             change (d_ast d) with v in H;
                             first [ discriminate H | (injection H; clear H; intros; subst) ]
  | H : ?l = Some ?c |- _ => let v := eval vm_compute in l in
                             match v with
                             | c => clear H
                             | _ => change l with v in H; discriminate H
                             end
  end.

(* ===================== tests/wasi/misfit.moss ========================== *)
(*   tag A(I32);  tag B(I32);
     assume Wasm { fn f(a: A): I32 { w::i32_one }
                   fn main() { f(B(w::i32_one)); } }
   `f` wants an A and is given a B.  Whatever the context, the call has no
   elaboration: the argument's type is B's and the parameter's A's, and two
   nominals of different heads do not fit ([D17]). *)

Definition misfit_rej := Eval vm_compute in elaborate_sx misfit_sx.
Example misfit_rejected : misfit_rej =
  Bad "tests/wasi/misfit.moss: argument `a` of `f` is `B[I32=I32]`, expected `A[I32=I32]`".
Proof. reflexivity. Qed.

Definition S_mf : st := Eval vm_compute in match run_world misfit_sx with Good s => s | Bad _ => dummy_st end.
Definition W_mf : world := W_of S_mf.

Lemma fits_heads : forall s1 s2 xs ys, s1 <> s2 -> ~ fits (TNom s1 xs) (TNom s2 ys).
Proof.
  intros s1 s2 xs ys Hne [H|[H|(ms & H & _)]]; try discriminate. injection H as H _. contradiction.
Qed.

Theorem misfit_unrelated : forall P F C exp x t,
  fr_mod F = "tests/wasi/misfit.moss" ->
  ~ Ex W_mf P F C (AECall ["f"] None [AECall ["B"] None [AEPath ["w"; "i32_one"] None]]) exp x t.
Proof.
  intros P F C exp x t HF H.
  inversion H; subst; rewrite HF in *; crunch.
  - (* a defined fn: its parameter is an A *)
    destruct H9 as (d & rcv & n & ps & rt & body & Hd & Ha & Hps & Hrt).
    crunch.
    destruct g as [gps gr]; simpl in *.
    inversion Hps as [|pt pa ps' gps' Hty Hrest]; subst. inversion Hrest; subst.
    simpl in Hty. inversion Hty; subst; crunch.
    (* the argument is a B *)
    inversion H13; subst.
    match goal with Hx : Ex _ _ _ _ (AECall ["B"] _ _) _ _ _ |- _ => inversion Hx; subst end;
      try rewrite HF in *; crunch.
    match goal with Hf : fits _ _ |- _ =>
      unfold canon in Hf; simpl in Hf; revert Hf; apply fits_heads; discriminate end.
  - (* an abstract fn: f has a body *)
    destruct H4 as (n & ps & rt & Hast). crunch.
Qed.


(* ===================== tests/wasi/match_not_exhaustive.moss ============= *)
(*   tag Red;  tag Green;  type Colour = | Green | Red;
     assume Wasm { fn pick(c: Colour): I32 { match c { Red => w::i32_one, } } … }
   The match covers Red only, with no catch-all arm: under any context the
   body has no elaboration, so `pick` is not related (and [D16]: it is
   checked although nothing in a well-formed program would call it). *)

Definition nonex_rej := Eval vm_compute in elaborate_sx nonex_sx.
Example nonex_rejected : nonex_rej =
  Bad "tests/wasi/match_not_exhaustive.moss: match is not exhaustive; missing Green".
Proof. reflexivity. Qed.

Definition S_ne : st := Eval vm_compute in match run_world nonex_sx with Good s => s | Bad _ => dummy_st end.
Definition W_ne : world := W_of S_ne.
Definition d_pick : decl := Eval vm_compute in match w_decl W_ne 365 with Some d => d | None => Decl "" (ADUnit "") [] end.

Theorem nonex_unrelated : forall P, ~ FnElab W_ne P 365.
Proof.
  intros P H.
  assert (Hd : w_decl W_ne 365 = Some d_pick) by (vm_compute; reflexivity).
  destruct (H d_pick _ _ _ _ _ Hd eq_refl)
    as (C & ns & this & g & F & x & t & Hctx & Hthis & Hsig & Hm & Hr & Ht & Hl & Hf & Hap & Hpi & Hnum & Hblk & Hbody).
  simpl in Hthis. subst this.
  (* the parameter's type: Colour, the union of the two units *)
  destruct Hsig as (d & rcv & n & ps & rt & body & Hd' & Ha & Hps & Hrt). crunch.
  destruct g as [gps gr]; simpl in *.
  inversion Hps as [|pt pa ps' gps' Hty Hrest]; subst. inversion Hrest; subst.
  simpl in Hty. inversion Hty; subst; crunch.
  match goal with H : TyOf _ _ _ _ (ATUnion _) _ |- _ => inversion H; subst end.
  repeat match goal with
         | H : Forall2 (TyOf _ _ _ _) (_ :: _) _ |- _ => inversion H; subst; clear H
         | H : Forall2 (TyOf _ _ _ _) [] _ |- _ => inversion H; subst; clear H
         end.
  repeat match goal with H : TyOf _ _ _ _ (ATRef _ None) _ |- _ => inversion H; subst; clear H; crunch end.
  repeat match goal with H : Forall2 _ (w_reqs _ _) _ |- _ => vm_compute in H; inversion H; subst; clear H end.
  (* the context's locals: just the parameter *)
  destruct Hctx as (dc & es & tm & er & tmr & σ & Hdc & Hit & Hroot & Hmg & -> & Hn).
  assert (Nm : ~ IsMainFn W_ne 365) by notmain.
  destruct (root_nonmain _ _ _ _ _ _ Nm Hroot) as [-> ->]. rewrite app_nil_r in *.
  (* the body: a block whose tail is the match *)
  inversion Hblk; subst.
  match goal with H : Blk _ _ _ _ [] _ _ _ _ _ |- _ => inversion H; subst; clear H end.
  match goal with H : Ex _ _ _ _ (AEMatch _ _) _ _ _ |- _ => inversion H; subst end.
  (* the scrutinee is the local c *)
  match goal with H : Ex _ _ _ _ (AEPath ["c"] None) _ _ _ |- _ => inversion H; subst end;
    try (match goal with H : forall n, ["c"] = [n] -> _ |- _ =>
           specialize (H "c" eq_refl); simpl in H; discriminate H end).
  match goal with H : slook "c" _ = Some _ |- _ => vm_compute in H; inversion H; subst; clear H end.
  (* the arms cover Red, and catch nothing *)
  match goal with H : Arms _ _ _ _ _ _ _ _ _ |- _ => inversion H; subst; clear H end.
  (* (an empty match, for `|` only, does not apply: there is an arm) *)
  match goal with H : ArmsList _ _ _ _ _ _ _ _ _ _ _ |- _ => inversion H; subst; clear H end;
      [| match goal with H : [] <> [] |- _ => contradiction H; reflexivity end].
  try rewrite Hm in *.
  match goal with H : PatOf _ _ _ _ _ _ _ _ |- _ => inversion H; subst; clear H end;
    try rewrite Hm in *;
    try (match goal with H : resolve _ _ _ = None |- _ => vm_compute in H; discriminate H end).
  crunch.
  (* not exhaustive: no catch-all, and Green's head is not covered *)
  match goal with H : false = true \/ ?R |- _ =>
    let H' := fresh "Hcov" in
    assert (H' : R) by (destruct H as [H|H]; [discriminate H | exact H]); clear H;
    destruct H' as [_ H'];
    specialize (H' (TNom 363 []) 363); vm_compute in H';
    destruct H' as [E|[]]; [left; reflexivity | reflexivity | discriminate E]
  end.
Qed.
