(* ------------------------------------------------------------------------ *)
(* EaDecide.v — deciders for Audit's negative and set-valued side          *)
(* conditions that are not about types: Breaks, Mentions_name, ResultLeaf/ *)
(* SameResult, CollidesOnlyAs, Join, NumericText; and the syntactic        *)
(* collection of a body's local names (for frame numberings).              *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Ascii.
From NM Require Import Audit EaBase EaTypes.
From NM Require Parse.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Section Decide.
Variable W : world.
Variable fuel : nat.

(* ---- Breaks (note 42) ---- *)

(* SOUND: breaks n md e = true <-> Breaks W md e (with enough fuel), and
   likewise block_breaks/stmt_breaks/item_breaks *)
Fixpoint breaks (n : nat) (md : string) (e : aexpr) {struct n} : bool :=
  match n with
  | 0 => false
  | S n' =>
      match e with
      | AEBreak => true
      | AECall _ _ args => existsb (breaks n' md) args
      | AEField o _ => breaks n' md o
      | AEMeth o _ args => breaks n' md o || existsb (breaks n' md) args
      | AERec _ _ fs => existsb (fun q => match snd q with Some y => breaks n' md y | None => false end) fs
      | AEIf c b oe => breaks n' md c || block_breaks n' md b || match oe with Some y => breaks n' md y | None => false end
      | AEMatch s arms => breaks n' md s || existsb (fun q => breaks n' md (snd q)) arms
      | AEReturn (Some y) => breaks n' md y
      | AEBlock b => block_breaks n' md b
      | AEUnary _ y => breaks n' md y
      | AEBinary _ l r => breaks n' md l || breaks n' md r
      | _ => false
      end
  end
with block_breaks (n : nat) (md : string) (b : ablock) {struct n} : bool :=
  match n with
  | 0 => false
  | S n' => match b with
            | ABlock ss tl => existsb (stmt_breaks n' md) ss || match tl with Some y => breaks n' md y | None => false end
            end
  end
with stmt_breaks (n : nat) (md : string) (s : astmt) {struct n} : bool :=
  match n with
  | 0 => false
  | S n' =>
      match s with
      | ASLet _ _ y | ASVar _ _ y | ASAssign _ y | ASExpr y => breaks n' md y
      | ASBind its => existsb (item_breaks n' md) its
      | ASWhile c _ => breaks n' md c
      | ASLoop _ => false
      end
  end
with item_breaks (n : nat) (md : string) (it : spec * option aexpr) {struct n} : bool :=
  match n with
  | 0 => false
  | S n' =>
      match it with
      | (_, Some y) => breaks n' md y
      | (Spec (Some pa) None None, None) =>
          match resolve_sym W md pa with
          | Some u => match w_decl W u with
                      | Some d => match d_ast d with
                                  | ADFunctor _ _ _ _ (Some bodies) => existsb (item_breaks n' (d_mod d)) (List.concat bodies)
                                  | _ => false end
                      | None => false end
          | None => false
          end
      | _ => false
      end
  end.

(* SOUND: mentions_name F body n = true <-> Mentions_name W F body n *)
Definition global_name (md : string) (n : name) : bool := is_some (resolve W md [n]).
Definition mentioned_names (F : frame) (body : aexpr) : list name :=
  free_names (global_name (fr_mod F)) body.

(* ---- ResultLeaf and SameResult ---- *)

Fixpoint result_spec_leaves (n : nat) (p : string) (sp : spec) {struct n} : list (nat * option name) :=
  match n with
  | 0 => []
  | S n' =>
      match sp with
      | Spec (Some pa) (Some dn) _ => match resolve_sym W p pa with Some s => [(s, Some dn)] | None => [] end
      | Spec (Some pa) None _ =>
          match resolve_sym W p pa with
          | Some s =>
              match w_decl W s with
              | Some d =>
                  match d_ast d with
                  | ADCtx _ its => flat_map (result_spec_leaves n' (d_mod d)) (flat_map citem_specs its)
                  | ast => if result_already_provided ast then [] else [(s, None)]
                  end
              | None => []
              end
          | None => []
          end
      | _ => []
      end
  end.
Definition result_leaves (u : nat) : list (nat * option name) :=
  match w_decl W u with
  | Some d => match d_ast d with
              | ADFunctor _ _ _ r _ => flat_map (result_spec_leaves fuel (d_mod d)) r
              | _ => [] end
  | None => []
  end.
Definition leaf_eqb (a b : nat * option name) : bool :=
  Nat.eqb (fst a) (fst b) && match snd a, snd b with
                             | Some x, Some y => String.eqb x y | None, None => true | _, _ => false end.
(* SOUND: same_result u g = true <-> SameResult W u g *)
Definition same_result (u g : nat) : bool :=
  let a := result_leaves u in let b := result_leaves g in
  forallb (fun k => existsb (leaf_eqb k) b) a && forallb (fun k => existsb (leaf_eqb k) a) b.

End Decide.

(* SOUND: collides_only_as ks ks0 = true <-> CollidesOnlyAs ks ks0 *)
Definition collides_only_as (ks ks0 : list item) : bool :=
  let ix := combine (seq 0 (List.length ks)) ks in
  forallb (fun a => forallb (fun b =>
    if item_eqb (provision_id (snd a)) (provision_id (snd b)) then
      match nth_error ks0 (fst a), nth_error ks0 (fst b) with
      | Some k0, Some k0' => item_eqb (provision_id k0) (provision_id k0')
      | _, _ => false
      end
    else true) ix) ix.

(* SOUND: join a b exp = Ok t -> Join a b exp t *)
Definition join (a b : ty) (exp : option ty) : er ty :=
  match exp with
  | Some t => Ok t
  | None =>
      if ty_eqb a never then Ok b
      else if ty_eqb b never || ty_eqb a b then Ok a
      else Err "branches have different types"
  end.

(* NumericText, decided by the parser's lexer (FrontEnd proves Parse.lex
   sound for Syntax.Lexes): the text is one INTEGER or DECIMAL token whose
   normal form is the text itself (raw := text).
   SOUND: numeric_text s = true -> NumericText s. *)
Definition numeric_text (s : string) : bool :=
  match Parse.lex (String.list_byte_of_string s) with
  | Parse.LOk [Syntax.Tok k x _; Syntax.Tok Syntax.EOF _ _] =>
      (Syntax.token_kind_eqb k Syntax.INTEGER || Syntax.token_kind_eqb k Syntax.DECIMAL) &&
      String.eqb x s && String.eqb (Syntax.lit_norm x) s
  | _ => false
  end.

(* ---- the names a body may bind (frame numberings) ---- *)

(* Every name in a binder position of a body, syntactically: let/var
   names, pattern binders, record-pattern fields, lambda parameters, D89's
   `%argN`, and, through `bind F;` for F with a body, F's bind items (they
   run in the enclosing frame, BApplyBody).  A superset is harmless: a
   numbering only has to cover the names the derivation reads. *)
Section Names.
Variable W : world.
Fixpoint pat_names (p : apat) : list name :=
  match p with
  | APWild => []
  | APPath [n] => [n]
  | APPath _ => []
  | APTag _ q => pat_names q
  | APRec _ fs => flat_map (fun q => match snd q with None => [fst q] | Some p => pat_names p end) fs
  end.
Fixpoint ex_names (n : nat) (md : string) (e : aexpr) {struct n} : list name :=
  match n with
  | 0 => []
  | S n' =>
      match e with
      | AECall _ a args =>
          (match a with Some _ => map Syntax.call_arg_name (seq 0 (List.length args)) | None => [] end) ++
          flat_map (ex_names n' md) args
      | AEField o _ | AEUnary _ o => ex_names n' md o
      | AEMeth o _ args => ex_names n' md o ++ flat_map (ex_names n' md) args
      | AERec _ _ fs => flat_map (fun q => match snd q with Some y => ex_names n' md y | None => [] end) fs
      | AEIf c b oe => ex_names n' md c ++ block_names n' md b ++ match oe with Some y => ex_names n' md y | None => [] end
      | AEMatch s arms => ex_names n' md s ++ flat_map (fun q => pat_names (fst q) ++ ex_names n' md (snd q)) arms
      | AEReturn (Some y) => ex_names n' md y
      | AEBlock b => block_names n' md b
      | AEBinary _ l r => ex_names n' md l ++ ex_names n' md r
      | AELam ps _ body => map fst ps ++ ex_names n' md body
      | _ => []
      end
  end
with block_names (n : nat) (md : string) (b : ablock) {struct n} : list name :=
  match n with
  | 0 => []
  | S n' => match b with
            | ABlock ss tl => flat_map (stmt_names n' md) ss ++ match tl with Some y => ex_names n' md y | None => [] end
            end
  end
with stmt_names (n : nat) (md : string) (s : astmt) {struct n} : list name :=
  match n with
  | 0 => []
  | S n' =>
      match s with
      | ASLet x _ y | ASVar x _ y => x :: ex_names n' md y
      | ASAssign _ y | ASExpr y => ex_names n' md y
      | ASBind its => flat_map (item_names n' md) its
      | ASWhile c b => ex_names n' md c ++ block_names n' md b
      | ASLoop b => block_names n' md b
      end
  end
with item_names (n : nat) (md : string) (it : spec * option aexpr) {struct n} : list name :=
  match n with
  | 0 => []
  | S n' =>
      match it with
      | (_, Some y) => ex_names n' md y
      | (Spec (Some pa) None None, None) =>
          match resolve_sym W md pa with
          | Some u => match w_decl W u with
                      | Some d => match d_ast d with
                                  | ADFunctor _ _ _ _ (Some bodies) => flat_map (item_names n' (d_mod d)) (List.concat bodies)
                                  | _ => [] end
                      | None => [] end
          | None => []
          end
      | _ => []
      end
  end.
End Names.
