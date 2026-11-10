(* Name validation shared by the source parser and the loader. *)
From Stdlib Require Import List String Ascii Bool.
From NM Require Import Audit Sx.
Import ListNotations Audit.Input Sx.Dump.
Local Open Scope string_scope.

Fixpoint strings_eqb (xs ys : list string) : bool :=
  match xs, ys with
  | [], [] => true
  | x :: xs, y :: ys => String.eqb x y && strings_eqb xs ys
  | _, _ => false
  end.

Definition path_ok (p : path) : bool := strings_eqb (split_path (join_path p)) p.
Definition opt_name_ok (o : option name) : bool :=
  match o with None => true | Some n => negb (String.eqb n "_") end.
Definition opt_path_ok (o : option path) : bool :=
  match o with None => true | Some p => path_ok p && negb (String.eqb (join_path p) "_") end.
Definition optional {A} (ok : A -> bool) (o : option A) : bool :=
  match o with None => true | Some x => ok x end.

Fixpoint spec_ok (s : spec) : bool :=
  match s with Spec p d a => opt_path_ok p && opt_name_ok d &&
    match a with None => true | Some bs =>
      forallb (fun b => path_ok (fst b) && spec_ok (snd b)) bs end
  end.
Definition app_ok (a : sapp) : bool :=
  match a with None => true | Some bs =>
    forallb (fun b => path_ok (fst b) && spec_ok (snd b)) bs end.

Fixpoint ty_ok (t : aty) : bool :=
  match t with
  | ATRec fs => forallb (fun p => ty_ok (snd p)) fs
  | ATUnion ms => forallb ty_ok ms
  | ATRef p a => path_ok p && app_ok a
  | _ => true
  end.
Fixpoint pat_ok (p : apat) : bool :=
  match p with
  | APWild => true
  | APPath q => path_ok q
  | APTag q p => path_ok q && pat_ok p
  | APRec q fs => opt_path_ok q && forallb (fun f => optional pat_ok (snd f)) fs
  end.
Fixpoint expr_ok (e : aexpr) : bool :=
  match e with
  | AEPath p a => path_ok p && app_ok a
  | AECall p a es => path_ok p && app_ok a && forallb expr_ok es
  | AEField o _ => expr_ok o
  | AEMeth o p es => expr_ok o && path_ok p && forallb expr_ok es
  | AERec p a fs => path_ok p && app_ok a && forallb (fun f => optional expr_ok (snd f)) fs
  | AEIf c b el => expr_ok c && block_ok b && optional expr_ok el
  | AEMatch e arms => expr_ok e && forallb (fun a => pat_ok (fst a) && expr_ok (snd a)) arms
  | AEReturn o => optional expr_ok o
  | AEBlock b => block_ok b
  | AEUnary _ e => expr_ok e
  | AEBinary _ l r => expr_ok l && expr_ok r
  | AELam ps ret body => forallb (fun p => optional ty_ok (snd p)) ps && optional ty_ok ret && expr_ok body
  | _ => true
  end
with block_ok (b : ablock) : bool :=
  match b with ABlock ss t => forallb stmt_ok ss && optional expr_ok t end
with stmt_ok (s : astmt) : bool :=
  match s with
  | ASLet _ t e | ASVar _ t e => optional ty_ok t && expr_ok e
  | ASAssign _ e | ASExpr e => expr_ok e
  | ASBind its => forallb (fun p => spec_ok (fst p) && optional expr_ok (snd p)) its
  | ASWhile c b => expr_ok c && block_ok b
  | ASLoop b => block_ok b
  end.
Definition bind_ok (its : list (spec * option aexpr)) : bool :=
  forallb (fun p => spec_ok (fst p) && optional expr_ok (snd p)) its.
Definition recv_ok (r : fnrecv) : bool :=
  match r with RAttached n => negb (String.eqb n "_") && negb (String.eqb n ".") | _ => true end.
Definition aitem_ok (it : aitem) : bool :=
  match it with
  | AIHead p => path_ok (fst p) && opt_name_ok (snd p)
  | AIEq l r => ty_ok l && ty_ok r
  end.
Definition citem_ok (it : citem) : bool :=
  match it with
  | CISpec s => spec_ok s
  | CIEq l r => ty_ok l && ty_ok r
  end.
Fixpoint decl_ok (d : adecl) : bool :=
  match d with
  | ADAssume its ds => forallb aitem_ok its && forallb decl_ok ds
  | ADAlias _ t | ADTag _ _ t => ty_ok t
  | ADVal _ t e => ty_ok t && optional expr_ok e
  | ADFn r _ ps ret b => recv_ok r && forallb (fun p => ty_ok (snd p)) ps && optional ty_ok ret && optional block_ok b
  | ADCtx _ its => forallb citem_ok its
  | ADFunctor _ _ args res body => forallb spec_ok args && forallb spec_ok res && optional (forallb bind_ok) body
  | _ => true
  end.
Definition use_side_ok (p : bool * name) : bool :=
  fst p || negb (String.prefix "." (snd p)).
Definition use_ok (u : ause) : bool :=
  forallb (fun c => negb (Ascii.eqb c ">"%char)) (list_ascii_of_string (u_name u)) &&
  use_side_ok (u_dot u, u_name u) && optional use_side_ok (u_alias u).
Definition import_ok (i : aimport) : bool := opt_name_ok (i_alias i) && forallb use_ok (i_uses i).
Definition wf_names (f : afile) : bool := forallb import_ok (f_imports f) && forallb decl_ok (f_decls f).
