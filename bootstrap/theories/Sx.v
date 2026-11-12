(* S-expression codec for parsed files and module bundles. *)
From Stdlib Require Import String Ascii List.
From NM Require Export ExecutionModel.
Export ExecutionModel.Input.
Import ListNotations.
Local Open Scope string_scope.

(* ===================== Results ============================================ *)

Inductive res (A : Type) : Type :=
| Good : A -> res A
| Bad  : string -> res A.
Arguments Good {A}.
Arguments Bad {A}.

Definition rbind {A B : Type} (r : res A) (f : A -> res B) : res B :=
  match r with Good a => f a | Bad m => Bad m end.

Notation "'let*' x ':=' r 'in' k" := (rbind r (fun x => k))
  (at level 200, x pattern, r at level 100, k at level 200).

Fixpoint rmap {A B : Type} (f : A -> res B) (l : list A) : res (list B) :=
  match l with
  | [] => Good []
  | x :: r => let* y := f x in let* ys := rmap f r in Good (y :: ys)
  end.

(* ===================== Tokens ============================================= *)

Inductive sx : Type :=
| SA : string -> sx                 (* an atom *)
| SL : bool -> list sx -> sx.       (* a list; true for [ … ] *)

(* ===================== String helpers ===================================== *)

Fixpoint split_on_colons (cs : list ascii) (acc : list ascii) : list (list ascii) :=
  match cs with
  | [] => [rev acc]
  | ":"%char :: ":"%char :: r => rev acc :: split_on_colons r []
  | c :: r => split_on_colons r (c :: acc)
  end.

Definition split_path (s : string) : path :=
  map string_of_list_ascii (split_on_colons (list_ascii_of_string s) []).

Definition join_path (p : path) : string := String.concat "::" p.

(* ===================== Reading the dump =================================== *)

Definition bad {A} (what : string) : res A := Bad ("malformed dump: " ++ what).

Definition atom (x : sx) : res string :=
  match x with SA a => Good a | _ => bad "atom" end.

Definition items (x : sx) : res (list sx) :=
  match x with SL true l => Good l | _ => bad "list" end.

(* `_` is sexpr.py's None. *)
Definition opt_atom (x : sx) : res (option string) :=
  match x with SA "_" => Good None | SA a => Good (Some a) | _ => bad "optional atom" end.

Definition opt_path (x : sx) : res (option path) :=
  let* o := opt_atom x in Good (option_map split_path o).

Fixpoint read_spec (fuel : nat) (x : sx) : res spec :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "I"; p; d; a] =>
          let* p' := opt_path p in
          let* d' := opt_atom d in
          let* a' := read_app k a in
          Good (Spec p' d' a')
      | _ => bad "spec"
      end
  end
with read_app (fuel : nat) (x : sx) : res sapp :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SA "_" => Good None
      | SL true bs =>
          let* l := rmap (fun b => match b with
                                   | SL false [SA "J"; SA p; s] =>
                                       let* s' := read_spec k s in Good (split_path p, s')
                                   | _ => bad "binding"
                                   end) bs in
          Good (Some l)
      | _ => bad "application"
      end
  end.

Fixpoint read_ty (fuel : nat) (x : sx) : res aty :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "a"] => Good ATUnit
      | SL false [SA "b"] => Good ATNever
      | SL false [SA "c"] => Good ATThis
      | SL false [SA "d"; SA p; a] =>
          let* a' := read_app k a in Good (ATRef (split_path p) a')
      | SL false [SA "f"; SL true fs] =>
          let* fs' := rmap (fun f => match f with
                                     | SL false [SA "h"; SA n; t] =>
                                         let* t' := read_ty k t in Good (n, t')
                                     | _ => bad "record field type"
                                     end) fs in
          Good (ATRec fs')
      | SL false [SA "g"; SL true ms] =>
          let* ms' := rmap (read_ty k) ms in Good (ATUnion ms')
      | _ => bad "type"
      end
  end.

Definition read_opt_ty (fuel : nat) (x : sx) : res (option aty) :=
  match x with
  | SA "_" => Good None
  | _ => let* t := read_ty fuel x in Good (Some t)
  end.

Fixpoint read_pat (fuel : nat) (x : sx) : res apat :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "D"] => Good APWild
      | SL false [SA "E"; SA p; SA "_"] => Good (APPath (split_path p))
      | SL false [SA "E"; SA p; sub] =>
          let* s := read_pat k sub in Good (APTag (split_path p) s)
      | SL false [SA "F"; p; SL true fs] =>
          let* p' := opt_path p in
          let* fs' := rmap (fun f => match f with
                                     | SL false [SA "G"; SA n; SA "_"] => Good (n, None)
                                     | SL false [SA "G"; SA n; s] =>
                                         let* s' := read_pat k s in Good (n, Some s')
                                     | _ => bad "pattern field"
                                     end) fs in
          Good (APRec p' fs')
      | _ => bad "pattern"
      end
  end.

Fixpoint read_expr (fuel : nat) (x : sx) : res aexpr :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "i"] => Good AEUnit
      | SL false [SA "j"] => Good AEThis
      | SL false [SA "k"; SA p; a] =>
          let* a' := read_app k a in Good (AEPath (split_path p) a')
      | SL false [SA "l"; SL false [SA "k"; SA p; a]; SL true args] =>
          let* a' := read_app k a in
          let* args' := rmap (read_expr k) args in
          Good (AECall (split_path p) a' args')
      | SL false [SA "m"; o; SA f] =>
          let* o' := read_expr k o in Good (AEField o' f)
      | SL false [SA "n"; o; SA p; SL true args] =>
          let* o' := read_expr k o in
          let* args' := rmap (read_expr k) args in
          Good (AEMeth o' (split_path p) args')
      | SL false [SA "o"; SL false [SA "k"; SA p; a]; SL true fs] =>
          let* a' := read_app k a in
          let* fs' := rmap (fun f => match f with
                                     | SL false [SA "p"; SA n; SA "_"] => Good (n, None)
                                     | SL false [SA "p"; SA n; v] =>
                                         let* v' := read_expr k v in Good (n, Some v')
                                     | _ => bad "record field"
                                     end) fs in
          Good (AERec (split_path p) a' fs')
      | SL false [SA "q"; c; t; e] =>
          let* c' := read_expr k c in
          let* t' := read_block k t in
          let* e' := (match e with
                      | SA "_" => Good None
                      | _ => let* e1 := read_expr k e in Good (Some e1)
                      end) in
          Good (AEIf c' t' e')
      | SL false [SA "r"; s; SL true arms] =>
          let* s' := read_expr k s in
          let* arms' := rmap (fun a => match a with
                                       | SL false [SA "H"; p; b] =>
                                           let* p' := read_pat k p in
                                           let* b' := read_expr k b in Good (p', b')
                                       | _ => bad "arm"
                                       end) arms in
          Good (AEMatch s' arms')
      | SL false [SA "s"; SA "_"] => Good (AEReturn None)
      | SL false [SA "s"; e] => let* e' := read_expr k e in Good (AEReturn (Some e'))
      | SL false [SA "t"] => Good AEBreak
      | SL false (SA "u" :: _) => let* b := read_block k x in Good (AEBlock b)
      | SL false [SA "W"; SA op; e] => let* e' := read_expr k e in Good (AEUnary op e')
      | SL false [SA "X"; SA op; l; r] =>
          let* l' := read_expr k l in let* r' := read_expr k r in Good (AEBinary op l' r')
      | SL false [SA "Y"; SA t] => Good (AELit t)
      | SL false [SA "Z"; SL true ps; ret; body] =>
          let* ps' := rmap (fun p => match p with
                                     | SL false [SA "K"; SA n; t] =>
                                         let* t' := read_opt_ty k t in Good (n, t')
                                     | _ => bad "lambda parameter"
                                     end) ps in
          let* ret' := read_opt_ty k ret in
          let* b := read_expr k body in
          Good (AELam ps' ret' b)
      | _ => bad "expression"
      end
  end
with read_block (fuel : nat) (x : sx) : res ablock :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "u"; SL true ss; t] =>
          let* ss' := rmap (read_stmt k) ss in
          let* t' := (match t with
                      | SA "_" => Good None
                      | _ => let* e := read_expr k t in Good (Some e)
                      end) in
          Good (ABlock ss' t')
      | _ => bad "block"
      end
  end
with read_stmt (fuel : nat) (x : sx) : res astmt :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "v"; SA n; t; e] =>
          let* t' := read_opt_ty k t in let* e' := read_expr k e in Good (ASLet n t' e')
      | SL false [SA "w"; SA n; t; e] =>
          let* t' := read_opt_ty k t in let* e' := read_expr k e in Good (ASVar n t' e')
      | SL false [SA "x"; SA n; e] => let* e' := read_expr k e in Good (ASAssign n e')
      | SL false [SA "y"; _] => let* b := read_bind k x in Good (ASBind b)
      | SL false [SA "A"; c; b] =>
          let* c' := read_expr k c in let* b' := read_block k b in Good (ASWhile c' b')
      | SL false [SA "B"; b] => let* b' := read_block k b in Good (ASLoop b')
      | SL false [SA "C"; e] => let* e' := read_expr k e in Good (ASExpr e')
      | _ => bad "statement"
      end
  end
with read_bind (fuel : nat) (x : sx) : res (list (spec * option aexpr)) :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "y"; SL true its] =>
          rmap (fun it => match it with
                          | SL false [SA "z"; s; SA "_"] =>
                              let* s' := read_spec k s in Good (s', None)
                          | SL false [SA "z"; s; v] =>
                              let* s' := read_spec k s in
                              let* v' := read_expr k v in Good (s', Some v')
                          | _ => bad "bind item"
                          end) its
      | _ => bad "bind"
      end
  end.

Fixpoint read_decl (fuel : nat) (x : sx) : res adecl :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "M"; SL true its; SL true ds] =>
          let* its' := rmap (fun it => match it with
                                       | SL false [SA "I"; SA p; d; SA "_"] =>
                                           let* d' := opt_atom d in Good (AIHead (split_path p, d'))
                                       | SL false [SA "V"; l; r] =>
                                           let* l' := read_ty k l in
                                           let* r' := read_ty k r in Good (AIEq l' r')
                                       | _ => bad "assume item"
                                       end) its in
          let* ds' := rmap (read_decl k) ds in
          Good (ADAssume its' ds')
      | SL false [SA "N"; SA n] => Good (ADType n)
      | SL false [SA "O"; SA n; t] => let* t' := read_ty k t in Good (ADAlias n t')
      | SL false [SA "P"; SA n; SL true ps; t] =>
          let* ps' := rmap atom ps in let* t' := read_ty k t in Good (ADTag n ps' t')
      | SL false [SA "Q"; SA n] => Good (ADBareTag n)
      | SL false [SA "R"; SA n; t; i] =>
          let* t' := read_ty k t in
          let* i' := (match i with
                      | SA "_" => Good None
                      | _ => let* e := read_expr k i in Good (Some e)
                      end) in
          Good (ADVal n t' i')
      | SL false [SA "S"; SA r; SA n; SL true ps; ret; body] =>
          let rcv := match r with "." => RDetached | "_" => RPlain | _ => RAttached r end in
          let* ps' := rmap (fun p => match p with
                                     | SL false [SA "K"; SA pn; t] =>
                                         let* t' := read_ty k t in Good (pn, t')
                                     | _ => bad "parameter"
                                     end) ps in
          let* ret' := read_opt_ty k ret in
          let* body' := (match body with
                         | SA "_" => Good None
                         | _ => let* b := read_block k body in Good (Some b)
                         end) in
          Good (ADFn rcv n ps' ret' body')
      | SL false [SA "T"; SA n; SL true ss] =>
          let* ss' := rmap (fun it => match it with
                                      | SL false [SA "V"; l; r] =>
                                          let* l' := read_ty k l in
                                          let* r' := read_ty k r in Good (CIEq l' r')
                                      | _ => let* s' := read_spec k it in Good (CISpec s')
                                      end) ss in Good (ADCtx n ss')
      | SL false (SA "U" :: SA n :: rest) =>
          (* 61b7b76: name args result body; the d84 branch adds the
             brackets after the name *)
          let* (ps, a, r, b) := match rest with
                                | [SL true a; SL true r; b] => Good (None, a, r, b)
                                | [SA "_"; SL true a; SL true r; b] => Good (None, a, r, b)
                                | [SL true ps; SL true a; SL true r; b] =>
                                    let* ps' := rmap atom ps in Good (Some ps', a, r, b)
                                | _ => bad "functor"
                                end in
          let* a' := rmap (read_spec k) a in
          let* r' := rmap (read_spec k) r in
          let* b' := (match b with
                      | SA "_" => Good None
                      | SL true bs => let* l := rmap (read_bind k) bs in Good (Some l)
                      | _ => bad "functor body"
                      end) in
          Good (ADFunctor n ps a' r' b')
      | _ => bad "declaration"
      end
  end.

(* `.name>alias`: a use item, with the dots that mark detached names. *)
Definition read_use_side (s : string) : bool * name :=
  match s with
  | String "."%char r => (true, r)
  | _ => (false, s)
  end.

Fixpoint split_gt (cs : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match cs with
  | [] => (rev acc, None)
  | ">"%char :: r => (rev acc, Some r)
  | c :: r => split_gt r (c :: acc)
  end.

Definition read_use (x : sx) : res ause :=
  let* s := atom x in
  let (l, r) := split_gt (list_ascii_of_string s) [] in
  let (d, n) := read_use_side (string_of_list_ascii l) in
  Good (AUse d n (option_map (fun r => read_use_side (string_of_list_ascii r)) r)).

Definition read_import (x : sx) : res aimport :=
  match x with
  | SL false [SA "L"; SA p; a; g; SL true us] =>
      let* a' := opt_atom a in
      let* us' := rmap read_use us in
      Good (AImport p a' (match g with SA "*" => true | _ => false end) us')
  | _ => bad "import"
  end.

Definition dump_fuel : nat := 100000.

Definition read_file (x : sx) : res afile :=
  match x with
  | SL false [SA "$"; SL true is; SL true ds] =>
      let* is' := rmap read_import is in
      let* ds' := rmap (read_decl dump_fuel) ds in
      Good (AFile is' ds')
  | _ => bad "file"
  end.

(* A bundle (py/bundle.py): the entry, the prelude, and each module's path,
   whether it imports the prelude, its imports' resolved paths, its dump. *)
Definition read_module (m : sx) : res amodule :=
  match m with
  | SL false [SA "module"; SA path; SA w; SL false ts; f] =>
      let* ts' := rmap atom ts in
      let* f' := read_file f in
      Good (AModule path (String.eqb w "1") ts' f')
  | _ => bad "module"
  end.

Definition read_bundle (x : sx) : res bundle :=
  match x with
  | SL false (SA "bundle" :: SA e :: SA p :: ms) =>
      let* ms' := rmap read_module ms in
      Good (Bundle e p ms')
  | _ => bad "bundle"
  end.
Module Dump.
Import Input.
Local Open Scope string_scope.

Definition nd (tag : string) (xs : list sx) : sx := SL false (SA tag :: xs).
Definition sl (xs : list sx) : sx := SL true xs.
Definition dopt {X} (f : X -> sx) (o : option X) : sx :=
  match o with None => SA "_" | Some x => f x end.
Definition dpath (p : path) : sx := SA (join_path p).

Fixpoint dspec (s : spec) : sx :=
  match s with
  | Spec p d a =>
      nd "I" [dopt dpath p; dopt SA d;
              match a with
              | None => SA "_"
              | Some bs => sl (map (fun '(bp, bs1) => nd "J" [dpath bp; dspec bs1]) bs)
              end]
  end.

Definition dapp (a : sapp) : sx :=
  match a with
  | None => SA "_"
  | Some bs => sl (map (fun '(bp, s) => nd "J" [dpath bp; dspec s]) bs)
  end.

Fixpoint dty (t : aty) : sx :=
  match t with
  | ATNever => nd "b" []
  | ATThis => nd "c" []
  | ATUnit => nd "a" []
  | ATRec fs => nd "f" [sl (map (fun '(x, t1) => nd "h" [SA x; dty t1]) fs)]
  | ATUnion ms => nd "g" [sl (map dty ms)]
  | ATRef p a => nd "d" [dpath p; dapp a]
  end.

Fixpoint dpat (p : apat) : sx :=
  match p with
  | APWild => nd "D" []
  | APPath q => nd "E" [dpath q; SA "_"]
  | APTag q s => nd "E" [dpath q; dpat s]
  | APRec q fs => nd "F" [dopt dpath q; sl (map (fun '(x, s) => nd "G" [SA x; dopt dpat s]) fs)]
  end.

Fixpoint dexpr (e : aexpr) : sx :=
  match e with
  | AEUnit => nd "i" []
  | AEThis => nd "j" []
  | AEPath p a => nd "k" [dpath p; dapp a]
  | AECall p a args => nd "l" [nd "k" [dpath p; dapp a]; sl (map dexpr args)]
  | AEField o f => nd "m" [dexpr o; SA f]
  | AEMeth o p args => nd "n" [dexpr o; dpath p; sl (map dexpr args)]
  | AERec p a fs => nd "o" [nd "k" [dpath p; dapp a];
                            sl (map (fun '(x, v) => nd "p" [SA x; dopt dexpr v]) fs)]
  | AEIf c t el => nd "q" [dexpr c; dblock t; dopt dexpr el]
  | AEMatch s arms => nd "r" [dexpr s; sl (map (fun '(p, b) => nd "H" [dpat p; dexpr b]) arms)]
  | AEReturn o => nd "s" [dopt dexpr o]
  | AEBreak => nd "t" []
  | AEBlock b => dblock b
  | AEUnary op x => nd "W" [SA op; dexpr x]
  | AEBinary op l r => nd "X" [SA op; dexpr l; dexpr r]
  | AELit t => nd "Y" [SA t]
  | AELam ps ret body => nd "Z" [sl (map (fun '(x, t) => nd "K" [SA x; dopt dty t]) ps);
                                 dopt dty ret; dexpr body]
  end
with dblock (b : ablock) : sx :=
  match b with
  | ABlock ss t => nd "u" [sl (map dstmt ss); dopt dexpr t]
  end
with dstmt (s : astmt) : sx :=
  match s with
  | ASLet x t e => nd "v" [SA x; dopt dty t; dexpr e]
  | ASVar x t e => nd "w" [SA x; dopt dty t; dexpr e]
  | ASAssign x e => nd "x" [SA x; dexpr e]
  | ASBind its => nd "y" [sl (map (fun '(s, v) => nd "z" [dspec s; dopt dexpr v]) its)]
  | ASWhile c b => nd "A" [dexpr c; dblock b]
  | ASLoop b => nd "B" [dblock b]
  | ASExpr e => nd "C" [dexpr e]
  end.

Definition dbind (its : list (spec * option aexpr)) : sx :=
  nd "y" [sl (map (fun '(s, v) => nd "z" [dspec s; dopt dexpr v]) its)].

(* An equation ([D90]) is `(V l r)`, in an assume's or a context's items. *)
Definition dassume (it : aitem) : sx :=
  match it with
  | AIHead (p, o) => nd "I" [dpath p; dopt SA o; SA "_"]
  | AIEq l r => nd "V" [dty l; dty r]
  end.
Definition dcitem (it : citem) : sx :=
  match it with
  | CISpec s => dspec s
  | CIEq l r => nd "V" [dty l; dty r]
  end.

Fixpoint ddecl (d : adecl) : sx :=
  match d with
  | ADAssume its ds => nd "M" [sl (map dassume its); sl (map ddecl ds)]
  | ADType x => nd "N" [SA x]
  | ADAlias x t => nd "O" [SA x; dty t]
  | ADTag x ps t => nd "P" [SA x; sl (map SA ps); dty t]
  | ADBareTag x => nd "Q" [SA x]
  | ADVal x t i => nd "R" [SA x; dty t; dopt dexpr i]
  | ADFn r x ps ret body =>
      nd "S" [SA (match r with RDetached => "." | RPlain => "_" | RAttached q => q end); SA x;
              sl (map (fun '(p, t) => nd "K" [SA p; dty t]) ps); dopt dty ret; dopt dblock body]
  | ADCtx x its => nd "T" [SA x; sl (map dcitem its)]
  | ADFunctor x ps args res body =>
      nd "U" [SA x; dopt (fun l => sl (map SA l)) ps; sl (map dspec args); sl (map dspec res);
              dopt (fun bs => sl (map dbind bs)) body]
  end.

Definition duse_side (d : bool) (x : name) : string := (if d then "." else "") ++ x.
Definition duse (u : ause) : sx :=
  SA (duse_side (u_dot u) (u_name u) ++
      match u_alias u with None => "" | Some (d, y) => ">" ++ duse_side d y end).

Definition dimport (i : aimport) : sx :=
  nd "L" [SA (i_path i); dopt SA (i_alias i); SA (if i_glob i then "*" else "_");
          sl (map duse (i_uses i))].

Definition dump_file (f : afile) : sx :=
  nd "$" [sl (map dimport (f_imports f)); sl (map ddecl (f_decls f))].

End Dump.
