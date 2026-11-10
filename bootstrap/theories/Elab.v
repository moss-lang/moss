(* ------------------------------------------------------------------------ *)
(* Elab.v — elaboration of Moss in Gallina (milestone 2).                    *)
(*                                                                           *)
(* Input: a bundle of parsed modules (Sx.v).  Output: an extended Core Moss *)
(* program (MossX.v) — function bodies, functor bodies, and the platform's  *)
(* provisions for `main` — or an error.  `elaborate` is a total function:   *)
(* every recursion is on a structure or on explicit fuel, and running out   *)
(* of fuel is an error like any other.                                       *)
(*                                                                           *)
(* It follows the bootstrap's front end (bootstrap/mossc/collect.py and     *)
(* lower.py at 61b7b76) step for step, so that the two can be compared, and *)
(* departs from it where milestone 2 asks:                                   *)
(*  - abstract functors (D78) are elaborated as D84 has them: `bind F = G;`  *)
(*    is the core's EBindFun, a closure (G's inputs that F takes are linked, *)
(*    the rest captured at the bind), G's body is elaborated once, in G's    *)
(*    own terms, and `bind F;` is the core's EApply at θ, the inputs read    *)
(*    where it is.  There is no closed-world scan of candidates.  At this   *)
(*    syntax every functor is unbracketed, so P(F) = M(F) and X(F) = ∅.      *)
(*  - its output is MossX terms directly (the bootstrap's IR plus           *)
(*    translate.py, fused).                                                  *)
(*                                                                           *)
(* Deferred, with the place each slots in:                                 *)
(*  - D84 rule 7 (family members on demand): `provided_call`, at the miss.   *)
(*  - `Size` and `for`: `synth_literal` / a new statement form.              *)
(*  - bracketed functor syntax `F[P…]`: `fun_poly` (today: all of M(F)).     *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Ascii Bool Arith PrimInt63 Uint63.
From NM Require Import ExecutionModel NMap Sx ReifyData.
From NM Require Export Sx.
Import ListNotations.
Local Open Scope list_scope.
Local Open Scope string_scope.
Local Infix "+++" := List.app (right associativity, at level 60).

(* ===================== 1. Utilities ======================================= *)

Fixpoint nat_to_string_go (fuel n : nat) (acc : string) : string :=
  match fuel with
  | 0 => acc
  | S f =>
      let d := n - 10 * Nat.div n 10 in
      let acc' := String (ascii_of_nat (48 + d)) acc in
      if Nat.ltb n 10 then acc' else nat_to_string_go f (Nat.div n 10) acc'
  end.
Definition nat_to_string (n : nat) : string := nat_to_string_go 30 n "".

Fixpoint assoc {K V} (eqb : K -> K -> bool) (k : K) (l : list (K * V)) : option V :=
  match l with
  | [] => None
  | (k', v) :: r => if eqb k k' then Some v else assoc eqb k r
  end.

Definition sassoc {V} := @assoc string V String.eqb.
Definition nassoc {V} := @assoc nat V Nat.eqb.

Fixpoint nmem (x : nat) (l : list nat) : bool :=
  match l with [] => false | y :: r => Nat.eqb x y || nmem x r end.

Fixpoint smem (x : string) (l : list string) : bool :=
  match l with [] => false | y :: r => String.eqb x y || smem x r end.

(* replace or add, keeping the position of an existing key *)
Fixpoint aset {K V} (eqb : K -> K -> bool) (k : K) (v : V) (l : list (K * V)) : list (K * V) :=
  match l with
  | [] => [(k, v)]
  | (k', v') :: r => if eqb k k' then (k, v) :: r else (k', v') :: aset eqb k v r
  end.

Fixpoint suffix_of (s t : list ascii) : bool :=
  (* whether s is a suffix of t *)
  match t with
  | [] => match s with [] => true | _ => false end
  | _ :: r => if list_eq_dec ascii_dec s t then true else suffix_of s r
  end.

Definition ends_with (suf s : string) : bool :=
  suffix_of (list_ascii_of_string suf) (list_ascii_of_string s).

Definition opt_default {A} (d : A) (o : option A) : A := match o with Some a => a | None => d end.

Definition is_some {A} (o : option A) : bool := match o with Some _ => true | None => false end.

(* ===================== 2. Symbols and modules ============================= *)

Inductive kind : Type :=
| KType | KUnit | KAlias | KTag | KVal | KFn | KCtx | KMethod | KFunctor.

Definition kind_eqb (a b : kind) : bool :=
  match a, b with
  | KType, KType | KUnit, KUnit | KAlias, KAlias | KTag, KTag | KVal, KVal
  | KFn, KFn | KCtx, KCtx | KMethod, KMethod | KFunctor, KFunctor => true
  | _, _ => false
  end.

Definition kind_name (k : kind) : string :=
  match k with
  | KType => "type" | KUnit => "unit" | KAlias => "alias" | KTag => "tag"
  | KVal => "val" | KFn => "fn" | KCtx => "context" | KMethod => "method"
  | KFunctor => "functor"
  end.

(* What a symbol is: collect.Symbol.  `gs_assumes` are the assume items
   lexically enclosing the declaration, outermost first, read in gs_mod.
   Synthesized symbols (forwarders, lambda providers, captured locals' vals,
   fresh type generations) are symbols too. *)
Inductive origin : Type :=
| OSource                          (* declared in a module *)
| OFresh (of_ : nat) (by_ : nat)   (* a generation `bind F;` made of a result type *)
| OCaptured                        (* a captured local's val (D70) *)
| OForwarder (target : nat)        (* D66 *)
| OLambda                          (* D70 *)
| OPlatformFun (u : nat)           (* the platform's binding of a wasm.moss functor *)
| OLink (u g : nat).               (* D84: G linked to F, the core's functor h *)

Record gsym : Type := GSym {
  gs_id : nat;
  gs_mod : nat;
  gs_name : string;
  gs_kind : kind;
  gs_decl : adecl;
  gs_recv : option nat;
  gs_assumes : list (path * option name);
  gs_origin : origin
}.

Inductive ent : Type := ESym (s : nat) | EMod (m : nat).

Definition ent_eqb (a b : ent) : bool :=
  match a, b with
  | ESym x, ESym y => Nat.eqb x y
  | EMod x, EMod y => Nat.eqb x y
  | _, _ => false
  end.

Record gmod : Type := GMod {
  gm_path : string;
  gm_names : list (name * nat);
  gm_detached : list (name * nat);
  gm_attached : list ((nat * name) * nat);
  gm_aliases : list (name * nat);
  gm_exp_names : list (name * nat);
  gm_exp_detached : list (name * nat);
  gm_exp_aliases : list (name * nat)
}.

Definition attached_eqb (a b : nat * name) : bool :=
  Nat.eqb (fst a) (fst b) && String.eqb (snd a) (snd b).

(* The program the loader builds: modules in dependency order (imports
   before importers), and every source symbol. *)
Record world : Type := World {
  w_mods : nmap gmod;
  w_order : list nat;
  w_entry : nat;
  w_syms : nmap gsym;
  w_nsyms : nat;                    (* symbols are 1 .. w_nsyms - 1 *)
  w_files : nmap afile;
  w_library : option string
}.

Definition err_in {A} (m : gmod) (msg : string) : res A := Bad (gm_path m ++ ": " ++ msg).

(* ---- declare (collect.declare) ---- *)

Definition decl_kind (d : adecl) : option (name * kind) :=
  match d with
  | ADType n => Some (n, KType) | ADUnit n => Some (n, KUnit)
  | ADAlias n _ => Some (n, KAlias) | ADTag n _ _ => Some (n, KTag)
  | ADVal n _ _ => Some (n, KVal) | ADCtx n _ => Some (n, KCtx)
  | ADFunctor n _ _ _ _ => Some (n, KFunctor)
  | _ => None
  end.

Record decl_st : Type := DSt {
  ds_mod : gmod;
  ds_syms : nmap gsym;
  ds_next : nat;
  ds_pending : list (adecl * list (path * option name))
}.

Definition fresh_sym (st : decl_st) (mi : nat) (n : string) (k : kind) (d : adecl)
    (rcv : option nat) (asm : list (path * option name)) : nat * decl_st :=
  let id := ds_next st in
  (id, DSt (ds_mod st) (nset (ds_syms st) id (GSym id mi n k d rcv asm OSource))
           (S id) (ds_pending st)).

Definition define_name (st : decl_st) (n : name) (id : nat) : res decl_st :=
  let m := ds_mod st in
  match sassoc n (gm_names m) with
  | Some _ => err_in m ("duplicate definition of `" ++ n ++ "`")
  | None =>
      Good (DSt (GMod (gm_path m) (gm_names m +++ [(n, id)]) (gm_detached m) (gm_attached m)
                      (gm_aliases m) (gm_exp_names m) (gm_exp_detached m) (gm_exp_aliases m))
                (ds_syms st) (ds_next st) (ds_pending st))
  end.

Fixpoint declare_walk (fuel : nat) (mi : nat) (st : decl_st) (asm : list (path * option name))
    (ds : list adecl) : res decl_st :=
  match fuel with 0 => Bad "declarations nest too deeply" | S fuel =>
  match ds with
  | [] => Good st
  | d :: rest =>
      let* st' :=
        match d with
        | ADAssume its inner => declare_walk fuel mi st (asm +++ its) inner
        | ADFn RDetached n ps ret body =>
            let m := ds_mod st in
            match body with
            | Some _ => err_in m ("detached method `." ++ n ++ "` cannot have a body")
            | None =>
                match sassoc n (gm_detached m) with
                | Some _ => err_in m ("duplicate definition of `." ++ n ++ "`")
                | None =>
                    let '(id, st1) := fresh_sym st mi ("." ++ n) KMethod d None asm in
                    let m1 := ds_mod st1 in
                    Good (DSt (GMod (gm_path m1) (gm_names m1) (gm_detached m1 +++ [(n, id)])
                                    (gm_attached m1) (gm_aliases m1) (gm_exp_names m1)
                                    (gm_exp_detached m1) (gm_exp_aliases m1))
                              (ds_syms st1) (ds_next st1) (ds_pending st1))
                end
            end
        | ADFn (RAttached _) _ _ _ _ =>
            Good (DSt (ds_mod st) (ds_syms st) (ds_next st) (ds_pending st +++ [(d, asm)]))
        | ADFn RPlain n _ _ _ =>
            let '(id, st1) := fresh_sym st mi n KFn d None asm in
            define_name st1 n id
        | _ =>
            match decl_kind d with
            | Some (n, k) =>
                let '(id, st1) := fresh_sym st mi n k d None asm in
                define_name st1 n id
            | None => Good st
            end
        end in
      declare_walk fuel mi st' asm rest
  end end.

Fixpoint declare_attached (mi : nat) (st : decl_st) (ps : list (adecl * list (path * option name)))
    : res decl_st :=
  match ps with
  | [] => Good st
  | (d, asm) :: rest =>
      let* st' :=
        match d with
        | ADFn (RAttached rn) n _ _ _ =>
            let m := ds_mod st in
            match sassoc rn (gm_names m) with
            | None => err_in m ("attached method receiver `" ++ rn ++ "` is not in scope")
            | Some rid =>
                match assoc attached_eqb (rid, n) (gm_attached m) with
                | Some _ => err_in m ("duplicate definition of `" ++ rn ++ "." ++ n ++ "`")
                | None =>
                    let '(id, st1) := fresh_sym st mi (rn ++ "." ++ n) KMethod d (Some rid) asm in
                    let m1 := ds_mod st1 in
                    Good (DSt (GMod (gm_path m1) (gm_names m1) (gm_detached m1)
                                    (gm_attached m1 +++ [((rid, n), id)])
                                    (gm_aliases m1) (gm_exp_names m1)
                                    (gm_exp_detached m1) (gm_exp_aliases m1))
                              (ds_syms st1) (ds_next st1) (ds_pending st1))
                end
            end
        | _ => Good st
        end in
      declare_attached mi st' rest
  end.

Definition declare (mi : nat) (path : string) (f : afile) (syms : nmap gsym) (next : nat)
    : res (gmod * nmap gsym * nat) :=
  let st0 := DSt (GMod path [] [] [] [] [] [] []) syms next [] in
  let* st1 := declare_walk 100000 mi st0 [] (f_decls f) in
  let* st2 := declare_attached mi (DSt (ds_mod st1) (ds_syms st1) (ds_next st1) []) (ds_pending st1) in
  let m := ds_mod st2 in
  Good (GMod (gm_path m) (gm_names m) (gm_detached m) (gm_attached m) (gm_aliases m)
             (gm_names m) (gm_detached m) [], ds_syms st2, ds_next st2).

(* ---- link (collect.link) ---- *)

Definition bind_name (m : gmod) (tbl : list (name * nat)) (n : name) (s : nat)
    : res (list (name * nat)) :=
  match sassoc n tbl with
  | Some s' => if Nat.eqb s s' then Good tbl
               else err_in m ("`" ++ n ++ "` would refer to two different symbols (D44)")
  | None => Good (tbl +++ [(n, s)])
  end.

Fixpoint bind_all (m : gmod) (tbl : list (name * nat)) (xs : list (name * nat))
    : res (list (name * nat)) :=
  match xs with
  | [] => Good tbl
  | (n, s) :: r => let* t := bind_name m tbl n s in bind_all m t r
  end.

Definition with_names (m : gmod) (nm dt al enm edt eal : list (name * nat)) : gmod :=
  GMod (gm_path m) nm dt (gm_attached m) al enm edt eal.

Fixpoint link_uses (m : gmod) (target : gmod) (tidx : nat) (us : list ause) : res gmod :=
  match us with
  | [] => Good m
  | u :: rest =>
      let local := match u_alias u with Some (_, a) => a | None => u_name u end in
      let* m' :=
        if u_dot u then
          match sassoc (u_name u) (gm_exp_detached target) with
          | None => err_in m ("`" ++ gm_path target ++ "` does not export `." ++ u_name u ++ "`")
          | Some s =>
              let* dt := bind_name m (gm_detached m) local s in
              Good (with_names m (gm_names m) dt (gm_aliases m) (gm_exp_names m)
                               (aset String.eqb local s (gm_exp_detached m)) (gm_exp_aliases m))
          end
        else
          match sassoc (u_name u) (gm_exp_names target) with
          | Some s =>
              let* nm := bind_name m (gm_names m) local s in
              Good (with_names m nm (gm_detached m) (gm_aliases m)
                               (aset String.eqb local s (gm_exp_names m)) (gm_exp_detached m)
                               (gm_exp_aliases m))
          | None =>
              match sassoc (u_name u) (gm_exp_aliases target) with
              | Some sub =>
                  let* al := bind_name m (gm_aliases m) local sub in
                  Good (with_names m (gm_names m) (gm_detached m) al (gm_exp_names m)
                                   (gm_exp_detached m) (aset String.eqb local sub (gm_exp_aliases m)))
              | None => err_in m ("`" ++ gm_path target ++ "` does not export `" ++ u_name u ++ "`")
              end
          end in
      link_uses m' target tidx rest
  end.

Definition link (m : gmod) (target : gmod) (tidx : nat) (imp : aimport) : res gmod :=
  let* m1 :=
    match i_alias imp with
    | Some a =>
        let* al := bind_name m (gm_aliases m) a tidx in
        Good (with_names m (gm_names m) (gm_detached m) al (gm_exp_names m) (gm_exp_detached m)
                         (aset String.eqb a tidx (gm_exp_aliases m)))
    | None => Good m
    end in
  if i_glob imp then
    let* nm := bind_all m1 (gm_names m1) (gm_exp_names target) in
    let* dt := bind_all m1 (gm_detached m1) (gm_exp_detached target) in
    let* al := bind_all m1 (gm_aliases m1) (gm_exp_aliases target) in
    Good (with_names m1 nm dt al (gm_exp_names m1) (gm_exp_detached m1) (gm_exp_aliases m1))
  else link_uses m1 target tidx (i_uses imp).

(* Explicit imports remain strict. The implicit prelude is linked last
   using left-biased lookup, without changing what the module exports. *)
Definition prelude_fallback (m defaults : gmod) : gmod :=
  with_names m (gm_names m +++ gm_exp_names defaults)
               (gm_detached m +++ gm_exp_detached defaults)
               (gm_aliases m +++ gm_exp_aliases defaults)
               (gm_exp_names m) (gm_exp_detached m) (gm_exp_aliases m).

Fixpoint link_modules (mods : nmap gmod) (m : gmod) (ds : list (nat * aimport)) : res gmod :=
  match ds with
  | [] => Good m
  | (ti, imp) :: rest =>
      match nget mods ti with
      | Some tm => let* m' := link m tm ti imp in link_modules mods m' rest
      | None => Bad "internal: module not loaded"
      end
  end.

Definition primary_deps (has_prelude : bool) (ds : list (nat * aimport)) :=
  if has_prelude then tl ds else ds.

Definition link_imports (mods : nmap gmod) (has_prelude : bool)
    (m : gmod) (ds : list (nat * aimport)) : res gmod :=
  let* primary := link_modules mods m (primary_deps has_prelude ds) in
  if has_prelude then
    match ds with
    | (ti, _) :: _ =>
        match nget mods ti with
        | Some defaults => Good (prelude_fallback primary defaults)
        | None => Bad "internal: prelude not loaded"
        end
    | [] => Bad "internal: prelude dependency missing"
    end
  else Good primary.

(* ---- load (collect.Loader.load) ---- *)

Record load_st : Type := LSt {
  ls_loaded : list (string * nat);   (* path -> module index *)
  ls_mods : nmap gmod;
  ls_files : nmap afile;
  ls_order : list nat;
  ls_syms : nmap gsym;
  ls_next_sym : nat;
  ls_next_mod : nat
}.

Fixpoint find_module (p : string) (ms : list amodule) : option amodule :=
  match ms with
  | [] => None
  | m :: r => if String.eqb (am_path m) p then Some m else find_module p r
  end.

Fixpoint load (fuel : nat) (b : bundle) (loading : list string) (p : string) (st : load_st)
    : res (nat * load_st) :=
  match fuel with
  | 0 => Bad "imports nest too deeply"
  | S k =>
      match sassoc p (ls_loaded st) with
      | Some i => Good (i, st)
      | None =>
          if smem p loading then
            Bad (p ++ ": import cycle: " ++ String.concat " -> " (rev (p :: loading)))
          else
            match find_module p (b_mods b) with
            | None => Bad (p ++ ": no such file")
            | Some am =>
                let f := am_file am in
                let deps0 := (if am_prelude am
                              then [(b_prelude b, AImport (b_prelude b) None true [])]
                              else []) +++ combine (am_targets am) (f_imports f) in
                (* load the dependencies, in order *)
                let fix go (ds : list (string * aimport)) (st : load_st)
                    : res (list (nat * aimport) * load_st) :=
                  match ds with
                  | [] => Good ([], st)
                  | (tp, imp) :: r =>
                      let* (ti, st1) := load k b (p :: loading) tp st in
                      let* (rest, st2) := go r st1 in
                      Good ((ti, imp) :: rest, st2)
                  end in
                let* (deps, st1) := go deps0 st in
                let mi := ls_next_mod st1 in
                let* (m0, syms, next) := declare mi p f (ls_syms st1) (ls_next_sym st1) in
                let* m1 := link_imports (ls_mods st1) (am_prelude am) m0 deps in
                Good (mi, LSt ((p, mi) :: ls_loaded st1) (nset (ls_mods st1) mi m1)
                              (nset (ls_files st1) mi f)
                              (ls_order st1 +++ [mi]) syms next (S mi))
            end
      end
  end.

Definition load_world (b : bundle) : res world :=
  let* (e, st) := load 10000 b [] (b_entry b) (LSt [] NE NE [] NE 1 0) in
  Good (World (ls_mods st) (ls_order st) e (ls_syms st) (ls_next_sym st) (ls_files st) (Loader.library_dir (b_prelude b))).

(* ---- resolution (collect.resolve_path, resolve_detached) ---- *)

Definition get_mod (w : world) (i : nat) : gmod :=
  opt_default (GMod "?" [] [] [] [] [] [] []) (nget (w_mods w) i).

Definition resolve_path (w : world) (mi : nat) (p : path) : res (option ent) :=
  let m := get_mod w mi in
  match p with
  | [] => Good None
  | hd :: rest =>
      let first := match sassoc hd (gm_names m) with
                   | Some s => Some (ESym s)
                   | None => option_map EMod (sassoc hd (gm_aliases m))
                   end in
      match first with
      | None => Good None
      | Some e0 =>
          let fix go (cur : ent) (hd : string) (segs : list string) : res (option ent) :=
            match segs with
            | [] => Good (Some cur)
            | seg :: r =>
                match cur with
                | ESym _ => err_in m ("`" ++ hd ++ "` is not a module")
                | EMod ci =>
                    let cm := get_mod w ci in
                    match sassoc seg (gm_exp_names cm) with
                    | Some s => go (ESym s) seg r
                    | None =>
                        match sassoc seg (gm_exp_aliases cm) with
                        | Some s => go (EMod s) seg r
                        | None => err_in m ("`" ++ gm_path cm ++ "` does not export `" ++ seg ++ "`")
                        end
                    end
                end
            end in
          go e0 hd rest
      end
  end.

Fixpoint removelast_path (p : path) : path * option name :=
  match p with
  | [] => ([], None)
  | [x] => ([], Some x)
  | x :: r => let '(a, l) := removelast_path r in (x :: a, l)
  end.

Definition resolve_detached (w : world) (mi : nat) (p : path) : res (option nat) :=
  match p with
  | [n] => Good (sassoc n (gm_detached (get_mod w mi)))
  | _ =>
      let '(pre, last) := removelast_path p in
      match last with
      | None => Good None
      | Some n =>
          let* t := resolve_path w mi pre in
          match t with
          | Some (EMod ti) => Good (sassoc n (gm_exp_detached (get_mod w ti)))
          | _ => Good None
          end
      end
  end.

(* ===================== 3. Elaborated types (types.py) ===================== *)

(* A type as elaboration knows it.  YAbs g is a *generation* (D72): the
   parameter of an assumed type symbol (g is the symbol's own id — what
   types.param(T) is to the bootstrap) or a fresh generation `bind F;`
   made.  A declared type symbol's current meaning is read from the
   environment's name table, never from the type. *)
Inductive ety : Type :=
| YUnit | YNever
| YAbs   : nat -> ety
| YNom   : nat -> list (nat * ety) -> ety
| YUnion : list ety -> ety
| YRec   : list (string * ety) -> ety.

Fixpoint ety_eqb (a b : ety) {struct a} : bool :=
  match a, b with
  | YUnit, YUnit | YNever, YNever => true
  | YAbs x, YAbs y => Nat.eqb x y
  | YNom n xs, YNom m ys =>
      Nat.eqb n m &&
      (fix go (xs ys : list (nat * ety)) : bool :=
         match xs, ys with
         | [], [] => true
         | (s, t) :: r, (s', t') :: r' => Nat.eqb s s' && ety_eqb t t' && go r r'
         | _, _ => false
         end) xs ys
  | YUnion xs, YUnion ys =>
      (fix go (xs ys : list ety) : bool :=
         match xs, ys with
         | [], [] => true
         | t :: r, t' :: r' => ety_eqb t t' && go r r'
         | _, _ => false
         end) xs ys
  | YRec xs, YRec ys =>
      (fix go (xs ys : list (string * ety)) : bool :=
         match xs, ys with
         | [], [] => true
         | (s, t) :: r, (s', t') :: r' => String.eqb s s' && ety_eqb t t' && go r r'
         | _, _ => false
         end) xs ys
  | _, _ => false
  end.

Definition head (t : ety) : option nat :=
  match t with YNom n _ => Some n | YAbs g => Some g | _ => None end.

Definition members (t : ety) : list ety :=
  match t with YUnion ms => ms | YNever => [] | _ => [t] end.

Fixpoint ety_mem (t : ety) (l : list ety) : bool :=
  match l with [] => false | x :: r => ety_eqb t x || ety_mem t r end.

(* types.fits: D17, equality or divergence or injection into a union *)
Definition fits_raw (t expected : ety) : bool :=
  ety_eqb t expected || match t with YNever => true | _ => false end ||
  match expected with
  | YUnion _ => forallb (fun m => ety_mem m (members expected)) (members t)
  | _ => false
  end.

Definition FUEL : nat := 5000.

(* The generations a type mentions. *)
Fixpoint ety_gens (t : ety) : list nat :=
  match t with
  | YAbs g => [g]
  | YNom _ xs => (fix go (l : list (nat * ety)) := match l with [] => [] | p :: r => ety_gens (snd p) +++ go r end) xs
  | YUnion ms => (fix go (l : list ety) := match l with [] => [] | m :: r => ety_gens m +++ go r end) ms
  | YRec fs => (fix go (l : list (string * ety)) := match l with [] => [] | p :: r => ety_gens (snd p) +++ go r end) fs
  | _ => []
  end.

(* A traversal budget computed from the fully elaborated type, including
   any structure introduced by canonicalization. There is no fixed cap. *)
Fixpoint ety_depth(t:ety):nat:=S(match t with
 |YNom _ xs=>fold_right Nat.max 0(map(fun q=>ety_depth(snd q))xs)
 |YUnion ms=>fold_right Nat.max 0(map ety_depth ms)
 |YRec fs=>fold_right Nat.max 0(map(fun q=>ety_depth(snd q))fs)
 |_=>0 end).

(* Substituting generations (a functor result read at fresh ones). *)
Fixpoint ety_subst (fuel : nat) (s : list (nat * ety)) (t : ety) : ety :=
  match fuel with
  | 0 => t
  | S f =>
      match t with
      | YAbs g => match nassoc g s with Some u => u | None => t end
      | YNom n xs => YNom n (map (fun p => (fst p, ety_subst f s (snd p))) xs)
      | YUnion ms => YUnion (map (ety_subst f s) ms)
      | YRec fs => YRec (map (fun p => (fst p, ety_subst f s (snd p))) fs)
      | _ => t
      end
  end.

Definition ety_subst_full(s:list(nat*ety))(t:ety):ety:=
 ety_subst(ety_depth t)s t.

(* ===================== 4. Environments (lower.Env) ======================== *)

Inductive key : Type :=
| KyVal : nat -> key
| KyFn  : nat -> key
| KyMth : ety -> nat -> key     (* receiver type (canonical), method symbol *)
| KyFun : nat -> key.

Definition key_eqb (a b : key) : bool :=
  match a, b with
  | KyVal x, KyVal y | KyFn x, KyFn y | KyFun x, KyFun y => Nat.eqb x y
  | KyMth t m, KyMth t' m' => Nat.eqb m m' && ety_eqb t t'
  | _, _ => false
  end.

Fixpoint key_mem (k : key) (l : list key) : bool :=
  match l with [] => false | x :: r => key_eqb k x || key_mem k r end.

Record fsig : Type := FSig { sg_params : list (string * ety); sg_ret : ety; sg_this : option ety }.

(* An instantiation: for each type symbol, what it denoted (None: not in force). *)
Definition inst := list (nat * option ety).

Inductive lmut : Type := LLet | LVar | LCaptured.

Record env : Type := Env {
  en_tn    : list (nat * (ety * nat));        (* declared type symbol -> meaning, stamp *)
  en_uf    : list (nat * ety);                (* generation -> what it was merged with *)
  en_vals  : nmap ety;
  en_fns   : nmap fsig;
  en_mths  : nmap (list (ety * fsig));          (* method -> receiver -> signature *)
  en_funs  : nmap nat;                          (* abstract functor -> stamp *)
  en_locals : list (string * (ety * lmut));
  en_apps  : list (key * (inst * nat))          (* provision -> instantiation, stamp *)
}.

Definition empty_env : env := Env [] [] NE NE NE NE [] [].

Definition tn_get (e : env) (t : nat) : option ety := option_map fst (nassoc t (en_tn e)).
Definition tymap (e : env) : list (nat * ety) := map (fun p => (fst p, fst (snd p))) (en_tn e).

Definition set_tn (e : env) (t : nat) (y : ety) (stamp : nat) : env :=
  Env (aset Nat.eqb t (y, stamp) (en_tn e)) (en_uf e) (en_vals e) (en_fns e) (en_mths e)
      (en_funs e) (en_locals e) (en_apps e).
Definition set_uf (e : env) (g : nat) (y : ety) : env :=
  Env (en_tn e) (aset Nat.eqb g y (en_uf e)) (en_vals e) (en_fns e) (en_mths e)
      (en_funs e) (en_locals e) (en_apps e).
Definition set_val (e : env) (v : nat) (y : ety) : env :=
  Env (en_tn e) (en_uf e) (nset (en_vals e) v y) (en_fns e) (en_mths e)
      (en_funs e) (en_locals e) (en_apps e).
Definition set_fn (e : env) (f : nat) (s : fsig) : env :=
  Env (en_tn e) (en_uf e) (en_vals e) (nset (en_fns e) f s) (en_mths e)
      (en_funs e) (en_locals e) (en_apps e).
Definition get_mth (e : env) (t : ety) (m : nat) : option fsig :=
  match nget (en_mths e) m with
  | Some l => assoc ety_eqb t l
  | None => None
  end.
Definition set_mth (e : env) (t : ety) (m : nat) (s : fsig) : env :=
  let l := opt_default [] (nget (en_mths e) m) in
  Env (en_tn e) (en_uf e) (en_vals e) (en_fns e) (nset (en_mths e) m (aset ety_eqb t s l))
      (en_funs e) (en_locals e) (en_apps e).
Definition set_fun (e : env) (u : nat) (stamp : nat) : env :=
  Env (en_tn e) (en_uf e) (en_vals e) (en_fns e) (en_mths e)
      (nset (en_funs e) u stamp) (en_locals e) (en_apps e).
Definition set_local (e : env) (x : string) (y : ety) (mu : lmut) : env :=
  Env (en_tn e) (en_uf e) (en_vals e) (en_fns e) (en_mths e)
      (en_funs e) (aset String.eqb x (y, mu) (en_locals e)) (en_apps e).
Definition with_locals (e : env) (ls : list (string * (ety * lmut))) : env :=
  Env (en_tn e) (en_uf e) (en_vals e) (en_fns e) (en_mths e) (en_funs e) ls (en_apps e).
Definition get_app (e : env) (k : key) : option (inst * nat) := assoc key_eqb k (en_apps e).
Definition set_app (e : env) (k : key) (a : inst) (stamp : nat) : env :=
  Env (en_tn e) (en_uf e) (en_vals e) (en_fns e) (en_mths e)
      (en_funs e) (en_locals e) (aset key_eqb k (a, stamp) (en_apps e)).

(* every provision in force, as the keys a lambda's needs list them *)
Fixpoint nmap_elems {A} (m : nmap A) (base step : nat) : list (nat * A) :=
  (* Traverse the whole finite trie.  A fixed depth would silently omit
     provisions from sufficiently large worlds.  Preserve preorder. *)
  match m with
  | NE => []
  | NN l v r =>
      (match v with Some x => [(base, x)] | None => [] end)
      +++ nmap_elems l (base + step) (2 * step)
      +++ nmap_elems r (base + 2 * step) (2 * step)
  end.
Definition elems {A} (m : nmap A) : list (nat * A) := nmap_elems m 0 1.

Definition all_keys (e : env) : list key :=
  map (fun p => KyVal (fst p)) (elems (en_vals e))
  +++ map (fun p => KyFn (fst p)) (elems (en_fns e))
  +++ flat_map (fun p => map (fun q => KyMth (fst q) (fst p)) (snd p)) (elems (en_mths e))
  +++ map (fun p => KyFun (fst p)) (elems (en_funs e)).

(* ===================== 5. The elaboration monad =========================== *)

Inductive xstmt : Type :=
| SLet      : nat -> xexpr -> xstmt
| SAssign   : nat -> xexpr -> xstmt
| SBindVal  : nat -> xexpr -> xstmt
| SBindFn   : nat -> nat -> smap -> xstmt
| SBindMth  : ty -> nat -> subst -> nat -> smap -> xstmt
| SBindFun  : nat -> nat -> smap -> smap -> xstmt
| SBindFunA : nat -> nat -> smap -> xstmt
| SApply    : nat -> subst -> smap -> xstmt
| SBindPrim : item -> prim -> xstmt
| SBindTy   : nat -> ty -> xstmt          (* `bind T = τ;`: no run-time effect *)
| SWhile    : xexpr -> xexpr -> xstmt
| SLoop     : xexpr -> xstmt
| SExpr     : xexpr -> xstmt.

Fixpoint fold_stmts (ss : list xstmt) (tail : xexpr) : xexpr :=
  match ss with
  | [] => tail
  | s :: r =>
      let rest := fold_stmts r tail in
      match s with
      | SLet x e => XLet x e rest
      | SAssign x e => XSeq (XAssign x e) rest
      | SBindVal v e => XBindVal v e rest
      | SBindFn f f' sm => XBindFn f f' sm rest
      | SBindMth t m th f' sm => XBindMth t m th f' sm rest
      | SBindFun u h sm smc => XBindFun u h sm smc rest
      | SBindFunA u w sm => XBindFunA u w sm rest
      | SApply u th sm => XApply u th sm rest
      | SBindPrim i p => XBindPrim i p rest
      | SBindTy t tau => XBindTy t tau rest
      | SWhile c b => XSeq (XWhile c b) rest
      | SLoop b => XSeq (XLoop b) rest
      | SExpr e => XSeq e rest
      end
  end.

Definition stmt_bind (s : xstmt) : option xbind :=
  match s with
  | SBindVal v e => Some (XBVal v e)
  | SBindFn f f' sm => Some (XBFn f f' sm)
  | SBindMth t m th f' sm => Some (XBMth t m th f' sm)
  | SBindFun u h sm smc => Some (XBFun u h sm smc)
  | SBindFunA u w sm => Some (XBFunA u w sm)
  | SApply u th sm => Some (XBApply u th sm)
  | SBindPrim i p => Some (XBPrim i p)
  | SBindTy t tau => Some (XBTy t tau)
  | _ => None
  end.

(* What lowering a function's assume nesting gives (lower.base_env and the
   bookkeeping lower_fn does before the body). *)
Record fninfo : Type := FInfo {
  fi_env : env;
  fi_needs : list key;
  fi_apps : list (key * inst);
  fi_merges : list (nat * ety)     (* D43 edges its assumes made, on generations *)
}.

Record st : Type := St {
  st_w : world;
  st_syms : nmap gsym;
  st_next : nat;
  st_reqs : nmap (list nat);
  st_sigsyms : nmap (list nat);
  st_info : nmap fninfo;
  st_fns : list (nat * xexpr);
  st_funs : list (nat * (tele * list xbind));
  st_fwd : nmap nat;
  st_links : list ((nat * nat) * nat);
  st_flds : list (string * nat);
  st_consts : nmap (xexpr * ety);
  st_wrappers : nmap nat;
  st_vars : list (string * nat);
  st_nvar : nat;
  st_memory_bound : bool;
  st_memory_used : bool;
  st_checked_total : list nat;
  st_lang : list (string * nat);   (* lib/{num,int,float,bool,wasm}.moss *)
  st_unsup : list string;          (* constructs the oracle does not model *)
  st_reify : reify_metadata ety key        (* checked metadata for immutable lowering *)
}.

Definition M (A : Type) : Type := st -> res (A * st).

Definition ret {A} (a : A) : M A := fun s => Good (a, s).
Definition mbind {A B} (m : M A) (f : A -> M B) : M B :=
  fun s => match m s with Good (a, s') => f a s' | Bad e => Bad e end.
Definition fail {A} (msg : string) : M A := fun _ => Bad msg.
Definition lift {A} (r : res A) : M A := fun s => match r with Good a => Good (a, s) | Bad e => Bad e end.
Definition get : M st := fun s => Good (s, s).
Definition put (s : st) : M unit := fun _ => Good (tt, s).

Notation "'let!' x ':=' m 'in' k" := (mbind m (fun x => k))
  (at level 200, x pattern, m at level 100, k at level 200).
Notation "m ;; k" := (mbind m (fun _ => k)) (at level 100, right associativity).

Fixpoint mmap {A B} (f : A -> M B) (l : list A) : M (list B) :=
  match l with
  | [] => ret []
  | x :: r => let! y := f x in let! ys := mmap f r in ret (y :: ys)
  end.

Fixpoint miter {A} (f : A -> M unit) (l : list A) : M unit :=
  match l with [] => ret tt | x :: r => f x ;; miter f r end.

Fixpoint mfold {A B} (f : A -> B -> M A) (a : A) (l : list B) : M A :=
  match l with [] => ret a | x :: r => let! a' := f a x in mfold f a' r end.

Definition upd (f : st -> st) : M unit := fun s => Good (tt, f s).

Definition fresh : M nat :=
  fun s => Good (st_next s,
    St (st_w s) (st_syms s) (S (st_next s)) (st_reqs s) (st_sigsyms s) (st_info s)
       (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
       (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
       (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s)).

(* record updates, one field each *)
Definition s_syms (f : nmap gsym -> nmap gsym) (s : st) : st :=
  St (st_w s) (f (st_syms s)) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_reqs (f : nmap (list nat) -> nmap (list nat)) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (f (st_reqs s)) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_sigsyms (f : nmap (list nat) -> nmap (list nat)) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (f (st_sigsyms s)) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_info (f : nmap fninfo -> nmap fninfo) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (f (st_info s))
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_fns (f : list (nat * xexpr) -> list (nat * xexpr)) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (f (st_fns s)) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_funs (f : list (nat * (tele * list xbind)) -> list (nat * (tele * list xbind))) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (f (st_funs s)) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_fwd (f : nmap nat -> nmap nat) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (f (st_fwd s)) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_links (f : list ((nat * nat) * nat) -> list ((nat * nat) * nat)) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (f (st_links s)) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_flds (f : list (string * nat) -> list (string * nat)) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (f (st_flds s)) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_consts (f : nmap (xexpr * ety) -> nmap (xexpr * ety)) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (f (st_consts s))
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_wrappers (f : nmap nat -> nmap nat) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (f (st_wrappers s)) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_vars (vs : list (string * nat)) (n : nat) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) vs n (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_memory (b u : bool) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (b || st_memory_bound s) (u || st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (st_reify s).
Definition s_checked (f : list nat -> list nat) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (f (st_checked_total s)) (st_lang s) (st_unsup s) (st_reify s).

Definition s_reify (f : reify_metadata ety key -> reify_metadata ety key) (s : st) : st :=
  St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s) (f (st_reify s)).

Definition in_reify_code {A} (owner : reify_owner) (m : M A) : M A :=
  let! before := get in
  upd (s_reify (fun r =>
      ReifyMetadata (Some owner) (rm_codes r) (rm_declarations r) (rm_constants r))) ;;
  let! a := m in
  upd (s_reify (fun r =>
      ReifyMetadata (rm_owner (st_reify before))
        (rm_codes r) (rm_declarations r) (rm_constants r))) ;;
  ret a.

Definition dummy_sym : gsym := GSym 0 0 "?" KUnit (ADUnit "?") None [] OSource.

Definition sym (s : nat) : M gsym :=
  fun x => match nget (st_syms x) s with
           | Some g => Good (g, x)
           | None => Bad ("internal: no symbol " ++ nat_to_string s)
           end.

Definition modpath (mi : nat) : M string := fun s => Good (gm_path (get_mod (st_w s) mi), s).

Definition err {A} (mi : nat) (msg : string) : M A :=
  let! p := modpath mi in fail (p ++ ": " ++ msg)%string.

Definition rpath (mi : nat) (p : path) : M (option ent) :=
  fun s => match resolve_path (st_w s) mi p with
           | Good r => Good (r, s)
           | Bad e => Bad e
           end.

Definition rdetached (mi : nat) (p : path) : M (option nat) :=
  fun s => match resolve_detached (st_w s) mi p with
           | Good r => Good (r, s)
           | Bad e => Bad e
           end.

Definition the_mod (mi : nat) : M gmod := fun s => Good (get_mod (st_w s) mi, s).

Definition new_sym (mi : nat) (n : string) (k : kind) (d : adecl) (o : origin) : M nat :=
  let! id := fresh in
  upd (s_syms (fun t => nset t id (GSym id mi n k d None [] o))) ;;
  ret id.

Definition fld_id (f : string) : M nat :=
  let! s := get in
  match sassoc f (st_flds s) with
  | Some n => ret n
  | None => let n := S (List.length (st_flds s)) in
            upd (s_flds (fun l => l +++ [(f, n)])) ;; ret n
  end.

Definition lang (short : string) : M (option nat) :=
  let! s := get in ret (sassoc short (st_lang s)).

(* ===================== 6. Requirements (lower.type_requirements) ========= *)

Definition show_path (p : path) : string := join_path p.

Definition resolve_sym (mi : nat) (p : path) : M (option gsym) :=
  let! r := rpath mi p in
  match r with
  | Some (ESym s) => let! g := sym s in ret (Some g)
  | _ => ret None
  end.

Definition decl_ty (d : adecl) : option aty :=
  match d with ADTag _ _ t => Some t | ADAlias _ t => Some t | ADVal _ t _ => Some t | _ => None end.

(* the tags and aliases a type expression names, brackets included *)
Fixpoint mentioned_nominals (fuel : nat) (t : aty) (mi : nat) : M (list nat) :=
  match fuel with
  | 0 => fail "type nests too deeply"
  | S f =>
      match t with
      | ATRec fs => let! ls := mmap (fun p => mentioned_nominals f (snd p) mi) fs in ret (List.concat ls)
      | ATUnion ms => let! ls := mmap (fun m => mentioned_nominals f m mi) ms in ret (List.concat ls)
      | ATRef p a =>
          let! tg := resolve_sym mi p in
          let here := match tg with
                      | Some g => if kind_eqb (gs_kind g) KTag || kind_eqb (gs_kind g) KAlias
                                  then [gs_id g] else []
                      | None => []
                      end in
          let! more := mmap (fun b => match snd b with
                                      | Spec (Some sp) _ sa => mentioned_nominals f (ATRef sp sa) mi
                                      | _ => ret []
                                      end) (opt_default [] a) in
          ret (here +++ List.concat more)
      | _ => ret []
      end
  end.

(* the type symbols of an assume nesting, through contexts (V.1's
   SpecTypes): depth first, entering each context once (`fst acc`, the
   contexts seen), so contexts nest to any depth and a cycle of them reads
   as the union of their items (ExecutionModel note 30).  The types come out in
   order of first reaching, as the walk of every context each time did on
   the nestings it accepted.  Each call enters a context not seen before,
   so `walk_fuel` (one more than the symbols) is never used up. *)
Fixpoint walk_specs (fuel : nat) (mi : nat) (specs : list spec) (acc : list nat * list nat)
    : M (list nat * list nat) :=
  match fuel with
  | 0 => fail "internal: the context walk ran out of fuel"
  | S f =>
      mfold (fun acc sp =>
               match sp with
               | Spec (Some p) None _ =>
                   let! tg := resolve_sym mi p in
                   match tg with
                   | Some g =>
                       if kind_eqb (gs_kind g) KType then
                         ret (fst acc, if nmem (gs_id g) (snd acc) then snd acc else snd acc +++ [gs_id g])
                       else if kind_eqb (gs_kind g) KCtx then
                         if nmem (gs_id g) (fst acc) then ret acc
                         else match gs_decl g with
                              | ADCtx _ its => walk_specs f (gs_mod g) its (gs_id g :: fst acc, snd acc)
                              | _ => ret acc
                              end
                       else ret acc
                   | None => ret acc
                   end
               | _ => ret acc
               end) acc specs
  end.

Definition walk_fuel : M nat := let! s := get in ret (S (w_nsyms (st_w s))).

(* the type symbols a run of context items names, through contexts *)
Definition nesting_types (mi : nat) (specs : list spec) : M (list nat) :=
  let! n := walk_fuel in
  let! r := walk_specs n mi specs ([], []) in
  ret (snd r).

(* an assume item `p` (not `R.m`) is the context item `p` *)
Definition assume_specs (its : list (path * option name)) : list spec :=
  flat_map (fun it => match it with (p, None) => [Spec (Some p) None None] | _ => [] end) its.

Definition assume_type_syms (g : gsym) : M (list nat) :=
  nesting_types (gs_mod g) (assume_specs (gs_assumes g)).

Definition cached_reqs (s : nat) : M (option (list nat)) :=
  let! x := get in ret (nget (st_reqs x) s).
Definition set_reqs (s : nat) (l : list nat) : M unit := upd (s_reqs (fun m => nset m s l)).

(* a tag's or alias's requirements, during solve_requirements.  Its group
   holds only tags and aliases and every list it computes is without
   repeats, so the guard and the dedup change nothing; they make the cache
   invariant local (Track A, ElabFrame.v) *)
Definition set_nominal_reqs (s : nat) (l : list nat) : M unit :=
  let! g := sym s in
  if kind_eqb (gs_kind g) KTag || kind_eqb (gs_kind g) KAlias then set_reqs s (nodup Nat.eq_dec l) else ret tt.

(* free_type_syms, against the current approximation of relevance *)
Fixpoint free_type_syms (fuel : nat) (t : aty) (mi : nat) (out : list nat) : M (list nat) :=
  match fuel with
  | 0 => fail "type nests too deeply"
  | S f =>
      match t with
      | ATRec fs => mfold (fun o p => free_type_syms f (snd p) mi o) out fs
      | ATUnion ms => mfold (fun o m => free_type_syms f m mi o) out ms
      | ATRef p a =>
          let! tg := resolve_sym mi p in
          match tg with
          | None => ret out
          | Some g =>
              let bs := opt_default [] a in
              let! bound := mmap (fun b => let! l := resolve_sym mi (fst b) in
                                           ret (match l with Some x => [gs_id x] | None => [] end)) bs in
              let bound := List.concat bound in
              let! out := mfold (fun o b => match snd b with
                                            | Spec (Some sp) _ sa => free_type_syms f (ATRef sp sa) mi o
                                            | _ => ret o
                                            end) out bs in
              if kind_eqb (gs_kind g) KType then
                ret (if nmem (gs_id g) bound || nmem (gs_id g) out then out else out +++ [gs_id g])
              else
                let! reqs := type_requirements f (gs_id g) in
                ret (fold_left (fun o r => if nmem r bound || nmem r o then o else o +++ [r]) reqs out)
          end
      | _ => ret out
      end
  end
with type_requirements (fuel : nat) (s : nat) : M (list nat) :=
  match fuel with
  | 0 => fail "relevance nests too deeply"
  | S f =>
      let! c := cached_reqs s in
      match c with
      | Some l => ret l
      | None =>
          let! g := sym s in
          match gs_kind g with
          | KUnit | KType => set_reqs s [] ;; ret []
          | KTag | KAlias => solve_requirements f g ;; let! c := cached_reqs s in ret (opt_default [] c)
          | _ => let! out := assume_type_syms g in set_reqs s out ;; ret out
          end
      end
  end
with solve_requirements (fuel : nat) (root : gsym) : M unit :=
  match fuel with
  | 0 => fail "relevance nests too deeply"
  | S f =>
      (* the group: every tag or alias reachable, not yet known *)
      let fix collect (n : nat) (stack : list nat) (seen : list nat) : M (list nat) :=
        match n with
        | 0 => fail "relevance: too many declarations"
        | S n' =>
            match stack with
            | [] => ret (rev seen)
            | s :: rest =>
                let! c := cached_reqs s in
                if nmem s seen || is_some c then collect n' rest seen
                else
                  let! g := sym s in
                  let! more := match decl_ty (gs_decl g) with
                               | Some t => mentioned_nominals 200 t (gs_mod g)
                               | None => ret []
                               end in
                  collect n' (more +++ rest) (s :: seen)
            end
        end in
      let! group := collect 100000 [gs_id root] [] in
      miter (fun s => set_nominal_reqs s []) group ;;
      let! listed := mmap (fun s => let! g := sym s in let! l := listed_params f g in ret (s, l)) group in
      let fix iterate (n : nat) : M unit :=
        match n with
        | 0 => fail "relevance does not converge"
        | S n' =>
            let! changed := mfold (fun ch s =>
                let! g := sym s in
                let base := opt_default [] (nassoc s listed) in
                let! found := match decl_ty (gs_decl g) with
                              | Some t => free_type_syms f t (gs_mod g) base
                              | None => ret base
                              end in
                let! old := cached_reqs s in
                if list_eq_dec Nat.eq_dec found (opt_default [] old) then ret ch
                else set_nominal_reqs s found ;; ret true) false group in
            if changed then iterate n' else ret tt
        end in
      iterate 10000
  end
with listed_params (fuel : nat) (g : gsym) : M (list nat) :=
  match gs_decl g with
  | ADTag _ ((_ :: _) as names) _ =>
      let! nesting := assume_type_syms g in
      mfold (fun out n =>
               let! t := resolve_sym (gs_mod g) [n] in
               match t with
               | Some t' =>
                   if nmem (gs_id t') nesting then
                     ret (if nmem (gs_id t') out then out else out +++ [gs_id t'])
                   else err (gs_mod g) ("`" ++ gs_name g ++ "` lists `" ++ n ++
                                        "`, which is not a type its assume nesting requires (D73)")
               | None => err (gs_mod g) ("`" ++ gs_name g ++ "` lists `" ++ n ++
                                         "`, which is not a type its assume nesting requires (D73)")
               end) [] names
  | _ => ret []
  end.

Definition reqs (s : nat) : M (list nat) := type_requirements 200 s.

(* Ascending insertion sort of symbol numbers, without repeats: the fixed order of a
   method's signature types outside its assume nesting (AppOf). *)
Fixpoint nsort_insert (x : nat) (l : list nat) : list nat :=
  match l with
  | [] => [x]
  | y :: r => if Nat.ltb x y then x :: l else if Nat.eqb x y then l else y :: nsort_insert x r
  end.
Definition nsort (l : list nat) : list nat := fold_right nsort_insert [] l.

(* The type symbols a method's provision is made at (its θ): the
   requirements its signature mentions, in their fixed order, then the
   other type symbols it mentions, ascending ([D20]: a method's signature
   is closed over its requirements, which include every type symbol it
   mentions). *)
(* Recompute from the declaration: a memo entry is not an independent
   source of signature requirements. Keep the write for diagnostics. *)
Definition signature_type_syms (g : gsym) (rs : list nat) : M (list nat) :=
  let! found :=
    match gs_decl g with
    | ADFn _ _ ps ret_ _ =>
        let! o := mfold (fun o p => free_type_syms 200 (snd p) (gs_mod g) o) [] ps in
        match ret_ with Some r => free_type_syms 200 r (gs_mod g) o | None => ret o end
    | _ => ret []
    end in
  let l := filter (fun t => nmem t found) rs +++ nsort (filter (fun t => negb (nmem t rs)) found) in
  upd (s_sigsyms (fun m => nset m (gs_id g) l)) ;; ret l.

(* ===================== 7. Merging and type elaboration =================== *)

(* The merges (en_uf) are triangular: unify maps a generation not merged
   yet to a canonical type that does not contain it, so an entry's type
   mentions no generation merged at or before it.  Following them from a
   type therefore takes at most as many steps along any path as there are
   merges, and canon's fuel counts only those steps, not the type's depth:
   it cannot run out on a type it has not resolved (ExecutionModel note 55). *)
Fixpoint canon_go (fuel : nat) (uf : list (nat * ety)) (t : ety) {struct fuel} : ety :=
  match fuel with
  | 0 => t
  | S f =>
      (fix go (t : ety) : ety :=
         match t with
         | YAbs g =>
             match nassoc g uf with
             | Some t' => if ety_eqb t' t then t else canon_go f uf t'
             | None => t
             end
         | YNom n xs => YNom n (map (fun p => (fst p, go (snd p))) xs)
         | YUnion ms => YUnion (map go ms)
         | YRec fs => YRec (map (fun p => (fst p, go (snd p))) fs)
         | _ => t
         end) t
  end.
Definition canon (e : env) (t : ety) : ety := canon_go (S (List.length (en_uf e))) (en_uf e) t.

Definition reify_sig_raw (sg : fsig) : reify_signature ety :=
  ReifySignature ety (map snd (sg_params sg)) (sg_ret sg) (sg_this sg).
Definition reify_sig_types (sg : fsig) : list ety :=
  map snd (sg_params sg) +++ sg_ret sg ::
    match sg_this sg with Some t => [t] | None => [] end.

Definition reify_sig (e : env) (sg : fsig) : reify_signature ety :=
  ReifySignature ety (map (fun p => canon e (snd p)) (sg_params sg))
    (canon e (sg_ret sg)) (option_map (canon e) (sg_this sg)).

Definition reify_arguments (e : env) : list (nat * ety) :=
  map (fun p => (fst p,canon e (snd p))) (tymap e).

Definition reify_scope_types(e:env):list ety:= map snd (tymap e) +++ map snd (en_uf e) ++
    map snd (elems (en_vals e)) ++
    flat_map (fun p => reify_sig_types (snd p)) (elems (en_fns e)) ++
    flat_map (fun p => flat_map (fun q => fst q :: reify_sig_types (snd q)) (snd p))
      (elems (en_mths e)) ++
    map (fun p => fst (snd p)) (en_locals e) ++
    flat_map (fun p => flat_map (fun q => match snd q with
       Some t => [t] | None => [] end) (fst (snd p))) (en_apps e).
(* Test membership in the already deduplicated suffix. This preserves
   nodup's last-occurrence order while avoiding repeated scans of duplicates. *)
Definition dedup_nat_step (ys : list nat) (x : nat) : list nat :=
 if in_dec Nat.eq_dec x ys then ys else x::ys.
Definition dedup_nat_fast (xs : list nat) : list nat :=
 fold_left dedup_nat_step (rev_append xs []) [].
Lemma dedup_nat_fast_fold : forall xs,
 dedup_nat_fast xs = fold_right (fun x ys => dedup_nat_step ys x) [] xs.
Proof.
 intro xs. unfold dedup_nat_fast. rewrite rev_append_rev, app_nil_r.
 change (fold_left (fun ys x => dedup_nat_step ys x) (rev xs) [] = fold_right (fun x ys => dedup_nat_step ys x) [] xs).
 rewrite <- (fold_left_rev_right (fun x ys => dedup_nat_step ys x) (rev xs) []).
 rewrite rev_involutive. reflexivity.
Qed.
Lemma dedup_nat_fast_eq : forall xs, dedup_nat_fast xs=nodup Nat.eq_dec xs.
Proof.
 intro xs. rewrite dedup_nat_fast_fold.
 induction xs as[|x xs IH];cbn [fold_right];[reflexivity|].
 rewrite IH. unfold dedup_nat_step. cbn [nodup].
 destruct(in_dec Nat.eq_dec x(nodup Nat.eq_dec xs))as[A|A];
 destruct(in_dec Nat.eq_dec x xs)as[B|B];try reflexivity.
 - exfalso;apply B. apply nodup_In in A;exact A.
 - exfalso;apply A. apply nodup_In;exact B.
Qed.

Definition reify_generation_ids(e:env):list nat:= dedup_nat_fast (map fst (en_tn e) +++ map fst (en_uf e) ++
    flat_map ety_gens (reify_scope_types e)).
Definition reify_scope_of (e : env) : reify_scope ety key :=
  let generations := map (fun t => (t,canon e (YAbs t))) (reify_generation_ids e) in
  let values := elems (en_vals e) in
  let functions := map (fun p => (fst p,reify_sig_raw (snd p))) (elems (en_fns e)) in
  let methods := flat_map (fun p => map (fun q =>
      ((fst q,fst p),reify_sig_raw (snd q))) (snd p)) (elems (en_mths e)) in
  ReifyScope ety key (tymap e) generations (all_keys e)
    values functions methods (en_apps e).


(* Family patterns use fresh rigid markers.  Avoid every generation the
   caller carries, even for independently supplied well formed contexts. *)
Definition marker_generation_ids(e:env):list nat:=
 reify_generation_ids e +++ flat_map(fun p=>
   (match fst p with KyMth r _=>ety_gens r|_=>[]end) +++
   map fst(fst(snd p)))(en_apps e).
Definition marker_floor(e:env):nat:=
 fold_right(fun g top=>Nat.max(S g)top)0(marker_generation_ids e).
Definition fresh_marker(e:env):M nat:=
 let floor:=marker_floor e in
 fun s=>let id:=Nat.max(st_next s)floor in Good(id,
  St(st_w s)(st_syms s)(S id)(st_reqs s)(st_sigsyms s)(st_info s)
    (st_fns s)(st_funs s)(st_fwd s)(st_links s)(st_flds s)(st_consts s)
    (st_wrappers s)(st_vars s)(st_nvar s)(st_memory_bound s)(st_memory_used s)
    (st_checked_total s)(st_lang s)(st_unsup s)(st_reify s)).


(* Also avoid generations supplied by the receiver itself. *)
Definition fresh_marker_for(e:env)(receiver:ety):M nat:=
 let floor:=Nat.max(marker_floor e)
   (fold_right(fun g top=>Nat.max(S g)top)0(ety_gens receiver))in
 fun s=>let id:=Nat.max(st_next s)floor in Good(id,
  St(st_w s)(st_syms s)(S id)(st_reqs s)(st_sigsyms s)(st_info s)
    (st_fns s)(st_funs s)(st_fwd s)(st_links s)(st_flds s)(st_consts s)
    (st_wrappers s)(st_vars s)(st_nvar s)(st_memory_bound s)(st_memory_used s)
    (st_checked_total s)(st_lang s)(st_unsup s)(st_reify s)).


Definition register_reify_code (owner : reify_owner) (e : env) (inputs : list key)
    (signature : option fsig) : M unit :=
  let code := ReifyCode ety key owner (reify_scope_of e) inputs
                (option_map (reify_sig e) signature) None [] in
  upd (s_reify (fun r => ReifyMetadata (rm_owner r)
    ((owner, code) :: rm_codes r) (rm_declarations r) (rm_constants r))).

Definition record_reify_event (event : reify_event ety key) : M unit :=
  upd (s_reify (fun r =>
    let codes := match rm_owner r with
    | None => rm_codes r
    | Some owner => map (fun p =>
        if reify_owner_eqb (fst p) owner then
          let code := snd p in
          (fst p, ReifyCode ety key (rc_owner ety key code)
            (rc_scope ety key code) (rc_inputs ety key code)
            (rc_signature ety key code) (rc_exports ety key code)
            (event :: rc_events ety key code))
        else p) (rm_codes r)
    end in ReifyMetadata (rm_owner r) codes (rm_declarations r) (rm_constants r))).


Definition record_reify_exports_at (owner : reify_owner) (types : list nat) (items : list key)
    (inputs promised actual : env) (roles : list (nat * nat)) : M unit :=
  upd (s_reify (fun r =>
    ReifyMetadata (rm_owner r)
      (map (fun p => if reify_owner_eqb (fst p) owner then
        let code := snd p in
        (fst p, ReifyCode ety key (rc_owner ety key code)
          (rc_scope ety key code) (rc_inputs ety key code)
          (rc_signature ety key code)
          (Some (ReifyExports ety key types items (reify_scope_of promised)
            (reify_scope_of actual) (reify_scope_of inputs) roles))
          (rc_events ety key code))
        else p) (rm_codes r)) (rm_declarations r) (rm_constants r))).

Definition record_reify_exports (owner : reify_owner) (types : list nat) (items : list key)
    (inputs promised actual : env) : M unit :=
  record_reify_exports_at owner types items inputs promised actual (map (fun t => (t,t)) types).

(* Cached constant IR is inlined at every read. Its annotation trace must
   likewise be copied into each reader, even in a different code owner. *)
Definition current_reify_events (r : reify_metadata ety key) : list (reify_event ety key) :=
  match rm_owner r with
  | None => []
  | Some owner => match assoc reify_owner_eqb owner (rm_codes r) with
      | Some code => rc_events ety key code | None => [] end
  end.
Definition remember_reify_constant (id start : nat) : M unit :=
  upd (s_reify (fun r =>
    let events := current_reify_events r in
    let fragment := firstn (List.length events - start) events in
    let constants := if is_some (assoc Nat.eqb id (rm_constants r)) then rm_constants r
                     else (id,fragment) :: rm_constants r in
    ReifyMetadata (rm_owner r) (rm_codes r) (rm_declarations r) constants)).
Definition replay_reify_constant (id : nat) : M unit :=
  let! s := get in
  let events := opt_default [] (assoc Nat.eqb id (rm_constants (st_reify s))) in
  miter record_reify_event (rev events).

(* lower.instantiate: a declaration's type read at a use — each parameter
   becomes the generation of its symbol in force here *)
Fixpoint inst_go (fuel : nat) (e : env) (t : ety) : ety :=
  match fuel with
  | 0 => t
  | S f =>
      match t with
      | YAbs g => match tn_get e g with Some h => canon e h | None => canon e t end
      | YNom n xs => YNom n (map (fun p => (fst p, inst_go f e (snd p))) xs)
      | YUnion ms => YUnion (map (inst_go f e) ms)
      | YRec fs => YRec (map (fun p => (fst p, inst_go f e (snd p))) fs)
      | _ => t
      end
  end.
(* Re-read declaration generations completely; a structural walk never
   leaves a deeply nested suffix at its original generations. *)
Fixpoint instantiate (e : env) (t : ety) : ety :=
  match t with
  | YAbs g => match tn_get e g with Some y => canon e y | None => canon e t end
  | YNom n xs => YNom n (map (fun p => (fst p, instantiate e (snd p))) xs)
  | YUnion ms => YUnion (map (instantiate e) ms)
  | YRec fs => YRec (map (fun p => (fst p, instantiate e (snd p))) fs)
  | _ => t
  end.

Definition fits (e : env) (t expected : ety) : bool := fits_raw (canon e t) (canon e expected).

Fixpoint show_go (fuel : nat) (t : ety) : M string :=
  match fuel with
  | 0 => ret "…"
  | S f =>
      match t with
      | YUnit => ret "()"
      | YNever => ret "|"
      | YRec fs =>
          let! ss := mmap (fun p => let! s := show_go f (snd p) in ret (fst p ++ ": " ++ s)%string) fs in
          ret ("{" ++ String.concat ", " ss ++ "}")%string
      | YUnion ms => let! ss := mmap (show_go f) ms in ret (String.concat " | " ss)
      | YAbs g => let! x := sym g in ret (gs_name x)
      | YNom n xs =>
          let! x := sym n in
          match xs with
          | [] => ret (gs_name x)
          | _ => let! ss := mmap (fun p => let! a := sym (fst p) in let! s := show_go f (snd p) in
                                           ret (gs_name a ++ "=" ++ s)%string) xs in
                 ret (gs_name x ++ "[" ++ String.concat ", " ss ++ "]")%string
          end
      end
  end.
Definition show (t : ety) : M string := show_go 50 t.

Fixpoint unify (fuel : nat) (e : env) (mi : nat) (a b : ety) (why : string) : M env :=
  match fuel with
  | 0 => fail "unification nests too deeply"
  | S f =>
      let a := canon e a in
      let b := canon e b in
      if ety_eqb a b then ret e else
      let mismatch := let! sa := show a in let! sb := show b in
                      err mi ("cannot merge `" ++ sa ++ "` with `" ++ sb ++ "`" ++ why ++ " (D43)") in
      (* occurs check: a generation is never merged with a type that
         contains it, which no finite type unifies (ExecutionModel note 53) *)
      let cyclic := let! sa := show a in let! sb := show b in
                    err mi ("cannot merge `" ++ sa ++ "` with `" ++ sb ++ "`" ++ why ++ ", which contains it (D43)") in
      match a, b with
      | YAbs g, _ => if nmem g (ety_gens b) then cyclic else ret (set_uf e g b)
      | _, YAbs g => if nmem g (ety_gens a) then cyclic else ret (set_uf e g a)
      | YNom n xs, YNom m ys =>
          (* one head has one argument list (its requirements, in their
             order); compared for the proof (ExecutionModel note 55) *)
          if Nat.eqb n m then
            if list_eq_dec Nat.eq_dec (map fst xs) (map fst ys)
            then mfold (fun e p => unify f e mi (fst p) (snd p) why) e
                       (combine (map snd xs) (map snd ys))
            else mismatch
          else mismatch
      | YUnion xs, YUnion ys =>
          if Nat.eqb (List.length xs) (List.length ys)
          then mfold (fun e p => unify f e mi (fst p) (snd p) why) e (combine xs ys)
          else mismatch
      | YRec xs, YRec ys =>
          if list_eq_dec string_dec (map fst xs) (map fst ys)
          then mfold (fun e p => unify f e mi (fst p) (snd p) why) e (combine (map snd xs) (map snd ys))
          else mismatch
      | _, _ => mismatch
      end
  end.

Definition unify_sig (e : env) (mi : nat) (a b : fsig) (nm : string) : M env :=
  let why := (" for `" ++ nm ++ "`")%string in
  if negb (Nat.eqb (List.length (sg_params a)) (List.length (sg_params b)))
  then err mi ("conflicting provisions" ++ why ++ " (arity)")
  else
    let! e := mfold (fun e p => unify 200 e mi (fst p) (snd p) why) e
                    (combine (map snd (sg_params a)) (map snd (sg_params b))) in
    let! e := unify 200 e mi (sg_ret a) (sg_ret b) why in
    match sg_this a, sg_this b with
    | Some x, Some y => unify 200 e mi x y why
    | _, _ => ret e
    end.

Definition resolve_type_symbol (mi : nat) (p : path) : M gsym :=
  let! r := rpath mi p in
  match r with
  | None => err mi ("`" ++ show_path p ++ "` is not in scope")
  | Some (EMod _) => err mi ("`" ++ show_path p ++ "` is not a type")
  | Some (ESym s) =>
      let! g := sym s in
      match gs_kind g with
      | KType | KUnit | KTag | KAlias => ret g
      | _ => err mi ("`" ++ show_path p ++ "` is not a type")
      end
  end.

Definition check_relevant (mi : nat) (g : gsym) (a : list (path * spec)) : M unit :=
  let! rs := reqs (gs_id g) in
  miter (fun b =>
           let! l := resolve_sym mi (fst b) in
           match l with
           | Some lg =>
               if kind_eqb (gs_kind lg) KType && negb (nmem (gs_id lg) rs) then
                 let! names := mmap (fun r => let! x := sym r in ret ("`" ++ gs_name x ++ "`")%string) rs in
                 err mi ("application of `" ++ gs_name g ++ "` binds `" ++ gs_name lg ++
                         "`, which is not one of its parameters (the type symbols its payload mentions: " ++
                         (match names with [] => "none" | _ => String.concat ", " names end) ++ "; D71)")
               else ret tt
           | None => ret tt
           end) a.

(* D89: a bracket application on a function call, `f[A = T](e1, …, en)`,
   is `{ let a1 = e1; …; let an = en; bind A = T; f(a1, …, an) }` with
   names `ai` no program can write (`Syntax.call_block`): the arguments are
   evaluated, in the caller's terms, before the binds.  Each binding
   names a type `f`'s assume nesting requires and binds it to a path,
   as a bracket on any reference does (D22). *)
Definition check_call_brackets (mi : nat) (g : gsym) (a : list (path * spec)) : M unit :=
  let! rs := reqs (gs_id g) in
  miter (fun b =>
           let! l := resolve_sym mi (fst b) in
           match l with
           | None => err mi ("`" ++ show_path (fst b) ++ "` is not in scope")
           | Some lg =>
               if kind_eqb (gs_kind lg) KType && nmem (gs_id lg) rs then
                 match snd b with
                 | Spec (Some _) None _ => ret tt
                 | _ => err mi ("a bracket application on a call of `" ++ gs_name g ++ "` binds `" ++
                                gs_name lg ++ "` to a method; it binds a type to a type (D89)")
                 end
               else
                 err mi ("a bracket application on a call of `" ++ gs_name g ++ "` binds `" ++ gs_name lg ++
                         "`, which is not a type `" ++ gs_name g ++ "` requires (D22, D89)")
           end) a.

Definition check_totality (mi : nat) (g : gsym) (subst : list (nat * ety)) (e : env) : M unit :=
  match subst with
  | [] => ret tt
  | _ =>
      let! rs := reqs (gs_id g) in
      let missing := filter (fun r => negb (is_some (nassoc r subst)) && negb (is_some (tn_get e r))) rs in
      match missing with
      | [] => ret tt
      | _ => let! names := mmap (fun r => let! x := sym r in ret (gs_name x)) missing in
             err mi ("application of `" ++ gs_name g ++ "` must bind all requirements (D22); missing " ++
                     String.concat ", " names)
      end
  end.

Definition nominal_args (mi : nat) (g : gsym) (tm : list (nat * ety)) : M (list (nat * ety)) :=
  let! rs := reqs (gs_id g) in
  mmap (fun r => match nassoc r tm with
                 | Some y => ret (r, y)
                 | None => let! x := sym r in
                           err mi ("type `" ++ gs_name x ++ "` (required by `" ++ gs_name g ++
                                   "`) is not available in the context here")
                 end) rs.

Fixpoint elab_type (fuel : nat) (t : aty) (mi : nat) (tm : list (nat * ety)) (e : env)
    (this : option ety) {struct fuel} : M ety :=
  match fuel with
  | 0 => fail "type nests too deeply"
  | S f =>
      match t with
      | ATNever => ret YNever
      | ATThis => match this with
                  | Some y => ret y
                  | None => err mi "`This` is only legal inside method declarations"
                  end
      | ATUnit => ret YUnit
      | ATRec fs =>
          let! fs' := mmap (fun p => let! y := elab_type f (snd p) mi tm e this in ret (fst p, y)) fs in
          ret (YRec fs')
      | ATUnion ms =>
          let! out := mfold (fun out m =>
              let! y := elab_type f m mi tm e this in
              mfold (fun out x =>
                       match x with
                       | YAbs _ => let! s := show x in
                                   err mi ("union member `" ++ s ++ "` is abstract and not constrained to a nominal type (D15)")
                       | YNom h _ =>
                           (* 0 is no symbol (records' reserved head, ExecutionModel
                              note 34): never so; checked for the proof *)
                           if Nat.eqb h 0 then err mi "union member is not nominal (D15)" else
                           if ety_mem x out then let! s := show x in err mi ("duplicate union member `" ++ s ++ "`")
                           else
                             (* D15: no two members share a head, so a
                                match arm's head names one member (ExecutionModel
                                note 33) *)
                             match find (fun y => match y with YNom h' _ => Nat.eqb h h' | _ => false end) out with
                             | Some y => let! s := show x in let! s' := show y in
                                         err mi ("union members `" ++ s' ++ "` and `" ++ s ++
                                                 "` share a head (D15)")
                             | None => ret (out +++ [x])
                             end
                       | _ => let! s := show x in err mi ("union member `" ++ s ++ "` is not nominal (D15)")
                       end) out (members y)) [] ms in
          (* a union of no members is `|` (ExecutionModel note 42) *)
          ret (match out with [] => YNever | _ => YUnion out end)
      | ATRef p a =>
          let! g := resolve_type_symbol mi p in
          match a with
          | None => symbol_type f mi g tm e
          | Some bs =>
              check_relevant mi g bs ;;
              let! s := app_subst f e mi bs tm in
              let tm' := s +++ tm in
              check_totality mi g tm' e ;;
              (* a type symbol takes no brackets, not even `T[]` (ExecutionModel
                 note 50) *)
              if kind_eqb (gs_kind g) KType then err mi ("type `" ++ gs_name g ++ "` takes no brackets") else
              symbol_type f mi g tm' e
          end
      end
  end
with app_subst (fuel : nat) (e : env) (mi : nat) (bs : list (path * spec)) (outer : list (nat * ety))
    {struct fuel} : M (list (nat * ety)) :=
  match fuel with
  | 0 => fail "type nests too deeply"
  | S f =>
      let! l := mmap (fun b =>
          let! tg := rpath mi (fst b) in
          let! g := match tg with
                    | Some (ESym s) => let! g := sym s in
                                       if kind_eqb (gs_kind g) KType then ret g
                                       else err mi ("`" ++ show_path (fst b) ++ "` in a bracket binding must be an abstract type symbol")
                    | _ => err mi ("`" ++ show_path (fst b) ++ "` in a bracket binding must be an abstract type symbol")
                    end in
          match snd b with
          | Spec (Some sp) None sa =>
              let! y := elab_type f (ATRef sp sa) mi (outer +++ tymap e) e None in ret (gs_id g, y)
          | _ => err mi "bracket bindings bind types in v0"
          end) bs in
      ret (rev l)   (* later bindings override earlier ones, as a dict's *)
  end
with symbol_type (fuel : nat) (mi : nat) (g : gsym) (tm : list (nat * ety)) (e : env) {struct fuel} : M ety :=
  match fuel with
  | 0 => fail "type nests too deeply"
  | S f =>
      match gs_kind g with
      | KType => match nassoc (gs_id g) tm with
                 | Some y => ret y
                 | None => err mi ("type `" ++ gs_name g ++ "` is not available in the context here")
                 end
      | KUnit | KTag => let! xs := nominal_args mi g tm in ret (YNom (gs_id g) xs)
      | KAlias => match gs_decl g with
                  | ADAlias _ t => elab_type f t (gs_mod g) tm e None
                  | _ => fail "internal: alias"
                  end
      | _ => err mi ("`" ++ gs_name g ++ "` is not a type")
      end
  end.

Definition ety_of (t : aty) (mi : nat) (tm : list (nat * ety)) (e : env) (this : option ety) : M ety :=
  elab_type 200 t mi tm e this.

Definition payload_type (t : ety) (e : env) : M ety :=
  match t with
  | YNom n xs =>
      let! g := sym n in
      match gs_decl g with
      | ADTag _ _ p => ety_of p (gs_mod g) xs e None
      | _ => ret YUnit
      end
  | _ => ret YUnit
  end.

Definition fn_params (g : gsym) : list (name * aty) :=
  match gs_decl g with ADFn _ _ ps _ _ => ps | _ => [] end.
Definition fn_ret (g : gsym) : option aty :=
  match gs_decl g with ADFn _ _ _ r _ => r | _ => None end.
Definition fn_body (g : gsym) : option ablock :=
  match gs_decl g with ADFn _ _ _ _ b => b | _ => None end.
Definition is_defined_fn (g : gsym) : bool :=
  kind_eqb (gs_kind g) KFn && is_some (fn_body g).

Definition fn_sig (g : gsym) (tm : list (nat * ety)) (e : env) (this : option ety) : M fsig :=
  let! ps := mmap (fun p => let! y := ety_of (snd p) (gs_mod g) tm e this in ret (fst p, y)) (fn_params g) in
  let! r := match fn_ret g with Some t => ety_of t (gs_mod g) tm e this | None => ret YUnit end in
  ret (FSig ps r this).

(* ===================== 8. Contexts in force (lower: add_*_item) ========== *)

Definition method_key (e : env) (mi : nat) (rty : ety) (m : gsym) : M key :=
  let r := canon e rty in
  match head r, gs_recv m with
  | None, Some _ => let! s := show rty in err mi ("`" ++ s ++ "` cannot receive methods (no nominal head)")
  | _, _ => ret (KyMth r (gs_id m))
  end.

Definition find_method_decl (mi : nat) (recv : gsym) (n : name) : M (option gsym) :=
  let! hm := the_mod (gs_mod recv) in
  let! here := the_mod mi in
  match assoc attached_eqb (gs_id recv, n) (gm_attached hm) with
  | Some s => let! g := sym s in ret (Some g)
  | None =>
      match sassoc n (gm_detached here) with
      | Some s => let! g := sym s in ret (Some g)
      | None => ret None
      end
  end.

Definition provision_app (g : gsym) (tm : list (nat * ety)) (meth : bool) : M inst :=
  let! rs := reqs (gs_id g) in
  let! rs := if meth then signature_type_syms g rs else ret rs in
  ret (map (fun t => (t, nassoc t tm)) rs).

Definition freeze_app (e : env) (a : inst) : inst :=
  map (fun p => (fst p, option_map (canon e) (snd p))) a.

Definition setdefault_app (e : env) (k : key) (a : inst) : M env :=
  match get_app e k with
  | Some _ => ret e
  | None => let! s := fresh in ret (set_app e k a s)
  end.

Definition functor_parts (g : gsym) : list spec * list spec * option (list (list (spec * option aexpr))) :=
  match gs_decl g with ADFunctor _ _ a r b => (a, r, b) | _ => ([], [], None) end.

(* D84 rule 1: P(F), the brackets; None for an unbracketed functor, which
   means P(F) = M(F) and X(F) = ∅ — the only case this milestone covers. *)
Definition functor_brackets (g : gsym) : option (list name) :=
  match gs_decl g with ADFunctor _ ps _ _ _ => ps | _ => None end.

(* the types a functor's result holds, in order (V.5 FunOutputs: the
   same walk as an assume nesting's) *)
Definition result_types (g : gsym) : M (list nat) :=
  let '(_, r, _) := functor_parts g in nesting_types (gs_mod g) r.

(* m4, D84 rule 1: P(F), the bracketed type symbols; None unbracketed *)
Definition functor_params (g : gsym) : M (option (list nat)) :=
  match functor_brackets g with
  | None => ret None
  | Some names =>
      let! ps := mmap (fun n =>
          let! t := resolve_sym (gs_mod g) [n] in
          match t with
          | Some tg => if kind_eqb (gs_kind tg) KType then ret (gs_id tg)
                       else err (gs_mod g) ("`" ++ n ++ "`, listed as a polymorphic input of `" ++ gs_name g ++ "`, is not a type in scope (D84)")
          | None => err (gs_mod g) ("`" ++ n ++ "`, listed as a polymorphic input of `" ++ gs_name g ++ "`, is not a type in scope (D84)")
          end) names in
      ret (Some ps)
  end.

(* D84 rule 1: the type symbols F's result mentions (M(F), before taking
   out the outputs): a val's or fn's dependencies (its whole telescope), a
   method's receiver and the symbols its signature mentions less those its
   brackets bind, and the symbols the brackets' right-hand sides mention. *)
Fixpoint result_mentions_go (fuel : nat) (self : nat) (mi : nat) (specs : list spec) (out : list nat) {struct fuel} : M (list nat) :=
  match fuel with
  | 0 => err mi "context nests too deeply"
  | S f =>
      let addl (l out : list nat) := fold_left (fun o x => if nmem x o then o else o +++ [x]) l out in
      mfold (fun out sp =>
        match sp with
        | Spec (Some p) d a =>
            let bs := opt_default [] a in
            let! bound := mmap (fun b => let! l := resolve_sym mi (fst b) in
                                         ret (match l with Some x => [gs_id x] | None => [] end)) bs in
            let bound := List.concat bound in
            let! rhs := mfold (fun o b => match snd b with
                                         | Spec (Some sp') _ sa => free_type_syms 200 (ATRef sp' sa) mi o
                                         | _ => ret o
                                         end) [] bs in
            let! tg := resolve_sym mi p in
            match tg with
            | None => ret out
            | Some g =>
                let! here :=
                  match d with
                  | Some dn =>
                      let! mo := find_method_decl mi g dn in
                      let! rr := if kind_eqb (gs_kind g) KType then ret [gs_id g] else reqs (gs_id g) in
                      let! ms := match mo with
                                 | Some m => let! rs := reqs (gs_id m) in signature_type_syms m rs
                                 | None => ret []
                                 end in
                      ret (rr +++ ms)
                  | None =>
                      match gs_kind g, gs_decl g with
                      | KCtx, ADCtx _ its => result_mentions_go f self (gs_mod g) its []
                      | KVal, _ => reqs (gs_id g)
                      (* #12b: a fn's requirements and every type symbol
                         its signature mentions, as for a method *)
                      | KFn, _ => let! rs := reqs (gs_id g) in
                                  let! ms := signature_type_syms g rs in ret (rs +++ ms)
                      (* m4, D84 rule 1: an abstract functor item G mentions X(G) *)
                      | KFunctor, ADFunctor _ _ _ _ None =>
                          if Nat.eqb (gs_id g) self then ret [] else functor_fixed_go f g
                      | _, _ => ret []
                      end
                  end in
                ret (addl (filter (fun x => negb (nmem x bound)) here +++ rhs) out)
            end
        | _ => ret out
        end) out specs
  end
(* m4, D84 rule 1: X(F) = M(F) ∖ P(F), M(F) less F's type outputs; empty
   for an unbracketed F (P(F) = M(F)) *)
with functor_fixed_go (fuel : nat) (g : gsym) {struct fuel} : M (list nat) :=
  match fuel with
  | 0 => err (gs_mod g) "functors nest too deeply"
  | S f =>
      let! ps := functor_params g in
      match ps with
      | None => ret []
      | Some ps =>
          let '(_, r, _) := functor_parts g in
          let! ms := result_mentions_go f (gs_id g) (gs_mod g) r [] in
          let! rts := result_types g in
          ret (filter (fun t => negb (nmem t ps) && negb (nmem t rts)) ms)
      end
  end.

Definition result_mentions (g : gsym) : M (list nat) :=
  let! n := walk_fuel in
  let '(_, r, _) := functor_parts g in result_mentions_go (2 * n) (gs_id g) (gs_mod g) r [].
Definition functor_fixed (g : gsym) : M (list nat) :=
  let! n := walk_fuel in functor_fixed_go (2 * n) g.

Definition add_need (needs : list key) (k : key) : list key :=
  if key_mem k needs then needs else needs +++ [k].

(* m4: since D79's amendment the memory instructions are `Addr`'s methods,
   members of the family `Memories` (D84 rule 7), not fns under `assume
   Memory`; using one is noted where `Memories` is applied (apply_family) *)
Definition is_memory_instruction (g : gsym) : M bool := ret false.
Definition is_memory_instruction_61b7b76 (g : gsym) : M bool :=
  let! w := lang "wasm" in
  match w with
  | None => ret false
  | Some wm =>
      if negb (Nat.eqb (gs_mod g) wm) || negb (kind_eqb (gs_kind g) KFn) then ret false else
      let! m := the_mod wm in
      match sassoc "Memory" (gm_names m) with
      | Some ms => let! rs := reqs (gs_id g) in ret (nmem ms rs)
      | None => ret false
      end
  end.

Definition memory_sym : M (option nat) :=
  let! w := lang "wasm" in
  match w with
  | None => ret None
  | Some wm => let! m := the_mod wm in ret (sassoc "Memory" (gm_names m))
  end.

Definition add_method_item (e : env) (mi : nat) (recv : gsym) (n : name) (subst : list (nat * ety))
    (needs : list key) : M (env * list key) :=
  let! mo := find_method_decl mi recv n in
  match mo with
  | None => err mi ("no method `." ++ n ++ "` for `" ++ gs_name recv ++ "` is in scope")
  | Some m =>
      let tm := subst +++ tymap e in
      let! this := match nassoc (gs_id recv) tm with
                   | Some y => ret y
                   | None => symbol_type 200 mi recv tm e
                   end in
      let! sg := fn_sig m tm e (Some this) in
      let! k := method_key e mi this m in
      let! e := match k with
                | KyMth r mid =>
                    match get_mth e r mid with
                    | Some old => unify_sig e mi old sg (gs_name m)
                    | None => ret (set_mth e r mid sg)
                    end
                | _ => ret e
                end in
      let! a := provision_app m tm true in
      let! e := setdefault_app e k a in
      ret (e, add_need needs k)
  end.


(* Assuming an item walks the contexts it names, and theirs (V.3's
   ItemEntries).  A context met again inside itself would be walked
   forever: the relation has no reading of it, and the elaborator reports
   it.  Each level of nesting takes two of the fuel, and an acyclic
   nesting enters each context at most once on the way down, so twice
   `walk_fuel` runs out only on a cycle (ExecutionModel note 30). *)
Definition ctx_fuel : M nat := let! n := walk_fuel in ret (2 * n).

Fixpoint add_symbol_item (fuel : nat) (e : env) (mi : nat) (g : gsym) (subst : list (nat * ety))
    (needs : list key) {struct fuel} : M (env * list key) :=
  match fuel with
  | 0 => err mi "contexts nest in a cycle"
  | S f =>
      check_totality mi g subst e ;;
      let tm := subst +++ tymap e in
      match gs_kind g with
      | KType =>
          match tn_get e (gs_id g) with
          | Some _ => ret (e, needs)
          | None => let! s := fresh in ret (set_tn e (gs_id g) (YAbs (gs_id g)) s, needs)
          end
      | KVal =>
          match gs_decl g with
          | ADVal _ t (Some _) => ret (e, needs)
          | ADVal _ t None =>
              let! y := ety_of t (gs_mod g) tm e None in
              let! e := match nget (en_vals e) (gs_id g) with
                        | Some old => unify 200 e mi old y (" for `" ++ gs_name g ++ "`")
                        | None => ret (set_val e (gs_id g) y)
                        end in
              let! a := provision_app g tm false in
              let! e := setdefault_app e (KyVal (gs_id g)) a in
              ret (e, add_need needs (KyVal (gs_id g)))
          | _ => fail "internal: val"
          end
      | KFn =>
          let! sg := fn_sig g tm e None in
          let! e := match nget (en_fns e) (gs_id g) with
                    | Some old => unify_sig e mi old sg (gs_name g)
                    | None => ret (set_fn e (gs_id g) sg)
                    end in
          let! a := provision_app g tm false in
          let! mem := is_memory_instruction g in
          let! ms := memory_sym in
          let a := if mem then match ms with
                               | Some x => filter (fun p => negb (Nat.eqb (fst p) x)) a
                               | None => a end
                   else a in
          let! e := setdefault_app e (KyFn (gs_id g)) a in
          ret (e, add_need needs (KyFn (gs_id g)))
      | KCtx =>
          match gs_decl g with
          | ADCtx _ its =>
              mfold (fun acc sp => add_spec f (fst acc) (gs_mod g) sp subst (snd acc)) (e, needs) its
          | _ => fail "internal: context"
          end
      | KUnit | KTag | KAlias => ret (e, needs)
      | KFunctor =>
          match gs_decl g with
          | ADFunctor _ _ _ _ None =>
              let! s := fresh in
              (* m4, D84 rule 2: F is at an instantiation of X(F), which
                 assuming or listing it needs in force *)
              let! xs := functor_fixed g in
              let absent := filter (fun x => negb (is_some (tn_get e x))) xs in
              (match absent with
               | [] => ret tt
               | _ => let! ns := mmap (fun x => let! xg := sym x in ret (gs_name xg)) absent in
                      err mi ("assuming `" ++ gs_name g ++ "` needs `" ++ String.concat "`, `" ns ++
                              "` in force: `" ++ gs_name g ++ "` is fixed in them (D84, D20)")
               end) ;;
              let e := set_fun e (gs_id g) s in
              let! e := setdefault_app e (KyFun (gs_id g)) (map (fun x => (x, tn_get e x)) xs) in
              ret (e, add_need needs (KyFun (gs_id g)))
          | _ => err mi ("cannot assume `" ++ gs_name g ++ "` (a " ++ kind_name (gs_kind g) ++ ")")
          end
      | _ => err mi ("cannot assume `" ++ gs_name g ++ "` (a " ++ kind_name (gs_kind g) ++ ")")
      end
  end
with add_spec (fuel : nat) (e : env) (mi : nat) (sp : spec) (outer : list (nat * ety))
    (needs : list key) {struct fuel} : M (env * list key) :=
  match fuel with
  | 0 => err mi "contexts nest in a cycle"
  | S f =>
      match sp with
      | Spec p d a =>
          let! subst := match a with
                        | Some bs => let! s := app_subst 200 e mi bs outer in ret (s +++ outer)
                        | None => ret outer
                        end in
          (* D71: a bracket on a context, val or fn item binds a type
             symbol among the item's requirements, as on a type or a bind
             (a method item's may bind any type symbol: ExecutionModel note 56) *)
          let relevant (g : gsym) : M unit :=
            match a with Some bs => check_relevant mi g bs | None => ret tt end in
          match d, p with
          | Some dn, None => err mi "a context item needs a receiver for a detached method"
          | Some dn, Some pp =>
              let! r := resolve_type_symbol mi pp in
              add_method_item e mi r dn subst needs
          | None, Some pp =>
              let! tg := rpath mi pp in
              match tg with
              | None => err mi ("`" ++ show_path pp ++ "` is not in scope")
              | Some (EMod _) => err mi "cannot put a module in a context"
              | Some (ESym s) =>
                  let! g := sym s in relevant g ;;
                  (* only a context, an assumed val and a fn take brackets
                     here: the others introduce no reading of them (ExecutionModel
                     note 57) *)
                  let bracketless :=
                    match gs_kind g, gs_decl g with
                    | KCtx, _ | KFn, _ | KVal, ADVal _ _ None => false
                    | _, _ => true
                    end in
                  if is_some a && bracketless then err mi ("`" ++ gs_name g ++ "` takes no brackets in a context (D71)")
                  else add_symbol_item f e mi g subst needs
              end
          | None, None => err mi "an empty context item"
          end
      end
  end.

Definition add_assume_item (e : env) (mi : nat) (it : path * option name) (needs : list key)
    : M (env * list key) :=
  match it with
  | (p, Some d) =>
      let! r := resolve_type_symbol mi p in add_method_item e mi r d [] needs
  | (p, None) =>
      let! tg := rpath mi p in
      match tg with
      | None => err mi ("`" ++ show_path p ++ "` is not in scope")
      | Some (EMod _) => err mi ("cannot assume a module (`" ++ show_path p ++ "`)")
      | Some (ESym s) => let! g := sym s in let! cf := ctx_fuel in add_symbol_item cf e mi g [] needs
      end
  end.


(* D43 edges: on generations (uf), and a declared symbol read as other
   than its own parameter *)
Definition merge_edges (e : env) : list (nat * ety) :=
  filter (fun p => negb (ety_eqb (snd p) (YAbs (fst p)))) (tymap e) +++ en_uf e.

Definition edges_eqb (a b : list (nat * ety)) : bool :=
  Nat.eqb (List.length a) (List.length b) &&
  forallb (fun p => match nassoc (fst p) b with Some y => ety_eqb y (snd p) | None => false end) a.

Definition env_with_edges (tn_edges uf_edges : list (nat * ety)) : env :=
  Env (map (fun p => (fst p, (snd p, 0))) tn_edges) uf_edges NE NE NE NE [] [].

(* A result name is a new output role even when its old input generation
   belonged to a merge class. Replace output roles first; canonicalize only
   the remaining old generations. Structural substitution has no depth tail. *)
Definition promised_result_type (body : env) (made : list (nat * ety)) (written : ety) : ety :=
  canon body (instantiate (env_with_edges made []) written).

(* Result functors retain the fixed interpretations captured at their
   binding. A final result type rebind cannot relabel that capture. *)
Definition result_functor_fixed_ok (body promised : env) (made : list (nat * ety))
    (actual_key promised_key : key) : bool :=
  let wanted := match get_app promised promised_key with Some (a,_) => a | None => [] end in
  let actual := match get_app body actual_key with Some (a,_) => a | None => [] end in
  forallb (fun p =>
    let have := opt_default None (assoc Nat.eqb (fst p) actual) in
    match have,snd p with
    | Some got,Some want => ety_eqb (canon body got) (promised_result_type body made want)
    | None,None => true
    | _,_ => false end) wanted.

(* lower.merged_walk: rebuild until the walk adds no edge, at most 8
   times.  A walk that still adds one in its 9th round, the check of the
   8th, is rejected: what it made is no merge of its telescope (ExecutionModel
   note 52). *)
Definition merged_walk (mi : nat) (walk : env -> M (env * list key)) : M (env * list key) :=
  let fix go (n : nat) (tne ufe : list (nat * ety)) : M (env * list key) :=
    let! (e, needs) := walk (env_with_edges tne ufe) in
    let tne' := filter (fun p => negb (ety_eqb (snd p) (YAbs (fst p)))) (tymap e) in
    let ufe' := en_uf e in
    if edges_eqb (tne' +++ ufe') (tne +++ ufe) then ret (e, needs) else
    match n with
    | 0 => err mi "merging the context does not settle in 8 rounds (D43)"
    | S n' => go n' tne' ufe'
    end in
  go 8 [] [].

(* Declaration-context snapshots contain provisions, never function locals.
   Both callers construct these contexts from a fresh environment. *)
Definition freeze_all_apps (e : env) : env :=
  Env (en_tn e) (en_uf e) (en_vals e) (en_fns e) (en_mths e) (en_funs e) []
      (map (fun p => let '(k, (a, s)) := p in
                     (k, (freeze_app e (map (fun q => (fst q, match snd q with
                                                               | Some y => Some y
                                                               | None => tn_get e (fst q)
                                                               end)) a), s))) (en_apps e)).

(* D84 (8), D37: a function named `main` in any loaded module's scope,
   including an imported function renamed to main, gets the root context. *)
Definition is_main (g : gsym) : M bool :=
  if negb (kind_eqb (gs_kind g) KFn) || is_some (gs_recv g) then ret false else
  let! s := get in
  ret (existsb (fun mi => match sassoc "main" (gm_names (get_mod (st_w s) mi)) with
                         | Some x => Nat.eqb x (gs_id g)
                         | None => false end) (w_order (st_w s))).

(* m4, D84 (8): the platform runs `main` in its root context: beside what
   `main` assumes, lib/wasm.moss's `Gc` (the platform's functors), with the
   types they are fixed in (lower.add_root_context) *)
Definition add_root_context (e : env) (mi : nat) (needs : list key) : M (env * list key) :=
  let! w := lang "wasm" in
  match w with
  | None => ret (e, needs)
  | Some wm =>
      let! m := the_mod wm in
      match sassoc "Gc" (gm_names m) with
      | None => ret (e, needs)
      | Some gcs =>
          let! gc := sym gcs in
          match gs_kind gc, gs_decl gc with
          | KCtx, ADCtx _ its =>
              mfold (fun acc sp =>
                  let '(e, needs) := acc in
                  match sp with
                  | Spec (Some p) None _ =>
                      let! fo := resolve_sym (gs_mod gc) p in
                      match fo with
                      | Some fg =>
                          if negb (kind_eqb (gs_kind fg) KFunctor) || is_some (nget (en_funs e) (gs_id fg)) then ret acc else
                          let! xs := functor_fixed fg in
                          let! cf := ctx_fuel in
                          let! (e, needs) := mfold (fun acc t => let! tg := sym t in
                                                     add_symbol_item cf (fst acc) mi tg [] (snd acc)) (e, needs) xs in
                          add_symbol_item cf e mi fg [] needs
                      | None => ret acc
                      end
                  | _ => ret acc
                  end) (e, needs) its
          | _, _ => ret (e, needs)
          end
      end
  end.

(* lower.base_env + the bookkeeping of lower_fn: a function's needs, the
   instantiation it needs each at, and the merges its context made.  The
   cache is keyed by symbol, so the info is computed from the symbol table's
   entry for it (ExecutionModel note 58). *)
Definition fn_info (g0 : gsym) : M fninfo :=
  let! x := get in
  match nget (st_info x) (gs_id g0) with
  | Some i => ret i
  | None =>
      let! g := sym (gs_id g0) in
      let mi := gs_mod g in
      let walk (e : env) : M (env * list key) :=
        match gs_origin g with
        | OForwarder t =>
            let! tg := sym t in
            let! rs := reqs t in
            let! cf := ctx_fuel in
            let! (e, needs) := mfold (fun acc r => let! rg := sym r in
                                                   add_symbol_item cf (fst acc) mi rg [] (snd acc)) (e, []) rs in
            add_symbol_item cf e mi tg [] needs
        | _ =>
            let! acc := mfold (fun acc it => add_assume_item (fst acc) mi it (snd acc)) (e, []) (gs_assumes g) in
            let! main := is_main g in
            if main then add_root_context (fst acc) mi (snd acc) else ret acc
        end in
      let! (e, needs) := merged_walk mi walk in
      let merges := merge_edges e in
      let e := freeze_all_apps e in
      let apps := map (fun k => (k, match get_app e k with Some (a, _) => a | None => [] end)) needs in
      let i := FInfo e needs apps merges in
      upd (s_info (fun m => nset m (gs_id g0) i)) ;; ret i
  end.

Definition unsupported (why : string) : M unit :=
  fun s => Good (tt, St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) (st_info s)
     (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
     (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
     (st_checked_total s) (st_lang s) (st_unsup s +++ [why]) (st_reify s)).

(* ===================== 9. Into MossX: keys, items, locals ================ *)

(* Records have no declaration: their key is a reserved head (0, never a
   symbol) applied to their fields, interned; `|` is the empty union. *)
Definition rec_head : nat := 0.

Fixpoint ety_ty_go (fuel : nat) (t : ety) : M ty :=
  match fuel with
  | 0 => fail "type nests too deeply"
  | S f =>
      match t with
      | YUnit => ret TUnit
      | YNever => ret (TUnion [])
      | YAbs g => ret (TAbs g)
      | YNom n xs => let! xs' := mmap (fun p => let! y := ety_ty_go f (snd p) in ret (fst p, y)) xs in
                     ret (TNom n xs')
      | YUnion ms => let! ms' := mmap (ety_ty_go f) ms in ret (TUnion ms')
      | YRec fs => let! fs' := mmap (fun p => let! i := fld_id (fst p) in
                                              let! y := ety_ty_go f (snd p) in ret (i, y)) fs in
                   ret (TNom rec_head fs')
      end
  end.
Definition ety_ty (t : ety) : M ty := ety_ty_go 200 t.

Definition key_item (k : key) : M item :=
  match k with
  | KyVal v => ret (IVal v)
  | KyFn f => ret (IFn f)
  | KyFun u => ret (IFun u)
  | KyMth t m => let! t' := ety_ty t in ret (IMth t' m [])
  end.

(* θ of a direct call of g (D20/D21, Track A): each of g's requirements,
   in reqs order, read at the site (its meaning here, or itself where it is
   not in force), canonical. *)
Definition call_theta (e : env) (g : gsym) : M subst :=
  let! rs := reqs (gs_id g) in
  mmap (fun r => let! y := ety_ty (canon e (instantiate e (YAbs r))) in ret (r, y)) rs.

(* θ of a tag: its type's arguments. *)
Definition tag_theta (e : env) (xs : list (nat * ety)) : M subst :=
  mmap (fun p => let! y := ety_ty (canon e (snd p)) in ret (fst p, y)) xs.

Definition smap_of (ps : list (key * key)) : M smap :=
  mmap (fun p => let! a := key_item (fst p) in let! b := key_item (snd p) in ret (a, b)) ps.

(* θ of a method provision's key in env e (Track A, note 28): the
   instantiation the provision was made at, as e records it, its known
   entries only, canonical. *)
Definition key_theta (e : env) (k : key) : M subst :=
  match get_app e k with
  | Some (a, _) =>
      let! l := mmap (fun q => match snd q with
                               | Some y => let! t := ety_ty (canon e y) in ret [(fst q, t)]
                               | None => ret []
                               end) a in
      ret (List.concat l)
  | None => ret []
  end.

Definition key_item_at (e : env) (k : key) : M item :=
  match k with
  | KyMth t m => let! t' := ety_ty t in let! th := key_theta e k in ret (IMth t' m th)
  | _ => key_item k
  end.

(* a satisfaction map: each pair's left key read in the callee's env ei,
   its right key in the caller's eo *)
Definition smap_at (ei eo : env) (ps : list (key * key)) : M smap :=
  mmap (fun p => let! a := key_item_at ei (fst p) in let! b := key_item_at eo (snd p) in ret (a, b)) ps.

Definition local_var (x : string) : M nat :=
  let! s := get in
  match sassoc x (st_vars s) with
  | Some n => ret n
  | None => upd (s_vars (st_vars s +++ [(x, st_nvar s)]) (S (st_nvar s))) ;; ret (st_nvar s)
  end.

Definition show_key (k : key) : M string :=
  match k with
  | KyVal v | KyFn v | KyFun v => let! g := sym v in ret ("`" ++ gs_name g ++ "`")
  | KyMth t m => let! s := show t in let! g := sym m in ret ("`" ++ s ++ gs_name g ++ "`")
  end.

(* ===================== 10. Layout (lower.slots_of, D59) =================== *)

Fixpoint slots_go (fuel : nat) (e : env) (t : ety) (visiting : list ety) : M nat :=
  match fuel with
  | 0 => fail "layout nests too deeply"
  | S f =>
      let t := canon e t in
      match t with
      | YRec fs => mfold (fun n p => let! k := slots_go f e (snd p) visiting in ret (n + k)) 0 fs
      | YUnion ms =>
          let! units := mfold (fun b m => match m with
                                          | YNom n _ => let! g := sym n in ret (b && kind_eqb (gs_kind g) KUnit)
                                          | _ => ret false
                                          end) true ms in
          if units then ret 1 else
          let! w := mfold (fun w m => match m with
                                      | YNom n _ => let! g := sym n in
                                                    if kind_eqb (gs_kind g) KTag then
                                                      let! k := slots_go f e m visiting in ret (Nat.max w k)
                                                    else ret w
                                      | _ => ret w
                                      end) 0 ms in
          ret (S w)
      | YNom n _ =>
          let! g := sym n in
          if kind_eqb (gs_kind g) KTag then
            if ety_mem t visiting || Nat.ltb 64 (List.length visiting) then
              err (gs_mod g) ("`" ++ gs_name g ++ "` contains itself, so it has no finite layout (D58)")
            else let! p := payload_type t e in slots_go f e p (t :: visiting)
          else ret 1
      | _ => ret 1
      end
  end.
Definition layout_ok (e : env) (t : ety) : M unit := mbind (slots_go 300 e t []) (fun _ => ret tt).

(* D39's native entry ABI has no argument or result lanes. Unit and Never
   erase, records concatenate lanes, and ordinary tags use payload lanes.
   A nominal unit and every union carry a discriminator and do not erase. *)
Fixpoint entry_zero_width (fuel : nat) (e : env) (t : ety) : M bool :=
  match fuel with
  | 0 => fail "main return type nests too deeply (D39)"
  | S f => match canon e t with
      | YUnit | YNever => ret true
      | YRec fs => mfold (fun zero p =>
          let! field_zero := entry_zero_width f e (snd p) in ret (zero && field_zero)) true fs
      | YNom n _ =>
          let! g := sym n in
          if kind_eqb (gs_kind g) KTag then
            let! payload := payload_type (canon e t) e in entry_zero_width f e payload
          else ret false
      | _ => ret false
      end
  end.

Definition check_main_arity (g : gsym) : M unit :=
  match fn_params g with
  | [] => ret tt
  | _ => err (gs_mod g) "main must take no parameters (D39)"
  end.

Definition check_main_signature (g : gsym) (info : fninfo) : M unit :=
  check_main_arity g ;;
  let e := fi_env info in
  let! signature := fn_sig g (tymap e) e None in
  let! zero := entry_zero_width 300 e (sg_ret signature) in
  if zero then ret tt
  else err (gs_mod g) "main must return a type with no runtime lanes (D39)".

(* ===================== 11. Needs and coherence (FnChecker: require_needs, merges) = *)

Definition oety_eqb (a b : option ety) : bool :=
  match a, b with
  | None, None => true
  | Some x, Some y => ety_eqb x y
  | _, _ => false
  end.

Definition require_merges (mi : nat) (e : env) (callee : gsym) (info : fninfo) : M unit :=
  miter (fun p =>
           let k := fst p in
           let v := canon (env_with_edges [] (fi_merges info)) (YAbs k) in
           let mentioned := k :: ety_gens v in
           if forallb (fun t => is_some (tn_get e t)) mentioned then
             let lhs := canon e (instantiate e (YAbs k)) in
             let rhs := canon e (instantiate e v) in
             if ety_eqb lhs rhs then ret tt
             else let! a := show (YAbs k) in let! b := show v in
                  let! a' := show lhs in let! b' := show rhs in
                  err mi ("calling `" ++ gs_name callee ++ "` needs `" ++ a ++ "` and `" ++ b ++
                          "` to be one type, which its context merged (D43), but here they are `" ++
                          a' ++ "` and `" ++ b' ++ "`")
           else ret tt) (fi_merges info).

(* Rule B (criteria: principality and coherence; Audit's
   `RequirementsInForce`): a use of an item whose assumptions include a
   type requirement r (a call of a defined fn or attached method, a fn or
   method provider bind) needs r in force here: bound or assumed here, or
   in force through r's class in the item's merges (fi_merges): a member
   in force here, or a structured type all of whose generations are in
   force here.  A requirement a caller could leave unbound would otherwise
   be read as itself, a guess (a declared symbol outside the caller's
   table, e.g. a family demand keyed at it). *)
Definition class_members (info : fninfo) : list nat := map fst (en_tn (fi_env info)).
Definition in_force_through (e : env) (info : fninfo) (r : nat) : bool :=
  let m := env_with_edges [] (fi_merges info) in
  is_some (tn_get e r) ||
  existsb (fun y => is_some (tn_get e y) && ety_eqb (canon m (YAbs y)) (canon m (YAbs r))) (class_members info) ||
  match canon m (YAbs r) with
  | YAbs _ => false
  | z => forallb (fun t => is_some (tn_get e t)) (ety_gens z)
  end.
(* the requirements are the types in force in the item's own context *)
Definition requirements_in_force (mi : nat) (e : env) (g : gsym) (info : fninfo) (what : string) : M unit :=
  miter (fun r => if in_force_through e info r then ret tt else
                  let! tg := sym r in
                  err mi (what ++ " `" ++ gs_name g ++ "` needs `" ++ gs_name tg ++
                          "` in force here (assume or bind it, or a type its context merges it with)"))
        (class_members info).

(* Rule B for a family applied on demand (D84 rule 7): every generation
   its θ mentions is in force here, or mentioned by a meaning in force
   here; a key read at a requirement the caller leaves unbound is not
   demanded. *)
Definition family_inputs_in_force (mi : nat) (e : env) (fg : gsym) (th : list (nat * ety)) : M unit :=
  let range := flat_map (fun p => ety_gens (canon e (fst (snd p)))) (en_tn e) in
  miter (fun q => miter (fun t => if is_some (tn_get e t) || nmem t range then ret tt else
                                   let! tg := sym t in let! ag := sym (fst q) in
                                   err mi ("applying the family `" ++ gs_name fg ++ "` at `" ++ gs_name ag ++
                                           "` mentions `" ++ gs_name tg ++ "`, which is not in force here (assume or bind it)"))
                        (ety_gens (canon e (snd q)))) th.

(* The principal reading of a callee's requirements (criteria, principality;
   #50, E24): a requirement not in force here whose class in the callee's
   merges (D43) has a member in force here reads as that member does here,
   for the whole call (its needs, family demands included, its signature
   and θ); any other is left as it is. *)
Definition principal_args (e : env) (info : fninfo) : list (nat * (ety * nat)) :=
  let m := env_with_edges [] (fi_merges info) in
  flat_map (fun r =>
    if is_some (tn_get e r) then [] else
    match find (fun y => is_some (tn_get e y) && ety_eqb (canon m (YAbs y)) (canon m (YAbs r))) (class_members info) with
    | Some y => match nassoc y (en_tn e) with
                | Some (_, st) => [(r, (canon e (instantiate e (YAbs y)), st))]
                | None => []
                end
    | None => []
    end) (class_members info).
Definition principal_env (e : env) (info : fninfo) : env :=
  fold_left (fun en q => set_tn en (fst q) (fst (snd q)) (snd (snd q))) (principal_args e info) e.

Definition key_present (e : env) (k : key) : bool :=
  match k with
  | KyVal v => is_some (nget (en_vals e) v)
  | KyFn f => is_some (nget (en_fns e) f)
  | KyFun u => is_some (nget (en_funs e) u)
  | KyMth t m => is_some (get_mth e t m)
  end.

(* the callee's instantiation of a need and the provision's, where they differ *)
Definition incoherent (e : env) (info : fninfo) (need k : key) : bool :=
  let wanted := opt_default [] (assoc key_eqb need (fi_apps info)) in
  let have := match get_app e k with Some (a, _) => a | None => [] end in
  existsb (fun p =>
             let '(t, ty) := p in
             let ty := match ty with Some y => Some y | None => tn_get e t end in
             let req := option_map (instantiate e) ty in
             let h := match assoc Nat.eqb t have with Some o => o | None => None end in
             negb (oety_eqb req h)) wanted.

Definition require_needs (mi : nat) (e : env) (callee : gsym) (info : fninfo) : M (list (key * key)) :=
  require_merges mi e callee info ;;
  let! pairs := mmap (fun need =>
      let missing := let! s := show_key need in
                     err mi ("calling `" ++ gs_name callee ++ "` needs " ++ s ++
                             ", which is not available in the context here") in
      match need with
      | KyMth r m =>
          let! mg := sym m in
          let! k := method_key e mi (instantiate e r) mg in
          if key_present e k then ret (need, k, true) else
          let! s := show_key k in
          err mi ("calling `" ++ gs_name callee ++ "` needs " ++ s ++
                  ", which is not available in the context here")
      | KyFun _ => if key_present e need then ret (need, need, true) else missing
      | _ => if key_present e need then ret (need, need, true) else missing
      end) (fi_needs info) in
  let bad := filter (fun p => let '(need, k, chk) := p in chk && incoherent e info need k) pairs in
  match bad with
  | [] => ret (map (fun p => let '(a, b, _) := p in (a, b)) pairs)
  | (need, k, _) :: _ =>
      let! s := show_key k in
      err mi ("calling `" ++ gs_name callee ++ "` needs " ++ s ++
              " at another instantiation than the one in the context here was provided at " ++
              "(a provision serves only the instantiation it was made at; D62)")
  end.

(* D20: an assumed provision brings requirements, not just a signature.
   Check them against the completed assume list, so list order is immaterial.
   An item's explicit brackets are available while its requirements are read. *)
Definition check_assumed_requirements (mi : nat) (e : env) (k : key) : M unit :=
  let id := match k with KyVal x | KyFn x | KyFun x | KyMth _ x => x end in
  let! g := sym id in
  match k with
  | KyFun _ => ret tt (* its fixed inputs are checked by add_symbol_item *)
  | _ =>
  let a := match get_app e k with Some (a, _) => a | None => [] end in
  let! view := mfold (fun view q => match snd q with
                                   | Some t => let! stamp := fresh in ret (set_tn view (fst q) t stamp)
                                   | None => ret view
                                   end) e a in
  let! info := fn_info g in
  let! rs := if kind_eqb (gs_kind g) KMethod then
               let! rs := reqs id in signature_type_syms g rs
             else ret (map fst (en_tn (fi_env info))) in
  miter (fun t => if is_some (tn_get view t) then ret tt else
                   let! tg := sym t in
                   err mi ("assuming `" ++ gs_name g ++ "` needs `" ++ gs_name tg ++ "` in force (D20)")) rs ;;
  let! _ := require_needs mi view g info in
  ret tt
  end.

Fixpoint list_same {A} (eqb : A -> A -> bool) (xs ys : list A) : bool :=
  match xs, ys with
  | [], [] => true
  | x :: xs, y :: ys => eqb x y && list_same eqb xs ys
  | _, _ => false
  end.

Definition same_assume (a b : path * option name) : bool :=
  list_same String.eqb (fst a) (fst b) &&
  match snd a, snd b with
  | None, None => true
  | Some x, Some y => String.eqb x y
  | _, _ => false
  end.

Definition check_assumes : M unit :=
  let! s := get in
  let w := st_w s in
  (* Declarations sharing a module, nesting and root context have exactly
     the same ambient requirements. Validate that context once. *)
  let! _ := mfold (fun seen id => match nget (w_syms w) id with
                  | Some g =>
                      match gs_kind g with
                      | KVal | KFn | KMethod | KCtx | KFunctor =>
                          let! main := is_main g in
                          if existsb (fun q => Nat.eqb (gs_mod (fst q)) (gs_mod g) &&
                               Bool.eqb (snd q) main &&
                               list_same same_assume (gs_assumes (fst q)) (gs_assumes g)) seen
                          then ret seen else
                          let! info := fn_info g in
                          miter (check_assumed_requirements (gs_mod g) (fi_env info)) (fi_needs info) ;;
                          ret ((g, main) :: seen)
                      | _ => ret seen
                      end
                  | None => ret seen
                  end) [] (seq 0 (w_nsyms w)) in
  ret tt.

(* ===================== 12. Syntax helpers ================================= *)

(* whether a `break` in x (or b) leaves the innermost enclosing loop: not
   one inside a nested `while` or `loop` (ExecutionModel note 42: structural, so at
   any depth) *)
Fixpoint has_break_e (x : aexpr) : bool :=
  match x with
  | AEBreak => true
  | AECall _ _ args => (fix go (l : list aexpr) := match l with [] => false | y :: r => has_break_e y || go r end) args
  | AEField o _ => has_break_e o
  | AEMeth o _ args =>
      has_break_e o || (fix go (l : list aexpr) := match l with [] => false | y :: r => has_break_e y || go r end) args
  | AERec _ _ fs =>
      (fix go (l : list (name * option aexpr)) :=
         match l with [] => false | (_, Some v) :: r => has_break_e v || go r | (_, None) :: r => go r end) fs
  | AEIf c t el => has_break_e c || has_break_b t ||
                   match el with Some y => has_break_e y | None => false end
  | AEMatch s arms =>
      has_break_e s ||
      (fix go (l : list (apat * aexpr)) := match l with [] => false | (_, y) :: r => has_break_e y || go r end) arms
  | AEReturn (Some y) => has_break_e y
  | AEBlock b => has_break_b b
  | AEUnary _ y => has_break_e y
  | AEBinary _ l r => has_break_e l || has_break_e r
  | AELam _ _ b => has_break_e b
  | _ => false
  end
with has_break_b (b : ablock) : bool :=
  match b with
  | ABlock ss t =>
      (fix go (l : list astmt) := match l with [] => false | s :: r => has_break_s s || go r end) ss ||
      match t with Some y => has_break_e y | None => false end
  end
with has_break_s (s : astmt) : bool :=
  match s with
  | ASLet _ _ y | ASVar _ _ y | ASAssign _ y | ASExpr y => has_break_e y
  | ASBind its =>
      (fix go (l : list (spec * option aexpr)) :=
         match l with [] => false | (_, Some y) :: r => has_break_e y || go r | (_, None) :: r => go r end) its
  | ASWhile _ _ | ASLoop _ => false
  end.

(* every single-segment name an expression might read as a local *)
(* Lexical capture reads the complete finite syntax tree.  The old fuel
   parameter remains in the public wrappers, but never truncates names. *)
Fixpoint free_names_complete_e (x : aexpr) : list string :=
  match x with
  | AEPath [n] None => [n]
  | AECall _ _ args => flat_map free_names_complete_e args
  | AEField o _ => free_names_complete_e o
  | AEMeth o _ args => free_names_complete_e o +++ flat_map free_names_complete_e args
  | AERec _ _ fs => flat_map (fun p => match snd p with
                    | Some v => free_names_complete_e v | None => [fst p] end) fs
  | AEIf c t el => free_names_complete_e c +++ free_names_complete_b t +++
                  match el with Some y => free_names_complete_e y | None => [] end
  | AEMatch value arms => free_names_complete_e value +++
                        flat_map (fun a => free_names_complete_e (snd a)) arms
  | AEReturn (Some y) => free_names_complete_e y
  | AEBlock b => free_names_complete_b b
  | AEUnary _ y => free_names_complete_e y
  | AEBinary _ l r => free_names_complete_e l +++ free_names_complete_e r
  | AELam _ _ body => free_names_complete_e body
  | _ => []
  end
with free_names_complete_b (b : ablock) : list string :=
  match b with
  | ABlock statements tail => flat_map free_names_complete_s statements +++
      match tail with Some value => free_names_complete_e value | None => [] end
  end
with free_names_complete_s (statement : astmt) : list string :=
  match statement with
  | ASLet _ _ value | ASVar _ _ value | ASExpr value => free_names_complete_e value
  | ASAssign n value => n :: free_names_complete_e value
  | ASBind items => flat_map (fun p => match snd p with
                    | Some value => free_names_complete_e value | None => [] end) items
  | ASWhile condition body => free_names_complete_e condition +++ free_names_complete_b body
  | ASLoop body => free_names_complete_b body
  end.
Definition free_names_e (_fuel : nat) (x : aexpr) : list string := free_names_complete_e x.
Definition free_names_b (_fuel : nat) (b : ablock) : list string := free_names_complete_b b.

(* A receiver, explicit parameters and captured locals occupy distinct
   names in a lambda frame, just as in an ordinary function frame. *)
Fixpoint duplicate_local_name (names : list string) : option string :=
  match names with
  | [] => None
  | name :: rest => if smem name rest then Some name else duplicate_local_name rest
  end.
Definition check_lambda_local_names (mi : nat) (names : list string) : M unit :=
  match duplicate_local_name names with
  | Some name => err mi ("anonymous function local `" ++ name ++
      "` occurs twice among its receiver, parameters or captures (D70)")
  | None => ret tt
  end.

Definition lang_symbol (short n : string) (detached : bool) (why : string) (mi : nat) : M gsym :=
  let! lm := lang short in
  let fail' := err mi (why ++ " means `" ++ n ++ "` of lib/" ++ short ++ ".moss, which is not loaded here") in
  match lm with
  | None => fail'
  | Some li =>
      let! m := the_mod li in
      match sassoc n (if detached then gm_detached m else gm_names m) with
      | Some s => sym s
      | None => fail'
      end
  end.

(* m4: the library module and detached method an operator means: num.moss's
   for D68's operators, access.moss's iteration methods for the `for.`
   operators both parsers desugar `for x in xs` into (D85) *)
Definition operator_method (op : string) (unary : bool) : option (string * string) :=
  let num n := Some ("num", n) in
  if unary then
    match op with "-" => num "neg" | "!" => num "not" | "for.begin" => Some ("access", "begin") | _ => None end
  else
    match op with
    | "*" => num "mul" | "/" => num "div" | "%" => num "rem" | "+" => num "add"
    | "-" => num "sub" | "<<" => num "shl" | ">>" => num "shr" | "==" => num "eq"
    | "!=" => num "ne" | "<" => num "lt" | ">" => num "gt" | "<=" => num "le"
    | ">=" => num "ge" | "&" => num "and" | "^" => num "xor" | "|" => num "or"
    | "for.more" => Some ("access", "more") | "for.get" => Some ("access", "get")
    | "for.after" => Some ("access", "after")
    | _ => None
    end.

(* ---- functors: leaves of a signature (lower.expand_spec) ---- *)

Definition already_provided (g : gsym) : bool :=
  match gs_kind g with
  | KUnit | KTag | KAlias => true
  | KFn => is_some (fn_body g)
  | KVal => match gs_decl g with ADVal _ _ (Some _) => true | _ => false end
  | _ => false
  end.

(* leaf: (target symbol, method name) *)
Definition leaf := (nat * option string)%type.
Definition leaf_eqb (a b : leaf) : bool :=
  Nat.eqb (fst a) (fst b) &&
  match snd a, snd b with
  | None, None => true | Some x, Some y => String.eqb x y | _, _ => false
  end.

Fixpoint expand_spec (fuel : nat) (mi : nat) (sp : spec) (out : list (leaf * string)) : M (list (leaf * string)) :=
  match fuel with
  | 0 => err mi "context nests too deeply"
  | S f =>
      match sp with
      | Spec (Some p) d _ =>
          let! tg := rpath mi p in
          match tg with
          | Some (ESym s) =>
              let! g := sym s in
              match d, gs_decl g with
              | None, ADCtx _ its => mfold (fun o it => expand_spec f (gs_mod g) it o) out its
              | None, _ =>
                  if already_provided g then ret out
                  else ret (aset leaf_eqb (s, None) ("`" ++ gs_name g ++ "`") out)
              | Some dn, _ => ret (aset leaf_eqb (s, Some dn) ("`" ++ gs_name g ++ "." ++ dn ++ "`") out)
              end
          | _ => ret out
          end
      | _ => ret out
      end
  end.

Definition signature_items (mi : nat) (specs : list spec) : M (list (leaf * string)) :=
  let! n := walk_fuel in
  mfold (fun o sp => expand_spec n mi sp o) [] specs.

(* lower.check_functor_total: a functor with a body binds its whole result *)
Fixpoint functor_total (fuel : nat) (g : gsym) : M unit :=
  match fuel with
  | 0 => fail "functors nest too deeply"
  | S f =>
      let! s := get in
      if nmem (gs_id g) (st_checked_total s) then ret tt else
      upd (s_checked (fun l => gs_id g :: l)) ;;
      match functor_parts g with
      | (_, _, None) => ret tt
      | (_, res, Some binds) =>
          let mi := gs_mod g in
          let! wanted := signature_items mi res in
          let! remaining := mfold (fun w it =>
              match it with
              | (Spec (Some p) _ _, None) =>
                  let! tg := resolve_sym mi p in
                  match tg with
                  | Some inner =>
                      if kind_eqb (gs_kind inner) KFunctor then
                        functor_total f inner ;;
                        let '(_, ires, _) := functor_parts inner in
                        let! got := signature_items (gs_mod inner) ires in
                        ret (filter (fun q => negb (existsb (fun r => leaf_eqb (fst q) (fst r)) got)) w)
                      else ret w
                  | None => ret w
                  end
              | (Spec (Some p) d _, Some _) =>
                  let! tg := rpath mi p in
                  match tg with
                  | Some (ESym s) => ret (filter (fun q => negb (leaf_eqb (fst q) (s, d))) w)
                  | _ => ret w
                  end
              | _ => ret w
              end) wanted (List.concat binds) in
          match remaining with
          | [] => ret tt
          | _ => err mi ("functor `" ++ gs_name g ++ "` does not bind " ++ String.concat ", " (map snd remaining))
          end
      end
  end.

(* A functor's own terms: the environment its argument signature makes
   (lower.check_functor_body's walk), with D84's implicit inputs — the
   types its result mentions that it neither lists nor makes. *)
Definition fun_env (g : gsym) : M (env * list key) :=
  (* m4: a bracketed functor's own terms are an unbracketed one's: its fixed
     inputs X(F) are among the types it reads (and θ maps each to what it
     reads at the application, which `fixed_ok` checks is what it read
     where F was provided) *)
  let '(args, _, _) := functor_parts g in
  let! rts := result_types g in
  let! ms := result_mentions g in
  let '(_, _, body) := functor_parts g in
  (* implicit inputs are a declaration's (rule 1): a functor with a body
     relies on its own argument signature alone *)
  let ms := if is_some body then [] else ms in
  let! cf := ctx_fuel in
  let! (e, needs) := merged_walk (gs_mod g) (fun e =>
      let! (e, needs) := mfold (fun acc sp => add_spec cf (fst acc) (gs_mod g) sp [] (snd acc)) (e, []) args in
      mfold (fun acc t =>
               if nmem t rts || is_some (tn_get (fst acc) t) then ret acc
               else let! tg := sym t in
                    if kind_eqb (gs_kind tg) KType then add_symbol_item cf (fst acc) (gs_mod g) tg [] (snd acc)
                    else ret acc) (e, needs) ms) in
  ret (freeze_all_apps e, needs).

(* provide_result: each item of a functor's result, provided here as the
   bind that makes it would; its types first, each read as `fresh` says. *)
Definition provided_at (e : env) (k : key) (g : gsym) (tm : list (nat * ety)) (meth : bool) : M env :=
  let! a := provision_app g tm meth in
  let! s := fresh in
  ret (set_app e k (freeze_app e a) s).

Fixpoint provide_spec (fuel : nat) (e : env) (fg : gsym) (mi : nat) (sp : spec)
    (outer : list (nat * ety)) (out : list key) {struct fuel} : M (env * list key) :=
  match fuel with
  | 0 => err mi "context nests too deeply"
  | S f =>
      match sp with
      | Spec p d a =>
          let! subst := match a with
                        | Some bs => let! s := app_subst 200 e mi bs outer in ret (s +++ outer)
                        | None => ret outer
                        end in
          let tm := subst +++ tymap e in
          match d, p with
          | Some dn, Some pp =>
              let! r := resolve_type_symbol mi pp in
              let! mo := find_method_decl mi r dn in
              match mo with
              | None => err mi ("no method `." ++ dn ++ "` for `" ++ gs_name r ++ "` is in scope")
              | Some m =>
                  let! this := match nassoc (gs_id r) tm with
                               | Some y => ret y
                               | None => symbol_type 200 mi r tm e
                               end in
                  let! sg := fn_sig m tm e (Some this) in
                  let! k := method_key e mi this m in
                  let e := match k with KyMth t mid => set_mth e t mid sg | _ => e end in
                  let! e := provided_at e k m tm true in
                  ret (e, out +++ [k])
              end
          | None, Some pp =>
              let! tg := resolve_sym mi pp in
              match tg with
              | None => err mi ("`" ++ show_path pp ++ "` is not in scope")
              | Some g =>
                  match gs_kind g, gs_decl g with
                  | KCtx, ADCtx _ its =>
                      mfold (fun acc it => provide_spec f (fst acc) fg (gs_mod g) it subst (snd acc)) (e, out) its
                  | KType, _ => ret (e, out)
                  | _, _ =>
                      if already_provided g then ret (e, out) else
                      match gs_kind g, gs_decl g with
                      | KVal, ADVal _ t _ =>
                          let! y := ety_of t (gs_mod g) tm e None in
                          let e := set_val e (gs_id g) y in
                          let! e := provided_at e (KyVal (gs_id g)) g tm false in
                          ret (e, out +++ [KyVal (gs_id g)])
                      | KFn, _ =>
                          let! sg := fn_sig g tm e None in
                          let e := set_fn e (gs_id g) sg in
                          let! e := provided_at e (KyFn (gs_id g)) g tm false in
                          ret (e, out +++ [KyFn (gs_id g)])
                      | KFunctor, ADFunctor _ _ _ _ None =>
                          let! s := fresh in
                          (* m4, D84 rule 5: at its fixed inputs as they are here *)
                          let! xs := functor_fixed g in
                          let e := set_fun e (gs_id g) s in
                          let! s2 := fresh in
                          let e := set_app e (KyFun (gs_id g)) (map (fun x => (x, option_map (canon e) (nassoc x tm))) xs) s2 in
                          ret (e, out +++ [KyFun (gs_id g)])
                      | _, _ => err mi ("`" ++ gs_name fg ++ "` cannot provide `" ++ gs_name g ++
                                        "` (a " ++ kind_name (gs_kind g) ++ "; D78)")
                      end
                  end
              end
          | _, _ => err mi "an empty context item"
          end
      end
  end.

(* returns the env with the result in force, and its non-type keys *)
Definition provide_result (e : env) (fg : gsym) (fresh_of : list (nat * nat)) : M (env * list key) :=
  let! ts := result_types fg in
  let! e := mfold (fun e t => match nassoc t fresh_of with
                              | Some g => let! s := fresh in ret (set_tn e t (YAbs g) s)
                              | None => ret e
                              end) e ts in
  let '(_, res, _) := functor_parts fg in
  let! n := walk_fuel in
  mfold (fun acc sp => provide_spec n (fst acc) fg (gs_mod fg) sp [] (snd acc)) (e, []) res.

(* Freeze the old input generations before installing fresh output roles.
   A role placeholder is check-only: raw IR translates it back explicitly. *)
Definition target_input_types (e : env) : env :=
  Env (map (fun p => (fst p,(canon e (fst (snd p)),snd (snd p)))) (en_tn e))
      (en_uf e) (en_vals e) (en_fns e) (en_mths e) (en_funs e) (en_locals e) (en_apps e).
Definition fresh_target_output_view (e : env) (fg : gsym) : M (env * list key * list (nat * nat)) :=
  let! ts := result_types fg in
  let! roles := mmap (fun t => let! tg := sym t in
      let! g := new_sym (gs_mod tg) (gs_name tg ++ "@result") KType
                       (ADType (gs_name tg)) (OFresh t (gs_id fg)) in ret (t,g)) ts in
  let! (view, keys) := provide_result (target_input_types e) fg roles in
  ret (view, keys, roles).
Definition output_role_types (roles : list (nat * nat)) : list (nat * ety) :=
  map (fun p => (snd p,YAbs (fst p))) roles.
Definition output_role_key (roles : list (nat * nat)) (k : key) : key :=
  match k with KyMth r m => KyMth (instantiate (env_with_edges (output_role_types roles) []) r) m | _ => k end.
Definition output_role_item (roles : list (nat * nat)) (i : item) : item :=
  app_subst_item (map (fun p => (snd p,TAbs (fst p))) roles) i.

(* The fixed inputs of a newly bound abstract functor must already be in
   force, just as in the native binding check. *)
Definition bind_fixed_present (mi : nat) (e : env) (fg : gsym) (xs : list nat) : M unit :=
  miter (fun x => if is_some (tn_get e x) then ret tt else
    let! xg := sym x in err mi ("binding `" ++ gs_name fg ++ "` needs `" ++ gs_name xg ++
      "` in force: it is a fixed input (D84)")) xs.
Definition copy_fixed_current (mi : nat) (e : env) (fg : gsym) (xs : list nat) : M unit :=
  let recorded := match get_app e (KyFun (gs_id fg)) with Some (a,_) => a | None => [] end in
  miter (fun x => match assoc Nat.eqb x recorded,tn_get e x with
    | Some (Some was),Some now =>
      if ety_eqb (canon e was) (canon e now) then ret tt else
      let! xg := sym x in err mi ("copying `" ++ gs_name fg ++ "` after rebinding its fixed input `" ++
        gs_name xg ++ "` changes its interpretation (D84)")
    | _,_ => ret tt end) xs.

Definition sig_eqb_in (e1 : env) (a : fsig) (e2 : env) (b : fsig) : bool :=
  Nat.eqb (List.length (sg_params a)) (List.length (sg_params b)) &&
  forallb (fun p => ety_eqb (canon e1 (fst p)) (canon e2 (snd p)))
          (combine (map snd (sg_params a)) (map snd (sg_params b))) &&
  ety_eqb (canon e1 (sg_ret a)) (canon e2 (sg_ret b)).

Definition key_sig (e : env) (k : key) : option (list ety * ety) :=
  match k with
  | KyVal v => option_map (fun y => ([], y)) (nget (en_vals e) v)
  | KyFn f => option_map (fun s => (map snd (sg_params s), sg_ret s)) (nget (en_fns e) f)
  | KyMth t m => option_map (fun s => (map snd (sg_params s), sg_ret s)) (get_mth e t m)
  | KyFun _ => Some ([], YUnit)
  end.

(* Match only formal generations. Caller generations are rigid values;
   replacements are single-pass, so their names are never solved in turn. *)
Fixpoint copy_match_type (vars : list nat) (pat actual : ety)
    (sol : list (nat * ety)) {struct pat} : option (list (nat * ety)) :=
  match pat,actual with
  | YAbs x,_ => if nmem x vars then
      match nassoc x sol with Some t => if ety_eqb t actual then Some sol else None
      | None => Some ((x,actual)::sol) end
      else if ety_eqb pat actual then Some sol else None
  | YUnit,YUnit | YNever,YNever => Some sol
  | YNom n xs,YNom m ys => if Nat.eqb n m then
      (fix go xs ys sol := match xs,ys with
       | [],[] => Some sol
       | (k,t)::xs,(l,u)::ys => if Nat.eqb k l then
           match copy_match_type vars t u sol with Some next => go xs ys next | None => None end
           else None
       | _,_ => None end) xs ys sol else None
  | YUnion xs,YUnion ys =>
      (fix go xs ys sol := match xs,ys with
       | [],[] => Some sol
       | t::xs,u::ys => match copy_match_type vars t u sol with
          Some next => go xs ys next | None => None end
       | _,_ => None end) xs ys sol
  | YRec xs,YRec ys =>
      (fix go xs ys sol := match xs,ys with
       | [],[] => Some sol
       | (k,t)::xs,(l,u)::ys => if String.eqb k l then
           match copy_match_type vars t u sol with Some next => go xs ys next | None => None end
           else None
       | _,_ => None end) xs ys sol
  | _,_ => None
  end.
Definition copy_interpret (generic : env) (view : env) (t : ety) : ety :=
  instantiate generic (canon view t).
Definition copy_key (generic view : env) (k : key) : key :=
  match k with KyMth r m => KyMth (copy_interpret generic view r) m | _ => k end.
Definition copy_app_eq (generic a b : env) (ka kb : key) : bool :=
  match get_app a ka,get_app b kb with
  | Some (xs,_),Some (ys,_) =>
      Nat.eqb (List.length xs) (List.length ys) &&
      forallb (fun p => match assoc Nat.eqb (fst p) ys,snd p with
        | Some (Some u),Some t => ety_eqb (copy_interpret generic a t) (copy_interpret generic b u)
        | Some None,None => true | _,_ => false end) xs
  | None,None => true | _,_ => false
  end.
Definition copy_slot_eq (generic a b : env) (ka kb : key) : bool :=
  copy_app_eq generic a b ka kb &&
  match key_sig a ka,key_sig b kb with
  | Some (ps,t),Some (qs,u) =>
      Nat.eqb (List.length ps) (List.length qs) &&
      forallb (fun p => ety_eqb (copy_interpret generic a (fst p))
                               (copy_interpret generic b (snd p))) (combine (t::ps) (u::qs))
  | None,None => true | _,_ => false end.
(* A captured provision keeps its old actual reading. Only G's prototype
   is instantiated through the binding site; re-instantiating the stored
   caller reading would erase the very stale-generation mismatch checked. *)
Definition captured_type_at (site prototype : env) (t : ety) : ety :=
  canon site (instantiate site (canon prototype t)).
Definition captured_reading_compatible (site prototype : env) (formal actual : key) : bool :=
  (match get_app prototype formal,get_app site actual with
   | Some(xs,_),Some(ys,_) => Nat.eqb(List.length xs)(List.length ys) &&
       forallb(fun p=>match assoc Nat.eqb(fst p)ys,snd p with
         | Some(Some was),Some required =>
             ety_eqb(captured_type_at site prototype required)(canon site was)
         | Some None,None=>true|_,_=>false end)xs
   | None,None=>true|_,_=>false end) &&
  match key_sig prototype formal,key_sig site actual with
  | Some(ps,t),Some(qs,u)=>Nat.eqb(List.length ps)(List.length qs) &&
      forallb(fun p=>ety_eqb(captured_type_at site prototype(fst p))(canon site(snd p)))
        (combine(t::ps)(u::qs))
  | None,None=>true|_,_=>false end.
Definition check_captured_reading (mi : nat) (site prototype : env) (formal actual : key) : M unit :=
  if captured_reading_compatible site prototype formal actual then ret tt else
    let! description:=show_key actual in
    err mi ("captured " ++ description ++ " has another frozen interpretation than required (D84 3b)").

Definition snapshot_type_at (scope : reify_scope ety key) (name : nat) : option ety :=
  option_map (instantiate (env_with_edges (rs_generations ety key scope) []))
    (nassoc name (rs_names ety key scope)).
(* Check the actual post-body reading, including type effects of intermediate
   functor applications. The pre-body spelling of a pin is insufficient. *)
Definition check_link_fixed_current (mi : nat) (site : env) (fg bg : gsym)
    (fixed : list nat) : M unit :=
  match fixed with
  | []=>ret tt
  | _=>let! s:=get in
      match assoc reify_owner_eqb (ROLink(gs_id fg)(gs_id bg)) (rm_codes(st_reify s)) with
      | Some code=>match rc_exports ety key code with
          | Some profile=>miter(fun name=>
              match snapshot_type_at(rx_actual ety key profile)name,tn_get site name with
              | Some after,Some before=>
                  if ety_eqb(canon site(instantiate site after))(canon site before)
                  then ret tt else let! tg:=sym name in
                    err mi ("`" ++ gs_name bg ++ "` changes fixed input `" ++ gs_name tg ++
                      "` in its actual body (D84 3d)")
              | _,_=>err mi "linked body has no fixed input reading (D84 3d)"
              end)fixed
          | None=>fail "internal: linked body has no result profile" end
      | None=>fail "internal: linked body has no checked code profile" end
  end.

Fixpoint copy_output_check (generic fv gv : env) (gkeys todo seen : list key) : bool :=
  match todo with
  | [] => true
  | k::rest => let mapped := copy_key generic fv k in
      if key_mem mapped seen then copy_output_check generic fv gv gkeys rest seen else
      match find (fun h => key_eqb mapped (copy_key generic gv h)) (rev gkeys) with
      | Some h => copy_slot_eq generic fv gv k h &&
                    copy_output_check generic fv gv gkeys rest (mapped::seen)
      | None => false end
  end.
Definition copy_outputs_match (generic fv gv : env) (fkeys gkeys : list key) : bool :=
  copy_output_check generic fv gv gkeys (rev fkeys) [].

Definition copy_output_compatible (mi : nat) (site ef eg : env) (fg bg : gsym)
    (fixed : list nat) (fneeds gneeds : list key) : M (reify_exports ety key) :=
  let formal := map (fun p => (fst p,canon ef (fst (snd p)))) (en_tn ef) in
  let vars := fold_left (fun acc p => fold_left (fun acc x => if nmem x acc then acc else x::acc) (ety_gens (snd p)) acc) formal [] in
  let solved := fold_left (fun acc x => match acc,tn_get ef x,tn_get site x with
    | Some sol,Some pat,Some actual => copy_match_type vars (canon ef pat) (canon site actual) sol
    | _,_,_ => None end) fixed (Some []) in
  match solved with
  | None => err mi ("fixed inputs of `" ++ gs_name fg ++ "` have incompatible types (D84)")
  | Some solved =>
    let! replacements := mmap (fun x => match nassoc x solved with
      | Some t => ret (x,t)
      | None => let! g := new_sym mi "@copy" KType (ADType "@copy") (OFresh x (gs_id fg)) in
                ret (x,YAbs g) end) vars in
    let subst := env_with_edges replacements [] in
    let generic := env_with_edges
       (map (fun p => (fst p,instantiate subst (snd p))) formal +++ replacements) [] in
    (if forallb (fun p => match nassoc (fst p) formal with
       | Some _ => match tn_get generic (fst p) with
         | Some t => ety_eqb t (copy_interpret generic eg (fst (snd p)))
         | None => false end
       | None => false end) (en_tn eg) &&
       forallb (fun k => match find (fun h => key_eqb (copy_key generic eg k)
                                                    (copy_key generic ef h)) (rev fneeds) with
         | Some h => copy_slot_eq generic eg ef k h | None => false end) gneeds
     then ret tt else err mi ("`" ++ gs_name bg ++ "` cannot be copied to `" ++ gs_name fg ++
                              "`: its input interpretation differs (D78)")) ;;
    let! (fv,fkeys,roles) := fresh_target_output_view ef fg in
    let! (gv,gkeys) := provide_result (target_input_types eg) bg roles in
    (* Reversed keys select the last setter after specialization too. *)
    (if copy_outputs_match generic fv gv fkeys gkeys then ret tt else
      err mi ("`" ++ gs_name bg ++ "` cannot be copied to `" ++ gs_name fg ++
              "`: its effective output interpretation differs (D78)")) ;;
    ret (ReifyExports ety key (map fst roles) fkeys (reify_scope_of fv)
          (reify_scope_of gv) (reify_scope_of ef) roles)
  end.

(* Linked provisions must satisfy G's own canonical input readings for
   every application of F, including when G's context merged more types. *)
Definition defined_link_inputs_compatible (mi : nat) (site ef eg : env) (fg bg : gsym)
    (fixed : list nat) (fneeds gneeds : list key) : M unit :=
  let formal := map (fun p => (fst p,canon ef (fst (snd p)))) (en_tn ef) in
  let vars := fold_left (fun acc p => fold_left (fun acc x => if nmem x acc then acc else x::acc)
               (ety_gens (snd p)) acc) formal [] in
  let solved := fold_left (fun acc x => match acc,tn_get ef x,tn_get site x with
    | Some sol,Some pat,Some actual => copy_match_type vars (canon ef pat) (canon site actual) sol
    | _,_,_ => None end) fixed (Some []) in
  match solved with
  | None => err mi "defined functor binding has incompatible fixed inputs (D84)"
  | Some solved =>
      let! replacements := mmap (fun x => match nassoc x solved with
        | Some t => ret (x,t)
        | None => let! g := new_sym mi "@linked" KType (ADType "@linked") (OFresh x (gs_id fg)) in
                  ret (x,YAbs g) end) vars in
      let subst := env_with_edges replacements [] in
      let generic := env_with_edges
         (map (fun p => (fst p,instantiate subst (snd p))) formal +++ replacements) [] in
      if forallb (fun k => if key_mem k fneeds then copy_slot_eq generic eg ef k k else true) gneeds
      then ret tt
      else err mi ("`" ++ gs_name bg ++ "` cannot provide `" ++ gs_name fg ++
                   "`: its linked input interpretation differs (D84)")
  end.

(* ===================== 12b. Families (m4, D84 rule 7) ===================== *)

(* A family is an abstract functor whose result is method items only, each
   of whose keys (receiver and brackets) determines every P(F) symbol the
   item mentions (lower.is_family).  Syntactic: a property of the
   declaration. *)
Fixpoint family_leaves_ok (fuel : nat) (mi : nat) (ps : list nat) (specs : list spec) {struct fuel} : M bool :=
  match fuel with
  | 0 => ret false
  | S f =>
      mfold (fun ok sp =>
        if negb ok then ret false else
        match sp with
        | Spec (Some p) (Some dn) a =>
            let! ro := resolve_sym mi p in
            match ro with
            | None => ret false
            | Some r =>
                let! mo := find_method_decl mi r dn in
                match mo with
                | None => ret false
                | Some m =>
                    let! keyed := free_type_syms 200 (ATRef p None) mi [] in
                    let bs := opt_default [] a in
                    let! bound := mmap (fun b => let! l := resolve_sym mi (fst b) in
                                                 ret (match l with Some x => [gs_id x] | None => [] end)) bs in
                    let bound := List.concat bound in
                    let! said := mfold (fun o b => match snd b with
                                                   | Spec (Some sp') _ sa => free_type_syms 200 (ATRef sp' sa) mi o
                                                   | _ => ret o
                                                   end) [] bs in
                    let! said := mfold (fun o q => free_type_syms 200 (snd q) (gs_mod m) o) said (fn_params m) in
                    let! said := match fn_ret m with Some t => free_type_syms 200 t (gs_mod m) said | None => ret said end in
                    let said := filter (fun t => negb (nmem t bound)) said +++
                                filter (fun t => nmem t bound && nmem t keyed) said in
                    ret (negb (existsb (fun t => nmem t ps && negb (nmem t keyed)) said))
                end
            end
        | Spec (Some p) None _ =>
            let! go := resolve_sym mi p in
            match go with
            | Some g => match gs_kind g, gs_decl g with
                        | KCtx, ADCtx _ its => family_leaves_ok f (gs_mod g) ps its
                        | _, _ => ret false
                        end
            | None => ret false
            end
        | _ => ret false
        end) true specs
  end.

Definition family_params (fg : gsym) : M (list nat) :=
  let! ps := functor_params fg in
  match ps with Some l => ret l | None => result_mentions fg end.

Definition is_family (fg : gsym) : M bool :=
  match functor_parts fg with
  | (_, res, None) =>
      if negb (kind_eqb (gs_kind fg) KFunctor) then ret false else
      let! ps := family_params fg in
      let! n := walk_fuel in family_leaves_ok n (gs_mod fg) ps res
  | _ => ret false
  end.

(* A family's result items in an environment: (module, receiver symbol,
   method, the substitution its brackets and enclosing contexts make). *)
Fixpoint family_leaves (fuel : nat) (e : env) (mi : nat) (specs : list spec) (outer : list (nat * ety))
    {struct fuel} : M (list (nat * gsym * gsym * list (nat * ety))) :=
  match fuel with
  | 0 => err mi "context nests too deeply"
  | S f =>
      let! ls := mmap (fun sp =>
        match sp with
        | Spec p d a =>
            let! subst := match a with
                          | Some bs => let! s := app_subst 200 e mi bs outer in ret (s +++ outer)
                          | None => ret outer
                          end in
            match d, p with
            | Some dn, Some pp =>
                let! r := resolve_type_symbol mi pp in
                let! mo := find_method_decl mi r dn in
                match mo with Some m => ret [(mi, r, m, subst)] | None => ret [] end
            | None, Some pp =>
                let! tg := resolve_sym mi pp in
                match tg with
                | Some g => match gs_kind g, gs_decl g with
                            | KCtx, ADCtx _ its => family_leaves f e (gs_mod g) its subst
                            | _, _ => ret []
                            end
                | None => ret []
                end
            | _, _ => ret []
            end
        end) specs in
      ret (List.concat ls)
  end.

(* lower.receiver_at: an item's receiver type, canonical *)
Definition receiver_at (e : env) (mi : nat) (rg : gsym) (subst : list (nat * ety)) : M ety :=
  let tm := subst +++ tymap e in
  let! this := match nassoc (gs_id rg) tm with
               | Some y => ret y
               | None => symbol_type 200 mi rg tm e
               end in
  ret (canon e this).

(* the P(F) symbols a key's receiver fixes: matching the receiver pattern
   (P(F) as markers) against the receiver (setdefault, as lower.match) *)
Fixpoint pmatch (fuel : nat) (back : list (nat * nat)) (pat t : ety) (acc : list (nat * ety)) : list (nat * ety) :=
  match fuel with
  | 0 => acc
  | S f =>
      match pat with
      | YAbs g =>
          match nassoc g back with
          | Some p => if is_some (nassoc p acc) then acc else acc +++ [(p, t)]
          | None => acc
          end
      | YNom n xs =>
          match t with
          | YNom n' ys => if Nat.eqb n n' then
                            fold_left (fun acc q => match nassoc (fst q) ys with
                                                   | Some b => pmatch f back (snd q) b acc
                                                   | None => acc
                                                   end) xs acc
                          else acc
          | _ => acc
          end
      | YUnion ms =>
          match t with
          | YUnion ns => if Nat.eqb (List.length ms) (List.length ns)
                         then fold_left (fun acc q => pmatch f back (fst q) (snd q) acc) (combine ms ns) acc
                         else acc
          | _ => acc
          end
      | YRec fs =>
          match t with
          | YRec gs => if Nat.eqb (List.length fs) (List.length gs)
                       then fold_left (fun acc q => pmatch f back (fst q) (snd q) acc)
                                      (combine (map snd fs) (map snd gs)) acc
                       else acc
          | _ => acc
          end
      | _ => acc
      end
  end.

(* lower.family_member: the family in force producing the method key
   (r, m), with P(F) read off it (θ); two families producing it, or two
   items of one family at this key instance, are errors *)
Definition pmatch_full(back:list(nat*nat))(pat t:ety)(acc:list(nat*ety)):=
 pmatch(ety_depth pat)back pat t acc.

Definition family_member (mi : nat) (e : env) (r : ety) (mid : nat) : M (option (gsym * list (nat * ety))) :=
  let! found := mfold (fun found u =>
      let! fg := sym u in
      let! fam := is_family fg in
      if negb fam then ret found else
      let! ps := family_params fg in
      let! marks := mmap (fun p => let! k := fresh_marker_for e r in ret (p, k)) ps in
      let! em := mfold (fun e q => let! s := fresh in ret (set_tn e (fst q) (YAbs (snd q)) s)) e marks in
      let '(_, res, _) := functor_parts fg in
      let! n := walk_fuel in
      let! leaves := family_leaves n em (gs_mod fg) res [] in
      let back := map (fun q => (snd q, fst q)) marks in
      mfold (fun found lf =>
          let '(lmi, rg, m, subst) := lf in
          if negb (Nat.eqb (gs_id m) mid) then ret found else
          let! pat := receiver_at em lmi rg subst in
          let th := pmatch_full back pat r [] in
          let mentioned := filter (fun q => nmem (snd q) (ety_gens pat)) marks in
          if negb (forallb (fun q => is_some (nassoc (fst q) th)) mentioned) then ret found else
          let at_th := canon e (ety_subst_full (map (fun q => (snd q, opt_default (YAbs (snd q)) (nassoc (fst q) th))) marks) pat) in
          if negb (ety_eqb at_th r) then ret found else
          match filter (fun q => Nat.eqb (gs_id (fst q)) u) found with
          | (_, th0) :: _ =>
              if forallb (fun q => match nassoc (fst q) th0 with Some y => ety_eqb y (snd q) | None => false end) th
                 && Nat.eqb (List.length th) (List.length th0) then ret found
              else let! sr := show r in
                   err mi ("`" ++ sr ++ gs_name m ++ "` is two items of the family `" ++ gs_name fg ++
                           "` here, so which one is meant is ambiguous (D84 rule 7)")
          | [] => ret (found +++ [(fg, th)])
          end) found leaves) [] (map fst (elems (en_funs e))) in
  match found with
  | [] => ret None
  | [x] => ret (Some x)
  | (f1, _) :: (f2, _) :: _ =>
      let! sr := show r in let! mg := sym mid in
      err mi ("`" ++ sr ++ gs_name mg ++ "` is produced by two families in force, `" ++ gs_name f1 ++
              "` and `" ++ gs_name f2 ++ "` (D84 rule 7, D44)")
  end.

(* the readings of the symbols ps in e, restored over e' *)
Definition restore_tn (e' e : env) (ps : list nat) : env :=
  let tn := fold_left (fun tn p => match nassoc p (en_tn e) with
                                   | Some v => aset Nat.eqb p v tn
                                   | None => filter (fun q => negb (Nat.eqb (fst q) p)) tn
                                   end) ps (en_tn e') in
  Env tn (en_uf e') (en_vals e') (en_fns e') (en_mths e') (en_funs e') (en_locals e') (en_apps e').

Definition is_memories (fg : gsym) : M bool :=
  let! w := lang "wasm" in
  ret (match w with Some wm => Nat.eqb (gs_mod fg) wm && String.eqb (gs_name fg) "Memories" | None => false end).

(* ===================== 13. The checker (lower.FnChecker) ================== *)

Record cx : Type := Cx {
  cx_mod : nat;                 (* the module names resolve in (apply_body moves it) *)
  cx_sig : fsig;
  cx_this : option ety;
  cx_loop : nat;
  cx_applying : list nat;       (* functors being applied, to catch cycles *)
  cx_nothis : string;
  cx_fresh : list nat;          (* the fresh generations `bind F;` made, innermost last *)
  cx_pins : list nat            (* D84 3d: in a body bound to F, the types of M(F) *)
}.

Definition cx_with_fresh (c : cx) (fr : list nat) : cx :=
  Cx (cx_mod c) (cx_sig c) (cx_this c) (cx_loop c) (cx_applying c) (cx_nothis c) fr (cx_pins c).
Definition cx_in_loop (c : cx) : cx :=
  Cx (cx_mod c) (cx_sig c) (cx_this c) (S (cx_loop c)) (cx_applying c) (cx_nothis c) (cx_fresh c) (cx_pins c).
Definition cx_applying_in (c : cx) (g : gsym) : cx :=
  Cx (gs_mod g) (cx_sig c) (cx_this c) (cx_loop c) (gs_id g :: cx_applying c) (cx_nothis c) (cx_fresh c) (cx_pins c).
Definition cx_restore (c : cx) (inner : cx) : cx := cx_with_fresh c (cx_fresh inner).

Inductive arg : Type := AA (a : aexpr) | AB (x : xexpr) (t : ety).

(* AB arguments in provided_call are the already elaborated operands of
   literal arithmetic. Unlike ordinary source calls, these binary steps
   must take and return exactly their receiver type (D68). *)
Definition literal_sig_ok (e : env) (oty : ety) (args : list arg) (sg : fsig) : bool :=
  match args with
  | [AB _ aty] =>
      match sg_params sg with
      | [(_, pty)] => ety_eqb (canon e aty) (canon e oty) &&
                     ety_eqb (canon e pty) (canon e oty) &&
                     ety_eqb (canon e (sg_ret sg)) (canon e oty)
      | _ => false
      end
  | _ => true
  end.

Definition check_literal_sig (e : env) (mi : nat) (m : gsym) (oty : ety) (args : list arg) (sg : fsig) : M unit :=
  if literal_sig_ok e oty args sg then ret tt else
  err mi ("a literal's `" ++ gs_name m ++ "` must take and return its own type (D68)").

Definition is_never (t : ety) : bool := match t with YNever => true | _ => false end.
Definition is_unit_or_never (t : ety) : bool := match t with YNever | YUnit => true | _ => false end.

(* Branches join on their canonical types (canon: the merges in force), as
   the relation reads them; the message shows them as written (ExecutionModel
   note 59). *)
Definition join (mi : nat) (e : env) (a b : ety) (exp : option ety) : M ety :=
  match exp with
  | Some t => ret t
  | None =>
      if is_never (canon e a) then ret b
      else if is_never (canon e b) || ety_eqb (canon e a) (canon e b) then ret a
      else let! sa := show a in let! sb := show b in
           err mi ("branches disagree: `" ++ sa ++ "` vs `" ++ sb ++ "` (annotate an expected type)")
  end.

Definition bool_type (mi : nat) : M ety :=
  let! bm := lang "bool" in
  match bm with
  | None => err mi "`if`/`while` need lib/bool.moss to be loaded (D45)"
  | Some b => let! m := the_mod b in
              match sassoc "Bool" (gm_names m) with
              | Some s => ret (YNom s [])
              | None => err mi "`if`/`while` need lib/bool.moss to be loaded (D45)"
              end
  end.

(* The library is the prelude's directory, including its subdirectories. *)
Definition in_library (mi : nat) : M bool :=
  let! s := get in
  ret (match w_library (st_w s) with
       | Some dir => String.prefix dir (gm_path (get_mod (st_w s) mi))
       | None => false
       end).

Definition is_platform_sym (name : string) (g : gsym) : M bool :=
  let! w := lang "wasm" in
  match w with
  | Some wm => let! m := the_mod wm in
               ret (match sassoc name (gm_names m) with
                    | Some s => Nat.eqb s (gs_id g) | None => false end)
  | None => ret false
  end.

Definition compile_pattern (mi : nat) (e : env) (p : apat) (sty : ety) (heads : list (nat * ety))
    : M (xpat * list (string * ety) * list nat * bool) :=
  let binder_name (q : apat) : M (option string) :=
    match q with
    | APPath [n] => let! t := rpath mi [n] in ret (match t with None => Some n | Some _ => None end)
    | _ => ret None
    end in
  match p with
  | APWild => ret (XPAny None, [], [], true)
  | APPath pp =>
      let! t := rpath mi pp in
      match t, pp with
      | None, [n] => let! v := local_var n in ret (XPAny (Some v), [(n, sty)], [], true)
      | Some (ESym s), _ =>
          let! g := sym s in
          if kind_eqb (gs_kind g) KUnit || kind_eqb (gs_kind g) KTag then
            (* a member of that head is the tag's (or unit's) application,
               never an abstract type that shares its number (ExecutionModel note 61) *)
            if match nassoc s heads with Some (YNom _ _) => true | _ => false end then ret (XPHead s None [], [], [s], false)
            else let! ss := show sty in err mi ("`" ++ gs_name g ++ "` is not a member of `" ++ ss ++ "`")
          else err mi ("`" ++ show_path pp ++ "` is not a matchable pattern")
      | _, _ => err mi ("`" ++ show_path pp ++ "` is not a matchable pattern")
      end
  | APTag pp sub =>
      let! t := resolve_sym mi pp in
      match t with
      | Some g =>
          if negb (kind_eqb (gs_kind g) KTag) then err mi ("`" ++ show_path pp ++ "` is not a tag") else
          match nassoc (gs_id g) heads with
          | None | Some (YUnit | YNever | YAbs _ | YUnion _ | YRec _) =>
              let! ss := show sty in err mi ("`" ++ gs_name g ++ "` is not a member of `" ++ ss ++ "`")
          | Some mem =>
              let! pay := payload_type mem e in
              let! bn := binder_name sub in
              match bn with
              | Some n => layout_ok e pay ;; let! v := local_var n in
                          ret (XPHead (gs_id g) (Some v) [], [(n, pay)], [gs_id g], false)
              | None => err mi "v0 supports only a binder as a tag pattern payload"
              end
          end
      | None => err mi ("`" ++ show_path pp ++ "` is not a tag")
      end
  | APRec None _ => err mi "v0 record patterns need a tag head"
  | APRec (Some pp) fs =>
      let! t := resolve_sym mi pp in
      match t with
      | Some g =>
          if negb (kind_eqb (gs_kind g) KTag) then err mi ("`" ++ show_path pp ++ "` is not a tag") else
          match nassoc (gs_id g) heads with
          | None | Some (YUnit | YNever | YAbs _ | YUnion _ | YRec _) =>
              let! ss := show sty in err mi ("`" ++ gs_name g ++ "` is not a member of `" ++ ss ++ "`")
          | Some mem =>
              let! pay := payload_type mem e in
              match pay with
              | YRec ftys =>
                  layout_ok e pay ;;
                  let! (bs, xs) := mfold (fun acc q =>
                      let '(bs, xs) := acc in
                      let '(fname, sub) := q in
                      match sassoc fname ftys with
                      | None => err mi ("`" ++ gs_name g ++ "` has no field `" ++ fname ++ "`")
                      | Some fty =>
                          let! fid := fld_id fname in
                          match sub with
                          | None => let! v := local_var fname in ret (bs +++ [(fname, fty)], xs +++ [(fid, v)])
                          | Some APWild => ret (bs, xs)
                          | Some s =>
                              let! bn := binder_name s in
                              match bn with
                              | Some n => let! v := local_var n in ret (bs +++ [(n, fty)], xs +++ [(fid, v)])
                              | None => err mi "v0 record patterns support binders only"
                              end
                          end
                      end) ([], []) fs in
                  ret (XPHead (gs_id g) None xs, bs, [gs_id g], false)
              | _ => err mi ("`" ++ gs_name g ++ "` does not have a record payload")
              end
          end
      | None => err mi ("`" ++ show_path pp ++ "` is not a tag")
      end
  end.

(* digits of a literal *)
Definition digit_val (c : ascii) : nat := nat_of_ascii c - 48.

Fixpoint strip_zeros (l : list ascii) : list ascii :=
  match l with "0"%char :: r => strip_zeros r | _ => l end.

Fixpoint split_at_e (l : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match l with
  | [] => (rev acc, None)
  | c :: r => if (Ascii.eqb c "e"%char || Ascii.eqb c "E"%char) then (rev acc, Some r)
              else split_at_e r (c :: acc)
  end.

Fixpoint split_at_dot (l : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match l with
  | [] => (rev acc, None)
  | c :: r => if Ascii.eqb c "."%char then (rev acc, Some r) else split_at_dot r (c :: acc)
  end.

(* an exponent's value: (negative?, magnitude) *)
Definition parse_exp (l : list ascii) : bool * nat :=
  let '(neg, ds) := match l with
                    | "-"%char :: r => (true, r)
                    | "+"%char :: r => (false, r)
                    | _ => (false, l)
                    end in
  (neg, fold_left (fun n c => 10 * n + digit_val c) ds 0).

(* A literal's text as the parser writes it (Parse.lit_norm): digits, an
   optional `.` and digits, an optional `e`, `-` and digits; no `E` (Track A,
   ExecutionModel note 65: Part V's lit_value reads only that form). *)
Definition is_digit_char (c : ascii) : bool := Nat.leb 48 (nat_of_ascii c) && Nat.leb (nat_of_ascii c) 57.
Definition digits_wf (l : list ascii) : bool :=
  match l with [] => false | _ => forallb is_digit_char l end.
Definition lit_wf (text : string) : bool :=
  let cs := list_ascii_of_string text in
  negb (existsb (fun c => Ascii.eqb c "E"%char) cs) &&
  let '(mant, ex) := split_at_e cs [] in
  let '(whole, frac) := split_at_dot mant [] in
  digits_wf whole &&
  match frac with None => true | Some fr => digits_wf fr end &&
  match ex with None => true | Some ("-"%char :: d) => digits_wf d | Some d => digits_wf d end.

Definition lowered (g : nat) : M bool :=
  let! s := get in ret (is_some (nassoc g (st_fns s))).

Definition emit_fn (g : nat) (body : xexpr) : M unit := upd (s_fns (fun l => (g, body) :: l)).

Definition with_vars {A} (init : list (string * nat)) (m : M A) : M A :=
  let! s := get in
  let saved := (st_vars s, st_nvar s) in
  upd (s_vars init (List.length init)) ;;
  let! r := m in
  upd (s_vars (fst saved) (snd saved)) ;;
  ret r.

Definition expect_defined_fn (mi : nat) (y : aexpr) : M gsym :=
  match y with
  | AEPath p None =>
      let! tg := resolve_sym mi p in
      match tg with
      | Some g => if is_defined_fn g then ret g
                  else err mi ("`" ++ show_path p ++ "` must be a defined function to provide a bind")
      | None => err mi ("`" ++ show_path p ++ "` must be a defined function to provide a bind")
      end
  | _ => err mi "the right side of a fn bind must name a defined function"
  end.

Definition expect_provider (c : cx) (e : env) (y : aexpr) (this : ety) : M gsym :=
  let mi := cx_mod c in
  match y with
  | AEField (AEPath rp None) n =>
      let! rt := resolve_sym mi rp in
      match rt with
      | Some rg =>
          if kind_eqb (gs_kind rg) KUnit || kind_eqb (gs_kind rg) KTag then
            let! hm := the_mod (gs_mod rg) in
            let! here := the_mod mi in
            let cand := match assoc attached_eqb (gs_id rg, n) (gm_attached hm) with
                        | Some s => Some s
                        | None => assoc attached_eqb (gs_id rg, n) (gm_attached here)
                        end in
            let! att := match cand with Some s => let! g := sym s in ret (Some g) | None => ret None end in
            match att with
            | Some ag =>
                if is_some (fn_body ag) then
                  match head (canon e this) with
                  | Some hh => if Nat.eqb hh (gs_id rg) then ret ag
                               else let! s := show this in
                                    err mi ("provider `" ++ gs_name ag ++ "` is attached to `" ++ gs_name rg ++ "`, not `" ++ s ++ "`")
                  | None => let! s := show this in
                            err mi ("provider `" ++ gs_name ag ++ "` is attached to `" ++ gs_name rg ++ "`, not `" ++ s ++ "`")
                  end
                else err mi ("`" ++ gs_name rg ++ "` has no defined attached method `." ++ n ++ "`")
            | None => err mi ("`" ++ gs_name rg ++ "` has no defined attached method `." ++ n ++ "`")
            end
          else expect_defined_fn mi y
      | None => expect_defined_fn mi y
      end
  | _ => expect_defined_fn mi y
  end.

Definition deep {A} : M A := fail "elaboration nests too deeply".

(* Receiver adapters are cached by the physical provider symbol. *)
Definition wrapper_create (pv : gsym) : M nat :=
  let! info := fn_info pv in
      let! sm := smap_at (fi_env info) (fi_env info) (map (fun k => (k, k)) (fi_needs info)) in
      let! w := new_sym (gs_mod pv) (gs_name pv ++ "$recv") KFn (gs_decl pv) OLambda in
      let n := List.length (fn_params pv) in
      let! th := call_theta (fi_env info) pv in
      register_reify_code (ROFunction w) (fi_env info) (fi_needs info) None ;;
      emit_fn w (XCallD (gs_id pv) th sm (map XVar (seq 1 n))) ;;
      upd (s_wrappers (fun m => nset m (gs_id pv) w)) ;;
      ret w.

Definition wrapper_cached (pv : gsym) : M nat :=
  let! s := get in
  match nget (st_wrappers s) (gs_id pv) with
  | Some w => ret w
  | None => let! physical := sym (gs_id pv) in wrapper_create physical
  end.


Definition opt_fits (e : env) (t : ety) (exp : option ety) : bool :=
  match exp with Some x => fits e t x | None => true end.

(* D58's injection: representation does not change in MossX (every nominal
   value carries its tag), but the bootstrap lays the union out here. *)
Definition inject_check (e : env) (t : ety) (exp : option ety) : M unit :=
  match exp with
  | Some w =>
      match canon e w, canon e t with
      | YUnion _ as u, t' =>
          match t' with
          | YUnion _ | YNever => ret tt
          | _ => if ety_eqb t' u then ret tt else
                 match head t' with Some _ => layout_ok e u | None => ret tt end
          end
      | _, _ => ret tt
      end
  | None => ret tt
  end.

(* Runtime primitives operate on machine shapes, not arbitrary declarations
   sharing their spelling. The selected library is checked, not trusted. *)
Definition runtime_names : list (string * string) :=
  [("wasm", "I32"); ("wasm", "I64"); ("wasm", "F32"); ("wasm", "F64");
   ("wasm", "StructRef"); ("wasm", "ArrayRef"); ("wasm", "Memory");
   ("wasm", "Boxed"); ("wasm", "Arrayed"); ("wasm", "Ref");
   ("wasm", "Array"); ("wasm", "Addr");
   ("bool", "Bool"); ("bool", "True"); ("bool", "False")].

Definition runtime_name_eqb (a b : string * string) : bool :=
  String.eqb (fst a) (fst b) && String.eqb (snd a) (snd b).
Definition runtime_schema := list ((string * string) * option nat).
Definition runtime_id (sc : runtime_schema) (short n : string) : option nat :=
  match assoc runtime_name_eqb (short, n) sc with Some x => x | None => None end.

Definition runtime_symbols : M runtime_schema :=
  mmap (fun q => let! lm := lang (fst q) in
                  match lm with
                  | None => ret (q, None)
                  | Some mi => let! m := the_mod mi in ret (q, sassoc (snd q) (gm_names m))
                  end) runtime_names.

Definition runtime_abs (sc : runtime_schema) (n : string) (t : ety) : bool :=
  match runtime_id sc "wasm" n with Some x => ety_eqb t (YAbs x) | None => false end.
Definition runtime_nom (sc : runtime_schema) (short n : string) (t : ety) : bool :=
  match runtime_id sc short n, t with
  | Some x, YNom y _ => Nat.eqb x y
  | _, _ => false
  end.
Definition runtime_content (sc : runtime_schema) (n par : string) (t : ety) : option ety :=
  match runtime_id sc "wasm" n, runtime_id sc "wasm" par, t with
  | Some x, Some p, YNom y xs => if Nat.eqb x y then nassoc p xs else None
  | _, _, _ => None
  end.

Definition runtime_xty (sc : runtime_schema) (t : ety) (x : xty) : bool :=
  match x with
  | XTI32 => runtime_abs sc "I32" t
  | XTI64 => runtime_abs sc "I64" t
  | XTF32 => runtime_abs sc "F32" t
  | XTF64 => runtime_abs sc "F64" t
  | XTUnit => ety_eqb t YUnit
  | XTBool => match runtime_id sc "bool" "Bool" with
              | Some b => ety_eqb t (YNom b []) | None => false end
  | XTAddr => runtime_nom sc "wasm" "Addr" t
  | XTTag => match t with YNom _ _ => true | _ => false end
  end.

Fixpoint runtime_xtys (sc : runtime_schema) (ts : list ety) (xs : list xty) : bool :=
  match ts, xs with
  | [], [] => true
  | t :: ts, x :: xs => runtime_xty sc t x && runtime_xtys sc ts xs
  | _, _ => false
  end.
Definition runtime_signature (sc : runtime_schema) (ps : list ety) (r : ety)
  (s : option (list xty * xty)) : bool :=
  match s with
  | Some (xs, x) => runtime_xtys sc ps xs && runtime_xty sc r x
  | None => false
  end.
Definition runtime_ref (sc : runtime_schema) (arr : bool) (t content : ety) : bool :=
  match runtime_content sc (if arr then "Array" else "Ref")
                            (if arr then "Arrayed" else "Boxed") t with
  | Some c => ety_eqb c content
  | None => false
  end.

Definition runtime_gc_signature (sc : runtime_schema) (p : prim) (ps : list ety) (r : ety) : bool :=
  match p, ps with
  | PBox rf, [c] => match runtime_id sc "wasm" "Ref" with
                   | Some x => Nat.eqb rf x && runtime_ref sc false r c
                   | None => false end
  | PDeref, [t] => runtime_ref sc false t r
  | PRefSet, [t; c] => runtime_ref sc false t c && ety_eqb r YUnit
  | PArrNew ar, [c; i] => runtime_abs sc "I32" i &&
      match runtime_id sc "wasm" "Array" with
      | Some x => Nat.eqb ar x && runtime_ref sc true r c | None => false end
  | PArrLen, [t] => runtime_abs sc "I32" r && is_some (runtime_content sc "Array" "Arrayed" t)
  | PArrGet, [t; i] => runtime_abs sc "I32" i && runtime_ref sc true t r
  | PArrSet, [t; i; c] => runtime_abs sc "I32" i && runtime_ref sc true t c && ety_eqb r YUnit
  | PArrFill, [t; i; c; n] => runtime_abs sc "I32" i && runtime_abs sc "I32" n &&
      runtime_ref sc true t c && ety_eqb r YUnit
  | PArrCopy, [t; i; t'; j; n] => runtime_abs sc "I32" i && runtime_abs sc "I32" j &&
      runtime_abs sc "I32" n && ety_eqb r YUnit &&
      match runtime_content sc "Array" "Arrayed" t, runtime_content sc "Array" "Arrayed" t' with
      | Some c, Some c' => ety_eqb c c' | _, _ => false end
  | _, _ => false
  end.

Definition runtime_primitive (sc : runtime_schema) (p : prim) (ps : list ety) (r : ety) : bool :=
  match p with
  | PUnsupported _ => true
  (* unreachable never returns a value, including at the standard `|` result *)
  | PUnreachable => match ps with [] => true | _ => false end
  | _ => runtime_signature sc ps r (prim_sig p) ||
         runtime_signature sc ps r (prim_msig p) || runtime_gc_signature sc p ps r
  end.

Definition check_runtime_primitive (mi : nat) (e : env) (p : prim) (ps : list ety) (r : ety) : M unit :=
  let! sc := runtime_symbols in
  if runtime_primitive sc p (map (canon e) ps) (canon e r) then ret tt
  else err mi "platform primitive has an incompatible declared signature".

Definition runtime_abstract_names : list string :=
  ["I32"; "I64"; "F32"; "F64"; "StructRef"; "ArrayRef"; "Memory"; "Boxed"; "Arrayed"].

Definition check_runtime_abstracts (sc : runtime_schema) : M unit :=
  let! _ := mfold (fun seen n => match runtime_id sc "wasm" n with
                   | None => ret seen
                   | Some x =>
                       let! g := sym x in
                       if kind_eqb (gs_kind g) KType && negb (nmem x seen)
                       then ret (x :: seen)
                       else err (gs_mod g) ("platform type `" ++ n ++ "` must be a distinct abstract type")
                   end) [] runtime_abstract_names in ret tt.

Definition check_runtime_tag (sc : runtime_schema) (n param handle : string) : M unit :=
  match runtime_id sc "wasm" n with
  | None => ret tt
  | Some x =>
      let! g := sym x in
      let! rs := reqs x in
      match runtime_id sc "wasm" param, runtime_id sc "wasm" handle, rs with
      | Some p, Some h, r :: _ =>
          if kind_eqb (gs_kind g) KTag && Nat.eqb p r then
            let! pay := payload_type (YNom x (map (fun r => (r, YAbs r)) rs)) empty_env in
            if ety_eqb pay (YAbs h) then ret tt
            else err (gs_mod g) ("platform tag `" ++ n ++ "` has an incompatible payload")
          else err (gs_mod g) ("platform tag `" ++ n ++ "` has incompatible parameters")
      | _, _, _ => err (gs_mod g) ("platform tag `" ++ n ++ "` has incompatible parameters")
      end
  end.

Definition runtime_bool_tuple (sc : runtime_schema) : nat * nat * nat :=
  match runtime_id sc "bool" "Bool" with
  | None => (0, 0, 0)
  | Some b => (b, opt_default 0 (runtime_id sc "bool" "True"),
                  opt_default 0 (runtime_id sc "bool" "False"))
  end.

Definition check_runtime_bool (sc : runtime_schema) : M unit :=
  match runtime_id sc "bool" "Bool" with
  | None => ret tt
  | Some b =>
      let! g := sym b in
      let! rs := reqs b in
      match runtime_id sc "bool" "True", runtime_id sc "bool" "False", rs with
      | Some t, Some f, [] =>
          let! gt := sym t in let! gf := sym f in
          if kind_eqb (gs_kind g) KTag && kind_eqb (gs_kind gt) KUnit &&
             kind_eqb (gs_kind gf) KUnit && negb (Nat.eqb t f) then
            let! pay := payload_type (YNom b []) empty_env in
            if ety_eqb pay (YUnion [YNom t []; YNom f []]) ||
               ety_eqb pay (YUnion [YNom f []; YNom t []]) then ret tt
            else err (gs_mod g) "platform Bool must contain exactly True and False"
          else err (gs_mod g) "platform Bool, True and False have incompatible declarations"
      | _, _, _ => err (gs_mod g) "platform Bool, True and False have incompatible declarations"
      end
  end.

(* Fresh machine heads are runtime type identities, never source symbols.
   Replacing platform abstracts injectively makes runtime grounding the same
   closed substitution used by XTyping, including raw GC handles. *)
Definition runtime_machine_names : list string :=
  ["I32"; "I64"; "F32"; "F64"; "StructRef"; "ArrayRef"].
Definition runtime_machines (sc : runtime_schema) (base : nat) : subst :=
  List.concat (map (fun q => match runtime_id sc "wasm" (snd q) with
                            | Some t => [(t, TNom (base + fst q) [])]
                            | None => []
                            end) (combine (seq 0 6) runtime_machine_names)).

Definition runtime_casts (sc : runtime_schema) (base : nat) : list (nsym * xcastspec) :=
  (match runtime_id sc "wasm" "Ref", runtime_id sc "wasm" "Boxed", runtime_id sc "wasm" "StructRef" with
   | Some n, Some t, Some h => [(n, XCastSpec false t (Some (h, TNom (base + 4) [])))]
   | _, _, _ => []
   end) +++
  (match runtime_id sc "wasm" "Array", runtime_id sc "wasm" "Arrayed", runtime_id sc "wasm" "ArrayRef" with
   | Some n, Some t, Some h => [(n, XCastSpec true t (Some (h, TNom (base + 5) [])))]
   | _, _, _ => []
   end).

Definition check_runtime_library : M unit :=
  let! sc := runtime_symbols in
  check_runtime_abstracts sc ;;
  check_runtime_bool sc ;;
  check_runtime_tag sc "Ref" "Boxed" "StructRef" ;;
  check_runtime_tag sc "Array" "Arrayed" "ArrayRef" ;;
  check_runtime_tag sc "Addr" "Memory" "I32".


(* D75/D81: the platform's binding of a functor lib/wasm.moss declares:
   the core's functor h whose body binds each result item to a primitive *)
Definition platform_fun (fg : gsym) : M nat :=
  let! s := get in
  match assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b)) (gs_id fg, gs_id fg) (st_links s) with
  | Some h => ret h
  | None =>
      let mi := gs_mod fg in
      let! (ef, platform_needs) := fun_env fg in
      (* the result in F's own terms, where its items' instantiations are *)
      let! rts := result_types fg in
      let! (efr, platform_results) := provide_result ef fg (map (fun t => (t, t)) rts) in
      let! wm := the_mod mi in
      let ref := opt_default 0 (sassoc "Ref" (gm_names wm)) in
      let arr := opt_default 0 (sassoc "Array" (gm_names wm)) in
      let '(_, res, _) := functor_parts fg in
      let! items := mmap (fun sp =>
          match sp with
          | Spec (Some p) (Some dn) None =>
              let! r := resolve_type_symbol mi p in
              let! mo := find_method_decl mi r dn in
              match mo with
              | None => err mi ("the platform cannot provide `" ++ show_path p ++ "." ++ dn ++ "`")
              | Some m =>
                  let tm := tymap ef in
                  let! this := match nassoc (gs_id r) tm with Some y => ret y | None => symbol_type 200 mi r tm ef end in
                  let! k := method_key ef mi this m in
                  let! it := key_item_at efr k in
                  let pr := match gs_name m with
                            | ".boxed" => Some (PBox ref) | "Ref.deref" => Some PDeref | "Ref.set" => Some PRefSet
                            | ".arrayed" => Some (PArrNew arr) | "Array.length" => Some PArrLen
                            | "Array.get" => Some PArrGet | "Array.set" => Some PArrSet
                            | "Array.fill" => Some PArrFill | "Array.copy" => Some PArrCopy
                            | _ => None
                            end in
                  let check q := match key_sig efr k with
                                 | Some (ps, rt) => check_runtime_primitive mi efr q (this :: ps) rt
                                 | None => err mi "platform method has no declared signature"
                                 end in
                  match pr with
                  | Some q => check q ;; ret (it, [XBPrim it q])
                  | None =>
                      (* m4, D79 as amended: `Memories`'s members, `Addr`'s
                         methods, are the memory primitives, which take their
                         address as an `Addr` (MossX.mem_args) *)
                      let addr := opt_default 0 (sassoc "Addr" (gm_names wm)) in
                      match prim_of_name dn with
                      | Some q =>
                          if negb (Nat.eqb (gs_id r) addr) then
                            err mi ("the platform cannot provide `" ++ show_path p ++ "." ++ dn ++ "`")
                          else check q ;; ret (it, [XBPrim it q])
                      | None => err mi ("the platform cannot provide `" ++ show_path p ++ "." ++ dn ++ "`")
                      end
                  end
              end
          | _ => err mi "the platform cannot provide a non-method item"
          end) res in
      let! h := new_sym mi (gs_name fg) KFunctor (gs_decl fg) (OPlatformFun (gs_id fg)) in
      register_reify_code (ROPlatform (gs_id fg)) ef platform_needs None ;;
      record_reify_exports (ROPlatform (gs_id fg)) rts platform_results ef efr efr ;;
      upd (s_funs (fun l => (h, (map fst items, List.concat (map snd items))) :: l)) ;;
      upd (s_links (fun l => ((gs_id fg, gs_id fg), h) :: l)) ;;
      ret h
  end.

(* provided_call's report when nothing provides m at r (lang_call reports it
   itself, so that a literal's arithmetic is only ever provided_call's
   direct case: Track A, BodyLit.v) *)
Definition no_provision (mi : nat) (e : env) (oty : ety) (m : gsym) (r : ety) (mid : nat) : M (xexpr * ety) :=
  let nm := match gs_name m with String "."%char r => r | s => s end in
  let others := filter (fun q => match head (fst q), head r with
                                 | Some a, Some b => Nat.eqb a b && negb (ety_eqb (fst q) r)
                                 | _, _ => false end)
                       (opt_default [] (nget (en_mths e) mid)) in
  let! so := show oty in
  match others with
  | _ :: _ =>
      let! ws := mmap (fun q => show (fst q)) others in
      err mi ("`." ++ nm ++ "` is provided at `" ++ String.concat "`, `" ws ++ "`, not at `" ++ so ++
              "`: a method provision serves only the instantiation it was made at (D62)")
  | [] => err mi ("no method `." ++ nm ++ "` is available for `" ++ so ++ "` in the context here")
  end.

(* A provider closure and a constant initializer have their own effect
   boundary. Function declarations reference separate bodies, so their local
   returns do not occur in this traversal. *)
Fixpoint no_nonlocal_return_expr (e : xexpr) : bool :=
  match e with
  | XReturn _ => false
  | XVar _ | XUnit | XVl _ | XBreak => true
  | XTag _ _ x | XAssign _ x | XLoop x | XField x _ | XConst _ x => no_nonlocal_return_expr x
  | XCallD _ _ _ xs | XCallA _ xs => forallb no_nonlocal_return_expr xs
  | XMeth x _ _ xs => no_nonlocal_return_expr x && forallb no_nonlocal_return_expr xs
  | XMatch x arms => no_nonlocal_return_expr x && forallb (fun p => no_nonlocal_return_expr (snd p)) arms
  | XLet _ a b | XBindVal _ a b | XSeq a b | XWhile a b => no_nonlocal_return_expr a && no_nonlocal_return_expr b
  | XIf a b c => no_nonlocal_return_expr a && no_nonlocal_return_expr b && no_nonlocal_return_expr c
  | XBindTy _ _ x | XBindFn _ _ _ x | XBindMth _ _ _ _ _ x
  | XBindFun _ _ _ _ x | XBindFunA _ _ _ x | XApply _ _ _ x
  | XApplyR _ _ _ x | XBindFunSym _ _ x | XBindFunCopy _ _ x
  | XBindPrim _ _ x => no_nonlocal_return_expr x
  | XRec fs => forallb (fun p => no_nonlocal_return_expr (snd p)) fs
  | XMatchP x arms => no_nonlocal_return_expr x && forallb (fun p => no_nonlocal_return_expr (snd p)) arms
  | XApplyBr _ arms x => forallb (fun p => forallb no_nonlocal_return_bind (snd (fst p))) arms && no_nonlocal_return_expr x
  end
with no_nonlocal_return_bind (b : xbind) : bool :=
  match b with
  | XBVal _ x => no_nonlocal_return_expr x
  | XBApplyBr _ arms => forallb (fun p => forallb no_nonlocal_return_bind (snd (fst p))) arms
  | _ => true
  end.

Definition check_constant_effects (mi : nat) (x : xexpr) : M unit :=
  if no_nonlocal_return_expr x then ret tt
  else err mi "a constant initializer cannot return from its caller".
Definition check_provider_effects (mi : nat) (bs : list xbind) : M unit :=
  if forallb no_nonlocal_return_bind bs then ret tt
  else err mi "a bound functor body cannot return from its caller".

(* D84 3d also constrains type changes made by abstract applications:
   checking direct type binds alone does not preserve the result's inputs. *)
Definition check_link_pins (mi : nat) (after : env) (pins : list nat) : M unit :=
  miter (fun t =>
    if ety_eqb (canon after (opt_default (YAbs t) (tn_get after t)))
               (canon after (YAbs t))
    then ret tt
    else let! g := sym t in
         err mi ("a functor provider changes the polymorphic result input `" ++
                 gs_name g ++ "` (D84 3d)")) pins.

Fixpoint synth (fuel : nat) (c : cx) (e : env) (x : aexpr) (exp : option ety) {struct fuel}
    : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! (v, t) :=
    match x with
    | AEUnit => ret (XUnit, YUnit)
    | AEThis => match cx_this c with
                | Some t => ret (XVar 0, t)
                | None => err mi ("`this` is only legal inside method bodies" ++ cx_nothis c)
                end
    | AEReturn None =>
        if ety_eqb (canon e (sg_ret (cx_sig c))) YUnit then ret (XReturn XUnit, YNever)
        else let! s := show (sg_ret (cx_sig c)) in
             err mi ("`return` with no value, but the function returns `" ++ s ++ "`")
    | AEReturn (Some y) =>
        let r := sg_ret (cx_sig c) in
        let! (v, t) := synth f c e y (Some r) in
        if fits e t r then ret (XReturn v, YNever)
        else let! a := show t in let! b := show r in err mi ("return of `" ++ a ++ "`, expected `" ++ b ++ "`")
    | AEBreak => if Nat.eqb (cx_loop c) 0 then err mi "`break` outside of a loop" else ret (XBreak, YNever)
    | AEIf _ _ _ => synth_if f c e x exp
    | AEMatch s arms => synth_match f c e s arms exp
    | AEBlock b => let! (v, t, _) := synth_block f c e b exp in ret (v, opt_default t exp)
    | AEPath p a => synth_path f c e p a
    | AECall p a args => synth_call f c e p a args
    | AERec p a fs => synth_record f c e p a fs
    | AEField o n => synth_field f c e o n
    | AEMeth o p args => synth_method f c e o p args
    | AEUnary op y =>
        match operator_method op true with
        | Some (lm, n) => synth_operator f c e lm n op y []
        | None => err mi ("unknown operator `" ++ op ++ "`")
        end
    | AEBinary op l r =>
        match operator_method op false with
        | Some (lm, n) => synth_operator f c e lm n op l [AA r]
        | None => err mi ("unknown operator `" ++ op ++ "`")
        end
    | AELit t => synth_literal f c e t
    | AELam _ _ _ => err mi "an anonymous function may only be the right side of a fn or method bind (D70)"
    end in
  inject_check e t exp ;; ret (v, t)
  end

with synth_arg (fuel : nat) (c : cx) (e : env) (a : arg) (exp : option ety) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  match a with
  | AA x => synth f c e x exp
  | AB v t => inject_check e t exp ;; ret (v, t)
  end end

(* returns the block, its type (`|` if it cannot fall through), and the
   type lower.block_type gives it (its tail's, `()` without one) *)
with synth_block (fuel : nat) (c : cx) (e : env) (b : ablock) (exp : option ety) {struct fuel}
    : M (xexpr * ety * ety) :=
  match fuel with 0 => deep | S f =>
  match b with
  | ABlock ss tl =>
      let! (stmts, e', c', div) := check_stmts f c e ss in
      let! (tx, tty) := match tl with
                        | None => ret (XUnit, if div then YNever else YUnit)
                        | Some y => synth f c' e' y exp
                        end in
      let made := skipn (List.length (cx_fresh c)) (cx_fresh c') in
      let gone := filter (fun g => nmem g made) (ety_gens (canon e' tty)) in
      match gone with
      | g :: _ =>
          let! gg := sym g in
          let! ts := show tty in
          err (cx_mod c) ("this block yields `" ++ ts ++ "`, which mentions `" ++ gs_name gg ++
                          "`, a type `bind F;` made in it: a value of that type cannot leave the block (D78)")
      | [] =>
          if opt_fits e' tty exp then
            ret (fold_stmts stmts tx, tty, match tl with None => YUnit | Some _ => tty end)
          else let! a := show tty in let! b := show (opt_default YUnit exp) in
               err (cx_mod c) ("block yields `" ++ a ++ "`, expected `" ++ b ++ "`")
      end
  end end

with check_stmts (fuel : nat) (c : cx) (e : env) (ss : list astmt) {struct fuel}
    : M (list xstmt * env * cx * bool) :=
  match fuel with 0 => deep | S f =>
  match ss with
  | [] => ret ([], e, c, false)
  | s :: rest =>
      let! (xs, e1, c1, d1) := check_stmt f c e s in
      match rest with
      | [] => ret (xs, e1, c1, d1)
      | _ => let! (ys, e2, c2, d2) := check_stmts f c1 e1 rest in ret (xs +++ ys, e2, c2, d2)
      end
  end end

with check_stmt (fuel : nat) (c : cx) (e : env) (s : astmt) {struct fuel}
    : M (list xstmt * env * cx * bool) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  match s with
  | ASLet n t y | ASVar n t y =>
      let mu := match s with ASVar _ _ _ => LVar | _ => LLet end in
      let! declared := match t with
                       | Some ty => let! d := ety_of ty mi (tymap e) e (cx_this c) in ret (Some d)
                       | None => ret None
                       end in
      let! (v, vt) := synth f c e y declared in
      let! ty := match declared with
                 | Some d => if fits e vt d then ret d
                             else let! a := show d in let! b := show vt in
                                  err mi ("`" ++ n ++ "` is declared `" ++ a ++ "` but initialized with `" ++ b ++ "`")
                 | None => ret vt
                 end in
      let! x := local_var n in
      ret ([SLet x v], set_local e n ty mu, c, false)
  | ASAssign n y =>
      match sassoc n (en_locals e) with
      | None => err mi ("`" ++ n ++ "` is not in scope")
      | Some (ty, LCaptured) =>
          err mi ("`" ++ n ++ "` is a local captured by an anonymous function, which captures it by value and may not assign it (D70)")
      | Some (ty, LLet) => err mi ("`" ++ n ++ "` is not a `var`")
      | Some (ty, LVar) =>
          let! (v, vt) := synth f c e y (Some ty) in
          if fits e vt ty then let! x := local_var n in ret ([SAssign x v], e, c, false)
          else let! a := show vt in let! b := show ty in
               err mi ("cannot assign `" ++ a ++ "` to `" ++ n ++ ": " ++ b ++ "`")
      end
  | ASBind its =>
      let! (xs, e', c') := mfold (fun acc it =>
          let '(xs, e, c) := acc in
          let! (ys, e', c') := match it with
                               | (sp, None) => apply_functor f c e sp
                               | (sp, Some y) => check_bind f c e sp y
                               end in
          ret (xs +++ ys, e', c')) ([], e, c) its in
      ret (xs, e', c', false)
  | ASWhile cnd b =>
      let! cond := check_bool f c e cnd in
      let! (body, _, _) := synth_block f (cx_in_loop c) e b None in
      ret ([SWhile cond body], e, c, false)
  | ASLoop b =>
      let! (body, _, _) := synth_block f (cx_in_loop c) e b None in
      ret ([SLoop body], e, c, negb (has_break_b b))
  | ASExpr y =>
      let! (v, t) := synth f c e y None in
      ret ([SExpr v], e, c, is_never (canon e t))
  end end

with check_bool (fuel : nat) (c : cx) (e : env) (y : aexpr) {struct fuel} : M xexpr :=
  match fuel with 0 => deep | S f =>
  let! bt := bool_type (cx_mod c) in
  let! (v, t) := synth f c e y (Some bt) in
  if fits e t bt then ret v
  else let! s := show t in err (cx_mod c) ("condition is `" ++ s ++ "`, expected `Bool`")
  end

with synth_if (fuel : nat) (c : cx) (e : env) (x : aexpr) (exp : option ety) {struct fuel}
    : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  match x with
  | AEIf cnd thn els =>
      let mi := cx_mod c in
      let! cond := check_bool f c e cnd in
      let! (tx, _, tty) := synth_block f c e thn exp in
      match els with
      | None =>
          if negb (opt_fits e YUnit exp) then err mi "an `if` without `else` yields `()`"
          else if negb (is_unit_or_never (canon e tty)) then
            let! s := show tty in err mi ("an `if` without `else` yields `()`, but its branch yields `" ++ s ++ "`")
          else ret (XIf cond tx XUnit, YUnit)
      | Some y =>
          let! (ex, ety) := match y with
                            | AEIf _ _ _ => synth_if f c e y exp
                            | AEBlock b => let! (v, _, t) := synth_block f c e b exp in ret (v, t)
                            | _ => synth f c e y exp
                            end in
          let! t := join mi e tty ety exp in
          ret (XIf cond tx ex, t)
      end
  | _ => fail "internal: synth_if"
  end end

with synth_match (fuel : nat) (c : cx) (e : env) (s : aexpr) (arms : list (apat * aexpr))
    (exp : option ety) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! (sx, sty0) := synth f c e s None in
  (* the scrutinee's type as the merges in force read it (ExecutionModel note 59) *)
  let sty := canon e sty0 in
  let heads := fold_left (fun hs m => match head m with Some h => aset Nat.eqb h m hs | None => hs end)
                         (members sty) [] in
  let! (xarms, covered, catch, result) := mfold (fun acc arm =>
      let '(xarms, covered, catch, result) := acc in
      let! (pat, binders, ah, ca) := compile_pattern mi e (fst arm) sty heads in
      let e_arm := fold_left (fun e p => set_local e (fst p) (snd p) LLet) binders e in
      let! (bx, bty) :=
        match snd arm with
        | AEBlock b => let! (bx, _, bt) := synth_block f c e_arm b exp in ret (bx, opt_default bt exp)
        | y => let! (bx, bt) := synth f c e_arm y exp in
               if opt_fits e_arm bt exp then ret (bx, bt)
               else let! a := show bt in let! b := show (opt_default YUnit exp) in
                    err mi ("match arm yields `" ++ a ++ "`, expected `" ++ b ++ "`")
        end in
      let! r := match result with None => ret bty | Some r => join mi e r bty exp end in
      ret (xarms +++ [(pat, bx)], covered +++ ah, catch || ca, Some r)) ([], [], false, None) arms in
  if is_never sty then
    match arms with
    | [] => ret (XMatchP sx [], opt_default YNever exp)
    | _ => err mi "a match on `|` takes no arms"
    end
  else
    let missing := filter (fun h => negb (nmem (fst h) covered)) heads in
    if negb catch && (negb (Nat.eqb (List.length missing) 0) || Nat.eqb (List.length heads) 0) then
      let! names := mmap (fun h => let! g := sym (fst h) in ret (gs_name g)) missing in
      let! ss := show sty in
      err mi ("match is not exhaustive; missing " ++ match names with [] => ss | _ => String.concat ", " names end)
    else ret (XMatchP sx xarms, opt_default YNever result)
  end

with const_val (fuel : nat) (g : gsym) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let! s := get in
  match nget (st_consts s) (gs_id g) with
  | Some r => replay_reify_constant (gs_id g) ;; ret r
  | None =>
      match gs_assumes g, gs_decl g with
      | _ :: _, _ => err (gs_mod g) ("defined val `" ++ gs_name g ++ "` cannot take assumptions yet (D50)")
      | [], ADVal _ t (Some init) =>
          let! declared := ety_of t (gs_mod g) [] empty_env None in
          let c := Cx (gs_mod g) (FSig [] declared None) None 0 [] "" [] [] in
          let start := List.length (current_reify_events (st_reify s)) in
          let! (v, vt) := with_vars [] (synth f c empty_env init (Some declared)) in
          if fits empty_env vt declared then
            (* the cache only grows (Track A: BodySound's Later reads every
               entry; an initializer never caches its own val, which would
               recurse until the fuel runs out) *)
            remember_reify_constant (gs_id g) start ;;
            upd (s_consts (fun m => match nget m (gs_id g) with
                                    | Some _ => m
                                    | None => nset m (gs_id g) (v, declared)
                                    end)) ;; ret (v, declared)
          else let! a := show declared in let! b := show vt in
               err (gs_mod g) ("`" ++ gs_name g ++ "` is declared `" ++ a ++ "` but its initializer is `" ++ b ++ "`")
      | _, _ => fail "internal: const"
      end
  end end

with synth_path (fuel : nat) (c : cx) (e : env) (p : path) (a : sapp) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let local := match p, a with
               | [n], None => match sassoc n (en_locals e) with Some (t, _) => Some (n, t) | None => None end
               | _, _ => None
               end in
  match local with
  | Some (n, t) => let! x := local_var n in ret (XVar x, t)
  | None =>
      (* a value is not applied to brackets: no rule reads them (ExecutionModel note 45) *)
      match a with
      | Some _ => err mi ("`" ++ show_path p ++ "[…]`: only a type or a tag constructor takes brackets")
      | None =>
      let! tg := rpath mi p in
      match tg with
      | None => err mi ("`" ++ show_path p ++ "` is not in scope")
      | Some (EMod _) => err mi ("`" ++ show_path p ++ "` is a module, not a value")
      | Some (ESym s) =>
          let! g := sym s in
          match gs_kind g, gs_decl g with
          | KUnit, _ => ret (XTag s [] XUnit, YNom s [])
          | KVal, ADVal _ _ (Some _) =>
              let! (initializer,t) := const_val f g in
              check_constant_effects mi initializer ;;
              ret (XConst s initializer,t)
          | KVal, _ => match nget (en_vals e) s with
                       | Some t => ret (XVl s, t)
                       | None => err mi ("`" ++ gs_name g ++ "` is not available in the context here (assume or bind it)")
                       end
          | KFn, _ => err mi ("`" ++ gs_name g ++ "` is a function; call it (no first-class fns yet)")
          | KTag, _ => err mi ("`" ++ gs_name g ++ "` is a tag; construct it with a payload")
          | _, _ => err mi ("`" ++ gs_name g ++ "` is not a value")
          end
      end
      end
  end end

with check_args (fuel : nat) (c : cx) (e : env) (nm : string) (sg : fsig) (args : list arg) {struct fuel}
    : M (list xexpr) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  if negb (Nat.eqb (List.length args) (List.length (sg_params sg))) then
    err mi ("`" ++ nm ++ "` takes " ++ nat_to_string (List.length (sg_params sg)) ++
            " argument(s), got " ++ nat_to_string (List.length args))
  else
    mmap (fun pa =>
            let '((pn, pty), a) := pa in
            let! (v, t) := synth_arg f c e a (Some pty) in
            if fits e t pty then ret v
            else let! x := show t in let! y := show pty in
                 err mi ("argument `" ++ pn ++ "` of `" ++ nm ++ "` is `" ++ x ++ "`, expected `" ++ y ++ "`"))
         (combine (sg_params sg) args)
  end

with synth_call (fuel : nat) (c : cx) (e : env) (p : path) (a : sapp) (args : list aexpr) {struct fuel}
    : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! tg := rpath mi p in
  match tg with
  | None => err mi ("`" ++ show_path p ++ "` is not in scope")
  | Some (EMod _) => err mi ("`" ++ show_path p ++ "` is a module")
  | Some (ESym s) =>
      let! g := sym s in
      match gs_kind g with
      | KTag =>
          let! tm := match a with
                     | Some bs => check_relevant mi g bs ;;
                                  let! sb := app_subst 200 e mi bs (tymap e) in ret (sb +++ tymap e)
                     | None => ret (tymap e)
                     end in
          let! xs := nominal_args mi g tm in
          let tag_ty := YNom s xs in
          let! pay := payload_type tag_ty e in
          match args with
          | [y] =>
              let! (v, vt) := synth f c e y (Some pay) in
              if negb (fits e vt pay) then
                let! x := show vt in let! z := show pay in
                err mi ("payload of `" ++ gs_name g ++ "` is `" ++ x ++ "`, expected `" ++ z ++ "`")
              else
                (* XTag performs the native checked cast for selected GC
                   wrappers; reconstruction is valid in every source module. *)
                let! th := tag_theta e xs in
                ret (XTag s th v, tag_ty)
          | _ => err mi ("tag `" ++ gs_name g ++ "` takes exactly one payload")
          end
      | KFn =>
          match a with
          | Some bs => check_call_brackets mi g bs ;; synth f c e (Syntax.call_block p bs args) None
          | None =>
              let! mem := is_memory_instruction g in
              let! ms := memory_sym in
              (if mem then match ms with
                           | Some m => if is_some (tn_get e m) then upd (s_memory false true) else ret tt
                           | None => ret tt end
               else ret tt) ;;
              if is_defined_fn g then
                let! info := fn_info g in
                requirements_in_force mi e g info "calling" ;;
                let e := principal_env e info in
                let! (pre, e) := demand_needs f c e info in
                let! pairs := require_needs mi e g info in
                let! sm := smap_at (fi_env info) e pairs in
                let! sg := fn_sig g (tymap e) e None in
                let! xs := check_args f c e (gs_name g) sg (map AA args) in
                layout_ok e (sg_ret sg) ;;
                let! th := call_theta e g in
                ret (fold_stmts pre (XCallD s th sm xs), sg_ret sg)
              else
                match nget (en_fns e) s with
                | None => err mi ("`" ++ gs_name g ++ "` is not available in the context here (assume or bind it)")
                | Some sg =>
                    let! xs := check_args f c e (gs_name g) sg (map AA args) in
                    layout_ok e (sg_ret sg) ;;
                    ret (XCallA s xs, sg_ret sg)
                end
          end
      | _ => err mi ("`" ++ gs_name g ++ "` is not callable")
      end
  end end

with synth_record (fuel : nat) (c : cx) (e : env) (p : path) (a : sapp) (fs : list (name * option aexpr))
    {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! tg := resolve_sym mi p in
  match tg with
  | Some g =>
      if negb (kind_eqb (gs_kind g) KTag) then err mi ("`" ++ show_path p ++ "` is not a tag with a record payload") else
      let! tm := match a with
                 | Some bs => check_relevant mi g bs ;;
                              let! sb := app_subst 200 e mi bs (tymap e) in ret (sb +++ tymap e)
                 | None => ret (tymap e)
                 end in
      let! xs := nominal_args mi g tm in
      let tag_ty := YNom (gs_id g) xs in
      let! pay := payload_type tag_ty e in
      match pay with
      | YRec ftys =>
          let! given := mfold (fun given q =>
              let '(fname, val) := q in
              match sassoc fname ftys with
              | None => err mi ("`" ++ gs_name g ++ "` has no field `" ++ fname ++ "`")
              | Some fty =>
                  if is_some (sassoc fname given) then err mi ("duplicate field `" ++ fname ++ "`") else
                  let! (v, vt) := synth f c e (match val with Some y => y | None => AEPath [fname] None end) (Some fty) in
                  if fits e vt fty then ret (given +++ [(fname, v)])
                  else let! x := show vt in let! z := show fty in
                       err mi ("field `" ++ fname ++ "` is `" ++ x ++ "`, expected `" ++ z ++ "`")
              end) [] fs in
          let missing := filter (fun q => negb (is_some (sassoc (fst q) given))) ftys in
          match missing with
          | _ :: _ => err mi ("missing field(s) " ++ String.concat ", " (map fst missing) ++ " of `" ++ gs_name g ++ "`")
          | [] =>
              (* every field given once: a payload that names a field twice
                 has no record literal (ExecutionModel note 62) *)
              if negb (Nat.eqb (List.length fs) (List.length ftys)) then
                err mi ("`" ++ gs_name g ++ "` names a field twice, so no record literal builds it")
              else
              let! xfs := mmap (fun q => let! i := fld_id (fst q) in
                                         ret (i, opt_default XUnit (sassoc (fst q) given))) ftys in
              let! th := tag_theta e xs in
              ret (XTag (gs_id g) th (XRec xfs), tag_ty)
          end
      | _ => err mi ("`" ++ gs_name g ++ "` does not have a record payload")
      end
  | None => err mi ("`" ++ show_path p ++ "` is not a tag with a record payload")
  end end

with synth_field (fuel : nat) (c : cx) (e : env) (o : aexpr) (n : name) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! (ox, oty) := synth f c e o None in
  let! record := match oty with
                 | YNom s _ => let! g := sym s in
                               if kind_eqb (gs_kind g) KTag then payload_type oty e else ret oty
                 | _ => ret oty
                 end in
  match record with
  | YRec ftys =>
      layout_ok e record ;;
      match sassoc n ftys with
      | Some t => let! i := fld_id n in ret (XField ox i, t)
      | None => let! s := show oty in err mi ("`" ++ s ++ "` has no field `" ++ n ++ "`")
      end
  | _ => let! s := show oty in err mi ("`" ++ s ++ "` has no fields")
  end end

with synth_method (fuel : nat) (c : cx) (e : env) (o : aexpr) (p : path) (args : list aexpr) {struct fuel}
    : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! (ox, oty) := synth f c e o None in
  let h := head (canon e oty) in
  let nm := last p "" in
  let! here := the_mod mi in
  let! m0 := match p with
             | [_] => ret None
             | _ => let! d := rdetached mi p in
                    match d with
                    | Some s => let! g := sym s in ret (Some g)
                    | None => err mi ("`" ++ show_path p ++ "` is not a detached method in scope")
                    end
             end in
  let attached_in (hid : nat) : M (option gsym) :=
    let! hg := sym hid in
    let! hm := the_mod (gs_mod hg) in
    match assoc attached_eqb (hid, nm) (gm_attached hm) with
    | Some s => let! g := sym s in ret (Some g)
    | None => match assoc attached_eqb (hid, nm) (gm_attached here) with
              | Some s => let! g := sym s in ret (Some g)
              | None => ret None
              end
    end in
  let detached := sassoc nm (gm_detached here) in
  (* D77: a provision of the detached method in scope wins over an attached one *)
  let! m1 := match m0, p, h with
             | None, [_], Some hid =>
                 let! att := attached_in hid in
                 match att, detached with
                 | Some _, Some d =>
                     let! dg := sym d in
                     if is_some (get_mth e (canon e oty) d) then ret (Some dg) else ret None
                 | _, _ => ret None
                 end
             | _, _, _ => ret m0
             end in
  match m1, h, detached with
  | None, None, None => let! s := show oty in err mi ("`" ++ s ++ "` has no methods (it has no nominal head)")
  | _, _, _ =>
  let! direct := match m1, h with
                 | None, Some hid => attached_in hid
                 | _, _ => ret None
                 end in
  match direct with
  | Some att =>
      if is_some (fn_body att) then
        (* a defined attached method: a direct call, at its receiver's instantiation (D74) *)
        let! info := fn_info att in
        let receiver := canon e oty in
        let! e2 := match receiver with
                   | YNom _ ((_ :: _) as xs) => mfold (fun e q => let! st := fresh in ret (set_tn e (fst q) (snd q) st)) e xs
                   | _ => ret e
                   end in
        requirements_in_force mi e2 att info "calling" ;;
        let e2 := principal_env e2 info in
        let! (pre, e2) := demand_needs f c e2 info in
        let! pairs := require_needs mi e2 att info in
        let! sm := smap_at (fi_env info) e2 pairs in
        let! sg := fn_sig att (tymap e2) e2 (Some oty) in
        let! xs := check_args f c e (gs_name att) sg (map AA args) in
        layout_ok e (sg_ret sg) ;;
        let! th := call_theta e2 att in
        ret (fold_stmts pre (XCallD (gs_id att) th sm (ox :: xs)), sg_ret sg)
      else provided_call f c e ox oty att (map AA args)
  | None =>
      match m1, p with
      | Some m, _ => provided_call f c e ox oty m (map AA args)
      | None, [_] =>
          match detached with
          | Some d => let! dg := sym d in provided_call f c e ox oty dg (map AA args)
          | None => err mi ("`." ++ nm ++ "` is not a detached method in scope; import it, or call it as `mod::m` (D61)")
          end
      | None, _ => err mi ("`" ++ show_path p ++ "` is not a detached method in scope")
      end
  end
  end end

(* A call of a method the context provides at the receiver's type.  D84
   rule 7 (family members on demand) slots in at the miss below: where the
   key is in a family's result, apply the family at θ read off the key. *)
with provided_call (fuel : nat) (c : cx) (e : env) (ox : xexpr) (oty : ety) (m : gsym) (args : list arg)
    {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! k := method_key e mi oty m in
  match k with
  | KyMth r mid =>
      match get_mth e r mid with
      | Some sg =>
          check_literal_sig e mi m oty args sg ;;
          let! xs := check_args f c e (gs_name m) sg args in
          let! it := key_item_at e k in
          layout_ok e (sg_ret sg) ;;
          ret (XMeth ox mid it xs, match args with [AB _ _] => oty | _ => sg_ret sg end)
      | None =>
          (* m4, D84 rule 7: a family in force producing the key is applied
             on demand, at θ read off the key, in a scope of its own *)
          let! fm := family_member mi e r mid in
          match fm with
          | Some (fg, th) =>
              let! itk := key_item k in
              let! (pre, e2) := apply_family f c e fg th [itk] in
              match get_mth e2 r mid with
              | Some sg =>
                  check_literal_sig e mi m oty args sg ;;
                  let! xs := check_args f c e (gs_name m) sg args in
                  let! it := key_item_at e2 k in
                  layout_ok e (sg_ret sg) ;;
                  ret (fold_stmts pre (XMeth ox mid it xs), match args with [AB _ _] => oty | _ => sg_ret sg end)
              | None => let! sr := show r in
                        err mi ("`" ++ gs_name fg ++ "` does not provide `" ++ sr ++ gs_name m ++ "` here (D84 rule 7)")
              end
          | None =>
          let nm := match gs_name m with String "."%char r => r | s => s end in
          let others := filter (fun q => match head (fst q), head r with
                                         | Some a, Some b => Nat.eqb a b && negb (ety_eqb (fst q) r)
                                         | _, _ => false end)
                               (opt_default [] (nget (en_mths e) mid)) in
          let! so := show oty in
          match others with
          | _ :: _ =>
              let! ws := mmap (fun q => show (fst q)) others in
              err mi ("`." ++ nm ++ "` is provided at `" ++ String.concat "`, `" ws ++ "`, not at `" ++ so ++
                      "`: a method provision serves only the instantiation it was made at (D62)")
          | [] => err mi ("no method `." ++ nm ++ "` is available for `" ++ so ++ "` in the context here")
          end
          end
      end
  | _ => fail "internal: method key"
  end end

(* m4, D84 rule 7: `bind F;` at θ (P(F) read as θ for the application only).
   `want` is the members the site uses; the elaborator no longer reads it
   (the application is narrowed to the members used after elaboration, by
   FastPath.fast_program). *)
with apply_family (fuel : nat) (c : cx) (e : env) (fg : gsym) (th : list (nat * ety)) (want : list item) {struct fuel}
    : M (list xstmt * env) :=
  match fuel with 0 => deep | S f =>
  family_inputs_in_force (cx_mod c) e fg th ;;
  let! e1 := mfold (fun e q => let! s := fresh in ret (set_tn e (fst q) (snd q) s)) e th in
  let! (pre, e2, _) := apply_abstract f c e1 fg in
  let! mem := is_memories fg in
  (if mem then upd (s_memory false true) else ret tt) ;;
  ret (pre, restore_tn e2 e (map fst th))
  end

(* m4, D84 rule 7 for a callee's needs (lower.demand_needs): each method need
   nothing here provides but a family in force produces, applied before the
   call, one application per family and θ *)
with demand_needs (fuel : nat) (c : cx) (e : env) (info : fninfo) {struct fuel} : M (list xstmt * env) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! apps := mfold (fun apps need =>
      match need with
      | KyMth r m =>
          let! mg := sym m in
          let! k := method_key e mi (instantiate e r) mg in
          if key_present e k then ret apps else
          match k with
          | KyMth r' _ =>
              let! fm := family_member mi e r' m in
              match fm with
              | Some (fg, th) =>
                  let! itk := key_item k in
                  let same q := Nat.eqb (gs_id (fst (fst q))) (gs_id fg) &&
                                Nat.eqb (List.length (snd (fst q))) (List.length th) &&
                                forallb (fun p => match nassoc (fst p) (snd (fst q)) with
                                                  | Some y => ety_eqb y (snd p) | None => false end) th in
                  if existsb same apps
                  then ret (map (fun q => if same q then (fst q, snd q +++ [itk]) else q) apps)
                  else ret (apps +++ [((fg, th), [itk])])
              | None => ret apps
              end
          | _ => ret apps
          end
      | _ => ret apps
      end) [] (fi_needs info) in
  mfold (fun acc q => let '(ss, e) := acc in
                      let! (ys, e') := apply_family f c e (fst (fst q)) (snd (fst q)) (snd q) in ret (ss +++ ys, e')) ([], e) apps
  end

with synth_operator (fuel : nat) (c : cx) (e : env) (lm n op : string) (operand : aexpr) (args : list arg)
    {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! m := lang_symbol lm n true (if String.eqb lm "access" then "`for`" else "`" ++ op ++ "`") mi in
  let! (ox, oty) := synth f c e operand None in
  match head (canon e oty) with
  | None => let! s := show oty in err mi ("`" ++ s ++ "` has no methods (it has no nominal head)")
  | Some _ => provided_call f c e ox oty m args
  end end

(* ---- literals (D68): arithmetic over fixed library symbols ---- *)
with lang_call (fuel : nat) (c : cx) (e : env) (o : xexpr * ety) (n : string) (args : list (xexpr * ety))
    (why : string) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let! m := lang_symbol "num" n true why (cx_mod c) in
  provided_call f c e (fst o) (snd o) m (map (fun a => AB (fst a) (snd a)) args)
  end

with int_literal (fuel : nat) (c : cx) (e : env) (ds : list ascii) (why : string) {struct fuel}
    : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let lang_val (n : string) : M (xexpr * ety) :=
    let! g := lang_symbol "int" n false why mi in
    match nget (en_vals e) (gs_id g) with
    | Some t => ret (XVl (gs_id g), t)
    | None => err mi (why ++ " means `" ++ n ++ "`, which is not available in the context here (assume or bind it)")
    end in
  match strip_zeros ds with
  | [] => lang_val "zero"
  | d0 :: rest =>
      let! one := lang_val "one" in
      let small (n : nat) : M (xexpr * ety) :=
        mfold (fun acc _ => lang_call f c e acc "add" [one] why) one (repeat tt (n - 1)) in
      let! acc := small (digit_val d0) in
      let! ten := match rest with [] => ret one | _ => small 10 end in
      mfold (fun acc d =>
               let! acc := lang_call f c e acc "mul" [ten] why in
               if Ascii.eqb d "0"%char then ret acc
               else let! sd := small (digit_val d) in lang_call f c e acc "add" [sd] why) acc rest
  end end

with float_of (fuel : nat) (c : cx) (e : env) (v : xexpr * ety) (why : string) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! g := lang_symbol "float" "float" false why mi in
  match nget (en_fns e) (gs_id g) with
  | None => err mi (why ++ " means `float`, which is not available in the context here (assume or bind it)")
  | Some sg =>
      let! xs := check_args f c e (gs_name g) sg [AB (fst v) (snd v)] in
      ret (XCallA (gs_id g) xs, sg_ret sg)
  end end

with synth_literal (fuel : nat) (c : cx) (e : env) (text : string) {struct fuel} : M (xexpr * ety) :=
  match fuel with 0 => deep | S f =>
  let why := "the literal `" ++ text ++ "`" in
  if negb (lit_wf text) then err (cx_mod c) (why ++ " is not a numeric literal (D68)") else
  let '(mant, ex) := split_at_e (list_ascii_of_string text) [] in
  let '(whole, frac) := split_at_dot mant [] in
  match ex, frac with
  | None, None => int_literal f c e mant why
  | _, _ =>
      let fr := opt_default [] frac in
      let '(eneg, emag) := match ex with Some l => parse_exp l | None => (false, 0) end in
      (* shift = exponent - #fraction digits, as a sign and magnitude *)
      let '(sneg, smag) := if eneg then (true, emag + List.length fr)
                           else if Nat.leb (List.length fr) emag then (false, emag - List.length fr)
                           else (true, List.length fr - emag) in
      let digits := strip_zeros (whole +++ fr) in
      let! mantissa :=
        match digits with
        | [] => let! z := int_literal f c e ["0"%char] why in float_of f c e z why
        | d0 :: rest =>
            let! i10 := int_literal f c e ["1"%char; "0"%char] why in
            let! ten := float_of f c e i10 why in
            let! i0 := int_literal f c e [d0] why in
            let! m0 := float_of f c e i0 why in
            mfold (fun acc d =>
                     let! acc := lang_call f c e acc "mul" [ten] why in
                     if Ascii.eqb d "0"%char then ret acc
                     else let! id := int_literal f c e [d] why in
                          let! fd := float_of f c e id why in
                          lang_call f c e acc "add" [fd] why) m0 rest
        end in
      match smag with
      | 0 => ret mantissa
      | _ =>
          let! i10 := int_literal f c e ["1"%char; "0"%char] why in
          let! ten := float_of f c e i10 why in
          let! scale := mfold (fun acc _ => lang_call f c e acc "mul" [ten] why) ten (repeat tt (smag - 1)) in
          lang_call f c e mantissa (if sneg then "div" else "mul") [scale] why
      end
  end end

(* ---- binds (FnChecker.check_bind) ---- *)
with check_bind (fuel : nat) (c : cx) (e : env) (sp : spec) (y : aexpr) {struct fuel}
    : M (list xstmt * env * cx) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  match sp with
  | Spec _ None (Some _) => err mi "a bind left-hand side takes no bracket application"
  | Spec (Some pp) (Some dn) a =>
      (* a method: `bind BigList.get[Elem=Big] = Packed.get;` (D61) *)
      let! tm := match a with
                 | Some bs => let! sb := app_subst 200 e mi bs [] in ret (sb +++ tymap e)
                 | None => ret (tymap e)
                 end in
      let! r := resolve_type_symbol mi pp in
      let! mo := find_method_decl mi r dn in
      match mo with
      | None => err mi ("no method `." ++ dn ++ "` for `" ++ gs_name r ++ "` is in scope")
      | Some m =>
          let! this := symbol_type 200 mi r tm e in
          let! sg := fn_sig m tm e (Some this) in
          let! ts := show this in
          let! (target, pre, sm) :=
            match y with
            | AELam ps rt body =>
                lambda_provider f c e ps rt body m sg (Some this) ("`" ++ ts ++ gs_name m ++ "`")
            | _ =>
                let! pv := expect_provider c e y this in
                let! sm := match_fn_sig f c e pv sg "method" in
                let! tgt := if is_some (gs_recv pv) then ret (gs_id pv) else wrapper f pv in
                ret (tgt, [], sm)
            end in
          let! k := method_key e mi this m in
          (* m4, D84 rule 7 (O3 a): a member of a family in force is provided
             by the family, not bound directly *)
          let! fm := match k with KyMth t mid => family_member mi e t mid | _ => ret None end in
          (match fm with
           | Some (fg, _) => err mi ("`" ++ ts ++ gs_name m ++ "` is a member of the family `" ++ gs_name fg ++
                                     "` in force, which provides it; it cannot be bound directly (D84 rule 7)")
           | None => ret tt
           end) ;;
          let e := match k with KyMth t mid => set_mth e t mid sg | _ => e end in
          let! e := provided_at e k m tm true in
          match k with
          | KyMth t mid => let! t' := ety_ty t in let! th := key_theta e k in
                           record_reify_event (REMethod ety key t mid target (reify_sig e sg) (map (fun p => (fst p,canon e (snd p))) tm)) ;;
                           ret (pre +++ [SBindMth t' mid th target sm], e, c)
          | _ => fail "internal: method key"
          end
      end
  | Spec (Some pp) None None =>
      let! tg := rpath mi pp in
      match tg with
      | None => err mi ("`" ++ show_path pp ++ "` is not in scope")
      | Some (EMod _) => err mi "cannot bind a module"
      | Some (ESym s) =>
          let! g := sym s in
          match gs_kind g with
          | KType =>
              match y with
              | AEPath rp ra =>
                  (if nmem s (cx_pins c) then
                     err mi ("`bind " ++ gs_name g ++ "` in a functor bound to an abstract one changes a type its result mentions; the fixed set is not widened per binding (D84 3d)")
                   else ret tt) ;;
                  let! ty := ety_of (ATRef rp ra) mi (tymap e) e None in
                  let! ms := memory_sym in
                  (match ms with Some m => if Nat.eqb m s then upd (s_memory true false) else ret tt
                               | None => ret tt end) ;;
                  let! stamp := fresh in
                  let! ty' := ety_ty (canon e ty) in
                  record_reify_event (REType ety key s (canon e ty)) ;;
                  ret ([SBindTy s ty'], set_tn e s (canon e ty) stamp, c)
              | _ => err mi ("binding type `" ++ gs_name g ++ "` needs a type on the right")
              end
          | KVal =>
              match gs_decl g with
              | ADVal _ t _ =>
                  let! declared := ety_of t (gs_mod g) (tymap e) e None in
                  let! (v, vt) := synth f c e y (Some declared) in
                  if negb (fits e vt declared) then
                    let! a := show vt in let! b := show declared in
                    err mi ("bind of `" ++ gs_name g ++ "`: got `" ++ a ++ "`, its declared type is `" ++ b ++ "`")
                  else
                    let e := set_val e s declared in
                    let! e := provided_at e (KyVal s) g (tymap e) false in
                    record_reify_event (REValue ety key s (canon e declared) (reify_arguments e)) ;;
                    ret ([SBindVal s v], e, c)
              | _ => fail "internal: val"
              end
          | KFunctor => let! (xs, e) := bind_functor f c e g y in ret (xs, e, c)
          | KFn =>
              let! sg := fn_sig g (tymap e) e None in
              let! (target, pre, sm) :=
                match y with
                | AELam ps rt body => lambda_provider f c e ps rt body g sg None ("`" ++ gs_name g ++ "`")
                | _ => let! pv := expect_fn_provider f c e y in
                       let! sm := match_fn_sig f c e pv sg "fn" in
                       ret (gs_id pv, [], sm)
                end in
              let e := set_fn e s sg in
              let! e := provided_at e (KyFn s) g (tymap e) false in
              record_reify_event (REFunction ety key s target (reify_sig e sg) (reify_arguments e)) ;;
              ret (pre +++ [SBindFn s target sm], e, c)
          | k => err mi ("cannot bind `" ++ gs_name g ++ "` (a " ++ kind_name k ++ ")")
          end
      end
  | _ => err mi "a bind needs a left-hand side"
  end end

with expect_fn_provider (fuel : nat) (c : cx) (e : env) (y : aexpr) {struct fuel} : M gsym :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  match y with
  | AEPath p None =>
      let! tg := resolve_sym mi p in
      match tg with
      | Some g =>
          if kind_eqb (gs_kind g) KFn && negb (is_some (fn_body g)) then
            if is_some (nget (en_fns e) (gs_id g)) then forwarder f g
            else err mi ("`" ++ gs_name g ++ "` is not available in the context here, so it cannot provide a bind (assume or bind it)")
          else expect_defined_fn mi y
      | None => expect_defined_fn mi y
      end
  | _ => expect_defined_fn mi y
  end end

(* D27: the provider's signature must match after the substitutions in force *)
with match_fn_sig (fuel : nat) (c : cx) (e : env) (pv : gsym) (wanted : fsig) (kind : string) {struct fuel}
    : M smap :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  let! info := fn_info pv in
  requirements_in_force mi e pv info "binding to" ;;
  let! pairs := require_needs mi e pv info in
  let! got := fn_sig pv (tymap e) e None in
  if sig_eqb_in e got e wanted then
    record_reify_event (REProvider ety key (gs_id pv) (fi_needs info)
      (reify_sig e got) (reify_arguments e)) ;;
    smap_at (fi_env info) e pairs
  else
    let! gp := mmap (fun p => show (snd p)) (sg_params got) in
    let! gr := show (sg_ret got) in
    let! wp := mmap (fun p => show (snd p)) (sg_params wanted) in
    let! wr := show (sg_ret wanted) in
    err mi (kind ++ " bind signature mismatch: provider `" ++ gs_name pv ++ "` has (" ++
            String.concat ", " gp ++ ") -> " ++ gr ++ ", need (" ++ String.concat ", " wp ++ ") -> " ++ wr ++
            (if String.eqb kind "method" && negb (is_some (gs_recv pv))
             then "; a plain function providing a method takes the method's parameters, which exclude the receiver (D54)"
             else ""))
  end

(* translate.py's eta-expansion: a plain fn providing a method gets the
   receiver as its first argument and drops it (Core Moss deviation 4) *)
with wrapper (fuel : nat) (pv : gsym) {struct fuel} : M nat :=
  match fuel with 0 => deep | S _ => wrapper_cached pv end

(* D66: binding an abstract fn the context provides is sugar for binding a
   defined forwarder to it *)
with forwarder (fuel : nat) (tg : gsym) {struct fuel} : M gsym :=
  match fuel with 0 => deep | S f =>
  let! s := get in
  match nget (st_fwd s) (gs_id tg) with
  | Some w => sym w
  | None =>
      let ps := fn_params tg in
      let names := map (fun i => "%" ++ nat_to_string i) (seq 0 (List.length ps)) in
      let call := AECall [match gs_decl tg with ADFn _ n _ _ _ => n | _ => gs_name tg end] None
                         (map (fun n => AEPath [n] None) names) in
      let d := ADFn RPlain (gs_name tg) (combine names (map snd ps)) (fn_ret tg) (Some (ABlock [] (Some call))) in
      let! w := new_sym (gs_mod tg) (gs_name tg) KFn d (OForwarder (gs_id tg)) in
      upd (s_fwd (fun m => nset m (gs_id tg) w)) ;;
      let! wg := sym w in
      lower_body f wg ;;
      ret wg
  end end

(* lower.lower_fn: a defined function, under its own assumptions *)
with lower_body (fuel : nat) (g : gsym) {struct fuel} : M unit :=
  match fuel with 0 => deep | S f =>
  let! done := lowered (gs_id g) in
  if done then ret tt else
  match fn_body g with
  | None => ret tt
  | Some b =>
      let mi := gs_mod g in
      let! info := fn_info g in
      let e := fi_env info in
      let! this := match gs_recv g with
                   | Some r =>
                       let! rg := sym r in
                       if kind_eqb (gs_kind rg) KUnit || kind_eqb (gs_kind rg) KTag then
                         let! xs := nominal_args mi rg (tymap e) in ret (Some (YNom r xs))
                       else err mi ("attached method `" ++ gs_name g ++ "` requires a nominal receiver (Q5); `" ++
                                    gs_name rg ++ "` is abstract")
                   | None => ret None
                   end in
      let! sg := fn_sig g (tymap e) e this in
      let e := fold_left (fun e p => set_local e (fst p) (snd p) LLet) (sg_params sg) e in
      let vars := (match this with Some _ => [("this", 0)] | None => [] end) in
      let vars := vars +++ combine (map fst (sg_params sg)) (seq (List.length vars) (List.length (sg_params sg))) in
      (* parameters are numbered by name: no two may share one (ExecutionModel note 43) *)
      let fix dup (l : list string) : option string :=
        match l with [] => None | x :: r => if smem x r then Some x else dup r end in
      match dup (map fst vars) with
      | Some n => err mi ("parameter `" ++ n ++ "` of `" ++ gs_name g ++ "` is declared twice")
      | None =>
      let c := Cx mi sg this 0 [] "" [] [] in
      emit_fn (gs_id g) XUnit ;;   (* a placeholder, so a recursive reference finds it lowered *)
      register_reify_code (ROFunction (gs_id g)) e (fi_needs info) (Some sg) ;;
      let! (body, _, _) := in_reify_code (ROFunction (gs_id g))
        (with_vars vars (synth_block f c e b (Some (sg_ret sg)))) in
      (match this with Some t => layout_ok e t | None => ret tt end) ;;
      miter (fun p => layout_ok e (snd p)) (sg_params sg) ;;
      layout_ok e (sg_ret sg) ;;
      miter (fun k => match k with
                      | KyVal v => match nget (en_vals e) v with Some t => layout_ok e t | None => ret tt end
                      | _ => ret tt end) (fi_needs info) ;;
      emit_fn (gs_id g) body
      end
  end end

(* D70: an anonymous function at a bind: captured locals become vals bound
   here, and the function a fresh defined one assuming everything here *)
with lambda_provider (fuel : nat) (c : cx) (e : env) (ps : list (name * option aty)) (rt : option aty)
    (body : aexpr) (bound : gsym) (sg : fsig) (this : option ety) (what : string) {struct fuel}
    : M (nat * list xstmt * smap) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  if negb (Nat.eqb (List.length ps) (List.length (sg_params sg))) then
    err mi ("the anonymous function bound to " ++ what ++ " takes " ++ nat_to_string (List.length ps) ++
            " parameter(s), but " ++ what ++ " takes " ++ nat_to_string (List.length (sg_params sg)) ++ " (D70)")
  else
  let names := map fst ps in
  let fix dup (l : list string) : option string :=
    match l with [] => None | x :: r => if smem x r then Some x else dup r end in
  match dup names with
  | Some n => err mi ("anonymous function parameter `" ++ n ++ "` is declared twice")
  | None =>
  miter (fun q => let '((pn, pt), (_, want)) := q in
                  match pt with
                  | Some t => let! aty1 := ety_of t mi (tymap e) e this in
                              if ety_eqb (canon e aty1) (canon e want) then ret tt
                              else let! a := show aty1 in let! b := show want in
                                   err mi ("parameter `" ++ pn ++ "` of the anonymous function bound to " ++ what ++
                                           " is annotated `" ++ a ++ "`, but " ++ what ++ " declares `" ++ b ++ "` there (D70)")
                  | None => ret tt
                  end) (combine ps (sg_params sg)) ;;
  (match rt with
   | Some t => let! aty1 := ety_of t mi (tymap e) e this in
               if ety_eqb (canon e aty1) (canon e (sg_ret sg)) then ret tt
               else let! a := show aty1 in let! b := show (sg_ret sg) in
                    err mi ("the anonymous function bound to " ++ what ++ " is annotated to return `" ++ a ++
                            "`, but " ++ what ++ " returns `" ++ b ++ "` (D70)")
   | None => ret tt
   end) ;;
  let outer := match cx_applying c with [] => en_locals e | _ => [] end in
  let named := free_names_e 10000 body in
  let captured := filter (fun q => smem (fst q) named && negb (smem (fst q) names)) outer in
  let! (le, pre, lets) := mfold (fun acc q =>
      let '(le, pre, lets) := acc in
      let '(n, (t, _)) := q in
      let! v := new_sym mi n KVal (ADVal n ATUnit None) OCaptured in
      let! st := fresh in
      let le := set_app (set_val le v t) (KyVal v) [] st in
      let! sv := local_var n in
      record_reify_event (REValue ety key v (canon e t) (reify_arguments e)) ;;
      ret (le, pre +++ [SBindVal v (XVar sv)], lets +++ [(n, v, t)])) (with_locals e [], [], []) captured in
  let needs := all_keys le in
  let! pid := new_sym mi (gs_name bound) KFn
                (ADFn RPlain (gs_name bound) [] None (Some (ABlock [] (Some body)))) OLambda in
  let lsig := FSig (combine names (map snd (sg_params sg))) (sg_ret sg) this in
  let le := fold_left (fun e p => set_local e (fst p) (snd p) LLet) (sg_params lsig) le in
  let le := fold_left (fun e q => let '(n, _, t) := q in set_local e n t LCaptured) lets le in
  let lc := Cx mi lsig this 0 (cx_applying c)
              "; an anonymous function bound to a fn has no receiver, and does not capture an enclosing one (D70)"
              [] (cx_pins c) in
  let vars := (match this with Some _ => [("this", 0)] | None => [] end) in
  let vars := vars +++ combine names (seq (List.length vars) (List.length names)) in
  check_lambda_local_names mi (map fst vars +++ map (fun q => let '(n, _, _) := q in n) lets) ;;
  register_reify_code (ROFunction pid) le needs (Some lsig) ;;
  let! fbody := in_reify_code (ROFunction pid) (with_vars vars (
      let! lx := mmap (fun q => let '(n, v, _) := q in let! x := local_var n in ret (SLet x (XVl v))) lets in
      let! (b, _, _) := synth_block f lc le (ABlock [] (Some body)) (Some (sg_ret sg)) in
      ret (fold_stmts lx b))) in
  emit_fn pid fbody ;;
  let info := FInfo le needs (map (fun k => (k, match get_app le k with Some (a, _) => a | None => [] end)) needs) [] in
  upd (s_info (fun m => nset m pid info)) ;;
  let! sm := smap_at le e (map (fun k => (k, k)) needs) in
  ret (pid, pre, sm)
  end end

(* ---- functors ---- *)
with apply_functor (fuel : nat) (c : cx) (e : env) (sp : spec) {struct fuel} : M (list xstmt * env * cx) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  match sp with
  | Spec (Some p) None None =>
      let! tg := rpath mi p in
      match tg with
      | None => err mi ("`" ++ show_path p ++ "` is not in scope")
      | Some (EMod _) => err mi ("`" ++ show_path p ++ "` is not a functor; a bind needs `= value`")
      | Some (ESym s) =>
          let! g := sym s in
          if negb (kind_eqb (gs_kind g) KFunctor) then
            err mi ("`" ++ gs_name g ++ "` is not a functor; a bind needs `= value`")
          else if nmem s (cx_applying c) then err mi ("functor `" ++ gs_name g ++ "` applies itself")
          else match functor_parts g with
               | (_, _, None) => apply_abstract f c e g
               | _ => functor_total 64 g ;; apply_body f c e g
               end
      end
  | _ => err mi "a functor application takes no method or bracket part"
  end end

(* D55: `bind F;` for F with a body runs F's binds here, written in F's module *)
with apply_body (fuel : nat) (c : cx) (e : env) (g : gsym) {struct fuel} : M (list xstmt * env * cx) :=
  match fuel with 0 => deep | S f =>
  if nmem (gs_id g) (cx_applying c) then err (cx_mod c) ("functor `" ++ gs_name g ++ "` applies itself") else
  let inner := cx_applying_in c g in
  match functor_parts g with
  | (_, _, Some binds) =>
      let! (xs, e', c') := mfold (fun acc it =>
          let '(xs, e, c1) := acc in
          let! (ys, e', c2) := match it with
                               | (sp, None) => apply_functor f c1 e sp
                               | (sp, Some y) => check_bind f c1 e sp y
                               end in
          ret (xs +++ ys, e', c2)) ([], e, inner) (List.concat binds) in
      ret (xs, e', cx_restore c c')
  | _ => ret ([], e, c)
  end end

(* D84 rule 5, `bind F;` for an abstract F: the core's EApply at θ, F's
   inputs read here; its result types fresh rigid generations, its result
   items provided at them *)
with apply_abstract (fuel : nat) (c : cx) (e : env) (fg : gsym) {struct fuel} : M (list xstmt * env * cx) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  if negb (is_some (nget (en_funs e) (gs_id fg))) then
    err mi ("`" ++ gs_name fg ++ "` is not available in the context here: nothing binds it, and nothing here assumes it (D78)")
  else
  let! (ef, needs) := fun_env fg in
  let argtys := map fst (en_tn ef) in
  (* m4, D84 rule 5 (fixed_ok): each x of X(F) denotes what it did where F
     was provided (bound, assumed, or the platform's root) *)
  let! xf := functor_fixed fg in
  let recorded := match get_app e (KyFun (gs_id fg)) with Some (a, _) => a | None => [] end in
  miter (fun x =>
           match assoc Nat.eqb x recorded, tn_get e x with
           | Some (Some was), Some now =>
               if ety_eqb (canon e was) (canon e now) then ret tt
               else let! xg := sym x in let! a := show (canon e now) in let! b := show (canon e was) in
                    err mi ("applying `" ++ gs_name fg ++ "` here reads `" ++ gs_name xg ++ "` as `" ++ a ++
                            "`, but `" ++ gs_name fg ++ "` is fixed in it and was provided where it was `" ++ b ++
                            "` (D84 rule 5, D72)")
           | _, _ => ret tt
           end) xf ;;
  miter (fun t => if is_some (tn_get e t) then ret tt
                  else let! tgg := sym t in
                       err mi ("applying `" ++ gs_name fg ++ "` needs `" ++ gs_name tgg ++ "`, which is not available in the context here")) argtys ;;
  let info := FInfo ef needs
    (map (fun k => (k, match get_app ef k with Some (a, _) => a | None => [] end)) needs)
    (merge_edges ef) in
  let! pairs := require_needs mi e fg info in
  let! rts := result_types fg in
  let! gens := mmap (fun t => match tn_get e t with
                              | None => ret (t, t)
                              | Some _ => let! tgg := sym t in
                                          let! gnew := new_sym (gs_mod tgg) (gs_name tgg ++ "@" ++ gs_name fg) KType
                                                               (ADType (gs_name tgg)) (OFresh t (gs_id fg)) in
                                          ret (t, gnew)
                              end) rts in
  let! th := mmap (fun t => let! y := ety_ty (canon e (opt_default YUnit (tn_get e t))) in ret (t, y)) argtys in
  let! th2 := mmap (fun p => ret (fst p, TAbs (snd p))) (filter (fun p => negb (Nat.eqb (fst p) (snd p))) gens) in
  let! sm := smap_at ef e pairs in
  let! (e', output_keys) := provide_result e fg gens in
  record_reify_event (REApply ety key (gs_id fg) needs output_keys
    (map (fun t => (t, canon e (opt_default YUnit (tn_get e t)))) argtys)
    (map (fun p => (fst p, YAbs (snd p))) gens) (reify_scope_of e')) ;;
  (* A new output shadows its old input meaning. Substitution lookup takes
     the first matching key, so fresh outputs must precede input readings. *)
  ret ([SApply (gs_id fg) (th2 +++ th) sm], e', cx_with_fresh c (cx_fresh c +++ map snd gens))
  end

(* D84 rule 3, `bind F = G;`, and rule 4, `bind F = H;` *)
with bind_functor (fuel : nat) (c : cx) (e : env) (fg : gsym) (y : aexpr) {struct fuel} : M (list xstmt * env) :=
  match fuel with 0 => deep | S f =>
  let mi := cx_mod c in
  match functor_parts fg with
  | (_, _, Some _) =>
      err mi ("`" ++ gs_name fg ++ "` has a body, so it is applied (`bind " ++ gs_name fg ++
              ";`), not bound; only an abstract functor is bound (D78)")
  | (fargs, fres, None) =>
  match y with
  | AEPath p None =>
      let! bt := resolve_sym mi p in
      match bt with
      | None => err mi ("`" ++ show_path p ++ "` is not in scope")
      | Some bg =>
          if negb (kind_eqb (gs_kind bg) KFunctor) then
            err mi ("`" ++ gs_name bg ++ "` is not a functor, so it cannot be bound to `" ++ gs_name fg ++ "`")
          else
          let '(bargs, bres, bbody) := functor_parts bg in
          (if negb (is_some bbody) && negb (is_some (nget (en_funs e) (gs_id bg))) then
             err mi ("`" ++ gs_name bg ++ "` is not available in the context here, so it cannot provide a bind (assume or bind it)")
           else ret tt) ;;
          (* lower.check_binding: the result exactly F's (D27) *)
          let! f_out := signature_items (gs_mod fg) fres in
          let! g_out := signature_items (gs_mod bg) bres in
          let missing := filter (fun q => negb (existsb (fun r => leaf_eqb (fst q) (fst r)) g_out)) f_out in
          let more := filter (fun q => negb (existsb (fun r => leaf_eqb (fst q) (fst r)) f_out)) g_out in
          (match missing, more with
           | _ :: _, _ => err mi ("`" ++ gs_name bg ++ "` cannot be bound to `" ++ gs_name fg ++ "`: it does not provide " ++
                                  String.concat ", " (map snd missing) ++ ", which `" ++ gs_name fg ++ "` promises (D78)")
           | [], _ :: _ => err mi ("`" ++ gs_name bg ++ "` cannot be bound to `" ++ gs_name fg ++ "`: it also provides " ++
                                   String.concat ", " (map snd more) ++ ", which `" ++ gs_name fg ++
                                   "` does not; a functor bound to an abstract one provides exactly its result (D78)")
           | [], [] => ret tt
           end) ;;
          let! (ef, fneeds) := fun_env fg in
          let! (eg, gneeds) := fun_env bg in
          let! target_fixed := functor_fixed fg in
          bind_fixed_present mi e fg target_fixed ;;
          let! stamp := fresh in
          match bbody with
          | None =>
              (* rule 4: H's inputs are among F's *)
              miter (fun k => if key_mem k fneeds then ret tt
                              else let! s := show_key k in
                                   err mi ("`" ++ gs_name bg ++ "` cannot be bound to `" ++ gs_name fg ++ "`: it needs " ++ s ++
                                           ", which `" ++ gs_name fg ++ "` does not take (D78)")) gneeds ;;
              (* m4, D84 rule 4: X(H) ⊆ X(F) (P(F) ⊆ P(H)) *)
              let! xh := functor_fixed bg in
              copy_fixed_current mi e bg xh ;;
              let! xf := functor_fixed fg in
              miter (fun x => if nmem x xf then ret tt
                              else let! xg := sym x in
                                   err mi ("`" ++ gs_name bg ++ "` cannot be bound to `" ++ gs_name fg ++ "`: it is fixed in `" ++
                                           gs_name xg ++ "`, in which `" ++ gs_name fg ++ "` is polymorphic (D84 rule 4)")) xh ;;
              (* An identity copy preserves its existing latent interface.
                 Do not force its outputs to be formable before application. *)
              let! copy_view := if Nat.eqb (gs_id fg) (gs_id bg) then ret None
                                else let! view := copy_output_compatible mi e ef eg fg bg xf fneeds gneeds in
                                     ret (Some view) in
              let! sm := smap_at eg ef (map (fun k => (k, k)) gneeds) in
              let e' := set_fun e (gs_id fg) stamp in
              let! s2 := fresh in
              let e' := set_app e' (KyFun (gs_id fg)) (map (fun x => (x, option_map (canon e) (tn_get e x))) xf) s2 in
              record_reify_event (REFunctor ety key (gs_id fg) (gs_id bg) gneeds (reify_arguments e) copy_view) ;;
              ret ([SBindFunA (gs_id fg) (gs_id bg) sm], e')
          | Some _ =>
              (* rule B for a closure: G's type inputs that F does not take
                 are captured here, each read as itself, so only one in
                 force here has a principal captured meaning *)
              let gtys := map fst (en_tn eg) in
              let ftys := map fst (en_tn ef) in
              miter (fun t => if nmem t ftys || is_some (tn_get e t) then ret tt else
                              let! tg := sym t in
                              err mi ("binding `" ++ gs_name fg ++ "` to `" ++ gs_name bg ++ "` captures `" ++ gs_name tg ++
                                      "`, which is not in force here (assume or bind it)")) gtys ;;
              defined_link_inputs_compatible mi e ef eg fg bg target_fixed fneeds gneeds ;;
              functor_total 64 bg ;;
              let! h := link_body f fg bg in
              let linked := filter (fun k => key_mem k fneeds) gneeds in
              let captured := filter (fun k => negb (key_mem k fneeds)) gneeds in
              let site k := match k with KyMth r m => KyMth (canon e (instantiate e r)) m | _ => k end in
              miter (fun k => if key_present e (site k) then ret tt
                              else let! s := show_key (site k) in
                                   err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, captures " ++ s ++
                                           ", which is not available here (D84 3b)")) captured ;;
              miter (fun k => check_captured_reading mi e eg k (site k)) captured ;;
              let! sm := smap_at eg ef (map (fun k => (k, k)) linked) in
              let! smc := smap_at eg e (map (fun k => (k, site k)) captured) in
              (* m4, D84 rule 7: a family's binding binds no val and applies no
                 functor but a family *)
              let! fam := is_family fg in
              let bmi := gs_mod bg in
              let gbinds := List.concat (opt_default [] bbody) in
              (if fam then
                 miter (fun it =>
                   match it with
                   | (Spec (Some p) None None, None) =>
                       let! ao := resolve_sym bmi p in
                       match ao with
                       | Some ag => let! af := is_family ag in
                                    if af then ret tt
                                    else err mi ("`" ++ gs_name bg ++ "`, bound to the family `" ++ gs_name fg ++ "`, applies `" ++
                                                 gs_name ag ++ "`, which is not a family, so where an application is placed could be observed (D84 rule 7)")
                       | None => ret tt
                       end
                   | (Spec (Some p) None _, Some _) =>
                       let! vo := resolve_sym bmi p in
                       match vo with
                       | Some vg => if kind_eqb (gs_kind vg) KVal then
                                      err mi ("`" ++ gs_name bg ++ "`, bound to the family `" ++ gs_name fg ++ "`, binds the val `" ++
                                              gs_name vg ++ "`, so where an application is placed could be observed (D84 rule 7)")
                                    else ret tt
                       | None => ret tt
                       end
                   | _ => ret tt
                   end) gbinds
               else ret tt) ;;
              (* m4, D84 3d: after G's binds each x of X(F) denotes what it does
                 here; a pin mentioning a type applications supply is refused *)
              let! xf := functor_fixed fg in
              let fargtys := filter (fun t => negb (nmem t xf)) (map fst (en_tn ef)) in
              let! _ := mfold (fun ep it =>
                  match it with
                  | (Spec (Some p) None None, Some (AEPath rp ra)) =>
                      let! xo := resolve_sym bmi p in
                      match xo with
                      | Some xg =>
                          if negb (kind_eqb (gs_kind xg) KType) || negb (nmem (gs_id xg) xf) then ret ep else
                          let! fs := free_type_syms 200 (ATRef rp ra) bmi [] in
                          (if true then
                             match filter (fun t => nmem t fargtys && negb (is_some (nassoc t (en_tn ep)) && false)) fs with
                             | t :: _ =>
                                 let! tg := sym t in
                                 err mi ("`" ++ gs_name bg ++ "` cannot be bound to `" ++ gs_name fg ++ "` here: it pins `" ++ gs_name xg ++
                                         "`, which `" ++ gs_name fg ++ "` is fixed in, to a type that mentions `" ++ gs_name tg ++
                                         "`, a type each application of `" ++ gs_name fg ++ "` supplies (D84 3d)")
                             | [] =>
                                 let! y := ety_of (ATRef rp ra) bmi (tymap ep) ep None in
                                 let want := canon e (opt_default (YAbs (gs_id xg)) (tn_get e (gs_id xg))) in
                                 if ety_eqb (canon ep y) want then ret tt
                                 else let! a := show (canon ep y) in let! b := show want in
                                      err mi ("`" ++ gs_name bg ++ "` cannot be bound to `" ++ gs_name fg ++ "` here: it binds `" ++
                                              gs_name xg ++ "` to `" ++ a ++ "`, and here `" ++ gs_name xg ++ "` is `" ++ b ++
                                              "`; `" ++ gs_name fg ++ "` is fixed in `" ++ gs_name xg ++ "` (D84)")
                             end
                           else ret tt) ;;
                          let! y := ety_of (ATRef rp ra) bmi (tymap ep) ep None in
                          let! st := fresh in ret (set_tn ep (gs_id xg) (canon ep y) st)
                      | None => ret ep
                      end
                  | _ => ret ep
                  end) e gbinds in
              check_link_fixed_current mi e fg bg target_fixed ;;
              let e' := set_fun e (gs_id fg) stamp in
              let! s2 := fresh in
              let e' := set_app e' (KyFun (gs_id fg)) (map (fun x => (x, option_map (canon e) (tn_get e x))) xf) s2 in
              record_reify_event (REFunctor ety key (gs_id fg) h gneeds (reify_arguments e) None) ;;
              ret ([SBindFun (gs_id fg) h sm smc], e')
          end
      end
  | _ => err mi ("the right side of a bind of functor `" ++ gs_name fg ++ "` must name a functor")
  end
  end end

(* D84 rule 3c: G's binds, checked once, in G's own terms: the core's
   functor h whose arguments are G's inputs F takes (linked) and whose
   captured inputs are the rest *)
with link_body (fuel : nat) (fg bg : gsym) {struct fuel} : M nat :=
  match fuel with 0 => deep | S f =>
  let! s := get in
  match assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b)) (gs_id fg, gs_id bg) (st_links s) with
  | Some h => ret h
  | None =>
      let mi := gs_mod bg in
      let! (ef, fneeds) := fun_env fg in
      let! (eg, gneeds) := fun_env bg in
      (* m4, D84 rule 3b: the types applications supply are F's polymorphic
         ones, not X(F), which a binding is fixed in and may capture *)
      let! xf := functor_fixed fg in
      let argtys := filter (fun t => negb (nmem t xf)) (map fst (en_tn ef)) in
      let! rts := result_types fg in
      (* rule 3b: no captured item mentions a type applications supply *)
      miter (fun k =>
               let mentions := match k with
                               | KyMth r m => ety_gens r +++
                                              match key_sig eg k with Some (ps, rt) => flat_map ety_gens (rt :: ps) | None => [] end
                               | KyVal v => match nget (en_vals eg) v with Some t => ety_gens (canon eg t) | None => [] end
                               | KyFn fn => match key_sig eg k with Some (ps, rt) => flat_map ety_gens (rt :: ps) | None => [] end
                               | KyFun _ => match get_app eg k with
                                   | Some (captured,_) => flat_map (fun p => match snd p with
                                       | Some ty => ety_gens (canon eg ty) | None => [] end) captured
                                   | None => [] end
                               end in
               match k with
                      | KyMth _ _ | KyVal _ | KyFn _ | KyFun _ =>
                          if negb (key_mem k fneeds) && existsb (fun g => nmem g argtys) mentions then
                            let! sk := show_key k in
                            err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, captures " ++ sk ++
                                    ", which mentions a type each application of `" ++ gs_name fg ++ "` supplies (D84 3b)")
                          else ret tt
                      end) gneeds ;;
      (* m4: G may take a type F makes among its inputs as long as it binds
         it (it must make it, D78: checked below, as lower.functor_closure
         checks `made_here`); tests/wasi/arena.moss's InScratch takes Wasm
         and binds Memory *)
      (* F's result in F's own terms, its types as themselves *)
      let! (ef', dr, output_roles) := fresh_target_output_view ef fg in
      let mf := filter (fun g => negb (nmem g (rts +++ map snd output_roles)))
                       (flat_map (fun k => match k with KyMth r _ => ety_gens r | _ => [] end) dr
                        +++ flat_map (fun k => match key_sig ef' k with
                                              | Some (ps, r) => flat_map ety_gens (r :: ps)
                                              | None => [] end) dr) in
      (* m4, D84 3d: pins may not change a type of P(F) ∩ M(F); X(F) may be
         pinned (a manifest equation, checked at the bind) *)
      let c := Cx mi (FSig [] YUnit None) None 0 [] "" [] (filter (fun t => negb (nmem t xf)) mf) in
      let owner := ROLink (gs_id fg) (gs_id bg) in
      register_reify_code owner eg gneeds None ;;
      let! (xs, eb, _) := in_reify_code owner (with_vars [] (apply_body f c eg bg)) in
      check_link_pins mi eb (cx_pins c) ;;
      let! binds := mmap (fun x => match stmt_bind x with
                                   | Some b => ret b
                                   | None => fail "internal: a functor body made a statement"
                                   end) xs in
      check_provider_effects mi binds ;;
      (* where the body bound a result type, a result method item's key reads
         through it; the result is filed under the key as F declares it *)
      (* m4: and X(F) as the body pinned it (D84 3d: a pin is a manifest
         equation, so F's promise is read through it) *)
      let made_tys := map (fun p => (snd p,canon eb (opt_default (YAbs (fst p)) (tn_get eb (fst p))))) output_roles +++
                      map (fun t => (t,canon eb (opt_default (YAbs t) (tn_get eb t)))) xf in
      let body_key k := match k with KyMth r m =>
        KyMth (canon eb (instantiate (env_with_edges made_tys []) r)) m | _ => k end in
      (* a result method item is filed under its key as F declares it (its
         receiver and θ in F's terms) besides the body's own key *)
      let! renames := mmap (fun k => match k, body_key k with
                                     | KyMth r m, KyMth r' _ =>
                                         let! t' := ety_ty r' in
                                         let! checked := key_item_at ef' k in
                                         let it := output_role_item output_roles checked in
                                         match it with
                                         | IMth t0 _ th0 => ret [(t', m, (t0, th0))]
                                         | _ => ret []
                                         end
                                     | _, _ => ret []
                                     end) dr in
      let renames := List.concat renames in
      let binds := flat_map (fun b => match b with
                                      | XBMth t m th f' sm =>
                                          b :: flat_map (fun q => let '(t', m', (t0, th0)) := q in
                                                                  if ty_eqb t t' && Nat.eqb m m' &&
                                                                     negb (item_eqb (IMth t0 m th0) (IMth t m th))
                                                                  then [XBMth t0 m th0 f' sm] else [])
                                                        renames
                                      | _ => [b]
                                      end) binds in
      (* m4, D78: G makes each type F's result holds (lower.functor_closure's
         `made_here`) *)
      miter (fun t =>
               let made := match nassoc t (en_tn eb), nassoc t (en_tn eg) with
                           | Some (_, s1), Some (_, s0) => negb (Nat.eqb s1 s0)
                           | Some _, None => true
                           | None, _ => false
                           end in
               if made then ret tt
               else let! tg := sym t in
                    err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, does not provide the type `" ++ gs_name tg ++ "` (D78)")) rts ;;
      (* rule 3e: it makes F's result, item by item, at the types F promises *)
      miter (fun k0 =>
               let k := body_key k0 in
               let made := match get_app eb k, get_app eg k with
                           | Some (_, s1), Some (_, s0) => negb (Nat.eqb s1 s0)
                           | Some _, None => true
                           | _, _ => match k with KyFun u => is_some (nget (en_funs eb) u) | _ => false end
                           end in
               let! sk := show_key k0 in
               if negb made then
                 err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, does not provide " ++ sk ++ " (D78)")
               else ret tt) dr ;;
      miter (fun k =>
               (match k with
                | KyFun _ =>
                    if result_functor_fixed_ok eb ef' made_tys (body_key k) k then ret tt
                    else let! sk := show_key k in
                         err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, provides " ++ sk ++
                                 " at another fixed interpretation than promised (D78)")
                | _ => ret tt end) ;;
               match key_sig eb (body_key k), key_sig ef' k with
               | Some (p1, r1), Some (p2, r2) =>
                   if Nat.eqb (List.length p1) (List.length p2) &&
                      forallb (fun q => ety_eqb (canon eb (fst q)) (promised_result_type eb made_tys (snd q)))
                              (combine (r1 :: p1) (r2 :: p2))
                   then ret tt
                   else let! sk := show_key k in
                        err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, provides " ++ sk ++
                                " at another type than `" ++ gs_name fg ++ "` promises (D78)")
               | _, _ => ret tt
               end) dr ;;
      record_reify_exports_at owner rts dr ef ef' eb output_roles ;;
      let! h := new_sym mi (gs_name bg) KFunctor (gs_decl bg) (OLink (gs_id fg) (gs_id bg)) in
      let! dri := mmap (fun k => let! it := key_item_at ef' k in ret (output_role_item output_roles it)) dr in
      upd (s_funs (fun l => (h, (map ITy rts +++ dri, binds)) :: l)) ;;
      upd (s_links (fun l => ((gs_id fg, gs_id bg), h) :: l)) ;;
      ret h
  end end.

Definition ELAB_FUEL : nat := 100000.

(* ===================== 14. The platform (translate.py's `entry`) ========= *)

(* The name table (which library fn is which primitive), prim_of_name, is
   audit-level: ExecutionModel.v, Part IV. *)

(* ===================== 15. The program =================================== *)

(* xprogram, the elaborator's output, is audit-level: ExecutionModel.v, Part II. *)

(* Validate the binding as main reads it, after its own context merges. *)
Definition check_platform_entry_view (e : env) (k : key) (entry : xdentry) : M unit :=
  match entry with
  | XDVal (XVI32 _) =>
      let! sc := runtime_symbols in
      match key_sig e k with
      | Some ([], t) => if runtime_abs sc "I32" (canon e t) then ret tt
                        else fail "platform I32 constant has an incompatible assumed type"
      | _ => fail "platform I32 constant has no assumed type"
      end
  | XDPrim p =>
      match key_sig e k with
      | Some (ps, r) => check_runtime_primitive 0 e p ps r
      | None => fail "platform primitive has no assumed signature"
      end
  | XDFun _ _ _ =>
      let! sc := runtime_symbols in
      let fixed := match get_app e k with Some (a, _) => a | None => [] end in
      if forallb (fun name => match runtime_id sc "wasm" name with
          | Some t => match assoc Nat.eqb t fixed with
              | Some (Some y) => runtime_abs sc name (canon e y)
              | Some None => false
              | None => true end
          | None => true end) runtime_machine_names
      then ret tt else fail "platform functor has an incompatible assumed fixed type"
  | _ => ret tt
  end.

Definition platform_entry (k : key) (prims : list string) : M (option (xdentry * list string)) :=
  let! wm := lang "wasm" in
  let! pm := lang "wasip1" in
  let! sm := lang "wasistd" in
  let plat mi := existsb (fun o => match o with Some x => Nat.eqb x mi | None => false end) [wm; pm; sm] in
  match k with
  | KyVal v => let! g := sym v in
               if plat (gs_mod g) then
                 match gs_name g with
                 | "i32_zero" | "i32_one" =>
                     let! info := fn_info g in
                     match gs_decl g with
                     | ADVal _ ty0 None =>
                         let e := fi_env info in
                         let! ty0 := ety_of ty0 (gs_mod g) (tymap e) e None in
                         let! sc := runtime_symbols in
                         if runtime_abs sc "I32" (canon e ty0) then
                           ret (Some (XDVal (XVI32 (if String.eqb (gs_name g) "i32_zero" then 0 else 1)%uint63), prims))
                         else err (gs_mod g) "platform I32 constant has an incompatible declared type"
                     | _ => err (gs_mod g) "platform I32 constant must be an abstract value"
                     end
                 | _ => ret None
                 end
               else ret None
  | KyFn fn => let! g := sym fn in
               if plat (gs_mod g) then
                 match prim_of_name (gs_name g) with
                 | Some p =>
                     let! info := fn_info g in
                     let e := fi_env info in
                     let! sg := fn_sig g (tymap e) e None in
                     check_runtime_primitive (gs_mod g) e p (map snd (sg_params sg)) (sg_ret sg) ;;
                     ret (Some (XDPrim p, prims))
                 | None => ret (Some (XDPrim (PUnsupported (List.length prims)), prims +++ [gs_name g]))
                 end
               else ret None
  | KyFun u => let! g := sym u in
               match functor_parts g with
               | (_, _, None) => if plat (gs_mod g) then
                                   let! h := platform_fun g in
                                   ret (Some (XDFun h [] [], prims))
                                 else ret None
               | _ => ret None
               end
  | KyMth _ _ => ret None
  end.

Definition module_function_ids (m : gmod) : list nat :=
  map snd(gm_names m) +++ map snd(gm_detached m) +++ map snd(gm_attached m).

Definition lower_all : M unit :=
  let! s := get in
  let w := st_w s in
  miter (fun mi =>
           let m := get_mod w mi in
           miter (fun sid => let! g := sym sid in
                             if is_some (fn_body g) && (kind_eqb (gs_kind g) KFn || kind_eqb (gs_kind g) KMethod)
                             then lower_body ELAB_FUEL g else ret tt)
                 (module_function_ids m)) (w_order w).

(* D73, of every tag, used or not: what it lists are types its assume
   nesting requires (ExecutionModel note 40; solve_requirements checks it only of
   the tags elaboration reaches) *)
Definition check_listed : M unit :=
  let! s := get in
  let w := st_w s in
  miter (fun i => match nget (w_syms w) i with
                  | Some g => if kind_eqb (gs_kind g) KTag then let! _ := listed_params 0 g in ret tt else ret tt
                  | None => ret tt
                  end) (seq 0 (w_nsyms w)).

(* Reject a cycle even when no body ever uses the context. This check
   reads the declaration graph only; it does not elaborate its items. *)
Fixpoint context_acyclic (fuel : nat) (w : world) (stack : list nat) (g : gsym) : res unit :=
  if nmem (gs_id g) stack then
    err_in (get_mod w (gs_mod g)) ("context `" ++ gs_name g ++ "` nests in a cycle")
  else match fuel with
  | 0 => err_in (get_mod w (gs_mod g)) "contexts nest in a cycle"
  | S f =>
      match gs_kind g, gs_decl g with
      | KCtx, ADCtx _ its =>
          let fix go (specs : list spec) : res unit :=
            match specs with
            | [] => Good tt
            | Spec (Some p) None _ :: rest =>
                let* target := resolve_path w (gs_mod g) p in
                let* _ := match target with
                          | Some (ESym x) =>
                              match nget (w_syms w) x with
                              | Some h => if kind_eqb (gs_kind h) KCtx
                                          then context_acyclic f w (gs_id g :: stack) h
                                          else Good tt
                              | None => Good tt
                              end
                          | _ => Good tt
                          end in go rest
            | _ :: rest => go rest
            end in go its
      | _, _ => Good tt
      end
  end.

Definition check_contexts : M unit :=
  let! s := get in
  let w := st_w s in
  lift (let fix go (ids : list nat) : res unit :=
          match ids with
          | [] => Good tt
          | x :: rest =>
              let* _ := match nget (w_syms w) x with
                        | Some g => context_acyclic (S (w_nsyms w)) w [] g
                        | None => Good tt
                        end in go rest
          end in go (seq 0 (w_nsyms w))).

(* Generated providers retain their own formal type-input domain. Abstract
   copies can have extra inputs which must not replace captured meanings. *)
Definition runtime_provider_inputs (h : usym) : M (list tsym) :=
  let! g := sym h in
  match gs_origin g with
  | OPlatformFun u =>
      let! fg := sym u in let! (ef, _) := fun_env fg in ret (map fst (en_tn ef))
  | OLink u v =>
      let! fg := sym u in let! bg := sym v in
      let! (ef, _) := fun_env fg in let! (eg, _) := fun_env bg in
      ret (filter (fun t => nmem t (map fst (en_tn ef))) (map fst (en_tn eg)))
  | _ => ret []
  end.

Definition elab_program : M xprogram :=
  check_listed ;;
  check_contexts ;;
  check_runtime_library ;;
  lower_all ;;
  check_assumes ;;
  let! s := get in
  let w := st_w s in
  let entry := get_mod w (w_entry w) in
  match sassoc "main" (gm_names entry) with
  | None => fail "unsupported: no main"
  | Some msym =>
      let! mg := sym msym in
      if negb (is_defined_fn mg) then fail "unsupported: no main" else
      let! info := fn_info mg in
      check_main_signature mg info ;;
      let! (env0, prims) := mfold (fun acc k =>
          let '(env0, prims) := acc in
          let! o := platform_entry k prims in
          match o with
          | Some (en, prims') => check_platform_entry_view (fi_env info) k en ;;
              let! it := key_item_at (fi_env info) k in ret (env0 +++ [(it, en)], prims')
          | None => let! sk := show_key k in
                    fail ("unsupported: main assumes " ++ sk ++ ", which only the bootstrap's native Std provides")
          end) ([], []) (fi_needs info) in
      let! s := get in
      (if st_memory_bound s && st_memory_used s then
         fail "unsupported: D79: a memory instruction under a rebound `Memory`" else ret tt) ;;
      (match st_unsup s with
       | why :: _ => fail ("unsupported: " ++ why)
       | [] => ret tt
       end) ;;
      let! sc := runtime_symbols in
      let '(b, t, fl) := runtime_bool_tuple sc in
      let! input_types := mmap (fun p => let! ts := runtime_provider_inputs (fst p) in ret (fst p, ts)) (st_funs s) in
      let! s := get in
      let machines := runtime_machines sc (st_next s) in
      let! main_types := mmap (fun q => let! tau := ety_ty (canon (fi_env info) (snd q)) in
                                        ret (fst q, ExecutionModel.app_subst machines tau)) (tymap (fi_env info)) in
      ret (XProgram (st_fns s) (st_funs s) b t fl msym
             (xinst [] main_types +++ env0) prims (runtime_casts sc (st_next s)) input_types [] false [] [])
  end.

Definition lang_modules (w : world) : list (string * nat) :=
  flat_map (fun mi =>
              let p := gm_path (get_mod w mi) in
              flat_map (fun short =>
                if match w_library w with
                   | Some dir => String.eqb p (dir ++ short ++ ".moss")
                   | None => false end then [(short, mi)] else [])
                       ["num"; "int"; "float"; "bool"; "wasm"; "wasip1"; "wasistd"; "access"])
           (w_order w).

(* The elaborator: a total function from a bundle to a program or an error. *)
Definition elaborate (b : bundle) : res xprogram :=
  let* w := load_world b in
  let s0 := St w (w_syms w) (w_nsyms w) NE NE NE [] [] NE [] [] NE NE [] 0 false false [] (lang_modules w) [] (empty_reify_metadata ety key) in
  match elab_program s0 with
  | Good (p, _) => Good p
  | Bad m => Bad m
  end.

(* The staged runtime pipeline retains checked source metadata.  The old
   [elaborate] is deliberately the raw source translation for Track A. *)
Definition elaborate_with_metadata (b : bundle) : res (xprogram * st) :=
  let* w := load_world b in
  let s0 := St w (w_syms w) (w_nsyms w) NE NE NE [] [] NE [] [] NE NE [] 0
    false false [] (lang_modules w) [] (empty_reify_metadata ety key) in
  elab_program s0.

Lemma elaborate_with_metadata_project : forall b,
    elaborate b =
    match elaborate_with_metadata b with
    | Good (p, _) => Good p
    | Bad message => Bad message
    end.
Proof.
  intro b. unfold elaborate, elaborate_with_metadata, rbind.
  destruct (load_world b); reflexivity.
Qed.

(* From the CLI's tokens. *)
Definition elaborate_sx (x : sx) : res xprogram :=
  let* b := read_bundle x in elaborate b.
