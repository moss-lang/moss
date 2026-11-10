(* Source declarations, imports and name resolution for a loaded bundle. *)
(* This is the executable loader shared by the Audit world construction  *)
(* and its existing loader soundness proofs.                             *)

From Stdlib Require Import List String Ascii Bool Arith.
From NM Require Import NMap Sx.
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
  gs_assumes : list aitem;
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
  ds_pending : list (adecl * list aitem)
}.

Definition fresh_sym (st : decl_st) (mi : nat) (n : string) (k : kind) (d : adecl)
    (rcv : option nat) (asm : list aitem) : nat * decl_st :=
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

Fixpoint declare_walk (fuel : nat) (mi : nat) (st : decl_st) (asm : list aitem)
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

Fixpoint declare_attached (mi : nat) (st : decl_st) (ps : list (adecl * list aitem))
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

(* Assume nesting as context specifications, including equation sides. *)
Definition assume_specs (its : list aitem) : list spec := flat_map aitem_specs its.
