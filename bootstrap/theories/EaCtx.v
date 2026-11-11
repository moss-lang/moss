(* ------------------------------------------------------------------------ *)
(* EaCtx.v — context formation (Audit V.3): ItemEntries, RootCtx, merging   *)
(* (MostGeneralMerge), CtxOf, FunCtx, and the w_context table.             *)
(*                                                                           *)
(* Merging is first-order unification in rounds: entries filed under one   *)
(* key (read under the current σ) have their signatures and (Δ4) their    *)
(* instantiations unified; keys are                                         *)
(* re-read and the round repeats until nothing changes (at most one round  *)
(* per generation, plus one).  The orientation of a merge of two           *)
(* generations is the strategy's (s_orient).  The result is validated:     *)
(* idempotent, a function, and Unifies.                                    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap EaBase EaTypes.
From NM Require PublicMergePresent.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Section Ctx.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.

(* ---- ItemEntries ---- *)

(* SOUND: item_entries n p ob tm sp = Ok (es, tm') -> ItemEntries W p ob tm sp es tm'
          items_entries n p ob tm sps = Ok (es, tm') -> ItemsEntries W p ob tm sps es tm' *)
Fixpoint item_entries (n : nat) (stk : list nat) (p : string) (ob tm : list (nat * ty)) (sp : spec) {struct n}
  : er (list entry * list (nat * ty)) :=
  match n with
  | 0 => Err "context: too deep (cyclic context?)"
  | S n' =>
      match sp with
      | Spec (Some pa) (Some dn) a =>
          let! r := ropt (resolve_sym W p pa) ("`" ++ show_path pa ++ "` is not in scope") in
          let! m := ropt (method_named W p r dn) ("no method `." ++ dn ++ "` for `" ++ show_path pa ++ "` is in scope") in
          let! bs := brackets W fuel p (ob ++ tm) m a in
          let tm2 := bs ++ ob ++ tm in
          let! rt := recv_ty W fuel r tm2 in
          let! g := sig_of W fuel m tm2 (Some rt) in
          Ok ([NMth rt m g (app_of W metadata_fuel m tm2)], tm)
      | Spec (Some pa) None a =>
          let! s := ropt (resolve_sym W p pa) ("`" ++ show_path pa ++ "` is not in scope") in
          let! d := decl_of W s in
          let none_a := match a with None => true | Some _ => false end in
          match d_ast d with
          | ADType _ =>
              let! _ := rcheck none_a (bq (sym_name W s) ++ " takes no brackets in a context (D71)") in
              Ok ([NTy s], match nlook s tm with Some _ => tm | None => (s, TAbs s) :: tm end)
          | ADUnit _ | ADTag _ _ _ | ADAlias _ _ | ADVal _ _ (Some _) =>
              let! _ := rcheck none_a (bq (sym_name W s) ++ " takes no brackets in a context (D71)") in Ok ([], tm)
          | ADVal _ t None =>
              let! bs := brackets W fuel p (ob ++ tm) s a in
              let tm2 := bs ++ ob ++ tm in
              let! ty0 := ty_of W fuel (d_mod d) tm2 None t in
              Ok ([NVal s ty0 (app_of W metadata_fuel s tm2)], tm)
          | ADFn RPlain _ _ _ _ =>
              let! bs := brackets W fuel p (ob ++ tm) s a in
              let tm2 := bs ++ ob ++ tm in
              let! g := sig_of W fuel s tm2 None in
              Ok ([NFn s g (app_of W metadata_fuel s tm2)], tm)
          | ADFunctor _ _ _ _ None =>
              let! _ := rcheck none_a (bq (sym_name W s) ++ " takes no brackets in a context (D71)") in
              let fx := fixed W metadata_fuel s in
              let! _ := match filter (fun x => negb (is_some (nlook x tm))) fx with
                        | [] => Ok tt
                        | absent => Err ("assuming " ++ bq (sym_name W s) ++ " needs `" ++
                                         String.concat "`, `" (map (sym_name W) absent) ++ "` in force: " ++
                                         bq (sym_name W s) ++ " is fixed in them (D84, D20)")
                        end in
              Ok ([NFun s (fixed_readings W metadata_fuel s tm)], tm)
          | ADCtx _ its =>
              (* a context on its own path has no finite derivation *)
              let! _ := rcheck (negb (nmem s stk)) ("context " ++ bq (sym_name W s) ++ " nests in a cycle") in
              let! bs := brackets W fuel p (ob ++ tm) s a in
              items_entries n' (s :: stk) (d_mod d) (bs ++ ob) tm its
          | _ => Err ("cannot assume " ++ bq (sym_name W s))
          end
      | _ => Err "an empty context item"
      end
  end
with items_entries (n : nat) (stk : list nat) (p : string) (ob tm : list (nat * ty)) (its : list citem) {struct n}
  : er (list entry * list (nat * ty)) :=
  match n with
  | 0 => Err "context: too deep"
  | S n' =>
      match its with
      | [] => Ok ([], tm)
      | CISpec sp :: r =>
          let! (es1, tm1) := item_entries n' stk p ob tm sp in
          let! (es2, tm2) := items_entries n' stk p ob tm1 r in
          Ok (es1 ++ es2, tm2)
      | CIEq l r :: rest =>
          (* [D90] (IsEq): the references the sides write, as unbracketed
             items, then both sides read, as one keyless entry *)
          let! (es1, tm1) := items_entries n' stk p ob tm (map CISpec (ty_heads l ++ ty_heads r)) in
          let! tl := ty_of W fuel p (ob ++ tm1) None l in
          let! tr := ty_of W fuel p (ob ++ tm1) None r in
          let! (es2, tm2) := items_entries n' stk p ob tm1 rest in
          Ok (es1 ++ NEq tl tr :: es2, tm2)
      end
  end.

(* ---- RootCtx (main's Gc functors, note 54) ---- *)

Definition has_nfun (u : nat) (es : list entry) : bool :=
  existsb (fun e => match e with NFun u' _ => Nat.eqb u u' | _ => false end) es.

(* SOUND: root_items q es0 tm its = Ok (es, tm') -> RootItems W q es0 tm its es tm' *)
Fixpoint root_items (q : string) (es0 : list entry) (tm : list (nat * ty)) (its : list citem)
  : er (list entry * list (nat * ty)) :=
  match its with
  | [] => Ok ([], tm)
  | sp :: rest =>
      let skip := let! (es, tm') := root_items q es0 tm rest in Ok (es, tm') in
      match sp with
      | CISpec (Spec (Some p) None _) =>
          match resolve_sym W q p with
          | Some u =>
              match w_decl W u with
              | Some du =>
                  match d_ast du with
                  | ADFunctor _ _ _ _ None =>
                      if has_nfun u es0 then skip else
                      let xs := fixed W metadata_fuel u in
                      let tm1 := add_tys xs tm in
                      let fx := fixed_readings W metadata_fuel u tm1 in
                      let! (es, tm') := root_items q (es0 ++ map NTy xs ++ [NFun u fx]) tm1 rest in
                      Ok (map NTy xs ++ [NFun u fx] ++ es, tm')
                  | ADFunctor _ _ _ _ (Some _) =>
                      if has_nfun u es0 then skip else Err ("the platform context lists " ++ bq (sym_name W u) ++ ", a functor with a body")
                  | _ => skip
                  end
              | None => skip
              end
          | None => skip
          end
      | _ => skip
      end
  end.

(* SOUND: root_ctx s es0 tm = Ok (er, tmr) -> RootCtx W s es0 tm er tmr *)
Definition root_ctx (s : nat) (es0 : list entry) (tm : list (nat * ty)) : er (list entry * list (nat * ty)) :=
  if nmem s (e_mains E) then
    match lang_item_sym W LIRoot with
    | Some gc =>
        match w_decl W gc with
        | Some d => match d_ast d with
                    | ADCtx _ its => root_items (d_mod d) es0 tm its
                    | _ => Ok ([], tm) end
        | None => Ok ([], tm)
        end
    | None => Ok ([], tm)
    end
  else Ok ([], tm).

(* ---- Merging ---- *)

Fixpoint walk (n : nat) (s : subst) (t : ty) : ty :=
  match n with
  | 0 => t
  | S n' => match t with
            | TAbs g => match lookup_subst g s with Some t' => walk n' s t' | None => t end
            | _ => t end
  end.
(* the full reading of t under a triangular s *)
Fixpoint resolve (n : nat) (s : subst) (t : ty) {struct n} : ty :=
  match n with
  | 0 => t
  | S n' =>
      match walk n s t with
      | TAbs g => TAbs g
      | TRecord fs => TRecord (map (fun q => (fst q, resolve n' s (snd q))) fs)
      | TNom m xs => TNom m (map (fun q => (fst q, resolve n' s (snd q))) xs)
      | TUnion ms => TUnion (map (resolve n' s) ms)
      | t' => t'
      end
  end.

Definition cannot_merge (a b : ty) (why tail : string) : string :=
  "cannot merge " ++ qty W a ++ " with " ++ qty W b ++ why ++ tail.
Fixpoint unify (n : nat) (bound : nat) (why : string) (s : subst) (a b : ty) {struct n} : er subst :=
  match n with
  | 0 => Err "merge: too deep"
  | S n' =>
      let a := walk bound s a in
      let b := walk bound s b in
      if ty_eqb a b then Ok s else
      let mismatch := Err (cannot_merge (resolve bound s a) (resolve bound s b) why " (D43)") in
      let bind x t := if nmem x (gens (resolve bound s t))
                      then Err (cannot_merge (TAbs x) (resolve bound s t) why ", which contains it (D43)")
                      else Ok ((x, t) :: s) in
      match a, b with
      | TAbs x, TAbs y => if s_orient (e_strat E) x y then Ok ((x, b) :: s) else Ok ((y, a) :: s)
      | TAbs x, _ => bind x b
      | _, TAbs y => bind y a
      | TNom m xs, TNom m' ys =>
          if Nat.eqb m m' && nlist_eqb (map fst xs) (map fst ys) then
            fold_left (fun acc q => let! s := acc in unify n' bound why s (fst q) (snd q)) (combine (map snd xs) (map snd ys)) (Ok s)
          else mismatch
      | TUnion xs, TUnion ys =>
          if Nat.eqb (List.length xs) (List.length ys) then
            fold_left (fun acc q => let! s := acc in unify n' bound why s (fst q) (snd q)) (combine xs ys) (Ok s)
          else mismatch
      | TRecord xs, TRecord ys =>
          if slist_eqb (map fst xs) (map fst ys) then
            fold_left (fun acc q => let! s := acc in unify n' bound why s (fst q) (snd q)) (combine (map snd xs) (map snd ys)) (Ok s)
          else mismatch
      | _, _ => mismatch
      end
  end.

Definition unify_sig (bound : nat) (why : string) (s : subst) (a b : fsig) : er subst :=
  if Nat.eqb (List.length (sg_params a)) (List.length (sg_params b)) then
    fold_left (fun acc q => let! s := acc in unify bound bound why s (fst q) (snd q))
              (combine (sg_ret a :: sg_params a) (sg_ret b :: sg_params b)) (Ok s)
  else Err ("conflicting provisions" ++ why ++ " (arity)").

(* Δ4 (lang-all 4934f50, D43): merged entries of one item also unify
   their instantiations (Audit's entry_inst) *)
Definition unify_inst (bound : nat) (why : string) (s : subst) (a b : list (nat * ty)) : er subst :=
  if nlist_eqb (map fst a) (map fst b) then
    fold_left (fun acc q => let! s := acc in unify bound bound why s (fst q) (snd q))
              (combine (map snd a) (map snd b)) (Ok s)
  else Err ("two instantiations" ++ why ++ " differ in their requirements (D43)").
Definition oinst_eqb (a b : option (list (nat * ty))) : bool :=
  match a, b with
  | Some x, Some y => nlist_eqb (map fst x) (map fst y) &&
                      forallb (fun q => ty_eqb (fst q) (snd q)) (combine (map snd x) (map snd y))
  | None, None => true
  | _, _ => false end.

Definition closure (bound : nat) (s : subst) : subst :=
  map (fun q => (fst q, resolve bound s (TAbs (fst q)))) s.

Definition okey_eqb (a b : option item) : bool :=
  match a, b with Some x, Some y => item_eqb x y | _, _ => false end.

(* one round: each entry's signature unified with the first earlier entry
   filed under the same key *)
Fixpoint merge_round (bound : nat) (sg : subst) (seen : list entry) (es : list entry) (s : subst) : er subst :=
  match es with
  | [] => Ok s
  | e :: r =>
      match entry_key sg e with
      | None =>
          match e with
          | NEq l r' =>
              (* [D90]: an equation's sides are one type *)
              let! s' := unify bound bound " in a type equation (D90)" s l r' in
              merge_round bound sg (seen ++ [e]) r s'
          | _ => merge_round bound sg (seen ++ [e]) r s
          end
      | Some k =>
          match find (fun e' => okey_eqb (entry_key sg e') (Some k)) seen with
          | Some e' =>
              match entry_sig sg e', entry_sig sg e with
              | Some a, Some b =>
                  let why := (" for " ++ match k with IMth _ m _ => bq (mname W m) | _ => qkey W k end)%string in
                  let! s' := unify_sig bound why s a b in
                  let! s'' := match entry_inst sg e', entry_inst sg e with
                              | Some ia, Some ib => unify_inst bound why s' ia ib
                              | None, None => Ok s'
                              | _, _ => Err ("two kinds of item are filed under " ++ qkey W k ++ " (D43)")
                              end in
                  merge_round bound sg (seen ++ [e]) r s''
              | None, None => merge_round bound sg (seen ++ [e]) r s
              | _, _ => Err ("two kinds of item are filed under " ++ qkey W k ++ " (D43)")
              end
          | None => merge_round bound sg (seen ++ [e]) r s
          end
      end
  end.

Fixpoint merge_rounds (n bound : nat) (es : list entry) (s : subst) : er subst :=
  match n with
  | 0 => Err "merging the context does not settle (D43)"
  | S n' =>
      let! s' := merge_round bound (closure bound s) [] es s in
      if Nat.eqb (List.length s') (List.length s) then Ok (closure bound s')
      else merge_rounds n' bound es s'
  end.

Definition unifiesb (s : subst) (es : list entry) : bool :=
  forallb (fun e1 => forallb (fun e2 =>
    match entry_key s e1, entry_key s e2 with
    | Some k1, Some k2 => negb (item_eqb k1 k2) ||
                          (ofsig_eqb (entry_sig s e1) (entry_sig s e2) && oinst_eqb (entry_inst s e1) (entry_inst s e2))
    | _, _ => true
    end) es) es &&
  (* [D90]: an equation's sides are one type under σ *)
  forallb (fun e => match e with
    | NEq l r => ty_eqb (app_subst s l) (app_subst s r)
    | _ => true end) es.

(* SOUND: merge es = Ok (s, M) -> MostGeneralMerge s es /\
     MergeOf es M /\ SectionOf M s. *)
Definition merge (es : list entry) : er (subst * merge) :=
  let gs := flat_map entry_gens es in
  let bound := S (S (List.length gs)) * 4 + fuel in
  let! s := merge_rounds (S (S (List.length gs))) bound es [] in
  let! _ := rcheck (unifiesb s es) "merging the context does not settle (D43)" in
  Ok (s, PublicMergePresent.present_merge s es).

(* The new class presentation keeps the exact section produced by the
   unifier. Principality of the unifier is a separate obligation. *)
Lemma merge_presentation : forall es s M,
  merge es = Ok (s, M) -> MostGeneralMerge s es ->
  MergeOf es M /\ SectionOf M s.
Proof.
  intros es s M. unfold merge, rbind.
  destruct (merge_rounds _ _ _ _) as [computed | message]; [|discriminate].
  destruct (unifiesb computed es); [|discriminate].
  intros Result Principal. injection Result as <- <-.
  now apply PublicMergePresent.present_merge_of.
Qed.

(* SOUND: Needs σ es (needs σ es) *)
Definition needs (s : subst) (es : list entry) : list item :=
  iuniq (flat_map (fun e => match entry_key s e with Some k => [k] | None => [] end) es).

(* ---- CtxOf and FunCtx ---- *)

(* SOUND: assume_ctx s = Ok (C, ns) -> AssumeCtx W s C ns; and, when the
   world's table records (C, ns) for s, CtxOf W s C ns. *)
Definition assume_ctx (s : nat) : er (cx * list item) :=
  let! d := decl_of W s in
  let! (es, tm) := items_entries fuel [] (d_mod d) [] [] (map assume_spec (d_assumes d)) in
  let! (er0, tmr) := root_ctx s es tm in
  let all := es ++ er0 in
  let! (sg, M) := merge all in
  Ok (entries_cx sg M tmr all, needs sg all).

(* SOUND: fun_ctx u = Ok (C, ns) -> FunCtx W u C ns, when the world's
   table records (C, ns) for u. *)
Definition fun_ctx (u : nat) : er (cx * list item) :=
  let! (d, fp) := ropt (functor_decl W u) "not a functor" in
  let '(ps, a, r, b) := fp in
  let! (es, tm) := items_entries fuel [] (d_mod d) [] [] (map CISpec a) in
  let implicit := match b with
                  | None => filter (fun t => negb (is_some (nlook t tm))) (mentions W metadata_fuel u)
                  | Some _ => [] end in
  let tm' := map (fun t => (t, TAbs t)) implicit ++ tm in
  let! (sg, M) := merge es in
  Ok (entries_cx sg M tm' es, needs sg es).

(* The w_context table: every fn, method and functor symbol below `bound`
   whose context forms.
   SOUND: for every s, nget (context_table bound) s = Some (C, ns) ->
     (kind fn/meth: AssumeCtx W s C ns; kind functor: FunCtx's premises
     other than the table's own).  With w_context := nget table, CtxOf and
     FunCtx then hold of exactly these entries. *)
Definition context_table (bound : nat) : nmap (cx * list item) :=
  fold_left (fun T s =>
    match kind_sym W s with
    | Some DKFn | Some DKMeth | Some DKVal =>
        match assume_ctx s with Ok cn => nset T s cn | Err _ => T end
    | Some DKFunctor =>
        match fun_ctx s with Ok cn => nset T s cn | Err _ => T end
    | _ => T
    end) (seq 0 bound) NE.

End Ctx.
