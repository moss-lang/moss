(* ------------------------------------------------------------------------ *)
(* EaFamily.v — functor results and families (Audit V.5, D84 rule 7):     *)
(* OutputItemEntries, ProvideResult, Family, FamilyProduces, OnlyFamily,   *)
(* FamilyReadOff, TypesFamilyApplication, FamilyStep/Demand.                    *)
(*                                                                           *)
(* Producer search reads each polymorphic input as a fresh generation.
   A successful reading of the family's result induces a substitution
   from these temporary generations to the receiver it produces.
   FamilyReadOff separately checks agreement of the family's matchings. *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaCall EaDecide.
From NM Require EaAssumptionAtoms.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* A context's items when it holds no equation ([D90]): `its = map CISpec sps`. *)
Fixpoint ctx_spec_items (its : list citem) : option (list spec) :=
  match its with
  | [] => Some []
  | CISpec sp :: r => option_map (cons sp) (ctx_spec_items r)
  | CIEq _ _ :: _ => None
  end.

Section Family.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.

(* ---- OutputBrackets, OutputItemEntries, ProvideResult ---- *)

(* SOUND: output_brackets p tm a = Ok bs -> OutputBrackets W p tm a bs *)
Definition output_brackets (p : string) (tm : list (nat * ty)) (a : option (list (path * spec))) : er (list (nat * ty)) :=
  match a with
  | None => Ok []
  | Some bs =>
      let! l := rmap (fun b =>
        match snd b with
        | Spec (Some sp) None sa =>
            let! x := ropt (resolve_sym W p (fst b)) (bq (show_path (fst b)) ++ " is not in scope") in
            let! _ := rcheck (kind_is W x DKType) (bq (show_path (fst b)) ++ " in a bracket binding must be an abstract type symbol") in
            let! t := ty_of W fuel p tm None (ATRef sp sa) in
            Ok (x, t)
        | _ => Err (bq (show_path (fst b)) ++ " in a bracket binding must be an abstract type symbol")
        end) bs in
      Ok (rev l)
  end.

(* SOUND: output_item_entries n p ob tm sp = Ok (es, tm') -> OutputItemEntries W p ob tm sp es tm' *)
Fixpoint output_item_entries (n : nat) (stk : list nat) (p : string) (ob tm : list (nat * ty)) (sp : spec) {struct n}
  : er (list entry * list (nat * ty)) :=
  match n with
  | 0 => Err "result: too deep"
  | S n' =>
      match sp with
      | Spec (Some pa) (Some dn) a =>
          let! r := ropt (resolve_sym W p pa) (bq (show_path pa) ++ " is not in scope") in
          let! m := ropt (method_named W p r dn) ("no method `." ++ dn ++ "` for " ++ bq (show_path pa) ++ " is in scope") in
          let! bs := output_brackets p (ob ++ tm) a in
          let tm2 := bs ++ ob ++ tm in
          let! rt := recv_ty W fuel r tm2 in
          let! g := sig_of W fuel m tm2 (Some rt) in
          Ok ([NMth rt m g (app_of W metadata_fuel m tm2)], tm)
      | Spec (Some pa) None a =>
          let! s := ropt (resolve_sym W p pa) (bq (show_path pa) ++ " is not in scope") in
          let! d := decl_of W s in
          let! bs := output_brackets p (ob ++ tm) a in
          let tm2 := bs ++ ob ++ tm in
          match d_ast d with
          | ADType _ => Ok ([NTy s], tm)
          | ADUnit _ | ADTag _ _ _ | ADAlias _ _ | ADVal _ _ (Some _) | ADFn RPlain _ _ _ (Some _) => Ok ([], tm)
          | ADVal _ t None =>
              let! ty0 := ty_of W fuel (d_mod d) tm2 None t in
              Ok ([NVal s ty0 (app_of W metadata_fuel s tm2)], tm)
          | ADFn RPlain _ _ _ None =>
              let! g := sig_of W fuel s tm2 None in
              Ok ([NFn s g (app_of W metadata_fuel s tm2)], tm)
          | ADFunctor _ _ _ _ None =>
              let! _ := rcheck (forallb (fun x => is_some (nlook x tm2)) (fixed W metadata_fuel s))
                          ("a functor result's functor " ++ qs W s ++ " needs the types it is fixed in in force (D84)") in
              Ok ([NFun s (fixed_readings W metadata_fuel s tm2)], tm)
          | ADCtx _ its =>
              let! _ := rcheck (negb (nmem s stk)) ("context " ++ qs W s ++ " nests in a cycle") in
              (* [D90]: an equation is a requirement, and a result provides *)
              let! sps := ropt (ctx_spec_items its)
                            (qs W s ++ " holds a type equation, which a functor's result cannot provide (D90)") in
              output_items_entries n' (s :: stk) (d_mod d) (bs ++ ob) tm sps
          | _ => Err (qs W s ++ " cannot be an item of a functor's result")
          end
      | _ => Err "an empty context item"
      end
  end
with output_items_entries (n : nat) (stk : list nat) (p : string) (ob tm : list (nat * ty)) (sps : list spec) {struct n}
  : er (list entry * list (nat * ty)) :=
  match n with
  | 0 => Err "result: too deep"
  | S n' =>
      match sps with
      | [] => Ok ([], tm)
      | sp :: r =>
          let! (es1, tm1) := output_item_entries n' stk p ob tm sp in
          let! (es2, tm2) := output_items_entries n' stk p ob tm1 r in
          Ok (es1 ++ es2, tm2)
      end
  end.

(* SOUND: provide_result u C out = Ok (C', ks) -> ProvideResult W u C out C' ks *)
Definition provide_result (u : nat) (C : cx) (out : nat -> ty) : er (cx * list item) :=
  let! (d, fp) := ropt (functor_decl W u) "not a functor" in
  let '(_, _, r, _) := fp in
  let outs := fun_outputs W metadata_fuel u in
  let! (es, tm') := output_items_entries fuel [] (d_mod d) [] (output_type_map C outs out) r in
  Ok (output_cx C tm' es, flat_map (fun e => match entry_key (c_sub C) e with Some k => [k] | None => [] end) es).

(* ---- Family ---- *)

Fixpoint family_result (n : nat) (mi : string) (sp : spec) {struct n} : bool :=
  match n with
  | 0 => false
  | S n' =>
      match sp with
      | Spec (Some _) (Some _) _ => true
      | Spec (Some pa) None _ =>
          match resolve_sym W mi pa with
          | Some u => match w_decl W u with
                      | Some d => match d_ast d with
                                  | ADCtx _ its => match ctx_spec_items its with
                                                    | Some sps => forallb (family_result n' (d_mod d)) sps
                                                    | None => false end
                                  | _ => false end
                      | None => false end
          | None => false
          end
      | _ => false
      end
  end.

Definition family_bracket_free (p : string) (a : option (list (path * spec))) (t : nat) : bool :=
  existsb (fun b => match snd b with
    | Spec (Some pa) _ bs => nmem t (free_in W metadata_fuel (w_reqs W) p (ATRef pa bs))
    | _ => false end) (bracket_list a).

Fixpoint family_keyed (n : nat) (u : nat) (pl : list nat) (p : string) (sp : spec) {struct n} : bool :=
  match n with
  | 0 => false
  | S n' =>
      match sp with
      | Spec (Some pa) (Some dn) a =>
          match resolve_sym W p pa with
          | Some r =>
              match method_named W p r dn with
              | Some m =>
                  let sm := sig_mentions W metadata_fuel m in
                  let recv := free_in W metadata_fuel (w_reqs W) p (ATRef pa None) in
                  forallb (fun t =>
                    if (nmem t sm || family_bracket_free p a t) && negb (nmem t (bracket_bound W p a))
                    then nmem t recv else true) pl
              | None => false
              end
          | None => false
          end
      | Spec (Some pa) None _ =>
          match resolve_sym W p pa with
          | Some c => match w_decl W c with
                      | Some d => match d_ast d with
                                  | ADCtx _ its => match ctx_spec_items its with
                                                    | Some sps => forallb (family_keyed n' u pl (d_mod d)) sps
                                                    | None => false end
                                  | _ => false end
                      | None => false end
          | None => false
          end
      | _ => false
      end
  end.

(* EaFamilySound proves the successful classification implies Family at the
   initialized metadata budget. Completeness also needs sufficient traversal
   fuel for family_result/family_keyed and remains a separate obligation. *)
Definition is_family (u : nat) : bool :=
  match w_decl W u with
  | Some d =>
      match d_ast d with
      | ADFunctor _ ps _ r None =>
          forallb (family_result fuel (d_mod d)) r &&
          (match ps with
           | Some names => forallb (fun x => match resolve_sym W (d_mod d) [x] with
                                             | Some t => kind_is W t DKType | None => true end) names
           | None => true end) &&
          forallb (family_keyed fuel u (poly W metadata_fuel u) (d_mod d)) r
      | _ => false
      end
  | None => false
  end.

(* ---- Matching a formal receiver against a key's (read-off) ---- *)

Fixpoint match_rows {A : Type} (step : A -> A -> subst -> option subst)
    (formal actual : list A) (acc : subst) : option subst :=
  match formal, actual with
  | [], [] => Some acc
  | x :: xs, y :: ys =>
      match step x y acc with
      | Some next => match_rows step xs ys next
      | None => None
      end
  | _, _ => None
  end.

Fixpoint match_ty (C : cx) (marks : list nat) (formal actual : ty) (acc : subst)
    {struct formal} : option subst :=
  match formal with
  | TAbs t =>
      if nmem t marks then
        match lookup_subst t acc with
        | Some v => if ty_eqb (canon C v) actual then Some acc else None
        | None => Some (acc ++ [(t, actual)])
        end
      else if ty_eqb (canon C formal) actual then Some acc else None
  | TNom tag fields =>
      match actual with
      | TNom tag' fields' =>
          if Nat.eqb tag tag' then
            match_rows (fun a b acc =>
              if Nat.eqb (fst a) (fst b) then match_ty C marks (snd a) (snd b) acc else None)
              fields fields' acc
          else None
      | _ => None
      end
  | TRecord fields =>
      match actual with
      | TRecord fields' =>
          match_rows (fun a b acc =>
            if String.eqb (fst a) (fst b) then match_ty C marks (snd a) (snd b) acc else None)
            fields fields' acc
      | _ => None
      end
  | TUnion members =>
      match actual with
      | TUnion members' => match_rows (match_ty C marks) members members' acc
      | _ => None
      end
  | _ => if ty_eqb formal actual then Some acc else None
  end.

(* the matching for one formal receiver, verified against the equation *)
Definition read_off_one (C : cx) (marks : list nat) (rf ot : ty) : option subst :=
  match match_ty C marks rf ot [] with
  | Some mt => if ty_eqb (canon C (app_subst mt rf)) ot then Some mt else None
  | None => None
  end.

Definition marks_cx (C : cx) (marks : list nat) : cx := family_context C (map (fun t => (t, TAbs t)) marks).

(* the formal keys of family u with method m whose receivers match ot *)
Definition formal_matches (C : cx) (u : nat) (ot : ty) (m : nat) : er (list subst) :=
  let marks := poly W metadata_fuel u in
  let! (_, fkeys) := provide_result u (marks_cx C marks) (fun t => TAbs t) in
  Ok (flat_map (fun k => match k with
    | IMth rf m' _ => if Nat.eqb m m' then match read_off_one C marks rf ot with Some mt => [mt] | None => [] end else []
    | _ => [] end) fkeys).

(* A producer probe gives each polymorphic source input its own role.
   The caller's merge section cannot absorb these roles. *)
Definition producer_marker_support (C : cx) : list nat :=
  flat_map (fun q => fst q :: gens (snd q)) (c_ty C ++ c_sub C).

Definition producer_marker (C : cx) : nat -> nat :=
  let floor := EaAssumptionAtoms.scope_floor metadata_fuel (producer_marker_support C) in
  Nat.add floor.

Definition producer_markings (C : cx) (marks : list nat) : subst :=
  let marker := producer_marker C in
  map (fun t => (t, TAbs (marker t))) marks.

Definition probe_producer (C : cx) (u : nat) (ot : ty) (m : nat) : er bool :=
  let marks := poly W metadata_fuel u in
  let! (_, keys) := provide_result u (family_context C (producer_markings C marks)) (fun t => TAbs t) in
  let markers := map (producer_marker C) marks in
  Ok (existsb (fun key => match key with
    | IMth rf method _ => Nat.eqb m method && is_some (read_off_one C markers rf ot)
    | _ => false
    end) keys).

(* If the marked result fails to form, resolve its written method
   occurrences in their own modules. A detached method can be imported
   under a different name from the one in its declaration. *)
Fixpoint result_has_method (depth : nat) (p : string) (m : nat) (sp : spec) : bool :=
  match depth, sp with
  | S next, Spec (Some pa) dn _ =>
      match resolve_sym W p pa with
      | Some receiver =>
          match dn with
          | Some name => match method_named W p receiver name with
                         | Some method => Nat.eqb m method | None => false end
          | None => match w_decl W receiver with
                    | Some d => match d_ast d with
                                | ADCtx _ items =>
                                    existsb (result_has_method next (d_mod d) m) (flat_map citem_specs items)
                                | _ => false end
                    | None => false end
          end
      | None => false
      end
  | _, _ => false
  end.

Definition functor_has_result_method (u m : nat) : bool :=
  match functor_decl W u with
  | Some (d, (_, _, results, _)) => existsb (result_has_method fuel (d_mod d) m) results
  | None => false
  end.

(* EaFamilyProducerSound proves coverage of FamilyProduces at method keys. *)
Definition family_produces (C : cx) (u : nat) (k : item) : bool :=
  match k with
  | IMth ot m _ =>
      is_some (cfun C u) && is_family u &&
      match probe_producer C u ot m with
      | Ok matches => matches
      (* the marked result does not form (e.g. a union with a P(F) member
         reads only at some θ): conservatively, F may produce the key if
         it has a result item of that name or resolved method identity. *)
      | Err _ =>
          existsb (fun l => match snd l with Some dn => String.eqb dn (sym_name W m) | None => false end)
                  (result_leaves W fuel u) || functor_has_result_method u m
      end
  | _ => false
  end.

(* the families in force that produce k *)
Definition producers (C : cx) (k : item) : list nat :=
  filter (fun u => family_produces C u k) (uniq (cfun_keys C)).

Definition subst_canon_agree (C : cx) (a b : subst) : bool :=
  forallb (fun t => oty_eqb (option_map (canon C) (lookup_subst t a)) (option_map (canon C) (lookup_subst t b)))
          (map fst a ++ map fst b).

(* SOUND: family_read_off C u ot m = Ok mt -> FamilyReadOff W C u ot m mt *)
Definition family_read_off (C : cx) (u : nat) (ot : ty) (m : nat) : er subst :=
  let! cands := formal_matches C u ot m in
  match cands with
  | [] => Err (bq (sym_name W u) ++ " does not provide " ++ bq (sty W ot ++ mname W m) ++ " here (D84 rule 7)")
  | c0 :: _ =>
      let chosen := nth (s_pick (e_strat E) 0 (List.length cands)) cands c0 in
      let! _ := rcheck (forallb (subst_canon_agree C chosen) cands)
                  (bq (sty W ot ++ mname W m) ++ " is two items of the family " ++ bq (sym_name W u) ++
                   " here, so which one is meant is ambiguous (D84 rule 7)") in
      Ok chosen
  end.

(* SOUND: fixed_agreement C u = true <-> FixedAgreement W C u *)
Definition fixed_agreement (C : cx) (u : nat) : bool :=
  forallb (fun x =>
    match capp C (IFun u), nlook x (c_ty C) with
    | Some recorded, Some now =>
        match nlook x recorded with
        | Some (Some was) => ty_eqb (canon C was) (canon C now)
        | _ => true
        end
    | _, _ => true
    end) (fixed W metadata_fuel u).

(* EaFamilySound proves that success supplies the typed family application
   and its emitted satisfaction map. *)
Definition family_application (C : cx) (u : nat) (mt : subst) : er (subst * smap * cx * list item) :=
  let fname := bq (sym_name W u) in
  let! _ := ropt (cfun C u) (fname ++ " is not available in the context here") in
  let! _ := rcheck (is_family u) (fname ++ " is not a family") in
  let pl := poly W metadata_fuel u in
  let! _ := rcheck (forallb (fun q => nmem (fst q) pl) mt) ("applying the family " ++ fname ++ " matches a type outside its polymorphic inputs") in
  let! (formal, fneeds) := ropt (w_context W u) ("the context of the family " ++ fname ++ " does not form") in
  let C1 := family_context C mt in
  let th := map (fun q => (fst q, canon C1 (app_subst (c_ty C1) (TAbs (fst q))))) (c_ty formal) in
  let! _ := match find (fun q => negb (bound_in C1 (fst q))) (c_ty formal) with
            | Some (t, _) => Err ("applying the family " ++ fname ++ " needs " ++ bq (sym_name W t) ++
                                  ", which is not available in the context here")
            | None => Ok tt end in
  let! _ := rcheck (fixed_agreement C1 u)
              ("applying the family " ++ fname ++ " here reads a type it is fixed in otherwise than where it was provided (D84 rule 5)") in
  let! sm := satisfies W ("applying the family " ++ fname) ("applying the family " ++ fname) C1 formal th fneeds in
  let! (C', ks) := provide_result u C1 (fun t => TAbs t) in
  let! _ := rcheck (inodupb (map provision_id ks)) ("the family " ++ fname ++ " provides two items under one key here (D84 rule 7)") in
  Ok (th, sm, C', ks).

(* The one family in force producing mkey ot m (OnlyFamily read in C0),
   applied in C at the matching read off ot.
   SOUND: family_for C0 C ot m = Ok (u, mt, th, sm, C', ks) ->
     OnlyFamily W C0 u (mkey ot m), a typed family application with its map,
     FamilyReadOff W C u ot m mt /\ In (mkey ot m) ks *)
(* Rule B for a family demand (Audit's MatchingInForce, lang-all 2d7e8fe).
   SOUND: matching_in_force C mt = true <-> MatchingInForce C mt *)
Definition matching_in_force (C : cx) (mt : subst) : bool :=
  forallb (fun q => forallb (fun y =>
      existsb (fun xu => nmem y (gens (canon C (snd xu)))) (c_ty C))
    (gens (canon C (snd q)))) mt.

(* a method as the old oracle's reports named it: `.m`, `.R.m` *)
Definition old_mname (m : nat) : string :=
  match ast_of W m with Some (ADFn (RAttached _) _ _ _ _) => "." ++ dname W m | _ => mname W m end.
(* the old oracle's report when nothing provides m at ot *)
Definition no_method_msg (C : cx) (ot : ty) (m : nat) : string :=
  let others := flat_map (fun q => match q with
      | (IMth r m' _, ProvidedMethod _ _) =>
          if Nat.eqb m m' && negb (ty_eqb r ot) &&
             match head r, head ot with Some a, Some b => Nat.eqb a b | _, _ => false end
          then [r] else []
      | _ => [] end) (c_provisions C) in
  match others with
  | _ :: _ => bq (old_mname m) ++ " is provided at `" ++ String.concat "`, `" (map (sty W) others) ++ "`, not at " ++
              qty W ot ++ ": a method provision serves only the instantiation it was made at (D62)"
  | [] => "no method " ++ bq (old_mname m) ++ " is available for " ++ qty W ot ++ " in the context here"
  end.

Definition family_for (C0 C : cx) (ot : ty) (m : nat) : er (nat * subst * subst * smap * cx * list item) :=
  match producers C0 (mkey ot m) with
  | [u] =>
      let! mt := family_read_off C u ot m in
      let! (((th, sm), C'), ks) := family_application C u mt in
      let! _ := rcheck (imem (mkey ot m) ks)
                  (bq (sym_name W u) ++ " does not provide " ++ bq (sty W ot ++ mname W m) ++ " here (D84 rule 7)") in
      let! _ := rcheck (matching_in_force C mt)
                  ("applying the family " ++ bq (sym_name W u) ++ " for " ++ bq (sty W ot ++ mname W m) ++
                   " mentions a type that is not in force here (assume or bind it; rule B)") in
      Ok (u, mt, th, sm, C', ks)
  | [] => Err (no_method_msg C ot m)
  | u1 :: u2 :: _ => Err (bq (sty W ot ++ mname W m) ++ " is produced by two families in force, " ++
                          bq (sym_name W u1) ++ " and " ++ bq (sym_name W u2) ++ " (D84 rule 7, D44)")
  end.

Definition is_nil {A} (l : list A) : bool := match l with [] => true | _ => false end.
(* SOUND: demand C wanted = Ok (wrap, C') -> Demand W C wanted [] C wrap C'.
   Each wanted method key missing in C (and in the context so far) is
   demanded from its one family, in order. *)
Fixpoint demand_go (C0 : cx) (used : list (nat * subst)) (wanted rest : list item) (C : cx) : er (wrapper * cx) :=
  match rest with
  | [] => Ok ((fun x => x), C)
  | IMth ot m th0 :: rest' =>
      if negb (is_some (cmth C0 ot m)) && negb (is_some (cmth C ot m))
         && imem (mkey ot m) wanted && negb (is_nil (producers C0 (mkey ot m))) then
        let! (((((u, mt), th), sm), Cr), ks) := family_for C0 C ot m in
        let! _ := rcheck (negb (existsb (fun q => Nat.eqb (fst q) u && subst_eqb (snd q) mt) used))
                    ("the family " ++ bq (sym_name W u) ++ " is demanded twice at one instantiation here") in
        let! (w, Cf) := demand_go C0 ((u, mt) :: used) wanted rest' (family_restore C Cr) in
        Ok ((fun x => apply_expression u th sm (map (rkof Cr) ks) (w x)), Cf)
      else demand_go C0 used wanted rest' C
  | _ :: rest' => demand_go C0 used wanted rest' C
  end.
Definition demand (C : cx) (wanted : list item) : er (wrapper * cx) := demand_go C [] wanted wanted C.

End Family.
