(* ------------------------------------------------------------------------ *)
(* EaTypes.v — readers of types, signatures and symbol sets (Audit V.1-V.3) *)
(*                                                                           *)
(* Each function computes the unique value an Audit relation determines    *)
(* (TyOf, Brackets, SigOf, AppOf, RecvTy, MethodNamed, PayloadOf, LangSym), *)
(* or the set of x for which a Prop holds (FreeIn, SigMentions, SpecTypes,  *)
(* FunOutputs, ResultMentions, Mentions, Poly, Fixed).  Recursion through   *)
(* aliases, contexts and functors is bounded by `fuel`. Type readers      *)
(* report exhausted fuel as an error. Set readers return [] at zero and   *)
(* use visited paths to collect dependencies through cycles. Their        *)
(* soundness and coverage conditions are in the corresponding proof files.*)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.

From NM Require Import Audit EaBase.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Section Readers.
Variable W : world.

Definition kind_is (s : nat) (k : dkind) : bool :=
  match kind_sym W s, k with
  | Some DKType, DKType | Some DKUnit, DKUnit | Some DKAlias, DKAlias | Some DKTag, DKTag
  | Some DKVal, DKVal | Some DKFn, DKFn | Some DKCtx, DKCtx | Some DKMeth, DKMeth
  | Some DKFunctor, DKFunctor => true
  | _, _ => false
  end.

Local Open Scope list_scope.

Definition decl_of (s : nat) : er decl := ropt (w_decl W s) "internal: unknown symbol".

(* D15: no two union members share a head; the first two that do *)
Fixpoint union_heads (l seen : list ty) : er unit :=
  match l with
  | [] => Ok tt
  | x :: r =>
      match find (fun y => match head x, head y with Some a, Some b => Nat.eqb a b | _, _ => false end) seen with
      | Some y => Err ("union members " ++ qty W y ++ " and " ++ qty W x ++ " share a head (D15)")
      | None => union_heads r (seen ++ [x])
      end
  end.

Definition bracket_list (a : option (list (path * spec))) : list (path * spec) :=
  match a with Some bs => bs | None => [] end.

(* the symbols a bracket application's left sides resolve to, in module p *)
Definition bracket_bound (p : string) (a : option (list (path * spec))) : list nat :=
  flat_map (fun b => match resolve_sym W p (fst b) with Some t => [t] | None => [] end) (bracket_list a).

(* SOUND: lang_sym short ns n = Some s -> LangSym W short ns n s *)
Definition lang_sym (short : string) (ns : nspace) (n : name) : option nat :=
  match w_library W with
  | Some dir =>
      match w_scope W (dir ++ (short ++ ".moss")) ns n with
      | Some (ESym s) => match w_decl W s with Some _ => Some s | None => None end
      | _ => None
      end
  | None => None
  end.

(* SOUND: lang_item_sym i = Some s -> LangItem W i s *)
Definition lang_item_sym (i : lang_item) : option nat :=
  let '(m, ns, n) := lang_item_key i in lang_sym m ns n.

(* SOUND: method_named p r n = Some m <-> MethodNamed W p r n m *)
Definition method_named (p : string) (r : nat) (n : name) : option nat :=
  match w_attached W r n with
  | Some m => Some m
  | None => match w_scope W p NSDetached n with Some (ESym m) => Some m | _ => None end
  end.

(* ---- TyOf and Brackets ---- *)

(* SOUND: ty_of fuel p tm this a = Ok t -> TyOf W p tm this a t
          brackets fuel p tm g a = Ok bs -> Brackets W p tm g a bs
   EaTypesSound proves these directions. Completeness with enough fuel
   remains an intended obligation; TyOf functionality is in PublicTypeRead. *)
Fixpoint ty_of (fuel : nat) (p : string) (tm : list (nat * ty)) (th : option ty) (a : aty) {struct fuel} : er ty :=
  match fuel with
  | 0 => Err "type nests too deeply (a cyclic alias?)"
  | S f =>
      match a with
      | ATNever => Ok never
      | ATUnit => Ok TUnit
      | ATThis => ropt th "`This` is only legal inside method declarations"
      | ATRec fs =>
          let! _ := rcheck (snodupb (map fst fs)) ("the record type " ++ "{" ++ String.concat ", " (map fst fs) ++ "}" ++ " names a field twice") in
          let! ts := rmap (fun nt => let! t := ty_of f p tm th (snd nt) in Ok (fst nt, t)) fs in
          Ok (TRecord ts)
      | ATUnion ms =>
          let! ts := rmap (ty_of f p tm th) ms in
          let flat := flat_map members ts in
          let! _ := match find (fun m => match m with TNom _ _ => false | _ => true end) flat with
                    | Some (TAbs g) => Err ("union member " ++ bq (sym_name W g) ++ " is abstract and not constrained to a nominal type (D15)")
                    | Some x => Err ("union member " ++ qty W x ++ " is not nominal (D15)")
                    | None => Ok tt end in
          let! _ := union_heads flat [] in
          Ok (TUnion flat)
      | ATRef pa br =>
          let! g := ropt (resolve_sym W p pa) (bq (show_path pa) ++ " is not in scope") in
          match kind_sym W g with
          | Some DKType =>
              match br with
              | None => ropt (nlook g tm) ("type " ++ bq (sym_name W g) ++ " is not available in the context here")
              | Some _ => Err ("type " ++ bq (sym_name W g) ++ " takes no brackets")
              end
          | Some DKTag | Some DKUnit =>
              let! bs := brackets f p tm g br in
              let! args := rmap (fun r => let! t := ropt (nlook r (bs ++ tm))
                                                 ("type " ++ bq (sym_name W r) ++ " (required by " ++ bq (sym_name W g) ++
                                                  ") is not available in the context here") in
                                          Ok (r, t)) (w_reqs W g) in
              Ok (TNom g args)
          | Some DKAlias =>
              let! d := decl_of g in
              match d_ast d with
              | ADAlias _ body =>
                  let! bs := brackets f p tm g br in
                  ty_of f (d_mod d) (bs ++ tm) None body
              | _ => Err "alias"
              end
          | _ => Err (bq (show_path pa) ++ " is not a type")
          end
      end
  end
with brackets (fuel : nat) (p : string) (tm : list (nat * ty)) (g : nat) (a : option (list (path * spec))) {struct fuel}
  : er (list (nat * ty)) :=
  match fuel with
  | 0 => Err "type nests too deeply (a cyclic alias?)"
  | S f =>
      match a with
      | None => Ok []
      | Some bs =>
          let! l := rmap (fun b =>
            match snd b with
            | Spec (Some sp) None sa =>
                let! x := ropt (resolve_sym W p (fst b)) (bq (show_path (fst b)) ++ " is not in scope") in
                let! _ := rcheck (kind_is x DKType)
                            (bq (show_path (fst b)) ++ " in a bracket binding must be an abstract type symbol") in
                let! _ := rcheck (kind_is g DKMeth || nmem x (w_reqs W g))
                            ("application of " ++ qs W g ++ " binds " ++ bq (sym_name W x) ++
                             ", which is not one of its parameters (the type symbols its payload mentions: " ++
                             match w_reqs W g with [] => "none" | rs => qnames W rs end ++ "; D71)") in
                let! t := ty_of f p tm None (ATRef sp sa) in
                Ok (x, t)
            | _ => Err "bracket bindings bind types in v0"
            end) bs in
          Ok (rev l)
      end
  end.

(* SOUND: sig_of fuel s tm this = Ok g -> SigOf W s tm this g *)
Definition sig_of (fuel : nat) (s : nat) (tm : list (nat * ty)) (this : option ty) : er fsig :=
  let! d := decl_of s in
  match d_ast d with
  | ADFn _ _ ps rt _ =>
      let! pts := rmap (fun pt => ty_of fuel (d_mod d) tm this (snd pt)) ps in
      let! r := match rt with Some r => ty_of fuel (d_mod d) tm this r | None => Ok TUnit end in
      Ok (FSig pts r)
  | _ => Err (qs W s ++ " is not a function")
  end.

(* ---- FreeIn and SigMentions ---- *)

(* SOUND: In x (free_in fuel R p a) <-> FreeIn W R p a x (with enough fuel;
   EaDependencyFuel proves exactness when type_depth a <= fuel) *)
Fixpoint free_in (fuel : nat) (R : nat -> list nat) (p : string) (a : aty) {struct fuel} : list nat :=
  match fuel with
  | 0 => []
  | S f =>
      match a with
      | ATRec fs => flat_map (fun nt => free_in f R p (snd nt)) fs
      | ATUnion ms => flat_map (free_in f R p) ms
      | ATRef pa br =>
          match resolve_sym W p pa with
          | None => []
          | Some g =>
              let bound := bracket_bound p br in
              let own := match kind_sym W g with
                         | Some DKType => [g]
                         | _ => R g
                         end in
              filter (fun x => negb (nmem x bound)) own ++
              flat_map (fun b => match snd b with
                                 | Spec (Some sp) _ sa => free_in f R p (ATRef sp sa)
                                 | _ => [] end) (bracket_list br)
          end
      | _ => []
      end
  end.

(* Exact membership when signature_depth W s <= fuel (EaDependencyFuel).
   Successful sig_of at the same fuel supplies this depth bound. *)
Definition sig_mentions (fuel : nat) (s : nat) : list nat :=
  match w_decl W s with
  | Some d =>
      match d_ast d with
      | ADFn _ _ ps rt _ =>
          sort_uniq (flat_map (fun pt => free_in fuel (w_reqs W) (d_mod d) (snd pt)) ps ++
                     match rt with Some r => free_in fuel (w_reqs W) (d_mod d) r | None => [] end)
      | _ => []
      end
  | None => []
  end.

(* AppOf soundness has the same signature-depth condition as sig_mentions.
   The requirement list is Select SigMentions of w_reqs, then SigOutside
   (ascending), for a method; otherwise it is w_reqs. *)
Definition app_requirements (fuel : nat) (s : nat) : list nat :=
  if kind_is s DKMeth then
    let sm := sig_mentions fuel s in
    filter (fun r => nmem r sm) (w_reqs W s) ++ filter (fun t => negb (nmem t (w_reqs W s))) sm
  else w_reqs W s.
Definition app_of (fuel : nat) (s : nat) (tm : list (nat * ty)) : inst :=
  map (fun r => (r, nlook r tm)) (app_requirements fuel s).

(* SOUND: recv_ty fuel r tm = Ok t -> RecvTy W r tm t *)
Definition recv_ty (fuel : nat) (r : nat) (tm : list (nat * ty)) : er ty :=
  let args_of := rmap (fun x => let! t := ropt (nlook x tm) ("type " ++ bq (sym_name W x) ++ " (required by " ++ bq (sym_name W r) ++
                                                              ") is not available in the context here") in Ok (x, t)) (w_reqs W r) in
  match nlook r tm with
  | Some t => Ok t
  | None =>
      match w_decl W r with
      | Some d =>
          match d_ast d with
          | ADAlias _ body => ty_of fuel (d_mod d) tm None body
          | _ => if kind_is r DKType then Err ("type " ++ bq (sym_name W r) ++ " is not available in the context here")
                 else let! args := args_of in Ok (TNom r args)
          end
      | None => let! args := args_of in Ok (TNom r args)
      end
  end.

(* SOUND: payload_of fuel s xs = Ok pay -> PayloadOf W s xs pay *)
Definition payload_of (fuel : nat) (s : nat) (xs : list (nat * ty)) : er ty :=
  let! d := decl_of s in
  match d_ast d with
  | ADTag _ _ t =>
      let! _ := match t with
                | ATRec fs => rcheck (snodupb (map fst fs)) (qs W s ++ " names a field twice, so no record literal builds it")
                | _ => Ok tt end in
      ty_of fuel (d_mod d) xs None t
  | _ => Err (qs W s ++ " is not a tag")
  end.

(* ---- SpecTypes, FunOutputs, ResultMentions, Mentions, Poly, Fixed ---- *)

(* Exact membership for spec_types when all declaration symbols are below
   N and N < fuel (EaContextFuel). A context already on the path adds
   nothing: a path through a context twice has a shorter one. *)
Fixpoint spec_types_go (fuel : nat) (stk : list nat) (p : string) (sp : spec) {struct fuel} : list nat :=
  match fuel with
  | 0 => []
  | S f =>
      match sp with
      | Spec (Some pa) None _ =>
          match resolve_sym W p pa with
          | Some t =>
              match kind_sym W t with
              | Some DKType => [t]
              | _ => if nmem t stk then [] else
                     match w_decl W t with
                     | Some d => match d_ast d with
                                 | ADCtx _ its => flat_map (spec_types_go f (t :: stk) (d_mod d)) (flat_map citem_specs its)
                                 | _ => [] end
                     | None => [] end
              end
          | None => []
          end
      | _ => []
      end
  end.

Definition spec_types (fuel : nat) (p : string) (sp : spec) : list nat := spec_types_go fuel [] p sp.

Definition functor_decl (u : nat) : option (decl * (option (list name) * list spec * list spec * option (list (list (spec * option aexpr))))) :=
  match w_decl W u with
  | Some d => match functor_parts (d_ast d) with Some fp => Some (d, fp) | None => None end
  | None => None
  end.

(* Exact FunOutputs membership under the same declaration and fuel bounds as
   spec_types. The result is sorted and has no repeats. *)
Definition fun_outputs (fuel : nat) (u : nat) : list nat :=
  match functor_decl u with
  | Some (d, (_, _, r, _)) => sort_uniq (flat_map (spec_types fuel (d_mod d)) r)
  | None => []
  end.

(* Membership implies ResultMentions when declaration symbols are below
   N and N + (N+1)*N < fuel (EaResultSound). The visited (self, symbol)
   pairs bound recursion; the remaining N levels cover output filtering.
   General membership completeness remains open. *)
Fixpoint pmem (x : nat * nat) (l : list (nat * nat)) : bool :=
  match l with [] => false | y :: r => (Nat.eqb (fst x) (fst y) && Nat.eqb (snd x) (snd y)) || pmem x r end.
Fixpoint result_mentions_go (fuel : nat) (stk : list (nat * nat)) (self : nat) (p : string) (sp : spec) {struct fuel} : list nat :=
  match fuel with
  | 0 => []
  | S f =>
      match sp with
      | Spec (Some pa) d a =>
          match resolve_sym W p pa with
          | None => []
          | Some s =>
              let bound := bracket_bound p a in
              let unbound l := filter (fun t => negb (nmem t bound)) l in
              let from_brackets := flat_map (fun b => match snd b with
                  | Spec (Some sp') _ sa => free_in fuel (w_reqs W) p (ATRef sp' sa)
                  | _ => [] end) (bracket_list a) in
              let own :=
                match d with
                | Some dn =>
                    unbound ((if kind_is s DKType then [s] else w_reqs W s) ++
                             match method_named p s dn with Some m => sig_mentions fuel m | None => [] end)
                | None =>
                    if kind_is s DKVal then unbound (w_reqs W s)
                    else if kind_is s DKFn then unbound (w_reqs W s ++ sig_mentions fuel s)
                    else match w_decl W s with
                         | Some ds =>
                             match d_ast ds with
                             | ADCtx _ its => if pmem (self, s) stk then [] else
                                                unbound (flat_map (result_mentions_go f ((self, s) :: stk) self (d_mod ds)) (ctx_specs its))
                             | ADFunctor _ (Some names) _ res None =>
                                 if Nat.eqb s self || pmem (s, s) stk then [] else
                                 let outs := fun_outputs fuel s in
                                 let nm := flat_map (fun x => match resolve_sym W (d_mod ds) [x] with Some t => [t] | None => [] end) names in
                                 unbound (filter (fun t => negb (nmem t outs) && negb (nmem t nm))
                                                 (flat_map (result_mentions_go f ((s, s) :: stk) s (d_mod ds)) res))
                             | _ => []
                             end
                         | None => []
                         end
                end in
              own ++ from_brackets
          end
      | _ => []
      end
  end.

Definition result_mentions (fuel : nat) (self : nat) (p : string) (sp : spec) : list nat :=
  result_mentions_go fuel [] self p sp.

(* Membership implies Mentions under the result_mentions budget.
   General membership completeness remains open. The result is sorted. *)
Definition mentions (fuel : nat) (u : nat) : list nat :=
  match functor_decl u with
  | Some (d, (_, _, r, _)) =>
      let outs := fun_outputs fuel u in
      sort_uniq (filter (fun t => negb (nmem t outs)) (flat_map (result_mentions fuel u (d_mod d)) r))
  | None => []
  end.

(* Membership implies Poly under the same budget (EaFixedSound).
   Explicitly bracketed abstract functors have exact membership at any fuel. *)
Definition poly (fuel : nat) (u : nat) : list nat :=
  match functor_decl u with
  | Some (d, (ps, _, _, None)) =>
      match ps with
      | None => mentions fuel u
      | Some names => sort_uniq (flat_map (fun x => match resolve_sym W (d_mod d) [x] with Some t => [t] | None => [] end) names)
      end
  | _ => []
  end.

(* Membership implies Fixed under the same budget. The result is sorted.
   Abstract unbracketed functors have no fixed inputs at any fuel. *)
Definition fixed (fuel : nat) (u : nat) : list nat :=
  let pl := poly fuel u in filter (fun t => negb (nmem t pl)) (mentions fuel u).

(* Emitted pairs satisfy Fixed and the type-map lookup under the budget.
   Full FixedReadings is proved for abstract unbracketed functors;
   general coverage remains open. *)
Definition fixed_readings (fuel : nat) (u : nat) (tm : list (nat * ty)) : subst :=
  flat_map (fun x => match nlook x tm with Some t => [(x, t)] | None => [] end) (fixed fuel u).

End Readers.
