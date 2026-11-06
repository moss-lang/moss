(* Generic payload extraction preserves written union nodes instead of
   asking unresolved formal parameters to have concrete nominal heads.
   Source-instance formation remains the ordinary checked elaborator. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit CoreMoss Elab ReifySource PayloadInst.
Import ListNotations Audit.Input.
Local Open Scope string_scope.
Local Infix "+++" := List.app (right associativity, at level 60).

Fixpoint skeleton_type (fuel : nat) (t : aty) (mi : nat) (tm : list (nat * ety)) (e : env)
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
          let! fs' := mmap (fun p => let! y := skeleton_type f (snd p) mi tm e this in ret (fst p, y)) fs in
          ret (YRec fs')
      | ATUnion ms =>
          let! operands := mmap (fun m => skeleton_type f m mi tm e this) ms in
          ret (YUnion (flat_map Elab.members operands))
      | ATRef p a =>
          let! g := resolve_type_symbol mi p in
          match a with
          | None => skeleton_symbol f mi g tm e
          | Some bs =>
              check_relevant mi g bs ;;
              let! s := skeleton_arguments f e mi bs tm in
              let tm' := s +++ tm in
              check_totality mi g tm' e ;;
              (* a type symbol takes no brackets, not even `T[]` (Audit
                 note 50) *)
              if kind_eqb (gs_kind g) KType then err mi ("type `" ++ gs_name g ++ "` takes no brackets") else
              skeleton_symbol f mi g tm' e
          end
      end
  end
with skeleton_arguments (fuel : nat) (e : env) (mi : nat) (bs : list (path * spec)) (outer : list (nat * ety))
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
              let! y := skeleton_type f (ATRef sp sa) mi (outer +++ tymap e) e None in ret (gs_id g, y)
          | _ => err mi "bracket bindings bind types in v0"
          end) bs in
      ret (rev l)   (* later bindings override earlier ones, as a dict's *)
  end
with skeleton_symbol (fuel : nat) (mi : nat) (g : gsym) (tm : list (nat * ety)) (e : env) {struct fuel} : M ety :=
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
                  | ADAlias _ t => skeleton_type f t (gs_mod g) tm e None
                  | _ => fail "internal: alias"
                  end
      | _ => err mi ("`" ++ gs_name g ++ "` is not a type")
      end
  end.


Definition skeleton_tag_payload (n : nsym) : M ety :=
  let! g := sym n in
  match gs_decl g with
  | ADTag _ _ payload =>
      let! parameters := reqs n in
      skeleton_type 200 payload (gs_mod g)
        (map (fun t => (t,YAbs t)) parameters) empty_env None
  | _ => ret YUnit
  end.
