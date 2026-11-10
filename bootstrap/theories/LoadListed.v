(* Read-only D73 checking, with the legacy traversal and diagnostics. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import LoadWorld NMap Sx.
Import ListNotations.
Local Open Scope string_scope.
Local Infix "+++" := List.app (right associativity, at level 60).

Fixpoint fold {A B} (f : A -> B -> res A) (a : A) (xs : list B) : res A :=
  match xs with
  | [] => Good a
  | x :: rest => let* a' := f a x in fold f a' rest
  end.

Section Reader.
Variable w : world.
Variable syms : nmap gsym.

Definition resolve_sym (mi : nat) (p : path) : res (option gsym) :=
  let* r := resolve_path w mi p in
  match r with
  | Some (ESym i) =>
      match nget syms i with
      | Some g => Good (Some g)
      | None => Bad ("internal: no symbol " ++ nat_to_string i)
      end
  | _ => Good None
  end.

Fixpoint walk_specs (fuel : nat) (mi : nat) (specs : list spec)
    (acc : list nat * list nat) : res (list nat * list nat) :=
  match fuel with
  | 0 => Bad "internal: the context walk ran out of fuel"
  | S f =>
      fold (fun acc sp =>
        match sp with
        | Spec (Some p) None _ =>
            let* tg := resolve_sym mi p in
            match tg with
            | Some g =>
                if kind_eqb (gs_kind g) KType then
                  Good (fst acc, if nmem (gs_id g) (snd acc) then snd acc else snd acc +++ [gs_id g])
                else if kind_eqb (gs_kind g) KCtx then
                  if nmem (gs_id g) (fst acc) then Good acc
                  else match gs_decl g with
                       | ADCtx _ its => walk_specs f (gs_mod g) (flat_map citem_specs its)
                           (gs_id g :: fst acc, snd acc)
                       | _ => Good acc
                       end
                else Good acc
            | None => Good acc
            end
        | _ => Good acc
        end) acc specs
  end.

Definition assume_type_syms (g : gsym) : res (list nat) :=
  let* acc := walk_specs (S (w_nsyms w)) (gs_mod g)
      (LoadWorld.assume_specs (gs_assumes g)) ([], []) in
  Good (snd acc).

Definition listed_params (g : gsym) : res (list nat) :=
  match gs_decl g with
  | ADTag _ ((_ :: _) as names) _ =>
      let* nesting := assume_type_syms g in
      fold (fun out n =>
        let* t := resolve_sym (gs_mod g) [n] in
        match t with
        | Some t' =>
            if nmem (gs_id t') nesting then
              Good (if nmem (gs_id t') out then out else out +++ [gs_id t'])
            else err_in (get_mod w (gs_mod g)) ("`" ++ gs_name g ++ "` lists `" ++ n ++
                 "`, which is not a type its assume nesting requires (D73)")
        | None => err_in (get_mod w (gs_mod g)) ("`" ++ gs_name g ++ "` lists `" ++ n ++
                 "`, which is not a type its assume nesting requires (D73)")
        end) [] names
  | _ => Good []
  end.

Definition check_with : res unit :=
  let* _ := fold (fun (_ : unit) i =>
    match nget (w_syms w) i with
    | Some g => if kind_eqb (gs_kind g) KTag then
                  let* _ := listed_params g in Good tt
                else Good tt
    | None => Good tt
    end) tt (seq 0 (w_nsyms w)) in
  Good tt.
End Reader.

Definition check (w : world) : res unit := check_with w (w_syms w).
