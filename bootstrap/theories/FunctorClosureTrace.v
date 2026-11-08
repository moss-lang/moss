(* Executable receipts for binding a defined functor as a closure. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound.
Import ListNotations.
Local Open Scope list_scope.
Local Open Scope string_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Definition defined_functor_binding (f : nat) (c : cx) (e : env) (fg bg : gsym)
 (bbody : option (list (list (spec * option aexpr))))
 (ef eg : env) (fneeds gneeds : list key) (target_fixed : list nat) (stamp : nat) :=
 let mi := cx_mod c in
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
ret ([SBindFun (gs_id fg) h sm smc], e').
