(* ------------------------------------------------------------------------ *)
(* EaCall.v — calls (Audit V.4): CallTheta and Satisfies.                   *)
(*                                                                           *)
(* θ is constructed as CallTheta's principal solution (a requirement C     *)
(* binds reads as it reads here; an unbound one through a bound member of  *)
(* its merge class, else through the class's representative; one merged   *)
(* with a structure through that structure) and then every clause is      *)
(* checked, so a malformed choice is an error, never an unsound result.   *)
(* lang-b/lang-q will change these: keep each clause a separate check.     *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap EaBase.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition bound_in (C : cx) (r : nat) : bool := is_some (nlook r (c_ty C)).
Definition read_here (C : cx) (r : nat) : ty := canon C (app_subst (c_ty C) (TAbs r)).

(* the members of the merge class (in Cs) whose canonical reading is t *)
Definition class_of (Cs : cx) (t : ty) : list nat :=
  (match t with TAbs z => if is_some (lookup_subst z (c_sub Cs)) then [] else [z] | _ => [] end) ++
  flat_map (fun q => if ty_eqb (snd q) t then [fst q] else []) (c_sub Cs).

(* a class representative z's value here: through its first member C binds *)
Definition class_value (C Cs : cx) (z : nat) : ty :=
  match find (bound_in C) (class_of Cs (TAbs z)) with
  | Some y => read_here C y
  | None => canon C (TAbs z)
  end.

(* SOUND: call_theta C Cs s = Ok th -> CallTheta W C Cs s th
   (θ's merge clause is checked; the other two hold by construction).
   Since rule B (lang-all 2d7e8fe) CallTheta no longer fixes an unbound
   requirement's value (every use also asks RequirementsInForce); the
   construction still reads a class with no member bound here through
   its representative z, which AssumedComplete's principal clause asks. *)
(* the entry of the earliest declared symbol, for a stable report *)
Definition min_first {A} (l : list (nat * A)) : option (nat * A) :=
  fold_left (fun acc q => match acc with Some p => if Nat.ltb (fst q) (fst p) then Some q else acc | None => Some q end) l None.
Definition merges_msg (W : world) (who : string) (g : nat) (b a' b' : ty) : string :=
  who ++ " needs " ++ bq (sym_name W g) ++ " and " ++ qty W b ++
  " to be one type, which its context merged (D43), but here they are " ++ qty W a' ++ " and " ++ qty W b'.
Definition call_theta (W : world) (who : string) (C Cs : cx) : er subst :=
  let th := map (fun q => let r := fst q in
               (r, if bound_in C r then read_here C r else
                   match canon Cs (TAbs r) with
                   | TAbs z => class_value C Cs z
                   | t => canon C (app_subst (map (fun z => (z, class_value C Cs z)) (gens t)) t)
                   end)) (c_ty Cs) in
  match min_first (filter (fun q => negb (ty_eqb (canon C (app_subst th (TAbs (fst q)))) (canon C (app_subst th (snd q)))))
             (c_sub Cs)) with
  | Some (g, b) => Err (merges_msg W who g b (canon C (app_subst th (TAbs g))) (canon C (app_subst th b)))
  | None => Ok th
  end.

(* Rule B (Audit's InForceThrough / RequirementsInForce).
   SOUND: in_force_through C Cs r = true <-> InForceThrough C Cs r
   (the witness y of the first disjunct is bound in C, so it ranges over
   C's table) *)
Definition in_force_through (C Cs : cx) (r : nat) : bool :=
  let cr := canon Cs (TAbs r) in
  existsb (fun q => ty_eqb (canon Cs (TAbs (fst q))) cr) (c_ty C) ||
  (match cr with TAbs _ => false | _ => true end && forallb (bound_in C) (gens cr)).
(* SOUND: requirements_in_force C Cs = true -> RequirementsInForce C Cs *)
Definition requirements_in_force (C Cs : cx) : bool :=
  forallb (fun q => in_force_through C Cs (fst q)) (c_ty Cs).
Definition rule_b (W : world) (who : string) (C Cs : cx) : er unit :=
  match find (fun q => negb (in_force_through C Cs (fst q))) (c_ty Cs) with
  | Some (r, _) => Err (who ++ " needs " ++ bq (sym_name W r) ++
                        " in force here (assume or bind it, or a type its context merges it with)")
  | None => Ok tt
  end.

(* Satisfies' clauses, each decided, through indexes of C's and Cs's
   provisions (ix, ixs). *)
Definition sat_provides (C : cx) (ix : pix) (th : subst) (ns : list item) : bool :=
  forallb (fun k => providesb_i C ix (at_th C th k)) ns.
Definition sat_coherent (C : cx) (ix ixs : pix) (th : subst) (ns : list item) : bool :=
  forallb (fun k =>
    match capp_i ixs k with
    | Some a =>
        forallb (fun q => match snd q with
          | Some want =>
              match capp_i ix (at_th C th k) with
              | Some have => existsb (fun h => Nat.eqb (fst h) (fst q) &&
                                       oty_eqb (snd h) (Some (canon C (app_subst th want)))) have
              | None => true
              end
          | None => true end) a
    | None => true
    end) ns.
Definition sat_types (C : cx) (ix ixs : pix) (th : subst) (ns : list item) : bool :=
  forallb (fun k =>
    match pfind ixs k with
    | Some p =>
        match pfind ix (at_th C th k) with
        | Some p' => ofsig_eqb (option_map (canon_sig C) (provision_signature p'))
                               (option_map (sig_at C th) (provision_signature p))
        | None => false
        end
    | None => true
    end) ns.
Definition sat_merge1 (C Cs : cx) (q : nat * ty) : bool :=
    let g := fst q in
    let cg := canon Cs (TAbs g) in
    let xs := g :: gens cg in
    let members := flat_map (fun x => class_of Cs (canon Cs (TAbs x))) xs in
    if forallb (bound_in C) members then
      ty_eqb (canon C (app_subst (c_ty C) (TAbs g))) (canon C (app_subst (c_ty C) cg))
    else true.
Definition sat_merges (C Cs : cx) : bool := forallb (sat_merge1 C Cs) (c_sub Cs).

Definition sat_map (C Cs : cx) (ix ixs : pix) (th : subst) (ns : list item) : smap :=
  map (fun k => (rkof_i Cs ixs k, rkof_i C ix (at_th C th k))) ns.

(* SOUND: satisfies C Cs th ns = Ok sm -> Satisfies C Cs th ns sm.
   `who` names the use in messages ("calling `f`"); `whot` the use for
   a type need ("assuming `f`"). *)
Definition sat_provides1 (C : cx) (ix : pix) (th : subst) (k : item) : bool := providesb_i C ix (at_th C th k).
Definition sat_coherent1 (C : cx) (ix ixs : pix) (th : subst) (k : item) : bool := sat_coherent C ix ixs th [k].
Definition sat_types1 (C : cx) (ix ixs : pix) (th : subst) (k : item) : bool := sat_types C ix ixs th [k].
Definition satisfies_gen (cap : bool) (W : world) (who whot : string) (C Cs : cx) (th : subst) (ns : list item) : er smap :=
  (* small need lists: a one-bucket index (a linear scan) beats building one *)
  let small := Nat.ltb (List.length ns) 12 in
  let ix := pix_of (negb small) (c_provisions C) in
  let ixs := pix_of (negb small) (c_provisions Cs) in
  (* the merges first, as the old oracle's require_merges *)
  let! _ := match min_first (filter (fun q => negb (sat_merge1 C Cs q)) (c_sub Cs)) with
            | Some (g, _) =>
                let cg := canon Cs (TAbs g) in
                Err (merges_msg W who g cg (canon C (app_subst (c_ty C) (TAbs g))) (canon C (app_subst (c_ty C) cg)))
            | None => Ok tt end in
  (* a type need is reported first, as the old oracle checked an item's
     requirements before its needs *)
  let missing := filter (fun k => negb (sat_provides1 C ix th k)) ns in
  let! _ := match filter (fun k => match at_th C th k with ITy _ => true | _ => false end) missing ++ missing with
            | k :: _ =>
                match at_th C th k with
                | ITy t => Err (whot ++ " needs " ++ bq (sym_name W t) ++ " in force (D20)")
                | IFun u =>
                    (* a functor's fixed inputs are its requirements: name
                       one not in force, as for a type need *)
                    match find (fun x => negb (bound_in C x))
                               (map fst (match capp_i ixs k with Some a => a | None => [] end)) with
                    | Some x => Err (whot ++ " needs " ++ bq (sym_name W x) ++ " in force (D20)")
                    | None => Err (whot ++ " needs " ++ bq (sym_name W u) ++ ", which is not available in the context here")
                    end
                | k' => Err (who ++ " needs " ++ qkey W k' ++ ", which is not available in the context here")
                end
            | [] => Ok tt end in
  let frozen k := ("captured " ++ qkey W (at_th C th k) ++ " has another frozen interpretation than required (D84 3b)")%string in
  let! _ := match find (fun k => negb (sat_coherent1 C ix ixs th k)) ns with
            | Some k => Err (if cap then frozen k else
                             who ++ " needs " ++ qkey W (at_th C th k) ++
                             " at another instantiation than the one in the context here was provided at " ++
                             "(a provision serves only the instantiation it was made at; D62)")
            | None => Ok tt end in
  let! _ := match find (fun k => negb (sat_types1 C ix ixs th k)) ns with
            | Some k =>
                let want := match pfind ixs k with
                            | Some p => match provision_signature p with Some g => (" " ++ show_sig W (sig_at C th g))%string | None => "" end
                            | None => "" end in
                let have := match pfind ix (at_th C th k) with
                            | Some p => match provision_signature p with Some g => show_sig W (canon_sig C g) | None => "another type" end
                            | None => "another type" end in
                Err (if cap then frozen k else
                     who ++ " needs " ++ qkey W (at_th C th k) ++ want ++ ", but the context here provides it at another type, " ++ have ++ " (D20)")
            | None => Ok tt end in
  Ok (sat_map C Cs ix ixs th ns).
Definition satisfies := satisfies_gen false.
