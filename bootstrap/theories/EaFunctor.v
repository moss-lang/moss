(* ------------------------------------------------------------------------ *)
(* EaFunctor.v — the functor rules (Audit V.5/V.6, D78/D84): BApplyAbstract,*)
(* BFunCopy with CopyInterpretation, BFunClosure with LinkBody.             *)
(*                                                                           *)
(* These are the rules the lang-b/lang-q branches rewrite (design §2.4,    *)
(* FUNCTOR_RULES).  Each is one function taking the open-recursion record  *)
(* R (LinkBody elaborates G's binds through R.r_bind_items) and returning  *)
(* the BindItem's wrap, context, frame and made generations, so a change   *)
(* to one rule is a local edit here.                                       *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaCall EaFamily EaDecide EaCtx EaLinkSite.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition copy_need_candidates (C : cx) (rho : subst) (needs : list item)
    (k : item) (p : provision) : list item :=
  let key := copy_need_key C rho k in
  let required := provision_subst rho (canon_provision C p) in
  filter (fun k' => item_eqb key (copy_need_key C rho k') &&
    match ilook (provision_id k') (c_provisions C) with
    | Some p' => provision_eqb required (provision_subst rho (canon_provision C p'))
    | None => false
    end) needs.

Definition choose_copy_need (E : env) (source target : string) (Cu Ch : cx)
    (rho : subst) (needs : list item) (k : item) (p : provision) : er item :=
  match copy_need_candidates Cu rho needs k p with
  | [k'] => Ok k'
  | [] =>
      if existsb (fun k' => item_eqb (copy_need_key Cu rho k) (copy_need_key Cu rho k')) needs then
        Err (source ++ " cannot be copied to " ++ target ++ ": its input interpretation differs (D78)")
      else Err (source ++ " cannot be bound to " ++ target ++ ": it needs " ++ qkey (e_W E) k ++
                ", which " ++ target ++ " does not take (D78)")
  | candidates =>
      let ambiguous := (source ++ " cannot be copied to " ++ target ++ ": its need " ++ qkey (e_W E) k ++
                        " matches more than one target provision at the copy site (D78)")%string in
      match EaCtx.merge E (merge_equations (c_merge Cu) ++ merge_equations (c_merge Ch)) with
      | Ok (_, M) =>
          match filter (item_eqbM M k) candidates with
          | [k'] => Ok k'
          | _ => Err ambiguous
          end
      | Err _ => Err ambiguous
      end
  end.

Section Functor.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.
Variable R : rec.

Definition gen_of (tbl : list (nat * nat)) (t : nat) : nat :=
  match nlook t tbl with Some g => g | None => t end.
Definition fresh_table (u : nat) (l : list nat) : M (list (nat * nat)) :=
  mmap (fun t => let* g := fresh in let* _ := put_name g (sym_name W t ++ "@" ++ sym_name W u) in ret (t, g)) l.

Definition fixed_here (u : nat) (C : cx) : subst :=
  flat_map (fun x => match nlook x (c_ty C) with Some t => [(x, t)] | None => [] end) (fixed W metadata_fuel u).

Definition abstract_functor (u : nat) : bool :=
  match ast_of W u with Some (ADFunctor _ _ _ _ None) => true | _ => false end.

(* the binding list a wrap puts around EUnit (BindsOf) *)
Fixpoint binds_of (e : expr) : option (list binding) :=
  match e with
  | EUnit => Some []
  | EBind b rest => option_map (cons b) (binds_of rest)
  | _ => None
  end.

(* the old oracle's report of a provider whose result is not F's *)
Definition leaf_name (l : nat * option name) : string :=
  bq (sym_name W (fst l) ++ match snd l with Some dn => "." ++ dn | None => "" end).
Definition result_mismatch (u g : nat) : string :=
  let a := result_leaves W fuel u in let b := result_leaves W fuel g in
  let missing := filter (fun k => negb (existsb (leaf_eqb k) b)) a in
  let more := filter (fun k => negb (existsb (leaf_eqb k) a)) b in
  match missing with
  | _ :: _ => qs W g ++ " cannot be bound to " ++ qs W u ++ ": it does not provide " ++
              String.concat ", " (map leaf_name missing) ++ ", which " ++ qs W u ++ " promises (D78)"
  | [] => qs W g ++ " cannot be bound to " ++ qs W u ++ ": it also provides " ++
          String.concat ", " (map leaf_name more) ++ ", which " ++ qs W u ++
          " does not; a functor bound to an abstract one provides exactly its result (D78)"
  end.

(* ---- BApplyAbstract ---- *)

Definition allocate_output (C : cx) (u : nat) (mentioned retg : list nat)
    (t : nat) : M (nat * output_identity) :=
  if bound_in C t || nmem t mentioned then
    let* g := fresh in let* _ := put_name g (sym_name W t ++ "@" ++ sym_name W u) in
    ret (t, GeneratedOutput g)
  else if nmem t retg then
    fail ("applying " ++ qs W u ++ " makes " ++ qs W t ++
          ", which the enclosing function's types mention (D78)")
  else ret (t, DeclaredOutput).

Definition allocate_application_outputs (F : frame) (C : cx) (u : nat)
    (outs : list nat) : M (list (nat * output_identity)) :=
  let retg := match fr_ret F with Some r => gens r | None => [] end ++
              match fr_this F with Some r => gens r | None => [] end in
  let mentioned := context_gens C ++ key_receiver_gens C in
  mmap (allocate_output C u mentioned retg) outs.

(* SOUND: apply_abstract F C pa u = Ok (wrap, C', F', made) ->
     BindItem W P F C (Spec (Some pa) None None, None) wrap C' F' made
   by BApplyAbstract (u = resolve_sym W (fr_mod F) pa). *)
Definition apply_abstract (F : frame) (C : cx) (u : nat) : M (wrapper * cx * frame * list nat) :=
  let fu := qs W u in
  let* fx := lift (cfun C u) (fu ++ " is not available in the context here: nothing binds it, and nothing here assumes it (D78)") in
  let* (Cu, ns) := lift (w_context W u) ("the context of " ++ fu ++ " does not form") in
  let* _ := match find (fun q => negb (bound_in C (fst q))) (c_ty Cu) with
            | Some (t, _) => fail ("applying " ++ fu ++ " needs " ++ qs W t ++ ", which is not available in the context here")
            | None => ret tt end in
  let th := map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cu) in
  let* _ := match find (fun q => match nlook (fst q) (c_ty C) with
                                 | Some here => negb (ty_eqb (canon C here) (canon C (snd q)))
                                 | None => true end) fx with
            | Some (x, was) =>
                fail ("applying " ++ fu ++ " here reads " ++ qs W x ++ " as " ++
                      match nlook x (c_ty C) with Some here => qty W (canon C here) | None => "nothing" end ++
                      ", but " ++ fu ++ " is fixed in it and was provided where it was " ++ qty W (canon C was) ++
                      " (D84 rule 5, D72)")
            | None => ret tt end in
  let* sm := pure (satisfies W ("applying " ++ fu) ("applying " ++ fu) C Cu th ns) in
  let outs := fun_outputs W metadata_fuel u in
  let* tbl := allocate_application_outputs F C u outs in
  let newgens := allocated_output_generations tbl in
  let gensof := output_generation_at tbl in
  let* (C', ks) := pure (provide_result E u C (fun t => TAbs (gensof t))) in
  let* (_, ksf) := pure (provide_result E u Cu (fun t => TAbs t)) in
  let* _ := check (collides_only_as ks ksf) ("applying " ++ fu ++ " provides two of its result items under one key here (D84 rule 7)") in
  ret ((fun x => apply_output_expression u th (map (fun t => (t, gensof t)) outs) sm (map (rkof C') ks) x),
       C', with_fresh F newgens, map gensof outs).

(* ---- BFunCopy and CopyInterpretation ---- *)

(* SOUND: copy_interpretation sigma u h Cu nsu nsh Ch = Ok (sm, rm)
   yields the chosen target keys and CopyInterpretation. *)
Definition copy_interpretation (sigma : subst) (u h : nat) (Cu : cx) (nsu nsh : list item) (Ch : cx)
    : M (smap * list nat) :=
  let nu := qs W u in let nh := qs W h in
  let outs := fun_outputs W metadata_fuel u in
  let fixed := copy_fixed_readings Cu sigma in
  let ys := uniq (filter (fun y => negb (is_some (lookup_subst y fixed)))
                         (flat_map (fun q => match nlook (fst q) (c_ty Cu) with Some tx => gens tx | None => [] end) (c_ty Cu))) in
  let* gtab := fresh_table u outs in
  let* ctab := fresh_table u ys in
  let gen := gen_of gtab in
  let rho := copy_subst fixed ys (gen_of ctab) in
  let* _ := check (forallb (fun q => match nlook (fst q) (c_ty Ch) with
                     | Some tx => ty_eqb (app_subst rho (canon Cu (TAbs (fst q)))) (app_subst rho (canon Cu tx)) | None => true end) (c_ty Ch))
             (nh ++ " cannot be copied to " ++ nu ++ ": its input interpretation differs (D78)") in
  let input_error := (nh ++ " cannot be copied to " ++ nu ++ ": its input interpretation differs (D78)")%string in
  let* needs := mmap (fun k =>
    let* p := lift (ilook (provision_id k) (c_provisions Ch)) input_error in
    let* k' := pure (choose_copy_need E nh nu Cu Ch rho nsu k p) in
    ret (k, k')) nsh in
  let* (Cr, ks) := pure (provide_result E u Cu (fun t => TAbs (gen t))) in
  let* (Cr', ks') := pure (provide_result E h Ch (fun t => TAbs (gen t))) in
  let at_rho := fun k => app_subst_item rho (provision_id k) in
  let at_rho_h := fun k => app_subst_item rho (canon_item Cu (provision_id k)) in
  let ix' := combine (seq 0 (List.length ks')) ks' in
  let* rm := mmap (fun k =>
    match rev (filter (fun q => item_eqb (at_rho_h (snd q)) (at_rho k)) ix') with
    | (j, k') :: _ =>
        let* _ := check (forallb (fun q => item_eqb (provision_id (snd q)) (provision_id k')) (filter (fun q => item_eqb (at_rho_h (snd q)) (at_rho k)) ix'))
                   (nh ++ " cannot be copied to " ++ nu ++ ": two of its result items fall under one key of " ++ nu ++ "'s (D84 rule 7)") in
        match ilook (provision_id k) (c_provisions Cr), ilook (provision_id k') (c_provisions Cr') with
        | Some p, Some p' =>
            let* _ := check (provision_eqb (provision_subst rho p) (provision_subst rho (canon_provision Cu p')))
                       (nh ++ " cannot be copied to " ++ nu ++ ": its effective output interpretation differs (D78)") in
            ret j
        | _, _ => fail (nh ++ " cannot be copied to " ++ nu ++ ": its effective output interpretation differs (D78)")
        end
    | [] => fail (nh ++ " cannot be bound to " ++ nu ++ ": it does not provide " ++ qkey W k ++ ", which " ++ nu ++ " promises (D78)")
    end) ks in
  ret (map (fun q => (rkof Ch (fst q), rkof Cu (snd q))) needs, rm).

(* SOUND: fun_copy F C u h = Ok (wrap, C') -> BindItem ... by BFunCopy *)
Definition fun_copy (F : frame) (C : cx) (u h : nat) : M (wrapper * cx) :=
  let nu := qs W u in let nh := qs W h in
  let* _ := check (abstract_functor u) (nu ++ " has a body, so it is applied (`bind " ++ sym_name W u ++ ";`), not bound (D78)") in
  let* _ := lift (cfun C h) (nh ++ " is not available in the context here, so it cannot provide a bind (assume or bind it)") in
  let* (Cu, nsu) := lift (w_context W u) ("the context of " ++ nu ++ " does not form") in
  let* (Ch, nsh) := lift (w_context W h) ("the context of " ++ nh ++ " does not form") in
  let fu := fixed W metadata_fuel u in
  let* _ := match find (fun x => negb (bound_in C x)) fu with
            | Some x => fail ("binding " ++ nu ++ " needs " ++ qs W x ++ " in force: it is a fixed input (D84)")
            | None => ret tt end in
  let* _ := match find (fun x => negb (nmem x fu)) (fixed W metadata_fuel h) with
            | Some x => fail (nh ++ " cannot be bound to " ++ nu ++ ": it is fixed in " ++ qs W x ++ ", in which " ++ nu ++ " is polymorphic (D84 rule 4)")
            | None => ret tt end in
  let* _ := check (fixed_agreement E C h) ("copying " ++ nh ++ " after rebinding its fixed input changes its interpretation (D84)") in
  let* _ := check (same_result W fuel u h) (result_mismatch u h) in
  let fx := fixed_here u C in
  let* _ := check (forallb (fun q => bound_in Cu (fst q)) (c_ty Ch)) (nh ++ " cannot be copied to " ++ nu ++ ": it takes an input " ++ nu ++ " does not (D78)") in
  let* (sm, rm) := copy_interpretation (map (fun q => (fst q, canon C (snd q))) fx) u h Cu nsu nsh Ch in
  ret ((fun x => copy_functor_expression u h sm rm x), set_fun C u fx).

(* ---- LinkBody and BFunClosure ---- *)

Definition provision_refinesb (promised actual : provision) : bool :=
  match promised, actual with
  | ProvidedVal _ _, ProvidedVal _ _ | ProvidedFn _ _, ProvidedFn _ _
  | ProvidedMethod _ _, ProvidedMethod _ _ | ProvidedFunctor _, ProvidedFunctor _ => true
  | _, _ => false end &&
  ofsig_eqb (provision_signature promised) (provision_signature actual) &&
  forallb (fun q => match snd q with
                    | Some v => existsb (fun q' => Nat.eqb (fst q') (fst q) && oty_eqb (snd q') (Some v)) (provision_inst actual)
                    | None => true end) (provision_inst promised).

Definition linked_need (Cu : cx) (nsu : list item) (k : item) : bool :=
  imem (canon_item Cu k) nsu.

Definition link_need_scoped (C : cx) (k : item) (p : provision) : bool :=
  forallb (bound_in C) (link_need_gens k p).

Definition choose_link_target (Cu Cg : cx) (nsu : list item) (rho : subst)
    (k : item) (p : provision) (error : string) : er (option item) :=
  if link_need_scoped Cu k p && link_need_scoped Cg k p then
    if existsb (fun other => item_eqb (copy_need_key Cu rho k) (copy_need_key Cu rho other)) nsu then
      let! other := match choose_copy_need E "" "" Cu Cg rho nsu k p with
                   | Ok other => Ok other | Err _ => Err error end in
      match ilook (provision_id other) (c_provisions Cu) with
      | Some q => if link_need_scoped Cu other q then Ok (Some other) else Err error
      | None => Err error
      end
    else Ok None
  else if linked_need Cu nsu k then Err error else Ok None.

Definition link_need_choice (Cu Cg : cx) (nsu : list item) (rho : subst) (error : string) (k : item) : er (option item) :=
  match ilook (provision_id k) (c_provisions Cg) with
  | Some p => choose_link_target Cu Cg nsu rho k p error
  | None => Err error
  end.

Definition unsafe_capture (Cu Cg : cx) (target : item -> option item) (fixed : list nat) (k : item) : bool :=
  if is_linked target k then false else
  match ilook (provision_id k) (c_provisions Cg) with
  | Some p => negb (forallb (fun t => negb (bound_in Cu t) || nmem t fixed) (link_need_gens k p))
  | None => false
  end.

Definition linked_type_agrees (Cu Cg : cx) (rho private : subst) (x : nat) : bool :=
  match nlook x (c_ty Cg) with
  | Some tx => if bound_in Cu x then
      forallb (fun y => bound_in Cu y || is_some (lookup_subst y private)) (gens tx) &&
      ty_eqb (app_subst rho (canon Cu (TAbs x))) (app_subst (private ++ rho) (canon Cu tx)) else true
  | None => true
  end.

(* Check the source binding separately from lookup or generation of body code. *)
Definition check_link_binding (u gg : nat) (Cu : cx) (nsu : list item) (Cg : cx) (nsg : list item)
    (recorded private : subst) : M (item -> option item) :=
  let nu := qs W u in let ng := qs W gg in
  let bound_to := (ng ++ ", bound to " ++ nu ++ ",")%string in
  let error := (ng ++ " cannot provide " ++ nu ++ ": its linked input interpretation differs (D84)")%string in
  let fu := fixed W metadata_fuel u in
  let* state := get in
  let rho := site_theta (es_next state) Cu recorded private in
  let* choices := pure (rmap (fun k =>
    let! choice := link_need_choice Cu Cg nsu rho error k in Ok (k, choice)) nsg) in
  let target := fun k => match ilook k choices with Some choice => choice | None => None end in
  let* _ := match find (unsafe_capture Cu Cg target fu) nsg with
      | Some k => fail (bound_to ++ " captures " ++ qkey W k ++ ", which mentions a type each application of " ++ nu ++ " supplies (D84 3b)")
      | None => ret tt end in
  let* _ := check (forallb (fun q => linked_type_agrees Cu Cg rho private (fst q)) (c_ty Cg))
      (ng ++ " cannot provide " ++ nu ++ ": its input interpretation differs (D84)") in
  ret target.

(* Allocate the temporary output roles used to read the promised result. *)
Definition link_output_roles (u : nat) (Cu Cb : cx) (bound_to : string)
    : M ((list nat * list (nat * nat)) * (cx * list item)) :=
  let outs := fun_outputs W metadata_fuel u in
  let* _ := match find (fun t => negb (bound_in Cb t)) outs with
            | Some t => fail (bound_to ++ " does not provide the type " ++ qs W t ++ " (D78)")
            | None => ret tt end in
  let* gtab := fresh_table u outs in
  let* promised := pure (provide_result E u Cu (fun t => TAbs (gen_of gtab t))) in
  ret ((outs, gtab), promised).

(* SOUND: link_body u gg = Ok h -> LinkBody W P u gg (slot_index h) pins, pins the
   ones it records (get_pins h) (P final, I2) *)
Definition link_body (u gg : nat) : M (code_slot FunctorTable) :=
  let* cached := get_link (u, gg) in
  match cached with
  | Some h => ret h
  | None =>
  let nu := qs W u in let ng := qs W gg in
  let bound_to := (ng ++ ", bound to " ++ nu ++ ",")%string in
  let* dg := lift (w_decl W gg) "internal: unknown functor" in
  match d_ast dg with
  | ADFunctor _ _ _ _ (Some bodies) =>
      let* (Cu, nsu) := lift (w_context W u) ("the context of " ++ nu ++ " does not form") in
      let* (Cg, nsg) := lift (w_context W gg) ("the context of " ++ ng ++ " does not form") in
      let fu := fixed W metadata_fuel u in
      let its := List.concat bodies in
      let Fg := Frame (d_mod dg) None None false (mk_frame_vars false [] (flat_map (item_names W fuel (d_mod dg)) its))
                      [] [] (let pl := poly W metadata_fuel u in
                             (* 3d (Q6, N5 #95): P(F) ∩ M(F) *)
                             filter (fun t => nmem t pl) (mentions W metadata_fuel u)) in
      let* (((wrap, Cb), Fb), made) := r_bind_items R Fg Cg its in
      let* _ := check (forallb (fun t => negb (bound_in Cu t)) made) (bound_to ++ " makes a type " ++ nu ++ " takes as an input (D84)") in
      let* ((outs, gtab), (Cr, ks)) := link_output_roles u Cu Cb bound_to in
      let gen := gen_of gtab in
      (* Q6, N5 (#95): the pins of fixed inputs: what the body binds each
         to, unless it is G's own reading of a fixed input G takes *)
      let pins := flat_map (fun x => match nlook x (c_ty Cb) with
                              | Some v => match nlook x (c_ty Cg) with
                                          | Some w => if ty_eqb w v then [] else [(x, v)]
                                          | None => [(x, v)] end
                              | None => [] end) fu in
      let pin_ok y := bound_in Cg y && negb (is_some (lookup_subst y (c_sub Cg))) && negb (nmem y made) &&
                      (negb (bound_in Cu y) || nmem y fu) in
      let* _ := match find (fun q => negb (forallb pin_ok (gens (snd q)))) pins with
                | Some (x, v) =>
                    match find (fun y => bound_in Cu y && negb (nmem y fu)) (gens v) with
                    | Some y => fail (ng ++ " cannot be bound to " ++ nu ++ " here: it pins " ++ qs W x ++ ", which " ++ nu ++
                                      " is fixed in, to a type that mentions " ++ qs W y ++ ", a type each application of " ++ nu ++
                                      " supplies (D84 3d)")
                    | None => fail (bound_to ++ " pins " ++ qs W x ++ ", which " ++ nu ++ " is fixed in, to " ++ qty W v ++
                                    ", which mentions a type that is not one of " ++ ng ++ "'s own unmerged inputs in its actual body (Q6, D84 3d)")
                    end
                | None => ret tt end in
      (* #73: link_site reads a pinned fixed input at its pin *)
      let site := link_site Cg Cb pins outs gen in
      let site0 := link_site Cg Cb [] outs gen in
      let reads := role_reads site outs gen ++ pin_reads Cb pins in
      (* #73: a key the pins re-read must be one the body made, not one of
         G's own provisions *)
      let bad0 := find (fun k =>
          negb (item_eqb (at_site site k) (at_site site0 k)) &&
          is_some (ilook (provision_id (at_site site k)) (c_provisions Cg))) ks in
      let* _ := match bad0 with
                | Some k => fail (bound_to ++ " does not provide " ++ qkey W k ++ " at the fixed input its body pins (#73, D78)")
                | None => ret tt end in
      let bad := find (fun k => negb
          match ilook (provision_id k) (c_provisions Cr), ilook (provision_id (at_site site k)) (c_provisions site) with
          | Some required, Some actual =>
              provision_refinesb (provision_subst (c_sub site) (provision_subst reads required))
                                 (provision_subst (c_sub site) actual)
          | _, _ => false
          end) ks in
      let* _ := match bad with
                | None => ret tt
                | Some k => fail (bound_to ++ " " ++
                                  match ilook (provision_id (at_site site k)) (c_provisions site) with
                                  | None => "does not provide " ++ qkey W k ++ " (D78)"
                                  | Some _ => match k with
                                              | IFun _ => "provides " ++ qkey W k ++ " at another fixed interpretation than promised (D78)"
                                              | _ => "provides " ++ qkey W k ++ " at another type than " ++ nu ++
                                                     " promises in its actual body (D78, D84 3e)" end end)
                end in
      let* binds0 := lift (binds_of (wrap EUnit)) "internal: a functor body made a statement" in
      let* _ := if is_family E u then
                  match find (fun b => match b with BValue _ _ => true | BApply v _ _ _ _ => negb (is_family E v) | _ => false end) binds0 with
                  | Some (BValue v _) => fail (ng ++ ", bound to the family " ++ nu ++ ", binds the val " ++ qs W v ++
                                               ", so where an application is placed could be observed (D84 rule 7)")
                  | Some (BApply v _ _ _ _) => fail (ng ++ ", bound to the family " ++ nu ++ ", applies " ++ qs W v ++
                                                     ", which is not a family, so where an application is placed could be observed (D84 rule 7)")
                  | _ => ret tt end
                else ret tt in
      let code := translate_linked_body Cu Cg outs binds0 (map (fun k => rkof site (at_site site k)) ks) in
      let* h := emit_fun_code code in
      let* _ := put_pins h pins in
      let* _ := put_link (u, gg) h in
      ret h
  | _ => fail (ng ++ " has no body")
  end
  end.

Definition translate_link_targets (Cu Cg : cx) (target : item -> option item) (needs : list item) : smap :=
  map (fun k => (rkof Cg k, rkof Cu (linked_target target k))) (linked_needs target needs).

(* SOUND: fun_closure F C u gg = Ok (wrap, C') -> BindItem ... by BFunClosure *)
Definition fun_closure (F : frame) (C : cx) (u gg : nat) : M (wrapper * cx) :=
  let nu := qs W u in let ng := qs W gg in
  let* _ := check (abstract_functor u) (nu ++ " has a body, so it is applied (`bind " ++ sym_name W u ++ ";`), not bound (D78)") in
  let* (Cu, nsu) := lift (w_context W u) ("the context of " ++ nu ++ " does not form") in
  let* (Cg, nsg) := lift (w_context W gg) ("the context of " ++ ng ++ " does not form") in
  let* _ := match find (fun x => negb (bound_in C x)) (fixed W metadata_fuel u) with
            | Some x => fail ("binding " ++ nu ++ " needs " ++ qs W x ++ " in force: it is a fixed input (D84)")
            | None => ret tt end in
  let* _ := check (same_result W fuel u gg) (result_mismatch u gg) in
  let fx := fixed_here u C in
  let* target := check_link_binding u gg Cu nsu Cg nsg (map (fun q => (fst q, canon C (snd q))) fx) (link_private_readings C Cu Cg) in
  let* h := link_body u gg in
  let sm := translate_link_targets Cu Cg target nsg in
  let thg := map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg) in
  let* smc := pure (satisfies_gen true W (ng ++ ", bound to " ++ nu ++ ",") ("binding " ++ nu ++ " to " ++ ng) C Cg thg (captured_needs target nsg)) in
  (* rule B for the closure: G's inputs F does not take are in force here,
     through G's merge class *)
  let* _ := match find (fun q => negb (bound_in Cu (fst q) || in_force_through C Cg (fst q))) (c_ty Cg) with
            | Some (t, _) => fail ("binding " ++ nu ++ " to " ++ ng ++ " captures " ++ qs W t ++
                                   ", which is not in force here (assume or bind it)")
            | None => ret tt end in
  (* Q6: each pin of a fixed input agrees with the bind site *)
  let* pins := get_pins h in
  let* _ := match find (fun q => negb (
              forallb (bound_in C) (gens (snd q)) &&
              match nlook (fst q) (c_ty C) with
              | Some here => ty_eqb (canon C here) (canon C (app_subst thg (snd q)))
              | None => false end)) pins with
            | Some (x, v) =>
                fail (ng ++ " cannot be bound to " ++ nu ++ " here: it binds " ++ qs W x ++ " to " ++ qty W (canon C (app_subst thg v)) ++
                      ", and here " ++ qs W x ++ " is " ++
                      match nlook x (c_ty C) with Some here => qty W (canon C here) | None => "not in force" end ++
                      "; " ++ nu ++ " is fixed in " ++ qs W x ++ " (D84)")
            | None => ret tt end in
  ret ((fun x => bind_functor_expression u (slot_index h) (lexical_capture C) sm smc x), set_fun C u fx).

End Functor.
