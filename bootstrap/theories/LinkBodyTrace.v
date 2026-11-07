(* Exact executable stages of the fresh link-body path. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound LinkPins.
Import ListNotations.
Local Open Scope string_scope.
Local Infix "+++" := List.app (right associativity, at level 60).
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

Definition link_trace_capture_guard (fg bg : gsym) (ef eg : env) (fneeds gneeds : list key) (xf : list nat) :=
let mi := gs_mod bg in
let argtys := filter (fun t => negb (nmem t xf)) (map fst (en_tn ef)) in
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
                      end) gneeds.

Definition link_trace_context (bg : gsym) (ef : env) (xf rts : list nat) (dr : list key) (output_roles : list (nat * nat)) :=
let mi := gs_mod bg in
let mf := filter (fun g => negb (nmem g (rts +++ map snd output_roles)))
                       (flat_map (fun k => match k with KyMth r _ => ety_gens r | _ => [] end) dr
                        +++ flat_map (fun k => match key_sig ef k with
                                              | Some (ps, r) => flat_map ety_gens (r :: ps)
                                              | None => [] end) dr) in
      (* m4, D84 3d: pins may not change a type of P(F) ∩ M(F); X(F) may be
         pinned (a manifest equation, checked at the bind) *)
       Cx mi (FSig [] YUnit None) None 0 [] "" [] (filter (fun t => negb (nmem t xf)) mf).

Definition link_trace_made_types (eb : env) (output_roles : list (nat * nat)) (xf : list nat) :=
map (fun p => (snd p,canon eb (opt_default (YAbs (fst p)) (tn_get eb (fst p))))) output_roles +++
                      map (fun t => (t,canon eb (opt_default (YAbs t) (tn_get eb t)))) xf.

Definition link_trace_body_key (eb : env) (made_tys : list (nat * ety)) (k : key) :=
match k with KyMth r m =>
        KyMth (canon eb (instantiate (env_with_edges made_tys []) r)) m | _ => k end.

Definition link_trace_renames (ef' eb : env) (output_roles : list (nat * nat)) (made_tys : list (nat * ety)) (dr : list key) :=
mmap (fun k => match k, link_trace_body_key eb made_tys k with
                                     | KyMth r m, KyMth r' _ =>
                                         let! t' := ety_ty r' in
                                         let! checked := key_item_at ef' k in
                                         let it := output_role_item output_roles checked in
                                         match it with
                                         | IMth t0 _ th0 => ret [(t', m, (t0, th0))]
                                         | _ => ret []
                                         end
                                     | _, _ => ret []
                                     end) dr.

Definition link_trace_rebinds (binds : list xbind) (renames : list (ty * nat * (ty * subst))) :=
flat_map (fun b => match b with
                                      | XBMth t m th f' sm =>
                                          b :: flat_map (fun q => let '(t', m', (t0, th0)) := q in
                                                                  if ty_eqb t t' && Nat.eqb m m' &&
                                                                     negb (item_eqb (IMth t0 m th0) (IMth t m th))
                                                                  then [XBMth t0 m th0 f' sm] else [])
                                                        renames
                                      | _ => [b]
                                      end) binds.

Definition link_trace_types_guard (fg bg : gsym) (eg eb : env) (rts : list nat) :=
let mi := gs_mod bg in
miter (fun t =>
               let made := match nassoc t (en_tn eb), nassoc t (en_tn eg) with
                           | Some (_, s1), Some (_, s0) => negb (Nat.eqb s1 s0)
                           | Some _, None => true
                           | None, _ => false
                           end in
               if made then ret tt
               else let! tg := sym t in
                    err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, does not provide the type `" ++ gs_name tg ++ "` (D78)")) rts.

Definition link_trace_items_guard (fg bg : gsym) (eg eb : env) (made_tys : list (nat * ety)) (dr : list key) :=
let mi := gs_mod bg in
miter (fun k0 =>
               let k := link_trace_body_key eb made_tys k0 in
               let made := match get_app eb k, get_app eg k with
                           | Some (_, s1), Some (_, s0) => negb (Nat.eqb s1 s0)
                           | Some _, None => true
                           | _, _ => match k with KyFun u => is_some (nget (en_funs eb) u) | _ => false end
                           end in
               let! sk := show_key k0 in
               if negb made then
                 err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, does not provide " ++ sk ++ " (D78)")
               else ret tt) dr.

Definition link_trace_readings_guard (fg bg : gsym) (ef' eb : env) (made_tys : list (nat * ety)) (dr : list key) :=
let mi := gs_mod bg in
miter (fun k =>
               (match k with
                | KyFun _ =>
                    if result_functor_fixed_ok eb ef' made_tys (link_trace_body_key eb made_tys k) k then ret tt
                    else let! sk := show_key k in
                         err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, provides " ++ sk ++
                                 " at another fixed interpretation than promised (D78)")
                | _ => ret tt end) ;;
               match key_sig eb (link_trace_body_key eb made_tys k), key_sig ef' k with
               | Some (p1, r1), Some (p2, r2) =>
                   if Nat.eqb (List.length p1) (List.length p2) &&
                      forallb (fun q => ety_eqb (canon eb (fst q)) (promised_result_type eb made_tys (snd q)))
                              (combine (r1 :: p1) (r2 :: p2))
                   then ret tt
                   else let! sk := show_key k in
                        err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, provides " ++ sk ++
                                " at another type than `" ++ gs_name fg ++ "` promises (D78)")
               | _, _ => ret tt
               end) dr.

Definition link_trace_finish (fg bg : gsym) (ef ef' eb : env) (rts : list nat) (dr : list key) (output_roles : list (nat * nat)) (binds : list xbind) :=
let mi := gs_mod bg in
let owner := ROLink (gs_id fg) (gs_id bg) in
record_reify_exports_at owner rts dr ef ef' eb output_roles ;;
      let! h := new_sym mi (gs_name bg) KFunctor (gs_decl bg) (OLink (gs_id fg) (gs_id bg)) in
      let! dri := mmap (fun k => let! it := key_item_at ef' k in ret (output_role_item output_roles it)) dr in
      upd (s_funs (fun l => (h, (map ITy rts +++ dri, binds)) :: l)) ;;
      upd (s_links (fun l => ((gs_id fg, gs_id bg), h) :: l)) ;;
      ret h.

Definition link_trace_after_body (fg bg : gsym) (ef eg ef' eb : env) (xf rts : list nat) (dr : list key) (output_roles : list (nat * nat)) (xs : list xstmt) :=
let! binds := mmap (fun x => match stmt_bind x with
 | Some b => ret b | None => fail "internal: a functor body made a statement" end) xs in
check_provider_effects (gs_mod bg) binds ;;
let made_tys := link_trace_made_types eb output_roles xf in
let! renames := link_trace_renames ef' eb output_roles made_tys dr in
let binds := link_trace_rebinds binds (List.concat renames) in
link_trace_types_guard fg bg eg eb rts ;;
link_trace_items_guard fg bg eg eb made_tys dr ;;
link_trace_readings_guard fg bg ef' eb made_tys dr ;;
link_trace_finish fg bg ef ef' eb rts dr output_roles binds.

Definition link_trace_after_capture (fuel : nat) (fg bg : gsym) (ef eg : env) (gneeds : list key) (xf rts : list nat) :=
let! (ef',dr,roles) := fresh_target_output_view ef fg in
let owner := ROLink (gs_id fg) (gs_id bg) in
register_reify_code owner eg gneeds None ;;
let! (xs,eb,_) := in_reify_code owner
 (with_vars [] (apply_body fuel (link_trace_context bg ef' xf rts dr roles) eg bg)) in
check_link_pins (gs_mod bg) eb (cx_pins (link_trace_context bg ef' xf rts dr roles)) ;;
link_trace_after_body fg bg ef eg ef' eb xf rts dr roles xs.

Definition link_trace_fresh (fuel : nat) (fg bg : gsym) :=
let! (ef,fneeds) := fun_env fg in
let! (eg,gneeds) := fun_env bg in
let! xf := functor_fixed fg in
let! rts := result_types fg in
link_trace_capture_guard fg bg ef eg fneeds gneeds xf ;;
link_trace_after_capture fuel fg bg ef eg gneeds xf rts.

Lemma link_body_fresh_path : forall fuel fg bg s h final,
 assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b))
  (gs_id fg,gs_id bg) (st_links s) = None ->
 link_body (S fuel) fg bg s = Good (h,final) ->
 link_trace_fresh fuel fg bg s = Good (h,final).
Proof.
 intros fuel fg bg s h final Miss Run.
 cbn [link_body] in Run.
 apply bind_inv in Run as [initial [s0 [Get Run]]].
 injection Get as <- <-. rewrite Miss in Run.
 exact Run.
Qed.

Theorem link_body_fresh_inputs : forall fuel fg bg s h final,
 assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b))
  (gs_id fg,gs_id bg) (st_links s) = None ->
 link_body (S fuel) fg bg s = Good (h,final) ->
 exists ef fneeds sf eg gneeds sg xf sx rts sr sc,
 fun_env fg s = Good ((ef,fneeds),sf) /\
 fun_env bg sf = Good ((eg,gneeds),sg) /\
 functor_fixed fg sg = Good (xf,sx) /\
 result_types fg sx = Good (rts,sr) /\
 link_trace_capture_guard fg bg ef eg fneeds gneeds xf sr = Good (tt,sc) /\
 link_trace_after_capture fuel fg bg ef eg gneeds xf rts sc = Good (h,final).
Proof.
 intros fuel fg bg s h final Miss Run.
 apply (link_body_fresh_path fuel fg bg s h final Miss) in Run.
 unfold link_trace_fresh in Run.
 apply bind_inv in Run as [[ef fneeds] [sf [EF Run]]].
 apply bind_inv in Run as [[eg gneeds] [sg [EG Run]]].
 apply bind_inv in Run as [xf [sx [XF Run]]].
 apply bind_inv in Run as [rts [sr [RT Run]]].
 apply bind_inv in Run as [[] [sc [Capture Run]]].
 exists ef,fneeds,sf,eg,gneeds,sg,xf,sx,rts,sr,sc.
 repeat split;assumption.
Qed.

Theorem link_body_fresh_body : forall fuel fg bg ef eg gneeds xf rts s h final,
 link_trace_after_capture fuel fg bg ef eg gneeds xf rts s = Good (h,final) ->
 exists promised dr roles sv registered xs eb cb bodyFinal,
 fresh_target_output_view ef fg s = Good ((promised,dr,roles),sv) /\
 register_reify_code (ROLink (gs_id fg) (gs_id bg)) eg gneeds None sv = Good (tt,registered) /\
 in_reify_code (ROLink (gs_id fg) (gs_id bg))
  (with_vars [] (apply_body fuel (link_trace_context bg promised xf rts dr roles) eg bg))
  registered = Good ((xs,eb,cb),bodyFinal) /\
 link_trace_after_body fg bg ef eg promised eb xf rts dr roles xs bodyFinal = Good (h,final).
Proof.
 intros fuel fg bg ef eg gneeds xf rts s h final Run.
 unfold link_trace_after_capture in Run.
 apply bind_inv in Run as [[[promised dr] roles] [sv [View Run]]].
 apply bind_inv in Run as [[] [registered [Register Run]]].
 apply bind_inv in Run as [[[xs eb] cb] [bodyFinal [Body Run]]].
 apply bind_inv in Run as [[] [checked [Pins Run]]].
 apply check_link_pins_state in Pins;subst checked.
 exists promised,dr,roles,sv,registered,xs,eb,cb,bodyFinal.
 repeat split;assumption.
Qed.

Theorem link_body_fresh_pin_receipt : forall fuel fg bg ef eg gneeds xf rts s h final,
 link_trace_after_capture fuel fg bg ef eg gneeds xf rts s = Good (h,final) ->
 exists promised dr roles sv registered xs eb cb bodyFinal,
 fresh_target_output_view ef fg s = Good ((promised,dr,roles),sv) /\
 register_reify_code (ROLink (gs_id fg) (gs_id bg)) eg gneeds None sv = Good (tt,registered) /\
 in_reify_code (ROLink (gs_id fg) (gs_id bg))
  (with_vars [] (apply_body fuel (link_trace_context bg promised xf rts dr roles) eg bg))
  registered = Good ((xs,eb,cb),bodyFinal) /\
 check_link_pins (gs_mod bg) eb (cx_pins (link_trace_context bg promised xf rts dr roles)) bodyFinal = Good (tt,bodyFinal) /\
 link_trace_after_body fg bg ef eg promised eb xf rts dr roles xs bodyFinal = Good (h,final).
Proof.
 intros fuel fg bg ef eg gneeds xf rts s h final Run.
 unfold link_trace_after_capture in Run.
 apply bind_inv in Run as [[[promised dr] roles] [sv [View Run]]].
 apply bind_inv in Run as [[] [registered [Register Run]]].
 apply bind_inv in Run as [[[xs eb] cb] [bodyFinal [Body Run]]].
 apply bind_inv in Run as [[] [checked [Pins Run]]].
 pose proof(check_link_pins_state _ _ _ _ _ Pins)as State;subst checked.
 exists promised,dr,roles,sv,registered,xs,eb,cb,bodyFinal.
 repeat split;assumption.
Qed.

Theorem link_body_fresh_checks : forall fg bg ef eg promised eb xf rts dr roles xs s h final,
 link_trace_after_body fg bg ef eg promised eb xf rts dr roles xs s = Good (h,final) ->
 exists raw sb se renames sn st si ss,
 mmap (fun x => match stmt_bind x with
  | Some b => ret b | None => fail "internal: a functor body made a statement" end) xs s = Good (raw,sb) /\
 check_provider_effects (gs_mod bg) raw sb = Good (tt,se) /\
 link_trace_renames promised eb roles (link_trace_made_types eb roles xf) dr se = Good (renames,sn) /\
 link_trace_types_guard fg bg eg eb rts sn = Good (tt,st) /\
 link_trace_items_guard fg bg eg eb (link_trace_made_types eb roles xf) dr st = Good (tt,si) /\
 link_trace_readings_guard fg bg promised eb (link_trace_made_types eb roles xf) dr si = Good (tt,ss) /\
 link_trace_finish fg bg ef promised eb rts dr roles (link_trace_rebinds raw (List.concat renames)) ss = Good (h,final).
Proof.
 intros fg bg ef eg promised eb xf rts dr roles xs s h final Run.
 unfold link_trace_after_body in Run.
 apply bind_inv in Run as [raw [sb [Binds Run]]].
 apply bind_inv in Run as [[] [se [Effects Run]]].
 apply bind_inv in Run as [renames [sn [Renames Run]]].
 apply bind_inv in Run as [[] [st [Types Run]]].
 apply bind_inv in Run as [[] [si [Items Run]]].
 apply bind_inv in Run as [[] [ss [Readings Run]]].
 exists raw,sb,se,renames,sn,st,si,ss.
 repeat split;assumption.
Qed.

Theorem link_body_fresh_emission : forall fg bg ef promised eb rts dr roles binds s h final,
 link_trace_finish fg bg ef promised eb rts dr roles binds s = Good (h,final) ->
 exists exported allocated dri indexed,
 record_reify_exports_at (ROLink (gs_id fg) (gs_id bg)) rts dr ef promised eb roles s = Good (tt,exported) /\
 new_sym (gs_mod bg) (gs_name bg) KFunctor (gs_decl bg) (OLink (gs_id fg) (gs_id bg)) exported = Good (h,allocated) /\
 mmap (fun k => let! it := key_item_at promised k in ret (output_role_item roles it)) dr allocated = Good (dri,indexed) /\
 final = s_links (fun l => ((gs_id fg,gs_id bg),h)::l)
  (s_funs (fun l => (h,(map ITy rts +++ dri,binds))::l) indexed) /\
 st_funs final = (h,(map ITy rts +++ dri,binds))::st_funs indexed /\
 st_links final = ((gs_id fg,gs_id bg),h)::st_links indexed.
Proof.
 intros fg bg ef promised eb rts dr roles binds s h final Run.
 unfold link_trace_finish in Run.
 apply bind_inv in Run as [[] [exported [Exports Run]]].
 apply bind_inv in Run as [fresh [allocated [Allocate Run]]].
 apply bind_inv in Run as [dri [indexed [Index Run]]].
 apply bind_inv in Run as [[] [sf [Funs Run]]].
 unfold upd in Funs;injection Funs as <-.
 apply bind_inv in Run as [[] [sl [Links Run]]].
 unfold upd in Links;injection Links as <-.
 injection Run as <- <-.
 exists exported,allocated,dri,indexed;repeat split;assumption||reflexivity.
Qed.
