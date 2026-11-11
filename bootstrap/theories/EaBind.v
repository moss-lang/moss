(* ------------------------------------------------------------------------ *)
(* EaBind.v — bind statements (Audit V.6): BindItems, BindItem (BTypeBind, *)
(* BValBind, BFnBind, BFnLambda, BMethBind, BApplyBody, and dispatch to    *)
(* EaFunctor's BApplyAbstract/BFunCopy/BFunClosure), FnProvider,           *)
(* MethProvider, LambdaProvider.                                           *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaCall EaFamily EaDecide EaFunctor.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Section Bind.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.
Variable R : rec.

(* ---- FnProvider ---- *)

(* SOUND: fn_provider F C pa sg = Ok (g, types, sm) ->
     FnProvider W P F C (AEPath pa None) sg None g types sm *)
Definition sig_mismatch (kind : string) (pv : nat) (got want : fsig) : string :=
  kind ++ " bind signature mismatch: provider " ++ qs W pv ++ " has " ++ show_sig W got ++ ", need " ++ show_sig W want ++
  (if String.eqb kind "method"
   then "; a plain function providing a method takes the method's parameters, which exclude the receiver (D54)"
   else "").

Definition read_forwarder (kind : string) (C : cx) (g : nat) (sg : fsig) : er forwarder_handle :=
  let! provided := ropt (cfn C g) (qs W g ++ " is not available in the context here, so it cannot provide a bind (assume or bind it)") in
  let! _ := rcheck (fsig_eqb provided sg) (sig_mismatch kind g provided sg) in
  Ok (ForwarderOf g).

Definition place_forwarder (handle : forwarder_handle) (sg : fsig) : M (code_slot FunctionTable) :=
  let g := forwarder_source handle in
  let* cached := get_fwd g in
  match cached with
  | Some slot => ret slot
  | None => let* slot := emit_fn_code (forwarder_body handle sg) in
            let* _ := put_fwd g slot in ret slot
  end.

Definition fn_provider (kind : string) (F : frame) (C : cx) (pa : path) (sg : fsig) : M (nat * type_capture * smap) :=
  let not_fn := (bq (show_path pa) ++ " must be a defined function to provide a bind")%string in
  let* g := lift (resolve_sym W (fr_mod F) pa) not_fn in
  let* _ := check (kind_is W g DKFn) not_fn in
  match ast_of W g with
  | Some (ADFn RPlain _ _ _ (Some _)) =>
      let who := ("binding to " ++ qs W g)%string in
      let* (Cg, ns) := lift (w_context W g) ("the context of " ++ qs W g ++ " does not form") in
      let* th := pure (call_theta W who C Cg) in
      let* sm := pure (satisfies W ("calling " ++ qs W g) who C Cg th ns) in
      let* _ := pure (rule_b W who C Cg) in
      let* g' := pure (sig_of W fuel g (c_ty C) None) in
      let want := FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg)) in
      let* _ := check (fsig_eqb g' want) (sig_mismatch kind g g' want) in
      ret (g, TypeCapture th th, sm)
  | Some (ADFn RPlain _ _ _ None) =>
      let* handle := pure (read_forwarder kind C g sg) in
      let* slot := place_forwarder handle sg in
      let input := forwarder_source handle in
      ret (slot_index slot, TypeCapture [] [], [(IFn input, IFn input)])
  | _ => fail not_fn
  end.

(* ---- LambdaProvider ---- *)

Definition captured_cx (C : cx) (caps : list name) (capvals : list nat) : cx :=
  fold_left (fun C' q => let '(n, v) := q in
                match slook n (c_locals C) with
                | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                | None => C' end)
            (combine caps capvals) (Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) []).

Definition lambda_capture_names (F : frame) (C : cx) (parameters : list name) (body : aexpr) : list name :=
  match fr_applying F with
  | [] => suniq (filter (fun n => is_some (slook n (c_locals C)) && negb (smem n parameters))
                        (mentioned_names W F body))
  | _ => []
  end.

Definition lambda_need_keys (C : cx) (index : pix) : list item :=
  filter (fun k => match k with ITy _ => false
    | _ => providesb_i C index k && item_eqb k (provision_id k)
    end) (iuniq_fast (map fst (c_provisions C))).

(* SOUND: lambda_provider F C ps rt body sg this = Ok (pid, pre, sm) ->
     LambdaProvider W P F C ps rt body sg this pid pre sm *)
Definition lambda_provider (what : string) (F : frame) (C : cx) (ps : list (name * option aty)) (rt : option aty) (body : aexpr)
    (sg : fsig) (this : option ty) : M (nat * wrapper * smap) :=
  let* _ := check (Nat.eqb (List.length ps) (List.length (sg_params sg)))
             ("the anonymous function bound to " ++ what ++ " takes " ++ nat_str (List.length ps) ++
              " parameter(s), but " ++ what ++ " takes " ++ nat_str (List.length (sg_params sg)) ++ " (D70)") in
  let* _ := match suniq_dup (map fst ps) with
            | Some n => fail ("anonymous function parameter `" ++ n ++ "` is declared twice")
            | None => ret tt end in
  let* _ := miter (fun q => match snd (fst q) with
              | Some a => let* t' := pure (ty_of W fuel (fr_mod F) (c_ty C) this a) in
                          check (ty_eqb (canon C t') (canon C (snd q)))
                            ("parameter `" ++ fst (fst q) ++ "` of the anonymous function bound to " ++ what ++
                             " is annotated " ++ qty W t' ++ ", but " ++ what ++ " declares " ++ qty W (snd q) ++ " there (D70)")
              | None => ret tt end) (combine ps (sg_params sg)) in
  let* _ := match rt with
            | Some a => let* t' := pure (ty_of W fuel (fr_mod F) (c_ty C) this a) in
                        check (ty_eqb (canon C t') (canon C (sg_ret sg)))
                          ("the anonymous function bound to " ++ what ++ " is annotated to return " ++ qty W t' ++
                           ", but " ++ what ++ " returns " ++ qty W (sg_ret sg) ++ " (D70)")
            | None => ret tt end in
  let pnames := map fst ps in
  let caps := lambda_capture_names F C pnames body in
  let* capvals := fresh_n (List.length caps) in
  let* pid := reserve_code FunctionTable in
  let Cl := captured_cx C caps capvals in
  let Fl := Frame (fr_mod F) (Some (sg_ret sg)) this false
             (mk_frame_vars (is_some this) pnames (caps ++ ex_names W fuel (fr_mod F) body))
             (fr_fresh F ++ context_gens C) (fr_applying F) [] in
  let* _ := check (forallb (fun n => is_some (slook n (fr_var F))) caps) "internal: a captured local is not numbered" in
  let* (lbody, ltype) := map_err (fun m => if String.eqb m "`this` is only legal inside method bodies" && negb (is_some this)
                                          then (m ++ "; an anonymous function bound to a fn has no receiver")%string else m) (r_blke R Fl (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
                                         (combine pnames (sg_params sg)) Cl)
                          (ABlock [] (Some body)) (Some (sg_ret sg))) in
  let* _ := check (fitsb ltype (sg_ret sg))
             ("the anonymous function bound to " ++ what ++ " yields " ++ qty W ltype ++ ", but " ++ what ++ " returns " ++ qty W (sg_ret sg)) in
  let captures := combine caps capvals in
  let* _ := write_fn_code pid (translate_lambda_body Fl captures lbody) in
  let* _ := put_lamcx pid C in
  let ixl := PIdx (pindex (c_provisions Cl)) in
  let ix := PIdx (pindex (c_provisions C)) in
  let keys := lambda_need_keys Cl ixl in
  let sm := map (fun k => (rkof_i Cl ixl k, rkof_i C ix k)) keys in
  ret (slot_index pid, translate_lambda_captures F captures, sm).

(* ---- MethProvider ---- *)

(* SOUND: meth_provider F C y sg rt = Ok (target, types, pre, sm) ->
     MethProvider W P F C y sg rt target types pre sm *)
Definition meth_provider (m : nat) (F : frame) (C : cx) (y : aexpr) (sg : fsig) (rt : ty) : M (nat * type_capture * wrapper * smap) :=
  match y with
  | AEField (AEPath rp None) n =>
      let* rg := lift (resolve_sym W (fr_mod F) rp) (bq (show_path rp) ++ " is not in scope") in
      let* att := lift (w_attached W rg n) (qs W rg ++ " has no defined attached method `." ++ n ++ "`") in
      match ast_of W att with
      | Some (ADFn (RAttached _) _ _ _ (Some _)) =>
          match canon C rt with
          | TNom rg' rargs =>
              let* _ := check (Nat.eqb rg rg') ("provider " ++ qs W att ++ " is attached to " ++ qs W rg ++ ", not " ++ qty W (canon C rt)) in
              let Cd := fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C in
              let who := ("binding to " ++ qs W att)%string in
              let* (Ca, ns) := lift (w_context W att) ("the context of " ++ qs W att ++ " does not form") in
              let* th := pure (call_theta W who Cd Ca) in
              let* sm := pure (satisfies W ("calling " ++ qs W att) who Cd Ca th ns) in
              let* _ := pure (rule_b W who Cd Ca) in
              let* g' := pure (sig_of W fuel att (c_ty Cd) (Some (canon C rt))) in
              let want := FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg)) in
              let* _ := check (fsig_eqb g' want) (sig_mismatch "attached method" att g' want) in
              ret (att, TypeCapture th th, (fun x => x), sm)
          | _ => fail ("provider " ++ qs W att ++ " is attached to " ++ qs W rg ++ ", not " ++ qty W (canon C rt))
          end
      | _ => fail (qs W rg ++ " has no defined attached method `." ++ n ++ "`")
      end
  | AEPath pa None =>
      let* ((g, types), sm) := fn_provider "method" F C pa sg in
      let* (Cg, nsg) := lift (w_context W g) ("the context of " ++ qs W g ++ " does not form") in
      let n := List.length (sg_params sg) in
      let* cached := get_wrap (g, n) in
      let* w := match cached with
                | Some w => ret w
                | None => let* w := emit_fn_code (ECall g (theta_ctx Cg) (map (fun k => (rkof Cg k, rkof Cg k)) nsg)
                                                        (map EVar (seq 1 n))) in
                          let* _ := put_wrap (g, n) w in ret w
                end in
      ret (slot_index w, types, (fun x => x), sm)
  | AELam ps r body =>
      let* ((pid, pre), sm) := lambda_provider (bq (sty W rt ++ mname W m)) F C ps r body sg (Some rt) in
      ret (pid, lexical_capture C, pre, sm)
  | _ => fail "the right side of a fn bind must name a defined function"
  end.

(* ---- BindItem and BindItems ---- *)

(* SOUND (given RecSound E P R):
   bind_item F C it = Ok (wrap, C', F', made) -> BindItem W P F C it wrap C' F' made *)
Definition bind_item (F : frame) (C : cx) (it : spec * option aexpr) : M (wrapper * cx * frame * list nat) :=
  let md := fr_mod F in
  match it with
  | (Spec (Some pa) (Some dn) a, Some y) =>
      let* r := lift (resolve_sym W md pa) (bq (show_path pa) ++ " is not in scope") in
      let* m := lift (method_named W md r dn) ("no method `." ++ dn ++ "` for " ++ qs W r ++ " is in scope") in
      (* Q3: a defined (attached) method is not bound *)
      let* _ := check (match ast_of W m with Some (ADFn _ _ _ _ (Some _)) => false | _ => true end)
                 (bq (sym_name W r ++ "." ++ dn) ++ " is a defined method of " ++ qs W r ++
                  ", which calls always reach; it cannot be bound (bind the detached method instead)") in
      let* bs := pure (brackets W fuel md (c_ty C) m a) in
      let* rt := pure (ty_of W fuel md (bs ++ c_ty C) None (ATRef pa None)) in
      let* sg := pure (sig_of W fuel m (bs ++ c_ty C) (Some rt)) in
      let* (((target, types), pre), sm) := meth_provider m F C y sg rt in
      let ap := app_of W metadata_fuel m (bs ++ c_ty C) in
      let key := mkey (canon C rt) m in
      let* _ := match find (fun u => family_produces E C u key) (cfun_keys C) with
                | Some u => fail (bq (sty W (canon C rt) ++ mname W m) ++ " is a member of the family " ++ qs W u ++
                                  " in force, which provides it; it cannot be bound directly (D84 rule 7)")
                | None => ret tt end in
      ret ((fun x => pre (bind_method_expression (canon C rt) m (theta_of C ap) target types sm x)),
           provide C key ap (set_mth C (canon C rt) m sg), F, [])
  | (Spec (Some pa) None None, oy) =>
      let* s := lift (resolve_sym W md pa) (bq (show_path pa) ++ " is not in scope") in
      let* d := lift (w_decl W s) "internal: unknown symbol" in
      match d_ast d, oy with
      | ADType _, Some (AEPath rp ra) =>
          let* _ := check (negb (nmem s (fr_pins F))) ("`bind " ++ sym_name W s ++ "` in a functor bound to an abstract one changes a type its result mentions; the fixed set is not widened per binding (D84 3d)") in
          let* tau := pure (ty_of W fuel md (c_ty C) None (ATRef rp ra)) in
          ret ((fun x => bind_type_expression s (canon C tau) x), set_ty C s (canon C tau), F, [])
      | ADVal _ t0 _, Some y =>
          let* declared := pure (ty_of W fuel (d_mod d) (c_ty C) None t0) in
          let* (x, xt) := r_ex R F C y (Some declared) in
          let* _ := check (fitsb xt declared) ("bind of " ++ qs W s ++ ": got " ++ qty W xt ++ ", its declared type is " ++ qty W declared) in
          let a := app_of W metadata_fuel s (c_ty C) in
          ret ((fun e => bind_value_expression s x e), provide C (IVal s) a (set_val C s declared), F, [])
      | ADFn RPlain _ _ _ _, Some (AEPath pg None) =>
          let* sg := pure (sig_of W fuel s (c_ty C) None) in
          let* ((g, types), sm) := fn_provider "fn" F C pg sg in
          let a := app_of W metadata_fuel s (c_ty C) in
          ret ((fun x => bind_function_expression s g types sm x), provide C (IFn s) a (set_fn C s sg), F, [])
      | ADFn RPlain _ _ _ _, Some (AELam ps rt body) =>
          let* sg := pure (sig_of W fuel s (c_ty C) None) in
          let* ((pid, pre), sm) := lambda_provider (qs W s) F C ps rt body sg None in
          let a := app_of W metadata_fuel s (c_ty C) in
          ret ((fun x => pre (bind_function_expression s pid (lexical_capture C) sm x)),
               provide C (IFn s) a (set_fn C s sg), F, [])
      | ADFunctor _ _ _ _ (Some bodies), None =>
          let* _ := check (negb (nmem s (fr_applying F))) ("functor " ++ qs W s ++ " applies itself") in
          let* (((wrap, C'), F'), made) := r_bind_items R (with_mod F (d_mod d) s) C (List.concat bodies) in
          ret (wrap, C', carry_frame_allocations F F', made)
      | ADFunctor _ _ _ _ None, None => apply_abstract E F C s
      | ADFunctor _ _ _ _ None, Some (AEPath pg None) =>
          let* g := lift (resolve_sym W md pg) (bq (show_path pg) ++ " is not in scope") in
          match ast_of W g with
          | Some (ADFunctor _ _ _ _ (Some _)) =>
              let* (wrap, C') := fun_closure E R F C s g in ret (wrap, C', F, [])
          | Some (ADFunctor _ _ _ _ None) =>
              let* (wrap, C') := fun_copy E F C s g in ret (wrap, C', F, [])
          | _ => fail (qs W g ++ " is not a functor, so it cannot be bound to " ++ qs W s)
          end
      | ADFunctor _ _ _ _ _, Some _ => fail ("the right side of a bind of functor " ++ qs W s ++ " must name a functor")
      | ADType _, Some _ => fail ("binding type " ++ qs W s ++ " needs a type on the right")
      | ADFn RPlain _ _ _ _, Some _ => fail "the right side of a fn bind must name a defined function"
      | ADVal _ _ _, None | ADFn _ _ _ _ _, None | ADType _, None | ADCtx _ _, None
      | ADTag _ _ _, None | ADUnit _, None | ADAlias _ _, None =>
          fail (qs W s ++ " is not a functor; a bind needs `= value`")
      | _, _ => fail ("cannot bind " ++ qs W s)
      end
  | (Spec _ None (Some _), _) => fail "a bind left-hand side takes no bracket application"
  | _ => fail "a bind needs a left-hand side"
  end.

(* SOUND: bind_items F C its = Ok (wrap, C', F', made) -> BindItems W P F C its wrap C' F' made *)
Fixpoint bind_items (F : frame) (C : cx) (its : list (spec * option aexpr)) : M (wrapper * cx * frame * list nat) :=
  match its with
  | [] => ret ((fun x => x), C, F, [])
  | it :: r =>
      let* (((w1, C1), F1), m1) := bind_item F C it in
      let* (((w2, C2), F2), m2) := bind_items F1 C1 r in
      ret ((fun x => w1 (w2 x)), C2, F2, m1 ++ m2)
  end.

End Bind.
