(* Closure compilation with cached runtime type projections.
   CachedEnvFacts and CachedCloFacts prove agreement with SlotX. *)
From Stdlib Require Import List Bool Arith Lia PrimInt63.
From NM Require Import Audit CoreMoss MossX FastX Elab Prune SlotX FoldX CloScalar.
Import ListNotations.
Set Implicit Arguments.
Local Open Scope nat_scope.
Record cached_env := CachedEnv {
  cache_raw : renv;
  cache_types : subst;
  cache_rows : renv
}.
Definition cache_of (d : renv) : cached_env :=
  CachedEnv d (SlotX.rdtypes d) (SlotX.rtype_env d).
Definition cache_empty := CachedEnv [] [] [].
Definition cpush (entry : item * rentry) (d : cached_env) : cached_env :=
  match entry with
  | (ITy t, QType tau) => CachedEnv (entry :: cache_raw d)
      ((t,tau) :: cache_types d) (entry :: cache_rows d)
  | _ => CachedEnv (entry :: cache_raw d) (cache_types d) (cache_rows d)
  end.
Definition cappend (a b : cached_env) : cached_env :=
  CachedEnv (cache_raw a ++ cache_raw b) (cache_types a ++ cache_types b)
    (cache_rows a ++ cache_rows b).
Definition rlook l d := SlotX.rlook l (cache_raw d).
Definition rnth2 n d := SlotX.rnth2 n (cache_raw d).
Definition rcompose d pl := option_map cache_of (SlotX.rcompose (cache_raw d) pl).
Definition rcompose_dyn d sm := option_map cache_of (SlotX.rcompose_dyn (cache_raw d) sm).
Fixpoint rchain (d : cached_env) (sms : list smap) : option cached_env :=
  match sms with
  | [] => Some d
  | sm :: rest => match rchain d rest with
      | Some d' => rcompose_dyn d' sm | None => None end
  end.
Definition rground d tau := Audit.app_subst (cache_types d) tau.
Definition rinst d th : cached_env :=
  let types := map (fun p => (fst p,rground d (snd p))) th in
  let rows := map (fun p => (ITy (fst p),QType (snd p))) types in
  CachedEnv rows types rows.
Definition rclose d dc := CachedEnv (cache_raw dc ++ cache_rows d)
  (cache_types dc ++ cache_types d) (cache_rows dc ++ cache_rows d).
Definition rcall_env P d th dc :=
  if xreify P then cappend (rinst d th) (rclose d dc) else dc.
Definition rapply_env P d h th dc := rcall_env P d
  (filter (fun p => existsb (Nat.eqb (fst p)) (xp_inputs P h)) th) dc.
Definition rbind_ty P d t tau :=
  if xreify P then cpush (ITy t,QType(rground d tau)) d else d.
Definition rprim_entry d i p := match p,i with
  | PBox _,IMth tau _ _ | PArrNew _,IMth tau _ _ => QPrimAt p (rground d tau)
  | _,_ => QPrim p end.
Definition rflag d b := match cache_rows d with []=>b|_=>false end.
Definition rtyped_cast P d n th w := SlotX.rtyped_cast P (cache_raw d) n th w.
Definition rresult_env P d Dr dr := if xreify P then
  let rows := filter (fun p => existsb (item_eqb(fst p)) Dr) (cache_rows d) in
  cappend dr (cache_of rows) else dr.
Definition instantiate_scalar_fast th d := match th with
  | []=>d
  | _::_=>CachedEnv (CloScalar.instantiate_scalar_fast th (cache_raw d))
      (cache_types d) (cache_rows d) end.
Definition prepare_scalar_cache Dr bs :=
  option_map (fun p => (fst p,cache_of(snd p))) (CloScalar.prepare_scalar Dr bs).

Definition code := nat -> cached_env -> xvenv -> mstate -> xres.
Definition lcode := nat -> cached_env -> xvenv -> mstate -> (list xvalue * xvenv * mstate) + xres.
Definition bcode := nat -> cached_env -> xvenv -> mstate -> (cached_env * xvenv * mstate) + xres.

Section CC.
Variable R : rprog.
Variable tbl : fsym -> option (code * code).
Variable ftbl : usym -> option (tele * bcode * option(nat*cached_env)).

Local Notation P := (rp_x R).

Definition lnil : lcode := fun n d g st =>
  match n with 0 => inr (XR XFuel g st) | S _ => inl ([], g, st) end.

(* rinterp_list's cons step, with e1's code c *)
Definition lcons (c : code) (cr : lcode) : lcode := fun n d g st =>
  match n with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match c k d g st with
      | XR (XOk w) g1 st1 =>
          match cr k d g1 st1 with
          | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
          | inr rr => inr rr
          end
      | rr => inr rr
      end
  end.

(* the same step when e1 is a variable: rinterp k (RVar x) unfolded *)
Definition lvar (x : var) (cr : lcode) : lcode := fun n d g st =>
  match n with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match k with
      | 0 => inr (XR XFuel g st)
      | S _ =>
          match lookup_xvenv x g with
          | Some w =>
              match cr k d g st with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | None => inr (XR (XErr e_var) g st)
          end
      end
  end.

(* ... and when e1 is a val: rinterp k (RVl l) unfolded *)
Definition lval (l : rlk) (cr : lcode) : lcode := fun n d g st =>
  match n with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match k with
      | 0 => inr (XR XFuel g st)
      | S _ =>
          match rlook l d with
          | Some (QVal w) =>
              match cr k d g st with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | _ => inr (XR (XErr e_val) g st)
          end
      end
  end.

Definition bnil : bcode := fun n d g st =>
  match n with 0 => inr (XR XFuel g st) | S _ => inl (d, g, st) end.

Fixpoint rfind_carm (n : nsym) (arms : list (nsym * var * code)) : option (var * code) :=
  match arms with
  | [] => None
  | (n', x, c) :: r => if Nat.eqb n n' then Some (x, c) else rfind_carm n r
  end.

Fixpoint rselect_carm (arms : list (xpat * code)) (w : xvalue) : option (xvenv * code) :=
  match arms with
  | [] => None
  | (p, c) :: r =>
      match match_pat p w with
      | PMatch bs => Some (bs, c)
      | PNo => rselect_carm r w
      | PBad => None
      end
  end.

Fixpoint rfind_cbranch (h : usym) (brs : list (usym * bcode * plan)) : option (bcode * plan) :=
  match brs with
  | [] => None
  | (h', c, pairs) :: r => if Nat.eqb h h' then Some (c, pairs) else rfind_cbranch h r
  end.

(* rinterp_apply, with the functor bodies' code from ftbl *)
Definition capply (l : rlk) (th : subst) (pl : plan) : nat -> cached_env -> mstate -> (cached_env * mstate) + xres :=
  fun n d st =>
  match n with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match rlook l d with
      | Some (QFun h sms dc) =>
          match ftbl h with
          | Some (Dr, cb, prepared) =>
              match rcompose d pl with
              | Some d1 =>
                  match rchain d1 sms with
                  | Some d2 =>
                      match prepared with
                      | Some (count, result) =>
                          if Nat.ltb count k then
                            inl (instantiate_scalar_fast th result, st)
                          else inr (XR XFuel [] st)
                      | None => match cb k (rapply_env (rp_x R) d h th (cappend d2 (cache_of dc))) [] st with
                      | inl (d3, _, st3) =>
                          match rcompose_dyn d3 (inst_smap th Dr) with
                          | Some dr => inl (rresult_env (rp_x R) d3 Dr dr, st3)
                          | None => inr (XR (XErr e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                      end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end.

Definition capply_r (l : rlk) (pl : plan) (outputs : smap) : nat -> cached_env -> mstate -> (cached_env * mstate) + xres :=
  fun n d st =>
  match n with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match rlook l d with
      | Some (QFun h sms dc) =>
          match ftbl h with
          | Some (_, cb, _) =>
              match rcompose d pl with
              | Some d1 =>
                  match rchain d1 sms with
                  | Some d2 =>
                      match cb k (cappend d2 (cache_of dc)) [] st with
                      | inl (d3, _, st3) =>
                          match rcompose_dyn d3 (xp_exports(rp_x R)h) with
                          | Some named =>
                              match rcompose_dyn named outputs with
                              | Some dr => inl(dr,st3)
                              | None => inr(XR(XErr e_compose)[]st3)
                              end
                          | None => inr(XR(XErr e_compose)[]st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end.

Definition cbranch (l : rlk) (brs : list (usym * bcode * plan))
  : nat -> cached_env -> xvenv -> mstate -> (cached_env * xvenv * mstate) + xres :=
  fun n d g st =>
  match n with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match rlook l d with
      | Some (QFunSym h) =>
          match rfind_cbranch h brs with
          | Some (cb, pairs) =>
              match cb k d g st with
              | inl (d3, g3, st3) =>
                  match rcompose d3 pairs with
                  | Some dr => inl (dr, g3, st3)
                  | None => inr (XR (XErr e_compose) g3 st3)
                  end
              | inr rr => inr rr
              end
          | None => inr (XR (XErr e_branch) g st)
          end
      | _ => inr (XR (XErr e_fun) g st)
      end
  end.

(* FoldX's folded calls, closure-compiled like everything else (csev_eq) *)
Definition scode := cached_env -> xvenv -> mstate -> option (xvalue * mstate).
Definition slcode := cached_env -> xvenv -> mstate -> option (list xvalue * mstate).

Definition csnil : slcode := fun d g st => Some ([], st).
Definition cscons (c : scode) (cr : slcode) : slcode := fun d g st =>
  match c d g st with
  | Some (w, st1) => match cr d g st1 with Some (ws, st2) => Some (w :: ws, st2) | None => None end
  | None => None
  end.

Fixpoint csev (e : sexpr) {struct e} : scode :=
  match e with
  | SVar x => fun d g st => match lookup_xvenv x g with Some w => Some (w, st) | None => None end
  | SVl l => fun d g st => match rlook l d with Some (QVal w) => Some (w, st) | _ => None end
  | STag t e1 => let c1 := csev e1 in fun d g st =>
      match c1 d g st with Some (w, st1) => Some (XVTag t w, st1) | None => None end
  | SPrim l args =>
      let ca := fold_right (fun a cr => let c := csev a in cscons c cr) csnil args in
      fun d g st =>
      match rlook l d with
      | Some (QPrim p) =>
          match ca d g st with
          | Some (ws, st1) => match run_prim_at P p None ws st1 with (XOk w, st2) => Some (w, st2) | _ => None end
          | None => None
          end
      | _ => None
      end
  | SCall j args body =>
      let ca := fold_right (fun a cr => let c := csev a in cscons c cr) csnil args in
      let cb := csev body in
      fun d g st =>
      match rnth2 j d with
      | Some _ => match ca d g st with Some (ws, st1) => cb d (mkv ws) st1 | None => None end
      | None => None
      end
  | SMatch1 e0 p b => let c0 := csev e0 in let cb := csev b in fun d g st =>
      match c0 d g st with
      | Some (w, st1) => match match_pat p w with PMatch bs => cb d (bs ++ g) st1 | _ => None end
      | None => None
      end
  end.

Definition csevl (l : list sexpr) : slcode := fold_right (fun a cr => let c := csev a in cscons c cr) csnil l.

Fixpoint cc (e : rexpr) {struct e} : code :=
  match e with
  | RVar x => fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S _ => match lookup_xvenv x g with
               | Some w => XR (XOk w) g st
               | None => XR (XErr e_var) g st
               end
      end
  | RUnit => fun n d g st => match n with 0 => XR XFuel g st | S _ => XR (XOk XVUnit) g st end
  | RVl l => fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S _ => match rlook l d with
               | Some (QVal w) => XR (XOk w) g st
               | _ => XR (XErr e_val) g st
               end
      end
  | RTag t th e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w) g1 st1 => XR (rtyped_cast (rp_x R) d t th w) g1 st1
               | r => r
               end
      end
  | RCallD f th b pl args =>
      let ca := fold_right (fun e cr => match e with
                                        | RVar x => lvar x cr
                                        | RVl l => lval l cr
                                        | _ => let c := cc e in lcons c cr end) lnil args in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match tbl f with
          | Some (cs, cd) =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  match rcompose d pl with
                  | Some d' => ret_call g1 ((if b && negb (xreify (rp_x R)) then cs else cd) k (rcall_env (rp_x R) d th d') (mkv ws) st1)
                  | None => XR (XErr e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => XR (XErr e_nobody) g st
          end
      end
  | RCallA l args =>
      let ca := fold_right (fun e cr => match e with
                                        | RVar x => lvar x cr
                                        | RVl l => lval l cr
                                        | _ => let c := cc e in lcons c cr end) lnil args in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match rlook l d with
          | Some (QClo b f' df) =>
              match tbl f' with
              | Some (cs, cd) =>
                  match ca k d g st with
                  | inl (ws, g1, st1) =>
                      ret_call g1 ((if b then cs else cd) k (cache_of df) (mkv ws) st1)
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (QPrim p) =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p None ws st1 in XR o g1 st2
              | inr r => r
              end
          | Some (QPrimAt p tau) =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p (Some tau) ws st1 in XR o g1 st2
              | inr r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      end
  | RMeth e0 l args =>
      let c0 := cc e0 in
      let ca := fold_right (fun e cr => match e with
                                        | RVar x => lvar x cr
                                        | RVl l => lval l cr
                                        | _ => let c := cc e in lcons c cr end) lnil args in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match rlook l d with
          | Some (QClo b f' d') =>
              match tbl f' with
              | Some (cs, cd) =>
                  match c0 k d g st with
                  | XR (XOk w0) g0 st0 =>
                      match ca k d g0 st0 with
                      | inl (ws, g1, st1) =>
                          ret_call g1 ((if b then cs else cd) k (cache_of d') (mkv (w0 :: ws)) st1)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (QPrim p) =>
              match c0 k d g st with
              | XR (XOk w0) g0 st0 =>
                  match ca k d g0 st0 with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p None (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | Some (QPrimAt p tau) =>
              match c0 k d g st with
              | XR (XOk w0) g0 st0 =>
                  match ca k d g0 st0 with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p (Some tau) (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      end
  | RMatch e0 arms =>
      let c0 := cc e0 in
      let carms := map (fun a => match a with (t, x, eb) => (t, x, cc eb) end) arms in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match c0 k d g st with
          | XR (XOk (XVTag t w')) g1 st1 =>
              match rfind_carm t carms with
              | Some (x, cb) => pop1 (cb k d ((x, w') :: g1) st1)
              | None => XR (XErr e_match) g1 st1
              end
          | XR (XOk _) g1 st1 => XR (XErr e_match) g1 st1
          | r => r
          end
      end
  | RLet x e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w1) g1 st1 => pop1 (c2 k d ((x, w1) :: g1) st1)
               | r => r
               end
      end
  | RBindTy t tau e1 => let c1 := cc e1 in fun n d g st =>
      match n with 0 => XR XFuel g st | S k => c1 k (rbind_ty (rp_x R) d t tau) g st end
  | RBindVal v e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w1) g1 st1 => c2 k (cpush (IVal v, QVal w1) d) g1 st1
               | r => r
               end
      end
  | RBindClo i f' b pl e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rcompose d pl with
               | Some dc => c1 k (cpush (i, QClo (rflag d b) f' (cache_raw (rclose d dc))) d) g st
               | None => XR (XErr e_compose) g st
               end
      end
  | RBindFun u h sm pl e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rcompose d pl with
               | Some dc => c1 k (cpush (IFun u, QFun h [sm] (cache_raw (rclose d dc))) d) g st
               | None => XR (XErr e_compose) g st
               end
      end
  | RBindFunA u l sm e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rlook l d with
               | Some (QFun h sms dc) => c1 k (cpush (IFun u, QFun h (sms ++ [sm]) dc) d) g st
               | _ => XR (XErr e_fun) g st
               end
      end
  | RApply l th pl e1 => let c1 := cc e1 in let ca := capply l th pl in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match ca k d st with
               | inl (dr, st1) => c1 k (cappend dr d) g st1
               | inr r => with_g g r
               end
      end
  | RApplyR l inputs outputs e1 => let c1 := cc e1 in let ca := capply_r l inputs outputs in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match ca k d st with
               | inl (dr, st1) => c1 k (cappend dr d) g st1
               | inr r => with_g g r
               end
      end
  | RSeq e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk _) g1 st1 => c2 k d g1 st1
               | r => r
               end
      end
  | RAssign x e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w) g1 st1 =>
                   match assign_xvenv x w g1 with
                   | Some g2 => XR (XOk XVUnit) g2 st1
                   | None => XR (XErr e_assign) g1 st1
                   end
               | r => r
               end
      end
  | RIf c e1 e2 => let c0 := cc c in let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c0 k d g st with
               | XR (XOk w) g1 st1 =>
                   match truthy P w with
                   | Some true => c1 k d g1 st1
                   | Some false => c2 k d g1 st1
                   | None => XR (XErr e_bool) g1 st1
                   end
               | r => r
               end
      end
  | RWhile c b => let c0 := cc c in let cb := cc b in
      fix loop (n : nat) (d : cached_env) (g : xvenv) (st : mstate) {struct n} : xres :=
      match n with
      | 0 => XR XFuel g st
      | S k => match c0 k d g st with
               | XR (XOk w) g1 st1 =>
                   match truthy P w with
                   | Some true =>
                       match cb k d g1 st1 with
                       | XR (XOk _) g2 st2 => loop k d g2 st2
                       | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
                       | r => r
                       end
                   | Some false => XR (XOk XVUnit) g1 st1
                   | None => XR (XErr e_bool) g1 st1
                   end
               | r => r
               end
      end
  | RLoop b => let cb := cc b in
      fix loop (n : nat) (d : cached_env) (g : xvenv) (st : mstate) {struct n} : xres :=
      match n with
      | 0 => XR XFuel g st
      | S k => match cb k d g st with
               | XR (XOk _) g2 st2 => loop k d g2 st2
               | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
               | r => r
               end
      end
  | RReturn e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w) g1 st1 => XR (XRet w) g1 st1
               | r => r
               end
      end
  | RBreak => fun n d g st => match n with 0 => XR XFuel g st | S _ => XR XBrk g st end
  | RRec fs =>
      let flds := map fst fs in
      let ca := fold_right (fun p cr => match p with (_, e) => let c := cc e in lcons c cr end) lnil fs in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match ca k d g st with
               | inl (ws, g1, st1) => XR (XOk (XVRec (combine flds ws))) g1 st1
               | inr r => r
               end
      end
  | RField e1 f => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w) g1 st1 =>
                   match get_field f w with
                   | Some v => XR (XOk v) g1 st1
                   | None => XR (XErr e_field) g1 st1
                   end
               | r => r
               end
      end
  | RMatchP e0 arms =>
      let c0 := cc e0 in
      let carms := map (fun a => match a with (p, eb) => (p, cc eb) end) arms in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c0 k d g st with
               | XR (XOk w) g1 st1 =>
                   match rselect_carm carms w with
                   | Some (bs, cb) => popn (List.length bs) (cb k d (bs ++ g1) st1)
                   | None => XR (XErr e_pat) g1 st1
                   end
               | r => r
               end
      end
  | RBindFunSym u h e1 => let c1 := cc e1 in fun n d g st =>
      match n with 0 => XR XFuel g st | S k => c1 k (cpush (IFun u, QFunSym h) d) g st end
  | RBindFunCopy u l e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rlook l d with
               | Some en => c1 k (cpush (IFun u, en) d) g st
               | None => XR (XErr e_fun) g st
               end
      end
  | RApplyBr l brs e1 =>
      let c1 := cc e1 in
      let brs' := map (fun br => match br with (h, bs, pairs) =>
                  (h, fold_right ccb bnil bs, pairs) end) brs in
      let cb := cbranch l brs' in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match cb k d g st with
               | inl (dr, g1, st1) => c1 k (cappend dr d) g1 st1
               | inr r => r
               end
      end
  | RBindPrim i p e1 => let c1 := cc e1 in fun n d g st =>
      match n with 0 => XR XFuel g st | S k => c1 k (cpush (i, rprim_entry d i p) d) g st end
  | RCallF f pl args D ss sb =>
      let cl := csevl ss in
      let cb := csev sb in
      let ca := fold_right (fun e cr => match e with
                                        | RVar x => lvar x cr
                                        | RVl l => lval l cr
                                        | _ => let c := cc e in lcons c cr end) lnil args in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match (if Nat.leb D k then
                   match cl d g st with
                   | Some (ws, st1) => match rcompose d pl with
                                       | Some d' => cb d' (mkv ws) st1
                                       | None => None
                                       end
                   | None => None
                   end
                 else None) with
          | Some (w, st2) => XR (XOk w) g st2
          | None =>
              match tbl f with
              | Some (cs, cd) =>
                  match ca k d g st with
                  | inl (ws, g1, st1) =>
                      match rcompose d pl with
                      | Some d' => ret_call g1 (cs k d' (mkv ws) st1)
                      | None => XR (XErr e_compose) g1 st1
                      end
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          end
      end
  end
(* rinterp_binds' step for one bind, given the rest's code *)
with ccb (b : rbind) (cr : bcode) {struct b} : bcode :=
  match b with
  | RBTy t tau => fun n d g st => match n with 0 => inr (XR XFuel g st) | S k => cr k (rbind_ty (rp_x R) d t tau) g st end
  | RBVal v e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match c1 k d g st with
               | XR (XOk w1) g1 st1 => cr k (cpush (IVal v, QVal w1) d) g1 st1
               | rr => inr rr
               end
      end
  | RBClo i f' b pl => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rcompose d pl with
               | Some dc => cr k (cpush (i, QClo (rflag d b) f' (cache_raw (rclose d dc))) d) g st
               | None => inr (XR (XErr e_compose) g st)
               end
      end
  | RBFun u h sm pl => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rcompose d pl with
               | Some dc => cr k (cpush (IFun u, QFun h [sm] (cache_raw (rclose d dc))) d) g st
               | None => inr (XR (XErr e_compose) g st)
               end
      end
  | RBFunA u l sm => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rlook l d with
               | Some (QFun h sms dc) => cr k (cpush (IFun u, QFun h (sms ++ [sm]) dc) d) g st
               | _ => inr (XR (XErr e_fun) g st)
               end
      end
  | RBApply l th pl => let ca := capply l th pl in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match ca k d st with
               | inl (dr, st1) => cr k (cappend dr d) g st1
               | inr rr => inr (with_g g rr)
               end
      end
  | RBApplyR l inputs outputs => let ca := capply_r l inputs outputs in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match ca k d st with
               | inl (dr, st1) => cr k (cappend dr d) g st1
               | inr rr => inr (with_g g rr)
               end
      end
  | RBFunSym u h => fun n d g st =>
      match n with 0 => inr (XR XFuel g st) | S k => cr k (cpush (IFun u, QFunSym h) d) g st end
  | RBFunCopy u l => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rlook l d with
               | Some en => cr k (cpush (IFun u, en) d) g st
               | None => inr (XR (XErr e_fun) g st)
               end
      end
  | RBApplyBr l brs =>
      let brs' := map (fun br => match br with (h, bs, pairs) =>
                  (h, fold_right ccb bnil bs, pairs) end) brs in
      let cb := cbranch l brs' in
      fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match cb k d g st with
               | inl (dr, g1, st1) => cr k (cappend dr d) g1 st1
               | inr rr => inr rr
               end
      end
  | RBPrim i p => fun n d g st =>
      match n with 0 => inr (XR XFuel g st) | S k => cr k (cpush (i, rprim_entry d i p) d) g st end
  end.

Definition ccl (l : list rexpr) : lcode :=
  fold_right (fun e cr => match e with
                          | RVar x => lvar x cr
                          | RVl l => lval l cr
                          | _ => let c := cc e in lcons c cr end) lnil l.

Definition ccbs (bs : list rbind) : bcode := fold_right ccb bnil bs.

Definition cbrs_c (brs : list (usym * list rbind * plan)) : list (usym * bcode * plan) :=
  map (fun br => match br with (h, bs, pairs) => (h, ccbs bs, pairs) end) brs.

End CC.

Definition crun_main (tbl : fsym -> option (code * code)) (fuel : nat) (main : fsym)
    (d0 : xdenv) (st0 : mstate) : xres :=
  match tbl main with
  | Some (_, cd) => ret_call [] (cd fuel (cache_of (conv_env d0)) [] st0)
  | None => XR (XErr e_nobody) [] st0
  end.
