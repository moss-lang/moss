(* Closure compilation over LazyX's split environments. LazyCloFacts proves
   exact agreement with LazyX, and thus Audit's interpreter, for every fuel.
   Child/list code is built before its evaluation closure; Extract2.v preserves
   that sharing by extracting this library with term optimizations disabled. *)
From Stdlib Require Import List Bool Arith Lia PrimInt63.
From NM Require Import Audit MossX LazyX.
Import ListNotations.
Set Implicit Arguments.
Local Open Scope nat_scope.
Notation cached_env := fenv.
Fixpoint mkv_go (i:nat)(ws:list xvalue):xvenv:=match ws with
 |[]=>[]|w::rest=>(i,w)::mkv_go(S i)rest end.
Definition mkv:=mkv_go 0.
Definition cpush(entry:item*fentry)d:=fadd(fst entry)(snd entry)d.
Definition cappend:=fmerge.
Definition rlook:=flookup.
Definition rcompose:=fcompose.
Definition rcompose_dyn:=fcompose.
Definition rchain:=fchain.
Definition rcall_env:=fcall_env.
Definition rapply_env:=fapply_env.
Definition rresult_env:=fresult_env.
Definition rbind_ty:=fbind_ty.
Definition rprim_entry:=fprim_entry.
Definition rtyped_cast:=fcast.
Definition rclose:=fattach_types.
Definition cache_of(d:fenv):=d.
Definition cache_raw(d:fenv):=d.
Definition code := nat -> cached_env -> xvenv -> mstate -> xres.
Definition lcode := nat -> cached_env -> xvenv -> mstate -> (list xvalue * xvenv * mstate) + xres.
Definition bcode := nat -> cached_env -> xvenv -> mstate -> (cached_env * xvenv * mstate) + xres.

Section CC.
Variable P : xprog.
Variable tbl : fsym -> option code.
Variable ftbl : usym -> option (tele * bcode).



Definition lnil : lcode := fun n d g st =>
  match n with 0 => inr (XR XFuel g st) | S _ => inl ([], g, st) end.

(* fxinterp_list's cons step, with e1's code c *)
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

(* the same step when e1 is a variable: fxinterp k (XVar x) unfolded *)
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

(* ... and when e1 is a val: fxinterp k (XVl v) unfolded *)
Definition lval (l : item) (cr : lcode) : lcode := fun n d g st =>
  match n with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match k with
      | 0 => inr (XR XFuel g st)
      | S _ =>
          match rlook l d with
          | Some (FVal w) =>
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

Fixpoint rfind_cbranch (h : usym) (brs : list (usym * bcode * smap)) : option (bcode * smap) :=
  match brs with
  | [] => None
  | (h', c, pairs) :: r => if Nat.eqb h h' then Some (c, pairs) else rfind_cbranch h r
  end.

(* fxinterp_apply, with the functor bodies' code from ftbl *)
Definition capply (u : usym) (th : subst) (pl : smap) : nat -> cached_env -> mstate -> (cached_env * mstate) + xres :=
  fun n d st =>
  match n with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match rlook (IFun u) d with
      | Some (FFun h sms dc) =>
          match ftbl h with
          | Some (Dr, cb) =>
              match rcompose d pl with
              | Some d1 =>
                  match rchain d1 sms with
                  | Some d2 =>
                      match cb k (rapply_env P d h th (cappend d2 dc)) [] st with
                      | inl(d3,_,st3)=>match rcompose_dyn d3(inst_smap th Dr)with
                        |Some dr=>inl(rresult_env P d3 Dr dr,st3)
                        |None=>inr(XR(XErr e_compose)[]st3)end
                      |inr rr=>inr rr end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end.

Definition capply_r (u : usym) (pl : smap) (outputs : smap) : nat -> cached_env -> mstate -> (cached_env * mstate) + xres :=
  fun n d st =>
  match n with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match rlook (IFun u) d with
      | Some (FFun h sms dc) =>
          match ftbl h with
          | Some (_, cb) =>
              match rcompose d pl with
              | Some d1 =>
                  match rchain d1 sms with
                  | Some d2 =>
                      match cb k (cappend d2 (cache_of dc)) [] st with
                      | inl (d3, _, st3) =>
                          match rcompose_dyn d3 (xp_exports P h) with
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

Definition cbranch (u : usym) (brs : list (usym * bcode * smap))
  : nat -> cached_env -> xvenv -> mstate -> (cached_env * xvenv * mstate) + xres :=
  fun n d g st =>
  match n with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match rlook (IFun u) d with
      | Some (FFunSym h) =>
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

Fixpoint cc (e : xexpr) {struct e} : code :=
  match e with
  | XVar x => fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S _ => match lookup_xvenv x g with
               | Some w => XR (XOk w) g st
               | None => XR (XErr e_var) g st
               end
      end
  | XUnit => fun n d g st => match n with 0 => XR XFuel g st | S _ => XR (XOk XVUnit) g st end
  | XVl v => fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S _ => match rlook (IVal v) d with
               | Some (FVal w) => XR (XOk w) g st
               | _ => XR (XErr e_val) g st
               end
      end
  | XTag t th e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w) g1 st1 => XR (rtyped_cast  P  d t th w) g1 st1
               | r => r
               end
      end
  | XCallD f th pl args =>
      let ca := fold_right (fun e cr => match e with
                                        | XVar x => lvar x cr
                                        | XVl v => lval (IVal v) cr
                                        | _ => let c := cc e in lcons c cr end) lnil args in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match tbl f with
          | Some cf =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  match rcompose d pl with
                  | Some d' => ret_call g1 (cf k (rcall_env  P  d th d') (mkv ws) st1)
                  | None => XR (XErr e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => XR (XErr e_nobody) g st
          end
      end
  | XCallA f args =>
      let ca := fold_right (fun e cr => match e with
                                        | XVar x => lvar x cr
                                        | XVl v => lval (IVal v) cr
                                        | _ => let c := cc e in lcons c cr end) lnil args in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match rlook (IFn f) d with
          | Some (FClo f' df) =>
              match tbl f' with
              | Some cf =>
                  match ca k d g st with
                  | inl (ws, g1, st1) =>
                      ret_call g1 (cf k (cache_of df) (mkv ws) st1)
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (FPrim p) =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p None ws st1 in XR o g1 st2
              | inr r => r
              end
          | Some (FPrimAt p tau) =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p (Some tau) ws st1 in XR o g1 st2
              | inr r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      end
  | XMeth e0 m it args =>
      let c0 := cc e0 in
      let ca := fold_right (fun e cr => match e with
                                        | XVar x => lvar x cr
                                        | XVl v => lval (IVal v) cr
                                        | _ => let c := cc e in lcons c cr end) lnil args in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k =>
          match rlook it d with
          | Some (FClo f' d') =>
              match tbl f' with
              | Some cf =>
                  match c0 k d g st with
                  | XR (XOk w0) g0 st0 =>
                      match ca k d g0 st0 with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (cf k (cache_of d') (mkv (w0 :: ws)) st1)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (FPrim p) =>
              match c0 k d g st with
              | XR (XOk w0) g0 st0 =>
                  match ca k d g0 st0 with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p None (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | Some (FPrimAt p tau) =>
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
  | XMatch e0 arms =>
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
  | XLet x e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w1) g1 st1 => pop1 (c2 k d ((x, w1) :: g1) st1)
               | r => r
               end
      end
  | XBindTy t tau e1 => let c1 := cc e1 in fun n d g st =>
      match n with 0 => XR XFuel g st | S k => c1 k (rbind_ty  P  d t tau) g st end
  | XBindVal v e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w1) g1 st1 => c2 k (cpush (IVal v, FVal w1) d) g1 st1
               | r => r
               end
      end
  | XBindFn f f' pl e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rcompose d pl with
               | Some dc => c1 k (cpush (IFn f, FClo f' (rclose d dc)) d) g st
               | None => XR (XErr e_compose) g st
               end
      end
  | XBindMth tk m th f' pl e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rcompose d pl with
               | Some dc => c1 k (cpush (IMth tk m th, FClo f' (rclose d dc)) d) g st
               | None => XR (XErr e_compose) g st
               end
      end
  | XBindFun u h sm pl e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rcompose d pl with
               | Some dc => c1 k (cpush (IFun u, FFun h [sm] (cache_raw (rclose d dc))) d) g st
               | None => XR (XErr e_compose) g st
               end
      end
  | XBindFunA u u' sm e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rlook (IFun u') d with
               | Some (FFun h sms dc) => c1 k (cpush (IFun u, FFun h (sms ++ [sm]) dc) d) g st
               | _ => XR (XErr e_fun) g st
               end
      end
  | XApply u th pl e1 => let c1 := cc e1 in let ca := capply u th pl in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match ca k d st with
               | inl (dr, st1) => c1 k (cappend dr d) g st1
               | inr r => with_g g r
               end
      end
  | XApplyR u inputs outputs e1 => let c1 := cc e1 in let ca := capply_r u inputs outputs in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match ca k d st with
               | inl (dr, st1) => c1 k (cappend dr d) g st1
               | inr r => with_g g r
               end
      end
  | XSeq e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk _) g1 st1 => c2 k d g1 st1
               | r => r
               end
      end
  | XAssign x e1 => let c1 := cc e1 in fun n d g st =>
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
  | XIf c e1 e2 => let c0 := cc c in let c1 := cc e1 in let c2 := cc e2 in fun n d g st =>
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
  | XWhile c b => let c0 := cc c in let cb := cc b in
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
  | XLoop b => let cb := cc b in
      fix loop (n : nat) (d : cached_env) (g : xvenv) (st : mstate) {struct n} : xres :=
      match n with
      | 0 => XR XFuel g st
      | S k => match cb k d g st with
               | XR (XOk _) g2 st2 => loop k d g2 st2
               | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
               | r => r
               end
      end
  | XReturn e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match c1 k d g st with
               | XR (XOk w) g1 st1 => XR (XRet w) g1 st1
               | r => r
               end
      end
  | XBreak => fun n d g st => match n with 0 => XR XFuel g st | S _ => XR XBrk g st end
  | XRec fs =>
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
  | XField e1 f => let c1 := cc e1 in fun n d g st =>
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
  | XMatchP e0 arms =>
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
  | XBindFunSym u h e1 => let c1 := cc e1 in fun n d g st =>
      match n with 0 => XR XFuel g st | S k => c1 k (cpush (IFun u, FFunSym h) d) g st end
  | XBindFunCopy u u' e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rlook (IFun u') d with
               | Some en => c1 k (cpush (IFun u, en) d) g st
               | None => XR (XErr e_fun) g st
               end
      end
  | XApplyBr u brs e1 =>
      let c1 := cc e1 in
      let brs' := map (fun br => match br with (h, bs, pairs) =>
                  (h, fold_right ccb bnil bs, pairs) end) brs in
      let cb := cbranch u brs' in
      fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match cb k d g st with
               | inl (dr, g1, st1) => c1 k (cappend dr d) g1 st1
               | inr r => r
               end
      end
  | XBindPrim i p e1 => let c1 := cc e1 in fun n d g st =>
      match n with 0 => XR XFuel g st | S k => c1 k (cpush (i, rprim_entry d i p) d) g st end

  | XConst _ initializer => let ci:=cc initializer in fun n d g st=>
      match n with 0=>XR XFuel g st|S k=>match k with 0=>XR XFuel g st|S _=>ci k d g st end end
  end
(* fxinterp_binds' step for one bind, given the rest's code *)
with ccb (b : xbind) (cr : bcode) {struct b} : bcode :=
  match b with
  | XBTy t tau => fun n d g st => match n with 0 => inr (XR XFuel g st) | S k => cr k (rbind_ty  P  d t tau) g st end
  | XBVal v e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match c1 k d g st with
               | XR (XOk w1) g1 st1 => cr k (cpush (IVal v, FVal w1) d) g1 st1
               | rr => inr rr
               end
      end
  | XBFn f f' pl => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rcompose d pl with
               | Some dc => cr k (cpush (IFn f, FClo f' (rclose d dc)) d) g st
               | None => inr (XR (XErr e_compose) g st)
               end
      end
  | XBMth tk m th f' pl => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rcompose d pl with
               | Some dc => cr k (cpush (IMth tk m th, FClo f' (rclose d dc)) d) g st
               | None => inr (XR (XErr e_compose) g st)
               end
      end
  | XBFun u h sm pl => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rcompose d pl with
               | Some dc => cr k (cpush (IFun u, FFun h [sm] (cache_raw (rclose d dc))) d) g st
               | None => inr (XR (XErr e_compose) g st)
               end
      end
  | XBFunA u u' sm => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rlook (IFun u') d with
               | Some (FFun h sms dc) => cr k (cpush (IFun u, FFun h (sms ++ [sm]) dc) d) g st
               | _ => inr (XR (XErr e_fun) g st)
               end
      end
  | XBApply u th pl => let ca := capply u th pl in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match ca k d st with
               | inl (dr, st1) => cr k (cappend dr d) g st1
               | inr rr => inr (with_g g rr)
               end
      end
  | XBApplyR u inputs outputs => let ca := capply_r u inputs outputs in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match ca k d st with
               | inl (dr, st1) => cr k (cappend dr d) g st1
               | inr rr => inr (with_g g rr)
               end
      end
  | XBFunSym u h => fun n d g st =>
      match n with 0 => inr (XR XFuel g st) | S k => cr k (cpush (IFun u, FFunSym h) d) g st end
  | XBFunCopy u u' => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rlook (IFun u') d with
               | Some en => cr k (cpush (IFun u, en) d) g st
               | None => inr (XR (XErr e_fun) g st)
               end
      end
  | XBApplyBr u brs =>
      let brs' := map (fun br => match br with (h, bs, pairs) =>
                  (h, fold_right ccb bnil bs, pairs) end) brs in
      let cb := cbranch u brs' in
      fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match cb k d g st with
               | inl (dr, g1, st1) => cr k (cappend dr d) g1 st1
               | inr rr => inr rr
               end
      end
  | XBPrim i p => fun n d g st =>
      match n with 0 => inr (XR XFuel g st) | S k => cr k (cpush (i, rprim_entry d i p) d) g st end
  end.

Definition ccl (l : list xexpr) : lcode :=
  fold_right (fun e cr => match e with
                          | XVar x => lvar x cr
                          | XVl v => lval (IVal v) cr
                          | _ => let c := cc e in lcons c cr end) lnil l.

Definition ccbs (bs : list xbind) : bcode := fold_right ccb bnil bs.

Definition cbrs_c (brs : list (usym * list xbind * smap)) : list (usym * bcode * smap) :=
  map (fun br => match br with (h, bs, pairs) => (h, ccbs bs, pairs) end) brs.

End CC.
Definition crun_main(tbl:fsym->option code)(fuel:nat)(main:fsym)(d0:xdenv)(st0:mstate):xres:=
 match tbl main with Some c=>ret_call[](c fuel(conv_env d0)[]st0)|None=>XR(XErr e_nobody)[]st0 end.
