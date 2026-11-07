(* Split runtime environments with deferred type substitution.
   LazyEnvFacts relates every observable lookup to the reference environment;
   LazyInterpFacts proves exact interpreter agreement, including fuel/errors.

   Coinductive values use Rocq's standard lazy extraction. A type binding holds
   its substitution until a cast or allocating primitive observes it. Captured
   environments keep their type projection, so entering a closure does not
   rescan its ordinary bindings. No new extraction directive or axiom is used. *)
From Stdlib Require Import List Bool Arith Lia PrimInt63.
From NM Require Import Audit MossX FastX.
Import ListNotations.
Set Implicit Arguments.
Local Open Scope nat_scope.
CoInductive delayed_ty : Type := Hold : ty -> delayed_ty.
Definition force_ty (v:delayed_ty) : ty := match v with Hold tau=>tau end.
(* Suspend each type stream as a whole. Instantiating seven type parameters
   need not allocate seven entries until a type lookup actually observes them.
   Appending/filtering streams is likewise delayed and memoized by extraction.
   Instantiation and call suspensions capture the caller's type projection,
   not its ordinary value stream or machine state. *)
CoInductive delayed_rows (A:Type) : Type := Rows : list(tsym*A)->delayed_rows A.
Arguments Rows {A}.
Definition force_rows {A}(v:delayed_rows A):list(tsym*A):=match v with Rows rows=>rows end.
CoFixpoint defer_cons {A}(t:tsym)(en:A)(rows:delayed_rows A):delayed_rows A:=
 Rows((t,en)::force_rows rows).
CoFixpoint defer_append {A}(a b:delayed_rows A):delayed_rows A:=
 Rows(force_rows a++force_rows b).
Lemma force_cons : forall A t (en:A) rows,
 force_rows(defer_cons t en rows)=(t,en)::force_rows rows.
Proof. reflexivity. Qed.
Lemma force_append : forall A(a b:delayed_rows A),
 force_rows(defer_append a b)=force_rows a++force_rows b.
Proof. reflexivity. Qed.
Inductive fentry : Type :=
| FVal : xvalue->fentry
| FClo : fsym->fenv->fentry
| FFun : usym->list smap->fenv->fentry
| FPrim : prim->fentry
| FFunSym : usym->fentry
| FType : delayed_ty->fentry
| FPrimAt : prim->ty->fentry
with fenv : Type :=
(* The streams preserve first-match order. The second contains every ITy row.
   None in the third field shares that stream as the proper-type projection;
   Some stores a separate projection when malformed type rows occur.
   xdtypes skips a malformed row without hiding a later proper type, whereas
   ordinary ITy lookup sees that malformed row. *)
| FEnv : list(item*fentry)->delayed_rows fentry->option(delayed_rows fentry)->fenv.
Definition fempty := FEnv [] (Rows []) None.
Fixpoint tlookup t (xs:list(tsym*fentry)) := match xs with
 |[]=>None|(u,en)::rest=>if Nat.eqb t u then Some en else tlookup t rest end.
Fixpoint dlookup i (xs:list(item*fentry)) := match xs with
 |[]=>None|(j,en)::rest=>if item_eqb i j then Some en else dlookup i rest end.
Definition flookup i d := match d with FEnv values types _=>match i with
 |ITy t=>tlookup t(force_rows types)|_=>dlookup i values end end.
Definition fadd i en d := match d with FEnv values types meta=>match i with
 |ITy t=>FEnv values (defer_cons t en types)
   (match en with
    |FType _=>option_map(fun rows=>defer_cons t en rows)meta
    |_=>Some(match meta with Some rows=>rows|None=>types end)end)
 |_=>FEnv ((i,en)::values) types meta end end.
(* Left-biased append preserves duplicate-key behavior in all three streams. *)
Definition fmerge a b := match a,b with FEnv av atp am,FEnv bv btp bm=>
 FEnv(av++bv)(defer_append atp btp)
 (match am,bm with None,None=>None|_,_=>
 Some(defer_append (match am with Some rows=>rows|None=>atp end)
      (match bm with Some rows=>rows|None=>btp end))end)end.
Fixpoint fcompose (d:fenv)(sm:smap):option fenv:=match sm with
 |[]=>Some fempty
 |(req,src)::rest=>match flookup src d,fcompose d rest with
 |Some en,Some tail=>Some(fadd req en tail)|_,_=>None end end.
Fixpoint fchain (d:fenv)(sms:list smap):option fenv:=match sms with
 |[]=>Some d|sm::rest=>match fchain d rest with Some dm=>fcompose dm sm|None=>None end end.
Definition meta d:=match d with FEnv _ types more=>match more with Some rows=>rows|None=>types end end.
Definition type_get_meta types t :option ty:=match tlookup t(force_rows types) with
 |Some(FType delayed)=>Some(force_ty delayed)|_=>None end.
Definition type_get d:=type_get_meta(meta d).
Fixpoint ground (get:tsym->option ty)(tau:ty):ty:=match tau with
 |TUnit=>TUnit|TAbs t=>match get t with Some actual=>actual|None=>TAbs t end
 |TNom n th=>TNom n((fix go l:=match l with []=>[]|(t,v)::r=>(t,ground get v)::go r end)th)
 |TUnion ts=>TUnion((fix go l:=match l with []=>[]|t::r=>ground get t::go r end)ts)end.
Definition fground d tau:=ground(type_get d)tau.
(* Capture only the type projection, never the current machine state or the
   ordinary environment. Extraction memoizes this delayed computation. *)
CoFixpoint defer_ground types tau : delayed_ty := Hold(ground(type_get_meta types)tau).
Lemma force_defer_ground:forall types tau,
 force_ty(defer_ground types tau)=ground(type_get_meta types)tau.
Proof. reflexivity. Qed.
Definition fattach_types d dc:=match dc with FEnv values types more=>
 FEnv values(defer_append types(meta d))(option_map(fun rows=>defer_append rows(meta d))more)end.
Definition fbind_ty P d t tau:=if xreify P then fadd(ITy t)(FType(defer_ground(meta d)tau))d else d.
(* Every instantiation row is a proper type. Build one shared stream on its
   first observation; ordinary value/function lookups leave it suspended. *)
CoFixpoint inst_rows types th:delayed_rows fentry:=
 Rows(map(fun p=>(fst p,FType(defer_ground types(snd p))))th).
Lemma force_inst_rows : forall types th,force_rows(inst_rows types th)=
 map(fun p=>(fst p,FType(defer_ground types(snd p))))th.
Proof. reflexivity. Qed.
Definition finst d th:=FEnv [] (inst_rows(meta d)th)None.
(* A call needs one suspended concatenation, rather than intermediate
   instantiation/attachment environments and three suspended appends. *)
CoFixpoint call_rows caller th captured:delayed_rows fentry:=Rows(
 map(fun p=>(fst p,FType(defer_ground caller(snd p))))th++
 (force_rows captured++force_rows caller)).
Lemma force_call_rows : forall caller th captured,force_rows(call_rows caller th captured)=
 map(fun p=>(fst p,FType(defer_ground caller(snd p))))th++
 (force_rows captured++force_rows caller).
Proof. reflexivity. Qed.
Definition fcall_env P d th dc:=if xreify P then
 match dc with FEnv values types more=>
 FEnv values(call_rows(meta d)th types)(option_map(call_rows(meta d)th)more)end
 else dc.
Definition fapply_env P d h th dc:=fcall_env P d
 (filter(fun p=>existsb(Nat.eqb(fst p))(xp_inputs P h))th)dc.
CoFixpoint result_rows Dr rows:delayed_rows fentry:=
 Rows(filter(fun p=>existsb(item_eqb(ITy(fst p)))Dr)(force_rows rows)).
Lemma force_result_rows : forall Dr rows,force_rows(result_rows Dr rows)=
 filter(fun p=>existsb(item_eqb(ITy(fst p)))Dr)(force_rows rows).
Proof. reflexivity. Qed.
Definition fresult_types Dr d:=FEnv [] (result_rows Dr(meta d))None.
Definition fresult_env P d Dr dr:=if xreify P then fmerge dr(fresult_types Dr d)else dr.
Definition fprim_entry d i p:=match p,i with
 |PBox _,IMth tau _ _|PArrNew _,IMth tau _ _=>FPrimAt p(fground d tau)|_,_=>FPrim p end.
Definition fcast P d n th w:=match lookup_cast n(xp_cast P)with
 |None=>XOk(XVTag n w)
 |Some spec=>let enabled:=match xc_handle spec with None=>true|Some(h,expected)=>
  ty_eqb(fground d(match lookup_subst h th with Some t=>t|None=>TAbs h end))expected end in
  if enabled then match lookup_subst(xc_held spec)th,w with
  |Some tau,XVRefAt k tau' _=>if Bool.eqb(xc_array spec)k&&ty_eqb(fground d tau)tau'
   then XOk(XVTag n w)else XTrap 37|_,_=>XTrap 37 end else XOk(XVTag n w)end.
Fixpoint conv_entry (en : xdentry) : fentry :=
  match en with
  | XDVal w => FVal w
  | XDClo f d =>
      FClo f ((fix go (l : list (item * xdentry)) : fenv :=
                 match l with [] => fempty | (i, e) :: r => fadd i (conv_entry e) (go r) end) d)
  | XDFun h sms d =>
      FFun h sms ((fix go (l : list (item * xdentry)) : fenv :=
                     match l with [] => fempty | (i, e) :: r => fadd i (conv_entry e) (go r) end) d)
  | XDPrim p => FPrim p
  | XDFunSym h => FFunSym h
  | XDType tau => FType (Hold tau)
  | XDPrimAt p tau => FPrimAt p tau
  end.

Fixpoint conv_env (l : list (item * xdentry)) : fenv :=
  match l with [] => fempty | (i, e) :: r => fadd i (conv_entry e) (conv_env r) end.

Fixpoint fxinterp (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (e : xexpr) {struct fuel} : xres :=
  match fuel with
  | 0 => XR XFuel g st
  | S k =>
      match e with
      | XVar x => match lookup_xvenv x g with
                  | Some w => XR (XOk w) g st
                  | None => XR (XErr e_var) g st
                  end
      | XUnit => XR (XOk XVUnit) g st
      | XVl v => match flookup (IVal v) d with
                 | Some (FVal w) => XR (XOk w) g st
                 | _ => XR (XErr e_val) g st
                 end
      | XTag n th e1 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (fcast P d n th w) g1 st1
          | r => r
          end
      | XCallD f th sm args =>
          match xp_fn P f with
          | Some body =>
              match fxinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  match fcompose d sm with
                  | Some d' => ret_call g1 (fxinterp k P (fcall_env P d th d') (mk_xvenv ws) st1 body)
                  | None => XR (XErr e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => XR (XErr e_nobody) g st
          end
      | XCallA f args =>
          match flookup (IFn f) d with
          | Some (FClo f' df) =>
              match xp_fn P f' with
              | Some body =>
                  match fxinterp_list k P d g st args with
                  | inl (ws, g1, st1) => ret_call g1 (fxinterp k P df (mk_xvenv ws) st1 body)
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (FPrim p) =>
              match fxinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p None ws st1 in XR o g1 st2
              | inr r => r
              end
          | Some (FPrimAt p tau) =>
              match fxinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p (Some tau) ws st1 in XR o g1 st2
              | inr r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMeth e0 m prov args =>
          match flookup prov d with
          | Some (FClo f' d') =>
              match xp_fn P f' with
              | Some body =>
                  match fxinterp k P d g st e0 with
                  | XR (XOk w0) g0 st0 =>
                      match fxinterp_list k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (fxinterp k P d' (mk_xvenv (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (FPrim p) =>
              match fxinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match fxinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p None (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | Some (FPrimAt p tau) =>
              match fxinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match fxinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p (Some tau) (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMatch e0 arms =>
          match fxinterp k P d g st e0 with
          | XR (XOk (XVTag n w')) g1 st1 =>
              match find_xarm n arms with
              | Some (x, eb) => pop1 (fxinterp k P d ((x, w') :: g1) st1 eb)
              | None => XR (XErr e_match) g1 st1
              end
          | XR (XOk _) g1 st1 => XR (XErr e_match) g1 st1
          | r => r
          end
      | XLet x e1 e2 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => pop1 (fxinterp k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | XBindTy t tau e1 => fxinterp k P (fbind_ty P d t tau) g st e1
      | XBindVal v e1 e2 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => fxinterp k P (fadd (IVal v) (FVal w1) d) g1 st1 e2
          | r => r
          end
      | XBindFn f f' sm e1 =>
          match fcompose d sm with
          | Some dc => fxinterp k P (fadd (IFn f) (FClo f' (fattach_types d dc)) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindMth tauk m th f' sm e1 =>
          match fcompose d sm with
          | Some dc => fxinterp k P (fadd (IMth tauk m th) (FClo f' (fattach_types d dc)) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFun u h sm smc e1 =>
          match fcompose d smc with
          | Some dc => fxinterp k P (fadd (IFun u) (FFun h [sm] (fattach_types d dc)) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFunA u u' sm e1 =>
          match flookup (IFun u') d with
          | Some (FFun h sms dc) =>
              fxinterp k P (fadd (IFun u) (FFun h (sms ++ [sm]) dc) d) g st e1
          | _ => XR (XErr e_fun) g st
          end
      | XApply u th sm e1 =>
          match fxinterp_apply k P d u th sm st with
          | inl (dr, st1) => fxinterp k P (fmerge dr d) g st1 e1
          | inr r => with_g g r
          end
      | XApplyR u inputs outputs e1 =>
          match fxinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => fxinterp k P (fmerge dr d) g st1 e1
          | inr rr => with_g g rr
          end
      | XSeq e1 e2 =>
          match fxinterp k P d g st e1 with
          | XR (XOk _) g1 st1 => fxinterp k P d g1 st1 e2
          | r => r
          end
      | XAssign x e1 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match assign_xvenv x w g1 with
              | Some g2 => XR (XOk XVUnit) g2 st1
              | None => XR (XErr e_assign) g1 st1
              end
          | r => r
          end
      | XIf c e1 e2 =>
          match fxinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true => fxinterp k P d g1 st1 e1
              | Some false => fxinterp k P d g1 st1 e2
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XWhile c b =>
          match fxinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true =>
                  match fxinterp k P d g1 st1 b with
                  | XR (XOk _) g2 st2 => fxinterp k P d g2 st2 (XWhile c b)
                  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
                  | r => r
                  end
              | Some false => XR (XOk XVUnit) g1 st1
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XLoop b =>
          match fxinterp k P d g st b with
          | XR (XOk _) g2 st2 => fxinterp k P d g2 st2 (XLoop b)
          | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
          | r => r
          end
      | XReturn e1 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (XRet w) g1 st1
          | r => r
          end
      | XBreak => XR XBrk g st
      | XRec fs =>
          match fxinterp_list k P d g st (map snd fs) with
          | inl (ws, g1, st1) => XR (XOk (XVRec (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | XField e1 f =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match get_field f w with
              | Some v => XR (XOk v) g1 st1
              | None => XR (XErr e_field) g1 st1
              end
          | r => r
          end
      | XMatchP e0 arms =>
          match fxinterp k P d g st e0 with
          | XR (XOk w) g1 st1 =>
              match select_arm arms w with
              | Some (bs, eb) =>
                  popn (List.length bs) (fxinterp k P d (bs ++ g1) st1 eb)
              | None => XR (XErr e_pat) g1 st1
              end
          | r => r
          end
      | XBindFunSym u h e1 => fxinterp k P (fadd (IFun u) (FFunSym h) d) g st e1
      | XBindFunCopy u u' e1 =>
          match flookup (IFun u') d with
          | Some en => fxinterp k P (fadd (IFun u) en d) g st e1
          | None => XR (XErr e_fun) g st
          end
      | XApplyBr u brs e1 =>
          match fxinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => fxinterp k P (fmerge dr d) g1 st1 e1
          | inr r => r
          end
      | XBindPrim i p e1 => fxinterp k P (fadd i (fprim_entry d i p) d) g st e1
      | XConst _ initializer => match k with O => XR XFuel g st
          | S _ => fxinterp k P d g st initializer end
      end
  end
with fxinterp_list (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (l : list xexpr) {struct fuel} : (list xvalue * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match fxinterp_list k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with fxinterp_binds (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (bs : list xbind) {struct fuel} : (fenv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | XBTy t tau :: r => fxinterp_binds k P (fbind_ty P d t tau) g st r
      | XBVal v e1 :: r =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => fxinterp_binds k P (fadd (IVal v) (FVal w1) d) g1 st1 r
          | rr => inr rr
          end
      | XBFn f f' sm :: r =>
          match fcompose d sm with
          | Some dc => fxinterp_binds k P (fadd (IFn f) (FClo f' (fattach_types d dc)) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBMth tauk m th f' sm :: r =>
          match fcompose d sm with
          | Some dc =>
              fxinterp_binds k P (fadd (IMth tauk m th) (FClo f' (fattach_types d dc)) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFun u h sm smc :: r =>
          match fcompose d smc with
          | Some dc => fxinterp_binds k P (fadd (IFun u) (FFun h [sm] (fattach_types d dc)) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFunA u u' sm :: r =>
          match flookup (IFun u') d with
          | Some (FFun h sms dc) =>
              fxinterp_binds k P (fadd (IFun u) (FFun h (sms ++ [sm]) dc) d) g st r
          | _ => inr (XR (XErr e_fun) g st)
          end
      | XBApply u th sm :: r =>
          match fxinterp_apply k P d u th sm st with
          | inl (dr, st1) => fxinterp_binds k P (fmerge dr d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBApplyR u inputs outputs :: r =>
          match fxinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => fxinterp_binds k P (fmerge dr d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBFunSym u h :: r => fxinterp_binds k P (fadd (IFun u) (FFunSym h) d) g st r
      | XBFunCopy u u' :: r =>
          match flookup (IFun u') d with
          | Some en => fxinterp_binds k P (fadd (IFun u) en d) g st r
          | None => inr (XR (XErr e_fun) g st)
          end
      | XBApplyBr u brs :: r =>
          match fxinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => fxinterp_binds k P (fmerge dr d) g1 st1 r
          | inr rr => inr rr
          end
      | XBPrim i p :: r => fxinterp_binds k P (fadd i (fprim_entry d i p) d) g st r
      end
  end
with fxinterp_apply (fuel : nat) (P : xprog) (d : fenv) (u : usym) (th : subst)
    (sm : smap) (st : mstate) {struct fuel} : (fenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match flookup (IFun u) d with
      | Some (FFun h sms dc) =>
          match xp_fun P h with
          | Some (Dr, bs) =>
              match fcompose d sm with
              | Some d1 =>
                  match fchain d1 sms with
                  | Some d2 =>
                      match fxinterp_binds k P (fapply_env P d h th (fmerge d2 dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match fcompose d3 (inst_smap th Dr) with
                          | Some dr => inl (fresult_env P d3 Dr dr, st3)
                          | None => inr (XR (XErr e_compose) [] st3)
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
  end
with fxinterp_apply_r (fuel : nat) (P : xprog) (d : fenv) (u : usym) (inputs outputs : smap) (st : mstate) {struct fuel} : (fenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match flookup (IFun u) d with
      | Some (FFun h sms dc) =>
          match xp_fun P h with
          | Some (_, bs) =>
              match fcompose d inputs with
              | Some d1 =>
                  match fchain d1 sms with
                  | Some d2 =>
                      match fxinterp_binds k P (fmerge d2 dc) [] st bs with
                      | inl (d3, _, st3) =>
                          match fcompose d3 (xp_exports P h) with
                          | Some named =>
                              match fcompose named outputs with
                              | Some dr => inl (dr, st3)
                              | None => inr (XR (XErr e_compose) [] st3)
                              end
                          | None => inr (XR (XErr e_compose) [] st3)
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
  end
with fxinterp_branch (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (u : usym) (brs : list (usym * list xbind * smap)) {struct fuel}
    : (fenv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match flookup (IFun u) d with
      | Some (FFunSym h) =>
          match find_branch h brs with
          | Some (bs, pairs) =>
              match fxinterp_binds k P d g st bs with
              | inl (d3, g3, st3) =>
                  match fcompose d3 pairs with
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

Definition frun_main (fuel : nat) (P : xprog) (main : fsym) (d0 : xdenv)
    (st0 : mstate) : xres :=
  match xp_fn P main with
  | Some body => ret_call [] (fxinterp fuel P (conv_env d0) [] st0 body)
  | None => XR (XErr e_nobody) [] st0
  end.

