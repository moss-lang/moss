(* ------------------------------------------------------------------------ *)
(* CloX.v — closure compilation of SlotX's pre-resolved programs.            *)
(*                                                                           *)
(* `cc e` turns a pre-resolved expression into a Gallina function (an OCaml *)
(* closure once extracted) that does, fuel step for fuel step, what         *)
(* `SlotX.rinterp` does on `e`; the dispatch on `e` happens once, at        *)
(* compile time.  Leaves (variables, vals, unit) in argument lists are      *)
(* evaluated in place, without building an intermediate `xres`.             *)
(*                                                                           *)
(* A call needs the callee's code.  Code cannot contain the table it is     *)
(* looked up in (the type would not be strictly positive), so the table is  *)
(* a parameter `tbl` with the obligation `tbl_ok`: tbl f is the closure     *)
(* compilation of rp_fn R f.  The driver builds it as an array it fills     *)
(* with closures that read the array (a knot), which satisfies tbl_ok as    *)
(* the array realizes `nlook` for program_of's xp_fn.                        *)
(*                                                                           *)
(* Proved (kernel primitives only):                                         *)
(*  - cc_agrees: under tbl_ok, `cc e fuel d g st = rinterp fuel R d g st e` *)
(*    for every fuel, environment, locals, machine and e;                   *)
(*  - crun_main_eq: crun_main = SlotX.rrun_main, hence (slot_run_main) =    *)
(*    XS.run_main on program_of p when R = rprog_of p;                     *)
(*  - clo_oracle_agrees: with pruning, as Prune.oracle_agrees.              *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia PrimInt63.
From NM Require Import Audit CoreMoss MossX FastX Elab Prune SlotX FoldX CloScalar.
Import ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

Definition code := nat -> renv -> xvenv -> mstate -> xres.
Definition lcode := nat -> renv -> xvenv -> mstate -> (list xvalue * xvenv * mstate) + xres.
Definition bcode := nat -> renv -> xvenv -> mstate -> (renv * xvenv * mstate) + xres.

Section CC.
Variable R : rprog.
Variable tbl : fsym -> option (code * code).
Variable ftbl : usym -> option (tele * bcode * option(nat*renv)).

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
Definition capply (l : rlk) (th : subst) (pl : plan) : nat -> renv -> mstate -> (renv * mstate) + xres :=
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
                      | None => match cb k (rapply_env (rp_x R) d h th (d2 ++ dc)) [] st with
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

Definition capply_r (l : rlk) (pl : plan) (outputs : smap) : nat -> renv -> mstate -> (renv * mstate) + xres :=
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
                      match cb k (d2 ++ dc) [] st with
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
  : nat -> renv -> xvenv -> mstate -> (renv * xvenv * mstate) + xres :=
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
Definition scode := renv -> xvenv -> mstate -> option (xvalue * mstate).
Definition slcode := renv -> xvenv -> mstate -> option (list xvalue * mstate).

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

Lemma csev_eq_n : forall n e d g st, sdepth e <= n -> csev e d g st = sev P d g st e.
Proof.
  induction n as [|n IH]; intros e d g st Hn; [destruct e; simpl in Hn; lia|].
  assert (HL : forall l d g st, needed l <= S n -> csevl l d g st = sevl P d g l st).
  { induction l as [|a r IHr]; intros d0 g0 st0 Hl; [reflexivity|].
    simpl in Hl. change (csevl (a :: r)) with (cscons (csev a) (csevl r)). unfold cscons.
    simpl sevl. rewrite IH by lia.
    destruct (sev P d0 g0 st0 a) as [[w st1]|]; auto. rewrite IHr by lia. reflexivity. }
  destruct e as [x|l|t e1|l args|j args body|e0 p b]; simpl in Hn.
  - reflexivity.
  - reflexivity.
  - simpl. rewrite IH by lia. reflexivity.
  - change (csev (SPrim l args) d g st) with
      (match rlook l d with
       | Some (QPrim p) => match csevl args d g st with
                           | Some (ws, st1) => match run_prim_at P p None ws st1 with (XOk w, st2) => Some (w, st2) | _ => None end
                           | None => None end
       | _ => None end).
    rewrite sev_prim. destruct (rlook l d) as [[]|]; auto.
    fold (needed args) in Hn. rewrite HL by lia. reflexivity.
  - change (csev (SCall j args body) d g st) with
      (match rnth2 j d with
       | Some _ => match csevl args d g st with Some (ws, st1) => csev body d (mkv ws) st1 | None => None end
       | None => None end).
    rewrite sev_call. fold (needed args) in Hn. destruct (rnth2 j d); auto.
    rewrite HL by lia. destruct (sevl P d g args st) as [[ws st1]|]; auto. apply IH. lia.
  - simpl. rewrite IH by lia. destruct (sev P d g st e0) as [[w st1]|]; auto.
    destruct (match_pat p w); auto. apply IH. lia.
Qed.

Lemma csev_eq : forall e d g st, csev e d g st = sev P d g st e.
Proof. intros. apply csev_eq_n with (n := sdepth e). lia. Qed.

Lemma csevl_eq : forall l d g st, csevl l d g st = sevl P d g l st.
Proof.
  induction l as [|a r IH]; intros d g st; [reflexivity|].
  change (csevl (a :: r)) with (cscons (csev a) (csevl r)). unfold cscons. simpl sevl.
  rewrite csev_eq. destruct (sev P d g st a) as [[w st1]|]; auto. rewrite IH. reflexivity.
Qed.

Lemma cfast_eq : forall ss sb pl d g st,
  match csevl ss d g st with
  | Some (ws, st1) => match rcompose d pl with
                      | Some d' => csev sb d' (mkv ws) st1
                      | None => None
                      end
  | None => None
  end = fastcall P d g st pl ss sb.
Proof.
  intros. unfold fastcall. rewrite csevl_eq.
  destruct (sevl P d g ss st) as [[ws st1]|]; auto.
  destruct (rcompose d pl); auto. apply csev_eq.
Qed.

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
                      ret_call g1 ((if b then cs else cd) k df (mkv ws) st1)
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
                          ret_call g1 ((if b then cs else cd) k d' (mkv (w0 :: ws)) st1)
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
               | XR (XOk w1) g1 st1 => c2 k ((IVal v, QVal w1) :: d) g1 st1
               | r => r
               end
      end
  | RBindClo i f' b pl e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rcompose d pl with
               | Some dc => c1 k ((i, QClo (rflag d b) f' (rclose d dc)) :: d) g st
               | None => XR (XErr e_compose) g st
               end
      end
  | RBindFun u h sm pl e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rcompose d pl with
               | Some dc => c1 k ((IFun u, QFun h [sm] (rclose d dc)) :: d) g st
               | None => XR (XErr e_compose) g st
               end
      end
  | RBindFunA u l sm e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rlook l d with
               | Some (QFun h sms dc) => c1 k ((IFun u, QFun h (sms ++ [sm]) dc) :: d) g st
               | _ => XR (XErr e_fun) g st
               end
      end
  | RApply l th pl e1 => let c1 := cc e1 in let ca := capply l th pl in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match ca k d st with
               | inl (dr, st1) => c1 k (dr ++ d) g st1
               | inr r => with_g g r
               end
      end
  | RApplyR l inputs outputs e1 => let c1 := cc e1 in let ca := capply_r l inputs outputs in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match ca k d st with
               | inl (dr, st1) => c1 k (dr ++ d) g st1
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
      fix loop (n : nat) (d : renv) (g : xvenv) (st : mstate) {struct n} : xres :=
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
      fix loop (n : nat) (d : renv) (g : xvenv) (st : mstate) {struct n} : xres :=
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
      match n with 0 => XR XFuel g st | S k => c1 k ((IFun u, QFunSym h) :: d) g st end
  | RBindFunCopy u l e1 => let c1 := cc e1 in fun n d g st =>
      match n with
      | 0 => XR XFuel g st
      | S k => match rlook l d with
               | Some en => c1 k ((IFun u, en) :: d) g st
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
               | inl (dr, g1, st1) => c1 k (dr ++ d) g1 st1
               | inr r => r
               end
      end
  | RBindPrim i p e1 => let c1 := cc e1 in fun n d g st =>
      match n with 0 => XR XFuel g st | S k => c1 k ((i, rprim_entry d i p) :: d) g st end
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
               | XR (XOk w1) g1 st1 => cr k ((IVal v, QVal w1) :: d) g1 st1
               | rr => inr rr
               end
      end
  | RBClo i f' b pl => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rcompose d pl with
               | Some dc => cr k ((i, QClo (rflag d b) f' (rclose d dc)) :: d) g st
               | None => inr (XR (XErr e_compose) g st)
               end
      end
  | RBFun u h sm pl => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rcompose d pl with
               | Some dc => cr k ((IFun u, QFun h [sm] (rclose d dc)) :: d) g st
               | None => inr (XR (XErr e_compose) g st)
               end
      end
  | RBFunA u l sm => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rlook l d with
               | Some (QFun h sms dc) => cr k ((IFun u, QFun h (sms ++ [sm]) dc) :: d) g st
               | _ => inr (XR (XErr e_fun) g st)
               end
      end
  | RBApply l th pl => let ca := capply l th pl in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match ca k d st with
               | inl (dr, st1) => cr k (dr ++ d) g st1
               | inr rr => inr (with_g g rr)
               end
      end
  | RBApplyR l inputs outputs => let ca := capply_r l inputs outputs in fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match ca k d st with
               | inl (dr, st1) => cr k (dr ++ d) g st1
               | inr rr => inr (with_g g rr)
               end
      end
  | RBFunSym u h => fun n d g st =>
      match n with 0 => inr (XR XFuel g st) | S k => cr k ((IFun u, QFunSym h) :: d) g st end
  | RBFunCopy u l => fun n d g st =>
      match n with
      | 0 => inr (XR XFuel g st)
      | S k => match rlook l d with
               | Some en => cr k ((IFun u, en) :: d) g st
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
               | inl (dr, g1, st1) => cr k (dr ++ d) g1 st1
               | inr rr => inr rr
               end
      end
  | RBPrim i p => fun n d g st =>
      match n with 0 => inr (XR XFuel g st) | S k => cr k ((i, rprim_entry d i p) :: d) g st end
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

(* ===================== Agreement ======================================== *)

Section Agree.
Variable R : rprog.
Variable tbl : fsym -> option (code * code).
Variable ftbl : usym -> option (tele * bcode * option(nat*renv)).
(* the table holds the code of R's bodies *)
Hypothesis Htbl : forall f, tbl f =
  match rp_fn R f with Some (bs, bd) => Some (cc R tbl ftbl bs, cc R tbl ftbl bd) | None => None end.
Hypothesis Hftbl : forall h, ftbl h =
  match rp_fun R h with Some (Dr, bs) => Some (Dr, ccbs R tbl ftbl bs, prepare_scalar Dr bs) | None => None end.

Local Notation cc := (cc R tbl ftbl).
Local Notation ccb := (ccb R tbl ftbl).
Local Notation ccl := (ccl R tbl ftbl).
Local Notation ccbs := (ccbs R tbl ftbl).
Local Notation capply := (capply R ftbl).
Local Notation capply_r := (capply_r R ftbl).
Local Notation cbrs_c := (cbrs_c R tbl ftbl).

Definition crec (fs : list (fld * rexpr)) : lcode :=
  fold_right (fun p cr => match p with (_, e) => let c := cc e in lcons c cr end) lnil fs.

Lemma rfind_carm_cc : forall t arms,
  rfind_carm t (map (fun a => match a with (t, x, eb) => (t, x, cc eb) end) arms)
  = match rfind_arm t arms with Some (x, eb) => Some (x, cc eb) | None => None end.
Proof. intros t arms. induction arms as [|[[t' x] eb] r IH]; simpl; auto. destruct (Nat.eqb t t'); auto. Qed.

Lemma rselect_carm_cc : forall arms w,
  rselect_carm (map (fun a => match a with (p, eb) => (p, cc eb) end) arms) w
  = match rselect_arm arms w with Some (bs, eb) => Some (bs, cc eb) | None => None end.
Proof. intros arms w. induction arms as [|[p eb] r IH]; simpl; auto. destruct (match_pat p w); auto. Qed.

Lemma rfind_cbranch_cc : forall h brs,
  rfind_cbranch h (cbrs_c brs)
  = match rfind_branch h brs with Some (bs, pairs) => Some (ccbs bs, pairs) | None => None end.
Proof. intros h brs. induction brs as [|[[h' bs] pairs] r IH]; simpl; auto. destruct (Nat.eqb h h'); auto. Qed.

Lemma cc_while : forall c b k d g st,
  cc (RWhile c b) (S k) d g st =
  match cc c k d g st with
  | XR (XOk w) g1 st1 =>
      match truthy (rp_x R) w with
      | Some true =>
          match cc b k d g1 st1 with
          | XR (XOk _) g2 st2 => cc (RWhile c b) k d g2 st2
          | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
          | r => r
          end
      | Some false => XR (XOk XVUnit) g1 st1
      | None => XR (XErr e_bool) g1 st1
      end
  | r => r
  end.
Proof. reflexivity. Qed.

Lemma cc_loop : forall b k d g st,
  cc (RLoop b) (S k) d g st =
  match cc b k d g st with
  | XR (XOk _) g2 st2 => cc (RLoop b) k d g2 st2
  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
  | r => r
  end.
Proof. reflexivity. Qed.

Definition AGREE (n : nat) : Prop :=
  (forall e d g st, cc e n d g st = rinterp n R d g st e)
  /\ (forall l d g st, ccl l n d g st = rinterp_list n R d g st l)
  /\ (forall fs d g st, crec fs n d g st = rinterp_list n R d g st (map snd fs))
  /\ (forall bs d g st, ccbs bs n d g st = rinterp_binds n R d g st bs)
  /\ (forall l th pl d st, capply l th pl n d st = rinterp_apply n R d l th pl st)
  /\ (forall l brs d g st, cbranch l (cbrs_c brs) n d g st = rinterp_branch n R d g st l brs)
  /\(forall l inputs outputs d st,capply_r l inputs outputs n d st=rinterp_apply_r n R d l inputs outputs st).

Ltac fold_lists :=
  repeat match goal with
  | |- context [fold_right ?F lnil ?a] =>
      lazymatch type of a with
      | list rexpr => change (fold_right F lnil a) with (ccl a)
      | list (fld * rexpr) => change (fold_right F lnil a) with (crec a)
      end
  | |- context [map ?F ?brs] =>
      lazymatch type of brs with
      | list (usym * list rbind * plan) => change (map F brs) with (cbrs_c brs)
      end
  end.

Lemma agree_step : forall k, AGREE k -> AGREE (S k).
Proof.
  intros k [IHe [IHl [IHr [IHb [IHa [IHbr IHar]]]]]].
  Ltac dig IHe IHl IHr IHb IHa IHbr IHar :=
    repeat (match goal with
    | |- context [match csevl ?R ?ss ?d ?g ?st with _ => _ end] => rewrite cfast_eq
    | |- context [cc ?e ?k ?d ?g ?st] => rewrite (IHe e d g st)
    | |- context [ccl ?l ?k ?d ?g ?st] => rewrite (IHl l d g st)
    | |- context [crec ?l ?k ?d ?g ?st] => rewrite (IHr l d g st)
    | |- context [ccbs ?l ?k ?d ?g ?st] => rewrite (IHb l d g st)
    | |- context [capply ?l ?th ?pl ?k ?d ?st] => rewrite (IHa l th pl d st)
    | |- context [capply_r ?l ?inputs ?outputs ?k ?d ?st] => rewrite(IHar l inputs outputs d st)
    | |- context [cbranch ?l (cbrs_c ?brs) ?k ?d ?g ?st] => rewrite (IHbr l brs d g st)
    | |- context [match rinterp ?k ?R ?d ?g ?st ?e with _ => _ end] =>
        destruct (rinterp k R d g st e) as [[| | | | | | |] ? ?]
    | |- context [match rinterp_list ?k ?R ?d ?g ?st ?e with _ => _ end] =>
        destruct (rinterp_list k R d g st e) as [[[? ?] ?]|?]
    | |- context [match rinterp_binds ?k ?R ?d ?g ?st ?e with _ => _ end] =>
        destruct (rinterp_binds k R d g st e) as [[[? ?] ?]|?]
    | |- context [match rinterp_apply ?k ?R ?d ?l ?th ?pl ?st with _ => _ end] =>
        destruct (rinterp_apply k R d l th pl st) as [[? ?]|?]
    | |- context [match rinterp_apply_r ?k ?R ?d ?l ?inputs ?outputs ?st with _ => _ end] =>
        destruct(rinterp_apply_r k R d l inputs outputs st)as[[? ?]|?]
    | |- context [match rinterp_branch ?k ?R ?d ?g ?st ?l ?brs with _ => _ end] =>
        destruct (rinterp_branch k R d g st l brs) as [[[? ?] ?]|?]
    | |- context [match rlook ?l ?d with _ => _ end] => destruct (rlook l d) as [[]|]
    | |- context [match rcompose ?d ?pl with _ => _ end] => destruct (rcompose d pl)
    | |- context [match rchain ?d ?pl with _ => _ end] => destruct (rchain d pl)
    | |- context [match truthy ?P ?w with _ => _ end] => destruct (truthy P w) as [[]|]
    | |- context [match tbl ?f with _ => _ end] => rewrite (Htbl f); destruct (rp_fn R f) as [[? ?]|]
    | |- context [match ftbl ?f with _ => _ end] => rewrite (Hftbl f); destruct (rp_fun R f) as [[? ?]|]
    | |- context [if ?b then cc ?x else cc ?y] => destruct b
    | |- context [match (if ?c then ?x else None) with _ => _ end] =>
        destruct (if c then x else None) as [[? ?]|]
    end; cbn beta iota zeta); try reflexivity.
  split; [|split; [|split; [|split; [|split; [|split]]]]].
  - intros e d g st.
    destruct e.
    all: try (rewrite cc_while; cbn [rinterp]; dig IHe IHl IHr IHb IHa IHbr IHar; fail).
    all: try (rewrite cc_loop; cbn [rinterp]; dig IHe IHl IHr IHb IHa IHbr IHar; fail).
    all: cbn [cc rinterp]; fold_lists.
    all: dig IHe IHl IHr IHb IHa IHbr IHar.
    all: try (match goal with |- context [match ?w with XVTag _ _ => _ | _ => _ end] => destruct w end;
              cbn iota; try reflexivity; rewrite rfind_carm_cc;
              match goal with |- context [rfind_arm ?t ?a] => destruct (rfind_arm t a) as [[? ?]|] end;
              dig IHe IHl IHr IHb IHa IHbr IHar).
    all: try (rewrite rselect_carm_cc;
              match goal with |- context [rselect_arm ?a ?w] => destruct (rselect_arm a w) as [[? ?]|] end;
              dig IHe IHl IHr IHb IHa IHbr IHar).
  - intros l d g st. destruct l as [|e r]; [reflexivity|].
    change (ccl (e :: r)) with (match e with RVar x => lvar x (ccl r) | RVl l => lval l (ccl r)
                                           | _ => lcons (cc e) (ccl r) end).
    destruct e; cbn [lvar lval lcons rinterp_list].
    all: try (rewrite (IHl r d g st); destruct k as [|k']; [reflexivity|]; cbn [rinterp];
              first [destruct (lookup_xvenv _ g) | destruct (rlook _ d) as [[]|]]; reflexivity).
    all: rewrite IHe; match goal with |- context [rinterp ?k0 R ?d0 ?g0 ?st0 ?e0] =>
           destruct (rinterp k0 R d0 g0 st0 e0) as [[| | | | | | |] ? ?] end; try reflexivity;
         rewrite IHl; reflexivity.
  - intros fs d g st. destruct fs as [|[f e] r]; [reflexivity|].
    change (crec ((f, e) :: r)) with (lcons (cc e) (crec r)).
    cbn [lcons map snd rinterp_list]. rewrite IHe.
    destruct (rinterp k R d g st e) as [[| | | | | | |] ? ?]; try reflexivity.
    rewrite IHr. reflexivity.
  - intros bs d g st. destruct bs as [|b r]; [reflexivity|].
    unfold ccbs; cbn [fold_right]; fold (ccbs r).
    destruct b; cbn [ccb rinterp_binds];
      try match goal with e : rexpr |- _ => fold (cc e) end;
      fold_lists; dig IHe IHl IHr IHb IHa IHbr IHar.
  - intros l th pl d st. cbn [capply rinterp_apply].
    destruct(rlook l d)as[a|];[destruct a|];try reflexivity.
    rewrite Hftbl;destruct(rp_fun R u)as[[Dr bs]|];[|reflexivity].
    destruct(rcompose d pl)as[d1|];[|reflexivity].
    destruct(rchain d1 l0)as[d2|];[|reflexivity].
    destruct(prepare_scalar Dr bs)as[[count result]|]eqn:Prepared.
    +rewrite instantiate_scalar_fast_exact;symmetry;eapply prepared_scalar_execution;exact Prepared.
    +rewrite IHb;reflexivity.
  - intros l brs d g st. cbn [cbranch rinterp_branch]. dig IHe IHl IHr IHb IHa IHbr IHar.
    all: rewrite rfind_cbranch_cc;
      match goal with |- context [rfind_branch ?h ?a] => destruct (rfind_branch h a) as [[? ?]|] end;
      dig IHe IHl IHr IHb IHa IHbr IHar.
  - intros l inputs outputs d st;cbn[capply_r rinterp_apply_r];dig IHe IHl IHr IHb IHa IHbr IHar.

Qed.

Theorem cc_agrees : forall n e d g st, cc e n d g st = rinterp n R d g st e.
Proof.
  intros n. assert (H : AGREE n).
  { induction n as [|k IH].
    - repeat split; intros;
        repeat match goal with
        | e : rexpr |- _ => destruct e; reflexivity
        | l : list _ |- _ => destruct l as [|[]]; reflexivity
        end.
    - apply agree_step, IH. }
  apply H.
Qed.

End Agree.

(* ===================== Running main ===================================== *)

Definition tbl_ok (R : rprog) (tbl : fsym -> option (code * code)) (ftbl : usym -> option (tele * bcode * option(nat*renv))) : Prop :=
  (forall f, tbl f =
     match rp_fn R f with Some (bs, bd) => Some (cc R tbl ftbl bs, cc R tbl ftbl bd) | None => None end)
  /\ (forall h, ftbl h =
     match rp_fun R h with Some (Dr, bs) => Some (Dr, ccbs R tbl ftbl bs, prepare_scalar Dr bs) | None => None end).

Definition crun_main (tbl : fsym -> option (code * code)) (fuel : nat) (main : fsym)
    (d0 : xdenv) (st0 : mstate) : xres :=
  match tbl main with
  | Some (_, cd) => ret_call [] (cd fuel (conv_env d0) [] st0)
  | None => XR (XErr e_nobody) [] st0
  end.

Theorem crun_main_eq : forall R tbl ftbl fuel main d0 st0, tbl_ok R tbl ftbl ->
  crun_main tbl fuel main d0 st0 = rrun_main fuel R main d0 st0.
Proof.
  intros R tbl ftbl fuel main d0 st0 [Ht Hf]. unfold crun_main, rrun_main.
  rewrite Ht. destruct (rp_fn R main) as [[bs bd]|]; auto.
  rewrite (cc_agrees R tbl ftbl Ht Hf). reflexivity.
Qed.

(* The closure engine on the compiled program computes Audit's semantics. *)
Theorem clo_run_main : forall p argv rd tbl ftbl fuel st0,
  tbl_ok (rprog_of p argv rd) tbl ftbl ->
  crun_main tbl fuel (xo_main p) (xo_env p) st0
  = run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd tbl ftbl fuel st0 H. rewrite (crun_main_eq fuel (xo_main p) (xo_env p) st0 H).
  apply slot_run_main.
Qed.

(* ... and with pruning, as Prune.oracle_agrees. *)
Theorem clo_oracle_agrees : forall p argv rd tbl ftbl fuel st0,
  tbl_ok (rprog_of (oracle_program p) argv rd) tbl ftbl ->
  let P := program_of p argv rd in
  stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ crun_main tbl fuel (xo_main p) (xo_env p) st0 = run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd tbl ftbl fuel st0 Hok P.
  rewrite (crun_main_eq fuel (xo_main p) (xo_env p) st0 Hok). apply slot_oracle_agrees.
Qed.

(* ===================== The oracle's engine, end to end =================== *)

(* What the driver's default engine runs: the elaborated program, pruned
   (Prune.oracle_program), pre-resolved (SlotX.rprog_of), its small calls
   folded (FoldX.fold_e, into R'), closure-compiled (tbl, ftbl).  The
   hypotheses are what the driver's arrays realize: tb holds the static
   bodies of R, R' is R with every body folded, tbl/ftbl the closure code
   of R'. *)
Theorem engine_agrees : forall p argv rd fuel st0 tb M N R' tbl ftbl,
  let R := rprog_of (oracle_program p) argv rd in
  (forall f bs, tb f = Some bs -> exists bd, rp_fn R f = Some (bs, bd)) ->
  rp_x R' = rp_x R ->
  (forall f, rp_fn R' f =
     match rp_fn R f with Some (bs, bd) => Some (guard_fold_e (rp_x R) tb M N bs, guard_fold_e (rp_x R) tb M N bd) | None => None end) ->
  (forall h, rp_fun R' h =
     match rp_fun R h with Some (Dr, bs) => Some (Dr, map (guard_fold_b (rp_x R) tb M N) bs) | None => None end) ->
  tbl_ok R' tbl ftbl ->
  let P := program_of p argv rd in
  stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ crun_main tbl fuel (xo_main p) (xo_env p) st0 = run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 tb M N R' tbl ftbl R Htb Hx Hfn Hfun Hok P.
  rewrite (crun_main_eq fuel (xo_main p) (xo_env p) st0 Hok).
  rewrite (@guard_fold_run_main R R' tb M N fuel (xo_main p) (xo_env p) st0 Htb Hx Hfn Hfun).
  apply slot_oracle_agrees.
Qed.
