(* ------------------------------------------------------------------------ *)
(* BodySound.v — the elaborator's bodies against Part V's V.6 judgments      *)
(* (Track A, elaborate_sound step 2).                                        *)
(*                                                                           *)
(* The architecture.  Elaboration is one run of the state monad from the    *)
(* initial state to a final state sF, whose program P and field table are   *)
(* what the relation reads (W = W_read w R Fl, ReqsTotal.elaborate_world_of).*)
(* A derivation made for a call that ran from s to s' may use only what     *)
(* the final state still holds of s': `Later s'` (the world, field numbers, *)
(* emitted bodies of fresh symbols, what was reported unsupported).  Each   *)
(* synth-group function is related to its judgment under                     *)
(*                                                                           *)
(*   CxRel c e F C : the elaborator's checker context c and environment e   *)
(*                   correspond to the relation's frame F and context C      *)
(*   VarsIn s' F   : the locals numbered so far (st_vars) are F's numbering *)
(*                                                                           *)
(* where F's numbering is the one the body ends with: local_var only appends *)
(* to st_vars, and with_vars restores it, so a number given is never       *)
(* changed (vars_prefix).  Types translate by `tr Fl` (ety_ty computes the  *)
(* same, ety_ty_tr).                                                         *)
(*                                                                           *)
(* Proved (bodies_sound): for every fuel, synth, synth_block, check_stmts, *)
(* check_stmt, check_bool, synth_if and synth_path are related to Ex, BlkE, *)
(* Blk, Cond and Ex of `if`/paths, given `Pending`, which lists exactly the *)
(* constructs not yet proved, each as a step from the hypothesis at fuel f  *)
(* to fuel f+1:                                                             *)
(*   pd_synth  calls, fields, method calls, records, tags, match,           *)
(*             operators, literals;                                         *)
(*   pd_stmt   bind statements (and so functors, lambdas, providers);       *)
(*   pd_const  a defined val's inlined initializer (ExConst);               *)
(*   pd_ty     elab_type against TyOf.                                      *)
(* Proved outright: unit, `this`, locals, units, vals in force, `return`,   *)
(* `break`, blocks (with the escape check), `let`/`var`, assignment, `while`,*)
(* `loop` (BlkLoopLast), expression statements, `if` with and without      *)
(* `else` (Branch, Join), conditions, with canonical merge readings. The   *)
(* section hypothesis Hbool (lib/bool.moss's `Bool` is                      *)
(* its own declaration) is still to be discharged.                          *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ElabFrame LambdaLocals.
From NM Require Export BodyTr.
From NM Require Import Merge CanonTypes TyOfSubst BodyUf NominalSupport NominalMergeSupport FunctorDecl.
From NM Require CoreMoss.
From NM Require Import InfoPreserve.
From NM Require Export ElabInv FwdReadonly FwdInvariant FwdPreserve FwdSynthInv FinalForwarders FinalLambdas FnSnapshot.
Import ListNotations.
Local Open Scope list_scope.

From NM Require Export BodySoundCore.
Include BodySoundCore.
Local Strategy opaque [Nat.of_num_uint FUEL].

Lemma info_vars : forall vs n s,InfoInv s->InfoInv(s_vars vs n s).
Proof.
 intros vs n s I. apply(info_step s);[exact I| |reflexivity|reflexivity].
 constructor;cbn;auto;[exists[];rewrite app_nil_r;reflexivity|apply ext_refl].
Qed.

Ltac cache_inv :=
 first [assumption|match goal with
 | |-InfoInv(s_vars ?vs ?n ?s)=>apply info_vars;cache_inv
 | E : ?m ?si = Good (?a,?sf) |- InfoInv ?sf =>
   tryif constr_eq si sf then fail else idtac;
   let O:=fresh "OI" in assert(O:okI m)by(first[solve[auto with okIdb]|okIt]);
   eapply(proj2 O si a sf);[eassumption|cache_inv|exact E]
 end].

Lemma fwd_vars : forall vs n s,FwdInv s->FwdInv(s_vars vs n s).
Proof. intros vs n s I;exact I. Qed.
Ltac fwd_inv :=
 first [assumption|match goal with
 | |-FwdInv(s_vars ?vs ?n ?s)=>apply fwd_vars;fwd_inv
 | E : ?m ?si = Good (?a,?sf), F : Fr ?si ?sf |- FwdInv ?sf =>
   tryif constr_eq si sf then fail else idtac;
   let K:=fresh "KF" in assert(K:keeps st_fwd m)by(first[solve[auto with keepsdb]|keepst]);
   eapply fwd_inv_frame;[fwd_inv|exact F|exact(K si a sf E)]
 | E : ?m ?si = Good (?a,?sf) |- FwdInv ?sf =>
   tryif constr_eq si sf then fail else idtac;
   let O:=fresh "OF" in assert(O:okF m)by(first[solve[auto with fwdb]|fwt]);
   eapply(proj2 O si a sf);[eassumption|fwd_inv|exact E]
 end].

Section Bodies.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation trF := (tr Fl).
Local Notation W := (@BodySoundCore.W w R Fl).
Local Notation ConstFact := (@BodySoundCore.ConstFact w P R Fl).
Local Notation Later := (@BodySoundCore.Later w P R Fl).
Local Notation later_back := (@BodySoundCore.later_back w P R Fl).
Local Notation CxRel := (@BodySoundCore.CxRel w R Fl).
Local Notation cxrel_local := (@BodySoundCore.cxrel_local w R Fl).
Local Notation cxrel_loop := (@BodySoundCore.cxrel_loop w R Fl).
Local Notation Post := (@BodySoundCore.Post w P R Fl).
Local Notation post_back := (@BodySoundCore.post_back w P R Fl).
Local Notation SynthOK := (@BodySoundCore.SynthOK w P R Fl).
Local Notation BlockOK := (@BodySoundCore.BlockOK w P R Fl).
Local Notation StmtConcl := (@BodySoundCore.StmtConcl w P R Fl).
Local Notation StmtOK := (@BodySoundCore.StmtOK w P R Fl).
Local Notation StmtsOK := (@BodySoundCore.StmtsOK w P R Fl).
Local Notation CondOK := (@BodySoundCore.CondOK w P R Fl).
Local Notation IfOK := (@BodySoundCore.IfOK w P R Fl).
Local Notation PathOK := (@BodySoundCore.PathOK w P R Fl).
Local Notation ConstOK := (@BodySoundCore.ConstOK w P R Fl).
Local Notation OK := (@BodySoundCore.OK w P R Fl).
Local Notation Pending := (@BodySoundCore.Pending w P R Fl).
Local Notation ok_zero := (@BodySoundCore.ok_zero w P R Fl).
Local Notation lt_w := (@BodySoundCore.lt_w w P R Fl).
Local Notation lt_flds := (@BodySoundCore.lt_flds w P R Fl).
Local Notation lt_unsup := (@BodySoundCore.lt_unsup w P R Fl).
Local Notation lt_lang := (@BodySoundCore.lt_lang w P R Fl).
Local Notation lt_reqs := (@BodySoundCore.lt_reqs w P R Fl).
Local Notation lt_consts := (@BodySoundCore.lt_consts w P R Fl).
Local Notation lt_reqclosed := (@BodySoundCore.lt_reqclosed w P R Fl).
Local Notation lt_reqnone := (@BodySoundCore.lt_reqnone w P R Fl).
Local Notation lt_bool_support := (@BodySoundCore.lt_bool_support w P R Fl).
Local Notation lt_forwarders := (@BodySoundCore.lt_forwarders w P R Fl).
Local Notation lt_lambdas := (@BodySoundCore.lt_lambdas w P R Fl).
Local Notation lt_contexts := (@BodySoundCore.lt_contexts w P R Fl).
Local Notation cr_mod_in := (@BodySoundCore.cr_mod_in w R Fl).
Local Notation cr_mod := (@BodySoundCore.cr_mod w R Fl).
Local Notation cr_ret := (@BodySoundCore.cr_ret w R Fl).
Local Notation cr_ret_wf := (@BodySoundCore.cr_ret_wf w R Fl).
Local Notation cr_this := (@BodySoundCore.cr_this w R Fl).
Local Notation cr_this_wf := (@BodySoundCore.cr_this_wf w R Fl).
Local Notation cr_loop := (@BodySoundCore.cr_loop w R Fl).
Local Notation cr_uf := (@BodySoundCore.cr_uf w R Fl).
Local Notation cr_sub := (@BodySoundCore.cr_sub w R Fl).
Local Notation cr_locals := (@BodySoundCore.cr_locals w R Fl).
Local Notation cr_locals_wf := (@BodySoundCore.cr_locals_wf w R Fl).
Local Notation cr_vals := (@BodySoundCore.cr_vals w R Fl).
Local Notation cr_vals_wf := (@BodySoundCore.cr_vals_wf w R Fl).
Local Notation cr_ty := (@BodySoundCore.cr_ty w R Fl).
Local Notation cr_ty_wf := (@BodySoundCore.cr_ty_wf w R Fl).
Local Notation cr_fns := (@BodySoundCore.cr_fns w R Fl).
Local Notation cr_mths := (@BodySoundCore.cr_mths w R Fl).
Local Notation cr_fns_clean := (@BodySoundCore.cr_fns_clean w R Fl).
Local Notation cr_mths_clean := (@BodySoundCore.cr_mths_clean w R Fl).
Local Notation cr_support := (@BodySoundCore.cr_support w R Fl).
Local Notation cr_ret_support := (@BodySoundCore.cr_ret_support w R Fl).
Local Notation cr_this_support := (@BodySoundCore.cr_this_support w R Fl).
Local Notation cr_apps := (@BodySoundCore.cr_apps w R Fl).
Local Notation cr_vapps := (@BodySoundCore.cr_vapps w R Fl).
Local Notation cr_fapps := (@BodySoundCore.cr_fapps w R Fl).
Local Notation cr_funapps := (@BodySoundCore.cr_funapps w R Fl).
Local Notation cr_funspresent := (@BodySoundCore.cr_funspresent w R Fl).
Local Notation cr_funfixed := (@BodySoundCore.cr_funfixed w R Fl).
Local Notation cr_pins := (@BodySoundCore.cr_pins w R Fl).
Local Notation cr_applying := (@BodySoundCore.cr_applying w R Fl).
Local Notation cr_fresh := (@BodySoundCore.cr_fresh w R Fl).
Local Notation cr_fun_decls := (@BodySoundCore.cr_fun_decls w R Fl).
Local Notation cr_fn_sigs := (@BodySoundCore.cr_fn_sigs w R Fl).
Local Notation cr_locals_nodup := (@BodySoundCore.cr_locals_nodup w R Fl).
Local Notation cr_mth_rows := (@BodySoundCore.cr_mth_rows w R Fl).
Local Notation cr_mth_domain := (@BodySoundCore.cr_mth_domain w R Fl).
Local Notation bs_synth := (@BodySoundCore.bs_synth w P R Fl).
Local Notation bs_block := (@BodySoundCore.bs_block w P R Fl).
Local Notation bs_stmts := (@BodySoundCore.bs_stmts w P R Fl).
Local Notation bs_stmt := (@BodySoundCore.bs_stmt w P R Fl).
Local Notation bs_cond := (@BodySoundCore.bs_cond w P R Fl).
Local Notation bs_if := (@BodySoundCore.bs_if w P R Fl).
Local Notation bs_path := (@BodySoundCore.bs_path w P R Fl).
Local Notation bs_const := (@BodySoundCore.bs_const w P R Fl).
Local Notation pd_synth := (@BodySoundCore.pd_synth w P R Fl).
Local Notation pd_stmt := (@BodySoundCore.pd_stmt w P R Fl).
Local Notation pd_const := (@BodySoundCore.pd_const w P R Fl).
Local Notation pd_ty := (@BodySoundCore.pd_ty w P R Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

(* [D45]: the Bool the elaborator reads is lib/bool.moss's own (an open
   obligation of the library's shape: see the report of this file) *)
Hypothesis Hbool : forall bi x, sassoc "bool"%string (lang_modules w) = Some bi ->
  sassoc "Bool"%string (gm_names (get_mod w bi)) = Some x -> A.LangSym W "bool"%string A.NSName "Bool"%string x.


Local Notation rpath_rel := (@BodySoundCore.rpath_rel w R Fl b Hload).
Local Notation sym_loader := (@BodySoundCore.sym_loader w R Fl b Hload).
Tactic Notation "binv" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Lemma path_step : forall f, ConstOK f -> PathOK (S f).
Proof.
  intros f HC c e p a s v t s' H F C Hi Ii Ifwd Hc [Hl Hv] exp.
  assert (Fs : Fr s s') by exact (proj2 (ok_synth_path (S f) c e p a s _ s' Hi H)).
  assert (Hw : st_w s = w) by (rewrite <- (lt_w _ Hl); symmetry; exact (fr_w _ _ Fs)).
  cbn[synth_path] in H.
  (* the part after the local lookup *)
  assert (Global : forall s0, Inv s0 -> InfoInv s0 -> FwdInv s0 -> st_w s0 = w -> Fr s0 s' ->
            (forall n', p = [n'] -> sassoc n' (en_locals e) = None) -> a = None ->
            (let! tg := rpath (cx_mod c) p in
             match tg with
             | None => err (cx_mod c) ("`" ++ show_path p ++ "` is not in scope")%string
             | Some (EMod _) => err (cx_mod c) ("`" ++ show_path p ++ "` is a module, not a value")%string
             | Some (ESym s) =>
                 let! g := sym s in
                 match gs_kind g, gs_decl g with
                 | KUnit, _ => ret (XTag s [] XUnit, YNom s [])
                 | KVal, ADVal _ _ (Some _) =>
                     let! (initializer,t) := const_val f g in
                     check_constant_effects (cx_mod c) initializer ;;
                     ret (XConst s initializer,t)
                 | KVal, _ => match nget (en_vals e) s with
                              | Some t => ret (XVl s, t)
                              | None => err (cx_mod c) ("`" ++ gs_name g ++ "` is not available in the context here (assume or bind it)")%string
                              end
                 | KFn, _ => err (cx_mod c) ("`" ++ gs_name g ++ "` is a function; call it (no first-class fns yet)")%string
                 | KTag, _ => err (cx_mod c) ("`" ++ gs_name g ++ "` is a tag; construct it with a payload")%string
                 | _, _ => err (cx_mod c) ("`" ++ gs_name g ++ "` is not a value")%string
                 end
             end) s0 = Good ((v, t), s') ->
            A.Ex W P F C (AEPath p a) exp v (trF (canon e t)) /\ WFty t /\ Support W t).
  { intros s0 Hi0 Ii0 Ifwd0 Hw0 Fs0 Hnl -> H0. binv H0 as r s1 Hr. destruct (rpath_rel _ _ _ _ _ Hw0 (cr_mod_in _ _ _ _ Hc) Hr) as [-> Hres].
    assert (Nl : forall n', p = [n'] -> A.slook n' (A.c_locals C) = None)
      by (intros n' E; rewrite (cr_locals _ _ _ _ Hc), (Hnl n' E); reflexivity).
    destruct r as [[x|j]|]; [|noerr H0|noerr H0].
    cbn[option_map ent_of] in Hres. binv H0 as a0 s2 Hs.
    destruct (sym_loader _ _ _ _ _ _ Hi0 Hw0 Hres Hs) as [-> Hg].
    assert (Hrs : A.resolve_sym W (A.fr_mod F) p = Some x)
      by (unfold A.resolve_sym; rewrite (cr_mod _ _ _ _ Hc), Hres; reflexivity).
    pose proof (kind_sym_read b w R Fl Hload x a0 Hg) as Hk. pose proof (load_world_kind b w Hload x a0 Hg) as KO.
    unfold KindOK in KO.
    destruct (gs_kind a0) eqn:Kg; try noerr H0.
    - (* a unit *)
      injection H0 as <- <- <-. split; [|split; [reflexivity|]].
      eapply A.ExUnitTag; [exact Nl | exact Hrs | unfold BodySoundCore.W; rewrite Hk; reflexivity].
      apply SupportNom; [left; exact Hk|symmetry; apply (lt_reqnone _ Hl x); right; exact Hk|constructor].
    - (* a val *)
      destruct (gs_decl a0) as [| | | | |n0 t0 [i|]| | |] eqn:Dg; cbn in KO;
        try (repeat match type of KO with context [match ?x with _ => _ end] => destruct x end; discriminate KO).
      + (* defined: marked lexical initializer *)
        binv H0 as cached scached Hcache. destruct cached as [initializer constant_type].
        unfold check_constant_effects in H0. destruct (no_nonlocal_return_expr initializer); try noerr H0.
        injection H0 as <- <- <-.
        pose proof (load_world_ids b w Hload x a0 Hg) as Eid.
        assert (Hd : A.w_decl W (gs_id a0) = Some (A.Decl (gm_path (get_mod w (gs_mod a0))) (gs_decl a0) (gs_assumes a0)))
          by (rewrite Eid; exact (w_decl_read w R Fl x a0 Hg)).
        assert (Hg' : nget (w_syms w) (gs_id a0) = Some a0) by (rewrite Eid; exact Hg).
        destruct (HC a0 s0 initializer constant_type scached Hcache Hi0 Ii0 Ifwd0 Hl Hg' _ n0 t0 i Hd ltac:(cbn; exact Dg)) as (Ha & Hty & Wt & St & F0 & t' & Hx & Hf & Hm).
        assert (Ec : trF (canon e constant_type) = trF constant_type).
        { rewrite <- (tr_canon Fl e constant_type (proj1 (cr_uf _ _ _ _ Hc))).
          apply CoreMoss.app_subst_closed.
          exact (tyof_closed W _ _ _ _ _ Hty ltac:(intros k y E; discriminate E) ltac:(intros y E; discriminate E)). }
        rewrite Ec. split; [|split; [exact Wt|exact St]].
        eapply A.ExConst; [exact Nl | exact Hrs | rewrite <- Eid; exact Hd | cbn; exact Dg | exact Ha | exact Hty |].
        exists F0, t'. auto.
      + (* in force *)
        destruct (nget (en_vals e) x) as [tv|] eqn:Ev; [|noerr H0]. injection H0 as <- <- <-.
        split; [|split; [exact (cr_vals_wf _ _ _ _ Hc x tv Ev)|exact (@es_vals W e (cr_support _ _ _ _ Hc) x tv Ev)]].
        eapply A.ExVal; [exact Nl | exact Hrs | unfold BodySoundCore.W; exact (w_decl_read w R Fl x a0 Hg) | cbn; exact Dg |].
        rewrite (cr_vals _ _ _ _ Hc), Ev. reflexivity. }
  destruct a as [bs|].
  - destruct p as [|n [|n2 r]]; cbn in H; noerr H.
  - destruct p as [|n [|n2 r]].
    + apply (Global s Hi Ii Ifwd Hw Fs); [intros n' E; discriminate E | reflexivity | exact H].
    + destruct (sassoc n (en_locals e)) as [[tl mu]|] eqn:El.
      * (* a local *)
        binv H as a s1 H0. injection H as <- <- <-.
        pose proof (local_var_spec _ _ _ _ H0) as Hk.
        split; [|split; [exact (cr_locals_wf _ _ _ _ Hc n _ El)|exact (@es_locals W e (cr_support _ _ _ _ Hc) n (tl,mu) El)]].
        replace (XVar a) with (XVar (A.var_of F n)).
        -- eapply A.ExLocal; [rewrite (cr_locals _ _ _ _ Hc), El; reflexivity|].
           unfold A.Named. rewrite (Hv n a Hk). discriminate.
        -- unfold A.var_of. rewrite (Hv n a Hk). reflexivity.
      * apply (Global s Hi Ii Ifwd Hw Fs); [intros n' E; injection E as <-; exact El | reflexivity | exact H].
    + apply (Global s Hi Ii Ifwd Hw Fs); [intros n' E; discriminate E | reflexivity | exact H].
Qed.

(* ======================================================================== *)
(* 6. Expressions, blocks, statements                                        *)
(* ======================================================================== *)

Lemma join_tr : forall mi e a b0 exp s t s', join mi e a b0 exp s = Good (t,s') ->
  Tri (en_uf e) /\ UfWF (en_uf e) -> WFty a -> WFty b0 -> opt_wf exp ->
  A.Join (trF (canon e a)) (trF (canon e b0))
    (option_map (fun t => trF (canon e t)) exp) (trF (canon e t)) /\ WFty t.
Proof. intros mi e a b0 exp s t s' H [_ U] Wa Wb Wx. exact (join_canon_tr Fl mi e a b0 exp s t s' H U Wa Wb Wx). Qed.

Lemma fits_tr : forall e t x, Tri (en_uf e) /\ UfWF (en_uf e) -> fits e t x = true ->
  A.fits (trF (canon e t)) (trF (canon e x)).
Proof. intros e t x _ H. exact (fits_canon_tr Fl e t x H). Qed.

Lemma opt_fits_lift : forall a c, A.fits a c -> A.opt_fits a (Some c).
Proof. intros a c H. exact H. Qed.

Lemma elab_opt_fits_tr : forall e t exp, Tri (en_uf e) /\ UfWF (en_uf e) -> Elab.opt_fits e t exp = true ->
  A.opt_fits (trF (canon e t)) (option_map (fun t => trF (canon e t)) exp).
Proof. intros e t exp _ H. exact (opt_fits_canon_tr Fl e t exp H). Qed.

Lemma varsin_frame : forall s F F', A.fr_var F' = A.fr_var F -> VarsIn s F -> VarsIn s F'.
Proof. intros s F F' E H n k Hk. rewrite E. exact (H n k Hk). Qed.

Lemma post_frame : forall s F F', A.fr_var F' = A.fr_var F -> Post s F -> Post s F'.
Proof. intros s F F' E [L V]. split; [exact L | exact (varsin_frame s F F' E V)]. Qed.

(* the local a statement or path names, as the frame numbers it *)
Lemma local_named : forall n s k s' s'' F, local_var n s = Good (k, s') -> Fr s' s'' -> Post s'' F ->
  A.Named F n /\ A.var_of F n = k.
Proof.
  intros n s k s' s'' F H Fs Hp. destruct (post_back _ _ _ Fs Hp) as [_ V].
  pose proof (V n k (local_var_spec _ _ _ _ H)) as E. unfold A.Named, A.var_of. rewrite E.
  split; [discriminate | reflexivity].
Qed.

Ltac stepok H L Hi Hi' Fs' := destruct (okstep _ _ _ _ _ L Hi H) as [Hi' Fs'].

Lemma synth_step : forall f, OK f -> Pending -> SynthOK (S f).
Proof.
  intros f IH PD c e x exp s v t s' H F C Hi Ii Ifwd Hc Hx Hxs Hp.
  destruct (pending_expr x) eqn:Pe; [exact (pd_synth PD f IH c e x exp s v t s' Pe H F C Hi Ii Ifwd Hc Hx Hxs Hp)|].
  cbn[synth] in H. binv H as vt0 s1 H1. destruct vt0 as [v0 t0].
  binv H as u s2 Hinj. injection H as <- <- <-.
  assert (Fin : forall s0, Inv s0 -> Fr s s0 -> s0 = s1 -> Post s1 F).
  { intros s0 Hi0 _ ->. stepok Hinj (ok_inject_check e t0 exp) Hi0 Hi2 Fs2. exact (post_back _ _ _ Fs2 Hp). }
  destruct x; cbn in Pe; try discriminate Pe; cbn beta iota zeta in H1;
    idtac.
  - (* () *) injection H1 as <- <- <-. split; [apply A.ExUnit |split;[reflexivity|constructor]].
  - (* this *) destruct (cx_this c) as [th|] eqn:Et; [|noerr H1]. injection H1 as <- <- <-.
    split; [apply A.ExThis; rewrite (cr_this _ _ _ _ Hc), Et; reflexivity|].
    split.
    + pose proof (cr_this_wf _ _ _ _ Hc) as W0. rewrite Et in W0. exact W0.
    + pose proof (cr_this_support _ _ _ _ Hc) as S0. rewrite Et in S0. exact S0.
  - (* a path *) stepok H1 (ok_synth_path f c e p s0) Hi Hi1 Fs1.
    destruct f as [|f']; [cbn[synth_path] in H1; discriminate H1|].
    exact (bs_path _ IH c e p s0 s v0 t0 s1 H1 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc (Fin s1 Hi1 Fs1 eq_refl) _).
  - (* if *) stepok H1 (ok_synth_if f c e (AEIf x a o) exp) Hi Hi1 Fs1.
    exact (bs_if _ IH c e _ exp s v0 t0 s1 H1 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hx Hxs (Fin s1 Hi1 Fs1 eq_refl)).
  - (* return *) destruct o as [y|].
    + binv H1 as vt s3 Hy. destruct vt as [vy ty].
      stepok Hy (ok_synth f c e y (Some (sg_ret (cx_sig c)))) Hi Hi3 Fs3.
      destruct (fits e ty (sg_ret (cx_sig c))) eqn:Ef; [|noerr H1]. injection H1 as <- <- <-.
      destruct (bs_synth _ IH c e y _ s vy ty s3 Hy F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc (cr_ret_wf _ _ _ _ Hc) (cr_ret_support _ _ _ _ Hc) (Fin s3 Hi3 Fs3 eq_refl)) as [Ey [Wy Sy]].
      split; [|split;[reflexivity|constructor]]. eapply A.ExReturn.
      * rewrite (cr_ret _ _ _ _ Hc). exact Ey.
      * rewrite (cr_ret _ _ _ _ Hc). exact (fits_tr e ty _ (cr_uf _ _ _ _ Hc) Ef).
    + destruct (is_unit_or_never (canon e (sg_ret (cx_sig c)))) eqn:Eu; [|noerr H1]. injection H1 as <- <- <-.
      split; [|split;[reflexivity|constructor]]. apply A.ExReturnUnit. rewrite (cr_ret _ _ _ _ Hc).
      destruct (canon e (sg_ret (cx_sig c))); try discriminate Eu; [left | right]; reflexivity.
  - (* break *) destruct (Nat.eqb (cx_loop c) 0) eqn:El; [noerr H1|]. injection H1 as <- <- <-.
    split; [|split;[reflexivity|constructor]]. apply A.ExBreak. rewrite (cr_loop _ _ _ _ Hc), El. reflexivity.
  - (* a block *) binv H1 as r3 s3 Hb. destruct r3 as [[vb tb] bt]. injection H1 as <- <- <-.
    stepok Hb (ok_synth_block f c e a exp) Hi Hi3 Fs3.
    destruct (bs_block _ IH c e a exp s vb tb bt s3 Hb F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hx Hxs (Fin s3 Hi3 Fs3 eq_refl)) as (Eb & Wb & Sb & _).
    split; [|split; [destruct exp;[exact Hx|exact Wb]|destruct exp;[exact Hxs|exact Sb]]].
    destruct exp as [x|]; cbn; apply A.ExBlock in Eb; exact Eb.
  - (* an anonymous function outside a bind *) noerr H1.
Qed.

Lemma block_step : forall f, OK f -> BlockOK (S f).
Proof.
  intros f IH c e bl exp s v t bt s' H F C Hi Ii Ifwd Hc Hx Hxs Hp.
  destruct bl as [ss tl]. cbn[synth_block] in H.
  binv H as r1 s1 H1. destruct r1 as [[[xs e1] c1] d].
  stepok H1 (ok_check_stmts f c e ss) Hi Hi1 Fs1.
  pose proof (check_stmts_uf f c e ss s xs e1 c1 d s1 H1) as Euf.
  pose proof (canon_uf e e1 Euf) as Ecanon.
  assert (Eexp : option_map (fun t => trF (canon e1 t)) exp = option_map (fun t => trF (canon e t)) exp).
  { destruct exp; cbn[option_map]; [rewrite Ecanon|]; reflexivity. }
  binv H as r2 s2 H2. destruct r2 as [tx tty].
  assert (Fs2 : Fr s1 s2 /\ Inv s2).
  { destruct tl as [y|].
    - stepok H2 (ok_synth f c1 e1 y exp) Hi1 Hi2 Fs2. auto.
    - injection H2 as _ _ <-. split; [apply fr_refl | exact Hi1]. }
  destruct Fs2 as [Fs2 Hi2].
  cbn zeta in H.
  destruct (filter _ _) as [|g gone] eqn:Eg; [|noerr H].
  destruct (Elab.opt_fits e1 tty exp) eqn:Eo; [|noerr H].
  injection H as <- <- <-. subst s'.
  assert (Hp1 : Post s1 F) by exact (post_back _ _ _ Fs2 Hp).
  (* the statements *)
  assert (Hst : exists F1 C1 made, CxRel c1 e1 F1 C1 /\ A.fr_var F1 = A.fr_var F /\ cx_fresh c1 = cx_fresh c ++ made /\
    (forall y exp tx tt, A.Ex W P F1 C1 y exp tx tt -> A.Blk W P F C ss (Some y) exp (fold_stmts xs tx) tt made) /\
    (forall exp, A.Blk W P F C ss None exp (fold_stmts xs XUnit) (if d then A.never else TUnit) made)).
  { destruct ss as [|s0 r].
    - destruct f as [|f']; [discriminate H1|]. cbn[check_stmts] in H1. injection H1 as <- <- <- <- <-.
      exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
      split; [intros; apply A.BlkTail; assumption | intro; apply A.BlkEmpty].
    - exact (bs_stmts _ IH c e (s0 :: r) s xs e1 c1 d s1 H1 ltac:(discriminate) F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hp1). }
  destruct Hst as (F1 & C1 & made & Hc1 & Fv & Ef & Kt & Ln).
  assert (Hmade : skipn (List.length (cx_fresh c)) (cx_fresh c1) = made)
    by (rewrite Ef; apply skipn_len_app).
  assert (Hesc : forall g0, In g0 made -> ~ In g0 (A.gens (trF (canon e1 tty)))).
  { intros g0 Hg Hin. rewrite gens_tr in Hin.
    rewrite Hmade in Eg. assert (In g0 (filter (fun g => nmem g made) (ety_gens (canon e1 tty))))
      by (apply filter_In; split; [exact Hin | apply nmem_in; exact Hg]).
    rewrite Eg in H. destruct H. }
  assert (Hof : A.opt_fits (trF (canon e1 tty)) (option_map (fun t => trF (canon e1 t)) exp)) by exact (elab_opt_fits_tr e1 tty exp (cr_uf _ _ _ _ Hc1) Eo).
  destruct tl as [y|].
  - stepok H2 (ok_synth f c1 e1 y exp) Hi1 Hi2' Fs2'.
    destruct (bs_synth _ IH c1 e1 y exp s1 tx tty s2 H2 F1 C1 Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hc1 Hx Hxs (post_frame _ _ _ Fv Hp)) as [Ey [Wy Sy]].
    rewrite Ecanon, Eexp in Ey, Hof. rewrite Ecanon in Hesc. split; [eapply A.BlkECheck; [exact (Kt _ _ _ _ Ey) | exact Hesc | exact Hof]|].
    split; [exact Wy|]. split; [exact Sy|]. split; reflexivity.
  - injection H2 as <- <- <-.
    rewrite Ecanon, Eexp in Hof. rewrite Ecanon in Hesc. split.
    + eapply A.BlkECheck; [| exact Hesc | exact Hof].
      replace (trF (canon e (if d then YNever else YUnit))) with (if d then A.never else TUnit) by (destruct d; reflexivity).
      apply Ln.
    + split; [destruct d; reflexivity|]. split; [destruct d;constructor|]. split; [reflexivity|]. destruct d; [right | left]; reflexivity.
Qed.

Lemma fold_stmts_app : forall xs ys tx, fold_stmts (xs ++ ys) tx = fold_stmts xs (fold_stmts ys tx).
Proof. intros xs ys tx. induction xs as [|x r IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Lemma stmts_step : forall f, OK f -> StmtsOK (S f).
Proof.
  intros f IH c e ss s xs e1 c1 d s' H Hne F C Hi Ii Ifwd Hc Hp.
  destruct ss as [|s0 rest]; [exfalso; exact (Hne eq_refl)|]. cbn[check_stmts] in H.
  binv H as r1 s1 H1. destruct r1 as [[[xs1 e2] c2] d1].
  stepok H1 (ok_check_stmt f c e s0) Hi Hi1 Fs1.
  destruct rest as [|s5 rest5].
  - injection H as -> -> -> -> ->.
    destruct (bs_stmt _ IH c e s0 s xs e1 c1 d s' H1 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hp) as (F1 & C1 & made & Hc1 & Fv & Ef & K & L).
    exists F1, C1, made. split; [exact Hc1|]. split; [exact Fv|]. split; [exact Ef|]. split; [|exact L].
    intros y exp tx tt Ey. rewrite <- (app_nil_r made). apply K; [right; discriminate | apply A.BlkTail; exact Ey].
  - binv H as r2 s2 H2. destruct r2 as [[[ys e3] c3] d2]. injection H as <- -> -> -> ->.
    stepok H2 (ok_check_stmts f c2 e2 (s5 :: rest5)) Hi1 Hi2 Fs2.
    destruct (bs_stmt _ IH c e s0 s xs1 e2 c2 d1 s1 H1 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc (post_back _ _ _ Fs2 Hp)) as (F1 & C1 & made1 & Hc1 & Fv1 & Ef1 & K1 & L1).
    destruct (bs_stmts _ IH c2 e2 (s5 :: rest5) s1 ys e1 c1 d s' H2 ltac:(discriminate) F1 C1 Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hc1
                (post_frame _ _ _ Fv1 Hp)) as (F2 & C2 & made2 & Hc2 & Fv2 & Ef2 & K2 & L2).
    exists F2, C2, (made1 ++ made2). split; [exact Hc2|]. split; [congruence|].
    split; [rewrite Ef2, Ef1, app_assoc; reflexivity|].
    split.
    + intros y exp tx tt Ey. rewrite fold_stmts_app. apply K1; [left; discriminate | exact (K2 _ _ _ _ Ey)].
    + intro exp. rewrite fold_stmts_app. apply K1; [left; discriminate | apply L2].
Qed.



Lemma stmt_step : forall f, OK f -> Pending -> StmtOK (S f).
Proof.
  intros f IH PD c e st0 s xs e1 c1 d s' H F C Hi Ii Ifwd Hc Hp.
  destruct (pending_stmt st0) eqn:Ps; [exact (pd_stmt PD f IH c e st0 s xs e1 c1 d s' Ps H F C Hi Ii Ifwd Hc Hp)|].
  destruct st0 as [n ann y|n ann y|n y|its|cnd bl|bl|y]; cbn in Ps; try discriminate Ps; cbn[check_stmt] in H.
  - (* LSLet *) binv H as decl s1 H1.
    binv H as vt s2 H2; destruct vt as [v vt].
    binv H as ty s3 H3.
    binv H as x s4 H4.
    injection H as <- <- <- <- <-.
    assert (Hd : Inv s1 /\ Fr s s1) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-; exact (okstep _ _ _ _ _ (ok_ety_of a (cx_mod c) (tymap e) e (cx_this c)) Hi Ht)
       | injection H1 as <- <-; split; [exact Hi | apply fr_refl] ]);
    destruct Hd as [Hi1 Fs1].
    stepok H2 (ok_synth f c e y decl) Hi1 Hi2 Fs2.
    assert (E3 : s3 = s2) by (destruct decl as [d0|]; [destruct (fits _ vt d0); [injection H3 as _ <-; reflexivity | noerr H3] | injection H3 as _ <-; reflexivity]).
    subst s3.
    stepok H4 (ok_local_var n) Hi2 Hi4 Fs4.
    pose proof (post_back _ _ _ Fs4 Hp) as Hp2.
    pose proof (post_back _ _ _ Fs2 Hp2) as Hp1.
    destruct (local_named n s2 x s4 s4 F H4 (fr_refl _) Hp) as [Nn Vn].
    (* the declared type *)
    assert (HA : match ann with
                 | Some a => exists d0, A.TyOf W (A.fr_mod F) (A.c_ty C) (A.fr_this F) a d0 /\ option_map (fun t => trF (canon e t)) decl = Some d0
                 | None => option_map (fun t => trF (canon e t)) decl = None
                 end /\ opt_wf decl /\ opt_support W decl) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-;
         destruct (pd_ty PD a c e s d0 s0 Ht F C Hi Hc (proj1 Hp1)) as [Ty [Wd Sd]];
         split; [exists (trF (canon e d0)); split; [exact Ty | reflexivity] | split; [exact Wd|exact Sd]]
       | injection H1 as <- <-; split; [reflexivity | split; exact I] ]);
    destruct HA as [HA [Wdecl Sdecl]].
    assert(Icache_prefix:InfoInv s1)by cache_inv.
    assert(Fwd_prefix:FwdInv s1)by fwd_inv.
    destruct (bs_synth _ IH c e y decl s1 v vt s2 H2 F C Hi1 Icache_prefix Fwd_prefix Hc Wdecl Sdecl Hp2) as [Ey [Wy Sy]].
    assert (HT : match option_map (fun t => trF (canon e t)) decl with Some d0 => A.fits (trF (canon e vt)) d0 /\ trF (canon e ty) = d0 | None => trF (canon e ty) = trF (canon e vt) end /\ WFty ty /\ Support W ty) by
      (destruct decl as [d0|];
       [ cbn in Wdecl |- *; destruct (fits e vt d0) eqn:Ef; [injection H3 as E; subst ty; split; [split; [exact (fits_tr e vt d0 (cr_uf _ _ _ _ Hc) Ef) | reflexivity] | split;[exact Wdecl|exact Sdecl]] | noerr H3]
       | injection H3 as E; subst ty; split; [reflexivity | split;[exact Wy|exact Sy]] ]);
    destruct HT as [HT [Wty Sty]].
    exists F, (A.add_local C n (trF (canon e ty)) (tr_mut Elab.LLet)), [].
    split; [exact (cxrel_local _ _ _ _ _ _ _ Hc Wty Sty)|].
    split; [reflexivity|]; split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn[fold_stmts app]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSLet | exact HA | exact Ey | exact HT | exact Nn | exact Hb].
    + intro exp. cbn[fold_stmts]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSLet | exact HA | exact Ey | exact HT | exact Nn | apply A.BlkEmpty].
  - (* LSVar *) binv H as decl s1 H1.
    binv H as vt s2 H2; destruct vt as [v vt].
    binv H as ty s3 H3.
    binv H as x s4 H4.
    injection H as <- <- <- <- <-.
    assert (Hd : Inv s1 /\ Fr s s1) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-; exact (okstep _ _ _ _ _ (ok_ety_of a (cx_mod c) (tymap e) e (cx_this c)) Hi Ht)
       | injection H1 as <- <-; split; [exact Hi | apply fr_refl] ]);
    destruct Hd as [Hi1 Fs1].
    stepok H2 (ok_synth f c e y decl) Hi1 Hi2 Fs2.
    assert (E3 : s3 = s2) by (destruct decl as [d0|]; [destruct (fits _ vt d0); [injection H3 as _ <-; reflexivity | noerr H3] | injection H3 as _ <-; reflexivity]).
    subst s3.
    stepok H4 (ok_local_var n) Hi2 Hi4 Fs4.
    pose proof (post_back _ _ _ Fs4 Hp) as Hp2.
    pose proof (post_back _ _ _ Fs2 Hp2) as Hp1.
    destruct (local_named n s2 x s4 s4 F H4 (fr_refl _) Hp) as [Nn Vn].
    (* the declared type *)
    assert (HA : match ann with
                 | Some a => exists d0, A.TyOf W (A.fr_mod F) (A.c_ty C) (A.fr_this F) a d0 /\ option_map (fun t => trF (canon e t)) decl = Some d0
                 | None => option_map (fun t => trF (canon e t)) decl = None
                 end /\ opt_wf decl /\ opt_support W decl) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-;
         destruct (pd_ty PD a c e s d0 s0 Ht F C Hi Hc (proj1 Hp1)) as [Ty [Wd Sd]];
         split; [exists (trF (canon e d0)); split; [exact Ty | reflexivity] | split; [exact Wd|exact Sd]]
       | injection H1 as <- <-; split; [reflexivity | split; exact I] ]);
    destruct HA as [HA [Wdecl Sdecl]].
    assert(Icache_prefix:InfoInv s1)by cache_inv.
    assert(Fwd_prefix:FwdInv s1)by fwd_inv.
    destruct (bs_synth _ IH c e y decl s1 v vt s2 H2 F C Hi1 Icache_prefix Fwd_prefix Hc Wdecl Sdecl Hp2) as [Ey [Wy Sy]].
    assert (HT : match option_map (fun t => trF (canon e t)) decl with Some d0 => A.fits (trF (canon e vt)) d0 /\ trF (canon e ty) = d0 | None => trF (canon e ty) = trF (canon e vt) end /\ WFty ty /\ Support W ty) by
      (destruct decl as [d0|];
       [ cbn in Wdecl |- *; destruct (fits e vt d0) eqn:Ef; [injection H3 as E; subst ty; split; [split; [exact (fits_tr e vt d0 (cr_uf _ _ _ _ Hc) Ef) | reflexivity] | split;[exact Wdecl|exact Sdecl]] | noerr H3]
       | injection H3 as E; subst ty; split; [reflexivity | split;[exact Wy|exact Sy]] ]);
    destruct HT as [HT [Wty Sty]].
    exists F, (A.add_local C n (trF (canon e ty)) (tr_mut Elab.LVar)), [].
    split; [exact (cxrel_local _ _ _ _ _ _ _ Hc Wty Sty)|].
    split; [reflexivity|]; split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn[fold_stmts app]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSVar | exact HA | exact Ey | exact HT | exact Nn | exact Hb].
    + intro exp. cbn[fold_stmts]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSVar | exact HA | exact Ey | exact HT | exact Nn | apply A.BlkEmpty].
  - (* assignment *)
    destruct (sassoc n (en_locals e)) as [[tv mu]|] eqn:El; [|noerr H].
    destruct mu; try noerr H.
    binv H as vt s2 H2. destruct vt as [v vt].
    stepok H2 (ok_synth f c e y (Some tv)) Hi Hi2 Fs2.
    destruct (fits e vt tv) eqn:Ef; [|noerr H].
    binv H as x s4 H4. injection H as <- <- <- <- <-.
    stepok H4 (ok_local_var n) Hi2 Hi4 Fs4.
    pose proof (post_back _ _ _ Fs4 Hp) as Hp2.
    destruct (local_named n s2 x s4 s4 F H4 (fr_refl _) Hp) as [Nn Vn].
    destruct (bs_synth _ IH c e y (Some tv) s v vt s2 H2 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc (cr_locals_wf _ _ _ _ Hc n _ El) (@es_locals W e (cr_support _ _ _ _ Hc) n (tv,Elab.LVar) El) Hp2) as [Ey [Wy Sy]].
    assert (Hl : A.slook n (A.c_locals C) = Some (trF (canon e tv), A.LVar)) by (rewrite (cr_locals _ _ _ _ Hc), El; reflexivity).
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn[fold_stmts app]. rewrite <- Vn.
      eapply A.BlkAssign; [exact Hl | exact Ey | exact (fits_tr e vt tv (cr_uf _ _ _ _ Hc) Ef) | exact Hb | exact Nn].
    + intro exp. cbn[fold_stmts]. rewrite <- Vn.
      eapply A.BlkAssign; [exact Hl | exact Ey | exact (fits_tr e vt tv (cr_uf _ _ _ _ Hc) Ef) | apply A.BlkEmpty | exact Nn].
  - (* while *)
    binv H as cond s1 H1. binv H as r2 s2 H2. destruct r2 as [[body bt0] bt1]. injection H as <- <- <- <- <-.
    stepok H1 (ok_check_bool f c e cnd) Hi Hi1 Fs1.
    stepok H2 (ok_synth_block f (cx_in_loop c) e bl None) Hi1 Hi2 Fs2.
    pose proof (post_back _ _ _ Fs2 Hp) as Hp1.
    pose proof (bs_cond _ IH c e cnd s cond s1 H1 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hp1) as Hcd.
    destruct (bs_block _ IH (cx_in_loop c) e bl None s1 body bt0 bt1 s2 H2 (A.with_loop F) C Hi1 ltac:(cache_inv) ltac:(fwd_inv) (cxrel_loop _ _ _ _ Hc) I I
                (post_frame _ _ _ eq_refl Hp)) as (Eb & _).
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn[fold_stmts app]. eapply A.BlkWhile; [exact Hcd | exact Eb | exact Hb].
    + intro exp. cbn[fold_stmts]. eapply A.BlkWhile; [exact Hcd | exact Eb | apply A.BlkEmpty].
  - (* loop *)
    binv H as r2 s2 H2. destruct r2 as [[body bt0] bt1]. injection H as <- <- <- <- <-.
    destruct (bs_block _ IH (cx_in_loop c) e bl None s body bt0 bt1 s2 H2 (A.with_loop F) C Hi ltac:(cache_inv) ltac:(fwd_inv) (cxrel_loop _ _ _ _ Hc) I I
                (post_frame _ _ _ eq_refl Hp)) as (Eb & _).
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 Hne Hb. cbn[fold_stmts app].
      eapply A.BlkLoop; [destruct Hne as [Hn|Hn]; [left | right; left]; exact Hn | exact Eb | exact Hb].
    + intro exp. cbn[fold_stmts]. destruct (has_break_b bl) eqn:Hk; cbn[negb].
      * eapply A.BlkLoop; [right; right; apply (proj1 (proj2 has_break_spec)); exact Hk | exact Eb | apply A.BlkEmpty].
      * eapply A.BlkLoopLast; [exact Eb|]. intro Hb. apply (proj1 (proj2 has_break_spec)) in Hb. congruence.
  - (* an expression *)
    binv H as r2 s2 H2. destruct r2 as [v vt]. injection H as <- <- <- <- <-.
    destruct (bs_synth _ IH c e y None s v vt s2 H2 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc I I Hp) as [Ey [Wy Sy]].
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 Hne Hb. cbn[fold_stmts app]. eapply A.BlkExpr; [exact Hne | exact Ey | exact Hb].
    + intro exp. cbn[fold_stmts].
      rewrite (is_never_tr Fl (canon e vt) (canon_wf e vt (proj2 (cr_uf _ _ _ _ Hc)) Wy)). apply A.BlkExprLast. exact Ey.
Qed.

Lemma ok_join : forall mi e a b0 exp, ok (join mi e a b0 exp).
Proof. intros. unfold join. okt. Qed.

Lemma bool_type_spec : forall mi s bt s', bool_type mi s = Good (bt, s') -> Later s ->
  s' = s /\ exists bi x, sassoc "bool"%string (lang_modules w) = Some bi /\
    sassoc "Bool"%string (gm_names (get_mod w bi)) = Some x /\ bt = YNom x [].
Proof.
  intros mi s bt s' H [Lw _ _ Ll]. unfold bool_type in H. binv H as bm s3 Hl. unfold lang in Hl.
  binv Hl as st0 s4 Hg. injection Hg as E1 E2. subst st0 s4. injection Hl as E1 E2. subst bm s3.
  rewrite Ll in H. destruct (sassoc "bool"%string (lang_modules w)) as [bi|] eqn:Eb; [|noerr H].
  binv H as m s5 Hm. unfold the_mod in Hm. injection Hm as E1 E2. subst m s5.
  rewrite Lw in H. destruct (sassoc "Bool"%string (gm_names (get_mod w bi))) as [x|] eqn:Ex; [|noerr H].
  injection H as E1 E2. subst bt s'. split; [reflexivity|]. exists bi, x. auto.
Qed.

Lemma cond_step : forall f, OK f -> CondOK (S f).
Proof.
  intros f IH c e y s v s' H F C Hi Ii Ifwd Hc Hp.
  assert (Fs : Fr s s') by exact (proj2 (ok_check_bool (S f) c e y s v s' Hi H)).
  destruct (post_back _ _ _ Fs Hp) as [Ls _].
  cbn[check_bool] in H. binv H as bt s1 H1. binv H as r s2 H2. destruct r as [v0 t0].
  destruct (bool_type_spec _ _ _ _ H1 Ls) as [-> (bi & x & Eb & Ex & ->)].
  destruct (fits e t0 (YNom x [])) eqn:Ef; [|noerr H]. injection H as E1 E2. subst.
  destruct (bs_synth _ IH c e y (Some (YNom x [])) s v t0 s' H2 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc eq_refl (lt_bool_support _ Ls bi x Eb Ex) Hp) as [Ey _].
  eapply A.CondBool; [exact (Hbool bi x Eb Ex) | exact Ey | exact (fits_tr e t0 _ (cr_uf _ _ _ _ Hc) Ef)].
Qed.

Lemma branch_tr : forall bl t, trF (block_bt bl t) = A.branch_ty bl (trF t).
Proof. intros [ss [y|]] t; reflexivity. Qed.

Lemma branch_canon_tr : forall e bl t, trF (canon e (block_bt bl t)) = A.branch_ty bl (trF (canon e t)).
Proof. intros e [ss [y|]] t; reflexivity. Qed.

Lemma block_bt_wf : forall bl t, WFty t -> WFty (block_bt bl t).
Proof. intros [ss [y|]] t W0; cbn; [exact W0 | reflexivity]. Qed.

Lemma block_bt_support : forall bl t, Support W t -> Support W(block_bt bl t).
Proof. intros [ss [y|]] t S; [exact S|constructor]. Qed.

Lemma if_step : forall f, OK f -> IfOK (S f).
Proof.
  intros f IH c e x exp s v t s' H F C Hi Ii Ifwd Hc Hx Hxs Hp.
  destruct x as [| | | | | | |c0 thn els| | | | | | | | ]; cbn[synth_if] in H; try discriminate H.
  binv H as cond s1 H1. binv H as r s2 H2. destruct r as [[tx t1] tty].
  stepok H1 (ok_check_bool f c e c0) Hi Hi1 Fs1.
  stepok H2 (ok_synth_block f c e thn exp) Hi1 Hi2 Fs2.
  destruct els as [y|]; cycle 1.
  - destruct (negb (Elab.opt_fits e YUnit exp)) eqn:E1; [noerr H|].
    destruct (negb (is_unit_or_never (canon e tty))) eqn:E2; [noerr H|]. injection H as <- <- <-.
    pose proof (bs_cond _ IH c e c0 s cond s1 H1 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc (post_back _ _ _ Fs2 Hp)) as Hcd.
    destruct (bs_block _ IH c e thn exp s1 tx t1 tty s2 H2 F C Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hc Hx Hxs Hp) as (Eb & Wb & Sb & Et & Tl).
    split; [|split;[reflexivity|constructor]].
    eapply A.ExIfThen; [exact Hcd | exact Eb | |].
    + apply negb_false_iff in E1. exact (elab_opt_fits_tr e YUnit exp (cr_uf _ _ _ _ Hc) E1).
    + apply negb_false_iff in E2. destruct thn as [ss [y0|]]; cbn in Et, Tl; subst tty.
      * destruct (canon e t1); try discriminate E2; [left | right]; reflexivity.
      * destruct Tl as [-> | ->]; [left | right]; reflexivity.
  - binv H as r3 s3 H3. destruct r3 as [ex et]. binv H as tj s4 H4. injection H as <- <- <-.
    assert (Hbr : Inv s3 /\ Fr s2 s3 /\ forall F', Post s3 F' -> CxRel c e F' C ->
              (exists et', A.Branch W P F' C y (option_map (fun t => trF (canon e t)) exp) ex et' /\ (exp = None -> et' = trF (canon e et))) /\ WFty et /\ Support W et).
    { destruct y as [| | | | | | |c5 b5 e5| | | |b5| | | | ]; cbn beta iota in H3;
        try (destruct (okstep _ _ _ _ _ (ok_synth _ _ _ _ _) Hi2 H3) as [Hi3 Fs3]; split; [exact Hi3|]; split; [exact Fs3|];
             intros F' Hp' Hc'; destruct (bs_synth _ IH c e _ exp s2 ex et s3 H3 F' C Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hc' Hx Hxs Hp') as [Ey [Wy Sy]];
             split; [exists (trF (canon e et)); split; [apply A.BranchExpr; [intros b6 E6; discriminate E6 | exact Ey] | reflexivity] | split;[exact Wy|exact Sy]]).
      - stepok H3 (ok_synth_if f c e (AEIf c5 b5 e5) exp) Hi2 Hi3 Fs3. split; [exact Hi3|]. split; [exact Fs3|].
        intros F' Hp' Hc'. destruct (bs_if _ IH c e _ exp s2 ex et s3 H3 F' C Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hc' Hx Hxs Hp') as [Ey [Wy Sy]].
        split; [exists (trF (canon e et)); split; [apply A.BranchExpr; [intros b6 E6; discriminate E6 | exact Ey] | reflexivity] | split;[exact Wy|exact Sy]].
      - binv H3 as r6 s6 H6. destruct r6 as [[v6 t6] bt6]. injection H3 as <- <- <-.
        stepok H6 (ok_synth_block f c e b5 exp) Hi2 Hi3 Fs3. split; [exact Hi3|]. split; [exact Fs3|].
        intros F' Hp' Hc'. destruct (bs_block _ IH c e b5 exp s2 v6 t6 bt6 s6 H6 F' C Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hc' Hx Hxs Hp') as (Eb & Wb & Sb & Et & _).
        split; [|split; [subst bt6; apply block_bt_wf; exact Wb|subst bt6; apply block_bt_support; exact Sb]].
        eexists. split; [apply A.BranchBlock; exact Eb|]. intros ->. subst bt6. symmetry. apply branch_canon_tr. }
    destruct Hbr as (Hi3 & Fs3 & Hbr).
    stepok H4 (ok_join (cx_mod c) e tty et exp) Hi3 Hi4' Fs4'.
    assert (Hp3 : Post s3 F) by exact (post_back _ _ _ Fs4' Hp).
    destruct (Hbr F Hp3 Hc) as ((et' & Hb' & Ee) & [We Se]).
    pose proof (bs_cond _ IH c e c0 s cond s1 H1 F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc (post_back _ _ _ Fs2 (post_back _ _ _ Fs3 Hp3))) as Hcd.
    destruct (bs_block _ IH c e thn exp s1 tx t1 tty s2 H2 F C Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hc Hx Hxs (post_back _ _ _ Fs3 Hp3)) as (Eb & Wb & Sb & Et & _).
    assert (Wt : WFty tty) by (subst tty; apply block_bt_wf; exact Wb).
    assert(Stty:Support W tty) by(subst tty;apply block_bt_support;exact Sb).
    destruct exp as [xe|].
    + unfold join in H4. injection H4 as <- <-. split; [|split;[exact Hx|exact Hxs]].
      eapply A.ExIfElse; [exact Hcd | exact Eb | exact Hb' | apply A.JoinExp].
    + destruct (join_tr _ _ _ _ _ _ _ _ H4 (cr_uf _ _ _ _ Hc) Wt We I) as [J Wj]. split; [|split;[exact Wj|exact(@join_support W (cx_mod c) e tty et None s3 tj s4 H4 (cr_support _ _ _ _ Hc) Stty Se I)]].
      eapply A.ExIfElse; [exact Hcd | exact Eb | exact Hb' |].
      rewrite (Ee eq_refl). subst tty. rewrite branch_canon_tr in J. exact J.
Qed.

Lemma ok_step : forall f, OK f -> Pending -> OK (S f).
Proof.
  intros f IH PD. constructor.
  - exact (synth_step f IH PD).
  - exact (block_step f IH).
  - exact (stmts_step f IH).
  - exact (stmt_step f IH PD).
  - exact (cond_step f IH).
  - exact (if_step f IH).
  - exact (path_step f (bs_const _ IH)).
  - exact (pd_const PD f IH).
Qed.

(* The fragment, at every fuel, modulo the pending constructs. *)
Theorem bodies_sound : Pending -> forall f, OK f.
Proof. intros PD f. induction f as [|f IH]; [exact ok_zero | exact (ok_step f IH PD)]. Qed.

End Bodies.
