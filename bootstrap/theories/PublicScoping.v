(* Scoping of elaborated types (T29), up to declared type symbols.

   PublicFormation's [FormedCx] says that a context's tables are ok and
   that its method keys and type-table keys are in scope ([ScopedTy]: in
   [scope_gens], or a declared type symbol). This file adds the matching
   facts for the rest of a context, [CxScoped]: its locals' types, its
   provisions' types and instantiations, and the frame's receiver type.

   - [ex_scoped] (a fixpoint alongside [PublicFormation.ex_ok]): an
     elaborated expression's type is scoped, and bind items keep
     [CxScoped]. A block may not let a type it made escape ([D78]), so a
     block's type is scoped in the context around it.
   - [literal_scoped]: a literal's type is scoped.
   - [demand_at_site_scoped]: a callee need read at the call site is ok
     and scoped. A requirement the caller leaves unbound is read as itself
     (`at_site`), a declared type symbol, which is why scoping admits
     declared symbols (examples/family-unbound-need*.moss).
   - Formation of the initial contexts: [ctxof_scoped], [functx_scoped],
     and the steps the slice takes. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Scoping.
Variable W : A.world.
Import A.

Definition LocalsScoped (C : cx) : Prop :=
  forall n t mu, In (n, (t, mu)) (c_locals C) -> ScopedTy W C t.
Definition ProvsScoped (C : cx) : Prop :=
  forall k p, In (k, p) (c_provisions C) -> GensIn (GenScoped W C) (provision_gens p).
Definition ThisScoped (F : frame) (C : cx) : Prop :=
  forall t, fr_this F = Some t -> ScopedTy W C t.
Definition CxScoped (F : frame) (C : cx) : Prop :=
  LocalsScoped C /\ ProvsScoped C /\ ThisScoped F C.
Definition OptScoped (C : cx) (o : option ty) : Prop := forall t, o = Some t -> ScopedTy W C t.

(* ---- generations ------------------------------------------------------------ *)

Lemma scoped_mono : forall C C' t, incl (scope_gens C) (scope_gens C') -> ScopedTy W C t -> ScopedTy W C' t.
Proof. intros C C' t Inc Sc y Iy. exact (inscope_mono W C C' y Inc (Sc y Iy)). Qed.

Lemma gensin_mono : forall (K K' : nat -> Prop) l, (forall y, K y -> K' y) -> GensIn K l -> GensIn K' l.
Proof. intros K K' l Imp G y Iy. exact (Imp y (G y Iy)). Qed.

Lemma tm_scoped : forall C y, In y (tm_gens (c_ty C)) -> GenScoped W C y.
Proof. intros C y Iy. left. exact (ty_scope C y Iy). Qed.

Lemma gens_canon : forall C t y, In y (gens (canon C t)) -> In y (gens t) \/ In y (scope_gens C).
Proof.
  intros C t y Iy. unfold canon in Iy. destruct (gens_subst t (c_sub C) y Iy) as [z [Iz [[_ ->]|[u [L Iu]]]]];
    [left; exact Iz|right; exact (scope_sub C z u y (lookup_subst_in_pair _ _ _ L) Iu)].
Qed.

Lemma never_gens : forall y, ~ In y (gens never).
Proof. intros y []. Qed.

Lemma unit_gens : forall y, ~ In y (gens TUnit).
Proof. intros y []. Qed.

Lemma sig_ret_gens : forall p g a, provision_signature p = Some g -> provision_inst p = a ->
  incl (gens (sg_ret g)) (provision_gens p).
Proof.
  intros p g a Sig _ x Ix. unfold provision_gens. rewrite Sig. apply in_or_app. left. apply in_flat_map.
  exists (sg_ret g). split; [left; reflexivity|exact Ix].
Qed.

Lemma vals_in : forall C v t, nlook v (c_vals C) = Some t -> exists a, In (IVal v, ProvidedVal t a) (c_provisions C).
Proof.
  intros C v t L. apply nlook_in in L. unfold c_vals in L. apply in_flat_map in L as [[k p] [Iq Ik]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as -> ->. eexists. exact Iq.
Qed.

Lemma fns_in : forall C f g, nlook f (c_fns C) = Some g -> exists a, In (IFn f, ProvidedFn g a) (c_provisions C).
Proof.
  intros C f g L. apply nlook_in in L. unfold c_fns in L. apply in_flat_map in L as [[k p] [Iq Ik]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as -> ->. eexists. exact Iq.
Qed.

Lemma mths_in : forall C r m g, mlook r m (c_mths C) = Some g -> exists k a, In (k, ProvidedMethod g a) (c_provisions C).
Proof.
  intros C r m g L. destruct (mlook_in _ _ _ _ L) as [q L']. unfold c_mths in L'. apply in_flat_map in L' as [[k p] [Iq Ik]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as _ E2; subst. do 2 eexists. exact Iq.
Qed.

Lemma val_scoped : forall C v t, ProvsScoped C -> nlook v (c_vals C) = Some t -> ScopedTy W C t.
Proof.
  intros C v t Ps L y Iy. destruct (vals_in C v t L) as [a Iq]. apply (Ps _ _ Iq). unfold provision_gens. cbn.
  apply in_or_app. left. apply in_or_app. left. exact Iy.
Qed.

Lemma fn_ret_scoped : forall C f g, ProvsScoped C -> nlook f (c_fns C) = Some g -> ScopedTy W C (sg_ret g).
Proof.
  intros C f g Ps L y Iy. destruct (fns_in C f g L) as [a Iq]. apply (Ps _ _ Iq).
  exact (sig_ret_gens (ProvidedFn g a) g a eq_refl eq_refl y Iy).
Qed.

Lemma mth_ret_scoped : forall C r m g, ProvsScoped C -> mlook r m (c_mths C) = Some g -> ScopedTy W C (sg_ret g).
Proof.
  intros C r m g Ps L y Iy. destruct (mths_in C r m g L) as [k [a Iq]]. apply (Ps _ _ Iq).
  exact (sig_ret_gens (ProvidedMethod g a) g a eq_refl eq_refl y Iy).
Qed.

Lemma tyof_tm_gens : forall p tm this a t y, TyOf W p tm this a t -> In y (gens t) ->
  In y (tm_gens tm) \/ (exists x, this = Some x /\ In y (gens x)).
Proof. intros p tm this a t y T Iy. exact (tyof_gens W _ _ _ _ _ T y Iy). Qed.

Lemma sigof_gens : forall s tm this g y, SigOf W s tm this g -> In y (sig_gens g) ->
  In y (tm_gens tm) \/ (exists x, this = Some x /\ In y (gens x)).
Proof.
  intros s tm this [ps r] y (d & rcv & n & args & rt & body & D & Ast & Ps & R) Iy.
  cbn [sg_ret sg_params] in Ps, R. unfold sig_gens in Iy. cbn [sg_ret sg_params flat_map] in Iy.
  apply in_app_or in Iy as [Ir|Ip].
  - destruct rt as [rta|]; cbn in R; [exact (tyof_gens W _ _ _ _ _ R y Ir)|subst r; contradiction].
  - apply in_flat_map in Ip as [t [It Iy]]. cbn in It. clear R D Ast. revert It. induction Ps as [|pt t' ps' ts' Ht Fr IH]; intros It; [contradiction|].
    destruct It as [<-|It]; [exact (tyof_gens W _ _ _ _ _ Ht y Iy)|exact (IH It)].
Qed.

Lemma sig_ret_in : forall g y, In y (gens (sg_ret g)) -> In y (sig_gens g).
Proof. intros g y Iy. unfold sig_gens. cbn. apply in_or_app. left. exact Iy. Qed.

Lemma sig_param_in : forall g t y, In t (sg_params g) -> In y (gens t) -> In y (sig_gens g).
Proof. intros g t y It Iy. unfold sig_gens. cbn. apply in_or_app. right. apply in_flat_map. exists t. split; assumption. Qed.

Lemma provision_gens_sig : forall p g y, provision_signature p = Some g ->
  In y (provision_gens p) -> In y (sig_gens g) \/ In y (inst_gens (provision_inst p)).
Proof.
  intros p g y Sig Iy. unfold provision_gens in Iy. rewrite Sig in Iy. apply in_app_or in Iy as [Iy|Iy]; [left|right]; exact Iy.
Qed.

(* ---- context steps ---------------------------------------------------------- *)

Lemma scope_add_local : forall C n t mu, scope_gens (add_local C n t mu) = scope_gens C.
Proof. reflexivity. Qed.

Lemma add_local_scoped : forall F C n t mu, CxScoped F C -> ScopedTy W C t -> CxScoped F (add_local C n t mu).
Proof.
  intros F C n t mu [Ls [Ps Ts]] St. split; [|split; [exact Ps|exact Ts]].
  intros n' t' mu' [E|Iq]; [injection E as -> -> ->; exact St|exact (Ls _ _ _ Iq)].
Qed.

Lemma add_locals_scoped : forall F mu l C, CxScoped F C -> (forall n t, In (n, t) l -> ScopedTy W C t) ->
  CxScoped F (fold_left (fun C q => add_local C (fst q) (snd q) mu) l C).
Proof.
  intros F mu l. induction l as [|[n t] r IH]; intros C Cs Sc; cbn; [exact Cs|].
  apply IH; [apply add_local_scoped; [exact Cs|exact (Sc n t (or_introl eq_refl))]|].
  intros n' t' Iq. exact (Sc n' t' (or_intror Iq)).
Qed.

Lemma add_locals_scope : forall mu l C, scope_gens (fold_left (fun C q => add_local C (fst q) (snd q) mu) l C) = scope_gens C.
Proof. intros mu l. induction l as [|[n t] r IH]; intros C; cbn; [reflexivity|]. rewrite IH. reflexivity. Qed.

Lemma set_ty_scoped : forall F C t v, CxScoped F C -> CxScoped F (set_ty C t v).
Proof.
  intros F C t v [Ls [Ps Ts]]. pose proof (scope_set_ty C t v) as Inc. split; [|split].
  - intros n t' mu Iq. exact (scoped_mono _ _ _ Inc (Ls n t' mu Iq)).
  - intros k p Iq y Iy. exact (inscope_mono W _ _ y Inc (Ps k p Iq y Iy)).
  - intros t' E. exact (scoped_mono _ _ _ Inc (Ts t' E)).
Qed.

Lemma fold_set_ty_scoped : forall F l C, CxScoped F C -> CxScoped F (fold_left (fun C q => set_ty C (fst q) (snd q)) l C).
Proof.
  intros F l. induction l as [|[x v] r IH]; intros C Cs; cbn; [exact Cs|]. apply IH. exact (set_ty_scoped F C x v Cs).
Qed.

Lemma set_provision_scoped : forall F C k p, CxScoped F C -> GensIn (GenScoped W C) (provision_gens p) ->
  CxScoped F (set_provision C k p).
Proof.
  intros F C k p [Ls [Ps Ts]] Sp. split; [exact Ls|split; [|exact Ts]].
  intros k' p' [E|Iq]; [injection E as <- <-; exact Sp|]. apply filter_In in Iq as [Iq _]. exact (Ps _ _ Iq).
Qed.

Lemma provide_scoped : forall F C k a C1, CxScoped F C1 -> GensIn (GenScoped W C1) (inst_gens a) ->
  CxScoped F (provide C k a C1).
Proof.
  intros F C k a C1 Cs Sa. unfold provide.
  destruct (ilook k (c_provisions C1)) as [p0|] eqn:L; [|exact Cs].
  apply ilook_in_key in L. pose proof (proj1 (proj2 Cs) k p0 L) as S0.
  destruct p0 as [t a0|g a0|g a0|fx]; try exact Cs; apply set_provision_scoped; try exact Cs;
    intros y Iy; unfold provision_gens in Iy; cbn [provision_signature provision_inst] in Iy;
    apply in_app_or in Iy as [Iy|Iy]; try exact (Sa y Iy);
    apply S0; unfold provision_gens; cbn [provision_signature]; apply in_or_app; left; exact Iy.
Qed.

Lemma set_val_scoped : forall F C v t, CxScoped F C -> ScopedTy W C t -> CxScoped F (set_val C v t).
Proof.
  intros F C v t Cs St. apply set_provision_scoped; [exact Cs|]. intros y Iy. unfold provision_gens in Iy. cbn in Iy.
  rewrite !app_nil_r in Iy. exact (St y Iy).
Qed.

Lemma set_sig_scoped : forall F C k g, CxScoped F C -> GensIn (GenScoped W C) (sig_gens g) ->
  CxScoped F (set_provision C k (ProvidedFn g [])) /\ CxScoped F (set_provision C k (ProvidedMethod g [])).
Proof.
  intros F C k g Cs Sg. split; apply set_provision_scoped; try exact Cs;
    intros y Iy; unfold provision_gens in Iy; cbn [provision_signature provision_inst] in Iy;
    rewrite app_nil_r in Iy; exact (Sg y Iy).
Qed.

Lemma set_fun_scoped : forall F C u fx, CxScoped F C -> (forall x tx, In (x, tx) fx -> nlook x (c_ty C) = Some tx) ->
  CxScoped F (set_fun C u fx).
Proof.
  intros F C u fx Cs Fx. apply set_provision_scoped; [exact Cs|]. intros y Iy. unfold provision_gens in Iy. cbn in Iy.
  apply in_flat_map in Iy as [[x o] [Iq Iy]]. apply in_map_iff in Iq as [[x' tx] [E Iq]]. cbn in E. injection E as -> <-.
  apply tm_scoped. exact (nlook_gens _ _ _ _ (Fx _ _ Iq) Iy).
Qed.

(* ---- a functor's result ------------------------------------------------------ *)

Definition TyIn (K : nat -> Prop) (t : ty) : Prop := GensIn K (gens t).

Lemma obr_gens' : forall p tm a bs, OutputBrackets W p tm a bs -> forall g, In g (tm_gens bs) -> In g (tm_gens tm).
Proof.
  intros p tm a bs B. destruct B as [|bs0 l F]; [intros g []|].
  assert (H : incl (tm_gens l) (tm_gens tm)).
  { induction F as [|b tb bs' l' (sp & sa & E1 & R1 & K1 & T1) Fr IH]; [intros g []|].
    destruct tb as [y v]. cbn in T1. intros g Ig. cbn in Ig. apply in_app_or in Ig as [Ig|Ig]; [|exact (IH g Ig)].
    exact (tyof_none_gens W _ _ _ _ T1 g Ig). }
  intros g Ig. apply H. destruct (tm_gens_in _ g Ig) as [x [u [Iq Iu]]].
  apply (proj2 (in_rev _ _)) in Iq. exact (in_tm_gens _ _ _ _ Iq Iu).
Qed.

Lemma inst_in_gens : forall a x t y, In (x, Some t) a -> In y (gens t) -> In y (inst_gens a).
Proof. intros a x t y Iq Iy. unfold inst_gens. apply in_flat_map. exists (x, Some t). split; assumption. Qed.

Lemma output_entries_gens : forall (K : nat -> Prop),
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' ->
     GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' ->
     GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es).
Proof.
  intros K.
  assert (Full : forall p ob tm a bs, OutputBrackets W p (ob ++ tm) a bs -> GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) ->
    GensIn K (tm_gens (bs ++ ob ++ tm))).
  { intros p ob tm a bs B SOb STm y Iy. assert (Two : forall y, In y (tm_gens (ob ++ tm)) -> K y)
      by (intros z Iz; apply tm_gens_app in Iz as [Iz|Iz]; [exact (SOb z Iz)|exact (STm z Iz)]).
    apply tm_gens_app in Iy as [Iy|Iy]; [exact (Two y (obr_gens' _ _ _ _ B y Iy))|exact (Two y Iy)]. }
  assert (Inst : forall s tm a, AppOf W s tm a -> GensIn K (tm_gens tm) -> InstP (TyIn K) a).
  { intros s tm a App S x t Iq y Iy. exact (S y (appof_scoped W _ _ _ App y (inst_in_gens a x t y Iq Iy))). }
  assert (Sig : forall s tm this g, SigOf W s tm this g -> GensIn K (tm_gens tm) -> (forall x, this = Some x -> TyIn K x) ->
    SigP (TyIn K) g).
  { intros s tm this g Sg S Th. assert (All : forall y, In y (sig_gens g) -> K y).
    { intros y Iy. destruct (sigof_gens s tm this g y Sg Iy) as [I|[x [E Ix]]]; [exact (S y I)|exact (Th x E y Ix)]. }
    split; [|intros y Iy; exact (All y (sig_ret_in g y Iy))].
    apply Forall_forall. intros t It y Iy. exact (All y (sig_param_in g t y It Iy)). }
  apply (oies_mut W
    (fun p ob tm sp es tm' _ => GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es)
    (fun p ob tm sps es tm' _ => GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es)).
  - (* type *) intros. repeat constructor.
  - (* nothing *) intros. constructor.
  - (* val *) intros p ob tm a pa s d n t ty0 bs ap R D Ast B T App SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl. constructor; [|constructor]. cbn.
    split; [intros y Iy; exact (Fl y (tyof_none_gens W _ _ _ _ T y Iy))|exact (Inst _ _ _ App Fl)].
  - (* fn *) intros p ob tm a pa s g bs ap d rcv n ps rt R Kd D Ast B Sg App SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl. constructor; [|constructor]. cbn.
    split; [exact (Sig _ _ _ _ Sg Fl (fun x E => ltac:(discriminate)))|exact (Inst _ _ _ App Fl)].
  - (* functor *) intros p ob tm a bs pa u d n ps args res fx R D Ast B Fixd FR SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl. constructor; [|constructor]. cbn.
    intros x tx Iq y Iy. destruct (proj1 (FR x tx) Iq) as [_ L]. exact (Fl y (nlook_gens _ _ _ _ L Iy)).
  - (* method *) intros p ob tm a pa dn r m rt g bs ap R MN B Recv Sg App SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl.
    assert (Rt : TyIn K rt) by (intros y Iy; exact (Fl y (recv_gens W _ _ _ Recv y Iy))).
    constructor; [|constructor]. cbn.
    split; [exact Rt|split; [exact (Sig _ _ _ _ Sg Fl (fun x E => ltac:(injection E as <-; exact Rt)))|exact (Inst _ _ _ App Fl)]].
  - (* context *) intros p ob tm a pa c d n its bs es tm' R D Ast B Rest IH SOb STm.
    apply IH; [|exact STm]. intros y Iy. apply tm_gens_app in Iy as [Iy|Iy]; [|exact (SOb y Iy)].
    apply (obr_gens' _ _ _ _ B) in Iy. apply tm_gens_app in Iy as [Iy|Iy]; [exact (SOb y Iy)|exact (STm y Iy)].
  - intros. constructor.
  - intros p ob tm sp rest es1 tm1 es2 tm2 H1 IH1 H2 IH2 SOb STm.
    pose proof (proj1 (output_entries_tm W) _ _ _ _ _ _ H1) as ->.
    apply Forall_app. split; [exact (IH1 SOb STm)|exact (IH2 SOb STm)].
Qed.

Lemma entry_provision_gens : forall (K : nat -> Prop) s e k p,
  (forall g u, In (g, u) s -> GensIn K (gens u)) -> EntryP (TyIn K) e ->
  In (k, p) (entry_provision s (fun a => a) e) -> GensIn K (provision_gens p).
Proof.
  intros K s [t|v t a|f g a|r m g a|u fx] k p Range Ok Iq; cbn in Iq; try contradiction;
    destruct Iq as [E|[]]; injection E as <- <-; cbn in Ok.
  - destruct Ok as [Ot Oa]. intros y Iy. unfold provision_gens in Iy. cbn in Iy. rewrite app_nil_r in Iy.
    apply in_app_or in Iy as [Iy|Iy]; [exact (subst_within K s t Range Ot y Iy)|].
    apply in_flat_map in Iy as [[x [w|]] [Iq Iy]]; [exact (Oa x w Iq y Iy)|contradiction].
  - destruct Ok as [[Ps Rt] Oa]. intros y Iy. unfold provision_gens in Iy. cbn [provision_signature provision_inst] in Iy.
    apply in_app_or in Iy as [Iy|Iy].
    + cbn in Iy. apply in_app_or in Iy as [Iy|Iy]; [exact (subst_within K s _ Range Rt y Iy)|].
      apply in_flat_map in Iy as [t [It Iy]]. apply in_map_iff in It as [t0 [<- It]].
      rewrite Forall_forall in Ps. exact (subst_within K s t0 Range (Ps t0 It) y Iy).
    + apply in_flat_map in Iy as [[x [w|]] [Iq Iy]]; [exact (Oa x w Iq y Iy)|contradiction].
  - destruct Ok as [_ [[Ps Rt] Oa]]. intros y Iy. unfold provision_gens in Iy. cbn [provision_signature provision_inst] in Iy.
    apply in_app_or in Iy as [Iy|Iy].
    + cbn in Iy. apply in_app_or in Iy as [Iy|Iy]; [exact (subst_within K s _ Range Rt y Iy)|].
      apply in_flat_map in Iy as [t [It Iy]]. apply in_map_iff in It as [t0 [<- It]].
      rewrite Forall_forall in Ps. exact (subst_within K s t0 Range (Ps t0 It) y Iy).
    + apply in_flat_map in Iy as [[x [w|]] [Iq Iy]]; [exact (Oa x w Iq y Iy)|contradiction].
  - intros y Iy. unfold provision_gens in Iy. cbn in Iy. apply in_flat_map in Iy as [[x o] [Iq Iy]].
    unfold fixed_subst in Iq. rewrite map_map in Iq. apply in_map_iff in Iq as [[x' w] [E Iq]]. cbn in E. injection E as -> <-.
    cbn in Iy. exact (subst_within K s w Range (Ok _ w Iq) y Iy).
Qed.

(* A functor's result read in C0: each new provision mentions only K. *)
Lemma provide_result_gens : forall u C0 out C' ks (K : nat -> Prop),
  ProvideResult W u C0 out C' ks ->
  (forall g v, In (g, v) (c_sub C0) -> GensIn K (gens v)) ->
  (forall x v, In (x, v) (c_ty C0) -> GensIn K (gens (canon C0 v))) ->
  (forall t, FunOutputs W u t -> GensIn K (gens (canon C0 (out t)))) ->
  forall k p, In (k, p) (c_provisions C') -> In (k, p) (c_provisions C0) \/ GensIn K (provision_gens p).
Proof.
  intros u C0 out C' ks K (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & ->) SubK Ty Out.
  assert (Sc0 : GensIn K (tm_gens (output_type_map C0 outs out))).
  { intros g Ig. destruct (tm_gens_in _ g Ig) as [x [v [Iq Iv]]].
    unfold output_type_map in Iq. apply in_map_iff in Iq as [[y w] [E Iq]]. cbn in E. injection E as -> <-.
    apply in_app_or in Iq as [Iq|Iq].
    - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. exact (Out t (proj1 (Outs t) It) g Iv).
    - exact (Ty _ _ Iq g Iv). }
  pose proof (proj2 (output_entries_gens K) _ _ _ _ _ _ OIE ltac:(intros g []) Sc0) as Es.
  intros k p Iq. cbn in Iq. apply in_app_or in Iq as [Iq|Iq]; [right|left; exact Iq].
  apply in_flat_map in Iq as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie. rewrite Forall_forall in Es.
  exact (entry_provision_gens K _ e k p SubK (Es e Ie) Iq).
Qed.

(* A family applied in C at a matching scoped in C adds only provisions
   scoped in C. *)
Lemma family_application_gens : forall C u matching th sm Cr ks,
  FamilyApplication W C u matching th sm Cr ks ->
  (forall x v, In (x, v) matching -> ScopedTy W C (canon C v)) ->
  forall k p, In (k, p) (c_provisions Cr) -> In (k, p) (c_provisions C) \/ GensIn (GenScoped W C) (provision_gens p).
Proof.
  intros C u matching th sm Cr ks (fixed & formal & needs & Fx & Fam & Poly & FC & Args & InF & FA & Sat & PR & Nd) Mt.
  destruct (family_context_shape matching C) as [Ety [Esub [Epr Elo]]].
  intros k p Iq. rewrite <- Epr.
  apply (provide_result_gens _ _ _ _ _ (GenScoped W C) PR); [| | |exact Iq].
  - intros g v Iq' y Iy. rewrite Esub in Iq'. left. exact (scope_sub C g v y Iq' Iy).
  - intros x v Iq' y Iy. rewrite Ety in Iq'. unfold canon in Iy. rewrite Esub in Iy. apply in_app_or in Iq' as [Iq'|Iq'].
    + apply (proj2 (in_rev _ _)) in Iq'. exact (Mt x v Iq' y Iy).
    + left. exact (scope_ty_canon C x v y Iq' Iy).
  - intros t Ft. exfalso. exact (family_no_outputs W u t (proj1 Fam) Ft).
Qed.

(* ---- readings in a context ----------------------------------------------------- *)

Lemma Forall2_In_r : forall X Y (R : X -> Y -> Prop) l l' y, Forall2 R l l' -> In y l' -> exists x, In x l /\ R x y.
Proof.
  intros X Y R l l' y F. induction F as [|a b l l' Rab Rest IH]; intros Iy; [contradiction|].
  destruct Iy as [<-|Iy]; [exists a; split; [left; reflexivity|exact Rab]|].
  destruct (IH Iy) as [x [Ix Rx]]. exists x. split; [right; exact Ix|exact Rx].
Qed.

Lemma nom_reqs_scoped : forall C p s a bs reqs xs n,
  Brackets W p (c_ty C) s a bs ->
  Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) reqs xs ->
  ScopedTy W C (TNom n xs).
Proof.
  intros C p s a bs reqs xs n B Fx y Iy. rewrite gens_nom in Iy. destruct (tm_gens_in _ y Iy) as [x [u [Iq Iu]]].
  apply tm_scoped. destruct (Forall2_In_r _ _ _ _ _ _ Fx Iq) as [r [_ [_ L]]]. cbn in L.
  pose proof (nlook_gens _ _ _ _ L Iu) as I. apply tm_gens_app in I as [I|I]; [exact (br_gens W _ _ _ _ _ B y I)|exact I].
Qed.

Lemma tyof_scoped_ctx : forall C p this a t, TyOf W p (c_ty C) this a t -> OptScoped C this -> ScopedTy W C t.
Proof.
  intros C p this a t T Th y Iy. destruct (tyof_gens W _ _ _ _ _ T y Iy) as [I|[x [E Ix]]]; [exact (tm_scoped C y I)|exact (Th x E y Ix)].
Qed.

Lemma payload_gens : forall s xs pay y, PayloadOf W s xs pay -> In y (gens pay) -> In y (tm_gens xs).
Proof. intros s xs pay y [s' xs' d n ps t pay' D Ast T] Iy. exact (tyof_none_gens W _ _ _ _ T y Iy). Qed.

Lemma join_scoped : forall C a b exp t, Join a b exp t -> ScopedTy W C a -> ScopedTy W C b -> OptScoped C exp -> ScopedTy W C t.
Proof. intros C a b exp t J Sa Sb Se. destruct J; [exact (Se _ eq_refl)|exact Sb|exact Sa]. Qed.

Lemma branch_ty_scoped : forall C b t, ScopedTy W C t -> ScopedTy W C (branch_ty b t).
Proof. intros C [ss [y|]] t St; cbn; [exact St|intros y' []]. Qed.

Lemma never_scoped : forall C, ScopedTy W C never.
Proof. intros C y []. Qed.

Lemma unit_scoped : forall C, ScopedTy W C TUnit.
Proof. intros C y []. Qed.

Lemma fieldpats_in : forall F ftys fs bs xs, FieldPats W F ftys fs bs xs -> forall n t, In (n, t) bs -> exists f, In (f, t) ftys.
Proof.
  intros F ftys fs bs xs FP. induction FP as [|n t r bs xs S Nm FP IH|n t r bs xs S FP IH|n y t r bs xs S R Nm FP IH];
    intros n' t' Ib; [contradiction| |exact (IH n' t' Ib)|].
  - destruct Ib as [E|Ib]; [injection E as <- <-; exists n; exact (slook_in _ _ _ _ S)|exact (IH _ _ Ib)].
  - destruct Ib as [E|Ib]; [injection E as <- <-; exists n; exact (slook_in _ _ _ _ S)|exact (IH _ _ Ib)].
Qed.

Lemma patof_scoped : forall C F st pat xp binds hs ca, PatOf W F st pat xp binds hs ca -> ScopedTy W C st ->
  forall n t, In (n, t) binds -> ScopedTy W C t.
Proof.
  intros C F st pat xp binds hs ca Pt Sc.
  destruct Pt as [|n R Nm|pa s k xs R K Kk Im|pa s xs y pay R K Im Pay Ry Nm|pa s xs fs ftys bs fxs R K Im Pay FP Nd];
    intros n' t' Ib.
  - contradiction.
  - destruct Ib as [E|[]]. injection E as _ <-. exact Sc.
  - contradiction.
  - destruct Ib as [E|[]]. injection E as _ <-. intros z Iz. apply Sc. apply (member_gens _ _ _ Im).
    rewrite gens_nom. exact (payload_gens _ _ _ z Pay Iz).
  - destruct (fieldpats_in _ _ _ _ _ FP n' t' Ib) as [f If]. intros z Iz. apply Sc. apply (member_gens _ _ _ Im).
    rewrite gens_nom. apply (payload_gens _ _ _ z Pay). rewrite gens_record. apply in_flat_map. exists (f, t'). split; assumption.
Qed.

Lemma literal_scoped : forall C tr x t, Literal W C tr x t -> ProvsScoped C -> ScopedTy W C t.
Proof.
  intros C tr x t L Ps.
  induction L as [z t Lz Found|o t Lo Found|a b x y r t m La IHa Lb IHb Lm Op|a b x y r t m La IHa Lb IHb Lm Op
                 |a b x y r t m La IHa Lb IHb Lm Op|a x ti tp t f La IHa Lf Found Fits].
  - exact (val_scoped C _ _ Ps Found).
  - exact (val_scoped C _ _ Ps Found).
  - exact IHa.
  - exact IHa.
  - exact IHa.
  - exact (fn_ret_scoped C _ _ Ps Found).
Qed.

Lemma provide_tables : forall C k a C1, c_ty (provide C k a C1) = c_ty C1 /\ c_sub (provide C k a C1) = c_sub C1 /\
  c_locals (provide C k a C1) = c_locals C1.
Proof.
  intros C k a C1. unfold provide. destruct (ilook k (c_provisions C1)) as [[t a0|g a0|g a0|fx]|]; repeat split.
Qed.

Lemma provide_scope : forall C k a C1, scope_gens (provide C k a C1) = scope_gens C1.
Proof. intros C k a C1. destruct (provide_tables C k a C1) as [E1 [E2 _]]. exact (scope_gens_ext _ _ E1 E2). Qed.

Lemma set_ty_back : forall C t v, (forall y, In y (gens v) -> In y (scope_gens C)) ->
  forall y, In y (scope_gens (set_ty C t v)) -> In y (scope_gens C).
Proof.
  intros C t v Sv y Iy. unfold scope_gens in Iy. cbn [set_ty c_sub c_ty flat_map] in Iy.
  apply in_app_or in Iy as [Iy|Iy]; [unfold scope_gens; apply in_or_app; left; exact Iy|].
  apply in_app_or in Iy as [Iy|Iy]; [|unfold scope_gens; apply in_or_app; right; exact Iy].
  apply in_app_or in Iy as [Iy|Iy]; [exact (Sv y Iy)|].
  change (In y (gens (canon C v))) in Iy. destruct (gens_canon C v y Iy) as [I|I]; [exact (Sv y I)|exact I].
Qed.

(* `bind F;` for F abstract: the result's scope is the old one and the
   output destinations. *)
Lemma apply_abstract_scope : forall C u gensof C' ks, ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
  incl (scope_gens C) (scope_gens C') /\
  (forall y, In y (scope_gens C') -> In y (scope_gens C) \/ exists t, FunOutputs W u t /\ y = gensof t).
Proof.
  intros C u gensof C' ks PR.
  destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo [outs [OutsI Ety]]]].
  assert (InTy : forall x v, In (x, v) (output_type_map C outs (fun t => TAbs (gensof t))) ->
    In (x, app_subst (c_sub C') v) (c_ty C')).
  { intros x v Iq. rewrite Ety, Esub. apply in_map_iff. exists (x, v). split; [reflexivity|exact Iq]. }
  assert (OldIn : forall x v, In (x, v) (c_ty C) -> In (x, canon C v) (output_type_map C outs (fun t => TAbs (gensof t)))).
  { intros x v Iq. unfold output_type_map. apply in_map_iff. exists (x, v). split; [reflexivity|apply in_or_app; right; exact Iq]. }
  split.
  - intros y Iy. unfold scope_gens in Iy. apply in_app_or in Iy as [Iy|Iy].
    + apply in_flat_map in Iy as [[g v] [Iq Iy]]. rewrite <- Esub in Iq. destruct Iy as [E|Iy].
      * cbn in E. subst y. exact (scope_sub_key C' _ _ Iq).
      * exact (scope_sub C' g v y Iq Iy).
    + apply in_flat_map in Iy as [[x v] [Iq Iy]]. cbn in Iy.
      pose proof (table_scope C' x (canon C v) (InTy _ _ (OldIn _ _ Iq))) as T.
      apply in_app_or in Iy as [Iy|Iy]; [|exact (T y Iy)].
      destruct (lookup_subst y (c_sub C)) as [w|] eqn:L.
      * rewrite <- Esub in L. exact (scope_sub_key C' y w (lookup_subst_in_pair _ _ _ L)).
      * exact (T y (gens_keep _ _ _ Iy L)).
  - intros y Iy. unfold scope_gens in Iy. apply in_app_or in Iy as [Iy|Iy].
    + left. unfold scope_gens. apply in_or_app. left. rewrite Esub in Iy. exact Iy.
    + apply in_flat_map in Iy as [[x w] [Iq Iy]]. cbn [fst snd] in Iy. rewrite Ety in Iq.
      apply in_map_iff in Iq as [[x1 w1] [E Iq]]. cbn in E. injection E as -> <-.
      unfold output_type_map in Iq. apply in_map_iff in Iq as [[x2 w2] [E Iq]]. cbn in E. injection E as -> <-.
      assert (Back : forall z, In z (gens w2) -> In z (scope_gens C) \/ exists t, FunOutputs W u t /\ z = gensof t).
      { intros z Iz. apply in_app_or in Iq as [Iq|Iq].
        - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. cbn in Iz. destruct Iz as [<-|[]].
          right. exists t. split; [exact (proj1 (OutsI t) It)|reflexivity].
        - left. exact (scope_ty C _ _ z Iq Iz). }
      assert (Can : forall t z, In z (gens (canon C t)) -> (forall z, In z (gens t) -> In z (scope_gens C) \/ exists t, FunOutputs W u t /\ z = gensof t) ->
        In z (scope_gens C) \/ exists t, FunOutputs W u t /\ z = gensof t).
      { intros t z Iz Bt. destruct (gens_canon C t z Iz) as [I|I]; [exact (Bt z I)|left; exact I]. }
      unfold canon in Iy. rewrite Esub in Iy. fold (canon C) in Iy.
      apply in_app_or in Iy as [Iy|Iy].
      * apply (Can _ y Iy). intros z Iz. exact (Can _ z Iz Back).
      * apply (Can _ y Iy). intros z Iz. apply (Can _ z Iz). intros z' Iz'. exact (Can _ z' Iz' Back).
Qed.

End Scoping.

(* ---- elaboration keeps types scoped ---------------------------------------------- *)

Section Elab.
Variable W : A.world.
Variable Prog : program.
Import A.

Hypothesis units_bare : UnitsBare W.
Hypothesis family_sig_ok : FamilySigReadOffOk W.

(* What a run of bind items does to the scope: it grows, by at most the
   generations the items made. *)
Definition BindScoped (F : frame) (C C' : cx) (F' : frame) (made : list nat) : Prop :=
  CxScoped W F' C' /\ incl (scope_gens C) (scope_gens C') /\
  (forall y, In y (scope_gens C') -> In y (scope_gens C) \/ In y made) /\ fr_this F' = fr_this F.

Lemma ex_ok' : forall F C a x t, Ex W Prog F C a None x t -> FormedCx W C -> FrameFormed W F -> TyOk W t.
Proof. intros F C a x t D Fc Ff. exact (ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ D Fc Ff (optok_none W)). Qed.

Lemma optscoped_none : forall C, OptScoped W C None.
Proof. intros C t E. discriminate. Qed.

Fixpoint ex_scoped F C a exp x t (D : Ex W Prog F C a exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp -> ScopedTy W C t
with branch_scoped F C a exp x t (D : Branch W Prog F C a exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp -> ScopedTy W C t
with arms_scoped F C st arms exp xarms t (D : Arms W Prog F C st arms exp xarms t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W st -> ScopedTy W C st -> OptScoped W C exp -> ScopedTy W C t
with armslist_scoped F C st arms exp xarms hs ca t (D : ArmsList W Prog F C st arms exp xarms hs ca t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W st -> ScopedTy W C st -> OptScoped W C exp -> ScopedTy W C t
with methcall_scoped F C ox ot pa args x t (D : MethCall W Prog F C ox ot pa args x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W ot -> ScopedTy W C ot -> ScopedTy W C t
with provided_scoped F C ox ot m args x t (D : Provided W Prog F C ox ot m args x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W ot -> ScopedTy W C ot -> ScopedTy W C t
with blke_scoped F C b exp x t (D : BlkE W Prog F C b exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp -> ScopedTy W C t
with blk_scoped F C ss tl exp x t made (D : Blk W Prog F C ss tl exp x t made) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp ->
  forall y, In y (gens t) -> GenScoped W C y \/ In y made
with binditems_scoped F C its wrap C' F' made (D : BindItems W Prog F C its wrap C' F' made) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> BindScoped F C C' F' made
with binditem_scoped F C it wrap C' F' made (D : BindItem W Prog F C it wrap C' F' made) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> BindScoped F C C' F' made.
Proof.
  (* ---- Ex ---- *)
  - destruct D as [F C exp|F C exp t Th|F C exp n t mu L Nm|F C exp pa s Hl R K|F C exp pa s d n t0 t Hl R D' Ast L
                  |F C exp pa s d n t0 i t x Hl R D' Ast As T Ex0|F C exp pa a args s bs xs pay y v ty R K B Fx Pay Ea Ey Fi
                  |F C exp pa args f Cf ns sm th g xs wrap C' R K Def Ctx Dm Sat CT Sig As
                  |F C exp pa args f g xs R K Abs L As|F C exp o pa args ox ot x t Eo Mc
                  |F C exp op y lib n m ox ot x t Op Ls Ey Hd Pv|F C exp op l r lib n m ox ot x t Op Ls El Hd Pv
                  |F C exp text x t Nt Lt|F C exp c b cx' bx bt Cd Bb Of Bt|F C exp c b y cx' bx bt ex' et t Cd Bb Br Of J
                  |F C exp s arms sx st xarms t Es Ar|F C exp b x t Bb|F C exp Rt|F C exp e expected v actual Rt Ee Fi
                  |F C exp Lp|F C exp pa a fs s bs xs pay ftys xfs R K B Fx Pay Ep Nd Len F2
                  |F C exp o n ox ot rec t Eo Rc Ft];
      intros Fc Ff Cs Se; pose proof Cs as [Lc [Ps Ts]].
    + exact (unit_scoped W C).
    + exact (Ts t Th).
    + exact (Lc n t mu (slook_in _ _ _ _ L)).
    + intros y' [].
    + exact (val_scoped W C _ _ Ps L).
    + intros y' Iy. destruct (tyof_none_gens W _ _ _ _ T y' Iy).
    + exact (nom_reqs_scoped W C _ _ _ _ _ _ _ B Fx).
    + destruct (demand_shape W _ _ _ _ _ _ Dm) as [Ety _].
      intros y' Iy. destruct (sigof_gens W _ _ _ _ y' Sig (sig_ret_in g y' Iy)) as [I|[x0 [E _]]]; [|discriminate].
      rewrite Ety in I. exact (tm_scoped W C y' I).
    + exact (fn_ret_scoped W C _ _ Ps L).
    + pose proof (ex_scoped _ _ _ _ _ _ Eo Fc Ff Cs (optscoped_none C)) as So.
      pose proof (ex_ok' _ _ _ _ _ Eo Fc Ff) as Ot.
      exact (methcall_scoped _ _ _ _ _ _ _ _ Mc Fc Ff Cs (canon_ok W C ot (proj1 Fc) Ot) (canon_scoped W C ot So)).
    + pose proof (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs (optscoped_none C)) as So.
      pose proof (ex_ok' _ _ _ _ _ Ey Fc Ff) as Ot.
      exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs (canon_ok W C ot (proj1 Fc) Ot) (canon_scoped W C ot So)).
    + pose proof (ex_scoped _ _ _ _ _ _ El Fc Ff Cs (optscoped_none C)) as So.
      pose proof (ex_ok' _ _ _ _ _ El Fc Ff) as Ot.
      exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs (canon_ok W C ot (proj1 Fc) Ot) (canon_scoped W C ot So)).
    + exact (literal_scoped W C _ _ _ Lt Ps).
    + exact (unit_scoped W C).
    + pose proof (blke_scoped _ _ _ _ _ _ Bb Fc Ff Cs Se) as Hb. pose proof (branch_scoped _ _ _ _ _ _ Br Fc Ff Cs Se) as He.
      exact (join_scoped W C _ _ _ _ J (branch_ty_scoped W C b bt Hb) He Se).
    + pose proof (ex_scoped _ _ _ _ _ _ Es Fc Ff Cs (optscoped_none C)) as Hs.
      pose proof (ex_ok' _ _ _ _ _ Es Fc Ff) as Ot.
      exact (arms_scoped _ _ _ _ _ _ _ Ar Fc Ff Cs (canon_ok W C st (proj1 Fc) Ot) (canon_scoped W C st Hs) Se).
    + destruct exp as [e|]; [exact (Se e eq_refl)|exact (blke_scoped _ _ _ _ _ _ Bb Fc Ff Cs Se)].
    + exact (never_scoped W C).
    + exact (never_scoped W C).
    + exact (never_scoped W C).
    + exact (nom_reqs_scoped W C _ _ _ _ _ _ _ B Fx).
    + pose proof (ex_scoped _ _ _ _ _ _ Eo Fc Ff Cs (optscoped_none C)) as So.
      assert (Rs : ScopedTy W C rec).
      { destruct Rc as [(s & xs & -> & Kt & Pay)| ->]; [|exact So].
        intros y' Iy. apply So. rewrite gens_nom. exact (payload_gens W _ _ _ y' Pay Iy). }
      destruct Ft as [ftys [-> S]]. intros y' Iy. apply Rs. rewrite gens_record. apply in_flat_map.
      exists (n, t). split; [exact (slook_in _ _ _ _ S)|exact Iy].
  (* ---- Branch ---- *)
  - destruct D as [F C b exp x t Bb|F C y exp x t Nb Ey]; intros Fc Ff Cs Se.
    + destruct exp as [e|]; [exact (Se e eq_refl)|exact (branch_ty_scoped W C b t (blke_scoped _ _ _ _ _ _ Bb Fc Ff Cs Se))].
    + exact (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs Se).
  (* ---- Arms ---- *)
  - destruct D as [F C exp|F C st arms exp xarms covered catch t Nn Al Exh]; intros Fc Ff Cs Os Ss Se.
    + destruct exp as [e|]; [exact (Se e eq_refl)|exact (never_scoped W C)].
    + exact (armslist_scoped _ _ _ _ _ _ _ _ _ Al Fc Ff Cs Os Ss Se).
  (* ---- ArmsList ---- *)
  - destruct D as [F C st pat body exp xp binds hs ca bx bt Pt Br Of
                  |F C st pat body rest exp xp binds hs ca bx bt xr hr cr tr t Ne Pt Br Of Al J]; intros Fc Ff Cs Os Ss Se.
    + pose proof (add_locals_formed W LLet binds C Fc (patof_ok W _ _ _ _ _ _ _ Pt Os)) as Fb.
      pose proof (add_locals_scoped W F LLet binds C Cs (patof_scoped W C _ _ _ _ _ _ _ Pt Ss)) as Cb.
      pose proof (add_locals_scope LLet binds C) as Eb.
      apply (scoped_ext W _ _ _ Eb). apply (branch_scoped _ _ _ _ _ _ Br Fb Ff Cb).
      intros e E. exact (scoped_ext W _ _ _ (eq_sym Eb) (Se e E)).
    + pose proof (add_locals_formed W LLet binds C Fc (patof_ok W _ _ _ _ _ _ _ Pt Os)) as Fb.
      pose proof (add_locals_scoped W F LLet binds C Cs (patof_scoped W C _ _ _ _ _ _ _ Pt Ss)) as Cb.
      pose proof (add_locals_scope LLet binds C) as Eb.
      assert (Hb : ScopedTy W C bt).
      { apply (scoped_ext W _ _ _ Eb). apply (branch_scoped _ _ _ _ _ _ Br Fb Ff Cb).
        intros e E. exact (scoped_ext W _ _ _ (eq_sym Eb) (Se e E)). }
      pose proof (armslist_scoped _ _ _ _ _ _ _ _ _ Al Fc Ff Cs Os Ss Se) as Hr.
      exact (join_scoped W C _ _ _ _ J Hb Hr Se).
  (* ---- MethCall ---- *)
  - destruct D as [F C ox ot pa args m x t Len Rd Pv|F C ox ot n args h att m x t Hd At Sc Ml Pv
                  |F C ox ot n args h att Ca ns sm th g xs rargs wrap Cd Hd At Nw Def Ra Ctx Dm Sat CT Sig As
                  |F C ox ot n args h att x t Hd At Nw Abs Pv|F C ox ot n args m x t Na Sc Pv]; intros Fc Ff Cs Ot So.
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
    + destruct (demand_shape W _ _ _ _ _ _ Dm) as [Ety _].
      destruct (fold_set_ty_shape rargs C) as [Ety' _].
      intros y Iy. destruct (sigof_gens W _ _ _ _ y Sig (sig_ret_in g y Iy)) as [I|[x0 [E Ix]]]; [|injection E as <-; exact (So y Ix)].
      rewrite Ety, Ety' in I. apply tm_gens_app in I as [I|I]; [|exact (tm_scoped W C y I)].
      apply So. subst rargs. destruct (tm_gens_in _ y I) as [x0 [u [Iq Iu]]]. apply (proj2 (in_rev _ _)) in Iq.
      destruct ot; try contradiction. rewrite gens_nom. exact (in_tm_gens _ _ _ _ Iq Iu).
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
  (* ---- Provided ---- *)
  - destruct D as [F C ox ot m args g xs Ml As|F C ox ot m args u matching th sm C' ks g xs Ml FAp Ro Ik Of Ml' As];
      intros Fc Ff Cs Ot So; pose proof Cs as [Ls [Ps Ts]].
    + exact (mth_ret_scoped W C _ _ _ Ps Ml).
    + destruct (mths_in C' _ _ _ Ml') as [k [a Iq]].
      assert (Mt : forall x v, In (x, v) matching -> ScopedTy W C (canon C v))
        by (intros x v Iv; exact (proj2 (family_matching_scoped W _ _ _ _ _ Ro Ot So x v Iv))).
      intros y Iy. pose proof (sig_ret_gens (ProvidedMethod g a) g a eq_refl eq_refl y Iy) as Ip.
      destruct (family_application_gens W _ _ _ _ _ _ _ FAp Mt _ _ Iq) as [Iq'|G]; [exact (Ps _ _ Iq' y Ip)|exact (G y Ip)].
  (* ---- BlkE ---- *)
  - destruct D as [F C ss tl exp x t made Bk Esc Of]; intros Fc Ff Cs Se.
    intros y Iy. destruct (blk_scoped _ _ _ _ _ _ _ _ Bk Fc Ff Cs Se y Iy) as [G|Im]; [exact G|].
    exfalso. exact (Esc y Im Iy).
  (* ---- Blk ---- *)
  - destruct D as [F C y exp x t Ey|F C exp
                  |F C st n ann y rest tl exp declared v vt t0 xr t made mu Ls Ann Ey Dc Nm Rest
                  |F C n y rest tl exp tv v vt xr t made L Ey Fi Rest Nm|F C y exp v vt Ey
                  |F C y rest tl exp v vt xr t made Ne Ey Rest|F C c b rest tl exp cx' bx bt xr t made Cd Bb Rest
                  |F C b rest tl exp bx bt xr t made Ne Bb Rest|F C b exp bx bt Bb Nb
                  |F C its rest tl exp wrap C' F' xr t made made' Bi Rest]; intros Fc Ff Cs Se.
    + intros y' Iy. left. exact (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs Se y' Iy).
    + intros y' [].
    + assert (Od : OptOk W declared /\ OptScoped W C declared).
      { destruct ann as [a|].
        - destruct Ann as [d [T ->]]. split; [apply optok_some; exact (tyof_ok W _ _ _ _ _ T (cf_ty _ _ (proj1 Fc)) (proj2 Ff))|].
          intros d' E. injection E as <-. apply (tyof_scoped_ctx W C _ _ _ _ T). exact (proj2 (proj2 Cs)).
        - rewrite Ann. split; [exact (optok_none W)|exact (optscoped_none C)]. }
      destruct Od as [Od Sd].
      pose proof (ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ Ey Fc Ff Od) as Hvt.
      assert (Ht0 : TyOk W t0 /\ ScopedTy W C t0).
      { destruct declared as [d|]; [destruct Dc as [_ ->]; exact (conj (Od d eq_refl) (Sd d eq_refl))|].
        rewrite Dc. exact (conj Hvt (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs Sd)). }
      exact (blk_scoped _ _ _ _ _ _ _ _ Rest (add_local_formed W C n t0 mu Fc (proj1 Ht0)) Ff
        (add_local_scoped W F C n t0 mu Cs (proj2 Ht0)) Se).
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + destruct (ty_eqb vt never); intros y' [].
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + intros y' [].
    + destruct (binditems_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ _ Bi Fc Ff) as [Fc' Ff'].
      destruct (binditems_scoped _ _ _ _ _ _ _ Bi Fc Ff Cs) as [Cs' [Inc [Back _]]].
      intros y' Iy. destruct (blk_scoped _ _ _ _ _ _ _ _ Rest Fc' Ff' Cs' (fun e E => scoped_mono W _ _ _ Inc (Se e E)) y' Iy)
        as [[G|K]|Im].
      * destruct (Back y' G) as [I|I]; [left; left; exact I|right; apply in_or_app; left; exact I].
      * left. right. exact K.
      * right. apply in_or_app. right. exact Im.
  (* ---- BindItems ---- *)
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 B1 B2]; intros Fc Ff Cs.
    + split; [exact Cs|split; [intros y I; exact I|split; [intros y I; left; exact I|reflexivity]]].
    + destruct (binditem_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ _ B1 Fc Ff) as [Fc1 Ff1].
      destruct (binditem_scoped _ _ _ _ _ _ _ B1 Fc Ff Cs) as [Cs1 [Inc1 [Back1 Th1]]].
      destruct (binditems_scoped _ _ _ _ _ _ _ B2 Fc1 Ff1 Cs1) as [Cs2 [Inc2 [Back2 Th2]]].
      split; [exact Cs2|split; [intros y I; exact (Inc2 y (Inc1 y I))|split; [|rewrite Th2; exact Th1]]].
      intros y I. destruct (Back2 y I) as [I1|I1]; [destruct (Back1 y I1) as [I2|I2]; [left; exact I2|]|];
        right; apply in_or_app; [left|right]; assumption.
  (* ---- BindItem ---- *)
  - destruct D as [F C pa t rp ra tau R K Np T
                  |F C pa v d n t0 i declared yv x xt a R D' Ast T Ey Fi App
                  |F C pa f g sg pg sm a types R K Sig FP App
                  |F C pa f sg ps rt body pid pre sm a R K Sig LP App
                  |F C pa dn a yp r m bs rt sg target pre sm ap types R MN B T Sig MP App NoFam
                  |F C pa u d n ps a r bodies wrap C' F' made R D' Ast Na Bi
                  |F C pa u fx Cu ns sm th gensof C' ks newgens outs R Fx FC In' Th Fix Sat FF Inj Outs Nd OutsI PR
                  |F C pa u pg h Cu nsu Ch nsh sm fx
                  |F C pa u pg gg Cu nsu Cg nsg h sm smc fx]; intros Fc Ff Cs.
    + assert (Sv : forall y, In y (gens (canon C tau)) -> In y (scope_gens C)).
      { intros y Iy. destruct (gens_canon C tau y Iy) as [I|I]; [exact (ty_scope C y (tyof_none_gens W _ _ _ _ T y I))|exact I]. }
      split; [exact (set_ty_scoped W F C t _ Cs)|split; [exact (scope_set_ty C t _)|split; [|reflexivity]]].
      intros y I. left. exact (set_ty_back C t _ Sv y I).
    + split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      apply provide_scoped; [apply set_val_scoped; [exact Cs|exact (tyof_scoped_ctx W C _ _ _ _ T (optscoped_none C))]|].
      intros z Iz. exact (tm_scoped W C z (appof_scoped W _ _ _ App z Iz)).
    + split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      apply provide_scoped; [|intros y Iy; exact (tm_scoped W C y (appof_scoped W _ _ _ App y Iy))].
      apply (set_sig_scoped W F C (IFn f) sg Cs). intros y Iy.
      destruct (sigof_gens W _ _ _ _ y Sig Iy) as [I|[x0 [E _]]]; [exact (tm_scoped W C y I)|discriminate].
    + split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      apply provide_scoped; [|intros y Iy; exact (tm_scoped W C y (appof_scoped W _ _ _ App y Iy))].
      apply (set_sig_scoped W F C (IFn f) sg Cs). intros y Iy.
      destruct (sigof_gens W _ _ _ _ y Sig Iy) as [I|[x0 [E _]]]; [exact (tm_scoped W C y I)|discriminate].
    + assert (Full : forall y, In y (tm_gens (bs ++ c_ty C)) -> GenScoped W C y).
      { intros y Iy. apply tm_scoped. apply tm_gens_app in Iy as [Iy|Iy]; [exact (br_gens W _ _ _ _ _ B y Iy)|exact Iy]. }
      split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      apply provide_scoped; [|intros y Iy; exact (Full y (appof_scoped W _ _ _ App y Iy))].
      apply (set_sig_scoped W F C _ sg Cs). intros y Iy.
      destruct (sigof_gens W _ _ _ _ y Sig Iy) as [I|[x0 [E Ix]]]; [exact (Full y I)|].
      injection E as <-. exact (Full y (tyof_none_gens W _ _ _ _ T y Ix)).
    + destruct (binditems_scoped _ _ _ _ _ _ _ Bi Fc Ff Cs) as [[Ls' [Ps' Ts']] [Inc [Back Th]]].
      split; [split; [exact Ls'|split; [exact Ps'|]]|split; [exact Inc|split; [exact Back|reflexivity]]].
      intros t E. cbn in E. apply Ts'. rewrite Th. exact E.
    + destruct (apply_abstract_scope W _ _ _ _ _ PR) as [Inc Back].
      destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo [outs' [OutsI' Ety]]]].
      pose proof Cs as [Ls [Ps Ts]].
      assert (InTy : forall x v, In (x, v) (output_type_map C outs' (fun t => TAbs (gensof t))) ->
        In (x, app_subst (c_sub C') v) (c_ty C')).
      { intros x v Iq. rewrite Ety, Esub. apply in_map_iff. exists (x, v). split; [reflexivity|exact Iq]. }
      split; [split; [|split]|split; [exact Inc|split; [|reflexivity]]].
      * intros n t mu Iq. rewrite Elo in Iq. exact (scoped_mono W _ _ _ Inc (Ls n t mu Iq)).
      * intros k p Iq.
        destruct (provide_result_gens W _ _ _ _ _ (GenScoped W C') PR) with (k := k) (p := p) as [Iq'|G]; [| | |exact Iq| |exact G].
        -- intros g v Iv y Iy. left. rewrite <- Esub in Iv. exact (scope_sub C' g v y Iv Iy).
        -- intros x v Iv y Iy. left. apply (table_scope C' x (canon C v)); [|exact Iy]. apply InTy.
           unfold output_type_map. apply in_map_iff. exists (x, v). split; [reflexivity|apply in_or_app; right; exact Iv].
        -- intros t Ft y Iy. left. apply (table_scope C' t (canon C (TAbs (gensof t)))); [|exact Iy]. apply InTy.
           unfold output_type_map. apply in_map_iff. exists (t, TAbs (gensof t)). split; [reflexivity|].
           apply in_or_app; left. apply in_map_iff. exists t. split; [reflexivity|exact (proj2 (OutsI' t) Ft)].
        -- intros y Iy. exact (inscope_mono W _ _ y Inc (Ps k p Iq' y Iy)).
      * intros t E. exact (scoped_mono W _ _ _ Inc (Ts t E)).
      * intros y I. destruct (Back y I) as [I'|[t [Ft ->]]]; [left; exact I'|right].
        apply in_map. exact (proj2 (OutsI t) Ft).
    + match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        split; [apply set_fun_scoped; [exact Cs|intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq))]|] end.
      split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]].
    + match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        split; [apply set_fun_scoped; [exact Cs|intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq))]|] end.
      split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]].
Qed.

End Elab.
