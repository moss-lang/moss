(* The run fact for a linked body (realize2).

   - [app_env_meaning]: the body's environment reads its table using
     application meanings for shared inputs and lexical capture for the
     private inputs.
   - [body_meaning_kept]: a run of binds changes no type meaning outside
     its destinations (PublicChainCore.bindings_writes).
   - With PublicLinkPins.link_dests_out (no destination is an input of
     F, by LinkBody's made clause), the body's final meanings of F's
     inputs are the application's. *)
From Stdlib Require Import List Arith Bool.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicCallRealize PublicCopyFresh PublicCopySubst.
From NM Require PublicChainCore PublicLinkPins PublicFamilyAgree.
From NM Require Import PublicContextFacts.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Lemma lookup_map_key_nat : forall (f : nat -> ty) l x, In x l -> lookup_subst x (map (fun s => (s, f s)) l) = Some (f x).
Proof.
  intros f l x. induction l as [|a r IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [reflexivity|]. destruct I as [E|I]; [congruence|exact (IH I)].
Qed.

Lemma lookup_map_snd : forall (f : ty -> ty) (s : subst) x,
  lookup_subst x (map (fun q => (fst q, f (snd q))) s) = option_map f (lookup_subst x s).
Proof.
  intros f s x. induction s as [|[a t] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb x a); [reflexivity|exact IH].
Qed.

Lemma lookup_subst_app' : forall x (l1 l2 : subst),
  lookup_subst x (l1 ++ l2) = match lookup_subst x l1 with Some v => Some v | None => lookup_subst x l2 end.
Proof. intros x l1 l2. rewrite !lookup_subst_nlook', nlook_app'. reflexivity. Qed.

Definition caller_readings (d : environment) (th : subst) : subst :=
  (map (fun q => (fst q, ground d (snd q))) th ++ type_meanings d)%list.

Lemma app_env_meaning : forall P d h Cu Cg outs binds sources th dc x tx,
  view_functor P h = Some (FunctorCode (map fst (A.c_ty Cg))
    (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg)))
    (A.c_ty Cg) outs binds sources) ->
  In x (map fst (A.c_ty Cg)) -> A.nlook x (A.c_ty Cg) = Some tx ->
  (forall y, In y (map fst (A.c_ty Cu)) -> lookup_subst y th <> None) ->
  incl (A.gens tx) (map fst (A.c_ty Cg)) ->
  lookup_subst x (type_meanings (application_environment P d h th dc)) =
    Some (app_subst (link_reading Cu Cg (caller_readings d th) dc) tx).
Proof.
  intros P d h Cu Cg outs binds sources th dc x tx Body Member Lookup Complete Scope.
  unfold application_environment. rewrite Body. cbn [type_meanings fc_inputs fc_linked fc_table].
  rewrite lookup_map_key_nat by exact Member. rewrite lookup_subst_nlook', Lookup. f_equal.
  apply subst_agree_on. intros y Occurs. cbn [app_subst].
  rewrite lookup_map_key_nat by exact (Scope y Occurs).
  set (linked := filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))).
  destruct (in_dec Nat.eq_dec y (map fst (A.c_ty Cu))) as [Shared|Private].
  - rewrite (link_reading_shared Cu Cg _ dc y Shared (Scope y Occurs)).
    assert (InLinked : In y linked).
    { apply filter_In. split; [exact (Scope y Occurs)|].
      apply existsb_exists. exists y. split; [exact Shared|apply Nat.eqb_refl]. }
    assert (ReadLinked : existsb (Nat.eqb y) linked = true).
    { apply existsb_exists. exists y. split; [exact InLinked|apply Nat.eqb_refl]. }
    rewrite ReadLinked. unfold caller_readings. cbn [app_subst].
    rewrite lookup_subst_app', lookup_map_snd. destruct (lookup_subst y th) eqn:Argument; [reflexivity|].
    exfalso. exact (Complete y Shared Argument).
  - rewrite (link_reading_private Cu Cg _ dc y Private (Scope y Occurs)).
    destruct (existsb (Nat.eqb y) linked) eqn:Read; [|reflexivity].
    apply existsb_exists in Read as [z [InLinked Equal]]. apply Nat.eqb_eq in Equal. subst z.
    apply filter_In in InLinked as [_ Present]. apply existsb_exists in Present as [z [InCu Equal]].
    apply Nat.eqb_eq in Equal. subst z. contradiction.
Qed.

(* Q6: reading a pinned fixed input through the pins. *)
Lemma lookup_pin_reads_none : forall Cb pins x, ~ In x (map fst pins) -> lookup_subst x (A.pin_reads Cb pins) = None.
Proof.
  intros Cb pins x. induction pins as [|[y t] rest IH]; intros N; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [exfalso; apply N; left; reflexivity|].
  apply IH. intros I. apply N. right. exact I.
Qed.
Lemma lookup_pin_reads_some : forall Cb pins x v, lookup_subst x (A.pin_reads Cb pins) = Some v ->
  exists tx, In (x, tx) pins /\ v = A.canon Cb tx.
Proof.
  intros Cb pins x v. induction pins as [|[y t] rest IH]; intros L; [discriminate|]. cbn in L.
  destruct (Nat.eqb_spec x y) as [->|Ne].
  - injection L as <-. exists t. split; [left; reflexivity|reflexivity].
  - destruct (IH L) as [tx [I E]]. exists tx. split; [right; exact I|exact E].
Qed.
Lemma pin_key_in : forall (pins : subst) x tx, In (x, tx) pins -> In x (map fst pins).
Proof. intros pins x tx I. change x with (fst (x, tx)). apply in_map. exact I. Qed.
Lemma lookup_pin_reads_in : forall Cb pins x, In x (map fst pins) -> exists v, lookup_subst x (A.pin_reads Cb pins) = Some v.
Proof.
  intros Cb pins x. induction pins as [|[y t] rest IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [eauto|]. destruct I as [E|I]; [cbn in E; congruence|exact (IH I)].
Qed.

(* #73: a pinned input reads at its pin in link_site's table. *)
Lemma lookup_subst_out : forall x (s : subst), ~ In x (map fst s) -> lookup_subst x s = None.
Proof.
  intros x s. induction s as [|[y v] s IH]; intros N; [reflexivity|]. cbn.
  destruct (Nat.eqb x y) eqn:E; [apply Nat.eqb_eq in E; subst y; exfalso; exact (N (or_introl eq_refl))|].
  apply IH. intros I. exact (N (or_intror I)).
Qed.
Lemma lookup_subst_dom : forall x (s : subst), In x (map fst s) -> exists v, lookup_subst x s = Some v /\ In (x, v) s.
Proof.
  intros x s I. destruct (lookup_subst x s) as [v|] eqn:L.
  - exists v. split; [reflexivity|exact (lookup_subst_in_pair _ _ _ L)].
  - exfalso. induction s as [|[y v] s IH]; [contradiction|]. cbn in L, I.
    destruct (Nat.eqb x y) eqn:E; [discriminate|]. destruct I as [<-|I]; [rewrite Nat.eqb_refl in E; discriminate|exact (IH I L)].
Qed.

Lemma existsb_eqb_in : forall y l, In y l -> existsb (Nat.eqb y) l = true.
Proof. intros y l I. apply existsb_exists. exists y. split; [exact I|apply Nat.eqb_refl]. Qed.
Lemma existsb_eqb_out : forall y l, ~ In y l -> existsb (Nat.eqb y) l = false.
Proof.
  intros y l N. destruct (existsb (Nat.eqb y) l) eqn:E; [|reflexivity]. exfalso.
  apply existsb_exists in E as [z [Iz E]]. apply Nat.eqb_eq in E. subst z. exact (N Iz).
Qed.

Lemma cmeaning_append_provisions : forall selected captured,
  type_meanings selected = [] -> type_aliases selected = [] -> forall x,
  cmeaning (append_environment selected captured) x = cmeaning captured x.
Proof.
  intros selected captured Meanings Aliases x. unfold cmeaning, output_meaning.
  cbn [type_aliases type_meanings append_environment]. now rewrite Meanings, Aliases.
Qed.

Module LinkRun (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module CC := PublicChainCore.ChainCore Prims I0.

(* A run of binds keeps every meaning outside its destinations. *)
Lemma body_meaning_kept : forall j Pv d g st bs d' g' st',
  CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j Pv d g st bs = inl (d', g', st') ->
  forall x, ~ In x (CC.binding_dests bs) -> lookup_subst x (type_meanings d') = lookup_subst x (type_meanings d).
Proof.
  intros j Pv d g st bs d' g' st' E x Nx.
  destruct (CC.bindings_writes j Pv d g st bs d' g' st' E) as [[w [Ew Iw]] _].
  rewrite Ew, lookup_subst_app'. destruct (lookup_subst x w) as [v|] eqn:L; [|reflexivity].
  exfalso. apply Nx, Iw. change x with (fst (x, v)). apply in_map. exact (lookup_subst_in_pair _ _ _ L).
Qed.

(* With LinkBody's made clause: every input of F keeps its meaning. *)
Lemma body_meaning_input : forall W Prog (Cu : A.cx) Fg Cg its wrap Cb Fb made binds,
  (forall t, In t made -> A.nlook t (A.c_ty Cu) = None) ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  forall j Pv d g st d' g' st', CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j Pv d g st binds = inl (d', g', st') ->
  forall x, A.nlook x (A.c_ty Cu) <> None ->
  lookup_subst x (type_meanings d') = lookup_subst x (type_meanings d).
Proof.
  intros W Prog Cu Fg Cg its wrap Cb Fb made binds Out D B j Pv d g st d' g' st' E x Nx.
  apply (body_meaning_kept j Pv d g st binds d' g' st' E). intros Ix.
  exact (Nx (PublicLinkPins.link_dests_out W Prog Cu Fg Cg its wrap Cb Fb made binds Out D B x Ix)).
Qed.

(* Q6: an input y of G that is its own merge class and that the body's
   binds do not make keeps, through the body, the meaning the application
   environment gives it: θ's reading when F links it, else the closure's. *)
Lemma link_own_meaning : forall W Prog (Cu : A.cx) gg Cg nsg h Fg its wrap Cb Fb made binds Dr srcs,
  A.FunCtx W gg Cg nsg ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  A.funbody_of Prog h = Some (FunctorCode (map fst (A.c_ty Cg))
      (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))) (A.c_ty Cg) Dr binds srcs) ->
  forall (th : subst) argv read j d dc g st d3 g3 st3,
  CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j (program_of Prog argv read) (application_environment (program_of Prog argv read) d h th dc) g st binds
    = inl (d3, g3, st3) ->
  forall y, In y (map fst (A.c_ty Cg)) -> lookup_subst y (A.c_sub Cg) = None -> ~ In y made ->
  lookup_subst y (type_meanings d3) = Some
    (match (if existsb (Nat.eqb y) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg)))
            then lookup_subst y th else None) with
     | Some meaning => ground d meaning
     | None => match output_meaning dc y with Some meaning => meaning | None => TAbs y end
     end).
Proof.
  intros W Prog Cu gg Cg nsg h Fg its wrap Cb Fb made binds Dr srcs FCg D B FB th argv read j d dc g st d3 g3 st3 Run y Ig Sy Nm.
  rewrite (body_meaning_kept j _ _ g st binds d3 g3 st3 Run y).
  2: { intros Iy. exact (Nm (PublicLinkPins.items_dests_made W Prog _ _ _ _ _ _ _ binds D B y Iy)). }
  assert (V : view_functor (program_of Prog argv read) h = A.funbody_of Prog h) by reflexivity. rewrite FB in V.
  unfold application_environment. rewrite V. cbn [type_meanings fc_inputs fc_linked fc_table].
  rewrite lookup_map_key_nat by exact Ig.
  destruct (PublicCallRealize.functx_scoped W gg Cg nsg FCg) as [TblG _].
  destruct (A.nlook y (A.c_ty Cg)) as [ty|] eqn:Ly; [|exfalso; exact (proj2 (PublicFamilyAgree.nlook_dom _ y _) Ig Ly)].
  rewrite lookup_subst_nlook', Ly. f_equal.
  rewrite (TblG y ty Ly). cbn [app_subst]. rewrite Sy. cbn [app_subst].
  rewrite lookup_map_key_nat by exact Ig. reflexivity.
Qed.

(* The run fact (realize3, extracted from linknewpf's family_link_new_ok):
   at an application of F at θ, a linked body's final meaning of an input x
   of both G and F is the caller's reading of x, θ read at the caller. It
   uses G's type over F's inputs at θ, θ defined on those inputs, and
   LinkBody's made clause, which keeps F's inputs through the body. *)
Lemma link_input_meaning : forall W Prog (Cu : A.cx) gg Cg nsg h Fg its wrap Cb Fb made binds Dr srcs,
  A.FunCtx W gg Cg nsg ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  (forall t, In t made -> A.nlook t (A.c_ty Cu) = None) ->
  A.funbody_of Prog h = Some (FunctorCode (map fst (A.c_ty Cg))
      (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))) (A.c_ty Cg) Dr binds srcs) ->
  forall (th : subst), (forall z, In z (map fst (A.c_ty Cu)) -> lookup_subst z th <> None) ->
  forall argv read j d dc g st d3 g3 st3,
  LinkedInputsRead Cu Cg (fun t => ground d (app_subst th t))
    (app_subst (link_reading Cu Cg (caller_readings d th) dc)) ->
  CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j (program_of Prog argv read) (application_environment (program_of Prog argv read) d h th dc) g st binds
    = inl (d3, g3, st3) ->
  forall x, In x (map fst (A.c_ty Cg)) -> In x (map fst (A.c_ty Cu)) ->
  lookup_subst x (type_meanings d3) = Some (ground d (app_subst th (TAbs x))).
Proof.
  intros W Prog Cu gg Cg nsg h Fg its wrap Cb Fb made binds Dr srcs FCg D B Out FB th ThD
    argv read j d dc g st d3 g3 st3 Inputs Run x Ig Iu.
  pose proof (proj2 (PublicFamilyAgree.nlook_dom _ x _) Iu) as Nu.
  rewrite (body_meaning_input W Prog Cu Fg Cg its wrap Cb Fb made binds Out D B j _ _ g st d3 g3 st3 Run x Nu).
  destruct (A.nlook x (A.c_ty Cg)) as [tx|] eqn:Lx; [|exfalso; exact (proj2 (PublicFamilyAgree.nlook_dom _ x _) Ig Lx)].
  destruct (PublicCallRealize.functx_scoped W gg Cg nsg FCg) as [TblG [_ [_ [_ [_ [_ SubG]]]]]].
  assert (GgDom : incl (A.gens tx) (map fst (A.c_ty Cg))).
  { intros z Iz. rewrite (TblG x tx Lx) in Iz. cbn [app_subst] in Iz.
    destruct (lookup_subst x (A.c_sub Cg)) as [v|] eqn:Lv.
    - exact (proj2 (SubG x v (lookup_subst_in_pair _ _ _ Lv)) z Iz).
    - destruct Iz as [<-|[]]. exact Ig. }
  assert (V : view_functor (program_of Prog argv read) h = A.funbody_of Prog h) by reflexivity. rewrite FB in V.
  rewrite (app_env_meaning _ d h Cu Cg _ _ _ th dc x tx V Ig Lx ThD GgDom).
  f_equal. symmetry. exact (Inputs x tx Lx Nu).
Qed.

(* So a type over inputs of both G and F grounds in the body's final
   meanings as its θ reading grounds in the caller's. *)
Corollary link_run_fact : forall W Prog (Cu : A.cx) gg Cg nsg h Fg its wrap Cb Fb made binds Dr srcs,
  A.FunCtx W gg Cg nsg ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  (forall t, In t made -> A.nlook t (A.c_ty Cu) = None) ->
  A.funbody_of Prog h = Some (FunctorCode (map fst (A.c_ty Cg))
      (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))) (A.c_ty Cg) Dr binds srcs) ->
  forall (th : subst), (forall z, In z (map fst (A.c_ty Cu)) -> lookup_subst z th <> None) ->
  forall argv read j d dc g st d3 g3 st3,
  LinkedInputsRead Cu Cg (fun t => ground d (app_subst th t))
    (app_subst (link_reading Cu Cg (caller_readings d th) dc)) ->
  CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j (program_of Prog argv read) (application_environment (program_of Prog argv read) d h th dc) g st binds
    = inl (d3, g3, st3) ->
  forall t, incl (A.gens t) (map fst (A.c_ty Cg)) -> incl (A.gens t) (map fst (A.c_ty Cu)) ->
  ground d3 t = ground d (app_subst th t).
Proof.
  intros W Prog Cu gg Cg nsg h Fg its wrap Cb Fb made binds Dr srcs FCg D B Out FB th ThD
    argv read j d dc g st d3 g3 st3 Inputs Run t Ig Iu.
  unfold ground. rewrite <- (PublicTypeRead.app_subst_nil (app_subst (type_meanings d3) t)).
  apply subst2_agree. intros z Iz. rewrite PublicTypeRead.app_subst_nil.
  change (app_subst (type_meanings d3) (TAbs z)) with (match lookup_subst z (type_meanings d3) with Some t => t | None => TAbs z end).
  rewrite (link_input_meaning W Prog Cu gg Cg nsg h Fg its wrap Cb Fb made binds Dr srcs FCg D B Out FB th ThD
    argv read j d dc g st d3 g3 st3 Inputs Run z (Ig z Iz) (Iu z Iz)).
  reflexivity.
Qed.

End LinkRun.
