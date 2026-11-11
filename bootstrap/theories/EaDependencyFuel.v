(* Local completeness facts needed when dependency readers are negated. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit AuditFacts EaBase EaTypes EaTypesSound EaMentionsSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
From NM Require Export EaDepth.
Local Open Scope list_scope.


Lemma largest_member : forall A (size : A -> nat) xs x,
  In x xs -> size x <= largest size xs.
Proof.
  intros A size xs x. induction xs as [|y xs IH]; cbn; [contradiction|].
  intros [<-|Member]; unfold largest; cbn [fold_right].
  - apply Nat.le_max_l.
  - eapply Nat.le_trans; [apply IH; exact Member|apply Nat.le_max_r].
Qed.

Section DepthBounds.
Variable W : world.

Theorem free_in_complete_above : forall R p syntax x,
  FreeIn W R p syntax x -> forall fuel, type_depth syntax <= fuel ->
  In x (free_in W fuel R p syntax).
Proof.
  intros R p syntax x Read. induction Read; intros [|fuel] Bound;
    cbn [type_depth] in Bound; try lia.
  - cbn [free_in]. apply in_flat_map. exists (n, t). split; [exact H|].
    apply IHRead. pose proof (largest_member _ (fun field => type_depth (snd field)) fs (n,t) H).
    cbn [snd] in H0. lia.
  - cbn [free_in]. apply in_flat_map. exists t. split; [exact H|].
    apply IHRead. pose proof (largest_member _ type_depth ms t H). lia.
  - cbn [free_in]. rewrite H,H0. apply in_or_app. left. apply filter_In.
    split; [left; reflexivity|]. apply negb_true_iff.
    destruct (nmem t (bracket_bound W p a)) eqn:Member; [|reflexivity].
    apply nmem_spec, bracket_bound_members in Member. contradiction.
  - cbn [free_in]. rewrite H.
    destruct (kind_sym W g) as [kind|] eqn:Kind; [destruct kind; try contradiction|].
    all: apply in_or_app; left; apply filter_In; split; [exact H1|].
    all: apply negb_true_iff; destruct (nmem x (bracket_bound W p a)) eqn:Member; [|reflexivity].
    all: apply nmem_spec, bracket_bound_members in Member; contradiction.
  - cbn [free_in]. destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|contradiction].
    apply in_or_app. right. apply in_flat_map. exists (bpath, Spec (Some sp) d sa).
    split; [exact H0|]. apply IHRead.
    pose proof (largest_member _ (fun b => spec_depth (snd b))
      (bracket_list a) (bpath, Spec (Some sp) d sa) H0) as Child.
    cbn [snd spec_depth] in Child. destruct sa; cbn [type_depth bracket_list] in Child |- *; try lia.
    change (1 <= fuel). lia.
Qed.

Theorem free_in_exact : forall R p syntax fuel,
  type_depth syntax <= fuel -> forall x,
  In x (free_in W fuel R p syntax) <-> FreeIn W R p syntax x.
Proof.
  intros R p syntax fuel Enough x. split.
  - apply free_in_sound.
  - intro Read. exact (free_in_complete_above R p syntax x Read fuel Enough).
Qed.

Definition signature_depth (symbol : nat) : nat :=
  match w_decl W symbol with
  | Some d => match d_ast d with
              | ADFn _ _ parameters result _ =>
                  Nat.max (largest (fun pt => type_depth (snd pt)) parameters)
                    (match result with Some syntax => type_depth syntax | None => 0 end)
              | _ => 0 end
  | None => 0
  end.

Theorem sig_mentions_complete_above : forall symbol x,
  SigMentions W symbol x -> forall fuel, signature_depth symbol <= fuel ->
  In x (sig_mentions W fuel symbol).
Proof.
  intros symbol x (d & receiver & name & parameters & result & body & Declaration & Ast & Read) fuel Bound.
  unfold signature_depth in Bound. rewrite Declaration, Ast in Bound.
  unfold sig_mentions. rewrite Declaration, Ast, sort_uniq_members.
  destruct Read as [(pt & Member & Read)|(syntax & Result & Read)].
  - apply in_or_app. left. apply in_flat_map. exists pt. split; [exact Member|].
    eapply free_in_complete_above; [exact Read|].
    pose proof (largest_member _ (fun pt => type_depth (snd pt)) parameters pt Member).
    pose proof (Nat.le_max_l (largest (fun pt => type_depth (snd pt)) parameters)
      (match result with Some syntax => type_depth syntax | None => 0 end)). lia.
  - rewrite Result in *. apply in_or_app. right.
    eapply free_in_complete_above; [exact Read|].
    pose proof (Nat.le_max_r (largest (fun pt => type_depth (snd pt)) parameters) (type_depth syntax)). lia.
Qed.

Theorem sig_mentions_exact : forall symbol fuel,
  signature_depth symbol <= fuel -> forall x,
  In x (sig_mentions W fuel symbol) <-> SigMentions W symbol x.
Proof.
  intros symbol fuel Enough x. split.
  - apply sig_mentions_sound.
  - intro Read. exact (sig_mentions_complete_above symbol x Read fuel Enough).
Qed.

End DepthBounds.

Lemma ascending_cons : forall x xs,
  Ascending xs -> (forall y, In y xs -> x < y) -> Ascending (x :: xs).
Proof.
  intros x [|y xs] Sorted Before; [exact I|]. split; [apply Before; left; reflexivity|exact Sorted].
Qed.

Lemma insert_sorted_ascending : forall x xs, Ascending xs -> Ascending (insert_sorted x xs).
Proof.
  intros x xs. induction xs as [|y xs IH]; intro Sorted; [exact I|].
  change (Ascending (if Nat.ltb x y then x :: y :: xs
    else if Nat.eqb x y then y :: xs else y :: insert_sorted x xs)).
  destruct (Nat.ltb x y) eqn:Before.
  - apply Nat.ltb_lt in Before. split; assumption.
  - destruct (Nat.eqb x y) eqn:Equal; [exact Sorted|].
    apply ascending_cons.
    + apply IH. exact (ascending_tail _ _ Sorted).
    + intros z Member. rewrite insert_sorted_members in Member.
      destruct Member as [->|Member].
      * apply Nat.ltb_ge in Before. apply Nat.eqb_neq in Equal. lia.
      * exact (ascending_lt _ _ Sorted z Member).
Qed.

Lemma sort_uniq_ascending : forall xs, Ascending (sort_uniq xs).
Proof.
  induction xs as [|x xs IH]; [exact I|]. unfold sort_uniq in *. cbn [fold_right].
  apply insert_sorted_ascending. exact IH.
Qed.

Lemma filter_ascending : forall test xs, Ascending xs -> Ascending (filter test xs).
Proof.
  intros test xs. induction xs as [|x xs IH]; intro Sorted; [exact I|].
  cbn [filter]. destruct (test x).
  - apply ascending_cons.
    + apply IH. exact (ascending_tail _ _ Sorted).
    + intros y Member. apply filter_In in Member as [Member _].
      exact (ascending_lt _ _ Sorted y Member).
  - apply IH. exact (ascending_tail _ _ Sorted).
Qed.

Lemma nmem_false_spec : forall x xs, nmem x xs = false <-> ~ In x xs.
Proof.
  intros x xs. split.
  - intros Absent Present. apply nmem_spec in Present. congruence.
  - intro Absent. destruct (nmem x xs) eqn:Member; [|reflexivity].
    exfalso. apply Absent, nmem_spec. exact Member.
Qed.

Lemma select_filter : forall A (test : A -> bool) (P : A -> Prop),
  (forall x, test x = true <-> P x) -> forall xs, Select P xs (filter test xs).
Proof.
  intros A test P Exact xs. induction xs as [|x xs IH]; [constructor|].
  cbn [filter]. destruct (test x) eqn:Checked.
  - apply SelectKeep; [apply Exact; exact Checked|exact IH].
  - apply SelectDrop; [|exact IH]. intro Holds. apply Exact in Holds. congruence.
Qed.

Section ApplicationReaders.
Variable W : world.

Theorem app_of_sound : forall fuel symbol tm,
  signature_depth W symbol <= fuel -> AppOf W symbol tm (app_of W fuel symbol tm).
Proof.
  intros fuel symbol tm Enough. exists (app_requirements W fuel symbol).
  split; [reflexivity|]. unfold app_requirements.
  destruct (kind_is W symbol DKMeth) eqn:Method.
  - left. split; [apply kind_is_spec; exact Method|].
    exists (filter (fun r => nmem r (sig_mentions W fuel symbol)) (w_reqs W symbol)),
      (filter (fun t => negb (nmem t (w_reqs W symbol))) (sig_mentions W fuel symbol)).
    split.
    + apply select_filter. intro x. rewrite nmem_spec.
      exact (sig_mentions_exact W symbol fuel Enough x).
    + split; [|reflexivity]. split.
      * apply filter_ascending. unfold sig_mentions.
        destruct (w_decl W symbol) as [d|]; [destruct (d_ast d)|]; try exact I.
        apply sort_uniq_ascending.
      * intro x. rewrite filter_In, negb_true_iff, nmem_false_spec.
        rewrite (sig_mentions_exact W symbol fuel Enough x). reflexivity.
  - right. split; [|reflexivity]. intro IsMethod.
    apply kind_is_spec in IsMethod. congruence.
Qed.

End ApplicationReaders.

Lemma largest_le : forall A (size : A -> nat) xs bound,
  (forall x, In x xs -> size x <= bound) -> largest size xs <= bound.
Proof.
  intros A size xs. induction xs as [|x xs IH]; intros bound Bounded.
  - change (0 <= bound). lia.
  - change (Nat.max (size x) (largest size xs) <= bound).
    apply Nat.max_lub; [apply Bounded; left; reflexivity|].
    apply IH. intros y Member. apply Bounded. right. exact Member.
Qed.

Lemma rmap_member_read : forall A B (f : A -> er B) xs ys,
  rmap f xs = Ok ys -> forall x, In x xs -> exists y, f x = Ok y.
Proof.
  intros A B f xs. induction xs as [|x xs IH]; intros ys Run y Member; [contradiction|].
  cbn [rmap] in Run. apply rbind_ok in Run as [value [Head Run]].
  apply rbind_ok in Run as [tail [Tail Run]].
  destruct Member as [<-|Member]; [exists value; exact Head|exact (IH _ Tail _ Member)].
Qed.

Lemma spec_depth_ref : forall pa detached application,
  spec_depth (Spec (Some pa) detached application) = type_depth (ATRef pa application).
Proof. intros pa detached [bindings|]; reflexivity. Qed.

Section SuccessfulReaderFuel.
Variable W : world.

Theorem type_readers_depth : forall fuel,
  (forall p tm this syntax meaning, ty_of W fuel p tm this syntax = Ok meaning ->
    type_depth syntax <= fuel) /\
  (forall p tm symbol syntax bindings, brackets W fuel p tm symbol syntax = Ok bindings ->
    largest (fun b => spec_depth (snd b)) (bracket_list syntax) <= fuel).
Proof.
  induction fuel as [|fuel [TypeDepth BracketDepth]].
  - split; intros; discriminate.
  - split.
    + intros p tm this syntax meaning Run.
      destruct syntax as [| | |fields|alternatives|pa application]; try (cbn [type_depth]; lia).
      * cbn [ty_of] in Run. destruct (snodupb (map fst fields)); [|discriminate].
        cbn [rbind] in Run. apply rbind_ok in Run as [types [Fields Run]].
        change (S (largest (fun field => type_depth (snd field)) fields) <= S fuel).
        apply le_n_S, largest_le. intros [label syntax] Member.
        destruct (rmap_member_read _ _ _ _ _ Fields _ Member) as [field_type Field].
        cbn [fst snd] in Field. apply rbind_ok in Field as [t [Read Field]].
        exact (TypeDepth _ _ _ _ _ Read).
      * cbn [ty_of] in Run. apply rbind_ok in Run as [types [Types Run]].
        change (S (largest type_depth alternatives) <= S fuel).
        apply le_n_S, largest_le. intros syntax Member.
        destruct (rmap_member_read _ _ _ _ _ Types _ Member) as [t Read].
        exact (TypeDepth _ _ _ _ _ Read).
      * cbn [ty_of] in Run.
        destruct (resolve_sym W p pa) as [symbol|]; [|discriminate]. cbn [rbind] in Run.
        destruct (kind_sym W symbol) as [kind|]; [destruct kind|]; try discriminate.
        -- destruct application; [discriminate|]. change (1 <= S fuel). lia.
        -- apply rbind_ok in Run as [bindings [Read Run]].
           apply le_n_S. exact (BracketDepth _ _ _ _ _ Read).
        -- apply rbind_ok in Run as [d [Declaration Run]].
           destruct (d_ast d) as [items declarations|name|name body|name parameters payload|name|name value_type initial|
             receiver name parameters result body|name items|name parameters arguments results body]; try discriminate.
           apply rbind_ok in Run as [bindings [Read Run]].
           apply le_n_S. exact (BracketDepth _ _ _ _ _ Read).
        -- apply rbind_ok in Run as [bindings [Read Run]].
           apply le_n_S. exact (BracketDepth _ _ _ _ _ Read).
    + intros p tm symbol syntax bindings Run. destruct syntax as [source|].
      * cbn [brackets] in Run. apply rbind_ok in Run as [values [Bindings Run]].
        unfold bracket_list. apply largest_le. intros [pa binding] Member.
        destruct (rmap_member_read _ _ _ _ _ Bindings _ Member) as [value Read].
        destruct binding as [[sp|] [name|] application]; try discriminate.
        cbn [fst snd] in Read.
        destruct (resolve_sym W p pa); [|discriminate]. cbn [rbind] in Read.
        destruct (kind_is W n DKType); [|discriminate]. cbn [rbind] in Read.
        destruct (kind_is W symbol DKMeth || nmem n (w_reqs W symbol)); [|discriminate].
        cbn [rbind] in Read. apply rbind_ok in Read as [meaning [Meaning Read]].
        cbn [snd]. rewrite spec_depth_ref. pose proof (TypeDepth _ _ _ _ _ Meaning). lia.
      * change (0 <= S fuel). lia.
Qed.

Corollary ty_of_depth : forall fuel p tm this syntax meaning,
  ty_of W fuel p tm this syntax = Ok meaning -> type_depth syntax <= fuel.
Proof. intros fuel. exact (proj1 (type_readers_depth fuel)). Qed.

Theorem sig_of_depth : forall fuel symbol tm this signature,
  sig_of W fuel symbol tm this = Ok signature -> signature_depth W symbol <= fuel.
Proof.
  intros fuel symbol tm this signature Run. unfold sig_of in Run.
  apply rbind_ok in Run as [d [Declaration Run]].
  destruct (d_ast d) as [items declarations|name|name body|name parameters payload|name|name value_type initial|
    receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast; try discriminate.
  apply rbind_ok in Run as [parameter_types [Parameters Run]].
  apply rbind_ok in Run as [result_type [Result Run]].
  unfold signature_depth. rewrite (decl_of_sound W _ _ Declaration), Ast.
  apply Nat.max_lub.
  - apply largest_le. intros pt Member.
    destruct (rmap_member_read _ _ _ _ _ Parameters _ Member) as [t Read].
    exact (ty_of_depth _ _ _ _ _ _ Read).
  - destruct result as [syntax|]; [exact (ty_of_depth _ _ _ _ _ _ Result)|lia].
Qed.

Theorem free_in_exact_after_type_read : forall fuel p tm this syntax meaning,
  ty_of W fuel p tm this syntax = Ok meaning -> forall R x,
  In x (free_in W fuel R p syntax) <-> FreeIn W R p syntax x.
Proof.
  intros fuel p tm this syntax meaning Read R x.
  apply free_in_exact. exact (ty_of_depth _ _ _ _ _ _ Read).
Qed.

Theorem app_of_sound_after_signature_read : forall fuel symbol tm this signature,
  sig_of W fuel symbol tm this = Ok signature -> AppOf W symbol tm (app_of W fuel symbol tm).
Proof.
  intros fuel symbol tm this signature Read. apply app_of_sound.
  exact (sig_of_depth _ _ _ _ _ Read).
Qed.

End SuccessfulReaderFuel.
