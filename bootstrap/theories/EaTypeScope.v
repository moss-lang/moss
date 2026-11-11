(* A successful type read uses only the source names in its requirements.
   Narrowing the context to those names keeps the result and the fuel. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaTypesSound.
From NM Require PublicTypeRead.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Local Lemma rmap_preserve_read : forall A B (f g : A -> er B) xs ys,
  (forall x y, In x xs -> f x = Ok y -> g x = Ok y) ->
  rmap f xs = Ok ys -> rmap g xs = Ok ys.
Proof.
  intros A B f g xs. induction xs as [|x xs IH]; intros ys Preserve Run; [exact Run|].
  cbn [rmap] in Run |- *.
  apply rbind_ok in Run as (y & Head & Run).
  apply rbind_ok in Run as (tail & Tail & Run).
  rewrite (Preserve _ _ (or_introl eq_refl) Head).
  cbn [rbind]. rewrite (IH _ (fun a b Member => Preserve a b (or_intror Member)) Tail).
  exact Run.
Qed.

Local Lemma nlook_key_present : forall A (rows : list (nat * A)) key,
  In key (map fst rows) -> nlook key rows <> None.
Proof.
  intros A rows. induction rows as [|[k v] rows IH]; intros key Present; [contradiction|].
  cbn in Present |- *. destruct (Nat.eqb_spec key k) as [->|Different]; [discriminate|].
  apply IH. destruct Present as [Equal|Present]; [congruence|exact Present].
Qed.

Local Lemma bracket_resolves_binding : forall W p tm g application rows,
  Brackets W p tm g application rows ->
  forall b key, In b (bracket_list application) -> resolve_sym W p (fst b) = Some key ->
  nlook key rows <> None.
Proof.
  intros W p tm g application rows Read b key Present Resolved.
  destruct Read as [p tm g|p tm g source rows Read]; [contradiction|].
  apply nlook_key_present. rewrite map_rev. apply (proj1 (in_rev _ _)).
  cbn [bracket_list] in Present.
  induction Read as [|bp row source rows (sp & args & Shape & Key & Kind & Param & Value) Read IH];
    [contradiction|].
  destruct Present as [<-|Present]; [cbn; left; congruence|right; exact (IH Present)].
Qed.

Local Lemma bracket_narrow_lookup : forall W p tm g application rows tm' pa key,
  Brackets W p tm g application rows ->
  resolve_sym W p pa = Some g -> kind_sym W g <> Some DKType ->
  (forall x, FreeIn W (w_reqs W) p (ATRef pa application) x -> nlook x tm = nlook x tm') ->
  In key (w_reqs W g) -> nlook key (rows ++ tm) = nlook key (rows ++ tm').
Proof.
  intros W p tm g application rows tm' pa key Read Resolved Kind Agree Required.
  rewrite !PublicTypeRead.nlook_app'. destruct (nlook key rows) eqn:Lookup; [reflexivity|].
  apply Agree. eapply FNom; try eassumption.
  intros (b & Present & Key).
  exact (bracket_resolves_binding _ _ _ _ _ _ Read _ _ Present Key Lookup).
Qed.

Section Reader.
Variable W : world.
Hypothesis Relevance : RelevanceClosed W (w_reqs W).

Definition ReadsSame (p : string) (syntax : aty) (tm tm' : subst) : Prop :=
  forall key, FreeIn W (w_reqs W) p syntax key -> nlook key tm = nlook key tm'.

Local Lemma nominal_arguments_narrow : forall p tm tm' pa application g rows args,
  resolve_sym W p pa = Some g -> kind_sym W g <> Some DKType ->
  Brackets W p tm g application rows -> ReadsSame p (ATRef pa application) tm tm' ->
  rmap (fun r => let! t := ropt (nlook r (rows ++ tm))
    ("type " ++ bq (sym_name W r) ++ " (required by " ++ bq (sym_name W g) ++
      ") is not available in the context here") in Ok (r, t)) (w_reqs W g) = Ok args ->
  rmap (fun r => let! t := ropt (nlook r (rows ++ tm'))
    ("type " ++ bq (sym_name W r) ++ " (required by " ++ bq (sym_name W g) ++
      ") is not available in the context here") in Ok (r, t)) (w_reqs W g) = Ok args.
Proof.
  intros p tm tm' pa application g rows args Resolved Kind Read Agree Run.
  eapply rmap_preserve_read; [|exact Run].
  intros key value Required Result.
  rewrite <- (bracket_narrow_lookup _ _ _ _ _ _ _ _ _ Read Resolved Kind Agree Required).
  exact Result.
Qed.

Theorem type_readers_narrow : forall fuel,
  (forall p tm tm' this syntax meaning, ReadsSame p syntax tm tm' ->
    ty_of W fuel p tm this syntax = Ok meaning -> ty_of W fuel p tm' this syntax = Ok meaning) /\
  (forall p tm tm' g pa application rows,
    resolve_sym W p pa = Some g -> ReadsSame p (ATRef pa application) tm tm' ->
    brackets W fuel p tm g application = Ok rows ->
    brackets W fuel p tm' g application = Ok rows).
Proof.
  induction fuel as [|fuel [TypeRead BracketRead]]; [split; intros; discriminate|].
  split.
  - intros p tm tm' this syntax meaning Agree Run.
    destruct syntax as [| | |fields|alternatives|pa application]; try exact Run.
    + cbn [ty_of] in Run |- *. destruct (snodupb (map fst fields)); [|discriminate].
      cbn [rbind] in Run |- *. apply rbind_ok in Run as (types & Fields & Run).
      assert (Narrow : rmap (fun nt => let! t := ty_of W fuel p tm' this (snd nt) in
          Ok (fst nt, t)) fields = Ok types).
      { eapply rmap_preserve_read; [|exact Fields].
        intros [label syntax] out Present Step. cbn [fst snd] in Step |- *.
        apply rbind_ok in Step as (t & Read & Step).
        rewrite (TypeRead p tm tm' this syntax t); [exact Step| |exact Read].
        intros key Free. apply Agree. eapply FRec; eassumption. }
      now rewrite Narrow.
    + cbn [ty_of] in Run |- *. apply rbind_ok in Run as (types & Types & Run).
      assert (Narrow : rmap (ty_of W fuel p tm' this) alternatives = Ok types).
      { eapply rmap_preserve_read; [|exact Types].
        intros syntax t Present Read. eapply TypeRead; [|exact Read].
        intros key Free. apply Agree. eapply FUnion; eassumption. }
      now rewrite Narrow.
    + cbn [ty_of] in Run |- *.
      destruct (resolve_sym W p pa) as [g|] eqn:Resolved; [|discriminate].
      cbn [rbind] in Run |- *.
      destruct (kind_sym W g) as [kind|] eqn:Kind; [destruct kind|]; try discriminate.
      * destruct application; [discriminate|].
        rewrite <- (Agree g); [exact Run|].
        eapply FType; [exact Resolved|exact Kind|intros (b & [] & _)].
      * apply rbind_ok in Run as (rows & Bindings & Run).
        rewrite (BracketRead _ _ _ _ _ _ _ Resolved Agree Bindings). cbn [rbind].
        apply rbind_ok in Run as (args & Arguments & Run).
        rewrite (nominal_arguments_narrow _ _ _ _ _ _ _ _ Resolved ltac:(congruence)
          (brackets_sound _ _ _ _ _ _ _ Bindings) Agree Arguments). exact Run.
      * apply rbind_ok in Run as (d & Declaration & Run).
        rewrite Declaration. cbn [rbind].
        destruct (d_ast d) as [items declarations|n|n body|n params payload|n|n value init|
          receiver n params result body|n items|n params inputs outputs body] eqn:Ast;
          try discriminate.
        apply rbind_ok in Run as (rows & Bindings & Run).
        rewrite (BracketRead _ _ _ _ _ _ _ Resolved Agree Bindings). cbn [rbind].
        eapply TypeRead; [|exact Run]. intros key Free.
        eapply bracket_narrow_lookup; try eassumption.
        -- eapply brackets_sound. exact Bindings.
        -- congruence.
        -- apply (proj2 (Relevance g d [] body (decl_of_sound _ _ _ Declaration)
             ltac:(unfold nominal_body; now rewrite Ast))). exact Free.
      * apply rbind_ok in Run as (rows & Bindings & Run).
        rewrite (BracketRead _ _ _ _ _ _ _ Resolved Agree Bindings). cbn [rbind].
        apply rbind_ok in Run as (args & Arguments & Run).
        rewrite (nominal_arguments_narrow _ _ _ _ _ _ _ _ Resolved ltac:(congruence)
          (brackets_sound _ _ _ _ _ _ _ Bindings) Agree Arguments). exact Run.
  - intros p tm tm' g pa application rows Resolved Agree Run.
    destruct application as [source|]; [|exact Run].
    cbn [brackets] in Run |- *. apply rbind_ok in Run as (values & Bindings & Run).
    lazymatch goal with
    | |- context [rmap ?read source] => assert (rmap read source = Ok values) as Narrow
    end.
    {
      eapply rmap_preserve_read; [|exact Bindings].
      intros [bp [[sp|] [detached|] sa]] value Present Step; try discriminate.
      cbn [fst snd] in Step |- *.
      destruct (resolve_sym W p bp) as [target|]; [|discriminate].
      cbn [rbind] in Step |- *.
      destruct (kind_is W target DKType); [|discriminate]. cbn [rbind] in Step |- *.
      destruct (kind_is W g DKMeth || nmem target (w_reqs W g)); [|discriminate].
      cbn [rbind] in Step |- *. apply rbind_ok in Step as (t & Read & Step).
      rewrite (TypeRead p tm tm' None (ATRef sp sa) t); [exact Step| |exact Read].
      intros key Free. apply Agree.
      eapply FBracket; [congruence|exact Present|exact Free].
    }
    rewrite Narrow. exact Run.
Qed.
End Reader.
