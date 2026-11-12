(* The executable source-type readers satisfy Audit.v's judgments. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma rbind_ok : forall A B (r : er A) (f : A -> er B) b,
  rbind r f = Ok b -> exists a, r = Ok a /\ f a = Ok b.
Proof.
  intros A B [a|message] f b Run; [exists a; split; [reflexivity|exact Run]|discriminate].
Qed.

Lemma rmap_sound : forall A B (f : A -> er B) (R : A -> B -> Prop),
  (forall a b, f a = Ok b -> R a b) ->
  forall xs ys, rmap f xs = Ok ys -> Forall2 R xs ys.
Proof.
  intros A B f R Sound xs. induction xs as [|x xs IH]; intros ys Run.
  - injection Run as <-. constructor.
  - cbn [rmap] in Run. apply rbind_ok in Run as [y [First Run]].
    apply rbind_ok in Run as [rest [Tail Run]]. injection Run as <-.
    constructor; [exact (Sound x y First)|exact (IH rest Tail)].
Qed.

Lemma smem_spec : forall x xs, smem x xs = true <-> In x xs.
Proof.
  intros x xs. induction xs as [|y xs IH]; cbn; [split; [discriminate|contradiction]|].
  rewrite orb_true_iff, String.eqb_eq, IH. intuition congruence.
Qed.

Lemma snodupb_sound : forall xs, snodupb xs = true -> NoDup xs.
Proof.
  induction xs as [|x xs IH]; intro Checked; [constructor|].
  cbn in Checked. apply andb_true_iff in Checked as [Absent Checked].
  constructor; [|exact (IH Checked)]. intro Present.
  apply smem_spec in Present. rewrite Present in Absent. discriminate.
Qed.

Lemma nmem_spec : forall x xs, nmem x xs = true <-> In x xs.
Proof.
  intros x xs. induction xs as [|y xs IH]; cbn; [split; [discriminate|contradiction]|].
  rewrite orb_true_iff, Nat.eqb_eq, IH. intuition congruence.
Qed.

Lemma kind_is_spec : forall W s k, kind_is W s k = true <-> kind_sym W s = Some k.
Proof.
  intros W s k. unfold kind_is. destruct (kind_sym W s) as [actual|]; [destruct actual, k|destruct k];
    cbn; split; congruence.
Qed.

Section Readers.
Variable W : world.

Definition nominal (t : ty) : Prop := exists n xs, t = TNom n xs.

Lemma nominal_check : forall ts,
  find (fun t => match t with TNom _ _ => false | _ => true end) ts = None ->
  Forall nominal ts.
Proof.
  intros ts Checked. apply Forall_forall. intros t Present.
  pose proof (find_none _ _ Checked t Present) as IsNominal.
  destruct t; try discriminate. eexists; eexists; reflexivity.
Qed.

Lemma head_collision : forall x y, nominal x -> nominal y -> head x = head y ->
  (match head x, head y with Some a, Some b => Nat.eqb a b | _, _ => false end) = true.
Proof.
  intros x y (n & xs & ->) (m & ys & ->) Equal.
  cbn in Equal |- *. injection Equal as ->. apply Nat.eqb_refl.
Qed.

Lemma union_heads_sound : forall xs seen,
  Forall nominal xs -> Forall nominal seen ->
  union_heads W xs seen = Ok tt ->
  NoDup (map head xs) /\
    (forall x y, In x xs -> In y seen -> head x <> head y).
Proof.
  induction xs as [|x xs IH]; intros seen Nominal NominalSeen Run.
  - split; [constructor|intros x y []].
  - inversion Nominal as [|x0 xs0 NominalX NominalXs]; subst.
    cbn [union_heads] in Run.
    destruct (find (fun y => match head x, head y with
      Some a, Some b => Nat.eqb a b | _, _ => false end) seen) eqn:Collision; [discriminate|].
    destruct (IH (seen ++ [x]) NominalXs) as [Distinct Apart].
    { apply Forall_app. split; [exact NominalSeen|constructor; [exact NominalX|constructor]]. }
    { exact Run. }
    assert (NotSeen : forall y, In y seen -> head x <> head y).
    { intros y Present Equal.
      pose proof (find_none _ _ Collision y Present) as NoCollision.
      change ((match head x, head y with Some a, Some b => Nat.eqb a b | _, _ => false end) = false) in NoCollision.
      rewrite (head_collision x y NominalX (proj1 (Forall_forall nominal seen) NominalSeen y Present) Equal) in NoCollision.
      discriminate. }
    split.
    + constructor; [|exact Distinct]. intro Present.
      apply in_map_iff in Present as [y [Equal Present]].
      apply (Apart y x Present); [apply in_or_app; right; left; reflexivity|exact Equal].
    + intros y z [<-|Present] Seen; [exact (NotSeen z Seen)|].
      exact (Apart y z Present (in_or_app _ _ _ (or_introl Seen))).
Qed.

Lemma decl_of_sound : forall symbol d,
  decl_of W symbol = Ok d -> w_decl W symbol = Some d.
Proof.
  intros symbol d Run. unfold decl_of in Run.
  destruct (w_decl W symbol) as [found|] eqn:Lookup; [injection Run as <-; reflexivity|discriminate].
Qed.

Theorem type_readers_sound : forall fuel,
  (forall p tm this syntax t, ty_of W fuel p tm this syntax = Ok t ->
    TyOf W p tm this syntax t) /\
  (forall p tm symbol syntax bindings, brackets W fuel p tm symbol syntax = Ok bindings ->
    Brackets W p tm symbol syntax bindings).
Proof.
  induction fuel as [|fuel [TypeSound BracketSound]].
  - split; intros; discriminate.
  - split.
    + intros p tm this syntax t Run. destruct syntax as [| | |fields|alternatives|pa application].
      * injection Run as <-. apply TyNever.
      * cbn [ty_of] in Run. destruct this as [receiver|]; [injection Run as <-; apply TyThis|discriminate].
      * injection Run as <-. apply TyUnit.
      * cbn [ty_of] in Run.
        destruct (snodupb (map fst fields)) eqn:Distinct; [|discriminate].
        cbn [rbind] in Run. apply rbind_ok in Run as [types [Fields Run]].
        injection Run as <-. apply TyRec; [exact (snodupb_sound _ Distinct)|].
        eapply rmap_sound; [|exact Fields].
        intros [label syntax] result Field. cbn [fst snd] in Field.
        apply rbind_ok in Field as [field_type [Read Field]].
        injection Field as <-. split; [reflexivity|exact (TypeSound p tm this syntax field_type Read)].
      * cbn [ty_of] in Run. apply rbind_ok in Run as [types [Types Run]].
        destruct (find (fun member => match member with TNom _ _ => false | _ => true end)
          (flat_map members types)) as [bad|] eqn:Nominal.
        { destruct bad; discriminate. }
        cbn [rbind] in Run. apply rbind_ok in Run as [checked [Heads Run]].
        destruct checked. injection Run as <-.
        pose proof (nominal_check _ Nominal) as NominalMembers.
        eapply TyUnion; [|reflexivity|exact NominalMembers|].
        -- eapply rmap_sound; [exact (TypeSound p tm this)|exact Types].
        -- exact (proj1 (union_heads_sound _ [] NominalMembers (Forall_nil _) Heads)).
      * cbn [ty_of] in Run.
        destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|discriminate].
        cbn [rbind] in Run.
        destruct (kind_sym W symbol) as [kind|] eqn:Kind; [|discriminate].
        destruct kind; try discriminate.
        -- destruct application as [parameters|]; [discriminate|].
           destruct (nlook symbol tm) as [meaning|] eqn:Lookup; [|discriminate].
           injection Run as <-. eapply TyType; eassumption.
        -- apply rbind_ok in Run as [d [Declaration Run]].
           destruct (d_ast d) as [items declarations|name|name body|name parameters arity payload|name|name value_type initial|
             receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
             try discriminate.
           apply rbind_ok in Run as [bindings [Brackets Run]].
           eapply TyAlias; [exact Resolve|exact (decl_of_sound _ _ Declaration)|exact Ast|
             exact (BracketSound _ _ _ _ _ Brackets)|exact (TypeSound _ _ _ _ _ Run)].
        -- apply rbind_ok in Run as [bindings [Brackets Run]].
           apply rbind_ok in Run as [arguments [Arguments Run]]. injection Run as <-.
           eapply TyNom; [exact Resolve|exact Kind|exact (BracketSound _ _ _ _ _ Brackets)|].
           eapply rmap_sound; [|exact Arguments].
           intros requirement argument Read. cbn [rbind] in Read.
           destruct (nlook requirement (bindings ++ tm)) as [meaning|] eqn:Lookup; [|discriminate].
           injection Read as <-. split; reflexivity.
    + intros p tm symbol syntax bindings Run. destruct syntax as [source|].
      * cbn [brackets] in Run. apply rbind_ok in Run as [values [Bindings Run]].
        injection Run as <-. apply BrSome. eapply rmap_sound; [|exact Bindings].
        intros [pa binding] value Read.
        destruct binding as [[sp|] [name|] application]; try discriminate.
        cbn [fst snd] in Read.
        destruct (resolve_sym W p pa) as [parameter|] eqn:Resolve; [|discriminate].
        cbn [rbind] in Read.
        destruct (kind_is W parameter DKType) eqn:Kind; [|discriminate].
        cbn [rbind] in Read.
        destruct (kind_is W symbol DKMeth || nmem parameter (w_reqs W symbol)) eqn:ParameterChecked; [|discriminate].
        cbn [rbind] in Read. apply rbind_ok in Read as [meaning [Meaning Read]].
        injection Read as <-. exists sp, application. split; [reflexivity|].
        split; [exact Resolve|]. split; [apply kind_is_spec; exact Kind|]. split.
        -- intro NotMethod. apply orb_true_iff in ParameterChecked as [Method|Member].
           ++ apply kind_is_spec in Method. contradiction.
           ++ apply nmem_spec. exact Member.
        -- exact (TypeSound _ _ _ _ _ Meaning).
      * injection Run as <-. apply BrNone.
Qed.

Corollary ty_of_sound : forall fuel p tm this syntax t,
  ty_of W fuel p tm this syntax = Ok t -> TyOf W p tm this syntax t.
Proof. intros fuel. exact (proj1 (type_readers_sound fuel)). Qed.

Corollary brackets_sound : forall fuel p tm symbol syntax bindings,
  brackets W fuel p tm symbol syntax = Ok bindings ->
  Brackets W p tm symbol syntax bindings.
Proof. intros fuel. exact (proj2 (type_readers_sound fuel)). Qed.

Theorem sig_of_sound : forall fuel symbol tm this signature,
  sig_of W fuel symbol tm this = Ok signature -> SigOf W symbol tm this signature.
Proof.
  intros fuel symbol tm this signature Run. unfold sig_of in Run.
  apply rbind_ok in Run as [d [Declaration Run]].
  destruct (d_ast d) as [items declarations|name|name body|name parameters arity payload|name|name value_type initial|
    receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
    try discriminate.
  apply rbind_ok in Run as [parameter_types [Parameters Run]].
  apply rbind_ok in Run as [result_type [Result Run]]. injection Run as <-.
  exists d, receiver, name, parameters, result, body. split; [exact (decl_of_sound _ _ Declaration)|].
  split; [exact Ast|]. split.
  - eapply rmap_sound; [|exact Parameters].
    intros [parameter syntax] t Read. exact (ty_of_sound _ _ _ _ _ _ Read).
  - destruct result as [syntax|]; [exact (ty_of_sound _ _ _ _ _ _ Result)|].
    injection Result as <-. reflexivity.
Qed.


Local Open Scope string_scope.

Theorem lang_sym_sound : forall short ns name symbol,
  lang_sym W short ns name = Some symbol -> LangSym W short ns name symbol.
Proof.
  intros short ns name symbol Run. unfold lang_sym in Run.
  destruct (w_library W) as [directory|] eqn:Library; [|discriminate].
  destruct (w_scope W (directory ++ (short ++ ".moss")) ns name) as [[found|module_path]|] eqn:Scope;
    try discriminate.
  destruct (w_decl W found) as [d|] eqn:Declaration; [|discriminate].
  injection Run as <-. exists d, (directory ++ (short ++ ".moss")).
  split; [exact Declaration|]. split; [unfold LibraryModule; rewrite Library; reflexivity|exact Scope].
Qed.

Theorem lang_item_sym_sound : forall item symbol,
  lang_item_sym W item = Some symbol -> LangItem W item symbol.
Proof.
  intros item symbol. unfold lang_item_sym, LangItem.
  destruct (lang_item_key item) as [[short ns] name]. exact (lang_sym_sound short ns name symbol).
Qed.

Theorem method_named_sound : forall p receiver name method,
  method_named W p receiver name = Some method -> MethodNamed W p receiver name method.
Proof.
  intros p receiver name method Run. unfold method_named in Run.
  destruct (w_attached W receiver name) as [attached|] eqn:AttachedMethod.
  - injection Run as <-. left. exact AttachedMethod.
  - right. split.
    + intros other Found. unfold Attached in Found. rewrite AttachedMethod in Found. discriminate.
    + destruct (w_scope W p NSDetached name) as [[found|module_path]|] eqn:Scope;
        try discriminate.
      injection Run as <-. reflexivity.
Qed.

Lemma bare_tagb_spec : forall symbol, bare_tagb W symbol = true <-> BareTag W symbol.
Proof.
  intros symbol. unfold bare_tagb, BareTag.
  destruct (ast_of W symbol) as [[]|]; split; try (intros [? H]; discriminate);
    try (intro H; discriminate).
  - eauto.
  - reflexivity.
Qed.

Theorem payload_of_sound : forall fuel symbol arguments payload,
  payload_of W fuel symbol arguments = Ok payload -> PayloadOf W symbol arguments payload.
Proof.
  intros fuel symbol arguments payload Run. unfold payload_of in Run.
  apply rbind_ok in Run as [d [Declaration Run]].
  destruct (d_ast d) as [| | |name params arity syntax|name| | | |] eqn:Ast; try discriminate.
  - apply rbind_ok in Run as [checked [Checked Run]].
    eapply PayTag; [exact (decl_of_sound _ _ Declaration)|rewrite Ast; reflexivity|].
    exact (ty_of_sound _ _ _ _ _ _ Run).
  - injection Run as <-.
    eapply PayTag; [exact (decl_of_sound _ _ Declaration)|rewrite Ast; reflexivity|constructor].
Qed.

Theorem recv_ty_sound : forall fuel receiver tm t,
  recv_ty W fuel receiver tm = Ok t -> RecvTy W receiver tm t.
Proof.
  intros fuel receiver tm t Run. unfold recv_ty, RecvTy in *.
  destruct (nlook receiver tm) as [meaning|] eqn:Lookup.
  - injection Run as <-. reflexivity.
  - assert (Arguments : forall arguments,
      rmap (fun x => let! meaning := ropt (nlook x tm)
        ("type " ++ bq (sym_name W x) ++ " (required by " ++ bq (sym_name W receiver) ++
          ") is not available in the context here") in Ok (x, meaning)) (w_reqs W receiver) = Ok arguments ->
      Forall2 (fun x argument => fst argument = x /\ nlook x tm = Some (snd argument))
        (w_reqs W receiver) arguments).
    { intros arguments Read. eapply rmap_sound; [|exact Read].
      intros x argument Item. cbn [rbind] in Item.
      destruct (nlook x tm) as [meaning|] eqn:Meaning; [|discriminate].
      injection Item as <-. split; reflexivity. }
    destruct (w_decl W receiver) as [d|] eqn:Declaration.
    + destruct (d_ast d) as [items declarations|name|name body|name parameters arity syntax|name|name value_type initial|
        rcvr name parameters result body|name items|name parameters inputs outputs body] eqn:Ast.
      all: try solve [exact (ty_of_sound _ _ _ _ _ _ Run)].
      all: destruct (kind_is W receiver DKType) eqn:Kind; [discriminate|].
      all: apply rbind_ok in Run as [arguments [Read Run]]; injection Run as <-;
        split; [|exists arguments; split; [reflexivity|exact (Arguments _ Read)]].
      all: intro IsType; assert (kind_is W receiver DKType = true) by (apply kind_is_spec; exact IsType);
        congruence.
    + apply rbind_ok in Run as [arguments [Read Run]]. injection Run as <-.
      exists arguments. split; [reflexivity|exact (Arguments _ Read)].
Qed.

End Readers.
