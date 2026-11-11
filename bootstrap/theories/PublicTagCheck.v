(* Checked tag construction from a typed payload, under the platform's
   layouts and cast representations (T24's premises). A cast tag (Ref or
   Array) reads its payload at its raw-handle requirement, so the runtime
   guard reads a complete argument, and an enabled guard makes the payload
   a handle type. Unit declarations have no cast entry. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import AuditBase.
From NM Require Import Audit PublicTypeRead PublicTypeFacts PublicValueTyping
  PublicCastTyping PublicBoolTyping PublicBodySafety.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Lemma machine_table_lookup : forall W name mach x, Platform.Layouts W -> In (name, mach) Platform.initial_machines ->
  A.LangSym W "wasm" A.NSName name x -> A.nlook x (Platform.machine_table W) = Some mach.
Proof.
  intros W name mach x [_ [Uniq _]] Inm Lang.
  pose proof Lang as (d & p & Decl & Lib & Scope).
  unfold A.LibraryModule in Lib. unfold Platform.machine_table.
  destruct (A.w_library W) as [dir|] eqn:Ld; [|contradiction]. subst p.
  change (String.append dir (String.append "wasm" ".moss")) with (String.append dir "wasm.moss") in Scope.
  assert (Abs : forall n' m', In (n', m') Platform.initial_machines -> In n' Platform.abstract_names)
    by (intros n' m' I; cbn in I; repeat (destruct I as [E|I]; [injection E as <- _; cbn; tauto|]); contradiction).
  assert (Names : NoDup (map fst Platform.initial_machines)) by (cbn; repeat constructor; cbn; intuition discriminate).
  assert (Gen : forall l, incl l Platform.initial_machines -> NoDup (map fst l) -> In (name, mach) l ->
    A.nlook x (flat_map (fun q => match A.w_scope W (String.append dir "wasm.moss") A.NSName (fst q) with
      | Some (A.ESym symbol) => match A.w_decl W symbol with Some _ => [(symbol, snd q)] | None => [] end
      | _ => [] end) l) = Some mach).
  { induction l as [|[n' m'] rest IH]; intros Sub Nd Il; [contradiction|].
    inversion Nd as [|? ? Out Nd']; subst. cbn [flat_map fst snd].
    destruct (A.w_scope W (String.append dir "wasm.moss") A.NSName n') as [[s'|t']|] eqn:E.
    - destruct (A.w_decl W s') eqn:D.
      2: { destruct Il as [Eq|Il].
           - injection Eq as -> ->. rewrite Scope in E. injection E as <-. congruence.
           - apply IH; [intros q Iq; apply Sub; right; exact Iq|exact Nd'|exact Il]. }
      cbn [app A.nlook]. destruct (Nat.eqb_spec x s') as [->|Ne].
      + assert (L' : A.LangSym W "wasm" A.NSName n' s') by (exists d, (String.append dir "wasm.moss");
          split; [exact Decl|split; [unfold A.LibraryModule; rewrite Ld; reflexivity|exact E]]).
        destruct Il as [Eq|Il]; [injection Eq as -> ->; reflexivity|].
        exfalso. apply Out. replace n' with name; [change name with (fst (name, mach)); apply in_map, Il|].
        exact (Uniq name n' s' (Abs name mach (Sub _ (or_intror Il))) (Abs n' m' (Sub _ (or_introl eq_refl))) Lang L').
      + destruct Il as [Eq|Il]; [injection Eq as -> ->; congruence|].
        apply IH; [intros q Iq; apply Sub; right; exact Iq|exact Nd'|exact Il].
    - cbn [app]. destruct Il as [Eq|Il]; [injection Eq as -> ->; congruence|].
      apply IH; [intros q Iq; apply Sub; right; exact Iq|exact Nd'|exact Il].
    - cbn [app]. destruct Il as [Eq|Il]; [injection Eq as -> ->; congruence|].
      apply IH; [intros q Iq; apply Sub; right; exact Iq|exact Nd'|exact Il]. }
  apply Gen; [intros q Iq; exact Iq|exact Names|exact Inm].
Qed.

Lemma machine_lookup_in : forall (machines : list (tsym * machine_class)) symbol class,
  A.nlook symbol machines = Some class -> In symbol (map fst machines).
Proof.
  intros machines symbol class Lookup.
  change symbol with (fst (symbol, class)). apply in_map, nlook_in. exact Lookup.
Qed.

Lemma lookup_machine_subst : forall machines symbol,
  In symbol (map fst machines) -> lookup_subst symbol (A.machine_subst machines) = Some (TMachine symbol).
Proof.
  induction machines as [|[key class] rest IH]; intros symbol Member; [contradiction|].
  cbn in *. destruct (Nat.eqb_spec symbol key) as [->|Different]; [reflexivity|].
  destruct Member as [->|Member]; [contradiction|auto].
Qed.

Lemma represented_machine : forall W Prog name class symbol,
  Platform.Layouts W -> Platform.Representations W Prog ->
  In (name, class) Platform.initial_machines -> A.LangSym W "wasm" A.NSName name symbol ->
  A.nlook symbol (program_machines Prog) = Some class.
Proof.
  intros W Prog name class symbol Layouts [Table _] Member Named.
  rewrite Table. exact (machine_table_lookup W name class symbol Layouts Member Named).
Qed.

Section Layout.
Variables (W : A.world) (Prog : program).

Definition identity_args (reqs : list nat) : list (nat * ty) := map (fun t => (t, TAbs t)) reqs.

Lemma nlook_identity : forall reqs k v,
  A.nlook k (identity_args reqs) = Some v -> v = TAbs k /\ In k reqs.
Proof.
  induction reqs as [|r rest IH]; intros k v Found; [discriminate|].
  cbn in Found. destruct (Nat.eqb_spec k r) as [->|_].
  - injection Found as <-. split; [reflexivity|left; reflexivity].
  - destruct (IH k v Found) as [-> In']. split; [reflexivity|right; exact In'].
Qed.

Lemma nlook_identity_in : forall reqs k, In k reqs -> A.nlook k (identity_args reqs) = Some (TAbs k).
Proof.
  induction reqs as [|r rest IH]; intros k In'; [contradiction|].
  cbn. destruct (Nat.eqb_spec k r) as [->|Different]; [reflexivity|].
  destruct In' as [->|In']; [congruence|exact (IH k In')].
Qed.

Lemma nlook_identity_out : forall reqs k, ~ In k reqs -> A.nlook k (identity_args reqs) = None.
Proof.
  induction reqs as [|r rest IH]; intros k Out; [reflexivity|].
  cbn. destruct (Nat.eqb_spec k r) as [->|_]; [exfalso; apply Out; left; reflexivity|].
  apply IH. intro; apply Out; right; assumption.
Qed.

Lemma lookup_subst_nlook : forall k (s : subst), lookup_subst k s = A.nlook k s.
Proof. intros k s; induction s as [|[a t] r IH]; cbn; [reflexivity|rewrite IH; reflexivity]. Qed.

Lemma keys_out : forall (xs : list (nat * ty)) k, ~ In k (map fst xs) -> A.nlook k xs = None.
Proof.
  induction xs as [|[a t] r IH]; intros k Out; [reflexivity|].
  cbn. destruct (Nat.eqb_spec k a) as [->|_]; [exfalso; apply Out; left; reflexivity|].
  apply IH. intro; apply Out; right; assumption.
Qed.

Lemma keys_in : forall (xs : list (nat * ty)) k, In k (map fst xs) -> exists v, A.nlook k xs = Some v.
Proof.
  induction xs as [|[a t] r IH]; intros k In'; [contradiction|].
  cbn. destruct (Nat.eqb_spec k a) as [->|Different]; [eauto|].
  destruct In' as [E|In']; [cbn in E; congruence|exact (IH k In')].
Qed.

(* A payload read at the identity arguments as a requirement is read at
   any complete arguments as that argument. *)
Lemma raw_payload : forall s xs pay raw,
  map fst xs = A.w_reqs W s ->
  A.PayloadOf W s (identity_args (A.w_reqs W s)) (TAbs raw) ->
  A.PayloadOf W s xs pay ->
  In raw (A.w_reqs W s) /\ lookup_subst raw xs = Some pay.
Proof.
  intros s xs pay raw Keys Identity Actual.
  inversion Identity as [? ? d n ps t ? D Ast T]; subst.
  destruct (tyof_abs W _ _ _ _ _ T raw eq_refl) as [This|[k Found]]; [discriminate|].
  destruct (nlook_identity _ _ _ Found) as [E InK]. injection E as <-.
  split; [exact InK|].
  assert (Rel : SubRel xs (identity_args (A.w_reqs W s)) xs).
  { intros j. destruct (in_dec Nat.eq_dec j (A.w_reqs W s)) as [In'|Out].
    - rewrite nlook_identity_in by exact In'. cbn. rewrite lookup_subst_nlook.
      rewrite <- Keys in In'. destruct (keys_in xs j In') as [v ->]. reflexivity.
    - rewrite nlook_identity_out by exact Out. apply keys_out. rewrite Keys. exact Out. }
  pose proof (tyof_subst W xs _ _ _ _ _ T xs Rel) as T'.
  assert (Read : A.PayloadOf W s xs (app_subst xs (TAbs raw))) by (econstructor; eassumption).
  rewrite (payload_det W _ _ _ _ Actual Read). cbn.
  rewrite lookup_subst_nlook. rewrite <- Keys in InK. destruct (keys_in xs raw InK) as [v Found'].
  rewrite Found'. reflexivity.
Qed.

Hypothesis Layouts : Platform.Layouts W.
Hypothesis Reps : Platform.Representations W Prog.

(* The cast table holds exactly the selected Ref and Array entries. *)
Lemma cast_entry : forall s schema,
  lookup_cast s (program_casts Prog) = Some schema ->
  exists (array : bool) (held : tsym) (raw : tsym),
    A.LangSym W "wasm" A.NSName (if array then "Array" else "Ref") s /\
    A.LangSym W "wasm" A.NSName (if array then "ArrayRef" else "StructRef") raw /\
    schema = CastSpec (Platform.ref_kind array) held (Some (raw, TMachine raw)).
Proof.
  intros s schema Found. destruct Reps as [_ [reference [array [Ref [Arr Casts]]]]].
  rewrite Casts in Found.
  destruct Ref as [[n [t [h [Tag [_ [Raw ->]]]]]]|[_ ->]]; cbn in Found.
  - destruct (Nat.eqb_spec s n) as [->|_].
    + injection Found as <-. exists false, t, h. auto.
    + destruct Arr as [[n' [t' [h' [Tag' [_ [Raw' ->]]]]]]|[_ ->]]; cbn in Found; [|discriminate].
      destruct (Nat.eqb_spec s n') as [->|_]; [|discriminate].
      injection Found as <-. exists true, t', h'. auto.
  - destruct Arr as [[n' [t' [h' [Tag' [_ [Raw' ->]]]]]]|[_ ->]]; cbn in Found; [|discriminate].
    destruct (Nat.eqb_spec s n') as [->|_]; [|discriminate].
    injection Found as <-. exists true, t', h'. auto.
Qed.

Lemma cast_layout : forall s schema,
  lookup_cast s (program_casts Prog) = Some schema ->
  A.kind_sym W s = Some A.DKTag /\
  exists (array : bool) (raw : tsym), schema = CastSpec (Platform.ref_kind array) (cast_held schema) (Some (raw, TMachine raw)) /\
    cast_kind schema = Platform.ref_kind array /\
    A.PayloadOf W s (identity_args (A.w_reqs W s)) (TAbs raw) /\
    A.nlook raw (program_machines Prog) = Some (Platform.ref_class array).
Proof.
  intros s schema Found.
  destruct (cast_entry s schema Found) as [array [held [raw [Tag [Raw ->]]]]].
  destruct (proj2 (proj2 Layouts)) as [RefLayout [ArrLayout _]].
  destruct (if array as b return
      A.LangSym W "wasm" A.NSName (if b then "Array" else "Ref") s ->
      exists param raw' rest, A.LangSym W "wasm" A.NSName (if b then "Arrayed" else "Boxed") param /\
        A.LangSym W "wasm" A.NSName (if b then "ArrayRef" else "StructRef") raw' /\
        A.kind_sym W s = Some A.DKTag /\ A.w_reqs W s = param :: rest /\
        A.PayloadOf W s (map (fun t => (t, TAbs t)) (A.w_reqs W s)) (TAbs raw')
    then ArrLayout s else RefLayout s) as [param [raw' [rest [_ [Raw' [Kind [_ Pay]]]]]]];
    [destruct array; exact Tag|].
  pose proof (lang_sym_unique _ _ _ _ _ _ Raw Raw') as <-.
  split; [exact Kind|]. exists array, raw. split; [reflexivity|]. split; [reflexivity|]. split; [exact Pay|].
  eapply represented_machine; [exact Layouts|exact Reps| |exact Raw].
  destruct array; cbn; tauto.
Qed.

Lemma unit_not_cast : forall s, A.kind_sym W s = Some A.DKUnit ->
  lookup_cast s (program_casts Prog) = None.
Proof.
  intros s Unit. destruct (lookup_cast s (program_casts Prog)) as [schema|] eqn:Found; [|reflexivity].
  destruct (cast_layout s schema Found) as [Kind _]. congruence.
Qed.

End Layout.

Module Checks (Prims : PRIMITIVES) (R0 : INTERP Prims).
Module CT := CastTyping Prims R0.
Module R := R0.

Theorem tag_check : forall W Prog, Platform.Layouts W -> Platform.Representations W Prog ->
  forall argv read d H s xs pay value,
  (A.kind_sym W s = Some A.DKTag /\ A.PayloadOf W s xs pay /\ map fst xs = A.w_reqs W s) \/
  (A.kind_sym W s = Some A.DKUnit /\ xs = [] /\ pay = TUnit) ->
  value_typed W (program_of Prog argv read) H value (ground d pay) ->
  match R.check_tag (program_of Prog argv read) d s xs value with
  | Normal result => value_typed W (program_of Prog argv read) H result (ground d (TNom s xs))
  | Trap _ => True
  | _ => False
  end.
Proof.
  intros W Prog Layouts Reps argv read d H s xs pay value [[Kind [Pay Keys]]|[Unit [-> ->]]] Typed.
  - apply (CT.check_tag_preserves W _ H d s xs value (ground d pay)).
    + exact (payload_subst W (type_meanings d) _ _ _ Pay).
    + exact Typed.
    + intros schema Found.
      destruct (cast_layout W Prog Layouts Reps s schema Found) as [_ [array [raw [Schema [Array [Identity Class]]]]]].
      destruct (raw_payload W s xs pay raw Keys Identity Pay) as [_ Raw].
      rewrite Schema. split.
      * intros symbol expected Handle. cbn in Handle. injection Handle as <- _. eauto.
      * intros Enabled. unfold cast_enabled in Enabled. cbn [cast_handle] in Enabled.
        unfold type_argument in Enabled. rewrite lookup_ground_arguments, Raw in Enabled.
        cbn in Enabled. apply ty_eqb_spec in Enabled.
        change (app_subst (type_meanings d) pay) with (ground d pay) in Enabled.
        cbn [cast_kind]. rewrite Enabled. constructor. exact Class.
  - unfold R.check_tag. cbn [view_casts program_of]. rewrite (unit_not_cast W Prog Layouts Reps s Unit).
    inversion Typed; subst. apply TypedUnitTag. exact Unit.
Qed.
End Checks.
