(* Primitive closures behave as typed calls. A
   primitive whose platform signature satisfies Platform.PrimitiveSignature,
   run by the reference interpreter under any interface satisfying
   Public.interface_laws that supports it, either stops (trap, exit,
   unsupported) or returns a value of its grounded result type, with the
   ghost heap extended and the interface's heap invariant kept.

   The signature's machine types are read through the meanings in force
   (MachinesGround), and its handle tags (Ref, Array, Addr) are formed with
   their raw-handle requirement read as itself (HandleFormed). An operation
   declared to return `|` has the result shape ShapeNever, which no value
   has, so the result law forbids it to return normally. *)
From Stdlib Require Import List String Arith Bool PrimInt63.
From NM Require AuditBase.
From NM Require Import BaseFacts.
From NM Require Import Audit PublicTypeRead PublicTypeFacts PublicValueTyping
  PublicHeapTyping PublicBoolTyping PublicTagCheck PublicOperationFacts.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.

Ltac in_list := cbn; repeat (first [left; reflexivity | right]).

(* The handle tags of lib/wasm.moss: the tag, its parameter, its raw payload. *)
Definition handle_tags : list (string * string * string) :=
  [("Ref", "Boxed", "StructRef"); ("Array", "Arrayed", "ArrayRef"); ("Addr", "Memory", "I32")].

Section Static.
Variable W : A.world.
Hypothesis Layouts : Platform.Layouts W.

Lemma handle_layout : forall name param handle, In (name, param, handle) handle_tags ->
  Platform.TagLayout W name param handle /\ In handle Platform.abstract_names.
Proof.
  intros name param handle Found. destruct Layouts as [_ [_ [Ref [Arr Addr]]]].
  destruct Found as [E|[E|[E|[]]]]; injection E as <- <- <-; (split; [assumption|in_list]).
Qed.

Lemma layout_raw : forall name param handle s, In (name, param, handle) handle_tags ->
  A.LangSym W "wasm" A.NSName name s ->
  exists raw, A.LangSym W "wasm" A.NSName handle raw /\ A.kind_sym W s = Some A.DKTag /\
    A.w_reqs W s <> [] /\ A.PayloadOf W s (identity_args (A.w_reqs W s)) (TAbs raw).
Proof.
  intros name param handle s Found Sym.
  destruct (handle_layout _ _ _ Found) as [Layout _].
  destruct (Layout s Sym) as [param' [raw [rest [_ [Raw [Kind [Reqs Pay]]]]]]].
  exists raw. split; [exact Raw|]. split; [exact Kind|]. split; [congruence|exact Pay].
Qed.

Lemma ref_tags : forall array : bool,
  In ((if array then "Array" else "Ref"), (if array then "Arrayed" else "Boxed"),
      (if array then "ArrayRef" else "StructRef")) handle_tags.
Proof. destruct array; in_list. Qed.

(* A tag laid out with a raw requirement reads its payload at complete
   arguments as that argument. *)
Lemma payload_at_args : forall s xs raw,
  map fst xs = A.w_reqs W s ->
  A.PayloadOf W s (identity_args (A.w_reqs W s)) (TAbs raw) ->
  A.PayloadOf W s xs (app_subst xs (TAbs raw)).
Proof.
  intros s xs raw Keys Identity.
  inversion Identity as [? ? d n ps t ? D Ast T]; subst.
  assert (Rel : SubRel xs (identity_args (A.w_reqs W s)) xs).
  { intros j. destruct (in_dec Nat.eq_dec j (A.w_reqs W s)) as [In'|Out].
    - rewrite nlook_identity_in by exact In'. cbn. rewrite lookup_subst_nlook.
      rewrite <- Keys in In'. destruct (keys_in xs j In') as [v ->]. reflexivity.
    - rewrite nlook_identity_out by exact Out. apply keys_out. rewrite Keys. exact Out. }
  pose proof (tyof_subst W xs _ _ _ _ _ T xs Rel) as T'.
  econstructor; eassumption.
Qed.
End Static.

Section Typing.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Local Notation P := (program_of Prog argv read).
Hypothesis Layouts : Platform.Layouts W.
Hypothesis Reps : Platform.Representations W Prog.
Hypothesis Bools : BoolSymbols W Prog.
Variable m : subst.

(* The meanings in force read each wasm machine type as its machine
   (AuditBase.machine_subst supplies exactly these at the entry). *)
Definition MachinesGround : Prop :=
  forall symbol, In symbol (map fst (program_machines Prog)) ->
    app_subst m (TAbs symbol) = TRigid symbol.

(* A handle tag in a platform signature is a complete application whose
   raw-handle requirement is read as itself, as lib/wasm.moss writes it. *)
Definition HandleFormed (t : ty) : Prop :=
  forall name param handle symbol args, In (name, param, handle) handle_tags ->
    A.LangSym W "wasm" A.NSName name symbol -> t = TNom symbol args ->
    map fst args = A.w_reqs W symbol /\
    forall raw, A.LangSym W "wasm" A.NSName handle raw -> A.nlook raw args = Some (TAbs raw).

Hypothesis Ground : MachinesGround.

Lemma machine_ground : forall name class t, In (name, class) Platform.initial_machines ->
  Platform.MachineType W name t ->
  exists symbol, app_subst m t = TRigid symbol /\
    A.nlook symbol (program_machines Prog) = Some class.
Proof.
  intros name class t Member [symbol [Sym [_ ->]]].
  pose proof (represented_machine W Prog name class symbol Layouts Reps Member Sym) as Class.
  exists symbol. split; [apply Ground; eauto using machine_lookup_in|exact Class].
Qed.

Lemma raw_ground : forall name param handle class symbol args raw,
  In (name, param, handle) handle_tags -> In (handle, class) Platform.initial_machines ->
  HandleFormed (TNom symbol args) -> A.LangSym W "wasm" A.NSName name symbol ->
  A.LangSym W "wasm" A.NSName handle raw ->
  lookup_subst raw (PublicTypeRead.smap m args) = Some (TRigid raw).
Proof.
  intros name param handle class symbol args raw Tag Machine Formed Sym Raw.
  destruct (Formed _ _ _ _ _ Tag Sym eq_refl) as [_ Raws].
  rewrite lookup_subst_nlook, nlook_smap, (Raws raw Raw). cbn. f_equal.
  apply Ground. eapply machine_lookup_in. eapply represented_machine; eassumption.
Qed.

Lemma keys_smap : forall (args : list (nat * ty)), map fst (PublicTypeRead.smap m args) = map fst args.
Proof. intros args. unfold PublicTypeRead.smap. rewrite map_map. reflexivity. Qed.

Lemma scalar_value : forall H w symbol sort,
  A.nlook symbol (program_machines Prog) = Some (MScalar sort) ->
  value_typed W P H w (TRigid symbol) ->
  exists bits, w = VScalar sort bits.
Proof.
  intros H w symbol sort Class Typed. inversion Typed; subst;
    match goal with Read : A.nlook _ (view_machines _) = Some _ |- _ =>
      change (view_machines P) with (program_machines Prog) in Read;
      rewrite Class in Read; inversion Read; subst; eauto
    end.
Qed.

Lemma ref_machine : forall array : bool,
  In ((if array then "ArrayRef" else "StructRef"), Platform.ref_class array) Platform.initial_machines.
Proof. destruct array; in_list. Qed.

(* The first type argument of a Base handle is its held type. *)
Lemma ref_site : forall array t held,
  Platform.ReferenceType W array t held -> HandleFormed t ->
  exists head key rest, app_subst m t = TNom head ((key, app_subst m held) :: rest).
Proof.
  intros array t held [symbol [param [args [Sym [Param [-> Held]]]]]] Formed.
  destruct (handle_layout W Layouts _ _ _ (ref_tags array)) as [Layout _].
  destruct (Layout symbol Sym) as [head_param [raw [tail [Param' [_ [_ [Reqs _]]]]]]].
  pose proof (lang_sym_unique _ _ _ _ _ _ Param Param') as <-.
  destruct (Formed _ _ _ _ _ (ref_tags array) Sym eq_refl) as [Keys _].
  rewrite Reqs in Keys. destruct args as [|[key actual] rest]; [discriminate|].
  injection Keys as <- _. cbn [A.nlook] in Held. rewrite Nat.eqb_refl in Held.
  injection Held as <-. rewrite subst_nom. exists symbol, key, (PublicTypeRead.smap m rest).
  reflexivity.
Qed.

(* A fresh handle at a reference type, read through its checked header. *)
Lemma ref_output : forall H array t held s hv, Platform.ReferenceType W array t held -> HandleFormed t ->
  A.LangSym W "wasm" A.NSName (if array then "Array" else "Ref") s ->
  handle_typed H (Platform.ref_kind array) (app_subst m held) hv ->
  value_typed W P H (VTag s hv) (app_subst m t).
Proof.
  intros H array t held s hv [symbol [param [args [Sym [Param [-> Held]]]]]] Formed S Handle.
  pose proof (lang_sym_unique _ _ _ _ _ _ S Sym); subst s.
  destruct (layout_raw W Layouts _ _ _ symbol (ref_tags array) Sym) as [raw [RawSym [_ [_ Identity]]]].
  pose proof (raw_ground _ _ _ _ symbol args raw (ref_tags array) (ref_machine array) Formed Sym RawSym) as Enabled.
  destruct (Formed _ _ _ _ _ (ref_tags array) Sym eq_refl) as [Keys _].
  pose proof (payload_at_args W symbol (PublicTypeRead.smap m args) raw ltac:(rewrite keys_smap; exact Keys) Identity) as Pay.
  cbn [app_subst] in Pay. rewrite Enabled in Pay.
  rewrite subst_nom.
  eapply TypedTag; [exact Pay|].
  eapply TypedRawHandle; [|exact Handle].
  eapply represented_machine; [exact Layouts|exact Reps|apply ref_machine|exact RawSym].
Qed.

Lemma addr_input : forall H t w, Platform.NominalType W "wasm" "Addr" t -> HandleFormed t ->
  value_typed W P H w (app_subst m t) -> exists s n, w = VTag s (VScalar 0 n).
Proof.
  intros H t w [symbol [args [Sym ->]]] Formed Typed.
  assert (Tag : In ("Addr", "Memory", "I32") handle_tags) by in_list.
  destruct (layout_raw W Layouts _ _ _ symbol Tag Sym) as [raw [RawSym [Kind [_ Identity]]]].
  pose proof (raw_ground _ _ _ Platform.i32_class symbol args raw Tag ltac:(in_list) Formed Sym RawSym) as Raw.
  pose proof (represented_machine W Prog "I32" Platform.i32_class raw Layouts Reps ltac:(in_list) RawSym) as Class.
  destruct (Formed _ _ _ _ _ Tag Sym eq_refl) as [Keys _].
  rewrite subst_nom in Typed. inversion Typed; subst.
  - match goal with Pay : A.PayloadOf _ _ _ ?payload, Inner : value_typed _ _ _ ?v ?payload |- _ =>
      destruct (raw_payload W symbol (PublicTypeRead.smap m args) payload raw ltac:(rewrite keys_smap; exact Keys) Identity Pay)
        as [_ Look];
      rewrite Raw in Look; injection Look as <-;
      destruct (scalar_value _ _ _ _ Class Inner) as [n ->]; eauto end.
  - congruence.

Qed.

Lemma bool_syms : forall s, A.LangSym W "bool" A.NSName "Bool" s ->
  s = program_bool Prog /\ forall truth falsity,
    A.LangSym W "bool" A.NSName "True" truth -> A.LangSym W "bool" A.NSName "False" falsity ->
    truth = program_true Prog /\ falsity = program_false Prog.
Proof.
  intros s Sym. destruct Bools as [_ [[B [T F]]|[None' _]]]; [|exfalso; exact (None' s Sym)].
  split; [exact (lang_sym_unique _ _ _ _ _ _ Sym B)|].
  intros truth falsity T' F'. split; eapply lang_sym_unique; eassumption.
Qed.

Lemma bool_typed : forall H s w, A.LangSym W "bool" A.NSName "Bool" s ->
  value_shape P w ShapeBool -> value_typed W P H w (TNom s []).
Proof.
  intros H s w Sym Shape.
  destruct w as [|b inner| |]; try contradiction.
  destruct inner as [|t' u| |]; try contradiction. destruct u; try contradiction.
  destruct Shape as [Eb Et]. cbn in Eb, Et.
  destruct (proj1 Bools s Sym) as [truth [falsity [T [F [_ [TU [FU [_ [_ Payload]]]]]]]]].
  destruct (bool_syms s Sym) as [-> Names]. destruct (Names _ _ T F) as [-> ->]. subst b.
  destruct Payload as [Pay|Pay]; (eapply TypedTag; [exact Pay|]);
    destruct Et as [->| ->]; (eapply TypedUnion; [apply TypedUnitTag; eassumption|in_list]).
Qed.

(* A typed argument has the shape its platform signature names. *)
Lemma shape_param : forall H t s w, Platform.ValueType W t s -> HandleFormed t ->
  value_typed W P H w (app_subst m t) -> value_shape P w s.
Proof.
  intros H t s w VT Formed Typed. destruct s; cbn in VT.
  - destruct (machine_ground "I32" Platform.i32_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (scalar_value _ _ _ _ Class Typed) as [n ->]; exact I.
  - destruct (machine_ground "I64" Platform.i64_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (scalar_value _ _ _ _ Class Typed) as [n ->]; exact I.
  - destruct (machine_ground "F32" Platform.f32_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (scalar_value _ _ _ _ Class Typed) as [n ->]; exact I.
  - destruct (machine_ground "F64" Platform.f64_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (scalar_value _ _ _ _ Class Typed) as [n ->]; exact I.
  - subst t. cbn in Typed. inversion Typed; exact I.
  - destruct VT as [symbol [Sym ->]]. cbn in Typed.
    destruct (bool_value_shape W P H symbol w (proj1 Bools) Sym Typed) as [truth [falsity [T [F Shape]]]].
    destruct (bool_syms symbol Sym) as [-> Names]. destruct (Names _ _ T F) as [-> ->].
    destruct Shape as [->| ->]; cbn; auto.
  - destruct (addr_input H t w VT Formed Typed) as [s [n ->]]; exact I.
  - destruct VT as [symbol [args ->]]. rewrite subst_nom in Typed. inversion Typed; exact I.
  - subst t. exfalso. exact (empty_union_uninhabited W P H w Typed).
Qed.

(* A result of a signature's shape is typed at its grounded type. *)
Lemma shape_result : forall H t s w, Platform.ValueType W t s ->
  s <> ShapeAddress -> s <> ShapeTag -> value_shape P w s -> value_typed W P H w (app_subst m t).
Proof.
  intros H t s w VT NA NT Shape. destruct s; cbn in VT; try congruence.
  - destruct (machine_ground "I32" Platform.i32_class t ltac:(in_list) VT) as [symbol [-> Class]].
    destruct w as [| | |[sort bits|]]; try contradiction.
    destruct sort as [|[|[|[|sort]]]]; try contradiction; constructor; exact Class.
  - destruct (machine_ground "I64" Platform.i64_class t ltac:(in_list) VT) as [symbol [-> Class]].
    destruct w as [| | |[sort bits|]]; try contradiction.
    destruct sort as [|[|[|[|sort]]]]; try contradiction; constructor; exact Class.
  - destruct (machine_ground "F32" Platform.f32_class t ltac:(in_list) VT) as [symbol [-> Class]].
    destruct w as [| | |[sort bits|]]; try contradiction.
    destruct sort as [|[|[|[|sort]]]]; try contradiction; constructor; exact Class.
  - destruct (machine_ground "F64" Platform.f64_class t ltac:(in_list) VT) as [symbol [-> Class]].
    destruct w as [| | |[sort bits|]]; try contradiction.
    destruct sort as [|[|[|[|sort]]]]; try contradiction; constructor; exact Class.
  - subst t. destruct w; try contradiction; constructor.
  - destruct VT as [symbol [Sym ->]]. cbn. exact (bool_typed H symbol w Sym Shape).
  - cbn in Shape. contradiction.
Qed.

Lemma shapes_params : forall H params inputs ws,
  Forall2 (Platform.ValueType W) params inputs -> Forall HandleFormed params ->
  Forall2 (fun w t => value_typed W P H w (app_subst m t)) ws params ->
  Forall2 (value_shape P) ws inputs.
Proof.
  intros H params inputs ws VT Formed Typed. revert inputs VT.
  induction Typed as [|w t ws params Tw Rest IH]; intros inputs VT; inversion VT; subst; constructor.
  - inversion Formed; subst. eapply shape_param; eassumption.
  - inversion Formed; subst. auto.
Qed.
End Typing.

Lemma output_shape : forall p ins out,
  prim_sig p = Some (ins, out) \/ prim_msig p = Some (ins, out) ->
  out <> ShapeAddress /\ out <> ShapeTag.
Proof.
  intros p ins out [E|E]; destruct p; cbn in E; try discriminate;
    repeat match type of E with context [match ?x with _ => _ end] => destruct x end;
    try discriminate; injection E as _ <-; split; discriminate.
Qed.

Lemma signature_plain : forall W params ret p sig,
  sig = prim_sig p \/ sig = prim_msig p -> Platform.Signature W params ret sig -> Heap.is_gc p = false.
Proof.
  intros W params ret p sig Which [ins [out [E _]]].
  destruct Which as [->| ->]; destruct p; cbn in E; try discriminate; reflexivity.
Qed.

(* A primitive method binding records its ground receiver type. *)
Definition HeldAt (m : subst) (params : list ty) (held : option ty) : Prop :=
  match held with
  | None => True
  | Some tau => exists first rest, params = first :: rest /\ tau = app_subst m first
  end.

Module PrimitiveSafety (Host : HOST) (M0 : PUBLIC Host).
Module M := M0.

(* HeapTyping.HeapSafety's allocation lemmas, at this instance of Public
   (functor instances have distinct interface records). *)
Lemma allocation_preserves_heap : forall W P iface H state p args held raw next tag (kind : nat),
  M.interface_laws P iface -> M.supports iface (encode p) = true ->
  M.heap_ok iface (heap_predicates W P H) state ->
  Heap.gc_pre (heap_predicates W P H) p args (fun value => value_typed W P H value held) ->
  (forall Psi Psi' Q w, Heap.gc_post Psi Psi' p Q w ->
     Heap.hext Psi Psi' /\ exists a, w = VTag tag (VRef a None) /\ Psi a = None /\ Psi' a = Some (kind, Q)) ->
  M.primitive iface P (encode p) args state = (Normal raw, next) ->
  exists H' address,
    heap_extends H H' /\ M.heap_ok iface (heap_predicates W P H') next /\
    raw = VTag tag (VRef address None) /\ H' address = Some (kind, held).
Proof.
  intros W P iface H state p args held raw next tag kind Laws Supported Valid Pre Post Run.
  destruct Laws as [_ [_ [_ [Weaken [_ [_ GC]]]]]].
  specialize (GC p args state (heap_predicates W P H) _ Supported Valid Pre).
  rewrite Run in GC.
  destruct GC as [extended [ValidNext PostRaw]].
  destruct (Post _ _ _ _ PostRaw) as [Extends [fresh [Raw [Absent Fresh]]]].
  assert (FreshH : H fresh = None).
  { unfold heap_predicates in Absent.
    destruct (H fresh) as [[old_kind tau]|]; [discriminate|reflexivity]. }
  exists (heap_add H fresh kind held), fresh. split.
  - apply heap_add_extends; exact FreshH.
  - split.
    + eapply Weaken; [exact ValidNext|].
      eapply allocation_predicates; eassumption.
    + split; [exact Raw|apply heap_add_here].
Qed.

Lemma args_plain : forall p ws, Heap.is_gc p = false -> BaseCall.primitive_arguments p ws = ws.
Proof. intros p ws GC; destruct p; try discriminate; reflexivity. Qed.

Lemma alloc_plain : forall p tau o, Heap.is_gc p = false -> BaseCall.allocation_result p tau o = o.
Proof. intros p tau o GC; destruct p; try discriminate; reflexivity. Qed.

Lemma raw_receiver_of : forall w s a k h,
  w = VTag s (VRef a None) \/ w = VTag s (VRef a (Some (k, h))) ->
  BaseCall.raw_receiver w = VTag s (VRef a None).
Proof. intros w s a k h [-> | ->]; reflexivity. Qed.


(* The reference host's state realizes the ghost heap H: the interface is
   the fixed one, and its heap invariant holds of H's predicates. *)
Definition state_ok (W : A.world) (P : program_view) (iface : M.interface)
    (H : heap_types) (st : M.ReferenceHost.mstate) : Prop :=
  fst st = iface /\ M.heap_ok iface (heap_predicates W P H) (M.external (snd st)).

Theorem state_ok_initial : forall W P iface state,
  M.interface_laws P iface -> state_ok W P iface (fun _ => None) (iface, state).
Proof.
  intros W P iface state Laws. split; [reflexivity|].
  destruct Laws as [_ [_ [_ [_ [Empty _]]]]]. exact (Empty (M.external state)).
Qed.

(* PublicBodySlice.PrimOutcome at Prims := M.ReferenceHost. *)
Definition prim_outcome (W : A.world) (P : program_view) (iface : M.interface)
    (H : heap_types) (t : ty) (r : signal * M.ReferenceHost.mstate) : Prop :=
  match r with
  | (signal, st) =>
      match signal with
      | Normal w => exists H', heap_extends H H' /\ state_ok W P iface H' st /\ value_typed W P H' w t
      | Trap _ | Exit _ | UnsupportedOperation _ | OutOfFuel => True
      | _ => False
      end
  end.

Lemma heap_extends_refl : forall H, heap_extends H H.
Proof. intros H a c F; exact F. Qed.

Lemma run_supported : forall P iface p held ws s,
  M.supports iface (encode p) = true ->
  M.Reference.run_prim_at P (encode p) held ws (iface, s) =
  match BaseCall.cast_operation p with
  | Some (array, tag) => (BaseCall.cast_result array tag held ws, (iface, s))
  | None =>
      if BaseCall.heap_arguments_match p (fun _ => held) ws then
      let '(out, next) := M.primitive iface P (encode p)
        (BaseCall.arguments_at_site p (fun _ => held) ws) (M.external s) in
      (match held with Some tau => BaseCall.allocation_result p tau out | None => out end,
       (iface, M.World (M.arguments s) (M.read_file s) next))
      else (Trap 37, (iface, s))
  end.
Proof.
  intros P iface p held ws s Supp. unfold M.Reference.run_prim_at, M.ReferenceHost.run_prim, Base.call, BaseCall.call, M.ReferenceHost.raw_primitive.
  rewrite decode_encode. cbn [BaseCall.cast_at_site BaseCall.heap_call_ready].
  destruct (BaseCall.cast_operation p) as [[array tag]|]; [reflexivity|].
  destruct (BaseCall.heap_arguments_match p _ ws); [|reflexivity].
  rewrite Supp. destruct (M.primitive iface P (encode p) (BaseCall.arguments_at_site p (fun _ => held) ws) (M.external s)); reflexivity.
Qed.

(* A non-allocating GC operation whose precondition holds at the ghost heap. *)
Lemma gc_plain : forall W P iface H s p held ws Q t,
  M.interface_laws P iface -> M.supports iface (encode p) = true ->
  M.heap_ok iface (heap_predicates W P H) (M.external s) ->
  (BaseCall.heap_arguments_match p (fun _ => held) ws = true ->
    Heap.gc_pre (heap_predicates W P H) p (BaseCall.primitive_arguments p ws) Q) ->
  (forall extended w, Heap.gc_post (heap_predicates W P H) extended p Q w -> value_typed W P H w t) ->
  (forall tau o, BaseCall.allocation_result p tau o = o) ->
  prim_outcome W P iface H t (M.Reference.run_prim_at P (encode p) held ws (iface, s)).
Proof.
  intros W P iface H s p held ws Q t Laws Supp Valid Pre Post Plain.
  rewrite run_supported by exact Supp.
  assert (Cast : BaseCall.cast_operation p = None).
  { destruct p; try reflexivity. specialize (Pre eq_refl). contradiction. }
  rewrite Cast.
  destruct (BaseCall.heap_arguments_match p _ ws) eqn:Ready; [|exact I].
  specialize (Pre eq_refl).
  rewrite arguments_at_site_gc by (destruct p; cbn in Pre; try contradiction; reflexivity).
  destruct Laws as [_ [_ [_ [Weaken [_ [_ GC]]]]]].
  specialize (GC p (BaseCall.primitive_arguments p ws) (M.external s) _ Q Supp Valid Pre).
  destruct (M.primitive iface P (encode p) (BaseCall.primitive_arguments p ws) (M.external s)) as [out next].
  assert (Out : match out with Normal w => exists H', heap_extends H H' /\
      state_ok W P iface H' (iface, M.World (M.arguments s) (M.read_file s) next) /\ value_typed W P H' w t
    | Trap _ | Exit _ | UnsupportedOperation _ | OutOfFuel => True | _ => False end).
  { destruct out; try exact I; try exact GC.
    destruct GC as [extended [ValidNext PostW]].
    exists H. split; [apply heap_extends_refl|]. split; [|exact (Post _ _ PostW)].
    split; [reflexivity|]. cbn. apply (Weaken extended); [exact ValidNext|].
    intros a k pr Found. exists pr. split; [exact (proj1 PostW a k pr Found)|auto]. }
  destruct held as [tau|]; [rewrite Plain|]; exact Out.
Qed.

Ltac shape_args := repeat match goal with
  | F : Forall2 _ [] (_ :: _) |- _ => inversion F
  | F : Forall2 _ ?l [] |- _ => is_var l; inversion F; subst; clear F
  | F : Forall2 _ (_ :: _) [] |- _ => inversion F
  | F : Forall2 _ (_ :: _) (_ :: _) |- _ => apply Forall2_cons_iff in F; destruct F as [? F]
  | F : Forall _ (_ :: _) |- _ => apply Forall_cons_iff in F; destruct F as [? F] end.
(* Values are named x, y, y0, y1, y2 in argument order. *)
Ltac args_inv := match goal with
  | Args : Forall2 _ ?ws _ |- _ => destruct ws as [|x [|y [|y0 [|y1 [|y2 ?]]]]]; shape_args end.

Lemma href_of : forall W P H k s a held,
  H a = Some (k, held) ->
  Heap.href (heap_predicates W P H) k (VTag s (VRef a None)) (fun v => value_typed W P H v held).
Proof. intros W P H k s a held Found. exists s, a. split; [reflexivity|]. unfold heap_predicates. rewrite Found. reflexivity. Qed.

Lemma checked_href : forall W P H m array receiver params held w t expected,
  (exists head key rest, app_subst m receiver = TNom head ((key, expected) :: rest)) ->
  HeldAt m (receiver :: params) held ->
  value_typed W P H w t ->
  BaseCall.reference_matches array held w = true ->
  Heap.href (heap_predicates W P H) (Platform.ref_kind array) (BaseCall.raw_receiver w)
    (fun v => value_typed W P H v expected).
Proof.
  intros W P H m array receiver params [site|] w t expected Shape Held Typed Checked;
    [|discriminate].
  destruct Held as [first [rest [Equation ->]]]. injection Equation as <- <-.
  destruct Shape as [head [key [arguments Site]]]. rewrite Site in Checked.
  destruct w as [|tag [| | |[|address [[kind actual]|]]]| |]; try discriminate.
  cbn [BaseCall.reference_matches] in Checked.
  change (if array then 1%nat else 0%nat) with (Platform.ref_kind array) in Checked.
  apply andb_true_iff in Checked as [Kind Same].
  apply Nat.eqb_eq in Kind. apply ty_eqb_spec in Same. subst kind actual.
  cbn [BaseCall.raw_receiver]. apply href_of.
  eapply tagged_checked_header; eassumption.
Qed.

Ltac reference_input array receiver params held :=
  eapply (checked_href _ _ _ _ array receiver params held);
  [ eapply ref_site; eassumption | assumption | eassumption | eassumption ].

Theorem gc_safe : forall W Prog argv read iface p held params ret m H s ws,
  Platform.Layouts W -> Platform.Representations W Prog ->
  M.interface_laws (program_of Prog argv read) iface -> M.supports iface (encode p) = true ->
  Platform.GCSignature W p params ret ->
  MachinesGround Prog m -> Forall (HandleFormed W) (ret :: params) -> HeldAt m params held ->
  M.heap_ok iface (heap_predicates W (program_of Prog argv read) H) (M.external s) ->
  Forall2 (fun w t => value_typed W (program_of Prog argv read) H w (app_subst m t)) ws params ->
  prim_outcome W (program_of Prog argv read) iface H (app_subst m ret)
    (M.Reference.run_prim_at (program_of Prog argv read) (encode p) held ws (iface, s)).
Proof.
  intros W Prog argv read iface p held params ret m H s ws Layouts Reps Laws Supp Sig Ground Formed Held Valid Args.
  set (P := program_of Prog argv read) in *.
  assert (I32 : forall H t w, Platform.MachineType W "I32" t -> value_typed W P H w (app_subst m t) ->
    exists n, w = VScalar 0 n).
  { intros H0 t w MT Typed.
    destruct (machine_ground W Prog Layouts Reps m Ground "I32" Platform.i32_class t ltac:(in_list) MT) as [symbol [Read Class]].
    rewrite Read in Typed. exact (scalar_value W Prog argv read _ _ _ _ Class Typed). }
  destruct p; try (exfalso; exact Sig).
  - (* PBox *) destruct params as [|heldt [|]]; try (exfalso; exact Sig). destruct Sig as [Sym RT].
    args_inv. rename x into v.
    rewrite run_supported by exact Supp. cbn [BaseCall.cast_operation BaseCall.heap_arguments_match BaseCall.arguments_at_site BaseCall.memory_arguments BaseCall.memory_operation BaseCall.primitive_arguments].
    pose proof Laws as Laws'. destruct Laws' as [_ [_ [_ [_ [_ [_ GC]]]]]].
    specialize (GC (PBox n) [v] (M.external s) _ (fun w => value_typed W P H w (app_subst m heldt)) Supp Valid ltac:(assumption)).
    destruct (M.primitive iface P (encode (PBox n)) [v] (M.external s)) as [out next] eqn:Run.
    destruct out; try contradiction; [|destruct held; exact I].
    destruct (allocation_preserves_heap W P iface H (M.external s) (PBox n) [v] (app_subst m heldt) v0 next n 0
      Laws Supp Valid ltac:(assumption) ltac:(intros ? ? ? ? [E X]; exact (conj E X)) Run) as [H' [a [Ext [Ok' [-> Cell]]]]].
    assert (Typed : forall hv, handle_typed H' 0 (app_subst m heldt) hv ->
      value_typed W P H' (VTag n hv) (app_subst m ret)).
    { intros hv Hv. exact (ref_output W Prog argv read Layouts Reps m Ground H' false ret heldt n hv RT ltac:(assumption) Sym Hv). }
    destruct held as [tau|]; cbn;
      (exists H'; split; [exact Ext|]; split; [split; [reflexivity|exact Ok']|]).
    + destruct Held as [first [rest [E ->]]]. injection E as <- <-. apply Typed. constructor. exact Cell.
    + apply Typed. constructor. exact Cell.
  - (* PDeref *) destruct params as [|reference [|]]; try (exfalso; exact Sig). args_inv.
    apply (gc_plain W P iface H s PDeref held [x] (fun v => value_typed W P H v (app_subst m ret))); auto.
    + intro Ready. cbn [BaseCall.heap_arguments_match] in Ready.
      cbn [BaseCall.primitive_arguments Heap.gc_pre]. reference_input false reference (@nil ty) held.
    + intros extended w [_ Q]. exact Q.
  - (* PRefSet *) destruct params as [|reference [|heldt [|]]]; try (exfalso; exact Sig). destruct Sig as [RT ->]. args_inv.
    apply (gc_plain W P iface H s PRefSet held [x; y] (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + intro Ready. cbn [BaseCall.heap_arguments_match] in Ready.
      cbn [BaseCall.primitive_arguments Heap.gc_pre]. split; [reference_input false reference [heldt] held|assumption].
    + intros extended w [_ ->]. constructor.
  - (* PArrNew *) destruct params as [|heldt [|count [|]]]; try (exfalso; exact Sig). destruct Sig as [Sym [Count RT]].
    args_inv. rename x into v.
    destruct (I32 H count y Count ltac:(assumption)) as [c ->].
    rewrite run_supported by exact Supp. cbn [BaseCall.cast_operation BaseCall.heap_arguments_match BaseCall.arguments_at_site BaseCall.memory_arguments BaseCall.memory_operation BaseCall.primitive_arguments].
    pose proof Laws as Laws'. destruct Laws' as [_ [_ [_ [_ [_ [_ GC]]]]]].
    specialize (GC (PArrNew n) [v; VScalar 0 c] (M.external s) _ (fun w => value_typed W P H w (app_subst m heldt)) Supp Valid ltac:(assumption)).
    destruct (M.primitive iface P (encode (PArrNew n)) [v; VScalar 0 c] (M.external s)) as [out next] eqn:Run.
    destruct out; try contradiction; [|destruct held; exact I].
    destruct (allocation_preserves_heap W P iface H (M.external s) (PArrNew n) [v; VScalar 0 c] (app_subst m heldt) v0 next n 1
      Laws Supp Valid ltac:(assumption) ltac:(intros ? ? ? ? [E X]; exact (conj E X)) Run) as [H' [a [Ext [Ok' [-> Cell]]]]].
    assert (Typed : forall hv, handle_typed H' 1 (app_subst m heldt) hv ->
      value_typed W P H' (VTag n hv) (app_subst m ret)).
    { intros hv Hv. exact (ref_output W Prog argv read Layouts Reps m Ground H' true ret heldt n hv RT ltac:(assumption) Sym Hv). }
    destruct held as [tau|]; cbn;
      (exists H'; split; [exact Ext|]; split; [split; [reflexivity|exact Ok']|]).
    + destruct Held as [first [rest [E ->]]]. injection E as <- <-. apply Typed. constructor. exact Cell.
    + apply Typed. constructor. exact Cell.
  - (* PArrLen *) destruct params as [|array [|]]; try (exfalso; exact Sig). destruct Sig as [MT [heldt RT]]. args_inv.
    apply (gc_plain W P iface H s PArrLen held [x] (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + intro Ready. cbn [BaseCall.heap_arguments_match] in Ready.
      cbn [BaseCall.primitive_arguments Heap.gc_pre]. reference_input true array (@nil ty) held.
    + intros extended w [_ [k ->]].
      destruct (machine_ground W Prog Layouts Reps m Ground "I32" Platform.i32_class ret ltac:(in_list) MT) as [symbol [-> Class]].
      constructor. exact Class.
  - (* PArrGet *) destruct params as [|array [|index [|]]]; try (exfalso; exact Sig). destruct Sig as [MT RT]. args_inv.
    destruct (I32 H index y MT ltac:(assumption)) as [k ->].
    apply (gc_plain W P iface H s PArrGet held [x; VScalar 0 k] (fun v => value_typed W P H v (app_subst m ret))); auto.
    + intro Ready. cbn [BaseCall.heap_arguments_match] in Ready.
      cbn [BaseCall.primitive_arguments Heap.gc_pre]. reference_input true array [index] held.
    + intros extended w [_ Q]. exact Q.
  - (* PArrSet *) destruct params as [|array [|index [|heldt [|]]]]; try (exfalso; exact Sig).
    destruct Sig as [MT [RT ->]]. args_inv.
    destruct (I32 H index y MT ltac:(assumption)) as [k ->].
    apply (gc_plain W P iface H s PArrSet held [x; VScalar 0 k; y0] (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + intro Ready. cbn [BaseCall.heap_arguments_match] in Ready.
      cbn [BaseCall.primitive_arguments Heap.gc_pre]. split; [reference_input true array [index; heldt] held|assumption].
    + intros extended w [_ ->]. constructor.
  - (* PArrFill *) destruct params as [|array [|index [|heldt [|count [|]]]]]; try (exfalso; exact Sig).
    destruct Sig as [MI [MC [RT ->]]]. args_inv.
    destruct (I32 H index y MI ltac:(assumption)) as [k ->].
    destruct (I32 H count y1 MC ltac:(assumption)) as [c ->].
    apply (gc_plain W P iface H s PArrFill held [x; VScalar 0 k; y0; VScalar 0 c]
      (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + intro Ready. cbn [BaseCall.heap_arguments_match] in Ready.
      cbn [BaseCall.primitive_arguments Heap.gc_pre]. split; [reference_input true array [index; heldt; count] held|assumption].
    + intros extended w [_ ->]. constructor.
  - (* PArrCopy *) destruct params as [|dst [|di [|src [|si [|count [|]]]]]]; try (exfalso; exact Sig).
    destruct Sig as [MDI [MSI [MC [-> [heldt [RD RS]]]]]]. args_inv.
    destruct (I32 H di y MDI ltac:(assumption)) as [k ->].
    destruct (I32 H si y1 MSI ltac:(assumption)) as [j ->].
    destruct (I32 H count y2 MC ltac:(assumption)) as [c ->].
    apply (gc_plain W P iface H s PArrCopy held [x; VScalar 0 k; y0; VScalar 0 j; VScalar 0 c]
      (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + intro Ready. cbn [BaseCall.heap_arguments_match] in Ready.
      apply andb_true_iff in Ready as [Dst Src].
      cbn [BaseCall.primitive_arguments Heap.gc_pre]. split.
      * reference_input true dst [di; src; si; count] held.
      * exists (fun v => value_typed W P H v (app_subst m heldt)). split; [|auto].
        reference_input true dst [di; src; si; count] held.
    + intros extended w [_ ->]. constructor.
  - (* explicit cast *)
    destruct params as [|request [|]]; try contradiction.
    destruct Sig as [Tag [wanted [Target Result]]].
    destruct Target as [caster [parameter [types [_ [_ [-> [rest ->]]]]]]].
    args_inv. rewrite run_supported by exact Supp. cbn [BaseCall.cast_operation].
    destruct held as [site|]; [|exact I].
    destruct Held as [first [others [Equation ->]]]. injection Equation as <- <-.
    rewrite subst_nom.
    destruct x as [|source raw| |]; try exact I.
    destruct raw as [| | |[|address [[kind actual]|]]]; try exact I.
    cbn [BaseCall.cast_result PublicTypeRead.smap map snd].
    change (if array then 1%nat else 0%nat) with (Platform.ref_kind array).
    destruct (Nat.eqb (Platform.ref_kind array) kind && ty_eqb (app_subst m wanted) actual)
      eqn:Checked; [|exact I].
    apply andb_true_iff in Checked as [Kind TypeEqual].
    apply Nat.eqb_eq in Kind. apply ty_eqb_spec in TypeEqual. subst kind actual.
    exists H. split; [apply heap_extends_refl|].
    split; [split; [reflexivity|exact Valid]|].
    eapply ref_output; [exact Layouts|exact Reps|exact Ground|exact Result|
      exact H1|exact Tag|].
    constructor. eapply tagged_checked_header; eassumption.
Qed.

(* T11/T17: a primitive closure is a typed call. *)
Lemma primitive_safe_known : forall W Prog argv read iface p held params ret m H st ws,
  Platform.Layouts W -> Platform.Representations W Prog -> BoolSymbols W Prog ->
  M.interface_laws (program_of Prog argv read) iface -> M.supports iface (encode p) = true ->
  Platform.WasmSignature W p params ret ->
  MachinesGround Prog m -> Forall (HandleFormed W) (ret :: params) -> HeldAt m params held ->
  state_ok W (program_of Prog argv read) iface H st ->
  Forall2 (fun w t => value_typed W (program_of Prog argv read) H w (app_subst m t)) ws params ->
  prim_outcome W (program_of Prog argv read) iface H (app_subst m ret)
    (M.Reference.run_prim_at (program_of Prog argv read) (encode p) held ws st).
Proof.
  intros W Prog argv read iface p held params ret m H [i s] ws Layouts Reps Bools Laws Supp
    Sig Ground Formed Held [Ei Valid] Args.
  cbn in Ei, Valid. subst i.
  destruct Sig as [Plain|[Plain|GCSig]];
    [| |exact (gc_safe W Prog argv read iface p held params ret m H s ws Layouts Reps Laws Supp
                 GCSig Ground Formed Held Valid Args)].
  - assert (GC : Heap.is_gc p = false) by exact (signature_plain W params ret p _ (or_introl eq_refl) Plain).
    destruct Plain as [ins [out [Sig' [VTs VT]]]].
    destruct Laws as [Law [_ [_ [_ [_ [Keep _]]]]]].
    pose proof (Law p ins out (fun _ => held) ws (M.external s) Supp Sig'
      (shapes_params W Prog argv read Layouts Reps Bools m Ground H params ins ws VTs
        (Forall_inv_tail Formed) Args)) as Result.
    set (raw := BaseCall.memory_arguments p (fun _ => held) ws) in *.
    pose proof (Keep (encode p) raw (M.external s) _ Supp ltac:(rewrite decode_encode; exact GC) Valid) as KeepH.
    destruct (output_shape p ins out (or_introl Sig')) as [NA NT].
    rewrite run_supported by exact Supp.
    assert (Cast : BaseCall.cast_operation p = None) by (destruct p; cbn in *; congruence).
    rewrite Cast. assert (Ready : BaseCall.heap_arguments_match p (fun _ => held) ws = true)
      by (destruct p; cbn in *; congruence).
    rewrite Ready. unfold BaseCall.arguments_at_site. rewrite args_plain by exact GC. fold raw.
    destruct (M.primitive iface _ (encode p) raw (M.external s)) as [o next].
    cbn in Result, KeepH. destruct held; [rewrite alloc_plain by exact GC|];
      destruct o; try contradiction; try exact I;
      (exists H; split; [apply heap_extends_refl|]; split; [split; [reflexivity|exact KeepH]|];
       eapply shape_result; eassumption).
  - assert (GC : Heap.is_gc p = false) by exact (signature_plain W params ret p _ (or_intror eq_refl) Plain).
    destruct Plain as [ins [out [Sig' [VTs VT]]]].
    destruct Laws as [_ [Law [_ [_ [_ [Keep _]]]]]].
    pose proof (Law p ins out (fun _ => held) ws (M.external s) Supp Sig'
      (shapes_params W Prog argv read Layouts Reps Bools m Ground H params ins ws VTs
        (Forall_inv_tail Formed) Args)) as Result.
    set (raw := BaseCall.memory_arguments p (fun _ => held) ws) in *.
    pose proof (Keep (encode p) raw (M.external s) _ Supp ltac:(rewrite decode_encode; exact GC) Valid) as KeepH.
    destruct (output_shape p ins out (or_intror Sig')) as [NA NT].
    rewrite run_supported by exact Supp.
    assert (Cast : BaseCall.cast_operation p = None) by (destruct p; cbn in *; congruence).
    rewrite Cast. assert (Ready : BaseCall.heap_arguments_match p (fun _ => held) ws = true)
      by (destruct p; cbn in *; congruence).
    rewrite Ready. unfold BaseCall.arguments_at_site. rewrite args_plain by exact GC. fold raw.
    destruct (M.primitive iface _ (encode p) raw (M.external s)) as [o next].
    cbn in Result, KeepH. destruct held; [rewrite alloc_plain by exact GC|];
      destruct o; try contradiction; try exact I;
      (exists H; split; [apply heap_extends_refl|]; split; [split; [reflexivity|exact KeepH]|];
       eapply shape_result; eassumption).
Qed.

Theorem primitive_safe : forall W Prog argv read iface p held params ret m H st ws,
  Platform.Layouts W -> Platform.Representations W Prog -> BoolSymbols W Prog ->
  M.interface_laws (program_of Prog argv read) iface -> M.supports iface p = true ->
  Platform.PrimitiveSignature W p params ret ->
  MachinesGround Prog m -> Forall (HandleFormed W) (ret :: params) -> HeldAt m params held ->
  state_ok W (program_of Prog argv read) iface H st ->
  Forall2 (fun w t => value_typed W (program_of Prog argv read) H w (app_subst m t)) ws params ->
  prim_outcome W (program_of Prog argv read) iface H (app_subst m ret)
    (M.Reference.run_prim_at (program_of Prog argv read) p held ws st).
Proof.
  intros W Prog argv read iface operation held params ret m H st ws
    Layouts Reps Bools Laws Supp Sig Ground Formed Held Valid Args.
  destruct (decode operation) as [p|] eqn:Decoded.
  - unfold Platform.PrimitiveSignature in Sig. rewrite Decoded in Sig.
    pose proof (decode_canonical _ _ Decoded) as ->.
    eapply primitive_safe_known; eassumption.
  - destruct st as [i s]. destruct Valid as [Ei _]. cbn in Ei. subst i.
    unfold M.Reference.run_prim_at, M.ReferenceHost.run_prim, Base.call, BaseCall.call, M.ReferenceHost.raw_primitive. rewrite Decoded, Supp.
    destruct Laws as [_ [_ [Unsupported _]]].
    specialize (Unsupported operation ws (M.external s) Decoded Supp).
    destruct (M.primitive iface _ operation ws (M.external s)) as [out next].
    cbn in Unsupported. subst out. destruct held; exact I.
Qed.
End PrimitiveSafety.
