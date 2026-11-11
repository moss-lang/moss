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
From NM Require Import Audit PublicTypeRead PublicTypeFacts PublicValueTyping
  PublicHeapTyping PublicBoolTyping PublicTagCheck.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Ltac in_list := cbn; repeat (first [left; reflexivity | right]).

(* The handle tags of lib/wasm.moss: the tag, its parameter, its raw payload. *)
Definition handle_tags : list (string * string * string) :=
  [("Ref", "Boxed", "StructRef"); ("Array", "Arrayed", "ArrayRef"); ("Addr", "Memory", "I32")].

Section Static.
Variables (W : A.world) (Prog : program).
Hypothesis Layouts : Platform.Layouts W.
Hypothesis Reps : Platform.Representations W Prog.

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

Lemma handle_distinct : forall n1 p1 h1 n2 p2 h2 s,
  In (n1, p1, h1) handle_tags -> In (n2, p2, h2) handle_tags ->
  A.LangSym W "wasm" A.NSName n1 s -> A.LangSym W "wasm" A.NSName n2 s -> h1 = h2.
Proof.
  intros n1 p1 h1 n2 p2 h2 s F1 F2 S1 S2.
  destruct (layout_raw _ _ _ _ F1 S1) as [r1 [R1 [_ [_ Pay1]]]].
  destruct (layout_raw _ _ _ _ F2 S2) as [r2 [R2 [_ [_ Pay2]]]].
  pose proof (payload_det W _ _ _ _ Pay1 Pay2) as E. injection E as <-.
  destruct Layouts as [_ [Distinct _]].
  exact (Distinct h1 h2 r1 (proj2 (handle_layout _ _ _ F1)) (proj2 (handle_layout _ _ _ F2)) R1 R2).
Qed.

Lemma ref_tags : forall array : bool,
  In ((if array then "Array" else "Ref"), (if array then "Arrayed" else "Boxed"),
      (if array then "ArrayRef" else "StructRef")) handle_tags.
Proof. destruct array; in_list. Qed.

Lemma ref_array_distinct : forall s,
  A.LangSym W "wasm" A.NSName "Ref" s -> A.LangSym W "wasm" A.NSName "Array" s -> False.
Proof.
  intros s R Ar. pose proof (handle_distinct _ _ _ _ _ _ s (ref_tags false) (ref_tags true) R Ar).
  discriminate.
Qed.

(* A Ref or Array symbol has its cast entry, at its own kind. *)
Lemma ref_cast : forall (array : bool) s,
  A.LangSym W "wasm" A.NSName (if array then "Array" else "Ref") s ->
  exists held raw, A.LangSym W "wasm" A.NSName (if array then "Arrayed" else "Boxed") held /\
    A.LangSym W "wasm" A.NSName (if array then "ArrayRef" else "StructRef") raw /\
    lookup_cast s (program_casts Prog) =
      Some (CastSpec array held (Some (raw, TMachine raw))).
Proof.
  intros array s Sym. destruct Reps as [_ [reference [arr [Ref [Arr Casts]]]]]. rewrite Casts.
  destruct array.
  - destruct Arr as [[n [t [h [Tag [Held [Raw ->]]]]]]|[None' _]]; [|exfalso; exact (None' s Sym)].
    pose proof (lang_sym_unique _ _ _ _ _ _ Sym Tag); subst n.
    exists t, h. split; [exact Held|]. split; [exact Raw|].
    destruct Ref as [[n' [t' [h' [Tag' [_ [_ ->]]]]]]|[_ ->]]; cbn.
    + destruct (Nat.eqb_spec s n') as [->|_]; [exfalso; exact (ref_array_distinct n' Tag' Sym)|].
      rewrite Nat.eqb_refl; reflexivity.
    + rewrite Nat.eqb_refl; reflexivity.
  - destruct Ref as [[n [t [h [Tag [Held [Raw ->]]]]]]|[None' _]]; [|exfalso; exact (None' s Sym)].
    pose proof (lang_sym_unique _ _ _ _ _ _ Sym Tag); subst n.
    exists t, h. split; [exact Held|]. split; [exact Raw|]. cbn. rewrite Nat.eqb_refl; reflexivity.
Qed.

Lemma addr_not_cast : forall s, A.LangSym W "wasm" A.NSName "Addr" s ->
  lookup_cast s (program_casts Prog) = None.
Proof.
  intros s Sym. destruct (lookup_cast s (program_casts Prog)) as [schema|] eqn:Found; [|reflexivity].
  destruct (cast_entry W Prog Reps s schema Found) as [array [_ [_ [Tag _]]]]. exfalso.
  assert (Addr : In ("Addr", "Memory", "I32") handle_tags) by in_list.
  pose proof (handle_distinct _ _ _ _ _ _ s (ref_tags array) Addr Tag Sym). destruct array; discriminate.
Qed.

Lemma bool_not_cast : A.LangItemsFormed W -> forall s, A.LangSym W "bool" A.NSName "Bool" s ->
  lookup_cast s (program_casts Prog) = None.
Proof.
  intros BoolLayout s Sym. destruct (lookup_cast s (program_casts Prog)) as [schema|] eqn:Found; [|reflexivity].
  destruct (cast_entry W Prog Reps s schema Found) as [array [_ [_ [Tag _]]]]. exfalso.
  destruct (layout_raw _ _ _ _ (ref_tags array) Tag) as [_ [_ [_ [Reqs _]]]].
  destruct (BoolLayout s Sym) as [_ [_ [_ [_ [_ [_ [_ [_ [Empty _]]]]]]]]]. exact (Reqs Empty).
Qed.

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
   (machine_subst supplies exactly these at the entry). *)
Definition MachinesGround : Prop :=
  forall symbol, In symbol (map fst (program_machines Prog)) ->
    app_subst m (TAbs symbol) = TMachine symbol.

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
  exists symbol, app_subst m t = TMachine symbol /\
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
  lookup_subst raw (PublicTypeRead.smap m args) = Some (TMachine raw).
Proof.
  intros name param handle class symbol args raw Tag Machine Formed Sym Raw.
  destruct (Formed _ _ _ _ _ Tag Sym eq_refl) as [_ Raws].
  rewrite lookup_subst_nlook, nlook_smap, (Raws raw Raw). cbn. f_equal.
  apply Ground. eapply machine_lookup_in. eapply represented_machine; eassumption.
Qed.

Lemma keys_smap : forall (args : list (nat * ty)), map fst (PublicTypeRead.smap m args) = map fst args.
Proof. intros args. unfold PublicTypeRead.smap. rewrite map_map. reflexivity. Qed.

Lemma machine_value : forall H w symbol class,
  A.nlook symbol (program_machines Prog) = Some class ->
  value_typed W P H w (TMachine symbol) ->
  match class with
  | MScalar 0 => exists n, w = VI32 n | MScalar 1 => exists n, w = VI64 n
  | MScalar 2 => exists n, w = VF32 n | MScalar 3 => exists n, w = VF64 n
  | _ => True end.
Proof.
  intros H w symbol class Class Typed. inversion Typed; subst;
    match goal with Read : A.nlook _ (view_machines _) = Some _ |- _ =>
      change (view_machines P) with (program_machines Prog) in Read;
      rewrite Class in Read; injection Read as ->; cbn; eauto
    end.
Qed.

Lemma ref_machine : forall array : bool,
  In ((if array then "ArrayRef" else "StructRef"), Platform.ref_class array) Platform.initial_machines.
Proof. destruct array; in_list. Qed.

(* A typed reference argument is a tagged handle the ghost heap types at
   the reference's held type. *)
Lemma ref_input : forall H array t held w, Platform.ReferenceType W array t held -> HandleFormed t ->
  value_typed W P H w (app_subst m t) ->
  exists s a, (w = VTag s (VHandle a) \/ w = VTag s (VCheckedHandle array (app_subst m held) a)) /\
    H a = Some (array, app_subst m held).
Proof.
  intros H array t held w [symbol [param [args [Sym [Param [-> Held]]]]]] Formed Typed.
  destruct (ref_cast W Prog Layouts Reps array symbol Sym) as [heldsym [raw [HeldSym [RawSym Cast]]]].
  pose proof (raw_ground _ _ _ _ symbol args raw (ref_tags array) (ref_machine array) Formed Sym RawSym) as Enabled.
  destruct (layout_raw W Layouts _ _ _ symbol (ref_tags array) Sym) as [_ [_ [Kind _]]].
  rewrite subst_nom in Typed. inversion Typed; subst.
  - exfalso. match goal with Ord : ordinary_tag _ _ _ |- _ => unfold ordinary_tag in Ord;
      cbn [view_casts program_of] in Ord; rewrite Cast in Ord; unfold cast_enabled, type_argument in Ord;
      cbn [cast_handle] in Ord; rewrite Enabled in Ord end.
    match goal with Ord : ty_eqb ?x ?x = false |- _ =>
      rewrite (proj2 (ty_eqb_spec x x) eq_refl) in Ord; discriminate end.
  - congruence.
  - match goal with Found : lookup_cast _ (view_casts _) = Some _ |- _ =>
      cbn [view_casts program_of] in Found; rewrite Cast in Found; injection Found as <- end.
    cbn [cast_held cast_array] in *.
    pose proof (lang_sym_unique _ _ _ _ _ _ HeldSym Param); subst heldsym.
    match goal with Look : lookup_subst param (PublicTypeRead.smap m args) = Some _ |- _ =>
      rewrite lookup_subst_nlook, nlook_smap, Held in Look; injection Look as <- end.
    match goal with Handle : handle_typed _ _ _ _ |- _ => inversion Handle; subst end;
      do 2 eexists; split; [left; reflexivity|eassumption|right; reflexivity|eassumption].
Qed.

(* A fresh handle at a reference type, read through its checked header. *)
Lemma ref_output : forall H array t held s hv, Platform.ReferenceType W array t held -> HandleFormed t ->
  A.LangSym W "wasm" A.NSName (if array then "Array" else "Ref") s ->
  handle_typed H array (app_subst m held) hv ->
  value_typed W P H (VTag s hv) (app_subst m t).
Proof.
  intros H array t held s hv [symbol [param [args [Sym [Param [-> Held]]]]]] Formed S Handle.
  pose proof (lang_sym_unique _ _ _ _ _ _ S Sym); subst s.
  destruct (ref_cast W Prog Layouts Reps array symbol Sym) as [heldsym [raw [HeldSym [RawSym Cast]]]].
  pose proof (lang_sym_unique _ _ _ _ _ _ HeldSym Param); subst heldsym.
  pose proof (raw_ground _ _ _ _ symbol args raw (ref_tags array) (ref_machine array) Formed Sym RawSym) as Enabled.
  destruct (layout_raw W Layouts _ _ _ symbol (ref_tags array) Sym) as [raw' [Raw' [_ [_ Identity]]]].
  pose proof (lang_sym_unique _ _ _ _ _ _ Raw' RawSym); subst raw'.
  destruct (Formed _ _ _ _ _ (ref_tags array) Sym eq_refl) as [Keys _].
  pose proof (payload_at_args W symbol (PublicTypeRead.smap m args) raw ltac:(rewrite keys_smap; exact Keys) Identity) as Pay.
  cbn [app_subst] in Pay. rewrite Enabled in Pay.
  rewrite subst_nom.
  eapply (TypedHeapTag W P H symbol (PublicTypeRead.smap m args) (CastSpec array param _) (app_subst m held)).
  - cbn [view_casts program_of]. exact Cast.
  - unfold cast_enabled, type_argument. cbn [cast_handle]. rewrite Enabled. apply ty_eqb_spec; reflexivity.
  - cbn [cast_held]. rewrite lookup_subst_nlook, nlook_smap, Held. reflexivity.
  - exact Pay.
  - constructor. eapply represented_machine; [exact Layouts|exact Reps|apply ref_machine|exact RawSym].
  - exact Handle.
Qed.

Lemma addr_input : forall H t w, Platform.NominalType W "wasm" "Addr" t -> HandleFormed t ->
  value_typed W P H w (app_subst m t) -> exists s n, w = VTag s (VI32 n).
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
      destruct (machine_value _ _ _ _ Class Inner) as [n ->]; eauto end.
  - congruence.
  - match goal with Found : lookup_cast _ (view_casts _) = Some _ |- _ =>
      cbn [view_casts program_of] in Found; rewrite (addr_not_cast W Prog Layouts Reps symbol Sym) in Found;
      discriminate end.
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
  destruct w as [|b inner| | | | | | |]; try contradiction.
  destruct inner as [|t' u| | | | | | |]; try contradiction. destruct u; try contradiction.
  destruct Shape as [Eb Et]. cbn in Eb, Et.
  destruct (proj1 Bools s Sym) as [truth [falsity [T [F [_ [TU [FU [_ [_ Payload]]]]]]]]].
  destruct (bool_syms s Sym) as [-> Names]. destruct (Names _ _ T F) as [-> ->]. subst b.
  assert (Ord : ordinary_tag P (program_bool Prog) []).
  { unfold ordinary_tag. cbn [view_casts program_of].
    rewrite (bool_not_cast W Prog Layouts Reps (proj1 Bools) _ Sym). exact I. }
  destruct Payload as [Pay|Pay]; (eapply TypedTag; [exact Ord|exact Pay|]);
    destruct Et as [->| ->]; (eapply TypedUnion; [apply TypedUnitTag; eassumption|in_list]).
Qed.

(* A typed argument has the shape its platform signature names. *)
Lemma shape_param : forall H t s w, Platform.ValueType W t s -> HandleFormed t ->
  value_typed W P H w (app_subst m t) -> value_shape P w s.
Proof.
  intros H t s w VT Formed Typed. destruct s; cbn in VT.
  - destruct (machine_ground "I32" Platform.i32_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (machine_value _ _ _ _ Class Typed) as [n ->]; exact I.
  - destruct (machine_ground "I64" Platform.i64_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (machine_value _ _ _ _ Class Typed) as [n ->]; exact I.
  - destruct (machine_ground "F32" Platform.f32_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (machine_value _ _ _ _ Class Typed) as [n ->]; exact I.
  - destruct (machine_ground "F64" Platform.f64_class t ltac:(in_list) VT) as [symbol [Read Class]].
    rewrite Read in Typed. destruct (machine_value _ _ _ _ Class Typed) as [n ->]; exact I.
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
    destruct w; try contradiction; constructor; exact Class.
  - destruct (machine_ground "I64" Platform.i64_class t ltac:(in_list) VT) as [symbol [-> Class]].
    destruct w; try contradiction; constructor; exact Class.
  - destruct (machine_ground "F32" Platform.f32_class t ltac:(in_list) VT) as [symbol [-> Class]].
    destruct w; try contradiction; constructor; exact Class.
  - destruct (machine_ground "F64" Platform.f64_class t ltac:(in_list) VT) as [symbol [-> Class]].
    destruct w; try contradiction; constructor; exact Class.
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

Lemma signature_cases : forall W p params ret, Platform.PrimitiveSignature W p params ret ->
  (exists c, p = PUnsupported c) \/
  Platform.Signature W params ret (prim_sig p) \/ Platform.Signature W params ret (prim_msig p) \/
  Platform.GCSignature W p params ret.
Proof.
  intros W p params ret Sig. destruct p;
    first [left; eexists; reflexivity | right; exact Sig].
Qed.

(* The closure's held header, as primitive_closure records it: the ground
   receiver type of the `boxed`/`arrayed` method. *)
Definition HeldAt (m : subst) (params : list ty) (held : option ty) : Prop :=
  match held with
  | None => True
  | Some tau => exists first rest, params = first :: rest /\ tau = app_subst m first
  end.

Module PrimitiveSafety (Host : HOST) (M0 : PUBLIC Host).
Module M := M0.

(* HeapTyping.HeapSafety's allocation lemmas, at this instance of Public
   (functor instances have distinct interface records). *)
Lemma allocation_preserves_heap : forall W P iface H state p args held raw next tag (array : bool),
  M.interface_laws P iface -> M.supports iface p = true ->
  M.heap_ok iface (heap_predicates W P H) state ->
  Heap.gc_pre (heap_predicates W P H) p args (fun value => value_typed W P H value held) ->
  (forall Psi Psi' Q w, Heap.gc_post Psi Psi' p Q w ->
     Heap.hext Psi Psi' /\ exists a, w = VTag tag (VHandle a) /\ Psi a = None /\ Psi' a = Some (array, Q)) ->
  M.primitive iface P p args state = (Normal raw, next) ->
  exists H' address,
    heap_extends H H' /\ M.heap_ok iface (heap_predicates W P H') next /\
    raw =VTag tag (VHandle address) /\ H' address = Some (array, held).
Proof.
  intros W P iface H state p args held raw next tag array Laws Supported Valid Pre Post Run.
  destruct Laws as [_ [_ [_ [Weaken [_ [_ GC]]]]]].
  specialize (GC p args state (heap_predicates W P H) _ Supported Valid Pre).
  rewrite Run in GC.
  destruct GC as [extended [ValidNext PostRaw]].
  destruct (Post _ _ _ _ PostRaw) as [Extends [fresh [Raw [Absent Fresh]]]].
  assert (FreshH : H fresh = None).
  { unfold heap_predicates in Absent.
    destruct (H fresh) as [[kind tau]|]; [discriminate|reflexivity]. }
  exists (heap_add H fresh array held), fresh. split.
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
  w = VTag s (VHandle a) \/ w = VTag s (VCheckedHandle k h a) ->
  BaseCall.raw_receiver w = VTag s (VHandle a).
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
  M.supports iface p = true ->
  M.Reference.run_prim_at P p held ws (iface, s) =
    (let '(out, next) := M.primitive iface P p (BaseCall.primitive_arguments p ws) (M.external s) in
     (match held with Some tau => BaseCall.allocation_result p tau out | None => out end,
      (iface, M.World (M.arguments s) (M.read_file s) next))).
Proof.
  intros P iface p held ws s Supp. unfold M.Reference.run_prim_at, M.ReferenceHost.run_prim, BaseCall.call, M.ReferenceHost.raw_primitive.
  rewrite Supp. destruct (M.primitive iface P p (BaseCall.primitive_arguments p ws) (M.external s)); reflexivity.
Qed.

(* A non-allocating GC operation whose precondition holds at the ghost heap. *)
Lemma gc_plain : forall W P iface H s p held ws Q t,
  M.interface_laws P iface -> M.supports iface p = true ->
  M.heap_ok iface (heap_predicates W P H) (M.external s) ->
  Heap.gc_pre (heap_predicates W P H) p (BaseCall.primitive_arguments p ws) Q ->
  (forall extended w, Heap.gc_post (heap_predicates W P H) extended p Q w -> value_typed W P H w t) ->
  (forall tau o, BaseCall.allocation_result p tau o = o) ->
  prim_outcome W P iface H t (M.Reference.run_prim_at P p held ws (iface, s)).
Proof.
  intros W P iface H s p held ws Q t Laws Supp Valid Pre Post Plain.
  rewrite run_supported by exact Supp.
  destruct Laws as [_ [_ [_ [Weaken [_ [_ GC]]]]]].
  specialize (GC p (BaseCall.primitive_arguments p ws) (M.external s) _ Q Supp Valid Pre).
  destruct (M.primitive iface P p (BaseCall.primitive_arguments p ws) (M.external s)) as [out next].
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
  Heap.href (heap_predicates W P H) k (VTag s (VHandle a)) (fun v => value_typed W P H v held).
Proof. intros W P H k s a held Found. exists s, a. split; [reflexivity|]. unfold heap_predicates. rewrite Found. reflexivity. Qed.

Theorem gc_safe : forall W Prog argv read iface p held params ret m H s ws,
  Platform.Layouts W -> Platform.Representations W Prog ->
  M.interface_laws (program_of Prog argv read) iface -> M.supports iface p = true ->
  Platform.GCSignature W p params ret ->
  MachinesGround Prog m -> Forall (HandleFormed W) (ret :: params) -> HeldAt m params held ->
  M.heap_ok iface (heap_predicates W (program_of Prog argv read) H) (M.external s) ->
  Forall2 (fun w t => value_typed W (program_of Prog argv read) H w (app_subst m t)) ws params ->
  prim_outcome W (program_of Prog argv read) iface H (app_subst m ret)
    (M.Reference.run_prim_at (program_of Prog argv read) p held ws (iface, s)).
Proof.
  intros W Prog argv read iface p held params ret m H s ws Layouts Reps Laws Supp Sig Ground Formed Held Valid Args.
  set (P := program_of Prog argv read) in *.
  assert (I32 : forall H t w, Platform.MachineType W "I32" t -> value_typed W P H w (app_subst m t) ->
    exists n, w = VI32 n).
  { intros H0 t w MT Typed.
    destruct (machine_ground W Prog Layouts Reps m Ground "I32" Platform.i32_class t ltac:(in_list) MT) as [symbol [Read Class]].
    rewrite Read in Typed. exact (machine_value W Prog argv read _ _ _ _ Class Typed). }
  destruct p; try (exfalso; exact Sig).
  - (* PBox *) destruct params as [|heldt [|]]; try (exfalso; exact Sig). destruct Sig as [Sym RT].
    args_inv. rename x into v.
    rewrite run_supported by exact Supp. cbn [BaseCall.primitive_arguments].
    pose proof Laws as Laws'. destruct Laws' as [_ [_ [_ [_ [_ [_ GC]]]]]].
    specialize (GC (PBox n) [v] (M.external s) _ (fun w => value_typed W P H w (app_subst m heldt)) Supp Valid ltac:(assumption)).
    destruct (M.primitive iface P (PBox n) [v] (M.external s)) as [out next] eqn:Run.
    destruct out; try contradiction; [|destruct held; exact I].
    destruct (allocation_preserves_heap W P iface H (M.external s) (PBox n) [v] (app_subst m heldt) v0 next n false
      Laws Supp Valid ltac:(assumption) ltac:(intros ? ? ? ? [E X]; exact (conj E X)) Run) as [H' [a [Ext [Ok' [-> Cell]]]]].
    assert (Typed : forall hv, handle_typed H' false (app_subst m heldt) hv ->
      value_typed W P H' (VTag n hv) (app_subst m ret)).
    { intros hv Hv. exact (ref_output W Prog argv read Layouts Reps m Ground H' false ret heldt n hv RT ltac:(assumption) Sym Hv). }
    destruct held as [tau|]; cbn;
      (exists H'; split; [exact Ext|]; split; [split; [reflexivity|exact Ok']|]).
    + destruct Held as [first [rest [E ->]]]. injection E as <- <-. apply Typed. constructor. exact Cell.
    + apply Typed. constructor. exact Cell.
  - (* PDeref *) destruct params as [|reference [|]]; try (exfalso; exact Sig). args_inv.
    destruct (ref_input W Prog argv read Layouts Reps m Ground H false reference ret x Sig ltac:(assumption) ltac:(assumption))
      as [s0 [a [Raw Found]]].
    apply (gc_plain W P iface H s PDeref held [x] (fun v => value_typed W P H v (app_subst m ret))); auto.
    + cbn. rewrite (raw_receiver_of _ _ _ _ _ Raw). apply href_of. exact Found.
    + intros extended w [_ Q]. exact Q.
  - (* PRefSet *) destruct params as [|reference [|heldt [|]]]; try (exfalso; exact Sig). destruct Sig as [RT ->]. args_inv.
    destruct (ref_input W Prog argv read Layouts Reps m Ground H false reference heldt x RT ltac:(assumption) ltac:(assumption))
      as [s0 [a [Raw Found]]].
    apply (gc_plain W P iface H s PRefSet held [x; y] (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + cbn. rewrite (raw_receiver_of _ _ _ _ _ Raw). split; [apply href_of; exact Found|assumption].
    + intros extended w [_ ->]. constructor.
  - (* PArrNew *) destruct params as [|heldt [|count [|]]]; try (exfalso; exact Sig). destruct Sig as [Sym [Count RT]].
    args_inv. rename x into v.
    destruct (I32 H count y Count ltac:(assumption)) as [c ->].
    rewrite run_supported by exact Supp. cbn [BaseCall.primitive_arguments].
    pose proof Laws as Laws'. destruct Laws' as [_ [_ [_ [_ [_ [_ GC]]]]]].
    specialize (GC (PArrNew n) [v; VI32 c] (M.external s) _ (fun w => value_typed W P H w (app_subst m heldt)) Supp Valid ltac:(assumption)).
    destruct (M.primitive iface P (PArrNew n) [v; VI32 c] (M.external s)) as [out next] eqn:Run.
    destruct out; try contradiction; [|destruct held; exact I].
    destruct (allocation_preserves_heap W P iface H (M.external s) (PArrNew n) [v; VI32 c] (app_subst m heldt) v0 next n true
      Laws Supp Valid ltac:(assumption) ltac:(intros ? ? ? ? [E X]; exact (conj E X)) Run) as [H' [a [Ext [Ok' [-> Cell]]]]].
    assert (Typed : forall hv, handle_typed H' true (app_subst m heldt) hv ->
      value_typed W P H' (VTag n hv) (app_subst m ret)).
    { intros hv Hv. exact (ref_output W Prog argv read Layouts Reps m Ground H' true ret heldt n hv RT ltac:(assumption) Sym Hv). }
    destruct held as [tau|]; cbn;
      (exists H'; split; [exact Ext|]; split; [split; [reflexivity|exact Ok']|]).
    + destruct Held as [first [rest [E ->]]]. injection E as <- <-. apply Typed. constructor. exact Cell.
    + apply Typed. constructor. exact Cell.
  - (* PArrLen *) destruct params as [|array [|]]; try (exfalso; exact Sig). destruct Sig as [MT [heldt RT]]. args_inv.
    destruct (ref_input W Prog argv read Layouts Reps m Ground H true array heldt x RT ltac:(assumption) ltac:(assumption))
      as [s0 [a [Raw Found]]].
    apply (gc_plain W P iface H s PArrLen held [x] (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + cbn. rewrite (raw_receiver_of _ _ _ _ _ Raw). apply href_of. exact Found.
    + intros extended w [_ [k ->]].
      destruct (machine_ground W Prog Layouts Reps m Ground "I32" Platform.i32_class ret ltac:(in_list) MT) as [symbol [-> Class]].
      constructor. exact Class.
  - (* PArrGet *) destruct params as [|array [|index [|]]]; try (exfalso; exact Sig). destruct Sig as [MT RT]. args_inv.
    destruct (ref_input W Prog argv read Layouts Reps m Ground H true array ret x RT ltac:(assumption) ltac:(assumption))
      as [s0 [a [Raw Found]]].
    destruct (I32 H index y MT ltac:(assumption)) as [k ->].
    apply (gc_plain W P iface H s PArrGet held [x; VI32 k] (fun v => value_typed W P H v (app_subst m ret))); auto.
    + cbn. rewrite (raw_receiver_of _ _ _ _ _ Raw). apply href_of. exact Found.
    + intros extended w [_ Q]. exact Q.
  - (* PArrSet *) destruct params as [|array [|index [|heldt [|]]]]; try (exfalso; exact Sig).
    destruct Sig as [MT [RT ->]]. args_inv.
    destruct (ref_input W Prog argv read Layouts Reps m Ground H true array heldt x RT ltac:(assumption) ltac:(assumption))
      as [s0 [a [Raw Found]]].
    destruct (I32 H index y MT ltac:(assumption)) as [k ->].
    apply (gc_plain W P iface H s PArrSet held [x; VI32 k; y0] (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + cbn. rewrite (raw_receiver_of _ _ _ _ _ Raw). split; [apply href_of; exact Found|assumption].
    + intros extended w [_ ->]. constructor.
  - (* PArrFill *) destruct params as [|array [|index [|heldt [|count [|]]]]]; try (exfalso; exact Sig).
    destruct Sig as [MI [MC [RT ->]]]. args_inv.
    destruct (ref_input W Prog argv read Layouts Reps m Ground H true array heldt x RT ltac:(assumption) ltac:(assumption))
      as [s0 [a [Raw Found]]].
    destruct (I32 H index y MI ltac:(assumption)) as [k ->].
    destruct (I32 H count y1 MC ltac:(assumption)) as [c ->].
    apply (gc_plain W P iface H s PArrFill held [x; VI32 k; y0; VI32 c]
      (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + cbn. rewrite (raw_receiver_of _ _ _ _ _ Raw). split; [apply href_of; exact Found|assumption].
    + intros extended w [_ ->]. constructor.
  - (* PArrCopy *) destruct params as [|dst [|di [|src [|si [|count [|]]]]]]; try (exfalso; exact Sig).
    destruct Sig as [MDI [MSI [MC [-> [heldt [RD RS]]]]]]. args_inv.
    destruct (ref_input W Prog argv read Layouts Reps m Ground H true dst heldt x RD ltac:(assumption) ltac:(assumption))
      as [s0 [a [Raw Found]]].
    destruct (ref_input W Prog argv read Layouts Reps m Ground H true src heldt y0 RS ltac:(assumption) ltac:(assumption))
      as [s1 [b [Raw' Found']]].
    destruct (I32 H di y MDI ltac:(assumption)) as [k ->].
    destruct (I32 H si y1 MSI ltac:(assumption)) as [j ->].
    destruct (I32 H count y2 MC ltac:(assumption)) as [c ->].
    apply (gc_plain W P iface H s PArrCopy held [x; VI32 k; y0; VI32 j; VI32 c]
      (fun v => value_typed W P H v (app_subst m heldt))); auto.
    + cbn. rewrite (raw_receiver_of _ _ _ _ _ Raw), (raw_receiver_of _ _ _ _ _ Raw'). split; [apply href_of; exact Found|].
      eexists; split; [apply href_of; exact Found'|auto].
    + intros extended w [_ ->]. constructor.
Qed.

(* T11/T17: a primitive closure is a typed call. *)
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
  intros W Prog argv read iface p held params ret m H [i s] ws Layouts Reps Bools Laws Supp
    Sig Ground Formed Held [Ei Valid] Args.
  cbn in Ei, Valid. subst i.
  destruct (signature_cases W p params ret Sig) as [[c ->]|[Plain|[Plain|GCSig]]];
    [| | |exact (gc_safe W Prog argv read iface p held params ret m H s ws Layouts Reps Laws Supp
                    GCSig Ground Formed Held Valid Args)].
  - rewrite run_supported by exact Supp. destruct Laws as [_ [_ [Unsupported _]]].
    specialize (Unsupported c (BaseCall.primitive_arguments (PUnsupported c) ws) (M.external s) Supp).
    destruct (M.primitive iface _ (PUnsupported c) _ (M.external s)) as [out next].
    cbn in Unsupported. subst out. destruct held; exact I.
  - assert (GC : Heap.is_gc p = false) by exact (signature_plain W params ret p _ (or_introl eq_refl) Plain).
    destruct Plain as [ins [out [Sig' [VTs VT]]]].
    destruct Laws as [Law [_ [_ [_ [_ [Keep _]]]]]].
    pose proof (Law p ins out ws (M.external s) Supp Sig'
      (shapes_params W Prog argv read Layouts Reps Bools m Ground H params ins ws VTs
        (Forall_inv_tail Formed) Args)) as Result.
    pose proof (Keep p ws (M.external s) _ Supp GC Valid) as KeepH.
    destruct (output_shape p ins out (or_introl Sig')) as [NA NT].
    rewrite run_supported by exact Supp. rewrite args_plain by exact GC.
    destruct (M.primitive iface _ p ws (M.external s)) as [o next].
    cbn in Result, KeepH. destruct held; [rewrite alloc_plain by exact GC|];
      destruct o; try contradiction; try exact I;
      (exists H; split; [apply heap_extends_refl|]; split; [split; [reflexivity|exact KeepH]|];
       eapply shape_result; eassumption).
  - assert (GC : Heap.is_gc p = false) by exact (signature_plain W params ret p _ (or_intror eq_refl) Plain).
    destruct Plain as [ins [out [Sig' [VTs VT]]]].
    destruct Laws as [_ [Law [_ [_ [_ [Keep _]]]]]].
    pose proof (Law p ins out ws (M.external s) Supp Sig'
      (shapes_params W Prog argv read Layouts Reps Bools m Ground H params ins ws VTs
        (Forall_inv_tail Formed) Args)) as Result.
    pose proof (Keep p ws (M.external s) _ Supp GC Valid) as KeepH.
    destruct (output_shape p ins out (or_intror Sig')) as [NA NT].
    rewrite run_supported by exact Supp. rewrite args_plain by exact GC.
    destruct (M.primitive iface _ p ws (M.external s)) as [o next].
    cbn in Result, KeepH. destruct held; [rewrite alloc_plain by exact GC|];
      destruct o; try contradiction; try exact I;
      (exists H; split; [apply heap_extends_refl|]; split; [split; [reflexivity|exact KeepH]|];
       eapply shape_result; eassumption).
Qed.
End PrimitiveSafety.
