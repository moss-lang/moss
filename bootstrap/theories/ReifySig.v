(* The runtime signature is computed from the reifier's real declarations.
   No generated signature is recovered from a fabricated source AST. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss ReifyData.
Import ListNotations.

Fixpoint lookup_reify_decl (i : item) (ds : list (item * reify_decl))
    : option reify_decl :=
  match ds with
  | [] => None
  | (j,d) :: rest => if item_eqb i j then Some d else lookup_reify_decl i rest
  end.

Fixpoint lookup_reify_method (m : msym) (ds : list (item * reify_decl))
    : option (tele * fnsig) :=
  match ds with
  | [] => None
  | (IMth _ n _, RDMethod D signature) :: rest =>
      if Nat.eqb m n then Some (D,signature) else lookup_reify_method m rest
  | _ :: rest => lookup_reify_method m rest
  end.

Definition lookup_reify_functor (u : usym) (ds : list (item * reify_decl))
    : option reify_functor_decl :=
  match lookup_reify_decl (IFun u) ds with
  | Some (RDFunctor schema) => Some schema
  | _ => None
  end.

Definition reify_fixed_inst (ds : list (item * reify_decl)) (u : usym) : subst :=
  match lookup_reify_functor u ds with
  | Some schema => rfd_fixed_inst schema
  | None => []
  end.

(* Actual immutable fixed keys remain separate from the Core erasure. *)
Definition reify_fixed_keys (ds : list (item * reify_decl)) (u : usym) : list tsym :=
  match lookup_reify_functor u ds with
  | Some schema => rfd_fixed schema | None => [] end.

Definition reify_signature_overlay (base : gsig)
    (ds : list (item * reify_decl)) : gsig :=
  mkGsigInterfaces
    (fun t => match lookup_reify_decl (ITy t) ds with
      | Some RDType => true | _ => g_ty base t end)
    (g_tag base)
    (fun v => match lookup_reify_decl (IVal v) ds with
      | Some (RDValue D tau) => Some (D,tau) | _ => g_val base v end)
    (fun f => match lookup_reify_decl (IFn f) ds with
      | Some (RDFunction D signature) => Some (D,signature,None) | _ => g_fn base f end)
    (fun m => match lookup_reify_method m ds with
      | Some sig => Some sig | None => g_mth base m end)
    (fun u => match lookup_reify_decl (IFun u) ds with
      | Some (RDFunctor s) => Some (rfd_inputs s,rfd_outputs s,None)
      | Some (RDOpaqueFunctor _) => None
      | _ => g_fun base u end)
    (fun u => match lookup_reify_functor u ds with
      | Some _ => [] | None => g_fixed base u end)
    (fun u => match lookup_reify_functor u ds with
      | Some s => rfd_poly s | None => g_poly base u end)
    (fun u => match lookup_reify_functor u ds with
      | Some _ => ([],[]) | None => g_cap base u end)
    (g_opaque base)
    (fun u => match lookup_reify_decl (IFun u) ds with
      | Some (RDOpaqueFunctor _) => true
      | Some (RDFunctor _) => false
      | _ => g_opaque_fun base u end).

Lemma reify_signature_tags : forall base ds n,
    g_tag (reify_signature_overlay base ds) n = g_tag base n.
Proof. reflexivity. Qed.

Lemma reify_signature_function : forall base ds f D signature,
    lookup_reify_decl (IFn f) ds = Some (RDFunction D signature) ->
    g_fn (reify_signature_overlay base ds) f = Some (D,signature,None).
Proof. intros. cbn. rewrite H. reflexivity. Qed.

Lemma reify_signature_functor : forall base ds u s,
    lookup_reify_functor u ds = Some s ->
    g_fun (reify_signature_overlay base ds) u =
      Some (rfd_inputs s,rfd_outputs s,None) /\
    g_fixed (reify_signature_overlay base ds) u = [] /\
    g_poly (reify_signature_overlay base ds) u = rfd_poly s /\
    g_cap (reify_signature_overlay base ds) u = ([],[]).
Proof.
 intros base ds u s H.
 assert (Read:lookup_reify_decl (IFun u) ds=Some(RDFunctor s)).
 { unfold lookup_reify_functor in H.
   destruct(lookup_reify_decl (IFun u) ds)as[d|]eqn:E;[|discriminate].
   destruct d;try discriminate;inversion H;reflexivity. }
 cbn. rewrite Read. rewrite H. repeat split;reflexivity.
Qed.

Lemma reify_fixed_inst_functor : forall ds u schema,
    lookup_reify_functor u ds = Some schema ->
    reify_fixed_inst ds u = rfd_fixed_inst schema.
Proof. intros. unfold reify_fixed_inst. rewrite H. reflexivity. Qed.


(* Core abstract declarations capture nothing.  Executable provider capture
   and pins remain genuine metadata for the separate MossX body table. *)
Definition reify_body_table (P : xprog) (ds : list (item * reify_decl)) (h : usym)
    : option (tele * tele * subst) :=
  match xp_fun P h, lookup_reify_functor h ds with
  | Some _, Some schema =>
      Some (rfd_inputs schema,rfd_captures schema,rfd_pins schema)
  | _, _ => None
  end.

Lemma reify_body_table_provider : forall P ds h schema code,
    xp_fun P h = Some code -> lookup_reify_functor h ds = Some schema ->
    reify_body_table P ds h =
      Some (rfd_inputs schema,rfd_captures schema,rfd_pins schema).
Proof. intros. unfold reify_body_table. rewrite H,H0. reflexivity. Qed.

Lemma reify_fixed_keys_functor : forall ds u schema,
    lookup_reify_functor u ds = Some schema ->
    reify_fixed_keys ds u = rfd_fixed schema.
Proof. intros. unfold reify_fixed_keys. rewrite H. reflexivity. Qed.

(* Finite program tables retain every IFun row, including an empty row for
   a malformed declaration, so their first-match behavior is identical to
   the proof-level reader without any hidden uniqueness assumption. *)
Fixpoint reify_fixed_keys_table (ds : list (item * reify_decl))
    : list (usym * list tsym) :=
  match ds with
  | [] => []
  | (IFun u,d)::rest => (u,match d with RDFunctor s => rfd_fixed s | _ => [] end)
      ::reify_fixed_keys_table rest
  | _::rest => reify_fixed_keys_table rest
  end.
Fixpoint reify_fixed_inst_table (ds : list (item * reify_decl))
    : list (usym * subst) :=
  match ds with
  | [] => []
  | (IFun u,d)::rest => (u,match d with RDFunctor s => rfd_fixed_inst s | _ => [] end)
      ::reify_fixed_inst_table rest
  | _::rest => reify_fixed_inst_table rest
  end.

Lemma reify_fixed_keys_table_lookup : forall ds u,
    (match ExecutionModel.Elaboration.nlook u (reify_fixed_keys_table ds) with
     | Some keys => keys | None => [] end) = reify_fixed_keys ds u.
Proof.
  induction ds as [|[i d]rest IH]; intro u; [reflexivity|].
  destruct i as [t|v|f|rt m th|target];
    cbn [reify_fixed_keys_table reify_fixed_keys lookup_reify_functor
      lookup_reify_decl ExecutionModel.Elaboration.nlook item_eqb]; try exact (IH u).
  destruct (Nat.eqb u target) eqn:E.
  - destruct d; unfold reify_fixed_keys,lookup_reify_functor;
      cbn [lookup_reify_decl item_eqb]; rewrite E; reflexivity.
  - unfold reify_fixed_keys,lookup_reify_functor; cbn [lookup_reify_decl item_eqb].
    rewrite E. exact (IH u).
Qed.
Lemma reify_fixed_inst_table_lookup : forall ds u,
    (match ExecutionModel.Elaboration.nlook u (reify_fixed_inst_table ds) with
     | Some interpretation => interpretation | None => [] end) = reify_fixed_inst ds u.
Proof.
  induction ds as [|[i d]rest IH]; intro u; [reflexivity|].
  destruct i as [t|v|f|rt m th|target];
    cbn [reify_fixed_inst_table reify_fixed_inst lookup_reify_functor
      lookup_reify_decl ExecutionModel.Elaboration.nlook item_eqb]; try exact (IH u).
  destruct (Nat.eqb u target) eqn:E.
  - destruct d; unfold reify_fixed_inst,lookup_reify_functor;
      cbn [lookup_reify_decl item_eqb]; rewrite E; reflexivity.
  - unfold reify_fixed_inst,lookup_reify_functor; cbn [lookup_reify_decl item_eqb].
    rewrite E. exact (IH u).
Qed.

Lemma reify_program_fixed_keys : forall program ds argv read u,
    xo_fixed_keys program = reify_fixed_keys_table ds ->
    xp_fixed_keys (program_of program argv read) u = reify_fixed_keys ds u.
Proof.
  intros program ds argv read u H. cbn [program_of xp_fixed_keys].
  rewrite H. apply reify_fixed_keys_table_lookup.
Qed.
Lemma reify_program_fixed_inst : forall program ds argv read u,
    xo_fixed_inst program = reify_fixed_inst_table ds ->
    xp_fixed_inst (program_of program argv read) u = reify_fixed_inst ds u.
Proof.
  intros program ds argv read u H. cbn [program_of xp_fixed_inst].
  rewrite H. apply reify_fixed_inst_table_lookup.
Qed.

Lemma reify_signature_opaque_functor : forall base ds u original,
 lookup_reify_decl (IFun u) ds=Some(RDOpaqueFunctor original)->
 g_fun(reify_signature_overlay base ds)u=None /\
 g_opaque_fun(reify_signature_overlay base ds)u=true.
Proof. intros;cbn;rewrite H;split;reflexivity. Qed.
