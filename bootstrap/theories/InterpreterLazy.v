(* ------------------------------------------------------------------------ *)
(* InterpreterLazy.v: Audit's reference interpreter with deferred type           *)
(* substitution.                                                            *)
(*                                                                           *)
(* AuditBase.Semantics.Interpreter grounds every type an activation receives    *)
(* when the activation starts: `instantiate` maps `ground d` over a call's  *)
(* θ, `typed_capture` over a closure's captured generations and aliases,    *)
(* `application_environment` over a functor's inputs. Most of those types   *)
(* are never observed, since only a cast (check_tag) and an allocating      *)
(* primitive (primitive_closure) read a type. On the self-compile this      *)
(* grounding was about a third of the run time.                              *)
(*                                                                           *)
(* Here the same interpreter, clause for clause, keeps each type meaning as *)
(* a suspension `dty`, a coinductive cell that Rocq's extraction compiles   *)
(* to an OCaml `lazy`, so a meaning is computed (once) only when something  *)
(* observes it. A suspension captures only the type meanings it reads,      *)
(* never provisions, locals or the machine state.                           *)
(*                                                                           *)
(* Proved, for every primitive implementation and every fuel:               *)
(*  - leval_agrees: on `to_env d`, the reference returns exactly what       *)
(*    `leval` returns on `d` (and likewise for arguments, bindings and      *)
(*    functor applications, whose environments are related by `to_env`);   *)
(*  - lrun_main_agrees: `lrun_main` on `lift_env d0` is `run_main` on d0.   *)
(* So the driver, which runs `lrun_main`, computes Audit's semantics, and   *)
(* every theorem about `Interpreter.run_main` carries over to it.           *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith String.
From NM Require AuditBase.
From NM Require Import Audit.
From NM Require Import PublicValueTyping.
Import AuditBase.Semantics ListNotations.

Local Open Scope nat_scope.

(* ===================== Suspended types ================================== *)

CoInductive dty : Type := Hold : ty -> dty.
Definition force (v : dty) : ty := match v with Hold t => t end.

(* A table of meanings is suspended as a whole too: building one (a
   call's θ, a capture, an append onto the callee's captured meanings)
   costs nothing until a type lookup first forces it. *)
CoInductive lsubst : Type := DS : list (tsym * dty) -> lsubst.
Definition dforce (s : lsubst) : list (tsym * dty) := match s with DS l => l end.
Definition dappend (a b : lsubst) : lsubst := DS (dforce a ++ dforce b).

Fixpoint llookup_rows (t : tsym) (s : list (tsym * dty)) : option dty :=
  match s with
  | [] => None
  | (t', v) :: r => if Nat.eqb t t' then Some v else llookup_rows t r
  end.
Definition llookup (t : tsym) (s : lsubst) : option dty := llookup_rows t (dforce s).

Definition lget (s : lsubst) (t : tsym) : option ty :=
  match llookup t s with Some v => Some (force v) | None => None end.

(* app_subst, reading a symbol's meaning through `get`. *)
Fixpoint gapp (get : tsym -> option ty) (tau : ty) {struct tau} : ty :=
  match tau with
  | TUnit => TUnit
  | TMachine m => TMachine m
  | TRecord fs => TRecord ((fix go (fs : list (string * ty)) :=
      match fs with [] => [] | (f, t) :: rest => (f, gapp get t) :: go rest end) fs)
  | TAbs t => match get t with Some tau' => tau' | None => TAbs t end
  | TNom n th =>
      TNom n ((fix go (l : list (tsym * ty)) : list (tsym * ty) :=
                 match l with
                 | [] => []
                 | p :: r => (fst p, gapp get (snd p)) :: go r
                 end) th)
  | TUnion ms =>
      TUnion ((fix go (l : list ty) : list ty :=
                 match l with
                 | [] => []
                 | t' :: r => gapp get t' :: go r
                 end) ms)
  end.

Definition lapp (s : lsubst) (tau : ty) : ty := gapp (lget s) tau.

(* The suspension of `lapp s tau`: it holds s and tau only. *)
CoFixpoint ldefer (s : lsubst) (tau : ty) : dty := Hold (lapp s tau).

Definition to_subst (s : lsubst) : subst := map (fun q => (fst q, force (snd q))) (dforce s).
Definition lift_subst (s : subst) : lsubst := DS (map (fun q => (fst q, Hold (snd q))) s).

Lemma force_ldefer : forall s tau, force (ldefer s tau) = lapp s tau.
Proof. reflexivity. Qed.

Lemma lookup_to_subst : forall t s, lookup_subst t (to_subst s) = lget s t.
Proof.
  intros t [l]. unfold to_subst, lget, llookup. cbn [dforce].
  induction l as [|[t' v] r IH]; [reflexivity|].
  cbn. destruct (Nat.eqb t t'); [reflexivity|exact IH].
Qed.

Lemma gapp_spec : forall s get, (forall t, lookup_subst t s = get t) ->
  forall tau, app_subst s tau = gapp get tau.
Proof.
  intros s get H. fix IH 1. intros tau. destruct tau as [m|fs| |t|n th|ms]; cbn.
  - reflexivity.
  - f_equal. induction fs as [|[f t] rest IHfs]; [reflexivity|]. cbn. rewrite (IH t). f_equal. exact IHfs.
  - reflexivity.
  - rewrite H. reflexivity.
  - f_equal. induction th as [|[a t] rest IHth]; [reflexivity|]. cbn. rewrite (IH t). f_equal. exact IHth.
  - f_equal. induction ms as [|t rest IHms]; [reflexivity|]. cbn. rewrite (IH t). f_equal. exact IHms.
Qed.

Lemma lapp_spec : forall s tau, app_subst (to_subst s) tau = lapp s tau.
Proof. intros s tau. apply gapp_spec. intro t. apply lookup_to_subst. Qed.

Lemma to_lift_subst : forall s, to_subst (lift_subst s) = s.
Proof.
  intro s. unfold to_subst, lift_subst. cbn [dforce].
  induction s as [|[t tau] r IH]; [reflexivity|]. cbn. f_equal. exact IH.
Qed.

(* ===================== Runtime environments ============================= *)

Inductive lprov : Type :=
| LValue : value -> lprov
| LFunction : fsym -> lenv -> lprov
| LFunctor : usym -> option smap -> lenv -> option (list (option nat)) -> lprov
| LPrimitive : op -> option ty -> lprov
with lenv : Type :=
| LEnv : lsubst -> lsubst -> list (item * lprov) -> lenv.

Definition lmeanings (d : lenv) := match d with LEnv ts _ _ => ts end.
Definition laliases (d : lenv) := match d with LEnv _ a _ => a end.
Definition lprovs (d : lenv) := match d with LEnv _ _ ps => ps end.

Fixpoint to_prov (p : lprov) : provision_value :=
  match p with
  | LValue w => ValueProvision w
  | LFunction f d => FunctionClosure f (to_env d)
  | LFunctor u a d q => FunctorClosure u a (to_env d) q
  | LPrimitive p o => PrimitiveClosure p o
  end
with to_env (d : lenv) : environment :=
  match d with
  | LEnv ts a ps => EnvironmentWithAliases (to_subst ts) (to_subst a)
      ((fix go (l : list (item * lprov)) : list (item * provision_value) :=
          match l with [] => [] | (i, p) :: r => (i, to_prov p) :: go r end) ps)
  end.

Definition to_provs (ps : list (item * lprov)) : list (item * provision_value) :=
  map (fun q => (fst q, to_prov (snd q))) ps.

Lemma to_env_eq : forall ts a ps,
  to_env (LEnv ts a ps) = EnvironmentWithAliases (to_subst ts) (to_subst a) (to_provs ps).
Proof.
  intros ts a ps. cbn [to_env]. f_equal.
  induction ps as [|[i p] r IH]; [reflexivity|]. cbn. f_equal. exact IH.
Qed.

Fixpoint lift_prov (p : provision_value) : lprov :=
  match p with
  | ValueProvision w => LValue w
  | FunctionClosure f d => LFunction f (lift_env d)
  | FunctorClosure u a d q => LFunctor u a (lift_env d) q
  | PrimitiveClosure p o => LPrimitive p o
  end
with lift_env (d : environment) : lenv :=
  match d with
  | EnvironmentWithAliases ts a ps => LEnv (lift_subst ts) (lift_subst a)
      ((fix go (l : list (item * provision_value)) : list (item * lprov) :=
          match l with [] => [] | (i, p) :: r => (i, lift_prov p) :: go r end) ps)
  end.

Fixpoint to_lift_prov (p : provision_value) : to_prov (lift_prov p) = p
with to_lift_env (d : environment) : to_env (lift_env d) = d.
Proof.
  - destruct p; cbn [lift_prov to_prov]; try reflexivity; rewrite to_lift_env; reflexivity.
  - destruct d as [ts a ps]. cbn [lift_env to_env]. rewrite !to_lift_subst. f_equal.
    induction ps as [|[i p] r IH]; [reflexivity|]. cbn. rewrite to_lift_prov. f_equal. exact IH.
Qed.

Definition lempty := LEnv (DS []) (DS []) [].
Definition lground (d : lenv) (tau : ty) : ty := lapp (lmeanings d) tau.
Definition lappend (a b : lenv) :=
  LEnv (dappend (lmeanings a) (lmeanings b)) (dappend (laliases a) (laliases b)) (lprovs a ++ lprovs b).
Definition linstall (key : item) (entry : lprov) (d : lenv) :=
  LEnv (lmeanings d) (laliases d) ((key, entry) :: lprovs d).
Definition ldefer_entries (s : lsubst) (th : subst) :=
  map (fun q => (fst q, ldefer s (snd q))) th.
Definition ldefer_all (s : lsubst) (th : subst) : lsubst := DS (ldefer_entries s th).
(* A call adds its instantiated parameters to each captured type table.
   Fuse the two deferred steps, and share the tail for a monomorphic call. *)
Definition ldefer_onto (s : lsubst) (th : subst) (tail : lsubst) : lsubst :=
  match th with [] => tail | _ => DS (ldefer_entries s th ++ dforce tail) end.
Definition linstantiate (d : lenv) (th : subst) : lenv :=
  let meanings := ldefer_all (lmeanings d) th in LEnv meanings meanings [].
Definition ltyped_capture (d : lenv) (types : type_capture) (dc : lenv) : lenv :=
  LEnv (ldefer_all (lmeanings d) (capture_generations types))
    (ldefer_all (lmeanings d) (capture_aliases types)) (lprovs dc).
Definition lprimitive_closure (d : lenv) (i : item) (p : op) : lprov :=
  LPrimitive p (match i with
    | IMth tau _ _ => Some (lapp (lmeanings d) tau)
    | _ => None end).
(* item_eqb with a method key's symbols compared before its receiver and
   θ, so a lookup passes over other methods' entries without comparing
   types. The same boolean (fast_item_eqb_spec). *)
Definition fast_item_eqb (a b : item) : bool :=
  match a, b with
  | IMth r1 m1 th1, IMth r2 m2 th2 => Nat.eqb m1 m2 && (ty_eqb r1 r2 && subst_eqb th1 th2)
  | _, _ => item_eqb a b
  end.
Lemma fast_item_eqb_spec : forall a b, fast_item_eqb a b = item_eqb a b.
Proof.
  intros [] []; try reflexivity. cbn.
  destruct (Nat.eqb _ _), (ty_eqb _ _), (subst_eqb _ _); reflexivity.
Qed.
Fixpoint llookup_entry (i : item) (entries : list (item * lprov)) : option lprov :=
  match entries with
  | [] => None
  | (key, value) :: rest => if fast_item_eqb i key then Some value else llookup_entry i rest
  end.
Definition llookup_provision (i : item) (d : lenv) := llookup_entry i (lprovs d).
Fixpoint lselect_provisions (d : lenv) (sm : smap) : option lenv :=
  match sm with
  | [] => Some lempty
  | (req, s) :: r =>
      match llookup_provision s d, lselect_provisions d r with
      | Some en, Some d' => Some (linstall req en d')
      | _, _ => None
      end
  end.
Definition ladapt_provisions (d : lenv) (a : option smap) : option lenv :=
  match a with
  | None => Some d
  | Some sm => lselect_provisions d sm
  end.
Definition lbind_type (d : lenv) (t : tsym) (tau : ty) : lenv :=
  LEnv (lmeanings d) (DS ((t, ldefer (lmeanings d) tau) :: dforce (laliases d))) (lprovs d).
Definition lcall_environment (d : lenv) (th : subst) (dc : lenv) : lenv :=
  LEnv (ldefer_onto (lmeanings d) th (lmeanings dc))
    (ldefer_onto (lmeanings d) th (laliases dc)) (lprovs dc).
Definition loutput_meaning (d : lenv) (source : tsym) : option dty :=
  match llookup source (laliases d) with
  | Some meaning => Some meaning
  | None => llookup source (lmeanings d)
  end.
Definition lbase (d : lenv) (linked : list tsym) (th : subst) (dc : lenv) (source : tsym) : dty :=
  match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
  | Some meaning => ldefer (lmeanings d) meaning
  | None => match loutput_meaning dc source with
    | Some meaning => meaning | None => Hold (TAbs source) end
  end.
Definition lapplication_environment (P : program_view) (d : lenv) (h : usym) (th : subst) (dc : lenv) : lenv :=
  let linked := match view_functor P h with Some code => fc_linked code | None => [] end in
  let inputs := match view_functor P h with Some code => fc_inputs code | None => [] end in
  let table := match view_functor P h with Some code => fc_table code | None => [] end in
  let bases := DS (map (fun source => (source, lbase d linked th dc source)) inputs) in
  let meanings := DS (map (fun source => (source,
    match lookup_subst source table with
    | Some t => ldefer bases t
    | None => lbase d linked th dc source
    end)) inputs) in
  LEnv meanings meanings (lprovs dc).
Fixpoint lread_result_types (d : lenv) (Dr : tele) (outputs : list (tsym * tsym))
    : option (list (tsym * dty) * list (tsym * dty)) :=
  match outputs with
  | [] => Some ([], [])
  | (source, destination) :: rest =>
      if existsb (item_eqb (ITy source)) Dr then
        match loutput_meaning d source, lread_result_types d Dr rest with
        | Some meaning, Some (writes, aliases) =>
            Some ((destination, meaning) :: writes, (source, meaning) :: aliases)
        | _, _ => None end
      else None
  end.
Definition lresult_environment (outputs : list (tsym * tsym))
    (d3 : lenv) (Dr : tele) (dr : lenv) : option lenv :=
  match lread_result_types d3 Dr outputs with
  | Some (writes, aliases) => Some (LEnv (DS writes) (DS aliases) (lprovs dr))
  | None => None
  end.

(* ===================== The operations commute with to_env ================ *)

Lemma meanings_to : forall d, type_meanings (to_env d) = to_subst (lmeanings d).
Proof. intros [ts a ps]. rewrite to_env_eq. reflexivity. Qed.
Lemma aliases_to : forall d, type_aliases (to_env d) = to_subst (laliases d).
Proof. intros [ts a ps]. rewrite to_env_eq. reflexivity. Qed.
Lemma provs_to : forall d, provision_values (to_env d) = to_provs (lprovs d).
Proof. intros [ts a ps]. rewrite to_env_eq. reflexivity. Qed.
Lemma to_env_fields : forall d,
  to_env d = EnvironmentWithAliases (to_subst (lmeanings d)) (to_subst (laliases d)) (to_provs (lprovs d)).
Proof. intros [ts a ps]. apply to_env_eq. Qed.

Lemma ground_to : forall d tau, ground (to_env d) tau = lground d tau.
Proof. intros d tau. unfold ground, lground. rewrite meanings_to. apply lapp_spec. Qed.

Lemma to_subst_app : forall a b, to_subst (dappend a b) = to_subst a ++ to_subst b.
Proof. intros. apply map_app. Qed.
Lemma to_provs_app : forall a b, to_provs (a ++ b) = to_provs a ++ to_provs b.
Proof. intros. apply map_app. Qed.

Lemma ldefer_all_to : forall s th,
  to_subst (ldefer_all s th) = map (fun q => (fst q, app_subst (to_subst s) (snd q))) th.
Proof.
  intros s th. unfold ldefer_all, ldefer_entries. unfold to_subst at 1. cbn [dforce]. rewrite map_map. apply map_ext.
  intros [t tau]. cbn [fst snd]. rewrite force_ldefer, lapp_spec. reflexivity.
Qed.

Lemma append_to : forall a b, append_environment (to_env a) (to_env b) = to_env (lappend a b).
Proof.
  intros a b. unfold append_environment, lappend.
  rewrite !meanings_to, !aliases_to, !provs_to, to_env_eq, !to_subst_app, to_provs_app. reflexivity.
Qed.

Lemma install_to : forall i p d,
  install_provision i (to_prov p) (to_env d) = to_env (linstall i p d).
Proof.
  intros i p d. unfold install_provision, linstall.
  rewrite meanings_to, aliases_to, provs_to, to_env_eq. reflexivity.
Qed.

Lemma instantiate_to : forall d th, instantiate (to_env d) th = to_env (linstantiate d th).
Proof.
  intros d th. unfold instantiate, linstantiate. rewrite to_env_eq, ldefer_all_to.
  unfold ground. rewrite meanings_to. reflexivity.
Qed.

Lemma call_to : forall d th dc,
  call_environment (to_env d) th (to_env dc) = to_env (lcall_environment d th dc).
Proof.
  intros d [|p th] dc.
  - destruct dc. reflexivity.
  - unfold call_environment. rewrite instantiate_to, append_to. reflexivity.
Qed.

Lemma typed_capture_to : forall d types dc,
  typed_capture (to_env d) types (to_env dc) = to_env (ltyped_capture d types dc).
Proof.
  intros d types dc. unfold typed_capture, ltyped_capture.
  rewrite to_env_eq, !ldefer_all_to, provs_to. unfold ground. rewrite meanings_to. reflexivity.
Qed.

Lemma bind_type_to : forall d t tau, bind_type (to_env d) t tau = to_env (lbind_type d t tau).
Proof.
  intros d t tau. unfold bind_type, lbind_type.
  rewrite to_env_eq, meanings_to, aliases_to, provs_to, ground_to. reflexivity.
Qed.

Lemma primitive_closure_to : forall d i p,
  primitive_closure (to_env d) i p = to_prov (lprimitive_closure d i p).
Proof.
  intros d i p. unfold primitive_closure, lprimitive_closure. cbn [to_prov].
  destruct i; rewrite ?ground_to; reflexivity.
Qed.

Lemma lookup_entry_to : forall i ps,
  lookup_entry i (to_provs ps) = option_map to_prov (llookup_entry i ps).
Proof.
  intros i ps. induction ps as [|[j p] r IH]; [reflexivity|].
  cbn. rewrite fast_item_eqb_spec. destruct (item_eqb i j); [reflexivity|exact IH].
Qed.

Lemma lookup_provision_to : forall i d,
  lookup_provision i (to_env d) = option_map to_prov (llookup_provision i d).
Proof. intros i d. unfold lookup_provision, llookup_provision. rewrite provs_to. apply lookup_entry_to. Qed.

Lemma select_to : forall d sm,
  select_provisions (to_env d) sm = option_map to_env (lselect_provisions d sm).
Proof.
  intros d sm. induction sm as [|[req s] r IH]; cbn.
  - reflexivity.
  - rewrite lookup_provision_to, IH.
    destruct (llookup_provision s d), (lselect_provisions d r); cbn; try reflexivity.
    rewrite install_to. reflexivity.
Qed.

Lemma adapt_to : forall d a,
  adapt_provisions (to_env d) a = option_map to_env (ladapt_provisions d a).
Proof. intros d [sm|]; [apply select_to|reflexivity]. Qed.

Lemma llookup_to : forall t s, lookup_subst t (to_subst s) = option_map force (llookup t s).
Proof.
  intros t [l]. unfold to_subst, llookup. cbn [dforce].
  induction l as [|[t' v] r IH]; [reflexivity|].
  cbn. destruct (Nat.eqb t t'); [reflexivity|exact IH].
Qed.

Lemma output_meaning_to : forall d source,
  output_meaning (to_env d) source = option_map force (loutput_meaning d source).
Proof.
  intros d source. unfold output_meaning, loutput_meaning.
  rewrite aliases_to, meanings_to, !llookup_to.
  destruct (llookup source (laliases d)); reflexivity.
Qed.

Lemma lbase_to : forall d linked th dc source,
  (match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
   | Some meaning => ground (to_env d) meaning
   | None => match output_meaning (to_env dc) source with
     | Some meaning => meaning | None => TAbs source end
   end) = force (lbase d linked th dc source).
Proof.
  intros d linked th dc source. unfold lbase. rewrite output_meaning_to.
  destruct (if existsb (Nat.eqb source) linked then lookup_subst source th else None).
  - rewrite force_ldefer. apply ground_to.
  - destruct (loutput_meaning dc source); reflexivity.
Qed.

Lemma application_environment_to : forall P d h th dc,
  application_environment P (to_env d) h th (to_env dc) = to_env (lapplication_environment P d h th dc).
Proof.
  intros P d h th dc. unfold application_environment, lapplication_environment. cbv zeta.
  rewrite to_env_eq, provs_to.
  generalize (match view_functor P h with Some code => fc_linked code | None => [] end) as linked.
  generalize (match view_functor P h with Some code => fc_inputs code | None => [] end) as inputs.
  generalize (match view_functor P h with Some code => fc_table code | None => [] end) as table.
  intros table inputs linked.
  assert (Hbases : forall inputs, map (fun source => (source,
      match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
      | Some meaning => ground (to_env d) meaning
      | None => match output_meaning (to_env dc) source with
        | Some meaning => meaning | None => TAbs source end
      end)) inputs
    = to_subst (DS (map (fun source => (source, lbase d linked th dc source)) inputs))).
  { intro l. unfold to_subst. cbn [dforce]. rewrite map_map. apply map_ext. intro source. cbn [fst snd].
    rewrite lbase_to. reflexivity. }
  assert (Hm : forall l, map (fun source => (source,
      match lookup_subst source table with
      | Some t => app_subst (map (fun source => (source,
          match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
          | Some meaning => ground (to_env d) meaning
          | None => match output_meaning (to_env dc) source with
            | Some meaning => meaning | None => TAbs source end
          end)) inputs) t
      | None => match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
          | Some meaning => ground (to_env d) meaning
          | None => match output_meaning (to_env dc) source with
            | Some meaning => meaning | None => TAbs source end
          end
      end)) l
    = to_subst (DS (map (fun source => (source,
      match lookup_subst source table with
      | Some t => ldefer (DS (map (fun source => (source, lbase d linked th dc source)) inputs)) t
      | None => lbase d linked th dc source
      end)) l))).
  { intro l. unfold to_subst at 1. cbn [dforce]. rewrite map_map. apply map_ext. intro source. cbn [fst snd]. f_equal.
    destruct (lookup_subst source table).
    - rewrite force_ldefer, <- lapp_spec, <- Hbases. reflexivity.
    - apply lbase_to. }
  rewrite !Hm. reflexivity.
Qed.

Lemma read_result_types_to : forall d Dr outputs,
  read_result_types (to_env d) Dr outputs =
  option_map (fun q => (to_subst (DS (fst q)), to_subst (DS (snd q)))) (lread_result_types d Dr outputs).
Proof.
  intros d Dr outputs. induction outputs as [|[source destination] rest IH]; [reflexivity|].
  cbn. destruct (existsb (item_eqb (ITy source)) Dr); [|reflexivity].
  rewrite output_meaning_to, IH.
  destruct (loutput_meaning d source), (lread_result_types d Dr rest) as [[w a]|]; reflexivity.
Qed.

Lemma result_environment_to : forall outputs d3 Dr dr,
  result_environment outputs (to_env d3) Dr (to_env dr)
  = option_map to_env (lresult_environment outputs d3 Dr dr).
Proof.
  intros outputs d3 Dr dr. unfold result_environment, lresult_environment.
  rewrite read_result_types_to. destruct (lread_result_types d3 Dr outputs) as [[w a]|]; cbn [option_map fst snd]; [|reflexivity].
  rewrite to_env_eq, provs_to. reflexivity.
Qed.

(* ===================== The interpreter =================================== *)

Module LazyInterpreter (Import Prims : PRIMITIVES) (Reference : INTERP Prims).
Module I := Reference.

Definition lcheck_tag (P : program_view) (d : lenv) (n : nsym) (th : subst) (w : value) : signal :=
  I.check_tag_with (lground d) P n th w.

Lemma check_tag_to : forall P d n th w, I.check_tag P (to_env d) n th w = lcheck_tag P d n th w.
Proof.
  intros. unfold I.check_tag, lcheck_tag, I.check_tag_with. destruct (lookup_cast n (view_casts P)) as [spec|]; [|reflexivity].
  unfold AuditBase.Representation.tag_trap.
  destruct (cast_handle spec) as [[h expected]|]; rewrite ?ground_to;
  destruct (lookup_subst (cast_held spec) th) as [tau|]; destruct w as [| | |[|a [[kind header]|]]]; rewrite ?ground_to; reflexivity.
Qed.

Fixpoint leval (fuel : nat) (P : program_view) (d : lenv) (g : locals) (st : mstate)
    (e : expr) {struct fuel} : I.evaluation :=
  match fuel with
  | 0 => I.Evaluation OutOfFuel g st
  | S k =>
      match e with
      | EVar x => match lookup_local x g with
                  | Some w => I.Evaluation (Normal w) g st
                  | None => I.Evaluation (Stuck I.e_var) g st
                  end
      | EUnit => I.Evaluation (Normal VUnit) g st
      | EVal v => match llookup_provision (IVal v) d with
                 | Some (LValue w) => I.Evaluation (Normal w) g st
                 | _ => I.Evaluation (Stuck I.e_val) g st
                 end
      | ETag n th e1 =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 => I.Evaluation (lcheck_tag P d n th w) g1 st1
          | r => r
          end
      | ECall f th sm args =>
          match view_function P f with
          | Some body =>
              match leval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  match lselect_provisions d sm with
                  | Some d' => I.ret_call g1 (leval k P (lcall_environment d th d') (parameter_values ws) st1 body)
                  | None => I.Evaluation (Stuck I.e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => I.Evaluation (Stuck I.e_nobody) g st
          end
      | EInvoke f args =>
          match llookup_provision (IFn f) d with
          | Some (LFunction f' df) =>
              match view_function P f' with
              | Some body =>
                  match leval_arguments k P d g st args with
                  | inl (ws, g1, st1) => I.ret_call g1 (leval k P df (parameter_values ws) st1 body)
                  | inr r => r
                  end
              | None => I.Evaluation (Stuck I.e_nobody) g st
              end
          | Some (LPrimitive p held) =>
              match leval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := I.run_prim_at P p held ws st1 in I.Evaluation o g1 st2
              | inr r => r
              end
          | _ => I.Evaluation (Stuck I.e_entry) g st
          end
      | EMethod e0 m prov args =>
          match llookup_provision prov d with
          | Some (LFunction f' d') =>
              match view_function P f' with
              | Some body =>
                  match leval k P d g st e0 with
                  | I.Evaluation (Normal w0) g0 st0 =>
                      match leval_arguments k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          I.ret_call g1 (leval k P d' (parameter_values (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => I.Evaluation (Stuck I.e_nobody) g st
              end
          | Some (LPrimitive p held) =>
              match leval k P d g st e0 with
              | I.Evaluation (Normal w0) g0 st0 =>
                  match leval_arguments k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := I.run_prim_at P p held (w0 :: ws) st1 in I.Evaluation o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => I.Evaluation (Stuck I.e_entry) g st
          end
      | ELet x e1 e2 =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal w1) g1 st1 => I.pop1 (leval k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | EBind binding e1 =>
          match leval_bindings k P d g st [binding] with
          | inl (d1, g1, st1) => leval k P d1 g1 st1 e1
          | inr result => result
          end
      | ESeq e1 e2 =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal _) g1 st1 => leval k P d g1 st1 e2
          | r => r
          end
      | EAssign x e1 =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 =>
              match assign_local x w g1 with
              | Some g2 => I.Evaluation (Normal VUnit) g2 st1
              | None => I.Evaluation (Stuck I.e_assign) g1 st1
              end
          | r => r
          end
      | EIf c e1 e2 =>
          match leval k P d g st c with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.truthy P w with
              | Some true => leval k P d g1 st1 e1
              | Some false => leval k P d g1 st1 e2
              | None => I.Evaluation (Stuck I.e_bool) g1 st1
              end
          | r => r
          end
      | EWhile c b =>
          match leval k P d g st c with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.truthy P w with
              | Some true =>
                  match leval k P d g1 st1 b with
                  | I.Evaluation (Normal _) g2 st2 => leval k P d g2 st2 (EWhile c b)
                  | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
                  | r => r
                  end
              | Some false => I.Evaluation (Normal VUnit) g1 st1
              | None => I.Evaluation (Stuck I.e_bool) g1 st1
              end
          | r => r
          end
      | ELoop b =>
          match leval k P d g st b with
          | I.Evaluation (Normal _) g2 st2 => leval k P d g2 st2 (ELoop b)
          | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
          | r => r
          end
      | EReturn e1 =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 => I.Evaluation (Returning w) g1 st1
          | r => r
          end
      | EBreak => I.Evaluation Breaking g st
      | ERecord fs =>
          match leval_arguments k P d g st (map snd fs) with
          | inl (ws, g1, st1) => I.Evaluation (Normal (VRecord (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | EField e1 f =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.get_field f w with
              | Some v => I.Evaluation (Normal v) g1 st1
              | None => I.Evaluation (Stuck I.e_field) g1 st1
              end
          | r => r
          end
      | EMatch e0 arms =>
          match leval k P d g st e0 with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.select_arm arms w with
              | Some (bs, eb) =>
                  I.popn (List.length bs) (leval k P d (bs ++ g1) st1 eb)
              | None => I.Evaluation (Stuck I.e_pat) g1 st1
              end
          | r => r
          end
      end
  end
with leval_arguments (fuel : nat) (P : program_view) (d : lenv) (g : locals) (st : mstate)
    (l : list expr) {struct fuel} : (list value * locals * mstate) + I.evaluation :=
  match fuel with
  | 0 => inr (I.Evaluation OutOfFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 =>
              match leval_arguments k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with leval_bindings (fuel : nat) (P : program_view) (d : lenv) (g : locals) (st : mstate)
    (bs : list binding) {struct fuel} : (lenv * locals * mstate) + I.evaluation :=
  match fuel with
  | 0 => inr (I.Evaluation OutOfFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | BType t tau :: r => leval_bindings k P (lbind_type d t tau) g st r
      | BValue v e1 :: r =>
          match leval k P d g st e1 with
          | I.Evaluation (Normal w1) g1 st1 => leval_bindings k P (linstall (IVal v) (LValue w1) d) g1 st1 r
          | rr => inr rr
          end
      | BFunction f f' types sm :: r =>
          match lselect_provisions d sm with
          | Some dc => leval_bindings k P (linstall (IFn f) (LFunction f' (ltyped_capture d types dc)) d) g st r
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      | BMethod tauk m th f' types sm :: r =>
          match lselect_provisions d sm with
          | Some dc => leval_bindings k P (linstall (IMth tauk m th) (LFunction f' (ltyped_capture d types dc)) d) g st r
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      | BFunctor u h types sm smc :: r =>
          match lselect_provisions d smc with
          | Some dc => leval_bindings k P (linstall (IFun u) (LFunctor h (Some sm) (ltyped_capture d types dc) None) d) g st r
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      | BCopyFunctor u u' sm rm :: r =>
          match llookup_provision (IFun u') d with
          | Some (LFunctor h a dc p) =>
              leval_bindings k P (linstall (IFun u) (LFunctor h (compose_adaptation a sm) dc (compose_positions p rm)) d) g st r
          | _ => inr (I.Evaluation (Stuck I.e_fun) g st)
          end
      | BApply u th outputs sm rs :: r =>
          match lapply_functor k P d u th outputs sm rs st with
          | inl (dr, st1) => leval_bindings k P (lappend dr d) g st1 r
          | inr rr => inr (I.with_g g rr)
          end
      | BPrimitive i p :: r => leval_bindings k P (linstall i (lprimitive_closure d i p) d) g st r
      end
  end
with lapply_functor (fuel : nat) (P : program_view) (d : lenv) (u : usym) (th : subst)
    (outputs : list (tsym * tsym)) (sm : smap) (rs : list item) (st : mstate) {struct fuel}
    : (lenv * mstate) + I.evaluation :=
  match fuel with
  | 0 => inr (I.Evaluation OutOfFuel [] st)
  | S k =>
      match llookup_provision (IFun u) d with
      | Some (LFunctor h a dc p) =>
          match view_functor P h with
          | Some code =>
              let Dr := fc_outputs code in
              let bs := fc_bindings code in
              match lselect_provisions d sm with
              | Some d1 =>
                  match ladapt_provisions d1 a with
                  | Some d2 =>
                      match leval_bindings k P (lapplication_environment P d h th (lappend d2 dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match lselect_provisions d3 (result_selection rs p (fc_sources code)) with
                          | Some dr => match lresult_environment outputs d3 Dr dr with
                              | Some result => inl (result, st3)
                              | None => inr (I.Evaluation (Stuck I.e_compose) [] st3) end
                          | None => inr (I.Evaluation (Stuck I.e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (I.Evaluation (Stuck I.e_chain) [] st)
                  end
              | None => inr (I.Evaluation (Stuck I.e_compose) [] st)
              end
          | None => inr (I.Evaluation (Stuck I.e_nobody) [] st)
          end
      | _ => inr (I.Evaluation (Stuck I.e_fun) [] st)
      end
  end.

Definition lrun_main (fuel : nat) (P : program_view) (main : fsym) (d0 : lenv)
    (st0 : mstate) : I.evaluation :=
  match view_function P main with
  | Some body => I.ret_call [] (leval fuel P d0 [] st0 body)
  | None => I.Evaluation (Stuck I.e_nobody) [] st0
  end.

(* ===================== Agreement ========================================= *)

Definition conv_bindings (r : (lenv * locals * mstate) + I.evaluation)
    : (environment * locals * mstate) + I.evaluation :=
  match r with inl (d, g, s) => inl (to_env d, g, s) | inr e => inr e end.
Definition conv_apply (r : (lenv * mstate) + I.evaluation) : (environment * mstate) + I.evaluation :=
  match r with inl (d, s) => inl (to_env d, s) | inr e => inr e end.

Lemma install_value_to : forall i w d,
  install_provision i (ValueProvision w) (to_env d) = to_env (linstall i (LValue w) d).
Proof. intros. apply (install_to i (LValue w)). Qed.
Lemma install_function_to : forall i f d dc,
  install_provision i (FunctionClosure f (to_env dc)) (to_env d) = to_env (linstall i (LFunction f dc) d).
Proof. intros. apply (install_to i (LFunction f dc)). Qed.
Lemma install_functor_to : forall i h a dc p d,
  install_provision i (FunctorClosure h a (to_env dc) p) (to_env d) = to_env (linstall i (LFunctor h a dc p) d).
Proof. intros. apply (install_to i (LFunctor h a dc p)). Qed.
Lemma install_primitive_to : forall i p d,
  install_provision i (primitive_closure (to_env d) i p) (to_env d) = to_env (linstall i (lprimitive_closure d i p) d).
Proof. intros. rewrite primitive_closure_to. apply install_to. Qed.

Ltac conv_rw IHe IHa IHb IHf :=
  repeat first
    [ rewrite IHe | rewrite IHa | rewrite IHb | rewrite IHf
    | rewrite lookup_provision_to | rewrite select_to | rewrite adapt_to
    | rewrite call_to | rewrite typed_capture_to | rewrite bind_type_to
    | rewrite append_to | rewrite install_primitive_to | rewrite install_value_to
    | rewrite install_function_to | rewrite install_functor_to
    | rewrite check_tag_to | rewrite application_environment_to | rewrite result_environment_to ].

Ltac conv_destruct :=
  match goal with
  | |- context [match ?x with _ => _ end] =>
      lazymatch x with
      | I.eval _ _ _ _ _ _ => fail
      | I.eval_arguments _ _ _ _ _ _ => fail
      | I.eval_bindings _ _ _ _ _ _ => fail
      | I.apply_functor _ _ _ _ _ _ _ _ _ => fail
      | to_prov _ => fail
      | to_env _ => fail
      | option_map _ _ => fail
      | conv_bindings _ => fail
      | conv_apply _ => fail
      | _ => destruct x
      end
  end.

Ltac conv_step IHe IHa IHb IHf :=
  repeat first
    [ reflexivity
    | progress (conv_rw IHe IHa IHb IHf)
    | progress (cbn [option_map conv_bindings conv_apply to_prov])
    | match goal with p : prod _ _ |- _ => destruct p end
    | conv_destruct ].

Theorem leval_agrees : forall k,
  (forall P d g st e, I.eval k P (to_env d) g st e = leval k P d g st e) /\
  (forall P d g st l, I.eval_arguments k P (to_env d) g st l = leval_arguments k P d g st l) /\
  (forall P d g st bs, I.eval_bindings k P (to_env d) g st bs = conv_bindings (leval_bindings k P d g st bs)) /\
  (forall P d u th outputs sm rs st,
     I.apply_functor k P (to_env d) u th outputs sm rs st = conv_apply (lapply_functor k P d u th outputs sm rs st)).
Proof.
  induction k as [|k [IHe [IHa [IHb IHf]]]].
  - repeat split; intros; reflexivity.
  - repeat split.
    + intros P d g st e. destruct e; cbn [I.eval leval]; conv_step IHe IHa IHb IHf.
    + intros P d g st l. destruct l; cbn [I.eval_arguments leval_arguments]; conv_step IHe IHa IHb IHf.
    + intros P d g st bs. destruct bs as [|b bs]; [reflexivity|].
      destruct b; cbn [I.eval_bindings leval_bindings]; conv_step IHe IHa IHb IHf.
    + intros P d u th outputs sm rs st. cbn [I.apply_functor lapply_functor].
      conv_step IHe IHa IHb IHf.
Qed.

Theorem lrun_main_agrees : forall fuel P main d0 st0,
  lrun_main fuel P main (lift_env d0) st0 = I.run_main fuel P main d0 st0.
Proof.
  intros. unfold lrun_main, I.run_main. destruct (view_function P main); [|reflexivity].
  rewrite <- (to_lift_env d0) at 2. rewrite (proj1 (leval_agrees fuel)). reflexivity.
Qed.

End LazyInterpreter.
