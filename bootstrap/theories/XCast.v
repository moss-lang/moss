(* Native checked-cast helpers: executable metadata and grounding laws. *)
From Stdlib Require Import List Bool.
From NM Require Import Audit MossX.
Import ListNotations.

Lemma xdtypes_app : forall d e,
  xdtypes (d ++ e) = xdtypes d ++ xdtypes e.
Proof.
  induction d as [|[i en] d IH]; intros e; simpl; auto.
  destruct i, en; simpl; rewrite IH; reflexivity.
Qed.

Lemma xdtypes_type_env : forall d, xdtypes (xtype_env d) = xdtypes d.
Proof.
  induction d as [|[i en] d IH]; simpl; auto.
  destruct i, en; simpl; rewrite ?IH; reflexivity.
Qed.

Lemma xdtypes_inst : forall d th,
  xdtypes (xinst d th) = map_snd (xground d) th.
Proof.
  intros d th. rewrite xinst_map. induction th as [|[t tau] th IH]; simpl; auto.
  rewrite IH. reflexivity.
Qed.

Lemma lookup_type_env_non_type : forall d i,
  is_ty_item i = false -> lookup_xdenv i (xtype_env d) = None.
Proof.
  induction d as [|[j en] d IH]; intros i Hi; simpl; auto.
  destruct j, en; simpl; try apply IH; auto.
  destruct i; simpl in *; try discriminate; apply IH; auto.
Qed.

Lemma xallocation_nonerror : forall p tau o,
  (match o with XOk _ | XTrap _ | XExit _ | XUns _ | XFuel => True | _ => False end) ->
  match XS.xallocation p tau o with
  | XOk _ | XTrap _ | XExit _ | XUns _ | XFuel => True
  | _ => False
  end.
Proof.
  intros p tau o H. unfold XS.xallocation.
  destruct p, o; simpl in *; try contradiction; auto.
  all: repeat (first [exact I | match goal with v : xvalue |- _ => destruct v; simpl end]).
Qed.

Lemma xcast_outcome : forall P d n th w,
  (exists v, XS.xcast P d n th w = XOk v) \/ XS.xcast P d n th w = XTrap 37.
Proof.
  intros P d n th w. unfold XS.xcast.
  destruct (lookup_cast n (xp_cast P)) as [spec|]; [|left; eauto].
  destruct (match xc_handle spec with
            | None => true
            | Some (h, expected) => ty_eqb (xground d (match lookup_subst h th with Some t => t | None => TAbs h end)) expected
            end); [|left; eauto].
  destruct (lookup_subst (xc_held spec) th) as [tau|]; [|right; reflexivity].
  destruct w; try (right; reflexivity).
  destruct (Bool.eqb (xc_array spec) b && ty_eqb (xground d tau) t);
    [left; eauto | right; reflexivity].
Qed.
