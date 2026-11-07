(* Lexical reifier state is separate from persistent generated code/view
   tables. The substitution records the actual type-alias bindings emitted
   at this point; it is never a guessed runtime grounding. *)
From Stdlib Require Import List.
From NM Require Import Audit CoreMoss Reify.
Import ListNotations.

Record reify_local : Type := ReifyLocal {
  rl_runtime : reify_state;
  rl_aliases : subst
}.
Definition restore_reify_local (before after : reify_local) : reify_local :=
  ReifyLocal (restore_reify_scope (rl_runtime before) (rl_runtime after))
    (rl_aliases before).
Definition local_type_alias (name : tsym) (raw : ty) (before : reify_local)
    : tsym * ty * reify_local :=
  let '(slot,meaning,after) := lower_type_alias name raw (rl_runtime before) in
  (slot,meaning,ReifyLocal after
    ((slot,app_subst (rl_aliases before) meaning) :: rl_aliases before)).

Definition aliases_after_temporaries (bindings : list (tsym * ty)) (aliases : subst) : subst :=
  fold_left (fun current p =>
    (fst p,app_subst current (snd p)) :: current) bindings aliases.
Definition local_after_temporaries (bindings : list (tsym * ty))
    (allocated : reify_state) (before : reify_local) : reify_local :=
  ReifyLocal allocated (aliases_after_temporaries bindings (rl_aliases before)).

Lemma restore_reify_local_scope : forall before after,
    rf_names (rl_runtime (restore_reify_local before after)) = rf_names (rl_runtime before) /\
    rf_generations (rl_runtime (restore_reify_local before after)) = rf_generations (rl_runtime before) /\
    rf_items (rl_runtime (restore_reify_local before after)) = rf_items (rl_runtime before) /\
    rl_aliases (restore_reify_local before after) = rl_aliases before /\
    rf_next (rl_runtime (restore_reify_local before after)) = rf_next (rl_runtime after) /\
    rf_declarations (rl_runtime (restore_reify_local before after)) = rf_declarations (rl_runtime after).
Proof. intros. repeat split; reflexivity. Qed.

Lemma local_type_alias_context : forall name raw before slot meaning after,
    local_type_alias name raw before = (slot,meaning,after) ->
    rl_aliases after = (slot,app_subst (rl_aliases before) meaning) :: rl_aliases before /\
    rf_generations (rl_runtime after) = rf_generations (rl_runtime before) /\
    rf_names (rl_runtime after) = (name,TAbs slot) :: rf_names (rl_runtime before).
Proof. intros. inversion H; subst. repeat split; reflexivity. Qed.

Lemma aliases_after_temporaries_cons : forall slot meaning rest aliases,
    aliases_after_temporaries ((slot,meaning)::rest) aliases =
    aliases_after_temporaries rest ((slot,app_subst aliases meaning)::aliases).
Proof. reflexivity. Qed.
