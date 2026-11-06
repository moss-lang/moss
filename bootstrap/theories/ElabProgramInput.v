(* Actual operational provenance at whole-program phase boundaries. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame InfoPreserve ElabConstInput.
Local Strategy opaque [fn_info].
Lemma program_info_listed:okI check_listed.
Proof. unfold check_listed;okIt. Qed.
Lemma program_info_contexts:okI check_contexts.
Proof. unfold check_contexts;okIt. Qed.
Lemma program_info_runtime:okI check_runtime_library.
Proof. unfold check_runtime_library;okIt. Qed.
Lemma program_info_assumes:okI check_assumes.
Proof. unfold check_assumes;okIt. Qed.
Lemma program_info_lower:okI lower_all.
Proof. unfold lower_all;okIt. Qed.
Lemma program_info_platform:forall k p,okI(platform_entry k p).
Proof. intros;unfold platform_entry;okIt. Qed.
Lemma program_info_fun_env:forall g,okI(fun_env g).
Proof. intros;apply okI_ni;[apply input_fun_env_ok|apply input_fun_env_ni|apply kwr_fun_env_input]. Qed.
#[local]Hint Resolve program_info_fun_env:okIdb.
Lemma program_info_runtime_inputs:forall h,okI(runtime_provider_inputs h).
Proof. intros;unfold runtime_provider_inputs;okIt. Qed.
Lemma program_info_ety_ty:forall t,okI(ety_ty t).
Proof. intros;unfold ety_ty;apply okI_ni;[apply ok_ety_ty_go|apply ni_ety_ty_go|keepst]. Qed.
Ltac program_info:=first[
 lazymatch goal with
 | |-okI(check_main_signature _ _)=>apply okI_check_main_signature
 | |-okI(check_main_arity _)=>apply okI_check_main_arity
 | |-okI(entry_zero_width _ _ _)=>apply okI_entry_zero_width
 | |-okI(ety_ty _)=>apply program_info_ety_ty
 | |-okI(fun_env _)=>apply program_info_fun_env
 | |-okI(runtime_provider_inputs _)=>apply program_info_runtime_inputs
 | |-okI check_listed=>exact program_info_listed
 | |-okI check_contexts=>exact program_info_contexts
 | |-okI check_runtime_library=>exact program_info_runtime
 | |-okI check_assumes=>exact program_info_assumes
 | |-okI lower_all=>exact program_info_lower
 | |-okI(platform_entry _ _)=>apply program_info_platform
 end|lazymatch goal with
 | |-okI(mbind(new_sym _ _ _ _ _)_)=>input_info
 | |-okI(mbind _ _)=>apply okI_bind;[program_info|intros;program_info]
 | |-okI(mfold _ _ _)=>apply okI_mfold;intros;program_info
 | |-okI(mmap _ _)=>apply okI_mmap;intros;program_info
 | |-okI(miter _ _)=>apply okI_miter;intros;program_info
 | |-okI(match ?x with _=>_ end)=>destruct x;program_info
 end|input_info].
Ltac program_qit_with ext:=repeat(fintros;first[ext|lazymatch goal with
 | |-okqi _ ?t=>first[solve[auto with okqidb]|lazymatch t with
  |mbind _ _=>first[apply okqi_bind;[solve[program_info]|solve[input_fwd]| |]|solve[okqi_keeps_leaf]]
  |mmap _ _=>apply okqi_mmap;[intros;solve[program_info]|intros;solve[input_fwd]|]
  |mfold _ _ _=>apply okqi_mfold;[intros;solve[program_info]|intros;solve[input_fwd]|]
  |miter _ _=>apply okqi_miter;[intros;solve[program_info]|intros;solve[input_fwd]|]
  |with_vars _ _=>apply okqi_with_vars
  |upd _=>okqi_keeps_leaf
  |(fun s=>_)=>okqi_keeps_leaf
  |_=>first[solve[okqi_keeps_lemma]|(lazymatch t with ?F _=>is_fix F end;qifind1)
    |(lazymatch t with ?F _ _ _=>is_fix F end;qifind3)
    |match t with context[match ?x with _=>_ end]=>destruct x end
    |let h:=head_of t in progress unfold h]end]end]).
Ltac program_qit:=program_qit_with fail.
