(* Reuse checked information-preservation certificates at program phase boundaries. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab.
From NM Require Export InfoPreserve.
Local Strategy opaque [fn_info].
Lemma program_info_listed:okI check_listed.
Proof. exact okI_check_listed. Qed.
Lemma program_info_contexts:okI check_contexts.
Proof. exact okI_check_contexts. Qed.
Lemma program_info_runtime:okI check_runtime_library.
Proof. exact okI_check_runtime_library. Qed.
Lemma program_info_assumes:okI check_assumes.
Proof. exact okI_check_assumes. Qed.
Lemma program_info_lower:okI lower_all.
Proof. exact okI_lower_all. Qed.
Lemma program_info_platform:forall k p,okI(platform_entry k p).
Proof. exact okI_platform_entry. Qed.
Lemma program_info_fun_env:forall g,okI(fun_env g).
Proof. exact okI_fun_env. Qed.
#[local]Hint Resolve program_info_fun_env:okIdb.
Lemma program_info_runtime_inputs:forall h,okI(runtime_provider_inputs h).
Proof. exact okI_runtime_provider_inputs. Qed.
Lemma program_info_ety_ty:forall t,okI(ety_ty t).
Proof. exact okI_ety_ty. Qed.
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
 | |-okI(mbind(new_sym _ _ _ _ _)_)=>okIt
 | |-okI(mbind _ _)=>apply okI_bind;[program_info|intros;program_info]
 | |-okI(mfold _ _ _)=>apply okI_mfold;intros;program_info
 | |-okI(mmap _ _)=>apply okI_mmap;intros;program_info
 | |-okI(miter _ _)=>apply okI_miter;intros;program_info
 | |-okI(match ?x with _=>_ end)=>destruct x;program_info
 end|okIt].
