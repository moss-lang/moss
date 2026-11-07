(* The staged target of the source-to-runtime typing bridge. Source
   elaboration proves Audit.Elaboration.Elaborates for the raw program;
   immutable lowering produces the separately typed executable program. *)
From NM Require Import Audit CoreMoss MossX XTyping XProvision XSafety Reify ReifyProgram Elab.

Definition typed_runtime_program (program : xprogram) : Prop :=
  forall argv read,
  exists Sg Rp Fb inputs body Phi ground,
    xwf Sg Rp (program_of program argv read) Fb /\
    g_fn Sg (xo_main program)=Some(inputs,mkSig nil TUnit,None) /\
    xp_fn (program_of program argv read)(xo_main program)=Some body /\
    wf_tele Sg empty_ctx inputs Phi /\
    ctx_grounded ground Phi /\
    provision_environment Sg Rp (program_of program argv read) Fb
      (fun _=>None) ground (cA Phi)(xo_env program).

Definition reified_typing_certificate (source : st) (raw : xprogram)
    (result : reify_result) : Prop :=
  reify_program source raw=Some result /\typed_runtime_program(rr_program result).

Theorem typed_runtime_never_err : forall program,
  typed_runtime_program program->forall fuel argv read initial code,
  MossX.XS.xr_out(MossX.XS.run_main fuel(program_of program argv read)
    (xo_main program)(xo_env program)initial)<>XErr code.
Proof.
 intros program Typed fuel argv read initial code.
 destruct(Typed argv read)as[Sg[Rp[Fb[inputs[body[Phi[ground
   [Well[Main[Body[Formed[Ground Environment]]]]]]]]]]]].
 eapply mossx_run_main_safe;[exact Well|exact Main|reflexivity|exact Body|
   exact Formed|exact Ground|exact Environment].
Qed.

Theorem certified_reification_never_err : forall source raw result,
  reified_typing_certificate source raw result->forall fuel argv read initial code,
  MossX.XS.xr_out(MossX.XS.run_main fuel(program_of(rr_program result)argv read)
    (xo_main(rr_program result))(xo_env(rr_program result))initial)<>XErr code.
Proof.
 intros source raw result[Reified Typed];eapply typed_runtime_never_err;exact Typed.
Qed.

(* ComponentContracts.SOURCE_RUNTIME_COMPLETE.source_runtime_certificate states the
   remaining universal construction obligation. For every successful source
   elaboration, lowering must succeed and supply the runtime typing
   certificate used above. Certified.v does not implement that contract. *)
