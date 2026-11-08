(* The static forwarder fact is obtained from the accepted compiler run. *)
From NM Require Import ExecutionModel Sx Elab ElabFrame FinalForwarders ForwarderHistoryProgram.
Theorem actual_final_forwarders:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->FinalForwarders w p final.
Proof. intros b w p final Load Run;unfold FinalForwarders;intros key g target Lookup Origin.
 split;[eapply elab_final_forwarder_nonzero;eassumption|eapply elab_final_forwarders;eassumption]. Qed.
