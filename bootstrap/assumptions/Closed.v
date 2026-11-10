(* These executable type-reader proofs are closed under the global context.
   check.sh audits every assumption reported here. *)
From NM Require EaTypesSound.

Print Assumptions NM.EaTypesSound.ty_of_sound.
Print Assumptions NM.EaTypesSound.brackets_sound.
Print Assumptions NM.EaTypesSound.sig_of_sound.
