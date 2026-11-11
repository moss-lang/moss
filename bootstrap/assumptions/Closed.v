(* These executable type-reader proofs are closed under the global context.
   check.sh audits every assumption reported here. *)
From NM Require EaTypesSound.

Print Assumptions NM.EaTypesSound.ty_of_sound.
Print Assumptions NM.EaTypesSound.brackets_sound.
Print Assumptions NM.EaTypesSound.sig_of_sound.

Print Assumptions NM.EaTypesSound.lang_sym_sound.
Print Assumptions NM.EaTypesSound.lang_item_sym_sound.
Print Assumptions NM.EaTypesSound.method_named_sound.
Print Assumptions NM.EaTypesSound.payload_of_sound.
Print Assumptions NM.EaTypesSound.recv_ty_sound.

From NM Require EaMentionsSound.
Print Assumptions NM.EaMentionsSound.free_in_sound.
Print Assumptions NM.EaMentionsSound.sig_mentions_sound.
Print Assumptions NM.EaMentionsSound.spec_types_go_sound.
Print Assumptions NM.EaMentionsSound.spec_types_sound.
Print Assumptions NM.EaMentionsSound.fun_outputs_sound.
