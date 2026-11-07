(* Family callbacks use receipts genuinely retained from the accepted run. *)
From Stdlib Require Import List String.
From NM Require Import Audit Sx Elab ElabFrameCore BodySoundCore ProvidedDirect LiteralDirect
 ProvidedFamilyExec LiteralFamilyExec.
Section Callbacks.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Hypothesis Hworld:Audit.Elaboration.WorldOf b(BodySoundCore.W w R Fl).
Theorem provided_family_step:forall fuel,OK w P R Fl fuel->ProvidedFamilyStep w P R Fl fuel.
Proof.
 intros fuel IH c e ox r m args s x t sf Run F C Hi Ii Fi HC Post Wr Sr Miss.
 destruct(lt_contexts _ _ _ _ _(proj1 Post))as(sc&Checked&World).
 exact(provided_family_exec w R Fl b Hload Finj Hworld sc Checked World fuel IH
 c e ox r m args s x t sf Run F C Hi Ii Fi HC Post Wr Sr Miss).
Qed.
Theorem literal_family_step:forall fuel,LiteralFamilyStep w P R Fl fuel.
Proof.
 intros fuel c e ox T oy m s x t sf Run F C Hi Ii Fi HC Post WT ST Miss.
 destruct(lt_contexts _ _ _ _ _(proj1 Post))as(sc&Checked&World).
 exact(literal_family_exec w R Fl b Hload Finj Hworld sc Checked World fuel
 c e ox T oy m s x t sf Run F C Hi Ii Fi HC Post WT ST Miss).
Qed.
End Callbacks.
