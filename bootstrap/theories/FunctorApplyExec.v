(* General application follows from the remaining ordinary bind cases. *)
From Stdlib Require Import String.
From NM Require Import Audit Sx Elab BodySoundCore BindSequence BindFnDispatch
 ApplyFunctorInduct AbstractApplyExec.
Section Application.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:Audit.Elaboration.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Theorem apply_bind_step:
 (forall f,OK w P R Fl f->RemainingBindStep w P R Fl f)->
 forall f,OK w P R Fl f->ApplyBindStep w P R Fl f.
Proof.
 intro Remaining;apply(apply_from_abstract_and_remaining w R Fl b Hload Hworld Finj).
 - exact(abstract_application_sound w R Fl b Hload Hworld Finj).
 - exact Remaining.
Qed.
End Application.
