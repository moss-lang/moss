(* Decode an actual fresh-marker specialization back to the raw structural matcher. *)
From Stdlib Require Import List.
From NM Require Import Audit NMap Sx Elab BodyTr PMatchComplete PMatchExec PMatchSemanticImage FamilyMarkedSubst FamilyMarkersExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Theorem family_semantic_raw_solution:forall Fl e receiver ps s marks sf table pat,
 (forall x y,Fl x=Fl y->x=y)->ety_ok pat=true->ety_ok receiver=true->
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->
 Audit.app_subst(family_marker_subst marks table)(tr Fl pat)=tr Fl receiver->
 mark_type(map(fun q=>(snd q,fst q))marks)(match_decoded_theta Fl receiver table)pat=receiver.
Proof.
 intros Fl e receiver ps s marks sf table pat Finj Clean Receiver Alloc Eq.
 destruct(family_marker_allocation _ _ _ _ _ _ Alloc)as[Nd Rest].
 eapply match_semantic_raw_solution;[exact Finj|exact Clean|exact Receiver| | | |exact Eq].
 - intros g p Read;exact(family_marker_subst_back marks table Nd g p Read).
 - intros g Read;exact(family_marker_subst_other marks table g Read).
 - intros g p Read;apply pmatch_lookup_member in Read.
   apply in_map_iff in Read as([p0 g0]&Eq0&Hin);cbn[fst snd]in Eq0;injection Eq0 as <- <-.
   exact(family_markers_avoid_receiver _ _ _ _ _ _ _ _ Alloc Hin).
Qed.
From NM Require Import FamilyCandidateComplete FamilySelectTrace.
Theorem family_candidate_semantic_complete:forall Fl mi e em r mid u fg marks found lmi rg m sub s pat sp found' sf ps seed sm table,
 (forall x y,Fl x=Fl y->x=y)->ety_ok pat=true->ety_ok r=true->
 mmap(fun p=>let!k:=fresh_marker_for e r in ret(p,k))ps seed=Good(marks,sm)->
 family_candidate_step mi e em r mid u fg marks(family_marker_back marks)found(lmi,rg,m,sub)s=Good(found',sf)->
 receiver_at em lmi rg sub s=Good(pat,sp)->gs_id m=mid->gs_id fg=u->
 MatchUnique pat->canon e r=r->
 Audit.app_subst(family_marker_subst marks table)(tr Fl pat)=tr Fl r->
 exists other th,In(other,th)found'/\gs_id other=gs_id fg.
Proof.
 intros Fl mi e em r mid u fg marks found lmi rg m sub s pat sp found' sf ps seed sm table
 Finj Clean Receiver Alloc Step Read Mid Fid Unique Can Eq.
 pose proof(family_semantic_raw_solution Fl e r ps seed marks sm table pat Finj Clean Receiver Alloc Eq)as Raw.
 destruct(family_marker_allocation _ _ _ _ _ _ Alloc)as[Nd Rest].
 eapply family_candidate_retains_solution;[exact Step|exact Read|exact Mid|exact Fid|exact Nd|exact Unique| |exact Can].
 symmetry;exact Raw.
Qed.
