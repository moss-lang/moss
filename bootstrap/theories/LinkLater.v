(* Link creator receipts transport final-program readings across local scopes. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore ReqsSolve
 InfoInvariant BodySoundCore FinalForwarders FinalLambdas FinalLinks LinkCacheInvariant.
Import ListNotations.
Lemma later_link_frame_back : forall w P R Fl s final,
 LinkFrame s final ->Later w P R Fl final ->Later w P R Fl s.
Proof.
 intros w P R Fl s final [Frame Rows] Last.
 destruct Last as[World Fields Unsupported Language Requirements Constants Closed None Bool Forwarders Lambdas Contexts].
 constructor.
 - exact(eq_trans(eq_sym(fl_w _ _ Frame))World).
 - intros name id Lookup;apply Fields;exact(fl_flds _ _ Frame _ _ Lookup).
 - destruct(fl_unsup _ _ Frame)as[extra Eq];rewrite Unsupported in Eq.
   destruct(st_unsup s);[reflexivity|discriminate].
 - exact(eq_trans(eq_sym(fl_lang _ _ Frame))Language).
 - intros key values Lookup;apply Requirements;exact(fl_reqs _ _ Frame _ _ Lookup).
 - intros key code ty Lookup;apply Constants;exact(fl_consts _ _ Frame _ _ Lookup).
 - exact Closed.
 - exact None.
 - exact Bool.
 - eapply final_forwarders_keep;[exact(fl_keep _ _ Frame)|exact Forwarders].
 - eapply final_lambdas_keep;[exact(fl_fn_rows _ _ Frame)|exact(fl_keep _ _ Frame)|exact Lambdas].
 - exact Contexts.
Qed.
Lemma final_links_link_frame_back : forall P s final,
 LinkFrame s final ->FinalLinks P final ->FinalLinks P s.
Proof.
 intros P s final Frame Last;apply(final_links_keep P s final);[exact(link_frame_functors _ _ Frame)|exact Last].
Qed.
