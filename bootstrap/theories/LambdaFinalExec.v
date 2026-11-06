(* Genuine whole-source execution establishes final bodies for every
   historical lambda row. There is no completed-body premise at the end. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame LoaderOrigin FnBound
 LambdaCompletion LambdaRows LambdaRowsProgram FinalLambdas ForwarderProgramTables.
Import ListNotations.
Set Implicit Arguments.
Lemma lambda_initial_inv:forall b w,load_world b=Good w->Inv(s_init w).
Proof.
 intros b w Load;destruct(load_world_syms b w Load)as[Positive Above].
 constructor;cbn[st_next st_syms st_flds st_w st_reqs s_init].
 - exact Above.
 - split;[constructor|reflexivity].
 - exact(load_world_ids b w Load).
 - intros b' Load' id g H;exact H.
 - intros b' Load' id g Missing H;rewrite Missing in H;discriminate.
 - intros b' Load' id values H;destruct id;discriminate.
 - intros b' Load' id g Missing H;rewrite Missing in H;discriminate.
Qed.
Lemma lambda_complete_init:forall b w,load_world b=Good w->LambdaComplete(s_init w).
Proof.
 intros b w Load key owner Lookup Origin.
 change(nget(w_syms w)key=Some owner)in Lookup.
 rewrite(load_world_source b w Load _ _ Lookup)in Origin;discriminate.
Qed.
Lemma lambda_nlook_source_lookup:forall X key(rows:list(nat*X)),Audit.Elaboration.nlook key rows=nassoc key rows.
Proof. intros X key rows;induction rows as[|[k v]rows IH];cbn[Audit.Elaboration.nlook nassoc assoc];[reflexivity|].
 destruct(Nat.eqb key k);[reflexivity|exact IH]. Qed.
Theorem actual_final_lambdas:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->FinalLambdas p final.
Proof.
 intros b w p final Load Run.
 assert(Rows:LambdaRows final).
 {exact(proj2 lcr_elab_program b w _ _ _ Load(@lambda_initial_inv b w Load)(@fn_bound_init w)
 (@lambda_rows_init w)(@lambda_complete_init b w Load)eq_refl Run). }
 intros key body owner Member Lookup Origin.
 unfold Audit.Elaboration.body_of.
 rewrite(@forwarder_elab_program_tables _ _ _ Run),lambda_nlook_source_lookup.
 exact(Rows _ _ _ Member Lookup Origin).
Qed.
