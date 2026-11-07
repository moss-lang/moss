(* Loaded/source and synthetic forwarder bodies may emit recursion markers;
   their owners have non-lambda origins, so lambda history stays exact. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnBound FnBoundSynth
 LambdaRows LambdaRowsPreserve LambdaRowsEmit ForwarderLowerCode.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Definition LCLower(fuel:nat)(g:gsym):Prop:=BF_at(gs_id g)(lower_body fuel g)/\
 forall s value final,Inv s->FnBound s->LambdaRows s->
 nget(st_syms s)(gs_id g)=Some g->gs_origin g<>OLambda->
 lower_body fuel g s=Good(value,final)->LambdaRows final.
Arguments LCLower:simpl never.
Tactic Notation "lambda_read_step" hyp(E) hyp(I) hyp(B) hyp(R) hyp(Symbol)
 ident(Iout) ident(Bout) ident(Rout) ident(Sout) :=
 match type of E with ?m ?s=Good(?v,?final)=>
 let O:=fresh "Op"in let K:=fresh "Kept"in let F:=fresh "Frame"in
 assert(O:ok m)by okt;
 assert(K:keeps st_fns m)by(first[solve[auto with keepsdb]|solve[keepst]]);
 destruct(@lambda_rows_readonly_run _ m s v final O K I B R E)as[Iout[Bout[Rout F]]];
 pose proof(fr_keep _ _ F _ _ Symbol)as Sout
 end.
Ltac lambda_lower_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Lemma lc_lower_body_step:forall fuel,
 (forall c e b exp,LC(synth_block fuel c e b exp))->forall g,LCLower(S fuel)g.
Proof.
 intros fuel Body g;split;[apply bf_lower_body_at|].
 intros s value final I Bound Rows Symbol Origin Run;cbn[lower_body]in Run.
 apply bind_inv in Run as[done[mid[Done Run]]].
 cbn[lowered mbind get ret]in Done;injection Done as <- <-.
 destruct(is_some(nassoc(gs_id g)(st_fns s)));[injection Run as <- <-;exact Rows|].
 destruct(fn_body g)as[b|]eqn:HasBody;[|injection Run as <- <-;exact Rows].
 apply bind_inv in Run as[info[si[Info Run]]].
 lambda_read_step Info I Bound Rows Symbol Ii Bi Ri Si.
 apply bind_inv in Run as[this[sr[Receiver Run]]].
 lambda_read_step Receiver Ii Bi Ri Si Ir Br Rr Sr.
 apply bind_inv in Run as[sg[ss[Signature Run]]].
 lambda_read_step Signature Ir Br Rr Sr Is Bs Rs Ss.
 fold parameter_duplicate in Run.
 destruct(parameter_duplicate _)as[duplicate|]eqn:Dup;[lambda_lower_noerr Run|].
 apply bind_inv in Run as[u[marker[Marker Run]]].
 destruct(@lambda_rows_emit_other_run ss u marker(gs_id g)XUnit g Is Bs Rs Ss Origin Marker)
 as[Im[Bm[Rm Fm]]].
 pose proof(fr_keep _ _ Fm _ _ Ss)as Sm.
 apply bind_inv in Run as[u2[registered[Register Run]]].
 lambda_read_step Register Im Bm Rm Sm Ireg Breg Rreg Sreg.
 apply bind_inv in Run as[[[x out]ty][bodyfinal[BodyRun Run]]].
 match type of BodyRun with ?m _ = _=>assert(LCbody:LC m)by(unfold in_reify_code;lct)end.
 destruct(proj1(proj1 LCbody)_ _ _ Ireg BodyRun)as[Ib Fb].
 pose proof(proj2(proj1 LCbody)_ _ _ Ireg Breg BodyRun)as Bb.
 pose proof(proj2 LCbody _ _ _ Ireg Breg Rreg BodyRun)as Rb.
 pose proof(fr_keep _ _ Fb _ _ Sreg)as Sb.
 apply bind_inv in Run as[u3[sl1[Layout1 Run]]].
 lambda_read_step Layout1 Ib Bb Rb Sb Il1 Bl1 Rl1 Sl1.
 apply bind_inv in Run as[u4[sl2[Layout2 Run]]].
 lambda_read_step Layout2 Il1 Bl1 Rl1 Sl1 Il2 Bl2 Rl2 Sl2.
 apply bind_inv in Run as[u5[sl3[Layout3 Run]]].
 lambda_read_step Layout3 Il2 Bl2 Rl2 Sl2 Il3 Bl3 Rl3 Sl3.
 apply bind_inv in Run as[u6[sl4[Layout4 Run]]].
 lambda_read_step Layout4 Il3 Bl3 Rl3 Sl3 Il4 Bl4 Rl4 Sl4.
 exact(proj1(proj2(proj2(@lambda_rows_emit_other_run sl4 value final(gs_id g)x g
 Il4 Bl4 Rl4 Sl4 Origin Run)))).
Qed.
