(* The generated empty-statement tail block is precisely an abstract call. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ForwarderArgsCode ForwarderCallCode.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Ltac fwd_code_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Theorem synth_abstract_call_code:forall fuel c e pa names slots exp s v t final target g,
 LocalSlots e names slots s ->Inv s ->
 rpath(cx_mod c)pa s=Good(Some(ESym target),s)->
 nget(st_syms s)target=Some g ->gs_kind g=KFn ->fn_body g=None ->
 synth fuel c e(AECall pa None(map(fun n=>AEPath[n]None)names))exp s=Good((v,t),final)->
 v=XCallA target(map XVar slots).
Proof.
 intros[|f]c e pa names slots exp s v t final target g Slots I Resolve Lookup Kind NoBody E;[discriminate|].
 cbn[synth]in E;apply bind_inv in E as[[vx tx][sCall[Call E]]].
 pose proof(synth_call_abstract_code _ _ _ _ _ _ _ _ _ _ _ _ Slots I Resolve Lookup Kind NoBody Call)as Code.
 apply bind_inv in E as[u[sEnd[Inject E]]];injection E as <- <- <-;exact Code.
Qed.
Theorem synth_forwarder_block_code:forall fuel c e pa names slots exp s v t bt final target g,
 LocalSlots e names slots s ->Inv s ->
 rpath(cx_mod c)pa s=Good(Some(ESym target),s)->
 nget(st_syms s)target=Some g ->gs_kind g=KFn ->fn_body g=None ->
 synth_block fuel c e(ABlock[](Some(AECall pa None(map(fun n=>AEPath[n]None)names))))exp s=
 Good((v,t,bt),final)->v=XCallA target(map XVar slots).
Proof.
 intros[|f]c e pa names slots exp s v t bt final target g Slots I Resolve Lookup Kind NoBody E;[discriminate|].
 cbn[synth_block]in E;apply bind_inv in E as[[[[stmts ee]cc]div][sStmts[Stmts E]]].
 destruct f as[|f];[discriminate Stmts|]. cbn[check_stmts]in Stmts;injection Stmts as <- <- <- <- <-.
 apply bind_inv in E as[[tx tty][sTail[Tail E]]].
 pose proof(synth_abstract_call_code _ _ _ _ _ _ _ _ _ _ _ _ _ Slots I Resolve Lookup Kind NoBody Tail)as Code.
 destruct(filter _ _)as[|bad more]eqn:Gone;[|fwd_code_noerr E].
 destruct(opt_fits _ _ _)eqn:Fits;[|fwd_code_noerr E].
 injection E as <- <- <- <-;cbn[fold_stmts];exact Code.
Qed.
