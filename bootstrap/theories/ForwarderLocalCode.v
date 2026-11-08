(* The forwarder's generated parameter paths compile to their existing slots. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma synth_local_path_code:forall fuel c e n t mode exp s v out final slot,
 sassoc n(en_locals e)=Some(t,mode)->sassoc n(st_vars s)=Some slot->
 synth fuel c e(AEPath[n]None)exp s=Good((v,out),final)->v=XVar slot /\out=t.
Proof.
 intros[|f]c e n t mode exp s v out final slot Local Vars E;[discriminate|].
 cbn[synth]in E;apply bind_inv in E as[[vx tx][mid[Path E]]].
 destruct f as[|f];[discriminate Path|]. cbn[synth_path]in Path;rewrite Local in Path.
 apply bind_inv in Path as[idx[s1[Read Path]]].
 cbn[local_var mbind get]in Read;rewrite Vars in Read;cbn[ret]in Read;injection Read as <- <-.
 injection Path as <- <- <-.
 apply bind_inv in E as[u[s2[Inject E]]];injection E as <- <- <-;split;reflexivity.
Qed.
Lemma synth_arg_local_path_code:forall fuel c e n t mode exp s v out final slot,
 sassoc n(en_locals e)=Some(t,mode)->sassoc n(st_vars s)=Some slot->
 synth_arg fuel c e(AA(AEPath[n]None))exp s=Good((v,out),final)->v=XVar slot /\out=t.
Proof. intros[|f]c e n t mode exp s v out final slot Local Vars E;[discriminate|].
 cbn[synth_arg]in E;eapply synth_local_path_code;eassumption. Qed.
