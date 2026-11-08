(* Exact argument slots of a synthesized forwarder, from real execution. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ForwarderLocalCode.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Definition LocalSlots(e:env)(names:list name)(slots:list nat)(s:st):Prop:=
 Forall2(fun n slot=>exists t mode,sassoc n(en_locals e)=Some(t,mode)/\sassoc n(st_vars s)=Some slot)names slots.
Lemma local_slots_frame:forall e names slots s s',LocalSlots e names slots s ->Fr s s'->LocalSlots e names slots s'.
Proof.
 intros e names slots s s' Slots Frame;destruct(fx_vars _ _(fr_x _ _ Frame))as[more Eq].
 eapply Forall2_impl;[|exact Slots]. intros n slot[t[mode[L V]]];exists t,mode;split;[exact L|].
 rewrite Eq,sassoc_app,V;reflexivity.
Qed.
Definition forward_arguments(f:nat)(c:cx)(e:env)(nm:string)(params:list(name*ety))(args:list arg):M(list xexpr):=
 mmap(fun pa=>let '((pn,pty),a):=pa in let! (v,t):=synth_arg f c e a(Some pty)in
 if fits e t pty then ret v else let! x:=show t in let! y:=show pty in
 err(cx_mod c)("argument `"++pn++"` of `"++nm++"` is `"++x++"`, expected `"++y++"`"))(combine params args).
Lemma forward_arguments_code:forall fuel c e nm params names slots s xs final,
 List.length params=List.length names->LocalSlots e names slots s->Inv s->
 forward_arguments fuel c e nm params(map(fun n=>AA(AEPath[n]None))names)s=Good(xs,final)->
 xs=map XVar slots.
Proof.
 intros fuel c e nm params;induction params as[|[pn pty]params IH];intros names slots s xs final Len Slots I E.
 - destruct names;[|discriminate Len]. inversion Slots;subst slots;cbn[forward_arguments mmap]in E;injection E as <- <-;reflexivity.
 - destruct names as[|n names];[discriminate Len|]. inversion Slots as[|n0 slot names0 rest Head Tail];subst.
   destruct Head as[t[mode[Local Vars]]].
   cbn[forward_arguments map combine mmap]in E.
   apply bind_inv in E as[v[mid[Step E]]].
   apply bind_inv in Step as[[vx tx][sArg[Arg Step]]].
   destruct(synth_arg_local_path_code _ _ _ _ _ _ _ _ _ _ _ _ Local Vars Arg)as[-> TypeEq].
   destruct(ok_synth_arg fuel c e(AA(AEPath[n]None))(Some pty)_ _ _ I Arg)as[IA FA].
   destruct(fits e tx pty)eqn:Fits.
   + injection Step as <- <-.
     apply bind_inv in E as[ys[last[Rest E]]];injection E as <- <-.
     cbn[map];f_equal;eapply IH with(names:=names);[cbn[List.length]in Len;lia|eapply local_slots_frame;[exact Tail|exact FA]|exact IA|exact Rest].
   + exfalso;repeat apply bind_inv in Step as[?[?[? Step]]];discriminate Step.
Qed.
Lemma check_args_local_code:forall fuel c e nm sg names slots s xs final,
 LocalSlots e names slots s->Inv s->
 check_args fuel c e nm sg(map(fun n=>AA(AEPath[n]None))names)s=Good(xs,final)->xs=map XVar slots.
Proof.
 intros[|f]c e nm sg names slots s xs final Slots I E;[discriminate|]. cbn[check_args]in E.
 destruct(negb(Nat.eqb(List.length(map(fun n=>AA(AEPath[n]None))names))(List.length(sg_params sg))))eqn:Len.
 - discriminate E.
 - apply negb_false_iff in Len;apply Nat.eqb_eq in Len;rewrite map_length in Len.
   eapply forward_arguments_code;[symmetry;exact Len|exact Slots|exact I|exact E].
Qed.
