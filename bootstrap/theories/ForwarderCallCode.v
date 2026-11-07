(* Exact call code in a forwarder's tail; success cannot change the slots. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ForwarderArgsCode.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Theorem synth_call_abstract_code:forall fuel c e pa names slots s v t final target g,
 LocalSlots e names slots s ->Inv s ->
 rpath(cx_mod c)pa s=Good(Some(ESym target),s)->
 nget(st_syms s)target=Some g ->gs_kind g=KFn ->fn_body g=None ->
 synth_call fuel c e pa None(map(fun n=>AEPath[n]None)names)s=Good((v,t),final)->
 v=XCallA target(map XVar slots).
Proof.
 intros[|f]c e pa names slots s v t final target g Slots I Resolve Lookup Kind NoBody E;[discriminate|].
 cbn[synth_call]in E;apply bind_inv in E as[found[s0[Read E]]].
 rewrite Resolve in Read;injection Read as <- <-.
 apply bind_inv in E as[got[s1[Sym E]]]. unfold sym in Sym;rewrite Lookup in Sym;injection Sym as <- <-.
 rewrite Kind in E. cbn[is_memory_instruction mbind ret]in E.
 apply bind_inv in E as[ms[sMem[Memory E]]].
 assert(Omem:ok memory_sym)by(unfold memory_sym;okt).
 destruct(Omem _ _ _ I Memory)as[IM FM].
 cbn[mbind ret]in E.
 assert(Defined:is_defined_fn g=false).
 {unfold is_defined_fn;rewrite Kind,NoBody;reflexivity. }
 rewrite Defined in E;destruct(nget(en_fns e)target)as[sg|]eqn:Fn;[|discriminate E].
 apply bind_inv in E as[args[sArgs[Args E]]].
 assert(ArgForm:map AA(map(fun n=>AEPath[n]None)names)=map(fun n=>AA(AEPath[n]None))names).
 {rewrite map_map;reflexivity. }
 rewrite ArgForm in Args.
 pose proof(check_args_local_code _ _ _ _ _ _ _ _ _ _
   (local_slots_frame _ _ _ _ _ Slots FM)IM Args)as Code;subst args.
 apply bind_inv in E as[u[sEnd[Layout E]]];injection E as <- <- <-;reflexivity.
Qed.
