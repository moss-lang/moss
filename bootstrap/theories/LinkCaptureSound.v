(* The successful executable capture check implies LinkBody's value rule. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodySoundCore BodyTr KeyTranslation Merge CtxWalk
 FunctorContext LinkBodyTrace LinkCaptureExec.
Import ListNotations.
Module A := ExecutionModel.Elaboration.

Theorem link_capture_values_sound : forall (W : A.world) Fl fg bg ef eg fneeds gneeds xf s u final Cu Cg,
 link_trace_capture_guard fg bg ef eg fneeds gneeds xf s = Good (u,final) ->
 FunEnvTables Fl ef Cu -> FunEnvTables Fl eg Cg ->
 (forall t, In t xf <-> A.Fixed W (gs_id fg) t) ->
 forall k t, In k (map (key_tr Fl) gneeds) -> ~ In k (map (key_tr Fl) fneeds) ->
 match k with
 | IVal v => forall vt, A.nlook v (A.c_vals Cg) = Some vt -> In t (A.gens vt) ->
     A.nlook t (A.c_ty Cu) <> None -> A.Fixed W (gs_id fg) t
 | _ => True end.
Proof.
 intros W Fl fg bg ef eg fneeds gneeds xf s u final Cu Cg Run Target Provider Fixed k t Member Captured.
 destruct k as [ty | v | fn | receiver method theta | functor];try exact I.
 intros vt Value Gen Input.
 apply in_map_iff in Member as [raw [Eq Member]].
 destruct raw;cbn [key_tr] in Eq;try discriminate Eq;injection Eq as ->.
 assert (NotLinked : key_mem (KyVal v) fneeds = false).
 { destruct (key_mem (KyVal v) fneeds) eqn:Linked;[|reflexivity].
   exfalso;apply Captured;apply in_map_iff;exists (KyVal v);split;[reflexivity|].
   apply key_mem_in;exact Linked. }
 rewrite (fet_vals Fl eg Cg Provider v) in Value.
 destruct (nget (en_vals eg) v) as [rawType|] eqn:Raw;[|discriminate Value].
 cbn [option_map] in Value;injection Value as <-.
 rewrite gens_tr in Gen.
 assert (InputRaw : In t (map fst (en_tn ef))).
 { rewrite (fet_ty Fl ef Cu Target t) in Input.
   unfold tn_get in Input.
   destruct (nassoc t (en_tn ef)) as [[value stamp]|] eqn:Here.
   - apply in_map_iff;exists (t,(value,stamp));split;[reflexivity|].
     exact (Merge.nassoc_in _ _ _ _ Here).
   - exfalso;apply Input;reflexivity. }
 apply Fixed;eapply link_capture_value_fixed;eassumption.
Qed.
