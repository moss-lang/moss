(* Actual output-role allocations supply the fresh generation function in LB. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 ApplyFreshExec NestingTypesExec LinkOutputRoles LinkOutputProvide.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma link_output_role_member:forall (roles:list(nat*nat)) t g,
 NoDup(map fst roles)->In(t,g)roles->nassoc t roles=Some g.
Proof.
 intros roles;induction roles as[|[key value]roles IH];intros t g Distinct Hit;[contradiction|].
 inversion Distinct as[|k ks Absent Tail];subst.
 cbn[nassoc assoc fst snd];destruct Hit as[Eq|Hit].
 - injection Eq as <- <-;rewrite Nat.eqb_refl;reflexivity.
 - destruct(Nat.eqb t key)eqn:Eq;[|apply IH;assumption].
   apply Nat.eqb_eq in Eq;subst t;exfalso;apply Absent.
   apply in_map with(f:=@fst nat nat)in Hit;exact Hit.
Qed.
Lemma link_output_generation_map:forall (roles:list(nat*nat)),
 NoDup(map fst roles)->map(generation_of roles)(map fst roles)=map snd roles.
Proof.
 intros roles Distinct;rewrite map_map;apply map_ext_in;intros[t g]Hit;cbn[fst snd].
 unfold generation_of;rewrite(link_output_role_member roles t g Distinct Hit);reflexivity.
Qed.
Lemma link_output_restore_map:forall (roles:list(nat*nat)),
 NoDup(map fst roles)->
 map(fun t=>(generation_of roles t,TAbs t))(map fst roles)=map(fun p=>(snd p,TAbs(fst p)))roles.
Proof.
 intros roles Distinct;rewrite map_map;apply map_ext_in;intros[t g]Hit;cbn[fst snd].
 unfold generation_of;rewrite(link_output_role_member roles t g Distinct Hit);reflexivity.
Qed.
Section Fresh.
Variables(b:bundle)(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Hypothesis Load:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Lemma link_output_above_fresh:forall s fg id,
 Inv s->st_w s=w->nget(w_syms w)(gs_id fg)=Some fg->st_next s<=id->A.Fresh W id.
Proof using b w R Fl Load.
 intros s fg id Hi Hw Loaded Above;split.
 - change(match nget(w_syms w)id with Some g=>Some(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))|None=>None end=None).
   destruct(nget(w_syms w)id)as[g|]eqn:Read;[|reflexivity].
   pose proof(iv_loader _ Hi b ltac:(rewrite Hw;exact Load)id g ltac:(rewrite Hw;exact Read))as Existing.
   rewrite(iv_above _ Hi _ Above)in Existing;discriminate.
 - pose proof(loaded_next_positive b w Load s fg Hi Hw Loaded);lia.
Qed.
Theorem fresh_target_output_generations:forall e fg name names args res s promised keys roles final,
 nget(w_syms w)(gs_id fg)=Some fg->gs_decl fg=ADFunctor name names args res None->
 fresh_target_output_view e fg s=Good((promised,keys,roles),final)->Inv s->st_w s=w->
 exists outs,(forall t,In t outs<->A.FunOutputs W(gs_id fg)t)/\
 map fst roles=outs/\NoDup(map(generation_of roles)outs)/\
 (forall t,In t outs->A.Fresh W(generation_of roles t))/\
 map(fun t=>(generation_of roles t,TAbs t))outs=map(fun p=>(snd p,TAbs(fst p)))roles.
Proof using b w R Fl Load.
 intros e fg name names args res s promised keys roles final Loaded Decl Run Hi Hw.
 destruct(fresh_target_output_phases e fg s promised keys roles final Run)
 as(ts&typed&allocated&provided&Types&Allocate&Provide&Same&Keys&Distinct&Bounds).
 destruct(result_types_rel w R Fl b Load fg name names args res None s ts typed Loaded Decl Types Hi Hw)
 as[-> [Unique Domain]].
 assert(RoleUnique:NoDup(map fst roles))by(rewrite Keys;exact Unique).
 exists ts;split;[exact Domain|];split;[exact Keys|];split.
 - rewrite <-Keys,link_output_generation_map by exact RoleUnique;exact Distinct.
 - split.
   + intros t Hit;assert(Present:nassoc t roles<>None)by(apply link_output_role_total;rewrite Keys;exact Hit).
     destruct(nassoc t roles)as[g|]eqn:Read;[|contradiction].
     unfold generation_of;rewrite Read;cbn[opt_default].
     apply(link_output_above_fresh s fg g Hi Hw Loaded).
     exact(proj1(Bounds t g(nassoc_some_in _ _ _ _ Read))).
   + rewrite <-Keys;apply link_output_restore_map;exact RoleUnique.
Qed.
End Fresh.

Lemma link_output_renaming_actual:forall outs roles,
 map fst roles=outs->NoDup outs->forall k,
 output_role_item roles k=A.output_renaming outs(generation_of roles)k.
Proof.
 intros outs roles Keys Unique k;unfold output_role_item,A.output_renaming.
 rewrite <-Keys,link_output_restore_map;[reflexivity|rewrite Keys;exact Unique].
Qed.
