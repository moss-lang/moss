(* Eta adapters use the provider's own canonical declaration context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 InfoInvariant InfoPreserve InfoCache BodySound BodyTy BodyTr Merge CtxMerge FnBodyContext
 WrapperInfoContext WrapperReceipt WrapperResult CallThetaExec SmapExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements canon].
Section Eta.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Theorem actual_wrapper_eta:forall fuel pv s id final,
 nget(w_syms w)(gs_id pv)=Some pv->wrapper fuel pv s=Good(id,final)->
 Inv s->InfoInv s->st_w s=w->Later w P R Fl final->
 exists Cg ns,A.CtxOf(BodySound.W w R Fl)(gs_id pv)Cg ns /\
 A.Fresh(BodySound.W w R Fl)id /\
 A.body_of P id=Some(XCallD(gs_id pv)(A.theta_at(BodySound.W w R Fl)Cg(gs_id pv))
   (map(fun k=>(A.rkey_of Cg k,A.rkey_of Cg k))ns)
   (map XVar(seq 1(List.length(fn_params pv))))).
Proof.
 intros fuel pv s id final Loaded Run Is Cache Hw Late.
 destruct(proj1(okI_wrapper fuel pv)_ _ _ Is Run)as[If Frame].
 pose proof(proj2(okI_wrapper fuel pv)_ _ _ Is Cache Run)as Cf.
 assert(Wf:st_w final=w)by(rewrite(fr_w _ _ Frame);exact Hw).
 pose proof(wrapper_receipt _ _ _ _ _ Is Cache Run)as Receipt.
 pose proof Receipt as Entry.
 destruct Entry as(g&info&th&sm&sms&sme&ths&the&news&newe&Id&Physical&Info&Im&Map&Fm&It&Theta&Ft&Inw&Phnew&New&Fn&Symbol&Row).
 assert(g=pv).
 {pose proof(iv_loader _ If b ltac:(rewrite Wf;exact Hload)_ _ ltac:(rewrite Wf;exact Loaded))as Here.
  rewrite Here in Physical;injection Physical;auto. }
 subst g.
 destruct(retained_info_cxrel w R Fl b Hload Hworld pv final info Loaded Info If Cf Wf Late)as[Cg[Context Own]].
 pose proof(retained_info_keys w R Fl b Hload Hworld pv final info Loaded Info If Cf Wf Late)as Keys.
 assert(Apps:Forall(fun p=>key_apps Fl(fi_env info)Cg(fst p)/\key_apps Fl(fi_env info)Cg(snd p))
   (map(fun k=>(k,k))(fi_needs info))).
 {apply Forall_forall;intros pair Member;apply in_map_iff in Member as[k[<- Hin]].
  rewrite Forall_forall in Keys;specialize(Keys k Hin);destruct k;cbn[key_apps fst snd];try tauto.
  split;exact(cr_apps _ _ _ _ _ _ _ Own _ _ Keys). }
 assert(Sm:sm=map(fun k=>(A.rkey_of Cg k,A.rkey_of Cg k))(map(key_tr Fl)(fi_needs info))).
 {rewrite(smap_at_context w R Fl _ _ _ _ _ _ _ _ Map Im(later_fromL w R Fl _ _ Fm Late)
   (cr_sub _ _ _ _ _ _ _ Own)(proj1(cr_uf _ _ _ _ _ _ _ Own))
   (cr_sub _ _ _ _ _ _ _ Own)(proj1(cr_uf _ _ _ _ _ _ _ Own))Apps).
  rewrite !map_map;reflexivity. }
 assert(Th:th=A.theta_at(BodySound.W w R Fl)Cg(gs_id pv)).
 {eapply call_theta_tr;[exact Theta|exact It|exact(later_fromL w R Fl _ _ Ft Late)|exact Own]. }
 exists Cg,(map(key_tr Fl)(fi_needs info));split;[exact Context|];split.
 - eapply wrapper_entry_fresh;[exact Hload|exact Wf|exact Receipt].
 - pose proof(lt_lambdas _ _ _ _ _ Late _ _ _ Row Symbol eq_refl)as Body.
   rewrite Sm,Th in Body;exact Body.
Qed.
End Eta.
