(* A loaded defined provider can create a fixed target type internally.
   The target's promises are therefore read in its own input context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore CoreMoss BodySoundCore.
Import ListNotations.
Local Open Scope string_scope.
Module A:=ExecutionModel.Elaboration.
Definition link_boundary_out:=Spec(Some["out"])None None.
Definition link_boundary_body:list(list(ExecutionModel.Input.spec*option aexpr)):=
 [[(Spec(Some["T"])None None,Some(AEPath["Concrete"]None));
   (link_boundary_out,Some(AEPath["Concrete"]None))]].
Definition link_boundary_bundle:bundle:=Bundle "/case.moss" ""
 [AModule "/case.moss" false [] (AFile[]
  [ADType "T";ADAssume[(["T"],None)][ADVal "out"(ATRef["T"]None)None];
   ADUnit "Concrete";
   ADFunctor "Target"(Some[])[][link_boundary_out]None;
   ADFunctor "Provider" None[][link_boundary_out](Some link_boundary_body)])].
Definition link_boundary_world:Elab.world:=ltac:(
 let result:=eval vm_compute in(load_world link_boundary_bundle)in
 lazymatch result with Good ?loaded=>exact loaded end).
Lemma link_boundary_loaded:load_world link_boundary_bundle=Good link_boundary_world.
Proof. vm_compute;reflexivity. Qed.
Definition link_boundary_target:gsym:=opt_default(GSym 0 0 "" KType(ADType "")None[]OSource)(nget(w_syms link_boundary_world)4).
Definition link_boundary_provider:gsym:=opt_default(GSym 0 0 "" KType(ADType "")None[]OSource)(nget(w_syms link_boundary_world)5).
Definition link_boundary_reqs(id:nat):list nat:=if Nat.eqb id 2 then[1]else[].
Definition link_boundary_W:=W_read link_boundary_world link_boundary_reqs(fun _=>0).
Definition link_boundary_Cg:=A.entries_cx[][][].
Definition link_boundary_Cu:=A.entries_cx[][(1,TAbs 1)][].
Lemma link_boundary_symbols:
 gs_id link_boundary_target=4 /\ gs_id link_boundary_provider=5 /\
 gs_decl link_boundary_target=ADFunctor "Target"(Some[])[][link_boundary_out]None /\
 gs_decl link_boundary_provider=ADFunctor "Provider" None[][link_boundary_out](Some link_boundary_body).
Proof. vm_compute;auto. Qed.
Lemma link_boundary_provider_inputs:
 match fun_env link_boundary_provider(s_init link_boundary_world)with
 |Good((e,needs),_)=>tn_get e 1=None /\ en_tn e=[] /\ needs=[]
 |Bad _=>False end.
Proof. vm_compute;auto. Qed.
Lemma link_boundary_target_input:
 match fun_env link_boundary_target(s_init link_boundary_world)with
 |Good((e,_),_)=>tn_get e 1=Some(YAbs 1)
 |Bad _=>False end.
Proof. vm_compute;reflexivity. Qed.
Lemma link_boundary_empty_merge:A.MostGeneralMerge[][].
Proof.
 split;[intros e1 e2 k H;contradiction|];split;intros;rewrite app_subst_nil;reflexivity.
Qed.
Lemma link_boundary_provider_context:A.FunCtx link_boundary_W 5 link_boundary_Cg[].
Proof.
 exists(A.Decl "/case.moss"(ADFunctor "Provider" None[][link_boundary_out](Some link_boundary_body))[]),
 None,[],[link_boundary_out],(Some link_boundary_body),[],[],[],[],[].
 split;[vm_compute;reflexivity|];split;[reflexivity|];split;[constructor|].
 split;[intro t;split;[contradiction|intros[Bad _];discriminate]|].
 split;[reflexivity|];split;[apply link_boundary_empty_merge|];split;[reflexivity|].
 split;[constructor|intro k;split;[contradiction|intros[e[Bad _]];contradiction]].
Qed.
Lemma link_boundary_provider_missing:A.nlook 1(A.c_ty link_boundary_Cg)=None.
Proof. reflexivity. Qed.
Lemma link_boundary_target_has:A.nlook 1(A.c_ty link_boundary_Cu)=Some(TAbs 1).
Proof. reflexivity. Qed.
Lemma link_boundary_no_outputs:forall t,~A.FunOutputs link_boundary_W 4 t.
Proof.
 intros t(d&ps&a&r&body&sp&Decl&Parts&Hit&Types).
 vm_compute in Decl;injection Decl as <-.
 cbn[A.d_ast A.functor_parts]in Parts;injection Parts as <- <- <- <-.
 destruct Hit as[<-|[]];inversion Types;subst.
 all: match goal with Resolve:A.resolve_sym _ _ _=Some _|-_=>vm_compute in Resolve;injection Resolve as <- end.
 - match goal with Kind:A.kind_sym _ _=Some A.DKType|-_=>vm_compute in Kind;discriminate end.
 - match goal with Decl:A.w_decl _ _=Some _|-_=>vm_compute in Decl;injection Decl as <- end.
   cbn[A.d_ast]in *;discriminate.
Qed.
Lemma link_boundary_mentions:forall t,A.Mentions link_boundary_W 4 t<->t=1.
Proof.
 intro t;split.
 - intros(d&ps&a&r&body&sp&Decl&Parts&Hit&Mention&NotOutput).
   vm_compute in Decl;injection Decl as <-.
   cbn[A.d_ast A.functor_parts]in Parts;injection Parts as <- <- <- <-.
   destruct Hit as[<-|[]];inversion Mention;subst.
   all: try contradiction.
   all: match goal with Resolve:A.resolve_sym _ _ _=Some _|-_=>vm_compute in Resolve;injection Resolve as <- end.
   all: first[
    match goal with Req:In ?ty(A.w_reqs _ 2)\/_|-_=>destruct Req as[Req|[Kf _]];
      [change(In ty[1])in Req;destruct Req as[Eq|[]];symmetry;exact Eq|vm_compute in Kf;discriminate] end
   |match goal with Decl:A.w_decl _ _=Some _|-_=>vm_compute in Decl;injection Decl as <-;cbn[A.d_ast]in *;discriminate end].
 - intros ->.
   exists(A.Decl "/case.moss"(ADFunctor "Target"(Some[])[][link_boundary_out]None)[]),
    (Some[]),[],[link_boundary_out],None,link_boundary_out.
   split;[vm_compute;reflexivity|];split;[reflexivity|];split;[left;reflexivity|];split.
   + eapply A.RMValFn with(s:=2).
     * vm_compute;reflexivity.
     * left;reflexivity.
     * left;left;reflexivity.
     * intros[b[Bad _]];contradiction.
   + apply link_boundary_no_outputs.
Qed.
Lemma link_boundary_target_context:A.FunCtx link_boundary_W 4 link_boundary_Cu[].
Proof.
 exists(A.Decl "/case.moss"(ADFunctor "Target"(Some[])[][link_boundary_out]None)[]),
 (Some[]),[],[link_boundary_out],None,[],[],[(1,TAbs 1)],[],[1].
 split;[vm_compute;reflexivity|];split;[reflexivity|];split;[constructor|].
 split.
 - intro t;rewrite link_boundary_mentions;cbn[A.nlook];split.
   + intros[Eq|[]];auto.
   + intros[_[-> _]];left;reflexivity.
 - split;[reflexivity|];split;[apply link_boundary_empty_merge|];split;[reflexivity|].
   split;[constructor|intro k;split;[contradiction|intros[e[Bad _]];contradiction]].
Qed.
Lemma link_boundary_actual_link_succeeds:
 match link_body 30 link_boundary_target link_boundary_provider(s_init link_boundary_world)with
 |Good(h,final)=>In(h,([IVal 2],[XBTy 1(TNom 3[]);XBVal 2(XTag 3[]XUnit)]))(st_funs final)
 |Bad _=>False end.
Proof. vm_compute;auto. Qed.
Lemma link_boundary_output_needs_input:forall tm es tm',
 A.OutputItemEntries link_boundary_W "/case.moss"[]tm link_boundary_out es tm'->
 A.nlook 1 tm<>None.
Proof.
 intros tm es tm' Read;inversion Read;subst.
 all: match goal with Resolve:A.resolve_sym _ _ _=Some _|-_=>vm_compute in Resolve;injection Resolve as <- end.
 all: try(match goal with Kind:A.kind_sym _ 2=Some _|-_=>vm_compute in Kind;injection Kind as <- end).
 all: try(match goal with Decl:A.w_decl _ 2=Some _|-_=>vm_compute in Decl;injection Decl as <- end).
 all: cbn[A.d_ast]in *;try discriminate.
 all: try(match goal with Br:A.OutputBrackets _ _ _ None _|-_=>inversion Br;subst end).
 all: try(match goal with Ast:ADVal _ _ _=ADVal _ _ _|-_=>injection Ast as <- <- end).
 all: try match goal with Ty:A.TyOf _ _ _ _ (ATRef["T"]None) _|-_=>inversion Ty;subst end.
 all: try match goal with Resolve:A.resolve_sym _ _ ["T"]=Some _|-_=>vm_compute in Resolve;injection Resolve as <- end.
 all: try match goal with Lookup:A.nlook 1 ?table=Some _|-_=>rewrite Lookup;discriminate end.
 all: try match goal with Kind:A.kind_sym _ 1=Some _|-_=>vm_compute in Kind;injection Kind as <-;intuition discriminate end.
 all: try match goal with Decl:A.w_decl _ 1=Some _|-_=>vm_compute in Decl;injection Decl as <-;discriminate end.
 all: try match goal with Cases:A.DKVal=A.DKUnit \/ _|-_=>
 destruct Cases as[Bad|[Bad|[Bad|[[Eq(d&n&t&i&Decl&Ast)]|[Bad Rest]]]]];try discriminate;
 vm_compute in Decl;injection Decl as <-;discriminate Ast end.
 all: cbn[List.app]in *;match goal with Lookup:A.nlook 1 ?table=Some _|-_=>rewrite Lookup;discriminate end.
Qed.
Lemma link_boundary_provider_cannot_read:forall Cr keys,
 ~A.ProvideResult link_boundary_W 4 link_boundary_Cg(fun t=>TAbs t)Cr keys.
Proof.
 intros Cr keys(d&ps&a&r&body&outs&es&tm'&Decl&Parts&Domain&Read&Keys&Ctx).
 assert(Empty:outs=[]).
 {destruct outs as[|t rest];[reflexivity|];exfalso;apply(link_boundary_no_outputs t);apply Domain;left;reflexivity. }
 subst outs;vm_compute in Decl;injection Decl as <-.
 cbn[A.d_ast A.functor_parts]in Parts;injection Parts as <- <- <- <-.
 inversion Read;subst.
 match goal with First:A.OutputItemEntries _ _ _ _ _ _ _|-_=>
 pose proof(link_boundary_output_needs_input _ _ _ First)as Need;exact(Need eq_refl)end.
Qed.
Lemma link_boundary_target_can_read:
 exists Cr,A.ProvideResult link_boundary_W 4 link_boundary_Cu(fun t=>TAbs t)Cr[IVal 2].
Proof.
 eexists.
 exists(A.Decl "/case.moss"(ADFunctor "Target"(Some[])[][link_boundary_out]None)[]),
 (Some[]),[],[link_boundary_out],None,[],[A.NVal 2(TAbs 1)[(1,Some(TAbs 1))]],[(1,TAbs 1)].
 split;[vm_compute;reflexivity|];split;[reflexivity|];split.
 - intro t;split;[contradiction|intro Out;exfalso;exact(link_boundary_no_outputs t Out)].
 - split.
   + eapply A.OIsCons with(es1:=[A.NVal 2(TAbs 1)[(1,Some(TAbs 1))]])(es2:=[])(tm1:=[(1,TAbs 1)]);[|constructor].
     eapply A.OIEVal with(d:=A.Decl "/case.moss"(ADVal "out"(ATRef["T"]None)None)[(["T"],None)])(n:="out")(t:=ATRef["T"]None)(bs:=[]).
     * vm_compute;reflexivity.
     * vm_compute;reflexivity.
     * reflexivity.
     * constructor.
     * eapply A.TyType with(t:=1);vm_compute;reflexivity.
     * exists[1];split;[reflexivity|];split;[intro;reflexivity|intro Bad;discriminate Bad].
   + split;reflexivity.
Qed.
Definition link_boundary_method_context:=A.Cx[(1,TNom 3[])][][][]
 [((TNom 3[],2),A.FSig[]TUnit)][][][].
Lemma link_boundary_method_promised_absent:
 ~A.provides link_boundary_method_context(IMth(TAbs 1)2[]).
Proof. intro Present;apply Present;reflexivity. Qed.
Lemma link_boundary_method_at_site_present:
 A.provides link_boundary_method_context(A.at_site link_boundary_method_context(IMth(TAbs 1)2[])).
Proof. vm_compute;discriminate. Qed.
