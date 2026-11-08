(* An unformable declaration interface is an uninhabitable capability, not
   a fabricated empty functor. Real checked provider profiles take priority. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss NMap Sx Elab ReifyData Reify ReifySource
 ReifyFunctors ReifyFunctorProfiles ReifyProfileGraph ReifySig.
Import ListNotations.
Definition source_functor_identity(source:st)(original:usym):bool:=
 match nget(st_syms source)original with
 | Some symbol=>kind_eqb(gs_kind symbol)KFunctor|None=>false end.
Definition opaque_interface_receipt(source:st)(original:usym):Prop:=
 source_functor_identity source original=true /\ declared_functor_profile source original=None.
Fixpoint ensure_interface_nodes(source:st)(todo:list usym)
 (profiles:list(usym*reify_exports ety key))(opaque:list usym)
 :option(list(usym*reify_exports ety key)*list usym):=
 match todo with
 | []=>Some(profiles,opaque)
 | original::rest=>match assoc Nat.eqb original profiles with
   | Some _=>ensure_interface_nodes source rest profiles opaque
   | None=>if existsb(Nat.eqb original)opaque then ensure_interface_nodes source rest profiles opaque
     else match declared_functor_profile source original with
       | Some profile=>ensure_interface_nodes source rest(profiles++[(original,profile)])opaque
       | None=>if source_functor_identity source original
           then ensure_interface_nodes source rest profiles(original::opaque)else None
       end end
 end.
Fixpoint expand_interface_nodes(fuel:nat)(source:st)(todo seen:list usym)
 (profiles:list(usym*reify_exports ety key))(opaque:list usym)
 :option(list(usym*reify_exports ety key)*list usym):=
 match todo with
 | []=>Some(profiles,opaque)
 | original::rest=>match fuel with
   | 0=>None
   | S remaining=>match assoc Nat.eqb original profiles with
     | None=>if existsb(Nat.eqb original)opaque
         then expand_interface_nodes remaining source rest(original::seen)profiles opaque else None
     | Some profile=>
       let children:=filter(fun u=>negb(existsb(Nat.eqb u)(original::seen++rest)))
         (nodup Nat.eq_dec(profile_children profile))in
       match ensure_interface_nodes source children profiles opaque with
       | Some(complete,opaque)=>expand_interface_nodes remaining source(rest++children)
           (original::seen)complete opaque
       | None=>None end end end end.
Definition complete_interface_graph(source:st)(needed:list usym)
 (profiles:list(usym*reify_exports ety key)):=
 match ensure_interface_nodes source needed profiles[]with
 | Some(profiles,opaque)=>expand_interface_nodes(S(st_next source))source
     (nodup Nat.eq_dec(map fst profiles++opaque))[]profiles opaque
 | None=>None end.

Lemma ensured_opaque_receipts : forall source todo profiles opaque complete after,
 ensure_interface_nodes source todo profiles opaque=Some(complete,after)->
 Forall(opaque_interface_receipt source)opaque->Forall(opaque_interface_receipt source)after.
Proof.
 intros source todo;induction todo as[|original rest IH];intros profiles opaque complete after Run Receipts.
 - inversion Run;subst;exact Receipts.
 - cbn[ensure_interface_nodes]in Run.
   destruct(assoc Nat.eqb original profiles);[eapply IH;eassumption|].
   destruct(existsb(Nat.eqb original)opaque);[eapply IH;eassumption|].
   destruct(declared_functor_profile source original)eqn:Profile;[eapply IH;eassumption|].
   destruct(source_functor_identity source original)eqn:Identity;[|discriminate].
   eapply IH;[exact Run|];constructor;[split;assumption|exact Receipts].
Qed.
Lemma expanded_opaque_receipts : forall fuel source todo seen profiles opaque complete after,
 expand_interface_nodes fuel source todo seen profiles opaque=Some(complete,after)->
 Forall(opaque_interface_receipt source)opaque->Forall(opaque_interface_receipt source)after.
Proof.
 induction fuel as[|fuel IH];intros source todo seen profiles opaque complete after Run Receipts;
 destruct todo as[|original rest];cbn[expand_interface_nodes]in Run;
 try(inversion Run;subst;exact Receipts);try discriminate.
 destruct(assoc Nat.eqb original profiles).
 - destruct(ensure_interface_nodes source _ profiles opaque)as[[middle markers]|]eqn:Ensure;[|discriminate].
   eapply IH;[exact Run|];eapply ensured_opaque_receipts;eassumption.
 - destruct(existsb(Nat.eqb original)opaque);[eapply IH;eassumption|discriminate].
Qed.
Theorem completed_opaque_receipts : forall source needed profiles complete opaque,
 complete_interface_graph source needed profiles=Some(complete,opaque)->
 Forall(opaque_interface_receipt source)opaque.
Proof.
 intros source needed profiles complete opaque Run;unfold complete_interface_graph in Run.
 destruct(ensure_interface_nodes source needed profiles[])as[[middle markers]|]eqn:Ensure;[|discriminate].
 eapply expanded_opaque_receipts;[exact Run|].
 eapply ensured_opaque_receipts;[exact Ensure|constructor].
Qed.

Fixpoint nested_views_with_opaque(source:st)(defaults:subst)
 (profiles:list reserved_functor_profile)(opaque:list usym)(scope:reify_scope ety key)
 (runtime:reify_state)(slots:list(usym*usym)):option(list reify_functor_view):=
 match slots with
 | []=>Some[]
 | (original,slot)::rest=>match lookup_reserved_functor original profiles with
   | Some child=>match nested_captured_interpretation source defaults scope runtime child,
       nested_views_with_opaque source defaults profiles opaque scope runtime rest with
       | Some captured,Some views=>Some(reserved_view source defaults child slot captured::views)
       | _,_=>None end
   | None=>if existsb(Nat.eqb original)opaque
       then nested_views_with_opaque source defaults profiles opaque scope runtime rest else None end end.
Fixpoint complete_views_with_opaque(source:st)(defaults:subst)
 (all todo:list reserved_functor_profile)(opaque:list usym):option(list reify_functor_view):=
 match todo with
 | []=>Some[]
 | profile::rest=>let draft:=rfp_draft(rsp_profile profile)in
   match nested_views_with_opaque source defaults all opaque(rfi_input_scope draft)(rfi_input_runtime draft)
       (rsp_input_functors profile),
     nested_views_with_opaque source defaults all opaque(rfi_output_scope draft)(rfi_output_runtime draft)
       (rsp_output_functors profile),complete_views_with_opaque source defaults all rest opaque with
   | Some inputs,Some outputs,Some others=>
       let fixed:=profile_fixed_roles(rsp_profile profile)defaults in
       Some(reserved_view source defaults profile(rfi_slot draft)
         (map(fun t=>(t,TAbs t))fixed)::inputs++outputs++others)
   | _,_,_=>None end end.
Definition opaque_slot_declarations(opaque:list usym)(slots:list(usym*usym)):
 list(item*reify_decl):=
 flat_map(fun p=>if existsb(Nat.eqb(fst p))opaque
   then[(IFun(snd p),RDOpaqueFunctor(fst p))]else[])slots.
Definition prepare_templates_with_opaque(source:st)(defaults:subst)
 (profiles:list(usym*reify_exports ety key))(opaque:list usym)(before:reify_state)
 :option(functor_work*reify_state):=
 match prepare_functor_profiles source defaults profiles[]before with
 | Some(profiles,middle)=>match reserve_nested_functor_profiles profiles middle with
   | Some(reserved,middle)=>match reserve_functor_slots(map KyFun opaque)middle with
     | Some(markers,after)=>match complete_views_with_opaque source defaults reserved reserved opaque with
       | Some views=>Some(FunctorWork views[][],
           ReifyState(rf_next after)(rf_names before)(rf_generations before)(rf_items before)
            (map(fun view=>(IFun(rfv_slot view),RDFunctor(rfv_schema view)))views++
             opaque_slot_declarations opaque(markers++flat_map(fun p=>rsp_input_functors p++rsp_output_functors p)reserved)
             ++rf_declarations after))
       | None=>None end
     | None=>None end
   | None=>None end
 | None=>None end.
Definition has_opaque_original(original:usym)(declarations:list(item*reify_decl)):bool:=
 existsb(fun p=>match p with(IFun _,RDOpaqueFunctor source)=>Nat.eqb original source|_=>false end)declarations.
Definition install_opaque_functor(original:usym)(before:reify_state):usym*reify_state:=
 let slot:=rf_next before in
 (slot,ReifyState(S slot)(rf_names before)(rf_generations before)
   ((IFun original,IFun slot)::rf_items before)
   ((IFun slot,RDOpaqueFunctor original)::rf_declarations before)).
Definition scope_has_opaque_functor(items:tele)(declarations:list(item*reify_decl)):bool:=
 existsb(fun i=>match i,lookup_reify_decl i declarations with
   | IFun _,Some(RDOpaqueFunctor _)=>true|_,_=>false end)items.
Lemma scope_has_opaque_functor_receipt : forall items declarations,
 scope_has_opaque_functor items declarations=true->
 exists u original,In(IFun u)items /\lookup_reify_decl(IFun u)declarations=Some(RDOpaqueFunctor original).
Proof.
 intros items declarations Run;apply existsb_exists in Run as[i[Member Run]].
 destruct i;try discriminate.
 destruct(lookup_reify_decl(IFun u)declarations)as[d|]eqn:Read;[|discriminate].
 destruct d;try discriminate;eauto.
Qed.
Lemma scope_has_opaque_functor_missing : forall base items declarations,
 scope_has_opaque_functor items declarations=true->
 exists u,In(IFun u)items /\g_opaque_fun(reify_signature_overlay base declarations)u=true /\
   g_fun(reify_signature_overlay base declarations)u=None.
Proof.
 intros base items declarations Run.
 destruct(scope_has_opaque_functor_receipt _ _ Run)as[u[original[Member Read]]].
 destruct(reify_signature_opaque_functor base declarations u original Read)as[Missing Marked].
 exists u;repeat split;assumption.
Qed.

Lemma ensured_interface_profiles_have_origins : forall source todo profiles opaque complete after initial,
 ensure_interface_nodes source todo profiles opaque=Some(complete,after)->
 Forall(interface_profile_origin source initial)profiles->
 Forall(interface_profile_origin source initial)complete.
Proof.
 intros source todo;induction todo as[|original rest IH];intros profiles opaque complete after initial Run Origins.
 - inversion Run;subst;exact Origins.
 - cbn[ensure_interface_nodes]in Run.
   destruct(assoc Nat.eqb original profiles);[eapply IH;eassumption|].
   destruct(existsb(Nat.eqb original)opaque);[eapply IH;eassumption|].
   destruct(declared_functor_profile source original)eqn:Profile.
   + eapply IH;[exact Run|];apply Forall_app;split;[exact Origins|].
     constructor;[right;exact Profile|constructor].
   + destruct(source_functor_identity source original);[eapply IH;eassumption|discriminate].
Qed.
Lemma expanded_interface_profiles_have_origins : forall fuel source todo seen profiles opaque complete after initial,
 expand_interface_nodes fuel source todo seen profiles opaque=Some(complete,after)->
 Forall(interface_profile_origin source initial)profiles->
 Forall(interface_profile_origin source initial)complete.
Proof.
 induction fuel as[|fuel IH];intros source todo seen profiles opaque complete after initial Run Origins;
 destruct todo as[|original rest];cbn[expand_interface_nodes]in Run;
 try(inversion Run;subst;exact Origins);try discriminate.
 destruct(assoc Nat.eqb original profiles).
 - destruct(ensure_interface_nodes source _ profiles opaque)as[[middle markers]|]eqn:Ensure;[|discriminate].
   eapply IH;[exact Run|];eapply ensured_interface_profiles_have_origins;eassumption.
 - destruct(existsb(Nat.eqb original)opaque);[eapply IH;eassumption|discriminate].
Qed.
Theorem completed_interface_profiles_have_origins : forall source needed initial complete opaque,
 complete_interface_graph source needed initial=Some(complete,opaque)->
 Forall(interface_profile_origin source initial)complete.
Proof.
 intros source needed initial complete opaque Run;unfold complete_interface_graph in Run.
 destruct(ensure_interface_nodes source needed initial[])as[[middle markers]|]eqn:Ensure;[|discriminate].
 eapply expanded_interface_profiles_have_origins;[exact Run|].
 eapply ensured_interface_profiles_have_origins;[exact Ensure|].
 apply Forall_forall;intros entry Member;left;exact Member.
Qed.
Lemma has_opaque_original_receipt : forall original declarations,
 has_opaque_original original declarations=true->
 exists slot,In(IFun slot,RDOpaqueFunctor original)declarations.
Proof.
 intros original declarations Run;apply existsb_exists in Run as[[i d][Member Run]].
 destruct i;destruct d;try discriminate;apply Nat.eqb_eq in Run;subst;eauto.
Qed.
Lemma installed_opaque_functor_identity : forall original before slot after,
 install_opaque_functor original before=(slot,after)->
 slot=rf_next before /\rf_next after=S slot /\
 lookup_reify_decl(IFun slot)(rf_declarations after)=Some(RDOpaqueFunctor original) /\
 lookup_smap(IFun original)(rf_items after)=Some(IFun slot) /\
 rf_names after=rf_names before /\rf_generations after=rf_generations before.
Proof.
 intros original before slot after Run;inversion Run;subst;cbn.
 rewrite !Nat.eqb_refl;repeat split;reflexivity.
Qed.
