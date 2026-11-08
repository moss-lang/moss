(* Fresh linking derives its input contexts, capture condition and body judgment
   from execution. Result interpretation is discharged by the output bridge. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr BodyTy KeyTranslation InfoInvariant InfoInputs InfoPreserve
 FwdReadonly ElabKeeps FwdInvariant FwdPreserve FwdSynthInv FunctorContext ResultMentionsExact
 NestingTypesExec BindSequence LinkBodyTrace LinkBodyFrame LinkBodyItems LinkCaptureSound SemanticPins FinalLinks
 FunctorBodyContext FunctorAbstractBodyContext ForwarderTrace LinkBindsExec
 ApplyFreshExec LinkOutputRoles LinkOutputFresh LinkPins.
Import ListNotations.
Module A := ExecutionModel.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Section Sound.
Variable w : Elab.world. Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Notation W := (BodySoundCore.W w R Fl).

Record link_body_data (fg bg : gsym) (h : nat) (final : st) : Type := {
 lbd_ef : env; lbd_fneeds : list key;
 lbd_eg : env; lbd_gneeds : list key;
 lbd_fixed : list nat; lbd_outputs : list nat;
 lbd_promised : env; lbd_keys : list key; lbd_roles : list (nat * nat);
 lbd_stmts : list xstmt; lbd_body_env : env;
 lbd_Cu : A.cx; lbd_Cg : A.cx; lbd_Cb : A.cx;
 lbd_Fg : A.frame; lbd_Fb : A.frame;
 lbd_made : list nat; lbd_wrap : xexpr -> xexpr; lbd_inner : cx;
 lbd_target_context : A.FunCtx W (gs_id fg) lbd_Cu (map (key_tr Fl) lbd_fneeds);
 lbd_target_tables : FunEnvTables Fl lbd_ef lbd_Cu;
 lbd_target_body_context : CxRel w R Fl (functor_body_context fg []) lbd_ef
   (functor_body_frame w fg [] []) lbd_Cu;
 lbd_provider_context : A.FunCtx W (gs_id bg) lbd_Cg (map (key_tr Fl) lbd_gneeds);
 lbd_provider_tables : FunEnvTables Fl lbd_eg lbd_Cg;
 lbd_captures : forall k t, In k (map (key_tr Fl) lbd_gneeds) ->
   ~ In k (map (key_tr Fl) lbd_fneeds) ->
   match k with IVal v => forall vt, A.nlook v (A.c_vals lbd_Cg) = Some vt ->
     In t (A.gens vt) -> A.nlook t (A.c_ty lbd_Cu) <> None -> A.Fixed W (gs_id fg) t
   | _ => True end;
 lbd_output_types : forall t, In t lbd_outputs <-> A.FunOutputs W (gs_id fg) t;
 lbd_outputs_unique : NoDup lbd_outputs;
 lbd_roles_domain : map fst lbd_roles = lbd_outputs;
 lbd_generations_unique : NoDup (map (generation_of lbd_roles) lbd_outputs);
 lbd_generations_fresh : forall t, In t lbd_outputs -> A.Fresh W (generation_of lbd_roles t);
 lbd_body_module : A.fr_mod lbd_Fg = gm_path (get_mod w (gs_mod bg));
 lbd_body_items : forall bodies, (exists name names args res,
   gs_decl bg = ADFunctor name names args res (Some bodies)) ->
   A.BindItems W P lbd_Fg lbd_Cg (List.concat bodies) lbd_wrap lbd_Cb lbd_Fb lbd_made;
 lbd_body_fold : forall tail, lbd_wrap tail = fold_stmts lbd_stmts tail;
 lbd_body_context : CxRel w R Fl lbd_inner lbd_body_env lbd_Fb lbd_Cb;
 lbd_body_pins : forall t,
   In t (cx_pins (link_trace_context bg lbd_promised lbd_fixed lbd_outputs lbd_keys lbd_roles)) ->
   canon lbd_body_env (opt_default (YAbs t) (tn_get lbd_body_env t)) = canon lbd_body_env (YAbs t);
 lbd_target_env : exists before after,
   fun_env fg before = Good ((lbd_ef,lbd_fneeds),after) /\
   Inv before /\ st_w before = w /\ Later w P R Fl after;
 lbd_output_view : exists before after,
   fresh_target_output_view lbd_ef fg before = Good ((lbd_promised,lbd_keys,lbd_roles),after) /\
   Inv before /\ st_w before = w /\ Later w P R Fl after;
 lbd_after_body : exists before,
   link_trace_after_body fg bg lbd_ef lbd_eg lbd_promised lbd_body_env
    lbd_fixed lbd_outputs lbd_keys lbd_roles lbd_stmts before = Good (h,final) /\
   Inv before /\ Later w P R Fl before
}.

Lemma link_finish_fresh : forall fg bg ef promised eb rts dr roles binds s h final,
 nget (w_syms w) (gs_id fg) = Some fg ->
 link_trace_finish fg bg ef promised eb rts dr roles binds s = Good (h,final) ->
 Inv s -> st_w s = w -> A.Fresh W h.
Proof.
 intros fg bg ef promised eb rts dr roles binds s h final Loaded Run Valid Hw.
 destruct (link_body_fresh_emission fg bg ef promised eb rts dr roles binds s h final Run)
  as (exported&allocated&dri&indexed&Exports&New&Index&Final&Funs&Links).
 assert (O : ok (record_reify_exports_at (ROLink (gs_id fg) (gs_id bg)) rts dr ef promised eb roles)) by okt.
 destruct (O _ _ _ Valid Exports) as [IE FE].
 assert (WE : st_w exported = w) by (rewrite (fr_w _ _ FE);exact Hw).
 pose proof (proj1 (new_sym_shape _ _ _ _ _ _ _ _ New)) as Id;subst h.
 split.
 - change (match nget (w_syms w) (st_next exported) with
    | Some g => Some (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g))
    | None => None end = None).
   destruct (nget (w_syms w) (st_next exported)) as [g|] eqn:Here;[|reflexivity].
   pose proof (iv_loader _ IE b ltac:(rewrite WE;exact Load) _ _ ltac:(rewrite WE;exact Here)) as Hit.
   rewrite (iv_above _ IE _ (le_n _)) in Hit;discriminate.
 - assert (Below : ~ st_next exported <= gs_id fg).
   { intro Bound.
     pose proof (iv_loader _ IE b ltac:(rewrite WE;exact Load) _ _ ltac:(rewrite WE;exact Loaded)) as Hit.
     rewrite (iv_above _ IE _ Bound) in Hit;discriminate. }
   lia.
Qed.
Lemma link_finish_program_body : forall fg bg ef promised eb rts dr roles binds s h final,
 link_trace_finish fg bg ef promised eb rts dr roles binds s = Good (h,final) ->
 FinalLinks P final -> exists exported allocated dri indexed,
 record_reify_exports_at (ROLink (gs_id fg) (gs_id bg)) rts dr ef promised eb roles s = Good (tt,exported) /\
 new_sym (gs_mod bg) (gs_name bg) KFunctor (gs_decl bg) (OLink (gs_id fg) (gs_id bg)) exported = Good (h,allocated) /\
 mmap (fun k => let! it := key_item_at promised k in ret (output_role_item roles it)) dr allocated = Good (dri,indexed) /\
 A.funbody_of P h = Some (map ITy rts ++ dri,binds).
Proof.
 intros fg bg ef promised eb rts dr roles binds s h final Run Last.
 destruct (link_body_fresh_emission fg bg ef promised eb rts dr roles binds s h final Run)
  as (exported&allocated&dri&indexed&Exports&New&Index&Final&Funs&Links).
 exists exported,allocated,dri,indexed;repeat split;try assumption.
 apply Last;rewrite Funs;left;reflexivity.
Qed.

Ltac link_frame E I Iout Frame :=
 lazymatch type of E with ?m ?s = Good (?a,?last) =>
 let O := fresh "O" in assert (O : ok m) by okt;
 destruct (O s a last I E) as [Iout Frame];clear O end.
Ltac link_info E I Cache Result :=
 lazymatch type of E with ?m ?s = Good (?a,?last) =>
 let O := fresh "O" in assert (O : okI m) by okIt;
 pose proof (proj2 O s a last I Cache E) as Result;clear O end.
Ltac link_forward E I Forward Result :=
 lazymatch type of E with ?m ?s = Good (?a,?last) =>
 let O := fresh "O" in assert (O : okF m) by (first [solve [auto with fwdb]|apply okF_readonly;[okt|keepst]]);
 pose proof (proj2 O s a last I Forward E) as Result;clear O end.
Ltac link_worlds := repeat multimatch goal with
 | F : Fr ?s ?s', H : st_w ?s = w |- _ =>
 tryif (match goal with _ : st_w s' = w |- _ => idtac end) then fail 0 else
 let N := fresh "World" in assert (N : st_w s' = w) by (rewrite (fr_w _ _ F);exact H)
 end.
Ltac link_laters := repeat multimatch goal with
 | F : Fr ?s ?s', L : Later w P R Fl ?s' |- _ =>
 tryif (match goal with _ : Later w P R Fl s |- _ => idtac end) then fail 0 else
 let N := fresh "Last" in assert (N : Later w P R Fl s) by exact (later_back w P R Fl _ _ F L)
 end.

Theorem link_body_fresh_data : forall f, OneStep w P R Fl f ->
 forall fg bg fname fps fargs fres gname gps gargs gres bodies s h final,
 A.WorldOf b W ->
 nget (w_syms w) (gs_id fg) = Some fg -> gs_decl fg = ADFunctor fname fps fargs fres None ->
 nget (w_syms w) (gs_id bg) = Some bg -> gs_decl bg = ADFunctor gname gps gargs gres (Some bodies) ->
 assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b))
  (gs_id fg,gs_id bg) (st_links s) = None ->
 link_body (S (S f)) fg bg s = Good (h,final) ->
 Inv s -> InfoInv s -> FwdInv s -> st_w s = w -> Later w P R Fl final ->
 inhabited (link_body_data fg bg h final).
Proof.
 intros f Step fg bg fname fps fargs fres gname gps gargs gres bodies s h final
  World Target TargetDecl Provider ProviderDecl Miss Run Valid Cache Forward Hw Last.
 destruct (link_body_fresh_inputs (S f) fg bg s h final Miss Run)
  as (ef&fneeds&sf&eg&gneeds&sg&xf&sx&rts&sr&sc&EF&EG&XF&RT&Capture&AfterCapture).
 link_frame EF Valid IF FF;link_info EF Valid Cache CF;link_forward EF Valid Forward VF.
 link_frame EG IF IG FG;link_info EG IF CF CG;link_forward EG IF VF VG.
 link_frame XF IG IX FX;link_info XF IG CG CX;link_forward XF IG VG VX.
 link_frame RT IX IR FR;link_info RT IX CX CR;link_forward RT IX VX VR.
 link_frame Capture IR IC FC;link_info Capture IR CR CC;link_forward Capture IR VR VC.
 destruct (link_body_fresh_pin_receipt (S f) fg bg ef eg gneeds xf rts sc h final AfterCapture)
  as (promised&keys&roles&sv&registered&xs&eb&exit&bodyFinal&View&Register&Body&PinReceipt&AfterBody).
 link_frame View IC InvView FrameView;link_info View IC CC CacheView;link_forward View IC VC ForwardView.
 link_frame Register InvView IRegistered FRegistered.
 link_info Register InvView CacheView CRegistered;link_forward Register InvView ForwardView VRegistered.
 link_frame Body IRegistered IBody FBody.
 link_frame AfterBody IBody IFinal FAfter.
 link_worlds;link_laters.
 destruct (abstract_fun_env_body_context w (P:=P) R Fl b Load fg fname fps fargs fres s ef fneeds sf [] []
  World Target TargetDecl EF Valid Hw ltac:(eassumption))
  as (Cu&TargetContext&TargetTables&TargetBodyContext).
 pose proof (functor_fixed_exact w (P:=P) R Fl b Load fg fname fps fargs fres sg xf sx
  Target TargetDecl XF IG ltac:(eassumption) ltac:(eassumption)) as Fixed.
 destruct (result_types_rel w R Fl b Load fg fname fps fargs fres None sx rts sr
  Target TargetDecl RT IX ltac:(eassumption)) as [Types [OutputUnique OutputDomain]].
 assert (CaptureState : sc = sr) by (eapply link_trace_capture_state;exact Capture).
 assert (TypesHere : result_types fg sc = Good (rts,sc)).
 { rewrite CaptureState,Types;rewrite Types in RT;exact RT. }
 destruct (fresh_target_output_phases ef fg sc promised keys roles sv View)
  as (viewOutputs&viewTypes&viewAllocated&viewProvided&ViewTypes&ViewAllocate&ViewProvide&ViewSame&RoleKeys&RoleDistinct&RoleBounds).
 rewrite TypesHere in ViewTypes;injection ViewTypes as <- <-.
 destruct (fresh_target_output_generations b w R Fl Load ef fg fname fps fargs fres sc promised keys roles sv
  Target TargetDecl View IC ltac:(eassumption)) as (roleOutputs&RoleDomain&RoleKeys'&GenUnique&GenFresh&Restore).
 assert (SameOutputs : roleOutputs = rts) by congruence;subst roleOutputs.
 rewrite RoleKeys in GenUnique,GenFresh,Restore.

 set (pins := cx_pins (link_trace_context bg promised xf (rts) keys roles)).
 change (in_reify_code (ROLink (gs_id fg) (gs_id bg))
   (with_vars [] (apply_body (S f) (functor_body_context bg pins) eg bg)) registered =
    Good ((xs,eb,exit),bodyFinal)) in Body.
 destruct (link_body_scoped_items w (P:=P) R Fl b Load f Step bg gname gps gargs gres bodies
  sf eg gneeds sg pins (ROLink (gs_id fg) (gs_id bg)) registered xs eb exit bodyFinal
  World Provider ProviderDecl EG IF ltac:(eassumption) ltac:(eassumption)
  Body IRegistered CRegistered VRegistered ltac:(eassumption))
  as (Cg&Cb&Fg&Fb&made&wrap&inner&ProviderContext&ProviderTables&Module&Pins&Items&Fold&BodyContext).
 constructor.
 refine {| lbd_ef:=ef; lbd_fneeds:=fneeds; lbd_eg:=eg; lbd_gneeds:=gneeds;
  lbd_fixed:=xf; lbd_outputs:=rts; lbd_promised:=promised;
  lbd_keys:=keys; lbd_roles:=roles; lbd_stmts:=xs; lbd_body_env:=eb;
  lbd_Cu:=Cu; lbd_Cg:=Cg; lbd_Cb:=Cb; lbd_Fg:=Fg; lbd_Fb:=Fb;
  lbd_made:=made; lbd_wrap:=wrap; lbd_inner:=inner;
  lbd_target_context:=TargetContext; lbd_target_tables:=TargetTables; lbd_target_body_context:=TargetBodyContext;
  lbd_provider_context:=ProviderContext; lbd_provider_tables:=ProviderTables;
  lbd_output_types:=OutputDomain; lbd_outputs_unique:=OutputUnique; lbd_roles_domain:=RoleKeys;
  lbd_generations_unique:=GenUnique; lbd_generations_fresh:=GenFresh; lbd_body_module:=Module;
  lbd_body_fold:=Fold; lbd_body_context:=BodyContext;
  lbd_body_pins:=proj2 (check_link_pins_success _ _ _ _ _ PinReceipt) |}.
 - eapply link_capture_values_sound;eassumption.
 - intros bodies' (name'&names'&args'&res'&D).
   rewrite ProviderDecl in D;injection D as <- <- <- <- <-;exact Items.
 - exists s,sf;repeat first [assumption|split].
 - exists sc,sv;repeat first [assumption|split].
 - exists bodyFinal;repeat first [assumption|split].
Qed.
Section Prepared.
Variables (fg bg : gsym) (h : nat) (final : st) (D : link_body_data fg bg h final).
Let ef := lbd_ef _ _ _ _ D.
Let eg := lbd_eg _ _ _ _ D.
Let promised := lbd_promised _ _ _ _ D.
Let eb := lbd_body_env _ _ _ _ D.
Let fixed := lbd_fixed _ _ _ _ D.
Let outputs := lbd_outputs _ _ _ _ D.
Let keys := lbd_keys _ _ _ _ D.
Let roles := lbd_roles _ _ _ _ D.
Let xs := lbd_stmts _ _ _ _ D.
Let Cu := lbd_Cu _ _ _ _ D.
Let Cg := lbd_Cg _ _ _ _ D.
Let Cb := lbd_Cb _ _ _ _ D.
Let wrap := lbd_wrap _ _ _ _ D.
Let gen := generation_of roles.

(* These are only the result-stage facts: reading the promised result,
   presence of its keys, method aliases, and the emitted telescope. *)
Definition link_output_certificate : Prop := exists Cr ks,
 A.ProvideResult W (gs_id fg) Cu (fun t => TAbs (gen t)) Cr ks /\
 (forall k, In k ks -> A.provides Cb (A.at_site Cb (A.output_renaming outputs gen k))) /\
 (forall start renamed last,
   link_trace_renames promised eb roles (link_trace_made_types eb roles fixed) keys start = Good (renamed,last) ->
   Inv start -> Later w P R Fl last -> forall raw,
   link_trace_rebinds raw (List.concat renamed) =
    flat_map (A.refile (map (fun k => (A.at_site Cb (A.output_renaming outputs gen k),A.output_renaming outputs gen (A.rkey_of Cr k))) ks)) raw) /\
 (forall start dri last,
   mmap (fun k => let! it := key_item_at promised k in ret (output_role_item roles it)) keys start = Good (dri,last) ->
   Inv start -> Later w P R Fl last -> dri = map (fun k => A.output_renaming outputs gen (A.rkey_of Cr k)) ks).

Theorem prepared_link_body_sound : forall name names args res bodies,
 nget (w_syms w) (gs_id fg) = Some fg ->
 gs_decl bg = ADFunctor name names args res (Some bodies) ->
 nget (w_syms w) (gs_id bg) = Some bg ->
 Later w P R Fl final -> FinalLinks P final -> link_output_certificate ->
 A.LinkBody W P (gs_id fg) (gs_id bg) h.
Proof.
 intros name names args res bodies Target ProviderDecl Provider Last Links
  (Cr&ks&Provide&Available&Aliases&Header).
 destruct (lbd_after_body _ _ _ _ D) as (before&AfterBody&IBefore&LBefore).
 change (link_trace_after_body fg bg ef eg promised eb fixed outputs keys roles xs before = Good (h,final)) in AfterBody.
 destruct (link_body_fresh_checks fg bg ef eg promised eb fixed outputs keys roles xs before h final AfterBody)
  as (raw&sb&se&renamed&sn&st&si&ss&Binds&Effects&Renames&Types&Items&Readings&Finish).
 link_frame Binds IBefore IB FB.
 link_frame Effects IB IE FE.
 link_frame Renames IE IN FN.
 link_frame Types IN IT FT.
 link_frame Items IT II FI.
 link_frame Readings II IS FS.
 link_frame Finish IS IFinal FFinish.
 assert (WB : st_w before = w) by exact (lt_w _ _ _ _ _ LBefore).
 link_worlds;link_laters.
 pose proof (link_finish_fresh fg bg ef promised eb outputs keys roles
  (link_trace_rebinds raw (List.concat renamed)) ss h final Target Finish IS ltac:(eassumption)) as Fresh.
 destruct (link_finish_program_body fg bg ef promised eb outputs keys roles
  (link_trace_rebinds raw (List.concat renamed)) ss h final Finish Links)
  as (exported&allocated&dri&indexed&Exports&New&Index&ProgramBody).
 link_frame Exports IS IExported FExported.
 link_frame New IExported IAllocated FAllocated.
 link_frame Index IAllocated IIndexed FIndexed.
 destruct (link_body_fresh_emission fg bg ef promised eb outputs keys roles
  (link_trace_rebinds raw (List.concat renamed)) ss h final Finish)
  as (exported'&allocated'&dri'&indexed'&Exports'&New'&Index'&Final&Funs&Cache).
 rewrite Exports in Exports';injection Exports' as <-.
 rewrite New in New';injection New' as <-.
 rewrite Index in Index';injection Index' as <- <-.
 assert (LIndexed : Later w P R Fl indexed).
 { rewrite Final in Last;destruct Last;constructor;assumption. }
 specialize (Header allocated dri indexed Index IAllocated LIndexed);rewrite Header in ProgramBody.
 pose proof (Aliases se renamed sn Renames IE ltac:(eassumption) raw) as Refile.
 assert (Typed : A.BindItems W P (lbd_Fg _ _ _ _ D) Cg (List.concat bodies)
   wrap Cb (lbd_Fb _ _ _ _ D) (lbd_made _ _ _ _ D)).
 { apply (lbd_body_items _ _ _ _ D bodies);exists name,names,args,res;exact ProviderDecl. }
 pose proof (actual_link_binds W P _ _ _ _ _ _ _ xs before raw sb Typed
  (lbd_body_fold _ _ _ _ D) Binds) as RawBinds.
 pose proof (binditems_erase_pins W P _ _ _ _ _ _ _ Typed) as Erased.
 eapply A.LB with (dg:=A.Decl (gm_path (get_mod w (gs_mod bg))) (gs_decl bg) (gs_assumes bg))
  (Cu:=Cu) (nsu:=map (key_tr Fl) (lbd_fneeds _ _ _ _ D))
  (Cg:=Cg) (nsg:=map (key_tr Fl) (lbd_gneeds _ _ _ _ D))
  (Fg:=erase_pins (lbd_Fg _ _ _ _ D)) (wrap:=wrap) (Cb:=Cb)
  (Fb:=erase_pins (lbd_Fb _ _ _ _ D)) (made:=lbd_made _ _ _ _ D)
  (outs:=outputs) (gen:=gen) (Cr:=Cr) (ks:=ks) (binds0:=raw)
  (binds:=link_trace_rebinds raw (List.concat renamed))
  (n:=name) (ps:=names) (a:=args) (r:=res) (bodies:=bodies).
 - exact (w_decl_read w R Fl _ _ Provider).
 - exact ProviderDecl.
 - exact (lbd_target_context _ _ _ _ D).
 - exact (lbd_provider_context _ _ _ _ D).
 - exact (lbd_captures _ _ _ _ D).
 - exact (lbd_body_module _ _ _ _ D).
 - left;reflexivity.
 - exact Erased.
 - exact (lbd_output_types _ _ _ _ D).
 - exact (lbd_generations_unique _ _ _ _ D).
 - exact (lbd_generations_fresh _ _ _ _ D).
 - exact Provide.
 - exact Available.
 - exact RawBinds.
 - exact Refile.
 - exact Fresh.
 - exact ProgramBody.
Qed.
End Prepared.
(* Fresh execution has no remaining input, capture, body, or allocation
   obligation. Its only remaining condition is the result-stage certificate. *)
Theorem link_body_fresh_conditional : forall f, OneStep w P R Fl f ->
 forall fg bg fname fps fargs fres gname gps gargs gres bodies s h final,
 A.WorldOf b W ->
 nget (w_syms w) (gs_id fg) = Some fg -> gs_decl fg = ADFunctor fname fps fargs fres None ->
 nget (w_syms w) (gs_id bg) = Some bg -> gs_decl bg = ADFunctor gname gps gargs gres (Some bodies) ->
 assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b))
  (gs_id fg,gs_id bg) (st_links s) = None ->
 link_body (S (S f)) fg bg s = Good (h,final) ->
 Inv s -> InfoInv s -> FwdInv s -> st_w s = w -> Later w P R Fl final -> FinalLinks P final ->
 exists D : link_body_data fg bg h final,
 link_output_certificate fg bg h final D -> A.LinkBody W P (gs_id fg) (gs_id bg) h.
Proof.
 intros f Step fg bg fname fps fargs fres gname gps gargs gres bodies s h final
  World Target TargetDecl Provider ProviderDecl Miss Run Valid Cache Forward Hw Last Links.
 destruct (link_body_fresh_data f Step fg bg fname fps fargs fres gname gps gargs gres bodies
  s h final World Target TargetDecl Provider ProviderDecl Miss Run Valid Cache Forward Hw Last) as [D].
 exists D;intro Output.
 eapply (prepared_link_body_sound fg bg h final D gname gps gargs gres bodies);eassumption.
Qed.
End Sound.
