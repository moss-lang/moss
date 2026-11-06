(* Actual lambda generation discharges every LP premise: lexical captures,
   fresh slots, checked annotations, exact body and finite provision keys. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound
 InfoPreserve CanonTypes NominalSupport MethodRows CtxMerge
 LambdaAnnotations LambdaPrefixExec LambdaTrace LambdaInputTrace LambdaEntryInv LambdaStageFrames
 LambdaCaptureNames LambdaCaptureContext LambdaCaptureFold LambdaCaptureTables LambdaCapturedLocals LambdaCapturedContext
 LambdaOwnContext LambdaBodyContext LambdaGeneratedExec LambdaGeneratedMono LambdaEmittedBody LambdaDeclarations
 LambdaLocalInterfaces LambdaKeysProvides LambdaKeyTranslation LambdaSmapIdentity ApplyFreshExec ProvideResultContext.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Provider.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySound.W w R Fl).
Theorem lambda_provider_sound:forall fuel c original ps rt body bound sg this what s pid pre sm final,
 OK w P R Fl fuel->lambda_provider fuel c original ps rt body bound sg this what s=Good((pid,pre,sm),final)->
 forall F Co,Inv s->InfoInv s->FwdInv s->st_w s=w->nget(w_syms w)(gs_id bound)=Some bound->
 CxRel w R Fl c original F Co->SigOK sg->SigSupport W sg->opt_wf this->opt_support W this->
 Post w P R Fl final F->
 A.LambdaProvider W P F Co ps rt body(output_sig Fl original sg)
 (option_map(fun t=>tr Fl(canon original t))this)pid(fold_stmts pre)sm.
Proof.
 intros[|f]c original ps rt body bound sg this what s pid pre sm final IH Run F Co I Cache Fwd Hw Loaded HC Clean Supported Wthis Sthis Hp;[discriminate Run|].
 destruct(lambda_provider_stages_inputs(S f)c original ps rt body bound sg this what s pid pre sm final Run I)
 as(f0&le&lets&captureStart&beforePid&afterPid&registered&compiled&bodyFinal&emitted&cached&Fuel&Capture&Prefix&Icap&Fprefix&Symbols&Functions&New&Register&Body&Emit&Install&Transfer&Names&Ip&Ir).
 injection Fuel as Ef;subst f0.
 destruct Prefix as[sp[Params Return]].
 destruct(lambda_entry_invariants c original ps rt body bound sg this what s sp captureStart le pre lets beforePid pid afterPid registered
 Params Return Capture New Register I Cache Fwd)as(_&Ccap&Fcap&_&Cp&Fp&_&Cr&Fr).
 destruct(lambda_stage_frames w P R Fl f c original F ps body bound sg this le lets beforePid pid afterPid registered compiled bodyFinal emitted cached sm final
 New Register Body Emit Install Transfer Ip Hp)as[Lbody[Lemit[PostCapture[Icached Fnewbody]]]].
 assert(Wcap:st_w captureStart=w)by(rewrite(fr_w _ _ Fprefix);exact Hw).
 assert(Wp:st_w beforePid=w)by exact(lt_w _ _ _ _ _(proj1 PostCapture)).
 destruct(lambda_captured_context w(P:=P)R Fl b Hload c original F Co ps body captureStart le pre lets beforePid bound
 Capture Icap Wcap Loaded HC PostCapture)
 as(Cl&Captured&NdNames&Fresh&NdIds&CallerNames&Pre&ClEq&HCcaptured&Uf&Bounds).
 destruct(lambda_capture_value_context w(P:=P)R Fl b Hload c original F Co ps body captureStart le pre lets beforePid bound
 Capture Icap Wcap Loaded HC PostCapture)as[HCvalue[Available Uf0]].
 set(Cvalue:=fold_left(capture_model Fl original)lets(lambda_clear_context Co)).
 change(CxRel w R Fl c le F Cvalue)in HCvalue.
 assert(ClLocal:Cl=fold_left(capture_local Fl original)lets Cvalue).
 {rewrite ClEq,(capture_full_reading w R Fl _ _ _ _ _ _ HC Available),capture_tables_factor;reflexivity. }
 pose proof(lambda_generated_mono f c(lambda_own_env ps sg this le lets)ps body sg this pid lets _ _ Body)as Raised.
 destruct(lambda_generated_sound w P R Fl(S f)c original F Co le Cvalue ps body sg this pid lets registered compiled bodyFinal
 IH Raised Ir Cr Fr Lbody HC HCvalue Uf Clean Supported Wthis Sthis Available Names)
 as(vars&lbody&ltype&Numbered&BodyNames&Block&Fits&Code).
 set(Fbody:=lambda_body_frame Fl original F sg this vars).
 change(A.Numbering Fbody(option_map(fun t=>tr Fl(canon original t))this)(map fst ps))in Numbered.
 change(Forall(A.Named Fbody)(map capture_name lets))in BodyNames.
 rewrite <-ClLocal in Block.
 assert(Program:A.body_of P pid=Some compiled).
 {eapply(lambda_emitted_body w P R Fl);[exact New|exact Fnewbody|exact Emit|exact Lemit]. }
 pose proof(new_sym_fresh b w R Fl Hload beforePid bound(cx_mod c)(gs_name bound)KFn
  (ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda pid afterPid Ip Wp Loaded New)as FreshPid.
 destruct(lambda_annotation_prefix w(P:=P)R Fl b Hload(S f)c original F Co ps rt body bound sg this what s pid pre sm final
 Run I HC Wthis(proj1 Hp))as[Length[Distinct[Parameters ReturnAnnotation]]].
 set(capenv:=fold_left(fun en q=>let '(n,_,t) :=q in set_local en n t LCaptured)lets le).
 change(CxRel w R Fl c capenv F Cl)in HCcaptured.
 assert(Urows:MethodRowsUnique capenv)by(intros m rows Lookup;exact(proj1(cr_mth_rows _ _ _ _ _ _ _ HCcaptured m rows Lookup))).
 assert(Crows:MethodRowsCanonical capenv)by(intros m rows Lookup;exact(proj2(cr_mth_rows _ _ _ _ _ _ _ HCcaptured m rows Lookup))).
 assert(Domain:LambdaMethodDomain Fl capenv Cl)by exact(cr_mth_domain _ _ _ _ _ _ _ HCcaptured).
 pose proof(all_keys_provides w R Fl c capenv F Cl HCcaptured Urows Crows Domain)as Keys.
 pose proof(all_keys_translation_unique Fl Finj capenv Urows Crows)as NdKeys.
 pose proof(all_keys_valid capenv Crows)as Valid.
 unfold capenv in Keys,NdKeys,Valid;rewrite lambda_captured_keys in Keys,NdKeys,Valid.
 pose proof(lambda_body_context w R Fl c original F Co le Cvalue ps sg this lets vars HC HCvalue Uf Clean Supported Wthis Sthis Available Names)as HCbody.
 rewrite <-ClLocal in HCbody.
 pose proof(lambda_smap_identity w P R Fl(all_keys le)(lambda_own_context c ps sg this)(lambda_own_env ps sg this le lets)Fbody
  (lambda_parameter_model Fl original ps sg Cl)c original F Co cached sm final Transfer Icached(proj1 Hp)HCbody HC Valid)as Sm.
 assert(Satisfaction:sm=map(fun k=>(A.rkey_of Cl k,A.rkey_of Co k))(map(key_tr Fl)(all_keys le))).
 {rewrite Sm;apply map_ext;intro key;rewrite lambda_parameters_key;reflexivity. }
 assert(PreCode:fold_stmts pre=(fun x=>fold_right(fun q x=>XBindVal(snd q)(XVar(A.var_of F(fst q)))x)x
   (combine(map capture_name lets)(map capture_id lets)))).
 {rewrite Pre;apply lambda_capture_pre_code. }
 rewrite PreCode.
 eapply A.LP with(caps:=map capture_name lets)(capvals:=map capture_id lets)(Cl:=Cl)(Fl:=Fbody)
 (lbody:=lbody)(ltype:=ltype)(keys:=map(key_tr Fl)(all_keys le)).
 - cbn[output_sig A.sg_params];rewrite map_length;exact Length.
 - exact Distinct.
 - exact Parameters.
 - exact ReturnAnnotation.
 - exact Captured.
 - exact NdNames.
 - now rewrite !map_length.
 - exact Fresh.
 - exact NdIds.
 - exact FreshPid.
 - exact ClEq.
 - reflexivity.
 - reflexivity.
 - reflexivity.
 - reflexivity.
 - exact Numbered.
 - exact BodyNames.
 - exact CallerNames.
 - exact Block.
 - exact Fits.
 - rewrite Code in Program;exact Program.
 - exact Keys.
 - exact NdKeys.
 - exact Satisfaction.
Qed.
End Provider.
