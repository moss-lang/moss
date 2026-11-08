(* The actual method-bind continuation installs the checked application and
   emits exactly the receiver and substitution read by BMethBind. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr BodyTy EtyTyExec CanonTypes NominalSupport NominalMergeSupport Merge CtxWalk MethodKeyTr
 ProvideResultReading ProvideResultFunction ProvideResultMethod ProvideResultCanonical ProvideResultSubst ProvideResultContext
 FreeTypesExec TySupport SourceSupport NeedMapExec BindSequence BindMethodTrace.
Import ListNotations.
Local Open Scope list_scope.
Module A := ExecutionModel.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Section Suffix.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Hypothesis Finj : forall a b, Fl a = Fl b -> a = b.
Notation W := (BodySoundCore.W w R Fl).
Theorem method_bind_suffix_sound : forall c e tm this m sg ts target pre sm s xs e' c' final
 F C pa dn brackets receiver bs y rcv name params result body,
 method_bind_suffix c e tm this m sg ts target pre sm s = Good ((xs,e',c'),final) ->
 Inv s -> st_w s = w -> Later w P R Fl final -> CxRel w R Fl c e F C ->
 nget (w_syms w) (gs_id m) = Some m -> gs_decl m = ADFn rcv name params result body ->
 A.kind_sym W (gs_id m) = Some A.DKMeth ->
 TmRel Fl tm (map (trp Fl) tm) e ->
 (forall k, A.nlook k (bs ++ A.c_ty C) = option_map (fun t => tr Fl (canon e t)) (nassoc k tm)) ->
 WFty this -> Support W this -> SigOK sg -> SigSupport W sg ->
 A.resolve_sym W (A.fr_mod F) pa = Some receiver ->
 A.MethodNamed W (A.fr_mod F) receiver dn (gs_id m) ->
 A.Brackets W (A.fr_mod F) (A.c_ty C) (gs_id m) brackets bs ->
 A.TyOf W (A.fr_mod F) (bs ++ A.c_ty C) None (ATRef pa None) (tr Fl (canon e this)) ->
 A.SigOf W (gs_id m) (bs ++ A.c_ty C) (Some (tr Fl (canon e this))) (output_sig Fl e sg) ->
 A.MethProvider W P F C y (output_sig Fl e sg) (tr Fl (canon e this)) target (fold_stmts pre) sm ->
 BindResult w P R Fl c e F C (Spec (Some pa) (Some dn) brackets,Some y) xs e' c'.
Proof.
 intros c e tm this m sg ts target pre sm s xs e' c' final F C pa dn brackets receiver bs y rcv name params result body
 Run I Hw Last HC Loaded Decl Kind TR Meaning Wthis Sthis Wsig Ssig Target Named Brackets ReceiverType Signature Provider.
 unfold method_bind_suffix in Run.
 apply bind_inv in Run as [k [sk [Key Run]]].
 destruct (@method_key_value e (cx_mod c) this m s k sk Key) as [-> ->].
 apply bind_inv in Run as [fm [selected [Select Run]]].
 assert (Oselect : ok (family_member (cx_mod c) e (canon e this) (gs_id m))) by (unfold family_member; okt).
 destruct (Oselect _ _ _ I Select) as [Is Fs].
 apply bind_inv in Run as [u [guarded [Guard Run]]].
 destruct fm as [[fg args]|]; [exfalso; exact (err_not_good _ _ _ _ _ _ Guard)|].
 injection Guard as <- <-.
 apply bind_inv in Run as [out [provided [Provision Run]]].
 destruct (output_provided_ok (set_mth e (canon e this) (gs_id m) sg)
   (KyMth (canon e this) (gs_id m)) m tm true _ _ _ Is Provision) as [Ip Fp].
 apply bind_inv in Run as [code [typed [TypeCode Run]]].
 destruct (ok_ety_ty _ _ _ _ Ip TypeCode) as [It Ft].
 apply bind_inv in Run as [theta [keyed [Theta Run]]].
 destruct (@ok_key_theta_frame _ _ _ _ _ It Theta) as [Ik Fk].
 apply bind_inv in Run as [u [recorded [Event Run]]].
 injection Run as <- <- <- <-.
 match type of Event with ?m _ = _ => assert (Oevent : ok m) by (unfold record_reify_event; apply ok_reify_update) end.
 destruct (Oevent _ _ _ Ik Event) as [_ Fe].
 pose proof (later_back w P R Fl _ _ Fe Last) as Lkey.
 pose proof (later_back w P R Fl _ _ Fk Lkey) as Ltype.
 pose proof (later_back w P R Fl _ _ Ft Ltype) as Lprov.
 rewrite (ety_ty_tr w P R Fl _ _ _ _ TypeCode Ip Ltype).
 unfold provided_at in Provision.
 apply bind_inv in Provision as [app [applied [Application Provision]]].
 apply bind_inv in Provision as [stamp [fresh [Fresh Provision]]].
 injection Provision as <- <-.
 destruct (output_provision_ok m tm true _ _ _ Is Application) as [Ia Fa].
 destruct (ok_fresh _ _ _ Ia Fresh) as [_ Ffresh].
 pose proof (later_back w P R Fl _ _ Ffresh Lprov) as Lapp.
 assert (Ws : st_w selected = w) by (rewrite (fr_w _ _ Fs); exact Hw).
 pose proof (method_provision_app_rel w (P:=P) R Fl b Hload m rcv name params result body tm (map (trp Fl) tm) e
   selected app applied Loaded Decl Kind Application Is Ws Lapp TR) as RawApp.
 change (A.AppOf W (gs_id m) (map (trp Fl) tm) (output_inst Fl app)) in RawApp.
 pose proof (proj1 (cr_uf _ _ _ _ _ _ _ HC)) as TriU.
 pose proof (output_appof_subst W (suf Fl (en_uf e)) _ _ _ RawApp (bs ++ A.c_ty C)
   (output_subrel Fl e tm (bs ++ A.c_ty C) TriU Meaning)) as App.
 rewrite output_freeze_translation in App by exact TriU.
 assert (Fixed : A.canon C (tr Fl (canon e this)) = tr Fl (canon e this)).
 { unfold A.canon; rewrite (cr_sub _ _ _ _ _ _ _ HC).
   rewrite (tr_canon Fl e (canon e this) TriU).
   rewrite canon_idem by exact TriU; reflexivity. }
 destruct (caller_types_ok w R Fl b Hload c e F C HC) as [Uok Tn].
 assert (Othis : ety_ok this = true) by exact (support_clean (loaded_record_unowned w R Fl b Hload) Sthis Wthis).
 assert (Orecv : ety_ok (canon e this) = true) by (apply canon_ok; [exact Uok|exact Othis]).
 assert (Srecv : Support W (canon e this)) by (apply canon_support; [exact (@es_uf W e (cr_support _ _ _ _ _ _ _ HC))|exact Sthis]).
 set (C' := A.provide C (A.mkey (tr Fl (canon e this)) (gs_id m)) (output_inst Fl (freeze_app e app))
   (A.set_mth C (tr Fl (canon e this)) (gs_id m) (output_sig Fl e sg))).
 assert (Context : CxRel w R Fl c
   (set_app (set_mth e (canon e this) (gs_id m) sg) (KyMth (canon e this) (gs_id m))
     (freeze_app e app) stamp) F C').
 { apply (cxrel_method w R Fl Finj); [exact HC|apply canon_idem;exact TriU|exact Orecv|exact Wsig|exact Ssig|exact Srecv|reflexivity]. }
 assert (ThetaEq : theta = A.theta_of C (output_inst Fl (freeze_app e app))).
 { rewrite (@key_theta_tr w P R Fl _ _ _ _ _ Theta It Lkey).
   unfold get_app, set_app; cbn [en_apps].
   rewrite support_assoc_aset by apply CtxWalk.key_eqb_spec.
   cbn [key_eqb]. rewrite Nat.eqb_refl, ety_eqb_refl; cbn [fst snd].
   apply (@read_known_ctx Fl _ C _); [exact (cr_sub _ _ _ _ _ _ _ HC)|exact TriU]. }
 rewrite ThetaEq.
 exists F,C',[],(fun tail => fold_stmts pre (XBindMth (tr Fl (canon e this)) (gs_id m)
   (A.theta_of C (output_inst Fl (freeze_app e app))) target sm tail)).
 split; [exact Context|]. split; [reflexivity|]. split; [now rewrite app_nil_r|]. split.
 - unfold C'; rewrite <- Fixed.
   eapply A.BMethBind; [exact Target|exact Named|exact Brackets|exact ReceiverType|exact Signature|exact Provider|exact App].
 - intro tail; rewrite fold_stmts_app; reflexivity.
Qed.
End Suffix.
