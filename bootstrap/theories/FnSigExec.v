(* Visible type readings suffice for function signatures after body binds. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy BodyTr
 SourceClean SourceSupport CanonBodyTy CanonTypes NominalSupport NeedMapExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Source.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Lemma fn_sig_clean_at : forall g tm e this s sg s',
  fn_sig g tm e this s = Good (sg,s') -> Inv s -> st_w s = w ->
  nget (w_syms w) (gs_id g) = Some g -> TmClean tm -> TmClean(tymap e) ->
  (match this with Some y => ety_ok y = true | None => True end) ->
  Later w P R Fl s' -> SigOK sg.
Proof.
  intros g tm e this s sg s' H Hi Hw Hg T E Wth L.
  unfold fn_sig in H. apply bind_inv in H as (ps & s1 & H1 & H).
  apply bind_inv in H as (r & s2 & H2 & H). injection H as <- <-.
  assert (Of : ok (match fn_ret g with Some t => ety_of t (gs_mod g) tm e this | None => ret YUnit end)).
  { destruct (fn_ret g); okt. }
  match type of H1 with ?m s = _ => assert (O1 : ok m) by (apply ok_mmap; intro p; apply ok_bind; [apply ok_ety_of|intro; apply ok_ret]) end.
  destruct (O1 _ _ _ Hi H1) as [Hi1 F1].
  destruct (Of _ _ _ Hi1 H2) as [Hi2 F2].
  assert (Q : Forall2 (fun p q => ety_ok (snd q) = true) (fn_params g) ps).
  { refine (mmap_ok_rel w R Fl _ _ _ _ _ s ps s1 _ _ H1 Hi Hw (later_back w P R Fl _ _ F2 L)).
    - intro p. apply ok_bind; [apply ok_ety_of|intro; apply ok_ret].
    - intros p _ s0 q s3 H3 Hi0 Hw0 L3. apply bind_inv in H3 as (y & s4 & H4 & H3). injection H3 as <- <-.
      exact (ety_of_clean_at w R Fl b Hload _ _ _ _ _ _ _ _ H4 Hi0 Hw0 (loader_mod_in w R Fl b Hload _ _ Hg) T E Wth L3). }
  unfold SigOK. cbn [sg_params sg_ret sg_this]. split.
  - apply Forall_forall. intros y Hy. apply in_map_iff in Hy as (q & <- & Hq).
    clear -Q Hq. induction Q; [contradiction|]. destruct Hq as [<-|Hq]; auto.
  - split; [|exact Wth]. destruct (fn_ret g).
    + exact (ety_of_clean_at w R Fl b Hload _ _ _ _ _ _ _ _ H2 Hi1 (fr_w' w _ _ F1 Hw) (loader_mod_in w R Fl b Hload _ _ Hg) T E Wth L).
    + injection H2 as <- _. reflexivity.
Qed.
Lemma fn_sig_support_at : forall g tm e this s sg s',
 fn_sig g tm e this s = Good (sg,s') -> Inv s -> st_w s = w -> In (gs_mod g) (w_order w) ->
 (forall k y,nassoc k tm=Some y->Support WW y) -> (forall k y,nassoc k tm=Some y->WFty y) ->
 EnvSupport WW e -> (forall k y, tn_get e k = Some y -> WFty y) ->
 (match this with None => True | Some y => Support WW y /\ WFty y end) ->
 Later w P R Fl s' -> SigSupport WW sg.
Proof.
 intros g tm e this s sg s' H Hi Hw Hmi St Wt Se We Sthis L.
 unfold fn_sig in H. apply bind_inv in H as (ps & s1 & Hp & H).
 apply bind_inv in H as (rt & s2 & Hr & H). injection H as <- <-.
 assert (Op : forall (p : name * aty), ok (let! y := ety_of (snd p) (gs_mod g) tm e this in ret (fst p,y))).
 { intro p. apply ok_bind; [apply ok_ety_of|intro; apply ok_ret]. }
 destruct (ok_mmap _ _ _ _ Op _ _ _ Hi Hp) as [Hi1 Fp].
 assert (Or : ok (match fn_ret g with Some t => ety_of t (gs_mod g) tm e this | None => ret YUnit end)).
 { destruct (fn_ret g); [apply ok_ety_of|apply ok_ret]. }
 destruct (Or _ _ _ Hi1 Hr) as [_ Fr].
 assert (Sp : Forall2 (fun p q => Support WW (snd q)) (fn_params g) ps).
 { eapply (mmap_ok_rel w R Fl); [exact Op| |exact Hp|exact Hi|exact Hw|exact (later_back w P R Fl _ _ Fr L)].
   intros p _ si q sf E Ii Wi Li. apply bind_inv in E as (y & sy & Ey & E). injection E as <- <-.
   eapply (ety_of_support_at w R Fl b Hload); eassumption. }
 unfold SigSupport. cbn [sg_params sg_ret sg_this]. split.
 - clear -Sp. induction Sp; constructor; auto.
 - split.
   + destruct (fn_ret g) as [t|]; [|injection Hr as <- _; constructor].
     eapply (ety_of_support_at w R Fl b Hload); try eassumption.
     exact (fr_w' w _ _ Fp Hw).
   + destruct this; [exact (proj1 Sthis)|exact I].
Qed.
Lemma fn_sig_body_properties : forall c e F C g s sg s',
 fn_sig g(tymap e)e None s=Good(sg,s')->Inv s->st_w s=w->
 nget(w_syms w)(gs_id g)=Some g->CxRel w R Fl c e F C->Later w P R Fl s'->
 SigOK sg/\SigSupport WW sg.
Proof.
 intros c e F C g s sg s' E Hi Hw Hg Hc L.
 destruct(caller_types_ok w R Fl b Hload c e F C Hc)as[U T].
 assert(Tc:TmClean(tymap e)).
 { intros k y Ey. rewrite nassoc_tymap in Ey;exact(T _ _ Ey). }
 split.
 - exact(fn_sig_clean_at _ _ _ _ _ _ _ E Hi Hw Hg Tc Tc I L).
 - eapply fn_sig_support_at;[exact E|exact Hi|exact Hw|exact(loader_mod_in w R Fl b Hload _ _ Hg)| | |exact(cr_support _ _ _ _ _ _ _ Hc)|exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|exact I|exact L].
   + intros k y Ey. rewrite nassoc_tymap in Ey. exact(@tn_get_support WW e k y(cr_support _ _ _ _ _ _ _ Hc)Ey).
   + intros k y Ey. rewrite nassoc_tymap in Ey. exact(cr_ty_wf _ _ _ _ _ _ _ Hc _ _ Ey).
Qed.
End Source.
