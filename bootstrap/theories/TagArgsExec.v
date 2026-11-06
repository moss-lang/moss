(* Actual nominal-argument enumeration, independent of source bracket parsing. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
  BodySound BodyTy ReqsSolve.
Import ListNotations.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Section Args.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat).
Theorem nominal_args_complete : forall mi g tm s xs s',
  nominal_args mi g tm s = Good (xs,s') -> Inv s -> Later w P R Fl s' ->
  Forall2 (fun r q => fst q = r /\ nassoc r tm = Some(snd q))
    (R (gs_id g)) xs.
Proof.
  intros mi g tm s xs s' E Hi L.
  unfold nominal_args in E. apply bind_inv in E as (rs & s1 & Er & E).
  match type of E with mmap ?fn _ _ = _ =>
    assert (MR : forall r, In r rs -> forall q s2, fn r s1 = Good(q,s2) ->
      s2=s1 /\ (fst q=r /\ nassoc r tm=Some(snd q)))
    by (intros r _ q s2 H; cbv beta in H;
      destruct (nassoc r tm) as [y|] eqn:Ey;
      [unfold ret in H; injection H as <- <-; auto|apply bind_inv in H as (g0 & s0 & Eg & H); exfalso; exact (err_not_good _ _ _ _ _ _ H)]) end.
  destruct (mmap_reader _ _ _ (fun r q => fst q=r /\ nassoc r tm=Some(snd q))
    rs s1 xs s' MR E) as [Es PP]. subst s'.
  rewrite (@reqs_R w P R Fl _ _ _ _ Hi Er L). exact PP.
Qed.

Theorem nominal_args_lookup : forall mi g tm s xs s' tmr (read : ety -> ty),
  nominal_args mi g tm s = Good (xs,s') -> Inv s -> Later w P R Fl s' ->
  (forall k y, nassoc k tm = Some y -> WFty y /\ A.nlook k tmr = Some (read y)) ->
  Forall2 (fun r q => fst q = r /\ A.nlook r tmr = Some (snd q))
    (R (gs_id g)) (map (fun p => (fst p,read(snd p))) xs) /\
  Forall (fun p => WFty(snd p)) xs.
Proof.
  intros mi g tm s xs s' tmr read E Hi L T.
  unfold nominal_args in E. apply bind_inv in E as (rs & s1 & Er & E).
  match type of E with mmap ?fn _ _ = _ =>
    assert (MR : forall r, In r rs -> forall q s2, fn r s1 = Good(q,s2) ->
      s2=s1 /\ (fst q=r /\ nassoc r tm=Some(snd q)))
    by (intros r _ q s2 H; cbv beta in H;
      destruct (nassoc r tm) as [y|] eqn:Ey;
      [unfold ret in H; injection H as <- <-; auto|apply bind_inv in H as (g0 & s0 & Eg & H); exfalso; exact (err_not_good _ _ _ _ _ _ H)]) end.
  destruct (mmap_reader _ _ _ (fun r q => fst q=r /\ nassoc r tm=Some(snd q))
    rs s1 xs s' MR E) as [Es PP]. subst s'.
  rewrite (@reqs_R w P R Fl _ _ _ _ Hi Er L).
  clear -PP T. induction PP as [|r q rs xs [Ek Eq] PP IH].
  - split; constructor.
  - destruct IH as [Ir Iw]. cbn. split.
    + constructor; [split; [exact Ek|exact (proj2(T _ _ Eq))]|exact Ir].
    + constructor; [exact (proj1(T _ _ Eq))|exact Iw].
Qed.
End Args.

From NM Require Import TagArgsCore CanonTypes TyOfSubst Merge.
Definition trxC (Fl:string->nat)(e:env)(xs:list(nat*ety)) :=
 map(fun p=>(fst p,tr Fl(canon e(snd p))))xs.
Lemma smap_trx : forall Fl e xs,Tri(en_uf e)->
 smap(suf Fl(en_uf e))(map(trp Fl)xs)=trxC Fl e xs.
Proof. intros. unfold smap,trxC. rewrite map_map. apply map_ext. intros[k y];cbn[fst snd trp]. rewrite tr_canon by assumption. reflexivity. Qed.
Section BracketArgs.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySound.W w R Fl).
Notation path_of mi := (gm_path (get_mod w mi)).

Theorem tag_args_rel_at : forall c e g a s xs s' tmr,
  tag_args c e g a s = Good(xs,s') -> Inv s -> st_w s = w ->
  In (cx_mod c) (w_order w) -> TmRel Fl (tymap e) tmr e ->
  Later w P R Fl s' ->
  exists bs, A.Brackets WW (path_of (cx_mod c)) tmr (gs_id g) a bs /\
    Forall2 (fun r q => fst q=r /\ A.nlook r (bs++tmr)=Some(snd q))
      (R(gs_id g)) (map (trp Fl) xs) /\ Forall(fun p=>WFty(snd p)) xs.
Proof.
  intros c e g a s xs s' tmr E Hi Hw Hmi T L.
  unfold tag_args in E. apply bind_inv in E as (tm & s0 & Et & En).
  destruct a as [bs|].
  - apply bind_inv in Et as (u & s1 & Ec & Et).
    apply bind_inv in Et as (sb & s2 & Es & Et). injection Et as <- <-.
    assert (Oc : ok(check_relevant (cx_mod c) g bs)).
    { unfold check_relevant. apply ok_bind; [apply ok_reqs|intro; okt]. }
    destruct (Oc _ _ _ Hi Ec) as [Hi1 F1].
    destruct (ok_app_subst _ _ _ _ _ _ _ _ Hi1 Es) as [Hi2 F2].
    assert (On : ok(nominal_args (cx_mod c) g (sb++tymap e))) by (unfold nominal_args; okt).
    destruct (On _ _ _ Hi2 En) as [_ Fn].
    pose proof (later_from w R Fl _ _ Fn L) as L2.
    pose proof (later_from w R Fl _ _ F2 L2) as L1.
    pose proof (fr_w' w _ _ F1 Hw) as Hw1.
    destruct (proj1(proj2(ty_all w R Fl b Hload 200)) _ _ _ _ _ _ _ tmr
      Es Hi1 Hw1 Hmi (tmrel_outer Fl _ _ _ T) L2) as (lr & Fa & El & Wl).
    assert (Br : A.Brackets WW (path_of(cx_mod c)) tmr (gs_id g) (Some bs) (rev lr)).
    { apply A.BrSome. assert (Gen : forall bs', incl bs' bs -> forall lr',
        Forall2 (BrRel w R Fl (path_of(cx_mod c)) tmr) bs' lr' ->
        Forall2 (fun b0 tb => exists sp sa, snd b0=Spec(Some sp)None sa /\
          A.resolve_sym WW (path_of(cx_mod c))(fst b0)=Some(fst tb) /\
          A.kind_sym WW (fst tb)=Some A.DKType /\
          (A.kind_sym WW (gs_id g)<>Some A.DKMeth -> In(fst tb)(A.w_reqs WW (gs_id g))) /\
          A.TyOf WW (path_of(cx_mod c)) tmr None (ATRef sp sa)(snd tb)) bs' lr').
      { intros bs' Hin lr' F. induction F as [|b0 tb r r' Hb Hr IH]; constructor.
        - destruct Hb as (sp & sa & E1 & E2 & E3 & E4).
          exists sp,sa. repeat split; try assumption. intros _.
          exact (check_relevant_spec w R Fl b Hload (cx_mod c) g bs s u s1 Hi Hw Hmi Ec L1
            b0 _ (Hin b0 (or_introl eq_refl)) E2 E3).
        - apply IH. intros z Hz. apply Hin. right; exact Hz. }
      exact (Gen bs (incl_refl _) lr Fa). }
    exists (rev lr). split; [exact Br|].
    eapply nominal_args_lookup; [exact En|exact Hi2|exact L|].
    intros k y Hy. pose proof (tmrel_app Fl _ _ _ _ _ T El Wl) as T'.
    split; [exact (proj1(proj2 T') _ _ Hy)|]. rewrite (proj1 T'),Hy. reflexivity.
  - injection Et as <- <-. exists []. split; [constructor|].
    eapply nominal_args_lookup; [exact En|exact Hi|exact L|].
    intros k y Hy. split; [exact (proj1(proj2 T) _ _ Hy)|].
    cbn. rewrite (proj1 T),Hy. reflexivity.
Qed.
End BracketArgs.

From NM Require Import CanonBodyTy CanonTypes TyOfSubst Merge.
Section CanonicalArgs.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.

Theorem tag_args_canonical : forall c e F C g a s xs s',
  tag_args c e g a s = Good(xs,s') -> Inv s -> st_w s=w ->
  CxRel w R Fl c e F C -> Later w P R Fl s' ->
  exists bs, A.Brackets (BodySound.W w R Fl) (A.fr_mod F) (A.c_ty C) (gs_id g) a bs /\
    Forall2 (fun r q=>fst q=r /\ A.nlook r (bs++A.c_ty C)=Some(snd q))
      (R(gs_id g)) (trxC Fl e xs) /\ Forall(fun p=>WFty(snd p)) xs.
Proof.
  intros c e F C g a s xs s' E Hi Hw Hc L.
  set (tm0:=map(trp Fl)(tymap e)).
  assert (T : TmRel Fl (tymap e) tm0 e).
  { apply tmrel_tymap; [exact (cr_ty_wf _ _ _ _ _ _ _ Hc)|].
    intro k. unfold tm0. rewrite nlook_trp,nassoc_tymap. reflexivity. }
  destruct (tag_args_rel_at w R Fl b Hload _ _ _ _ _ _ _ _ E Hi Hw (cr_mod_in _ _ _ _ _ _ _ Hc) T L)
    as (bs & Br & Q & Wx).
  set (sb:=suf Fl (en_uf e)).
  assert (SR : SubRel sb tm0 (A.c_ty C)).
  { intro k. rewrite (cr_ty _ _ _ _ _ _ _ Hc). unfold tm0. rewrite nlook_trp,nassoc_tymap.
    destruct (tn_get e k) as [y|]; cbn [option_map]; [|reflexivity].
    unfold sb. rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)). reflexivity. }
  exists (smap sb bs). split.
  - rewrite (cr_mod _ _ _ _ _ _ _ Hc). exact (br_subst _ sb _ _ _ _ _ Br _ SR).
  - split; [|exact Wx].
    rewrite <- (smap_trx Fl e xs (proj1(cr_uf _ _ _ _ _ _ _ Hc))).
    change (Forall2 (fun r q=>fst q=r /\ A.nlook r (smap sb bs++A.c_ty C)=Some(snd q)) (R(gs_id g)) (smap sb (map(trp Fl) xs))).
    pose proof (subrel_app sb bs tm0 (A.c_ty C) SR) as SRa.
    clear -Q SRa. induction Q as [|r q rs qs [Ek Eq] Q IH]; cbn [smap map]; constructor; [|exact IH].
    split; [exact Ek|]. cbn [snd]. rewrite SRa,Eq. reflexivity.
Qed.
End CanonicalArgs.

Section ArgumentDomain.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Theorem tag_args_domain : forall c e F C g a s xs s',
 tag_args c e g a s=Good(xs,s') -> Inv s -> st_w s=w ->
 CxRel w R Fl c e F C -> Later w P R Fl s' ->
 map fst xs=R(gs_id g) /\ Forall(fun p=>WFty(snd p))xs.
Proof.
 intros c e F C g a s xs s' E Hi Hw Hc L.
 destruct(tag_args_canonical w R Fl b Hload _ _ _ _ _ _ _ _ _ E Hi Hw Hc L) as (bs & Br & Q & Wx).
 split; [|exact Wx].
 assert (Keys : map fst (trxC Fl e xs)=R(gs_id g)).
 { clear -Q. induction Q as [|r q rs qs [Ek Eq] Q IH]; cbn; [reflexivity|]. f_equal; assumption. }
 unfold trxC in Keys. rewrite map_map in Keys. change (map fst xs=R(gs_id g)) in Keys. exact Keys.
Qed.
End ArgumentDomain.

From NM Require Import SourceSupport NominalSupport.
Section SupportedArgs.
Variable w:Elab.world.
Context {P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).

Lemma nominal_args_support : forall mi g tm s xs s',
 nominal_args mi g tm s=Good(xs,s') -> Inv s -> Later w P R Fl s' ->
 (forall k y,nassoc k tm=Some y -> Support WW y) ->
 Forall(fun p=>Support WW(snd p))xs.
Proof.
 intros mi g tm s xs s' E Hi L T.
 pose proof(nominal_args_complete w R Fl _ _ _ _ _ _ E Hi L)as Q.
 clear -Q T. induction Q as[|r q rs qs [Ek Eq]Q IH];constructor;[exact(T _ _ Eq)|exact IH].
Qed.

Theorem tag_args_support : forall c e F C g a s xs s',
 tag_args c e g a s=Good(xs,s') -> Inv s -> st_w s=w ->
 CxRel w R Fl c e F C -> Later w P R Fl s' ->
 nget(w_syms w)(gs_id g)=Some g -> gs_kind g=KTag ->
 Support WW(YNom(gs_id g)xs).
Proof.
 intros c e F C g a s xs s' E Hi Hw Hc L Hg Kg.
 destruct(tag_args_domain w R Fl b Hload _ _ _ _ _ _ _ _ _ E Hi Hw Hc L)as[Keys Wx].
 apply SupportNom; [right; pose proof(kind_sym_read b w R Fl Hload _ _ Hg)as K;rewrite Kg in K;exact K|exact Keys|].
 unfold tag_args in E. apply bind_inv in E as(tm & s0 & Et & En).
 destruct a as[bs|].
 - apply bind_inv in Et as(u & s1 & Ec & Et).
   apply bind_inv in Et as(sb & s2 & Es & Et). injection Et as <- <-.
   assert(Oc:ok(check_relevant(cx_mod c)g bs))by(unfold check_relevant;apply ok_bind;[apply ok_reqs|intro;okt]).
   destruct(Oc _ _ _ Hi Ec)as[I1 F1].
   destruct(ok_app_subst _ _ _ _ _ _ _ _ I1 Es)as[I2 F2].
   assert(On:ok(nominal_args(cx_mod c)g(sb++tymap e)))by(unfold nominal_args;okt).
   destruct(On _ _ _ I2 En)as[_ Fn].
   pose proof(later_from w R Fl _ _ Fn L)as L2.
   pose proof(app_subst_support_at w R Fl b Hload 200 e(cx_mod c)bs(tymap e)s1 sb s2 Es I1
     (fr_w' w _ _ F1 Hw)(cr_mod_in _ _ _ _ _ _ _ Hc)
     ltac:(intros k y Ey;rewrite nassoc_tymap in Ey;exact(@tn_get_support WW e k y(cr_support _ _ _ _ _ _ _ Hc)Ey))
     ltac:(intros k y Ey;rewrite nassoc_tymap in Ey;exact(cr_ty_wf _ _ _ _ _ _ _ Hc k y Ey))
     (cr_support _ _ _ _ _ _ _ Hc)(cr_ty_wf _ _ _ _ _ _ _ Hc)L2)as Ssb.
   eapply nominal_args_support;[exact En|exact I2|exact L|].
   intros k y Ey. rewrite BodyTy.nassoc_app in Ey. destruct(nassoc k sb)as[z|]eqn:Ez.
   + injection Ey as <-. rewrite Forall_forall in Ssb. exact(Ssb(k,z)(Merge.nassoc_in _ _ _ _ Ez)).
   + rewrite nassoc_tymap in Ey. exact(@tn_get_support WW e k y(cr_support _ _ _ _ _ _ _ Hc)Ey).
 - injection Et as <- <-. eapply nominal_args_support;[exact En|exact Hi|exact L|].
   intros k y Ey. rewrite nassoc_tymap in Ey. exact(@tn_get_support WW e k y(cr_support _ _ _ _ _ _ _ Hc)Ey).
Qed.
End SupportedArgs.
