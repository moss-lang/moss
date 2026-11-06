(* The actual payload reader, before specializing its argument environment. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
  BodySound BodyTy SigSound.
Import ListNotations.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL].
Section Payload.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.

Theorem payload_type_rel_at : forall x g name ps body xs xsr e s pay s',
  nget (w_syms w) x = Some g ->
  gs_decl g = ADTag name ps body ->
  payload_type (YNom x xs) e s = Good (pay,s') ->
  Inv s -> st_w s = w -> TmRel Fl xs xsr e ->
  Later w P R Fl s' ->
  A.PayloadOf (BodySound.W w R Fl) x xsr (tr Fl pay) /\ WFty pay.
Proof.
  intros x g name ps body xs xsr e s pay s' Hg Hd E Hi Hw T L.
  pose proof (iv_loader _ Hi b ltac:(rewrite Hw; exact Hload) x g
    ltac:(rewrite Hw; exact Hg)) as Hgs.
  cbn [payload_type] in E. apply bind_inv in E as (g0 & s0 & Eg & E).
  unfold sym in Eg. rewrite Hgs in Eg. injection Eg as <- <-.
  rewrite Hd in E.
  destruct (ety_of_rel_at w R Fl b Hload _ _ _ _ _ _ _ _ _ E Hi Hw
    (loader_mod_in w R Fl b Hload x g Hg) T I L) as [Ty Wp].
  split; [|exact Wp].
  eapply A.PayTag with (d:=A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g));
    [exact (w_decl_read w R Fl x g Hg)|exact Hd|exact Ty].
Qed.
End Payload.

From NM Require Import PartialBodyTy TyOfRestrict NominalSupport CanonBodyTy.
Section SupportedPayload.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySound.W w R Fl).

Theorem payload_type_complete : forall c e F C x xs s pay s',
  payload_type (YNom x xs) e s = Good(pay,s') -> Inv s -> st_w s=w ->
  CxRel w R Fl c e F C -> Later w P R Fl s' ->
  map fst xs = A.w_reqs WW x -> Forall(fun p=>WFty(snd p))xs ->
  A.RelevanceClosed WW (A.w_reqs WW) ->
  (exists g,nget(w_syms w)x=Some g /\ gs_kind g=KTag) ->
  A.PayloadOf WW x (map(trp Fl)xs) (tr Fl pay) /\ WFty pay.
Proof.
 intros c e F C x xs s pay s' E Hi Hw Hc L Keys Wx Closed [g [Hg Kg]].
 pose proof (kind_sym_read b w R Fl Hload x g Hg) as Kd.
 unfold A.kind_sym in Kd. rewrite (w_decl_read w R Fl x g Hg) in Kd.
 cbn [A.d_ast] in Kd. rewrite Kg in Kd.
 destruct (gs_decl g) as [asm ds|nt0|na ta|nt ps body|nu|nv tv vi|recv nf pf rf bf|nc its|nf pp aa rr bb] eqn:D;
   cbn [A.kind_of kconv] in Kd; try discriminate;
   try (destruct recv; discriminate).
 pose proof (iv_loader _ Hi b ltac:(rewrite Hw; exact Hload) x g
   ltac:(rewrite Hw; exact Hg)) as Hgs.
 cbn [payload_type] in E. apply bind_inv in E as (g0 & s0 & Eg & E).
 unfold sym in Eg. rewrite Hgs in Eg. injection Eg as <- <-. rewrite D in E.
 set (tm0:=map(trp Fl)xs++map(trp Fl)(tymap e)).
 assert (T : PartialReading.TmRel Fl xs tm0 e).
 { unfold PartialReading.TmRel,tm0.
   eapply tmrel_app; [|reflexivity|exact Wx].
   apply tmrel_tymap; [exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|].
   intro k. rewrite nlook_trp,nassoc_tymap. reflexivity. }
 rewrite PartialReading.ety_of_eq in E.
 destruct (proj1(PartialReading.ty_all w R Fl b Hload 200) _ _ _ _ _ _ _ _ _ E Hi Hw
   (loader_mod_in w R Fl b Hload x g Hg) T I L) as [Ty Wpay].
 split; [|exact Wpay].
 eapply A.PayTag with (d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g));
   [exact(w_decl_read w R Fl x g Hg)|exact D|].
 apply (@tyof_restrict WW Closed _ _ _ _ _ Ty (map(trp Fl)xs)). intros r Fr.
 assert (Hr : In r (A.w_reqs WW x)).
 { exact (proj2(@Closed x (A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)) ps body
     (w_decl_read w R Fl x g Hg) ltac:(cbn[A.d_ast]; rewrite D; reflexivity)) r Fr). }
 assert (Hkeys : In r (map fst xs)) by (rewrite Keys; exact Hr).
 apply in_map_iff in Hkeys as ([k y] & Ek & Hin). cbn in Ek. subst k.
 unfold tm0. rewrite BodyTy.nlook_app.
 destruct(A.nlook r(map(trp Fl)xs)) as [z|] eqn:Er; [reflexivity|].
 exfalso. apply (nlook_in_defined r (map(trp Fl)xs) (tr Fl y)); [|exact Er].
 apply in_map_iff. exists(r,y). split; [reflexivity|exact Hin].
Qed.
Theorem payload_type_supported : forall c e F C x xs s pay s',
  payload_type (YNom x xs) e s = Good(pay,s') -> Inv s -> st_w s=w ->
  CxRel w R Fl c e F C -> Later w P R Fl s' ->
  Support WW (YNom x xs) -> WFty(YNom x xs) ->
  A.RelevanceClosed WW (A.w_reqs WW) ->
  (exists g,nget(w_syms w)x=Some g /\ gs_kind g=KTag) ->
  A.PayloadOf WW x (map(trp Fl)xs) (tr Fl pay) /\ WFty pay.
Proof.
 intros c e F C x xs s pay s' E Hi Hw Hc L Sup Wnom Closed Hg.
 eapply payload_type_complete; try eassumption.
 - exact (proj1(proj2(support_nominal Sup))).
 - unfold WFty in Wnom. cbn [ety_wf] in Wnom.
   rewrite Forall_forall. intros p Hp. exact (proj1(forallb_forall _ _) Wnom p Hp).
Qed.
End SupportedPayload.

From NM Require Import SourceSupport.
Section PayloadSupport.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySound.W w R Fl).

Theorem payload_type_support : forall c e F C x xs s pay s',
 payload_type(YNom x xs)e s=Good(pay,s') -> Inv s -> st_w s=w ->
 CxRel w R Fl c e F C -> Later w P R Fl s' -> EnvSupport WW e ->
 Support WW(YNom x xs) -> WFty(YNom x xs) ->
 (exists g,nget(w_syms w)x=Some g) -> Support WW pay.
Proof.
 intros c e F C x xs s pay s' E Hi Hw Hc L Se Sup Wnom [g Hg].
 pose proof(iv_loader _ Hi b ltac:(rewrite Hw; exact Hload)x g
   ltac:(rewrite Hw; exact Hg))as Hgs.
 cbn[payload_type]in E. apply bind_inv in E as(g0 & s0 & Eg & E).
 unfold sym in Eg. rewrite Hgs in Eg. injection Eg as <- <-.
 destruct(gs_decl g)eqn:D; try (injection E as <- <-; constructor).
 eapply ety_of_support; [exact Hload|exact E|exact Hi|exact Hw|exact(loader_mod_in w R Fl b Hload x g Hg)| | |exact Se|exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|exact I|exact L].
 - exact(proj2(proj2(support_nominal Sup))).
 - unfold WFty in Wnom. cbn[ety_wf]in Wnom.
   rewrite Forall_forall. intros p Hp. exact(proj1(forallb_forall _ _) Wnom p Hp).
Qed.

Theorem payload_type_supported_all : forall c e F C x xs s pay s',
 payload_type(YNom x xs)e s=Good(pay,s') -> Inv s -> st_w s=w ->
 CxRel w R Fl c e F C -> Later w P R Fl s' -> EnvSupport WW e ->
 Support WW(YNom x xs) -> WFty(YNom x xs) ->
 (exists g,nget(w_syms w)x=Some g /\ gs_kind g=KTag) ->
 A.PayloadOf WW x(map(trp Fl)xs)(tr Fl pay) /\ WFty pay /\ Support WW pay.
Proof.
 intros c e F C x xs s pay s' E Hi Hw Hc L Se Sup Wnom Hg.
 destruct(payload_type_supported w R Fl b Hload _ _ _ _ _ _ _ _ _ E Hi Hw Hc L Sup Wnom
   (lt_reqclosed _ _ _ _ _ L) Hg)as[Po Wp].
 split; [exact Po|]. split; [exact Wp|].
 eapply payload_type_support; try eassumption. destruct Hg as[g[Hg _]]. exists g; exact Hg.
Qed.
End PayloadSupport.
