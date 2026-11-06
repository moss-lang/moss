(* Actual source type reading preserves declaration and argument support. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound
 BodyTy SigSound CanonBodyTy PartialBodyTy SourceClean NominalSupport TySupport.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

Lemma supported_map_tm : forall W Fl tm,
 Forall(fun p=>Support W(snd p))tm -> TmSupport W(map(trp Fl)tm).
Proof.
 intros W Fl tm S x y. induction S as [|[k t]tm St S IH]; cbn[map trp fst snd A.nlook]; [discriminate|].
 destruct(Nat.eqb x k); [intro E; injection E as <-; exact(support_tr Fl St)|exact IH].
Qed.

Section Source.
Variable w:Elab.world.
Context {P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).

Lemma loaded_record_unowned : A.kind_sym WW A.rec_head=None.
Proof.
 assert(E:nget(w_syms w)0=None).
 { destruct(nget(w_syms w)0)as[g|]eqn:Eg; [|reflexivity].
   exfalso. exact(loader_nonzero w R Fl b Hload 0 g Eg eq_refl). }
 unfold A.kind_sym,BodySound.W,WorldSound.W_read,A.rec_head.
 cbn[A.w_decl]. rewrite E. reflexivity.
Qed.

Theorem ety_of_support_at : forall t mi tm e this s d s',
 ety_of t mi tm e this s=Good(d,s') -> Inv s -> st_w s=w -> In mi(w_order w) ->
 (forall k y,nassoc k tm=Some y -> Support WW y) -> (forall k y,nassoc k tm=Some y -> WFty y) ->
 EnvSupport WW e -> (forall k y,tn_get e k=Some y -> WFty y) ->
 (match this with None=>True|Some y=>Support WW y /\ WFty y end) ->
 Later w P R Fl s' -> Support WW d.
Proof.
 intros t mi tm e this s d s' E Hi Hw Hmi St Wt Se We Sthis L.
 assert(Ct:TmClean tm).
 { intros k y Ey. exact(support_clean loaded_record_unowned(St k y Ey)(Wt k y Ey)). }
 assert(Ctn:TmClean(tymap e)).
 { intros k y Ey. rewrite nassoc_tymap in Ey.
   exact(support_clean loaded_record_unowned(@tn_get_support WW e k y Se Ey)(We k y Ey)). }
 assert(Cthis:match this with None=>True|Some y=>ety_ok y=true end).
 { destruct this; [exact(support_clean loaded_record_unowned(proj1 Sthis)(proj2 Sthis))|exact I]. }
 pose proof(ety_of_clean_at w R Fl b Hload _ _ _ _ _ _ _ _ E Hi Hw Hmi Ct Ctn Cthis L)as Cd.
 assert(TR:PartialReading.TmRel Fl tm (map(trp Fl)tm++map(trp Fl)(tymap e))e).
 { unfold PartialReading.TmRel. unfold BodyTy.TmRel. split.
   - intro k. rewrite BodyTy.nlook_app,!nlook_trp,BodyTy.nassoc_app.
     destruct(nassoc k tm); reflexivity.
   - split.
     + intros k y Ey. rewrite BodyTy.nassoc_app in Ey. destruct(nassoc k tm)eqn:Et.
       * injection Ey as <-. exact(Wt _ _ Et).
       * rewrite nassoc_tymap in Ey. exact(We _ _ Ey).
     + intros k Ey. rewrite BodyTy.nassoc_app in Ey. destruct(nassoc k tm); [discriminate|].
       rewrite nassoc_tymap in Ey. exact Ey. }
 assert(TS:TmSupport WW(map(trp Fl)tm++map(trp Fl)(tymap e))).
 { intros k y Ey. rewrite BodyTy.nlook_app,!nlook_trp in Ey.
   destruct(nassoc k tm)as[z|]eqn:Et; cbn[option_map]in Ey.
   - injection Ey as <-. exact(support_tr Fl(St _ _ Et)).
   - rewrite nassoc_tymap in Ey. destruct(tn_get e k)as[z|]eqn:Ez; [|discriminate].
     injection Ey as <-. exact(support_tr Fl(@tn_get_support WW e k z Se Ez)). }
 pose proof E as Et. rewrite PartialReading.ety_of_eq in Et.
 destruct(proj1(PartialReading.ty_all w R Fl b Hload 200) _ _ _ _ _ _ _ _ _ Et Hi Hw Hmi TR
   ltac:(destruct this; [exact(proj2 Sthis)|exact I]) L)as[Ty Wd].
 apply(@support_of_tr WW Fl d Cd). apply(@tyof_support WW _ _ _ _ _ Ty); [exact TS|].
 destruct this; [exact(support_tr Fl(proj1 Sthis))|exact I].
Qed.

Theorem ety_of_support : forall t mi tm e this s d s',
 ety_of t mi tm e this s=Good(d,s') -> Inv s -> st_w s=w -> In mi(w_order w) ->
 Forall(fun p=>Support WW(snd p))tm -> Forall(fun p=>WFty(snd p))tm ->
 EnvSupport WW e -> (forall k y,tn_get e k=Some y -> WFty y) ->
 (match this with None=>True|Some y=>Support WW y /\ WFty y end) ->
 Later w P R Fl s' -> Support WW d.
Proof.
 intros t mi tm e this s d s' E Hi Hw Hmi St Wt Se We Sthis L.
 eapply ety_of_support_at; try eassumption.
 - intros k y Ey. rewrite Forall_forall in St. exact(St (k,y)(Merge.nassoc_in _ _ _ _ Ey)).
 - intros k y Ey. rewrite Forall_forall in Wt. exact(Wt (k,y)(Merge.nassoc_in _ _ _ _ Ey)).
Qed.

Theorem symbol_type_support_at : forall fuel mi g tm e s d s',
 symbol_type fuel mi g tm e s=Good(d,s') -> Inv s -> st_w s=w ->
 nget(w_syms w)(gs_id g)=Some g ->
 (forall k y,nassoc k tm=Some y -> Support WW y) -> (forall k y,nassoc k tm=Some y -> WFty y) ->
 EnvSupport WW e -> (forall k y,tn_get e k=Some y -> WFty y) ->
 Later w P R Fl s' -> Support WW d.
Proof.
 intros fuel mi g tm e s d s' E Hi Hw Hg St Wt Se We L.
 assert(Ct:TmClean tm).
 { intros k y Ey. exact(support_clean loaded_record_unowned(St k y Ey)(Wt k y Ey)). }
 assert(Ctn:TmClean(tymap e)).
 { intros k y Ey. rewrite nassoc_tymap in Ey.
   exact(support_clean loaded_record_unowned(@tn_get_support WW e k y Se Ey)(We k y Ey)). }
 pose proof(proj2(proj2(source_clean_at w R Fl b Hload fuel)) _ _ _ _ _ _ _ E Hi Hw Hg Ct Ctn L)as Cd.
 assert(TR:PartialReading.TmRel Fl tm (map(trp Fl)tm++map(trp Fl)(tymap e))e).
 { unfold PartialReading.TmRel. unfold BodyTy.TmRel. split.
   - intro k. rewrite BodyTy.nlook_app,!nlook_trp,BodyTy.nassoc_app.
     destruct(nassoc k tm); reflexivity.
   - split.
     + intros k y Ey. rewrite BodyTy.nassoc_app in Ey. destruct(nassoc k tm)eqn:Et.
       * injection Ey as <-. exact(Wt _ _ Et).
       * rewrite nassoc_tymap in Ey. exact(We _ _ Ey).
     + intros k Ey. rewrite BodyTy.nassoc_app in Ey. destruct(nassoc k tm); [discriminate|].
       rewrite nassoc_tymap in Ey. exact Ey. }
 assert(TS:TmSupport WW(map(trp Fl)tm++map(trp Fl)(tymap e))).
 { intros k y Ey. rewrite BodyTy.nlook_app,!nlook_trp in Ey.
   destruct(nassoc k tm)as[z|]eqn:Et; cbn[option_map]in Ey.
   - injection Ey as <-. exact(support_tr Fl(St _ _ Et)).
   - rewrite nassoc_tymap in Ey. destruct(tn_get e k)as[z|]eqn:Ez; [|discriminate].
     injection Ey as <-. exact(support_tr Fl(@tn_get_support WW e k z Se Ez)). }
 destruct(proj2(proj2(PartialReading.ty_all w R Fl b Hload fuel)) _ _ _ _ _ _ _ _ E Hi Hw Hg TR L)as[Wd Ty].
 apply(@support_of_tr WW Fl d Cd).
 pose proof(kind_sym_read b w R Fl Hload (gs_id g) g Hg)as Kd.
 destruct(gs_kind g)eqn:Kg; try contradiction.
 - exact(TS _ _ Ty).
 - destruct Ty as(args & Et & Q). rewrite Et. apply TSNom.
   + left; exact Kd.
   + change(map fst args=R(gs_id g)). clear -Q. induction Q as[|r q rs qs[Ek El]Q IH]; cbn; [reflexivity|]. f_equal; assumption.
   + clear -Q TS. induction Q as[|r q rs qs[Ek El]Q IH]; constructor; [exact(TS _ _ El)|exact IH].
 - destruct Ty as(n & body & D & Ty). exact(@tyof_support WW _ _ _ _ _ Ty TS I).
 - destruct Ty as(args & Et & Q). rewrite Et. apply TSNom.
   + right; exact Kd.
   + change(map fst args=R(gs_id g)). clear -Q. induction Q as[|r q rs qs[Ek El]Q IH]; cbn; [reflexivity|]. f_equal; assumption.
   + clear -Q TS. induction Q as[|r q rs qs[Ek El]Q IH]; constructor; [exact(TS _ _ El)|exact IH].
Qed.

Theorem symbol_type_support : forall fuel mi g tm e s d s',
 symbol_type fuel mi g tm e s=Good(d,s') -> Inv s -> st_w s=w ->
 nget(w_syms w)(gs_id g)=Some g ->
 Forall(fun p=>Support WW(snd p))tm -> Forall(fun p=>WFty(snd p))tm ->
 EnvSupport WW e -> (forall k y,tn_get e k=Some y -> WFty y) ->
 Later w P R Fl s' -> Support WW d.
Proof.
 intros fuel mi g tm e s d s' E Hi Hw Hg St Wt Se We L.
 eapply symbol_type_support_at; try eassumption.
 - intros k y Ey. rewrite Forall_forall in St. exact(St (k,y)(Merge.nassoc_in _ _ _ _ Ey)).
 - intros k y Ey. rewrite Forall_forall in Wt. exact(Wt (k,y)(Merge.nassoc_in _ _ _ _ Ey)).
Qed.

Theorem app_subst_support_at : forall fuel e mi bs outer s xs s',
 app_subst fuel e mi bs outer s=Good(xs,s') -> Inv s -> st_w s=w ->In mi(w_order w) ->
 (forall k y,nassoc k outer=Some y -> Support WW y) -> (forall k y,nassoc k outer=Some y -> WFty y) ->
 EnvSupport WW e -> (forall k y,tn_get e k=Some y -> WFty y) ->
 Later w P R Fl s' -> Forall(fun p=>Support WW(snd p))xs.
Proof.
 intros fuel e mi bs outer s xs s' E Hi Hw Hmi St Wt Se We L.
 assert(Ct:TmClean outer).
 { intros k y Ey. exact(support_clean loaded_record_unowned(St k y Ey)(Wt k y Ey)). }
 assert(Ctn:TmClean(tymap e)).
 { intros k y Ey. rewrite nassoc_tymap in Ey.
   exact(support_clean loaded_record_unowned(@tn_get_support WW e k y Se Ey)(We k y Ey)). }
 pose proof(proj1(proj2(source_clean_at w R Fl b Hload fuel)) _ _ _ _ _ _ _ E Hi Hw Hmi Ct Ctn L)as Cxs.
 assert(TR:PartialReading.TmRel Fl outer (map(trp Fl)outer++map(trp Fl)(tymap e))e).
 { unfold PartialReading.TmRel. unfold BodyTy.TmRel. split.
   - intro k. rewrite BodyTy.nlook_app,!nlook_trp,BodyTy.nassoc_app.
     destruct(nassoc k outer); reflexivity.
   - split.
     + intros k y Ey. rewrite BodyTy.nassoc_app in Ey. destruct(nassoc k outer)eqn:Et.
       * injection Ey as <-. exact(Wt _ _ Et).
       * rewrite nassoc_tymap in Ey. exact(We _ _ Ey).
     + intros k Ey. rewrite BodyTy.nassoc_app in Ey. destruct(nassoc k outer); [discriminate|].
       rewrite nassoc_tymap in Ey. exact Ey. }
 assert(TS:TmSupport WW(map(trp Fl)outer++map(trp Fl)(tymap e))).
 { intros k y Ey. rewrite BodyTy.nlook_app,!nlook_trp in Ey.
   destruct(nassoc k outer)as[z|]eqn:Et; cbn[option_map]in Ey.
   - injection Ey as <-. exact(support_tr Fl(St _ _ Et)).
   - rewrite nassoc_tymap in Ey. destruct(tn_get e k)as[z|]eqn:Ez; [|discriminate].
     injection Ey as <-. exact(support_tr Fl(@tn_get_support WW e k z Se Ez)). }
 destruct(proj1(proj2(PartialReading.ty_all w R Fl b Hload fuel)) _ _ _ _ _ _ _ _ E Hi Hw Hmi
   (PartialReading.tmrel_outer _ _ _ _ TR) L)as(lr & F & El & Wx).
 change(map(trp Fl)xs=rev lr)in El.
 assert(Tlr:Forall(fun q=>TySupport WW(snd q))lr).
 { clear -F TS. induction F as[|b0 q bs lr (sp & sa & E1 & E2 & E3 & Ty)F IH]; constructor; [|exact IH].
   exact(@tyof_support WW _ _ _ _ _ Ty TS I). }
 assert(Txs:Forall(fun q=>TySupport WW(tr Fl(snd q)))xs).
 { assert(M:Forall(fun q=>TySupport WW(snd q))(map(trp Fl)xs)).
   { rewrite El. apply Forall_rev; exact Tlr. }
   rewrite Forall_map in M. exact M. }
 rewrite Forall_forall in Cxs,Txs|-*. intros p Hp. exact(@support_of_tr WW Fl(snd p)(Cxs p Hp)(Txs p Hp)).
Qed.

Theorem app_subst_support : forall fuel e mi bs outer s xs s',
 app_subst fuel e mi bs outer s=Good(xs,s') -> Inv s -> st_w s=w ->In mi(w_order w) ->
 Forall(fun p=>Support WW(snd p))outer -> Forall(fun p=>WFty(snd p))outer ->
 EnvSupport WW e -> (forall k y,tn_get e k=Some y -> WFty y) ->
 Later w P R Fl s' -> Forall(fun p=>Support WW(snd p))xs.
Proof.
 intros fuel e mi bs outer s xs s' E Hi Hw Hmi St Wt Se We L.
 eapply app_subst_support_at; try eassumption.
 - intros k y Ey. rewrite Forall_forall in St. exact(St (k,y)(Merge.nassoc_in _ _ _ _ Ey)).
 - intros k y Ey. rewrite Forall_forall in Wt. exact(Wt (k,y)(Merge.nassoc_in _ _ _ _ Ey)).
Qed.

End Source.
