(* Every loaded function body occurs in the actual lower_all traversal;
   all three declaration tables are included, independently of imports. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
  FnHistory FnStable FnEmitStable FnSynthStable FnReadonly FnCompleted.
Import ListNotations.
Module A:=Audit.Elaboration.
#[local] Hint Resolve kf_entry_zero_width kf_check_main_signature : keepsdb.
Local Strategy opaque [entry_zero_width].

Lemma coverage_assoc_in : forall K V (eqb:K->K->bool) k (xs:list(K*V)) v,
  assoc eqb k xs=Some v ->exists key,In(key,v)xs.
Proof.
 intros K V eqb k xs;induction xs as[|[a b]rest IH];intros v H;cbn in H;[discriminate|].
 destruct(eqb k a);[injection H as <-;exists a;left;reflexivity|].
 destruct(IH v H)as[key Hin];exists key;right;exact Hin.
Qed.

Lemma loaded_body_traversed : forall b w id g receiver name parameters result body,
  load_world b=Good w ->nget(w_syms w)id=Some g ->
  gs_decl g=ADFn receiver name parameters result(Some body) ->
  In(gs_mod g)(w_order w) /\
  In id(module_function_ids(get_mod w(gs_mod g))) /\
  (is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod))=true.
Proof.
 intros b w id g receiver name parameters result body Load Lookup Decl.
 unfold load_world in Load;apply rbind_good in Load as[[entry state][Run Final]].
 injection Final as <-.
 destruct(load_spec b _ _ _ _ _ _ Run(linv_init b))as[Inv _].
 destruct(li_symfacts _ _ Inv id g Lookup)
   as(m & p & am & Module & Loaded & Source & Occurs & Kind & Named & Detached & Attached).
 assert(Get:get_mod (World(ls_mods state)(ls_order state)entry(ls_syms state)
   (ls_next_sym state)(ls_files state)(Loader.library_dir(b_prelude b)))(gs_mod g)=m).
 { unfold get_mod;cbn[w_mods];rewrite Module;reflexivity. }
 split;[apply(li_order _ _ Inv);exists p;exact Loaded|].
 split.
 - rewrite Get;unfold module_function_ids;destruct receiver as[| |rn].
   + apply in_or_app;left;apply in_map_iff;exists(name,id);split;[reflexivity|].
     apply sassoc_in;apply Named;rewrite Decl;reflexivity.
   + apply in_or_app;right;apply in_or_app;left;apply in_map_iff.
     exists(name,id);split;[reflexivity|apply sassoc_in;apply Detached;rewrite Decl;reflexivity].
   + destruct(Attached rn name parameters result(Some body)Decl)
       as[r[gr[Name[Gr[Same Method]]]]].
     apply in_or_app;right;apply in_or_app;right;apply in_map_iff.
     destruct(coverage_assoc_in _ _ _ _ _ _ Method)as[key Hin].
     exists(key,id);auto.
 - pose proof(li_kind _ _ Inv id g Lookup)as K.
   unfold KindOK in K;rewrite Decl in K.
   unfold fn_body;rewrite Decl;cbn[is_some].
   destruct receiver;destruct(gs_kind g);cbn[kconv]in K;try discriminate;reflexivity.
Qed.

Lemma lower_body_preserves_present : forall fuel g b s final id old,
  Inv s ->load_world b=Good(st_w s)->loaded_fn_key(st_w s)id ->
  nassoc id(st_fns s)=Some old ->lower_body fuel g s=Good(tt,final)->
  exists body,nassoc id(st_fns final)=Some body.
Proof.
 intros fuel g b s final id old Inv Load Loaded Before Run.
 destruct(Nat.eq_dec id(gs_id g))as[Same|Different].
 - subst id;pose proof(@lower_body_already_present fuel g s old final Before Run)as E.
   subst final;exists old;exact Before.
 - exists old;rewrite(proj2(ne_lower_body fuel g)b s tt final Inv Load Run id Loaded Different).
   exact Before.
Qed.

Lemma lower_one_preserves_present : forall fuel key b s final id old,
  Inv s ->load_world b=Good(st_w s)->loaded_fn_key(st_w s)id ->
  nassoc id(st_fns s)=Some old ->lower_one fuel key s=Good(tt,final)->
  exists body,nassoc id(st_fns final)=Some body.
Proof.
 intros fuel key b s final id old Inv Load Loaded Before Run.
 unfold lower_one, LowerOne.lower_one in Run;apply bind_inv in Run as[g[mid[Read Run]]].
 unfold sym in Read;destruct(nget(st_syms s)key)as[got|];[|discriminate].
 injection Read as <- <-.
 destruct(is_some(fn_body got)&&(kind_eqb(gs_kind got)KFn||kind_eqb(gs_kind got)KMethod)).
 - eapply lower_body_preserves_present;eassumption.
 - injection Run as <-;exists old;exact Before.
Qed.

Lemma lower_many_preserves_present : forall keys fuel b s final id old,
  Inv s ->load_world b=Good(st_w s)->loaded_fn_key(st_w s)id ->
  nassoc id(st_fns s)=Some old ->miter(lower_one fuel)keys s=Good(tt,final)->
  exists body,nassoc id(st_fns final)=Some body.
Proof.
 induction keys as[|key rest IH];intros fuel b s final id old Inv Load Loaded Before Run.
 - cbn[miter ret]in Run;injection Run as <-;exists old;exact Before.
 - cbn[miter]in Run;apply bind_inv in Run as[u[mid[First Last]]];destruct u.
   destruct(lower_one_preserves_present fuel key b s mid id old Inv Load Loaded Before First)
     as[body Present].
   destruct(proj1(proj1(fh_lower_one fuel key))s tt mid Inv First)as[InvMid Frame].
   eapply IH;[exact InvMid|rewrite(fr_w _ _ Frame);exact Load|
     rewrite(fr_w _ _ Frame);exact Loaded|exact Present|exact Last].
Qed.

Lemma lower_many_visits : forall keys fuel b s final id g,
  Inv s ->load_world b=Good(st_w s)->nget(w_syms(st_w s))id=Some g ->
  fn_body g<>None ->
  (is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod))=true ->
  In id keys ->miter(lower_one fuel)keys s=Good(tt,final)->
  exists body,nassoc id(st_fns final)=Some body.
Proof.
 induction keys as[|key rest IH];intros fuel b s final id g Inv Load Loaded Body Select Visit Run;
   [contradiction|].
 cbn[miter]in Run;apply bind_inv in Run as[u[mid[First Last]]];destruct u.
 destruct(proj1(proj1(fh_lower_one fuel key))s tt mid Inv First)as[InvMid Frame].
 assert(LoadMid:load_world b=Good(st_w mid))by(rewrite(fr_w _ _ Frame);exact Load).
 assert(LoadedMid:nget(w_syms(st_w mid))id=Some g)by(rewrite(fr_w _ _ Frame);exact Loaded).
 destruct Visit as[Same|Visit].
 - subst key;unfold lower_one, LowerOne.lower_one in First.
   pose proof(iv_loader s Inv b Load id g Loaded)as Present.
   unfold sym,mbind in First;rewrite Present,Select in First.
   destruct(@lower_body_emits fuel g s mid Body First)as[body Emitted].
   rewrite(iv_ids s Inv id g Present)in Emitted.
   eapply lower_many_preserves_present;[exact InvMid|exact LoadMid|
     exists g;exact LoadedMid|exact Emitted|exact Last].
 - exact(IH fuel b mid final id g InvMid LoadMid LoadedMid Body Select Visit Last).
Qed.

Definition source_function_ids(w:Elab.world):list nat:=
  flat_map(fun mi=>module_function_ids(get_mod w mi))(w_order w).

Lemma miter_app_unit : forall X(f:X->M unit)xs ys s final,
  miter f(xs++ys)s=Good(tt,final) <->
  exists mid,miter f xs s=Good(tt,mid)/\miter f ys mid=Good(tt,final).
Proof.
 intros X f xs;induction xs as[|x rest IH];intros ys s final;cbn[miter].
 - split;intro H;[exists s;auto|destruct H as[mid[First Last]];injection First as <-;exact Last].
 - split.
   + intro H;apply bind_inv in H as[u[mid[First Last]]];destruct u.
     apply IH in Last as[next[A B]];exists next;split;[|exact B].
     unfold mbind;rewrite First;exact A.
   + intros[next[First Last]];apply bind_inv in First as[u[mid[A B]]];destruct u.
     change(mbind(f x)(fun _=>miter f(rest++ys))s=Good(tt,final)).
     unfold mbind;rewrite A;apply IH;exists next;auto.
Qed.

Lemma miter_flat_map_unit : forall X Y(f:Y->M unit)(parts:X->list Y)xs s final,
  miter(fun x=>miter f(parts x))xs s=Good(tt,final) <->
  miter f(flat_map parts xs)s=Good(tt,final).
Proof.
 intros X Y f parts xs;induction xs as[|x rest IH];intros s final;cbn[miter flat_map];[tauto|].
 rewrite miter_app_unit;split.
 - intro H;apply bind_inv in H as[u[mid[First Last]]];destruct u.
   exists mid;split;[exact First|apply IH;exact Last].
 - intros[mid[First Last]];unfold mbind;rewrite First;apply IH;exact Last.
Qed.

Theorem lower_all_contains_defined_body : forall b s final id g receiver name parameters result body,
  Inv s ->load_world b=Good(st_w s)->nget(w_syms(st_w s))id=Some g ->
  gs_decl g=ADFn receiver name parameters result(Some body) ->
  lower_all s=Good(tt,final)->exists lowered,nassoc id(st_fns final)=Some lowered.
Proof.
 intros b s final id g receiver name parameters result body Inv Load Loaded Decl Run.
 destruct(loaded_body_traversed b(st_w s)id g receiver name parameters result body
   Load Loaded Decl)as[Module[Visit Select]].
 unfold lower_all in Run;apply bind_inv in Run as[snapshot[mid[Read Run]]];injection Read as <- <-.
 change (miter (fun mi => miter (lower_one ELAB_FUEL)
   (module_function_ids (get_mod (st_w s) mi)))
   (w_order (st_w s)) s = Good (tt,final)) in Run.
 apply miter_flat_map_unit in Run.
 eapply lower_many_visits;[exact Inv|exact Load|exact Loaded| |exact Select| |exact Run].
 - unfold fn_body;rewrite Decl;discriminate.
 - apply in_flat_map;exists(gs_mod g);auto.
Qed.

Definition all_defined_present(s:st):Prop:=forall id g receiver name parameters result body,
 nget(w_syms(st_w s))id=Some g ->gs_decl g=ADFn receiver name parameters result(Some body)->
 exists lowered,nassoc id(st_fns s)=Some lowered.
Definition completes_defined{A}(m:M A):Prop:=ok m /\forall b s a final,
 Inv s ->load_world b=Good(st_w s)->m s=Good(a,final)->all_defined_present final.
Definition preserves_defined{A}(m:M A):Prop:=ok m /\forall b s a final,
 Inv s ->load_world b=Good(st_w s)->all_defined_present s ->m s=Good(a,final)->all_defined_present final.
Arguments completes_defined:simpl never.
Arguments preserves_defined:simpl never.

Lemma nf_preserves_defined:forall A(m:M A),NF m ->preserves_defined m.
Proof.
 intros A m[OK Stable];split;[exact OK|].
 intros b s value final Inv Load Before Run id g receiver name parameters result body Loaded Decl.
 destruct(OK s value final Inv Run)as[InvFinal Frame].
 rewrite(fr_w _ _ Frame)in Loaded.
 destruct(Before id g receiver name parameters result body Loaded Decl)as[lowered Present].
 exists lowered;rewrite(Stable b s value final Inv Load Run id);[exact Present|exists g;exact Loaded].
Qed.
Lemma lower_all_completes_defined:completes_defined lower_all.
Proof.
 split;[exact(proj1(proj1 fh_lower_all))|].
 intros b s value final Inv Load Run;destruct value.
 destruct(proj1(proj1 fh_lower_all)s tt final Inv Run)as[InvFinal Frame].
 intros id g receiver name parameters result body Loaded Decl.
 rewrite(fr_w _ _ Frame)in Loaded.
 exact(lower_all_contains_defined_body b s final id g receiver name parameters result body
   Inv Load Loaded Decl Run).
Qed.
Lemma completes_bind_first:forall A B(m:M A)(k:A->M B),
 completes_defined m ->(forall a,preserves_defined(k a))->completes_defined(mbind m k).
Proof.
 intros A B m k[OKm Complete]K;split.
 - apply ok_bind;[exact OKm|intro a;exact(proj1(K a))].
 - intros b s value final Inv Load Run;apply bind_inv in Run as[a[mid[First Last]]].
   destruct(OKm s a mid Inv First)as[InvMid Frame].
   eapply(proj2(K a));[exact InvMid|rewrite(fr_w _ _ Frame);exact Load|
     exact(Complete b s a mid Inv Load First)|exact Last].
Qed.
Lemma completes_bind_later:forall A B(m:M A)(k:A->M B),
 ok m ->(forall a,completes_defined(k a))->completes_defined(mbind m k).
Proof.
 intros A B m k OKm K;split.
 - apply ok_bind;[exact OKm|intro a;exact(proj1(K a))].
 - intros b s value final Inv Load Run;apply bind_inv in Run as[a[mid[First Last]]].
   destruct(OKm s a mid Inv First)as[InvMid Frame].
   eapply(proj2(K a));[exact InvMid|rewrite(fr_w _ _ Frame);exact Load|exact Last].
Qed.

Lemma elab_program_completes_defined:completes_defined elab_program.
Proof.
 unfold elab_program.
 apply completes_bind_later;[solve[okt]|intros []].
 apply completes_bind_later;[solve[okt]|intros []].
 apply completes_bind_later;[solve[okt]|intros []].
 apply completes_bind_first;[exact lower_all_completes_defined|intros []].
 apply nf_preserves_defined.
 apply nf_keeps.
 - okt.
 - keepst.
Qed.

Theorem elab_program_defined_bodies_present:forall b s p final id g receiver name parameters result body,
 Inv s ->load_world b=Good(st_w s)->elab_program s=Good(p,final)->
 nget(w_syms(st_w s))id=Some g ->gs_decl g=ADFn receiver name parameters result(Some body)->
 exists lowered,nassoc id(Audit.xo_fns p)=Some lowered.
Proof.
 intros b s p final id g receiver name parameters result body Inv Load Run Loaded Decl.
 destruct(proj1 elab_program_completes_defined s p final Inv Run)as[InvFinal Frame].
 rewrite(@elab_program_tables s p final Run).
 eapply(proj2 elab_program_completes_defined b s p final Inv Load Run);[|exact Decl].
 rewrite(fr_w _ _ Frame);exact Loaded.
Qed.

Lemma fn_loaded_initial_inv:forall b w,load_world b=Good w->Inv(s_init w).
Proof.
 intros b w Load;destruct(load_world_syms b w Load)as[Positive Above].
 constructor;cbn[st_next st_syms st_flds st_w st_reqs s_init].
 - exact Above.
 - split;[constructor|reflexivity].
 - exact(load_world_ids b w Load).
 - intros b' Load' id g H;exact H.
 - intros b' Load' id g Missing H;rewrite Missing in H;discriminate.
 - intros b' Load' id values H;destruct id;discriminate.
 - intros b' Load' id g Missing H;rewrite Missing in H;discriminate.
Qed.

Theorem accepted_defined_functions_completed:forall b p,
 elaborate b=Good p->exists w final,
 load_world b=Good w/\elab_program(s_init w)=Good(p,final)/\
 forall id g receiver name parameters result source_body,
 nget(w_syms w)id=Some g->gs_decl g=ADFn receiver name parameters result(Some source_body)->
 exists lowered,nassoc id(Audit.xo_fns p)=Some lowered/\completed_source_fn w final id lowered.
Proof.
 intros b p Accepted;unfold elaborate in Accepted.
 destruct(load_world b)as[w|message]eqn:Load;cbn[rbind]in Accepted;[|discriminate].
 change(match elab_program(s_init w)with Good(program,_)=>Good program|Bad message=>Bad message end=Good p)in Accepted.
 destruct(elab_program(s_init w))as[[program final]|message]eqn:Run;[|discriminate].
 injection Accepted as <-;exists w,final;split;[reflexivity|];split;[exact Run|].
 intros id g receiver name parameters result source_body Loaded Decl.
 pose proof(fn_loaded_initial_inv b w Load)as Inv.
 destruct(elab_program_defined_bodies_present b(s_init w)program final id g receiver name parameters result source_body
   Inv Load Run Loaded Decl)as[lowered Present].
 exists lowered;split;[exact Present|].
 eapply(@elab_completed_source_bodies b w program final Load Inv Run id lowered);[exists g;exact Loaded|exact Present].
Qed.
