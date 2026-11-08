(* Completed runs account for the loaded bodies visible after lowering. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv InfoInvariant InfoPreserve ProgramInfo FnReadonly FnHistory FnStable FnEmitStable FnSynthStable FwdReadonly FwdInvariant FwdPreserve FwdSynthInv FwdProgramInv LowerOne.
From NM Require Import ProgramTail.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
#[local]Hint Resolve kw_entry_zero_width_present kw_check_main_signature_present:keepsdb.
#[local]Hint Resolve okF_lower_all:fwdb.
Definition NF_except(owner:nat){A}(m:M A):Prop:=ok m /\forall b s a s',
 Inv s ->load_world b=Good(st_w s)->m s=Good(a,s')->
 forall f,loaded_fn_key(st_w s)f ->f<>owner ->nassoc f(st_fns s')=nassoc f(st_fns s).
Arguments NF_except:simpl never.
Lemma ne_of_nf:forall A owner(m:M A),NF m ->NF_except owner m.
Proof. intros A owner m[OK Stable];split;[exact OK|];intros;eapply Stable;eassumption. Qed.
Lemma ne_bind:forall A B owner(m:M A)(k:A->M B),
 NF_except owner m ->(forall a,NF_except owner(k a))->NF_except owner(mbind m k).
Proof.
 intros A B owner m k[OKm Nm]K;split.
 - apply ok_bind;[exact OKm|intro a;exact(proj1(K a))].
 - intros b s value final InvS Load Run f Loaded Other.
   apply bind_inv in Run as[a[mid[First Last]]].
   destruct(OKm s a mid InvS First)as[InvMid Frame].
   assert(LoadMid:load_world b=Good(st_w mid))by(rewrite(fr_w _ _ Frame);exact Load).
   assert(LoadedMid:loaded_fn_key(st_w mid)f)by(rewrite(fr_w _ _ Frame);exact Loaded).
   rewrite(proj2(K a)b mid value final InvMid LoadMid Last f LoadedMid Other).
   exact(Nm b s a mid InvS Load First f Loaded Other).
Qed.
Lemma ne_emit:forall owner body,NF_except owner(emit_fn owner body).
Proof.
 intros owner body;split;[apply ok_emit_fn|].
 intros b s a final InvS Load Run f Loaded Other.
 unfold emit_fn,upd in Run;injection Run as <- <-.
 cbn[s_fns st_fns nassoc assoc];destruct(Nat.eqb f owner)eqn:Same;[apply Nat.eqb_eq in Same;contradiction|reflexivity].
Qed.
Ltac net:=repeat(fintros;first[
 solve[apply ne_of_nf;nft]|
 lazymatch goal with
 | |-NF_except ?owner(emit_fn ?owner _)=>apply ne_emit
 | |-NF_except _(mbind _ _)=>apply ne_bind
 | |-NF_except _ ?m=>match m with context[match ?x with _=>_ end]=>destruct x end
 end]).
Lemma ne_lower_body:forall fuel g,NF_except(gs_id g)(lower_body fuel g).
Proof. intros[|fuel]g;cbn[lower_body];net. Qed.

Lemma lower_body_already_present:forall fuel g s old final,
 nassoc(gs_id g)(st_fns s)=Some old ->
 lower_body fuel g s=Good(tt,final)->final=s.
Proof.
 intros[|fuel]g s old final Lookup Run;[discriminate|].
 cbn[lower_body]in Run;unfold lowered in Run.
 apply bind_inv in Run as[done[mid[Read Run]]].
 unfold mbind,get,ret in Read;rewrite Lookup in Read;injection Read as <- <-.
 injection Run as <-;reflexivity.
Qed.

Lemma lower_body_history:forall fuel g b s final,
 nget(w_syms(st_w s))(gs_id g)=Some g ->Inv s ->InfoInv s ->FwdInv s ->
 load_world b=Good(st_w s)->lower_body fuel g s=Good(tt,final)->
 fn_history(st_w s)s final.
Proof.
 intros fuel g b s final LoadedG InvS InfoS FwdS Load Run f body Loaded Lookup.
 destruct(Nat.eq_dec f(gs_id g))as[Same|Other].
 - subst f;destruct(nassoc(gs_id g)(st_fns s))as[old|]eqn:Before.
   + pose proof(@lower_body_already_present fuel g s old final Before Run)as E.
     subst final;left;rewrite <-Before;exact Lookup.
   + right;exists fuel,g,s,final;repeat first[assumption|reflexivity|solve[apply frl_of_fr;apply fr_refl]|split].
 - left;rewrite <-(proj2(ne_lower_body fuel g)b s tt final InvS Load Run f Loaded Other).
   exact Lookup.
Qed.

Definition FH{A}(m:M A):Prop:=okI m /\forall b s a final,
 Inv s ->InfoInv s ->FwdInv s ->load_world b=Good(st_w s)->m s=Good(a,final)->
 fn_history(st_w s)s final.
Arguments FH:simpl never.
Lemma fh_ret:forall A(a:A),FH(ret a).
Proof. intros A a;split;[apply okI_ret|];intros b s value final InvS InfoS FwdS Load Run;injection Run as <- <-;apply fn_history_refl. Qed.
Lemma fh_bind:forall A B(m:M A)(k:A->M B),FH m ->okF m->(forall a,FH(k a))->FH(mbind m k).
Proof.
 intros A B m k[OKm Histm]Fwdm K;split.
 - apply okI_bind;[exact OKm|intro a;exact(proj1(K a))].
 - intros b s value final InvS InfoS FwdS Load Run.
   apply bind_inv in Run as[a[mid[First Last]]].
   destruct(proj1 OKm s a mid InvS First)as[InvMid Frame].
   pose proof(proj2 OKm s a mid InvS InfoS First)as InfoMid.
   pose proof(proj2 Fwdm s a mid InvS FwdS First)as FwdMid.
   assert(LoadMid:load_world b=Good(st_w mid))by(rewrite(fr_w _ _ Frame);exact Load).
   pose proof(proj2(K a)b mid value final InvMid InfoMid FwdMid LoadMid Last)as HistLast.
   rewrite(fr_w _ _ Frame)in HistLast.
   eapply fn_history_trans;[exact(Histm b s a mid InvS InfoS FwdS Load First)|exact HistLast|].
   apply frl_of_fr;exact(proj2(proj1(proj1(K a)) _ _ _ InvMid Last)).
Qed.
Lemma fh_miter:forall A(f:A->M unit)xs,(forall x,FH(f x))->(forall x,okF(f x))->FH(miter f xs).
Proof.
 intros A f xs F Fwd;induction xs as[|x xs IH];cbn[miter];[apply fh_ret|].
 apply fh_bind;[apply F|apply Fwd|intro;exact IH].
Qed.

Definition lower_one := LowerOne.lower_one.
Lemma fh_lower_one:forall fuel id,FH(lower_one fuel id).
Proof.
 intros fuel id;split.
 - unfold lower_one, LowerOne.lower_one;okIt.
 - intros b s value final InvS InfoS FwdS Load Run;unfold lower_one, LowerOne.lower_one in Run.
   apply bind_inv in Run as[g[mid[Read Run]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate].
   injection Read as <- <-.
   destruct(is_some(fn_body got)&&(kind_eqb(gs_kind got)KFn||kind_eqb(gs_kind got)KMethod))eqn:Selected.
   + assert(Id:gs_id got=id)by(exact(iv_ids s InvS id got Lookup)).
     destruct(nget(w_syms(st_w s))id)as[original|]eqn:Loaded.
     * pose proof(iv_loader s InvS b Load id original Loaded)as Same.
       rewrite Lookup in Same;injection Same as <-.
       destruct value; eapply lower_body_history;try eassumption;rewrite Id;exact Loaded.
     * apply fn_stable_history; eapply(proj2(nf_lower_body_unloaded fuel got));try eassumption.
       intros[original Present];rewrite Id,Loaded in Present;discriminate.
   + injection Run as <- <-;apply fn_history_refl.
Qed.
Lemma fh_lower_all:FH lower_all.
Proof.
 split;[unfold lower_all;okIt|].
 intros b s value final InvS InfoS FwdS Load Run;unfold lower_all in Run.
 apply bind_inv in Run as[s0[mid[Read Run]]];injection Read as <- <-.
 assert(Fold:FH(miter(fun mi=>let m:=get_mod(st_w s)mi in
   miter(lower_one ELAB_FUEL)(module_function_ids m))
   (w_order(st_w s)))).
 {apply fh_miter;[intro mi;apply fh_miter;[intro id;apply fh_lower_one|intro id;unfold lower_one, LowerOne.lower_one;fwt]|intro mi;apply okF_miter;intro id;unfold lower_one, LowerOne.lower_one;fwt]. }
 exact(proj2 Fold b s value final InvS InfoS FwdS Load Run).
Qed.

Lemma fh_keeps:forall A(m:M A),okI m ->keeps st_fns m ->FH m.
Proof.
 intros A m OK Keep;split;[exact OK|].
 intros b s a final InvS InfoS FwdS Load Run;apply fn_history_same;apply(Keep s a final Run).
Qed.
Lemma fh_mmap:forall A B(f:A->M B)xs,(forall x,FH(f x))->(forall x,okF(f x))->FH(mmap f xs).
Proof.
 intros A B f xs F Fwd;induction xs as[|x xs IH];cbn[mmap];[apply fh_ret|].
 apply fh_bind;[apply F|apply Fwd|intro a;apply fh_bind;[exact IH|apply okF_mmap;exact Fwd|intro;apply fh_ret]].
Qed.
Lemma fh_mfold:forall A B(f:A->B->M A)xs init,
 (forall a x,FH(f a x))->(forall a x,okF(f a x))->FH(mfold f init xs).
Proof.
 intros A B f xs;induction xs as[|x xs IH];intros init F Fwd;cbn[mfold];[apply fh_ret|].
 apply fh_bind;[apply F|apply Fwd|intro a;apply IH;assumption].
Qed.
Lemma fh_input_ety_ty:forall t,FH(ety_ty t).
Proof.
 intro t;apply fh_keeps.
 - unfold ety_ty;apply okI_ni;[apply ok_ety_ty_go|apply ni_ety_ty_go|keepst].
 - unfold ety_ty;apply kf_ety_ty_go.
Qed.

Lemma kf_entry_zero_width:forall fuel e t,keeps st_fns(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kf_entry_zero_width:keepsdb.
Lemma kf_check_main_signature:forall g info,keeps st_fns(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kf_check_main_signature:keepsdb.

Ltac history_fwd:=repeat(fintros;first[
 lazymatch goal with
 | |-okF lower_all=>apply okF_lower_all
 | |-okF(mbind _ _)=>apply okF_bind
 | |-okF(mmap _ _)=>apply okF_mmap
 | |-okF(miter _ _)=>apply okF_miter
 | |-okF(mfold _ _ _)=>apply okF_mfold
 | |-okF ?m=>first[
   solve[apply okF_readonly;[let O:=fresh "OI" in assert(O:okI m)by program_info;exact(proj1 O)|keepst]]|
   match m with context[match ?x with _=>_ end]=>destruct x end|
   let h:=head_of m in progress unfold h]
 end]).
Ltac fht:=repeat(fintros;first[
 lazymatch goal with
 | |-okF _=>solve[history_fwd]
 | |-FH lower_all=>apply fh_lower_all
 | |-FH(ety_ty _)=>apply fh_input_ety_ty
 | |-FH(mbind _ _)=>apply fh_bind
 | |-FH(mmap _ _)=>apply fh_mmap
 | |-FH(miter _ _)=>apply fh_miter
 | |-FH(mfold _ _ _)=>apply fh_mfold
 | |-FH ?m=>first[solve[apply fh_keeps;[program_info|keepst]]|match m with context[match ?x with _=>_ end]=>destruct x end|
   let h:=head_of m in progress unfold h]
 end]).
Lemma fh_elab_program:FH elab_program.
Proof.
 unfold elab_program.
 apply fh_bind;[apply fh_keeps;[program_info|keepst]| |intro u1].
 1:history_fwd.
 apply fh_bind;[apply fh_keeps;[program_info|keepst]| |intro u2].
 1:history_fwd.
 apply fh_bind;[apply fh_keeps;[program_info|keepst]| |intro u3].
 1:history_fwd.
 apply fh_bind;[apply fh_lower_all|apply okF_lower_all|intro u4].
 change (FH program_tail).
 apply fh_keeps;[exact program_tail_info|exact program_tail_functions].
Qed.

Definition ProgramTables(m:M xprogram):Prop:=forall s p final,
 m s=Good(p,final)->ExecutionModel.xo_fns p=st_fns final.
Arguments ProgramTables:simpl never.
Lemma tables_bind:forall A(m:M A)(k:A->M xprogram),
 (forall a,ProgramTables(k a))->ProgramTables(mbind m k).
Proof.
 intros A m k K s p final Run;apply bind_inv in Run as[a[mid[First Last]]];exact(K a mid p final Last).
Qed.
Lemma tables_snapshot:forall A(m:st->M A)(make:st->A->xprogram),
 (forall s,keeps st_fns(m s))->
 (forall s a,ExecutionModel.xo_fns(make s a)=st_fns s)->
 ProgramTables(mbind get(fun s=>mbind(m s)(fun a=>ret(make s a)))).
Proof.
 intros A m make Keep Shape s p final Run.
 apply bind_inv in Run as[snapshot[mid[Read Run]]];injection Read as <- <-.
 apply bind_inv in Run as[a[next[Body Run]]];injection Run as <- <-.
 rewrite Shape; symmetry;exact(Keep s s a next Body).
Qed.
Ltac tables_t:=repeat(fintros;first[
 solve[eapply tables_snapshot;[intro;keepst|intros;reflexivity]]|
 lazymatch goal with
 | |-ProgramTables(fun _=>Bad _)=>unfold ProgramTables;intros;discriminate
 | |-ProgramTables(mbind _ _)=>apply tables_bind
 | |-ProgramTables ?m=>first[
    match m with context[match ?x with _=>_ end]=>destruct x end|
    let h:=head_of m in progress unfold h]
 end]).
Lemma elab_program_tables:ProgramTables elab_program.
Proof. unfold elab_program;tables_t. Qed.

Theorem elab_completed_source_bodies:forall b w p final,
 load_world b=Good w ->Inv(s_init w)->elab_program(s_init w)=Good(p,final)->
 forall f body,loaded_fn_key w f ->nassoc f(ExecutionModel.xo_fns p)=Some body ->
 completed_source_fn w final f body.
Proof.
 intros b w p final Load InvS Run f body Loaded Lookup.
 pose proof(proj2 fh_elab_program b(s_init w)p final InvS(info_init w)(@fwd_inv_init w)Load Run)as Hist.
 eapply(@fn_history_from_empty w(s_init w)final);[reflexivity|exact Hist|exact Loaded|].
 rewrite <-(@elab_program_tables(s_init w)p final Run);exact Lookup.
Qed.

Definition Emits(key:nat)(m:M unit):Prop:=forall s final,
 m s=Good(tt,final)->exists body,nassoc key(st_fns final)=Some body.
Arguments Emits:simpl never.
Lemma emits_bind:forall A key(m:M A)(k:A->M unit),
 (forall a,Emits key(k a))->Emits key(mbind m k).
Proof. intros A key m k K s final Run;apply bind_inv in Run as[a[mid[First Last]]];exact(K a mid final Last). Qed.
Lemma emits_emit:forall key body,Emits key(emit_fn key body).
Proof.
 intros key body s final Run;unfold emit_fn,upd in Run;injection Run as <-.
 exists body;cbn[s_fns st_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
Qed.
Ltac emits_t:=repeat(fintros;first[solve[apply emits_emit]|
 lazymatch goal with
 | |-Emits _(fun _=>Bad _)=>unfold Emits;intros;discriminate
 | |-Emits _(mbind _ _)=>apply emits_bind
 | |-Emits _ ?m=>first[match m with context[match ?x with _=>_ end]=>destruct x end|let h:=head_of m in progress unfold h]
 end]).
Lemma lower_body_emits:forall fuel g s final,
 fn_body g<>None ->lower_body fuel g s=Good(tt,final)->
 exists body,nassoc(gs_id g)(st_fns final)=Some body.
Proof.
 intros[|fuel]g s final HasBody Run;[discriminate|].
 cbn[lower_body]in Run;apply bind_inv in Run as[done[mid[Read Run]]].
 unfold lowered in Read;unfold mbind,get,ret in Read;injection Read as <- <-.
 destruct(nassoc(gs_id g)(st_fns s))as[old|]eqn:Before;cbn[is_some]in Run.
 - injection Run as <-;exists old;exact Before.
 - destruct(fn_body g)as[body|]eqn:Body;[|contradiction].
   match type of Run with ?m s=Good(tt,final)=>
     assert(Ends:Emits(gs_id g)m)by emits_t;exact(Ends s final Run)end.
Qed.
