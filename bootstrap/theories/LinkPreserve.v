(* Preservation of physical link history by actual link-body creation. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ReqsSolve ElabInv ReifyData
 InfoInvariant InfoPreserve InfoInputs FwdInvariant FwdPreserve FwdSynthInv
 LinkCacheInvariant LinkReadonly LinkBodyTrace WrapperKeyReadonly FwdReadonly.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go].
Local Lemma kw_link_ety_ty : forall t,keeps st_fwd(ety_ty t).
Proof. intro t;unfold ety_ty;apply kw_ety_ty_go. Qed.
Local Lemma kl_link_ety_ty : forall t,keeps st_links(ety_ty t).
Proof. intro t;unfold ety_ty;apply kl_ety_ty_go. Qed.
#[local] Hint Resolve kw_link_ety_ty kl_link_ety_ty : keepsdb.
Local Lemma kw_link_key_theta : forall e k,keeps st_fwd(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
Local Lemma kl_link_key_theta : forall e k,keeps st_links(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
#[local] Hint Resolve kw_link_key_theta kl_link_key_theta : keepsdb.
Local Lemma kw_link_key_item : forall e k,keeps st_fwd(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
Local Lemma kl_link_key_item : forall e k,keeps st_links(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
#[local] Hint Resolve kw_link_key_item kl_link_key_item : keepsdb.

Lemma okL_run : forall A(m:M A)s a final,
 okL m -> Inv s -> InfoInv s -> FwdInv s -> LinkInv s -> m s=Good(a,final) ->
 Inv final /\ InfoInv final /\ FwdInv final /\ LinkInv final /\ Fr s final.
Proof.
 intros A m s a final [Info[Forward Linked]] Valid Cached Forwarded Links Run.
 destruct(proj1 Info _ _ _ Valid Run)as[Last Frame].
 split;[exact Last|];split;[exact(proj2 Info _ _ _ Valid Cached Run)|].
 split;[exact(proj2 Forward _ _ _ Valid Forwarded Run)|].
 split;[exact(Linked _ _ _ Valid Cached Forwarded Links Run)|exact Frame].
Qed.
Local Ltac link_quiet := apply okL_keeps;
 [apply okI_ni;[solve[okt]|solve[nit]|solve[keepst]]|
  apply okF_readonly;[solve[okt]|solve[keepst]]|solve[keepst]].
Lemma okL_in_reify_code : forall A owner(m:M A),okL m -> okL(in_reify_code owner m).
Proof.
 intros A owner m Body;unfold in_reify_code.
 apply okL_bind;[link_quiet|intro saved;apply okL_bind;[link_quiet|intro]].
 apply okL_bind;[exact Body|intro got;apply okL_bind;[link_quiet|intro;apply okL_ret]].
Qed.

Local Ltac advance Run Certificate :=
 lazymatch type of Run with ?m ?s=Good(?a,?final) =>
 let Valid:=fresh "Valid" in let Cached:=fresh "Cached" in
 let Forwarded:=fresh "Forwarded" in let Links:=fresh "Links" in let Frame:=fresh "Frame" in
 destruct(@okL_run _ m s a final Certificate ltac:(assumption) ltac:(assumption)
 ltac:(assumption) ltac:(assumption) Run)as[Valid[Cached[Forwarded[Links Frame]]]] end.
Local Ltac advance_quiet Run :=
 lazymatch type of Run with ?m ?s=Good(?a,?final) =>
 let Cert:=fresh "Quiet" in assert(Cert:okL m)by link_quiet;advance Run Cert end.

Lemma link_finish_preserves : forall fuel fg bg before ef promised eb rts dr roles binds s h final,
 Inv before -> InfoInv before -> FwdInv before ->
 nget(st_syms before)(gs_id fg)=Some fg -> nget(st_syms before)(gs_id bg)=Some bg ->
 link_lookup before(gs_id fg)(gs_id bg)=None ->
 link_body(S fuel)fg bg before=Good(h,final) ->
 Inv s -> InfoInv s -> FwdInv s -> LinkInv s ->
 link_trace_finish fg bg ef promised eb rts dr roles binds s=Good(h,final) ->LinkInv final.
Proof.
 intros fuel fg bg before ef promised eb rts dr roles binds s h final
 ValidBefore InfoBefore FwdBefore Target Provider Miss Whole Valid Info Forward Links Finish.
 destruct(link_body_fresh_emission _ _ _ _ _ _ _ _ _ _ _ _ Finish)as
 [exported[allocated[dri[indexed[Exports[Allocate[Index[Final[Rows Cache]]]]]]]]].
 advance_quiet Exports.
 advance_quiet Allocate.
 advance_quiet Index.
 assert(Updates:(upd(s_funs(fun l=>(h,(map ITy rts++dri,binds))::l));;
   upd(s_links(fun l=>((gs_id fg,gs_id bg),h)::l))) indexed=Good(tt,final)).
 {rewrite Final;reflexivity. }
 assert(UpdateOK:ok(upd(s_funs(fun l=>(h,(map ITy rts++dri,binds))::l));;
   upd(s_links(fun l=>((gs_id fg,gs_id bg),h)::l))))by okt.
 destruct(UpdateOK indexed tt final ltac:(assumption) Updates)as[_ FinalFrame].
 apply(link_inv_write indexed final(gs_id fg)(gs_id bg)h);
 [assumption|apply link_frame_of_fr;exact FinalFrame|exact Cache|].
 exact(source_link_entry fuel fg bg before h final ValidBefore InfoBefore FwdBefore Target Provider Miss Whole).
Qed.

Theorem link_body_preserves_step : forall fuel,
 (forall c e g,okL(apply_body fuel c e g)) ->
 forall fg bg s h final,
 Inv s -> InfoInv s -> FwdInv s -> LinkInv s ->
 nget(st_syms s)(gs_id fg)=Some fg -> nget(st_syms s)(gs_id bg)=Some bg ->
 link_body(S fuel)fg bg s=Good(h,final) ->LinkInv final.
Proof.
 intros fuel Bodies fg bg s h final Valid Info Forward Links Target Provider Run.
 destruct(link_lookup s(gs_id fg)(gs_id bg))as[cached|]eqn:Hit.
 {cbn[link_body]in Run;apply bind_inv in Run as[initial[s0[Get Run]]];injection Get as <- <-.
  unfold link_lookup in Hit.
  rewrite Hit in Run;injection Run as <- <-;exact Links. }
 pose proof Run as Whole.
 destruct(link_body_fresh_inputs _ _ _ _ _ _ Hit Run)as
 [ef[fneeds[sf[eg[gneeds[sg[xf[sx[rts[sr[sc[EF[EG[Fixed[Types[Capture After]]]]]]]]]]]]]]]].
 advance_quiet EF.
 advance_quiet EG.
 advance_quiet Fixed.
 advance_quiet Types.
 advance_quiet Capture.
 destruct(link_body_fresh_body _ _ _ _ _ _ _ _ _ _ _ After)as
 [promised[dr[roles[sv[registered[xs[eb[cb[bodyFinal[View[Register[Body Tail]]]]]]]]]]]].
 advance_quiet View.
 advance_quiet Register.
 assert(BodyCert:okL(in_reify_code(ROLink(gs_id fg)(gs_id bg))
  (with_vars[](apply_body fuel(link_trace_context bg promised xf rts dr roles)eg bg)))).
 {apply okL_in_reify_code;apply okL_with_vars;apply Bodies. }
 advance Body BodyCert.
 destruct(link_body_fresh_checks fg bg ef eg promised eb xf rts dr roles xs bodyFinal h final Tail)as
 [raw[sb[se[renames[sn[st[si[ss[Raw[Effects[Rename[TypeGuard[ItemGuard[Readings Finish]]]]]]]]]]]]]].
 advance_quiet Raw.
 advance_quiet Effects.
 advance_quiet Rename.
 advance_quiet TypeGuard.
 advance_quiet ItemGuard.
 advance_quiet Readings.
 eapply link_finish_preserves;[exact Valid|exact Info|exact Forward|exact Target|exact Provider|
 exact Hit|exact Whole|eassumption|eassumption|eassumption|eassumption|exact Finish].
Qed.

Theorem platform_fun_preserves : forall fg s h final,
 Inv s -> InfoInv s -> FwdInv s -> LinkInv s ->
 nget(st_syms s)(gs_id fg)=Some fg ->
 platform_fun fg s=Good(h,final) ->LinkInv final.
Proof.
 intros fg s h final Valid Info Forward Links Target Whole.
 pose proof Whole as Run;unfold platform_fun in Run.
 apply bind_inv in Run as[initial[s0[Get Run]]];injection Get as <- <-.
 destruct(link_lookup s(gs_id fg)(gs_id fg))as[cached|]eqn:Hit.
 {unfold link_lookup in Hit;rewrite Hit in Run;injection Run as <- <-;exact Links. }
 unfold link_lookup in Hit;rewrite Hit in Run.
 apply bind_inv in Run as[[ef needs][sf[EF Run]]];advance_quiet EF.
 apply bind_inv in Run as[rts[sr[Types Run]]];advance_quiet Types.
 apply bind_inv in Run as[[promised results][sp[Provide Run]]];advance_quiet Provide.
 apply bind_inv in Run as[wm[sw[Module Run]]];advance_quiet Module.
 destruct(functor_parts fg)as[[args output]body]eqn:Parts.
 apply bind_inv in Run as[items[si[Items Run]]];advance_quiet Items.
 apply bind_inv in Run as[code[allocated[Allocate Run]]];advance_quiet Allocate.
 apply bind_inv in Run as[[][registered[Register Run]]];advance_quiet Register.
 apply bind_inv in Run as[[][exported[Exports Run]]];advance_quiet Exports.
 pose proof Run as Updates.
 apply bind_inv in Run as[[][emitted[Emit Run]]].
 unfold upd in Emit;injection Emit as <-.
 apply bind_inv in Run as[[][cached[Install Run]]].
 unfold upd in Install;injection Install as <-.
 injection Run as <- <-.
 assert(UpdateOK:ok(upd(s_funs(fun l=>(code,(map fst items,List.concat(map snd items)))::l));;
   upd(s_links(fun l=>((gs_id fg,gs_id fg),code)::l));;ret code))by okt.
 destruct(UpdateOK exported code _ ltac:(assumption) Updates)as[_ FinalFrame].
 apply(link_inv_write exported _ (gs_id fg)(gs_id fg)code);
 [assumption|apply link_frame_of_fr;exact FinalFrame|reflexivity|].
 exact(platform_link_entry fg s code _ Valid Info Forward Target Hit Whole).
Qed.
