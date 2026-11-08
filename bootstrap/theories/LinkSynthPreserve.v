(* The actual mutual traversal preserves authenticated link history. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv ReifyData
 InfoInvariant InfoPreserve InfoInputs FwdInvariant FwdReadonly FwdPreserve FwdSynthInv
 LinkCacheInvariant LinkReadonly LinkPreserve LinkConditional LinkBindPreserve
 ProviderPhysical WrapperKeyReadonly LinkHigherPreserve ReadonlyHelpers.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go].
Lemma okL_deep : forall A,@okL A deep.
Proof.
 intro A;split;[unfold deep;apply okI_fail|];split.
 - split;[unfold deep;apply ok_fail|intros;discriminate].
 - intros;discriminate.
Qed.
Lemma link_single_physical_stable : forall g,LinkStable(fun s=>PhysicalProvider s g).
Proof. intros g s final Frame Physical;exact(fr_keep s final Frame _ _ Physical). Qed.
Lemma okLP_bind_functor : forall fuel c e fg y,BindFunctorPreserves fuel c e fg y ->
 okLP(fun s=>PhysicalProvider s fg)(bind_functor fuel c e fg y).
Proof.
 intros fuel c e fg y Linked;split;[apply okI_bind_functor|];split;[apply okF_bind_functor|].
 intros;eapply Linked;eassumption.
Qed.
Lemma okL_sym_bind : forall A id(k:gsym->M A),
 (forall g,okLP(fun s=>PhysicalProvider s g)(k g))->okL(mbind(sym id)k).
Proof.
 intros A id k K;split.
 - apply okI_bind;[apply okI_sym|intro;exact(proj1(K a))].
 - split;[apply okF_bind;[apply okF_readonly;[solve[okt]|unfold sym;keepst]|intro;exact(proj1(proj2(K a)))]|].
   intros s a final Valid Cached Forwarded Links Run.
   apply bind_inv in Run as[g[mid[Sym Tail]]].
   destruct(sym_physical_provider _ _ _ _ Sym Valid)as[-> Physical].
   exact(proj2(proj2(K g))s a final Valid Cached Forwarded Links Physical Tail).
Qed.
Local Lemma kws_ety_ty : forall t,keeps st_fwd(ety_ty t).
Proof. intro t;unfold ety_ty;apply kw_ety_ty_go. Qed.
Local Lemma kls_ety_ty : forall t,keeps st_links(ety_ty t).
Proof. intro t;unfold ety_ty;apply kl_ety_ty_go. Qed.
#[local] Hint Resolve kws_ety_ty kls_ety_ty:keepsdb.
Local Lemma kws_key_theta : forall e k,keeps st_fwd(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
Local Lemma kls_key_theta : forall e k,keeps st_links(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
#[local] Hint Resolve kws_key_theta kls_key_theta:keepsdb.
Local Lemma kws_key_item : forall e k,keeps st_fwd(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
Local Lemma kls_key_item : forall e k,keeps st_links(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
#[local] Hint Resolve kws_key_item kls_key_item:keepsdb.
Lemma okL_readonly : forall A(m:M A),keeps readonly_state m->NI m->keeps st_links m->ok m->okL m.
Proof.
 intros A m Read Info Links Valid;apply okL_keeps.
 - apply okI_ni;[exact Valid|exact Info|].
   eapply keeps_projection with(p:=readonly_state)(q:=fun x=>snd(fst(fst x)));exact Read.
 - apply okF_readonly;[exact Valid|].
   eapply keeps_projection with(p:=readonly_state)(q:=fun x=>snd(fst(fst(fst x))));exact Read.
 - exact Links.
Qed.
Local Ltac synth_link_quiet := first[solve[apply okL_readonly;[auto with keepsdb|auto with nidb|auto with keepsdb|auto with okdb]]|apply okL_keeps;
 [first[solve[auto with okIdb]|apply okI_ni;[solve[okt]|solve[nit]|solve[keepst]]]|
  first[solve[auto with fwdb]|apply okF_readonly;[solve[okt]|solve[keepst]]]|solve[keepst]]].
Local Lemma cached_link_get : okL get.
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_show : forall t,okL(show t).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_show_key : forall k,okL(show_key k).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fresh : okL fresh.
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_local_var : forall name,okL(local_var name).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fld_id : forall name,okL(fld_id name).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_unsupported : forall why,okL(unsupported why).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fn_info : forall g,okL(fn_info g).
Proof. intros;synth_link_quiet. Qed.
Local Ltac cached_link_quiet := lazymatch goal with|-okL ?m=>let h:=head_of m in lazymatch h with
| get => apply cached_link_get
| show => apply cached_link_show
| show_key => apply cached_link_show_key
| fresh => apply cached_link_fresh
| local_var => apply cached_link_local_var
| fld_id => apply cached_link_fld_id
| unsupported => apply cached_link_unsupported
| fn_info => apply cached_link_fn_info
end end.
Local Ltac link_synth_tac previous Bind := repeat(fintros;first[
 solve[previous ()]|solve[cached_link_quiet]|
 lazymatch goal with
 | |-okL(ret _)=>apply okL_ret
 | |-okL(mbind(sym _)_)=>apply okL_sym_bind
 | |-okL(mbind _ _)=>apply okL_bind
 | |-okL(mmap _ _)=>apply okL_mmap
 | |-okL(miter _ _)=>apply okL_miter
 | |-okL(mfold _ _ _)=>apply okL_mfold
 | |-okL(with_vars _ _)=>apply okL_with_vars
 | |-okL(in_reify_code _ _)=>apply okL_in_reify_code
 | |-okLP _ (bind_functor _ _ _ _ _)=>apply okLP_bind_functor;apply Bind
 | |-okLP _ (mbind _ _)=>apply okLP_bind;[apply link_single_physical_stable| |]
 | |-okLP _ (mmap _ _)=>apply okLP_mmap;[apply link_single_physical_stable|]
 | |-okLP _ (miter _ _)=>apply okLP_miter;[apply link_single_physical_stable|]
 | |-okLP _ (mfold _ _ _)=>apply okLP_mfold;[apply link_single_physical_stable|]
 | |-okLP _ (match ?x with _=>_ end)=>destruct x
 | |-okLP _ ?m=>apply okLP_of_okL
 | |-okL(match ?x with _=>_ end)=>destruct x
 | |-okL ?m=>first[solve[synth_link_quiet]|
   match m with context[match ?x with _=>_ end]=>destruct x end|
   let h:=head_of m in progress unfold h]
 end]).

Lemma okL_synth_group : forall fuel,
  (forall c e x exp, okL (synth fuel c e x exp)) /\
  (forall c e a exp, okL (synth_arg fuel c e a exp)) /\
  (forall c e b exp, okL (synth_block fuel c e b exp)) /\
  (forall c e ss, okL (check_stmts fuel c e ss)) /\
  (forall c e s, okL (check_stmt fuel c e s)) /\
  (forall c e y, okL (check_bool fuel c e y)) /\
  (forall c e x exp, okL (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, okL (synth_match fuel c e s arms exp)) /\
  (forall g, okL(const_val fuel g)) /\
  (forall c e p a, okL(synth_path fuel c e p a)) /\
  (forall c e nm sg args, okL (check_args fuel c e nm sg args)) /\
  (forall c e p a args, okL (synth_call fuel c e p a args)) /\
  (forall c e p a fs, okL (synth_record fuel c e p a fs)) /\
  (forall c e o n, okL (synth_field fuel c e o n)) /\
  (forall c e o p args, okL (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, okL (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, okL (apply_family fuel c e fg th want)) /\
  (forall c e info, okL (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, okL (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, okL (lang_call fuel c e o n args why)) /\
  (forall c e ds why, okL (int_literal fuel c e ds why)) /\
  (forall c e v why, okL (float_of fuel c e v why)) /\
  (forall c e text, okL (synth_literal fuel c e text)) /\
  (forall c e sp y, okL (check_bind fuel c e sp y)) /\
  (forall c e y, okL (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, okL (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, okL (wrapper fuel pv)) /\
  (forall tg, okL (forwarder fuel tg)) /\
  (forall g, okL(lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, okL (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, okL (apply_functor fuel c e sp)) /\
  (forall c e g, okL (apply_body fuel c e g)) /\
  (forall c e fg, okL (apply_abstract fuel c e fg)) /\
  (forall c e fg y, BindFunctorPreserves fuel c e fg y) /\
  (forall fg bg, LinkBodyPreserves fuel fg bg).
Proof.
 induction fuel as[|fuel IH].
 {
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
 all: fintros.
 all: try change(okL deep).
 all: try apply okL_deep.
 all: try solve[unfold BindFunctorPreserves;intros;discriminate|unfold LinkBodyPreserves;intros;discriminate].
 }
 destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
 all: fintros.
 all: let previous _ := lazymatch goal with |-okL ?m=>let h:=head_of m in lazymatch h with
 | synth =>apply I0
 | synth_arg =>apply I1
 | synth_block =>apply I2
 | check_stmts =>apply I3
 | check_stmt =>apply I4
 | check_bool =>apply I5
 | synth_if =>apply I6
 | synth_match =>apply I7
 | const_val =>apply I8
 | synth_path =>apply I9
 | check_args =>apply I10
 | synth_call =>apply I11
 | synth_record =>apply I12
 | synth_field =>apply I13
 | synth_method =>apply I14
 | provided_call =>apply I15
 | apply_family =>apply I16
 | demand_needs =>apply I17
 | synth_operator =>apply I18
 | lang_call =>apply I19
 | int_literal =>apply I20
 | float_of =>apply I21
 | synth_literal =>apply I22
 | check_bind =>apply I23
 | expect_fn_provider =>apply I24
 | match_fn_sig =>apply I25
 | wrapper =>apply I26
 | forwarder =>apply I27
 | lower_body =>apply I28
 | lambda_provider =>apply I29
 | apply_functor =>apply I30
 | apply_body =>apply I31
 | apply_abstract =>apply I32
 end end in
 solve[apply okL_wrapper|apply okL_forwarder_step;exact I28|apply okL_lambda_provider_step;exact I2|apply bind_functor_preserves_step;exact I34|lazymatch goal with |-LinkBodyPreserves _ ?fg ?bg=>exact(link_body_preserves_step fuel I31 fg bg)end|fintros;cbn[synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body];link_synth_tac previous I33].
Qed.

Create HintDb linkdb.
Lemma okL_synth : forall fuel, forall c e x exp, okL (synth fuel c e x exp).
Proof. intro fuel;exact (proj1 (okL_synth_group fuel)). Qed.
#[export] Hint Resolve okL_synth : linkdb.
Lemma okL_synth_arg : forall fuel, forall c e a exp, okL (synth_arg fuel c e a exp).
Proof. intro fuel;exact (proj1 (proj2 (okL_synth_group fuel))). Qed.
#[export] Hint Resolve okL_synth_arg : linkdb.
Lemma okL_synth_block : forall fuel, forall c e b exp, okL (synth_block fuel c e b exp).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (okL_synth_group fuel)))). Qed.
#[export] Hint Resolve okL_synth_block : linkdb.
Lemma okL_check_stmts : forall fuel, forall c e ss, okL (check_stmts fuel c e ss).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (okL_synth_group fuel))))). Qed.
#[export] Hint Resolve okL_check_stmts : linkdb.
Lemma okL_check_stmt : forall fuel, forall c e s, okL (check_stmt fuel c e s).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))). Qed.
#[export] Hint Resolve okL_check_stmt : linkdb.
Lemma okL_check_bool : forall fuel, forall c e y, okL (check_bool fuel c e y).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))). Qed.
#[export] Hint Resolve okL_check_bool : linkdb.
Lemma okL_synth_if : forall fuel, forall c e x exp, okL (synth_if fuel c e x exp).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))). Qed.
#[export] Hint Resolve okL_synth_if : linkdb.
Lemma okL_synth_match : forall fuel, forall c e s arms exp, okL (synth_match fuel c e s arms exp).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))). Qed.
#[export] Hint Resolve okL_synth_match : linkdb.
Lemma okL_const_val : forall fuel, forall g, okL(const_val fuel g).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))). Qed.
#[export] Hint Resolve okL_const_val : linkdb.
Lemma okL_synth_path : forall fuel, forall c e p a, okL(synth_path fuel c e p a).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))). Qed.
#[export] Hint Resolve okL_synth_path : linkdb.
Lemma okL_check_args : forall fuel, forall c e nm sg args, okL (check_args fuel c e nm sg args).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))). Qed.
#[export] Hint Resolve okL_check_args : linkdb.
Lemma okL_synth_call : forall fuel, forall c e p a args, okL (synth_call fuel c e p a args).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))). Qed.
#[export] Hint Resolve okL_synth_call : linkdb.
Lemma okL_synth_record : forall fuel, forall c e p a fs, okL (synth_record fuel c e p a fs).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))). Qed.
#[export] Hint Resolve okL_synth_record : linkdb.
Lemma okL_synth_field : forall fuel, forall c e o n, okL (synth_field fuel c e o n).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))). Qed.
#[export] Hint Resolve okL_synth_field : linkdb.
Lemma okL_synth_method : forall fuel, forall c e o p args, okL (synth_method fuel c e o p args).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))). Qed.
#[export] Hint Resolve okL_synth_method : linkdb.
Lemma okL_provided_call : forall fuel, forall c e ox oty m args, okL (provided_call fuel c e ox oty m args).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))). Qed.
#[export] Hint Resolve okL_provided_call : linkdb.
Lemma okL_apply_family : forall fuel, forall c e fg th want, okL (apply_family fuel c e fg th want).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))). Qed.
#[export] Hint Resolve okL_apply_family : linkdb.
Lemma okL_demand_needs : forall fuel, forall c e info, okL (demand_needs fuel c e info).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))). Qed.
#[export] Hint Resolve okL_demand_needs : linkdb.
Lemma okL_synth_operator : forall fuel, forall c e lm n op operand args, okL (synth_operator fuel c e lm n op operand args).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))))). Qed.
#[export] Hint Resolve okL_synth_operator : linkdb.
Lemma okL_lang_call : forall fuel, forall c e o n args why, okL (lang_call fuel c e o n args why).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_lang_call : linkdb.
Lemma okL_int_literal : forall fuel, forall c e ds why, okL (int_literal fuel c e ds why).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_int_literal : linkdb.
Lemma okL_float_of : forall fuel, forall c e v why, okL (float_of fuel c e v why).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_float_of : linkdb.
Lemma okL_synth_literal : forall fuel, forall c e text, okL (synth_literal fuel c e text).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_synth_literal : linkdb.
Lemma okL_check_bind : forall fuel, forall c e sp y, okL (check_bind fuel c e sp y).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_check_bind : linkdb.
Lemma okL_expect_fn_provider : forall fuel, forall c e y, okL (expect_fn_provider fuel c e y).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_expect_fn_provider : linkdb.
Lemma okL_match_fn_sig : forall fuel, forall c e pv wanted kind, okL (match_fn_sig fuel c e pv wanted kind).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_match_fn_sig : linkdb.
Lemma okL_forwarder : forall fuel, forall tg, okL (forwarder fuel tg).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_forwarder : linkdb.
Lemma okL_lower_body : forall fuel, forall g, okL(lower_body fuel g).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_lower_body : linkdb.
Lemma okL_lambda_provider : forall fuel, forall c e ps rt body bound sg this what, okL (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_lambda_provider : linkdb.
Lemma okL_apply_functor : forall fuel, forall c e sp, okL (apply_functor fuel c e sp).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_apply_functor : linkdb.
Lemma okL_apply_body : forall fuel, forall c e g, okL (apply_body fuel c e g).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel))))))))))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_apply_body : linkdb.
Lemma okL_apply_abstract : forall fuel, forall c e fg, okL (apply_abstract fuel c e fg).
Proof. intro fuel;exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okL_synth_group fuel)))))))))))))))))))))))))))))))))). Qed.
#[export] Hint Resolve okL_apply_abstract : linkdb.
