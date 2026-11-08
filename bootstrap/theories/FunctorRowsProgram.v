(* Final functor rows are derived from the executable whole-program pipeline. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv FinalLinks FunctorReadonly FunctorRows FunctorRowsEmit FunctorTraversal.
From NM Require Import ProgramTail ProgramInfo MainProgramTrace.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma ft_synth_group : forall fuel,
  (forall c e x exp, FT (synth fuel c e x exp)) /\
  (forall c e a exp, FT (synth_arg fuel c e a exp)) /\
  (forall c e b exp, FT (synth_block fuel c e b exp)) /\
  (forall c e ss, FT (check_stmts fuel c e ss)) /\
  (forall c e s, FT (check_stmt fuel c e s)) /\
  (forall c e y, FT (check_bool fuel c e y)) /\
  (forall c e x exp, FT (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, FT (synth_match fuel c e s arms exp)) /\
  (forall g, FT (const_val fuel g)) /\
  (forall c e p a, FT (synth_path fuel c e p a)) /\
  (forall c e nm sg args, FT (check_args fuel c e nm sg args)) /\
  (forall c e p a args, FT (synth_call fuel c e p a args)) /\
  (forall c e p a fs, FT (synth_record fuel c e p a fs)) /\
  (forall c e o n, FT (synth_field fuel c e o n)) /\
  (forall c e o p args, FT (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, FT (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, FT (apply_family fuel c e fg th want)) /\
  (forall c e info, FT (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, FT (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, FT (lang_call fuel c e o n args why)) /\
  (forall c e ds why, FT (int_literal fuel c e ds why)) /\
  (forall c e v why, FT (float_of fuel c e v why)) /\
  (forall c e text, FT (synth_literal fuel c e text)) /\
  (forall c e sp y, FT (check_bind fuel c e sp y)) /\
  (forall c e y, FT (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, FT (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, FT (wrapper fuel pv)) /\
  (forall tg, FT(forwarder fuel tg)) /\
  (forall g, FT(lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, FT (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, FT (apply_functor fuel c e sp)) /\
  (forall c e g, FT (apply_body fuel c e g)) /\
  (forall c e fg, FT (apply_abstract fuel c e fg)) /\
  (forall c e fg y, FT (bind_functor fuel c e fg y)) /\
  (forall fg bg, FT (link_body fuel fg bg)).
Proof.
 exact(@FunctorTraversal.synth_preservation_group (@FT) ft_ret ft_bind ft_mmap ft_mfold ft_miter ft_with_vars ft_keeps ft_link_body_step).
Qed.
Lemma ft_lower_body:forall fuel g,FT(lower_body fuel g).
Proof. intros;apply ft_synth_group. Qed.
Lemma ft_lower_all:FT lower_all.
Proof.
 unfold lower_all;apply ft_bind;[ft_leaf|intro s].
 apply ft_miter;intro mi;apply ft_miter;intro id.
 apply ft_bind;[ft_leaf|intro g].
 destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));
 [apply ft_lower_body|apply ft_ret].
Qed.
Local Lemma kfun_entry_zero_width:forall fuel e t,keeps st_funs(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kfun_entry_zero_width:keepsdb.
Local Lemma kfun_main_signature:forall g info,keeps st_funs(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kfun_main_signature:keepsdb.
Lemma ft_program_tail:FT program_tail.
Proof. unfold program_tail;ftt. all:apply ft_platform_fun. Qed.
Lemma ft_elab_program:FT elab_program.
Proof.
 rewrite program_split.
 apply ft_bind;[apply ft_keeps;[unfold check_listed;keepst|exact(proj1 program_info_listed)]|intro].
 apply ft_bind;[apply ft_keeps;[unfold check_contexts;keepst|exact(proj1 program_info_contexts)]|intro].
 apply ft_bind;[apply ft_keeps;[unfold check_runtime_library;keepst|exact(proj1 program_info_runtime)]|intro].
 apply ft_bind;[apply ft_lower_all|intro;apply ft_program_tail].
Qed.
Lemma main_runtime_types_functors:forall e sc base,keeps st_funs(main_runtime_types e sc base).
Proof. intros;unfold main_runtime_types;keepst. Qed.
Lemma elab_program_functors:forall initial P final,
 elab_program initial=Good(P,final)->xo_funs P=st_funs final.
Proof.
 intros initial P final Run;apply main_program_trace in Run.
 destruct Run as(listed&contexts&library&lowered&assumes&mainid&main&info&infos&signature&platform&prims&supplied&sc&inputstate&inputs&types&Listed&Contexts&Library&Lowered&Assumes&Mainid&Main&Defined&Info&Signature&Platform&Unsupported&Runtime&Inputs&Types&Program).
 destruct(runtime_bool_tuple sc)as[[boolid trueid]falseid];subst P;cbn[xo_funs].
 symmetry;eapply main_runtime_types_functors;exact Types.
Qed.
Theorem elab_program_final_links:forall initial P final,
 Inv initial ->FunctorRows initial ->FunctorBound initial ->
 elab_program initial=Good(P,final)->FinalLinks P final.
Proof.
 intros initial P final I R B Run.
 apply final_links_table;[exact(elab_program_functors _ _ _ Run)|].
 exact(proj1(proj2 ft_elab_program _ _ _ I R B Run)).
Qed.
Theorem elab_program_historical_links:forall initial P final earlier,
 Inv initial ->FunctorRows initial ->FunctorBound initial ->
 elab_program initial=Good(P,final)->Fr earlier final ->FinalLinks P earlier.
Proof. intros;apply(final_links_back P earlier final);[assumption|eapply elab_program_final_links;eassumption]. Qed.
Corollary elab_init_final_links:forall w P final,
 Inv(s_init w)->elab_program(s_init w)=Good(P,final)->FinalLinks P final.
Proof. intros;eapply elab_program_final_links;[eassumption|apply functor_rows_init|apply functor_bound_init|eassumption]. Qed.
Corollary elab_init_historical_links:forall w P final earlier,
 Inv(s_init w)->elab_program(s_init w)=Good(P,final)->Fr earlier final->FinalLinks P earlier.
Proof. intros;eapply final_links_back;[eassumption|eapply elab_init_final_links;eassumption]. Qed.
