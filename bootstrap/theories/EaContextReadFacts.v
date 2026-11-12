(* Source reads are independent of the context table being installed. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaCtx ElabWorld.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma rbind_pointwise : forall A B (x y : er A) (f g : A -> er B),
  x = y -> (forall a, f a = g a) -> rbind x f = rbind y g.
Proof. intros A B x y f g <- Same. destruct x; [apply Same|reflexivity]. Qed.

Lemma rmap_pointwise : forall A B (f g : A -> er B),
  (forall a, f a = g a) -> forall xs, rmap f xs = rmap g xs.
Proof.
  intros A B f g Same xs. induction xs as [|x xs IH]; [reflexivity|].
  cbn [rmap]. rewrite Same, IH. reflexivity.
Qed.

Lemma flat_map_pointwise : forall A B (f g : A -> list B),
  (forall a, f a = g a) -> forall xs, flat_map f xs = flat_map g xs.
Proof. intros A B f g Same xs. induction xs; cbn; [reflexivity|]. now rewrite Same, IHxs. Qed.

Section ContextReads.
Variable W : world.
Variable contexts : nat -> option (cx * list item).
Let W' := ElabWorld.with_context W contexts.

Lemma resolve_sym_context : forall p pa, resolve_sym W' p pa = resolve_sym W p pa.
Proof. intros p pa. destruct W. apply ElabWorld.resolve_sym_ctx. Qed.

Lemma sym_name_context : sym_name W' = sym_name W.
Proof. reflexivity. Qed.
Lemma show_ty_go_context : forall fuel, show_ty_go W' fuel = show_ty_go W fuel.
Proof.
  induction fuel as [|fuel IH]; [reflexivity|].
  cbn [show_ty_go]. rewrite IH, sym_name_context. reflexivity.
Qed.
Lemma qty_context : qty W' = qty W.
Proof. unfold qty, sty. rewrite show_ty_go_context. reflexivity. Qed.
Local Opaque qty.
Lemma qs_context : qs W' = qs W.
Proof. reflexivity. Qed.
Lemma qnames_context : qnames W' = qnames W.
Proof. reflexivity. Qed.
Lemma kind_sym_context : kind_sym W' = kind_sym W.
Proof. reflexivity. Qed.
Lemma kind_is_context : kind_is W' = kind_is W.
Proof. reflexivity. Qed.
Lemma decl_of_context : decl_of W' = decl_of W.
Proof. reflexivity. Qed.
Lemma method_named_context : method_named W' = method_named W.
Proof. reflexivity. Qed.

Lemma union_heads_context : forall values seen,
  union_heads W' values seen = union_heads W values seen.
Proof.
  induction values as [|value values IH]; intros seen; [reflexivity|].
  cbn [union_heads]. rewrite qty_context, IH. reflexivity.
Qed.

Theorem type_readers_context : forall fuel,
  (forall p tm this syntax, ty_of W' fuel p tm this syntax = ty_of W fuel p tm this syntax) /\
  (forall p tm symbol syntax, brackets W' fuel p tm symbol syntax = brackets W fuel p tm symbol syntax).
Proof.
  induction fuel as [|fuel [TypeRead BracketRead]]; [split; reflexivity|]. split.
  - intros p tm this syntax.
    destruct syntax as [| | |fields|alternatives|pa application]; [reflexivity|reflexivity|reflexivity| | |].
    + cbn [ty_of]. apply rbind_pointwise; [reflexivity|intros []].
      apply rbind_pointwise; [|reflexivity]. apply rmap_pointwise.
      intros [label t]. cbn [fst snd]. rewrite TypeRead. reflexivity.
    + cbn [ty_of]. apply rbind_pointwise; [apply rmap_pointwise; exact (TypeRead p tm this)|].
      intro values. rewrite sym_name_context, qty_context, union_heads_context. reflexivity.
    + cbn [ty_of]. rewrite resolve_sym_context.
      destruct (resolve_sym W p pa) as [symbol|]; cbn [rbind]; [|reflexivity].
      rewrite kind_sym_context.
      destruct (kind_sym W symbol) as [kind|]; [|reflexivity].
      destruct kind; [reflexivity| | |reflexivity|reflexivity|reflexivity|reflexivity|reflexivity].
      * rewrite decl_of_context.
        apply rbind_pointwise; [reflexivity|intros d].
        destruct (d_ast d); try reflexivity. rewrite BracketRead.
        apply rbind_pointwise; [reflexivity|intros bs; apply TypeRead].
      * rewrite BracketRead. reflexivity.
  - intros p tm symbol [bindings|]; cbn [brackets]; [|reflexivity].
    apply rbind_pointwise; [|reflexivity]. apply rmap_pointwise.
    intros [bp [[head|] [detached|] application]]; cbn [fst snd]; try reflexivity.
    rewrite resolve_sym_context.
    destruct (resolve_sym W p bp) as [target|]; cbn [rbind]; [|reflexivity].
    rewrite !kind_is_context.
    destruct (kind_is W target DKType); cbn [rbind]; [|reflexivity].
    change (w_reqs W') with (w_reqs W).
    destruct (kind_is W symbol DKMeth || nmem target (w_reqs W symbol)); cbn [rbind]; [|reflexivity].
    rewrite TypeRead. reflexivity.
Qed.

Corollary ty_of_context : forall fuel p tm this syntax,
  ty_of W' fuel p tm this syntax = ty_of W fuel p tm this syntax.
Proof. intro fuel. exact (proj1 (type_readers_context fuel)). Qed.

Corollary brackets_context : forall fuel p tm symbol syntax,
  brackets W' fuel p tm symbol syntax = brackets W fuel p tm symbol syntax.
Proof. intro fuel. exact (proj2 (type_readers_context fuel)). Qed.

Lemma sig_of_context : forall fuel symbol tm this,
  sig_of W' fuel symbol tm this = sig_of W fuel symbol tm this.
Proof.
  intros fuel symbol tm this. unfold sig_of. rewrite decl_of_context.
  apply rbind_pointwise; [reflexivity|intros d]. destruct (d_ast d); try reflexivity.
  apply rbind_pointwise.
  - apply rmap_pointwise. intro pt. apply ty_of_context.
  - intro params. apply rbind_pointwise; [|reflexivity]. destruct o; [apply ty_of_context|reflexivity].
Qed.

Lemma recv_ty_context : forall fuel receiver tm,
  recv_ty W' fuel receiver tm = recv_ty W fuel receiver tm.
Proof.
  intros fuel receiver tm. unfold recv_ty.
  change (w_decl W') with (w_decl W). change (w_reqs W') with (w_reqs W).
  rewrite kind_is_context. destruct (nlook receiver tm); [reflexivity|].
  destruct (w_decl W receiver) as [d|]; [destruct (d_ast d)|]; try reflexivity.
  apply ty_of_context.
Qed.

Lemma bracket_bound_context : forall p application,
  bracket_bound W' p application = bracket_bound W p application.
Proof.
  intros p application. unfold bracket_bound. apply flat_map_pointwise.
  intro binding. rewrite resolve_sym_context. reflexivity.
Qed.

Lemma free_in_context : forall fuel R p syntax,
  free_in W' fuel R p syntax = free_in W fuel R p syntax.
Proof.
  induction fuel as [|fuel IH]; intros R p syntax; [reflexivity|].
  destruct syntax as [| | |fields|alternatives|pa application]; cbn [free_in]; try reflexivity.
  - apply flat_map_pointwise. intro field. apply IH.
  - apply flat_map_pointwise. intro t. apply IH.
  - rewrite resolve_sym_context. destruct (resolve_sym W p pa) as [symbol|]; [|reflexivity].
    rewrite bracket_bound_context, kind_sym_context. f_equal.
    apply flat_map_pointwise. intros [bp [[head|] detached bindings]]; cbn [snd]; [apply IH|reflexivity].
Qed.

Lemma sig_mentions_context : forall fuel symbol,
  sig_mentions W' fuel symbol = sig_mentions W fuel symbol.
Proof.
  intros fuel symbol. unfold sig_mentions. change (w_decl W') with (w_decl W).
  change (w_reqs W') with (w_reqs W). destruct (w_decl W symbol) as [d|]; [destruct (d_ast d)|]; try reflexivity.
  f_equal. f_equal.
  - apply flat_map_pointwise. intro pt. apply free_in_context.
  - destruct o; [apply free_in_context|reflexivity].
Qed.

Lemma app_of_context : forall fuel symbol tm,
  app_of W' fuel symbol tm = app_of W fuel symbol tm.
Proof.
  intros fuel symbol tm. unfold app_of, app_requirements. rewrite kind_is_context, sig_mentions_context.
  reflexivity.
Qed.

Lemma spec_types_go_context : forall fuel stack p syntax,
  spec_types_go W' fuel stack p syntax = spec_types_go W fuel stack p syntax.
Proof.
  induction fuel as [|fuel IH]; intros stack p [[pa|] [name|] application]; cbn [spec_types_go]; try reflexivity.
  rewrite resolve_sym_context. destruct (resolve_sym W p pa) as [symbol|]; [|reflexivity].
  rewrite kind_sym_context. change (w_decl W') with (w_decl W).
  destruct (kind_sym W symbol) as [kind|]; [destruct kind|]; try reflexivity.
  all: destruct (nmem symbol stack); try reflexivity.
  all: destruct (w_decl W symbol) as [d|]; [destruct (d_ast d)|]; try reflexivity.
  all: apply flat_map_pointwise; intro child; apply IH.
Qed.

Lemma spec_types_context : forall fuel p syntax,
  spec_types W' fuel p syntax = spec_types W fuel p syntax.
Proof. intros. unfold spec_types. apply spec_types_go_context. Qed.

Lemma functor_decl_context : functor_decl W' = functor_decl W.
Proof. reflexivity. Qed.

Lemma fun_outputs_context : forall fuel symbol,
  fun_outputs W' fuel symbol = fun_outputs W fuel symbol.
Proof.
  intros fuel symbol. unfold fun_outputs. rewrite functor_decl_context.
  destruct (functor_decl W symbol) as [[d [[[parameters arguments] results] body]]|]; [|reflexivity].
  f_equal. apply flat_map_pointwise. intro syntax. apply spec_types_context.
Qed.

Lemma result_brackets_context : forall fuel p application,
  flat_map (fun b => match snd b with
    | Spec (Some pa) _ bindings => free_in W' fuel (w_reqs W') p (ATRef pa bindings)
    | _ => [] end) (bracket_list application) =
  flat_map (fun b => match snd b with
    | Spec (Some pa) _ bindings => free_in W fuel (w_reqs W) p (ATRef pa bindings)
    | _ => [] end) (bracket_list application).
Proof.
  intros fuel p application. change (w_reqs W') with (w_reqs W). apply flat_map_pointwise.
  intros [bp [[pa|] detached bindings]]; cbn [snd]; [apply free_in_context|reflexivity].
Qed.

Lemma named_symbols_context : forall p names,
  flat_map (fun name => match resolve_sym W' p [name] with Some t => [t] | None => [] end) names =
  flat_map (fun name => match resolve_sym W p [name] with Some t => [t] | None => [] end) names.
Proof. intros p names. apply flat_map_pointwise. intro name. rewrite resolve_sym_context. reflexivity. Qed.

Lemma result_mentions_go_context : forall fuel stack self p syntax,
  result_mentions_go W' fuel stack self p syntax = result_mentions_go W fuel stack self p syntax.
Proof.
  induction fuel as [|fuel IH]; intros stack self p [[pa|] detached application]; cbn [result_mentions_go]; try reflexivity.
  rewrite resolve_sym_context. destruct (resolve_sym W p pa) as [symbol|]; [|reflexivity].
  rewrite bracket_bound_context, result_brackets_context.
  apply (f_equal2 (@app nat)); [|reflexivity].
  rewrite !kind_is_context. change (w_reqs W') with (w_reqs W).
  destruct detached as [name|].
  - rewrite method_named_context. destruct (method_named W p symbol name); [rewrite sig_mentions_context|]; reflexivity.
  - destruct (kind_is W symbol DKVal); [reflexivity|].
    destruct (kind_is W symbol DKFn); [rewrite sig_mentions_context; reflexivity|].
    change (w_decl W') with (w_decl W). destruct (w_decl W symbol) as [d|]; [|reflexivity].
    destruct (d_ast d) as [items declarations|name|name syntax|name parameters arity payload|name|name value_type initial|
      receiver name parameters result body|name items|name parameters arguments results body]; try reflexivity.
    + destruct (pmem (self,symbol) stack); [reflexivity|].
      f_equal. apply flat_map_pointwise. intro syntax. apply IH.
    + destruct parameters as [names|], body as [body|]; try reflexivity.
      destruct (Nat.eqb symbol self || pmem (symbol,symbol) stack); [reflexivity|].
      rewrite fun_outputs_context, named_symbols_context.
      f_equal. f_equal. apply flat_map_pointwise. intro syntax. apply IH.
Qed.

Lemma result_mentions_context : forall fuel self p syntax,
  result_mentions W' fuel self p syntax = result_mentions W fuel self p syntax.
Proof. intros. unfold result_mentions. apply result_mentions_go_context. Qed.

Lemma mentions_context : forall fuel symbol,
  mentions W' fuel symbol = mentions W fuel symbol.
Proof.
  intros fuel symbol. unfold mentions. rewrite functor_decl_context.
  destruct (functor_decl W symbol) as [[d [[[parameters arguments] results] body]]|]; [|reflexivity].
  rewrite fun_outputs_context. f_equal. f_equal. apply flat_map_pointwise. intro syntax. apply result_mentions_context.
Qed.

Lemma poly_context : forall fuel symbol,
  poly W' fuel symbol = poly W fuel symbol.
Proof.
  intros fuel symbol. unfold poly. rewrite functor_decl_context.
  destruct (functor_decl W symbol) as [[d [[[parameters arguments] results] body]]|]; [|reflexivity].
  destruct body as [body|]; [reflexivity|]. destruct parameters as [names|].
  - rewrite named_symbols_context. reflexivity.
  - apply mentions_context.
Qed.

Lemma fixed_context : forall fuel symbol,
  fixed W' fuel symbol = fixed W fuel symbol.
Proof. intros. unfold fixed. rewrite poly_context, mentions_context. reflexivity. Qed.

Lemma fixed_readings_context : forall fuel symbol tm,
  fixed_readings W' fuel symbol tm = fixed_readings W fuel symbol tm.
Proof. intros. unfold fixed_readings. rewrite fixed_context. reflexivity. Qed.

End ContextReads.

Local Opaque ty_of brackets sig_of recv_ty app_of fixed fixed_readings mentions poly fun_outputs result_mentions resolve_sym.

Section ContextConstruction.
Variable W : world.
Variable contexts : nat -> option (cx * list item).
Variable strategy : strategy.
Variables modules : list string.
Variables mains : list nat.
Variables fuel metadata : nat.
Local Notation W' := (ElabWorld.with_context W contexts).
Let E := Env W strategy modules mains fuel metadata.
Let E' := Env W' strategy modules mains fuel metadata.

Lemma equation_types_context : forall p l r,
  equation_types W' p l r = equation_types W p l r.
Proof.
  intros p l r. unfold equation_types. apply filter_ext.
  intro sp. destruct sp as [[pa|] [dn|] [app|]]; try reflexivity.
  rewrite resolve_sym_context, kind_sym_context. reflexivity.
Qed.

Theorem entries_context : forall n,
  (forall stack p ob tm syntax, item_entries E' n stack p ob tm syntax = item_entries E n stack p ob tm syntax) /\
  (forall stack p ob tm syntax, items_entries E' n stack p ob tm syntax = items_entries E n stack p ob tm syntax).
Proof.
  induction n as [|n [ItemRead ItemsRead]]; [split; reflexivity|]. split.
  - intros stack p ob tm [[pa|] [detached|] application]; [| |reflexivity|reflexivity];
      cbn [item_entries E E' e_W e_fuel e_metadata_fuel].
    + apply rbind_pointwise; [rewrite resolve_sym_context; reflexivity|intro receiver].
      apply rbind_pointwise; [rewrite method_named_context; reflexivity|intro method].
      apply rbind_pointwise; [apply brackets_context|intro bindings].
      apply rbind_pointwise; [apply recv_ty_context|intro receiver_type].
      apply rbind_pointwise; [apply sig_of_context|intro signature].
      rewrite app_of_context. reflexivity.
    + apply rbind_pointwise; [rewrite resolve_sym_context; reflexivity|intro symbol].
      apply rbind_pointwise; [rewrite decl_of_context; reflexivity|intro d].
      rewrite sym_name_context, ?qs_context.
      destruct (d_ast d) as [items declarations|name|name syntax|name parameters arity payload|name|name value_type initial|
        receiver name parameters result body|name items|name parameters arguments results body];
        [reflexivity|reflexivity|reflexivity|reflexivity|reflexivity| | | |].
      * destruct initial as [initial|]; [reflexivity|].
        apply rbind_pointwise; [apply brackets_context|intro bindings].
        apply rbind_pointwise; [apply ty_of_context|intro meaning].
        rewrite app_of_context. reflexivity.
      * destruct receiver; [|reflexivity|reflexivity].
        apply rbind_pointwise; [apply brackets_context|intro bindings].
        apply rbind_pointwise; [apply sig_of_context|intro signature].
        rewrite app_of_context. reflexivity.
      * apply rbind_pointwise; [reflexivity|intros []].
        apply rbind_pointwise; [apply brackets_context|intro bindings]. apply ItemsRead.
      * destruct body as [body|]; [reflexivity|].
        apply rbind_pointwise; [reflexivity|intros []].
        rewrite fixed_context, fixed_readings_context. reflexivity.
  - intros stack p ob tm [|[syntax|left right] rest]; cbn [items_entries E E' e_W e_fuel e_metadata_fuel].
    + reflexivity.
    + apply rbind_pointwise; [apply ItemRead|intros [head_entries head_meanings]].
      apply rbind_pointwise; [apply ItemsRead|reflexivity].
    + rewrite equation_types_context.
      apply rbind_pointwise; [apply ItemsRead|intros [head_entries head_meanings]].
      apply rbind_pointwise; [apply ty_of_context|intro left_type].
      apply rbind_pointwise; [apply ty_of_context|intro right_type].
      apply rbind_pointwise; [apply ItemsRead|reflexivity].
Qed.

Corollary items_entries_context : forall n stack p ob tm syntax,
  items_entries E' n stack p ob tm syntax = items_entries E n stack p ob tm syntax.
Proof. intro n. exact (proj2 (entries_context n)). Qed.

Lemma root_items_context : forall q es0 tm syntax,
  root_items E' q es0 tm syntax = root_items E q es0 tm syntax.
Proof.
  intros q es0 tm syntax. revert es0 tm.
  induction syntax as [|[syntax|left right] rest IH]; intros es0 tm; cbn [root_items E E' e_W e_metadata_fuel]; [reflexivity| |].
  - destruct syntax as [[pa|] [name|] application]; [rewrite (IH es0 tm); reflexivity| |rewrite (IH es0 tm); reflexivity|rewrite (IH es0 tm); reflexivity].
    rewrite resolve_sym_context. change (w_decl W') with (w_decl W).
    destruct (resolve_sym W q pa) as [symbol|]; [|rewrite (IH es0 tm); reflexivity].
    destruct (w_decl W symbol) as [d|]; [|rewrite (IH es0 tm); reflexivity].
    destruct (d_ast d) as [items declarations|name|name syntax|name parameters arity payload|name|name value_type initial|
      receiver name parameters result body|name items|name parameters arguments results body].
    1-8: rewrite (IH es0 tm); reflexivity.
    destruct body as [body|].
    { destruct (has_nfun symbol es0); [rewrite (IH es0 tm); reflexivity|reflexivity]. }
    destruct (has_nfun symbol es0); [rewrite (IH es0 tm); reflexivity|].
    rewrite fixed_context, fixed_readings_context.
    apply rbind_pointwise; [apply IH|reflexivity].
  - rewrite (IH es0 tm). reflexivity.
Qed.

Lemma lang_item_sym_context : lang_item_sym W' = lang_item_sym W.
Proof. reflexivity. Qed.

Lemma root_ctx_context : forall symbol es0 tm,
  root_ctx E' symbol es0 tm = root_ctx E symbol es0 tm.
Proof.
  intros symbol es0 tm. unfold root_ctx. cbn [E E' e_mains e_W].
  change (root_context_symbol W') with (root_context_symbol W).
  change (w_decl W') with (w_decl W).
  destruct (nmem symbol mains); [|reflexivity].
  destruct (root_context_symbol W) as [gc|]; [|reflexivity].
  destruct (w_decl W gc) as [d|]; [|reflexivity].
  destruct (d_ast d); [reflexivity|reflexivity|reflexivity|reflexivity|reflexivity|reflexivity|reflexivity| |reflexivity].
  apply root_items_context.
Qed.

Lemma qkey_context : qkey W' = qkey W.
Proof.
  unfold qkey, show_key, mname, sty. rewrite (show_ty_go_context W contexts), (sym_name_context W contexts). reflexivity.
Qed.
Lemma mname_context : mname W' = mname W.
Proof. reflexivity. Qed.
Local Opaque qkey.

Lemma cannot_merge_context : cannot_merge E' = cannot_merge E.
Proof. unfold cannot_merge. cbn [E E' e_W]. rewrite qty_context. reflexivity. Qed.

Lemma unify_context : forall n, unify E' n = unify E n.
Proof.
  induction n as [|n IH]; [reflexivity|].
  cbn [unify E E' e_strat]. rewrite IH, cannot_merge_context. reflexivity.
Qed.

Lemma unify_sig_context : forall bound, unify_sig E' bound = unify_sig E bound.
Proof. intro bound. unfold unify_sig. rewrite (unify_context bound). reflexivity. Qed.
Lemma unify_inst_context : forall bound, unify_inst E' bound = unify_inst E bound.
Proof. intro bound. unfold unify_inst. rewrite (unify_context bound). reflexivity. Qed.

Lemma merge_round_context : forall bound section seen es substitution,
  merge_round E' bound section seen es substitution = merge_round E bound section seen es substitution.
Proof.
  intros bound section seen es. revert seen.
  induction es as [|entry rest IH]; intros seen substitution; [reflexivity|].
  cbn [merge_round E E' e_W].
  rewrite (unify_context bound), (unify_sig_context bound), (unify_inst_context bound), qkey_context, mname_context.
  destruct (entry_key section entry) as [key|].
  - destruct (find (fun previous => okey_eqb (entry_key section previous) (Some key)) seen) as [previous|]; [|apply IH].
    destruct (entry_sig section previous), (entry_sig section entry); [|reflexivity|reflexivity|apply IH].
    apply rbind_pointwise; [reflexivity|intro substitution'].
    apply rbind_pointwise; [reflexivity|intro substitution'']; apply IH.
  - destruct entry; [apply IH|apply IH|apply IH|apply IH|apply IH|].
    apply rbind_pointwise; [reflexivity|intro substitution']; apply IH.
Qed.

Lemma merge_rounds_context : forall n bound es substitution,
  merge_rounds E' n bound es substitution = merge_rounds E n bound es substitution.
Proof.
  induction n as [|n IH]; intros bound es substitution; [reflexivity|].
  cbn [merge_rounds]. apply rbind_pointwise; [apply merge_round_context|intro substitution'].
  destruct (Nat.eqb (List.length substitution') (List.length substitution)); [reflexivity|apply IH].
Qed.

Theorem merge_context : forall es, EaCtx.merge E' es = EaCtx.merge E es.
Proof.
  intro es. unfold EaCtx.merge. cbn [E E' e_fuel].
  apply rbind_pointwise; [apply merge_rounds_context|reflexivity].
Qed.

Theorem assume_ctx_context : forall symbol, assume_ctx E' symbol = assume_ctx E symbol.
Proof.
  intro symbol. unfold assume_ctx. cbn [E E' e_W e_fuel].
  apply rbind_pointwise; [rewrite decl_of_context; reflexivity|intro d].
  apply rbind_pointwise; [apply items_entries_context|intros [entries meanings]].
  apply rbind_pointwise; [apply root_ctx_context|intros [root_entries root_meanings]].
  apply rbind_pointwise; [apply merge_context|reflexivity].
Qed.

Theorem fun_ctx_context : forall symbol, fun_ctx E' symbol = fun_ctx E symbol.
Proof.
  intro symbol. unfold fun_ctx. cbn [E E' e_W e_fuel e_metadata_fuel].
  apply rbind_pointwise; [rewrite functor_decl_context; reflexivity|intros [d [[[parameters arguments] results] body]]].
  apply rbind_pointwise; [apply items_entries_context|intros [entries meanings]].
  apply rbind_pointwise; [apply merge_context|intros [substitution classes]].
  rewrite mentions_context. reflexivity.
Qed.

Lemma fold_left_pointwise : forall A B (f g : A -> B -> A),
  (forall a b, f a b = g a b) -> forall xs acc,
  fold_left f xs acc = fold_left g xs acc.
Proof.
  intros A B f g Same xs. induction xs; intro acc; [reflexivity|].
  cbn [fold_left]. rewrite Same. apply IHxs.
Qed.

Theorem context_table_context : forall bound,
  context_table E' bound = context_table E bound.
Proof.
  intro bound. unfold context_table. apply fold_left_pointwise.
  intros table symbol. cbn [E E' e_W]. rewrite kind_sym_context, assume_ctx_context, fun_ctx_context.
  reflexivity.
Qed.

End ContextConstruction.
