(* A complete source reading and public build/run derivation. In particular,
   the revised contract is inhabited without any runtime-typing hypothesis. *)
From Stdlib Require Import List String Arith Lia.
From NM Require Import AuditBase.
Import AuditBase.Compilation.
From NM Require Import BaseFacts.
From NM Require BodyProgramTyping.
From NM Require Import LegacyBodyJudgments.
From NM Require Import Audit PublicModelExamples FrontEnd.
From NM Require PublicMergePresent.
Import AuditBase.Semantics.
Import ListNotations Input Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition main_body := ABlock [] None.
Definition main_decl := Decl "/lib/main.moss" (ADFn RPlain "main" [] None (Some main_body)) [].
Definition main_file := AFile [] [d_ast main_decl].
Definition main_module := AModule "/lib/main.moss" false [] main_file.
Definition main_bundle := Bundle "/lib/main.moss" "/lib/prelude.moss" [main_module].
Definition scope (p : string) (ns : nspace) (n : string) : option ent :=
  if String.eqb p "/lib/main.moss" then
    match ns with NSName => if String.eqb n "main" then Some (ESym 1) else None | _ => None end
  else None.
Definition world_main := World
  (fun s => if Nat.eqb s 1 then Some main_decl else None)
  scope scope (fun _ _ => None) (fun _ => []) (Some "/lib/") Platform.root_item
  (fun s => if Nat.eqb s 1 then Some (Cx [] [] [] [] [], []) else None).

Lemma reads_decl : forall s d, w_decl world_main s = Some d -> s = 1 /\ d = main_decl.
Proof. intros s d H. cbn in H. destruct (Nat.eqb s 1) eqn:E; [|discriminate].
  apply Nat.eqb_eq in E. injection H as <-. auto.
Qed.
Lemma loaded_main : forall p, Loaded main_bundle p -> p = "/lib/main.moss".
Proof.
  intros p H. induction H as [|p m t imp Loaded IH Read Import]; [reflexivity|].
  subst p. cbn in Read. injection Read as <-. contradiction.
Qed.
Lemma declared_main : Declared main_bundle main_decl.
Proof. split; [constructor|]. exists main_module. split; [reflexivity|].
  apply OccHere; [simpl;auto|intros;discriminate].
Qed.
Lemma declared_only_main : forall d, Declared main_bundle d -> d = main_decl.
Proof.
  intros [p ast assumes] [Loaded [m [Read Occurs]]].
  apply loaded_main in Loaded. cbn in Loaded. subst p.
  cbn in Read. injection Read as <-. inversion Occurs; subst; cbn in *.
  - destruct H as [<-|[]]. rewrite <- H2. reflexivity.
  - destruct H as [Impossible|[]]. discriminate.
Qed.
Lemma exports_main : forall ns n e, Exports main_bundle world_main "/lib/main.moss" ns n e <->
  ns = NSName /\ n = "main" /\ e = ESym 1.
Proof.
  intros ns n e. split.
  - intro H. inversion H; subst.
    + apply reads_decl in H0 as [-> ->]. cbn in H2. injection H2 as <- <-. auto.
    + cbn in H0. injection H0 as <-. contradiction.
    + cbn in H0. injection H0 as <-. contradiction.
  - intros [-> [-> ->]]. eapply ExOwn with (d:=main_decl); reflexivity.
Qed.
Lemma ordinary_scope_main : forall ns n e,
  OrdinaryScope main_bundle world_main "/lib/main.moss" ns n e <->
  ns = NSName /\ n = "main" /\ e = ESym 1.
Proof.
  intros ns n e. split.
  - intro H. inversion H; subst.
    + apply exports_main; assumption.
    + cbn in H0. injection H0 as <-. contradiction.
  - intro H. constructor. apply exports_main. exact H.
Qed.
Lemma scope_main : forall ns n e, InScope main_bundle world_main "/lib/main.moss" ns n e <->
  ns = NSName /\ n = "main" /\ e = ESym 1.
Proof.
  intros ns n e. split.
  - intro H. inversion H; subst.
    + apply ordinary_scope_main; assumption.
    + cbn in H0. injection H0 as <-. discriminate.
  - intro H. apply ScPrimary. apply ordinary_scope_main. exact H.
Qed.
Lemma scope_table : forall ns n e,
  scope "/lib/main.moss" ns n = Some e <-> ns = NSName /\ n = "main" /\ e = ESym 1.
Proof.
  intros ns n e. destruct ns; cbn.
  - split; intro H.
    + destruct (String.eqb n "main") eqn:E; [|discriminate].
      apply String.eqb_eq in E. injection H as <-. auto.
    + destruct H as [_ [-> ->]]. reflexivity.
  - split; [discriminate|intros [H _];discriminate].
  - split; [discriminate|intros [H _];discriminate].
Qed.
Lemma no_platform_symbols : forall lib,
  In lib ["wasm";"bool";"wasip1";"wasistd"] -> forall ns n symbol,
  ~ LangSym world_main lib ns n symbol.
Proof.
  intros lib Library ns n symbol [d [p [Read [Module Scoped]]]].
  destruct Library as [<-|[<-|[<-|[<-|[]]]]];
    unfold LibraryModule in Module; cbn in Module; subst p; discriminate Scoped.
Qed.

Lemma world_main_valid : WorldOf main_bundle world_main.
Proof.
  constructor.
  - exists (fun _ => 0). intros p m t imp Loaded Read Import.
    apply loaded_main in Loaded. subst p. cbn in Read. injection Read as <-. contradiction.
  - intros s d Read. apply reads_decl in Read as [-> ->]. split; [exact declared_main|discriminate].
  - intros d Declared Kind. apply declared_only_main in Declared. subst d.
    exists 1. split; [reflexivity|]. intros s Read. apply reads_decl in Read. tauto.
  - intros p ns n e Loaded. apply loaded_main in Loaded. subst p.
    change (scope "/lib/main.moss" ns n = Some e <-> Exports main_bundle world_main "/lib/main.moss" ns n e).
    rewrite scope_table, exports_main. reflexivity.
  - intros p ns n e Loaded. apply loaded_main in Loaded. subst p.
    change (scope "/lib/main.moss" ns n = Some e <-> InScope main_bundle world_main "/lib/main.moss" ns n e).
    rewrite scope_table, scope_main. reflexivity.
  - intros r n symbol. split; [discriminate|].
    intros [d [rn [ps [rt [body [Read [Ast _]]]]]]].
    apply reads_decl in Read as [-> ->]. discriminate Ast.
  - intro s. constructor.
  - intros s k Kind _ _ _ t. split; [contradiction|].
    intros [d [pa [Read [InAssumes _]]]].
    apply reads_decl in Read as [-> ->]. contradiction.
  - intros. reflexivity.
  - intros g d ps t Read Body. apply reads_decl in Read as [-> ->]. discriminate Body.
  - intros R Closed Agrees g Body. unfold has_nominal_body, ast_of in Body. cbn in Body.
    destruct (Nat.eqb g 1); discriminate.
  - intros g d ps t n Read Body Member. apply reads_decl in Read as [-> ->]. discriminate Body.
  - reflexivity.
Qed.

Lemma subst_empty : forall t, app_subst [] t = t.
Proof.
  fix IH 1. intros [machine|fields| |symbol|head args|members]; cbn; try reflexivity.
  - f_equal. induction fields as [|[f t] rest Rest]; cbn; [reflexivity|]. rewrite IH, Rest. reflexivity.
  - f_equal. induction args as [|[f t] rest Rest]; cbn; [reflexivity|]. rewrite IH, Rest. reflexivity.
  - f_equal. induction members as [|t rest Rest]; cbn; [reflexivity|]. rewrite IH, Rest. reflexivity.
Qed.
Definition empty_context := Cx [] [] [] [] [].
Lemma main_context : CtxOf world_main 1 empty_context [].
Proof.
  exists main_decl, [], [], [], [], [], []. split; [reflexivity|].
  split; [constructor|]. split.
  - apply RootNone. right. intros gc d n its Named.
    exfalso. eapply no_platform_symbols with (lib := "wasm"); [simpl;auto|exact Named].
  - split.
    + split; [split; [intros e1 e2 k H;contradiction|intros l r []]|]. split.
      * intros. repeat rewrite subst_empty. reflexivity.
      * split; [intros; rewrite subst_empty; reflexivity|split; intros g t []].
    + split; [apply PublicMergePresent.empty_merge_of; split; [intros e e' k [] | intros l r []] |].
      split; [apply PublicMergePresent.empty_merge_section |].
      split; [reflexivity|]. split; [split; [constructor|]; intros k; split; [contradiction|];
        intros [e [Member _]]; contradiction|]. reflexivity.
Qed.
Lemma main_function : FnElab world_main PublicModelExamples.unit_program 1.
Proof.
  intros d recv n ps rt body Read Ast.
  apply reads_decl in Read as [_ ->]. injection Ast as <- <- <- <- <-.
  exists empty_context, [], None, (FSig [] TUnit),
    (Frame "/lib/main.moss" (Some TUnit) None false [] [] [] []), EUnit, TUnit.
  split; [exact main_context|]. split; [reflexivity|]. split.
  - exists main_decl, RPlain, "main", [], None, (Some main_body).
    split; [reflexivity|]. split; [reflexivity|]. split; [constructor|reflexivity].
  - repeat split; try reflexivity; try constructor.
    + intros i pn H. destruct i; discriminate.
    + eapply BlkECheck with (made := []).
      * constructor.
      * intros g H. contradiction.
      * left. reflexivity.
Qed.

(* The only node with a context is main, whose context merges nothing. *)
Lemma node_section_unmerged : forall n section,
  CodeSection world_main PublicModelExamples.unit_program n section -> section = [].
Proof.
  intros [f|h] section [_ Read].
  - cbn in Read. destruct (Nat.eqb f 1); [now injection Read|discriminate].
  - discriminate.
Qed.

(* No nominal has a payload and the one body enters nothing, so no edge. *)
Lemma main_bounded : NoExpandingCycle world_main PublicModelExamples.unit_program.
Proof.
  split; [exists (fun _ => 0)|exists (fun _ => 0); split;
    [|split; [intros n section t v s Read Occ|intros; lia]]].
  - intros a b strict (pay & args & tau & Pay & _). inversion Pay as [s xs d n ps t p Read Ast].
    apply reads_decl in Read as [_ ->]. discriminate.
  - intros [[f|h] t] b strict (site & th & tau & v & s & s' & At & _); cbn in At;
      [destruct (Nat.eqb f 1)|]; cbn in At; contradiction.
  - rewrite (node_section_unmerged n section Read) in Occ. cbn in Occ.
    destruct Occ as [[= <- <-]|[]]. lia.
Qed.

(* No nominal has a payload, so every layout is finite. *)
Lemma main_layouts : FiniteLayouts world_main.
Proof.
  intros n pay Pay. inversion Pay as [s xs d n' ps t p Read Ast].
  apply reads_decl in Read as [_ ->]. discriminate.
Qed.

(* main's nesting is empty; it is the only symbol. *)
Lemma main_assumes : AssumesComplete world_main.
Proof.
  intros s k Kind. unfold kind_sym in Kind.
  destruct (w_decl world_main s) as [d|] eqn:Read; [|discriminate].
  apply reads_decl in Read as [-> ->]. split.
  2: { intros d Declared. injection Declared as <-.
       split; [exists [], []; split; [constructor|exact I]|exact I]. }
  exists empty_context, [].
  destruct main_context as (d & es & tm & er & tmr & σ & classes & R & Its & Root & M & Form & Section & E & N & _).
  exists d, es, tm, er, tmr, σ, classes.
  exact (conj R (conj Its (conj Root (conj M (conj Form (conj Section (conj E N))))))).
Qed.

(* main is the only declaration; its signature, `fn main()`, forms. *)
Lemma main_decls : DeclsForm world_main PublicModelExamples.unit_program.
Proof.
  intros s d Read. apply reads_decl in Read as [-> ->]. cbn.
  exists [], (FSig [] TUnit), main_decl, RPlain, "main", [], None, (Some main_body).
  split; [reflexivity|]. split; [reflexivity|]. split; [constructor|reflexivity].
Qed.

(* lib/bool.moss is not loaded, so there is no Bool to form. *)
Lemma main_lang_items : LangItemsFormed world_main.
Proof.
  intros symbol Bool. exfalso.
  eapply no_platform_symbols with (lib := "bool"); [simpl; auto|exact Bool].
Qed.

Lemma main_elaborates : ElaboratesIn main_bundle world_main PublicModelExamples.unit_program.
Proof.
  apply BodyProgramTyping.separated_elaboration_readback.
  split; [exact world_main_valid|]. split.
  - intros f d recv n ps rt body Read Ast. apply reads_decl in Read as [-> ->]. exact main_function.
  - split; [reflexivity|]. split.
    + exists main_decl, "main", None, main_body. split; reflexivity.
    + split.
      * exists empty_context, [], [], []. split; [exact main_context|].
        split; reflexivity.
      * split; [|split; [exact main_lang_items|split; [exact main_assumes|exact main_decls]]]. right. split.
        -- intros symbol. eapply no_platform_symbols with (lib:="bool"); simpl; auto.
        -- repeat split; reflexivity.
Qed.
Lemma main_linked : Platform.Linked world_main PublicModelExamples.unit_program.
Proof.
  apply (proj2 (platform_linked_fields _ _)).
  assert (NoWasm : forall ns n symbol, ~ LangSym world_main "wasm" ns n symbol).
  { intros. eapply no_platform_symbols with (lib:="wasm"); simpl; auto. }
  assert (NoBool : forall ns n symbol, ~ LangSym world_main "bool" ns n symbol).
  { intros. eapply no_platform_symbols with (lib:="bool"); simpl; auto. }
  split.
  - exists empty_context, [], (FSig [] TUnit). split; [exact main_context|]. split.
    + exists main_decl, RPlain, "main", [], None, (Some main_body).
      split; [reflexivity|]. split; [reflexivity|]. split; [constructor|reflexivity].
    + split; [reflexivity|constructor].
  - split.
    + unfold Platform.Layouts, Platform.TagLayout. repeat split; intros; unfold LangItem, lang_item_key in *;
      exfalso; first [eapply NoWasm; eassumption | eapply NoBool; eassumption].
    + split.
      * reflexivity.
      * exists empty_context, [], [], []. split; [exact main_context|].
        split.
        { split; [|reflexivity]. split; [intros r []|]. repeat split; intros; contradiction. }
        split; [reflexivity|]. split; [constructor|].
        split; [constructor|]. split; [reflexivity|exact (conj main_layouts (conj main_bounded eq_refl))].
Qed.

Definition main_text := "fn main() {}".
Definition entry : file_path := AbsolutePath "/lib/main.moss" eq_refl.
Definition prelude : file_path := AbsolutePath "/lib/prelude.moss" eq_refl.
Definition main_source : source.
Proof.
  refine (Source [(entry, main_text)] _).
  repeat constructor; simpl; tauto.
Defined.
Lemma main_bundled : Loader.Bundled (source_read main_source) "/"
    "/lib/prelude.moss" "/lib/main.moss" main_bundle.
Proof.
  split; [reflexivity|]. split; [reflexivity|]. split.
  - constructor; [simpl;tauto|constructor].
  - split; [simpl;auto|]. intros m [<-|[]]. exists main_text, main_file.
    split; [reflexivity|]. split.
    + apply FrontEnd.parse_moss_sound. vm_compute. reflexivity.
    + split; [reflexivity|]. split; [reflexivity|]. split; [reflexivity|].
      split; [intros t H; contradiction|intros H;discriminate].
Qed.
Example public_build_is_inhabited : PublicModelExamples.Model.Builds prelude main_source entry PublicModelExamples.unit_program.
Proof. exists main_bundle, world_main. split; [exact main_bundled|]. split; [exact main_elaborates|exact main_linked]. Qed.
Example built_program_runs :
  PublicModelExamples.Model.Runs PublicModelExamples.unit_program PublicModelExamples.Model.default_interface PublicModelExamples.initial
    (PublicModelExamples.Model.Result (PublicModelExamples.Model.Returned VUnit) PublicModelExamples.initial).
Proof. exact PublicModelExamples.unit_program_runs. Qed.
