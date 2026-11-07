(* The final Bool clause of Elaborates follows from the real program run,
   including the earlier library check and preservation of its selections. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit Sx Elab ElabSound WorldSound ElabFrame
 ProgramInfo MainProgramTrace RuntimeBoolSupport RuntimeBoolProgram.
Import ListNotations.
Module A := Audit.Elaboration.
Local Open Scope string_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL fn_info].

Lemma runtime_schema_frame : forall initial final left right,
 Fr initial final -> runtime_symbols initial = Good (left, initial) ->
 runtime_symbols final = Good (right, final) -> left = right.
Proof.
 intros initial final left right Frame First Last.
 eapply runtime_schemas_same; [symmetry; exact (fr_w _ _ Frame)|
   symmetry; exact (fx_lang _ _ (fr_x _ _ Frame))|exact First|exact Last].
Qed.

Lemma ok_main_platform_fold : forall info, ok (main_platform_fold info).
Proof. intro info; unfold main_platform_fold; okt. Qed.

Definition bool_selection_matches (world : A.world) (p : xprogram) : Prop :=
 (A.LangSym world "bool" A.NSName "Bool" (xo_bool p) /\
  A.LangSym world "bool" A.NSName "True" (xo_true p) /\
  A.LangSym world "bool" A.NSName "False" (xo_false p)) \/
 ((forall x, ~ A.LangSym world "bool" A.NSName "Bool" x) /\
  xo_bool p = 0 /\ xo_true p = 0 /\ xo_false p = 0).

Theorem main_program_bool : forall b w R Fl initial p final,
 load_world b = Good w -> Inv initial ->
 st_w initial = w -> st_lang initial = lang_modules w ->
 elab_program initial = Good (p, final) ->
 bool_selection_matches (W_read w R Fl) p.
Proof.
 intros b w R Fl initial p final Load I World Lang Run.
 destruct (main_program_trace initial p final Run) as
  (listed & contexts & library & lowered & assumes & mainid & main & info & infos &
   signature & platform & prims & supplied & sc & inputstate & inputs & types &
   Listed & Contexts & Library & Lower & Assumes & Main & Symbol & Defined &
   Info & Signature & Platform & Unsup & Runtime & Inputs & Types & Output).
 destruct (proj1 program_info_listed _ _ _ I Listed) as [Ilisted Flisted].
 destruct (proj1 program_info_contexts _ _ _ Ilisted Contexts) as [Icontexts Fcontexts].
 destruct (proj1 program_info_runtime _ _ _ Icontexts Library) as [Ilibrary Flibrary].
 destruct (proj1 program_info_lower _ _ _ Ilibrary Lower) as [Ilowered Flower].
 destruct (proj1 program_info_assumes _ _ _ Ilowered Assumes) as [Iassumes Fassumes].
 destruct (ok_fn_info_frame main _ _ _ Iassumes Info) as [Iinfos Finfo].
 destruct (ok_check_main_signature main info _ _ _ Iinfos Signature) as [Isignature Fsignature].
 destruct (ok_main_platform_fold info _ _ _ Isignature Platform) as [Isupplied Fplatform].
 pose proof (fr_trans _ _ _ Flisted Fcontexts) as ToContexts.
 pose proof (fr_trans _ _ _ Flibrary
   (fr_trans _ _ _ Flower (fr_trans _ _ _ Fassumes
    (fr_trans _ _ _ Finfo (fr_trans _ _ _ Fsignature Fplatform))))) as ToSupplied.
 pose proof (runtime_schema_frame contexts supplied _ sc ToSupplied
   (runtime_symbols_exact contexts) Runtime) as Schema.
 pose proof (checked_library_bool_shape contexts library Library) as Shape.
 rewrite Schema in Shape.
 assert (Whole : Fr initial supplied) by (eapply fr_trans; eassumption).
 assert (Wsup : st_w supplied = w) by (rewrite (fr_w _ _ Whole); exact World).
 assert (Lsup : st_lang supplied = lang_modules w)
   by (rewrite (fx_lang _ _ (fr_x _ _ Whole)); exact Lang).
 pose proof (runtime_bool_source_contract b w R Fl supplied sc Load Wsup Lsup Runtime Shape) as Bool.
 destruct (runtime_bool_tuple sc) as [[bid tid] fid].
 subst p; exact Bool.
Qed.

(* The wrapper has no outstanding body-soundness or final-state premises. *)
Lemma loaded_initial_invariant : forall b w,
 load_world b = Good w -> Inv (s_init w).
Proof.
 intros b w Load; destruct (load_world_syms b w Load) as [Positive Above].
 constructor; cbn [st_next st_syms st_flds st_w st_reqs s_init].
 - exact Above.
 - split; [constructor | reflexivity].
 - exact (load_world_ids b w Load).
 - intros b' Load' i g Lookup; exact Lookup.
 - intros b' Load' i g Absent Lookup; rewrite Absent in Lookup; discriminate.
 - intros b' Load' x l Lookup; destruct x; discriminate Lookup.
 - intros b' Load' i g Absent Lookup; rewrite Absent in Lookup; discriminate.
Qed.

Corollary elaborate_bool : forall b p,
 elaborate b = Good p -> exists w,
 load_world b = Good w /\
 forall R Fl, bool_selection_matches (W_read w R Fl) p.
Proof.
 intros b p Accepted.
 destruct (elaborate_frame b p Accepted) as (w & final & Load & Run & Rest).
 exists w; split; [exact Load|]; intros R Fl.
 eapply main_program_bool; [exact Load|exact (loaded_initial_invariant b w Load)|
   reflexivity|reflexivity|exact Run].
Qed.
