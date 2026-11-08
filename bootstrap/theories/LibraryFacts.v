(* The language's library symbols are selected in the prelude's directory.
   Their scopes may reexport declarations, exactly as ordinary scopes do. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound MethodLookup.
Import ListNotations.
Module A := ExecutionModel.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma sassoc_append : forall V key (xs ys : list (string * V)),
  sassoc key (xs ++ ys) = match sassoc key xs with Some v => Some v | None => sassoc key ys end.
Proof.
  intros V key xs. induction xs as [|[k v] xs IH]; intro ys; [reflexivity|].
  cbn. destruct (String.eqb key k); [reflexivity|exact (IH ys)].
Qed.

Definition library_names : list string :=
  ["num"; "int"; "float"; "bool"; "wasm"; "wasip1"; "wasistd"; "access"].
Definition library_rows (dir p : string) (mi : nat) (names : list string) : list (string * nat) :=
  flat_map (fun short => if String.eqb p (String.append dir (String.append short ".moss")) then [(short, mi)] else []) names.

Lemma rows_lookup : forall names dir p mi short,
  sassoc short (library_rows dir p mi names) =
  if smem short names && String.eqb p (String.append dir (String.append short ".moss")) then Some mi else None.
Proof.
  induction names as [|n names IH]; intros dir p mi short; [reflexivity|].
  cbn [library_rows flat_map smem].
  destruct (String.eqb_spec short n) as [->|N].
  - cbn.
    destruct (String.eqb p (String.append dir (String.append n ".moss"))) eqn:Ep; cbn.
    + rewrite String.eqb_refl. reflexivity.
    + change (sassoc n (library_rows dir p mi names) = None).
      rewrite (IH dir p mi n), Ep. destruct (smem n names); reflexivity.
  - assert (En : String.eqb short n = false) by (apply String.eqb_neq; exact N).
    rewrite ?En. cbn.
    destruct (String.eqb p (String.append dir (String.append n ".moss"))); cbn; rewrite ?En;
      exact (IH dir p mi short).
Qed.

Lemma rows_member : forall names dir p mi short x,
  In (short, x) (library_rows dir p mi names) ->
  x = mi /\ In short names /\ p = String.append dir (String.append short ".moss").
Proof.
  intros names dir p mi short x H.
  apply in_flat_map in H as (n & Hn & H).
  destruct (String.eqb p (String.append dir (String.append n ".moss"))) eqn:Ep; [|destruct H].
  destruct H as [H|[]]. injection H as <- <-.
  split; [reflexivity|]. split; [exact Hn|]. apply String.eqb_eq. exact Ep.
Qed.

Lemma lang_entry : forall w short mi, sassoc short (lang_modules w) = Some mi ->
  In mi (w_order w) /\ A.LibraryModule (W_read w (fun _ => []) (fun _ => 0)) short (gm_path (get_mod w mi)).
Proof.
  intros w short mi H. apply sassoc_in in H.
  unfold lang_modules in H. apply in_flat_map in H as (j & Hj & H).
  destruct (w_library w) as [dir|] eqn:Ed.
  - change (In (short, mi) (library_rows dir (gm_path (get_mod w j)) j library_names)) in H.
    destruct (rows_member _ _ _ _ _ _ H) as (-> & Hn & Ep).
    split; [exact Hj|]. unfold A.LibraryModule. cbn [A.w_library W_read]. rewrite Ed. exact Ep.
  - cbn in H. contradiction.
Qed.

Lemma lookup_selected_module : forall w dir short ids gm,
  w_library w = Some dir -> In short library_names ->
  find_mod w (String.append dir (String.append short ".moss")) ids = Some gm ->
  exists mi, sassoc short
    (flat_map (fun j => library_rows dir (gm_path (get_mod w j)) j library_names) ids) = Some mi /\
    get_mod w mi = gm /\ gm_path gm = String.append dir (String.append short ".moss").
Proof.
  intros w dir short ids. induction ids as [|j ids IH]; intros gm Ed Hn H; [discriminate|].
  cbn [find_mod] in H. cbn [flat_map]. rewrite sassoc_append, rows_lookup.
  assert (Hs : smem short library_names = true).
  { destruct (smem short library_names) eqn:E; [reflexivity|].
    exfalso. exact (smem_false _ _ E Hn). }
  rewrite Hs. cbn.
  destruct (String.eqb (gm_path (get_mod w j)) (String.append dir (String.append short ".moss"))) eqn:Ep.
  - injection H as <-. exists j. split; [reflexivity|]. split; [reflexivity|]. apply String.eqb_eq. exact Ep.
  - exact (IH gm Ed Hn H).
Qed.

Lemma selected_scope : forall w R Fl p ns n x,
  A.LibraryModule (W_read w R Fl) "wasm" p ->
  A.w_scope (W_read w R Fl) p ns n = Some (A.ESym x) ->
  exists wm, sassoc "wasm" (lang_modules w) = Some wm /\
    p = gm_path (get_mod w wm) /\ sassoc n (tbl (get_mod w wm) false ns) = Some x.
Proof.
  intros w R Fl p ns n x Hp Hs.
  unfold A.LibraryModule in Hp. cbn [A.w_library W_read] in Hp.
  destruct (w_library w) as [dir|] eqn:Ed; [|contradiction]. subst p.
  cbn [A.w_scope W_read] in Hs.
  destruct (find_mod w (String.append dir (String.append "wasm" ".moss")) (w_order w)) as [gm|] eqn:Eg; [|discriminate].
  destruct (lookup_selected_module w dir "wasm" (w_order w) gm Ed ltac:(cbn; auto 10) Eg)
    as (wm & Ew & Em & Ep).
  exists wm. split.
  - unfold lang_modules. rewrite Ed. exact Ew.
  - rewrite Em. split; [symmetry; exact Ep|].
    destruct ns; cbn [tbl aent] in Hs |- *.
    + destruct (sassoc n (gm_names gm)) eqn:En; [|discriminate]. injection Hs as <-. reflexivity.
    + destruct (sassoc n (gm_detached gm)) eqn:En; [|discriminate]. injection Hs as <-. reflexivity.
    + destruct (sassoc n (gm_aliases gm)); discriminate.
Qed.

Section Declarations.
Variables (b : bundle) (W : A.world).
Fixpoint export_decl p ns n e (H : A.Exports b W p ns n e) {struct H} :
  forall s, e = A.ESym s -> exists d, A.w_decl W s = Some d.
Proof.
  destruct H as [p s0 d ns n Hd Hm Hf|p m t imp u e ns Hm Hi Hg Hu Ens Hx|p m t imp a Hm Hi Ha];
    intros s Es.
  - injection Es as <-. exists d. exact Hd.
  - destruct Hx as [Hx|(_ & _ & Hx)]; exact (export_decl _ _ _ _ Hx s Es).
  - discriminate.
Qed.

Lemma scope_decl : LoaderReading b W -> forall p ns n s,
  A.Loaded b p -> A.w_scope W p ns n = Some (A.ESym s) -> exists d, A.w_decl W s = Some d.
Proof.
  intros L p ns n s Hl Hs. apply (proj1 (lr_scope _ _ L p ns n (A.ESym s) Hl)) in Hs.
  destruct (@A.scope_exports b W p ns n (A.ESym s) Hs) as [He|(m & t & imp & Hm & Hi & He)];
    exact (export_decl _ _ _ _ He s eq_refl).
Qed.
End Declarations.

Theorem lang_symbol_scope : forall b w R Fl,
  load_world b = Good w -> forall (short : string) (det : bool) (n : name) (mi x : nat),
  sassoc short (lang_modules w) = Some mi ->
  sassoc n (if det then gm_detached (get_mod w mi) else gm_names (get_mod w mi)) = Some x ->
  A.LangSym (W_read w R Fl) short (if det then A.NSDetached else A.NSName) n x.
Proof.
  intros b w R Fl Hl short det n mi x Hm Hn.
  destruct (lang_entry w short mi Hm) as (Hi & Hp).
  assert (Hs : A.w_scope (W_read w R Fl) (gm_path (get_mod w mi))
      (if det then A.NSDetached else A.NSName) n = Some (A.ESym x)).
  { rewrite (loaded_scope_table b w R Fl Hl mi _ n Hi).
    destruct det; cbn [tbl aent]; rewrite Hn; reflexivity. }
  destruct (scope_decl b (W_read w R Fl) (load_world_reads b w R Fl Hl)
    _ _ _ _ (load_world_loaded b w Hl mi Hi) Hs) as (d & Hd).
  exists d, (gm_path (get_mod w mi)). split; [exact Hd|]. split; [exact Hp|exact Hs].
Qed.
