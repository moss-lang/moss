(* ------------------------------------------------------------------------ *)
(* RelSupport.v — support for RelExamples.v / RelNegative.v: evidence that the DRAFT relation (Audit.v, Part V) says  *)
(* what is intended: on small programs of tests/wasi, the elaborator's      *)
(* output for a function is related to its source, and on ill-formed ones   *)
(* no output is.                                                             *)
(*                                                                           *)
(* Method.  The bundle is elaborated inside Rocq (vm_compute).  The static  *)
(* reading W is the elaborator's own (its symbol numbering, scopes, and     *)
(* requirements), converted to Audit.Elaboration.world by `W_of` below.     *)
(* For each example function the body judgment FnElab W P f is then proved  *)
(* — the context formation (CtxOf, with merging), the calls' satisfaction   *)
(* maps and coherence, binds, functor application — leaving one thing      *)
(* stated and not proved: that W is a reading of the bundle (WorldOf b W),  *)
(* which is a property of name resolution only.                             *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit CoreMoss MossX NMap Sx Elab.
Import ListNotations.
Import Audit.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ===================== The elaborator's state, kept =================== *)

Definition dummy_st : st :=
  St (Elab.World NE [] 0 NE 0 NE) NE 0 NE NE NE [] [] NE [] [] NE NE [] 0 false false [] [] [].

(* Run the elaborator and keep its final state, with every symbol's
   requirements computed (the elaborator computes them lazily). *)
Definition run_elab (x : Input.sx) : Input.res (xprogram * st) :=
  match read_bundle x with
  | Good b =>
      match load_world b with
      | Good w =>
          let s0 := St w (w_syms w) (w_nsyms w) NE NE NE [] [] NE [] [] NE NE [] 0 false false []
                       (lang_modules w) [] in
          match elab_program s0 with
          | Good (p, s) =>
              Good (p, fold_left (fun s i => match reqs i s with Good (_, s') => s' | Bad _ => s end)
                                  (seq 1 (w_nsyms w)) s)
          | Bad m => Bad m
          end
      | Bad m => Bad m
      end
  | Bad m => Bad m
  end.

Fixpoint find_mod (w : Elab.world) (p : string) (l : list nat) : option gmod :=
  match l with
  | [] => None
  | i :: r => let m := get_mod w i in if String.eqb (gm_path m) p then Some m else find_mod w p r
  end.

Definition has_kind (d : Input.adecl) : bool :=
  match kind_of d with Some _ => true | None => false end.

(* The elaborator's world, as an Audit world. *)
Definition W_of (s : st) : Elaboration.world :=
  let w := st_w s in
  let scope (exp : bool) (p : string) (ns : nspace) (n : Input.name) : option ent :=
    match find_mod w p (w_order w) with
    | None => None
    | Some m =>
        match ns with
        | NSName => option_map ESym (sassoc n (if exp then gm_exp_names m else gm_names m))
        | NSDetached => option_map ESym (sassoc n (if exp then gm_exp_detached m else gm_detached m))
        | NSAlias => option_map (fun i => EMod (gm_path (get_mod w i)))
                                (sassoc n (if exp then gm_exp_aliases m else gm_aliases m))
        end
    end in
  World
    (fun x => match nget (st_syms s) x with
              | Some g => match gs_origin g with
                          | OSource => if has_kind (gs_decl g)
                                       then Some (Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g))
                                       else None
                          | _ => None
                          end
              | None => None
              end)
    (scope false) (scope true)
    (fun r n => match nget (st_syms s) r with
                | Some g => assoc attached_eqb (r, n) (gm_attached (get_mod w (gs_mod g)))
                | None => None
                end)
    (fun x => opt_default [] (nget (st_reqs s) x))
    (fun f => opt_default 0 (sassoc f (st_flds s))).

(* ===================== Merging with nothing to merge =================== *)

Definition fsig_eqb (a b : fsig) : bool :=
  (fix go (l1 l2 : list ty) := match l1, l2 with
                               | [], [] => true
                               | x :: r1, y :: r2 => ty_eqb x y && go r1 r2
                               | _, _ => false end) (sg_params a) (sg_params b)
  && ty_eqb (sg_ret a) (sg_ret b).

Lemma fsig_eqb_eq : forall a b, fsig_eqb a b = true -> a = b.
Proof.
  intros [pa ra] [pb rb]. unfold fsig_eqb. simpl. intro H. apply andb_true_iff in H as [H1 H2].
  apply ty_eqb_eq in H2. subst. f_equal.
  revert pb H1. induction pa as [|x r IH]; intros [|y r'] H; try discriminate; [reflexivity|].
  apply andb_true_iff in H as [Hx Hr]. apply ty_eqb_eq in Hx. subst. f_equal. apply IH. exact Hr.
Qed.

Definition osig_eqb (a b : option fsig) : bool :=
  match a, b with Some x, Some y => fsig_eqb x y | None, None => true | _, _ => false end.

Definition okey_eqb (a b : option item) : bool :=
  match a, b with Some x, Some y => item_eqb x y | _, _ => false end.

(* every two entries filed under one key have one signature *)
Definition unifies_b (s : subst) (es : list entry) : bool :=
  forallb (fun e1 => forallb (fun e2 =>
    negb (okey_eqb (entry_key s e1) (entry_key s e2)) || osig_eqb (entry_sig s e1) (entry_sig s e2)) es) es.

Lemma unifies_b_sound : forall s es, unifies_b s es = true -> Unifies s es.
Proof.
  intros s es H e1 e2 k H1 H2 K1 K2. unfold unifies_b in H.
  rewrite forallb_forall in H. specialize (H e1 H1). rewrite forallb_forall in H. specialize (H e2 H2).
  rewrite K1, K2 in H. simpl in H. rewrite (proj2 (item_eqb_eq k k) eq_refl) in H. simpl in H.
  destruct (entry_sig s e1) as [a|], (entry_sig s e2) as [b|]; try discriminate; [|reflexivity].
  apply fsig_eqb_eq in H. subst. reflexivity.
Qed.

Lemma mgm_nil : forall es, unifies_b [] es = true -> MostGeneralMerge [] es.
Proof.
  intros es H. split; [apply unifies_b_sound; exact H|]. split.
  - intro t. rewrite !app_subst_nil. reflexivity.
  - intros s' _ t. rewrite app_subst_nil. reflexivity.
Qed.

(* the needs of a telescope, in order of first occurrence *)
Fixpoint keys_of (s : subst) (es : list entry) (acc : list item) : list item :=
  match es with
  | [] => acc
  | e :: r => match entry_key s e with
              | Some k => keys_of s r (if existsb (item_eqb k) acc then acc else acc ++ [k])
              | None => keys_of s r acc
              end
  end.

Lemma keys_of_spec : forall s es acc,
  NoDup acc ->
  NoDup (keys_of s es acc) /\
  (forall k, In k (keys_of s es acc) <-> In k acc \/ exists e, In e es /\ entry_key s e = Some k).
Proof.
  intros s es. induction es as [|e r IH]; intros acc Hacc; simpl.
  - split; [exact Hacc|]. intro k. split; [auto|]. intros [H|[e [[] _]]]. exact H.
  - destruct (entry_key s e) as [k0|] eqn:Ek.
    + destruct (existsb (item_eqb k0) acc) eqn:Ex.
      * destruct (IH acc Hacc) as [N I]. split; [exact N|]. intro k. rewrite I. split.
        -- intros [H|(e' & He' & Hk)]; [auto|right; eauto].
        -- intros [H|(e' & [<-|He'] & Hk)]; [auto| |right; eauto].
           left. rewrite Ek in Hk. injection Hk as <-. apply existsb_exists in Ex as (x & Hx & Eq).
           apply item_eqb_eq in Eq. subst. exact Hx.
      * assert (Hn : NoDup (acc ++ [k0])).
        { apply NoDup_app; [exact Hacc | repeat constructor; auto |].
          intros x Hx [Hk|[]]. subst x. assert (existsb (item_eqb k0) acc = true)
            by (apply existsb_exists; exists k0; split; [exact Hx | apply item_eqb_eq; reflexivity]).
          congruence. }
        destruct (IH _ Hn) as [N I]. split; [exact N|]. intro k. rewrite I. rewrite in_app_iff. simpl. split.
        -- intros [[H|[<-|[]]]|(e' & He' & Hk)]; [auto|right; eauto|right; eauto].
        -- intros [H|(e' & [<-|He'] & Hk)]; [auto| |right; eauto].
           rewrite Ek in Hk. injection Hk as <-. auto.
    + destruct (IH acc Hacc) as [N I]. split; [exact N|]. intro k. rewrite I. split.
      * intros [H|(e' & He' & Hk)]; [auto|right; eauto].
      * intros [H|(e' & [<-|He'] & Hk)]; [auto| congruence |right; eauto].
Qed.

Lemma needs_keys_of : forall s es, Needs s es (keys_of s es []).
Proof.
  intros s es. destruct (keys_of_spec s es [] (NoDup_nil _)) as [N I]. split; [exact N|].
  intro k. rewrite I. split; [intros [[]|H]; exact H | intro H; right; exact H].
Qed.

(* ===================== Satisfaction, decided ========================== *)

Definition provides_b (C : cx) (i : item) : bool :=
  match i with
  | IVal v => is_some (nlook v (c_vals C))
  | IFn f => is_some (nlook f (c_fns C))
  | IMth r m _ => is_some (mlook r m (c_mths C))
  | IFun u => is_some (nlook u (c_funs C))
  | ITy t => is_some (nlook t (c_ty C))
  end.

Lemma provides_b_sound : forall C i, provides_b C i = true -> provides C i.
Proof.
  intros C [t|v|f|r m th|u]; simpl; intros H E; rewrite E in H; discriminate.
Qed.

Definition oty_eqb (a b : option ty) : bool :=
  match a, b with Some x, Some y => ty_eqb x y | None, None => true | _, _ => false end.

Lemma oty_eqb_eq : forall a b, oty_eqb a b = true -> a = b.
Proof.
  intros [x|] [y|]; simpl; intro H; try discriminate; [|reflexivity].
  apply ty_eqb_eq in H. subst. reflexivity.
Qed.

Definition coh_b (C Cs : cx) (ns : list item) : bool :=
  forallb (fun k =>
    match ilook k (c_apps Cs) with
    | None => true
    | Some a =>
        forallb (fun q => match q with
                          | (t, Some want) =>
                              match ilook (at_site C k) (c_apps C) with
                              | Some have => existsb (fun h => Nat.eqb (fst h) t &&
                                                       oty_eqb (snd h) (Some (canon C (Audit.app_subst (c_ty C) want)))) have
                              | None => true
                              end
                          | (_, None) => true
                          end) a
    end) ns.

Lemma satisfies_by : forall C Cs ns,
  forallb (fun k => provides_b C (at_site C k)) ns = true ->
  coh_b C Cs ns = true ->
  c_sub Cs = [] ->
  Satisfies C Cs ns (map (fun k => (k, at_site C k)) ns).
Proof.
  intros C Cs ns Hp Hc Hs. split; [|split; [|split]].
  - rewrite map_map. simpl. apply map_id.
  - intros k k' Hin. apply in_map_iff in Hin as (k0 & E & Hk0). injection E as <- <-.
    split; [reflexivity|]. apply provides_b_sound. rewrite forallb_forall in Hp. exact (Hp k0 Hk0).
  - intros k k' a Hin Ha t want Hw. apply in_map_iff in Hin as (k0 & E & Hk0). injection E as <- <-.
    unfold coh_b in Hc. rewrite forallb_forall in Hc. specialize (Hc k0 Hk0). rewrite Ha in Hc.
    rewrite forallb_forall in Hc. specialize (Hc _ Hw). simpl in Hc.
    destruct (ilook (at_site C k0) (c_apps C)) as [have|]; [|exact I].
    apply existsb_exists in Hc as ([t' o] & Hh & E). apply andb_true_iff in E as [E1 E2].
    apply Nat.eqb_eq in E1. apply oty_eqb_eq in E2. simpl in E1, E2. subst. exact Hh.
  - intros g t H. rewrite Hs in H. destruct H.
Qed.

(* ===================== Tactics ======================================== *)

(* `l = r` where l computes (no evars) and r may hold evars *)
Ltac comp :=
  match goal with
  | |- ?l = ?r => let v := eval vm_compute in l in unify r v; vm_compute; reflexivity
  end.

Ltac compne := try unfold Named; let HH := fresh "HH" in intro HH; vm_compute in HH; discriminate HH.

Ltac reduce_list l :=
  let v := eval vm_compute in l in change l with v.

Ltac forall2_args :=
  match goal with |- Forall2 _ ?l _ => reduce_list l end;
  repeat (refine (@Forall2_cons _ _ _ _ (_, _) _ _ _ _); [simpl; split; [reflexivity | comp] |]);
  apply Forall2_nil.

Ltac tyof :=
  first
  [ apply TyUnit | apply TyNever | apply TyThis
  | (eapply TyType; [comp | comp | comp])
  | (eapply TyNom; [comp | comp | (left; reflexivity) || (right; reflexivity) | apply BrNone | forall2_args])
  | (eapply TyAlias; [comp | comp | comp | apply BrNone | tyof]) ].

Ltac forall2_tyof :=
  cbn [sg_params sg_ret];
  match goal with |- Forall2 _ ?l ?l2 => reduce_list l; tryif has_evar l2 then idtac else reduce_list l2 end;
  repeat (apply Forall2_cons; [simpl; tyof |]); apply Forall2_nil.

Ltac open_sig :=
  match goal with
  | |- SigOf _ _ _ _ ?g =>
      is_evar g;
      let ps := fresh "ps" in let r := fresh "r" in
      evar (ps : list ty); evar (r : ty); unify g (FSig ps r); subst ps r
  | _ => idtac
  end.

Ltac sigof :=
  open_sig; do 6 eexists; split; [comp|]; split; [comp|]; split;
  [ forall2_tyof
  | simpl; first [ reflexivity | tyof ] ].

Ltac appof :=
  match goal with
  | |- AppOf ?W ?s ?tm ?a =>
      exists (w_reqs W s); split; [apply incl_refl | split; [ reflexivity | intros _; reflexivity ] ]
  end.

Ltac recvty :=
  unfold RecvTy;
  match goal with |- context [nlook ?r ?tm] => let v := eval vm_compute in (nlook r tm) in change (nlook r tm) with v end;
  cbv iota beta;
  first [ reflexivity | (eexists; split; [reflexivity | forall2_args]) ].

Ltac methodnamed :=
  first [ (left; unfold Attached; comp) | (right; split; [intros ? HA; unfold Attached in HA; vm_compute in HA; discriminate HA | comp]) ].

Ltac item :=
  first
  [ (eapply IEType; [comp | comp])
  | (eapply IEMeth; [comp | methodnamed | apply BrNone | recvty | sigof | appof])
  | (eapply IEVal; [comp | comp | comp | apply BrNone | simpl; tyof | appof])
  | (eapply IEFn; [comp | comp | apply BrNone | sigof | appof])
  | (eapply IEFun; [comp | comp | comp])
  | (eapply IECtx; [comp | comp | comp | apply BrNone | items])
  | (eapply IENothing; [comp | comp | first [ left; reflexivity | right; left; reflexivity
                                            | right; right; left; reflexivity
                                            | right; right; right; split; [reflexivity | do 4 eexists; split; comp] ] ]) ]
with items :=
  match goal with |- ItemsEntries _ ?p ?tm ?l _ _ => reduce_list p; reduce_list tm; reduce_list l end;
  first [ apply IsNil | (eapply IsCons; [item | items]) ].

(* the context a symbol's assumes form, with nothing to merge *)
Ltac ctxof :=
  eexists; eexists; eexists; exists [];
  split; [comp|];
  split; [items|];
  split; [apply mgm_nil; vm_compute; reflexivity|];
  split; [reflexivity | apply needs_keys_of].


(* contexts proved once, by name (RelExamples.v redefines this with ::=) *)
Ltac ctx_tac := ctxof.

Ltac locals_none :=
  let n := fresh "n" in let E := fresh "E" in
  intros n E; first [ discriminate E | injection E as <-; comp ].

Ltac fits_tac := first [ left; vm_compute; reflexivity | right; left; vm_compute; reflexivity ].

Ltac opt_fits_tac := first [ exact I | simpl; fits_tac ].

Ltac nodup_tac :=
  vm_compute; repeat (apply NoDup_cons; [simpl; intuition discriminate |]); apply NoDup_nil.

Ltac numbering :=
  split; [nodup_tac|]; split; [nodup_tac|];
  let i := fresh "i" in let pn := fresh "pn" in let H := fresh "H" in
  intros i pn H;
  do 6 (destruct i as [|i]; [simpl in H; first [ discriminate H | injection H as <-; comp ] |]);
  simpl in H; destruct i; discriminate H.

Ltac ex :=
  first
  [ apply ExUnit
  | (eapply ExLocal; [comp | compne])
  | (eapply ExUnitTag; [locals_none | comp | comp])
  | (eapply ExVal; [locals_none | comp | comp | comp | comp])
  | (eapply ExCallA; [comp | comp | do 3 eexists; comp | comp | args])
  | (eapply ExCallD; [comp | comp | do 4 eexists; comp | ctx_tac | satisfies | sigof | args])
  | (eapply ExTag; [comp | comp | apply BrNone | forall2_args | payload | reflexivity | ex | fits_tac]) ]
with args := first [ apply ArgsNil | (eapply ArgsCons; [ex | fits_tac | args]) ]
with payload := eapply PayTag; [comp | comp | simpl; tyof]
with satisfies :=
  apply satisfies_by; [vm_compute; reflexivity | vm_compute; reflexivity | vm_compute; reflexivity].

Ltac fnprov :=
  first
  [ (eapply FPDefined; [comp | comp | do 4 eexists; comp | ctx_tac | satisfies | sigof]) ].

Ltac bind_item :=
  first
  [ (eapply BTypeBind; [comp | comp | simpl; tauto | tyof])
  | (eapply BValBind; [comp | comp | comp | simpl; tyof | ex | fits_tac | appof])
  | (eapply BFnBind; [comp | comp | sigof | fnprov | appof])
  | (eapply BApplyBody; [comp | comp | comp | simpl; tauto | binds]) ]
with binds :=
  match goal with |- BindItems _ _ _ _ ?l _ _ _ _ => reduce_list l end;
  first [ apply BINil | (eapply BICons; [bind_item | binds]) ].

Ltac blk :=
  first
  [ (eapply BlkTail; ex)
  | apply BlkEmpty
  | (eapply BlkExprLast; ex)
  | (eapply BlkExpr; [ (left; discriminate) || (right; discriminate) | ex | blk ])
  | (eapply BlkLet; [constructor | simpl; reflexivity | ex | simpl; reflexivity | compne | blk])
  | (eapply BlkBind; [binds | blk]) ].

Ltac blke := eapply BlkECheck; [blk | intros ? [] | opt_fits_tac].

(* FnElab for a plain fn with a body, given its module, return type and numbering *)
Ltac fnelab m ret vars :=
  let d := fresh "d" in let Hd := fresh "Hd" in let Ha := fresh "Ha" in
  intros d ? ? ? ? ? Hd Ha; vm_compute in Hd; injection Hd as <-; simpl in Ha;
  injection Ha as <- <- <- <- <-;
  eexists; eexists; exists None; eexists; exists (Frame m ret None false vars [] [] []); eexists; eexists;
  split; [ctx_tac|]; split; [reflexivity|]; split; [sigof|];
  split; [reflexivity|]; split; [reflexivity|]; split; [reflexivity|]; split; [reflexivity|];
  split; [reflexivity|]; split; [reflexivity|]; split; [reflexivity|];
  split; [numbering|];
  split; [simpl; blke | comp].


(* The static reading of a bundle the elaborator rejects: its loader's
   world and every source symbol's requirements, without elaborating any
   body. *)
Definition run_world (x : Input.sx) : Input.res st :=
  match read_bundle x with
  | Good b =>
      match load_world b with
      | Good w =>
          let s0 := St w (w_syms w) (w_nsyms w) NE NE NE [] [] NE [] [] NE NE [] 0 false false []
                       (lang_modules w) [] in
          Good (fold_left (fun s i => match reqs i s with Good (_, s') => s' | Bad _ => s end)
                          (seq 1 (w_nsyms w)) s0)
      | Bad m => Bad m
      end
  | Bad m => Bad m
  end.
