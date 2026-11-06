(* ------------------------------------------------------------------------ *)
(* XExample.v — a well-formed MossX program, so `xwf` is not vacuous.        *)
(*                                                                           *)
(* The program binds three primitives as providers (memory.size, i32.add,   *)
(* wasistd's i32_bool), builds a struct over an I32, reads its field, runs  *)
(* a `while` whose body assigns a local and breaks, and matches on a Bool:  *)
(*                                                                           *)
(*   fn main() {                                                             *)
(*     bind size = memory_size; bind add = i32_add; bind bool = i32_bool;   *)
(*     let x = size();  let p = Pt { f: x };                                 *)
(*     while bool(p.f) { x = add(x, size()); break; }                       *)
(*     match bool(x) { Bool => () }                                          *)
(*   }                                                                       *)
(*                                                                           *)
(* It is typed (`example_wf`), so XSafety applies (`example_safe`).         *)
(* XExampleRef, below, does the same for a program on the GC heap.         *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith PrimInt63 Uint63 PArray.
From NM Require Import Audit CoreMoss XTyping XEnv XSafety.
From NM Require MossX.
Import ListNotations.

Module XExample.

Definition nBool : nsym := 1.
Definition nTrue : nsym := 2.
Definition nFalse : nsym := 3.
Definition nI32 : nsym := 4.
Definition nPt : nsym := 5.
Definition fMain : fsym := 1.
Definition fSize : fsym := 2.
Definition fAdd : fsym := 3.
Definition fBool : fsym := 4.
Definition fldF : fld := 0.

Definition I32 : ty := TNom nI32 [].
Definition Bool : ty := TNom nBool [].
Definition Pt : ty := TNom nPt [].

Definition Sg : gsig := mkGsig
  (fun _ => false)
  (fun n =>
     if Nat.eqb n nBool then Some ([], TUnion [TNom nTrue []; TNom nFalse []])
     else if Nat.eqb n nTrue then Some ([], TUnit)
     else if Nat.eqb n nFalse then Some ([], TUnit)
     else if Nat.eqb n nI32 then Some ([], TUnit)
     else if Nat.eqb n nPt then Some ([], TNom rec_nom [(fldF, I32)])
     else None)
  (fun _ => None)
  (fun f =>
     if Nat.eqb f fMain then Some ([], mkSig [] TUnit, None)
     else if Nat.eqb f fSize then Some ([], mkSig [] I32, None)
     else if Nat.eqb f fAdd then Some ([], mkSig [I32; I32] I32, None)
     else if Nat.eqb f fBool then Some ([], mkSig [I32] Bool, None)
     else None)
  (fun _ => None)
  (fun _ => None)
  (fun _ => []) (fun _ => []) (fun _ => ([], [])).

Definition Rp (n : nsym) : option rep :=
  if Nat.eqb n nI32 then Some RI32
  else None.

Definition main_body : xexpr :=
  XBindPrim (IFn fSize) PMemSize
  (XBindPrim (IFn fAdd) (PI32Bin IAdd)
  (XBindPrim (IFn fBool) PI32Bool
  (XLet 0 (XCallA fSize [])
  (XLet 1 (XTag nPt [] (XRec [(fldF, XVar 0)]))
  (XSeq (XWhile (XCallA fBool [XField (XVar 1) fldF])
                (XSeq (XAssign 0 (XCallA fAdd [XVar 0; XCallA fSize []])) XBreak))
        (XMatchP (XCallA fBool [XVar 0]) [(XPHead nBool None [], XUnit)])))))).

Definition Fb (_ : usym) : option (tele * tele * subst) := None.

Definition P : xprog :=
  mkXProg (fun f => if Nat.eqb f fMain then Some main_body else None)
          (fun _ => None) nBool nTrue nFalse [] (fun _ => None) [] (fun _ => []) (fun _ => []) false (fun _ => []) (fun _ => []).

(* --- Formation. ----------------------------------------------------------- *)

Lemma tele_nil : tele_ctx [] = Some empty_ctx.
Proof. reflexivity. Qed.

Lemma sat_nil : forall Phi, sat Sg Phi [] [].
Proof.
  intros Phi. exists empty_ctx. split; [reflexivity|].
  split; [reflexivity | split; constructor].
Qed.

Lemma sat0_nil : forall Phi, sat0 Sg Phi [].
Proof.
  intros Phi. exists empty_ctx. split; [reflexivity|].
  split; [split; [reflexivity | split; constructor] | constructor].
Qed.

Lemma wf_nom : forall Phi n, (exists tp, g_tag Sg n = Some ([], tp)) -> wf_ty Sg Phi (TNom n []).
Proof.
  intros Phi n [tp H]. eapply WfTag; [exact H | apply sat_nil | constructor].
Qed.

Lemma wf_I32 : forall Phi, wf_ty Sg Phi I32.
Proof. intros. apply wf_nom. eexists; reflexivity. Qed.

Lemma wf_Bool : forall Phi, wf_ty Sg Phi Bool.
Proof. intros. apply wf_nom. eexists; reflexivity. Qed.

Lemma wf_Pt : forall Phi, wf_ty Sg Phi Pt.
Proof. intros. apply wf_nom. eexists; reflexivity. Qed.

Lemma wf_sig : wf_gsig Sg.
Proof.
  constructor.
  - intros n D taup H. exists empty_ctx. unfold Sg in H. cbn [mkGsig g_tag] in H.
    destruct (Nat.eqb n nBool) eqn:E1;
      [| destruct (Nat.eqb n nTrue) eqn:E2;
         [| destruct (Nat.eqb n nFalse) eqn:E3;
            [| destruct (Nat.eqb n nI32) eqn:E4;
               [| destruct (Nat.eqb n nPt) eqn:E5; [| discriminate]]]]];
      injection H as <- <-; (split; [constructor|apply (@wf_ty_frees Sg empty_ctx)]); try (apply WfUnit || apply WfUnion).
    + constructor; [apply wf_nom; eexists; reflexivity |].
      constructor; [apply wf_nom; eexists; reflexivity | constructor].
    + repeat constructor; eexists; eexists; reflexivity.
    + simpl. apply NoDup_cons; [intros [Hx | []]; discriminate Hx | apply NoDup_cons; [intros [] | constructor]].
    + apply WfRec; [reflexivity | constructor; [apply wf_I32 | constructor]].
  - intros v D tauv H. discriminate H.
  - intros f D S b H. exists empty_ctx. unfold Sg in H. cbn [mkGsig g_fn] in H.
    destruct (Nat.eqb f fMain) eqn:E1;
      [| destruct (Nat.eqb f fSize) eqn:E2;
         [| destruct (Nat.eqb f fAdd) eqn:E3;
            [| destruct (Nat.eqb f fBool) eqn:E4; [| discriminate]]]];
      injection H as <- <- <-;
      (split; [constructor|]);
      (split; [repeat constructor; apply wf_I32 |]);
      (split; [simpl; first [constructor | apply wf_I32 | apply wf_Bool] |]);
      intros body Hb; discriminate.
  - intros m D S H. discriminate H.
  - intros u Du Dr ob H. discriminate H.
  - intros n D Impossible;discriminate Impossible.
Qed.

(* --- The body's typing. --------------------------------------------------- *)

Definition Phi3 : ctx :=
  add_item (IFn fBool) (add_item (IFn fAdd) (add_item (IFn fSize) empty_ctx)).

Definition G2 : tenv := [(1, Pt); (0, I32)].

Lemma call_size : forall R L G, xhas Sg Rp P Fb R L Phi3 G (XCallA fSize []) I32.
Proof.
  intros. change I32 with (app_subst (cS Phi3) (fs_ret (mkSig [] I32))).
  eapply XtNeed; [reflexivity | reflexivity | constructor].
Qed.

Lemma field_f : forall R L, xhas Sg Rp P Fb R L Phi3 G2 (XField (XVar 1) fldF) I32.
Proof.
  intros. change I32 with (app_subst [] I32).
  eapply XtField; [apply XtVar; reflexivity | apply wf_Pt | reflexivity | reflexivity | reflexivity | discriminate | reflexivity].
Qed.

Lemma main_typed :
  xhas Sg Rp P Fb (Some TUnit) false empty_ctx [] main_body TUnit.
Proof.
  unfold main_body.
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; left; eexists; eexists; split; [left; reflexivity | split; [constructor | eexists; eexists; split; reflexivity]] |].
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; left; eexists; eexists; split; [left; reflexivity |
      split; [repeat constructor; eexists; eexists; split; reflexivity | eexists; eexists; split; reflexivity]] |].
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; left; eexists; eexists; split; [left; reflexivity |
      split; [repeat constructor; eexists; eexists; split; reflexivity | split; [discriminate|reflexivity]]] |].
  fold Phi3.
  eapply XtLet; [apply call_size | intros u [] |].
  eapply XtLet with (tau1 := Pt).
  { change Pt with (app_subst (cS Phi3) (TNom nPt [])).
    eapply XtTag; [reflexivity | reflexivity | apply sat_nil | constructor | discriminate |]. cbn.
    apply XtRec; [reflexivity | constructor; [apply XtVar; reflexivity | constructor]]. }
  { intros u []. }
  fold G2.
  eapply XtSeq.
  - eapply XtWhile with (tb := TUnit).
    + change (bool_ty P) with (app_subst (cS Phi3) (fs_ret (mkSig [I32] Bool))).
      eapply XtNeed; [reflexivity | reflexivity |].
      constructor; [apply field_f | constructor].
    + eapply XtSeq; [| apply XtBreak].
      eapply XtAssign; [reflexivity |].
      change I32 with (app_subst (cS Phi3) (fs_ret (mkSig [I32; I32] I32))).
      eapply XtNeed; [reflexivity | reflexivity |].
      constructor; [apply XtVar; reflexivity | constructor; [apply call_size | constructor]].
  - eapply XtMatchP with (tau0 := Bool) (Gbs := [[]]).
    + change Bool with (app_subst (cS Phi3) (fs_ret (mkSig [I32] Bool))).
      eapply XtNeed; [reflexivity | reflexivity |].
      constructor; [apply XtVar; reflexivity | constructor].
    + apply wf_Bool.
    + intros ms Hm. injection Hm as <-. repeat constructor. intros [].
    + right. exists [Bool]. split; [reflexivity|].
      constructor; [| constructor].
      exists nBool, []. split; [reflexivity|]. split; [left;reflexivity|]. split; [discriminate|].
      exists None, [], XUnit. left. reflexivity.
    + constructor; [| constructor].
      exact (@PoTag Sg Rp P Bool Bool nBool [] [Bool] [] (TUnion [TNom nTrue []; TNom nFalse []]) None
               eq_refl (or_introl eq_refl) eq_refl eq_refl ltac:(discriminate)).
    + repeat constructor.
    + constructor; [apply XtUnit | constructor].
Qed.

Theorem example_wf : xwf Sg Rp P Fb.
Proof.
  constructor.
  - exact wf_sig.
  - intros f D S b H. unfold Sg in H. simpl in H.
    repeat (destruct (Nat.eqb _ _); [injection H as _ _ <-; reflexivity |]). discriminate.
  - intros u Du Dr b H. discriminate H.
  - intros _ h Dh Dc pins Dr bs H. discriminate H.
  - intros Bad;discriminate Bad.
  - intros Bad;discriminate Bad.
  - intros f D S body Hf Hb. unfold P in Hb. simpl in Hb.
    destruct (Nat.eqb f fMain) eqn:E; [| discriminate].
    apply Nat.eqb_eq in E. subst f. injection Hb as <-.
    unfold Sg in Hf. simpl in Hf. injection Hf as <- <-.
    exists empty_ctx. split; [constructor | exact main_typed].
  - right;split;[reflexivity|]. split;[reflexivity|]. split;[reflexivity|]. split.
    + exists [],[TNom nTrue [];TNom nFalse []];split;[reflexivity|].
      split;[intros x H;exact H|]. split;[left;reflexivity|right;left;reflexivity].
    + split;exists [];reflexivity.
  - split; reflexivity.
  - intros n spec H;discriminate H.
  - intros n spec D pay H;discriminate H.
  - intros h Dh Dc pins H;discriminate H.
  - intros n spec h expected H;discriminate H.
Qed.

(* So no run of it gets stuck, on the oracle's interpreter. *)
Corollary example_safe : forall fuel st c,
  MossX.XS.xr_out (MossX.XS.run_main fuel P fMain [] st) <> XErr c.
Proof.
  intros fuel st c.
  eapply (@mossx_run_main_safe Sg Rp P Fb example_wf fMain [] (mkSig [] TUnit) main_body
            empty_ctx [] []); try reflexivity.
  - constructor.
  - split; [constructor|]. split; [intros t tau Ht; discriminate Ht|].
    split; [intros t []|]. split; [split; [intros x [] | constructor] | intros []].
  - split; [split; [constructor | split; [intros i en [] | constructor]]|intros Reify t []].
Qed.

End XExample.

(* ------------------------------------------------------------------------ *)
(* A program on the GC heap, so the heap part of XSafety is not vacuous: it *)
(* binds the `Ref` primitives at `Ref[I32]` (rep RRef), allocates a box,     *)
(* writes it and reads it back:                                             *)
(*                                                                           *)
(*   fn main() {                                                             *)
(*     bind size = memory_size; bind box = Ref.new; bind get = Ref.get;     *)
(*     bind set = Ref.set;                                                   *)
(*     let r = box(size());  set(r, add(size(), size()));  let x = get(r);  *)
(*   }                                                                       *)
(*                                                                           *)
(* Its typing (`ref_wf`) uses gc_shape for each GC primitive; safety from   *)
(* the empty store typing (`ref_safe`) comes from mossx_typed_safe; and a  *)
(* short run on a small machine (`ref_runs`) allocates the box and leaves  *)
(* the written value in it.                                                  *)
(* ------------------------------------------------------------------------ *)

Module XExampleRef.

Definition nBool : nsym := 1.
Definition nTrue : nsym := 2.
Definition nFalse : nsym := 3.
Definition nI32 : nsym := 4.
Definition nRef : nsym := 5.
Definition nHandle : nsym := 6.
Definition tX : tsym := 1.
Definition fMain : fsym := 1.
Definition fSize : fsym := 2.
Definition fBox : fsym := 3.
Definition fGet : fsym := 4.
Definition fSet : fsym := 5.
Definition fAdd : fsym := 6.

Definition I32 : ty := TNom nI32 [].
Definition RefI : ty := TNom nRef [(tX, I32)].

Definition Sg : gsig := mkGsig
  (fun t => Nat.eqb t tX)
  (fun n =>
     if Nat.eqb n nBool then Some ([], TUnion [TNom nTrue []; TNom nFalse []])
     else if Nat.eqb n nTrue then Some ([], TUnit)
     else if Nat.eqb n nFalse then Some ([], TUnit)
     else if Nat.eqb n nI32 then Some ([], TUnit)
     else if Nat.eqb n nRef then Some ([ITy tX], TNom nHandle [])
     else if Nat.eqb n nHandle then Some ([],TUnit)
     else None)
  (fun _ => None)
  (fun f =>
     if Nat.eqb f fMain then Some ([], mkSig [] TUnit, None)
     else if Nat.eqb f fSize then Some ([], mkSig [] I32, None)
     else if Nat.eqb f fBox then Some ([], mkSig [I32] RefI, None)
     else if Nat.eqb f fGet then Some ([], mkSig [RefI] I32, None)
     else if Nat.eqb f fSet then Some ([], mkSig [RefI; I32] TUnit, None)
     else if Nat.eqb f fAdd then Some ([], mkSig [I32; I32] I32, None)
     else None)
  (fun _ => None)
  (fun _ => None)
  (fun _ => []) (fun _ => []) (fun _ => ([], [])).

Definition Rp (n : nsym) : option rep :=
  if Nat.eqb n nI32 then Some RI32
  else if Nat.eqb n nRef then Some (RRef tX)
  else if Nat.eqb n nHandle then Some (RHandle false)
  else None.

Definition main_body : xexpr :=
  XBindPrim (IFn fSize) PMemSize
  (XBindPrim (IFn fBox) (PBox nRef)
  (XBindPrim (IFn fGet) PDeref
  (XBindPrim (IFn fSet) PRefSet
  (XBindPrim (IFn fAdd) (PI32Bin IAdd)
  (XLet 0 (XCallA fBox [XCallA fSize []])
  (XSeq (XCallA fSet [XVar 0; XCallA fAdd [XCallA fSize []; XCallA fSize []]])
        (XLet 1 (XCallA fGet [XVar 0]) XUnit))))))).

Definition Fb (_ : usym) : option (tele * tele * subst) := None.

Definition P : xprog :=
  mkXProg (fun f => if Nat.eqb f fMain then Some main_body else None)
          (fun _ => None) nBool nTrue nFalse [] (fun _ => None) [(nRef,XCastSpec false tX None)] (fun _ => []) (fun _ => []) false (fun _ => []) (fun _ => []).

(* --- Formation. ----------------------------------------------------------- *)

Lemma sat_nil : forall Phi, sat Sg Phi [] [].
Proof.
  intros Phi. exists empty_ctx. split; [reflexivity|].
  split; [reflexivity | split; constructor].
Qed.

Lemma sat0_nil : forall Phi, sat0 Sg Phi [].
Proof.
  intros Phi. exists empty_ctx. split; [reflexivity|].
  split; [split; [reflexivity | split; constructor] | constructor].
Qed.

Lemma wf_nom : forall Phi n, (exists tp, g_tag Sg n = Some ([], tp)) -> wf_ty Sg Phi (TNom n []).
Proof.
  intros Phi n [tp H]. eapply WfTag; [exact H | apply sat_nil | constructor].
Qed.

Lemma wf_I32 : forall Phi, wf_ty Sg Phi I32.
Proof. intros. apply wf_nom. eexists; reflexivity. Qed.

Lemma wf_RefI : forall Phi, wf_ty Sg Phi RefI.
Proof.
  intros Phi. eapply WfTag; [reflexivity | | repeat constructor; apply wf_I32].
  eexists; split; [reflexivity|].
  split; [reflexivity | split; [repeat constructor; left; reflexivity | constructor]].
Qed.

Lemma wf_sig : wf_gsig Sg.
Proof.
  constructor.
  - intros n D taup H. unfold Sg in H. cbn [mkGsig g_tag] in H.
    destruct (Nat.eqb n nBool) eqn:E1;
      [| destruct (Nat.eqb n nTrue) eqn:E2;
         [| destruct (Nat.eqb n nFalse) eqn:E3;
            [| destruct (Nat.eqb n nI32) eqn:E4;
               [| destruct (Nat.eqb n nRef) eqn:E5;
                  [|destruct(Nat.eqb n nHandle)eqn:E6;[|discriminate]]]]]];
      injection H as <- <-.
    1-4: exists empty_ctx; (split; [constructor|apply (@wf_ty_frees Sg empty_ctx)]); try constructor.
    + constructor; [apply wf_nom; eexists; reflexivity |].
      constructor; [apply wf_nom; eexists; reflexivity | constructor].
    + repeat constructor; eexists; eexists; reflexivity.
    + simpl. apply NoDup_cons; [intros [Hx | []]; discriminate Hx | apply NoDup_cons; [intros [] | constructor]].
    + eexists. split; [| intros x Hx;cbn in Hx;contradiction].
      eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
    + exists empty_ctx;split;[constructor|intros x Hx;contradiction].
  - intros v D tauv H. discriminate H.
  - intros f D S b H. exists empty_ctx. unfold Sg in H. cbn [mkGsig g_fn] in H.
    destruct (Nat.eqb f fMain) eqn:E1;
      [| destruct (Nat.eqb f fSize) eqn:E2;
         [| destruct (Nat.eqb f fBox) eqn:E3;
            [| destruct (Nat.eqb f fGet) eqn:E4;
               [| destruct (Nat.eqb f fSet) eqn:E5;
                  [| destruct (Nat.eqb f fAdd) eqn:E6; [| discriminate]]]]]];
      injection H as <- <- <-;
      (split; [constructor|]);
      (split; [repeat constructor; first [apply wf_I32 | apply wf_RefI] |]);
      (split; [simpl; first [constructor | apply wf_I32 | apply wf_RefI] |]);
      intros body Hb; discriminate.
  - intros m D S H. discriminate H.
  - intros u Du Dr ob H. discriminate H.
  - intros n D Impossible;discriminate Impossible.
Qed.

(* --- The body's typing: each GC primitive by its shape. ------------------ *)

Definition Phi5 : ctx :=
  add_item (IFn fAdd) (add_item (IFn fSet) (add_item (IFn fGet)
    (add_item (IFn fBox) (add_item (IFn fSize) empty_ctx)))).

Definition G1 : tenv := [(0, RefI)].

Lemma call_size : forall R L G, xhas Sg Rp P Fb R L Phi5 G (XCallA fSize []) I32.
Proof.
  intros. change I32 with (app_subst (cS Phi5) (fs_ret (mkSig [] I32))).
  eapply XtNeed; [reflexivity | reflexivity | constructor].
Qed.

Lemma call_add : forall R L G,
  xhas Sg Rp P Fb R L Phi5 G (XCallA fAdd [XCallA fSize []; XCallA fSize []]) I32.
Proof.
  intros. change I32 with (app_subst (cS Phi5) (fs_ret (mkSig [I32; I32] I32))).
  eapply XtNeed; [reflexivity | reflexivity |].
  constructor; [apply call_size | constructor; [apply call_size | constructor]].
Qed.

Lemma call_set : forall R L,
  xhas Sg Rp P Fb R L Phi5 G1
    (XCallA fSet [XVar 0; XCallA fAdd [XCallA fSize []; XCallA fSize []]]) TUnit.
Proof.
  intros. change TUnit with (app_subst (cS Phi5) (fs_ret (mkSig [RefI; I32] TUnit))).
  eapply XtNeed; [reflexivity | reflexivity |].
  constructor; [apply XtVar; reflexivity | constructor; [apply call_add | constructor]].
Qed.

Lemma call_get : forall R L,
  xhas Sg Rp P Fb R L Phi5 G1 (XCallA fGet [XVar 0]) I32.
Proof.
  intros. change I32 with (app_subst (cS Phi5) (fs_ret (mkSig [RefI] I32))).
  eapply XtNeed; [reflexivity | reflexivity |].
  constructor; [apply XtVar; reflexivity | constructor].
Qed.

Lemma ref_contents : contents [(tX, I32)] tX = I32.
Proof. reflexivity. Qed.

Lemma schema_ref : forall th,gc_schema Sg Rp P false nRef th tX.
Proof.
 intros th;exists(XCastSpec false tX None).
 split;[reflexivity|]. split;[reflexivity|]. split;[reflexivity|]. split;[exact I|].
 exists [ITy tX],(TNom nHandle []);split;[reflexivity|].
 apply HtNom;reflexivity.
Qed.
Lemma ref_shape : is_ref Sg Rp P false RefI I32.
Proof.
 exists nRef,[(tX,I32)],tX;split;[reflexivity|]. split;[reflexivity|].
 split;[reflexivity|apply schema_ref].
Qed.

Lemma main_typed :
  xhas Sg Rp P Fb (Some TUnit) false empty_ctx [] main_body TUnit.
Proof.
  unfold main_body.
  (* memory.size: its Part IV signature *)
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; left; eexists; eexists; split; [left; reflexivity | split; [constructor | eexists; eexists; split; reflexivity]] |].
  (* Ref.new at I32 -> Ref[I32] *)
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; right; exists [(tX, I32)], tX; split;[reflexivity|split;[reflexivity|split;[reflexivity|apply schema_ref]]] |].
  (* Ref.get at Ref[I32] -> I32 *)
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; right; exact ref_shape |].
  (* Ref.set at (Ref[I32], I32) -> () *)
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; right; split; [exact ref_shape|reflexivity] |].
  (* i32.add *)
  eapply XtBindPrim; [reflexivity | apply sat0_nil |
    right; left; eexists; eexists; split; [left; reflexivity |
      split; [repeat constructor; eexists; eexists; split; reflexivity | eexists; eexists; split; reflexivity]] |].
  fold Phi5.
  eapply XtLet with (tau1 := RefI).
  { change RefI with (app_subst (cS Phi5) (fs_ret (mkSig [I32] RefI))).
    eapply XtNeed; [reflexivity | reflexivity |].
    constructor; [apply call_size | constructor]. }
  { intros u []. }
  fold G1.
  eapply XtSeq; [apply call_set |].
  eapply XtLet with (tau1 := I32); [apply call_get | intros u [] | apply XtUnit].
Qed.

Theorem ref_wf : xwf Sg Rp P Fb.
Proof.
  constructor.
  - exact wf_sig.
  - intros f D S b H. unfold Sg in H. simpl in H.
    repeat (destruct (Nat.eqb _ _); [injection H as _ _ <-; reflexivity |]). discriminate.
  - intros u Du Dr b H. discriminate H.
  - intros _ h Dh Dc pins Dr bs H. discriminate H.
  - intros Bad;discriminate Bad.
  - intros Bad;discriminate Bad.
  - intros f D S body Hf Hb. unfold P in Hb. simpl in Hb.
    destruct (Nat.eqb f fMain) eqn:E; [| discriminate].
    apply Nat.eqb_eq in E. subst f. injection Hb as <-.
    unfold Sg in Hf. simpl in Hf. injection Hf as <- <-.
    exists empty_ctx. split; [constructor | exact main_typed].
  - right;split;[reflexivity|]. split;[reflexivity|]. split;[reflexivity|]. split.
    + exists [],[TNom nTrue [];TNom nFalse []];split;[reflexivity|].
      split;[intros x H;exact H|]. split;[left;reflexivity|right;left;reflexivity].
    + split;exists [];reflexivity.
  - split; reflexivity.
  - intros n spec H;cbn[lookup_cast P xp_cast]in H.
    destruct(Nat.eqb n nRef)eqn:E;[|discriminate]. apply Nat.eqb_eq in E;subst n.
    injection H as <-;reflexivity.
  - intros n spec D pay H Tag;cbn[lookup_cast P xp_cast]in H.
    destruct(Nat.eqb n nRef)eqn:E;[|discriminate]. apply Nat.eqb_eq in E;subst n.
    injection H as <-;cbn[Sg g_tag nRef nBool nTrue nFalse nI32]in Tag;injection Tag as <- <-.
    split;[left;reflexivity|intros h expected Bad;discriminate Bad].
  - intros h Dh Dc pins H;discriminate H.
  - intros n spec h expected H;cbn[lookup_cast P xp_cast]in H.
    destruct(Nat.eqb n nRef);[|discriminate]. injection H as <-;discriminate.
Qed.

(* --- Safety on the GC heap. ---------------------------------------------- *)

(* From the empty store typing and any machine whose heap agrees with it
   (XHeapC's heap_ok), every run of the body is safe: no XErr, and its
   outcome is typed at an extension of the store typing. *)
Corollary ref_safe : forall st fuel,
  XHeapC.ConcreteHeap.heap_ok (XSafetyC.PsiH Sg Rp P (fun _ => None)) st ->
  XSafetyC.xsafe Sg Rp P (fun _ => None) [] empty_ctx [] (Some TUnit) false TUnit
    (MossX.XS.xinterp fuel P [] [] st main_body).
Proof.
  intros st fuel Hh.
  eapply (@mossx_typed_safe Sg Rp P Fb ref_wf); [exact main_typed | | | | exact Hh].
  - split; [constructor|]. split; [intros t tau Ht; discriminate Ht|].
    split; [intros t []|]. split; [split; [intros x [] | constructor] | intros []].
  - split; [split; [constructor | split; [intros i en [] | constructor]]|intros Reify t []].
  - split; constructor.
Qed.

Corollary ref_main_safe : forall fuel st c,
  MossX.XS.xr_out (MossX.XS.run_main fuel P fMain [] st) <> XErr c.
Proof.
  intros fuel st c.
  eapply (@mossx_run_main_safe Sg Rp P Fb ref_wf fMain [] (mkSig [] TUnit) main_body
            empty_ctx [] []); try reflexivity.
  - constructor.
  - split; [constructor|]. split; [intros t tau Ht; discriminate Ht|].
    split; [intros t []|]. split; [split; [intros x [] | constructor] | intros []].
  - split; [split; [constructor | split; [intros i en [] | constructor]]|intros Reify t []].
Qed.

(* --- A run. ---------------------------------------------------------------- *)

(* A small machine: one page slot, no pages, an empty heap. *)
Definition st0 : MossX.mstate :=
  MossX.MS (PArray.make 1 (PArray.make 1 0%uint63)) 3%uint63 [] [] HTrie.TE 0%uint63 [] 4%uint63.

(* memory.size is 3: the box is allocated at address 0 holding 3, then
   overwritten with 6. *)
Lemma ref_runs :
  MossX.XS.xr_out (MossX.XS.run_main 30 P fMain [] st0) = XOk XVUnit
  /\ MossX.ms_heap (MossX.XS.xr_st (MossX.XS.run_main 30 P fMain [] st0))
     = HTrie.TN HTrie.TE (Some (MossX.HBox (XVI32 6%uint63))) HTrie.TE.
Proof. split; reflexivity. Qed.

End XExampleRef.
