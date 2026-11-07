(* Raw provenance of actual fn_info cache entries. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore ReqsSolve ReqsStable.
From NM Require Export FnInfoCore.
Import ListNotations.
Record FrL (s s' : st) : Prop := {
  fl_w : st_w s' = st_w s;
  fl_flds : forall f n, sassoc f (st_flds s) = Some n -> sassoc f (st_flds s') = Some n;
  fl_unsup : exists l, st_unsup s' = st_unsup s ++ l;
  fl_lang : st_lang s' = st_lang s;
  fl_reqs : Ext (st_reqs s) (st_reqs s');
  fl_keep : forall i g, nget (st_syms s) i = Some g -> nget (st_syms s') i = Some g;
  fl_consts : forall k r, nget (st_consts s) k = Some r -> nget (st_consts s') k = Some r;
  fl_fn_rows : forall row, In row (st_fns s) -> In row (st_fns s')
}.

Lemma frl_of_fr : forall s s', Fr s s' -> FrL s s'.
Proof.
  intros s s' [[Fw _ Fk _ _ Ffl [_ _ (rows & Erows) _ Fu Fl Fc]] _ Fx]. constructor; try assumption.
  intros row H. rewrite Erows. apply in_or_app. right; exact H.
Qed.

Lemma frl_trans : forall s1 s2 s3, FrL s1 s2 -> FrL s2 s3 -> FrL s1 s3.
Proof.
  intros s1 s2 s3 [W1 F1 (u1 & U1) L1 X1 K1 C1 B1] [W2 F2 (u2 & U2) L2 X2 K2 C2 B2]. constructor.
  - congruence.
  - auto.
  - exists (u1 ++ u2). rewrite U2, U1, app_assoc. reflexivity.
  - congruence.
  - exact (ext_trans _ _ _ X1 X2).
  - auto.
  - auto.
  - auto.
Qed.

Definition NL (s : st) (x : nat) : Prop :=
  forall b, load_world b = Good (st_w s) -> nget (w_syms (st_w s)) x = None.

(* x is a symbol the elaborator made that is not a forwarder (a lambda
   provider's) *)
Definition NLx (s : st) (x : nat) : Prop :=
  NL s x /\ exists g, nget (st_syms s) x = Some g /\ forall t, gs_origin g <> OForwarder t.

Lemma nlx_frl : forall s s' x, NLx s x -> FrL s s' -> NLx s' x.
Proof.
  intros s s' x [N (g & G & O)] F. split; [unfold NL; rewrite (fl_w _ _ F); exact N|].
  exists g. split; [exact (fl_keep _ _ F _ _ G) | exact O].
Qed.

(* an entry of the cache: fn_info's merged walk of the symbol table's entry,
   or (lambda_provider's) an entry for a symbol the elaborator made, not a
   forwarder *)
Definition InfoEntry (s : st) (x : nat) (i : fninfo) : Prop :=
  (exists g s0 e needs s1, nget (st_syms s0) x = Some g /\ Inv s0 /\
    merged_walk (gs_mod g) (fi_walk g) s0 = Good ((e, needs), s1) /\
    i = FInfo (freeze_all_apps e) needs
              (map (fun k => (k, match get_app (freeze_all_apps e) k with Some (a, _) => a | None => [] end)) needs)
              (merge_edges e) /\
    FrL s1 s) \/ NLx s x.

Definition FnCacheInv (s : st) : Prop := forall x i, nget (st_info s) x = Some i -> InfoEntry s x i.

(* Creator receipts survive cache hits and later source emissions.  No body
   identity or signature is guessed from the wrapper-cache key alone. *)
Definition WrapperEntry(s:st)(provider id:nat):Prop:=
 exists pv info th sm smstart smend thstart thend newstart newend,
 gs_id pv=provider /\ nget(st_syms s)provider=Some pv /\ InfoEntry s provider info /\
 Inv smstart /\ smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))smstart=Good(sm,smend) /\ FrL smend s /\
 Inv thstart /\ call_theta(fi_env info)pv thstart=Good(th,thend) /\ FrL thend s /\
 Inv newstart /\ nget(st_syms newstart)provider=Some pv /\
 new_sym(gs_mod pv)(gs_name pv++"$recv")%string KFn(gs_decl pv)OLambda newstart=Good(id,newend) /\ FrL newend s /\
 nget(st_syms s)id=Some(GSym id(gs_mod pv)(gs_name pv++"$recv")%string KFn(gs_decl pv)None[]OLambda) /\
 In(id,XCallD provider th sm(map XVar(seq 1(List.length(fn_params pv)))))(st_fns s).

Record InfoInv(s:st):Prop:={
 info_functions:forall x i,nget(st_info s)x=Some i->InfoEntry s x i;
 info_wrappers:forall provider id,nget(st_wrappers s)provider=Some id->WrapperEntry s provider id
}.
Coercion info_functions : InfoInv >-> Funclass.

Lemma info_entry_keep:forall s s' x i,InfoEntry s x i->FrL s s'->InfoEntry s' x i.
Proof.
 intros s s' x i[(g&s0&e&needs&s1&H1&H2&H3&H4&H5)|N]F.
 - left;exists g,s0,e,needs,s1;repeat(split;[assumption|]);exact(frl_trans _ _ _ H5 F).
 - right;exact(nlx_frl _ _ _ N F).
Qed.
Lemma wrapper_entry_keep:forall s s' p id,WrapperEntry s p id->FrL s s'->WrapperEntry s' p id.
Proof.
 intros s s' p id(pv&info&th&sm&sms&sme&ths&the&news&newe&Id&Physical&Entry&Is&Map&Fs&It&Theta&Ft&Inw&Phnew&New&Fn&Symbol&Row)F.
 exists pv,info,th,sm,sms,sme,ths,the,news,newe.
 split;[exact Id|];split;[eapply fl_keep;eassumption|];split;[eapply info_entry_keep;eassumption|].
 split;[exact Is|];split;[exact Map|];split;[eapply frl_trans;eassumption|].
 split;[exact It|];split;[exact Theta|];split;[eapply frl_trans;eassumption|].
 split;[exact Inw|];split;[exact Phnew|];split;[exact New|];split;[eapply frl_trans;eassumption|].
 split;[eapply fl_keep;eassumption|];eapply fl_fn_rows;eassumption.
Qed.
