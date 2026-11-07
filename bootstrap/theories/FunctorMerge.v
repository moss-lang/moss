(* Type-only implicit inputs do not change a telescope's merge obligations. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore Merge CanonTypes SourceClean ContextClean CtxWalk CtxMerge.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma unifies_append_types:forall sb es ts,
 A.Unifies sb(es++map A.NTy ts)<->A.Unifies sb es.
Proof.
 intros sb es ts;split.
 - intros H e1 e2 k H1 H2;apply H;apply in_or_app;left;assumption.
 - intros H e1 e2 k H1 H2 K1 K2.
   apply in_app_or in H1 as[H1|H1];apply in_app_or in H2 as[H2|H2].
   + exact(H e1 e2 k H1 H2 K1 K2).
   + apply in_map_iff in H2 as(t&<-&_);discriminate K2.
   + apply in_map_iff in H1 as(t&<-&_);discriminate K1.
   + apply in_map_iff in H1 as(t&<-&_);discriminate K1.
Qed.
Lemma mgm_append_types:forall sb es ts,
 A.MostGeneralMerge sb(es++map A.NTy ts)<->A.MostGeneralMerge sb es.
Proof.
 intros sb es ts;unfold A.MostGeneralMerge;split;intros[U[I M]];split.
 - apply(proj1(unifies_append_types sb es ts));exact U.
 - split;[exact I|]. intros sb' U';apply M;apply(proj2(unifies_append_types sb' es ts));exact U'.
 - apply(proj2(unifies_append_types sb es ts));exact U.
 - split;[exact I|]. intros sb' U';apply M;apply(proj1(unifies_append_types sb' es ts));exact U'.
Qed.
Lemma needs_append_types:forall sb es ts ns,
 A.Needs sb(es++map A.NTy ts)ns<->A.Needs sb es ns.
Proof.
 intros sb es ts ns;unfold A.Needs;split;intros[N H];split;[exact N| |exact N|];intro k;rewrite H;split.
 - intros(e&Hin&K);apply in_app_or in Hin as[Hin|Hin].
   + exists e;split;assumption.
   + apply in_map_iff in Hin as(t&<-&_);discriminate K.
 - intros(e&Hin&K);exists e;split;[apply in_or_app;left;exact Hin|exact K].
 - intros(e&Hin&K);exists e;split;[apply in_or_app;left;exact Hin|exact K].
 - intros(e&Hin&K);apply in_app_or in Hin as[Hin|Hin].
   + exists e;split;assumption.
   + apply in_map_iff in Hin as(t&<-&_);discriminate K.
Qed.
Lemma flat_types_nil:forall X(f:A.entry->list X)ts,
 (forall t,f(A.NTy t)=[])->flat_map f(map A.NTy ts)=[].
Proof. intros X f ts H;induction ts;cbn;[reflexivity|rewrite H,IHts;reflexivity]. Qed.
Lemma entries_cx_append_types:forall sb tm es ts,
 A.entries_cx sb tm(es++map A.NTy ts)=A.entries_cx sb tm es.
Proof.
 intros sb tm es ts;unfold A.entries_cx;repeat rewrite flat_map_app.
 repeat rewrite flat_types_nil by reflexivity;repeat rewrite app_nil_r;reflexivity.
Qed.

Section EquivalentRounds.
Variables (Fl : string -> nat) (walk : env -> M (env * list key)).
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Hypothesis Hok : forall e, ok (walk e).
(* what a round reads: its entries (and types in force), the same in every
   round (IEs is functional) *)
Variable Rd : list A.entry -> list (nat * ty) -> Prop.
Hypothesis Hdet : forall es tm es' tm' sb, Rd es tm -> Rd es' tm' -> A.Unifies sb es <-> A.Unifies sb es'.
(* what the state must satisfy before (kept going forwards) and after
   (kept going back) *)
Variables (Pre Post : st -> Prop).
Hypothesis Pre_fwd : forall s s', Fr s s' -> Pre s -> Pre s'.
Hypothesis Post_back : forall s s', Fr s s' -> Post s' -> Post s.
(* one round, from the merges U *)
Hypothesis Hround : forall U s e needs s', Tri U -> UfOK U -> Inv s -> Pre s -> Post s' ->
  walk (env_with_edges [] U) s = Good ((e, needs), s') ->
  exists les tm, Rd (map (tr_ent Fl) les) tm /\ WS Fl U e tm les needs.

Definition MG_equiv (U : list (nat * ety)) : Prop :=
  forall es tm sb, Rd es tm -> A.Unifies sb es -> RespE Fl sb U.

Lemma rounds_equiv : forall mi n U s e needs s',
  mw_go walk mi n [] U s = Good ((e, needs), s') ->
  Inv s -> Pre s -> Post s' -> Tri U -> UfOK U -> MG_equiv U ->
  exists les tm, Rd (map (tr_ent Fl) les) tm /\ WS Fl (en_uf e) e tm les needs /\ MG_equiv (en_uf e).
Proof.
  intros mi n. induction n as [|n IH]; intros U s e needs s' H Hi Hp Hq T O Mg; cbn [mw_go] in H;
    apply bind_inv in H as ([e1 n1] & s1 & H1 & H2); destruct (Hok _ s _ s1 Hi H1) as [Hi1 F1];
    cbn beta iota zeta in H2.
  - destruct (edges_eqb _ _) eqn:Ee; [|noerr H2]. injection H2; intros; subst.
    destruct (Hround U s e needs s' T O Hi Hp Hq H1) as (les & tm & Rl & Hws).
    rewrite (tne_nil _ _ _ _ _ _ Hws) in Ee. destruct (ws_ext _ _ _ _ _ _ Hws) as (nw & En).
    rewrite En in Ee. pose proof (edges_settled _ _ Ee) as ->. rewrite app_nil_r in En.
    exists les, tm. rewrite En. auto.
  - destruct (edges_eqb _ _) eqn:Ee.
    + injection H2; intros; subst.
      destruct (Hround U s e needs s' T O Hi Hp Hq H1) as (les & tm & Rl & Hws).
      rewrite (tne_nil _ _ _ _ _ _ Hws) in Ee. destruct (ws_ext _ _ _ _ _ _ Hws) as (nw & En).
      rewrite En in Ee. pose proof (edges_settled _ _ Ee) as ->. rewrite app_nil_r in En.
      exists les, tm. rewrite En. auto.
    + destruct (ok_mw_go walk Hok mi n _ _ s1 _ s' Hi1 H2) as [_ F2].
      destruct (Hround U s e1 n1 s1 T O Hi Hp (Post_back _ _ F2 Hq) H1) as (les & tm & Rl & Hws).
      rewrite (tne_nil _ _ _ _ _ _ Hws) in H2.
      apply (IH (en_uf e1) s1 e needs s' H2 Hi1 (Pre_fwd _ _ F1 Hp) Hq (ws_tri _ _ _ _ _ _ Hws) (cc_uf _ (ws_clean _ _ _ _ _ _ Hws))).
      intros es tm' sb Rs Hu. apply (ws_mg _ _ _ _ _ _ Hws sb (Mg es tm' sb Rs Hu)).
      exact(proj2(Hdet _ _ _ _ sb Rl Rs)Hu).
Qed.

(* merged_walk: its last round's entries, read under σ = suf (its merges),
   form a MostGeneralMerge, and its needs are their Needs *)
Theorem merged_walk_equiv_spec : forall mi s e needs s', merged_walk mi walk s = Good ((e, needs), s') ->
  Inv s -> Pre s -> Post s' ->
  exists les tm, Rd (map (tr_ent Fl) les) tm /\ WS Fl (en_uf e) e tm les needs /\
    A.MostGeneralMerge (suf Fl (en_uf e)) (map (tr_ent Fl) les) /\
    A.Needs (suf Fl (en_uf e)) (map (tr_ent Fl) les) (map (key_tr Fl) needs).
Proof.
  intros mi s e needs s' H Hi Hp Hq. rewrite mw_go_eq in H.
  destruct (rounds_equiv mi 8 [] s e needs s' H Hi Hp Hq TriNil (fun g t Hin => match Hin with end) (fun es tm sb _ _ g t Hin => match Hin with end))
    as (les & tm & Rl & Hws & Mg).
  exists les, tm. split; [exact Rl|]. split; [exact Hws|]. split.
  - apply (settled_mgm Fl Finj _ e tm les needs Hws eq_refl). intros sb Hu. exact (Mg _ _ sb Rl Hu).
  - exact (settled_needs Fl Finj _ e tm les needs Hws eq_refl).
Qed.

End EquivalentRounds.
