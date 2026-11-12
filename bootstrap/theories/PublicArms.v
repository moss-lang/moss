(* Match arm selection for a typed scrutinee. A value typed at the
   scrutinee's runtime type selects an arm whose pattern binds typed
   locals; exhaustiveness rules out falling through. The scrutinee type
   must be formed: a union has nominal members with distinct heads, as
   TyUnion produces. Formation of elaborated types is a separate (T29)
   obligation. *)
From Stdlib Require Import List String Arith Bool.
From NM Require AuditBase.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety.
From NM Require Import LegacyBodyJudgments.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Definition UnionFormed (st : ty) : Prop :=
  forall ms, st = TUnion ms ->
    Forall (fun m => exists n xs, m = TNom n xs) ms /\ NoDup (map A.head ms).

Module Arms (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module O := Outcomes Prims I0.

Section Arms.
Variables (W : A.world) (Prog : program) (P : program_view) (d : environment) (H : heap_types).

Notation typed := (value_typed W P H).
Notation gr := (ground d).

Lemma ground_member : forall st w, typed w (gr st) ->
  UnionFormed st -> st <> A.never ->
  (exists s xs inner, In (TNom s xs) (A.members st) /\ w = VTag s inner /\ typed w (gr (TNom s xs))) \/
  (forall s xs, ~ In (TNom s xs) (A.members st)).
Proof.
  intros st w T Formed NotNever.
  destruct st as [m|fs| |g|n xs|ms]; cbn [A.members];
    try (right; intros s xs [E|[]]; discriminate).
  - left. rewrite ground_nom in T. destruct (nominal_value W P _ _ _ _ T) as [inner ->].
    exists n, xs, inner. split; [left; reflexivity|]. split; [reflexivity|rewrite ground_nom; exact T].
  - left. destruct (Formed ms eq_refl) as [Nominal _].
    rewrite ground_union in T. inversion T as [| | | |? member ? Member In'|]; subst.
    apply in_map_iff in In' as [m [<- InM]].
    rewrite Forall_forall in Nominal. destruct (Nominal m InM) as [s [xs ->]].
    rewrite ground_nom in Member. destruct (nominal_value W P _ _ _ _ Member) as [inner ->].
    exists s, xs, inner. split; [exact InM|]. split; [reflexivity|rewrite ground_nom; exact Member].
Qed.

Lemma heads_unique : forall ms s xs xs',
  NoDup (map A.head ms) -> In (TNom s xs) ms -> In (TNom s xs') ms -> xs = xs'.
Proof.
  induction ms as [|m r IH]; intros s xs xs' Distinct In1 In2; [contradiction|].
  cbn in Distinct. inversion Distinct as [|? ? Absent Rest]; subst.
  destruct In1 as [E1|In1]; destruct In2 as [E2|In2].
  - rewrite E1 in E2. injection E2 as ->; reflexivity.
  - exfalso. subst m. apply Absent. change (A.head (TNom s xs)) with (A.head (TNom s xs')). apply in_map, In2.
  - exfalso. subst m. apply Absent. change (A.head (TNom s xs')) with (A.head (TNom s xs)). apply in_map, In1.
  - exact (IH s xs xs' Rest In1 In2).
Qed.

(* A tag of a formed scrutinee is typed at its member. *)
Lemma tag_member : forall st s xs inner, typed (VTag s inner) (gr st) ->
  UnionFormed st -> st <> A.never -> In (TNom s xs) (A.members st) ->
  typed (VTag s inner) (gr (TNom s xs)).
Proof.
  intros st s xs inner T Formed NotNever Member.
  destruct (ground_member st _ T Formed NotNever) as [[s' [xs' [inner' [Member' [Same T']]]]]|None'];
    [|exfalso; exact (None' s xs Member)].
  injection Same as <- <-.
  destruct st as [m|fs| |g|n ys|ms]; cbn [A.members] in Member, Member';
    try (destruct Member as [E|[]]; discriminate).
  - destruct Member as [E|[]]; destruct Member' as [E'|[]]; congruence.
  - destruct (Formed ms eq_refl) as [_ Distinct]. rewrite (heads_unique ms s xs xs' Distinct Member Member'). exact T'.
Qed.

Lemma assoc_slook : forall f (fs : list (fld * value)), O.I.assoc_fld f fs = A.slook f fs.
Proof. intros f fs; induction fs as [|[g v] r IH]; cbn; [reflexivity|rewrite IH; reflexivity]. Qed.

Lemma fieldpats_bind : forall F ftys fs bs fxs values,
  LegacyBodyJudgments.FieldPats W F ftys fs bs fxs ->
  fields_typed W P H values (map (fun q => (fst q, gr (snd q))) ftys) ->
  exists vals, O.I.bind_fields fxs values = Some vals /\
    locals_typed W P H vals (map (fun q => (A.var_of F (fst q), gr (snd q))) bs) /\
    Forall (fun q => A.Named F (fst q)) bs /\ List.length bs = List.length fxs.
Proof.
  intros F ftys fs bs fxs values Pats Fields.
  induction Pats as [|n t r bs xs Look Named Rest IH|n t r bs xs Look Rest IH|n y t r bs xs Look Res Named Rest IH].
  - exists []; repeat split; constructor.
  - destruct IH as [vals [Bind [Typed [Names Len]]]].
    destruct (field_lookup_typed W P H values _ n (gr t) Fields) as [v [Found TV]];
      [rewrite slook_ground, Look; reflexivity|].
    exists ((A.var_of F n, v) :: vals). cbn. rewrite assoc_slook, Found, Bind.
    repeat split; [constructor; [split; [reflexivity|exact TV]|exact Typed]|constructor; assumption|cbn; congruence].
  - exact IH.
  - destruct IH as [vals [Bind [Typed [Names Len]]]].
    destruct (field_lookup_typed W P H values _ n (gr t) Fields) as [v [Found TV]];
      [rewrite slook_ground, Look; reflexivity|].
    exists ((A.var_of F y, v) :: vals). cbn. rewrite assoc_slook, Found, Bind.
    repeat split; [constructor; [split; [reflexivity|exact TV]|exact Typed]|constructor; assumption|cbn; congruence].
Qed.

Definition PatResult (F : A.frame) (binds : list (name * ty)) (bs : locals) : Prop :=
  Forall (fun q => A.Named F (fst q)) binds /\ NoDup (map fst binds) /\
  locals_typed W P H bs (map (fun q => (A.var_of F (fst q), gr (snd q))) binds).

Lemma pat_match : forall F st pat xp binds hs ca w,
  LegacyBodyJudgments.PatOf W F st pat xp binds hs ca -> UnionFormed st -> st <> A.never -> typed w (gr st) ->
  (forall s, In s hs -> exists xs, In (TNom s xs) (A.members st)) /\
  (ca = false -> hs <> []) /\
  ((exists bs, O.I.match_pat xp w = O.I.PMatch bs /\ PatResult F binds bs) \/
   (O.I.match_pat xp w = O.I.PNo /\ ca = false /\ forall s, In s hs -> forall inner, w <> VTag s inner)).
Proof.
  intros F st pat xp binds hs ca w Pat Formed NotNever T.
  destruct Pat as [|n Res Named|pa s xs R K Member
                   |pa s xs y pay opening R K Member Pay Res Named
                   |pa s xs fs ftys bs fxs R K Member Pay Arity Pats Distinct].
  - split; [intros s []|]. split; [discriminate|]. left. exists []; repeat split; constructor.
  - split; [intros s []|]. split; [discriminate|]. left.
    exists [(A.var_of F n, w)]; repeat split; repeat constructor; auto; intros [].
  - split; [intros s' [<-|[]]; eauto|]. split; [discriminate|].
    destruct w as [|s' inner| |]; cbn [O.I.match_pat];
      try (right; repeat split; [intros ? [<-|[]] ? ?; discriminate]).
    destruct (Nat.eqb_spec s s') as [<-|Different].
    + left. exists []; repeat split; constructor.
    + right. repeat split. intros ? [<-|[]] inner' E. injection E as E; congruence.
  - split; [intros s' [<-|[]]; eauto|]. split; [discriminate|].
    destruct w as [|s' inner| |]; cbn [O.I.match_pat];
      try (right; repeat split; [intros ? [<-|[]] ? ?; discriminate]).
    destruct (Nat.eqb_spec s s') as [<-|Different].
    + left. exists [(A.var_of F y, inner)].
      pose proof (tag_member st s xs inner T Formed NotNever Member) as TM. rewrite ground_nom in TM.
      destruct (tagged_payload_typed W P H s _ inner TM) as [payload [Pay' Inner]].
      pose proof (payload_det W _ _ _ _ Pay' (payload_subst W (type_meanings d) _ _ _ Pay)) as ->.
      repeat split; repeat constructor; auto; try (intros []); exact Inner.
    + right. repeat split. intros ? [<-|[]] inner' E. injection E as E; congruence.
  - split; [intros s' [<-|[]]; eauto|]. split; [discriminate|].
    destruct w as [|s' inner| |]; cbn [O.I.match_pat];
      try (right; repeat split; [intros ? [<-|[]] ? ?; discriminate]).
    destruct (Nat.eqb_spec s s') as [<-|Different].
    + left.
      pose proof (tag_member st s xs inner T Formed NotNever Member) as TM. rewrite ground_nom in TM.
      destruct (tagged_payload_typed W P H s _ inner TM) as [payload [Pay' Inner]].
      pose proof (payload_det W _ _ _ _ Pay' (payload_subst W (type_meanings d) _ _ _ Pay)) as ->.
      change (app_subst (type_meanings d) (TRecord ftys)) with (gr (TRecord ftys)) in Inner.
      rewrite ground_record in Inner.
      destruct (record_value W P _ _ _ Inner) as [values [-> Fields]].
      destruct (fieldpats_bind F ftys fs bs fxs values Pats Fields) as [vals [Bind [Typed [Names Len]]]].
      destruct fxs as [|fx fxs'].
      * destruct bs; [|discriminate]. exists []; repeat split; constructor.
      * rewrite Bind. exists (vals ++ []); rewrite app_nil_r. split; [reflexivity|].
        split; [exact Names|]. split; [exact Distinct|exact Typed].
    + right. repeat split. intros ? [<-|[]] inner' E. injection E as E; congruence.
Qed.

Definition Selected F C st (arms : list (apat * aexpr)) exp t xarms w : Prop :=
  exists pat body bs eb binds et,
    O.I.select_arm xarms w = Some (bs, eb) /\ In (pat, body) arms /\
    LegacyBodyJudgments.Branch W Prog F (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) body exp eb et /\
    A.fits et t /\ A.opt_fits et exp /\ PatResult F binds bs /\
    exists xp hs ca, LegacyBodyJudgments.PatOf W F st pat xp binds hs ca.

Lemma armslist_select : forall F C st arms exp xarms covered catch t w,
  LegacyBodyJudgments.ArmsList W Prog F C st arms exp xarms covered catch t ->
  UnionFormed st -> st <> A.never -> typed w (gr st) ->
  Selected F C st arms exp t xarms w \/
  (O.I.select_arm xarms w = None /\ catch = false /\ covered <> [] /\
   forall s, In s covered -> (forall inner, w <> VTag s inner) /\ exists xs, In (TNom s xs) (A.members st)).
Proof.
  intros F C st arms exp xarms covered catch t w List Formed NotNever T.
  induction List as [F C st pat body exp xp binds hs ca bx bt Pat Br Fits
                    |F C st pat body rest exp xp binds hs ca bx bt xr hr cr tr t NonEmpty Pat Br Fits Rest IH Join].
  - destruct (pat_match F st pat xp binds hs ca w Pat Formed NotNever T) as [Heads [Catch [[bs [Match Res]]|[Match [NoCatch Miss]]]]].
    + left. exists pat, body, bs, bx, binds, bt. split; [cbn; rewrite Match; reflexivity|].
      split; [left; reflexivity|]. split; [exact Br|]. split; [apply fits_refl|]. split; [exact Fits|split; [exact Res|eauto]].
    + right. cbn. rewrite Match. split; [reflexivity|]. split; [exact NoCatch|].
      split; [exact (Catch NoCatch)|]. intros s In'. split; [exact (Miss s In')|exact (Heads s In')].
  - destruct (pat_match F st pat xp binds hs ca w Pat Formed NotNever T) as [Heads [Catch [[bs [Match Res]]|[Match [NoCatch Miss]]]]].
    + left. exists pat, body, bs, bx, binds, bt.
      assert (Left : A.fits bt t).
      { destruct Join as [a b e|b|a b _ _]; [exact Fits|apply fits_never|apply fits_refl]. }
      split; [cbn; rewrite Match; reflexivity|].
      split; [left; reflexivity|]. split; [exact Br|]. split; [exact Left|]. split; [exact Fits|split; [exact Res|eauto]].
    + destruct (IH Formed NotNever T) as
        [[pat' [body' [bs [eb [binds' [et [Sel [In' [Br' [FitsT [FitsE [Res PO]]]]]]]]]]]]|[Sel [NoCatch' [Covered Miss']]]].
      * left. exists pat', body', bs, eb, binds', et. split; [cbn; rewrite Match; exact Sel|]. split; [right; exact In'|].
        split; [exact Br'|]. split; [|split; [exact FitsE|split; [exact Res|exact PO]]].
        destruct Join as [a b e|b|a b [Never|Same] NotNever'].
        -- exact FitsE.
        -- exact FitsT.
        -- subst b. rewrite (fits_into_never _ FitsT). apply fits_never.
        -- subst b. exact FitsT.
      * right. split; [cbn; rewrite Match; exact Sel|]. rewrite NoCatch, NoCatch'. split; [reflexivity|].
        split; [destruct hs; [exfalso; exact (Catch NoCatch eq_refl)|discriminate]|].
        intros s In'. apply in_app_or in In' as [Here|There].
        -- split; [exact (Miss s Here)|exact (Heads s Here)].
        -- exact (Miss' s There).
Qed.

Theorem arms_select : forall F C st arms exp xarms t w,
  LegacyBodyJudgments.Arms W Prog F C st arms exp xarms t -> UnionFormed st -> typed w (gr st) ->
  Selected F C st arms exp t xarms w.
Proof.
  intros F C st arms exp xarms t w Arms Formed T. destruct Arms as [F C exp|F C st arms exp xarms covered catch t NotNever List Exhaustive].
  - exfalso. exact (empty_union_uninhabited W P H w T).
  - destruct (armslist_select F C st arms exp xarms covered catch t w List Formed NotNever T) as [Sel|[_ [NoCatch [Covered Miss]]]];
      [exact Sel|exfalso].
    destruct Exhaustive as [Catch|[Members Heads]]; [congruence|].
    destruct (ground_member st w T Formed NotNever) as [[s [xs [inner [Member [-> _]]]]]|NoNominal].
    + destruct (Miss s (Heads (TNom s xs) s Member eq_refl)) as [NotTag _]. exact (NotTag inner eq_refl).
    + destruct covered as [|s rest]; [exact (Covered eq_refl)|].
      destruct (Miss s (or_introl eq_refl)) as [_ [xs Member]]. exact (NoNominal s xs Member).
Qed.
End Arms.
End Arms.
