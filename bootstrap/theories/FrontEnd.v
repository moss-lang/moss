(* The source lexer and Menhir parser against Audit.Syntax.Derives. *)

From Stdlib Require Import String Ascii List Bool Arith Lia.
From Stdlib Require Import Strings.Byte.
From NM Require Import ExecutionModel Parse.
From NM Require MossGrammar MossActions.
Import ListNotations.
Import ExecutionModel.Input ExecutionModel.Syntax.
Local Open Scope string_scope.

(* ######################################################################## *)
(* Basics                                                                    *)
(* ######################################################################## *)

(* Kinds: equality is decidable, by `token_kind_eqb`. *)
Lemma token_kind_index_inj a b : token_kind_index a = token_kind_index b -> a = b.
Proof.
  set (all :=
    [NAME; STRING; INTEGER; DECIMAL; AS; ASSUME; FUNCTOR; BIND; BREAK; CONTEXT;
     ELSE; FN; FOR; IF; IMPORT; LET; LOOP; MATCH; RETURN; TAG; THIS_TYPE; THIS;
     TYPE; USE; VAL; VAR; WHILE; IN; UNDERSCORE; EXCLAM; PERCENT; AMPERSAND;
     LPAREN; RPAREN; STAR; PLUS; COMMA; HYPHEN; DOT; SLASH; COLON; SEMI; LESS;
     EQUAL; GREATER; LBRACKET; RBRACKET; CARET; LBRACE; PIPE; RBRACE;
     EXCLAM_EQUAL; COLON_COLON; EQUAL_GREATER; LESS_LESS; LESS_EQUAL;
     EQUAL_EQUAL; GREATER_EQUAL; GREATER_GREATER; HYPHEN_GREATER; EOF]).
  assert (R : forall k, nth (token_kind_index k) all EOF = k) by (destruct k; reflexivity).
  intro H. rewrite <- (R a), <- (R b), H. reflexivity.
Qed.
Lemma token_kind_eqb_eq a b : token_kind_eqb a b = true <-> a = b.
Proof.
  unfold token_kind_eqb. rewrite Nat.eqb_eq. split; [apply token_kind_index_inj | intros ->; reflexivity].
Qed.
Lemma token_kind_eqb_refl a : token_kind_eqb a a = true.
Proof. apply token_kind_eqb_eq. reflexivity. Qed.
Lemma token_kind_eqb_neq a b : token_kind_eqb a b = false <-> a <> b.
Proof. rewrite <- token_kind_eqb_eq. destruct (token_kind_eqb a b); intuition discriminate. Qed.
Lemma token_kind_eqb_spec a b : reflect (a = b) (token_kind_eqb a b).
Proof. apply iff_reflect. symmetry. apply token_kind_eqb_eq. Qed.

(* ===================== The lexer ========================================== *)

Lemma span_spec p cs a b : span p cs = (a, b) ->
  cs = (a ++ b)%list /\ forallb p a = true /\ starts p b = false.
Proof.
  revert a b. induction cs as [|c r IH]; intros a b H; simpl in H.
  - injection H as <- <-. auto.
  - destruct (p c) eqn:E.
    + destruct (span p r) as [a' b'] eqn:Es. injection H as <- <-.
      destruct (IH _ _ eq_refl) as (-> & H1 & H2). simpl. rewrite E, H1. auto.
    + injection H as <- <-. simpl. auto.
Qed.

Lemma lex_string_other i j c r acc : c <> x0a -> c <> """"%byte -> c <> "\"%byte ->
  lex_string i j (c :: r) acc = lex_string i (S j) r (c :: acc).
Proof. intros H1 H2 H3. destruct c; try reflexivity; congruence. Qed.

Lemma lex_string_sound_n : forall n cs i j acc b r', length cs <= n ->
  lex_string i j cs acc = inl (b, r') ->
  exists body, cs = (body ++ """"%byte :: r')%list /\ b = (rev acc ++ body)%list /\ StrBody body.
Proof.
  induction n as [|n IH]; intros cs i j acc b r' Hl H; [destruct cs; [discriminate | simpl in Hl; lia]|].
  destruct cs as [|c r]; [discriminate|]. simpl in Hl.
  destruct (Byte.eqb c x0a) eqn:E1; [apply Byte.byte_dec_bl in E1; subst; discriminate|].
  destruct (Byte.eqb c """"%byte) eqn:E2.
  - apply Byte.byte_dec_bl in E2. subst. simpl in H. injection H as <- <-.
    exists []. unfold rev'. rewrite <- rev_alt, app_nil_r. repeat split; constructor.
  - destruct (Byte.eqb c "\"%byte) eqn:E3.
    + apply Byte.byte_dec_bl in E3. subst. simpl in H.
      destruct r as [|d r]; [discriminate|]. destruct (is_escape d) eqn:Ed; [|discriminate].
      simpl in Hl. destruct (IH r _ _ _ _ _ ltac:(lia) H) as (body & -> & -> & Hb).
      exists ("\"%byte :: d :: body). simpl. rewrite <- !app_assoc. repeat split; auto.
      apply SBEsc; auto.
    + rewrite lex_string_other in H; try (intro; subst; discriminate).
      destruct (IH r _ _ _ _ _ ltac:(lia) H) as (body & -> & -> & Hb).
      exists (c :: body). simpl. rewrite <- !app_assoc. repeat split; auto.
      apply SBByte; auto; intro; subst; discriminate.
Qed.

Lemma lex_string_sound cs i j acc b r' : lex_string i j cs acc = inl (b, r') ->
  exists body, cs = (body ++ """"%byte :: r')%list /\ b = (rev acc ++ body)%list /\ StrBody body.
Proof. apply lex_string_sound_n with (n := length cs). lia. Qed.

Lemma byte_dot_other {A} (c : byte) (X Y : A) : c <> "."%byte ->
  match c with "."%byte => X | _ => Y end = Y.
Proof. intro H. destruct c; congruence. Qed.

Lemma frac_spec r1 kd frac r2 :
  match r1 with
  | "."%byte :: d :: r' =>
      if is_digit d then let (ds2, r'') := span is_digit r' in (DECIMAL, "."%byte :: d :: ds2, r'')
      else (INTEGER, [], r1)
  | _ => (INTEGER, [], r1)
  end = (kd, frac, r2) ->
  (kd = INTEGER /\ frac = [] /\ r2 = r1 /\ forall c r, r1 = "."%byte :: c :: r -> is_digit c = false) \/
  (kd = DECIMAL /\ exists d ds2, frac = "."%byte :: d :: ds2 /\ forallb is_digit (d :: ds2) = true /\
                       r1 = (frac ++ r2)%list /\ starts is_digit r2 = false).
Proof.
  intro H. destruct r1 as [|c1 r1].
  - injection H as <- <- <-. left. repeat split; auto. discriminate.
  - destruct (Byte.byte_eq_dec c1 "."%byte) as [->|Hc].
    + destruct r1 as [|d r'].
      * injection H as <- <- <-. left. repeat split; auto. discriminate.
      * destruct (is_digit d) eqn:Ed.
        -- destruct (span is_digit r') as [ds2 r''] eqn:Es. apply span_spec in Es as (-> & H1 & H2).
           injection H as <- <- <-. right. split; auto. exists d, ds2. simpl. rewrite Ed, H1. auto.
        -- injection H as <- <- <-. left. repeat split; auto. intros c r E. injection E as -> ->. auto.
    + rewrite (byte_dot_other c1) in H by exact Hc. injection H as <- <- <-. left.
      repeat split; auto. intros c r E. injection E as E. congruence.
Qed.

Lemma span_digits_first d r eds r4 : is_digit d = true -> span is_digit (d :: r) = (eds, r4) ->
  exists ds', eds = d :: ds'.
Proof. intros Hd H. simpl in H. rewrite Hd in H. destruct (span is_digit r). injection H as <- _. eauto. Qed.

Lemma lex_number_sound c r i t r' j : is_digit c = true -> lex_number c r i = inl (t, r', j) ->
  exists cs k, (c :: r) = (cs ++ r')%list /\ Lexeme cs k r' /\ t = Tok k (string_of_list_byte cs) i /\
               j = i + length cs.
Proof.
  intros Hc H. unfold lex_number in H.
  destruct (span is_digit r) as [ds r1] eqn:Hs. apply span_spec in Hs as (-> & Hds & _).
  match type of H with context [match ?m with (_, _) => _ end] =>
    destruct m as [[kd frac] r2] eqn:Hf end.
  apply frac_spec in Hf.
  assert (Hfr : Fraction frac /\ (ds ++ r1 = ds ++ frac ++ r2)%list /\
                (frac = [] -> forall c r, r2 = "."%byte :: c :: r -> is_digit c = false) /\
                kd = match frac with [] => INTEGER | _ => DECIMAL end).
  { destruct Hf as [(-> & -> & -> & Hn) | (-> & d & ds2 & -> & Hd & -> & _)].
    - repeat split; auto. left; auto.
    - repeat split; auto. right; eauto. discriminate. }
  clear Hf. destruct Hfr as (Hfrac & Hr1 & Hdot & Hkd). rewrite Hr1. clear Hr1 r1.
  (* the exponent *)
  assert (Hnone : forall r', starts is_name_cont r' = false ->
            (forall c0 r0, frac = [] -> r' = "."%byte :: c0 :: r0 -> is_digit c0 = false) ->
            exists cs k, (c :: ds ++ frac ++ r')%list = (cs ++ r')%list /\ Lexeme cs k r' /\
              Tok kd (string_of_list_byte ((c :: ds) ++ frac)) i = Tok k (string_of_list_byte cs) i /\
              i + length (c :: ds) + length frac = i + length cs).
  { intros r'' Hn Hd. exists (c :: ds ++ frac ++ [])%list, kd.
    rewrite !app_nil_r. split; [simpl; rewrite app_assoc; reflexivity|]. split.
    - replace kd with (match frac, @nil byte with [], [] => INTEGER | _, _ => DECIMAL end)
        by (subst kd; destruct frac; reflexivity).
      replace (c :: ds ++ frac)%list with (c :: ds ++ frac ++ [])%list by (rewrite app_nil_r; reflexivity).
      apply LxNumber; auto. simpl; rewrite Hc; auto. left; auto. intros Hf0 _. eauto.
    - simpl. rewrite app_length. split; [reflexivity | lia]. }
  assert (Hsome : forall ex r4, Exponent ex -> ex <> [] -> starts is_name_cont r4 = false ->
            exists cs k, (c :: ds ++ frac ++ ex ++ r4)%list = (cs ++ r4)%list /\ Lexeme cs k r4 /\
              Tok DECIMAL (string_of_list_byte (((c :: ds) ++ frac) ++ ex)) i = Tok k (string_of_list_byte cs) i /\
              i + length (c :: ds) + length frac + length ex = i + length cs).
  { intros ex r4 Hex Hne Hn. exists (c :: ds ++ frac ++ ex)%list, DECIMAL.
    split; [simpl; rewrite !app_assoc; reflexivity|]. split.
    - assert (L : Lexeme (c :: ds ++ frac ++ ex) (match frac, ex with [], [] => INTEGER | _, _ => DECIMAL end) r4).
      { apply LxNumber; auto. simpl; rewrite Hc; auto. intros _ E. contradiction. }
      replace (match frac, ex with [], [] => INTEGER | _, _ => DECIMAL end) with DECIMAL in L
        by (destruct frac, ex; congruence). exact L.
    - simpl. rewrite <- !app_assoc, !app_length. split; [reflexivity | lia]. }
  destruct r2 as [|e [|s r3]].
  - injection H as <- <- <-.
    destruct (Hnone [] eq_refl ltac:(intros; discriminate)) as (cs & k & F1 & F2 & F3 & F4).
    exists cs, k. auto.
  - destruct (is_name_cont e) eqn:En; [discriminate|]. injection H as <- <- <-.
    destruct (Hnone [e] ltac:(simpl; auto) ltac:(intros; discriminate)) as (cs & k & F1 & F2 & F3 & F4).
    exists cs, k. auto.
  - destruct (is_e e && is_sign s) eqn:E1.
    + destruct r3 as [|d r3]; [discriminate|]. destruct (is_digit d) eqn:Ed; [|discriminate].
      destruct (span is_digit (d :: r3)) as [eds r4] eqn:Hs.
      destruct (span_digits_first _ _ _ _ Ed Hs) as [ds' ->].
      apply span_spec in Hs as (Hr3 & Hdg & _).
      destruct (MAX_EXPONENT_DIGITS <? length (strip_zeros (d :: ds')))%nat eqn:Em; [discriminate|].
      apply andb_prop in E1 as [Ee Es].
      assert (Hex : Exponent ([e; s] ++ d :: ds')%list).
      { right. exists e, [s], d, ds'. split; [reflexivity|]. repeat split; auto. right; eauto.
        apply Nat.ltb_ge in Em. unfold MAX_EXPONENT_DIGITS in Em. lia. }
      destruct r4 as [|b r4].
      * injection H as <- <- <-.
        edestruct (Hsome _ [] Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | reflexivity |].
        exists cs, k. rewrite Hr3, <- F1. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
      * destruct (is_name_cont b) eqn:Eb; [discriminate|]. injection H as <- <- <-.
        edestruct (Hsome _ (b :: r4) Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | simpl; auto |].
        exists cs, k. rewrite Hr3, <- F1. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
    + destruct (is_e e && is_digit s) eqn:E2.
      * destruct (span is_digit (s :: r3)) as [eds r4] eqn:Hs.
        apply andb_prop in E2 as [Ee Es].
        destruct (span_digits_first _ _ _ _ Es Hs) as [ds' ->].
        apply span_spec in Hs as (Hr3 & Hdg & _).
        destruct (MAX_EXPONENT_DIGITS <? length (strip_zeros (s :: ds')))%nat eqn:Em; [discriminate|].
        assert (Hex : Exponent ([e] ++ s :: ds')%list).
        { right. exists e, [], s, ds'. split; [reflexivity|]. repeat split; auto.
          apply Nat.ltb_ge in Em. unfold MAX_EXPONENT_DIGITS in Em. lia. }
        destruct r4 as [|b r4].
        -- injection H as <- <- <-.
           edestruct (Hsome _ [] Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | reflexivity |].
           exists cs, k. rewrite <- F1. rewrite Hr3. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
        -- destruct (is_name_cont b) eqn:Eb; [discriminate|]. injection H as <- <- <-.
           edestruct (Hsome _ (b :: r4) Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | simpl; auto |].
           exists cs, k. rewrite <- F1, Hr3. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
      * destruct (is_name_cont e) eqn:En; [discriminate|]. injection H as <- <- <-.
        destruct (Hnone (e :: s :: r3) ltac:(simpl; auto)) as (cs & k & F1 & F2 & F3 & F4).
        { intros c0 r0 Hf0 E. injection E as -> -> ->. eapply Hdot; eauto. }
        exists cs, k. auto.
Qed.

Lemma lex_tok i cs k r ts all j : Lexeme cs k r -> Lexes j r ts -> all = (cs ++ r)%list ->
  j = i + length cs -> Lexes i all (Tok k (string_of_list_byte cs) i :: ts).
Proof. intros H Hl -> ->. apply LexToken; auto. Qed.

Definition LexStep (i : nat) (cs : list byte) (acc : list token) (r' : list byte) (i' : nat)
  (acc' : list token) : Prop :=
  (acc' = acc /\ forall T, Lexes i' r' T -> Lexes i cs T) \/
  (exists t, acc' = t :: acc /\ forall T, Lexes i' r' T -> Lexes i cs (t :: T)).

Lemma lex_char_sound c r i acc r' i' acc' : lex_char c r i acc = inl (r', i', acc') ->
  LexStep i (c :: r) acc r' i' acc'.
Proof.
  unfold lex_char, LexStep. intro H.
  destruct (is_ws c) eqn:Ews.
  { injection H as <- <- <-. left. split; auto. intros. apply LexSpace; auto. }
  destruct (Byte.eqb c "#"%byte) eqn:Eh.
  { apply Byte.byte_dec_bl in Eh. subst.
    destruct (span (fun b => negb (Byte.eqb b x0a)) r) as [com rest] eqn:Es.
    apply span_spec in Es as (-> & H1 & H2). injection H as <- <- <-. left. split; auto.
    intros. apply LexComment; auto. }
  destruct r as [|d r0] eqn:Er.
  2: destruct (two_char c d) as [k|] eqn:E2.
  2: { injection H as <- <- <-. right. eexists. split; [reflexivity|]. intros T HT.
       eapply lex_tok; [apply LxSym2; eauto | exact HT | reflexivity | simpl; lia]. }
  all: destruct (one_char c) as [k|] eqn:E1.
  1, 3: injection H as <- <- <-; right; eexists; split; [reflexivity|]; intros T HT;
        (eapply lex_tok; [apply LxSym1; [exact E1|] | exact HT | reflexivity | simpl; lia]);
        intros b r1 Eb; first [discriminate | injection Eb as <- <-; auto].
  all: destruct (is_digit c) eqn:Ed.
  1, 3: match type of H with context [lex_number ?cc ?x ?ii] =>
          destruct (lex_number cc x ii) as [[[t r1] j]|[m o]] eqn:En; [|discriminate] end;
        injection H as <- <- <-; apply lex_number_sound in En as (cs & k & Ec & Hl & -> & ->); auto;
        right; eexists; split; [reflexivity|]; intros T HT; eapply lex_tok; eauto.
  all: destruct (is_name_start c) eqn:Ens.
  1, 3: match type of H with context [span is_name_cont ?x] =>
          destruct (span is_name_cont x) as [ns r1] eqn:Es end;
        apply span_spec in Es as (Es & H1 & H2);
        injection H as <- <- <-; right; eexists; split; [reflexivity|]; intros T HT;
        (eapply lex_tok; [apply LxWord; eauto | exact HT | simpl; rewrite Es; reflexivity | simpl; lia]).
  all: destruct (Byte.eqb c """"%byte) eqn:Eq; [|discriminate].
  all: apply Byte.byte_dec_bl in Eq; subst.
  all: match type of H with context [lex_string ?ii (S ?ii) ?x []] =>
         destruct (lex_string ii (S ii) x []) as [[body r1]|[m o]] eqn:Es; [|discriminate] end.
  all: apply lex_string_sound in Es as (body' & Es & Eb & Hb); simpl in Eb; subst body'.
  all: try (destruct body; discriminate).
  all: injection H as <- <- <-; right; eexists; split; [reflexivity|]; intros T HT.
  all: eapply lex_tok; [apply LxString; eauto | exact HT | rewrite Es; simpl; rewrite <- app_assoc; reflexivity
                       | simpl; rewrite app_length; simpl; lia].
Qed.

Lemma lex_go_sound : forall fuel cs i acc ts, lex_go fuel cs i acc = LOk ts ->
  exists ts', ts = (rev acc ++ ts')%list /\ Lexes i cs ts'.
Proof.
  induction fuel as [|fuel IH]; intros cs i acc ts H; simpl in H; [discriminate|].
  destruct cs as [|c r].
  - injection H as <-. exists [Tok EOF "" i]. unfold rev'. rewrite <- rev_alt. split; [reflexivity|constructor].
  - destruct (lex_char c r i acc) as [[[r' i'] acc']|[m o]] eqn:E; [|discriminate].
    apply lex_char_sound in E. apply IH in H as (ts' & -> & Hl).
    destruct E as [(-> & Hs) | (t & -> & Hs)].
    + exists ts'. auto.
    + exists (t :: ts'). simpl. rewrite <- app_assoc. auto.
Qed.

Theorem lex_sound cs ts : lex cs = LOk ts -> Lexes 0 cs ts.
Proof. unfold lex. intro H. apply lex_go_sound in H as (ts' & -> & H). exact H. Qed.

Lemma span_app p a b : forallb p a = true -> starts p b = false -> span p (a ++ b)%list = (a, b).
Proof.
  induction a as [|c a IH]; intros Ha Hb; simpl in *.
  - destruct b as [|d b]; simpl in *; [reflexivity|]. rewrite Hb. reflexivity.
  - apply andb_prop in Ha as [H1 H2]. rewrite H1, IH; auto.
Qed.

Lemma split_unique p a b a' b' : forallb p a = true -> forallb p a' = true ->
  starts p b = false -> starts p b' = false -> (a ++ b = a' ++ b')%list -> a = a' /\ b = b'.
Proof.
  intros Ha Ha' Hb Hb' E.
  assert (E1 := span_app p a b Ha Hb). assert (E2 := span_app p a' b' Ha' Hb').
  rewrite E in E1. rewrite E1 in E2. injection E2 as -> ->. auto.
Qed.

Lemma lex_string_complete i j body r acc : StrBody body ->
  lex_string i j (body ++ """"%byte :: r)%list acc = inl ((rev acc ++ body)%list, r).
Proof.
  intro H. revert i j acc. induction H as [|c r0 H1 H2 H3 Hb IH|d r0 Hd Hb IH]; intros i j acc.
  - simpl. unfold rev'. rewrite <- rev_alt, app_nil_r. reflexivity.
  - simpl app. rewrite lex_string_other by auto. rewrite IH. simpl. rewrite <- app_assoc. reflexivity.
  - simpl. rewrite Hd, IH. simpl. rewrite <- !app_assoc. reflexivity.
Qed.

(* Facts about the first byte of each kind of token. *)
Lemma two_char_first a b k : two_char a b = Some k ->
  is_ws a = false /\ a <> "#"%byte /\ is_digit a = false /\ is_name_start a = false /\ a <> """"%byte.
Proof. destruct a; simpl; try discriminate; intros; repeat split; discriminate. Qed.

Lemma one_char_first a k : one_char a = Some k ->
  is_ws a = false /\ a <> "#"%byte /\ is_digit a = false /\ is_name_start a = false /\ a <> """"%byte.
Proof. destruct a; simpl; try discriminate; intros; repeat split; discriminate. Qed.

Lemma name_first c : is_name_start c = true ->
  is_ws c = false /\ c <> "#"%byte /\ (forall b, two_char c b = None) /\ one_char c = None /\
  is_digit c = false /\ c <> """"%byte.
Proof. destruct c; simpl; try discriminate; intros; repeat split; try discriminate; reflexivity. Qed.

Lemma digit_first c : is_digit c = true ->
  is_ws c = false /\ c <> "#"%byte /\ (forall b, two_char c b = None) /\ one_char c = None /\
  is_name_cont c = true.
Proof. destruct c; simpl; try discriminate; intros; repeat split; try discriminate; reflexivity. Qed.

Lemma e_name_cont e : is_e e = true -> is_name_cont e = true /\ is_digit e = false /\ e <> "."%byte.
Proof. destruct e; simpl; try discriminate; intros; repeat split; discriminate. Qed.
Lemma sign_not_digit s : is_sign s = true -> is_digit s = false.
Proof. destruct s; simpl; try discriminate; reflexivity. Qed.
Lemma digit_name_cont d : is_digit d = true -> is_name_cont d = true.
Proof. destruct d; simpl; try discriminate; reflexivity. Qed.
Lemma starts_digit_of_name_cont r : starts is_name_cont r = false -> starts is_digit r = false.
Proof. destruct r as [|d r]; simpl; auto. destruct (is_digit d) eqn:E; auto. rewrite digit_name_cont; auto. Qed.

Lemma frac_ex_not_digit frac ex rest : Fraction frac -> Exponent ex -> starts is_name_cont rest = false ->
  starts is_digit (frac ++ ex ++ rest)%list = false.
Proof.
  intros [->|(d & ds & -> & _)] [->|(e & sign & d' & ds' & -> & He & _)] Hr; simpl; auto.
  - apply starts_digit_of_name_cont; auto.
  - apply e_name_cont in He. apply He.
Qed.

Ltac fin_num frac :=
  match goal with |- inl (Tok ?k1 ?t1 _, _, ?n1) = inl (Tok ?k2 ?t2 _, _, ?n2) =>
    replace n1 with n2 by (simpl; rewrite ?app_length; simpl; rewrite ?app_length; simpl; lia);
    replace t1 with t2 by (f_equal; simpl; rewrite <- ?app_assoc, ?app_nil_r; reflexivity) end;
  first [reflexivity | destruct frac; reflexivity].

Lemma lex_number_complete d ds frac ex rest i :
  forallb is_digit (d :: ds) = true -> Fraction frac -> Exponent ex -> starts is_name_cont rest = false ->
  (frac = [] -> ex = [] -> forall c r, rest = "."%byte :: c :: r -> is_digit c = false) ->
  lex_number d (ds ++ frac ++ ex ++ rest)%list i =
    inl (Tok (match frac, ex with [], [] => INTEGER | _, _ => DECIMAL end)
             (string_of_list_byte (d :: ds ++ frac ++ ex)%list) i, rest,
         i + length (d :: ds ++ frac ++ ex)%list).
Proof.
  intros Hd Hf He Hr Hdot. simpl in Hd. apply andb_prop in Hd as [Hd Hds].
  unfold lex_number.
  rewrite span_app by (auto; apply frac_ex_not_digit; auto).
  match goal with |- context [match ?m with (_, _) => _ end] => destruct m as [[kd frac'] r2] eqn:Hm end.
  apply frac_spec in Hm.
  assert (Hfr : kd = match frac with [] => INTEGER | _ => DECIMAL end /\ frac' = frac /\ r2 = (ex ++ rest)%list).
  { destruct Hf as [->|(d1 & ds1 & -> & Hd1)]; destruct Hm as [(-> & -> & -> & Hn) | (-> & d2 & ds2 & -> & Hd2 & E & Hs)].
    - auto.
    - exfalso. simpl in E. destruct He as [->|(e & sign & d3 & ds3 & -> & Hee & _)].
      + simpl in E. subst rest. simpl in Hd2. apply andb_prop in Hd2 as [Hd2 _].
        rewrite (Hdot eq_refl eq_refl d2 _ eq_refl) in Hd2. discriminate.
      + injection E as E. subst e. discriminate.
    - exfalso. simpl in Hd1. apply andb_prop in Hd1 as [Hd1 _]. rewrite (Hn d1 _ eq_refl) in Hd1. discriminate.
    - injection E as E. simpl in E.
      assert (Hsplit := split_unique is_digit (d1 :: ds1) (ex ++ rest) (d2 :: ds2) r2 Hd1 Hd2
                          (frac_ex_not_digit [] ex rest (or_introl eq_refl) He Hr) Hs).
      destruct Hsplit as [E1 E2]; [subst; simpl; rewrite H; reflexivity|]. injection E1 as -> ->. auto. }
  destruct Hfr as (-> & -> & ->). clear Hm.
  destruct He as [->|(e & sign & d3 & ds3 & -> & Hee & Hsign & Hd3 & Hlen)].
  - (* no exponent *)
    simpl app. destruct rest as [|b r'].
    + simpl. fin_num frac.
    + destruct r' as [|b2 r'].
      * simpl in Hr. simpl. rewrite Hr. fin_num frac.
      * assert (Hb : is_e b = false).
        { destruct (is_e b) eqn:E; auto. apply e_name_cont in E. simpl in Hr. destruct E as [E _]. congruence. }
        simpl in Hr. simpl. rewrite Hb. simpl. rewrite Hr. fin_num frac.
  - (* an exponent *)
    destruct Hsign as [-> | (s & -> & Hs)].
    + simpl app.
      assert (Hns : is_sign d3 = false).
      { simpl in Hd3. apply andb_prop in Hd3 as [Hd3 _]. destruct d3; simpl in *; try discriminate; reflexivity. }
      simpl in Hd3. apply andb_prop in Hd3 as [Hd3 Hds3].
      cbn [app]. rewrite Hee, Hns, Hd3. simpl.
      rewrite Hd3, span_app by (auto; apply starts_digit_of_name_cont; auto).
      assert (Hl : (MAX_EXPONENT_DIGITS <? length (strip_zeros (d3 :: ds3)))%nat = false)
        by (apply Nat.ltb_ge; unfold MAX_EXPONENT_DIGITS; lia).
      cbv zeta. rewrite Hl.
      destruct rest as [|b r']; simpl in Hr |- *; [|rewrite Hr]; fin_num frac.
    + simpl in Hd3. apply andb_prop in Hd3 as [Hd3 Hds3].
      cbn [app]. rewrite Hee, Hs. simpl. rewrite Hd3.
      change (d3 :: ds3 ++ rest)%list with ((d3 :: ds3) ++ rest)%list.
      rewrite span_app by (simpl; rewrite ?Hd3; auto; apply starts_digit_of_name_cont; auto).
      assert (Hl : (MAX_EXPONENT_DIGITS <? length (strip_zeros (d3 :: ds3)))%nat = false)
        by (apply Nat.ltb_ge; unfold MAX_EXPONENT_DIGITS; lia).
      cbv zeta. rewrite Hl.
      destruct rest as [|b r']; simpl in Hr |- *; [|rewrite Hr]; fin_num frac.
Qed.

Ltac fin_off := match goal with |- inl (_, ?n1, _) = inl (_, ?n2, _) =>
  replace n2 with n1 by (simpl; rewrite ?app_length; simpl; lia) end; reflexivity.

Lemma lexeme_char cs k r i acc : Lexeme cs k r ->
  exists c rest0, (cs ++ r)%list = c :: rest0 /\
    lex_char c rest0 i acc = inl (r, i + length cs, Tok k (string_of_list_byte cs) i :: acc).
Proof.
  intro H. destruct H as [a b k rest Hk | a k rest Hk Hn | c ns rest Hc Hns Hr | body rest Hb
                         | d ds frac ex rest Hd Hf He Hr Hdot].
  - exists a, (b :: rest). split; [reflexivity|].
    destruct (two_char_first _ _ _ Hk) as (H1 & H2 & _). unfold lex_char. rewrite H1.
    destruct (Byte.eqb a "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    rewrite Hk. fin_off.
  - exists a, rest. split; [reflexivity|].
    destruct (one_char_first _ _ Hk) as (H1 & H2 & _). unfold lex_char. rewrite H1.
    destruct (Byte.eqb a "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    replace (match rest with
             | [] => None
             | d :: r' => match two_char a d with Some k0 => Some (k0, d, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct rest as [|b r']; auto. rewrite (Hn b r' eq_refl). reflexivity. }
    rewrite Hk. fin_off.
  - exists c, (ns ++ rest)%list. split; [reflexivity|].
    destruct (name_first _ Hc) as (H1 & H2 & H3 & H4 & H5 & H6). unfold lex_char. rewrite H1.
    destruct (Byte.eqb c "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    replace (match (ns ++ rest)%list with
             | [] => None
             | d :: r' => match two_char c d with Some k0 => Some (k0, d, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct (ns ++ rest)%list as [|b r']; auto. rewrite H3. reflexivity. }
    rewrite H4, H5, Hc, span_app by auto. fin_off.
  - exists """"%byte, (body ++ [""""%byte] ++ rest)%list. split; [simpl; rewrite <- app_assoc; reflexivity|].
    unfold lex_char. simpl is_ws. cbv iota beta.
    replace (match (body ++ [""""%byte] ++ rest)%list with
             | [] => None
             | d :: r' => match two_char """"%byte d with Some k0 => Some (k0, d, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct (body ++ [""""%byte] ++ rest)%list as [|b r']; auto. }
    simpl one_char. simpl is_digit. simpl is_name_start. cbv iota beta. simpl Byte.eqb. cbv iota beta.
    simpl app. rewrite lex_string_complete by auto. simpl.
    fin_off.
  - simpl in Hd. apply andb_prop in Hd as [Hd Hds].
    exists d, (ds ++ frac ++ ex ++ rest)%list. split; [simpl; rewrite <- !app_assoc; reflexivity|].
    destruct (digit_first _ Hd) as (H1 & H2 & H3 & H4 & H5). unfold lex_char. rewrite H1.
    destruct (Byte.eqb d "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    replace (match (ds ++ frac ++ ex ++ rest)%list with
             | [] => None
             | d0 :: r' => match two_char d d0 with Some k0 => Some (k0, d0, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct (ds ++ frac ++ ex ++ rest)%list as [|b r']; auto. rewrite H3. reflexivity. }
    rewrite H4, Hd. rewrite lex_number_complete; auto. simpl. rewrite Hd, Hds. reflexivity.
Qed.

Lemma lex_go_complete i cs ts : Lexes i cs ts ->
  forall fuel acc, length cs < fuel -> lex_go fuel cs i acc = LOk (rev acc ++ ts)%list.
Proof.
  induction 1 as [i | i c r ts Hws H IH | i com r ts Hcom Hr H IH | i cs k r ts Hx H IH];
    intros fuel acc Hf; (destruct fuel as [|fuel]; [simpl in Hf; lia|]).
  - simpl. unfold rev'. rewrite <- rev_alt. reflexivity.
  - simpl. unfold lex_char. rewrite Hws. apply IH. simpl in Hf. lia.
  - simpl. unfold lex_char. simpl is_ws. cbv iota beta. simpl Byte.eqb. cbv iota beta.
    rewrite span_app by auto. apply IH. simpl in Hf. rewrite app_length in Hf. lia.
  - destruct (lexeme_char _ _ _ i acc Hx) as (c & rest0 & E & Hc).
    rewrite E. simpl. rewrite Hc. rewrite IH.
    + simpl. rewrite <- app_assoc. reflexivity.
    + rewrite E in Hf. simpl in Hf.
      assert (length (cs ++ r)%list = S (length rest0)) by (rewrite E; reflexivity).
      rewrite app_length in H0. destruct cs; [inversion Hx|]. simpl in H0. lia.
Qed.

Theorem lex_complete cs ts : Lexes 0 cs ts -> lex cs = LOk ts.
Proof. intro H. unfold lex. rewrite (lex_go_complete _ _ _ H); auto. Qed.

(* Menhir parses a prefix of an infinite stream. Padding is outside the
   grammar, and the entry point checks that it is next on success. *)
Module G := MossGrammar.Gram.
Module LR := MossGrammar.MenhirLibParser.Inter.

Definition source_symbol (s : G.symbol) : bool :=
  match s with G.T G.PADDING't => false | _ => true end.

Lemma source_productions :
  forallb (fun p => forallb source_symbol (G.prod_rhs_rev p))
    (MenhirLib.Alphabet.all_list (A := G.production)) = true.
Proof. vm_compute. reflexivity. Qed.

Lemma production_has_source_symbols p : Forall (fun s => source_symbol s = true) (G.prod_rhs_rev p).
Proof.
  apply (proj2 (Forall_forall _ _)). intros s Hin.
  pose proof source_productions as H.
  rewrite forallb_forall in H.
  specialize (H p (MenhirLib.Alphabet.all_list_forall p)).
  rewrite forallb_forall in H. exact (H s Hin).
Qed.

Lemma tree_has_source_tokens :
  forall s word (tree : G.parse_tree s word),
    source_symbol s = true -> Forall (fun t => MossGrammar.is_padding t = false) word.
Proof.
  fix go 3 with (go_list (ss : list G.symbol) (word : list G.token)
      (trees : G.parse_tree_list ss word) {struct trees} :
      Forall (fun s => source_symbol s = true) ss ->
      Forall (fun t => MossGrammar.is_padding t = false) word).
  - intros s word [t | p w children] H.
    + constructor; [destruct t; discriminate || reflexivity | constructor].
    + apply (go_list _ _ children), production_has_source_symbols.
  - intros ss word [|symbols w children s u child] H.
    + constructor.
    + apply Forall_app. inversion H; subst. split; eauto.
Qed.

Lemma mapped_source_tokens ts :
  Forall (fun t => MossGrammar.is_padding t = false) (map MossGrammar.of_token ts).
Proof.
  induction ts as [|[k text offset] ts IH]; simpl; constructor; auto.
  now destruct k.
Qed.

Lemma whole_source_prefix xs ys rest :
  Forall (fun t => MossGrammar.is_padding t = false) xs ->
  Forall (fun t => MossGrammar.is_padding t = false) ys ->
  LR.app_buf xs padding = LR.app_buf ys rest ->
  MossGrammar.is_padding (LR.buf_head rest) = true ->
  xs = ys.
Proof.
  revert ys. induction xs as [|x xs IH]; intros [|y ys] Hx Hy Hbuf Hend; auto.
  - inversion Hy; subst.
    pose proof (f_equal LR.buf_head Hbuf) as E. change (MossGrammar.PADDING tt = y) in E.
    subst y. discriminate.
  - inversion Hx; subst.
    pose proof (f_equal LR.buf_head Hbuf) as E. change (x = LR.buf_head rest) in E.
    congruence.
  - inversion Hx; inversion Hy; subst.
    pose proof (f_equal LR.buf_head Hbuf) as E. change (x = y) in E. subst y.
    f_equal. eapply IH; eauto. exact (f_equal LR.buf_tail Hbuf).
Qed.

Theorem parse_tokens_sound ts f : parse_tokens ts = MossActions.Checked f -> File ts f.
Proof.
  unfold parse_tokens.
  set (input := LR.app_buf (map MossGrammar.of_token ts) padding).
  pose proof (MossGrammar.file_correct (parse_log_fuel ts) input) as Sound.
  destruct (MossGrammar.file (parse_log_fuel ts) input) as [st t | | result rest]; try discriminate.
  destruct (MossGrammar.is_padding (LR.buf_head rest)) eqn:End; [|discriminate].
  intros ->. destruct Sound as (word & tree & Prefix & Value).
  assert (Tokens : map MossGrammar.of_token ts = word).
  { eapply whole_source_prefix; [apply mapped_source_tokens | | exact Prefix | exact End].
    eapply tree_has_source_tokens; [exact tree | reflexivity]. }
  unfold File. rewrite Tokens. now exists tree.
Qed.

Theorem parse_moss_sound s f : parse_moss s = Good f -> Derives (list_byte_of_string s) f.
Proof.
  unfold parse_moss.
  destruct (lex (map fst (raw_source (newlines (list_byte_of_string s))))) as [ts|m o] eqn:El; [|discriminate].
  destruct (parse_tokens ts) as [f'|m o] eqn:Ep; [|discriminate].
  intros H. injection H as <-. exists ts. split; [now apply lex_sound | now apply parse_tokens_sound].
Qed.
