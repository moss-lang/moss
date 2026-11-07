(* The compact AST dump preserves every constructor and field. Names need
   only avoid the dump's few reserved spellings and path separators. *)
From Stdlib Require Import List String Ascii Bool Arith.
From NM Require Import Audit Sx.
Import ListNotations Audit.Input Sx.Dump.
Local Open Scope string_scope.

Fixpoint strings_eqb (xs ys : list string) : bool :=
  match xs, ys with
  | [], [] => true
  | x :: xs, y :: ys => String.eqb x y && strings_eqb xs ys
  | _, _ => false
  end.

Lemma strings_eqb_eq xs ys : strings_eqb xs ys = true -> xs = ys.
Proof.
  revert ys. induction xs as [|x xs IH]; intros [|y ys] H; try discriminate; [reflexivity|].
  apply andb_true_iff in H as [H1 H2]. apply String.eqb_eq in H1. subst y.
  f_equal. exact (IH ys H2).
Qed.

Definition path_ok (p : path) : bool := strings_eqb (split_path (join_path p)) p.
Definition opt_name_ok (o : option name) : bool :=
  match o with None => true | Some n => negb (String.eqb n "_") end.
Definition opt_path_ok (o : option path) : bool :=
  match o with None => true | Some p => path_ok p && negb (String.eqb (join_path p) "_") end.
Definition optional {A} (ok : A -> bool) (o : option A) : bool :=
  match o with None => true | Some x => ok x end.

Fixpoint spec_ok (s : spec) : bool :=
  match s with Spec p d a => opt_path_ok p && opt_name_ok d &&
    match a with None => true | Some bs =>
      forallb (fun b => path_ok (fst b) && spec_ok (snd b)) bs end
  end.
Definition app_ok (a : sapp) : bool :=
  match a with None => true | Some bs =>
    forallb (fun b => path_ok (fst b) && spec_ok (snd b)) bs end.

Fixpoint ty_ok (t : aty) : bool :=
  match t with
  | ATRec fs => forallb (fun p => ty_ok (snd p)) fs
  | ATUnion ms => forallb ty_ok ms
  | ATRef p a => path_ok p && app_ok a
  | _ => true
  end.
Fixpoint pat_ok (p : apat) : bool :=
  match p with
  | APWild => true
  | APPath q => path_ok q
  | APTag q p => path_ok q && pat_ok p
  | APRec q fs => opt_path_ok q && forallb (fun f => optional pat_ok (snd f)) fs
  end.
Fixpoint expr_ok (e : aexpr) : bool :=
  match e with
  | AEPath p a => path_ok p && app_ok a
  | AECall p a es => path_ok p && app_ok a && forallb expr_ok es
  | AEField o _ => expr_ok o
  | AEMeth o p es => expr_ok o && path_ok p && forallb expr_ok es
  | AERec p a fs => path_ok p && app_ok a && forallb (fun f => optional expr_ok (snd f)) fs
  | AEIf c b el => expr_ok c && block_ok b && optional expr_ok el
  | AEMatch e arms => expr_ok e && forallb (fun a => pat_ok (fst a) && expr_ok (snd a)) arms
  | AEReturn o => optional expr_ok o
  | AEBlock b => block_ok b
  | AEUnary _ e => expr_ok e
  | AEBinary _ l r => expr_ok l && expr_ok r
  | AELam ps ret body => forallb (fun p => optional ty_ok (snd p)) ps && optional ty_ok ret && expr_ok body
  | _ => true
  end
with block_ok (b : ablock) : bool :=
  match b with ABlock ss t => forallb stmt_ok ss && optional expr_ok t end
with stmt_ok (s : astmt) : bool :=
  match s with
  | ASLet _ t e | ASVar _ t e => optional ty_ok t && expr_ok e
  | ASAssign _ e | ASExpr e => expr_ok e
  | ASBind its => forallb (fun p => spec_ok (fst p) && optional expr_ok (snd p)) its
  | ASWhile c b => expr_ok c && block_ok b
  | ASLoop b => block_ok b
  end.
Definition bind_ok (its : list (spec * option aexpr)) : bool :=
  forallb (fun p => spec_ok (fst p) && optional expr_ok (snd p)) its.
Definition recv_ok (r : fnrecv) : bool :=
  match r with RAttached n => negb (String.eqb n "_") && negb (String.eqb n ".") | _ => true end.
Fixpoint decl_ok (d : adecl) : bool :=
  match d with
  | ADAssume its ds => forallb (fun p => path_ok (fst p) && opt_name_ok (snd p)) its && forallb decl_ok ds
  | ADAlias _ t | ADTag _ _ t => ty_ok t
  | ADVal _ t e => ty_ok t && optional expr_ok e
  | ADFn r _ ps ret b => recv_ok r && forallb (fun p => ty_ok (snd p)) ps && optional ty_ok ret && optional block_ok b
  | ADCtx _ its => forallb spec_ok its
  | ADFunctor _ _ args res body => forallb spec_ok args && forallb spec_ok res && optional (forallb bind_ok) body
  | _ => true
  end.
Definition use_side_ok (p : bool * name) : bool :=
  fst p || negb (String.prefix "." (snd p)).
Definition use_ok (u : ause) : bool :=
  forallb (fun c => negb (Ascii.eqb c ">"%char)) (list_ascii_of_string (u_name u)) &&
  use_side_ok (u_dot u, u_name u) && optional use_side_ok (u_alias u).
Definition import_ok (i : aimport) : bool := opt_name_ok (i_alias i) && forallb use_ok (i_uses i).
Definition wf_names (f : afile) : bool := forallb import_ok (f_imports f) && forallb decl_ok (f_decls f).

Lemma path_ok_read p : path_ok p = true -> split_path (join_path p) = p.
Proof. apply strings_eqb_eq. Qed.
Lemma opt_atom_dump o : opt_name_ok o = true -> opt_atom (dopt SA o) = Good o.
Proof.
  destruct o as [n|]; [|reflexivity]. cbn [opt_name_ok].
  intro H. apply negb_true_iff, String.eqb_neq in H.
  unfold dopt, opt_atom. destruct n as [|c n]; [reflexivity|].
  destruct c as [b0 b1 b2 b3 b4 b5 b6 b7].
  destruct b0, b1, b2, b3, b4, b5, b6, b7; try reflexivity.
  destruct n as [|c' n]; [exfalso; apply H; reflexivity|reflexivity].
Qed.
Lemma opt_path_dump o : opt_path_ok o = true -> opt_path (dopt dpath o) = Good o.
Proof.
  destruct o as [p|]; [|reflexivity]. cbn [opt_path_ok].
  rewrite andb_true_iff. intros [Hp Hn].
  unfold opt_path.
  change (rbind (opt_atom (dopt SA (Some (join_path p))))
    (fun o => Good (option_map split_path o)) = Good (Some p)).
  rewrite (opt_atom_dump (Some (join_path p)) Hn). cbn [rbind option_map].
  rewrite (path_ok_read p Hp). reflexivity.
Qed.

Lemma dump_bind_inv {A B} (r : res A) (k : A -> res B) y :
  rbind r k = Good y -> exists x, r = Good x /\ k x = Good y.
Proof. destruct r as [x|m]; cbn; [eauto|discriminate]. Qed.

Definition Inverse {A} (read : sx -> res A) (dump : A -> sx) (ok : A -> bool) : Prop :=
  forall x y, ok x = true -> read (dump x) = Good y -> y = x.

Lemma rmap_inverse {A} (read : sx -> res A) (dump : A -> sx) (ok : A -> bool) :
  Inverse read dump ok -> forall xs ys,
  forallb ok xs = true -> rmap read (map dump xs) = Good ys -> ys = xs.
Proof.
  intros Hi xs. induction xs as [|x xs IH]; intros ys Hok H; cbn in H;
    [injection H as <-; reflexivity|].
  apply andb_true_iff in Hok as [Hx Hxs].
  apply dump_bind_inv in H as (y & Hy & H).
  apply dump_bind_inv in H as (ys' & Hys & H).
  injection H as <-. rewrite (Hi x y Hx Hy), (IH ys' Hxs Hys). reflexivity.
Qed.

Theorem spec_app_inverse : forall fuel,
  Inverse (read_spec fuel) dspec spec_ok /\ Inverse (read_app fuel) dapp app_ok.
Proof.
  induction fuel as [|fuel [IHs IHa]].
  - split; intros x y _ H; discriminate H.
  - split.
    + intros [p d a] y Hok H.
      change ((opt_path_ok p && opt_name_ok d) && app_ok a = true) in Hok.
      apply andb_true_iff in Hok as [Hpd Ha]. apply andb_true_iff in Hpd as [Hp Hd].
      change (rbind (opt_path (dopt dpath p)) (fun p' =>
        rbind (opt_atom (dopt SA d)) (fun d' =>
        rbind (read_app fuel (dapp a)) (fun a' => Good (Spec p' d' a')))) = Good y) in H.
      rewrite (opt_path_dump p Hp), (opt_atom_dump d Hd) in H.
      cbn [rbind] in H. apply dump_bind_inv in H as (a' & H1 & H2).
      rewrite (IHa a a' Ha H1) in H2. injection H2 as <-. reflexivity.
    + intros [bs|] y Hok H; [|injection H as <-; reflexivity].
      change (rbind (rmap (fun b => match b with
        | SL false [SA "J"; SA p; s] =>
            rbind (read_spec fuel s) (fun s' => Good (split_path p, s'))
        | _ => bad "binding" end)
        (map (fun '(p, s) => nd "J" [dpath p; dspec s]) bs))
        (fun l => Good (Some l)) = Good y) in H.
      apply dump_bind_inv in H as (bs' & H1 & H2).
      assert (Hb : bs' = bs).
      { eapply rmap_inverse; [|exact Hok|exact H1].
        intros [p s] [p' s'] Hk Hr.
        apply andb_true_iff in Hk as [Hp Hs].
        change (rbind (read_spec fuel (dspec s))
          (fun s0 => Good (split_path (join_path p), s0)) = Good (p', s')) in Hr.
        apply dump_bind_inv in Hr as (s0 & Hs0 & E).
        rewrite (IHs s s0 Hs Hs0), (path_ok_read p Hp) in E.
        injection E as <- <-. reflexivity. }
      subst bs'. injection H2 as <-. reflexivity.
Qed.

Corollary spec_inverse fuel : Inverse (read_spec fuel) dspec spec_ok.
Proof. exact (proj1 (spec_app_inverse fuel)). Qed.
Corollary app_inverse fuel : Inverse (read_app fuel) dapp app_ok.
Proof. exact (proj2 (spec_app_inverse fuel)). Qed.

Theorem ty_inverse : forall fuel, Inverse (read_ty fuel) dty ty_ok.
Proof.
  induction fuel as [|fuel IH]; intros t y Hok H; [discriminate H|].
  destruct t as [| | |fs|ms|p a].
  - injection H as <-. reflexivity.
  - injection H as <-. reflexivity.
  - injection H as <-. reflexivity.
  - change (rbind (rmap (fun f => match f with
      | SL false [SA "h"; SA n; t] => rbind (read_ty fuel t) (fun t' => Good (n, t'))
      | _ => bad "record field type" end)
      (map (fun '(x,t) => nd "h" [SA x; dty t]) fs))
      (fun fs' => Good (ATRec fs')) = Good y) in H.
    apply dump_bind_inv in H as (fs' & Hr & E).
    assert (Ef : fs' = fs).
    { eapply rmap_inverse; [|exact Hok|exact Hr].
      intros [n t] [n' t'] Hk Ht.
      change (rbind (read_ty fuel (dty t)) (fun t0 => Good (n, t0)) = Good (n',t')) in Ht.
      apply dump_bind_inv in Ht as (t0 & Ht0 & Et).
      rewrite (IH t t0 Hk Ht0) in Et. injection Et as <- <-. reflexivity. }
    subst fs'. injection E as <-. reflexivity.
  - change (rbind (rmap (read_ty fuel) (map dty ms))
      (fun ms' => Good (ATUnion ms')) = Good y) in H.
    apply dump_bind_inv in H as (ms' & Hr & E).
    rewrite (rmap_inverse _ _ _ IH ms ms' Hok Hr) in E. injection E as <-. reflexivity.
  - apply andb_true_iff in Hok as [Hp Ha].
    change (rbind (read_app fuel (dapp a))
      (fun a' => Good (ATRef (split_path (join_path p)) a')) = Good y) in H.
    apply dump_bind_inv in H as (a' & Ha' & E).
    rewrite (app_inverse fuel a a' Ha Ha'), (path_ok_read p Hp) in E.
    injection E as <-. reflexivity.
Qed.

Definition optional_read {A} (read : sx -> res A) (x : sx) : res (option A) :=
  match x with SA "_" => Good None | _ => rbind (read x) (fun y => Good (Some y)) end.
Definition dumps_list {A} (dump : A -> sx) : Prop := forall x, exists b xs, dump x = SL b xs.

Lemma optional_inverse {A} (read : sx -> res A) (dump : A -> sx) (ok : A -> bool) :
  dumps_list dump -> Inverse read dump ok ->
  Inverse (optional_read read) (dopt dump) (optional ok).
Proof.
  intros Hl Hi [x|] y Hok H; [|injection H as <-; reflexivity].
  destruct (Hl x) as (b & xs & E).
  unfold dopt in H. rewrite E in H. cbn [optional_read] in H. rewrite <- E in H.
  apply dump_bind_inv in H as (x' & Hr & Er).
  rewrite (Hi x x' Hok Hr) in Er. injection Er as <-. reflexivity.
Qed.

Lemma ty_dumps_list : dumps_list dty.
Proof. intros []; repeat eexists; reflexivity. Qed.
Lemma pat_dumps_list : dumps_list dpat.
Proof. intros []; repeat eexists; reflexivity. Qed.
Lemma expr_dumps_list : dumps_list dexpr.
Proof. intros []; try (repeat eexists; reflexivity). destruct a; repeat eexists; reflexivity. Qed.
Lemma block_dumps_list : dumps_list dblock.
Proof. intros []; repeat eexists; reflexivity. Qed.

Lemma opt_ty_inverse fuel : Inverse (read_opt_ty fuel) (dopt dty) (optional ty_ok).
Proof. exact (optional_inverse _ _ _ ty_dumps_list (ty_inverse fuel)). Qed.

Lemma read_dump_tag fuel q p :
  read_pat (S fuel) (nd "E" [SA q; dpat p]) =
  rbind (read_pat fuel (dpat p)) (fun p' => Good (APTag (split_path q) p')).
Proof. destruct p; reflexivity. Qed.

Theorem pat_inverse : forall fuel, Inverse (read_pat fuel) dpat pat_ok.
Proof.
  induction fuel as [|fuel IH]; intros p y Hok H; [discriminate H|].
  destruct p as [|q|q p|q fs].
  - injection H as <-. reflexivity.
  - change (Good (APPath (split_path (join_path q))) = Good y) in H.
    rewrite (path_ok_read q Hok) in H. injection H as <-. reflexivity.
  - apply andb_true_iff in Hok as [Hq Hp].
    change (read_pat (S fuel) (nd "E" [SA (join_path q); dpat p]) = Good y) in H.
    rewrite read_dump_tag in H. apply dump_bind_inv in H as (p' & Hp' & E).
    rewrite (IH p p' Hp Hp'), (path_ok_read q Hq) in E. injection E as <-. reflexivity.
  - apply andb_true_iff in Hok as [Hq Hfs].
    change (rbind (opt_path (dopt dpath q)) (fun q' =>
      rbind (rmap (fun f => match f with
        | SL false [SA "G"; SA n; SA "_"] => Good (n, None)
        | SL false [SA "G"; SA n; s] => rbind (read_pat fuel s) (fun s' => Good (n, Some s'))
        | _ => bad "pattern field" end)
        (map (fun '(n,p) => nd "G" [SA n; dopt dpat p]) fs))
        (fun fs' => Good (APRec q' fs'))) = Good y) in H.
    rewrite (opt_path_dump q Hq) in H. cbn [rbind] in H.
    apply dump_bind_inv in H as (fs' & Hr & E).
    assert (Ef : fs' = fs).
    { eapply rmap_inverse; [|exact Hfs|exact Hr].
      intros [n p] [n' p'] Hk Hf.
      destruct p as [p|]; [|injection Hf as <- <-; reflexivity].
      destruct (pat_dumps_list p) as (b & xs & Ep).
      cbn [nd dopt] in Hf.
      change (match dpat p with
              | SA "_" => Good (n, None)
              | _ => rbind (read_pat fuel (dpat p)) (fun p0 => Good (n, Some p0))
              end = Good (n', p')) in Hf.
      rewrite Ep in Hf. cbn in Hf. rewrite <- Ep in Hf.
      apply dump_bind_inv in Hf as (p0 & Hp0 & Et).
      rewrite (IH p p0 Hk Hp0) in Et. injection Et as <- <-. reflexivity. }
    subst fs'. injection E as <-. reflexivity.
Qed.

Lemma read_dump_return fuel e :
  read_expr (S fuel) (nd "s" [dexpr e]) =
  rbind (read_expr fuel (dexpr e)) (fun e' => Good (AEReturn (Some e'))).
Proof.
  destruct (expr_dumps_list e) as (b & xs & E).
  unfold nd. rewrite E. cbn [read_expr]. reflexivity.
Qed.
Lemma read_dump_block_expr fuel b :
  read_expr (S fuel) (dblock b) =
  rbind (read_block fuel (dblock b)) (fun b' => Good (AEBlock b')).
Proof. destruct b. reflexivity. Qed.

Theorem expr_group_inverse : forall fuel,
  Inverse (read_expr fuel) dexpr expr_ok /\
  Inverse (read_block fuel) dblock block_ok /\
  Inverse (read_stmt fuel) dstmt stmt_ok /\
  Inverse (read_bind fuel) dbind bind_ok.
Proof.
  induction fuel as [|fuel [IHe [IHb [IHs IHi]]]].
  { repeat split; intros x y _ H; discriminate H. }
  repeat split.
  - intros e y Hok H.
    destruct e as [| |p a|p a args|o f|o p args|p a fs|c b el|e arms|o| |b|op e|op l r|txt|ps ret body].
    + injection H as <-. reflexivity.
    + injection H as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hp Ha].
      change (rbind (read_app fuel (dapp a)) (fun a' => Good (AEPath (split_path (join_path p)) a')) = Good y) in H.
      apply dump_bind_inv in H as (a' & Ha' & E).
      rewrite (app_inverse fuel a a' Ha Ha'), (path_ok_read p Hp) in E.
      injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hpa Hargs]. apply andb_true_iff in Hpa as [Hp Ha].
      change (rbind (read_app fuel (dapp a)) (fun a' =>
        rbind (rmap (read_expr fuel) (map dexpr args))
        (fun args' => Good (AECall (split_path (join_path p)) a' args'))) = Good y) in H.
      apply dump_bind_inv in H as (a' & Ha' & H).
      apply dump_bind_inv in H as (args' & Hr & E).
      rewrite (app_inverse fuel a a' Ha Ha'), (path_ok_read p Hp),
        (rmap_inverse _ _ _ IHe args args' Hargs Hr) in E.
      injection E as <-. reflexivity.
    + change (rbind (read_expr fuel (dexpr o)) (fun o' => Good (AEField o' f)) = Good y) in H.
      apply dump_bind_inv in H as (o' & Ho & E). rewrite (IHe o o' Hok Ho) in E.
      injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hop Hargs]. apply andb_true_iff in Hop as [Ho Hp].
      change (rbind (read_expr fuel (dexpr o)) (fun o' =>
        rbind (rmap (read_expr fuel) (map dexpr args))
        (fun args' => Good (AEMeth o' (split_path (join_path p)) args'))) = Good y) in H.
      apply dump_bind_inv in H as (o' & Ho' & H).
      apply dump_bind_inv in H as (args' & Hr & E).
      rewrite (IHe o o' Ho Ho'), (path_ok_read p Hp),
        (rmap_inverse _ _ _ IHe args args' Hargs Hr) in E.
      injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hpa Hfs]. apply andb_true_iff in Hpa as [Hp Ha].
      change (rbind (read_app fuel (dapp a)) (fun a' =>
        rbind (rmap (fun f => match f with
          | SL false [SA "p"; SA n; SA "_"] => Good (n,None)
          | SL false [SA "p"; SA n; v] => rbind (read_expr fuel v) (fun v' => Good (n,Some v'))
          | _ => bad "record field" end)
          (map (fun '(n,v) => nd "p" [SA n; dopt dexpr v]) fs))
          (fun fs' => Good (AERec (split_path (join_path p)) a' fs'))) = Good y) in H.
      apply dump_bind_inv in H as (a' & Ha' & H).
      apply dump_bind_inv in H as (fs' & Hr & E).
      assert (Ef : fs' = fs).
      { eapply rmap_inverse; [|exact Hfs|exact Hr].
        intros [n v] [n' v'] Hk Hv. destruct v as [v|]; [|injection Hv as <- <-; reflexivity].
        destruct (expr_dumps_list v) as (b0 & xs & Ev).
        cbn [nd dopt] in Hv. rewrite Ev in Hv. cbn in Hv. rewrite <- Ev in Hv.
        apply dump_bind_inv in Hv as (v0 & Hv0 & Et).
        rewrite (IHe v v0 Hk Hv0) in Et. injection Et as <- <-. reflexivity. }
      rewrite Ef, (app_inverse fuel a a' Ha Ha'), (path_ok_read p Hp) in E.
      injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hcb Hel]. apply andb_true_iff in Hcb as [Hc Hb].
      change (rbind (read_expr fuel (dexpr c)) (fun c' =>
        rbind (read_block fuel (dblock b)) (fun b' =>
        rbind (optional_read (read_expr fuel) (dopt dexpr el))
        (fun el' => Good (AEIf c' b' el')))) = Good y) in H.
      apply dump_bind_inv in H as (c' & Hc' & H).
      apply dump_bind_inv in H as (b' & Hb' & H).
      apply dump_bind_inv in H as (el' & Hel' & E).
      rewrite (IHe c c' Hc Hc'), (IHb b b' Hb Hb'),
        (optional_inverse _ _ _ expr_dumps_list IHe el el' Hel Hel') in E.
      injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [He Harms].
      change (rbind (read_expr fuel (dexpr e)) (fun e' =>
        rbind (rmap (fun a => match a with
          | SL false [SA "H"; p; b] => rbind (read_pat fuel p) (fun p' =>
              rbind (read_expr fuel b) (fun b' => Good (p',b')))
          | _ => bad "arm" end)
          (map (fun '(p,b) => nd "H" [dpat p; dexpr b]) arms))
          (fun arms' => Good (AEMatch e' arms'))) = Good y) in H.
      apply dump_bind_inv in H as (e' & He' & H).
      apply dump_bind_inv in H as (arms' & Hr & E).
      assert (Ea : arms' = arms).
      { eapply rmap_inverse; [|exact Harms|exact Hr].
        intros [p b] [p' b'] Hk Ha. apply andb_true_iff in Hk as [Hp Hb].
        change (rbind (read_pat fuel (dpat p)) (fun p0 =>
          rbind (read_expr fuel (dexpr b)) (fun b0 => Good (p0,b0))) = Good (p',b')) in Ha.
        apply dump_bind_inv in Ha as (p0 & Hp0 & Ha).
        apply dump_bind_inv in Ha as (b0 & Hb0 & Et).
        rewrite (pat_inverse fuel p p0 Hp Hp0), (IHe b b0 Hb Hb0) in Et.
        injection Et as <- <-. reflexivity. }
      rewrite Ea, (IHe e e' He He') in E. injection E as <-. reflexivity.
    + destruct o as [e|]; [|injection H as <-; reflexivity].
      change (read_expr (S fuel) (nd "s" [dexpr e]) = Good y) in H.
      rewrite read_dump_return in H. apply dump_bind_inv in H as (e' & He' & E).
      rewrite (IHe e e' Hok He') in E. injection E as <-. reflexivity.
    + injection H as <-. reflexivity.
    + change (read_expr (S fuel) (dblock b) = Good y) in H.
      rewrite read_dump_block_expr in H. apply dump_bind_inv in H as (b' & Hb' & E).
      rewrite (IHb b b' Hok Hb') in E. injection E as <-. reflexivity.
    + change (rbind (read_expr fuel (dexpr e)) (fun e' => Good (AEUnary op e')) = Good y) in H.
      apply dump_bind_inv in H as (e' & He' & E).
      rewrite (IHe e e' Hok He') in E. injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hl Hr].
      change (rbind (read_expr fuel (dexpr l)) (fun l' =>
        rbind (read_expr fuel (dexpr r)) (fun r' => Good (AEBinary op l' r'))) = Good y) in H.
      apply dump_bind_inv in H as (l' & Hl' & H).
      apply dump_bind_inv in H as (r' & Hr' & E).
      rewrite (IHe l l' Hl Hl'), (IHe r r' Hr Hr') in E. injection E as <-. reflexivity.
    + injection H as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hpsr Hb]. apply andb_true_iff in Hpsr as [Hps Hret].
      change (rbind (rmap (fun p => match p with
          | SL false [SA "K"; SA n; t] => rbind (read_opt_ty fuel t) (fun t' => Good (n,t'))
          | _ => bad "lambda parameter" end)
          (map (fun '(n,t) => nd "K" [SA n; dopt dty t]) ps)) (fun ps' =>
        rbind (read_opt_ty fuel (dopt dty ret)) (fun ret' =>
        rbind (read_expr fuel (dexpr body)) (fun body' => Good (AELam ps' ret' body')))) = Good y) in H.
      apply dump_bind_inv in H as (ps' & Hpr & H).
      apply dump_bind_inv in H as (ret' & Hret' & H).
      apply dump_bind_inv in H as (body' & Hb' & E).
      assert (Ep : ps' = ps).
      { eapply rmap_inverse; [|exact Hps|exact Hpr].
        intros [n t] [n' t'] Hk Ht.
        change (rbind (read_opt_ty fuel (dopt dty t)) (fun t0 => Good (n,t0)) = Good (n',t')) in Ht.
        apply dump_bind_inv in Ht as (t0 & Ht0 & Et).
        rewrite (opt_ty_inverse fuel t t0 Hk Ht0) in Et. injection Et as <- <-. reflexivity. }
      rewrite Ep, (opt_ty_inverse fuel ret ret' Hret Hret'), (IHe body body' Hb Hb') in E.
      injection E as <-. reflexivity.
  - intros [ss t] y Hok H. apply andb_true_iff in Hok as [Hs Ht].
    change (rbind (rmap (read_stmt fuel) (map dstmt ss)) (fun ss' =>
      rbind (optional_read (read_expr fuel) (dopt dexpr t))
      (fun t' => Good (ABlock ss' t'))) = Good y) in H.
    apply dump_bind_inv in H as (ss' & Hs' & H).
    apply dump_bind_inv in H as (t' & Ht' & E).
    rewrite (rmap_inverse _ _ _ IHs ss ss' Hs Hs'),
      (optional_inverse _ _ _ expr_dumps_list IHe t t' Ht Ht') in E.
    injection E as <-. reflexivity.
  - intros s y Hok H. destruct s as [n t e|n t e|n e|its|c b|b|e].
    + apply andb_true_iff in Hok as [Ht He].
      change (rbind (read_opt_ty fuel (dopt dty t)) (fun t' =>
        rbind (read_expr fuel (dexpr e)) (fun e' => Good (ASLet n t' e'))) = Good y) in H.
      apply dump_bind_inv in H as (t' & Ht' & H).
      apply dump_bind_inv in H as (e' & He' & E).
      rewrite (opt_ty_inverse fuel t t' Ht Ht'), (IHe e e' He He') in E.
      injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [Ht He].
      change (rbind (read_opt_ty fuel (dopt dty t)) (fun t' =>
        rbind (read_expr fuel (dexpr e)) (fun e' => Good (ASVar n t' e'))) = Good y) in H.
      apply dump_bind_inv in H as (t' & Ht' & H).
      apply dump_bind_inv in H as (e' & He' & E).
      rewrite (opt_ty_inverse fuel t t' Ht Ht'), (IHe e e' He He') in E.
      injection E as <-. reflexivity.
    + change (rbind (read_expr fuel (dexpr e)) (fun e' => Good (ASAssign n e')) = Good y) in H.
      apply dump_bind_inv in H as (e' & He' & E).
      rewrite (IHe e e' Hok He') in E. injection E as <-. reflexivity.
    + change (rbind (read_bind fuel (dbind its)) (fun its' => Good (ASBind its')) = Good y) in H.
      apply dump_bind_inv in H as (its' & Hi' & E).
      rewrite (IHi its its' Hok Hi') in E. injection E as <-. reflexivity.
    + apply andb_true_iff in Hok as [Hc Hb].
      change (rbind (read_expr fuel (dexpr c)) (fun c' =>
        rbind (read_block fuel (dblock b)) (fun b' => Good (ASWhile c' b'))) = Good y) in H.
      apply dump_bind_inv in H as (c' & Hc' & H).
      apply dump_bind_inv in H as (b' & Hb' & E).
      rewrite (IHe c c' Hc Hc'), (IHb b b' Hb Hb') in E. injection E as <-. reflexivity.
    + change (rbind (read_block fuel (dblock b)) (fun b' => Good (ASLoop b')) = Good y) in H.
      apply dump_bind_inv in H as (b' & Hb' & E).
      rewrite (IHb b b' Hok Hb') in E. injection E as <-. reflexivity.
    + change (rbind (read_expr fuel (dexpr e)) (fun e' => Good (ASExpr e')) = Good y) in H.
      apply dump_bind_inv in H as (e' & He' & E).
      rewrite (IHe e e' Hok He') in E. injection E as <-. reflexivity.
  - intros its ys Hok H.
    change (rmap (fun it => match it with
      | SL false [SA "z"; s; SA "_"] => rbind (read_spec fuel s) (fun s' => Good (s',None))
      | SL false [SA "z"; s; v] => rbind (read_spec fuel s) (fun s' =>
          rbind (read_expr fuel v) (fun v' => Good (s',Some v')))
      | _ => bad "bind item" end)
      (map (fun '(s,v) => nd "z" [dspec s; dopt dexpr v]) its) = Good ys) in H.
    eapply rmap_inverse; [|exact Hok|exact H].
    intros [s v] [s' v'] Hk Hi. apply andb_true_iff in Hk as [Hs Hv].
    destruct v as [v|].
    + destruct (expr_dumps_list v) as (b & xs & Ev).
      cbn [nd dopt] in Hi. rewrite Ev in Hi. cbn in Hi. rewrite <- Ev in Hi.
      apply dump_bind_inv in Hi as (s0 & Hs0 & Hi).
      apply dump_bind_inv in Hi as (v0 & Hv0 & E).
      rewrite (spec_inverse fuel s s0 Hs Hs0), (IHe v v0 Hv Hv0) in E.
      injection E as <- <-. reflexivity.
    + change (rbind (read_spec fuel (dspec s)) (fun s0 => Good (s0,None)) = Good (s',v')) in Hi.
      apply dump_bind_inv in Hi as (s0 & Hs0 & E).
      rewrite (spec_inverse fuel s s0 Hs Hs0) in E. injection E as <- <-. reflexivity.
Qed.

Corollary expr_inverse fuel : Inverse (read_expr fuel) dexpr expr_ok.
Proof. exact (proj1 (expr_group_inverse fuel)). Qed.
Corollary block_inverse fuel : Inverse (read_block fuel) dblock block_ok.
Proof. exact (proj1 (proj2 (expr_group_inverse fuel))). Qed.
Corollary stmt_inverse fuel : Inverse (read_stmt fuel) dstmt stmt_ok.
Proof. exact (proj1 (proj2 (proj2 (expr_group_inverse fuel)))). Qed.
Corollary bind_inverse fuel : Inverse (read_bind fuel) dbind bind_ok.
Proof. exact (proj2 (proj2 (proj2 (expr_group_inverse fuel)))). Qed.

Lemma rmap_atoms_dump xs : rmap atom (map SA xs) = Good xs.
Proof. induction xs as [|x xs IH]; [reflexivity|]. cbn [map rmap atom rbind]. rewrite IH. reflexivity. Qed.
Definition read_receiver (s : string) : fnrecv :=
  match s with "." => RDetached | "_" => RPlain | _ => RAttached s end.
Lemma receiver_dump r : recv_ok r = true ->
  read_receiver (match r with RPlain => "_" | RDetached => "." | RAttached n => n end) = r.
Proof.
  destruct r as [| |n]; [reflexivity|reflexivity|].
  cbn [recv_ok]. rewrite andb_true_iff. intros [Hu Hd].
  apply negb_true_iff, String.eqb_neq in Hu, Hd.
  destruct n as [|[b0 b1 b2 b3 b4 b5 b6 b7] n]; [reflexivity|].
  destruct b0, b1, b2, b3, b4, b5, b6, b7; try reflexivity;
    destruct n as [|c n]; try reflexivity; exfalso;
    first [apply Hu; reflexivity|apply Hd; reflexivity].
Qed.

Theorem decl_inverse : forall fuel, Inverse (read_decl fuel) ddecl decl_ok.
Proof.
  induction fuel as [|fuel IH]; intros d y Hok H; [discriminate H|].
  destruct d as [its ds|n|n t|n ps t|n|n t init|rcv n ps ret body|n ss|n ps args res body].
  - apply andb_true_iff in Hok as [Hits Hds].
    change (rbind (rmap (fun it => match it with
      | SL false [SA "I"; SA p; d; SA "_"] => rbind (opt_atom d) (fun d' => Good (split_path p,d'))
      | _ => bad "assume item" end)
      (map (fun '(p,d) => nd "I" [dpath p; dopt SA d; SA "_"]) its)) (fun its' =>
      rbind (rmap (read_decl fuel) (map ddecl ds)) (fun ds' => Good (ADAssume its' ds'))) = Good y) in H.
    apply dump_bind_inv in H as (its' & Hitr & H).
    apply dump_bind_inv in H as (ds' & Hdr & E).
    assert (Ei : its' = its).
    { eapply rmap_inverse; [|exact Hits|exact Hitr].
      intros [p d] [p' d'] Hk Hi. apply andb_true_iff in Hk as [Hp Hd].
      change (rbind (opt_atom (dopt SA d)) (fun d0 => Good (split_path (join_path p),d0)) = Good (p',d')) in Hi.
      rewrite (opt_atom_dump d Hd), (path_ok_read p Hp) in Hi. cbn [rbind] in Hi.
      injection Hi as <- <-. reflexivity. }
    rewrite Ei, (rmap_inverse _ _ _ IH ds ds' Hds Hdr) in E. injection E as <-. reflexivity.
  - injection H as <-. reflexivity.
  - change (rbind (read_ty fuel (dty t)) (fun t' => Good (ADAlias n t')) = Good y) in H.
    apply dump_bind_inv in H as (t' & Ht & E).
    rewrite (ty_inverse fuel t t' Hok Ht) in E. injection E as <-. reflexivity.
  - change (rbind (rmap atom (map SA ps)) (fun ps' =>
      rbind (read_ty fuel (dty t)) (fun t' => Good (ADTag n ps' t'))) = Good y) in H.
    rewrite rmap_atoms_dump in H. cbn [rbind] in H.
    apply dump_bind_inv in H as (t' & Ht & E).
    rewrite (ty_inverse fuel t t' Hok Ht) in E. injection E as <-. reflexivity.
  - injection H as <-. reflexivity.
  - apply andb_true_iff in Hok as [Ht Hi].
    change (rbind (read_ty fuel (dty t)) (fun t' =>
      rbind (optional_read (read_expr fuel) (dopt dexpr init))
      (fun init' => Good (ADVal n t' init'))) = Good y) in H.
    apply dump_bind_inv in H as (t' & Ht' & H).
    apply dump_bind_inv in H as (init' & Hi' & E).
    rewrite (ty_inverse fuel t t' Ht Ht'),
      (optional_inverse _ _ _ expr_dumps_list (expr_inverse fuel) init init' Hi Hi') in E.
    injection E as <-. reflexivity.
  - apply andb_true_iff in Hok as [Hpr Hb].
    apply andb_true_iff in Hpr as [Hrp Hr]. apply andb_true_iff in Hrp as [Hrcv Hp].
    change (rbind (rmap (fun p => match p with
        | SL false [SA "K"; SA pn; t] => rbind (read_ty fuel t) (fun t' => Good (pn,t'))
        | _ => bad "parameter" end)
        (map (fun '(p,t) => nd "K" [SA p; dty t]) ps)) (fun ps' =>
      rbind (read_opt_ty fuel (dopt dty ret)) (fun ret' =>
      rbind (optional_read (read_block fuel) (dopt dblock body)) (fun body' =>
      Good (ADFn (read_receiver (match rcv with RPlain => "_" | RDetached => "." | RAttached q => q end))
        n ps' ret' body')))) = Good y) in H.
    rewrite (receiver_dump rcv Hrcv) in H.
    apply dump_bind_inv in H as (ps' & Hps & H).
    apply dump_bind_inv in H as (ret' & Hr' & H).
    apply dump_bind_inv in H as (body' & Hb' & E).
    assert (Ep : ps' = ps).
    { eapply rmap_inverse; [|exact Hp|exact Hps].
      intros [pn t] [pn' t'] Hk Hp0.
      change (rbind (read_ty fuel (dty t)) (fun t0 => Good (pn,t0)) = Good (pn',t')) in Hp0.
      apply dump_bind_inv in Hp0 as (t0 & Ht0 & Et).
      rewrite (ty_inverse fuel t t0 Hk Ht0) in Et. injection Et as <- <-. reflexivity. }
    rewrite Ep, (opt_ty_inverse fuel ret ret' Hr Hr'),
      (optional_inverse _ _ _ block_dumps_list (block_inverse fuel) body body' Hb Hb') in E.
    injection E as <-. reflexivity.
  - change (rbind (rmap (read_spec fuel) (map dspec ss)) (fun ss' => Good (ADCtx n ss')) = Good y) in H.
    apply dump_bind_inv in H as (ss' & Hs & E).
    rewrite (rmap_inverse _ _ _ (spec_inverse fuel) ss ss' Hok Hs) in E.
    injection E as <-. reflexivity.
  - apply andb_true_iff in Hok as [Har Hb]. apply andb_true_iff in Har as [Ha Hr].
    assert (Hps : (match dopt (fun l => sl (map SA l)) ps with
                      | SA "_" => Good None
                      | SL true xs => rbind (rmap atom xs) (fun xs' => Good (Some xs'))
                      | _ => bad "functor" end) = Good ps).
    { destruct ps as [ps|]; [|reflexivity]. cbn [dopt sl]. rewrite rmap_atoms_dump. reflexivity. }
    change (rbind (match dopt (fun l => sl (map SA l)) ps with
                      | SA "_" => Good (None,map dspec args,map dspec res,dopt (fun bs => sl (map dbind bs)) body)
                      | SL true xs => rbind (rmap atom xs) (fun xs' =>
                          Good (Some xs',map dspec args,map dspec res,dopt (fun bs => sl (map dbind bs)) body))
                      | _ => bad "functor" end)
      (fun '(ps',args0,res0,body0) =>
        rbind (rmap (read_spec fuel) args0) (fun args' =>
        rbind (rmap (read_spec fuel) res0) (fun res' =>
        rbind (match body0 with
               | SA "_" => Good None
               | SL true bs => rbind (rmap (read_bind fuel) bs) (fun bs' => Good (Some bs'))
               | _ => bad "functor body" end)
          (fun body' => Good (ADFunctor n ps' args' res' body'))))) = Good y) in H.
    destruct ps as [ps|]; cbn [dopt sl] in H;
      try rewrite rmap_atoms_dump in H; cbn [rbind] in H.
    all: apply dump_bind_inv in H as (args' & Ha' & H).
    all: apply dump_bind_inv in H as (res' & Hr' & H).
    all: apply dump_bind_inv in H as (body' & Hb' & E).
    all: assert (Eb : body' = body).
    all: try (destruct body as [bs|]; [|injection Hb' as <-; reflexivity];
      cbn [dopt sl] in Hb'; apply dump_bind_inv in Hb' as (bs' & Hbs & Eb0);
      rewrite (rmap_inverse _ _ _ (bind_inverse fuel) bs bs' Hb Hbs) in Eb0;
      injection Eb0 as <-; reflexivity).
    all: rewrite Eb, (rmap_inverse _ _ _ (spec_inverse fuel) args args' Ha Ha'),
      (rmap_inverse _ _ _ (spec_inverse fuel) res res' Hr Hr') in E.
    all: injection E as <-; reflexivity.
Qed.

Lemma ascii_list_append a b : list_ascii_of_string (a ++ b) =
  (list_ascii_of_string a ++ list_ascii_of_string b)%list.
Proof. induction a as [|c a IH]; [reflexivity|]. cbn. f_equal. exact IH. Qed.
Lemma split_gt_cons c r acc : c <> ">"%char ->
  split_gt (c :: r) acc = split_gt r (c :: acc).
Proof.
  intro H. destruct c as [b0 b1 b2 b3 b4 b5 b6 b7].
  destruct b0,b1,b2,b3,b4,b5,b6,b7; try reflexivity.
  exfalso. apply H. reflexivity.
Qed.
Lemma split_gt_prefix cs r acc :
  forallb (fun c => negb (Ascii.eqb c ">"%char)) cs = true ->
  split_gt (cs ++ r) acc = split_gt r (rev cs ++ acc).
Proof.
  revert acc. induction cs as [|c cs IH]; intros acc H; [reflexivity|].
  apply andb_true_iff in H as [Hc Hcs]. cbn [app].
  rewrite split_gt_cons by (intro E; subst c; discriminate Hc).
  rewrite (IH (c :: acc) Hcs). cbn [rev]. rewrite <- app_assoc. reflexivity.
Qed.
Lemma use_side_dump d n : use_side_ok (d,n) = true ->
  read_use_side (duse_side d n) = (d,n).
Proof.
  destruct d; [reflexivity|]. cbn [use_side_ok fst snd orb]. intro H.
  destruct n as [|[b0 b1 b2 b3 b4 b5 b6 b7] n]; [reflexivity|].
  destruct b0,b1,b2,b3,b4,b5,b6,b7; try reflexivity.
  destruct n; discriminate H.
Qed.
Lemma use_side_nogt d n :
  forallb (fun c => negb (Ascii.eqb c ">"%char)) (list_ascii_of_string n) = true ->
  forallb (fun c => negb (Ascii.eqb c ">"%char)) (list_ascii_of_string (duse_side d n)) = true.
Proof. destruct d; cbn [duse_side append list_ascii_of_string forallb]; auto. Qed.

Lemma use_dump u : use_ok u = true -> read_use (duse u) = Good u.
Proof.
  destruct u as [d n alias]. cbn [use_ok u_dot u_name u_alias].
  change ((forallb (fun c => negb (Ascii.eqb c ">"%char)) (list_ascii_of_string n) &&
    use_side_ok (d,n)) && optional use_side_ok alias = true ->
    read_use (duse (AUse d n alias)) = Good (AUse d n alias)).
  rewrite !andb_true_iff. intros [[Hn Hd] Ha].
  pose proof (use_side_nogt d n Hn) as Hside.
  unfold read_use, duse. cbn [u_dot u_name u_alias atom rbind].
  rewrite ascii_list_append, (split_gt_prefix _ _ [] Hside).
  destruct alias as [[d' n']|].
  - cbn [append list_ascii_of_string split_gt]. rewrite app_nil_r, rev_involutive.
    rewrite string_of_list_ascii_of_string, (use_side_dump d n Hd).
    cbn [option_map].
    rewrite string_of_list_ascii_of_string, (use_side_dump d' n' Ha). reflexivity.
  - cbn [append list_ascii_of_string split_gt]. rewrite app_nil_r, rev_involutive.
    rewrite string_of_list_ascii_of_string, (use_side_dump d n Hd). reflexivity.
Qed.

Lemma import_dump i : import_ok i = true -> read_import (dimport i) = Good i.
Proof.
  destruct i as [p alias glob uses]. cbn [import_ok i_alias i_uses].
  change (opt_name_ok alias && forallb use_ok uses = true ->
    read_import (dimport (AImport p alias glob uses)) = Good (AImport p alias glob uses)).
  rewrite andb_true_iff. intros [Ha Hu].
  change (rbind (opt_atom (dopt SA alias)) (fun alias' =>
    rbind (rmap read_use (map duse uses))
      (fun uses' => Good (AImport p alias' (match SA (if glob then "*" else "_") with
        | SA "*" => true | _ => false end) uses'))) = Good (AImport p alias glob uses)).
  rewrite (opt_atom_dump alias Ha). cbn [rbind].
  assert (Hr : rmap read_use (map duse uses) = Good uses).
  { induction uses as [|u uses IH]; [reflexivity|].
    apply andb_true_iff in Hu as [Hu Huses]. cbn [map rmap].
    rewrite (use_dump u Hu). cbn [rbind]. rewrite (IH Huses). reflexivity. }
  rewrite Hr. destruct glob; reflexivity.
Qed.

Theorem dump_reads f f' : wf_names f = true -> read_file (dump_file f) = Good f' -> f' = f.
Proof.
  destruct f as [is ds]. cbn [wf_names f_imports f_decls].
  change (forallb import_ok is && forallb decl_ok ds = true ->
    read_file (dump_file (AFile is ds)) = Good f' -> f' = AFile is ds).
  rewrite andb_true_iff. intros [Hi Hd] H.
  change (rbind (rmap read_import (map dimport is)) (fun is' =>
    rbind (rmap (read_decl dump_fuel) (map ddecl ds))
      (fun ds' => Good (AFile is' ds'))) = Good f') in H.
  apply dump_bind_inv in H as (is' & Hi' & H).
  apply dump_bind_inv in H as (ds' & Hd' & E).
  assert (Ei : is' = is).
  { eapply rmap_inverse; [|exact Hi|exact Hi'].
    intros i i' Hok Hread. rewrite (import_dump i Hok) in Hread.
    injection Hread as <-. reflexivity. }
  rewrite Ei, (rmap_inverse _ _ _ (decl_inverse dump_fuel) ds ds' Hd Hd') in E.
  injection E as <-. reflexivity.
Qed.
