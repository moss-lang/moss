(* Executable fn_info views, independent of body semantics. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame.
Import ListNotations.
Section Walker.
Variable walk:env->M(env*list key).
Hypothesis Hok:forall e,ok(walk e).
Definition mw_go (mi : nat) :=
  fix go (n : nat) (tne ufe : list (nat * ety)) : M (env * list key) :=
     let! (e, needs) := walk (env_with_edges tne ufe) in
     let tne' := filter (fun p => negb (ety_eqb (snd p) (YAbs (fst p)))) (tymap e) in
     let ufe' := en_uf e in
     if edges_eqb (tne' ++ ufe') (tne ++ ufe) then ret (e, needs) else
     match n with
     | 0 => err mi "merging the context does not settle in 8 rounds (D43)"
     | S n' => go n' tne' ufe'
     end.

Lemma mw_go_eq : forall mi, merged_walk mi walk = mw_go mi 8 [] [].
Proof. reflexivity. Qed.

Lemma ok_mw_go : forall mi n tne ufe, ok (mw_go mi n tne ufe).
Proof.
  intros mi n. induction n as [|n IH]; intros tne ufe; cbn [mw_go];
    apply ok_bind; [apply Hok | | apply Hok |]; intros [e needs]; cbn beta iota zeta;
    destruct (edges_eqb _ _); try apply ok_ret; [unfold err; okt | apply IH].
Qed.

End Walker.

Definition fi_walk (g : gsym) (e : env) : M (env * list key) :=
  let mi := gs_mod g in
  match gs_origin g with
  | OForwarder t =>
      let! tg := sym t in
      let! rs := reqs t in
      let! cf := ctx_fuel in
      let! (e, needs) := mfold (fun acc r => let! rg := sym r in
                                             add_symbol_item cf (fst acc) mi rg [] (snd acc)) (e, []) rs in
      add_symbol_item cf e mi tg [] needs
  | _ =>
      let! acc := mfold (fun acc it => add_assume_item (fst acc) mi it (snd acc)) (e, []) (gs_assumes g) in
      let! main := is_main g in
      if main then add_root_context (fst acc) mi (snd acc) else ret acc
  end.

Lemma fn_info_miss : forall g0 s i s', fn_info g0 s = Good (i, s') -> nget (st_info s) (gs_id g0) = None ->
  exists g e needs s1, nget (st_syms s) (gs_id g0) = Some g /\
    merged_walk (gs_mod g) (fi_walk g) s = Good ((e, needs), s1) /\
    i = FInfo (freeze_all_apps e) needs
              (map (fun k => (k, match get_app (freeze_all_apps e) k with Some (a, _) => a | None => [] end)) needs)
              (merge_edges e) /\
    s' = St (st_w s1) (st_syms s1) (st_next s1) (st_reqs s1) (st_sigsyms s1) (nset (st_info s1) (gs_id g0) i)
            (st_fns s1) (st_funs s1) (st_fwd s1) (st_links s1) (st_flds s1) (st_consts s1)
            (st_wrappers s1) (st_vars s1) (st_nvar s1) (st_memory_bound s1) (st_memory_used s1)
            (st_checked_total s1) (st_lang s1) (st_unsup s1) (st_reify s1).
Proof.
  intros g0 s i s' H N. unfold fn_info in H. apply bind_inv in H as (x & s0 & H0 & H). injection H0 as <- <-.
  rewrite N in H. apply bind_inv in H as (g & s2 & H2 & H). unfold sym in H2.
  destruct (nget (st_syms s) (gs_id g0)) as [g'|] eqn:Eg; [|discriminate]. injection H2 as -> <-.
  apply bind_inv in H as ([e needs] & s1 & H1 & H). exists g, e, needs, s1. split; [reflexivity|]. split; [exact H1|].
  cbn beta iota zeta in H. apply bind_inv in H as (u & s3 & H3 & H). injection H as <- <-. unfold upd in H3.
  injection H3 as _ <-. split; reflexivity.
Qed.
