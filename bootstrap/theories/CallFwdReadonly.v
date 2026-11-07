From NM Require Import Elab ElabSound ElabFrame ElabInv FwdReadonly.
Lemma kw_require_needs : forall mi e g info,keeps st_fwd(require_needs mi e g info).
Proof. intros;unfold require_needs,require_merges;keepst. Qed.
Lemma kw_smap_at : forall ei eo ps,keeps st_fwd(smap_at ei eo ps).
Proof. intros;unfold smap_at;keepst. Qed.
#[export]Hint Resolve kw_require_needs kw_smap_at:keepsdb.
