(* Named eta-cache readonly facts remain usable with opaque type readers. *)
From Stdlib Require Import List.
From NM Require Import Sx Elab ElabInv.
From NM Require Export WrapperReadonly.
Lemma kwr_ety_ty:forall t,keeps st_wrappers(ety_ty t).
Proof. intro t;unfold ety_ty;apply kwr_ety_ty_go. Qed.
#[export]Hint Resolve kwr_ety_ty:keepsdb.
Lemma kwr_key_theta:forall e k,keeps st_wrappers(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
#[export]Hint Resolve kwr_key_theta:keepsdb.
Lemma kwr_key_item:forall k,keeps st_wrappers(key_item k).
Proof. intros;unfold key_item;keepst. Qed.
#[export]Hint Resolve kwr_key_item:keepsdb.
Lemma kwr_key_item_at:forall e k,keeps st_wrappers(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
#[export]Hint Resolve kwr_key_item_at:keepsdb.
Lemma kwr_smap_at:forall ei eo pairs,keeps st_wrappers(smap_at ei eo pairs).
Proof. intros;unfold smap_at;keepst. Qed.
#[export]Hint Resolve kwr_smap_at:keepsdb.
