(* The two functor-body insertion sites use a newly allocated symbol. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv FinalLinks FunctorReadonly FunctorRows.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Ltac ft_kept:=apply ft_keeps;[solve[auto with keepsdb]|solve[okt]].
Ltac ft_leaf:=apply ft_keeps;[solve[keepst]|solve[okt]].
Ltac ftt:=repeat(fintros;first[solve[auto]|lazymatch goal with
 | |-FT_at ?key(mbind(upd(s_funs _))_)=>apply fta_emit_bind;intro
 | |-FT(mbind(new_sym _ _ _ _ _)_)=>apply ft_new_sym_bind;[discriminate|split;discriminate|intro]
 | |-FT(mbind _ _)=>apply ft_bind
 | |-FT(mmap _ _)=>apply ft_mmap
 | |-FT(miter _ _)=>apply ft_miter
 | |-FT(mfold _ _ _)=>apply ft_mfold
 | |-FT(with_vars _ _)=>apply ft_with_vars
 | |-FT(in_reify_code _ _)=>unfold in_reify_code
 | |-FT_at _(mbind _ _)=>apply fta_bind_quiet;[|solve[keepst]|intro]
 | |-FT_at _ ?m=>first[solve[apply fta_of_ft;ftt]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-FT ?m=>first[solve[ft_kept]|lazymatch m with
   |apply_body _ _ _ _=>fail 1
   |upd _=>ft_leaf
   |(fun s=>_)=>ft_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).
Lemma ft_platform_fun:forall g,FT(platform_fun g).
Proof. intro g;unfold platform_fun;ftt. Qed.
Lemma ft_platform_entry:forall k prims,FT(platform_entry k prims).
Proof. intros;unfold platform_entry;ftt. all:apply ft_platform_fun. Qed.
Lemma ft_link_body_step:forall fuel,
 (forall c e g,FT(apply_body fuel c e g))->forall fg bg,FT(link_body(S fuel)fg bg).
Proof. intros fuel Body fg bg;cbn[link_body];ftt. Qed.
