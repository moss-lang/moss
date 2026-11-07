(* Actual eta-provider lookup supplies the physical symbol used by the cache. *)
From Stdlib Require Import List String Bool.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore ElabFrameAtoms.
Import ListNotations.
Definition PhysicalProvider(s:st)(g:gsym):Prop:=nget(st_syms s)(gs_id g)=Some g.
Lemma sym_physical_provider:forall key s g sf,
 sym key s=Good(g,sf)->Inv s->sf=s/\PhysicalProvider s g.
Proof.
 intros key s g sf Run Hi;unfold sym in Run;destruct(nget(st_syms s)key)as[gg|]eqn:Lookup;[|discriminate Run].
 injection Run as <- <-;split;[reflexivity|];unfold PhysicalProvider;rewrite(iv_ids _ Hi _ _ Lookup);exact Lookup.
Qed.
Lemma resolve_sym_physical_provider:forall mi pa s found sf,
 resolve_sym mi pa s=Good(found,sf)->Inv s->sf=s/\forall g,found=Some g->PhysicalProvider s g.
Proof.
 intros mi pa s found sf Run Hi;unfold resolve_sym in Run;apply bind_inv in Run as(ent&sr&Path&Run).
 unfold rpath in Path;destruct(resolve_path(st_w s)mi pa)as[resolved|msg];[|discriminate Path];injection Path as EqRes EqState;subst resolved sr.
 destruct ent as[[key|mid]|];try(injection Run as <- <-;split;[reflexivity|intros g Eq;discriminate Eq]).
 apply bind_inv in Run as(g&sg&Sym&Ret);destruct(sym_physical_provider _ _ _ _ Sym Hi)as[-> Ph].
 injection Ret as <- <-;split;[reflexivity|intros gg Eq;injection Eq as <-;exact Ph].
Qed.
Ltac provider_no_good H:=exfalso;solve[repeat(first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(?&?&?&H)])].
Lemma expect_defined_fn_physical:forall mi y s g sf,
 expect_defined_fn mi y s=Good(g,sf)->Inv s->sf=s/\PhysicalProvider s g.
Proof.
 intros mi y s g sf Run Hi;unfold expect_defined_fn in Run;destruct y;try(provider_no_good Run).
 match goal with a:sapp|-_=>destruct a end;try(provider_no_good Run).
 apply bind_inv in Run as(found&sr&Resolve&Run);destruct(resolve_sym_physical_provider _ _ _ _ _ Resolve Hi)as[-> Phys].
 destruct found as[pv|];[|provider_no_good Run];destruct(is_defined_fn pv);[|provider_no_good Run].
 injection Run as <- <-;split;[reflexivity|apply Phys;reflexivity].
Qed.
Lemma expect_provider_physical:forall c e y this s g sf,
 expect_provider c e y this s=Good(g,sf)->Inv s->sf=s/\PhysicalProvider s g.
Proof.
 intros c e y this s g sf Run Hi;unfold expect_provider in Run;destruct y;
 try solve[eapply expect_defined_fn_physical;eassumption].
 match goal with o:aexpr|-_=>destruct o end;
 try solve[eapply expect_defined_fn_physical;eassumption].
 apply bind_inv in Run as(found&sr&Resolve&Run);destruct(resolve_sym_physical_provider _ _ _ _ _ Resolve Hi)as[-> Phys].
 destruct found as[rg|];[|eapply expect_defined_fn_physical;eassumption].
 destruct(kind_eqb(gs_kind rg)KUnit||kind_eqb(gs_kind rg)KTag);[|eapply expect_defined_fn_physical;eassumption].
 apply bind_inv in Run as(hm&sh&Home&Run);injection Home as <- <-.
 apply bind_inv in Run as(here&st&Here&Run);injection Here as <- <-.
 apply bind_inv in Run as(att&sa&Att&Run).
 destruct(match assoc attached_eqb(gs_id rg,n)(gm_attached(get_mod(st_w s)(gs_mod rg)))with
 Some key=>Some key|None=>assoc attached_eqb(gs_id rg,n)(gm_attached(get_mod(st_w s)(cx_mod c)))end)as[key|]eqn:Candidate.
 - apply bind_inv in Att as(pv&sp&Lookup&Ret);destruct(sym_physical_provider _ _ _ _ Lookup Hi)as[-> Physical].
   injection Ret as <- <-.
   destruct(is_some(fn_body pv));[|provider_no_good Run];destruct(head this);[|provider_no_good Run].
   destruct(Nat.eqb n0(gs_id rg));[|provider_no_good Run].
   injection Run as <- <-;split;[reflexivity|exact Physical].
 - injection Att as <- <-;provider_no_good Run.
Qed.
Lemma match_fn_sig_keeps_physical:forall fuel c e pv wanted kind s sm sf,
 PhysicalProvider s pv->match_fn_sig fuel c e pv wanted kind s=Good(sm,sf)->Inv s->PhysicalProvider sf pv.
Proof.
 intros fuel c e pv wanted kind s sm sf Physical Run Hi.
 destruct(ok_match_fn_sig_operation fuel c e pv wanted kind _ _ _ Hi Run)as[If Frame].
 exact(fr_keep _ _ Frame _ _ Physical).
Qed.
