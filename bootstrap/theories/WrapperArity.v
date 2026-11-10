(* Successful signature checking supplies the eta adapter's real arity. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import NMap Sx Elab ElabSound RuleBKeep.
Lemma mmap_output_length:forall A B(f:A->M B)xs s ys sf,
 mmap f xs s=Good(ys,sf)->List.length ys=List.length xs.
Proof.
 intros A B f xs;induction xs as[|x xs IH];intros s ys sf Run.
 - injection Run as <- _;reflexivity.
 - cbn[mmap]in Run;apply bind_inv in Run as(y&sy&Head&Run).
   apply bind_inv in Run as(rest&sr&Tail&Ret);injection Ret as <- _.
   cbn[List.length];f_equal;eapply IH;exact Tail.
Qed.
Lemma fn_sig_arity:forall g tm e this s sg sf,
 fn_sig g tm e this s=Good(sg,sf)->List.length(sg_params sg)=List.length(fn_params g).
Proof.
 intros g tm e this s sg sf Run;unfold fn_sig in Run.
 apply bind_inv in Run as(ps&sp&Params&Run).
 apply bind_inv in Run as(rt&sr&Ret&Run);injection Run as <- _.
 cbn[sg_params];eapply mmap_output_length;exact Params.
Qed.
Lemma match_fn_sig_arity:forall fuel c e pv wanted kind s sm sf,
 match_fn_sig fuel c e pv wanted kind s=Good(sm,sf)->
 List.length(fn_params pv)=List.length(sg_params wanted).
Proof.
 intros[|fuel]c e pv wanted kind s sm sf Run;[discriminate|];cbn[match_fn_sig]in Run.
 apply bind_inv in Run as(info&s1&Info&Run).
 apply bind_inv in Run as(rbu&rbs&RBk&Run). pose proof(requirements_in_force_keep _ _ _ _ _ _ _ _ RBk)as ->.
 apply bind_inv in Run as(pairs&s2&Needs&Run).
 apply bind_inv in Run as(got&s3&Signature&Run).
 destruct(sig_eqb_in e got e wanted)eqn:Equal.
 - unfold sig_eqb_in in Equal;apply andb_true_iff in Equal as[Equal _].
   apply andb_true_iff in Equal as[Equal _];apply Nat.eqb_eq in Equal.
   rewrite(fn_sig_arity _ _ _ _ _ _ _ Signature)in Equal;exact Equal.
 - exfalso;repeat(apply bind_inv in Run as(?&?&?&Run));discriminate Run.
Qed.
