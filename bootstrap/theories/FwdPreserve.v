(* The sole writer of st_fwd installs the symbol it just allocated. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv FwdReadonly ForwarderTrace FwdInvariant.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma okF_forwarder_step:forall fuel,(forall g,okF(lower_body fuel g))->forall tg,okF(forwarder(S fuel)tg).
Proof.
 intros fuel Lower tg;split;[apply ok_forwarder|]. intros s g final I FI E.
 cbn[forwarder]in E;apply bind_inv in E as[initial[s0[Read E]]];injection Read as <- <-.
 destruct(nget(st_fwd s)(gs_id tg))as[id|]eqn:Cached.
 - assert(Os:ok(sym id))by okt. destruct(Os _ _ _ I E)as[_ F]. eapply fwd_inv_frame;[exact FI|exact F|].
   unfold sym in E;destruct(nget(st_syms s)id);[injection E as <- <-;reflexivity|discriminate].
 - apply bind_inv in E as[id[allocated[New E]]].
   assert(Onew:ok(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by okt.
   destruct(Onew _ _ _ I New)as[IA FA].
   assert(FIA:FwdInv allocated).
   {eapply fwd_inv_frame;[exact FI|exact FA|].
    assert(K:keeps st_fwd(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;keepst).
    exact(K _ _ _ New). }
   destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Lookup]].
   apply bind_inv in E as[u[cached[Cache E]]];destruct u.
   assert(Ocache:ok(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by okt.
   destruct(Ocache _ _ _ IA Cache)as[IC FC].
   assert(FIC:FwdInv cached).
   {unfold upd in Cache;injection Cache as <-.
    eapply fwd_inv_install;[exact FIA|exact Lookup|reflexivity]. }
   apply bind_inv in E as[got[ready[Sym E]]].
   assert(Os:ok(sym id))by okt. destruct(Os _ _ _ IC Sym)as[IR FR].
   assert(FIR:FwdInv ready).
   {eapply fwd_inv_frame;[exact FIC|exact FR|]. unfold sym in Sym;destruct(nget(st_syms cached)id);[injection Sym as <- <-;reflexivity|discriminate]. }
   apply bind_inv in E as[u[last[Run E]]];destruct u;injection E as <- <-.
   exact(proj2(Lower got)_ _ _ IR FIR Run).
Qed.
Create HintDb fwdb.
Lemma okF_fresh_marker:forall e,okF(fresh_marker e).
Proof. intro e;apply okF_readonly;[apply ok_fresh_marker|unfold fresh_marker,keeps;intros;injection H as _ <-;reflexivity]. Qed.
#[export]Hint Resolve okF_ret okF_fresh_marker:fwdb.
Lemma fw_keeps_first : forall A (m : M A), keeps st_fwd m -> ok m -> okF m.
Proof. intros A m Keeps0 Valid; apply okF_readonly; assumption. Qed.

Ltac fw_kept:=apply fw_keeps_first;[solve[auto with keepsdb]|solve[okt]].
Ltac fw_leaf:=apply fw_keeps_first;[solve[keepst]|solve[okt]].
Ltac fwt:=repeat(fintros;first[solve[auto with fwdb]|lazymatch goal with
 | |-okF(mbind _ _)=>apply okF_bind
 | |-okF(mmap _ _)=>apply okF_mmap
 | |-okF(miter _ _)=>apply okF_miter
 | |-okF(mfold _ _ _)=>apply okF_mfold
 | |-okF(with_vars _ _)=>apply okF_with_vars
 | |-okF(in_reify_code _ _)=>unfold in_reify_code
 | |-okF ?m=>first[solve[fw_kept]|lazymatch m with
   |forwarder _ _=>fail 1
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>fw_leaf
   |(fun s=>_)=>fw_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|let h:=head_of m in progress unfold h]
   end]
 end]).
Lemma okF_wrapper:forall fuel pv,okF(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];fwt. Qed.
Lemma okF_lower_body_step:forall fuel,(forall c e b exp,okF(synth_block fuel c e b exp))->forall g,okF(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];fwt. Qed.
Lemma okF_lambda_provider_step:forall fuel,(forall c e b exp,okF(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,okF(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];fwt. Qed.
