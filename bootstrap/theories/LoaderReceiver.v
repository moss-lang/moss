(* Receiver metadata agrees with the actual source declaration and table. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve LoaderOrigin MethodLookup.
Module A:=ExecutionModel.Elaboration.
Theorem loaded_attached_receiver:forall b w R Fl key g rn name parameters result body,
 load_world b=Good w->nget(w_syms w)key=Some g->
 gs_decl g=ADFn(RAttached rn)name parameters result body->
 exists r gr,gs_recv g=Some r/\nget(w_syms w)r=Some gr/\
 gs_mod gr=gs_mod g/\A.Attached(W_read w R Fl)r name key.
Proof.
 intros b w R Fl key g rn name parameters result body Load At Decl.
 pose proof(load_world_receiver_shape b w Load key g At)as Receiver.
 unfold ReceiverAt in Receiver;rewrite Decl in Receiver.
 destruct(load_world_inv b w Load)as[ls[entry[Inv Eq]]];subst w.
 destruct(li_symfacts _ _ Inv key g At)
 as[m[path[am[Mod[Home[Module[Occurs[Kind[Named[Detached Attached]]]]]]]]]].
 destruct(Attached rn name parameters result body Decl)as[r[gr[Name[Gr[Owner Table]]]]].
 unfold get_mod in Receiver;cbn[w_mods]in Receiver;rewrite Mod in Receiver.
 exists r,gr;split;[rewrite Receiver;exact Name|];split;[exact Gr|];split;[exact Owner|].
 unfold A.Attached;cbn[A.w_attached W_read w_syms];rewrite Gr,Owner.
 unfold get_mod;cbn[w_mods];rewrite Mod;exact Table.
Qed.
Theorem loaded_nonattached_receiver:forall b w key g receiver name parameters result body,
 load_world b=Good w->nget(w_syms w)key=Some g->
 gs_decl g=ADFn receiver name parameters result body->
 (forall rn,receiver<>RAttached rn)->gs_recv g=None.
Proof.
 intros b w key g receiver name parameters result body Load At Decl Plain.
 pose proof(load_world_receiver_shape b w Load key g At)as Shape.
 unfold ReceiverAt in Shape;rewrite Decl in Shape;destruct receiver as[| |rn];try exact Shape.
 exfalso;exact(Plain rn eq_refl).
Qed.
