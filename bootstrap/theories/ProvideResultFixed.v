(* Fixed output functors capture precisely the meanings present at output
   installation, retaining the visitor's actual order of fixed inputs. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition output_fixed_values(tm:list(nat*ty))(xs:list nat):subst:=
 map(fun x=>(x,match A.nlook x tm with Some t=>t|None=>TUnit end))xs.
Lemma output_fixed_values_exact:forall W u tm xs,
 (forall x,In x xs<->A.Fixed W u x)->
 (forall x,A.Fixed W u x->A.nlook x tm<>None)->
 forall x tx,In(x,tx)(output_fixed_values tm xs)<->
 A.Fixed W u x /\ A.nlook x tm=Some tx.
Proof.
 intros W u tm xs Dom Av x tx;unfold output_fixed_values;split.
 - intro H;apply in_map_iff in H as(y&Eq&Inx);injection Eq as <- Eq.
   assert(F:A.Fixed W u y)by(apply Dom;exact Inx).
   destruct(A.nlook y tm)as[t|]eqn:L;[split;[exact F|now rewrite <-Eq]|].
   exfalso;exact(Av y F L).
 - intros[F L];apply in_map_iff;exists x;split;[now rewrite L|apply Dom;exact F].
Qed.
Lemma output_fixed_values_apps:forall tm xs,
 (forall x,In x xs->A.nlook x tm<>None)->
 map(fun x=>(x,A.nlook x tm))xs=
 map(fun q=>(fst q,Some(snd q)))(output_fixed_values tm xs).
Proof.
 intros tm xs Av;unfold output_fixed_values;rewrite map_map;apply map_ext_in.
 intros x H;cbn[fst snd];destruct(A.nlook x tm)eqn:E;[reflexivity|].
 exfalso;exact(Av x H E).
Qed.
Lemma output_fixed_values_keys:forall tm xs,
 map fst(output_fixed_values tm xs)=xs.
Proof. intros;unfold output_fixed_values;rewrite map_map;apply map_id. Qed.
