(* Pure grounding contracts for complete immutable provision maps. *)
From Stdlib Require Import List.
From NM Require Import Audit CoreMoss ReifyMaps ReifyChains.
Import ListNotations.
Set Implicit Arguments.

Definition meanings_on (A:list item)(ga gb:subst):Prop:=
 forall t,In t(tyreqs A)->app_subst ga(TAbs t)=app_subst gb(TAbs t).
Definition runtime_dep_tys(Sg:gsig)(P:xprog)(i:item):list tsym:=
 match i with IFun u=>if xp_immutable P then
 flat_map(fun t=>ty_frees(app_subst(xp_fixed_inst P u)(TAbs t)))(runtime_fixed Sg P u)
 else dep_tys Sg i|_=>dep_tys Sg i end.
Definition reified_scope_closed(Sg:gsig)(P:xprog)(A:list item):Prop:=
 Forall(fun i=>incl(runtime_dep_tys Sg P i++item_ty_frees i++method_item_frees Sg i)(tyreqs A))A.

Definition reify_scope_closed(Sg:gsig)(A:list item):Prop:=
 Forall(fun i=>incl(dep_tys Sg i++item_ty_frees i++method_item_frees Sg i)(tyreqs A))A.

Definition complete_map_contract(Sg:gsig)(P:xprog)(source target:ctx)(sm:smap):Prop:=
 forall gs,ctx_grounded gs source ->exists gt,
 ctx_grounded gt target /\full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))
 gt gs(cA target)(cA source)sm.

(* Closure code is entered under its complete formal context.  Its argument
   and return meanings must be the callable slot's meanings in the caller. *)
Definition callable_map_contract(Sg:gsig)(P:xprog)(source target:ctx)
 (sm:smap)(slot code:fnsig):Prop:=
 forall gs,ctx_grounded gs source ->exists gt,
 ctx_grounded gt target /\full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))
 gt gs(cA target)(cA source)sm /\
 grounded_call_signature gt code=grounded_call_signature gs slot.

Definition application_input_contract(Sg:gsig)(P:xprog)(Phi:ctx)
 (u:usym)(Du:tele)(inputs:smap):Prop:=
 forall base,ctx_grounded base Phi ->exists gin,
 schema_input_ground (runtime_signature Sg P)(xp_fixed_inst P)u base Du gin /\
 full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))gin base Du(cA Phi)inputs.

(* Fresh result slots extend the caller's meanings, rather than overwriting
   any generation which an earlier expression could still reference. *)
Definition result_map_contract(Sg:gsig)(P:xprog)(Phi:ctx)(u:usym)
 (Du Dr caller_results:tele)(outputs:smap):Prop:=
 forall base gin gout,
 ctx_grounded base Phi ->
 schema_input_ground (runtime_signature Sg P)(xp_fixed_inst P)u base Du gin ->
 schema_result_ground Du Dr gin gout ->exists gcaller,
 ctx_grounded gcaller(open_res caller_results Phi) /\
 meanings_on(cA Phi)gcaller base /\
 full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))gcaller gout caller_results Dr outputs.

(* The body's input and capture interpretations must jointly satisfy its
   checked pin equations.  This contract concerns only types and contexts. *)
Definition body_ground_contract(Sg:gsig)(P:xprog)(h:usym)
 (PhiD PhiC:ctx)(pins:subst)(captured:subst):Prop:=
 forall Dh Dr gin,g_fun Sg h=Some(Dh,Dr,None) ->
 schema_input_ground (runtime_signature Sg P)(xp_fixed_inst P)h captured Dh gin ->exists gb,
 ctx_grounded gb(fun_ctx PhiD PhiC pins) /\
 meanings_on(cA PhiD)gb gin /\meanings_on(cA PhiC)gb captured.

(* A defined provider captures a complete formal scope.  Linking and pins
   are checked in that captured grounding, before any body is executed. *)
Definition functor_capture_contract(Sg:gsig)(P:xprog)(source PhiD PhiC:ctx)
 (u h:usym)(Dr:tele)(pins:subst)(inputs captures:smap):Prop:=
 forall outer,ctx_grounded outer source ->exists captured,
 ctx_grounded captured PhiC /\
 full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))captured outer(cA PhiC)(cA source)captures /\
 body_ground_contract Sg P h PhiD PhiC pins captured /\
 functor_link (runtime_signature Sg P)(xp_fixed_inst P)outer u captured h inputs(identity_smap Dr).

Lemma meanings_on_refl:forall A g,meanings_on A g g.
Proof. intros A g t Hin;reflexivity. Qed.
Lemma meanings_on_sym:forall A a b,meanings_on A a b->meanings_on A b a.
Proof. intros A a b H t Hin;symmetry;apply H;exact Hin. Qed.
Lemma meanings_on_trans:forall A a b c,meanings_on A a b->meanings_on A b c->meanings_on A a c.
Proof. intros A a b c H1 H2 t Hin;rewrite(H1 t Hin);exact(H2 t Hin). Qed.

Lemma fixed_meanings_on:forall Sg P A ga gb u,
 meanings_on A ga gb ->
 incl(flat_map(fun t=>ty_frees(app_subst(xp_fixed_inst P u)(TAbs t)))(runtime_fixed Sg P u))(tyreqs A)->
 forall t,In t(runtime_fixed Sg P u)->fixed_interpretation(xp_fixed_inst P)u ga t=fixed_interpretation(xp_fixed_inst P)u gb t.
Proof.
 intros Sg P A ga gb u Eq Closed t Hin;unfold fixed_interpretation.
 apply dentry_agree_retype_aux1;intros x Hx;apply Eq,Closed,in_flat_map.
 exists t;auto.
Qed.

Lemma schema_input_base_ext:forall Sg Finst u base old D gin,
 (forall t,In t(g_fixed Sg u)->fixed_interpretation Finst u base t=fixed_interpretation Finst u old t)->
 schema_input_ground Sg Finst u old D gin ->schema_input_ground Sg Finst u base D gin.
Proof.
 intros Sg Finst u base old D gin Eq[Phi[Tele[Ground[Cover[Abs [Fixed Equations]]]]]].
 exists Phi;split;[exact Tele|]. split;[exact Ground|]. split;[exact Cover|]. split;[exact Abs|].
 split;[intros t Hin;rewrite(Eq t Hin);exact(Fixed t Hin)|exact Equations].
Qed.

(* A body may extend its formal input with captures without changing any
   formal input meaning. Result preservation therefore transports back. *)
Lemma schema_result_input_ext : forall Din Dout old actual gout,
 meanings_on Din actual old ->schema_result_ground Din Dout actual gout ->
 schema_result_ground Din Dout old gout.
Proof.
 intros Din Dout old actual gout Eq [Phi[Tele[Ground[Cover[Abs Preserve]]]]].
 exists Phi;split;[exact Tele|]. split;[exact Ground|].
 split;[exact Cover|]. split;[exact Abs|].
 intros t Hin Hout;rewrite(Preserve t Hin Hout);apply Eq;exact Hin.
Qed.

(* The provider's checked final context exports its canonical result schema.
   This is a static type-map property; it makes no assertion about execution. *)
Definition body_export_contract (Sg:gsig)(P:xprog)(PhiStart PhiEnd:ctx)
 (Din Dout:tele)(exports:smap):Prop:=
 forall gin bodyout,ctx_grounded gin PhiStart ->
 ctx_grounded bodyout PhiEnd ->meanings_on(cA PhiStart)bodyout gin ->
 exists result,schema_result_ground Din Dout gin result /\
 full_map_ok Sg(identity_functor_schema (runtime_signature Sg P)(xp_fixed_inst P))
 result bodyout Dout(cA PhiEnd)exports.

Lemma meanings_on_type : forall A ga gb tau,
 meanings_on A ga gb ->incl(ty_frees tau)(tyreqs A) ->
 app_subst ga tau=app_subst gb tau.
Proof.
 intros A ga gb tau Mean Closed;apply dentry_agree_retype_aux1.
 intros t Hin;apply Mean,Closed;exact Hin.
Qed.
