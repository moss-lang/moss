(* A checked copy preserves the last effective output at every specialized
   key. Formal templates stay generic; checking substitutions are separate. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Sx Elab ElabSound ElabFrame BodyTr ReifyData.
Import ListNotations.
Local Open Scope list_scope.

Lemma copy_key_eq : forall a b, key_eqb a b=true -> a=b.
Proof.
 intros [x|x|t m|x] [y|y|u n|y] E; cbn in E; try discriminate;
 try(apply Nat.eqb_eq in E; subst; reflexivity).
 apply andb_true_iff in E as [E1 E2]. apply Nat.eqb_eq in E1.
 apply ety_eqb_true in E2; subst; reflexivity.
Qed.
Lemma copy_key_refl : forall a,key_eqb a a=true.
Proof. intros []; cbn;rewrite ?Nat.eqb_refl,?ElabSound.ety_eqb_refl; reflexivity. Qed.
Lemma copy_key_mem : forall a xs,key_mem a xs=true <->In a xs.
Proof.
 intros a xs;induction xs as [|x xs IH]; cbn;[tauto||(split;[discriminate|contradiction])|].
 rewrite orb_true_iff,IH;split.
 - intros [E|E]; [apply copy_key_eq in E; subst; auto|auto].
 - intros [<-|E]; [left;apply copy_key_refl|auto].
Qed.

Definition FinalCopyOutputs (generic fv gv : env) (fkeys gkeys:list key) : Prop :=
 forall mapped k,
 find(fun a=>key_eqb mapped(copy_key generic fv a))(rev fkeys)=Some k ->
 exists h,find(fun a=>key_eqb mapped(copy_key generic gv a))(rev gkeys)=Some h /\
          copy_slot_eq generic fv gv k h=true.

Lemma copy_output_check_sound : forall generic fv gv gkeys todo seen mapped k,
 copy_output_check generic fv gv gkeys todo seen=true ->
 ~In mapped seen ->
 find(fun a=>key_eqb mapped(copy_key generic fv a))todo=Some k ->
 exists h,find(fun a=>key_eqb mapped(copy_key generic gv a))(rev gkeys)=Some h /\
          copy_slot_eq generic fv gv k h=true.
Proof.
 intros generic fv gv gkeys todo;induction todo as [|a rest IH]; intros seen mapped k H N F;
 cbn[copy_output_check]in H;cbn[find]in F;[discriminate|].
 destruct(key_mem(copy_key generic fv a)seen)eqn:M.
 - destruct(key_eqb mapped(copy_key generic fv a))eqn:E.
   + apply copy_key_eq in E; subst mapped;apply copy_key_mem in M;contradiction.
   + exact(IH seen mapped k H N F).
 - destruct(find(fun h=>key_eqb(copy_key generic fv a)(copy_key generic gv h))(rev gkeys))as[h|]eqn:G;[|discriminate].
   apply andb_true_iff in H as [S H].
   destruct(key_eqb mapped(copy_key generic fv a))eqn:E.
   + apply copy_key_eq in E; subst mapped;injection F as <-. exists h; auto.
   + apply(IH (copy_key generic fv a::seen) mapped k H);[|exact F].
     intros[Q|Q]; [subst mapped;rewrite copy_key_refl in E;discriminate|auto].
Qed.
Theorem copy_outputs_match_sound : forall generic fv gv fkeys gkeys,
 copy_outputs_match generic fv gv fkeys gkeys=true ->FinalCopyOutputs generic fv gv fkeys gkeys.
Proof.
 intros generic fv gv fkeys gkeys H mapped k F.
 eapply copy_output_check_sound;[exact H|simpl;tauto|exact F].
Qed.

(* This execution theorem exposes the actual formal output views, rather
   than inventing a compatibility premise at a later call site. *)
Theorem copy_output_compatible_views : forall mi site ef eg fg bg fixed fneeds gneeds s profile s',
 copy_output_compatible mi site ef eg fg bg fixed fneeds gneeds s=Good(profile,s') ->
 exists generic fv gv fkeys gkeys roles,
 profile=ReifyExports ety key (map fst roles) fkeys (reify_scope_of fv)
       (reify_scope_of gv)(reify_scope_of ef)roles /\
 FinalCopyOutputs generic fv gv fkeys gkeys.
Proof.
 intros mi site ef eg fg bg fixed fneeds gneeds s profile s' H.
 unfold copy_output_compatible in H.
 destruct(fold_left _ fixed (Some []))as[sol|]eqn:Sol;[|discriminate].
 apply bind_inv in H as(replacements&s1&Rep&H).
 apply bind_inv in H as(u&s2&Input&H).
 destruct(_ && _)eqn:I in Input;[injection Input as <- <-|discriminate].
 apply bind_inv in H as([[fv fkeys]roles]&s2&Views&H).
 apply bind_inv in H as([gv gkeys]&s3&GViews&H).
 apply bind_inv in H as(u&s4&Check&H).
 destruct(copy_outputs_match _ _ _ _ _)eqn:C in Check;[injection Check as <- <-|discriminate].
 injection H as <- <-.
 lazymatch type of C with
 | copy_outputs_match ?generic fv gv fkeys gkeys=true =>
   exists generic,fv,gv,fkeys,gkeys,roles
 end.
 split;[reflexivity|]. apply copy_outputs_match_sound;exact C.
Qed.

Definition CopySignatureReading(generic view:env)(k:key):option(list ety*ety) :=
 option_map(fun p=>(map(copy_interpret generic view)(fst p),copy_interpret generic view(snd p)))
           (key_sig view k).
Lemma copy_type_list_eq : forall generic a b xs ys,
 length xs=length ys ->
 forallb(fun p=>ety_eqb(copy_interpret generic a(fst p))(copy_interpret generic b(snd p)))
         (combine xs ys)=true ->
 map(copy_interpret generic a)xs=map(copy_interpret generic b)ys.
Proof.
 intros generic a b xs;induction xs as[|x xs IH];intros[|y ys] L H;cbn in *;try discriminate;[reflexivity|].
 apply andb_true_iff in H as[E H]. apply ety_eqb_true in E.
 f_equal;[exact E|]. apply IH;[lia|exact H].
Qed.
Lemma copy_slot_signature : forall generic a b ka kb,
 copy_slot_eq generic a b ka kb=true ->
 CopySignatureReading generic a ka=CopySignatureReading generic b kb.
Proof.
 intros generic a b ka kb H;unfold copy_slot_eq in H;apply andb_true_iff in H as[_ H].
 unfold CopySignatureReading.
 destruct(key_sig a ka)as[[ps t]|]eqn:A;
 destruct(key_sig b kb)as[[qs u]|]eqn:B;try discriminate;[|reflexivity].
 apply andb_true_iff in H as[L H]. apply Nat.eqb_eq in L.
 pose proof(copy_type_list_eq generic a b(t::ps)(u::qs))as E.
 specialize(E ltac:(cbn;congruence) H);cbn in E;injection E as E1 E2.
 cbn;rewrite E1,E2;reflexivity.
Qed.
Definition CopyProvisionReadings(generic a b:env)(ka kb:key):Prop :=
 match get_app a ka,get_app b kb with
 |Some(xs,_),Some(ys,_)=>length xs=length ys /\
     forall x v,In(x,v)xs ->exists u,assoc Nat.eqb x ys=Some u /\
       option_map(copy_interpret generic a)v=option_map(copy_interpret generic b)u
 |None,None=>True |_,_=>False end.
Lemma copy_slot_provisions : forall generic a b ka kb,
 copy_slot_eq generic a b ka kb=true ->CopyProvisionReadings generic a b ka kb.
Proof.
 intros generic a b ka kb H;unfold copy_slot_eq in H;apply andb_true_iff in H as[H _].
 unfold copy_app_eq in H;unfold CopyProvisionReadings.
 destruct(get_app a ka)as[[xs stamp]|]eqn:A;
 destruct(get_app b kb)as[[ys stamp']|]eqn:B;try discriminate;[|exact I].
 apply andb_true_iff in H as[L H];apply Nat.eqb_eq in L;split;[exact L|].
 intros x v Inx;apply forallb_forall with(x:=(x,v))in H;[|exact Inx];cbn in H.
 destruct(assoc Nat.eqb x ys)as[[t|]|]eqn:E;destruct v as[u|];try discriminate.
 - exists(Some t);split;[reflexivity|]. apply ety_eqb_true in H;cbn;congruence.
 - exists None;auto.
Qed.
Theorem checked_copy_output_readings : forall mi site ef eg fg bg fixed fneeds gneeds s profile s',
 copy_output_compatible mi site ef eg fg bg fixed fneeds gneeds s=Good(profile,s') ->
 exists generic fv gv fkeys gkeys roles,
 profile=ReifyExports ety key(map fst roles)fkeys(reify_scope_of fv)(reify_scope_of gv)(reify_scope_of ef)roles /\
 forall mapped k,
 find(fun a=>key_eqb mapped(copy_key generic fv a))(rev fkeys)=Some k ->
 exists h,find(fun a=>key_eqb mapped(copy_key generic gv a))(rev gkeys)=Some h /\
 CopySignatureReading generic fv k=CopySignatureReading generic gv h /\
 CopyProvisionReadings generic fv gv k h.
Proof.
 intros mi site ef eg fg bg fixed fneeds gneeds s profile s' H.
 destruct(copy_output_compatible_views _ _ _ _ _ _ _ _ _ _ _ _ H)
  as(generic&fv&gv&fkeys&gkeys&roles&E&Read).
 exists generic,fv,gv,fkeys,gkeys,roles;split;[exact E|].
 intros mapped k F;destruct(Read mapped k F)as(h&Found&Slot).
 exists h;split;[exact Found|]. split;[apply copy_slot_signature|apply copy_slot_provisions];exact Slot.
Qed.
