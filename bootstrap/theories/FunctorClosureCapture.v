(* The linked/captured split and the two maps of a defined functor closure. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTr BodyTy Merge CanonTypes CtxWalk CtxMerge TypeReadingInj CoreMoss
 FunctorContext FunctorCopyMap SmapExec NeedExec NeedMapExec InstantiateTr.
Import ListNotations.
Local Open Scope list_scope.
Module A := ExecutionModel.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

Definition closure_linked (inputs needs : list key) : list key :=
 filter (fun key => key_mem key inputs) needs.
Definition closure_captured (inputs needs : list key) : list key :=
 filter (fun key => negb (key_mem key inputs)) needs.
Definition closure_capture_guard (mi : nat) (e : env) (fg bg : gsym) (keys : list key) : M unit :=
 miter (fun key => if key_present e (site_key e key) then ret tt else
   let! shown := show_key (site_key e key) in
   err mi ("`" ++ gs_name bg ++ "`, bound to `" ++ gs_name fg ++ "`, captures " ++ shown ++
           ", which is not available here (D84 3b)")%string) keys.

Lemma closure_capture_present : forall mi e fg bg keys s u final,
 closure_capture_guard mi e fg bg keys s = Good (u,final) ->
 forall key, In key keys -> key_present e (site_key e key) = true.
Proof.
 intros mi e fg bg keys s u final Run key Member.
 destruct (miter_checked _ _ _ _ _ _ Run key Member) as [before [value [after Check]]].
 destruct (key_present e (site_key e key)) eqn:Present; [reflexivity|].
 apply bind_inv in Check as [shown [state [Show Bad]]].
 exfalso;exact (err_not_good _ _ _ _ _ _ Bad).
Qed.

Section Translation.
Variable Fl : string -> nat.
Hypothesis Finj : forall a b, Fl a = Fl b -> a = b.
Lemma closure_key_translation_injective : forall a b,
 key_ok a = true -> key_ok b = true -> key_tr Fl a = key_tr Fl b -> a = b.
Proof.
 intros [v|f|r m|u] [w|g|t n|h] Good Other Equal;
 cbn [key_ok key_tr] in *;try discriminate Equal;try congruence.
 injection Equal as Types Methods;subst n;f_equal.
 exact (tr_inj Fl Finj r t Good Other Types).
Qed.
Lemma closure_key_eqb_translation : forall a b,
 key_ok a = true -> key_ok b = true -> item_eqb (key_tr Fl a) (key_tr Fl b) = key_eqb a b.
Proof.
 intros a b Good Other; destruct (key_eqb a b) eqn:Equal.
 - apply key_eqb_true in Equal;subst b;apply CoreMoss.item_eqb_eq;reflexivity.
 - destruct (item_eqb (key_tr Fl a) (key_tr Fl b)) eqn:Translated;[|reflexivity].
   apply CoreMoss.item_eqb_eq in Translated.
   pose proof (closure_key_translation_injective a b Good Other Translated) as Same;subst b.
   rewrite key_eqb_refl in Equal;discriminate Equal.
Qed.
Lemma closure_membership_translation : forall key keys,
 key_ok key = true -> Forall (fun k => key_ok k = true) keys ->
 existsb (item_eqb (key_tr Fl key)) (map (key_tr Fl) keys) = key_mem key keys.
Proof.
 intros key keys Good Clean; induction Clean as [|other rest Other Rest IH]; [reflexivity|].
 cbn [existsb map key_mem].
 rewrite (closure_key_eqb_translation key other Good Other),IH;reflexivity.
Qed.
Lemma closure_linked_translation : forall inputs needs,
 Forall (fun key => key_ok key = true) inputs ->
 Forall (fun key => key_ok key = true) needs ->
 map (key_tr Fl) (closure_linked inputs needs) =
 filter (fun key => existsb (item_eqb key) (map (key_tr Fl) inputs)) (map (key_tr Fl) needs).
Proof.
 intros inputs needs Inputs Needs; unfold closure_linked; induction Needs as [|key rest Good Rest IH]; [reflexivity|].
 cbn [filter map].
 rewrite (closure_membership_translation key inputs Good Inputs).
 destruct (key_mem key inputs);cbn;rewrite IH;reflexivity.
Qed.
Lemma closure_captured_translation : forall inputs needs,
 Forall (fun key => key_ok key = true) inputs ->
 Forall (fun key => key_ok key = true) needs ->
 map (key_tr Fl) (closure_captured inputs needs) =
 filter (fun key => negb (existsb (item_eqb key) (map (key_tr Fl) inputs))) (map (key_tr Fl) needs).
Proof.
 intros inputs needs Inputs Needs; unfold closure_captured; induction Needs as [|key rest Good Rest IH]; [reflexivity|].
 cbn [filter map].
 rewrite (closure_membership_translation key inputs Good Inputs).
 destruct (key_mem key inputs);cbn;rewrite IH;reflexivity.
Qed.
End Translation.

Lemma closure_filter_clean : forall predicate keys,
 Forall (fun key => key_ok key = true) keys ->
 Forall (fun key => key_ok key = true) (filter predicate keys).
Proof.
 intros predicate keys Clean;rewrite Forall_forall in *;intros key Member.
 apply filter_In in Member as [Member _];exact (Clean key Member).
Qed.

Lemma provides_runtime_key : forall C key,
 A.provides C (A.rkey_of C key) <-> A.provides C key.
Proof. intros C [t|v|f|r m theta|u];reflexivity. Qed.

Section Maps.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Notation W := (BodySoundCore.W w R Fl).

Theorem closure_captured_map : forall c e F C prototype Cg keys s sm final,
 CxRel w R Fl c e F C -> FunEnvTables Fl prototype Cg -> Tri (en_uf prototype) ->
 Forall (fun key => key_ok key = true) keys ->
 smap_at prototype e (map (fun key => (key,site_key e key)) keys) s = Good (sm,final) ->
 Inv s -> Later w P R Fl final ->
 sm = map (fun key => (A.rkey_of Cg (key_tr Fl key), A.rkey_of C (A.at_site C (key_tr Fl key)))) keys.
Proof.
 intros c e F C prototype Cg keys s sm final Caller Tables Triangular Keys Run Valid Last.
 rewrite (smap_at_context w (P:=P) R Fl prototype e Cg C _ s sm final Run Valid Last
   (fet_sub _ _ _ Tables) Triangular (cr_sub _ _ _ _ _ _ _ Caller)
   (proj1 (cr_uf _ _ _ _ _ _ _ Caller))).
 - rewrite map_map;apply map_ext;intro key;cbn [fst snd].
   rewrite (key_tr_site w R Fl c e F C key Caller);reflexivity.
 - apply Forall_forall;intros pair Member;apply in_map_iff in Member as [key [<- Member]].
   rewrite Forall_forall in Keys;pose proof (Keys key Member) as Good.
   cbn [fst snd];split.
   + destruct key;cbn [key_apps];try exact I.
     exact (fet_apps _ _ _ Tables _ eq_refl Good).
   + exact (caller_key_apps w R Fl b Load c e F C key Caller Good).
Qed.

Theorem closure_captured_available : forall c e F C prototype Cg keys mi fg bg
 guardStart guardOut guardEnd s sm final,
 CxRel w R Fl c e F C -> FunEnvTables Fl prototype Cg -> Tri (en_uf prototype) ->
 Forall (fun key => key_ok key = true) keys ->
 closure_capture_guard mi e fg bg keys guardStart = Good (guardOut,guardEnd) ->
 smap_at prototype e (map (fun key => (key,site_key e key)) keys) s = Good (sm,final) ->
 Inv s -> Later w P R Fl final ->
 forall formal actual, In (formal,actual) sm -> A.provides C actual.
Proof.
 intros c e F C prototype Cg keys mi fg bg guardStart guardOut guardEnd s sm final
   Caller Tables Triangular Keys Guard Run Valid Last formal actual Member.
 rewrite (closure_captured_map c e F C prototype Cg keys s sm final Caller Tables Triangular Keys Run Valid Last) in Member.
 apply in_map_iff in Member as [key [Pair InKeys]].
 injection Pair as SameFormal SameActual;subst actual.
 apply provides_runtime_key.
 eapply (key_present_site_provides w R Fl b Load c e F C key Caller).
 - rewrite Forall_forall in Keys;exact (Keys key InKeys).
 - exact (closure_capture_present mi e fg bg keys guardStart guardOut guardEnd Guard key InKeys).
Qed.

Theorem closure_linked_map : forall (Finj : forall a b, Fl a = Fl b -> a = b)
 formal actual Cg Cu inputs needs s sm final,
 FunEnvTables Fl formal Cg -> FunEnvTables Fl actual Cu ->
 Tri (en_uf formal) -> Tri (en_uf actual) ->
 Forall (fun key => key_ok key = true) inputs ->
 Forall (fun key => key_ok key = true) needs ->
 smap_at formal actual (map (fun key => (key,key)) (closure_linked inputs needs)) s = Good (sm,final) ->
 Inv s -> Later w P R Fl final ->
 sm = map (fun key => (A.rkey_of Cg key,A.rkey_of Cu key))
   (filter (fun key => existsb (item_eqb key) (map (key_tr Fl) inputs)) (map (key_tr Fl) needs)).
Proof.
 intros Finj formal actual Cg Cu inputs needs s sm final Formal Actual Tf Ta Inputs Needs Run Valid Last.
 rewrite <- (closure_linked_translation Fl Finj inputs needs Inputs Needs),map_map.
 eapply (copy_map_from_tables w (P:=P) R Fl formal actual Cg Cu (closure_linked inputs needs)
   s sm final);try eassumption.
 apply closure_filter_clean;exact Needs.
Qed.

Theorem closure_captured_split_map : forall (Finj : forall a b, Fl a = Fl b -> a = b)
 c e F C prototype Cg inputs needs s sm final,
 CxRel w R Fl c e F C -> FunEnvTables Fl prototype Cg -> Tri (en_uf prototype) ->
 Forall (fun key => key_ok key = true) inputs ->
 Forall (fun key => key_ok key = true) needs ->
 smap_at prototype e (map (fun key => (key,site_key e key)) (closure_captured inputs needs)) s = Good (sm,final) ->
 Inv s -> Later w P R Fl final ->
 sm = map (fun key => (A.rkey_of Cg key,A.rkey_of C (A.at_site C key)))
   (filter (fun key => negb (existsb (item_eqb key) (map (key_tr Fl) inputs))) (map (key_tr Fl) needs)).
Proof.
 intros Finj c e F C prototype Cg inputs needs s sm final Caller Tables Triangular Inputs Needs Run Valid Last.
 rewrite <- (closure_captured_translation Fl Finj inputs needs Inputs Needs),map_map.
 eapply (closure_captured_map c e F C prototype Cg (closure_captured inputs needs) s sm final);
   try eassumption.
 apply closure_filter_clean;exact Needs.
Qed.

End Maps.
