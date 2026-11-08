(* Actual method-binding stages, shared by anonymous and named providers. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound ElabFrame MethodKeyTr.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL].

Definition method_bind_types (e : env) (mi : nat) (a : sapp) :=
 match a with
 | Some bs => let! sb := app_subst 200 e mi bs [] in ret (sb ++ tymap e)
 | None => ret (tymap e)
 end.

Definition method_bind_provider (fuel : nat) (c : cx) (e : env) (y : aexpr)
 (this : ety) (m : gsym) (sg : fsig) (ts : string) :=
 match y with
 | AELam ps rt body =>
     lambda_provider fuel c e ps rt body m sg (Some this) ("`" ++ ts ++ gs_name m ++ "`")%string
 | _ =>
     let! pv := expect_provider c e y this in
     let! sm := match_fn_sig fuel c e pv sg "method" in
     let! tgt := if is_some (gs_recv pv) then ret (gs_id pv) else wrapper fuel pv in
     ret (tgt, [], sm)
 end.

Definition method_bind_suffix (c : cx) (e : env) (tm : list (nat * ety))
 (this : ety) (m : gsym) (sg : fsig) (ts : string) (target : nat) (pre : list xstmt) (sm : smap) :=
 let mi := cx_mod c in
 let! k := method_key e mi this m in
 let! fm := match k with KyMth t mid => family_member mi e t mid | _ => ret None end in
 (match fm with
 | Some (fg, _) => err mi ("`" ++ ts ++ gs_name m ++ "` is a member of the family `" ++ gs_name fg ++
                         "` in force, which provides it; it cannot be bound directly (D84 rule 7)")%string
 | None => ret tt
 end) ;;
 let e := match k with KyMth t mid => set_mth e t mid sg | _ => e end in
 let! e := provided_at e k m tm true in
 match k with
 | KyMth t mid => let! t' := ety_ty t in let! th := key_theta e k in
     record_reify_event (REMethod ety key t mid target (reify_sig e sg)
       (map (fun p => (fst p, canon e (snd p))) tm)) ;;
     ret (pre ++ [SBindMth t' mid th target sm], e, c)
 | _ => fail "internal: method key"
 end.

Theorem check_bind_method_trace : forall fuel c e pa dn a y s xs e' c' final,
 check_bind (S fuel) c e (Spec (Some pa) (Some dn) a) y s = Good ((xs,e',c'),final) ->
 exists tm types recv resolved m selected this typed sg signed ts shown target pre sm provided,
 method_bind_types e (cx_mod c) a s = Good (tm,types) /\
 resolve_type_symbol (cx_mod c) pa types = Good (recv,resolved) /\
 find_method_decl (cx_mod c) recv dn resolved = Good (Some m,selected) /\
 symbol_type 200 (cx_mod c) recv tm e selected = Good (this,typed) /\
 fn_sig m tm e (Some this) typed = Good (sg,signed) /\
 show this signed = Good (ts,shown) /\
 method_bind_provider fuel c e y this m sg ts shown = Good ((target,pre,sm),provided) /\
 method_bind_suffix c e tm this m sg ts target pre sm provided = Good ((xs,e',c'),final).
Proof.
 intros fuel c e pa dn a y s xs e' c' final Run.
 cbn [check_bind] in Run.
 apply bind_inv in Run as [tm [types [Types Run]]].
 change (method_bind_types e (cx_mod c) a s = Good (tm,types)) in Types.
 apply bind_inv in Run as [recv [resolved [Resolve Run]]].
 apply bind_inv in Run as [candidate [selected [Select Run]]].
 destruct candidate as [m|]; [|exfalso; exact (err_not_good _ _ _ _ _ _ Run)].
 apply bind_inv in Run as [this [typed [TypeRun Run]]].
 apply bind_inv in Run as [sg [signed [Signature Run]]].
 apply bind_inv in Run as [ts [shown [Show Run]]].
 fold (method_bind_provider fuel c e y this m sg ts) in Run.
 apply bind_inv in Run as [[[target pre] sm] [provided [Provider Run]]].
 change (method_bind_suffix c e tm this m sg ts target pre sm provided = Good ((xs,e',c'),final)) in Run.
 exists tm,types,recv,resolved,m,selected,this,typed,sg,signed,ts,shown,target,pre,sm,provided.
 repeat split; assumption.
Qed.
