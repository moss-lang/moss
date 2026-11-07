(* The fueled interpreter for the Core Moss evaluation relation in CoreFragment.v. *)
From Stdlib Require Import List.
From NM Require Export Audit CoreFragment.
Import ListNotations.

Fixpoint interp (fuel : nat) (Sg : gsig) (d : denv) (g : venv) (e : expr)
  {struct fuel} : res :=
  match fuel with
  | 0 => OutOfFuel
  | S k =>
      let interp_args := interp_list k Sg d g in
      match e with
      | EVar x => match lookup_venv x g with
                  | Some w => Ok w
                  | None => Err
                  end
      | EUnit => Ok VUnit
      | EVl v => match lookup_denv (IVal v) d with
                 | Some (DVal w) => Ok w
                 | _ => Err
                 end
      | ETag n th e1 =>
          match interp k Sg d g e1 with
          | Ok w => Ok (VTag n w)
          | rr => rr
          end
      | ECallD f th sm args =>
          match g_fn Sg f with
          | Some (_, _, Some body) =>
              match interp_args args with
              | inl ws =>
                  match compose_smap d sm with
                  | Some d' => interp k Sg d' (mk_venv ws) body
                  | None => Err
                  end
              | inr rr => rr
              end
          | _ => Err
          end
      | ECallA f args =>
          match lookup_denv (IFn f) d with
          | Some (DClo f' df) =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp_args args with
                  | inl ws => interp k Sg df (mk_venv ws) body
                  | inr rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMeth e0 m prov args =>
          match lookup_denv prov d with
          | Some (DClo f' d') =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp k Sg d g e0 with
                  | Ok w0 =>
                      match interp_args args with
                      | inl ws => interp k Sg d' (mk_venv (w0 :: ws)) body
                      | inr rr => rr
                      end
                  | rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMatch e0 arms =>
          match interp k Sg d g e0 with
          | Ok (VTag n w') =>
              match find_arm n arms with
              | Some (x, eb) => interp k Sg d ((x, w') :: g) eb
              | None => Err
              end
          | Ok VUnit => Err
          | rr => rr
          end
      | ELet x e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg d ((x, w1) :: g) e2
          | rr => rr
          end
      | EBindTy _ _ e1 => interp k Sg d g e1
      | EBindVal v e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg ((IVal v, DVal w1) :: d) g e2
          | rr => rr
          end
      | EBindFn f f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IFn f, DClo f' dc) :: d) g e1
          | None => Err
          end
      | EBindMth tauk m th f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IMth tauk m th, DClo f' dc) :: d) g e1
          | None => Err
          end
      | EBindFun u h sm smc e1 =>
          match compose_smap d smc with
          | Some dc => interp k Sg ((IFun u, DFun h [sm] dc) :: d) g e1
          | None => Err
          end
      | EBindFunA u u' sm e1 =>
          match lookup_denv (IFun u') d with
          | Some (DFun h sms dc) =>
              interp k Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) g e1
          | _ => Err
          end
      | EApply u th sm e1 =>
          match interp_apply k Sg d u th sm with
          | inl dr => interp k Sg (dr ++ d) g e1
          | inr rr => rr
          end
      end
  end
with interp_list (fuel : nat) (Sg : gsig) (d : denv) (g : venv)
    (l : list expr) {struct fuel} : sum (list value) res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match l with
      | [] => inl []
      | e1 :: r =>
          match interp k Sg d g e1 with
          | Ok w =>
              match interp_list k Sg d g r with
              | inl ws => inl (w :: ws)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with interp_binds (fuel : nat) (Sg : gsig) (d : denv) (bs : list bnd)
    {struct fuel} : sum denv res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match bs with
      | [] => inl d
      | BTy _ _ :: r => interp_binds k Sg d r
      | BVal v e1 :: r =>
          match interp k Sg d [] e1 with
          | Ok w1 => interp_binds k Sg ((IVal v, DVal w1) :: d) r
          | rr => inr rr
          end
      | BFn f f' sm :: r =>
          match compose_smap d sm with
          | Some dc => interp_binds k Sg ((IFn f, DClo f' dc) :: d) r
          | None => inr Err
          end
      | BMth tauk m th f' sm :: r =>
          match compose_smap d sm with
          | Some dc => interp_binds k Sg ((IMth tauk m th, DClo f' dc) :: d) r
          | None => inr Err
          end
      | BFun u h sm smc :: r =>
          match compose_smap d smc with
          | Some dc => interp_binds k Sg ((IFun u, DFun h [sm] dc) :: d) r
          | None => inr Err
          end
      | BFunA u u' sm :: r =>
          match lookup_denv (IFun u') d with
          | Some (DFun h sms dc) =>
              interp_binds k Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) r
          | _ => inr Err
          end
      | BApply u th sm :: r =>
          match interp_apply k Sg d u th sm with
          | inl dr => interp_binds k Sg (dr ++ d) r
          | inr rr => inr rr
          end
      end
  end
with interp_apply (fuel : nat) (Sg : gsig) (d : denv) (u : usym) (th : subst)
    (sm : smap) {struct fuel} : sum denv res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match lookup_denv (IFun u) d with
      | Some (DFun h sms dc) =>
          match g_fun Sg h with
          | Some (_, Dr, Some bs) =>
              match compose_smap d sm with
              | Some d1 =>
                  match run_chain d1 sms with
                  | Some d2 =>
                      match interp_binds k Sg (d2 ++ dc) bs with
                      | inl d3 =>
                          match compose_smap d3 (inst_smap th Dr) with
                          | Some dr => inl dr
                          | None => inr Err
                          end
                      | inr rr => inr rr
                      end
                  | None => inr Err
                  end
              | None => inr Err
              end
          | _ => inr Err
          end
      | _ => inr Err
      end
  end.
