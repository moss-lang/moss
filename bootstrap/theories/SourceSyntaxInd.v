(* Reusable induction through the nested lists of the source AST. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Sx.
Import ListNotations.
Section AInd.
  Variables (Pe : aexpr -> Prop) (Pb : ablock -> Prop) (Ps : astmt -> Prop).
  Definition Po (o : option aexpr) : Prop := match o with Some y => Pe y | None => True end.
  Hypothesis HUnit : Pe AEUnit.
  Hypothesis HThis : Pe AEThis.
  Hypothesis HPath : forall p a, Pe (AEPath p a).
  Hypothesis HCall : forall p a args, Forall Pe args -> Pe (AECall p a args).
  Hypothesis HField : forall o n, Pe o -> Pe (AEField o n).
  Hypothesis HMeth : forall o p args, Pe o -> Forall Pe args -> Pe (AEMeth o p args).
  Hypothesis HRec : forall p a fs, Forall (fun q => Po (snd q)) fs -> Pe (AERec p a fs).
  Hypothesis HIf : forall c bl el, Pe c -> Pb bl -> Po el -> Pe (AEIf c bl el).
  Hypothesis HMatch : forall sc arms, Pe sc -> Forall (fun q => Pe (snd q)) arms -> Pe (AEMatch sc arms).
  Hypothesis HRet : forall o, Po o -> Pe (AEReturn o).
  Hypothesis HBreak : Pe AEBreak.
  Hypothesis HBlock : forall bl, Pb bl -> Pe (AEBlock bl).
  Hypothesis HUn : forall op y, Pe y -> Pe (AEUnary op y).
  Hypothesis HBin : forall op l r, Pe l -> Pe r -> Pe (AEBinary op l r).
  Hypothesis HLit : forall t, Pe (AELit t).
  Hypothesis HLam : forall ps r y, Pe y -> Pe (AELam ps r y).
  Hypothesis HBlk : forall ss tl, Forall Ps ss -> Po tl -> Pb (ABlock ss tl).
  Hypothesis HLet : forall n t y, Pe y -> Ps (ASLet n t y).
  Hypothesis HVar : forall n t y, Pe y -> Ps (ASVar n t y).
  Hypothesis HAssign : forall n y, Pe y -> Ps (ASAssign n y).
  Hypothesis HBind : forall its, Forall (fun q => Po (snd q)) its -> Ps (ASBind its).
  Hypothesis HWhile : forall c bl, Pe c -> Pb bl -> Ps (ASWhile c bl).
  Hypothesis HLoop : forall bl, Pb bl -> Ps (ASLoop bl).
  Hypothesis HExpr : forall y, Pe y -> Ps (ASExpr y).

  Fixpoint aexpr_ind' (x : aexpr) : Pe x :=
    let po (o : option aexpr) : Po o := match o as o0 return Po o0 with Some y => aexpr_ind' y | None => I end in
    match x with
    | AEUnit => HUnit
    | AEThis => HThis
    | AEPath p a => HPath p a
    | AECall p a args => HCall p a args ((fix go (l : list aexpr) : Forall Pe l :=
        match l with [] => Forall_nil _ | y :: r => Forall_cons _ (aexpr_ind' y) (go r) end) args)
    | AEField o n => HField o n (aexpr_ind' o)
    | AEMeth o p args => HMeth o p args (aexpr_ind' o) ((fix go (l : list aexpr) : Forall Pe l :=
        match l with [] => Forall_nil _ | y :: r => Forall_cons _ (aexpr_ind' y) (go r) end) args)
    | AERec p a fs => HRec p a fs ((fix go (l : list (name * option aexpr)) : Forall (fun q => Po (snd q)) l :=
        match l with [] => Forall_nil _ | q :: r => @Forall_cons _ (fun q => Po (snd q)) q r (po (snd q)) (go r) end) fs)
    | AEIf c bl el => HIf c bl el (aexpr_ind' c) (ablock_ind' bl) (po el)
    | AEMatch sc arms => HMatch sc arms (aexpr_ind' sc) ((fix go (l : list (apat * aexpr)) : Forall (fun q => Pe (snd q)) l :=
        match l with [] => Forall_nil _ | q :: r => @Forall_cons _ (fun q => Pe (snd q)) q r (aexpr_ind' (snd q)) (go r) end) arms)
    | AEReturn o => HRet o (po o)
    | AEBreak => HBreak
    | AEBlock bl => HBlock bl (ablock_ind' bl)
    | AEUnary op y => HUn op y (aexpr_ind' y)
    | AEBinary op l r => HBin op l r (aexpr_ind' l) (aexpr_ind' r)
    | AELit t => HLit t
    | AELam ps r y => HLam ps r y (aexpr_ind' y)
    end
  with ablock_ind' (bl : ablock) : Pb bl :=
    match bl with
    | ABlock ss tl => HBlk ss tl ((fix go (l : list astmt) : Forall Ps l :=
        match l with [] => Forall_nil _ | q :: r => Forall_cons _ (astmt_ind' q) (go r) end) ss)
        (match tl as o0 return Po o0 with Some y => aexpr_ind' y | None => I end)
    end
  with astmt_ind' (st0 : astmt) : Ps st0 :=
    match st0 with
    | ASLet n t y => HLet n t y (aexpr_ind' y)
    | ASVar n t y => HVar n t y (aexpr_ind' y)
    | ASAssign n y => HAssign n y (aexpr_ind' y)
    | ASBind its => HBind its ((fix go (l : list (spec * option aexpr)) : Forall (fun q => Po (snd q)) l :=
        match l with [] => Forall_nil _ | q :: r =>
          @Forall_cons _ (fun q => Po (snd q)) q r (match snd q as o0 return Po o0 with Some y => aexpr_ind' y | None => I end) (go r) end) its)
    | ASWhile c bl => HWhile c bl (aexpr_ind' c) (ablock_ind' bl)
    | ASLoop bl => HLoop bl (ablock_ind' bl)
    | ASExpr y => HExpr y (aexpr_ind' y)
    end.
End AInd.
