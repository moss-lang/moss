(* Audit.v — source-to-program and program-to-world semantics.
   The public build/run interface and its five obligations appear at the end.
   All generic definitions of Builds and Runs are in this file:
     I. Source syntax and module loading.
     II. Elaborated programs and runtime structures.
     III. Reference execution.
     V. Declarative typing and translation.
   The surface grammar is generated from MossGrammar.vy. *)
From Stdlib Require Import PrimInt63.
From Stdlib Require Import String Ascii List Bool Arith.
From Stdlib Require Strings.Byte.
From NM Require Surface MossActions MossGrammar.
Import ListNotations.

(* ######################################################################## *)
(* Part I. The input                                                         *)
(* ######################################################################## *)

Module Input := Surface.Input.

(* The file relation uses the parse trees and semantic actions generated
   from MossGrammar.vy. Lexes connects it to the source bytes. *)
Module Syntax.
Include Surface.Syntax.

Definition File (ts : list token) (f : Input.afile) : Prop :=
  exists tree : MossGrammar.Gram.parse_tree
    (MossGrammar.Gram.NT MossGrammar.Gram.file'nt) (map MossGrammar.of_token ts),
    MossGrammar.Gram.pt_sem tree = MossActions.Checked f.

Definition Derives (bytes : list Byte.byte) (f : Input.afile) : Prop :=
  exists ts, Lexes 0 (source_bytes bytes) ts /\ File ts f.
End Syntax.

(* ######################################################################## *)
(* Part I.3  The module loader                                               *)
(* ######################################################################## *)

(* The loader (py/bundle.py; collect.Loader's normalize, resolve and
   wants_prelude, moved verbatim from Parse.v), and `Bundled`, what a
   bundle loaded from a file system `read` must be. *)
Module Loader.
Import Stdlib.Strings.Byte.
Import Input.
Local Open Scope string_scope.

Fixpoint split_bytes (sep : byte) (cs : list byte) (cur : list byte) : list (list byte) :=
  match cs with
  | [] => [rev' cur]
  | c :: r => if Byte.eqb c sep then rev' cur :: split_bytes sep r [] else split_bytes sep r (c :: cur)
  end.

(* PurePosixPath's parts after the root: empty and `.` segments dropped *)
Definition segs (s : string) : list string :=
  map string_of_list_byte
    (filter (fun seg => negb (match seg with [] => true | ["."%byte] => true | _ => false end))
            (split_bytes "/"%byte (list_byte_of_string s) [])).

Definition is_abs (s : string) : bool :=
  match list_byte_of_string s with "/"%byte :: _ => true | _ => false end.

(* Loader.normalize(base, path), with `root` the working directory *)
Definition normalize (root : string) (base : option string) (p : string) : string :=
  let '(jabs, jsegs) :=
    if is_abs p then (true, segs p)
    else match base with
         | None => (false, segs p)
         | Some b => (is_abs b, app (removelast (segs b)) (segs p))
         end in
  let parts := if jabs then "/" :: jsegs else "/" :: app (segs root) jsegs in
  let acc := fold_left (fun acc part =>
                if String.eqb part ".." then
                  match acc with h :: t => if String.eqb h ".." then part :: acc else t | [] => [part] end
                else part :: acc) parts [] in
  match rev' acc with
  | [] => "."
  | "/" :: rest => "/" ++ String.concat "/" rest
  | ps => String.concat "/" ps
  end.

Definition first_segment (p : string) : string :=
  match split_bytes "/"%byte (list_byte_of_string p) [] with
  | s :: _ => string_of_list_byte s
  | [] => ""
  end.

(* Loader.resolve (D64): `.`/`..`-relative and absolute paths are the
   importer's; any other names a file of the library, the prelude's
   directory *)
Definition resolve (root prelude importer p : string) : string :=
  let first := first_segment p in
  if is_abs p || String.eqb first "." || String.eqb first ".." then normalize root (Some importer) p
  else normalize root (Some prelude) p.

(* The directory prefix, including its final slash, preserves relative
   paths as well as absolute ones. Disabling the auto-prelude retains
   the default library directory, `lib/` relative to the working directory. *)
Fixpoint directory_cut (p : string) (pos cut : nat) : nat :=
  match p with
  | EmptyString => cut
  | String c rest => directory_cut rest (S pos)
      (if Ascii.eqb c "/"%char then S pos else cut)
  end.
Definition directory (p : string) : string := substring 0 (directory_cut p 0 0) p.
Definition library_dir (prelude : string) : option string :=
  if String.eqb prelude "" then Some "lib/" else Some (directory prelude).

Definition parent_str (p : string) : string :=
  "/" ++ String.concat "/" (removelast (segs p)).

Definition wants_prelude (prelude p : string) : bool :=
  negb (String.eqb p prelude) && negb (String.prefix (parent_str prelude ++ "/") p).


(* `Bundled read root prelude entry b`: the bundle b is what loading
   `entry` with `prelude` means.  Its entry and prelude are the given
   paths, normalized against the working directory `root`; it has each
   module once, the entry among them; and each module is a file that
   `read` gives, whose bytes derive an AST f (Syntax.Derives), and the
   module holds exactly f, whether it wants the prelude,
   and its imports resolved (D64); and every module it imports, and the
   prelude if it wants it, is in the bundle too. *)
Definition Bundled (read : string -> option string) (root prelude_in entry_in : string) (b : bundle) : Prop :=
  b_entry b = normalize root None entry_in /\ b_prelude b = normalize root None prelude_in /\
  NoDup (map am_path (b_mods b)) /\ In (b_entry b) (map am_path (b_mods b)) /\
  forall m, In m (b_mods b) ->
    exists text f, read (am_path m) = Some text /\ Syntax.Derives (list_byte_of_string text) f /\
      am_file m = f /\
      am_prelude m = wants_prelude (b_prelude b) (am_path m) /\
      am_targets m = map (fun i => resolve root (b_prelude b) (am_path m) (i_path i)) (f_imports f) /\
      (forall t, In t (am_targets m) -> In t (map am_path (b_mods b))) /\
      (am_prelude m = true -> In (b_prelude b) (map am_path (b_mods b))).
End Loader.

(* Rocq strings store bytes. source contents must pass the UTF-8 check
   below; paths use normalized absolute names in the modeled filesystem. *)
Definition canonical_path (name : string) : bool :=
  Loader.is_abs name &&
  String.eqb (Loader.normalize "/" None name) name.

Record file_path := AbsolutePath {
  path_text : string;
  path_canonical : canonical_path path_text = true
}.
(* A build receives an explicit finite snapshot, not an oracle for an
   unbounded filesystem. Distinct entries have distinct canonical paths. *)
Record source := Source {
  source_files : list (file_path * string);
  source_unique : NoDup (map (fun file => path_text (fst file)) source_files)
}.
Definition path_of_string (name : string) : option file_path :=
  match canonical_path name as b return canonical_path name = b -> option file_path with
  | true => fun Valid => Some (AbsolutePath name Valid)
  | false => fun _ => None
  end eq_refl.

Definition source_lookup (snapshot : source) (name : string) : option string :=
  option_map snd (find (fun file => String.eqb name (path_text (fst file)))
    (source_files snapshot)).

Definition source_read (snapshot : source) (name : string) : option string :=
  match source_lookup snapshot name with
  | Some text => if Syntax.string_utf8_valid text then Some text else None
  | None => None
  end.

(* The public model has its own namespace. Implementation and proof helpers
   may import the shared surface syntax without importing this model's names. *)
Module Type REPRESENTATION.
  Parameter program_data : Type.
  Parameter payload : Type -> Type.
End REPRESENTATION.

Module Semantics (Repr : REPRESENTATION).

(* ######################################################################## *)
(* Part II. Elaborated programs                                              *)
(* ######################################################################## *)

Set Implicit Arguments.

(* Shared symbols, types and provision keys. *)


(* ===================== Symbols =========================================== *)

Definition tsym := nat.  (* abstract type symbols *)
Definition nsym := nat.  (* nominal tag names *)
Definition vsym := nat.  (* val symbols *)
Definition fsym := nat.  (* fn symbols *)
Definition msym := nat.  (* method symbols *)
Definition var  := nat.  (* term variables *)
Definition usym := nat.  (* functor symbols [D55]/[D78] *)

(* ===================== Types and substitutions =========================== *)

(* Type arguments are finite rows over a nominal's parameter symbols. *)

Inductive ty : Type :=
| TRigid : tsym -> ty                       (* fixed abstract type identity *)
| TRecord : list (string * ty) -> ty
| TUnit  : ty
| TAbs   : tsym -> ty                       (* abstract type symbol (incl. This) *)
| TNom   : nsym -> list (tsym * ty) -> ty   (* total applied nominal reference *)
| TUnion : list ty -> ty.                   (* members must be nominal refs *)

Definition subst := list (tsym * ty).

Fixpoint lookup_subst (t : tsym) (s : subst) : option ty :=
  match s with
  | [] => None
  | (t', tau) :: r => if Nat.eqb t t' then Some tau else lookup_subst t r
  end.

(* Applying a substitution.  Substitutions accumulated by the statics are
   idempotent by construction (bind t = τ stores στ), so one pass suffices. *)
Fixpoint app_subst (s : subst) (tau : ty) {struct tau} : ty :=
  match tau with
  | TUnit => TUnit
  | TRigid m => TRigid m
  | TRecord fs => TRecord ((fix go (fs : list (string * ty)) :=
      match fs with [] => [] | (f, t) :: rest => (f, app_subst s t) :: go rest end) fs)
  | TAbs t => match lookup_subst t s with Some tau' => tau' | None => TAbs t end
  | TNom n th =>
      TNom n ((fix go (l : list (tsym * ty)) : list (tsym * ty) :=
                 match l with
                 | [] => []
                 | p :: r => (fst p, app_subst s (snd p)) :: go r
                 end) th)
  | TUnion ms =>
      TUnion ((fix go (l : list ty) : list ty :=
                 match l with
                 | [] => []
                 | t' :: r => app_subst s t' :: go r
                 end) ms)
  end.

(* Boolean equality on types. *)
Fixpoint ty_eqb (a b : ty) {struct a} : bool :=
  match a, b with
  | TRigid a, TRigid b => Nat.eqb a b
  | TRecord fs, TRecord gs =>
      (fix go (fs gs : list (string * ty)) :=
        match fs, gs with
        | [], [] => true
        | (f,t)::fs, (g,u)::gs => String.eqb f g && ty_eqb t u && go fs gs
        | _, _ => false end) fs gs
  | TUnit, TUnit => true
  | TAbs t1, TAbs t2 => Nat.eqb t1 t2
  | TNom n1 th1, TNom n2 th2 =>
      Nat.eqb n1 n2 &&
      ((fix go (l1 l2 : list (tsym * ty)) : bool :=
          match l1, l2 with
          | [], [] => true
          | p1 :: r1, p2 :: r2 =>
              Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && go r1 r2
          | _, _ => false
          end) th1 th2)
  | TUnion m1, TUnion m2 =>
      (fix go (l1 l2 : list ty) : bool :=
         match l1, l2 with
         | [], [] => true
         | t1 :: r1, t2 :: r2 => ty_eqb t1 t2 && go r1 r2
         | _, _ => false
         end) m1 m2
  | _, _ => false
  end.

(* ===================== Items and telescopes ============================== *)

(* A method item carries the *receiver type* it is provided at: the paper's
   §1 key, the pair (receiver type, method symbol), plus the application θ
   that interprets the method's own requirements (deviation 6). *)
Inductive item : Type :=
| ITy  : tsym -> item
| IVal : vsym -> item
| IFn  : fsym -> item
| IMth : ty -> msym -> subst -> item
| IFun : usym -> item.   (* an abstract functor [D78]: at no instantiation *)

Definition tele := list item.

Fixpoint subst_eqb (a b : subst) : bool :=
  match a, b with
  | [], [] => true
  | p1 :: r1, p2 :: r2 =>
      Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && subst_eqb r1 r2
  | _, _ => false
  end.

Definition item_eqb (a b : item) : bool :=
  match a, b with
  | ITy t1, ITy t2 => Nat.eqb t1 t2
  | IVal v1, IVal v2 => Nat.eqb v1 v2
  | IFn f1, IFn f2 => Nat.eqb f1 f2
  | IMth r1 m1 th1, IMth r2 m2 th2 =>
      ty_eqb r1 r2 && Nat.eqb m1 m2 && subst_eqb th1 th2
  | IFun u1, IFun u2 => Nat.eqb u1 u2
  | _, _ => false
  end.

Definition map_snd (A B C : Type) (f : B -> C) (l : list (A * B)) : list (A * C) :=
  map (fun p => (fst p, f (snd p))) l.

(* A substitution acts on an item through the types it carries. *)
Definition app_subst_item (s : subst) (i : item) : item :=
  match i with
  | ITy t => ITy t
  | IVal v => IVal v
  | IFn f => IFn f
  | IMth rt m th => IMth (app_subst s rt) m (map_snd (app_subst s) th)
  | IFun u => IFun u
  end.

Definition is_ty_item (i : item) : bool :=
  match i with ITy _ => true | _ => false end.

(* Satisfaction maps: for each requirement of a callee/provider, the in-force
   item that satisfies it (recorded by elaboration, used by the dynamics). *)
Definition smap := list (item * item).

(* The reference language: typed annotations are retained through execution. *)

Local Open Scope nat_scope.

(* ===================== Values ============================================ *)

Definition fld := string.  (* source field names *)

Inductive value : Type :=
| VUnit : value
| VTag : nsym -> value -> value
| VRecord : list (fld * value) -> value
| VOpaque : Repr.payload ty -> value.

(* An operation's platform name and operands. *)
Record op := Op { op_name : string; op_syms : list nat }.

(* ===================== Expressions and bindings ========================== *)

Inductive pattern : Type :=
| PAny  : option var -> pattern                          (* _ or a binder *)
| PHead : nsym -> option var -> list (fld * var) -> pattern.

(* Code determines the generations and source aliases a closure uses.
   Capture reads these expressions once, rather than copying ambient types. *)
Record type_capture := TypeCapture {
  capture_generations : subst;
  capture_aliases : subst
}.

(* Expressions include calls, provision binding, records and control flow.
   Satisfaction maps identify the provisions passed to a call. Each binding
   has exactly one syntax form, used in expressions and in functor bodies. *)
Inductive expr : Type :=
| EVar                 : var -> expr
| EUnit                : expr
| EVal                 : vsym -> expr
| ETag                 : nsym -> subst -> expr -> expr
| ECall                : fsym -> subst -> smap -> list expr -> expr
| EInvoke              : fsym -> list expr -> expr
| EMethod              : expr -> msym -> item -> list expr -> expr
| ELet                 : var -> expr -> expr -> expr
| EBind                : binding -> expr -> expr
| ESeq                 : expr -> expr -> expr
| EAssign              : var -> expr -> expr
| EWhile               : expr -> expr -> expr
| ELoop                : expr -> expr
| EReturn              : expr -> expr
| EBreak               : expr
| ERecord              : list (fld * expr) -> expr
| EField               : expr -> fld -> expr
| EMatch               : expr -> list (pattern * expr) -> expr
with binding : Type :=
| BType                : tsym -> ty -> binding
| BValue               : vsym -> expr -> binding
| BFunction            : fsym -> fsym -> type_capture -> smap -> binding
| BMethod              : ty -> msym -> subst -> fsym -> type_capture -> smap -> binding
| BFunctor             : usym -> usym -> type_capture -> smap -> smap -> binding
(* `bind F = H;`: the closure of H, its needs read through the smap and
   each position of F's declared result read at the given position of H's *)
| BCopyFunctor         : usym -> usym -> smap -> list nat -> binding
(* an application: θ, the output destinations, the needs, and the caller's
   key for each position of F's declared result *)
| BApply               : usym -> subst -> list (tsym * tsym) -> smap -> list item -> binding
| BPrimitive           : item -> op -> binding.

(* A conditional tests the payload of a checked Bool. The inner match's
   binding is scoped to its scrutinee result, before either branch runs. *)
Definition conditional_expression (slot bool_tag true_tag false_tag : nat)
    (condition yes no : expr) : expr :=
  EMatch (EMatch condition [(PHead bool_tag (Some slot) [], EVar slot)])
    [(PHead true_tag None [], yes); (PHead false_tag None [], no)].

(* Named builders are notation for the single binding form. *)
Definition apply_expression u arguments inputs results body := EBind (BApply u arguments [] inputs results) body.
Definition apply_output_expression u arguments outputs inputs results body :=
  EBind (BApply u arguments outputs inputs results) body.
Definition bind_type_expression t tau e := EBind (BType t tau) e.
Definition bind_value_expression v value e := EBind (BValue v value) e.
Definition bind_function_expression f body types captures e := EBind (BFunction f body types captures) e.
Definition bind_method_expression receiver method application body types captures e :=
  EBind (BMethod receiver method application body types captures) e.
Definition bind_functor_expression u body types linked captured e := EBind (BFunctor u body types linked captured) e.
Definition copy_functor_expression u provider inputs positions e := EBind (BCopyFunctor u provider inputs positions) e.
Definition bind_primitive_expression key operation e := EBind (BPrimitive key operation) e.

(* ===================== Runtime structures ================================ *)

(* Generation meanings are stable within an activation. Source aliases track
   rebinding and final type outputs separately. A closure captures all three
   namespaces; grounding never reads the mutable source aliases. *)
Inductive provision_value : Type :=
| ValueProvision : value -> provision_value
| FunctionClosure : fsym -> environment -> provision_value
(* code, needs adaptation, captures, result positions. A copy composes its
   step into both at once (compose_adaptation, compose_positions), so a closure
   carries one of each however many copies made it. None is the root's
   own: a platform family's needs as they are, a root's sources in order. *)
| FunctorClosure : usym -> option smap -> environment -> option (list (option nat)) -> provision_value
| PrimitiveClosure : op -> option ty -> provision_value
with environment : Type :=
| EnvironmentWithAliases : subst -> subst -> list (item * provision_value) -> environment.
Definition Environment (types : subst) (provisions : list (item * provision_value)) :=
  EnvironmentWithAliases types [] provisions.
Definition type_meanings (d : environment) := match d with EnvironmentWithAliases ts _ _ => ts end.
Definition type_aliases (d : environment) := match d with EnvironmentWithAliases _ aliases _ => aliases end.
Definition provision_values (d : environment) := match d with EnvironmentWithAliases _ _ ps => ps end.
Definition empty_environment := Environment [] [].
Definition locals := list (var * value).
Definition ground (d : environment) (tau : ty) : ty := app_subst (type_meanings d) tau.
Definition append_environment (a b : environment) :=
  EnvironmentWithAliases (type_meanings a ++ type_meanings b)
    (type_aliases a ++ type_aliases b) (provision_values a ++ provision_values b).
Definition install_provision (key : item) (entry : provision_value) (d : environment) :=
  EnvironmentWithAliases (type_meanings d) (type_aliases d) ((key,entry) :: provision_values d).
Definition instantiate (d : environment) (th : subst) : environment :=
  let meanings := map (fun q => (fst q, ground d (snd q))) th in
  EnvironmentWithAliases meanings meanings [].
Definition typed_capture (d : environment) (types : type_capture) (dc : environment) : environment :=
  EnvironmentWithAliases
    (map (fun q => (fst q, ground d (snd q))) (capture_generations types))
    (map (fun q => (fst q, ground d (snd q))) (capture_aliases types))
    (provision_values dc).
Definition capture_environment (d dc : environment) : environment :=
  EnvironmentWithAliases (type_meanings dc ++ type_meanings d)
    (type_aliases dc ++ type_aliases d) (provision_values dc).
Definition primitive_closure (d : environment) (i : item) (p : op) : provision_value :=
  PrimitiveClosure p (match i with
    | IMth tau _ _ => Some (ground d tau)
    | _ => None end).
Fixpoint lookup_entry (i : item) (entries : list (item * provision_value)) : option provision_value :=
  match entries with
  | [] => None
  | (key,value)::rest => if item_eqb i key then Some value else lookup_entry i rest
  end.
Definition lookup_provision (i : item) (d : environment) := lookup_entry i (provision_values d).

Fixpoint lookup_local (x : var) (g : locals) : option value :=
  match g with
  | [] => None
  | (x', w) :: r => if Nat.eqb x x' then Some w else lookup_local x r
  end.

Fixpoint assign_local (x : var) (w : value) (g : locals) : option locals :=
  match g with
  | [] => None
  | (x', w') :: r =>
      if Nat.eqb x x' then Some ((x', w) :: r)
      else match assign_local x w r with
           | Some r' => Some ((x', w') :: r')
           | None => None
           end
  end.

Fixpoint select_provisions (d : environment) (sm : smap) : option environment :=
  match sm with
  | [] => Some empty_environment
  | (req, s) :: r =>
      match lookup_provision s d, select_provisions d r with
      | Some en, Some d' => Some (install_provision req en d')
      | _, _ => None
      end
  end.

(* A closure's needs adaptation, applied to what the application selected. *)
Definition adapt_provisions (d : environment) (a : option smap) : option environment :=
  match a with
  | None => Some d
  | Some sm => select_provisions d sm
  end.
(* The source a satisfaction map selects for a requirement: its first
   entry's, as select_provisions installs them. *)
Fixpoint smap_source (req : item) (sm : smap) : option item :=
  match sm with
  | [] => None
  | (r, s) :: rest => if item_eqb req r then Some s else smap_source req rest
  end.
(* `bind F = H;` composes its smap sm (H's needs, read from F's) under H's
   adaptation a: each requirement a reads from H's key s reads from sm's
   source for s. Selecting through the composition is selecting through sm
   and then a (EagerCopy.adapt_compose). A key sm lacks is kept, which only
   matters where the two-step selection is stuck. *)
Definition compose_adaptation (a : option smap) (sm : smap) : option smap :=
  Some (match a with
    | None => sm
    | Some c => map (fun q => (fst q, match smap_source (snd q) sm with Some s => s | None => snd q end)) c
    end).

Definition parameter_values (ws : list value) : locals := combine (seq 0 (List.length ws)) ws.

(* Program bodies and their declared formal interfaces. *)
(* A semantic program contains: the bodies of every
   defined fn (and of the fns elaboration synthesizes: forwarders, lambda
   providers, wrappers), the core functor bodies `bind F = G;` made, Bool's
   symbols, `main`, main's initial runtime environment (the platform's
   provisions for what main assumes), and the names of primitives the
   model does not implement, in diagnostic index order. *)
Record functor_code := FunctorCode {
  fc_inputs : list tsym;
  (* the inputs each application supplies (the rest are captured) *)
  fc_linked : list tsym;
  (* the body's type table (G's canonical c_ty): an input it maps to a
     type reads as that type over the inputs' meanings, so the body's
     environment absorbs G's merges; an input it omits reads as itself *)
  fc_table : list (tsym * ty);
  (* the result's type outputs (the body's roles); an application reads
     each one's final meaning *)
  fc_outputs : tele;
  fc_bindings : list binding;
  (* where the body left each item of the declared result, position by
     position (the declared result's keys, ResultKeys): the body's own key
     for it. An application installs the item at position i under the
     caller's key for position i, so no key of the body's is read in the
     caller's terms (tests/regressions/functor_input_output_generations.moss,
     tests/wasi/copy_extra_key.moss, tests/wasi/linked_copy_bracket.moss) *)
  fc_sources : list item
}.
(* The static readings attached to generated code by its translation. *)
Record code_origins := CodeOrigins {
  lambda_sections : list (nat * subst);
  linked_origins : list (nat * (nat * nat))  (* slot, declared target and provider *)
}.
Definition empty_code_origins : code_origins := CodeOrigins [] [].

Record program : Type := Program {
  program_functions : list (nat * expr);
  program_functors : list (usym * functor_code);
  program_bool : nat; program_true : nat; program_false : nat;
  program_main : nat;
  program_environment : environment;
  program_unsupported : list string;
  program_platform : Repr.program_data;
  program_origins : code_origins
}.

(* Execution view: symbol lookups, runtime inputs and representation data.
   program_of constructs this view from program and the host inputs. *)
Record program_view : Type := ProgramView {
  view_function    : fsym -> option expr;
  view_functor   : usym -> option functor_code;
  view_bool  : nsym;
  view_true  : nsym;
  view_false : nsym;
  view_arguments  : list (list int);
  (* the host filesystem, an input like argv: a path's bytes, if readable *)
  view_read  : list int -> option (list int);
  view_platform : Repr.program_data
}.

Definition bind_type (d : environment) (t : tsym) (tau : ty) : environment :=
  EnvironmentWithAliases (type_meanings d) ((t, ground d tau) :: type_aliases d) (provision_values d).
Definition call_environment (d : environment) (th : subst) (dc : environment) : environment :=
  (* A new activation receives its formals and lexical capture, rather than
     unrelated ambient caller generations which may reuse its local names. *)
  append_environment (instantiate d th) dc.

Definition output_meaning (d : environment) (source : tsym) : option ty :=
  match lookup_subst source (type_aliases d) with
  | Some meaning => Some meaning
  | None => lookup_subst source (type_meanings d)
  end.
(* Rebase exactly the provider's formal roles. Linked inputs (fc_linked)
   are read from θ here; the rest, including an input that only a copy of
   the functor takes, use their captured source meanings. Unrelated
   captured generations cannot occupy this activation's local output names.
   An input the body's table (fc_table) maps to a type then reads as that
   type over those meanings: a merged-away input reads as its
   representative, so the body's merges hold by construction. Elaborated
   code reads types through that table, so it never sees the difference. *)
Definition application_environment (P : program_view) (d : environment) (h : usym) (th : subst) (dc : environment) : environment :=
  let linked := match view_functor P h with Some code => fc_linked code | None => [] end in
  let inputs := match view_functor P h with Some code => fc_inputs code | None => [] end in
  let table := match view_functor P h with Some code => fc_table code | None => [] end in
  let base source :=
    match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
    | Some meaning => ground d meaning
    | None => match output_meaning dc source with
      | Some meaning => meaning | None => TAbs source end
    end in
  let bases := map (fun source => (source, base source)) inputs in
  let meanings := map (fun source => (source,
    match lookup_subst source table with
    | Some t => app_subst bases t
    | None => base source
    end)) inputs in
  EnvironmentWithAliases meanings meanings (provision_values dc).

(* A closure's result positions: position i reads the root's source at
   the i-th entry (None, the root's own sources in order). Each position of
   a copy names a position of its source's declared result; a stray one
   (None) is unreachable for a checked program (CopyInterpretation), and no
   provision is keyed by a type item, so it makes the selection stuck. *)
Definition closure_sources (p : option (list (option nat))) (sources : list item) : list item :=
  match p with
  | None => sources
  | Some q => map (fun o => match o with Some i => nth i sources (ITy 0) | None => ITy 0 end) q
  end.
(* `bind F = H;` composes its positions rm (position j of F's result is
   position rm_j of H's) under H's positions p. Reading the root's sources
   through the composition is reading them through p and then rm
   (EagerCopy.sources_compose), exactly. *)
Definition compose_positions (p : option (list (option nat))) (rm : list nat) : option (list (option nat)) :=
  Some (match p with
    | None => map Some rm
    | Some q => map (fun j => nth j q None) rm
    end).
(* An application's selection: the caller's key for each position, read
   from that position's source. A later position wins, as the caller's
   static context reads a repeated key at its last setter (output_cx's
   `rev es`), so the run-time provision under a repeated caller key is the
   one its static type describes, whatever the sources of the earlier
   positions (two of F's keys may meet only at the caller: A.m and B.m
   with A and B read as one type). *)
Definition result_selection (rs : list item) (p : option (list (option nat))) (sources : list item) : smap :=
  rev (combine rs (closure_sources p sources)).
(* Read every output from one completed body snapshot before installing any
   caller destination. An unchanged formal may be an output too. *)
Fixpoint read_result_types (d : environment) (Dr : tele) (outputs : list (tsym * tsym))
    : option (subst * subst) :=
  match outputs with
  | [] => Some ([], [])
  | (source, destination) :: rest =>
      if existsb (item_eqb (ITy source)) Dr then
        match output_meaning d source, read_result_types d Dr rest with
        | Some meaning, Some (writes, aliases) =>
            Some ((destination, meaning) :: writes, (source, meaning) :: aliases)
        | _, _ => None end
      else None
  end.
Fixpoint fresh_result_types (writes occupied : subst) : bool :=
  match writes with
  | [] => true
  | (destination, meaning) :: rest =>
      match lookup_subst destination occupied with
      | Some _ => false
      | None => fresh_result_types rest ((destination, meaning) :: occupied)
      end
  end.
(* The writes shadow any meaning the caller has for a destination, as the
   oracle's (dr ++ d) does. Audit's static rules keep every destination out
   of the caller's context and frame types (FreshFor, the output clauses),
   so nothing typed in scope is re-grounded; an occupied destination can
   still occur at run time (an inlined defined val's kept output, a
   lambda's capture of a declared symbol it mentions), so a freshness check
   here would make accepted programs Stuck (scratch/orch/results2-AUDIT.md).
   [fresh_result_types] stays as the check's statement. *)
Definition result_environment (outputs : list (tsym * tsym))
    (d3 : environment) (Dr : tele) (dr : environment) : option environment :=
  match read_result_types d3 Dr outputs with
  | Some (writes, aliases) => Some (EnvironmentWithAliases writes aliases (provision_values dr))
  | None => None
  end.

(* Outcomes.  Normal/Stuck/OutOfFuel are the core's Ok/Err/OutOfFuel; Returning and Breaking
   are control transfers; Trap is a defined failure,
   Exit a platform-requested process exit, UnsupportedOperation a primitive this model does not implement.
   The nat in Stuck/Trap/UnsupportedOperation is a diagnostic code only. *)
Inductive signal : Type :=
| Normal               : value -> signal
| Returning            : value -> signal
| Breaking             : signal
| Stuck                : nat -> signal
| Trap                 : nat -> signal
| Exit                 : int -> signal
| UnsupportedOperation : nat -> signal
| OutOfFuel            : signal.

(* ######################################################################## *)
(* Part III. Dynamic semantics                                               *)
(* ######################################################################## *)

(* Rule A: an application files each item of F's declared result under
   the caller's key for it (the key its scope refers to it by), read from
   the body where the root left it (closure_sources, result_selection).
   Exactly F's declared result is installed, as `bind F;` keeps exactly
   F's result (tests/wasi/copy_extra_key.moss), and no key of the body's is
   read in the caller's terms (tests/wasi/linked_copy_bracket.moss). *)

(* Reference execution is parameterized by primitive operations and their
   state. The platform supplies the laws for these calls and their state.
   The callback receives the closure's optional site and implements its
   platform's calling convention. A concrete implementation must prove that
   its representation refines this interface; the existing ExecutionModel
   proofs do not supply that bridge. *)
Module Type PRIMITIVES.
  Parameter mstate : Type.
  Parameter run_prim : program_view -> op -> option ty -> list value -> mstate -> signal * mstate.
End PRIMITIVES.

Module Interpreter (Import Prims : PRIMITIVES).
Record evaluation : Type := Evaluation { evaluation_signal : signal; evaluation_locals : locals; evaluation_state : mstate }.

(* ===================== Reference execution =============================== *)

(* Diagnostic codes for Stuck (stuck states) *)
Definition e_var := 1.      Definition e_val := 2.      Definition e_compose := 3.
Definition e_nobody := 4.   Definition e_break := 5.    Definition e_entry := 6.
Definition e_match := 7.    Definition e_chain := 8.    Definition e_assign := 9.
Definition e_bool := 10.    Definition e_field := 11.   Definition e_branch := 12.
Definition e_fun := 13.     Definition e_pat := 14.

Definition ret_call (g : locals) (r : evaluation) : evaluation :=
  match r with
  | Evaluation o _ st => Evaluation (match o with Returning w => Normal w | Breaking => Stuck e_break | o => o end) g st
  end.

Definition pop1 (r : evaluation) : evaluation :=
  match r with Evaluation o g st => Evaluation o (tl g) st end.

Definition popn (n : nat) (r : evaluation) : evaluation :=
  match r with Evaluation o g st => Evaluation o (skipn n g) st end.

Definition with_g (g : locals) (r : evaluation) : evaluation :=
  match r with Evaluation o _ st => Evaluation o g st end.

(* Transparent call entry for the interpreter-agreement proofs. *)
Definition run_prim_at := run_prim.

Definition truthy (P : program_view) (w : value) : option bool :=
  match w with
  | VTag b (VTag tag VUnit) =>
      if Nat.eqb b (view_bool P) then
        if Nat.eqb tag (view_true P) then Some true
        else if Nat.eqb tag (view_false P) then Some false else None
      else None
  | _ => None
  end.

Fixpoint assoc_fld (f : fld) (fs : list (fld * value)) : option value :=
  match fs with
  | [] => None
  | (f', w) :: r => if String.eqb f f' then Some w else assoc_fld f r
  end.

Definition get_field (f : fld) (w : value) : option value :=
  match w with
  | VRecord fs => assoc_fld f fs
  | VTag _ (VRecord fs) => assoc_fld f fs
  | _ => None
  end.

Inductive pm : Type := PMatch (bs : locals) | PNo | PBad.

Fixpoint bind_fields (fs : list (fld * var)) (r : list (fld * value)) : option locals :=
  match fs with
  | [] => Some []
  | (f, x) :: rest =>
      match assoc_fld f r, bind_fields rest r with
      | Some w, Some bs => Some ((x, w) :: bs)
      | _, _ => None
      end
  end.

Definition match_pat (p : pattern) (w : value) : pm :=
  match p with
  | PAny None => PMatch []
  | PAny (Some x) => PMatch [(x, w)]
  | PHead n ob fs =>
      match w with
      | VTag n' pay =>
          if Nat.eqb n n' then
            let b1 := match ob with Some x => [(x, pay)] | None => [] end in
            match fs with
            | [] => PMatch b1
            | _ => match pay with
                   | VRecord r => match bind_fields fs r with
                                | Some bs => PMatch (bs ++ b1)
                                | None => PBad
                                end
                   | _ => PBad
                   end
            end
          else PNo
      | _ => PNo
      end
  end.

Fixpoint select_arm (arms : list (pattern * expr)) (w : value)
  : option (locals * expr) :=
  match arms with
  | [] => None
  | (p, eb) :: r =>
      match match_pat p w with
      | PMatch bs => Some (bs, eb)
      | PNo => select_arm r w
      | PBad => None
      end
  end.

Fixpoint eval (fuel : nat) (P : program_view) (d : environment) (g : locals) (st : mstate)
    (e : expr) {struct fuel} : evaluation :=
  match fuel with
  | 0 => Evaluation OutOfFuel g st
  | S k =>
      match e with
      | EVar x => match lookup_local x g with
                  | Some w => Evaluation (Normal w) g st
                  | None => Evaluation (Stuck e_var) g st
                  end
      | EUnit => Evaluation (Normal VUnit) g st
      | EVal v => match lookup_provision (IVal v) d with
                 | Some (ValueProvision w) => Evaluation (Normal w) g st
                 | _ => Evaluation (Stuck e_val) g st
                 end
      | ETag n _ e1 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 => Evaluation (Normal (VTag n w)) g1 st1
          | r => r
          end
      | ECall f th sm args =>
          match view_function P f with
          | Some body =>
              match eval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  match select_provisions d sm with
                  | Some d' => ret_call g1 (eval k P (call_environment d th d') (parameter_values ws) st1 body)
                  | None => Evaluation (Stuck e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => Evaluation (Stuck e_nobody) g st
          end
      | EInvoke f args =>
          match lookup_provision (IFn f) d with
          | Some (FunctionClosure f' df) =>
              match view_function P f' with
              | Some body =>
                  match eval_arguments k P d g st args with
                  | inl (ws, g1, st1) => ret_call g1 (eval k P df (parameter_values ws) st1 body)
                  | inr r => r
                  end
              | None => Evaluation (Stuck e_nobody) g st
              end
          | Some (PrimitiveClosure p held) =>
              match eval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p held ws st1 in Evaluation o g1 st2
              | inr r => r
              end
          | _ => Evaluation (Stuck e_entry) g st
          end
      | EMethod e0 m prov args =>
          match lookup_provision prov d with
          | Some (FunctionClosure f' d') =>
              match view_function P f' with
              | Some body =>
                  match eval k P d g st e0 with
                  | Evaluation (Normal w0) g0 st0 =>
                      match eval_arguments k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (eval k P d' (parameter_values (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => Evaluation (Stuck e_nobody) g st
              end
          | Some (PrimitiveClosure p held) =>
              match eval k P d g st e0 with
              | Evaluation (Normal w0) g0 st0 =>
                  match eval_arguments k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p held (w0 :: ws) st1 in Evaluation o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => Evaluation (Stuck e_entry) g st
          end
      | ELet x e1 e2 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w1) g1 st1 => pop1 (eval k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | EBind binding e1 =>
          match eval_bindings k P d g st [binding] with
          | inl (d1, g1, st1) => eval k P d1 g1 st1 e1
          | inr result => result
          end
            | ESeq e1 e2 =>
          match eval k P d g st e1 with
          | Evaluation (Normal _) g1 st1 => eval k P d g1 st1 e2
          | r => r
          end
      | EAssign x e1 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 =>
              match assign_local x w g1 with
              | Some g2 => Evaluation (Normal VUnit) g2 st1
              | None => Evaluation (Stuck e_assign) g1 st1
              end
          | r => r
          end
      | EWhile c b =>
          match eval k P d g st c with
          | Evaluation (Normal w) g1 st1 =>
              match truthy P w with
              | Some true =>
                  match eval k P d g1 st1 b with
                  | Evaluation (Normal _) g2 st2 => eval k P d g2 st2 (EWhile c b)
                  | Evaluation Breaking g2 st2 => Evaluation (Normal VUnit) g2 st2
                  | r => r
                  end
              | Some false => Evaluation (Normal VUnit) g1 st1
              | None => Evaluation (Stuck e_bool) g1 st1
              end
          | r => r
          end
      | ELoop b =>
          match eval k P d g st b with
          | Evaluation (Normal _) g2 st2 => eval k P d g2 st2 (ELoop b)
          | Evaluation Breaking g2 st2 => Evaluation (Normal VUnit) g2 st2
          | r => r
          end
      | EReturn e1 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 => Evaluation (Returning w) g1 st1
          | r => r
          end
      | EBreak => Evaluation Breaking g st
      | ERecord fs =>
          match eval_arguments k P d g st (map snd fs) with
          | inl (ws, g1, st1) => Evaluation (Normal (VRecord (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | EField e1 f =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 =>
              match get_field f w with
              | Some v => Evaluation (Normal v) g1 st1
              | None => Evaluation (Stuck e_field) g1 st1
              end
          | r => r
          end
      | EMatch e0 arms =>
          match eval k P d g st e0 with
          | Evaluation (Normal w) g1 st1 =>
              match select_arm arms w with
              | Some (bs, eb) =>
                  popn (List.length bs) (eval k P d (bs ++ g1) st1 eb)
              | None => Evaluation (Stuck e_pat) g1 st1
              end
          | r => r
          end
      end
  end
with eval_arguments (fuel : nat) (P : program_view) (d : environment) (g : locals) (st : mstate)
    (l : list expr) {struct fuel} : (list value * locals * mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 =>
              match eval_arguments k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with eval_bindings (fuel : nat) (P : program_view) (d : environment) (g : locals) (st : mstate)
    (bs : list binding) {struct fuel} : (environment * locals * mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | BType t tau :: r => eval_bindings k P (bind_type d t tau) g st r
      | BValue v e1 :: r =>
          match eval k P d g st e1 with
          | Evaluation (Normal w1) g1 st1 => eval_bindings k P (install_provision (IVal v) (ValueProvision w1) d) g1 st1 r
          | rr => inr rr
          end
      | BFunction f f' types sm :: r =>
          match select_provisions d sm with
          | Some dc => eval_bindings k P (install_provision (IFn f) (FunctionClosure f' (typed_capture d types dc)) d) g st r
          | None => inr (Evaluation (Stuck e_compose) g st)
          end
      | BMethod tauk m th f' types sm :: r =>
          match select_provisions d sm with
          | Some dc => eval_bindings k P (install_provision (IMth tauk m th) (FunctionClosure f' (typed_capture d types dc)) d) g st r
          | None => inr (Evaluation (Stuck e_compose) g st)
          end
      | BFunctor u h types sm smc :: r =>
          match select_provisions d smc with
          | Some dc => eval_bindings k P (install_provision (IFun u) (FunctorClosure h (Some sm) (typed_capture d types dc) None) d) g st r
          | None => inr (Evaluation (Stuck e_compose) g st)
          end
      | BCopyFunctor u u' sm rm :: r =>
          match lookup_provision (IFun u') d with
          | Some (FunctorClosure h a dc p) =>
              eval_bindings k P (install_provision (IFun u) (FunctorClosure h (compose_adaptation a sm) dc (compose_positions p rm)) d) g st r
          | _ => inr (Evaluation (Stuck e_fun) g st)
          end
      | BApply u th outputs sm rs :: r =>
          match apply_functor k P d u th outputs sm rs st with
          | inl (dr, st1) => eval_bindings k P (append_environment dr d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | BPrimitive i p :: r => eval_bindings k P (install_provision i (primitive_closure d i p) d) g st r
      end
  end
with apply_functor (fuel : nat) (P : program_view) (d : environment) (u : usym) (th : subst)
    (outputs : list (tsym * tsym)) (sm : smap) (rs : list item) (st : mstate) {struct fuel}
    : (environment * mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel [] st)
  | S k =>
      match lookup_provision (IFun u) d with
      | Some (FunctorClosure h a dc p) =>
          match view_functor P h with
          | Some code =>
              let Dr := fc_outputs code in
              let bs := fc_bindings code in
              match select_provisions d sm with
              | Some d1 =>
                  match adapt_provisions d1 a with
                  | Some d2 =>
                      match eval_bindings k P (application_environment P d h th (append_environment d2 dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match select_provisions d3 (result_selection rs p (fc_sources code)) with
                          | Some dr => match result_environment outputs d3 Dr dr with
                              | Some result => inl (result, st3)
                              | None => inr (Evaluation (Stuck e_compose) [] st3) end
                          | None => inr (Evaluation (Stuck e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (Evaluation (Stuck e_chain) [] st)
                  end
              | None => inr (Evaluation (Stuck e_compose) [] st)
              end
          | None => inr (Evaluation (Stuck e_nobody) [] st)
          end
      | _ => inr (Evaluation (Stuck e_fun) [] st)
      end
  end.

(* Running `main`: its body in the initial environment (the platform's
   provisions for main's needs) and the initial machine. *)
Definition run_main (fuel : nat) (P : program_view) (main : fsym) (d0 : environment)
    (st0 : mstate) : evaluation :=
  match view_function P main with
  | Some body => ret_call [] (eval fuel P d0 [] st0 body)
  | None => Evaluation (Stuck e_nobody) [] st0
  end.
End Interpreter.

(* ######################################################################## *)
(* Part V. Declarative typing and translation                               *)
(* ######################################################################## *)

(* EaMain.v implements the executable elaborator for these judgments.
   PublicBuildExample.v checks an inhabited public build/run derivation.
   Elaborator soundness and completeness are obligations of PublicFor.CORRECT.

   WHAT IT SAYS.  `Elaborates b p` relates a bundle `b` (Part I) to an
   elaborated program `p` (Part II): `p` is *an* elaboration of `b` if
   there is a static reading `W` of the bundle (which declaration each
   symbol number is, what each name in each module denotes, each symbol's
   requirements) satisfying the conditions of V.1. Typing chooses a
   source plan for each function body and constant initializer under its
   context. Translation uses those plans to emit code, number locals,
   and place generated bodies in the program.

   WHAT MAKES IT DECLARATIVE.  It fixes no algorithm, no traversal order,
   no fuel, and no numbering. Symbol numbers, local-variable numbers,
   fresh generations and synthesized symbols are all
   existentially chosen (subject only to being distinct where that is
   observable).  Name resolution is stated as "this name denotes that
   symbol iff it is declared or imported so" (D44 then follows: if two
   symbols qualify there is no reading `W`).  Merging (D43) is a most
   general unifier, not a union-find run.  Requirement and need lists are
   sets.  Which provision a call uses (D61/D62/D77) is a case analysis on
   what is declared and what the context provides, and the context holds at
   most one provision per key, so there is nothing to search (D1).

   HOW TO READ IT.  V.1 the static world; V.2 types; V.3 context formation;
   V.4 calls, needs and coherence; V.5 functors (D84); V.6 expressions,
   statements, binds; V.7 the program.  Each rule cites the decision it
   encodes. Historical differences, counterexamples and open design
   questions are recorded in docs/design/audit-notes.md.
*)

Module Elaboration.
Import Input.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ======================================================================== *)
(* V.0 Small helpers: association lists, as the context's tables            *)
(* ======================================================================== *)

Fixpoint nlook {A : Type} (k : nat) (l : list (nat * A)) : option A :=
  match l with [] => None | (k', a) :: r => if Nat.eqb k k' then Some a else nlook k r end.

Fixpoint slook {A : Type} (k : string) (l : list (string * A)) : option A :=
  match l with [] => None | (k', a) :: r => if String.eqb k k' then Some a else slook k r end.

Fixpoint ilook {A : Type} (k : item) (l : list (item * A)) : option A :=
  match l with [] => None | (k', a) :: r => if item_eqb k k' then Some a else ilook k r end.

(* An instantiation: for each type symbol, what it meant (None: not in force). *)
Definition inst := list (nat * option ty).

(* A signature as elaboration reads it: parameter types and result. *)
Record fsig := FSig { sg_params : list ty; sg_ret : ty }.

Definition sig_subst (s : subst) (g : fsig) : fsig :=
  FSig (map (app_subst s) (sg_params g)) (app_subst s (sg_ret g)).

(* The generations (abstract type symbols) a type mentions. *)
Fixpoint gens (t : ty) : list nat :=
  match t with
  | TUnit | TRigid _ => []
  | TRecord fs => (fix go (fs : list (string * ty)) :=
      match fs with [] => [] | (_,t)::rest => gens t ++ go rest end) fs
  | TAbs g => [g]
  | TNom _ xs => (fix go (l : list (tsym * ty)) := match l with [] => [] | p :: r => gens (snd p) ++ go r end) xs
  | TUnion ms => (fix go (l : list ty) := match l with [] => [] | m :: r => gens m ++ go r end) ms
  end.

Definition head (t : ty) : option nat :=
  match t with TNom n _ => Some n | TAbs g => Some g | _ => None end.

Definition members (t : ty) : list ty :=
  match t with TUnion ms => ms | _ => [t] end.

Definition never : ty := TUnion [].

(* D17: a value of type t is accepted where `expected` is wanted: equal,
   divergent, or injected into a union that has all its members. *)
Definition fits (t expected : ty) : Prop :=
  t = expected \/ t = never \/
  (exists ms, expected = TUnion ms /\ forall m, In m (members t) -> In m ms).

Definition opt_fits (t : ty) (exp : option ty) : Prop :=
  match exp with Some e => fits t e | None => True end.

(* ======================================================================== *)
(* V.1 The static world: what each symbol is, what each name denotes        *)
(* ======================================================================== *)

(* The modules of a bundle, and the imports of one (the prelude first, when
   the module wants it, as an unqualified glob import). *)
Definition module_of (b : bundle) (p : string) : option amodule :=
  find (fun m => String.eqb (am_path m) p) (b_mods b).

Definition explicit_imports (m : amodule) : list (string * aimport) :=
  combine (am_targets m) (f_imports (am_file m)).

Definition imports_of (b : bundle) (m : amodule) : list (string * aimport) :=
  (if am_prelude m then [(b_prelude b, AImport (b_prelude b) None true [])] else [])
  ++ combine (am_targets m) (f_imports (am_file m)).

Inductive Loaded (b : bundle) : string -> Prop :=
| LoadEntry : Loaded b (b_entry b)
| LoadImport : forall p m t imp,
    Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) -> Loaded b t.

(* A declaration together with where it is: its module and the assume
   items enclosing it, outermost first ([D20]: requirements nest by union). *)
Record decl := Decl { d_mod : string; d_ast : adecl; d_assumes : list aitem }.

Inductive OccursIn : list adecl -> list aitem -> adecl -> Prop :=
| OccHere : forall ds d,
    In d ds -> (forall its inner, d <> ADAssume its inner) -> OccursIn ds [] d
| OccNested : forall ds its inner asm d,
    In (ADAssume its inner) ds -> OccursIn inner asm d -> OccursIn ds (its ++ asm) d.

Definition Declared (b : bundle) (d : decl) : Prop :=
  Loaded b (d_mod d) /\
  exists m, module_of b (d_mod d) = Some m /\ OccursIn (f_decls (am_file m)) (d_assumes d) (d_ast d).

Inductive dkind := DKType | DKAlias | DKTag | DKVal | DKFn | DKCtx | DKMeth | DKFunctor.

Definition kind_of (d : adecl) : option dkind :=
  match d with
  | ADType _ => Some DKType | ADBareTag _ => Some DKTag | ADAlias _ _ => Some DKAlias
  | ADTag _ _ _ _ => Some DKTag | ADVal _ _ _ => Some DKVal | ADCtx _ _ => Some DKCtx
  | ADFunctor _ _ _ _ _ => Some DKFunctor
  | ADFn RPlain _ _ _ _ => Some DKFn
  | ADFn _ _ _ _ _ => Some DKMeth
  | ADAssume _ _ => None
  end.

(* The name a declaration is filed under: plain names (types, tags,
   aliases, vals, fns, contexts, functors) and detached methods `.m`; an
   attached method `R.m` is filed under its receiver instead ([D61]). *)
Inductive nspace := NSName | NSDetached | NSAlias.

Definition filed_name (d : adecl) : option (nspace * name) :=
  match d with
  | ADFn RDetached n _ _ _ => Some (NSDetached, n)
  | ADFn (RAttached _) _ _ _ _ => None
  | ADAssume _ _ => None
  | _ => match d with
         | ADType n | ADBareTag n | ADAlias n _ | ADTag n _ _ _ | ADVal n _ _ | ADCtx n _
         | ADFunctor n _ _ _ _ | ADFn _ n _ _ _ => Some (NSName, n)
         | _ => None
         end
  end.

(* What a name in a module's scope denotes: a symbol, or (an alias) a module. *)
Inductive ent := ESym (s : nat) | EMod (p : string).

(* A context in force at a point of a body.  Every table is keyed by
   identity ([D62]): a val, fn or functor by its symbol, a method by its
   receiver's canonical *type* and its symbol; a lookup takes the latest
   entry.  `c_sub` is the merging substitution ([D43]) on generations;
   types in the other tables are canonical (c_sub applied).  `c_apps`
   records, for every provision, the instantiation it was made at ([D62],
   [D71] coherence).  `c_funs` records, for each abstract functor in
   force, what its fixed inputs X(F) denoted where it was provided ([D84]
   rules 2 and 5). *)
Inductive lmut := LLet | LVar | LCaptured.

(* A provision's signature and its captured interpretation occupy one row.
   Identity is the value/function/functor symbol or receiver-and-method pair;
   type arguments belong to the provision, not to a parallel cache. *)
Inductive provision :=
| ProvidedVal (value_type : ty) (interpretation : inst)
| ProvidedFn (signature : fsig) (interpretation : inst)
| ProvidedMethod (signature : fsig) (interpretation : inst)
| ProvidedFunctor (fixed : subst).
(* A merge records the members of each class and, when the class has a
   structured meaning, a type read from the telescope. *)
Record mclass := MClass {
  mc_members : list tsym;
  mc_shape : option ty
}.
Definition merge := list mclass.

Record cx := Cx {
  c_ty : subst;
  c_sub : subst;
  c_merge : merge;
  c_provisions : list (item * provision);
  c_locals : list (name * (ty * lmut))
}.
(* The static world: a reading of the bundle.  It is a witness: the
   relation asks only that one exist satisfying `WorldOf`. *)
Record world := World {
  w_decl  : nat -> option decl;                      (* symbol -> declaration *)
  w_scope : string -> nspace -> name -> option ent;  (* module, namespace, name -> denotation *)
  w_exp   : string -> nspace -> name -> option ent;  (* what a module exports *)
  w_attached : nat -> name -> option nat;            (* receiver symbol, name -> attached method *)
  w_reqs  : nat -> list nat;                         (* a symbol's requirements (type symbols) *)
  w_library : option string;                        (* prelude's directory prefix *)
  w_root_item : option (string * name);              (* platform library and context *)
  (* The context and needs each fn, method or functor symbol forms.
     Merge classes describe type equality ([D43]), and the stored section
     supplies its spelling during migration. Every rule reading a symbol's
     context uses this one, so call sites, providers and bodies agree on
     canonical types and provision keys. *)
  w_context : nat -> option (cx * list item)
}.

Definition kind_sym (W : world) (s : nat) : option dkind :=
  match w_decl W s with Some d => kind_of (d_ast d) | None => None end.

Definition ast_of (W : world) (s : nat) : option adecl := option_map d_ast (w_decl W s).
(* Bare tags use the path form of construction and have no parameters. *)
Definition BareTag (W : world) (s : nat) : Prop :=
  exists n, ast_of W s = Some (ADBareTag n).

Definition mod_of (W : world) (s : nat) : string :=
  match w_decl W s with Some d => d_mod d | None => "" end.

(* [D44]/[D61]/[D64]: what a module exports, and what its scope holds.
   A module exports its own declarations and what it imported by `use`
   (with the use's local name), not what a glob import brought in.  Its
   ordinary scope holds what it exports and everything its explicit glob
   imports export. The implicit prelude fills only unbound names. *)
Inductive Exports (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ExOwn : forall p s d ns n,
    w_decl W s = Some d -> d_mod d = p -> filed_name (d_ast d) = Some (ns, n) ->
    Exports b W p ns n (ESym s)
| ExUse : forall p m t imp u e ns,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_glob imp = false ->
    In u (i_uses imp) ->
    ns = (if u_dot u then NSDetached else NSName) ->
    (* a plain name is the symbol t exports under it, else the module alias
       (as path resolution reads it): note 25 *)
    (Exports b W t ns (u_name u) e \/
     (u_dot u = false /\ w_exp W t NSName (u_name u) = None /\ Exports b W t NSAlias (u_name u) e)) ->
    Exports b W p (match e with EMod _ => NSAlias | _ => ns end)
            (match u_alias u with Some (_, a) => a | None => u_name u end) e
| ExAlias : forall p m t imp a,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_alias imp = Some a ->
    Exports b W p NSAlias a (EMod t).

Inductive OrdinaryScope (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ScExported : forall p ns n e, Exports b W p ns n e -> OrdinaryScope b W p ns n e
| ScGlob : forall p m t imp ns n e,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_glob imp = true ->
    Exports b W t ns n e -> OrdinaryScope b W p ns n e.

(* Shadowing points from this module toward the implicit outer scope:
   a local declaration or explicit import hides the matching prelude binding.
   The prelude is visible only when this module has no ordinary binding in
   that namespace. Other names and other modules retain their defaults.
   Ordinary bindings have equal priority: their conflicts are errors under
   WorldOf, not a choice made by import order. Prelude defaults are not exports. *)
Inductive InScope (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ScPrimary : forall p ns n e,
    OrdinaryScope b W p ns n e -> InScope b W p ns n e
| ScPrelude : forall p m ns n e,
    module_of b p = Some m -> am_prelude m = true ->
    Exports b W (b_prelude b) ns n e ->
    (forall e', ~ OrdinaryScope b W p ns n e') -> InScope b W p ns n e.

(* Path resolution: the first segment in scope, the rest through exports. *)
Fixpoint resolve_rest (W : world) (e : ent) (segs : list name) : option ent :=
  match segs with
  | [] => Some e
  | seg :: r =>
      match e with
      | ESym _ => None
      | EMod t =>
          match w_exp W t NSName seg with
          | Some e' => resolve_rest W e' r
          | None => match w_exp W t NSAlias seg with Some e' => resolve_rest W e' r | None => None end
          end
      end
  end.

Definition resolve (W : world) (p : string) (pa : path) : option ent :=
  match pa with
  | [] => None
  | hd :: rest =>
      match w_scope W p NSName hd with
      | Some e => resolve_rest W e rest
      | None => match w_scope W p NSAlias hd with Some e => resolve_rest W e rest | None => None end
      end
  end.

Definition resolve_sym (W : world) (p : string) (pa : path) : option nat :=
  match resolve W p pa with Some (ESym s) => Some s | _ => None end.

(* A detached method name: `.m` in scope, or `mod::m` ([D61]). *)
Definition resolve_detached (W : world) (p : string) (pa : path) : option nat :=
  match rev pa with
  | [] => None
  | [n] => match w_scope W p NSDetached n with Some (ESym s) => Some s | _ => None end
  | n :: rpre => match resolve W p (rev rpre) with
                 | Some (EMod t) => match w_exp W t NSDetached n with Some (ESym s) => Some s | _ => None end
                 | _ => None
                 end
  end.

(* [D61]: an attached method `R.m` is declared in R's own file. *)
Definition AttachedDecl (W : world) (r : nat) (n : name) (s : nat) : Prop :=
  exists d rn ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn (RAttached rn) n ps rt body /\
    d_mod d = mod_of W r /\ w_scope W (d_mod d) NSName rn = Some (ESym r).

(* The attached method of receiver r named n, as the world files it. *)
Definition Attached (W : world) (r : nat) (n : name) (s : nat) : Prop := w_attached W r n = Some s.

(* [D71]/[D73]: a nominal's requirements are its *relevant* parameters: the
   ones it lists, and the type symbols its payload mentions, closed through
   the nominals it mentions (less what brackets bind there).  `FreeIn R`
   reads a type expression's free type symbols against a candidate R. *)
Inductive FreeIn (W : world) (R : nat -> list nat) (p : string) : aty -> nat -> Prop :=
| FRec : forall fs n t x, In (n, t) fs -> FreeIn W R p t x -> FreeIn W R p (ATRec fs) x
| FUnion : forall ms t x, In t ms -> FreeIn W R p t x -> FreeIn W R p (ATUnion ms) x
| FType : forall pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    FreeIn W R p (ATRef pa a) t
| FNom : forall pa a g x,
    resolve_sym W p pa = Some g -> kind_sym W g <> Some DKType -> In x (R g) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some x) ->
    FreeIn W R p (ATRef pa a) x
| FBracket : forall pa a bpath sp d sa x,
    (* the head resolves to a symbol (note 31) *)
    resolve_sym W p pa <> None ->
    In (bpath, Spec (Some sp) d sa) (match a with Some bs => bs | None => [] end) ->
    FreeIn W R p (ATRef sp sa) x -> FreeIn W R p (ATRef pa a) x.

(* [D20]/[D21]: the type symbols an assume nesting requires, through the
   contexts it names, with those its equations write ([D90]).  For a val,
   fn, method, context or functor these are its requirements. *)
Inductive SpecTypes (W : world) : string -> spec -> nat -> Prop :=
| STType : forall p pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType -> SpecTypes W p (Spec (Some pa) None a) t
| STCtx : forall p pa a c d n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    In sp (flat_map citem_specs its) -> SpecTypes W (d_mod d) sp t -> SpecTypes W p (Spec (Some pa) None a) t.

Definition AssumeTypes (W : world) (s : nat) (t : nat) : Prop :=
  exists d sp, w_decl W s = Some d /\ In sp (flat_map aitem_specs (d_assumes d)) /\
               SpecTypes W (d_mod d) sp t.

Definition nominal_body (d : adecl) : option (list name * aty) :=
  match d with ADTag _ ps _ t => Some (ps, t) | ADAlias _ t => Some ([], t) | _ => None end.

Definition has_nominal_body (W : world) (s : nat) : bool :=
  match ast_of W s with
  | Some a => match nominal_body a with Some _ => true | None => false end
  | None => false
  end.

Definition RelevanceClosed (W : world) (R : nat -> list nat) : Prop :=
  forall g d ps t, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) ->
    (forall n x, In n ps -> resolve_sym W (d_mod d) [n] = Some x -> In x (R g)) /\
    (forall x, FreeIn W R (d_mod d) t x -> In x (R g)).

(* The conditions on a reading W of b. *)
Record WorldOf (b : bundle) (W : world) : Prop := {
  (* imports are acyclic and every module imported is in the bundle *)
  wo_acyclic : exists rank : string -> nat, forall p m t imp,
      Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) ->
      module_of b t <> None /\ rank t < rank p;
  (* every symbol is a declaration of the bundle, and every declaration is
     exactly one symbol. Structural records have no nominal identity *)
  wo_decl_sound : forall s d, w_decl W s = Some d -> Declared b d /\ kind_of (d_ast d) <> None;
  wo_decl_complete : forall d, Declared b d -> kind_of (d_ast d) <> None ->
      exists s, w_decl W s = Some d /\ forall s', w_decl W s' = Some d -> s' = s;
  (* [D44]: scopes and exports are exactly what declarations and imports
     put there; since w_scope is a function, two symbols under one name in
     one scope make the program have no reading at all *)
  wo_exports : forall p ns n e, Loaded b p -> (w_exp W p ns n = Some e <-> Exports b W p ns n e);
  wo_scope : forall p ns n e, Loaded b p -> (w_scope W p ns n = Some e <-> InScope b W p ns n e);
  (* an attached method is filed under its receiver, a type of its own
     file, and the table holds exactly the declared ones ([D61]) *)
  wo_attached : forall r n s, w_attached W r n = Some s <-> AttachedDecl W r n s;
  (* requirements: of a val/fn/method/context/functor, its assume nesting's
     type symbols; of an abstract type or bare tag, none; of a declaration
     with a nominal body, the least relevance-closed sets ([D71]), with
     every listed name a type of its nesting
     ([D73]).  Lists have no repeats; their order is free, but fixed per
     symbol, which fixes the order of a nominal's arguments in `TNom`. *)
  wo_reqs_nodup : forall s, NoDup (w_reqs W s);
  wo_reqs_plain : forall s k, kind_sym W s = Some k ->
      k <> DKTag -> k <> DKAlias -> k <> DKType ->
      forall t, In t (w_reqs W s) <-> AssumeTypes W s t;
  wo_reqs_none : forall s, kind_sym W s = Some DKType \/ BareTag W s -> w_reqs W s = [];
  wo_reqs_closed : RelevanceClosed W (w_reqs W);
  wo_reqs_least : forall R, RelevanceClosed W R ->
      (* R agrees with the fixed requirements of declarations with no
         nominal body, including bare tags. *)
      (forall s, has_nominal_body W s = false -> R s = w_reqs W s) ->
      forall g, has_nominal_body W g = true -> incl (w_reqs W g) (R g);
  wo_listed : forall g d ps t n, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) -> In n ps ->
      exists x, resolve_sym W (d_mod d) [n] = Some x /\ AssumeTypes W g x;
  wo_library : w_library W = Loader.library_dir (b_prelude b)
}.

(* Library symbols the language itself refers to ([D45], [D68]): the
   symbol in scope as `n` in `<prelude directory>/<short>.moss`.
   Reexports are ordinary scope entries, including here (note 68). *)
Definition LibraryModule (W : world) (short p : string) : Prop :=
  match w_library W with
  | Some dir => p = String.append dir (String.append short ".moss")
  | None => False
  end.

Definition LangSym (W : world) (short : string) (ns : nspace) (n : name) (s : nat) : Prop :=
  exists d p, w_decl W s = Some d /\ LibraryModule W short p /\
              w_scope W p ns n = Some (ESym s).

(* The language items: the library items the language's own rules need
   (criteria section 8), each by its module's short name, namespace and
   name.  The rules outside Platform name library items only through this
   table, `operator_method` (the operators, [D68], [D85]) and the string
   literal's desugaring (Syntax.string_desugar, resolved in scope).
   - Bool, True, False ([D45]): a condition's type, and its two values.
   - zero, one, and num.moss's add, mul and div ([D68]): a numeric
     literal's arithmetic; float.moss's float: a fractional literal's. *)
Inductive lang_item := LIBool | LITrue | LIFalse | LIZero | LIOne | LIAdd | LIMul | LIDiv | LIFloat.
Definition lang_item_key (i : lang_item) : string * nspace * name :=
  match i with
  | LIBool => ("bool", NSName, "Bool") | LITrue => ("bool", NSName, "True")
  | LIFalse => ("bool", NSName, "False")
  | LIZero => ("int", NSName, "zero") | LIOne => ("int", NSName, "one")
  | LIAdd => ("num", NSDetached, "add") | LIMul => ("num", NSDetached, "mul")
  | LIDiv => ("num", NSDetached, "div")
  | LIFloat => ("float", NSName, "float")
  end.
Definition LangItem (W : world) (i : lang_item) (s : nat) : Prop :=
  let '(short, ns, n) := lang_item_key i in LangSym W short ns n s.

Definition BooleanTags (W : world) (bool_tag true_tag false_tag : nat) : Prop :=
  LangItem W LIBool bool_tag /\ LangItem W LITrue true_tag /\ LangItem W LIFalse false_tag.

(* A name the lexer cannot produce. Its binding is scoped inside the
   scrutinee, so its slot cannot shadow a local in either branch. *)
Definition conditional_payload_name : name := "%condition".

(* ======================================================================== *)
(* V.2 Types                                                                 *)
(* ======================================================================== *)

(* `TyOf W p tm this t τ`: the type expression t, written in module p,
   denotes τ, where `tm` gives each type symbol in force its meaning here
   ([D72]: a type symbol's meaning is read where the type is written;
   rebinding it later does not change τ) and `this` the receiver type
   inside a method declaration.  A nominal reference is total: each of its
   requirements is bound in brackets or in force ([D22] as clarified). *)
Inductive TyOf (W : world) : string -> list (nat * ty) -> option ty -> aty -> ty -> Prop :=
| TyNever : forall p tm th, TyOf W p tm th ATNever never
| TyUnit  : forall p tm th, TyOf W p tm th ATUnit TUnit
| TyThis  : forall p tm t, TyOf W p tm (Some t) ATThis t
| TyRec   : forall p tm th fs ts,
    NoDup (map fst fs) ->
    Forall2 (fun nt ft => fst ft = fst nt /\ TyOf W p tm th (snd nt) (snd ft)) fs ts ->
    TyOf W p tm th (ATRec fs) (TRecord ts)
| TyUnion : forall p tm th ms ts flat,
    Forall2 (TyOf W p tm th) ms ts ->
    flat = flat_map members ts ->
    (* Union members have nominal identities, with one payload type per head. *)
    Forall (fun m => exists n xs, m = TNom n xs) flat -> NoDup (map head flat) ->
    TyOf W p tm th (ATUnion ms) (TUnion flat)
| TyType  : forall p tm th pa t tau,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType -> nlook t tm = Some tau ->
    TyOf W p tm th (ATRef pa None) tau
| TyNom   : forall p tm th pa a g bs args,
    resolve_sym W p pa = Some g -> kind_sym W g = Some DKTag ->
    Brackets W p tm g a bs ->
    (* the arguments: each requirement, in its fixed order, as bound or in force *)
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ tm) = Some (snd ra)) (w_reqs W g) args ->
    TyOf W p tm th (ATRef pa a) (TNom g args)
| TyAlias : forall p tm th pa a g d n body bs tau,
    resolve_sym W p pa = Some g -> w_decl W g = Some d -> d_ast d = ADAlias n body ->
    Brackets W p tm g a bs ->
    TyOf W (d_mod d) (bs ++ tm) None body tau ->
    TyOf W p tm th (ATRef pa a) tau
(* `Brackets W p tm g a bs`: the bracket application `a` on g binds each
   left side (a type symbol among g's requirements, [D71]; on a method,
   any type symbol: note 56) to its right side read here; later bindings
   override earlier ones. *)
with Brackets (W : world) : string -> list (nat * ty) -> nat -> option (list (path * spec)) -> list (nat * ty) -> Prop :=
| BrNone : forall p tm g, Brackets W p tm g None []
| BrSome : forall p tm g bs l,
    Forall2 (fun b tb =>
               exists sp sa, snd b = Spec (Some sp) None sa /\
                 resolve_sym W p (fst b) = Some (fst tb) /\ kind_sym W (fst tb) = Some DKType /\
                 (kind_sym W g <> Some DKMeth -> In (fst tb) (w_reqs W g)) /\
                 TyOf W p tm None (ATRef sp sa) (snd tb)) bs l ->
    Brackets W p tm g (Some bs) (rev l).

(* ======================================================================== *)
(* V.3 Contexts in force, and their formation with merging                  *)
(* ======================================================================== *)

(* The context types lmut, provision and cx are declared before the world
   (V.1), which records each symbol's context. *)
Definition c_vals (C : cx) : list (nat * ty) :=
  flat_map (fun q => match q with
    | (IVal v, ProvidedVal t _) => [(v,t)] | _ => [] end) (c_provisions C).
Definition c_fns (C : cx) : list (nat * fsig) :=
  flat_map (fun q => match q with
    | (IFn f, ProvidedFn g _) => [(f,g)] | _ => [] end) (c_provisions C).
Definition c_mths (C : cx) : list ((ty * nat) * fsig) :=
  flat_map (fun q => match q with
    | (IMth r m _, ProvidedMethod g _) => [((r,m),g)] | _ => [] end) (c_provisions C).
Definition c_funs (C : cx) : list (nat * subst) :=
  flat_map (fun q => match q with
    | (IFun u, ProvidedFunctor fx) => [(u,fx)] | _ => [] end) (c_provisions C).
Definition c_apps (C : cx) : list (item * inst) :=
  map (fun q => (fst q, match snd q with
    | ProvidedVal _ a | ProvidedFn _ a | ProvidedMethod _ a => a
    | ProvidedFunctor fx => map (fun p => (fst p, Some (snd p))) fx end)) (c_provisions C).

Fixpoint mlook (r : ty) (m : nat) (l : list ((ty * nat) * fsig)) : option fsig :=
  match l with
  | [] => None
  | ((r', m'), g) :: rest => if ty_eqb r r' && Nat.eqb m m' then Some g else mlook r m rest
  end.

Definition canon (C : cx) (t : ty) : ty := app_subst (c_sub C) t.
(* A key, and a provision, read through C's merge.  Two contexts choose
   their merges' representatives independently, so a need one context
   spells `X.m` another may spell `Y.m`; read through F's merge, both are
   F's one need (criteria.md section 2, coherence). *)
Definition canon_item (C : cx) (k : item) : item := app_subst_item (c_sub C) k.

(* A provision identity omits the captured instantiation. Runtime references
   retain that annotation so a callee's requirements can name its own interface. *)
Definition mkey (r : ty) (m : nat) : item := IMth r m [].

(* Note 28: a method provision is filed (c_mths, c_apps) under its
   receiver and method, mkey; its run-time key carries θ, the
   instantiation it was made at (its known entries, canonical). *)
Definition known (a : inst) : subst :=
  flat_map (fun q => match snd q with Some t => [(fst q, t)] | None => [] end) a.
Definition theta_of (C : cx) (a : inst) : subst := map (fun q => (fst q, canon C (snd q))) (known a).
Definition rkey (C : cx) (r : ty) (m : nat) : item :=
  IMth r m (match ilook (mkey r m) (c_apps C) with Some a => theta_of C a | None => [] end).
Definition rkey_of (C : cx) (k : item) : item :=
  match k with IMth r m _ => rkey C r m | _ => k end.

(* A provision has one identity; method type arguments are read separately. *)
Definition provision_id (it : item) : item :=
  match it with IMth r m _ => IMth r m [] | _ => it end.

(* The least congruence generated by the class equations. The template
   rule permits substitution under every type constructor. *)
Inductive MergeEq (M : merge) : ty -> ty -> Prop :=
| MERefl : forall t, MergeEq M t t
| MESym : forall t u, MergeEq M t u -> MergeEq M u t
| METrans : forall t u v, MergeEq M t u -> MergeEq M u v -> MergeEq M t v
| MEClass : forall c x y, In c M -> In x (mc_members c) -> In y (mc_members c) ->
    MergeEq M (TAbs x) (TAbs y)
| MEShape : forall c x t, In c M -> In x (mc_members c) -> mc_shape c = Some t ->
    MergeEq M (TAbs x) t
| MECong : forall t s r,
    (forall x, In x (gens t) -> MergeEq M (app_subst s (TAbs x)) (app_subst r (TAbs x))) ->
    MergeEq M (app_subst s t) (app_subst r t).

Definition Solved (M : merge) : Prop :=
  NoDup (flat_map mc_members M) /\
  (forall c t, In c M -> mc_shape c = Some t -> forall x, t <> TAbs x) /\
  exists rank : tsym -> nat,
    (forall c x y, In c M -> In x (mc_members c) -> In y (mc_members c) -> rank x = rank y) /\
    (forall c t x y, In c M -> mc_shape c = Some t ->
       In x (mc_members c) -> In y (gens t) -> rank y < rank x).

Definition merge_symbols (M : merge) : list tsym :=
  flat_map (fun c => mc_members c ++
    match mc_shape c with Some t => gens t | None => [] end) M.

(* Expand the structured classes simultaneously. Along any dependency
   path, a solved merge visits each class at most once. *)
Fixpoint shape_table (M : merge) (fuel : nat) : subst :=
  match fuel with
  | 0 => []
  | S n =>
      let rest := shape_table M n in
      flat_map (fun c => match mc_shape c with
        | Some t => map (fun x => (x, app_subst rest t)) (mc_members c)
        | None => []
        end) M
  end.

Definition expand (M : merge) (t : ty) : ty :=
  app_subst (shape_table M (length M)) t.

Definition same_classb (M : merge) (x y : tsym) : bool :=
  Nat.eqb x y ||
  existsb (fun c => existsb (Nat.eqb x) (mc_members c) &&
                     existsb (Nat.eqb y) (mc_members c)) M.

Fixpoint ty_eqb_with (eq_abs : tsym -> tsym -> bool) (a b : ty) {struct a} : bool :=
  match a, b with
  | TRigid a, TRigid b => Nat.eqb a b
  | TRecord fs, TRecord gs =>
      (fix go (fs gs : list (string * ty)) :=
        match fs, gs with
        | [], [] => true
        | (f,t)::fs, (g,u)::gs => String.eqb f g && ty_eqb_with eq_abs t u && go fs gs
        | _, _ => false end) fs gs
  | TUnit, TUnit => true
  | TAbs t1, TAbs t2 => eq_abs t1 t2
  | TNom n1 th1, TNom n2 th2 =>
      Nat.eqb n1 n2 &&
      ((fix go (l1 l2 : list (tsym * ty)) : bool :=
          match l1, l2 with
          | [], [] => true
          | p1 :: r1, p2 :: r2 =>
              Nat.eqb (fst p1) (fst p2) && ty_eqb_with eq_abs (snd p1) (snd p2) && go r1 r2
          | _, _ => false
          end) th1 th2)
  | TUnion m1, TUnion m2 =>
      (fix go (l1 l2 : list ty) : bool :=
         match l1, l2 with
         | [], [] => true
         | t1 :: r1, t2 :: r2 => ty_eqb_with eq_abs t1 t2 && go r1 r2
         | _, _ => false
         end) m1 m2
  | _, _ => false
  end.

Definition merge_eqb (M : merge) (t u : ty) : bool :=
  ty_eqb_with (same_classb M) (expand M t) (expand M u).

(* Keep the outer constructor and its labels, with children erased. *)
Definition type_outer (t : ty) : ty :=
  match t with
  | TRigid m => TRigid m
  | TRecord fs => TRecord (map (fun p => (fst p, TUnit)) fs)
  | TUnit => TUnit
  | TAbs _ => TAbs 0
  | TNom n args => TNom n (map (fun p => (fst p, TUnit)) args)
  | TUnion ms => TUnion (map (fun _ => TUnit) ms)
  end.

Definition type_children (t : ty) : list ty :=
  match t with
  | TRecord fs => map snd fs
  | TNom _ args => map snd args
  | TUnion ms => ms
  | _ => []
  end.

Definition HeadM (M : merge) (t : ty) : Prop := head (expand M t) <> None.

Definition FitsM (M : merge) (t expected : ty) : Prop :=
  MergeEq M t expected \/ MergeEq M t never \/
  exists ms, expand M expected = TUnion ms /\
    forall x, In x (members (expand M t)) -> exists y, In y ms /\ MergeEq M x y.

Inductive JoinM (M : merge) : ty -> ty -> option ty -> ty -> Prop :=
| JoinMExp : forall a b t, JoinM M a b (Some t) t
| JoinMLeftNever : forall a b, MergeEq M a never -> JoinM M a b None b
| JoinMSame : forall a b, (MergeEq M b never \/ MergeEq M a b) ->
    ~ MergeEq M a never -> JoinM M a b None a.

Definition KeyEq (M : merge) (k k' : item) : Prop :=
  match k, k' with
  | IMth t m _, IMth u n _ => m = n /\ MergeEq M t u
  | _, _ => provision_id k = provision_id k'
  end.

Definition item_eqbM (M : merge) (k k' : item) : bool :=
  match k, k' with
  | IMth t m _, IMth u n _ => Nat.eqb m n && merge_eqb M t u
  | _, _ => item_eqb (provision_id k) (provision_id k')
  end.

(* Return the stored key along with its provision. *)
Fixpoint ilookM {A : Type} (M : merge) (k : item)
    (l : list (item * A)) : option (item * A) :=
  match l with
  | [] => None
  | (k', a) :: rest => if item_eqbM M k k' then Some (k', a) else ilookM M k rest
  end.

Fixpoint mlookM {A : Type} (M : merge) (r : ty) (m : msym)
    (l : list ((ty * msym) * A)) : option ((ty * msym) * A) :=
  match l with
  | [] => None
  | ((r', m'), a) :: rest =>
      if Nat.eqb m m' && merge_eqb M r r' then Some ((r', m'), a)
      else mlookM M r m rest
  end.

Definition SigEq (M : merge) (g g' : fsig) : Prop :=
  Forall2 (MergeEq M) (sg_params g) (sg_params g') /\ MergeEq M (sg_ret g) (sg_ret g').

Definition InstEq (M : merge) (a a' : subst) : Prop :=
  Forall2 (fun q r => fst q = fst r /\ MergeEq M (snd q) (snd r)) a a'.

Definition read_inst (a : inst) : subst :=
  map (fun q => (fst q, match snd q with Some t => t | None => TAbs (fst q) end)) a.

Definition ProvEq (M : merge) (p p' : provision) : Prop :=
  match p, p' with
  | ProvidedVal t a, ProvidedVal u b => MergeEq M t u /\ InstEq M (read_inst a) (read_inst b)
  | ProvidedFn g a, ProvidedFn h b | ProvidedMethod g a, ProvidedMethod h b =>
      SigEq M g h /\ InstEq M (read_inst a) (read_inst b)
  | ProvidedFunctor a, ProvidedFunctor b => InstEq M a b
  | _, _ => False
  end.

(* A reading satisfies each equation in its source merge at its target
   merge. A section chooses one spelling for each class during migration. *)
Definition Respects (M : merge) (s : subst) (N : merge) : Prop :=
  (forall c x y, In c M -> In x (mc_members c) -> In y (mc_members c) ->
     MergeEq N (app_subst s (TAbs x)) (app_subst s (TAbs y))) /\
  (forall c x t, In c M -> In x (mc_members c) -> mc_shape c = Some t ->
     MergeEq N (app_subst s (TAbs x)) (app_subst s t)).

Definition SectionOf (M : merge) (s : subst) : Prop :=
  (forall x, MergeEq M (TAbs x) (app_subst s (TAbs x))) /\
  (forall t u, MergeEq M t u -> app_subst s t = app_subst s u).

Definition provides (C : cx) (i : item) : Prop :=
  match i with
  | IVal v => nlook v (c_vals C) <> None
  | IFn f => nlook f (c_fns C) <> None
  | IMth r m _ => mlook r m (c_mths C) <> None
  | IFun u => nlook u (c_funs C) <> None
  | ITy t => nlook t (c_ty C) <> None
  end.

Definition providesM (M : merge) (C : cx) (i : item) : Prop :=
  match i with
  | IMth r m _ => mlookM M r m (c_mths C) <> None
  | _ => provides C i
  end.

Definition provision_inst (p : provision) : inst :=
  match p with
  | ProvidedVal _ a | ProvidedFn _ a | ProvidedMethod _ a => a
  | ProvidedFunctor fx => map (fun q => (fst q, Some (snd q))) fx end.
Definition provision_subst (sigma : subst) (p : provision) : provision :=
  let inst_sub a := map (fun q => (fst q, option_map (app_subst sigma) (snd q))) a in
  match p with
  | ProvidedVal t a => ProvidedVal (app_subst sigma t) (inst_sub a)
  | ProvidedFn g a => ProvidedFn (sig_subst sigma g) (inst_sub a)
  | ProvidedMethod g a => ProvidedMethod (sig_subst sigma g) (inst_sub a)
  | ProvidedFunctor fx => ProvidedFunctor (map (fun q => (fst q, app_subst sigma (snd q))) fx)
  end.
Definition canon_provision (C : cx) (p : provision) : provision := provision_subst (c_sub C) p.
Definition provision_signature (p : provision) : option fsig :=
  match p with
  | ProvidedVal t _ => Some (FSig [] t)
  | ProvidedFn g _ | ProvidedMethod g _ => Some g
  | ProvidedFunctor _ => None end.
Definition provision_refines (promised actual : provision) : Prop :=
  (match promised, actual with
   | ProvidedVal _ _, ProvidedVal _ _ | ProvidedFn _ _, ProvidedFn _ _
   | ProvidedMethod _ _, ProvidedMethod _ _ | ProvidedFunctor _, ProvidedFunctor _ => True
   | _, _ => False end) /\
  provision_signature promised = provision_signature actual /\
  forall t value, In (t, Some value) (provision_inst promised) ->
    In (t, Some value) (provision_inst actual).
Definition provision_gens (p : provision) : list nat :=
  (match provision_signature p with
   | Some g => flat_map gens (sg_ret g :: sg_params g) | None => [] end) ++
  flat_map (fun q => match snd q with Some t => gens t | None => [] end) (provision_inst p).

(* A raw context entry, as an item of a telescope introduces it. *)
Inductive entry :=
| NTy  (t : nat)
| NVal (v : nat) (t : ty) (a : inst)
| NFn  (f : nat) (g : fsig) (a : inst)
| NMth (r : ty) (m : nat) (g : fsig) (a : inst)
| NFun (u : nat) (fx : subst)
(* an equation item's two sides, read ([D90]): no key, only a merge *)
| NEq  (l r : ty).

(* `SigOf W s tm this g`: the declared signature of fn/method s read with
   the type meanings tm (and receiver type `this`). *)
Definition SigOf (W : world) (s : nat) (tm : list (nat * ty)) (this : option ty) (g : fsig) : Prop :=
  exists d rcv n ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    Forall2 (fun pt t => TyOf W (d_mod d) tm this (snd pt) t) ps (sg_params g) /\
    match rt with Some r => TyOf W (d_mod d) tm this r (sg_ret g) | None => sg_ret g = TUnit end.

(* A type symbol t that the signature of fn or method s (a parameter's or
   the result's type) mentions (note 51). *)
Definition SigMentions (W : world) (s : nat) (t : nat) : Prop :=
  exists d rcv n ps rt body, w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    ((exists pt, In pt ps /\ FreeIn W (w_reqs W) (d_mod d) (snd pt) t) \/
     (exists r, rt = Some r /\ FreeIn W (w_reqs W) (d_mod d) r t)).

(* The instantiation a provision of s is made at: each requirement's meaning
   (for a method, exactly the requirements its signature mentions, in
   their fixed order: note 51). *)
Inductive Select {A : Type} (P : A -> Prop) : list A -> list A -> Prop :=
| SelectNil : Select P [] []
| SelectKeep : forall x xs selected, P x -> Select P xs selected ->
    Select P (x :: xs) (x :: selected)
| SelectDrop : forall x xs selected, ~ P x -> Select P xs selected ->
    Select P (x :: xs) selected.
(* [D20], sigclose: a method's signature is closed over its requirements,
   which include every type symbol it mentions, so θ records each. The
   mentioned symbols outside its assume nesting (w_reqs) follow the
   nesting's, ascending: a list fixed by its members. *)
Fixpoint Ascending (l : list nat) : Prop :=
  match l with
  | x :: ((y :: _) as r) => x < y /\ Ascending r
  | _ => True
  end.
Definition SigOutside (W : world) (s : nat) (extra : list nat) : Prop :=
  Ascending extra /\ forall t, In t extra <-> SigMentions W s t /\ ~ In t (w_reqs W s).
Definition AppOf (W : world) (s : nat) (tm : subst) (a : inst) : Prop :=
  exists requirements,
    a = map (fun r => (r, nlook r tm)) requirements /\
    ((kind_sym W s = Some DKMeth /\ exists selected extra,
        Select (SigMentions W s) (w_reqs W s) selected /\ SigOutside W s extra /\
        requirements = selected ++ extra) \/
     (kind_sym W s <> Some DKMeth /\ requirements = w_reqs W s)).

(* The receiver type of a method item `R.m`: R's meaning if R is a type
   symbol, else R as a nominal at its requirements' meanings.  A type
   symbol not in force has no meaning: it is not read as a nominal `TNom R
   []` ([D20]: a type an item mentions must already be in force; the
   oracle rejects `assume T.m, T`, tests/errors/assume_method_before_type.moss). *)
Definition RecvTy (W : world) (r : nat) (tm : list (nat * ty)) (t : ty) : Prop :=
  match nlook r tm with
  | Some t' => t = t'
  | None =>
      match w_decl W r with
      | Some d =>
          match d_ast d with
          | ADAlias _ body => TyOf W (d_mod d) tm None body t
          | _ => kind_sym W r <> Some DKType /\ exists args, t = TNom r args /\
                   Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) (w_reqs W r) args
          end
      | None => exists args, t = TNom r args /\
                  Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) (w_reqs W r) args
      end
  end.

(* The method a name `.n` on receiver symbol r means in module p's items:
   the attached one, else the detached one in scope ([D61]). *)
Definition MethodNamed (W : world) (p : string) (r : nat) (n : name) (m : nat) : Prop :=
  Attached W r n m \/
  ((forall m', ~ Attached W r n m') /\ w_scope W p NSDetached n = Some (ESym m)).

(* The fixed inputs of a functor ([D84] rule 1), here because assuming an
   abstract functor asks them in force (ItemEntries, note 70). *)

Definition functor_parts (d : adecl) : option (option (list name) * list spec * list spec * option (list (list (spec * option aexpr)))) :=
  match d with ADFunctor _ ps a r b => Some (ps, a, r, b) | _ => None end.

(* The type outputs of F: the type symbols its result lists (through contexts). *)
Definition FunOutputs (W : world) (u : nat) (t : nat) : Prop :=
  exists d ps a r b sp, w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    In sp r /\ SpecTypes W (d_mod d) sp t.

(* M(F) ([D84] rule 1, with DECISIONS.md's correction): the type symbols the
   result items mention — a val's or fn's requirements (its dependencies,
   for a fn its whole telescope), a method's receiver, and every type
   symbol a fn's or method's signature mentions (sigclose, #12b), less
   what brackets bind, plus the brackets' right sides; closed through
   relevance (w_reqs); less F's outputs. *)
Inductive ResultMentions (W : world) : nat -> string -> spec -> nat -> Prop :=
| RMValFn : forall self p pa a s t,
    resolve_sym W p pa = Some s -> (kind_sym W s = Some DKVal \/ kind_sym W s = Some DKFn) ->
    (In t (w_reqs W s) \/ kind_sym W s = Some DKFn /\ SigMentions W s t) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t
| RMReceiver : forall self p pa dn a r t,
    resolve_sym W p pa = Some r ->
    (t = r /\ kind_sym W r = Some DKType \/
     kind_sym W r <> Some DKType /\ In t (w_reqs W r)) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) (Some dn) a) t
| RMMeth : forall self p pa dn a r m t,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    SigMentions W m t ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) (Some dn) a) t
| RMBracket : forall self p pa d a bp sp dr sa t,
    resolve_sym W p pa <> None ->
    In (bp, Spec (Some sp) dr sa) (match a with Some bs => bs | None => [] end) ->
    FreeIn W (w_reqs W) p (ATRef sp sa) t ->
    ResultMentions W self p (Spec (Some pa) d a) t
| RMCtx : forall self p pa a c dc n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some dc -> d_ast dc = ADCtx n its ->
    In (CISpec sp) its -> ResultMentions W self (d_mod dc) sp t ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t
| RMFunctor : forall self p pa a u du n names args res sp t,
    resolve_sym W p pa = Some u -> w_decl W u = Some du ->
    d_ast du = ADFunctor n (Some names) args res None -> u <> self ->
    In sp res -> ResultMentions W u (d_mod du) sp t ->
    ~ FunOutputs W u t ->
    ~ (exists name, In name names /\ resolve_sym W (d_mod du) [name] = Some t) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t.

Definition Mentions (W : world) (u : nat) (t : nat) : Prop :=
  exists d ps a r b sp, w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    In sp r /\ ResultMentions W u (d_mod d) sp t /\ ~ FunOutputs W u t.

(* P(F): the brackets; an unbracketed abstract functor has P(F) = M(F). *)
Definition Poly (W : world) (u : nat) (t : nat) : Prop :=
  exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None /\
    match ps with
    | None => Mentions W u t
    | Some names => exists x, In x names /\ resolve_sym W (d_mod d) [x] = Some t
    end.

(* X(F) = M(F) ∖ P(F), the fixed inputs ([D84] rule 1). *)
Definition Fixed (W : world) (u : nat) (t : nat) : Prop := Mentions W u t /\ ~ Poly W u t.

(* A functor provision retains its fixed inputs as read where provided. *)
Definition FixedReadings (W : world) (u : nat) (tm : list (nat * ty)) (fx : subst) : Prop :=
  forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x tm = Some tx.
Definition fixed_subst (sigma : subst) (fx : subst) : subst :=
  map (fun q => (fst q, app_subst sigma (snd q))) fx.
Definition fixed_app (fx : subst) : inst :=
  map (fun q => (fst q, Some (snd q))) fx.

(* Each of xs in force as its own generation, unless it is already. *)
Definition add_tys (xs : list nat) (tm : list (nat * ty)) : list (nat * ty) :=
  fold_left (fun tm x => match nlook x tm with Some _ => tm | None => (x, TAbs x) :: tm end) xs tm.

(* An equation introduces the abstract types it names. Its concrete
   references are read as types when the two sides are checked. *)
Definition equation_types (W : world) (p : string) (l r : aty) : list spec :=
  filter (fun sp =>
    match sp with
    | Spec (Some path) None None =>
        match resolve_sym W p path with
        | Some t => match kind_sym W t with Some DKType => true | _ => false end
        | None => false
        end
    | _ => false
    end) (ty_heads l ++ ty_heads r).

(* `ItemEntries W p ob tm sp es tm'`: the context item `sp`, written in
   module p, under the enclosing items' bracket bindings `ob`, introduces
   entries `es`; the type meanings in force grow from tm to tm' (each
   assumed type is its own generation, TAbs t).  An item is read at
   `ob ++ tm`: a bracket binds only while the item it is on is read, and a
   type item is put in force as its own generation even where a bracket
   binds it (note 71).  [D20]: a type the item mentions must already be in
   force or bound (TyOf fails otherwise). *)
Inductive ItemEntries (W : world) : string -> list (nat * ty) -> list (nat * ty) -> spec -> list entry -> list (nat * ty) -> Prop :=
| IEType : forall p ob tm pa t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [NTy t]
                (match nlook t tm with Some _ => tm | None => (t, TAbs t) :: tm end)
| IEVal : forall p ob tm a pa s d n t ty0 bs ap,
    resolve_sym W p pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t None ->
    Brackets W p (ob ++ tm) s a bs ->
    TyOf W (d_mod d) (bs ++ ob ++ tm) None t ty0 -> AppOf W s (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) None a) [NVal s ty0 ap] tm
| IEFn : forall p ob tm a pa s g bs ap,
    resolve_sym W p pa = Some s -> kind_sym W s = Some DKFn ->
    Brackets W p (ob ++ tm) s a bs ->
    SigOf W s (bs ++ ob ++ tm) None g -> AppOf W s (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) None a) [NFn s g ap] tm
| IEFun : forall p ob tm pa u d n ps args res fx,
    resolve_sym W p pa = Some u -> w_decl W u = Some d -> d_ast d = ADFunctor n ps args res None ->
    (* [D84] rule 2, [D20]: F is at an instantiation of X(F), so assuming
       it asks X(F) in force (note 70) *)
    (forall x, Fixed W u x -> nlook x tm <> None) ->
    FixedReadings W u tm fx ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [NFun u fx] tm
| IEMeth : forall p ob tm a pa dn r m rt g bs ap,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    Brackets W p (ob ++ tm) m a bs ->
    RecvTy W r (bs ++ ob ++ tm) rt ->
    SigOf W m (bs ++ ob ++ tm) (Some rt) g -> AppOf W m (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) (Some dn) a) [NMth rt m g ap] tm
| IECtx : forall p ob tm a pa c d n its bs es tm',
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    Brackets W p (ob ++ tm) c a bs ->
    (* [D61]: the items of a context are read in the module that wrote them *)
    ItemsEntries W (d_mod d) (bs ++ ob) tm its es tm' ->
    ItemEntries W p ob tm (Spec (Some pa) None a) es tm'
with ItemsEntries (W : world) : string -> list (nat * ty) -> list (nat * ty) -> list citem -> list entry -> list (nat * ty) -> Prop :=
| IsNil : forall p ob tm, ItemsEntries W p ob tm [] [] tm
| IsCons : forall p ob tm sp r es1 tm1 es2 tm2,
    ItemEntries W p ob tm sp es1 tm1 -> ItemsEntries W p ob tm1 r es2 tm2 ->
    ItemsEntries W p ob tm (CISpec sp :: r) (es1 ++ es2) tm2
(* [D90]: introduce the abstract types the equation names, read both
   sides, then add their equality as a constraint with no provision key.
   The sides' order does not matter (`PublicEquation`). *)
| IsEq : forall p ob tm l r es1 tm1 tl tr rest es2 tm2,
    ItemsEntries W p ob tm (map CISpec (equation_types W p l r)) es1 tm1 ->
    TyOf W p (ob ++ tm1) None l tl -> TyOf W p (ob ++ tm1) None r tr ->
    ItemsEntries W p ob tm1 rest es2 tm2 ->
    ItemsEntries W p ob tm (CIEq l r :: rest) (es1 ++ NEq tl tr :: es2) tm2.

(* An assume item `p` or `R.m` is the context item `p` or `R.m` unbracketed;
   an equation is itself. *)
Definition assume_spec (it : aitem) : citem :=
  match it with
  | AIHead h => CISpec (Spec (Some (fst h)) (snd h) None)
  | AIEq l r => CIEq l r
  end.

(* The key an entry is filed under, read under σ. *)
Definition entry_key (s : subst) (e : entry) : option item :=
  match e with
  | NTy _ => None
  | NVal v _ _ => Some (IVal v)
  | NFn f _ _ => Some (IFn f)
  | NMth r m _ _ => Some (mkey (app_subst s r) m)
  | NFun u _ => Some (IFun u)
  | NEq _ _ => None
  end.

Definition entry_sig (s : subst) (e : entry) : option fsig :=
  match e with
  | NVal _ t _ => Some (FSig [] (app_subst s t))
  | NFn _ g _ | NMth _ _ g _ => Some (sig_subst s g)
  | _ => None
  end.

(* An entry's instantiation, read under σ.  A requirement not in force
   where the item was read (None) is read as itself: the context's freeze
   completes it from the type table, where each assumed type is its own
   generation. *)
Definition inst_read (s : subst) (a : inst) : list (nat * ty) :=
  map (fun q => (fst q, app_subst s (match snd q with Some t => t | None => TAbs (fst q) end))) a.
Definition entry_inst (s : subst) (e : entry) : option (list (nat * ty)) :=
  match e with
  | NVal _ _ a | NFn _ _ a | NMth _ _ _ a => Some (inst_read s a)
  | _ => None
  end.

(* [D43] Merging.  σ *unifies* the entries if any two entries filed under
   one key (read under σ) have one signature and one instantiation under
   σ: merging two entries of one item also unifies their instantiations,
   so the merged key's instantiation does not depend on which entry comes
   first (criteria section 2, coherence; Δ4).  The merge of a
   telescope is a most general such σ that is idempotent (no cyclic merge:
   a finding of the mechanization, [D43] calculus note), mapping only
   generations of the telescope, each to one value (as the elaborator's
   union-find does; a shadowed second value would never be read, but
   would still sit in the context's substitution).  A merge identifying two distinct
   concrete heads has no unifier, so the telescope does not form. *)
Definition Unifies (s : subst) (es : list entry) : Prop :=
  (forall e1 e2 k, In e1 es -> In e2 es -> entry_key s e1 = Some k -> entry_key s e2 = Some k ->
    entry_sig s e1 = entry_sig s e2 /\ entry_inst s e1 = entry_inst s e2) /\
  (* [D90]: an equation's sides are one type under σ *)
  (forall l r, In (NEq l r) es -> app_subst s l = app_subst s r).

Definition inst_gens (a : inst) : list nat :=
  flat_map (fun q => match snd q with Some t => gens t | None => [] end) a.
Definition sig_gens (g : fsig) : list nat := flat_map gens (sg_ret g :: sg_params g).
Definition entry_gens (e : entry) : list nat :=
  match e with
  | NTy t => [t]
  | NVal _ t a => gens t ++ inst_gens a
  | NFn _ g a => sig_gens g ++ inst_gens a
  | NMth r _ g a => gens r ++ sig_gens g ++ inst_gens a
  | NFun _ fx => flat_map (fun q => gens (snd q)) fx
  | NEq l r => gens l ++ gens r
  end.
Definition MostGeneralMerge (s : subst) (es : list entry) : Prop :=
  Unifies s es /\
  (forall t, app_subst s (app_subst s t) = app_subst s t) /\
  (forall s', Unifies s' es -> (forall t, app_subst s' (app_subst s t) = app_subst s' t)) /\
  (* σ maps only generations of the telescope, to types over them *)
  (forall g t, In (g, t) s -> In g (flat_map entry_gens es) /\ incl (gens t) (flat_map entry_gens es)) /\
  (* σ is a function: no generation has a second, shadowed value *)
  (forall g t, In (g, t) s -> lookup_subst g s = Some t).

Definition raw_entry_key (e : entry) : option item :=
  match e with
  | NTy _ | NEq _ _ => None
  | NVal v _ _ => Some (IVal v)
  | NFn f _ _ => Some (IFn f)
  | NMth r m _ _ => Some (mkey r m)
  | NFun u _ => Some (IFun u)
  end.

Definition raw_entry_sig (e : entry) : option fsig :=
  match e with
  | NVal _ t _ => Some (FSig [] t)
  | NFn _ g _ | NMth _ _ g _ => Some g
  | _ => None
  end.

Definition raw_entry_inst (e : entry) : option subst :=
  match e with
  | NVal _ _ a | NFn _ _ a | NMth _ _ _ a => Some (read_inst a)
  | _ => None
  end.

Definition entry_types (e : entry) : list ty :=
  match e with
  | NTy x => [TAbs x]
  | NVal _ t a => t :: map snd (read_inst a)
  | NFn _ g a => sg_ret g :: sg_params g ++ map snd (read_inst a)
  | NMth r _ g a => r :: sg_ret g :: sg_params g ++ map snd (read_inst a)
  | NFun _ fx => map snd fx
  | NEq l r => [l; r]
  end.

Inductive TypeSubterm (u : ty) : ty -> Prop :=
| SubtermSelf : TypeSubterm u u
| SubtermRecord : forall fs f t, In (f, t) fs -> TypeSubterm u t -> TypeSubterm u (TRecord fs)
| SubtermNominal : forall n args x t, In (x, t) args -> TypeSubterm u t -> TypeSubterm u (TNom n args)
| SubtermUnion : forall ts t, In t ts -> TypeSubterm u t -> TypeSubterm u (TUnion ts).

Definition ShapesRead (es : list entry) (M : merge) : Prop :=
  forall c t, In c M -> mc_shape c = Some t ->
    exists e u, In e es /\ In u (entry_types e) /\ TypeSubterm t u.

Definition OptionEq {A : Type} (R : A -> A -> Prop) (a b : option A) : Prop :=
  match a, b with
  | Some x, Some y => R x y
  | None, None => True
  | _, _ => False
  end.

Definition UnifiesMerge (M : merge) (es : list entry) : Prop :=
  (forall e e' k k', In e es -> In e' es ->
     raw_entry_key e = Some k -> raw_entry_key e' = Some k' -> KeyEq M k k' ->
     OptionEq (SigEq M) (raw_entry_sig e) (raw_entry_sig e') /\
     OptionEq (InstEq M) (raw_entry_inst e) (raw_entry_inst e')) /\
  (forall l r, In (NEq l r) es -> MergeEq M l r).

Definition MergeOf (es : list entry) (M : merge) : Prop :=
  Solved M /\ ShapesRead es M /\ incl (merge_symbols M) (flat_map entry_gens es) /\
  UnifiesMerge M es /\
  forall s, Unifies s es -> forall t u, MergeEq M t u -> app_subst s t = app_subst s u.

(* The context a merged telescope makes: its tables are the entries read
   under σ.  Instantiations keep None for a requirement not in force where
   the item was written, completed from the context (the elaborator's
   "freeze"). *)
Definition entry_provision (s : subst) (freeze : inst -> inst) (e : entry)
    : list (item * provision) :=
  match e with
  | NTy _ => []
  | NVal v t a => [(IVal v, ProvidedVal (app_subst s t) (freeze a))]
  | NFn f g a => [(IFn f, ProvidedFn (sig_subst s g) (freeze a))]
  | NMth r m g a => [(mkey (app_subst s r) m, ProvidedMethod (sig_subst s g) (freeze a))]
  | NFun u fx => [(IFun u, ProvidedFunctor (fixed_subst s fx))]
  | NEq _ _ => []
  end.
Definition entries_cx (s : subst) (M : merge) (tm : subst) (es : list entry) : cx :=
  let freeze a := map (fun q => (fst q, option_map (app_subst s)
    (match snd q with Some t => Some t | None => nlook (fst q) tm end))) a in
  Cx (map (fun q => (fst q, app_subst s (snd q))) tm) s M
     (flat_map (entry_provision s freeze) es) [].

(* The needs of a telescope: its non-type keys, as a set (order is free). *)
Definition Needs (s : subst) (es : list entry) (ns : list item) : Prop :=
  NoDup ns /\ forall k, In k ns <-> exists e, In e es /\ entry_key s e = Some k.

(* [D84] (8), [D37] (note 54): `main` runs in the platform's root context.
   Beside what it assumes, any function in scope as `main` has the root
   context selected by the platform: each abstract functor it lists
   that is not already in force,
   after the types it is fixed in (X(F), [D84] rule 1), each in force as
   its own generation unless it is already. *)
Definition IsMainFn (W : world) (s : nat) : Prop :=
  exists d n ps rt body p, w_decl W s = Some d /\ d_ast d = ADFn RPlain n ps rt body /\
    w_scope W p NSName "main" = Some (ESym s).


(* `RootItems W q es0 tm its es tm'`: the root context's items `its` (written in module
   q), after the entries es0 and with meanings tm, add the entries es. *)
Inductive RootItems (W : world) (q : string) : list entry -> list (nat * ty) -> list citem -> list entry -> list (nat * ty) -> Prop :=
| RINil : forall es0 tm, RootItems W q es0 tm [] [] tm
| RIFun : forall es0 tm p a rest u du n ps args res xs fx es tm',
    resolve_sym W q p = Some u -> w_decl W u = Some du -> d_ast du = ADFunctor n ps args res None ->
    ~ (exists fx0, In (NFun u fx0) es0) -> NoDup xs -> (forall x, In x xs <-> Fixed W u x) ->
    FixedReadings W u (add_tys xs tm) fx ->
    RootItems W q (es0 ++ map NTy xs ++ [NFun u fx]) (add_tys xs tm) rest es tm' ->
    RootItems W q es0 tm (CISpec (Spec (Some p) None a) :: rest) (map NTy xs ++ [NFun u fx] ++ es) tm'
| RISkip : forall es0 tm sp rest es tm',
    match sp with
    | CISpec (Spec (Some p) None _) => forall u, resolve_sym W q p = Some u -> kind_sym W u = Some DKFunctor -> exists fx, In (NFun u fx) es0
    | _ => True
    end ->
    RootItems W q es0 tm rest es tm' -> RootItems W q es0 tm (sp :: rest) es tm'.

Definition RootContextItem (W : world) (symbol : nat) : Prop :=
  match w_root_item W with
  | Some (library, context) => LangSym W library NSName context symbol
  | None => False
  end.

Inductive RootCtx (W : world) (s : nat) (es0 : list entry) (tm : list (nat * ty)) : list entry -> list (nat * ty) -> Prop :=
| RootNone :
    (~ IsMainFn W s \/ forall root d n its, RootContextItem W root -> w_decl W root = Some d -> d_ast d <> ADCtx n its) ->
    RootCtx W s es0 tm [] tm
| RootContext : forall root d n its es tm',
    IsMainFn W s -> RootContextItem W root -> w_decl W root = Some d -> d_ast d = ADCtx n its ->
    RootItems W (d_mod d) es0 tm its es tm' -> RootCtx W s es0 tm es tm'.

(* `CtxOf W s C ns`: the context the assume nesting of symbol s forms (and,
   for `main`, the root context: note 54), and its needs (what a caller
   must provide, [D20]). *)
Definition CtxOf (W : world) (s : nat) (C : cx) (ns : list item) : Prop :=
  exists d es tm er tmr σ M,
    w_decl W s = Some d /\
    ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) es tm /\
    RootCtx W s es tm er tmr /\
    MostGeneralMerge σ (es ++ er) /\ MergeOf (es ++ er) M /\ SectionOf M σ /\
    C = entries_cx σ M tmr (es ++ er) /\ Needs σ (es ++ er) ns /\
    w_context W s = Some (C, ns).

(* ======================================================================== *)
(* V.4 Calls: needs, satisfaction maps and coherence                        *)
(* ======================================================================== *)

(* A need of the callee, read at the call site: a method need's receiver
   is re-read with the caller's type meanings ([D21]: an unapplied
   reference shares the symbols in force). *)
Definition at_site (C : cx) (k : item) : item :=
  match k with
  | IMth r m th => IMth (canon C (app_subst (map (fun q => (fst q, snd q)) (c_ty C)) r)) m th
  | _ => k
  end.

(* θ of a direct call of s from context C (note 27): each of s's
   requirements, in their fixed order (w_reqs), read at the site — its
   meaning in C, or itself where C gives it none — canonically. *)
Definition theta_at (W : world) (C : cx) (s : nat) : subst :=
  map (fun r => (r, canon C (app_subst (c_ty C) (TAbs r)))) (w_reqs W s).
(* A symbol's instantiation in its own context: each type in force there
   as it reads there (a wrapper calling the symbol it wraps). *)
Definition theta_ctx (C : cx) : subst :=
  map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty C).

Definition bound_in (C : cx) (r : nat) : bool :=
  match nlook r (c_ty C) with Some _ => true | None => false end.
Definition read_here (C : cx) (r : nat) : ty := canon C (app_subst (c_ty C) (TAbs r)).

(* the members of the merge class (in Cs) whose canonical reading is t *)
Definition class_of (Cs : cx) (t : ty) : list nat :=
  (match t with
   | TAbs z => match lookup_subst z (c_sub Cs) with Some _ => [] | None => [z] end
   | _ => []
   end) ++
  flat_map (fun q => if ty_eqb (snd q) t then [fst q] else []) (c_sub Cs).

(* a class representative z's value here: through its first member C binds *)
Definition class_value (C Cs : cx) (z : nat) : ty :=
  match find (bound_in C) (class_of Cs (TAbs z)) with
  | Some y => read_here C y
  | None => canon C (TAbs z)
  end.

Definition call_read (C Cs : cx) (read_class : nat -> ty) (r : nat) : ty :=
  if bound_in C r then read_here C r else
  match canon Cs (TAbs r) with
  | TAbs z => read_class z
  | t => canon C (app_subst (map (fun z => (z, read_class z)) (gens t)) t)
  end.

Definition call_type_arguments (C Cs : cx) (read_class : nat -> ty) : subst :=
  map (fun q => (fst q, call_read C Cs read_class (fst q))) (c_ty Cs).

(* The instantiation of a call of s (assume context Cs) from context C: a
   meaning for each of s's requirements, in their fixed order. Where C
   gives a requirement a meaning, θ reads it here (theta_at). Every merge
   of s's context ([D43]) must hold of θ, including for requirements C
   leaves unbound: merged types are one type, so a merged class bound
   here gives its unbound members that meaning too, and a caller cannot
   bind two members of one class differently.
   θ is principal (criteria, principality): every use of s also asks its
   requirements in force here (`RequirementsInForce`, rule B), so each
   requirement is bound here, or merged in Cs with a member bound here, or
   merged with a structured type whose symbols are bound here, and these
   clauses determine θ up to the merges here. *)
Definition CallTheta (W : world) (C Cs : cx) (s : nat) (th : subst) : Prop :=
  map fst th = map fst (c_ty Cs) /\
  (forall r, In r (map fst (c_ty Cs)) -> nlook r (c_ty C) <> None ->
     lookup_subst r th = Some (canon C (app_subst (c_ty C) (TAbs r)))) /\
  Respects (c_merge Cs) th (c_merge C).

(* Rule B: a requirement may be supplied through any member of its merge
   class. For a structured meaning, the same rule applies to each abstract
   generation remaining after the structured classes have been expanded. *)
Definition InForceClass (C : cx) (M : merge) (r : nat) : Prop :=
  exists y, MergeEq M (TAbs y) (TAbs r) /\ nlook y (c_ty C) <> None.

Definition InForceThrough (C Cs : cx) (r : nat) : Prop :=
  InForceClass C (c_merge Cs) r \/
  let t := expand (c_merge Cs) (TAbs r) in
  (forall z, t <> TAbs z) /\ forall y, In y (gens t) -> InForceClass C (c_merge Cs) y.
Definition RequirementsInForce (C Cs : cx) : Prop :=
  forall r, In r (map fst (c_ty Cs)) -> InForceThrough C Cs r.

(* A signature in Cs's terms, read here at instantiation th. *)
(* A need of the callee, read here at instantiation th. *)
Definition at_th (C : cx) (th : subst) (k : item) : item :=
  match k with
  | IMth r m a => IMth (canon C (app_subst th r)) m a
  | _ => k
  end.
Definition sig_at (C : cx) (th : subst) (g : fsig) : fsig :=
  FSig (map (fun t => canon C (app_subst th t)) (sg_params g)) (canon C (app_subst th (sg_ret g))).
Definition canon_sig (C : cx) (g : fsig) : fsig :=
  FSig (map (canon C) (sg_params g)) (canon C (sg_ret g)).

(* [D20]/[D62]/[D71]: a call of callee s (assume context Cs, needs ns)
   from context C at instantiation th needs the caller's provision of
   each requirement. The provision must be in force at the required
   instantiation, with the type or signature the callee's context gives
   it read at th. The callee's merges ([D43]) must hold here. A
   provision's type can mention types beyond its own requirements. *)
Definition NeedsSatisfied (C Cs : cx) (th : subst) (ns : list item) : Prop :=
  (forall k, In k ns -> provides C (at_th C th k)) /\
  (* coherence: where both say what a requirement means, they agree *)
  (forall k a, In k ns -> ilook k (c_apps Cs) = Some a ->
     forall t want, In (t, Some want) a ->
       match ilook (at_th C th k) (c_apps C) with
       | Some have => In (t, Some (canon C (app_subst th want))) have
       | None => True
       end) /\
  (* types: the provision selected here has the callee's view of it *)
  (forall k p, In k ns -> ilook k (c_provisions Cs) = Some p ->
     exists p', ilook (at_th C th k) (c_provisions C) = Some p' /\
       option_map (canon_sig C) (provision_signature p') = option_map (sig_at C th) (provision_signature p)) /\
  (* merges: every merge class of the callee whose members, and the
     classes of the symbols of its structure, are all in force here holds
     of the caller's meanings.  Stated by class (criteria section 2,
     coherence): the classes do not depend on the callee's choice of
     representatives (PublicOrientation.mgm_classes_iff), while the
     equations of its σ do.  A class only partly in force here is not
     checked under any representative (a closure's class with a linked
     member: BFunClosure; before, an equation of σ was skipped only when it
     mentioned a symbol not in force, so such a class was checked under a
     representative that is captured and not under one that is linked).
     At a call, CallTheta's merge clause already holds of every class. *)
  (forall g, (forall y, (exists x, (x = g \/ In x (gens (canon Cs (TAbs g)))) /\
                          canon Cs (TAbs y) = canon Cs (TAbs x)) -> nlook y (c_ty C) <> None) ->
     canon C (app_subst (c_ty C) (TAbs g)) = canon C (app_subst (c_ty C) (canon Cs (TAbs g)))).

(* Translation uses the same provision readings to pair each callee
   key with the caller's run-time key. *)
Definition satisfaction_map (C Cs : cx) (th : subst) (ns : list item) : smap :=
  map (fun k => (rkey_of Cs k, rkey_of C (at_th C th k))) ns.

Definition translate_call (C Cs : cx) (f : fsym) (th : subst)
    (needs : list item) (args : list expr) : expr :=
  ECall f th (satisfaction_map C Cs th needs) args.

Definition Satisfies (C Cs : cx) (th : subst) (ns : list item) (sm : smap) : Prop :=
  sm = satisfaction_map C Cs th ns /\ NeedsSatisfied C Cs th ns.

(* Arguments checked against parameter types ([D17] fits), elaborated in order. *)

(* ======================================================================== *)
(* V.5 Functors ([D55], [D78], [D84] with DECISIONS.md)                       *)
(* ======================================================================== *)

(* F's own terms: the context its argument telescope forms, plus its
   implicit mentioned inputs (symbols the explicit inputs do not list). *)
Definition FunCtx (W : world) (u : nat) (C : cx) (ns : list item) : Prop :=
  exists d ps a r b es tm tm' σ M implicit,
    w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    ItemsEntries W (d_mod d) [] [] (map CISpec a) es tm /\
    (* implicit inputs: only an abstract functor has them *)
    (forall t, In t implicit <-> (b = None /\ Mentions W u t /\ nlook t tm = None)) /\
    tm' = map (fun t => (t, TAbs t)) implicit ++ tm /\
    MostGeneralMerge σ es /\ MergeOf es M /\ SectionOf M σ /\
    C = entries_cx σ M tm' es /\ Needs σ es ns /\
    w_context W u = Some (C, ns).

(* F's result, provided in context C with each output type t read as
   `out t` (a fresh generation at an application): the context grows by
   the result's items, each at the instantiation it is made at, and `ks`
   are its non-type keys. *)
(* Output brackets are bindings at the point an output is installed.  Unlike
   an input telescope application, the output reader permits a binding that
   the target does not mention.  The right-hand types still use [TyOf]. *)
Inductive OutputBrackets (W : world) (p : string) (tm : list (nat * ty)) :
    option (list (path * spec)) -> list (nat * ty) -> Prop :=
| OBrNone : OutputBrackets W p tm None []
| OBrSome : forall bs l,
    Forall2 (fun b tb =>
      exists sp sa, snd b = Spec (Some sp) None sa /\
        resolve_sym W p (fst b) = Some (fst tb) /\
        kind_sym W (fst tb) = Some DKType /\
        TyOf W p tm None (ATRef sp sa) (snd tb)) bs l ->
    OutputBrackets W p tm (Some bs) (rev l).

(* Output installation follows the result list in order.  Type outputs have
   already received their generations; the item walk leaves their meanings
   alone.  A defined function or value is already available and adds no slot. *)
Inductive OutputItemEntries (W : world) : string -> list (nat * ty) ->
    list (nat * ty) -> spec -> list entry -> list (nat * ty) -> Prop :=
| OIEType : forall p ob tm pa a bs t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    OutputBrackets W p (ob ++ tm) a bs ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NTy t] tm
| OIENothing : forall p ob tm pa a bs s k,
    resolve_sym W p pa = Some s -> kind_sym W s = Some k ->
    OutputBrackets W p (ob ++ tm) a bs ->
    (k = DKTag \/ k = DKAlias \/
      (k = DKVal /\ exists d n t i, w_decl W s = Some d /\ d_ast d = ADVal n t (Some i)) \/
      (k = DKFn /\ exists d r n ps rt b, w_decl W s = Some d /\
         d_ast d = ADFn r n ps rt (Some b))) ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [] tm
| OIEVal : forall p ob tm a pa s d n t ty0 bs ap,
    resolve_sym W p pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    TyOf W (d_mod d) (bs ++ ob ++ tm) None t ty0 ->
    AppOf W s (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NVal s ty0 ap] tm
| OIEFn : forall p ob tm a pa s g bs ap d rcv n ps rt,
    resolve_sym W p pa = Some s -> kind_sym W s = Some DKFn ->
    w_decl W s = Some d -> d_ast d = ADFn rcv n ps rt None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    SigOf W s (bs ++ ob ++ tm) None g -> AppOf W s (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NFn s g ap] tm
| OIEFun : forall p ob tm a bs pa u d n ps args res fx,
    resolve_sym W p pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADFunctor n ps args res None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    (forall x, Fixed W u x -> nlook x (bs ++ ob ++ tm) <> None) ->
    FixedReadings W u (bs ++ ob ++ tm) fx ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NFun u fx] tm
| OIEMeth : forall p ob tm a pa dn r m rt g bs ap,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    OutputBrackets W p (ob ++ tm) a bs -> RecvTy W r (bs ++ ob ++ tm) rt ->
    SigOf W m (bs ++ ob ++ tm) (Some rt) g -> AppOf W m (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) (Some dn) a) [NMth rt m g ap] tm
(* a result's context holds no equation: an equation is a requirement
   ([D90]), and a result provides *)
| OIECtx : forall p ob tm a pa c d n its sps bs es tm',
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    its = map CISpec sps ->
    OutputBrackets W p (ob ++ tm) a bs ->
    OutputItemsEntries W (d_mod d) (bs ++ ob) tm sps es tm' ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) es tm'
with OutputItemsEntries (W : world) : string -> list (nat * ty) ->
    list (nat * ty) -> list spec -> list entry -> list (nat * ty) -> Prop :=
| OIsNil : forall p ob tm, OutputItemsEntries W p ob tm [] [] tm
| OIsCons : forall p ob tm sp rest es1 tm1 es2 tm2,
    OutputItemEntries W p ob tm sp es1 tm1 ->
    OutputItemsEntries W p ob tm1 rest es2 tm2 ->
    OutputItemsEntries W p ob tm (sp :: rest) (es1 ++ es2) tm2.

(* Result slots follow the written output occurrences.  Repeated output keys
   remain repeated slots, as in both evaluators; input [Needs] stays set-valued. *)
Definition ResultKeys (s : subst) (es : list entry) (ks : list item) : Prop :=
  ks = flat_map (fun e => match entry_key s e with Some k => [k] | None => [] end) es.

(* Replace the output roles before normalizing through the caller's merges.
   Output provisions and their recorded applications use these completed
   meanings, as [provided_at] freezes the corresponding source readings. *)
Definition output_type_map (C : cx) (outs : list nat) (out : nat -> ty) : list (nat * ty) :=
  map (fun q => (fst q, canon C (snd q))) (map (fun t => (t, out t)) outs ++ c_ty C).

Definition output_cx (C : cx) (tm : subst) (es : list entry) : cx :=
  Cx (map (fun q => (fst q, canon C (snd q))) tm) (c_sub C) (c_merge C)
    (flat_map (entry_provision (c_sub C) (fun a => a)) (rev es) ++ c_provisions C)
    (c_locals C).

Definition ProvideResult (W : world) (u : nat) (C : cx) (out : nat -> ty) (C' : cx) (ks : list item) : Prop :=
  exists d ps a r b outs es tm',
    w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    (forall t, In t outs <-> FunOutputs W u t) /\
    OutputItemsEntries W (d_mod d) [] (output_type_map C outs out) r es tm' /\
    ResultKeys (c_sub C) es ks /\
    C' = output_cx C tm' es.

(* [D84] rule 7: a family is an abstract functor whose result is method
   items only and holds no type, each item's key determining every P(F)
   symbol it mentions; its bindings bind no vals and apply no functor but
   a family (DECISIONS.md). *)
Inductive FamilyResult (W : world) : string -> spec -> Prop :=
| FRMethod : forall mi pa dn br,
    FamilyResult W mi (Spec (Some pa) (Some dn) br)
| FRContext : forall mi pa br u d n its sps,
    resolve_sym W mi pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADCtx n its -> its = map CISpec sps ->
    Forall (FamilyResult W (d_mod d)) sps ->
    FamilyResult W mi (Spec (Some pa) None br).

(* A family's listed polymorphic inputs are type symbols ([D84] rule 1; the
   oracle's functor_params rejects any other, tests/errors/family_poly_nontype.moss). *)
Definition FamilyShape (W : world) (u : nat) : Prop :=
  exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None /\
    Forall (FamilyResult W (d_mod d)) r /\
    (forall names x t, ps = Some names -> In x names -> resolve_sym W (d_mod d) [x] = Some t ->
       kind_sym W t = Some DKType).

Definition FamilyBracketBound (W : world) (p : string)
    (a : option (list (path * spec))) (t : nat) : Prop :=
  exists b, In b (match a with Some bs => bs | None => [] end) /\
    resolve_sym W p (fst b) = Some t.
Definition FamilyBracketFree (W : world) (p : string)
    (a : option (list (path * spec))) (t : nat) : Prop :=
  exists bp pa dn bs, In (bp, Spec (Some pa) dn bs)
    (match a with Some brackets => brackets | None => [] end) /\
    FreeIn W (w_reqs W) p (ATRef pa bs) t.

(* Every polymorphic signature/bracket dependency not replaced by the leaf's
   brackets is determined by its receiver key, as the actual D84 classifier
   checks.  A method-only abstract functor need not be a family. *)
Inductive FamilyKeyed (W : world) (u : nat) : string -> spec -> Prop :=
| FKMethod : forall p pa dn a r m,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    (forall t, Poly W u t ->
       (SigMentions W m t \/ FamilyBracketFree W p a t) ->
       ~ FamilyBracketBound W p a t ->
       FreeIn W (w_reqs W) p (ATRef pa None) t) ->
    FamilyKeyed W u p (Spec (Some pa) (Some dn) a)
| FKContext : forall p pa a c d n its sps,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    its = map CISpec sps ->
    Forall (FamilyKeyed W u (d_mod d)) sps ->
    FamilyKeyed W u p (Spec (Some pa) None a).

Definition FamilyKeys (W : world) (u : nat) : Prop :=
  forall d n ps a r, w_decl W u = Some d -> d_ast d = ADFunctor n ps a r None ->
    Forall (FamilyKeyed W u (d_mod d)) r.
Definition Family (W : world) (u : nat) : Prop :=
  FamilyShape W u /\ FamilyKeys W u.

(* ======================================================================== *)
(* V.6 Bodies: expressions, blocks, binds                                    *)
(* ======================================================================== *)

(* The frame of a body: its module, return type, receiver type (a method),
   whether a `break` has a loop, the numbering of local names (injective;
   parameters, after `this`, are numbered from 0 in order: the runtime
   passes arguments so), and the generations `bind F;` made in it so far
   (a value of their types may not leave the block that made them, [D78]). *)
Record frame := Frame {
  fr_mod : string; fr_ret : option ty; fr_this : option ty; fr_loop : bool;
  fr_var : list (name * nat); fr_fresh : list nat;
  fr_applying : list nat;   (* functors being applied by name: [D55] no cycle *)
  fr_pins : list nat        (* in a body bound to F: P(F) ∩ M(F), which it may not rebind ([D84] 3d) *)
}.

(* a local's number, for a name the numbering covers ([Named]) *)
Definition var_of (F : frame) (n : name) : nat :=
  match slook n (fr_var F) with Some k => k | None => 0 end.
Definition Named (F : frame) (n : name) : Prop := slook n (fr_var F) <> None.
(* the numbering is injective, and numbers the parameters 0, 1, … after `this` *)
Definition Numbering (F : frame) (this : option ty) (params : list name) : Prop :=
  NoDup (map fst (fr_var F)) /\ NoDup (map snd (fr_var F)) /\
  (* `this` is a keyword, never a numbered name: with a receiver, slot 0
     is the receiver alone, so no local can shadow it. *)
  (match this with Some _ => forall n, slook n (fr_var F) <> Some 0
                   | None => True end) /\
  (forall i pn, nth_error params i = Some pn ->
     slook pn (fr_var F) = Some (match this with Some _ => S i | None => i end)).

Definition with_loop (F : frame) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) true (fr_var F) (fr_fresh F) (fr_applying F) (fr_pins F).
Definition with_mod (F : frame) (p : string) (u : nat) : frame :=
  Frame p (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F) (u :: fr_applying F) (fr_pins F).
Definition with_fresh (F : frame) (gs : list nat) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F ++ gs) (fr_applying F) (fr_pins F).
(* A nested block's frame: [D84] 3d's pins constrain only a linked body's
   own binds, as a lambda body's frame is unconstrained. *)
Definition without_pins (F : frame) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F) (fr_applying F) [].

Definition add_local (C : cx) (n : name) (t : ty) (mu : lmut) : cx :=
  Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) ((n, (t, mu)) :: c_locals C).
Definition set_ty (C : cx) (t : nat) (tau : ty) : cx :=
  Cx ((t, tau) :: c_ty C) (c_sub C) (c_merge C) (c_provisions C) (c_locals C).
Definition set_provision (C : cx) (k : item) (p : provision) : cx :=
  Cx (c_ty C) (c_sub C) (c_merge C) ((k,p) :: filter (fun q => negb (item_eqb k (fst q))) (c_provisions C)) (c_locals C).
Definition provide (C : cx) (k : item) (a : inst) (C1 : cx) : cx :=
  match ilook k (c_provisions C1) with
  | Some (ProvidedVal t _) => set_provision C1 k (ProvidedVal t a)
  | Some (ProvidedFn g _) => set_provision C1 k (ProvidedFn g a)
  | Some (ProvidedMethod g _) => set_provision C1 k (ProvidedMethod g a)
  | _ => C1
  end.
Definition set_val (C : cx) (v : nat) (t : ty) : cx :=
  set_provision C (IVal v) (ProvidedVal t []).
Definition set_fn (C : cx) (f : nat) (g : fsig) : cx :=
  set_provision C (IFn f) (ProvidedFn g []).
Definition set_mth (C : cx) (r : ty) (m : nat) (g : fsig) : cx :=
  set_provision C (mkey r m) (ProvidedMethod g []).
Definition set_fun (C : cx) (u : nat) (fx : subst) : cx :=
  set_provision C (IFun u) (ProvidedFunctor fx).

Definition body_of (P : program) (f : nat) : option expr := nlook f (program_functions P).
Definition funbody_of (P : program) (h : nat) : option functor_code := nlook h (program_functors P).

(* A symbol elaboration makes (a lambda's provider, a forwarder, a wrapper,
   a captured local's val, a generation, a linked functor body) is not a
   source symbol. *)
Definition Fresh (W : world) (s : nat) : Prop := w_decl W s = None.

(* A generation is fresh for this scope when it is absent from the source
   world and from every type currently reachable in the static environment,
   including its merges: a merged-away key keeps its meaning (that of its
   representative) wherever the context is captured. *)
Definition context_gens (C : cx) : list nat :=
  flat_map (fun q => fst q :: gens (snd q)) (c_sub C) ++
  flat_map (fun q => gens (snd q) ++ gens (canon C (snd q))) (c_ty C) ++
  flat_map (fun q => provision_gens (snd q) ++
    provision_gens (provision_subst (c_sub C) (snd q))) (c_provisions C) ++
  flat_map (fun q => gens (fst (snd q)) ++ gens (canon C (fst (snd q)))) (c_locals C).
(* The generations a method key's receiver mentions (the fresh-output
   clause of BApplyAbstract, #10, names them). *)
Definition key_receiver_gens (C : cx) : list nat :=
  flat_map (fun q => match fst q with IMth r _ _ => gens r | _ => [] end) (c_provisions C).

Definition context_type_support (C : cx) : list nat :=
  map fst (c_ty C) ++ context_gens C ++ key_receiver_gens C.
(* A lambda or closure keeps the meanings of the generations its context
   mentions.  A method key's receiver mentions only those: rule B puts a
   callee's requirements in force at every use, so a family demand keys
   its methods at types in force (PublicFormation.KeysScoped). *)
Definition lexical_capture (C : cx) : type_capture :=
  TypeCapture
    (map (fun generation => (generation, TAbs generation)) (context_gens C))
    (map (fun q => (fst q, canon C (snd q))) (c_ty C)).
(* An allocation scope reserves identities from the surrounding namespace
   and its live support. Its allocation history grows by whole batches. *)
Record allocation_scope (identity : Type) := AllocationScope {
  scope_permits : identity -> Prop;
  scope_allocated : list identity;
  scope_support : list identity
}.

Definition AllocationFresh {identity} (before : allocation_scope identity)
    (batch : list identity) : Prop :=
  NoDup batch /\ Forall (fun g => scope_permits before g /\
    ~ In g (scope_allocated before ++ scope_support before)) batch.

Definition extend_allocation_scope {identity} (before : allocation_scope identity)
    (batch : list identity) : allocation_scope identity :=
  AllocationScope (scope_permits before) (scope_allocated before ++ batch) (scope_support before).

Definition Allocates {identity} (before : allocation_scope identity)
    (batch : list identity) (after : allocation_scope identity) : Prop :=
  AllocationFresh before batch /\ after = extend_allocation_scope before batch.

(* The existing frame and context supply the initial scope. The history is
   also the frame's record of generations that may not escape its block. *)
Definition body_allocation_scope (W : world) (F : frame) (C : cx) : allocation_scope nat :=
  AllocationScope (Fresh W) (fr_fresh F)
    (context_gens C ++
     (match fr_ret F with Some t => gens t | None => [] end) ++
     match fr_this F with Some t => gens t | None => [] end).

Definition FreshFor (W : world) (F : frame) (C : cx) (gs : list nat) : Prop :=
  AllocationFresh (body_allocation_scope W F C) gs.

Definition frame_after_allocation (F : frame) (after : allocation_scope nat) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F)
    (scope_allocated after) (fr_applying F) (fr_pins F).

(* Returning from a nested source body keeps its allocation history in
   the caller's scope. *)
Definition carry_frame_allocations (caller nested : frame) : frame :=
  Frame (fr_mod caller) (fr_ret caller) (fr_this caller) (fr_loop caller)
    (fr_var caller) (fr_fresh nested) (fr_applying caller) (fr_pins caller).

Definition value_provision_keys (entries : list (item * provision)) : list nat :=
  flat_map (fun q => match fst q with IVal v => [v] | _ => [] end) entries.

Definition capture_allocation_scope (W : world) (C : cx) : allocation_scope nat :=
  AllocationScope (Fresh W) [] (value_provision_keys (c_provisions C)).

(* The meaning of a decimal literal ([D68]): digits, an optional fraction
   and an optional exponent, as numerator / denominator. *)
Fixpoint digits_val (l : list ascii) (acc : nat) : nat :=
  match l with [] => acc | c :: r => digits_val r (10 * acc + (nat_of_ascii c - 48)) end.

Fixpoint split_at (c : ascii) (l : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match l with
  | [] => (rev acc, None)
  | x :: r => if Ascii.eqb x c then (rev acc, Some r) else split_at c r (x :: acc)
  end.

(* Literal syntax determines an arithmetic tree, including evaluation order.
   Algebraically equal trees are not interchangeable: contextual operations
   may have effects, overflow, trap, or round differently. *)
Inductive literal_tree :=
| LZero | LOne
| LAdd (x y : literal_tree) | LMul (x y : literal_tree)
| LDiv (x y : literal_tree) | LFloat (x : literal_tree).
Definition small_nat (n : nat) : literal_tree :=
  match n with 0 => LZero | S k => Nat.iter k (fun x => LAdd x LOne) LOne end.
Fixpoint strip_digit_zeros (ds : list ascii) : list ascii :=
  match ds with "0"%char :: rest => strip_digit_zeros rest | _ => ds end.
Definition digit_tree (d : ascii) := small_nat (nat_of_ascii d - 48).
Definition integer_tree (ds : list ascii) : literal_tree :=
  match strip_digit_zeros ds with
  | [] => LZero
  | first :: rest => fold_left (fun acc d =>
      let product := LMul acc (small_nat 10) in
      if Ascii.eqb d "0"%char then product else LAdd product (digit_tree d))
      rest (digit_tree first)
  end.
Definition literal_tree_of (text : string) : literal_tree :=
  let '(mant, ex) := split_at "e"%char (list_ascii_of_string text) [] in
  let '(whole, frac) := split_at "."%char mant [] in
  match ex, frac with
  | None, None => integer_tree whole
  | _, _ =>
      let fraction := match frac with Some ds => ds | None => [] end in
      let '(negative, magnitude) := match ex with
        | Some ("-"%char :: ds) => (true, digits_val ds 0)
        | Some ds => (false, digits_val ds 0)
        | None => (false, 0) end in
      let '(divide, power) := if negative then (true, magnitude + length fraction)
        else if Nat.leb (length fraction) magnitude then (false, magnitude - length fraction)
        else (true, length fraction - magnitude) in
      let ten := LFloat (small_nat 10) in
      let value := match strip_digit_zeros (whole ++ fraction) with
        | [] => LFloat LZero
        | first :: rest => fold_left (fun acc d =>
            let product := LMul acc ten in
            if Ascii.eqb d "0"%char then product
            else LAdd product (LFloat (digit_tree d))) rest (LFloat (digit_tree first)) end in
      match power with
      | 0 => value
      | S k => let scale := Nat.iter k (fun acc => LMul acc ten) ten in
          if divide then LDiv value scale else LMul value scale
      end
  end.
Definition NumericText (text : string) : Prop :=
  exists raw kind, Syntax.Lexeme (String.list_byte_of_string raw) kind [] /\
    (kind = Syntax.INTEGER \/ kind = Syntax.DECIMAL) /\ Syntax.lit_norm raw = text.

(* [D84] rule 7 / DECISIONS.md: at most one family in force produces a key.
   Here θ is the matching substitution over P(F), not the full emitted
   application argument list; fixed caller meanings cannot be overridden. *)
Definition FixedAgreement (W : world) (C : cx) (u : nat) : Prop :=
  forall x, Fixed W u x ->
    match ilook (IFun u) (c_apps C), nlook x (c_ty C) with
    | Some recorded, Some now =>
        match nlook x recorded with
        | Some (Some was) => MergeEq (c_merge C) was now
        | _ => True
        end
    | _, _ => True
    end.

Definition FamilyProduces (W : world) (C : cx) (u : nat) (k : item) : Prop :=
  exists fx th C' ks, nlook u (c_funs C) = Some fx /\ Family W u /\
    Forall (fun q => Poly W u (fst q)) th /\
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) th C) (fun t => TAbs t) C' ks /\
    In k ks.

Definition OnlyFamily (W : world) (C : cx) (u : nat) (k : item) : Prop :=
  forall u', u' <> u -> ~ FamilyProduces W C u' k.

Definition family_context (C : cx) (matching : subst) : cx :=
  fold_left (fun C q => set_ty C (fst q) (snd q)) matching C.
Definition TypesFamilyApplication (W : world) (C : cx) (u : usym)
    (matching arguments : subst) (formal : cx) (needs : list item)
    (result : cx) (keys : list item) : Prop :=
  let caller := family_context C matching in
  exists fixed,
    nlook u (c_funs C) = Some fixed /\ Family W u /\
    Forall (fun q => Poly W u (fst q)) matching /\
    FunCtx W u formal needs /\
    arguments = map (fun q => (fst q,
      canon caller (app_subst (c_ty caller) (TAbs (fst q))))) (c_ty formal) /\
    (forall t, In t (map fst (c_ty formal)) -> nlook t (c_ty caller) <> None) /\
    FixedAgreement W caller u /\
    NeedsSatisfied caller formal arguments needs /\
    ProvideResult W u caller (fun t => TAbs t) result keys /\
    NoDup (map provision_id keys).

Record family_use := FamilyUse {
  demand_functor : nat;
  demand_matching : subst;
  demand_arguments : subst;
  demand_formal : cx;
  demand_needs : list item;
  demand_result : cx;
  demand_keys : list item
}.

Definition translate_family_use (C : cx) (d : family_use) : expr -> expr :=
  let caller := family_context C (demand_matching d) in
  apply_expression (demand_functor d) (demand_arguments d)
    (satisfaction_map caller (demand_formal d) (demand_arguments d) (demand_needs d))
    (map (rkey_of (demand_result d)) (demand_keys d)).

(* Rule B for a family demand (criteria sections 2 and 3): the matching
   read off the demanded key is in force here, each generation it
   mentions one that a type in force here means.  A key whose receiver
   mentions a symbol out of force (a callee requirement left unbound,
   before rule B) is not demanded. *)
Definition MatchingInForce (C : cx) (matching : subst) : Prop :=
  forall q y, In q matching -> In y (gens (canon C (snd q))) ->
    exists x u, In (x, u) (c_ty C) /\ In y (gens (canon C u)).

(* [D84] rule 7: θ is read off the demanded key.  The result is formed once
   with each P(F) symbol standing for itself (the oracle's markers), and the
   demanded method's formal receiver there is matched against the key's
   receiver: θ's domain is exactly the P(F) symbols that receiver mentions,
   and each value is the part of the key at that symbol.  The member is
   unique: every item of the family with method m whose receiver matches
   the key gives the same matching (up to the merges here), else which one
   is meant is ambiguous and the demand fails (D84 rule 7; the oracle's
   `family_member`, tests/errors/family_ambiguous_member.moss). *)
Definition FamilyReadOff (W : world) (C : cx) (u : usym) (ot : ty) (m : nat)
    (matching : subst) : Prop :=
  exists marks formal_result formal_keys rf a,
    (forall t, Poly W u t <-> In t marks) /\
    ProvideResult W u (family_context C (map (fun t => (t, TAbs t)) marks))
      (fun t => TAbs t) formal_result formal_keys /\
    In (IMth rf m a) formal_keys /\
    (forall t, In t (map fst matching) <-> In t marks /\ In t (gens rf)) /\
    NoDup (map fst matching) /\ canon C (app_subst matching rf) = ot /\
    (forall rf' a' matching', In (IMth rf' m a') formal_keys ->
       (forall t, In t (map fst matching') <-> In t marks /\ In t (gens rf')) ->
       canon C (app_subst matching' rf') = ot ->
       forall t, option_map (canon C) (lookup_subst t matching') =
                 option_map (canon C) (lookup_subst t matching)).

(* Operators ([D68], [D77] rule 2): fixed to num.moss's detached methods;
   the `for.` operators of [D85]'s `for` sugar (Parse.for_desugar) to
   access.moss's (note 60).  The library module's short name, and the
   method's name in it. *)
Definition operator_method (op : string) (unary : bool) : option (string * string) :=
  if unary then
    match op with
    | "-" => Some ("num", "neg") | "!" => Some ("num", "not")
    | "for.begin" => Some ("access", "begin") | _ => None
    end
  else
    match op with
    | "*" => Some ("num", "mul") | "/" => Some ("num", "div") | "%" => Some ("num", "rem")
    | "+" => Some ("num", "add") | "-" => Some ("num", "sub") | "<<" => Some ("num", "shl")
    | ">>" => Some ("num", "shr") | "==" => Some ("num", "eq") | "!=" => Some ("num", "ne")
    | "<" => Some ("num", "lt") | ">" => Some ("num", "gt") | "<=" => Some ("num", "le")
    | ">=" => Some ("num", "ge") | "&" => Some ("num", "and") | "^" => Some ("num", "xor")
    | "|" => Some ("num", "or")
    | "for.more" => Some ("access", "more") | "for.get" => Some ("access", "get")
    | "for.after" => Some ("access", "after")
    | _ => None
    end.

(* Bare syntax supplies the unit payload. *)
Definition tag_body (a : adecl) : option (name * list name * aty) :=
  match a with
  | ADBareTag n => Some (n, [], ATUnit)
  | ADTag n ps _ t => Some (n, ps, t)
  | _ => None
  end.

(* The payload type of a tag at its arguments. *)
Inductive PayloadOf (W : world) : nat -> list (nat * ty) -> ty -> Prop :=
| PayTag : forall s xs d n ps t pay,
    w_decl W s = Some d -> tag_body (d_ast d) = Some (n, ps, t) -> TyOf W (d_mod d) xs None t pay ->
    PayloadOf W s xs pay.

(* `let` binds an immutable local, `var` an assignable one. *)
Inductive LetStmt : astmt -> name -> option aty -> aexpr -> lmut -> Prop :=
| LSLet : forall n a y, LetStmt (ASLet n a y) n a y LLet
| LSVar : forall n a y, LetStmt (ASVar n a y) n a y LVar.

(* Whether a `break` in an expression, block or statement leaves the
   innermost loop around it: one not inside a nested `while` or `loop`
   (note 42). *)
Inductive Breaks (W : world) : string -> aexpr -> Prop :=
| BkBreak : forall md,Breaks W md AEBreak
| BkCall : forall md p a args y, In y args -> Breaks W md y -> Breaks W md (AECall p a args)
| BkField : forall md o n bare, Breaks W md o -> Breaks W md (AEField o n bare)
| BkMethRecv : forall md o p args, Breaks W md o -> Breaks W md (AEMeth o p args)
| BkMethArg : forall md o p args y, In y args -> Breaks W md y -> Breaks W md (AEMeth o p args)
| BkBareRecord : forall md fs n y, In (n, Some y) fs -> Breaks W md y -> Breaks W md (AEBareRecord fs)
| BkRec : forall md p a fs n y, In (n, Some y) fs -> Breaks W md y -> Breaks W md (AERec p a fs)
| BkIfC : forall md c b e, Breaks W md c -> Breaks W md (AEIf c b e)
| BkIfB : forall md c b e, BlockBreaks W md b -> Breaks W md (AEIf c b e)
| BkIfE : forall md c b y, Breaks W md y -> Breaks W md (AEIf c b (Some y))
| BkMatchS : forall md s arms, Breaks W md s -> Breaks W md (AEMatch s arms)
| BkMatchA : forall md s arms pt y, In (pt, y) arms -> Breaks W md y -> Breaks W md (AEMatch s arms)
| BkReturn : forall md y, Breaks W md y -> Breaks W md (AEReturn (Some y))
| BkBlock : forall md b, BlockBreaks W md b -> Breaks W md (AEBlock b)
| BkUnary : forall md op y, Breaks W md y -> Breaks W md (AEUnary op y)
| BkBinL : forall md op l r, Breaks W md l -> Breaks W md (AEBinary op l r)
| BkBinR : forall md op l r, Breaks W md r -> Breaks W md (AEBinary op l r)
with BlockBreaks (W : world) : string -> ablock -> Prop :=
| BBStmt : forall md ss tl s, In s ss -> StmtBreaks W md s -> BlockBreaks W md (ABlock ss tl)
| BBTail : forall md ss y, Breaks W md y -> BlockBreaks W md (ABlock ss (Some y))
with StmtBreaks (W : world) : string -> astmt -> Prop :=
| SBLet : forall md n t y, Breaks W md y -> StmtBreaks W md (ASLet n t y)
| SBVar : forall md n t y, Breaks W md y -> StmtBreaks W md (ASVar n t y)
| SBAssign : forall md n y, Breaks W md y -> StmtBreaks W md (ASAssign n y)
| SBExpr : forall md y, Breaks W md y -> StmtBreaks W md (ASExpr y)
| SBBind : forall md its it, In it its -> ItemBreaks W md it -> StmtBreaks W md (ASBind its)
(* a `while` condition is evaluated outside the loop it controls *)
| SBWhile : forall md c b, Breaks W md c -> StmtBreaks W md (ASWhile c b)
(* a bind item breaks when its expression does, or, for `bind F;` with F
   a functor with a body, when an item of F's body does: the body's binds
   run in place, inside the loop around the `bind` (read in F's module) *)
with ItemBreaks (W : world) : string -> spec * option aexpr -> Prop :=
| IBExpr : forall md sp y, Breaks W md y -> ItemBreaks W md (sp, Some y)
| IBApply : forall md pa u d n ps a r bodies it,
    resolve_sym W md pa = Some u -> w_decl W u = Some d -> d_ast d = ADFunctor n ps a r (Some bodies) ->
    In it (concat bodies) -> ItemBreaks W (d_mod d) it ->
    ItemBreaks W md (Spec (Some pa) None None, None).

(* The type a block has as a branch of an `if` or a `match` arm: its
   tail's, and `()` without a tail, even if it cannot fall through
   (note 42). *)
Definition branch_ty (b : ablock) (t : ty) : ty :=
  match b with ABlock _ None => TUnit | ABlock _ (Some _) => t end.

(* Auxiliary judgments (not mutually recursive with the main ones). *)

(* A forwarder's identity is determined by the abstract input it invokes.
   Typing checks that input in the context. Translation assigns the
   forwarder's function body its program-table number. *)
Record forwarder_handle := ForwarderOf { forwarder_source : nat }.

Definition TypesForwarder (W : world) (F : frame) (C : cx)
    (pa : path) (sg : fsig) (handle : forwarder_handle) : Prop :=
  let g := forwarder_source handle in
  resolve_sym W (fr_mod F) pa = Some g /\ kind_sym W g = Some DKFn /\
  (exists nm ps rt, ast_of W g = Some (ADFn RPlain nm ps rt None)) /\
  nlook g (c_fns C) = Some sg.

Definition forwarder_body (handle : forwarder_handle) (sg : fsig) : expr :=
  EInvoke (forwarder_source handle) (map EVar (seq 0 (length (sg_params sg)))).

Inductive Join : ty -> ty -> option ty -> ty -> Prop :=
| JoinExp : forall a b t, Join a b (Some t) t
| JoinLeftNever : forall b, Join never b None b
| JoinSame : forall a b, (b = never \/ a = b) -> a <> never -> Join a b None a.

(* D78 compares raw signature leaves, recursively through contexts. Types
   remain leaves; tags, aliases, and concrete values/functions do
   not require a provision. Brackets affect later interpretations, not
   this preliminary leaf-identity check. *)
Definition result_already_provided (d : adecl) : bool :=
  match d with
  | ADBareTag _ | ADTag _ _ _ _ | ADAlias _ _ => true
  | ADVal _ _ (Some _) | ADFn RPlain _ _ _ (Some _) => true
  | _ => false
  end.
Inductive ResultSpecLeaf (W : world) : string -> spec -> (nat * option name) -> Prop :=
| RSLMethod : forall p pa dn br s,
    resolve_sym W p pa = Some s ->
    ResultSpecLeaf W p (Spec (Some pa) (Some dn) br) (s,Some dn)
| RSLPlain : forall p pa br s d,
    resolve_sym W p pa = Some s -> w_decl W s = Some d ->
    (forall n its, d_ast d <> ADCtx n its) ->
    result_already_provided (d_ast d) = false ->
    ResultSpecLeaf W p (Spec (Some pa) None br) (s,None)
| RSLContext : forall p pa br c d n its sp k,
    resolve_sym W p pa = Some c -> w_decl W c = Some d ->
    d_ast d = ADCtx n its -> In sp (flat_map citem_specs its) ->
    ResultSpecLeaf W (d_mod d) sp k ->
    ResultSpecLeaf W p (Spec (Some pa) None br) k.
Inductive ResultLeaf (W : world) : nat -> (nat * option name) -> Prop :=
| RL : forall u d n ps a r b sp k,
    w_decl W u = Some d -> d_ast d = ADFunctor n ps a r b ->
    In sp r -> ResultSpecLeaf W (d_mod d) sp k -> ResultLeaf W u k.

(* `CollidesOnlyAs ks ks0`: two positions of a result that `ks` files
   under one provision id are one item in the reading `ks0` too (the same
   result read elsewhere, position by position).  A collision `ks0` lacks
   would leave which item is installed to a choice ([D84] rule 7's
   ambiguity, for a non-family functor; criteria section 2). *)
Definition CollidesOnlyAs (ks ks0 : list item) : Prop :=
  forall i j k k', nth_error ks i = Some k -> nth_error ks j = Some k' ->
    provision_id k = provision_id k' ->
    exists k0 k0', nth_error ks0 i = Some k0 /\ nth_error ks0 j = Some k0' /\
      provision_id k0 = provision_id k0'.

(* [D78]/[D84] 3e: G's result is F's, leaf by leaf. *)
Definition SameResult (W : world) (u g : nat) : Prop :=
  forall k, ResultLeaf W u k <-> ResultLeaf W g k.

(* [D78] (the oracle's copy_output_compatible): `bind F = H;` is
   checked at every application of F at once. F's fixed inputs read as
   sigma says (what they mean where F is bound, which every application
   keeps, BApplyAbstract's rule 5), and every other generation of F's
   input readings (one sigma does not read: F's non-fixed inputs, which
   each application supplies afresh) reads as its own fresh symbol cg y,
   as the oracle replaces each unsolved generation by a fresh `@copy`
   symbol. copy_subst is that reading. H's terms are read in F's through
   F's merge (H's inputs are among F's), then at copy_subst, and there:
   - H's inputs read as F's: each of H's merges holds. A merge of H that
     F lacks is accepted when F's fixed readings make it hold, as they do
     at every application (tests/wasi/copy_site_merge.moss); one a later application
     could break is not (copy-poly-merge.moss, tests/errors/copy_merge.moss).
   - H's needs read as F's. A candidate must give the same key,
     signature and instantiation at the copy site. If several candidates
     agree there, the unique one already equal in the declared merges
     supplies the provision. An unresolved choice would be observable
     when an application has separate provisions for the target keys.
   - H's result gives each key of F's result the provision F's result
     gives it (each key's last setter), both read with the same fresh
     output roles. H's result is read in its own context Ch, where its
     merges hold. rm records, for each position i of F's result, the
     position j of H's whose item F's i-th is: H's key there is F's key
     at copy_subst, H's provision there agrees with F's, and every
     position of H's with that key at copy_subst has one key in H's own
     terms, of which j is the last (the oracle's `find ... (rev gkeys)`).
     So two result items distinct in H's own terms are never one key of
     F's, where choosing either would be a choice (coherence, criteria
     section 2; tests/errors/copy_result_collide.moss), and repeated
     positions of F read one source. An application of F installs
     exactly F's result, position by position under the caller's keys
     (Rule A, result_selection), so a key H has and F lacks is never
     installed (tests/wasi/copy_extra_key.moss).
   sigma's readings are the copy site's and may name a symbol that is also
   one of F's inputs, which must not be identified with it
   (tests/errors/copy_bracket_nonfixed.moss, tests/errors/copy_fixed_reads_poly.moss;
   scratch/orch/copyfresh-AUDIT.md). Fresh means fresh for sigma's image
   too, as the oracle's @copy symbols are: a copy site can read a fixed
   input as a fresh application generation, and a cg y or output role
   equal to it would identify it with F's input or output again
   (tests/errors/copy_fresh_collide.moss, scratch/orch/realizepf-AUDIT.md). *)
Definition copy_subst (sigma : subst) (ys : list nat) (cg : nat -> nat) : subst :=
  sigma ++ map (fun y => (y, TAbs (cg y))) ys.

(* Fixed readings of an atomic class use one slot of the current
   section. A class with different recorded readings stays symbolic. *)
Definition copy_fixed_slot (C : cx) (x : nat) : nat :=
  match canon C (TAbs x) with TAbs y => y | _ => x end.

Definition copy_fixed_readings (C : cx) (sigma : subst) : subst :=
  let readings := map (fun q => (copy_fixed_slot C (fst q), snd q)) sigma in
  filter (fun q =>
    forallb (fun r => negb (Nat.eqb (fst q) (fst r)) || ty_eqb (snd q) (snd r)) readings)
    readings.

Definition copy_allocation_scope (W : world) (sigma : subst) : allocation_scope nat :=
  AllocationScope (Fresh W) [] (flat_map (fun q => gens (snd q)) sigma).

Definition copy_need_key (C : cx) (s : subst) (k : item) : item :=
  app_subst_item s (canon_item C (provision_id k)).

Definition merge_equations (M : merge) : list entry :=
  flat_map (fun c =>
    match mc_members c with
    | [] => []
    | x :: rest =>
        let members := map (fun y => NEq (TAbs x) (TAbs y)) rest in
        match mc_shape c with
        | Some t => NEq (TAbs x) t :: members
        | None => members
        end
    end) M.

Definition CopyNeedAt (C : cx) (s : subst) (k : item) (p : provision) (k' : item) : Prop :=
  copy_need_key C s k = copy_need_key C s k' /\
  exists p', ilook (provision_id k') (c_provisions C) = Some p' /\
    provision_subst s (canon_provision C p) = provision_subst s (canon_provision C p').

Definition CopyNeedChoice (Cu Ch : cx) (s : subst) (needs : list item)
    (k : item) (p : provision) (target : item) : Prop :=
  In target needs /\ CopyNeedAt Cu s k p target /\
  ((forall other, In other needs -> CopyNeedAt Cu s k p other -> other = target) \/
   exists M, MergeOf (merge_equations (c_merge Cu) ++ merge_equations (c_merge Ch)) M /\
     KeyEq M k target /\
     (forall other, In other needs -> CopyNeedAt Cu s k p other -> KeyEq M k other -> other = target)).

Definition CopyInterpretation (W : world) (sigma : subst) (u h : nat) (Cu : cx) (nsu nsh : list item) (Ch : cx)
    (rm : list nat) (target : item -> item) : Prop :=
  let fixed := copy_fixed_readings Cu sigma in
  exists outs gen ys cg Cr ks Cr' ks' scope_after,
    (forall t, In t outs <-> FunOutputs W u t) /\
    Allocates (copy_allocation_scope W sigma) (map gen outs ++ map cg ys) scope_after /\
    (forall y, In y ys <-> (exists x tx, nlook x (c_ty Cu) = Some tx /\ In y (gens tx)) /\
                          lookup_subst y fixed = None) /\
    (* inputs *)
    (forall x tx, nlook x (c_ty Ch) = Some tx ->
       app_subst (copy_subst fixed ys cg) (canon Cu (TAbs x)) = app_subst (copy_subst fixed ys cg) (canon Cu tx)) /\
    (* needs *)
    (forall k, In k nsh -> exists p,
       ilook (provision_id k) (c_provisions Ch) = Some p /\
       CopyNeedChoice Cu Ch (copy_subst fixed ys cg) nsu k p (target k)) /\
    (* the result *)
    ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks /\
    ProvideResult W h Ch (fun t => TAbs (gen t)) Cr' ks' /\
    length rm = length ks /\
    (forall i k, nth_error ks i = Some k -> exists j k' p p',
       nth_error rm i = Some j /\ nth_error ks' j = Some k' /\
       app_subst_item (copy_subst fixed ys cg) (canon_item Cu (provision_id k')) =
         app_subst_item (copy_subst fixed ys cg) (provision_id k) /\
       (forall j' k'', nth_error ks' j' = Some k'' ->
          app_subst_item (copy_subst fixed ys cg) (canon_item Cu (provision_id k'')) =
            app_subst_item (copy_subst fixed ys cg) (provision_id k) ->
          provision_id k'' = provision_id k' /\ j' <= j) /\
       ilook (provision_id k) (c_provisions Cr) = Some p /\
       ilook (provision_id k') (c_provisions Cr') = Some p' /\
       provision_subst (copy_subst fixed ys cg) p = provision_subst (copy_subst fixed ys cg) (canon_provision Cu p')).

(* A promised item is compared with the body's at the body's meanings of
   the output roles (each role at what the site's table makes of it,
   canonically), as the oracle's promised_result_type does: a body that
   binds `T = Bool` provides `mk(): T` as `mk(): Bool`. *)
Definition role_reads (site : cx) (outs : list nat) (gen : nat -> nat) : subst :=
  map (fun t => (gen t, canon site (app_subst (c_ty site) (TAbs (gen t))))) outs.
(* Q6: a fixed input of F that the body pins (binds, whether or not G takes
   it) is compared at the pin, canonically: a body that pins `Tok = Char`
   provides `tok(): Tok` as `tok(): Char` (tests/wasi/pin_fixed_agree.moss).
   Every application reads Tok as the bind site did (rule 5), and the bind
   site's reading is the pin's (BFunClosure), so the two agree. *)
Definition pin_reads (body : cx) (pins : subst) : subst :=
  map (fun q => (fst q, canon body (snd q))) pins.
(* Where a linked body's result key is read (LinkBody): an output role
   (gen t) at the body's final table (a body that binds T = Bool provides
   T.m as Bool.m), every symbol at G's own readings. An input's meaning at
   an application is the caller's, and a rebind in the body writes only its
   alias, so a key's receiver may not follow it (an unpinned merge
   representative, tests/errors/link_merge_rep_rebind.moss; the oracle's
   check is by written leaf, scratch/orch/linknewpf-AUDIT.md). That holds
   for an input of F that is also an output: its input occurrences (a
   merge representative's) read at G's table, only its role at the body's
   (tests/errors/link_output_input_rep.moss, scratch/orch/realize3-AUDIT.md).
   The roles stay in the keys and promised types, so the site's table
   alone reads them.  A pinned fixed input (Q6, LinkBody's pins) reads at
   its pin, the body's meaning of it: every application reads it as the
   bind site did, which BFunClosure checks is the pin, so the body's key
   is the caller's (tests/wasi/pin_fixed_key.moss, AUDIT-CHANGES #73). *)
Definition link_site (Cg Cb : cx) (pins : subst) (outs : list nat) (gen : nat -> nat) : cx :=
  Cx (map (fun t => (gen t, match nlook t (c_ty Cb) with Some v => v | None => TAbs t end)) outs ++ pins ++ c_ty Cg)
     (c_sub Cb) (c_merge Cb) (c_provisions Cb) (c_locals Cb).
(* A linked body's output probes are local to the comparison with its
   promised result. They are read at [link_site] before code emission. *)
Definition link_output_scope (W : world) : allocation_scope nat :=
  AllocationScope (Fresh W) [] [].

Definition TypesLinkOutputs (W : world) (u : nat) (Cu Cb : cx)
    (outs : list nat) (gen : nat -> nat) (Cr : cx) (ks : list item) : Prop :=
  (forall t, In t outs <-> FunOutputs W u t) /\
  (forall t, In t outs -> exists meaning, nlook t (c_ty Cb) = Some meaning) /\
  AllocationFresh (link_output_scope W) (map gen outs) /\
  ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks.

(* An atomic source class that has a shared input can use that input
   to read its private representative. The emitted table keeps the source
   symbols as keys, so body code still reads its own lexical names. *)
Definition link_input_anchors (Cu Cg : cx) : subst :=
  flat_map (fun q =>
    let y := fst q in
    match nlook y (c_ty Cu) with
    | Some _ => []
    | None =>
        match find (fun shared =>
          match nlook (fst shared) (c_ty Cu) with
          | Some _ => ty_eqb (snd shared) (TAbs y)
          | None => false
          end) (c_ty Cg) with
        | Some shared => [(y, TAbs (fst shared))]
        | None => []
        end
    end) (c_ty Cg).

Definition link_input_table (Cu Cg : cx) : subst :=
  let anchors := link_input_anchors Cu Cg in
  map (fun q => (fst q, app_subst anchors (snd q))) (c_ty Cg).

Definition translate_linked_body (Cu Cg : cx) (outs : list nat)
    (bindings : list binding) (keys : list item) : functor_code :=
  FunctorCode (map fst (c_ty Cg))
    (filter (fun t => existsb (Nat.eqb t) (map fst (c_ty Cu))) (map fst (c_ty Cg)))
    (link_input_table Cu Cg) (map ITy outs) bindings keys.

(* Each promised key, read at the body's site (at_site re-reads its
   receiver), is provided there, at the promised provision with `reads`
   substituted (the roles' meanings, role_reads). *)
Definition ResultRealizes (body promised : cx) (reads : subst) (keys : list item) : Prop :=
  forall k, In k keys -> exists required actual,
    ilook (provision_id k) (c_provisions promised) = Some required /\
    ilook (provision_id (at_site body k)) (c_provisions body) = Some actual /\
    provision_refines
      (provision_subst (c_sub body) (provision_subst reads required))
      (provision_subst (c_sub body) actual).
(* Capture is lexical: an inner binder hides an outer name only after its
   initializer. Assigning an outer name is a mention as well as reading it. *)
Section LexicalNames.
Variable global_name : name -> bool.
Fixpoint pattern_names (pattern : apat) : list name :=
  match pattern with
  | APWild => []
  | APPath [n] => if global_name n then [] else [n]
  | APPath _ => []
  | APTag _ payload _ => pattern_names payload
  | APRec _ fields => flat_map (fun q => match snd q with
      | None => [fst q] | Some p => pattern_names p end) fields
  end.
Fixpoint free_names (e : aexpr) : list name :=
  match e with
  | AEUnit | AEThis | AEBreak | AELit _ => []
  | AEPath [n] None => [n]
  | AEPath _ _ => []
  | AECall _ _ args => flat_map free_names args
  | AEField receiver _ _ | AEUnary _ receiver => free_names receiver
  | AEMeth receiver _ args => free_names receiver ++ flat_map free_names args
  | AERec _ _ fields | AEBareRecord fields => flat_map (fun q => match snd q with
      | Some value => free_names value | None => [fst q] end) fields
  | AEIf condition body otherwise => free_names condition ++ free_block_names body ++
      match otherwise with Some e => free_names e | None => [] end
  | AEMatch scrutinee arms => free_names scrutinee ++
      flat_map (fun arm => fold_right (remove String.string_dec) (free_names (snd arm))
        (pattern_names (fst arm))) arms
  | AEReturn value => match value with Some e => free_names e | None => [] end
  | AEBlock body => free_block_names body
  | AEBinary _ lhs rhs => free_names lhs ++ free_names rhs
  | AELam params _ body => fold_right (fun p names => remove String.string_dec (fst p) names)
      (free_names body) params
  end
with free_block_names (body : ablock) : list name :=
  match body with ABlock statements tail =>
    let rest := match tail with Some e => free_names e | None => [] end in
    (fix walk (ss : list astmt) : list name :=
      match ss with
      | [] => rest
      | ASLet n _ value :: more | ASVar n _ value :: more =>
          free_names value ++ remove String.string_dec n (walk more)
      | statement :: more => free_statement_names statement ++ walk more
      end) statements
  end
with free_statement_names (statement : astmt) : list name :=
  match statement with
  | ASLet _ _ value | ASVar _ _ value | ASExpr value => free_names value
  | ASAssign n value => n :: free_names value
  | ASBind bindings => flat_map (fun q => match snd q with
      | Some e => free_names e | None => [] end) bindings
  | ASWhile condition body => free_names condition ++ free_block_names body
  | ASLoop body => free_block_names body
  end.
End LexicalNames.
Definition Mentions_name (W : world) (F : frame) (body : aexpr) (n : name) : Prop :=
  In n (free_names (fun n => match resolve W (fr_mod F) [n] with
    | Some _ => true | None => false end) body).

(* A lambda captures the enclosing locals it mentions, except its own
   parameters. The corresponding value identities extend the capture
   scope as a batch, preserving every provision already in force. *)
Definition TypesLambdaCaptures (W : world) (F : frame) (C : cx)
    (parameters : list name) (body : aexpr) (caps : list name) (values : list nat) : Prop :=
  (forall n, In n caps <-> (Mentions_name W F body n /\ slook n (c_locals C) <> None /\
    ~ In n parameters /\ fr_applying F = [])) /\
  NoDup caps /\ length values = length caps /\
  exists after, Allocates (capture_allocation_scope W C) values after.

Definition translate_lambda_captures (F : frame) (captures : list (name * nat)) (next : expr) : expr :=
  fold_right (fun q rest => bind_value_expression (snd q) (EVar (var_of F (fst q))) rest) next captures.

Definition translate_lambda_body (F : frame) (captures : list (name * nat)) (body : expr) : expr :=
  fold_right (fun q rest => ELet (var_of F (fst q)) (EVal (snd q)) rest) body captures.

(* Automatic family demand checks an initial missing method key, then applies
   its unique family in the context extended by preceding demands.  Type
   arguments are temporary; a family provides methods only. *)
Definition family_restore (C Cr : cx) : cx :=
  Cx (c_ty C) (c_sub Cr) (c_merge Cr) (c_provisions Cr) (c_locals Cr).

Inductive FamilyStep (W : world) (C0 : cx) (wanted : list item)
    : cx -> nat -> subst -> subst -> cx -> list item -> cx -> list item -> Prop :=
| FamilyStepApply : forall C ot m u matching th formal needs Cr ks,
    In (mkey ot m) wanted -> mlook ot m (c_mths C0) = None ->
    OnlyFamily W C0 u (mkey ot m) ->
    TypesFamilyApplication W C u matching th formal needs Cr ks ->
    FamilyReadOff W C u ot m matching ->
    In (mkey ot m) ks -> MatchingInForce C matching ->
    FamilyStep W C0 wanted C u matching th formal needs Cr ks.

(* The matching identifies a demand group. Separate groups can read their
   complete input arguments alike. *)
Inductive Demand (W : world) (C0 : cx) (wanted : list item)
    : list (nat * subst) -> cx -> list family_use -> cx -> Prop :=
| DemandDone : forall used C,
    Demand W C0 wanted used C [] C
| DemandApply : forall used C u matching th formal needs Cr ks rest Cf,
    ~ In (u, matching) used -> FamilyStep W C0 wanted C u matching th formal needs Cr ks ->
    Demand W C0 wanted ((u, matching) :: used) (family_restore C Cr) rest Cf ->
    Demand W C0 wanted used C (FamilyUse u matching th formal needs Cr ks :: rest) Cf.

Fixpoint translate_demands (C : cx) (demands : list family_use) (body : expr) : expr :=
  match demands with
  | [] => body
  | d :: rest =>
      translate_family_use C d
        (translate_demands (family_restore C (demand_result d)) rest body)
  end.

(* D89: the bracket application on a call of the fn f: each binding names
   a type symbol among f's requirements and binds it to a type by name. *)
Definition CallBrackets (W : world) (p : string) (f : nat) (bs : list (path * spec)) : Prop :=
  Forall (fun b => (exists x, resolve_sym W p (fst b) = Some x /\ kind_sym W x = Some DKType /\
                              In x (w_reqs W f)) /\
                   exists rp ra, snd b = Spec (Some rp) None ra) bs.

(* A declared output can use its own name. A shadowing output instead
   receives a generated identity, encoded as a runtime number. *)
Inductive output_identity :=
| DeclaredOutput
| GeneratedOutput (generation : nat).

Definition output_choice_at (choices : list (nat * output_identity)) (t : nat) : output_identity :=
  match nlook t choices with Some choice => choice | None => DeclaredOutput end.

Definition encode_output_identity (t : nat) (choice : output_identity) : nat :=
  match choice with DeclaredOutput => t | GeneratedOutput generation => generation end.

Definition output_generation_at (choices : list (nat * output_identity)) (t : nat) : nat :=
  encode_output_identity t (output_choice_at choices t).

Definition allocated_output_generations (choices : list (nat * output_identity)) : list nat :=
  flat_map (fun q => match snd q with
    | DeclaredOutput => []
    | GeneratedOutput generation => [generation]
    end) choices.

(* [D78]/[D84] rule 5, `bind F;` for F abstract and in force.
   Typing resolves the functor and determines the meanings at its use,
   the new identities and the provisions available afterward. *)
Record application := Application {
  app_functor : nat;
  app_input : cx;
  app_needs : list item;
  app_arguments : subst;
  app_output_choices : list (nat * output_identity);
  app_result : cx;
  app_provisions : list item
}.

Definition app_outputs (a : application) : list nat := map fst (app_output_choices a).
Definition app_identity (a : application) : nat -> nat := output_generation_at (app_output_choices a).
Definition app_allocated (a : application) : list nat := allocated_output_generations (app_output_choices a).

(* A fresh generation preserves an existing meaning of the same source
   name. A provision key can mention a type even when it has no binding
   (tests/errors/lambda_declared_output.moss). LinkBody separately checks
   that a body does not make an input of the functor to which it is bound
   (tests/errors/link_kept_output_pin.moss). *)
Definition OutputIdentityAt (W : world) (F : frame) (C : cx)
    (a : application) : Prop :=
  forall t, FunOutputs W (app_functor a) t ->
    match output_choice_at (app_output_choices a) t with
    | DeclaredOutput =>
        nlook t (c_ty C) = None /\
        ~ In t ((match fr_ret F with Some r => gens r | None => [] end) ++
                (match fr_this F with Some r => gens r | None => [] end)) /\
        ~ In t (context_gens C ++ key_receiver_gens C)
    | GeneratedOutput _ =>
        nlook t (c_ty C) <> None \/ In t (context_gens C ++ key_receiver_gens C)
    end.

Definition TypesApplication (W : world) (F : frame) (C : cx)
    (pa : path) (a : application) : Prop :=
  let u := app_functor a in
  let Cu := app_input a in
  let th := app_arguments a in
  let gensof := app_identity a in
  resolve_sym W (fr_mod F) pa = Some u /\
  (* Fixed inputs retain their meanings from the provision site. *)
  (exists fixed, nlook u (c_funs C) = Some fixed /\
    forall x tx, In (x, tx) fixed -> exists here,
      nlook x (c_ty C) = Some here /\ canon C here = canon C tx) /\
  FunCtx W u Cu (app_needs a) /\
  (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here, nlook t (c_ty C) = Some here) /\
  th = map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cu) /\
  NeedsSatisfied C Cu th (app_needs a) /\
  FreshFor W F C (app_allocated a) /\
  (forall t t', FunOutputs W u t -> FunOutputs W u t' -> gensof t = gensof t' -> t = t') /\
  OutputIdentityAt W F C a /\
  NoDup (app_outputs a) /\ (forall t, In t (app_outputs a) <-> FunOutputs W u t) /\
  ProvideResult W u C (fun t => TAbs (gensof t)) (app_result a) (app_provisions a) /\
  (* Distinct result items must stay distinct at this application.
     The declared result may repeat a key (tests/wasi/result_repeat_in_f.moss). *)
  (exists Cf ksf, ProvideResult W u Cu (fun t => TAbs t) Cf ksf /\
    CollidesOnlyAs (app_provisions a) ksf).

(* Encode the checked application as a run-time binding. *)
Definition translate_application (C : cx) (a : application) : expr -> expr :=
  let th := app_arguments a in
  apply_output_expression (app_functor a) th
    (map (fun t => (t, app_identity a t)) (app_outputs a))
    (satisfaction_map C (app_input a) th (app_needs a))
    (map (rkey_of (app_result a)) (app_provisions a)).

Definition linked_target (target : item -> option item) (k : item) : item :=
  match target k with Some key => key | None => k end.

Definition is_linked (target : item -> option item) (k : item) : bool :=
  match target k with Some _ => true | None => false end.

Definition linked_needs (target : item -> option item) (needs : list item) : list item :=
  filter (is_linked target) needs.

Definition captured_needs (target : item -> option item) (needs : list item) : list item :=
  filter (fun k => negb (is_linked target k)) needs.

Definition link_map (Cu Cg : cx) (target : item -> option item) (needs : list item) : smap :=
  map (fun k => (rkey_of Cg k, rkey_of Cu (linked_target target k))) (linked_needs target needs).

Definition link_need_gens (k : item) (p : provision) : list nat :=
  provision_gens p ++ match k with IMth r _ _ => gens r | _ => [] end.

Definition LinkNeedScope (C : cx) (k : item) (p : provision) : Prop :=
  forall x, In x (link_need_gens k p) -> nlook x (c_ty C) <> None.

Definition LinkNeedChoice (Cu Cg : cx) (nsu : list item) (rho : subst)
    (k : item) (p : provision) (choice : option item) : Prop :=
  match choice with
  | Some other => LinkNeedScope Cu k p /\ LinkNeedScope Cg k p /\
      CopyNeedChoice Cu Cg rho nsu k p other /\
      (forall q, ilook (provision_id other) (c_provisions Cu) = Some q -> LinkNeedScope Cu other q)
  | None =>
      ((LinkNeedScope Cu k p /\ LinkNeedScope Cg k p) ->
       forall other, In other nsu -> copy_need_key Cu rho k <> copy_need_key Cu rho other) /\
      (~ (LinkNeedScope Cu k p /\ LinkNeedScope Cg k p) -> ~ In (canon_item Cu k) nsu)
  end.

(* A missing private representative is supplied by the linked class only
   when the closure has no lexical meaning for it. *)
Definition link_adopted_inputs (C Cu Cg : cx) : subst :=
  filter (fun q => negb (existsb (Nat.eqb (fst q))
    (map fst (c_ty C) ++ context_gens C))) (link_input_anchors Cu Cg).

(* A private input that is directly in force reads from the closure's
   lexical capture. The shared inputs always come from its application. *)
Definition link_private_readings (C Cu Cg : cx) : subst :=
  flat_map (fun q =>
    match nlook (fst q) (c_ty Cu), nlook (fst q) (c_ty C) with
    | None, Some t => [(fst q, canon C t)]
    | _, _ => []
    end) (c_ty Cg).

(* Shared inputs are checked under the target's fixed site readings and
   fresh atoms for the other classes. The body is checked separately. *)
Record LinkBinding (W : world) (u : nat) (Cu : cx) (nsu : list item)
    (Cg : cx) (nsg : list item) (recorded private adopted : subst) (target : item -> option item) : Prop := {
  link_bind_scope : forall k p t, In k nsg -> target k = None ->
    ilook (provision_id k) (c_provisions Cg) = Some p ->
    In t (link_need_gens k p) -> lookup_subst t adopted = None /\ (nlook t (c_ty Cu) <> None -> Fixed W u t);
  link_bind_interpretation :
    let fixed := copy_fixed_readings Cu recorded in
    exists ys cg scope_after,
      let rho := copy_subst fixed ys cg in
      Allocates (copy_allocation_scope W (recorded ++ private)) (map cg ys) scope_after /\
      (forall y, In y ys <-> (exists x tx, nlook x (c_ty Cu) = Some tx /\ In y (gens tx)) /\
        lookup_subst y fixed = None) /\
      (forall k, In k nsg -> exists p, ilook (provision_id k) (c_provisions Cg) = Some p /\
        LinkNeedChoice Cu Cg nsu rho (app_subst_item adopted k) (provision_subst adopted p) (target k)) /\
      (forall x tx, nlook x (c_ty Cg) = Some tx -> nlook x (c_ty Cu) <> None ->
        let tx := app_subst adopted tx in
        (forall y, In y (gens tx) -> nlook y (c_ty Cu) <> None \/ lookup_subst y private <> None) /\
        app_subst rho (canon Cu (TAbs x)) = app_subst (private ++ rho) (canon Cu tx))
}.

(* ======================================================================== *)
(* V.7 Programs                                                              *)
(* ======================================================================== *)

(* [D20]: an assumed provision brings its requirements and its needs, not
   just a signature.  `AssumeCtx W s C ns`: the context the assume nesting
   of symbol s forms, and its needs (CtxOf without the world's record,
   since a functor's record is its inputs' context, FunCtx). *)
Definition AssumeCtx (W : world) (s : nat) (C : cx) (ns : list item) : Prop :=
  exists d es tm er tmr σ M,
    w_decl W s = Some d /\
    ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) es tm /\
    RootCtx W s es tm er tmr /\
    MostGeneralMerge σ (es ++ er) /\ MergeOf (es ++ er) M /\ SectionOf M σ /\
    C = entries_cx σ M tmr (es ++ er) /\ Needs σ (es ++ er) ns.

(* An atomic class needed by an assumption is a leaf of an expanded
   requirement. It needs a fresh reading when no member is in force. *)
Definition MissingAtomicClass (C Cs : cx) (r : nat) : Prop :=
  (exists x z, In x (map fst (c_ty Cs)) /\
    In z (gens (expand (c_merge Cs) (TAbs x))) /\
    MergeEq (c_merge Cs) (TAbs r) (TAbs z)) /\
  ~ InForceClass C (c_merge Cs) r.

Definition AssumptionAllocation {identity} (C Cs : cx) (before : allocation_scope identity)
    (reading : nat -> option identity) : Prop :=
  exists batch after,
    Allocates before batch after /\
    (forall r, MissingAtomicClass C Cs r <-> exists g, reading r = Some g) /\
    (forall g, In g batch <-> exists r, reading r = Some g) /\
    (forall r r' g g', reading r = Some g -> reading r' = Some g' ->
      (g = g' <-> MergeEq (c_merge Cs) (TAbs r) (TAbs r'))).

Definition assumption_scope (W : world) (C Cs : cx) : allocation_scope nat :=
  AllocationScope (Fresh W) [] (context_type_support C ++ context_type_support Cs).

Definition AssumptionReadings (W : world) (C Cs : cx) (th : subst) : Prop :=
  exists atom,
    AssumptionAllocation C Cs (assumption_scope W C Cs) atom /\
    forall r a, In r (map fst (c_ty Cs)) -> atom r = Some a ->
      lookup_subst r th = Some (TAbs a).

(* A callable entry carries the instantiation read from its prefix and
   its enclosing brackets. *)
Definition assumed_application (e : entry) : option (nat * inst) :=
  match e with
  | NVal s _ a | NFn s _ a | NMth _ s _ a => Some (s, a)
  | _ => None
  end.

Definition extend_assumed_types (tm : subst) (e : entry) : subst :=
  match e with NTy t => add_tys [t] tm | _ => tm end.

(* Each callable provision is checked against the merged prefix before it
   is added. Its brackets can supply readings for its own requirements. *)
Definition AssumedEntry (W : world) (before : list entry) (tm : subst) (e : entry) : Prop :=
  forall s a, assumed_application e = Some (s, a) ->
    (forall t o, In (t, o) a -> o <> None) /\
    exists Cs needs, CtxOf W s Cs needs /\
      ((c_sub Cs = [] /\ needs = [] /\
        forall t, In t (map fst (c_ty Cs)) -> nlook t (known a) <> None) \/
       exists sigma M th sm,
         MostGeneralMerge sigma before /\ MergeOf before M /\ SectionOf M sigma /\
         let C := entries_cx sigma M tm before in
         let Cv := fold_left (fun C q => set_ty C (fst q) (snd q)) (known a) C in
         (kind_sym W s <> Some DKMeth -> RequirementsInForce Cv Cs) /\
         CallTheta W Cv Cs s th /\ Satisfies Cv Cs th needs sm /\
         AssumptionReadings W Cv Cs th).

Fixpoint OrderedEntries (W : world) (before : list entry) (tm : subst) (es : list entry) : Prop :=
  match es with
  | [] => True
  | e :: rest =>
      AssumedEntry W before tm e /\
      OrderedEntries W (before ++ [e]) (extend_assumed_types tm e) rest
  end.

Definition OrderedItems (W : world) (p : string) (its : list citem) : Prop :=
  exists es tm, ItemsEntries W p [] [] its es tm /\ OrderedEntries W [] [] es.

(* Every declaration's assume nesting forms and its items check in order.
   Functor inputs are read by the same rule. Named contexts are checked at
   each expansion, in the surrounding prefix and bracket bindings. *)
Definition AssumesComplete (W : world) : Prop :=
  forall s k, kind_sym W s = Some k ->
    (exists C ns, AssumeCtx W s C ns) /\
    forall d, w_decl W s = Some d ->
      OrderedItems W (d_mod d) (map assume_spec (d_assumes d)) /\
      match functor_parts (d_ast d) with
      | Some (_, inputs, _, _) => OrderedItems W (d_mod d) (map CISpec inputs)
      | None => True
      end.

(* Criteria section 7: every declaration is checked as written, whether
   or not anything uses it ([D16]'s rule for defined fns, for every kind
   of declaration).  As written means read where it is written, before
   anything is bound: each type symbol stands for itself.  A use reads the
   declaration at its own meanings, checked there as before. *)
Definition as_written (L : list nat) : list (nat * ty) := map (fun t => (t, TAbs t)) L.

(* [D55] a functor with a body binds everything its result promises: each
   leaf of its declared result (ResultLeaf) is bound by an item of the
   body, by written name, as the oracle's functor_total and native check
   it: `bind p = e` and `bind R.m = e` bind their own leaf, and `bind G;`
   binds G's declared result.  A promise is not kept by an item that
   merely reads as it here (`bind Scalar = Int` does not bind
   `Scalar.add`, even where `Int.add` is in force).  For a direct
   application this is a check only: the caller sees the body's binds
   (BApplyBody), not the promise. *)
Inductive BodyBinds (W : world) (p : string) : spec * option aexpr -> nat * option name -> Prop :=
| BBItem : forall pa dn a y s,
    resolve_sym W p pa = Some s -> BodyBinds W p (Spec (Some pa) dn a, Some y) (s, dn)
| BBApply : forall pa dn a g k,
    resolve_sym W p pa = Some g -> kind_sym W g = Some DKFunctor -> ResultLeaf W g k ->
    BodyBinds W p (Spec (Some pa) dn a, None) k.

Definition FunctorTotal (W : world) (u : nat) : Prop :=
  forall d n ps a r bodies k, w_decl W u = Some d -> d_ast d = ADFunctor n ps a r (Some bodies) ->
    ResultLeaf W u k -> exists it, In it (concat bodies) /\ BodyBinds W (d_mod d) it k.

(* `DeclForms W P s`: s's declaration forms as written.
   - A context's items form (ItemsEntries), so a context that nests in a
     cycle, or names an item not in scope, is an error even when nothing
     assumes it (the oracle's context_acyclic; native expands every
     context).
   - A tag's payload and an alias's body denote a type (TyOf), so an
     unbound name, an alias cycle, a repeated record field, or a union
     member that is not nominal or shares a head is an error, used or not
     ([D15]).
   - A val's type, and a fn's or method's signature, denote types; a
     defined val's initializer elaborates (ConstElab).
   - A functor's argument telescope forms (FunCtx) and its result forms
     over it, each other type symbol standing for itself (a body may bind
     a type its result mentions, as lib/wasistd.moss's WasiFloats binds
     Int), and, if it has a body, the body binds its whole result
     (FunctorTotal), applied or not. *)
Definition DeclFormsWith (W : world) (constant : decl -> aty -> aexpr -> Prop) (s : nat) : Prop :=
  forall d, w_decl W s = Some d ->
    match d_ast d with
    | ADCtx _ its => exists L es tm, ItemsEntries W (d_mod d) [] (as_written L) its es tm
    | ADTag _ _ _ t | ADAlias _ t => exists L tau, TyOf W (d_mod d) (as_written L) None t tau
    | ADVal _ t i => (exists L tau, TyOf W (d_mod d) (as_written L) None t tau) /\
                     (forall e, i = Some e -> constant d t e)
    | ADFn RPlain _ _ _ _ => exists L g, SigOf W s (as_written L) None g
    | ADFn _ _ _ _ _ => exists L this g, SigOf W s (as_written L) (Some this) g
    | ADFunctor _ _ _ _ _ => exists C ns L C' ks, FunCtx W s C ns /\
        ProvideResult W s (Cx (c_ty C ++ as_written L) (c_sub C) (c_merge C) (c_provisions C) (c_locals C))
          (fun t => TAbs t) C' ks /\ FunctorTotal W s
    | _ => True
    end.

(* The language items are what the language's rules take them to be
   ([D45]): Bool is a tag with no requirements whose payload is the union of
   its two values, True and False, distinct bare tags. *)
Definition LangItemsFormed (W : world) : Prop :=
  forall symbol, LangItem W LIBool symbol ->
    exists truth falsity,
      LangItem W LITrue truth /\ LangItem W LIFalse falsity /\
      kind_sym W symbol = Some DKTag /\ BareTag W truth /\
      BareTag W falsity /\ truth <> falsity /\ w_reqs W symbol = [] /\
      (PayloadOf W symbol [] (TUnion [TNom truth []; TNom falsity []]) \/
       PayloadOf W symbol [] (TUnion [TNom falsity []; TNom truth []])).

(* The relation.  [D16]: every defined fn is elaborated, reached or not. *)
Definition ElaboratesWith (b : bundle) (W : world) (P : program)
    (function : nat -> Prop) (constant : decl -> aty -> aexpr -> Prop) : Prop :=
  WorldOf b W /\
    (forall f d rcv n ps rt body, w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) ->
       function f) /\
    (* main: the entry's `main`, a defined fn assuming only what the
       platform provides *)
    w_scope W (b_entry b) NSName "main" = Some (ESym (program_main P)) /\
    (exists d n rt body, w_decl W (program_main P) = Some d /\ d_ast d = ADFn RPlain n [] rt (Some body)) /\
    (* main's environment provides exactly main's needs; what each one is,
       the platform's provision, is checked by Linking using Provides. *)
    (exists C ns types platform, CtxOf W (program_main P) C ns /\
       program_environment P = Environment types platform /\ map fst platform = map (rkey_of C) ns) /\
    (* Bool's symbols, when lib/bool.moss is loaded ([D45]) *)
    (LangItem W LIBool (program_bool P) /\ LangItem W LITrue (program_true P) /\
     LangItem W LIFalse (program_false P)
     \/ ((forall s, ~ LangItem W LIBool s) /\ program_bool P = 0 /\ program_true P = 0 /\ program_false P = 0)) /\
    (* the language items are formed: Bool's layout *)
    LangItemsFormed W /\
    (* [D20]: assumed items bring their requirements and needs *)
    AssumesComplete W /\
    (* criteria section 7: every declaration forms as written *)
    forall s, DeclFormsWith W constant s.

Definition record_field_source (n : name) (v : option aexpr) : aexpr :=
  match v with Some e => e | None => AEPath [n] None end.

Definition record_field_expected (expected : option ty) (field : ty) : option ty :=
  match expected with Some _ => Some field | None => None end.

Definition record_field_fits (expected : option ty) (actual field : ty) : Prop :=
  match expected with Some _ => fits actual field | None => actual = field end.

(* In synthesis, a record's fields are ordered as written. Checking uses
   the expected record's field order, just as a tagged record does. *)
Definition record_literal_shape (given : list (fld * option aexpr)) (expected : option ty)
    (fields : list (fld * ty)) : Prop :=
  match expected with
  | None => map fst given = map fst fields
  | Some t => t = TRecord fields /\ NoDup (map fst fields)
  end.

Module Export Typing.

(* Named providers *)

Record named_provider_plan := NamedProviderPlan {
  provider_symbol : nat;
  provider_formal : cx;
  provider_needs : list item;
  provider_arguments : subst
}.

Record TypesNamedProviderCall (W : world) (C : cx) (g : nat) (recv : option ty)
    (want : fsig) (plan : named_provider_plan) : Prop := {
  provider_source_exact : provider_symbol plan = g;
  provider_context_typed : CtxOf W g (provider_formal plan) (provider_needs plan);
  provider_needs_satisfied : NeedsSatisfied C (provider_formal plan)
    (provider_arguments plan) (provider_needs plan);
  provider_call_arguments : CallTheta W C (provider_formal plan) g (provider_arguments plan);
  provider_signature_typed : SigOf W g (c_ty C) recv want;
  provider_requirements_in_force : RequirementsInForce C (provider_formal plan)
}.

Definition named_signature_at (C : cx) (g : fsig) : fsig :=
  FSig (map (canon C) (sg_params g)) (canon C (sg_ret g)).

Definition TypesNamedFunctionPlan (W : world) (F : frame) (C : cx) (path : path)
    (signature : fsig) (plan : named_provider_plan) : Prop :=
  exists g,
    resolve_sym W (fr_mod F) path = Some g /\ kind_sym W g = Some DKFn /\
    (exists n ps rt body, ast_of W g = Some (ADFn RPlain n ps rt (Some body))) /\
    TypesNamedProviderCall W C g None (named_signature_at C signature) plan.

Definition TypesAttachedMethodPlan (W : world) (F : frame) (C : cx) (path : path)
    (name : name) (receiver : ty) (signature : fsig) (site : cx) (plan : named_provider_plan) : Prop :=
  exists root arguments method,
    resolve_sym W (fr_mod F) path = Some root /\ Attached W root name method /\
    (exists rn ps rt body, ast_of W method = Some (ADFn (RAttached rn) name ps rt (Some body))) /\
    canon C receiver = TNom root arguments /\ site = family_context C arguments /\
    TypesNamedProviderCall W site method (Some (canon C receiver)) (named_signature_at C signature) plan.

(* Function and wrapped method providers *)

Inductive function_provider_plan :=
| NamedFunctionProvider (provider : named_provider_plan)
| AbstractFunctionForwarder (occurrence : nat) (handle : forwarder_handle).

Definition TypesFunctionProvider (W : world) (F : frame) (C : cx) (pa : path)
    (signature : fsig) (plan : function_provider_plan) : Prop :=
  match plan with
  | NamedFunctionProvider named => TypesNamedFunctionPlan W F C pa signature named
  | AbstractFunctionForwarder _ handle => TypesForwarder W F C pa signature handle
  end.

Record wrapped_method_plan := WrappedMethodOf {
  wrapped_method_occurrence : nat;
  wrapped_method_provider : named_provider_plan
}.

Definition TypesWrappedMethodPlan (W : world) (F : frame) (C : cx)
    (path : path) (signature : fsig) (plan : wrapped_method_plan) : Prop :=
  TypesNamedFunctionPlan W F C path signature (wrapped_method_provider plan).

(* Linked results and bodies *)

Record linked_result_plan := LinkedResultPlan {
  link_plan_pins : subst;
  link_plan_site : cx;
  link_plan_keys : list item
}.

Record TypesLinkedResult (W : world) (u : nat) (Cu Cg Cb Cr : cx)
    (made outs : list nat) (gen : nat -> nat) (keys : list item) (plan : linked_result_plan) : Prop := {
  linked_plan_result_keys : link_plan_keys plan = keys;
  linked_plan_result_site : link_plan_site plan = link_site Cg Cb (link_plan_pins plan) outs gen;
  linked_plan_pin_provisions : forall k, In k keys ->
    at_site (link_plan_site plan) k <> at_site (link_site Cg Cb [] outs gen) k ->
    ilook (provision_id (at_site (link_plan_site plan) k)) (c_provisions Cg) = None;
  linked_plan_realizes : ResultRealizes (link_plan_site plan) Cr
    (role_reads (link_plan_site plan) outs gen ++ pin_reads Cb (link_plan_pins plan)) keys;
  linked_plan_pins_exact : forall x tx, In (x, tx) (link_plan_pins plan) <->
    Fixed W u x /\ nlook x (c_ty Cb) = Some tx /\
    (In x (map fst (c_ty Cg)) -> nlook x (c_ty Cg) <> Some tx);
  linked_plan_pin_scope : forall x tx y, In (x, tx) (link_plan_pins plan) -> In y (gens tx) ->
    In y (map fst (c_ty Cg)) /\ lookup_subst y (c_sub Cg) = None /\ ~ In y made /\
    (In y (map fst (c_ty Cu)) -> Fixed W u y)
}.

Record linked_body_handle := LinkedBodyOf {
  linked_target_symbol : nat;
  linked_provider_symbol : nat;
  linked_occurrence : nat
}.

Record linked_body_descriptor (BindPlan : Type) := LinkedBodyDescriptor {
  linked_handle : linked_body_handle;
  linked_target_context : cx;
  linked_provider_context : cx;
  linked_output_roles : list nat;
  linked_result_reading : linked_result_plan;
  linked_bind_plan : BindPlan
}.

Arguments LinkedBodyDescriptor {BindPlan}.
Arguments linked_handle {BindPlan}.
Arguments linked_target_context {BindPlan}.
Arguments linked_provider_context {BindPlan}.
Arguments linked_output_roles {BindPlan}.
Arguments linked_result_reading {BindPlan}.
Arguments linked_bind_plan {BindPlan}.

Section LinkedSource.
Variable BindPlan : Type.
Inductive TypesLinkedHeader (W : world) (Fg : frame) (descriptor : linked_body_descriptor BindPlan)
    : list (spec * option aexpr) -> cx -> list nat -> Prop :=
| TLinkedHeader : forall dg name parameters inputs results bodies target_needs provider_needs Cb made gen promised,
    let target := linked_target_symbol (linked_handle descriptor) in
    let provider := linked_provider_symbol (linked_handle descriptor) in
    let Cu := linked_target_context descriptor in
    let Cg := linked_provider_context descriptor in
    let outs := linked_output_roles descriptor in
    let reading := linked_result_reading descriptor in
    w_decl W provider = Some dg -> d_ast dg = ADFunctor name parameters inputs results (Some bodies) ->
    FunCtx W target Cu target_needs -> FunCtx W provider Cg provider_needs ->
    fr_mod Fg = d_mod dg -> fr_ret Fg = None -> fr_this Fg = None -> fr_loop Fg = false ->
    (forall x, In x (fr_pins Fg) <-> Mentions W target x /\ Poly W target x) ->
    (forall t, In t made -> nlook t (c_ty Cu) = None) ->
    TypesLinkOutputs W target Cu Cb outs gen promised (link_plan_keys reading) ->
    TypesLinkedResult W target Cu Cg Cb promised made outs gen (link_plan_keys reading) reading ->
    TypesLinkedHeader W Fg descriptor (concat bodies) Cb made.

End LinkedSource.

(* Lambda providers *)

Definition captured_cx (C : cx) (caps : list name) (capvals : list nat) : cx :=
  fold_left (fun C' q => let '(n, v) := q in
                match slook n (c_locals C) with
                | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                | None => C' end)
            (combine caps capvals) (Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) []).

Definition lambda_parameter_context (C : cx) (parameters : list name) (types : list ty) : cx :=
  fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine parameters types) C.

Record lambda_body_descriptor (BodyPlan : Type) := LambdaBodyOf {
  lambda_occurrence : nat;
  lambda_plan_capture_names : list name;
  lambda_plan_capture_symbols : list nat;
  lambda_plan_capture_context : cx;
  lambda_source_body : BodyPlan;
  lambda_provision_needs : list item
}.

Arguments LambdaBodyOf {BodyPlan}.
Arguments lambda_occurrence {BodyPlan}.
Arguments lambda_plan_capture_names {BodyPlan}.
Arguments lambda_plan_capture_symbols {BodyPlan}.
Arguments lambda_plan_capture_context {BodyPlan}.
Arguments lambda_source_body {BodyPlan}.
Arguments lambda_provision_needs {BodyPlan}.

Definition lambda_descriptor_captures {BodyPlan : Type}
    (descriptor : lambda_body_descriptor BodyPlan) : list (name * nat) :=
  combine (lambda_plan_capture_names descriptor) (lambda_plan_capture_symbols descriptor).

Section LambdaSource.
Variable BodyPlan : Type.

Definition lambda_descriptor_site (signature : fsig) (ps : list (name * option aty))
    (descriptor : lambda_body_descriptor BodyPlan) : cx :=
  lambda_parameter_context (lambda_plan_capture_context descriptor) (map fst ps) (sg_params signature).

Inductive TypesLambdaHeader (W : world) (F : frame) (C : cx)
    (ps : list (name * option aty)) (rt : option aty) (body : aexpr) (signature : fsig)
    (this : option ty) (Fl : frame) (descriptor : lambda_body_descriptor BodyPlan) (result : ty) : Prop :=
| TLambdaHeader :
    let caps := lambda_plan_capture_names descriptor in
    let capvals := lambda_plan_capture_symbols descriptor in
    let Cl := lambda_plan_capture_context descriptor in
    length ps = length (sg_params signature) -> NoDup (map fst ps) ->
    Forall2 (fun parameter t =>
      match snd parameter with
      | Some annotation => exists read,
          TyOf W (fr_mod F) (c_ty C) this annotation read /\ MergeEq (c_merge C) read t
      | None => True
      end) ps (sg_params signature) ->
    (match rt with
     | Some annotation => exists read,
         TyOf W (fr_mod F) (c_ty C) this annotation read /\ MergeEq (c_merge C) read (sg_ret signature)
     | None => True
     end) ->
    TypesLambdaCaptures W F C (map fst ps) body caps capvals ->
    Cl = captured_cx C caps capvals ->
    fr_mod Fl = fr_mod F -> fr_ret Fl = Some (sg_ret signature) -> fr_this Fl = this -> fr_loop Fl = false ->
    incl (fr_fresh F ++ context_gens C) (fr_fresh Fl) ->
    Forall (Named Fl) caps -> Forall (Named F) caps ->
    fits result (sg_ret signature) ->
    (forall k, In k (lambda_provision_needs descriptor) <->
      provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = provision_id k) ->
    NoDup (lambda_provision_needs descriptor) ->
    TypesLambdaHeader W F C ps rt body signature this Fl descriptor result.

End LambdaSource.

(* Method and literal typing *)

Inductive TypesMethodProvision (W : world) (C : cx) (receiver : ty) (method : nat)
    : option family_use -> fsig -> Prop :=
| TypesMethodIn : forall g,
    mlook receiver method (c_mths C) = Some g ->
    TypesMethodProvision W C receiver method None g
| TypesMethodFamily : forall u matching arguments formal needs result keys g,
    mlook receiver method (c_mths C) = None ->
    TypesFamilyApplication W C u matching arguments formal needs result keys ->
    FamilyReadOff W C u receiver method matching ->
    In (mkey receiver method) keys -> OnlyFamily W C u (mkey receiver method) ->
    mlook receiver method (c_mths result) = Some g -> MatchingInForce C matching ->
    TypesMethodProvision W C receiver method
      (Some (FamilyUse u matching arguments formal needs result keys)) g.

Inductive method_call_plan : Type :=
| SelectedProvision (method : nat) (family : option family_use)
| SelectedDefinition (method : nat) (formal : cx) (needs : list item)
    (types : subst) (demands : list family_use) (result : cx).

Inductive TypesMethodSelection (W : world) (F : frame) (C : cx) (receiver : ty)
    : path -> method_call_plan -> fsig -> Prop :=
| TypesMethodQualified : forall path m choice g,
    List.length path > 1 -> resolve_detached W (fr_mod F) path = Some m ->
    TypesMethodProvision W C receiver m choice g ->
    TypesMethodSelection W F C receiver path (SelectedProvision m choice) g
| TypesMethodProvisionWins : forall name h attached m choice g,
    head receiver = Some h -> Attached W h name attached ->
    w_scope W (fr_mod F) NSDetached name = Some (ESym m) ->
    mlook receiver m (c_mths C) <> None ->
    TypesMethodProvision W C receiver m choice g ->
    TypesMethodSelection W F C receiver [name] (SelectedProvision m choice) g
| TypesMethodAttachedDefined : forall name h attached formal needs types g demands result,
    head receiver = Some h -> Attached W h name attached ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached name = Some (ESym m) /\
      mlook receiver m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W attached = Some (ADFn (RAttached rn) name ps rt (Some b))) ->
    let receiver_types := match receiver with TNom _ rows => rows | _ => [] end in
    let caller := family_context C receiver_types in
    CtxOf W attached formal needs ->
    Demand W caller (map (at_th caller types) needs) [] caller demands result ->
    NeedsSatisfied result formal types needs -> CallTheta W result formal attached types ->
    SigOf W attached (c_ty result) (Some receiver) g ->
    RequirementsInForce caller formal ->
    TypesMethodSelection W F C receiver [name]
      (SelectedDefinition attached formal needs types demands result) g
| TypesMethodAttachedAbstract : forall name h attached choice g,
    head receiver = Some h -> Attached W h name attached ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached name = Some (ESym m) /\
      mlook receiver m (c_mths C) <> None) ->
    (exists rn ps rt, ast_of W attached = Some (ADFn (RAttached rn) name ps rt None)) ->
    TypesMethodProvision W C receiver attached choice g ->
    TypesMethodSelection W F C receiver [name] (SelectedProvision attached choice) g
| TypesMethodDetached : forall name m choice g,
    (forall h attached, head receiver = Some h -> ~ Attached W h name attached) ->
    w_scope W (fr_mod F) NSDetached name = Some (ESym m) ->
    TypesMethodProvision W C receiver m choice g ->
    TypesMethodSelection W F C receiver [name] (SelectedProvision m choice) g.

Inductive literal_plan : Type :=
| PlanLiteralValue (value : nat)
| PlanLiteralArithmetic (operand_type : ty) (method : nat) (family : option family_use)
    (left right : literal_plan)
| PlanLiteralFloat (function : nat) (operand : literal_plan).

Inductive literal_arithmetic : Type := LiteralAddition | LiteralMultiplication | LiteralDivision.

Definition arithmetic_tree (operation : literal_arithmetic) : literal_tree -> literal_tree -> literal_tree :=
  match operation with
  | LiteralAddition => LAdd
  | LiteralMultiplication => LMul
  | LiteralDivision => LDiv
  end.

Definition arithmetic_item (operation : literal_arithmetic) : lang_item :=
  match operation with
  | LiteralAddition => LIAdd
  | LiteralMultiplication => LIMul
  | LiteralDivision => LIDiv
  end.

Inductive TypesLiteral (W : world) (C : cx) : literal_tree -> literal_plan -> ty -> Prop :=
| TypesLiteralZero : forall z t,
    LangItem W LIZero z -> nlook z (c_vals C) = Some t ->
    TypesLiteral W C LZero (PlanLiteralValue z) t
| TypesLiteralOne : forall o t,
    LangItem W LIOne o -> nlook o (c_vals C) = Some t ->
    TypesLiteral W C LOne (PlanLiteralValue o) t
| TypesLiteralArithmetic : forall operation left right x y t method family,
    TypesLiteral W C left x t -> TypesLiteral W C right y t ->
    LangItem W (arithmetic_item operation) method ->
    TypesMethodProvision W C t method family (FSig [t] t) ->
    TypesLiteral W C (arithmetic_tree operation left right) (PlanLiteralArithmetic t method family x y) t
| TypesLiteralFloat : forall tree operand actual expected t f,
    TypesLiteral W C tree operand actual -> LangItem W LIFloat f ->
    nlook f (c_fns C) = Some (FSig [expected] t) -> fits actual expected ->
    TypesLiteral W C (LFloat tree) (PlanLiteralFloat f operand) t.

Unset Implicit Arguments.

(* Local bindings and patterns *)

Inductive field_pattern_plan : Type :=
| PatternPun (field : fld) (field_type : ty)
| PatternIgnore (field : fld)
| PatternRename (field : fld) (binder : name) (field_type : ty).

Definition field_pattern_bindings (fields : list field_pattern_plan) : list (name * ty) :=
  flat_map (fun field => match field with
    | PatternPun n t | PatternRename _ n t => [(n, t)]
    | PatternIgnore _ => []
    end) fields.

Inductive TypesFieldPatterns (W : world) (F : frame) (fields : list (fld * ty))
    : list (name * option apat) -> list field_pattern_plan -> Prop :=
| TypesFieldsEmpty : TypesFieldPatterns W F fields [] []
| TypesFieldsPun : forall n t rest plan,
    slook n fields = Some t -> Named F n -> TypesFieldPatterns W F fields rest plan ->
    TypesFieldPatterns W F fields ((n, None) :: rest) (PatternPun n t :: plan)
| TypesFieldsIgnore : forall n t rest plan,
    slook n fields = Some t -> TypesFieldPatterns W F fields rest plan ->
    TypesFieldPatterns W F fields ((n, Some APWild) :: rest) (PatternIgnore n :: plan)
| TypesFieldsRename : forall n binder t rest plan,
    slook n fields = Some t -> resolve W (fr_mod F) [binder] = None -> Named F binder ->
    TypesFieldPatterns W F fields rest plan ->
    TypesFieldPatterns W F fields ((n, Some (APPath [binder])) :: rest) (PatternRename n binder t :: plan).

Inductive pattern_plan : Type :=
| PatternWildcard
| PatternBinder (binder : name) (binder_type : ty)
| PatternNominal (tag : nat)
| PatternPayload (tag : nat) (binder : name) (payload : ty)
| PatternRecord (tag : nat) (fields : list field_pattern_plan).

Definition pattern_bindings (plan : pattern_plan) : list (name * ty) :=
  match plan with
  | PatternWildcard | PatternNominal _ => []
  | PatternBinder n t | PatternPayload _ n t => [(n, t)]
  | PatternRecord _ fields => field_pattern_bindings fields
  end.

Definition pattern_heads (plan : pattern_plan) : list nat :=
  match plan with
  | PatternNominal tag | PatternPayload tag _ _ | PatternRecord tag _ => [tag]
  | PatternWildcard | PatternBinder _ _ => []
  end.

Definition pattern_catches (plan : pattern_plan) : bool :=
  match plan with PatternWildcard | PatternBinder _ _ => true | _ => false end.

Definition pattern_scope (C : cx) (plan : pattern_plan) : cx :=
  fold_left (fun C q => add_local C (fst q) (snd q) LLet) (pattern_bindings plan) C.

(* A positional pattern lists every element. Named record patterns may
   select a subset of the fields. Only tuple syntax produces its labels. *)
Definition record_pattern_arity (given : list (name * option apat)) (fields : list (fld * ty)) : bool :=
  match given with
  | (first, _) :: _ =>
      if String.eqb first (Syntax.tuple_field_name 0) then Nat.eqb (List.length given) (List.length fields) else true
  | [] => true
  end.

Definition tag_binding_shape (W : world) (tag : nat) (arity : option nat) : bool :=
  match ast_of W tag, arity with
  | Some (ADBareTag _), Some 0 => true
  | Some (ADTag _ _ None _), None => true
  | Some (ADTag _ _ (Some expected) _), Some given => Nat.eqb given expected
  | _, _ => false
  end.

Definition TagOpening (W : world) (scrutinee : ty) (tag : nat) (arguments : subst)
    (opening : option (option nat)) : Prop :=
  match opening with
  | None => ~ BareTag W tag
  | Some arity => scrutinee = TNom tag arguments /\ tag_binding_shape W tag arity = true
  end.

Inductive TypesPattern (W : world) (F : frame) (scrutinee : ty) : apat -> pattern_plan -> Prop :=
| TypesPatternWildcard : TypesPattern W F scrutinee APWild PatternWildcard
| TypesPatternBinder : forall n,
    resolve W (fr_mod F) [n] = None -> Named F n ->
    TypesPattern W F scrutinee (APPath [n]) (PatternBinder n scrutinee)
| TypesPatternNominal : forall path tag arguments,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
    In (TNom tag arguments) (members scrutinee) ->
    TypesPattern W F scrutinee (APPath path) (PatternNominal tag)
| TypesPatternPayload : forall path tag arguments binder payload opening,
    resolve_sym W (fr_mod F) path = Some tag -> TagOpening W scrutinee tag arguments opening ->
    In (TNom tag arguments) (members scrutinee) -> PayloadOf W tag arguments payload ->
    resolve W (fr_mod F) [binder] = None -> Named F binder ->
    TypesPattern W F scrutinee (APTag path (APPath [binder]) opening) (PatternPayload tag binder payload)
| TypesPatternRecord : forall path tag arguments fields field_types plan,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
    In (TNom tag arguments) (members scrutinee) -> PayloadOf W tag arguments (TRecord field_types) ->
    record_pattern_arity fields field_types = true ->
    TypesFieldPatterns W F field_types fields plan -> NoDup (map fst (field_pattern_bindings plan)) ->
    TypesPattern W F scrutinee (APRec (Some path) fields) (PatternRecord tag plan).

(* Expressions, blocks and bindings *)

Inductive expression_plan : Type :=
| PlanUnit
| PlanThis
| PlanLocal (n : name)
| PlanBlock (body : block_plan)
| PlanReturnUnit
| PlanReturn (result : expression_plan)
| PlanBreak
| PlanDefinedCall (f : nat) (formal : cx) (needs : list item)
    (arguments : subst) (demands : list family_use) (call_site : cx) (args : list expression_plan)
| PlanAbstractCall (f : nat) (args : list expression_plan)
| PlanTag (tag : nat) (arguments : subst) (payload : expression_plan)
| PlanBareRecord (fields : list (fld * expression_plan))
| PlanRecord (tag : nat) (arguments : subst) (fields : list (fld * expression_plan))
| PlanField (object : expression_plan) (field : fld)
| PlanMatch (scrutinee : expression_plan) (arms : list (pattern_plan * expression_plan))
| PlanMethod (receiver : ty) (selected : method_call_plan) (object : expression_plan)
    (args : list expression_plan)
| PlanOperator (receiver : ty) (method : nat) (family : option family_use) (object : expression_plan)
    (args : list expression_plan)
| PlanNumeric (literal : literal_plan)
| PlanGlobalValue (symbol : nat)
(* A constant's initializer has its own numbering, recorded as a layout
   witness so the bridge can reproduce the exact code at each occurrence. *)
| PlanConstant (initializer_layout : frame) (initializer : expression_plan)
| PlanBracketedCall (expansion : expression_plan)
with block_plan : Type :=
| PlanEmpty
| PlanTail (result : expression_plan)
| PlanLet (n : name) (declared : ty) (mode : lmut) (initializer : expression_plan) (rest : block_plan)
| PlanAssign (n : name) (value : expression_plan) (rest : block_plan)
| PlanExprLast (value : expression_plan)
| PlanExprThen (value : expression_plan) (rest : block_plan)
| PlanWhile (condition : expression_plan) (body rest : block_plan)
| PlanLoop (body rest : block_plan)
| PlanLoopLast (body : block_plan)
| PlanBind (items : list binding_plan) (result : cx) (rest : block_plan)
with binding_plan : Type :=
| PlanTypeBind (symbol : nat) (meaning : ty)
| PlanValueBind (site : cx) (symbol : nat) (initializer : expression_plan)
| PlanInlineApply (items : list binding_plan)
| PlanAbstractApply (site : cx) (application : application)
| PlanFunctorCopy (target provider : nat) (target_context provider_context : cx)
    (selected : item -> item) (needs : list item) (positions : list nat)
| PlanFunctionBind (site : cx) (symbol : nat) (signature : fsig) (provider : function_provider_plan)
| PlanAttachedBind (receiver : ty) (method : nat) (arguments : subst) (site : cx)
    (provider : named_provider_plan)
| PlanWrappedBind (receiver : ty) (method : nat) (arguments : subst) (site : cx)
    (signature : fsig) (provider : wrapped_method_plan)
| PlanFunctionLambda (site : cx) (symbol : nat) (signature : fsig) (parameters : list name)
    (layout : frame) (provider : lambda_body_descriptor block_plan)
| PlanMethodLambda (receiver : ty) (method : nat) (arguments : subst) (site : cx) (signature : fsig)
    (parameters : list name) (layout : frame) (provider : lambda_body_descriptor block_plan)
| PlanFunctorClosure (site : cx) (layout : frame) (body : linked_body_descriptor (list binding_plan))
    (selected : item -> option item) (provider_needs : list item).

Definition TypesMethodBindHeader (W : world) (F : frame) (C : cx)
    (path : path) (label : name) (brackets : sapp) (method : nat) (receiver : ty)
    (signature : fsig) (arguments : inst) : Prop :=
  exists receiver_symbol readings,
    resolve_sym W (fr_mod F) path = Some receiver_symbol /\
    MethodNamed W (fr_mod F) receiver_symbol label method /\
    Brackets W (fr_mod F) (c_ty C) method brackets readings /\
    TyOf W (fr_mod F) (readings ++ c_ty C) None (ATRef path None) receiver /\
    SigOf W method (readings ++ c_ty C) (Some receiver) signature /\
    AppOf W method (readings ++ c_ty C) arguments /\
    (forall u, ~ FamilyProduces W C u (mkey (canon C receiver) method)) /\
    ~ (exists rc n ps rt b, ast_of W method = Some (ADFn rc n ps rt (Some b))).

Inductive binding_activity : Type :=
| BindValueActivity
| BindApplyActivity (family : nat)
| BindOtherActivity.

Fixpoint binding_plan_activities (plan : binding_plan) : list binding_activity :=
  match plan with
  | PlanValueBind _ _ _ => [BindValueActivity]
  | PlanAbstractApply _ application => [BindApplyActivity (app_functor application)]
  | PlanInlineApply items => flat_map binding_plan_activities items
  | PlanFunctionLambda _ _ _ _ _ provider | PlanMethodLambda _ _ _ _ _ _ _ provider =>
      map (fun _ => BindValueActivity) (lambda_descriptor_captures provider) ++ [BindOtherActivity]
  | _ => [BindOtherActivity]
  end.

Definition allowed_family_activity (W : world) (activity : binding_activity) : Prop :=
  match activity with
  | BindValueActivity => False
  | BindApplyActivity family => Family W family
  | BindOtherActivity => True
  end.

Definition FamilyBindPlans (W : world) (plans : list binding_plan) : Prop :=
  Forall (allowed_family_activity W) (flat_map binding_plan_activities plans).

Definition closure_arguments (C formal : cx) : subst :=
  map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty formal).

Definition TagCallPayload (W : world) (tag : nat) (args : list aexpr) (payload : aexpr) : Prop :=
  exists name parameters arity syntax,
    ast_of W tag = Some (ADTag name parameters arity syntax) /\
    Syntax.tag_call_payload arity args = Some payload.

(* Checking a conditional produces the same plans as two ordinary matches.
   The condition is checked at Bool before the inner match unpacks it. *)
Definition conditional_plan (bool_tag true_tag false_tag : nat)
    (condition : expression_plan) (then_body : block_plan)
    (otherwise : option expression_plan) : expression_plan :=
  let name := conditional_payload_name in
  let payload := TUnion [TNom false_tag []; TNom true_tag []] in
  PlanMatch (PlanMatch condition [(PatternPayload bool_tag name payload, PlanLocal name)])
    [(PatternNominal true_tag, PlanBlock then_body);
     (PatternNominal false_tag, match otherwise with Some plan => plan | None => PlanUnit end)].

Inductive TypesExpr (W : world) : frame -> cx -> aexpr -> option ty -> expression_plan -> ty -> Prop :=
| TypesUnit : forall F C exp, TypesExpr W F C AEUnit exp PlanUnit TUnit
| TypesThis : forall F C exp t, fr_this F = Some t -> TypesExpr W F C AEThis exp PlanThis t
| TypesLocal : forall F C exp n t mu,
    slook n (c_locals C) = Some (t, mu) -> Named F n ->
    TypesExpr W F C (AEPath [n] None) exp (PlanLocal n) t
| TypesBlock : forall F C exp b plan t,
    TypesBlkE W F C b exp plan t ->
    TypesExpr W F C (AEBlock b) exp (PlanBlock plan) (match exp with Some e => e | None => t end)
| TypesIfThen : forall F C exp c b cp bp bt bool_tag true_tag false_tag,
    BooleanTags W bool_tag true_tag false_tag ->
    TypesCond W F C c cp -> TypesBlkE W F C b exp bp bt ->
    opt_fits TUnit exp -> (bt = TUnit \/ bt = never) ->
    TypesExpr W F C (AEIf c b None) exp (conditional_plan bool_tag true_tag false_tag cp bp None) TUnit
| TypesIfElse : forall F C exp c b e cp bp bt ep et t bool_tag true_tag false_tag,
    BooleanTags W bool_tag true_tag false_tag ->
    TypesCond W F C c cp -> TypesBlkE W F C b exp bp bt ->
    TypesBranch W F C e exp ep et -> opt_fits et exp -> Join (branch_ty b bt) et exp t ->
    TypesExpr W F C (AEIf c b (Some e)) exp (conditional_plan bool_tag true_tag false_tag cp bp (Some ep)) t
| TypesReturnUnit : forall F C exp, fr_ret F = Some TUnit ->
    TypesExpr W F C (AEReturn None) exp PlanReturnUnit never
| TypesReturn : forall F C exp e expected plan actual,
    fr_ret F = Some expected ->
    TypesExpr W F C e (Some expected) plan actual -> fits actual expected ->
    TypesExpr W F C (AEReturn (Some e)) exp (PlanReturn plan) never
| TypesBreak : forall F C exp, fr_loop F = true -> TypesExpr W F C AEBreak exp PlanBreak never
| TypesDefinedCall : forall F C exp pa args f formal needs arguments g plans demands call_site,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f formal needs ->
    Demand W C (map (at_th C arguments) needs) [] C demands call_site ->
    NeedsSatisfied call_site formal arguments needs -> CallTheta W call_site formal f arguments ->
    SigOf W f (c_ty call_site) None g ->
    TypesArgs W F C args (sg_params g) plans ->
    RequirementsInForce C formal ->
    TypesExpr W F C (AECall pa None args) exp
      (PlanDefinedCall f formal needs arguments demands call_site plans) (sg_ret g)
| TypesAbstractCall : forall F C exp pa args f g plans,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt, ast_of W f = Some (ADFn RPlain n ps rt None)) ->
    nlook f (c_fns C) = Some g -> TypesArgs W F C args (sg_params g) plans ->
    TypesExpr W F C (AECall pa None args) exp (PlanAbstractCall f plans) (sg_ret g)
| TypesBareTag : forall F C exp path tag,
    (forall n, path = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) path = Some tag -> BareTag W tag ->
    TypesExpr W F C (AEPath path None) exp (PlanTag tag [] PlanUnit) (TNom tag [])
| TypesTag : forall F C exp path brackets args payload tag readings arguments payload_type plan actual,
    resolve_sym W (fr_mod F) path = Some tag -> ~ BareTag W tag ->
    Brackets W (fr_mod F) (c_ty C) tag brackets readings ->
    Forall2 (fun r q => fst q = r /\ nlook r (readings ++ c_ty C) = Some (snd q)) (w_reqs W tag) arguments ->
    PayloadOf W tag arguments payload_type -> TagCallPayload W tag args payload ->
    TypesExpr W F C payload (Some payload_type) plan actual -> fits actual payload_type ->
    TypesExpr W F C (AECall path brackets args) exp (PlanTag tag arguments plan) (TNom tag arguments)
| TypesRecord : forall F C exp path brackets given tag readings arguments fields plans,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) tag brackets readings ->
    Forall2 (fun r q => fst q = r /\ nlook r (readings ++ c_ty C) = Some (snd q)) (w_reqs W tag) arguments ->
    PayloadOf W tag arguments (TRecord fields) ->
    NoDup (map fst given) -> List.length given = List.length fields ->
    TypesRecordFields W F C given (Some (TRecord fields)) fields plans ->
    TypesExpr W F C (AERec path brackets given) exp (PlanRecord tag arguments plans) (TNom tag arguments)
| TypesBareRecord : forall F C exp given fields plans,
    NoDup (map fst given) -> List.length given = List.length fields ->
    record_literal_shape given exp fields ->
    TypesRecordFields W F C given exp fields plans ->
    TypesExpr W F C (AEBareRecord given) exp (PlanBareRecord plans) (TRecord fields)
| TypesField : forall F C exp object field plan actual payload t bare,
    TypesExpr W F C object None plan actual ->
    ((bare = false /\ exists tag arguments, actual = TNom tag arguments /\ kind_sym W tag = Some DKTag /\
      PayloadOf W tag arguments payload) \/ payload = actual) ->
    (exists fields, payload = TRecord fields /\ slook field fields = Some t) ->
    TypesExpr W F C (AEField object field bare) exp (PlanField plan field) t
| TypesMatch : forall F C exp scrutinee arms plan st plans t,
    TypesExpr W F C scrutinee None plan st -> TypesArms W F C (canon C st) arms exp plans t ->
    TypesExpr W F C (AEMatch scrutinee arms) exp (PlanMatch plan plans) t
| TypesMethod : forall F C exp object path args receiver object_type selected signature plans,
    TypesExpr W F C object None receiver object_type ->
    TypesMethodSelection W F C (canon C object_type) path selected signature ->
    TypesArgs W F C args (sg_params signature) plans ->
    TypesExpr W F C (AEMeth object path args) exp
      (PlanMethod (canon C object_type) selected receiver plans) (sg_ret signature)
| TypesUnary : forall F C exp op operand lib name method receiver object_type family signature plans,
    operator_method op true = Some (lib, name) -> LangSym W lib NSDetached name method ->
    TypesExpr W F C operand None receiver object_type -> head (canon C object_type) <> None ->
    TypesMethodProvision W C (canon C object_type) method family signature ->
    TypesArgs W F C [] (sg_params signature) plans ->
    TypesExpr W F C (AEUnary op operand) exp
      (PlanOperator (canon C object_type) method family receiver plans) (sg_ret signature)
| TypesBinary : forall F C exp op left right lib name method receiver object_type family signature plans,
    operator_method op false = Some (lib, name) -> LangSym W lib NSDetached name method ->
    TypesExpr W F C left None receiver object_type -> head (canon C object_type) <> None ->
    TypesMethodProvision W C (canon C object_type) method family signature ->
    TypesArgs W F C [right] (sg_params signature) plans ->
    TypesExpr W F C (AEBinary op left right) exp
      (PlanOperator (canon C object_type) method family receiver plans) (sg_ret signature)
| TypesNumeric : forall F C exp text plan t,
    NumericText text -> TypesLiteral W C (literal_tree_of text) plan t ->
    TypesExpr W F C (AELit text) exp (PlanNumeric plan) t
| TypesGlobalValue : forall F C exp path symbol d name annotation t,
    (forall n, path = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADVal name annotation None -> nlook symbol (c_vals C) = Some t ->
    TypesExpr W F C (AEPath path None) exp (PlanGlobalValue symbol) t
| TypesConstant : forall F C exp path symbol d name annotation source t layout plan actual,
    (forall n, path = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADVal name annotation (Some source) -> d_assumes d = [] ->
    TyOf W (d_mod d) [] None annotation t ->
    TypesExpr W layout (Cx [] [] [] [] []) source (Some t) plan actual -> fits actual t ->
    fr_mod layout = d_mod d -> fr_ret layout = None -> fr_this layout = None -> fr_loop layout = false ->
    fr_fresh layout = [] -> fr_applying layout = [] -> fr_pins layout = [] ->
    TypesExpr W F C (AEPath path None) exp (PlanConstant layout plan) t
| TypesBracketedCall : forall F C exp path brackets args f expansion t,
    resolve_sym W (fr_mod F) path = Some f -> kind_sym W f = Some DKFn ->
    CallBrackets W (fr_mod F) f brackets ->
    TypesExpr W F C (Syntax.call_block path brackets args) None expansion t ->
    TypesExpr W F C (AECall path (Some brackets) args) exp (PlanBracketedCall expansion) t
with TypesBlkE (W : world) : frame -> cx -> ablock -> option ty -> block_plan -> ty -> Prop :=
| TypesBlkCheck : forall F C ss tl exp plan t made,
    TypesBlk W (without_pins F) C ss tl exp plan t made ->
    (forall g, In g made -> ~ In g (gens t)) -> opt_fits t exp ->
    TypesBlkE W F C (ABlock ss tl) exp plan t
with TypesBlk (W : world) : frame -> cx -> list astmt -> option aexpr -> option ty -> block_plan -> ty -> list nat -> Prop :=
| TypesTail : forall F C y exp plan t,
    TypesExpr W F C y exp plan t -> TypesBlk W F C [] (Some y) exp (PlanTail plan) t []
| TypesEmpty : forall F C exp, TypesBlk W F C [] None exp PlanEmpty TUnit []
| TypesLet : forall F C st n ann y rest tl exp declared v vt t0 cont t made mu,
    LetStmt st n ann y mu ->
    (match ann with
    | Some a => exists d, TyOf W (fr_mod F) (c_ty C) (fr_this F) a d /\ declared = Some d
    | None => declared = None end) ->
    TypesExpr W F C y declared v vt ->
    (match declared with Some d => fits vt d /\ t0 = d | None => t0 = vt end) ->
    Named F n ->
    TypesBlk W F (add_local C n t0 mu) rest tl exp cont t made ->
    TypesBlk W F C (st :: rest) tl exp (PlanLet n t0 mu v cont) t made
| TypesAssign : forall F C n y rest tl exp tv v vt cont t made,
    slook n (c_locals C) = Some (tv, LVar) ->
    TypesExpr W F C y (Some tv) v vt -> fits vt tv ->
    TypesBlk W F C rest tl exp cont t made -> Named F n ->
    TypesBlk W F C (ASAssign n y :: rest) tl exp (PlanAssign n v cont) t made
| TypesExprLast : forall F C y exp v vt,
    TypesExpr W F C y None v vt ->
    TypesBlk W F C [ASExpr y] None exp (PlanExprLast v) (if ty_eqb vt never then never else TUnit) []
| TypesExprThen : forall F C y rest tl exp v vt cont t made,
    (rest <> [] \/ tl <> None) ->
    TypesExpr W F C y None v vt -> TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASExpr y :: rest) tl exp (PlanExprThen v cont) t made
| TypesWhile : forall F C c b rest tl exp cp bp bt cont t made,
    TypesCond W F C c cp -> TypesBlkE W (with_loop F) C b None bp bt ->
    TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASWhile c b :: rest) tl exp (PlanWhile cp bp cont) t made
| TypesLoop : forall F C b rest tl exp bp bt cont t made,
    (rest <> [] \/ tl <> None \/ BlockBreaks W (fr_mod F) b) ->
    TypesBlkE W (with_loop F) C b None bp bt ->
    TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASLoop b :: rest) tl exp (PlanLoop bp cont) t made
| TypesLoopLast : forall F C b exp bp bt,
    TypesBlkE W (with_loop F) C b None bp bt -> ~ BlockBreaks W (fr_mod F) b ->
    TypesBlk W F C [ASLoop b] None exp (PlanLoopLast bp) never []
| TypesBindBlock : forall F C items rest tail exp plans result F' continuation t made created,
    TypesBindItems W F C items plans result F' made ->
    TypesBlk W F' result rest tail exp continuation t created ->
    TypesBlk W F C (ASBind items :: rest) tail exp (PlanBind plans result continuation) t (made ++ created)
with TypesArgs (W : world) : frame -> cx -> list aexpr -> list ty -> list expression_plan -> Prop :=
| TypesArgsNil : forall F C, TypesArgs W F C [] [] []
| TypesArgsCons : forall F C a rest t ts plan plans actual,
    TypesExpr W F C a (Some t) plan actual -> fits (canon C actual) (canon C t) ->
    TypesArgs W F C rest ts plans -> TypesArgs W F C (a :: rest) (t :: ts) (plan :: plans)
with TypesCond (W : world) : frame -> cx -> aexpr -> expression_plan -> Prop :=
| TypesBool : forall F C c plan t b,
    LangItem W LIBool b -> TypesExpr W F C c (Some (TNom b [])) plan t ->
    fits t (TNom b []) -> TypesCond W F C c plan
with TypesBranch (W : world) : frame -> cx -> aexpr -> option ty -> expression_plan -> ty -> Prop :=
| TypesBranchBlock : forall F C b exp plan t,
    TypesBlkE W F C b exp plan t ->
    TypesBranch W F C (AEBlock b) exp (PlanBlock plan) (match exp with Some e => e | None => branch_ty b t end)
| TypesBranchExpr : forall F C e exp plan t,
    (forall b, e <> AEBlock b) -> TypesExpr W F C e exp plan t ->
    TypesBranch W F C e exp plan t
with TypesRecordFields (W : world) : frame -> cx -> list (name * option aexpr) ->
    option ty -> list (fld * ty) -> list (fld * expression_plan) -> Prop :=
| TypesRecordFieldsEmpty : forall F C given exp, TypesRecordFields W F C given exp [] []
| TypesRecordFieldsCons : forall F C given exp n t syntax plan actual rest plans,
    In (n, syntax) given ->
    TypesExpr W F C (record_field_source n syntax) (record_field_expected exp t) plan actual ->
    record_field_fits exp actual t -> TypesRecordFields W F C given exp rest plans ->
    TypesRecordFields W F C given exp ((n, t) :: rest) ((n, plan) :: plans)
with TypesArms (W : world) : frame -> cx -> ty -> list (apat * aexpr) ->
    option ty -> list (pattern_plan * expression_plan) -> ty -> Prop :=
| TypesArmsEmpty : forall F C exp,
    TypesArms W F C never [] exp [] (match exp with Some e => e | None => never end)
| TypesArmsSome : forall F C st arms exp plans covered catch t,
    st <> never -> TypesArmList W F C st arms exp plans covered catch t ->
    (catch = true \/ (members st <> [] /\ forall m h, In m (members st) -> head m = Some h -> In h covered)) ->
    TypesArms W F C st arms exp plans t
with TypesArmList (W : world) : frame -> cx -> ty -> list (apat * aexpr) ->
    option ty -> list (pattern_plan * expression_plan) -> list nat -> bool -> ty -> Prop :=
| TypesArmOne : forall F C st pat body exp pattern plan t,
    TypesPattern W F st pat pattern -> TypesBranch W F (pattern_scope C pattern) body exp plan t ->
    opt_fits t exp ->
    TypesArmList W F C st [(pat, body)] exp [(pattern, plan)] (pattern_heads pattern) (pattern_catches pattern) t
| TypesArmMore : forall F C st pat body rest exp pattern plan bt plans covered catch rt t,
    rest <> [] ->
    TypesPattern W F st pat pattern -> TypesBranch W F (pattern_scope C pattern) body exp plan bt ->
    opt_fits bt exp -> TypesArmList W F C st rest exp plans covered catch rt -> Join bt rt exp t ->
    TypesArmList W F C st ((pat, body) :: rest) exp ((pattern, plan) :: plans)
      (pattern_heads pattern ++ covered) (pattern_catches pattern || catch) t
with TypesBindItems (W : world) : frame -> cx -> list (spec * option aexpr) ->
    list binding_plan -> cx -> frame -> list nat -> Prop :=
| TypesBindEmpty : forall F C, TypesBindItems W F C [] [] C F []
| TypesBindCons : forall F C item rest plan plans middle F1 made1 result F2 made2,
    TypesBindItem W F C item plan middle F1 made1 ->
    TypesBindItems W F1 middle rest plans result F2 made2 ->
    TypesBindItems W F C (item :: rest) (plan :: plans) result F2 (made1 ++ made2)
with TypesBindItem (W : world) : frame -> cx -> (spec * option aexpr) ->
    binding_plan -> cx -> frame -> list nat -> Prop :=
| TypesBindType : forall F C path symbol right arguments meaning,
    resolve_sym W (fr_mod F) path = Some symbol -> kind_sym W symbol = Some DKType ->
    ~ In symbol (fr_pins F) ->
    TyOf W (fr_mod F) (c_ty C) None (ATRef right arguments) meaning ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath right arguments))
      (PlanTypeBind symbol (canon C meaning)) (set_ty C symbol (canon C meaning)) F []
| TypesBindValue : forall F C path symbol d name annotation initializer declared source plan actual arguments,
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADVal name annotation initializer ->
    TyOf W (d_mod d) (c_ty C) None annotation declared ->
    TypesExpr W F C source (Some declared) plan actual -> fits actual declared ->
    AppOf W symbol (c_ty C) arguments ->
    TypesBindItem W F C (Spec (Some path) None None, Some source) (PlanValueBind C symbol plan)
      (provide C (IVal symbol) arguments (set_val C symbol declared)) F []
| TypesBindInlineApply : forall F C path symbol d name parameters inputs outputs bodies plans result nested made,
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADFunctor name parameters inputs outputs (Some bodies) -> ~ In symbol (fr_applying F) ->
    TypesBindItems W (with_mod F (d_mod d) symbol) C (List.concat bodies) plans result nested made ->
    TypesBindItem W F C (Spec (Some path) None None, None) (PlanInlineApply plans)
      result (carry_frame_allocations F nested) made
| TypesBindAbstractApply : forall F C path plan,
    TypesApplication W F C path plan ->
    TypesBindItem W F C (Spec (Some path) None None, None) (PlanAbstractApply C plan) (app_result plan)
      (frame_after_allocation F (extend_allocation_scope (body_allocation_scope W F C) (app_allocated plan)))
      (map (app_identity plan) (app_outputs plan))
| TypesBindFunctorCopy : forall F C path u provider h Cu nsu Ch nsh target fixed positions,
    resolve_sym W (fr_mod F) path = Some u -> resolve_sym W (fr_mod F) provider = Some h ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) -> nlook h (c_funs C) <> None ->
    FunCtx W u Cu nsu -> FunCtx W h Ch nsh ->
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    (forall x, Fixed W h x -> Fixed W u x) -> FixedAgreement W C h -> SameResult W u h ->
    (forall x t, In (x, t) fixed <-> Fixed W u x /\ nlook x (c_ty C) = Some t) ->
    (forall x t, nlook x (c_ty Ch) = Some t -> nlook x (c_ty Cu) <> None) ->
    CopyInterpretation W (map (fun q => (fst q, canon C (snd q))) fixed) u h Cu nsu nsh Ch positions target ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath provider None))
      (PlanFunctorCopy u h Cu Ch target nsh positions) (set_fun C u fixed) F []
| TypesBindFunction : forall F C path f signature provider plan arguments,
    resolve_sym W (fr_mod F) path = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None signature -> TypesFunctionProvider W F C provider signature plan ->
    AppOf W f (c_ty C) arguments ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath provider None))
      (PlanFunctionBind C f signature plan) (provide C (IFn f) arguments (set_fn C f signature)) F []
| TypesBindAttached : forall F C path label brackets method receiver signature arguments provider name site plan,
    TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
    TypesAttachedMethodPlan W F C provider name receiver signature site plan ->
    TypesBindItem W F C (Spec (Some path) (Some label) brackets, Some (AEField (AEPath provider None) name false))
      (PlanAttachedBind (canon C receiver) method (theta_of C arguments) site plan)
      (provide C (mkey (canon C receiver) method) arguments (set_mth C (canon C receiver) method signature)) F []
| TypesBindWrapped : forall F C path label brackets method receiver signature arguments provider plan,
    TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
    TypesWrappedMethodPlan W F C provider signature plan ->
    TypesBindItem W F C (Spec (Some path) (Some label) brackets, Some (AEPath provider None))
      (PlanWrappedBind (canon C receiver) method (theta_of C arguments) C signature plan)
      (provide C (mkey (canon C receiver) method) arguments (set_mth C (canon C receiver) method signature)) F []
| TypesBindFunctionLambda : forall F C path f signature ps ann body arguments layout provider result,
    resolve_sym W (fr_mod F) path = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None signature -> AppOf W f (c_ty C) arguments ->
    TypesLambdaHeader W F C ps ann body signature None layout provider result ->
    TypesBlkE W layout (lambda_descriptor_site signature ps provider) (ABlock [] (Some body))
      (Some (sg_ret signature)) (lambda_source_body provider) result ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AELam ps ann body))
      (PlanFunctionLambda C f signature (map fst ps) layout provider)
      (provide C (IFn f) arguments (set_fn C f signature)) F []
| TypesBindMethodLambda : forall F C path label brackets method receiver signature arguments ps ann body layout provider result,
    TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
    TypesLambdaHeader W F C ps ann body signature (Some receiver) layout provider result ->
    TypesBlkE W layout (lambda_descriptor_site signature ps provider) (ABlock [] (Some body))
      (Some (sg_ret signature)) (lambda_source_body provider) result ->
    TypesBindItem W F C (Spec (Some path) (Some label) brackets, Some (AELam ps ann body))
      (PlanMethodLambda (canon C receiver) method (theta_of C arguments) C signature (map fst ps) layout provider)
      (provide C (mkey (canon C receiver) method) arguments (set_mth C (canon C receiver) method signature)) F []
| TypesBindClosure : forall F C path provider layout descriptor target_needs provider_needs
    selected fixed items result nested made,
    let handle := linked_handle descriptor in
    let u := linked_target_symbol handle in
    let g := linked_provider_symbol handle in
    let Cu := linked_target_context descriptor in
    let Cg := linked_provider_context descriptor in
    let pins := link_plan_pins (linked_result_reading descriptor) in
    resolve_sym W (fr_mod F) path = Some u -> resolve_sym W (fr_mod F) provider = Some g ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) ->
    w_context W u = Some (Cu, target_needs) -> w_context W g = Some (Cg, provider_needs) ->
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    SameResult W u g ->
    LinkBinding W u Cu target_needs Cg provider_needs
      (map (fun q => (fst q, canon C (snd q))) fixed) (link_private_readings C Cu Cg)
      (link_adopted_inputs C Cu Cg) selected ->
    TypesLinkedHeader W layout descriptor items result made ->
    (Family W u -> FamilyBindPlans W (linked_bind_plan descriptor)) ->
    TypesBindItems W layout Cg items (linked_bind_plan descriptor) result nested made ->
    NeedsSatisfied C Cg (closure_arguments C Cg) (captured_needs selected provider_needs) ->
    (forall x t, In (x, t) fixed <-> Fixed W u x /\ nlook x (c_ty C) = Some t) ->
    (forall x, In x (map fst (c_ty Cg)) -> ~ In x (map fst (c_ty Cu)) -> InForceThrough C Cg x) ->
    (forall x t, In (x, t) pins -> exists here, nlook x (c_ty C) = Some here /\
      (forall y, In y (gens t) -> nlook y (c_ty C) <> None) /\
      canon C here = canon C (app_subst (closure_arguments C Cg) t)) ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath provider None))
      (PlanFunctorClosure C layout descriptor selected provider_needs) (set_fun C u fixed) F [].

(* Declared functions and constant initializers *)

Definition FunctionReceiver (W : world) (f : nat) (rcv : fnrecv)
    (n : name) (C : cx) (this : option ty) : Prop :=
  match rcv with
  | RAttached _ => exists r args,
      Attached W r n f /\ kind_sym W r = Some DKTag /\
      this = Some (TNom r args) /\
      Forall2 (fun x xa => fst xa = x /\ nlook x (c_ty C) = Some (snd xa)) (w_reqs W r) args
  | _ => this = None
  end.

Record function_plan := FunctionPlan {
  function_context : cx;
  function_needs : list item;
  function_receiver : option ty;
  function_signature : fsig;
  function_layout : frame;
  function_body : block_plan;
  function_result : ty
}.

Definition function_parameters (parameters : list name) (plan : function_plan) : cx :=
  fold_left (fun C q => add_local C (fst q) (snd q) LLet)
    (combine parameters (sg_params (function_signature plan))) (function_context plan).

Definition TypesFunction (W : world) (f : nat) (d : decl) (receiver : fnrecv) (n : name)
    (parameters : list name) (body : ablock) (plan : function_plan) : Prop :=
  let F := function_layout plan in
  let C := function_context plan in
  let g := function_signature plan in
  CtxOf W f C (function_needs plan) /\
  FunctionReceiver W f receiver n C (function_receiver plan) /\
  SigOf W f (c_ty C) (function_receiver plan) g /\
  fr_mod F = d_mod d /\ fr_ret F = Some (sg_ret g) /\ fr_this F = function_receiver plan /\
  fr_loop F = false /\ fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
  TypesBlkE W F (function_parameters parameters plan) body (Some (sg_ret g))
    (function_body plan) (function_result plan).

Record constant_plan := ConstantPlan {
  constant_layout : frame;
  constant_body : expression_plan;
  constant_declared : ty;
  constant_actual : ty
}.

Definition TypesConstantInitializer (W : world) (d : decl) (annotation : aty) (initial : aexpr)
    (plan : constant_plan) : Prop :=
  let F := constant_layout plan in
  let C := Cx [] [] [] [] [] in
  d_assumes d = [] /\ TyOf W (d_mod d) [] None annotation (constant_declared plan) /\
  TypesExpr W F C initial (Some (constant_declared plan)) (constant_body plan) (constant_actual plan) /\
  fits (constant_actual plan) (constant_declared plan) /\
  fr_mod F = d_mod d /\ fr_ret F = None /\ fr_this F = None /\
  fr_loop F = false /\ fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [].

Set Implicit Arguments.

End Typing.

Module Export Translation.

(* Provider captures and placements *)

Definition named_provider_capture (plan : named_provider_plan) : type_capture :=
  TypeCapture (provider_arguments plan) (provider_arguments plan).

Definition named_provider_provisions (C : cx) (plan : named_provider_plan) : smap :=
  satisfaction_map C (provider_formal plan) (provider_arguments plan) (provider_needs plan).

Definition function_plan_target (resolve : nat -> nat) (plan : function_provider_plan) : nat :=
  match plan with
  | NamedFunctionProvider named => provider_symbol named
  | AbstractFunctionForwarder occurrence _ => resolve occurrence
  end.

Definition function_plan_capture (plan : function_provider_plan) : type_capture :=
  match plan with
  | NamedFunctionProvider named => named_provider_capture named
  | AbstractFunctionForwarder _ _ => TypeCapture [] []
  end.

Definition function_plan_provisions (C : cx) (plan : function_provider_plan) : smap :=
  match plan with
  | NamedFunctionProvider named => named_provider_provisions C named
  | AbstractFunctionForwarder _ handle =>
      [(IFn (forwarder_source handle), IFn (forwarder_source handle))]
  end.

Definition wrapped_method_code (signature : fsig) (plan : wrapped_method_plan) : expr :=
  let provider := wrapped_method_provider plan in
  let formal := provider_formal provider in
  ECall (provider_symbol provider) (theta_ctx formal)
    (map (fun k => (rkey_of formal k, rkey_of formal k)) (provider_needs provider))
    (map EVar (seq 1 (length (sg_params signature)))).

(* Linked body placement *)

Section LinkedTranslation.
Variable BindPlan : Type.

Definition linked_descriptor_code (descriptor : linked_body_descriptor BindPlan) (bindings : list binding)
    : functor_code :=
  let reading := linked_result_reading descriptor in
  let site := link_plan_site reading in
  translate_linked_body (linked_target_context descriptor) (linked_provider_context descriptor)
    (linked_output_roles descriptor) bindings (map (fun key => rkey_of site (at_site site key)) (link_plan_keys reading)).

End LinkedTranslation.

(* Lambda body placement *)

Section LambdaTranslation.
Variable BodyPlan : Type.

Definition lambda_descriptor_needs (C : cx) (descriptor : lambda_body_descriptor BodyPlan) : smap :=
  map (fun k => (rkey_of (lambda_plan_capture_context descriptor) k, rkey_of C k))
    (lambda_provision_needs descriptor).

Definition lambda_descriptor_code (Fl : frame) (descriptor : lambda_body_descriptor BodyPlan)
    (emitted : expr) : expr :=
  translate_lambda_body Fl (lambda_descriptor_captures descriptor) emitted.

End LambdaTranslation.

(* Method and literal emission *)

Definition emit_provided_method (C : cx) (receiver : ty) (method : nat)
    (choice : option family_use) (object : expr) (arguments : list expr) : expr :=
  match choice with
  | None => EMethod object method (rkey C receiver method) arguments
  | Some selected => translate_family_use C selected
      (EMethod object method (rkey (demand_result selected) receiver method) arguments)
  end.

Definition emit_method_call (C : cx) (receiver : ty) (selected : method_call_plan)
    (object : expr) (arguments : list expr) : expr :=
  match selected with
  | SelectedProvision method choice => emit_provided_method C receiver method choice object arguments
  | SelectedDefinition method formal needs types demands result =>
      let receiver_types := match receiver with TNom _ rows => rows | _ => [] end in
      translate_demands (family_context C receiver_types) demands
        (translate_call result formal method types needs (object :: arguments))
  end.

Fixpoint emit_literal (C : cx) (plan : literal_plan) : expr :=
  match plan with
  | PlanLiteralValue value => EVal value
  | PlanLiteralArithmetic t method family lhs rhs =>
      emit_provided_method C t method family (emit_literal C lhs) [emit_literal C rhs]
  | PlanLiteralFloat f operand => EInvoke f [emit_literal C operand]
  end.

Unset Implicit Arguments.

(* Pattern layout *)

Definition local_number (slots : list (name * nat)) (n : name) : nat :=
  match slook n slots with Some slot => slot | None => 0 end.

Definition emit_field_patterns (slots : list (name * nat)) (fields : list field_pattern_plan) : list (fld * var) :=
  flat_map (fun field => match field with
    | PatternPun n _ => [(n, local_number slots n)]
    | PatternRename n binder _ => [(n, local_number slots binder)]
    | PatternIgnore _ => []
    end) fields.

Definition emit_pattern (slots : list (name * nat)) (plan : pattern_plan) : pattern :=
  match plan with
  | PatternWildcard => PAny None
  | PatternBinder n _ => PAny (Some (local_number slots n))
  | PatternNominal tag => PHead tag None []
  | PatternPayload tag n _ => PHead tag (Some (local_number slots n)) []
  | PatternRecord tag fields => PHead tag None (emit_field_patterns slots fields)
  end.

(* Code and its placement requirements *)

Definition splice_bindings (bindings : list binding) (body : expr) : expr := fold_right EBind body bindings.

Definition application_binding (C : cx) (a : application) : binding :=
  BApply (app_functor a) (app_arguments a) (map (fun t => (t, app_identity a t)) (app_outputs a))
    (satisfaction_map C (app_input a) (app_arguments a) (app_needs a))
    (map (rkey_of (app_result a)) (app_provisions a)).

Inductive placement_requirement : Type :=
| PlaceFunction (slot : nat) (body : expr) (section : option subst)
| PlaceFunctor (slot : nat) (body : functor_code) (source : nat * nat)
| PlaceLayout (layout : frame) (receiver : option ty) (parameters : list name).

Definition placement_in (W : world) (P : program) (requirement : placement_requirement) : Prop :=
  match requirement with
  | PlaceFunction slot body section =>
      Fresh W slot /\ body_of P slot = Some body /\
      match section with
      | Some reads => nlook slot (lambda_sections (program_origins P)) = Some reads
      | None => True
      end
  | PlaceFunctor slot body source =>
      Fresh W slot /\ funbody_of P slot = Some body /\
      nlook slot (linked_origins (program_origins P)) = Some source
  | PlaceLayout layout receiver parameters => Numbering layout receiver parameters
  end.

Definition placements_in (W : world) (P : program) :=
  Forall (placement_in W P).

Definition function_placements (resolve : nat -> nat) (signature : fsig)
    (provider : function_provider_plan) : list placement_requirement :=
  match provider with
  | NamedFunctionProvider _ => []
  | AbstractFunctionForwarder occurrence handle =>
      [PlaceFunction (resolve occurrence) (forwarder_body handle signature) None]
  end.

Record emission (A : Type) := Emission {
  emitted_code : A;
  emitted_placements : list placement_requirement
}.
Arguments Emission {A}.
Arguments emitted_code {A}.
Arguments emitted_placements {A}.

Definition emit_only {A} (code : A) : emission A := Emission code [].

Definition emit_map {A B} (make : A -> B) (source : emission A) : emission B :=
  Emission (make (emitted_code source)) (emitted_placements source).

Definition emit_pair {A B C} (make : A -> B -> C) (first : emission A) (second : emission B) : emission C :=
  Emission (make (emitted_code first) (emitted_code second))
    (emitted_placements first ++ emitted_placements second).

Definition emit_each {A B} (emit : A -> emission B) (sources : list A) : emission (list B) :=
  Emission (map (fun source => emitted_code (emit source)) sources)
    (flat_map (fun source => emitted_placements (emit source)) sources).

Definition emit_flat {A B} (emit : A -> emission (list B)) (sources : list A) : emission (list B) :=
  Emission (flat_map (fun source => emitted_code (emit source)) sources)
    (flat_map (fun source => emitted_placements (emit source)) sources).

Definition require_placement {A} (requirement : placement_requirement) (source : emission A) : emission A :=
  Emission (emitted_code source) (requirement :: emitted_placements source).

Definition lambda_capture_bindings (slots : list (name * nat)) (provider : lambda_body_descriptor block_plan) :=
  map (fun '(n, symbol) => BValue symbol (EVar (local_number slots n)))
    (lambda_descriptor_captures provider).

Definition lambda_binding_placements (resolve : nat -> nat) (site : cx) (parameters : list name)
    (layout : frame) (provider : lambda_body_descriptor block_plan) (body : emission expr) :=
  PlaceLayout layout (fr_this layout) parameters ::
  PlaceFunction (resolve (lambda_occurrence provider))
    (lambda_descriptor_code layout provider (emitted_code body)) (Some (c_sub site)) ::
  emitted_placements body.

Definition emit_lambda_binding (resolve : nat -> nat) (slots : list (name * nat)) (site : cx)
    (parameters : list name) (layout : frame) (provider : lambda_body_descriptor block_plan)
    (body : emission expr) (make : nat -> smap -> binding) : emission (list binding) :=
  Emission (lambda_capture_bindings slots provider ++
    [make (resolve (lambda_occurrence provider)) (lambda_descriptor_needs site provider)])
    (lambda_binding_placements resolve site parameters layout provider body).

Definition linked_binding_placements (resolve : nat -> nat) (layout : frame)
    (descriptor : linked_body_descriptor (list binding_plan)) (body : emission (list binding)) :=
  let handle := linked_handle descriptor in
  PlaceLayout layout None [] ::
  PlaceFunctor (resolve (linked_occurrence handle))
    (linked_descriptor_code descriptor (emitted_code body))
    (linked_target_symbol handle, linked_provider_symbol handle) ::
  emitted_placements body.

Fixpoint emit_expr (resolve : nat -> nat) (slots : list (name * nat)) (C : cx)
    (plan : expression_plan) : emission expr :=
  match plan with
  | PlanUnit => emit_only EUnit
  | PlanThis => emit_only (EVar 0)
  | PlanLocal n => emit_only (EVar (local_number slots n))
  | PlanBlock body => emit_block resolve slots C body
  | PlanReturnUnit => emit_only (EReturn EUnit)
  | PlanReturn result => emit_map EReturn (emit_expr resolve slots C result)
  | PlanBreak => emit_only EBreak
  | PlanDefinedCall f formal needs arguments demands call_site args =>
      emit_map (fun values => translate_demands C demands (translate_call call_site formal f arguments needs values))
        (emit_each (emit_expr resolve slots C) args)
  | PlanAbstractCall f args => emit_map (EInvoke f) (emit_each (emit_expr resolve slots C) args)
  | PlanTag tag arguments payload => emit_map (ETag tag arguments) (emit_expr resolve slots C payload)
  | PlanBareRecord fields =>
      emit_map ERecord
        (emit_each (fun '(n, child) => emit_map (pair n) (emit_expr resolve slots C child)) fields)
  | PlanRecord tag arguments fields =>
      emit_map (fun values => ETag tag arguments (ERecord values))
        (emit_each (fun '(n, child) => emit_map (pair n) (emit_expr resolve slots C child)) fields)
  | PlanField object field => emit_map (fun value => EField value field) (emit_expr resolve slots C object)
  | PlanMatch scrutinee arms =>
      emit_pair EMatch (emit_expr resolve slots C scrutinee)
        (emit_each (fun '(pattern, body) => emit_map (pair (emit_pattern slots pattern))
          (emit_expr resolve slots (pattern_scope C pattern) body)) arms)
  | PlanMethod receiver selected object args =>
      emit_pair (emit_method_call C receiver selected) (emit_expr resolve slots C object)
        (emit_each (emit_expr resolve slots C) args)
  | PlanOperator receiver method family object args =>
      emit_pair (emit_provided_method C receiver method family) (emit_expr resolve slots C object)
        (emit_each (emit_expr resolve slots C) args)
  | PlanNumeric literal => emit_only (emit_literal C literal)
  | PlanGlobalValue symbol => emit_only (EVal symbol)
  | PlanConstant layout initializer =>
      require_placement (PlaceLayout layout None [])
        (emit_expr resolve (fr_var layout) (Cx [] [] [] [] []) initializer)
  | PlanBracketedCall expansion => emit_expr resolve slots C expansion
  end
with emit_block (resolve : nat -> nat) (slots : list (name * nat)) (C : cx)
    (plan : block_plan) : emission expr :=
  match plan with
  | PlanEmpty => emit_only EUnit
  | PlanTail result => emit_expr resolve slots C result
  | PlanLet n t mode initializer rest =>
      emit_pair (ELet (local_number slots n)) (emit_expr resolve slots C initializer)
        (emit_block resolve slots (add_local C n t mode) rest)
  | PlanAssign n value rest =>
      emit_pair (fun value => ESeq (EAssign (local_number slots n) value))
        (emit_expr resolve slots C value) (emit_block resolve slots C rest)
  | PlanExprLast value => emit_map (fun value => ESeq value EUnit) (emit_expr resolve slots C value)
  | PlanExprThen value rest =>
      emit_pair ESeq (emit_expr resolve slots C value) (emit_block resolve slots C rest)
  | PlanWhile condition body rest =>
      emit_pair ESeq (emit_pair EWhile (emit_expr resolve slots C condition) (emit_block resolve slots C body))
        (emit_block resolve slots C rest)
  | PlanLoop body rest =>
      emit_pair (fun body => ESeq (ELoop body)) (emit_block resolve slots C body) (emit_block resolve slots C rest)
  | PlanLoopLast body => emit_map (fun body => ESeq (ELoop body) EUnit) (emit_block resolve slots C body)
  | PlanBind items result rest =>
      emit_pair splice_bindings (emit_flat (emit_bind_item resolve slots) items) (emit_block resolve slots result rest)
  end
with emit_bind_item (resolve : nat -> nat) (slots : list (name * nat)) (plan : binding_plan)
    : emission (list binding) :=
  match plan with
  | PlanTypeBind symbol meaning => emit_only [BType symbol meaning]
  | PlanValueBind site symbol initializer =>
      emit_map (fun value => [BValue symbol value]) (emit_expr resolve slots site initializer)
  | PlanInlineApply items => emit_flat (emit_bind_item resolve slots) items
  | PlanAbstractApply site plan => emit_only [application_binding site plan]
  | PlanFunctorCopy target provider Cu Ch selected needs positions =>
      emit_only [BCopyFunctor target provider (map (fun k => (rkey_of Ch k, rkey_of Cu (selected k))) needs) positions]
  | PlanFunctionBind site symbol signature provider =>
      Emission [BFunction symbol (function_plan_target resolve provider)
        (function_plan_capture provider) (function_plan_provisions site provider)]
        (function_placements resolve signature provider)
  | PlanAttachedBind receiver method arguments site provider =>
      emit_only [BMethod receiver method arguments (provider_symbol provider)
        (named_provider_capture provider) (named_provider_provisions site provider)]
  | PlanWrappedBind receiver method arguments site signature provider =>
      let named := wrapped_method_provider provider in
      let slot := resolve (wrapped_method_occurrence provider) in
      Emission [BMethod receiver method arguments slot (named_provider_capture named)
        (named_provider_provisions site named)] [PlaceFunction slot (wrapped_method_code signature provider) None]
  | PlanFunctionLambda site symbol signature parameters layout provider =>
      let body_site := lambda_parameter_context (lambda_plan_capture_context provider) parameters (sg_params signature) in
      emit_lambda_binding resolve slots site parameters layout provider
        (emit_block resolve (fr_var layout) body_site (lambda_source_body provider))
        (fun target needs => BFunction symbol target (lexical_capture site) needs)
  | PlanMethodLambda receiver method arguments site signature parameters layout provider =>
      let body_site := lambda_parameter_context (lambda_plan_capture_context provider) parameters (sg_params signature) in
      emit_lambda_binding resolve slots site parameters layout provider
        (emit_block resolve (fr_var layout) body_site (lambda_source_body provider))
        (fun target needs => BMethod receiver method arguments target (lexical_capture site) needs)
  | PlanFunctorClosure site layout descriptor selected needs =>
      let handle := linked_handle descriptor in
      let Cu := linked_target_context descriptor in
      let Cg := linked_provider_context descriptor in
      let body := emit_flat (emit_bind_item resolve (fr_var layout)) (linked_bind_plan descriptor) in
      Emission [BFunctor (linked_target_symbol handle) (resolve (linked_occurrence handle)) (lexical_capture site)
        (link_map Cu Cg selected needs)
        (satisfaction_map site Cg (closure_arguments site Cg) (captured_needs selected needs))]
        (linked_binding_placements resolve layout descriptor body)
  end.

Definition expression_emission (resolve : nat -> nat) (F : frame) := emit_expr resolve (fr_var F).
Definition block_emission (resolve : nat -> nat) (F : frame) := emit_block resolve (fr_var F).

(* Declared functions and constant initializers *)

Definition emit_function (resolve : nat -> nat) (parameters : list name)
    (plan : function_plan) : emission expr :=
  block_emission resolve (without_pins (function_layout plan)) (function_parameters parameters plan)
    (function_body plan).

Definition TranslatesFunction (W : world) (P : program) (resolve : nat -> nat) (f : nat)
    (parameters : list name) (plan : function_plan) : Prop :=
  let output := emit_function resolve parameters plan in
  Numbering (function_layout plan) (function_receiver plan) parameters /\
  placements_in W P (emitted_placements output) /\
  body_of P f = Some (emitted_code output).

Definition emit_constant (resolve : nat -> nat) (plan : constant_plan) : emission expr :=
  expression_emission resolve (constant_layout plan) (Cx [] [] [] [] []) (constant_body plan).

Definition TranslatesConstant (W : world) (P : program) (resolve : nat -> nat)
    (plan : constant_plan) : Prop :=
  Numbering (constant_layout plan) None [] /\
  placements_in W P (emitted_placements (emit_constant resolve plan)).

Set Implicit Arguments.

End Translation.

Unset Implicit Arguments.

Definition FunctionPlanAt (W : world) (P : program) (resolve : nat -> nat) (f : nat) : Prop :=
  forall d receiver n parameters annotation body,
    w_decl W f = Some d -> d_ast d = ADFn receiver n parameters annotation (Some body) ->
    exists plan, TypesFunction W f d receiver n (map fst parameters) body plan /\
      TranslatesFunction W P resolve f (map fst parameters) plan.

Definition ConstantPlanAt (W : world) (P : program) (resolve : nat -> nat)
    (d : decl) (annotation : aty) (initial : aexpr) : Prop :=
  exists plan, TypesConstantInitializer W d annotation initial plan /\
    TranslatesConstant W P resolve plan.

(* One resolution assigns the generated body occurrences their slots. *)
Definition ElaboratesIn (bundle : bundle) (W : world) (P : program) : Prop :=
  exists resolve, ElaboratesWith bundle W P (FunctionPlanAt W P resolve) (ConstantPlanAt W P resolve).

Set Implicit Arguments.

Definition Elaborates (b : bundle) (P : program) : Prop :=
  exists W, ElaboratesIn b W P.

End Elaboration.


(* Linking reads the platform's entry types and provisions in the
   same source world as elaboration. The platform also supplies the root
   assumptions, entry ABI, and admissibility conditions. *)
Module Type STATIC_PLATFORM.
  Parameter root_item : option (string * Input.name).
  Parameter Tables : Elaboration.world -> Repr.program_data -> Prop.
  Parameter InitialTypes : Elaboration.world -> fsym -> Elaboration.cx -> subst -> Prop.
  Parameter Provides : Elaboration.world -> program -> Elaboration.cx -> item -> provision_value -> Prop.
  Parameter EntryABI : Elaboration.world -> program -> Prop.
  Parameter Admissible : Elaboration.world -> program -> Prop.
End STATIC_PLATFORM.

Module Linking (Pf : STATIC_PLATFORM).
Import Elaboration.
Definition Linked (W : world) (P : program) : Prop :=
  w_root_item W = Pf.root_item /\ Pf.EntryABI W P /\ Pf.Admissible W P /\
  Pf.Tables W (program_platform P) /\
  exists context needs types entries,
    CtxOf W (program_main P) context needs /\ Pf.InitialTypes W (program_main P) context types /\
    program_environment P = Environment types entries /\
    Forall (fun entry => Pf.Provides W P context (fst entry) (snd entry)) entries.
Definition Ready (P : program) : Prop :=
  exists bundle world, ElaboratesIn bundle world P /\ Linked world P.
End Linking.

(* The operations installed anywhere in a program, including captured
   environments and platform-family bodies. Extra interface operations are
   harmless; an interface must supply at least these requirements. *)
Fixpoint expression_primitives (expression : expr) : list op :=
  match expression with
  | EVar _ | EUnit | EVal _ | EBreak => []
  | ETag _ _ e | EAssign _ e | ELoop e | EReturn e | EField e _ => expression_primitives e
  | EBind binding e => binding_primitives binding ++ expression_primitives e
  | ECall _ _ _ args | EInvoke _ args => flat_map expression_primitives args
  | EMethod receiver _ _ args => expression_primitives receiver ++ flat_map expression_primitives args
  | ELet _ a b | ESeq a b | EWhile a b => expression_primitives a ++ expression_primitives b
  | ERecord fields => flat_map (fun field => expression_primitives (snd field)) fields
  | EMatch e arms => expression_primitives e ++ flat_map (fun arm => expression_primitives (snd arm)) arms
  end
with binding_primitives (binding : binding) : list op :=
  match binding with
  | BValue _ e => expression_primitives e
  | BPrimitive _ operation => [operation]
  | _ => []
  end.
Fixpoint entry_primitives (entry : provision_value) : list op :=
  match entry with
  | PrimitiveClosure operation _ => [operation]
  | FunctionClosure _ (EnvironmentWithAliases _ _ captured) | FunctorClosure _ _ (EnvironmentWithAliases _ _ captured) _ =>
      flat_map (fun entry => entry_primitives (snd entry)) captured
  | _ => []
  end.
Definition required_primitives (program : program) : list op :=
  flat_map (fun fn => expression_primitives (snd fn)) (program_functions program) ++
  flat_map (fun fn => flat_map binding_primitives (fc_bindings (snd fn))) (program_functors program) ++
  flat_map (fun entry => entry_primitives (snd entry)) (provision_values (program_environment program)).

Definition program_of (p : program) (argv : list (list int))
    (rd : list int -> option (list int)) : program_view :=
  ProgramView (fun f => Elaboration.nlook f (program_functions p))
    (fun u => Elaboration.nlook u (program_functors p))
    (program_bool p) (program_true p) (program_false p) argv rd (program_platform p).

(* ######################################################################## *)
(* Public source-to-world contract                                           *)
(* ######################################################################## *)

(* The language uses the same static platform at build and run time.
   The platform supplies its raw-host contract and adapts primitive calls. *)
Module Type PLATFORM.
  Include STATIC_PLATFORM.
  Parameter hty : Type.
  Parameter call : forall S : Type,
    (program_view -> op -> list value -> S -> signal * S) ->
    program_view -> op -> option ty -> list value -> S -> signal * S.
  Parameter laws : forall S : Type, program_view -> (op -> bool) ->
    (program_view -> op -> list value -> S -> signal * S) -> (hty -> S -> Prop) -> Prop.
End PLATFORM.

(* A host chooses the primitive-managed state and raw operations.
   Public also keeps arguments and the read-only file snapshot in the world. *)
Module Type HOST_FOR (Pf : PLATFORM).
  Parameter state : Type.
  Parameter primitive : program_view -> op -> list value -> state -> signal * state.
  Parameter heap_ok : Pf.hty -> state -> Prop.
End HOST_FOR.

Module PublicFor (Pf : PLATFORM) (Host : HOST_FOR Pf).
  Module Link := Linking Pf.

  Record state := World {
    arguments : list (list int);
    read_file : list int -> option (list int);
    external : Host.state
  }.

  Record interface := HostInterface {
    supports : op -> bool;
    primitive : program_view -> op -> list value -> Host.state -> signal * Host.state;
    heap_ok : Pf.hty -> Host.state -> Prop
  }.

  (* The default host makes every operation available; the platform laws
     describe the behavior of its raw operations. *)
  Definition default_interface : interface :=
    HostInterface (fun _ => true) Host.primitive Host.heap_ok.

  Definition runtime (program : program) (initial : state) : program_view :=
    program_of program (arguments initial) (read_file initial).

  Definition interface_laws (P : program_view) (interface : interface) : Prop :=
    @Pf.laws Host.state P (supports interface) (primitive interface) (heap_ok interface).

  Definition Compatible (program : program) (interface : interface) : Prop :=
    Link.Ready program /\
    (forall operation, In operation (required_primitives program) -> supports interface operation = true) /\
    forall argv read, interface_laws (program_of program argv read) interface.

  (* Traps, exits and unsupported primitives are explicit outcomes. Wrong
     represents an internal error, including a missing interface operation. *)
  Inductive outcome :=
  | Returned : value -> outcome
  | Exited : int -> outcome
  | Trapped : nat -> outcome
  | Unsupported : nat -> outcome
  | Wrong : nat -> outcome.

  Record execution := Result {
    execution_outcome : outcome;
    final_state : state
  }.

  (* None means this finite observation has not produced an outcome. *)
  Definition partial (A : Type) := nat -> option A.
  Definition e_missing_primitive := 15.

  Module ReferenceHost <: PRIMITIVES.
    Definition mstate := (interface * state)%type.
    Definition raw_primitive P p values (machine : mstate) :=
      let '(interface, state) := machine in
      if supports interface p then
        let '(out, next) := primitive interface P p values (external state) in
        (out, (interface, World (arguments state) (read_file state) next))
      else (Stuck e_missing_primitive, machine).
    Definition run_prim := @Pf.call mstate raw_primitive.
  End ReferenceHost.
  Module Reference := Interpreter ReferenceHost.

  Definition observe (result : Reference.evaluation) : option execution :=
    let state := snd (Reference.evaluation_state result) in
    match Reference.evaluation_signal result with
    | Normal value | Returning value => Some (Result (Returned value) state)
    | Exit code => Some (Result (Exited code) state)
    | Trap code => Some (Result (Trapped code) state)
    | UnsupportedOperation code => Some (Result (Unsupported code) state)
    | Stuck code => Some (Result (Wrong code) state)
    | Breaking => Some (Result (Wrong Reference.e_break) state)
    | OutOfFuel => None
    end.

  (* The build target is this platform. source declarations and primitive
     bindings are checked against one shared static reading of the bundle.
     The prelude is fixed configuration and is read through source too. *)
  Definition Builds (prelude : file_path) (source : source) (entry : file_path)
      (program : program) : Prop :=
    exists bundle world,
      Loader.Bundled (source_read source) "/" (path_text prelude) (path_text entry) bundle /\
      Elaboration.ElaboratesIn bundle world program /\ Link.Linked world program.

  Definition Admissible (prelude : file_path) (program : program) : Prop :=
    exists source entry, Builds prelude source entry program.

  Definition Runs (program : program) (interface : interface)
      (initial : state) (result : execution) : Prop :=
    exists fuel,
      observe (Reference.run_main fuel (runtime program initial)
        (program_main program) (program_environment program) (interface, initial)) = Some result.

  (* The five public obligations for an implementation. Completeness covers builds
     for this platform and permits the implementation's own fresh symbols.
     Compatibility with the default interface follows from building;
     other compatible interfaces are covered by the same run/safety claims.
     Safety concerns Builds and Runs themselves; executable safety follows
     by combining it with build_sound and run_sound. *)
  Module Type CORRECT.
    Parameter prelude : file_path.
    Parameter build : source -> file_path -> option program.
    Parameter run : program -> interface -> state -> partial execution.

    Axiom build_sound : forall source entry program,
      build source entry = Some program ->
      Builds prelude source entry program /\ Compatible program default_interface.

    Axiom build_complete : forall source entry,
      (exists program, Builds prelude source entry program) ->
      exists program, build source entry = Some program.

    Axiom run_sound : forall program interface initial,
      Admissible prelude program ->
      Compatible program interface ->
      forall fuel result,
        run program interface initial fuel = Some result ->
        Runs program interface initial result.

    Axiom run_complete : forall program interface initial,
      Admissible prelude program ->
      Compatible program interface ->
      forall result, Runs program interface initial result ->
        exists fuel, run program interface initial fuel = Some result.

    Axiom safety : forall program interface initial,
      Admissible prelude program ->
      Compatible program interface ->
      forall code final,
        ~ Runs program interface initial (Result (Wrong code) final).
  End CORRECT.
End PublicFor.

End Semantics.
