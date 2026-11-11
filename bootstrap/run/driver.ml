(* The reference implementation's driver (README.md).

   Usage: DRIVER [--pages N] [--argv0 A] [--prelude P] [--elab-only] [--time]
                 [--debug-no-passes] PROGRAM.moss [ARG...]
          DRIVER --dump OUTDIR FILE.moss...

   PROGRAM.moss is read, lexed, parsed and bundled by the Rocq front end
   (Parse.load; this driver only reads files) and elaborated by the
   executable elaborator (EaMain.elaborate) into an Audit.Semantics
   program.  That program is given the proved passes (ProgramFastPath.fastpath,
   ProgramPrune.prune, ProgramCapTrim.captrim, each proved to keep Runs) and run
   by Audit.v's reference interpreter (Audit.Semantics.Interpreter.run_main),
   its primitives MossX.run_prim (prims.ml).  The prelude is P (`--prelude ""`
   for none), else $MOSS_LIB/prelude.moss, else lib/prelude.moss, from the
   working directory.

   --elab-only stops after elaboration.  --time writes timing lines to
   stderr.  --debug-no-passes runs the elaborated program as it is: a
   debugging aid, for comparing the passes with their absence.  --dump
   writes each file's AST dump (Parse.print_sx) or `ERROR msg` to
   OUTDIR/<mangled absolute path>.rocq.sx.

   stdout is the program's output and stderr its error output, then a line
   for an abnormal outcome.  Paths in messages are relative to the working
   directory.  Exit status: 63 the front end rejected the program (a lex,
   parse or load error), 64 the elaborator rejected it, 65 a construct not
   modelled (a message "unsupported: ...": no `main`, a `main` that
   assumes what no platform library provides, or more than one memory,
   D79); otherwise 0 normal, the code for
   proc_exit, 134 trap, 3 stuck, 4 unsupported primitive, 5 out of fuel; 2
   a usage error. *)
module A = Audit.Semantics
module C = InterpreterCompiled.CloInterpreter(Prims.P)
module I = C.I

let read_file f = try Some (In_channel.with_open_bin f In_channel.input_all) with Sys_error _ -> None
let timing = ref false
let time fmt = Printf.ksprintf (fun s -> if !timing then Printf.eprintf "[time: %s]\n%!" s) fmt
let now = Unix.gettimeofday

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* The front end resolves paths against the working directory, so messages
   name absolute paths; print them relative to it. *)
let relative msg =
  let root = Filename.concat (Sys.getcwd ()) "" in
  let n = String.length root and b = Buffer.create (String.length msg) in
  let rec go i =
    if i >= String.length msg then ()
    else if i + n <= String.length msg && String.sub msg i n = root then go (i + n)
    else (Buffer.add_char b msg.[i]; go (i + 1)) in
  go 0; Buffer.contents b

(* An elaborator message for a construct not modelled (exit 65) rather than
   a rejection (64) starts with "unsupported: ", as the old oracle's did.
   Ea gives it only for a bundle without a defined `main` (a library), and
   for a `main` that assumes an item no platform library provides. *)
let unsupported msg = starts_with "unsupported: " msg

(* D79: the host (MossX) has one linear memory, so a program that gives
   `Memory` another meaning, by a bind or as a functor application's output,
   would run its memories on one address space, and could print a wrong
   answer.  Such a program is refused as not modelled, as the old oracle
   refused it. *)
let rebinds_memory (w : A.Elaboration.world) (p : A.program) =
  match EaTypes.lang_sym w "wasm" A.Elaboration.NSName "Memory" with
  | None -> false
  | Some m ->
      let rec e (x : A.expr) = match x with
        | A.EVar _ | A.EUnit | A.EVal _ | A.EBreak -> false
        | A.ETag (_, _, a) | A.EAssign (_, a) | A.ELoop a | A.EReturn a | A.EField (a, _) -> e a
        | A.ECall (_, _, _, args) | A.EInvoke (_, args) -> List.exists e args
        | A.EMethod (a, _, _, args) -> e a || List.exists e args
        | A.ELet (_, a, c) | A.ESeq (a, c) | A.EWhile (a, c) -> e a || e c
        | A.EBind (bd, a) -> b bd || e a
        | A.EIf (a, c, d) -> e a || e c || e d
        | A.ERecord fs -> List.exists (fun (_, a) -> e a) fs
        | A.EMatch (a, arms) -> e a || List.exists (fun (_, a) -> e a) arms
      and b (x : A.binding) = match x with
        | A.BType (t, _) -> t = m
        | A.BValue (_, a) -> e a
        | A.BApply (_, _, outs, _, _) -> List.exists (fun (s, t) -> s = m || t = m) outs
        | _ -> false in
      List.exists (fun (_, x) -> e x) p.A.program_functions
      || List.exists (fun (_, c) -> List.exists b c.A.fc_bindings) p.A.program_functors

let mangle f =
  let p = if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f else f in
  String.concat "_" (List.filter (fun s -> s <> "") (String.split_on_char '/' p))

let dump dir files =
  List.iter (fun f ->
    let text = match read_file f with
      | Some t -> t | None -> Printf.eprintf "[front end: cannot read %s]\n" f; exit 63 in
    let out = match Parse.parse_moss text with
      | Sx.Good a -> Parse.print_sx (Sx.Dump.dump_file a) ^ "\n"
      | Sx.Bad m -> "ERROR " ^ m ^ "\n" in
    Out_channel.with_open_bin (Filename.concat dir (mangle f ^ ".rocq.sx"))
      (fun oc -> output_string oc out)) files;
  exit 0

let bytes_of_string s = List.init (String.length s) (fun i -> Uint63.of_int (Char.code s.[i]))
let string_of_bytes l = String.init (List.length l) (fun i -> Char.chr (snd (Uint63.to_int2 (List.nth l i))))
(* MossX's output buffers are reversed byte lists *)
let output l =
  let b = Buffer.create 4096 in
  List.iter (fun c -> Buffer.add_char b (Char.chr (snd (Uint63.to_int2 c)))) (List.rev l);
  Buffer.contents b

let passes = [
  ("fastpath", ProgramFastPath.fastpath);
  ("prune", ProgramPrune.prune);
  ("captrim", ProgramCapTrim.captrim);
]

let trap_name = function
  | 1 -> "unreachable" | 2 -> "integer divide by zero" | 3 -> "integer overflow"
  | 4 -> "out of bounds memory access" | 5 -> "invalid conversion to integer"
  | c -> string_of_int c

let usage () =
  prerr_endline "usage: [--pages N] [--argv0 A] [--prelude P] [--elab-only] [--time] [--debug-no-passes] PROGRAM.moss [ARG...]\n       --dump OUTDIR FILE.moss...";
  exit 2

let () =
  let pages = ref 1 and argv0 = ref None and elab_only = ref false and run_passes = ref true in
  let prelude = ref (match Sys.getenv_opt "MOSS_LIB" with
                     | Some d -> Filename.concat d "prelude.moss" | None -> "lib/prelude.moss") in
  let rec opts = function
    | "--pages" :: n :: r -> (match int_of_string_opt n with Some n -> pages := n | None -> usage ()); opts r
    | "--argv0" :: a :: r -> argv0 := Some a; opts r
    | "--prelude" :: p :: r -> prelude := p; opts r
    | "--elab-only" :: r -> elab_only := true; opts r
    | "--time" :: r -> timing := true; opts r
    | "--debug-no-passes" :: r -> run_passes := false; opts r
    | "--dump" :: d :: r -> dump d r
    | r -> r in
  let file, args = match opts (List.tl (Array.to_list Sys.argv)) with
    | f :: a when not (starts_with "--" f) -> f, a | _ -> usage () in
  let t0 = now () in
  let b = match Parse.load read_file (Sys.getcwd ()) !prelude file with
    | Sx.Bad m -> Printf.eprintf "[front end: %s]\n" (relative m); exit 63
    | Sx.Good b -> b in
  time "loaded in %.3fs" (now () -. t0);
  let t1 = now () in
  let p = match EaMain.elaborate b with
    | EaBase.Err m ->
        let m = relative m in
        Printf.eprintf "[elab: %s]\n" m; exit (if unsupported m then 65 else 64)
    | EaBase.Ok (w, p) ->
        if rebinds_memory w p then begin
          prerr_endline "[elab: unsupported: D79: more than one memory (`Memory` rebound); the host has one linear memory]";
          exit 65
        end;
        p in
  time "elaborated in %.3fs: %d fns, %d functor bodies" (now () -. t1)
    (List.length p.A.program_functions) (List.length p.A.program_functors);
  if !elab_only then exit 0;
  let p = if not !run_passes then p else
    List.fold_left (fun p (name, pass) ->
      let t = now () in let p = pass p in time "%s in %.3fs" name (now () -. t); p) p passes in
  (* Resolve each required operation in the shared decoder cache before
     entering the interpreter. *)
  List.iter (fun operation -> ignore (A.WasmOps.decode operation)) (A.required_primitives p);
  let argv0 = match !argv0 with Some a -> a | None -> Filename.remove_extension (Filename.basename file) ^ ".wasm" in
  let argv = List.map bytes_of_string (argv0 :: args) in
  let read name = Option.map bytes_of_string (read_file (string_of_bytes name)) in
  Prims.host := Some {
    ExecutionModel.xp_fn = (fun _ -> None); xp_fun = (fun _ -> None); xp_inputs = (fun _ -> []);
    xp_cast = []; xp_exports = (fun _ -> []); xp_immutable = false;
    xp_fixed_inst = (fun _ -> []); xp_fixed_keys = (fun _ -> []);
    xp_bool = p.A.program_bool; xp_true = p.A.program_true; xp_false = p.A.program_false;
    xp_argv = argv; xp_read = read };
  (* program_of's view, its two lookups realized with Hashtbls: the first
     body for a symbol wins, as program_of's nlook. *)
  (* Symbols are OCaml ints: hash them as ints, not with caml_hash. *)
  let module T = Hashtbl.Make (struct type t = int let equal = Int.equal let hash x = x land max_int end) in
  let table l = let t = T.create 4096 in List.iter (fun (k, v) -> T.replace t k v) (List.rev l); t in
  let fns = table p.A.program_functions and funs = table p.A.program_functors in
  let view = { (A.program_of p argv read) with
               A.view_function = T.find_opt fns; A.view_functor = T.find_opt funs } in
  let t2 = now () in
  (* InterpreterCompiled.crun_main_reference: under tbl_ok, the compiled run on the
     converted environment returns exactly what Interpreter.run_main
     returns. The arrays are trusted driver code, as the Hashtbls realizing
     the view are: tbl and ftbl hold the code of the view's bodies (the
     first body for a symbol wins, as there), closures that read the very
     arrays they are stored in, filled before the run starts. *)
  let nf = 1 + List.fold_left (fun n (f, _) -> max n f) 0 p.A.program_functions in
  let nu = 1 + List.fold_left (fun n (u, _) -> max n u) 0 p.A.program_functors in
  let a = Array.make nf None and b = Array.make nu None in
  let tbl f = if f >= 0 && f < nf then a.(f) else None in
  let ftbl u = if u >= 0 && u < nu then b.(u) else None in
  List.iter (fun (f, e) -> a.(f) <- Some (C.cc view tbl ftbl e)) (List.rev p.A.program_functions);
  List.iter (fun (u, code) -> b.(u) <- Some (C.ccbs view tbl ftbl code.A.fc_bindings)) (List.rev p.A.program_functors);
  let r = C.crun_main view tbl max_int p.A.program_main
    (InterpreterFast.fast_env (InterpreterLazy.lift_env p.A.program_environment)) (MossX.init_state (Uint63.of_int !pages)) in
  let st = r.I.evaluation_state in
  print_string (output st.MossX.ms_out); flush stdout;
  prerr_string (output st.MossX.ms_err);
  time "ran in %.3fs" (now () -. t2);
  match r.I.evaluation_signal with
  | A.Normal _ -> exit 0
  | A.Exit c -> exit (snd (Uint63.to_int2 c))
  | A.Trap c -> Printf.eprintf "[trap %s]\n" (trap_name c); exit 134
  | A.Stuck c -> Printf.eprintf "[stuck %d]\n" c; exit 3
  | A.UnsupportedOperation c ->
      Printf.eprintf "[unsupported primitive %s]\n"
        (match List.nth_opt p.A.program_unsupported c with Some n -> n | None -> string_of_int c);
      exit 4
  | A.OutOfFuel -> prerr_endline "[out of fuel]"; exit 5
  | A.Returning _ | A.Breaking -> prerr_endline "[stray control transfer]"; exit 3
