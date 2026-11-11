type entry = ..

type operation = {
  name : string;
  symbols : int list;
  mutable cached : entry option;
  mutable host_cached : entry option;
}

let operation (name, symbols) = { name; symbols; cached = None; host_cached = None }
let name operation = operation.name
let symbols operation = operation.symbols

(* Name and operands are immutable. Only this decoder's private constructor
   can satisfy a hit, and its value comes from applying the pure decoder
   to those same fields. The cached result serves both the call adapter
   and the host step. *)
let memoize_at (type result) get set (decode : operation -> result) =
  let module Decoder = struct type entry += Ready of result end in
  fun operation ->
    match get operation with
    | Some (Decoder.Ready result) -> result
    | _ ->
        let result = decode operation in
        set operation (Some (Decoder.Ready result));
        result

let memoize decode =
  memoize_at (fun op -> op.cached) (fun op value -> op.cached <- value) decode

let memoize_host decode =
  memoize_at (fun op -> op.host_cached) (fun op value -> op.host_cached <- value) decode
