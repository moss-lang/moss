type entry = ..

type operation = {
  name : string;
  symbols : int list;
  mutable cached : entry option;
}

let operation (name, symbols) = { name; symbols; cached = None }
let name operation = operation.name
let symbols operation = operation.symbols

(* Name and operands are immutable. Only this decoder's private constructor
   can satisfy a hit, and its value comes from applying the pure decoder
   to those same fields. The cached result serves both the call adapter
   and the host step. *)
let memoize (type result) (decode : operation -> result) =
  let module Decoder = struct type entry += Ready of result end in
  fun operation ->
    match operation.cached with
    | Some (Decoder.Ready result) -> result
    | _ ->
        let result = decode operation in
        operation.cached <- Some (Decoder.Ready result);
        result
