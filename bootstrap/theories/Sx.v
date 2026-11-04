(* ------------------------------------------------------------------------ *)
(* Sx.v — the elaborator's input.  Its definitions (the s-expression tokens, *)
(* the surface AST of ast.py, and the total readers of sexpr.py's dump) are *)
(* audit-level and now live in Audit.v, Part I (module Audit.Input); this   *)
(* file only re-exports them under their old names.                         *)
(* ------------------------------------------------------------------------ *)

From NM Require Export Audit.
Export Audit.Input.
