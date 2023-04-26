open Printf
open Ast
open Target

let path (msg : string) (p : path) : unit =
  printf "%s: %s\n" msg (Path.path_to_string p)

let trm (msg : string) (t : trm) : unit =
  printf "%s: %s\n" msg (AstC_to_c.ast_to_string t)

let trm_internal (msg : string) (t : trm) : unit =
  printf "%s: %s\n" msg (Ast_to_text.ast_to_string t)

let trms (msg : string) (ts : trms) : unit =
  printf "%s: %s\n" msg (Tools.list_to_string (List.map AstC_to_c.ast_to_string ts))

let current_ast (msg : string) : unit =
  trm msg (Trace.ast ())

let current_ast_at_path (msg : string) (p : Path.path) : unit =
  trm msg (Path.resolve_path p (Trace.ast ()))