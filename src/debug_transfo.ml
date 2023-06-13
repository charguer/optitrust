open Printf
open Ast
open Target

let path (msg : string) (p : path) : unit =
  printf "%s: %s\n" msg (Path.path_to_string p)

let paths (msg : string) (ps : paths) : unit =
  printf "%s: %s\n" msg (Tools.list_to_string (List.map Path.path_to_string ps))

let trm ?(internal : bool = false) (msg : string) (t : trm) : unit =
  let t_str = if internal
    then Ast_to_text.ast_to_string t
    else AstC_to_c.ast_to_string t
    in
  printf "%s: %s\n" msg t_str

let trms (msg : string) (ts : trms) : unit =
  printf "%s: %s\n" msg (Tools.list_to_string (List.map AstC_to_c.ast_to_string ts))

let current_ast ?(internal : bool = false) (msg : string) : unit =
  trm ~internal msg (Trace.ast ())

let current_ast_at_path ?(internal : bool = false) (msg : string) (p : Path.path) : unit =
  trm ~internal msg (Path.resolve_path p (Trace.ast ()))

let current_ast_at_target ?(internal : bool = false) (msg : string) (tg : Target.target) : unit =
  Target.iter (fun t p -> trm ~internal msg (Path.resolve_path p t)) tg