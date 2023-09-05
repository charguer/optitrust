open Printf
open Syntax
open Target

let prt = printf

let path (msg : string) (p : path) : unit =
  prt "%s: %s\n" msg (Path.path_to_string p)

let paths (msg : string) (ps : paths) : unit =
  prt "%s: %s\n" msg (Tools.list_to_string (List.map Path.path_to_string ps))

type debug_trm_style =
  | Display
  | Internal
  | InternalDisplay

let trm ?(style = Display) (msg : string) (t : trm) : unit =
  let t_str = match style with
  | Display -> Ast_to_text.ast_to_string t
  | Internal -> AstC_to_c.ast_to_string t
  | InternalDisplay -> AstC_to_c.ast_to_string ~optitrust_syntax:true t
  in
  prt "%s: %s\n" msg t_str

let trms (msg : string) (ts : trms) : unit =
  prt "%s: %s\n" msg (Tools.list_to_string (List.map AstC_to_c.ast_to_string ts))

let typ (msg : string) (t : typ) : unit =
  let t_str = AstC_to_c.typ_to_string t in
  prt "%s: %s\n" msg t_str

let typs (msg : string) (ts : typ list) : unit =
  prt "%s: %s\n" msg (Tools.list_to_string (List.map AstC_to_c.typ_to_string ts))

let current_ast ?(style = Display) (msg : string) : unit =
  trm ~style msg (Trace.ast ())

let current_ast_at_path ?(style = Display) (msg : string) (p : Path.path) : unit =
  trm ~style msg (Path.resolve_path p (Trace.ast ()))

let current_ast_at_target ?(style = Display) (msg : string) (tg : Target.target) : unit =
  let found = ref 0 in
  Target.iter (fun t p ->
    found := !found + 1;
    trm ~style (sprintf "%s (%d)" msg !found) (Path.resolve_path p t)) tg;
  if !found = 0 then prt "%s: no match\n" msg
