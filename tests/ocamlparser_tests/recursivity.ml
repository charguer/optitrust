open Optitrust
open Prelude

open Trm_matching

let _ = Ast.behavior_ocaml := true

let _ = Flags.dump_as_c := true

let _ = Flags.debug_var_id := true

let _ = Flags.c_parser_name := "Ocaml_parser"

let _ = Run.script_ml (fun () ->
  ()
)
