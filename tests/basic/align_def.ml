open Optitrust
open Target
open Ast


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun () ->
  !! Align_basic.def (lit "16") [cVarDef "a"];

  !! Align_basic.def (lit "16") [cVarDef "coeffs_x"];


)