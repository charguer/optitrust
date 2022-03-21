open Optitrust
open Target

let _ =
  Flags.use_light_diff := false;
  Flags.dump_ast_details := true

let _ = Run.script_cpp ~parser:Parsers.Menhir (fun () ->

  (*show [cVarDef "v"];*)
  !! Trace.reparse ~parser:Parsers.Menhir  ()
  )
