open Optitrust
open Target

let _ =
  Flags.use_light_diff := false;
  Flags.dump_ast_details := true

let _ = Run.script_cpp ~parser:CParsers.menhir (fun () ->
  show [cFunDef "f"];
  !! Trace.reparse ~parser:CParsers.menhir  ()
  )
