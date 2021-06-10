open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp ( fun () ->
  Declaration.remove [cTypDef "T"];
  Declaration.remove [cVarDef "x"];
  Declaration.remove [cFunDef "f"];
  Declaration.remove [cVarDef "z"];
)