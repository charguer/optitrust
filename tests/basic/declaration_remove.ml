open Optitrust
open Run

(* Works *)
let _ = run_unit_test( fun () ->
  Declaration.remove [cTypDef "T"];
  Declaration.remove [cVarDef "x"];
  Declaration.remove [cFunDef "f"];
  Declaration.remove [cVarDef "z"];
)