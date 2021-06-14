open Optitrust
open Target 

(* Works *)

let _ = Run.script_cpp (fun () ->
  show [cTypDef "particle"];
  Arrays.to_variables ["ua";"ub"] [cVarDef "u"];
  Arrays.to_variables ["va";"vb"] [cVarDef "v"];
)
