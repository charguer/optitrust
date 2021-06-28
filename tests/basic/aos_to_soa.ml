open Optitrust
open Target
(* TODO: Solve the issue with the following exprssion *)
let _ = Run.script_cpp (fun () ->
  !!Arrays.aos_to_soa [cTypDef "vects"];
)
