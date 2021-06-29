open Optitrust
open Target
(* TODO: Solve the issue int b2 = ( *u )[i].x exprssion  *)

let _ = Run.script_cpp (fun () ->
  !! Arrays.aos_to_soa [cTypDef "vects"];
)
