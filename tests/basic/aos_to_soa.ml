open Optitrust
open Target
open Target

(* TODO: Fix the issue with get_typedef *)
let _ = Run.script_cpp (fun () ->
  show [cTypDef "vects"];
  Arrays.aos_to_soa [cTypDef "vects"];
)
