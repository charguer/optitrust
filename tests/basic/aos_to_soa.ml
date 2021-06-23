open Optitrust
open Target
open Target


let _ = Run.script_cpp (fun () ->
  Arrays.aos_to_soa [cTypDef "vects"];
)
