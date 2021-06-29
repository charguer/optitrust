open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! ();
  !! Arrays.aos_to_soa [cTypDef "vects"];
)
