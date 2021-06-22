open Optitrust
open Target
open Target


(* No error is produced but it doesn't transform the following expression' *)
let _ = Run.script_cpp (fun () ->
  (* show [cTypDef "vects"]; *)
  Arrays.aos_to_soa [cTypDef "vects"];
)
