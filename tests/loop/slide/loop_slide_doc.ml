open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.slide ~size:(trm_int 3) ~step:(trm_int 1) ~index:"bi" [cFor "i"];
)
