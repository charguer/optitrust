open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  !! Arith_basic.(simpls [expand; euclidian]) [nbMulti; cAccesses()];
)
