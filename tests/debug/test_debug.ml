open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.disable_stringreprs := true

let _ = Run.script_cpp (fun () ->
  !! Arith_basic.(simpls_rec [expand; gather_rec; compute]) [nbMulti; cAccesses()];

)
