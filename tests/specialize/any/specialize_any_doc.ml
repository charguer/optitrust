open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Specialize_basic.any  (expr "i") [cAny];

  !!! (); (* TODO: Find how to eliminate this reparse *)
)
