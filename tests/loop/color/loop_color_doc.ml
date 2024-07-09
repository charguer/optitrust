open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.color (lit "2") ~index:"ci" [cFor "i"];

  !!! (); (* TODO: Find how to eliminate this reparse *)
)
