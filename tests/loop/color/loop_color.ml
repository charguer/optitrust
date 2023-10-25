open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.color (expr "C") ~index:"ci" [cFor "i"];
  !! Loop_basic.color (expr "C") [cFor "j"];

)
