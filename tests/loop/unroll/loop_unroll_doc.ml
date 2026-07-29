open Optitrust
open Target

let _ = Flags.typechecking_mode := Unverified

let _ = Run.script_cpp (fun _ ->

  !! Loop.unroll [cFor "a"];

)
