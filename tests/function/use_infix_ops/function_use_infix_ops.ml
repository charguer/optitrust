open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Function.use_infix_ops ~indepth:true [cFunBody "g"];
)
