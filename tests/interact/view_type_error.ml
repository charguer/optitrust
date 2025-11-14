open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! (); (* invokes the typechecker, and raises a type-error *)

)
