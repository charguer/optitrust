open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! ()
  (* TODO: !! Accesses_basic.shift ~factor:(trm_float 5.0) [cOr [[cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];[cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()]]] *)

)
