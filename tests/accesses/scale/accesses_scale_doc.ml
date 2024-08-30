open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Accesses.scale ~factor:(trm_float 5.0) [cVarDef "x"];
  (* TODO: !! Accesses.scale ~factor:(trm_float 5.0) [cVarDef "y"; cVar "x"]; *)
  )
