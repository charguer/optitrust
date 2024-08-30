open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  (* TODO: !! Accesses_basic.scale ~factor:(trm_float 5.0) [cCellReadOrWrite ~base:[cVar "t"] ~index:[cVar "i"] ()]; *)
  !! Accesses_basic.scale ~factor:(trm_float 2.0) [cVarDef "v"]; (* [cReadOrWrite ~addr:[cVar "v"] ()]; *)
  !! Accesses_basic.scale_immut ~factor:(trm_float 0.5) [cVarDef "u"];
)
