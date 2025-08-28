open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Variable.elim_reuse [cVarDef "x"];
  !! Variable.elim_reuse [cVarDef "c"];
)
